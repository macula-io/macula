%%%-------------------------------------------------------------------
%%% @doc Supervised bridge process for relay distribution tunnels.
%%%
%%% Each tunnel gets one bridge gen_server that owns:
%%% - A BridgeSock (gen_tcp, raw byte pipe)
%%% - A reader process (linked, reads socket → publishes to relay)
%%% - A writer loop (handle_info, receives from relay → writes to socket)
%%% - Metrics counters for the tunnel
%%% - A relay subscription (unsubscribed on terminate)
%%%
%%% Started by `macula_dist_bridge_sup' (simple_one_for_one).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_bridge).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2]).

-define(BRIDGE_RECV_TIMEOUT, 60000).
-define(BACKPRESSURE_HWM, 64).

-define(METRIC_BYTES_OUT, 1).
-define(METRIC_BYTES_IN, 2).
-define(METRIC_MSGS_OUT, 3).
-define(METRIC_MSGS_IN, 4).

-record(state, {
    bridge_sock  :: port(),
    tunnel_id    :: binary(),
    client       :: pid(),
    sub_ref      :: reference() | undefined,
    key          :: binary(),
    metrics      :: counters:counters_ref(),
    reader_pid   :: pid() | undefined,
    send_topic   :: binary()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start a bridge process.
%% Args: #{client, bridge_sock, send_topic, recv_topic, tunnel_id, key, metrics}
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(#{client := Client, bridge_sock := BridgeSock, send_topic := SendTopic,
       recv_topic := RecvTopic, tunnel_id := TunnelId, key := Key,
       metrics := Metrics}) ->
    process_flag(trap_exit, true),
    Self = self(),

    {ok, SubRef} = macula_relay_client:subscribe(Client, RecvTopic,
        fun(Msg) -> Self ! {tunnel_in, macula_dist_relay:extract_payload(Msg)} end),

    ReaderPid = spawn_link(fun() ->
        bridge_reader_loop(Client, BridgeSock, SendTopic, TunnelId, Key, Metrics)
    end),

    ?LOG_INFO("[dist_bridge] Started for tunnel ~s (reader ~p)", [TunnelId, ReaderPid]),

    {ok, #state{bridge_sock = BridgeSock, tunnel_id = TunnelId, client = Client,
                sub_ref = SubRef, key = Key, metrics = Metrics,
                reader_pid = ReaderPid, send_topic = SendTopic},
     ?BRIDGE_RECV_TIMEOUT}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State, ?BRIDGE_RECV_TIMEOUT}.

handle_cast(_Msg, State) ->
    {noreply, State, ?BRIDGE_RECV_TIMEOUT}.

handle_info({tunnel_in, EncData}, #state{key = Key, bridge_sock = BridgeSock,
                                          metrics = Metrics, tunnel_id = TunnelId} = State)
        when is_binary(EncData) ->
    case decrypt(Key, EncData) of
        {ok, Data} ->
            counters:add(Metrics, ?METRIC_BYTES_IN, byte_size(Data)),
            counters:add(Metrics, ?METRIC_MSGS_IN, 1),
            case gen_tcp:send(BridgeSock, Data) of
                ok ->
                    {noreply, State, ?BRIDGE_RECV_TIMEOUT};
                {error, Reason} ->
                    ?LOG_WARNING("[dist_bridge] Send error ~p for ~s", [Reason, TunnelId]),
                    {stop, {send_error, Reason}, State}
            end;
        {error, decrypt_failed} ->
            ?LOG_WARNING("[dist_bridge] Decrypt failed for ~s", [TunnelId]),
            {noreply, State, ?BRIDGE_RECV_TIMEOUT}
    end;

handle_info({'EXIT', Pid, Reason}, #state{reader_pid = Pid, tunnel_id = TunnelId} = State) ->
    ?LOG_INFO("[dist_bridge] Reader exited (~p) for ~s", [Reason, TunnelId]),
    {stop, {reader_exit, Reason}, State};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State, ?BRIDGE_RECV_TIMEOUT};

handle_info(timeout, #state{tunnel_id = TunnelId} = State) ->
    ?LOG_WARNING("[dist_bridge] Timeout (no data) for ~s", [TunnelId]),
    {stop, timeout, State};

handle_info(_Msg, State) ->
    {noreply, State, ?BRIDGE_RECV_TIMEOUT}.

terminate(_Reason, #state{bridge_sock = BridgeSock, tunnel_id = TunnelId,
                           client = Client, sub_ref = SubRef}) ->
    ?LOG_INFO("[dist_bridge] Cleaning up tunnel ~s", [TunnelId]),
    catch macula_relay_client:unsubscribe(Client, SubRef),
    catch gen_tcp:close(BridgeSock),
    remove_metrics(TunnelId),
    ok.

%%%===================================================================
%%% Internal — Reader Loop (runs in linked process)
%%%===================================================================

bridge_reader_loop(MeshClient, BridgeSock, SendTopic, TunnelId, Key, Metrics) ->
    case gen_tcp:recv(BridgeSock, 0, ?BRIDGE_RECV_TIMEOUT) of
        {ok, Data} ->
            maybe_backpressure(MeshClient),
            Encrypted = encrypt(Key, Data),
            macula_relay_client:publish(MeshClient, SendTopic, Encrypted),
            counters:add(Metrics, ?METRIC_BYTES_OUT, byte_size(Data)),
            counters:add(Metrics, ?METRIC_MSGS_OUT, 1),
            bridge_reader_loop(MeshClient, BridgeSock, SendTopic, TunnelId, Key, Metrics);
        {error, closed} ->
            ?LOG_INFO("[dist_bridge] Reader closed for ~s", [TunnelId]);
        {error, Reason} ->
            ?LOG_WARNING("[dist_bridge] Reader error ~p for ~s", [Reason, TunnelId])
    end.

%%%===================================================================
%%% Internal — Crypto (delegates to same algorithm as macula_dist_relay)
%%%===================================================================

encrypt(Key, Plaintext) ->
    Nonce = crypto:strong_rand_bytes(12),
    {Ciphertext, Tag} = crypto:crypto_one_time_aead(
        aes_256_gcm, Key, Nonce, Plaintext, <<>>, true),
    <<Nonce/binary, Tag/binary, Ciphertext/binary>>.

decrypt(Key, <<Nonce:12/binary, Tag:16/binary, Ciphertext/binary>>) ->
    case crypto:crypto_one_time_aead(
            aes_256_gcm, Key, Nonce, Ciphertext, <<>>, Tag, false) of
        error -> {error, decrypt_failed};
        Plaintext -> {ok, Plaintext}
    end;
decrypt(_Key, _Data) ->
    {error, decrypt_failed}.

%%%===================================================================
%%% Internal — Backpressure + Metrics
%%%===================================================================

maybe_backpressure(MeshClient) ->
    case erlang:process_info(MeshClient, message_queue_len) of
        {message_queue_len, Len} when Len > ?BACKPRESSURE_HWM ->
            timer:sleep(1);
        _ ->
            ok
    end.

remove_metrics(TunnelId) ->
    case persistent_term:get(macula_dist_tunnels, undefined) of
        undefined -> ok;
        Tunnels ->
            persistent_term:put(macula_dist_tunnels, maps:remove(TunnelId, Tunnels))
    end.
