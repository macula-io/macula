%%%-------------------------------------------------------------------
%%% @doc Supervised bridge process for relay distribution tunnels.
%%%
%%% Each tunnel gets one bridge gen_server that owns:
%%% - A BridgeSock (gen_tcp, raw byte pipe)
%%% - A reader process (linked, reads socket -> publishes to relay)
%%% - A writer loop (handle_info, receives from relay -> writes to socket)
%%% - Metrics counters for the tunnel
%%% - A relay subscription (unsubscribed on terminate)
%%%
%%% == Relay Reconnection ==
%%%
%%% The relay_client replays subscriptions on reconnect, so the bridge's
%%% incoming data (tunnel_in messages) resumes automatically. The reader
%%% handles publish failures by retrying — if the relay client is
%%% temporarily disconnected, publishes buffer in its gen_server mailbox
%%% and flush when the QUIC connection is re-established.
%%%
%%% If the relay client PID dies (process crash, not just QUIC drop),
%%% the bridge attempts to re-acquire a new client from persistent_term
%%% and re-subscribe. If no client is available within RECONNECT_TIMEOUT,
%%% the bridge exits and the distribution connection drops.
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
-define(RECONNECT_INTERVAL, 2000).
-define(RECONNECT_MAX_ATTEMPTS, 15).  %% 15 * 2s = 30s max reconnect window

-define(METRIC_BYTES_OUT, 1).
-define(METRIC_BYTES_IN, 2).
-define(METRIC_MSGS_OUT, 3).
-define(METRIC_MSGS_IN, 4).
-define(PUBLISH_RETRIES, 3).

-record(state, {
    bridge_sock      :: port(),
    tunnel_id        :: binary(),
    client           :: pid(),
    client_mon       :: reference(),
    sub_ref          :: reference() | undefined,
    key              :: binary(),
    metrics          :: counters:counters_ref(),
    reader_pid       :: pid() | undefined,
    send_topic       :: binary(),
    recv_topic       :: binary(),
    reconnect_count  :: non_neg_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

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

    MonRef = erlang:monitor(process, Client),
    {ok, SubRef} = subscribe_to_tunnel(Client, RecvTopic),

    %% Don't start reader yet — socket ownership hasn't been transferred.
    %% The caller sends {socket_ready} after gen_tcp:controlling_process.
    ?LOG_INFO("[dist_bridge] Waiting for socket handoff for tunnel ~s", [TunnelId]),

    {ok, #state{bridge_sock = BridgeSock, tunnel_id = TunnelId, client = Client,
                client_mon = MonRef, sub_ref = SubRef, key = Key, metrics = Metrics,
                reader_pid = undefined, send_topic = SendTopic, recv_topic = RecvTopic,
                reconnect_count = 0},
     ?BRIDGE_RECV_TIMEOUT}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State, ?BRIDGE_RECV_TIMEOUT}.

handle_cast(_Msg, State) ->
    {noreply, State, ?BRIDGE_RECV_TIMEOUT}.

%% --- Tunnel data from relay subscription ---
handle_info({tunnel_in, EncData}, #state{key = Key, bridge_sock = BridgeSock,
                                          metrics = Metrics, tunnel_id = TunnelId} = State)
        when is_binary(EncData) ->
    ?LOG_WARNING("[BRIDGE-TRACE] tunnel_in: ~p bytes for ~s", [byte_size(EncData), TunnelId]),
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

%% --- Socket ownership transferred, start reader ---
handle_info(socket_ready, #state{reader_pid = undefined, client = Client,
                                  bridge_sock = BridgeSock, send_topic = SendTopic,
                                  tunnel_id = TunnelId, key = Key, metrics = Metrics} = State) ->
    ReaderPid = start_reader(Client, BridgeSock, SendTopic, TunnelId, Key, Metrics),
    ?LOG_WARNING("[BRIDGE-TRACE] Reader started (~p) for ~s sock=~p", [ReaderPid, TunnelId, BridgeSock]),
    {noreply, State#state{reader_pid = ReaderPid}, ?BRIDGE_RECV_TIMEOUT};

%% --- Reader exited (linked process) ---
handle_info({'EXIT', Pid, Reason}, #state{reader_pid = Pid, tunnel_id = TunnelId} = State) ->
    ?LOG_INFO("[dist_bridge] Reader exited (~p) for ~s", [Reason, TunnelId]),
    {stop, {reader_exit, Reason}, State};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State, ?BRIDGE_RECV_TIMEOUT};

%% --- Relay client died (Gap 2: reconnection) ---
handle_info({'DOWN', MonRef, process, _Pid, Reason},
            #state{client_mon = MonRef, tunnel_id = TunnelId} = State) ->
    ?LOG_WARNING("[dist_bridge] Relay client died (~p) for ~s, attempting reconnect",
                  [Reason, TunnelId]),
    attempt_reconnect(State#state{client_mon = undefined, sub_ref = undefined,
                                   reconnect_count = 0});

%% --- Reconnect timer ---
handle_info(reconnect_tick, State) ->
    attempt_reconnect(State);

%% --- No data timeout ---
handle_info(timeout, #state{tunnel_id = TunnelId} = State) ->
    ?LOG_WARNING("[dist_bridge] Timeout (no data) for ~s", [TunnelId]),
    {stop, timeout, State};

handle_info(_Msg, State) ->
    {noreply, State, ?BRIDGE_RECV_TIMEOUT}.

terminate(_Reason, #state{bridge_sock = BridgeSock, tunnel_id = TunnelId,
                           client = Client, sub_ref = SubRef,
                           client_mon = MonRef}) ->
    ?LOG_INFO("[dist_bridge] Cleaning up tunnel ~s", [TunnelId]),
    catch demonitor_if_set(MonRef),
    catch unsubscribe_if_set(Client, SubRef),
    catch gen_tcp:close(BridgeSock),
    remove_metrics(TunnelId),
    ok.

%%%===================================================================
%%% Internal — Reconnection (Gap 2)
%%%===================================================================

attempt_reconnect(#state{reconnect_count = N, tunnel_id = TunnelId} = State)
        when N >= ?RECONNECT_MAX_ATTEMPTS ->
    ?LOG_ERROR("[dist_bridge] Reconnect exhausted (~p attempts) for ~s", [N, TunnelId]),
    {stop, relay_reconnect_failed, State};
attempt_reconnect(#state{recv_topic = RecvTopic, tunnel_id = TunnelId,
                          reconnect_count = N} = State) ->
    case macula_dist_relay:get_mesh_client() of
        undefined ->
            ?LOG_INFO("[dist_bridge] No relay client yet, retry ~p for ~s", [N + 1, TunnelId]),
            erlang:send_after(?RECONNECT_INTERVAL, self(), reconnect_tick),
            {noreply, State#state{reconnect_count = N + 1}, ?BRIDGE_RECV_TIMEOUT};
        NewClient ->
            ?LOG_INFO("[dist_bridge] Re-acquired relay client for ~s", [TunnelId]),
            MonRef = erlang:monitor(process, NewClient),
            {ok, SubRef} = subscribe_to_tunnel(NewClient, RecvTopic),
            %% Restart reader with new client
            catch exit(State#state.reader_pid, kill),
            NewReader = start_reader(NewClient, State#state.bridge_sock,
                                      State#state.send_topic, TunnelId,
                                      State#state.key, State#state.metrics),
            {noreply, State#state{client = NewClient, client_mon = MonRef,
                                   sub_ref = SubRef, reader_pid = NewReader,
                                   reconnect_count = 0},
             ?BRIDGE_RECV_TIMEOUT}
    end.

%%%===================================================================
%%% Internal — Reader (linked process with publish retry)
%%%===================================================================

start_reader(Client, BridgeSock, SendTopic, TunnelId, Key, Metrics) ->
    spawn_link(fun() ->
        bridge_reader_loop(Client, BridgeSock, SendTopic, TunnelId, Key, Metrics)
    end).

bridge_reader_loop(MeshClient, BridgeSock, SendTopic, TunnelId, Key, Metrics) ->
    case gen_tcp:recv(BridgeSock, 0, ?BRIDGE_RECV_TIMEOUT) of
        {ok, Data} ->
            ?LOG_WARNING("[BRIDGE-TRACE] Reader recv ~p bytes for ~s", [byte_size(Data), TunnelId]),
            maybe_backpressure(MeshClient),
            Encrypted = encrypt(Key, Data),
            publish_with_retry(MeshClient, SendTopic, Encrypted, ?PUBLISH_RETRIES),
            counters:add(Metrics, ?METRIC_BYTES_OUT, byte_size(Data)),
            counters:add(Metrics, ?METRIC_MSGS_OUT, 1),
            bridge_reader_loop(MeshClient, BridgeSock, SendTopic, TunnelId, Key, Metrics);
        {error, closed} ->
            ?LOG_WARNING("[BRIDGE-TRACE] Reader CLOSED for ~s sock=~p", [TunnelId, BridgeSock]);
        {error, Reason} ->
            ?LOG_WARNING("[BRIDGE-TRACE] Reader ERROR ~p for ~s sock=~p", [Reason, TunnelId, BridgeSock])
    end.

publish_with_retry(_Client, _Topic, _Data, 0) ->
    ?LOG_WARNING("[dist_bridge] Publish retries exhausted"),
    ok;
publish_with_retry(Client, Topic, Data, Retries) ->
    case is_process_alive(Client) of
        true ->
            macula_mesh_client:publish(Client, Topic, Data);
        false ->
            timer:sleep(1000),
            publish_with_retry(Client, Topic, Data, Retries - 1)
    end.

subscribe_to_tunnel(Client, RecvTopic) ->
    Self = self(),
    macula_mesh_client:subscribe(Client, RecvTopic,
        fun(Msg) -> Self ! {tunnel_in, macula_dist_relay:extract_payload(Msg)} end).

%%%===================================================================
%%% Internal — Crypto
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
%%% Internal — Helpers
%%%===================================================================

maybe_backpressure(MeshClient) ->
    case erlang:process_info(MeshClient, message_queue_len) of
        {message_queue_len, Len} when Len > ?BACKPRESSURE_HWM ->
            timer:sleep(1);
        _ ->
            ok
    end.

demonitor_if_set(undefined) -> ok;
demonitor_if_set(Ref) -> erlang:demonitor(Ref, [flush]).

unsubscribe_if_set(_Client, undefined) -> ok;
unsubscribe_if_set(Client, Ref) -> macula_mesh_client:unsubscribe(Client, Ref).

remove_metrics(TunnelId) ->
    case persistent_term:get(macula_dist_tunnels, undefined) of
        undefined -> ok;
        Tunnels ->
            persistent_term:put(macula_dist_tunnels, maps:remove(TunnelId, Tunnels))
    end.
