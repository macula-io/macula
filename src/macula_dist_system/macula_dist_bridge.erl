%%%-------------------------------------------------------------------
%%% @doc Supervised bridge process for relay distribution tunnels.
%%%
%%% Each tunnel gets one bridge gen_server that owns:
%%% - A BridgeSock (gen_tcp, raw byte pipe)
%%% - A reader process (linked, reads socket -> publishes to mesh pool)
%%% - A writer loop (handle_info, receives `macula_event' frames from
%%%   the V2 pool and writes the decrypted payload to the socket)
%%% - Metrics counters for the tunnel
%%% - A V2 pool subscription (unsubscribed on terminate)
%%%
%%% == Pool Reconnection ==
%%%
%%% The V2 pool replays subscriptions on link respawn, so the bridge's
%%% incoming data resumes automatically. The reader handles publish
%%% failures by retrying — if the pool is temporarily wedged the
%%% publishes buffer in its gen_server mailbox and flush when at least
%%% one station_link inside the pool comes back online.
%%%
%%% If the pool gen_server PID itself dies (process crash, not just
%%% an internal link drop), the bridge attempts to re-acquire a new
%%% pool from persistent_term and re-subscribe. If no pool is available
%%% within RECONNECT_TIMEOUT, the bridge exits and the distribution
%%% connection drops.
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

%% Realm tag stamped on every dist tunnel frame. Mirrors
%% `macula_dist_relay:?DIST_REALM' (the all-zeros realm SDK
%% convention for protocol-internal infrastructure traffic).
-define(DIST_REALM, <<0:256>>).

-record(state, {
    bridge_sock      :: port(),
    tunnel_id        :: binary(),
    pool             :: pid(),
    pool_mon         :: reference() | undefined,
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

init(#{pool := Pool, bridge_sock := BridgeSock, send_topic := SendTopic,
       recv_topic := RecvTopic, tunnel_id := TunnelId, key := Key,
       metrics := Metrics}) ->
    process_flag(trap_exit, true),

    MonRef = erlang:monitor(process, Pool),
    {ok, SubRef} = subscribe_to_tunnel(Pool, RecvTopic),

    %% Don't start reader yet — socket ownership hasn't been transferred.
    %% The caller sends {socket_ready} after gen_tcp:controlling_process.
    ?LOG_INFO("[dist_bridge] Waiting for socket handoff for tunnel ~s", [TunnelId]),

    {ok, #state{bridge_sock = BridgeSock, tunnel_id = TunnelId, pool = Pool,
                pool_mon = MonRef, sub_ref = SubRef, key = Key, metrics = Metrics,
                reader_pid = undefined, send_topic = SendTopic, recv_topic = RecvTopic,
                reconnect_count = 0}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --- Tunnel data from V2 pool subscription ---
%% The pool delivers `{macula_event, SubRef, Topic, Payload, Meta}'
%% directly. Match on the SubRef stored in state so unrelated events
%% to other subscriptions on this pid do not mix.
handle_info({macula_event, SubRef, _Topic, EncData, _Meta},
            #state{sub_ref = SubRef, key = Key, bridge_sock = BridgeSock,
                   metrics = Metrics, tunnel_id = TunnelId} = State)
        when is_binary(EncData) ->
    ?LOG_DEBUG("[dist_bridge] tunnel_in ~p bytes for ~s", [byte_size(EncData), TunnelId]),
    deliver_tunnel_in(decrypt(Key, EncData), EncData, BridgeSock, Metrics,
                      TunnelId, State);

%% --- Subscription terminal signal (pool closed or our pid was demonitored
%% out) — drop the local sub_ref so the reconnect path doesn't try to
%% unsubscribe a stale ref. The pool DOWN handler below kicks in for the
%% actual respawn cycle.
handle_info({macula_event_gone, SubRef, _Reason},
            #state{sub_ref = SubRef} = State) ->
    {noreply, State#state{sub_ref = undefined}};

%% --- Socket ownership transferred, start reader ---
handle_info(socket_ready, #state{reader_pid = undefined, pool = Pool,
                                  bridge_sock = BridgeSock, send_topic = SendTopic,
                                  tunnel_id = TunnelId, key = Key, metrics = Metrics} = State) ->
    ReaderPid = start_reader(Pool, BridgeSock, SendTopic, TunnelId, Key, Metrics),
    ?LOG_INFO("[dist_bridge] Reader started (~p) for ~s", [ReaderPid, TunnelId]),
    {noreply, State#state{reader_pid = ReaderPid}};

%% --- Reader exited (linked process) ---
%% Clean exits (peer closed the dist tunnel, `global` disconnected, reader
%% finished draining, supervisor shutdown) are not crashes — `{stop, normal}`
%% so the gen_server terminates without a CRASH REPORT.
handle_info({'EXIT', Pid, normal}, #state{reader_pid = Pid,
                                           tunnel_id = TunnelId} = State) ->
    ?LOG_INFO("[dist_bridge] Reader closed cleanly for ~s", [TunnelId]),
    {stop, normal, State};
handle_info({'EXIT', Pid, shutdown}, #state{reader_pid = Pid,
                                             tunnel_id = TunnelId} = State) ->
    ?LOG_INFO("[dist_bridge] Reader shutdown for ~s", [TunnelId]),
    {stop, normal, State};
handle_info({'EXIT', Pid, {shutdown, _}}, #state{reader_pid = Pid,
                                                  tunnel_id = TunnelId} = State) ->
    ?LOG_INFO("[dist_bridge] Reader shutdown for ~s", [TunnelId]),
    {stop, normal, State};
handle_info({'EXIT', Pid, Reason}, #state{reader_pid = Pid,
                                           tunnel_id = TunnelId} = State) ->
    ?LOG_WARNING("[dist_bridge] Reader crashed (~p) for ~s", [Reason, TunnelId]),
    {stop, {reader_exit, Reason}, State};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};

%% --- Mesh pool died (Gap 2: reconnection) ---
handle_info({'DOWN', MonRef, process, _Pid, Reason},
            #state{pool_mon = MonRef, tunnel_id = TunnelId} = State) ->
    ?LOG_WARNING("[dist_bridge] Mesh pool died (~p) for ~s, attempting reconnect",
                  [Reason, TunnelId]),
    attempt_reconnect(State#state{pool_mon = undefined, sub_ref = undefined,
                                   reconnect_count = 0});

%% --- Reconnect timer ---
handle_info(reconnect_tick, State) ->
    attempt_reconnect(State);

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{bridge_sock = BridgeSock, tunnel_id = TunnelId,
                           pool = Pool, sub_ref = SubRef,
                           pool_mon = MonRef}) ->
    ?LOG_INFO("[dist_bridge] Cleaning up tunnel ~s", [TunnelId]),
    catch demonitor_if_set(MonRef),
    catch unsubscribe_if_set(Pool, SubRef),
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
    on_reacquire_pool(macula_dist_relay:get_mesh_pool(), RecvTopic, TunnelId,
                      N, State).

on_reacquire_pool(undefined, _RecvTopic, TunnelId, N, State) ->
    ?LOG_INFO("[dist_bridge] No mesh pool yet, retry ~p for ~s", [N + 1, TunnelId]),
    erlang:send_after(?RECONNECT_INTERVAL, self(), reconnect_tick),
    {noreply, State#state{reconnect_count = N + 1}};
on_reacquire_pool(NewPool, RecvTopic, TunnelId, _N, State) ->
    ?LOG_INFO("[dist_bridge] Re-acquired mesh pool for ~s", [TunnelId]),
    MonRef = erlang:monitor(process, NewPool),
    {ok, SubRef} = subscribe_to_tunnel(NewPool, RecvTopic),
    catch exit(State#state.reader_pid, kill),
    NewReader = start_reader(NewPool, State#state.bridge_sock,
                              State#state.send_topic, TunnelId,
                              State#state.key, State#state.metrics),
    {noreply, State#state{pool = NewPool, pool_mon = MonRef,
                           sub_ref = SubRef, reader_pid = NewReader,
                           reconnect_count = 0}}.

%%%===================================================================
%%% Internal — Reader (linked process with publish retry)
%%%===================================================================

start_reader(Pool, BridgeSock, SendTopic, TunnelId, Key, Metrics) ->
    spawn_link(fun() ->
        bridge_reader_loop(Pool, BridgeSock, SendTopic, TunnelId, Key, Metrics)
    end).

%% Read from the loopback socket and forward to the V2 pool.
%% {error, timeout} is NOT fatal — just means no traffic in the window.
%% Only exit on {error, closed} or genuine errors.
bridge_reader_loop(Pool, BridgeSock, SendTopic, TunnelId, Key, Metrics) ->
    on_socket_recv(gen_tcp:recv(BridgeSock, 0, ?BRIDGE_RECV_TIMEOUT),
                   Pool, BridgeSock, SendTopic, TunnelId, Key, Metrics).

on_socket_recv({ok, Data}, Pool, BridgeSock, SendTopic, TunnelId, Key, Metrics) ->
    maybe_backpressure(Pool),
    Encrypted = encrypt(Key, Data),
    publish_with_retry(Pool, SendTopic, Encrypted, ?PUBLISH_RETRIES),
    counters:add(Metrics, ?METRIC_BYTES_OUT, byte_size(Data)),
    counters:add(Metrics, ?METRIC_MSGS_OUT, 1),
    bridge_reader_loop(Pool, BridgeSock, SendTopic, TunnelId, Key, Metrics);
on_socket_recv({error, timeout}, Pool, BridgeSock, SendTopic, TunnelId, Key, Metrics) ->
    bridge_reader_loop(Pool, BridgeSock, SendTopic, TunnelId, Key, Metrics);
on_socket_recv({error, closed}, _Pool, _BridgeSock, _SendTopic, TunnelId, _Key, _Metrics) ->
    ?LOG_INFO("[dist_bridge] Reader closed for ~s", [TunnelId]);
on_socket_recv({error, Reason}, _Pool, _BridgeSock, _SendTopic, TunnelId, _Key, _Metrics) ->
    ?LOG_WARNING("[dist_bridge] Reader error ~p for ~s", [Reason, TunnelId]).

publish_with_retry(_Pool, _Topic, _Data, 0) ->
    ?LOG_WARNING("[dist_bridge] Publish retries exhausted"),
    ok;
publish_with_retry(Pool, Topic, Data, Retries) ->
    publish_if_alive(is_process_alive(Pool), Pool, Topic, Data, Retries).

publish_if_alive(true, Pool, Topic, Data, _Retries) ->
    macula_pubsub:publish(Pool, ?DIST_REALM, Topic, Data, #{});
publish_if_alive(false, Pool, Topic, Data, Retries) ->
    timer:sleep(1000),
    publish_with_retry(Pool, Topic, Data, Retries - 1).

subscribe_to_tunnel(Pool, RecvTopic) ->
    macula_pubsub:subscribe(Pool, ?DIST_REALM, RecvTopic, self()).

deliver_tunnel_in({ok, Data}, _EncData, BridgeSock, Metrics, TunnelId, State) ->
    counters:add(Metrics, ?METRIC_BYTES_IN, byte_size(Data)),
    counters:add(Metrics, ?METRIC_MSGS_IN, 1),
    on_send(gen_tcp:send(BridgeSock, Data), State, TunnelId);
deliver_tunnel_in({error, decrypt_failed}, _EncData, _BridgeSock, _Metrics,
                  TunnelId, State) ->
    ?LOG_WARNING("[dist_bridge] Decrypt failed for ~s", [TunnelId]),
    {noreply, State}.

on_send(ok, State, _TunnelId) ->
    {noreply, State};
on_send({error, Reason}, State, TunnelId) ->
    ?LOG_WARNING("[dist_bridge] Send error ~p for ~s", [Reason, TunnelId]),
    {stop, {send_error, Reason}, State}.

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

maybe_backpressure(Pool) ->
    backpressure_decide(erlang:process_info(Pool, message_queue_len)).

backpressure_decide({message_queue_len, Len}) when Len > ?BACKPRESSURE_HWM ->
    timer:sleep(1);
backpressure_decide(_) ->
    ok.

demonitor_if_set(undefined) -> ok;
demonitor_if_set(Ref) -> erlang:demonitor(Ref, [flush]).

unsubscribe_if_set(_Pool, undefined) -> ok;
unsubscribe_if_set(Pool, Ref) -> macula_pubsub:unsubscribe(Pool, Ref).

remove_metrics(TunnelId) ->
    case persistent_term:get(macula_dist_tunnels, undefined) of
        undefined -> ok;
        Tunnels ->
            persistent_term:put(macula_dist_tunnels, maps:remove(TunnelId, Tunnels))
    end.
