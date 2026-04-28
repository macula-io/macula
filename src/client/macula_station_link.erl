%% @private
%% @doc Per-station link — internal to `macula_client' (the pool).
%%
%% A `macula_station_link' is a `gen_server' that owns one
%% `macula_peering' connection to a single station endpoint. The
%% pool spawns one link per healthy seed and routes operations
%% through them transparently. **Application code should not call
%% `macula_station_link' directly** — use `macula_client' (or the
%% `macula' facade), which handles failover, replication, dedup,
%% and subscription replay.
%%
%% This module is kept publicly accessible for diagnostics and
%% special-case use (e.g. probing a specific station). It is
%% marked `@private' so it does not appear in user-facing
%% documentation indices.
%%
%% Per `PLAN_V2_PARITY' Q6: the per-station worker name is
%% `macula_station_link' (not `macula_station_client') — a station
%% is an identity bound to one IPv6:port; one relay box hosts many
%% stations; a "client" name is taken by the pool above.
%%
%% It drives the CONNECT/HELLO handshake as the client side, then
%% exposes three surfaces over the same peering pipe:
%%
%% <ul>
%%   <li><strong>Request/response</strong> — `call/5' sends a CALL
%%       frame and matches inbound RESULT/ERROR frames against
%%       pending callers using the 16-byte CALL id. Convenience
%%       wrappers cover `_dht.put_record', `_dht.find_record', and
%%       `_dht.find_records_by_type'.</li>
%%   <li><strong>Streaming subscribe</strong> — `subscribe/4' sends
%%       a SUBSCRIBE frame and registers a delivery pid. Inbound
%%       EVENT frames matching the (realm, topic) fan out to
%%       subscribers as
%%       `{macula_event, SubRef, Topic, Payload, Meta}'. On
%%       disconnect each subscriber receives a single
%%       `{macula_event_gone, SubRef, Reason}'.</li>
%%   <li><strong>Publish</strong> — `publish/4' sends a PUBLISH
%%       frame fire-and-forget. Per-link monotonic `seq' counter
%%       stamps each frame for downstream dedup.</li>
%% </ul>
%%
%% == Realm-per-call ==
%%
%% Per `PLAN_V2_PARITY' Q2 sub-decision §2: realm is **per-call**, not
%% connect-time. Stations are realm-agnostic infrastructure; every
%% wire frame carries its own 32-byte `realm' tag. The link advertises
%% an empty realms list in CONNECT and stamps the realm passed to each
%% public op onto the outbound frame.
%%
%% == Lifecycle ==
%%
%% <ol>
%%   <li>`start_link/1' — spawn worker, schedule connect.</li>
%%   <li>`connect_now/1' (cast) — build connect opts, call
%%       `macula_peering:connect/1', store the worker pid.</li>
%%   <li>Peering handshake completes → `{macula_peering, connected,
%%       Pid, PeerNodeId}' arrives → state moves to `connected'.</li>
%%   <li>`call/5' from caller → build CALL frame, sign happens inside
%%       peering, store `{from, deadline_timer}` keyed by CALL id, send
%%       frame via `macula_peering:send_frame/2'.</li>
%%   <li>RESULT or ERROR arrives as `{macula_peering, frame, Pid, Frame}'
%%       → look up `call_id', cancel timer, reply to caller.</li>
%%   <li>`{macula_peering, disconnected, Pid, Reason}' → fail all
%%       pending calls with `{error, {disconnected, Reason}}', notify
%%       all subscribers via `macula_event_gone', stop the client
%%       (caller is responsible for restart / reconnect).</li>
%% </ol>
%%
%% == Call reply taxonomy ==
%%
%% <table>
%%   <tr><th>Inbound frame</th><th>`call/5' returns</th></tr>
%%   <tr><td>RESULT(payload=`{error, Reason}')</td><td>`{ok, {error, Reason}}'</td></tr>
%%   <tr><td>RESULT(payload=Value)</td><td>`{ok, Value}'</td></tr>
%%   <tr><td>ERROR(code=C, name=N)</td><td>`{error, {call_error, C, N}}'</td></tr>
%%   <tr><td>(deadline elapses)</td><td>`{error, timeout}'</td></tr>
%%   <tr><td>(connection drops)</td><td>`{error, {disconnected, Reason}}'</td></tr>
%% </table>
-module(macula_station_link).
-behaviour(gen_server).

-export([
    start_link/1,
    stop/1,
    call/5,
    publish/4,
    put_record/2, put_record/3,
    find_record/2, find_record/3,
    find_records_by_type/2, find_records_by_type/3,
    subscribe/4,
    unsubscribe/2,
    advertise/4,
    unadvertise/3,
    is_connected/1,
    peer_node_id/1
]).

-export_type([handler/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([opts/0]).

-type url() :: binary() | string().

-type opts() :: #{
    %% Endpoint to dial. Either a URL (https://host:port) or a
    %% pre-parsed #{host, port} map.
    seed     := url() | #{host := binary() | string(),
                          port := inet:port_number()},
    %% Local Ed25519 keypair used to sign the CONNECT frame and any
    %% subsequent application frames. Auto-generated when absent.
    identity => macula_identity:key_pair(),
    %% Capability bitfield announced in CONNECT (default 0).
    capabilities => non_neg_integer(),
    %% ALPN list passed through to QUIC (default [<<"macula">>]).
    alpn         => [binary()],
    %% Connect timeout in ms (default 30_000).
    connect_timeout_ms => non_neg_integer()
}.

-define(DHT_REALM, <<0:256>>).
-define(DEFAULT_DEADLINE_MS, 5_000).
-define(CONNECT_RETRY_BACKOFF_MS, 1_000).

-record(state, {
    seed             :: #{host := binary() | string(),
                          port := inet:port_number()},
    identity         :: macula_identity:key_pair(),
    capabilities     :: non_neg_integer(),
    alpn             :: [binary()],
    connect_timeout_ms :: non_neg_integer(),
    %% peering worker pid (`macula_peering_conn`). undefined while
    %% disconnected.
    peer_pid         :: pid() | undefined,
    %% peer's node id, set on `connected'.
    peer_node_id     :: macula_identity:pubkey() | undefined,
    %% map of CALL id (16 bytes) -> {From, TimerRef}.
    pending = #{}    :: #{<<_:128>> => {gen_server:from(), reference()}},
    %% Active topic subscriptions keyed by SubRef returned to the
    %% subscriber. The reverse `topic_index' lets inbound EVENT
    %% frames fan out to all SubRefs subscribed to a given
    %% (realm, topic) without scanning the whole subscriptions map.
    subscriptions = #{} :: #{reference() => subscription()},
    topic_index   = #{} :: #{{<<_:256>>, binary()} => sets:set(reference())},
    %% Monotonic per-link publish sequence (stamps outbound PUBLISH
    %% frames). Resets on link respawn — pool dedup absorbs the gap.
    publish_seq = 0 :: non_neg_integer(),
    %% Advertised RPC procedures. Keyed by `{Realm, Procedure}`. The
    %% link sends one ADVERTISE frame per entry on every successful
    %% (re)connect (drained alongside subscriptions on `connected').
    %% Inbound CALL frames whose `(realm, procedure)' is in this map
    %% are dispatched to the registered handler; the resulting
    %% RESULT or call_error frame is shipped back over the same
    %% peering connection.
    procedures = #{} :: #{{<<_:256>>, binary()} => handler()}
}).

-type subscription() :: {Realm     :: <<_:256>>,
                         Topic     :: binary(),
                         Subscriber :: pid(),
                         Mon        :: reference()}.

-type handler() :: fun((term()) -> term())
                 | {module(), atom()}.

%%====================================================================
%% Public API
%%====================================================================

%% @doc Start a station-client connected to `seed'.
%% Returns once the gen_server is alive; the QUIC handshake completes
%% asynchronously. Use `is_connected/1' to poll readiness or just
%% issue `call/5' (which blocks the caller until ready or until its
%% timeout elapses).
-spec start_link(opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) when is_map(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Issue a CALL frame and block until the station replies, the
%% deadline elapses, or the connection drops.
%%
%% `Realm' is the 32-byte realm id stamped on the outbound CALL frame.
%% Stations are realm-agnostic infrastructure; the realm is carried
%% per-frame so a single link can multiplex many realms.
%%
%% `Procedure' is the V2 procedure name, e.g.
%% `<<"_dht.find_records_by_type">>'. `Payload' is any term that
%% `macula_frame:call/1' accepts (typically a map).
-spec call(pid(), <<_:256>>, binary(), term(), pos_integer()) ->
    {ok, term()} | {error, term()}.
call(Pid, Realm, Procedure, Payload, TimeoutMs)
  when is_pid(Pid),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       is_integer(TimeoutMs), TimeoutMs > 0 ->
    %% gen_server timeout = TimeoutMs + 500 to give the server time to
    %% report a clean `{error, timeout}' rather than the caller seeing
    %% a hard `exit({timeout, ...})'.
    GenTimeout = TimeoutMs + 500,
    try
        gen_server:call(Pid, {call, Realm, Procedure, Payload, TimeoutMs},
                        GenTimeout)
    catch
        %% try/catch retained: collapses the three distinct gen_server
        %% exit signals into the SDK's call-result taxonomy. Without
        %% it the caller sees `exit({timeout, _})' instead of
        %% `{error, timeout}', breaking the contract documented above.
        exit:{timeout, _}      -> {error, timeout};
        exit:{noproc, _}       -> {error, noproc};
        exit:{normal, _}       -> {error, gone}
    end.

%% @doc Send a PUBLISH frame fire-and-forget. The link stamps a
%% monotonic per-link `seq' onto the frame and the local
%% `published_at_ms' clock; the station relays it to subscribers.
%%
%% Returns `ok' once the frame is on the wire, `{error, not_connected}'
%% when the link has not yet completed the QUIC handshake. Publishes
%% are NOT queued during disconnect — they would arrive at the wrong
%% wall-clock and fight pool-level dedup. The pool retries on a peer
%% link instead.
-spec publish(pid(), <<_:256>>, binary(), term()) ->
    ok | {error, not_connected | term()}.
publish(Pid, Realm, Topic, Payload)
  when is_pid(Pid),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Topic) ->
    gen_server:call(Pid, {publish, Realm, Topic, Payload}, 5_000).

%% @doc Convenience wrapper for `_dht.put_record'. The record must be
%% a fully-signed `macula_record:record()' map (build via
%% `macula_record:envelope/3,4' + `macula_record:sign/2'). Returns
%% `ok' on success, `{error, Reason}' on RPC failure or unexpected
%% reply.
%%
%% Stations replicate the put across the K-nearest peers in their
%% Kademlia routing table, so a single `put_record/2' call against
%% any one connected station propagates to the rest of the DHT.
%%
%% DHT-internal procedures travel under the all-zeros realm tag —
%% they are protocol-internal, not bound to any business realm.
-spec put_record(pid(), map()) -> ok | {error, term()}.
put_record(Pid, Record) ->
    put_record(Pid, Record, ?DEFAULT_DEADLINE_MS).

-spec put_record(pid(), map(), pos_integer()) -> ok | {error, term()}.
put_record(Pid, Record, TimeoutMs) when is_pid(Pid), is_map(Record) ->
    classify_put(call(Pid, ?DHT_REALM, <<"_dht.put_record">>,
                      Record, TimeoutMs)).

classify_put({ok, ok})       -> ok;
classify_put({ok, Other})    -> {error, {unexpected_reply, Other}};
classify_put({error, _} = E) -> E.

%% @doc Convenience wrapper for `_dht.find_record'. Looks up a record
%% by its `macula_record:storage_key/1' (32-byte BLAKE3 digest).
%% Returns `{error, not_found}' when no record exists at the key.
%% Callers SHOULD verify the returned record's signature with
%% `macula_record:verify/1' before trusting its payload.
-spec find_record(pid(), <<_:256>>) ->
    {ok, map()} | {error, not_found | term()}.
find_record(Pid, Key) ->
    find_record(Pid, Key, ?DEFAULT_DEADLINE_MS).

-spec find_record(pid(), <<_:256>>, pos_integer()) ->
    {ok, map()} | {error, not_found | term()}.
find_record(Pid, Key, TimeoutMs)
  when is_pid(Pid), is_binary(Key), byte_size(Key) =:= 32 ->
    classify_find(call(Pid, ?DHT_REALM, <<"_dht.find_record">>,
                       #{key => Key}, TimeoutMs)).

classify_find({ok, #{type := _, payload := _, sig := _} = R}) -> {ok, R};
classify_find({ok, not_found})   -> {error, not_found};
classify_find({ok, Other})       -> {error, {unexpected_reply, Other}};
classify_find({error, _} = E)    -> E.

%% @doc Convenience wrapper for `_dht.find_records_by_type'. Returns
%% the decoded list of signed records (CBOR-decoded maps as produced
%% by `macula_record').
-spec find_records_by_type(pid(), 0..255) ->
    {ok, [map()]} | {error, term()}.
find_records_by_type(Pid, Type) ->
    find_records_by_type(Pid, Type, ?DEFAULT_DEADLINE_MS).

-spec find_records_by_type(pid(), 0..255, pos_integer()) ->
    {ok, [map()]} | {error, term()}.
find_records_by_type(Pid, Type, TimeoutMs)
  when is_integer(Type), Type >= 0, Type =< 255 ->
    classify_records(call(Pid, ?DHT_REALM, <<"_dht.find_records_by_type">>,
                          #{type => Type}, TimeoutMs)).

classify_records({ok, Records}) when is_list(Records) -> {ok, Records};
classify_records({ok, Other})                          -> {error, {unexpected_reply, Other}};
classify_records({error, _} = E)                       -> E.

%% @doc Subscribe to a peering pubsub topic in `Realm'. Sends a
%% SUBSCRIBE frame to the connected station and registers
%% `Subscriber' as the delivery pid for inbound EVENT frames matching
%% `(Realm, Topic)'.
%%
%% Returns `{ok, SubRef}' once the SUBSCRIBE frame is sent (or queued
%% if the peering handshake has not yet completed — drained on
%% `connected'). Stations do not acknowledge SUBSCRIBE — the contract
%% is best-effort, mirroring the existing peering pubsub semantics.
%%
%% Subscriber receives one of:
%%
%% <ul>
%%   <li>`{macula_event, SubRef, Topic, Payload, Meta}' — every time
%%       an EVENT frame arrives for `(Realm, Topic)'. `Meta' is a map
%%       with `realm', `publisher', `seq', and `delivered_via'
%%       fields.</li>
%%   <li>`{macula_event_gone, SubRef, Reason}' — once, when the
%%       connection drops or the client stops. The subscription map
%%       is cleared on the same transition.</li>
%% </ul>
%%
%% The client monitors `Subscriber'; if it dies the subscription is
%% torn down (best-effort UNSUBSCRIBE on the wire).
-spec subscribe(pid(), <<_:256>>, binary(), pid()) ->
    {ok, reference()}.
subscribe(Client, Realm, Topic, Subscriber)
  when is_pid(Client),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Topic), is_pid(Subscriber) ->
    gen_server:call(Client, {subscribe, Realm, Topic, Subscriber}, 5_000).

%% @doc Drop a subscription. Sends a best-effort UNSUBSCRIBE frame
%% to the station and clears local bookkeeping. Always returns `ok',
%% even when `SubRef' is unknown — unsubscribe is idempotent.
-spec unsubscribe(pid(), reference()) -> ok.
unsubscribe(Client, SubRef)
  when is_pid(Client), is_reference(SubRef) ->
    gen_server:call(Client, {unsubscribe, SubRef}, 5_000).

%% @doc Advertise an RPC procedure handler. The link sends an
%% ADVERTISE frame to the connected station; the station forwards
%% inbound CALL frames matching `(Realm, Procedure)' back over the
%% peering connection where this link dispatches them to `Handler'.
%%
%% Idempotent: re-advertising replaces the prior handler. Replayed
%% on every (re)connect — the caller does not need to re-call
%% `advertise/4' after a peering reconnect.
%%
%% Returns once the handler is registered locally. The wire frame
%% goes out immediately if the peering handshake has completed; if
%% not, it is queued for the post-HELLO drain (matches `subscribe/4'
%% semantics).
%%
%% Handlers run in a transient process spawned per CALL. They must
%% return `{ok, Reply}', `{error, Reason}', or any other term (treated
%% as `{ok, Other}' shorthand). A handler crash is mapped to a
%% structured `temporary_relay_failure' BOLT#4 error.
-spec advertise(pid(), <<_:256>>, binary(), handler()) -> ok.
advertise(Pid, Realm, Procedure, Handler)
  when is_pid(Pid),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       (is_function(Handler, 1) orelse
        (is_tuple(Handler) andalso tuple_size(Handler) =:= 2)) ->
    gen_server:call(Pid, {advertise, Realm, Procedure, Handler}, 5_000).

%% @doc Drop a previously-advertised procedure. Sends a best-effort
%% UNADVERTISE frame to the station and clears the local handler
%% binding. Idempotent: unknown `(Realm, Procedure)' is a no-op.
-spec unadvertise(pid(), <<_:256>>, binary()) -> ok.
unadvertise(Pid, Realm, Procedure)
  when is_pid(Pid),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure) ->
    gen_server:call(Pid, {unadvertise, Realm, Procedure}, 5_000).

-spec is_connected(pid()) -> boolean().
is_connected(Pid) ->
    case gen_server:call(Pid, is_connected, 1_000) of
        true  -> true;
        false -> false
    end.

-spec peer_node_id(pid()) -> {ok, macula_identity:pubkey()} | {error, not_connected}.
peer_node_id(Pid) ->
    gen_server:call(Pid, peer_node_id, 1_000).

%%====================================================================
%% gen_server
%%====================================================================

init(Opts) ->
    Seed     = parse_seed(maps:get(seed, Opts)),
    Identity = maps:get(identity, Opts, macula_identity:generate()),
    Caps     = maps:get(capabilities, Opts, 0),
    Alpn     = maps:get(alpn, Opts, [<<"macula">>]),
    Tmo      = maps:get(connect_timeout_ms, Opts, 30_000),
    State    = #state{seed = Seed, identity = Identity,
                      capabilities = Caps, alpn = Alpn,
                      connect_timeout_ms = Tmo},
    process_flag(trap_exit, true),
    self() ! attempt_connect,
    {ok, State}.

handle_call({call, _Realm, _Proc, _Payload, _Tmo}, _From,
            #state{peer_node_id = undefined} = S) ->
    %% Gate CALL on the full CONNECT/HELLO handshake (mirrors the
    %% `{publish, ...}' clause below). `peer_pid' is set the moment
    %% `macula_peering:connect/1' returns, BEFORE the peering worker
    %% has finished handshaking. Frames sent during the peering
    %% statem's `handshaking' state have no clause for
    %% `cast({send_frame, _})' and silently fall into
    %% `drop_unexpected', so the call frame never lands on the wire
    %% and the caller eventually times out at `Tmo'. Returning
    %% `{error, not_connected}' here lets the caller back off and
    %% retry once the handshake completes.
    {reply, {error, not_connected}, S};
handle_call({call, Realm, Proc, Payload, Tmo}, From,
            #state{peer_pid = Pid, identity = Id, pending = P} = S) ->
    CallId = crypto:strong_rand_bytes(16),
    Caller = macula_identity:public(Id),
    DeadlineMs = erlang:system_time(millisecond) + Tmo,
    Frame = macula_frame:call(#{
        call_id     => CallId,
        procedure   => Proc,
        realm       => Realm,
        payload     => Payload,
        deadline_ms => DeadlineMs,
        caller      => Caller
    }),
    ok = macula_peering:send_frame(Pid, Frame),
    TRef = erlang:send_after(Tmo, self(), {call_timeout, CallId}),
    {noreply, S#state{pending = P#{CallId => {From, TRef}}}};

handle_call({publish, _Realm, _Topic, _Payload}, _From,
            #state{peer_node_id = undefined} = S) ->
    %% Require the full HELLO handshake before publishing — the
    %% peering worker may exist mid-handshake while the wire is not
    %% yet ready for application frames. Matches `is_connected/1'.
    {reply, {error, not_connected}, S};
handle_call({publish, Realm, Topic, Payload}, _From,
            #state{peer_pid = Pid, identity = Id,
                   publish_seq = Seq} = S) ->
    Pub = macula_identity:public(Id),
    Frame = macula_frame:publish(#{
        topic           => Topic,
        realm           => Realm,
        publisher       => Pub,
        seq             => Seq,
        payload         => Payload,
        published_at_ms => erlang:system_time(millisecond)
    }),
    ok = macula_peering:send_frame(Pid, Frame),
    {reply, ok, S#state{publish_seq = Seq + 1}};

handle_call(is_connected, _From, #state{peer_pid = undefined} = S) ->
    {reply, false, S};
handle_call(is_connected, _From, #state{peer_node_id = undefined} = S) ->
    {reply, false, S};
handle_call(is_connected, _From, S) ->
    {reply, true, S};
handle_call(peer_node_id, _From, #state{peer_node_id = undefined} = S) ->
    {reply, {error, not_connected}, S};
handle_call(peer_node_id, _From, #state{peer_node_id = Id} = S) ->
    {reply, {ok, Id}, S};

handle_call({subscribe, Realm, Topic, Subscriber}, _From,
            #state{subscriptions = Subs, topic_index = Idx} = S) ->
    SubRef  = make_ref(),
    Mon     = erlang:monitor(process, Subscriber),
    NewSubs = Subs#{SubRef => {Realm, Topic, Subscriber, Mon}},
    NewIdx  = add_topic_sub(Realm, Topic, SubRef, Idx),
    %% Send the SUBSCRIBE frame now if peering is up; otherwise the
    %% `connected' handler drains every stored subscription on
    %% handshake completion. Avoids the race where a consumer calls
    %% `subscribe/4' immediately after `start_link/1' before the
    %% peering CONNECT/HELLO has finished — the SUBSCRIBE used to
    %% return `{error, not_connected}' and silently never land on
    %% the wire even though the client became connected milliseconds
    %% later.
    maybe_send_subscribe(Realm, Topic, S),
    {reply, {ok, SubRef}, S#state{subscriptions = NewSubs,
                                  topic_index   = NewIdx}};

handle_call({unsubscribe, SubRef}, _From, S) ->
    {reply, ok, on_unsubscribe(SubRef, S)};

handle_call({advertise, Realm, Proc, Handler}, _From,
            #state{procedures = P} = S) ->
    %% Register locally first so that any CALL frame arriving in the
    %% same scheduler tick as the ADVERTISE round-trips correctly.
    %% Replays from the post-HELLO drain pick up the same map.
    NewS = S#state{procedures = P#{{Realm, Proc} => Handler}},
    maybe_send_advertise(Realm, Proc, NewS),
    {reply, ok, NewS};

handle_call({unadvertise, Realm, Proc}, _From,
            #state{procedures = P} = S) ->
    %% Best-effort UNADVERTISE on the wire; ignore disconnected.
    %% Local clear happens regardless so subsequent inbound CALLs for
    %% this procedure surface as `unknown_next_peer' from the relay.
    NewS = S#state{procedures = maps:remove({Realm, Proc}, P)},
    maybe_send_unadvertise(Realm, Proc, S),
    {reply, ok, NewS};

handle_call(_Req, _From, S) ->
    {reply, {error, unknown_call}, S}.

handle_cast(_Msg, S) -> {noreply, S}.

%%-------------------------------------------------------------------
%% Connect
%%-------------------------------------------------------------------

handle_info(attempt_connect, #state{seed = Seed, identity = Id,
                                    capabilities = Caps, alpn = Alpn,
                                    connect_timeout_ms = Tmo} = S) ->
    Pub = macula_identity:public(Id),
    PeeringOpts = #{
        role            => client,
        target          => Seed#{alpn => Alpn, timeout_ms => Tmo},
        node_id         => Pub,
        identity        => Id,
        %% Realm-agnostic: the link advertises no realm membership.
        %% Each frame carries its own realm tag.
        realms          => [],
        capabilities    => Caps,
        controlling_pid => self()
    },
    after_connect_request(macula_peering:connect(PeeringOpts), S);

handle_info({macula_peering, connected, Pid, PeerNodeId},
            #state{peer_pid = Pid} = S) ->
    NewS = S#state{peer_node_id = PeerNodeId},
    drain_pending_subscribes(NewS),
    drain_pending_advertises(NewS),
    {noreply, NewS};

handle_info({macula_peering, frame, Pid, Frame},
            #state{peer_pid = Pid} = S) ->
    {noreply, on_frame(Frame, S)};

handle_info({macula_peering, disconnected, Pid, Reason},
            #state{peer_pid = Pid} = S) ->
    NewS = fail_all_pending({disconnected, Reason}, S),
    %% Stop normally — the supervisor (or owning gen_server) decides
    %% whether to restart us.
    {stop, normal, NewS#state{peer_pid = undefined,
                              peer_node_id = undefined}};

handle_info({call_timeout, CallId}, #state{pending = P} = S) ->
    on_timeout(maps:take(CallId, P), S);

handle_info({'EXIT', Pid, Reason}, #state{peer_pid = Pid} = S) ->
    NewS = fail_all_pending({peering_exit, Reason}, S),
    {stop, normal, NewS#state{peer_pid = undefined,
                              peer_node_id = undefined}};

handle_info({'DOWN', Mon, process, _Pid, _Reason}, S) ->
    {noreply, on_subscriber_down(Mon, S)};

handle_info(_Other, S) ->
    {noreply, S}.

terminate(_Reason, #state{peer_pid = Pid}) when is_pid(Pid) ->
    catch macula_peering:close(Pid, client_stop),
    ok;
terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) -> {ok, S}.

%%====================================================================
%% Internals
%%====================================================================

after_connect_request({ok, Pid}, S) ->
    link(Pid),
    {noreply, S#state{peer_pid = Pid}};
after_connect_request({error, Reason}, S) ->
    macula_diagnostics:event(<<"_macula.station_link.connect_failed">>, #{
        reason => Reason,
        seed   => S#state.seed
    }),
    erlang:send_after(?CONNECT_RETRY_BACKOFF_MS, self(), attempt_connect),
    {noreply, S}.

%% RESULT
on_frame(#{frame_type := result, call_id := CallId, payload := Payload},
         #state{pending = P} = S) ->
    deliver_pending(maps:take(CallId, P), {ok, Payload}, S);
%% ERROR
on_frame(#{frame_type := error, call_id := CallId} = Frame,
         #state{pending = P} = S) ->
    Code = maps:get(code, Frame, 0),
    Name = maps:get(name, Frame, undefined),
    deliver_pending(maps:take(CallId, P),
                    {error, {call_error, Code, Name}}, S);
%% EVENT — pubsub delivery. Fan out to every subscriber whose
%% (realm, topic) matches. Stations may push EVENTs without a prior
%% SUBSCRIBE on this connection (e.g. wildcard / catalog channels);
%% silently drop those.
on_frame(#{frame_type := event, topic := Topic, realm := Realm} = Frame, S) ->
    deliver_event(Realm, Topic, Frame, S);
%% Inbound CALL — relay forwarded a CALL whose (realm, procedure)
%% this link advertised. Dispatch to the registered handler and ship
%% the resulting RESULT or call_error frame back over the same
%% peering connection.
on_frame(#{frame_type := call} = Frame, S) ->
    handle_inbound_call(Frame, S),
    S;
%% HyParView / Plumtree / SWIM / content frames pass through here.
%% This client cares only about call/result/error and event; the
%% rest is for dedicated overlay modules.
on_frame(_Frame, S) ->
    S.

deliver_pending(error, _Reply, S) ->
    %% Unknown call_id (race with timeout, or duplicate reply).
    S;
deliver_pending({{From, TRef}, NewP}, Reply, S) ->
    _ = erlang:cancel_timer(TRef),
    gen_server:reply(From, Reply),
    S#state{pending = NewP}.

on_timeout(error, S) ->
    {noreply, S};
on_timeout({{From, _OldTRef}, NewP}, S) ->
    gen_server:reply(From, {error, timeout}),
    {noreply, S#state{pending = NewP}}.

fail_all_pending(Reason, #state{pending = P, subscriptions = Subs} = S) ->
    maps:foreach(fun(_CallId, {From, TRef}) ->
        _ = erlang:cancel_timer(TRef),
        gen_server:reply(From, {error, Reason})
    end, P),
    maps:foreach(fun(SubRef, {_Realm, _Topic, Subscriber, Mon}) ->
        erlang:demonitor(Mon, [flush]),
        Subscriber ! {macula_event_gone, SubRef, Reason}
    end, Subs),
    S#state{pending = #{}, subscriptions = #{}, topic_index = #{}}.

%%-------------------------------------------------------------------
%% Subscription helpers
%%-------------------------------------------------------------------

add_topic_sub(Realm, Topic, SubRef, Idx) ->
    Key = {Realm, Topic},
    Set = maps:get(Key, Idx, sets:new()),
    Idx#{Key => sets:add_element(SubRef, Set)}.

del_topic_sub(Realm, Topic, SubRef, Idx) ->
    Key = {Realm, Topic},
    on_set_after_del(Key, sets:del_element(SubRef, maps:get(Key, Idx, sets:new())), Idx).

on_set_after_del(Key, Set, Idx) ->
    on_empty_set(sets:is_empty(Set), Key, Set, Idx).

on_empty_set(true,  Key, _Set, Idx) -> maps:remove(Key, Idx);
on_empty_set(false, Key,  Set, Idx) -> Idx#{Key => Set}.

%% Drop a single subscription. Best-effort UNSUBSCRIBE on the wire
%% (drops silently when disconnected — the station prunes stale
%% subscribers eventually). Idempotent: unknown SubRef is a no-op.
on_unsubscribe(SubRef, #state{subscriptions = Subs,
                              topic_index   = Idx,
                              peer_pid      = Pid,
                              identity      = Id} = S) ->
    on_unsubscribe_take(maps:take(SubRef, Subs), SubRef, Idx, Pid, Id, S).

on_unsubscribe_take(error, _SubRef, _Idx, _Pid, _Id, S) ->
    S;
on_unsubscribe_take({{Realm, Topic, _Subscriber, Mon}, NewSubs},
                    SubRef, Idx, Pid, Id, S) ->
    erlang:demonitor(Mon, [flush]),
    NewIdx = del_topic_sub(Realm, Topic, SubRef, Idx),
    send_unsubscribe(Pid, Realm, Topic, Id),
    S#state{subscriptions = NewSubs, topic_index = NewIdx}.

send_unsubscribe(undefined, _Realm, _Topic, _Id) ->
    ok;
send_unsubscribe(Pid, Realm, Topic, Id) ->
    SubKey = macula_identity:public(Id),
    Frame  = macula_frame:unsubscribe(#{topic      => Topic,
                                        realm      => Realm,
                                        subscriber => SubKey}),
    catch macula_peering:send_frame(Pid, Frame),
    ok.

%% Send a SUBSCRIBE frame for `(Realm, Topic)' iff peering is connected.
maybe_send_subscribe(_Realm, _Topic, #state{peer_pid = undefined}) ->
    ok;
maybe_send_subscribe(Realm, Topic, #state{peer_pid = Pid, identity = Id}) ->
    SubKey = macula_identity:public(Id),
    Frame  = macula_frame:subscribe(#{topic      => Topic,
                                      realm      => Realm,
                                      subscriber => SubKey}),
    catch macula_peering:send_frame(Pid, Frame),
    ok.

%% On handshake completion, send a SUBSCRIBE frame for every stored
%% subscription. Subscribers that came in before connect have been
%% sitting in `subscriptions' with no wire frame yet sent — drain
%% them now. De-duplicate by `(Realm, Topic)' since multiple local
%% SubRefs may share the same wire-level subscription (one SUBSCRIBE
%% frame per identity per (realm, topic), reused across consumers).
drain_pending_subscribes(#state{subscriptions = Subs} = S) ->
    Pairs = lists:usort(
              [{R, T} || {_Ref, {R, T, _Sub, _Mon}} <- maps:to_list(Subs)]),
    [maybe_send_subscribe(R, T, S) || {R, T} <- Pairs],
    ok.

%% Subscriber pid died — find its SubRef(s) by monitor ref, drop
%% them. A pid can only have one subscription via one monitor, but
%% scan defensively.
on_subscriber_down(Mon, #state{subscriptions = Subs} = S) ->
    Found = maps:fold(fun
        (SubRef, {_R, _T, _P, M}, Acc) when M =:= Mon -> [SubRef | Acc];
        (_, _, Acc) -> Acc
    end, [], Subs),
    lists:foldl(fun on_unsubscribe/2, S, Found).

%% Fan an EVENT frame out to every subscriber for that (realm, topic).
deliver_event(Realm, Topic, Frame, #state{topic_index = Idx} = S) ->
    deliver_event_to(maps:find({Realm, Topic}, Idx), Realm, Topic, Frame, S),
    S.

deliver_event_to(error, _Realm, _Topic, _Frame, _S) ->
    ok;
deliver_event_to({ok, Set}, Realm, Topic, Frame, #state{subscriptions = Subs}) ->
    Payload = maps:get(payload, Frame),
    Meta = #{realm         => Realm,
             publisher     => maps:get(publisher, Frame),
             seq           => maps:get(seq, Frame),
             delivered_via => maps:get(delivered_via, Frame, direct)},
    sets:fold(fun(SubRef, _) ->
        deliver_event_one(SubRef, Topic, Payload, Meta, Subs)
    end, ok, Set).

deliver_event_one(SubRef, Topic, Payload, Meta, Subs) ->
    fan_event(maps:find(SubRef, Subs), SubRef, Topic, Payload, Meta).

fan_event(error, _SubRef, _Topic, _Payload, _Meta) ->
    ok;
fan_event({ok, {_R, _T, Subscriber, _Mon}}, SubRef, Topic, Payload, Meta) ->
    Subscriber ! {macula_event, SubRef, Topic, Payload, Meta},
    ok.

%%-------------------------------------------------------------------
%% Advertise helpers
%%-------------------------------------------------------------------

%% Send an ADVERTISE frame iff peering is connected. Otherwise the
%% post-HELLO drain replays it. Mirrors `maybe_send_subscribe/3'.
maybe_send_advertise(_Realm, _Procedure, #state{peer_node_id = undefined}) ->
    ok;
maybe_send_advertise(Realm, Procedure,
                     #state{peer_pid = Pid, identity = Id}) ->
    Pub = macula_identity:public(Id),
    Frame = macula_frame:advertise(#{realm      => Realm,
                                     procedure  => Procedure,
                                     advertiser => Pub}),
    catch macula_peering:send_frame(Pid, Frame),
    ok.

%% Best-effort UNADVERTISE on the wire. Disconnected → no-op (the
%% station purges advertised procedures on peer disconnect anyway).
maybe_send_unadvertise(_Realm, _Procedure, #state{peer_node_id = undefined}) ->
    ok;
maybe_send_unadvertise(Realm, Procedure,
                       #state{peer_pid = Pid, identity = Id}) ->
    Pub = macula_identity:public(Id),
    Frame = macula_frame:unadvertise(#{realm      => Realm,
                                       procedure  => Procedure,
                                       advertiser => Pub}),
    catch macula_peering:send_frame(Pid, Frame),
    ok.

%% On handshake completion, send an ADVERTISE frame for every stored
%% procedure. Mirrors `drain_pending_subscribes/1'.
drain_pending_advertises(#state{procedures = Procs} = S) ->
    maps:foreach(fun({Realm, Procedure}, _Handler) ->
        maybe_send_advertise(Realm, Procedure, S)
    end, Procs),
    ok.

%% Inbound CALL — relay forwarded a CALL whose `(realm, procedure)'
%% this link has advertised. Look up the handler, invoke it, and
%% ship the resulting RESULT or call_error frame back over the same
%% peering connection.
%%
%% A handler crash maps to BOLT#4 `temporary_relay_failure' (0x02);
%% an unknown `(realm, procedure)' (race between UNADVERTISE in
%% flight and a stale forwarded CALL) maps to `unknown_next_peer'
%% (0x01) — same taxonomy as `hecate_handler_dispatch'.
handle_inbound_call(#{call_id := CallId, procedure := Proc, realm := Realm,
                      payload := Payload} = _Frame,
                    #state{procedures = Procs, identity = Id,
                           peer_pid = Pid}) when is_pid(Pid) ->
    SelfPub = macula_identity:public(Id),
    Reply   = build_inbound_call_reply(maps:find({Realm, Proc}, Procs),
                                       CallId, Payload, SelfPub),
    macula_peering:send_frame(Pid, Reply),
    ok;
handle_inbound_call(_Frame, _State) ->
    ok.

%% Handler not registered locally — synthesise a signed
%% `unknown_next_peer' BOLT#4 error.
build_inbound_call_reply(error, CallId, _Payload, SelfPub) ->
    macula_frame:call_error(#{call_id     => CallId,
                              code        => 16#01,
                              reported_by => SelfPub});
build_inbound_call_reply({ok, Handler}, CallId, Payload, SelfPub) ->
    safe_invoke_handler(Handler, Payload, CallId, SelfPub).

%% Handler dispatch with crash trap. The try/catch is justified:
%% it converts otherwise-opaque process failures into a structured
%% BOLT#4 error so the caller observes a reliable taxonomy rather
%% than a `{disconnected, killed}' signal when a single bad CALL
%% takes down the link.
safe_invoke_handler(Handler, Payload, CallId, SelfPub) ->
    try invoke_handler(Handler, Payload) of
        Reply ->
            macula_frame:result(#{call_id      => CallId,
                                  payload      => normalise_reply(Reply),
                                  responded_by => SelfPub})
    catch
        Class:Reason:Stack ->
            logger:warning(
              "[station_link] handler crashed: ~p:~p~n  stack=~p",
              [Class, Reason, Stack]),
            macula_frame:call_error(#{call_id     => CallId,
                                      code        => 16#02,
                                      reported_by => SelfPub})
    end.

invoke_handler(Fun, Args) when is_function(Fun, 1) ->
    Fun(Args);
invoke_handler({M, F}, Args) when is_atom(M), is_atom(F) ->
    M:F(Args).

%% Match `hecate_handler_dispatch:normalise/1' so handlers writen
%% against either side return the same shape.
normalise_reply({ok, Value})        -> Value;
normalise_reply({error, _} = Error) -> Error;
normalise_reply(Other)              -> Other.

%%-------------------------------------------------------------------
%% Helpers
%%-------------------------------------------------------------------

parse_seed(#{host := _, port := _} = Map) ->
    Map;
parse_seed(Url) when is_binary(Url) ->
    parse_seed(binary_to_list(Url));
parse_seed(Url) when is_list(Url) ->
    case uri_string:parse(Url) of
        #{host := H, port := P} when is_integer(P) ->
            #{host => list_to_binary(H), port => P};
        #{host := H, scheme := "https"} ->
            #{host => list_to_binary(H), port => 4433};
        _ ->
            error({invalid_seed_url, Url})
    end.
