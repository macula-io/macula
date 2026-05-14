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
    %% Streaming RPC (SDK 3.17+, Part 6 §5.6)
    call_stream/5,
    advertise_stream/5,
    unadvertise_stream/3,
    send_stream_frame/3,
    is_connected/1,
    peer_node_id/1
]).

-export_type([handler/0, stream_handler/0]).

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
    %% ALPN list passed through to QUIC (default [&lt;&lt;"macula"&gt;&gt;]).
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
    procedures = #{} :: #{{<<_:256>>, binary()} => handler()},
    %% Advertised streaming procedures. Same wire shape as `procedures'
    %% (one `advertise' frame per entry replayed on reconnect); the
    %% stored value carries the declared mode (`server_stream' /
    %% `client_stream' / `bidi') plus a 2-arg handler invoked on
    %% inbound STREAM_OPEN. Distinct from `procedures' so the
    %% dispatch path can pick the right shape per inbound frame
    %% (CALL → `procedures'; STREAM_OPEN → `stream_procedures').
    stream_procedures = #{} :: #{{<<_:256>>, binary()} =>
                                 {macula_frame:stream_mode(),
                                  stream_handler()}},
    %% Open streams keyed by 16-byte stream_id. Each entry pairs the
    %% local `macula_stream' pid with the monitor reference returned
    %% when this link started watching it. Split by role so a same-pool
    %% streaming RPC — where the relay bounces the STREAM_OPEN back
    %% over the SAME conn and `spawn_inbound_stream' would otherwise
    %% overwrite the client entry under one shared map — keeps
    %% client-side and server-side state disjoint. Inbound STREAM_DATA
    %% / END / ERROR / REPLY dispatch tries `client_streams' first
    %% (server_stream mode flows server→client; the common case),
    %% then falls through to `server_streams' (client_stream / bidi
    %% server-receive).
    client_streams = #{} :: #{macula_frame:stream_id() =>
                              {pid(), reference()}},
    server_streams = #{} :: #{macula_frame:stream_id() =>
                              {pid(), reference()}}
}).

-type subscription() :: {Realm     :: <<_:256>>,
                         Topic     :: binary(),
                         Subscriber :: pid(),
                         Mon        :: reference()}.

-type handler() :: fun((term()) -> term())
                 | {module(), atom()}.

-type stream_handler() :: fun((pid(), term()) -> any()).

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
%% `&lt;&lt;"_dht.find_records_by_type"&gt;&gt;'. `Payload' is any term that
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

classify_find({ok, #{type := _, payload := _, signature := _} = R}) -> {ok, R};
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
    {ok, reference()} | {error, term()}.
subscribe(Client, Realm, Topic, Subscriber)
  when is_pid(Client),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Topic), is_pid(Subscriber) ->
    gen_server:call(Client, {subscribe, Realm, Topic, Subscriber}, 5_000).

%% @doc Drop a subscription. Sends a best-effort UNSUBSCRIBE frame
%% to the station and clears local bookkeeping. Always returns `ok',
%% even when `SubRef' is unknown — unsubscribe is idempotent.
-spec unsubscribe(pid(), reference()) -> ok | {error, term()}.
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
-spec advertise(pid(), <<_:256>>, binary(), handler()) -> ok | {error, term()}.
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
-spec unadvertise(pid(), <<_:256>>, binary()) -> ok | {error, term()}.
unadvertise(Pid, Realm, Procedure)
  when is_pid(Pid),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure) ->
    gen_server:call(Pid, {unadvertise, Realm, Procedure}, 5_000).

%% @doc Open a streaming RPC on this link. Returns `{ok, StreamPid}'
%% bound to the caller; the caller drives the stream via
%% `macula_stream:send/2,3', `recv/1,2', `close_send/1', `close/1',
%% and `await_reply/1,2' (for client-stream / bidi modes).
%%
%% `Realm' and `Procedure' name the remote streaming endpoint.
%% `Args' is the opening payload (any term that
%% `macula_frame:stream_open/1' accepts). `Opts' may include:
%%
%% <ul>
%%   <li>`mode'  — `server_stream' (default), `client_stream', or
%%                 `bidi'.</li>
%%   <li>`owner' — the pid that owns the stream lifecycle (default:
%%                 the calling pid). Stream stops when the owner
%%                 dies.</li>
%%   <li>`deadline_ms' — wall-clock deadline stamped on the
%%                 STREAM_OPEN frame (default: now + 30s).</li>
%% </ul>
%%
%% Returns `{error, not_connected}' when the QUIC handshake has not
%% completed; the caller may retry once the link reports
%% `is_connected/1'. The pool layer (`macula_client') should be
%% preferred over direct invocation — it picks a healthy link
%% transparently.
-spec call_stream(pid(), <<_:256>>, binary(), term(), map()) ->
    {ok, pid()} | {error, term()}.
call_stream(Pid, Realm, Procedure, Args, Opts)
  when is_pid(Pid),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       is_map(Opts) ->
    gen_server:call(Pid,
                    {stream_open, Realm, Procedure, Args, Opts, self()},
                    5_000).

%% @doc Advertise a streaming RPC handler. Idempotent — re-advertising
%% replaces the prior `{Mode, Handler}'. Replayed on every
%% (re)connect alongside unary advertisements. Wire shape is the
%% existing `advertise' frame; the receiving station routes inbound
%% STREAM_OPEN frames for `(Realm, Procedure)' back over this peering
%% connection where this link spawns a server-side
%% `macula_stream' and dispatches `Handler(StreamPid, Args)' in a
%% transient process.
-spec advertise_stream(pid(), <<_:256>>, binary(),
                        macula_frame:stream_mode(), stream_handler()) ->
    ok | {error, term()}.
advertise_stream(Pid, Realm, Procedure, Mode, Handler)
  when is_pid(Pid),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       (Mode =:= server_stream orelse Mode =:= client_stream
        orelse Mode =:= bidi),
       is_function(Handler, 2) ->
    gen_server:call(Pid,
                    {stream_advertise, Realm, Procedure, Mode, Handler},
                    5_000).

%% @doc Drop a previously-advertised streaming procedure. Idempotent.
-spec unadvertise_stream(pid(), <<_:256>>, binary()) ->
    ok | {error, term()}.
unadvertise_stream(Pid, Realm, Procedure)
  when is_pid(Pid),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure) ->
    gen_server:call(Pid,
                    {stream_unadvertise, Realm, Procedure},
                    5_000).

%% @doc Cast a STREAM_* outbound frame. Called by `macula_stream'
%% processes paired against this link via the
%% `{remote_via_link, Pid, Sid}' peer shape — the stream invokes this
%% to ship its STREAM_DATA / STREAM_END / STREAM_ERROR / STREAM_REPLY
%% bytes through the link's peering connection.
%%
%% Always returns `ok' (the operation is fire-and-forget; the link
%% drops the frame if not yet connected and the stream's own backoff
%% policy decides what to do).
-spec send_stream_frame(pid(), atom(), map()) -> ok.
send_stream_frame(Pid, Type, Spec) when is_pid(Pid), is_atom(Type), is_map(Spec) ->
    gen_server:cast(Pid, {send_stream_frame, Type, Spec}).

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
    Frame0 = macula_frame:publish(#{
        topic           => Topic,
        realm           => Realm,
        publisher       => Pub,
        seq             => Seq,
        payload         => Payload,
        published_at_ms => erlang:system_time(millisecond)
    }),
    %% Optionally attach the publisher-end-to-end signature so a
    %% relay station can carry it onto the EVENT it derives and any
    %% downstream consumer can verify authenticity against the
    %% publisher regardless of which relay delivered the event. Off
    %% by default: a relay on a build that does not strip
    %% `publisher_sig' from a frame's per-hop signing bytes would
    %% reject the PUBLISH, so emission must not be enabled until
    %% every relay is on macula >= 4.4.0. See CHANGELOG 4.4.1 and
    %% macula-station/plans/PLAN_PUBSUB_E2E_SIGNED_EVENTS.md.
    Frame = maybe_add_publisher_sig(Frame0, Id),
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

%%-- Streaming RPC ---------------------------------------------------

handle_call({stream_open, _R, _P, _A, _O, _Caller}, _From,
            #state{peer_node_id = undefined} = S) ->
    %% Mirror the gating used for `call' / `publish' — STREAM_OPEN
    %% frames sent before HELLO completes hit `drop_unexpected' in
    %% the peering statem and never make it to the wire.
    {reply, {error, not_connected}, S};
handle_call({stream_open, Realm, Proc, Args, Opts, Caller}, _From, S) ->
    {reply_value, Reply, NewS} = open_client_stream(Realm, Proc, Args, Opts,
                                                    Caller, S),
    {reply, Reply, NewS};

handle_call({stream_advertise, Realm, Proc, Mode, Handler}, _From,
            #state{stream_procedures = SP} = S) ->
    NewS = S#state{stream_procedures = SP#{{Realm, Proc} => {Mode, Handler}}},
    maybe_send_advertise(Realm, Proc, NewS),
    {reply, ok, NewS};

handle_call({stream_unadvertise, Realm, Proc}, _From,
            #state{stream_procedures = SP} = S) ->
    NewS = S#state{stream_procedures = maps:remove({Realm, Proc}, SP)},
    maybe_send_unadvertise(Realm, Proc, S),
    {reply, ok, NewS};

handle_call(_Req, _From, S) ->
    {reply, {error, unknown_call}, S}.

%%-- Outbound STREAM_* from a paired stream_v1 process ---------------

handle_cast({send_stream_frame, _Type, _Spec},
            #state{peer_pid = undefined} = S) ->
    {noreply, S};
handle_cast({send_stream_frame, Type, Spec},
            #state{peer_pid = Pid, identity = Id} = S) ->
    Frame = build_stream_frame(Type, finalise_stream_spec(Type, Spec, Id)),
    catch macula_peering:send_frame(Pid, Frame),
    {noreply, on_outbound_stream_frame(Type, Spec, S)};

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
    drain_pending_stream_advertises(NewS),
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

handle_info({'DOWN', Mon, process, Pid, _Reason}, S) ->
    %% Two monitor sources land here: subscriber pids paired by
    %% `subscribe/4', and stream pids tracked in `streams'. Probe
    %% the streams map first by pid (cheap), fall back to the
    %% subscriber path on miss.
    {noreply, on_monitor_down(Pid, Mon, S)};

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

%% Attach the publisher-end-to-end signature to an outbound PUBLISH
%% iff `macula' env `pubsub_emit_publisher_sig' is true. Read per
%% publish (a fast ETS lookup) so an operator can flip it without a
%% restart once the relay fleet is confirmed on macula >= 4.4.0.
maybe_add_publisher_sig(Frame, Identity) ->
    case application:get_env(macula, pubsub_emit_publisher_sig, false) of
        true  -> macula_frame:sign_publisher(Frame, Identity);
        _     -> Frame
    end.

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
    on_inbound_event(check_publisher_sig(Frame), Realm, Topic, Frame, S);
%% Inbound CALL — relay forwarded a CALL whose (realm, procedure)
%% this link advertised. Dispatch to the registered handler and ship
%% the resulting RESULT or call_error frame back over the same
%% peering connection.
on_frame(#{frame_type := call} = Frame, S) ->
    handle_inbound_call(Frame, S),
    S;
%% Inbound STREAM_OPEN — relay forwarded a streaming RPC opener.
%% Look up the streaming handler; spawn a server-side stream_v1
%% paired to this link plus a transient handler process.
on_frame(#{frame_type := stream_open} = Frame, S) ->
    handle_inbound_stream_open(Frame, S);
%% STREAM_DATA / STREAM_END / STREAM_ERROR / STREAM_REPLY — chunk
%% delivery on a previously-opened stream. Look up the local
%% stream_v1 pid by stream_id and forward.
on_frame(#{frame_type := stream_data} = Frame, S) ->
    deliver_stream_data(Frame, S);
on_frame(#{frame_type := stream_end} = Frame, S) ->
    deliver_stream_end(Frame, S);
on_frame(#{frame_type := stream_error} = Frame, S) ->
    deliver_stream_error(Frame, S);
on_frame(#{frame_type := stream_reply} = Frame, S) ->
    deliver_stream_reply(Frame, S);
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

fail_all_pending(Reason, #state{pending = P, subscriptions = Subs,
                                client_streams = CS,
                                server_streams = SS} = S) ->
    maps:foreach(fun(_CallId, {From, TRef}) ->
        _ = erlang:cancel_timer(TRef),
        gen_server:reply(From, {error, Reason})
    end, P),
    maps:foreach(fun(SubRef, {_Realm, _Topic, Subscriber, Mon}) ->
        erlang:demonitor(Mon, [flush]),
        Subscriber ! {macula_event_gone, SubRef, Reason}
    end, Subs),
    %% Abort every open stream with a `disconnected' STREAM_ERROR.
    %% Caller waiters (recv / await_reply) unblock immediately;
    %% transient handler processes see the abort and exit.
    AbortFun = fun(_Sid, {Pid, Mon}) ->
        erlang:demonitor(Mon, [flush]),
        catch macula_stream:abort(Pid, <<"disconnected">>,
                                     iolist_to_binary(io_lib:format("~p", [Reason])))
    end,
    maps:foreach(AbortFun, CS),
    maps:foreach(AbortFun, SS),
    S#state{pending = #{}, subscriptions = #{}, topic_index = #{},
            client_streams = #{}, server_streams = #{}}.

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

%% Pubsub Phase 2 — verify the publisher-end-to-end signature on an
%% inbound EVENT if it carries one (a relay propagates `publisher_sig'
%% when the original publisher had `pubsub_emit_publisher_sig'
%% enabled). No `publisher_sig' on the frame → nothing to check
%% (feature off everywhere, or a legacy relay).
check_publisher_sig(#{publisher_sig := _} = Frame) ->
    macula_frame:verify_publisher(Frame);
check_publisher_sig(_Frame) ->
    ok.

%% `ok'           — no publisher_sig present → deliver as before.
%% `{ok, _}'      — publisher_sig verified → deliver.
%% `{error, Why}' — publisher_sig present but invalid: always warn;
%%                  drop only if `pubsub_strict_publisher_sig' is set
%%                  (default lenient — a relay bug should surface, not
%%                  silently lose events, during the Phase 2 rollout).
on_inbound_event(ok, Realm, Topic, Frame, S) ->
    deliver_event(Realm, Topic, Frame, S);
on_inbound_event({ok, _Verified}, Realm, Topic, Frame, S) ->
    deliver_event(Realm, Topic, Frame, S);
on_inbound_event({error, Why}, Realm, Topic, Frame, S) ->
    logger:warning("[macula_pubsub] inbound EVENT publisher_sig invalid (~p)"
                   " realm=~s topic=~s", [Why, hex_prefix(Realm), Topic]),
    on_invalid_publisher_sig(
      application:get_env(macula, pubsub_strict_publisher_sig, false),
      Realm, Topic, Frame, S).

on_invalid_publisher_sig(true, _Realm, _Topic, _Frame, S) ->
    S;
on_invalid_publisher_sig(_Lenient, Realm, Topic, Frame, S) ->
    deliver_event(Realm, Topic, Frame, S).

hex_prefix(B) when is_binary(B), byte_size(B) >= 4 ->
    binary:encode_hex(binary:part(B, 0, 4));
hex_prefix(B) when is_binary(B) ->
    binary:encode_hex(B);
hex_prefix(_) ->
    <<"?">>.

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

%% Handler dispatch with crash trap and error-return funnel.
%%
%% Two failure paths reach the wire as a BOLT#4 `call_error' frame
%% so the caller observes a reliable taxonomy rather than either
%%
%%   * a `{disconnected, killed}' signal when a single bad CALL
%%     takes the link down, or
%%   * a successful-looking RESULT frame whose payload was an
%%     `{error, _}' tuple — the CBOR encoder has no clause for raw
%%     tuples and crashes the peering gen_statem at frame-sign
%%     time, dropping every other multiplexed RPC on the same
%%     connection.
%%
%% Mapping:
%%   * handler returns `{error, Reason}' →
%%     `call_error(code = 0x0F unknown_error,
%%                 detail = format(Reason))'
%%   * handler crashes →
%%     `call_error(code = 0x02 temporary_relay_failure)'
%%   * handler returns anything else →
%%     `result(payload = normalise_reply(Reply))'
safe_invoke_handler(Handler, Payload, CallId, SelfPub) ->
    try invoke_handler(Handler, Payload) of
        {error, Reason} ->
            macula_frame:call_error(#{call_id     => CallId,
                                      code        => 16#0F,
                                      reported_by => SelfPub,
                                      detail      => format_error_detail(Reason)});
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

%% Successful handler returns can be `{ok, Value}', `Value', or any
%% legacy shape — strip the `{ok, _}' wrapper if present, otherwise
%% pass through. `{error, _}' no longer reaches this function: the
%% caller funnels error returns into `call_error' frames first.
normalise_reply({ok, Value}) -> Value;
normalise_reply(Other)       -> Other.

%% BOLT#4 error frames carry an optional `detail' binary. Format the
%% handler's `Reason' with `~p' so the caller sees a faithful, if
%% Erlang-shaped, rendering. Capped at 256 bytes to keep CALL_ERROR
%% frames bounded — bigger reasons are truncated with an ellipsis.
format_error_detail(Reason) ->
    Bin = iolist_to_binary(io_lib:format("~0p", [Reason])),
    case byte_size(Bin) of
        N when N =< 256 -> Bin;
        _ -> <<(binary:part(Bin, 0, 253))/binary, "...">>
    end.

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

%%-------------------------------------------------------------------
%% Streaming RPC — outbound CALL_STREAM (client-side)
%%-------------------------------------------------------------------

%% Spawn a client-side `macula_stream' linked to this link, attach
%% it as a `{remote_via_link, self(), Sid}' peer, then ship the
%% STREAM_OPEN frame. The caller drives the stream from outside; the
%% returned pid is bound to the requested `owner' (default = caller)
%% so a crashing owner tears the stream down.
open_client_stream(Realm, Proc, Args, Opts, Caller,
                   #state{peer_pid = Pid, identity = Id} = S) ->
    Sid       = crypto:strong_rand_bytes(16),
    Mode      = maps:get(mode, Opts, server_stream),
    Owner     = maps:get(owner, Opts, Caller),
    DeadlineMs = maps:get(deadline_ms, Opts,
                          erlang:system_time(millisecond) + 30_000),
    {ok, StreamPid} = macula_stream:start_link(#{
        id    => Sid,
        role  => client,
        mode  => Mode,
        owner => Owner
    }),
    ok = macula_stream:attach_to_link(StreamPid, self(), Sid),
    Mon = erlang:monitor(process, StreamPid),
    Frame = macula_frame:stream_open(#{
        stream_id   => Sid,
        procedure   => Proc,
        realm       => Realm,
        mode        => Mode,
        args        => Args,
        deadline_ms => DeadlineMs,
        caller      => macula_identity:public(Id)
    }),
    catch macula_peering:send_frame(Pid, Frame),
    CS = S#state.client_streams,
    NewS = S#state{client_streams = CS#{Sid => {StreamPid, Mon}}},
    {reply_value, {ok, StreamPid}, NewS}.

%%-------------------------------------------------------------------
%% Streaming RPC — outbound STREAM_DATA / END / ERROR / REPLY
%%-------------------------------------------------------------------

%% Each `macula_stream' bound to this link via the
%% `{remote_via_link, _, Sid}' peer shape casts an outbound frame
%% spec here. Build the corresponding `macula_frame:stream_*' and
%% ship through the peering connection. Outbound STREAM_END (full
%% close), STREAM_ERROR, or STREAM_REPLY also drop the local
%% routing entry — the stream is finished from our side.
build_stream_frame(stream_data, Spec)  -> macula_frame:stream_data(Spec);
build_stream_frame(stream_end, Spec)   -> macula_frame:stream_end(Spec);
build_stream_frame(stream_error, Spec) -> macula_frame:stream_error(Spec);
build_stream_frame(stream_reply, Spec) -> macula_frame:stream_reply(Spec).

%% `stream_reply' carries `responded_by' (the link's own pubkey)
%% which the v1 stream gen_server has no way to know. Stamp it on
%% before frame construction; other types pass through untouched.
finalise_stream_spec(stream_reply, Spec, Id) ->
    Spec#{responded_by => macula_identity:public(Id)};
finalise_stream_spec(_Type, Spec, _Id) ->
    Spec.

%% After sending an outbound terminal frame, drop the local routing
%% entry. STREAM_DATA does not terminate; STREAM_END does only on
%% `role = both'.
on_outbound_stream_frame(stream_end, #{role := both, stream_id := Sid}, S) ->
    drop_stream(Sid, S);
on_outbound_stream_frame(stream_error, #{stream_id := Sid}, S) ->
    drop_stream(Sid, S);
on_outbound_stream_frame(stream_reply, #{stream_id := Sid}, S) ->
    drop_stream(Sid, S);
on_outbound_stream_frame(_Type, _Spec, S) ->
    S.

%% Terminal frames (stream_end role=both, stream_error, stream_reply)
%% close the stream from both ends. Drop the Sid from whichever map
%% holds it. Same-pool case has the same Sid in BOTH maps; drop both
%% so the link doesn't leak entries.
drop_stream(Sid, #state{client_streams = CS,
                        server_streams = SS} = S) ->
    {CS2, ClientMon} = drop_one(Sid, CS),
    {SS2, ServerMon} = drop_one(Sid, SS),
    _ = [erlang:demonitor(M, [flush])
         || M <- [ClientMon, ServerMon], M =/= undefined],
    S#state{client_streams = CS2, server_streams = SS2}.

drop_one(Sid, Map) ->
    case maps:take(Sid, Map) of
        error                -> {Map, undefined};
        {{_Pid, Mon}, NewMap} -> {NewMap, Mon}
    end.

%%-------------------------------------------------------------------
%% Streaming RPC — inbound STREAM_OPEN (server-side dispatch)
%%-------------------------------------------------------------------

%% The relay forwarded a STREAM_OPEN whose `(Realm, Procedure)' this
%% link advertised. Spawn a server-side stream_v1 paired to this
%% link, store it, then dispatch the registered handler in a
%% transient process so a slow / crashing handler can't block the
%% link's gen_server.
handle_inbound_stream_open(#{stream_id := Sid, procedure := Proc,
                              realm := Realm, args := Args} = Frame, S) ->
    DeclaredMode = maps:get(mode, Frame, server_stream),
    dispatch_stream_open(maps:find({Realm, Proc}, S#state.stream_procedures),
                         Sid, Proc, DeclaredMode, Args, S).

%% Unknown (Realm, Procedure) → ship a STREAM_ERROR back so the
%% client unblocks immediately rather than waiting for its deadline.
dispatch_stream_open(error, Sid, _Proc, _Declared, _Args,
                     #state{peer_pid = Pid} = S) when is_pid(Pid) ->
    Frame = macula_frame:stream_error(#{
        stream_id => Sid,
        code      => <<"not_found">>,
        message   => <<"procedure not advertised">>
    }),
    catch macula_peering:send_frame(Pid, Frame),
    S;
dispatch_stream_open(error, _Sid, _Proc, _Declared, _Args, S) ->
    S;
dispatch_stream_open({ok, {AdvMode, Handler}}, Sid, Proc, _Declared, Args, S) ->
    %% Advertised mode wins — the server declared the shape.
    spawn_inbound_stream(Sid, Proc, AdvMode, Handler, Args, S).

spawn_inbound_stream(Sid, Proc, Mode, Handler, Args,
                     #state{server_streams = SS} = S) ->
    Host = spawn(fun() -> receive stop -> ok end end),
    {ok, StreamPid} = macula_stream:start_link(#{
        id    => Sid,
        role  => server,
        mode  => Mode,
        owner => Host
    }),
    ok = macula_stream:attach_to_link(StreamPid, self(), Sid),
    Mon = erlang:monitor(process, StreamPid),
    _Worker = spawn_stream_handler(Handler, StreamPid, Args, Proc),
    S#state{server_streams = SS#{Sid => {StreamPid, Mon}}}.

%% Handler runs in a transient process. A handler crash maps to a
%% STREAM_ERROR abort with the crash class as the code so callers
%% see a stable error taxonomy. The try/catch is justified (mirrors
%% `safe_invoke_handler/4' for unary CALLs): without it a crash
%% would silently leave the caller waiting on its deadline.
spawn_stream_handler(Handler, Stream, Args, Proc) ->
    spawn(fun() ->
        try Handler(Stream, Args)
        catch
            Class:Reason:Stack ->
                Code = atom_to_binary(Class, utf8),
                Msg = iolist_to_binary(io_lib:format(
                    "stream handler ~s crashed: ~p:~p~n~p",
                    [Proc, Class, Reason, Stack])),
                _ = macula_stream:abort(Stream, Code, Msg)
        end
    end).

%%-------------------------------------------------------------------
%% Streaming RPC — inbound STREAM_DATA / END / ERROR / REPLY
%%-------------------------------------------------------------------

%% Unknown stream_id (race with terminal frame from our side) is
%% silently dropped — same policy as `mesh_client'. Lookup order:
%% client_streams first (server_stream mode flows server→client, the
%% common case; in same-pool both maps hold Sid and the bounced
%% server-emitted STREAM_DATA must reach the caller's recv waiter),
%% then server_streams (client_stream / bidi server-receive).
deliver_stream_data(#{stream_id := Sid} = Frame, S) ->
    deliver_to_stream(find_stream(Sid, S),
                      fun({Pid, _Mon}) ->
                          macula_stream:deliver_chunk(
                            Pid,
                            maps:get(encoding, Frame, raw),
                            maps:get(body, Frame, <<>>))
                      end),
    S.

deliver_stream_end(#{stream_id := Sid} = Frame, S) ->
    Role = maps:get(role, Frame, both),
    deliver_to_stream(find_stream(Sid, S),
                      fun({Pid, _Mon}) ->
                          macula_stream:deliver_end(Pid, Role)
                      end),
    %% Full close drops the routing entry; half close keeps it open
    %% for outbound chunks back to the peer.
    forget_on_full_close(Role, Sid, S).

deliver_stream_error(#{stream_id := Sid} = Frame, S) ->
    Code = maps:get(code, Frame, <<"error">>),
    Message = maps:get(message, Frame, <<>>),
    deliver_to_stream(find_stream(Sid, S),
                      fun({Pid, _Mon}) ->
                          macula_stream:deliver_error(Pid, Code, Message)
                      end),
    drop_stream(Sid, S).

deliver_stream_reply(#{stream_id := Sid, payload := Payload}, S) ->
    deliver_to_stream(find_stream(Sid, S),
                      fun({Pid, _Mon}) ->
                          macula_stream:deliver_reply(Pid, {ok, Payload})
                      end),
    S.

deliver_to_stream(error, _Fun) ->
    ok;
deliver_to_stream({ok, Entry}, Fun) ->
    _ = Fun(Entry),
    ok.

forget_on_full_close(both, Sid, S) -> drop_stream(Sid, S);
forget_on_full_close(_, _, S)      -> S.

%%-------------------------------------------------------------------
%% Streaming RPC — replay on (re)connect
%%-------------------------------------------------------------------

%% Mirror `drain_pending_advertises/1' for streaming procedures. The
%% wire frame is the existing `advertise' (no separate streaming
%% advertise frame); the link's local `stream_procedures' map
%% remains the source of truth for mode-aware dispatch.
drain_pending_stream_advertises(#state{stream_procedures = SP} = S) ->
    maps:foreach(fun({Realm, Procedure}, _Entry) ->
        maybe_send_advertise(Realm, Procedure, S)
    end, SP),
    ok.

%%-------------------------------------------------------------------
%% Streaming RPC — DOWN routing (stream pid vs subscriber pid)
%%-------------------------------------------------------------------

%% Probe the client_streams and server_streams maps by pid; fall back
%% to the subscriber path. Stream pids are added by
%% `open_client_stream/6' (client_streams) and `spawn_inbound_stream/6'
%% (server_streams).
on_monitor_down(Pid, Mon, #state{client_streams = CS,
                                 server_streams = SS} = S) ->
    case find_stream_by_pid(Pid, CS) of
        {ok, Sid} ->
            erlang:demonitor(Mon, [flush]),
            S#state{client_streams = maps:remove(Sid, CS)};
        error ->
            case find_stream_by_pid(Pid, SS) of
                {ok, Sid} ->
                    erlang:demonitor(Mon, [flush]),
                    S#state{server_streams = maps:remove(Sid, SS)};
                error ->
                    on_subscriber_down(Mon, S)
            end
    end.

%% Lookup a stream by Sid across both maps. Client-side first (the
%% common server_stream mode delivers server→client chunks to the
%% client entry); fall back to server-side for client_stream / bidi
%% server-receive.
find_stream(Sid, #state{client_streams = CS, server_streams = SS}) ->
    case maps:find(Sid, CS) of
        {ok, _} = R -> R;
        error       -> maps:find(Sid, SS)
    end.

find_stream_by_pid(Pid, Streams) ->
    Found = [Sid || {Sid, {P, _}} <- maps:to_list(Streams), P =:= Pid],
    first_or_error(Found).

first_or_error([H | _]) -> {ok, H};
first_or_error([])      -> error.
