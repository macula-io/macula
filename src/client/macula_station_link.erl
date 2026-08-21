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
%%   <tr><td>ERROR(code=0x0F, detail=D)</td><td>`{error, D}' — the handler's own reason</td></tr>
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
    call/6,
    publish/4,
    publish/5,
    put_record/2, put_record/3,
    find_record/2, find_record/3,
    find_records_by_type/2, find_records_by_type/3,
    subscribe/4,
    unsubscribe/2,
    advertise/4,
    advertise/5,
    unadvertise/3,
    %% Streaming RPC (SDK 3.17+, Part 6 §5.6)
    call_stream/5,
    advertise_stream/5,
    unadvertise_stream/3,
    send_stream_frame/3,
    is_connected/1,
    peer_node_id/1,
    %% Dedicated-stream content transfer (PLAN_PER_STREAM_QUIC_ISOLATION.md
    %% Phase 2). Not for general RPC use — see the moduledoc on
    %% `open_content_stream/1'.
    open_content_stream/1,
    call_on_stream/6,
    close_content_stream/2,
    abort_content_stream/4
]).

-export_type([handler/0, stream_handler/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([opts/0]).

-type url() :: binary() | string().

-type opts() :: #{
    %% Endpoint to dial. Either a URL (https://host:port) or a
    %% pre-parsed #{host, port} map. The map form may carry the
    %% optional `macula_peering_conn:connect_opts()' trust keys,
    %% forwarded verbatim into the dial target:
    %%   `expected_node_id' — pin the relay's Ed25519 identity (TLS
    %%       SPKI pin + HELLO node_id binding), strongest;
    %%   `verify' — `webpki' (default) or `none' (dev/self-signed
    %%       only; logs a warning per dial).
    seed     := url() | #{host := binary() | string(),
                          port := inet:port_number(),
                          _    => _},
    %% Local Ed25519 keypair used to sign the CONNECT frame and any
    %% subsequent application frames. Auto-generated when absent.
    identity => macula_identity:key_pair(),
    %% Capability bitfield announced in CONNECT (default 0).
    capabilities => non_neg_integer(),
    %% ALPN list passed through to QUIC (default [&lt;&lt;"macula"&gt;&gt;]).
    alpn         => [binary()],
    %% Connect timeout in ms (default 30_000).
    connect_timeout_ms => non_neg_integer(),
    %% App-liveness probe interval in ms (default 30_000) and the
    %% consecutive-miss count that recycles the link (default 2). Widen
    %% both for a pool of links to busy stations that answer the probe
    %% slowly; keep the tight default where fast zombie detection matters.
    liveness_interval_ms => non_neg_integer(),
    liveness_max_misses  => non_neg_integer(),
    %% Backoff in ms before re-dialling after a failed connect (default
    %% 1_000). Raise on a pool that cycles links to soften reconnect storms.
    connect_retry_backoff_ms => non_neg_integer()
    %% QUIC transport knobs (`idle_timeout_ms' default 300_000,
    %% `keep_alive_interval_ms' default 15_000, `peer_bidi_stream_count',
    %% `peer_unidi_stream_count') may additionally be carried in the `seed'
    %% map — station_link merges it into the dial target verbatim.
}.

-define(DHT_REALM, <<0:256>>).
-define(DEFAULT_DEADLINE_MS, 5_000).
-define(CONNECT_RETRY_BACKOFF_MS, 1_000).

%% App-level liveness probe. Sends a tiny CALL (`_macula.ping' on the
%% DHT realm, no handler expected — station replies with
%% `unknown_next_peer') every `?LIVENESS_INTERVAL_MS' and tracks the
%% outstanding probe's call_id. On `?LIVENESS_MAX_MISSES' consecutive
%% misses (i.e. no reply received within the next tick), close
%% `peer_pid' to force the supervisor / pool layer to respawn a fresh
%% link. Closes the "QUIC layer keeps connection alive but server
%% application has no record of us" zombie window — empirically
%% observed at 14+ minutes after a server-side container restart
%% (idle_timeout=300s never fires because the server's Quinn still
%% ACKs our keep-alive PINGs at the transport layer).
-define(LIVENESS_INTERVAL_MS, 30_000).
-define(LIVENESS_MAX_MISSES,  2).
-define(LIVENESS_PROCEDURE,   <<"_macula.ping">>).

%% Grace added on top of `connect_timeout_ms' before the connect
%% watchdog fires. The dial NIF is meant to bound itself at
%% `connect_timeout_ms'; the grace covers CONNECT/HELLO frame exchange
%% after the QUIC layer is up. If the whole thing hasn't produced a
%% `connected' message by `connect_timeout_ms + grace', the worker is
%% wedged and we recycle the link.
-define(CONNECT_WATCHDOG_GRACE_MS, 10_000).

-record(state, {
    seed             :: #{host := binary() | string(),
                          port := inet:port_number(),
                          _    => _},
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
    %% Per-procedure auth policy. Absent = `open' (serve any identified
    %% caller). `{ucan_required, Issuer}' gates the procedure: an inbound
    %% CALL must carry a `ucan_token' that verifies against `Issuer', else
    %% the link refuses with BOLT#4 `unauthorized'. Direct-dial dual-trust
    %% (Slice 7b).
    policies   = #{} :: #{{<<_:256>>, binary()} => macula_client:auth_policy()},
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
    %% Third element is the dedicated QUIC stream this session's
    %% frames travel on (see PLAN_PER_STREAM_QUIC_ISOLATION.md) —
    %% opened via `macula_peering:open_dedicated_stream/1' on the
    %% outbound (client) side, handed off from a `new_dedicated_stream'
    %% notification on the inbound (server) side. Every session has
    %% one; there is no shared-control-stream fallback.
    client_streams = #{} :: #{macula_frame:stream_id() =>
                              {pid(), reference(), reference()}},
    server_streams = #{} :: #{macula_frame:stream_id() =>
                              {pid(), reference(), reference()}},
    %% Inbound byte buffer per dedicated QUIC stream, keyed by the
    %% QUIC stream reference itself (stable for the stream's life,
    %% known before any frame — let alone its `stream_id' — has been
    %% decoded off it). Entries are created the moment a
    %% `new_dedicated_stream' notification arrives and removed when
    %% the session tears down.
    stream_bufs = #{} :: #{reference() => binary()},
    %% Dedicated content-transfer streams (PLAN_PER_STREAM_QUIC_ISOLATION.md
    %% Phase 2). One `put_content'/`get_content' call pins one link and
    %% opens one of these via `open_content_stream/1', then issues every
    %% block/manifest CALL for that transfer on it via `call_on_stream/6'
    %% — sequentially, never concurrently, so unlike `pending' (keyed by
    %% CALL id, many outstanding at once) this needs no per-call id: at
    %% most one entry per stream reference at any time.
    %% `content_stream_bufs' buffers partial frames the same way
    %% `stream_bufs' does for streaming-RPC dedicated streams; a content
    %% stream is a wholly separate reference space from `client_streams'
    %% / `server_streams' even though the underlying primitive
    %% (`macula_peering:open_dedicated_stream/1') is the same one.
    content_stream_bufs = #{} :: #{reference() => binary()},
    content_pending = #{}     :: #{reference() => {gen_server:from(), reference()}},
    %% App-level liveness state. `liveness_timer' is the next-tick
    %% reference (or undefined when not armed). `liveness_outstanding'
    %% holds the call_id of an in-flight probe (or undefined when no
    %% probe is awaiting reply). `liveness_misses' is the consecutive-
    %% miss count; reaches `?LIVENESS_MAX_MISSES' → close peer_pid.
    liveness_timer        :: undefined | reference(),
    liveness_outstanding  :: undefined | <<_:128>>,
    liveness_misses = 0   :: non_neg_integer(),
    %% Tunable liveness thresholds (start opts `liveness_interval_ms' /
    %% `liveness_max_misses', each defaulting to the module `?LIVENESS_*'
    %% value). A consumer holding many links to variously-loaded stations
    %% (the realm's station pool) can widen these so a slow-but-alive
    %% station is not recycled on a transient probe miss; the daemon keeps
    %% the tight default for fast zombie detection.
    liveness_interval_ms  :: non_neg_integer(),
    liveness_max_misses   :: non_neg_integer(),
    %% Backoff before re-attempting a connect after a failed dial (start
    %% opt `connect_retry_backoff_ms', default `?CONNECT_RETRY_BACKOFF_MS').
    %% A busy pool cycling links raises this to soften reconnect storms.
    connect_retry_backoff_ms :: non_neg_integer(),
    %% Connect/handshake watchdog. Armed the moment the peering worker
    %% is spawned (peer_pid set) and cancelled on `connected'. If it
    %% fires while `peer_node_id' is still undefined the CONNECT/HELLO
    %% handshake never completed within the deadline — the peering
    %% worker is wedged (e.g. a QUIC dial NIF that hangs past its own
    %% timeout, or a stalled handshake that emitted no `disconnected').
    %% We kill the worker and stop so the owner (pool / subscriber)
    %% respawns a fresh link. This is the ONLY bound on the
    %% un-connected phase: the app-liveness probe only arms AFTER
    %% `connected', so without this a link that never finishes
    %% handshaking sits alive-but-dead forever with no self-heal.
    connect_watchdog      :: undefined | reference(),
    %% Optional explicit watchdog deadline. When unset it is derived
    %% as `connect_timeout_ms + ?CONNECT_WATCHDOG_GRACE_MS'. Exposed as
    %% the `connect_watchdog_ms' start opt for operational tuning and
    %% for tests that need a short deadline.
    connect_watchdog_ms   :: undefined | non_neg_integer()
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
call(Pid, Realm, Procedure, Payload, TimeoutMs) ->
    call(Pid, Realm, Procedure, Payload, TimeoutMs, <<>>).

%% @doc As `call/5', presenting a capability token (UCAN) to a gated
%% provider. Empty token = none. Slice 7b.
-spec call(pid(), <<_:256>>, binary(), term(), pos_integer(), binary()) ->
    {ok, term()} | {error, term()}.
call(Pid, Realm, Procedure, Payload, TimeoutMs, UcanToken)
  when is_pid(Pid),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       is_integer(TimeoutMs), TimeoutMs > 0,
       is_binary(UcanToken) ->
    %% gen_server timeout = TimeoutMs + 500 to give the server time to
    %% report a clean `{error, timeout}' rather than the caller seeing
    %% a hard `exit({timeout, ...})'.
    GenTimeout = TimeoutMs + 500,
    try
        gen_server:call(Pid,
                        {call, Realm, Procedure, Payload, TimeoutMs, UcanToken},
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

%% @doc Open a dedicated QUIC stream for a sequence of related unary
%% CALLs — content transfer's one purpose so far (see
%% PLAN_PER_STREAM_QUIC_ISOLATION.md Phase 2). NOT a general-purpose
%% "any RPC can have its own stream" facility: `call/5,6' remains the
%% right choice for an ordinary one-off CALL, and this link's pool
%% caller is expected to have already picked ONE link for the whole
%% sequence (`macula_client:pick_connected_link/1') before opening a
%% stream on it, since a dedicated stream only isolates traffic on
%% the link it was opened on.
-spec open_content_stream(pid()) -> {ok, reference()} | {error, term()}.
open_content_stream(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, open_content_stream, 10_000).

%% @doc Send a CALL on `Stream' (from `open_content_stream/1') and
%% block for its RESULT/ERROR on that same stream. Sequential by
%% design — sending a second CALL on `Stream' before the first
%% replies is a caller bug (undefined which reply matches which
%% call), so this link only ever tracks one outstanding call per
%% content stream.
-spec call_on_stream(pid(), reference(), <<_:256>>, binary(), term(),
                     pos_integer()) -> {ok, term()} | {error, term()}.
call_on_stream(Pid, Stream, Realm, Procedure, Payload, TimeoutMs)
  when is_pid(Pid), is_reference(Stream),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       is_integer(TimeoutMs), TimeoutMs > 0 ->
    GenTimeout = TimeoutMs + 500,
    try
        gen_server:call(Pid,
                        {call_on_stream, Stream, Realm, Procedure, Payload,
                         TimeoutMs},
                        GenTimeout)
    catch
        exit:{timeout, _} -> {error, timeout};
        exit:{noproc, _}  -> {error, noproc};
        exit:{normal, _}  -> {error, gone}
    end.

%% @doc Close a content stream opened via `open_content_stream/1'.
%% Idempotent; a stream already closed by disconnect cleanup is a
%% no-op. Any pending call on `Stream' is failed with
%% `{error, closed}' first, so a caller that closes out from under
%% its own in-flight `call_on_stream/6' gets a clean reply instead of
%% a hang.
-spec close_content_stream(pid(), reference()) -> ok.
close_content_stream(Pid, Stream) when is_pid(Pid), is_reference(Stream) ->
    gen_server:cast(Pid, {close_content_stream, Stream}).

%% @doc Abort a content stream opened via `open_content_stream/1' —
%% the cancel-with-a-real-signal counterpart to `close_content_stream/2'.
%% Resets `Stream''s send side with `Code' via
%% `macula_quic:reset_stream/2', a QUIC RESET_STREAM frame the PEER's
%% own read genuinely observes (`{quic, stream_closed, PeerStream,
%% {reset, Code}}' — see `macula_content_transfer', PLAN_PUSH_UPLOAD.md
%% Phase 1), not merely a dropped connection to infer from the way
%% `close_content_stream/2''s graceful FIN is. Any pending call on
%% `Stream' is failed with `{error, cancelled}' (distinct from
%% `close_content_stream/2''s `{error, closed}' — the caller asked for
%% this one, it didn't just lose its connection). `Message' is local
%% diagnostics only; QUIC RESET_STREAM carries only the numeric `Code'
%% on the wire, no string.
-spec abort_content_stream(pid(), reference(), non_neg_integer(), binary()) -> ok.
abort_content_stream(Pid, Stream, Code, Message)
  when is_pid(Pid), is_reference(Stream), is_integer(Code), Code >= 0,
       is_binary(Message) ->
    gen_server:cast(Pid, {abort_content_stream, Stream, Code, Message}).

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

%% @doc Publish with a caller-supplied monotonic `Seq'. The pool
%% (`macula_client') owns the sequence so the station-side
%% `{publisher, seq}' dedup stays stable across link respawns; the
%% per-link `publish/4' counter is only for standalone (pool-less)
%% link use.
-spec publish(pid(), <<_:256>>, binary(), term(), non_neg_integer()) ->
    ok | {error, not_connected | term()}.
publish(Pid, Realm, Topic, Payload, Seq)
  when is_pid(Pid),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Topic),
       is_integer(Seq), Seq >= 0 ->
    gen_server:call(Pid, {publish, Realm, Topic, Payload, Seq}, 5_000).

%% @doc Convenience wrapper for `_dht.put_record'. The record must be
%% a fully-signed `macula_record:m_record()' map (build via
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
advertise(Pid, Realm, Procedure, Handler) ->
    advertise(Pid, Realm, Procedure, Handler, open).

%% @doc Advertise with an auth policy (`open' | `{ucan_required, Issuer}').
-spec advertise(pid(), <<_:256>>, binary(), handler(),
                macula_client:auth_policy()) -> ok.
advertise(Pid, Realm, Procedure, Handler, Policy)
  when is_pid(Pid),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       (is_function(Handler, 1) orelse
        (is_tuple(Handler) andalso tuple_size(Handler) =:= 2)) ->
    gen_server:call(Pid, {advertise, Realm, Procedure, Handler, Policy}, 5_000).

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
    %% TLS policy (`verify' / `expected_node_id') rides in the seed map,
    %% which is spread into the peering target at connect — so a caller
    %% can dial a self-signed or pubkey-pinned station, same as the
    %% station-side outbound link.
    Seed     = add_tls_opts(parse_seed(maps:get(seed, Opts)), Opts),
    Identity = maps:get(identity, Opts, macula_identity:generate()),
    Caps     = maps:get(capabilities, Opts, 0),
    Alpn     = maps:get(alpn, Opts, [<<"macula">>]),
    Tmo      = maps:get(connect_timeout_ms, Opts, 30_000),
    WdMs     = maps:get(connect_watchdog_ms, Opts, undefined),
    LiveMs   = maps:get(liveness_interval_ms, Opts, app_env(liveness_interval_ms, ?LIVENESS_INTERVAL_MS)),
    LiveMiss = maps:get(liveness_max_misses, Opts, app_env(liveness_max_misses, ?LIVENESS_MAX_MISSES)),
    RetryMs  = maps:get(connect_retry_backoff_ms, Opts, app_env(connect_retry_backoff_ms, ?CONNECT_RETRY_BACKOFF_MS)),
    State    = #state{seed = Seed, identity = Identity,
                      capabilities = Caps, alpn = Alpn,
                      connect_timeout_ms = Tmo,
                      connect_watchdog_ms = WdMs,
                      liveness_interval_ms = LiveMs,
                      liveness_max_misses = LiveMiss,
                      connect_retry_backoff_ms = RetryMs},
    process_flag(trap_exit, true),
    self() ! attempt_connect,
    {ok, State}.

%% Fall back to the `macula' application environment when a tuning opt is not
%% passed explicitly in `start_link/1'. A consumer with many links spread across
%% subsystems (the realm holds ~64 via its Mesh pool, subscribers, and the
%% topology pool) can then widen liveness/backoff GLOBALLY from one config point
%% -- `config :macula, liveness_max_misses: N' -- instead of threading the opt
%% through every link-creation site. The `?DEFINE' stays the ground default.
app_env(Key, Default) ->
    application:get_env(macula, Key, Default).

handle_call({call, _Realm, _Proc, _Payload, _Tmo, _Ucan}, _From,
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
handle_call({call, Realm, Proc, Payload, Tmo, Ucan}, From,
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
        caller      => Caller,
        ucan_token  => Ucan
    }),
    %% NOT `ok = send_frame(...)'. Since the frame is now checked before
    %% the cast, an unsendable RPC payload comes back as an error, and a
    %% hard match on `ok' would badmatch here and take this link's
    %% gen_server down — turning a caller's bad argument into an outage
    %% for every other caller on the link. Reply with the reason instead.
    await_call_reply(macula_peering:send_frame(Pid, Frame),
                     CallId, From, Tmo, P, S);

handle_call(open_content_stream, _From, #state{peer_node_id = undefined} = S) ->
    {reply, {error, not_connected}, S};
handle_call(open_content_stream, _From,
            #state{peer_pid = Pid, content_stream_bufs = Bufs} = S) ->
    open_content_stream_result(macula_peering:open_dedicated_stream(Pid), Bufs, S);

handle_call({call_on_stream, _Stream, _Realm, _Proc, _Payload, _Tmo}, _From,
            #state{peer_node_id = undefined} = S) ->
    {reply, {error, not_connected}, S};
handle_call({call_on_stream, Stream, Realm, Proc, Payload, Tmo}, From,
            #state{identity = Id, content_pending = CP,
                   content_stream_bufs = Bufs} = S)
        when is_map_key(Stream, Bufs) ->
    Caller = macula_identity:public(Id),
    DeadlineMs = erlang:system_time(millisecond) + Tmo,
    Frame = macula_frame:call(#{
        call_id     => crypto:strong_rand_bytes(16),
        procedure   => Proc,
        realm       => Realm,
        payload     => Payload,
        deadline_ms => DeadlineMs,
        caller      => Caller,
        ucan_token  => <<>>
    }),
    await_content_call_reply(
      send_on_content_stream(Stream, Frame, Id), Stream, From, Tmo, CP, S);
handle_call({call_on_stream, _Stream, _Realm, _Proc, _Payload, _Tmo}, _From, S) ->
    {reply, {error, invalid_stream}, S};

handle_call({publish, _Realm, _Topic, _Payload}, _From,
            #state{peer_node_id = undefined} = S) ->
    %% Require the full HELLO handshake before publishing — the
    %% peering worker may exist mid-handshake while the wire is not
    %% yet ready for application frames. Matches `is_connected/1'.
    {reply, {error, not_connected}, S};
handle_call({publish, _Realm, _Topic, _Payload, _Seq}, _From,
            #state{peer_node_id = undefined} = S) ->
    {reply, {error, not_connected}, S};
handle_call({publish, Realm, Topic, Payload}, _From,
            #state{publish_seq = Seq} = S) ->
    %% Standalone (pool-less) publish: fall back to the per-link
    %% counter. Pool-driven publishes use `publish/5' with the pool's
    %% own monotone seq (see `macula_client').
    %% NOT `ok = ...'. send_publish_frame/5 returns the seam's verdict,
    %% so a hard match here kills this link's gen_server — subscriptions,
    %% pending calls and streams with it — over one caller's bad payload.
    %% A refused frame also does NOT consume a seq: nothing reached the
    %% wire, and burning the number would fake a gap in the
    %% (publisher, seq) sequence the station dedup keys on.
    publish_reply(send_publish_frame(Realm, Topic, Payload, Seq, S), Seq, S);
handle_call({publish, Realm, Topic, Payload, Seq}, _From, S) ->
    %% Pool-driven: the pool owns the seq, so there is none to advance.
    {reply, send_publish_frame(Realm, Topic, Payload, Seq, S), S};

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

handle_call({advertise, Realm, Proc, Handler, Policy}, _From,
            #state{procedures = P, policies = Pols} = S) ->
    %% Register locally first so that any CALL frame arriving in the
    %% same scheduler tick as the ADVERTISE round-trips correctly.
    %% Replays from the post-HELLO drain pick up the same map.
    NewS = S#state{procedures = P#{{Realm, Proc} => Handler},
                   policies   = set_policy({Realm, Proc}, Policy, Pols)},
    maybe_send_advertise(Realm, Proc, NewS),
    {reply, ok, NewS};

handle_call({unadvertise, Realm, Proc}, _From,
            #state{procedures = P, policies = Pols} = S) ->
    %% Best-effort UNADVERTISE on the wire; ignore disconnected.
    %% Local clear happens regardless so subsequent inbound CALLs for
    %% this procedure surface as `unknown_next_peer' from the relay.
    NewS = S#state{procedures = maps:remove({Realm, Proc}, P),
                   policies   = maps:remove({Realm, Proc}, Pols)},
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
handle_cast({send_stream_frame, Type, #{stream_id := Sid} = Spec},
            #state{identity = Id} = S) ->
    Frame = build_stream_frame(Type, finalise_stream_spec(Type, Spec, Id)),
    send_on_dedicated_stream(find_stream(Sid, S), Frame, Id),
    {noreply, on_outbound_stream_frame(Type, Spec, S)};

handle_cast({close_content_stream, Stream}, S) ->
    {noreply, close_content_stream_state(Stream, S)};

handle_cast({abort_content_stream, Stream, Code, Message}, S) ->
    macula_diagnostics:event(<<"_macula.station_link.content_abort">>,
                             #{stream => Stream, code => Code,
                               message => Message}),
    {noreply, abort_content_stream_state(Stream, Code, S)};

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
    %% Handshake completed — cancel the connect watchdog and hand over
    %% to the steady-state app-liveness probe.
    S1 = cancel_connect_watchdog(S),
    NewS = arm_liveness(S1#state{peer_node_id = PeerNodeId,
                                 liveness_misses = 0,
                                 liveness_outstanding = undefined}),
    drain_pending_subscribes(NewS),
    drain_pending_advertises(NewS),
    drain_pending_stream_advertises(NewS),
    {noreply, NewS};

handle_info({macula_peering, frame, Pid, Frame},
            #state{peer_pid = Pid} = S) ->
    {noreply, fold_frames(drain_frames(Pid, [Frame]), S)};

%% 5-tuple variant: peering_conn opted into `timing_enabled', appended
%% the monotonic-microsecond timestamp captured the instant the frame
%% finished decoding. station_link does not act on it yet (Phase 1
%% telemetry is station-side); kept for forward compatibility.
handle_info({macula_peering, frame, Pid, Frame, _RecvAtUs},
            #state{peer_pid = Pid} = S) ->
    {noreply, fold_frames(drain_frames(Pid, [Frame]), S)};

handle_info({macula_peering, disconnected, Pid, Reason},
            #state{peer_pid = Pid} = S) ->
    NewS = fail_all_pending({disconnected, Reason}, cancel_liveness(S)),
    %% Stop normally — the supervisor (or owning gen_server) decides
    %% whether to restart us.
    {stop, normal, NewS#state{peer_pid = undefined,
                              peer_node_id = undefined}};

%% Peer opened a dedicated stream toward us — a streaming RPC session
%% we didn't initiate. `macula_peering_conn' already handed off
%% controlling_process and enabled active mode; open this stream's
%% buffer and wait for its first frame (expected: STREAM_OPEN). See
%% PLAN_PER_STREAM_QUIC_ISOLATION.md.
handle_info({macula_peering, new_dedicated_stream, Pid, Stream},
            #state{peer_pid = Pid, stream_bufs = Bufs} = S) ->
    {noreply, S#state{stream_bufs = Bufs#{Stream => <<>>}}};
handle_info({macula_peering, new_dedicated_stream, _OtherPid, _Stream}, S) ->
    %% Stale notification from a link that is no longer `peer_pid'
    %% (respawned mid-flight) — nothing to attach it to.
    {noreply, S};

%% Bytes on one of our dedicated streams. Decode whatever complete
%% frames are available and dispatch each; the tail (a partial frame)
%% stays buffered for the next chunk, same as the shared-stream case
%% in `macula_peering_conn:connected/3'.
handle_info({quic, Bin, Stream, _Flags}, #state{stream_bufs = Bufs} = S)
        when is_binary(Bin), is_map_key(Stream, Bufs) ->
    Buf = maps:get(Stream, Bufs),
    {Frames, Tail} = macula_frame:parse_stream(<<Buf/binary, Bin/binary>>),
    NewS = lists:foldl(fun(F, Acc) -> dispatch_dedicated_frame(F, Stream, Acc) end,
                       S#state{stream_bufs = Bufs#{Stream => Tail}}, Frames),
    {noreply, NewS};

%% Bytes on one of our content-transfer streams (opened via
%% `open_content_stream/1', always by us — content is never
%% peer-initiated, unlike streaming RPC's inbound STREAM_OPEN case,
%% so there is no `new_dedicated_stream' seeding clause to match this
%% one).
handle_info({quic, Bin, Stream, _Flags},
            #state{content_stream_bufs = Bufs} = S)
        when is_binary(Bin), is_map_key(Stream, Bufs) ->
    Buf = maps:get(Stream, Bufs),
    {Frames, Tail} = macula_frame:parse_stream(<<Buf/binary, Bin/binary>>),
    NewS = lists:foldl(fun(F, Acc) -> dispatch_content_frame(F, Stream, Acc) end,
                       S#state{content_stream_bufs = Bufs#{Stream => Tail}},
                       Frames),
    {noreply, NewS};

handle_info({call_timeout, CallId}, #state{pending = P} = S) ->
    on_timeout(maps:take(CallId, P), S);

handle_info({content_call_timeout, Stream}, #state{content_pending = CP} = S) ->
    on_content_timeout(maps:take(Stream, CP), S);

handle_info(liveness_tick, S) ->
    {noreply, on_liveness_tick(S)};

%% Connect watchdog fired. If we are connected by now the timer was a
%% late straggler (we cancel on `connected', but async cancels can
%% race) — ignore it. Otherwise the handshake never completed: recycle.
handle_info(connect_watchdog, #state{peer_node_id = NodeId} = S)
        when NodeId =/= undefined ->
    {noreply, S#state{connect_watchdog = undefined}};
handle_info(connect_watchdog, #state{peer_pid = Pid, seed = Seed} = S) ->
    macula_diagnostics:event(<<"_macula.station_link.connect_watchdog">>, #{
        seed          => Seed,
        peer_pid      => Pid,
        timeout_ms    => connect_watchdog_ms(S)
    }),
    %% Best-effort kill of the wedged peering worker. If it is blocked
    %% in a non-yielding dirty NIF the kill is deferred until the NIF
    %% returns, but stopping here still frees the owner to respawn a
    %% fresh link (fresh dial) immediately — the bounded self-heal.
    kill_peer(Pid),
    {stop, normal, fail_all_pending(connect_timeout,
                                    cancel_liveness(
                                      S#state{connect_watchdog = undefined,
                                              peer_pid = undefined,
                                              peer_node_id = undefined}))};

handle_info({'EXIT', Pid, Reason}, #state{peer_pid = Pid} = S) ->
    NewS = fail_all_pending({peering_exit, Reason}, cancel_liveness(S)),
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

%% Drain consecutive frame messages from the peering process so they
%% process in one handle_info pass — fewer context switches, fewer
%% reduction-counter resets, better data-cache locality across the
%% verify/dispatch path. Frames remain in arrival order; we cap the
%% batch so a continuous burst can't park us indefinitely.
-define(MAX_FRAME_BATCH, 64).

drain_frames(Pid, Acc) ->
    drain_frames(Pid, Acc, ?MAX_FRAME_BATCH - 1).

drain_frames(_Pid, Acc, 0) ->
    lists:reverse(Acc);
drain_frames(Pid, Acc, N) ->
    receive
        {macula_peering, frame, Pid, F} ->
            drain_frames(Pid, [F | Acc], N - 1);
        {macula_peering, frame, Pid, F, _RecvAtUs} ->
            drain_frames(Pid, [F | Acc], N - 1)
    after 0 ->
        lists:reverse(Acc)
    end.

fold_frames(Frames, S) ->
    lists:foldl(fun on_frame/2, S, Frames).

terminate(_Reason, #state{peer_pid = Pid}) when is_pid(Pid) ->
    try macula_peering:close(Pid, client_stop) catch _:_ -> ok end,
    ok;
terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) -> {ok, S}.

%%====================================================================
%% Internals
%%====================================================================

%% Build, optionally publisher-sign, and send a PUBLISH frame stamped
%% with `Seq'. Shared by `publish/4' (per-link fallback seq) and
%% `publish/5' (pool-owned monotone seq).
%% Returns whatever the seam decided: `ok', or the structured reason the
%% frame was refused, which flows back through `summarize_publish/2' to
%% the caller of `macula_client:publish/5'.
publish_reply(ok, Seq, S) ->
    {reply, ok, S#state{publish_seq = Seq + 1}};
publish_reply({error, _} = Refused, _Seq, S) ->
    {reply, Refused, S}.

await_call_reply(ok, CallId, From, Tmo, Pending, S) ->
    TRef = erlang:send_after(Tmo, self(), {call_timeout, CallId}),
    {noreply, S#state{pending = Pending#{CallId => {From, TRef}}}};
await_call_reply({error, _} = Refused, _CallId, _From, _Tmo, _Pending, S) ->
    {reply, Refused, S}.

-spec send_publish_frame(<<_:256>>, binary(), term(), non_neg_integer(),
                         #state{}) -> ok | {error, term()}.
send_publish_frame(Realm, Topic, Payload, Seq,
                   #state{peer_pid = Pid, identity = Id}) ->
    Pub = macula_identity:public(Id),
    Frame0 = macula_frame:publish(#{
        topic           => Topic,
        realm           => Realm,
        publisher       => Pub,
        seq             => Seq,
        payload         => Payload,
        published_at_ms => erlang:system_time(millisecond)
    }),
    Frame = maybe_add_publisher_sig(Frame0, Id),
    macula_peering:send_frame(Pid, Frame).

%% Attach the publisher-end-to-end signature to an outbound PUBLISH.
%% Default flipped to `true' in 4.6.0 (was `false' since 4.4.0 when
%% the field was introduced). Flipping enables multi-hop pubsub: the
%% receiving station verifies against `publisher' via
%% `macula_frame:verify_publisher/1' so the frame stays valid across
%% any relay path, and the (publisher, seq) dedup cache on each
%% station kills loops. See `macula_station_event_dedup' in the
%% station repo for the dedup side. Wire-compat: the field has been
%% carried verbatim through relay hops since macula 4.4.0; stations
%% on >= 4.4.0 strip it from their canonical-signing bytes so adding
%% it does not break the per-hop relay signature. Operators can
%% override per-app via `application:set_env(macula,
%% pubsub_emit_publisher_sig, false)` if a regression surfaces.
maybe_add_publisher_sig(Frame, Identity) ->
    case application:get_env(macula, pubsub_emit_publisher_sig, true) of
        true  -> macula_frame:sign_publisher(Frame, Identity);
        _     -> Frame
    end.

after_connect_request({ok, Pid}, S) ->
    link(Pid),
    %% Arm the connect watchdog now: from here we are waiting for the
    %% peering worker's `connected' message. If it never arrives (dial
    %% NIF hangs, handshake stalls) the watchdog recycles the link.
    {noreply, arm_connect_watchdog(S#state{peer_pid = Pid})};
after_connect_request({error, Reason}, S) ->
    macula_diagnostics:event(<<"_macula.station_link.connect_failed">>, #{
        reason => Reason,
        seed   => S#state.seed
    }),
    erlang:send_after(S#state.connect_retry_backoff_ms, self(), attempt_connect),
    {noreply, S}.

%% RESULT
on_frame(#{frame_type := result, call_id := CallId, payload := Payload},
         #state{pending = P} = S) ->
    case maybe_clear_liveness(CallId, S) of
        {true, NewS}  -> NewS;
        {false, NewS} ->
            deliver_pending(maps:take(CallId, P), {ok, Payload}, NewS)
    end;
%% ERROR
on_frame(#{frame_type := error, call_id := CallId} = Frame,
         #state{pending = P} = S) ->
    Failure = call_failure(maps:get(code, Frame, 0),
                           maps:get(name, Frame, undefined),
                           maps:get(detail, Frame, undefined)),
    case maybe_clear_liveness(CallId, S) of
        {true, NewS}  -> NewS;
        {false, NewS} -> deliver_pending(maps:take(CallId, P), Failure, NewS)
    end;
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
%% STREAM_OPEN / STREAM_DATA / STREAM_END / STREAM_ERROR / STREAM_REPLY
%% no longer arrive here — every streaming session travels on its
%% own dedicated QUIC stream (see PLAN_PER_STREAM_QUIC_ISOLATION.md
%% and `dispatch_dedicated_frame/3'), not the shared control stream
%% `on_frame/2' decodes. A stream frame reaching this function is a
%% protocol violation and falls through to the catch-all below.
%%
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

%% -- Content-transfer dedicated streams (Phase 2) -------------------

open_content_stream_result({ok, Stream}, Bufs, S) ->
    {reply, {ok, Stream}, S#state{content_stream_bufs = Bufs#{Stream => <<>>}}};
open_content_stream_result({error, _} = E, _Bufs, S) ->
    {reply, E, S}.

send_on_content_stream(Stream, Frame, Id) ->
    try macula_peering:send_on_stream(Stream, Frame, Id)
    catch C:R -> {error, {C, R}}
    end.

await_content_call_reply(ok, Stream, From, Tmo, Pending, S) ->
    TRef = erlang:send_after(Tmo, self(), {content_call_timeout, Stream}),
    {noreply, S#state{content_pending = Pending#{Stream => {From, TRef}}}};
await_content_call_reply({error, _} = Refused, _Stream, _From, _Tmo, _Pending, S) ->
    {reply, Refused, S}.

on_content_timeout(error, S) ->
    {noreply, S};
on_content_timeout({{From, _OldTRef}, NewCP}, S) ->
    gen_server:reply(From, {error, timeout}),
    {noreply, S#state{content_pending = NewCP}}.

dispatch_content_frame(#{frame_type := result, payload := Payload}, Stream, S) ->
    deliver_content_reply(Stream, {ok, Payload}, S);
dispatch_content_frame(#{frame_type := error} = Frame, Stream, S) ->
    deliver_content_reply(Stream, call_failure(maps:get(code, Frame, 0),
                                               maps:get(name, Frame, undefined),
                                               maps:get(detail, Frame, undefined)),
                          S);
dispatch_content_frame(_Frame, _Stream, S) ->
    %% Anything else arriving on a content stream is a protocol
    %% violation — this side only ever sends CALL on one, so the only
    %% legitimate replies are RESULT/ERROR.
    S.

deliver_content_reply(Stream, Reply, #state{content_pending = CP} = S) ->
    reply_content_pending(maps:take(Stream, CP), Reply, S).

reply_content_pending(error, _Reply, S) ->
    %% No caller waiting (race with timeout, or a stray reply after
    %% `close_content_stream/2' already failed it).
    S;
reply_content_pending({{From, TRef}, NewCP}, Reply, S) ->
    _ = erlang:cancel_timer(TRef),
    gen_server:reply(From, Reply),
    S#state{content_pending = NewCP}.

close_content_stream_state(Stream, S) ->
    teardown_content_stream_state(Stream, {error, closed},
                                  fun macula_quic:close_stream/1, S).

abort_content_stream_state(Stream, Code, S) ->
    teardown_content_stream_state(Stream, {error, cancelled},
                                  fun(St) -> macula_quic:reset_stream(St, Code) end,
                                  S).

teardown_content_stream_state(Stream, LocalFailReason, CloseFun,
                              #state{content_pending = CP,
                                     content_stream_bufs = Bufs} = S) ->
    NewCP = fail_content_pending(maps:take(Stream, CP), CP, LocalFailReason),
    catch CloseFun(Stream),
    S#state{content_pending = NewCP,
            content_stream_bufs = maps:remove(Stream, Bufs)}.

fail_content_pending(error, CP, _Reason) ->
    CP;
fail_content_pending({{From, TRef}, NewCP}, _CP, Reason) ->
    _ = erlang:cancel_timer(TRef),
    gen_server:reply(From, Reason),
    NewCP.

fail_all_pending(Reason, #state{pending = P, subscriptions = Subs,
                                client_streams = CS,
                                server_streams = SS,
                                content_pending = ContentP} = S) ->
    maps:foreach(fun(_CallId, {From, TRef}) ->
        _ = erlang:cancel_timer(TRef),
        gen_server:reply(From, {error, Reason})
    end, P),
    maps:foreach(fun(_Stream, {From, TRef}) ->
        _ = erlang:cancel_timer(TRef),
        gen_server:reply(From, {error, Reason})
    end, ContentP),
    maps:foreach(fun(SubRef, {_Realm, _Topic, Subscriber, Mon}) ->
        erlang:demonitor(Mon, [flush]),
        Subscriber ! {macula_event_gone, SubRef, Reason}
    end, Subs),
    %% Abort every open stream with a `disconnected' STREAM_ERROR, and
    %% close its dedicated QUIC stream — the peering connection this
    %% stream belonged to is already gone or going, but the stream
    %% resource itself is independent and won't be reclaimed on its own.
    %% Caller waiters (recv / await_reply) unblock immediately;
    %% transient handler processes see the abort and exit.
    AbortFun = fun(_Sid, {Pid, Mon, Stream}) ->
        erlang:demonitor(Mon, [flush]),
        close_dedicated_stream(Stream),
        try
            macula_stream:abort(Pid, <<"disconnected">>,
                                 iolist_to_binary(io_lib:format("~p", [Reason])))
        catch _:_ -> ok end
    end,
    maps:foreach(AbortFun, CS),
    maps:foreach(AbortFun, SS),
    %% Content streams have no paired process to abort — just reclaim
    %% the QUIC resource, same as `close_content_stream_state/2' does
    %% on a normal close.
    maps:foreach(fun(Stream, _Buf) -> close_dedicated_stream(Stream) end,
                S#state.content_stream_bufs),
    S#state{pending = #{}, subscriptions = #{}, topic_index = #{},
            client_streams = #{}, server_streams = #{}, stream_bufs = #{},
            content_pending = #{}, content_stream_bufs = #{}}.

%%-------------------------------------------------------------------
%% Liveness probe — bounded zombie-connection detection
%%-------------------------------------------------------------------
%% On handshake-complete we arm a periodic tick. Each tick:
%%   1. If a prior probe is still outstanding (no reply received in the
%%      interval), increment misses. If misses >= MAX, close peer_pid
%%      via macula_peering — emits `disconnected', station_link stops,
%%      pool respawns.
%%   2. Otherwise (or after counting the miss), send a fresh probe
%%      (CALL with procedure `_macula.ping' on the DHT realm). The
%%      station has no such handler, so it replies with an `error'
%%      frame (`unknown_next_peer'). Either response shape clears the
%%      outstanding slot via `maybe_clear_liveness/2'.
%%   3. Re-arm the timer.
%% Connect watchdog helpers. Bounds the time from "peering worker
%% spawned" to "handshake complete". See the record field docs.
connect_watchdog_ms(#state{connect_watchdog_ms = Ms}) when is_integer(Ms) ->
    Ms;
connect_watchdog_ms(#state{connect_timeout_ms = Tmo}) ->
    Tmo + ?CONNECT_WATCHDOG_GRACE_MS.

arm_connect_watchdog(S) ->
    S1 = cancel_connect_watchdog(S),
    Ref = erlang:send_after(connect_watchdog_ms(S1), self(), connect_watchdog),
    S1#state{connect_watchdog = Ref}.

cancel_connect_watchdog(#state{connect_watchdog = undefined} = S) ->
    S;
cancel_connect_watchdog(#state{connect_watchdog = Ref} = S)
        when is_reference(Ref) ->
    _ = erlang:cancel_timer(Ref, [{async, true}, {info, false}]),
    S#state{connect_watchdog = undefined}.

kill_peer(Pid) when is_pid(Pid) ->
    _ = (try unlink(Pid) catch _:_ -> ok end),
    _ = (try exit(Pid, kill) catch _:_ -> ok end),
    ok;
kill_peer(_) ->
    ok.

arm_liveness(S) ->
    cancel_liveness_timer(S),
    Ref = erlang:send_after(S#state.liveness_interval_ms, self(), liveness_tick),
    S#state{liveness_timer = Ref}.

cancel_liveness(S) ->
    cancel_liveness_timer(S#state{liveness_outstanding = undefined,
                                  liveness_misses = 0}).

cancel_liveness_timer(#state{liveness_timer = undefined} = S) ->
    S;
cancel_liveness_timer(#state{liveness_timer = Ref} = S) when is_reference(Ref) ->
    _ = erlang:cancel_timer(Ref, [{async, true}, {info, false}]),
    S#state{liveness_timer = undefined}.

%% Called on every inbound RESULT / ERROR. If the call_id matches the
%% outstanding liveness probe, reset miss counter; tells the caller
%% (on_frame) not to deliver to user-pending logic.
maybe_clear_liveness(CallId, #state{liveness_outstanding = CallId} = S)
        when CallId =/= undefined ->
    {true, S#state{liveness_outstanding = undefined,
                   liveness_misses = 0}};
maybe_clear_liveness(_CallId, S) ->
    {false, S}.

on_liveness_tick(#state{peer_pid = undefined} = S) ->
    %% Not connected — don't probe, don't re-arm.
    cancel_liveness(S);
on_liveness_tick(#state{peer_node_id = undefined} = S) ->
    %% Mid-handshake — defer probing until `connected' message
    %% re-arms us.
    arm_liveness(S);
on_liveness_tick(S0) ->
    S1 = on_outstanding_check(S0#state.liveness_outstanding, S0),
    case is_pid(S1#state.peer_pid) of
        true  -> arm_liveness(send_probe(S1));
        false -> S1
    end.

on_outstanding_check(undefined, S) ->
    %% No prior probe pending; nothing to count.
    S;
on_outstanding_check(_CallId, S) ->
    %% Prior probe never got a reply within the tick interval.
    Misses = S#state.liveness_misses + 1,
    case Misses >= S#state.liveness_max_misses of
        true  -> trigger_zombie_close(S#state{liveness_misses = Misses});
        false -> S#state{liveness_misses = Misses,
                         liveness_outstanding = undefined}
    end.

trigger_zombie_close(#state{peer_pid = Pid} = S) when is_pid(Pid) ->
    macula_diagnostics:event(<<"_macula.station_link.liveness_lost">>, #{
        seed   => S#state.seed,
        misses => S#state.liveness_misses
    }),
    try macula_peering:close(Pid, app_liveness_lost) catch _:_ -> ok end,
    S#state{liveness_outstanding = undefined};
trigger_zombie_close(S) ->
    S.

send_probe(#state{peer_pid = Pid, identity = Id} = S) when is_pid(Pid) ->
    CallId = crypto:strong_rand_bytes(16),
    Caller = macula_identity:public(Id),
    DeadlineMs = erlang:system_time(millisecond) + S#state.liveness_interval_ms,
    Frame = macula_frame:call(#{
        call_id     => CallId,
        procedure   => ?LIVENESS_PROCEDURE,
        realm       => ?DHT_REALM,
        payload     => #{},
        deadline_ms => DeadlineMs,
        caller      => Caller
    }),
    Signed = macula_frame:sign(Frame, Id),
    try macula_peering:send_frame(Pid, Signed) catch _:_ -> ok end,
    S#state{liveness_outstanding = CallId};
send_probe(S) ->
    S.

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
    try macula_peering:send_frame(Pid, Frame) catch _:_ -> ok end,
    ok.

%% Send a SUBSCRIBE frame for `(Realm, Topic)' iff peering is connected.
maybe_send_subscribe(_Realm, _Topic, #state{peer_pid = undefined}) ->
    ok;
maybe_send_subscribe(Realm, Topic, #state{peer_pid = Pid, identity = Id}) ->
    SubKey = macula_identity:public(Id),
    Frame  = macula_frame:subscribe(#{topic      => Topic,
                                      realm      => Realm,
                                      subscriber => SubKey}),
    try macula_peering:send_frame(Pid, Frame) catch _:_ -> ok end,
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
    try macula_peering:send_frame(Pid, Frame) catch _:_ -> ok end,
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
    try macula_peering:send_frame(Pid, Frame) catch _:_ -> ok end,
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
                      payload := Payload} = Frame,
                    #state{procedures = Procs, policies = Pols, identity = Id,
                           peer_pid = Pid}) when is_pid(Pid) ->
    SelfPub = macula_identity:public(Id),
    %% Gate first (Slice 7b): an `open' procedure serves any identified
    %% caller; a gated one requires a valid `ucan_token', else refuse
    %% with BOLT#4 `unauthorized' instead of invoking the handler.
    Reply   = authorized_reply(authorize({Realm, Proc}, Frame, Pols),
                               maps:find({Realm, Proc}, Procs),
                               CallId, Payload, SelfPub),
    sent_or_faulted(macula_peering:send_frame(Pid, Reply),
                    Pid, CallId, SelfPub);
handle_inbound_call(_Frame, _State) ->
    ok.

authorized_reply(ok, Found, CallId, Payload, SelfPub) ->
    build_inbound_call_reply(Found, CallId, Payload, SelfPub);
authorized_reply(unauthorized, _Found, CallId, _Payload, SelfPub) ->
    macula_frame:call_error(#{call_id     => CallId,
                              code        => macula_bolt4:code(unauthorized),
                              reported_by => SelfPub}).

authorize(Key, Frame, Pols) ->
    authorize_policy(maps:get(Key, Pols, open), Frame).

authorize_policy(open, _Frame) ->
    ok;
authorize_policy({ucan_required, Issuer}, Frame) ->
    check_ucan(maps:get(ucan_token, Frame, <<>>), Issuer).

check_ucan(<<>>, _Issuer) ->
    unauthorized;
check_ucan(Token, Issuer) when is_binary(Token) ->
    ucan_verdict(macula_ucan_nif:verify(Token, Issuer));
check_ucan(_Other, _Issuer) ->
    unauthorized.

ucan_verdict({ok, _Payload}) -> ok;
ucan_verdict(_Error)         -> unauthorized.

%% `open' is the default, so store it as absence to keep the map small.
set_policy(Key, open, Pols)   -> maps:remove(Key, Pols);
set_policy(Key, Policy, Pols) -> Pols#{Key => Policy}.

%% A RESULT the wire refuses must not simply vanish. Dropping it leaves
%% the remote caller burning its entire deadline waiting for a frame
%% that died here, which is a timeout where a taxonomy was available:
%% the handler's return value was the problem and BOLT#4 can say so.
%% A `call_error' frame is all binaries and small integers, so it is
%% sendable by construction and cannot recurse into this path.
sent_or_faulted(ok, _Pid, _CallId, _SelfPub) ->
    ok;
sent_or_faulted({error, Reason}, Pid, CallId, SelfPub) ->
    logger:error("[macula_station_link] handler result unsendable, "
                 "faulting the call: ~ts", [macula_frame:explain(Reason)]),
    _ = macula_peering:send_frame(
          Pid, macula_frame:call_error(#{call_id     => CallId,
                                         code        => refusal_code(Reason),
                                         reported_by => SelfPub})),
    ok.

refusal_code({unsupported_payload_type, payload_too_large, _Path}) -> 16#0D;
refusal_code(_Other)                                               -> 16#0F.

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

%% BOLT#4 error frames carry an optional `detail' binary, and it is the
%% only way a handler's refusal reaches the caller who provoked it.
%%
%% A reason that is ALREADY a binary crosses verbatim, so a handler
%% answering `{error, <<"hold_full">>}' gives its caller
%% `{error, <<"hold_full">>}' and the caller can match on it. Before
%% 8.0.0 every reason went through `~0p' and that same handler produced
%% `<<"<<\"hold_full\">>">>', a rendering of a binary rather than the
%% binary, which no caller could sensibly compare against.
%%
%% Anything that is not a binary is still rendered with `~0p'. It is
%% faithful but it is a printed form and not the term: a reason that
%% crosses a wire crosses it as bytes, and a handler that wants its
%% caller to match on the reason should say it in a binary.
%%
%% Capped at 256 bytes to keep CALL_ERROR frames bounded. A reason long
%% enough to be truncated is a reason nobody can match on, which is one
%% more argument for short ones.
format_error_detail(Reason) when is_binary(Reason) ->
    capped(Reason);
format_error_detail(Reason) ->
    capped(iolist_to_binary(io_lib:format("~0p", [Reason]))).

capped(Bin) when byte_size(Bin) =< 256 -> Bin;
capped(Bin) -> <<(binary:part(Bin, 0, 253))/binary, "...">>.

%% What an inbound ERROR frame means to the caller who is waiting.
%%
%% `0x0F' is the code THIS SDK puts on the wire when a handler answered
%% `{error, Reason}' (see safe_invoke_handler/4). So it is not an unknown
%% error at all: it is the handler refusing, and what the caller wants is
%% the refusal, not a constant that means "something went wrong".
%%
%% Every other code is the transport failing rather than a handler
%% speaking, so it keeps the `{call_error, Code, Name}' shape.
%%
%% This also settles the retry question where it is asked. BOLT#4 rates
%% `0x0F' `log_and_caution', so `macula_bolt4:is_retryable/1' answers
%% `true' for it, which is right for a genuinely unknown error and wrong
%% for a handler that has just said no. The spec table is the spec's and
%% is left alone; a caller who gets the reason back does not need to ask.
call_failure(16#0F, _Name, Detail) when is_binary(Detail) ->
    {error, Detail};
call_failure(Code, Name, _Detail) ->
    {error, {call_error, Code, Name}}.

%%-------------------------------------------------------------------
%% Helpers
%%-------------------------------------------------------------------

%% Fold TLS-policy opts (`verify' / `expected_node_id' / `pin_tls_cert')
%% from the link opts into the seed map, so they reach the peering
%% target at connect.
add_tls_opts(Seed, Opts) ->
    lists:foldl(fun(K, Acc) -> copy_opt(K, Opts, Acc) end,
                Seed, [verify, expected_node_id, pin_tls_cert]).

copy_opt(K, Opts, Seed) ->
    case maps:find(K, Opts) of
        {ok, V} -> Seed#{K => V};
        error   -> Seed
    end.

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
    NewS = open_client_stream_dedicated(Pid, Frame, Sid, StreamPid, Mon, Id, S),
    {reply_value, {ok, StreamPid}, NewS}.

%% Open this session's dedicated QUIC stream and write STREAM_OPEN as
%% the first bytes on it — not the shared control stream. If the
%% dedicated stream can't be opened (connection gone, flow-control
%% credit exhausted), the `macula_stream' already spawned above gets
%% a clean error the same way an unknown procedure does on the
%% inbound side, instead of hanging until its deadline.
open_client_stream_dedicated(Pid, Frame, Sid, StreamPid, Mon, Id, S) ->
    dedicated_open_result(macula_peering:open_dedicated_stream(Pid),
                          Frame, Sid, StreamPid, Mon, Id, S).

dedicated_open_result({ok, Stream}, Frame, Sid, StreamPid, Mon, Id,
                      #state{stream_bufs = Bufs} = S) ->
    try macula_peering:send_on_stream(Stream, Frame, Id) catch _:_ -> ok end,
    CS = S#state.client_streams,
    S#state{client_streams = CS#{Sid => {StreamPid, Mon, Stream}},
            %% This stream is bidirectional (`open_bi/1`) — the
            %% provider's STREAM_DATA/END/ERROR/REPLY arrives back on
            %% this same stream, so its inbound buffer needs to exist
            %% now, not just for peer-initiated streams (see the
            %% `new_dedicated_stream' handler).
            stream_bufs = Bufs#{Stream => <<>>}};
dedicated_open_result({error, _Reason}, _Frame, _Sid, StreamPid, Mon, _Id, S) ->
    erlang:demonitor(Mon, [flush]),
    catch macula_stream:deliver_error(StreamPid, <<"unavailable">>,
                                      <<"failed to open dedicated stream">>),
    S.

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

%% Every open stream session has its own dedicated QUIC stream by
%% the time anything is outbound on it — `find_stream/2' returning
%% `error' here means the session already tore down (peer closed,
%% monitor DOWN raced this cast); nothing to send to.
send_on_dedicated_stream(error, _Frame, _Id) ->
    ok;
send_on_dedicated_stream({ok, {_Pid, _Mon, Stream}}, Frame, Id) ->
    try macula_peering:send_on_stream(Stream, Frame, Id) catch _:_ -> ok end.

%% `stream_reply' carries `responded_by' (the link's own pubkey) which
%% the v1 stream gen_server has no way to know. `stream_data',
%% `stream_end' and `stream_error' carry `signer' (the emitter's
%% pubkey) so the station-side verify path can authenticate non-OPEN
%% stream frames end-to-end across multi-hop relays — same pattern as
%% CALL's `caller'. Without it, station_B receiving a chunk forwarded
%% by station_A would verify the signature against station_A's NodeId,
%% but the frame was signed by the originating daemon, and verify
%% would fail silently — every cross-station stream chunk dropped.
finalise_stream_spec(stream_reply, Spec, Id) ->
    Spec#{responded_by => macula_identity:public(Id)};
finalise_stream_spec(Type, Spec, Id) when Type =:= stream_data;
                                          Type =:= stream_end;
                                          Type =:= stream_error ->
    Spec#{signer => macula_identity:public(Id)};
finalise_stream_spec(_Type, Spec, _Id) ->
    Spec.

%% After sending an outbound terminal frame, drop the local routing
%% entry — but ONLY when this link owns just one side of the stream.
%% Same-pool streaming RPC keeps the same Sid in BOTH client_streams
%% and server_streams (one link is both caller and advertiser, the
%% relay bounces the frames back); the handler emits STREAM_END
%% outbound on the server side, and the station then bounces back
%% server-emitted STREAM_DATA chunks plus the STREAM_END itself.
%% Dropping on the outbound here would clear the client_streams
%% entry before any of those bounced inbound frames arrive, and the
%% caller's recv waiter would silently miss every chunk. Defer to
%% the inbound terminal handler (`deliver_stream_end' /
%% `deliver_stream_error' / `deliver_stream_reply') which fires
%% after the bounce and tears down both entries via `drop_stream'.
on_outbound_stream_frame(stream_end, #{role := both, stream_id := Sid}, S) ->
    maybe_drop_outbound(Sid, S);
on_outbound_stream_frame(stream_error, #{stream_id := Sid}, S) ->
    maybe_drop_outbound(Sid, S);
on_outbound_stream_frame(stream_reply, #{stream_id := Sid}, S) ->
    maybe_drop_outbound(Sid, S);
on_outbound_stream_frame(_Type, _Spec, S) ->
    S.

maybe_drop_outbound(Sid, #state{client_streams = CS,
                                server_streams = SS} = S) ->
    case {maps:is_key(Sid, CS), maps:is_key(Sid, SS)} of
        {true, true}  -> S;
        _             -> drop_stream(Sid, S)
    end.

%% Terminal frames (stream_end role=both, stream_error, stream_reply)
%% close the stream from both ends. Drop the Sid from whichever map
%% holds it, and the dedicated QUIC stream's inbound buffer along
%% with it — otherwise `stream_bufs' leaks one entry per finished
%% session. Same-pool case has the same Sid in BOTH maps (and, in
%% principle, the same dedicated stream); drop both so the link
%% doesn't leak entries.
drop_stream(Sid, #state{client_streams = CS, server_streams = SS,
                        stream_bufs = Bufs} = S) ->
    {CS2, ClientMon, ClientStream} = drop_one(Sid, CS),
    {SS2, ServerMon, ServerStream} = drop_one(Sid, SS),
    _ = [erlang:demonitor(M, [flush])
         || M <- [ClientMon, ServerMon], M =/= undefined],
    %% Same-pool sessions share one dedicated stream across both
    %% maps; closing it twice is harmless (`nif_close_stream` is
    %% idempotent against an already-finished send half).
    _ = [close_dedicated_stream(Stream)
         || Stream <- lists:usort([ClientStream, ServerStream]),
            Stream =/= undefined],
    Bufs2 = drop_bufs([ClientStream, ServerStream], Bufs),
    S#state{client_streams = CS2, server_streams = SS2, stream_bufs = Bufs2}.

drop_one(Sid, Map) ->
    case maps:take(Sid, Map) of
        error -> {Map, undefined, undefined};
        {{_Pid, Mon, Stream}, NewMap} -> {NewMap, Mon, Stream}
    end.

drop_bufs(Streams, Bufs) ->
    lists:foldl(fun(undefined, Acc) -> Acc;
                   (Stream, Acc) -> maps:remove(Stream, Acc)
                end, Bufs, Streams).

%%-------------------------------------------------------------------
%% Streaming RPC — dispatch for frames decoded off a dedicated stream
%%-------------------------------------------------------------------

%% Every stream-related frame type this link ever needs to act on,
%% now sourced from a session's own dedicated QUIC stream instead of
%% the shared control stream's `on_frame/2'. STREAM_OPEN is the only
%% one that can legitimately be the *first* frame on a freshly
%% handed-off inbound stream; the rest belong to a session already
%% tracked in `client_streams' / `server_streams'.
dispatch_dedicated_frame(#{frame_type := stream_open} = Frame, Stream, S) ->
    handle_inbound_stream_open(Frame, Stream, S);
dispatch_dedicated_frame(#{frame_type := stream_data} = Frame, _Stream, S) ->
    deliver_stream_data(Frame, S);
dispatch_dedicated_frame(#{frame_type := stream_end} = Frame, _Stream, S) ->
    deliver_stream_end(Frame, S);
dispatch_dedicated_frame(#{frame_type := stream_error} = Frame, _Stream, S) ->
    deliver_stream_error(Frame, S);
dispatch_dedicated_frame(#{frame_type := stream_reply} = Frame, _Stream, S) ->
    deliver_stream_reply(Frame, S);
dispatch_dedicated_frame(_Frame, _Stream, S) ->
    %% Anything else arriving first on a dedicated stream is a
    %% protocol violation — nothing but our own peer code opens one
    %% of these, and only for a stream session.
    S.

%%-------------------------------------------------------------------
%% Streaming RPC — inbound STREAM_OPEN (server-side dispatch)
%%-------------------------------------------------------------------

%% STREAM_OPEN arrives as the first frame decoded off a freshly
%% handed-off dedicated QUIC stream (see `dispatch_dedicated_frame/3'
%% below) — `Stream' is that stream's reference, and every frame
%% this session sends or receives from here on travels on it.
%% Look up `(Realm, Procedure)' this link advertised, spawn a
%% server-side stream_v1 paired to this link, then dispatch the
%% registered handler in a transient process so a slow / crashing
%% handler can't block the link's gen_server.
handle_inbound_stream_open(#{stream_id := Sid, procedure := Proc,
                              realm := Realm, args := Args} = Frame,
                           Stream, S) ->
    DeclaredMode = maps:get(mode, Frame, server_stream),
    dispatch_stream_open(maps:find({Realm, Proc}, S#state.stream_procedures),
                         Sid, Proc, DeclaredMode, Args, Stream, S).

%% Unknown (Realm, Procedure) → ship a STREAM_ERROR back on the
%% caller's own dedicated stream so it unblocks immediately rather
%% than waiting for its deadline. The shared control stream is not
%% in this session's path at all, so the reply has to go here.
dispatch_stream_open(error, Sid, _Proc, _Declared, _Args, Stream,
                     #state{identity = Id} = S) ->
    Frame = macula_frame:stream_error(#{
        stream_id => Sid,
        code      => <<"not_found">>,
        message   => <<"procedure not advertised">>
    }),
    try macula_peering:send_on_stream(Stream, Frame, Id) catch _:_ -> ok end,
    S;
dispatch_stream_open({ok, {AdvMode, Handler}}, Sid, Proc, _Declared, Args,
                     Stream, S) ->
    %% Advertised mode wins — the server declared the shape.
    spawn_inbound_stream(Sid, Proc, AdvMode, Handler, Args, Stream, S).

spawn_inbound_stream(Sid, Proc, Mode, Handler, Args, Stream,
                     #state{server_streams = SS} = S) ->
    Host = spawn(fun stream_host_loop/0),
    {ok, StreamPid} = macula_stream:start_link(#{
        id    => Sid,
        role  => server,
        mode  => Mode,
        owner => Host
    }),
    ok = macula_stream:attach_to_link(StreamPid, self(), Sid),
    Mon = erlang:monitor(process, StreamPid),
    _Worker = spawn_stream_handler(Handler, StreamPid, Args, Proc),
    S#state{server_streams = SS#{Sid => {StreamPid, Mon, Stream}}}.

stream_host_loop() ->
    receive stop -> ok end.

%% Handler runs in a transient process. A handler crash maps to a
%% STREAM_ERROR abort with the crash class as the code so callers
%% see a stable error taxonomy. The try/catch is justified (mirrors
%% `safe_invoke_handler/4' for unary CALLs): without it a crash
%% would silently leave the caller waiting on its deadline.
spawn_stream_handler(Handler, Stream, Args, Proc) ->
    spawn(fun() -> run_stream_handler(Handler, Stream, Args, Proc) end).

run_stream_handler(Handler, Stream, Args, Proc) ->
    try Handler(Stream, Args)
    catch
        Class:Reason:Stack ->
            Code = atom_to_binary(Class, utf8),
            Msg = iolist_to_binary(io_lib:format(
                "stream handler ~s crashed: ~p:~p~n~p",
                [Proc, Class, Reason, Stack])),
            _ = macula_stream:abort(Stream, Code, Msg)
    end.

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
                      fun({Pid, _Mon, _Stream}) ->
                          macula_stream:deliver_chunk(
                            Pid,
                            maps:get(encoding, Frame, raw),
                            maps:get(body, Frame, <<>>))
                      end),
    S.

deliver_stream_end(#{stream_id := Sid} = Frame, S) ->
    Role = maps:get(role, Frame, both),
    deliver_to_stream(find_stream(Sid, S),
                      fun({Pid, _Mon, _Stream}) ->
                          macula_stream:deliver_end(Pid, Role)
                      end),
    %% Full close drops the routing entry; half close keeps it open
    %% for outbound chunks back to the peer.
    forget_on_full_close(Role, Sid, S).

deliver_stream_error(#{stream_id := Sid} = Frame, S) ->
    Code = maps:get(code, Frame, <<"error">>),
    Message = maps:get(message, Frame, <<>>),
    deliver_to_stream(find_stream(Sid, S),
                      fun({Pid, _Mon, _Stream}) ->
                          macula_stream:deliver_error(Pid, Code, Message)
                      end),
    drop_stream(Sid, S).

deliver_stream_reply(#{stream_id := Sid, payload := Payload}, S) ->
    deliver_to_stream(find_stream(Sid, S),
                      fun({Pid, _Mon, _Stream}) ->
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
on_monitor_down(Pid, Mon, #state{client_streams = CS} = S) ->
    on_client_stream_down(find_stream_by_pid(Pid, CS), Pid, Mon, S).

on_client_stream_down({ok, Sid}, _Pid, Mon, #state{client_streams = CS,
                                                   stream_bufs = Bufs} = S) ->
    erlang:demonitor(Mon, [flush]),
    {CS2, Stream} = take_dedicated_stream(Sid, CS),
    close_dedicated_stream(Stream),
    S#state{client_streams = CS2, stream_bufs = drop_bufs([Stream], Bufs)};
on_client_stream_down(error, Pid, Mon, #state{server_streams = SS} = S) ->
    on_server_stream_down(find_stream_by_pid(Pid, SS), Mon, S).

on_server_stream_down({ok, Sid}, Mon, #state{server_streams = SS,
                                             stream_bufs = Bufs} = S) ->
    erlang:demonitor(Mon, [flush]),
    {SS2, Stream} = take_dedicated_stream(Sid, SS),
    close_dedicated_stream(Stream),
    S#state{server_streams = SS2, stream_bufs = drop_bufs([Stream], Bufs)};
on_server_stream_down(error, Mon, S) ->
    on_subscriber_down(Mon, S).

take_dedicated_stream(Sid, Map) ->
    case maps:take(Sid, Map) of
        error -> {Map, undefined};
        {{_Pid, _Mon, Stream}, NewMap} -> {NewMap, Stream}
    end.

%% The owning `macula_stream' died — nothing is driving this
%% dedicated QUIC stream anymore. Close it rather than leaking a live
%% stream resource for a session that will never resume.
close_dedicated_stream(undefined) -> ok;
close_dedicated_stream(Stream) -> catch macula_quic:close_stream(Stream), ok.

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
    Found = [Sid || {Sid, {P, _, _}} <- maps:to_list(Streams), P =:= Pid],
    first_or_error(Found).

first_or_error([H | _]) -> {ok, H};
first_or_error([])      -> error.
