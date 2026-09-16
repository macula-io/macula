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
%%   <li><strong>Request/response</strong> — `call/6' sends a signed
%%       CALL to a target, the connected station or a provider's
%%       node_id, and completes the pending caller when a reply
%%       verifies against that request. Convenience wrappers call the
%%       station for `_dht.put_record', `_dht.find_record', and
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
%%   <li>`call/6' from caller → sign the CALL with the link's key, keep
%%       the caller, its deadline timer and the request under the
%%       request_id, send the frame via `macula_peering:send_frame/2'.</li>
%%   <li>A reply arrives as `{macula_peering, frame, Pid, Frame}' → look
%%       up its request_id, verify it against the held request, cancel
%%       the timer, reply to the caller. A reply that does not verify is
%%       counted and leaves the call pending.</li>
%%   <li>`{macula_peering, disconnected, Pid, Reason}' → fail all
%%       pending calls with `{error, {disconnected, Name}}', `Name'
%%       being the reason's name from `macula_reason_name:text/1', notify
%%       all subscribers via `macula_event_gone', stop the client
%%       (caller is responsible for restart / reconnect).</li>
%% </ol>
%%
%% == Call reply taxonomy ==
%%
%% <table>
%%   <tr><th>Verified reply</th><th>`call/6' returns</th></tr>
%%   <tr><td>RESULT(payload=Value)</td><td>`{ok, Value}', its text as `{text, Bin}'</td></tr>
%%   <tr><td>provider ERROR(code=`handler_error', detail=D)</td><td>`{error, D}' — the handler's own reason</td></tr>
%%   <tr><td>provider ERROR(code=C, detail=D)</td><td>`{error, {call_error, C, D}}', C and D binaries, D `undefined' when absent</td></tr>
%%   <tr><td>station ERROR, no such next peer</td><td>`{error, {call_error, unknown_next_peer, undefined}}'</td></tr>
%%   <tr><td>(deadline elapses)</td><td>`{error, timeout}'</td></tr>
%%   <tr><td>(connection drops)</td><td>`{error, {disconnected, Name}}' or `{error, {peering_exit, Name}}'</td></tr>
%%   <tr><td>(link stops for any other reason, with the call pending or still waiting to reach it)</td><td>`{error, {link_stopped, Name}}'</td></tr>
%%   <tr><td>(not connected yet)</td><td>`{error, not_connected}', not sent</td></tr>
%%   <tr><td>(payload, or procedure text over 512 bytes or not UTF-8, refused before sending)</td><td>`{error, {refused, Reason}}', not sent</td></tr>
%% </table>
%%
%% `not_sent/1' says whether an error means the CALL never went out.
-module(macula_station_link).
-behaviour(gen_server).

-export([
    start_link/1,
    stop/1,
    call/6,
    call/7,
    publish/4,
    publish/5,
    put_record/2, put_record/3,
    find_record/2, find_record/3,
    find_records_by_type/2, find_records_by_type/3,
    subscribe/4,
    unsubscribe/2,
    unsubscribe_async/2,
    advertise/4,
    advertise/5,
    unadvertise/3,
    %% Overlay-protocol frame transport (HyParView, Plumtree, and any
    %% future frame type the built-in call/event handling doesn't
    %% recognise) — not for general RPC/pubsub use, see the moduledoc
    %% on `overlay_subscribe/3'.
    overlay_subscribe/3,
    overlay_unsubscribe/2,
    send_overlay_frame/2,
    send_overlay_frame/3,
    overlay_frame_refused/3,
    %% Streaming RPC (SDK 3.17+, Part 6 §5.6)
    call_stream/6,
    advertise_stream/5,
    advertise_stream/6,
    unadvertise_stream/3,
    send_stream_bytes/4,
    is_connected/1,
    not_sent/1,
    peer_node_id/1,
    %% Dedicated-stream content transfer (PLAN_PER_STREAM_QUIC_ISOLATION.md
    %% Phase 2). Not for general RPC use — see the moduledoc on
    %% `open_content_stream/1'.
    open_content_stream/1,
    call_on_stream/6,
    close_content_stream/2,
    abort_content_stream/4,
    %% Exported for `macula_client''s discovery-seed dedup, which needs
    %% to know whether two differently-spelled seeds (e.g. `https://'
    %% bootstrap vs. `quic://' discovery-generated) name the same
    %% `{host, port}' — the exact normalization this link already does
    %% at connect time. See `macula_client:normalize_seed/1'.
    parse_seed/1
]).

-export_type([handler/0, stream_handler/0, overlay_subscription/0]).

-ifdef(TEST).
-export([with_client_stream/3]).
-endif.

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/1]).

-export_type([opts/0]).

-ifdef(TEST).
%% The process a served stream's handler runs in, spawned before the stream
%% exists: exported for macula_stream_tests.erl.
-export([spawn_stream_handler/3]).
%% A state field's position in the state tuple, by name, for tests that read
%% or set the state.
-export([state_field_index/1]).
-endif.

-type url() :: binary() | string().

-type opts() :: #{
    %% Endpoint to dial. Either a URL (https://host:port) or a
    %% pre-parsed #{host, port} map. The map form may carry the
    %% `macula_peering_conn:connect_opts()' trust keys, forwarded
    %% verbatim into the dial target:
    %%   `expected_node_id' — the station's node_id, which the
    %%       handshake checks (D16); required, and a link without one
    %%       refuses to start;
    %%   `verify' — `webpki' (default) or `none' (dev/self-signed
    %%       only; logs a warning per dial).
    seed     := url() | #{host := binary() | string(),
                          port := inet:port_number(),
                          _    => _},
    %% A function that returns the node identity key, so no start
    %% argument holds the key. Required: a link makes no key of its own.
    node_identity := fun(() -> macula_node_keys:node_key()),
    %% The pool's statement issuer, which every connection draws its
    %% CONNECT material and status statements from. Required, and the
    %% link ends when the issuer does.
    issuer := pid(),
    %% The pool's request admission, where every verified request the link
    %% receives is judged, and this link's share in it: the normalized seed
    %% the pool counts the peer by. Both required.
    admission := pid(),
    share := term(),
    %% The function that opens the peering connection, by default
    %% `macula_peering:connect/1'. An option, so a test replaces no
    %% shared module.
    connect => fun((map()) -> {ok, pid()} | {error, term()}),
    %% The functions that open a dedicated stream on the peering
    %% connection, write the bytes of one frame on one, and close one, by
    %% default `macula_peering:open_dedicated_stream/1', `send_on_stream/2'
    %% and `close_dedicated_stream/1'. Options, so a test replaces no
    %% shared module. A link given one of another shape refuses to start.
    open_stream => fun((pid()) -> {ok, reference()} | {error, term()}),
    send_on_stream => fun((reference(), binary()) -> ok | {error, term()}),
    close_stream => fun((reference()) -> ok),
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
%% The longest a call waits: the deadline window a provider accepts, ten
%% minutes past its clock.
-define(MAX_CALL_TIMEOUT_MS, 600_000).
%% The code a provider's ERROR carries for a handler that refused, with the
%% handler's text as its detail.
-define(HANDLER_ERROR_CODE, <<"handler_error">>).
-define(CONNECT_RETRY_BACKOFF_MS, 1_000).

%% App-level liveness probe. Sends a tiny CALL (`_macula.ping' on the
%% DHT realm, no handler expected — station replies with
%% `unknown_next_peer') every `?LIVENESS_INTERVAL_MS' and keeps the
%% outstanding probe's request, which only a verified reply from the
%% connected station clears. On `?LIVENESS_MAX_MISSES' consecutive
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
%% Replies the link refuses are counted by reason, and logged at most once a
%% window with the count since the last line.
-define(REFUSED_REPLIES_WINDOW_MS, 60_000).
%% A relayed overlay frame the link drops is logged at most once a minute per kind.
-define(REFUSED_RELAYS_WINDOW_MS, 60_000).

%% Grace added on top of `connect_timeout_ms' before the connect
%% watchdog fires. The dial NIF is meant to bound itself at
%% `connect_timeout_ms'; the grace covers CONNECT/HELLO frame exchange
%% after the QUIC layer is up. If the whole thing hasn't produced a
%% `connected' message by `connect_timeout_ms + grace', the worker is
%% wedged and we recycle the link.
-define(CONNECT_WATCHDOG_GRACE_MS, 10_000).

%% How long a dedicated stream the peer opened may take to bring its first
%% whole frame before it closes, unless `dedicated_stream_open_timeout_ms'
%% in the macula application env says otherwise.
-define(DEDICATED_STREAM_OPEN_TIMEOUT_MS, 10_000).

%% The longest STREAM_OPEN a dedicated stream may start with, 1 MiB, unless
%% `max_stream_open_bytes' in the macula application env says otherwise.
%% `call_stream/6' refuses a longer open by the same limit, so both sides on
%% one node agree.
-define(MAX_STREAM_OPEN_BYTES, 16#100000).

%% How long a received request waits on the pool's admission for its verdict
%% before it is refused, so a stalled admission never holds up the link.
-define(ADMIT_TIMEOUT_MS, 1_000).

-record(state, {
    seed             :: #{host := binary() | string(),
                          port := inet:port_number(),
                          _    => _},
    node_identity    :: macula_node_keys:node_key(),
    profile          :: macula_crypto_profile:profile(),
    issuer           :: pid(),
    %% The pool's request admission, which judges every verified request
    %% this link receives, and this link's share in it.
    admission        :: pid(),
    share            :: term(),
    connect          :: fun((map()) -> {ok, pid()} | {error, term()}),
    %% The functions every dedicated stream is opened, written and closed
    %% through (start opts `open_stream', `send_on_stream', `close_stream').
    open_stream      :: fun((pid()) -> {ok, reference()} | {error, term()}),
    send_on_stream   :: fun((reference(), binary()) -> ok | {error, term()}),
    close_stream     :: fun((reference()) -> ok),
    capabilities     :: non_neg_integer(),
    alpn             :: [binary()],
    connect_timeout_ms :: non_neg_integer(),
    %% peering worker pid (`macula_peering_conn`). undefined while
    %% disconnected.
    peer_pid         :: pid() | undefined,
    %% peer's node id, set on `connected'.
    peer_node_id     :: <<_:256>> | undefined,
    %% map of CALL id (16 bytes) -> {From, TimerRef}.
    pending = #{}    :: #{<<_:128>> => {gen_server:from(), reference(), macula_frame:verified_request()}},
    %% Active topic subscriptions keyed by SubRef returned to the
    %% subscriber. The reverse `topic_index' lets inbound EVENT
    %% frames fan out to all SubRefs subscribed to a given
    %% (realm, topic) without scanning the whole subscriptions map.
    subscriptions = #{} :: #{reference() => subscription()},
    topic_index   = #{} :: #{{<<_:256>>, binary()} => sets:set(reference())},
    %% Overlay-frame subscriptions (HyParView, Plumtree, and future
    %% overlay-protocol frame types the built-in call/event/result/
    %% error handling doesn't recognise) — see `overlay_subscribe/3'.
    %% Simpler than the topic case: no wire-level SUBSCRIBE/UNSUBSCRIBE
    %% round trip, since these frames are already addressed at this
    %% specific connection by the station, not fanned out by topic
    %% interest. Keyed by realm only, no topic dimension.
    overlay_subscriptions = #{} :: #{reference() => overlay_subscription()},
    overlay_realm_index   = #{} :: #{<<_:256>> => sets:set(reference())},
    %% Monotonic per-link publish sequence (stamps outbound PUBLISH
    %% frames). Resets on link respawn — pool dedup absorbs the gap.
    publish_seq = 0 :: non_neg_integer(),
    %% Advertised RPC procedures. Keyed by `{Realm, Procedure}`. The
    %% link sends nothing for them. Inbound CALL frames whose
    %% `(realm, procedure)' is in this map are dispatched to the
    %% registered handler; the resulting RESULT or call_error frame is
    %% shipped back over the same peering connection.
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
    %% decoded off it). An entry is created when this link opens a
    %% client stream, or when a stream the peer opened brings its first
    %% whole frame, and removed when the session tears down.
    stream_bufs = #{} :: #{reference() => binary()},
    %% A dedicated stream the peer opened is buffered here until its first
    %% whole frame comes, which decides whether the stream moves on to
    %% `stream_bufs' or closes. A dedicated stream stays open only while it
    %% carries a session or is this link's own client stream.
    opening_bufs = #{} :: #{reference() => binary()},
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
    content_pending = #{}     :: #{reference() => {gen_server:from(), reference(), macula_frame:verified_request()}},
    %% App-level liveness state. `liveness_timer' is the next-tick
    %% reference (or undefined when not armed). `liveness_outstanding'
    %% holds the request_id and the request of an in-flight probe (or
    %% undefined when no probe is awaiting reply): a reply is checked
    %% against that request. `liveness_misses' is the consecutive-
    %% miss count; reaches `?LIVENESS_MAX_MISSES' → close peer_pid.
    liveness_timer        :: undefined | reference(),
    liveness_outstanding  :: undefined | {<<_:128>>, macula_frame:verified_request()},
    liveness_misses = 0   :: non_neg_integer(),
    %% Replies refused before they could clear a probe, counted by reason.
    refused_replies       :: macula_refusal_report:t(),
    %% Relayed overlay frames dropped, and refusals of delivered overlay
    %% frames charged to no connection, counted by kind.
    refused_relays        :: macula_refusal_report:t(),
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
    connect_watchdog_ms   :: undefined | non_neg_integer(),
    %% Auth policy per advertised STREAMING procedure, kept apart from the
    %% unary `policies' map so unadvertising a unary procedure never clears
    %% the policy of a stream advertised under the same name. Absent means
    %% `open'.
    stream_policies = #{} :: #{{<<_:256>>, binary()} =>
                                   macula_client:auth_policy()}
}).

-type subscription() :: {Realm     :: <<_:256>>,
                         Topic     :: binary(),
                         Subscriber :: pid(),
                         Mon        :: reference()}.

-type overlay_subscription() :: {Realm      :: <<_:256>>,
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
%% issue `call/6' (which blocks the caller until ready or until its
%% timeout elapses).
-spec start_link(opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) when is_map(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Issue a CALL to `Target' and block until its verified reply, the
%% deadline, or the connection dropping.
%%
%% `Target' is `station', the station this link is connected to, for the
%% procedures a station serves itself such as `_dht.*', or a provider's
%% node_id. `Realm' is the 32-byte realm id and `Procedure' the procedure
%% name. `Payload' is any term the wire carries (typically a map).
%% `TimeoutMs' is from 1 ms to ten minutes, the deadline window a provider
%% accepts; anything else raises `function_clause' in the caller.
%%
%% The result is `{ok, Payload}' for a RESULT; `{error, Text}' for a
%% provider's `handler_error' carrying its detail text; `{error, {call_error,
%% Code, Detail}}' for any other provider error, `Code' a binary and `Detail'
%% a binary or `undefined'; `{error, {call_error, unknown_next_peer,
%% undefined}}' when the station reports it holds no connection to the
%% target; or `{error, Reason}' for a call refused, timed out or lost with
%% the connection (see `not_sent/1').
-spec call(pid(), station | <<_:256>>, <<_:256>>, binary(), term(), 1..600_000) ->
    {ok, term()} | {error, term()}.
call(Pid, Target, Realm, Procedure, Payload, TimeoutMs) ->
    call(Pid, Target, Realm, Procedure, Payload, TimeoutMs, <<>>).

%% @doc As `call/6', presenting a capability token to a gated provider. An
%% empty token is none.
-spec call(pid(), station | <<_:256>>, <<_:256>>, binary(), term(), 1..600_000, binary()) ->
    {ok, term()} | {error, term()}.
call(Pid, Target, Realm, Procedure, Payload, TimeoutMs, Token)
  when is_pid(Pid),
       (Target =:= station orelse (is_binary(Target) andalso byte_size(Target) =:= 32)),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       is_integer(TimeoutMs), TimeoutMs > 0, TimeoutMs =< ?MAX_CALL_TIMEOUT_MS,
       is_binary(Token) ->
    %% The deadline is the caller's: a link busy past it doesn't send the
    %% CALL at all (see call_in_time/4). gen_server timeout = TimeoutMs +
    %% 500 to give the server time to report a clean `{error, timeout}'
    %% rather than the caller seeing a hard `exit({timeout, ...})'.
    DeadlineMs = erlang:system_time(millisecond) + TimeoutMs,
    GenTimeout = TimeoutMs + 500,
    try
        gen_server:call(Pid,
                        {call, Target, Realm, Procedure, Payload, DeadlineMs, Token},
                        GenTimeout)
    catch
        %% The exits of a gen_server call, read as call results. No link
        %% process is `noproc': the call never reached a link. A link that
        %% ended while the call still waited in its mailbox answers
        %% `{link_stopped, Name}', as a link that stops answers its pending
        %% calls, so no stop reads as a call that never went out. A caller is
        %% told the reason's name, as a stream is.
        exit:{timeout, _}                    -> {error, timeout};
        exit:{noproc, _}                     -> {error, noproc};
        exit:{Reason, {gen_server, call, _}} -> {error, {link_stopped, macula_reason_name:text(Reason)}}
    end.

%% @doc Whether an error from `call/6,7' means the CALL never went out, so
%% the call can be tried on another link without a provider running it
%% twice: the link was not connected yet, there was no link process, or
%% the link refused the call before sending it. Any other error, a timeout
%% or a station's `unknown_next_peer' included, may follow a CALL that
%% reached its provider. It matches only terms the link builds; a
%% provider's code or detail is a binary and never matches.
-spec not_sent({error, term()}) -> boolean().
not_sent({error, not_connected}) -> true;
not_sent({error, noproc}) -> true;
not_sent({error, {refused, _Reason}}) -> true;
not_sent({error, _Reason}) -> false.

%% @doc Open a dedicated QUIC stream for a sequence of related unary
%% CALLs — content transfer's one purpose so far (see
%% PLAN_PER_STREAM_QUIC_ISOLATION.md Phase 2). NOT a general-purpose
%% "any RPC can have its own stream" facility: `call/6,7' remains the
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
        %% Read as call/7 reads the exits of its call, the reason by name only.
        exit:{timeout, _}                    -> {error, timeout};
        exit:{noproc, _}                     -> {error, noproc};
        exit:{Reason, {gen_server, call, _}} -> {error, {link_stopped, macula_reason_name:text(Reason)}}
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
    classify_put(call(Pid, station, ?DHT_REALM,<<"_dht.put_record">>,
                      Record, TimeoutMs)).

classify_put({ok, ok})       -> ok;
classify_put({ok, Other})    -> {error, {unexpected_reply, Other}};
classify_put({error, _} = E) -> E.

%% @doc Convenience wrapper for `_dht.find_record'. Looks up a record
%% by its `macula_record:storage_key/1': 32 bytes, either the record's own
%% key (for some record types) or a SHA-256 digest (for the rest).
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
    classify_find(call(Pid, station, ?DHT_REALM,<<"_dht.find_record">>,
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
    classify_records(call(Pid, station, ?DHT_REALM,<<"_dht.find_records_by_type">>,
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
%%       with `realm', `publisher', `publisher_verified'
%%       (`not_signed' | `true' | `false'), `seq', and `delivered_via'
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

%% @doc As `unsubscribe/2', without waiting for the link. The request
%% queues behind whatever the caller sent this link before it, so a
%% `subscribe/4' the same caller makes afterwards reaches the link after
%% it. For a caller that must not wait on a busy link, such as the pool.
-spec unsubscribe_async(pid(), reference()) -> ok.
unsubscribe_async(Client, SubRef)
  when is_pid(Client), is_reference(SubRef) ->
    gen_server:cast(Client, {unsubscribe, SubRef}).

%% @doc Register an RPC procedure handler on this link. A CALL for
%% `(Realm, Procedure)' that the connected station delivers to this
%% node, by its target, is dispatched to `Handler'. The link sends no
%% frame for it.
%%
%% Idempotent: re-advertising replaces the prior handler. The pool
%% registers its handlers again on a link it respawns.
%%
%% Returns once the handler is registered.
%%
%% Handlers run in a transient process spawned per CALL. They must
%% return `{ok, Reply}', `{error, Reason}', or any other term (treated
%% as `{ok, Other}' shorthand). A handler crash is mapped to a
%% structured `temporary_relay_failure' BOLT#4 error.
-spec advertise(pid(), <<_:256>>, binary(), handler()) -> ok | {error, term()}.
advertise(Pid, Realm, Procedure, Handler) ->
    advertise(Pid, Realm, Procedure, Handler, open).

%% @doc Advertise with an auth policy -- see `macula_client:auth_policy()'
%% for the full set (`open' | `{ucan_required, Issuer}' |
%% `{realm_member_required, RealmDid, RequiredCan}').
%%
%% `Policy''s own shape is validated HERE, at the call boundary, one
%% clause per valid shape with no catch-all: a malformed `Issuer'/
%% `RealmDid' (wrong type, wrong byte size) raises `function_clause' in
%% the CALLING process before ever reaching this link's gen_server loop,
%% rather than being accepted now and only crashing later -- inside the
%% loop, on the first inbound CALL that exercises `authorize_policy/2' --
%% which would fault every OTHER procedure multiplexed on the same link,
%% not just this one (Fable review, 2026-09-05).
-spec advertise(pid(), <<_:256>>, binary(), handler(),
                macula_client:auth_policy()) -> ok.
advertise(Pid, Realm, Procedure, Handler, open = Policy)
  when is_pid(Pid),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       (is_function(Handler, 1) orelse
        (is_tuple(Handler) andalso tuple_size(Handler) =:= 2)) ->
    gen_server:call(Pid, {advertise, Realm, Procedure, Handler, Policy}, 5_000);
advertise(Pid, Realm, Procedure, Handler, {ucan_required, Issuer} = Policy)
  when is_pid(Pid),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       (is_function(Handler, 1) orelse
        (is_tuple(Handler) andalso tuple_size(Handler) =:= 2)),
       is_binary(Issuer), byte_size(Issuer) =:= 32 ->
    gen_server:call(Pid, {advertise, Realm, Procedure, Handler, Policy}, 5_000);
advertise(Pid, Realm, Procedure, Handler,
          {realm_member_required, RealmDid, RequiredCan} = Policy)
  when is_pid(Pid),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       (is_function(Handler, 1) orelse
        (is_tuple(Handler) andalso tuple_size(Handler) =:= 2)),
       is_binary(RealmDid), byte_size(RealmDid) =:= 32,
       is_binary(RequiredCan), RequiredCan =/= <<>> ->
    gen_server:call(Pid, {advertise, Realm, Procedure, Handler, Policy}, 5_000).

%% @doc Drop a previously-advertised procedure's handler from this link.
%% Sends nothing. Idempotent: unknown `(Realm, Procedure)' is a no-op.
-spec unadvertise(pid(), <<_:256>>, binary()) -> ok | {error, term()}.
unadvertise(Pid, Realm, Procedure)
  when is_pid(Pid),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure) ->
    gen_server:call(Pid, {unadvertise, Realm, Procedure}, 5_000).

%% @doc Subscribe to overlay-protocol frames for `Realm' — any frame
%% type the built-in call/event/result/error handling above doesn't
%% recognise (today: HyParView `hyparview_*', Plumtree `plumtree_*';
%% SWIM and content-transfer frames have their own dedicated paths and
%% never reach this). Every such frame whose `realm' field matches
%% `Realm' is delivered to `Subscriber'.
%%
%% Unlike `subscribe/4' there is no wire-level SUBSCRIBE/UNSUBSCRIBE
%% round trip: these frames already arrive addressed at this specific
%% connection by the station (a direct peer-to-peer protocol, not a
%% topic fan-out), so registration is purely local bookkeeping and
%% takes effect immediately regardless of connection state.
%%
%% Subscriber receives one of:
%%
%% <ul>
%%   <li>`{macula_overlay_frame, SubRef, Frame, Meta}': every time a
%%       matching frame arrives. `Frame' is the fully decoded frame
%%       map (a `record' field, if the frame carried one, as its wire
%%       form, the form `macula_hyparview_endorsement:verify_endorsement/3'
%%       takes; see `macula_frame:hyparview_join_spec()'). `Meta' is a map
%%       with a `sender' field: the connected peer's NodeId, or for a frame
%%       relayed in an `overlay_relay' envelope, the envelope's origin, since
%%       a frame does not self-identify its sender at the application
%%       layer. A relayed frame's `Meta' also has `via', the NodeId of the
%%       station that relayed it.</li>
%%   <li>`{macula_overlay_gone, SubRef, Reason}' — once, when the
%%       connection drops or the client stops. The subscription is
%%       cleared on the same transition.</li>
%% </ul>
%%
%% The client monitors `Subscriber'; if it dies the subscription is
%% torn down.
%%
%% A frame goes to the subscribers of its `realm'. A `plumtree_gossip'
%% names its realm only inside its publication, so it goes to the
%% subscribers of the realm its publication claims
%% (`macula_frame:claimed_publication_realm/1'). A relayed plumtree_gossip
%% frame is delivered as it arrived: its publication is unverified until
%% macula_frame:verify_publication/3 accepts it, and a subscriber other
%% than the Plumtree layer verifies it before acting on it. Whoever wires
%% the Plumtree layer to a link makes it the verifying consumer, and keeps
%% plumtree frames from reaching any other overlay subscriber unverified.
%% A relayed frame, or a GOSSIP from the connected peer, that names no
%% realm, or a realm with no subscriber on this link, is counted and not
%% delivered. Any other frame from the connected peer with no subscriber
%% for its realm is dropped.
%%
%% A subscriber that refuses what a delivered frame carries reports it
%% with `overlay_frame_refused/3'. A refusal of what a relayed frame
%% carries is counted on this link by kind and never charged to the
%% station connection that relayed it; a refusal of a frame from the
%% connected peer is charged to that connection as
%% `macula_frame:charged_refusal/1' says.
-spec overlay_subscribe(pid(), <<_:256>>, pid()) ->
    {ok, reference()} | {error, term()}.
overlay_subscribe(Client, Realm, Subscriber)
  when is_pid(Client),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_pid(Subscriber) ->
    gen_server:call(Client, {overlay_subscribe, Realm, Subscriber}, 5_000).

%% @doc Drop an overlay-frame subscription. Idempotent: unknown
%% `SubRef' is a no-op. Always returns `ok'.
-spec overlay_unsubscribe(pid(), reference()) -> ok.
overlay_unsubscribe(Client, SubRef)
  when is_pid(Client), is_reference(SubRef) ->
    gen_server:call(Client, {overlay_unsubscribe, SubRef}, 5_000).

%% @doc Send a pre-built, pre-signed overlay-protocol frame directly
%% over this link's peering connection. The caller is responsible for
%% constructing and signing `Frame' (e.g. via `macula_hyparview_proto:
%% build_join/1' or `macula_hyparview_endorsement:build_join/4') — this
%% is a raw transport primitive, not a builder. `{error, not_connected}'
%% if the peering handshake hasn't completed; unlike `subscribe'/
%% `advertise' there is no drain-on-reconnect queue, since a caller
%% building protocol-level frames (like a HyParView JOIN) needs to know
%% immediately whether it actually went out, not have it silently
%% queued behind a reconnect that may invalidate the frame's contents
%% (e.g. a JOIN naming a peer that was only reachable via the old link).
%%
%% Delivers to whoever is on the OTHER END of this specific connection —
%% typically only correct when that's the intended logical peer itself
%% (a direct station-to-station link, or a connection already scoped to
%% one target some other way). To reach a specific third-party peer by
%% its own NodeId through a relay, use `send_overlay_frame/3' instead.
-spec send_overlay_frame(pid(), macula_frame:frame()) -> ok | {error, term()}.
send_overlay_frame(Client, Frame) when is_pid(Client), is_map(Frame) ->
    gen_server:call(Client, {send_overlay_frame, Frame}, 5_000).

%% @doc Send a pre-built, pre-signed overlay-protocol frame to a specific
%% `TargetPeer' (that peer's 32-byte node_id), relayed through whatever
%% station this connection is dialed into. Wraps `Frame' in an
%% `overlay_relay' envelope (Part 6 §9.x), a control frame the connection
%% signs for its neighbour as it sends it, so the station relays it as sent
%% by this connection's authenticated node_id and a claimed `TargetPeer'
%% can never be spoofed by an unrelated connection. The station forwards it
%% to whichever of its OTHER connections authenticates as `TargetPeer'. See `macula_station_peer_observer:dispatch_overlay/5' on
%% the relay side. The receiving link takes `Frame' only when its type is
%% one `macula_frame:relayed_without_signature/1' names, and delivers it
%% with this connection's authenticated NodeId as sender and the station as
%% `via'; a frame of any other type is dropped and counted there. This
%% function only wraps it in the envelope, and never touches `Frame'.
%% Silently dropped by the station if `TargetPeer' isn't currently
%% connected there; HyParView's own periodic shuffle/retry is the
%% recovery path, the same way it already tolerates ordinary packet loss.
%%
%% `{error, not_connected}' if the peering handshake to the station
%% itself hasn't completed.
-spec send_overlay_frame(pid(), <<_:256>>, macula_frame:frame()) ->
    ok | {error, term()}.
send_overlay_frame(Client, TargetPeer, Frame)
  when is_pid(Client), is_binary(TargetPeer), byte_size(TargetPeer) =:= 32,
       is_map(Frame) ->
    gen_server:call(Client, {send_overlay_frame_to, TargetPeer, Frame}, 5_000).

%% @doc Report that an overlay frame this link delivered was refused for
%% what it carries. `Meta' is the map the frame was delivered with, and
%% `Kind' the refusal's kind, as `hecate_plumtree' returns it. The report
%% goes to the connection, which charges it as
%% `macula_frame:charged_refusal/1' says, only when the frame provably came
%% from the link's current peer: `Meta' has no `via' and names that peer as
%% `sender'. Every other report, for a relayed frame, for a `Meta' that lost
%% its `via', or from before the link took another peer, is counted on this
%% link by kind and charges no one. A kind `charged_refusal/1' does not
%% classify is counted as `unknown_refusal'. A cast: it never blocks the
%% caller, and a link that has stopped ignores it.
-spec overlay_frame_refused(pid(), map(), term()) -> ok.
overlay_frame_refused(Client, Meta, Kind) when is_pid(Client) ->
    gen_server:cast(Client, {overlay_frame_refused, Meta, Kind}).

%% @doc Open a streaming RPC on this link. Returns `{ok, StreamPid}'
%% bound to the caller; the caller drives the stream via
%% `macula_stream:send/2,3', `recv/1,2', `close_send/1', `close/1',
%% and `await_reply/1,2' (for client-stream / bidi modes).
%%
%% `Target' is the provider the open names: `station', the station this
%% link is connected to, or a provider's node_id. `Realm' and `Procedure'
%% name the remote streaming endpoint. `Args' is the opening payload (any
%% term `macula_frame:stream_open/2' takes as a payload). `Opts' may include:
%%
%% <ul>
%%   <li>`mode'  — `server_stream' (default), `client_stream', or
%%                 `bidi'.</li>
%%   <li>`owner' — the pid that owns the stream lifecycle (default:
%%                 the calling pid). Stream stops when the owner
%%                 dies.</li>
%%   <li>`deadline_ms' — wall-clock deadline stamped on the
%%                 STREAM_OPEN frame (default: now + 30s).</li>
%%   <li>`ucan_token' — a UCAN presented to a streaming procedure
%%                 advertised with an auth policy
%%                 (`advertise_stream/6'). Absent or empty sends
%%                 none.</li>
%% </ul>
%%
%% Returns `{error, not_connected}' when the QUIC handshake has not
%% completed; the caller may retry once the link reports
%% `is_connected/1'. Returns `{error, {open_too_large, Limit}}', sending
%% nothing and starting no stream, when the signed STREAM_OPEN would be
%% longer than `Limit' bytes, the `max_stream_open_bytes' macula
%% application env (1 MiB by default), and `{error, {refused, Why}}' the
%% same way for an open the frame refuses to build, such as a procedure
%% name over 512 bytes. A consumer opens a stream through
%% `macula:call_stream/5', which resolves the provider and its station,
%% or `macula:call_stream_station/7', rather than on a link directly.
-spec call_stream(pid(), station | <<_:256>>, <<_:256>>, binary(), term(), map()) ->
    {ok, pid()} | {error, term()}.
call_stream(Pid, Target, Realm, Procedure, Args, Opts)
  when is_pid(Pid),
       (Target =:= station orelse (is_binary(Target) andalso byte_size(Target) =:= 32)),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       is_map(Opts) ->
    ok = valid_stream_opts(Opts),
    gen_server:call(Pid, {stream_open, Target, Realm, Procedure, Args, Opts, self()}, 5_000).

%% A stream open's options are checked here, in the calling process, as
%% `advertise_stream/6' checks a policy: one outside its type raises
%% `function_clause' in the caller and never in the link.
valid_stream_opts(Opts) ->
    ok = valid_stream_mode(maps:get(mode, Opts, server_stream)),
    ok = valid_stream_token(maps:get(ucan_token, Opts, <<>>)),
    valid_stream_deadline(maps:get(deadline_ms, Opts, 0)).

valid_stream_mode(Mode) when Mode =:= server_stream; Mode =:= client_stream; Mode =:= bidi -> ok.

valid_stream_token(Token) when is_binary(Token) -> ok.

valid_stream_deadline(DeadlineMs) when is_integer(DeadlineMs), DeadlineMs >= 0 -> ok.

%% @doc Register a streaming RPC handler on this link. Idempotent —
%% re-advertising replaces the prior `{Mode, Handler}'. A STREAM_OPEN
%% for `(Realm, Procedure)' that the connected station delivers to this
%% node spawns a server-side `macula_stream' and dispatches
%% `Handler(StreamPid, Args)' in a transient process. The link sends no
%% frame for it. Same as `advertise_stream/6' with policy `open'.
-spec advertise_stream(pid(), <<_:256>>, binary(),
                        macula_frame:stream_mode(), stream_handler()) ->
    ok | {error, term()}.
advertise_stream(Pid, Realm, Procedure, Mode, Handler) ->
    advertise_stream(Pid, Realm, Procedure, Mode, Handler, open).

%% @doc Advertise a streaming RPC handler with an auth policy, the same
%% `macula_client:auth_policy()' set `advertise/5' takes. An inbound
%% STREAM_OPEN the policy refuses gets a STREAM_ERROR with code
%% `<<"unauthorized">>' on its stream and runs no handler. The policy's
%% shape is checked here, in the calling process, as `advertise/5' does.
-spec advertise_stream(pid(), <<_:256>>, binary(),
                        macula_frame:stream_mode(), stream_handler(),
                        macula_client:auth_policy()) ->
    ok | {error, term()}.
advertise_stream(Pid, Realm, Procedure, Mode, Handler, Policy)
  when is_pid(Pid),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       (Mode =:= server_stream orelse Mode =:= client_stream
        orelse Mode =:= bidi),
       is_function(Handler, 2) ->
    ok = valid_policy(Policy),
    gen_server:call(Pid,
                    {stream_advertise, Realm, Procedure, Mode, Handler, Policy},
                    5_000).

%% One clause per valid `macula_client:auth_policy()' shape and no
%% catch-all, so a malformed policy raises `function_clause' in the caller.
valid_policy(open) -> ok;
valid_policy({ucan_required, <<_:256>>}) -> ok;
valid_policy({realm_member_required, <<_:256>>, RequiredCan})
  when is_binary(RequiredCan), RequiredCan =/= <<>> -> ok.

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

%% @doc Write the bytes of one frame a paired `macula_stream' built,
%% signed and encoded onto that stream's dedicated QUIC stream. `Last'
%% is true for the last frame from the stream's side (STREAM_END with
%% role both, STREAM_ERROR or STREAM_REPLY), after which the link
%% forgets the stream. A write that fails is reported to the stream as
%% `{stream_write_failed, Sid, Reason}', and the link forgets the stream.
%%
%% Always returns `ok': the write happens in the link.
-spec send_stream_bytes(pid(), binary(), binary(), boolean()) -> ok.
send_stream_bytes(Pid, Sid, Bytes, Last)
  when is_pid(Pid), is_binary(Sid), is_binary(Bytes), is_boolean(Last) ->
    gen_server:cast(Pid, {send_stream_bytes, Sid, Bytes, Last}).

-spec is_connected(pid()) -> boolean().
is_connected(Pid) ->
    case gen_server:call(Pid, is_connected, 1_000) of
        true  -> true;
        false -> false
    end.

-spec peer_node_id(pid()) -> {ok, <<_:256>>} | {error, not_connected}.
peer_node_id(Pid) ->
    gen_server:call(Pid, peer_node_id, 1_000).

%%====================================================================
%% gen_server
%%====================================================================

%% A link starts only with a node identity key, a statement issuer and a
%% seed that names the node_id it expects. Otherwise it refuses with the
%% name of what is missing, in that order, and makes no key of its own.
start_checked(#{node_identity := Identity} = Opts) when is_function(Identity, 0) ->
    issuer_checked(maps:find(issuer, Opts), Identity, Opts);
start_checked(_Opts) ->
    {error, {node_identity, required}}.

issuer_checked({ok, Issuer}, Identity, Opts) when is_pid(Issuer) ->
    admission_checked(maps:find(admission, Opts), Identity, Issuer, Opts);
issuer_checked(_NoIssuer, _Identity, _Opts) ->
    {error, {issuer, required}}.

%% The pool's request admission and this link's share in it are required: every
%% request the link receives is judged there, under that share.
admission_checked({ok, Admission}, Identity, Issuer, Opts) when is_pid(Admission) ->
    share_checked(is_map_key(share, Opts), Identity, Issuer, Opts);
admission_checked(_NoAdmission, _Identity, _Issuer, _Opts) ->
    {error, {admission, required}}.

share_checked(true, Identity, Issuer, Opts) ->
    stream_functions_checked(first_refusal([stream_function(open_stream, 1, not_an_opener, Opts),
                                            stream_function(send_on_stream, 2, not_a_writer, Opts),
                                            stream_function(close_stream, 1, not_a_closer, Opts)]),
                             Identity, Issuer, Opts);
share_checked(false, _Identity, _Issuer, _Opts) ->
    {error, {share, required}}.

%% A dedicated-stream function the link is given takes the place of the
%% peering connection's own, so it must be a function of the arity the link
%% calls it with.
stream_functions_checked(none, Identity, Issuer, Opts) ->
    identity_checked(Identity(), Issuer, Opts);
stream_functions_checked(Refusal, _Identity, _Issuer, _Opts) ->
    {error, Refusal}.

stream_function(Key, Arity, Refusal, Opts) ->
    given_function(maps:find(Key, Opts), Key, Arity, Refusal).

given_function(error, _Key, _Arity, _Refusal) -> none;
given_function({ok, Fun}, _Key, Arity, _Refusal) when is_function(Fun, Arity) -> none;
given_function({ok, _OtherShape}, Key, _Arity, Refusal) -> {Key, Refusal}.

first_refusal([none | Rest]) -> first_refusal(Rest);
first_refusal([Refusal | _Rest]) -> Refusal;
first_refusal([]) -> none.

identity_checked(#{purpose := identity, profile := Profile} = Key, Issuer, Opts) ->
    seed_checked(add_tls_opts(parse_seed(maps:get(seed, Opts)), Opts), Key, Profile, Issuer);
identity_checked(_NotAnIdentityKey, _Issuer, _Opts) ->
    {error, {node_identity, not_an_identity_key}}.

seed_checked(#{expected_node_id := <<_:256>>} = Seed, Key, Profile, Issuer) ->
    {ok, Seed, Key, Profile, Issuer};
seed_checked(_Seed, _Key, _Profile, _Issuer) ->
    {error, {seed, expected_node_id_required}}.

%% The node_id of the node identity key: the identity the frames this link builds name.
node_id(Key) ->
    {ok, NodeId} = macula_node_keys:node_id(Key),
    NodeId.

%% TLS policy (`verify' / `expected_node_id') rides in the seed map, which
%% is spread into the peering target at connect. The link watches its
%% issuer and ends when the issuer does.
init(Opts) ->
    started(start_checked(Opts), Opts).

started({error, Refusal}, _Opts) ->
    {stop, Refusal};
started({ok, Seed, Key, Profile, Issuer}, Opts) ->
    _ = erlang:monitor(process, Issuer),
    Connect  = maps:get(connect, Opts, fun macula_peering:connect/1),
    OpenStream   = maps:get(open_stream, Opts, fun macula_peering:open_dedicated_stream/1),
    SendOnStream = maps:get(send_on_stream, Opts, fun macula_peering:send_on_stream/2),
    CloseStream  = maps:get(close_stream, Opts, fun macula_peering:close_dedicated_stream/1),
    Caps     = maps:get(capabilities, Opts, 0),
    Alpn     = maps:get(alpn, Opts, [<<"macula">>]),
    Tmo      = maps:get(connect_timeout_ms, Opts, 30_000),
    WdMs     = maps:get(connect_watchdog_ms, Opts, undefined),
    LiveMs   = maps:get(liveness_interval_ms, Opts, app_env(liveness_interval_ms, ?LIVENESS_INTERVAL_MS)),
    LiveMiss = maps:get(liveness_max_misses, Opts, app_env(liveness_max_misses, ?LIVENESS_MAX_MISSES)),
    RetryMs  = maps:get(connect_retry_backoff_ms, Opts, app_env(connect_retry_backoff_ms, ?CONNECT_RETRY_BACKOFF_MS)),
    State    = #state{seed = Seed, node_identity = Key, profile = Profile,
                      issuer = Issuer, admission = maps:get(admission, Opts),
                      share = maps:get(share, Opts), connect = Connect, open_stream = OpenStream,
                      send_on_stream = SendOnStream, close_stream = CloseStream,
                      capabilities = Caps, alpn = Alpn,
                      connect_timeout_ms = Tmo,
                      connect_watchdog_ms = WdMs,
                      liveness_interval_ms = LiveMs,
                      liveness_max_misses = LiveMiss,
                      refused_replies = macula_refusal_report:new(?REFUSED_REPLIES_WINDOW_MS),
                      refused_relays = macula_refusal_report:new(?REFUSED_RELAYS_WINDOW_MS),
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

handle_call({call, _Target, _Realm, _Proc, _Payload, _DeadlineMs, _Token}, _From,
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
handle_call({call, Target, Realm, Proc, Payload, DeadlineMs, Token}, From, S) ->
    call_in_time(DeadlineMs - erlang:system_time(millisecond),
                 {Target, Realm, Proc, Payload, DeadlineMs, Token}, From, S);

handle_call(open_content_stream, _From, #state{peer_node_id = undefined} = S) ->
    {reply, {error, not_connected}, S};
handle_call(open_content_stream, _From,
            #state{peer_pid = Pid, content_stream_bufs = Bufs} = S) ->
    open_content_stream_result(macula_peering:open_dedicated_stream(Pid), Bufs, S);

handle_call({call_on_stream, _Stream, _Realm, _Proc, _Payload, _Tmo}, _From,
            #state{peer_node_id = undefined} = S) ->
    {reply, {error, not_connected}, S};
handle_call({call_on_stream, Stream, Realm, Proc, Payload, Tmo}, From,
            #state{node_identity = Id, peer_node_id = Station,
                   content_pending = CP, content_stream_bufs = Bufs} = S)
        when is_map_key(Stream, Bufs) ->
    CallSpec = #{
        request_id => crypto:strong_rand_bytes(16),
        procedure  => Proc,
        realm      => Realm,
        target     => Station,
        deadline   => erlang:system_time(millisecond) + Tmo,
        payload    => Payload
    },
    await_content_call_reply(
      send_on_content_stream(Stream, CallSpec, Id), Stream, From, Tmo, CP, S);
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

%% Advertising registers the handler on the link and sends nothing.
handle_call({advertise, Realm, Proc, Handler, Policy}, _From,
            #state{procedures = P, policies = Pols} = S) ->
    {reply, ok, S#state{procedures = P#{{Realm, Proc} => Handler},
                        policies   = set_policy({Realm, Proc}, Policy, Pols)}};

handle_call({unadvertise, Realm, Proc}, _From,
            #state{procedures = P, policies = Pols} = S) ->
    {reply, ok, S#state{procedures = maps:remove({Realm, Proc}, P),
                        policies   = maps:remove({Realm, Proc}, Pols)}};

%%-- Overlay-protocol frame transport --------------------------------

handle_call({overlay_subscribe, Realm, Subscriber}, _From,
            #state{overlay_subscriptions = Subs, overlay_realm_index = Idx} = S) ->
    SubRef  = make_ref(),
    Mon     = erlang:monitor(process, Subscriber),
    NewSubs = Subs#{SubRef => {Realm, Subscriber, Mon}},
    NewIdx  = add_realm_sub(Realm, SubRef, Idx),
    {reply, {ok, SubRef}, S#state{overlay_subscriptions = NewSubs,
                                  overlay_realm_index   = NewIdx}};

handle_call({overlay_unsubscribe, SubRef}, _From, S) ->
    {reply, ok, on_overlay_unsubscribe(SubRef, S)};

handle_call({send_overlay_frame, _Frame}, _From,
            #state{peer_pid = undefined} = S) ->
    {reply, {error, not_connected}, S};
handle_call({send_overlay_frame, Frame}, _From, #state{peer_pid = Pid} = S) ->
    Result = try macula_peering:send_frame(Pid, Frame)
             catch C:R -> {error, {C, R}}
             end,
    {reply, Result, S};

handle_call({send_overlay_frame_to, _Target, _Frame}, _From,
            #state{peer_pid = undefined} = S) ->
    {reply, {error, not_connected}, S};
handle_call({send_overlay_frame_to, Target, Frame}, _From,
            #state{peer_pid = Pid} = S) ->
    %% The envelope is a control frame: the connection signs it for its
    %% neighbour as it sends it, and the station relays it as sent by this
    %% connection's authenticated node_id (see macula_station_peer_observer:
    %% dispatch_overlay/5). `Frame' itself (the wrapped inner frame) is the
    %% caller's own responsibility, same as `send_overlay_frame/2'.
    Envelope = macula_frame:overlay_relay(#{peer    => Target,
                                            payload => macula_frame:encode(Frame)}),
    Result = try macula_peering:send_frame(Pid, Envelope)
             catch C:R -> {error, {C, R}}
             end,
    {reply, Result, S};

%%-- Streaming RPC ---------------------------------------------------

handle_call({stream_open, _Target, _R, _P, _A, _O, _Caller}, _From,
            #state{peer_node_id = undefined} = S) ->
    %% Mirror the gating used for `call' / `publish' — STREAM_OPEN
    %% frames sent before HELLO completes hit `drop_unexpected' in
    %% the peering statem and never make it to the wire.
    {reply, {error, not_connected}, S};
handle_call({stream_open, Target, Realm, Proc, Args, Opts, Caller}, _From, S) ->
    {reply_value, Reply, NewS} = open_client_stream(Target, Realm, Proc, Args, Opts, Caller, S),
    {reply, Reply, NewS};

handle_call({stream_advertise, Realm, Proc, Mode, Handler, Policy}, _From,
            #state{stream_procedures = SP, stream_policies = SPols} = S) ->
    {reply, ok, S#state{stream_procedures = SP#{{Realm, Proc} => {Mode, Handler}},
                        stream_policies   = set_policy({Realm, Proc}, Policy, SPols)}};

handle_call({stream_unadvertise, Realm, Proc}, _From,
            #state{stream_procedures = SP, stream_policies = SPols} = S) ->
    {reply, ok, S#state{stream_procedures = maps:remove({Realm, Proc}, SP),
                        stream_policies   = maps:remove({Realm, Proc}, SPols)}};

handle_call(_Req, _From, S) ->
    {reply, {error, unknown_call}, S}.

%%-- Outbound STREAM_* bytes from a paired macula_stream -------------

handle_cast({send_stream_bytes, Sid, Bytes, Last}, S) ->
    {noreply, stream_bytes_sent(find_stream(Sid, S), Sid, Bytes, Last, S)};

handle_cast({close_content_stream, Stream}, S) ->
    {noreply, close_content_stream_state(Stream, S)};

handle_cast({abort_content_stream, Stream, Code, Message}, S) ->
    macula_diagnostics:event(<<"_macula.station_link.content_abort">>,
                             #{stream => Stream, code => Code,
                               message => Message}),
    {noreply, abort_content_stream_state(Stream, Code, S)};

handle_cast({unsubscribe, SubRef}, S) ->
    {noreply, on_unsubscribe(SubRef, S)};

handle_cast({overlay_frame_refused, Meta, Kind}, S) ->
    {noreply, overlay_refusal(macula_frame:refusal_charge(Kind), Meta, Kind, S)};

handle_cast(_Msg, S) -> {noreply, S}.

%%-------------------------------------------------------------------
%% Connect
%%-------------------------------------------------------------------

%% Each dial carries the node identity key, the pool's issuer and a target
%% that names the station's node_id.
handle_info(attempt_connect, #state{seed = Seed, node_identity = Key, issuer = Issuer,
                                    connect = Connect, capabilities = Caps, alpn = Alpn,
                                    connect_timeout_ms = Tmo} = S) ->
    PeeringOpts = #{
        role            => client,
        target          => Seed#{alpn => Alpn, timeout_ms => Tmo},
        identity        => Key,
        issuer          => Issuer,
        capabilities    => Caps,
        controlling_pid => self()
    },
    after_connect_request(Connect(PeeringOpts), S);

handle_info({macula_peering, connected, Pid, PeerNodeId},
            #state{peer_pid = Pid} = S) ->
    %% Handshake completed — cancel the connect watchdog and hand over
    %% to the steady-state app-liveness probe.
    S1 = cancel_connect_watchdog(S),
    NewS = arm_liveness(S1#state{peer_node_id = PeerNodeId,
                                 liveness_misses = 0,
                                 liveness_outstanding = undefined}),
    drain_pending_subscribes(NewS),
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
            #state{peer_pid = Pid, seed = Seed} = S) ->
    %% The one place the PEER-SIDE disconnect reason (e.g. `peer_closed'
    %% detail, `drained') is ever known. Everything downstream of this
    %% -- `fail_all_pending' and the eventual `{stop, normal, ...}' --
    %% discards it in favor of a uniform `normal' exit, which is all
    %% `macula_client:on_down_routed/5' has left to log. Without this,
    %% a station-initiated close is indistinguishable from any other
    %% disconnect in every log this link ever produces.
    macula_diagnostics:event(notice, <<"_macula.station_link.disconnected">>, #{
        seed     => Seed,
        peer_pid => Pid,
        reason   => macula_reason_name:text(Reason)
    }),
    NewS = fail_all_pending({disconnected, macula_reason_name:text(Reason)}, cancel_liveness(S)),
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
            #state{peer_pid = Pid, opening_bufs = Opening} = S) ->
    _ = erlang:send_after(application:get_env(macula, dedicated_stream_open_timeout_ms,
                                              ?DEDICATED_STREAM_OPEN_TIMEOUT_MS),
                          self(), {dedicated_stream_open_deadline, Stream}),
    {noreply, S#state{opening_bufs = Opening#{Stream => <<>>}}};
handle_info({macula_peering, new_dedicated_stream, _OtherPid, _Stream}, S) ->
    %% Stale notification from a link that is no longer `peer_pid'
    %% (respawned mid-flight) — nothing to attach it to.
    {noreply, S};

%% A dedicated stream the peer opened that brought no whole first frame
%% within `dedicated_stream_open_timeout_ms' closes without a word.
handle_info({dedicated_stream_open_deadline, Stream}, #state{opening_bufs = Opening} = S)
        when is_map_key(Stream, Opening) ->
    ok = close_dedicated_stream(Stream, S),
    {noreply, S#state{opening_bufs = maps:remove(Stream, Opening)}};
handle_info({dedicated_stream_open_deadline, _Stream}, S) ->
    {noreply, S};

%% Bytes on a dedicated stream the peer opened, before its first whole
%% frame. That frame is read with a cap of `max_stream_open_bytes', the way
%% the handshake reads its frames, so a longer length header closes the
%% stream as soon as it arrives (`opening_parse/2'). Once a whole first
%% frame has come, the stream's buffer moves on to `stream_bufs' and the
%% first item decides whether the stream stays open; the items after it go
%% the way of any dedicated stream's (`opening_items/3').
handle_info({quic, Bin, Stream, _Flags}, #state{opening_bufs = Opening} = S)
        when is_binary(Bin), is_map_key(Stream, Opening) ->
    Bytes = <<(maps:get(Stream, Opening))/binary, Bin/binary>>,
    {noreply, opening_items(opening_parse(macula_frame:parse_received(Bytes, stream_open_limit()), Bytes),
                            Stream, S)};

%% Bytes on one of our dedicated streams. Decode whatever complete
%% frames are available and dispatch each; the tail (a partial frame)
%% stays buffered for the next chunk, same as the shared-stream case
%% in `macula_peering_conn:connected/3'. Bytes that do not decode, or a
%% frame missing a field its type requires, end this stream only
%% (`dedicated_items/3').
handle_info({quic, Bin, Stream, _Flags}, #state{stream_bufs = Bufs} = S)
        when is_binary(Bin), is_map_key(Stream, Bufs) ->
    Buf = maps:get(Stream, Bufs),
    {noreply, dedicated_items(macula_frame:parse_received(<<Buf/binary, Bin/binary>>), Stream, S)};

%% Bytes on one of our content-transfer streams (opened via
%% `open_content_stream/1', always by us — content is never
%% peer-initiated, unlike streaming RPC's inbound STREAM_OPEN case,
%% so there is no `new_dedicated_stream' seeding clause to match this
%% one). Bytes that do not decode, or a reply missing a field it
%% requires, end the stream and fail the call waiting on it
%% (`content_items/3').
handle_info({quic, Bin, Stream, _Flags},
            #state{content_stream_bufs = Bufs} = S)
        when is_binary(Bin), is_map_key(Stream, Bufs) ->
    Buf = maps:get(Stream, Bufs),
    {noreply, content_items(macula_frame:parse_received(<<Buf/binary, Bin/binary>>), Stream, S)};

%% A write on one of our dedicated streams failed: the sessions it carries
%% can send nothing more, so they end as they do when the link is lost.
handle_info({quic, send_failed, Stream, Reason}, #state{stream_bufs = Bufs} = S)
        when is_map_key(Stream, Bufs) ->
    {noreply, end_sessions_on_stream(Stream, {send_failed, Reason}, S)};

%% A write on one of our content-transfer streams failed: no call can
%% reach the peer on it any more, so it is torn down as on a close, and a
%% call still waiting on it fails with the write's reason.
handle_info({quic, send_failed, Stream, Reason},
            #state{content_stream_bufs = Bufs} = S)
        when is_map_key(Stream, Bufs) ->
    {noreply, teardown_content_stream_state(Stream, {error, {send_failed, Reason}},
                                            fun macula_quic:close_stream/1, S)};

handle_info({call_timeout, RequestId}, #state{pending = P} = S) ->
    on_timeout(maps:take(RequestId, P), S);

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

handle_info({'EXIT', Pid, Reason}, #state{peer_pid = Pid, seed = Seed} = S) ->
    %% Same swallowed-reason gap as the `disconnected' clause above, for
    %% the case where the peering worker itself exits (crash or
    %% deliberate stop) rather than sending a `disconnected' notification.
    macula_diagnostics:event(notice, <<"_macula.station_link.peering_exit">>, #{
        seed     => Seed,
        peer_pid => Pid,
        reason   => macula_reason_name:text(Reason)
    }),
    NewS = fail_all_pending({peering_exit, macula_reason_name:text(Reason)}, cancel_liveness(S)),
    {stop, normal, NewS#state{peer_pid = undefined,
                              peer_node_id = undefined}};

%% The issuer every connection of this link draws from is gone: the link
%% ends with a shutdown reason, so its end is no crash report, its waiting
%% callers are answered as it ends (terminate/2), and the pool starts it
%% again with the pool's next issuer.
handle_info({'DOWN', _Mon, process, Issuer, Reason}, #state{issuer = Issuer} = S) ->
    {stop, {shutdown, {issuer_down, Reason}}, S};
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

%% A link ends answering every caller still waiting on it, whatever it ends
%% for, so no caller waits out its timeout or reads the end as a call that
%% never went out. A stop that already failed its callers left none waiting.
%% A caller is told the reason's name only.
terminate(Reason, #state{peer_pid = Pid} = S) when is_pid(Pid) ->
    answer_waiting_callers({link_stopped, macula_reason_name:text(Reason)}, S),
    try macula_peering:close(Pid, client_stop) catch _:_ -> ok end,
    ok;
terminate(Reason, S) ->
    answer_waiting_callers({link_stopped, macula_reason_name:text(Reason)}, S).

answer_waiting_callers(Reason, #state{pending = Pending, content_pending = ContentPending}) ->
    maps:foreach(fun(_RequestId, {From, _TRef, _Request}) -> gen_server:reply(From, {error, Reason}) end, Pending),
    maps:foreach(fun(_Stream, {From, _TRef}) -> gen_server:reply(From, {error, Reason}) end, ContentPending).

code_change(_OldVsn, S, _Extra) -> {ok, S}.

%% Status output and crash reports show this process's keys with their private halves redacted.
format_status(Status) -> macula_node_keys:redacted(Status).

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

%% A CALL the link reaches after its caller's deadline is not sent: the
%% caller has already been told it timed out, and a provider must not run a
%% call its caller gave up on. Otherwise the request carries the caller's
%% deadline, and the call waits for its reply until then.
call_in_time(RemainingMs, _Call, _From, S) when RemainingMs =< 0 ->
    {reply, {error, timeout}, S};
call_in_time(RemainingMs, {_Target, _Realm, Proc, Payload, _DeadlineMs, _Token} = Call, From, S) ->
    call_sendable(sendable(macula_frame:text_checked(procedure, Proc), Payload), RemainingMs, Call, From, S).

%% A procedure the frame's text bound refuses, or a payload the wire cannot
%% carry, is refused before anything is built.
sendable(ok, Payload) -> macula_frame:check_payload(Payload);
sendable({error, _} = Refused, _Payload) -> Refused.

%% A call refused before building never goes out. Otherwise the request is
%% signed with the node identity key and kept as a verifier reads it, for its
%% reply to be checked against. A request that does not verify as built is
%% refused as well, so no caller's argument takes the link down.
call_sendable({error, Unsendable}, _RemainingMs, _Call, _From, S) ->
    {reply, {error, {refused, Unsendable}}, S};
call_sendable(ok, RemainingMs, {Target, Realm, Proc, Payload, DeadlineMs, Token}, From,
              #state{node_identity = Key, profile = Profile} = S) ->
    RequestId = crypto:strong_rand_bytes(16),
    Frame = macula_frame:call(with_token(Token, #{request_id => RequestId, realm => Realm, procedure => Proc,
                                                   target => target_node_id(Target, S), deadline => DeadlineMs,
                                                   payload => Payload}), Key),
    call_verified(macula_frame:verify_request(Frame, Profile), Frame, RequestId, From, RemainingMs, S).

call_verified({ok, Request}, Frame, RequestId, From, RemainingMs, #state{peer_pid = Pid, pending = P} = S) ->
    %% NOT `ok = send_frame(...)': a frame the peering refuses comes back as
    %% an error, and a hard match would take this link down for every other
    %% caller on it. Reply with the reason instead.
    await_call_reply(macula_peering:send_frame(Pid, Frame), RequestId, Request, From, RemainingMs, P, S);
call_verified({error, Refusal}, _Frame, _RequestId, _From, _RemainingMs, S) ->
    {reply, {error, {refused, Refusal}}, S}.

target_node_id(station, #state{peer_node_id = Station}) -> Station;
target_node_id(NodeId, _S) -> NodeId.

with_token(<<>>, Spec) -> Spec;
with_token(Token, Spec) -> Spec#{token => Token}.

await_call_reply(ok, RequestId, Request, From, Tmo, Pending, S) ->
    TRef = erlang:send_after(Tmo, self(), {call_timeout, RequestId}),
    {noreply, S#state{pending = Pending#{RequestId => {From, TRef, Request}}}};
await_call_reply({error, Reason}, _RequestId, _Request, _From, _Tmo, _Pending, S) ->
    {reply, {error, {refused, Reason}}, S}.

-spec send_publish_frame(<<_:256>>, binary(), term(), non_neg_integer(),
                         #state{}) -> ok | {error, term()}.
send_publish_frame(Realm, Topic, Payload, Seq,
                   #state{peer_pid = Pid, node_identity = Id}) ->
    Frame = macula_frame:publish(#{
        realm        => Realm,
        topic        => Topic,
        seq          => Seq,
        published_at => erlang:system_time(millisecond),
        payload      => Payload
    }, Id),
    macula_peering:send_frame(Pid, Frame).

after_connect_request({ok, Pid}, S) ->
    link(Pid),
    %% Arm the connect watchdog now: from here we are waiting for the
    %% peering worker's `connected' message. If it never arrives (dial
    %% NIF hangs, handshake stalls) the watchdog recycles the link.
    {noreply, arm_connect_watchdog(S#state{peer_pid = Pid})};
after_connect_request({error, Reason}, S) ->
    macula_diagnostics:event(<<"_macula.station_link.connect_failed">>, #{
        reason => macula_reason_name:text(Reason),
        seed   => S#state.seed
    }),
    erlang:send_after(S#state.connect_retry_backoff_ms, self(), attempt_connect),
    {noreply, S}.

%% A RESULT or provider ERROR, or a station's relay ERROR, on the control
%% stream. The request it answers is found by the ids it claims, and the
%% verifier against that request decides; only a verified answer completes
%% the pending call, or clears the probe, it names. Routes are one station
%% long, so a relay error on a link is reported by the station it is
%% connected to.
on_frame(#{frame_type := Type, reply := _} = Frame, S) when Type =:= result; Type =:= error ->
    on_claimed_reply(macula_frame:claimed_reply_ids(Frame), Frame, S);
on_frame(#{frame_type := error, relay_error := _} = Frame, S) ->
    on_claimed_reply(macula_frame:claimed_reply_ids(Frame), Frame, S);
%% EVENT — pubsub delivery. The publication the EVENT carries is
%% verified before fan-out, and its verified fields name the
%% (realm, topic) the event is delivered to. Stations may push EVENTs
%% without a prior SUBSCRIBE on this connection (e.g. wildcard /
%% catalog channels); silently drop those.
on_frame(#{frame_type := event} = Frame, S) ->
    on_inbound_event(macula_frame:verify_publication(Frame, S#state.profile,
                                                      erlang:system_time(millisecond)),
                     Frame, S);
%% Inbound CALL — a CALL the station delivered to this link, for a
%% (realm, procedure) with a registered handler. The request verifies
%% under the link's profile first; only a verified one reaches the
%% registered handler. Dispatch to the handler and ship the resulting
%% RESULT or provider ERROR back over the same peering connection.
on_frame(#{frame_type := call} = Frame, S) ->
    V = macula_frame:verify_request(Frame, S#state.profile),
    C = erlang:get(served_debug_count),
    C1 = case element(1, V) of ok -> (case C of undefined -> 0; N -> N end) + 1; _ -> case C of undefined -> 0; N -> N end end,
    erlang:put(served_debug_count, C1),

    io:format("T=~p ONFRAME ~p count=~p~n", [erlang:monotonic_time(microsecond), element(1, V), C1]),
    on_inbound_call(V, Frame, S);
%% STREAM_OPEN / STREAM_DATA / STREAM_END / STREAM_ERROR / STREAM_REPLY
%% no longer arrive here — every streaming session travels on its
%% own dedicated QUIC stream (see PLAN_PER_STREAM_QUIC_ISOLATION.md
%% and `dispatch_dedicated_frame/3'), not the shared control stream
%% `on_frame/2' decodes. A stream frame reaching this function is a
%% protocol violation and falls through to the catch-all below.
%%
%% SWIM and content-transfer frames have their own dedicated paths
%% (content: `dispatch_dedicated_frame/3'; SWIM: station-to-station,
%% never reaches a daemon-side client connection at all) and never
%% land here. HyParView / Plumtree frames DO land here — fan out to
%% whoever called `overlay_subscribe/3' for the frame's realm, if
%% anyone. A frame with no `realm' field, or no matching subscriber,
%% is dropped, same as every overlay frame was before this existed.
%%
%% `overlay_relay' is a relayed third-party frame: the station forwards a
%% frame from another node, and `Origin' is that node's identity as the
%% station authenticated it. It is delivered to the realm's overlay
%% subscribers with `Meta.sender' set to `Origin', not `peer_node_id' (the
%% station's own identity), and `Meta.via' set to the station. The frames
%% `macula_frame:relayed_without_signature/1' names are taken as they are,
%% and `Origin' is their sender whatever the frame itself names. A relayed
%% frame of any other type is dropped and counted as `not_overlay', never
%% verified and never delivered. The envelope reached this link through its
%% own connection, which in pq_hybrid checked the station's neighbour
%% signature on it first.
%% A payload that is not exactly one frame is dropped and counted
%% (`relayed_payload/3'), and the link carries on.
%% Must be matched before the bare `#{realm := Realm}' clause below,
%% since an `overlay_relay' envelope has no `realm' field of its own. So
%% must a GOSSIP, which names its realm only inside its publication
%% (`direct_gossip/3').
on_frame(#{frame_type := overlay_relay, peer := Origin, payload := Bytes}, S) ->
    relayed_payload(macula_frame:decode(Bytes), Origin, S);
on_frame(#{frame_type := plumtree_gossip} = Frame, S) ->
    direct_gossip(macula_frame:claimed_publication_realm(Frame), Frame, S);
on_frame(#{realm := Realm} = Frame, S) ->
    deliver_overlay_frame(Realm, Frame, S);
on_frame(_Frame, S) ->
    S.

on_claimed_reply({ok, #{request_id := RequestId}}, Frame, S) ->
    answered(held_request(RequestId, S), Frame, S);
on_claimed_reply({error, Refusal}, _Frame, S) ->
    refused_reply(Refusal, S).

%% The request a claimed request_id names: the outstanding probe's, a pending call's, or none the link holds.
held_request(RequestId, #state{liveness_outstanding = {RequestId, Request}}) ->
    {probe, Request};
held_request(RequestId, #state{pending = Pending}) ->
    pending_request(RequestId, maps:find(RequestId, Pending)).

pending_request(RequestId, {ok, {From, TRef, Request}}) ->
    {call, RequestId, From, TRef, Request};
pending_request(_RequestId, error) ->
    unknown_request.

answered({probe, Request}, Frame, S) ->
    probe_answered(verified_answer(Frame, Request, S), S);
answered({call, RequestId, From, TRef, Request}, Frame, S) ->
    call_answered(verified_answer(Frame, Request, S), RequestId, From, TRef, S);
answered(unknown_request, _Frame, S) ->
    refused_reply(unknown_request, S).

%% A provider's reply verifies against the request, with responded_by its target. A relay error verifies against the
%% request and must be reported by the station this link is connected to.
verified_answer(#{reply := _} = Frame, Request, #state{profile = Profile}) ->
    macula_frame:verify_reply(Frame, Request, Profile);
verified_answer(#{relay_error := _} = Frame, Request, #state{profile = Profile, peer_node_id = Station}) ->
    macula_frame:verify_relay_error(Frame, Request, Profile, Station).

probe_answered({ok, _Verified}, S) ->
    S#state{liveness_outstanding = undefined, liveness_misses = 0};
probe_answered({error, Refusal}, S) ->
    refused_reply(Refusal, S).

%% A verified answer completes the call; a refused one leaves it pending.
call_answered({ok, Fields}, RequestId, From, TRef, #state{pending = P} = S) ->
    _ = erlang:cancel_timer(TRef),
    gen_server:reply(From, call_result(Fields)),
    S#state{pending = maps:remove(RequestId, P)};
call_answered({error, Refusal}, _RequestId, _From, _TRef, S) ->
    refused_reply(Refusal, S).

%% What a verified answer means to its caller. A provider's code and detail
%% reach the caller as the binaries they arrived as, so nothing a provider
%% sends takes the shape of a reason the link or the pool builds itself. A
%% relay error names its code from a closed set.
call_result(#{frame_type := result, payload := Payload}) ->
    {ok, Payload};
call_result(#{frame_type := error, reported_by := _, code := Code}) ->
    {error, {call_error, Code, undefined}};
call_result(#{frame_type := error, code := ?HANDLER_ERROR_CODE, detail := Detail}) ->
    {error, Detail};
call_result(#{frame_type := error, code := Code} = Fields) ->
    {error, {call_error, Code, maps:get(detail, Fields, undefined)}}.

%% A refused reply changes nothing but its count, and the log hears of it at most once a window.
refused_reply(Refusal, #state{refused_replies = Report} = S) ->
    Refused = macula_refusal_report:refused(Report, Refusal, erlang:monotonic_time(millisecond)),
    S#state{refused_replies = logged_refused_reply(Refused, Refusal)}.

logged_refused_reply({report, Count, Report}, Refusal) ->
    logger:warning("[macula_station_link] refused ~b reply frame(s): ~p", [Count, Refusal]),
    Report;
logged_refused_reply({quiet, Report}, _Refusal) ->
    Report.

%% A CALL whose signature does not verify against its own `caller' never
%% reaches its handler and gets no reply.
on_inbound_call({ok, _Verified} = VerifiedRequest, _Frame, S) ->
    handle_inbound_call(VerifiedRequest, S),
    S;
on_inbound_call({error, Why}, _Frame, S) ->
    logger:warning("[macula_station_link] dropped inbound CALL whose request"
                   " does not verify (~p)", [Why]),
    S.

on_timeout(error, S) ->
    {noreply, S};
on_timeout({{From, _OldTRef, _Request}, NewP}, S) ->
    gen_server:reply(From, {error, timeout}),
    {noreply, S#state{pending = NewP}}.

%% -- Content-transfer dedicated streams (Phase 2) -------------------

open_content_stream_result({ok, Stream}, Bufs, S) ->
    {reply, {ok, Stream}, S#state{content_stream_bufs = Bufs#{Stream => <<>>}}};
open_content_stream_result({error, _} = E, _Bufs, S) ->
    {reply, E, S}.

send_on_content_stream(Stream, CallSpec, Id) ->
    try
        case macula_frame:stream_bytes({call, CallSpec}, Id) of
            {ok, Built} ->
                Bytes = macula_frame:written_bytes(Built),
                case macula_peering:send_on_stream(Stream, Bytes) of
                    ok -> {ok, content_request(CallSpec, Id)};
                    {error, _} = E -> E
                end;
            {error, _} = Refused -> Refused
        end
    catch C:R -> {error, {C, R}}
    end.

%% The request data a content reply verifies against: the id, the hash
%% of the request's signed tbs, and the target the reply must report.
content_request(CallSpec, Id) ->
    #{request := #{tbs := Tbs}} = macula_frame:call(CallSpec, Id),
    #{request_id   => maps:get(request_id, CallSpec),
      request_hash => crypto:hash(sha384, Tbs),
      target       => maps:get(target, CallSpec)}.

await_content_call_reply({ok, Request}, Stream, From, Tmo, Pending, S) ->
    TRef = erlang:send_after(Tmo, self(), {content_call_timeout, Stream}),
    {noreply, S#state{content_pending = Pending#{Stream => {From, TRef, Request}}}};
await_content_call_reply({error, _} = Refused, _Stream, _From, _Tmo, _Pending, S) ->
    {reply, Refused, S}.

on_content_timeout(error, S) ->
    {noreply, S};
on_content_timeout({{From, _OldTRef}, NewCP}, S) ->
    gen_server:reply(From, {error, timeout}),
    {noreply, S#state{content_pending = NewCP}}.

dispatch_content_frame(#{frame_type := Type} = Frame, Stream, S)
  when Type =:= result; Type =:= error ->
    verify_content_reply(Frame, Stream, S);
dispatch_content_frame(_Frame, _Stream, S) ->
    %% Anything else arriving on a content stream is a protocol
    %% violation — this side only ever sends CALL on one, so the only
    %% legitimate replies are RESULT/ERROR.
    S.

%% A reply on a content stream verifies against the request the stream
%% carries; one that does not ends the stream and fails the call
%% waiting on it, naming the refusal.
verify_content_reply(Frame, Stream, #state{content_pending = CP, profile = Profile} = S) ->
    case maps:find(Stream, CP) of
        {ok, {_From, _TRef, Request}} ->
            case macula_frame:verify_reply(Frame, Request, Profile) of
                {ok, Fields} ->
                    deliver_content_reply(Stream, content_reply_result(Fields), S);
                {error, Refusal} ->
                    teardown_content_stream_state(Stream, {error, Refusal},
                                                  fun macula_quic:close_stream/1, S)
            end;
        error ->
            S
    end.

content_reply_result(#{frame_type := result, payload := Payload}) ->
    {ok, Payload};
content_reply_result(#{frame_type := error, code := Code} = Fields) ->
    {error, {call_error, Code, maps:get(detail, Fields, undefined)}}.

%% What `macula_frame:parse_received/1' gave for a content stream, handled as
%% `dedicated_items/3' handles a dedicated stream's: a reply that does not
%% decode, or lacks a field it requires, ends the stream and fails the call
%% waiting on it.
content_items({ok, Items, Tail}, Stream, #state{content_stream_bufs = Bufs} = S) ->
    {_Open, NewS} = dispatch_content_items(Items, Stream,
                                           S#state{content_stream_bufs = Bufs#{Stream => Tail}}),
    NewS;
content_items({malformed, Items, Reason}, Stream, S) ->
    end_malformed_content(dispatch_content_items(Items, Stream, S), Stream, Reason).

dispatch_content_items([], _Stream, S) ->
    {open, S};
dispatch_content_items([{invalid_frame, _Type, _Field} = Invalid | _Rest], Stream, S) ->
    {ended, teardown_malformed_content(Stream, Invalid, S)};
dispatch_content_items([Frame | Rest], Stream, S) ->
    dispatch_content_items(Rest, Stream, dispatch_content_frame(Frame, Stream, S)).

end_malformed_content({open, S}, Stream, Reason) ->
    teardown_malformed_content(Stream, Reason, S);
end_malformed_content({ended, S}, _Stream, _Reason) ->
    S.

teardown_malformed_content(Stream, Reason, S) ->
    teardown_content_stream_state(Stream, {error, {malformed, Reason}},
                                  fun macula_quic:close_stream/1, S).

deliver_content_reply(Stream, Reply, #state{content_pending = CP} = S) ->
    reply_content_pending(maps:take(Stream, CP), Reply, S).

reply_content_pending(error, _Reply, S) ->
    %% No caller waiting (race with timeout, or a stray reply after
    %% `close_content_stream/2' already failed it).
    S;
reply_content_pending({{From, TRef, _Request}, NewCP}, Reply, S) ->
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
    try CloseFun(Stream) catch _:_ -> ok end,
    S#state{content_pending = NewCP,
            content_stream_bufs = maps:remove(Stream, Bufs)}.

fail_content_pending(error, CP, _Reason) ->
    CP;
fail_content_pending({{From, TRef, _Request}, NewCP}, _CP, Reason) ->
    _ = erlang:cancel_timer(TRef),
    gen_server:reply(From, Reason),
    NewCP.

fail_all_pending(Reason, #state{pending = P, subscriptions = Subs,
                                overlay_subscriptions = OverlaySubs,
                                client_streams = CS,
                                server_streams = SS,
                                content_pending = ContentP} = S) ->
    maps:foreach(fun(_RequestId, {From, TRef, _Request}) ->
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
    maps:foreach(fun(SubRef, {_Realm, Subscriber, Mon}) ->
        erlang:demonitor(Mon, [flush]),
        Subscriber ! {macula_overlay_gone, SubRef, Reason}
    end, OverlaySubs),
    %% Abort every open stream with a `disconnected' STREAM_ERROR, and
    %% close its dedicated QUIC stream — the peering connection this
    %% stream belonged to is already gone or going, but the stream
    %% resource itself is independent and won't be reclaimed on its own.
    %% Caller waiters (recv / await_reply) unblock immediately;
    %% transient handler processes see the abort and exit.
    AbortFun = fun(_Sid, {Pid, Mon, Stream}) ->
        erlang:demonitor(Mon, [flush]),
        close_dedicated_stream(Stream, S),
        abort_stream_process(Pid, Reason)
    end,
    maps:foreach(AbortFun, CS),
    maps:foreach(AbortFun, SS),
    %% Content streams have no paired process to abort — just reclaim
    %% the QUIC resource, same as `close_content_stream_state/2' does
    %% on a normal close.
    maps:foreach(fun(Stream, _Buf) -> close_dedicated_stream(Stream, S) end,
                S#state.content_stream_bufs),
    %% A stream the peer opened that brought no whole frame yet carries no
    %% session to abort either.
    maps:foreach(fun(Stream, _Buf) -> close_dedicated_stream(Stream, S) end,
                S#state.opening_bufs),
    S#state{pending = #{}, subscriptions = #{}, topic_index = #{},
            overlay_subscriptions = #{}, overlay_realm_index = #{},
            client_streams = #{}, server_streams = #{}, stream_bufs = #{},
            opening_bufs = #{}, content_pending = #{}, content_stream_bufs = #{}}.

abort_stream_process(Pid, Reason) ->
    try
        macula_stream:abort(Pid, <<"disconnected">>, macula_reason_name:text(Reason))
    catch _:_ -> ok end.

%%-------------------------------------------------------------------
%% Liveness probe — bounded zombie-connection detection
%%-------------------------------------------------------------------
%% On handshake-complete we arm a periodic tick. Each tick:
%%   1. If a prior probe is still outstanding (no reply received in the
%%      interval), increment misses. If misses >= MAX, close peer_pid
%%      via macula_peering — emits `disconnected', station_link stops,
%%      pool respawns.
%%   2. Otherwise (or after counting the miss), send a fresh probe
%%      (CALL with procedure `_macula.ping' on the DHT realm), and keep
%%      its request. The station answers with a RESULT, or with a relay
%%      ERROR (`unknown_next_peer') when it has no such handler. Either
%%      clears the outstanding slot once it verifies against the probe's
%%      request (`on_claimed_reply/3'); any other reply is counted in
%%      `refused_replies' and clears nothing.
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
on_outstanding_check(_Probe, S) ->
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

%% The probe is a request to the station the link is connected to, signed with the node identity key. The link keeps its
%% request_id and the request as a verifier reads it, which a reply to the probe is checked against.
send_probe(#state{peer_pid = Pid, peer_node_id = Station, node_identity = Key, profile = Profile} = S)
  when is_pid(Pid) ->
    RequestId = crypto:strong_rand_bytes(16),
    Probe = macula_frame:call(#{request_id => RequestId, realm => ?DHT_REALM, procedure => ?LIVENESS_PROCEDURE,
                                target => Station,
                                deadline => erlang:system_time(millisecond) + S#state.liveness_interval_ms,
                                payload => #{}}, Key),
    {ok, Request} = macula_frame:verify_request(Probe, Profile),
    try macula_peering:send_frame(Pid, Probe) catch _:_ -> ok end,
    S#state{liveness_outstanding = {RequestId, Request}};
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
                              node_identity = Id} = S) ->
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
    SubKey = node_id(Id),
    Frame  = macula_frame:unsubscribe(#{topic      => Topic,
                                        realm      => Realm,
                                        subscriber => SubKey}),
    try macula_peering:send_frame(Pid, Frame) catch _:_ -> ok end,
    ok.

%% Send a SUBSCRIBE frame for `(Realm, Topic)' iff peering is connected.
%% Must gate on `peer_node_id' (set only once the CONNECT/HELLO handshake
%% completes), not `peer_pid' (set the moment `macula_peering:connect/1'
%% returns, before handshaking finishes) -- matches `is_connected/1'.
%% Gating on `peer_pid' alone let a
%% SUBSCRIBE frame through mid-handshake, where the peering statem has no
%% clause for `cast({send_frame, _})' and silently drops it via
%% `drop_unexpected' (logged as `_macula.peering.unexpected_event') --
%% verified live: every occurrence in a real deployment's logs landed in
%% the few-hundred-ms window right after a `_macula.client.link_down'
%% reconnect. Harmless for a *stored* subscription, since
%% `drain_pending_subscribes/1' resends it once `connected' genuinely
%% fires, but wasteful and alarming on every reconnect, and NOT harmless
%% for a caller that assumed the frame had actually gone out now.
maybe_send_subscribe(_Realm, _Topic, #state{peer_pid = undefined}) ->
    ok;
maybe_send_subscribe(_Realm, _Topic, #state{peer_node_id = undefined}) ->
    ok;
maybe_send_subscribe(Realm, Topic, #state{peer_pid = Pid, node_identity = Id}) ->
    SubKey = node_id(Id),
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
%% scan defensively. Also sweeps overlay-frame subscriptions sharing
%% the same monitor namespace — a pid that called both `subscribe/4'
%% and `overlay_subscribe/3' gets torn down on both sides by one DOWN.
on_subscriber_down(Mon, #state{subscriptions = Subs} = S) ->
    Found = maps:fold(fun
        (SubRef, {_R, _T, _P, M}, Acc) when M =:= Mon -> [SubRef | Acc];
        (_, _, Acc) -> Acc
    end, [], Subs),
    S1 = lists:foldl(fun on_unsubscribe/2, S, Found),
    on_overlay_subscriber_down(Mon, S1).

on_overlay_subscriber_down(Mon, #state{overlay_subscriptions = Subs} = S) ->
    Found = maps:fold(fun
        (SubRef, {_R, _P, M}, Acc) when M =:= Mon -> [SubRef | Acc];
        (_, _, Acc) -> Acc
    end, [], Subs),
    lists:foldl(fun on_overlay_unsubscribe/2, S, Found).

%%-------------------------------------------------------------------
%% Overlay-frame subscription helpers (mirrors the topic-subscription
%% helpers above; no wire-level SUBSCRIBE/UNSUBSCRIBE, see
%% `overlay_subscribe/3''s own moduledoc for why).
%%-------------------------------------------------------------------

add_realm_sub(Realm, SubRef, Idx) ->
    Set = maps:get(Realm, Idx, sets:new()),
    Idx#{Realm => sets:add_element(SubRef, Set)}.

del_realm_sub(Realm, SubRef, Idx) ->
    on_realm_set_after_del(Realm, sets:del_element(SubRef, maps:get(Realm, Idx, sets:new())), Idx).

on_realm_set_after_del(Realm, Set, Idx) ->
    on_realm_empty_set(sets:is_empty(Set), Realm, Set, Idx).

on_realm_empty_set(true,  Realm, _Set, Idx) -> maps:remove(Realm, Idx);
on_realm_empty_set(false, Realm,  Set, Idx) -> Idx#{Realm => Set}.

%% Drop a single overlay-frame subscription. Idempotent: unknown
%% SubRef is a no-op.
on_overlay_unsubscribe(SubRef, #state{overlay_subscriptions = Subs,
                                      overlay_realm_index   = Idx} = S) ->
    on_overlay_unsubscribe_take(maps:take(SubRef, Subs), SubRef, Idx, S).

on_overlay_unsubscribe_take(error, _SubRef, _Idx, S) ->
    S;
on_overlay_unsubscribe_take({{Realm, _Subscriber, Mon}, NewSubs}, SubRef, Idx, S) ->
    erlang:demonitor(Mon, [flush]),
    NewIdx = del_realm_sub(Realm, SubRef, Idx),
    S#state{overlay_subscriptions = NewSubs, overlay_realm_index = NewIdx}.

%% Fan an overlay frame out to every subscriber for its realm. Sender is
%% this connection's own peer identity — correct for a frame that
%% genuinely arrived directly from the connected peer.
deliver_overlay_frame(Realm, Frame, #state{overlay_realm_index = Idx,
                                           peer_node_id        = PeerNodeId} = S) ->
    deliver_overlay_frame_to(maps:find(Realm, Idx), Frame, #{sender => PeerNodeId}, S),
    S.

%% A GOSSIP from the connected peer goes to the subscribers of the realm
%% its publication claims, read without verifying: they verify it. The
%% peer sent it, so what the link refuses goes to its connection: a
%% publication that names no realm as malformed_frame, which is charged,
%% and a realm with no subscriber on this link as no_subscriber, which is
%% not.
direct_gossip({ok, Realm}, Frame, #state{overlay_realm_index = Idx} = S) ->
    direct_gossip_delivered(maps:find(Realm, Idx), Frame, S);
direct_gossip({error, no_realm}, _Frame, S) ->
    connection_refused(malformed_frame, S).

direct_gossip_delivered({ok, _Set} = Found, Frame, #state{peer_node_id = PeerNodeId} = S) ->
    deliver_overlay_frame_to(Found, Frame, #{sender => PeerNodeId}, S),
    S;
direct_gossip_delivered(error, _Frame, S) ->
    connection_refused(no_subscriber, S).

%% A relayed third-party frame arrived wrapped in an `overlay_relay'
%% envelope. `Origin' is the envelope's own `peer' field (the
%% station-authenticated origin of the ORIGINAL frame), never this
%% connection's own `peer_node_id', which is the station's identity and
%% goes in `via'. The frame goes to the subscribers of its realm: its own
%% `realm' field, or for a GOSSIP the realm its publication claims, read
%% without verifying. A frame that names no realm is counted as no_realm,
%% and one for a realm with no subscriber on this link as no_subscriber, so
%% a relayed frame taken is always delivered or counted.
relayed_routed({ok, Realm}, Frame, Origin, #state{overlay_realm_index = Idx} = S) ->
    relayed_delivered(maps:find(Realm, Idx), Frame, Origin, S);
relayed_routed({error, no_realm}, _Frame, Origin, S) ->
    refused_relay(no_realm, Origin, S).

relayed_delivered({ok, _Set} = Found, Frame, Origin, #state{peer_node_id = Station} = S) ->
    deliver_overlay_frame_to(Found, Frame, #{sender => Origin, via => Station}, S),
    S;
relayed_delivered(error, _Frame, Origin, S) ->
    refused_relay(no_subscriber, Origin, S).

%% The realm an overlay frame is routed by.
overlay_realm(#{frame_type := plumtree_gossip} = Frame) -> macula_frame:claimed_publication_realm(Frame);
overlay_realm(#{realm := Realm}) -> {ok, Realm};
overlay_realm(_Frame) -> {error, no_realm}.

%% A relayed payload is taken only when it is exactly one frame. A payload
%% shorter than its length header, one that does not decode, and one with
%% bytes after its frame are dropped and counted by kind.
relayed_payload({ok, #{frame_type := Type} = Inner, <<>>}, Origin, S) ->
    on_relayed_overlay_frame(relayed(macula_frame:relayed_without_signature(Type), Inner), Origin, S);
relayed_payload({ok, _Inner, _Trailing}, Origin, S) ->
    refused_relay(trailing_bytes, Origin, S);
relayed_payload({more, _Needed}, Origin, S) ->
    refused_relay(truncated, Origin, S);
relayed_payload({error, Why}, Origin, S) ->
    refused_relay(decode_refusal(Why), Origin, S).

%% The kind a payload that does not decode is counted under: one of a fixed
%% set, never a term of the payload. An error decode/1 comes to name later
%% counts as bad_frame, so no relayed payload takes the link down.
decode_refusal({invalid_frame, _Type, _Field}) -> invalid_frame;
decode_refusal(Kind) when Kind =:= frame_too_large; Kind =:= too_many_elements -> Kind;
decode_refusal(_BadFrame) -> bad_frame.

%% A relayed frame of a type `macula_frame:relayed_without_signature/1'
%% names is taken as it is; a frame of any other type is not taken.
relayed(true, Inner) -> {ok, Inner};
relayed(false, _Inner) -> not_overlay.

%% A relayed frame taken is routed by its realm, with `Origin' as its
%% sender (`relayed_routed/4'). A frame of any other type is dropped and
%% counted as `not_overlay'.
on_relayed_overlay_frame({ok, Inner}, Origin, S) ->
    relayed_routed(overlay_realm(Inner), Inner, Origin, S);
on_relayed_overlay_frame(not_overlay, Origin, S) ->
    refused_relay(not_overlay, Origin, S).

%% A dropped relayed frame changes nothing but its count, and the log hears of
%% it at most once a window per kind, with the origin of the frame that reports.
refused_relay(Kind, Origin, #state{refused_relays = Report} = S) ->
    Refused = macula_refusal_report:refused(Report, Kind, erlang:monotonic_time(millisecond)),
    S#state{refused_relays = logged_refused_relay(Refused, Kind, Origin)}.

logged_refused_relay({report, Count, Report}, Kind, Origin) ->
    logger:warning("[macula_station_link] dropped ~b relayed overlay frame(s): ~p origin=~s",
                   [Count, Kind, hex_prefix(Origin)]),
    Report;
logged_refused_relay({quiet, Report}, _Kind, _Origin) ->
    Report.

%% A consumer's report of a refused overlay frame (`overlay_frame_refused/3')
%% reaches the connection only for a kind `macula_frame:charged_refusal/1'
%% classifies, and only for a frame that provably came from the link's
%% current peer. Every other report is counted on the link, under the kind's
%% name, or as unknown_refusal for a kind no rule classifies.
overlay_refusal(unclassified, Meta, _Kind, S) ->
    refused_relay(unknown_refusal, reported_sender(Meta), S);
overlay_refusal(_Classified, Meta, Kind, S) ->
    reported_refusal(from_current_peer(Meta, S), Meta, Kind, S).

reported_refusal(true, _Meta, Kind, S) ->
    connection_refused(Kind, S);
reported_refusal(false, Meta, Kind, S) ->
    refused_relay(refusal_name(Kind), reported_sender(Meta), S).

%% A frame provably came from the current peer when its Meta has no via and
%% names that peer's node_id as sender, while the link has a connection. A
%% link still connecting has no peer node_id yet, so no report charges it.
from_current_peer(#{sender := Peer} = Meta, #state{peer_pid = Conn, peer_node_id = Peer}) when is_binary(Peer) ->
    is_pid(Conn) andalso not is_map_key(via, Meta);
from_current_peer(_Meta, _S) ->
    false.

refusal_name({Name, _Amount}) -> Name;
refusal_name(Name) -> Name.

reported_sender(#{sender := Sender}) -> Sender;
reported_sender(_Meta) -> undefined.

%% A refusal of a frame from the connected peer goes to its connection,
%% which counts it and charges it as `macula_frame:charged_refusal/1' says.
connection_refused(Kind, #state{peer_pid = Conn} = S) when is_pid(Conn) ->
    ok = macula_peering:object_refused(Conn, Kind),
    S;
connection_refused(_Kind, S) ->
    S.

deliver_overlay_frame_to(error, _Frame, _Meta, _S) ->
    ok;
deliver_overlay_frame_to({ok, Set}, Frame, Meta,
                         #state{overlay_subscriptions = Subs}) ->
    sets:fold(fun(SubRef, _) ->
        fan_overlay_frame(maps:find(SubRef, Subs), SubRef, Frame, Meta)
    end, ok, Set).

fan_overlay_frame(error, _SubRef, _Frame, _Meta) ->
    ok;
fan_overlay_frame({ok, {_Realm, Subscriber, _Mon}}, SubRef, Frame, Meta) ->
    Subscriber ! {macula_overlay_frame, SubRef, Frame, Meta},
    ok.

%% Pubsub — verify the publication an EVENT carries, under the link's
%% profile, before anything is delivered. A publication that verifies
%% delivers with `publisher_verified => true'. One that does not is
%% dropped (the strict default), or, when
%% `pubsub_strict_publisher_sig' is explicitly `false', delivered with
%% `publisher_verified => false', its fields read from the publication
%% without trusting the signature. An EVENT without a publication has
%% no fields to deliver and is dropped either way.
on_inbound_event({ok, Verified}, Frame, S) ->
    deliver_event(Verified, Frame, true, S);
on_inbound_event({error, Why}, Frame, S) ->
    logger:warning("[macula_pubsub] inbound EVENT publication invalid (~p)",
                   [Why]),
    on_invalid_publication(
      application:get_env(macula, pubsub_strict_publisher_sig, true),
      Frame, S).

on_invalid_publication(true, _Frame, S) ->
    S;
on_invalid_publication(_Lenient, Frame, S) ->
    on_claimed_publication(macula_frame:claimed_publication(Frame), Frame, S).

on_claimed_publication({ok, Fields}, Frame, S) ->
    deliver_event(Fields, Frame, false, S);
on_claimed_publication({error, _}, _Frame, S) ->
    S.

hex_prefix(B) when is_binary(B), byte_size(B) >= 4 ->
    binary:encode_hex(binary:part(B, 0, 4));
hex_prefix(B) when is_binary(B) ->
    binary:encode_hex(B);
hex_prefix(_) ->
    <<"?">>.

%% The publication's hash, for the pool's dedup: the verified
%% publication carries it; a claimed one (lenient mode) recomputes it
%% from the tbs the signature covers.
publication_hash_of(#{publication_hash := Hash}, _Frame) ->
    Hash;
publication_hash_of(_Fields, #{publication := #{tbs := Tbs}}) ->
    crypto:hash(sha384, Tbs).

%% The last moment a verifier accepts the publication: its published_at
%% plus ttl (10 minutes without one) plus 5 minutes, as verify says.
publication_expiry(#{expires_at := Expiry}) ->
    Expiry;
publication_expiry(#{published_at := PublishedAt} = Fields) ->
    Ttl = maps:get(ttl_ms, Fields, 600_000),
    PublishedAt + Ttl + 300_000.

%% Fan an EVENT out to every subscriber for its (realm, topic). The
%% fields come from the verified (or claimed) publication; the frame
%% contributes only `delivered_via'. `PublisherVerified' is
%% `on_inbound_event/3''s already-computed outcome (`true' | `false').
deliver_event(Fields, Frame, PublisherVerified, #state{topic_index = Idx} = S) ->
    Realm = maps:get(realm, Fields),
    Topic = maps:get(topic, Fields),
    deliver_event_to(maps:find({Realm, Topic}, Idx), Fields, Frame,
                      PublisherVerified, S),
    S.

deliver_event_to(error, _Fields, _Frame, _PublisherVerified, _S) ->
    ok;
deliver_event_to({ok, Set}, Fields, Frame, PublisherVerified,
                 #state{subscriptions = Subs}) ->
    Topic = maps:get(topic, Fields),
    Payload = maps:get(payload, Fields),
    Meta = #{realm              => maps:get(realm, Fields),
             publisher          => maps:get(publisher, Fields),
             publisher_verified => PublisherVerified,
             seq                => maps:get(seq, Fields),
             delivered_via      => maps:get(delivered_via, Frame, direct),
             publication_hash   => publication_hash_of(Fields, Frame),
             expires_at         => publication_expiry(Fields)},
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

%% Inbound CALL — relay forwarded a CALL whose `(realm, procedure)'
%% this link has advertised. Authorise and look up the handler here,
%% where the state lives, then run the handler in a process of its
%% own and ship the resulting RESULT or call_error frame back over
%% the same peering connection from there.
%%
%% The handler must not run in this process. This link is the only
%% reader of its peering connection: while it waits on a handler it
%% cannot read the RESULT of any call that handler makes through the
%% pool over this same link, nor answer the pool's advertise and
%% publish calls (5 s), so a handler that touches the mesh at all
%% deadlocks against itself until its own timeout fires. Found live
%% 2026-09-02 on hecate-rag: every semantic search waited 30 s on its
%% embedder call and crashed, and the link's advertise republishes
%% timed out meanwhile, so the service flickered out of the station's
%% registry. The worker is a plain spawn, matching
%% `spawn_stream_handler/4': `safe_invoke_handler/4' already turns a
%% handler crash into a call_error frame, and
%% `macula_peering:send_frame/2' is a cast, so the worker needs no
%% link to this process and a peer gone by reply time is harmless.
%%
%% A handler crash maps to BOLT#4 `temporary_relay_failure' (0x02);
%% an unknown `(realm, procedure)' (no handler registered on this
%% link) maps to `unknown_next_peer'
%% (0x01) — same taxonomy as `hecate_handler_dispatch'.
handle_inbound_call({ok, #{request_id := _CallId, procedure := Proc, realm := Realm,
                            payload := Payload} = Request},
                    #state{procedures = Procs, policies = Pols, node_identity = Id,
                           peer_pid = Pid}) when is_pid(Pid) ->
    %% Gate first (Slice 7b): an `open' procedure serves any identified
    %% caller; a gated one requires a valid `token', else refuse
    %% with BOLT#4 `unauthorized' instead of invoking the handler.
    Verdict = authorize({Realm, Proc}, Request, Pols),
    Found   = maps:find({Realm, Proc}, Procs),
    PayloadWithCaller = with_caller(Payload, maps:get(caller, Request, undefined)),
    _ = spawn(fun() ->
            Reply = try authorized_reply(Verdict, Found, Request,
                                         PayloadWithCaller, Id)
                    catch
                        error:Reason ->
                            macula_frame:provider_error(
                              #{request => Request, code => fault_code(Reason)}, Id)
                    end,
            sent_or_faulted(macula_peering:send_frame(Pid, Reply),
                            Pid, Request, Id)
        end),
    ok;
handle_inbound_call(_VerifiedRequest, _State) ->
    ok.

%% The CALL frame carries `caller' (a required, wire-authenticated field,
%% see `macula_frame''s CALL spec) but no application handler ever saw
%% it: `handle_request/2''s contract is fixed at 2-arity across every
%% existing provider (`macula_response', and every hecate-om desk built
%% on it), so threading it as a new function argument would be a
%% breaking change to all of them. Merging it into `Payload' instead
%% needs no arity change anywhere downstream — a handler that wants
%% provenance reads `caller' the same way it reads any other field
%% (`hecate_om_wire:field/2,3'); one that doesn't, ignores an extra map
%% key exactly as it already ignores fields it doesn't ask for.
%%
%% The merge happens here, not earlier, specifically so it happens AFTER
%% the payload has been fully decoded from whatever the remote peer
%% actually sent — `Payload#{caller => Caller}' deterministically
%% overwrites any `caller' key a caller's own payload might have
%% supplied, so the field a handler reads is always the wire-
%% authenticated identity, never a value the caller could spoof by
%% naming their own field the same thing.
with_caller(Payload, Caller) when is_map(Payload), Caller =/= undefined ->
    Payload#{caller => Caller};
with_caller(Payload, _Caller) ->
    Payload.

authorized_reply(ok, Found, Request, Payload, Key) ->
    build_inbound_call_reply(Found, Request, Payload, Key);
authorized_reply(unauthorized, _Found, Request, _Payload, Key) ->
    macula_frame:provider_error(#{request => Request, code => <<"unauthorized">>},
                                Key).

authorize(Key, Frame, Pols) ->
    authorize_policy(maps:get(Key, Pols, open), Frame).

authorize_policy(open, _Frame) ->
    ok;
authorize_policy({ucan_required, Issuer}, Frame) ->
    check_ucan(maps:get(token, Frame, <<>>), Issuer,
               maps:get(caller, Frame, undefined));
authorize_policy({realm_member_required, RealmDid, RequiredCan}, Frame) ->
    check_realm_membership(maps:get(token, Frame, <<>>), RealmDid,
                            maps:get(caller, Frame, undefined), RequiredCan).

%% `macula_ucan_nif:verify/2' checks signature + `exp' + `nbf' only. It does
%% NOT check `aud' (see that function's own doc): a verified token proves
%% its issuer granted it to SOMEONE, not that it belongs to whoever is
%% presenting it now. Both gated policies therefore also require the
%% token's audience to be the caller, through `audience_is_caller/2'.
%% Without that, any token a caller obtained a copy of -- not necessarily
%% its own -- would authorize as if it were the caller it was minted for.
%% `Caller' is the verified request's `caller', the key id of the key its
%% signature verified under, so by the time these checks run `Caller' is the
%% identity that signed the request. The token is the request's `token'.
check_ucan(Token, Issuer, Caller)
  when is_binary(Token), Token =/= <<>>, is_binary(Caller) ->
    ucan_verdict(macula_ucan_nif:verify(Token, Issuer), Caller);
check_ucan(_Token, _Issuer, _Caller) ->
    unauthorized.

ucan_verdict({ok, Payload}, Caller) ->
    audience_verdict(audience_is_caller(Payload, Caller));
ucan_verdict(_Error, _Caller) ->
    unauthorized.

audience_verdict(true)  -> ok;
audience_verdict(false) -> unauthorized.

%% `RealmDid' is a realm's own DID (a real Ed25519 keypair the realm
%% holds), never the 32-byte `RealmId' routing/scoping hash used in
%% `-realm' flags and DHT scoping elsewhere -- the two are unrelated
%% values, and nobody could verify a signature against a hash. A service
%% already has its realm's DID with no new plumbing: it reads `iss' off
%% its own realm-issued service credential (`macula_ucan_nif:get_issuer/1'
%% on whatever `hecate_om:service_cert/0' or equivalent already returns).
%%
%% A verified token signed by `RealmDid' is a genuine grant from this
%% realm; its audience is bound to the caller exactly as for
%% `ucan_required' (see `check_ucan/3').
%%
%% Signature and audience are still not enough on their own: a realm
%% mints membership UCANs at more than one tier from the same key (see
%% `auth_policy()' in `macula_client' for why), so `RequiredCan' is
%% checked against the verified token's own `cap' list too -- a device
%% that self-enrolled at a weaker tier presents a token that is entirely
%% genuine and entirely correctly-audienced, and must still be refused if
%% it doesn't carry the capability this procedure actually requires.
check_realm_membership(Token, RealmDid, Caller, RequiredCan)
  when is_binary(Token), Token =/= <<>>, is_binary(Caller) ->
    membership_verdict(macula_ucan_nif:verify(Token, RealmDid), Caller,
                        RequiredCan);
check_realm_membership(_Token, _RealmDid, _Caller, _RequiredCan) ->
    unauthorized.

membership_verdict({ok, Payload}, Caller, RequiredCan) ->
    grant_verdict(audience_is_caller(Payload, Caller),
                  has_required_capability(maps:get(<<"cap">>, Payload, []),
                                           RequiredCan));
membership_verdict(_Result, _Caller, _RequiredCan) ->
    unauthorized.

%% The one audience check both gated policies share. A token names its
%% audience as the caller's key id, hex-encoded in lowercase; `Caller' is
%% that key id, so hex-encoding it the same way makes the two directly
%% comparable. A token without a binary `aud' has no audience to match.
audience_is_caller(#{<<"aud">> := Aud}, Caller) when is_binary(Aud) ->
    Aud =:= binary:encode_hex(Caller, lowercase);
audience_is_caller(_Payload, _Caller) ->
    false.

grant_verdict(true, true) -> ok;
grant_verdict(_AudienceOk, _CapabilityOk) -> unauthorized.

%% A membership UCAN's `cap' list entries are `#{<<"with">> := _,
%% <<"can">> := _}' maps (macula-realm's own minting shape, decoded
%% straight off the verified JWT payload). Only `can' is checked here --
%% this policy gates on TIER (which admission path minted the token), not
%% on any particular MRI scope, so `with' is left alone.
has_required_capability(Caps, RequiredCan) when is_list(Caps) ->
    lists:any(fun(#{<<"can">> := Can}) -> Can =:= RequiredCan;
                 (_Other) -> false
              end, Caps);
has_required_capability(_Caps, _RequiredCan) ->
    false.

%% `open' is the default, so store it as absence to keep the map small.
set_policy(Key, open, Pols)   -> maps:remove(Key, Pols);
set_policy(Key, Policy, Pols) -> Pols#{Key => Policy}.

%% A RESULT the wire refuses must not simply vanish. Dropping it leaves
%% the remote caller burning its entire deadline waiting for a frame
%% that died here, which is a timeout where a taxonomy was available:
%% the handler's return value was the problem and BOLT#4 can say so.
sent_or_faulted(ok, _Pid, _Request, _Key) ->
    ok;
sent_or_faulted({error, Reason}, Pid, Request, Key) ->
    logger:error("[macula_station_link] handler result unsendable, "
                 "faulting the call: ~ts", [macula_frame:explain(Reason)]),
    _ = macula_peering:send_frame(
          Pid, macula_frame:provider_error(#{request => Request,
                                             code    => fault_code(Reason)}, Key)),
    ok.

fault_code({unsupported_payload_type, payload_too_large, _Path}) -> <<"payload_too_large">>;
fault_code(_Other)                                               -> <<"unknown_error">>.

%% Handler not registered locally — synthesise a signed
%% `unknown_next_peer' BOLT#4 error.
build_inbound_call_reply(error, Request, _Payload, Key) ->
    macula_frame:provider_error(#{request => Request, code => <<"unknown_next_peer">>},
                                Key);
build_inbound_call_reply({ok, Handler}, Request, Payload, Key) ->
    safe_invoke_handler(Handler, Payload, Request, Key).

%% Handler dispatch with crash trap and error-return funnel.
%%
%% Two failure paths reach the wire as a BOLT#4 provider ERROR frame
%% so the caller observes a reliable taxonomy rather than either
%%
%%   * a `{disconnected, killed}' signal when a single bad CALL
%%     takes the link down, or
%%   * a successful-looking RESULT frame whose payload was an
%%     `{error, _}' tuple — the CBOR encoder has no clause for raw
%%     tuples and crashes the frame build, dropping every other
%%     multiplexed RPC on the same connection.
%%
%% Mapping:
%%   * handler returns `{error, Reason}' →
%%     `provider_error(code = <<"unknown_error">>,
%%                     detail = handler_error_detail(Reason))'
%%   * handler crashes →
%%     `provider_error(code = <<"temporary_relay_failure">>)'
%%   * handler returns anything else →
%%     `result(payload = normalise_reply(Reply))'
safe_invoke_handler(Handler, Payload, Request, Key) ->
    try invoke_handler(Handler, Payload) of
        {error, Reason} ->
            macula_frame:provider_error(#{request => Request,
                                          code    => <<"unknown_error">>,
                                          detail  => handler_error_detail(Reason)},
                                        Key);
        Reply ->
            macula_frame:result(#{request => Request,
                                  payload => normalise_reply(Reply)}, Key)
    catch
        Class:Reason:Stack ->
            logger:warning(
              "[station_link] handler crashed: ~ts",
              [macula_reason_name:logged("~p:~p~n  stack=~p", [Class, Reason, Stack])]),
            macula_frame:provider_error(#{request => Request,
                                          code    => <<"temporary_relay_failure">>},
                                        Key)
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
%% A reason that is ALREADY text, a binary or a printable Unicode
%% charlist, crosses as that text, so a handler answering
%% `{error, <<"hold_full">>}' or `{error, "hold_full"}' gives its caller
%% `{error, <<"hold_full">>}' and the caller can match on it. Before
%% 8.0.0 every reason went through `~0p' and that same handler produced
%% `<<"<<\"hold_full\">>">>', a rendering of a binary rather than the
%% binary, which no caller could sensibly compare against.
%%
%% Any other reason crosses as its name, such as `refused' for
%% `{refused, Why}', and a reason with no name crosses as no detail at
%% all; see `macula_reason_name'. None of a reason's terms leave the
%% node, so a handler that wants its caller to know more than a name
%% says it in text.
%%
%% Text is capped at 256 bytes of valid UTF-8, cut on a character
%% boundary, to keep CALL_ERROR frames bounded. A reason long enough to
%% be cut is a reason nobody can match on, which is one more argument
%% for short ones.
handler_error_detail(Reason) ->
    detail_or_none(macula_reason_name:reply_text(Reason)).

detail_or_none({ok, Text}) -> Text;
detail_or_none(error) -> undefined.

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
%%-------------------------------------------------------------------
%% Helpers
%%-------------------------------------------------------------------

%% Fold TLS-policy opts (`verify' / `expected_node_id' / `pin_tls_cert')
%% from the link opts into the seed map, so they reach the peering
%% target at connect.
%% The trust keys a seed map names stand, and the link's options fill only the ones it leaves out, so a pool-wide
%% expected_node_id never replaces a seed's own pin.
add_tls_opts(Seed, Opts) ->
    maps:merge(maps:with([verify, expected_node_id, pin_tls_cert], Opts), Seed).

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

%% A client session's STREAM_OPEN is built, signed and encoded before anything
%% starts: a build the frame refuses, or an open longer than the limit a
%% provider reads a stream's first frame by, is refused here, so the caller
%% learns at once and no stream process or dedicated stream exists for it.
%% Otherwise the session's stream starts with the link's key as a closure, the
%% open as its provider verifies it, the peering connection and the profile,
%% under the request id as its attach id, and the open goes out as the first
%% bytes on a dedicated stream of its own. The returned pid is bound to the
%% requested `owner' (default: the caller), so a crashing owner ends it.
open_client_stream(Target, Realm, Proc, Args, Opts, Caller, #state{node_identity = Key} = S) ->
    Spec = maps:merge(#{request_id => crypto:strong_rand_bytes(16), realm => Realm, procedure => Proc,
                        target => target_node_id(Target, S),
                        deadline => maps:get(deadline_ms, Opts, erlang:system_time(millisecond) + 30_000),
                        payload => Args, mode => maps:get(mode, Opts, server_stream)},
                      open_token(maps:get(ucan_token, Opts, <<>>))),
    open_built(macula_frame:stream_bytes({stream_open, Spec}, Key), Opts, Caller, S).

%% An absent or empty token sends none.
open_token(<<>>) -> #{};
open_token(Token) -> #{token => Token}.

open_built({error, Refusal}, _Opts, _Caller, S) ->
    {reply_value, {error, {refused, Refusal}}, S};
open_built({ok, Built}, Opts, Caller, S) ->
    Bytes = macula_frame:written_bytes(Built),
    open_within_limit(byte_size(Bytes) - 4 =< stream_open_limit(), Bytes, Opts, Caller, S).

open_within_limit(false, _Bytes, _Opts, _Caller, S) ->
    {reply_value, {error, {open_too_large, stream_open_limit()}}, S};
open_within_limit(true, Bytes, Opts, Caller, #state{profile = Profile} = S) ->
    {ok, Frame, <<>>} = macula_frame:decode(Bytes),
    {ok, Open} = macula_frame:verify_request(Frame, Profile),
    client_session(attach_id_free(maps:get(request_id, Open), S), Bytes, Open, Opts, Caller, S).

client_session(false, _Bytes, _Open, _Opts, _Caller, S) ->
    {reply_value, {error, {refused, attach_id_taken}}, S};
client_session(true, Bytes, #{request_id := AttachId, mode := Mode} = Open, Opts, Caller,
               #state{peer_pid = Conn, profile = Profile, node_identity = Key} = S) ->
    {ok, StreamPid} = macula_stream:start_link(#{id => AttachId, role => client, mode => Mode,
                                                 owner => maps:get(owner, Opts, Caller),
                                                 key => fun() -> Key end, open => Open, conn => Conn,
                                                 profile => Profile}),
    ok = macula_stream:attach_to_link(StreamPid, self(), AttachId),
    Mon = erlang:monitor(process, StreamPid),
    {reply_value, {ok, StreamPid}, client_stream_opened(opened_stream(S), Bytes, AttachId, StreamPid, Mon, S)}.

%% Opens this session's dedicated stream on the peering connection.
opened_stream(#state{open_stream = Open, peer_pid = Conn}) ->
    try Open(Conn)
    catch Class:Reason -> {error, {Class, Reason}}
    end.

%% The provider's frames come back on the stream the open goes out on, so its
%% buffer exists from the start. A dedicated stream that does not open, or an
%% open that is not written, ends the session as a failed write does.
client_stream_opened({ok, Stream}, Bytes, AttachId, StreamPid, Mon,
                     #state{client_streams = CS, stream_bufs = Bufs} = S) ->
    Opened = S#state{client_streams = CS#{AttachId => {StreamPid, Mon, Stream}}, stream_bufs = Bufs#{Stream => <<>>}},
    stream_written(written(Stream, Bytes, Opened), StreamPid, AttachId, false, Opened);
client_stream_opened({error, Reason}, _Bytes, AttachId, StreamPid, Mon, S) ->
    erlang:demonitor(Mon, [flush]),
    StreamPid ! {stream_write_failed, AttachId, Reason},
    S.

%% A session's attach id names it in both session maps, so an id already taken
%% there is not given to another session.
attach_id_free(AttachId, #state{client_streams = CS, server_streams = SS}) ->
    not (is_map_key(AttachId, CS) orelse is_map_key(AttachId, SS)).

%%-------------------------------------------------------------------
%% Streaming RPC — outbound STREAM_DATA / END / ERROR / REPLY
%%-------------------------------------------------------------------

%% Each `macula_stream' bound to this link via the
%% `{remote_via_link, _, Sid}' peer shape casts the bytes of each frame
%% it signs here, and the link writes them on the stream's dedicated
%% QUIC stream. Every open stream session has its own dedicated QUIC
%% stream by the time anything is outbound on it: `find_stream/2'
%% returning `error' means the session already tore down (peer closed,
%% monitor DOWN raced this cast), and there is nothing to write to.
stream_bytes_sent(error, _Sid, _Bytes, _Last, S) ->
    S;
stream_bytes_sent({ok, {Pid, _Mon, Stream}}, Sid, Bytes, Last, S) ->
    stream_written(written(Stream, Bytes, S), Pid, Sid, Last, S).

written(Stream, Bytes, #state{send_on_stream = Send}) ->
    try Send(Stream, Bytes)
    catch Class:Reason -> {error, {Class, Reason}}
    end.

%% A failed write ends the session: the stream hears why, and the link
%% forgets it.
stream_written(ok, _Pid, Sid, true, S) ->
    drop_stream(Sid, S);
stream_written(ok, _Pid, _Sid, false, S) ->
    S;
stream_written({error, Reason}, Pid, Sid, _Last, S) ->
    Pid ! {stream_write_failed, Sid, Reason},
    drop_stream(Sid, S).

%% A session's own last frame, a failed write, or a stream that carried a
%% malformed frame ends its routing: its attach id leaves whichever map holds
%% it, its dedicated stream closes, and the stream's inbound buffer goes with
%% it, so `stream_bufs' keeps no entry for a finished session.
drop_stream(Sid, #state{client_streams = CS, server_streams = SS,
                        stream_bufs = Bufs} = S) ->
    {CS2, ClientMon, ClientStream} = drop_one(Sid, CS),
    {SS2, ServerMon, ServerStream} = drop_one(Sid, SS),
    _ = [erlang:demonitor(M, [flush])
         || M <- [ClientMon, ServerMon], M =/= undefined],
    _ = [close_dedicated_stream(Stream, S)
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

%% What `macula_frame:parse_received/1' gave for a dedicated stream: its frames
%% are dispatched in order and its tail is buffered. A frame that failed
%% validation ends the stream, and nothing after it is dispatched; bytes that
%% do not decode end it after the frames before them. Other streams on the
%% link, and the link itself, carry on.
dedicated_items({ok, Items, Tail}, Stream, #state{stream_bufs = Bufs} = S) ->
    {_Open, NewS} = dispatch_dedicated_items(Items, Stream,
                                             S#state{stream_bufs = Bufs#{Stream => Tail}}),
    NewS;
dedicated_items({malformed, Items, Reason}, Stream, S) ->
    end_malformed_dedicated(dispatch_dedicated_items(Items, Stream, S), Stream, Reason).

dispatch_dedicated_items([], _Stream, S) ->
    {open, S};
dispatch_dedicated_items([{invalid_frame, _Type, _Field} = Invalid | _Rest], Stream, S) ->
    {ended, end_sessions_on_stream(Stream, {malformed, Invalid}, S)};
dispatch_dedicated_items([Frame | Rest], Stream, S) ->
    dispatch_dedicated_items(Rest, Stream, dispatch_while_open(Frame, Stream, S)).

end_malformed_dedicated({open, S}, Stream, Reason) ->
    end_sessions_on_stream(Stream, {malformed, Reason}, S);
end_malformed_dedicated({ended, S}, _Stream, _Reason) ->
    S.

%% End every session carried by `Stream' once a write on it failed, or once
%% its bytes turned out not to be frames this link accepts: abort each
%% session's process with `Reason', as a lost link does, and drop its
%% routing. A same-pool session is in both maps under one Sid. `Stream' may
%% carry no session yet (a STREAM_ERROR refusal is written before one is
%% registered, or its first frame was malformed); its buffer goes and the
%% stream is closed either way.
end_sessions_on_stream(Stream, Reason, #state{client_streams = CS,
                                              server_streams = SS} = S) ->
    Carried = [{Sid, Pid} || {Sid, {Pid, _Mon, On}} <- maps:to_list(CS) ++ maps:to_list(SS),
                             On =:= Stream],
    _ = [abort_stream_process(Pid, Reason) || Pid <- lists:usort([P || {_, P} <- Carried])],
    S2 = lists:foldl(fun drop_stream/2, S, lists:usort([Sid || {Sid, _} <- Carried])),
    close_dedicated_stream(Stream, S2),
    S2#state{stream_bufs = maps:remove(Stream, S2#state.stream_bufs)}.

%%-------------------------------------------------------------------
%% Streaming RPC — dispatch for frames decoded off a dedicated stream
%%-------------------------------------------------------------------

%% A frame read off a dedicated stream is dispatched only while the stream
%% is open: once a frame closes it, as a refused STREAM_OPEN does, the frames
%% after it in the same read are not taken either.
dispatch_while_open(Frame, Stream, #state{stream_bufs = Bufs} = S) when is_map_key(Stream, Bufs) ->
    dispatch_dedicated_frame(Frame, Stream, S);
dispatch_while_open(_Frame, _Stream, S) ->
    S.

%% A STREAM_OPEN is verified here, and the link decides whether a session starts
%% for it. Every other stream frame belongs to the one session whose dedicated
%% stream it arrived on, and goes to that session's stream process, which
%% verifies it under its own open and tells the connection of a refusal. A
%% session frame on a stream that carries no session closes that stream. Any
%% other frame type has no place on a dedicated stream and is dropped.
dispatch_dedicated_frame(#{frame_type := stream_open} = Frame, Stream, #state{profile = Profile} = S) ->
    on_inbound_stream_open(macula_frame:verify_request(Frame, Profile), Stream, S);
dispatch_dedicated_frame(#{frame_type := Type} = Frame, Stream, S)
  when Type =:= stream_data; Type =:= stream_end; Type =:= stream_error; Type =:= stream_reply ->
    deliver_on_stream(session_on(Stream, S), Frame, Stream, S);
dispatch_dedicated_frame(_Frame, _Stream, S) ->
    S.

%%-------------------------------------------------------------------
%% Streaming RPC — inbound STREAM_OPEN (server-side dispatch)
%%-------------------------------------------------------------------

%% A verified, admitted and authorized STREAM_OPEN is served by the procedure
%% this link advertised under its realm and name: `Stream' is the dedicated
%% stream it came in on, and every frame of its session travels there.
handle_inbound_stream_open(#{realm := Realm, procedure := Proc} = Open, Stream, S) ->
    dispatch_stream_open(maps:find({Realm, Proc}, S#state.stream_procedures), Open, Stream, S).

%% A first frame longer than the limit is refused from its length header, as
%% `{malformed, [], frame_too_large}', which closes the stream
%% (`opening_items/3'). The frames after a first frame that fits have the
%% usual frame cap, so when only a later frame in the same read is longer
%% than the limit, the read is parsed again with that cap.
opening_parse({malformed, [_First | _], frame_too_large}, Bytes) ->
    macula_frame:parse_received(Bytes);
opening_parse(Parsed, _Bytes) ->
    Parsed.

%% The longest STREAM_OPEN a dedicated stream may start with, and the limit
%% `call_stream/6' refuses a longer open by. No frame is longer than the
%% 16 MiB frame cap, so a setting above it reads as that cap, which is also
%% the most `macula_frame:parse_received/2' takes.
stream_open_limit() ->
    min(application:get_env(macula, max_stream_open_bytes, ?MAX_STREAM_OPEN_BYTES), 16#FFFFFF).

%% What `macula_frame:parse_received/1' gave for a dedicated stream the peer
%% opened, before its first whole frame. With no whole frame yet the bytes
%% wait, and bytes that do not decode close the stream. Otherwise the
%% stream's buffer moves on to `stream_bufs' first, so a served STREAM_OPEN
%% keeps it and every refusal takes it away again, and the first item decides
%% whether the stream stays open. The items after it go the way of any
%% dedicated stream's (`dedicated_items/3'), and only while it stays open.
opening_items({ok, [], Tail}, Stream, #state{opening_bufs = Opening} = S) ->
    S#state{opening_bufs = Opening#{Stream => Tail}};
opening_items({malformed, [], _Reason}, Stream, #state{opening_bufs = Opening} = S) ->
    ok = close_dedicated_stream(Stream, S),
    S#state{opening_bufs = maps:remove(Stream, Opening)};
opening_items({Outcome, [First | Rest], TailOrReason}, Stream,
              #state{opening_bufs = Opening, stream_bufs = Bufs} = S) ->
    Moved = S#state{opening_bufs = maps:remove(Stream, Opening), stream_bufs = Bufs#{Stream => <<>>}},
    after_first_item(dispatch_first_frame(First, Stream, Moved), {Outcome, Rest, TailOrReason}, Stream).

after_first_item(#state{stream_bufs = Bufs} = S, Parsed, Stream) when is_map_key(Stream, Bufs) ->
    dedicated_items(Parsed, Stream, S);
after_first_item(S, _Parsed, _Stream) ->
    S.

%% A dedicated stream the peer opens must start with a STREAM_OPEN that
%% decodes whole. On any other first item, a frame of another type or one
%% that failed validation, it closes without a word: it carries no session,
%% and nothing on it is authenticated to answer.
dispatch_first_frame(#{frame_type := stream_open} = Frame, Stream, S) ->
    dispatch_dedicated_frame(Frame, Stream, S);
dispatch_first_frame(_NotAnOpen, Stream, S) ->
    close_sessionless_stream(Stream, S).

%% A STREAM_OPEN that does not verify never reaches a handler and gets nothing
%% back on its stream: the connection is told why, and a stream that carries no
%% session closes with it. A verified open for another node is not this node's
%% to answer either: the connection is told `not_the_target', and nothing is
%% signed, written or admitted for it.
on_inbound_stream_open({error, Kind}, Stream, #state{peer_pid = Conn} = S) ->
    ok = refusal_reported(Conn, Kind),
    close_unless_carrying(carries_a_session(Stream, S), Stream, S);
on_inbound_stream_open({ok, Open}, Stream, S) ->
    on_open_for(for_this_node(Open, S), Open, Stream, S).

on_open_for(false, _Open, Stream, #state{peer_pid = Conn} = S) ->
    ok = refusal_reported(Conn, not_the_target),
    close_unless_carrying(carries_a_session(Stream, S), Stream, S);
on_open_for(true, Open, Stream, #state{admission = Admission, share = Share} = S) ->
    on_admission(admitted(Admission, Open, Share), Open, Stream, S).

%% A verified request is for this node when its target is this link's node_id.
for_this_node(#{target := Target}, #state{node_identity = Key}) ->
    Target =:= node_id(Key).

refusal_reported(Conn, Kind) when is_pid(Conn) ->
    macula_peering:object_refused(Conn, Kind);
refusal_reported(_NoConnection, _Kind) ->
    ok.

%% The pool's admission judges a verified request once per caller and request
%% id, before any policy or handler. An admission that does not answer within
%% `?ADMIT_TIMEOUT_MS', or has stopped, refuses the request and never holds up
%% or ends the link.
admitted(Admission, Open, Share) ->
    try macula_request_admission:admit(Admission, Open, Share, erlang:system_time(millisecond), ?ADMIT_TIMEOUT_MS)
    catch exit:_NotAnswered -> {refused, unavailable}
    end.

%% A copy of an admitted open starts no second session, and a refused one none
%% at all: each gets a STREAM_ERROR under its own open, whose code names why.
on_admission(new, Open, Stream, S) ->
    on_stream_open_on(carries_a_session(Stream, S), Open, Stream, S);
on_admission({copy, _Reply}, Open, Stream, S) ->
    refuse_open(Stream, Open, <<"request_copy">>, <<"this request is already admitted">>, S);
on_admission({refused, Refusal}, Open, Stream, S) ->
    refuse_open(Stream, Open, admission_code(Refusal), <<"this request is not admitted">>, S).

%% The code a refusal by the request admission travels as: its kind's name.
admission_code({Kind, _Ms}) when is_atom(Kind) -> atom_to_binary(Kind);
admission_code(Kind) when is_atom(Kind) -> atom_to_binary(Kind).

close_unless_carrying(true, _Stream, S) ->
    S;
close_unless_carrying(false, Stream, S) ->
    close_sessionless_stream(Stream, S).

%% Closes a dedicated stream that carries no session and forgets its buffer,
%% so the link takes no more frames from it.
close_sessionless_stream(Stream, #state{stream_bufs = Bufs} = S) ->
    ok = close_dedicated_stream(Stream, S),
    S#state{stream_bufs = maps:remove(Stream, Bufs)}.

%% A dedicated stream carries one session. A STREAM_OPEN on a stream that
%% already carries one, served here or opened by this link as a caller, is
%% refused under that open, before its procedure's policy is asked; the session
%% already on the stream keeps it, and the stream stays open for that session.
on_stream_open_on(true, Open, Stream, S) ->
    refuse_open(Stream, Open, <<"refused">>, <<"this stream already carries a session">>, S);
on_stream_open_on(false, #{realm := Realm, procedure := Proc} = Open, Stream, #state{stream_policies = SPols} = S) ->
    on_stream_open_verdict(authorize({Realm, Proc}, Open, SPols), Open, Stream, S).

carries_a_session(Stream, #state{client_streams = CS, server_streams = SS}) ->
    lists:any(fun({_Pid, _Mon, On}) -> On =:= Stream end, maps:values(CS) ++ maps:values(SS)).

%% Refused by the procedure's auth policy: a STREAM_ERROR on the caller's own
%% stream, so it fails fast instead of waiting out its deadline, and no handler
%% runs.
on_stream_open_verdict(ok, Open, Stream, S) ->
    handle_inbound_stream_open(Open, Stream, S);
on_stream_open_verdict(unauthorized, Open, Stream, S) ->
    refuse_open(Stream, Open, <<"unauthorized">>, <<"not authorized for this procedure">>, S).

%% A procedure this link does not advertise is refused `not_found'. The open's
%% signed mode binds both sides' verifiers, so an open in a mode other than the
%% one its procedure is advertised in is refused `mode_mismatch', not served.
dispatch_stream_open(error, Open, Stream, S) ->
    refuse_open(Stream, Open, <<"not_found">>, <<"procedure not advertised">>, S);
dispatch_stream_open({ok, {Mode, Handler}}, #{mode := Mode} = Open, Stream, S) ->
    AttachId = crypto:strong_rand_bytes(16),
    served_with_id(attach_id_free(AttachId, S), AttachId, Handler, Open, Stream, S);
dispatch_stream_open({ok, {_OtherMode, _Handler}}, Open, Stream, S) ->
    refuse_open(Stream, Open, <<"mode_mismatch">>, <<"the procedure is advertised in another mode">>, S).

%% A served session's attach id is chosen here, since callers choose request
%% ids and two callers' opens may carry the same one. Its stream starts with the
%% link's key as a closure, the verified open, the peering connection and the
%% profile, owned by the process its handler will run in.
served_with_id(false, _AttachId, _Handler, Open, Stream, S) ->
    refuse_open(Stream, Open, <<"unavailable">>, <<"sessions are not being admitted now">>, S);
served_with_id(true, AttachId, Handler, #{procedure := Proc, payload := Args, caller := Caller, mode := Mode} = Open,
               Stream, #state{peer_pid = Conn, profile = Profile, node_identity = Key} = S) ->
    Worker = spawn_stream_handler(Handler, Args, Proc),
    {ok, StreamPid} = macula_stream:start_link(#{id => AttachId, role => server, mode => Mode, owner => Worker,
                                                 key => fun() -> Key end, open => Open, conn => Conn,
                                                 profile => Profile}),
    serve_if_admitted(macula_stream_sessions:admit(Caller, StreamPid), AttachId, Worker, StreamPid, Open, Stream, S).

%% A session past its caller's or the node's cap on served sessions, or one
%% the session counter could not admit, is refused on its own stream: its
%% handler process ends before it serves, and the stream process it would
%% have owned ends with it.
serve_if_admitted(ok, AttachId, Worker, StreamPid, _Open, Stream, #state{server_streams = SS} = S) ->
    ok = macula_stream:attach_to_link(StreamPid, self(), AttachId),
    Mon = erlang:monitor(process, StreamPid),
    Worker ! {serve, StreamPid},
    S#state{server_streams = SS#{AttachId => {StreamPid, Mon, Stream}}};
serve_if_admitted({error, Refusal}, _AttachId, Worker, _StreamPid, Open, Stream, S) ->
    exit(Worker, kill),
    {Code, Message} = admission_refusal(Refusal),
    refuse_open(Stream, Open, Code, Message, S).

admission_refusal(unavailable) ->
    {<<"unavailable">>, <<"sessions are not being admitted now">>};
admission_refusal(_AtACap) ->
    {<<"too_many_sessions">>, <<"no more sessions are served now">>}.

%% A refused STREAM_OPEN gets a STREAM_ERROR, the provider's first frame under
%% that open, and then a stream that carries no session closes: the link keeps
%% no buffer for it and takes no more frames from it. The STREAM_ERROR is
%% written before the close, and a close lets written data through before its
%% FIN.
refuse_open(Stream, Open, Code, Message, S) ->
    ok = send_stream_refusal(Stream, Open, Code, Message, S),
    close_unless_carrying(carries_a_session(Stream, S), Stream, S).

%% A refusal this link cannot build is not written.
send_stream_refusal(Stream, Open, Code, Message, #state{node_identity = Key} = S) ->
    refusal_built(macula_frame:stream_bytes({provider_stream, #{frame_type => stream_error, seq => 0, code => Code,
                                                               message => Message}, Open}, Key),
                  Stream, S).

refusal_built({ok, Built}, Stream, S) ->
    _ = written(Stream, macula_frame:written_bytes(Built), S),
    ok;
refusal_built({error, _Unbuildable}, _Stream, _S) ->
    ok.

%% Handler runs in a transient process, which owns the session's stream:
%% the stream ends when the handler returns or crashes, unless the handler
%% hands it over first (`macula_stream:controlling_process/2'). The process
%% runs the handler once the link has attached the stream, and ends without
%% running it when the link ends first. A handler crash
%% maps to a STREAM_ERROR abort with the crash class as the code and the
%% reason's name as the message, so callers see a stable error taxonomy and
%% none of the crash's terms; the crash goes to the node's log. The
%% try/catch is justified (mirrors `safe_invoke_handler/4' for unary
%% CALLs): without it a crash would silently leave the caller waiting on
%% its deadline.
spawn_stream_handler(Handler, Args, Proc) ->
    Link = self(),
    spawn(fun() -> serve_stream_when_attached(erlang:monitor(process, Link), Handler, Args, Proc) end).

%% Once the handler runs, the link's end is no concern of it, so no notice of
%% it is left in the handler's mailbox.
serve_stream_when_attached(LinkRef, Handler, Args, Proc) ->
    receive
        {serve, Stream} ->
            true = erlang:demonitor(LinkRef, [flush]),
            run_stream_handler(Handler, Stream, Args, Proc);
        {'DOWN', LinkRef, process, _Link, _Reason} ->
            ok
    end.

run_stream_handler(Handler, Stream, Args, Proc) ->
    try Handler(Stream, Args)
    catch
        Class:Reason:Stack ->
            logger:warning(
              "[macula_station_link] stream handler ~ts crashed: ~ts",
              [Proc, macula_reason_name:logged("~p:~p~n  stack=~p",
                                               [Class, Reason, Stack])]),
            _ = macula_stream:abort(Stream, atom_to_binary(Class, utf8),
                                    macula_reason_name:text(Reason))
    end.

%%-------------------------------------------------------------------
%% Streaming RPC — inbound STREAM_DATA / END / ERROR / REPLY
%%-------------------------------------------------------------------

%% The session a dedicated stream carries: at most one, in whichever map holds
%% it.
session_on(Stream, #state{client_streams = CS, server_streams = SS}) ->
    first_or_error([Pid || {Pid, _Mon, On} <- maps:values(CS) ++ maps:values(SS), On =:= Stream]).

%% A session frame goes to the stream process of the session its stream
%% carries. A peer's terminal frame does not end the routing: the session's
%% stream verifies it first, and the routing goes when that process ends or the
%% session writes its own last frame.
deliver_on_stream({ok, Pid}, Frame, _Stream, S) ->
    ok = macula_stream:deliver_frame(Pid, Frame),
    S;
deliver_on_stream(error, _Frame, Stream, S) ->
    close_sessionless_stream(Stream, S).

%%-------------------------------------------------------------------
%% Streaming RPC — DOWN routing (stream pid vs subscriber pid)
%%-------------------------------------------------------------------

%% Probe the client_streams and server_streams maps by pid; fall back
%% to the subscriber path. Stream pids are added by
%% `open_client_stream/6' (client_streams) and `spawn_inbound_stream/8'
%% (server_streams).
on_monitor_down(Pid, Mon, #state{client_streams = CS} = S) ->
    on_client_stream_down(find_stream_by_pid(Pid, CS), Pid, Mon, S).

on_client_stream_down({ok, Sid}, _Pid, Mon, #state{client_streams = CS,
                                                   stream_bufs = Bufs} = S) ->
    erlang:demonitor(Mon, [flush]),
    {CS2, Stream} = take_dedicated_stream(Sid, CS),
    close_dedicated_stream(Stream, S),
    S#state{client_streams = CS2, stream_bufs = drop_bufs([Stream], Bufs)};
on_client_stream_down(error, Pid, Mon, #state{server_streams = SS} = S) ->
    on_server_stream_down(find_stream_by_pid(Pid, SS), Mon, S).

on_server_stream_down({ok, Sid}, Mon, #state{server_streams = SS,
                                             stream_bufs = Bufs} = S) ->
    erlang:demonitor(Mon, [flush]),
    {SS2, Stream} = take_dedicated_stream(Sid, SS),
    close_dedicated_stream(Stream, S),
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
close_dedicated_stream(undefined, _S) -> ok;
close_dedicated_stream(Stream, #state{close_stream = Close}) ->
    try Close(Stream) catch _:_ -> ok end,
    ok.

-ifdef(TEST).
%% A client stream entry as `call_stream/6' makes one, for tests of the
%% stream write path.
with_client_stream(#state{client_streams = CS} = S, Sid, {StreamPid, Stream}) ->
    S#state{client_streams = CS#{Sid => {StreamPid, erlang:monitor(process, StreamPid), Stream}}}.
-endif.

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

-ifdef(TEST).
%% The position of a field in the state tuple, read from the record itself:
%% a test names the field, so a field added to the record cannot shift what
%% the test reads or sets.
state_field_index(Field) ->
    field_index(Field, record_info(fields, state), 2).

field_index(Field, [Field | _Rest], Index) -> Index;
field_index(Field, [_Other | Rest], Index) -> field_index(Field, Rest, Index + 1).
-endif.
