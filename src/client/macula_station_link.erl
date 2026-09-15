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
%%       pending calls with `{error, {disconnected, Reason}}', notify
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
%%   <tr><td>(connection drops)</td><td>`{error, {disconnected, Reason}}' or `{error, {peering_exit, Reason}}'</td></tr>
%%   <tr><td>(not connected yet)</td><td>`{error, not_connected}', not sent</td></tr>
%%   <tr><td>(frame refused before sending)</td><td>`{error, {refused, Reason}}', not sent</td></tr>
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
    %% Streaming RPC (SDK 3.17+, Part 6 §5.6)
    call_stream/5,
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
    %% The function that opens the peering connection, by default
    %% `macula_peering:connect/1'. An option, so a test replaces no
    %% shared module.
    connect => fun((map()) -> {ok, pid()} | {error, term()}),
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
%% `call_stream/5' refuses a longer open by the same limit, so both sides on
%% one node agree.
-define(MAX_STREAM_OPEN_BYTES, 16#100000).

-record(state, {
    seed             :: #{host := binary() | string(),
                          port := inet:port_number(),
                          _    => _},
    node_identity    :: macula_node_keys:node_key(),
    profile          :: macula_crypto_profile:profile(),
    issuer           :: pid(),
    connect          :: fun((map()) -> {ok, pid()} | {error, term()}),
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
    content_pending = #{}     :: #{reference() => {gen_server:from(), reference()}},
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
        %% try/catch retained: collapses the three distinct gen_server
        %% exit signals into the SDK's call-result taxonomy. Without
        %% it the caller sees `exit({timeout, _})' instead of
        %% `{error, timeout}', breaking the contract documented above.
        exit:{timeout, _}      -> {error, timeout};
        exit:{noproc, _}       -> {error, noproc};
        exit:{normal, _}       -> {error, gone}
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
%%   <li>`{macula_overlay_frame, SubRef, Frame, Meta}' — every time a
%%       matching frame arrives. `Frame' is the fully decoded frame
%%       map (including a `record' field already inflated to a
%%       `macula_record:m_record()' if the frame carried one — see
%%       `macula_frame:hyparview_join_spec()'). `Meta' is a map with
%%       a `sender' field: the connected peer's NodeId, since a frame
%%       does not self-identify its sender at the application layer.</li>
%%   <li>`{macula_overlay_gone, SubRef, Reason}' — once, when the
%%       connection drops or the client stops. The subscription is
%%       cleared on the same transition.</li>
%% </ul>
%%
%% The client monitors `Subscriber'; if it dies the subscription is
%% torn down.
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
%% the relay side. `Frame' carries its own, separate signature, which the
%% caller must make with THIS connection's identity: the receiving link
%% delivers `Frame' only if that signature verifies against the sender the
%% station names in the envelope, and that sender is this connection's
%% authenticated NodeId. This function only wraps it in the envelope, and
%% never touches `Frame'.
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
%% application env (1 MiB by default). The pool layer (`macula_client') should be
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
    identity_checked(Identity(), Issuer, Opts);
issuer_checked(_NoIssuer, _Identity, _Opts) ->
    {error, {issuer, required}}.

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
    Caps     = maps:get(capabilities, Opts, 0),
    Alpn     = maps:get(alpn, Opts, [<<"macula">>]),
    Tmo      = maps:get(connect_timeout_ms, Opts, 30_000),
    WdMs     = maps:get(connect_watchdog_ms, Opts, undefined),
    LiveMs   = maps:get(liveness_interval_ms, Opts, app_env(liveness_interval_ms, ?LIVENESS_INTERVAL_MS)),
    LiveMiss = maps:get(liveness_max_misses, Opts, app_env(liveness_max_misses, ?LIVENESS_MAX_MISSES)),
    RetryMs  = maps:get(connect_retry_backoff_ms, Opts, app_env(connect_retry_backoff_ms, ?CONNECT_RETRY_BACKOFF_MS)),
    State    = #state{seed = Seed, node_identity = Key, profile = Profile,
                      issuer = Issuer, connect = Connect,
                      capabilities = Caps, alpn = Alpn,
                      connect_timeout_ms = Tmo,
                      connect_watchdog_ms = WdMs,
                      liveness_interval_ms = LiveMs,
                      liveness_max_misses = LiveMiss,
                      refused_replies = macula_refusal_report:new(?REFUSED_REPLIES_WINDOW_MS),
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
            #state{node_identity = Id, content_pending = CP,
                   content_stream_bufs = Bufs} = S)
        when is_map_key(Stream, Bufs) ->
    Caller = node_id(Id),
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
    macula_diagnostics:event(<<"_macula.station_link.disconnected">>, #{
        seed     => Seed,
        peer_pid => Pid,
        reason   => Reason
    }),
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
    ok = close_dedicated_stream(Stream),
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
    macula_diagnostics:event(<<"_macula.station_link.peering_exit">>, #{
        seed     => Seed,
        peer_pid => Pid,
        reason   => Reason
    }),
    NewS = fail_all_pending({peering_exit, Reason}, cancel_liveness(S)),
    {stop, normal, NewS#state{peer_pid = undefined,
                              peer_node_id = undefined}};

%% The issuer every connection of this link draws from is gone: the link
%% ends with a shutdown reason, so its end is no crash report, and the pool
%% starts it again with the pool's next issuer.
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

terminate(_Reason, #state{peer_pid = Pid}) when is_pid(Pid) ->
    try macula_peering:close(Pid, client_stop) catch _:_ -> ok end,
    ok;
terminate(_Reason, _S) ->
    ok.

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
call_in_time(RemainingMs, {_Target, _Realm, _Proc, Payload, _DeadlineMs, _Token} = Call, From, S) ->
    call_sendable(macula_frame:check_payload(Payload), RemainingMs, Call, From, S).

%% A payload the wire cannot carry is refused before anything is built or
%% signed, so the call never goes out. Otherwise the request is signed with
%% the node identity key and kept as a verifier reads it, for its reply to be
%% checked against.
call_sendable({error, Unsendable}, _RemainingMs, _Call, _From, S) ->
    {reply, {error, {refused, Unsendable}}, S};
call_sendable(ok, RemainingMs, {Target, Realm, Proc, Payload, DeadlineMs, Token}, From,
              #state{peer_pid = Pid, node_identity = Key, profile = Profile, pending = P} = S) ->
    RequestId = crypto:strong_rand_bytes(16),
    Frame = macula_frame:call(with_token(Token, #{request_id => RequestId, realm => Realm, procedure => Proc,
                                                   target => target_node_id(Target, S), deadline => DeadlineMs,
                                                   payload => Payload}), Key),
    {ok, Request} = macula_frame:verify_request(Frame, Profile),
    %% NOT `ok = send_frame(...)': a frame the peering refuses comes back as
    %% an error, and a hard match would take this link down for every other
    %% caller on it. Reply with the reason instead.
    await_call_reply(macula_peering:send_frame(Pid, Frame), RequestId, Request, From, RemainingMs, P, S).

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
    Pub = node_id(Id),
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
%% EVENT — pubsub delivery. Fan out to every subscriber whose
%% (realm, topic) matches. Stations may push EVENTs without a prior
%% SUBSCRIBE on this connection (e.g. wildcard / catalog channels);
%% silently drop those.
on_frame(#{frame_type := event, topic := Topic, realm := Realm} = Frame, S) ->
    on_inbound_event(check_publisher_sig(Frame), Realm, Topic, Frame, S);
%% Inbound CALL — a CALL the station delivered to this link, for a
%% (realm, procedure) with a registered handler. Once the CALL's signature verifies against its
%% own `caller' (`on_inbound_call/3'), dispatch to the registered handler
%% and ship the resulting RESULT or call_error frame back over the same
%% peering connection.
on_frame(#{frame_type := call} = Frame, S) ->
    on_inbound_call(verify_signed_by(Frame, call_signer(Frame)), Frame, S);
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
%% `overlay_relay' is a relayed third-party frame (Phase 3.5): the
%% station forwarded it here because its `peer' field named US, having
%% received it from a DIFFERENT connection whose authenticated identity
%% is `Origin'. Decode the wrapped frame and deliver with `Origin' as
%% `Meta.sender' — NOT `peer_node_id' (that's the station's own
%% identity, always wrong for a genuine third-party HyParView peer).
%% Must be matched before the bare `#{realm := Realm}' clause below,
%% since an `overlay_relay' envelope has no `realm' field of its own.
%% The wrapped frame is delivered only once its own signature verifies
%% against `Origin' (`on_relayed_overlay_frame/3').
on_frame(#{frame_type := overlay_relay, peer := Origin, payload := Bytes}, S) ->
    case macula_frame:decode(Bytes) of
        {ok, Inner, _Rest} ->
            on_relayed_overlay_frame(macula_frame:verify(Inner, Origin), Origin, S);
        {error, _Reason} -> S
    end;
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
on_inbound_call({ok, _Verified}, Frame, S) ->
    handle_inbound_call(Frame, S),
    S;
on_inbound_call({error, Why}, Frame, S) ->
    logger:warning("[macula_station_link] dropped inbound CALL whose signature"
                   " does not verify against its caller (~p) procedure=~p",
                   [Why, maps:get(procedure, Frame, undefined)]),
    S.

%% The identity a CALL names as its signer, `caller'.
call_signer(#{caller := <<_:256>> = Pub}) -> {ok, Pub};
call_signer(_Frame)                      -> {error, no_signer}.

verify_signed_by(Frame, {ok, Pub})       -> macula_frame:verify(Frame, Pub);
verify_signed_by(_Frame, {error, _} = E) -> E.

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
    try CloseFun(Stream) catch _:_ -> ok end,
    S#state{content_pending = NewCP,
            content_stream_bufs = maps:remove(Stream, Bufs)}.

fail_content_pending(error, CP, _Reason) ->
    CP;
fail_content_pending({{From, TRef}, NewCP}, _CP, Reason) ->
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
        close_dedicated_stream(Stream),
        abort_stream_process(Pid, Reason)
    end,
    maps:foreach(AbortFun, CS),
    maps:foreach(AbortFun, SS),
    %% Content streams have no paired process to abort — just reclaim
    %% the QUIC resource, same as `close_content_stream_state/2' does
    %% on a normal close.
    maps:foreach(fun(Stream, _Buf) -> close_dedicated_stream(Stream) end,
                S#state.content_stream_bufs),
    %% A stream the peer opened that brought no whole frame yet carries no
    %% session to abort either.
    maps:foreach(fun(Stream, _Buf) -> close_dedicated_stream(Stream) end,
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
    deliver_overlay_frame_to(maps:find(Realm, Idx), Frame, PeerNodeId, S),
    S.

%% Phase 3.5: a relayed third-party frame arrived wrapped in an
%% `overlay_relay' envelope. `Sender' is the envelope's own `peer' field
%% (the station-authenticated origin of the ORIGINAL frame), never this
%% connection's own `peer_node_id' — that would always be the station's
%% identity, not the logical HyParView peer. See `on_frame/2''s
%% `overlay_relay' clause.
deliver_overlay_frame_from(Sender, #{realm := Realm} = Frame,
                           #state{overlay_realm_index = Idx} = S) ->
    deliver_overlay_frame_to(maps:find(Realm, Idx), Frame, Sender, S),
    S;
deliver_overlay_frame_from(_Sender, _Frame, S) ->
    %% Wrapped frame carries no `realm' — nothing to route on, same as
    %% the bare-frame catch-all in on_frame/2.
    S.

%% The inner frame of an `overlay_relay' is signed by the peer that emitted
%% it, and `Origin' is that peer's identity as the station authenticated
%% it. Every HyParView frame `macula_hyparview_proto' emits is signed with
%% the emitting peer's own identity, the one its link connects with, so a
%% genuine relayed frame verifies here. One that does not is dropped.
on_relayed_overlay_frame({ok, Inner}, Origin, S) ->
    deliver_overlay_frame_from(Origin, Inner, S);
on_relayed_overlay_frame({error, Why}, Origin, S) ->
    logger:warning("[macula_station_link] dropped relayed overlay frame whose"
                   " signature does not verify against its origin (~p)"
                   " origin=~s", [Why, hex_prefix(Origin)]),
    S.

deliver_overlay_frame_to(error, _Frame, _Sender, _S) ->
    ok;
deliver_overlay_frame_to({ok, Set}, Frame, Sender,
                         #state{overlay_subscriptions = Subs}) ->
    Meta = #{sender => Sender},
    sets:fold(fun(SubRef, _) ->
        fan_overlay_frame(maps:find(SubRef, Subs), SubRef, Frame, Meta)
    end, ok, Set).

fan_overlay_frame(error, _SubRef, _Frame, _Meta) ->
    ok;
fan_overlay_frame({ok, {_Realm, Subscriber, _Mon}}, SubRef, Frame, Meta) ->
    Subscriber ! {macula_overlay_frame, SubRef, Frame, Meta},
    ok.

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
%% `{error, Why}' — publisher_sig present but invalid: always warn, and
%%                  drop unless `pubsub_strict_publisher_sig' is
%%                  explicitly `false', which delivers it with
%%                  `publisher_verified => false'.
%%
%% The verification OUTCOME itself used to stop here: `deliver_event/4'
%% got only `Frame', so a subscriber could see `publisher' but never
%% learn whether its signature checked out — indistinguishable from
%% "never signed" for anyone downstream trying to weight a fact's
%% confidence by provenance quality. `not_signed' / `true' / `false'
%% now rides through to `Meta' as `publisher_verified' precisely so
%% that distinction survives to the subscriber.
on_inbound_event(ok, Realm, Topic, Frame, S) ->
    deliver_event(Realm, Topic, Frame, not_signed, S);
on_inbound_event({ok, _Verified}, Realm, Topic, Frame, S) ->
    deliver_event(Realm, Topic, Frame, true, S);
on_inbound_event({error, Why}, Realm, Topic, Frame, S) ->
    logger:warning("[macula_pubsub] inbound EVENT publisher_sig invalid (~p)"
                   " realm=~s topic=~s", [Why, hex_prefix(Realm), Topic]),
    on_invalid_publisher_sig(
      application:get_env(macula, pubsub_strict_publisher_sig, true),
      Realm, Topic, Frame, S).

on_invalid_publisher_sig(true, _Realm, _Topic, _Frame, S) ->
    S;
on_invalid_publisher_sig(_Lenient, Realm, Topic, Frame, S) ->
    deliver_event(Realm, Topic, Frame, false, S).

hex_prefix(B) when is_binary(B), byte_size(B) >= 4 ->
    binary:encode_hex(binary:part(B, 0, 4));
hex_prefix(B) when is_binary(B) ->
    binary:encode_hex(B);
hex_prefix(_) ->
    <<"?">>.

%% Fan an EVENT frame out to every subscriber for that (realm, topic).
%% `PublisherVerified' is `on_inbound_event/5''s already-computed
%% signature-check outcome (`not_signed' | `true' | `false') — see its
%% own doc for why this must ride through rather than be recomputed or
%% dropped.
deliver_event(Realm, Topic, Frame, PublisherVerified, #state{topic_index = Idx} = S) ->
    deliver_event_to(maps:find({Realm, Topic}, Idx), Realm, Topic, Frame,
                      PublisherVerified, S),
    S.

deliver_event_to(error, _Realm, _Topic, _Frame, _PublisherVerified, _S) ->
    ok;
deliver_event_to({ok, Set}, Realm, Topic, Frame, PublisherVerified,
                 #state{subscriptions = Subs}) ->
    Payload = maps:get(payload, Frame),
    Meta = #{realm              => Realm,
             publisher          => maps:get(publisher, Frame),
             publisher_verified => PublisherVerified,
             seq                => maps:get(seq, Frame),
             delivered_via      => maps:get(delivered_via, Frame, direct)},
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
handle_inbound_call(#{call_id := CallId, procedure := Proc, realm := Realm,
                      payload := Payload} = Frame,
                    #state{procedures = Procs, policies = Pols, node_identity = Id,
                           peer_pid = Pid}) when is_pid(Pid) ->
    SelfPub = node_id(Id),
    %% Gate first (Slice 7b): an `open' procedure serves any identified
    %% caller; a gated one requires a valid `ucan_token', else refuse
    %% with BOLT#4 `unauthorized' instead of invoking the handler.
    Verdict = authorize({Realm, Proc}, Frame, Pols),
    Found   = maps:find({Realm, Proc}, Procs),
    PayloadWithCaller = with_caller(Payload, maps:get(caller, Frame, undefined)),
    _ = spawn(fun() ->
            Reply = authorized_reply(Verdict, Found, CallId,
                                     PayloadWithCaller, SelfPub),
            sent_or_faulted(macula_peering:send_frame(Pid, Reply),
                            Pid, CallId, SelfPub)
        end),
    ok;
handle_inbound_call(_Frame, _State) ->
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
    check_ucan(maps:get(ucan_token, Frame, <<>>), Issuer,
               maps:get(caller, Frame, undefined));
authorize_policy({realm_member_required, RealmDid, RequiredCan}, Frame) ->
    check_realm_membership(maps:get(ucan_token, Frame, <<>>), RealmDid,
                            maps:get(caller, Frame, undefined), RequiredCan).

%% `macula_ucan_nif:verify/2' checks signature + `exp' + `nbf' only. It does
%% NOT check `aud' (see that function's own doc): a verified token proves
%% its issuer granted it to SOMEONE, not that it belongs to whoever is
%% presenting it now. Both gated policies therefore also require the
%% token's audience to be the caller, through `audience_is_caller/2'.
%% Without that, any token a caller obtained a copy of -- not necessarily
%% its own -- would authorize as if it were the caller it was minted for.
%% `Caller' is the frame's own `caller' field: `on_inbound_call/3' lets a
%% CALL through to `handle_inbound_call/2', and `on_inbound_stream_open/4'
%% lets a STREAM_OPEN through to `authorize/3', only once the frame's
%% signature verifies against that same `caller', so by the time these
%% checks run `Caller' is the identity that signed the frame.
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
%% audience as that identity's public key, hex-encoded in lowercase (as
%% `macula-realm''s `RealmUcanIssuer.mint_membership/2' does); `Caller' is
%% the same identity's raw wire public key, so hex-encoding it the same way
%% makes the two directly comparable. A token without a binary `aud' has no
%% audience to match.
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
%%                 detail = handler_error_detail(Reason))'
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
                                      detail      => handler_error_detail(Reason)});
        Reply ->
            macula_frame:result(#{call_id      => CallId,
                                  payload      => normalise_reply(Reply),
                                  responded_by => SelfPub})
    catch
        Class:Reason:Stack ->
            logger:warning(
              "[station_link] handler crashed: ~ts",
              [macula_reason_name:logged("~p:~p~n  stack=~p", [Class, Reason, Stack])]),
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

%% Spawn a client-side `macula_stream' linked to this link, attach
%% it as a `{remote_via_link, self(), Sid}' peer, then ship the
%% STREAM_OPEN frame. The caller drives the stream from outside; the
%% returned pid is bound to the requested `owner' (default = caller)
%% so a crashing owner tears the stream down.
open_client_stream(Realm, Proc, Args, Opts, Caller, #state{node_identity = Id} = S) ->
    Sid       = crypto:strong_rand_bytes(16),
    Mode      = maps:get(mode, Opts, server_stream),
    DeadlineMs = maps:get(deadline_ms, Opts,
                          erlang:system_time(millisecond) + 30_000),
    Frame = macula_frame:sign(macula_frame:stream_open(#{
        stream_id   => Sid,
        procedure   => Proc,
        realm       => Realm,
        mode        => Mode,
        args        => Args,
        deadline_ms => DeadlineMs,
        caller      => node_id(Id),
        ucan_token  => maps:get(ucan_token, Opts, <<>>)
    }), Id),
    open_within_limit(fits_open_limit(macula_frame:check_frame(Frame), Frame),
                      Frame, Sid, Mode, Opts, Caller, S).

%% A signed STREAM_OPEN longer than the limit a provider reads a stream's
%% first frame by is refused here, before a stream process starts or a
%% stream opens, so the caller learns at once instead of waiting out its
%% deadline. A frame that cannot be encoded goes the way it always has:
%% `macula_peering:send_on_stream/3' refuses it and says why.
fits_open_limit(ok, Frame) ->
    byte_size(macula_frame:encode(Frame)) - 4 =< stream_open_limit();
fits_open_limit({error, _Unsendable}, _Frame) ->
    true.

open_within_limit(false, _Frame, _Sid, _Mode, _Opts, _Caller, S) ->
    {reply_value, {error, {open_too_large, stream_open_limit()}}, S};
open_within_limit(true, Frame, Sid, Mode, Opts, Caller,
                  #state{peer_pid = Pid, node_identity = Id} = S) ->
    Owner = maps:get(owner, Opts, Caller),
    {ok, StreamPid} = macula_stream:start_link(#{
        id    => Sid,
        role  => client,
        mode  => Mode,
        owner => Owner
    }),
    ok = macula_stream:attach_to_link(StreamPid, self(), Sid),
    Mon = erlang:monitor(process, StreamPid),
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
    try macula_stream:deliver_error(StreamPid, <<"unavailable">>,
                                    <<"failed to open dedicated stream">>)
    catch _:_ -> ok end,
    S.

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
    stream_written(written(Stream, Bytes), Pid, Sid, Last, S).

written(Stream, Bytes) ->
    try macula_peering:send_on_stream(Stream, Bytes)
    catch Class:Reason -> {error, {Class, Reason}}
    end.

%% A failed write ends the session: the stream hears why, and the link
%% forgets it.
stream_written(ok, _Pid, Sid, true, S) ->
    maybe_drop_outbound(Sid, S);
stream_written(ok, _Pid, _Sid, false, S) ->
    S;
stream_written({error, Reason}, Pid, Sid, _Last, S) ->
    Pid ! {stream_write_failed, Sid, Reason},
    drop_stream(Sid, S).

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
    close_dedicated_stream(Stream),
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

%% Every stream-related frame type this link ever needs to act on,
%% now sourced from a session's own dedicated QUIC stream instead of
%% the shared control stream's `on_frame/2'. STREAM_OPEN is the only
%% one that can legitimately be the *first* frame on a freshly
%% handed-off inbound stream; the rest belong to a session already
%% tracked in `client_streams' / `server_streams'.
dispatch_dedicated_frame(#{frame_type := stream_open} = Frame, Stream, S) ->
    on_inbound_stream_open(verify_signed_by(Frame, call_signer(Frame)), Frame,
                           Stream, S);
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
                              realm := Realm, args := Args, caller := Caller},
                           Stream, S) ->
    dispatch_stream_open(maps:find({Realm, Proc}, S#state.stream_procedures),
                         Sid, Proc, Caller, Args, Stream, S).

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
%% `call_stream/5' refuses a longer open by. No frame is longer than the
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
    ok = close_dedicated_stream(Stream),
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

%% A STREAM_OPEN whose signature does not verify against its own `caller'
%% never reaches a handler and gets nothing back on its stream, the same
%% rule `on_inbound_call/3' applies to a unary CALL, and a stream that
%% carries no session closes with it. Once it verifies, the
%% procedure's auth policy (`advertise_stream/6') decides through the same
%% `authorize/3' a unary CALL goes through, before any handler runs.
on_inbound_stream_open({ok, _Verified}, Frame, Stream, S) ->
    on_stream_open_on(carries_a_session(Stream, S), Frame, Stream, S);
on_inbound_stream_open({error, Why}, Frame, Stream, S) ->
    logger:warning("[macula_station_link] dropped inbound STREAM_OPEN whose"
                   " signature does not verify against its caller (~p)"
                   " procedure=~p",
                   [Why, maps:get(procedure, Frame, undefined)]),
    close_unless_carrying(carries_a_session(Stream, S), Stream, S).

close_unless_carrying(true, _Stream, S) ->
    S;
close_unless_carrying(false, Stream, S) ->
    close_sessionless_stream(Stream, S).

%% Closes a dedicated stream that carries no session and forgets its buffer,
%% so the link takes no more frames from it.
close_sessionless_stream(Stream, #state{stream_bufs = Bufs} = S) ->
    ok = close_dedicated_stream(Stream),
    S#state{stream_bufs = maps:remove(Stream, Bufs)}.

%% A dedicated stream carries one session. A STREAM_OPEN on a stream that
%% already carries one, served here or opened by this link as a caller, is
%% refused on that stream, for its own stream id, before its procedure's
%% policy is asked; the session already on the stream keeps it, and the
%% stream stays open for that session.
on_stream_open_on(true, Frame, Stream, S) ->
    ok = send_stream_refusal(Stream, maps:get(stream_id, Frame), <<"refused">>,
                             <<"this stream already carries a session">>, S),
    S;
on_stream_open_on(false, Frame, Stream, #state{stream_policies = SPols} = S) ->
    Key = {maps:get(realm, Frame, undefined), maps:get(procedure, Frame, undefined)},
    on_stream_open_verdict(authorize(Key, Frame, SPols), Frame, Stream, S).

carries_a_session(Stream, #state{client_streams = CS, server_streams = SS}) ->
    lists:any(fun({_Pid, _Mon, On}) -> On =:= Stream end, maps:values(CS) ++ maps:values(SS)).

%% Refused by the procedure's auth policy: a STREAM_ERROR on the caller's
%% own stream, so it fails fast instead of waiting out its deadline, and no
%% handler runs.
on_stream_open_verdict(ok, Frame, Stream, S) ->
    handle_inbound_stream_open(Frame, Stream, S);
on_stream_open_verdict(unauthorized, Frame, Stream, S) ->
    refuse_stream_open(Stream, maps:get(stream_id, Frame), <<"unauthorized">>,
                       <<"not authorized for this procedure">>, S).

%% Unknown (Realm, Procedure) → ship a STREAM_ERROR back on the
%% caller's own dedicated stream so it unblocks immediately rather
%% than waiting for its deadline. The shared control stream is not
%% in this session's path at all, so the reply has to go here.
dispatch_stream_open(error, Sid, _Proc, _Caller, _Args, Stream, S) ->
    refuse_stream_open(Stream, Sid, <<"not_found">>, <<"procedure not advertised">>, S);
dispatch_stream_open({ok, {AdvMode, Handler}}, Sid, Proc, Caller, Args,
                     Stream, S) ->
    %% Advertised mode wins — the server declared the shape.
    spawn_inbound_stream(Sid, Proc, AdvMode, Handler, Args, Caller, Stream, S).

spawn_inbound_stream(Sid, Proc, Mode, Handler, Args, Caller, Stream, S) ->
    Worker = spawn_stream_handler(Handler, Args, Proc),
    {ok, StreamPid} = macula_stream:start_link(#{
        id    => Sid,
        role  => server,
        mode  => Mode,
        owner => Worker
    }),
    serve_if_admitted(macula_stream_sessions:admit(Caller, StreamPid),
                      Sid, Worker, StreamPid, Stream, S).

%% A session past its caller's or the node's cap on served sessions, or one
%% the session counter could not admit, is refused on its own stream: its
%% handler process ends before it serves, and the stream process it would
%% have owned ends with it.
serve_if_admitted(ok, Sid, Worker, StreamPid, Stream, #state{server_streams = SS} = S) ->
    ok = macula_stream:attach_to_link(StreamPid, self(), Sid),
    Mon = erlang:monitor(process, StreamPid),
    Worker ! {serve, StreamPid},
    S#state{server_streams = SS#{Sid => {StreamPid, Mon, Stream}}};
serve_if_admitted({error, Refusal}, Sid, Worker, _StreamPid, Stream, S) ->
    exit(Worker, kill),
    {Code, Message} = admission_refusal(Refusal),
    refuse_stream_open(Stream, Sid, Code, Message, S).

admission_refusal(unavailable) ->
    {<<"unavailable">>, <<"sessions are not being admitted now">>};
admission_refusal(_AtACap) ->
    {<<"too_many_sessions">>, <<"no more sessions are served now">>}.

%% A refused STREAM_OPEN on a stream that carries no session gets its
%% STREAM_ERROR, and then its stream closes: the link keeps no buffer for it
%% and takes no more frames from it. The STREAM_ERROR is written before the
%% close, and a close lets written data through before its FIN.
refuse_stream_open(Stream, Sid, Code, Message, S) ->
    ok = send_stream_refusal(Stream, Sid, Code, Message, S),
    close_sessionless_stream(Stream, S).

send_stream_refusal(Stream, Sid, Code, Message, #state{node_identity = Id}) ->
    Refusal = macula_frame:stream_error(#{stream_id => Sid, code => Code, message => Message}),
    _ = try macula_peering:send_on_stream(Stream, Refusal, Id) catch _:_ -> ok end,
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
close_dedicated_stream(Stream) ->
    try macula_peering:close_dedicated_stream(Stream) catch _:_ -> ok end,
    ok.

-ifdef(TEST).
%% A client stream entry as `call_stream/5' makes one, for tests of the
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
