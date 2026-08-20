%% @doc `macula_client' — the canonical pool client.
%%
%% Holds N peering links to N stations and routes ops with
%% replication, subscription replay, and inbound-event dedup. Apps
%% don't manage individual `macula_station_link' workers; they call
%% `macula_client' (or the `macula' facade, which re-exports the
%% public surface).
%%
%% Per `PLAN_V2_PARITY' Q2 §1: pool is the canonical client handle.
%% A single-station link is an internal worker only.
%%
%% == Lifecycle ==
%%
%% <pre>
%% {ok, Pool} = macula_client:connect(Seeds, Opts).
%% ok          = macula_client:publish(Pool, Realm, Topic, Payload, #{}).
%% {ok, Sub}   = macula_client:subscribe(Pool, Realm, Topic, self(), #{}).
%% receive {macula_event, Sub, Topic, Payload, Meta} -> ... end.
%% ok          = macula_client:unsubscribe(Pool, Sub).
%% ok          = macula_client:close(Pool).
%% </pre>
%%
%% == Replication ==
%%
%% `publish/5' fans the PUBLISH frame to `replication_factor' (default
%% 1) currently-spawned links. **Partial success counts as success**
%% per `PLAN_V2_PARITY' §5.1.1: the call returns `ok' as soon as one
%% link accepts the frame; the others are best-effort. When zero
%% links are spawned the call returns
%% `{error, {transient, no_healthy_station}}'.
%%
%% `subscribe/5' applies to every spawned link. The pool delivers a
%% deduped event stream to the consumer regardless of which link
%% relayed any given EVENT.
%%
%% == Dedup ==
%%
%% Inbound EVENT frames are keyed by `(Realm, Publisher, Seq)' in an
%% ETS table owned by the pool. The table is swept every
%% `dedup_sweep_ms' (default 30s) for entries older than
%% `dedup_window_ms' (default 60s).
%%
%% == Replay ==
%%
%% When a link's process dies the pool monitor fires; the pool
%% schedules a respawn after ?LINK_RESPAWN_DELAY_MS (1s). On respawn,
%% the pool re-issues every currently-tracked (Realm, Topic)
%% subscription against the new link via the internal
%% macula_client_replay helper.
-module(macula_client).
-behaviour(gen_server).

-export([connect/2, close/1, child_spec/3, status/1, links/1]).
%% Internal API — called by `macula_pubsub' (and future surfaces).
-export([publish/5, subscribe/5, unsubscribe/2]).
%% RPC fan-out (since 3.16.0) — called by the `macula' facade.
-export([call/5, call_station/6, call_station/7, call_station/8,
         advertise/4, advertise/5, unadvertise/3]).
%% Dedicated-stream content transfer (see
%% PLAN_PER_STREAM_QUIC_ISOLATION.md Phase 2) — called by the
%% `macula' facade to pin one link for a whole put_content/get_content
%% transfer instead of letting `call/5' pick per underlying block CALL.
-export([pick_connected_link/1]).
%% Streaming RPC (since 3.17.0) — called by the `macula' facade.
-export([call_stream/5, call_stream_station/6,
         advertise_stream/5, unadvertise_stream/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ifdef(TEST).
%% Probe guards — exported so a test can hang a link and prove the pool
%% survives it. See the note above safe_is_connected/1.
-export([safe_is_connected/1, safe_peer_node_id/1]).
-endif.

-export_type([pool/0, opts/0, seed/0, status/0, link_info/0, handler/0,
              stream_handler/0, auth_policy/0]).

%% Per-procedure auth policy for `advertise'. `open' (default) serves any
%% identified caller; `{ucan_required, Issuer}' gates the procedure on a
%% valid capability token chaining from `Issuer'. Direct-dial dual-trust
%% (Slice 7b).
-type auth_policy() :: open | {ucan_required, macula_identity:pubkey()}.

-type pool() :: pid().

%% RPC handler — accepted by `advertise/4'. Either a 1-arg fun called
%% with the inbound payload, or `{Module, Function}' invoked as
%% `Module:Function(Payload)'. Re-exported here so consumers do not
%% have to reach into the private `macula_station_link' module.
-type handler() :: fun((term()) -> term())
                 | {module(), atom()}.

%% Streaming RPC handler — accepted by `advertise_stream/5'. A 2-arg
%% fun invoked as `Handler(StreamPid, Args)' where `StreamPid' is
%% the local `macula_stream' bound to the inbound STREAM_OPEN.
-type stream_handler() :: fun((pid(), term()) -> any()).

%% Aggregate health snapshot of a pool. See `status/1'.
-type status() :: #{
    seeds            := [seed()],
    healthy_links    := non_neg_integer(),
    failed_links     := non_neg_integer(),
    self_node_id     := macula_identity:pubkey(),
    subscriptions    := non_neg_integer(),
    pubsub_gap_skips := non_neg_integer()
}.
%% Per-link view returned by `links/1'. One entry per configured seed
%% that currently has a spawned link worker. `node_id' is the peer
%% station's pubkey (`undefined' until CONNECT/HELLO completes);
%% `host' is the dial host parsed from the seed.
-type link_info() :: #{
    seed      := seed(),
    host      := binary() | undefined,
    pid       := pid(),
    connected := boolean(),
    node_id   := macula_identity:pubkey() | undefined
}.
-type seed() :: binary() | string()
              | #{host := binary() | string(),
                  port := inet:port_number()}.

-type opts() :: #{
    %% Shared Ed25519 keypair for every link in the pool. Stations see
    %% the pool as a single peer (one pubkey across N links).
    %% Auto-generated when absent.
    identity           => macula_identity:key_pair(),

    %% How many of the pool's currently-connected links accept a
    %% single PUBLISH frame. Partial success counts as success
    %% (`PLAN_V2_PARITY' §5.1.1). Default 1.
    replication_factor => pos_integer(),

    %% Per-link capability bitfield, forwarded to every
    %% `macula_station_link'. Default 0. Reserved for future use.
    capabilities       => non_neg_integer(),

    %% ALPN identifiers offered to the QUIC handshake. Default
    %% `[<<"macula">>]'.
    alpn               => [binary()],

    %% Per-link CONNECT/HELLO deadline in milliseconds. Default 30_000.
    %% Applies to each link independently — total pool readiness
    %% wallclock can be up to N×timeout for sequential dial fallback.
    connect_timeout_ms => pos_integer(),

    %% Inbound-EVENT dedup window in milliseconds. The pool keys
    %% inbound events on `(Realm, Publisher, Seq)' so duplicate
    %% deliveries from multiple subscribed links collapse to one
    %% emission per consumer. Default 60_000.
    dedup_window_ms    => non_neg_integer(),

    %% How often the dedup table is swept for entries older than
    %% `dedup_window_ms'. Default 30_000.
    dedup_sweep_ms     => pos_integer()
}.

%% V1 multi_relay options that have NO V2 equivalent. Callers passing
%% these from a V1 migration get a one-shot warning and the opt is
%% silently ignored. Keeping the names listed here so the warning
%% can name them helpfully.
-define(V1_LEGACY_OPTS, [relays, realm, site, connections]).

-define(DEFAULT_REPLICATION, 1).
-define(DEFAULT_DEDUP_WINDOW_MS, 60_000).
-define(DEFAULT_DEDUP_SWEEP_MS, 30_000).
%% How long an `ordered' subscription waits for a missing seq before
%% skipping the gap (a genuinely lost fact). Bounds head-of-line delay.
-define(DEFAULT_ORDER_TIMEOUT_MS, 250).
%% Per-publisher reorder-buffer count cap (bounds memory for a publisher
%% gapping under a high rate; the timeout bounds it in time).
-define(DEFAULT_ORDER_MAX_BUFFER, 1024).
-define(LINK_RESPAWN_DELAY_MS, 1_000).

-record(link_state, {
    seed     :: seed(),
    pid      :: pid() | undefined,
    mon      :: reference() | undefined
}).

-record(sub_spec, {
    realm      :: <<_:256>>,
    topic      :: binary(),
    subscriber :: pid(),
    mon        :: reference(),
    %% Per-publisher delivery ordering for this subscription (the
    %% `delivery' mode: ordered | latest_only | as_arrives).
    order      :: macula_pubsub_order:t()
}).

-record(state, {
    seeds         :: [seed()],
    identity      :: macula_identity:key_pair(),
    link_opts     :: map(),
    replication   :: pos_integer(),
    dedup_window  :: non_neg_integer(),
    dedup_sweep   :: pos_integer(),
    %% Pool-owned monotonic publish sequence. Stamped onto every
    %% outbound PUBLISH (via `macula_station_link:publish/5') so the
    %% station-side `(publisher, seq)' dedup stays stable across link
    %% respawns — the publisher pubkey is the pool's, shared by all
    %% links, so the seq must be owned by the pool, not the link.
    %% Seeded from wall-clock µs at init so a pool restart does not
    %% re-issue seqs that collide with the pre-restart tail still in a
    %% station's dedup window (see
    %% macula-station/plans/PLAN_PUBSUB_E2E_SIGNED_EVENTS.md).
    publish_seq   :: non_neg_integer(),
    %% seed → link_state
    links = #{}   :: #{seed() => #link_state{}},
    %% pool-owned SubRef → sub_spec
    subs = #{}    :: #{reference() => #sub_spec{}},
    %% {realm, topic} → set of pool-owned SubRefs
    topic_index = #{} :: #{{<<_:256>>, binary()} => sets:set(reference())},
    %% Advertised procedures — pool replays these on link respawn.
    %% {realm, procedure} → handler
    procs = #{}   :: #{{<<_:256>>, binary()} => {handler(), auth_policy()}},
    %% Advertised streaming procedures — replayed on link respawn
    %% alongside `procs'. {realm, procedure} → {mode, handler}
    stream_procs = #{} :: #{{<<_:256>>, binary()} =>
                            {macula_frame:stream_mode(),
                             stream_handler()}},
    dedup_tab     :: ets:tid(),
    %% Per-`ordered'-subscription reorder-buffer timeout + count cap, and
    %% a lazily armed one-shot timer that fires to release timed-out gaps.
    order_timeout    :: non_neg_integer(),
    order_max_buffer :: pos_integer(),
    flush_timer      :: reference() | undefined
}).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Spawn a pool with one link per seed. Returns immediately;
%% link handshakes complete asynchronously. Publish/subscribe block
%% until at least one link is connected (or fail with
%% `{error, {transient, no_healthy_station}}' on the publish path).
-spec connect([seed()], opts()) -> {ok, pool()} | {error, term()}.
connect(Seeds, Opts) when is_list(Seeds), is_map(Opts) ->
    gen_server:start_link(?MODULE, {Seeds, Opts}, []).

%% @doc Stop the pool. Every subscriber receives a final
%% `{macula_event_gone, SubRef, pool_closed}' message; every link
%% terminates with the pool.
-spec close(pool()) -> ok.
close(Pool) ->
    gen_server:stop(Pool, normal, 5_000).

%% @doc OTP child spec — drop the pool into a caller's supervision
%% tree. `Id' is the supervisor child id.
-spec child_spec(term(), [seed()], opts()) -> supervisor:child_spec().
child_spec(Id, Seeds, Opts) ->
    #{id       => Id,
      start    => {?MODULE, connect, [Seeds, Opts]},
      restart  => permanent,
      shutdown => 5_000,
      type     => worker,
      modules  => [?MODULE]}.

%% @doc Issue a CALL frame against the pool. Tries each healthy link
%% in turn and returns the first non-error reply. Returns
%% `{error, no_healthy_station}' when no link has completed its
%% CONNECT/HELLO handshake.
%%
%% Realm is per-call (32 bytes). Different realms can share a single
%% pool with no extra plumbing.
-spec call(pool(), <<_:256>>, binary(), term(), pos_integer()) ->
    {ok, term()} | {error, term()}.
call(Pool, Realm, Procedure, Payload, TimeoutMs)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       is_integer(TimeoutMs), TimeoutMs > 0 ->
    gen_server:call(Pool, {rpc_call, Realm, Procedure, Payload, TimeoutMs},
                    TimeoutMs + 1_000).

%% @doc Pick one currently-connected link and return its pid, without
%% issuing a call. For a caller that needs to pin ONE link across a
%% sequence of related calls — a dedicated QUIC stream, opened once
%% on the returned pid (via the internal station-link module's
%% content-stream API), only isolates one link's traffic, so every
%% call in the sequence must go over that same link. `call/5' picks
%% fresh per call (`call_first_success/5') and is the wrong primitive
%% for that.
%%
%% Selection matches `call_first_success/5''s ordering (first
%% connected link wins) so behaviour is unsurprising relative to the
%% existing pool-routed path.
-spec pick_connected_link(pool()) -> {ok, pid()} | {error, no_healthy_station}.
pick_connected_link(Pool) when is_pid(Pool) ->
    gen_server:call(Pool, pick_connected_link).

%% @doc Issue a CALL to ONE specific station, dialing it directly even
%% if it is not in the pool's seed set. `Station' is a seed URL (e.g.
%% `<<"quic://[::1]:4433">>'). The pool ensures a link to it (reusing an
%% existing one, or dialing and monitoring a new one exactly like a
%% seed), waits for the handshake within the deadline, and calls through
%% that link. This is the direct-dial data path: resolve a
%% serving_station (Slice 2) to its endpoint (Slice 3), then reach it in
%% one hop here — no mesh relay.
%%
%% Returns `{error, not_connected}' if the link does not complete its
%% handshake before the deadline.
-spec call_station(pool(), seed(), <<_:256>>, binary(), term(),
                   pos_integer()) -> {ok, term()} | {error, term()}.
call_station(Pool, Station, Realm, Procedure, Payload, TimeoutMs) ->
    call_station(Pool, Station, Realm, Procedure, Payload, TimeoutMs, <<>>).

%% @doc As `call_station/6', presenting a capability token (UCAN) to a
%% gated provider. Empty token = none. Slice 7b.
-spec call_station(pool(), seed(), <<_:256>>, binary(), term(),
                   pos_integer(), binary()) -> {ok, term()} | {error, term()}.
call_station(Pool, Station, Realm, Procedure, Payload, TimeoutMs, UcanToken) ->
    call_station(Pool, Station, Realm, Procedure, Payload, TimeoutMs,
                UcanToken, #{}).

%% @doc As `call_station/7', with a per-call TLS trust override for
%% THIS dial only — `verify' (webpki | none) and/or `expected_node_id'
%% (pin the station's Ed25519 identity) in `LinkOpts'. The pool's own
%% `connect/2'-time `verify'/`expected_node_id' are fixed at connect
%% time and apply uniformly to every link the pool dials (seeds and
%% every `call_station' target alike) — unworkable for direct-dial,
%% whose whole point is reaching a station not known until resolved at
%% call time. This lets a direct-dial caller pin trust to the specific
%% pubkey a signed DHT record just resolved, without weakening (or
%% needing to know in advance) the pool's default verification for its
%% other links. Only applies when a NEW link is dialed for `Station' —
%% an already-connected link keeps whatever trust it was dialed under.
-spec call_station(pool(), seed(), <<_:256>>, binary(), term(),
                   pos_integer(), binary(), map()) ->
    {ok, term()} | {error, term()}.
call_station(Pool, Station, Realm, Procedure, Payload, TimeoutMs, UcanToken,
            LinkOpts)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       is_integer(TimeoutMs), TimeoutMs > 0,
       is_binary(UcanToken),
       is_map(LinkOpts) ->
    gen_server:call(Pool,
                    {call_station, Station, Realm, Procedure, Payload,
                     TimeoutMs, UcanToken, LinkOpts},
                    TimeoutMs + 2_000).

%% @doc Advertise a procedure handler on every healthy link. Stored
%% in pool state so links respawned later replay the advertisement.
%% Returns `ok' when at least one link accepted the registration.
-spec advertise(pool(), <<_:256>>, binary(), handler()) ->
    ok | {error, term()}.
advertise(Pool, Realm, Procedure, Handler) ->
    advertise(Pool, Realm, Procedure, Handler, open).

%% @doc Advertise with an auth policy (`open' | `{ucan_required, Issuer}').
-spec advertise(pool(), <<_:256>>, binary(), handler(), auth_policy()) ->
    ok | {error, term()}.
advertise(Pool, Realm, Procedure, Handler, Policy)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       (is_function(Handler, 1) orelse
        (is_tuple(Handler) andalso tuple_size(Handler) =:= 2)) ->
    gen_server:call(Pool, {advertise, Realm, Procedure, Handler, Policy},
                    5_000).

%% @doc Drop a previously-advertised procedure on every healthy link
%% and remove it from the pool's replay state. Idempotent.
-spec unadvertise(pool(), <<_:256>>, binary()) -> ok.
unadvertise(Pool, Realm, Procedure)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure) ->
    gen_server:call(Pool, {unadvertise, Realm, Procedure}, 5_000).

%% @doc Open a streaming RPC against the pool. Picks the first
%% currently-healthy link and opens the stream there; the returned
%% stream pid is sticky — if the underlying link dies, the stream
%% errors with `{error, peer_down}' and the caller must re-open.
%%
%% Returns `{error, no_healthy_station}' when no link has completed
%% its CONNECT/HELLO handshake. `Realm' (32 bytes) and `Procedure'
%% name the remote endpoint. `Args' is the opening payload; `Opts'
%% accepts `mode' (default `server_stream'), `owner' (default the
%% calling pid), and `deadline_ms'.
-spec call_stream(pool(), <<_:256>>, binary(), term(), map()) ->
    {ok, pid()} | {error, term()}.
call_stream(Pool, Realm, Procedure, Args, Opts)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       is_map(Opts) ->
    gen_server:call(Pool,
                    {rpc_call_stream, Realm, Procedure, Args,
                     Opts#{owner => maps:get(owner, Opts, self())}},
                    5_000).

%% @doc Open a streaming RPC by DIALING a specific station directly
%% (direct-dial), instead of routing through an existing pool link.
%% The streaming analogue of `call_station/7': ensure (reuse or dial) a
%% link to `Station', await the handshake, then open the stream there.
%% `Opts' may set `dial_timeout_ms' (default 10_000) for the dial +
%% handshake, plus any `call_stream' option (e.g. `mode').
-spec call_stream_station(pool(), seed(), <<_:256>>, binary(), term(),
                          map()) -> {ok, pid()} | {error, term()}.
call_stream_station(Pool, Station, Realm, Procedure, Args, Opts)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       is_map(Opts) ->
    DialTimeout = maps:get(dial_timeout_ms, Opts, 10_000),
    gen_server:call(Pool,
                    {call_stream_station, Station, Realm, Procedure, Args,
                     Opts#{owner => maps:get(owner, Opts, self())}},
                    DialTimeout + 2_000).

%% @doc Advertise a streaming procedure handler on every healthy
%% link. Stored in pool state so links respawned later replay the
%% advertisement. Returns `ok' when at least one link accepted the
%% registration.
-spec advertise_stream(pool(), <<_:256>>, binary(),
                        macula_frame:stream_mode(),
                        stream_handler()) ->
    ok | {error, term()}.
advertise_stream(Pool, Realm, Procedure, Mode, Handler)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       (Mode =:= server_stream orelse Mode =:= client_stream
        orelse Mode =:= bidi),
       is_function(Handler, 2) ->
    gen_server:call(Pool,
                    {advertise_stream, Realm, Procedure, Mode, Handler},
                    5_000).

%% @doc Drop a streaming procedure on every healthy link and remove
%% it from the pool's replay state. Idempotent.
-spec unadvertise_stream(pool(), <<_:256>>, binary()) -> ok.
unadvertise_stream(Pool, Realm, Procedure)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure) ->
    gen_server:call(Pool, {unadvertise_stream, Realm, Procedure}, 5_000).

%% @doc Aggregate health snapshot of the pool. Single round-trip to
%% the pool's gen_server plus one `is_connected' probe per spawned
%% link (each capped at 1s). Suitable for `/health' or
%% `/status' endpoints; not for hot-loop polling.
%%
%% Counts:
%% <ul>
%%   <li>`healthy_links' — links whose worker pid is alive and whose
%%       CONNECT/HELLO handshake has completed.</li>
%%   <li>`failed_links' — every other configured seed (link not yet
%%       spawned, dead, or still handshaking).</li>
%% </ul>
-spec status(pool()) -> {ok, status()}.
status(Pool) when is_pid(Pool) ->
    gen_server:call(Pool, status, 5_000).

%% @doc Per-link snapshot of the pool — one `link_info()' per
%% configured seed that currently has a spawned link worker. Unlike
%% `status/1' (which only aggregates counts), this exposes each link's
%% `node_id' (peer station pubkey), dial `host', `pid', and
%% `connected' flag, so a caller can resolve a specific station (by
%% pubkey or hostname) to its link and address it directly.
%%
%% One `is_connected/1' + `peer_node_id/1' probe per spawned link
%% (each capped at 1s). Not for hot-loop polling.
-spec links(pool()) -> {ok, [link_info()]}.
links(Pool) when is_pid(Pool) ->
    gen_server:call(Pool, links, 5_000).

%% @doc Publish a frame to `replication_factor' currently-spawned
%% links. Partial success = success. Realm is per-call (32 bytes).
%%
%% The payload is checked for wire admissibility HERE, in the caller's
%% process, before the pool is touched. Downstream the send is a
%% `gen_statem:cast' into a shared peering connection that encodes
%% without a try/catch, so an unrepresentable term would kill that
%% connection and every other producer's in-flight traffic with it,
%% asynchronously, after this function had already answered `ok'.
%% Checking first is what makes the `ok' falsifiable.
%%
%% Returns `{error, {unsupported_payload_type, Type, Path}}' naming the
%% offending value and where it sits in the term. Floats are the common
%% case: scale them to integers (micro-units) or send binary strings.
-spec publish(pool(), <<_:256>>, binary(), term(), map()) ->
    ok | {error, term()}.
publish(Pool, Realm, Topic, Payload, Opts)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Topic),
       is_map(Opts) ->
    publish_checked(macula_frame:check_payload(Payload),
                    Pool, Realm, Topic, Payload, Opts).

publish_checked(ok, Pool, Realm, Topic, Payload, Opts) ->
    Timeout = maps:get(timeout_ms, Opts, 5_000),
    gen_server:call(Pool, {publish, Realm, Topic, Payload, Opts},
                    Timeout + 500);
publish_checked({error, _} = Rejected, _Pool, _Realm, _Topic, _Payload, _Opts) ->
    Rejected.

%% @doc Subscribe `Subscriber' to `(Realm, Topic)'. The pool
%% subscribes every currently-spawned link and dedupes inbound
%% events before fan-out. Returns `{ok, SubRef}'; `Subscriber'
%% receives `{macula_event, SubRef, Topic, Payload, Meta}' for each
%% delivered event and `{macula_event_gone, SubRef, Reason}' once
%% when the pool closes or the subscriber pid dies.
-spec subscribe(pool(), <<_:256>>, binary(), pid(), map()) ->
    {ok, reference()}.
subscribe(Pool, Realm, Topic, Subscriber, Opts)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Topic), is_pid(Subscriber),
       is_map(Opts) ->
    gen_server:call(Pool, {subscribe, Realm, Topic, Subscriber, Opts},
                    5_000).

%% @doc Drop a subscription. Idempotent — unknown `SubRef' is a
%% no-op. The wire-level link subscription persists for the pool's
%% lifetime (one wire sub per `(Realm, Topic)' multiplexed across
%% local consumers); Phase 4 will tighten this.
-spec unsubscribe(pool(), reference()) -> ok.
unsubscribe(Pool, SubRef) when is_pid(Pool), is_reference(SubRef) ->
    gen_server:call(Pool, {unsubscribe, SubRef}, 5_000).

%%====================================================================
%% gen_server
%%====================================================================

init({Seeds, Opts}) ->
    process_flag(trap_exit, true),
    warn_legacy_opts(Opts),
    Identity = maps:get(identity, Opts, macula_identity:generate()),
    LinkOpts = maps:merge(
        #{
            identity           => Identity,
            capabilities       => maps:get(capabilities, Opts, 0),
            alpn               => maps:get(alpn, Opts, [<<"macula">>]),
            connect_timeout_ms => maps:get(connect_timeout_ms, Opts, 30_000)
        },
        %% TLS policy for the links this pool dials (seeds AND
        %% `call_station' targets): `verify' (webpki | none) and
        %% `expected_node_id' (pin the station's Ed25519 identity).
        %% Forwarded only when the caller set them.
        maps:with([verify, expected_node_id], Opts)),
    DedupWindow = maps:get(dedup_window_ms, Opts, ?DEFAULT_DEDUP_WINDOW_MS),
    DedupSweep  = maps:get(dedup_sweep_ms, Opts, ?DEFAULT_DEDUP_SWEEP_MS),
    Replication = maps:get(replication_factor, Opts, ?DEFAULT_REPLICATION),
    DedupTab    = macula_client_dedup:new(),
    OrderTimeout = maps:get(order_timeout_ms, Opts, ?DEFAULT_ORDER_TIMEOUT_MS),
    OrderMaxBuf  = maps:get(order_max_buffer, Opts, ?DEFAULT_ORDER_MAX_BUFFER),
    State0 = #state{seeds = Seeds, identity = Identity,
                    link_opts = LinkOpts, replication = Replication,
                    dedup_window = DedupWindow, dedup_sweep = DedupSweep,
                    dedup_tab = DedupTab,
                    order_timeout = OrderTimeout, order_max_buffer = OrderMaxBuf,
                    flush_timer = undefined,
                    publish_seq = erlang:system_time(microsecond)},
    State1 = lists:foldl(fun start_link_for_seed/2, State0, Seeds),
    erlang:send_after(DedupSweep, self(), dedup_sweep),
    {ok, State1}.

handle_call({publish, Realm, Topic, Payload, _Opts}, From, S) ->
    %% Publish only to links that have completed CONNECT/HELLO. A
    %% frame sent to a still-handshaking link is dropped on the floor
    %% — unlike ADVERTISE, which the link replays on connect — so
    %% selecting the first `replication' *spawned* links could report
    %% `{error, not_connected}' while other links are healthy. RPC and
    %% streams already filter by `is_connected/1'; publish must too.
    %%
    %% Dispatch via a one-shot worker so concurrent publishes don't
    %% serialise through this gen_server (the per-link `publish/4'
    %% calls are sync 5s timeouts; under load they pile up at the pool
    %% and the harness's `multi_publisher_pubsub' case fails with
    %% empty receives).
    Targets = connected_link_pids(S),
    N = min(length(Targets), S#state.replication),
    Selected = lists:sublist(Targets, N),
    AllTargets = Targets,
    %% One pool-monotone seq per fact, reused across every replicated
    %% link so `{publisher, seq}' identifies the fact regardless of
    %% which station relayed it.
    Seq = S#state.publish_seq,
    _ = spawn(fun() ->
        Results = [macula_station_link:publish(P, Realm, Topic, Payload, Seq)
                   || P <- Selected],
        gen_server:reply(From, summarize_publish(Results, AllTargets))
    end),
    {noreply, S#state{publish_seq = Seq + 1}};

handle_call({subscribe, Realm, Topic, Subscriber, Opts}, _From, S) ->
    SubRef = make_ref(),
    Mon = erlang:monitor(process, Subscriber),
    Spec = #sub_spec{realm = Realm, topic = Topic,
                     subscriber = Subscriber, mon = Mon,
                     order = macula_pubsub_order:new(delivery_mode(Opts),
                                                     S#state.order_max_buffer)},
    Key = {Realm, Topic},
    AlreadyTracked = maps:is_key(Key, S#state.topic_index),
    NewS = register_sub(SubRef, Spec, S),
    issue_wire_subs(AlreadyTracked, Realm, Topic, NewS),
    {reply, {ok, SubRef}, NewS};

handle_call({unsubscribe, SubRef}, _From, S) ->
    {reply, ok, drop_sub(SubRef, S)};

handle_call(pick_connected_link, _From, S) ->
    {reply, first_connected_link(connected_link_pids(S)), S};

handle_call({rpc_call, Realm, Procedure, Payload, TimeoutMs}, From, S) ->
    %% Worker-spawn so concurrent CALLs don't serialise through the
    %% pool gen_server. Each per-link `macula_station_link:call/5'
    %% is a sync gen_server:call to the link; with the old
    %% `{reply, ..., S}' shape every caller blocked the pool until
    %% the link replied, capping concurrent CALL throughput at 1.
    Pids = spawned_link_pids(S),
    _ = spawn(fun() ->
        Reply = call_first_success(Pids, Realm, Procedure, Payload,
                                    TimeoutMs),
        gen_server:reply(From, Reply)
    end),
    {noreply, S};

handle_call({call_station, Station, Realm, Procedure, Payload, TimeoutMs,
             Ucan, LinkOpts}, From, S) ->
    %% Ensure (reuse or dial) a link to the specific station, then hand
    %% the wait-for-handshake + call to a worker so the pool gen_server
    %% is never blocked (same rationale as rpc_call).
    {Pid, S1} = ensure_link(Station, LinkOpts, S),
    _ = spawn(fun() ->
        Reply = call_when_connected(Pid, Realm, Procedure, Payload,
                                    TimeoutMs, Ucan),
        gen_server:reply(From, Reply)
    end),
    {noreply, S1};

handle_call({advertise, Realm, Procedure, Handler, Policy}, _From,
            #state{procs = P} = S) ->
    Pids = spawned_link_pids(S),
    Reply = fanout_advertise(Pids, Realm, Procedure, Handler, Policy),
    {reply, Reply,
     S#state{procs = P#{{Realm, Procedure} => {Handler, Policy}}}};

handle_call({unadvertise, Realm, Procedure}, _From,
            #state{procs = P} = S) ->
    _ = fanout_unadvertise(spawned_link_pids(S), Realm, Procedure),
    {reply, ok, S#state{procs = maps:remove({Realm, Procedure}, P)}};

handle_call({rpc_call_stream, Realm, Procedure, Args, Opts}, From, S) ->
    %% Worker-spawn for the same reason as `rpc_call' — the harness's
    %% `many_concurrent_streams' fires N parallel `call_stream/4' from
    %% separate caller processes; without this each one queued behind
    %% the pool gen_server.
    Pids = spawned_link_pids(S),
    _ = spawn(fun() ->
        Reply = stream_first_healthy(Pids, Realm, Procedure, Args, Opts),
        gen_server:reply(From, Reply)
    end),
    {noreply, S};

handle_call({call_stream_station, Station, Realm, Procedure, Args, Opts},
            From, S) ->
    %% Direct-dial streaming: ensure (reuse or dial) a link to the
    %% specific station, then open the stream there. Same worker-spawn
    %% rationale as call_station — the pool gen_server never blocks on
    %% the dial + handshake.
    {Pid, S1} = ensure_link(Station, S),
    _ = spawn(fun() ->
        Reply = stream_when_connected(Pid, Realm, Procedure, Args, Opts),
        gen_server:reply(From, Reply)
    end),
    {noreply, S1};

handle_call({advertise_stream, Realm, Procedure, Mode, Handler}, _From,
            #state{stream_procs = SP} = S) ->
    Pids = spawned_link_pids(S),
    Reply = fanout_advertise_stream(Pids, Realm, Procedure, Mode, Handler),
    {reply, Reply,
     S#state{stream_procs = SP#{{Realm, Procedure} => {Mode, Handler}}}};

handle_call({unadvertise_stream, Realm, Procedure}, _From,
            #state{stream_procs = SP} = S) ->
    _ = fanout_unadvertise_stream(spawned_link_pids(S), Realm, Procedure),
    {reply, ok,
     S#state{stream_procs = maps:remove({Realm, Procedure}, SP)}};

handle_call(status, _From,
            #state{seeds = Seeds, links = Links, subs = Subs,
                   identity = Identity} = S) ->
    {Healthy, Failed} = count_link_health(Seeds, Links),
    Status = #{
        seeds            => Seeds,
        healthy_links    => Healthy,
        failed_links     => Failed,
        self_node_id     => macula_identity:public(Identity),
        subscriptions    => map_size(Subs),
        %% Per-publisher gaps given up on after the reorder timeout —
        %% the genuine loss rate an `ordered' subscriber could not fill.
        pubsub_gap_skips => total_skips(Subs)
    },
    {reply, {ok, Status}, S};

handle_call(links, _From, #state{links = Links} = S) ->
    {reply, {ok, link_infos(Links)}, S};

handle_call(_Req, _From, S) ->
    {reply, {error, unknown_call}, S}.

handle_cast(_Msg, S) -> {noreply, S}.

handle_info({macula_event, _LinkSubRef, Topic, Payload, Meta}, S) ->
    Realm     = maps:get(realm, Meta, <<0:256>>),
    Publisher = maps:get(publisher, Meta),
    Seq       = maps:get(seq, Meta),
    on_inbound_event(macula_client_dedup:check(S#state.dedup_tab,
                                               Realm, Publisher, Seq),
                     Realm, Topic, Payload, Meta, S);

handle_info({macula_event_gone, _LinkSubRef, _Reason}, S) ->
    %% A link torn down its subscription end. Pool will respawn the
    %% link via the DOWN handler and replay subs. Don't propagate to
    %% local consumers — they see a continuous stream.
    {noreply, S};

handle_info({'DOWN', Mon, process, Pid, Reason}, S) ->
    on_down(Mon, Pid, Reason, S);

handle_info({respawn_link, Seed}, S) ->
    {noreply, on_respawn_link(Seed, S)};

handle_info(dedup_sweep, S) ->
    _ = macula_client_dedup:sweep(S#state.dedup_tab, S#state.dedup_window),
    erlang:send_after(S#state.dedup_sweep, self(), dedup_sweep),
    {noreply, S};

handle_info(order_flush, S) ->
    %% Release timed-out gaps, then re-arm only if something is still
    %% buffered (a fresh gap opened while this timer was pending).
    S1 = flush_all_subs(S#state{flush_timer = undefined}),
    {noreply, ensure_flush_timer(S1)};

handle_info({'EXIT', _Pid, _Reason}, S) ->
    %% Links are linked to us via gen_server:start_link in
    %% start_link_for_seed (we trap_exit). The DOWN monitor fires
    %% alongside; that path handles cleanup. Drop the EXIT.
    {noreply, S};

handle_info(_Other, S) ->
    {noreply, S}.

terminate(_Reason, #state{subs = Subs}) ->
    %% Notify every subscriber that the pool is gone.
    maps:foreach(
      fun(SubRef, #sub_spec{subscriber = Pid, mon = Mon}) ->
          erlang:demonitor(Mon, [flush]),
          Pid ! {macula_event_gone, SubRef, pool_closed}
      end, Subs),
    ok.

code_change(_OldVsn, S, _Extra) -> {ok, S}.

%%====================================================================
%% Internals — link lifecycle
%%====================================================================

start_link_for_seed(Seed, S) -> start_link_for_seed(Seed, #{}, S).

start_link_for_seed(Seed, ExtraOpts, S) ->
    LinkOpts = maps:merge(S#state.link_opts, ExtraOpts#{seed => Seed}),
    after_link_start(macula_station_link:start_link(LinkOpts), Seed, S).

after_link_start({ok, Pid}, Seed, S) ->
    Mon = erlang:monitor(process, Pid),
    LinkState = #link_state{seed = Seed, pid = Pid, mon = Mon},
    S#state{links = (S#state.links)#{Seed => LinkState}};
after_link_start({error, Reason}, Seed, S) ->
    macula_diagnostics:event(<<"_macula.client.link_start_failed">>,
                             #{seed => Seed, reason => Reason}),
    erlang:send_after(?LINK_RESPAWN_DELAY_MS, self(), {respawn_link, Seed}),
    Empty = #link_state{seed = Seed, pid = undefined, mon = undefined},
    S#state{links = (S#state.links)#{Seed => Empty}}.

spawned_link_pids(#state{links = Links}) ->
    [P || #link_state{pid = P} <- maps:values(Links), is_pid(P)].

%% Reuse a live link to `Station', else dial a new one and add it to the
%% pool exactly like a seed link (monitored, respawn-on-DOWN). Returns
%% the link pid (or `undefined' if the dial failed to spawn) + new state.
ensure_link(Station, S) -> ensure_link(Station, #{}, S).

%% As `ensure_link/2', but a FRESH dial (only) is made with `ExtraOpts'
%% merged on top of the pool's own `link_opts' — e.g. a direct-dial
%% caller's per-call `expected_node_id'. An already-connected, reused
%% link keeps whatever trust it was originally dialed under; `ExtraOpts'
%% only shapes a dial that happens as a result of THIS call.
ensure_link(Station, ExtraOpts, #state{links = Links} = S) ->
    ensure_link_for(maps:find(Station, Links), Station, ExtraOpts, S).

ensure_link_for({ok, #link_state{pid = Pid}}, _Station, _ExtraOpts, S)
        when is_pid(Pid) ->
    {Pid, S};
ensure_link_for(_Missing, Station, ExtraOpts, S) ->
    S1 = start_link_for_seed(Station, ExtraOpts, S),
    {link_pid(Station, S1), S1}.

link_pid(Station, #state{links = Links}) ->
    case maps:find(Station, Links) of
        {ok, #link_state{pid = Pid}} -> Pid;
        _                            -> undefined
    end.

%% Wait for a freshly-dialed link's handshake within the deadline, then
%% call over it with whatever time remains. A reused, already-connected
%% link calls immediately.
call_when_connected(undefined, _Realm, _Proc, _Payload, _TimeoutMs, _Ucan) ->
    {error, not_connected};
call_when_connected(Pid, Realm, Proc, Payload, TimeoutMs, Ucan) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    call_after_connect(await_connected(Pid, Deadline), Pid, Realm, Proc,
                       Payload, Deadline, Ucan).

await_connected(Pid, Deadline) ->
    connected_or_wait(safe_is_connected(Pid), Pid, Deadline).

connected_or_wait(true, _Pid, _Deadline) ->
    true;
connected_or_wait(false, Pid, Deadline) ->
    wait_or_give_up(erlang:monotonic_time(millisecond) < Deadline, Pid, Deadline).

wait_or_give_up(true, Pid, Deadline) ->
    timer:sleep(50),
    await_connected(Pid, Deadline);
wait_or_give_up(false, _Pid, _Deadline) ->
    false.

call_after_connect(true, Pid, Realm, Proc, Payload, Deadline, Ucan) ->
    Remaining = max(100, Deadline - erlang:monotonic_time(millisecond)),
    macula_station_link:call(Pid, Realm, Proc, Payload, Remaining, Ucan);
call_after_connect(false, _Pid, _Realm, _Proc, _Payload, _Deadline, _Ucan) ->
    {error, not_connected}.

%% Live links that have completed CONNECT/HELLO. Used by publish,
%% which (unlike advertise) gains nothing from dispatching to a
%% mid-handshake link.
connected_link_pids(#state{} = S) ->
    [P || P <- spawned_link_pids(S),
          is_process_alive(P),
          safe_is_connected(P)].

first_connected_link([Pid | _]) -> {ok, Pid};
first_connected_link([])        -> {error, no_healthy_station}.

%% Surface a one-shot warning when a caller passes V1 multi_relay
%% options that have no V2 equivalent. The opts are silently dropped
%% (V2's `init/1' simply doesn't read them) but the warning gives a
%% caller migrating from V1 a chance to spot the no-op.
warn_legacy_opts(Opts) ->
    Stale = [K || K <- ?V1_LEGACY_OPTS, maps:is_key(K, Opts)],
    notify_legacy(Stale).

notify_legacy([]) -> ok;
notify_legacy(Keys) ->
    logger:notice(
      "[macula_client] ignoring V1-only opts ~p — V2 is realm-per-call "
      "and one-link-per-seed. See macula:connect/2 docs.", [Keys]),
    ok.

%% First-success across the pool's healthy links. Tries each link in
%% turn; the first non-error reply wins. Falls through on
%% per-link errors (timeout, not_connected) so a single dead link does
%% not block the call.
call_first_success([], _Realm, _Proc, _Payload, _Tmo) ->
    {error, no_healthy_station};
call_first_success([Pid | Rest], Realm, Proc, Payload, Tmo) ->
    next_or_first(macula_station_link:is_connected(Pid),
                  Pid, Rest, Realm, Proc, Payload, Tmo).

next_or_first(false, _Pid, Rest, Realm, Proc, Payload, Tmo) ->
    call_first_success(Rest, Realm, Proc, Payload, Tmo);
next_or_first(true, Pid, Rest, Realm, Proc, Payload, Tmo) ->
    keep_or_next(macula_station_link:call(Pid, Realm, Proc, Payload, Tmo),
                 Rest, Realm, Proc, Payload, Tmo).

keep_or_next({ok, _} = R, _Rest, _Realm, _Proc, _Payload, _Tmo) -> R;
keep_or_next({error, _} = E, [], _Realm, _Proc, _Payload, _Tmo) -> E;
keep_or_next({error, _}, Rest, Realm, Proc, Payload, Tmo) ->
    call_first_success(Rest, Realm, Proc, Payload, Tmo).

%% Fan-out advertise: register on every live link. Returns ok if at
%% least one link accepted; per-link errors are logged and discarded.
%%
%% Pre-handshake links MUST receive the call too — `advertise/4' on
%% the link gen_server updates its local `procedures' map regardless
%% of connection state, and `drain_pending_advertises/1' replays that
%% map on the next handshake. Filtering by `is_connected/1' here
%% leaves the link's map out of sync with the pool's intent: a later
%% `unadvertise' that *also* gets filtered (still pre-handshake)
%% never clears the link's map, and the link will silently re-ADVERTISE
%% the dead procedure when it eventually handshakes — the station
%% re-registers a stale entry that nothing in the SDK will ever
%% withdraw.
fanout_advertise([], _Realm, _Proc, _Handler, _Policy) ->
    {error, no_healthy_station};
fanout_advertise(Pids, Realm, Proc, Handler, Policy) ->
    Results = [safe_link_advertise(P, Realm, Proc, Handler, Policy)
               || P <- Pids, is_process_alive(P)],
    summarize_advertise([R || R <- Results, R =/= skipped]).

safe_link_advertise(Pid, Realm, Proc, Handler, Policy) ->
    try macula_station_link:advertise(Pid, Realm, Proc, Handler, Policy)
    catch _:_ -> skipped
    end.

summarize_advertise([]) ->
    {error, no_healthy_station};
summarize_advertise(Results) ->
    Ok = lists:any(fun(ok) -> true; (_) -> false end, Results),
    case Ok of
        true  -> ok;
        false -> {error, all_stations_failed}
    end.

%% Fan-out unadvertise: best-effort; ignored errors. The local pool
%% state is dropped regardless so subsequent CALLs surface
%% `unknown_next_peer' from the station.
%%
%% MUST dispatch to every LIVE link (not just connected ones): the
%% link gen_server's `unadvertise' handler clears its local
%% `procedures' map unconditionally, and the wire UNADVERTISE is
%% best-effort inside `maybe_send_unadvertise' (no-op when
%% pre-handshake). Filtering by `is_connected/1' here leaks: a
%% link that was disconnected at unadvertise time keeps the proc in
%% its local map, and on the next handshake `drain_pending_advertises'
%% replays a now-dead ADVERTISE — the station re-registers an entry
%% that the pool already considers withdrawn.
fanout_unadvertise(Pids, Realm, Proc) ->
    [_ = safe_link_unadvertise(P, Realm, Proc)
     || P <- Pids, is_process_alive(P)],
    ok.

safe_link_unadvertise(Pid, Realm, Proc) ->
    try macula_station_link:unadvertise(Pid, Realm, Proc)
    catch _:_ -> skipped
    end.

%% Sticky-to-link selection for streams. Walk the healthy links in
%% order; the first one that opens cleanly wins. The returned stream
%% pid is bound to that link's `{remote_via_link, _, _}' peer; if
%% the link dies, the stream errors and the caller re-opens.
%% Per-link `{error, not_connected}' (handshake not done) falls
%% through; any other error short-circuits and is returned to the
%% caller, since it likely indicates a real problem (deadline,
%% protocol mismatch) the next link would also hit.
%% Direct-dial streaming: wait for the ensured link's handshake, then
%% open the stream there. Mirrors `call_when_connected' for streams.
stream_when_connected(undefined, _Realm, _Proc, _Args, _Opts) ->
    {error, not_connected};
stream_when_connected(Pid, Realm, Proc, Args, Opts) ->
    DialTimeout = maps:get(dial_timeout_ms, Opts, 10_000),
    Deadline = erlang:monotonic_time(millisecond) + DialTimeout,
    stream_after_connect(await_connected(Pid, Deadline), Pid, Realm, Proc,
                         Args, Opts).

stream_after_connect(true, Pid, Realm, Proc, Args, Opts) ->
    macula_station_link:call_stream(Pid, Realm, Proc, Args, Opts);
stream_after_connect(false, _Pid, _Realm, _Proc, _Args, _Opts) ->
    {error, not_connected}.

stream_first_healthy([], _Realm, _Proc, _Args, _Opts) ->
    {error, no_healthy_station};
stream_first_healthy([Pid | Rest], Realm, Proc, Args, Opts) ->
    on_stream_link(macula_station_link:is_connected(Pid),
                   Pid, Rest, Realm, Proc, Args, Opts).

on_stream_link(false, _Pid, Rest, Realm, Proc, Args, Opts) ->
    stream_first_healthy(Rest, Realm, Proc, Args, Opts);
on_stream_link(true, Pid, Rest, Realm, Proc, Args, Opts) ->
    keep_or_next_stream(macula_station_link:call_stream(
                          Pid, Realm, Proc, Args, Opts),
                        Rest, Realm, Proc, Args, Opts).

keep_or_next_stream({ok, _Stream} = R, _Rest, _Realm, _Proc, _Args, _Opts) ->
    R;
keep_or_next_stream({error, not_connected}, Rest, Realm, Proc, Args, Opts) ->
    stream_first_healthy(Rest, Realm, Proc, Args, Opts);
keep_or_next_stream({error, _} = E, _Rest, _Realm, _Proc, _Args, _Opts) ->
    E.

%% Fan-out streaming advertise across every live link. Same shape
%% as `fanout_advertise/4' for unary; partial success counts. Same
%% rationale for dispatching to pre-handshake links — see the
%% comment on `fanout_advertise/4'.
fanout_advertise_stream([], _Realm, _Proc, _Mode, _Handler) ->
    {error, no_healthy_station};
fanout_advertise_stream(Pids, Realm, Proc, Mode, Handler) ->
    Results = [safe_link_advertise_stream(P, Realm, Proc, Mode, Handler)
               || P <- Pids, is_process_alive(P)],
    summarize_advertise([R || R <- Results, R =/= skipped]).

safe_link_advertise_stream(Pid, Realm, Proc, Mode, Handler) ->
    try macula_station_link:advertise_stream(Pid, Realm, Proc, Mode, Handler)
    catch _:_ -> skipped
    end.

fanout_unadvertise_stream(Pids, Realm, Proc) ->
    [_ = safe_link_unadvertise_stream(P, Realm, Proc)
     || P <- Pids, is_process_alive(P)],
    ok.

safe_link_unadvertise_stream(Pid, Realm, Proc) ->
    try macula_station_link:unadvertise_stream(Pid, Realm, Proc)
    catch _:_ -> skipped
    end.

%% Count `(healthy, failed)' links across configured seeds. A seed is
%% healthy when its worker pid is alive AND its station_link reports
%% `is_connected'. Anything else (no pid yet, dead pid, mid-handshake)
%% counts as failed. Probes are sequential; cap at 1s per probe via
%% `is_connected/1' so a hung station can't stall the whole
%% `status/1' call past one second per stuck seed.
count_link_health(Seeds, Links) ->
    lists:foldl(fun(Seed, Acc) -> tally_seed(maps:find(Seed, Links), Acc) end,
                {0, 0}, Seeds).

tally_seed({ok, #link_state{pid = Pid}}, {H, F}) when is_pid(Pid) ->
    bump(link_healthy(Pid), H, F);
tally_seed(_, {H, F}) ->
    {H, F + 1}.

bump(true,  H, F) -> {H + 1, F};
bump(false, H, F) -> {H, F + 1}.

link_healthy(Pid) ->
    is_process_alive(Pid) andalso safe_is_connected(Pid).

%% Build one `link_info()' per spawned link. Skips seeds whose link
%% worker is not (yet) a live pid — those have no addressable station.
link_infos(Links) ->
    [link_info(Seed, Pid)
     || {Seed, #link_state{pid = Pid}} <- maps:to_list(Links),
        is_pid(Pid)].

link_info(Seed, Pid) ->
    Connected = link_healthy(Pid),
    #{seed      => Seed,
      host      => seed_host(Seed),
      pid       => Pid,
      connected => Connected,
      node_id   => link_node_id(Pid, Connected)}.

%% Only probe the peer pubkey on a connected link; a mid-handshake
%% link answers `{error, not_connected}'.
link_node_id(Pid, true) ->
    safe_peer_node_id(Pid);
link_node_id(_Pid, false) ->
    undefined.

%%--------------------------------------------------------------------
%% Link probes that cannot kill the pool
%%
%% `is_connected/1' and `peer_node_id/1' are both `gen_server:call' with
%% a 1s cap, and every caller below runs INSIDE the pool's own process.
%% A `gen_server:call' exits the CALLER two ways, and the pool is the
%% caller:
%%
%%   - `{noproc, _}'  — the link died since the `is_process_alive/1'
%%     check. Narrow, microseconds wide.
%%   - `{timeout, _}' — the link is merely alive and UNRESPONSIVE for a
%%     second. No race required at all, and a wedged station produces
%%     exactly this.
%%
%% The second is the reachable one and it was unguarded. Either takes
%% the pool down, and with it every subscription, advertisement and
%% pending call the process is holding — so probing one sick link
%% destroyed the client's entire connection to the mesh.
%%
%% ⚠ Deviation from let-it-crash, deliberate, per this repo's rule that
%% try/catch is permitted where it preserves a signal that would
%% otherwise be lost: without it, one failed probe and a genuinely
%% unhealthy pool are indistinguishable, because there is no pool left
%% to report either. An unreachable link answers `false' / `undefined'
%% here — truthful, conservative, and never mistakable for healthy.
%%--------------------------------------------------------------------

safe_is_connected(Pid) ->
    try macula_station_link:is_connected(Pid)
    catch _:_ -> false
    end.

%% Also absorbs an unexpected reply shape. The previous `case' matched
%% only `{ok, _}' and `{error, not_connected}', so any third answer was
%% a `case_clause' in the pool — the same fatality by another route.
safe_peer_node_id(Pid) ->
    try macula_station_link:peer_node_id(Pid) of
        {ok, NodeId} -> NodeId;
        _Other       -> undefined
    catch _:_ -> undefined
    end.

%% Dial host parsed from a seed. Mirrors `macula_station_link:parse_seed/1'
%% host extraction without re-dialing — URL form or pre-parsed map.
seed_host(#{host := H}) when is_binary(H) -> H;
seed_host(#{host := H}) when is_list(H)   -> list_to_binary(H);
seed_host(Url) when is_binary(Url)        -> seed_host(binary_to_list(Url));
seed_host(Url) when is_list(Url) ->
    case uri_string:parse(Url) of
        #{host := H} when H =/= "" -> list_to_binary(H);
        _                          -> undefined
    end;
seed_host(_) ->
    undefined.

on_respawn_link(Seed, S) ->
    NewS = start_link_for_seed(Seed, S),
    replay_to_seed(maps:get(Seed, NewS#state.links, undefined), NewS).

replay_to_seed(#link_state{pid = Pid}, S) when is_pid(Pid) ->
    macula_client_replay:subs_to(Pid, S#state.topic_index),
    macula_client_replay:advs_to(Pid, S#state.procs),
    macula_client_replay:stream_advs_to(Pid, S#state.stream_procs),
    S;
replay_to_seed(_, S) ->
    S.

%%====================================================================
%% Internals — DOWN routing (link vs subscriber)
%%====================================================================

on_down(Mon, Pid, Reason, S) ->
    on_down_routed(find_link_by_mon(Mon, S), Mon, Pid, Reason, S).

on_down_routed({ok, Seed}, _Mon, Pid, Reason, S) ->
    macula_diagnostics:event(<<"_macula.client.link_down">>,
                             #{seed => Seed, pid => Pid, reason => Reason}),
    erlang:send_after(?LINK_RESPAWN_DELAY_MS, self(), {respawn_link, Seed}),
    {noreply, S#state{links = maps:remove(Seed, S#state.links)}};
on_down_routed(error, Mon, _Pid, _Reason, S) ->
    {noreply, on_subscriber_down(Mon, S)}.

find_link_by_mon(Mon, #state{links = Links}) ->
    case [Seed || {Seed, #link_state{mon = M}} <- maps:to_list(Links),
                  M =:= Mon] of
        [Seed | _] -> {ok, Seed};
        []         -> error
    end.

on_subscriber_down(Mon, #state{subs = Subs} = S) ->
    Found = [SubRef || {SubRef, #sub_spec{mon = M}}
                       <- maps:to_list(Subs), M =:= Mon],
    lists:foldl(fun drop_sub/2, S, Found).

%%====================================================================
%% Internals — subscription bookkeeping
%%====================================================================

register_sub(SubRef, #sub_spec{realm = R, topic = T} = Spec,
             #state{subs = Subs, topic_index = Idx} = S) ->
    Key = {R, T},
    Set = maps:get(Key, Idx, sets:new()),
    NewIdx  = Idx#{Key => sets:add_element(SubRef, Set)},
    NewSubs = Subs#{SubRef => Spec},
    S#state{subs = NewSubs, topic_index = NewIdx}.

drop_sub(SubRef, #state{subs = Subs} = S) ->
    drop_sub_take(maps:take(SubRef, Subs), SubRef, S).

drop_sub_take(error, _SubRef, S) ->
    S;
drop_sub_take({#sub_spec{realm = R, topic = T, mon = Mon}, NewSubs},
              SubRef, #state{topic_index = Idx} = S) ->
    erlang:demonitor(Mon, [flush]),
    Key = {R, T},
    NewSet = sets:del_element(SubRef, maps:get(Key, Idx, sets:new())),
    NewIdx = on_index_after_drop(sets:is_empty(NewSet), Key, NewSet, Idx),
    S#state{subs = NewSubs, topic_index = NewIdx}.

on_index_after_drop(true,  Key, _Set, Idx) -> maps:remove(Key, Idx);
on_index_after_drop(false, Key,  Set, Idx) -> Idx#{Key => Set}.

issue_wire_subs(true, _Realm, _Topic, _S) ->
    %% A sibling consumer already triggered the wire-level subscribe;
    %% the pool fans out to every local SubRef on inbound EVENT.
    ok;
issue_wire_subs(false, Realm, Topic, S) ->
    PoolPid = self(),
    [_ = macula_station_link:subscribe(P, Realm, Topic, PoolPid)
     || P <- spawned_link_pids(S)],
    ok.

%%====================================================================
%% Internals — inbound event fan-out
%%====================================================================

on_inbound_event(duplicate, _Realm, _Topic, _Payload, _Meta, S) ->
    {noreply, S};
on_inbound_event(new, Realm, Topic, Payload, Meta, S) ->
    {noreply, ensure_flush_timer(fan_to_local(Realm, Topic, Payload, Meta, S))}.

fan_to_local(Realm, Topic, Payload, Meta, S) ->
    fan_to_set(maps:find({Realm, Topic}, S#state.topic_index),
               Topic, Payload, Meta, S).

fan_to_set(error, _Topic, _Payload, _Meta, S) ->
    S;
fan_to_set({ok, Set}, Topic, Payload, Meta, S) ->
    sets:fold(fun(SubRef, Acc) ->
        deliver_one(SubRef, Topic, Payload, Meta, Acc)
    end, S, Set).

deliver_one(SubRef, Topic, Payload, Meta, S) ->
    deliver_to(maps:find(SubRef, S#state.subs), SubRef, Topic, Payload, Meta, S).

deliver_to(error, _SubRef, _Topic, _Payload, _Meta, S) ->
    S;
deliver_to({ok, #sub_spec{subscriber = Pid, order = Order} = Spec}, SubRef,
           Topic, Payload, Meta, S) ->
    %% Run the fact through this subscription's delivery ordering; send
    %% whatever it releases now, and keep the updated per-publisher state.
    {Events, Order2} = macula_pubsub_order:offer(
                         Order, maps:get(publisher, Meta), maps:get(seq, Meta),
                         {Payload, Meta}, now_ms()),
    send_events(Pid, SubRef, Topic, Events),
    S#state{subs = maps:put(SubRef, Spec#sub_spec{order = Order2},
                            S#state.subs)}.

send_events(Pid, SubRef, Topic, Events) ->
    _ = [Pid ! {macula_event, SubRef, Topic, P, M} || {P, M} <- Events],
    ok.

now_ms() -> erlang:monotonic_time(millisecond).

total_skips(Subs) ->
    lists:sum([macula_pubsub_order:skips(O)
               || #sub_spec{order = O} <- maps:values(Subs)]).

%% Delivery mode from subscribe opts; `ordered' is the default (a
%% publish/subscribe API implies per-publisher order).
delivery_mode(#{delivery := M})
  when M =:= ordered; M =:= latest_only; M =:= as_arrives ->
    M;
delivery_mode(_Opts) ->
    ordered.

%% One-shot flush timer, armed lazily: only when an `ordered' buffer is
%% actually holding an out-of-order fact. Re-armed by the handler while
%% anything is still buffered; never runs when idle.
ensure_flush_timer(#state{flush_timer = undefined} = S) ->
    arm_flush_timer(any_buffered(S), S);
ensure_flush_timer(S) ->
    S.

arm_flush_timer(false, S) ->
    S;
arm_flush_timer(true, S) ->
    Ref = erlang:send_after(S#state.order_timeout, self(), order_flush),
    S#state{flush_timer = Ref}.

any_buffered(#state{subs = Subs}) ->
    lists:any(fun(#sub_spec{order = O}) ->
                  macula_pubsub_order:buffered(O) > 0
              end, maps:values(Subs)).

%% Release any gaps that have waited past the timeout, across every
%% subscription, sending what each frees.
flush_all_subs(#state{subs = Subs, order_timeout = Timeout} = S) ->
    Now = now_ms(),
    Subs2 = maps:map(
              fun(SubRef, #sub_spec{order = O, subscriber = Pid,
                                    topic = Topic} = Spec) ->
                  {Events, O2} = macula_pubsub_order:flush(O, Now, Timeout),
                  send_events(Pid, SubRef, Topic, Events),
                  Spec#sub_spec{order = O2}
              end, Subs),
    S#state{subs = Subs2}.

%%====================================================================
%% Internals — publish summary
%%====================================================================

summarize_publish([], []) ->
    {error, {transient, no_healthy_station}};
summarize_publish([], _NotEmpty) ->
    %% Replication factor capped at 0 by config; treat as no-op ok.
    ok;
summarize_publish(Results, _Targets) ->
    on_publish_results(lists:any(fun(R) -> R =:= ok end, Results), Results).

on_publish_results(true,  _Results)        -> ok;
on_publish_results(false, [First | _])     -> First;
on_publish_results(false, [])              -> {error, no_publish_attempts}.
