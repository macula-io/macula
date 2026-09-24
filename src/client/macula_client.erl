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
%% 2 with >=2 connected links, since 10.19.0 -- see `?DEFAULT_REPLICATION''s
%% own doc for exactly what this does and does not protect against)
%% currently-connected links. **Partial success counts as success**
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
%% A link verifies each publication before it hands the event to the
%% pool, and the pool delivers each publication at most once. It keys an
%% ETS table it owns on the event's `publication_hash', the SHA-384 of
%% the publication's `tbs', and keeps each entry until the publication's
%% `expires_at', after which every verifier refuses it. The table is swept
%% every `dedup_sweep_ms' (default 30s). The pool checks an event only
%% while a subscription matches it, so a copy that arrives while nothing
%% is subscribed never hides the publication from a later subscriber, and
%% it drops an event whose `expires_at' has passed when the check runs.
%%
%% == Replay ==
%%
%% When a link's process dies the pool monitor fires; the pool
%% schedules a respawn after ?LINK_RESPAWN_DELAY_MS (1s). On respawn,
%% the pool re-issues every currently-tracked (Realm, Topic)
%% subscription against the new link via the internal
%% macula_client_replay helper, and starts the link with the options it
%% was started with before (`#link_state.extra_opts'), which is what
%% keeps a direct dial's `expected_node_id' across a bounce.
-module(macula_client).
-behaviour(gen_server).

-export([connect/2, close/1, child_spec/3, status/1, links/1, sign_node_record/2, sign_node_record/3, sign_domain_record/2,
         withdraw_node_record/3, realm_key/2]).
%% Internal API — called by `macula_pubsub' (and future surfaces).
-export([publish/5, subscribe/5, unsubscribe/2]).
%% RPC fan-out (since 3.16.0) — called by the `macula' facade.
-export([call_linked_station/5, call_station/7, call_station/8, call_station/9,
         call_station/10,
         advertise/4, advertise/5, advertise/6, unadvertise/3,
         advertise_stream/5, advertise_stream/6, advertise_stream/7,
         unadvertise_stream/3]).
%% Dedicated-stream content transfer (see
%% PLAN_PER_STREAM_QUIC_ISOLATION.md Phase 2) — called by the
%% `macula' facade to pin one link for a whole put_content/get_content
%% transfer instead of letting `call_linked_station/5' pick per underlying block CALL.
-export([pick_connected_link/1]).
%% Direct-dial content transfer — called by the `macula' facade to pin
%% a link to a SPECIFIC (resolved) station rather than picking from the
%% pool's existing links.
-export([ensure_station_link/4]).
%% Direct-dial resolution head start — called by `macula_direct_dial' to
%% remember which station last answered a procedure, and to get it back.
-export([resolved_candidate/3, remember_resolved/5]).
%% Streaming RPC (since 3.17.0) — called by the `macula' facade.
-export([call_stream_station/7]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/1]).

-ifdef(TEST).
%% The issuer restart delay and backoff, exported for macula_client_pool_keys_tests.
-export([issuer_restart_delay/2, next_issuer_backoff/1]).
%% Probe guards — exported so a test can hang a link and prove the pool
%% survives it. See the note above safe_is_connected/1.
-export([safe_is_connected/1, safe_peer_node_id/1]).
%% The connect wait, exported so a test can prove the instrument on it emits
%% ONE event per wait rather than one per 50 ms poll. That property lives in
%% which function the recursion runs through and is invisible on inspection.
-export([await_connected/2]).
%% The two decisions behind `resolved_candidate/3' and `remember_resolved/5',
%% exported so a test can drive them with explicit clock readings. Both are
%% about TIME and about a LIVE LINK, and neither property can be shown by
%% inspection: that an entry's life is measured in elapsed monotonic
%% milliseconds rather than against a wall clock is visible only by moving
%% the reading, and that a remembered station is dropped the moment its link
%% goes is visible only by taking the link away.
-export([still_usable/3, remembered/5]).
%% The pure selection math behind publish/5's replication fan-out, and
%% the per-link crash guard its fan-out worker uses — exported for
%% macula_client_tests.erl only, see their own docs.
-export([select_publish_targets/2, safe_link_publish/5]).
%% Station discovery selection math — exported for direct testing, same
%% rationale as `select_publish_targets/2' above.
-export([ordered_for_selection/2, select_discovery_seeds/3, station_seed/1, seed_peer/1]).
%% A state field's position in the state tuple, by name, for the key redaction tests.
-export([state_field_index/1]).
%% How a pool call moves from one link to the next, over any links and
%% call -- exported for macula_client_call_first_success_tests.erl, which
%% replaces no module.
-export([first_success/3]).
-endif.

-export_type([pool/0, opts/0, seed/0, status/0, link_info/0, last_disconnect/0, handler/0,
              stream_handler/0, auth_policy/0]).

%% Per-procedure auth policy for `advertise'. `open' (default) serves any
%% identified caller. `{ucan_required, IssuerNodeId}' gates the procedure
%% on one known node: a valid token (macula_ucan) issued by the node with
%% that node_id whose audience is the calling node itself, so a token minted
%% for anyone else is refused.
%%
%% `{realm_member_required, RealmKeyId, RequiredCan}' gates on membership in
%% a realm instead: a valid token issued by the realm's key, named by its
%% key id (NOT the 32-byte realm id used in `-realm' flags and DHT scoping;
%% the two are unrelated values), whose audience is the calling node itself,
%% carrying the capability `RequiredCan'.
%%
%% Both policies bind the audience the same way: macula_ucan:authorize/3
%% compares a token's `aud', the audience's node_id in lowercase hex, with
%% the wire-authenticated caller.
%%
%% `RequiredCan' is mandatory, not optional-with-a-default: a realm mints
%% membership UCANs at more than one tier from the SAME signing key --
%% e.g. macula-realm's own citizen tier (`member/email-verified', a human
%% confirmed a join session) versus its device tier
%% (`member/device-verified', any device that proves it holds a keypair,
%% no human involved, gated only by an admission list that ships
%% permissive-by-default). A signature+audience check alone cannot tell
%% these apart -- both are genuine, correctly-audienced tokens from the
%% real realm. Accepting either would let any device self-enroll and pass
%% as a genuine member, defeating the isolation this policy exists to
%% provide (Fable review, 2026-09-05). There is no silent default here on
%% purpose: a caller must name the tier it actually requires (typically
%% the realm's citizen/human-confirmed capability string) rather than
%% inherit a guess that might be wrong for its threat model.
-type auth_policy() :: open | macula_ucan:policy().

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
    seeds              := [seed()],
    healthy_links      := non_neg_integer(),
    failed_links       := non_neg_integer(),
    self_node_id       := <<_:256>>,
    subscriptions      := non_neg_integer(),
    replication_factor := pos_integer(),
    pubsub_gap_skips   := non_neg_integer(),
    refused_dials      := #{too_many_direct_links | new_peer_budget_spent | unusable_seed
                            | link_start_waits_for_issuer | seed_without_expected_node_id
                            | pin_tls_cert_refused => pos_integer()},
    issuer_restarts    := non_neg_integer(),
    issuer_losses      := non_neg_integer()
}.
%% Per-link view returned by `links/1'. One entry per configured seed
%% that currently has a spawned link worker. `node_id' is the peer
%% station's pubkey (`undefined' until CONNECT/HELLO completes);
%% `host' is the dial host parsed from the seed. `last_disconnect' is why
%% this seed's link last went down, kept across the respawn that replaced
%% it, or `undefined' if it never has: see `last_disconnect()'.
-type link_info() :: #{
    seed            := seed(),
    host            := binary() | undefined,
    pid             := pid(),
    connected       := boolean(),
    node_id         := macula_node_keys:node_id() | undefined,
    last_disconnect := last_disconnect() | undefined
}.
%% Why a seed's link last went down, and when (`at_ms', system time). `reason'
%% is the reason's name only, never its terms. A `peer_identity_mismatch' also
%% names the node id the seed expected and the one the station presented, in
%% lowercase hex: both are public, and an operator needs both to tell a stale
%% pin from a station whose identity moved.
-type last_disconnect() :: #{
    reason            := binary(),
    at_ms             := integer(),
    expected_node_id  => binary(),
    presented_node_id => binary()
}.
-type seed() :: binary() | string()
              | #{host := binary() | string(),
                  port := inet:port_number()}.

-type opts() :: #{
    %% The node identity key that every link in the pool shares: an
    %% identity key in the node's crypto profile. Stations see the pool
    %% as a single peer (one node_id across N links). When absent, the
    %% node's one stored identity (macula_node_keys:node_identity/1). Given as the key, or
    %% as a loader {Module, Function, Args} that returns {ok, Key}, which a
    %% child spec must use so the spec holds no key. A loader's Args say
    %% where the key is and never hold it, because a supervisor logs them
    %% when a start fails.
    node_identity      => macula_node_keys:node_key() | {module(), atom(), [term()]},
    %% The function the pool starts its statement issuer with, of the
    %% shape of macula_statement_issuer_sup:start_issuer/2. For tests.
    issuer_start       => fun((fun(() -> macula_node_keys:node_key()), pid()) -> {ok, pid()} | {error, term()}),

    %% How many of the pool's currently-connected links accept a
    %% single PUBLISH frame. Partial success counts as success
    %% (`PLAN_V2_PARITY' §5.1.1). Default 2, since 10.19.0 (was 1) --
    %% see `?DEFAULT_REPLICATION''s own doc for why.
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

    %% How often the inbound publication dedup table is swept for
    %% entries whose publication has expired. Default 30_000.
    dedup_sweep_ms     => pos_integer(),

    %% The realm keys the pool pins, one per realm id: each realm's public
    %% key as carried, configured per deployment beside the realm id. A call
    %% trusts an org namespaced advertisement only through the key pinned for
    %% its realm. The pool does not start unless every id is 32 bytes and
    %% every key is well formed for the node's crypto profile.
    realm_trust        => #{<<_:256>> => binary()},

    %% Opt-in dynamic station discovery via `hecate_stations.list_stations'
    %% (the mesh's canonical station directory). Absent, or
    %% `#{enabled => false}': the pool behaves exactly as before this
    %% option existed -- `Seeds' is the whole story, forever. Enabled:
    %% `Seeds' becomes the BOOTSTRAP list (unchanged meaning -- dialled
    %% first, and the permanent fallback if discovery never succeeds).
    %% Once a bootstrap link connects, the pool resolves which realm
    %% `hecate_stations.list_stations' is advertised under (a DHT lookup;
    %% there is no way to know its realm without asking first) and calls
    %% it; every station it returns that isn't already a link gets one
    %% (up to `max_links'), replayed with current subs/advertises exactly
    %% like a respawned link. Refresh is additive only: a station absent
    %% from a later discovery response never tears down an existing live
    %% link (replication lag in the read model is not evidence a station
    %% is gone) -- removal stays tied to the existing crash/DOWN cleanup.
    %% Default OFF: fleet memory records at least one deployment relying
    %% on Frankfurt being first in a hand-configured seed list (a 1-hop
    %% sentinel path); this must never change under a caller that hasn't
    %% opted in.
    station_discovery => #{
        enabled    => boolean(),
        %% Re-run discovery on this cadence, and opportunistically the
        %% moment every currently-held link goes unhealthy at once
        %% (independent of the timer -- that's exactly the moment "the
        %% world changed" is most likely true). Default 1_800_000 (30 min)
        %% -- the station directory changes on provisioning timescales,
        %% not seconds.
        refresh_ms => pos_integer(),
        %% Cap on total concurrent links (bootstrap + discovered) --
        %% counts EVERY entry in `#state.links', including direct-dial
        %% targets (`call_station'/`ensure_station_link') and a seed
        %% still mid-respawn after a failed dial, not only successfully
        %% connected discovered stations. A large station directory
        %% should not mean dozens of QUIC connections. Default 5.
        max_links  => pos_integer(),

        %% A discovered station that NEVER once connects (e.g. one
        %% whose only reachable address is a bare IP this pool has no
        %% way to dial safely yet -- see the Pinned-trust design note
        %% on `seed()') would otherwise retry forever and permanently
        %% occupy a `max_links' slot, silently starving room for any
        %% OTHER station a later refresh tick discovers. Unlike a
        %% bootstrap seed (a human explicitly chose it; it keeps
        %% retrying forever, unchanged), a discovered seed nobody chose
        %% is safe to give up on and free its slot -- it is
        %% rediscoverable again on a later refresh tick if the station
        %% ever becomes reachable. A discovered link that connects even
        %% once is never subject to this again, for its whole lifetime,
        %% even if it later disconnects. Default 60_000 (1 minute) --
        %% generous relative to a normal handshake (~100ms) but short
        %% relative to the default `refresh_ms' (30 min), so a stuck
        %% slot is freed well before the next refresh cycle needs it.
        giveup_after_ms => pos_integer(),
        %% How often to check for a stale, never-connected discovered
        %% link. Independent of `refresh_ms' (which controls how often
        %% NEW stations are discovered, not how often existing
        %% never-connected ones are re-judged). Default 5_000.
        giveup_sweep_ms => pos_integer()
    },

    %% How a one-shot CALL (`call_linked_station/5') or PUBLISH (`publish/5', within its
    %% `replication_factor' slice) picks among currently-connected links.
    %% `first_success' (default, unless `station_discovery' is enabled --
    %% see below): today's behaviour, unchanged -- try links in the order
    %% they were spawned, first non-error reply wins. `random': shuffle
    %% the candidate list first, so load spreads across every connected
    %% link instead of pinning to whichever seed happened to be listed or
    %% discovered first. Defaults to `random' when `station_discovery' is
    %% enabled (there is little reason to discover a bigger station set
    %% and then still only ever call the first one) and `first_success'
    %% otherwise, but either can be set explicitly to override that
    %% pairing.
    link_selection => first_success | random,

    %% Most configured seeds a pool starts with. A pool given more does
    %% not start: `connect/2' returns `{error, {too_many_seeds, Given,
    %% Max}}'. Every link a pool holds can carry inbound requests, so the
    %% links it holds at once are bounded: its seeds, `max_links' for
    %% discovery, and `max_direct_links'. Default 16.
    max_seeds => pos_integer(),

    %% Most direct-dial links a pool holds at once: links dialed by
    %% `call_station', `ensure_station_link' or `call_stream_station' to a
    %% station that is not already a link. A fresh dial past it is refused
    %% with `{error, too_many_direct_links}'. Default 8.
    max_direct_links => pos_integer(),

    %% Most new peers a pool links to per 15 minutes, each counted once by
    %% its normalized seed, for a fresh direct dial and for a discovered
    %% station alike. A configured seed never spends it. Past it, a fresh
    %% direct dial is refused with `{error, new_peer_budget_spent}' and a
    %% discovered station is left for a later discovery run. Default 16.
    new_peer_budget => pos_integer(),

    %% Limits of the request admission the pool runs for every request its
    %% links receive (plans/DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md,
    %% Requests): entries per caller (default 256) and per link's share
    %% (1024), and stored reply bytes per caller (256 KiB) and in total
    %% (16 MiB). A link's share is its normalized seed. A key not given here
    %% falls back to the `macula' application environment's
    %% `request_admission', then to its default. Each is an integer from 1 to
    %% a cap (65,536; 65,536; 16 MiB; 1 GiB), with the quota per caller no
    %% larger than the share and the reply bytes per caller no larger than
    %% the total, or the pool does not start: `{error,
    %% {invalid_admission_limit, Key, Value}}' or `{error,
    %% {admission_limit_above, Smaller, Larger}}'.
    request_admission => #{caller_quota => pos_integer(), share => pos_integer(),
                           reply_bytes => pos_integer(), reply_bytes_total => pos_integer()},

    %% The station node_id every link this pool dials must prove.
    expected_node_id   => <<_:256>>
    %% ⚠ `pin_tls_cert' and `verify' are DELIBERATELY NOT DECLARED. Both
    %% are refused at runtime (`macula:connect/2'), and leaving them out of
    %% this closed map means a typed caller hears it from dialyzer as well.
    %% See macula#15 and `macula:trust_options_checked/1'.
}.

%% V1 multi_relay options that have NO V2 equivalent. Callers passing
%% these from a V1 migration get a one-shot warning and the opt is
%% silently ignored. Keeping the names listed here so the warning
%% can name them helpfully.
-define(V1_LEGACY_OPTS, [relays, realm, site, connections]).

%% 2, not 1: a station can look perfectly healthy (answers the app-liveness
%% ping -- CONNECT/HELLO done, still answers `_macula.ping') while silently
%% relaying a PUBLISH nowhere for a reason that liveness check cannot see,
%% e.g. it just doesn't serve/route the caller's realm even though nothing
%% about the connection itself looks wrong. At replication_factor=1 that
%% single "connected" link is the whole story for every publish through
%% the pool: total, silent data loss, with `ok' returned throughout.
%%
%% What this does NOT protect against: a wrong `Realm' passed by the
%% CALLER itself. Every replicated copy carries the identical `Realm'
%% argument (see `handle_call({publish, Realm, ...})' below) -- a
%% publisher-side realm misconfiguration blackholes every selected link
%% identically, replication factor notwithstanding. That specific failure
%% mode is what surfaced this gap live 2026-09-05 (a warden whose presence
%% heartbeat used a stale realm id), but replication_factor would not have
%% fixed THAT incident; it protects the adjacent, genuinely link-local
%% case where the caller's own config is right and one specific station's
%% relay path is the thing silently broken.
%%
%% Also only helps a pool with >=2 connected links -- a single-seed pool
%% gets no benefit from raising this. 2 is the minimum that helps at all
%% (one bad selected link no longer means zero delivery, as long as a
%% second is live) without defaulting every publisher in the ecosystem to
%% 3x traffic (2x, really, on top of the previous 1x) for marginal extra
%% protection past "survives one bad station" -- see CONNECTING_GUIDE.md's
%% Replication factor section. Note publish's own fan-out worker
%% (`safe_link_publish/5') must not let one selected link's crash or
%% timeout swallow an earlier link's already-accepted frame -- with
%% replication_factor=1 there was never a "later" link for that to matter;
%% raising the default makes it matter for everyone.
-define(DEFAULT_REPLICATION, 2).
-define(DEFAULT_DEDUP_SWEEP_MS, 30_000).
%% How long an `ordered' subscription waits for a missing seq before
%% skipping the gap (a genuinely lost fact). Bounds head-of-line delay.
-define(DEFAULT_ORDER_TIMEOUT_MS, 250).
%% Per-publisher reorder-buffer count cap (bounds memory for a publisher
%% gapping under a high rate; the timeout bounds it in time).
-define(DEFAULT_ORDER_MAX_BUFFER, 1024).
-define(LINK_RESPAWN_DELAY_MS, 1_000).

%% Station discovery (opt-in, `station_discovery' opt -- see `opts()').
-define(DEFAULT_DISCOVERY_REFRESH_MS, 1_800_000).
-define(DEFAULT_DISCOVERY_MAX_LINKS, 5).
%% First discovery attempt fires shortly after `connect/2' returns, not
%% immediately -- gives the bootstrap links a moment to complete
%% CONNECT/HELLO (healthy handshakes finish in ~100ms per
%% `macula_station_listener''s own doc). Not load-bearing for
%% correctness: a discovery attempt with zero connected links just gets
%% `no_healthy_station' back and retries on the next refresh tick, same
%% as any other transient failure.
-define(INITIAL_DISCOVERY_DELAY_MS, 500).
%% `hecate_stations.list_stations' is a plain mesh RPC; this timeout
%% belongs to the discovery worker only, an ordinary call deadline. The
%% `_dht.find_records_by_type' lookup half of discovery uses `macula.erl'
%% own `?DHT_RECORD_TIMEOUT_MS' internally.
-define(DISCOVERY_CALL_TIMEOUT_MS, 5_000).
-define(LIST_STATIONS_PROCEDURE, <<"hecate_stations.list_stations">>).

%% A discovered station nobody chose -- unlike a hand-configured seed,
%% it is safe to give up on one that never once connects and free its
%% `max_links' slot (it's rediscoverable again on a later refresh tick
%% if the station comes back). Bootstrap seeds never go through this
%% path regardless of these defaults -- see `giveup_after_ms''s own
%% doc on `station_discovery'.
-define(DEFAULT_DISCOVERY_GIVEUP_MS, 60_000).
-define(DEFAULT_DISCOVERY_GIVEUP_SWEEP_MS, 5_000).

%% The links a pool holds and dials are bounded: every link can carry
%% inbound requests, and each peer behind a link holds a share of the
%% provider's seen requests while they live (`macula_request_admission').
-define(DEFAULT_MAX_SEEDS, 16).
-define(DEFAULT_MAX_DIRECT_LINKS, 8).
-define(DEFAULT_NEW_PEER_BUDGET, 16).
%% A seen request lives until its deadline plus 5 minutes, and a deadline
%% lies at most 10 minutes ahead of the provider's clock, so 15 minutes is
%% the longest one entry lives. The new-peer budget counts over that window.
-define(NEW_PEER_WINDOW_MS, 15 * 60_000).
%% A refused dial is counted every time and logged at most once a minute per
%% reason.
-define(REFUSAL_REPORT_WINDOW_MS, 60_000).
%% Each link limit is an integer from 1 to its cap, or the pool does not
%% start. The caps keep the links a pool holds, and so the shares of a
%% provider's seen requests, within a fixed bound.
-define(MAX_SEEDS_CAP, 64).
-define(MAX_DIRECT_LINKS_CAP, 64).
-define(NEW_PEER_BUDGET_CAP, 256).
-define(DISCOVERY_MAX_LINKS_CAP, 64).
%% The request admission limits a pool starts with unless it is given others
%% (plans/DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md, Requests), and the most each
%% may be set to. The admission's cap is the share times the pool's link
%% limits summed, the most distinct shares one entry lifetime can see.
-define(DEFAULT_ADMISSION_LIMITS, #{caller_quota => 256, share => 1024, reply_bytes => 262144,
                                    reply_bytes_total => 16777216}).
-define(ADMISSION_LIMIT_CAPS, #{caller_quota => 65536, share => 65536, reply_bytes => 16777216,
                                reply_bytes_total => 1073741824}).

-record(link_state, {
    seed          :: seed(),
    pid           :: pid() | undefined,
    mon           :: reference() | undefined,
    %% `true' for a link a fresh direct dial made (`dial_fresh/3'). It
    %% counts against `max_direct_links' for its whole life, respawns
    %% included.
    direct = false :: boolean(),
    %% The three fields below are meaningless (left at their defaults)
    %% for a bootstrap or direct-dial link -- only
    %% `add_one_discovered_seed/2' ever sets `discovered = true', via
    %% `mark_discovered/2' immediately after the link is created. See
    %% `sweep_stale_discovered_links/2'.
    discovered     = false :: boolean(),
    %% Set `true' the first time a discovery-added link is ever
    %% observed connected -- once true, it behaves exactly like any
    %% other link forever after (no more give-up checks), same as a
    %% hand-configured seed that happens to be down right now.
    ever_connected = false :: boolean(),
    %% When this link entry was created (`erlang:monotonic_time
    %% (millisecond)'). Preserved across a respawn of the SAME seed
    %% (`after_link_start/4') rather than reset, so a discovered link
    %% that keeps dying and respawning without ever once connecting
    %% doesn't get an ever-renewing grace period.
    spawned_at     :: integer() | undefined
}).

%% `undefined' on `#state.discovery' means the feature is off for this
%% pool -- checked throughout instead of a separate boolean, so "is
%% discovery enabled" is one pattern match, not two fields kept in sync.
-record(discovery_state, {
    refresh_ms      :: pos_integer(),
    max_links       :: pos_integer(),
    timer           :: reference() | undefined,
    giveup_after_ms :: pos_integer(),
    giveup_sweep_ms :: pos_integer()
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
    node_id       :: <<_:256>>,
    %% The realm keys pinned at start, from the `realm_trust' option: realm id
    %% to the realm's key as carried.
    realm_keys = #{} :: #{<<_:256>> => binary()},
    link_opts     :: map(),
    replication   :: pos_integer(),
    dedup_sweep   :: pos_integer(),
    %% seed → link_state
    links = #{}   :: #{seed() => #link_state{}},
    %% Why each seed's link last went down. Outlives the link entry, which
    %% goes with the link, so the respawned link still reports it.
    last_disconnects = #{} :: #{seed() => last_disconnect()},
    %% pool-owned SubRef → sub_spec
    subs = #{}    :: #{reference() => #sub_spec{}},
    %% {realm, topic} → set of pool-owned SubRefs
    topic_index = #{} :: #{{<<_:256>>, binary()} => sets:set(reference())},
    %% The keys of `topic_index' whose topic has a `*' segment, with that
    %% topic's segments: an inbound event is matched against these alone,
    %% not against every topic in the index.
    wildcard_topics = #{} :: #{{<<_:256>>, binary()} => [binary()]},
    %% link pid → {realm, topic} → the SubRef that link returned for its
    %% SUBSCRIBE, from a subscribe or from the replay onto a respawned
    %% link, so an unsubscribe reaches every link that carried it.
    link_subs = #{} :: #{pid() => #{{<<_:256>>, binary()} => reference()}},
    %% Advertised procedures — pool replays these on link respawn.
    %% {realm, procedure} → handler
    procs = #{}   :: #{{<<_:256>>, binary()} => {handler(), auth_policy()}},
    %% Advertised streaming procedures — replayed on link respawn
    %% alongside `procs'. {realm, procedure} → {mode, handler, policy}
    stream_procs = #{} :: #{{<<_:256>>, binary()} =>
                            {macula_frame:stream_mode(),
                             stream_handler(), auth_policy()}},
    dedup_tab     :: ets:tid(),
    %% Per-`ordered'-subscription reorder-buffer timeout + count cap, and
    %% a lazily armed one-shot timer that fires to release timed-out gaps.
    order_timeout    :: non_neg_integer(),
    order_max_buffer :: pos_integer(),
    flush_timer      :: reference() | undefined,
    %% `undefined' == station discovery disabled for this pool (the
    %% default). See `#discovery_state{}' and the `station_discovery'
    %% opt.
    discovery        :: #discovery_state{} | undefined,
    link_selection   :: first_success | random,
    max_direct_links :: pos_integer(),
    %% New peers per `?NEW_PEER_WINDOW_MS', by normalized seed. The
    %% configured seeds are exempt.
    dial_budget      :: macula_client_peer_budget:t(),
    refused_dials    :: macula_refusal_report:t(),
    %% The node identity key, for the next issuer, and the pool's statement
    %% issuer, `undefined' while a new one waits for its backoff.
    node_identity    :: macula_node_keys:node_key(),
    issuer           :: pid() | undefined,
    issuer_started_at :: integer(),
    issuer_backoff_ms :: pos_integer(),
    %% How the pool starts an issuer, how many it has started after its first, and how many issuers it has lost.
    issuer_start     :: fun((fun(() -> macula_node_keys:node_key()), pid()) -> {ok, pid()} | {error, term()}),
    issuer_restarts = 0 :: non_neg_integer(),
    issuer_losses = 0 :: non_neg_integer(),
    %% Link starts that wait for the next issuer: seed → the start's extra
    %% options.
    held_starts = #{} :: #{seed() => map()},
    %% How each link was started beyond the pool's own options: seed → the
    %% start's extra options (`dial_fresh/3''s `ExtraOpts'). A respawn
    %% starts the link the same way, because a DIRECT DIAL's trust options
    %% live here and not in its seed: `macula:call_station/8' names the
    %% station as a URL binary and passes `expected_node_id' as an option,
    %% which `macula_station_link:add_tls_opts/2' folds into the seed at
    %% start. A respawn without them is refused
    %% (`seed_without_expected_node_id'), which loses the route rather than
    %% dialling anything unpinned. A configured seed MAP carries its own pin
    %% and does not depend on this.
    %%
    %% Kept at the pool rather than in `#link_state{}' because
    %% `on_down_routed/5' REMOVES the link entry, a whole
    %% ?LINK_RESPAWN_DELAY_MS before the respawn would read it back.
    %% Dropped only when a seed goes for good (`give_up_on/4').
    dial_extra_opts = #{} :: #{seed() => map()},
    %% The request admission in which all the pool's links judge the requests
    %% they receive.
    admission :: pid(),
    %% The station that last answered a procedure here, so direct dial can
    %% try it before asking the DHT again: {realm, procedure} -> the
    %% candidate and the monotonic millisecond it stops being usable.
    %%
    %% MONOTONIC, not a wall-clock expiry. The horizon is derived once from
    %% the advertisement's own expires_at at the moment it verified, and
    %% from then on only elapsed time is read, so a wall clock that steps
    %% between remembering and reading cannot lengthen or shorten an entry.
    %%
    %% An entry is a head start, never an answer: `resolved_candidate/3'
    %% hands one back only while the pool still holds a LIVE link to that
    %% station, and direct dial falls through to the DHT whenever it does
    %% not. Expired entries are dropped as new ones arrive, so the map
    %% holds about one entry per procedure this pool actually calls.
    resolved = #{} :: #{{<<_:256>>, binary()} =>
                        #{candidate := map(), until_mono := integer()}}
}).

%% The issuer restart backoff doubles from the least to the most, and
%% starts from the least again once an issuer has run for a minute.
-define(ISSUER_RESTART_MIN_MS, 100).
-define(ISSUER_RESTART_MAX_MS, 5_000).
-define(ISSUER_STABLE_MS, 60_000).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Spawn a pool with one link per seed. Returns immediately;
%% link handshakes complete asynchronously. Publish/subscribe block
%% until at least one link is connected (or fail with
%% `{error, {transient, no_healthy_station}}' on the publish path).
%% A node with no crypto profile, or a `node_identity' that is not an
%% identity key in the node's profile, starts no pool: the refusal is
%% returned and no link is dialed. Nor does a seed that names no node_id
%% it expects, in the seed or in the `expected_node_id' option: the start
%% returns `{error, {seeds, expected_node_id_required}}'. A `verify' key, in
%% the options or on a seed, returns `{error, {seeds, {verify,
%% one_verification_mode}}}': there is one verification mode and it is not
%% the caller's to pick.
-spec connect([seed()], opts()) -> {ok, pool()} | {error, term()}.
connect(Seeds, Opts) when is_list(Seeds), is_map(Opts) ->
    gen_server:start_link(?MODULE, {Seeds, identity_wrapped(Opts)}, []).

%% A supplied node identity key travels as a function that returns it, so
%% the pool's start arguments, and a supervisor's child spec, hold no key.
identity_wrapped(#{node_identity := Key} = Opts) when is_map(Key) ->
    Opts#{node_identity := fun() -> Key end};
identity_wrapped(Opts) ->
    Opts.

%% @doc Stop the pool. Every subscriber receives a final
%% `{macula_event_gone, SubRef, pool_closed}' message; every link
%% terminates with the pool.
-spec close(pool()) -> ok.
close(Pool) ->
    gen_server:stop(Pool, normal, 5_000).

%% @doc OTP child spec — drop the pool into a caller's supervision
%% tree. `Id' is the supervisor child id. A supervisor keeps the spec for
%% its child's life, so the spec names how to load the node identity key
%% and never holds the key: give `node_identity' as a loader
%% `{Module, Function, Args}' that returns `{ok, Key}'. `Args' say where the
%% key is, such as a file name, and never hold the key, because a supervisor
%% that fails to start the pool logs the spec, `Args' included. A key, or a
%% function that could hold one, given here raises
%% `{node_identity, loader_required}'.
-spec child_spec(term(), [seed()], opts()) -> supervisor:child_spec().
child_spec(_Id, _Seeds, #{node_identity := Given}) when is_map(Given); is_function(Given) ->
    erlang:error({node_identity, loader_required});
child_spec(Id, Seeds, Opts) ->
    #{id       => Id,
      %% ⚠ `macula:connect/2', NOT `?MODULE:connect/2'. This is the path
      %% the facade's own documentation tells production callers to use,
      %% so a check that lives on the facade must be on it. Pointing this
      %% at `?MODULE' bypassed every one of them. See macula#15.
      start    => {macula, connect, [Seeds, Opts]},
      restart  => permanent,
      shutdown => 5_000,
      type     => worker,
      modules  => [?MODULE]}.

%% @doc Issue a CALL for a procedure the pool's linked stations serve
%% themselves, such as `_dht.*': first success across the pool's healthy
%% links, each CALL targeting the station its link is connected to. It
%% moves on to the next link only when the CALL never went out on the one
%% before (macula_station_link:not_sent/1). Returns
%% `{error, no_healthy_station}' when no link has completed its
%% CONNECT/HELLO handshake. A procedure a provider serves is called through
%% `macula:call/5', which resolves the provider.
%%
%% Realm is per-call (32 bytes). `TimeoutMs' is from 1 ms to ten minutes,
%% the deadline window a provider accepts.
-spec call_linked_station(pool(), <<_:256>>, binary(), term(), 1..600_000) ->
    {ok, term()} | {error, term()}.
call_linked_station(Pool, Realm, Procedure, Payload, TimeoutMs)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       is_integer(TimeoutMs), TimeoutMs > 0, TimeoutMs =< 600_000 ->
    gen_server:call(Pool, {linked_station_call, Realm, Procedure, Payload, TimeoutMs},
                    TimeoutMs + 1_000).

%% @doc Pick one currently-connected link and return its pid, without
%% issuing a call. For a caller that needs to pin ONE link across a
%% sequence of related calls — a dedicated QUIC stream, opened once
%% on the returned pid (via the internal station-link module's
%% content-stream API), only isolates one link's traffic, so every
%% call in the sequence must go over that same link. `call_linked_station/5' picks
%% fresh per call (`call_first_success/5') and is the wrong primitive
%% for that.
%%
%% Selection matches `call_first_success/5''s ordering (first
%% connected link wins) so behaviour is unsurprising relative to the
%% existing pool-routed path.
-spec pick_connected_link(pool()) -> {ok, pid()} | {error, no_healthy_station}.
pick_connected_link(Pool) when is_pid(Pool) ->
    gen_server:call(Pool, pick_connected_link).

%% @doc As `pick_connected_link/1', but for a SPECIFIC station — reuse
%% a live link to `Station' or dial (and wait up to `TimeoutMs' for the
%% handshake on) a fresh one, naming the station it must prove through
%% `LinkOpts' (`expected_node_id', mirroring `call_station/9'). This is direct-dial's content-transfer primitive:
%% the returned pid is pinned for a whole `put_content'/`get_content'
%% dedicated-stream transfer exactly like `pick_connected_link/1', just
%% against a caller-resolved station instead of whichever pool link is
%% already up.
-spec ensure_station_link(pool(), seed(), map(), pos_integer()) ->
    {ok, pid()} | {error, term()}.
ensure_station_link(Pool, Station, LinkOpts, TimeoutMs)
  when is_pid(Pool), is_map(LinkOpts),
       is_integer(TimeoutMs), TimeoutMs > 0 ->
    gen_server:call(Pool, {ensure_station_link, Station, LinkOpts, TimeoutMs},
                    TimeoutMs + 2_000).

%% @doc Issue a CALL to `Target', a provider's node_id, at ONE specific
%% station, dialing it directly even if it is not in the pool's seed set.
%% `Station' is a seed URL (e.g. `<<"quic://[::1]:4433">>'). The pool
%% ensures a link to it (reusing an existing one, or dialing and monitoring
%% a new one exactly like a seed), waits for the handshake within the
%% deadline, and calls through that link; the station delivers the CALL to
%% the provider `Target' names. This is the direct-dial data path: resolve a
%% provider's serving_station to its endpoint, then reach it in one hop
%% here, with no mesh relay.
%%
%% Returns `{error, not_connected}' if the link does not complete its
%% handshake before the deadline.
-spec call_station(pool(), seed(), <<_:256>>, <<_:256>>, binary(), term(),
                   1..600_000) -> {ok, term()} | {error, term()}.
call_station(Pool, Station, Target, Realm, Procedure, Payload, TimeoutMs) ->
    call_station(Pool, Station, Target, Realm, Procedure, Payload, TimeoutMs, <<>>).

%% @doc As `call_station/7', presenting a capability token (UCAN) to a
%% gated provider. Empty token = none. Slice 7b.
-spec call_station(pool(), seed(), <<_:256>>, <<_:256>>, binary(), term(),
                   1..600_000, binary()) -> {ok, term()} | {error, term()}.
call_station(Pool, Station, Target, Realm, Procedure, Payload, TimeoutMs, UcanToken) ->
    call_station(Pool, Station, Target, Realm, Procedure, Payload, TimeoutMs,
                 UcanToken, #{}).

%% @doc As `call_station/8', naming in `LinkOpts' the station THIS dial
%% must prove: `expected_node_id'. `pin_tls_cert => true' and `verify' are
%% REFUSED, see `macula:call_station/8'. The pool's own `connect/2'-time
%% `expected_node_id' is fixed at connect time and applies to every link
%% the pool dials (seeds and every `call_station' target alike),
%% unworkable for direct-dial, whose whole point is reaching a station not
%% known until resolved at call time. This lets a direct-dial caller name
%% the node_id a signed DHT record just resolved, without changing the
%% pool's expectation for its other links. Only applies when a NEW link is
%% dialed for `Station': an already-connected link keeps the identity it
%% proved.
-spec call_station(pool(), seed(), <<_:256>>, <<_:256>>, binary(), term(),
                   1..600_000, binary(), map()) ->
    {ok, term()} | {error, term()}.
call_station(Pool, Station, Target, Realm, Procedure, Payload, TimeoutMs, UcanToken,
             LinkOpts) ->
    call_station(Pool, Station, Target, Realm, Procedure, Payload, TimeoutMs, UcanToken,
                 LinkOpts, TimeoutMs).

%% @doc As `call_station/9', waiting at most `DialTimeoutMs' of `TimeoutMs'
%% for a freshly-dialed link's handshake; the CALL gets whatever remains of
%% `TimeoutMs'. `{error, not_connected}' then comes back after
%% `DialTimeoutMs', before any CALL was sent, so a direct-dial caller can
%% move on to another station within its own deadline.
-spec call_station(pool(), seed(), <<_:256>>, <<_:256>>, binary(), term(),
                   1..600_000, binary(), map(), pos_integer()) ->
    {ok, term()} | {error, term()}.
call_station(Pool, Station, Target, Realm, Procedure, Payload, TimeoutMs, UcanToken,
             LinkOpts, DialTimeoutMs)
  when is_pid(Pool),
       is_binary(Target), byte_size(Target) =:= 32,
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       is_integer(TimeoutMs), TimeoutMs > 0, TimeoutMs =< 600_000,
       is_binary(UcanToken),
       is_map(LinkOpts),
       is_integer(DialTimeoutMs), DialTimeoutMs > 0 ->
    gen_server:call(Pool,
                    {call_station, Station, Target, Realm, Procedure, Payload,
                     TimeoutMs, DialTimeoutMs, UcanToken, LinkOpts},
                    TimeoutMs + 2_000).

%% @doc Register a procedure handler on every healthy link. Stored
%% in pool state so a respawned link registers it again. A caller
%% reaches this provider only through a `procedure_advertisement'
%% record that names it; registering the handler publishes none.
%% Returns `ok' when at least one link accepted the registration.
%% A handler that answers `{error, Text}' with a binary or a printable
%% charlist sends that text to its caller, up to 256 bytes of it; any
%% other error reason reaches the caller as its name only.
-spec advertise(pool(), <<_:256>>, binary(), handler()) ->
    ok | {error, term()}.
advertise(Pool, Realm, Procedure, Handler) ->
    advertise(Pool, Realm, Procedure, Handler, open).

%% @doc Advertise with an auth policy -- see `auth_policy()' above for
%% the full set (`open' | `{ucan_required, IssuerNodeId}' |
%% `{realm_member_required, RealmKeyId, RequiredCan}').
-spec advertise(pool(), <<_:256>>, binary(), handler(), auth_policy()) ->
    ok | {error, term()}.
advertise(Pool, Realm, Procedure, Handler, Policy)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       (is_function(Handler, 1) orelse
        (is_tuple(Handler) andalso tuple_size(Handler) =:= 2)) ->
    advertise(Pool, Realm, Procedure, Handler, Policy, undefined).

%% @doc As `advertise/5', with the provider advertisement (the resolved
%% D25 authorization included) fanned out to every link as an ADVERTISE
%% frame. `Ad' is an advertisement spec each link signs per send, naming
%% its own station (what the facade passes), or a pre-signed
%% advertisement's wire form every link sends as it is. The facade
%% resolves the authorization before calling here; `undefined'
%% registers the handler locally and sends no frame.
-spec advertise(pool(), <<_:256>>, binary(), handler(), auth_policy(),
                macula_station_link:advertisement() | undefined) ->
    ok | {error, term()}.
advertise(Pool, Realm, Procedure, Handler, Policy, EncodedAd)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       (is_function(Handler, 1) orelse
        (is_tuple(Handler) andalso tuple_size(Handler) =:= 2)),
       (is_binary(EncodedAd) orelse is_map(EncodedAd)
        orelse EncodedAd =:= undefined) ->
    gen_server:call(Pool, {advertise, Realm, Procedure, Handler, Policy,
                           EncodedAd},
                    5_000).

%% @doc Drop a previously-advertised procedure on every healthy link
%% and remove it from the pool's replay state. Idempotent.
-spec unadvertise(pool(), <<_:256>>, binary()) -> ok.
unadvertise(Pool, Realm, Procedure)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure) ->
    gen_server:call(Pool, {unadvertise, Realm, Procedure}, 5_000).

%% @doc Open a streaming RPC to `Target', a provider's node_id, by DIALING
%% a specific station directly (direct-dial). The streaming analogue of
%% `call_station/7': ensure (reuse or dial) a link to `Station', await the
%% handshake, then open the stream there, naming `Target'.
%% `Opts' may set `dial_timeout_ms' (default 10_000) for the dial and
%% handshake, plus any stream option (e.g. `mode').
%% `Opts' also names the station this dial must prove, `expected_node_id',
%% as `call_station/8' does. It is kept apart as the dial's own option, so
%% it reaches `ensure_link/3' and not the stream open.
-spec call_stream_station(pool(), seed(), <<_:256>>, <<_:256>>, binary(), term(),
                          map()) -> {ok, pid()} | {error, term()}.
call_stream_station(Pool, Station, Target, Realm, Procedure, Args, Opts)
  when is_pid(Pool),
       is_binary(Target), byte_size(Target) =:= 32,
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       is_map(Opts) ->
    DialTimeout = maps:get(dial_timeout_ms, Opts, 10_000),
    LinkOpts = maps:with([expected_node_id], Opts),
    gen_server:call(Pool,
                    {call_stream_station, Station, Target, Realm, Procedure, Args,
                     Opts#{owner => maps:get(owner, Opts, self())}, LinkOpts},
                    DialTimeout + 2_000).

%% @doc Advertise a streaming procedure handler on every healthy
%% link. Stored in pool state so links respawned later replay the
%% advertisement. Returns `ok' when at least one link accepted the
%% registration. Same as `advertise_stream/6' with policy `open'.
-spec advertise_stream(pool(), <<_:256>>, binary(),
                        macula_frame:stream_mode(),
                        stream_handler()) ->
    ok | {error, term()}.
advertise_stream(Pool, Realm, Procedure, Mode, Handler) ->
    advertise_stream(Pool, Realm, Procedure, Mode, Handler, open).

%% @doc Advertise a streaming procedure with an auth policy -- the same
%% `auth_policy()' set `advertise/5' takes. The policy is stored with the
%% procedure, so a link respawned later re-advertises it still gated.
-spec advertise_stream(pool(), <<_:256>>, binary(),
                        macula_frame:stream_mode(),
                        stream_handler(), auth_policy()) ->
    ok | {error, term()}.
advertise_stream(Pool, Realm, Procedure, Mode, Handler, Policy)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       (Mode =:= server_stream orelse Mode =:= client_stream
        orelse Mode =:= bidi),
       is_function(Handler, 2) ->
    advertise_stream(Pool, Realm, Procedure, Mode, Handler, Policy,
                     undefined).

%% @doc As `advertise_stream/6', with the provider advertisement fanned
%% out to every link as an ADVERTISE frame: a spec each link signs
%% naming its own station, or a pre-signed wire form (see
%% `advertise/6'). `undefined' registers the handler locally and sends
%% no frame.
-spec advertise_stream(pool(), <<_:256>>, binary(),
                       macula_frame:stream_mode(),
                       stream_handler(), auth_policy(),
                       macula_station_link:advertisement() | undefined) ->
    ok | {error, term()}.
advertise_stream(Pool, Realm, Procedure, Mode, Handler, Policy, EncodedAd)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       (Mode =:= server_stream orelse Mode =:= client_stream
        orelse Mode =:= bidi),
       is_function(Handler, 2),
       (is_binary(EncodedAd) orelse is_map(EncodedAd)
        orelse EncodedAd =:= undefined) ->
    gen_server:call(Pool,
                    {advertise_stream, Realm, Procedure, Mode, Handler,
                     Policy, EncodedAd},
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

%% @doc The realm key the pool pinned for `RealmId' when it started, from its `realm_trust' option, or `none'. Direct
%% dial checks an org namespaced advertisement's authorization against this key alone.
-spec realm_key(pool(), <<_:256>>) -> {ok, binary()} | none.
realm_key(Pool, <<_:256>> = RealmId) when is_pid(Pool) ->
    gen_server:call(Pool, {realm_key, RealmId}, 5_000).

%% @doc The station that last answered `Procedure' in `RealmId' from this
%% pool, as a direct-dial candidate plus the seed the pool's own live link
%% to that station is keyed by, or `none'.
%%
%% A HEAD START, NEVER AN ANSWER. It is handed back only while three things
%% hold together: an entry was remembered, its horizon has not elapsed, and
%% the pool STILL HOLDS A LIVE LINK to that station. The live link is the
%% evidence: a link either exists or it does not, so unlike a signed
%% endpoint record up to five minutes old it cannot be stale. Direct dial
%% resolves through the DHT exactly as before whenever this answers `none',
%% and also whenever the candidate it returns fails.
%%
%% ⚠ THE LINK IS MATCHED BY NODE ID, NOT BY NAME. `find_seed_by_node_id/2'
%% asks each live link who its handshake peer is, so the identity a
%% direct-dial `expected_node_id' pin would have checked has already been
%% checked here, before the seed is named. Naming the link afterwards by the
%% key the pool itself holds it under is what makes `ensure_link/3' match it
%% on its first lookup instead of dialling; the name is how the call REACHES
%% the link, and the node id is what makes it the RIGHT one.
-spec resolved_candidate(pool(), <<_:256>>, binary()) ->
    {ok, map(), seed()} | none.
resolved_candidate(Pool, <<_:256>> = RealmId, Procedure)
  when is_pid(Pool), is_binary(Procedure) ->
    gen_server:call(Pool, {resolved_candidate, RealmId, Procedure}, 5_000).

%% @doc Remember `Candidate' as the station that answered `Procedure' in
%% `RealmId', usable for `TtlMs' more milliseconds.
%%
%% A cast, because remembering must not cost the call that earned it any
%% latency, and because losing one is only a lost head start.
-spec remember_resolved(pool(), <<_:256>>, binary(), map(), non_neg_integer()) -> ok.
remember_resolved(Pool, <<_:256>> = RealmId, Procedure, Candidate, TtlMs)
  when is_pid(Pool), is_binary(Procedure), is_map(Candidate),
       is_integer(TtlMs), TtlMs >= 0 ->
    gen_server:cast(Pool, {remember_resolved, RealmId, Procedure, Candidate, TtlMs}).

%% @doc Sign a record this node signs about itself with the pool's node identity key, in the pool's own process, and
%% return the signed record: the node record, a procedure advertisement or a content announcement that names this node.
%% The pool stamps it with a new version and created_at, keeping the lifetime it was built with. The key never leaves
%% the pool, so a caller never holds it. Only the record's type, created_at, expires_at and payload reach the pool. A
%% record of another type, a tombstone included, is `{error, not_a_node_signed_type}'; one that names another node
%% `{error, key_id_mismatch}'; one whose lifetime passes its type's maximum or runs backwards
%% `{error, lifetime_too_long}' or `{error, lifetime_reversed}'; a payload over 256 KiB, refused before the call, or a
%% signed record that would pass 256 KiB `{error, record_too_large}'; a record with a subject, which no type a node
%% signs about itself carries, refused before the call, and anything else the pool cannot sign
%% `{error, malformed_record}'.
-spec sign_node_record(pool(), macula_record:m_record()) ->
          {ok, macula_record:m_record()}
        | {error, not_a_node_signed_type | key_id_mismatch | lifetime_too_long | lifetime_reversed | record_too_large
                | malformed_record}.
sign_node_record(Pool, #{type := Type, created_at := Created, expires_at := Expires, payload := Payload} = Record)
  when is_pid(Pool), is_integer(Type), is_integer(Created), is_integer(Expires), is_map(Payload),
       not is_map_key(subject, Record) ->
    pool_signs(macula_record:payload_bounded(Payload), Pool,
               {sign_node_record, #{type => Type, created_at => Created, expires_at => Expires, payload => Payload}});
sign_node_record(Pool, _NotARecord) when is_pid(Pool) ->
    {error, malformed_record}.

%% @doc As `sign_node_record/2', bounded by `Opts' `not_after' (a Unix
%% millisecond). The pool judges the bound on its own clock: one already
%% passed is `{error, not_after_passed}'; one before the record's
%% lifetime runs out ends the record at the bound; one after keeps the
%% built lifetime. The refusals of `sign_node_record/2' stand under a
%% bound. `Opts' without `not_after' signs as `sign_node_record/2' does,
%% and a `not_after' that is not an integer raises `function_clause'
%% in the caller.
-spec sign_node_record(pool(), macula_record:m_record(), map()) ->
          {ok, macula_record:m_record()}
        | {error, not_a_node_signed_type | key_id_mismatch | lifetime_too_long | lifetime_reversed | record_too_large
                | malformed_record | not_after_passed}.
sign_node_record(Pool,
                 #{type := Type, created_at := Created, expires_at := Expires, payload := Payload} = Record,
                 #{not_after := NotAfter})
  when is_pid(Pool), is_integer(Type), is_integer(Created), is_integer(Expires), is_map(Payload),
       not is_map_key(subject, Record), is_integer(NotAfter) ->
    pool_signs(macula_record:payload_bounded(Payload), Pool,
               {sign_node_record_bounded, #{type => Type, created_at => Created, expires_at => Expires,
                                            payload => Payload, not_after => NotAfter}});
sign_node_record(Pool, _NotARecord, #{not_after := NotAfter})
  when is_pid(Pool), is_integer(NotAfter) ->
    {error, malformed_record};
sign_node_record(Pool, Record, Opts)
  when is_pid(Pool), is_map(Opts), not is_map_key(not_after, Opts) ->
    sign_node_record(Pool, Record).

%% @doc Sign a domain record (tags 0x20 to 0xFF) as this node, with the pool's node identity key, in the pool's own
%% process, and return the signed record, stored under this node's key id with its subject when it has one. Build it
%% with `macula_record:envelope/3'. The pool stamps it with a new version and created_at, keeping the lifetime it was
%% built with. Only the record's type, created_at, expires_at, payload and subject reach the pool, and each of these
%% is refused before the call: a type outside 0x20 to 0xFF, `{error, not_a_domain_type}'; a subject that is not a
%% non-empty binary, `{error, invalid_subject}'; a lifetime past the domain maximum of 7 days, or running backwards,
%% `{error, lifetime_too_long}' or `{error, lifetime_reversed}', never shortened to fit; a payload and subject over
%% 256 KiB together, `{error, record_too_large}'; and a term that is no domain record, `{error, malformed_record}'. A
%% signed record that would pass 256 KiB is `{error, record_too_large}' from the pool. Withdraw a domain record with
%% withdraw_node_record/3.
-spec sign_domain_record(pool(), macula_record:m_record()) ->
          {ok, macula_record:m_record()}
        | {error, not_a_domain_type | invalid_subject | lifetime_too_long | lifetime_reversed | record_too_large
                | malformed_record}.
sign_domain_record(Pool, Record) when is_pid(Pool) ->
    domain_record_sent(macula_record:domain_record_checked(Record), Pool, Record).

%% Only a domain record that passes its checks reaches the pool, and of it only the fields the pool signs from.
domain_record_sent(ok, Pool, Record) ->
    Fields = maps:with([type, created_at, expires_at, payload, subject], Record),
    gen_server:call(Pool, {sign_domain_record, Fields}, 5_000);
domain_record_sent({error, malformed}, _Pool, _Record) ->
    {error, malformed_record};
domain_record_sent({error, _} = Refusal, _Pool, _Record) ->
    Refusal.

%% @doc Sign a tombstone that withdraws a record this node signed, with the pool's node identity key, in the pool's own
%% process. The pool first verifies the record, as its wire form or its signed map, under its profile, and withdraws it
%% only when it is of a type a node signs about itself or a domain type, and it carries the pool's own key, so the
%% tombstone lands on the record's own slot. A record that does not verify gets its refusal; one of another type
%% `{error, not_a_node_signed_type}'; another node's
%% `{error, not_this_nodes_record}'; a wire form over 256 KiB, or a signed map whose key, tbs and signature pass 256 KiB
%% together, `{error, record_too_large}', refused before the call; and anything else the pool cannot sign, a map whose
%% key, tbs or signature is not a binary included, `{error, malformed_record}'. Of a signed map, only its key, tbs and
%% signature reach the pool. The tombstone lives until the record has expired plus the clock tolerance.
-spec withdraw_node_record(pool(), macula_record:m_record() | binary(), macula_record:reason()) ->
          {ok, macula_record:m_record()}
        | {error, not_this_nodes_record | not_a_node_signed_type | lifetime_too_long | lifetime_reversed
                | record_too_large | malformed_record | macula_record:refusal()}.
withdraw_node_record(Pool, Withdrawn, Reason)
  when is_pid(Pool), (Reason =:= shutdown orelse Reason =:= moved orelse Reason =:= revoked) ->
    pool_withdraws(macula_record:wire_bounded(Withdrawn), Pool, Withdrawn, Reason).

%% A payload past the record bounds never reaches the pool.
pool_signs(ok, Pool, Request) -> gen_server:call(Pool, Request, 5_000);
pool_signs({error, record_too_large}, _Pool, _Request) -> {error, record_too_large};
pool_signs({error, malformed}, _Pool, _Request) -> {error, malformed_record}.

%% A record to withdraw reaches the pool only as a wire form within the record bound, or as a signed map's key, tbs and
%% signature within it.
pool_withdraws(ok, Pool, Withdrawn, Reason) ->
    gen_server:call(Pool, {withdraw_node_record, wire_fields(Withdrawn), Reason}, 5_000);
pool_withdraws({error, record_too_large}, _Pool, _Withdrawn, _Reason) -> {error, record_too_large};
pool_withdraws({error, malformed}, _Pool, _Withdrawn, _Reason) -> {error, malformed_record}.

wire_fields(Bytes) when is_binary(Bytes) -> Bytes;
wire_fields(Record) -> maps:with([key, tbs, signature], Record).

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

%% @doc Publish a frame to `replication_factor' currently-connected
%% links. Partial success = success. Realm is per-call (32 bytes) and
%% identical across every replicated link — a wrong `Realm' here
%% blackholes the publish on every selected station alike, regardless
%% of `replication_factor'; see `?DEFAULT_REPLICATION''s own doc for
%% what raising the factor does and does not protect against.
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
%% A topic over 512 bytes or not UTF-8 is refused first, as
%% `{error, {text_too_long, topic}}' or `{error, {invalid_text, topic}}'.
-spec publish(pool(), <<_:256>>, binary(), term(), map()) ->
    ok | {error, term()}.
publish(Pool, Realm, Topic, Payload, Opts)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Topic),
       is_map(Opts) ->
    publish_checked(publishable(macula_frame:text_checked(topic, Topic), Payload),
                    Pool, Realm, Topic, Payload, Opts).

%% A topic a PUBLISH cannot carry is refused before the payload is looked at.
publishable(ok, Payload) -> macula_frame:check_payload(Payload);
publishable({error, _} = Refused, _Payload) -> Refused.

publish_checked(ok, Pool, Realm, Topic, Payload, Opts) ->
    Timeout = maps:get(timeout_ms, Opts, 5_000),
    gen_server:call(Pool, {publish, Realm, Topic, Payload, Opts},
                    Timeout + 500);
publish_checked({error, _} = Rejected, _Pool, _Realm, _Topic, _Payload, _Opts) ->
    Rejected.

%% @doc Subscribe `Subscriber' to `(Realm, Topic)'. The pool
%% subscribes every currently-spawned link and dedupes inbound
%% events before fan-out. Returns `{ok, SubRef}', or the topic's refusal
%% when a SUBSCRIBE cannot carry it (over 512 bytes, or not UTF-8); `Subscriber'
%% receives `{macula_event, SubRef, Topic, Payload, Meta}' for each
%% delivered event and `{macula_event_gone, SubRef, Reason}' once
%% when the pool closes or the subscriber pid dies.
-spec subscribe(pool(), <<_:256>>, binary(), pid(), map()) ->
    {ok, reference()} | {error, {text_too_long | invalid_text, topic}}.
subscribe(Pool, Realm, Topic, Subscriber, Opts)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Topic), is_pid(Subscriber),
       is_map(Opts) ->
    subscribed(macula_frame:text_checked(topic, Topic), Pool, Realm, Topic, Subscriber, Opts).

%% A topic a SUBSCRIBE cannot carry is refused before the pool or its links build anything.
subscribed(ok, Pool, Realm, Topic, Subscriber, Opts) ->
    gen_server:call(Pool, {subscribe, Realm, Topic, Subscriber, Opts}, 5_000);
subscribed({error, _} = Refused, _Pool, _Realm, _Topic, _Subscriber, _Opts) ->
    Refused.

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
    init_within_realm_trust(realm_trust_refusal(Opts), Seeds, Opts).

%% A realm trust that is not a map of 32-byte realm ids to realm keys well formed for the node's crypto profile does
%% not start the pool, loads no key and dials nothing: a key that can never equal an org directory's signer would leave
%% every org namespaced advertisement of its realm untrusted without saying why. A key well formed for the other
%% profile is refused by its own name.
init_within_realm_trust(none, Seeds, Opts) ->
    init_within_link_limits(link_limit_outside_its_range(Opts), Seeds, Opts);
init_within_realm_trust(Refusal, _Seeds, _Opts) ->
    {error, Refusal}.

realm_trust_refusal(#{realm_trust := Trust}) when is_map(Trust) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    first_outside([Refusal || Id := Key <- Trust,
                              Refusal <- [realm_key_refusal(Id, Key, Profile)], Refusal =/= none]);
realm_trust_refusal(#{realm_trust := _NotAMap}) ->
    {realm_trust, invalid};
realm_trust_refusal(_NoRealmTrust) ->
    none.

realm_key_refusal(<<_:256>>, Key, Profile) when is_binary(Key) ->
    realm_key_formed(macula_node_keys:carried_key_well_formed(Key, Profile), Key, Profile);
realm_key_refusal(_Id, _Key, _Profile) ->
    {realm_trust, invalid}.

realm_key_formed(true, _Key, _Profile) ->
    none;
realm_key_formed(false, Key, Profile) ->
    other_profile(lists:any(fun(Candidate) -> macula_node_keys:carried_key_well_formed(Key, Candidate) end,
                            [Other || Other <- [pq_pure, pq_hybrid], Other =/= Profile])).

other_profile(true)  -> {realm_trust, profile_mismatch};
other_profile(false) -> {realm_trust, invalid}.

pinned_realm_key({ok, Key}) -> {ok, Key};
pinned_realm_key(error)     -> none.

%% A link limit that is not an integer from 1 to its cap does not start the
%% pool, and nothing is dialed: an atom would otherwise sort above every
%% integer and lift its bound.
init_within_link_limits(none, Seeds, Opts) ->
    init_within_admission_limits(admission_limits_refusal(admission_given(Opts)), Seeds, Opts);
init_within_link_limits({Key, Value}, _Seeds, _Opts) ->
    {error, {invalid_link_limit, Key, Value}}.

%% Request admission limits outside their ranges, or out of order, do not
%% start the pool, and nothing is dialed.
init_within_admission_limits(none, Seeds, Opts) ->
    init_within_seed_limit(length(Seeds), maps:get(max_seeds, Opts, ?DEFAULT_MAX_SEEDS), Seeds, Opts);
init_within_admission_limits(Refusal, _Seeds, _Opts) ->
    {error, Refusal}.

%% The request admission limits a pool is given, key by key: its
%% `request_admission' option over the `macula' application environment's
%% `request_admission'. A value that is not a map is refused as a limit of
%% that name.
admission_given(Opts) ->
    maps:merge(given_limits(application:get_env(macula, request_admission, #{})),
               given_limits(maps:get(request_admission, Opts, #{}))).

given_limits(#{} = Given) -> Given;
given_limits(NotAMap)     -> #{request_admission => NotAMap}.

%% A given key the admission does not take, or a value that is not an integer
%% from 1 to its cap, is refused by name. So is a quota per caller above the
%% share, or stored reply bytes per caller above the total, since the smaller
%% limit would then never bind.
admission_limits_refusal(Given) ->
    admission_range_refusal(first_outside([{Key, Value} || Key := Value <- Given,
                                                           not admission_limit_in_range(Key, Value)]),
                            maps:merge(?DEFAULT_ADMISSION_LIMITS, Given)).

admission_range_refusal(none, Limits)          -> admission_order_refusal(Limits);
admission_range_refusal({Key, Value}, _Limits) -> {invalid_admission_limit, Key, Value}.

admission_order_refusal(#{caller_quota := Quota, share := Share}) when Quota > Share ->
    {admission_limit_above, caller_quota, share};
admission_order_refusal(#{reply_bytes := Bytes, reply_bytes_total := Total}) when Bytes > Total ->
    {admission_limit_above, reply_bytes, reply_bytes_total};
admission_order_refusal(_InOrder) ->
    none.

admission_limit_in_range(Key, Value) ->
    is_map_key(Key, ?ADMISSION_LIMIT_CAPS) andalso in_link_range(Value, maps:get(Key, ?ADMISSION_LIMIT_CAPS)).

%% The limits the pool's admission starts with: the given limits over the
%% defaults, and a cap of the share times the pool's link limits summed, the
%% most distinct shares one entry lifetime can see.
admission_start_limits(Opts) ->
    #{share := Share} = Limits = maps:merge(?DEFAULT_ADMISSION_LIMITS, admission_given(Opts)),
    Limits#{cap => Share * lists:sum([Value || {_Key, Value, _Cap} <- link_limits(Opts)])}.

link_limit_outside_its_range(Opts) ->
    first_outside([{Key, Value} || {Key, Value, Cap} <- link_limits(Opts), not in_link_range(Value, Cap)]).

link_limits(Opts) ->
    [{max_seeds, maps:get(max_seeds, Opts, ?DEFAULT_MAX_SEEDS), ?MAX_SEEDS_CAP},
     {max_direct_links, maps:get(max_direct_links, Opts, ?DEFAULT_MAX_DIRECT_LINKS), ?MAX_DIRECT_LINKS_CAP},
     {new_peer_budget, maps:get(new_peer_budget, Opts, ?DEFAULT_NEW_PEER_BUDGET), ?NEW_PEER_BUDGET_CAP},
     {max_links, discovery_max_links(maps:get(station_discovery, Opts, #{})), ?DISCOVERY_MAX_LINKS_CAP}].

discovery_max_links(#{} = Discovery) -> maps:get(max_links, Discovery, ?DEFAULT_DISCOVERY_MAX_LINKS);
discovery_max_links(_NotAMap)        -> ?DEFAULT_DISCOVERY_MAX_LINKS.

in_link_range(Value, Cap) ->
    is_integer(Value) andalso Value >= 1 andalso Value =< Cap.

first_outside([])            -> none;
first_outside([Outside | _]) -> Outside.

%% A pool given more seeds than its limit does not start, and dials nothing.
init_within_seed_limit(Given, Max, Seeds, Opts) when Given =< Max ->
    init_without_verify(names_verify(Seeds, Opts), Seeds, Opts);
init_within_seed_limit(Given, Max, _Seeds, _Opts) ->
    {error, {too_many_seeds, Given, Max}}.

%% A pool asked for a TLS verification mode does not start. There is one
%% (`macula_quic:connect/4'), so a `verify' key names a check that cannot
%% run, in the options or on any seed. `macula:connect/2' refuses it before
%% this, and this is what a caller of `macula_client:connect/2' directly, as
%% the services do, hears instead of silence.
init_without_verify(false, Seeds, Opts) ->
    init_with_pinned_seeds(lists:all(fun(Seed) -> pinned_seed(Seed, Opts) end, Seeds), Seeds, Opts);
init_without_verify(true, _Seeds, _Opts) ->
    {error, {seeds, {verify, one_verification_mode}}}.

names_verify(Seeds, Opts) ->
    lists:any(fun(Map) -> is_map(Map) andalso is_map_key(verify, Map) end, [Opts | Seeds]).

%% A pool given a seed that names no node_id it expects does not start, loads no key and dials nothing: a link would
%% refuse that seed at every start, and the pool would look ready with nothing it could reach. A seed map's own
%% expected_node_id stands over the pool's option, as a link reads it.
init_with_pinned_seeds(true, Seeds, Opts) ->
    init_with_keys(pool_keys(Opts), Seeds, Opts);
init_with_pinned_seeds(false, _Seeds, _Opts) ->
    {error, {seeds, expected_node_id_required}}.

pinned_seed(#{expected_node_id := NodeId}, _Opts) -> node_id_sized(NodeId);
pinned_seed(_Seed, #{expected_node_id := NodeId}) -> node_id_sized(NodeId);
pinned_seed(_Seed, _Opts) -> false.

node_id_sized(NodeId) -> is_binary(NodeId) andalso byte_size(NodeId) =:= 32.

%% A pool whose keys cannot be had does not start: `connect/2' returns
%% the refusal and no link is dialed.
init_with_keys({error, _} = Refusal, _Seeds, _Opts) ->
    Refusal;
init_with_keys({ok, #{node_identity := NodeIdentity, issuer := Issuer, issuer_start := Start} = Keys}, Seeds, Opts) ->
    {ok, NodeId} = macula_node_keys:node_id(NodeIdentity),
    _ = erlang:monitor(process, Issuer),
    %% One request admission for every link. The pool ends when it ends, and
    %% ends it in terminate.
    {ok, Admission} = macula_request_admission:start_link(admission_start_limits(Opts)),
    %% No node identity key in the link options: each link start gets a
    %% function that returns it, made at that start, so the pool's state
    %% holds no function over the key that redaction cannot see into.
    LinkOpts = maps:merge(
        (maps:without([node_identity, issuer_start], Keys))#{
            capabilities       => maps:get(capabilities, Opts, 0),
            alpn               => maps:get(alpn, Opts, [<<"macula">>]),
            connect_timeout_ms => maps:get(connect_timeout_ms, Opts, 30_000),
            admission          => Admission,
            %% Every link tells this pool why it disconnected before it stops.
            pool               => self()
        },
        %% For the links this pool dials (seeds AND `call_station'
        %% targets): `expected_node_id', the station node_id the
        %% handshake must prove. Forwarded only when the caller set it.
        %%
        %% `pin_tls_cert' and `verify' are NOT among them: both are
        %% refused; see `macula:trust_options_checked/1'.
        maps:with([expected_node_id], Opts)),
    DedupSweep  = maps:get(dedup_sweep_ms, Opts, ?DEFAULT_DEDUP_SWEEP_MS),
    Replication = maps:get(replication_factor, Opts, ?DEFAULT_REPLICATION),
    DedupTab    = macula_client_dedup:new(),
    OrderTimeout = maps:get(order_timeout_ms, Opts, ?DEFAULT_ORDER_TIMEOUT_MS),
    OrderMaxBuf  = maps:get(order_max_buffer, Opts, ?DEFAULT_ORDER_MAX_BUFFER),
    Discovery = init_discovery(maps:get(station_discovery, Opts, #{})),
    LinkSelection = maps:get(link_selection, Opts, default_link_selection(Discovery)),
    State0 = #state{seeds = Seeds, node_id = NodeId, realm_keys = maps:get(realm_trust, Opts, #{}),
                    link_opts = LinkOpts, replication = Replication,
                    dedup_sweep = DedupSweep,
                    dedup_tab = DedupTab,
                    order_timeout = OrderTimeout, order_max_buffer = OrderMaxBuf,
                    flush_timer = undefined,
                    discovery = Discovery, link_selection = LinkSelection,
                    max_direct_links = maps:get(max_direct_links, Opts, ?DEFAULT_MAX_DIRECT_LINKS),
                    dial_budget = macula_client_peer_budget:new(
                                    #{budget => maps:get(new_peer_budget, Opts, ?DEFAULT_NEW_PEER_BUDGET),
                                      window_ms => ?NEW_PEER_WINDOW_MS,
                                      exempt => [seed_peer(Seed) || Seed <- Seeds]}),
                    refused_dials = macula_refusal_report:new(?REFUSAL_REPORT_WINDOW_MS),
                    node_identity = NodeIdentity, issuer = Issuer, issuer_started_at = now_ms(),
                    issuer_backoff_ms = ?ISSUER_RESTART_MIN_MS, issuer_start = Start,
                    admission = Admission},
    State1 = lists:foldl(fun start_link_for_seed/2, State0, Seeds),
    erlang:send_after(DedupSweep, self(), dedup_sweep),
    arm_giveup_sweep(Discovery),
    {ok, schedule_discovery(?INITIAL_DISCOVERY_DELAY_MS, State1)}.

%% Self-rearming, like `dedup_sweep' -- unlike `run_discovery''s own
%% timer, this one is never opportunistically re-triggered elsewhere,
%% so (unlike `schedule_discovery/2') there is nothing to cancel and
%% no risk of two chains running side by side.
arm_giveup_sweep(undefined) ->
    ok;
arm_giveup_sweep(#discovery_state{giveup_sweep_ms = Ms}) ->
    erlang:send_after(Ms, self(), discovery_giveup_sweep),
    ok.

init_discovery(#{enabled := true} = Opts) ->
    #discovery_state{
        refresh_ms      = maps:get(refresh_ms, Opts, ?DEFAULT_DISCOVERY_REFRESH_MS),
        max_links       = maps:get(max_links, Opts, ?DEFAULT_DISCOVERY_MAX_LINKS),
        timer           = undefined,
        giveup_after_ms = maps:get(giveup_after_ms, Opts, ?DEFAULT_DISCOVERY_GIVEUP_MS),
        giveup_sweep_ms = maps:get(giveup_sweep_ms, Opts, ?DEFAULT_DISCOVERY_GIVEUP_SWEEP_MS)
    };
init_discovery(_NotEnabled) ->
    undefined.

default_link_selection(undefined)          -> first_success;
default_link_selection(#discovery_state{}) -> random.

schedule_discovery(_DelayMs, #state{discovery = undefined} = S) ->
    S;
schedule_discovery(DelayMs, #state{discovery = D} = S) ->
    %% Cancel any timer already pending -- unlike `dedup_sweep' (which
    %% only ever reschedules itself, so it never has more than one
    %% pending timer at a time), this is also armed opportunistically by
    %% `maybe_rediscover_now/1', so without cancelling here two chains
    %% can end up running side by side forever, each rescheduling itself
    %% on every fire.
    cancel_discovery_timer(D#discovery_state.timer),
    Timer = erlang:send_after(DelayMs, self(), run_discovery),
    S#state{discovery = D#discovery_state{timer = Timer}}.

cancel_discovery_timer(undefined) -> ok;
cancel_discovery_timer(Timer)     -> erlang:cancel_timer(Timer).

handle_call({publish, Realm, Topic, Payload, _Opts}, From, S) ->
    %% Publish only to links that have completed CONNECT/HELLO. A
    %% frame sent to a still-handshaking link is dropped on the floor, so
    %% selecting the first `replication' *spawned* links could report
    %% `{error, not_connected}' while other links are healthy. RPC and
    %% streams already filter by `is_connected/1'; publish must too.
    %%
    %% Dispatch via a one-shot worker so concurrent publishes don't
    %% serialise through this gen_server (the per-link `publish/4'
    %% calls are sync 5s timeouts; under load they pile up at the pool
    %% and the harness's `multi_publisher_pubsub' case fails with
    %% empty receives).
    Targets = ordered_for_selection(connected_link_pids(S), S#state.link_selection),
    Selected = select_publish_targets(Targets, S#state.replication),
    AllTargets = Targets,
    %% One seq per publication, from the node's counter for the pool's key
    %% (macula_publication_seq), reused across every replicated link.
    Seq = macula_publication_seq:next(S#state.node_id),
    _ = spawn(fun() ->
        Results = [safe_link_publish(P, Realm, Topic, Payload, Seq)
                   || P <- Selected],
        gen_server:reply(From, summarize_publish(Results, AllTargets))
    end),
    {noreply, S};

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
    {reply, {ok, SubRef}, issue_wire_subs(AlreadyTracked, Realm, Topic, NewS)};

handle_call({unsubscribe, SubRef}, _From, S) ->
    {reply, ok, drop_sub(SubRef, S)};

handle_call(pick_connected_link, _From, S) ->
    {reply, first_connected_link(ordered_for_selection(connected_link_pids(S),
                                                       S#state.link_selection)), S};

handle_call({linked_station_call, Realm, Procedure, Payload, TimeoutMs}, From, S) ->
    %% Worker-spawn so concurrent CALLs don't serialise through the
    %% pool gen_server. Each per-link `macula_station_link:call/6'
    %% is a sync gen_server:call to the link; with the old
    %% `{reply, ..., S}' shape every caller blocked the pool until
    %% the link replied, capping concurrent CALL throughput at 1.
    Pids = ordered_for_selection(spawned_link_pids(S), S#state.link_selection),
    _ = spawn(fun() ->
        Reply = call_first_success(Pids, Realm, Procedure, Payload,
                                    TimeoutMs),
        gen_server:reply(From, Reply)
    end),
    {noreply, S};

handle_call({call_station, Station, Target, Realm, Procedure, Payload, TimeoutMs,
             DialTimeoutMs, Ucan, LinkOpts}, From, S) ->
    %% Ensure (reuse or dial) a link to the specific station, then hand
    %% the wait-for-handshake + call to a worker so the pool gen_server
    %% is never blocked (same rationale as a linked station call).
    on_link(ensure_link(Station, LinkOpts, S), From,
            fun(Pid) ->
                call_when_connected(Pid, Target, Realm, Procedure, Payload, TimeoutMs, DialTimeoutMs, Ucan)
            end);

handle_call({ensure_station_link, Station, LinkOpts, TimeoutMs}, From, S) ->
    %% Same shape as call_station: ensure the link, wait for its
    %% handshake in a worker so the pool never blocks. Unlike
    %% call_station this hands back the connected pid itself rather
    %% than making a call over it — the caller opens a dedicated
    %% content stream on it directly (see macula:with_content_stream/2).
    on_link(ensure_link(Station, LinkOpts, S), From,
            fun(Pid) -> content_link_when_connected(Pid, TimeoutMs) end);

handle_call({advertise, Realm, Procedure, Handler, Policy}, _From,
            #state{procs = P} = S) ->
    Pids = spawned_link_pids(S),
    Reply = fanout_advertise(Pids, Realm, Procedure, Handler, Policy,
                             undefined),
    {reply, Reply,
     S#state{procs = P#{{Realm, Procedure} =>
                        {Handler, Policy, undefined}}}};
handle_call({advertise, Realm, Procedure, Handler, Policy, EncodedAd}, _From,
            #state{procs = P} = S) ->
    Pids = spawned_link_pids(S),
    Reply = fanout_advertise(Pids, Realm, Procedure, Handler, Policy,
                             EncodedAd),
    {reply, Reply,
     S#state{procs = P#{{Realm, Procedure} =>
                        {Handler, Policy, EncodedAd}}}};

handle_call({unadvertise, Realm, Procedure}, _From,
            #state{procs = P, node_identity = Key} = S) ->
    Withdrawal = withdrawal_for(maps:find({Realm, Procedure}, P),
                                Realm, Procedure, Key),
    _ = fanout_unadvertise(spawned_link_pids(S), Realm, Procedure,
                           Withdrawal),
    {reply, ok, S#state{procs = maps:remove({Realm, Procedure}, P)}};

handle_call({call_stream_station, Station, Target, Realm, Procedure, Args, Opts,
             LinkOpts}, From, S) ->
    %% Direct-dial streaming: ensure (reuse or dial) a link to the
    %% specific station, then open the stream there. Same worker-spawn
    %% rationale as call_station — the pool gen_server never blocks on
    %% the dial + handshake. LinkOpts (expected_node_id) shapes a fresh
    %% dial only, same as call_station.
    on_link(ensure_link(Station, LinkOpts, S), From,
            fun(Pid) -> stream_when_connected(Pid, Target, Realm, Procedure, Args, Opts) end);

handle_call({advertise_stream, Realm, Procedure, Mode, Handler, Policy}, _From,
            #state{stream_procs = SP} = S) ->
    Pids = spawned_link_pids(S),
    Reply = fanout_advertise_stream(Pids, Realm, Procedure, Mode, Handler,
                                    Policy, undefined),
    {reply, Reply,
     S#state{stream_procs = SP#{{Realm, Procedure} =>
                                {Mode, Handler, Policy, undefined}}}};
handle_call({advertise_stream, Realm, Procedure, Mode, Handler, Policy,
             EncodedAd}, _From, #state{stream_procs = SP} = S) ->
    Pids = spawned_link_pids(S),
    Reply = fanout_advertise_stream(Pids, Realm, Procedure, Mode, Handler,
                                    Policy, EncodedAd),
    {reply, Reply,
     S#state{stream_procs = SP#{{Realm, Procedure} =>
                                {Mode, Handler, Policy, EncodedAd}}}};

handle_call({unadvertise_stream, Realm, Procedure}, _From,
            #state{stream_procs = SP, node_identity = Key} = S) ->
    Withdrawal = withdrawal_for(stream_registration(maps:find({Realm, Procedure}, SP)),
                                Realm, Procedure, Key),
    _ = fanout_unadvertise_stream(spawned_link_pids(S), Realm, Procedure,
                                  Withdrawal),
    {reply, ok,
     S#state{stream_procs = maps:remove({Realm, Procedure}, SP)}};

handle_call({realm_key, RealmId}, _From, #state{realm_keys = Keys} = S) ->
    {reply, pinned_realm_key(maps:find(RealmId, Keys)), S};
handle_call({resolved_candidate, RealmId, Procedure}, _From,
            #state{resolved = Resolved, links = Links} = S) ->
    {reply, still_usable(maps:find({RealmId, Procedure}, Resolved),
                         erlang:monotonic_time(millisecond), live_links(Links)), S};
handle_call({sign_node_record, Record}, _From, #state{node_identity = Key} = S) ->
    {reply, node_record_signed(macula_record:node_signed(Record), Record, Key), S};
handle_call({sign_node_record_bounded, Record}, _From, #state{node_identity = Key} = S) ->
    {reply, node_record_signed_bounded(macula_record:node_signed(Record), Record, Key,
                                       maps:get(not_after, Record)), S};
handle_call({sign_domain_record, Record}, _From, #state{node_identity = Key} = S) ->
    {reply, domain_record_signed(macula_record:domain_record_checked(Record), Record, Key), S};
handle_call({withdraw_node_record, Withdrawn, Reason}, _From, #state{node_identity = Key} = S) ->
    {reply, tombstone_signed(verified_record(Withdrawn, Key), macula_node_keys:public_key(Key), Reason, Key), S};
handle_call(status, _From,
            #state{seeds = Seeds, links = Links, subs = Subs,
                   node_id = NodeId, replication = Replication} = S) ->
    {Healthy, Failed} = count_link_health(Seeds, Links),
    Status = #{
        seeds              => Seeds,
        healthy_links      => Healthy,
        failed_links       => Failed,
        self_node_id       => NodeId,
        subscriptions      => map_size(Subs),
        %% How many links one publish/5 call fans to (Opts'
        %% `replication_factor', or the pool default) — surfaced so a
        %% caller (and macula_client_tests.erl) can confirm what the
        %% pool actually resolved, not just what a doc claims.
        replication_factor => Replication,
        %% Per-publisher gaps given up on after the reorder timeout —
        %% the genuine loss rate an `ordered' subscriber could not fill.
        pubsub_gap_skips   => total_skips(Subs),
        %% Dials and link starts refused, and discovered stations deferred
        %% by the budget or refused, by reason.
        refused_dials      => macula_refusal_report:counts(S#state.refused_dials),
        %% Issuers the pool started after its first, one for each that ended.
        issuer_restarts    => S#state.issuer_restarts,
        %% Issuers the pool lost, whether or not a new one runs yet.
        issuer_losses      => S#state.issuer_losses
    },
    {reply, {ok, Status}, S};

handle_call(links, _From, #state{links = Links, last_disconnects = Last} = S) ->
    {reply, {ok, link_infos(Links, Last)}, S};

handle_call(_Req, _From, S) ->
    {reply, {error, unknown_call}, S}.

%% Delivered by the discovery worker (`run_station_discovery/1') after a
%% successful `hecate_stations.list_stations' call, as raw
%% `[{Seed, NodeId}]' pairs -- NOT pre-deduped or pre-capped (the worker
%% only knows the seeds/links at spawn time, so all of that has to
%% happen here, against the pool's actual current state, not a stale
%% snapshot). See `add_discovered_seeds/2'.
handle_cast({remember_resolved, RealmId, Procedure, Candidate, TtlMs},
            #state{resolved = Resolved} = S) ->
    Now = erlang:monotonic_time(millisecond),
    {noreply, S#state{resolved = remembered({RealmId, Procedure}, Candidate,
                                            Now + TtlMs, Now, Resolved)}};
handle_cast({discovered_stations, NewSeeds}, S) ->
    {noreply, add_discovered_seeds(NewSeeds, S)};

handle_cast(_Msg, S) -> {noreply, S}.

handle_info({macula_event, _LinkSubRef, Topic, Payload,
             #{realm := Realm, publication_hash := Hash,
               expires_at := ExpiresAt} = Meta}, S) ->
    {noreply, on_inbound_event(matching_subscriptions(Realm, Topic, S),
                               Hash, ExpiresAt, Topic, Payload, Meta, S)};

handle_info({macula_event_gone, LinkSubRef, _Reason}, S) ->
    %% A link torn down its subscription end. Pool will respawn the
    %% link via the DOWN handler and replay subs. Don't propagate to
    %% local consumers — they see a continuous stream. The link's SubRef
    %% is gone, so unsubscribe no longer sends it.
    {noreply, forget_link_sub(LinkSubRef, S)};

handle_info({'DOWN', Mon, process, Pid, Reason}, S) ->
    on_down(Mon, Pid, Reason, S);

handle_info({respawn_link, Seed}, S) ->
    {noreply, on_respawn_link(Seed, S)};

%% A restart timer that fires while an issuer runs starts nothing.
handle_info(restart_issuer, #state{issuer = Issuer} = S) when is_pid(Issuer) ->
    {noreply, S};
handle_info(restart_issuer, #state{node_identity = NodeIdentity, issuer_start = Start} = S) ->
    {noreply, issuer_restarted(Start(fun() -> NodeIdentity end, self()), S)};

handle_info(run_discovery, #state{discovery = undefined} = S) ->
    %% Disabled after being scheduled (should not happen -- discovery
    %% is fixed at connect/2 time today) or a stray message. No-op.
    {noreply, S};
handle_info(run_discovery, #state{discovery = D} = S) ->
    Self = self(),
    _ = spawn(fun() -> run_station_discovery(Self) end),
    {noreply, schedule_discovery(D#discovery_state.refresh_ms, S)};

handle_info(dedup_sweep, S) ->
    _ = macula_client_dedup:sweep(S#state.dedup_tab, erlang:system_time(millisecond)),
    erlang:send_after(S#state.dedup_sweep, self(), dedup_sweep),
    {noreply, S};

handle_info(discovery_giveup_sweep, #state{discovery = undefined} = S) ->
    %% Defensive only -- `arm_giveup_sweep/1' never arms this timer
    %% unless discovery was enabled at `connect/2' time, and nothing
    %% today can disable it afterwards. Dropped, not re-armed.
    {noreply, S};
handle_info(discovery_giveup_sweep, #state{discovery = D} = S) ->
    S1 = sweep_stale_discovered_links(D#discovery_state.giveup_after_ms, S),
    erlang:send_after(D#discovery_state.giveup_sweep_ms, self(),
                      discovery_giveup_sweep),
    {noreply, S1};

handle_info(order_flush, S) ->
    %% Release timed-out gaps, then re-arm only if something is still
    %% buffered (a fresh gap opened while this timer was pending).
    S1 = flush_all_subs(S#state{flush_timer = undefined}),
    {noreply, ensure_flush_timer(S1)};

handle_info({'EXIT', Admission, Reason}, #state{admission = Admission} = S) ->
    %% The pool's request admission ended. A pool that went on would judge
    %% requests without the ones it has seen, so it stops, and its owner
    %% starts a fresh one.
    {stop, {shutdown, {admission_down, Reason}}, S};
%% A link says why it disconnected, just before it stops `normal'. Its DOWN
%% follows this message, as signals from one process arrive in order.
handle_info({macula_link_disconnected, Pid, Summary}, S) ->
    {noreply, disconnect_kept(find_link_by_pid(Pid, S), Summary, S)};
handle_info({'EXIT', _Pid, _Reason}, S) ->
    %% Links are linked to us via gen_server:start_link in
    %% start_link_for_seed (we trap_exit). The DOWN monitor fires
    %% alongside; that path handles cleanup. Drop the EXIT.
    {noreply, S};

handle_info(_Other, S) ->
    {noreply, S}.

terminate(_Reason, #state{subs = Subs, admission = Admission}) ->
    %% Notify every subscriber that the pool is gone.
    maps:foreach(
      fun(SubRef, #sub_spec{subscriber = Pid, mon = Mon}) ->
          erlang:demonitor(Mon, [flush]),
          Pid ! {macula_event_gone, SubRef, pool_closed}
      end, Subs),
    %% The admission is linked to the pool, and a normal exit would not end it.
    true = exit(Admission, shutdown),
    ok.

code_change(_OldVsn, S, _Extra) -> {ok, S}.

%% The link options hold the node's keys: status output and crash reports show them with their private halves
%% redacted.
format_status(Status) -> macula_node_keys:redacted(Status).

%% A record the pool signs is one a node signs about itself. macula_record:refresh/2 stamps it now and signs it, and
%% sign/2 checks the key's purpose, the lifetime, that the payload names this node, verify/3's field and payload
%% rules, and the size. A refusal names what failed and carries neither the key nor a stack.
node_record_signed(false, _Record, _Key) ->
    {error, not_a_node_signed_type};
node_record_signed(true, Record, Key) ->
    signed_here(fun() -> macula_record:refresh(Record, Key) end).

%% The bounded signing: `macula_record:refresh/3' stamps the record and ends it
%% at the bound in one step, on one clock read, so the time this call takes
%% cannot carry the record past the bound. It refuses a bound already passed,
%% by name, on the same clock read.
%%
%% ⚠ This used to write the bound into the record as `expires_at' and then
%% refresh it. Refresh keeps a record's LIFETIME, so the bound was re-anchored
%% to a second, later clock read and the record ended at the bound plus its own
%% age at signing time (Mars, 2026-09-22).
node_record_signed_bounded(false, _Record, _Key, _NotAfter) ->
    {error, not_a_node_signed_type};
node_record_signed_bounded(true, Record, Key, NotAfter) ->
    bounded_signed(fun() -> macula_record:refresh(Record, Key, NotAfter) end).

bounded_signed(Sign) ->
    unwrapped(signed_here(Sign)).

%% refresh/3 answers `{ok, Record}' or `{error, not_after_passed}', and
%% `signed_here/1' wraps whatever it returns in another `{ok, _}'.
unwrapped({ok, {ok, Signed}})  -> {ok, Signed};
unwrapped({ok, {error, _} = Refusal}) -> Refusal;
unwrapped({error, _} = Refusal) -> Refusal.

%% A domain record the pool signs is signed as this node: macula_record:refresh/2 stamps it now and signs it, and
%% sign/2 checks the key's purpose, the lifetime and the size. The pool checks the record in its own process as the
%% caller side does, so a record handed to it directly is refused by name, and the pool never signs one no verifier
%% accepts.
domain_record_signed(ok, Record, Key) ->
    signed_here(fun() -> macula_record:refresh(Record, Key) end);
domain_record_signed({error, malformed}, _Record, _Key) ->
    {error, malformed_record};
domain_record_signed({error, _} = Refusal, _Record, _Key) ->
    Refusal.

%% A tombstone is signed only for a record that verifies, is of a type a node signs about itself or a domain type, and
%% carries this pool's own key. The carried key is compared, not a key id: a domain record's key id is the key id of
%% its key as carried, which differs from the node_id a node record names.
verified_record(Withdrawn, #{profile := Profile}) ->
    try macula_record:verify(wire_record(Withdrawn), Profile)
    catch _:_ -> {error, malformed_record}
    end.

wire_record(Bytes) when is_binary(Bytes) -> Bytes;
wire_record(Record) -> macula_record:encode(Record).

tombstone_signed({ok, Verified}, Own, Reason, Key) ->
    withdrawable(macula_record:node_signed(Verified) orelse macula_record:domain_type(Verified), Verified, Own,
                 Reason, Key);
tombstone_signed({error, _} = Refusal, _Own, _Reason, _Key) ->
    Refusal.

withdrawable(false, _Verified, _NodeId, _Reason, _Key) ->
    {error, not_a_node_signed_type};
withdrawable(true, #{key := Own} = Verified, Own, Reason, Key) ->
    signed_here(fun() -> macula_record:sign(macula_record:tombstone(Verified, Reason), Key) end);
withdrawable(true, _Verified, _Own, _Reason, _Key) ->
    {error, not_this_nodes_record}.

signed_here(Sign) ->
    try Sign() of
        Signed -> {ok, Signed}
    catch
        error:{key_id_mismatch, _Type} -> {error, key_id_mismatch};
        error:{lifetime_too_long, _Type} -> {error, lifetime_too_long};
        error:{lifetime_reversed, _Type} -> {error, lifetime_reversed};
        error:{record_too_large, _Bytes} -> {error, record_too_large};
        error:{malformed, _Type} -> {error, malformed_record};
        _:_ -> {error, malformed_record}
    end.

%%====================================================================
%% Internals — link lifecycle
%%====================================================================

start_link_for_seed(Seed, S) -> start_link_for_seed(Seed, #{}, S).

start_link_for_seed(Seed, ExtraOpts, #state{issuer = undefined} = S) ->
    held_start(Seed, ExtraOpts, S);
start_link_for_seed(Seed, ExtraOpts, #state{node_identity = NodeIdentity} = S) ->
    LinkOpts = maps:merge(S#state.link_opts,
                          ExtraOpts#{seed => Seed, node_identity => fun() -> NodeIdentity end,
                                     share => seed_peer(Seed)}),
    after_link_start(macula_station_link:start_link(LinkOpts), Seed,
                     remember_dial_opts(Seed, ExtraOpts, S)).

%% Keep how this link was started, so its respawn can start it the same way.
remember_dial_opts(Seed, ExtraOpts, #state{dial_extra_opts = Opts} = S) ->
    S#state{dial_extra_opts = Opts#{Seed => ExtraOpts}}.

%% While the pool has no issuer, a link start waits for the next one
%% instead of starting a link that could not connect, and counts as one
%% refusal.
held_start(Seed, ExtraOpts, #state{held_starts = Held, links = Links} = S) ->
    Empty = (prior_link_state(Seed, S))#link_state{seed = Seed, pid = undefined, mon = undefined},
    count_refused_dial(link_start_waits_for_issuer,
                       S#state{held_starts = Held#{Seed => ExtraOpts}, links = Links#{Seed => Empty}}).

after_link_start({ok, Pid}, Seed, S) ->
    Mon = erlang:monitor(process, Pid),
    LinkState = (prior_link_state(Seed, S))#link_state{
        seed = Seed, pid = Pid, mon = Mon},
    S#state{links = (S#state.links)#{Seed => LinkState}};
after_link_start({error, Reason}, Seed, S) ->
    macula_diagnostics:event(<<"_macula.client.link_start_failed">>,
                             #{seed => Seed, reason => Reason}),
    Empty = (prior_link_state(Seed, S))#link_state{
        seed = Seed, pid = undefined, mon = undefined},
    start_refused(permanent_refusal(Reason), Seed, S#state{links = (S#state.links)#{Seed => Empty}}).

%% A seed that can never start a link counts once and is not tried again;
%% any other refusal is tried again after the respawn delay.
%%
%% ⚠ A refusal that is deterministic in the SEED must be permanent. Left to
%% the transient catch-all it schedules a respawn every
%% ?LINK_RESPAWN_DELAY_MS, the gate refuses the same seed again, and the
%% pool loops forever on a link that cannot start, uncounted and invisible
%% to `status/1'.
start_refused({permanent, Kind}, _Seed, S) ->
    count_refused_dial(Kind, S);
start_refused(transient, Seed, S) ->
    erlang:send_after(?LINK_RESPAWN_DELAY_MS, self(), {respawn_link, Seed}),
    S.

permanent_refusal({seed, expected_node_id_required}) -> {permanent, seed_without_expected_node_id};
permanent_refusal({seed, {pin_tls_cert, _Reason}}) -> {permanent, pin_tls_cert_refused};
permanent_refusal(_Transient) -> transient.

%% Carries a seed's `discovered'/`ever_connected'/`spawned_at' across
%% its own respawn (both branches above) instead of resetting them --
%% a discovered link that dies and respawns before ever connecting
%% must not get a fresh give-up clock every time
%% (`sweep_stale_discovered_links/2'). No-op for a seed with no prior
%% entry (a first dial) or a bootstrap/direct-dial seed (these three
%% fields stay at their record defaults regardless).
prior_link_state(Seed, #state{links = Links}) ->
    maps:get(Seed, Links, #link_state{seed = Seed}).

spawned_link_pids(#state{links = Links}) ->
    [P || #link_state{pid = P} <- maps:values(Links), is_pid(P)].

%% Reuse a live link to `Station', else dial a new one and add it to the
%% pool exactly like a seed link (monitored, respawn-on-DOWN). Returns
%% the link pid (or `undefined' if the dial failed to spawn) + new
%% state. A FRESH dial (only) is made with `ExtraOpts' merged on top of
%% the pool's own `link_opts' — e.g. a direct-dial caller's per-call
%% `expected_node_id'. An already-connected, reused
%% link keeps whatever trust it was originally dialed under; `ExtraOpts'
%% only shapes a dial that happens as a result of THIS call.
%%
%% `Links' is keyed by the literal `Station' STRING, not by the
%% station's actual identity. A direct-dial caller names `Station' by a
%% URL it just resolved (`macula-io/macula-station'-style: a
%% `station_endpoint' record's `quic://[host]:port'), which very often
%% spells the SAME physical station differently than however the pool's
%% own seeds (or an earlier direct-dial call to it) already named it —
%% and a literal-string miss here used to dial a genuinely SECOND,
%% redundant connection to a station the pool already held a live
%% connection to.
%%
%% Found live 2026-08-29: reproducible on literally the SECOND
%% `call_station' to the same station from one pool, regardless of
%% realm/procedure — the station closed one of the two duplicate
%% connections, and whichever caller's next attempt landed on the
%% closed one failed with `{disconnected, {peer_closed, ...}}'.
%%
%% `expected_node_id' (when a direct-dial caller supplies one — see
%% `call_station/8') is exactly the station identity that caller already
%% resolved and verified via a signed DHT record before ever reaching
%% here. A literal-key miss now falls back to asking every link this
%% pool currently holds whether IT is already connected to that same
%% identity, under `find_link_by_node_id/2', before dialing fresh.
%%
%% Deliberately does NOT do this for a caller with no `expected_node_id'
%% (the pool's own seed-connect path never sets one — see
%% `call_station/9''s own doc: "The pool's own `connect/2'-time
%% `expected_node_id' is fixed at connect time"): scanning every link for
%% a plain seed dial would add cost to the common path for no benefit,
%% since a seed's `Station' string IS already its own canonical `Links'
%% key. This only ever runs on a direct-dial literal-key miss, which any
%% SUBSEQUENT call to that SAME resolved URL will skip entirely (it hits
%% the ordinary literal-key match above).
ensure_link(Station, ExtraOpts, #state{links = Links} = S) ->
    ensure_link_for(maps:find(Station, Links), Station, ExtraOpts, S).

ensure_link_for({ok, #link_state{pid = Pid}}, _Station, _ExtraOpts, S)
        when is_pid(Pid) ->
    {Pid, S};
ensure_link_for(_Missing, Station, ExtraOpts, S) ->
    reuse_by_node_id(maps:get(expected_node_id, ExtraOpts, undefined),
                     Station, ExtraOpts, S).

reuse_by_node_id(undefined, Station, ExtraOpts, S) ->
    dial_fresh(Station, ExtraOpts, S);
reuse_by_node_id(NodeId, Station, ExtraOpts, #state{links = Links} = S) ->
    reuse_or_dial(find_link_by_node_id(NodeId, Links), Station, ExtraOpts, S).

reuse_or_dial(Pid, _Station, _ExtraOpts, S) when is_pid(Pid) ->
    {Pid, S};
reuse_or_dial(undefined, Station, ExtraOpts, S) ->
    dial_fresh(Station, ExtraOpts, S).

%% A fresh direct dial names a seed the pool can dial, and fits the pool's
%% direct links and its new-peer budget, or is refused with a named error and
%% dials nothing.
dial_fresh(Station, ExtraOpts, S) ->
    usable_dial(usable_seed(Station), Station, ExtraOpts, S).

usable_dial(false, _Station, _ExtraOpts, S) ->
    refused_dial(unusable_seed, S);
usable_dial(true, Station, ExtraOpts, #state{max_direct_links = Max} = S) ->
    direct_dial(direct_link_count(S) < Max, Station, ExtraOpts, S).

direct_dial(false, _Station, _ExtraOpts, S) ->
    refused_dial(too_many_direct_links, S);
direct_dial(true, Station, ExtraOpts, S) ->
    budgeted_dial(spend_dial_budget(Station, S), Station, ExtraOpts).

budgeted_dial({spent, S}, _Station, _ExtraOpts) ->
    refused_dial(new_peer_budget_spent, S);
budgeted_dial({ok, S}, Station, ExtraOpts) ->
    S1 = mark_direct(Station, start_link_for_seed(Station, ExtraOpts, S)),
    {link_pid(Station, S1), S1}.

direct_link_count(#state{links = Links}) ->
    length([Seed || Seed := #link_state{direct = true} <- Links]).

%% `start_link_for_seed/3' always leaves an entry for the seed.
mark_direct(Station, #state{links = Links} = S) ->
    S#state{links = maps:update_with(Station, fun(L) -> L#link_state{direct = true} end, Links)}.

%% A link to work on, or a refused dial: the refusal is the reply, and no
%% worker starts.
on_link({{error, _} = Refused, S}, _From, _Work) ->
    {reply, Refused, S};
on_link({Pid, S}, From, Work) ->
    _ = spawn(fun() -> gen_server:reply(From, Work(Pid)) end),
    {noreply, S}.

%% A peer a pool links to, by its normalized seed.
seed_peer(Seed) ->
    {seed, normalize_seed(Seed)}.

%% A seed the pool can dial names a text host and a port from 1 to 65535.
%% Any other seed is refused where it enters the pool, from a direct dial or
%% from discovery, and counted, instead of starting a link that could never
%% connect.
usable_seed(Seed) ->
    usable_normalized(normalize_seed(Seed)).

usable_normalized(#{host := Host, port := Port})
  when is_binary(Host), byte_size(Host) > 0, is_integer(Port), Port > 0, Port =< 65535 ->
    true;
usable_normalized(_Unusable) ->
    false.

spend_dial_budget(Station, #state{dial_budget = Budget} = S) ->
    {Verdict, Spent} = macula_client_peer_budget:spend(Budget, seed_peer(Station),
                                                       erlang:monotonic_time(millisecond)),
    {Verdict, S#state{dial_budget = Spent}}.

%% A dial this pool would not make: its direct-link cap, its new-peer budget,
%% or a seed it cannot dial at all. Nothing is built and nothing is sent.
%%
%% ⚠ THE WRAPPER CARRIES THE SCOPE, and that is the point of it rather than
%% decoration. `{dial_refused, _}' is `candidate' scoped to
%% `macula_station_link:failure_scope/1': another candidate may need NO NEW
%% LINK AT ALL, because the pool may already hold a live one to it, so a
%% caller working through candidates should try the next. Returned bare, these
%% reasons fell into that function's catch-all and read as `provider', which
%% ended resolution with candidates untried (macula#20). The COUNT stays keyed
%% on the bare reason: `status/1''s `refused_dials' is a tally of why dials
%% were refused, not of what a caller saw.
refused_dial(Reason, S) ->
    {{error, {dial_refused, Reason}}, count_refused_dial(Reason, S)}.

%% Counted every time, logged at most once per window per reason with the
%% count since the last line.
count_refused_dial(Reason, #state{refused_dials = Report} = S) ->
    Refused = macula_refusal_report:refused(Report, Reason, erlang:monotonic_time(millisecond)),
    S#state{refused_dials = logged_refusal(Refused, Reason)}.

logged_refusal({report, Count, Report}, Reason) ->
    logger:warning("[macula_client] refused ~b dial(s): ~p", [Count, Reason]),
    Report;
logged_refusal({quiet, Report}, _Reason) ->
    Report.

%% Bounded by however many links this pool currently holds — typically
%% a handful (its configured seeds plus any prior direct-dial targets),
%% so a synchronous `peer_node_id/1' round trip per link is an
%% acceptable, RARE cost: only a direct-dial literal-key MISS reaches
%% here at all (see `ensure_link/3''s own doc for why every subsequent
%% call to the same resolved URL skips this entirely).
find_link_by_node_id(NodeId, Links) ->
    link_pid_of(first_matching_link(live_links(Links), NodeId)).

%% The pool's links that have a process, as {seed, pid}: the shape both
%% lookups below work on, and the shape a test can build without the pool.

%% The SEED a live link to `NodeId' is keyed by, for a caller that needs to
%% NAME that link rather than hold it: `macula:call_station/8' takes a seed,
%% and `ensure_link/3' matches the pool's own key before anything else, so
%% handing back the key the pool already uses reaches the live link on its
%% first lookup and dials nothing.
find_seed_by_node_id(NodeId, LiveLinks) ->
    link_seed_of(first_matching_link(LiveLinks, NodeId)).

link_pid_of({_Seed, Pid}) -> Pid;
link_pid_of(undefined)    -> undefined.

link_seed_of({Seed, _Pid}) -> {ok, Seed};
link_seed_of(undefined)    -> none.

live_links(Links) ->
    [{Seed, Pid} || Seed := #link_state{pid = Pid} <- Links, is_pid(Pid)].

first_matching_link([], _NodeId) ->
    undefined;
first_matching_link([{_Seed, Pid} = Link | Rest], NodeId) ->
    %% safe_peer_node_id/1 (below, pre-existing) already absorbs a dead
    %% or wedged link's gen_server:call exit -- exactly the "one bad
    %% link must not crash this whole reuse scan" concern this function
    %% would otherwise need its own try/catch for.
    keep_or_next(safe_peer_node_id(Pid), Link, Rest, NodeId).

keep_or_next(NodeId, Link, _Rest, NodeId) when NodeId =/= undefined -> Link;
keep_or_next(_Other, _Link, Rest, NodeId) -> first_matching_link(Rest, NodeId).

%% A remembered candidate is handed back only while its horizon has not
%% elapsed AND the pool still holds a live link to the station it names.
%% Both together are what make it a head start rather than a guess: the
%% horizon bounds how stale the ADVERTISEMENT may be, and the live link is
%% direct evidence about the STATION that no stored record can give.
still_usable({ok, #{candidate := Candidate, until_mono := Until}}, Now, LiveLinks)
  when Now < Until ->
    usable_with_seed(find_seed_by_node_id(maps:get(station, Candidate), LiveLinks),
                     Candidate);
still_usable(_MissingOrElapsed, _Now, _LiveLinks) ->
    none.

usable_with_seed({ok, Seed}, Candidate) -> {ok, Candidate, Seed};
usable_with_seed(none, _Candidate)      -> none.

%% Entries whose horizon has elapsed are dropped as a new one arrives, so
%% the map tracks the procedures this pool currently calls instead of every
%% procedure it has ever called. Nothing else prunes it, and nothing else
%% needs to: a pool that stops calling a procedure stops writing here too,
%% and the entries it leaves behind are one small map each.
remembered(Key, Candidate, UntilMono, Now, Resolved) ->
    Live = maps:filter(fun(_K, #{until_mono := U}) -> Now < U end, Resolved),
    Live#{Key => #{candidate => Candidate, until_mono => UntilMono}}.

link_pid(Station, #state{links = Links}) ->
    case maps:find(Station, Links) of
        {ok, #link_state{pid = Pid}} -> Pid;
        _                            -> undefined
    end.

%% Wait for a freshly-dialed link's handshake within `DialTimeoutMs' (and
%% never past the call's own deadline), then call over it with whatever
%% time remains of `TimeoutMs'. A reused, already-connected link calls
%% immediately.
call_when_connected(undefined, _Target, _Realm, _Proc, _Payload, _TimeoutMs, _DialTimeoutMs,
                    _Ucan) ->
    {error, not_connected};
call_when_connected(Pid, Target, Realm, Proc, Payload, TimeoutMs, DialTimeoutMs, Ucan) ->
    Now = erlang:monotonic_time(millisecond),
    Deadline = Now + TimeoutMs,
    call_after_connect(await_connected(Pid, Now + min(DialTimeoutMs, TimeoutMs)), Pid,
                       Target, Realm, Proc, Payload, Deadline, Ucan).

%% INSTRUMENT. Every connect wait reports HOW it ended, HOW LONG it took and
%% WHOSE link it was, on both outcomes, with no threshold.
%%
%% ⚠ Deliberately agnostic. It does not look for a wait of any particular
%% length, because a threshold encodes the hypothesis it is meant to test and
%% a wait that is never recorded cannot refute anything. Reading the
%% distribution is the caller's job; producing it honestly is this one's.
%%
%% It exists because a stall was observed INSIDE a single resolve step rather
%% than across a retry (120 calls, three runs, every one exactly four route
%% steps and exactly one `find_records'), so the thing to see is a step that
%% waits and then succeeds. `elapsed_ms' on a `connected' outcome is that
%% measurement; the same field on a `deadline' outcome is the give-up time.
%%
%% `node_id' is the handshake peer and is `undefined' when the wait ended at
%% the deadline, because a link that never connected has no peer identity.
%% `link' is carried so a timed-out wait can still be told from another.
%%
%% The recursion runs through `poll_connected/2`, NOT back through here, so
%% one wait emits one event rather than one per 50 ms poll.
await_connected(Pid, Deadline) ->
    Started = erlang:monotonic_time(millisecond),
    Outcome = poll_connected(Pid, Deadline),
    report_connect_wait(Outcome, Pid, erlang:monotonic_time(millisecond) - Started),
    Outcome.

report_connect_wait(Outcome, Pid, ElapsedMs) ->
    macula_diagnostics:event(<<"_macula.client.connect_wait_ended">>,
                             #{reason => connect_wait_reason(Outcome),
                               elapsed_ms => ElapsedMs,
                               node_id => link_node_id(Pid, Outcome),
                               link => Pid}),
    ok.

connect_wait_reason(true)  -> connected;
connect_wait_reason(false) -> deadline.

poll_connected(Pid, Deadline) ->
    connected_or_wait(safe_is_connected(Pid), Pid, Deadline).

connected_or_wait(true, _Pid, _Deadline) ->
    true;
connected_or_wait(false, Pid, Deadline) ->
    wait_or_give_up(erlang:monotonic_time(millisecond) < Deadline, Pid, Deadline).

wait_or_give_up(true, Pid, Deadline) ->
    timer:sleep(50),
    poll_connected(Pid, Deadline);
wait_or_give_up(false, _Pid, _Deadline) ->
    false.

call_after_connect(true, Pid, Target, Realm, Proc, Payload, Deadline, Ucan) ->
    Remaining = max(100, Deadline - erlang:monotonic_time(millisecond)),
    macula_station_link:call(Pid, Target, Realm, Proc, Payload, Remaining, Ucan);
call_after_connect(false, _Pid, _Target, _Realm, _Proc, _Payload, _Deadline, _Ucan) ->
    {error, not_connected}.

%% As `call_when_connected/8', but for `ensure_station_link/4': waits
%% for a freshly-dialed link's handshake, then hands back the pid
%% itself rather than making a call over it.
content_link_when_connected(undefined, _TimeoutMs) ->
    {error, not_connected};
content_link_when_connected(Pid, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    content_link_after_connect(await_connected(Pid, Deadline), Pid).

content_link_after_connect(true, Pid)   -> {ok, Pid};
content_link_after_connect(false, _Pid) -> {error, not_connected}.

%% Live links that have completed CONNECT/HELLO. Used by publish,
%% which (unlike advertise) gains nothing from dispatching to a
%% mid-handshake link.
connected_link_pids(#state{} = S) ->
    [P || P <- spawned_link_pids(S),
          is_process_alive(P),
          safe_is_connected(P)].

%% Which of the currently-connected links receive one publish, given
%% the pool's replication_factor: the first `Replication' of `Targets'
%% (in `connected_link_pids/1' order), capped at however many are
%% actually connected. A pure function, exported for
%% macula_client_tests.erl, specifically so the selection math itself
%% (and the default replication_factor's effect on it) is directly
%% testable without needing a live QUIC handshake.
-spec select_publish_targets([pid()], pos_integer()) -> [pid()].
select_publish_targets(Targets, Replication) ->
    %% lists:sublist/2 already caps at length(Targets) on its own.
    lists:sublist(Targets, Replication).

first_connected_link([Pid | _]) -> {ok, Pid};
first_connected_link([])        -> {error, no_healthy_station}.

%% Candidate ordering for `call_first_success/5', `select_publish_targets/2'
%% (via its `lists:sublist/2' first-N), and `pick_connected_link/1'.
%% `first_success': today's behaviour, byte-for-byte -- whatever order
%% `Pids' already came in (spawn order, i.e. seed-list order). `random':
%% shuffle first, so which link is tried first (and which N a
%% replicated publish lands on) varies per call instead of always
%% favouring whichever seed happens to be first in the list.
-spec ordered_for_selection([pid()], first_success | random) -> [pid()].
ordered_for_selection(Pids, first_success) ->
    Pids;
ordered_for_selection(Pids, random) ->
    %% Schwartzian-transform shuffle -- O(n log n), no external dep.
    [P || {_Rand, P} <- lists:sort([{rand:uniform(), P} || P <- Pids])].

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

%% The pool's keys, in the node's crypto profile: the node identity key
%% that every link shares, and the pool's own statement issuer under
%% macula_statement_issuer_sup, which holds the pool's CONNECT key and
%% signs each connection's material under a binding by the identity key
%% (D16). The issuer ends with the pool.
pool_keys(Opts) ->
    keys_in_profile(macula_crypto_profile:configured(), Opts).

keys_in_profile({ok, Profile}, Opts) ->
    keys_with_identity(node_identity(identity_opt(maps:find(node_identity, Opts)), Profile), Profile,
                       issuer_start(Opts));
keys_in_profile({error, _} = Refusal, _Opts) ->
    Refusal.

%% A node identity key comes as a function that returns it, or as a loader {Module, Function, Args} that returns
%% {ok, Key}, as a child spec names it. A loader that returns anything else refuses the pool, naming no key.
identity_opt({ok, Identity}) when is_function(Identity, 0) -> {ok, Identity()};
identity_opt({ok, {Module, Function, Args}}) when is_atom(Module), is_atom(Function), is_list(Args) ->
    loaded(run_loader(Module, Function, Args));
identity_opt(NotGiven) -> NotGiven.

%% A loader runs once, at the pool's start. One that raises refuses the pool as one that returns no key does. The try
%% is what keeps a loader's crash from becoming the pool's crash report, and the refusal carries none of the loader's
%% error, which can hold the key the loader read. The loader's Args never hold the key, only where it is, because a
%% supervisor logs them when a start fails (child_spec/3).
run_loader(Module, Function, Args) ->
    try apply(Module, Function, Args)
    catch _Class:_Reason -> loader_raised
    end.

loaded({ok, #{purpose := identity} = Key}) -> {ok, Key};
loaded({ok, _NotAnIdentityKey}) ->
    {error, {node_identity, loader_failed}};
loaded({error, Reason}) ->
    {error, {node_identity, loader_refusal(Reason)}};
loaded(_NoKey) ->
    {error, {node_identity, loader_failed}}.

%% A loader that returns one of macula_node_keys:load/3's documented
%% refusals refuses the pool with that reason nested under
%% loader_failed, so the service that holds the pool can say why. Any
%% other reason — one that could carry the key the loader read — stays
%% flat, and only well-formed values nest.
loader_refusal(Reason) ->
    case well_formed_loader_refusal(Reason) of
        true  -> {loader_failed, Reason};
        false -> loader_failed
    end.

well_formed_loader_refusal(Reason)
  when Reason =:= key_file_permissions; Reason =:= bad_key_file;
       Reason =:= private_key_invalid; Reason =:= public_key_mismatch;
       Reason =:= round_trip_failed; Reason =:= enoent;
       Reason =:= eacces; Reason =:= enospc ->
    true;
well_formed_loader_refusal({Tag, Value}) ->
    loader_refusal_value(Tag, Value);
well_formed_loader_refusal(_Other) ->
    false.

loader_refusal_value(Tag, Value)
  when (Tag =:= wrong_profile orelse Tag =:= wrong_purpose orelse
        Tag =:= unknown_purpose orelse Tag =:= crypto_profile_unknown),
       is_atom(Value) ->
    true;
loader_refusal_value(wrong_algorithms, Value) ->
    is_list(Value) andalso lists:all(fun erlang:is_atom/1, Value);
loader_refusal_value(wrong_key_size, {N, M}) ->
    is_integer(N) andalso N > 0 andalso is_integer(M) andalso M > 0;
loader_refusal_value(_Tag, _Value) ->
    false.

%% How the pool starts its statement issuer: macula_statement_issuer_sup:start_issuer/2, unless the issuer_start option
%% names a function of the same shape, as a test does to refuse a restart.
issuer_start(#{issuer_start := Start}) when is_function(Start, 2) -> Start;
issuer_start(_Opts) -> fun macula_statement_issuer_sup:start_issuer/2.

%% The key redaction filter goes in place once the pool holds its key and before anything uses it, so a pool started
%% without the macula application still keeps private halves out of its tree's crash reports. A loader that fails
%% installs nothing, and its refusal alone keeps the key out.
keys_with_identity({ok, NodeIdentity}, Profile, Start) ->
    ok = macula_node_keys:install_log_redaction(),
    keys_with_issuer(Start(fun() -> NodeIdentity end, self()), NodeIdentity, Profile, Start);
keys_with_identity({error, _} = Refusal, _Profile, _Start) ->
    Refusal.

keys_with_issuer({ok, Issuer}, NodeIdentity, Profile, Start) ->
    {ok, #{profile => Profile, node_identity => NodeIdentity, issuer => Issuer, issuer_start => Start}};
keys_with_issuer({error, Reason}, _NodeIdentity, _Profile, _Start) ->
    {error, {issuer, Reason}}.

%% The pool's node identity key. A supplied key is used as it is, puzzle
%% solved or not, when it is an identity key in the node's profile; any
%% other key is refused.
%%
%% Without one, the pool generates a key whose node_id meets
%% `macula_node_keys:puzzle_difficulty/0', not a bare
%% `macula_node_keys:generate/2' key: stations check the puzzle on the
%% node_id derived from the identity key in CONNECT, and the caller who
%% did not think to pass a key is the one most surprised by a refused
%% handshake behind links that report healthy (seen live 2026-08-21: a
%% pool started with no options showed five healthy links and delivered
%% no event for over an hour). `maps:find/2' keeps the puzzle from being
%% ground when the caller did pass a key.
node_identity({error, _} = Refusal, _Profile) ->
    Refusal;
%% ⚠ NO KEY SUPPLIED MEANS THE NODE'S IDENTITY, NOT A NEW ONE. This clause used to call
%% `macula_node_keys:generate/3' and grind a fresh puzzle per pool, so two pools on one machine were two
%% different nodes and every restart made a stranger of anything not handed a key: its (org, node_id)
%% grants stopped matching a node that no longer existed. Raf's ruling, 2026-09-23: one identity per node,
%% stored, shared by pools and the distribution tunnel. `node_identity/1' loads it or grinds it ONCE and
%% stores it, and the supplied-key clauses below are untouched so an application can still run a
%% deliberately separate participant on the same machine.
node_identity(error, Profile) ->
    macula_node_keys:node_identity(Profile);
node_identity({ok, #{purpose := identity, profile := Profile} = Key}, Profile) ->
    {ok, Key};
node_identity({ok, #{purpose := identity, profile := Other}}, _Profile) ->
    {error, {node_identity, {wrong_profile, Other}}};
node_identity({ok, _NotAnIdentityKey}, _Profile) ->
    {error, {node_identity, not_an_identity_key}}.

%% First-success across the pool's healthy links. Tries each link in
%% turn; the first non-error reply wins. Which errors it walks past is
%% `macula_station_link:failure_scope/1''s judgement and not a second copy of
%% it here: only a `candidate' scoped failure moves to the next link, so a link
%% that isn't connected does not block the call. A CALL that may have reached
%% its provider, a timeout included, is never sent again, so a provider never
%% runs one call twice. AND A `request' SCOPED FAILURE STOPS AT THE FIRST LINK:
%% a payload the wire cannot carry is refused identically by every link in the
%% pool, so walking the rest burns the caller's deadline to collect the same
%% answer it already has.
call_first_success(Pids, Realm, Proc, Payload, Tmo) ->
    first_success(Pids, fun macula_station_link:is_connected/1,
                  fun(Pid) -> macula_station_link:call(Pid, station, Realm, Proc, Payload, Tmo) end).

%% `call_first_success/5' over any links: `Connected(Link)' says whether a
%% link can take the call, and `Call(Link)' makes it there.
first_success([], _Connected, _Call) ->
    {error, no_healthy_station};
first_success([Link | Rest], Connected, Call) ->
    next_or_first(Connected(Link), Link, Rest, Connected, Call).

next_or_first(false, _Link, Rest, Connected, Call) ->
    first_success(Rest, Connected, Call);
next_or_first(true, Link, Rest, Connected, Call) ->
    answer_or_next(Call(Link), Rest, Connected, Call).

answer_or_next({ok, _} = R, _Rest, _Connected, _Call) -> R;
answer_or_next({error, _} = E, [], _Connected, _Call) -> E;
answer_or_next({error, _} = E, Rest, Connected, Call) ->
    next_in_scope(macula_station_link:failure_scope(E), E, Rest, Connected, Call).

next_in_scope(candidate, _E, Rest, Connected, Call) ->
    first_success(Rest, Connected, Call);
next_in_scope(Settled, E, _Rest, _Connected, _Call)
  when Settled =:= request; Settled =:= provider ->
    E.

%% Fan-out advertise: register on every live link. Returns ok if at
%% least one link accepted; per-link errors are logged and discarded.
%%
%% Pre-handshake links MUST receive the call too — `advertise/4' on
%% the link gen_server updates its local `procedures' map regardless
%% of connection state, and that map is what dispatches a CALL the
%% station delivers once the link connects. Filtering by
%% `is_connected/1' here leaves the link's map out of sync with the
%% pool's intent.
fanout_advertise([], _Realm, _Proc, _Handler, _Policy, _EncodedAd) ->
    {error, no_healthy_station};
fanout_advertise(Pids, Realm, Proc, Handler, Policy, EncodedAd) ->
    Results = [safe_link_advertise(P, Realm, Proc, Handler, Policy, EncodedAd)
               || P <- Pids, is_process_alive(P)],
    summarize_advertise([R || R <- Results, R =/= skipped]).

safe_link_advertise(Pid, Realm, Proc, Handler, Policy, EncodedAd) ->
    try macula_station_link:advertise(Pid, Realm, Proc, Handler, Policy,
                                      EncodedAd)
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
%% state is dropped regardless.
%%
%% MUST dispatch to every LIVE link (not just connected ones): the
%% link gen_server's `unadvertise' handler clears its local
%% `procedures' map unconditionally. Filtering by `is_connected/1'
%% here leaks: a link that was disconnected at unadvertise time keeps
%% the handler in its local map, and once it connects dispatches a CALL
%% for a procedure the pool already considers withdrawn.
fanout_unadvertise(Pids, Realm, Proc, Withdrawal) ->
    [_ = safe_link_unadvertise(P, Realm, Proc, Withdrawal)
     || P <- Pids, is_process_alive(P)],
    ok.

safe_link_unadvertise(Pid, Realm, Proc, Withdrawal) ->
    try macula_station_link:unadvertise(Pid, Realm, Proc, Withdrawal)
    catch _:_ -> skipped
    end.

%% Direct-dial streaming: wait for the ensured link's handshake, then
%% open the stream there, naming its target. Mirrors `call_when_connected'
%% for streams.
stream_when_connected(undefined, _Target, _Realm, _Proc, _Args, _Opts) ->
    {error, not_connected};
stream_when_connected(Pid, Target, Realm, Proc, Args, Opts) ->
    DialTimeout = maps:get(dial_timeout_ms, Opts, 10_000),
    Deadline = erlang:monotonic_time(millisecond) + DialTimeout,
    stream_after_connect(await_connected(Pid, Deadline), Pid, Target, Realm, Proc, Args, Opts).

stream_after_connect(true, Pid, Target, Realm, Proc, Args, Opts) ->
    macula_station_link:call_stream(Pid, Target, Realm, Proc, Args, Opts);
stream_after_connect(false, _Pid, _Target, _Realm, _Proc, _Args, _Opts) ->
    {error, not_connected}.

%% Fan-out streaming advertise across every live link. Same shape
%% as `fanout_advertise/4' for unary; partial success counts. Same
%% rationale for dispatching to pre-handshake links — see the
%% comment on `fanout_advertise/4'.
fanout_advertise_stream([], _Realm, _Proc, _Mode, _Handler, _Policy, _EncodedAd) ->
    {error, no_healthy_station};
fanout_advertise_stream(Pids, Realm, Proc, Mode, Handler, Policy, EncodedAd) ->
    Results = [safe_link_advertise_stream(P, Realm, Proc, Mode, Handler,
                                          Policy, EncodedAd)
               || P <- Pids, is_process_alive(P)],
    summarize_advertise([R || R <- Results, R =/= skipped]).

safe_link_advertise_stream(Pid, Realm, Proc, Mode, Handler, Policy, EncodedAd) ->
    try macula_station_link:advertise_stream(Pid, Realm, Proc, Mode, Handler,
                                             Policy, EncodedAd)
    catch _:_ -> skipped
    end.

fanout_unadvertise_stream(Pids, Realm, Proc, Withdrawal) ->
    [_ = safe_link_unadvertise_stream(P, Realm, Proc, Withdrawal)
     || P <- Pids, is_process_alive(P)],
    ok.

safe_link_unadvertise_stream(Pid, Realm, Proc, Withdrawal) ->
    try macula_station_link:unadvertise_stream(Pid, Realm, Proc, Withdrawal)
    catch _:_ -> skipped
    end.

%% A stored stream registration `{Mode, Handler, Policy, Ad}' in the
%% `{HandlerOrMode, Policy, Ad}' shape `withdrawal_for/4' reads.
stream_registration({ok, {Mode, _Handler, Policy, Ad}}) -> {ok, {Mode, Policy, Ad}};
stream_registration(error) -> error.

%% The withdrawal for an unadvertise: a tombstone over a FRESH
%% advertisement of the same (realm, procedure) — fresh so its version
%% is later than the registered one — signed by the provider (the
%% pool's own node identity). Built from the registered authorization:
%% the spec's own, or the pre-signed advertisement's, decoded back from
%% the wire form. A tombstone's slot is the (realm, procedure), not the
%% serving station, so one withdrawal serves every link. `undefined'
%% for a local-only register, which sent no advertisement to withdraw.
withdrawal_for(error, _Realm, _Proc, _Key) ->
    undefined;
withdrawal_for({ok, {_HandlerOrMode, _Policy, Ad}}, Realm, Proc, Key) ->
    withdrawal_signed(registered_authorization(Ad, Key), Realm, Proc, Key).

registered_authorization(undefined, _Key) ->
    none;
registered_authorization(#{authorization := Authorization}, _Key) ->
    {ok, Authorization};
registered_authorization(EncodedAd, #{profile := Profile}) when is_binary(EncodedAd) ->
    case macula_record:verify(EncodedAd, Profile) of
        {ok, Ad} ->
            {ok, maps:get(authorization, macula_record:read_procedure_advertisement(Ad))};
        {error, _} ->
            none
    end.

withdrawal_signed(none, _Realm, _Proc, _Key) ->
    undefined;
withdrawal_signed({ok, Authorization}, Realm, Proc, Key) ->
    {ok, NodeId} = macula_node_keys:node_id(Key),
    Unsigned = macula_record:procedure_advertisement(
                 NodeId, Realm, Proc, NodeId,
                 #{authorization => Authorization}),
    %% The pool's own custody paths: sign the fresh ad the way
    %% sign_node_record does, then withdraw it the way
    %% withdraw_node_record does.
    case node_record_signed(macula_record:node_signed(Unsigned),
                            Unsigned, Key) of
        {ok, Signed} ->
            case tombstone_signed({ok, Signed}, macula_node_keys:public_key(Key),
                                  shutdown, Key) of
                {ok, Tombstone} -> macula_record:encode(Tombstone);
                {error, _}      -> undefined
            end;
        {error, _} ->
            undefined
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
link_infos(Links, Last) ->
    [link_info(Seed, Pid, maps:get(Seed, Last, undefined))
     || {Seed, #link_state{pid = Pid}} <- maps:to_list(Links),
        is_pid(Pid)].

link_info(Seed, Pid, LastDisconnect) ->
    Connected = link_healthy(Pid),
    #{seed            => Seed,
      host            => seed_host(Seed),
      pid             => Pid,
      connected       => Connected,
      node_id         => link_node_id(Pid, Connected),
      last_disconnect => LastDisconnect}.

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

%% A respawn restarts the link the way it was started, options included:
%% a direct dial's `expected_node_id' lives in those options rather than in
%% a URL-binary seed, and starting without it is refused by the link
%% (`seed_without_expected_node_id'), which loses the route rather than
%% dialling anything unpinned.
on_respawn_link(Seed, S) ->
    NewS = start_link_for_seed(Seed, dial_opts_for(Seed, S), S),
    replay_to_seed(maps:get(Seed, NewS#state.links, undefined), NewS).

dial_opts_for(Seed, #state{dial_extra_opts = Opts}) -> maps:get(Seed, Opts, #{}).

replay_to_seed(#link_state{pid = Pid}, S) when is_pid(Pid) ->
    LinkSubRefs = macula_client_replay:subs_to(Pid, S#state.topic_index),
    macula_client_replay:advs_to(Pid, S#state.procs),
    macula_client_replay:stream_advs_to(Pid, S#state.stream_procs),
    S#state{link_subs = (S#state.link_subs)#{Pid => LinkSubRefs}};
replay_to_seed(_, S) ->
    S.

%%====================================================================
%% Internals — station discovery (opt-in, see `station_discovery' opt)
%%====================================================================

%% Add every seed in `NewSeeds' not already a link, up to the pool's
%% configured `max_links' (bootstrap + already-discovered links count
%% against the same cap). Each added seed is spawned and replayed
%% exactly like a respawned link (`start_link_for_seed/2' +
%% `replay_to_seed/2', both pre-existing) -- a discovered station joins
%% the pool the same way any other link does; SUBSCRIBE and handler
%% registration fan-out (`spawned_link_pids/1') reach it from then on.
add_discovered_seeds(_Stations, #state{discovery = undefined} = S) ->
    %% Discovery was disabled after a worker was already in flight (only
    %% possible if a future caller adds a way to toggle it at runtime --
    %% not exposed today) or this is a stray cast. Ignore.
    S;
add_discovered_seeds(Stations, S) ->
    {Usable, Unusable} = lists:partition(fun usable_station/1, Stations),
    add_usable_discovered_seeds(Usable, lists:foldl(fun refused_unusable_station/2, S, Unusable)).

%% A discovered station whose seed the pool cannot dial is refused and
%% counted before selection, so it takes no place in the discovery budget.
usable_station({Seed, _NodeId}) -> usable_seed(Seed).

refused_unusable_station(_Station, S) -> count_refused_dial(unusable_seed, S).

add_usable_discovered_seeds(Stations, #state{discovery = D, links = Links,
                                             seeds = ConfiguredSeeds} = S) ->
    %% `ConfiguredSeeds' (the pool's original bootstrap list, fixed for
    %% its whole lifetime) is included alongside `maps:keys(Links)'
    %% because a bootstrap seed's *link* drops out of `Links' for the
    %% ~1s gap between a DOWN and its respawn (`on_down_routed/5') --
    %% without it, a bootstrap station that is down, mid-handshake, or
    %% simply slow to answer HELLO would be invisible to this dedup
    %% pass for that whole window, and re-added under whatever scheme
    %% discovery spells it in. See `select_discovery_seeds/3''s doc.
    ExistingSeeds = ConfiguredSeeds ++ maps:keys(Links),
    ByString = select_discovery_seeds([Seed || {Seed, _NodeId} <- Stations],
                                      ExistingSeeds, D#discovery_state.max_links),
    ByStringSet = sets:from_list(ByString, [{version, 2}]),
    Fresh = [Seed || {Seed, NodeId} <- Stations,
                     sets:is_element(Seed, ByStringSet),
                     not already_connected_to(NodeId, Links)],
    lists:foldl(fun add_one_discovered_seed/2, S, Fresh).

%% Pure: which of `NewSeeds' are worth adding, given `ExistingSeeds'
%% already held and a total `MaxLinks' budget. Exported (like
%% `select_publish_targets/2') so this arithmetic is directly testable
%% without a live pool. Already-held seeds are dropped first (order
%% doesn't matter there); the remainder is capped at whatever budget is
%% left, in `NewSeeds' order.
%%
%% Comparison is by NORMALIZED `{host, port}' (via `normalize_seed/1'),
%% not raw seed term equality: real bootstrap seeds are `https://...'
%% (every live deployment config), discovery always builds `quic://...'
%% (`seed_url/2') -- as plain strings those never compare equal for the
%% identical physical station. `MaxLinks - length(ExistingSeeds)' relies
%% on `ExistingSeeds' containing no duplicate STATIONS (only possible
%% if the caller passes a raw, non-deduped union -- `sets:size/1' is
%% used for the actual room count precisely to stay correct even then).
%% See `already_connected_to/2' for the secondary, identity-based check
%% this still needs: two genuinely different hostnames for the same
%% station (aliasing) normalize to two different `{host, port}' pairs
%% and this pass alone cannot catch that.
-spec select_discovery_seeds([seed()], [seed()], pos_integer()) -> [seed()].
select_discovery_seeds(NewSeeds, ExistingSeeds, MaxLinks) ->
    Existing = normalized_seed_set(ExistingSeeds),
    Room = MaxLinks - sets:size(Existing),
    lists:sublist([Seed || Seed <- NewSeeds, not is_known_seed(Seed, Existing)],
                  max(0, Room)).

normalized_seed_set(Seeds) ->
    sets:from_list([normalize_seed(Seed) || Seed <- Seeds], [{version, 2}]).

is_known_seed(Seed, ExistingNormalized) ->
    sets:is_element(normalize_seed(Seed), ExistingNormalized).

%% `macula_station_link:parse_seed/1' already does exactly this
%% normalization at connect time (collapsing `https://host:port',
%% `quic://host:port' and a pre-built `#{host, port}' map to the same
%% shape whenever the port is explicit, which every real seed in this
%% codebase's config always is). Reused here rather than re-implemented
%% so the two can never drift. Total over malformed input: a seed that
%% fails to parse falls back to comparing its own raw term, same as
%% before this normalization existed -- no worse, never crashes the
%% pool over a bad seed string.
%%
%% On top of it the host is made canonical, so one station is one seed
%% however it is spelled: a binary with its ASCII letters lowercased and the
%% brackets around an IPv6 literal dropped, and an IP literal in the one text
%% form `inet:ntoa/1' gives, with an IPv4 address mapped into IPv6 as that
%% IPv4 address. Every other byte stays as it is, a trailing dot included:
%% DNS compares those exactly, so two such names are two peers, never one.
%% A host that is not text leaves the seed as its own term. Nothing here
%% raises, whatever a seed holds.
normalize_seed(Seed) ->
    try macula_station_link:parse_seed(Seed) of
        #{host := H, port := P} -> canonical_seed(canonical_host(H), P, Seed)
    catch
        _:_ -> Seed
    end.

canonical_seed({ok, Host}, Port, _Seed) -> #{host => Host, port => Port};
canonical_seed(not_text, _Port, Seed)   -> Seed.

canonical_host(Host) when is_binary(Host) ->
    {ok, canonical_text(Host)};
canonical_host(Host) ->
    char_list_host(io_lib:char_list(Host), Host).

char_list_host(true, Host)      -> {ok, canonical_text(unicode:characters_to_binary(Host))};
char_list_host(false, _NotText) -> not_text.

canonical_text(Host) ->
    Bare = << <<(ascii_lowercase(Byte))>> || <<Byte>> <= unbracketed(Host) >>,
    ip_literal(inet:parse_address(binary_to_list(Bare)), Bare).

unbracketed(Host) when byte_size(Host) >= 2, binary_part(Host, 0, 1) =:= <<"[">>,
                       binary_part(Host, byte_size(Host) - 1, 1) =:= <<"]">> ->
    binary_part(Host, 1, byte_size(Host) - 2);
unbracketed(Host) ->
    Host.

ascii_lowercase(Byte) when Byte >= $A, Byte =< $Z -> Byte + ($a - $A);
ascii_lowercase(Byte)                             -> Byte.

ip_literal({ok, {0, 0, 0, 0, 0, 16#ffff, _, _} = Mapped}, _Bare) ->
    list_to_binary(inet:ntoa(inet:ipv4_mapped_ipv6_address(Mapped)));
ip_literal({ok, Address}, _Bare) ->
    list_to_binary(inet:ntoa(Address));
ip_literal({error, _NotAnAddress}, Bare) ->
    Bare.

%% Secondary, identity-based backstop for exactly the case
%% `select_discovery_seeds/3''s host/port normalization cannot catch:
%% the same station reachable under two genuinely DIFFERENT hostnames
%% (aliasing) -- those normalize to two different `{host, port}' pairs,
%% so the primary pass lets both through. This check asks whether any
%% currently-held link's actual live peer identity already matches, via
%% the SAME `find_link_by_node_id/2' the direct-dial literal-key-miss
%% path already uses for exactly this problem (see its own doc).
%% Without either check, discovery would dial a second connection to a
%% station already held; the station closes one of the two (one
%% connection per identity, keyed by `PeerNodeId' --
%% `macula_station_listener.erl'), the pool's monitor sees that as a
%% DOWN, respawns after 1s, gets closed again -- a permanent flap that
%% never self-heals since discovery never removes a seed. Unlike the
%% primary pass, this one can only see links that have completed HELLO
%% (a spawned-but-not-yet-connected or respawn-gap link answers
%% `undefined' here, same as no link at all) -- that gap is exactly why
%% the primary pass now also checks against `S#state.seeds' rather than
%% relying on this identity check alone.
already_connected_to(undefined, _Links) ->
    %% `list_stations' didn't carry a node_id for this row (defensive;
    %% every row in the real read model does). Can't rule the station
    %% in or out by identity here -- falls through to the host/port
    %% dedup above, no worse than before this check existed.
    false;
already_connected_to(NodeId, Links) ->
    find_link_by_node_id(NodeId, Links) =/= undefined.

add_one_discovered_seed(Seed, S) ->
    discovered_within_budget(spend_dial_budget(Seed, S), Seed).

%% A discovered station past the new-peer budget is left for a later
%% discovery run, and the deferral is counted like a refused dial.
discovered_within_budget({spent, S}, _Seed) ->
    count_refused_dial(new_peer_budget_spent, S);
discovered_within_budget({ok, S}, Seed) ->
    NewS = mark_discovered(Seed, start_link_for_seed(Seed, S)),
    replay_to_seed(maps:get(Seed, NewS#state.links, undefined), NewS).

%% The ONLY place `discovered' is ever set -- every other link
%% (bootstrap, direct-dial) keeps the record default `false' for its
%% whole life. `spawned_at' is set here too, once, since this always
%% runs on the SAME dial that created the entry (`start_link_for_seed/2'
%% just above never had a prior entry for a freshly discovered seed).
mark_discovered(Seed, #state{links = Links} = S) ->
    give_up_sweep_tag(maps:find(Seed, Links), Seed, S).

give_up_sweep_tag({ok, LinkState}, Seed, #state{links = Links} = S) ->
    Tagged = LinkState#link_state{
        discovered = true,
        spawned_at = erlang:monotonic_time(millisecond)},
    S#state{links = Links#{Seed => Tagged}};
give_up_sweep_tag(error, _Seed, S) ->
    %% `start_link_for_seed/2' always inserts an entry (connected or
    %% not) -- this branch is unreachable in practice, kept only so
    %% this function is total rather than crashing the pool.
    S.

%% Give up on any discovered link that has NEVER once connected and
%% has been alive longer than `GiveupAfterMs' -- frees its `Links'
%% entry (and its `max_links' slot) rather than letting it retry
%% forever, exactly as `already_connected_to/2''s own doc describes the
%% problem this solves for the case where the retry loop never even
%% produces a DOWN to hang a fix off (a dial against a
%% non-resolving/unreachable host can retry internally forever without
%% the link process itself ever dying -- see the seed()/Pinned-trust
%% design note). A discovered link proven connected on this same sweep
%% is marked `ever_connected' and never checked again.
sweep_stale_discovered_links(GiveupAfterMs, #state{links = Links} = S) ->
    Now = erlang:monotonic_time(millisecond),
    maps:fold(fun(Seed, LinkState, Acc) ->
                  check_discovered_link(Seed, LinkState, Now, GiveupAfterMs, Acc)
              end, S, Links).

check_discovered_link(Seed, #link_state{discovered = true, ever_connected = false,
                                        pid = Pid} = LinkState,
                      Now, GiveupAfterMs, S) when is_pid(Pid) ->
    judge_unconnected_link(safe_is_connected(Pid), Seed, LinkState, Now,
                           GiveupAfterMs, S);
check_discovered_link(_Seed, _LinkState, _Now, _GiveupAfterMs, S) ->
    %% Not a discovered link, already proven connected once, or has no
    %% live pid yet (mid-respawn -- the NEXT sweep tick will see it once
    %% `after_link_start/3' gives it a fresh pid, `spawned_at' intact).
    S.

judge_unconnected_link(true, Seed, LinkState, _Now, _GiveupAfterMs, S) ->
    set_ever_connected(Seed, LinkState, S);
judge_unconnected_link(false, Seed, #link_state{spawned_at = SpawnedAt, mon = Mon,
                                                pid = Pid}, Now, GiveupAfterMs, S)
  when is_integer(SpawnedAt), Now - SpawnedAt >= GiveupAfterMs ->
    give_up_on(Seed, Pid, Mon, S);
judge_unconnected_link(false, _Seed, _LinkState, _Now, _GiveupAfterMs, S) ->
    S.

set_ever_connected(Seed, LinkState, #state{links = Links} = S) ->
    S#state{links = Links#{Seed => LinkState#link_state{ever_connected = true}}}.

%% This seed is done, not bouncing, so how it was started goes with it.
give_up_on(Seed, Pid, Mon, #state{links = Links, dial_extra_opts = Opts} = S) ->
    macula_diagnostics:event(<<"_macula.client.discovery_link_given_up">>,
                             #{seed => Seed}),
    is_reference(Mon) andalso erlang:demonitor(Mon, [flush]),
    macula_station_link:stop(Pid),
    S#state{links = maps:remove(Seed, Links),
            dial_extra_opts = maps:remove(Seed, Opts)}.

%% Runs in a spawned worker (never the pool gen_server — the DHT lookup
%% + `list_stations' call are ordinary blocking RPCs with real network
%% round trips). Failure at any step (DHT lookup, `hecate_stations' not
%% currently advertised, the `list_stations' call itself) leaves the
%% pool exactly as it was; the next refresh tick tries again. This is
%% the fallback Raf's brief asked for: it falls out of "a failed
%% discovery attempt is just a no-op" rather than needing its own
%% special-cased error path.
run_station_discovery(Pool) ->
    Type = macula_record:type_procedure_advertisement(),
    resolve_realm_and_list(macula:find_records_by_type(Pool, Type), Pool).

resolve_realm_and_list({ok, Records}, Pool) ->
    list_stations_with_realm(find_list_stations_realm(Records), Pool);
resolve_realm_and_list({error, _Reason}, _Pool) ->
    ok.

%% `procedure_advertisement' is a mesh-wide DHT record type: ANY
%% identity can `_dht.put_record' one (the station's only gate is
%% `macula_record:verify/1', a signature/envelope check with no URI
%% shape validation), so unlike `hecate_stations.list_stations''s own
%% RESULT payload (first-party, this pool's own already-authenticated
%% call), this SET is not trusted input. A single malformed record
%% anywhere in the DHT view must not crash the whole discovery pass --
%% pre-filter to the shape `read_procedure_advertisement/1' actually
%% expects before calling it, and make every parsing step below total.
find_list_stations_realm(Records) ->
    Type = macula_record:type_procedure_advertisement(),
    WellFormed = [R || R = #{type := T, payload := P} <- Records,
                       T =:= Type, is_map(P)],
    Advertisements = [macula_record:read_procedure_advertisement(R) || R <- WellFormed],
    case lists:filtermap(fun realm_for_list_stations/1, Advertisements) of
        [Realm | _] -> {ok, Realm};
        []          -> error
    end.

%% `procedure_uri' is `<64 hex realm>/<procedure>' (see
%% `macula_record.erl''s own storage_key/1 doc). Exact-match the
%% procedure segment -- `advertise_direct' publishes a SECOND, distinct
%% record naming a `_/'-prefixed procedure for the direct-dial path;
%% this deliberately only matches the plain, gossip-routed one, since
%% discovery calls through the pool's own already-established links,
%% not direct-dial. Same filter macula-mcp's `mesh_list_stations' tool
%% already uses (`mesh_stations.ts'), the reference this was ported from.
%% `binary:split/3' with `[global]' yields 3+ segments for the `_/'
%% variant (and for any procedure name that itself contains a `/'), so
%% the 2-element pattern below rejects both without misparsing either.
realm_for_list_stations(#{procedure_uri := Uri}) when is_binary(Uri) ->
    match_procedure_uri(binary:split(Uri, <<"/">>, [global]));
realm_for_list_stations(_) ->
    false.

match_procedure_uri([RealmHex, ?LIST_STATIONS_PROCEDURE]) ->
    realm_from_hex(RealmHex);
match_procedure_uri(_) ->
    false.

%% Total over any peer-suppliable `RealmHex': a right-sized-but-non-hex
%% value would otherwise reach `binary:decode_hex/1' and `badarg' --
%% caught, not guarded against up front, since validating hex-ness
%% without a regex is awkward in pure guards. `try...catch', not a bare
%% `catch Expr' (deprecated, a hard compile failure under a consumer's
%% own `warnings_as_errors' -- see the commit replacing 12 other sites
%% of this same pattern earlier today).
realm_from_hex(Hex) when is_binary(Hex), byte_size(Hex) =:= 64 ->
    valid_realm(decode_hex_safe(Hex));
realm_from_hex(_Hex) ->
    false.

decode_hex_safe(Hex) ->
    try binary:decode_hex(Hex)
    catch _:_ -> invalid
    end.

valid_realm(Bin) when is_binary(Bin), byte_size(Bin) =:= 32 ->
    {true, Bin};
valid_realm(_NotAValidRealm) ->
    false.

list_stations_with_realm({ok, Realm}, Pool) ->
    report_discovered(macula:call(Pool, Realm, ?LIST_STATIONS_PROCEDURE,
                                  #{}, ?DISCOVERY_CALL_TIMEOUT_MS),
                      Pool);
list_stations_with_realm(error, _Pool) ->
    ok.

report_discovered({ok, Payload}, Pool) ->
    Seeds = station_seeds(Payload),
    Seeds =/= [] andalso gen_server:cast(Pool, {discovered_stations, Seeds}),
    ok;
report_discovered({error, _Reason}, _Pool) ->
    ok.

%% `payload_field/2' (macula_record.erl, exported) is the established
%% defensive accessor for reading an arbitrary wire-decoded payload
%% field in this codebase: `macula_frame:from_wire_envelope/1' collapses
%% a TEXT value into an atom whenever that atom already exists in this
%% VM (a station's `kind' field, literal text "station", silently
%% arriving as the atom `station' — already-atomized here — while
%% `hostname'/`city' values from the very same record stayed binaries —
%% never pre-declared atoms — is the exact, previously-hit bug this
%% guards). `quic_port'/`host_advertised' are `hecate_stations'' own
%% atoms, not declared anywhere in this SDK's own source, so they are
%% NOT guaranteed to already exist in an arbitrary caller's VM — hand-
%% rolled atom-keyed pattern matching against this reply would be
%% exactly that bug, not a hypothetical one.
%% Returns `[{Seed, NodeId}]', not bare seeds -- `NodeId' is what
%% `already_connected_to/2' needs to detect the same physical station
%% already held under a differently-spelled seed (see its own doc).
station_seeds(Payload) ->
    Stations = macula_record:payload_field(Payload, <<"stations">>),
    lists:filtermap(fun station_seed/1, list_or_empty(Stations)).

list_or_empty(L) when is_list(L) -> L;
list_or_empty(_)                 -> [].

station_seed(Station) when is_map(Station) ->
    seed_from_fields(macula_record:payload_field(Station, <<"hostname">>),
                     macula_record:payload_field(Station, <<"host_advertised">>),
                     macula_record:payload_field(Station, <<"quic_port">>),
                     macula_record:payload_field(Station, <<"node_id">>));
station_seed(_) ->
    false.

%% `hostname' -- the DNS name the station's own TLS certificate actually
%% covers -- is preferred whenever present, unchanged from before this
%% station ever had a fallback: it dials under the pool's normal
%% default (WebPKI), keeping full TLS-layer MITM resistance for the
%% common case (every fleet station checked so far has a working
%% hostname except one, deliberately -- see below).
%%
%% Only when a row has NO `hostname' at all does this fall back to
%% `host_advertised' (a bare IP literal -- checked live, every entry in
%% this fleet is a raw IPv6 address, never a DNS name) dialled under
%% Pinned trust: `expected_node_id' set to the row's own `node_id',
%% no TLS certificate pin is possible for a bare
%% IP to validate against WebPKI-style -- trust is enforced entirely at
%% the application layer, via the handshake's signed
%% `node_id', exactly the mode the peering layer
%% documents for "TLS terminated by a PKI unrelated to its macula
%% identity". Requires a `node_id' to pin against -- without one there
%% is nothing safe to authenticate a bare IP with, so it is skipped
%% exactly like a station with neither field at all.
%%
%% This priority (hostname first, IP+Pinned only as a genuine fallback)
%% was a deliberate choice, not the only one considered: a station's
%% `node_id' is present on essentially every row regardless of whether
%% it also has a working hostname, so a "prefer IP+Pinned whenever a
%% node_id exists" rule (checked directly against a real, independent,
%% already-shipped implementation of exactly that priority) would also
%% work mechanically -- live-verified against this exact fleet, dialing
%% an ordinary Let's-Encrypt-backed station by its bare IP under Pinned
%% trust connects just as cleanly as dialing it by hostname. It was
%% rejected as the DEFAULT ordering specifically because it means every
%% station's dial loses TLS-layer verification (`macula_quic' itself
%% logs "vulnerable to MITM ... outside development" for this mode),
%% not just the ones that genuinely have no other option -- a much
%% larger blast radius for no benefit to the common case, which already
%% has a perfectly good hostname to dial through WebPKI.
%%
%% UPDATE (2026-09-05): the producer-side bug this section originally
%% described is fixed (`macula_station_app:hostname_or_default/1',
%% deleted -- an unconfigured `geo.hostname' is now genuinely omitted,
%% not defaulted to the OS hostname). That fix alone is NOT sufficient
%% to make Toronto's row actually change, though, per a second
%% Fable review: `hecate_stations'' own read model
%% (`station_read_model:upsert_node_record/1') is read-modify-write --
%% a fresh announcement with `hostname' genuinely absent does not
%% clear an ALREADY-PERSISTED `hostname' value from a prior (buggy)
%% announcement, it only ever ADDS a field, never removes one. Toronto
%% announced "station-ca-toronto" under the old bug for a while, so
%% its existing `hecate_stations' doc likely still carries that value
%% until either a tombstone/retire cycle runs or `hecate_stations'
%% own upsert semantics change to make an announcer's record
%% authoritative for its own optional fields -- a separate, not-yet-
%% decided fix in a different service. This function is NOT the thing
%% to check for whether Toronto is actually reachable yet -- query
%% `hecate_stations.list_stations' directly and look at the real row.
%% This clause is still correct and worth keeping regardless: it is
%% exactly right for ANY row that genuinely has no `hostname' at all
%% (the `kind = daemon' rows already look like this), independent of
%% whichever specific station eventually clears its stale record.
seed_from_fields(Hostname, _HostAdvertised, Port, NodeId)
  when is_binary(Hostname), byte_size(Hostname) > 0, is_integer(Port) ->
    {true, {seed_url(unwrap_wire_text(Hostname), Port), NodeId}};
%% `NodeId' must be exactly 32 bytes, a node_id, or there is no identity
%% for the dial to expect, and the handshake would refuse it downstream.
%% Failing closed here instead means a malformed row never burns a
%% `max_links' slot until `giveup_after_ms' only to fail the same way.
seed_from_fields(_Hostname, HostAdvertised, Port, NodeId)
  when is_integer(Port), is_binary(NodeId), byte_size(NodeId) =:= 32 ->
    pinned_seed_from_host_advertised(HostAdvertised, Port, NodeId);
seed_from_fields(_Hostname, _HostAdvertised, _Port, _NodeId) ->
    false.

pinned_seed_from_host_advertised([Ip | _], Port, NodeId) ->
    ip_pinned_seed(unwrap_wire_text(Ip), Port, NodeId);
pinned_seed_from_host_advertised(_NotAList, _Port, _NodeId) ->
    false.

%% No `seed_url/2' bracketing here: this is a MAP seed, not a URL
%% string -- `macula_station_link:parse_seed/1' passes a `#{host,
%% port}' map through unchanged (no `uri_string:parse/1' involved), and
%% the underlying QUIC dial takes `host' as a plain literal either way
%% (live-verified with an unbracketed IPv6 `host' value).
ip_pinned_seed(Ip, Port, NodeId) when is_binary(Ip), byte_size(Ip) > 0 ->
    {true, {#{host => Ip, port => Port,
             expected_node_id => NodeId}, NodeId}};
ip_pinned_seed(_Ip, _Port, _NodeId) ->
    false.

%% Bracket only a literal IPv6 address (contains a colon) per RFC 3986 --
%% `hostname' never needs this. Only used for the hostname/WebPKI path;
%% the IP+Pinned fallback builds a map seed, not a URL string.
seed_url(Host, Port) ->
    PortBin = integer_to_binary(Port),
    HostBin = case binary:match(Host, <<":">>) of
                 nomatch -> Host;
                 _       -> <<"[", Host/binary, "]">>
              end,
    <<"quic://", HostBin/binary, ":", PortBin/binary>>.

%% Small local duplicate of `macula_record:unwrap_text/1' (not exported
%% there) — a `host_advertised' list ENTRY is not itself run through
%% `payload_field/2' (that only unwraps the field's own top-level
%% value, a list, not each element inside it), so an individual hostname
%% could in principle still arrive as `{text, Bin}' or an atom under the
%% exact same collapse rule. Low real risk (a DNS hostname colliding
%% with a pre-existing atom is unlikely) but cheap to guard correctly
%% rather than assume away.
unwrap_wire_text({text, B}) -> B;
unwrap_wire_text(B) when is_binary(B) -> B;
unwrap_wire_text(A) when is_atom(A), A =/= true, A =/= false,
                         A =/= undefined, A =/= null ->
    atom_to_binary(A, utf8);
unwrap_wire_text(V) -> V.

%%====================================================================
%% Internals — DOWN routing (link vs subscriber)
%%====================================================================

on_down(_Mon, Issuer, Reason, #state{issuer = Issuer} = S) ->
    {noreply, issuer_down(Reason, S)};
on_down(Mon, Pid, Reason, S) ->
    on_down_routed(find_link_by_mon(Mon, S), Mon, Pid, Reason, S).

%% The pool's issuer ended, and its links end with it. The pool starts a
%% new issuer after the backoff, reported, and holds every link start
%% until that issuer runs.
issuer_down(Reason, #state{issuer_started_at = StartedAt, issuer_backoff_ms = Backoff} = S) ->
    Delay = issuer_restart_delay(now_ms() - StartedAt, Backoff),
    ok = macula_diagnostics:bounded_event(warning, <<"_macula.client.issuer_down">>,
                                          #{reason => Reason, restart_in_ms => Delay}),
    erlang:send_after(Delay, self(), restart_issuer),
    S#state{issuer = undefined, issuer_backoff_ms = next_issuer_backoff(Delay), issuer_losses = S#state.issuer_losses + 1,
            link_opts = maps:remove(issuer, S#state.link_opts)}.

%% The delay before the pool starts a new issuer: the least once the issuer that ended has run a minute, and the backoff
%% before that.
-spec issuer_restart_delay(integer(), pos_integer()) -> pos_integer().
issuer_restart_delay(RanMs, _Backoff) when RanMs >= ?ISSUER_STABLE_MS -> ?ISSUER_RESTART_MIN_MS;
issuer_restart_delay(_RanMs, Backoff) -> Backoff.

%% The backoff after a delay: twice the delay, up to the most.
-spec next_issuer_backoff(pos_integer()) -> pos_integer().
next_issuer_backoff(Delay) -> min(2 * Delay, ?ISSUER_RESTART_MAX_MS).

%% A new issuer runs: the pool counts it, and the held link starts go ahead with it.
issuer_restarted({ok, Issuer}, #state{held_starts = Held, issuer_restarts = Restarts} = S) ->
    _ = erlang:monitor(process, Issuer),
    S1 = S#state{issuer = Issuer, issuer_started_at = now_ms(), held_starts = #{}, issuer_restarts = Restarts + 1,
                 link_opts = (S#state.link_opts)#{issuer => Issuer}},
    maps:fold(fun held_start_resumed/3, S1, Held);
issuer_restarted({error, Reason}, #state{issuer_backoff_ms = Backoff} = S) ->
    ok = macula_diagnostics:bounded_event(warning, <<"_macula.client.issuer_start_failed">>,
                                          #{reason => Reason, restart_in_ms => Backoff}),
    erlang:send_after(Backoff, self(), restart_issuer),
    S#state{issuer_backoff_ms = next_issuer_backoff(Backoff)}.

held_start_resumed(Seed, ExtraOpts, S) ->
    Started = start_link_for_seed(Seed, ExtraOpts, S),
    replay_to_seed(maps:get(Seed, Started#state.links, undefined), Started).

on_down_routed({ok, Seed}, _Mon, Pid, Reason, S0) ->
    macula_diagnostics:event(<<"_macula.client.link_down">>,
                             #{seed => Seed, pid => Pid, reason => Reason}),
    erlang:send_after(?LINK_RESPAWN_DELAY_MS, self(), {respawn_link, Seed}),
    S = exit_kept(Reason, Seed, S0),
    S1 = S#state{links = maps:remove(Seed, S#state.links),
                 link_subs = maps:remove(Pid, S#state.link_subs)},
    {noreply, maybe_rediscover_now(S1)};
on_down_routed(error, Mon, _Pid, _Reason, S) ->
    {noreply, on_subscriber_down(Mon, S)}.

%% The moment every currently-held link is gone is exactly the moment
%% "the world changed" is most likely true -- re-discover soon rather
%% than wait for the next periodic tick. Only fires when discovery is
%% enabled; a pool without it relies purely on the existing per-seed
%% respawn timer, unchanged. Delayed past `?LINK_RESPAWN_DELAY_MS' (the
%% existing per-seed respawn timer already pending for every link this
%% just removed) rather than firing at 0: discovery's own worker calls
%% back into this SAME pool (`macula:find_records_by_type/2' -> a plain
%% CALL), so running it with genuinely zero links connected can only
%% ever return `no_healthy_station' -- dead on arrival, not "the world
%% changed" at all. Giving the ordinary respawn a chance to reconnect
%% first is what actually gives this trigger something to work with.
maybe_rediscover_now(#state{discovery = undefined} = S) ->
    S;
maybe_rediscover_now(S) ->
    rediscover_if_no_links(spawned_link_pids(S), S).

rediscover_if_no_links([], S) ->
    schedule_discovery(?LINK_RESPAWN_DELAY_MS + ?INITIAL_DISCOVERY_DELAY_MS, S);
rediscover_if_no_links(_Pids, S) ->
    S.

find_link_by_pid(Pid, #state{links = Links}) ->
    case [Seed || {Seed, #link_state{pid = P}} <- maps:to_list(Links), P =:= Pid] of
        [Seed | _] -> {ok, Seed};
        []         -> error
    end.

%% A link's own account of its disconnect: its reason's name and the facts
%% the link chose to name, never the reason's terms.
disconnect_kept({ok, Seed}, Summary, #state{last_disconnects = Last} = S) ->
    S#state{last_disconnects = Last#{Seed => Summary}};
disconnect_kept(error, _Summary, S) ->
    S.

%% A link that ended without saying why (killed, crashed) is kept by its exit
%% reason's name. A `normal' exit follows a disconnect the link already told
%% us about, or a stop the pool asked for, so it replaces nothing.
exit_kept(normal, _Seed, S) ->
    S;
exit_kept(Reason, Seed, #state{last_disconnects = Last} = S) ->
    S#state{last_disconnects = Last#{Seed => #{reason => macula_reason_name:text(Reason),
                                               at_ms => erlang:system_time(millisecond)}}}.

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
    Segments = topic_segments(T),
    track_wildcard(lists:member(<<"*">>, Segments), Key, Segments,
                   S#state{subs = NewSubs, topic_index = NewIdx}).

track_wildcard(true, Key, Segments, #state{wildcard_topics = W} = S) ->
    S#state{wildcard_topics = W#{Key => Segments}};
track_wildcard(false, _Key, _Segments, S) ->
    S.

drop_sub(SubRef, #state{subs = Subs} = S) ->
    drop_sub_take(maps:take(SubRef, Subs), SubRef, S).

drop_sub_take(error, _SubRef, S) ->
    S;
drop_sub_take({#sub_spec{realm = R, topic = T, mon = Mon}, NewSubs},
              SubRef, #state{topic_index = Idx} = S) ->
    erlang:demonitor(Mon, [flush]),
    Key = {R, T},
    NewSet = sets:del_element(SubRef, maps:get(Key, Idx, sets:new())),
    Empty = sets:is_empty(NewSet),
    NewIdx = on_index_after_drop(Empty, Key, NewSet, Idx),
    unsubscribe_links(Empty, Key,
                      S#state{subs = NewSubs, topic_index = NewIdx,
                              wildcard_topics = wildcards_after_drop(Empty, Key, S#state.wildcard_topics)}).

wildcards_after_drop(true,  Key, W) -> maps:remove(Key, W);
wildcards_after_drop(false, _Key, W) -> W.

on_index_after_drop(true,  Key, _Set, Idx) -> maps:remove(Key, Idx);
on_index_after_drop(false, Key,  Set, Idx) -> Idx#{Key => Set}.

issue_wire_subs(true, _Realm, _Topic, S) ->
    %% A sibling consumer already triggered the wire-level subscribe;
    %% the pool fans out to every local SubRef on inbound EVENT.
    S;
issue_wire_subs(false, Realm, Topic, S) ->
    PoolPid = self(),
    Key = {Realm, Topic},
    lists:foldl(fun(P, Acc) ->
                        record_link_sub(P, Key, macula_station_link:subscribe(P, Realm, Topic, PoolPid), Acc)
                end, S, spawned_link_pids(S)).

%% Keeps the SubRef a link returned for Key's SUBSCRIBE, so unsubscribe can
%% reach that link. A link that did not accept it keeps nothing.
record_link_sub(LinkPid, Key, {ok, LinkSubRef}, #state{link_subs = LS} = S) ->
    Keys = maps:get(LinkPid, LS, #{}),
    S#state{link_subs = LS#{LinkPid => Keys#{Key => LinkSubRef}}};
record_link_sub(_LinkPid, _Key, _NotAccepted, S) ->
    S.

%% When the last local subscriber of Key has left, each link that carried
%% its SUBSCRIBE is told to send UNSUBSCRIBE. The pool does not wait on the
%% link, and the request reaches the link ahead of any later subscribe the
%% pool sends it.
unsubscribe_links(false, _Key, S) ->
    S;
unsubscribe_links(true, Key, #state{link_subs = LS} = S) ->
    S#state{link_subs = maps:map(fun(LinkPid, Keys) -> unsubscribe_link(LinkPid, Key, Keys) end, LS)}.

unsubscribe_link(LinkPid, Key, Keys) ->
    unsubscribed_link(maps:take(Key, Keys), LinkPid, Keys).

unsubscribed_link({LinkSubRef, Rest}, LinkPid, _Keys) ->
    ok = macula_station_link:unsubscribe_async(LinkPid, LinkSubRef),
    Rest;
unsubscribed_link(error, _LinkPid, Keys) ->
    Keys.

%% Drops a SubRef a link reported gone, from whichever link held it.
forget_link_sub(LinkSubRef, #state{link_subs = LS} = S) ->
    S#state{link_subs = maps:map(fun(_LinkPid, Keys) -> without_link_sub(LinkSubRef, Keys) end, LS)}.

without_link_sub(LinkSubRef, Keys) ->
    maps:filter(fun(_Key, Ref) -> Ref =/= LinkSubRef end, Keys).

%%====================================================================
%% Internals — inbound event fan-out
%%====================================================================

%% The subscriptions an event reaches: those of its own topic and those of
%% every wildcard pattern it matches (`macula_topic_pattern:matches/2'), as
%% one set, so the publication is checked once and each subscription gets it
%% once.
matching_subscriptions(Realm, Topic, #state{topic_index = Idx, wildcard_topics = Wildcards}) ->
    Keys = [{Realm, Topic} | matching_wildcards(Realm, Topic, Wildcards)],
    subscriptions_found([Set || {ok, Set} <- [maps:find(Key, Idx) || Key <- Keys]]).

subscriptions_found([]) -> error;
subscriptions_found(Sets) -> {ok, sets:union(Sets)}.

%% The wildcard patterns in `Realm' that `Topic' matches. A topic that is
%% itself one of them is found by the exact lookup.
matching_wildcards(Realm, Topic, Wildcards) ->
    Segments = topic_segments(Topic),
    [Key || {{R, T} = Key, Pattern} <- maps:to_list(Wildcards),
            R =:= Realm, T =/= Topic,
            macula_topic_pattern:matches(Pattern, Segments)].

topic_segments(Topic) ->
    binary:split(Topic, <<"/">>, [global]).

%% A publication is checked, and so recorded, only while a subscription
%% matches it: a copy that arrives while nothing is subscribed must not
%% hide the publication from a later subscriber. The check drops a
%% publication whose expiry has passed.
on_inbound_event(error, _Hash, _ExpiresAt, _Topic, _Payload, _Meta, S) ->
    S;
on_inbound_event({ok, Set}, Hash, ExpiresAt, Topic, Payload, Meta, S) ->
    on_sighting(macula_client_dedup:check(S#state.dedup_tab, Hash, ExpiresAt,
                                          erlang:system_time(millisecond)),
                Set, Topic, Payload, Meta, S).

on_sighting(new, Set, Topic, Payload, Meta, S) ->
    ensure_flush_timer(fan_to_set(Set, Topic, Payload, Meta, S));
on_sighting(_DuplicateOrExpired, _Set, _Topic, _Payload, _Meta, S) ->
    S.

fan_to_set(Set, Topic, Payload, Meta, S) ->
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
                         Order, order_key(Meta), maps:get(seq, Meta),
                         {Payload, Meta}, now_ms()),
    send_events(Pid, SubRef, Topic, Events),
    S#state{subs = maps:put(SubRef, Spec#sub_spec{order = Order2},
                            S#state.subs)}.

%% Every event reaching the pool carries a publication its link verified,
%% so each publisher's ordering state is its own.
order_key(#{publisher := Pub}) -> Pub.

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

%% A crash or exit from ONE selected link (dead pid between selection
%% and call, a wedged connection timing out its 5s gen_server:call,
%% the link process itself erroring) must never take the whole
%% fan-out worker down with it. This list comprehension has no other
%% guard, and an unhandled exit here skips straight past
%% `gen_server:reply/2' entirely -- the caller then hangs until its
%% OWN timeout and gets a hard error, even if an EARLIER link in
%% `Selected' already accepted the frame. That specific ordering
%% (success then a later failure) was structurally impossible at the
%% old default replication_factor=1 (never more than one element to
%% fail "after"); raising the default to 2 makes it a real, common-path
%% risk for the first time. Same idiom as safe_link_advertise/5 below,
%% which this fan-out should have matched from the start.
safe_link_publish(Pid, Realm, Topic, Payload, Seq) ->
    try macula_station_link:publish(Pid, Realm, Topic, Payload, Seq)
    catch _:Reason -> {error, Reason}
    end.

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

-ifdef(TEST).
%% The position of a field in the state tuple, read from the record itself, for the key redaction tests that set a
%% field to the whole state on purpose: a test names the field, so a field added to the record cannot shift it.
state_field_index(Field) ->
    field_index(Field, record_info(fields, state), 2).

field_index(Field, [Field | _Rest], Index) -> Index;
field_index(Field, [_Other | Rest], Index) -> field_index(Field, Rest, Index + 1).
-endif.
