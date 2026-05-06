# PLAN — Macula SDK 3.16.0

**Status:** Active  
**Date:** 2026-05-06  
**Driving consumer:** hecate-daemon V1→V2 migration (`PLAN_DAEMON_V2_MIGRATION.md`)

---

## tl;dr

Five SDK gaps surfaced during daemon migration drafting. This release lands them as
purely additive APIs (no breaking change) plus a quality sweep + docs refresh.

Sequencing: **gaps first, sweep second, docs third, release last.** Each gap is a
self-contained commit with eunit coverage. Sanitization stays internal-only and
preserves all public exports.

## Baseline (2026-05-06)

| Check | State |
|---|---|
| `rebar3 compile` | clean |
| `rebar3 eunit` | 743/743 pass; one teardown timeout flake (`macula_multi_relay_tests:stop_test/1`) |
| `rebar3 dialyzer` | clean (89 files) |
| `rebar3 ex_doc` | exit 0; 4 minor warnings (broken doc refs to `set_handler/1`, `send/2`, hidden `macula_station_link:call/5` from CHANGELOG.md, unclosed backtick) |

## Group A — daemon-driven additions

Numbering matches `PLAN_DAEMON_V2_MIGRATION.md` §3.

### A1 — `macula:status/1` (was §3.2)

**Why:** Daemon needs a single roll-up of pool health for `/api/mesh/activate`. V1 had
`macula_multi_relay:get_status/1`. V2 has `list_nodes/1` + per-link `is_connected/1`
but no aggregate.

**Surface:**
```erlang
-spec macula:status(pool()) -> {ok, status()}.
-type status() :: #{
    seeds              := [seed()],
    healthy_links      := non_neg_integer(),
    failed_links       := non_neg_integer(),
    realm              := binary() | undefined,
    self_node_id       := macula_identity:pubkey() | undefined,
    pending_subs       := non_neg_integer(),
    pending_advs       := non_neg_integer()
}.
```

Implementation: query `macula_client` state via gen_server call; aggregate over its
known links.

### A2 — `macula:subscribe_callback/4` (was §3.3)

**Why:** V2 `subscribe/4` delivers `{macula_event, Sub, Topic, Payload, Meta}` messages
to a subscriber pid. Daemon + stub each have their own ad-hoc receive loops wrapping
this to invoke a `fun(Topic, Payload) -> ...)` callback. One blessed implementation
in the SDK avoids three subtly-different copies.

**Surface:**
```erlang
-spec macula:subscribe_callback(pool(), realm(), topic(),
                                fun((topic(), term(), map()) -> any())) ->
    {ok, sub()} | {error, term()}.
```

Internal: spawns a small linked receiver, invokes the fun on each event, traps and logs
crashes so a misbehaving callback can't take down the consumer.

### A3 — pool-aware non-streaming RPC (was §3.0)

**Why:** `macula_client` (pool) currently exposes only pubsub. RPC lives at
`macula_station_link` per-station. The daemon hand-rolls fan-out via
`call_via_any_station/5` etc. Lifting that into the SDK consolidates one well-tested
implementation.

**Surface:**
```erlang
-spec macula:call(pool(), realm(), procedure(), term(), pos_integer()) ->
    {ok, term()} | {error, term()}.
-spec macula:advertise(pool(), realm(), procedure(), handler()) ->
    ok | {error, term()}.
-spec macula:unadvertise(pool(), realm(), procedure()) -> ok.
```

Semantics: `call` = first-success across pool; `advertise` = fan-out to all healthy
links; `unadvertise` = fan-out drop. Mirrors the daemon's existing helper shape.

### A4 — pool-aware streaming RPC (was §3.1)

**Why:** Streaming RPC paths in `macula:call_stream/3,4` and `macula:advertise_stream/3,4`
delegate to V1 `macula_mesh_client`. `macula_station_link` has NO stream variants.
Daemon Phase A2 (streaming RPC migration) is blocked on this.

**Surface (per-station, in `macula_station_link`):**
```erlang
-spec macula_station_link:call_stream(pid(), <<_:256>>, binary(), term(), map()) ->
    {ok, stream()} | {error, term()}.
-spec macula_station_link:advertise_stream(pid(), <<_:256>>, binary(),
                                            stream_mode(), stream_handler()) ->
    ok | {error, term()}.
-spec macula_station_link:unadvertise_stream(pid(), <<_:256>>, binary()) -> ok.
```

**Surface (pool-level, in `macula`):**
```erlang
-spec macula:call_stream(pool(), realm(), procedure(), term(), map()) ->
    {ok, stream()} | {error, term()}.
-spec macula:advertise_stream(pool(), realm(), procedure(), stream_mode(),
                               stream_handler()) -> ok | {error, term()}.
```

Largest piece of work in this release; new wire-level stream-frame plumbing in
`macula_station_link`.

### A5 — `connect/2` opts surface audit (was §3.4)

**Why:** V1's `macula_multi_relay:start_link` took `#{relays, realm, identity, site,
connections}`. V2's `macula:connect(Seeds, Opts)` accepts a less-documented `Opts`
map. Need to verify what's accepted; if `site` / `connections` (used by daemon today)
have no V2 equivalent, surface as additional opts.

**Action:** read `macula_client:connect/2` thoroughly, add missing opts, document the
full surface in module @doc.

### A6 — pool dedup config exposure (was §3.5)

**Why:** `macula_client` doc references `dedup_window_ms` / `dedup_sweep_ms`. Verify
caller-tunable; expose if not.

**Surface:** opts in `macula:connect/2` accepting these values; doc them.

## Group B — sanitization sweep

Full SDK pass per user's "(c) full sweep" choice. Erlang style per the SDK CLAUDE.md:

- Max 1-2 levels of nesting
- Multi-clause functions over `case` over `if`
- Guards over `case` where viable
- Avoid `try..catch` (let it crash; pattern-match on return values)
- Declarative > imperative

**Cadence:** module-by-module, 1-3 modules per commit. Public exports preserved
exactly. Internal helpers may be renamed if the rename is more screaming.

**Order of attack:**
1. Modules touched by Group A (already in flight; sweep as part of those commits)
2. High-traffic modules: `macula_client`, `macula_station_link`, `macula_pubsub`, 
   `macula_record`, `macula_frame`, `macula_peering_conn`
3. Identity / crypto: `macula_identity`, `macula_did`, `macula_ucan`
4. DHT (vendored from station umbrella later? track)
5. Everything else

A running checklist will be maintained at `docs/PLAN_SDK_3_16_PROGRESS.md` once
Group B starts.

## Group C — docs / guides / diagrams

- Audit all 10 existing guides in `docs/guides/` against post-sweep code
- Resolve the 4 baseline ex_doc warnings
- New guides:
  - `DHT_GUIDE.md` (Kademlia primitives + transport wiring; cross-link with macula-station's PLAN_DHT_TRANSPORT_WIRING.md)
  - `PEER_OBSERVER_GUIDE.md` (the routing-table-of-conns pattern)  
  - `REGRESSION_PATTERNS.md` (controlling_pid divergence, source-of-truth reconciliation; lessons from Phase 4 + Phase A1)
- New diagrams (SVG, in `assets/`):
  - DHT lookup walk
  - peering event flow (controlling_pid contract)
  - self-forming reconciliation pattern

## Group D — test extension

- Extend coverage where Group A/B touched code
- Property-based tests if the codebase already uses `proper` (audit first)
- Specifically:
  - Address propagation roundtrip in peering connected event
  - Pool fan-out semantics (advertise reaches all healthy, call returns first-success)
  - Stream RPC roundtrip on station_link
  - Status/1 shape stability under churn

## Group E — release hygiene

- Bump `vsn` from `3.15.3` → `3.16.0` in `src/macula.app.src`
- CHANGELOG.md entry (Keep-a-Changelog format; include "Why" context per existing voice)
- README highlights for new features
- `rebar3 dialyzer` clean
- `rebar3 ex_doc` builds without warnings; visually verify guides + images render
- `rebar3 hex publish` (operator does this manually)

## Sequencing summary

```
A1 status/1            → A2 subscribe_callback/4   → A3 pool RPC
   ↓ (independent)
A5 connect opts audit
   ↓
A4 streaming RPC       → unblocks daemon Phase A2
   ↓
A6 dedup config
   ↓
B sweep (module-by-module; can run concurrent with C/D)
C docs
D tests
E release 3.16.0
```

Each Group A step is one or two commits; B is many small commits; C/D are batched.

## Open decisions

1. Does `macula:status/1` need Prometheus-metric-style fields (counters, gauges)
   or is a snapshot map sufficient?
2. Should `subscribe_callback/4` use a `link` or `monitor` to the consumer for
   cleanup-on-death? Memo the answer here once decided.
3. Streaming RPC pool semantics: which station gets the stream frames? Sticky to
   first-success station, or free routing per chunk?
4. `connect/2` opts compatibility: drop V1's `connections` integer (was relay count)
   if no V2 use, or honor as max-pool-size?
