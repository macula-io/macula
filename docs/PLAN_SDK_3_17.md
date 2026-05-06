# PLAN — Macula SDK 3.17.0

**Status:** Drafted, awaiting kickoff
**Date:** 2026-05-06
**Predecessor:** `PLAN_SDK_3_16.md` (shipped as v3.16.0)

---

## tl;dr

3.16 shipped five of six daemon-driven gaps plus an SDK quality-pass
warm-up. 3.17 finishes Group A (the streaming-RPC piece), runs the
full quality sweep (was Group B), refreshes guides + diagrams (was
Group C), and extends test coverage (was Group D).

Roughly: **the heavy work the 3.16 release deliberately deferred.**

## Carry-over from 3.16

### A4 — pool-aware streaming RPC (the deferred gap)

**Why:** V1 streaming RPC routes through `macula_mesh_client`. V2
`macula_station_link` has NO stream variants. `macula:call_stream/3,4`
and `macula:advertise_stream/3,4` fall through to V1, which means
the daemon's streaming RPC path (currently `git_over_mesh`,
`hecate_mesh_inproc`) cannot migrate to V2 until this lands.

**Scope estimate:** 400+ LOC across 2-3 commits; new wire-level
stream-frame plumbing in `macula_station_link` plus pool wrappers in
`macula_client`. Plus integration tests covering local + over-wire.

**Per-station surface (in `macula_station_link`):**
```erlang
-spec macula_station_link:call_stream(pid(), <<_:256>>, binary(),
                                       term(), map()) ->
    {ok, stream()} | {error, term()}.
-spec macula_station_link:advertise_stream(pid(), <<_:256>>, binary(),
                                            stream_mode(),
                                            stream_handler()) ->
    ok | {error, term()}.
-spec macula_station_link:unadvertise_stream(pid(), <<_:256>>, binary())
    -> ok.
```

**Pool surface (in `macula`):**
```erlang
-spec macula:call_stream(pool(), realm(), procedure(), term(), map()) ->
    {ok, stream()} | {error, term()}.
-spec macula:advertise_stream(pool(), realm(), procedure(),
                               stream_mode(), stream_handler()) ->
    ok | {error, term()}.
```

**Open design questions** (decide during implementation):
1. **Stream stickiness on the pool.** Once a `call_stream` opens
   against link L1, do mid-stream chunks stay on L1 or can they
   migrate if L1 dies? Recommended: sticky-to-link, error on link
   death, caller retries.
2. **Replay of advertised streams on link respawn.** `advertise/4`
   replay was added in 3.16; mirror that for stream advertisements
   so spawned-late links pick up handlers automatically.
3. **Reuse vs. fork of `macula_stream_v1` machinery.** The V1 stream
   gen_server already exists (569 LOC) and bridges to mesh_client.
   Decision: extend `macula_stream_v1` with a `{remote_via_link, Pid,
   Sid}` peer shape parallel to its current `{remote, Client, Sid}`,
   OR fork into `macula_stream_v2` for clean separation. Lean toward
   **extend** to avoid duplication; revisit if the change set grows.

## Group B — full SDK sanitization sweep

User-requested "(c) full sweep" treatment per
`PLAN_DAEMON_V2_MIGRATION.md` discussion. Apply the SDK CLAUDE.md
style rules across every module:

- max 1-2 nesting levels
- multi-clause functions > `case` > `if`
- guards > `case` where viable
- no `try..catch` (let it crash; pattern-match return values).
  Documented exception lives in `macula_pubsub:invoke/4` where the
  SDK guards an opaque user callback — keep that one.
- declarative > imperative

**Cadence:** module-by-module, 1-3 modules per commit, public exports
preserved exactly. Internal helpers may rename if the new name screams
intent better.

**Order (high-traffic first):**
1. `macula_client`, `macula_pubsub` (already touched in 3.16; light
   sweep)
2. `macula_station_link` (touched by A4; sweep alongside)
3. `macula_peering_conn`, `macula_peering`
4. `macula_record`, `macula_frame`
5. `macula_identity`, `macula_did`, `macula_ucan`
6. `macula_dist_*` (Erlang dist over mesh — large surface)
7. `macula_net_*` (substrate; recently introduced, may already be
   close to style)
8. Everything else, alphabetical

A running checklist lives in
`docs/PLAN_SDK_3_17_PROGRESS.md` (created on kickoff).

## Group C — docs / guides / diagrams

Audit the 10 guides in `docs/guides/` against post-sweep code. New
guides:

- **`DHT_GUIDE.md`** — Kademlia primitives, transport wiring, link to
  `macula-station/plans/PLAN_DHT_TRANSPORT_WIRING.md`. Explains the
  `send_frame` callback model the station ships with.
- **`PEER_OBSERVER_GUIDE.md`** — the routing-table-of-conns pattern,
  controlling_pid contract, how observers self-form from
  `peer_links`. Useful for anyone building their own
  station-equivalent.
- **`REGRESSION_PATTERNS.md`** — captured lessons from Phase 4 + Phase
  A1 of the recovery work: controlling_pid divergence,
  source-of-truth reconciliation, the V1→V2 outbound_link saga.

New SVG diagrams (in `assets/`):
- DHT lookup walk (5 nodes, α=3)
- Peering event flow showing the `controlling_pid` contract and where
  it can break
- Self-forming reconciliation pattern (init replay + monitor)

## Group D — test extension

Where 3.16's gap implementations exposed thin coverage:

- Address propagation in peering's `connected` event roundtrip
- Pool fan-out semantics under churn (advertise reaches all healthy;
  call returns first-success even when some links time out)
- Stream RPC roundtrip on station_link (post-A4)
- `status/1` shape stability under heavy churn (sub add/remove,
  link respawn)

Audit whether the codebase already uses `proper` for property-based
testing; if so, add property tests for the pool RPC fan-out
invariants.

## Group E — release hygiene

- Bump `vsn` `3.16.0` → `3.17.0` in `src/macula.app.src`
- CHANGELOG.md entry (Keep-a-Changelog format)
- README highlights for streaming-RPC + sweep summary
- `rebar3 dialyzer` clean
- `rebar3 ex_doc` clean (zero warnings)
- Visually verify guides + images render in local doc build before
  shipping
- Hex publish (operator)

## Sequencing

```
A4 streaming RPC (per-station) → A4 pool wrappers → A4 tests
     │
     ↓
Group B sweep (concurrent, different files)
     ↓
Group C docs / new guides / diagrams
     ↓
Group D test extension fills gaps
     ↓
Group E release 3.17.0
```

Each section is bounded; pause between groups if priorities shift.

## What 3.16 already shipped (for reference)

| | What | Commit |
|---|---|---|
| A1 | `macula:status/1` | `d434e2a` |
| A2 | `macula:subscribe_callback/4` | `0a7190a` |
| A3 | Pool RPC (`call/5`, `advertise/5`, `unadvertise/3`) | `4a1f8d9` |
| A5 | `connect/2` opts audit + V1-legacy warning | `9057110` |
| A6 | dedup config tunability verified | `9f4a31f` |

3.16 also archived pre-3.0 changelog history to `CHANGELOG_LEGACY.md`
(intentionally not rendered by ex_doc) and got the documentation
build to zero warnings.
