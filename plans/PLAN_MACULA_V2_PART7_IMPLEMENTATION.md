# PLAN — Macula V2, Part 7: Implementation Plan

**Parent:** `PLAN_MACULA_V2_ROOT.md`
**Depends on:** Parts 1–6 (complete architectural spec).
**Feeds:** Part 8 (Verification — tests map to modules defined here).
**Status:** Draft — authored 2026-04-14.
**Scope:** Phase-by-phase build schedule. Repository skeleton. Module boundaries + naming. Rebar3 layout. Migration from V1 to V2. CI + release pipeline. No algorithmic behaviour (Parts 3–5), no wire formats (Part 6), no verification strategy (Part 8).

---

## 1. Purpose

Part 7 is the bridge from design to code. It answers: *in what order do we build, where does code live, and how does each phase prove itself before the next starts?*

Three constraints shape the plan:

1. **Walking skeleton first.** Two stations exchanging one signed record end-to-end is the minimum viable artifact; everything else is iteration on top.
2. **Phase-gated.** Each phase has an acceptance test (Part 8) that must pass before the next phase begins. No stacked debt.
3. **Clean repo.** New code goes into `hecate-social/hecate-station` (new private repo) + `macula-io/macula` (V2 branch = new main). V1 codebase (`macula-relay`) receives no changes.

---

## 2. Repository skeleton

### 2.1 Two active repos

| Repo | Role | Branch model |
|------|------|--------------|
| `macula-io/macula` | Macula SDK (client library for consumers) | V1 frozen on `v1.x` branches; V2 develops on `main` (hex `macula` 2.x) |
| `hecate-social/hecate-station` | Reference station implementation (the product) | `main` is V2; no V1 history |

### 2.2 Archived repos

| Repo | Disposition |
|------|-------------|
| `macula-io/macula-relay` | Archived; README redirects to `hecate-station`; V1 Docker images continue to run in legacy fleet until Phase 8 cutover. |

### 2.3 Continuously active repos (consuming V2 later)

| Repo | Role | V2 adoption |
|------|------|-------------|
| `hecate-social/hecate-daemon` | User-facing Hecate runtime | Phase 7+ (once SDK stable) |
| `macula-io/macula-realm` | Realm server | Phase 7+ |
| `macula-io/macula-demo` | Fleet GitOps | Docker image swap at Phase 8 |

---

## 3. `hecate-station` rebar3 layout

Umbrella layout. Root app is `hecate_station`; domain apps live under `apps/`.

```
hecate-station/
├── apps/
│   ├── hecate_station/              % root; supervises, owns config, admin endpoint
│   │   ├── src/
│   │   │   ├── hecate_station.erl           % facade + lifecycle API
│   │   │   ├── hecate_station_app.erl
│   │   │   ├── hecate_station_sup.erl
│   │   │   ├── hecate_station_config.erl    % loads config, derives node_record
│   │   │   ├── hecate_station_health.erl    % /status endpoint + metrics
│   │   │   ├── hecate_station_admin.erl     % /admin endpoint + authn
│   │   │   └── hecate_station.app.src
│   │   └── rebar.config
│   │
│   ├── macula_identity/              % StationId/NodeId generation, crypto puzzle
│   ├── macula_record/                % PKARR record CBOR encode/decode + sign/verify
│   ├── macula_frame/                 % BERT frame encode/decode + sign/verify
│   ├── macula_transport/             % QUIC wrapper over quicer NIF
│   ├── macula_peering/               % peer-client + peer-server state machines (Part 4 §10)
│   ├── macula_handler/               % handler-pid lifecycle; single-provider enforcement
│   ├── macula_dht/                   % S/Kademlia: buckets, lookups, STORE path
│   ├── macula_swim/                  % SWIM-Lifeguard
│   ├── macula_routing/               % Suurballe, source-route header, path cache
│   ├── macula_bootstrap/             % 5-tier bootstrap cascade
│   ├── macula_overlay/               % HyParView + Plumtree (intra-realm)
│   ├── macula_realm/                 % realm directory, endorsement flows
│   └── macula_diagnostics/           % structured logs + observability emitters
│
├── plans/                           % PLAN_MACULA_V2_*.md (this plan) after repo creation
├── config/
│   ├── sys.config
│   └── vm.args
├── rebar.config
├── rebar.lock                       % in .gitignore (per MEMORY)
├── .gitignore
├── .github/workflows/               % CI
├── Dockerfile
├── README.md
└── LICENSE                          % Apache-2.0
```

### 3.1 Module-per-app discipline

One coherent concern per app. An app that grows beyond ~3000 LOC is a sign to split. Apps named `macula_*` implement pure protocol; apps named `hecate_station_*` are product shell (Part 1 §10.4).

### 3.2 Dependency arrows

```
hecate_station_app (root)
  └─ macula_bootstrap ── macula_dht ── macula_peering ── macula_transport
                                                   └── macula_frame
                                                   └── macula_record
     macula_overlay ── macula_dht
     macula_handler ── macula_frame
     macula_swim ── macula_transport
     macula_routing ── macula_dht
     macula_realm ── macula_record
     macula_identity   (used everywhere; no deps besides stdlib + crypto)
     macula_diagnostics (used everywhere; no deps)
```

No cycles. Apps reach downward only; `hecate_station_*` apps may depend on any `macula_*` app but not vice versa.

---

## 4. Module catalog (naming)

### 4.1 Conventions

- **`macula_<concern>`** for protocol-layer modules (reusable into `macula` SDK if extracted later).
- **`hecate_station_<concern>`** for station-shell modules (product-specific).
- **`<verb>_<noun>`** for command-style modules when vertical slicing applies (e.g. `register_station_in_realm.erl`).
- Erlang modules use snake_case; types use lower_snake_case; macros SCREAMING_SNAKE.

### 4.2 Key module inventory

| Module | Role |
|--------|------|
| `macula_identity:new_node_id/0` | Generate + mint NodeId with crypto puzzle |
| `macula_identity:verify_puzzle/1` | Check leading-zeros invariant |
| `macula_record:sign/2`, `verify/1` | PKARR record sign/verify (Part 6 §10) |
| `macula_record:canonical_cbor/1` | Deterministic encoding |
| `macula_frame:encode/1`, `decode/1` | BERT frame codec |
| `macula_transport:open_quic/2`, `close/1` | QUIC connection lifecycle |
| `macula_peering:connect/2` | Peer state machine (Part 4 §10) |
| `macula_peering:refresh/1` | REFRESH phase (Part 4 §7) |
| `macula_handler:register/3`, `unregister/2` | Pillar 1 + 2 registry |
| `macula_handler:heartbeat/1` | Pillar 3 |
| `macula_dht:lookup/2`, `store/2`, `find_node/2` | DHT ops (Part 3 §10) |
| `macula_dht:admit_peer/2` | Tier-diverse bucket admission (Part 3 §4.3) |
| `macula_swim:join/2`, `leave/1` | SWIM group membership |
| `macula_swim:probe_round/1` | Lifeguard probe logic |
| `macula_routing:compute_paths/3` | Suurballe k=3 disjoint (Part 3 §6.3) |
| `macula_routing:verify_header/2` | Source-route per-hop check (Part 6 §11.2) |
| `macula_bootstrap:cascade/1` | 5-tier cascade (Part 5 §3) |
| `macula_overlay:join_realm/2` | HyParView join |
| `macula_overlay:publish/3`, `subscribe/3` | Plumtree push-lazy |
| `macula_realm:endorse_member/3`, `verify_endorsement/2` | Realm admission |
| `macula_diagnostics:emit_metric/3`, `emit_event/2` | Structured observability |
| `hecate_station:start/0`, `stop/0` | Public lifecycle facade |
| `hecate_station_health:status/0` | /status JSON snapshot |
| `hecate_station_admin:authorise/2` | /admin authn gate |

### 4.3 Behaviours

Reusable OTP behaviours defined under `macula_*`:

- `macula_registry` behaviour — for anything implementing Pillars 1+2+6 (register/unregister/heartbeat/idempotent).
- `macula_record_type` behaviour — one callback module per PKARR record type, for validation + domain-specific checks.

---

## 5. Phase 0 — Repo bootstrap (1 session)

### 5.1 Deliverable

Private repo `hecate-social/hecate-station` exists; CI green on empty skeleton; ROOT + all Part plans committed to `plans/` subdir; README committed; Apache-2.0 LICENSE.

### 5.2 Tasks

1. Create GitHub repo via `gh repo create hecate-social/hecate-station --private --description "Macula V2 reference station"`.
2. Seed from local scaffold: `rebar3 new release` + prune to umbrella layout.
3. Copy all PLAN_MACULA_V2_* files from `~/.claude/plans/` into `hecate-station/plans/`.
4. Copy `THREAT_MODEL_MACULA.md` to `hecate-station/plans/`.
5. Write minimal README pointing at PLAN_MACULA_V2_ROOT.md + explaining repo status.
6. `.gitignore` includes `rebar.lock`, `_build/`, `.eunit/`, `priv/` generated artefacts.
7. `.github/workflows/ci.yml`: rebar3 compile + eunit + dialyzer + xref on push.
8. Docker skeleton — empty Dockerfile that builds an OTP 27 release; no runtime yet.
9. Archive `macula-io/macula-relay` with README replacement pointing to new repo.

### 5.3 Acceptance

- `gh pr view` green.
- `rebar3 compile` succeeds (empty apps still compile).
- README accurately describes "not usable yet, design phase".

### 5.4 Expected duration

1 AI session (≈4 hours work).

---

## 6. Phase 1 — Walking skeleton (5–6 sessions)

### 6.1 Deliverable

Two stations exchange a signed `node_record` over QUIC. Tombstone on stop works. No DHT, no SWIM, no source routing. This is the smallest deployable artefact that proves transport + identity + record lifecycle.

### 6.2 Tasks

1. `macula_identity` — Ed25519 keygen, crypto puzzle grinding.
2. `macula_record` — CBOR canonicalisation, node_record type only, sign + verify.
3. `macula_frame` — BERT envelope + CONNECT/HELLO/GOODBYE frames only.
4. `macula_transport` — thin wrapper over quicer; accept + connect.
5. `macula_peering` — CONNECTING → HANDSHAKING → CONNECTED state machine (simplified; no REFRESH phase yet).
6. `hecate_station` — start/stop API; loads config, derives identity, opens listener.
7. `hecate_station_health` — `/status` returning JSON with peer list, node_record version.
8. `macula_diagnostics` — structured logs; metrics emitted to process dictionary (upgrade later).
9. Phase-1 Integration test: launch two stations, connect, exchange records, verify signatures, stop one, observe tombstone.

### 6.3 Acceptance

- End-to-end test passes: two stations, signed `node_record` exchange, tombstone on stop.
- Dialyzer clean.
- Cold-boot-to-ready < 2 s on lab hardware.

### 6.4 Expected duration

5–6 AI sessions. Human-equivalent ≈ 2 weeks.

---

## 7. Phase 2 — SWIM-Lifeguard (4–6 sessions)

### 7.1 Deliverable

Liveness between stations. Killing a peer is detected within 10 s by surviving stations.

### 7.2 Tasks

1. `macula_swim` — SWIM core (ping, indirect-ping, suspect, confirm). Lifeguard extensions (self-awareness, dogpile, refutation-buddy).
2. SWIM frame types in `macula_frame`.
3. Heartbeat obligation in `macula_peering` (Part 4 §5).
4. Fast-fail on `suspect|confirmed_failed` in ongoing operations (partial Pillar 4).
5. Chaos test harness — `hecate_station_testkit` or separate `apps/macula_chaos/` — SIGKILL a station mid-flight and verify detection.

### 7.3 Acceptance

- 3-station test: kill one, peers detect within 10 s (2s SWIM period × 6 rounds).
- False-positive rate <1% under 20% packet-loss injection.
- Lifeguard self-awareness demonstrated: under simulated local CPU spike, station does not mass-suspect peers.

### 7.4 Expected duration

4–6 AI sessions.

---

## 8. Phase 3 — S/Kademlia DHT (8–12 sessions)

### 8.1 Deliverable

DHT with tier-diverse buckets, crypto-puzzle admission, disjoint-path lookups. Simulated 100-station fleet achieves >99.5% lookup success.

### 8.2 Tasks

1. `macula_dht` buckets (160 buckets, k=20 entries).
2. Tier-diverse bucket admission (Part 3 §4.3).
3. Sibling list (s=16).
4. FIND_NODE, FIND_VALUE, STORE, REPLICATE, PING ops.
5. α=3 concurrency, d=3 disjoint paths (Part 3 §4.5).
6. Record tReplicate (1h) + tRepublish (24h) + tExpire (48h) timers (Part 4 §11).
7. Diversity-constrained replica placement (Part 3 §5.2) with 3/4 quorum writes.
8. All 17 PKARR record types in `macula_record` (Part 6 §9).
9. Crypto-puzzle enforcement on peer admission.
10. Simulation harness: N=100 synthetic stations in a single BEAM VM, verifiable DHT convergence.

### 8.3 Acceptance

- Lookup success rate >99.5% at N=100 simulated stations.
- Bucket diversity ≥5 ASN / ≥3 country satisfied for >95% of buckets with adequate population.
- Replica placement satisfies ≥8 ASN / ≥5 country / ≥3 tier constraints.
- tRepublish and tReplicate cycles observed end-to-end over simulated 48 h (accelerated time).

### 8.4 Expected duration

8–12 AI sessions. This is the longest single phase.

---

## 9. Phase 4 — Source routing (6–8 sessions)

### 9.1 Deliverable

Cross-relay CALLs work via source-computed k=3 disjoint paths. Failed edge triggers structured BOLT#4 error; retry via path[1] succeeds in <50 ms.

### 9.2 Tasks

1. `macula_routing:compute_paths/3` — Suurballe k=3 disjoint over routing-table graph.
2. Source-route header encode/decode in `macula_frame` (Part 6 §11).
3. Per-hop verification + forwarding in `macula_peering`.
4. BOLT#4 error taxonomy (Part 6 §13) — full 25-entry implementation.
5. CALL state machine (Part 4 §6.2) with per-transition deadlines.
6. Path cache (5 min TTL) with SWIM-event invalidation.
7. Retry budget + disjoint-path rotation.
8. Chaos test: kill mid-path hop, verify retry via alternate path.

### 9.3 Acceptance

- Cross-relay RPC p95 latency <200 ms (intra-EU).
- Failed-edge reroute p95 <50 ms.
- V1 blocker from memory `dist-tunnel-blocker.md` resolved: cross-relay CALL works across >2 hops.

### 9.4 Expected duration

6–8 AI sessions.

---

## 10. Phase 5 — Intra-realm overlay (4–6 sessions)

### 10.1 Deliverable

Realm of 20 stations converges on OR-Set add/remove in <1 s via Plumtree.

### 10.2 Tasks

1. `macula_overlay` HyParView partial view (active + passive + shuffles).
2. Plumtree eager-push + lazy-push + tree repair.
3. OR-Set CRDT + delta-state compaction.
4. Realm-scoped topic dispatching (PUBLISH / SUBSCRIBE / EVENT, Part 6 §6).
5. Topic-subscription-hint aggregation (Part 3 §5.4).
6. Realm-join handshake using `realm_member_endorsement` records.

### 10.3 Acceptance

- 20-station realm converges on add/remove in <1 s.
- HyParView active-view repair after single failure <3 s.
- Plumtree delivers every message to every member at least once (log-verified).
- No cross-realm leakage: realm-A gossip never reaches realm-B station.

### 10.4 Expected duration

4–6 AI sessions.

---

## 11. Phase 6 — Bootstrap cascade (4–6 sessions)

### 11.1 Deliverable

Cold-boot station bootstraps via any single tier of the 5-tier cascade in <60 s.

### 11.2 Tasks

1. `macula_bootstrap` orchestrator.
2. Tier A — DoH resolver integration (≥3 resolvers); anycast IPv6 probe.
3. Tier B — mDNS `_macula._udp.local` advertise + listen.
4. Tier C — Mainline DHT bridge integration (BitTorrent DHT client library).
5. Tier D — Bitcoin + Ethereum anchor readers (light-client libs).
6. Tier E — CLI `hecate bootstrap add-peer <signed-url>`.
7. Firmware-embedded foundation pubkeys (5, multi-custodian).
8. Parameter-record fetching + verification (Part 6 §9.14-§9.16).

### 11.3 Acceptance

- Cold boot Tier A: <2 s to first DHT lookup.
- Cold boot Tier B only (A disabled): <3 s.
- Cold boot Tier C only: <10 s.
- Cold boot Tier D only: <30 s.
- Full cascade attempted under adversarial drop: succeeds in <60 s.

### 11.4 Expected duration

4–6 AI sessions.

---

## 12. Phase 7 — Hardening (6–8 sessions)

### 12.1 Deliverable

Chaos suite passes; Sybil + eclipse scenarios resisted; 24-hour burn-in clean.

### 12.2 Tasks

1. Full chaos test suite (Part 8 scope) — node kills, network partitions, clock skew, BGP simulation.
2. Sybil flood tests: 10 000 adversary NodeIds across 3 AS; observe bucket diversity holds.
3. Eclipse simulation: adversary controls 50% of target's routing table; measure lookup degradation.
4. Adaptive crypto-puzzle difficulty (foundation-signed parameter bumps).
5. 24h burn-in on 10-station lab fleet; zero crashes tolerated.
6. Tier 3 SLA behaviour elicitation (limited T4 reachability).
7. Security review of all signed-record paths.
8. Observability polish: Prometheus/OpenTelemetry export from `macula_diagnostics`.

### 12.3 Acceptance

- Chaos pass-rate documented per pillar (Part 4 §3).
- Sybil budget exhaustion curve matches theoretical model.
- Eclipse adversary at 50% routing table ≤ 1% lookup success drop.
- 24h burn-in: zero crashes, no memory leaks, no goroutine/process explosion.

### 12.4 Expected duration

6–8 AI sessions.

---

## 13. Phase 8 — Lab cutover (2–4 sessions)

### 13.1 Deliverable

mesh_chat demo runs end-to-end on V2 stations only. V1 `macula-relay` fleet decommissioned.

### 13.2 Tasks

1. Build `ghcr.io/hecate-social/hecate-station:main` OCI image via CI.
2. Update `macula-io/macula-demo` compose files to use new image.
3. Stage on one box (relays-hetzner-helsinki); validate for 48 h.
4. Roll to remaining two boxes with 1 h gap for observability.
5. Retire V1 images from ghcr.io (keep one version pinned for rollback).
6. Verify mesh_chat demo passes all 5 resilience scenarios on V2.
7. Update MEMORY + relevant session logs.

### 13.3 Acceptance

- All 5 mesh_chat resilience scenarios pass on V2-only fleet.
- 72 h post-cutover with zero production incidents.
- Rollback tested in staging: V2 image can be swapped back to V1 in <5 minutes.

### 13.4 Expected duration

2–4 AI sessions.

---

## 14. Cross-cutting: CI, release, observability

### 14.1 CI pipeline

`.github/workflows/ci.yml`:
- On every push: `rebar3 compile`, `eunit`, `dialyzer`, `xref`, `ct` (common test).
- On tag: build OCI image, push to `ghcr.io/hecate-social/hecate-station:{tag,latest,main}`.
- Matrix: OTP 27, 28.
- Arch matrix: amd64 + arm64 (target RPi 4B/5 + x86 mini-PCs).

### 14.2 Release cadence

- **Alpha tags** (0.1.0-alpha.N) during Phases 1–5.
- **Beta tags** (0.9.0-beta.N) during Phase 6–7.
- **1.0.0** at Phase 8 cutover success.
- **Semver** thereafter.

Signed releases: CI signs with a GitHub-Actions-held OpenSSF Sigstore key; foundation-FROST signs tagged releases separately.

### 14.3 Observability

`macula_diagnostics` emits:
- Prometheus metrics (http endpoint on `/metrics`, behind `/admin` auth).
- OpenTelemetry traces (opt-in; default off).
- Structured logs (lager/logger JSON).

Foundation monitoring (opt-in) consumes a subset via `_macula.health.*` topics (Part 6).

### 14.4 Docs

- `plans/` — this design document set.
- `guides/` — operator guides (to be written during Phase 6–7).
- `docs/` — ex_doc-generated API reference.
- `README.md` — overview + status + install.

---

## 15. Migration detail (V1 → V2)

### 15.1 What survives

From Part 1 §11, inherited unchanged into V2:
- `quicer` NIF (pinned same version as V1 uses).
- Frame encoder/decoder skeleton (V1's BERT-based design; V2 adds new frame types).
- Ed25519 identity via OTP `crypto`.
- Fleet deployment tooling in `macula-demo`.

### 15.2 What is cherry-picked (reviewed + rewritten)

- Peer-client pattern (design; re-implemented against Pillars 1–6).
- Realm-endorsement flow (concept; re-implemented with Part 5 governance).
- Monitoring/telemetry emitter style (approach; re-implemented in `macula_diagnostics`).

### 15.3 What is replaced

- V1 naive Kademlia → S/Kademlia (Phase 3).
- V1 static peer lists → tier-diverse buckets (Phase 3).
- V1 no-heartbeat → Pillar 3 heartbeats everywhere (Phase 1+).
- V1 silent timeouts → BOLT#4 taxonomy + signed errors (Phase 4).
- V1 no source routing → Suurballe k=3 (Phase 4).
- V1 no Sybil defence → crypto puzzle + disjoint paths (Phase 3).
- V1 geography-agnostic routing → 5-tier hierarchy (Phase 2+).

### 15.4 What is deleted

- "300 virtual relay identities across 3 boxes" — V2 stations are 1:1 with devices.
- `boot` naming — retired.
- `_v2` suffixing — retired (versioning lives at repo/hex level).
- V1 "distinguishable realm relay vs regular relay" — V2 any station may endorse any realm.

### 15.5 Data migration

None. V1 DHT records are not migrated to V2. Fleet runs both in parallel (V1 on legacy boxes, V2 on `hecate-station` boxes) during Phase 7 staging; Phase 8 cutover removes V1 altogether. No user data loss because V1 is pre-user.

---

## 16. Resource and timeline estimates

### 16.1 Session budget

| Phase | AI sessions | Human-equivalent weeks |
|-------|-------------|------------------------|
| 0 | 1 | 0.5 |
| 1 | 5–6 | 2 |
| 2 | 4–6 | 2 |
| 3 | 8–12 | 4 |
| 4 | 6–8 | 3 |
| 5 | 4–6 | 2 |
| 6 | 4–6 | 2 |
| 7 | 6–8 | 3 |
| 8 | 2–4 | 1 |
| **Total** | **40–57** | **~20 weeks** |

Async-checkpoint cadence (Q3 decision): human review at each phase gate; no calendar pressure between gates.

### 16.2 Hardware needs during development

- Dev laptop (existing).
- 3 lab stations (beam00/01/02 or equivalent) for Phase 2+ multi-node tests.
- Optional: rented Hetzner / OVH T3-class VM for Phase 4 cross-region testing.
- Mainline DHT access: any public internet; no additional setup.
- Bitcoin / Ethereum anchor reads: any free RPC (e.g. public Alchemy demo).

### 16.3 Cost of infra over full build

- Existing lab hardware: 0 EUR incremental.
- Optional cross-region VM: ~30 EUR/month × 6 months = ~180 EUR.
- Quarterly blockchain anchor writes during Phase 6 dev: ~40 EUR × 2 quarters = ~80 EUR.
- **Total marginal infra cost: <300 EUR for the full V2 build.**

---

## 17. Risk register

| Risk | Impact | Mitigation |
|------|--------|------------|
| S/Kademlia simulation doesn't scale in single-BEAM process | High; Phase 3 stalls | Distribute simulated stations across multi-node BEAM cluster if single-VM stalls |
| Quicer NIF instability under high connection count | High; Phase 2+ | Phase 1 stress test; if unstable, Q O10 triggers Quinn-NIF swap |
| Suurballe implementation bugs | Medium; Phase 4 stalls | Cross-check against literature test cases; property-test with path-validity oracle |
| Lifeguard parameters wrong for residential hardware | Medium; false positives in lab | Phase 2 calibration; Q O16 revisits |
| Mainline DHT library not mature on BEAM | Low; Phase 6 slips | Fallback: Rust NIF wrapping libp2p-kad; rebuild if needed |
| Blockchain anchor library lag | Low; Phase 6 Tier D slips | Hand-crafted light-client path via block-explorer JSON APIs |
| Foundation FROST key ceremony delay | Medium; Phase 6 Tier A/B partial | Use simple Ed25519 with key rotation until FROST ceremony complete; foundation-issued deprecation window |

---

## 18. Open questions specific to Part 7

- **O30 (new)** — Should `macula_*` apps extract to a separate `macula-io/macula-mesh` library now (even with YAGNI penalty), or wait for a second consumer? Current: wait.
- **O31 (new)** — Quicer vs Quinn-NIF final call. Leans stay-with-Quicer; Phase 1 stress test decides.
- **O32 (new)** — CI parallelism — run OTP 27 and 28 matrix or just 27 to cut CI minutes. Leans: just 27 until Phase 7.
- **O33 (new)** — Reproducible build sigstore + foundation co-sign of release artefacts. Leans: add at Phase 7 hardening.

---

## 19. Success criteria for Part 7

Part 7 is complete when a reader can:

1. Locate the **repo + branch** for every piece of V2 code (§2).
2. Recite the **8 phases** with their acceptance test (§5-§13).
3. State the **module dependency arrow** for `macula_dht` and explain why no cycle exists (§3.2).
4. Identify which phase would **demand more human review** than others (§16.1 — Phase 3 at 8–12 sessions).
5. Explain how V1→V2 **migration avoids data loss** (§15.5 — it's pre-user).
6. Walk through the **CI release pipeline** from commit to OCI image (§14.1-§14.2).
7. Name **three risks** from §17 and their mitigations.

If any is ambiguous, Part 7 revises before first implementation session.

---

*Part 7 answers "how do we build it". Parts 8 and 9 remain: verification strategy (Part 8), open-questions index + references + glossary (Part 9).*
