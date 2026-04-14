# PLAN — Macula V2, Part 8: Verification

**Parent:** `PLAN_MACULA_V2_ROOT.md`
**Depends on:** Parts 1–7 (all substantive content).
**Feeds:** Part 7 phase-acceptance gates (Part 8 is *how* gates are checked).
**Status:** Draft — authored 2026-04-14.
**Scope:** The strategy for proving V2 works. Test taxonomy, property specifications for the 7 pillars, conformance testing against Part 6 wire formats, chaos scenarios, adversarial suites, canary + burn-in, metrics, per-phase acceptance checklists. No implementation code (Part 7 places modules), no design (Parts 1–5).

---

## 1. Purpose of this Part

V1 collapsed because invariants were implicit — nobody could point at a test that proved "Pillar 1 holds" because Pillar 1 wasn't named. V2 is different: every pillar is a property, every property is a test, every test is a gate on a phase.

Part 8 answers three questions:

1. **How do we know each pillar (Part 4 §3–§9) actually holds in the code we shipped?** — Property-based tests generated from pillar specs, §4.
2. **How do we know the wire (Part 6) is what we said it is?** — Conformance test vectors + round-trip + cross-peer tests, §5.
3. **How do we know the system degrades gracefully under chaos, Sybil, eclipse, burn-in?** — Chaos suite + adversarial harness + 24h+ stability runs, §§7–10.

Verification is a *deliverable*, not a luxury. Each phase ships with its test matrix; each test matrix has a documented pass/fail status. No phase advances with a gate-test red.

---

## 2. The verification pyramid

```
                          ┌────────────────────┐
                          │  Formal (TLA+)     │   1–2 invariants, optional
                          └────────────────────┘
                         ┌──────────────────────┐
                         │  Canary / Burn-in    │   24h+, 10-station lab
                         └──────────────────────┘
                      ┌────────────────────────────┐
                      │  Adversarial (Sybil/       │   targeted scenarios
                      │  Eclipse / Byzantine)      │
                      └────────────────────────────┘
                   ┌──────────────────────────────────┐
                   │  Chaos (kills, partitions,       │   Phase 2+ continuous
                   │  latency, clock skew)            │
                   └──────────────────────────────────┘
                ┌────────────────────────────────────────┐
                │  Integration (multi-station in single  │   per-phase
                │  BEAM; `ct` suites)                    │
                └────────────────────────────────────────┘
             ┌──────────────────────────────────────────────┐
             │  Conformance (wire vectors + round-trip)     │   Part 6-driven
             └──────────────────────────────────────────────┘
          ┌────────────────────────────────────────────────────┐
          │  Property-based (PropEr; the 7 pillars as specs)   │   per-module
          └────────────────────────────────────────────────────┘
       ┌──────────────────────────────────────────────────────────┐
       │  Unit (EUnit; function-level; tight loop)                │   every commit
       └──────────────────────────────────────────────────────────┘
```

Each layer has a distinct question:

| Layer | Question it answers |
|-------|---------------------|
| Unit | Did this function behave correctly for the cases we thought of? |
| Property | Does this function behave correctly for *all* cases in the domain? |
| Conformance | Do we speak the wire protocol as specified in Part 6? |
| Integration | Do the modules compose correctly in a single-VM harness? |
| Chaos | Do the 7 pillars hold under injected failure? |
| Adversarial | Do our defences resist the threat model (`THREAT_MODEL_MACULA.md`)? |
| Canary/Burn-in | Does the code remain stable for 24h+? |
| Formal | Are the critical invariants mathematically sound? |

Pyramid shape by test count: thousands of unit cases, hundreds of property generators, tens of chaos scenarios, a handful of canary runs, optionally 1–2 TLA+ specs.

---

## 3. Tooling

| Tool | Purpose | When |
|------|---------|------|
| **EUnit** | Unit tests | Every commit; `rebar3 eunit` |
| **PropEr** | Property-based testing | Every commit; `rebar3 proper` |
| **Common Test (`ct`)** | Integration, multi-node tests | Every phase-gate; `rebar3 ct` |
| **Dialyzer** | Type-check / discrepancy analysis | Every commit |
| **xref** | Cross-reference (unused funcs, dep cycles) | Every commit |
| **Cover** | Line/branch coverage | Weekly + phase-gate |
| **eflame / recon** | Profiling, runtime introspection | Phase 7 burn-in |
| **Mnesia-like harness** | In-VM simulated fleet (N=100) | Phase 3+ |
| **Docker Compose** | Multi-container chaos runs | Phase 2+ |
| **toxiproxy** | Network fault injection (latency, loss, partition) | Phase 2+ |
| **tc / netem** | Kernel-level packet loss + delay (Linux) | Phase 4+ |
| **Fuzzing harness** (`cuter` or custom `proper` generators) | Malformed wire input | Phase 6+ |
| **TLA+ (TLC / Apalache)** | Formal spec model-check (optional) | Phase 7 (1–2 specs) |
| **Prometheus + Grafana** | Metrics during chaos + burn-in | Phase 7+ |
| **OpenTelemetry** | Distributed tracing through multi-hop CALLs | Phase 7 |

---

## 4. Property-based specifications for the 7 pillars

Each pillar from Part 4 becomes one or more PropEr properties. The property statements below are the **acceptance specs**; implementation in `apps/<app>/test/prop_*.erl`.

### 4.1 Pillar 1 — Process-resource binding

**Property P1_owner_dies_state_dies:**
> ∀ sequences S of operations `[register(K, Pid, Opts)?, die(Pid)?]*`: after all owner pids in S have terminated, the registry has zero entries for any `K` that was ever registered and whose owner terminated.

PropEr generator emits random action sequences including `{register, key(), pid(), opts()}`, `{die, pid()}`, `{query, key()}`. State-machine model (`proper_statem`) asserts the property invariantly.

**Property P1_no_orphan:**
> ∀ `{ok, MonRef}` returned by `register_*`, `erlang:is_reference(MonRef) == true` AND a subsequent `{'DOWN', MonRef, _, _, _}` message is delivered within 100ms of owner process termination.

**Shrinking targets:** If the property fails, PropEr shrinks to the minimal action sequence that triggers orphaning. Counterexamples published to `test/counterexamples/` with reproduction instructions.

### 4.2 Pillar 2 — Single-provider invariant

**Property P2_singleton_exactly_one:**
> ∀ sequences S of `register_singleton(K, Pid)`: at any point in S's execution, `gproc:lookup_pids({n, l, K})` returns a list of length 0 or 1.

**Property P2_evicted_notified:**
> ∀ `register_singleton(K, Pid1)` followed by `register_singleton(K, Pid2)` where `Pid1 ≠ Pid2`: `Pid1` receives `{evicted_by, Pid2, K}` within 100ms.

**Property P2_idempotent_reregister:**
> `register_singleton(K, P)` followed by `register_singleton(K, P)` leaves exactly one entry; Pid P receives no eviction message.

### 4.3 Pillar 3 — Liveness probes

**Property P3_heartbeat_miss_evicts:**
> ∀ registration with `heartbeat_interval_ms = H` and `miss_tolerance = N`: if the owner process emits no heartbeats for `H × (N+1)` milliseconds, the registry evicts the entry within one scanner tick.

**Property P3_alive_process_fresh_registration:**
> After eviction by missed heartbeat, if the original process is still alive and calls `register_*` again, re-registration succeeds without conflict.

**Property P3_swim_false_positive_bound:**
> ∀ packet-loss rate `L ≤ 30%`: the SWIM-Lifeguard false-positive rate over 1000 rounds is `< 1%`. (Chaos test with injected `tc`/toxiproxy loss; PropEr generates loss patterns.)

### 4.4 Pillar 4 — Fast-fail over silent timeout

**Property P4_known_unreachable_fast_fail:**
> ∀ CALL to next-hop `H` whose SWIM state is `confirmed_failed`: the caller receives a structured ERROR frame with `code = unknown_next_peer` in `< 10ms`. No QUIC-connect attempt occurs.

**Property P4_deadline_monotonic:**
> ∀ CALL with `deadline_ms = T`: no transition in the CALL state machine occurs after wall-clock `T`. If `T` passes while any hop is processing, the next attempted forward returns `expiry_too_soon`.

**Property P4_error_signatures_verifiable:**
> ∀ ERROR frames returned: `macula_record:verify(ErrorFrame)` succeeds using `reported_by`'s pubkey.

### 4.5 Pillar 5 — Cascade refresh

**Property P5_refresh_completeness:**
> ∀ reconnect event: after entering `CONNECTED` state, for every category C in `{procedures, subscriptions, dht_replicas}`, the peer's observed version-set is a superset of the category's baseline-at-disconnect-time.

**Property P5_bounded_refresh_window:**
> ∀ reconnect: REFRESH completes (all tasks ack'd) within 30 seconds. Failing REFRESH transitions state back to RECONNECTING.

### 4.6 Pillar 6 — Idempotent operations

**Property P6_call_id_dedupe:**
> ∀ CALL with `call_id = C`: a retry (same `C`) within the 10-minute bloom window produces identical response bytes; the target handler is invoked at most once.

**Property P6_register_unregister_commute:**
> ∀ orderings of `{register(K,V), unregister(K)}`: the final registry state is deterministic (no entry if unregister-last, entry if register-last; inconsistent state impossible).

**Property P6_uuid7_monotonic:**
> ∀ UUIDv7 generated on the same station in the same second: the time-ordered prefix is monotonically non-decreasing.

### 4.7 Pillar 7 — Graceful degradation tiers

**Property P7_tier_declared_matches_behaviour:**
> ∀ station in declared tier `T`: observed heartbeat period, SWIM period, retry budget match the Part 4 §9.3 table for T within ±10%.

**Property P7_tier_transition_hysteresis:**
> ∀ station transitioning S → 1 → 2: each transition is preceded by sustained observable condition ≥ 60 s (no flapping).

**Property P7_tier_recovery_bounded:**
> After the conditions for Tier N recover, transition back to Tier N-k occurs within 60 s × k.

### 4.8 Cross-pillar composition properties

**Property C_pillar_1_and_3:**
> Owner death AND missed heartbeat composed: the state is reaped regardless of which pathway triggers first.

**Property C_pillar_4_and_6:**
> A fast-fail ERROR returned to a retry with the same call-id produces the same ERROR bytes (idempotent failure).

**Property C_pillar_2_and_5:**
> After reconnect + REFRESH, singleton keys held by the peer are still singleton on our side; no double-provider state emerges.

---

## 5. Conformance tests (wire protocol)

Part 6 specifies the wire. Conformance has three sub-layers.

### 5.1 Encoding round-trip

For every frame type (§3–§8 of Part 6) and every record type (§9):

```
For 1000 random instances generated by PropEr:
  bytes = encode(instance)
  instance' = decode(bytes)
  assert instance == instance'
  assert encode(instance') == bytes     % canonical reproducibility
```

Canonical reproducibility especially matters for CBOR — two implementations encoding the same record MUST produce identical bytes (prerequisite for signature verification).

### 5.2 Signed-vector test bank

For each record type, a curated set of **test vectors** committed to `test/vectors/`:

- Canonical CBOR encoding (hex).
- Signing keypair (pubkey hex, privkey hex).
- Expected Ed25519 signature (hex).
- Expected record envelope bytes.

Loaded in CI. Any change to signing/canonicalisation that shifts vectors is a breaking change flagged in diff.

Vector count target at Phase 7 gate: ≥5 per record type × 17 types = 85+ vectors.

### 5.3 Cross-version round-trip

Once V2.1 exists, same vector bank must round-trip on V2.0. Backward-compatibility proofs.

For Phase 8 cutover, cross-peer conformance: two stations of identical V2.0 build exchange every frame type, assert byte-identical encoding observed.

### 5.4 Malformed-input fuzzing

PropEr + custom generators emit deliberately malformed frames/records:

- Truncated.
- Extra bytes.
- Invalid UTF-8 in text fields.
- Negative lengths.
- Unknown type tags (must reject with `unsupported_type`).
- Signature with one bit flipped (must reject with `signature_invalid`).
- Replay from 1 hour ago with valid sig (must reject if `valid_until` expired).
- Maps with duplicated keys in non-deterministic CBOR (must reject).

Phase 7 success criterion: 100 000 fuzz iterations, zero crashes, zero accepted malformed frames.

---

## 6. Integration tests

Multi-station, single-BEAM-VM. Each station is a separate OTP release running as a nested supervisor under a `ct` suite.

### 6.1 In-VM fleet harness

`macula_fleet_testkit` (under `apps/` — test-only):

- Spawns `N` station processes, each with distinct identity + port.
- Simulates network via pg message-passing or local UDP on loopback.
- Supports injected latency, loss, partition.
- Collects per-station metrics + assertions.

### 6.2 Phase-1 integration

`phase1_SUITE.erl`:
- `two_stations_exchange_records/1`: launch 2, verify sig, check /status output.
- `tombstone_on_stop/1`: stop one, assert peer sees tombstone within 2 × heartbeat.
- `reconnect_refresh/1`: disconnect mid-session, verify REFRESH runs.

### 6.3 Phase-2 integration

`phase2_SUITE.erl`:
- `swim_detects_dead/1`: kill 1 of 3, assert `confirmed_failed` within 12 s.
- `swim_resists_loss/1`: 20% loss via toxiproxy, false-positive rate <1% over 500 rounds.
- `lifeguard_self_awareness/1`: inject simulated CPU spike, verify no mass-suspect.

### 6.4 Phase-3 integration

`phase3_SUITE.erl`:
- `dht_lookup_success_100/1`: 100-station simulated fleet, 10 000 lookups, success >99.5%.
- `bucket_diversity/1`: verify bucket constraints hold across all stations.
- `replica_placement_diversity/1`: 200 records placed, each has ≥8 ASN, ≥5 country, ≥3 tier.
- `trepublish_cycle/1`: accelerated-time test (mock clock), verify tReplicate + tRepublish fire.
- `custody_handover/1`: new station joins closer to key, takes custody, old drops.

### 6.5 Phase-4 integration

`phase4_SUITE.erl`:
- `cross_relay_call_6hops/1`: PT-BE-NL path; p95 <200ms.
- `retry_on_mid_path_fail/1`: kill hop mid-call, assert retry via path[1] <50ms.
- `bolt4_error_signatures/1`: simulate failures at each hop, verify each ERROR is signed + verifiable.
- `path_diversity/1`: k=3 paths computed; verify ASN/country/tier disjointness.

### 6.6 Phase-5 integration

`phase5_SUITE.erl`:
- `realm_20_converge/1`: 20-station realm, add 100 members via OR-Set, converge <1s.
- `hyparview_repair/1`: kill 1 active-view peer, repair <3s.
- `plumtree_delivery/1`: log-verify every message reaches every member.
- `realm_isolation/1`: publish in realm-A, assert no leakage to realm-B station.

### 6.7 Phase-6 integration

`phase6_SUITE.erl`:
- `cold_boot_tier_a/1`: <2s.
- `cold_boot_tier_b_only/1`: disable A, <3s.
- `cold_boot_tier_c_only/1`: disable A+B, <10s.
- `cold_boot_tier_d_only/1`: disable A+B+C, <30s.
- `cold_boot_tier_e/1`: CLI import peer, successful.
- `doh_hijack_corroboration/1`: one DoH returns malicious, two honest: station uses honest.

---

## 7. Chaos test suite

Chaos is where the 7 pillars face reality. Each scenario maps to one or more pillars; failure mode is observable + attributable.

### 7.1 Failure taxonomy

| Scenario | Tools | Pillars tested |
|----------|-------|----------------|
| **Process kill** (SIGKILL) | `erlang:halt/0` on victim node | 1, 3 |
| **Graceful stop** | `hecate_station:stop/0` | 1, 5, 7 |
| **Network partition** (symmetric) | toxiproxy `bandwidth=0` | 3, 5, 7 |
| **Asymmetric partition** (A→B ok, B→A drop) | toxiproxy directional | 3, 5 |
| **Packet loss 10/30/50%** | toxiproxy `packet_loss` | 3 |
| **Latency spike +500ms** | toxiproxy `latency` | 3, 4, 7 |
| **Clock skew ±30s, ±5m** | `meck` on `erlang:system_time/1` | 4 (deadlines), 8 |
| **Slow loris** (stalled QUIC stream) | Custom test client | 3, 4 |
| **Memory pressure** | `erlang:garbage_collect/0` storm | 3, 7 |
| **Disk full** | mount tmpfs, fill | 7 |
| **TLS cert expiry** (mock) | advance clock | 7 |
| **NIF crash** | inject `erlang:nif_error/1` | 1 (supervisor repair) |

### 7.2 Phase-2 chaos

Run on every CI build after Phase 2 completes:

- Random 1-of-3 kill every 30 s for 1 h; assert SWIM always converges within 12 s.
- 10% packet loss for 30 min; false-positive rate <1%.

### 7.3 Phase-4 chaos

- Random mid-path hop kill during CALL; assert retry <50ms.
- Asymmetric partition: 3-hop path where middle hop's forward path dies but reverse works; assert origin receives signed ERROR.
- Clock-skew +30s on one hop; assert CALL still succeeds (tolerance).
- Clock-skew +5min on one hop; assert `expiry_too_soon` fast-fail.

### 7.4 Phase-7 chaos (full suite)

All scenarios above + compositional:

- Partition + kill + latency simultaneously.
- Kill adjacent-tier stations to force tier fallback path.
- Disk full on 1 of 3 stations; assert graceful degradation not cascading failure.

Acceptance: pass-rate per pillar documented; target ≥95% on all scenarios; any regression blocks Phase 8.

---

## 8. Adversarial suite

Adversaries are the threat model. Each scenario in `THREAT_MODEL_MACULA.md` has a corresponding test.

### 8.1 Sybil flood

**Scenario:** Adversary mints 10 000 NodeIds at current crypto-puzzle difficulty, attempts to flood a target's routing table.

**Harness:**
- Spawn 10 000 synthetic peer processes with grinded NodeIds (pre-computed in test fixtures).
- Concentrate them in one ASN.
- Attempt to connect to target over simulated network.
- Observe target's bucket composition + diversity scores.

**Acceptance:** Bucket ASN diversity ≥5 holds on target after flood; adversary NodeIds from one ASN hold ≤4 bucket slots per bucket.

### 8.2 Eclipse attempt

**Scenario:** Adversary controls 50% of target's routing table.

**Harness:**
- Populate target's buckets such that exactly 50% of peers are adversarial (cooperative in protocol but return biased FIND_NODE responses favouring other adversaries).
- Origin performs lookups with d=3 disjoint paths.

**Acceptance:** Lookup success rate drops by <1% compared to all-honest baseline.

### 8.3 Byzantine relay

**Scenario:** Adversary station on a source-route path returns garbage instead of forwarding.

**Harness:** Replace forwarding logic with `return_invalid_payload/1`.

**Acceptance:** Preceding hop detects invalid signature, returns signed ERROR with `signature_invalid` or equivalent; origin retries on disjoint path.

### 8.4 Replay

**Scenario:** Adversary captures a legitimate CALL frame and replays 10 minutes later.

**Harness:** Record frame; time-warp 10 min; re-send.

**Acceptance:** Target station rejects with `expired` or deadline-based error; dedupe bloom (if within window) returns cached response without handler invocation.

### 8.5 DoS on bootstrap

**Scenario:** Adversary DDoSes all DoH resolvers for Tier A.

**Harness:** toxiproxy blackholes all listed DoH resolver IPs.

**Acceptance:** Station falls through to Tier B/C/D; bootstrap completes under SLA.

### 8.6 Forged foundation seed

**Scenario:** Adversary serves forged `foundation_seed_list` from a hijacked DoH.

**Harness:** Mock DoH server returns record signed by adversary key.

**Acceptance:** Station rejects (signature fails against foundation pubkey embedded in firmware); falls through.

### 8.7 Firmware tamper (design-level test)

Not a runtime test; verified by reproducible build + signature pipeline in CI (Part 7 §14.2). Inclusion is manual audit at release.

---

## 9. Canary + burn-in

### 9.1 24h lab burn-in

**Setup:** 10 stations (5 VMs + 5 containers on lab hardware) connected via mesh; run 10 realms with synthetic PubSub + CALL load.

**Load profile:**
- 100 CALL/s across the fleet.
- 1000 PubSub message/s.
- DHT lookup every second.
- Random SWIM event generation.

**Metrics monitored:**
- Memory: heap + atom + binary + ETS. Growth rate < 2% over 24h per station.
- Process count. Stable within ±10%.
- ETS table count. Stable.
- Open QUIC connections. Stable after warm-up.
- CALL p95 latency drift. <10% over 24h.
- Zero supervisor-restart cycles of long-lived workers.
- Zero unhandled crashes.

**Acceptance:** All metrics within bounds; Grafana dashboard exported as artefact.

### 9.2 72h extended burn-in (Phase 7)

Repeat 24h profile × 3. Additional:

- tReplicate cycle fully exercised.
- tRepublish cycle fully exercised.
- Foundation-parameter refresh exercised.

### 9.3 Adversarial burn-in

24h with *one adversary station* in the fleet running Byzantine behaviours (occasional drops, mild Sybil flood, some garbage replies).

Acceptance: Fleet-level metrics unaffected; adversary observable via diagnostics topic.

---

## 10. Performance benchmarks

### 10.1 Targets

Derived from Part 4 §9 tier SLAs and Part 3 lookup success rates:

| Metric | V2.0 MVP | Year 1 | Rationale |
|--------|----------|--------|-----------|
| Cold-boot Tier A | <2 s | <1 s | Bootstrap cascade |
| DHT lookup p50 intra-EU | <80 ms | <50 ms | Part 4 §9 Tier S |
| DHT lookup p95 intra-EU | <200 ms | <150 ms | Same |
| Cross-relay CALL p95 | <200 ms | <150 ms | Part 7 Phase 4 accept |
| SWIM convergence (1 failure in 10-station group) | <12 s | <10 s | Part 4 §5.2 |
| Realm Plumtree convergence (20-station realm) | <1 s | <500 ms | Part 3 §7.2 |
| Memory per station at 100 records held | <50 MB | <30 MB | RPi 4B floor |
| Sustained CALL throughput | 500 CALL/s | 2000 CALL/s | Single-RPi baseline |

### 10.2 Benchmark harness

`macula_bench` test-only app:
- Runs workload profiles against a target fleet.
- Exports HDR-histogram latency distributions.
- Compares against baseline committed to `test/baselines/`.
- Regression >10% on any p95 fails CI.

### 10.3 Profiling

`eflame` flame graphs generated per Phase-gate; archived in `docs/profiles/`.

Target hotspots:
- `crypto:verify/4` (Ed25519 sig check) — batched where possible.
- `macula_frame:decode/1` — BERT hot path; avoid copies.
- `macula_dht:lookup/2` — parallelism limits.

---

## 11. Formal methods (optional)

Two TLA+ specs under consideration for Phase 7:

### 11.1 Pillar 1+2 registry spec

Model the registry as `{Key → Pid}` with actions `register`, `register_singleton`, `die`, `heartbeat_miss`. Invariants:
- No singleton key maps to >1 pid.
- Dead pid implies key absent (Pillar 1).
- Eviction on singleton re-registration is atomic (no in-between state where both or neither are registered).

TLC model-checker with bounded key-set size (say, 5 keys, 5 pids, 20 actions).

### 11.2 SWIM-Lifeguard convergence spec

Model partial views + incarnations + suspicion. Invariant: eventually all honest members agree on the membership modulo `miss_tolerance` rounds.

Leveraging existing SWIM TLA+ specs from academic literature as starting point.

### 11.3 Effort trade-off

TLA+ is high-effort, high-assurance. V2.0 makes this **optional** — property-based testing covers the same ground probabilistically. Formal spec becomes valuable if V3 expansion or third-party audit demands it.

---

## 12. Per-phase verification matrix

Cross-reference of Part 7 phase gates to Part 8 artefacts:

| Phase | Unit | Property | Conformance | Integration | Chaos | Adversarial | Canary |
|-------|------|----------|-------------|-------------|-------|-------------|--------|
| 0 | — | — | — | — | — | — | — |
| 1 | `macula_identity`, `macula_record`, `macula_frame` | P1, P2, P6 (register primitives) | Round-trip node_record; signed vectors | `phase1_SUITE` | — | — | — |
| 2 | `macula_swim` | P3, P5 | SWIM frames | `phase2_SUITE` | Kill + 20% loss | — | — |
| 3 | `macula_dht` | P1-3, P6 (DHT ops); diversity invariants | DHT frames; 17 record types | `phase3_SUITE` with N=100 sim | Custody churn | — | — |
| 4 | `macula_routing` | P4; source-header integrity | Source-route bytes | `phase4_SUITE` | Mid-path kill; clock skew | — | — |
| 5 | `macula_overlay` | CRDT convergence | PUBLISH/SUBSCRIBE/EVENT | `phase5_SUITE` | Realm partition heal | — | — |
| 6 | `macula_bootstrap` (each tier) | Cascade fallback | Foundation records | `phase6_SUITE` | Cascade-level DoS | DoH hijack, forged seed | — |
| 7 | Gaps found during 1–6 | All pillars composition | Fuzz 100k | Full multi-phase | Full suite | Sybil 10k + eclipse 50% + Byzantine + replay | 24h + 72h + adversarial |
| 8 | Regression on Phase 7 | — | — | mesh_chat resilience 5 scenarios | — | — | Production 72h observation |

Every row's listed artefact must be green for the phase to close.

---

## 13. Metrics + reporting

Every verification artefact produces structured output consumed by a **gate dashboard**:

- Unit/property: `rebar3` JSON output → aggregated pass/fail + coverage.
- Conformance: test-vector diff (binary-identical or not).
- Integration: `ct` HTML + machine-readable.
- Chaos: per-scenario pass/fail + latency histograms.
- Adversarial: per-scenario pass/fail + extra attack-cost measurement.
- Canary: Grafana dashboards + alert trail.

Gate dashboard is a single HTML page committed to `plans/verification/phase_N.html` per phase. Manual read-off at phase-gate review.

---

## 14. Continuous vs phase-gate verification

**Continuous (every commit):** unit, property, dialyzer, xref.
**Pull-request:** above + conformance round-trip + coverage check.
**Nightly:** integration suites of current + prior phases; Phase-2 chaos.
**Weekly:** full regression of all phase suites; benchmark vs baseline.
**Phase-gate:** everything; full chaos; manual review of metric dashboards.

Phase-gate is a **human decision point** — automated checks enumerate; owner signs off.

---

## 15. Test code standards

- Test modules live in `apps/<app>/test/`.
- Property modules prefixed `prop_*`.
- Common Test suites suffixed `_SUITE.erl`.
- Test-only apps live under `apps/` with `{applications, [..., test_only]}` flag in `.app.src`.
- No flakiness tolerated: a test that fails intermittently is fixed or quarantined with explicit `@known_flaky` tag + issue tracker link.
- Fixtures under `test/fixtures/`; vectors under `test/vectors/`; counterexamples under `test/counterexamples/`.

---

## 16. Open questions specific to Part 8

- **O34 (new)** — Simulation harness authoritative library: build custom or adopt? `cuter` integration? PropEr's `proper_statem` as primary? Current leaning: PropEr native + custom harness.
- **O35 (new)** — TLA+ spec effort/value in V2.0. Leaning: defer to V2.1 unless an audit requirement surfaces.
- **O36 (new)** — Foundation-signed conformance test vector registry: who maintains, who signs? Leaning: foundation does, publishes monthly.
- **O37 (new)** — Public chaos-report transparency: publish pass-rate dashboards openly? Leaning: yes, post-Phase-8 for ecosystem credibility.
- **O38 (new)** — Cross-implementation conformance partner: any volunteering second implementation (Rust)? Unknown; track in Part 9.
- **O39 (new)** — Burn-in duration pre-cutover. 72h minimum; 7 days if schedule permits.

---

## 17. Success criteria for Part 8

Part 8 is complete when a reader can:

1. Map each of the **7 pillars** to at least one concrete PropEr property (§4).
2. Describe the **3 sub-layers of conformance** testing and why each is needed (§5).
3. List **5 chaos scenarios** and identify which pillar each stresses (§7.1).
4. Explain why Sybil flood + eclipse + Byzantine form a **spectrum of adversarial tests** not a single one (§8).
5. Predict what a **24h burn-in failure** looks like in metrics and what it implies (§9.1).
6. State the **phase-gate criterion** for Phase 3 and Phase 7 and what blocks promotion (§12).
7. Describe the **continuous vs phase-gate** verification cadence (§14).
8. Name **one situation** where TLA+ adds value over property-based testing and why V2.0 defers it (§11).

If any is ambiguous, Part 8 revises before first verification commits land.

---

*Part 8 closes the verification loop. Only Part 9 remains: open-questions master index, glossary, references, history appendix.*
