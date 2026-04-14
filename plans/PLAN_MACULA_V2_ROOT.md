# PLAN — Macula V2 (ROOT)

**Status:** ✅ **Design complete** — ROOT + Parts 1-9 authored 2026-04-14. Ready for Phase 0 execution (repo bootstrap).
**Scope:** Architectural spec for V2 of the Macula mesh protocol and the Hecate Station product built on it.
**Authoring home (for now):** `~/.claude/plans/`. Moves to `hecate-social/hecate-station/plans/` once the repo is created.
**Related:** `THREAT_MODEL_MACULA.md` (referenced from Part 1).

---

## 0. TL;DR

V1 of Macula was an evolutionary WAMP-era prototype. V2 is a rebirth from first principles for a **street-level, IPv6-native, Europe-bounded sovereign mesh**.

Three drivers force V2:

1. **Operational fragility** — V1 has accumulated seven structural bugs in 72 hours (see history §12). The absence of a process-resource-lifecycle model is producing a new "surprise failure" every stress test.
2. **Architectural mismatch** — V1 was designed for DC-hosted relays. The production target is millions of RPi-class street-level devices. Key assumptions (full-mesh peering, small fleet, no Sybil concern, no TTL management) do not scale.
3. **Identity + trust gaps** — V1 has no cryptographic identity model. V2 makes Ed25519-signed PKARR-compatible records the universal primitive for every piece of routable state.

BEAM Campus is the company. Hecate is the product line (sovereign application platform). Macula is the protocol Hecate runs on. Stations are the deployable units (street-level ARM/x86 SBCs at €4-30/month). This plan specifies Macula V2 + the first Hecate Station implementation.

---

## 1. Why this plan exists

- Three ad-hoc plans (Routing V2, Resilience, Relay Refactor) kept overlapping. One plan, nine Parts, single source of truth.
- Foundation-first. The next 3-6 months of work all descend from this document.
- Pre-user. No migration pressure. Clean slate is cheap now; impossible later.
- Zero-demo-deadline. The proposed short video is a teaser; no hard calendar. Architecture > theatrics.

---

## 2. Organizational and product hierarchy

```
BEAM Campus                           — the company (consultancy + products)
  ├─ Hecate                           — flagship product: Sovereign Application Platform
  │   ├─ Hecate Station (NEW in V2)   — the deployable unit; what customers run
  │   ├─ Hecate Daemon                — user-facing runtime layer
  │   ├─ Hecate Web (Tauri + Svelte)  — UI
  │   ├─ Hecate Apps                  — plugin applications (marketplace, weather, chat, …)
  │   └─ Hecate Realm Server          — realm governance + billing
  └─ Macula                           — the mesh protocol (open)
      ├─ macula (SDK, hex pkg)        — what BEAM apps use to speak Macula
      └─ Stations                     — any conforming server; Hecate ships the reference one
```

Public-facing pitch:

> *"BEAM Campus builds Hecate, the Sovereign Application Platform. Hecate runs on the Macula mesh protocol. A Hecate Station is 4-30 EUR/month of hardware at street level. The mesh is indestructible because the stations are many and cheap."*

---

## 3. Scope

### In-scope

- Macula V2 wire protocol + identity + record format + routing.
- Hecate Station V1 — reference implementation of a V2 station.
- Migration story from V1 artifacts (what survives, what dies).
- Europe-bounded initial coverage (EU + EEA + UK + CH; see Part 2).
- IPv6-native data plane with IPv4 fallback via relay-forward.
- Resilience and failure-handling as first-class design (Part 4).
- Threat model: Sybil, eclipse, residential churn, state-actor BGP, fiber cuts (see `THREAT_MODEL_MACULA.md`).

### Explicitly out of scope (V2.0; revisited post-launch)

- Global (non-European) coverage.
- Onion-routed query privacy (additive Phase 9+).
- Incentive / token layer (Helium-style).
- Satellite / DTN integration.
- Mobile relays (changing IPv6 addresses live).
- Relay-level content storage (Hecate apps do that at the app layer).
- Cross-protocol interop (libp2p, matrix federation, etc.).

### Explicitly NOT touched (existing plans continue separately)

- Realm governance (`PLAN_MNS_AND_REALM_JOIN`)
- Realm server architecture at macula.io
- Hecate daemon + apps (continue evolving independently)
- Git-over-mesh, Weather, etc. — these CONSUME Macula V2 once available

---

## 4. Design principles — the 7 pillars

Every V2 decision passes these gates. Full treatment in Part 4 (Lifecycle).

| # | Pillar | One-line statement |
|---|--------|--------------------|
| 1 | Process-resource binding | One owner, one lifetime. External resource dies ⇒ owner dies. gproc/pg/ETS entries auto-clean via OTP monitors. |
| 2 | Single-provider invariant | Session-bound keys (e.g. `_dist.tunnel.X`) permit exactly one provider; registration evicts prior. |
| 3 | Liveness probes, not existence checks | Gproc membership alone does not prove the owner is functional. Every path has end-to-end heartbeat. |
| 4 | Fast-fail over silent timeout | Known-unreachable targets return structured errors immediately. BOLT#4-derived failure taxonomy. |
| 5 | Cascade refresh on reconnect | Any reconnect triggers refresh of dependent state (subs, records, presence) automatically. |
| 6 | Idempotent operations, stable call-IDs | Repeat registers are no-ops. Retries with same call_id dedupe. Register/unregister commute. |
| 7 | Graceful degradation tiers | Explicit SLA S / 1 / 2 / 3 / 4 with defined behaviour at each tier. |

These map directly into V2 architecture — §4.10 presence binding (Pillar 1), §4.9 SWIM-Lifeguard (Pillar 3), source-routing header (Pillar 4), etc.

---

## 5. Repository topology

| Repo | Org | Status | Content |
|------|-----|--------|---------|
| `macula-io/macula` | macula-io | ACTIVE (V1 frozen at 1.4.23; V2 develops on new main/v2) | Mesh protocol SDK: identity, records, protocol, QUIC, client SDK. Published as hex pkg `macula`. V2 = `macula` 2.x. |
| `hecate-social/hecate-station` | hecate-social | **NEW (PRIVATE)** | Reference station implementation. Consumes `macula` SDK + adds station-only apps (peering, DHT, SWIM, handler, /status, admin). Product: the Hecate Station. |
| `macula-io/macula-relay` | macula-io | **ARCHIVED** | Superseded by `hecate-station`. Git-archive flag set; README pointers. V1 codebase preserved for reference + legacy fleet Docker images continue to run unchanged. |
| `macula-io/macula-realm` | macula-io | ACTIVE | Realm server. Will consume Macula 2.x SDK when V2 matures. |
| `macula-io/macula-architecture` | macula-io | ACTIVE | Existing architecture index. V2 plans may be mirrored or linked; the station repo is the canonical home. |
| `hecate-social/hecate-daemon` | hecate-social | ACTIVE | User-facing runtime. Will consume Macula 2.x when ready. |
| `macula-io/macula-demo` | macula-io | ACTIVE (legacy fleet) | Continues hosting the lab fleet on V1 Docker images until V2 cutover. |

Module naming (decision 2026-04-14):

- **`macula_*`** = protocol primitives, live in **`macula-io/macula` v2** (the SDK).
  Anything shared meaningfully between station, daemon, and stub.
  - `macula` (facade), `macula_identity`, `macula_record`, `macula_frame`,
    `macula_transport`, `macula_peering`, `macula_diagnostics`.
- **`hecate_*`** = station-role implementations, live in **`hecate-social/hecate-station`**.
  Code only stations run.
  - `hecate_station` (root + config + admin + health),
    `hecate_dht`, `hecate_swim`, `hecate_routing`, `hecate_handler`,
    `hecate_bootstrap`, `hecate_overlay`, `hecate_realm`.

The earlier "all `macula_*` lives in hecate-station as YAGNI" framing was
revised: SDK-vs-station split is the cleaner separation. Algorithms (SWIM,
S/Kademlia, Plumtree) are Macula's; *this Erlang implementation* of them
is Hecate's. Other implementations (Rust station, Go station) would carry
their own equivalents.

Wire topics:
- `_macula.*` — protocol-level system topics (replaces V1 `_relay.*`)
- `_hecate.*` — Hecate application-level topics (replaces V1 `_chat.*`, `_marketplace.*`, etc.)
- No `_v2.*` suffix anywhere. V2 is the reborn protocol, not a shim.

---

## 6. Part catalog

The plan body is split across nine Parts. Authored in dependency order.

| Part | Title | Status | Approx. size |
|------|-------|--------|--------------|
| [1](PLAN_MACULA_V2_PART1_FOUNDATIONS.md) | Foundations — design principles, identity, threat model link, scale targets, hardware floor | ✅ Drafted 2026-04-14 | ~500 lines |
| [2](PLAN_MACULA_V2_PART2_TOPOLOGY.md) | Topology — 5-tier geographic hierarchy, NodeId semantics, IPv6-first / IPv4 fallback | ✅ Drafted 2026-04-14 | ~400 lines |
| [3](PLAN_MACULA_V2_PART3_DISCOVERY.md) | Discovery & Routing — S/Kademlia + tier-diverse buckets, diversity-constrained replicas, source routing, intra-realm mesh | ✅ Drafted 2026-04-14 | ~900 lines |
| [4](PLAN_MACULA_V2_PART4_LIFECYCLE.md) | Lifecycle & Resilience — the 7 pillars fully elaborated, SWIM-Lifeguard, failure taxonomy | ✅ Drafted 2026-04-14 | ~700 lines |
| [5](PLAN_MACULA_V2_PART5_BOOTSTRAP.md) | Bootstrap & Governance — multi-tier cascade, gateway roles, realm admission, Sybil/eclipse hardening | ✅ Drafted 2026-04-14 | ~500 lines |
| [6](PLAN_MACULA_V2_PART6_PROTOCOL.md) | Wire Protocol Catalog — PKARR record formats, Kademlia key derivation, CALL / PUBLISH / CONNECT frames + extensions | ✅ Drafted 2026-04-14 | ~600 lines |
| [7](PLAN_MACULA_V2_PART7_IMPLEMENTATION.md) | Implementation Plan — phase-by-phase schedule, migration strategy, module boundaries, rebar layout | ✅ Drafted 2026-04-14 | ~500 lines |
| [8](PLAN_MACULA_V2_PART8_VERIFICATION.md) | Verification — unit / property / chaos / canary strategy, success criteria per phase | ✅ Drafted 2026-04-14 | ~400 lines |
| [9](PLAN_MACULA_V2_PART9_OPEN.md) | Open Questions + Appendices — unresolved decisions, glossary, references, history | ✅ Drafted 2026-04-14 | ~400 lines |

Authoring order: 1 → 2 → 4 → 3 → 5 → 6 → 7 → 8 → 9. Part 4 (Lifecycle) before Part 3 (Discovery) because lifecycle invariants drive discovery design, not vice versa.

---

## 7. Phase timeline

High-level only. Per-phase detail in Part 7.

| Phase | Sessions | Deliverable | Acceptance |
|-------|----------|-------------|------------|
| **0** | 1 | Repo created + bootstrapped | Private repo at `hecate-social/hecate-station` exists; CI green; rebar skeleton compiles; README + PLAN files committed |
| **1** | 5-6 | **Walking skeleton** — two stations exchange signed `node_record`s via QUIC | Integration test: 2 stations, both hold verified records, tombstone-on-stop works |
| **2** | 4-6 | SWIM-Lifeguard liveness between stations | Chaos test: kill one station, peers detect within 10s |
| **3** | 8-12 | S/Kademlia DHT with tier-diverse buckets + diversity replica placement | Lookup success >99.5% at N=100 simulated stations |
| **4** | 6-8 | Source routing (44B header, Suurballe k=3) + BOLT#4 failure taxonomy | Cross-relay RPC <200 ms p95; failed-edge reroute <50 ms |
| **5** | 4-6 | Intra-realm Plumtree + HyParView mesh | Realm of 20 stations converges in <1s for OR-Set add/remove |
| **6** | 4-6 | Bootstrap cascade (mDNS + DoH PKARR + foundation seed + Mainline DHT + blockchain anchor + social) | Cold-boot station bootstraps via any single tier in <60s |
| **7** | 6-8 | Hardening — chaos suite, Sybil flood, eclipse scenarios, 24h burn-in | All chaos scenarios pass; pass rate documented per pillar |
| **8** | 2-4 | Lab cutover — Hecate stations replace `macula-relay` on the three boxes | mesh_chat demo runs end-to-end on V2 stations only |

AI-paced session counts. Human-equivalent is 4-6× per phase. Total: 40-60 sessions to full V2. Elapsed calendar depends on review cadence; with async checkpoint rhythm (Q3 resolution) likely 3-6 months.

---

## 8. Naming discipline

- **Stations** are the deployable product units (singular, concrete, street-level). Not "relays", not "nodes", not "hubs".
- **Nodes** are BEAM VMs where Hecate applications run. Each node connects outbound to one station.
- **Devices** are physical hardware. One device can host one station + multiple nodes, or just a node, etc.
- **Peers** = any two entities at the same tier (station-peer, node-peer).
- **Realm** = sovereignty boundary. A realm has one admin key; realms may federate.
- **Gateway** = a station with elevated capacity declaring itself as a tier bridge (opt-in).

Wire topic discipline:
- `_macula.*` for Macula protocol topics (peering, liveness, DHT ops)
- `_hecate.*` for Hecate application topics (chat, marketplace, weather)
- Realm-specific app topics inherit the realm's namespace (`{realm}/...`)

Module naming:
- `macula_*` — protocol primitives in the SDK (`macula-io/macula`)
- `hecate_*` — Hecate product code: daemon, web, plugins, AND the station's
  server-role apps (`hecate_dht`, `hecate_swim`, `hecate_routing`, `hecate_handler`,
  `hecate_bootstrap`, `hecate_overlay`, `hecate_realm`, `hecate_station*`)
- Nothing ever carries a `_v2` suffix — the repo and version numbers convey version

---

## 9. Migration strategy

Pre-user, no migration in the traditional sense. Three disciplines govern the transition:

1. **Legacy freeze.** `macula-relay` receives no further code changes. V1 fleet continues running frozen Docker images until V2 cutover. Critical bugs ONLY if they block the lab (unlikely).
2. **Fresh repo.** `hecate-station` starts empty. Code is cherry-picked from `macula-relay` (quicer bindings, peer_client pattern, frame encoder) but every module is reviewed and rewritten against V2 principles before committing. No copy-and-patch.
3. **Phase 8 cutover.** When `hecate-station` reaches functional parity for the mesh_chat demo, the fleet's Docker image swaps from `ghcr.io/macula-io/macula-relay:main` to `ghcr.io/hecate-social/hecate-station:main`. The ecosystem already uses `macula-demo` for deployment orchestration — that's the coordination point.

Code reuse ratio expectation: **~40% cherry-picked from V1, ~60% new V2 code.** The cherry-picked portion is primarily quicer transport + frame encoding; everything identity-adjacent (handlers, DHT, routing) is new.

---

## 10. Open questions

Documented in Part 9; tracked here for visibility. Decisions needed BEFORE the relevant Phase starts:

| # | Question | Decision by |
|---|----------|-------------|
| O1 | Crypto-puzzle difficulty policy (adaptive vs fixed) | Phase 3 start |
| O2 | Blockchain anchor chain (Bitcoin, Ethereum, both, neither?) | Phase 6 start |
| O3 | Foundation vs federated bootstrap-anchor operators in Year 1 | Phase 6 start |
| O4 | UK + CH + EEA + western Balkans inclusion — exact list | Phase 7 start |
| O5 | IPv4 fallback depth — CGNAT-only operators allowed? | Phase 4 start |
| O6 | mDNS bootstrap privacy default (announce vs stealth) | Phase 6 start |
| O7 | GDPR controller model — three-level (node / realm / foundation)? | Phase 7 start; needs legal review |
| O8 | Gateway-tier incentive model — foundation-funded v1, tokens v2? | V2 launch |
| O9 | Post-quantum migration timeline (Dilithium or hybrid) | Revisited Phase 9 |
| O10 | ~~Quinn vs quicer NIF~~ — **RESOLVED 2026-04-14: Quinn.** V1 already migrated; V2 inherits. | — |

---

## 11. References

### Superseded plans (archived)

All moved to `~/.claude/plans/archive/`:

- `PLAN_MACULA_ROUTING_V2.md` — my earlier monolithic V2 draft. Split into Parts 1-9 of this plan.
- `PLAN_MACULA_RELAY_RESILIENCE.md` — 7 pillars. Now Part 4 of this plan.
- `PLAN_MESH_PATHFINDING.md` — Lightning-style source routing. Now Part 3 of this plan.
- `PLAN_INDESTRUCTIBLE_ROUTING.md` — multi-homing, edge-relay concepts. Absorbed into Parts 4-5.
- `PLAN_RELAY_DISCOVERY_ROUTING.md` — three-tier discovery. Subsumed by Part 3 S/Kademlia + Part 2 tier hierarchy.
- `PLAN_DIRECTED_RPC.md` — directed RPC. Superseded by Part 3 source routing.
- `PLAN_RELAY_HARDENING.md` — S/Kademlia content. Now Part 5.
- `PLAN_REALM_SERVER_ARCHITECTURE.md` — realm-scoping. Referenced in Parts 1-2; realm server code continues separately at `macula-io/macula-realm`.

### Complementary active plans (not superseded)

- `macula-relay/plans/PLAN_MACULA_RELAY_REFACTOR.md` — P0-P2 shipped; P3-P5 superseded by this plan's Part 4. Document retained as historical record of the P0-P2 work.
- `THREAT_MODEL_MACULA.md` — threat-vector catalogue. Referenced throughout; may live alongside this plan in `hecate-station/plans/`.
- `PLAN_MNS_AND_REALM_JOIN.md` — realm governance; application-level, rides on V2.
- `PLAN_GIT_OVER_MESH.md` — future application.
- `PLAN_LOCAL_FIRST_BOOT.md` — daemon-level, rides on V2.

### Academic + production systems studied (short list; fuller list in Part 9)

- **Maymounkov & Mazières** — *Kademlia* (2002)
- **Baumgart & Mies** — *S/Kademlia* (ICPADS 2007)
- **Freedman, Freudenthal, Mazières** — *Coral* (NSDI 2004)
- **Das, Gupta, Motivala** — *SWIM* (DSN 2002) + *Lifeguard* (Dadgar et al.)
- **Leitão, Pereira, Rodrigues** — *HyParView*, *Plumtree* (DSN/SRDS 2007)
- **Lightning BOLT #4 / #7** — onion failure taxonomy + gossip
- **Iroh / Number 0** — closest production analog (QUIC + PKARR + home-relay pattern)
- **BitTorrent Mainline DHT** — Kademlia at 20M-node internet scale, 20+ years operational
- **IPFS Amino DHT** — what NOT to do at scale (the 30-40% record loss rate we specifically engineer against)
- **I2P netDb floodfill** — relay-tier-as-discovery-layer reference

---

## 12. History

### What triggered V2

2026-04-13 through 2026-04-14 saw seven structural bugs surface within 72 hours:

1. DHT records aged out — no republish (Phase 0 hotfix).
2. Broadcast fan-out race — `procedure_not_found` from wrong peer beats correct reply.
3. Stub URL in DHT ≠ peer_clients key — cross-relay calls never routed correctly.
4. Peer CONNECT `endpoint` was destination not sender — broke `via` attribution.
5. Alias-map broadcast amplification — same pid received every message 50×.
6. Stale handler_node ghost pids — `alive_providers` picked dead handlers.
7. dist_bridge CRASH REPORT on clean reader exit — noise, not bug, but visible.

Each fix landed. Each revealed the next rake. The pattern made clear: V1 lacks a systematic resilience model. Today's ad-hoc fixes are technical debt in code scheduled for replacement.

### What V2 is NOT a reaction against

- WAMP / bondy — V1's first architecture, already replaced
- libp2p — never adopted, considered and rejected pre-V1
- IPFS architecture — useful contrast; V2 steals lessons, not shape
- Matrix / Nostr — parallel-track federated systems; different problem space

### Decision history

- 2026-04-13 (earlier today):
  - Routing V2 plan drafted as single 1700-line document.
  - Threat model drafted separately.
  - Relay Resilience plan drafted separately (7 pillars).
  - All three had overlapping content.
- 2026-04-14 (planning session):
  - Consolidation to single V2 plan agreed.
  - New private repo `hecate-social/hecate-station` agreed (Q1).
  - Walking-skeleton Phase 1 definition agreed (Q2).
  - Async-checkpoint review rhythm agreed (Q3).
  - Legacy P3 SWIM-wire work absorbed into V2 Part 4 (Q4).
  - SDK home = `macula-io/macula` (v2 on main); station home = `hecate-social/hecate-station` (Q5, revised).
  - Name `station` chosen over `relay` / `router` / `node` (Q1.5).
  - `_v2` suffix dropped; `_macula.*` topic namespace for protocol-layer (Q2 follow-up).
- 2026-04-14 (Phase 1 kickoff):
  - **SDK / station naming split**: `macula_*` apps stay in macula-io/macula; station-role apps in hecate-station rename `macula_*` → `hecate_*`. Test: "shared meaningfully" between station + daemon + stub ⇒ SDK; otherwise station.
  - **`macula` SDK = umbrella** with 7 sub-apps: `macula`, `macula_identity`, `macula_record`, `macula_frame`, `macula_transport`, `macula_peering`, `macula_diagnostics`.
  - **Quinn NIF** lives at `apps/macula_transport/native/macula_quic/` (was umbrella root); pre_hook moved to `macula_transport/rebar.config`. Enables `{git_subdir, ...}` consumption.
  - **hecate-station deps** = 7 `git_subdir` entries to macula v2 branch until 2.0.0 ships on hex.
  - **Diagnostics** SDK only — namespacing via event names (`_macula.*` vs `_hecate.*`).
  - **macula_handler** folded into `macula` facade (client-side `advertise/3`); station owns `hecate_handler` (server-side dispatch registry).
  - O10 (Quinn vs quicer) RESOLVED: Quinn.
  - macula v2 branch cut + pushed; macula_identity fully implemented (eunit 19/0); both repos compile + xref + dialyzer clean.

### Related session logs

- `~/.claude/my-sessions/2026-04-13_mesh-chat-handoff-debug.md`
- `~/.claude/my-sessions/2026-04-14_*.md` (this session — summarising this document)

---

## 13. Next concrete actions

1. Create private repo: `hecate-social/hecate-station` via `gh repo create` + push this ROOT plan.
2. Archive `macula-io/macula-relay`: GitHub archive flag + README-replace pointing at station.
3. Update `MEMORY.md` (auto-memory) to reflect the consolidated plan.
4. Begin Part 1 authoring in the next session.
5. After all Parts written, begin Phase 0 (actual code-bootstrap work).

---

## 14. Status summary for quick-scan sessions

- ✅ ROOT authored
- ✅ Part 1 — Foundations (2026-04-14)
- ✅ Part 2 — Topology (2026-04-14)
- ✅ Part 4 — Lifecycle & Resilience (2026-04-14)
- ✅ Part 3 — Discovery & Routing (2026-04-14)
- ✅ Part 5 — Bootstrap & Governance (2026-04-14)
- ✅ Part 6 — Wire Protocol Catalog (2026-04-14)
- ✅ Part 7 — Implementation Plan (2026-04-14)
- ✅ Part 8 — Verification (2026-04-14)
- ✅ Part 9 — Open Questions + Appendices (2026-04-14)
- ⏳ Repo creation (Phase 0 — next concrete action)

---

*This ROOT is intentionally short. It's the map, not the territory. Each Part is where the technical substance lives.*
