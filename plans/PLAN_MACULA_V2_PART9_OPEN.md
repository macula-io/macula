# PLAN — Macula V2, Part 9: Open Questions + Appendices

**Parent:** `PLAN_MACULA_V2_ROOT.md`
**Depends on:** Parts 1–8 (all substantive design).
**Feeds:** nothing — Part 9 is the closing chapter.
**Status:** Draft — authored 2026-04-14.
**Scope:** Master index of every open question accumulated across the plan (O1–O39), a canonical glossary, external references studied, list of superseded plans, decision history, acknowledgements, a design-completion checklist, and resume instructions for the first implementation session.

---

## 1. Purpose

Parts 1–8 defined the design. Part 9 is the reference shelf — the single place to look up an open question, a term, a paper, or the reason a decision was made.

Three pragmatic goals:

1. **One table for every unresolved question** so no decision lives only in one Part's §"Open questions" section.
2. **One definition per term** so the plan speaks consistently.
3. **One closing checklist** that says "the design is ready; start Phase 0."

---

## 2. Master open-questions index (O1–O39)

Consolidated from Parts 1–8. Each entry has: number, question, current leaning, decision deadline, the Part that owns it. Priority classification in §3.

| # | Question | Current leaning | Decide by | Owner |
|---|----------|-----------------|-----------|-------|
| **O1** | Crypto-puzzle difficulty policy — adaptive or fixed? Who signs difficulty updates? | Adaptive, foundation FROST signs | Phase 3 start | Part 3 §12.3, Part 5 §12 |
| **O2** | Blockchain anchor chain — Bitcoin, Ethereum, both? | Both (redundancy, ~240 EUR/year) | Phase 6 start | Part 5 §7.4 |
| **O3** | Foundation custodian set — how many, which organisations, which jurisdictions? | 5 custodians, 3-of-5 FROST, EU-spread | Phase 6 start | Part 5 §12.2 |
| **O4** | Jurisdiction list — UK + CH + EEA + Western Balkans inclusion? | EU27 + EEA + UK + CH baseline; Balkans opt-in | Phase 7 start | Part 2 §6.1 |
| **O5** | CGNAT-only operators — allowed as stations or nodes-only? | Degraded-T0 station; no routing participation | Phase 4 start | Part 2 §5.3 |
| **O6** | mDNS bootstrap default — announce on or off? | Announce on (LAN = trust boundary) | Phase 6 start | Part 5 §5.3 |
| **O7** | GDPR controller model — three-level (node / realm / foundation)? | Yes, pending legal review | Phase 7 start (legal) | Part 1 §9.2 |
| **O8** | Gateway-tier incentive model — foundation-funded v1, tokens v2? | Foundation-funded; tokens deferred | V2 launch | Part 2 §3 |
| **O9** | Post-quantum migration — Dilithium, hybrid, timeline? | Hybrid classical+PQ; timeline TBD | Phase 9 | Part 1 §4.1 |
| **O10** | Quicer NIF vs Quinn/Rust — stay or switch? | Stay with quicer unless Phase 1 chaos flags issues | Phase 1 start | Part 1 §8.5 |
| **O11** | "T-1" tier for nodes-as-proxy-stations? | No; nodes ≠ stations. Revisit on user friction | Post-V2.0 | Part 2 §13 |
| **O12** | Tunnel-only (Hurricane Electric) stations — full T0 or degraded? | Full T0, reduced tier-diversity weight | Phase 4 | Part 2 §5.5 |
| **O13** | SWIM group partition threshold (currently 256)? | Keep 256 until 1000-station chaos test | Phase 2 chaos | Part 4 §5.4 |
| **O14** | Heartbeat period under Tier 3 (currently 15 s)? | 15 s; measure on constrained bandwidth | Phase 7 field | Part 4 §9.3 |
| **O15** | Tombstone retention (currently 2× tExpire = 96 h)? | 96 h; adversary replay attack may push to 7 days | Phase 3 review | Part 4 §11.2 |
| **O16** | Lifeguard self-awareness sensitivity on RPi-class HW? | Calibrate against HashiCorp defaults | Phase 2 calibration | Part 4 §5.2 |
| **O17** | Source-route path cache TTL (currently 5 min)? | 5 min; shorter if churn observed | Phase 4 tuning | Part 3 §6.3 |
| **O18** | k=3 vs k=5 disjoint paths? | k=3 at V2.0; revisit if chaos inadequate | Phase 7 | Part 3 §4.2 |
| **O19** | HyParView active-view ceiling (currently 15)? | 15; revisit for realms >1000 members | V2.1 | Part 3 §7.1 |
| **O20** | Subscription-hint aggregation — per-realm or per-station-global? | Per-realm (avoid cross-realm leakage) | Phase 5 | Part 3 §5.4 |
| **O21** | Tier-penalty coefficient in source-route edge weighting? | Heuristic now; chaos-calibrate | Phase 4 | Part 3 §6.3 |
| **O22** | Bootstrap-tier observation attestation — opt-in or required? | Opt-in (privacy) | Phase 6 | Part 5 §11.4 |
| **O23** | Firmware signing — foundation or coop? Reproducible builds mandatory? | Reproducible builds mandatory; signer = foundation-FROST | Phase 6 | Part 5 §11.2 |
| **O24** | Foundation key rotation rehearsal cadence? | Quarterly drill | Phase 6 | Part 5 §12.4 |
| **O25** | BERT-vs-CBOR regret on RPi-class hardware? | Measure at Phase 2; revisit if parse cost bites | Phase 2 measurement | Part 6 §2.1 |
| **O26** | Max source-route hops = 8 — too restrictive for Phase 9 onion routing? | Revisit when onion planned | Phase 9 | Part 6 §11 |
| **O27** | Error code space 0x80–0xFF — realm-app arbitration? | Foundation-maintained registry | Phase 7 | Part 6 §13 |
| **O28** | Tombstone tag sharing vs 0x0C distinct? | 0x0C distinct (locked) | *Resolved 2026-04-14* | Part 6 §9.1 |
| **O29** | Anonymous-auth cipher for realm-join flows? | Deferred to realm-join plan | Separate plan | Part 6 §13 (0x15) |
| **O30** | Extract `macula_*` apps to separate `macula-io/macula-mesh` library now vs wait for second consumer? | Wait (YAGNI) | V2.1 review | Part 7 §2 |
| **O31** | Quicer vs Quinn-NIF final call? | Stay quicer; Phase 1 stress confirms | Phase 1 | Part 7 §17 |
| **O32** | CI OTP 27 + 28 matrix or OTP 27 only? | OTP 27 only until Phase 7 hardening | Phase 7 | Part 7 §14.1 |
| **O33** | Sigstore + foundation co-sign of release artefacts — when to add? | Phase 7 hardening | Phase 7 | Part 7 §14.2 |
| **O34** | Simulation harness — build custom or adopt existing (cuter/proper_statem)? | PropEr native + custom harness | Phase 2 | Part 8 §6.1 |
| **O35** | TLA+ spec effort/value in V2.0? | Defer to V2.1 unless audit demands | V2.1 | Part 8 §11 |
| **O36** | Foundation-signed conformance test vector registry — who maintains? | Foundation, monthly refresh | Phase 8 | Part 8 §5.2 |
| **O37** | Public chaos-report transparency — publish openly? | Yes, post-Phase-8 (ecosystem credibility) | Post-Phase-8 | Part 8 §13 |
| **O38** | Cross-implementation conformance partner (Rust port)? | No committed partner; track interest | TBD | Part 8 §5.3 |
| **O39** | Burn-in duration pre-cutover (72 h minimum)? | 72 h minimum; 7 days if schedule permits | Phase 7 | Part 8 §9.2 |

### 2.1 Resolved during plan authoring (2026-04-14)

| # | Question | Resolution |
|---|----------|-----------|
| — | Single consolidated plan vs three overlapping plans? | Single plan; three legacy plans archived. |
| — | Private repo for station implementation? | Yes — `hecate-social/hecate-station`. |
| — | Archive `macula-io/macula-relay`? | Yes; README redirect; V1 images continue until Phase 8 cutover. |
| — | SDK home location? | `macula-io/macula` (V2.x on main). |
| — | Naming the product unit? | **Station** (not relay, not hub, not router). |
| — | `_v2` suffix on new code? | Dropped; versioning at repo + hex level. |
| — | Walking-skeleton definition? | Two stations exchanging signed `node_record` over QUIC + tombstone-on-stop. |
| — | Review cadence? | Async checkpoint per Phase gate. |
| — | Legacy `macula-relay` refactor P3 SWIM-wire? | Absorbed into V2 Part 4. |
| — | Authoring order? | 1 → 2 → 4 → 3 → 5 → 6 → 7 → 8 → 9 (lifecycle before routing). |
| — | Tombstone record tag disposition? | Distinct tag 0x0C (O28 resolved). |

---

## 3. Open-questions priority classification

### 3.1 Gating (must resolve before a named phase can proceed)

- **Phase 1:** O10 (Quicer vs Quinn), O31 (same).
- **Phase 2:** O13 (SWIM group size), O16 (Lifeguard calibration), O25 (BERT/CBOR measurement), O34 (harness tool).
- **Phase 3:** O1 (puzzle difficulty policy), O15 (tombstone retention review).
- **Phase 4:** O5 (CGNAT station policy), O12 (tunnel-only tier weight), O17 (path cache TTL), O21 (tier-penalty coeff).
- **Phase 5:** O20 (hint aggregation scope).
- **Phase 6:** O2 (blockchain chain), O3 (foundation custodians), O6 (mDNS default), O22 (bootstrap attestation), O23 (firmware signing), O24 (rotation cadence).
- **Phase 7:** O4 (jurisdiction list), O7 (GDPR controller), O14 (Tier 3 heartbeat), O18 (k=3 disjoint review), O27 (error code registry), O32 (CI matrix), O33 (sigstore).
- **Phase 8 gate:** O36 (vector registry), O37 (chaos-report transparency), O39 (burn-in duration).

### 3.2 Operational (tune in-flight)

- O30 (extract macula_* library timing), O11 (T-1 tier), O19 (HyParView ceiling).

### 3.3 Strategic (cross-phase, long-term)

- O8 (gateway incentive), O9 (post-quantum), O26 (max source-route hops), O35 (TLA+ adoption), O38 (cross-implementation partner).

### 3.4 Deferred to separate plans

- O29 (realm-join cipher) — belongs to `PLAN_MNS_AND_REALM_JOIN`.

---

## 4. Glossary

Canonical definitions. Where a term could be confused with V1 usage, V1's meaning is marked ⚠ **retired**.

### 4.1 Organisational

- **BEAM Campus** — The company. Consultancy + products.
- **Hecate** — The flagship product: Sovereign Application Platform.
- **Foundation** — BEAM Campus / future "Macula Foundation". Optional trust root.
- **Venture / Division / Department / Desk / Dossier** — Company-internal process vocabulary (see `CLAUDE.md`). Not protocol terms.

### 4.2 Protocol units (Part 2 §10)

- **Station** — The deployable product unit. Street-level ARM/x86 device; runs Macula + serves realms. 4–30 EUR/month hardware class. ⚠ **V1 "relay" retired.**
- **Node** — A BEAM VM where Hecate applications run. Each node connects outbound to one station.
- **Device** — The physical hardware. Can host a station + nodes.
- **Peer** — Any two entities at the same tier (station-peer, node-peer).
- **Realm** — A sovereignty boundary; one admin key (possibly threshold).
- **Gateway** — A station with elevated capacity that self-declared tier ≥ 1.
- **Route** — A source-computed sequence of station hops for relay-forward.
- **Record** — A signed piece of state in the DHT.
- **Procedure** — A named callable endpoint; canonical URI form `{realm}/{org}/{app}/{domain}/{name}_v{N}`.

### 4.3 Identity

- **NodeId** — 32-byte Ed25519 pubkey; node's identity.
- **RealmId** — 32-byte Ed25519 pubkey; realm's identity (admin-held).
- **StationId** — 32-byte Ed25519 pubkey; station's identity.
- **CallId** — 16-byte UUIDv7; caller-assigned.
- **RecordVersion** — 16-byte UUIDv7; time-ordered.

### 4.4 Topology

- **Tier (T0–T4)** — T0 street station (default), T1 metro/ISP gateway, T2 country gateway, T3 continental, T4 foundation anchor (optional).
- **Diversity axes** — Station, operator, ASN, country, IPv6 prefix, tier.
- **IPv6 GUA** — Global Unicast Address. V2 requires.
- **CGNAT** — Carrier-grade NAT (IPv4). V2 treats CGNAT-only stations as degraded.

### 4.5 Lifecycle and resilience

- **Pillar 1..7** — The 7 invariants (Part 4 §3–§9): process-resource binding, single-provider, liveness probes, fast-fail, cascade refresh, idempotent ops, graceful degradation.
- **SLA tier (S, 1, 2, 3, 4)** — Behavioural modes under stress (Part 4 §9).
- **SWIM** — Scalable Weakly-consistent Infection-style Membership protocol (Das et al. 2002).
- **Lifeguard** — SWIM extensions by HashiCorp (self-awareness, dogpile, refutation-buddy).
- **HyParView** — Hybrid Partial View overlay protocol (Leitão et al. 2007).
- **Plumtree** — Push-lazy gossip protocol (Leitão et al. 2007).
- **OR-Set / delta-state CRDT** — Conflict-Free Replicated Data Types for realm-shared state.
- **tReplicate** — Custodian-initiated 1 h replica refresh cycle.
- **tRepublish** — Owner-initiated 24 h record refresh cycle.
- **tExpire** — 48 h TTL before custodian drops unrefreshed record.
- **REFRESH phase** — 30 s post-reconnect state-sync (Part 4 §7).

### 4.6 Discovery and routing

- **Kademlia / S/Kademlia** — DHT family; S/Kademlia adds crypto puzzle, siblings, disjoint paths.
- **XOR distance** — Metric over NodeId space.
- **Bucket** — Routing-table slot for peers sharing a NodeId prefix.
- **Sibling list** — Extra s=16 closest-by-XOR peers.
- **Disjoint path (d=3)** — Path-disjoint parallel lookups.
- **Suurballe** — Algorithm for k disjoint shortest paths over a weighted graph.
- **Source-routing header** — 28 + 16×N bytes encoding hop sequence (Part 6 §11).
- **BOLT#4** — Lightning Network's failure taxonomy; V2 adapts to structured error codes.
- **Crypto puzzle** — `leading_zeros(SHA-256(pubkey)) ≥ difficulty`; Sybil-raising cost.
- **Eclipse attack** — Adversary surrounds target's routing table.
- **Byzantine relay** — Adversarial station that participates but misbehaves.

### 4.7 Wire and encoding (Part 6)

- **CBOR** — Concise Binary Object Representation (RFC 8949). Used for signed long-lived records.
- **BERT** — Binary ERlang Term format. Used for intra-mesh frames.
- **Canonical encoding** — Deterministic serialisation (RFC 8949 §4.2.1 for CBOR; sorted-map-key BERT).
- **Domain separation** — Prefix string added before signing/hashing to prevent cross-context signature reuse.
- **Ed25519** — EdDSA over Curve25519 (RFC 8032).
- **FROST** — Flexible Round-Optimized Schnorr Threshold signatures.
- **Capability bits** — 32-bit bitmask declaring feature support, negotiated at HELLO.

### 4.8 Bootstrap

- **PKARR** — Public-Key Addressable Resource Records (https://github.com/Pubky/pkarr).
- **Mainline DHT** — BitTorrent's Kademlia-based DHT, ~20M nodes.
- **DoH** — DNS-over-HTTPS (RFC 8484).
- **Anycast** — IP routing convention where one address maps to many hosts; nearest wins.
- **RPKI** — Resource Public Key Infrastructure (RFC 6480); BGP prefix origin signing.
- **Tombstone** — Signed supersession record.
- **Endorsement** — Realm-admin-signed attestation of membership/status.

### 4.9 V1 terms retired ⚠

- **Relay** (as product unit) — replaced by Station.
- **Hub** — replaced by Station or Gateway.
- **Boot** (V1 boot→relay rename era) — retired.
- **Box** — slang; now Device.
- **Virtual relay identity** — V2 stations are 1:1 with physical devices.
- **`_v2` suffix** — retired; versioning at repo + hex level.
- **`_relay.*` topic namespace** — replaced by `_macula.*`.
- **`_chat.*` / `_marketplace.*`** — replaced by `_hecate.chat.*`, `_hecate.marketplace.*`.

---

## 5. References

### 5.1 Academic / core papers

| Ref | Title | Citation |
|-----|-------|----------|
| [KAD02] | Kademlia: A Peer-to-peer Information System Based on the XOR Metric | Maymounkov & Mazières, IPTPS 2002 |
| [SKA07] | S/Kademlia: A Practicable Approach Towards Secure Key-Based Routing | Baumgart & Mies, ICPADS 2007 |
| [COR04] | Democratizing Content Publication with Coral | Freedman, Freudenthal, Mazières, NSDI 2004 |
| [SWIM02] | SWIM: Scalable Weakly-consistent Infection-style Process Group Membership Protocol | Das, Gupta, Motivala, DSN 2002 |
| [LG18] | Lifeguard: SWIM-ing with Situational Awareness | Dadgar et al., arXiv:1707.00788 |
| [HP07] | HyParView: a membership protocol for reliable gossip-based broadcast | Leitão, Pereira, Rodrigues, DSN 2007 |
| [PT07] | Epidemic Broadcast Trees | Leitão, Pereira, Rodrigues, SRDS 2007 |
| [SUUR74] | Disjoint paths in a network | Suurballe, Networks 1974 |
| [CRDT11] | Conflict-Free Replicated Data Types | Shapiro, Preguiça, Baquero, Zawirski, INRIA 7687 |
| [DSC18] | Delta State Replicated Data Types | Almeida, Shoker, Baquero, JPDC 2018 |

### 5.2 Production systems studied

| System | Role in V2 | Lesson |
|--------|-----------|--------|
| **BitTorrent Mainline DHT** | Tier C bootstrap bridge | Kademlia at internet scale for 20+ years |
| **Iroh / Number 0** | Closest production analog | QUIC + PKARR + home-relay pattern |
| **IPFS Amino DHT** | What NOT to do | 30–40% record loss rate is what we engineer against |
| **I2P netDb floodfill** | Reference for relay-tier-as-discovery | Tier-based discovery layer |
| **Lightning Network (BOLT#4, BOLT#7)** | Failure taxonomy; gossip | Signed structured errors over retry timeouts |
| **Tor directory authorities** | Reference for bootstrap-anchor model | Small trusted set + consensus |
| **Consul / HashiCorp memberlist** | SWIM-Lifeguard implementation | Battle-tested self-awareness tuning |
| **PKARR (Pubky project)** | Record format compatibility | DNS-packet-compatible signed records |
| **libp2p** | Considered, rejected | Assumption mismatch for street-level Macula |
| **Matrix / ActivityPub** | Parallel-track federation | Different problem space; not competitors |

### 5.3 RFCs + specifications

- **RFC 8032** — Edwards-Curve Digital Signature Algorithm (Ed25519).
- **RFC 8446** — TLS 1.3.
- **RFC 8484** — DNS-over-HTTPS (DoH).
- **RFC 8949** — Concise Binary Object Representation (CBOR).
- **RFC 9000** — QUIC transport.
- **RFC 9562** — UUIDv7 (draft-ietf-uuidrev, widely implemented).
- **RFC 6480** — Resource Public Key Infrastructure (RPKI).
- **RFC 6762** — Multicast DNS (mDNS).
- **RFC 6763** — DNS-Based Service Discovery (DNS-SD).
- **RFC 8200** — IPv6 Specification.

### 5.4 Regulatory (Part 1 §9)

- **Regulation (EU) 2016/679** — GDPR.
- **Regulation (EU) 2022/1925** — Digital Markets Act.
- **Regulation (EU) 2022/2065** — Digital Services Act.
- **Directive (EU) 2022/2555** — NIS2.
- **Regulation (EU) 910/2014** + 2024 amendments — eIDAS.
- **Regulation (EU) 2023/2854** — Data Act.
- **Regulation (EU) 2024/1689** — AI Act.
- **Regulation (EU) 2021/784** — Terrorism content regulation.

### 5.5 Lightning Network specs

- **BOLT #4** — Onion Routing Protocol (failure taxonomy source).
- **BOLT #7** — P2P Node and Channel Discovery.
- **BOLT #1** — Base Protocol (framing inspiration).

### 5.6 Threat model

- `THREAT_MODEL_MACULA.md` — companion document; catalogues 12+ threat vectors. Referenced from Part 1 §2 and Part 3 §12.4.

---

## 6. Superseded plans (archived 2026-04-14)

Moved to `~/.claude/plans/archive/`:

| Plan | Disposition |
|------|-------------|
| `PLAN_MACULA_ROUTING_V2.md` | Monolithic V2 draft. Split into this plan's Parts 1–9. |
| `PLAN_MACULA_RELAY_RESILIENCE.md` | 7 pillars. Now Part 4. |
| `PLAN_MESH_PATHFINDING.md` | Lightning-style source routing. Now Part 3. |
| `PLAN_INDESTRUCTIBLE_ROUTING.md` | Multi-homing, edge-relay. Absorbed into Parts 4–5. |
| `PLAN_RELAY_DISCOVERY_ROUTING.md` | Three-tier discovery. Subsumed by Part 3 + Part 2. |
| `PLAN_DIRECTED_RPC.md` | Directed RPC. Superseded by Part 3 source routing. |
| `PLAN_RELAY_HARDENING.md` | S/Kademlia content. Now Part 5. |

### 6.1 Related active plans (not superseded)

- `macula-relay/plans/PLAN_MACULA_RELAY_REFACTOR.md` — P0–P2 shipped; P3–P5 superseded by V2 Part 4. Retained as historical record.
- `PLAN_MNS_AND_REALM_JOIN.md` — Realm governance; application-level; rides on V2.
- `PLAN_GIT_OVER_MESH.md` — Future application.
- `PLAN_LOCAL_FIRST_BOOT.md` — Daemon-level; rides on V2.
- `THREAT_MODEL_MACULA.md` — Companion.

---

## 7. History

### 7.1 The 72-hour trigger (2026-04-11 → 2026-04-14)

V1 accumulated **seven structural bugs in 72 hours**:

1. DHT records aged out — no republish (hotfix landed).
2. Broadcast fan-out race — `procedure_not_found` from wrong peer beats correct reply.
3. Stub URL in DHT ≠ `peer_clients` key — cross-relay calls never routed.
4. Peer CONNECT `endpoint` was destination not sender — broke `via` attribution.
5. Alias-map broadcast amplification — same pid received every message 50×.
6. Stale `handler_node` ghost pids — `alive_providers` picked dead handlers.
7. `dist_bridge` CRASH REPORT on clean reader exit — noise, not bug, but visible.

Each fix landed. Each revealed the next rake. The pattern made clear V1 lacked a systematic resilience model.

### 7.2 Decision timeline

**2026-04-13:**
- Routing V2 plan drafted as single 1700-line document.
- Threat model drafted separately.
- Relay Resilience plan drafted separately (7 pillars).
- All three had overlapping content.

**2026-04-14 (this session):**
- Consolidation to single V2 plan agreed.
- New private repo `hecate-social/hecate-station` agreed.
- Walking-skeleton Phase 1 definition agreed.
- Async-checkpoint review rhythm agreed.
- Legacy P3 SWIM-wire work absorbed into V2 Part 4.
- SDK home = `macula-io/macula` (v2 on main).
- Station home = `hecate-social/hecate-station`.
- Name **station** chosen over relay / router / node.
- `_v2` suffix dropped; `_macula.*` topic namespace.
- ROOT + Parts 1–9 authored end-to-end in one work session.
- Authoring order: 1 → 2 → 4 → 3 → 5 → 6 → 7 → 8 → 9 (lifecycle before discovery).

### 7.3 What V2 is NOT a reaction against

- WAMP / bondy — V1's first architecture; already replaced in V1.
- libp2p — considered and rejected pre-V1.
- IPFS architecture — useful contrast; V2 borrows lessons not shape.
- Matrix / Nostr — parallel-track federated systems; different problem.

---

## 8. Acknowledgements

V2's architecture stands on the shoulders of:

- **Maymounkov, Mazières, Baumgart, Mies** for Kademlia and its secure extension.
- **Das, Gupta, Motivala, and the HashiCorp / Dadgar team** for SWIM and Lifeguard.
- **Leitão, Pereira, Rodrigues** for HyParView and Plumtree — the clearest thinking on realistic gossip.
- **The Lightning Network contributors** for BOLT#4's failure taxonomy, a template for structured errors.
- **The Iroh / Number 0 team** for proving home-relay + QUIC + PKARR is a deployable shape.
- **The PKARR / Pubky project** for the record format that makes federated identity simple.
- **The BitTorrent Mainline DHT operators** for 20 years of free bootstrap infrastructure.
- **The RIPE NCC** for RPKI + bulk-WHOIS + RIS, without which diversity computation would be impossible.

V2's failures will be our own; its successes lean on this collective work.

---

## 9. Design-completion checklist

The design is ready to execute when every box below is checked.

### 9.1 Content

- [x] ROOT authored (scope, hierarchy, phases, part catalog).
- [x] Part 1 — Foundations (principles, identity, trust, scale, hardware, jurisdiction, taxonomy).
- [x] Part 2 — Topology (5 tiers, NodeId semantics, addresses, diversity axes).
- [x] Part 3 — Discovery & Routing (S/Kademlia, source routing, overlay).
- [x] Part 4 — Lifecycle & Resilience (7 pillars, SWIM-Lifeguard, failure taxonomy).
- [x] Part 5 — Bootstrap & Governance (5-tier cascade, governance layers).
- [x] Part 6 — Wire Protocol Catalog (frames, records, signing, error codes).
- [x] Part 7 — Implementation Plan (repo layout, modules, 8 phases).
- [x] Part 8 — Verification (pyramid, pillar-as-property, chaos, burn-in).
- [x] Part 9 — Open Questions + Appendices (this document).
- [x] THREAT_MODEL_MACULA.md exists and is referenced.

### 9.2 Process

- [x] Seven superseded plans archived.
- [x] Authoring order resolved (1, 2, 4, 3, 5, 6, 7, 8, 9).
- [x] Each Part cross-references dependencies explicitly.
- [x] Every open question numbered (O1–O39) and routed to an owning Part.
- [x] Glossary present; V1 terms marked retired.
- [x] Academic + production + RFC references cited.

### 9.3 Ready for Phase 0

- [ ] Private repo `hecate-social/hecate-station` created via `gh repo create`.
- [ ] All PLAN_MACULA_V2_* committed to `plans/` subdirectory of new repo.
- [ ] `macula-io/macula-relay` archived with README redirect.
- [ ] MEMORY.md references updated.
- [ ] Foundational open questions (O10, O31) decided so Phase 1 can start.

Status: **Design complete; repo bootstrap pending.** No further design work required before code starts. Open questions are tracked and gated — none blocks starting Phase 0.

---

## 10. Resume instructions for the first implementation session

When the next session begins to build V2:

1. **Read order:** ROOT → this Part (Part 9) for questions-index → Part 7 for implementation layout → Part 8 for test scaffolding expectations.
2. **First action:** Execute Phase 0 checklist (§9.3) — create private repo, push plans, archive V1 relay repo.
3. **Second action:** Decide O10 (Quicer vs Quinn) based on current quicer stability evidence. Lean stay-with-quicer.
4. **Third action:** Scaffold the rebar3 umbrella per Part 7 §3. Empty apps; confirm CI green.
5. **Fourth action:** Begin Phase 1 (walking skeleton) per Part 7 §6.

Each session's commit messages reference: `[Phase N / Part X §Y] description`. This keeps the plan-to-code trail traceable.

---

## 11. Closing

V1 is a prototype that taught us what the problem is. V2 is the design that takes the answer seriously.

The plan is ~4000 lines across ROOT + Parts 1–9. That is roughly one order of magnitude larger than the V1 code it will eventually supersede. The asymmetry is intentional: design once, implement repeatedly. Every argument worth having is in here; every decision worth defending is explicit; every open question has a number and an owner.

The next artefact is not more design. It is a git repo and a rebar3 skeleton.

---

*End of PLAN — Macula V2.*
