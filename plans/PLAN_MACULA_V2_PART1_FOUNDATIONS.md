# PLAN — Macula V2, Part 1: Foundations

**Parent:** `PLAN_MACULA_V2_ROOT.md`
**Status:** Draft — authored 2026-04-14.
**Scope:** The invariants everything else in V2 rests on. Design principles, identity, trust, scale, hardware floor, regulatory frame, taxonomy. No routing, no wire format, no code layout — those live in later Parts.

---

## 1. Purpose of this Part

Part 1 answers the question: **"What must be true before we can design anything else?"**

Every subsequent Part (topology, discovery, lifecycle, bootstrap, wire protocol, implementation, verification) depends on decisions made here. If a decision needs to be revisited, Part 1 is where the debate happens. Once Part 1 is frozen, downstream Parts treat it as non-negotiable.

Part 1 is deliberately opinionated. V1 accumulated seven structural bugs in 72 hours because the foundation was implicit. V2's foundation is explicit, written down, and defended.

Reading order: this Part depends on nothing. Read it first. Parts 2 and 4 reference it constantly.

---

## 2. The core claim

Macula V2 is a **sovereign, IPv6-native, Europe-bounded mesh protocol for street-level microcluster compute**, built for **millions of €4–30/month SBC stations** operated by **diverse sovereign entities** (citizens, small businesses, cooperatives, municipalities), and designed to remain functional under **residential churn, adversarial relays, state-actor traffic shaping, BGP hijack, fiber cuts, and coordinated physical attack on datacenters**.

Everything else is a consequence of this claim. If any clause above is wrong, the design is wrong.

Counter-claims considered and rejected:

| Counter-claim | Why rejected |
|---------------|-------------|
| "Global coverage from day one" | Latency + regulatory coherence break. Europe first; expand after. |
| "Datacenter-hosted relays are fine" | DCs concentrate attack surface; subscription model kills sovereignty; loses the street-level narrative. |
| "IPv4 with NAT-traversal is good enough" | CGNAT breaks inbound; NAT-traversal is brittle and adds 3rd-party dependencies. IPv6 is mature in EU (>55% and rising). |
| "Trust a small anchor tier" | One seizure collapses the network. Anchors are fallback, not critical path. |
| "Reuse libp2p / Matrix federation / IPFS" | Each carries architectural assumptions (DHT churn tolerance, HTTP homeserver, content addressing) that misfit street-level Macula. Lessons borrowed; shape rejected. |

---

## 3. Design principles — the 7 pillars

Every V2 decision passes through these seven gates. Part 4 (Lifecycle & Resilience) elaborates each with implementation detail. Part 1 introduces them so downstream Parts can reference by number.

### Pillar 1 — Process-resource binding

> **One owner, one lifetime. External resource dies ⇒ owner dies.**

Every routable piece of state (DHT record, gproc registration, pg membership, ETS entry, subscription, record lease) is bound to the lifetime of a single owner process. When the owner process exits — for any reason — the state is automatically cleaned up via OTP monitors (`erlang:monitor/2`, supervisor `terminate` callbacks, `process_flag(trap_exit, true)`).

**V1 violation example:** a node's advertised procedure remained in gproc after the node's QUIC stream closed, causing `alive_providers` to route calls to dead pids.

**V2 enforcement:** no registration may be stored without a corresponding monitor reference. Every `register_*` call returns `{ok, MonRef}`. Supervisors assert this at code review time.

### Pillar 2 — Single-provider invariant

> **Session-bound keys permit exactly one provider. Registration evicts prior.**

Some resources are structurally singleton (the `_dist.tunnel.X` for distributing Erlang messages to node `X`; the handler for a CALL in progress). Registering a new provider for a singleton key atomically evicts any prior provider, notifying them.

**V1 violation example:** stale `handler_node` ghost pids accumulated in gproc because registration did not evict prior; CALL dispatch picked dead handlers at random.

**V2 enforcement:** singleton keys are tagged in the schema. Registration for a tagged key uses `gproc:reg_other/3` + eviction with a `{evicted_by, NewPid}` message. No manual cleanup path required.

### Pillar 3 — Liveness probes, not existence checks

> **Gproc membership alone does not prove the owner is functional. Every routing decision must be backed by end-to-end heartbeat.**

A process can exist but be unresponsive (blocked on a busy NIF, infinite loop, deadlock). A gproc lookup that returns a pid does not guarantee the pid can *serve*. Every long-lived binding carries a heartbeat obligation — if the owner misses a heartbeat, it is treated as dead regardless of process liveness.

**V1 violation example:** a handler pid was alive but its QUIC stream had silently half-closed; calls routed to it timed out after 30s because nobody was probing.

**V2 enforcement:** heartbeat is part of every registration schema. Missed heartbeat ⇒ caller forcibly unregisters and picks another provider. Fast-fail (Pillar 4) applies.

### Pillar 4 — Fast-fail over silent timeout

> **Known-unreachable targets return structured errors immediately.**

A CALL to a node known to be offline must fail *immediately* with a structured error code, not wait for a timeout. Silent timeouts are the enemy of diagnosability; they hide root cause, burn retry budgets, and make post-mortem impossible.

The failure taxonomy is adapted from Lightning BOLT#4: `unknown_next_peer`, `temporary_relay_failure`, `relay_disabled`, `node_not_found_at_target_relay`, `target_realm_refused`, `loop_detected`, `expiry_too_soon`, `upstream_congestion`, `invalid_path_header`, `unknown_error`. Full table in Part 3.

**V1 violation example:** cross-relay CALL to a node whose home relay was down waited 30s for QUIC timeout, instead of the 50ms it takes to check routing-table liveness.

**V2 enforcement:** every CALL enters a state machine whose states include `resolving`, `selected_target`, `awaiting_ack`, `succeeded`, `failed`. Every transition has a bounded deadline; missed deadline ⇒ immediate structured failure.

### Pillar 5 — Cascade refresh on reconnect

> **Any reconnect triggers refresh of dependent state automatically.**

When a station reconnects to a peer after a transient disconnect, *all* state that was replicated through that peer is refreshed: subscriptions re-established, DHT records re-published, presence re-announced, gossip state re-synced.

**V1 violation example:** a station dropped and reconnected; its subscriptions were gone on the peer side but still visible locally, so publications appeared successful but silently dropped.

**V2 enforcement:** peer connection state machine has an explicit `REFRESH` phase after every `(re)CONNECT`. Peer-tracked state is versioned; refresh is a compare-and-push.

### Pillar 6 — Idempotent operations, stable call-IDs

> **Repeat registers are no-ops. Retries with same call-id dedupe. Register/unregister commute.**

Every mutating operation carries a caller-assigned idempotency key (call-id, registration-id, record-hash). A repeat of the same operation is either a no-op (if it succeeded previously) or a resume (if it was in-flight). Registration and unregistration may arrive in any order; final state is deterministic.

**V1 violation example:** a CALL retry doubled the work on the target because call-id was relay-assigned, not caller-assigned, so the retry looked like a fresh call.

**V2 enforcement:** call-id is UUIDv7 (time-ordered, caller-assigned, 128-bit). Deduplication at every hop via 10-minute bloom filter. Registrations are keyed by `(type, identity, payload_hash)` — same hash ⇒ no mutation.

### Pillar 7 — Graceful degradation tiers

> **Explicit SLA S / 1 / 2 / 3 / 4 with defined behaviour at each tier.**

Macula is not binary (working / broken). Under partial failure — DoS, regional outage, state-actor BGP interference, fiber cut — it degrades through defined SLA tiers, each with documented latency and success-rate targets.

| Tier | Conditions | p95 latency | Success rate |
|------|-----------|-------------|--------------|
| S (Peacetime) | Normal | <50 ms cross-EU | >99.99% |
| 1 (Partial) | One country degraded, DoS underway | <150 ms | >99% |
| 2 (Multi-region) | 2+ countries simultaneously under attack | <500 ms | >95%, best-effort ordering |
| 3 (Severe) | Only tier-D (social / blockchain) bootstrap works; most anchors down | Minutes — hours, signed receipts | Eventually-delivered |
| 4 (Blackout) | Internet unavailable; satellite / radio / DTN only | Days, store-and-forward | Eventually-delivered |

V1 hit Tier 2 SLA during the 2026-04-13 debug session because multiple failures cascaded into total unreachability. V2 targets Tier 1 under the same conditions.

Tier S, 1, 2 are in V2.0 scope. Tier 3 is Phase 7 hardening. Tier 4 is V3 (DTN integration).

---

## 4. Identity

### 4.1 NodeId

**NodeId = Ed25519 public key (256 bits, serialised as 32 bytes).** Every station, every node, every realm has one.

Properties:

- Self-generated; no central issuer.
- Binding to a private key is the only claim of identity.
- Signature by that private key is the only proof.
- No geographic or organisational information in the bits. Topology (tier, ISP, country) is *derived at runtime* from the station's IPv6 address.

Ed25519 chosen over secp256k1 (Bitcoin) because:
- Smaller signatures (64 vs 72 bytes average).
- Deterministic signing (no RFC-6979 quirks).
- Widely supported in BEAM (`crypto` module), Rust (ed25519-dalek), C (libsodium).
- Future PQ migration path (Dilithium hybrid) is feasible.

Ed25519 *not* chosen permanently. Post-quantum migration is open question O9 (decision deferred to Phase 9). Hybrid classical + PQ is the expected path.

### 4.2 RealmId

**RealmId = Ed25519 public key (256 bits).** Identical structure to NodeId; what differs is the holder and governance rules.

A realm is a sovereignty boundary. Its key is held by its admin (a single human, a multisig council, a threshold scheme). The holder of a realm's key is the realm's **root authority** for:

- Admitting new nodes (signed `node_record` with realm endorsement).
- Endorsing stations as realm-member infrastructure.
- Signing the realm directory entry (which stations currently serve this realm).
- Issuing revocations (a tombstone signed by the realm key is authoritative for that realm).

Realms MAY cross-sign each other (for federation or reputation transfer). Realms MAY appear on a public trust list signed by the foundation (for bootstrap preference). Neither is mandatory.

### 4.3 StationId

**StationId = Ed25519 public key (256 bits).** A station is a physical (or virtual) unit; its key is generated at first boot, stored encrypted on the device.

Stations *advertise* which realms they serve. A station can serve multiple realms. A realm is served by multiple stations. The relationship is N:N, expressed via signed records:

- `station → realm`: station publishes `station_realm_endorsement` signed by itself.
- `realm → station`: realm publishes `realm_station_endorsement` signed by the realm key.

Both must be present for the station to be an *authoritative* server of the realm's records. A station claiming to serve realm X without matching realm endorsement is a pretender; peers reject.

### 4.4 Crypto puzzle (S/Kademlia NodeId hardening)

To resist Sybil attacks, a NodeId is not arbitrary. It must satisfy:

```
leading_zeros(SHA-256(pubkey)) ≥ difficulty
```

Target cost: ~1 CPU-second of grinding per valid NodeId on commodity hardware. Difficulty is **adaptive** — it rises when the network observes anomalously fast identity growth (open question O1, decision Phase 3).

Cost implications:

| Actor | Cost to mint 10 000 NodeIds | Cost to mint 1 000 000 NodeIds |
|-------|----------------------------|-------------------------------|
| Legitimate (one person, one station) | 1 CPU-second. Trivial. | Irrelevant; not a use case. |
| Opportunistic adversary (home PC) | ~3 CPU-hours. Tedious. | ~12 CPU-days. Prohibitive. |
| State-level adversary (datacenter) | ~30 CPU-seconds in parallel. Trivial. | ~1 CPU-hour in parallel. Feasible. |

Against state-level adversaries the puzzle alone is insufficient; it combines with **IPv6-prefix diversity** (§4.3 of Part 2) and **realm admission** (Part 5) to box in Sybil damage.

### 4.5 Record signing

Every DHT record and every gossip message carries Ed25519 signature. A station cannot fake a record for an identity it does not hold. A Byzantine station can *refuse* to serve, *drop* requests, *lie about reachability* — but it cannot **fabricate content** attributable to others.

Signature scheme: Ed25519 over the record's canonical byte encoding (CBOR, deterministic serialisation). Details in Part 6.

### 4.6 PKARR compatibility

V2 record format is **PKARR-compatible** (Public-Key Addressable Resource Records, https://github.com/Pubky/pkarr). PKARR defines a signed-DNS-packet format that rides on DNS infrastructure and Mainline DHT. Compatibility buys:

- Interop with existing PKARR tooling (resolvers, publishers).
- Discovery via Mainline DHT as a bootstrap fallback tier (Part 5).
- DNS-over-HTTPS presence for trivially-reachable identity lookups.

Macula V2 extends PKARR with its own record types (`macula_node_record`, `macula_procedure_advertisement`, `macula_realm_directory`, `macula_gateway_capability`, etc.) encoded as typed payloads in the PKARR packet's answer section. Record type catalog lives in Part 6.

---

## 5. Trust model

### 5.1 Sovereignty unit = the realm

A realm is the sovereign unit of Macula. Within a realm, trust flows from the realm admin. Across realms, trust is explicit and limited.

- A realm is its own root CA for its members.
- A realm admin can revoke any member of that realm.
- Cross-realm trust requires explicit agreement (cross-signing, or realm-to-realm federation treaty).
- No entity — foundation, government, platform operator — holds root authority over all realms.

Macula protocol itself takes no position on *how* a realm governs. One-person realms, council realms, DAO realms, municipal realms, corporate realms all coexist. Governance is realm-local.

### 5.2 Trust zones

| Zone | Members | Trust assumption |
|------|---------|-----------------|
| **Intra-realm** | Stations + nodes in the same realm | Mutually trusting within the realm admin's governance; breach means realm-internal failure |
| **Cross-realm, cross-signed** | Two realms with mutual trust declaration | Trust limited by the scope of cross-signing (auth, directory, content — whichever is signed) |
| **Cross-realm, untrusted** | Any two realms without explicit trust | Protocol functions (lookup, public CALL) work; higher-trust operations refused |
| **Foundation** | BEAM Campus / future Macula Foundation | Signs bootstrap seeds, publishes trust lists, operates monitoring. Optional trust — all realms can operate without it |
| **Adversary** | Anyone | No trust; cryptographic defenses (signatures, puzzle, diversity) must hold |

### 5.3 What the foundation is and is not

The foundation exists because bootstrapping trust from *nothing* is impossible. Someone has to sign the initial seed list that a fresh station fetches before it can verify anything else.

The foundation:

- Publishes a signed seed list of well-known stations (monthly refresh).
- Operates monitoring that observes DHT health and publishes anomaly reports.
- Publishes a trust list of realms that have declared themselves (purely informational).
- Holds a threshold key (m-of-n FROST, HSM-backed) for the above.

The foundation **does not**:

- Operate a significant fraction of the stations.
- Hold any authority over a realm's internal state.
- Gate access to the protocol.
- Make or approve routing decisions.
- Charge, meter, or account traffic.
- Maintain a global reputation scoreboard.

Foundation compromise scenario: if the foundation's threshold key is compromised, bootstrap preferences are corrupted *for stations that trust the foundation* — but every realm still operates on its own key. Bootstrap cascade falls back to DNS PKARR, Mainline DHT, blockchain anchor, social tiers. See threat model §2.12.

### 5.4 Trust minimisation for adversarial operators

V2 assumes a meaningful fraction of stations are adversarial — compromised, lazy, or actively malicious. Design principle: **adversarial stations can refuse service but cannot corrupt protocol state**. This is achieved through:

1. Signed records (Byzantine refusal ⇒ retry elsewhere; cannot forge).
2. k=20 replication with ISP/country diversity (lose one, many remain).
3. S/Kademlia disjoint-path lookups (d=3; single adversarial path insufficient for eclipse).
4. Realm revocation (foundation and realm admin can tombstone malicious members).

Residual risk: traffic analysis by an adversarial station that observes which NodeIds connect. Not prevented in V2.0; onion-routed lookups planned for Phase 9+.

---

## 6. Scale targets

The numbers below drive algorithmic choice (Kademlia vs linear; gossip fanout; bucket size k; replication factor).

| Horizon | Stations | Nodes | Realms | Geographic span |
|---------|----------|-------|--------|-----------------|
| V2.0 MVP (6 months post-plan) | 1 000 | 10 000 | 50 | Lab + early adopters, 5–10 EU countries |
| V2 Year 1 | 10 000 | 100 000 | 500 | Full EU + EEA + UK + CH |
| V2 Year 3 | 100 000 | 5 000 000 | 10 000 | Same span; denser |
| V2 Year 5 (saturation) | 1 000 000+ | 100 000 000+ | 100 000+ | Same span; street-level density |

**V2.0 must not contain algorithms that fail between Year 1 and Year 5.** Specifically:

- No full-mesh (N² peering) at any layer.
- No linear scans over all stations (any operation). Kademlia and gossip are O(log N) and bounded.
- No global coordination events (every station participating in anything at once). Gossip is partial; DHT is local-lookup-only.
- No global reputation tracking (it would require N² observations or centralised aggregation).

**Memory budget per station, V2 Year 5 scale:**

- Routing table: ~160 buckets × 20 peers × 128 bytes/peer = ~400 KB
- Realm directory cache (stations in own realm): 500 stations × 512 bytes = 256 KB
- DHT records held (replicas): 20 × (own records + stored-for-others) ≈ 500 KB
- SWIM membership (same-tier + adjacent): ~1000 entries × 128 bytes = 128 KB
- Intra-realm gossip state (HyParView + Plumtree): 512 KB
- TLS / QUIC per-peer state: 4 KB × 200 peers = 800 KB
- **Total protocol state: ~2.5 MB** plus application state.

Fits comfortably in a 2 GB RPi. Year 5 saturation does not force hardware upgrade.

### 6.1 Why Europe first, explicitly

Europe is a single jurisdictional cluster with:
- Coherent regulatory frame (GDPR, DMA, DSA, eIDAS).
- <80ms worst-case intra-continent latency.
- >55% IPv6 penetration as of 2026, rising.
- Dense terrestrial fiber + IX mesh (minimal fiber-cut isolation).
- Comparable political stability (single-failure risk low).
- Common legal-recourse framework (realm-level disputes).

Global scope fails every one of these. V2.1 or V3 revisits expansion; V2.0 is Europe-bounded.

Specific jurisdictions *in* for V2.0 (baseline — open question O4 finalises):
- EU27 (all).
- EEA: Norway, Iceland, Liechtenstein.
- CH: Switzerland (adequacy frame).
- UK: United Kingdom (post-Brexit adequacy frame; case-by-case).

Specific jurisdictions *out*:
- Balkans, Ukraine, Moldova, Türkiye (opt-in case-by-case after V2.0).
- Russia, Belarus (sanctions / policy).
- Anything non-European.

---

## 7. Hardware floor

### 7.1 Reference devices

| Class | Example | CPU | RAM | Storage | Network |
|-------|---------|-----|-----|---------|---------|
| **Minimum viable station** | RPi 4B, RPi 5, Celeron J4105 mini-PC, ODROID H3+ | 4-core ARM A72 / low-end x86, 1.5 GHz+ | 2 GB | 16 GB SD or eMMC | 100 Mbps |
| **Typical hobbyist station** | RPi 5 w/ SSD hat, Minisforum mini-PC, NanoPi R6C | 4-8 core ARM A76 / modern x86 low-power, 2 GHz+ | 4–8 GB | 256 GB NVMe/SSD | 1 Gbps symmetric residential fiber |
| **Prosumer / SMB station** | Framework Mainboard, UnRaid-class NAS, mini-rack SFF | 6+ core modern x86 or ARM, 3 GHz | 16–32 GB | 1 TB NVMe | 1 Gbps symmetric + static IPv6 |
| **Gateway station (opt-in)** | Xeon-D embedded, EPYC mini-server, SuperMicro A2SDi | 8+ core, ECC RAM | 32–64 GB ECC | 2 TB NVMe RAID | 10 Gbps, BGP-capable, RPKI-valid |

A "station" is not a homogeneous unit. The protocol must run at the minimum viable spec; gateway spec unlocks opt-in roles (Part 2 §3, Part 5).

**BEAM on every class.** Erlang/OTP 27+ either directly on Linux (Ubuntu/Debian/Arch) or via Nerves-embedded firmware. All NIFs must compile on both ARM and x86_64; no x86-only assumptions.

### 7.2 Cost envelope

| Device class | Capital cost (EUR, 2026) | Monthly electricity (EUR, EU avg 0.30/kWh) | Monthly all-in | Target market |
|--------------|--------------------------|--------------------------------------------|----------------|---------------|
| Minimum viable | 50–100 | 2–3 | 4–6 | Hobbyists, first-adopters |
| Typical hobbyist | 200–400 | 3–6 | 8–12 | Prosumers, self-hosters |
| Prosumer / SMB | 500–1500 | 6–12 | 15–30 | Cooperatives, SMBs, municipalities |
| Gateway | 2 000–5 000 | 15–40 | 40–80 | Gateway operators (opt-in, possibly subsidised) |

Monthly figure includes amortisation (3-year straight-line) + electricity. Internet is assumed sunk (already paid). Bandwidth cost is assumed flat-rate residential (near-zero marginal).

**Target narrative: "€4–30/month of hardware at street level."** Lower bound is hobbyist RPi. Upper bound is prosumer SMB. Gateways are above; opt-in.

### 7.3 Deployment surface

V2 targets the following deployment surfaces in order of priority:

1. **Home broadband + SBC** — single device on residential fiber, IPv6 native, UPS-backed preferred.
2. **Home broadband + VM** — existing NAS or server running station as VM/container alongside other workloads.
3. **SMB / coop / municipality** — a shared station serving a small community (5–50 members).
4. **Co-located low-end VM** — entry-level VPS (Hetzner Cloud / OVH Eco / Linode Nanode) as **supplementary** station, not primary. Primary priority remains street-level.
5. **Datacenter rack** — explicitly **de-prioritised**. Allowed for foundation seeds + opt-in gateways. Not the product.

The shift from #5 to #1 as *primary* is the single largest cultural change from V1. V1's fleet is on Hetzner / Linode / local-to-the-developer mini-PCs. V2's target fleet is on thousands of residential lines.

### 7.4 Operating-system assumptions

- **Linux-based**, glibc or musl.
- **systemd** or equivalent service supervision.
- **IPv6 /64 from the upstream ISP** (or /48 for SMB, /56 typical residential).
- **No kernel module requirements**. User-space BEAM + NIFs only.
- **File system:** ext4 / btrfs / zfs acceptable. No hard dependency on any specific feature.
- **Clock:** NTP-synchronised; 1-second tolerance sufficient (call-id generation uses UUIDv7 which needs sub-millisecond when possible but tolerates seconds).

Windows and macOS are not V2.0 targets. They may run a *node* (Hecate Daemon), but the station itself is Linux-only in V2.0.

### 7.5 Boot / storage lifecycle

- Fresh install generates StationId on first boot, stores private key encrypted with a key derived from an operator-provided passphrase or TPM-sealed (if available).
- Re-install from scratch with same key requires backup+restore of the encrypted key blob.
- Key loss = station identity loss = must re-announce itself as a new identity. Realm operators re-endorse.

---

## 8. Network assumptions

### 8.1 IPv6-first

V2 **requires** IPv6 reachability for stations. A station without global IPv6 is degraded (can only participate via relay-forward) and loses most of V2's properties.

**Why strict.** IPv6 is mature in EU (BE 58%, DE 61%, FR 67%, NL 62% as of 2026). CGNAT on mobile networks is a non-IPv6 regime that breaks inbound — but mobile is a node path (outbound to home station), not a station path. Fixed-line residential fiber in target countries is effectively ubiquitous and IPv6-enabled.

**Operator experience.** Station installer probes IPv6 at first run. No IPv6 ⇒ installer explicitly warns that V2 features are degraded, offers IPv6 tunnelbroker instructions (Hurricane Electric) as workaround.

### 8.2 IPv4 fallback — via relay-forward only

IPv4 is used only for:

- Outbound connections from IPv4-only nodes (behind CGNAT).
- Legacy client interop.
- Bootstrap DNS queries (fallback).

Station-to-station IPv4 direct reachability is **not relied upon**. A station's contribution to the mesh requires IPv6. If a pair must communicate over IPv4, they use relay-forward via a mutually-reachable IPv6-enabled relay.

Open question O5 governs whether CGNAT-only operators may register as stations at all (leaning: yes, in degraded mode; they only serve nodes in the same NAT and don't participate in cross-station routing).

### 8.3 Latency model

Intra-EU latency distribution (measured, not assumed):
- Median: 25 ms (neighbouring countries, major IX).
- p95: 60 ms (cross-EU, typical).
- p99: 80 ms (Lisbon ↔ Istanbul, worst-case).
- p99.9: 120 ms (unusual routing).

**V2.0 protocol design budgets assume p95 ≤ 80 ms** for single-hop RTT between arbitrary European stations. Higher levels only under failure modes (Tier 1+).

### 8.4 Bandwidth assumptions

Per-station sustained bandwidth obligations:
- Protocol maintenance (SWIM, Kademlia refresh, gossip): ≤ 50 kbps steady-state.
- Application traffic: unbounded, rate-limited per realm.
- Gateway stations: up to 100 Mbps if opted in.

Residential fiber (1 Gbps symmetric) is vastly over-provisioned for protocol maintenance. Application traffic scaling is the realm's problem.

Bandwidth cost in EU residential context: flat-rate unlimited; marginal = zero. The protocol does not need to optimise bytes — latency is the scarce resource.

### 8.5 Transport

**QUIC over UDP, port 7000 default.**

- Every station-to-station link is a QUIC connection.
- Multi-streamed (many logical streams per connection).
- TLS 1.3 with Ed25519-derived peer identity (RFC 8446 + extended peer-identity binding).
- Stream-per-CALL for CALL traffic; stream-per-subscription for PubSub.
- Dist over QUIC (Erlang distribution tunnelled across the mesh) uses a dedicated QUIC stream.

Implementation library: **quicer** (BEAM NIF wrapper around msquic) is the V1 choice. Open question O10 (Phase 1) revisits whether to stay with quicer or switch to a Rust-based alternative (Quinn). Inertia favours quicer; we stay unless Phase 1 chaos testing reveals structural issues.

UDP multiplex alternative (DCCP, SCTP) not considered. QUIC is the standard.

### 8.6 Multicast / broadcast

No protocol-level multicast. Gossip approximates broadcast within a realm via Plumtree (Part 3). IP multicast is not used.

---

## 9. Regulatory and jurisdictional frame

### 9.1 Legal regime

V2.0's regulatory environment is the EU + EEA + UK + CH legal space. The relevant instruments:

| Instrument | Relevance | Macula implication |
|------------|-----------|---------------------|
| **GDPR** (2016/679) | Personal data processing | Realm admin is controller; foundation is controller for Europe-tier metadata. Short record TTLs + tombstones enable "right to erasure". Cross-realm data flow ⇒ cross-controller agreement. |
| **DMA** (2022/1925) | Platform gatekeepers | Not applicable at V2.0 scale; revisit if a single realm grows to 45M+ users. |
| **DSA** (2022/2065) | Intermediary liability | Stations are "mere conduits" in most cases. Realms are "hosting services" for their members' content. Notice-and-action obligations accrue at the realm level. |
| **NIS2** (2022/2555) | Cybersecurity / critical infrastructure | Foundation-operated bootstrap infrastructure may fall under NIS2 "digital infrastructure" sector; realms probably not. |
| **eIDAS** (2014/910 + 2024 amendments) | Electronic identity | Not required. Macula's key-pair identity is not an eIDAS QTS. Could be mapped to eIDAS in a future business layer. |
| **Data Act** (2023/2854) | Data sharing / portability | Mostly B2B; realms implementing business services must attend to it. |
| **AI Act** (2024/1689) | AI systems | Only relevant if realms host AI services (general Macula is out-of-scope). |
| **eCommerce Directive** (2000/31) | Online intermediaries | Subsumed into DSA for EU; still relevant in non-EU EEA. |

### 9.2 GDPR controller model (three levels)

Open question O7 (Phase 7, legal review required) decides the exact model. Current leaning:

| Data class | Controller | Processor |
|------------|-----------|-----------|
| Node-level personal data (content of messages, user profile) | Realm admin | Station operators in the realm |
| Realm directory entries (who is in realm X) | Realm admin | All stations replicating |
| Europe-tier metadata (realm exists, foundation trust list) | Foundation | Stations holding replicas |
| Transport metadata (a connection happened between IPs) | Station operator (data controller for their box) | Mostly transient; short logs |

Documented DPA (Data Processing Agreement) templates between realm admins and station operators are a foundation deliverable.

### 9.3 Data residency

Some jurisdictions / regulated industries require data residency guarantees ("all my realm's data stays in DE"). V2.0 offers **best-effort** residency via diversity-constrained replica placement (Part 3 §4.4): a realm can request ≥3 distinct countries but cannot strictly forbid replication to country X.

Strict residency enforcement (hard exclusion, geographic gating) is V2.1. Current design constraint: no structural prohibition against adding it later.

### 9.4 Lawful intercept

Open question (documented as policy item, not a technical feature). Realm admins as controllers bear the legal obligation to respond to lawful requests. Macula protocol does not itself provide intercept capability; realm admins can cooperate at the realm data plane. Foundation takes no position on realm-level compliance but publishes clear operator responsibilities.

Explicitly rejected: any protocol-level intercept backdoor.

### 9.5 Content moderation

Realms are sovereign. A realm may host content its admin considers acceptable. Another realm may refuse to federate with it. The protocol does not arbitrate.

**Exceptions** — content explicitly illegal under EU law (e.g. CSAM, terrorism under Regulation (EU) 2021/784). Foundation policy: such realms become subject to realm-level revocation by foundation (removal from bootstrap trust list) and technical isolation at the DHT level. Full mechanics deferred to a separate governance document.

The foundation **does not** host takedown infrastructure. Illegal content is a law-enforcement matter, not a protocol matter.

---

## 10. Taxonomy

Words are load-bearing. V2 drops ambiguous terms from V1 and defines a fresh vocabulary.

### 10.1 Primary nouns

| Term | Definition | What it is NOT |
|------|-----------|----------------|
| **Station** | The deployable product unit. Street-level ARM/x86 device. Runs Macula protocol + serves realms. 4–30 EUR/month class of hardware. | Not a "relay" (V1 terminology); not a "hub"; not a "gateway" unless opted-in; not a "node". |
| **Node** | A BEAM VM where Hecate applications run. Each node connects outbound to exactly one station. | Not a station. Not a user. |
| **Device** | The physical hardware. Can host a station, one or more nodes, or both. | Not a station or node per se — the chassis. |
| **Peer** | Any two entities at the same tier. Station-peer (two stations), node-peer (two nodes). | Not a role; a symmetric relationship. |
| **Realm** | A sovereignty boundary. Has one admin key (possibly threshold). Members: nodes + possibly stations. | Not a "tenant"; not a "server"; not a "room" (Matrix-like). |
| **Gateway** | A station with elevated capacity that has declared itself a tier bridge (signed `gateway_capability` record, opt-in). | Not a *role* applied by others; self-declared. |
| **Foundation** | BEAM Campus / future "Macula Foundation". Optional trust root. | Not a governing body over realms. Not a necessary party. |
| **Route** | A sequence of station hops used for relay-forward. Source-computed with k=3 disjoint paths. | Not a permanent structure; per-call. |
| **Call** | A request-response over the protocol (Macula's CALL frame). Single call-id. | Not a session; sessions are app-level. |
| **Record** | A signed piece of state in the DHT (node_record, procedure_advertisement, etc.). | Not a message; not transient. |
| **Procedure** | A named callable endpoint (`<realm>/<org>/<app>/<domain>/<name>_v<N>`). Advertised by nodes, discovered via DHT. | Not a method; not a REST endpoint. |

### 10.2 Dead words from V1

These terms are retired in V2. When found in V1 code or docs:

| V1 term | V2 replacement | Rationale |
|---------|---------------|-----------|
| Relay (as product unit) | Station | "Relay" suggests forwarding only; stations do identity + discovery + compute hosting too. |
| Hub | Station (or Gateway if opted-in) | "Hub" implies star topology, misleading. |
| Mesh member | Station or Node | Ambiguous which layer. |
| Box | Device or Station | Slang; formalised. |
| Identity (as V1 relay-virtual-identity) | Realm-endorsed station | V1's "identity" conflated stations and realms. |

### 10.3 Topic namespaces

Wire-level topics follow two namespaces:

- `_macula.*` — protocol-level system topics (peering, liveness, DHT operations). Emitted by the protocol layer itself.
- `_hecate.*` — application-level topics in the Hecate product (chat, marketplace, weather, realm admin).

Within a realm, app topics inherit the realm's namespace: `{realm-id}/{org}/{app}/{domain}/{topic_v<N>}`.

V1 `_relay.*` and `_chat.*` are retired. The former becomes `_macula.*`; the latter becomes `_hecate.chat.*`.

### 10.4 Module namespaces (code)

Inside `hecate-social/hecate-station`:

- `macula_*` — reusable Macula protocol implementation. (`macula_peering`, `macula_handler`, `macula_dht`, `macula_swim`, `macula_record`, `macula_identity`.)
- `hecate_station_*` — station-as-product shell. (`hecate_station`, `hecate_station_sup`, `hecate_station_health`, `hecate_station_config`, `hecate_station_admin`.)

Inside `macula-io/macula` (SDK, v2.x on main):

- `macula_*` — client SDK (as before). (`macula_client`, `macula_connection`, `macula_frame`, etc.)

No `_v2` suffix anywhere. No `v2_` prefix. The repo and the hex version number convey the protocol generation.

---

## 11. What V1 got right and what V2 inherits

Not everything in V1 is wrong. Part 1 of V2 explicitly names what survives.

**Inherited from V1:**

- **QUIC transport layer** (quicer NIF wrapper). Works. Battle-tested over six months of lab operation.
- **Frame encoder/decoder** (CONNECT / CALL / RESULT / PUBLISH / SUBSCRIBE / ERROR). Clean BERT-based wire format; V2 keeps the frame catalog, extends it with source-routing + failure-code frames.
- **BEAM-native identity** (Ed25519 keypairs in `crypto` module). Nothing to change.
- **Realm-as-sovereignty-boundary** concept. V1's only structural success; V2 doubles down.
- **Peer-client pattern** for outbound-initiated connections from node to station. Correctly models CGNAT'd nodes.
- **Fleet deployment via GitOps** (`macula-demo` repo orchestrates all deployments). Continues; only the Docker image swap at Phase 8.
- **Monitoring culture** (every fix accompanied by observability; every session log documented). Preserved.

**Explicitly replaced in V2:**

- V1's naive Kademlia DHT (6-min TTL, no republish). V2 uses S/Kademlia with proper tReplicate/tRepublish (Part 3).
- V1's ad-hoc peer discovery (static peer lists, relay-to-relay full mesh). V2 uses tier-diverse routing tables + S/Kademlia lookups.
- V1's absence of liveness heartbeats (process-existence checks only). V2 enforces Pillar 3.
- V1's silent timeouts on CALL failure. V2 enforces Pillar 4.
- V1's lack of source routing for relay-forward paths. V2 adds the 44-byte header (Part 3).
- V1's implicit trust model (everyone in the lab fleet trusts everyone). V2 formalises realm-scoped trust.
- V1's lack of Sybil / eclipse defense. V2 adds crypto puzzle + disjoint paths + diversity.
- V1's geography-agnostic routing. V2 adds five-tier hierarchy (Part 2).

**Explicitly deleted in V2:**

- The V1 "300 virtual relay identities across 3 boxes" fleet construct. V2 stations are 1:1 with physical devices; no virtual-identity-per-physical-box inflation.
- The V1 "boot" naming (boot→relay rename happened mid-V1; in V2 the unit is "station" and there is no "boot").
- `_v2` suffixing of new code (anti-pattern; versioning lives at repo / hex-version level).
- The V1 notion of "distinguishable realm relay vs regular relay" — in V2, any station may endorse any realm.

---

## 12. Success criteria for Part 1

Part 1 is complete when a reader can:

1. Explain **why Europe first**, not global (§6.1).
2. Name **the 7 pillars** and give a one-sentence example of each (§3).
3. Describe the **identity stack** top to bottom: NodeId / RealmId / StationId, Ed25519, crypto puzzle, PKARR compatibility (§4).
4. Describe the **three-level controller model** for GDPR (§9.2).
5. Distinguish **station / node / device / peer / realm / gateway** without hesitation (§10).
6. Name **three things V1 got right** that V2 preserves and **three things V1 got wrong** that V2 replaces (§11).
7. Predict whether a given decision lands in **Tier S, 1, 2, 3, or 4** SLA (§3 Pillar 7).

If any of these is ambiguous, Part 1 needs revision before Parts 2–9 are authored.

---

## 13. Open questions addressed in Part 1

None are closed in Part 1; all are tabled for the Part indicated.

| # | Question | Target Part / Phase |
|---|----------|---------------------|
| O1 | Crypto-puzzle difficulty policy (adaptive vs fixed) | Part 3 / Phase 3 start |
| O5 | IPv4 fallback depth — CGNAT-only operators? | Part 2 / Phase 4 start |
| O7 | GDPR controller model — three-level? | Part 9 / Phase 7 (legal review) |
| O9 | PQ migration timeline | Part 9 / Phase 9 |
| O10 | quicer vs Quinn | Part 7 / Phase 1 start |

Parts 2 and later reference these by number.

---

## 14. Next

Part 2 (Topology) depends on the hardware floor (§7), network assumptions (§8), and scale targets (§6) defined here. Authoring proceeds in the order 1 → 2 → 4 → 3 → 5 → 6 → 7 → 8 → 9 (per ROOT §6).

---

*End of Part 1. Referenced by: all subsequent Parts.*
