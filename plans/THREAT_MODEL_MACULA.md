# Threat Model — Macula Routing V2

**Scope:** Europe-bounded, street-level relay mesh, S/Kademlia DHT, IPv6-direct routing
**Plan:** `PLAN_MACULA_ROUTING_V2.md`
**Created:** 2026-04-13
**Status:** Baseline for v1; to be refined during Phase 1

---

## 1. Threat actor classes

| Actor class | Capability | Intent | Likely target |
|-------------|-----------|--------|---------------|
| **Opportunistic script kiddie** | Commodity botnet, basic tooling | Disruption for kicks, low-stakes DoS | Any exposed endpoint |
| **Cyber-criminal / cartel** | Mid-tier botnet, ransomware toolkits, traffic analysis | Monetization via extortion, data theft, spam | High-value realms (finance, healthcare), discovery infrastructure |
| **Hacktivist / advocacy group** | Moderate resources, public-facing attacks | Censorship of specific realms, deplatforming | Realms perceived as adversarial |
| **Commercial competitor** | Corporate resources, lawful + grey-zone | Degrade service, poison data, de-anonymize users | All Macula infrastructure |
| **Insider (rogue relay operator)** | Full access to one relay's code + keys | Selective betrayal, misrouting, record forging for their records | Own realm + connected relays |
| **Law enforcement (lawful)** | Legal warrants, lawful intercept within EU | Targeted investigation of individuals | Realm operators, relay operators via compulsion |
| **National security service (hostile)** | State-actor resources, BGP influence, 0-day, supply-chain | Bulk surveillance, selective disruption, coercion | Bootstrap anchors, foundation operators, key realms |
| **Civil-war / kinetic adversary** | Physical attack capability, EMP, drone swarm | Destroy critical communications | Concentrated infrastructure; all datacenters |
| **Hostile-nation actor** | Full state-actor toolkit | Deplatform / destroy / surveil across borders | Pan-European infrastructure |

Macula v1 defends realistically against classes 1-6 and raises significant barriers for 7. Classes 8-9 are out of v1 scope (Tier 3-4 survival in plan); residual design prevents complete collapse.

---

## 2. Attack vectors and defenses

### 2.1 Sybil attack

**Vector:** Attacker creates thousands of fake relay identities to dominate routing tables, partition the DHT, or eclipse victims.

**Defenses (layered):**

1. **S/Kademlia crypto puzzle**: NodeId = H(pubkey) must satisfy `leading_zeros ≥ difficulty`. ~1 CPU-second per identity at baseline. Adaptive difficulty grows with observed Sybil pressure.
2. **Realm admission**: a node joining a realm requires realm-admin signed admission token. Sybil within one realm only harms that realm.
3. **IPv6-prefix diversity**: a routing-table bucket rejects peers from the same /32 ISP prefix beyond a small threshold. Attacker must acquire diverse prefixes (expensive).
4. **Latency verification**: claimed tier membership must match measured RTT neighborhood. Liars get evicted from tier-specific routing.
5. **Social proof (optional)**: realm admins may cross-sign other trusted realms; discovery preference for cross-signed peers.

**Residual risk:** well-funded attacker with datacenter IPv6 allocation can mint ~1000 Sybils/hour. Mitigation: foundation monitoring + adaptive difficulty spikes. Not prevented, but made expensive and visible.

### 2.2 Eclipse attack

**Vector:** Attacker controls all entries in victim's routing table for some key prefix, feeding false lookup responses.

**Defenses:**

1. **S/Kademlia disjoint-path lookups**: each lookup uses `d=3` disjoint paths through the DHT. Attacker must control all paths, not just one.
2. **Tier-diverse buckets**: each bucket holds peers from multiple tiers and prefix regions. Single-source eclipse impossible.
3. **Routing-table pinning**: long-lived peers receive preference; new peers must prove themselves over time before displacing established ones.
4. **Cross-validation**: for high-sensitivity records, retrieve from ≥ 3 sources and require consistency.

**Residual risk:** coordinated state-actor with pervasive BGP access could still eclipse; out of scope for v1.

### 2.3 Byzantine relay (malicious / compromised)

**Vector:** A relay returns false records, drops requests selectively, forwards incorrectly.

**Defenses:**

1. **Ed25519 signatures on all records**: cannot forge records for identities not held. Compromised relay can refuse to serve, but cannot lie about content.
2. **Replication k=20**: dropping a record by one relay doesn't hide it; 19 others still hold.
3. **Path diversity in source routing**: if one relay misroutes, alternate paths exist.
4. **Failure-rate tracking**: relays with unusually high failure rates down-weighted in routing-table selection.
5. **Realm revocation**: realm admin can revoke a rogue relay's authority to serve realm records. Revocation propagates via DHT tombstone.

**Residual risk:** traffic analysis via relay position (which calls traverse a given relay). Addressed by optional onion-routed lookups in v2.

### 2.4 State-actor BGP hijack

**Vector:** Adversary announces route advertisements for IPv6 prefixes holding Macula relays, redirecting traffic.

**Defenses:**

1. **RPKI-valid origin**: relay operators deploy RPKI ROAs; gateway peering preferred for RPKI-valid origins.
2. **IPv6 preference over IPv4**: larger address space, harder to hijack granularly.
3. **Multi-homed gateways**: backbone relays use multiple upstream transit providers.
4. **Cryptographic record signing**: hijacked traffic reaching a relay cannot forge responses (see 2.3).
5. **Anomaly detection**: foundation-operated monitoring for mass routing-table shifts.

**Residual risk:** brief window during hijack where calls may fail. Self-healing via route convergence + client retry.

### 2.5 DNS poisoning (bootstrap vector)

**Vector:** Adversary poisons DNS responses to direct new relays to rogue bootstrap seeds.

**Defenses:**

1. **Multi-resolver DoH**: bootstrap queries sent to ≥ 3 independent DoH providers (Cloudflare, Google, Quad9, NextDNS). Consensus required.
2. **PKARR signed records**: DNS TXT records are Ed25519-signed by realm or foundation key. Poisoning produces invalid signatures.
3. **Bootstrap cascade**: DNS is order 3 in the cascade; mDNS (order 1) and cached peer DHT (order 2) try first. Adversary must poison multiple layers.
4. **Foundation seed-list signed**: the fallback seed list is distributed out-of-band and signed by foundation key.
5. **Blockchain anchor**: ultimate fallback — realm directory hash pinned to a public chain.

**Residual risk:** first-ever bootstrap on a fresh install without any prior cache. Mitigation: ship installers with baked-in foundation public key + signed initial seed list.

### 2.6 Datacenter / physical destruction (drone swarm)

**Vector:** Adversary physically destroys relay infrastructure.

**Defenses:**

1. **Street-level distribution**: primary infrastructure is 10M+ residential/SMB devices. Physical attack at scale is infeasible.
2. **No critical-path DCs**: foundation anchors are backup-only; swarm operates without them.
3. **Tier hierarchy isolates damage**: loss of a city-tier gateway doesn't affect neighboring cities.
4. **Replication diversity**: k=20 replicas spread across ≥ 3 countries for sensitive records.
5. **Store-and-forward at relay**: transient physical losses don't lose in-flight messages (queued at home relay).

**Residual risk:** coordinated multi-country kinetic attack within minutes could degrade SLA to Tier 2-3. System continues; some latency inflation.

### 2.7 Fiber cable / physical-layer attack

**Vector:** Adversary cuts undersea/trunk fibers (per Baltic Sea 2024 and Red Sea incidents).

**Defenses:**

1. **Intra-Europe rich terrestrial mesh**: Europe has dense overland fiber. Islands (UK, IE, IS) are affected more by cable cuts.
2. **IX peering redundancy**: gateways peer at multiple IXs across multiple countries.
3. **Satellite uplink fallback**: gateways may opt-in to Starlink/OneWeb for resilience.
4. **Long-horizon SLA degradation, not failure**: degraded Tier 2 operation continues.

**Residual risk:** UK/IE/IS isolation during Atlantic cable severance until repair.

### 2.8 CGNAT / IPv4-only network environments

**Vector:** Some users are behind CGNAT (mobile carriers, some residential ISPs) and cannot accept inbound connections. Not an "attack" but a real network constraint exploitable for DoS.

**Defenses:**

1. **IPv6-first**: where IPv6 available, bypasses CGNAT entirely.
2. **Home relay as connection broker**: CGNAT'd nodes connect outbound to their realm relay; that relay is IPv6-reachable. Return traffic via the same outbound-initiated connection.
3. **ICE + hole punching (Iroh pattern)**: opportunistic direct connections when possible.
4. **Relay-forward fallback**: if ICE fails, home relay forwards. Cost is latency, not connectivity loss.

**Residual risk:** mobile-only users reliant on relay forwarding always. Acceptable — SLA Tier 1.

### 2.9 Denial-of-service / amplification

**Vector:** Adversary floods relays with lookups, registrations, or calls.

**Defenses:**

1. **Authenticated DHT queries at sensitive records**: realm directory lookups may require a realm-signed query token.
2. **Per-prefix rate limits**: lookups and registrations rate-limited per /48 IPv6 prefix. Small attacker, bounded damage.
3. **Proof-of-work for expensive operations**: NodeId registration requires crypto puzzle (doubles as Sybil defense).
4. **QUIC connection limits**: per-peer connection cap at each relay.
5. **Priority queues**: intra-realm traffic has priority over cross-realm public traffic.
6. **Admission gates**: foundation-anchor tier may admit only relays that have completed proof-of-uptime (v2 incentive layer).

**Residual risk:** large botnet can still degrade discovery latency. Not outright service denial due to replication.

### 2.10 Data exfiltration / traffic analysis

**Vector:** Adversary observes relay traffic to determine who is calling whom.

**Defenses:**

1. **QUIC transport encryption**: TLS 1.3 on every hop; content is opaque.
2. **IPv6-direct primary path**: caller and callee communicate without relay in the loop, reducing observation vectors.
3. **Onion-routed lookups (v2)**: for privacy-sensitive lookups, Sphinx-style wrapping of DHT queries.
4. **Random traffic padding (v3)**: uniform packet sizing to defeat size-based analysis.
5. **GDPR-mandated minimization**: relays store only what is needed for routing; no long-lived traffic logs.

**Residual risk:** relay operators can observe which NodeIds connect to their relay. Addressed by v2 onion-routed lookups for lookups; in-flight traffic is IPv6-direct, not observable by middleboxes.

### 2.11 GDPR subject-access-request (SAR) complexity

**Vector:** User requests erasure of their records; distributed nature makes compliance complex.

**Defenses:**

1. **Short record TTLs**: all node records expire within 2 h unless republished. Withdrawal stops republication; records age out.
2. **Tombstone records**: explicit signed deletion spreads through DHT replication.
3. **Realm-controlled records**: realm admin is GDPR controller for their realm's records; clear responsibility chain.
4. **Documented DPA**: realms use standardized Data Processing Agreement with their members and with Macula foundation.
5. **Foundation is controller** for Europe-tier metadata (realm directory); has its own SAR procedure.

**Residual risk:** withdrawal propagation takes up to 24 h for full expiry. Technically unavoidable in eventually-consistent systems; legally acceptable with "reasonable promptness" standard.

### 2.12 Compromised foundation key (catastrophic)

**Vector:** Foundation's private key (signing bootstrap seed list, blockchain anchors) is compromised.

**Defenses:**

1. **Threshold signatures**: foundation key held via m-of-n threshold scheme (e.g., 5-of-9 FROST).
2. **Hardware security modules**: signing only within HSMs held in geographically distributed sites.
3. **Key rotation**: annual rotation with signed revocation of old key.
4. **Realm keys independent**: compromise of foundation does not compromise individual realms.
5. **Blockchain anchor as tamper-evident log**: unauthorized changes visible on public chain.
6. **Recovery plan**: emergency foundation key rotation procedure, documented and rehearsed.

**Residual risk:** temporary trust shock; realms may lose confidence in bootstrap tier. Realm-level sovereignty means each realm still operates on its own key.

### 2.13 Supply-chain attack on BEAM / dependency

**Vector:** Malicious code injected into an upstream dependency (quicer, partisan, etc.) ships to all Macula relays.

**Defenses:**

1. **Dependency pinning**: exact hashes pinned in rebar.lock.
2. **Reproducible builds**: Nerves-based firmware images built reproducibly and signed.
3. **Signed releases**: Macula releases signed by foundation key (see 2.12).
4. **Staged rollout**: updates canary-deployed on foundation anchors first, then opt-in release, then mainline after 1-2 weeks.
5. **Run-time integrity checks**: BEAM module fingerprint validation at startup.

**Residual risk:** zero-day in a dependency could still affect all relays briefly.

---

## 3. Residual risk matrix

| Threat | Pre-mitigation impact | Post-mitigation impact | Acceptable for v1? |
|--------|----------------------|------------------------|---------------------|
| Sybil flood | Total DHT poisoning | Expensive, visible, bounded | ✅ Yes |
| Eclipse attack | Full target compromise | Requires state-actor resources | ✅ Yes |
| Byzantine relay | Record forging | Cannot forge, can only refuse | ✅ Yes |
| BGP hijack | Total traffic interception | Brief window, self-healing | ✅ Yes |
| DNS poisoning | Bootstrap compromise | Multiple independent fallbacks | ✅ Yes |
| Drone swarm | Total destruction (DC) | Graceful degradation (street) | ✅ Yes |
| Fiber cut | Regional isolation | Terrestrial + satellite alternatives | ✅ Yes |
| CGNAT environments | Half of users unreachable | Relay-forward fallback | ✅ Yes |
| DoS flood | Service denial | Bounded per-prefix; replication survives | ✅ Yes |
| Traffic analysis | Full social graph visible | Opaque content; metadata only | ⚠️ Yes for v1; onion-route in v2 |
| GDPR SAR | Legal exposure | Short TTLs + tombstones + realm-controller model | ✅ Yes |
| Foundation key compromise | Bootstrap trust collapse | Threshold signatures + blockchain anchor + realm independence | ✅ Yes |
| Supply-chain attack | All relays compromised | Pinned + reproducible + signed + staged | ⚠️ Conditionally yes |
| Civil-war / kinetic attack | Infrastructure destroyed | Swarm-native, no critical-path targets | ✅ Yes (Tier 2-3 survival) |
| Nation-state EW / EMP | Regional blackout | Tier 4 DTN fallback | ❌ Out of scope v1 |

---

## 4. SLA tiers under attack conditions

| Tier | Conditions | Expected SLA |
|------|-----------|--------------|
| **S — Peacetime** | Normal operation | <50 ms p99 cross-EU, >99.99% success |
| **1 — Partial attack** | Single country degraded, DoS underway | <150 ms p95, >99% success |
| **2 — Multi-region** | 2+ countries under simultaneous attack, bootstrap-anchor tier partially seized | <500 ms p95, >95% success, best-effort ordering |
| **3 — Severe** | Only tier-D social/blockchain bootstrap works, most anchors down | Eventual delivery, minutes-to-hours, signed receipts |
| **4 — Blackout** | Internet unavailable; satellite/radio/DTN only | Days-scale delivery via store-and-forward |

Tier-1 and Tier-2 are within v1 scope. Tier-3 capability planned in Phase 7. Tier-4 is v3 territory (DTN integration).

---

## 5. Open threat-model questions

1. **Quantum-computing threat timeline**: Ed25519 is quantum-vulnerable. Migration to PQ signatures (e.g., Dilithium) is on NIST track but not urgent. When does Macula need PQ-ready signing? Leaning: design hybrid classical+PQ by v2.

2. **Data-residency enforcement**: regulations may require "DE realm records must never touch non-DE infrastructure." Current tier-local replication is best-effort; strict enforcement requires geographic gating. v1: best-effort + realm-config. v2: strict mode.

3. **Lawful-intercept compliance**: if compelled by EU law (GDPR Article 23 derogation), how does a realm admin cooperate technically without breaking Macula security? Needs legal + engineering joint design. Out of scope for v1 technical plan but document as a pending policy item.

4. **Counter-adversarial speech**: if a realm is used for content deemed illegal by EU law (e.g., CSAM), how is it taken down? Depends on realm governance model. Realms are sovereign but not above law. Foundation stance: never host takedown infrastructure, but publish clear realm-operator responsibilities.

5. **Reputation system**: do we track relay reputation globally? Prone to abuse. Leaning: track locally per relay (failure rates in routing decisions); no global reputation scoreboard.

---

## 6. Incident response

### 6.1 Detection

- Foundation operates a monitoring collective measuring DHT health, lookup success rates, unusual traffic patterns
- Relay operators report anomalies via a signed `incident_report` record type (v2)
- Automated alerts on: Sybil growth spike, replication factor drop, unusual record churn

### 6.2 Response playbook (to be authored in Phase 7)

- Triage: determine threat class, scope, affected realms
- Communication: signed incident advisory published to bootstrap cascade (DNS + social + chain)
- Mitigation: difficulty increases, admission gating, operator communication
- Recovery: post-incident state audit, public post-mortem (when safe)

### 6.3 Coordinated vulnerability disclosure

- Security contact: to be established by foundation
- Disclosure timeline: 90 days standard, shorter for actively-exploited
- Bug bounty program: post-v1

---

## 7. References

- OWASP Threat Modeling: https://owasp.org/www-community/Threat_Modeling
- MITRE ATT&CK for Networks: https://attack.mitre.org/
- Douceur. **The Sybil Attack.** IPTPS 2002. https://www.freehaven.net/anonbib/cache/sybil.pdf
- Castro et al. **Secure routing for structured peer-to-peer overlay networks.** OSDI 2002. https://dl.acm.org/doi/10.1145/844128.844156
- RFC 9171 (Bundle Protocol v7): https://datatracker.ietf.org/doc/rfc9171/
- NIST PQC standardization: https://csrc.nist.gov/projects/post-quantum-cryptography
- GDPR Art. 23 (lawful intercept derogation): https://eur-lex.europa.eu/eli/reg/2016/679/oj
- RPKI: https://www.rfc-editor.org/rfc/rfc6480

---

*Threat model authored alongside `PLAN_MACULA_ROUTING_V2.md` on 2026-04-13. Baseline for v1 implementation; expected to be refined during Phase 1 and Phase 7 hardening.*
