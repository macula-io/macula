# PLAN — Macula V2, Part 2: Topology

**Parent:** `PLAN_MACULA_V2_ROOT.md`
**Depends on:** Part 1 (Foundations) — identity, scale targets, hardware floor, IPv6 assumption.
**Status:** Draft — authored 2026-04-14.
**Scope:** The geographic and addressing model the mesh lives in. Five-tier station hierarchy, NodeId semantics, IPv6-native address model, IPv4 fallback, diversity axes. No routing algorithm yet (that is Part 3), no lifecycle (Part 4), no wire formats (Part 6).

---

## 1. Purpose of this Part

Part 1 declared a sovereign IPv6-native Europe-bounded mesh of millions of street-level stations. Part 2 makes that concrete: *where* do those stations sit, *how* are they addressed, and *what geographic structure* does the mesh exploit for routing, replication, and resilience?

Topology is the skeleton. If Part 2 is wrong, Part 3's S/Kademlia routing sits on unstable ground — diversity-constrained replica placement, tier-diverse buckets, source-routed relay-forward all depend on the topology model defined here.

Part 2 answers four questions:

1. **How are stations stratified?** — the 5-tier hierarchy (§2, §3).
2. **What does a NodeId say about where a station is?** — nothing by design (§4); geography is derived from addresses at runtime.
3. **How is a station reachable?** — IPv6 primary, IPv4 fallback, multi-homing (§5).
4. **What axes drive diversity?** — ASN, country, tier, prefix, device class (§6).

---

## 2. Tier model — the concept

Macula stations are not homogeneous. A residential RPi on consumer fiber and a datacenter-hosted EPYC with BGP peering have 1000× difference in capacity, reachability, and operational cost. Pretending they are equal — as V1 did — produces routing decisions that ignore capacity and trust floors that ignore exposure.

V2 introduces **five tiers** ordered from street to sky. Each tier has:

- A **capacity profile** (bandwidth, CPU, RAM, storage).
- A **reachability profile** (residential NAT? static IPv6? BGP? RPKI-valid?).
- A **role set** (which protocol obligations this tier carries).
- A **trust handle** (who endorses / monitors this tier).

Critically: **tier is self-declared**, not assigned. A station announces its tier in its signed `node_record`. Peers verify the declaration against observable evidence (bandwidth under load, reachability, BGP presence) but cannot forcibly promote or demote. Self-declaration prevents central control; observability prevents bluffing.

Default tier for a fresh Hecate Station out of the box is T0. Higher tiers require opt-in.

---

## 3. The five tiers

```
 sky                                        rarest, most trusted
  ▲
  │   T4  Foundation anchor        ~10–50 globally           Bootstrap trust root (optional)
  │   T3  Continental gateway      ~100–500 EU-wide          Cross-country backbone
  │   T2  Country gateway          ~1 000–5 000 per EU       National aggregation
  │   T1  Metro/ISP gateway        ~10 000–50 000 per EU     City / ISP aggregation
  │   T0  Street station           millions                  Default; residential / SMB
  ▼
 street                                   most numerous, least trusted (individually)
```

### 3.1 T0 — Street station (the default)

**What it is.** A residential or small-business device running Hecate Station firmware on consumer-grade hardware, connected to consumer ISP fiber (or equivalent). The unit that makes the mesh "street-level".

| Dimension | Range |
|-----------|-------|
| Example hardware | RPi 4B/5, Celeron J4105, Minisforum mini-PC, Framework Mainboard |
| RAM | 2–32 GB |
| Storage | 16 GB–2 TB |
| Uplink | 100 Mbps–1 Gbps residential, typically symmetric fiber (EU context) |
| IPv6 | /64 or /56 from ISP (residential typical); /48 for SMB |
| BGP | No — consumer broadband, no peering |
| Churn | Moderate — ISP router reboot, power blip; intra-day availability >98% typical |
| Endorsement | Self-generated StationId + realm endorsement by realms the operator has joined |
| Trust weight | Individually: low. Collectively (k=20 replicas across diverse T0s): strong. |

**Role obligations.**
- Serve realms the operator has chosen (store `realm_directory` replicas, accept member connections).
- Participate in S/Kademlia DHT (store records, answer lookups).
- Route CALLs for which it is on the source-computed path (opt-out possible per realm).
- Run SWIM-Lifeguard within its tier bucket.

**Not obligated.**
- Any cross-tier bridging.
- Any non-realm serving.
- Any bandwidth commitment beyond best-effort.

**Population target.** Millions (V2 Year 5 saturation). T0 is where the "indestructible because many and cheap" narrative lives.

### 3.2 T1 — Metro / ISP gateway

**What it is.** A station with elevated capacity that aggregates within a city or ISP prefix. Typically a prosumer mini-rack, SMB server, or cooperatively-operated small rack. Runs `hecate_station` + opt-in `gateway_capability` record with `tier=1`.

| Dimension | Range |
|-----------|-------|
| Example hardware | Mini-rack SFF Xeon-D, EPYC mini-server, UnRaid NAS-class |
| RAM | 16–64 GB |
| Storage | 1–4 TB NVMe |
| Uplink | 1–10 Gbps symmetric; business ISP, may have SLA |
| IPv6 | /48 static; may have /56 sub-delegations |
| BGP | Optional — some T1s peer at a local IX (AMS-IX, DE-CIX access points) |
| Churn | Low — hours–weeks between incidents |
| Endorsement | Realm-endorsed; MAY carry foundation T1-capability attestation (optional, informational) |
| Trust weight | Moderate. Observed capacity + uptime + membership in a stable realm. |

**Additional role obligations (on top of T0).**
- Bucket-diverse replica custody — Part 3 §4 places more replicas per T1 than per T0, because T1 uptime is higher.
- Realm-local gossip seed — T1s serve as stable peers for HyParView/Plumtree within their realm.
- Cross-realm relay-forward within the same country (opt-in).
- Bootstrap refresh — publish a monthly-refreshed list of reachable T0 peers in the same metro/ISP prefix.

**Not obligated.**
- Cross-country routing (that is T2).
- Serving as bootstrap anchor (that is T3+).

**Population target.** Tens of thousands Europe-wide at saturation.

### 3.3 T2 — Country gateway

**What it is.** A national aggregation station. Typically operated by a regional cooperative, a municipality, a small ISP, or an established realm operator. One per country is a floor; dense countries have dozens.

| Dimension | Range |
|-----------|-------|
| Example hardware | Rack-mount 1U/2U Xeon/EPYC, ECC RAM |
| RAM | 32–128 GB ECC |
| Storage | 4–16 TB NVMe RAID |
| Uplink | 10 Gbps symmetric; colocation at a national IX (BNIX, NL-IX, DE-CIX, SIX-SK, etc.) |
| IPv6 | /48 or /44; PI space common |
| BGP | Yes — at least one upstream peering, preferably two. RPKI-signed announcements required. |
| Churn | Very low — months between incidents |
| Endorsement | Realm-endorsed + foundation T2-capability attestation (informational; bootstrap preference only) |
| Trust weight | High. Verifiable BGP + RPKI + physical presence claim + multi-realm endorsement. |

**Additional role obligations (on top of T1).**
- Cross-metro / intra-country routing — if the source-computed path requires crossing metros within DE, a T2 typically carries the middle hop.
- **Country-prefix bucket anchor** in tier-diverse routing tables — at least one T2 per country is expected to be reachable.
- **Serve as stable T0/T1 bootstrap peer** for fresh stations in its country (bootstrap cascade Tier B; Part 5).
- Publish a **country-topology snapshot** (count of T0/T1 by ISP prefix) for foundation monitoring.

**Not obligated.**
- Continental routing (that is T3).
- Operating a DNS PKARR service (T4).

**Population target.** Thousands Europe-wide at saturation (~10–100 per country, depending on country size).

### 3.4 T3 — Continental gateway

**What it is.** A Europe-scale backbone node. Typically at a major IX (AMS-IX, DE-CIX Frankfurt, LINX, FR-IX) with multi-homed upstream and 10 Gbps+ peering. Operated by the foundation, a large coop, or a well-funded realm.

| Dimension | Range |
|-----------|-------|
| Example hardware | Enterprise server, redundant PSU, ECC, remote management |
| RAM | 128 GB–1 TB |
| Storage | 16–64 TB NVMe |
| Uplink | 10–100 Gbps, multi-homed, BGP with multiple upstreams |
| IPv6 | /32 or /29 PI (LIR status); anycast space permitted |
| BGP | Mandatory — multi-homed, RPKI-valid, anycast-capable for DNS PKARR serving |
| Churn | Effectively zero — year-scale MTBF |
| Endorsement | Foundation T3 attestation + at least two large-realm endorsements |
| Trust weight | Very high. Public identity, audited operation, physical access control claims. |

**Additional role obligations (on top of T2).**
- **Cross-country Europe backbone** — source-computed paths spanning distant countries (PT ↔ FI, IE ↔ GR) typically include one T3 hop.
- **Anycast DNS PKARR** — T3s serve the `_pkarr.{anycast-label}` lookup for bootstrap cascade Tier A.
- **Mainline DHT bridging** — T3s run reciprocal Mainline DHT nodes that make Macula PKARR records discoverable from BitTorrent-DHT-speaking clients.
- **Foundation monitoring data sink** — publish observability streams (DHT health snapshots, SWIM anomalies) to foundation-signed topics.

**Not obligated.**
- Serving as ultimate trust root (T4 only).
- Carrying foundation threshold key shards (T4 only).

**Population target.** Low hundreds Europe-wide at saturation.

### 3.5 T4 — Foundation anchor

**What it is.** The optional trust root. Operated by the foundation (BEAM Campus / future Macula Foundation). Carries threshold-key shards for signing seed lists, trust-list updates, realm-admission attestations.

| Dimension | Range |
|-----------|-------|
| Hardware | Hardened enterprise server; HSM-integrated key management |
| RAM | 64 GB+ ECC |
| Storage | 2–8 TB NVMe with encrypted at-rest |
| Uplink | 10 Gbps, multi-homed, BGP+RPKI, DDoS-mitigation upstream (Cloudflare Magic Transit / Path.net class) |
| IPv6 | PI /32, anycast |
| BGP | Multi-homed mandatory, RPKI mandatory |
| Churn | Zero tolerated — year-scale MTBF; planned maintenance only |
| Endorsement | Foundation threshold key (m-of-n FROST); member of foundation trust list by default |
| Trust weight | Maximum (within the "trust the foundation" model). **Still optional** — a realm can operate fully without trusting any T4. |

**Role obligations (on top of T3).**
- Hold a shard of the foundation threshold key.
- Sign the **monthly foundation seed list** (a rolling snapshot of recommended bootstrap stations).
- Sign the **foundation trust list** of realms (purely informational).
- Publish **foundation-signed DHT health reports** (anomaly alerts).
- Serve the **first-tier bootstrap cascade** (Tier A in Part 5 §3.1).

**Hard limits.**
- Foundation holds *no* authority over realm-internal state.
- Foundation *cannot* unilaterally revoke a realm — only remove it from trust list.
- Foundation operation must remain *optional* — if all T4s are compromised simultaneously, the network still operates via T3/T2/DoH-PKARR/Mainline DHT / blockchain anchor / social bootstrap tiers.

**Population target.** 10–50 globally. Diverse hosters, diverse jurisdictions (but all Europe), diverse operational teams. No single operator runs >3 T4s.

---

## 4. NodeId semantics

Part 1 §4.1 declared: NodeId = Ed25519 public key, 32 bytes, no geographic bits. Part 2 makes that rule operational.

### 4.1 What NodeId encodes

**Nothing about location.** NodeId is the 32-byte Ed25519 public key, grinded until `leading_zeros(SHA-256(pubkey)) ≥ difficulty` (Part 1 §4.4). Every bit is load-bearing cryptographically; no bits are load-bearing semantically.

This is deliberate. Embedding country / tier / ISP in the NodeId would:

- Leak geographic metadata unprotected on the wire.
- Break after migration (station moves country; NodeId would be stale).
- Allow trivial Sybil bucketing (adversary grinds NodeIds that land in a specific tier).
- Violate the PKARR compatibility contract (PKARR keys are pubkeys, nothing else).

### 4.2 What NodeId *is used for*

| Use | How |
|-----|-----|
| Routing table bucket derivation | XOR distance to lookup key (Kademlia). Pure math, no geography. |
| DHT lookup addressing | `lookup(key)` targets NodeIds closest by XOR to `key`. |
| Signature verification | Record signatures verify against NodeId pubkey. |
| Connection handshake | QUIC TLS 1.3 peer identity binds to NodeId. |
| Revocation | A NodeId-signed tombstone invalidates records owned by that NodeId. |

### 4.3 How geography is derived

Geographic / tier / capacity state is **derived at runtime** from:

1. **Station's IPv6 address** (primary source). The /32 upstream prefix maps to an AS via the RIR bulk-WHOIS / RIPEstat snapshot. AS maps to country via geoip (MaxMind GeoLite2 / open-source equivalent). Prefix length + RIR class maps to a rough tier floor (LIR /32 ⇒ at least T2-capable).

2. **Station's signed `node_record`** (authoritative source). The record contains:
   - Declared country ISO-3166 code.
   - Declared tier (T0–T4).
   - Declared ASN (must match observed AS from IPv6).
   - Declared hostname/label (for human readability only).
   - Optional: declared metro (OpenStreetMap place-id hint).

3. **Observed behaviour** (corroborating). Ping RTT distribution, bandwidth under probe, uptime over 30 days. Cross-checks declaration.

The three sources form a consistency triangle. If a station's IPv6 AS says "ISP Proximus BE" but the signed record claims "tier=3 in IE", peers flag the inconsistency and downgrade the station's trust weight.

### 4.4 Regenerating NodeId vs re-homing

Moving a station to a new country / ISP / datacenter does **not** require regenerating NodeId. The NodeId is identity; address is address. The station publishes a new `node_record` with updated IPv6, AS, country, declared tier (if changed). Peers observe, update routing tables, continue.

NodeId regeneration is triggered only by: key compromise, permanent decommission of the operator, or realm-scope change that demands identity rotation. Regeneration = new identity = re-endorsement by realms.

---

## 5. Address model

### 5.1 IPv6-first — the canonical form

A station's canonical address is **`[IPv6]:7000`** (QUIC port 7000 by default; configurable).

- IPv6 is GUA (Global Unicast). ULA is not used (no mesh-scope aggregation planned).
- Link-local is used only for first-hop discovery on the same L2 (mDNS bootstrap tier B; Part 5).
- A station publishes **multiple IPv6 addresses** in its `node_record` if it is multi-homed:
  - Primary upstream IPv6.
  - Secondary upstream IPv6 (if dual-homed).
  - Tunnelbroker IPv6 (Hurricane Electric) if used as IPv6 fallback over IPv4 transit.
- Each listed address is tagged with a preference (`primary`, `secondary`, `fallback`).
- Peers attempt connections in preference order, parallelised under happy-eyeballs-style bounded concurrency.

### 5.2 Why GUA /64 — and not addresses per station

Residential fiber hands out /56 (typical) or /48 (SMB). The station uses one /64 within that delegation. The specific `::` suffix is free — some operators prefer `::1` for humans, some randomise to resist scanning, some rotate for privacy. Macula takes no position; the `node_record` carries whatever the operator picked.

Consequence: **two stations behind the same residential connection share a /56 but have distinct /64s.** That is the diversity floor — Part 3's replica placement treats them as the same *operator* (AS-level diversity), different *station* (address-level).

### 5.3 IPv4 — fallback paths only

A station's IPv4 address, if any, is recorded as **fallback transit only**. Specifically:

- **IPv4 serving:** Stations may accept QUIC on IPv4 for clients whose nodes are IPv4-only. This is a reachability concession, not a core capability.
- **IPv4 station-to-station:** Not used when both endpoints have IPv6. A pair of IPv4-only stations relays through an IPv6-capable intermediate.
- **IPv4-only nodes (client/daemon):** Supported. Node connects outbound over IPv4 to the station's IPv4 address; station forwards into the IPv6 mesh.
- **CGNAT:** Stations operating behind CGNAT get a flag `cgnat=true` in their node_record; they are treated as degraded-T0 (serve local nodes only, do not participate in relay-forward). Open question O5 Phase 4 — may tighten to "not a station at all, just a node".

### 5.4 Multi-homing

Multi-homing is a first-class feature. A station that has simultaneous residential fiber + LTE backup + Starlink fallback lists all three:

```
node_record.addresses = [
  { v6: "2a02:a03f:...", asn: 47377, kind: "primary",   pref: 100 },
  { v6: "2a02:578b:...", asn: 6848,  kind: "secondary", pref: 50  },
  { v6: "2606:4700:...", asn: 14593, kind: "fallback",  pref: 10  }
]
```

Peers track all three. Failure of one upstream triggers Pillar 5 (cascade refresh) and switches preference; the station itself does not regenerate identity.

Part 4 §? details the liveness state machine per-address.

### 5.5 Tunnelbroker (IPv6 over IPv4 transit)

Operators without native IPv6 use Hurricane Electric or Go6Lab tunnels. The station probes tunnel reachability on every boot and includes the tunnelled /64 as a fallback address. Open question O5 weighs whether tunnel-only stations can be T0 (leaning: yes, with reduced tier-diversity weight).

---

## 6. Diversity axes

Routing and replication decisions in V2 are **diversity-constrained**. A naive k=20 replication where all 20 replicas sit in the same AS is no better than k=1 against a BGP hijack of that AS. Part 2 defines the axes; Part 3 §4 uses them.

### 6.1 The six axes

| Axis | Source | Why it matters |
|------|--------|----------------|
| **Station** | NodeId | Same NodeId never counted twice. |
| **Operator** | Realm endorsement OR ASN-owner WHOIS OR self-declared operator label | One operator controlling many stations is a single point of failure. |
| **ASN** | IPv6 prefix → RIPEstat snapshot | One BGP-hijacked AS can drop all its prefixes. |
| **Country** | ASN → country (primary) OR IPv6 geoip fallback | One state-actor routing disruption affects one country. |
| **IPv6 prefix** | /32 for LIR, /48 for SMB, /56 for residential | Diversity within an ISP requires distinct allocations. |
| **Tier** | Declared in node_record + observed | Replica spread across T0 / T1 / T2 prevents capacity-class cascading failures. |

### 6.2 Diversity constraints — routing table

Each S/Kademlia bucket holds up to k=20 peers. The bucket's composition is constrained:

- **At least 5 distinct ASNs** per bucket (if population allows).
- **At least 3 distinct countries** per bucket (if bucket spans >1 country by XOR distance — rare in low buckets, common in high buckets).
- **At least 2 tiers** represented per bucket for buckets ≥ 8 entries (enforces T0/T1 mix).

Insertion algorithm: new peer fills an empty slot iff its insertion improves diversity metrics (or at worst maintains them). Displacement of existing entries is allowed when a new peer carries higher observed uptime AND improves diversity simultaneously.

### 6.3 Diversity constraints — replica placement

k=20 replicas for each DHT record. Constraints:

- **All 20 distinct stations** (trivially).
- **≥8 distinct ASNs.**
- **≥5 distinct countries.**
- **≥3 tiers represented** (T0 majority; at least 1 T1 and 1 T2).
- **Operator cap:** no operator owns >3 of the 20 replicas.

If the diversity requirement cannot be met (small realm, few T1/T2 peers reachable), the record is placed with fewer replicas and flagged `diversity_degraded=true` in its metadata. Realms observing degraded diversity MAY surface warnings to operators.

### 6.4 Diversity observability

Every station computes diversity metrics continuously over its routing table + stored replicas. The numbers are surfaced:

- In `/status` local admin endpoint (Part 7).
- In foundation monitoring telemetry (opt-in; Part 5).
- In the `_macula.health.diversity_report` topic (intra-realm).

A realm with falling diversity metrics (e.g. ASN count dropping because an ISP exits the market) surfaces a nudge: "You're underrepresented in FR; consider onboarding a station at ISP X."

---

## 7. Geographic inference infrastructure

§4.3 and §6 depend on **reliable ASN → country and prefix → ASN mappings**. Part 2 specifies the infrastructure.

### 7.1 Data sources

| Source | Used for | Refresh |
|--------|----------|---------|
| **RIPE bulk WHOIS** (primary for EU AS space) | ASN ownership, AS name, country-of-registration | Weekly |
| **RIPEstat / RIS snapshots** | BGP-visible prefix-to-AS mapping | Daily |
| **MaxMind GeoLite2 / IPinfo Lite** | IPv6 → country geoip (fallback when AS is multi-country) | Monthly |
| **RPKI repositories** (RIPE, AFRINIC, ARIN, APNIC, LACNIC) | ROA-signed prefix-to-AS attestations | Daily |
| **IANA IPv6 global unicast registry** | Sanity: is this prefix even globally routable? | Quarterly |

### 7.2 Where the mappings live

- **Every T2/T3 station** holds a full compressed snapshot (~5 MB compressed for EU).
- **T1 stations** hold a country-scoped snapshot (~500 KB).
- **T0 stations** query the nearest T1 on demand via `_macula.geoip.lookup`, cached for 24h.

The snapshots are themselves published as signed records (T3-signed, refreshed monthly; realm-level T2s may also publish country-scoped variants).

### 7.3 Graceful fallback under mapping unavailability

If all mapping sources are simultaneously unreachable, station falls back to:
1. Last known cached snapshot (may be weeks stale; acceptable).
2. Declared country in peer's signed record (no cross-check; lower trust).
3. Refuse to compute diversity until mapping recovers (replica placement stalls rather than placing blindly).

---

## 8. Tier declaration and gateway opt-in

### 8.1 Declaring a tier

A station declares tier N by including `tier: N` in its signed `node_record`. For N > 0, additional `gateway_capability` record SHOULD be published (signed by the same StationId) with fields:

```
gateway_capability:
  tier: 2
  bw_sustained_mbps: 5000
  bw_burst_mbps: 10000
  uptime_30d: 99.97
  asn: 12345
  asn_owner: "Foo Telecom B.V."
  rpki_valid: true
  multi_homed: true
  country: "BE"
  metro: "brussels"
  contact: "ops@example.be"
  policy_url: "https://..."
  endorsements:
    - realm: <RealmId_1>
      signature: ...
    - realm: <RealmId_2>
      signature: ...
  foundation_attestation: ...  # optional, T2+ only, informational
  signature: ...  # StationId signs the whole record
```

### 8.2 Verifying a declared tier

Peers verify a declaration by:

1. **Signature** — StationId signed the record; record hasn't been tampered.
2. **ASN match** — declared ASN matches observed ASN from IPv6 prefix.
3. **RPKI** — if declared `rpki_valid`, resolver checks live ROA.
4. **Bandwidth probe** — over 30 days, peers observe sustained and burst bandwidth; deviation >50% from declared ⇒ observed-tier downgrade.
5. **Uptime** — SWIM-Lifeguard observation (Part 4) over 30 days; deviation >20% ⇒ observed-tier downgrade.
6. **Endorsements** — at least one realm endorsement for T1+; two for T2; three + foundation for T3.
7. **Foundation attestation** — informational only; absence does not disqualify.

Observed tier can be *lower* than declared. It cannot be *higher* — a station cannot accidentally be a T3 without declaring.

### 8.3 Gateway obligations under opt-in

Declaring T1+ is opt-in. Consequences:

- Station agrees to carry the additional obligations of that tier (§3.2–§3.5).
- Station agrees to publish observability data (bandwidth, uptime) for the `foundation-monitor` topic.
- Station agrees to honour `relay-forward` requests from same-realm peers (rate-limited).
- Station does NOT agree to serve arbitrary cross-realm traffic unless also opted into public-transit role (Phase 5+).

Withdrawal: station publishes a new `gateway_capability` with `tier: 0` and a tombstone for the prior record. 24h grace period before peers stop expecting T1+ service.

---

## 9. Station identity ↔ realm endorsement ↔ topology

A station declaring tier T2 in the signed record is a claim. Multiple realms co-signing it is corroborating evidence. Foundation attestation is third-party corroboration.

The interaction matrix:

| What the station has | Observable tier |
|----------------------|-----------------|
| Self-declaration only | Declared tier × 0.5 trust multiplier |
| + 1 realm endorsement | Declared tier × 0.7 |
| + 2+ realm endorsements | Declared tier × 0.9 |
| + foundation attestation | Declared tier × 1.0 |

Trust multiplier governs how eagerly peers place replicas on this station, not whether they connect at all. A freshly declared T3 with no endorsements is simply treated as T0.5 for replica placement and routing weight — no penalty, no veto.

---

## 10. Migration and mobility

### 10.1 Changing country / ISP

Operator moves station from BE to FR.

1. Station retains StationId.
2. Station publishes a new `node_record` with updated IPv6, ASN, country.
3. Peers observe change via DHT record refresh (TTL 1h or sooner on Pillar 5 cascade refresh).
4. Routing tables update — bucket membership may shift (different XOR-distance peers become closer).
5. Replica responsibilities reshuffle — records the station was storing for neighbours in BE are offloaded; records from neighbours in FR are accepted.
6. Realm endorsement remains valid (realm-level, not country-level) unless realm explicitly geo-gates membership (rare; Part 5).

### 10.2 Changing tier (upgrading T0 → T1)

Operator upgrades hardware and uplink.

1. Station publishes new `gateway_capability` with `tier: 1`.
2. Peers observe declaration; begin treating station as T1 candidate.
3. Over next 30 days, SWIM-Lifeguard + bandwidth probes corroborate.
4. Trust multiplier climbs from 0.5 → 0.9 as realm endorsements arrive.
5. Station is visible in tier-1 slots of neighbours' routing tables.

Downgrade is symmetrical but fast: station publishes `tier: 0` and a 24h grace-period tombstone.

### 10.3 Mobile / changing IPv6

Changing IPv6 *with same ISP* is routine — CPE reboot, DHCPv6 renew, IPv6 prefix rotation by ISP. Station detects address change and re-publishes `node_record` within 30s. Pillar 5 cascade refresh carries the update.

Changing IPv6 *with same /48* is similar; routing table reshuffling is minimal because AS is unchanged.

Changing AS (ISP swap) is a rarer event; see §10.1.

### 10.4 Out-of-scope: genuinely mobile stations

V2.0 explicitly excludes stations that change IPv6 live during operation (roaming mobile networks, in-vehicle stations, satellite-only mobile). These produce too much routing churn for the current model. Mobile *nodes* (phones, laptops) are fine — they connect outbound to a fixed station. Mobile *stations* revisited in V2.1+.

---

## 11. Example: three stations in three tiers

Illustrative to verify the model hangs together.

**Station A — Tienen, BE, T0.**
- Hardware: RPi 5, 8 GB, 1 TB NVMe.
- ISP: Proximus. Residential /56 IPv6.
- Declared tier: 0. No `gateway_capability`.
- Endorsements: 2 personal realms.
- Observable: ping 1–2 ms within same metro, uptime 99.5%, bandwidth 500 Mbps sustained.
- Role: serves its two realms; participates in k=20 replicas for records XOR-close to its NodeId; routes only for its own realm's CALLs.

**Station B — Brussels, BE, T1.**
- Hardware: Xeon-D mini-rack, 32 GB ECC, 4 TB.
- ISP: Edpnet business. Static /48 IPv6. BGP optional, not peered.
- Declared tier: 1. `gateway_capability{tier=1, bw=1000 Mbps, country=BE}`.
- Endorsements: 5 realms including 2 large BE realms; no foundation attestation.
- Observable: 99.9% uptime over 90 days, sustained 900 Mbps.
- Role: country-metro aggregator; holds replicas for its realms and adjacent realms' records; serves as stable HyParView peer for realm-local gossip; carries relay-forward for BE metropolitan paths.

**Station C — Amsterdam, NL, T3.**
- Hardware: dual-Xeon 2U, 256 GB ECC, 32 TB, HSM-backed keystore.
- Hoster: colo at NIKHEF; multi-homed BGP; AMS-IX + NL-IX peering; RPKI-valid /32.
- Declared tier: 3. `gateway_capability{tier=3, bw=40000 Mbps, country=NL, metro=amsterdam, multi_homed=true, rpki_valid=true}`.
- Endorsements: 40+ realms across EU; foundation T3 attestation present.
- Observable: 99.99% uptime; 35 Gbps sustained.
- Role: continental backbone; anycast DNS PKARR serving; foundation monitoring data sink; source-computed paths crossing PT ↔ PL typically traverse it; reciprocates into Mainline DHT.

Same protocol on all three. Same NodeId format. Vastly different roles, observable differences, trust weights.

---

## 12. Relationship to Part 3 (Discovery & Routing)

Part 3 consumes Part 2 as follows:

- **Routing table buckets** are populated per §6.2 diversity constraints, using tier + ASN + country from node_records.
- **S/Kademlia lookups** use the routing table; disjoint paths (d=3) exploit tier + ASN diversity to resist eclipse.
- **Replica placement** uses §6.3 constraints, prioritising T1+ for stability and T0 for span.
- **Source routing** (k=3 paths) is computed over a graph whose edges are weighted by observed latency + tier capacity. Paths are required to be tier-increasing-then-decreasing (a PT T0 → PT T2 → T3 → NL T2 → NL T0 path; no pure T0-only cross-country paths).
- **Intra-realm mesh** (HyParView + Plumtree) ignores tier — realm peers are peers; the abstraction sits above the tier layer.

Part 3 extensively references `Part 2 §X.Y`; the references are correct once Part 2 is frozen.

---

## 13. Open questions specific to topology

Open questions from ROOT §10 that land primarily in Part 2:

- **O4** — Exact jurisdiction list (EU + EEA + UK + CH + ?). Decision Phase 7.
- **O5** — CGNAT operators — T0 or not-a-station? Decision Phase 4.
- **O6** — mDNS bootstrap announce default on/off. Decision Phase 6.

Additional Part 2 open questions:

- **O11 (new)** — Should there be a "T-1" tier for *nodes acting as proxy stations* (a node-on-laptop holding a /128 from a station's /64)? Current: no; nodes ≠ stations. Revisit if user-research shows friction.
- **O12 (new)** — Tunnel-only (Hurricane Electric only) stations: full T0 or degraded T0? Current: full T0, with reduced tier-diversity weight in replica placement. Revisit Phase 4 after chaos testing.

---

## 14. Success criteria for Part 2

Part 2 is complete when a reader can:

1. Name the **5 tiers** and give one example hardware profile for each (§3).
2. Explain why NodeId carries **no geographic bits** (§4.1–§4.2).
3. Describe the **three sources** of geographic derivation and how inconsistencies are handled (§4.3).
4. Predict how diversity constraints apply to a **concrete replica placement** problem (§6.3).
5. Describe the flow when an operator **moves a station between countries** (§10.1).
6. Explain the **trust multiplier** interaction between self-declaration, realm endorsement, and foundation attestation (§9).
7. State exactly what V2.0 **excludes** from topology-supported cases (§10.4, §5.3 CGNAT).

If any of the above is ambiguous, Part 2 needs revision before Parts 3 + 4 build on it.

---

*Part 2 deliberately leaves routing, replica algorithm, wire records, and lifecycle to later Parts. The skeleton is now in place.*
