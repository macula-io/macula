# PLAN — Macula V2, Part 5: Bootstrap & Governance

**Parent:** `PLAN_MACULA_V2_ROOT.md`
**Depends on:** Part 1 (trust model, foundation), Part 2 (tier hierarchy, T3/T4 roles), Part 3 (DHT, crypto-puzzle), Part 4 (lifecycle).
**Feeds:** Part 7 (Implementation), Part 8 (Verification).
**Status:** Draft — authored 2026-04-14.
**Scope:** How a cold-boot station finds the mesh, how new stations join realms, how gateway roles are admitted, how the foundation signs and rotates parameters, and how Sybil/eclipse attempts are resisted during bootstrap. No wire formats (Part 6), no per-Phase schedule (Part 7).

---

## 1. Purpose of this Part

A station with no prior contacts must reliably find its way into the mesh. The naive approach — hard-code one anchor — collapses if the anchor is seized, DDoSed, or BGP-hijacked. V2's bootstrap is **a multi-tier cascade**: five independent discovery paths, any one of which suffices to find honest peers.

Governance is the twin problem. Who signs the parameters that tell a station the current crypto-puzzle difficulty? Who authorises a new realm? Who promotes a station to T3 gateway status? Governance answers must avoid two failure modes: centralisation (one actor can freeze the network) and chaos (no authority, adversary wins by impersonation).

Part 5 answers:

1. **How does a cold-boot station find honest peers, resiliently?** — §3–§8 (five-tier cascade).
2. **How does a station declare itself a gateway?** — §9 (gateway admission).
3. **How does a new realm come into existence?** — §10 (realm admission).
4. **How are protocol parameters (crypto-puzzle difficulty, tRepublish interval, foundation seed list) signed and rotated?** — §12.
5. **How does the foundation behave such that its failure does not collapse the network?** — §11, §13.

---

## 2. The bootstrap problem

A fresh station has:
- Its own StationId (generated on first boot).
- IPv6 connectivity (Part 2 §8 required).
- A filesystem that may or may not contain cached peer records.
- Operator-configured realm memberships (out-of-band).

It lacks:
- Any peer NodeIds.
- Any signed trust lists.
- Any sense of "is the network healthy?".

Bootstrap solves this — without requiring a fixed central authority, without requiring prior presence, and without collapsing if any single discovery mechanism is compromised.

**Design rule:** any single tier of the cascade must suffice to reach the mesh. Failure of N-1 tiers degrades recovery speed, not possibility.

---

## 3. The five-tier bootstrap cascade

```
┌───────────────────────────────────────────────────────────────┐
│ Tier A — Foundation anchors                                   │
│   Fast, centralised, trusts foundation keys                   │
│   DoH → PKARR lookup of well-known NodeIds                    │
│   Anycast-reachable T4s + a small set of T3s                  │
└───────────────────────────────────────────────────────────────┘
        │ fallback if A unreachable
        ▼
┌───────────────────────────────────────────────────────────────┐
│ Tier B — Local network discovery                              │
│   mDNS/DNS-SD on link-local IPv6                              │
│   Finds peers on same LAN or same home network                │
└───────────────────────────────────────────────────────────────┘
        │ fallback if B empty
        ▼
┌───────────────────────────────────────────────────────────────┐
│ Tier C — Mainline DHT bridge                                  │
│   PKARR-compatible records reachable from BitTorrent DHT      │
│   Resolves well-known identity keys via public infrastructure │
└───────────────────────────────────────────────────────────────┘
        │ fallback if C unreachable
        ▼
┌───────────────────────────────────────────────────────────────┐
│ Tier D — Blockchain anchor (optional, slow)                   │
│   Signed seed record on Bitcoin/Ethereum L1 (OP_RETURN)       │
│   Immutable, jurisdiction-resistant, high-latency             │
└───────────────────────────────────────────────────────────────┘
        │ fallback if D unreachable
        ▼
┌───────────────────────────────────────────────────────────────┐
│ Tier E — Social / out-of-band                                 │
│   Operator pastes a peer QR code, signed URL, business card   │
│   Manual but unreseizable                                     │
└───────────────────────────────────────────────────────────────┘
```

Cascade invariants:
- Tiers probed in order A → B → C → D → E with parallelism (start B at 200 ms of A not responding; start C at 500 ms; etc.).
- First tier that yields ≥3 verifiable peers satisfies bootstrap.
- Every verified peer feeds the routing table immediately; subsequent discovery walks the DHT (Part 3) from there.
- Bootstrap target: <60 s to "first successful DHT lookup" under Tier S; <5 min under Tier 3.

---

## 4. Tier A — Foundation anchors

### 4.1 What Tier A is

Foundation-operated T4 stations (Part 2 §3.5) + a publicly-listed set of T3s. Their NodeIds and addresses are published as **signed PKARR records** reachable via:

- **DNS-over-HTTPS (DoH)** at well-known names: `_pkarr.<NodeId-base32>.macula.io`, `_pkarr.<NodeId-base32>.macula.eu`, plus a foundation-rotating list.
- **Anycast IPv6 addresses** from a foundation-assigned /32. Reaching any anycast address reaches the nearest T4.

Fresh station bootstraps by:

```
1. Ship with 5 hard-coded foundation-signed NodeIds (embedded in the station firmware).
2. Resolve PKARR for each via DoH (Cloudflare, Quad9, Google, Mullvad — rotate).
3. Each resolved record is Ed25519-signed by the NodeId whose key it belongs to; verify.
4. Connect QUIC to verified addresses; initial routing-table populated from the responses.
```

### 4.2 Cost of compromise

If foundation threshold-key is compromised:
- Adversary signs a malicious seed list.
- Station following Tier A only gets adversary peers.
- **But:** signed records carry `valid_from` + `valid_until`. Old records held in station's cache survive compromise.
- Tiers B/C/D/E are independent; station still reaches honest peers via fallback.

If DoH resolvers collude or are DDoSed:
- Station falls through to Tier B within 500 ms.
- Anycast IPv6 is a third path that is independent of DoH.

### 4.3 DoH resolver selection

Station queries ≥3 resolvers in parallel with different operators (Cloudflare, Quad9, Mullvad, NextDNS, …). Any two corroborating results suffice. Single-resolver answer treated as low-confidence; triggers Tier B/C in parallel.

### 4.4 Anycast

Foundation operates `2001:db8:macula:a::/48` (or equivalent real allocation) as anycast. Every T4 announces this prefix via BGP from its home AS. Station reaching the anycast address reaches a nearby T4.

BGP-hijack resistance: every T4 announcement is RPKI-signed (Part 2 §3.5). Hijacked announcements by unauthorised parties are filtered by RPKI-respecting upstreams. Hijack by an RPKI-compromising adversary is state-actor scale; cascade-fallback catches it.

---

## 5. Tier B — Local network discovery

### 5.1 mDNS / DNS-SD on link-local IPv6

Every station publishes on `ff02::fb` (IPv6 mDNS multicast) a record:

```
_macula._udp.local
  → TXT { "node_id": <hex>, "port": 7000, "tier": 0 }
  → AAAA <link-local or GUA address>
```

Fresh station sends mDNS query for `_macula._udp.local` on its local segment. Responses corroborate each other; at least one signed `node_record` over QUIC confirms the peer is legitimate.

### 5.2 When Tier B wins

- Home network with a prior-installed station (the operator's existing node).
- Same LAN as other stations (coop space, cluster).
- Fallback when no internet upstream is available but intranet is.

### 5.3 Privacy considerations

mDNS announces presence to every device on the LAN. Operator can disable mDNS announce (receive-only mode) via config. Default: announce on. Open question O6 — should default flip to off?

Rationale for default-on: the LAN is already a trust boundary; the operator implicitly trusts devices there enough to install a station. Receive-only is the exception for paranoid deployments.

### 5.4 Beyond link-local

Multi-LAN SMB environments can configure a `.local.`-resolving unicast resolver that stations query. Protocol supports this; administrative configuration required.

---

## 6. Tier C — Mainline DHT bridge

### 6.1 What it is

PKARR records are DNS-packet-compatible; Mainline DHT (BitTorrent's 20M-node Kademlia) can hold them at a public key. Foundation-signed seed records, plus any realm-public bootstrap records, are published to Mainline DHT.

Fresh station uses a small embedded Mainline DHT client:

```
1. Query Mainline DHT for well-known public keys (embedded in firmware).
2. Retrieve the signed record.
3. Verify signature against embedded pubkey.
4. Extract Macula endpoint(s); connect via QUIC.
```

### 6.2 Why it works

Mainline DHT is:
- **Decentralised** — no single operator to seize.
- **Large** — 20M+ nodes; eclipse is expensive.
- **Operationally stable for 20+ years.**
- **Free** — no per-lookup cost.

Mainline DHT is not without risks — public-key records are rate-limited, and its Kademlia is plain (no crypto-puzzle). V2 treats it as a **bootstrap signal only**, not a trust anchor. Any peer discovered via Mainline DHT is then verified against cached foundation signatures.

### 6.3 Mainline DHT bridging stations

T3+ stations (Part 2 §3.4) run reciprocal Mainline DHT bridge nodes. They serve Macula PKARR records into Mainline and relay Mainline lookups into Macula's own DHT for Macula-originated queries. Bridging is opt-in per station.

### 6.4 Rate and abuse concerns

Mainline DHT has per-source rate limits. Bootstrap from a single station occurs at most once per restart (cached result). At fleet scale of 1M stations cold-booting, Mainline DHT sees <1 QPS — trivial.

Adversary publishing malicious PKARR records at the same public keys is blocked by signature verification; attacker cannot sign records for a key they don't hold.

---

## 7. Tier D — Blockchain anchor

### 7.1 What it is

Foundation writes a quarterly-refreshed **signed seed record** onto Bitcoin or Ethereum L1 via `OP_RETURN` (Bitcoin) or a contract event (Ethereum). The record:

- Ed25519 signature over `(timestamp, seed_list, valid_until, difficulty_floor)`.
- Seed list = 20 T4/T3 NodeIds.
- Cost per quarterly write: ~50 EUR Bitcoin fee, ~10 EUR Ethereum fee.

Fresh station bootstraps:

```
1. Fetch most recent OP_RETURN tagged with foundation marker (via public block explorers, multiple independent).
2. Parse & verify signature against foundation's multi-sig key (m-of-n FROST).
3. Extract seed list; resolve each seed's endpoint via Tier A/C as a follow-up.
```

### 7.2 Why it works

Blockchain is:
- **Immutable** — seizure-resistant.
- **Multi-jurisdiction** — miner/validator set is globally distributed.
- **Verifiable** — any station can independently fetch and verify.

Latency: minutes (block confirmation) for writes. Reads are fast (archive nodes).

### 7.3 When Tier D wins

Pathological scenarios where Tiers A/B/C are all blocked:
- State-actor DNS blockade including DoH.
- Mainline DHT under coordinated eclipse (expensive, not impossible).
- Local network has no other stations and internet-DNS is filtered.

Station falls through; fetches blockchain seed list; bootstraps from there.

### 7.4 Which chain

Open question O2 — Bitcoin, Ethereum, both?

Preferences:
- **Bitcoin** wins on cost-of-disruption and censorship-resistance.
- **Ethereum** wins on cost-per-write and programmability.
- **Both** is redundancy; modest annual cost (~240 EUR/year writing quarterly to each).

Leaning: both. Decision Phase 6.

### 7.5 Cost of compromise

If foundation's blockchain-writing key is compromised, adversary publishes malicious seeds. Mitigation:
- m-of-n FROST threshold (e.g. 3-of-5 foundation custodians).
- Hard-coded `valid_until` prevents indefinite forgery.
- Multiple chains require compromising both.

---

## 8. Tier E — Social / out-of-band

### 8.1 What it is

Manual peer exchange. Operator receives a peer's identity out-of-band:
- QR code printed on a sticker or presented in an app.
- Signed URL shared via email / chat.
- Keybase / Matrix / Nostr post with peer endpoint.
- Verbal exchange, transcribed.

Station imports the signed peer record via CLI:

```
hecate bootstrap add-peer <signed-url-or-QR-contents>
```

Signature verification ensures the peer is who they claim. Subsequent DHT lookups walk from there.

### 8.2 When Tier E wins

The network is fully partitioned (cataclysm, internet blackout); operators exchange identities physically. Tier E is the floor — it works even in air-gapped scenarios if two devices can exchange bits once.

### 8.3 Tier E is a feature, not a workaround

Tier E normalises operator sovereignty. An operator can refuse all other tiers and configure their station to bootstrap *only* from named peers — a deliberate cliquish configuration for high-trust realms.

---

## 9. Gateway role admission

Opt-in promotion T0 → T1/T2/T3 is specified in Part 2 §8. Part 5 elaborates **the admission side**: what peers accept.

### 9.1 Self-declaration

Station publishes signed `gateway_capability` (§8.1 Part 2). Record contains declared tier, bandwidth claims, ASN, RPKI validity, endorsements, contact.

### 9.2 Peer verification

Peers observe declarations and corroborate:

- **Bandwidth probe:** over 30 days, peers exchange test traffic at declared peak and sustained rates. ≥10% shortfall triggers observed-tier downgrade.
- **Uptime probe:** SWIM-Lifeguard (Part 4 §5.2) observes uptime. ≥5% shortfall from declared triggers downgrade.
- **ASN / RPKI:** checked against IPv6 prefix WHOIS + RPKI repositories.
- **Endorsement count:** T1 requires 1, T2 requires 2, T3 requires 3 + foundation attestation.

A station consistently failing corroboration is downgraded in *observed tier* — declared tier remains but trust-multiplier weight (Part 2 §9) drops.

### 9.3 Foundation attestation for T3

Foundation issues T3 attestations based on:
- Physical audit of colocation + BGP peering claims.
- RPKI compliance.
- Multi-homing evidence.
- Realm endorsement from ≥3 large realms.
- Operator identity verification (KYC-lite; foundation policy).

Attestation is signed by the foundation threshold key with `valid_until` 1 year out. Annual re-audit.

### 9.4 Foundation does NOT gate tier

A station without foundation attestation *can still declare T3* — just without the foundation's recommendation. Peers may weight it lower by default but are free to treat it as T3 if their realm trusts it. Foundation attestation is **a recommendation**, not an authorisation.

### 9.5 Withdrawal

Station publishes a new `gateway_capability` with `tier: N-k` + tombstone over the old record. 24 h grace period before peers stop expecting the higher-tier service.

---

## 10. Realm admission

### 10.1 Realm creation

A new realm is created when an operator publishes:

```
realm_directory:
  realm_id: <new Ed25519 pubkey>
  admin_key: <same pubkey, or separate governance key>
  name: "<human-readable label>"
  policy_url: "<governance docs>"
  member_limit: <optional>
  federation_policy: {...}
  created_at: <timestamp>
  signature: <signed by realm admin key>
```

No registration, no central approval. Realm *exists* once its record is published to the DHT and accepted by custodians.

### 10.2 Member admission

Members join by receiving signed endorsements from realm admin:

```
realm_member_endorsement:
  realm: <RealmId>
  member_node: <NodeId of joining node>
  roles: [...]
  valid_from: <timestamp>
  valid_until: <timestamp>
  signature: <by realm admin key>
```

Admission flows (see `PLAN_MNS_AND_REALM_JOIN` — separate plan, rides on V2):

- OAuth-like flow: node requests, admin approves via UI.
- Invitation code: admin pre-signs an endorsement-template; member redeems.
- Self-serve with social-proof: admin's policy auto-endorses upon satisfying condition (e.g. email domain).

### 10.3 Federation

Two realms federate by cross-signing:

```
cross_realm_trust:
  realm_a: <RealmId_A>
  realm_b: <RealmId_B>
  scope: [directory_lookup, member_auth, ...]  # what's trusted
  valid_until: <timestamp>
  signature_a: <signed by A admin>
  signature_b: <signed by B admin>
```

Federation is scoped — A may trust B for directory lookups but not member authentication. No transitive federation by default (A-B and B-C does not imply A-C).

### 10.4 Dissolution

Admin publishes `realm_dissolved` signed record (tombstone for realm_directory). Members' endorsements become void. Custodians reap realm records at `2 × tExpire`.

Alternatively: abandoned realm (admin vanishes). Admin key rotation protocol (m-of-n council realms, or emergency key-escrow) handles this; single-admin realms with lost keys die.

### 10.5 Realm revocation by foundation

Foundation CAN remove a realm from its **trust list** — causes bootstrap to deprioritise the realm. Foundation CANNOT unilaterally destroy the realm; its record lives in the DHT until the admin tombstones or until custodians lose all replicas.

Foundation trust-list revocation is reserved for realms hosting content explicitly illegal under EU law (CSAM, terrorism Regulation (EU) 2021/784). Foundation policy document governs procedure; legal review required.

---

## 11. Sybil / eclipse hardening at bootstrap

Bootstrap is the highest-value attack window: a new station has no prior state to fall back on.

### 11.1 Attack surface

- **Forged foundation seed list.** Adversary presents a seed list signed by a fake key.
- **DoH hijack.** Adversary returns wrong PKARR records.
- **mDNS spoofing.** LAN-local adversary responds first with fake station record.
- **Mainline DHT poisoning.** Adversary publishes malicious records at foundation keys.
- **Embedded firmware tamper.** Adversary modifies shipped firmware to trust their keys.

### 11.2 Defences

| Attack | Defence |
|--------|---------|
| Forged foundation seed list | Foundation threshold key (m-of-n FROST); embedded pubkey in firmware; signature verification mandatory. |
| DoH hijack | Use ≥3 independent DoH providers; require ≥2 corroborate; fall through to Tier B/C. |
| mDNS spoofing | Still must present signed `node_record` over QUIC; spoofer needs valid signature, which they don't have. |
| Mainline DHT poisoning | PKARR records are Ed25519-signed; adversary can't forge. |
| Firmware tamper | Signed firmware (reproducible builds + foundation signature); TPM-verified boot where available. |

### 11.3 First-boot vs re-boot

Re-boot uses cached peer list first. Cache poisoned ⇒ cascade fallback. Cached peers are still re-verified on reconnect (Part 4 §7).

First-boot (empty cache) is most vulnerable. Mitigation: shipped firmware includes 5 foundation pubkeys; one compromised, four survive. Cascade adds tiers B/C/D/E.

### 11.4 Bootstrap observation

Stations report bootstrap metrics: which tier succeeded, how long it took, how many peers verified. Foundation monitoring aggregates. Sudden drop in Tier A success across fleet ⇒ investigation.

---

## 12. Parameter signing and rotation

### 12.1 Parameters the foundation signs

- **Crypto-puzzle difficulty floor** (Part 3 §12.3).
- **tReplicate / tRepublish / tExpire** defaults.
- **Bootstrap seed list** (20 T4/T3 NodeIds; rotates monthly).
- **Trust list of realms** (informational).
- **Foundation attestation list** (T3 gateway attestations).

### 12.2 Signature scheme

m-of-n FROST threshold Ed25519. V2.0: 5 custodians, 3-of-5 threshold. Custodians distributed across:
- 5 different jurisdictions (BE, NL, DE, FR, EE).
- 5 different organisations.
- 2 cloud-HSM + 3 physical-HSM variety.

### 12.3 Record versioning

Each signed parameter record carries:
- `version :: uuid_v7` (time-ordered).
- `signed_at :: timestamp`.
- `valid_until :: timestamp`.
- Prior version's hash (chain integrity).

Stations accept a new version iff `version > current` AND `valid_until > now` AND `signed_at > current.signed_at`.

### 12.4 Emergency rotation

Custodian key compromise ⇒ threshold rotation ceremony. Stations observe rotated-signer record signed by surviving quorum; cached old-key records remain valid until `valid_until`.

Total foundation compromise (impossible under threshold, but model it): every V1 sign-able is revokable by publishing `valid_until: past`. Stations fall back to cascade Tier B/C/D/E. Long-term recovery: foundation re-bootstrap from Tier E (operator consensus).

### 12.5 Non-signed protocol parameters

Parameters NOT signed by foundation (each station decides locally or each realm configures):
- Bucket size k (20 standard; station MAY configure).
- Siblings s (16).
- Disjoint paths d (3).
- HyParView active-view size (realm-configurable).
- Plumtree fanout (realm-configurable).

Local/realm parameters can drift without coordination; compatibility with protocol versioning ensures interop.

---

## 13. Governance model

Three-layer governance:

```
┌──────────────────────────────────────────────┐
│ Foundation — protocol-scale governance       │
│   - Protocol version negotiations            │
│   - Parameter signing (m-of-n FROST)         │
│   - Bootstrap trust list                     │
│   - Illegality response (trust-list curation)│
│   - NOT: realm-internal matters              │
└──────────────────────────────────────────────┘
                ▲ optional trust
                │
                │
┌──────────────────────────────────────────────┐
│ Realm — sovereignty-scale governance         │
│   - Admin key (single / multisig / council)  │
│   - Member admission policy                  │
│   - Federation decisions                     │
│   - Content moderation within realm          │
│   - Revocation of own members                │
└──────────────────────────────────────────────┘
                ▲ membership
                │
                │
┌──────────────────────────────────────────────┐
│ Station — local governance                   │
│   - Which realms to serve                    │
│   - Whether to be a gateway                  │
│   - Which SLA tier to target                 │
│   - Local config (ports, logs, storage)      │
└──────────────────────────────────────────────┘
```

### 13.1 Foundation scope

Foundation acts within a **strictly enumerated scope**. Actions outside scope require protocol-version-update (a breaking change requiring realms to opt in).

In scope:
- Signing bootstrap seed lists.
- Signing parameter updates (difficulty, TTLs).
- Publishing (not enforcing) trust lists.
- Operating monitoring infrastructure (opt-in observations).

Out of scope:
- Admission of individual realms (realms self-publish).
- Admission of individual stations (stations self-publish).
- Routing decisions (no central routing).
- Content arbitration (deferred to realm).
- Pricing, metering, accounting.

### 13.2 Foundation charter

A written charter published alongside this plan enumerates scope, decision procedures, custodian rotation, conflict resolution. Charter is amendable by 4-of-5 FROST + public comment period. V2.0 ships with draft charter; Phase 7 hardens.

### 13.3 Realm governance variety

Realm admins may be:
- **Single key** (one-person realm; personal).
- **Multisig** (small council; SMB / cooperative).
- **FROST threshold** (medium realm with distributed admin team).
- **DAO** (on-chain governance voting; large realms; Phase 9+ integration).
- **Hybrid** (multi-mode per-action; e.g. admission = single, trust list = council).

Protocol takes no position on governance choice. Realm's published policy URL documents for members.

### 13.4 Conflict resolution

Disputes within a realm ⇒ realm admin adjudicates.
Disputes between realms ⇒ cross-realm cross-sign defines limits; nothing else.
Disputes involving foundation ⇒ charter process.
Disputes with law-enforcement jurisdictional demands ⇒ realm admin (as GDPR controller) responds.

---

## 14. Protocol versioning and evolution

### 14.1 Version field

Every Macula wire frame carries a protocol version byte. V2.0 = 0x02. Future changes:

- **Compatible extension** (new optional fields, new error codes, etc.): same version byte; peers that don't understand ignore.
- **Backward-compatible change**: same version byte; behaviour gated by capability bits in `node_record`.
- **Incompatible change**: bumps version byte (0x03 for next generation).

V2 lifetime expectation: 3–5 years before V3 discussion (if any).

### 14.2 Negotiation

On HANDSHAKING (Part 4 §10), peers exchange protocol versions + capability bits. Highest common version wins. Mismatch on major version ⇒ connection refused with `protocol_version_unsupported`.

### 14.3 Deprecation

Foundation announces deprecation of a version ≥ 6 months in advance. Stations are encouraged to update. Deprecated versions continue to function technically but may lose Tier A bootstrap preference.

---

## 15. Bootstrap failure modes — worked examples

### 15.1 Happy path

Fresh install. Tier A available. First DoH query returns foundation record in 150 ms; QUIC connects to T4 in 50 ms more; routing table populated from T4's FIND_NODE response within 400 ms. Bootstrap complete in ~600 ms.

### 15.2 Tier A partial failure

DoH resolver 1 (Cloudflare) returns wrong record (hijack attempt). Resolver 2 (Quad9) returns correct. Resolver 3 (Mullvad) returns correct. 2/3 corroborate ⇒ trusted. Slight latency hit (worst-resolver wait) but bootstrap succeeds in <2 s.

### 15.3 Tier A complete failure, Tier B lucky hit

DoH unreachable (network incident). Station starts Tier B in parallel at 200 ms. mDNS on LAN returns a cached neighbour station. Signed QUIC handshake confirms. Bootstrap complete in ~800 ms.

### 15.4 Tiers A/B fail, Tier C succeeds

No DoH. No local stations. Mainline DHT query for foundation pubkey returns signed record at 3 s. Bootstrap in ~3.5 s.

### 15.5 Tiers A/B/C fail, Tier D succeeds

Internet blockade blocks all above. Station's operator has configured blockchain-anchor fetch via a satellite modem or neighbouring operator's relay. Quarterly seed list retrieved in 10 s. Bootstrap in ~12 s.

### 15.6 All automated tiers fail, Tier E

Full isolation. Operator scans a QR code from a neighbour operator's device. Bootstrap in manual time.

---

## 16. Open questions specific to Part 5

- **O2** — Blockchain anchor chain: Bitcoin, Ethereum, both. Decision Phase 6.
- **O3** — Foundation custodian set — how many, which organisations. Decision Phase 6.
- **O6** — mDNS default announce on/off. Decision Phase 6.
- **O22 (new)** — Bootstrap observation — should stations *require* attestation of which tier succeeded (foundation monitoring), or keep opt-in? Privacy tension.
- **O23 (new)** — Firmware signing — whose signature (foundation? a package-maintainer coop?). Reproducible builds required.
- **O24 (new)** — Emergency key rotation rehearsal cadence — foundation custodians drill how often?

---

## 17. Success criteria for Part 5

Part 5 is complete when a reader can:

1. Describe the **five-tier cascade** and what each tier defends against (§3–§8).
2. Trace a **cold-boot sequence** under Tier A success (§15.1) and under Tier A + B + C failure (§15.4).
3. Explain how **foundation compromise does not collapse the network** (§4.2, §11, §12.4).
4. State the **three-layer governance model** and name three things the foundation *cannot* do (§13.1).
5. Walk through **new realm creation** (§10.1) and **new realm admission-to-member** (§10.2).
6. Describe how a **T3 gateway attestation** is obtained and why foundation attestation is a *recommendation not an authorisation* (§9.3–§9.4).
7. Explain the **parameter signing scheme** (m-of-n FROST) and emergency rotation (§12).
8. Identify which bootstrap tier would be **decisive in an adversarial scenario** of the reader's choice (§11, §15).

If any is ambiguous, Part 5 revises before Parts 6–8 build on top.

---

*Part 5 closes governance + bootstrap. Remaining: Part 6 (wire protocol catalog), Part 7 (implementation phase schedule), Part 8 (verification), Part 9 (appendices / references / open questions index).*
