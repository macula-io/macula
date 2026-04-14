# PLAN — Macula V2, Part 3: Discovery & Routing

**Parent:** `PLAN_MACULA_V2_ROOT.md`
**Depends on:** Part 1 (identity, scale), Part 2 (tiers, addresses, diversity axes), Part 4 (lifecycle invariants, fast-fail taxonomy, SWIM).
**Feeds:** Part 5 (Bootstrap), Part 6 (Wire), Part 7 (Implementation).
**Status:** Draft — authored 2026-04-14.
**Scope:** How records are found, how replicas are placed, how paths are computed, how realms gossip internally. S/Kademlia DHT with tier-diverse buckets. Diversity-constrained replica placement (k=20, ≥8 ASN, ≥5 country, ≥3 tier). Source routing with k=3 disjoint paths (Suurballe). Intra-realm HyParView + Plumtree. No wire formats (Part 6), no bootstrap anchors (Part 5), no governance (Part 5).

---

## 1. Purpose of this Part

Part 3 is the heart of the mesh. Everything before it defines *what* is routable (identities, records, realms); everything after it uses those routes to deliver value.

Four questions anchor Part 3:

1. **How does a station find another station, given a key, in a mesh of millions?** — S/Kademlia lookup, §4.
2. **Where does a record live, and how many copies exist, such that a BGP hijack or regional outage cannot erase it?** — Diversity-constrained replication, §5.
3. **How does a CALL travel from origin to target when they are in different realms and tiers?** — Source routing with k=3 disjoint paths, §6.
4. **How do members of the same realm exchange updates without routing every message through a DHT lookup?** — HyParView + Plumtree intra-realm overlay, §7.

The answers must hold at V2 Year 5 scale (1M+ stations, 100M+ nodes, 100k+ realms) and under adversarial conditions (Sybil, eclipse, BGP hijack, fiber cut). Every decision in Part 3 passes the 7 pillars (Part 4) as gates.

Part 3 is prescriptive at the algorithmic level, but defers concrete wire byte-layouts to Part 6.

---

## 2. Overview of the routing stack

```
┌──────────────────────────────────────────────────────────────┐
│ Application layer (Hecate apps, realm gossip)                │
│   - PubSub topics, CALLs, realm directory events             │
└──────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌──────────────────────────────────────────────────────────────┐
│ Intra-realm overlay (§7)                                     │
│   - HyParView partial view                                   │
│   - Plumtree push-lazy gossip                                │
│   - OR-Set CRDT convergence                                  │
└──────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌──────────────────────────────────────────────────────────────┐
│ Global discovery (§3–§5)                                     │
│   - S/Kademlia DHT with tier-diverse buckets (k=20, α=3, d=3)│
│   - PKARR-compatible record format                           │
│   - Diversity-constrained replica placement                  │
│   - Crypto-puzzle Sybil defense                              │
└──────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌──────────────────────────────────────────────────────────────┐
│ Global routing (§6)                                          │
│   - Source-computed paths (Suurballe k=3 disjoint)           │
│   - 44-byte source-routing header                            │
│   - Tier-diverse edge weighting                              │
│   - BOLT#4 failure taxonomy (from Part 4)                    │
└──────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌──────────────────────────────────────────────────────────────┐
│ Lifecycle & Transport (Part 4, Part 6)                       │
│   - QUIC, TLS 1.3, Ed25519 peer binding                      │
│   - SWIM-Lifeguard, 7 pillars                                │
└──────────────────────────────────────────────────────────────┘
```

Three layers collaborate:

- **Global discovery**: where is X? Answer via DHT.
- **Global routing**: how do I get a packet to X? Answer via source-computed path.
- **Intra-realm overlay**: how do I gossip cheaply to all my realm peers? Answer via Plumtree.

They are separable. A CALL may traverse only intra-realm (cheapest), or intra-realm + DHT lookup + source route (most complex). The router picks the minimum-sufficient path.

---

## 3. The XOR metric and NodeId space

### 3.1 NodeId distance

Reuses Kademlia's metric exactly:

```
distance(A, B) = A XOR B
```

where `A` and `B` are 256-bit NodeIds (Ed25519 pubkeys, Part 1 §4.1). The XOR metric:

- Is symmetric: `d(A,B) = d(B,A)`.
- Is unidirectional: for any key `K`, there is exactly one "closest" node modulo ties.
- Divides the space into a prefix-based tree that maps naturally onto routing-table buckets.

### 3.2 Bucket indexing

A station's routing table contains up to 256 buckets (one per bit of NodeId length). Bucket `i` holds peers whose NodeId shares the first `i` bits with the station's own NodeId but differs in bit `i`.

In practice, populated buckets number ≈ log₂(N) where N is the fleet size. At V2 Year 5 saturation (1M stations), ~20 buckets hold >10 peers each; the deepest buckets near the station's own NodeId hold 0–1 peers (rare close collisions).

### 3.3 Lookup key types

Any 256-bit value is a lookup key. V2 uses these key families:

| Key | Derivation | Purpose |
|-----|-----------|---------|
| `NodeId` | the 32 bytes directly | Station/node discovery |
| `RealmId` | the 32 bytes directly | Realm directory lookup |
| `ProcedureKey` | `SHA-256(procedure_uri)` | Procedure discovery (§5.3) |
| `TopicKey` | `SHA-256(topic_uri)` | PubSub subscription discovery (§5.3) |
| `RealmStationsKey` | `SHA-256("station_set" ‖ RealmId)` | Which stations serve this realm |
| `GatewayKey` | `SHA-256("gateway" ‖ tier ‖ country)` | Find gateway stations for a tier/country |

Key derivation is stable and documented. Every client computes keys identically; no coordination required.

---

## 4. S/Kademlia: the DHT

Macula V2 implements **S/Kademlia** (Baumgart & Mies, ICPADS 2007). Plain Kademlia is insufficient against Sybil/eclipse; S/Kademlia adds three defences that Part 3 adopts wholesale.

### 4.1 The three S/Kademlia extensions

1. **Crypto-puzzle NodeIds.** Part 1 §4.4 — NodeId pubkey must satisfy `leading_zeros(SHA-256(pubkey)) ≥ difficulty`. Prevents a single adversary from cheaply minting many NodeIds in a target bucket. V2 default difficulty = 16 leading zero bits (~1 CPU-second).
2. **Sibling lists.** Each node maintains an extra `s` siblings (closest-by-XOR peers to self) beyond the normal routing table. V2 uses **s=16**. Siblings give tighter failure detection around self and faster rebalancing after local churn.
3. **Disjoint-path lookups.** A lookup uses `d` parallel, path-disjoint lookup branches. V2 uses **d=3**. Eclipse attack succeeds only if the adversary controls all `d` paths simultaneously.

### 4.2 Parameters table

| Parameter | Symbol | V2 value | Rationale |
|-----------|--------|----------|-----------|
| Bucket size | k | 20 | Classic Kademlia. Enough redundancy for residential churn. |
| Lookup concurrency | α | 3 | Standard. Balances bandwidth against latency. |
| Disjoint paths | d | 3 | S/Kademlia. Raises eclipse cost cubically. |
| Siblings | s | 16 | S/Kademlia. Fast local rebalancing. |
| Crypto-puzzle leading zeros | — | 16 bits (adaptive O1) | ~1 CPU-second mint. |
| tReplicate interval | — | 1 h | See Part 4 §11.1. |
| tRepublish interval | — | 24 h | Owner obligation. |
| tExpire TTL | — | 48 h | Safety margin over 2× tRepublish. |
| Record replica count | — | 20 | Matches k. |
| Record diversity | — | ≥8 ASN, ≥5 country, ≥3 tier (Part 2 §6.3) | Regional outage survivability. |

### 4.3 Tier-diverse buckets

Plain S/Kademlia assumes peers are interchangeable. V2 adds a diversity constraint on top — a bucket is not just k peers closest by XOR but **the best k peers closest by XOR that satisfy diversity**.

Bucket admission algorithm (on discovering peer P closer-by-XOR than slot in bucket B):

```
1. If B has fewer than k entries, admit P.
2. Else compute a composite score for P and for each current member M of B:
   score(X) = weighted sum of:
     - uptime_30d           (Pillar 3 observed)
     - observed_latency     (lower is better)
     - ASN-novelty in B     (does X bring a new ASN?)
     - country-novelty in B
     - tier-novelty in B
     - time-in-bucket       (incumbency bonus, small)
3. Find the lowest-scoring member M_min in B.
4. If score(P) > score(M_min), evict M_min, admit P. Else reject P.
```

Constraints enforced over the assembled bucket (soft; violated only if no alternative):

- ≥5 distinct ASNs per bucket of 20 (if the reachable population allows).
- ≥3 distinct countries per bucket.
- ≥2 tiers represented per bucket for buckets ≥8 entries.

Violations are **soft** — a bucket that can't satisfy the constraints (too few reachable peers in the relevant NodeId range) holds what it can and flags `diversity_degraded=true` in metrics.

### 4.4 Sibling-list composition

The 16 siblings are just the 16 closest-by-XOR peers known, irrespective of bucket boundaries. Siblings replace plain-Kademlia's "closest peers known" set. Uses:

- Fast republish target set (owner's records replicate to siblings + k-closest-to-key).
- SWIM probe target preference (siblings get probed more often than far peers).
- Realm-directory coherence (if you and your siblings are in the same realm, you can gossip via HyParView to them first).

### 4.5 FIND_NODE and FIND_VALUE

S/Kademlia lookup procedure:

```
LOOKUP(key K, target_count n, paths d=3):
  for path p in 1..d, in parallel but path-disjoint:
    shortlist[p] = α closest-to-K known by self (must not overlap with shortlists of other paths)
    loop:
      pick α not-yet-queried peers from shortlist[p]
      send FIND_NODE(K) to each
      await replies; add returned peers to shortlist[p]
      terminate when shortlist[p] contains n peers closest-to-K that have responded
  return union of top-n from all d paths, deduped
```

Path disjointness enforced at the *peer level*: path `p`'s shortlist never contains a peer that path `p'≠p` has ever queried in this lookup. The more d disjoint peers reachable at each depth, the stronger the eclipse resistance.

FIND_VALUE is identical but each hop checks its local store first; if found, returns the record and signals early termination. A caller who needs *authoritative* answer (e.g. for replica reshuffling) continues the lookup to all d paths even after first success.

### 4.6 Eclipse resistance analysis

Eclipse attack: adversary surrounds target's routing-table buckets with adversary-controlled peers so lookups return only adversarial answers.

With d=3 disjoint paths and per-bucket ASN diversity ≥5:

- To eclipse a target, adversary must control peers in **all 3 paths at every hop**.
- Each path must pass through peers in ≥5 distinct ASNs at the bucket layer.
- Crypto-puzzle limits how fast adversary can mint new NodeIds (hours per target bucket).
- Foundation monitoring (§8) detects bucket-composition anomalies.

Estimated adversary cost to eclipse one target at steady state: tens of CPU-hours + presence in 15+ ASNs simultaneously. Not zero — but lifted from "one botnet" to "nation-state".

---

## 5. Record storage and replica placement

### 5.1 Record types in the DHT

| Type | Key | TTL | Owner |
|------|-----|-----|-------|
| `node_record` | NodeId | 48 h | Station itself |
| `realm_directory` | RealmId | 48 h | Realm admin |
| `realm_stations` | `SHA-256("station_set" ‖ RealmId)` | 48 h | Realm admin |
| `procedure_advertisement` | `SHA-256(procedure_uri)` | 48 h | Node advertising it |
| `topic_subscription_hint` | `SHA-256(topic_uri)` | 48 h | Subscriber (aggregated) |
| `gateway_capability` | Derived from NodeId + tier | 48 h | Gateway station |
| `tombstone` | same key as tombstoned record | 2× TTL of what it replaces | Original owner or authorised reaper |

TTL managed per Part 4 §11 (tReplicate/tRepublish/tExpire).

### 5.2 Replica placement (k=20, diversity-constrained)

Classic Kademlia: record stored on k closest peers by XOR. V2 modifies this. Placement procedure:

```
PLACE(record R with key K, replicas=20):
  candidates = top-40 peers closest to K by XOR (from routing table + lookups)
  chosen = []
  for each candidate C in order of closest-first:
    if chosen.size == 20: break
    if placing C would violate any diversity constraint: continue
    chosen += C
  if chosen.size < 20:
    # could not satisfy all constraints; relax weakest constraint and retry
    retry with diversity_degraded=true flag
  publish R to chosen
```

Diversity constraints at k=20:

- 20 distinct stations (trivial).
- ≥8 distinct ASNs.
- ≥5 distinct countries.
- ≥3 tiers represented (T0 majority; ≥1 T1 and ≥1 T2 if possible).
- ≤3 replicas per operator (soft).

A record that couldn't meet constraints is published with `diversity_degraded=true` and the realm observers are notified via `_macula.health.replica_diversity` topic.

### 5.3 Procedure and topic advertisement

Advertising a procedure / topic is publishing a `procedure_advertisement` / `topic_subscription_hint` record.

**Procedure advertisement content:**
- Procedure URI (Part 6 canonical form).
- Advertiser NodeId + StationId serving them.
- Session token (how calling stations connect).
- Capacity hints (rate limit, max concurrency).
- Signature.

**Topic subscription hint content:**
- Topic URI.
- Subscriber NodeId + StationId.
- Subscription options (filter, QoS).
- Signature.

Lookup for a procedure:
```
LOOKUP(SHA-256(procedure_uri))
  → returns 1..20 advertisement records
  → caller picks preferred advertiser
  → source-routes CALL via §6
```

Crucially: **discovery is separate from routing**. Finding "who serves this" is a lookup; getting the CALL there is source routing. V1 conflated these; V2 separates.

### 5.4 Subscription aggregation

A raw subscription hint per subscriber explodes on popular topics. V2 aggregates: each station subscribes locally to many topics; stations publish **one aggregated hint per station per realm**, listing the topics it has subscribers for. Publishers lookup `topic_subscription_hint` and find stations (not subscribers); the station then fans out intra-realm via Plumtree (§7).

Aggregation reduces DHT entries from O(subscribers × topics) to O(stations × realms).

### 5.5 Replica refresh under churn

When a station joins that is closer-by-XOR to a key than some existing custodian, the new station proactively offers to take custody. Procedure:

```
JOIN closer-peer-of-K:
  1. Station S detects that it is now one of the k-closest to key K (via routing table).
  2. S queries the existing k-closest for the record with key K (FIND_VALUE).
  3. If K is found, S stores a local replica + informs the existing custodian that was furthest-by-XOR that S has joined as replacement.
  4. On next tReplicate, the displaced custodian drops its copy.
```

Diversity is re-checked on every join. If the join *reduces* diversity (e.g. new custodian is in the same ASN as an existing one), the join is suppressed and the existing custodian retains custody.

### 5.6 Write-path semantics

When owner publishes record R with key K:

```
1. Compute k-closest-to-K (= 20 stations) satisfying diversity constraints.
2. Send STORE(R) to all 20 in parallel.
3. Await ≥16 acks (3/4 quorum) before considering publish successful.
4. Asynchronously retry the 4 missing.
5. If <16 acks arrive within 5 s, publish is degraded; owner retries with exponential backoff.
```

Quorum at 3/4 ensures majority of custodians witness; missing minority catches up via tReplicate cycle within 1 h.

---

## 6. Source routing

### 6.1 Why source routing

Hop-by-hop routing (each router independently forwarding toward destination) fails under adversarial conditions — a single hijacked hop can drop all traffic. Source routing has the origin compute the path; every hop verifies and forwards blindly; failures are attributable at the origin.

V2 adopts source routing inspired by **Lightning Network BOLT#4 onion** — but simplified because:

- Macula is not payment-sensitive (no HTLCs; no cryptographic linkage of hops).
- Anonymity is not a Phase 1 requirement (Phase 9+ adds onion-routed query privacy).
- Latency budget is tight; we cannot afford per-hop public-key ops.

V2 source routing = **44-byte header** carried in each CALL frame, listing the sequence of station hops.

### 6.2 Header format (logical; Part 6 has byte layout)

```
SourceRouteHeader:
  version: 1 byte
  total_hops: 1 byte             (max 8)
  current_hop: 1 byte            (incremented each hop)
  deadline: 8 bytes              (unix_ms; expire CALL if past)
  path_hash: 16 bytes            (SHA-256 over hop sequence; tamper detect)
  hops[0..total_hops]:           (up to 8 × NodeId-truncated-16-bytes)
    NodeId_prefix_16: 16 bytes   (first 16 bytes of target NodeId at this hop)
```

44 bytes = 1+1+1+8+16 + 16 (first hop). Subsequent hops add 16 bytes each; total ≤ 1+1+1+8+16+(8×16) = 155 bytes for maximum-depth path. "44-byte" is the *fixed overhead* of the header.

### 6.3 Computing paths

Path computation uses **Suurballe's algorithm** for k disjoint shortest paths over the graph of stations. Graph:

- **Vertices:** stations the origin knows (routing table + siblings + recent lookups).
- **Edges:** observed QUIC latency between stations, weighted by tier capacity.
- **Weights:** `edge_weight = latency_ms + (1 / observed_bandwidth_Mbps × 1000) + tier_penalty(hop_tier)`.
- **tier_penalty:** T0→T0 edges cost more than T1→T1 (favours gateway paths for cross-country traffic); same-tier T0 direct preferred for intra-metro.

Suurballe produces k=3 edge-disjoint (and ideally vertex-disjoint) shortest paths from origin to destination. V2 **prefers** vertex-disjoint; falls back to edge-disjoint if vertex-disjoint insufficient paths exist.

Computation occurs at the origin, cached per-destination for 5 min. Re-computation on:
- Cache expiry.
- SWIM event (any hop on a cached path transitions to `suspect`/`confirmed_failed`).
- Explicit CALL failure returning `unknown_next_peer` / `temporary_relay_failure`.

### 6.4 Path-diversity constraints

When computing k=3 paths, the same diversity logic from §4.3 applies:

- Each path traverses distinct ASN sequences where possible.
- Each path traverses distinct country sequences where possible.
- No path contains >1 hop per operator.

If diversity cannot be satisfied (small network, few alternatives), paths degrade to merely edge-disjoint and `path_diversity_degraded=true` is flagged to the origin.

### 6.5 Path validity under Part 2 tier rules

Source paths are constrained to be **tier-increasing-then-decreasing** for cross-country traffic:

- PT T0 → PT T1 → PT T2 → T3 → NL T2 → NL T1 → NL T0 (valid).
- PT T0 → NL T0 (invalid unless same-ISP or direct-peer observed; rejected by path compiler).

Intra-metro same-tier paths are allowed. T0-only paths are allowed within the same metro. Cross-country T0-only paths are rejected because they would consume bandwidth of residential-grade stations unnecessarily.

### 6.6 Per-hop processing

Each hop on receiving a CALL frame with source header:

```
1. Verify path_hash hasn't been tampered (hash over remaining hops).
2. Check deadline not expired (Pillar 4: expiry_too_soon).
3. Extract self's 16-byte prefix; confirm current_hop position matches.
4. Lookup next_hop station locally; check SWIM state == alive.
5. If next_hop not alive: return BOLT#4 unknown_next_peer, signed.
6. Else: forward frame, incrementing current_hop.
```

No per-hop cryptography on the path (unlike Lightning onion). Hop verifies positional self and freshness; payload confidentiality relies on TLS 1.3 per-link encryption.

### 6.7 Onion-routing-lite for privacy

For realms requiring hop-unlinkability (Phase 9+), path can carry per-hop-encrypted layers as in Sphinx. Base V2 uses plaintext source routing; privacy is additive.

---

## 7. Intra-realm overlay

Realms form closed groups. Members update each other frequently — realm directory, shared state, chat, presence. Doing every update as a DHT lookup is wasteful. V2 overlays a **realm-scoped gossip mesh** on top of DHT discovery.

### 7.1 HyParView partial view

Each realm member runs **HyParView** (Leitão, Pereira, Rodrigues 2007) to maintain a partial view of other members:

- **Active view:** symmetric, TCP-like. Size ≈ log₂(realm_size) + c (e.g. 5 for realm of 30, 10 for realm of 1000).
- **Passive view:** candidate pool. Size 4–5× active.
- **Shuffles:** periodic view swap with active neighbours; promotes passive → active on active-peer failure.

HyParView is robust to high churn: node departures trigger fast active-view repair via passive promotion. View membership is realm-only — HyParView is not a cross-realm graph.

Parameters:
- Active view size: `max(5, ceil(log₂(realm_size)))` capped at 15.
- Passive view size: `4 × active`.
- Shuffle interval: 30 s.
- Active view repair attempts on failure: 3.

### 7.2 Plumtree push-lazy gossip

On top of HyParView, **Plumtree** disseminates realm-scoped messages:

- **Eager push:** each message sent in full to active-view peers on first hop. Tree edges emerge from eager paths.
- **Lazy push:** message IDs sent to non-tree active-view peers. Peers request the payload if they haven't seen it.
- **Tree repair:** a peer that receives a lazy-pushed ID before the full payload switches the source to an eager path.

Dissemination latency: log₂(realm_size) × hop_latency. For a 1000-node realm with 20ms avg hop, full convergence ~200ms.

Plumtree guarantees: every member receives every message in O(log N) hops under steady state; recovery is self-healing after partitions.

### 7.3 What rides on Plumtree

- Realm chat (message_published events).
- Realm membership changes (signed by realm admin).
- Realm-scoped PubSub topics.
- CRDT (OR-Set) mutations for shared collaborative state.
- Operational fact updates (station_joined, station_left — mirror of DHT, but faster within realm).

**Not on Plumtree:**
- Cross-realm CALLs (they go via DHT lookup + source routing).
- DHT STORE/FIND_* traffic (DHT-level).
- SWIM (SWIM runs per tier, cross-realm; intra-realm membership uses HyParView).

### 7.4 CRDT convergence

Realm state that is shared mutably (member list, chat threads, directory metadata) uses **OR-Set CRDT** over Plumtree. Properties:

- Concurrent adds + removes converge.
- Members may be offline and sync on reconnect.
- No coordination required for updates.
- Tombstones age out after realm-configurable TTL (default 30 days).

Complex state (nested documents) uses **delta-state CRDTs** (Almeida et al.) to avoid sending whole-state on each update.

### 7.5 Joining a realm

A node joins a realm:

```
1. Node obtains signed realm endorsement from realm admin (out-of-band or via realm_join flow).
2. Node's home station looks up `realm_directory(RealmId)` in DHT.
3. Station contacts a realm-member station from the directory; becomes HyParView active peer.
4. Station syncs realm state via Plumtree catch-up.
5. Station pushes aggregated `topic_subscription_hint` for its subscribed topics.
6. Realm directory receives a member-joined fact; realm admin observes; directory republishes with new member.
```

Join latency: <2 s for well-populated realm (directory + HyParView handshake + initial CRDT sync).

### 7.6 Leaving a realm

Graceful leave: node publishes `node_left_realm` fact (Plumtree); HyParView peers drop active view entry; realm admin removes from directory on next republish.

Abrupt leave (node disappears): HyParView peers observe active-view failure; repair via passive promotion; SWIM-layer detection parallel. Within 30 s, the departed node is removed from all active views.

### 7.7 Realm-to-realm isolation

Plumtree / HyParView scope is strictly per-realm. A station participating in N realms runs N HyParView instances, each with its own active/passive view, each disseminating messages scoped to that realm only.

Cross-realm interaction uses DHT + source routing (§4, §6), never gossip.

---

## 8. Interaction: DHT, routing, and overlay

Three layers coexist. Rules for which layer handles what:

| Need | Layer | Why |
|------|-------|-----|
| Find "who serves topic T" | DHT | Topic may be served anywhere |
| Find "who is in realm R" | DHT + overlay | DHT for initial; overlay for steady state |
| Deliver message to realm member | Overlay | Already-known members; log(N) gossip |
| Deliver CALL to node not in my realm | DHT + source routing | Cross-realm |
| Update realm member list | Overlay | Realm-local mutation |
| Resolve name of procedure | DHT | Global namespace |
| Maintain "is peer alive" | SWIM (Part 4) | Cross-realm, tier-scoped |
| Maintain "is realm peer alive" | HyParView | Realm-scoped, richer signal |

A single CALL may touch all three: overlay to discover that target is not in realm → DHT lookup to find serving station → source routing to deliver.

---

## 9. Cross-relay RPC: resolving the V1 blocker

V1's blocker: cross-relay RPC didn't work because the gateway only checked *local* handlers and never queried DHT. V2 fixes by design:

```
CALL to procedure P from node N in station A targeting procedure in realm R:
1. Station A looks up P = SHA-256(procedure_uri) in DHT.
2. DHT returns up to 20 procedure_advertisement records.
3. Station A filters by realm R, tier preference, latency.
4. Station A picks target T (best match).
5. Station A computes k=3 source-routed paths to T's station (Suurballe).
6. Station A sends CALL with source-route header via path[0].
7. Each hop forwards blindly (verifies header, checks next-hop SWIM).
8. Target station receives CALL; dispatches to local handler (verified alive via Pillar 3).
9. Response returns via reverse source-route path.
10. If path[0] fails mid-flight, caller retries with path[1] (different disjoint path).
```

No gateway "local handlers only" shortcut. Every cross-realm CALL traverses the full discovery + routing stack. Local-handlers fast path is preserved only for *same-realm same-station* optimisation.

---

## 10. DHT operations catalog

The five operations a DHT speaker implements. Wire shapes in Part 6.

### 10.1 PING

- Request: `PING(nonce)`.
- Response: `PONG(nonce, signed)`.
- Purpose: verify a peer is alive at the DHT layer (separate from SWIM, used for routing-table refresh).
- Use: routing-table maintenance, lookup terminal check.

### 10.2 FIND_NODE

- Request: `FIND_NODE(key K)`.
- Response: `NODES(up-to-k stations closest-to-K known by responder, signed)`.
- Purpose: peer discovery, lookup recursion.

### 10.3 FIND_VALUE

- Request: `FIND_VALUE(key K)`.
- Response: `VALUE(record_list)` or `NODES(closest-to-K)`.
- Purpose: record lookup; falls back to FIND_NODE style if responder doesn't hold record.

### 10.4 STORE

- Request: `STORE(record R)`.
- Response: `ACK(stored=true|false, reason)`.
- Purpose: write-path (§5.6). Responder validates signature, checks TTL, checks replication constraints, stores.

### 10.5 REPLICATE

- Request: `REPLICATE(record R, new_custodian_join=true)`.
- Response: `ACK`.
- Purpose: used during custody handover (§5.5). Distinct from STORE because semantics differ (custody transfer, not primary write).

---

## 11. Routing table maintenance

### 11.1 Refresh buckets

A bucket that hasn't been modified in 1 h is "stale". On staleness, station performs:

```
REFRESH_BUCKET(bucket B):
  random_id_in_B = generate 256-bit ID in B's range
  FIND_NODE(random_id_in_B)  → populates bucket B with any new peers
```

Buckets close to self refresh more frequently (every 10 min); distant buckets less often.

### 11.2 Bucket split

When a bucket containing self's prefix overflows (>k entries wanting admission):
- Split into two buckets differing in the next bit.
- Peers distribute to appropriate child bucket based on XOR-distance.

Buckets NOT containing self's prefix do not split — they cap at k. This prevents unbounded routing-table growth at large scale.

### 11.3 Eviction on repeated failure

A peer that fails to respond to k=3 consecutive PINGs is evicted from the bucket. Pending peers from the bucket's "waiting list" (peers seen but bucket was full) promote into the vacated slot if still alive.

### 11.4 Bucket diversity enforcement

On every admission (§4.3), diversity constraint is re-checked. A new peer that would push the bucket below the constraint threshold is either rejected (if an existing member strictly dominates on score) or admitted with a flag triggering `diversity_degraded` metric.

---

## 12. Crypto-puzzle and Sybil defense

### 12.1 Puzzle spec

NodeId generation:
```
repeat:
  pubkey, privkey = Ed25519_keypair()
  if leading_zeros(SHA-256(pubkey)) >= difficulty:
    accept
else continue
```

Difficulty `d` is signed in the foundation-published parameter record (§12.3). Peers verify on receipt: any record whose NodeId doesn't satisfy the current difficulty is rejected outright.

### 12.2 Cost model

Difficulty `d` implies expected 2^d tries per valid pubkey. At `d=16`, mean 65 536 tries; on commodity CPU ~65 536 × Ed25519_keygen(~15μs) = ~1 CPU-second per valid ID.

Calibration target: legitimate first-boot NodeId generation completes in <5 s on minimum-viable RPi 4B hardware.

### 12.3 Adaptive difficulty

The **adaptive-difficulty knob** (Open Q1) raises `d` when:
- Foundation monitoring observes NodeId minting rate exceeding plausible organic growth.
- Specific NodeId-prefix range shows concentration anomaly (bucket-flooding attempt).

Raising `d` is an infrequent foundation-signed parameter update. Existing NodeIds remain valid at prior difficulty (grandfathering). New-registration difficulty climbs.

### 12.4 Sybil defense in depth

Crypto-puzzle alone insufficient against state-actor. Defense-in-depth stack:

1. **Crypto-puzzle** (this section): raises per-NodeId cost.
2. **ASN/IP diversity** (§4.3, §5.2): many NodeIds from same AS diluted in diversity budget.
3. **Realm admission** (Part 5): unendorsed stations cannot serve realm data.
4. **Disjoint-path lookups** (§4.5): eclipse requires presence at every path.
5. **Foundation monitoring** (§13): fleet-wide anomaly detection.
6. **Trust multipliers** (Part 2 §9): self-declared tier without endorsement weighted low.

No single defense stops state-level adversary. Combined, cost is millions of EUR + multi-AS + multi-country presence.

---

## 13. Monitoring and observability

Runtime signals Part 3's algorithms expose:

| Metric | Source | Consumer |
|--------|--------|----------|
| DHT lookup success rate (per key type) | station | self, foundation |
| DHT lookup latency p50/p95/p99 | station | self |
| Bucket diversity score (ASN/country/tier) | station | self, foundation |
| Replica diversity score (per owned record) | owner | self, realm admin |
| Sybil-suspicion: NodeIds minted in last 24h exceeding difficulty floor | foundation | foundation |
| Eclipse-suspicion: disjoint-path lookups returning overlap >expected | station | self, foundation |
| Source-route path computation latency | station | self |
| Source-route failure code histogram | station | self, realm admin |
| HyParView active-view size per realm | station | self, realm admin |
| Plumtree tree reconfig events | station | self |

Foundation monitoring (opt-in) aggregates across the fleet and publishes signed health reports. Anomalies trigger foundation-signed alerts to affected realms.

---

## 14. Worked examples

### 14.1 Fresh CALL, cold routing cache

Node N₁ on station A (T0, BE) calls procedure `realm42/org/app/weather/get_forecast_v1` served by node N₂ on station Z (T0, PT).

1. A's routing table doesn't know Z. Lookup: `SHA-256("realm42/org/app/weather/get_forecast_v1")` via k=3 disjoint FIND_VALUE, α=3.
2. Lookup returns 20 advertisement records. A filters by realm42; 3 results; picks Z (lowest-latency from A's estimator).
3. A computes Suurballe paths to Z. Best path: `A → BE_T1 → BE_T2 → T3_AMS → PT_T2 → PT_T1 → Z`. Second path: `A → BE_T1' → DE_T2 → T3_FRA → PT_T2' → Z`. Third: a slower IT-detour alternative.
4. A sends CALL with header encoding path-1; header includes deadline now + 5 s.
5. Each hop validates header, checks SWIM-alive, forwards. Latency: 80 ms total across 6 hops.
6. Z's handler processes; response returns via reverse path; total round trip ~170 ms.

### 14.2 Retry on mid-path failure

Above CALL fails at `T3_AMS`: that station dropped the QUIC stream silently mid-hop.

1. Preceding hop (`BE_T2`) detects QUIC stream failure; sends BOLT#4 `temporary_relay_failure` signed, upstream.
2. A receives structured failure within ~30 ms of T3_AMS failure observed.
3. A retries with path-2 (disjoint); CALL succeeds.
4. A reports the failure signature to foundation monitoring.

### 14.3 Realm chat, intra-realm only

Node N₁ in realm42 publishes chat message.

1. Station A is N₁'s home station; N₁ is a realm42 member.
2. A is a HyParView active-view peer in realm42's overlay.
3. A eager-pushes message to its active peers (log₂(N)); lazy-pushes IDs to non-tree peers.
4. Message converges to all realm42 members in log₂(realm_size) × avg_latency.
5. No DHT lookups. No source-routing. All intra-realm.

### 14.4 Eclipse attempt

Adversary attempts to eclipse target T's lookups for procedure P.

1. Adversary mints 100 NodeIds in T's routing table bucket for key SHA-256(P). Cost: 100 × 1 CPU-second per node × (adaptive difficulty bump if foundation notices) = minutes to hours.
2. T's routing-table admission requires ≥5 ASN diversity per bucket. Adversary's NodeIds from a single AS fail admission; must span ≥5 ASNs.
3. Even with multi-AS presence, T's lookup uses d=3 disjoint paths. Adversary must control all 3 paths — requires deep penetration of multiple AS + country combinations.
4. Foundation monitoring observes Sybil-suspicion metric spike; foundation raises difficulty; existing adversary NodeIds grandfathered but new ones costlier; realm admins MAY refuse endorsement.

Defense holds probabilistically against all but nation-state.

---

## 15. Open questions specific to Part 3

- **O1 (from ROOT)** — Crypto-puzzle difficulty adaptive policy. Specifically: who signs the difficulty-update record (foundation threshold? Realm majority?). Decision Phase 3 start.
- **O17 (new)** — Path cache TTL. Current: 5 min. Short enough for churn; long enough to amortise Suurballe compute.
- **O18 (new)** — k=3 vs k=5 disjoint paths. V2.0 uses k=3; larger increases overhead. Revisit if Phase 7 chaos shows insufficient resilience.
- **O19 (new)** — HyParView active-view ceiling. Currently capped at 15; above this, overlay fan-out cost rises. Revisit for realms >1000 members.
- **O20 (new)** — Subscription-hint aggregation: per-station-per-realm vs per-station-global. V2.0 picks per-realm; global may leak cross-realm membership.
- **O21 (new)** — Tier-penalty coefficient in source-route edge weighting. Currently heuristic; Phase 4 chaos testing should calibrate.

---

## 16. Success criteria for Part 3

Part 3 is complete when a reader can:

1. Explain the **three S/Kademlia extensions** and why each is necessary (§4.1, §4.5, §12).
2. State the **six key families** and how each is derived (§3.3).
3. Walk through the **diversity-constrained replica placement** algorithm for k=20 (§5.2).
4. Describe what happens when **diversity constraints cannot be satisfied** (§5.2).
5. Explain **source routing with k=3 disjoint paths** including tier-increasing-then-decreasing constraint (§6).
6. Distinguish **when to use HyParView/Plumtree vs DHT + source routing** (§8).
7. Trace a **cross-realm CALL** through discovery → routing → delivery (§9, §14.1).
8. Describe **eclipse attack cost** and the defense-in-depth stack (§12.4, §14.4).
9. Explain why **tReplicate is custodian's obligation while tRepublish is owner's** (Part 4 §11; referenced).
10. Identify which V1 bug **cross-realm RPC blocker** is resolved by which §3 mechanism (§9).

If any is ambiguous, Part 3 revises before Parts 5–7 build on top.

---

*Part 3 completes the core mechanic. Parts 5–9 put skin on it: bootstrap (Part 5), wire formats (Part 6), implementation order (Part 7), verification (Part 8), open questions + references (Part 9).*
