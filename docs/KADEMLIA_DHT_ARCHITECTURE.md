# Kademlia DHT Architecture in Macula

## Overview

Macula uses a **Kademlia Distributed Hash Table (DHT)** for decentralized service discovery and peer-to-peer routing. This document describes how Kademlia principles are implemented in the HTTP/3 mesh network.

**Key Reference:** [Kademlia: A Peer-to-peer Information System Based on the XOR Metric](http://www.scs.stanford.edu/~dm/home/papers/kpos.pdf) (Maymounkov & Mazières, 2002)

## Why Kademlia?

Traditional DHTs (like Chord) use numeric distance metrics that don't align well with network topology. Kademlia's XOR-based metric provides:

1. **Symmetric distance** - `distance(A, B) = distance(B, A)`
2. **Network-aware routing** - Nodes sharing common prefixes tend to be routed through similar paths
3. **O(log N) lookup complexity** - Efficient even with millions of nodes
4. **Self-organizing** - No central coordination required
5. **NAT-friendly** - Works well with HTTP/3's connection-oriented transport

## Core Concepts

### 1. Node IDs and XOR Metric

Every node in the Macula mesh has a 160-bit **Node ID** derived from:
- Gateway mode: Hash of (realm + IP + port)
- Edge peer mode: Randomly generated on startup

**XOR Distance Formula:**
```
distance(A, B) = A ⊕ B (bitwise XOR)
```

**Example:**
```
Node A: 0b10110101
Node B: 0b11010011
-------
XOR:    0b01100110  (distance = 102 decimal)
```

Nodes are considered "closer" when their XOR distance is smaller. This creates a geometric address space where routing naturally follows network topology.

### 2. K-Buckets (Routing Table)

Each node maintains a **routing table** of `160 k-buckets`, where `k = 20` (typical value).

**Structure:**
- **Bucket 0**: Nodes at distance `2^0` to `2^1 - 1` (immediate neighbors)
- **Bucket 1**: Nodes at distance `2^1` to `2^2 - 1`
- **Bucket i**: Nodes at distance `2^i` to `2^(i+1) - 1`
- **Bucket 159**: Nodes at distance `2^159` to `2^160 - 1` (furthest)

**K-Bucket Properties:**
- Maximum of `k` entries per bucket
- Least-recently-seen nodes are evicted first (LRU)
- Bucket 0 (closest nodes) is most important for routing

**Macula Implementation:**
```erlang
%% In macula_routing.erl (conceptual)
-define(KADEMLIA_K, 20).          %% Bucket size
-define(KADEMLIA_ALPHA, 3).       %% Parallel lookup factor
-define(ID_BITS, 160).            %% SHA-1 hash size

-record(k_bucket, {
    distance_range :: {integer(), integer()},
    nodes :: [node_entry()],       %% Max 20 entries
    last_updated :: erlang:timestamp()
}).
```

### 3. DHT Operations

Kademlia defines four core RPC operations:

#### PING
**Purpose:** Check if a node is alive

**Request:**
```erlang
{ping, FromNodeID, Timestamp}
```

**Response:**
```erlang
{pong, ToNodeID, Timestamp}
```

**Use Case:** Health checking, routing table maintenance

---

#### STORE
**Purpose:** Store a key-value pair on a node

**Request:**
```erlang
{store, Key, Value, TTL}
```

**Response:**
```erlang
{stored, Key, ExpiresAt}
```

**Macula Mapping:**
- **Key**: Service name hash (e.g., `sha1("game.matchmaking")`)
- **Value**: Service endpoint info (realm, node_id, capabilities)
- **TTL**: Advertisement lifetime (default: 5 minutes)

---

#### FIND_NODE
**Purpose:** Locate k closest nodes to a target ID

**Request:**
```erlang
{find_node, TargetID, K}
```

**Response:**
```erlang
{nodes, [
    {NodeID1, IP1, Port1, Realm1},
    {NodeID2, IP2, Port2, Realm2},
    ...
]} %% Sorted by XOR distance to TargetID
```

**Use Case:** Populating routing table, iterative lookups

---

#### FIND_VALUE
**Purpose:** Retrieve value for a key (service discovery)

**Request:**
```erlang
{find_value, Key}
```

**Response (if found):**
```erlang
{value, Key, ServiceInfo}
```

**Response (if not found):**
```erlang
{nodes, [...]}  %% Fallback to FIND_NODE
```

**Macula Mapping:**
- **Key**: Service name (e.g., `"game.matchmaking"`)
- **ServiceInfo**: List of providers advertising that service

---

### 4. Iterative Lookup Algorithm

To find a service or node, Macula performs an **iterative FIND_VALUE** lookup:

**Algorithm:**
1. Start with the `α = 3` closest nodes from local routing table
2. Send parallel `FIND_VALUE` requests to these nodes
3. If value found → return immediately
4. Otherwise, add returned nodes to candidate set
5. Select next `α` closest unqueried nodes
6. Repeat until:
   - Value is found, OR
   - No closer nodes remain, OR
   - Maximum hops reached (log₂N)

**Convergence:** Guaranteed to find value in O(log N) hops

**Macula Implementation (conceptual):**
```erlang
find_service(ServiceName, Realm) ->
    Key = crypto:hash(sha, ServiceName),
    ClosestNodes = get_closest_nodes(Key, ?KADEMLIA_ALPHA),
    iterative_lookup(Key, ClosestNodes, #{}, 0).

iterative_lookup(Key, [], _Queried, _Hops) ->
    {error, not_found};
iterative_lookup(Key, Candidates, Queried, Hops) when Hops > 20 ->
    {error, max_hops_exceeded};
iterative_lookup(Key, Candidates, Queried, Hops) ->
    %% Query α closest unqueried nodes in parallel
    Results = query_nodes(Candidates, {find_value, Key}),

    case find_value_in_results(Results) of
        {ok, Value} ->
            {ok, Value};
        not_found ->
            NewNodes = extract_nodes(Results),
            NextCandidates = select_closest_unqueried(Key, NewNodes, Queried),
            iterative_lookup(Key, NextCandidates,
                           maps:merge(Queried, mark_queried(Candidates)),
                           Hops + 1)
    end.
```

---

## Macula-Specific Adaptations

### 1. Realm-Scoped DHT

Unlike traditional Kademlia, Macula implements **multi-tenancy via realms**:

- Each realm has its own isolated DHT keyspace
- Node IDs include realm hash: `sha1(Realm || IP || Port)`
- Service keys are realm-scoped: `sha1(Realm || ServiceName)`
- Cross-realm queries are blocked at protocol level

**Example:**
```erlang
%% Realm "macula.arcade"
NodeID_A = sha1("macula.arcade" ++ "192.168.1.10" ++ "4433")

%% Realm "macula.energy"
NodeID_B = sha1("macula.energy" ++ "192.168.1.10" ++ "4433")

%% Same IP/port, different realms → different DHT partitions
```

### 2. Gateway Bootstrap Nodes

Traditional Kademlia requires **bootstrap nodes** to join the network. Macula handles this differently:

**Gateway Mode:**
- Gateways act as well-known bootstrap nodes
- Published at predictable URLs (e.g., `https://gateway.example.com:4433`)
- Maintain authoritative registry for their realm

**Edge Peer Mode:**
- Peers connect to gateway via `MACULA_BOOTSTRAP_REGISTRY` env var
- Gateway returns `α` closest nodes to peer's ID
- Peer populates routing table via `FIND_NODE` requests

**No Hardcoded Bootstrap IPs:** Unlike BitTorrent DHT, Macula uses DNS/HTTPS URLs for gateway discovery, making it firewall-friendly.

### 3. Service Advertisement TTL

Services are stored in the DHT with a **Time-To-Live (TTL)**:

- Default TTL: **5 minutes**
- Providers re-advertise every **60 seconds** (heartbeat)
- Expired entries are removed by `macula_advertisement_manager`

**Rationale:** Short TTL ensures stale services don't linger after node crashes, while frequent heartbeats maintain availability.

### 4. HTTP/3 Transport Integration

Traditional Kademlia uses UDP for RPCs. Macula uses **QUIC (HTTP/3)**:

**Advantages:**
- Reliable transport (no packet loss issues)
- Connection multiplexing (multiple RPCs over one connection)
- TLS 1.3 encryption (secure by default)
- NAT traversal (QUIC connection migration)

**Message Encoding:**
```erlang
%% DHT query wrapped in Macula protocol
#{
  type => dht_query,
  operation => find_value,
  key => <<ServiceNameHash:160>>,
  realm => <<"macula.arcade">>,
  from_node_id => <<SenderNodeID:160>>,
  timestamp => erlang:system_time(millisecond)
}
```

---

## Routing Table Maintenance

Macula keeps routing tables fresh through **active probing**:

### 1. Passive Updates
- When receiving any message from node N, refresh N's entry in k-bucket
- Update last-seen timestamp
- Move N to tail of LRU list (most recently seen)

### 2. Active Probing
- Every **60 seconds**, ping least-recently-seen node in each non-empty bucket
- If ping fails 3 times consecutively, evict node
- Backfill bucket via `FIND_NODE` request

### 3. Bucket Splitting
- When bucket 0 (closest nodes) exceeds `k` entries, split into two buckets
- Move nodes to new buckets based on refined distance ranges
- Only split buckets containing own node ID (hot zone)

**Implementation:**
```erlang
%% In macula_routing.erl
-define(REFRESH_INTERVAL, 60000).  %% 60 seconds
-define(PING_TIMEOUT, 5000).       %% 5 seconds
-define(MAX_FAILURES, 3).

refresh_buckets(State) ->
    lists:foldl(fun refresh_bucket/2, State, State#state.k_buckets).

refresh_bucket(Bucket, State) ->
    case get_least_recent_node(Bucket) of
        undefined -> State;  %% Empty bucket
        Node ->
            case ping_node(Node) of
                pong -> update_node_timestamp(Node, State);
                timeout -> handle_ping_failure(Node, State)
            end
    end.
```

---

## Performance Characteristics

| Metric | Value | Notes |
|--------|-------|-------|
| **Lookup Time** | O(log N) hops | N = total nodes in mesh |
| **Routing Table Size** | O(log N) entries | ~160 buckets × 20 nodes = 3,200 max |
| **Network Traffic** | O(log N) messages/lookup | α parallel requests per hop |
| **Storage** | O(k × log N) | Per node, for advertised services |
| **Convergence** | < 1 second | Typical for 10,000-node network |

**Example (10,000-node network):**
- Expected hops: `log₂(10,000) ≈ 13.3` → 14 hops max
- Parallel factor α = 3 → ~5 rounds of queries
- Average lookup time: `5 rounds × 100ms RTT = 500ms`

---

## Comparison to Other DHTs

| Feature | Kademlia (Macula) | Chord | Pastry |
|---------|-------------------|-------|--------|
| **Distance Metric** | XOR (symmetric) | Modular arithmetic | Numeric proximity |
| **Routing Complexity** | O(log N) | O(log N) | O(log N) |
| **Lookup Parallelism** | Yes (α = 3) | No (sequential) | Limited |
| **NAT-Friendly** | ✅ (with HTTP/3) | ❌ | ❌ |
| **Bootstrap Required** | Optional (gateway) | Yes | Yes |
| **Self-Organizing** | ✅ | ✅ | ✅ |

---

## Future Enhancements

### 1. S/Kademlia (Secure Kademlia)
- Cryptographic node ID generation (prevent Sybil attacks)
- Require nodes to solve proof-of-work for ID assignment
- Disjoint routing paths for redundancy

### 2. DHT Persistence
- Store service advertisements in distributed database (e.g., CRDT)
- Survive network partitions with eventual consistency
- Reduce re-advertisement overhead

### 3. Adaptive K-Bucket Sizing
- Dynamically adjust `k` based on network churn rate
- Larger `k` for stable networks (less probing overhead)
- Smaller `k` for high-churn networks (faster convergence)

---

## References

1. **Kademlia Paper:** [Maymounkov & Mazières (2002)](http://www.scs.stanford.edu/~dm/home/papers/kpos.pdf)
2. **S/Kademlia:** [Baumgart & Mies (2007)](https://ieeexplore.ieee.org/document/4447808)
3. **BitTorrent DHT (BEP-0005):** [bittorrent.org/beps/bep_0005.html](http://www.bittorrent.org/beps/bep_0005.html)
4. **QUIC Protocol:** [RFC 9000](https://www.rfc-editor.org/rfc/rfc9000.html)
5. **Macula Architecture:** `docs/QUIC_TLS_GATEWAY_SETUP.md`

---

**Last Updated:** 2025-11-15
**Macula Version:** 0.6.0
**Status:** ✅ Production-ready DHT implementation
