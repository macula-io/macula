# Gateway Responsibilities - Authoritative Reference

## Executive Summary

The Macula Gateway's role **evolves across versions** as the platform transitions from centralized relay to true P2P mesh.

**Current Reality (v0.7.28)**: Gateway acts as **message router** (temporary, not ideal)
**Target Architecture (v0.8.0+)**: Gateway acts as **bootstrap service only** (correct P2P design)

## Gateway Evolution Timeline

### v0.7.x: Hybrid Architecture (CURRENT)

**Gateway acts as:** Message router + Bootstrap service (temporary design)

✅ **What Gateway DOES (v0.7.x)**:
1. **Bootstrap New Nodes**
   - Accept initial connections from edge peers
   - Provide node ID assignment
   - Share DHT routing table (K-closest nodes)
   - Enable DHT participation

2. **DHT Participation**
   - Store FIND_VALUE/STORE requests
   - Maintain routing table
   - Respond to DHT queries

3. **Message Routing** (TEMPORARY - should be removed!)
   - Routes pub/sub messages between peers
   - Routes RPC calls between peers
   - Acts as relay for all peer-to-peer communication
   - **Problem**: Creates hub-and-spoke bottleneck

❌ **Why Current Design is Wrong**:
- Gateway becomes single point of failure
- Gateway becomes performance bottleneck
- Violates mesh networking principles
- Prevents true decentralization

**Status**: This is a **transitional architecture**. Message routing through gateway works but is NOT the target design.

---

### v0.8.0: True P2P Architecture (TARGET)

**Gateway acts as:** Bootstrap service ONLY (correct design)

✅ **What Gateway SHOULD DO (v0.8.0+)**:
1. **Bootstrap New Nodes**
   - Accept initial connections
   - Provide node ID
   - Share K-closest nodes for DHT participation
   - Help peers discover other peers

2. **NAT Traversal Assistance**
   - Act as rendezvous point for hole punching
   - Coordinate simultaneous connection attempts
   - Relay connection setup messages
   - **NOT** relay application messages!

3. **Inter-Realm Bridging** (if multi-realm)
   - Bridge messages between different realms (e.g., `macula.arcade` ↔ `macula.trading`)
   - Enforce realm isolation policies
   - **Only** for cross-realm traffic, NOT intra-realm

❌ **What Gateway SHOULD NOT DO (v0.8.0+)**:
- Route intra-realm pub/sub messages
- Route intra-realm RPC calls
- Store application state
- Act as central coordinator

**Why This is Correct**:
- Peers communicate directly via DHT-routed messages
- Gateway is just a bootstrap helper
- No single point of failure for messaging
- True mesh topology
- Scales horizontally

---

## Detailed Responsibilities

### 1. Bootstrap & Discovery (ALL VERSIONS)

**Purpose**: Help new peers join the mesh

**Responsibilities**:
- Accept QUIC connections from edge peers
- Assign node IDs (or validate self-generated IDs)
- Provide initial DHT routing table (K-closest nodes)
- Share realm configuration
- Enable peer to start DHT participation

**Protocol Flow**:
```
Edge Peer                    Gateway
    |                           |
    |--- CONNECT -------------→ |
    |    (realm_id, capabilities)|
    |                           |
    |←-- CONNECT_ACK ---------- |
    |    (node_id, k_closest,   |
    |     realm_config)         |
    |                           |
    |--- FIND_NODE(random) ---→ |
    |    (populate routing      |
    |     table)                |
```

**Key Point**: After bootstrap, peer **should** communicate with other peers directly, not through gateway.

---

### 2. DHT Participation (ALL VERSIONS)

**Purpose**: Act as stable DHT node for routing table

**Responsibilities**:
- Maintain Kademlia routing table (160 k-buckets)
- Respond to FIND_NODE requests
- Respond to FIND_VALUE requests
- Accept STORE requests
- Participate in iterative DHT queries

**Why Gateway Participates in DHT**:
- Stable node (always online)
- Well-known endpoint
- Helps with DHT convergence
- Improves lookup success rate

**Key Point**: Gateway participates in DHT like any other peer, no special privileges.

---

### 3. Message Routing (v0.7.x ONLY - TO BE REMOVED)

**Purpose**: Temporary relay until P2P works

⚠️ **CURRENT BEHAVIOR (v0.7.x)**:
- Gateway receives PUBLISH messages
- Gateway queries DHT for subscribers
- Gateway routes messages to subscriber nodes
- **All messages** flow through gateway

❌ **WHY THIS IS TEMPORARY**:
- Creates bottleneck
- Single point of failure
- Not true mesh
- Doesn't scale

✅ **CORRECT BEHAVIOR (v0.8.0+)**:
- Peers query DHT directly
- Peers route messages via multi-hop DHT routing
- Messages hop through mesh (O(log N) hops)
- Gateway NOT involved in message routing

**Migration Path**:
1. v0.7.8: Implement `macula_pubsub_routing.erl` (✅ DONE)
2. v0.7.9: Enable DHT-routed pub/sub by default
3. v0.8.0: Remove gateway routing, make DHT routing mandatory

---

### 4. NAT Traversal (v0.8.0+)

**Purpose**: Help peers behind NAT establish direct connections

**Responsibilities**:
- **Rendezvous Point**: Coordinate hole-punching attempts
- **Connection Relay** (setup only): Relay connection setup messages
- **Fallback Relay** (last resort): Relay messages if direct connection fails

**NOT Responsible For**:
- Relaying all application messages (only fallback for symmetric NAT)
- Being the primary message path (P2P is primary)

**Protocol Flow**:
```
Peer A (behind NAT)      Gateway         Peer B (behind NAT)
    |                       |                    |
    |--- HOLE_PUNCH_REQ --->|                    |
    |    (want to connect   |                    |
    |     to Peer B)        |                    |
    |                       |--- HOLE_PUNCH --->  |
    |                       |    (Peer A wants   |
    |                       |     to connect)    |
    |                       |                    |
    |←------ Simultaneous UDP packets ----------→|
    |         (hole punched!)                    |
    |                       |                    |
    |←---------- Direct P2P Connection ---------→|
    |        (Gateway no longer involved)        |
```

**Success Rates**:
- v0.8.0: 80% direct P2P, 20% relay fallback
- v0.9.0: 95% direct P2P, 5% relay fallback (with STUN/TURN)

---

### 5. Inter-Realm Bridging (FUTURE - v0.9.0+)

**Purpose**: Bridge messages between different realms

**When Needed**:
- Multi-realm deployments (e.g., `macula.arcade` + `macula.trading`)
- Realm isolation required
- Cross-realm communication needed

**Responsibilities**:
- Participate in multiple realm DHTs
- Route messages from Realm A to Realm B
- Enforce realm isolation policies
- Apply cross-realm security rules

**Key Point**: Only for **inter-realm** traffic. Intra-realm traffic is pure P2P.

---

## Current State Analysis (v0.7.28)

### What Actually Works

✅ **Gateway Bootstrap**: Works perfectly
- Edge peers connect to gateway
- Get node ID and routing table
- Start participating in DHT

✅ **DHT Participation**: Works correctly
- Gateway responds to FIND_NODE
- Gateway responds to FIND_VALUE
- Gateway accepts STORE

⚠️ **Gateway Routing**: Works but should be P2P
- Messages flow through gateway
- DHT lookup happens at gateway
- Peers don't route messages themselves
- **Violates mesh principles**

❌ **DHT-Routed Pub/Sub**: Code exists but not enabled
- `macula_pubsub_routing.erl` exists with `wrap_publish/4`
- Multi-hop routing logic implemented
- **BUT**: Macula-arcade uses gateway routing instead
- **Result**: Matchmaking broken (DHT returns multiple subscribers but gateway routing has bugs)

### Why Matchmaking is Broken

**Root Cause**: Hybrid architecture confusion

1. **Subscription Phase** (works):
   - Peer subscribes to `arcade.matchmaking.snake`
   - Subscription advertised to DHT via gateway
   - DHT correctly stores multiple subscribers

2. **Publishing Phase** (broken):
   - Peer publishes matchmaking message
   - Message goes to **gateway** for routing
   - Gateway queries DHT (gets multiple subscribers)
   - Gateway routing has bugs (only routes to 1 subscriber)
   - **Should**: Peer queries DHT and routes directly via `macula_pubsub_routing.erl`

**Fix**: Enable DHT-routed pub/sub in Macula-arcade instead of using gateway routing.

---

## Roadmap Summary

### v0.7.x (Current)
- ✅ Gateway bootstrap works
- ✅ DHT participation works
- ⚠️ Gateway routing works (temporary)
- ❌ DHT-routed pub/sub exists but not used

### v0.8.0 (Target - 6-8 weeks)
- ✅ Gateway bootstrap (keep)
- ✅ DHT participation (keep)
- ✅ Enable DHT-routed pub/sub (activate existing code)
- ✅ Add NAT traversal (hole punching)
- ❌ Remove gateway routing (replace with P2P)

### v0.9.0 (Future - 8-10 weeks after v0.8.0)
- ✅ Gateway bootstrap (keep)
- ✅ DHT participation (keep)
- ✅ DHT-routed messaging (keep)
- ✅ Add STUN/TURN for 95% P2P success
- ✅ Add inter-realm bridging (if multi-realm)

---

## Decision Log

### Why Gateway Routing is Temporary

**Decision**: Gateway routing in v0.7.x is a **pragmatic compromise** to get Macula working while P2P infrastructure is developed.

**Reasons**:
- Easier to debug centralized routing
- Guaranteed connectivity (no NAT issues)
- Simpler to implement initially
- Code reuse from Bondy architecture

**Drawbacks**:
- Violates mesh principles
- Creates bottleneck
- Single point of failure
- Not the target architecture

**Timeline**: Remove in v0.8.0 when DHT-routed pub/sub is stable and NAT traversal works.

### Why Not Remove Gateway Entirely

**Decision**: Keep gateway for bootstrap + NAT traversal, even in v0.9.0

**Reasons**:
- New peers need a well-known entry point
- NAT hole punching needs rendezvous coordination
- Inter-realm bridging needs central bridge
- Provides stable DHT anchor node

**Key**: Gateway remains, but **role changes** from router to helper.

---

## References

- **DHT-Routed Pub/Sub Design**: `architecture/dht_routed_pubsub.md`
- **DHT-Routed RPC Design**: `architecture/dht_routed_rpc.md`
- **NAT Traversal Plan**: `architecture/NAT_TRAVERSAL_ROADMAP.md`
- **Kademlia Spec**: Maymounkov & Mazières, 2002

---

**Document Status**: Authoritative
**Last Updated**: 2025-11-17
**Supersedes**: All 6 previous gateway documents (archived)
