# Macula Architecture Evolution

**Status**: Living document tracking architecture phases
**Last Updated**: 2025-11-15

---

## The Journey from Star to Mesh

Macula is **evolving** from a centralized hub-and-spoke architecture to a true decentralized mesh. This document clarifies what exists today vs. the vision for tomorrow.

---

## Phase 1: Hub-and-Spoke (v0.5.0 - v0.7.x) ✅ CURRENT

**Architecture**: Star topology with gateway relay

```
┌─────────────┐
│   Peer 1    │─────┐
└─────────────┘     │
                    ↓
┌─────────────┐  ┌──────────┐
│   Peer 2    │─→│ Gateway  │ ← All traffic flows here
└─────────────┘  │          │
                 └──────────┘
┌─────────────┐     ↑
│   Peer 3    │─────┘
└─────────────┘
```

### Characteristics

**Topology**:
- ❌ **NOT a mesh** - star/hub-and-spoke topology
- ❌ Gateway is single point of failure
- ❌ Gateway is bandwidth bottleneck
- ❌ Not decentralized (gateway controls routing)
- ✅ 100% connectivity (gateway relay always works)

**How It Works**:
- Edge peers connect to gateway (persistent QUIC connection)
- Gateway maintains DHT routing table
- Gateway routes all messages between peers
- Pub/Sub: Peer A → Gateway → Peer B
- RPC: Caller → Gateway → Provider → Gateway → Caller

**Why This Exists**:
- NAT traversal not yet implemented
- Edge peers behind NAT cannot accept incoming connections
- Gateway relay is **only way** to achieve 100% connectivity
- Foundation for future P2P (DHT routing, protocol support)

**Version History**:
- v0.5.0: Initial release
- v0.6.0: Realm multi-tenancy
- v0.7.0: Nomenclature refactoring (connection → peer)
- v0.7.1-v0.7.3: Bug fixes (pub/sub, DNS, DHT serialization)
- v0.7.4: Keep-alive prevents connection timeout

**Key Limitation**: Gateway must stay running or network fails.

---

## Phase 2: Hybrid Mesh (v0.8.0) 🎯 NEXT

**Architecture**: Opportunistic P2P with gateway fallback

```
                    Gateway (Discovery/Fallback)
                          ↓
 Peer 1 (NAT) ←─────────────────→ Peer 2 (NAT)
    ↕                                    ↕
    └──────→ Peer 3 (Public IP) ←───────┘
         (Direct P2P via hole punching)
```

### Target Characteristics

**Topology**:
- ✅ **Partial mesh** - direct P2P where possible
- ✅ Gateway as fallback (not primary route)
- ✅ No single point of failure for most traffic
- ⚠️ Symmetric NAT still requires relay
- ✅ 80% direct P2P + 20% gateway relay

**How It Works**:
- Peers attempt NAT hole punching on connection
- Direct QUIC connection established if NAT allows
- Gateway coordinates hole punching (exchange addresses)
- Fall back to gateway relay if hole punch fails
- Messages route peer→peer when direct connection exists

**New Modules**:
- `macula_nat_discovery.erl` - Detect public IP and NAT type
- `macula_hole_punch.erl` - Coordinate simultaneous connection attempts
- `macula_connection_upgrade.erl` - Migrate relay→direct

**Implementation Timeline**:
- Target: Q2 2025
- Duration: 6-8 weeks
- See: `architecture/NAT_TRAVERSAL_ROADMAP.md`

**Success Criteria**:
- 80%+ direct P2P connections (cone NAT)
- 100% connectivity (with relay fallback)
- Gateway bandwidth reduced by 70-80%
- Backward compatible with v0.7.x clients

---

## Phase 3: Kademlia DHT Mesh (v0.9.0) 🔮 FUTURE

**Architecture**: True P2P with DHT routing and STUN/TURN infrastructure

```
                Kademlia DHT Overlay
                (Each node: O(log N) connections)

┌─────────────┐
│   Peer 1    │──→ K-bucket neighbors (20-50 peers)
└─────────────┘      ↓
      XOR routing    Multi-hop via closest nodes

┌─────────────┐
│   Peer 2    │──→ K-bucket neighbors (20-50 peers)
└─────────────┘      ↓

┌─────────────┐      Different neighbors per peer
│  Gateway A  │──→ K-bucket neighbors (bootstrap only)
└─────────────┘
```

**IMPORTANT**: This is **NOT a fully-connected mesh** (O(N²) - impossible to scale). This is a **Kademlia DHT overlay network** where each peer maintains **O(log N) connections** to neighbors in its routing table.

### Target Characteristics

**Topology**:
- ✅ **DHT-routed mesh** - O(log N) connections per node
- ✅ Kademlia routing table (160 k-buckets, ~20-50 active connections)
- ✅ Multi-hop routing via XOR distance metric
- ✅ Gateway only for discovery/bootstrap
- ✅ Scales to thousands of nodes
- ✅ 95% direct P2P + 5% TURN relay

**How It Works**:
- ICE (Interactive Connectivity Establishment)
- STUN for address discovery
- TURN for symmetric NAT relay
- Gateway provides discovery only
- Multi-hop DHT routing through peers

**New Infrastructure**:
- STUN servers (address discovery)
- TURN servers (relay for symmetric NAT)
- Multi-gateway deployment

**Implementation Timeline**:
- Target: Q4 2025
- Duration: 8-10 weeks
- See: `architecture/NAT_TRAVERSAL_ROADMAP.md`

**Success Criteria**:
- 95%+ direct P2P connections
- Gateway only for bootstrap (not routing)
- Network survives gateway failure
- Multi-region deployment

---

## Comparison Matrix

| Feature | v0.7.x (Hub) | v0.8.0 (Hybrid) | v0.9.0 (DHT Mesh) |
|---------|--------------|-----------------|---------------------|
| **Direct P2P** | 0% | 80% | 95% |
| **Connections per Node** | 1 (gateway) | 1-5 (opportunistic) | O(log N) (20-50) |
| **Routing** | Gateway relay | Mixed (direct/relay) | DHT multi-hop |
| **Gateway Dependency** | 100% routing | 20% fallback | Bootstrap only |
| **Single Point of Failure** | Yes (gateway) | No (mostly) | No |
| **NAT Support** | All (relay) | Cone NAT | All NAT types |
| **Bandwidth Bottleneck** | Gateway | Minimal | None |
| **Decentralized** | No | Partial | Yes |
| **Scalability** | Limited | Hundreds | Thousands |
| **Routing Hops** | 1 (via gateway) | 1 direct / 1 relay | O(log N) avg |
| **DHT Routing** | ❌ | ⚠️ | ✅ |

---

## Critical Clarification: What "Mesh" Means

### ❌ NOT Fully-Connected Mesh (O(N²))

**WRONG Architecture** (does not scale):
```
Every peer connects to every other peer
1,000 nodes = 999,000 connections (impossible!)
```

**Why this fails**:
- O(N²) connection growth
- Bandwidth explosion
- Memory explosion
- Cannot scale beyond ~100 nodes

### ✅ Kademlia DHT Overlay (O(log N))

**CORRECT Architecture** (scales to millions):
```
Each peer maintains ~20-50 connections to DHT neighbors
1,000 nodes = ~30,000 total connections (10 avg per node)
Messages route via XOR distance metric (multi-hop)
```

**How Kademlia scales**:
- O(log N) connections per node
- O(log N) routing hops
- 160 k-buckets (one per bit of 160-bit ID space)
- Only ~1-5 peers per k-bucket on average
- Scales to millions of nodes (see: BitTorrent, IPFS)

**Macula uses Kademlia**, not fully-connected mesh!

---

## Why Version Numbers Matter

**v0.7.x**: Star topology improvements (bug fixes, keep-alive)
**v0.8.0**: **Architectural shift** to hybrid mesh (P2P begins)
**v0.9.0**: **Architectural completion** to full mesh (true P2P)

Version numbers reflect **topology changes**, not just features.

---

## Current Status (v0.7.4)

**What We Have**:
- ✅ Stable gateway relay architecture
- ✅ 100% connectivity (via relay)
- ✅ DHT routing table (at gateway)
- ✅ Protocol supports P2P (not yet used)
- ✅ Keep-alive prevents connection timeout

**What We DON'T Have Yet**:
- ❌ Direct peer-to-peer connections
- ❌ NAT traversal (hole punching, STUN/TURN)
- ❌ True mesh routing
- ❌ Decentralization
- ❌ Gateway redundancy

**Honest Assessment**: Macula v0.7.4 is **not yet a mesh** - it's a centralized message relay with a DHT-based routing table. The mesh functionality is **planned for v0.8.0+**.

---

## References

- **NAT Traversal Design**: `architecture/NAT_TRAVERSAL_ROADMAP.md`
- **DHT Architecture**: `docs/KADEMLIA_DHT_ARCHITECTURE.md`
- **Project Roadmap**: `architecture/macula_http3_mesh_roadmap.md`
- **Changelog**: `CHANGELOG.md`

---

**Key Takeaway**: Macula is **becoming** a Kademlia DHT overlay network. The infrastructure exists (DHT routing table, protocol), but NAT traversal is required before peers can establish the O(log N) direct connections needed for distributed routing. v0.8.0 will mark the transition from centralized relay to decentralized DHT routing.

---

## Frequently Asked Questions

### Q: Is Macula a "fully-connected mesh"?

**No.** Macula uses **Kademlia DHT** with O(log N) connections per node, not O(N²) fully-connected mesh. This is the same architecture as:
- BitTorrent (DHT for peer discovery)
- IPFS (Kademlia routing)
- Ethereum (node discovery)

A fully-connected mesh cannot scale beyond ~100 nodes due to O(N²) connection explosion.

### Q: How many connections does each peer maintain?

**Current (v0.7.x)**: 1 connection (to gateway)
**Target (v0.9.0)**: ~20-50 connections (to DHT neighbors in k-buckets)

With 1,000 nodes, each maintains ~30-50 connections (O(log 1000) ≈ 10 hops), NOT 999 connections.

### Q: How do messages route without connecting to every peer?

**Kademlia XOR distance routing**:
1. Message has target ID (e.g., topic hash for pub/sub)
2. Current node forwards to **closest peer** in routing table (XOR distance)
3. That peer forwards to its closest peer
4. Repeats until target reached (O(log N) hops)

Similar to how DNS works (hierarchical routing), not like a telephone network (direct connection).

### Q: Why not just connect everyone to everyone?

**Mathematics makes it impossible**:
- 100 nodes = 4,950 connections (manageable)
- 1,000 nodes = 499,500 connections (barely feasible)
- 10,000 nodes = 49,995,000 connections (impossible)

Kademlia DHT enables **millions of nodes** with **O(log N) scalability**.
