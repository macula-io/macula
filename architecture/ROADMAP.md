# Macula Roadmap

> **Last Updated:** 2025-12-02
> **Current Version:** v0.14.0
> **Status:** Ra/Raft removed, CRDT foundation complete. Preparing v0.14.1 with pub/sub fixes.

---

## Executive Summary

This roadmap reflects a significant architectural refinement (December 2025) that introduces:

1. **Macula Cluster** - Deployment-agnostic logical grouping (replaces "MuC")
2. **CRDTs + Gossip** - Replaces Raft consensus for coordination
3. **Bridge Nodes** - Cross-Cluster federation for SuperMesh
4. **Federated Registry** - Secure application distribution
5. **Protocol Gateway** - HTTP/3 API for non-BEAM clients

**Key Change:** The reckon_db dependency is removed. CRDTs provide distributed state without external event store.

---

## Architecture Overview

### Fractal Mesh Hierarchy

SuperMesh is **fractal** - it nests at any geographic scale:

```
Cluster (Home)           ←── Smallest unit (1-10 nodes)
    └─► Street Mesh          ←── Neighbors
            └─► Neighborhood Mesh
                    └─► City Mesh
                            └─► Province Mesh
                                    └─► Country Mesh
                                            └─► Region Mesh (EU, NA, APAC)
                                                    └─► Global Mesh
```

### Example: City-Level View

```
┌─────────────────────────────────────────────────────────────────┐
│                      CITY MESH: Amsterdam                       │
│  ┌─────────────────────────┐  ┌─────────────────────────┐      │
│  │  NEIGHBORHOOD: Centrum   │  │  NEIGHBORHOOD: Zuid      │      │
│  │  ┌───────┐  ┌───────┐   │  │  ┌───────┐  ┌───────┐   │      │
│  │  │Street │  │Street │   │  │  │Street │  │Street │   │      │
│  │  │ Mesh  │  │ Mesh  │   │  │  │ Mesh  │  │ Mesh  │   │      │
│  │  └───┬───┘  └───┬───┘   │  │  └───┬───┘  └───┬───┘   │      │
│  │      └──────────┘       │  │      └──────────┘       │      │
│  └────────────┬────────────┘  └────────────┬────────────┘      │
│               └────────────────────────────┘                    │
│                       Bridge Nodes                              │
└─────────────────────────────────────────────────────────────────┘
```

### Single Cluster (Smallest Unit)

Nodes within a Cluster form their own **intra-cluster mesh** (Erlang distribution, or QUIC distribution in future).

```
┌─────────────────────────────────────────────────────────────────┐
│              CLUSTER (e.g., Home Server)                        │
│                                                                  │
│   ┌─────┐◄────────────►┌─────┐◄────────────►┌─────┐            │
│   │Node │              │Node │              │Node │            │
│   │  1  │◄────────────►│  2  │◄────────────►│  3  │            │
│   └─────┘              └─────┘              └─────┘            │
│          Intra-cluster: Erlang distributed mesh                 │
│              Local DHT (Kademlia) + CRDT State                  │
│                           │                                     │
│                    Bridge Node ──► Inter-cluster: QUIC/HTTP3    │
└─────────────────────────────────────────────────────────────────┘
```

**Two transport layers:**
- **Intra-cluster:** Erlang distribution (or QUIC distribution when ready)
- **Inter-cluster:** Macula QUIC/HTTP3 via Bridge Nodes

### Terminology

| Term | Definition |
|------|------------|
| **Macula Cluster** | Smallest unit: local deployment (home, office, edge). 1-10 nodes. |
| **Seed Node** | DHT entry point for new peers. No special software - just well-known address. |
| **Bridge Node** | Connects to next mesh level. Bridges form their own mesh + DHT at each level. |
| **SuperMesh** | Any federation level above Cluster (street, city, country, global). Fractal. |
| **Realm** | Virtual namespace spanning the entire hierarchy (like DNS domain). |

### Hierarchical DHT

Each level of the mesh has its own DHT. Bridge Nodes form meshes at each level:

```
┌─────────────────────────────────────────────────────────────────┐
│                    CITY MESH (Bridge Layer)                     │
│   Bridge◄──►Bridge◄──►Bridge    ← City-level DHT               │
└───────┬─────────┬─────────┬─────────────────────────────────────┘
        │         │         │
┌───────▼───┐ ┌───▼───┐ ┌───▼───┐
│NEIGHBORHOOD│ │NEIGHB.│ │NEIGHB.│  ← Neighborhood-level DHTs
│Bridge mesh │ │ mesh  │ │ mesh  │
└───────┬────┘ └───┬───┘ └───┬───┘
        │          │         │
┌───────▼───┐ ┌────▼──┐ ┌────▼──┐
│STREET mesh│ │STREET │ │STREET │  ← Street-level DHTs
│(Bridges)  │ │ mesh  │ │ mesh  │
└───────┬───┘ └───┬───┘ └───┬───┘
        │         │         │
     Clusters  Clusters  Clusters   ← Cluster-level DHTs
```

**DHT query escalation (locality-first):**
1. Query local Cluster DHT
2. If miss → escalate to Street Mesh DHT
3. If miss → escalate to Neighborhood Mesh DHT
4. Continue until found or top level reached
5. Cache results at lower levels

---

## Current State (v0.12.6)

### Completed Features

| Component | Status | Notes |
|-----------|--------|-------|
| QUIC Transport | **Complete** | Full gen_server, quicer integration |
| PubSub (local) | **Complete** | Topic-based, wildcard support |
| RPC (local) | **Complete** | NATS-style async, direct P2P |
| DHT Kademlia | **Complete** | k-bucket routing, service discovery |
| Gateway System | **Complete** | Message routing, client management |
| Bootstrap System | **Complete** | DHT bootstrap, service registry |
| TLS Security | **Complete** | Two-mode (production/development) |
| Hybrid Trust | **Complete** | Realm auth + TOFU + rate limiting |
| NAT Traversal | **Complete** | Hole punching, connection pooling, relay |
| Memory Management | **Complete** | Bounded pools, TTL cleanup |

### What's Incomplete (Being Replaced)

| Component | Old Plan | New Approach |
|-----------|----------|--------------|
| Platform Layer (Raft) | reckon_db integration | **CRDTs + Gossip** (no Raft) |
| Distributed CRDTs | Local ETS only | **Gossip-replicated CRDTs** |
| Cross-realm | Not planned | **Bridge Nodes + Federation** |
| App distribution | Not planned | **Federated Registry** |
| Non-BEAM clients | Not planned | **Protocol Gateway (HTTP/3 API)** |

---

## Revised Version Plan

| Version | Focus | Status |
|---------|-------|--------|
| v0.13.0 | **Bridge System** | ✅ COMPLETED - Hierarchical DHT, Bridge Nodes, Cache |
| v0.14.0 | **CRDT Foundation** | ✅ COMPLETED - Ra/Raft removed, OR-Set, G-Counter, PN-Counter |
| v0.14.1 | **Pub/Sub Fixes** | IN PROGRESS - Remove message amplification, DHT routing fixes |
| v0.15.0 | **Registry System** | Package signing, Cluster Controller, security scanning |
| v0.16.0 | **Protocol Gateway** | HTTP/3 API, WebTransport, OpenAPI spec |
| v1.0.0 | **Production Ready** | Full Cluster + Bridge + Registry |
| v1.1.0+ | **Ecosystem** | QUIC Distribution, macula_crdt hex package |

### v0.13.0 - Bridge System (COMPLETED)

Implemented hierarchical DHT with Bridge mesh support:

- ✅ `macula_bridge_system.erl` - Supervisor for bridge subsystem
- ✅ `macula_bridge_node.erl` - Parent mesh connection and query escalation
- ✅ `macula_bridge_mesh.erl` - Peer-to-peer mesh between bridges
- ✅ `macula_bridge_cache.erl` - TTL-based caching with LRU eviction
- ✅ Routing integration with `find_value_with_escalation/5`
- ✅ 40 tests for bridge system

---

## v0.14.0 - CRDT Foundation (COMPLETED - December 2025)

**Goal:** Replace ETS-based registries with CRDT-backed versions using gossip replication.

**Status:** ✅ Ra/Raft **REMOVED**, CRDT Foundation **IMPLEMENTED** (48 tests)

**What was delivered:**
- ✅ `macula_crdt.erl` - Core types and operations
- ✅ `macula_crdt_orset.erl` - OR-Set implementation (17 tests)
- ✅ `macula_crdt_lww.erl` - LWW-Register implementation (14 tests)
- ✅ `macula_crdt_gcounter.erl` - G-Counter implementation (9 tests)
- ✅ `macula_crdt_pncounter.erl` - PN-Counter implementation (8 tests)
- ✅ Removed `macula_leader_election.erl` (deleted)
- ✅ Removed `macula_leader_machine.erl` (deleted)
- ✅ Removed `ra` dependency from rebar.config
- ✅ Updated `macula_platform_system.erl` (now masterless)

### Deliverables

#### 1. Core CRDT Module

```erlang
%% apps/macula_crdt/src/macula_crdt.erl
-type or_set() :: #{
    adds => #{element() => vector_clock()},
    removes => #{element() => vector_clock()}
}.

-type lww_register() :: #{
    value => term(),
    timestamp => integer(),
    node_id => node_id()
}.
```

**Operations:**
- `or_set_add/2`, `or_set_remove/2`, `or_set_merge/2`, `or_set_value/1`
- `lww_register_set/2`, `lww_register_merge/2`, `lww_register_value/1`

#### 2. Gossip Protocol

```erlang
%% apps/macula_crdt/src/macula_gossip.erl
-spec start_gossip(cell_id()) -> ok.
-spec push_state(node_id()) -> ok.   %% Push local state to peer
-spec pull_state(node_id()) -> ok.   %% Request state from peer
-spec anti_entropy() -> ok.          %% Periodic full state sync
```

**Parameters:**
- Push interval: 1 second
- Anti-entropy: 30 seconds
- Fanout: 3 peers per round

#### 3. CRDT-backed Registry

Wrap existing `macula_bootstrap_registry` with CRDT frontend:
- Service registrations use OR-Set
- Node metadata uses LWW-Register
- Gossip syncs state across Cluster

### Files to Create

```
apps/macula_crdt/
├── src/
│   ├── macula_crdt.erl           # Core types and operations
│   ├── macula_crdt_orset.erl     # OR-Set implementation
│   ├── macula_crdt_lww.erl       # LWW-Register implementation
│   └── macula_gossip.erl         # Gossip protocol
├── test/
│   ├── macula_crdt_tests.erl
│   └── macula_gossip_tests.erl
└── rebar.config
```

### Acceptance Criteria

- [x] OR-Set correctly handles concurrent add/remove (17 tests)
- [x] LWW-Register resolves conflicts by timestamp (14 tests)
- [ ] Gossip achieves convergence within 5 seconds (3-node Cluster) - **Deferred to v0.14.1+**
- [x] Existing tests pass with CRDT backend
- [x] 48 new tests for CRDT operations (originally targeted 50+)

---

## v0.14.1 - Pub/Sub Routing Fixes (IN PROGRESS)

**Goal:** Fix message amplification issues in DHT-routed pub/sub and improve routing reliability.

**Status:** IN PROGRESS (December 2025)

### Changes

**Bug Fixes:**
- ✅ Removed `relay_to_mesh_peers/4` from `macula_gateway.erl` - caused exponential message amplification
- ✅ Added `build_gateway_endpoint/1` for proper PONG response endpoint construction
- ✅ Fixed `macula_protocol_types_tests.erl` - updated for new pubsub_route (0x13) message type

**DHT Routing Improvements:**
- ✅ Enhanced DHT routing in `macula_pubsub_dht.erl`
- ✅ Improved topic subscription handling

### Files Modified

| File | Change |
|------|--------|
| `src/macula_gateway_system/macula_gateway.erl` | Removed relay_to_mesh_peers, added build_gateway_endpoint |
| `src/macula_pubsub_system/macula_pubsub_dht.erl` | DHT routing enhancements |
| `test/macula_protocol_types_tests.erl` | Fixed unassigned ID tests (0x13→0x14, 0x24→0x26) |

### Acceptance Criteria

- [x] No message amplification in pub/sub routing
- [x] Protocol type tests pass (0x13 is pubsub_route, not unassigned)
- [x] Version bumped to v0.14.1
- [x] Documentation updated (CHANGELOG.md, CLAUDE.md, ROADMAP.md)
- [ ] All unit tests pass (20 infrastructure-related failures remain - require QUIC/TLS services)
- [ ] Published to hex.pm (requires manual `rebar3 hex publish`)

---

## Bridge Nodes - COMPLETED in v0.13.0

> **Note:** The Bridge Node functionality was implemented in v0.13.0 (December 2025).
> See "v0.13.0 - Bridge System (COMPLETED)" section above.

**What was delivered:**
- ✅ `macula_bridge_system.erl` - Supervisor
- ✅ `macula_bridge_node.erl` - Parent mesh connection
- ✅ `macula_bridge_mesh.erl` - Peer-to-peer mesh
- ✅ `macula_bridge_cache.erl` - TTL-based caching
- ✅ DHT query escalation via `find_value_with_escalation/5`
- ✅ 40 tests

**Future enhancements (v0.15.0+):**
- [ ] Federation policy enforcement
- [ ] DNS SRV bridge discovery
- [ ] boot.macula.io directory integration

---

## v0.15.0 - Registry System

**Goal:** Secure application distribution with federated registries.

### Deliverables

#### 1. Registry Server

```erlang
%% apps/macula_registry/src/macula_registry.erl
-spec publish_package(package(), signature()) -> {ok, pkg_id()} | {error, term()}.
-spec fetch_package(pkg_id()) -> {ok, package()} | {error, not_found}.
-spec verify_package(package(), pubkey()) -> ok | {error, invalid_signature}.
```

#### 2. Package Manifest

```erlang
%% myapp.macula.manifest
#{
    name => <<"myapp">>,
    version => <<"1.0.0">>,
    capabilities => [
        {network, [{connect, <<"*.example.com:443">>}]},
        {pubsub, [{publish, <<"myapp.*">>}]},
        {rpc, [{register, <<"myapp.api.*">>}]}
    ],
    nifs => [],
    otp_release => <<"27">>
}.
```

#### 3. Cluster Controller

- Watches configured registries
- Verifies signatures before deploy
- Manages app lifecycle (deploy, upgrade, remove)
- Enforces local deployment policy

#### 4. Security Scanning (Basic)

Layer 1 - Static Analysis:
- Scan for dangerous BIFs (os:cmd, open_port)
- Audit NIF dependencies
- Flag undeclared capabilities

Layer 2 - Runtime Monitor:
- Memory limits
- Message queue limits
- Crash rate detection
- Throttle → Kill → Quarantine

### Files to Create

```
apps/macula_registry/
├── src/
│   ├── macula_registry.erl           # Registry server
│   ├── macula_registry_pkg.erl       # Package management
│   ├── macula_registry_verify.erl    # Signature verification
│   ├── macula_registry_scan.erl      # Static analysis
│   ├── macula_cell_controller.erl    # Deployment controller
│   └── macula_app_monitor.erl        # Runtime defense
└── test/
```

### Acceptance Criteria

- [ ] Package signing and verification
- [ ] Cluster Controller deploys from registry
- [ ] Static analysis detects dangerous BIFs
- [ ] Runtime monitor kills runaway apps
- [ ] 50+ tests for registry system

---

## v0.16.0 - Protocol Gateway

**Goal:** HTTP/3 API for non-BEAM clients.

### Deliverables

#### 1. HTTP/3 Endpoints

| Method | Path | Purpose |
|--------|------|---------|
| `POST` | `/v1/session` | Establish session |
| `POST` | `/v1/rpc/{realm}/{procedure}` | Call RPC |
| `POST` | `/v1/publish/{realm}/{topic}` | Publish event |
| `GET` | `/v1/subscribe/{realm}/{topic}` | Subscribe (SSE) |
| `GET` | `/v1/discover/{realm}/{pattern}` | Find services |

#### 2. Message Encoding

- Primary: MessagePack
- Fallback: JSON
- Content negotiation via Accept header

#### 3. WebTransport Support

- Browser-to-mesh direct communication
- Bidirectional streams
- WebSocket fallback for older browsers

#### 4. Protocol Documentation

- `docs/PROTOCOL.md` - Wire protocol spec
- `docs/AUTH.md` - Authentication flows
- `docs/ERRORS.md` - Error taxonomy
- `docs/openapi.yaml` - OpenAPI spec

### Files to Create

```
apps/macula_protocol/
├── src/
│   ├── macula_protocol_http3.erl    # HTTP/3 handler
│   ├── macula_protocol_codec.erl    # MessagePack/JSON
│   └── macula_protocol_auth.erl     # Session management
└── test/
```

### Acceptance Criteria

- [ ] curl can call RPC endpoints
- [ ] SSE subscription works
- [ ] MessagePack encoding/decoding
- [ ] OpenAPI spec validates
- [ ] 30+ tests for protocol

---

## v1.0.0 - Production Ready

**Goal:** Complete Cluster + Bridge + Registry for production use.

### Checklist

- [ ] All v0.13-v0.16 items complete
- [ ] E2E test suite with multi-Cluster scenarios
- [ ] Production deployment guide
- [ ] Monitoring and alerting documentation
- [ ] Performance benchmarks (target: 10k msg/sec/Cluster)
- [ ] Security audit of registry and federation

### Documentation

- `docs/ARCHITECTURE.md` - Updated conceptual model
- `docs/GLOSSARY.md` - Cluster, Seed, Bridge, SuperMesh
- `docs/FEDERATION.md` - How to federate Clusters
- `docs/REGISTRY.md` - Registry operation
- `docs/DEPLOYMENT.md` - Production guide

---

## v1.1.0+ - Ecosystem Contributions

### QUIC Distribution

- Integrate with Erlang distribution
- libcluster strategy
- Test with Horde, Mnesia, :pg
- Publish as separate hex package

### macula_crdt Hex Package

- Extract CRDT module as standalone library
- Publish to hex.pm
- Community contribution

---

## Design Decisions

### 1. No Raft Consensus

**Rationale:** Raft adds operational complexity for consistency guarantees Macula doesn't need.

**Impact:**
- No quorum management
- No leader election
- Nodes operate during partitions (AP in CAP)
- State converges eventually

### 2. CRDT Selection

| State Type | CRDT | Rationale |
|------------|------|-----------|
| Service Registry | OR-Set | Concurrent add/remove |
| Peer Membership | OR-Set | Peers join/leave |
| Node Metadata | LWW-Register | Last update wins |
| Subscriptions | OR-Set per topic | Topic subscribers |

### 3. Hierarchical DHT

- Local DHT per Cluster
- Bridge Nodes forward cross-Cluster queries
- No global DHT state
- Locality-first (most queries resolve locally)

### 4. Federated Registries

- Organizations control their own registry
- boot.macula.io is one option among many
- Trust is configurable per Cluster
- Capability-based security model

### 5. Protocol-First Multi-Language

- Define HTTP/3 protocol thoroughly
- Let community build SDKs
- BEAM-native remains first-class
- Non-BEAM via Protocol Gateway

---

## Gap Analysis

### Resolved

| Gap | Resolution |
|-----|------------|
| Cluster membership | Config overrides discovery |
| Bridge discovery | Layered (env → DNS → directory) |
| Capability enforcement | Layered defense model |

### Deferred to v1.1+

| Gap | Notes |
|-----|-------|
| CRDT garbage collection | Merkle tree + compaction |
| Federation PKI | Manual key exchange for v1.0 |
| Cross-Cluster addressing format | TBD |

---

## Risk Analysis

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| CRDT divergence | Medium | High | Anti-entropy protocol |
| Gossip storms | Low | High | Adaptive fanout |
| Bridge bottleneck | Medium | Medium | Multiple Bridges per Cluster |
| Security scan false negatives | High | High | Runtime defense layer |
| Scope creep | High | High | Strict phase gating |

---

## Opportunity Analysis

| Opportunity | Potential | Notes |
|-------------|-----------|-------|
| Edge-first federation | High | Unique differentiator |
| GitOps without K8s | High | Registry + Cluster Controller |
| Nerves integration | High | Cluster on embedded |
| Hosted registry (SaaS) | High | boot.macula.io revenue |
| Enterprise Bridge | High | Managed federation |

---

## Archived Plans

Previous planning documents preserved in `architecture/archive/`:
- `ROADMAP-pre-supermesh.md` - Previous roadmap (pre-December 2025)
- `reckon_db-integration.md` - Superseded by CRDT approach

---

## References

- Plan document: `~/.claude/plans/snuggly-finding-simon.md` (session artifact)
- Source analysis: 102 `.erl` files, 52 test files
- Current tests: 200+ passing

---

**Document Version:** 3.0 (SuperMesh Architecture)
**Last Updated:** 2025-12-01
