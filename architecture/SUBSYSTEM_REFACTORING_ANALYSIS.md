# Macula Subsystem Refactoring Analysis

**Created**: 2025-01-17
**Target Version**: v0.8.0
**Status**: Architecture Decision Record (ADR)

---

## Executive Summary

This document analyzes the proposed refactoring of macula into three distinct subsystems:
1. **Gateway System** (NAT traversal)
2. **Bootstrap System** (DHT bootstrap/discovery)
3. **Bridge System** (realm bridges)

**Recommendation**: **Partially adopt** with critical modifications. The proposed split conflates **roles** with **subsystems**, creating confusion. We propose a clearer architecture based on **deployment modes** and **functional subsystems**.

---

## Current Architecture Analysis

### Current Deployment (v0.7.30)

Macula currently has a **monolithic gateway** that handles:

1. **Client connection management** (`macula_gateway_clients.erl`)
2. **Pub/Sub routing** (`macula_gateway_pubsub.erl`, `macula_gateway_pubsub_router.erl`)
3. **RPC routing** (`macula_gateway_rpc.erl`, `macula_gateway_rpc_router.erl`)
4. **Mesh connection pooling** (`macula_gateway_mesh.erl`)
5. **DHT query forwarding** (`macula_gateway_dht.erl`)
6. **Bootstrap/registry functions** (DHT storage, peer discovery)
7. **NAT relay** (implicit - edge peers route through gateway)

### Problem: Role Confusion

The current `macula_gateway` module conflates **THREE DISTINCT ROLES**:

1. **Bootstrap Node** - Provides DHT bootstrap, peer discovery, service registry
2. **Relay Node** - Relays messages for edge peers behind NAT
3. **Bridge Node** - Bridges messages between different realms

**This creates several issues**:
- Can't deploy a "bootstrap-only" node without also having relay/bridge code
- Can't deploy a "relay-only" node without bootstrap/bridge code
- Configuration is complex (modes scattered across env vars)
- Testing is difficult (can't test subsystems in isolation)
- Scaling is inflexible (must scale entire gateway, not individual functions)

---

## Proposed Refactoring (User's Suggestion)

### User's Proposal

```
- macula_gateway_system  => NAT traversal ONLY
- macula_bootstrap_system => Bootstrap mesh nodes for Kademlia DHT
- macula_bridge_system    => Realm bridges
```

### Analysis of Proposal

**Problems**:

1. **Naming confusion**: "gateway" historically means relay/routing, not NAT traversal
2. **Missing role**: Where do "regular edge peers" fit? They're neither gateway nor bootstrap
3. **Unclear ownership**: Who manages mesh connections? Who handles DHT queries?
4. **NAT traversal scope**: NAT hole punching is a **capability**, not a standalone system

**What the proposal gets RIGHT**:

✅ Recognizes need for separation of concerns
✅ Identifies bootstrap as distinct from relay
✅ Identifies realm bridging as distinct functionality

---

## Recommended Architecture: Deployment Modes + Functional Subsystems

### Key Insight

**Separate DEPLOYMENT MODES from FUNCTIONAL SUBSYSTEMS**

**Deployment Modes** = How you run macula (what role it plays in the mesh)
**Functional Subsystems** = Reusable capabilities (libraries) that modes compose

---

## Deployment Modes (v0.8.0)

### 1. Bootstrap Node

**Purpose**: Provide DHT bootstrap and service registry
**Characteristics**:
- Publicly accessible (stable IP/domain)
- Stores DHT routing table
- No message relay (v0.8.0 goal)
- No realm bridging
- High availability (clustered)

**Functional subsystems used**:
- `macula_bootstrap_system` (NEW)
- `macula_routing` (DHT server)
- `macula_quic` (accept connections)
- `macula_protocol` (decode messages)

**Config**:
```erlang
{macula, [
    {mode, bootstrap},
    {realm, <<"macula.global">>},
    {listen_port, 4433},
    {dht_enabled, true},
    {relay_enabled, false},  % v0.8.0: bootstrap-only
    {bridge_enabled, false}
]}
```

**Docker**: `macula/bootstrap:0.8.0`

---

### 2. Relay Node (Gateway)

**Purpose**: Relay messages for edge peers behind NAT
**Characteristics**:
- Publicly accessible OR well-connected
- Participates in DHT (but not authoritative)
- Relays pub/sub and RPC messages
- May do NAT hole punching coordination (v0.8.0)

**Functional subsystems used**:
- `macula_relay_system` (NEW - renamed from gateway)
- `macula_nat_system` (NEW - v0.8.0)
- `macula_routing` (DHT client)
- `macula_pubsub` (relay pub/sub)
- `macula_rpc` (relay RPC)

**Config**:
```erlang
{macula, [
    {mode, relay},
    {realm, <<"macula.edge">>},
    {bootstrap_nodes, ["bootstrap.macula.io:4433"]},
    {relay_enabled, true},
    {nat_traversal_enabled, true},  % v0.8.0
    {bridge_enabled, false}
]}
```

**Docker**: `macula/relay:0.8.0`

---

### 3. Bridge Node

**Purpose**: Bridge messages between different realms
**Characteristics**:
- Joins multiple realms simultaneously
- Applies policy/filtering between realms
- May translate topics/procedures
- Audit logs cross-realm traffic

**Functional subsystems used**:
- `macula_bridge_system` (NEW)
- `macula_routing` (multi-realm DHT)
- `macula_pubsub` (multi-realm pub/sub)
- `macula_rpc` (multi-realm RPC)
- `macula_security` (ACLs, audit)

**Config**:
```erlang
{macula, [
    {mode, bridge},
    {realms, [
        {<<"macula.production">>, #{}},
        {<<"macula.staging">>, #{}}
    ]},
    {bridge_enabled, true},
    {bridge_policy, #{
        allow_topics => ["logs.*", "metrics.*"],
        deny_topics => ["secrets.*"]
    }}
]}
```

**Docker**: `macula/bridge:0.8.0`

---

### 4. Edge Peer (Default)

**Purpose**: Application node in the mesh
**Characteristics**:
- May be behind NAT
- Connects to bootstrap nodes
- May connect to relay nodes
- Does NOT relay for others (v0.7.x)
- MAY do direct P2P (v0.8.0+)

**Functional subsystems used**:
- `macula_peer_system` (already exists)
- `macula_nat_client` (NEW - v0.8.0)
- `macula_routing` (DHT client)
- `macula_pubsub` (pub/sub)
- `macula_rpc` (RPC)

**Config**:
```erlang
{macula, [
    {mode, edge},  % or just omit mode (default)
    {realm, <<"macula.myapp">>},
    {bootstrap_nodes, ["bootstrap.macula.io:4433"]},
    {nat_traversal_enabled, true}  % v0.8.0
]}
```

**Docker**: `macula/peer:0.8.0`

---

## Functional Subsystems (Libraries)

### Existing (Keep)

These are already well-separated:

- `macula_core` - Core types
- `macula_quic` - QUIC transport
- `macula_protocol` - Wire protocol
- `macula_routing` - Kademlia DHT
- `macula_pubsub` - Pub/sub logic
- `macula_rpc` - RPC logic
- `macula_security` - Auth/TLS
- `macula_discovery` - Node discovery (mDNS)

---

### New Subsystems (v0.8.0)

#### 1. `macula_bootstrap_system` (NEW)

**Purpose**: Bootstrap-specific functionality
**Responsibilities**:
- Accept client connections
- Store DHT routing table (authoritative)
- Answer DHT queries (`FIND_NODE`, `FIND_VALUE`, `STORE`)
- NO message relay (delegates to `macula_relay_system` if co-located)

**Modules**:
```erlang
macula_bootstrap_system.erl       - Bootstrap supervisor
macula_bootstrap_server.erl       - Bootstrap GenServer
macula_bootstrap_registry.erl     - Service registry
macula_bootstrap_health.erl       - Health checks
```

**Used by**: Bootstrap nodes only

---

#### 2. `macula_relay_system` (NEW - rename from gateway)

**Purpose**: Message relay for NAT-ed peers
**Responsibilities**:
- Relay pub/sub messages
- Relay RPC calls/replies
- Coordinate NAT hole punching (v0.8.0)
- Manage relay connections

**Modules**:
```erlang
macula_relay_system.erl           - Relay supervisor
macula_relay_server.erl           - Relay GenServer
macula_relay_pubsub.erl           - Pub/sub relay
macula_relay_rpc.erl              - RPC relay
macula_relay_connections.erl      - Connection pool
```

**Used by**: Relay nodes (and optionally bootstrap nodes if co-located)

---

#### 3. `macula_nat_system` (NEW - v0.8.0)

**Purpose**: NAT traversal and hole punching
**Responsibilities**:
- Discover public address (via gateway)
- Detect NAT type
- Coordinate hole punching
- Upgrade relay → direct connections

**Modules**:
```erlang
macula_nat_system.erl             - NAT supervisor
macula_nat_discovery.erl          - Public address discovery
macula_nat_detector.erl           - NAT type detection
macula_nat_hole_punch.erl         - Hole punching logic
macula_nat_upgrade.erl            - Connection upgrade
```

**Used by**: ALL nodes (bootstrap, relay, edge)

**See**: `architecture/NAT_TRAVERSAL_ROADMAP.md` for detailed design

---

#### 4. `macula_bridge_system` (NEW - future)

**Purpose**: Realm bridging
**Responsibilities**:
- Join multiple realms
- Route messages between realms
- Apply ACL policies
- Audit cross-realm traffic
- Translate topics/procedures (optional)

**Modules**:
```erlang
macula_bridge_system.erl          - Bridge supervisor
macula_bridge_server.erl          - Bridge GenServer
macula_bridge_policy.erl          - ACL enforcement
macula_bridge_translator.erl      - Topic translation
macula_bridge_audit.erl           - Audit logging
```

**Used by**: Bridge nodes only

---

## Migration Path from v0.7.30 → v0.8.0

### Phase 1: Extract Bootstrap System (Week 1-2)

**Goal**: Make `macula_gateway` bootstrap-only by default

**Steps**:
1. Create `macula_bootstrap_system` as new OTP application
2. Move bootstrap-specific code from `macula_gateway`:
   - DHT storage logic
   - Service registry
   - Health checks
3. Add `{mode, bootstrap}` config support
4. Test bootstrap-only deployment

**Result**: Can deploy `macula/bootstrap:0.8.0` Docker image

---

### Phase 2: Extract Relay System (Week 3-4)

**Goal**: Separate relay functionality from bootstrap

**Steps**:
1. Create `macula_relay_system` as new OTP application
2. Move relay code from `macula_gateway`:
   - `macula_gateway_pubsub.erl` → `macula_relay_pubsub.erl`
   - `macula_gateway_rpc.erl` → `macula_relay_rpc.erl`
   - `macula_gateway_mesh.erl` → `macula_relay_connections.erl`
3. Add `{mode, relay}` config support
4. Test relay-only deployment (no DHT storage)

**Result**: Can deploy `macula/relay:0.8.0` Docker image

---

### Phase 3: Implement NAT System (Week 5-8)

**Goal**: Add NAT traversal capabilities

**Steps**:
1. Create `macula_nat_system` as new OTP application
2. Implement modules per `NAT_TRAVERSAL_ROADMAP.md`:
   - `macula_nat_discovery.erl`
   - `macula_nat_hole_punch.erl`
   - `macula_nat_upgrade.erl`
3. Integrate with `macula_relay_system` for coordination
4. Test hole punching in Docker environment

**Result**: Edge peers can establish direct P2P connections

**See**: `architecture/NAT_TRAVERSAL_ROADMAP.md`

---

### Phase 4: Deprecate Old Gateway (Week 9-10)

**Goal**: Remove legacy `macula_gateway` code

**Steps**:
1. Migrate all applications to new mode-based config
2. Mark `macula_gateway` as deprecated
3. Update documentation
4. Remove old code in v0.9.0

**Result**: Clean architecture with clear separation

---

## Configuration Examples (v0.8.0)

### Bootstrap-Only Node

```erlang
%% config/sys.config
[
  {macula, [
    {mode, bootstrap},
    {realm, <<"macula.bootstrap">>},
    {listen_port, 4433},
    {listen_address, {0,0,0,0}},  % Public

    %% Bootstrap config
    {dht_storage_enabled, true},
    {service_registry_enabled, true},

    %% Disable relay (bootstrap-only)
    {relay_enabled, false},
    {bridge_enabled, false}
  ]}
].
```

**Start**: `./bin/macula start`
**Docker**: `docker run -p 4433:4433/udp macula/bootstrap:0.8.0`

---

### Relay Node (with NAT traversal)

```erlang
%% config/sys.config
[
  {macula, [
    {mode, relay},
    {realm, <<"macula.relay">>},
    {listen_port, 4433},

    %% Bootstrap discovery
    {bootstrap_nodes, [
      "bootstrap1.macula.io:4433",
      "bootstrap2.macula.io:4433"
    ]},

    %% Relay config
    {relay_enabled, true},
    {max_relay_connections, 10000},

    %% NAT traversal (v0.8.0)
    {nat_traversal_enabled, true},
    {hole_punch_timeout, 5000},

    %% Not a bridge
    {bridge_enabled, false}
  ]}
].
```

---

### Edge Peer (Application Node)

```erlang
%% config/sys.config
[
  {macula, [
    {mode, edge},  % Default if omitted
    {realm, <<"macula.myapp">>},

    %% Bootstrap discovery
    {bootstrap_nodes, ["bootstrap.macula.io:4433"]},

    %% NAT traversal (v0.8.0)
    {nat_traversal_enabled, true},

    %% Not a relay or bridge
    {relay_enabled, false},
    {bridge_enabled, false}
  ]}
].
```

---

### Bridge Node (Multi-Realm)

```erlang
%% config/sys.config
[
  {macula, [
    {mode, bridge},

    %% Join multiple realms
    {realms, [
      #{
        realm => <<"macula.production">>,
        bootstrap_nodes => ["prod-bootstrap.macula.io:4433"]
      },
      #{
        realm => <<"macula.staging">>,
        bootstrap_nodes => ["staging-bootstrap.macula.io:4433"]
      }
    ]},

    %% Bridge policy
    {bridge_policy, #{
      allow_topics => ["logs.*", "metrics.*"],
      deny_topics => ["secrets.*", "internal.*"],
      rate_limit => 1000  % msgs/sec per realm pair
    }},

    %% Bridge-specific
    {bridge_enabled, true},
    {bridge_audit_log, "/var/log/macula/bridge-audit.log"}
  ]}
].
```

---

## Comparison: Old vs New Architecture

### Before (v0.7.30) - Monolithic Gateway

```
macula_gateway (1 process)
├── Client management
├── Pub/Sub routing
├── RPC routing
├── DHT queries
├── Service registry
├── NAT relay (implicit)
└── (No realm bridging yet)
```

**Problems**:
- Can't deploy bootstrap-only (always includes relay code)
- Can't scale relay independently from bootstrap
- Configuration is scattered
- Testing is monolithic

---

### After (v0.8.0) - Mode-Based Subsystems

```
Deployment Modes:
├── bootstrap     => macula_bootstrap_system + macula_routing (DHT server)
├── relay         => macula_relay_system + macula_nat_system
├── bridge        => macula_bridge_system (multi-realm)
└── edge (default)=> macula_peer_system + macula_nat_system (client)

Functional Subsystems (Libraries):
├── macula_bootstrap_system  (NEW)
├── macula_relay_system      (NEW - renamed from gateway)
├── macula_nat_system        (NEW - v0.8.0)
├── macula_bridge_system     (NEW - future)
├── macula_routing           (DHT - existing)
├── macula_pubsub            (existing)
├── macula_rpc               (existing)
└── macula_quic              (existing)
```

**Benefits**:
✅ Clear separation: bootstrap ≠ relay ≠ bridge
✅ Can deploy each mode independently
✅ Can scale each subsystem independently
✅ Testing is modular
✅ Configuration is mode-specific

---

## Answering Original Question

> Would this be a meaningful refactor?

**YES, but with modifications.**

**User's original proposal**:
```
- macula_gateway_system  => NAT traversal ONLY      # ❌ Too narrow
- macula_bootstrap_system => bootstrap for DHT      # ✅ Correct
- macula_bridge_system    => realm bridges          # ✅ Correct
```

**Our recommendation**:
```
- macula_bootstrap_system => DHT bootstrap/registry  # ✅ Keep
- macula_relay_system     => Message relay (rename gateway) # ✅ Add
- macula_nat_system       => NAT traversal (all nodes) # ✅ Add
- macula_bridge_system    => Realm bridging         # ✅ Keep (future)
```

**Key differences**:
1. **NAT system is NOT a deployment mode** - it's a capability used by all modes
2. **Gateway renamed to relay** - clearer semantics
3. **Added deployment modes** - bootstrap, relay, bridge, edge (default)
4. **Functional subsystems ≠ deployment roles** - modes compose subsystems

---

## Other Architectural Possibilities

### Alternative 1: Plugin Architecture

```
macula_core (minimal runtime)
├── Plugins loaded based on mode:
│   ├── bootstrap_plugin.so
│   ├── relay_plugin.so
│   ├── nat_plugin.so
│   └── bridge_plugin.so
```

**Pros**: Smaller binaries, dynamic loading
**Cons**: Complexity, NIF safety, not idiomatic Erlang

**Verdict**: ❌ Over-engineered for Erlang/BEAM

---

### Alternative 2: Microservices (Separate Processes)

```
macula-bootstrap (separate binary)
macula-relay (separate binary)
macula-bridge (separate binary)
```

**Pros**: Complete isolation, can use different languages
**Cons**: Network overhead, operational complexity, violates BEAM philosophy

**Verdict**: ❌ Goes against BEAM/OTP principles

---

### Alternative 3: Keep Monolith, Add Modes (Our Choice)

```
macula (single binary)
├── Mode-based supervision tree
└── Load only required subsystems
```

**Pros**: Idiomatic Erlang, flexible deployment, shared code
**Cons**: Slightly larger binary (but negligible)

**Verdict**: ✅ Best fit for Erlang/OTP

---

## Recommended Next Steps

### Immediate (v0.8.0 - Q1 2025)

1. **Create architecture ADR** (this document) ✅
2. **Extract `macula_bootstrap_system`** (2 weeks)
3. **Extract `macula_relay_system`** (2 weeks)
4. **Implement `macula_nat_system`** (4 weeks)
5. **Update configuration system** (1 week)
6. **Update documentation** (1 week)
7. **Deprecate `macula_gateway`** (mark deprecated)

**Total**: ~10 weeks (2.5 months)

---

### Future (v0.9.0 - Q3 2025)

1. **Implement `macula_bridge_system`**
2. **Add STUN/TURN support** (per NAT roadmap)
3. **Remove deprecated `macula_gateway` code**

---

## Decision

**APPROVED with modifications**:

✅ Create `macula_bootstrap_system` (bootstrap-only deployment)
✅ Create `macula_relay_system` (rename from gateway)
✅ Create `macula_nat_system` (NAT traversal for all modes)
✅ Create `macula_bridge_system` (multi-realm bridging)
✅ Introduce deployment modes: bootstrap, relay, bridge, edge

❌ Do NOT make "gateway" synonymous with "NAT traversal"
❌ Do NOT split into separate binaries (keep single release)

---

**Last Updated**: 2025-01-17
**Status**: Architecture Decision Approved
**Target**: v0.8.0 (Q1-Q2 2025)
