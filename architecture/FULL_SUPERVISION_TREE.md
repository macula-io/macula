# Macula Full Supervision Tree

## Complete Supervision Hierarchy for a Fully-Capable Peer

**Date:** 2025-11-18
**Mode:** `hybrid` (all capabilities enabled: bootstrap + gateway + peer)
**Based on:** Current codebase after subsystem reorganization

---

## Visual Tree

```
macula (OTP application)
└── macula_root [one_for_one]
    │
    ├── macula_routing_server (worker)
    │   └── Core DHT infrastructure (always running)
    │
    ├── macula_bootstrap_system [one_for_one] (optional: bootstrap/hybrid modes)
    │   ├── macula_bootstrap_server (worker)
    │   │   └── DHT queries, routing table storage
    │   │
    │   ├── macula_bootstrap_registry (worker)
    │   │   └── Service registry (advertised RPC endpoints)
    │   │
    │   └── macula_bootstrap_health (worker)
    │       └── System health monitoring
    │
    └── macula_gateway_system [rest_for_one] (optional: gateway/hybrid modes)
        ├── macula_gateway_health (worker)
        │   └── Health check HTTP server
        │
        ├── macula_gateway_diagnostics (worker)
        │   └── Diagnostics service
        │
        ├── macula_gateway_quic_server (worker)
        │   └── QUIC transport layer (UDP listener)
        │
        ├── macula_gateway (worker)
        │   └── Message routing coordinator
        │
        └── macula_gateway_workers_sup [rest_for_one]
            ├── macula_gateway_clients (worker)
            │   └── Client connection tracking and stream management
            │
            ├── macula_gateway_pubsub (worker)
            │   └── Pub/Sub message routing with wildcards
            │
            ├── macula_gateway_rpc (worker)
            │   └── RPC handler registration and management
            │
            └── macula_gateway_mesh (worker)
                └── Mesh connection pooling (LRU, max 1000 connections)
```

---

## Peer Connections (Per Client/Peer)

When a peer connects (either as client to gateway, or peer-to-peer), it gets its own supervision tree:

```
macula_peer_system [rest_for_one] (per connection)
├── macula_connection (worker)
│   └── QUIC connection lifecycle (transport layer)
│
├── macula_pubsub_handler (worker)
│   └── Pub/sub operations for this peer
│
├── macula_rpc_handler (worker)
│   └── RPC operations for this peer
│
└── macula_advertisement_manager (worker)
    └── DHT service advertisements for this peer
```

**Note:** Each peer connection gets its own `macula_peer_system` supervisor with dedicated handlers.

---

## Detailed Component Breakdown

### 1. Application Root (macula_root)

**Strategy:** `one_for_one`
**Intensity:** 10 restarts in 5 seconds

**Children (mode-dependent):**
1. `macula_routing_server` - Always started (core DHT)
2. `macula_bootstrap_system` - Only in `bootstrap` or `hybrid` modes
3. `macula_gateway_system` - Only in `gateway` or `hybrid` modes

**Fault Isolation:**
- Routing server crash → Only routing restarts
- Bootstrap system crash → Only bootstrap restarts
- Gateway system crash → Only gateway restarts
- Each subsystem is isolated

---

### 2. Bootstrap System (macula_bootstrap_system)

**Strategy:** `one_for_one`
**Intensity:** 10 restarts in 60 seconds
**Started when:** `mode ∈ {bootstrap, hybrid}`

**Children (all workers):**
1. **macula_bootstrap_server**
   - Handles DHT queries (FIND_NODE, FIND_VALUE, STORE)
   - Stores routing table entries
   - Tracks query statistics

2. **macula_bootstrap_registry**
   - Service registry for advertised RPC endpoints
   - Generic key/value store for DHT
   - Provides lookup API

3. **macula_bootstrap_health**
   - System health monitoring
   - Periodic health checks
   - Unhealthy service detection

**Purpose:**
- Provides DHT bootstrap for new peers joining the mesh
- Acts as initial contact point (well-known peer)
- Does NOT route messages (that's gateway's job if enabled)

---

### 3. Gateway System (macula_gateway_system)

**Strategy:** `rest_for_one`
**Intensity:** 10 restarts in 60 seconds
**Started when:** `mode ∈ {gateway, hybrid}` AND `start_gateway = true`

**Children (in dependency order):**

#### Child 1: macula_gateway_health
- **Type:** Worker
- **Purpose:** Health check HTTP server
- **Port:** Configurable (default 8080)
- **Endpoint:** `/health`
- **Crash impact:** Restarts health + diagnostics + quic_server + gateway + workers_sup

#### Child 2: macula_gateway_diagnostics
- **Type:** Worker
- **Purpose:** Diagnostics service
- **Provides:** System metrics, debugging info
- **Crash impact:** Restarts diagnostics + quic_server + gateway + workers_sup

#### Child 3: macula_gateway_quic_server
- **Type:** Worker
- **Purpose:** QUIC transport layer (UDP listener)
- **Port:** Configurable (default 9443, or MACULA_QUIC_PORT)
- **Protocol:** HTTP/3 over QUIC
- **TLS:** Required (cert/key files)
- **Crash impact:** Restarts quic_server + gateway + workers_sup

#### Child 4: macula_gateway
- **Type:** Worker
- **Purpose:** Message routing coordinator
- **Role:** Facade/orchestrator that delegates to worker children
- **Crash impact:** Restarts gateway + workers_sup

#### Child 5: macula_gateway_workers_sup
- **Type:** Supervisor
- **Strategy:** `rest_for_one`
- **Purpose:** Supervises business logic workers
- **Crash impact:** Restarts workers_sup only (quic_server and gateway continue)

---

### 4. Gateway Workers (macula_gateway_workers_sup)

**Strategy:** `rest_for_one`
**Intensity:** 10 restarts in 60 seconds

**Children (in dependency order):**

#### Child 1: macula_gateway_clients
- **Type:** Worker
- **Purpose:** Client connection and stream tracking
- **Max clients:** 10,000 (configurable, with backpressure)
- **Tracks:** ClientID → {ConnPid, Streams, LastSeen}
- **Stream cleanup:** Coordinated map cleanup on disconnect
- **Crash impact:** Restarts all workers (foundational)

#### Child 2: macula_gateway_pubsub
- **Type:** Worker
- **Purpose:** Pub/Sub message routing with wildcards
- **Supports:** Topic hierarchies (e.g., `game.events.*`)
- **Wildcard matching:** Prefix matching for subscriptions
- **Crash impact:** Restarts pubsub + rpc + mesh

#### Child 3: macula_gateway_rpc
- **Type:** Worker
- **Purpose:** RPC handler registration and management
- **Registry:** Procedure → HandlerPid mapping
- **Routing:** Local handlers + DHT discovery for remote
- **Crash impact:** Restarts rpc + mesh

#### Child 4: macula_gateway_mesh
- **Type:** Worker
- **Purpose:** Mesh connection pooling and management
- **Pool size:** Max 1,000 connections (LRU eviction)
- **Connects to:** Other gateways/peers in the mesh
- **Purpose:** Efficient connection reuse for multi-hop routing
- **Crash impact:** Restarts mesh only

---

### 5. Peer System (macula_peer_system)

**Strategy:** `rest_for_one`
**Intensity:** 10 restarts in 60 seconds
**Instantiation:** One per peer connection (client or P2P peer)

**Children (in dependency order):**

#### Child 1: macula_connection
- **Type:** Worker
- **Purpose:** QUIC connection lifecycle (transport layer)
- **Role:** Low-level QUIC send/receive, stream management
- **Crash impact:** Restarts all peer handlers (foundational)

#### Child 2: macula_pubsub_handler
- **Type:** Worker
- **Purpose:** Pub/sub operations for this specific peer
- **Operations:** Subscribe, unsubscribe, publish
- **Crash impact:** Restarts pubsub + rpc + advertisement

#### Child 3: macula_rpc_handler
- **Type:** Worker
- **Purpose:** RPC operations for this specific peer
- **Operations:** Register handler, call RPC, handle requests
- **Pending calls:** Tracked with correlation IDs and timeouts
- **Caller monitoring:** Immediate cleanup on caller death
- **Crash impact:** Restarts rpc + advertisement

#### Child 4: macula_advertisement_manager
- **Type:** Worker
- **Purpose:** DHT service advertisements for this peer
- **Operations:** Advertise service, unadvertise, list active
- **TTL:** 5 minutes (configurable)
- **Cleanup:** Automatic removal of expired advertisements
- **Crash impact:** Restarts advertisement only

---

## Supervision Strategies Explained

### one_for_one
- **Crash:** Child N crashes
- **Restart:** Only child N restarts
- **Use case:** Independent children, no dependencies

**Used by:**
- `macula_root` - Core subsystems are independent
- `macula_bootstrap_system` - Bootstrap workers are independent

### rest_for_one
- **Crash:** Child N crashes
- **Restart:** Child N + all children after N restart
- **Use case:** Ordered dependencies (later children depend on earlier ones)

**Used by:**
- `macula_gateway_system` - Dependency chain: health → diagnostics → quic → gateway → workers
- `macula_gateway_workers_sup` - clients is foundational, others depend on it
- `macula_peer_system` - connection is foundational, handlers depend on it

---

## Mode-Based Configuration

### Mode: `bootstrap`
```erlang
macula_root
├── macula_routing_server ✓
├── macula_bootstrap_system ✓
└── macula_gateway_system ✗ (disabled)
```

**Use case:** Lightweight DHT bootstrap node

---

### Mode: `edge` (peer-only)
```erlang
macula_root
├── macula_routing_server ✓
├── macula_bootstrap_system ✗
└── macula_gateway_system ✗
```

**Use case:** Pure P2P peer (connects to others, doesn't accept incoming)

---

### Mode: `gateway`
```erlang
macula_root
├── macula_routing_server ✓
├── macula_bootstrap_system ✗
└── macula_gateway_system ✓
```

**Use case:** Message routing/relay node (no bootstrap capability)

---

### Mode: `hybrid` (fully-capable peer)
```erlang
macula_root
├── macula_routing_server ✓
├── macula_bootstrap_system ✓
└── macula_gateway_system ✓
    └── (full gateway tree with all workers)
```

**Use case:** All capabilities enabled
- DHT bootstrap ✓
- Gateway routing ✓
- Mesh connections ✓
- Can act as bootstrap, gateway, or relay as needed

---

## Process Count Estimation

### Minimal (edge mode):
```
1   macula_root
1   macula_routing_server
---
2   processes
```

### Bootstrap only:
```
1   macula_root
1   macula_routing_server
1   macula_bootstrap_system
3   bootstrap workers (server, registry, health)
---
6   processes
```

### Gateway only:
```
1   macula_root
1   macula_routing_server
1   macula_gateway_system
5   gateway workers (health, diagnostics, quic, gateway, workers_sup)
4   business logic workers (clients, pubsub, rpc, mesh)
---
12  processes
```

### Hybrid (fully-capable):
```
1   macula_root
1   macula_routing_server
1   macula_bootstrap_system
3   bootstrap workers
1   macula_gateway_system
5   gateway workers
4   business logic workers
---
16  processes (base)

+ 4 per connected peer (peer_system with 4 handlers)
```

**Example:** Hybrid node with 10 peer connections = 16 + (10 × 4) = **56 processes**

---

## Fault Tolerance Examples

### Scenario 1: Gateway worker crashes
```
1. macula_gateway_pubsub crashes
2. rest_for_one restarts: pubsub + rpc + mesh
3. Clients continue (no disconnection)
4. quic_server continues (no listening disruption)
5. Recovery: ~100ms (restart + state rebuild)
```

### Scenario 2: QUIC server crashes
```
1. macula_gateway_quic_server crashes
2. rest_for_one restarts: quic_server + gateway + workers_sup
3. All workers restart
4. New QUIC listener started
5. Clients must reconnect (UDP listener changed)
6. Recovery: ~500ms (full gateway restart)
```

### Scenario 3: Bootstrap system crashes
```
1. macula_bootstrap_system crashes
2. one_for_one restarts: bootstrap_system only
3. Gateway continues (independent)
4. Routing server continues (independent)
5. Recovery: ~200ms (restart 3 bootstrap workers)
```

### Scenario 4: Peer connection handler crashes
```
1. macula_rpc_handler (for peer X) crashes
2. rest_for_one restarts: rpc_handler + advertisement_manager
3. Peer X's connection and pubsub_handler continue
4. Peer X doesn't need to reconnect
5. Recovery: ~50ms (restart 2 handlers)
```

---

## Memory Management

Each subsystem implements bounded data structures:

1. **Gateway mesh pool:** Max 1,000 connections (LRU eviction)
2. **Gateway clients:** Max 10,000 clients (backpressure)
3. **Service registry:** 5-min TTL with automatic cleanup every 60s
4. **RPC pending calls:** Monitored caller PIDs (cleanup on death)
5. **Stream tracking:** Coordinated cleanup on client disconnect

**See:** `architecture/memory_management/` for comprehensive details

---

## Configuration

### Environment Variables
```bash
# Mode selection
MACULA_MODE=hybrid                    # bootstrap|edge|gateway|hybrid

# Gateway configuration
MACULA_START_GATEWAY=true             # Override mode-based default
MACULA_QUIC_PORT=4433                 # QUIC listener port
MACULA_REALM=macula.arcade            # Realm name
HEALTH_PORT=8080                      # Health check HTTP port

# TLS certificates
TLS_CERT_FILE=/path/to/cert.pem
TLS_KEY_FILE=/path/to/key.pem

# Bootstrap
MACULA_BOOTSTRAP_URL=https://bootstrap:4433  # For edge/gateway modes
```

### Application Config (sys.config)
```erlang
{macula, [
    {mode, hybrid},                    % bootstrap|edge|gateway|hybrid
    {start_gateway, true},             % Override
    {gateway_port, 4433},              % QUIC listener
    {gateway_realm, <<"macula.arcade">>},
    {health_port, 8080},
    {bootstrap_health_interval, 60000} % Health check interval (ms)
]}
```

---

## Related Documents

- `architecture/memory_management/` - Memory leak prevention and bounded data structures
- `architecture/dht_routed_rpc.md` - DHT-routed RPC design
- `architecture/NAT_TRAVERSAL_ROADMAP.md` - v0.8.0/v0.9.0 P2P connectivity
- `architecture/v0.9.0-CONSISTENCY-CONCERNS.md` - Correlation and consistency patterns
- `CODE_REVIEW_REPORT.md` - Code quality and health score

---

## Summary

**Fully-capable peer (hybrid mode) runs:**
- ✅ Core DHT routing (always)
- ✅ Bootstrap service (helps new peers join)
- ✅ Gateway with QUIC listener (accepts connections)
- ✅ Pub/Sub routing (message forwarding)
- ✅ RPC handling (service registry + routing)
- ✅ Mesh connections (connection pooling)
- ✅ Per-peer handlers (connection + pubsub + rpc + advertisements)

**Total processes:** 16 base + 4 per connected peer

**Fault tolerance:** Proper OTP supervision with `one_for_one` and `rest_for_one` strategies

**Scalability:** Bounded pools prevent unbounded memory growth (see memory management docs)
