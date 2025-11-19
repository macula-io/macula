# Macula Full Supervision Tree

## Complete Supervision Hierarchy (v0.8.5 Always-On Architecture)

**Date:** 2025-11-18
**Version:** v0.8.5+
**Architecture:** Always-On (all capabilities enabled on every node)
**Based on:** v0.8.5 architectural foundations release

---

## Visual Tree

```
macula (OTP application)
└── macula_root [one_for_one]
    │
    ├── macula_routing_server (worker)
    │   └── Core DHT infrastructure (always on)
    │
    ├── macula_bootstrap_system [one_for_one] (always on)
    │   ├── macula_bootstrap_server (worker)
    │   │   └── DHT queries, routing table storage
    │   │
    │   ├── macula_bootstrap_registry (worker)
    │   │   └── Service registry (advertised RPC endpoints)
    │   │
    │   └── macula_bootstrap_health (worker)
    │       └── System health monitoring
    │
    ├── macula_gateway_system [rest_for_one] (always on)
    │   ├── macula_gateway_health (worker)
    │   │   └── Health check HTTP server
    │   │
    │   ├── macula_gateway_diagnostics (worker)
    │   │   └── Diagnostics service
    │   │
    │   ├── macula_gateway_quic_server (worker)
    │   │   └── QUIC transport layer (UDP listener)
    │   │
    │   ├── macula_gateway (worker)
    │   │   └── Message routing coordinator
    │   │
    │   └── macula_gateway_workers_sup [rest_for_one]
    │       ├── macula_gateway_clients (worker)
    │       │   └── Client connection tracking and stream management
    │       │
    │       ├── macula_gateway_pubsub (worker)
    │       │   └── Pub/Sub message routing with wildcards
    │       │
    │       ├── macula_gateway_rpc (worker)
    │       │   └── RPC handler registration and management
    │       │
    │       └── macula_gateway_mesh (worker)
    │           └── Mesh connection pooling (LRU, max 1000 connections)
    │
    └── macula_peers_sup [simple_one_for_one] (always on)
        └── (dynamically spawned macula_peer_system instances)
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

**Children (v0.8.5 - always-on):**
1. `macula_routing_server` - Core DHT infrastructure (always on)
2. `macula_bootstrap_system` - Bootstrap services (always on)
3. `macula_gateway_system` - Gateway services (always on)
4. `macula_peers_sup` - Dynamic peer connections supervisor (always on)

**Startup Sequence:**
- TLS certificates auto-generated if missing (stable Node ID)
- All subsystems start unconditionally
- Beautiful startup banner displays configuration
- Node ready for P2P mesh participation

**Fault Isolation:**
- Routing server crash → Only routing restarts
- Bootstrap system crash → Only bootstrap restarts
- Gateway system crash → Only gateway restarts
- Peers supervisor crash → Only peers supervisor restarts (existing connections preserved)
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

### simple_one_for_one
- **Purpose:** Template-based dynamic child spawning
- **Restart:** Each child has its own restart strategy (not collective)
- **Use case:** Managing many similar children (peer connections)

**Used by:**
- `macula_peers_sup` - Dynamic peer connections (v0.8.5+)

---

## v0.8.5 Always-On Architecture

**As of v0.8.5, mode-based configuration has been removed.**

### Always-On Configuration
```erlang
macula_root [one_for_one]
├── macula_routing_server (always on)
├── macula_bootstrap_system (always on)
├── macula_gateway_system (always on)
└── macula_peers_sup (always on)
```

**Every node has ALL capabilities:**
- ✅ DHT routing (core infrastructure)
- ✅ Bootstrap service (helps new peers join)
- ✅ Gateway with QUIC listener (accepts connections)
- ✅ Dynamic peer management (on-demand connections)

**Benefits:**
- Zero configuration required
- Simplified deployment (no mode selection)
- True P2P mesh (nodes connect on-demand)
- TLS auto-generated (stable Node ID)

**Environment Variables (v0.8.5+):**
```bash
MACULA_QUIC_PORT=4433                  # QUIC listener port
MACULA_REALM=my.realm                  # Realm name
MACULA_BOOTSTRAP_URL=https://...       # Optional bootstrap peer
HEALTH_PORT=8080                       # Health check port
MACULA_CERT_PATH=/path/to/cert.pem    # Optional (auto-generated)
MACULA_KEY_PATH=/path/to/key.pem      # Optional (auto-generated)
```

### Legacy Mode Configuration (v0.8.4 and earlier)

**DEPRECATED:** Mode-based configuration was removed in v0.8.5.

For historical reference, v0.8.4 supported these modes:
- `bootstrap` - DHT bootstrap only
- `edge` - Peer-only (no incoming connections)
- `gateway` - Gateway routing only
- `hybrid` - All capabilities (equivalent to v0.8.5 default)

---

## Process Count Estimation

### v0.8.5 Always-On Architecture:
```
1   macula_root
1   macula_routing_server
1   macula_bootstrap_system
3   bootstrap workers (server, registry, health)
1   macula_gateway_system
5   gateway workers (health, diagnostics, quic, gateway, workers_sup)
4   business logic workers (clients, pubsub, rpc, mesh)
1   macula_peers_sup
---
17  processes (base)

+ 4 per peer connection (peer_system with 4 handlers)
```

**Example:** Node with 10 peer connections = 17 + (10 × 4) = **57 processes**

**Example:** Node with 100 peer connections = 17 + (100 × 4) = **417 processes**

### Legacy v0.8.4 (for reference):

**Minimal (edge mode):** 2 processes
**Bootstrap only:** 6 processes
**Gateway only:** 12 processes
**Hybrid:** 16 + (4 × peers) processes

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

### Environment Variables (v0.8.5+)
```bash
# QUIC configuration
MACULA_QUIC_PORT=4433                 # QUIC listener port
MACULA_REALM=macula.arcade            # Realm name
HEALTH_PORT=8080                      # Health check HTTP port

# TLS certificates (auto-generated if not provided)
MACULA_CERT_PATH=/var/lib/macula/cert.pem    # Optional
MACULA_KEY_PATH=/var/lib/macula/key.pem      # Optional

# Bootstrap (optional - for joining existing mesh)
MACULA_BOOTSTRAP_URL=https://bootstrap:4433

# Legacy support (v0.8.4 compatibility)
GATEWAY_PORT=4433                     # Falls back to MACULA_QUIC_PORT
```

### Application Config (sys.config) - v0.8.5+
```erlang
{macula, [
    {quic_port, 4433},                 % QUIC listener
    {realm, <<"macula.arcade">>},      % Realm name
    {health_port, 8080},               % Health check port
    {bootstrap_health_interval, 60000}, % Health check interval (ms)

    % TLS configuration (optional - auto-generated if missing)
    {cert_path, "/var/lib/macula/cert.pem"},
    {key_path, "/var/lib/macula/key.pem"},
    {cert_validity_days, 3650},        % 10 years
    {cert_key_bits, 2048}              % RSA key size
]}
```

### Legacy Configuration (v0.8.4)
```erlang
{macula, [
    {mode, hybrid},                    % DEPRECATED in v0.8.5
    {start_gateway, true},             % DEPRECATED in v0.8.5
    {gateway_port, 4433},              % Use quic_port in v0.8.5
    {gateway_realm, <<"macula.arcade">>} % Use realm in v0.8.5
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

**v0.8.5 Always-On Architecture - Every node runs:**
- ✅ Core DHT routing (always on)
- ✅ Bootstrap service (helps new peers join)
- ✅ Gateway with QUIC listener (accepts connections)
- ✅ Pub/Sub routing (message forwarding)
- ✅ RPC handling (service registry + routing)
- ✅ Mesh connections (connection pooling)
- ✅ Per-peer handlers (connection + pubsub + rpc + advertisements)

**Total processes:** 17 base + 4 per connected peer

**Zero configuration:**
- TLS certificates auto-generated on first boot
- Stable Node ID derived from public key (SHA-256)
- No mode selection needed

**Fault tolerance:** Proper OTP supervision with `one_for_one` and `rest_for_one` strategies

**Scalability:** Bounded pools prevent unbounded memory growth (see memory management docs)
