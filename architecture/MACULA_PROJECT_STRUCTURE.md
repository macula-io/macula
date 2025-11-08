# Macula HTTP/3 Mesh - Project Structure and Libraries

**Actual Erlang/Elixir projects and libraries to build**

**Created**: 2025-01-08
**Status**: Planning Document

---

## Overview

This document outlines the actual Erlang/Elixir libraries, applications, and projects that need to be created to implement the Macula HTTP/3 Mesh platform.

---

## Repository Organization Strategy

### Option A: Monorepo (Recommended for Initial Development)

```
macula/
├── apps/
│   ├── macula_core/           # Core protocols and types
│   ├── macula_quic/           # QUIC transport layer
│   ├── macula_protocol/       # Wire protocol (framing, encoding)
│   ├── macula_membership/     # SWIM membership
│   ├── macula_routing/        # Kademlia DHT routing
│   ├── macula_topology/       # Mesh topology management
│   ├── macula_pubsub/         # Pub/sub implementation
│   ├── macula_rpc/            # RPC implementation
│   ├── macula_gateway/        # Cross-realm gateway
│   ├── macula_discovery/      # Node discovery
│   ├── macula_security/       # Auth, ACLs, certificates
│   └── macula/                # Main application (umbrella)
├── rebar.config               # Rebar3 umbrella config
├── mix.exs                    # Mix umbrella config (if supporting Elixir)
└── README.md
```

**Benefits**:
- Easy cross-library development
- Shared dependencies
- Single release
- Atomic commits across components
- Simplified CI/CD

### Option B: Multi-repo (For Maturity/Modularity)

Separate repositories for each library (similar to Erlang/OTP structure).

**Benefits**:
- Independent versioning
- Smaller, focused repos
- Can use different libraries independently
- Clear boundaries

**Recommendation**: Start with **Option A (monorepo)**, split later if needed.

---

## Core Libraries (Required for MVP)

### 1. `macula_core`

**Purpose**: Core types, protocols, and shared utilities.

**Modules**:
```erlang
%% Core types
macula_types.erl           - Common type definitions
macula_node.erl            - Node identity and metadata
macula_realm.erl           - Realm management
macula_topic.erl           - Topic parsing and validation

%% Utilities
macula_time.erl            - Time utilities
macula_id.erl              - ID generation (SHA-256 node IDs)
macula_uri.erl             - Macula URI parsing (macula://realm/node)
```

**Dependencies**: None (pure Erlang)

**Rebar3 Config**:
```erlang
{application, macula_core, [
    {description, "Macula core types and protocols"},
    {vsn, "0.1.0"},
    {modules, []},
    {registered, []},
    {applications, [kernel, stdlib]}
]}.
```

---

### 2. `macula_quic`

**Purpose**: QUIC transport layer (wrapper around quicer).

**Modules**:
```erlang
macula_quic_listener.erl   - QUIC listener (accept connections)
macula_quic_client.erl     - QUIC client (initiate connections)
macula_quic_connection.erl - GenServer per QUIC connection
macula_quic_stream.erl     - Stream management
macula_quic_config.erl     - QUIC configuration (TLS, ALPN, etc.)
```

**Dependencies**:
- `quicer` (NIF for MsQuic)

**Key Features**:
- Connection pooling
- Stream multiplexing
- 0-RTT support
- Connection migration
- Backpressure handling

**Rebar3 Config**:
```erlang
{application, macula_quic, [
    {description, "Macula QUIC transport layer"},
    {vsn, "0.1.0"},
    {modules, []},
    {registered, []},
    {applications, [kernel, stdlib, macula_core, quicer]},
    {mod, {macula_quic_app, []}}
]}.

{deps, [
    {quicer, {git, "https://github.com/emqx/quic.git", {branch, "main"}}}
]}.
```

---

### 3. `macula_protocol`

**Purpose**: Wire protocol (message framing, encoding/decoding).

**Modules**:
```erlang
macula_protocol.erl        - Main protocol API
macula_frame.erl           - Frame encoding/decoding
macula_message.erl         - Message types and validation
macula_codec.erl           - Binary serialization (Erlang term format or MessagePack)
```

**Message Types**:
```erlang
-define(MSG_HANDSHAKE, 16#01).
-define(MSG_HANDSHAKE_ACK, 16#02).
-define(MSG_HEARTBEAT, 16#03).
-define(MSG_PING, 16#04).
-define(MSG_PONG, 16#05).
-define(MSG_PUBLISH, 16#10).
-define(MSG_SUBSCRIBE, 16#11).
-define(MSG_UNSUBSCRIBE, 16#12).
-define(MSG_EVENT, 16#13).
-define(MSG_RPC_CALL, 16#20).
-define(MSG_RPC_RESULT, 16#21).
-define(MSG_RPC_ERROR, 16#22).
-define(MSG_ERROR, 16#F0).
-define(MSG_CLOSE, 16#FF).
```

**Dependencies**:
- `macula_core`

**Optional Dependencies**:
- `msgpack` (if using MessagePack instead of Erlang term format)

**Rebar3 Config**:
```erlang
{application, macula_protocol, [
    {description, "Macula wire protocol"},
    {vsn, "0.1.0"},
    {applications, [kernel, stdlib, macula_core]}
]}.
```

---

### 4. `macula_membership`

**Purpose**: SWIM-based membership and failure detection.

**Modules**:
```erlang
macula_membership.erl      - Main membership API
macula_swim.erl            - SWIM protocol GenServer
macula_swim_detector.erl   - Failure detector
macula_swim_gossip.erl     - Gossip dissemination
macula_member.erl          - Member record and state
```

**Features**:
- Membership list management
- Direct ping / indirect ping
- Suspicion mechanism
- Incarnation numbers
- Realm-scoped membership

**Dependencies**:
- `macula_core`
- `macula_protocol`
- `macula_quic`

**Rebar3 Config**:
```erlang
{application, macula_membership, [
    {description, "Macula SWIM membership"},
    {vsn, "0.1.0"},
    {applications, [kernel, stdlib, macula_core, macula_protocol, macula_quic]},
    {mod, {macula_membership_app, []}}
]}.
```

---

### 5. `macula_routing`

**Purpose**: Kademlia DHT for routing.

**Modules**:
```erlang
macula_routing.erl         - Routing API
macula_kademlia.erl        - Kademlia DHT GenServer
macula_k_bucket.erl        - K-bucket management
macula_node_lookup.erl     - Node lookup (iterative)
macula_topic_registry.erl  - Topic → Nodes mapping (for pub/sub)
macula_rpc_registry.erl    - RPC name → Node mapping
```

**Features**:
- 256 k-buckets (for 256-bit node IDs)
- XOR distance metric
- Iterative lookups
- Bucket refresh
- Realm partitioning

**Dependencies**:
- `macula_core`
- `macula_membership`

**Rebar3 Config**:
```erlang
{application, macula_routing, [
    {description, "Macula Kademlia DHT routing"},
    {vsn, "0.1.0"},
    {applications, [kernel, stdlib, macula_core, macula_membership]},
    {mod, {macula_routing_app, []}}
]}.
```

---

### 6. `macula_topology`

**Purpose**: Mesh topology management (k-regular graph).

**Modules**:
```erlang
macula_topology.erl        - Topology management
macula_k_regular.erl       - k-regular graph algorithm
macula_connection_pool.erl - Connection pool supervisor
macula_connection.erl      - GenServer per peer connection
```

**Features**:
- k-regular graph topology
- Connection lifecycle (connect, disconnect, reconnect)
- Exponential backoff for reconnections
- Topology visualization

**Dependencies**:
- `macula_core`
- `macula_quic`
- `macula_membership`
- `macula_routing`

**Rebar3 Config**:
```erlang
{application, macula_topology, [
    {description, "Macula mesh topology management"},
    {vsn, "0.1.0"},
    {applications, [kernel, stdlib, macula_core, macula_quic,
                    macula_membership, macula_routing]},
    {mod, {macula_topology_app, []}}
]}.
```

---

### 7. `macula_pubsub`

**Purpose**: Publish/subscribe messaging.

**Modules**:
```erlang
macula_pubsub.erl          - Pub/sub API
macula_topic_tree.erl      - Topic subscription tree (pattern matching)
macula_subscription.erl    - Subscription management
macula_publisher.erl       - Publishing logic
macula_event_router.erl    - Event routing to subscribers
```

**Features**:
- Topic pattern matching (exact, prefix, wildcard)
- Local and remote subscriptions
- Subscription announcements (via DHT)
- Event delivery guarantees (at-most-once, at-least-once)

**Dependencies**:
- `macula_core`
- `macula_protocol`
- `macula_routing`
- `macula_topology`

**Rebar3 Config**:
```erlang
{application, macula_pubsub, [
    {description, "Macula pub/sub messaging"},
    {vsn, "0.1.0"},
    {applications, [kernel, stdlib, macula_core, macula_protocol,
                    macula_routing, macula_topology]},
    {mod, {macula_pubsub_app, []}}
]}.
```

---

### 8. `macula_rpc`

**Purpose**: Remote procedure call (RPC) implementation.

**Modules**:
```erlang
macula_rpc.erl             - RPC API (call, register, unregister)
macula_rpc_server.erl      - RPC request handler
macula_rpc_client.erl      - RPC call client
macula_rpc_registry.erl    - Local RPC endpoint registry
```

**Features**:
- Synchronous RPC (with timeout)
- Asynchronous RPC (cast)
- RPC endpoint registration (name → handler function)
- Endpoint discovery via DHT
- Load balancing (if multiple nodes register same RPC)

**Dependencies**:
- `macula_core`
- `macula_protocol`
- `macula_routing`
- `macula_topology`

**Rebar3 Config**:
```erlang
{application, macula_rpc, [
    {description, "Macula RPC"},
    {vsn, "0.1.0"},
    {applications, [kernel, stdlib, macula_core, macula_protocol,
                    macula_routing, macula_topology]},
    {mod, {macula_rpc_app, []}}
]}.
```

---

### 9. `macula_discovery`

**Purpose**: Node discovery (DNS-SD, mDNS, static, cloud).

**Modules**:
```erlang
macula_discovery.erl       - Discovery coordinator
macula_discovery_static.erl - Static bootstrap nodes
macula_discovery_mdns.erl  - mDNS (Multicast DNS)
macula_discovery_dns.erl   - DNS SRV records
macula_discovery_consul.erl - Consul service discovery
macula_discovery_k8s.erl   - Kubernetes endpoints
```

**Features**:
- Multiple discovery methods (configurable)
- Continuous discovery (periodic re-discovery)
- Bootstrap node list
- Realm-aware discovery

**Dependencies**:
- `macula_core`
- `mdns` (for mDNS support)

**Rebar3 Config**:
```erlang
{application, macula_discovery, [
    {description, "Macula node discovery"},
    {vsn, "0.1.0"},
    {applications, [kernel, stdlib, macula_core]},
    {mod, {macula_discovery_app, []}}
]}.

{deps, [
    {mdns, {git, "https://github.com/benoitc/erlang-mdns.git", {branch, "master"}}}
]}.
```

---

### 10. `macula_security`

**Purpose**: Security (TLS certificates, ACLs, audit logging).

**Modules**:
```erlang
macula_security.erl        - Security API
macula_cert.erl            - Certificate generation and validation
macula_acl.erl             - Access control lists
macula_audit.erl           - Audit logging
macula_crypto.erl          - Message signing/verification
```

**Features**:
- Certificate generation (self-signed, CA-signed)
- Certificate validation (realm extraction from SAN)
- ACL enforcement (topic/RPC access control)
- Audit log (security events)
- Optional message signing

**Dependencies**:
- `macula_core`
- `public_key` (Erlang stdlib)
- `ssl` (Erlang stdlib)

**Rebar3 Config**:
```erlang
{application, macula_security, [
    {description, "Macula security"},
    {vsn, "0.1.0"},
    {applications, [kernel, stdlib, public_key, ssl, macula_core]},
    {mod, {macula_security_app, []}}
]}.
```

---

### 11. `macula_gateway`

**Purpose**: Cross-realm gateway functionality.

**Modules**:
```erlang
macula_gateway.erl         - Gateway API
macula_gateway_server.erl  - Gateway GenServer
macula_policy.erl          - Policy engine (topic filtering, rate limiting)
macula_translation.erl     - Topic translation
macula_rate_limiter.erl    - Rate limiting per realm pair
```

**Features**:
- Multi-realm support
- Policy-based message filtering
- Topic translation
- Rate limiting
- Audit logging of cross-realm traffic

**Dependencies**:
- `macula_core`
- `macula_protocol`
- `macula_pubsub`
- `macula_rpc`
- `macula_security`

**Rebar3 Config**:
```erlang
{application, macula_gateway, [
    {description, "Macula cross-realm gateway"},
    {vsn, "0.1.0"},
    {applications, [kernel, stdlib, macula_core, macula_protocol,
                    macula_pubsub, macula_rpc, macula_security]},
    {mod, {macula_gateway_app, []}}
]}.
```

---

### 12. `macula` (Main Application)

**Purpose**: Umbrella application that ties everything together.

**Modules**:
```erlang
macula.erl                 - Main API
macula_app.erl             - Application callback
macula_sup.erl             - Top-level supervisor
macula_config.erl          - Configuration management
```

**Supervision Tree**:
```erlang
macula_sup (one_for_one)
├── macula_discovery_sup
├── macula_quic_sup
├── macula_membership_sup
├── macula_routing_sup
├── macula_topology_sup
├── macula_pubsub_sup
├── macula_rpc_sup
├── macula_security_sup
└── macula_gateway_sup (optional, if gateway mode)
```

**Dependencies**: All macula_* libraries

**Rebar3 Config**:
```erlang
{application, macula, [
    {description, "Macula HTTP/3 Mesh Platform"},
    {vsn, "0.1.0"},
    {applications, [
        kernel, stdlib,
        macula_core,
        macula_quic,
        macula_protocol,
        macula_membership,
        macula_routing,
        macula_topology,
        macula_pubsub,
        macula_rpc,
        macula_discovery,
        macula_security,
        macula_gateway
    ]},
    {mod, {macula_app, []}},
    {env, [
        {realm, <<"org.example.mesh">>},
        {listen_port, 4433},
        {discovery, [{methods, [static, mdns]}]},
        {topology, [{type, k_regular}, {k, 2}]}
    ]}
]}.
```

---

## Supporting Tools and Utilities

### 13. `macula_cli`

**Purpose**: Command-line tool for Macula operations.

**Features**:
- Start/stop nodes
- Join mesh
- View topology
- Send test messages
- Query membership
- Inspect routing table

**Implementation**: Escript

**Rebar3 Config**:
```erlang
{escript_name, macula}.
{escript_emu_args, "%%! -escript main macula_cli\n"}.
```

**Usage**:
```bash
macula start --realm org.example.mesh --port 4433
macula join 192.168.1.100:4433
macula topology
macula publish topic.name '{"data": "hello"}'
macula stats
```

---

### 14. `macula_observer`

**Purpose**: Real-time mesh visualization and monitoring.

**Features**:
- Visual mesh topology (graphviz-style)
- Live message flow
- Membership state
- Connection status
- Metrics dashboard

**Implementation**: Phoenix LiveView application (if using Elixir)

**Alternative**: Standalone Erlang application with web UI (Cowboy + WebSocket)

---

### 15. `macula_loadtest`

**Purpose**: Load testing and benchmarking tool.

**Features**:
- Spawn N virtual nodes
- Pub/sub throughput testing
- RPC latency testing
- Failure injection
- Report generation

**Implementation**: Standalone Erlang application using Tsung or custom framework

---

## Optional/Future Libraries

### 16. `macula_wamp_compat`

**Purpose**: WAMP compatibility layer (bridge WAMP clients to Macula).

**Modules**:
- WAMP protocol adapter
- WebSocket server
- Message translation (WAMP ↔ Macula)

**Use Case**: Migrate from Bondy/WAMP to Macula gradually

---

### 17. `macula_http_bridge`

**Purpose**: HTTP/REST gateway for Macula (publish via HTTP POST).

**Modules**:
- Cowboy HTTP handler
- REST API (publish, call, subscribe via SSE)

**Use Case**: Non-BEAM clients accessing Macula

---

### 18. `macula_kafka_connector`

**Purpose**: Kafka bridge (publish Macula events to Kafka, consume Kafka events).

**Dependencies**: `brod` (Kafka client)

---

### 19. `macula_postgres_connector`

**Purpose**: PostgreSQL integration (CDC, event sourcing).

**Dependencies**: `epgsql` or `postgrex`

---

### 20. `macula_metrics`

**Purpose**: Metrics and observability (Prometheus, OpenTelemetry).

**Modules**:
- Prometheus exporter
- OpenTelemetry integration
- StatsD reporter

**Dependencies**:
- `prometheus` or `prometheus_ex`
- `opentelemetry` and `opentelemetry_exporter`

---

## Development Roadmap

### Phase 1: Foundation (Weeks 1-4)

**Goal**: Get basic QUIC transport and protocol working.

**Libraries to build**:
1. `macula_core` - Types and utilities
2. `macula_quic` - QUIC wrapper
3. `macula_protocol` - Wire protocol

**Deliverable**: Two nodes can connect and exchange handshake messages.

---

### Phase 2: Mesh Topology (Weeks 5-8)

**Goal**: Self-organizing mesh network.

**Libraries to build**:
4. `macula_membership` - SWIM
5. `macula_routing` - Kademlia DHT
6. `macula_topology` - k-regular graph
7. `macula_discovery` - Node discovery

**Deliverable**: N nodes form a mesh and detect failures.

---

### Phase 3: Messaging (Weeks 9-12)

**Goal**: Pub/sub and RPC working across mesh.

**Libraries to build**:
8. `macula_pubsub` - Pub/sub
9. `macula_rpc` - RPC

**Deliverable**: Applications can publish/subscribe and make RPC calls.

---

### Phase 4: Security and Gateways (Weeks 13-16)

**Goal**: Production-ready security and multi-tenancy.

**Libraries to build**:
10. `macula_security` - Certificates, ACLs, audit
11. `macula_gateway` - Cross-realm gateway

**Deliverable**: Secure mesh with realm isolation.

---

### Phase 5: Tooling and Monitoring (Weeks 17-20)

**Goal**: Developer experience and operations tooling.

**Tools to build**:
13. `macula_cli` - Command-line tool
14. `macula_observer` - Visualization
15. `macula_loadtest` - Benchmarking
20. `macula_metrics` - Observability

**Deliverable**: Production-ready platform with tooling.

---

## Testing Strategy

### Unit Tests

Each library has its own test suite:
```
apps/macula_core/test/
apps/macula_quic/test/
apps/macula_protocol/test/
...
```

**Framework**: EUnit (Erlang) or ExUnit (Elixir)

**Run**:
```bash
rebar3 eunit
# or
mix test
```

---

### Integration Tests

Multi-node integration tests:
```
test/integration/
├── mesh_formation_test.erl
├── pubsub_test.erl
├── rpc_test.erl
├── failure_recovery_test.erl
└── gateway_test.erl
```

**Framework**: Common Test (Erlang)

**Run**:
```bash
rebar3 ct
```

---

### Property-Based Tests

Use PropEr (Erlang) or StreamData (Elixir):
```
apps/macula_routing/test/prop_kademlia.erl
apps/macula_membership/test/prop_swim.erl
```

**Run**:
```bash
rebar3 proper
```

---

### Load Tests

Separate load testing suite:
```
loadtest/
├── pubsub_throughput.erl
├── rpc_latency.erl
├── mesh_scale.erl (1000+ nodes)
└── failure_injection.erl
```

**Framework**: Tsung or custom

---

## Continuous Integration

### GitHub Actions Workflow

```yaml
name: CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '26.2'
          rebar3-version: '3.22'
      - run: rebar3 compile
      - run: rebar3 eunit
      - run: rebar3 ct
      - run: rebar3 dialyzer

  integration:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: erlef/setup-beam@v1
      - run: rebar3 as test release
      - run: ./test/integration/run_all.sh
```

---

## Release Strategy

### Rebar3 Release

```erlang
%% rebar.config
{relx, [
    {release, {macula, "0.1.0"}, [
        macula,
        sasl
    ]},

    {mode, prod},
    {include_erts, true},
    {extended_start_script, true},

    {overlay, [
        {copy, "config/sys.config.example", "etc/sys.config"},
        {copy, "config/vm.args.example", "etc/vm.args"}
    ]}
]}.
```

**Build**:
```bash
rebar3 release
```

**Result**: `_build/default/rel/macula/`

---

### Docker Image

```dockerfile
FROM erlang:26-alpine AS builder

WORKDIR /build
COPY . .
RUN rebar3 as prod release

FROM alpine:latest

RUN apk add --no-cache openssl ncurses-libs libstdc++

COPY --from=builder /build/_build/prod/rel/macula /opt/macula

EXPOSE 4433/udp

CMD ["/opt/macula/bin/macula", "foreground"]
```

**Build**:
```bash
docker build -t macula:latest .
```

---

## Repository Initialization

### Create Umbrella Application

```bash
# Erlang (Rebar3)
rebar3 new umbrella macula
cd macula

# Create apps
rebar3 new lib apps/macula_core
rebar3 new lib apps/macula_quic
rebar3 new lib apps/macula_protocol
# ... etc

# Compile
rebar3 compile

# Test
rebar3 eunit

# Release
rebar3 release
```

### OR Elixir (Mix)

```bash
# Elixir (Mix)
mix new macula --umbrella
cd macula

# Create apps
cd apps
mix new macula_core
mix new macula_quic
mix new macula_protocol
# ... etc

cd ..

# Compile
mix compile

# Test
mix test

# Release
mix release
```

---

## Summary

**Core Libraries** (12):
1. ✅ `macula_core` - Core types and utilities
2. ✅ `macula_quic` - QUIC transport
3. ✅ `macula_protocol` - Wire protocol
4. ✅ `macula_membership` - SWIM membership
5. ✅ `macula_routing` - Kademlia DHT
6. ✅ `macula_topology` - Mesh topology
7. ✅ `macula_pubsub` - Pub/sub
8. ✅ `macula_rpc` - RPC
9. ✅ `macula_discovery` - Node discovery
10. ✅ `macula_security` - Security
11. ✅ `macula_gateway` - Cross-realm gateway
12. ✅ `macula` - Main application

**Tools** (3):
13. ✅ `macula_cli` - CLI tool
14. ✅ `macula_observer` - Visualization
15. ✅ `macula_loadtest` - Load testing

**Optional** (5):
16. ⚠️ `macula_wamp_compat` - WAMP bridge
17. ⚠️ `macula_http_bridge` - HTTP gateway
18. ⚠️ `macula_kafka_connector` - Kafka integration
19. ⚠️ `macula_postgres_connector` - PostgreSQL integration
20. ⚠️ `macula_metrics` - Metrics/observability

**Total**: 20 libraries/applications

---

**Next Step**: Initialize the repository structure and start with Phase 1 (Foundation).

---

**Last Updated**: 2025-01-08
**Maintainers**: [To be assigned]
