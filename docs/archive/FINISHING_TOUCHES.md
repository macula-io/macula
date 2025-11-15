# Macula Finishing Touches

## Overview

After completing the core protocol stack (macula_protocol, macula_quic, macula_membership, macula_routing, macula_pubsub, macula_rpc), these finishing touches will polish the implementation for production readiness.

## 1. Integration Layer

### macula Application

Create top-level `macula` application that provides unified API and supervision tree.

```erlang
%% apps/macula/src/macula.erl
-module(macula).

%% Application lifecycle
-export([start/0, stop/0]).

%% Node management
-export([start_node/1, stop_node/0, node_id/0]).

%% Membership API
-export([join/1, leave/0, members/0]).

%% Pub/Sub API
-export([subscribe/2, unsubscribe/2, publish/2]).

%% RPC API
-export([register/3, unregister/1, call/2, call/3]).

%% Configuration
-export([get_config/1, set_config/2]).
```

**Responsibilities**:
- Start all subsystems in correct order
- Provide unified configuration
- Expose clean public API
- Handle graceful shutdown

**Supervision Tree**:
```
macula_sup (one_for_one)
├── macula_quic_sup (rest_for_one)
│   └── macula_quic_server
├── macula_membership_sup (rest_for_one)
│   └── macula_membership_server
├── macula_routing_sup (rest_for_one)
│   └── macula_routing_server
├── macula_pubsub_sup (rest_for_one)
│   └── macula_pubsub_server
└── macula_rpc_sup (rest_for_one)
    └── macula_rpc_server
```

**Configuration Schema**:
```erlang
#{
    node => #{
        id => binary() | auto,       % Auto-generate 256-bit ID
        address => {inet:ip_address(), inet:port_number()}
    },
    quic => #{
        cert_file => string(),
        key_file => string(),
        alpn => [binary()],
        stream_timeout => pos_integer()
    },
    membership => #{
        protocol_period => pos_integer(),
        indirect_count => pos_integer(),
        suspect_timeout => pos_integer()
    },
    routing => #{
        k => pos_integer(),          % Bucket size
        alpha => pos_integer(),      % Lookup concurrency
        refresh_interval => pos_integer()
    },
    pubsub => #{
        cache_ttl => pos_integer(),
        announce_interval => pos_integer(),
        max_cache_size => pos_integer()
    },
    rpc => #{
        default_timeout => pos_integer(),
        cache_enabled => boolean(),
        routing_strategy => atom()
    }
}
```

---

## 2. Bootstrap and Discovery

### Seed Nodes

Implement bootstrap mechanism for new nodes joining the mesh.

```erlang
%% apps/macula/src/macula_bootstrap.erl
-module(macula_bootstrap).

-export([bootstrap/1, add_seed/1, remove_seed/1]).

-spec bootstrap([{inet:ip_address(), inet:port_number()}]) -> ok | {error, Reason}.
%% Connect to seed nodes and populate routing table
```

**Bootstrap Flow**:
```
New Node                    Seed Node               DHT Network
    |                           |                        |
    |--connect----------------->|                        |
    |                           |                        |
    |--find_node(self)--------->|                        |
    |                           |                        |
    |<--k closest nodes---------|                        |
    |                           |                        |
    |--add to routing table-----|                        |
    |                           |                        |
    |--find_node(random)--------|----------------------->|
    |                           |                        |
    |<--k closest nodes---------|------------------------|
    |                           |                        |
    |--populate buckets---------|                        |
    |                           |                        |
  [Fully joined mesh]
```

**Seed Discovery Methods**:
- Static configuration (config file)
- DNS SRV records
- Environment variables
- Multicast discovery (local network)

---

## 3. Health and Observability

### Telemetry Integration

Comprehensive metrics and events using `:telemetry`.

```erlang
%% Telemetry events
[macula, quic, connection, opened]
[macula, quic, connection, closed]
[macula, quic, stream, opened]
[macula, quic, stream, closed]
[macula, quic, message, sent]
[macula, quic, message, received]

[macula, membership, member, joined]
[macula, membership, member, left]
[macula, membership, member, suspected]
[macula, membership, member, confirmed]
[macula, membership, gossip, transmitted]

[macula, routing, lookup, started]
[macula, routing, lookup, completed]
[macula, routing, bucket, split]
[macula, routing, bucket, refreshed]

[macula, pubsub, published]
[macula, pubsub, delivered]
[macula, pubsub, subscription, added]
[macula, pubsub, subscription, removed]

[macula, rpc, call, started]
[macula, rpc, call, completed]
[macula, rpc, call, timeout]
[macula, rpc, registration, added]
[macula, rpc, registration, removed]
```

**Metrics**:
- Counter: connections, messages, calls
- Gauge: active members, routing table size, subscriptions
- Histogram: latency, message size
- Summary: call duration, lookup hops

**Prometheus Export**:
```erlang
%% apps/macula/src/macula_prometheus.erl
-module(macula_prometheus).

-export([setup/0, metrics/0]).

%% Export metrics in Prometheus format
%% GET /metrics
```

---

### Health Checks

Health check endpoint for monitoring.

```erlang
%% apps/macula/src/macula_health.erl
-module(macula_health).

-export([check/0, liveness/0, readiness/0]).

-spec check() -> #{
    status := healthy | degraded | unhealthy,
    checks := [check_result()]
}.

-type check_result() :: #{
    name := binary(),
    status := ok | error,
    message := binary()
}.
```

**Health Checks**:
- QUIC listener active
- Routing table populated (>= k nodes)
- Membership detector running
- Pub/Sub server responsive
- RPC server responsive

---

## 4. Security Enhancements

### Authentication

Node authentication using certificates.

```erlang
%% apps/macula/src/macula_auth.erl
-module(macula_auth).

-export([verify_peer/1, verify_certificate/1]).

%% Verify peer certificate during QUIC handshake
-spec verify_peer(Cert :: binary()) -> ok | {error, Reason}.
```

**Certificate-Based Auth**:
- Each node has TLS certificate
- Certificates signed by CA
- Verify peer certificate on connection
- Extract node ID from certificate subject

---

### Authorization

Topic/procedure-level access control.

```erlang
%% apps/macula/src/macula_authz.erl
-module(macula_authz).

-export([authorize_publish/2, authorize_subscribe/2, authorize_call/2]).

%% Check if node can publish to topic
-spec authorize_publish(NodeId :: binary(), Topic :: binary()) -> ok | {error, forbidden}.

%% Check if node can subscribe to pattern
-spec authorize_subscribe(NodeId :: binary(), Pattern :: binary()) -> ok | {error, forbidden}.

%% Check if node can call procedure
-spec authorize_call(NodeId :: binary(), Uri :: binary()) -> ok | {error, forbidden}.
```

**Authorization Policies**:
```erlang
%% Example policy configuration
#{
    <<"node_abc">> => #{
        publish => [<<"be.cortexiq.home.*">>],
        subscribe => [<<"be.cortexiq.#">>],
        call => [<<"be.cortexiq.provider.*">>]
    }
}
```

---

## 5. Performance Optimizations

### ETS Storage

Replace map-based storage with ETS for scalability.

**Areas to optimize**:
- `macula_routing_table`: ETS table per bucket
- `macula_pubsub_registry`: ETS table for subscriptions
- `macula_rpc_registry`: ETS table for registrations
- `macula_membership_list`: ETS table for members

**Example**:
```erlang
%% Before: map-based
-type routing_table() :: #{
    buckets := #{bucket_index() => bucket()}
}.

%% After: ETS-based
-record(routing_table, {
    local_node_id :: binary(),
    buckets_ets :: ets:tid(),  % ETS table
    k :: pos_integer()
}).
```

---

### Connection Pooling

Pool QUIC connections to frequently contacted nodes.

```erlang
%% apps/macula/src/macula_connection_pool.erl
-module(macula_connection_pool).

-export([get_connection/1, return_connection/2]).

%% Get or create connection to node
-spec get_connection(NodeInfo :: node_info()) -> {ok, Conn :: quicer:connection_handle()}.

%% Return connection to pool
-spec return_connection(NodeId :: binary(), Conn :: quicer:connection_handle()) -> ok.
```

**Pool Strategy**:
- Max 100 connections per node
- Idle timeout: 60 seconds
- Health check: Ping every 30 seconds
- LRU eviction when pool full

---

## 6. Tooling and CLI

### macula_ctl CLI

Command-line interface for node management.

```bash
# Start node
./macula_ctl start

# Join cluster
./macula_ctl join 192.168.1.10:4000

# Show status
./macula_ctl status

# List members
./macula_ctl members

# Show routing table
./macula_ctl routing-table

# List subscriptions
./macula_ctl subscriptions

# List registrations
./macula_ctl registrations

# Publish message
./macula_ctl publish be.cortexiq.test '{"foo": "bar"}'

# Call procedure
./macula_ctl call be.cortexiq.test.add '{"a": 1, "b": 2}'

# Stop node
./macula_ctl stop
```

---

### macula_console

Interactive debugging console (observer-like).

```erlang
%% apps/macula/src/macula_console.erl
-module(macula_console).

-export([start/0]).

%% Start web-based console
%% http://localhost:8080
```

**Console Features**:
- Live routing table visualization
- Member list with status
- Pub/Sub topic browser
- RPC procedure browser
- Real-time message tracing
- Performance metrics graphs

---

## 7. Documentation

### API Documentation

Complete ExDoc/EDoc documentation for all modules.

**Documentation Standards**:
- Module overview with examples
- Function specs with parameter descriptions
- Usage examples for key functions
- Architecture diagrams where helpful

---

### User Guide

Comprehensive user guide covering:

1. **Getting Started**
   - Installation
   - Configuration
   - First cluster setup
   - Hello World example

2. **Core Concepts**
   - Node IDs and addressing
   - QUIC transport
   - SWIM membership
   - Kademlia DHT
   - Pub/Sub patterns
   - RPC patterns

3. **Deployment**
   - Single node setup
   - Multi-node cluster
   - Docker deployment
   - Kubernetes deployment
   - Production checklist

4. **Operations**
   - Monitoring and metrics
   - Health checks
   - Troubleshooting
   - Performance tuning
   - Security hardening

5. **API Reference**
   - Pub/Sub API
   - RPC API
   - Membership API
   - Configuration options

---

### Examples

Example applications demonstrating Macula usage:

1. **chat_room** - Distributed chat using pub/sub
2. **kv_store** - Distributed key-value store using RPC + DHT
3. **event_log** - Distributed event log using pub/sub
4. **service_mesh** - Microservices example using RPC

---

## 8. Testing Enhancements

### Chaos Testing

Introduce failures to verify resilience.

```erlang
%% apps/macula/test/macula_chaos_SUITE.erl
-module(macula_chaos_SUITE).

%% Test scenarios
random_node_crashes_test() -> ...
network_partition_test() -> ...
message_loss_test() -> ...
slow_network_test() -> ...
```

**Chaos Scenarios**:
- Random node crashes (verify re-convergence)
- Network partitions (verify split-brain handling)
- Message loss (verify retries)
- Slow network (verify timeouts)

---

### Load Testing

Performance benchmarks and load tests.

```erlang
%% apps/macula/test/macula_load_SUITE.erl
-module(macula_load_SUITE).

%% Benchmarks
pubsub_throughput_test() -> ...  % Messages/sec
rpc_latency_test() -> ...        % Call latency
routing_lookup_test() -> ...     % Lookup performance
```

**Load Targets**:
- Pub/Sub: 10,000 msg/sec per node
- RPC: 5,000 calls/sec per node
- Routing: < 5 hops for 10,000 node network
- Membership: 1000 node cluster

---

### Fuzz Testing

Property-based and fuzz testing for edge cases.

```erlang
%% PropEr generators for protocol messages
prop_roundtrip_encoding() -> ...
prop_routing_convergence() -> ...
prop_membership_eventual_consistency() -> ...
```

---

## 9. Packaging and Distribution

### Rebar3 Plugin

Rebar3 plugin for easy Macula project setup.

```bash
# Create new Macula project
rebar3 new macula my_app

# Start Macula node
rebar3 macula start

# Run in foreground
rebar3 macula console
```

---

### Docker Images

Official Docker images for deployment.

```dockerfile
# Dockerfile
FROM erlang:26-alpine

COPY _build/default/rel/macula /opt/macula

ENTRYPOINT ["/opt/macula/bin/macula"]
CMD ["foreground"]
```

**Images**:
- `macula/macula:latest` - Latest release
- `macula/macula:1.0.0` - Tagged releases
- `macula/macula:dev` - Development builds

---

### Helm Chart

Kubernetes deployment via Helm.

```yaml
# values.yaml
replicaCount: 3

image:
  repository: macula/macula
  tag: "1.0.0"

service:
  type: LoadBalancer
  port: 4000

config:
  node:
    address: "0.0.0.0:4000"
  membership:
    protocol_period: 1000
  routing:
    k: 20
```

---

## 10. Continuous Integration

### GitHub Actions

Automated testing and releases.

```yaml
# .github/workflows/ci.yml
name: CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '26'
          rebar3-version: '3.22'
      - run: rebar3 compile
      - run: rebar3 eunit
      - run: rebar3 ct
      - run: rebar3 dialyzer
      - run: rebar3 xref
      - run: rebar3 cover
```

---

## Implementation Checklist

### Phase 1: Core Integration (Week 1)
- [ ] Create `macula` application
- [ ] Unified supervision tree
- [ ] Public API module
- [ ] Configuration system
- [ ] Bootstrap mechanism

### Phase 2: Observability (Week 2)
- [ ] Telemetry events
- [ ] Prometheus metrics
- [ ] Health checks
- [ ] Logging framework

### Phase 3: Security (Week 3)
- [ ] Certificate-based authentication
- [ ] Authorization policies
- [ ] Encrypted connections (already via QUIC TLS)

### Phase 4: Performance (Week 4)
- [ ] ETS storage migration
- [ ] Connection pooling
- [ ] Profiling and optimization

### Phase 5: Tooling (Week 5)
- [ ] macula_ctl CLI
- [ ] macula_console web interface
- [ ] Example applications

### Phase 6: Documentation (Week 6)
- [ ] API documentation (ExDoc)
- [ ] User guide
- [ ] Deployment guide
- [ ] Tutorial videos

### Phase 7: Testing (Week 7)
- [ ] Chaos testing suite
- [ ] Load testing benchmarks
- [ ] Fuzz testing with PropEr

### Phase 8: Packaging (Week 8)
- [ ] Rebar3 plugin
- [ ] Docker images
- [ ] Helm chart
- [ ] CI/CD pipeline

---

## Success Criteria

The Macula platform is production-ready when:

1. **Functionality**: All protocol layers working together seamlessly
2. **Performance**: Meets or exceeds target metrics
3. **Reliability**: Passes chaos testing scenarios
4. **Security**: Authentication and authorization implemented
5. **Observability**: Full metrics and tracing coverage
6. **Documentation**: Complete user guide and API docs
7. **Tooling**: CLI and console for operations
8. **Packaging**: Easy deployment via Docker/K8s
9. **Testing**: 95%+ code coverage, all tests passing
10. **Community**: Example apps and tutorials available

---

## Future Enhancements (Post-1.0)

- Progressive call results (WAMP-style)
- Call cancellation
- Subscription filters
- Message encryption (end-to-end)
- Multi-realm support
- Federation between Macula meshes
- WebAssembly plugins
- GraphQL API layer
