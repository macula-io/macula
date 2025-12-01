# Bootstrap System

The Bootstrap System provides DHT bootstrapping, peer registry, and health check functionality for the Macula mesh network.

## Module Table

| Module | Purpose | LOC |
|--------|---------|-----|
| `macula_bootstrap_system` | Supervisor for bootstrap subsystem | 2.9k |
| `macula_bootstrap_server` | Main bootstrap server handling join requests | 4.3k |
| `macula_bootstrap_registry` | Peer registry with DHT integration | 4.8k |
| `macula_bootstrap_health` | Health check endpoints and monitoring | 4.9k |

## Overview

Every Macula node starts with a "seed node" address. The Bootstrap System handles:

1. **Initial Connection**: New peer connects to seed node
2. **DHT Bootstrapping**: Receive k-closest nodes to populate routing table
3. **Registration**: Peer registers services/subscriptions in DHT
4. **Health Monitoring**: Periodic health checks between nodes

## DHT Bootstrap Process

```
New Peer                    Seed Node (Bootstrap)
    |                              |
    |--- HELLO message ----------->|
    |   (peer_id, capabilities)    |
    |                              |
    |<-- WELCOME message ----------|
    |   (seed's peer_id, config)   |
    |                              |
    |--- FIND_NODE(self) --------->|
    |                              |
    |<-- NODES_FOUND --------------|
    |   (k-closest nodes)          |
    |                              |
    |   [Peer contacts returned    |
    |    nodes to populate DHT]    |
    |                              |
    |--- STORE (services) -------->|
    |   (advertise capabilities)   |
    |                              |
    |<-- STORE_OK ----------------|
```

## Registry Operations

### Peer Registration

```erlang
%% Register a peer in the bootstrap registry
ok = macula_bootstrap_registry:register_peer(PeerId, #{
    endpoint => {Host, Port},
    capabilities => [pubsub, rpc],
    nat_type => full_cone
}).

%% Find peers by capability
{ok, Peers} = macula_bootstrap_registry:find_peers_by_capability(pubsub).

%% Unregister peer
ok = macula_bootstrap_registry:unregister_peer(PeerId).
```

### Service Registration

```erlang
%% Register a service (via DHT)
ok = macula_bootstrap_registry:register_service(
    <<"my.service.name">>,
    #{node_id => NodeId, endpoint => Endpoint}
).

%% Find service providers
{ok, Providers} = macula_bootstrap_registry:find_service(<<"my.service.name">>).
```

## Health Check Endpoints

The `macula_bootstrap_health` module provides health monitoring:

```erlang
%% Get health status
{ok, Status} = macula_bootstrap_health:get_status().
%% Returns: #{
%%     status => healthy | degraded | unhealthy,
%%     uptime => UptimeSeconds,
%%     peer_count => Count,
%%     dht_entries => Count
%% }

%% Check specific component health
{ok, DHT} = macula_bootstrap_health:check_dht().
{ok, QUIC} = macula_bootstrap_health:check_quic().
```

### HTTP Health Endpoint

When running with HTTP enabled:
```
GET /health        -> 200 OK / 503 Service Unavailable
GET /health/live   -> 200 OK (always, if process running)
GET /health/ready  -> 200 OK (when fully bootstrapped)
```

## Configuration

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `MACULA_BOOTSTRAP_PORT` | 4433 | QUIC bootstrap port |
| `MACULA_SEED_NODES` | - | Comma-separated seed node addresses |
| `MACULA_HEALTH_PORT` | 8080 | HTTP health endpoint port |
| `MACULA_REGISTRY_TTL` | 300 | Peer registry TTL in seconds |

### Programmatic Configuration

```erlang
macula_bootstrap_system:start_link(#{
    port => 4433,
    seed_nodes => [
        {<<"seed1.example.com">>, 4433},
        {<<"seed2.example.com">>, 4433}
    ],
    registry_ttl => 600,
    health_enabled => true
}).
```

## Supervision Tree

```
macula_bootstrap_system (supervisor)
├── macula_bootstrap_server (worker)
│   └── Handles HELLO/WELCOME/FIND_NODE messages
├── macula_bootstrap_registry (worker)
│   └── Peer and service registry with TTL
└── macula_bootstrap_health (worker)
    └── Health monitoring and endpoints
```

## Message Types

| Type | Direction | Purpose |
|------|-----------|---------|
| HELLO | Peer → Seed | Initial connection request |
| WELCOME | Seed → Peer | Acknowledge connection, share config |
| FIND_NODE | Peer → Seed | Request k-closest nodes |
| NODES_FOUND | Seed → Peer | Return k-closest nodes |
| PING | Bidirectional | Health check |
| PONG | Bidirectional | Health check response |

## Seed Node Selection

For production deployments:

1. **Multiple Seed Nodes**: Configure 3-5 geographically distributed seeds
2. **DNS Round-Robin**: Use DNS for seed node discovery
3. **Fallback**: Nodes become seeds after successful bootstrap

```erlang
%% Environment: comma-separated seed addresses
MACULA_SEED_NODES="seed1.macula.io:4433,seed2.macula.io:4433,seed3.macula.io:4433"
```

## Metrics

| Metric | Description |
|--------|-------------|
| `bootstrap_joins_total` | Total peer join requests |
| `bootstrap_active_peers` | Currently registered peers |
| `bootstrap_dht_entries` | DHT routing table entries |
| `bootstrap_health_checks` | Health check count |

## Related Documentation

- [DHT Guide](../../docs/guides/DHT_GUIDE.md)
- [Quick Start](../../docs/user/QUICK_START.md)
- [Operator Performance Guide](../../docs/operator/PERFORMANCE_GUIDE.md)
