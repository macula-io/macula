# Distribution System

The Distribution System provides QUIC-based distributed Erlang node discovery and communication, replacing EPMD with decentralized discovery mechanisms.

## Module Table

| Module | Purpose | LOC |
|--------|---------|-----|
| `macula_dist_system` | Supervisor for distribution subsystem | 4k |
| `macula_dist` | QUIC carrier for Erlang distribution | 8k |
| `macula_dist_discovery` | Decentralized node discovery (replaces EPMD) | 12k |
| `macula_dist_mdns_advertiser` | mDNS service advertisement for local discovery | 6k |
| `macula_cluster_strategy` | libcluster-compatible cluster formation strategy | 5k |

## Overview

Traditional Erlang distribution uses EPMD (Erlang Port Mapper Daemon) and TCP for inter-node communication. Macula replaces this with:

- **QUIC transport**: Single UDP port, built-in TLS 1.3, connection migration
- **DHT discovery**: Decentralized node discovery via Kademlia DHT
- **mDNS discovery**: Local network discovery without central registry
- **Automatic clustering**: libcluster-compatible automatic cluster formation

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     QUIC Distribution                           │
│  ┌─────────────┐  ┌──────────────┐  ┌────────────────────┐     │
│  │ macula_dist │  │ dist_discovery│  │ cluster_strategy   │     │
│  │ (transport) │  │ (DHT/mDNS)   │  │ (libcluster)       │     │
│  └──────┬──────┘  └───────┬──────┘  └─────────┬──────────┘     │
│         │                 │                    │                │
│         └────────┬────────┴────────────────────┘                │
│                  │                                              │
│         ┌────────▼────────┐                                     │
│         │ Single QUIC Port │ (default: 4433)                    │
│         │    UDP/TLS 1.3   │                                    │
│         └──────────────────┘                                    │
└─────────────────────────────────────────────────────────────────┘
```

## Node Discovery Mechanisms

### DHT Discovery (Default)
Uses the Kademlia DHT to register and discover nodes:
```erlang
%% Node registers itself in DHT with node name and endpoint
macula_dist_discovery:register_node(NodeName, Endpoint).

%% Find other nodes via DHT lookup
{ok, Nodes} = macula_dist_discovery:find_nodes(ClusterName).
```

### mDNS Discovery
For local network discovery without internet access:
```erlang
%% Enable mDNS advertisement
macula_dist_mdns_advertiser:start_link(#{
    service_name => <<"macula">>,
    service_type => <<"_macula._udp">>,
    port => 4433
}).
```

### Combined Discovery
Both mechanisms can run simultaneously (default: `both`):
```erlang
macula_dist_system:start_link(#{
    discovery_type => both  %% dht | mdns | both
}).
```

## Configuration

### vm.args Configuration

```erlang
%% Enable QUIC distribution (deferred until v1.1.0+)
-proto_dist macula
-no_epmd
-start_epmd false
-macula_dist_port 4433
```

### sys.config Configuration

```erlang
{macula, [
    {dist_port, 4433},
    {discovery_type, both},        %% dht | mdns | both
    {auto_cluster, true},          %% Enable automatic clustering
    {cluster_config, #{
        topology => macula_cluster,
        connect_timeout => 5000
    }}
]}
```

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `MACULA_DIST_PORT` | 4433 | QUIC distribution port |
| `MACULA_DISCOVERY_TYPE` | both | Discovery mechanism (dht/mdns/both) |
| `MACULA_AUTO_CLUSTER` | false | Enable automatic cluster formation |

## Supervision Tree

```
macula_dist_system (supervisor)
├── macula_dist_discovery (worker)
│   ├── DHT registration
│   └── mDNS advertisement (if enabled)
└── macula_cluster_strategy (worker, if auto_cluster=true)
    └── libcluster topology management
```

## libcluster Integration

The `macula_cluster_strategy` module implements the libcluster strategy behavior:

```erlang
%% In your cluster config
[
    {libcluster, [
        {topologies, [
            {macula_cluster, [
                {strategy, macula_cluster_strategy},
                {config, [
                    {bootstrap_nodes, [<<"boot1.example.com:4433">>]},
                    {poll_interval, 5000}
                ]}
            ]}
        ]}
    ]}
]
```

## Benefits vs EPMD/TCP Distribution

| Feature | EPMD/TCP | Macula QUIC |
|---------|----------|-------------|
| Port requirements | Multiple (4369 + dynamic range) | Single UDP port |
| NAT traversal | Poor | Excellent |
| TLS encryption | Optional add-on | Built-in TLS 1.3 |
| Discovery | Centralized (EPMD) | Decentralized (DHT/mDNS) |
| Connection migration | No | Yes (QUIC feature) |
| Firewall friendly | No | Yes |

## Status

**Current Status**: Implemented, deferred for production use until v1.1.0+

The distribution system is fully implemented but the `-proto_dist macula` integration is deferred until after v1.0.0 proves stable. Currently:
- Discovery mechanisms work
- Cluster strategy integrates with libcluster
- QUIC transport layer is functional
- Full integration testing pending

## Related Documentation

- [QUIC Distribution Architecture](../../architecture/v0.11.0-QUIC_DISTRIBUTION.md)
- [Cluster Strategy Guide](../../docs/operator/CLUSTER_STRATEGY.md)
