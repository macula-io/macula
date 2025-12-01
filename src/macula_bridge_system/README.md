# Macula Bridge System

This directory contains all modules related to the hierarchical DHT bridge subsystem (v0.13.0+).

## Overview

The Bridge System enables **hierarchical mesh organization** where Bridge Nodes at each mesh level form their own mesh with a shared DHT. This creates a fractal topology:

```
Cluster < Street < Neighborhood < City < Province < Country < Region < Global
```

When a DHT query fails locally, it **escalates to parent levels**, with results cached at lower levels to avoid repeated escalation.

## Architecture Diagrams

The following SVG diagrams provide visual explanations of the bridge system architecture:

### SuperMesh Hierarchy
![SuperMesh Hierarchy](../../doc/diagrams/supermesh_hierarchy.svg)

Shows the complete 8-level hierarchy from local clusters to global network, demonstrating the fractal mesh organization with bridge nodes connecting each level.

### Hierarchical Mesh Topology
![Hierarchical Mesh Topology](../../doc/diagrams/hierarchical_mesh_topology.svg)

Illustrates how City, Street, and Cluster meshes interconnect through bridge nodes, with peers forming full mesh within each cluster.

### Cluster Mesh Detail
![Cluster Mesh Detail](../../doc/diagrams/cluster_mesh_detail.svg)

Shows the internal structure of a cluster (smallest mesh unit), where every node has all subsystems (Gateway, Bootstrap, Peer, Bridge) and forms a full P2P mesh via QUIC.

### Bridge Escalation Flow
![Bridge Escalation Flow](../../doc/diagrams/bridge_escalation_flow.svg)

Demonstrates the DHT query escalation flow: Local DHT → Bridge Cache → Parent Escalation → Parent DHT → Cache Result.

## Modules

| Module | Purpose |
|--------|---------|
| `macula_bridge_system.erl` | Supervisor for bridge subsystem, manages lifecycle |
| `macula_bridge_node.erl` | Manages connection to parent mesh level, handles escalation |
| `macula_bridge_mesh.erl` | Peer-to-peer mesh formation between bridges at same level |
| `macula_bridge_cache.erl` | TTL-based caching for escalated query results with LRU eviction |

## Query Escalation Flow

```
1. Local DHT query → Not found
2. Check bridge cache → Cache miss
3. Escalate to parent via macula_bridge_node
4. Parent DHT query → Found
5. Cache result in bridge_cache (TTL varies by level)
6. Return result to caller
```

## TTL Configuration by Mesh Level

| Level | Default TTL | Rationale |
|-------|-------------|-----------|
| Cluster | 5 minutes | Local, changes frequently |
| Street | 10 minutes | Relatively stable |
| Neighborhood | 15 minutes | More stable |
| City | 30 minutes | Regional stability |
| Province+ | 60 minutes | Wide-area stability |

## Configuration

Environment variables:

```bash
MACULA_BRIDGE_ENABLED=true           # Enable bridge functionality
MACULA_MESH_LEVEL=cluster            # cluster|street|neighborhood|city|...
MACULA_PARENT_BRIDGES=quic://parent1:9443,quic://parent2:9443
MACULA_BRIDGE_DISCOVERY=static       # static|mdns|dns_srv
MACULA_BRIDGE_CACHE_TTL=300          # seconds (optional, uses level default)
MACULA_BRIDGE_CACHE_SIZE=10000       # max entries (optional)
```

Application config:

```erlang
{macula, [
    {bridge_enabled, true},
    {mesh_level, cluster},
    {parent_bridges, [<<"quic://parent1:9443">>, <<"quic://parent2:9443">>]},
    {bridge_discovery, static},  % static | mdns | dns_srv
    {bridge_cache_ttl, 300},     % seconds
    {bridge_cache_size, 10000}   % max entries
]}.
```

## API Usage

### Check bridge status
```erlang
%% Check if bridge is enabled and connected
true = macula_bridge_system:is_bridge_enabled().

%% Get bridge statistics
{ok, Stats} = macula_bridge_system:get_stats().
%% => #{mesh_level => cluster, bridge_connected => true, cache_hits => 42, ...}
```

### Get component PIDs
```erlang
%% Get Bridge Node PID (manages parent connection)
{ok, BridgePid} = macula_bridge_system:get_bridge_pid().

%% Get Bridge Mesh PID (manages peer bridges)
{ok, MeshPid} = macula_bridge_system:get_mesh_pid().
```

### Cache operations
```erlang
%% Direct cache access (usually handled automatically)
{ok, Value} = macula_bridge_cache:get(macula_bridge_cache, Key).
ok = macula_bridge_cache:put(macula_bridge_cache, Key, Value).
```

## Message Types

| Type | ID | Purpose |
|------|-----|---------|
| BRIDGE_RPC | 0x60 | Bridge RPC call to parent level |
| BRIDGE_DATA | 0x61 | Bridge data frame for escalated results |

## Integration

The bridge system integrates with:
- **Routing System**: `find_value_with_escalation/5` uses bridge for escalation
- **Gateway System**: Routes BRIDGE_* messages to bridge node
- **Bootstrap System**: Coordinates with DHT for local queries before escalation

## Discovery Methods

### Static (default)
Configured parent bridges via environment or config file.

### mDNS
Local network discovery using `_macula_bridge._udp` service type:
```erlang
%% Bridge advertises itself
mdns:advertise("_macula_bridge._udp", #{level => street, port => 9443}).

%% Bridge discovers peers
mdns:subscribe(advertisement).
```

### DNS SRV
WAN-scale discovery via DNS SRV records:
```
_macula-bridge._udp.street.eu.macula.net. 86400 IN SRV 10 5 9443 bridge1.eu.macula.net.
_macula-bridge._udp.street.eu.macula.net. 86400 IN SRV 10 5 9443 bridge2.eu.macula.net.
```

## Tests

See: `test/macula_bridge_system/`

Run bridge tests:
```bash
rebar3 eunit --module=macula_bridge_system_tests
rebar3 eunit --module=macula_bridge_node_tests
rebar3 eunit --module=macula_bridge_mesh_tests
rebar3 eunit --module=macula_bridge_cache_tests
```

**Test Coverage:**

| Test Module | Tests | Description |
|-------------|-------|-------------|
| `macula_bridge_system_tests` | 9 | Supervisor, child processes, mesh levels |
| `macula_bridge_node_tests` | 10 | Connection state, escalation, parent bridges |
| `macula_bridge_mesh_tests` | 9 | Add/remove peers, discovery, mesh levels |
| `macula_bridge_cache_tests` | 12 | Put/get, TTL, expiration, LRU eviction, stats |

**Total:** 40 tests

## Key Concepts

1. **Each level forms its own mesh** with shared DHT
2. **Bridge nodes connect to parent level** mesh
3. **Queries escalate up** when local DHT misses
4. **Results cached at lower levels** (TTL by level)
5. **Same fractal pattern** at every level
6. **No single point of failure** - redundant bridges
