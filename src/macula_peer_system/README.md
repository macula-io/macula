# Macula Peer Subsystem

This directory contains all modules related to peer-to-peer connection management.

## Overview

The peer subsystem provides:
- Direct P2P QUIC connections between nodes
- Fire-and-forget connection establishment
- Connection pooling integration
- NAT traversal coordination

## Modules

| Module | Purpose |
|--------|---------|
| `macula_peer_connector.erl` | Fire-and-forget P2P QUIC connections |
| `macula_peer_system.erl` | Peer subsystem supervisor |
| `macula_peer_connection_pool.erl` | Connection pool with LRU eviction |

## Architecture

```
┌─────────────────────────────────────────────────┐
│             macula_peer_system                  │
│               (supervisor)                      │
└──────────────────────┬──────────────────────────┘
                       │
       ┌───────────────┼───────────────┐
       │               │               │
┌──────▼──────┐ ┌──────▼──────┐ ┌──────▼──────┐
│ peer_conn_  │ │  connector  │ │    NAT      │
│    pool     │ │ (outbound)  │ │ traversal   │
└─────────────┘ └─────────────┘ └─────────────┘
```

## Connection Flow

### Outbound Connection (peer_connector)
```
1. Request connection to endpoint
2. Check connection pool for existing connection
3. If not found:
   a. Try direct QUIC connect
   b. If NAT blocked, coordinate hole punch
   c. If punch fails, use relay
4. Cache connection in pool
5. Send message
```

### Connection Pool
```
Pool State:
┌─────────────────────────────────────┐
│ endpoint → {connection_pid, last_used} │
│ ...                                 │
│ Max: 1000 connections (LRU evicted) │
└─────────────────────────────────────┘
```

## Usage

### Send message to peer
```erlang
%% Fire-and-forget - returns immediately
macula_peer_connector:send_to_peer(Endpoint, MessageType, Message).

%% With options
macula_peer_connector:send_to_peer(Endpoint, MessageType, Message, #{
    timeout => 5000,
    retry => 1
}).
```

### Pool operations
```erlang
%% Get pooled connection
case macula_peer_connection_pool:get(Endpoint) of
    {ok, ConnPid} -> use_connection(ConnPid);
    not_found -> establish_new()
end.

%% Add to pool
macula_peer_connection_pool:put(Endpoint, ConnPid).

%% Pool stats
Stats = macula_peer_connection_pool:get_stats().
%% => #{size => 450, hits => 9450, misses => 550, hit_rate => 0.945}
```

## NAT Traversal Integration

The peer connector integrates with the NAT traversal subsystem:

1. **Detection**: NAT type detected on startup
2. **Hole Punching**: Coordinated via bootstrap node
3. **Relay Fallback**: If direct fails, use relay

See `src/macula_nat_system/` for NAT traversal details.

## Configuration

| Key | Default | Description |
|-----|---------|-------------|
| `pool_max_size` | 1000 | Max pooled connections |
| `connect_timeout` | 5000 | Connection timeout (ms) |
| `idle_timeout` | 300000 | Close idle connections after (ms) |

## Tests

See: `test/macula_peer_system/`

Run peer tests:
```bash
rebar3 eunit --module=macula_peer_connector_tests
rebar3 eunit --module=macula_peer_connection_pool_tests
```
