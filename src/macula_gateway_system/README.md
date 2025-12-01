# Macula Gateway Subsystem

This directory contains all modules related to the gateway subsystem, which handles QUIC listener management, message routing, and client connections.

## Overview

The gateway subsystem is responsible for:
- QUIC/HTTP3 server management
- Client connection lifecycle
- Message routing (PubSub, RPC)
- Mesh connection pooling
- DHT query forwarding
- Health monitoring and diagnostics

## Modules

| Module | Purpose |
|--------|---------|
| `macula_gateway.erl` | Main gateway gen_server - QUIC listener and message router |
| `macula_gateway_sup.erl` | Supervisor for gateway worker processes |
| `macula_gateway_client_manager.erl` | Client connection lifecycle management |
| `macula_gateway_pubsub.erl` | PubSub message routing with wildcard support |
| `macula_gateway_pubsub_router.erl` | Multi-hop PubSub routing via DHT |
| `macula_gateway_rpc.erl` | RPC handler registration and dispatch |
| `macula_gateway_rpc_router.erl` | Multi-hop RPC routing via DHT |
| `macula_gateway_mesh.erl` | Mesh connection pooling (LRU eviction) |
| `macula_gateway_dht.erl` | DHT query forwarding to routing subsystem |
| `macula_gateway_quic_server.erl` | Low-level QUIC server configuration |
| `macula_gateway_health.erl` | Health check endpoints |
| `macula_gateway_diagnostics.erl` | Diagnostic utilities |
| `macula_gateway_system.erl` | Application supervisor for gateway subsystem |
| `macula_gateway_workers_sup.erl` | Dynamic worker supervisor |

## Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                    macula_gateway_system                     │
│                      (application_sup)                       │
└───────────────────────────┬──────────────────────────────────┘
                            │
            ┌───────────────┴───────────────┐
            │                               │
   ┌────────▼────────┐           ┌──────────▼──────────┐
   │  macula_gateway │           │ macula_gateway_sup  │
   │   (gen_server)  │           │    (supervisor)     │
   └────────┬────────┘           └──────────┬──────────┘
            │                               │
            │              ┌────────────────┼────────────────┐
            │              │                │                │
            │     ┌────────▼────────┐ ┌─────▼─────┐ ┌────────▼────────┐
            │     │ client_manager  │ │  pubsub   │ │      rpc        │
            │     └─────────────────┘ └───────────┘ └─────────────────┘
            │
   ┌────────▼────────┐
   │   QUIC Listener │
   │  (quicer_nif)   │
   └─────────────────┘
```

## Message Flow

### Incoming Client Connection
```
QUIC Accept → client_manager:add_client → assign stream ID → track lifecycle
```

### PubSub Message Flow
```
PUBLISH → pubsub:route → DHT lookup → direct delivery to subscribers
```

### RPC Message Flow
```
CALL → rpc:handle → DHT lookup → forward to provider → REPLY back
```

## Configuration

| Environment Variable | Default | Description |
|---------------------|---------|-------------|
| `MACULA_QUIC_PORT` | 9443 | QUIC listener port |
| `MACULA_MAX_CLIENTS` | 10000 | Max concurrent clients |
| `MACULA_MESH_POOL_SIZE` | 1000 | Max mesh connections |

## Connection Pool

The gateway maintains a bounded connection pool for mesh communications:
- **LRU eviction**: Oldest connections removed when pool is full
- **94.5% hit rate**: Most requests served from pool
- **Max 1,000 connections**: Prevents unbounded memory growth

## Tests

See: `test/macula_gateway_system/`

Run gateway tests:
```bash
rebar3 eunit --module=macula_gateway_tests
rebar3 eunit --module=macula_gateway_client_manager_tests
rebar3 eunit --module=macula_gateway_pubsub_tests
rebar3 eunit --module=macula_gateway_rpc_tests
```
