# Macula Routing Subsystem

This directory contains all modules related to the Kademlia-based DHT routing subsystem.

## Overview

The routing subsystem implements a Kademlia distributed hash table (DHT) for:
- Service provider discovery (RPC procedures)
- Subscriber discovery (PubSub topics)
- Node lookup and routing
- Key-value storage across the mesh

## Modules

| Module | Purpose |
|--------|---------|
| `macula_routing_server.erl` | Gen_server managing the DHT state, handles store/find/delete operations |
| `macula_routing_table.erl` | Routing table implementation with k-buckets |
| `macula_routing_bucket.erl` | Individual k-bucket management (node info storage) |
| `macula_routing_dht.erl` | Pure functions for DHT algorithms (iterative lookup, store) |
| `macula_routing_protocol.erl` | Wire protocol encoding/decoding for DHT messages |
| `macula_routing_nodeid.erl` | Node ID generation and XOR distance calculations |

## Architecture

```
                    ┌─────────────────────────┐
                    │  macula_routing_server  │
                    │      (gen_server)       │
                    └───────────┬─────────────┘
                                │
            ┌───────────────────┼───────────────────┐
            │                   │                   │
   ┌────────▼────────┐ ┌────────▼────────┐ ┌───────▼────────┐
   │ routing_table   │ │  routing_dht    │ │routing_protocol│
   │ (k-buckets)     │ │ (algorithms)    │ │  (wire format) │
   └────────┬────────┘ └─────────────────┘ └────────────────┘
            │
   ┌────────▼────────┐
   │ routing_bucket  │
   │  (node storage) │
   └─────────────────┘
```

## Key Parameters

- **K = 20**: Replication factor (nodes per key)
- **Alpha = 3**: Concurrent queries during iterative lookup
- **Bucket Size = 20**: Max nodes per k-bucket
- **Node ID = 256 bits**: SHA-256 hash of node identifier

## Usage

### Storing a value
```erlang
%% Store via routing server
macula_routing_server:store(ServerPid, Key, Value).

%% Or via simple interface (finds server automatically)
macula_routing_dht:store(Key, Value).
```

### Finding a value
```erlang
%% Find via routing server
{ok, Value} = macula_routing_server:find_value(ServerPid, Key, #{}).

%% Or via simple interface
{ok, Value} = macula_routing_dht:find(Key).
```

### Subscribing to DHT events
```erlang
%% Subscribe to keys matching a prefix
macula_routing_dht:subscribe(<<"service:">>, self()).

%% Receive events:
%% {dht_stored, Key, Value} - When a key is stored
%% {dht_deleted, Key} - When a key is deleted
```

## Message Types

| Type | ID | Purpose |
|------|-----|---------|
| FIND_NODE | 0x40 | Request nodes closest to target |
| FIND_NODE_REPLY | 0x41 | Response with closest nodes |
| STORE | 0x42 | Store key-value pair |
| FIND_VALUE | 0x43 | Request value for key |
| FIND_VALUE_REPLY | 0x44 | Response with value or closer nodes |

## Integration

The routing subsystem integrates with:
- **Gateway System**: Receives STORE/FIND messages from peers
- **PubSub System**: Stores subscriber endpoints
- **RPC System**: Stores service provider endpoints
- **Bridge System**: Escalates queries to parent mesh levels

## Tests

See: `test/macula_routing_system/`

Run routing tests:
```bash
rebar3 eunit --module=macula_routing_server_tests
rebar3 eunit --module=macula_routing_table_tests
rebar3 eunit --module=macula_routing_dht_tests
```
