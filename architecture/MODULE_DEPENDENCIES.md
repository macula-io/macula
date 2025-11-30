# Macula HTTP/3 Mesh - Module Dependencies

**Version**: 1.0 (Current Implementation)
**Last Updated**: November 11, 2025
**Status**: Reflects actual implemented architecture

## Architecture Overview

Macula uses a **layered architecture** separating transport concerns from business logic:

```
┌─────────────────────────────────────────────────────────────┐
│                    APPLICATION LAYER                         │
│  (Your Elixir/Erlang Applications using macula_sdk)        │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                      CLIENT SDK                              │
│  ┌────────────────────────────────────────────────────┐    │
│  │  macula_connection  (Connection management)        │    │
│  │  - RPC call() / register()                         │    │
│  │  - Pub/Sub publish() / subscribe()                 │    │
│  │  - Lifecycle management                            │    │
│  └────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                  BUSINESS LOGIC LAYER                        │
│  ┌───────────────────────┐  ┌─────────────────────────┐    │
│  │  macula_rpc_server    │  │  macula_pubsub_server   │    │
│  │  ─────────────────    │  │  ─────────────────────  │    │
│  │  - Local registry     │  │  - Subscription registry│    │
│  │  - DHT discovery      │  │  - Topic caching        │    │
│  │  - Result caching     │  │  - DHT discovery        │    │
│  │  - Smart routing:     │  │  - Pattern matching     │    │
│  │    • local_first      │  │  - Delivery to          │    │
│  │    • round_robin      │  │    subscribers          │    │
│  │    • random           │  │                         │    │
│  │  - Execution          │  │                         │    │
│  └───────────────────────┘  └─────────────────────────┘    │
│              ↓                          ↓                    │
│  ┌───────────────────────────────────────────────────┐     │
│  │  macula_dht_server  (Service Discovery)           │     │
│  │  ──────────────────────────────────────────       │     │
│  │  - Kademlia DHT implementation                    │     │
│  │  - FIND_NODE / FIND_VALUE / STORE                 │     │
│  │  - k-bucket routing table                         │     │
│  │  - XOR distance metric                            │     │
│  └───────────────────────────────────────────────────┘     │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                    TRANSPORT LAYER                           │
│  ┌────────────────────────────────────────────────────┐    │
│  │  macula_gateway  (QUIC Gateway)                    │    │
│  │  ──────────────────────────────────                │    │
│  │  - HTTP/3 listener (port 9443)                     │    │
│  │  - QUIC connection management                      │    │
│  │  - Stream multiplexing                             │    │
│  │  - Message routing:                                │    │
│  │    • RPC calls → macula_rpc_server                 │    │
│  │    • Pub/Sub → track subscriptions                 │    │
│  │    • Pub/Sub → distribute to subscribers           │    │
│  │  - Simple in-memory state:                         │    │
│  │    • connections (clients)                         │    │
│  │    • subscriptions (topic → [streams])             │    │
│  │    • registrations (procedure → client_pid)        │    │
│  └────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                    PROTOCOL LAYER                            │
│  ┌────────────────────────────────────────────────────┐    │
│  │  macula_protocol_encoder / decoder                 │    │
│  │  ──────────────────────────────────────────        │    │
│  │  - MessagePack encoding/decoding                   │    │
│  │  - Message framing (8-byte header + payload)       │    │
│  │  - Type validation                                 │    │
│  └────────────────────────────────────────────────────┘    │
│  ┌────────────────────────────────────────────────────┐    │
│  │  macula_protocol_types                             │    │
│  │  ──────────────────────────                        │    │
│  │  - Message type definitions                        │    │
│  │  - Type IDs (0x01-0x44)                            │    │
│  └────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                   QUIC TRANSPORT                             │
│  ┌────────────────────────────────────────────────────┐    │
│  │  macula_quic  (Wrapper around quicer/MsQuic)      │    │
│  │  ──────────────────────────────────────────        │    │
│  │  - Connection lifecycle                            │    │
│  │  - Stream send/receive                             │    │
│  │  - TLS certificate management                      │    │
│  └────────────────────────────────────────────────────┘    │
│                              ↓                               │
│  ┌────────────────────────────────────────────────────┐    │
│  │  quicer (NIF to Microsoft MsQuic)                  │    │
│  └────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

## Detailed Module Responsibilities

### Transport Layer

#### `macula_gateway` ⭐ **Core Transport Module**
**Purpose**: HTTP/3 QUIC listener and connection manager (like nginx or Cowboy)

**Responsibilities**:
- Listen on configured port (default 9443)
- Accept QUIC connections from clients
- Manage bidirectional streams
- Route messages to appropriate handlers:
  - RPC → `macula_rpc_server`
  - DHT → `macula_dht_server`
  - Pub/Sub → track and distribute locally
- **Simple in-memory tracking**:
  - Connected clients
  - Topic subscriptions (topic → list of stream refs)
  - RPC registrations (procedure → client PID)

**Key Insight**: Gateway is **dumb routing** - it doesn't understand business logic, just moves bytes and maintains simple maps.

---

### Business Logic Layer

#### `macula_rpc_server` ⭐ **RPC Orchestration**
**Purpose**: Full-featured RPC service discovery and execution (like Istio service mesh)

**Responsibilities**:
- **Local Registry**: Track procedures registered by local clients
- **Caching**: Remember where procedures were found (avoid DHT lookups)
- **DHT Discovery**: Query `macula_dht_server` to find remote procedures
- **Smart Routing Strategies**:
  - `local_first`: Prefer local handlers
  - `round_robin`: Distribute load across multiple providers
  - `random`: Random selection
- **Execution**: Invoke local handlers and return results
- **Error Handling**: Timeouts, procedure not found, execution errors

**Dependencies**:
- `macula_dht_server` for service discovery
- `macula_gateway` for network communication

---

#### `macula_pubsub_server` ⭐ **Pub/Sub Orchestration**
**Purpose**: Full-featured pub/sub service discovery and delivery (like RabbitMQ/Kafka)

**Responsibilities**:
- **Subscription Registry**: Track local subscriptions with pattern matching
- **Topic Caching**: Remember subscriber locations
- **DHT Discovery**: Find remote subscribers via DHT queries
- **Pattern Matching**: Support wildcards (e.g., `sensor.*.temperature`)
- **Message Delivery**: Route published messages to all matching subscribers
- **QoS Management**: Handle delivery guarantees

**Dependencies**:
- `macula_dht_server` for subscriber discovery
- `macula_gateway` for network communication

**Current Status**:
✅ Gateway-level distribution working (messages sent to local subscribers)
⚠️ Full DHT-based discovery and remote subscriber distribution in progress

---

#### `macula_dht_server` ⭐ **Service Discovery**
**Purpose**: Kademlia distributed hash table for peer-to-peer service discovery

**Responsibilities**:
- **Node Management**: Maintain routing table of known peers (k-buckets)
- **Distance Metric**: XOR-based distance calculation for key routing
- **DHT Operations**:
  - `FIND_NODE`: Locate peers close to a key
  - `FIND_VALUE`: Retrieve value stored at a key
  - `STORE`: Store key-value pairs
- **Service Registration**: Store RPC procedures and pub/sub topics
- **Peer Discovery**: Integrate with mDNS for local network discovery

---

### Protocol Layer

#### `macula_protocol_encoder` / `macula_protocol_decoder`
**Purpose**: Message serialization and deserialization

**Responsibilities**:
- Encode Erlang/Elixir maps to binary MessagePack format
- Decode binary MessagePack to Erlang/Elixir maps
- Add 8-byte frame header (version, type, flags, length)
- Validate message structure for each type

**Wire Format**:
```
┌──────────┬──────────┬──────────┬──────────┬─────────────────┬───────────┐
│ Version  │  Type ID │  Flags   │ Reserved │ Payload Length  │  Payload  │
│  (1 byte)│ (1 byte) │ (1 byte) │ (1 byte) │   (4 bytes BE)  │ (N bytes) │
└──────────┴──────────┴──────────┴──────────┴─────────────────┴───────────┘
```

---

#### `macula_protocol_types`
**Purpose**: Type definitions and constants

**Responsibilities**:
- Define message types (connect, disconnect, publish, subscribe, call, etc.)
- Map type names to numeric IDs (0x01 = connect, 0x10 = publish, 0x20 = call)
- Provide type specifications for validation

---

### QUIC Transport Layer

#### `macula_quic`
**Purpose**: Erlang-friendly wrapper around `quicer` NIF

**Responsibilities**:
- Start/stop QUIC listeners
- Manage connection lifecycle
- Send/receive data on streams
- Handle TLS certificates
- Abstract away NIF complexity

---

#### `quicer` (External Dependency)
**Purpose**: Erlang NIF bindings to Microsoft MsQuic

**Provides**: Low-level QUIC protocol implementation

---

### Client SDK

#### `macula_connection`
**Purpose**: High-level client API for applications

**Public API**:
```erlang
%% Connection
{ok, Conn} = macula_connection:start_link(Url, Options).

%% RPC
{ok, Result} = macula_connection:call(Conn, Procedure, Args).
ok = macula_connection:register(Conn, Procedure, Handler).

%% Pub/Sub
ok = macula_connection:publish(Conn, Topic, Message).
{ok, SubRef} = macula_connection:subscribe(Conn, Topic, Callback).
ok = macula_connection:unsubscribe(Conn, SubRef).
```

---

## Layering Philosophy

### Gateway = Transport (Nginx/Cowboy)
- **Stateless request routing**
- Simple maps for connection tracking
- No business logic
- Scales horizontally easily

### RPC/PubSub Servers = Business Logic (Istio/RabbitMQ)
- **Stateful orchestration**
- DHT integration
- Caching and optimization
- Smart routing decisions
- Delivery guarantees

### Separation Benefits

1. **Incremental Feature Addition**: Can add features to RPC/PubSub servers without touching gateway
2. **Independent Scaling**: Scale transport separate from business logic
3. **Testing**: Can test business logic without network layer
4. **Observability**: Clear boundaries for metrics and tracing
5. **Evolution**: Can replace gateway (e.g., add WebSocket support) without changing RPC/PubSub logic

---

## Message Flow Examples

### RPC Call Flow
```
Application
    │
    ├─> macula_connection:call("math.add", [1, 2])
    │
    └─> macula_quic:send(CallMessage)
            │
            └─> [Network: QUIC Stream]
                    │
                    └─> macula_gateway receives
                            │
                            ├─> Decodes message
                            └─> Routes to macula_rpc_server
                                    │
                                    ├─> Check local registry
                                    ├─> Query DHT if needed
                                    ├─> Select provider (routing strategy)
                                    ├─> Execute handler
                                    └─> Return result
                                            │
                                            └─> macula_gateway encodes reply
                                                    │
                                                    └─> [Network: QUIC Stream]
                                                            │
                                                            └─> Application receives result
```

### Pub/Sub Flow (Current Implementation)
```
Publisher                                          Subscribers
    │                                                  │
    ├─> macula_connection:publish(Topic, Msg)        │
    │                                                  │
    └─> macula_quic:send(PublishMessage)             │
            │                                          │
            └─> [Network: QUIC Stream]                │
                    │                                  │
                    └─> macula_gateway:               │
                         handle_publish()              │
                            │                          │
                            ├─> Find subscribers       │
                            │   in subscriptions map   │
                            │                          │
                            ├─> Encode message         │
                            │                          │
                            └─> Send to each           │
                                subscriber stream ─────┼─> Subscriber 1 stream
                                                       │
                                                       └─> Subscriber 2 stream
                                                                │
                                                                └─> Callback invoked
                                                                    (when client-side
                                                                     handling complete)
```

---

## Current Implementation Status

### ✅ Fully Implemented
- HTTP/3/QUIC transport
- RPC with DHT discovery
- Gateway-level pub/sub distribution
- Protocol encoding/decoding
- Connection management
- Service registry

### 🚧 In Progress
- Client-side pub/sub message handling
- Cross-realm bridging
- Topic pattern matching (wildcards)

### 📋 Planned
- QoS levels (1, 2)
- Message retention
- Offline message delivery
- Clustering/replication
- Observability (metrics, tracing)

---

## Key Architectural Decisions

1. **HTTP/3/QUIC over WAMP**: NAT-friendly, modern transport, broader ecosystem
2. **Layered Architecture**: Separation of concerns enables independent evolution
3. **Kademlia DHT**: Decentralized service discovery without central registry
4. **MessagePack**: Efficient binary encoding with wide language support
5. **Pure Erlang/OTP**: No distributed Erlang dependency, explicit networking
6. **Realm-based Multi-tenancy**: Isolation at gateway level

---

## Comparison: Macula vs Distributed Erlang

See: [Does Macula Augment Distributed Erlang?](#distributed-erlang-relationship) section below.
