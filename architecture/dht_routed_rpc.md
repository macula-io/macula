# DHT-Routed RPC Architecture

> **⚠️ HISTORICAL DOCUMENT (v0.7.x Planning)**
>
> This document describes the **originally planned** multi-hop DHT routing approach.
> **v0.8.0 took a different approach**: direct P2P connections via `macula_peer_connector`
> with DHT used only for service/subscriber propagation.
>
> **Current Implementation (v0.8.0+)**:
> - RPC uses direct QUIC connections to providers
> - DHT stores provider locations (not used for message routing)
> - No multi-hop message forwarding
> - See `v0.8.0-OVERVIEW.md` for current architecture
>
> **Why the change?** Direct connections provide:
> - Lower latency (1 hop vs O(log N) hops)
> - Simpler debugging
> - Better throughput
> - NAT traversal via gateway relay (acceptable for v0.8.x)
>
> This document is preserved for historical reference and may inform future
> optimization if multi-hop becomes beneficial.

---

## Problem Statement

The initial RPC implementation used a **client/server model** where clients created direct QUIC connections to providers after DHT discovery. This violated the mesh network architecture principles:

❌ **Problems with Direct Connection Model:**
- O(N²) connection scaling
- Not truly decentralized (providers act as servers)
- NAT traversal problems (providers must be directly reachable)
- Connection idle timeout issues
- Violates mesh network principles

## Solution: Multi-Hop DHT Routing

✅ **DHT-Routed RPC Benefits:**
- True peer-to-peer mesh architecture
- Uses existing DHT neighbor connections
- No new connections needed
- NAT-friendly (routes through mesh)
- Scales to thousands of nodes
- Aligns with Kademlia principles

## Architecture

### Message Flow

```
Client                  Node A                  Node B                  Provider
  |                       |                       |                        |
  |--rpc_route(CALL)----->|                       |                        |
  |  dest: Provider       |--rpc_route(CALL)----->|                        |
  |  payload: {proc,args} |  (forward to closer)  |--rpc_route(CALL)------>|
  |                       |                       |                        |
  |                       |                       |                        |  Process CALL
  |                       |                       |                        |
  |                       |                       |<--rpc_route(REPLY)-----|
  |                       |<--rpc_route(REPLY)----|  dest: Client         |
  |<--rpc_route(REPLY)----|  (forward to closer)  |  payload: {result}    |
  |                       |                       |                        |
```

### Routing Algorithm

Each node receiving an `rpc_route` message:

1. **Check if local delivery:**
   ```erlang
   if destination_node_id == local_node_id ->
       unwrap_and_deliver_locally(rpc_route);
   ```

2. **Find next hop:**
   ```erlang
   NextHop = find_closest_node_to(destination_node_id, routing_table),
   forward_to(NextHop, rpc_route_with_incremented_hop_count)
   ```

3. **TTL protection:**
   ```erlang
   if hop_count >= max_hops ->
       return {error, max_hops_exceeded}
   ```

### Message Structure

```erlang
-type rpc_route_msg() :: #{
    destination_node_id := binary(),  % 32-byte final destination
    source_node_id := binary(),       % 32-byte original sender
    hop_count := non_neg_integer(),   % Current hop count
    max_hops := pos_integer(),        % TTL (default: 10)
    payload_type := call | reply,     % Wrapped message type
    payload := call_msg() | reply_msg()  % Actual RPC message
}.
```

## Implementation Plan

### 1. Protocol Layer (`macula_protocol_encoder.erl`, `macula_protocol_decoder.erl`)

Add encoding/decoding for `rpc_route` messages (MessagePack format).

### 2. Routing Layer (NEW: `macula_rpc_router.erl`)

Create dedicated module for routing logic:
```erlang
-module(macula_rpc_router).
-export([
    route_message/2,        % Route rpc_route to next hop
    wrap_call/3,            % Wrap CALL in rpc_route envelope
    wrap_reply/3,           % Wrap REPLY in rpc_route envelope
    unwrap_and_deliver/2    % Unwrap and deliver locally
]).
```

### 3. Gateway Layer (`macula_gateway.erl`)

- Handle incoming `rpc_route` messages
- Route or deliver based on destination
- Send REPLY messages via `rpc_route` instead of direct stream

### 4. Connection Layer (`macula_connection.erl`)

- Send CALL messages via `rpc_route` instead of direct endpoint connection
- Remove `get_or_create_endpoint_connection` logic
- Handle REPLY messages received via routing

### 5. Test Topology

Update test to have **true mesh** with multi-hop routing required:

```
Registry ←→ NodeA ←→ NodeB ←→ Provider1
             ↓                     ↓
           Client              Provider2
                                   ↓
                              Provider3
```

Client → NodeA → NodeB → Provider (2 hops minimum)

## Migration Path

1. ✅ Add `rpc_route` protocol type
2. ⏳ Implement routing logic
3. ⏳ Update gateway to route messages
4. ⏳ Update connection to use routing
5. ⏳ Update test topology
6. ⏳ Verify multi-hop E2E test
7. 🔮 Remove old direct-connection code

## Benefits Realized

After migration:

- **Decentralization**: True mesh, no client/server distinction
- **Scalability**: O(log N) routing hops via Kademlia
- **NAT Traversal**: Works through existing mesh connections
- **Connection Efficiency**: No new connections per RPC call
- **Fault Tolerance**: Routes around failed nodes automatically

## References

- Kademlia Paper: https://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf
- `src/macula_protocol_types.erl:156-163` - rpc_route_msg definition
- `src/macula_routing_dht.erl` - Existing Kademlia DHT algorithms
