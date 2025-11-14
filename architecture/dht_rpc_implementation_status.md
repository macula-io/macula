# DHT-Routed RPC Implementation Status

## Summary

We discovered that the current RPC implementation violates mesh network principles by using direct client→provider connections. We're implementing a **hybrid strategy: multi-hop DHT routing with smart direct connection fallback**.

**Strategy**: See `architecture/hybrid_routing_strategy.md` for complete design.

**Current Focus**: Phase 1 - Multi-hop foundation (must work reliably before adding optimizations)

## Completed ✅

1. **Protocol Type** (`src/macula_protocol_types.erl`)
   - Added `rpc_route` message type (ID: `16#23`)
   - Added `rpc_route_msg()` type definition with:
     - `destination_node_id`: 32-byte target node
     - `source_node_id`: 32-byte originator
     - `hop_count`: Current hop count
     - `max_hops`: TTL protection (default: 10)
     - `payload_type`: `call` or `reply`
     - `payload`: The actual RPC message

2. **Protocol Encoding** (`src/macula_protocol_encoder.erl`)
   - Added validation for `rpc_route` messages
   - Supports both atom and binary keys (MessagePack compatibility)

3. **Architecture Documentation**
   - `architecture/dht_routed_rpc.md` - Complete architectural design
   - `architecture/dht_rpc_implementation_status.md` - This file

4. **Compilation** - All changes compile successfully

## In Progress ⏳

### DHT Routing Module

Need to create `src/macula_rpc_routing.erl` with:

```erlang
-module(macula_rpc_routing).
-export([
    wrap_call/4,           % Wrap CALL in rpc_route
    wrap_reply/4,          % Wrap REPLY in rpc_route
    route_or_deliver/3,    % Route or deliver message
    should_deliver_locally/2
]).
```

**Key Functions:**
- `wrap_call/4` - Wraps call_msg in rpc_route envelope
- `wrap_reply/4` - Wraps reply_msg in rpc_route envelope
- `route_or_deliver/3` - Decides: local delivery or forward to next hop
- Uses `macula_routing_server:find_closest/3` for Kademlia lookupImplementation: ~150 lines

### Gateway Integration (`src/macula_gateway.erl`)

**Add `rpc_route` message handling:**
```erlang
handle_decoded_message(rpc_route, RpcRouteMsg, Stream, State) ->
    LocalNodeId = State#state.node_id,
    RoutingServerPid = whereis(macula_routing_server),

    case macula_rpc_routing:route_or_deliver(LocalNodeId, RpcRouteMsg, RoutingServerPid) of
        {deliver, call, CallMsg} ->
            %% Unwrap and process CALL locally
            handle_decoded_message(call, CallMsg, Stream, State);
        {deliver, reply, _ReplyMsg} ->
            %% ERROR: Gateway shouldn't receive REPLY (goes to connection)
            {noreply, State};
        {forward, NextHopNodeInfo, UpdatedRpcRouteMsg} ->
            %% Forward to next hop
            forward_rpc_route(NextHopNodeInfo, UpdatedRpcRouteMsg, State);
        {error, Reason} ->
            %% Log error, drop message
            io:format("[Gateway] RPC route error: ~p~n", [Reason]),
            {noreply, State}
    end;
```

**Replace direct reply sending with routing:**
```erlang
%% OLD (direct stream send):
macula_quic:send(Stream, ReplyBinary)

%% NEW (DHT routing):
RpcRouteMsg = macula_rpc_routing:wrap_reply(
    LocalNodeId,
    SourceNodeId,  % From original CALL's source_node_id
    ReplyMsg,
    10  % max_hops
),
route_and_send(RpcRouteMsg, State)
```

### Connection Integration (`src/macula_connection.erl`)

**Replace direct endpoint connections with DHT routing:**

```erlang
%% OLD (creates new connection to provider):
get_or_create_endpoint_connection(Endpoint, State)

%% NEW (send via DHT routing):
do_remote_call(Procedure, Args, Opts, From, Providers, State) ->
    %% Pick any provider (they're all reachable via DHT)
    [#{node_id := ProviderNodeId} | _] = Providers,

    {CallId, State2} = next_message_id(State),
    CallMsg = #{...},

    %% Wrap in rpc_route and send via main stream
    LocalNodeId = State2#state.node_id,
    RpcRouteMsg = macula_rpc_routing:wrap_call(
        LocalNodeId,
        ProviderNodeId,
        CallMsg,
        10  % max_hops
    ),

    %% Send via existing main stream (to registry/DHT)
    send_message(rpc_route, RpcRouteMsg, State2),

    %% Store pending call
    {noreply, State3}
```

**Handle incoming routed REPLY:**

```erlang
handle_decoded_message(rpc_route, RpcRouteMsg, Stream, State) ->
    LocalNodeId = State#state.node_id,

    case macula_rpc_routing:should_deliver_locally(LocalNodeId, RpcRouteMsg) of
        true ->
            %% Unwrap and process REPLY
            #{payload_type := reply, payload := ReplyMsg} = RpcRouteMsg,
            handle_decoded_message(reply, ReplyMsg, Stream, State);
        false ->
            %% This shouldn't happen (connection only receives its own replies)
            {noreply, State}
    end;
```

### Code Removal

**Delete direct endpoint connection code:**
- `get_or_create_endpoint_connection/2`
- `endpoint_connections` state field
- All endpoint stream management

Estimated: ~500 lines removed

## Pending 📋

### Multi-Hop Test Topology

Create `docker/docker-compose.mesh-test.yml` with true mesh requiring multi-hop:

```
   NodeA ←→ NodeB
     ↓         ↓
  Client    NodeC ←→ Provider

Client → NodeA → NodeB → NodeC → Provider (3 hops!)
```

Each node:
- Runs macula_gateway
- Participates in DHT
- Configured with limited peer connections to force multi-hop

### E2E Test

Create `test/e2e/run-mesh-rpc-test.sh`:
- Start 5-node mesh (client, 3 intermediate, 1 provider)
- Provider advertises service
- Client makes RPC call
- Verify:
  - Call routes through multiple hops
  - Reply routes back through mesh
  - Success within timeout

### Documentation Updates

Update `CLAUDE.md` to reference:
- `architecture/dht_routed_rpc.md`
- New routing architecture
- Remove references to direct connections

## Benefits

After completion:

✅ **True P2P Mesh** - No client/server distinction
✅ **Scalability** - O(log N) routing hops
✅ **NAT Traversal** - Works through existing connections
✅ **Connection Efficiency** - No new connections per call
✅ **Fault Tolerance** - Routes around failures

## Effort Estimate

- **Routing Module**: 2-3 hours
- **Gateway Integration**: 3-4 hours
- **Connection Integration**: 4-5 hours
- **Code Removal**: 1-2 hours
- **Test Topology**: 2-3 hours
- **E2E Testing**: 2-3 hours
- **Documentation**: 1 hour

**Total**: ~15-20 hours of focused development

## Next Steps

1. Create `macula_rpc_routing.erl` module
2. Integrate routing into gateway
3. Integrate routing into connection
4. Remove direct-connection code
5. Create multi-hop test topology
6. Run E2E tests
7. Update documentation

This is the correct architectural direction for a true mesh network!
