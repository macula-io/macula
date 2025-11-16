# DHT-Routed Pub/Sub Architecture

## Problem Statement

The v0.7.7 pub/sub implementation has a **split-brain architecture** where subscribers register in two places:

❌ **Problems with Hub-and-Spoke Pub/Sub:**
- Subscribers register locally on their own `macula_pubsub_handler`
- Gateway tracks subscriptions in separate `macula_gateway_pubsub`
- Publishers query local handler (knows nothing about remote subscribers)
- Gateway query returns 0 subscribers despite successful subscriptions
- DHT returns gateway's endpoint instead of actual subscriber nodes
- Violates mesh network principles (hub-and-spoke bottleneck)

**Concrete Example (Macula Arcade Matchmaking):**
```
Peer1 subscribes to "arcade.matchmaking.snake"
  → Stores in Peer1's local macula_pubsub_handler
  → Sends SUBSCRIBE to gateway
  → Gateway stores in macula_gateway_pubsub

Peer2 publishes to "arcade.matchmaking.snake"
  → Queries Peer2's local macula_pubsub_handler
  → Finds 0 subscribers (only knows about itself)
  → Message never reaches Peer1
  → Matchmaking stuck on "Looking for opponent..."
```

## Solution: Multi-Hop DHT Routing (v0.7.8)

✅ **DHT-Routed Pub/Sub Benefits:**
- True peer-to-peer mesh architecture
- Uses existing DHT neighbor connections
- No new connections needed (works with relay)
- NAT-friendly (routes through mesh)
- Scales to thousands of nodes
- Aligns with Kademlia principles
- **Same pattern as `dht_routed_rpc.md`**

## Architecture

### Message Flow

```
Publisher              Node A              Node B              Subscriber
  |                      |                    |                     |
  |--pubsub_route------->|                    |                     |
  |  dest: Subscriber    |--pubsub_route----->|                     |
  |  topic: "matchmaking"|  (forward closer)  |--pubsub_route------>|
  |  payload: {msg}      |                    |                     |
  |                      |                    |                     |  Deliver locally
```

### Subscription Advertisement

```
Subscriber subscribes to topic "arcade.matchmaking.snake"
  ↓
Compute DHT key = crypto:hash(sha256, Topic)
  ↓
Advertise to DHT: Key → #{node_id, endpoint, ttl}
  ↓
Refresh every 4 minutes (5 min TTL)
```

### Publishing Flow

```
Publisher wants to publish to "arcade.matchmaking.snake"
  ↓
Compute DHT key = crypto:hash(sha256, Topic)
  ↓
Query DHT for subscribers: find_value(Key)
  ↓
Returns: [#{node_id => SubA}, #{node_id => SubB}, ...]
  ↓
For each subscriber:
  Wrap PUBLISH in pubsub_route envelope
  Send to local gateway connection
  Gateway routes via Kademlia XOR distance
  ↓
Subscriber receives pubsub_route
  ↓
Unwrap and deliver PUBLISH locally
```

### Routing Algorithm

Each node receiving a `pubsub_route` message:

1. **Check if local delivery:**
   ```erlang
   case destination_node_id =:= local_node_id of
       true ->
           unwrap_and_deliver_locally(pubsub_route);
       false ->
           route_to_next_hop(pubsub_route)
   end
   ```

2. **Find next hop:**
   ```erlang
   NextHop = find_closest_node_to(destination_node_id, routing_table),
   forward_to(NextHop, pubsub_route_with_incremented_hop_count)
   ```

3. **TTL protection:**
   ```erlang
   case hop_count >= max_hops of
       true -> {error, max_hops_exceeded};
       false -> forward_message()
   end
   ```

### Message Structure

```erlang
-type pubsub_route_msg() :: #{
    destination_node_id := binary(),  % 32-byte subscriber node ID
    source_node_id := binary(),       % 32-byte publisher node ID
    hop_count := non_neg_integer(),   % Current hop count
    max_hops := pos_integer(),        % TTL (default: 10)
    topic := binary(),                % Pub/sub topic
    payload := publish_msg()          % Actual PUBLISH message
}.

-type publish_msg() :: #{
    topic := binary(),
    qos := 0 | 1 | 2,
    payload := term()
}.
```

## Implementation Plan

### 1. Protocol Layer (SMALL)

**Files**: `macula_protocol_encoder.erl`, `macula_protocol_decoder.erl`

Add `pubsub_route` message type (MessagePack format):

```erlang
%% In macula_protocol_encoder.erl
encode(pubsub_route, Msg) ->
    macula_msgpack:pack(#{
        <<"type">> => <<"pubsub_route">>,
        <<"destination_node_id">> => maps:get(destination_node_id, Msg),
        <<"source_node_id">> => maps:get(source_node_id, Msg),
        <<"hop_count">> => maps:get(hop_count, Msg, 0),
        <<"max_hops">> => maps:get(max_hops, Msg, 10),
        <<"topic">> => maps:get(topic, Msg),
        <<"payload">> => maps:get(payload, Msg)
    }).

%% In macula_protocol_decoder.erl
decode_message_type(<<"pubsub_route">>, Data) ->
    {ok, {pubsub_route, Data}}.
```

### 2. Routing Layer (MEDIUM - NEW MODULE)

**File**: `macula_pubsub_router.erl` (NEW)

Clone from `macula_gateway_rpc_router.erl` pattern:

```erlang
-module(macula_pubsub_router).
-export([
    wrap_publish/3,         % Wrap PUBLISH in pubsub_route envelope
    route_message/3,        % Route pubsub_route to next hop
    unwrap_and_deliver/2    % Unwrap and deliver locally
]).

%% @doc Wrap PUBLISH message in routing envelope.
-spec wrap_publish(
    DestinationNodeId :: binary(),
    SourceNodeId :: binary(),
    PublishMsg :: map()
) -> map().
wrap_publish(DestNodeId, SrcNodeId, PubMsg) ->
    Topic = maps:get(<<"topic">>, PubMsg),
    #{
        destination_node_id => DestNodeId,
        source_node_id => SrcNodeId,
        hop_count => 0,
        max_hops => 10,
        topic => Topic,
        payload => PubMsg
    }.

%% @doc Route pubsub_route message to next hop or deliver locally.
-spec route_message(
    RouteMsg :: map(),
    LocalNodeId :: binary(),
    MeshPid :: pid()
) -> ok | {error, term()}.
route_message(RouteMsg, LocalNodeId, MeshPid) ->
    DestNodeId = maps:get(destination_node_id, RouteMsg),
    HopCount = maps:get(hop_count, RouteMsg, 0),
    MaxHops = maps:get(max_hops, RouteMsg, 10),

    %% Check TTL
    case HopCount >= MaxHops of
        true ->
            io:format("[PubSubRouter] Max hops exceeded (~p >= ~p)~n",
                     [HopCount, MaxHops]),
            {error, max_hops_exceeded};
        false ->
            route_or_deliver(RouteMsg, DestNodeId, LocalNodeId, HopCount, MeshPid)
    end.

%% @private Route to next hop or deliver locally.
route_or_deliver(RouteMsg, DestNodeId, LocalNodeId, HopCount, MeshPid)
  when DestNodeId =:= LocalNodeId ->
    %% Destination reached - deliver locally
    unwrap_and_deliver(RouteMsg, self());
route_or_deliver(RouteMsg, DestNodeId, _LocalNodeId, HopCount, MeshPid) ->
    %% Forward to next hop
    case macula_gateway_mesh:find_closest_peer(MeshPid, DestNodeId) of
        {ok, NextHopPid} ->
            %% Increment hop count and forward
            UpdatedMsg = maps:put(hop_count, HopCount + 1, RouteMsg),
            RouteBinary = macula_protocol_encoder:encode(pubsub_route, UpdatedMsg),
            macula_quic:send(NextHopPid, RouteBinary);
        {error, no_route} ->
            io:format("[PubSubRouter] No route to destination ~p~n", [DestNodeId]),
            {error, no_route}
    end.

%% @doc Unwrap routing envelope and deliver PUBLISH locally.
-spec unwrap_and_deliver(RouteMsg :: map(), LocalPubSubPid :: pid()) -> ok.
unwrap_and_deliver(RouteMsg, LocalPubSubPid) ->
    PublishMsg = maps:get(payload, RouteMsg),
    macula_pubsub_handler:deliver_publish(LocalPubSubPid, PublishMsg).
```

### 3. Pub/Sub Handler (MEDIUM - MODIFY EXISTING)

**File**: `macula_pubsub_handler.erl`

Update publishing to use DHT routing:

```erlang
%% Current (BROKEN):
handle_publish(Topic, Payload, State) ->
    %% Only queries LOCAL subscribers
    Subscribers = maps:get(Topic, State#state.subscriptions, []),
    deliver_to_subscribers(Subscribers, Topic, Payload),
    {noreply, State}.

%% New (DHT-ROUTED):
handle_publish(Topic, Payload, State) ->
    %% 1. Deliver to local subscribers (fast path)
    LocalSubscribers = maps:get(Topic, State#state.subscriptions, []),
    deliver_to_subscribers(LocalSubscribers, Topic, Payload),

    %% 2. Query DHT for remote subscribers
    TopicKey = crypto:hash(sha256, Topic),
    RemoteSubscribers = case macula_dht:find_value(TopicKey) of
        {ok, Subs} -> Subs;
        {error, not_found} -> []
    end,

    %% 3. Route to each remote subscriber via pubsub_route
    LocalNodeId = State#state.node_id,
    lists:foreach(fun(Sub) ->
        DestNodeId = maps:get(<<"node_id">>, Sub),
        PublishMsg = #{
            <<"topic">> => Topic,
            <<"qos">> => 0,
            <<"payload">> => Payload
        },
        RouteMsg = macula_pubsub_router:wrap_publish(
            DestNodeId, LocalNodeId, PublishMsg
        ),
        RouteBinary = macula_protocol_encoder:encode(pubsub_route, RouteMsg),
        macula_quic:send(State#state.gateway_stream, RouteBinary)
    end, RemoteSubscribers),

    {noreply, State}.
```

### 4. Gateway Layer (SMALL - ADD ROUTING CASE)

**File**: `macula_gateway.erl`

Add handler for `pubsub_route` messages:

```erlang
%% In handle_decoded_message/3
handle_decoded_message({ok, {pubsub_route, RouteMsg}}, _Stream, State) ->
    io:format("[Gateway] Received PUBSUB_ROUTE message~n"),
    LocalNodeId = State#state.node_id,
    MeshPid = State#state.mesh,

    %% Route or deliver
    macula_pubsub_router:route_message(RouteMsg, LocalNodeId, MeshPid),

    {noreply, State};
```

### 5. Subscription Advertisement (ALREADY EXISTS)

**No changes needed!** `macula_advertisement_manager.erl` already advertises subscriptions to DHT:

```erlang
%% In macula_advertisement_manager.erl (ALREADY WORKING)
advertise_subscription(Topic, NodeId, Endpoint) ->
    TopicKey = crypto:hash(sha256, Topic),
    Value = #{
        <<"node_id">> => NodeId,
        <<"endpoint">> => Endpoint,
        <<"ttl">> => 300  % 5 minutes
    },
    macula_pubsub_dht:store(TopicKey, Value).
```

## Testing Strategy

### Unit Tests

1. **Protocol encoding/decoding** (`macula_protocol_tests.erl`)
   - Encode `pubsub_route` message
   - Decode `pubsub_route` message
   - Round-trip integrity

2. **Routing logic** (`macula_pubsub_router_tests.erl`)
   - Wrap PUBLISH in envelope
   - Route to next hop (increment hop_count)
   - Deliver locally when destination matches
   - Reject when max_hops exceeded

3. **Pub/sub handler** (`macula_pubsub_handler_tests.erl`)
   - Publish to local subscribers (fast path)
   - Query DHT for remote subscribers
   - Send pubsub_route for each remote

### Integration Tests

4. **Multi-node pub/sub** (`test/e2e/run-mesh-pubsub-test.sh`)
   - 3 peers subscribe to same topic
   - 1 peer publishes
   - All 3 receive message
   - Verify routing hops

### End-to-End Test

5. **Matchmaking** (Macula Arcade)
   - Open 3 browser tabs
   - All subscribe to `arcade.matchmaking.snake`
   - One clicks "Find Game"
   - All three receive matchmaking event
   - Game starts successfully

## Migration Strategy

### Phase 1: Parallel Implementation (v0.7.8-alpha)
- Add `pubsub_route` alongside existing PUBLISH
- Both paths active (redundancy)
- Feature flag: `dht_routed_pubsub_enabled`

### Phase 2: Gradual Rollout (v0.7.8-beta)
- Enable DHT routing by default
- Keep local-only as fallback
- Monitor metrics

### Phase 3: Remove Legacy (v0.7.9)
- Remove `macula_gateway_pubsub` (no longer needed)
- Remove local PUBLISH forwarding
- Pure DHT routing

## Performance Characteristics

### Latency

**Local Subscribers** (same node):
- 0 hops - immediate delivery
- Latency: < 1ms

**Remote Subscribers** (relay via gateway):
- Average: 2 hops (peer → gateway → peer)
- Latency: 5-10ms

**Remote Subscribers** (direct P2P - v0.8.0+):
- Average: 1 hop (peer → peer)
- Latency: 2-5ms

### Scalability

**Topic Fanout**:
- N subscribers = N `pubsub_route` messages
- Linear scaling with subscriber count
- Each route independent (parallel delivery)

**DHT Lookup**:
- O(log N) lookup time (Kademlia)
- Cached for 5 minutes (TTL)
- Refresh on expiry

## Comparison to v0.7.7

| Aspect | v0.7.7 (Broken) | v0.7.8 (DHT-Routed) |
|--------|-----------------|---------------------|
| Architecture | Hub-and-spoke split-brain | True P2P mesh |
| Subscriber discovery | Local only (fails) | DHT + local |
| Routing | N/A (messages lost) | Multi-hop Kademlia |
| NAT traversal | Requires gateway relay | Works with relay OR direct |
| Scalability | Poor (bottleneck) | Good (distributed) |
| Matchmaking | Broken | Works |

## Future Enhancements (v0.8.0+)

### Direct P2P Connections
When v0.8.0 adds hole punching:
- `pubsub_route` uses direct connections when available
- Falls back to relay automatically
- **No pub/sub code changes needed**

### Subscription Caching
- Cache DHT lookups for frequently published topics
- Invalidate on TTL expiry
- Reduces DHT queries

### Message Batching
- Batch multiple publishes to same subscriber
- Reduces routing overhead
- Improves throughput

## Summary

**v0.7.8 Goal**: Fix matchmaking by implementing DHT-routed pub/sub

**Timeline**: 3-5 days development + testing

**Impact**:
- ✅ Matchmaking works immediately
- ✅ True P2P architecture (no hub-and-spoke)
- ✅ Foundation for v0.8.0 direct connections
- ✅ Proven pattern (same as RPC routing)

**Next Steps**:
1. Add `pubsub_route` to protocol encoder/decoder
2. Create `macula_pubsub_router.erl`
3. Update `macula_pubsub_handler` publishing
4. Add gateway routing handler
5. Write tests
6. Deploy and verify matchmaking

## Future Refactoring Opportunities (v0.7.9+)

### Unified Routing Module

**Observation**: `macula_pubsub_routing` and `macula_rpc_routing` are nearly identical:
- Both wrap messages in routing envelopes
- Both use `route_or_deliver/3` with same logic
- Both forward via XOR distance to next hop
- Both check TTL (hop_count vs max_hops)
- Both return `{deliver, Type, Payload}` or `{forward, NextHop, UpdatedMsg}`

**Potential Consolidation**: Create `macula_dht_routing` generic module:

```erlang
-module(macula_dht_routing).

%% Generic DHT routing for any message type
wrap_message(MessageType, SourceNodeId, DestinationNodeId, Payload, MaxHops) ->
    #{
        <<"destination_node_id">> => DestinationNodeId,
        <<"source_node_id">> => SourceNodeId,
        <<"hop_count">> => 0,
        <<"max_hops">> => MaxHops,
        <<"message_type">> => MessageType,  % rpc_route | pubsub_route
        <<"payload">> => Payload
    }.

route_or_deliver(LocalNodeId, RouteMsg, RoutingServerPid) ->
    %% Generic routing logic (works for both RPC and pub/sub)
    ...
```

**Benefits**:
- DRY principle (Don't Repeat Yourself)
- Single source of truth for routing logic
- Easier to maintain and extend
- Reduces code duplication (~250 LOC → ~150 LOC)

**Considerations**:
- May reduce code clarity (two simple modules vs one generic)
- Type-specific logic still needed (unwrapping CALL vs PUBLISH)
- RPC has CALL/REPLY distinction, pub/sub only has PUBLISH
- Keep separate if logic diverges in future

**Recommendation**: Monitor for 2-3 release cycles. If routing logic stays identical, consolidate in v0.7.9+. If it diverges, keep separate.
