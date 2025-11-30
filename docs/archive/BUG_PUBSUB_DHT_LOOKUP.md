# BUG: Gateway Pub/Sub Not Querying DHT for Remote Subscribers

## Status
✅ **FIXED in v0.7.7** - Matchmaking and distributed pub/sub now working correctly

### Implementation Complete
- ✅ **Phase 1 COMPLETE**: Endpoint tracking in `macula_gateway_client_manager`
  - Added `endpoint_to_stream :: #{binary() => pid()}` to state
  - Added `get_stream_by_endpoint/2` API function
  - Updated `store_client_stream/4` to accept and track endpoint
  - Updated `remove_client/2` to clean up endpoint mappings
  - 39 tests passing (32 existing + 7 new endpoint tests)
- ✅ **Phase 2 COMPLETE**: Modified `handle_publish` to query DHT
  - Queries local subscribers (existing behavior)
  - Queries DHT for remote subscribers via `crypto:hash(sha256, Topic)`
  - Converts remote endpoints to stream PIDs
  - Combines local + remote and delivers to all
- ✅ **Phase 3 COMPLETE**: Added DHT lookup function
  - Added `macula_gateway_dht:lookup_value/1`
  - Calls `macula_routing_server:find_value/3` with K=20
  - Returns `{ok, [Subscriber]}` or `{error, not_found}`
  - 121 tests passing (gateway + pubsub + client_manager + endpoint + dht)

## Root Cause
`macula_gateway:handle_publish/3` (line 821-849) ONLY queries local subscriptions via `macula_gateway_pubsub`, never queries the DHT for remote subscribers.

## Evidence

### Logs Show the Problem
```
[Gateway] *** RECEIVED PUBLISH MESSAGE ***
[Gateway] Publishing message to topic: arcade.matchmaking.snake (delegating to pubsub)
[Gateway] Found 0 subscribers for topic arcade.matchmaking.snake
```

Even though all 3 peers subscribed:
```
22:38:03.244 [info] Game Coordinator subscribed to arcade.matchmaking.snake
[Gateway] Stream #Ref<0.1026626995.189661184.232462> subscribing to topics: [<<"arcade.matchmaking.snake">>]
```

### Current Broken Code
```erlang
%% macula_gateway.erl:821-849
handle_publish(_PublisherStream, PubMsg, State) ->
    Topic = maps:get(<<"topic">>, PubMsg),
    PubSub = State#state.pubsub,

    %% BUG: Only checks LOCAL subscribers
    {ok, Subscribers} = macula_gateway_pubsub:get_subscribers(PubSub, Topic),

    io:format("[Gateway] Found ~p subscribers for topic ~s~n", [length(Subscribers), Topic]),

    %% Sends to local subscribers only (always 0 in multi-peer setup)
    PubBinary = macula_protocol_encoder:encode(publish, PubMsg),
    lists:foreach(
        fun(SubscriberStream) ->
            case macula_quic:send(SubscriberStream, PubBinary) of
                ok -> io:format("[Gateway] Successfully sent to stream ~p~n", [SubscriberStream]);
                {error, Reason} -> io:format("[Gateway] Failed to send to stream ~p: ~p~n",
                                              [SubscriberStream, Reason])
            end
        end,
        Subscribers
    ),

    {noreply, State}.
```

## How Subscriptions ARE Stored in DHT

Subscriptions are correctly stored in the DHT (confirmed by logs showing "Advertising subscription..."):

```erlang
%% macula_pubsub_dht.erl:56
TopicKey = crypto:hash(sha256, Topic),

%% macula_pubsub_dht.erl:59-63
SubscriberValue = #{
    node_id => NodeId,
    endpoint => Url,  %% e.g., "https://arcade-peer1:4001"
    ttl => TTL
},

%% Stored via STORE message to DHT
StoreMsg = macula_routing_protocol:encode_store(TopicKey, SubscriberValue),
```

## Required Fix

`handle_publish` needs to:

1. **Get local subscribers** from `macula_gateway_pubsub` (current behavior - stream PIDs)
2. **Query DHT** for remote subscribers using `crypto:hash(sha256, Topic)`
3. **Send FIND_VALUE** message to DHT
4. **Handle async response** with list of remote subscriber endpoints
5. **Combine** local stream PIDs + remote endpoints
6. **Deliver** message to all subscribers:
   - Local: Send directly to stream PID (current behavior)
   - Remote: Send via QUIC to remote endpoint (NEW - needs connection pooling)

## Implementation Challenges

### Challenge 1: Async DHT Query
FIND_VALUE is asynchronous - response comes via separate message. Current `handle_publish` is synchronous.

**Options:**
- A) Make publish async - store pending publish, wait for FIND_VALUE reply
- B) Cache DHT subscriber lists (with TTL)
- C) Hybrid: Start with cache, refresh async

### Challenge 2: Remote Endpoint Connections
Remote subscribers have endpoints like `"https://arcade-peer1:4001"`, not stream PIDs.

**Need:**
- Connection pool to remote endpoints
- Or: Route via gateway mesh connections (current architecture)
- Or: Direct peer-to-peer connections (future v0.8.0)

### Challenge 3: Message Routing
**Current hub-and-spoke (v0.7.x):**
- All peers connect to gateway
- Gateway should route to connected peer streams
- Need mapping: endpoint → stream PID

**Solution for v0.7.x:**
Gateway already has connections from all peers. Need to:
1. Track `endpoint → stream PID` mapping when peers connect
2. When FIND_VALUE returns endpoints, look up stream PIDs
3. Send to those streams

## Recommended Implementation Plan

### Phase 1: Add Endpoint Tracking (IMMEDIATE)
```erlang
%% macula_gateway_client_manager.erl
%% Add endpoint → stream mapping
-record(state, {
    ...
    endpoint_to_stream :: #{binary() => pid()}  % NEW
}).

%% When client connects and sends JOIN:
handle_join(Stream, JoinMsg, State) ->
    Endpoint = maps:get(<<"url">>, JoinMsg, <<>>),  % Client's advertised URL
    %% Track endpoint → stream mapping
    NewEndpointMap = maps:put(Endpoint, Stream, State#state.endpoint_to_stream),
    ...
```

### Phase 2: Modify handle_publish to Query DHT (CRITICAL)
```erlang
handle_publish(_PublisherStream, PubMsg, State) ->
    Topic = maps:get(<<"topic">>, PubMsg),

    %% 1. Get local subscribers
    PubSub = State#state.pubsub,
    {ok, LocalSubscribers} = macula_gateway_pubsub:get_subscribers(PubSub, Topic),

    %% 2. Query DHT for remote subscribers
    TopicKey = crypto:hash(sha256, Topic),
    FindValueMsg = macula_routing_protocol:encode_find_value(TopicKey),

    %% Send FIND_VALUE to DHT module
    %% NOTE: This is synchronous for now - should be made async
    case macula_gateway_dht:lookup(State#state.dht, TopicKey) of
        {ok, RemoteSubscribers} ->
            %% RemoteSubscribers = [#{node_id => ..., endpoint => ...}, ...]

            %% 3. Convert endpoints to stream PIDs
            ClientManager = State#state.client_manager,
            RemoteStreams = lists:filtermap(
                fun(Subscriber) ->
                    Endpoint = maps:get(<<"endpoint">>, Subscriber, <<>>),
                    case macula_gateway_client_manager:get_stream_by_endpoint(
                           ClientManager, Endpoint) of
                        {ok, StreamPid} -> {true, StreamPid};
                        {error, not_found} -> false  % Subscriber not connected
                    end
                end,
                RemoteSubscribers
            ),

            %% 4. Combine local + remote
            AllSubscribers = lists:usort(LocalSubscribers ++ RemoteStreams),

            io:format("[Gateway] Found ~p total subscribers (~p local, ~p remote)~n",
                     [length(AllSubscribers), length(LocalSubscribers), length(RemoteStreams)]);

        {error, not_found} ->
            AllSubscribers = LocalSubscribers
    end,

    %% 5. Deliver to all
    PubBinary = macula_protocol_encoder:encode(publish, PubMsg),
    lists:foreach(
        fun(SubscriberStream) ->
            case macula_quic:send(SubscriberStream, PubBinary) of
                ok -> ok;
                {error, Reason} ->
                    io:format("[Gateway] Failed to send to stream ~p: ~p~n",
                             [SubscriberStream, Reason])
            end
        end,
        AllSubscribers
    ),

    {noreply, State}.
```

### Phase 3: Add DHT Lookup Function
```erlang
%% macula_gateway_dht.erl
-export([..., lookup/2]).

%% Synchronous DHT lookup (for now - should be async)
lookup(DhtState, Key) ->
    %% Query local DHT routing table
    case macula_dht_routing:get_value(DhtState, Key) of
        {ok, Values} when is_list(Values) ->
            {ok, Values};
        {ok, Value} ->
            {ok, [Value]};
        {error, not_found} ->
            {error, not_found}
    end.
```

## Testing

### Test 1: Verify DHT Storage
```bash
docker logs arcade-gateway | grep "STORE.*matchmaking"
# Should show subscriptions being stored
```

### Test 2: Verify DHT Lookup
Add debug logging to `handle_publish`:
```erlang
io:format("[Gateway] Looking up topic ~s (key: ~p) in DHT~n", [Topic, TopicKey]),
case macula_gateway_dht:lookup(...) of
    {ok, RemoteSubscribers} ->
        io:format("[Gateway] Found ~p remote subscribers in DHT~n", [length(RemoteSubscribers)]);
    {error, not_found} ->
        io:format("[Gateway] No remote subscribers found in DHT~n")
end
```

### Test 3: End-to-End Matchmaking
1. Deploy v0.7.7 with fix
2. Open 2 browser tabs (peer1, peer2)
3. Click "Find Game" in both
4. Should see: "Matchmaking event received from mesh: player X joined"
5. Should match and start game

## Priority
**P0 - CRITICAL** - Matchmaking completely broken without this fix

## Version Target
v0.7.7 - Pub/Sub DHT Lookup Fix

## Related Files
- `src/macula_gateway.erl` - handle_publish needs modification
- `src/macula_gateway_dht.erl` - needs lookup function
- `src/macula_gateway_client_manager.erl` - needs endpoint tracking
- `src/macula_pubsub_dht.erl` - subscription storage (already working)
- `test/macula_gateway_pubsub_dht_tests.erl` - test documentation

## Notes
- v0.7.6 successfully fixed connection stability (PING/PONG + idle timeout)
- This is a separate bug in the pub/sub routing logic
- The architecture is hub-and-spoke, so all peers connect to gateway
- Gateway has all the stream PIDs, just needs to look them up by endpoint
