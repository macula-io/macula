# Pub/Sub Throughput Optimization Recommendations

## Current Performance Baseline

**Without DHT Discovery**: 5,000-10,000 msg/sec
**With Current DHT Discovery**: 500-2,000 msg/sec

**Bottleneck**: Querying DHT on every publish operation, even for the same topic.

---

## Optimization 1: DHT Result Caching (HIGHEST PRIORITY)

### Problem
Currently querying DHT on every publish, even for the same topic repeatedly.

### Solution
Cache discovered subscribers with TTL-based invalidation.

### Expected Impact
**5-10x throughput improvement** for repeated publishes to same topic.

### Implementation
Add to `#state` record in `macula_connection.erl`:
```erlang
-record(state, {
    ...
    subscriber_cache = #{},  %% #{Topic => {Subscribers, ExpiryTimestamp}}
    cache_ttl = 60000       %% 60 seconds default TTL
}).
```

Modified `discover_remote_subscribers/5`:
```erlang
discover_remote_subscribers(Topic, Payload, Qos, _Opts, State) ->
    Now = erlang:system_time(millisecond),
    Cache = State#state.subscriber_cache,

    case maps:get(Topic, Cache, undefined) of
        {Subscribers, ExpiryTime} when ExpiryTime > Now ->
            %% Cache hit - use cached subscribers
            ?LOG_DEBUG("[~s] Using cached subscribers for topic: ~s (~p subscribers)",
                      [State#state.node_id, Topic, length(Subscribers)]),
            route_to_remote_subscribers(Subscribers, Topic, Payload, Qos, State);

        _ ->
            %% Cache miss or expired - query DHT
            ?LOG_DEBUG("[~s] Cache miss for topic: ~s, querying DHT",
                      [State#state.node_id, Topic]),
            query_dht_for_subscribers(Topic, Payload, Qos, State)
    end.
```

Store discovered subscribers (when FIND_VALUE response arrives):
```erlang
cache_discovered_subscribers(Topic, Subscribers, State) ->
    Now = erlang:system_time(millisecond),
    ExpiryTime = Now + State#state.cache_ttl,
    Cache = State#state.subscriber_cache,
    Cache2 = Cache#{Topic => {Subscribers, ExpiryTime}},
    State#state{subscriber_cache = Cache2}.
```

### Trade-offs
- ✅ 5-10x throughput improvement for repeated publishes to same topic
- ✅ Reduces DHT query load significantly
- ⚠️ Subscribers joining/leaving won't be detected until cache expires (60s default)
- ⚠️ Increased memory usage (minimal - ~1KB per cached topic)

### Configuration Options
- `cache_ttl`: Time before cache entry expires (default: 60000ms = 60s)
- Lower TTL = fresher subscriber info, more DHT queries
- Higher TTL = better performance, slower detection of topology changes

---

## Optimization 2: Direct Routing Table (HIGH PRIORITY)

### Problem
After discovering subscribers once, we should remember their endpoints and route directly.

### Solution
Build a routing table mapping topics to subscriber endpoints.

### Expected Impact
**3-5x additional throughput improvement** - near-zero latency for known topics.

### Implementation
Add to `#state` record:
```erlang
-record(state, {
    ...
    routing_table = #{}  %% #{Topic => [{NodeId, Endpoint, LastSeen}]}
}).
```

Update routing table when FIND_VALUE response arrives:
```erlang
handle_find_value_response(Topic, SubscriberEndpoints, State) ->
    Now = erlang:system_time(millisecond),

    %% Update routing table
    Entries = [{NodeId, Endpoint, Now} || {NodeId, Endpoint} <- SubscriberEndpoints],
    RoutingTable = State#state.routing_table,
    RoutingTable2 = RoutingTable#{Topic => Entries},

    %% Route this message directly
    State2 = State#state{routing_table = RoutingTable2},
    route_to_endpoints(SubscriberEndpoints, Topic, Payload, Qos, State2).
```

Publish using routing table first:
```erlang
publish_with_routing_table(Topic, Payload, Qos, State) ->
    case maps:get(Topic, State#state.routing_table, undefined) of
        undefined ->
            %% No routing info - use DHT
            discover_remote_subscribers(Topic, Payload, Qos, #{}, State);

        Entries ->
            %% Route directly to known subscribers
            Endpoints = [{NodeId, Endpoint} || {NodeId, Endpoint, _LastSeen} <- Entries],
            route_to_endpoints(Endpoints, Topic, Payload, Qos, State)
    end.
```

Periodic cleanup of stale entries:
```erlang
clean_stale_routing_entries(State) ->
    Now = erlang:system_time(millisecond),
    MaxAge = 300000,  %% 5 minutes

    RoutingTable = State#state.routing_table,
    RoutingTable2 = maps:map(fun(_Topic, Entries) ->
        [{NodeId, Endpoint, LastSeen} || {NodeId, Endpoint, LastSeen} <- Entries,
                                         Now - LastSeen < MaxAge]
    end, RoutingTable),

    State#state{routing_table = RoutingTable2}.
```

### Trade-offs
- ✅ Near-zero latency for known topics
- ✅ No DHT queries after initial discovery
- ✅ Builds on caching infrastructure
- ⚠️ Memory usage grows with number of topics
- ⚠️ Requires periodic cleanup of stale entries (every 5 minutes)

### Configuration Options
- `max_routing_age`: Maximum age before routing entry expires (default: 300000ms = 5 minutes)
- `cleanup_interval`: How often to clean stale entries (default: 60000ms = 1 minute)

---

## Optimization 3: Adaptive Discovery Rate-Limiting (MODERATE PRIORITY)

### Problem
Even with caching, cache expiry triggers DHT queries too frequently during high-frequency publishing.

### Solution
Use adaptive rate-limiting that prevents DHT queries within a minimum time window per topic.

### Expected Impact
**2-3x improvement** for high-frequency publishes - prevents discovery storms.

### Implementation
Add to `#state` record:
```erlang
-record(state, {
    ...
    topic_last_discovery = #{},  %% #{Topic => LastDiscoveryTimestamp}
    min_discovery_interval = 5000  %% Minimum 5 seconds between discoveries
}).
```

Rate-limit discovery:
```erlang
discover_remote_subscribers(Topic, Payload, Qos, _Opts, State) ->
    Now = erlang:system_time(millisecond),
    LastDiscovery = maps:get(Topic, State#state.topic_last_discovery, 0),
    TimeSinceLastDiscovery = Now - LastDiscovery,

    if
        TimeSinceLastDiscovery < State#state.min_discovery_interval ->
            %% Too soon - skip discovery, use cached or best-effort routing
            ?LOG_DEBUG("[~s] Skipping discovery for topic ~s (rate-limited)",
                      [State#state.node_id, Topic]),
            State;

        true ->
            %% Allowed - perform discovery
            State2 = query_dht_for_subscribers(Topic, Payload, Qos, State),
            LastDiscoveryMap = State2#state.topic_last_discovery,
            LastDiscoveryMap2 = LastDiscoveryMap#{Topic => Now},
            State2#state{topic_last_discovery = LastDiscoveryMap2}
    end.
```

### Trade-offs
- ✅ Reduces DHT load significantly
- ✅ Prevents discovery storms during high-frequency publishing
- ✅ Works well combined with caching
- ⚠️ May miss new subscribers within rate-limit window (5s default)
- ⚠️ Requires tuning of `min_discovery_interval`

### Configuration Options
- `min_discovery_interval`: Minimum time between discoveries per topic (default: 5000ms = 5s)

---

## Optimization 4: Persistent QUIC Streams (MODERATE PRIORITY)

### Problem
Opening new QUIC stream for each publish adds overhead.

### Solution
Keep streams open and reuse them for multiple messages (connection pooling).

### Expected Impact
**1.5-2x improvement** - eliminates stream creation overhead.

### Implementation
Add to `#state` record:
```erlang
-record(state, {
    ...
    stream_pool = #{},  %% #{SubscriberEndpoint => StreamPid}
    max_streams_per_endpoint = 5
}).
```

Get or create stream:
```erlang
get_or_create_stream(Endpoint, State) ->
    case maps:get(Endpoint, State#state.stream_pool, undefined) of
        undefined ->
            %% Create new stream
            case connect_and_open_stream(Endpoint) of
                {ok, StreamPid} ->
                    Pool = State#state.stream_pool,
                    Pool2 = Pool#{Endpoint => StreamPid},
                    {ok, StreamPid, State#state{stream_pool = Pool2}};
                Error ->
                    Error
            end;

        StreamPid ->
            %% Reuse existing stream
            {ok, StreamPid, State}
    end.
```

Send via pooled stream:
```erlang
send_via_pooled_stream(Endpoint, Message, State) ->
    case get_or_create_stream(Endpoint, State) of
        {ok, StreamPid, State2} ->
            case macula_quic:async_send(StreamPid, Message) of
                ok -> {ok, State2};
                {error, _Reason} ->
                    %% Stream failed - remove from pool and retry
                    Pool = State2#state.stream_pool,
                    Pool2 = maps:remove(Endpoint, Pool),
                    State3 = State2#state{stream_pool = Pool2},
                    send_via_pooled_stream(Endpoint, Message, State3)
            end;
        Error ->
            Error
    end.
```

### Trade-offs
- ✅ Eliminates stream creation overhead
- ✅ Better QUIC performance with warm connections
- ✅ Lower latency for subsequent messages
- ⚠️ Increased connection state to manage
- ⚠️ Need connection health monitoring and recovery

### Configuration Options
- `max_streams_per_endpoint`: Maximum pooled streams per endpoint (default: 5)
- `stream_idle_timeout`: Close stream after this period of inactivity (default: 60000ms)

---

## Optimization 5: Message Batching (OPTIONAL - For Burst Traffic)

### Problem
Each publish creates a separate QUIC send operation, causing overhead during burst traffic.

### Solution
Batch multiple publishes to same topic within a time window.

### Expected Impact
**2x improvement for burst traffic** - reduces network overhead.

### Implementation
Add to `#state` record:
```erlang
-record(state, {
    ...
    publish_batch = #{},     %% #{Topic => [Messages]}
    batch_timer_ref = undefined,
    batch_window_ms = 50    %% 50ms batching window
}).
```

Add to batch instead of sending immediately:
```erlang
handle_cast({do_publish, Topic, Payload, Qos, Opts}, State) ->
    Batch = State#state.publish_batch,
    Messages = maps:get(Topic, Batch, []),
    Messages2 = [{Payload, Qos, Opts} | Messages],
    Batch2 = Batch#{Topic => Messages2},

    %% Start batch timer if not already running
    State2 = case State#state.batch_timer_ref of
        undefined ->
            TimerRef = erlang:send_after(State#state.batch_window_ms, self(), flush_batch),
            State#state{batch_timer_ref = TimerRef};
        _ ->
            State
    end,

    {noreply, State2#state{publish_batch = Batch2}}.
```

Flush batch when timer expires:
```erlang
handle_info(flush_batch, State) ->
    Batch = State#state.publish_batch,

    %% Send batched messages for each topic
    State2 = maps:fold(fun(Topic, Messages, StateAcc) ->
        %% Encode all messages together
        BatchedPayload = encode_batch(Messages),
        send_batched_messages(Topic, BatchedPayload, StateAcc)
    end, State, Batch),

    {noreply, State2#state{
        publish_batch = #{},
        batch_timer_ref = undefined
    }}.
```

### Trade-offs
- ✅ Reduces network overhead for burst traffic
- ✅ Better utilization of QUIC stream bandwidth
- ✅ More efficient use of network resources
- ⚠️ Adds latency (up to `batch_window_ms`, default 50ms)
- ⚠️ Requires protocol changes to support batched messages
- ⚠️ Complexity increase in message encoding/decoding

### Configuration Options
- `batch_window_ms`: Time window for batching (default: 50ms)
- `max_batch_size`: Maximum messages per batch (default: 100)

### When to Use
Only implement if you have burst traffic patterns where multiple messages are published to the same topic within short time windows (e.g., sensor data, logs).

---

## Recommended Implementation Order

### Phase 1: Foundation (Week 1)
**Priority**: CRITICAL
**Expected Improvement**: 5-10x throughput

1. **DHT Result Caching** (`macula_connection.erl` lines 1769-1814)
   - Implementation time: ~30 minutes
   - Highest impact, lowest complexity
   - Expected: 5,000-10,000 msg/sec for repeated topics

### Phase 2: Direct Routing (Week 2)
**Priority**: HIGH
**Expected Improvement**: Additional 3-5x throughput

2. **Direct Routing Table**
   - Implementation time: ~1 hour
   - Builds on caching infrastructure
   - Expected: 10,000-20,000 msg/sec total

### Phase 3: Rate Limiting (Week 2)
**Priority**: MODERATE
**Expected Improvement**: Additional 2-3x for high-frequency publishes

3. **Adaptive Discovery Rate-Limiting**
   - Implementation time: ~20 minutes
   - Prevents discovery storms
   - Expected: 15,000-30,000 msg/sec for high-frequency traffic

### Phase 4: Connection Pooling (Week 3 - if needed)
**Priority**: MODERATE
**Expected Improvement**: Additional 1.5-2x

4. **Persistent QUIC Streams**
   - Implementation time: ~1 hour
   - For sustained high throughput
   - Expected: 20,000-50,000 msg/sec total

### Phase 5: Batching (Week 4 - optional)
**Priority**: LOW (only for burst traffic)
**Expected Improvement**: 2x for burst traffic

5. **Message Batching**
   - Implementation time: ~2 hours (requires protocol changes)
   - Only if you have burst traffic patterns
   - Expected: 40,000-100,000 msg/sec for bursts

---

## Expected Final Throughput

### With Phase 1-2 (Caching + Routing)
- **Single topic, repeated publishes**: 10,000-20,000 msg/sec
- **Multiple topics**: 5,000-10,000 msg/sec

### With Phase 1-3 (+ Rate Limiting)
- **High-frequency publishing**: 15,000-30,000 msg/sec
- **Multiple topics**: 10,000-15,000 msg/sec

### With All Optimizations (Phase 1-5)
- **Sustained throughput**: 20,000-50,000 msg/sec
- **Burst throughput**: 40,000-100,000 msg/sec

---

## Configuration Tuning Guide

### For Low-Latency Applications
```erlang
cache_ttl => 10000,              %% 10 seconds (fresher data)
min_discovery_interval => 2000,  %% 2 seconds (more frequent updates)
max_routing_age => 60000,        %% 1 minute (fresher routing)
batch_window_ms => 10            %% 10ms batching (if needed)
```

### For High-Throughput Applications
```erlang
cache_ttl => 300000,             %% 5 minutes (less DHT load)
min_discovery_interval => 30000, %% 30 seconds (minimal queries)
max_routing_age => 600000,       %% 10 minutes (stable routing)
batch_window_ms => 100           %% 100ms batching (if needed)
```

### For Dynamic Topologies
```erlang
cache_ttl => 30000,              %% 30 seconds (balance freshness)
min_discovery_interval => 5000,  %% 5 seconds (detect changes)
max_routing_age => 120000,       %% 2 minutes (adapt to changes)
batch_window_ms => 50            %% 50ms batching (if needed)
```

---

## Monitoring Metrics

Track these metrics to measure optimization effectiveness:

1. **DHT Query Rate**: Queries per second (should decrease)
2. **Cache Hit Ratio**: Cache hits / total publishes (target: >90%)
3. **Publish Latency**: Time from publish call to network send (target: <10ms)
4. **Throughput**: Messages per second (target: >10,000 msg/sec)
5. **Memory Usage**: Cache + routing table size (monitor growth)
6. **Discovery Latency**: Time to discover new subscribers (should remain low)

---

## Testing Strategy

### Throughput Test Script
See `/tmp/test-throughput.sh` - sends 100 messages as fast as possible.

### Test Scenarios
1. **Single Topic, Repeated Publishes**: Measure cache effectiveness
2. **Multiple Topics**: Measure routing table effectiveness
3. **High-Frequency Publishing**: Measure rate-limiting effectiveness
4. **Dynamic Subscribers**: Measure discovery latency with cache
5. **Burst Traffic**: Measure batching effectiveness (if implemented)

---

## Document History

- **Created**: 2025-11-12
- **Context**: Fixing publisher timeout issue in mesh-wide pub/sub
- **Issue**: Second publish timed out due to synchronous DHT discovery blocking gen_server
- **Fix Applied**: Spawned discovery process to prevent blocking
- **Next Step**: Implement optimizations to improve throughput from 500-2,000 msg/sec to 10,000+ msg/sec
