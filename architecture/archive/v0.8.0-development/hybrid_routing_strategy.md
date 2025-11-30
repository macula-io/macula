# Hybrid Routing Strategy: Multi-hop with Smart Direct Fallback

## Design Principle

**Foundation**: Multi-hop DHT routing (always works, NAT-friendly)
**Optimization**: Direct connections when beneficial
**Adaptive**: Learn and improve over time

## Architecture Overview

```
┌─────────────────────────────────────────────────────────┐
│                   RPC Call Request                      │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│              1. DHT Service Discovery                   │
│         (Find provider via Kademlia lookup)             │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│           2. Check Connection Strategy Cache            │
│                                                          │
│  ┌─────────────────┐      ┌─────────────────┐          │
│  │ Unknown/Multi   │      │ Direct Proven   │          │
│  └─────────────────┘      └─────────────────┘          │
│          ↓                        ↓                     │
│    Multi-hop Route          Direct Connection          │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│            3. Measure & Learn from Result               │
│   - Latency, Success Rate, Reachability                │
│   - Update Connection Strategy Cache                    │
└─────────────────────────────────────────────────────────┘
```

## Three-Phase Implementation

### Phase 1: Multi-hop Foundation (4-6 weeks)

**Goal**: Get pure DHT routing working end-to-end

**Components:**

1. **`macula_rpc_routing.erl`** - Core routing module
   ```erlang
   -export([
       wrap_call/4,           % Wrap CALL in rpc_route
       wrap_reply/4,          % Wrap REPLY in rpc_route
       route_or_deliver/3,    % Route to next hop OR deliver locally
       find_next_hop/3        % Use Kademlia to find closest node
   ]).
   ```

2. **Gateway Integration** (`macula_gateway.erl`)
   - Handle incoming `rpc_route` messages
   - Forward to next hop based on Kademlia distance
   - Unwrap and deliver when destination == local node

3. **Connection Integration** (`macula_connection.erl`)
   - Send CALLs via `rpc_route` wrapper
   - Receive REPLYs via DHT routing
   - Remove direct endpoint connection code

4. **Multi-hop Test Topology**
   ```
   Registry ←→ NodeA ←→ NodeB ←→ Provider
                ↓
              Client

   Client → NodeA → NodeB → Provider (2+ hops required)
   ```

**Success Criteria:**
- ✅ RPC calls successfully route through 2-3 hops
- ✅ Replies route back through mesh
- ✅ Works with nodes behind simulated NAT
- ✅ E2E tests pass consistently

---

### Phase 2: Connection Learning & Metrics (2-3 weeks)

**Goal**: Gather data to make smart optimization decisions

**Components:**

1. **Connection Strategy Cache** (`macula_connection_cache.erl`)
   ```erlang
   -record(connection_strategy, {
       node_id :: binary(),
       strategy :: multi_hop | direct | unknown,
       last_updated :: integer(),
       metrics :: #{
           avg_latency_ms => float(),
           success_rate => float(),
           direct_reachable => boolean(),
           sample_count => integer()
       }
   }).

   -export([
       get_strategy/1,        % Get cached strategy for node
       update_metrics/3,      % Update after each RPC
       should_try_direct/1,   % Decision logic
       periodic_cleanup/0     % TTL-based cache expiry
   ]).
   ```

2. **Metrics Collection** (add to `macula_connection.erl`)
   ```erlang
   %% Measure each RPC call:
   - Start timestamp
   - End timestamp
   - Success/failure
   - Hop count (for multi-hop)
   - Connection type used

   %% After each call:
   macula_connection_cache:update_metrics(NodeId, CallMetrics, State)
   ```

3. **Observability**
   - Log routing decisions
   - Export metrics (Prometheus-style)
   - Dashboard showing: multi-hop vs direct ratio, latency distributions

**Success Criteria:**
- ✅ Metrics collected for every RPC call
- ✅ Cache stores strategy per node
- ✅ Can query: "What's the best strategy for node X?"
- ✅ Cache expires stale entries (TTL: 5 minutes)

---

### Phase 3: Direct Connection Optimization (3-4 weeks)

**Goal**: Add direct connection path with smart decision logic

**Components:**

1. **Direct Connection Attempt** (`macula_direct_connector.erl`)
   ```erlang
   -export([
       try_direct_connection/2,  % Attempt direct QUIC to endpoint
       is_directly_reachable/1   % Probe endpoint reachability
   ]).

   %% Strategy:
   try_direct_connection(Endpoint, Timeout) ->
       %% 1. Open QUIC connection to endpoint
       %% 2. Send PING (timeout: 2s)
       %% 3. Return {ok, Connection} | {error, unreachable}
   ```

2. **Smart Routing Decision** (update `macula_connection.erl`)
   ```erlang
   route_rpc_call(Procedure, Args, ProviderNodeId, State) ->
       Strategy = macula_connection_cache:get_strategy(ProviderNodeId),

       case Strategy of
           direct ->
               %% Use direct connection
               send_direct_rpc(ProviderNodeId, Procedure, Args, State);

           multi_hop ->
               %% Use DHT routing
               send_routed_rpc(ProviderNodeId, Procedure, Args, State);

           unknown ->
               %% First time seeing this node
               %% Start with multi-hop (safe default)
               %% Background: probe for direct reachability
               spawn_link(fun() ->
                   probe_direct_reachability(ProviderNodeId)
               end),
               send_routed_rpc(ProviderNodeId, Procedure, Args, State)
       end.
   ```

3. **Learning Logic** (`macula_connection_cache.erl`)
   ```erlang
   %% Decision algorithm:
   should_try_direct(NodeId) ->
       case get_strategy(NodeId) of
           #connection_strategy{
               strategy = unknown,
               sample_count = N
           } when N >= 5 ->
               %% Collected enough multi-hop samples
               %% Try direct probe
               true;

           #connection_strategy{
               strategy = multi_hop,
               metrics = #{avg_latency_ms := LatencyMs}
           } when LatencyMs > 50 ->
               %% High latency on multi-hop
               %% Worth trying direct
               true;

           #connection_strategy{
               strategy = direct,
               metrics = #{success_rate := Rate}
           } when Rate < 0.8 ->
               %% Direct connection unreliable
               %% Fall back to multi-hop
               false;

           _ ->
               %% Keep current strategy
               false
       end.
   ```

4. **Periodic Optimization** (background task)
   ```erlang
   %% Run every 60 seconds:
   optimize_connections() ->
       %% 1. Find nodes with high multi-hop latency
       HighLatencyNodes = find_high_latency_nodes(),

       %% 2. Probe them for direct reachability
       lists:foreach(fun(NodeId) ->
           case macula_direct_connector:is_directly_reachable(NodeId) of
               true ->
                   %% Update cache: use direct
                   macula_connection_cache:update_strategy(NodeId, direct);
               false ->
                   %% Still needs relay
                   ok
           end
       end, HighLatencyNodes).
   ```

**Success Criteria:**
- ✅ System learns which nodes are directly reachable
- ✅ Direct connections used when beneficial (latency <50ms improvement)
- ✅ Falls back to multi-hop when direct fails
- ✅ Adapts to network changes (NAT, firewall changes)
- ✅ No regression in reliability (always works via multi-hop)

---

## Decision Logic Summary

### When to Use Multi-hop (Default)
- First time contacting a node (unknown)
- Node is behind NAT (learned from failed direct attempts)
- Direct connection failed/unstable (success rate <80%)
- Privacy/anonymity required (future: configurable per realm)

### When to Use Direct
- Node is proven directly reachable (successful probes)
- Multi-hop latency is high (>50ms)
- High-frequency RPC to same node (>10 calls/min)
- Direct connection success rate >90%

### Transition Rules
```
unknown → multi_hop (default, after 5 samples)
unknown → direct (if probe succeeds)
multi_hop → direct (if latency high + probe succeeds)
direct → multi_hop (if success rate drops <80%)
```

## Configuration

```erlang
%% In macula.app.src or sys.config:
{macula, [
    {routing_strategy, hybrid},  % hybrid | multi_hop_only | direct_only

    %% Multi-hop settings
    {max_hops, 10},
    {hop_timeout_ms, 5000},

    %% Direct connection settings
    {direct_probe_timeout_ms, 2000},
    {direct_min_latency_improvement_ms, 50},
    {direct_min_success_rate, 0.90},

    %% Cache settings
    {strategy_cache_ttl_sec, 300},
    {metrics_sample_size, 100},

    %% Optimization
    {optimization_interval_sec, 60},
    {probe_high_latency_threshold_ms, 50}
]}.
```

## Testing Strategy

### Phase 1 Tests
- Multi-hop routing through 2-3 hops
- NAT traversal (simulated with Docker network policies)
- Failure recovery (node drops, re-routing)

### Phase 2 Tests
- Metrics collection accuracy
- Cache TTL expiry
- Strategy persistence across restarts

### Phase 3 Tests
- Direct connection success/failure handling
- Learning algorithm correctness
- Adaptive behavior under network changes
- Performance comparison: multi-hop vs direct
- Load testing: mixed workload (some direct, some relay)

## Monitoring & Observability

### Key Metrics
```erlang
%% Per-node metrics:
macula_rpc_call_duration_ms{node_id, strategy} histogram
macula_rpc_call_success{node_id, strategy} counter
macula_connection_strategy{node_id, strategy} gauge

%% Aggregate metrics:
macula_routing_strategy_distribution{strategy} gauge
macula_direct_probe_success_rate counter
macula_avg_hop_count histogram
```

### Dashboards
- Routing strategy distribution (pie chart)
- Latency comparison: multi-hop vs direct
- Learning efficiency: time to discover direct reachability
- Success rates by strategy

## Benefits of This Approach

✅ **Reliability**: Multi-hop always works (NAT-friendly)
✅ **Performance**: Direct connections when beneficial
✅ **Adaptive**: System learns and improves over time
✅ **Graceful**: Falls back seamlessly on failure
✅ **Observable**: Clear metrics for optimization
✅ **Incremental**: Can deploy phase-by-phase

## Timeline

- **Phase 1**: 4-6 weeks (multi-hop foundation)
- **Phase 2**: 2-3 weeks (metrics & learning)
- **Phase 3**: 3-4 weeks (direct optimization)

**Total**: ~10-13 weeks to full hybrid implementation

Each phase is independently valuable and can be deployed to production.

## References

- libp2p relay protocol: https://docs.libp2p.io/concepts/nat/circuit-relay/
- WebRTC ICE (STUN/TURN): https://webrtc.org/getting-started/peer-connections
- Kademlia paper: https://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf
- IPFS DHT: https://docs.ipfs.tech/concepts/dht/
