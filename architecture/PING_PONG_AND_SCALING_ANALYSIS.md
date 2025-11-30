# PING/PONG Dialog and Bootstrap Scaling Analysis

**Date**: November 2025
**Version**: v0.12.0
**Context**: 50-peer NAT test environment running successfully

---

## Part 1: PING/PONG Dialog Implementation

### Current State

The current `macula_chatter.erl` uses a simple **broadcast-only** model:
- Each peer broadcasts "Hello from X (#N)" every 10 seconds
- Messages go via pub/sub to all subscribed peers
- No bidirectional dialog - fire-and-forget

### Proposed PING/PONG Model

#### Message Types

```erlang
%% PING: Initiator requests response
#{
    <<"type">> => <<"ping">>,
    <<"from">> => <<"fc01">>,
    <<"to">> => <<"rc05">>,           % Target peer
    <<"ping_id">> => <<"uuid-123">>,  % Correlation ID
    <<"timestamp">> => 1732940000000,
    <<"payload">> => <<"optional data">>
}

%% PONG: Responder acknowledges
#{
    <<"type">> => <<"pong">>,
    <<"from">> => <<"rc05">>,
    <<"to">> => <<"fc01">>,
    <<"ping_id">> => <<"uuid-123">>,  % Same correlation ID
    <<"timestamp">> => 1732940000500,
    <<"rtt_ms">> => 500               % Round-trip time
}
```

#### Flow Comparison

**Current (Broadcast):**
```
fc01 --[HELLO]--> Bootstrap --[fan-out]--> All 49 peers
                                           (no response expected)
```

**Proposed (PING/PONG):**
```
fc01 --[PING to rc05]--> Bootstrap --[deliver]--> rc05
fc01 <--[PONG from rc05]-- Bootstrap <--[reply]-- rc05

fc01 measures RTT = PONG.timestamp - PING.timestamp
```

#### Implementation Options

**Option A: RPC-based (Request/Response)**
- Use existing `macula_rpc_handler:call/3`
- Synchronous, blocks caller until PONG received
- Built-in timeout handling
- **Pros**: Simple, reliable, already works
- **Cons**: Blocking, one dialog at a time

**Option B: PubSub with Correlation**
- Send PING via pub/sub to specific peer topic
- PONG returned via separate pub/sub topic
- Correlation via `ping_id`
- **Pros**: Non-blocking, parallel dialogs
- **Cons**: More complex state management

**Option C: Hybrid (Recommended)**
- PING via RPC call with async callback
- Track pending PINGs in gen_server state
- Timeout handler for unanswered PINGs
- **Pros**: Best of both worlds

### Recommended Implementation

```erlang
-record(state, {
    node_id :: binary(),
    pending_pings = #{} :: #{binary() => {pid(), reference(), integer()}},
    ping_stats = #{} :: #{binary() => #{count => integer(), avg_rtt => float()}},
    interval :: pos_integer(),
    timer_ref :: reference() | undefined
}).

%% Send PING to random peer
handle_info(ping_tick, State) ->
    case select_random_peer(State) of
        {ok, TargetPeer} ->
            PingId = generate_uuid(),
            Timestamp = erlang:system_time(millisecond),

            %% Send PING via RPC
            spawn_link(fun() ->
                Result = send_ping_rpc(TargetPeer, PingId, State#state.node_id),
                ?SERVER ! {ping_result, PingId, TargetPeer, Result}
            end),

            %% Track pending PING with timeout
            TimerRef = erlang:send_after(5000, self(), {ping_timeout, PingId}),
            NewPending = maps:put(PingId, {TargetPeer, TimerRef, Timestamp}, State#state.pending_pings),
            {noreply, State#state{pending_pings = NewPending}};
        {error, no_peers} ->
            {noreply, State}
    end;

%% Handle PONG response
handle_info({ping_result, PingId, TargetPeer, {ok, Response}}, State) ->
    case maps:take(PingId, State#state.pending_pings) of
        {{TargetPeer, TimerRef, StartTime}, NewPending} ->
            erlang:cancel_timer(TimerRef),
            RTT = erlang:system_time(millisecond) - StartTime,
            io:format("[Chatter ~s] PONG from ~s: ~pms RTT~n",
                      [State#state.node_id, TargetPeer, RTT]),
            NewStats = update_ping_stats(TargetPeer, RTT, State#state.ping_stats),
            {noreply, State#state{pending_pings = NewPending, ping_stats = NewStats}};
        error ->
            {noreply, State}  % Already timed out or duplicate
    end.
```

### State Tracking Required

```erlang
-record(ping_stats, {
    total_sent = 0 :: non_neg_integer(),
    total_received = 0 :: non_neg_integer(),
    total_timeouts = 0 :: non_neg_integer(),
    per_peer = #{} :: #{binary() => peer_stats()},
    per_nat_type = #{} :: #{nat_type() => nat_stats()}
}).

-record(peer_stats, {
    pings_sent = 0 :: non_neg_integer(),
    pongs_received = 0 :: non_neg_integer(),
    min_rtt_ms :: non_neg_integer() | undefined,
    max_rtt_ms :: non_neg_integer() | undefined,
    avg_rtt_ms :: float() | undefined,
    last_seen :: integer() | undefined
}).
```

---

## Part 2: Bootstrap Scaling Analysis (1000 Peers)

### Current Architecture Constraints

With 50 peers, we observe:
- **17 Full Cone** peers: ~420 messages received each
- **17 Restricted** peers: ~430 messages received each
- **16 Symmetric** peers: ~400 messages received each

All traffic flows through the single bootstrap node.

### Scaling to 1000 Peers - Single Bootstrap

#### Connection Load

| Metric | 50 Peers | 1000 Peers | Factor |
|--------|----------|------------|--------|
| Active connections | 50 | 1,000 | 20x |
| DHT entries | ~100 | ~2,000 | 20x |
| Memory (per conn ~50KB) | ~2.5MB | ~50MB | 20x |
| Pub/Sub fan-out per msg | 49 | 999 | 20x |

#### Message Throughput

Assuming each peer sends 1 message every 10 seconds:

| Metric | 50 Peers | 1000 Peers |
|--------|----------|------------|
| Incoming msg/sec | 5 | 100 |
| Outgoing msg/sec (fan-out) | 245 | 99,900 |
| Total QUIC streams/sec | ~250 | ~100,000 |

**Critical Issue**: With 1000 peers, the bootstrap must handle **100,000 outgoing QUIC streams per second** for pub/sub fan-out.

#### Bootstrap Bottlenecks

1. **CPU**: Message encoding/decoding, QUIC encryption
   - Current: ~5% CPU with 50 peers
   - Projected: ~100% CPU with 500-700 peers

2. **Memory**: Connection state, DHT entries, stream buffers
   - Current: ~100MB with 50 peers
   - Projected: ~2GB with 1000 peers

3. **Network I/O**: UDP packet processing
   - Current: ~1 Mbps with 50 peers
   - Projected: ~20 Mbps with 1000 peers

4. **QUIC Streams**: MsQuic stream creation overhead
   - Limit: ~50,000 concurrent streams per listener
   - Risk: Stream exhaustion under burst load

### Practical Single-Bootstrap Limit

Based on analysis:

| Bootstrap Spec | Max Peers (Conservative) | Max Peers (Optimized) |
|----------------|--------------------------|----------------------|
| 2 CPU, 4GB RAM | 100-200 | 300-400 |
| 4 CPU, 8GB RAM | 300-500 | 600-800 |
| 8 CPU, 16GB RAM | 600-800 | 1000-1200 |

**Recommendation**: A single bootstrap can handle **500-800 peers** with good performance.

---

## Part 3: Multi-Bootstrap (Submesh) Architecture

### Why Multiple Bootstraps?

1. **Horizontal Scaling**: Distribute load across machines
2. **Geographic Distribution**: Lower latency for regional peers
3. **Fault Tolerance**: No single point of failure
4. **Network Isolation**: Separate environments (prod/staging/dev)

### Architecture Options

#### Option A: Federated Bootstraps (Recommended)

Each bootstrap manages its own submesh, but bootstraps communicate with each other.

```
                    ┌─────────────────┐
                    │   Root DHT      │
                    │  (Bootstrap-0)  │
                    └────────┬────────┘
                             │
           ┌─────────────────┼─────────────────┐
           │                 │                 │
    ┌──────▼──────┐   ┌──────▼──────┐   ┌──────▼──────┐
    │ Bootstrap-A │───│ Bootstrap-B │───│ Bootstrap-C │
    │  (US-East)  │   │  (EU-West)  │   │  (APAC)     │
    └──────┬──────┘   └──────┬──────┘   └──────┬──────┘
           │                 │                 │
    ┌──────┴──────┐   ┌──────┴──────┐   ┌──────┴──────┐
    │ 300 peers   │   │ 400 peers   │   │ 300 peers   │
    └─────────────┘   └─────────────┘   └─────────────┘
```

**Inter-Bootstrap Protocol:**
```erlang
%% Bootstrap advertises to root DHT
#{
    type => bootstrap_announce,
    bootstrap_id => <<"bootstrap-a">>,
    endpoint => <<"quic://10.0.1.10:4433">>,
    region => <<"us-east">>,
    peer_count => 300,
    capacity => 500,
    load_factor => 0.6
}

%% Cross-mesh routing
#{
    type => cross_mesh_route,
    source_mesh => <<"bootstrap-a">>,
    target_mesh => <<"bootstrap-b">>,
    target_peer => <<"peer-xyz">>,
    message => #{...}
}
```

#### Option B: DHT Replication Ring

All bootstraps participate in a single DHT with replication.

```
    Bootstrap-A ←──→ Bootstrap-B ←──→ Bootstrap-C
         │                │                │
         └────────────────┴────────────────┘
                    DHT Ring
                (k=3 replication)
```

**Pros:**
- Any peer can connect to any bootstrap
- Automatic failover
- Simpler peer configuration

**Cons:**
- Higher inter-bootstrap traffic
- Consistency complexity (eventual)
- All bootstraps must know all peers

#### Option C: Realm-Based Isolation

Each realm has dedicated bootstrap(s), no cross-realm communication.

```
    Realm: com.company.prod           Realm: com.company.staging
    ┌───────────────────┐            ┌───────────────────┐
    │   Bootstrap-Prod  │            │ Bootstrap-Staging │
    │    500 peers      │            │    100 peers      │
    └───────────────────┘            └───────────────────┘
          │      │                         │      │
       ┌──┘      └──┐                   ┌──┘      └──┐
       │            │                   │            │
    Production   Production          Staging     Staging
     Peers        Peers               Peers       Peers
```

**Pros:**
- Complete isolation (security)
- Simple, no inter-bootstrap protocol
- Independent scaling per realm

**Cons:**
- No cross-realm communication
- Peer must know correct bootstrap

### Recommended: Federated Bootstraps

#### Configuration

```erlang
%% Peer configuration (chooses nearest bootstrap)
#{
    realm => <<"com.macula.global">>,
    bootstrap_nodes => [
        #{endpoint => <<"quic://bootstrap-a.macula.io:4433">>, region => <<"us-east">>},
        #{endpoint => <<"quic://bootstrap-b.macula.io:4433">>, region => <<"eu-west">>},
        #{endpoint => <<"quic://bootstrap-c.macula.io:4433">>, region => <<"apac">>}
    ],
    bootstrap_selection => nearest  % or: random, roundrobin
}

%% Bootstrap configuration
#{
    bootstrap_id => <<"bootstrap-a">>,
    region => <<"us-east">>,
    federation => #{
        peers => [
            <<"quic://bootstrap-b.macula.io:4433">>,
            <<"quic://bootstrap-c.macula.io:4433">>
        ],
        sync_interval_ms => 30000,
        replication_factor => 2
    }
}
```

#### Cross-Mesh Message Flow

```
Peer-A (US-East) wants to send to Peer-B (EU-West):

1. Peer-A sends PUBLISH to Bootstrap-A
2. Bootstrap-A looks up Peer-B in local DHT
3. Not found → queries federated bootstraps
4. Bootstrap-B responds: "Peer-B is my client"
5. Bootstrap-A forwards message to Bootstrap-B
6. Bootstrap-B delivers to Peer-B

Total hops: 4 (vs 2 for same-mesh)
```

### Implementation Requirements

| Component | Current | Multi-Bootstrap |
|-----------|---------|-----------------|
| `macula_bootstrap_system` | Single node | Federation-aware |
| `macula_dht_server` | Local only | Cross-mesh queries |
| `macula_peer_connector` | Single bootstrap | Multi-bootstrap failover |
| Configuration | Single endpoint | Endpoint list + selection |
| Health checks | None | Bootstrap liveness probing |

### New Modules Required

1. **`macula_bootstrap_federation.erl`**
   - Discovers and connects to peer bootstraps
   - Maintains federation membership
   - Routes cross-mesh queries

2. **`macula_bootstrap_sync.erl`**
   - Synchronizes DHT entries across bootstraps
   - Handles conflict resolution (LWW)
   - Implements replication factor

3. **`macula_peer_bootstrap_selector.erl`**
   - Selects optimal bootstrap for peer
   - Implements failover logic
   - Monitors bootstrap health

### Scaling Table with Multiple Bootstraps

| Peers | Bootstraps | Peers/Bootstrap | Inter-Bootstrap Traffic |
|-------|------------|-----------------|------------------------|
| 100 | 1 | 100 | N/A |
| 500 | 1 | 500 | N/A |
| 1,000 | 2 | 500 | ~10% of total |
| 2,000 | 4 | 500 | ~15% of total |
| 5,000 | 10 | 500 | ~20% of total |
| 10,000 | 20 | 500 | ~25% of total |

**Inter-Bootstrap Traffic** = Messages that need cross-mesh routing (depends on peer distribution and communication patterns).

---

## Summary: Action Items

### Immediate (PING/PONG)

1. Create `macula_ping_pong.erl` module (or extend chatter)
2. Add pending PING tracking with correlation IDs
3. Implement timeout handling
4. Add RTT statistics collection
5. Update run-test.sh to enable PING/PONG mode

### Short-term (Bootstrap Optimization)

1. Profile bootstrap under 100+ peer load
2. Identify CPU/memory hotspots
3. Optimize pub/sub fan-out (batching, connection pooling)
4. Add bootstrap health metrics endpoint

### Medium-term (Multi-Bootstrap)

1. Design federation protocol (ADR)
2. Implement `macula_bootstrap_federation.erl`
3. Modify peer bootstrap selection
4. Test with 2 bootstraps in Docker
5. Document multi-bootstrap deployment

### Long-term (Production Scale)

1. Geographic bootstrap deployment
2. Automated bootstrap scaling (K8s HPA)
3. Cross-region latency optimization
4. Global DHT consistency model

---

## Appendix: Test Configuration for PING/PONG

```bash
# docker-compose.50-chatters.yml environment addition
environment:
  CHATTER_MODE: ping_pong    # vs "broadcast"
  PING_INTERVAL_MS: 5000
  PING_TIMEOUT_MS: 3000
  PING_TARGET_STRATEGY: random  # or: roundrobin, all
```
