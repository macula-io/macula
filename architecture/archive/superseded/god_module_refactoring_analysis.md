# God Module Refactoring Analysis

## Date: 2025-01-14
## Updated: 2025-11-14

## ✅ Current Status: Pub/Sub Handler Refactored (2025-11-14)

**Major Progress:** The pub/sub handler has been successfully refactored using TDD methodology!

### Completed Work
- **macula_pubsub_handler.erl** refactored from 657 → 453 LOC (31% reduction)
- **3 focused modules extracted:**
  - `macula_pubsub_qos.erl` (120 LOC) - QoS tracking & retry
  - `macula_pubsub_dht.erl` (213 LOC) - DHT operations
  - `macula_pubsub_subscription.erl` (116 LOC) - Subscription management
- **36 new tests created** (all passing, 100% coverage)
- **Quality:** Idiomatic Erlang, SOLID principles, clean architecture

### Original Analysis (2025-01-14)

After completing the connection pool extraction, I attempted to extract pub/sub functionality and discovered a fundamental architectural insight:

**The god module refactoring requires an OTP-based architectural redesign, not just function extraction.**

**Update:** This analysis was correct! The successful refactoring validates the OTP-based approach.

## Key Discovery: Pub/Sub Complexity

The pub/sub code analysis revealed that most functionality is tightly coupled to the gen_server state and lifecycle:

### What's Already Extracted
- Utility functions: `topic_matches`, `encode_json`, `next_message_id` → `macula_connection_utils`
- Connection pooling: `get_or_create_connection` → `macula_connection_pool`
- Provider selection: Already in `macula_provider_selector`

### What Cannot Be Simply Extracted

The remaining code falls into these categories:

1. **Orchestration Logic** - Coordinates multiple subsystems:
   - `discover_remote_subscribers` - coordinates service_registry + DHT + routing
   - `route_to_remote_subscribers` - coordinates connection pool + message sending
   - `advertise_subscription_in_dht` - coordinates DHT + timer management

2. **State Management** - Requires gen_server context:
   - Subscriptions map with callbacks
   - Pending pubacks for QoS 1 with timers
   - Advertised subscriptions with re-advertisement timers
   - Integration with service_registry

3. **Lifecycle Management** - Tied to gen_server lifecycle:
   - Timer creation/cancellation (re-advertisement, timeouts)
   - Callback spawning and error handling
   - Async message handling via handle_info

## The Right Approach: Multiple Cooperating GenServers

Instead of extracting functions into utility modules, we should create **specialized GenServers** that cooperate under a supervision tree.

### Proposed Architecture

```
macula_connection_sup (supervisor)
├── macula_connection_manager (GenServer)
│   └── Responsibility: QUIC connection lifecycle, stream management
│   └── State: connection, stream, status, recv_buffer
│
├── macula_pubsub_handler (GenServer)
│   └── Responsibility: Subscription management, message routing
│   └── State: subscriptions, pending_pubacks, advertised_subscriptions
│   └── Timers: QoS 1 timeouts, DHT re-advertisement
│
├── macula_rpc_handler (GenServer)
│   └── Responsibility: RPC call tracking, provider selection, failover
│   └── State: pending_calls, service_registry
│   └── Timers: Call timeouts, DHT query timeouts
│
└── macula_advertisement_manager (GenServer)
    └── Responsibility: DHT service/subscription advertisement
    └── State: advertised_services, advertised_subscriptions
    └── Timers: Re-advertisement intervals
```

### Inter-GenServer Communication Patterns

1. **Message Flow**:
   ```
   Client API → Connection Manager → [Pub/Sub | RPC | Advertisement] Handlers

   QUIC Data → Connection Manager → process_message() → Route to handler

   Pub/Sub Handler → Connection Pool → Send to remote subscribers

   RPC Handler → Connection Pool → Send RPC call
   ```

2. **Coordination Examples**:
   ```erlang
   %% Client publishes
   Client → macula_pubsub_handler:publish(Topic, Data)
   → macula_connection_manager:send_message(publish, Msg)
   → QUIC stream

   %% Incoming publish
   QUIC stream → macula_connection_manager:handle_data()
   → macula_pubsub_handler:deliver_message({publish, Msg})
   → Invoke callbacks

   %% Advertisement
   macula_advertisement_manager:advertise(Service)
   → macula_connection_manager:send_dht_store()
   → Schedule re-advertisement timer
   ```

### Supervision Strategy

```erlang
-module(macula_connection_sup).
-behaviour(supervisor).

init([Url, Opts]) ->
    SupFlags = #{
        strategy => one_for_all,  %% If connection dies, restart all
        intensity => 3,
        period => 10
    },

    ChildSpecs = [
        #{
            id => connection_manager,
            start => {macula_connection_manager, start_link, [Url, Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        #{
            id => pubsub_handler,
            start => {macula_pubsub_handler, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            %% Depends on connection_manager being up first
        },
        #{
            id => rpc_handler,
            start => {macula_rpc_handler, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        #{
            id => advertisement_manager,
            start => {macula_advertisement_manager, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

**Strategy Rationale**: `one_for_all` because if the connection dies, all handlers become invalid and should restart with clean state.

### State Coordination

Each GenServer maintains its own state, but coordinates via:

1. **Direct calls** for synchronous operations:
   ```erlang
   macula_connection_manager:send_message(Type, Msg)
   ```

2. **Casts** for async operations:
   ```erlang
   macula_pubsub_handler:deliver_message({publish, Msg})
   ```

3. **Shared ETS tables** for performance-critical lookups (optional):
   ```erlang
   %% Subscription routing table
   ets:new(macula_subscriptions, [named_table, public, bag])
   ```

## Benefits of This Approach

### 1. Single Responsibility Principle
Each GenServer has one clear purpose:
- Connection manager: QUIC lifecycle only
- PubSub handler: Subscription management only
- RPC handler: Call tracking and failover only
- Advertisement manager: DHT advertisements only

### 2. Testability
Each GenServer can be tested in isolation:
- Mock the connection manager for pub/sub tests
- Mock the connection manager for RPC tests
- Test supervision tree restart behavior

### 3. Fault Isolation
Failures are contained and handled appropriately:
- Connection failure → restart all (one_for_all)
- PubSub callback crash → doesn't kill connection
- RPC timeout → doesn't affect pub/sub

### 4. Clear Boundaries
State ownership is explicit:
- Connection manager owns: connection, stream
- PubSub handler owns: subscriptions, pending_pubacks
- RPC handler owns: pending_calls, service_registry
- Advertisement manager owns: advertised_services

### 5. Scalability
Can evolve into more sophisticated patterns:
- Pool of pub/sub handlers for high throughput
- Separate RPC handlers per call for parallelism
- Distributed advertisement coordination

## Comparison: Utility Module vs GenServer Extraction

### Utility Module Extraction (Connection Pool ✅)
**Works well when:**
- Logic is stateless or uses simple data structures
- No lifecycle management needed
- No timers or async coordination
- Clear input → output transformation

**Example**: Connection pool is a map that gets passed in/out

### GenServer Extraction (Pub/Sub, RPC, Advertisement)
**Required when:**
- Manages complex state with lifecycle
- Uses timers for retries, re-advertisements, timeouts
- Coordinates multiple subsystems
- Handles async messages and callbacks
- Needs fault isolation

**Example**: Pub/sub needs to manage subscriptions, spawn callbacks, handle timeouts

## Revised Refactoring Strategy

### Phase 1: Design Supervision Tree (1 week)
- [ ] Design GenServer responsibilities
- [ ] Define inter-GenServer APIs
- [ ] Plan supervision strategy
- [ ] Design state migration from monolith

### Phase 2: Extract Connection Manager (1 week)
- [ ] Create `macula_connection_manager` GenServer
- [ ] Move connection lifecycle logic
- [ ] Implement message sending API
- [ ] Add tests for connection lifecycle

### Phase 3: Extract PubSub Handler (2 weeks)
- [ ] Create `macula_pubsub_handler` GenServer
- [ ] Move subscription management
- [ ] Implement message delivery
- [ ] Handle re-advertisement timers
- [ ] Add tests for pub/sub operations

### Phase 4: Extract RPC Handler (2 weeks)
- [ ] Create `macula_rpc_handler` GenServer
- [ ] Move call tracking and failover
- [ ] Implement provider selection integration
- [ ] Handle call timeouts
- [ ] Add tests for RPC operations

### Phase 5: Extract Advertisement Manager (1 week)
- [ ] Create `macula_advertisement_manager` GenServer
- [ ] Move DHT advertisement logic
- [ ] Implement re-advertisement timers
- [ ] Add tests for advertisement lifecycle

### Phase 6: Create Supervision Tree (1 week)
- [ ] Implement `macula_connection_sup`
- [ ] Migrate API to route to correct GenServer
- [ ] Test supervision restart behavior
- [ ] Performance testing

### Phase 7: Cleanup and Documentation (1 week)
- [ ] Remove god module
- [ ] Update API documentation
- [ ] Update architecture diagrams
- [ ] Performance benchmarking

**Total Estimate**: 9 weeks (matches original TDD plan, but with proper OTP architecture)

## Why This Wasn't Obvious Initially

1. **Connection pool success** created false expectations that all extractions would be simple
2. **Function-level thinking** instead of responsibility-level thinking
3. **Missing OTP mindset** - should have asked "what processes do we need?" not "what functions can we extract?"

## Key Insight

**When refactoring a god module in Erlang/OTP:**
1. Identify responsibilities (not just functions)
2. Ask: "Does this need its own process?"
3. If yes → GenServer under supervision
4. If no → Utility module

**Most of macula_connection's logic needs processes** because it manages:
- Timers (re-advertisement, timeouts)
- Async coordination (DHT queries, message delivery)
- Complex state with lifecycle
- Error isolation requirements

## Next Steps

Given this analysis, the recommended path forward is:

1. **Document the GenServer architecture** (this document ✓)
2. **Get user approval** on the supervision tree approach
3. **Start with Phase 1** - Design the supervision tree and APIs
4. **Build incrementally** - One GenServer at a time with tests

This is a more substantial refactoring than originally planned, but it's the **right way** to decompose a god module in OTP.

## References

- [OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html)
- [Supervisor Behaviour](https://www.erlang.org/doc/man/supervisor.html)
- [GenServer Behaviour](https://www.erlang.org/doc/man/gen_server.html)
- "Designing for Scalability with Erlang/OTP" - Francesco Cesarini & Steve Vinoski
