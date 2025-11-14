# God Module Refactoring Plan: macula_connection.erl

**Status:** 🚧 In Progress
**Date:** 2025-01-13
**Related:** `CODE_REVIEW_REPORT.md`, `CLAUDE.md`
**Module Size:** 2,030 lines (CRITICAL)

## Overview

This document outlines the TDD-based refactoring plan for splitting `macula_connection.erl` (2,030 LOC) into 6 focused modules, each following the Single Responsibility Principle.

## Current State Analysis

### Module Responsibilities (10 concerns - TOO MANY!)

1. **QUIC Connection Lifecycle** - Connection management, stream setup
2. **QUIC Stream Management** - Stream send/receive, buffering
3. **Protocol Message Encoding/Sending** - MessagePack encoding, QUIC send
4. **RPC Calls** - Call execution, failover logic, 8 provider selection strategies
5. **Pub/Sub Subscriptions** - Subscribe, unsubscribe, topic pattern matching
6. **Service Advertisement** - DHT advertisement, TTL management
7. **QoS Handling** - At-most-once, At-least-once delivery
8. **Provider Selection** - Random, round-robin, least-connections, etc.
9. **Connection Pooling** - Multi-endpoint management
10. **Message Buffering** - Async delivery, partial message handling

### Current Metrics

- **Lines of Code:** 2,030
- **Function Clauses:** 78+
- **Cyclomatic Complexity:** Very High
- **Test Coverage:** Limited (basic connection tests exist)
- **Cognitive Load:** Excessive

### Impact

- ❌ Cannot test individual concerns in isolation
- ❌ High coupling between unrelated features
- ❌ Difficult to modify without breaking other features
- ❌ Violates Single Responsibility Principle
- ❌ Hard to onboard new developers

## Refactoring Strategy

Following **TDD methodology** (established in DHT cache implementation):

### Phase 1: Establish Test Coverage (Week 1-2)

**Goal:** Comprehensive test suite for existing `macula_connection.erl` before any refactoring

**Approach:**
1. Document all current behaviors and edge cases
2. Write failing tests for each responsibility area
3. Make existing code pass tests (if needed)
4. Achieve >80% test coverage for connection module

**Test Areas:**

#### A. Connection Lifecycle Tests
```erlang
% test/macula_connection_lifecycle_tests.erl
- [ ] Connection initialization
- [ ] QUIC connection establishment
- [ ] Stream creation and management
- [ ] Connection state transitions (connecting → connected → disconnected)
- [ ] Connection error handling
- [ ] Connection timeout handling
- [ ] Graceful shutdown
```

#### B. Message Protocol Tests
```erlang
% test/macula_connection_protocol_tests.erl
- [ ] Message encoding (JSON, MessagePack)
- [ ] Message sending over QUIC stream
- [ ] Message receiving and buffering
- [ ] Partial message assembly
- [ ] Protocol error handling
- [ ] Message ID generation and uniqueness
```

#### C. Pub/Sub Operation Tests
```erlang
% test/macula_connection_pubsub_tests.erl
- [ ] Subscribe to topic
- [ ] Unsubscribe from topic
- [ ] Publish message (QoS 0)
- [ ] Publish message (QoS 1) with acknowledgment
- [ ] Topic pattern matching (exact, wildcard)
- [ ] Multiple subscriptions per connection
- [ ] Subscription callback invocation
- [ ] DHT subscriber discovery
- [ ] Subscriber cache usage
```

#### D. RPC Operation Tests
```erlang
% test/macula_connection_rpc_tests.erl
- [ ] RPC call execution
- [ ] RPC response handling
- [ ] RPC timeout handling
- [ ] Provider discovery via DHT
- [ ] Provider selection strategies (all 8)
- [ ] RPC failover logic
- [ ] Concurrent RPC calls
- [ ] RPC error propagation
```

#### E. Service Advertisement Tests
```erlang
% test/macula_connection_advertisement_tests.erl
- [ ] Advertise service to DHT
- [ ] Unadvertise service from DHT
- [ ] Service TTL management
- [ ] Re-advertisement timer
- [ ] Service metadata handling
- [ ] Service handler registration
```

#### F. Provider Selection Tests
```erlang
% test/macula_connection_provider_tests.erl
- [ ] Random selection
- [ ] Round-robin selection
- [ ] Least-connections selection
- [ ] Weighted selection
- [ ] Failover to next provider
- [ ] Provider health tracking
- [ ] Provider exclusion list
- [ ] Provider cache usage
```

#### G. Connection Pool Tests
```erlang
% test/macula_connection_pool_tests.erl
- [ ] Multi-endpoint connection management
- [ ] Connection pool initialization
- [ ] Connection reuse
- [ ] Connection health checks
- [ ] Pool size limits
- [ ] Connection cleanup on error
```

**Deliverable:** Comprehensive test suite with >80% coverage for `macula_connection.erl`

---

### Phase 2: Extract Connection Manager (Week 3)

**Goal:** Extract core connection lifecycle management into focused module

**New Module:** `macula_connection_manager.erl` (~300 LOC)

**Responsibilities:**
- QUIC connection lifecycle (init, connect, disconnect, terminate)
- QUIC stream management (create, send, receive)
- Base message sending/receiving
- Connection state tracking
- Receive buffer management

**State:**
```erlang
-record(manager_state, {
    connection :: undefined | pid(),
    stream :: undefined | pid(),
    status :: connecting | connected | disconnected | error,
    recv_buffer :: binary()
}).
```

**Public API:**
```erlang
-export([
    start_link/2,
    stop/1,
    send_message/3,
    get_status/1
]).
```

**TDD Steps:**
1. Write failing tests for new module API
2. Implement `macula_connection_manager.erl` (Green)
3. Extract code from `macula_connection.erl`
4. Run tests - ensure all pass
5. Update callers to use new module

**Test Coverage Target:** 90%+

---

### Phase 3: Extract Pub/Sub Handler (Week 4)

**Goal:** Extract all pub/sub operations into focused module

**New Module:** `macula_connection_pubsub.erl` (~250 LOC)

**Responsibilities:**
- Subscribe/unsubscribe operations
- Topic pattern matching
- QoS handling (0 and 1)
- Publish message delivery
- Subscription callback management
- DHT subscriber discovery
- Subscriber cache integration

**State:**
```erlang
-record(pubsub_state, {
    subscriptions :: #{reference() => {binary(), fun((map()) -> ok)}},
    advertised_subscriptions :: #{binary() => #{sub_ref, ttl, timer_ref}},
    pending_pubacks :: #{binary() => {binary(), binary(), integer(), integer(), reference()}},
    service_registry :: macula_service_registry:registry()
}).
```

**Public API:**
```erlang
-export([
    init/1,
    subscribe/4,
    unsubscribe/2,
    publish/5,
    handle_puback/2,
    handle_message_delivery/3
]).
```

**TDD Steps:**
1. Write failing tests for pub/sub API
2. Implement `macula_connection_pubsub.erl`
3. Extract pub/sub code from `macula_connection.erl`
4. Run full test suite
5. Update callers

**Test Coverage Target:** 90%+

---

### Phase 4: Extract RPC Handler (Week 5)

**Goal:** Extract RPC call operations into focused module

**New Module:** `macula_connection_rpc.erl` (~400 LOC)

**Responsibilities:**
- RPC call execution
- Request/response tracking
- Timeout management
- Failover logic
- Concurrent call handling
- Call ID generation

**State:**
```erlang
-record(rpc_state, {
    pending_calls :: #{binary() => {term(), reference()}},
    msg_id_counter :: non_neg_integer()
}).
```

**Public API:**
```erlang
-export([
    init/1,
    call/5,
    handle_reply/3,
    handle_timeout/2,
    next_call_id/1
]).
```

**TDD Steps:**
1. Write failing tests for RPC API (including failover scenarios)
2. Implement `macula_connection_rpc.erl`
3. Extract RPC code from `macula_connection.erl`
4. Test all 8 provider selection strategies
5. Run full test suite

**Test Coverage Target:** 90%+

---

### Phase 5: Extract Advertisement Handler (Week 6)

**Goal:** Extract service advertisement operations

**New Module:** `macula_connection_advertisement.erl` (~200 LOC)

**Responsibilities:**
- Service advertisement to DHT
- Service unadvertisement
- TTL management
- Re-advertisement timers
- Service metadata handling
- Handler registration

**State:**
```erlang
-record(advertisement_state, {
    advertised_services :: #{binary() => #{
        handler := fun((map()) -> {ok, term()} | {error, term()}),
        metadata := map(),
        ttl := pos_integer(),
        timer_ref := reference()
    }},
    service_registry :: macula_service_registry:registry()
}).
```

**Public API:**
```erlang
-export([
    init/1,
    advertise/5,
    unadvertise/2,
    handle_timer/2,
    get_handler/2
]).
```

**TDD Steps:**
1. Write failing tests for advertisement API
2. Implement `macula_connection_advertisement.erl`
3. Extract advertisement code
4. Test timer-based re-advertisement
5. Run full test suite

**Test Coverage Target:** 90%+

---

### Phase 6: Extract Connection Pool Handler (Week 7)

**Goal:** Extract connection pooling logic

**New Module:** `macula_connection_pool.erl` (~300 LOC)

**Responsibilities:**
- Connection pooling
- Multi-endpoint management
- Connection health tracking
- Connection reuse
- Pool size management

**State:**
```erlang
-record(pool_state, {
    connections :: #{endpoint() => connection_info()},
    max_connections :: pos_integer(),
    health_checks :: #{endpoint() => health_status()}
}).
```

**Public API:**
```erlang
-export([
    init/1,
    get_connection/2,
    return_connection/2,
    add_endpoint/2,
    remove_endpoint/2,
    check_health/1
]).
```

**TDD Steps:**
1. Write failing tests for pool API
2. Implement `macula_connection_pool.erl`
3. Extract pooling code
4. Test connection reuse and limits
5. Run full test suite

**Test Coverage Target:** 90%+

---

### Phase 7: Extract Provider Selection (Week 8)

**Goal:** Extract provider selection strategies

**New Module:** `macula_connection_provider.erl` (~200 LOC)

**Responsibilities:**
- Provider selection strategies (8 types)
- Provider discovery via DHT
- Provider caching
- Provider exclusion tracking
- Selection state management

**State:**
```erlang
-record(provider_state, {
    strategy :: atom(),
    selector_state :: map(),
    provider_cache :: #{procedure() => [provider()]},
    excluded_providers :: [node_id()]
}).
```

**Public API:**
```erlang
-export([
    init/1,
    discover_providers/3,
    select_provider/3,
    exclude_provider/2,
    update_strategy/2
]).
```

**TDD Steps:**
1. Write failing tests for all 8 selection strategies
2. Implement `macula_connection_provider.erl`
3. Extract provider selection code
4. Test strategy switching
5. Run full test suite

**Test Coverage Target:** 90%+

---

### Phase 8: Refactor Main Module (Week 9)

**Goal:** Reduce `macula_connection.erl` to coordination layer only

**New Responsibilities (single concern):**
- API facade (public interface)
- Coordination between sub-modules
- GenServer callbacks (delegating to sub-modules)
- State composition

**New State:**
```erlang
-record(state, {
    url :: binary(),
    opts :: map(),
    realm :: binary(),
    node_id :: binary(),

    %% Sub-module states
    manager :: macula_connection_manager:state(),
    pubsub :: macula_connection_pubsub:state(),
    rpc :: macula_connection_rpc:state(),
    advertisement :: macula_connection_advertisement:state(),
    pool :: macula_connection_pool:state(),
    provider :: macula_connection_provider:state()
}).
```

**Public API (unchanged for backward compatibility):**
```erlang
-export([
    start_link/2,
    stop/1,
    publish/3,
    publish/4,
    subscribe/3,
    unsubscribe/2,
    call/3,
    call/4,
    advertise/4,
    unadvertise/2
]).
```

**Implementation Pattern:**
```erlang
%% API delegates to sub-modules
publish(Pid, Topic, Payload, Opts) ->
    gen_server:call(Pid, {publish, Topic, Payload, Opts}).

handle_call({publish, Topic, Payload, Opts}, From, State) ->
    PubSubState = State#state.pubsub,
    ManagerState = State#state.manager,

    case macula_connection_pubsub:publish(PubSubState, Topic, Payload, Opts, ManagerState) of
        {ok, PubSubState2} ->
            {reply, ok, State#state{pubsub = PubSubState2}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.
```

**TDD Steps:**
1. Write integration tests for refactored API
2. Update `macula_connection.erl` to delegate to sub-modules
3. Run ALL tests (unit + integration)
4. Performance regression testing
5. Update documentation

**Target LOC:** <400 lines (coordination only)
**Test Coverage Target:** 95%+ (integration tests)

---

## Validation Criteria

### Code Quality Metrics

- [ ] All modules < 400 LOC
- [ ] Each module has single, clear responsibility
- [ ] No `if` statements (pattern matching on function clauses)
- [ ] No `try..catch` for normal flow
- [ ] Maximum 2 levels of nesting
- [ ] Functions < 50 lines (ideally < 30)

### Test Coverage Metrics

- [ ] Unit test coverage > 90% per module
- [ ] Integration test coverage > 85% overall
- [ ] All edge cases covered
- [ ] All error paths tested
- [ ] Concurrency scenarios tested

### Performance Metrics

- [ ] No performance regression (benchmark before/after)
- [ ] RPC latency unchanged or improved
- [ ] Pub/Sub throughput unchanged or improved
- [ ] Memory usage unchanged or reduced

### Architecture Metrics

- [ ] No circular dependencies
- [ ] Clear module boundaries
- [ ] Minimal coupling between modules
- [ ] Well-defined interfaces
- [ ] Backward compatible public API

---

## Testing Strategy

### Unit Testing (EUnit)

**File naming:** `test/<module_name>_tests.erl`

**Pattern:**
```erlang
-module(macula_connection_manager_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test groups
connection_lifecycle_test_() ->
    [
        ?_test(test_init()),
        ?_test(test_connect()),
        ?_test(test_disconnect()),
        ?_test(test_error_handling())
    ].

%% Individual tests
test_init() ->
    State = macula_connection_manager:init(#{}),
    ?assertEqual(connecting, State#manager_state.status).
```

### Integration Testing (Common Test)

**File:** `test/macula_connection_refactored_SUITE.erl`

```erlang
all() ->
    [
        test_pubsub_roundtrip,
        test_rpc_roundtrip,
        test_advertisement_roundtrip,
        test_failover_behavior,
        test_concurrent_operations
    ].
```

### Property-Based Testing (PropEr)

**For algorithmic components:**
```erlang
-module(macula_connection_provider_prop_tests).
-include_lib("proper/include/proper.hrl").

prop_round_robin_fairness() ->
    ?FORALL(Providers, list(provider()),
        length(Providers) > 0 ->
            test_round_robin_distribution(Providers)
    ).
```

---

## Risks and Mitigation

### Risk 1: Breaking Existing Functionality

**Mitigation:**
- Comprehensive test suite BEFORE refactoring
- Maintain backward-compatible public API
- Integration tests for all user-facing features
- Gradual extraction (one module at a time)
- Rollback plan for each phase

### Risk 2: Performance Regression

**Mitigation:**
- Benchmark current performance before starting
- Performance tests after each extraction
- Profile hot paths (RPC, pub/sub)
- Optimize if regression detected
- Target: No more than 5% slowdown

### Risk 3: Test Suite Incompleteness

**Mitigation:**
- Review existing integration tests
- Document all current behaviors
- Test matrix covering all combinations
- Edge case analysis
- Manual testing of critical paths

### Risk 4: Module Coupling

**Mitigation:**
- Clear interface definitions
- Minimal state sharing
- Message passing between modules
- Dependency injection for testing
- Avoid circular dependencies

---

## Timeline Summary

| Phase | Week | Deliverable | LOC | Tests |
|-------|------|-------------|-----|-------|
| 1. Test Coverage | 1-2 | Comprehensive test suite for existing module | N/A | 50+ tests |
| 2. Connection Manager | 3 | `macula_connection_manager.erl` | 300 | 20+ tests |
| 3. Pub/Sub Handler | 4 | `macula_connection_pubsub.erl` | 250 | 25+ tests |
| 4. RPC Handler | 5 | `macula_connection_rpc.erl` | 400 | 30+ tests |
| 5. Advertisement Handler | 6 | `macula_connection_advertisement.erl` | 200 | 15+ tests |
| 6. Connection Pool | 7 | `macula_connection_pool.erl` | 300 | 20+ tests |
| 7. Provider Selection | 8 | `macula_connection_provider.erl` | 200 | 20+ tests |
| 8. Main Module Refactor | 9 | Refactored `macula_connection.erl` | <400 | 30+ tests |

**Total Estimated Time:** 9 weeks
**Total New Tests:** 210+ tests
**Expected Final LOC:** ~2,050 lines across 7 modules (avg 293 LOC/module)

---

## Success Criteria

### Must Have
- ✅ All existing tests pass
- ✅ All new unit tests pass (>90% coverage per module)
- ✅ Integration tests pass
- ✅ No performance regression
- ✅ Backward-compatible public API
- ✅ All modules follow idiomatic Erlang (no `if`, no `try..catch`, pattern matching)

### Should Have
- ✅ Documentation updated (module docs, architecture docs)
- ✅ CHANGELOG entry
- ✅ Code review approval
- ✅ Performance improvements demonstrated

### Nice to Have
- ✅ Property-based tests for algorithmic components
- ✅ Benchmark comparisons documented
- ✅ Architecture decision records (ADRs) for major decisions

---

## Next Steps

1. **Review this plan** - Ensure all stakeholders agree on approach
2. **Set up test infrastructure** - EUnit, Common Test, PropEr
3. **Begin Phase 1** - Write comprehensive tests for existing module
4. **Execute phases sequentially** - One module at a time, TDD throughout
5. **Monitor progress** - Weekly checkpoints, test coverage tracking

---

## References

- `CODE_REVIEW_REPORT.md` - Original analysis and recommendations
- `CLAUDE.md` - Coding guidelines (TDD, idiomatic Erlang)
- `architecture/dht_cache_performance_guide.md` - Example of TDD implementation
- `test/macula_service_registry_tests.erl` - Example of comprehensive test suite

---

**Document Version:** 1.0
**Last Updated:** 2025-01-13
**Next Review:** After Phase 1 completion
