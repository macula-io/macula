# Pub/Sub Handler Refactoring Plan - TDD Approach

**Date:** January 15, 2025
**Module:** macula_pubsub_handler.erl (657 LOC → 3 modules)
**Approach:** Test-Driven Development + Declarative Programming
**Duration:** 20 hours estimated

---

## Executive Summary

Refactor `macula_pubsub_handler.erl` from a 657-line god module mixing 7 concerns into 3 focused, testable modules using TDD principles and idiomatic Erlang patterns.

### Current State

**File:** macula_pubsub_handler.erl (657 LOC)
**Tests:** 22 functions, 8 passing, 3 cancelled (connection manager dependency)
**Issues:**
- 7 mixed responsibilities
- spawn/1 fire-and-forget (no monitoring)
- Deep nesting in unsubscribe
- case statements where pattern matching better
- Blocks pub/sub optimization work

### Target State

**3 Focused Modules:**
1. `macula_pubsub_subscription.erl` (250 LOC) - Subscription management
2. `macula_pubsub_dht.erl` (200 LOC) - DHT operations
3. `macula_pubsub_qos.erl` (150 LOC) - QoS 1 tracking & retries
4. `macula_pubsub_handler.erl` (150 LOC) - Coordinator/facade

**Tests:** 80%+ coverage with mocks for external dependencies
**Benefits:** Testable, maintainable, enables sharding optimization

---

## Current Module Analysis

### 7 Mixed Concerns

#### 1. Subscription Management (lines 132-201, ~70 LOC)
**Responsibility:** Local subscription storage and lifecycle
```erlang
- subscribe/3 - Add subscription with callback
- unsubscribe/2 - Remove subscription
- subscriptions map - #{SubRef => {Topic, Callback}}
- advertised_subscriptions map - #{Topic => SubInfo}
```

**Extract To:** `macula_pubsub_subscription.erl`

#### 2. DHT Advertisement (lines 415-464, ~50 LOC)
**Responsibility:** Advertise subscriptions in DHT for mesh routing
```erlang
- advertise_subscription_in_dht/3 - STORE in DHT
- Timer management for re-advertisement
- TTL handling (300s, re-advertise at TTL-60s)
```

**Extract To:** `macula_pubsub_dht.erl` (advertisement part)

#### 3. Publishing (lines 207-300, ~95 LOC)
**Responsibility:** Publish messages locally and mesh-wide
```erlang
- publish/4 - Build and send publish message
- encode_payload/1 - Encode to binary
- next_message_id/1 - Generate unique IDs
- Async publish via cast (avoid blocking)
```

**Keep In:** `macula_pubsub_handler.erl` (coordinator)

#### 4. DHT Discovery (lines 466-524, ~60 LOC)
**Responsibility:** Discover remote subscribers via DHT
```erlang
- discover_remote_subscribers/5 - Check cache, query DHT
- query_dht_for_subscribers/4 - FIND_VALUE in DHT
- pending_subscriber_queries map - Track queries
- Cache integration with service_registry
```

**Extract To:** `macula_pubsub_dht.erl` (discovery part)

#### 5. Message Routing (lines 306-326, 526-551, ~70 LOC)
**Responsibility:** Route incoming publishes to matching subscriptions
```erlang
- handle_incoming_publish - Pattern match subscriptions
- route_to_remote_subscribers/5 - Forward to mesh
- Pattern matching logic - topic_matches/3
```

**Keep In:** `macula_pubsub_handler.erl` (coordinator)

#### 6. QoS Management (lines 268-402, ~135 LOC)
**Responsibility:** QoS 1 at-least-once delivery with retries
```erlang
- pending_pubacks map - #{MsgId => {Topic, Payload, QoS, RetryCount, TimerRef}}
- puback_timeout handler - Retry logic
- Max retry handling - Give up after 3 retries
```

**Extract To:** `macula_pubsub_qos.erl`

#### 7. Callback Invocation (lines 617-657, ~40 LOC)
**Responsibility:** Invoke subscription callbacks asynchronously
```erlang
- invoke_subscription_callbacks/4 - Spawn callbacks
- Error handling for callback failures
- Decode JSON payloads
```

**Keep In:** `macula_pubsub_handler.erl` (invocation stays with routing)

---

## Refactoring Strategy

### Phase 1: Test Coverage & Mocking (4-6 hours)

**Goal:** Ensure 80%+ test coverage with mocks for external dependencies

**Tasks:**
1. Create mock for `macula_connection_manager` (gen_server behavior)
2. Add tests for DHT operations (advertisement, discovery)
3. Add tests for QoS 1 retry logic (timeout, max retries)
4. Add tests for callback invocation (error handling)
5. Fix 3 cancelled tests (connection manager dependency)

**Deliverable:** Full test suite passing (30+ tests, 0 failures, 0 cancelled)

### Phase 2: Extract macula_pubsub_qos.erl (4-5 hours)

**Goal:** Extract QoS 1 management to dedicated module

**New Module API:**
```erlang
-module(macula_pubsub_qos).

%% API
-export([
    new/0,                    % Create QoS state
    track_publish/4,          % Track QoS 1 publish
    handle_timeout/2,         % Handle PUBACK timeout
    handle_ack/2,             % Handle PUBACK received
    get_pending_count/1       % Monitoring
]).

%% Types
-type qos_state() :: #{
    pending_pubacks => #{
        binary() => {Topic :: binary(), Payload :: binary(),
                     RetryCount :: integer(), TimerRef :: reference()}
    }
}.

%% QoS levels
-define(QOS_0, 0).  % At-most-once (fire-and-forget)
-define(QOS_1, 1).  % At-least-once (requires PUBACK)
-define(QOS_2, 2).  % Exactly-once (not implemented)

%% Retry configuration
-define(PUBACK_TIMEOUT, 5000).        % 5 seconds
-define(PUBACK_MAX_RETRIES, 3).       % Give up after 3 retries
```

**Refactoring Steps:**
1. Write tests for new QoS module API
2. Create `macula_pubsub_qos.erl` with functions
3. Update `macula_pubsub_handler.erl` to call QoS module
4. Run tests - ensure all pass
5. Remove QoS code from handler

**Pattern Improvements:**
- Use pattern matching on function heads for QoS levels
- Guards for retry count checks
- Declarative style for state updates

### Phase 3: Extract macula_pubsub_dht.erl (5-6 hours)

**Goal:** Extract DHT operations (advertisement + discovery)

**New Module API:**
```erlang
-module(macula_pubsub_dht).

%% API
-export([
    %% Advertisement
    advertise_subscription/5,       % Advertise in DHT
    handle_readvertise/3,          % Re-advertisement timer
    cancel_advertisement/2,        % Stop advertising

    %% Discovery
    discover_subscribers/3,        % Discover via DHT (with cache)
    query_dht/4,                  % Query DHT for topic
    handle_dht_response/3,        % Process FIND_VALUE response

    %% State management
    new/0,                        % Create DHT state
    get_advertised_count/1        % Monitoring
]).

%% Types
-type dht_state() :: #{
    %% Advertised subscriptions with timers
    advertised => #{
        Topic :: binary() => #{
            sub_ref := reference(),
            ttl := pos_integer(),
            timer_ref := reference()
        }
    },

    %% Pending DHT queries
    pending_queries => #{
        MsgId :: binary() => {
            Topic :: binary(),
            Payload :: binary(),
            QoS :: integer(),
            Opts :: map()
        }
    }
}.

%% DHT configuration
-define(SUBSCRIPTION_TTL, 300).  % 5 minutes
-define(READVERTISE_OFFSET, 60).  % Re-advertise 60s before expiry
```

**Refactoring Steps:**
1. Write tests for DHT module API (advertisement + discovery)
2. Create `macula_pubsub_dht.erl` with functions
3. Update handler to call DHT module
4. Run tests - ensure all pass
5. Remove DHT code from handler

**Pattern Improvements:**
- Replace spawn/1 with monitored tasks or gen_server:cast
- Pattern matching for cache hit/miss
- Guards for TTL validation
- Declarative state updates (maps:merge, maps:update)

### Phase 4: Extract macula_pubsub_subscription.erl (4-5 hours)

**Goal:** Extract subscription management

**New Module API:**
```erlang
-module(macula_pubsub_subscription).

%% API
-export([
    %% Subscription lifecycle
    new/0,                        % Create subscription state
    add_subscription/3,           % Add subscription
    remove_subscription/2,        % Remove subscription
    find_matches/3,               % Find matching subscriptions
    get_all/1,                   % Get all subscriptions

    %% Query
    get_subscription/2,           % Get by ref
    get_count/1,                  % Count subscriptions
    has_subscription/2            % Check existence
]).

%% Types
-type subscription() :: #{
    ref := reference(),
    topic := binary(),
    callback := fun((map()) -> ok),
    created_at := integer()  % erlang:system_time(millisecond)
}.

-type subscription_state() :: #{
    subscriptions => #{reference() => subscription()}
}.
```

**Refactoring Steps:**
1. Write tests for subscription module API
2. Create `macula_pubsub_subscription.erl` with functions
3. Update handler to call subscription module
4. Run tests - ensure all pass
5. Remove subscription code from handler

**Pattern Improvements:**
- Pattern matching on function heads for empty/non-empty
- Guards for subscription validation
- Declarative lists:filter for matching
- No deep nesting (extract helper functions)

### Phase 5: Simplify macula_pubsub_handler.erl (2-3 hours)

**Goal:** Reduce handler to coordinator/facade (150 LOC)

**Final Handler Responsibilities:**
```erlang
%% Coordination only:
1. Gen_server callbacks (init, handle_call, handle_cast, handle_info)
2. API facade (subscribe, unsubscribe, publish, handle_incoming_publish)
3. Delegate to:
   - macula_pubsub_subscription (subscription management)
   - macula_pubsub_dht (DHT operations)
   - macula_pubsub_qos (QoS 1 tracking)
4. Message routing (invoke callbacks for matching subscriptions)
5. Connection manager integration
```

**State After Refactoring:**
```erlang
-record(state, {
    opts :: map(),
    node_id :: binary(),
    url :: binary(),
    realm :: binary(),
    connection_manager_pid :: pid() | undefined,

    %% Delegated state
    subscription_state :: macula_pubsub_subscription:subscription_state(),
    dht_state :: macula_pubsub_dht:dht_state(),
    qos_state :: macula_pubsub_qos:qos_state(),

    %% Topic matching config (could be extracted too)
    topic_separator = <<".">> :: binary(),
    topic_wildcard_single = <<"*">> :: binary(),
    topic_wildcard_multi = <<"**">> :: binary(),

    %% Service registry (for cache)
    service_registry :: macula_service_registry:registry()
}).
```

**Refactoring Steps:**
1. Update handler to use extracted modules
2. Simplify handle_call/handle_cast/handle_info
3. Remove deep nesting
4. Convert case to pattern matching where appropriate
5. Run full test suite - ensure all pass
6. Update documentation

---

## Code Quality Improvements

### Replace spawn/1 with Monitored Tasks

**Before (fire-and-forget, line 295):**
```erlang
handle_cast({discover_subscribers, Topic, Payload, Qos, Opts}, State) ->
    spawn(fun() ->
        _State2 = discover_remote_subscribers(Topic, Payload, Qos, Opts, State),
        ok
    end),
    {noreply, State};
```

**After (monitored task or gen_server:cast):**
```erlang
%% Option 1: Use gen_server:cast to self (already async)
handle_cast({discover_subscribers, Topic, Payload, Qos, Opts}, State) ->
    %% Process asynchronously but within gen_server (can update state)
    DhtState = State#state.dht_state,
    {DhtState2, SubscriberData} = macula_pubsub_dht:discover_subscribers(
        DhtState, Topic, State#state.service_registry
    ),

    %% Route to subscribers if found
    State2 = case SubscriberData of
        {ok, Subscribers} ->
            route_to_remote_subscribers(Topic, Payload, Qos, Subscribers, State),
            State#state{dht_state = DhtState2};
        {cache_miss, QueryId} ->
            %% DHT module sent query, track it
            State#state{dht_state = DhtState2}
    end,
    {noreply, State2};

%% Option 2: Use task supervisor (if tasks are long-running)
handle_cast({discover_subscribers, Topic, Payload, Qos, Opts}, State) ->
    TaskSup = State#state.task_supervisor,
    Task = {macula_pubsub_dht, discover_subscribers,
            [State#state.dht_state, Topic, State#state.service_registry]},
    {ok, _TaskPid} = supervisor:start_child(TaskSup, [Task]),
    {noreply, State};
```

**Rationale:** spawn/1 provides no error handling or monitoring. Use gen_server:cast (already in gen_server context) or task supervisor for proper OTP supervision.

### Simplify Deep Nesting in unsubscribe

**Before (deep nesting, lines 175-191):**
```erlang
AdvertisedSubscriptions2 = maps:fold(
    fun(T, SubInfo, Acc) ->
        case maps:get(sub_ref, SubInfo) of
            SubRef ->
                %% This is the subscription being removed
                TimerRef = maps:get(timer_ref, SubInfo),
                erlang:cancel_timer(TimerRef),
                ?LOG_DEBUG("[~s] Cancelled DHT advertisement for topic ~s",
                          [State#state.node_id, T]),
                Acc;  % Don't include in new map
            _ ->
                Acc#{T => SubInfo}  % Keep other subscriptions
        end
    end,
    #{},
    AdvertisedSubscriptions
),
```

**After (pattern matching in helper function):**
```erlang
%% Extract to helper function
cancel_advertisement_for_subscription(SubRef, AdvertisedSubscriptions, NodeId) ->
    maps:fold(
        fun(Topic, SubInfo, Acc) ->
            cancel_if_match(SubRef, Topic, SubInfo, Acc, NodeId)
        end,
        #{},
        AdvertisedSubscriptions
    ).

%% Pattern match on subscription reference
cancel_if_match(SubRef, Topic, #{sub_ref := SubRef, timer_ref := TimerRef}, Acc, NodeId) ->
    erlang:cancel_timer(TimerRef),
    ?LOG_DEBUG("[~s] Cancelled DHT advertisement for topic ~s", [NodeId, Topic]),
    Acc;  % Don't include in accumulator (removes from map)
cancel_if_match(_SubRef, Topic, SubInfo, Acc, _NodeId) ->
    Acc#{Topic => SubInfo}.  % Keep subscription
```

**Rationale:** Extracting to named helper function with pattern matching on function heads eliminates deep nesting and makes intent clearer.

### Convert case to Pattern Matching

**Before (case statement, lines 209-243):**
```erlang
handle_call({publish, Topic, Data, Opts}, _From, State) ->
    case State#state.connection_manager_pid of
        undefined ->
            {reply, {error, not_connected}, State};
        ConnMgrPid ->
            case macula_connection_manager:get_status(ConnMgrPid) of
                connected ->
                    %% Build and send publish...
                    {reply, ok, State2};
                _Status ->
                    {reply, {error, not_connected}, State}
            end
    end;
```

**After (guards + helper function):**
```erlang
handle_call({publish, Topic, Data, Opts}, _From, State) ->
    check_connection_and_publish(Topic, Data, Opts, State).

%% Pattern matching on connection status
check_connection_and_publish(_Topic, _Data, _Opts, #state{connection_manager_pid = undefined} = State) ->
    {reply, {error, not_connected}, State};
check_connection_and_publish(Topic, Data, Opts, State) ->
    check_status_and_publish(Topic, Data, Opts, State).

check_status_and_publish(Topic, Data, Opts, State) ->
    ConnMgrPid = State#state.connection_manager_pid,
    case macula_connection_manager:get_status(ConnMgrPid) of
        connected -> do_publish(Topic, Data, Opts, State);
        _Status -> {reply, {error, not_connected}, State}
    end.

do_publish(Topic, Data, Opts, State) ->
    Qos = maps:get(qos, Opts, 0),
    Retain = maps:get(retain, Opts, false),
    {MsgId, State2} = next_message_id(State),
    BinaryTopic = ensure_binary(Topic),
    Payload = encode_payload(Data),

    PublishMsg = #{
        topic => BinaryTopic,
        payload => Payload,
        qos => Qos,
        retain => Retain,
        message_id => MsgId
    },

    gen_server:cast(self(), {do_publish, PublishMsg, Qos, BinaryTopic, Payload, Opts, MsgId}),
    {reply, ok, State2}.
```

**Rationale:** Extract nested case to helper functions with pattern matching on function heads. Makes each function do one thing (single responsibility).

---

## Testing Strategy

### Test Structure

```
test/
  macula_pubsub_handler_tests.erl       (integration tests)
  macula_pubsub_subscription_tests.erl  (unit tests)
  macula_pubsub_dht_tests.erl           (unit tests)
  macula_pubsub_qos_tests.erl           (unit tests)
  macula_test_helpers.erl               (mocks, fixtures)
```

### Mock Strategy

**Connection Manager Mock:**
```erlang
-module(macula_test_helpers).
-export([start_mock_connection_manager/0, stop_mock_connection_manager/1]).

start_mock_connection_manager() ->
    MockPid = spawn(fun() -> connection_manager_loop(connected, []) end),
    register(mock_connection_manager, MockPid),
    MockPid.

connection_manager_loop(Status, Messages) ->
    receive
        {send_message, From, Type, Msg} ->
            %% Record message
            From ! {mock_ack, ok},
            connection_manager_loop(Status, [{Type, Msg} | Messages]);
        {get_status, From} ->
            From ! {status, Status},
            connection_manager_loop(Status, Messages);
        {get_messages, From} ->
            From ! {messages, lists:reverse(Messages)},
            connection_manager_loop(Status, Messages);
        {set_status, NewStatus} ->
            connection_manager_loop(NewStatus, Messages);
        stop ->
            ok
    end.

stop_mock_connection_manager(Pid) ->
    Pid ! stop.
```

### Test Coverage Goals

**Phase 1 (Current + Gaps):**
- ✅ Lifecycle tests (3 tests) - Already passing
- ✅ Connection manager PID handling (2 tests) - Already passing
- ✅ Subscribe/unsubscribe (8 tests) - Some cancelled, fix with mock
- ⏳ DHT advertisement (5 tests) - Need to add
- ⏳ DHT discovery (5 tests) - Need to add
- ⏳ QoS 1 retry logic (8 tests) - Need to add
- ⏳ Callback invocation (5 tests) - Need to add
- ⏳ Message routing (5 tests) - Need to add

**Total:** 41+ tests (current 22 → add 19)

**Phase 2-4 (After Extraction):**
- macula_pubsub_subscription_tests.erl: 15 tests
- macula_pubsub_dht_tests.erl: 18 tests
- macula_pubsub_qos_tests.erl: 15 tests
- macula_pubsub_handler_tests.erl: 20 tests (integration)

**Total:** 68 tests, 80%+ coverage

---

## Success Criteria

### After Each Phase

**Phase 1: Test Coverage**
- ✅ 40+ tests passing
- ✅ 0 cancelled tests (mocks in place)
- ✅ 80%+ code coverage

**Phase 2: QoS Module Extracted**
- ✅ macula_pubsub_qos.erl created (~150 LOC)
- ✅ 15 QoS tests passing
- ✅ Handler reduced by ~135 LOC

**Phase 3: DHT Module Extracted**
- ✅ macula_pubsub_dht.erl created (~200 LOC)
- ✅ 18 DHT tests passing
- ✅ Handler reduced by ~110 LOC

**Phase 4: Subscription Module Extracted**
- ✅ macula_pubsub_subscription.erl created (~250 LOC)
- ✅ 15 subscription tests passing
- ✅ Handler reduced by ~70 LOC

**Phase 5: Handler Simplified**
- ✅ macula_pubsub_handler.erl reduced to ~150 LOC
- ✅ Clean facade pattern (delegation only)
- ✅ No deep nesting (2-level max)
- ✅ Idiomatic Erlang (pattern matching, guards)
- ✅ All 68 tests passing

### Final Metrics

```
BEFORE:
- Lines of Code: 657 LOC (single module)
- Responsibilities: 7 mixed concerns
- Test Coverage: 22 tests, 3 cancelled
- Nesting Depth: 3-4 levels
- Code Quality: 6.0/10 (god module)

AFTER:
- Lines of Code:
  - macula_pubsub_handler.erl: 150 LOC (facade)
  - macula_pubsub_subscription.erl: 250 LOC
  - macula_pubsub_dht.erl: 200 LOC
  - macula_pubsub_qos.erl: 150 LOC
  - TOTAL: 750 LOC (4 modules, +15% for better separation)
- Responsibilities: Each module has 1 clear purpose
- Test Coverage: 68 tests, 0 cancelled, 80%+ coverage
- Nesting Depth: 2 levels max
- Code Quality: 8.5/10 (idiomatic, testable, maintainable)
```

---

## Dependencies & Timeline

### Dependencies

- ✅ Phase 1 (Tests) - No dependencies
- ⏳ Phase 2 (QoS) - Depends on Phase 1
- ⏳ Phase 3 (DHT) - Depends on Phase 1
- ⏳ Phase 4 (Subscription) - Depends on Phases 1-3
- ⏳ Phase 5 (Simplify) - Depends on all above

### Timeline (20 hours total)

```
Week 1:
  Mon: Phase 1 - Test coverage (4-6h)
  Tue: Phase 2 - Extract QoS module (4-5h)
  Wed: Phase 3 - Extract DHT module (5-6h)

Week 2:
  Thu: Phase 4 - Extract subscription module (4-5h)
  Fri: Phase 5 - Simplify handler (2-3h)
```

**Estimated Completion:** 5 days (1 developer, full-time)

---

## Risks & Mitigations

### Risk 1: Breaking Existing Functionality

**Impact:** HIGH
**Probability:** MEDIUM

**Mitigation:**
- TDD approach: Write tests first, run after each change
- Small incremental changes: Extract one module at a time
- Validate all tests pass before proceeding to next phase

### Risk 2: Performance Regression

**Impact:** MEDIUM
**Probability:** LOW

**Mitigation:**
- Module calls are local (same BEAM node) - negligible overhead
- Measure baseline performance before refactoring
- Profile after each extraction

### Risk 3: Test Mocking Complexity

**Impact:** MEDIUM
**Probability:** MEDIUM

**Mitigation:**
- Create reusable mock helpers (macula_test_helpers.erl)
- Use gen_server behavior for mocks (familiar pattern)
- Document mock usage in test files

---

## Next Steps

1. **Review this plan** with team/stakeholders
2. **Create Git branch** `refactor/pubsub-handler-tdd`
3. **Phase 1:** Add test coverage and mocks (4-6 hours)
4. **Phase 2:** Extract QoS module (4-5 hours)
5. **Phase 3:** Extract DHT module (5-6 hours)
6. **Phase 4:** Extract subscription module (4-5 hours)
7. **Phase 5:** Simplify handler (2-3 hours)
8. **Code Review** and merge to main

---

**Status:** Ready to start Phase 1
**Confidence:** HIGH (TDD approach de-risks refactoring)
**Blocker for:** Pub/sub sharding optimization (8-16x parallelization)
