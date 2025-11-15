# Code Quality Analysis: Declarative vs Imperative Patterns

## Executive Summary

**Date:** January 14, 2025
**Scope:** All src/*.erl modules (61 files), with focus on newly extracted modules
**Analysis Type:** Static code analysis for idiomatic Erlang patterns

### Key Findings

| Metric | Count | Assessment |
|--------|-------|------------|
| **Case statements** | 363 | ⚠️ High - many can be refactored |
| **If statements** | 3 | ✅ Good - minimal use |
| **Try-catch blocks** | 39 | ⚠️ Review needed - likely overused |
| **Deep nesting (>2 levels)** | ~15-20 | ⚠️ Refactor to 1-2 levels max |
| **Modules analyzed** | 61 | All Erlang source files |

### Health Score: **6.8/10**

**Strengths:**
- ✅ Minimal if statements (only 3 in entire codebase)
- ✅ Good pattern matching on function heads in many modules
- ✅ No complex andalso/orelse chains
- ✅ Clean separation in newly extracted modules

**Areas for Improvement:**
- ⚠️ **363 case statements** - many can become pattern matching
- ⚠️ **39 try-catch blocks** - likely inappropriate use for normal error handling
- ⚠️ **Deep nesting** - several functions exceed 2 levels
- ⚠️ **Mixed imperative/declarative** style - lacks consistency

---

## 1. Case Statements Analysis

### Overview

Total case statements: **363**
New modules (4): **51** (14% of total)

### Breakdown by Module Priority

| Module | Case Count | Priority | Complexity |
|--------|-----------|----------|-----------|
| **Newly Extracted (High Priority)** ||||
| macula_pubsub_handler | 17 | 🔴 HIGH | Medium |
| macula_rpc_handler | 15 | 🔴 HIGH | Medium |
| macula_connection_manager | 13 | 🔴 HIGH | Medium |
| macula_advertisement_manager | 6 | 🟡 MEDIUM | Low |
| **Other Critical Modules** ||||
| macula_gateway | ~30 | 🔴 HIGH | High |
| macula_routing_dht | 10 | 🟡 MEDIUM | Medium |
| macula_routing_bucket | 6 | 🟢 LOW | Low |
| macula_connection (facade) | 15 | 🟡 MEDIUM | Medium |

### Recommendations

**Phase 1: New Modules (2-3 weeks)**
- Focus on the 4 newly extracted modules (51 case statements)
- Set pattern for future development
- Estimated effort: 12-17 hours

**Phase 2: Gateway (1-2 weeks)**
- Refactor macula_gateway.erl (~30 case statements)
- Largest concentration of imperative patterns
- Estimated effort: 6-8 hours

**Phase 3: Routing Modules (1 week)**
- DHT and bucket modules
- Medium complexity
- Estimated effort: 5-6 hours

---

## 2. Specific Refactoring Opportunities

### 2.1 Connection Manager - Nested Case for QUIC Connection

**File:** src/macula_connection_manager.erl
**Lines:** 172-244
**Issue:** 3 levels of nesting, mixed concerns

**Current Pattern (IMPERATIVE):**
```erlang
case ConnectResult of
    {ok, Conn} ->
        case macula_quic:open_stream(Conn) of
            {ok, Stream} ->
                case quicer:setopt(Stream, active, true) of
                    ok -> finalize_connection(...);
                    {error, SetOptErr} -> handle_error(...)
                end;
            {error, Reason} -> cleanup_and_error(...)
        end;
    {error, Reason} -> error_response(...)
end
```

**Recommended Pattern (DECLARATIVE):**
```erlang
% Separate concerns into focused functions
do_connect(State) ->
    #{host := Host, port := Port} = State#state.opts,
    QuicOpts = build_quic_opts(),
    connect_and_setup(Host, Port, QuicOpts, State).

connect_and_setup(Host, Port, QuicOpts, State) ->
    case attempt_quic_connection(Host, Port, QuicOpts) of
        {ok, Conn, Stream} -> finalize_connection(Conn, Stream, State);
        {error, Reason} -> {error, Reason}
    end.

attempt_quic_connection(Host, Port, QuicOpts) ->
    with_quic_connection(Host, Port, QuicOpts,
                        fun configure_bidirectional_stream/1).

with_quic_connection(Host, Port, QuicOpts, StreamSetupFn) ->
    case macula_quic:connect(Host, Port, QuicOpts, ?DEFAULT_TIMEOUT) of
        {ok, Conn} -> StreamSetupFn(Conn);
        {error, _} = Err -> Err
    end.

configure_bidirectional_stream(Conn) ->
    case macula_quic:open_stream(Conn) of
        {ok, Stream} -> activate_stream(Conn, Stream);
        {error, Reason} -> cleanup_connection(Conn, Reason)
    end.

activate_stream(Conn, Stream) ->
    case quicer:setopt(Stream, active, true) of
        ok -> {ok, Conn, Stream};
        {error, _Reason} -> cleanup_stream_and_conn(Conn, Stream)
    end.

% Cleanup functions
cleanup_connection(Conn, Reason) ->
    macula_quic:close(Conn),
    {error, {stream_open_failed, Reason}}.

cleanup_stream_and_conn(Conn, Stream) ->
    macula_quic:close(Stream),
    macula_quic:close(Conn),
    {error, setopt_failed}.
```

**Benefits:**
- ✅ Each function has single responsibility
- ✅ Maximum nesting depth: 1 level
- ✅ Easy to test each step independently
- ✅ Clear error handling at each stage
- ✅ No deep nesting

**Effort:** 2-3 hours (including tests)

---

### 2.2 PubSub Handler - Message Routing with Case

**File:** src/macula_pubsub_handler.erl
**Lines:** Multiple locations
**Issue:** Case used for simple dispatch

**Current Pattern (IMPERATIVE):**
```erlang
handle_call({publish, Topic, Data, Opts}, _From, State) ->
    BinaryTopic = ensure_binary(Topic),
    EncodedPayload = case Data of
        D when is_binary(D) -> D;
        D when is_map(D) -> encode_json(D);
        D when is_list(D) -> list_to_binary(D);
        D -> term_to_binary(D)
    end,
    ...
```

**Recommended Pattern (DECLARATIVE):**
```erlang
% Pattern match on function heads with guards
encode_payload(Data) when is_binary(Data) -> Data;
encode_payload(Data) when is_map(Data) -> encode_json(Data);
encode_payload(Data) when is_list(Data) -> list_to_binary(Data);
encode_payload(Data) -> term_to_binary(Data).

handle_call({publish, Topic, Data, Opts}, _From, State) ->
    BinaryTopic = ensure_binary(Topic),
    EncodedPayload = encode_payload(Data),
    ...
```

**Benefits:**
- ✅ Self-documenting function name
- ✅ Each clause clear and focused
- ✅ Easy to add new encoding types
- ✅ Guards make intent explicit

**Effort:** 30 minutes per occurrence

---

### 2.3 RPC Handler - Service Discovery Cascade

**File:** src/macula_rpc_handler.erl
**Lines:** 122-154
**Issue:** Nested case for service lookup → discovery → routing

**Current Pattern (IMPERATIVE):**
```erlang
case macula_service_registry:get_local_handler(Registry, BinaryProcedure) of
    {ok, Handler} ->
        execute_locally(Handler, Args, From);
    not_found ->
        case macula_service_registry:discover_service(Registry, BinaryProcedure) of
            {ok, Providers, Registry2} when Providers =/= [] ->
                do_remote_call(...);
            {cache_miss, Registry2} ->
                send_dht_query(...);
            {ok, [], Registry2} ->
                {reply, {error, service_not_found}, State}
        end
end
```

**Recommended Pattern (DECLARATIVE):**
```erlang
% Pipeline pattern with clear stages
handle_call({call, Procedure, Args, Opts}, From, State) ->
    BinaryProcedure = ensure_binary(Procedure),
    handle_rpc_request(BinaryProcedure, Args, Opts, From, State).

handle_rpc_request(Procedure, Args, Opts, From, State) ->
    case try_local_execution(Procedure, Args, From, State) of
        {handled, State2} -> {noreply, State2};
        not_local -> try_remote_execution(Procedure, Args, Opts, From, State)
    end.

try_local_execution(Procedure, Args, From, State) ->
    Registry = State#state.service_registry,
    case macula_service_registry:get_local_handler(Registry, Procedure) of
        {ok, Handler} ->
            spawn_local_handler(Handler, Args, From),
            {handled, State};
        not_found ->
            not_local
    end.

try_remote_execution(Procedure, Args, Opts, From, State) ->
    case discover_remote_service(Procedure, State) of
        {found, Providers, State2} ->
            do_remote_call(Procedure, Args, Opts, From, Providers, State2);
        {query_dht, State2} ->
            send_dht_query(Procedure, Args, Opts, From, State2);
        {not_found, State2} ->
            {reply, {error, service_not_found}, State2}
    end.

discover_remote_service(Procedure, State) ->
    Registry = State#state.service_registry,
    case macula_service_registry:discover_service(Registry, Procedure) of
        {ok, Providers, Registry2} when Providers =/= [] ->
            {found, Providers, State#state{service_registry = Registry2}};
        {cache_miss, Registry2} ->
            {query_dht, State#state{service_registry = Registry2}};
        {ok, [], Registry2} ->
            {not_found, State#state{service_registry = Registry2}}
    end.
```

**Benefits:**
- ✅ Clear separation: local → remote → DHT
- ✅ Each function <20 lines
- ✅ Testable in isolation
- ✅ Self-documenting flow

**Effort:** 3-4 hours (including tests)

---

## 3. Try-Catch Block Analysis

### Overview

Total try-catch blocks: **39**

### Categorization

| Category | Count | Action Required |
|----------|-------|----------------|
| **Inappropriate Use** |||
| JSON encoding/decoding | ~12 | ⚠️ Refactor to tagged tuples |
| Handler execution | ~6 | ⚠️ Remove (let it crash) |
| DHT operations | ~8 | ⚠️ Refactor to tagged tuples |
| **Appropriate Use** |||
| Resource cleanup (QUIC, files) | ~8 | ✅ Keep as-is |
| NIF/Port interfacing | ~5 | ✅ Keep as-is |

### Problem Pattern #1: JSON Decoding

**Current (NOT IDIOMATIC):**
```erlang
DecodedPayload = try
    decode_json(Payload)
catch
    _:_ -> Payload  % Fallback to raw
end
```

**Why not idiomatic:**
- JSON parse errors are expected, not exceptional
- Should use pattern matching on return value
- Hiding potential bugs (what if Payload is invalid?)

**Recommended:**
```erlang
% Update decode_json to return tagged tuple
-spec decode_json(binary()) -> {ok, term()} | {error, term()}.
decode_json(Binary) when is_binary(Binary) ->
    case thoas:decode(Binary) of
        {ok, Decoded} -> {ok, Decoded};
        {error, Reason} -> {error, {json_decode, Reason}}
    end;
decode_json(NotBinary) ->
    {error, {not_binary, NotBinary}}.

% Use pattern matching
case decode_json(Payload) of
    {ok, Decoded} -> Decoded;
    {error, _Reason} -> Payload  % Explicit fallback
end
```

**Effort:** 4-6 hours to refactor all JSON operations

---

### Problem Pattern #2: Handler Execution

**Current (NOT IDIOMATIC):**
```erlang
spawn(fun() ->
    try
        Result = Handler(Args),
        gen_server:reply(From, Result)
    catch
        _:Error ->
            gen_server:reply(From, {error, Error})
    end
end)
```

**Why not idiomatic:**
- Hides broken handlers
- Prevents supervisor from detecting failures
- Handler contract unclear

**Recommended:**
```erlang
% Document handler contract
-callback rpc_handler(Args :: term()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.

% Let it crash - supervisor will restart if handler is broken
spawn(fun() ->
    Result = Handler(Args),  % Will crash if handler is bad
    gen_server:reply(From, Result)
end)

% Or if you must catch, make it explicit
spawn(fun() ->
    case safe_handler_call(Handler, Args) of
        {ok, _} = Success -> gen_server:reply(From, Success);
        {error, _} = Error -> gen_server:reply(From, Error);
        {crash, Reason} ->
            ?LOG_ERROR("Handler crashed: ~p", [Reason]),
            gen_server:reply(From, {error, handler_crashed})
    end
end).

safe_handler_call(Handler, Args) ->
    try
        Handler(Args)
    catch
        Class:Reason:Stack ->
            {crash, {Class, Reason, Stack}}
    end.
```

**Effort:** 2-3 hours to review and refactor

---

### Appropriate Try-Catch Usage

**Example: Resource Cleanup (CORRECT)**
```erlang
% From macula_quic.erl
close_connection_and_stream(Conn, Stream) ->
    try
        quicer:close_stream(Stream)
    catch
        _:_ -> ok  % Best effort
    after
        try
            quicer:close_connection(Conn)
        catch
            _:_ -> ok  % Guarantee connection cleanup
        end
    end.
```

**Why this is correct:**
- ✅ Cleaning up external resources (NIF handles)
- ✅ Must complete even if errors occur
- ✅ Using `after` clause for guaranteed cleanup
- ✅ No way to handle via pattern matching

---

## 4. If Statement Analysis

### Overview

Total if statements: **3** (excellent!)

### Location

All 3 if statements are in macula_membership_gossip.erl and macula_routing_nodeid.erl

### Example - Membership Gossip

**Current:**
```erlang
NewUpdate = if
    Incarnation > OldInc -> {Status, Incarnation, 0, Timestamp};
    Incarnation =:= OldInc andalso Status =/= OldStatus ->
        {Status, Incarnation, 0, Timestamp};
    true -> {OldStatus, OldInc, TransmitCount, OldTimestamp}
end
```

**Recommended:**
```erlang
% Pattern matching with guards
merge_update(Status, Incarnation, Timestamp, undefined) ->
    {Status, Incarnation, 0, Timestamp};

merge_update(Status, Incarnation, Timestamp, {_OldStatus, OldInc, _TC, _OldTs})
        when Incarnation > OldInc ->
    {Status, Incarnation, 0, Timestamp};

merge_update(Status, Incarnation, Timestamp, {OldStatus, OldInc, _TC, _OldTs})
        when Incarnation =:= OldInc, Status =/= OldStatus ->
    {Status, Incarnation, 0, Timestamp};

merge_update(_Status, _Incarnation, _Timestamp, OldUpdate) ->
    OldUpdate.
```

**Effort:** 1-2 hours total (only 3 instances)

---

## 5. Deep Nesting Analysis

### High-Priority Functions (>2 levels)

| File | Function | Lines | Nesting | Priority |
|------|----------|-------|---------|----------|
| macula_connection_manager.erl | do_connect/1 | 172-244 | 3 | 🔴 HIGH |
| macula_pubsub_handler.erl | handle_call({unsubscribe}) | 174-200 | 3 | 🔴 HIGH |
| macula_rpc_handler.erl | handle_call({call}) | 122-154 | 3 | 🔴 HIGH |
| macula_gateway.erl | handle_info({quic, Data}) | ~400-500 | 3-4 | 🔴 HIGH |
| macula_routing_dht.erl | handle_find_value/2 | ~200-250 | 3 | 🟡 MEDIUM |

### Strategy: Extract Helper Functions

**General Pattern:**
```erlang
% BEFORE: Nested case statements (hard to read/test)
function(Args, State) ->
    case step1(Args) of
        {ok, Result1} ->
            case step2(Result1) of
                {ok, Result2} ->
                    case step3(Result2) of
                        {ok, Result3} -> finalize(Result3, State);
                        {error, R3} -> {error, R3}
                    end;
                {error, R2} -> {error, R2}
            end;
        {error, R1} -> {error, R1}
    end.

% AFTER: Pipeline with helper functions (clear, testable)
function(Args, State) ->
    case execute_pipeline(Args) of
        {ok, FinalResult} -> finalize(FinalResult, State);
        {error, Reason} -> {error, Reason}
    end.

execute_pipeline(Args) ->
    with_result(step1(Args), fun step2/1, fun step3/1).

% Helper for chaining operations
with_result({ok, Value}, NextFn) ->
    NextFn(Value);
with_result({ok, Value}, NextFn, ThenFn) ->
    case NextFn(Value) of
        {ok, V2} -> ThenFn(V2);
        {error, _} = Err -> Err
    end;
with_result({error, _} = Err, _) ->
    Err.
```

---

## 6. Positive Patterns (Keep Doing This!)

### ✅ Excellent: Pattern Matching on Booleans

**macula_connection_manager.erl (Lines 147-152):**
```erlang
% EXCELLENT: Dispatch based on boolean pattern match
handle_stream_data(true, Data, _Stream, State) ->
    handle_received_data(Data, State);
handle_stream_data(false, _Data, Stream, State) ->
    ?LOG_WARNING("Received data from unknown stream: ~p", [Stream]),
    {noreply, State}.
```

### ✅ Excellent: Guards on Function Heads

**macula_pubsub_handler.erl:**
```erlang
% EXCELLENT: Type dispatch with guards
encode_payload(Data) when is_binary(Data) -> Data;
encode_payload(Data) when is_map(Data) -> encode_json(Data);
encode_payload(Data) when is_list(Data) -> list_to_binary(Data).
```

### ✅ Excellent: Clear Recursion

**macula_routing_bucket.erl:**
```erlang
% EXCELLENT: Tail recursion with pattern matching
find_node_in_list([], _NodeId) ->
    not_found;
find_node_in_list([Node | Rest], NodeId) ->
    case maps:get(node_id, Node) of
        NodeId -> {found, Node};
        _ -> find_node_in_list(Rest, NodeId)
    end.
```

---

## 7. Prioritized Action Plan

### Phase 1: Foundation (Week 1-2) - 15-20 hours

**Goal:** Refactor newly extracted modules to set patterns

1. **macula_connection_manager.erl** (4-5 hours)
   - Refactor do_connect/1 (reduce 3-level nesting)
   - Extract stream configuration helpers
   - Remove unnecessary case statements (3-4 instances)

2. **macula_rpc_handler.erl** (4-5 hours)
   - Refactor service discovery pipeline
   - Extract failover logic helpers
   - Simplify provider selection (2-3 case statements)

3. **macula_pubsub_handler.erl** (4-5 hours)
   - Refactor subscription management
   - Extract QoS handling
   - Simplify message routing (3-4 case statements)

4. **macula_advertisement_manager.erl** (2-3 hours)
   - Minimal refactoring (only 6 case statements)
   - Extract timer management helpers

**Deliverables:**
- ✅ All new modules follow declarative patterns
- ✅ Maximum nesting: 1-2 levels
- ✅ Each function <25 lines
- ✅ All tests passing

---

### Phase 2: Try-Catch Cleanup (Week 3) - 8-10 hours

**Goal:** Remove inappropriate error handling

1. **JSON Operations** (4-6 hours)
   - Update encode_json/decode_json to return tagged tuples
   - Update all callers to use pattern matching
   - ~15 locations to update

2. **Handler Execution** (2-3 hours)
   - Document handler contracts
   - Remove defensive try-catch where inappropriate
   - Keep only for NIF/resource cleanup

3. **DHT Operations** (2-3 hours)
   - Refactor DHT query error handling
   - Use tagged tuples instead of exceptions

**Deliverables:**
- ✅ Try-catch only for: resource cleanup, NIF/port interfacing
- ✅ Clear contracts for handlers
- ✅ Explicit error handling via pattern matching

---

### Phase 3: Gateway Module (Week 4-5) - 8-10 hours

**Goal:** Refactor largest concentration of imperative patterns

1. **macula_gateway.erl** (~30 case statements)
   - Extract message routing helpers
   - Simplify client management
   - Reduce nesting in handle_info clauses

**Deliverables:**
- ✅ Gateway module follows declarative patterns
- ✅ Clear separation of routing concerns
- ✅ Testable message handlers

---

### Phase 4: If Statement Cleanup (Week 6) - 2 hours

**Goal:** Eliminate all if statements

1. **macula_membership_gossip.erl** (1 hour)
   - Refactor update merging logic
   - Use pattern matching with guards

2. **macula_routing_nodeid.erl** (1 hour)
   - Refactor distance comparisons
   - Use guards instead of if

**Deliverables:**
- ✅ Zero if statements in codebase
- ✅ All comparisons use guards or pattern matching

---

## 8. Testing Strategy

### Before Refactoring

1. **Verify current test coverage** (already at 72% for new modules)
2. **Write characterization tests** for functions being refactored
3. **Document current behavior** via tests

### During Refactoring

1. **Refactor incrementally** (one function at a time)
2. **Run tests after each change**
3. **Commit frequently** (small, focused commits)
4. **Use type specs** to document contracts

### After Refactoring

1. **Verify tests still pass**
2. **Check test coverage** (should maintain or improve)
3. **Review for declarative style**
4. **Update documentation**

---

## 9. Code Metrics Targets

| Metric | Current | Target | Timeline |
|--------|---------|--------|----------|
| Case statements (new modules) | 51 | <25 | Phase 1 (2 weeks) |
| Case statements (total) | 363 | <200 | Phase 3 (5 weeks) |
| If statements | 3 | 0 | Phase 4 (1 week) |
| Try-catch (inappropriate) | ~30 | 0 | Phase 2 (1 week) |
| Max nesting depth | 3-4 | 1-2 | Phase 1 (2 weeks) |
| Avg function length | ~18 | <15 | Ongoing |
| Functions >50 lines | ~15 | 0 | Phase 3 (5 weeks) |

---

## 10. Success Criteria

### Quantitative

- ✅ Case statements reduced by 40% in new modules
- ✅ Zero if statements
- ✅ Try-catch only for resource cleanup and NIF/port interfacing
- ✅ Maximum nesting depth: 2 levels
- ✅ No functions >50 lines
- ✅ Test coverage maintained at >70%

### Qualitative

- ✅ Code reads like documentation
- ✅ Functions have single responsibility
- ✅ Pattern matching on function heads >90% of time
- ✅ Guards preferred over conditional logic
- ✅ Clear error handling via tagged tuples

---

## 11. Conclusion

The codebase shows good architectural separation with the recent god module refactoring, but has significant opportunities for improving code quality by embracing idiomatic Erlang patterns:

### Strengths
- ✅ Minimal if statements (only 3)
- ✅ Good use of pattern matching in many modules
- ✅ Clean module separation

### Areas for Improvement
- ⚠️ High case statement count (363)
- ⚠️ Inappropriate try-catch usage (~30 instances)
- ⚠️ Deep nesting in critical functions
- ⚠️ Mixed imperative/declarative style

### Recommended Approach

Start with **Phase 1** (newly extracted modules) to:
1. Set patterns for future development
2. Demonstrate benefits of declarative style
3. Build team muscle memory
4. Create reference implementations

**Total estimated effort:** 35-45 hours over 6 weeks

**Priority:** HIGH - These patterns will be copied by future code, so establishing good practices now has multiplier effect.

---

*Analysis performed by: Claude Code*
*Date: January 14, 2025*
*Modules analyzed: 61 Erlang source files*
