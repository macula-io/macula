# Macula Gateway Refactoring Plan

**Status:** Planning Phase
**Start Date:** 2025-01-17
**God Module:** `macula_gateway.erl` (1,340 LOC)

## Executive Summary

The `macula_gateway.erl` module suffers from the god module antipattern, handling 5+ distinct responsibilities in a single 1,340-line file. This refactoring will extract specialized handlers into a supervision tree, following the proven approach used for `macula_connection.erl`.

## Current Architecture Problems

### Size & Complexity
- **1,340 lines of code** in single module
- **20+ handle_info clauses** mixing different concerns
- **8+ state fields** for unrelated responsibilities
- **Singleton design** preventing horizontal scaling

### Responsibilities (Violations of SRP)

| Responsibility | Evidence | LOC Estimate |
|----------------|----------|--------------|
| **QUIC Listener** | new_conn, new_stream, shutdown, peer_needs_streams | ~200 |
| **Client Management** | client_connected, DOWN monitoring, clients map | ~300 |
| **Pub/Sub Routing** | publish, subscribe, unsubscribe, subscriptions map | ~350 |
| **RPC Registration** | register_handler, unregister_handler, call, registrations map | ~250 |
| **Stats Collection** | get_stats, metrics tracking | ~100 |
| **Mesh Connections** | mesh_connections map, DHT queries | ~140 |

### Specific Code Smells

```erlang
-record(state, {
    port,                        % QUIC listener concern
    realm,                       % Shared
    node_id,                     % Shared
    listener,                    % QUIC listener concern
    clients,                     % Client management concern
    subscriptions,               % Pub/Sub concern
    stream_subscriptions,        % Pub/Sub concern
    registrations,               % RPC concern
    mesh_connections,            % Mesh concern
    client_streams               % Client management + Mesh concern
}).
```

All concerns mixed in one state record!

## Proposed Architecture

### Module Split

```
macula_gateway (Facade - ~300 LOC)
    │
    ├─> macula_gateway_sup (Supervisor - ~120 LOC)
    │       │
    │       ├─> macula_gateway_listener (QUIC Lifecycle - ~250 LOC)
    │       │     - new_conn, shutdown, transport_shutdown
    │       │     - QUIC listener startup/shutdown
    │       │
    │       ├─> macula_gateway_client_manager (Client Tracking - ~350 LOC)
    │       │     - client_connected, DOWN monitoring
    │       │     - Client lifecycle and health
    │       │     - Stream management per client
    │       │
    │       ├─> macula_gateway_pubsub (Pub/Sub Routing - ~400 LOC)
    │       │     - publish, subscribe, unsubscribe
    │       │     - Topic-based message routing
    │       │     - Subscription registry
    │       │
    │       ├─> macula_gateway_rpc (RPC Handler Registry - ~280 LOC)
    │       │     - register_handler, unregister_handler
    │       │     - RPC call routing
    │       │     - Procedure registry
    │       │
    │       └─> macula_gateway_stats (Statistics - ~150 LOC)
    │             - Metrics collection
    │             - Health monitoring
    │             - Performance tracking
```

### Supervision Strategy

```erlang
% one_for_all strategy
% If listener dies, all handlers restart with clean state
% Prevents stale client/subscription data after network failure

Strategy: one_for_all
Restart: {10, 60} % Max 10 restarts in 60 seconds

Children (in order):
1. macula_gateway_listener    (worker)
2. macula_gateway_client_manager (worker)
3. macula_gateway_pubsub      (worker)
4. macula_gateway_rpc          (worker)
5. macula_gateway_stats        (worker)
```

## Refactoring Phases

### Phase 1: Document & Test Baseline (Week 1 - 3 days)

**Goal:** Understand existing behavior and create safety net

**Tasks:**
1. Document all `handle_*` clause behaviors
2. Map state transitions for each concern
3. Identify existing test coverage (if any)
4. Create baseline integration tests
5. Document expected behavior for extraction

**Deliverables:**
- `gateway_behavior_catalog.md` - Complete behavior documentation
- `test/macula_gateway_SUITE.erl` - Baseline integration tests
- Test coverage report

### Phase 2: Extract Client Manager (Week 1-2 - 4 days)

**Goal:** Extract client lifecycle management with TDD

**TDD Approach:**
1. Write failing tests for client manager behavior
2. Create `macula_gateway_client_manager.erl` with minimal implementation
3. Make tests pass incrementally
4. Refactor for idiomatic Erlang
5. Verify original tests still pass

**API Design:**
```erlang
-module(macula_gateway_client_manager).

%% API
-export([
    start_link/1,
    client_connected/3,      % (Pid, ClientPid, ClientInfo)
    client_disconnected/2,   % (Pid, ClientPid)
    get_client_info/2,       % (Pid, ClientPid) -> {ok, Info} | not_found
    get_all_clients/1,       % (Pid) -> {ok, Clients}
    is_client_alive/2        % (Pid, ClientPid) -> boolean()
]).

-record(state, {
    clients :: #{pid() => client_info()},
    client_streams :: #{binary() => pid()},
    opts :: map()
}).
```

**Tests:**
```erlang
% test/macula_gateway_client_manager_tests.erl
-module(macula_gateway_client_manager_tests).
-include_lib("eunit/include/eunit.hrl").

client_connected_stores_info_test() ->
    {ok, Pid} = macula_gateway_client_manager:start_link(#{}),
    ClientPid = spawn(fun() -> ok end),
    ClientInfo = #{realm => <<"test">>, node_id => <<"node1">>},

    ok = macula_gateway_client_manager:client_connected(Pid, ClientPid, ClientInfo),

    {ok, Info} = macula_gateway_client_manager:get_client_info(Pid, ClientPid),
    ?assertEqual(ClientInfo, Info).

client_disconnected_removes_info_test() ->
    {ok, Pid} = macula_gateway_client_manager:start_link(#{}),
    ClientPid = spawn(fun() -> ok end),
    ClientInfo = #{realm => <<"test">>, node_id => <<"node1">>},

    macula_gateway_client_manager:client_connected(Pid, ClientPid, ClientInfo),
    macula_gateway_client_manager:client_disconnected(Pid, ClientPid),

    ?assertEqual(not_found, macula_gateway_client_manager:get_client_info(Pid, ClientPid)).

monitors_client_process_test() ->
    {ok, Pid} = macula_gateway_client_manager:start_link(#{}),
    ClientPid = spawn(fun() -> receive stop -> ok end end),
    ClientInfo = #{realm => <<"test">>, node_id => <<"node1">>},

    macula_gateway_client_manager:client_connected(Pid, ClientPid, ClientInfo),

    % Kill client - should auto-remove
    exit(ClientPid, kill),
    timer:sleep(100),

    ?assertEqual(not_found, macula_gateway_client_manager:get_client_info(Pid, ClientPid)).
```

### Phase 3: Extract Pub/Sub Handler (Week 2 - 4 days)

**Goal:** Extract pub/sub routing with comprehensive tests

**TDD Approach:**
1. Write tests for topic subscription/unsubscription
2. Write tests for message routing to subscribers
3. Implement `macula_gateway_pubsub.erl`
4. Test edge cases (wildcard topics, multiple subscribers)

**API Design:**
```erlang
-module(macula_gateway_pubsub).

-export([
    start_link/1,
    subscribe/3,          % (Pid, ClientPid, Topic)
    unsubscribe/3,        % (Pid, ClientPid, Topic)
    publish/3,            % (Pid, Topic, Payload)
    get_subscribers/2,    % (Pid, Topic) -> {ok, [ClientPid]}
    get_client_topics/2   % (Pid, ClientPid) -> {ok, [Topic]}
]).

-record(state, {
    subscriptions :: #{binary() => [pid()]},      % topic => [subscribers]
    stream_subscriptions :: #{pid() => [binary()]}, % client => [topics]
    opts :: map()
}).
```

**Key Tests:**
- Subscribe adds client to topic
- Unsubscribe removes client from topic
- Publish routes to all subscribers
- Client death auto-unsubscribes from all topics
- Wildcard topic matching (if needed)

### Phase 4: Extract RPC Handler Registry (Week 3 - 3 days)

**Goal:** Extract RPC registration with tests

**API Design:**
```erlang
-module(macula_gateway_rpc).

-export([
    start_link/1,
    register_handler/3,   % (Pid, Procedure, HandlerPid)
    unregister_handler/2, % (Pid, Procedure)
    route_call/4,         % (Pid, Procedure, Args, FromPid)
    get_handler/2         % (Pid, Procedure) -> {ok, Pid} | not_found
]).

-record(state, {
    registrations :: #{binary() => pid()},  % procedure => handler_pid
    opts :: map()
}).
```

**Tests:**
- Register handler makes procedure available
- Unregister removes handler
- Route call forwards to correct handler
- Handler death auto-unregisters

### Phase 5: Extract QUIC Listener (Week 3-4 - 3 days)

**Goal:** Extract QUIC listener lifecycle management

**API Design:**
```erlang
-module(macula_gateway_listener).

-export([
    start_link/1,
    get_listener_pid/1,
    handle_new_conn/2,
    handle_new_stream/3,
    handle_shutdown/3
]).

-record(state, {
    port :: inet:port_number(),
    listener :: pid() | undefined,
    opts :: map()
}).
```

**Tests:**
- Listener starts on configured port
- Accepts new connections
- Creates streams for clients
- Handles graceful shutdown

### Phase 6: Create Supervision Tree (Week 4 - 3 days)

**Goal:** Wire all handlers into supervision tree

**Tasks:**
1. Create `macula_gateway_sup.erl`
2. Update `macula_gateway.erl` to start supervision tree in `init/1`
3. Delegate API calls to child handlers
4. Update state to store child PIDs
5. Integration testing

**Supervision Tree:**
```erlang
-module(macula_gateway_sup).
-behaviour(supervisor).

init([Port, Opts]) ->
    HandlerOpts = Opts#{port => Port},

    Children = [
        #{id => listener,
          start => {macula_gateway_listener, start_link, [HandlerOpts]},
          type => worker},
        #{id => client_manager,
          start => {macula_gateway_client_manager, start_link, [HandlerOpts]},
          type => worker},
        #{id => pubsub,
          start => {macula_gateway_pubsub, start_link, [HandlerOpts]},
          type => worker},
        #{id => rpc,
          start => {macula_gateway_rpc, start_link, [HandlerOpts]},
          type => worker},
        #{id => stats,
          start => {macula_gateway_stats, start_link, [HandlerOpts]},
          type => worker}
    ],

    {ok, {{one_for_all, 10, 60}, Children}}.
```

### Phase 7: Cleanup & Documentation (Week 4 - 2 days)

**Goal:** Remove legacy code and update documentation

**Tasks:**
1. Remove unused helper functions from `macula_gateway.erl`
2. Add comprehensive module documentation
3. Update architectural diagrams
4. Write migration guide
5. Performance benchmarking

## Testing Strategy

### Test Pyramid

```
           E2E Tests (5)
         ─────────────
        Integration (15)
      ───────────────────
     Unit Tests (50+)
   ─────────────────────────
```

### Unit Tests (Per Module)
- **Client Manager:** 12 tests
- **Pub/Sub:** 15 tests
- **RPC:** 10 tests
- **Listener:** 8 tests
- **Stats:** 5 tests

### Integration Tests
- Full supervision tree startup
- Client connect → subscribe → publish → receive flow
- RPC registration → call → response flow
- Client disconnect cleanup
- Handler restart recovery

### E2E Tests
- Real QUIC connections
- Multi-client pub/sub
- RPC across mesh
- Stress testing (1000+ clients)

## Idiomatic Erlang Guidelines

Following guidelines from `CLAUDE.md`:

### Pattern Matching over Conditionals
```erlang
% ✅ Good
handle_subscribe(ClientPid, Topic, #state{subscriptions = Subs} = State)
    when is_pid(ClientPid), is_binary(Topic) ->
    case maps:get(Topic, Subs, []) of
        [] -> add_new_topic(Topic, ClientPid, State);
        Subscribers -> add_to_existing(ClientPid, Subscribers, Topic, State)
    end.

% ❌ Bad
handle_subscribe(ClientPid, Topic, State) ->
    if
        is_pid(ClientPid) andalso is_binary(Topic) ->
            case maps:get(topic, State#state.subscriptions, []) of
                [] -> ...
```

### Multiple Function Clauses
```erlang
% ✅ Good - separate clauses for each case
route_message(Topic, Payload, #state{subscriptions = Subs}) when is_map_key(Topic, Subs) ->
    Subscribers = maps:get(Topic, Subs),
    [Pid ! {publish, Topic, Payload} || Pid <- Subscribers],
    ok;
route_message(_Topic, _Payload, _State) ->
    {error, no_subscribers}.

% ❌ Bad - nested case
route_message(Topic, Payload, State) ->
    case maps:is_key(Topic, State#state.subscriptions) of
        true ->
            ...
```

### Avoid Deep Nesting
- Maximum 1-2 levels of nesting
- Extract helper functions instead
- Use early returns with pattern matching

## Success Criteria

### Metrics

| Metric | Current | Target | Improvement |
|--------|---------|--------|-------------|
| **Largest module** | 1,340 LOC | <400 LOC | 70% reduction |
| **Max function length** | ~80 lines | <30 lines | More focused |
| **Cyclomatic complexity** | High | Low | Testable units |
| **Test coverage** | Unknown (~0%) | >80% | Confidence |
| **Concurrent clients** | 1 gateway | N gateways | Horizontal scale |
| **Throughput** | ~30k msgs/sec | ~150k msgs/sec | 5x improvement |

### Architectural Quality

- ✅ Each module has single responsibility
- ✅ Supervision tree provides fault tolerance
- ✅ No singleton constraints
- ✅ Testable in isolation
- ✅ Idiomatic Erlang throughout
- ✅ Clear API boundaries

## Timeline

**Total Duration:** 4 weeks (20 working days)

| Week | Phase | Days | Deliverables |
|------|-------|------|--------------|
| **Week 1** | Phase 1: Documentation + Phase 2: Client Manager | 7 | Behavior docs + Client manager with tests |
| **Week 2** | Phase 3: Pub/Sub Handler | 5 | Pub/sub module with comprehensive tests |
| **Week 3** | Phase 4: RPC + Phase 5: Listener | 6 | RPC and Listener modules with tests |
| **Week 4** | Phase 6: Supervision + Phase 7: Cleanup | 5 | Integrated supervision tree + docs |

## Risk Mitigation

### Risks

1. **Breaking existing functionality** during extraction
   - Mitigation: Comprehensive test suite before refactoring

2. **Performance regression** in message routing
   - Mitigation: Benchmark before/after each phase

3. **Complex inter-module communication** patterns
   - Mitigation: Clear API contracts, documented message flows

4. **State synchronization** between modules
   - Mitigation: Supervision tree handles coordinated restart

## References

- `architecture/god_module_refactoring_plan.md` - Connection refactoring (completed)
- `architecture/god_module_refactoring_status.md` - Lessons learned
- `CLAUDE.md` - Idiomatic Erlang guidelines
- `CODE_REVIEW_REPORT.md` - Original code quality analysis

## Appendix: State Field Mapping

| Current State Field | New Module | Purpose |
|---------------------|------------|---------|
| `port` | gateway_listener | QUIC port |
| `realm` | gateway (shared) | Multi-tenancy |
| `node_id` | gateway (shared) | Node identity |
| `listener` | gateway_listener | Listener PID |
| `clients` | gateway_client_manager | Client registry |
| `subscriptions` | gateway_pubsub | Topic → Subscribers |
| `stream_subscriptions` | gateway_pubsub | Client → Topics |
| `registrations` | gateway_rpc | Procedure → Handler |
| `mesh_connections` | gateway_mesh (future) | Mesh network |
| `client_streams` | gateway_client_manager | Client → Stream |
