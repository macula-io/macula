# Peer-Connection Separation: TDD Implementation Plan

**Date:** 2025-11-15
**Target Version:** 0.6.0
**Approach:** Option B - Full separation with comprehensive test coverage

---

## Executive Summary

Split `macula_connection` into two distinct responsibilities:
1. **`macula_peer`** - High-level mesh participant (pub/sub, RPC, DHT)
2. **`macula_connection`** - Low-level QUIC transport

**Strategy:** Test-Driven Development with no regression

---

## Current Test Coverage

### Existing Test Files (144 tests total)

| Test File | Tests | Coverage |
|-----------|-------|----------|
| `macula_connection_error_tests.erl` | 25 | Error handling |
| `macula_connection_manager_tests.erl` | 25 | QUIC lifecycle |
| `macula_connection_rpc_tests.erl` | 25 | RPC operations |
| `macula_connection_pool_tests.erl` | 19 | Connection pooling |
| `macula_connection_sup_tests.erl` | 15 | Supervision |
| `macula_connection_protocol_tests.erl` | 14 | Protocol encoding |
| `macula_connection_tests.erl` | 11 | Facade API |
| `macula_connection_pubsub_tests.erl` | 10 | Pub/sub operations |
| `macula_connection_integration_tests.erl` | 0 | Integration (empty) |
| `macula_connection_pattern_qos_tests.erl` | 0 | QoS patterns (empty) |

**Total:** 144 test functions across 10 files

---

## Target Architecture

### macula_peer.erl - Mesh Participant API

**Responsibilities:**
- Mesh-level operations (pub/sub, RPC, service discovery)
- DHT participation and peer discovery
- Peer identity management (node ID, realm)
- Application-facing high-level API

**API:**
```erlang
%% Lifecycle
-spec start_link(PeerOpts :: map()) -> {ok, pid()} | {error, term()}.
-spec stop(Peer :: pid()) -> ok.
-spec get_info(Peer :: pid()) -> #{node_id => binary(), realm => binary(), status => atom()}.

%% Pub/Sub (mesh-level)
-spec publish(Peer :: pid(), Topic :: binary(), Data :: term()) -> ok | {error, term()}.
-spec publish(Peer :: pid(), Topic :: binary(), Data :: term(), Opts :: map()) -> ok | {error, term()}.
-spec subscribe(Peer :: pid(), Topic :: binary(), Callback :: fun()) -> {ok, reference()} | {error, term()}.
-spec unsubscribe(Peer :: pid(), SubRef :: reference()) -> ok | {error, term()}.

%% RPC (mesh-level)
-spec call(Peer :: pid(), Procedure :: binary(), Args :: term()) -> {ok, term()} | {error, term()}.
-spec call(Peer :: pid(), Procedure :: binary(), Args :: term(), Opts :: map()) -> {ok, term()} | {error, term()}.
-spec advertise(Peer :: pid(), Procedure :: binary(), Handler :: fun(), Opts :: map()) -> ok | {error, term()}.
-spec unadvertise(Peer :: pid(), Procedure :: binary()) -> ok | {error, term()}.

%% DHT/Mesh Operations
-spec discover_peers(Peer :: pid(), Realm :: binary()) -> {ok, [peer_info()]} | {error, term()}.
-spec find_service(Peer :: pid(), Service :: binary()) -> {ok, [endpoint()]} | {error, term()}.
```

**State:**
```erlang
-record(peer_state, {
    node_id :: binary(),
    realm :: binary(),
    status :: connecting | connected | disconnected,

    %% Child process PIDs
    connection_pid :: pid() | undefined,      %% macula_connection
    pubsub_handler_pid :: pid() | undefined,  %% macula_pubsub_handler
    rpc_handler_pid :: pid() | undefined,     %% macula_rpc_handler
    advertisement_manager_pid :: pid() | undefined,  %% macula_advertisement_manager

    %% Mesh-level state
    subscriptions :: #{reference() => subscription()},
    advertised_services :: #{binary() => handler()}
}).
```

---

### macula_connection.erl - QUIC Transport Layer

**Responsibilities:**
- QUIC connection establishment and lifecycle
- Stream management (send/receive)
- Message encoding/decoding
- Reconnection and error handling
- Transport-level concerns ONLY

**API:**
```erlang
%% Lifecycle
-spec start_link(ConnectionOpts :: map()) -> {ok, pid()} | {error, term()}.
-spec stop(Connection :: pid()) -> ok.
-spec get_status(Connection :: pid()) -> connecting | connected | disconnected | error.

%% Transport operations
-spec send(Connection :: pid(), Message :: binary()) -> ok | {error, term()}.
-spec send_message(Connection :: pid(), Type :: atom(), Data :: map()) -> ok | {error, term()}.

%% Stream management
-spec get_stream_info(Connection :: pid()) -> #{stream_id => integer(), bytes_sent => integer()}.

%% Callbacks (for receiving data)
-spec set_message_handler(Connection :: pid(), Handler :: pid()) -> ok.
```

**State:**
```erlang
-record(connection_state, {
    url :: binary(),
    host :: binary(),
    port :: integer(),

    %% QUIC state
    connection :: pid() | undefined,  %% quicer connection
    stream :: pid() | undefined,      %% quicer stream
    status :: connecting | connected | disconnected | error,

    %% Transport state
    recv_buffer :: binary(),
    message_handler :: pid() | undefined,  %% Who to send decoded messages to

    %% Reconnection
    reconnect_timer :: reference() | undefined,
    reconnect_attempts :: integer()
}).
```

---

## Implementation Phases

### Phase 1: Test Coverage Analysis ✅ (COMPLETED)

**Tasks:**
- [x] Identify all existing test files
- [x] Count test functions per file
- [x] Map tests to responsibilities (peer vs connection)

**Results:**
- 144 tests across 10 files
- Need to split into peer tests (pub/sub, RPC, DHT) vs connection tests (QUIC, transport)

---

### Phase 2: Design New Architecture (2 days)

**Tasks:**
1. Define `macula_peer` API and behavior specification
2. Define `macula_connection` API and behavior specification
3. Design interaction protocol between peer and connection
4. Design supervision tree structure
5. Document state management and ownership

**Deliverables:**
- `docs/macula_peer_specification.md`
- `docs/macula_connection_specification.md`
- `docs/peer_connection_interaction_protocol.md`

---

### Phase 3: Write Peer Tests (3 days)

**Test Files to Create:**

#### `test/macula_peer_tests.erl` - Core peer functionality
```erlang
%% Lifecycle tests
peer_start_link_test() -> ...
peer_stop_test() -> ...
peer_get_info_test() -> ...

%% State management
peer_tracks_node_id_test() -> ...
peer_tracks_realm_test() -> ...
peer_tracks_status_test() -> ...

%% Connection integration
peer_starts_connection_test() -> ...
peer_handles_connection_failure_test() -> ...
peer_reconnects_after_disconnect_test() -> ...
```

#### `test/macula_peer_pubsub_tests.erl` - Pub/sub operations
```erlang
%% Publishing
peer_publish_simple_test() -> ...
peer_publish_with_opts_test() -> ...
peer_publish_while_disconnected_test() -> ...
peer_publish_queues_when_connecting_test() -> ...

%% Subscribing
peer_subscribe_test() -> ...
peer_unsubscribe_test() -> ...
peer_receives_published_events_test() -> ...
peer_wildcard_subscribe_test() -> ...

%% Error handling
peer_publish_invalid_topic_test() -> ...
peer_subscribe_callback_error_test() -> ...
```

#### `test/macula_peer_rpc_tests.erl` - RPC operations
```erlang
%% Calling services
peer_call_simple_test() -> ...
peer_call_with_timeout_test() -> ...
peer_call_nonexistent_service_test() -> ...
peer_call_multiple_providers_test() -> ...

%% Advertising services
peer_advertise_test() -> ...
peer_unadvertise_test() -> ...
peer_handler_receives_call_test() -> ...
peer_handler_returns_result_test() -> ...

%% Failover
peer_call_failover_on_timeout_test() -> ...
peer_call_failover_on_error_test() -> ...
```

#### `test/macula_peer_dht_tests.erl` - DHT and discovery
```erlang
%% Peer discovery
peer_discover_peers_test() -> ...
peer_discover_peers_in_realm_test() -> ...
peer_discover_peers_empty_test() -> ...

%% Service discovery
peer_find_service_test() -> ...
peer_find_service_multiple_providers_test() -> ...
peer_find_service_not_found_test() -> ...

%% DHT operations
peer_dht_query_test() -> ...
peer_dht_store_test() -> ...
```

**Estimated Tests:** ~60-70 new tests

---

### Phase 4: Write Connection Tests (2 days)

**Test Files to Create:**

#### `test/macula_connection_tests.erl` - Core transport
```erlang
%% Lifecycle
connection_start_link_test() -> ...
connection_stop_test() -> ...
connection_get_status_test() -> ...

%% QUIC connection
connection_establishes_quic_test() -> ...
connection_creates_stream_test() -> ...
connection_handles_connection_failure_test() -> ...

%% Reconnection
connection_reconnects_on_disconnect_test() -> ...
connection_backs_off_on_repeated_failures_test() -> ...
connection_max_reconnect_attempts_test() -> ...
```

#### `test/macula_connection_send_tests.erl` - Sending messages
```erlang
%% Send operations
connection_send_binary_test() -> ...
connection_send_message_test() -> ...
connection_send_while_connecting_test() -> ...
connection_send_while_disconnected_test() -> ...

%% Encoding
connection_encodes_message_type_test() -> ...
connection_encodes_message_data_test() -> ...
connection_handles_encoding_error_test() -> ...
```

#### `test/macula_connection_receive_tests.erl` - Receiving messages
```erlang
%% Receive operations
connection_receives_data_test() -> ...
connection_decodes_messages_test() -> ...
connection_handles_partial_messages_test() -> ...
connection_forwards_to_handler_test() -> ...

%% Buffering
connection_buffers_incomplete_messages_test() -> ...
connection_clears_buffer_on_reconnect_test() -> ...
```

**Estimated Tests:** ~40-50 new tests

---

### Phase 5: Implement macula_connection (TDD) (4 days)

**Approach:**
1. Write tests first (Red)
2. Implement minimal code to pass (Green)
3. Refactor for idiomatic Erlang (Refactor)
4. Repeat

**Sub-tasks:**
- Day 1: Lifecycle and QUIC connection setup
- Day 2: Send operations and encoding
- Day 3: Receive operations and decoding
- Day 4: Reconnection logic and error handling

**Success Criteria:**
- All connection tests passing
- No direct mesh logic in connection module
- Clean transport abstraction

---

### Phase 6: Implement macula_peer (TDD) (5 days)

**Approach:** Same TDD cycle

**Sub-tasks:**
- Day 1: Lifecycle and peer identity
- Day 2: Pub/sub operations (uses macula_pubsub_handler)
- Day 3: RPC operations (uses macula_rpc_handler)
- Day 4: DHT and service discovery
- Day 5: Error handling and edge cases

**Success Criteria:**
- All peer tests passing
- Clean delegation to connection layer
- No transport logic in peer module

---

### Phase 7: Integration and Migration (3 days)

**Tasks:**
1. Update all references from old `macula_connection` to new modules
2. Create backward-compatible wrapper (optional)
3. Update supervision trees
4. Update documentation
5. Update examples and demos

**Migration Script:**
```bash
#!/bin/bash
# scripts/migrate-to-peer-connection.sh

# Rename test files
mv test/macula_connection_tests.erl test/macula_peer_facade_tests.erl
mv test/macula_connection_manager_tests.erl test/macula_connection_tests.erl

# Update references in source files
find src -name "*.erl" -exec sed -i 's/macula_connection:start_link/macula_peer:start_link/g' {} \;
find src -name "*.erl" -exec sed -i 's/macula_connection:publish/macula_peer:publish/g' {} \;
# ... (complete list of API replacements)

# Update test references
find test -name "*.erl" -exec sed -i 's/macula_connection:start_link/macula_peer:start_link/g' {} \;
```

---

### Phase 8: Testing and Validation (2 days)

**Test Levels:**

1. **Unit Tests:**
   - All macula_peer tests passing (60-70 tests)
   - All macula_connection tests passing (40-50 tests)
   - All existing tests still passing (144 tests)

2. **Integration Tests:**
   - Multi-node pub/sub test
   - Multi-node RPC test
   - Service discovery test
   - Reconnection scenario test

3. **Regression Tests:**
   - Run full eunit suite
   - Run integration tests
   - Test macula-arcade compatibility

**Success Criteria:**
- 100% of existing tests passing
- 100% of new tests passing
- No performance degradation
- Clean separation of concerns

---

## Timeline

| Phase | Duration | Status |
|-------|----------|--------|
| 1. Test Coverage Analysis | 0.5 days | ✅ COMPLETED |
| 2. Design New Architecture | 2 days | 📋 PENDING |
| 3. Write Peer Tests | 3 days | 📋 PENDING |
| 4. Write Connection Tests | 2 days | 📋 PENDING |
| 5. Implement macula_connection (TDD) | 4 days | 📋 PENDING |
| 6. Implement macula_peer (TDD) | 5 days | 📋 PENDING |
| 7. Integration and Migration | 3 days | 📋 PENDING |
| 8. Testing and Validation | 2 days | 📋 PENDING |
| **TOTAL** | **21.5 days** (~4.5 weeks) | |

---

## Risk Mitigation

### Risk 1: Breaking Existing Functionality
**Mitigation:**
- Keep all existing tests
- Run tests after each phase
- Create backward-compatible wrapper if needed

### Risk 2: State Management Complexity
**Mitigation:**
- Design state ownership upfront
- Document interaction protocol
- Use well-defined message passing

### Risk 3: Performance Degradation
**Mitigation:**
- Benchmark before and after
- Profile message passing overhead
- Optimize hot paths

### Risk 4: Timeline Overrun
**Mitigation:**
- Daily progress tracking
- Adjust scope if needed
- Pair programming for complex areas

---

## Success Metrics

### Code Quality
- [x] Single Responsibility Principle: Each module has ONE clear purpose
- [ ] Test Coverage: >90% for new modules
- [ ] Idiomatic Erlang: Pattern matching, guards, no deep nesting
- [ ] Documentation: Comprehensive specs and examples

### Functionality
- [ ] All existing features working
- [ ] Clean API separation
- [ ] No performance regression
- [ ] Backward compatibility (via wrapper)

### Architecture
- [ ] macula_peer = mesh logic only
- [ ] macula_connection = transport only
- [ ] Clear interface contract
- [ ] Easy to extend/modify

---

## Next Steps

1. **Get approval** for 4.5-week timeline
2. **Start Phase 2**: Design specifications
3. **Daily standup**: Track progress and blockers
4. **Weekly review**: Adjust timeline if needed

---

**Decision Point:** Proceed with Option B full separation?

**Estimated Completion:** 4.5 weeks from start (around early December 2025)
