# Macula Testing Guide

Comprehensive testing strategy for Macula HTTP/3 Mesh with TDD workflow.

---

## Testing Philosophy

**Goal**: High test coverage (>80%) with fast, reliable tests enabling confident refactoring.

**Approach**: Test-Driven Development (TDD) - Write tests first, then implementation.

**Test Pyramid**:
```
         /\
        /  \    Property-based tests (PropEr)
       /____\
      /      \  Integration tests (Common Test)
     /        \
    /__________\ Unit tests (EUnit)
```

---

## Test Frameworks

### 1. EUnit - Unit Tests

**When**: Testing pure functions, modules, isolated logic
**Speed**: Very fast (<1ms per test)
**Coverage**: 70% of tests should be EUnit

**Example**: See `apps/macula_core/test/macula_core_example_test.erl`

**Commands**:
```bash
# Run all EUnit tests
rebar3 eunit

# Run specific app tests
rebar3 eunit --app macula_core

# Run specific module tests
rebar3 eunit --module macula_core_types_test

# With coverage
rebar3 do eunit, cover
```

### 2. Common Test (CT) - Integration Tests

**When**: Testing OTP applications, supervision trees, multi-process interactions
**Speed**: Slower (~10-100ms per test)
**Coverage**: 25% of tests should be CT

**Example**: See `apps/macula_quic/test/macula_quic_SUITE.erl`

**Commands**:
```bash
# Run all CT suites
rebar3 ct

# Run specific suite
rebar3 ct --suite apps/macula_quic/test/macula_quic_SUITE

# With coverage
rebar3 do ct, cover
```

### 3. PropEr - Property-Based Tests

**When**: Testing invariants, edge cases, protocol correctness
**Speed**: Variable (runs many test cases)
**Coverage**: 5% of tests should be PropEr

**Example**: See `apps/macula_protocol/test/macula_protocol_prop.erl`

**Commands**:
```bash
# Run property tests
rebar3 proper

# Verbose output
rebar3 proper -v

# More test cases
rebar3 proper -n 1000
```

---

## Test Organization

### Directory Structure

```
apps/macula_core/
├── src/
│   ├── macula_core.erl
│   └── macula_core_types.erl
└── test/
    ├── macula_core_test.erl          # EUnit tests for macula_core
    ├── macula_core_types_test.erl    # EUnit tests for types module
    └── macula_core_SUITE.erl         # CT suite for integration
```

**Convention**: Mirror source structure in test directory.

### Naming Conventions

- **EUnit**: `{module}_test.erl` (e.g., `macula_core_types_test.erl`)
- **CT**: `{app_name}_SUITE.erl` (e.g., `macula_quic_SUITE.erl`)
- **PropEr**: `{module}_prop.erl` (e.g., `macula_protocol_prop.erl`)

---

## TDD Workflow

### Red-Green-Refactor Cycle

1. **Red** - Write failing test
2. **Green** - Write minimal code to pass
3. **Refactor** - Improve code while keeping tests green

### Example TDD Session

**Scenario**: Implementing `macula_core_types:node_id/0` to generate unique node IDs.

#### Step 1: Write Test (Red)

```erlang
%% apps/macula_core/test/macula_core_types_test.erl
-module(macula_core_types_test).
-include_lib("eunit/include/eunit.hrl").

%% Test: node_id should return 32-byte binary
node_id_returns_binary_test() ->
    NodeId = macula_core_types:node_id(),
    ?assert(is_binary(NodeId)),
    ?assertEqual(32, byte_size(NodeId)).

%% Test: node_id should be unique
node_id_is_unique_test() ->
    Id1 = macula_core_types:node_id(),
    Id2 = macula_core_types:node_id(),
    ?assertNotEqual(Id1, Id2).
```

Run test (should fail):
```bash
$ rebar3 eunit --app macula_core
# Error: undefined function macula_core_types:node_id/0
```

#### Step 2: Implement (Green)

```erlang
%% apps/macula_core/src/macula_core_types.erl
-module(macula_core_types).
-export([node_id/0]).

-spec node_id() -> binary().
node_id() ->
    crypto:strong_rand_bytes(32).
```

Run test (should pass):
```bash
$ rebar3 eunit --app macula_core
# All tests passed
```

#### Step 3: Refactor

Add property-based test for extra confidence:

```erlang
%% apps/macula_core/test/macula_core_types_prop.erl
-module(macula_core_types_prop).
-include_lib("proper/include/proper.hrl").

prop_node_id_always_32_bytes() ->
    ?FORALL(
        _Iteration,
        integer(1, 100),
        begin
            NodeId = macula_core_types:node_id(),
            is_binary(NodeId) andalso byte_size(NodeId) =:= 32
        end
    ).
```

Run property test:
```bash
$ rebar3 proper
# 100 tests passed
```

---

## Test Patterns

### Pattern 1: Pure Function Testing (EUnit)

**What**: Functions without side effects

```erlang
-module(macula_core_util_test).
-include_lib("eunit/include/eunit.hrl").

%% Test pure function
hex_encode_test() ->
    Binary = <<1, 2, 3, 255>>,
    Expected = <<"010203ff">>,
    ?assertEqual(Expected, macula_core_util:hex_encode(Binary)).

%% Test edge case: empty binary
hex_encode_empty_test() ->
    ?assertEqual(<<>>, macula_core_util:hex_encode(<<>>)).
```

### Pattern 2: GenServer Testing (EUnit + Mocking)

**What**: Testing GenServer behavior without starting application

```erlang
-module(macula_quic_connection_test).
-include_lib("eunit/include/eunit.hrl").

connection_lifecycle_test_() ->
    {setup,
     fun() ->
         meck:new(quicer, [passthrough]),
         meck:expect(quicer, connect, fun(_, _, _, _) -> {ok, mock_conn} end),
         ok
     end,
     fun(_) ->
         meck:unload(quicer)
     end,
     [
         {"connection starts successfully",
          fun() ->
              {ok, Pid} = macula_quic_connection:start_link(#{}),
              ?assert(is_pid(Pid)),
              gen_server:stop(Pid)
          end
         },
         {"connection handles connect event",
          fun() ->
              {ok, Pid} = macula_quic_connection:start_link(#{}),
              ok = gen_server:cast(Pid, {connect, "127.0.0.1", 4433}),
              timer:sleep(10),
              State = sys:get_state(Pid),
              ?assertMatch(#{connection := mock_conn}, State),
              gen_server:stop(Pid)
          end
         }
     ]
    }.
```

### Pattern 3: Integration Testing (Common Test)

**What**: Testing full application with supervision tree

```erlang
-module(macula_quic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_listen_and_accept/1]).

all() -> [test_listen_and_accept].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(macula_quic),
    Config.

end_per_suite(_Config) ->
    application:stop(macula_quic),
    ok.

test_listen_and_accept(_Config) ->
    %% Start listener
    {ok, Listener} = macula_quic:listen(4433, []),

    %% Connect from client
    {ok, Conn} = macula_quic:connect("127.0.0.1", 4433, []),

    %% Verify connection
    {ok, AcceptedConn} = macula_quic:accept(Listener, 1000),
    true = is_pid(AcceptedConn),

    %% Cleanup
    ok = macula_quic:close(Conn),
    ok = macula_quic:close(Listener).
```

### Pattern 4: Property-Based Testing (PropEr)

**What**: Testing invariants and protocol properties

```erlang
-module(macula_protocol_prop).
-include_lib("proper/include/proper.hrl").

%% Generator: valid QUIC packets
quic_packet() ->
    ?LET({Type, Payload},
         {oneof([handshake, data, ack]), binary()},
         macula_protocol:encode_packet(Type, Payload)).

%% Property: encode-decode round-trip
prop_packet_roundtrip() ->
    ?FORALL(
        Packet,
        quic_packet(),
        begin
            {ok, Decoded} = macula_protocol:decode_packet(Packet),
            {ok, Reencoded} = macula_protocol:encode_packet(Decoded),
            Packet =:= Reencoded
        end
    ).

%% Property: protocol message size limits
prop_message_size_limit() ->
    ?FORALL(
        Msg,
        macula_protocol:message(),
        begin
            Encoded = macula_protocol:encode(Msg),
            byte_size(Encoded) =< 65535  % Max QUIC datagram size
        end
    ).
```

---

## Coverage Goals

### Per Library

| Library | Target Coverage | Priority |
|---------|-----------------|----------|
| macula_core | 90% | P0 |
| macula_protocol | 95% | P0 |
| macula_quic | 85% | P0 |
| macula_membership | 85% | P1 |
| macula_routing | 85% | P1 |
| macula_topology | 80% | P1 |
| macula_pubsub | 85% | P1 |
| macula_rpc | 85% | P1 |
| macula_discovery | 75% | P2 |
| macula_security | 90% | P0 |
| macula_gateway | 75% | P2 |

### Measuring Coverage

```bash
# Generate coverage report for all tests
rebar3 do eunit, ct, cover

# View HTML coverage report
open _build/test/cover/index.html

# Coverage summary
rebar3 cover -v
```

### CI/CD Integration

Enforce coverage in CI:

```yaml
# .github/workflows/test.yml
- name: Run tests with coverage
  run: rebar3 do eunit, ct, cover

- name: Check coverage threshold
  run: |
    COVERAGE=$(rebar3 cover -v | grep 'Total' | awk '{print $2}' | sed 's/%//')
    if [ "$COVERAGE" -lt "80" ]; then
      echo "Coverage $COVERAGE% is below threshold 80%"
      exit 1
    fi
```

---

## Quick Reference

### Run All Tests

```bash
# Everything (EUnit + CT + coverage)
rebar3 do eunit, ct, cover

# Just unit tests (fastest)
rebar3 eunit

# Just integration tests
rebar3 ct

# Property-based tests
rebar3 proper
```

### TDD Commands (Fast Feedback Loop)

```bash
# Watch mode (requires entr or similar)
find apps/macula_core -name "*.erl" | entr -c rebar3 eunit --app macula_core

# Single module test
rebar3 eunit --module macula_core_types_test

# Verbose output for debugging
rebar3 eunit -v
```

### Coverage Commands

```bash
# Generate coverage
rebar3 cover

# Verbose coverage output
rebar3 cover -v

# Reset coverage data
rebar3 cover -r
```

---

## Best Practices

### ✅ DO

1. **Write tests first** (TDD red-green-refactor)
2. **Test one thing per test** (focused, descriptive test names)
3. **Use descriptive assertions**: `?assertEqual(Expected, Actual)`
4. **Test edge cases**: empty inputs, max values, invalid data
5. **Mock external dependencies** (use meck for external APIs)
6. **Keep tests fast** (<1s for unit tests, <10s for integration)
7. **Use property tests for invariants** (encode/decode, bounds checking)
8. **Test both success and error paths**

### ❌ DON'T

1. **Don't test implementation details** (test behavior, not internals)
2. **Don't use sleep() for timing** (use proper synchronization)
3. **Don't share state between tests** (each test independent)
4. **Don't skip cleanup** (always stop processes, close sockets)
5. **Don't test library code** (trust OTP, quicer, etc.)
6. **Don't ignore failing tests** (fix or remove)

---

## Example Test Suite (Complete)

See `apps/macula_core/test/` for reference implementation:

- `macula_core_test.erl` - Application startup tests
- `macula_core_types_test.erl` - Type encoding/decoding
- `macula_core_types_prop.erl` - Type invariants
- `macula_core_SUITE.erl` - Integration with supervision tree

---

## Resources

- [EUnit User Guide](https://www.erlang.org/doc/apps/eunit/chapter.html)
- [Common Test User Guide](https://www.erlang.org/doc/apps/common_test/users_guide.html)
- [PropEr Documentation](https://proper-testing.github.io/)
- [Meck (Mocking Library)](https://github.com/eproxus/meck)

---

---

## Current Coverage Status (2025-11-12)

### Baseline Metrics

**Total Coverage**: 1% (CRITICAL - Target: 60%+ before refactoring)

| Module | Coverage | Status |
|--------|----------|--------|
| macula_quic | 20% | ✓ Some tests |
| macula_connection | 5% | ⚠️ God module - needs comprehensive tests |
| macula_service_registry | 2% | ⚠️ Minimal |
| **All other modules** | 0% | ❌ Untested |

**Total Modules**: 50
**Tested Modules**: 6 (12%)
**Untested Modules**: 44 (88%)

### Quick Start for Coverage Reporting

```bash
# Run tests with coverage
rebar3 eunit --cover

# Generate detailed coverage report
rebar3 cover --verbose

# Use the coverage helper script
./scripts/run_coverage.sh

# View HTML reports
firefox _build/test/cover/index.html
```

### Coverage Configuration

Coverage settings are configured in `rebar.config`:
```erlang
{cover_enabled, true}.
{cover_opts, [verbose]}.
{cover_print_enabled, true}.
{cover_export_enabled, true}.
```

Reports are generated at:
- Summary: `_build/test/cover/index.html`
- Per-module: `_build/test/cover/aggregate/[module_name].html`
- Raw data: `_build/test/cover/eunit.coverdata`

### Priority Testing Roadmap

See `CODE_REVIEW_REPORT.md` for comprehensive testing strategy.

**Phase 1: Core Infrastructure (Weeks 1-2)**
- macula_gateway.erl (795 LOC)
- macula_routing_dht.erl (306 LOC)
- macula_routing_server.erl (334 LOC)
- macula_rpc_server.erl (339 LOC)
- macula_pubsub_server.erl

**Phase 2: Protocol & Transport (Weeks 3-4)**
- macula_protocol_encoder.erl
- macula_protocol_decoder.erl
- macula_quic.erl
- macula_routing_table.erl
- macula_routing_bucket.erl

**Phase 3: God Module (After 60% coverage)**
- macula_connection.erl (1,869 LOC - requires comprehensive tests before refactoring)

**CRITICAL**: Must reach 60%+ test coverage before ANY refactoring begins.

---

**Last Updated**: 2025-11-12
**Status**: Living document - update as testing patterns evolve
