# Macula Architecture Improvements

**Version:** v0.13.0
**Date:** December 1, 2025
**Status:** Actionable items for v0.14.0+

---

## Priority 1: Critical (v0.14.0)

### 1.1 Remove Ra/Raft Dependency

**Current State:** Ra dependency present but deprecated per CLAUDE.md
**Impact:** Simplifies operational model, reduces complexity
**Effort:** 1-2 weeks

**Files to Modify:**
- [ ] `rebar.config` - Remove `ra` from dependencies
- [ ] `src/macula_platform_system/macula_leader_election.erl` - DELETE
- [ ] `src/macula_platform_system/macula_leader_machine.erl` - DELETE
- [ ] `src/macula_platform_system/macula_platform_system.erl` - Remove Ra children

**Replacement Plan:**
1. Expand `macula_crdt.erl` with OR-Set implementation
2. Implement gossip-based state sync between nodes
3. Use CRDTs for service registry and peer membership
4. No leader election needed (masterless design)

**Verification:**
```bash
# After removal, ensure no Ra references remain
grep -r "ra:" src/
rebar3 compile  # Should succeed without ra
rebar3 eunit    # All tests should pass
```

---

### 1.2 Complete Test Coverage for NAT System

**Current State:** 6 test files for 10 modules (0.60:1 ratio)
**Target:** 10+ test files (1.0:1 ratio)
**Effort:** 3-5 days

**Modules Needing Tests:**
- [ ] `macula_nat_system.erl` - Supervisor tests
- [ ] `macula_nat_probe.erl` - Probe message handling tests
- [ ] `macula_connection_upgrade.erl` - More edge cases (partially covered)
- [ ] `macula_port_predictor.erl` - Port prediction accuracy tests

**Test File Locations:**
```
test/macula_nat_system/
├── macula_nat_system_tests.erl      (NEW)
├── macula_nat_probe_tests.erl       (NEW)
├── macula_port_predictor_tests.erl  (NEW - expand)
└── ...existing files...
```

---

### 1.3 Complete Test Coverage for Dist System

**Current State:** 3 test files for 5 modules (0.60:1 ratio)
**Target:** 5+ test files (1.0:1 ratio)
**Effort:** 2-3 days

**Modules Needing Tests:**
- [ ] `macula_dist_system.erl` - Supervisor tests
- [ ] `macula_cluster_strategy.erl` - libcluster strategy tests

**Test File Locations:**
```
test/macula_dist_system/
├── macula_dist_system_tests.erl      (NEW)
├── macula_cluster_strategy_tests.erl (NEW)
└── ...existing files...
```

---

## Priority 2: High (v0.14.x)

### 2.1 Add Missing Subsystem READMEs

**Current State:** 7 subsystems have READMEs, 4 do not
**Effort:** 4-6 hours

**Files to Create:**
- [ ] `src/macula_nat_system/README.md`
- [ ] `src/macula_dist_system/README.md`
- [ ] `src/macula_platform_system/README.md`
- [ ] `src/macula_bootstrap_system/README.md`

**Template:**
```markdown
# Macula [System] System

Brief description...

## Modules
| Module | Purpose |
|--------|---------|
| ... | ... |

## Usage
...

## Tests
...
```

---

### 2.2 Standardize Error Handling Patterns

**Current State:** Mixed use of `{ok, _} | {error, _}` and exceptions
**Impact:** Clearer error propagation, better debugging
**Effort:** 3-5 days

**Audit Points:**
- [ ] All exported functions return `{ok, _} | {error, _}` tuples
- [ ] No `throw` or `exit` for expected errors
- [ ] Consistent error tuple formats: `{error, atom()}` or `{error, {atom(), term()}}`

**Files to Review:**
```bash
# Find functions with non-standard error returns
grep -rn "throw\|exit\|erlang:error" src/ --include="*.erl"
```

---

### 2.3 Add Property-Based Tests (PropEr)

**Current State:** PropEr dependency present but minimal use
**Impact:** Better edge case coverage for protocol encoding/decoding
**Effort:** 1 week

**Priority Modules for Property Tests:**
- [ ] `macula_protocol_encoder.erl` - Encode/decode roundtrip
- [ ] `macula_protocol_decoder.erl` - Malformed input handling
- [ ] `macula_routing_nodeid.erl` - Distance calculations
- [ ] `macula_core_types.erl` - Type encoding/decoding

**Example Test:**
```erlang
prop_encode_decode_roundtrip() ->
    ?FORALL(Msg, publish_msg_gen(),
        begin
            Encoded = macula_protocol_encoder:encode(publish, Msg),
            {ok, Decoded, <<>>} = macula_protocol_decoder:decode(Encoded),
            Msg =:= Decoded
        end).
```

---

## Priority 3: Medium (v0.15.0+)

### 3.1 Extract macula_rpc_handler Complexity

**Current State:** 1,476 LOC - largest after gateway
**Impact:** Easier testing and maintenance
**Effort:** 1-2 weeks

**Proposed Extraction:**
```
macula_rpc_handler.erl (1,476 LOC)
    ↓ Extract to:
├── macula_rpc_handler.erl (~800 LOC) - Core gen_server
├── macula_rpc_pending.erl (~300 LOC) - Pending call tracking
├── macula_rpc_timeout.erl (~200 LOC) - Timeout management
└── macula_rpc_discovery.erl (~200 LOC) - Service discovery integration
```

**Warning:** Only proceed if module continues to grow. Current size is acceptable.

---

### 3.2 Implement Metrics Collection

**Current State:** Logging only, no metrics export
**Impact:** Production observability
**Effort:** 1-2 weeks

**Metrics to Add:**
- [ ] Connection pool hit rate (currently logged)
- [ ] DHT query latency histogram
- [ ] PubSub message throughput
- [ ] NAT punch success rate
- [ ] Bridge cache hit ratio

**Integration Options:**
1. Prometheus-compatible exporter
2. OpenTelemetry integration
3. Custom metrics gen_server

---

### 3.3 Add Benchmarking Suite

**Current State:** No systematic benchmarks
**Impact:** Performance regression detection
**Effort:** 3-5 days

**Benchmarks to Create:**
- [ ] PubSub throughput (msgs/sec)
- [ ] RPC latency (p50, p95, p99)
- [ ] DHT lookup time
- [ ] Connection establishment time
- [ ] Message encoding/decoding speed

**Location:** `bench/` directory with `rebar3_bench` integration

---

## Priority 4: Low (Future)

### 4.1 WebSocket Transport Option

**Current State:** QUIC-only transport
**Impact:** Browser compatibility for web clients
**Effort:** 2-3 weeks

**Approach:**
- Add `cowboy` websocket handler as alternative transport
- Share protocol encoder/decoder
- Gateway accepts both QUIC and WebSocket

---

### 4.2 Admin API/Console

**Current State:** No runtime introspection API
**Impact:** Operational visibility
**Effort:** 2-4 weeks

**Features:**
- Node status and health
- Active connections list
- DHT routing table inspection
- PubSub subscription listing
- RPC handler registry

---

### 4.3 Cluster Membership Visualization

**Current State:** Log-based debugging only
**Impact:** Easier troubleshooting
**Effort:** 1-2 weeks

**Output Formats:**
- ASCII topology diagram
- JSON mesh state export
- Optional web UI (macula_console integration)

---

## Completed Improvements (v0.13.0)

- [x] Fix dialyzer type specifications (24 → 5 warnings)
- [x] Add SVG diagrams for bridge system documentation
- [x] Create comprehensive code quality report
- [x] Clean separation of QUIC handle types (`reference()` not `pid()`)
- [x] Fix client_behaviour callback specifications

---

## Notes

### Dependency on External Issues

**quicer behaviour bug:**
Lines 96 and 126 of `macula_quic_conn_callback.erl` have dialyzer warnings due to quicer's `quicer_connection` behaviour using atom `cb_state` instead of type `cb_state()`. This is an upstream issue.

**mDNS optional dependency:**
Warnings about `mdns:subscribe/1` and related functions are expected - mDNS is in `_checkouts/` as an optional dependency for local discovery.

### Test Coverage Strategy

When adding tests, prioritize:
1. Integration tests for message flows
2. Unit tests for pure functions
3. Property tests for encoding/decoding
4. Edge case tests for error handling

### Code Style Reminders

Per CLAUDE.md:
- No `if` statements (use pattern matching)
- No `try...catch` (use pattern matching on results)
- Guards over `case` where possible
- Maximum 2 levels of nesting
