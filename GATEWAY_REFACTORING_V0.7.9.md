# Gateway Refactoring Progress - v0.7.9

## Phase 1: PubSub Router Extraction ✅ COMPLETED

### What Was Done
- Created `macula_gateway_pubsub_router.erl` (160 LOC)
- Extracted 7 helper functions for DHT-routed pub/sub message distribution
- Updated `macula_gateway.erl` to delegate to new module

### Results
- **Before**: 1,115 LOC in gateway
- **After**: 1,011 LOC in gateway
- **Reduction**: 104 lines (9.3%)
- **Tests**: All 46 gateway tests passing ✅
- **Compilation**: Successful ✅

### Files Modified
- `/home/rl/work/github.com/macula-io/macula/src/macula_gateway.erl` (1,011 LOC)
- `/home/rl/work/github.com/macula-io/macula/src/macula_gateway_pubsub_router.erl` (160 LOC) - **NEW**

### API
```erlang
%% macula_gateway_pubsub_router.erl
-export([distribute/4]).

distribute(LocalSubscribers, PubMsg, LocalNodeId, Mesh) -> ok.
```

### Functions Extracted
1. `deliver_to_local_subscribers/2` - Deliver to local QUIC streams
2. `send_to_local_stream/2` - Send to single stream
3. `route_to_remote_subscribers/4` - Query DHT and route remotely
4. `route_to_each_subscriber/5` - Recursively route to each subscriber
5. `route_to_single_subscriber/5` - Route to single remote subscriber
6. `send_via_dht/3` - Get mesh connection and send
7. `send_route_message/3` - Encode and send pubsub_route message

---

## Phase 2: QUIC Transport Layer Extraction 📋 DESIGNED (NOT YET IMPLEMENTED)

### Design Complete
- **Strategy**: Stateless callback module (not gen_server)
- **Target Reduction**: ~300 LOC from gateway
- **Estimated Time**: 2-3 hours for full implementation + testing

### What Will Be Extracted

#### QUIC Event Handlers (6 functions, ~250 LOC)
1. `handle_info({quic, new_stream, ...})` - Line 362
2. `handle_info({quic, Data, Stream, ...})` - Line 380 (includes message decoding)
3. `handle_info({quic, peer_needs_streams, ...})` - Line 426
4. `handle_info({quic, new_conn, ...})` - Line 432
5. `handle_info({quic, shutdown, ...})` - Line 446
6. `handle_info({quic, transport_shutdown, ...})` - Line 451

#### QUIC Helper Functions (5 functions, ~80 LOC)
1. `complete_handshake/1` - TLS handshake (lines 934-943)
2. `accept_streams/1` - Accept streams on connection (lines 947-956)
3. `register_next_connection/1` - Register for next connection (lines 960-968)
4. `parse_endpoint/1` - Parse endpoint URL (lines 978-990)
5. `resolve_host/2` - DNS resolution (lines 996-1004)

### Proposed Module Structure

```erlang
-module(macula_gateway_quic).

%% QUIC Event Callbacks
-export([
    handle_new_stream/2,
    handle_data/4,  % Note: needs message handler callback
    handle_peer_needs_streams/2,
    handle_new_conn/3,
    handle_shutdown/3,
    handle_transport_shutdown/3
]).

%% Helper Functions
-export([
    complete_handshake/1,
    accept_streams/1,
    register_next_connection/1,
    parse_endpoint/1,
    resolve_host/2
]).
```

### Implementation Approach

**Step 1**: Extract helper functions (LOW RISK)
- Move 5 helper functions to `macula_gateway_quic.erl`
- Update gateway to call `macula_gateway_quic:parse_endpoint/1`, etc.
- Test compilation and tests

**Step 2**: Extract simple event handlers (MEDIUM RISK)
- Move handlers for: new_conn, shutdown, transport_shutdown, peer_needs_streams
- Gateway delegates via callback pattern
- Test after each handler

**Step 3**: Extract complex event handlers (HIGH RISK)
- Move `new_stream` handler
- Move `Data` handler (requires message decoding callback)
- Gateway passes message handler function to QUIC module
- Thorough testing

### Challenges

1. **Message Decoding Callback**: `handle_info({quic, Data, ...})` calls `handle_decoded_message/3` which must stay in gateway. Solution: Pass callback function.

2. **State Sharing**: Event handlers need access to gateway state. Solution: Pass only required state fields, not entire `#state{}` record.

3. **Testing**: QUIC events arrive at gateway's process mailbox, can't redirect. Solution: Keep delegation thin, test callback functions in isolation.

### Expected Results After Phase 2

- **Gateway**: ~700 LOC (down from 1,115)
- **Total Reduction**: ~415 LOC (37%)
- **New Module**: `macula_gateway_quic.erl` (~300 LOC)
- **Clarity**: QUIC transport fully separated from business logic

---

## Analysis Documents Created

1. `/tmp/gateway_extraction_analysis.md` - Comprehensive analysis of remaining code in gateway
2. `/tmp/quic_extraction_design.md` - Detailed design for QUIC transport extraction

---

## Next Steps

1. **Immediate**: Implement Phase 2 (QUIC extraction) if time allows
2. **Testing**: Run full test suite after Phase 2
3. **E2E**: Test Macula Arcade matchmaking with refactored gateway
4. **Documentation**: Update gateway module header comments
5. **CHANGELOG**: Document refactoring in v0.7.9 or v0.8.0

---

## Context from Previous Work

### v0.7.9 Already Includes
- DHT-routed pub/sub infrastructure (`pubsub_route` protocol)
- `macula_pubsub_routing.erl` module
- Gateway using DHT routing for pub/sub (NOT the old endpoint-based routing)

### January 2025 Refactoring (Already Done)
- `macula_gateway_client_manager.erl` (~235 LOC, 24 tests)
- `macula_gateway_pubsub.erl` (~280 LOC, 31 tests)
- `macula_gateway_rpc.erl` (~215 LOC, 20 tests)
- `macula_gateway_mesh.erl` (~295 LOC, 16 tests)
- `macula_gateway_dht.erl` (~149 LOC, stateless)
- `macula_gateway_rpc_router.erl` (~265 LOC, 17 tests)
- `macula_gateway_sup.erl` (~113 LOC, 24 tests)

**Total Previously Extracted**: ~1,552 LOC across 7 modules

---

## Why Gateway Was Still Large Despite January Refactoring

January 2025 extracted **business logic**:
- ✅ Client management
- ✅ Subscriber tracking
- ✅ RPC handler registry
- ✅ Connection pooling
- ✅ DHT queries

But left **infrastructure** in gateway:
- ❌ QUIC transport layer (~400 LOC)
- ❌ PubSub routing logic (~100 LOC) - **NOW EXTRACTED in Phase 1**
- ❌ Configuration helpers (~100 LOC)
- ❌ Message routing dispatcher (~300 LOC) - **SHOULD STAY** (clean pattern matching)

---

## Summary

**Phase 1 Complete** ✅
- Gateway: 1,115 → 1,011 LOC (9.3% reduction)
- All tests passing
- Clean extraction with proper module separation

**Phase 2 Designed** 📋
- Will reduce gateway to ~700 LOC (37% total reduction from original)
- Stateless callback pattern designed
- Implementation ready to begin

**Overall Impact**
- Cleaner separation of concerns
- More maintainable codebase
- Easier to test in isolation
- Following single responsibility principle
