# macula_gateway.erl Behavior Documentation

**Status:** 📝 Documentation Phase - CRITICAL FINDINGS
**Date:** 2025-01-13
**Purpose:** Complete catalog of all behaviors, edge cases, and test scenarios for `macula_gateway.erl`
**Module Size:** 1,340 lines
**Related:** `gateway_refactoring_plan.md`, `CLAUDE.md`

## ⚠️ CRITICAL DISCOVERY

**THE EXTRACTED MODULES ARE NOT INTEGRATED!**

Phases 2-4, 6 created three focused modules with comprehensive tests (99 passing):
- ✅ `macula_gateway_client_manager.erl` (235 LOC, 24 tests)
- ✅ `macula_gateway_pubsub.erl` (280 LOC, 31 tests)
- ✅ `macula_gateway_rpc.erl` (215 LOC, 20 tests)
- ✅ `macula_gateway_sup.erl` (113 LOC, 24 tests)

**BUT:** The old implementation code is STILL in `macula_gateway.erl` at:
- Lines 364-394: Client lifecycle (duplicates client_manager)
- Lines 397-404: Publish logic (duplicates pubsub)
- Lines 407-428: Subscribe/unsubscribe (duplicates pubsub)
- Lines 431-442: RPC routing (duplicates rpc)

**The gateway is still running the OLD CODE, not the extracted modules!**

---

## Overview

This document systematically catalogs all behaviors implemented in `macula_gateway.erl` to:
1. Document what was already extracted but NOT integrated
2. Identify additional responsibilities requiring extraction
3. Plan complete refactoring and integration strategy

---

## Responsibility Analysis

### Group A: Already Extracted (But Not Integrated)

#### 1. Client Lifecycle Management
**Lines:** 364-394 (handle_info)
**Status:** ✅ Extracted as `macula_gateway_client_manager` (NOT integrated)
**What It Does:**
- Client connection tracking
- Process monitoring (handle_info {'DOWN', ...})
- Client disconnection cleanup
- Remove from subscriptions/registrations on disconnect

**State Used:**
- `clients :: #{pid() => client_info()}`

**Integration Required:**
- Replace lines 364-394 with calls to client_manager
- Remove `clients` from state record
- Use supervisor to get client_manager PID

---

#### 2. Pub/Sub Operations
**Lines:** 397-428 (handle_info), 1138-1295 (handlers)
**Status:** ✅ Extracted as `macula_gateway_pubsub` (NOT integrated)
**What It Does:**
- Subscribe/unsubscribe to topics
- Publish messages to subscribers
- Wildcard topic matching (*, **)
- Track stream subscriptions

**State Used:**
- `subscriptions :: #{binary() => [pid()]}`
- `stream_subscriptions :: #{pid() => [binary()]}`

**Integration Required:**
- Replace lines 397-428 with calls to pubsub
- Replace lines 1138-1295 with delegations to pubsub
- Remove subscriptions/stream_subscriptions from state
- Use supervisor to get pubsub PID

---

#### 3. RPC Handler Management
**Lines:** 310-322 (handle_call), 431-442 (handle_info), 866-935 (handler)
**Status:** ✅ Extracted as `macula_gateway_rpc` (NOT integrated)
**What It Does:**
- Register/unregister RPC handlers
- Route RPC calls to handlers
- Direct RPC call handling (legacy)
- Error handling for missing handlers

**State Used:**
- `registrations :: #{binary() => pid()}`

**Integration Required:**
- Replace lines 310-322 with calls to rpc module
- Replace lines 431-442 with calls to rpc module
- Replace lines 866-935 with delegations to rpc
- Remove registrations from state
- Use supervisor to get rpc PID

---

### Group B: Additional Responsibilities (Not Yet Extracted)

#### 4. QUIC Listener Management
**Lines:** 212-298 (start_quic_listener), 241-247 (async_accept), 451-497 (new_conn)
**Status:** ❌ Not extracted
**What It Does:**
- Start QUIC listener with TLS certificates
- Handle async_accept for new connections
- Complete TLS handshake
- Accept streams on new connections
- Register for next connection

**Complexity:** Medium-High
- Certificate loading logic
- Handshake state management
- Connection acceptance flow

**Extraction Candidate:** `macula_gateway_quic_listener`
**Estimated LOC:** ~250-300

**Key Functions:**
- `start_listener/4` - Start QUIC listener
- `handle_new_conn/2` - Handle new_conn events
- `handle_handshake/2` - Complete handshake
- `accept_next_connection/1` - Register for next conn

**State Needed:**
- `listener :: pid()`
- `port :: inet:port_number()`
- TLS certificate paths

---

#### 5. QUIC Stream Management
**Lines:** 339-354 (new_stream), 357-361 (stream data)
**Status:** ❌ Not extracted
**What It Does:**
- Handle new_stream events
- Set streams to active mode
- Receive binary data from streams
- Decode protocol messages

**Complexity:** Low-Medium
- Simple stream setup
- Data buffering/decoding

**Could Be:** Part of listener module or separate stream manager
**Estimated LOC:** ~100-150

---

#### 6. Mesh Connection Management
**Lines:** 831-1073 (forward_rpc_route, get_or_create_mesh_connection, create_mesh_connection)
**Status:** ❌ Not extracted
**What It Does:**
- Create peer-to-peer QUIC connections
- Pool connections to remote nodes
- Check connection liveness
- Open new streams on existing connections
- Remove failed connections
- Cache connection info by node_id

**Complexity:** High
- Connection pooling logic
- Liveness checking
- Stream management
- Error handling and retry

**Extraction Candidate:** `macula_gateway_mesh_connection`
**Estimated LOC:** ~300-350

**Key Functions:**
- `get_or_create_connection/3` - Get cached or create new
- `create_connection/2` - Establish new peer connection
- `check_liveness/1` - Verify connection alive
- `open_stream/1` - Open new stream on connection
- `remove_connection/1` - Clean up failed connection

**State Needed:**
- `mesh_connections :: #{binary() => mesh_connection_info()}`
- `client_streams :: #{binary() => pid()}`

---

#### 7. DHT Query Handling
**Lines:** 510-537 (dht_query), 636-683 (handle_dht_store, handle_dht_find_value, handle_dht_find_node)
**Status:** ❌ Not extracted
**What It Does:**
- Process DHT queries (find_node, find_value, store)
- Forward to routing server
- Encode/send replies
- Handle routing server errors

**Complexity:** Medium
- Message type routing
- Encoding/decoding
- Error handling

**Extraction Candidate:** `macula_gateway_dht`
**Estimated LOC:** ~150-200

**Key Functions:**
- `handle_query/2` - Route DHT query to handler
- `handle_store/2` - Process STORE message
- `handle_find_value/2` - Process FIND_VALUE
- `handle_find_node/2` - Process FIND_NODE

---

#### 8. RPC Routing (Multi-hop)
**Lines:** 685-829 (handle_rpc_call_routed, send_reply_via_routing, forward_rpc_route)
**Status:** ❌ Not extracted
**What It Does:**
- Handle routed RPC calls (via DHT)
- Send replies via routing
- Forward messages to next hop
- Handle local delivery vs forwarding

**Complexity:** High
- Multi-hop routing logic
- Route/deliver decision
- Mesh forwarding
- Reply routing

**Extraction Candidate:** `macula_gateway_rpc_routing`
**Estimated LOC:** ~200-250

**Key Functions:**
- `handle_routed_call/3` - Process routed CALL
- `handle_routed_reply/2` - Process routed REPLY
- `send_reply_via_routing/3` - Route reply back
- `forward_to_next_hop/3` - Forward to next peer

---

#### 9. TLS Certificate Management
**Lines:** 157-191 (get_tls_certificates)
**Status:** ❌ Not extracted
**What It Does:**
- Load certificates from environment variables
- Fallback to pre-generated certs
- Validate certificate files exist
- Log certificate source

**Complexity:** Low
- Simple configuration logic
- Could remain in gateway

**Extraction:** Optional (low priority)

---

#### 10. Node ID Management
**Lines:** 196-208 (get_node_id)
**Status:** ❌ Not extracted
**What It Does:**
- Check NODE_NAME environment variable
- Generate hash from realm/port if not set
- Return 32-byte node ID

**Complexity:** Low
- Simple configuration
- Could remain in gateway

**Extraction:** Optional (low priority)

---

#### 11. Health Service Integration
**Lines:** 226-237 (start_quic_listener)
**Status:** ❌ Not extracted
**What It Does:**
- Set health status when ready
- Register diagnostics procedures
- Handle optional services gracefully

**Complexity:** Low
- Simple service calls
- Should remain in gateway

**Extraction:** No (keep in gateway)

---

#### 12. Message Decoding/Routing
**Lines:** 357-361 (handle_info), 1079-1164 (handle_decoded_message)
**Status:** ❌ Not extracted
**What It Does:**
- Decode incoming messages
- Route to appropriate handler based on type
- Handle connect, store, find_value, find_node, rpc_route, call, subscribe, unsubscribe, publish

**Complexity:** Medium
- Message type routing
- Delegation to handlers

**Extraction:** No (remains as gateway orchestration)
**Refactoring:** Replace handler calls with module delegations

---

#### 13. Statistics/Diagnostics
**Lines:** 300-308 (handle_call get_stats)
**Status:** ❌ Not extracted
**What It Does:**
- Return current gateway statistics
- Count clients, subscriptions, registrations

**Complexity:** Low
- Simple query aggregation

**Extraction:** No (keep in gateway)
**Refactoring:** Query child modules for their stats

---

#### 14. CONNECT Message Handling
**Lines:** 556-634 (handle_connect)
**Status:** ❌ Not extracted
**What It Does:**
- Validate realm matches
- Extract client node_id, endpoint, capabilities
- Parse endpoint URL
- Store client stream for bidirectional communication
- Cache mesh connection info
- Add peer to DHT routing table
- Send PONG acknowledgment

**Complexity:** Medium-High
- Multiple subsystem interactions
- State updates
- Endpoint parsing

**Extraction:** Potentially part of client_manager integration
**Estimated LOC:** ~80

---

## State Record Analysis

```erlang
-record(state, {
    port :: inet:port_number(),                          % Listener config
    realm :: binary(),                                   % Listener config
    node_id :: binary(),                                 % Gateway identity
    listener :: pid() | undefined,                       % QUIC listener PID

    %% GROUP A: Should be in extracted modules (NOT INTEGRATED)
    clients :: #{pid() => client_info()},                % → client_manager
    subscriptions :: #{binary() => [pid()]},             % → pubsub
    stream_subscriptions :: #{pid() => [binary()]},      % → pubsub
    registrations :: #{binary() => pid()},               % → rpc

    %% GROUP B: Legitimate gateway state
    mesh_connections :: #{binary() => mesh_connection_info()},  % Mesh connection pooling
    client_streams :: #{binary() => pid()}                      % Bidirectional client streams
}).
```

**After Full Refactoring, State Should Contain:**
- `port`, `realm`, `node_id`, `listener` (configuration)
- `mesh_connections`, `client_streams` (mesh management)
- Child module PIDs (client_manager, pubsub, rpc, quic_listener, mesh_manager, dht_handler, rpc_router)

---

## Refactoring Plan Summary

### Phase 7: Integration of Extracted Modules

**PRIORITY: CRITICAL**

Integrate the three already-extracted modules:

1. **Replace handle_info {'DOWN', ...} with client_manager call**
   - Lines 374-394
   - Delegate to `macula_gateway_client_manager:client_disconnected/2`

2. **Replace publish/subscribe/unsubscribe handle_info with pubsub calls**
   - Lines 397-428
   - Delegate to `macula_gateway_pubsub` functions

3. **Replace RPC routing handle_info with rpc module calls**
   - Lines 431-442
   - Delegate to `macula_gateway_rpc:call/4`

4. **Remove duplicate state fields**
   - Remove `clients`, `subscriptions`, `stream_subscriptions`, `registrations`

5. **Update init/1 to start supervisor instead**
   - Start `macula_gateway_sup` as child
   - Get child PIDs from supervisor
   - Store PIDs in state

### Phase 8: Extract QUIC Listener Module

**Priority:** High (foundational)

Extract QUIC listener management to `macula_gateway_quic_listener`:
- Lines 212-298 (start_quic_listener)
- Lines 451-497 (new_conn handling)
- Lines 339-354 (new_stream handling)

**Estimated Time:** 1-2 weeks

### Phase 9: Extract Mesh Connection Module

**Priority:** High (enables routing)

Extract mesh connection pooling to `macula_gateway_mesh_connection`:
- Lines 831-1073 (connection management)
- Lines 946-1027 (get_or_create, create_mesh_connection)

**Estimated Time:** 2-3 weeks

### Phase 10: Extract DHT Handler Module

**Priority:** Medium

Extract DHT query handling to `macula_gateway_dht`:
- Lines 510-537 (dht_query)
- Lines 636-683 (handle_dht_*)

**Estimated Time:** 1 week

### Phase 11: Extract RPC Routing Module

**Priority:** Medium-High

Extract multi-hop RPC routing to `macula_gateway_rpc_routing`:
- Lines 685-829 (routed RPC handling)

**Estimated Time:** 1-2 weeks

### Phase 12: Final Gateway Cleanup

**Priority:** Low

- Keep message decoding/routing in gateway (orchestration)
- Keep statistics aggregation
- Keep health service integration
- Convert gateway to facade/orchestrator pattern

**Estimated Time:** 1 week

---

## Total Module Count After Refactoring

1. ✅ `macula_gateway_client_manager` (235 LOC) - DONE, needs integration
2. ✅ `macula_gateway_pubsub` (280 LOC) - DONE, needs integration
3. ✅ `macula_gateway_rpc` (215 LOC) - DONE, needs integration
4. ✅ `macula_gateway_sup` (113 LOC) - DONE, needs children added
5. ❌ `macula_gateway_quic_listener` (~250-300 LOC) - TODO
6. ❌ `macula_gateway_mesh_connection` (~300-350 LOC) - TODO
7. ❌ `macula_gateway_dht` (~150-200 LOC) - TODO
8. ❌ `macula_gateway_rpc_routing` (~200-250 LOC) - TODO
9. 🔧 `macula_gateway` (~200-250 LOC after cleanup) - Facade/orchestrator

**Current:** 1,340 LOC in one module
**After Refactoring:** ~2,200 LOC across 9 focused modules (~244 LOC average)

---

## Test Coverage Requirements

### Already Tested (99 tests passing):
- ✅ Client manager (24 tests)
- ✅ Pub/Sub (31 tests)
- ✅ RPC (20 tests)
- ✅ Supervisor (24 tests)

### Need Tests:
- ❌ Integration tests (verify modules work together)
- ❌ QUIC listener module (~25 tests)
- ❌ Mesh connection module (~30 tests)
- ❌ DHT handler module (~20 tests)
- ❌ RPC routing module (~25 tests)
- ❌ Gateway facade/orchestration (~20 tests)

**Total Test Target:** ~240 tests (currently 99)

---

## Next Immediate Steps

1. **Create gateway integration plan** (`architecture/gateway_integration_plan.md`)
2. **Phase 7a: Write integration tests** for client_manager, pubsub, rpc
3. **Phase 7b: Replace old code** with module delegations
4. **Phase 7c: Update state record** (remove duplicate fields)
5. **Phase 7d: Update supervisor** to include all children
6. **Phase 7e: Verify integration** (all tests still passing)

**Then proceed with Phases 8-12 (extract remaining modules)**

---

## Conclusion

The gateway refactoring is **~40% complete**:
- ✅ 3 modules extracted with comprehensive tests
- ✅ Supervision tree created
- ❌ **BUT: Old code still running (not integrated)**
- ❌ 5 more modules need extraction
- ❌ Integration work required

**Critical Path:** Complete Phase 7 (integration) before extracting more modules.

**Estimated Total Time:**
- Phase 7 (Integration): 1-2 weeks
- Phases 8-12 (Remaining modules): 5-8 weeks
- **Total: 6-10 weeks**
