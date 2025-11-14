# Macula Gateway Behavior Catalog

**Purpose:** Complete documentation of all behaviors in `macula_gateway.erl` (1,340 LOC) to support TDD-based refactoring.

**Created:** 2025-01-13
**Status:** Phase 1 - Baseline Documentation

---

## Table of Contents

1. [Overview](#overview)
2. [State Structure](#state-structure)
3. [Initialization](#initialization)
4. [handle_call Behaviors](#handle_call-behaviors)
5. [handle_cast Behaviors](#handle_cast-behaviors)
6. [handle_info Behaviors](#handle_info-behaviors)
7. [Internal Functions](#internal-functions)
8. [Test Coverage Analysis](#test-coverage-analysis)

---

## Overview

### God Module Responsibilities

The gateway currently handles **6 major responsibilities**:

| Responsibility | LOC Estimate | State Fields | Evidence |
|----------------|--------------|--------------|----------|
| **QUIC Listener** | ~200 | `listener`, `port` | `new_conn`, `new_stream`, `shutdown`, async_accept |
| **Client Management** | ~300 | `clients`, `client_streams` | `client_connected`, `DOWN`, monitoring |
| **Pub/Sub Routing** | ~350 | `subscriptions`, `stream_subscriptions` | `subscribe`, `unsubscribe`, `publish` |
| **RPC Registration** | ~250 | `registrations` | `register_handler`, `unregister_handler`, `call` |
| **DHT Operations** | ~140 | `node_id` | `store`, `find_value`, `find_node`, DHT queries |
| **Mesh Connections** | ~100 | `mesh_connections` | connection pooling, peer-to-peer routing |

**Total:** 1,340 LOC mixing 6 concerns in a single gen_server.

---

## State Structure

```erlang
-record(state, {
    port :: inet:port_number(),                    % QUIC listener concern
    realm :: binary(),                             % Shared - multi-tenancy
    node_id :: binary(),                           % Shared - DHT identity (32-byte)
    listener :: pid() | undefined,                 % QUIC listener concern
    clients :: #{pid() => client_info()},          % Client management
    subscriptions :: #{binary() => [pid()]},       % Pub/Sub - topic => subscribers
    stream_subscriptions :: #{pid() => [binary()]}, % Pub/Sub - stream => topics
    registrations :: #{binary() => pid()},         % RPC - procedure => handler
    mesh_connections :: #{binary() => mesh_connection_info()}, % Mesh - node_id => connection
    client_streams :: #{binary() => pid()}         % Mesh - node_id => bidirectional stream
}).
```

**Problem:** All concerns share one state record, violating Single Responsibility Principle.

---

## Initialization

### `init/1` - Lines 153-298

**Purpose:** Start QUIC listener with TLS certificates and initialize gateway state.

**Input:** `Opts` - Proplist with configuration options

**Flow:**
1. Extract port (default: 9443) and realm (default: "macula.default")
2. Get TLS certificates from environment or defaults
3. Validate certificate files exist (`macula_quic_cert:validate_files/2`)
4. Start QUIC listener (`start_quic_listener/4`)

**Success:** `{ok, #state{...}}`

**Failure Cases:**
- Certificate validation fails → `{stop, {cert_validation_failed, Reason}}`
- QUIC listen fails → `{stop, {listen_failed, Reason}}`

### `start_quic_listener/4` - Lines 212-298

**Purpose:** Start QUIC listener and routing server, initialize empty state maps.

**Flow:**
1. Configure QUIC listener with TLS certs, ALPN="macula", stream counts
2. Call `macula_quic:listen/2`
3. Mark health server ready (`macula_gateway_health:set_ready(true)`)
4. Register diagnostics procedures
5. Register for async connection acceptance (`quicer:async_accept/2`)
6. Generate or get NODE_NAME-based node ID
7. Start DHT routing server (`macula_routing_server:start_link/2`)
8. Initialize state with empty maps for clients, subscriptions, registrations, mesh connections

**State Initialization:**
```erlang
State = #state{
    port = Port,
    realm = Realm,
    node_id = LocalNodeId,
    listener = Listener,
    clients = #{},                 % Empty client registry
    subscriptions = #{},           % Empty topic subscriptions
    stream_subscriptions = #{},    % Empty stream subscriptions
    registrations = #{},           % Empty RPC registrations
    mesh_connections = #{},        % Empty mesh connections
    client_streams = #{}           % Empty bidirectional streams
}
```

### `get_node_id/2` - Lines 196-208

**Purpose:** Get node ID from NODE_NAME environment variable or generate from realm/port hash.

**Pattern Matching:**
```erlang
get_node_id(Realm, Port) ->
    case os:getenv("NODE_NAME") of
        false ->
            %% Generate deterministic hash
            crypto:hash(sha256, term_to_binary({Realm, Port}));
        NodeName when is_list(NodeName) ->
            %% Use environment variable
            list_to_binary(NodeName)
    end.
```

**Idiomatic Pattern:** Pattern matching on function head, guards, avoiding `if`.

---

## handle_call Behaviors

### `handle_call(get_stats, _, State)` - Lines 300-308

**Purpose:** Return gateway statistics.

**Returns:**
```erlang
Stats = #{
    port => State#state.port,
    realm => State#state.realm,
    clients => maps:size(State#state.clients),
    subscriptions => maps:size(State#state.subscriptions),
    registrations => maps:size(State#state.registrations)
}
```

**Reply:** `{reply, Stats, State}`

**Test Gap:** No test for actual statistics returned (only API existence test).

### `handle_call({register_handler, Procedure, Handler}, _, State)` - Lines 310-315

**Purpose:** Register RPC handler for a procedure.

**Flow:**
1. Log registration
2. Add `Procedure => Handler` to `State#state.registrations`
3. Return `ok`

**Reply:** `{reply, ok, NewState}`

**State Change:** `registrations` map updated

**Test Gap:** No test verifying handler is stored in state and can be invoked.

### `handle_call({unregister_handler, Procedure}, _, State)` - Lines 317-322

**Purpose:** Remove RPC handler for a procedure.

**Flow:**
1. Log unregistration
2. Remove `Procedure` from `State#state.registrations`
3. Return `ok`

**Reply:** `{reply, ok, NewState}`

**State Change:** `registrations` map updated

**Test Gap:** No test verifying handler is removed from state.

### `handle_call(_Request, _, State)` - Lines 324-325

**Purpose:** Catch-all for unknown requests.

**Reply:** `{reply, {error, unknown_request}, State}`

---

## handle_cast Behaviors

### `handle_cast({process_rpc_route, RpcRouteMsg}, State)` - Lines 328-333

**Purpose:** Process RPC route message forwarded from connection.

**Flow:**
1. Extract `payload` from `RpcRouteMsg` (contains CALL message)
2. Call `handle_rpc_call_routed(undefined, CallMsg, RpcRouteMsg, State)`

**Return:** Result of `handle_rpc_call_routed/4`

**Test Gap:** No test for RPC route message processing flow.

### `handle_cast(_Request, State)` - Lines 335-336

**Purpose:** Catch-all for unknown cast messages.

**Return:** `{noreply, State}`

---

## handle_info Behaviors

### QUIC Events

#### `handle_info({quic, new_stream, Stream, StreamProps}, State)` - Lines 338-354

**Purpose:** Handle new stream created by peer.

**Flow:**
1. Log stream creation
2. Set stream to active mode (`quicer:setopt(Stream, active, true)`)
3. If successful, continue; if failed, log error and continue

**Return:** `{noreply, State}`

**Test Gap:** No test for stream activation flow.

#### `handle_info({quic, Data, Stream, _Flags}, State)` - Lines 356-361

**Purpose:** Handle data from QUIC stream (active mode).

**Flow:**
1. Log data receipt
2. Decode data (`macula_protocol_decoder:decode(Data)`)
3. Call `handle_decoded_message(DecodeResult, Stream, State)`

**Return:** Result of `handle_decoded_message/3`

**Test Gap:** No test for data decoding and dispatch.

#### `handle_info({quic, new_conn, Conn, ConnInfo}, State)` - Lines 451-497

**Purpose:** Handle new QUIC connection established.

**Flow:**
1. Log connection
2. Complete TLS handshake (`quicer:handshake(Conn)`)
3. Start accepting streams on connection (`quicer:async_accept_stream(Conn, #{})`)
4. Register for next connection (`quicer:async_accept(State#state.listener, #{})`)

**Return:** `{noreply, State}`

**Test Gap:** No test for connection handshake flow.

#### `handle_info({quic, shutdown, Conn, Reason}, State)` - Lines 499-502

**Purpose:** Handle connection shutting down.

**Flow:** Log shutdown

**Return:** `{noreply, State}`

#### `handle_info({quic, transport_shutdown, Conn, Reason}, State)` - Lines 504-507

**Purpose:** Handle transport layer shutting down.

**Flow:** Log shutdown

**Return:** `{noreply, State}`

#### `handle_info({quic, peer_needs_streams, _Conn, _StreamType}, State)` - Lines 444-448

**Purpose:** Handle peer signaling it wants to open more streams.

**Flow:** Acknowledge (no action needed)

**Return:** `{noreply, State}`

### Client Management

#### `handle_info({client_connected, ClientPid, ClientInfo}, State)` - Lines 363-371

**Purpose:** Handle client registration.

**Flow:**
1. Log client connection
2. Monitor client process (`erlang:monitor(process, ClientPid)`)
3. Add client to `State#state.clients` map

**Return:** `{noreply, NewState}`

**State Change:** `clients` map updated

**Test Gap:** No test for client monitoring and registration.

#### `handle_info({'DOWN', _Ref, process, ClientPid, _Reason}, State)` - Lines 373-394

**Purpose:** Handle client disconnection (process death).

**Flow:**
1. Log disconnection
2. Remove client from all topic subscriptions
3. Remove all RPC registrations owned by client
4. Remove client from clients map

**Return:** `{noreply, NewState}`

**State Changes:**
- `subscriptions` - client removed from all topics
- `registrations` - client's handlers removed
- `clients` - client entry removed

**Test Gap:** No test for cleanup on client disconnect.

### Pub/Sub Operations

#### `handle_info({publish, FromPid, Topic, Payload}, State)` - Lines 396-404

**Purpose:** Handle publish message from client.

**Flow:**
1. Get all subscribers to topic
2. Send `{event, Topic, Payload}` to all subscribers except sender

**Return:** `{noreply, State}`

**Filter:** Publisher does NOT receive own message

**Test Gap:** No test for message routing and filtering.

#### `handle_info({subscribe, ClientPid, Topic}, State)` - Lines 406-416

**Purpose:** Handle subscription request.

**Flow:**
1. Get current subscribers to topic
2. Add client to subscribers list
3. Update `subscriptions` map
4. Send `{subscribed, Topic}` acknowledgment to client

**Return:** `{noreply, NewState}`

**State Change:** `subscriptions` map updated

**Test Gap:** No test for subscription flow.

#### `handle_info({unsubscribe, ClientPid, Topic}, State)` - Lines 418-428

**Purpose:** Handle unsubscription request.

**Flow:**
1. Get current subscribers to topic
2. Remove client from subscribers list
3. Update `subscriptions` map
4. Send `{unsubscribed, Topic}` acknowledgment to client

**Return:** `{noreply, NewState}`

**State Change:** `subscriptions` map updated

**Test Gap:** No test for unsubscription flow.

### RPC Operations

#### `handle_info({call, FromPid, CallId, Procedure, Args}, State)` - Lines 430-442

**Purpose:** Handle RPC call request.

**Flow:**
1. Look up handler for procedure in `State#state.registrations`
2. If no handler: send `{call_error, CallId, <<"wamp.error.no_such_procedure">>}` to caller
3. If handler exists: send `{invoke, FromPid, CallId, Procedure, Args}` to handler

**Return:** `{noreply, State}`

**Test Gap:** No test for call routing and error handling.

### DHT Operations

#### `handle_info({dht_query, FromPid, _QueryType, QueryData}, State)` - Lines 509-537

**Purpose:** Handle DHT query (find_node, find_value, store).

**Flow:**
1. Decode query data
2. Forward to DHT routing server (`macula_routing_server:handle_message/2`)
3. Encode reply based on message type
4. Send reply back to client via `FromPid ! {dht_reply, ReplyData}`

**Return:** `{noreply, State}`

**Test Gap:** No test for DHT query routing.

### Catch-all

#### `handle_info(Info, State)` - Lines 539-541

**Purpose:** Catch-all for unhandled messages.

**Flow:** Log warning

**Return:** `{noreply, State}`

---

## Internal Functions

### Message Decoding and Dispatch

#### `handle_decoded_message({ok, {connect, ConnectMsg}}, Stream, State)` - Lines 1080-1082

**Purpose:** Dispatch CONNECT message.

**Delegates to:** `handle_connect(Stream, ConnectMsg, State)`

#### `handle_decoded_message({ok, {store, StoreMsg}}, Stream, State)` - Lines 1085-1088

**Purpose:** Dispatch DHT STORE message.

**Delegates to:** `handle_dht_store(Stream, StoreMsg, State)`

#### `handle_decoded_message({ok, {find_value, FindValueMsg}}, Stream, State)` - Lines 1091-1094

**Purpose:** Dispatch DHT FIND_VALUE message.

**Delegates to:** `handle_dht_find_value(Stream, FindValueMsg, State)`

#### `handle_decoded_message({ok, {find_node, FindNodeMsg}}, Stream, State)` - Lines 1097-1100

**Purpose:** Dispatch DHT FIND_NODE message.

**Delegates to:** `handle_dht_find_node(Stream, FindNodeMsg, State)`

#### `handle_decoded_message({ok, {rpc_route, RpcRouteMsg}}, Stream, State)` - Lines 1103-1130

**Purpose:** Dispatch RPC route message (multi-hop DHT routing).

**Flow:**
1. Call `macula_rpc_routing:route_or_deliver(LocalNodeId, RpcRouteMsg, RoutingServerPid)`
2. Pattern match on result:
   - `{deliver, <<"call">>, CallMsg}` → deliver CALL locally
   - `{deliver, <<"reply">>, ReplyMsg}` → deliver REPLY locally
   - `{forward, NextHopNodeInfo, UpdatedRpcRouteMsg}` → forward to next hop
   - `{error, Reason}` → log error

**Test Gap:** No test for multi-hop routing logic.

#### `handle_decoded_message({ok, {call, CallMsg}}, Stream, State)` - Lines 1133-1136

**Purpose:** Dispatch RPC call (legacy direct call).

**Delegates to:** `handle_rpc_call(Stream, CallMsg, State)`

#### `handle_decoded_message({ok, {subscribe, SubMsg}}, Stream, State)` - Lines 1139-1142

**Purpose:** Dispatch SUBSCRIBE message.

**Delegates to:** `handle_subscribe(Stream, SubMsg, State)`

#### `handle_decoded_message({ok, {unsubscribe, UnsubMsg}}, Stream, State)` - Lines 1145-1148

**Purpose:** Dispatch UNSUBSCRIBE message.

**Delegates to:** `handle_unsubscribe(Stream, UnsubMsg, State)`

#### `handle_decoded_message({ok, {publish, PubMsg}}, Stream, State)` - Lines 1151-1154

**Purpose:** Dispatch PUBLISH message.

**Delegates to:** `handle_publish(Stream, PubMsg, State)`

#### `handle_decoded_message({ok, {Type, Other}}, _Stream, State)` - Lines 1157-1159

**Purpose:** Catch-all for other message types.

**Flow:** Log message, no action

**Return:** `{noreply, State}`

#### `handle_decoded_message({error, DecodeErr}, _Stream, State)` - Lines 1162-1164

**Purpose:** Handle decode error.

**Flow:** Log error

**Return:** `{noreply, State}`

### Connection Management

#### `handle_connect(Stream, ConnectMsg, State)` - Lines 556-634

**Purpose:** Process client CONNECT message and establish connection.

**Flow:**
1. Validate realm matches gateway realm
2. Extract `node_id`, `endpoint`, and `capabilities` from message
3. Parse endpoint to IP address
4. Store client stream for bidirectional communication (`client_streams` map)
5. Store connection info for mesh routing (`mesh_connections` map)
6. Add peer to DHT routing table
7. Send PONG acknowledgment back to client

**State Changes:**
- `client_streams` - store stream indexed by node_id
- `mesh_connections` - store connection info indexed by node_id

**Failure Case:** Realm mismatch → close stream

**Test Gap:** No test for CONNECT flow and state updates.

### DHT Handlers

#### `handle_dht_store(_Stream, StoreMsg, State)` - Lines 637-649

**Purpose:** Process DHT STORE message.

**Flow:**
1. Forward to routing server (`macula_routing_server:handle_message/2`)
2. No response sent back (STORE is fire-and-forget)

**Return:** `{noreply, State}`

**Test Gap:** No test for STORE processing.

#### `handle_dht_find_value(Stream, FindValueMsg, State)` - Lines 652-666

**Purpose:** Process DHT FIND_VALUE message.

**Flow:**
1. Forward to routing server
2. Encode reply (`macula_protocol_encoder:encode(find_value_reply, Reply)`)
3. Send reply over stream

**Return:** `{noreply, State}`

**Test Gap:** No test for FIND_VALUE flow.

#### `handle_dht_find_node(Stream, FindNodeMsg, State)` - Lines 669-683

**Purpose:** Process DHT FIND_NODE message.

**Flow:**
1. Forward to routing server
2. Encode reply (`macula_protocol_encoder:encode(find_node_reply, Reply)`)
3. Send reply over stream

**Return:** `{noreply, State}`

**Test Gap:** No test for FIND_NODE flow.

### RPC Handlers

#### `handle_rpc_call_routed(_Stream, CallMsg, RpcRouteMsg, State)` - Lines 687-748

**Purpose:** Process routed RPC CALL delivered locally.

**Flow:**
1. Extract `procedure`, `call_id`, `args`, `source_node_id`
2. Look up handler in `State#state.registrations`
3. If no handler: send error reply via DHT routing
4. If handler exists:
   - Decode JSON args
   - Invoke handler
   - Encode result
   - Send reply via DHT routing (`send_reply_via_routing/3`)
5. Catch handler errors and send error reply

**Return:** `{noreply, State}`

**Important:** Handler must persist in registrations after invocation.

**Test Gap:** No test for routed RPC call flow and handler persistence.

#### `handle_rpc_reply_routed(_ReplyMsg, RpcRouteMsg, State)` - Lines 752-806

**Purpose:** Process routed REPLY delivered locally.

**Flow:**
1. Extract `destination_node_id`
2. Check if destination is local node or remote client
3. If local: send to local connection via gproc (`gen_server:cast`)
4. If remote: look up client stream and send reply

**Return:** `{noreply, State}`

**Test Gap:** No test for reply routing.

#### `send_reply_via_routing(DestinationNodeId, ReplyMsg, State)` - Lines 809-829

**Purpose:** Send REPLY via DHT routing.

**Flow:**
1. Wrap reply in `rpc_route` envelope
2. Call `macula_rpc_routing:route_or_deliver/3`
3. Pattern match on result:
   - `{forward, NextHopNodeInfo, UpdatedRpcRouteMsg}` → forward to next hop
   - `{deliver, <<"reply">>, _}` → log warning (shouldn't deliver to ourselves)
   - `{error, Reason}` → log error

**Test Gap:** No test for reply routing logic.

#### `forward_rpc_route(NextHopNodeInfo, RpcRouteMsg, State)` - Lines 832-841

**Purpose:** Forward rpc_route message to next hop.

**Flow:**
1. Extract `next_node_id` and `address`
2. Call `forward_via_mesh_connection/4`

**Test Gap:** No test for forwarding logic.

#### `forward_via_mesh_connection(NextNodeId, Address, RpcRouteMsg, State)` - Lines 845-864

**Purpose:** Forward message via mesh connection (peer-to-peer).

**Flow:**
1. Get or create mesh connection (`get_or_create_mesh_connection/3`)
2. Encode `rpc_route` message
3. Send via QUIC stream
4. If send fails: remove connection from cache

**Return:** `{noreply, State}`

**Test Gap:** No test for mesh connection forwarding.

#### `handle_rpc_call(Stream, CallMsg, State)` - Lines 867-935

**Purpose:** Process RPC call (legacy direct call).

**Flow:**
1. Extract `procedure`, `call_id`, `args`
2. Look up handler
3. If no handler: send error reply
4. If handler exists:
   - Decode JSON args
   - Invoke handler
   - Encode result
   - Send reply on stream
5. Catch handler errors and send error reply

**Return:** `{noreply, State}`

**Test Gap:** No test for direct RPC call flow.

### Mesh Connection Management

#### `get_or_create_mesh_connection(NodeId, Address, State)` - Lines 950-1027

**Purpose:** Get existing or create new QUIC connection to peer node.

**Flow:**
1. Check if connection exists in `mesh_connections`
2. If undefined: create new connection
3. If exists but closed: remove and recreate
4. If exists and alive: open new stream on existing connection
5. Cache connection in `mesh_connections` map

**Idiomatic Pattern:** Multiple function clauses via `case` pattern matching.

**Return:** `{ok, Stream, NewState}` or `{error, Reason, NewState}`

**Test Gap:** No test for connection pooling and reuse.

#### `create_mesh_connection({Host, Port}, State)` - Lines 1032-1073

**Purpose:** Create new QUIC connection to peer.

**Flow:**
1. Get TLS certificates
2. Configure connection options
3. Call `macula_quic:connect/4`
4. Open bidirectional stream

**Return:** `{ok, Conn, Stream, State}` or `{error, Reason, State}`

**Test Gap:** No test for connection creation.

#### `is_connection_alive(Conn)` - Lines 132-147

**Purpose:** Check if QUIC connection is alive and ready.

**Pattern Matching:**
```erlang
is_connection_alive(undefined) -> false;
is_connection_alive(Conn) when is_pid(Conn) orelse is_reference(Conn) ->
    try
        case quicer:getopt(Conn, param_conn_state) of
            {ok, connected} -> true;
            {ok, _OtherState} -> false;
            {error, _} -> false
        end
    catch
        _:_ -> false
    end;
is_connection_alive(_Other) -> false.
```

**Idiomatic:** Multiple function clauses, guards, `try..catch` is acceptable here for NIF interaction.

**Test Coverage:** Has 2 tests (undefined, contract)

### Pub/Sub Handlers

#### `handle_subscribe(Stream, SubMsg, State)` - Lines 1171-1204

**Purpose:** Process subscription to topics.

**Flow:**
1. Extract topics from message
2. For each topic: add stream to subscribers list
3. Update `subscriptions` map (topic → [streams])
4. Update `stream_subscriptions` map (stream → [topics])

**Return:** `{noreply, NewState}`

**State Changes:**
- `subscriptions` - topics updated with new stream
- `stream_subscriptions` - stream updated with new topics

**Test Gap:** No test for subscription logic.

#### `handle_unsubscribe(Stream, UnsubMsg, State)` - Lines 1207-1245

**Purpose:** Process unsubscription from topics.

**Flow:**
1. Extract topics from message
2. For each topic: remove stream from subscribers list
3. Remove empty topics from `subscriptions` map
4. Update `stream_subscriptions` map
5. Remove stream from `stream_subscriptions` if no topics left

**Return:** `{noreply, NewState}`

**State Changes:**
- `subscriptions` - stream removed from topics
- `stream_subscriptions` - topics removed from stream

**Test Gap:** No test for unsubscription logic.

#### `handle_publish(_PublisherStream, PubMsg, State)` - Lines 1248-1295

**Purpose:** Distribute published message to all subscribers.

**Flow:**
1. Extract topic from message
2. Find all subscribers whose patterns match topic (wildcard support)
3. Use `macula_connection_utils:topic_matches/5` for pattern matching
4. Encode publish message
5. Send to all matching subscribers

**Wildcard Support:** Patterns with `*` and `**` wildcards

**Return:** `{noreply, State}`

**Test Gap:** No test for wildcard matching and message distribution.

### Helper Functions

#### `encode_json(Data)` - Lines 938-940

**Purpose:** Encode map/list to JSON binary.

**Pattern:**
```erlang
-spec encode_json(map() | list()) -> binary().
encode_json(Data) ->
    iolist_to_binary(json:encode(Data)).
```

**Test Gap:** No test (trivial wrapper).

#### `parse_endpoint(Endpoint)` - Lines 1304-1339

**Purpose:** Parse endpoint URL to address tuple.

**Pattern Matching:**
```erlang
parse_endpoint(undefined) -> {{0,0,0,0}, 0};
parse_endpoint(Endpoint) when is_binary(Endpoint) ->
    %% Parse using uri_string, resolve hostname via inet:getaddr
    ...
```

**Returns:** `{{IP_tuple}, Port}` or `{{0,0,0,0}, 0}` for invalid

**Test Gap:** No test for endpoint parsing.

---

## Test Coverage Analysis

### Existing Test Coverage (~29 tests)

**API Structure Tests (✅ Good Coverage):**
- Module exports
- Gen_server callbacks
- Function arities
- Function contracts
- Type safety

**Configuration Tests (✅ Good Coverage):**
- Default port and realm
- Options handling

**Helper Function Tests (⚠️ Partial):**
- `is_connection_alive/1` - 2 tests

**Business Logic Tests (❌ Missing):**
- Client connection/disconnection flow
- Pub/sub subscribe/unsubscribe/publish
- RPC registration/call/reply
- DHT operations
- Mesh connection management
- Multi-hop routing

### Test Gaps (High Priority)

#### 1. Client Management (~10 tests needed)
- [ ] Client connects and is added to clients map
- [ ] Client is monitored after connection
- [ ] Client disconnects and is removed from clients map
- [ ] Client disconnection removes all subscriptions
- [ ] Client disconnection removes all RPC registrations
- [ ] Client stream is stored for bidirectional communication
- [ ] Client stream is removed on disconnect

#### 2. Pub/Sub Operations (~15 tests needed)
- [ ] Subscribe adds stream to topic subscribers
- [ ] Subscribe updates bidirectional mapping
- [ ] Multiple subscribers can subscribe to same topic
- [ ] Unsubscribe removes stream from topic
- [ ] Unsubscribe removes empty topics
- [ ] Publish routes to all subscribers
- [ ] Publish filters out sender
- [ ] Wildcard subscriptions (*) match topics
- [ ] Wildcard subscriptions (**) match nested topics
- [ ] Failed sends to subscribers don't block others

#### 3. RPC Operations (~12 tests needed)
- [ ] Register handler stores handler in state
- [ ] Unregister handler removes handler from state
- [ ] Call with no handler returns error
- [ ] Call with handler invokes handler
- [ ] Call with handler error returns error reply
- [ ] Handler persists after invocation
- [ ] Routed call processes correctly
- [ ] Routed reply routes to correct destination
- [ ] Reply routing handles binary payload_type
- [ ] Direct call flow works
- [ ] Call ID matching works

#### 4. DHT Operations (~8 tests needed)
- [ ] STORE message forwards to routing server
- [ ] FIND_VALUE returns value if found
- [ ] FIND_VALUE returns closest nodes if not found
- [ ] FIND_NODE returns closest nodes
- [ ] Node ID generation from NODE_NAME
- [ ] Node ID generation from realm/port hash

#### 5. Mesh Connection Management (~10 tests needed)
- [ ] Connection created on first use
- [ ] Connection reused on subsequent use
- [ ] Dead connection removed and recreated
- [ ] Connection liveness check works
- [ ] Stream opened on existing connection
- [ ] Failed connection removed from cache
- [ ] Endpoint parsing works correctly
- [ ] TLS certificate validation

#### 6. QUIC Listener (~8 tests needed)
- [ ] Listener starts on configured port
- [ ] Listener registers for connections
- [ ] New connection triggers handshake
- [ ] New stream sets active mode
- [ ] Data decoded and dispatched
- [ ] Connection shutdown handled
- [ ] Stream shutdown handled

**Total Test Gap:** ~63 additional tests needed for >80% coverage

### Integration Test Requirements

**E2E Tests Needed (~5 scenarios):**
1. Full pub/sub flow: connect → subscribe → publish → receive
2. Full RPC flow: connect → register handler → call → reply
3. Multi-hop RPC routing: client → gateway1 → gateway2 → provider
4. DHT service discovery: provider advertises → consumer discovers → consumer calls
5. Client reconnection and state recovery

**Existing Integration Tests:**
- Docker Compose multi-node tests (e2e tests in `test/e2e/`)

---

## Summary

### Current State
- **Lines of Code:** 1,340 (god module)
- **Responsibilities:** 6 mixed concerns
- **Test Coverage:** ~29 unit tests (API structure only)
- **Test Gap:** ~63 tests needed for business logic

### Refactoring Target
- **5 extracted modules:** listener, client_manager, pubsub, rpc, stats
- **Supervision tree:** `macula_gateway_sup` with one_for_all strategy
- **Test Coverage:** >80% before extraction
- **LOC per module:** <400 lines

### Next Steps (Phase 1)
1. ✅ Complete behavior catalog (this document)
2. ⏳ Run existing tests to establish baseline
3. ⏳ Create integration test suite (baseline)
4. ⏳ Fill test gaps (63 tests) over 2-3 weeks
5. ⏳ Begin Phase 2: Extract modules with TDD

---

**Document Status:** Phase 1 Complete - Ready for test creation
