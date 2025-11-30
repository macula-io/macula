# NATS-Style Asynchronous RPC Architecture

**Date**: November 2025
**Version**: v0.12.3
**Status**: ✅ COMPLETED (with Async DHT Discovery + Pull-based Discovery)

---

## Implementation Summary

**Tests**: 22 unit tests passing (`test/macula_async_rpc_tests.erl`)

| Category | Tests | Description |
|----------|-------|-------------|
| Local handler | 7 | Registration, invocation, args, errors, multiple handlers |
| Protocol types | 5 | Message type IDs (0x24, 0x25), validation, binary keys |
| Async reply | 2 | Unknown request handling, unique request IDs |
| Async DHT discovery | 3 | Cache miss handling, request queuing, timeout |
| Pull-based discovery | 5 | Init config, normalization, dynamic prefetch |

**Key Implementation Files:**
- `src/macula_rpc_system/macula_rpc_handler.erl` - Core async RPC handler
- `src/macula_rpc_system/macula_service_registry.erl` - Local handler registry
- `src/macula_protocol_types.erl` - Protocol types (rpc_request=0x24, rpc_reply=0x25)
- `src/macula_protocol_encoder.erl` - Message validation
- `src/macula_ping_pong.erl` - Reference implementation using async RPC

---

## Problem Statement

The current Macula RPC implementation uses **synchronous request/response** via `gen_server:call/3`:

```erlang
%% Current: Blocking call
Result = macula_rpc_handler:call(RpcPid, Procedure, Args, Opts)
%% Caller blocks until response or timeout
```

**Issues with synchronous RPC:**
1. **Blocks the caller** - Can't do other work while waiting
2. **Timeout complexity** - Must track timeouts, retries at multiple layers
3. **Resource coupling** - Caller process tied to responder availability
4. **Poor fault tolerance** - Caller crashes if responder is slow/unavailable
5. **DHT lookup blocking** - Must wait for DHT discovery before sending

---

## NATS-Style Request/Reply Pattern

NATS uses an **asynchronous request/reply** pattern with **unique inbox topics**:

```
Request:
  Sender → Direct to Provider (discovered via DHT)
         Includes: reply_to inbox ID

Reply:
  Provider → Direct to Sender's inbox
```

### Key Characteristics

1. **Fire-and-forget sends** - Sender doesn't block on delivery
2. **Reply via inbox** - Response comes back to unique request ID
3. **Timeout at caller** - Only caller tracks timeout
4. **DHT for discovery** - Find providers, then send direct
5. **Direct P2P delivery** - No routing through bootstrap

---

## Correct Architecture for Macula

### Bootstrap Role (IMPORTANT)

Bootstrap nodes are **NOT pub/sub routers**. Their roles are:

1. **DHT Bootstrapping** - Initial node discovery and DHT population
2. **NAT Relay** - Relay traffic when direct P2P fails (symmetric NAT)
3. **Cross-submesh Routing** - Route between isolated network segments

**Bootstrap does NOT:**
- Route pub/sub messages between peers
- Act as a message broker
- Store and forward RPC calls

### Message Flow (Correct)

```
fc01 (Requester)                DHT                          rc05 (Provider)
     │                           │                                │
     │ 1. Find providers for     │                                │
     │    "ping.handler"         │                                │
     │ ─────────────────────────>│                                │
     │                           │                                │
     │<────────────────────────── │ 2. Returns: [{rc05, endpoint}]│
     │                           │                                │
     │                                                            │
     │ 3. Direct QUIC connection to rc05                          │
     │ ═══════════════════════════════════════════════════════════>
     │                                                            │
     │ 4. Send RPC_REQUEST with reply_to inbox_id                 │
     │ ──────────────────────────────────────────────────────────>│
     │                                                            │
     │                                           5. Execute handler
     │                                                            │
     │ 6. Send RPC_REPLY to inbox_id (same connection)            │
     │ <──────────────────────────────────────────────────────────│
     │                                                            │
     │ 7. Process reply, cancel timer                             │
     │                                                            │
```

### When NAT Relay is Used

```
fc01 (Symmetric NAT)     Bootstrap (Relay)              rc05 (Full Cone)
     │                         │                              │
     │ 1. Direct P2P fails     │                              │
     │ ═══════ FAILS ═════════════════════════════════════════>
     │                         │                              │
     │ 2. Fallback to relay    │                              │
     │ ────────────────────────>                              │
     │                         │ 3. Forward to rc05           │
     │                         │ ─────────────────────────────>
     │                         │                              │
     │                         │ 4. Reply via relay           │
     │                         │ <─────────────────────────────
     │ 5. Receive reply        │                              │
     │ <────────────────────────                              │
```

---

## API Design

### Async RPC (New - Default)

```erlang
%% Option 1: Callback-based
macula_rpc:request(Procedure, Args, #{
    callback => fun(Result) -> handle_result(Result) end,
    timeout => 3000
})

%% Option 2: Process message-based
{ok, RequestId} = macula_rpc:request(Procedure, Args, #{timeout => 3000})
%% Later receive:
handle_info({rpc_reply, RequestId, Result}, State) ->
    %% Handle response

%% Option 3: Explicit target (skip DHT lookup)
{ok, RequestId} = macula_rpc:request_to(TargetNodeId, Procedure, Args, Opts)
```

### Sync RPC (Existing - Wrapper)

```erlang
%% Keeps backward compatibility by wrapping async
{ok, Result} = macula_rpc:call(Procedure, Args, #{timeout => 3000})
%% Internally uses request/3 + receive
```

---

## Implementation in macula_rpc_handler.erl

### New State Fields

```erlang
-record(state, {
    %% ... existing fields ...

    %% Pending async requests: #{RequestId => #pending_request{}}
    pending_requests = #{} :: #{binary() => #pending_request{}},

    %% Request ID counter
    request_id_counter = 0 :: non_neg_integer()
}).

-record(pending_request, {
    id :: binary(),
    procedure :: binary(),
    callback :: fun() | {pid, pid()} | {gen_server, term()},
    timer_ref :: reference(),
    sent_at :: integer(),
    target_node :: binary() | undefined
}).
```

### New API Functions

```erlang
%% Async request with callback
-spec request(pid(), binary(), term(), map()) -> {ok, binary()} | {error, term()}.
request(Pid, Procedure, Args, Opts) ->
    gen_server:call(Pid, {request, Procedure, Args, Opts}).

%% Async request to specific node (skip DHT)
-spec request_to(pid(), binary(), binary(), term(), map()) -> {ok, binary()} | {error, term()}.
request_to(Pid, TargetNode, Procedure, Args, Opts) ->
    gen_server:call(Pid, {request_to, TargetNode, Procedure, Args, Opts}).
```

### Request Flow

```erlang
handle_call({request, Procedure, Args, Opts}, _From, State) ->
    %% 1. Generate request ID
    {RequestId, State2} = next_request_id(State),

    %% 2. Extract callback (fun or caller pid)
    Callback = get_callback(Opts, From),

    %% 3. DHT lookup for providers
    case discover_providers(Procedure, State2) of
        {ok, Providers} ->
            %% 4. Send to best provider via macula_peer_connector
            send_request_direct(RequestId, Providers, Procedure, Args, Opts, Callback, State2);
        {error, Reason} ->
            {reply, {error, Reason}, State2}
    end.

send_request_direct(RequestId, [Provider | _], Procedure, Args, Opts, Callback, State) ->
    #{node_id := TargetNode, endpoint := Endpoint} = Provider,

    %% Build request message
    Msg = #{
        type => <<"rpc_request">>,
        request_id => RequestId,
        procedure => Procedure,
        args => encode_args(Args),
        from_node => State#state.node_id,
        timestamp => erlang:system_time(millisecond)
    },

    %% Send directly via peer connector (fire-and-forget)
    macula_peer_connector:send_message(TargetNode, Endpoint, rpc_request, Msg),

    %% Set timeout
    Timeout = maps:get(timeout, Opts, 5000),
    TimerRef = erlang:send_after(Timeout, self(), {request_timeout, RequestId}),

    %% Track pending request
    PendingReq = #pending_request{
        id = RequestId,
        procedure = Procedure,
        callback = Callback,
        timer_ref = TimerRef,
        sent_at = erlang:system_time(millisecond),
        target_node = TargetNode
    },

    PendingRequests = maps:put(RequestId, PendingReq, State#state.pending_requests),
    {reply, {ok, RequestId}, State#state{pending_requests = PendingRequests}}.
```

### Reply Handling

```erlang
%% Handle incoming RPC reply
handle_cast({incoming_rpc_reply, Msg}, State) ->
    RequestId = maps:get(<<"request_id">>, Msg),
    case maps:take(RequestId, State#state.pending_requests) of
        {PendingReq, NewPending} ->
            erlang:cancel_timer(PendingReq#pending_request.timer_ref),
            Result = maps:get(<<"result">>, Msg),
            invoke_callback(PendingReq#pending_request.callback, {ok, Result}),
            {noreply, State#state{pending_requests = NewPending}};
        error ->
            %% Already timed out or duplicate
            {noreply, State}
    end.

invoke_callback({fun, Fun}, Result) -> Fun(Result);
invoke_callback({pid, Pid}, Result) -> Pid ! {rpc_reply, Result};
invoke_callback({gen_server, From}, Result) -> gen_server:reply(From, Result).
```

---

## Message Types

### RPC_REQUEST (0x30)

Sent directly from requester to provider:

```erlang
#{
    type => <<"rpc_request">>,
    request_id => <<16 bytes>>,
    procedure => <<"ping.handler">>,
    args => <<msgpack encoded>>,
    from_node => <<32 bytes node_id>>,
    from_endpoint => <<"ip:port">>,   % For reply routing
    timestamp => 1732940000000
}
```

### RPC_REPLY (0x31)

Sent directly from provider back to requester:

```erlang
#{
    type => <<"rpc_reply">>,
    request_id => <<16 bytes>>,       % Same as request
    result => <<msgpack encoded>>,
    error => undefined | <<binary>>,  % If error
    from_node => <<32 bytes node_id>>,
    timestamp => 1732940000500
}
```

---

## Migration Path

### Phase 1: Add Async API ✅ COMPLETE

- ✅ Added `request/4` and `request_to/5` to `macula_rpc_handler`
- ✅ Keep existing `call/4` as sync wrapper
- ✅ Added `pending_requests` tracking
- ✅ 14 unit tests passing

### Phase 2: Update Demos ✅ COMPLETE

- ✅ Updated `macula_ping_pong` to use async RPC
- ✅ Verified with Docker NAT simulation tests (simple-pingpong)

### Phase 2.5: Async DHT Discovery ✅ COMPLETE (v0.12.2)

- ✅ Async RPC requests on cache miss now initiate DHT discovery
- ✅ Multiple requests for same procedure are queued during discovery
- ✅ All queued requests are dispatched when providers are found
- ✅ Timeout handling notifies all queued callbacks
- ✅ 3 new unit tests (17 total)

**Key Implementation:**
```erlang
%% When cache miss occurs, request ID is returned immediately
%% DHT discovery happens in background
{ok, RequestId} = macula_rpc_handler:request(RpcPid, <<"service">>, Args, #{
    callback => fun(Result) -> handle_result(Result) end,
    dht_timeout => 5000  %% Optional: DHT discovery timeout (default: 3000ms)
}).

%% Multiple requests for same service are queued:
%% - First request starts DHT discovery
%% - Subsequent requests are queued (not sent until discovery completes)
%% - When providers found, all queued requests dispatched to provider
%% - On timeout, all callbacks receive {error, dht_discovery_timeout}
```

### Phase 3: Pull-based Service Discovery ✅ COMPLETE (v0.12.3)

- ✅ Service interests can be configured at init time
- ✅ When connection manager is set, configured services are prefetched
- ✅ Dynamic prefetch via `prefetch_services/2` API
- ✅ Supports binary, atom, and string service names (normalized to binary)
- ✅ 5 new unit tests (22 total)

**Key Implementation:**
```erlang
%% Configure service interests at init time
{ok, RpcPid} = macula_rpc_handler:start_link(#{
    node_id => NodeId,
    realm => Realm,
    peer_id => PeerId,
    service_interests => [<<"ping.handler">>, <<"user.service">>]
}).

%% Or dynamically prefetch at runtime
macula_rpc_handler:prefetch_services(RpcPid, [<<"new.service">>]).

%% Query configured interests
Interests = macula_rpc_handler:get_service_interests(RpcPid).
%% => [<<"ping.handler">>, <<"user.service">>, <<"new.service">>]
```

**Benefits:**
- First RPC call to prefetched service is instant (cache hit)
- Eliminates DHT discovery latency for known services
- Reduces `dht_discovery_pending` errors during startup
- Works with both init-time config and runtime API

### Phase 4: Make Async Default (Future)

- Deprecate sync `call/4` in favor of `request/4`
- Update documentation (ongoing)

---

## Integration Test Results (50-Peer NAT Simulation)

**Test Environment:** Docker NAT simulation with 50 peers across 3 NAT types
**Test Date:** November 30, 2025
**Status:** ✅ PASSED

### Configuration
- **Peers:** 50 total (17 Full Cone, 17 Restricted, 16 Symmetric NAT)
- **Network:** Isolated subnets with NAT routers simulating real-world conditions
- **Demo:** Ping/Pong using async RPC (`macula_ping_pong.erl`)
- **Interval:** 5000ms ping interval, 3000ms timeout

### Results
| Metric | Result |
|--------|--------|
| Total Peers | 50 |
| Successful PONGs | 100% |
| Timeouts | 0 |
| Average RTT | 0-1ms |
| Cross-NAT Communication | ✅ All types communicating |

### Test Commands
```bash
cd docker/nat-test
./run-50-ping-pong.sh rebuild  # Build and start all 50 peers
./run-50-ping-pong.sh stats    # Show RTT statistics
./run-50-ping-pong.sh logs     # Follow ping/pong messages
./run-50-ping-pong.sh stop     # Stop all containers
```

### Sample Output
```
[PingPong fc01] PING -> rc15 (async RPC)
[PingPong fc01] PONG <- rc15 (full_cone): 0ms RTT
[PingPong sy10] PING -> fc01 (async RPC)
[PingPong sy10] PONG <- fc01 (symmetric): 1ms RTT
```

---

## Benefits

1. **Non-blocking** - Caller continues while waiting for reply
2. **Direct P2P** - No routing through bootstrap (except NAT relay)
3. **Scalable** - Bootstrap not bottleneck for RPC traffic
4. **Simple timeouts** - Caller-only timeout tracking
5. **Flexible callbacks** - Support funs, pids, and gen_server replies

---

## Files to Modify

1. **`src/macula_rpc_system/macula_rpc_handler.erl`** - Add async API
2. **`src/macula_rpc_system/macula_rpc.erl`** - High-level API module (new)
3. **`src/macula_ping_pong.erl`** - Update to use async API
4. **`src/macula_gateway.erl`** - Handle RPC_REQUEST for local providers
