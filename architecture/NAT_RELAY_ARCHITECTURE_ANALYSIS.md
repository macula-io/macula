# NAT Relay Architecture Analysis

## Executive Summary

This document analyzed the cross-NAT relay communication and identified multiple architectural issues that were causing PING/PONG failures.

### Issues Identified and Fixed

| Issue | Root Cause | Fix | Status |
|-------|------------|-----|--------|
| Race condition | Reply arrived before pending request stored | Store pending BEFORE send | ✅ FIXED |
| Wrong RPC handler | Lookup used `peer_id` (connection-specific) | Use `node_id` (node-wide) | ✅ FIXED |
| Stream closures | NAT timeout closes streams silently | Cleanup stale streams on send failure | ✅ FIXED |

### Verified Working

After fixes, successful PONG exchanges observed:
```
[PingPong fc06] PONG <- sy16 [full_cone -> symmetric]: 81ms RTT
[PingPong fc06] PONG <- sy05 [full_cone -> symmetric]: 73ms RTT
[PingPong fc07] PONG <- ...
```

## Bug 1: Race Condition in Async Request Flow (FIXED)

**ROOT CAUSE IDENTIFIED**: The replies arrived **BEFORE** the pending request was stored!

The `send_via_gateway()` call blocked for 70-116ms (synchronous QUIC send), and during this time:
1. The request reaches the gateway
2. Gateway forwards to target
3. Target executes handler
4. Reply comes back through the gateway
5. Reply arrives at requester
6. ...but the RPC handler was still blocked in `send_via_gateway()`!

When the send finally completed and the pending request was stored, the reply had already been processed and discarded as "unknown request_id".

**Timeline (Before Fix):**
```
T+0     REQUEST_CREATED (RequestId generated)
T+0-70  send_via_gateway() BLOCKS (synchronous QUIC stream creation + send)
T+10    Gateway receives request
T+11    Gateway forwards to target
T+12    Target executes handler
T+13    Reply sent back
T+14    Gateway receives reply
T+15    Gateway forwards to requester
T+20    Requester receives reply -> "NOT FOUND" (pending not stored yet!)
T+70    SEND_COMPLETE -> NOW stores pending request (TOO LATE!)
T+3070  TIMEOUT_FIRED (request was never matched with reply)
```

**FIX APPLIED**: Store the pending request BEFORE calling send_via_gateway().

**Timeline (After Fix):**
```
T+0     REQUEST_CREATED + STORED (RequestId generated, pending stored immediately)
T+0-70  send_via_gateway() BLOCKS (synchronous QUIC send)
T+20    Reply arrives -> FOUND! (pending was stored at T+0)
T+70    SEND_COMPLETE
```

## Bug 2: RPC Handler Routing by peer_id (FIXED)

**ROOT CAUSE**: RPC handler was registered and looked up using `peer_id` (connection-specific), but replies arrived on a different connection than the one that sent the request.

Each peer has:
- One outbound connection to the bootstrap (peer_id = A)
- One inbound client stream from the bootstrap relay (peer_id = B)

When a reply came back, the connection used `peer_id = B` to look up the RPC handler, but the handler was registered with `peer_id = A`.

**FIX APPLIED**: Use `node_id` instead of `peer_id` for RPC handler registration and lookup.

- `macula_rpc_handler.erl`: Register with `{rpc_handler, Realm, NodeId}`
- `macula_connection.erl`: Lookup with `{rpc_handler, Realm, State#state.node_id}`

## Bug 3: Stream Closures (FIXED)

Many sends failed with `"closed"` error because QUIC streams were closed by NAT timeouts but the gateway still held stale stream references.

**Symptoms:**
```
[TIMING xxx] GW_REPLY_SEND_FAILED: closed
[TIMING xxx] GW_REQ_SEND_FAILED: closed
```

**Root Cause:**
1. Peers connect, streams are stored in gateway
2. NAT routers timeout UDP port mappings (~30s for symmetric NAT)
3. QUIC connection dies silently (no Erlang process exit to trigger cleanup)
4. Gateway still holds invalid stream reference
5. Sends fail with "closed"

**FIX APPLIED:** Implemented stream health monitoring with cleanup on send failure:

- Added `remove_stale_stream/2` to `macula_gateway_clients.erl` (async cast)
- Gateway calls cleanup when `quicer:send/2` returns connection-related errors:
  - `closed`
  - `stream_closed`
  - `connection_closed`
  - `stm_send_error`
- Cleanup removes stale stream from both `client_streams` and `endpoint_to_stream` maps
- Uses async cast to avoid blocking the gateway during send operations
- Allows clients to reconnect with fresh streams

**Key Code Changes:**

1. `macula_gateway_clients.erl`:
   ```erlang
   %% @doc Remove a stale stream for a node when send fails with 'closed'.
   -spec remove_stale_stream(pid(), binary()) -> ok.
   remove_stale_stream(Pid, NodeId) ->
       gen_server:cast(Pid, {remove_stale_stream, NodeId}).
   ```

2. `macula_gateway.erl`:
   ```erlang
   %% Called when forward_via_stream or forward_rpc_reply_via_stream fails
   cleanup_stale_stream(ClientMgr, NodeId, NodeIdHex, Reason) when
           Reason =:= closed;
           Reason =:= stream_closed;
           Reason =:= connection_closed;
           Reason =:= stm_send_error ->
       ?LOG_WARNING("[Gateway] Cleaning up stale stream for ~s (reason: ~p)", [NodeIdHex, Reason]),
       macula_gateway_clients:remove_stale_stream(ClientMgr, NodeId);
   ```

**Tests:** 6 new unit tests in `macula_gateway_clients_tests.erl` (41 total tests passing)

## Current Architecture: 7-Hop Relay

### Sequence Diagram: Cross-NAT PING/PONG (Current Implementation)

```
┌────────┐         ┌────────────┐        ┌───────────┐         ┌────────────┐
│ fc01   │         │ Bootstrap  │        │ Bootstrap │         │   rc05     │
│(Req.)  │         │ Gateway    │        │ macula_   │         │  (Target)  │
│        │         │            │        │ connection│         │            │
└───┬────┘         └─────┬──────┘        └─────┬─────┘         └─────┬──────┘
    │                    │                     │                     │
    │ 1. send_via_gateway│                     │                     │
    │    (rpc_request)   │                     │                     │
    │ ──────────────────>│                     │                     │
    │                    │                     │                     │
    │                    │ 2. handle_rpc_request│                    │
    │                    │    (check target_node)                    │
    │                    │ ─┐                   │                     │
    │                    │  │ should_forward_request() = true        │
    │                    │ <┘                   │                     │
    │                    │                     │                     │
    │                    │ 3. forward_via_stream│                    │
    │                    │    (to rc05's client stream)              │
    │                    │ ────────────────────────────────────────> │
    │                    │                     │                     │
    │                    │                     │  4. macula_connection│
    │                    │                     │     process_message  │
    │                    │                     │     {rpc_request}    │
    │                    │                     │  ─┐                  │
    │                    │                     │   │ execute handler  │
    │                    │                     │  <┘                  │
    │                    │                     │                     │
    │                    │                     │  5. send RPC_REPLY   │
    │                    │                     │     (via stream)     │
    │                    │ <──────────────────────────────────────── │
    │                    │                     │                     │
    │                    │ 6. handle_rpc_reply │                     │
    │                    │    (check to_node)  │                     │
    │                    │ ─┐                  │                     │
    │                    │  │ to_node = fc01   │                     │
    │                    │  │ handle_relayed_  │                     │
    │                    │  │ rpc_reply()      │                     │
    │                    │ <┘                  │                     │
    │                    │                     │                     │
    │ 7. forward_rpc_reply                     │                     │
    │    (to fc01's client stream)             │                     │
    │ <──────────────────│                     │                     │
    │                    │                     │                     │
    │ 8. macula_connection                     │                     │
    │    process_message │                     │                     │
    │    {rpc_reply}     │                     │                     │
    │ ─┐                 │                     │                     │
    │  │ lookup pending  │                     │                     │
    │  │ TIMEOUT!        │                     │                     │
    │ <┘                 │                     │                     │
```

### The 7 Processing Hops

| Hop | Location | Function | Time Est. |
|-----|----------|----------|-----------|
| 1 | fc01 → Gateway | send_via_gateway() | ~5ms |
| 2 | Gateway | handle_rpc_request() + should_forward | ~2ms |
| 3 | Gateway → rc05 | forward_via_stream() | ~5ms |
| 4 | rc05 connection | process_message + execute handler | ~10ms |
| 5 | rc05 → Gateway | send RPC_REPLY | ~5ms |
| 6 | Gateway | handle_rpc_reply() + relayed lookup | ~5ms |
| 7 | Gateway → fc01 | forward_rpc_reply_via_stream() | ~5ms |
| 8 | fc01 connection | process_message rpc_reply | ~2ms |
| **Total** | | | **~39ms** |

**Wait, 39ms should be well under 3000ms timeout!**

## The Real Problem: Multiple Independent Issues

### Issue 1: gen_server Call Blocking

The `macula_rpc_handler:request_to/5` is a `gen_server:call` which blocks the caller:

```erlang
request_to(Pid, TargetNodeId, Procedure, Args, Opts) ->
    gen_server:call(Pid, {async_request_to, TargetNodeId, Procedure, Args, Opts}).
```

This means the ping_pong process is blocked waiting for the gen_server to return `{ok, RequestId}`, not waiting for the actual reply. The timeout is handled internally by the RPC handler.

### Issue 2: Connection Lookup by peer_id, Not node_id

In `macula_connection.erl` line 655:
```erlang
case gproc:lookup_local_name({rpc_handler, State#state.realm, State#state.peer_id}) of
```

The RPC handler is looked up by `peer_id`, but the reply needs to reach the handler that **sent** the request. If there are multiple connections with different peer_ids, the reply might go to the wrong handler!

### Issue 3: Stream Identity Confusion

The peer connects to the bootstrap via `macula_connection`, which creates a stream. The bootstrap stores this stream keyed by `node_id` from the CONNECT message. But:

1. The **outbound** connection from fc01 → bootstrap uses peer_id `outbound_conn_1`
2. The RPC handler is registered with peer_id `outbound_conn_1`
3. But the reply arrives on the **inbound** stream which has a different peer_id
4. The lookup `gproc:lookup_local_name({rpc_handler, realm, inbound_peer_id})` fails!

**This is the root cause: peer_id mismatch between outbound request and inbound reply.**

### Issue 4: Missing Connection Correlation

When fc01 sends a request via its outbound connection to the bootstrap, the request goes through:
- `macula_ping_pong` → `macula_rpc_handler` → `send_via_gateway` → `macula_connection:send_message`

But the **reply** comes back through a DIFFERENT path:
- Bootstrap receives reply → looks up fc01's stream → sends to fc01
- fc01 receives on **gateway's client connection handler**, not the original connection!

## The Architecture Flaw Visualized

```
┌─────────────────────────────────────────────────────────────────────────┐
│                              fc01                                        │
│  ┌──────────────────┐        ┌──────────────────┐                       │
│  │ macula_ping_pong │        │ macula_rpc_handler│                       │
│  │  (calls request_to)       │  (peer_id: peer1) │                       │
│  └────────┬─────────┘        └────────┬─────────┘                       │
│           │                           │                                  │
│           │ request_to()              │                                  │
│           └──────────────────────────>│                                  │
│                                       │                                  │
│  ┌────────────────────────────────────┼──────────────────────────────┐  │
│  │ macula_connection (OUTBOUND)       │                              │  │
│  │ peer_id: outbound_bootstrap        │                              │  │
│  │ Connected to: bootstrap:4433       │                              │  │
│  └────────────────────────────────────┼──────────────────────────────┘  │
│                                       │                                  │
│                              ─────────┼─────────                        │
│                                       │ REQUEST goes OUT                │
│                                       ▼                                  │
│                                                                          │
│  ┌────────────────────────────────────────────────────────────────────┐ │
│  │ macula_gateway_clients (INBOUND client connection)                 │ │
│  │ peer_id: client_stream_fc01                                        │ │
│  │ ◀──────────────────── REPLY comes IN here!                         │ │
│  │                                                                    │ │
│  │ process_message({rpc_reply, ...}) →                                │ │
│  │   gproc:lookup({rpc_handler, realm, client_stream_fc01})           │ │
│  │   → NOT FOUND! (handler is registered with peer_id: peer1)         │ │
│  └────────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────┘
```

## Wait - Looking at the Logs Again

The logs show:
```
[Connection] RECEIVED RPC_REPLY: request_id=..., msg=...
[fc01] ASYNC_REPLY: received request_id=..., pending_count=0
[fc01] ASYNC_REPLY: NOT FOUND request_id=... (already timed out or duplicate)
```

So the reply IS being delivered to the RPC handler! The `[fc01]` prefix comes from `State#state.node_id`, not `peer_id`. The handler IS found.

The issue is `pending_count=0` - the request already timed out.

## Root Cause: Timing Analysis

Let me trace the actual timing issue:

1. **T+0ms**: fc01 sends PING to rc05
2. **T+?ms**: Request travels through relay chain
3. **T+?ms**: rc05 executes handler, sends reply
4. **T+?ms**: Reply travels back through relay chain
5. **T+3000ms**: TIMEOUT fires in fc01's RPC handler
6. **T+?ms**: Reply arrives AFTER timeout

The question is: what's happening between T+0 and when the reply arrives?

## Checking the Actual Bottleneck

Looking at the Docker logs timing would reveal:
- When was the request sent?
- When did bootstrap receive it?
- When did rc05 receive it?
- When did rc05 send reply?
- When did bootstrap receive reply?
- When did fc01 receive reply?
- When did the timeout fire?

**Hypothesis**: The bottleneck is likely in the **startup sequence** where:
1. Peers take time to connect to bootstrap
2. Bootstrap takes time to store client streams
3. First few PING attempts fail because streams aren't ready
4. By the time streams ARE ready, the old requests have timed out

## Optimal Architecture: 2-Hop Relay

The minimum required hops for cross-NAT relay are:

```
┌────────┐         ┌────────────┐         ┌────────┐
│ fc01   │         │ Bootstrap  │         │  rc05  │
│(Req.)  │         │  Gateway   │         │(Target)│
└───┬────┘         └─────┬──────┘         └───┬────┘
    │                    │                    │
    │ 1. RPC_REQUEST     │                    │
    │ ──────────────────>│                    │
    │                    │                    │
    │                    │ 2. RPC_REQUEST     │
    │                    │ ──────────────────>│
    │                    │                    │
    │                    │                    │ (execute)
    │                    │                    │
    │                    │ 3. RPC_REPLY       │
    │                    │ <──────────────────│
    │                    │                    │
    │ 4. RPC_REPLY       │                    │
    │ <──────────────────│                    │
    │                    │                    │
```

**4 messages, 4 network hops** - this is optimal for relay.

The current implementation has the same 4 network hops, but the **processing** in between is what's slow.

## Recommendations

### 1. Simplify Gateway Message Handling

The gateway currently does:
1. Decode message
2. Pattern match on message type
3. Call handler function
4. Handler does its own pattern matching
5. Multiple nested function calls

Should be:
1. Decode message
2. Forward directly based on target

### 2. Reduce gen_server Round-trips

Each gen_server:call adds latency. Consider:
- Using gen_server:cast where possible
- Batching operations
- Direct ETS lookups instead of gen_server calls

### 3. Pre-warm Connection Pool

Ensure streams are established and stored BEFORE any RPC requests are made.

### 4. Add Timing Instrumentation

Add precise timing logs to identify actual bottleneck:
```erlang
T1 = erlang:monotonic_time(microsecond),
%% do work
T2 = erlang:monotonic_time(microsecond),
?LOG_WARNING("[TIMING] operation=~s, duration_us=~p", [Op, T2-T1])
```

### 5. Consider Connection-Local RPC Handler Registration

Instead of:
```erlang
gproc:lookup_local_name({rpc_handler, Realm, PeerId})
```

Use:
```erlang
gproc:lookup_local_name({rpc_handler, Realm, NodeId})
```

Or better: store RPC handler PID directly in connection state.

## Conclusion

The timeout issue is **not** due to network latency (39ms << 3000ms). It's likely due to:
1. Startup timing - streams not ready when first requests arrive
2. Process lookup overhead - multiple gproc lookups per message
3. Message routing complexity - 7 function calls where 2 would suffice

Increasing timeout is a band-aid. The real fix is architectural simplification.
