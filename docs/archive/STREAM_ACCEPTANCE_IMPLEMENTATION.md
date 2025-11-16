# QUIC Stream Acceptance Implementation - Status Report

**Date:** 2025-11-11
**Goal:** Implement DHT-based service discovery over QUIC, replacing centralized registry
**Current Status:** ⚠️ BLOCKED - Stream events not being delivered to Erlang processes

## Problem Statement

We're implementing DHT (Distributed Hash Table) based service discovery using Kademlia algorithm over QUIC transport. The architecture requires:

1. Providers send `STORE` messages over QUIC streams to advertise services
2. Clients send `FIND_VALUE` messages over QUIC streams to discover services
3. Registry (DHT node) must accept incoming QUIC streams to receive these messages

**Critical Issue:** QUIC streams created by clients are NOT being received by the server's stream acceptor process, despite successful connection establishment.

## Architecture Implemented

### Stream Acceptor Pattern (Option A - Connection Ownership Transfer)

```
Gateway Process                    Stream Acceptor Process              Client
      |                                    |                              |
      |-- accept connection -------------->|                              |
      |                                    |                              |
      |-- spawn acceptor ----------------->|                              |
      |                                    |                              |
      |-- transfer ownership ------------->|                              |
      |                         (acceptor now owns connection)            |
      |                                    |                              |
      |                                    |<-- async_accept_stream() ----|
      |                                    |                              |
      |                                    |    ❌ NO EVENTS RECEIVED     |
      |                                    |                              |
      |                                    |<==== create stream =========>|
      |                                    |    (Status: connected)       |
      |                                    |                              |
      |                                    |    ❌ NO {quic,new_stream,...}
```

### Files Modified

1. **`/home/rl/work/github.com/macula-io/macula/src/macula_gateway.erl`**
   - Lines 142-148: Added `peer_bidi_stream_count: 100` to listener options
   - Lines 445-463: Modified connection acceptance to spawn acceptor and transfer ownership

2. **`/home/rl/work/github.com/macula-io/macula/src/macula_stream_acceptor.erl`** (NEW FILE)
   - Dedicated process for accepting QUIC streams on a connection
   - Lines 19-30: Initialization with `async_accept_stream()` call
   - Lines 32-69: Passive receive loop waiting for stream events

## What's Working ✅

1. **Connection Establishment**
   ```
   [Gateway] Accepted connection: #Ref<...>
   [Gateway] Stream acceptor started: <0.132.0>
   [Gateway] Connection ownership transferred to acceptor
   ```

2. **Bidirectional Stream Configuration**
   - Listener properly configured with `peer_bidi_stream_count: 100`
   - No more `{quic,peer_needs_streams,...}` messages

3. **Client Stream Creation**
   ```
   [provider2] Stream: #Ref<...>, Status: connected
   [provider2] STORE sent successfully for test.calculator
   ```
   - Clients successfully create streams
   - Stream handshake completes
   - Data sent without errors

4. **Process Architecture**
   - Acceptor process spawned successfully
   - Connection ownership transferred successfully
   - Acceptor calls `async_accept_stream()` without errors

## What's NOT Working ❌

**CRITICAL:** Server NEVER receives stream events!

Expected:
```erlang
{quic, new_stream, Stream, StreamProps}
```

Actual:
```
(no messages received - receive loop hangs indefinitely)
```

## Attempts Made

### Attempt 1: Blocking Accept Loop
```erlang
%% In stream acceptor init
accept_loop(Conn, Gateway) ->
    case quicer:accept_stream(Conn, #{}, 30000) of
        {ok, Stream} -> handle_stream(Stream);
        {error, timeout} -> accept_loop(Conn, Gateway)
    end.
```
**Result:** Timeouts - streams created BEFORE accept_stream() was called

### Attempt 2: Async Accept with Gateway Ownership
```erlang
%% Gateway owns connection, calls async_accept_stream
quicer:async_accept_stream(Conn, #{})
%% Then transfer ownership
quicer:controlling_process(Conn, AcceptorPid)
```
**Result:** No events delivered (ownership transfer may have cleared async accept registration)

### Attempt 3: Connection Active Mode
```erlang
%% Try to set connection to active mode
quicer:setopt(Conn, active, true)
```
**Result:** `{error, param_error}` - active mode not supported on connections

### Attempt 4: Async Accept After Ownership Transfer (CURRENT)
```erlang
%% Gateway transfers ownership first
quicer:controlling_process(Conn, AcceptorPid)
%% Then acceptor calls async_accept_stream
quicer:async_accept_stream(Conn, #{})
```
**Result:** No stream events received, despite:
- Successful async_accept_stream() call
- Client successfully creating streams
- No errors logged

## Key Findings

1. **Race Condition Eliminated**
   - Initially, clients created streams before server called accept
   - Adding bidirectional stream limit configuration solved `peer_needs_streams` messages
   - But streams still not being accepted

2. **Connection vs Stream Ownership**
   - Stream events delivered to connection owner
   - Successfully transferred connection ownership
   - But events still not arriving

3. **Active Mode**
   - `active: true` only works on streams, not connections
   - Connections must use `async_accept_stream()` or blocking `accept_stream()`

4. **Client Side Works**
   - `quicer:start_stream()` succeeds
   - Stream status shows `connected`
   - Data can be sent without errors

## Testing Evidence

**Log file:** `/tmp/test_with_bidi_streams.log`

Key observations:
```
macula-registry  | [StreamAcceptor] Started for connection #Ref<...>
macula-registry  | [StreamAcceptor] Async stream acceptance registered
macula-provider2 | [provider2] Stream: #Ref<...>, Status: connected
macula-provider2 | [provider2] STORE sent successfully
```

But NO `[StreamAcceptor] STREAM RECEIVED` messages ever appear.

## Potential Root Causes

1. **Missing Initialization**
   - quicer may require additional listener or connection setup we're missing
   - Examples in quicer repo may show required steps

2. **Timing Issue**
   - Despite async_accept_stream(), there may still be a timing window
   - Streams might be "accepted" at protocol level but events not queued

3. **Library Bug**
   - Possible issue in quicer's stream event delivery mechanism
   - May need to consult quicer maintainers

4. **Configuration Missing**
   - Listener or connection options that enable stream event delivery
   - Check quicer examples for required settings

## Next Steps

### Immediate Actions

1. **Search quicer Examples**
   ```bash
   cd /home/rl/work/github.com/macula-io/macula/_build/default/lib/quicer
   grep -r "new_stream" examples/
   ```
   Look for:
   - How other projects accept streams
   - Required initialization steps
   - Listener/connection options

2. **Enable MsQuic Tracing**
   - Set environment variable to enable MsQuic debug logging
   - May reveal what's happening at protocol level
   - Check if streams are being accepted but events not delivered

3. **Try Option B: Event Delegation**
   - Keep gateway as connection owner
   - Gateway receives stream events
   - Gateway forwards to acceptor via message passing
   - More coupling but may work around quicer issue

### Alternative Approach: Event Delegation (Option B)

```erlang
%% Gateway owns connection, receives events
handle_info({quic, new_stream, Stream, Props}, State) ->
    %% Find acceptor for this connection
    AcceptorPid = get_acceptor_for_connection(Stream),
    %% Forward stream to acceptor
    AcceptorPid ! {stream_accepted, Stream, Props},
    {noreply, State}.
```

**Pros:**
- No ownership transfer complexity
- Simpler message flow
- May work around quicer event delivery issues

**Cons:**
- Gateway must track acceptors
- More coupling between gateway and acceptors
- Gateway becomes bottleneck for stream events

### Longer Term

1. **Consult quicer Maintainers**
   - Open GitHub issue with minimal reproduction
   - Ask about stream event delivery requirements
   - May be known issue or missing documentation

2. **Consider Alternative QUIC Libraries**
   - If quicer fundamentally doesn't support our use case
   - Evaluate other Erlang QUIC implementations
   - May need to wait for quicer improvements

## Code References

### macula_gateway.erl:142-148
```erlang
ListenOpts = [
    {cert, CertFile},
    {key, KeyFile},
    {alpn, ["macula"]},
    {peer_unidi_stream_count, 3},
    {peer_bidi_stream_count, 100}  % Allow clients to create bidirectional streams
],
```

### macula_gateway.erl:445-463
```erlang
handle_accept_result({ok, Conn}, State) ->
    io:format("[Gateway] Accepted connection: ~p~n", [Conn]),
    %% Spawn dedicated stream acceptor process for this connection
    case macula_stream_acceptor:start_link(Conn, self()) of
        {ok, AcceptorPid} ->
            io:format("[Gateway] Stream acceptor started: ~p~n", [AcceptorPid]),
            %% Transfer connection ownership to the acceptor so it receives stream events
            %% The acceptor will call async_accept_stream() after receiving ownership
            case quicer:controlling_process(Conn, AcceptorPid) of
                ok ->
                    io:format("[Gateway] Connection ownership transferred to acceptor~n");
                {error, OwnershipErr} ->
                    io:format("[Gateway] ERROR: Failed to transfer ownership: ~p~n", [OwnershipErr])
            end;
        {error, AcceptorErr} ->
            io:format("[Gateway] WARNING: Failed to start stream acceptor: ~p~n", [AcceptorErr])
    end,
    self() ! accept,
    {noreply, State};
```

### macula_stream_acceptor.erl:19-30
```erlang
%% @doc Initialize the stream acceptor loop.
init(Conn, Gateway) ->
    io:format("[StreamAcceptor] Started for connection ~p, gateway ~p~n", [Conn, Gateway]),
    %% Register interest in incoming streams
    case quicer:async_accept_stream(Conn, #{}) of
        {ok, Conn} ->
            io:format("[StreamAcceptor] Async stream acceptance registered~n"),
            receive_loop(Conn, Gateway);
        {error, AcceptErr} ->
            io:format("[StreamAcceptor] ERROR: Failed to register async accept: ~p~n", [AcceptErr]),
            receive_loop(Conn, Gateway)
    end.
```

### macula_stream_acceptor.erl:32-69
```erlang
%% @doc Receive stream events in a passive loop.
receive_loop(Conn, Gateway) ->
    receive
        {quic, new_stream, Stream, _StreamProps} ->
            io:format("[StreamAcceptor] ========================================~n"),
            io:format("[StreamAcceptor] STREAM RECEIVED: ~p~n", [Stream]),
            io:format("[StreamAcceptor] ========================================~n"),

            %% Set stream to active mode for automatic data delivery
            case quicer:setopt(Stream, active, true) of
                ok ->
                    io:format("[StreamAcceptor] Stream set to active mode~n"),
                    %% Transfer ownership to gateway
                    case quicer:controlling_process(Stream, Gateway) of
                        ok ->
                            io:format("[StreamAcceptor] Stream ownership transferred to gateway~n"),
                            %% Notify gateway about new stream
                            Gateway ! {stream_accepted, Stream},
                            %% Continue receiving more streams
                            receive_loop(Conn, Gateway);
                        {error, TransferErr} ->
                            io:format("[StreamAcceptor] ERROR: Failed to transfer stream ownership: ~p~n", [TransferErr]),
                            quicer:close_stream(Stream),
                            receive_loop(Conn, Gateway)
                    end;
                {error, SetOptErr} ->
                    io:format("[StreamAcceptor] ERROR: Failed to set stream active: ~p~n", [SetOptErr]),
                    quicer:close_stream(Stream),
                    receive_loop(Conn, Gateway)
            end;

        {quic, closed, Conn, _Flags} ->
            io:format("[StreamAcceptor] Connection closed, exiting~n"),
            ok;

        Other ->
            io:format("[StreamAcceptor] Unexpected message: ~p~n", [Other]),
            receive_loop(Conn, Gateway)
    end.
```

## Environment

- **Erlang/OTP:** 27
- **quicer:** 0.2.15 (wraps Microsoft MsQuic v2.3.8)
- **OS:** Linux (Docker Alpine)
- **Test Setup:** Multi-node Docker Compose with 1 registry + 3 providers + 1 client

## Conclusion

The architecture and implementation are sound - we have properly:
- Configured bidirectional streams
- Implemented dedicated stream acceptor processes
- Transferred connection ownership
- Called async_accept_stream() correctly

However, **QUIC stream events are simply not being delivered** to the Erlang process that owns the connection, despite clients successfully creating streams and sending data.

This appears to be either:
1. A missing initialization step in how we're using quicer
2. A timing/ordering issue with how events are registered
3. A potential bug or limitation in quicer itself

**Recommended Next Action:** Search quicer repository examples for working stream acceptance patterns and compare with our implementation to identify any missing steps.
