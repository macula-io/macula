# QUIC Stream Acceptance - Option B Status Report

**Date:** 2025-11-11
**Implementation:** Option B (Event Delegation - Gateway owns connection and receives streams)
**Status:** ⚠️ STREAMS STILL NOT RECEIVED

## Summary

Switched from Option A (ownership transfer) to Option B (event delegation). The gateway now retains connection ownership and calls `async_accept_stream()` successfully, but stream events are STILL not being delivered to the Erlang process.

## Implementation Details - Option B

###File Modified: `/home/rl/work/github.com/macula-io/macula/src/macula_gateway.erl`

**1. Connection Acceptance (Lines 454-466)**
```erlang
%% @doc Handle successful connection acceptance.
%% Using Option B: Gateway keeps connection ownership and handles streams directly.
handle_accept_result({ok, Conn}, State) ->
    io:format("[Gateway] Accepted connection: ~p~n", [Conn]),
    %% Register interest in incoming streams - this is REQUIRED for stream events to be delivered!
    case quicer:async_accept_stream(Conn, #{}) of
        {ok, Conn} ->
            io:format("[Gateway] Registered for stream events on connection~n");
        {error, AcceptErr} ->
            io:format("[Gateway] WARNING: Failed to register for streams: ~p~n", [AcceptErr])
    end,
    %% Gateway keeps ownership - streams will arrive as {quic, new_stream, ...} messages
    self() ! accept,
    {noreply, State};
```

**2. New Stream Handler (Lines 216-231)**
```erlang
%% Handle new stream from peer (Option B: gateway receives orphan streams)
handle_info({quic, new_stream, Stream, _Props}, State) ->
    io:format("[Gateway] ========================================~n"),
    io:format("[Gateway] NEW STREAM RECEIVED!~n"),
    io:format("[Gateway] Stream: ~p~n", [Stream]),
    io:format("[Gateway] ========================================~n"),

    %% Set stream to active mode so we receive data automatically
    case quicer:setopt(Stream, active, true) of
        ok ->
            io:format("[Gateway] Stream set to active mode~n"),
            {noreply, State};
        {error, SetOptErr} ->
            io:format("[Gateway] ERROR: Failed to set stream active: ~p~n", [SetOptErr]),
            quicer:close_stream(Stream),
            {noreply, State}
    end;
```

**3. File Deleted:**
- `/home/rl/work/github.com/macula-io/macula/src/macula_stream_acceptor.erl` (no longer needed)

## What's Working ✅

1. **Code Deployment**
   ```
   [Gateway] Registered for stream events on connection
   ```
   - NEW message confirms Option B code is running
   - Replaces old "[Gateway] Waiting for stream events from peer"

2. **async_accept_stream() Success**
   - Gateway calls `quicer:async_accept_stream(Conn, #{})` after accepting connection
   - Function returns `{ok, Conn}` - no errors

3. **Client Side Success**
   ```
   [provider3] Stream: #Ref<...>, Status: connected
   [provider3] STORE sent successfully for test.calculator
   ```
   - Clients create streams without errors
   - Stream status shows `connected`
   - Data transmission reports success

4. **Connection Establishment**
   ```
   [Gateway] Accepted connection: #Ref<0.3081421303.2419982336.23865>
   ```
   - QUIC connections established successfully
   - Gateway owns connections (no ownership transfer)

## What's NOT Working ❌

**CRITICAL ISSUE:** Gateway NEVER receives `{quic, new_stream, ...}` events!

**Expected:**
```erlang
{quic, new_stream, Stream, StreamProps}
```

**Actual:**
- No stream events in logs
- Handler at lines 216-231 NEVER executes
- No `[Gateway] NEW STREAM RECEIVED!` messages
- No unhandled `{quic, ...}` messages in catch-all handler

## Comparison with quicer Source Code

### quicer_stream.erl Pattern (WORKS)
```erlang
%% In init:
case quicer:async_accept_stream(Conn, StreamOpts) of
    {ok, Conn} ->
        ?tp(new_stream_accept, #{module => ?MODULE, conn => Conn}),
        {ok, InitState#{stream => undefined, ...}};
...

%% In handle_info:
handle_info(
    {quic, new_stream, Stream, #{flags := _Flags, is_orphan := false} = Props},
    State) ->
    %% Process stream
```

### Our Pattern (DOESN'T WORK)
```erlang
%% In handle_accept_result:
case quicer:async_accept_stream(Conn, #{}) of
    {ok, Conn} ->
        io:format("[Gateway] Registered for stream events on connection~n");
...

%% In handle_info:
handle_info({quic, new_stream, Stream, _Props}, State) ->
    %% Process stream (NEVER CALLED!)
```

**Key Difference:** quicer_stream uses `is_orphan := false` pattern match - maybe we should too?

## Testing Evidence

**Latest Test:** `/tmp/test_final_option_b.log`

### Registry Logs
```
macula-registry | [Gateway] Accepted connection: #Ref<0.3081421303.2419982336.23865>
macula-registry | [Gateway] Registered for stream events on connection  ← NEW!
```

### Provider Logs
```
macula-provider3 | [provider3] Connected to registry
macula-provider3 | [provider3] Sending STORE to DHT for service test.calculator
macula-provider3 | [provider3]   Stream: #Ref<0.1963066847.3762421792.173863>, Status: connected
macula-provider3 | [provider3] STORE sent successfully for test.calculator
```

### What's Missing
```
macula-registry | [Gateway] NEW STREAM RECEIVED!  ← NEVER APPEARS!
```

## Potential Root Causes

### 1. Pattern Matching Issue
- quicer source uses `#{is_orphan := false}` in stream props pattern
- Our handler doesn't match on `is_orphan` flag
- Maybe streams ARE arriving but with different props shape?
- **Action:** Add logging to catch-all handler to see ALL messages

### 2. Listener Configuration Missing
- Current listener config:
  ```erlang
  {peer_unidi_stream_count, 3},
  {peer_bidi_stream_count, 100}
  ```
- Maybe missing some option that enables stream event delivery?
- **Action:** Compare with quicer example listener configurations

### 3. Connection State Issue
- Maybe connection needs to be in a specific state for stream events?
- Maybe missing a handshake completion step?
- **Action:** Check `quicer:getstat(Conn)` to inspect connection state

### 4. quicer Library Limitation
- Possible bug or undocumented requirement in quicer
- Stream events may only work with callback-based approach
- **Action:** Search quicer GitHub issues for similar problems

### 5. Registration Timing
- Maybe `async_accept_stream()` needs to be called at a different point?
- Maybe needs to be called BEFORE connection handshake completes?
- **Action:** Try calling `async_accept_stream()` on listener instead of connection

## Next Steps

### Immediate Investigation

1. **Add comprehensive logging to see ALL incoming messages:**
   ```erlang
   handle_info(Info, State) ->
       io:format("[Gateway] RECEIVED MESSAGE: ~p~n", [Info]),
       case Info of
           {quic, _, _, _} = QuicMsg ->
               io:format("[Gateway] !!! QUIC MESSAGE DETECTED: ~p~n", [QuicMsg]);
           _ -> ok
       end,
       %% Continue with normal handling
   ```

2. **Try pattern matching on is_orphan flag:**
   ```erlang
   handle_info({quic, new_stream, Stream, #{is_orphan := IsOrphan} = Props}, State) ->
       io:format("[Gateway] STREAM with is_orphan=~p~n", [IsOrphan]),
       ...
   ```

3. **Check connection state after async_accept_stream:**
   ```erlang
   case quicer:async_accept_stream(Conn, #{}) of
       {ok, Conn} ->
           io:format("[Gateway] Connection state: ~p~n", [quicer:getstat(Conn)]),
           ...
   ```

4. **Try calling async_accept_stream on Listener instead:**
   ```erlang
   %% In start_quic_listener after macula_quic:listen
   quicer:async_accept_stream(Listener, #{})
   ```

### Alternative Approaches if Current Fails

**Option C: Dedicated Stream Acceptor with Callback Module**
- Implement a proper `quicer_remote_stream` callback module
- Use quicer's built-in callback mechanism instead of direct async calls
- More aligned with quicer's expected usage patterns

**Option D: Poll-Based Approach**
- Instead of waiting for async events, periodically call `accept_stream/3` with timeout
- More CPU intensive but might work around event delivery issues

**Option E: Upstream Fix**
- If quicer has a bug, contribute a fix or workaround
- Open GitHub issue with minimal reproduction case

## Conclusion

Option B is properly implemented according to the quicer API documentation:
- ✅ Gateway owns connection
- ✅ `async_accept_stream()` called successfully
- ✅ No errors reported
- ✅ Clients create streams successfully

However, **stream events are simply not being delivered** to the gateway process. This suggests either:
1. A missing step or configuration in our implementation
2. A subtle API usage requirement not captured in documentation
3. A bug or limitation in the quicer library itself

The fact that client-side stream creation succeeds but server-side acceptance fails points to a server-side event registration or delivery issue within quicer's internals.

**Recommended Action:** Comprehensive logging and experimentation to identify why events aren't arriving, potentially escalating to quicer maintainers if no solution found.
