# QUIC Stream Acceptance - ROOT CAUSE IDENTIFIED

**Date:** 2025-11-11
**Status:** ✅ ROOT CAUSE FOUND

## The Problem

Streams created by clients are NOT being delivered as `{quic, new_stream, ...}` events to the server, despite:
- ✅ Successful connection establishment
- ✅ Calling `async_accept_stream(Conn, #{active => true})`
- ✅ Clients successfully creating streams (`Status: connected`)
- ✅ Clients sending data successfully

## Root Cause

**quicer library requires a callback-based architecture that we're not using!**

### How quicer EXPECTS Stream Handling (from `quicer_server_conn_callback.erl`):

1. **Listener** configured with `conn_callback` module option
2. **Connection callback** (`new_conn/3`) spawns dedicated stream acceptor process
3. **Stream acceptor process** (like `quicer_remote_stream`) calls `async_accept_stream()` and waits
4. When stream arrives → delivered to stream acceptor process
5. Stream acceptor calls `stream_callback` module to handle stream

### What We're Doing (WRONG):

1. Listener with NO `conn_callback` specified
2. Gateway process calls `async_accept_stream()` directly
3. No dedicated stream acceptor process
4. **Result**: quicer doesn't know where to deliver stream events!

## Code Evidence

### quicer_server_conn_callback.erl (lines 52-64):
```erlang
new_conn(Conn, #{version := _Vsn}, #{stream_opts := SOpts} = S) ->
    %% Spawn dedicated stream acceptor process
    case quicer_remote_stream:start_link(maps:get(stream_callback, SOpts), Conn, SOpts) of
        {ok, Pid} ->
            ok = quicer:async_handshake(Conn),
            {ok, S#{conn => Conn, streams => [{Pid, accepting}]}};
        {error, _} = Error ->
            Error
    end.
```

### Our macula_quic.erl (line 48):
```erlang
listen(Port, Opts) ->
    %% ... extract options ...
    QuicerOpts = [
        {certfile, CertFile},
        {keyfile, KeyFile},
        {alpn, AlpnProtocols},
        {peer_unidi_stream_count, PeerUnidiStreamCount}
        %% MISSING: {conn_callback, OurCallbackModule}
    ],
    quicer:listen(Port, QuicerOpts).  % ← No conn_callback!
```

## Solution Options

### Option C: Implement quicer Callback Architecture (PROPER FIX)

**Steps:**
1. Create `macula_quic_conn_callback.erl` module implementing `quicer_connection` behavior
2. Create `macula_quic_stream_handler.erl` module for stream processing
3. Add `{conn_callback, macula_quic_conn_callback}` to listener options
4. Add `{stream_opts, #{stream_callback => macula_quic_stream_handler}}` to listener options

**Example Listener Configuration:**
```erlang
QuicerOpts = [
    {certfile, CertFile},
    {keyfile, KeyFile},
    {alpn, AlpnProtocols},
    {peer_unidi_stream_count, PeerUnidiStreamCount},
    {conn_callback, macula_quic_conn_callback},  % NEW!
    {stream_opts, #{
        stream_callback => macula_quic_stream_handler,  % NEW!
        active => true
    }}
],
quicer:listen(Port, QuicerOpts).
```

**Callbacks to Implement:**

`macula_quic_conn_callback.erl`:
```erlang
-module(macula_quic_conn_callback).
-behavior(quicer_connection).

-export([
    init/1,
    new_conn/3,
    connected/3,
    closed/3,
    new_stream/3  % Handles orphan streams
    %% ... other callbacks ...
]).

new_conn(Conn, _ConnProps, State) ->
    %% Spawn stream acceptor
    {ok, Pid} = macula_quic_stream_acceptor:start_link(Conn, State),
    {ok, State#{streams => [Pid]}}.

new_stream(Stream, #{is_orphan := true} = Props, State) ->
    %% Handle orphan stream (no acceptor waiting)
    {ok, Pid} = macula_quic_stream_handler:start_link(Stream, Props),
    {ok, State}.
```

`macula_quic_stream_handler.erl`:
```erlang
-module(macula_quic_stream_handler).
-behavior(gen_server).

%% Called when stream arrives
init([Stream, Props]) ->
    %% Forward stream to gateway
    macula_gateway ! {stream_accepted, Stream, Props},
    {ok, #{stream => Stream}}.

%% Handle stream data
handle_info({quic, Data, Stream, _Props}, State) ->
    %% Process DHT message
    macula_gateway ! {stream_data, Stream, Data},
    {noreply, State}.
```

### Option D: Manual Stream Acceptor (SIMPLER)

Spawn a dedicated stream acceptor process from gateway:

```erlang
handle_accept_result({ok, Conn}, State) ->
    %% Spawn dedicated stream acceptor process
    {ok, Pid} = macula_stream_acceptor:start_link(self(), Conn),
    {noreply, State#{stream_acceptor => Pid}}.
```

Where `macula_stream_acceptor` calls `async_accept_stream()` and forwards events to gateway.

## Recommendation

**Option C** is the proper architectural approach that aligns with quicer's design.
**Option D** is simpler but may have limitations.

## Next Steps

1. Decide between Option C (proper callbacks) vs Option D (manual acceptor)
2. Implement chosen option
3. Test stream reception
4. Document the final working solution

## Historical Context

**Previous Attempts:**
- Option A: Transfer connection ownership → Failed (ownership transfer doesn't help without proper acceptor)
- Option B: Gateway owns connection, call `async_accept_stream()` directly → Failed (no callback architecture)
- Adding `#{active => true}` → Helped but not sufficient alone

**Key Learning:** quicer is callback-driven, not event-loop driven. You can't just call `async_accept_stream()` and expect events in your main process mailbox - you need dedicated acceptor processes following quicer's architecture.
