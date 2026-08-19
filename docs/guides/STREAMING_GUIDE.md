# Macula SDK — Streaming Guide

**Streaming RPC over the mesh: server-push, client-push, and bidirectional.**

![Content Streaming](assets/content_streaming.svg)

> **Audience:** applications that need more than one request/response — a live
> feed of frames, an upload of many chunks, or a long-lived two-way exchange.
> Available since SDK 3.17.

---

## Overview

A plain `macula:call/5` is one request and one reply. A **streaming RPC** keeps
the channel open so either side (or both) can send a sequence of chunks before a
final result, over an ordered QUIC stream with per-stream flow control pacing the
sender to the receiver.

Two ways to open one, mirroring unary RPC:

- **`call_stream/5`** — opens on the pool's own healthy link; the station routes
  the STREAM_OPEN to whichever connection advertised the procedure. Good when you
  don't know or care which station serves it.
- **`call_stream_station/6`** (direct-dial) — dials a *specific* station and opens
  the stream there in one hop, exactly like `call_station/6` for unary RPC. Use it
  after resolving a provider's `procedure_advertisement` and `station_endpoint` in
  the DHT (see the [RPC Guide](RPC_GUIDE.md)), so a stream reaches its provider the
  same way a unary call does.

```erlang
{ok, Stream} = macula:call_stream_station(Pool, StationUrl, Realm, Procedure,
                                          Args, #{}).
```

`Opts` may set `dial_timeout_ms` (default 10_000) for the dial + handshake.

There are three modes:

| Mode | Who streams | Typical use |
|---|---|---|
| `server_stream` | provider pushes many chunks, consumer reads | **content streaming** — a live feed, a query that yields many rows |
| `client_stream` | consumer pushes many chunks, provider reads | an upload, a batch submit |
| `bidi` | both push concurrently | an interactive session, a duplex protocol |

A stream is a `pid()`. It is **sticky to one link**: if that link dies you get
`{error, peer_down}` and re-open.

---

## Consumer side

### server_stream — read a feed

```erlang
{ok, Stream} = macula:call_stream(Pool, Realm, <<"live.feed">>, Request, #{}),
loop(Stream).

loop(Stream) ->
    case macula:recv(Stream) of
        {chunk, Bin}  -> handle(Bin), loop(Stream);   %% raw bytes
        {data, Term}  -> handle(Term), loop(Stream);  %% decoded (msgpack)
        eof           -> ok;                           %% source stopped
        {error, R}    -> {error, R}                    %% e.g. peer_down -> re-open
    end.
```

`recv/1` blocks for the next chunk; `recv/2` takes a timeout. `eof` means the
source closed the stream cleanly.

### client_stream — push then await the result

```erlang
{ok, Stream} = macula:call_stream(Pool, Realm, <<"bulk.ingest">>, Meta,
                                  #{mode => client_stream}),
[ok = macula:send(Stream, Chunk) || Chunk <- Chunks],
ok = macula:close_send(Stream),               %% signal "no more input"
{ok, Result} = macula:await_reply(Stream).    %% the provider's final reply
```

`send/2` sends raw bytes; `send/3` takes an encoding (`raw` | `msgpack`).
`close_send/1` half-closes your direction; `await_reply/1,2` returns the
provider's single final result.

---

## Provider side

Advertise a streaming procedure with a mode and a `fun(Stream, Args)` handler.
The handler drives the stream with the same `send` / `recv` primitives, and ends
it with `set_reply` (a final result) or `abort` (an error).

```erlang
%% server_stream: push N chunks, then CLOSE — that is what produces `eof'
%% for a consumer looping on `recv'. `set_reply' is for client_stream /
%% bidi (see below); a pure push-only server_stream does not use it.
ok = macula:advertise_stream(
       Pool, Realm, <<"live.feed">>, server_stream,
       fun(Stream, _Args) ->
           lists:foreach(fun(Frame) -> macula:send(Stream, Frame) end, frames()),
           macula:close_stream(Stream)
       end),

%% client_stream: drain the consumer's chunks, then reply
ok = macula:advertise_stream(
       Pool, Realm, <<"bulk.ingest">>, client_stream,
       fun(Stream, _Args) ->
           N = drain(Stream, 0),
           macula:set_reply(Stream, #{ingested => N})
       end),

drain(Stream, N) ->
    case macula:recv(Stream) of
        {chunk, Bin} -> store(Bin), drain(Stream, N + 1);
        eof          -> N;
        {error, _}   -> N
    end.
```

> **`close_stream` vs. `set_reply` — do not mix them for `server_stream`.**
> `close_stream/1` is what makes a consumer's `recv` loop see `eof`.
> `set_reply/2` only resolves `await_reply/1,2`; it does **not** close the
> stream. A `server_stream` handler that calls `set_reply` without also
> closing leaves a consumer's `recv`-until-`eof` loop waiting forever — use
> `close_stream` for a pure push, and reserve `set_reply` + `await_reply`
> for `client_stream` / `bidi`, where the consumer already knows to stop
> sending and ask for the result instead of draining chunks.

Abort with a BOLT#4-style code and message when something goes wrong:

```erlang
macula:abort(Stream, <<"0F">>, <<"source unavailable">>).
```

---

## Content streaming

"Content streaming" is the `server_stream` mode applied to a live source: the
provider advertises the stream procedure and its `procedure_advertisement` in the
DHT; a viewer resolves it, dials the serving station directly with
`call_stream_station` (as the diagram shows), and reads frames until the source
stops. Unlike [content sharing](CONTENT_GUIDE.md) there is no fixed size or
`chunk_count` — the stream is open-ended and ordered, and QUIC flow control paces
the source to the viewer's consumption.

**Freshness is not optional.** A live source can go away. Treat a `recv` stall or
`{error, peer_down}` as a signal to **re-resolve** the source and re-open, exactly
as a direct-dial caller re-resolves on a dial failure.

---

## Local (in-process) streams

`macula:open_stream/3,4`, `macula:advertise_stream/2,3`, and `call_stream/2,3`
drive streams **inside one BEAM** (no mesh), backed by `macula_stream_local`.
They are for unit tests and same-node dispatch. The pool forms
(`call_stream/5`, `advertise_stream/5`) are the ones that go over the mesh.

---

## Reference

| Function | Role |
|---|---|
| `call_stream(Pool, Realm, Proc, Args, Opts)` | consumer: open a stream on the pool's own link (`Opts` may set `mode`) |
| `call_stream_station(Pool, Station, Realm, Proc, Args, Opts)` | consumer: **direct-dial** — dial `Station` and open the stream there in one hop |
| `advertise_stream(Pool, Realm, Proc, Mode, Handler)` | provider: serve a streaming procedure |
| `send(Stream, Bin)` / `send(Stream, Body, Enc)` | send a chunk (`Enc` = `raw` \| `msgpack`) |
| `recv(Stream)` / `recv(Stream, Timeout)` | read the next `{chunk,_}` / `{data,_}` / `eof` |
| `close_send(Stream)` | half-close your send direction |
| `await_reply(Stream)` / `/2` | consumer: get the provider's final result |
| `set_reply(Stream, Result)` | provider: set the final result |
| `abort(Stream, Code, Message)` | provider: end the stream with an error |
| `close_stream(Stream)` | tear the stream down |
