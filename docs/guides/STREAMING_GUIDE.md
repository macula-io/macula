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
final result. Each session gets its own dedicated QUIC stream, opened for that
session alone — not multiplexed onto the connection's shared control stream the
way an ordinary CALL or a PUBLISH is — so QUIC's own per-stream flow control
paces the sender to the receiver, and a slow or stalled stream cannot
head-of-line-block other RPC/PubSub traffic on the same connection.

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

`Opts` may set `dial_timeout_ms` (default 10_000) for the dial + handshake,
plus the same per-call TLS trust override as `call_station/8`: `verify`,
`expected_node_id`, `pin_tls_cert` (see the [RPC Guide](RPC_GUIDE.md)) — a
fresh dial from `call_stream_station/6` had no way to set these at all
before macula 9.8.0.

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

## Supervised wrappers: `macula_streamer` / `macula_stream_sink`

The bare handler fun above runs in a transient process per inbound
STREAM_OPEN, and a consumer has to hand-write its own `recv/2` loop around
`call_stream/5`'s raw stream pid. `macula_streamer` and `macula_stream_sink`
wrap the same two primitives as proper OTP behaviours, and each publishes
its own `streaming.started_v1` / `streaming.completed_v1` mesh fact — from
its own side's perspective, not deduplicated against the other side's copy.

Provider side — each inbound stream starts one supervised child under a
factory supervisor this module owns. Sending is push-based: once
`handle_open/2` has stashed `self()` somewhere discoverable (a registry, an
ETS table, a message to a known process), any code holding that pid can
call `macula_streamer:send/2,3` and `close/1` on it from outside:

```erlang
-module(log_tailer_provider).
-behaviour(macula_streamer).
-export([init/1, handle_open/2]).

init(Registry) -> {ok, Registry}.

handle_open(#{<<"topic">> := Topic}, Registry) ->
    Registry ! {tailer_ready, Topic, self()},
    {ok, Registry}.
```

```erlang
{ok, _Sup} = macula_streamer:advertise(Pool, Realm, <<"logs.tail">>,
                                       log_tailer_provider, self()).

%% elsewhere, once the provider has announced its pid:
ok = macula_streamer:send(TailerPid, <<"a log line\n">>).
```

Consumer side — `start_link/5,6` opens the stream and drives the `recv/2`
loop in a linked reader process, delivering each chunk to
`Module:handle_chunk/2`:

```erlang
-module(log_tailer).
-behaviour(macula_stream_sink).
-export([init/1, handle_chunk/2, handle_close/2]).

init(_Args) -> {ok, []}.

handle_chunk(Line, Lines) ->
    io:format("~s", [Line]),
    {noreply, [Line | Lines]}.

handle_close(_Reason, _Lines) -> ok.
```

```erlang
{ok, Pid} = macula_stream_sink:start_link(log_tailer, Pool, Realm,
                                          <<"logs.tail">>, []).
```

**`client_stream` providers get the mirror-image receive loop.** Export the
same optional `handle_chunk/2` callback on the PROVIDER-side module and
`macula_streamer` drives a linked-reader loop against its own stream,
symmetric to `macula_stream_sink`'s consumer-side one. A `server_stream`
module that doesn't export it is unaffected — the reader is only spawned
when the callback is present.

```erlang
-module(batch_upload_provider).
-behaviour(macula_streamer).
-export([init/1, handle_open/2, handle_chunk/2]).

init(Parent) -> {ok, {Parent, []}}.

handle_open(_StreamArgs, State) -> {ok, State}.

handle_chunk(Chunk, {Parent, Acc}) ->
    {noreply, {Parent, [Chunk | Acc]}}.
```

```erlang
{ok, _Sup} = macula_streamer:advertise(Pool, Realm, <<"bulk.ingest">>,
                                       batch_upload_provider, self(),
                                       #{mode => client_stream}).
```

**Cancel.** Stopping either wrapper for a non-`normal` reason (a crash, a
`recv` error, a callback returning a non-normal stop) sends the peer an
explicit `macula_stream:abort/3` STREAM_ERROR instead of an ordinary close
or a silent link-crash — the peer can tell a genuine cancellation/failure
from a clean end-of-stream. A `normal` stop closes both sides cleanly, same
as before.

### Direct-dial: `advertise_direct` / `start_link_direct`

The direct-dial counterparts — resolve the procedure's
`procedure_advertisement` from the DHT and dial the serving station in one
hop, instead of routing through the pool's existing links. A
`procedure_advertisement` does not distinguish RPC from streaming, so this
is the exact same resolve-and-trust mechanism as [RPC direct-dial](RPC_GUIDE.md) —
see that guide for the full trust-model writeup.

Provider — `advertise_direct/6,7` does everything `advertise/5,6` does, and
additionally publishes the discoverable record:

```erlang
Identity = macula_identity:generate(),
{ok, _Sup} = macula_streamer:advertise_direct(Pool, Realm, <<"logs.tail">>,
                                              log_tailer_provider, self(),
                                              Identity).
```

Consumer — `start_link_direct/5,6` resolves and dials in one hop:

```erlang
{ok, Pid} = macula_stream_sink:start_link_direct(log_tailer, Pool, Realm,
                                                 <<"logs.tail">>, []).
```

Requires the provider to have advertised via `advertise_direct/6,7`, not
plain `advertise/5,6` — a plain advertise publishes no discoverable record.

---

## Push/upload: `macula_pusher` / `macula_upload`

<p align="center">
  <img src="assets/push_upload.svg" alt="Push-Initiated Content Transfer — macula_pusher / macula_upload" width="100%">
</p>

`client_stream` mode with `macula_feeder`/`macula_download`'s own integrity
machinery bolted on: push a file at a specific, already-known recipient
(not into content-addressed storage for someone to discover and pull
later — that's what `macula_feeder` is for). `macula_manifest:create/2`
chunks and hashes the bytes up front; the manifest rides the stream's
open-time `Args`, not an in-band header chunk; the recipient reassembles
and verifies against it — receiver-side, never sender-trusted — before
replying. No multi-stream parallelism here: that mechanism is
content-sharing-only, built on a wire format `client_stream` doesn't have.

Sender:

```erlang
-module(doc_pusher).
-behaviour(macula_pusher).
-export([init/1, handle_pushed/2]).

init(Parent) -> {ok, Parent}.

handle_pushed(Result, Parent) ->
    Parent ! {pushed, Result},
    {stop, normal, Parent}.
```

```erlang
{ok, Pid} = macula_pusher:start_link(doc_pusher, Pool, Realm,
    <<"bulk.ingest">>, Bytes, self()).
```

Receiver — advertises the procedure once, handles every push sent at it:

```erlang
-module(doc_upload).
-behaviour(macula_upload).
-export([init/1, handle_uploaded/2]).

init(Parent) -> {ok, Parent}.

handle_uploaded(Result, Parent) ->
    Parent ! {uploaded, Result},
    ok.
```

```erlang
{ok, _Sup} = macula_upload:advertise(Pool, Realm, <<"bulk.ingest">>,
    doc_upload, self()).
```

`Result` is `{ok, Mcid, Bytes} | {error, _}` on the receiver's side,
`{ok, Mcid} | {error, _}` on the sender's — the sender never sees the
receiver's own copy of the bytes back, only confirmation that they
verified. `macula_pusher:start_link_direct/5,6` /
`macula_upload:advertise_direct/6,7` are the direct-dial counterparts,
same shape as `macula_stream_sink`/`macula_streamer`'s own (a `Procedure`
resolves via its `procedure_advertisement`, no `Station` parameter — a
push targets a specific advertised procedure, not a named station the way
content-sharing's own direct-dial does). Both inherit Phase 5's
abort-wired cancel: `macula_pusher:cancel/1` reaches the real underlying
stream, not just the local proxy process.

---

## Content streaming

"Content streaming" is the `server_stream` mode applied to a live source: the
provider advertises the stream procedure and its `procedure_advertisement` in the
DHT; a viewer resolves it, dials the serving station directly with
`call_stream_station` (as the diagram shows), and reads frames until the source
stops. Unlike [content sharing](CONTENT_GUIDE.md) there is no fixed size or
`chunk_count` — the stream is open-ended and ordered, riding its own dedicated
QUIC stream (see Overview above), and QUIC's per-stream flow control paces the
source to the viewer's consumption without contending with anything else on the
connection.

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
| `unadvertise_stream(Pool, Realm, Proc)` | provider: stop serving it |
| `macula_streamer:advertise/5,6` | provider: supervised, `streaming.*_v1`-announcing wrapper. Optional `Module:handle_chunk/2` drives a receive loop for `client_stream` mode; optional `Module:handle_eof/1` sets the terminal reply; abort-wired cancel |
| `macula_streamer:advertise_direct/6,7` | provider: as above, and publishes a `procedure_advertisement` for direct-dial |
| `macula_stream_sink:start_link/5,6` | consumer: supervised, `streaming.*_v1`-announcing wrapper; abort-wired cancel |
| `macula_stream_sink:start_link_direct/5,6` | consumer: **direct-dial** — resolve the provider and dial in one hop |
| `macula_pusher:start_link/5,6` / `start_link_direct/5,6` | sender: chunk+hash `Bytes`, push over `client_stream`, deliver the recipient's verified `{ok, Mcid} \| {error, _}` to `handle_pushed/2` |
| `macula_upload:advertise/5,6` / `advertise_direct/6,7` | receiver: accept pushes for `Procedure`, verify against the manifest, deliver `{ok, Mcid, Bytes} \| {error, _}` to `handle_uploaded/2` |
| `send(Stream, Bin)` / `send(Stream, Body, Enc)` | send a chunk (`Enc` = `raw` \| `msgpack`) |
| `recv(Stream)` / `recv(Stream, Timeout)` | read the next `{chunk,_}` / `{data,_}` / `eof` |
| `close_send(Stream)` | half-close your send direction |
| `await_reply(Stream)` / `/2` | consumer: get the provider's final result |
| `set_reply(Stream, Result)` | provider: set the final result |
| `abort(Stream, Code, Message)` | provider: end the stream with an error |
| `close_stream(Stream)` | tear the stream down |
