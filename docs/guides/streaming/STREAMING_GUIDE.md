# Macula SDK — Streaming Guide

**Streaming RPC over the mesh: server-push, client-push, and bidirectional.**

![Content Streaming](assets/content_streaming.svg)

> **Audience:** applications that need more than one request/response — a live
> feed of frames, an upload of many chunks, or a long-lived two-way exchange.
> Available since SDK 3.17. Building something the wrappers below don't fit?
> See [STREAMING_PROTOCOL.md](STREAMING_PROTOCOL.md) for the raw
> `call_stream/5` / `advertise_stream/5` primitives underneath.

---

## Overview

A plain `macula:call/5` is one request and one reply. A **streaming RPC** keeps
the channel open so either side (or both) can send a sequence of chunks before a
final result. Each session gets its own dedicated QUIC stream, opened for that
session alone — not multiplexed onto the connection's shared control stream the
way an ordinary CALL or a PUBLISH is — so QUIC's own per-stream flow control
paces the sender to the receiver, and a slow or stalled stream cannot
head-of-line-block other RPC/PubSub traffic on the same connection.

There are three modes:

| Mode | Who streams | Typical use |
|---|---|---|
| `server_stream` | provider pushes many chunks, consumer reads | **content streaming** — a live feed, a query that yields many rows |
| `client_stream` | consumer pushes many chunks, provider reads | an upload, a batch submit |
| `bidi` | both push concurrently | an interactive session, a duplex protocol |

A stream is a `pid()`. It is **sticky to one link**: if that link dies you get
`{error, peer_down}` and re-open.

Two layers can drive any of this, and **most applications want the supervised
one** — an addressable pid you can monitor and cancel, `streaming.*_v1` mesh
facts around each session: see
[Supervised wrappers](#supervised-wrappers-macula_streamer-macula_stream_sink),
right below.

---

## Supervised wrappers: `macula_streamer` / `macula_stream_sink`

The bare handler fun and the raw stream pid (both covered in
[STREAMING_PROTOCOL.md](STREAMING_PROTOCOL.md)) run per inbound STREAM_OPEN or
require a hand-written `recv/2` loop, with no addressable pid you can
supervise, monitor, or cancel from outside. `macula_streamer` and
`macula_stream_sink` wrap the same two primitives as proper OTP behaviours,
and each publishes its own `streaming.started_v1` / `streaming.completed_v1`
mesh fact — from its own side's perspective, not deduplicated against the
other side's copy.

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
is the exact same resolve-and-trust mechanism as [RPC direct-dial](../rpc/RPC_GUIDE.md) —
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

## Content streaming

"Content streaming" is the `server_stream` mode applied to a live source: the
provider advertises the stream procedure and its `procedure_advertisement` in the
DHT; a viewer resolves it, dials the serving station directly with
`call_stream_station` (see [STREAMING_PROTOCOL.md](STREAMING_PROTOCOL.md)), and
reads frames until the source stops. Unlike [content sharing](../content/CONTENT_GUIDE.md)
there is no fixed size or `chunk_count` — the stream is open-ended and
ordered, riding its own dedicated QUIC stream (see Overview above), and
QUIC's per-stream flow control paces the source to the viewer's consumption
without contending with anything else on the connection.

**Freshness is not optional.** A live source can go away. Treat a `recv` stall or
`{error, peer_down}` as a signal to **re-resolve** the source and re-open, exactly
as a direct-dial caller re-resolves on a dial failure.

---

## Reference

| Function | Role |
|---|---|
| `macula_streamer:advertise/5,6` | provider: supervised, `streaming.*_v1`-announcing wrapper. Optional `Module:handle_chunk/2` drives a receive loop for `client_stream` mode; optional `Module:handle_eof/1` sets the terminal reply; abort-wired cancel |
| `macula_streamer:advertise_direct/6,7` | provider: as above, and publishes a `procedure_advertisement` for direct-dial |
| `macula_stream_sink:start_link/5,6` | consumer: supervised, `streaming.*_v1`-announcing wrapper; abort-wired cancel |
| `macula_stream_sink:start_link_direct/5,6` | consumer: **direct-dial** — resolve the provider and dial in one hop |

For a targeted push at a specific known recipient instead of an open-ended
feed, see [Push/upload](../content/CONTENT_GUIDE.md#push-upload-macula_pusher-macula_upload)
in the Content Guide (`macula_pusher`/`macula_upload` — `client_stream` mode
with content-integrity checking bolted on).

See [STREAMING_PROTOCOL.md's Reference](STREAMING_PROTOCOL.md#reference) for
the raw primitives these wrap, and for local in-process streams (unit tests,
same-node dispatch).

---

## See also

- [STREAMING_PROTOCOL.md](STREAMING_PROTOCOL.md) — the raw primitives
  underneath: `call_stream`/`advertise_stream`, `send`/`recv`, local
  in-process streams.
- [Content Guide](../content/CONTENT_GUIDE.md) — content-addressed blob
  storage, and `macula_pusher`/`macula_upload` for a targeted push.
- [RPC Guide](../rpc/RPC_GUIDE.md) — the same direct-dial trust model, applied
  to request/response instead of streaming.
- [`macula_streamer`](https://hexdocs.pm/macula/macula_streamer.html) /
  [`macula_stream_sink`](https://hexdocs.pm/macula/macula_stream_sink.html) —
  supervised, fact-announcing wrappers around `advertise_stream/5` and
  `call_stream/5`.
