# Macula SDK — Content Guide

**Content-addressed blob storage on the mesh, by MCID.**

![Content Sharing (MCID)](assets/content_sharing.svg)

> **Audience:** applications that store and fetch immutable blobs — files,
> snapshots, artifacts — and want integrity for free, or push a file at a
> specific known recipient. Building something the wrappers below don't fit?
> See [CONTENT_PROTOCOL.md](CONTENT_PROTOCOL.md) for the raw
> `macula:put_content/2` / `get_content/2` primitives, MCID wire format, and
> discovery internals underneath. For a live, open-ended feed instead of a
> fixed blob, see the [Streaming Guide](../streaming/STREAMING_GUIDE.md).

---

## Overview

Macula content is **content-addressed**: a blob is named by the hash of its
bytes, not by where it lives. That name is an **MCID** (Macula Content ID). Two
consequences follow directly, and they are the whole point:

- **Integrity is self-verifying.** The name *is* the hash (or, for larger
  content, a Merkle root over chunk hashes), so a fetched blob can be checked
  against its own MCID — a corrupted or substituted blob fails the check by
  construction.
- **Location stops mattering.** Any host holding the bytes serves the same
  MCID, so hosts are interchangeable and content deduplicates naturally.

Two ways to move bytes:

- **`macula_feeder` / `macula_download`** — store bytes under content
  addressing for anyone who resolves the MCID to fetch, single-block or
  transparently chunked by size. See
  [Supervised wrappers](#supervised-wrappers-macula_feeder-macula_download),
  below.
- **`macula_pusher` / `macula_upload`** — push a file at a specific,
  already-known recipient — not content-addressed storage for someone to
  discover later. See [Push/upload](#push-upload-macula_pusher-macula_upload),
  further down.

Building something either wrapper doesn't fit (custom retry logic,
observability, an SDK for another language)? See
[CONTENT_PROTOCOL.md](CONTENT_PROTOCOL.md) for the raw `put_content/2` /
`get_content/2` primitives underneath, and the 34-byte MCID wire format.

---

## Supervised wrappers: `macula_feeder` / `macula_download`

`put_content/2` and `get_content/2` are plain blocking calls — no
addressable pid to cancel one from outside. `macula_feeder` and
`macula_download` wrap them as proper OTP behaviours: `start_link/4,5`
returns immediately with a pid and delivers the outcome to your callback.
Each publishes its own `sharing.put_started_v1` / `sharing.put_completed_v1`
(feeder) or `sharing.get_started_v1` / `sharing.get_completed_v1` (download)
mesh fact, carrying `chunked => true | false` so an observer can tell
single-block transfers from manifest ones without decoding the MCID itself.
Internally each drives `macula_content_transfer` directly (not a blocking
call in a plain linked worker), so `cancel/1` is a real, peer-visible abort —
see [Real cancel](CONTENT_PROTOCOL.md#real-cancel-macula_content_transfer) in
the Protocol doc for what that reaches down to.

```erlang
-module(doc_download).
-behaviour(macula_download).
-export([init/1, handle_downloaded/2]).

init(Parent) -> {ok, Parent}.

handle_downloaded(Result, Parent) ->
    Parent ! {downloaded, Result},
    {stop, normal, Parent}.
```

```erlang
{ok, Pid} = macula_download:start_link(doc_download, Pool, Realm,
                                       Mcid, self()).

%% cancel before the get resolves — resets the open content stream for
%% real (macula_content_transfer:cancel/1 underneath, since 9.11.1) and
%% publishes sharing.get_completed_v1 with outcome => cancelled
ok = macula_download:cancel(Pid).
```

`macula_feeder` is the symmetric put-side counterpart (`Module:handle_fed/2`
in place of `handle_downloaded/2`). Embed `macula_feeder_sup` /
`macula_download_sup` (each a `simple_one_for_one` factory) in your own
supervision tree to enumerate or cancel in-flight transfers via
`supervisor:which_children/1` / `terminate_child/2`.

Each transfer mints a `share_id`, carried in the `sharing.*_started_v1` mesh
fact — if you only saw the id there, not the pid,
`macula_content_transfer_registry:whereis_share/1` resolves it (see the
Protocol doc's [Real cancel](CONTENT_PROTOCOL.md#real-cancel-macula_content_transfer)
section for the full registry mechanics).

### Direct-dial: `start_link_direct`

`macula_download:start_link_direct/4,5` resolves a chunked MCID's provider
from its `content_announcement` and dials that station directly — no change
needed on the `macula_feeder` side, since `content_announcement` is
published automatically (see
[Discovery](CONTENT_PROTOCOL.md#discovery-who-has-this-mcid) in the Protocol
doc for the mechanics). `macula_feeder:start_link_direct/5,6` is the
put-side counterpart: a PUT has no discovery step, so it takes the target
`Station` (a pubkey) directly instead of resolving one — deliberately
seeding that specific station rather than whichever the pool would pick.

```erlang
{ok, Pid} = macula_download:start_link_direct(doc_download, Pool, Realm,
                                              Mcid, self()),

{ok, FeedPid} = macula_feeder:start_link_direct(doc_feeder, Pool, StationPubkey,
                                                Realm, Bytes, self()).
```

Only chunked content is discoverable this way — a single-block put is never
announced, so `start_link_direct` has nothing to resolve for one.

---

## Single block vs. chunked

`put_content/2` picks the shape for you, by size, against
`macula_manifest:default_chunk_size/0` (256 KiB):

| Size | Shape | MCID codec | Wire calls |
|---|---|---|---|
| `=< 256 KiB` | single block | `16#55` | one `_content.put_block` / `_content.get_block` |
| `> 256 KiB` | chunked manifest | `16#56` | N `_content.put_block` + one `_content.put_manifest`; symmetric on get |

The single-block shape is unchanged since v4.2.7 — same MCID formula
(`<<1, 16#55, BLAKE3(Bytes)>>`), same single RPC round trip. It is not a
special case bolted on top of chunking; a one-chunk manifest's chunk MCID is
*identical* to the single-block MCID, so the two shapes agree at the boundary.

For content over the chunk size, `put_content/2` splits the bytes into
fixed-size chunks, uploads each (BLAKE3-verified by the station), builds a
**manifest** — chunk count, per-chunk offsets/sizes/hashes, and a Merkle
root over the chunk hashes — and uploads that too, returning the manifest's
own MCID (codec `16#56`). `get_content/2` on a manifest MCID fetches the
manifest, then every chunk in order, reassembles, and verifies the whole
against the manifest's size and Merkle root before returning — a tampered
or truncated chunk is caught before the caller ever sees the bytes. A chunk
failure during put stops immediately without uploading the manifest — a
manifest naming missing chunks would resolve but never reassemble, which is
worse than a clean error.

The whole transfer — every block call plus the manifest call for chunked
content — rides one dedicated QUIC stream, opened once and reused for the
sequence, isolating a large blob transfer from other RPC/PubSub traffic on
the same connection. See
[Parallel multi-stream chunk transfer](CONTENT_PROTOCOL.md#parallel-multi-stream-chunk-transfer)
in the Protocol doc if you need more than one stream working a large
transfer at once — that knob is raw-only, not reachable through
`macula_feeder`/`macula_download`.

---

## Push/upload: `macula_pusher` / `macula_upload`

<p align="center">
  <img src="assets/push_upload.svg" alt="Push-Initiated Content Transfer — macula_pusher / macula_upload" width="100%">
</p>

`client_stream` mode (see the [Streaming Guide](../streaming/STREAMING_GUIDE.md))
with `macula_feeder`/`macula_download`'s own integrity machinery bolted on:
push a file at a specific, already-known recipient (not into
content-addressed storage for someone to discover and pull later — that's
what `macula_feeder` is for). `macula_manifest:create/2` chunks and hashes
the bytes up front; the manifest rides the stream's open-time `Args`, not
an in-band header chunk; the recipient reassembles and verifies against it
— receiver-side, never sender-trusted — before replying. No multi-stream
parallelism here: that mechanism is content-sharing-only, built on a wire
format `client_stream` doesn't have.

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

## When to use content vs. records vs. streaming

| You have | Use |
|---|---|
| An immutable blob to store and fetch by identity | **Content** (`put_content` / `get_content`) |
| A small, signed, TTL'd fact to publish in the DHT | **Records** (`put_record` / `find_records`) |
| An open-ended live feed with no fixed size | **[Streaming](../streaming/STREAMING_GUIDE.md)** (`call_stream`) |

Content is for bytes addressed by *what they are*; records are for signed
statements addressed by *who said them*; streaming is for a flow with no end
known in advance.

---

## Reference

| Function | Role |
|---|---|
| `macula_feeder:start_link/4,5` | supervised, `sharing.put_*_v1`-announcing wrapper around `macula_content_transfer`, real `cancel/1` |
| `macula_feeder:start_link_direct/5,6` | **direct-dial** supervised wrapper — names its own target `Station` |
| `macula_download:start_link/4,5` | supervised, `sharing.get_*_v1`-announcing wrapper around `macula_content_transfer`, real `cancel/1` |
| `macula_download:start_link_direct/4,5` | **direct-dial** supervised wrapper — resolves the provider automatically |
| `macula_pusher:start_link/5,6` / `start_link_direct/5,6` | sender: chunk+hash `Bytes`, push over `client_stream`, deliver the recipient's verified `{ok, Mcid} \| {error, _}` to `handle_pushed/2` |
| `macula_upload:advertise/5,6` / `advertise_direct/6,7` | receiver: accept pushes for `Procedure`, verify against the manifest, deliver `{ok, Mcid, Bytes} \| {error, _}` to `handle_uploaded/2` |
| `macula_content_transfer_registry:whereis_share/1` | resolve a transfer's `share_id` (from a mesh fact) to its pid |
| `macula_manifest:default_chunk_size()` | the single-block / chunked threshold (256 KiB) |

See [CONTENT_PROTOCOL.md's Reference](CONTENT_PROTOCOL.md#reference) for the
raw primitives these wrap.

---

## See also

- [CONTENT_PROTOCOL.md](CONTENT_PROTOCOL.md) — the raw primitives underneath:
  MCID wire format, discovery, direct-dial resolution, real
  cancel/pause/resume, parallel multi-stream transfer.
- [Streaming Guide](../streaming/STREAMING_GUIDE.md) — when you need an
  open-ended live feed instead of a fixed blob.
- [RPC Guide](../rpc/RPC_GUIDE.md) — the same direct-dial trust model, applied
  to request/response instead of content.
- [`macula_feeder`](https://hexdocs.pm/macula/macula_feeder.html) /
  [`macula_download`](https://hexdocs.pm/macula/macula_download.html) —
  supervised, fact-announcing wrappers around `put_content/2` and
  `get_content/2`.
