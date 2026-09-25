# Macula SDK — Content Guide

**Share a blob from your node, fetch it by its content id (MCID).**

![Content Sharing (MCID)](assets/content_sharing.svg)

> **Audience:** applications that share and fetch immutable blobs (files,
> snapshots, artifacts) and want integrity for free, or push a file at a
> specific known recipient. For the wire details (the announcement record,
> the content procedure, the stream bodies and the bounds) see
> [CONTENT_PROTOCOL.md](CONTENT_PROTOCOL.md). For a live, open-ended feed
> instead of a fixed blob, see the [Streaming Guide](../streaming/STREAMING_GUIDE.md).

---

## Overview

Macula content is **content-addressed**: a blob is named by the hash of its
bytes, not by where it lives. That name is an **MCID** (Macula Content ID).

- **Integrity is self-verifying.** The name *is* the hash (or, for larger
  content, a Merkle root over chunk hashes), so fetched bytes are checked
  against the MCID they were asked for. A corrupted or substituted blob fails
  the check by construction, whoever served it.
- **The node that shares content serves it.** A station keeps no content; it
  only relays (design decision D27). Your node keeps what it shares, serves it
  on a procedure of its own, and announces it in the DHT. Content is available
  while the sharing node is online; several nodes sharing the same bytes serve
  the same MCID.

Two ways to move bytes:

- **Share and fetch** — `macula:share_content/3,4` and
  `macula:get_content/3,4`, or their supervised forms `macula_feeder` and
  `macula_download`: anyone who knows the MCID can fetch it.
- **Push** — `macula_pusher` / `macula_upload`: push a file at a specific,
  already-known recipient. See [Push/upload](#push-upload-macula_pusher-macula_upload).

---

## Share and fetch

```erlang
%% On the sharing node:
{ok, Mcid} = macula:share_content(Pool, Realm, Bytes).

%% On any node:
{ok, Bytes} = macula:get_content(Pool, Realm, Mcid).

%% When the content should no longer be offered:
ok = macula:unshare_content(Pool, Realm, Mcid).
```

`share_content` keeps the bytes in the pool's sharer (one per pool), serves
them, and announces their root MCID in the DHT naming the realm, the station
your node is reachable through, and your node's content procedure. It returns
at once; the announcement is renewed while the content is shared, announced
again if your node moves to another station, and made as soon as a station is
connected if none is yet.

Your node serves content on one procedure per realm:

| `share_content` opts | Procedure |
|---|---|
| no `org` | `~<node id hex>/content_v1`, your node's own namespace (no org needed) |
| `#{org => Org}` | `<Org>/content_v1_<node id hex>`, under an org your node holds a delegation for |

`get_content` finds the MCID's announcements, tries the sharers in a random
order through the station each one announced, and verifies every byte against
the MCID before returning. A sharer that is offline or answers wrongly moves
the fetch to the next. It needs no realm key: content verifies itself.

| `get_content` answer | Meaning |
|---|---|
| `{ok, Bytes}` | the content, verified |
| `{error, not_shared}` | no node announces this MCID in this realm |
| `{error, {unavailable, [{Sharer, Reason}]}}` | every announcing node failed, each named with why |
| `{error, invalid_mcid}` | not a tag 2 (SHA-384) content id |

`get_content/4` takes bounds: `max_bytes` (256 MiB by default), `max_chunks`,
`chunk_timeout_ms`, `parallel`.

---

## Single block vs. chunked

`share_content` picks the shape by size, against
`macula_manifest:default_chunk_size/0` (256 KiB):

| Size | Shape | MCID codec |
|---|---|---|
| `=< 256 KiB` | single raw block | `16#55`: `<<2, 16#55, SHA-384(Bytes)>>` |
| `> 256 KiB` | manifest over 256 KiB chunks | `16#56`: the manifest's own content id |

For chunked content the fetcher asks for the manifest first and uses it only
if its MCID, recomputed from its canonical fields, is the one requested, and
only if its size and chunk count fit the caller's bounds. It then asks for
every chunk (four at a time), verifies each against its own chunk MCID,
reassembles, and verifies the whole against the manifest's size and Merkle
root. A tampered or truncated chunk is caught before the caller sees the bytes.

---

## Supervised wrappers: `macula_feeder` / `macula_download`

`macula_feeder` and `macula_download` run a share or a fetch as an OTP
behaviour: `start_link/4,5,6` returns immediately with a pid and delivers the
outcome to your callback. Each publishes `sharing.put_started_v1` /
`sharing.put_completed_v1` (feeder) or `sharing.get_started_v1` /
`sharing.get_completed_v1` (download) mesh facts, carrying
`chunked => true | false`.

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
{ok, Pid} = macula_download:start_link(doc_download, Pool, Realm, Mcid, self()).

%% cancel before the fetch resolves: its streams stop, and
%% sharing.get_completed_v1 carries outcome => cancelled
ok = macula_download:cancel(Pid).
```

`macula_feeder` is the share-side counterpart (`Module:handle_fed/2`). A feeder
cancelled before its share resolves withdraws the share, so a cancel never
leaves content shared behind your back. Embed `macula_feeder_sup` /
`macula_download_sup` (each a `simple_one_for_one` factory) in your own tree to
enumerate or cancel in-flight work.

---

## Push/upload: `macula_pusher` / `macula_upload`

<p align="center">
  <img src="assets/push_upload.svg" alt="Push-Initiated Content Transfer — macula_pusher / macula_upload" width="100%">
</p>

`client_stream` mode (see the [Streaming Guide](../streaming/STREAMING_GUIDE.md))
with content integrity added: push a file at a specific, already-known
recipient, rather than sharing it for anyone to fetch. `macula_manifest:create/2`
chunks and hashes the bytes up front; the manifest rides the stream's
open-time `Args`; the recipient reassembles and verifies against it
(receiver-side, never sender-trusted) before replying. The recipient uses the
manifest only when its MCID, recomputed from its canonical fields, is the MCID
it names; one that isn't is refused when the stream opens.

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
    <<"bulk/ingest">>, Bytes, self()).
```

Receiver, which advertises the procedure once and handles every push sent at it:

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
{ok, _Sup} = macula_upload:advertise(Pool, Realm, <<"bulk/ingest">>,
    doc_upload, self()).
```

`Result` is `{ok, Mcid, Bytes} | {error, _}` on the receiver's side,
`{ok, Mcid} | {error, _}` on the sender's. `macula_pusher:start_link_direct/5,6`
/ `macula_upload:advertise_direct/6,7` are the direct-dial counterparts (a
`Procedure` resolves via its `procedure_advertisement`). `macula_pusher:cancel/1`
reaches the real underlying stream.

---

## When to use content vs. records vs. streaming

| You have | Use |
|---|---|
| An immutable blob to share and fetch by identity | **Content** (`share_content` / `get_content`) |
| A small, signed, TTL'd fact to publish in the DHT | **Records** (`put_record` / `find_records`) |
| An open-ended live feed with no fixed size | **[Streaming](../streaming/STREAMING_GUIDE.md)** (`call_stream`) |

---

## Reference

| Function | Role |
|---|---|
| `macula:share_content/3,4` | share bytes from this node in a realm; returns the root MCID |
| `macula:unshare_content/3` | stop sharing a root: withdraw its announcement, drop the bytes |
| `macula:get_content/3,4` | fetch an MCID from a node that shares it, verified |
| `macula_feeder:start_link/4,5,6` | supervised, fact-announcing share; `cancel/1` withdraws it |
| `macula_download:start_link/4,5,6` | supervised, fact-announcing fetch; `cancel/1` stops it |
| `macula_pusher:start_link/5,6` / `start_link_direct/5,6` | sender: push over `client_stream`, deliver the recipient's verified `{ok, Mcid} \| {error, _}` |
| `macula_upload:advertise/5,6` / `advertise_direct/6,7` | receiver: accept pushes for `Procedure`, verify, deliver `{ok, Mcid, Bytes} \| {error, _}` |
| `macula_manifest:default_chunk_size()` | the single-block / chunked threshold (256 KiB) |

---

## See also

- [CONTENT_PROTOCOL.md](CONTENT_PROTOCOL.md): the announcement record, the
  content procedure, the stream bodies, the bounds, and what a fetcher checks.
- [Streaming Guide](../streaming/STREAMING_GUIDE.md): an open-ended live feed
  instead of a fixed blob.
- [Records Guide](../shared/RECORDS_GUIDE.md): the DHT records content
  announcements are.
