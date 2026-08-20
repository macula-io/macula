# Macula SDK — Content Guide

**Content-addressed blob storage on the mesh, by MCID.**

![Content Sharing (MCID)](assets/content_sharing.svg)

> **Audience:** applications that store and fetch immutable blobs — files,
> snapshots, artifacts — and want integrity for free. Single-block storage
> since SDK 4.2.7; chunked content, discovery, and direct-host fetch since
> 8.10.0. For a live, open-ended feed instead of a fixed blob, see the
> [Streaming Guide](STREAMING_GUIDE.md).

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

```erlang
{ok, MCID}  = macula:put_content(Pool, Bytes),
{ok, Bytes} = macula:get_content(Pool, MCID).
```

`put_content/2` stores `Bytes` and returns its MCID — transparently as one
block for small content, or as a chunked, Merkle-verified manifest for large
content (see below). `get_content/2` fetches the bytes back for either shape,
or `{error, not_found}` if no reachable host holds a copy. Integrity is
verified before the bytes are returned, so the caller does not re-verify.

---

## MCID format

An MCID is a **34-byte** binary:

```
<<Version:8, Codec:8, Hash:32/binary>>
    1         1        32
```

- byte 0 — version (`1`)
- byte 1 — codec: `16#55` (raw) for a single block, `16#56` (manifest) for
  chunked content
- bytes 2..33 — the 32-byte hash: BLAKE3 of the bytes (raw), or BLAKE3 over a
  canonical encoding of the manifest's metadata (manifest — see below)

Because the MCID is derived purely from the content, storing identical bytes
always yields the same MCID — that is what makes it a content address.

---

## Supervised wrappers: `macula_feeder` / `macula_download`

`put_content/2` and `get_content/2` are plain blocking calls — no
addressable pid to cancel one from outside. `macula_feeder` and
`macula_download` wrap them as proper OTP behaviours: `start_link/4,5`
returns immediately with a pid, runs the put/get in a linked worker, and
delivers the outcome to your callback. Each publishes its own
`sharing.put_started_v1` / `sharing.put_completed_v1` (feeder) or
`sharing.get_started_v1` / `sharing.get_completed_v1` (download) mesh
fact, carrying `chunked => true | false` so an observer can tell single-block
transfers from manifest ones without decoding the MCID itself.

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

%% cancel before the get resolves — publishes sharing.get_completed_v1
%% with outcome => cancelled
ok = macula_download:cancel(Pid).
```

`macula_feeder` is the symmetric put-side counterpart (`Module:handle_fed/2`
in place of `handle_downloaded/2`). Embed `macula_feeder_sup` /
`macula_download_sup` (each a `simple_one_for_one` factory) in your own
supervision tree to enumerate or cancel in-flight transfers via
`supervisor:which_children/1` / `terminate_child/2`.

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

For content over the chunk size, `put_content/2`:

1. splits `Bytes` into fixed-size chunks (`macula_manifest:create/1`);
2. uploads each chunk via `_content.put_block` (BLAKE3-verified by the station,
   same as single-block);
3. builds a **manifest** — chunk count, per-chunk offsets/sizes/hashes, and a
   Merkle root over the chunk hashes — and uploads it via `_content.put_manifest`;
4. returns the manifest's own MCID (codec `16#56`).

`get_content/2` on a manifest MCID fetches the manifest, then every chunk in
order, reassembles, and verifies the whole against the manifest's size and
Merkle root before returning — a tampered or truncated chunk is caught before
the caller ever sees the bytes.

A chunk failure during put stops immediately without uploading the manifest —
a manifest naming missing chunks would resolve but never reassemble, which is
worse than a clean error.

The whole transfer — every `_content.put_block`/`_content.get_block` call plus
the manifest call for chunked content — rides one dedicated QUIC stream, opened
once and reused for the sequence, isolating a large blob transfer from other
RPC/PubSub traffic on the same connection.

---

## Discovery: who has this MCID?

Chunked content gets **announced** automatically: when a station stores a
manifest, it publishes a signed `content_announcement` DHT record naming
itself as a host, the same way a station announces its endpoint. Resolve every
host currently announcing an MCID:

```erlang
{ok, Providers} = macula:find_content_providers(Pool, MCID),
%% [#{announcer_node := StationPubkey, endpoint := <<"quic://host:443">>,
%%    name := ..., size := ..., chunk_count := ...}, ...]
```

Each entry's record signature is verified before its `endpoint` is trusted;
unverifiable or malformed records are dropped silently, never surfaced as an
error. **Single-block content is not announced** (there is no manifest-stored
event to trigger it) — resolving its MCID returns `{ok, []}`, not an error.

### Dialing a specific host directly

`get_content/2` already reaches a copy via the connected station's own 1-hop
peer relay — for most topologies that is enough. When it is not (a
partial-mesh pair with no mutual peer, or you want to route around a specific
host deliberately), dial an announced host **directly**, the same way
[direct-dial RPC](RPC_GUIDE.md) reaches a specific provider — `call_station/6`
is procedure-agnostic, so no new primitive is needed:

```erlang
{ok, [#{endpoint := Url} | _]} = macula:find_content_providers(Pool, MCID),
{ok, Manifest} = macula:call_station(Pool, Url, <<0:256>>,
                                     <<"_content.get_manifest">>,
                                     #{mcid => MCID}, 5_000).
```

Guarantees reach in one hop regardless of the connected station's relay hop
budget — the same value `call_station` already gives unary RPC calls.

---

## When to use content vs. records vs. streaming

| You have | Use |
|---|---|
| An immutable blob to store and fetch by identity | **Content** (`put_content` / `get_content`) |
| A small, signed, TTL'd fact to publish in the DHT | **Records** (`put_record` / `find_records`) |
| An open-ended live feed with no fixed size | **[Streaming](STREAMING_GUIDE.md)** (`call_stream`) |

Content is for bytes addressed by *what they are*; records are for signed
statements addressed by *who said them*; streaming is for a flow with no end
known in advance.

---

## Reference

| Function | Role |
|---|---|
| `put_content(Pool, Bytes)` | store a blob (single-block or chunked, by size), return its MCID |
| `get_content(Pool, MCID)` | fetch the bytes for an MCID (`{error, not_found}` if none reachable) |
| `find_content_providers(Pool, MCID)` | resolve every host currently announcing an MCID |
| `macula_manifest:default_chunk_size()` | the single-block / chunked threshold (256 KiB) |
| `macula_blake3_nif:hash(Bytes)` | the BLAKE3 hash a single-block MCID wraps |
| `macula_feeder:start_link/4,5` | supervised, `sharing.put_*_v1`-announcing wrapper around `put_content/2` |
| `macula_download:start_link/4,5` | supervised, `sharing.get_*_v1`-announcing wrapper around `get_content/2` |

`_content.*` CALLs retry on a BOLT#4-retryable error (e.g.
`temporary_relay_failure`) up to 3 attempts with a short backoff, per that
error's own documented retry contract.
