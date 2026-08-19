# Macula SDK — Content Guide

**Content-addressed blob storage on the mesh, by MCID.**

![Content Sharing (MCID)](assets/content_sharing.svg)

> **Audience:** applications that store and fetch immutable blobs — files,
> snapshots, artifacts — and want integrity for free. Available since SDK 4.2.7.
> For a live, open-ended feed instead of a fixed blob, see the
> [Streaming Guide](STREAMING_GUIDE.md).

---

## Overview

Macula content is **content-addressed**: a blob is named by the hash of its
bytes, not by where it lives. That name is an **MCID** (Macula Content ID). Two
consequences follow directly, and they are the whole point:

- **Integrity is self-verifying.** The name *is* the hash, so a fetched blob can
  be checked against its own MCID — a corrupted or substituted blob fails the
  check by construction.
- **Location stops mattering.** Any host holding the bytes serves the same MCID,
  so hosts are interchangeable and content deduplicates naturally.

```erlang
{ok, MCID}  = macula:put_content(Pool, <<"the bytes">>),
{ok, Bytes} = macula:get_content(Pool, MCID).
```

`put_content/2` stores the blob and returns its MCID. `get_content/2` fetches the
bytes for an MCID, or `{error, not_found}` if no reachable host holds a copy. The
store verifies BLAKE3 against the MCID on both put and get, so the caller does not
re-verify.

---

## MCID format

An MCID is a **34-byte** binary:

```
<<Codec:8, Algo:8, Hash:32/binary>>
   1        16#55   BLAKE3(bytes)
```

- byte 0 — codec tag (`1`)
- byte 1 — hash algorithm (`16#55` = BLAKE3)
- bytes 2..33 — the 32-byte BLAKE3 hash of the content

Because the MCID is derived purely from the bytes, `put_content` of identical
bytes always yields the same MCID — that is what makes it a content address.

```erlang
%% the MCID is BLAKE3 of the bytes, tagged
Hash = macula_blake3_nif:hash(Bytes),
MCID = <<1, 16#55, Hash/binary>>.
```

---

## Current shape and direction

The diagram above shows the content-addressed **model**: hosts announce content,
a fetcher resolves an MCID in the DHT, dials a host, and pulls the chunks. Today
the SDK exposes the minimum-viable form of that model:

- **Single block.** `put_content` sends the blob as one block to the content
  store and `get_content` fetches it back as one block. There is no client-side
  chunking yet, so it suits blobs in the kilobyte-to-low-megabyte range (the
  store's default block size is 256 KiB). An oversized payload surfaces as a
  CALL-deadline timeout, not silent truncation.
- **Store-mediated fetch.** Retrieval goes through the content store rather than
  dialing a specific announcing host directly.

Planned, and reflected in the diagram: **chunked manifests** (so blobs larger
than one block transfer via parallel block calls), **`content_announcement` DHT
records** (so many hosts can advertise the same MCID), and **direct host
selection** on fetch. When those land, large-blob and multi-host sharing become
first-class; the MCID and its integrity guarantee do not change.

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
| `put_content(Pool, Bytes)` | store a blob, return its MCID |
| `get_content(Pool, MCID)` | fetch the bytes for an MCID (`{error, not_found}` if none reachable) |
| `macula_blake3_nif:hash(Bytes)` | the BLAKE3 hash an MCID wraps |
