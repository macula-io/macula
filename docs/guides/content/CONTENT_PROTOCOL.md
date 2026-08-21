# Macula SDK — Content Protocol

**The raw wire primitives underneath `macula_feeder` / `macula_download`.**

> **Audience:** building something the supervised wrappers don't fit —
> custom retry logic, observability, an SDK for another language. Most
> applications want the [Content Guide](CONTENT_GUIDE.md) instead — it
> covers the same capability via `macula_feeder`/`macula_download` (and
> `macula_pusher`/`macula_upload` for a targeted push), with an addressable
> pid, cancel, and mesh facts already wired in.

---

## Raw primitives

```erlang
{ok, MCID}  = macula:put_content(Pool, Bytes),
{ok, Bytes} = macula:get_content(Pool, MCID).
```

`put_content/2` stores `Bytes` and returns its MCID — transparently as one
block for small content, or as a chunked, Merkle-verified manifest for large
content (see [Single block vs. chunked](CONTENT_GUIDE.md#single-block-vs-chunked)
in the Guide). `get_content/2` fetches the bytes back for either shape, or
`{error, not_found}` if no reachable host holds a copy. Integrity is
verified before the bytes are returned, so the caller does not re-verify.

This is what [`macula_feeder`/`macula_download` wrap](CONTENT_GUIDE.md#supervised-wrappers-macula_feeder-macula_download) —
an addressable pid you can monitor and cancel, `sharing.*_v1` mesh facts
around each transfer. Reach for the raw calls below directly only if you're
building something the wrappers don't fit.

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
  canonical encoding of the manifest's metadata (manifest — see
  [Single block vs. chunked](CONTENT_GUIDE.md#single-block-vs-chunked) in the
  Guide)

Because the MCID is derived purely from the content, storing identical bytes
always yields the same MCID — that is what makes it a content address.

---

## Real cancel: `macula_content_transfer`

`put_content/2`/`get_content/2` (and their `_station` variants) are thin
blocking wrappers over **`macula_content_transfer`** — the module that
actually picks the link, opens the dedicated content stream, and drives the
block/manifest exchange. Call it directly when you want the addressable
handle yourself, without a `macula_feeder`/`macula_download` behaviour
module:

```erlang
{ok, Pid} = macula_content_transfer:start_put(Pool, Bytes),
case macula_content_transfer:await(Pid) of
    {ok, MCID}      -> ok;
    {error, Reason} -> handle_error(Reason)
end,
macula_content_transfer:cancel(Pid).   % reap the handle when done
```

`cancel/1,3` is a **real, peer-visible abort**: if a content stream is
already open, `cancel/3` resets it with `macula_quic:reset_stream/2` — a
QUIC RESET_STREAM frame the peer's own read genuinely observes, not merely
a connection that went away. `Message` is local-only (QUIC RESET_STREAM
carries just the numeric `Code` on the wire); `cancel/1` defaults to
`Code = 0`. A transfer cancelled before it has picked a link yet has
nothing to reset — the worker is simply killed. `macula_feeder`/
`macula_download`'s own `cancel/1` (still `gen_server:stop/1` — same public
API as always) reaches all the way down to this same real abort too, since
9.11.1 (PLAN_PUSH_UPLOAD.md Phase 4) — before that it could only kill their
own local worker, orphaning the `macula_content_transfer` underneath rather
than actually cancelling it.

Each transfer mints a `share_id` (override via `Opts`'s `share_id` key), kept
in `macula_content_transfer_registry` with monitor-based cleanup, so a caller
that only saw the id in a published `sharing.*_started_v1` mesh fact — not the
pid — can still resolve it: `macula_content_transfer_registry:whereis_share/1`.

---

## Real pause/resume for chunked transfers

For content over the chunk threshold, `pause/1` genuinely stops the transfer
between chunks — the chunk already in flight, if any, still completes (a
chunk's own round trip stays one uninterrupted blocking call: pausing
mid-chunk would leave a half-sent block the station can't verify), but the
next one does not start until `resume/1`, which continues from exactly the
next un-sent/un-fetched chunk, never from the beginning:

```erlang
{ok, Pid} = macula_content_transfer:start_put(Pool, LargeBytes),
ok = macula_content_transfer:pause(Pid),
%% ... later ...
ok = macula_content_transfer:resume(Pid),
{ok, MCID} = macula_content_transfer:await(Pid).
```

Single-block content has no "between chunks" to pause at, so `pause/1` there
is a harmless no-op — the transfer just runs to completion regardless.
`cancel/1,3` still works at any point, paused or not: mid-chunk it kills that
chunk's worker and resets the stream exactly as above; paused between chunks
(no worker in flight) it just resets the stream directly.

`macula_feeder`/`macula_download` have no `pause`/`resume` in their own
public API — reach for `macula_content_transfer` directly if you need this.

---

## Parallel multi-stream chunk transfer

Chunked content spreads across multiple dedicated content streams on the same
link instead of one — pass `stream_count` to use more or fewer than the
default of 4 (always capped at the actual chunk count, so a 2-chunk transfer
never opens more than 2 streams):

```erlang
{ok, Pid} = macula_content_transfer:start_put(Pool, LargeBytes, #{stream_count => 8}),
{ok, MCID} = macula_content_transfer:await(Pid).
```

Each stream runs its own chunk-by-chunk loop concurrently; the manifest is
put (or, for a get, reassembled and verified) only once every stream has
drained its own share. Reassembly reads fetched chunks back out by INDEX, not
arrival order, so it doesn't matter which stream's chunk lands first.
`pause/1`/`resume/1` gate every stream the same way, uniformly. If one
stream's chunk genuinely fails (a real `{error, _}`, not a crash), the whole
transfer fails with it — every other stream's in-flight work is killed and
every stream reset before `await/1,2` sees the error, same as a sequential
transfer would behave, just faster to notice since fewer chunks were still
outstanding elsewhere. Opening an extra stream is best-effort: a failure
degrades to fewer streams rather than failing the transfer outright — a
single-stream transfer is still correct, just slower.

`start_link/4,5` (the `macula_feeder`/`macula_download` wrappers) has no way
to reach `stream_count` — it is `Opts` on `macula_content_transfer`, and the
wrappers' own last argument is `Args` (for `Module:init/1`), not `Opts`.

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

Each entry's record signature is verified, AND its signer checked to be
exactly the `announcer_node` it claims — not just any valid signature —
before its `endpoint` is trusted; unverifiable, signer-mismatched, or
malformed records are dropped silently, never surfaced as an error.
**Single-block content is not announced** (there is no manifest-stored
event to trigger it) — resolving its MCID returns `{ok, []}`, not an error.

This is what [`macula_download:start_link_direct/4,5`](CONTENT_GUIDE.md#direct-dial-start_link_direct)
does for you automatically. Reach for `find_content_providers/2` directly
only if you need the provider list itself, not just a dial.

### Dialing a specific host directly

`get_content/2` already reaches a copy via the connected station's own 1-hop
peer relay — for most topologies that is enough. When it is not (a
partial-mesh pair with no mutual peer, or you want to route around a specific
host deliberately), dial an announced host **directly** with
`get_content_station/4,5` — the content-transfer counterpart to
[direct-dial RPC](../rpc/RPC_GUIDE.md)'s `call_station/6,7`:

```erlang
{ok, [#{announcer_node := Node, endpoint := Url} | _]} =
    macula:find_content_providers(Pool, MCID),
{ok, Bytes} = macula:get_content_station(Pool, Url, MCID, 30_000,
                                         #{expected_node_id => Node,
                                           pin_tls_cert => false,
                                           verify => none}).
```

Or resolve and fetch in one call with `macula_direct_dial:get_content/3`,
which does exactly the above — retrying past DHT propagation lag the same
way `find_content_providers/2` returning an empty list means "not yet
replicated," not "doesn't exist":

```erlang
{ok, Bytes} = macula_direct_dial:get_content(Pool, MCID, 30_000).
```

`pin_tls_cert => false` matters against a real production station: its TLS
is terminated by an unrelated PKI (Let's Encrypt), so pinning the cert's own
key can never succeed there — trust instead rests on the application-layer
CONNECT/HELLO handshake (see the [RPC Guide](../rpc/RPC_GUIDE.md) for the full
mechanism). Unlike RPC, content direct-dial has **no cert-chain-equivalent
opt-in**: content is content-addressed and independently re-hashed
client-side regardless of which peer serves it, so a rogue or unauthorized
announcer can at most refuse to serve or waste a dial — never make a caller
accept content that doesn't hash to the MCID it asked for.

Seeding a specific station directly — the put-side counterpart, with no
discovery step since you already know which station you're choosing — uses
`put_content_station/4,5` or `macula_direct_dial:put_content/4`, resolving
`Station`'s own `station_endpoint` the same way RPC resolves a
`serving_station`:

```erlang
{ok, MCID} = macula_direct_dial:put_content(Pool, StationPubkey, Bytes, 30_000).
```

Guarantees reach in one hop regardless of the connected station's relay hop
budget — the same value `call_station` already gives unary RPC calls.

---

## Reference

| Function | Role |
|---|---|
| `put_content(Pool, Bytes)` | store a blob (single-block or chunked, by size), return its MCID |
| `get_content(Pool, MCID)` | fetch the bytes for an MCID (`{error, not_found}` if none reachable); single-block bytes are re-verified against the MCID's BLAKE3 hash client-side |
| `get_content_station(Pool, Station, MCID, TimeoutMs, Opts)` | **direct-dial**: fetch from a specific, already-resolved station |
| `put_content_station(Pool, Station, Bytes, TimeoutMs, Opts)` | **direct-dial**: seed a specific station directly |
| `find_content_providers(Pool, MCID)` | resolve every host currently announcing an MCID (signature- and signer-verified) |
| `macula_direct_dial:get_content(Pool, MCID, TimeoutMs)` | **direct-dial**: resolve a provider and fetch, in one call |
| `macula_direct_dial:put_content(Pool, Station, Bytes, TimeoutMs)` | **direct-dial**: resolve `Station`'s endpoint and put, in one call |
| `macula_manifest:default_chunk_size()` | the single-block / chunked threshold (256 KiB) |
| `macula_blake3_nif:hash(Bytes)` | the BLAKE3 hash a single-block MCID wraps |
| `macula_content_transfer:start_put/2,3`, `start_get/2,3` | addressable put/get — `put_content`/`get_content`'s foundation, real `cancel/1,3` |
| `macula_content_transfer:start_put_station/4,5`, `start_get_station/4,5` | **direct-dial** addressable variants |
| `macula_content_transfer:await/1,2` | block for an addressable transfer's outcome, repeatable, cacheable |
| `macula_content_transfer:cancel/1,3` | real, peer-visible abort (QUIC RESET_STREAM) if a stream is open; pure reap otherwise |
| `macula_content_transfer:pause/1`/`resume/1` | real pause/resume between chunks, every open stream (chunked content only — a no-op on single-block) |
| `Opts`'s `stream_count` (start_put/start_get etc.) | parallel streams for a chunked transfer, default 4, capped at the chunk count |
| `macula_content_transfer_registry:whereis_share/1` | resolve a transfer's `share_id` (from a mesh fact) to its pid |

`_content.*` CALLs retry on a BOLT#4-retryable error (e.g.
`temporary_relay_failure`) up to 3 attempts with a short backoff, per that
error's own documented retry contract.

---

## See also

- [CONTENT_GUIDE.md](CONTENT_GUIDE.md) — the supervised wrappers most
  applications should use instead of these raw primitives.
