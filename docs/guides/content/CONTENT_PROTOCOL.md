# Macula SDK — Content Protocol

**How node-served content works on the wire: the content id, the announcement,
the content procedure, the fetch and its bounds.**

> **Audience:** SDK authors porting content sharing to another language, and
> anyone debugging a fetch. Application code uses the calls in the
> [Content Guide](CONTENT_GUIDE.md). The design and its reasons are in
> `plans/DESIGN_D27_NODE_SERVED_CONTENT.md`.

A station keeps no content (D27). The node that shares content keeps it,
serves it on a stream procedure of its own, and announces it in the DHT; a
fetcher finds the announcement, dials the sharer through the station it named,
and verifies everything it receives against the content id it asked for.

---

## Content id (MCID)

50 bytes: `<<Tag:8, Codec:8, Hash:48/binary>>`. The tag names the hash; the
post-quantum format has only tag 2, SHA-384 (D24), and any other tag is
refused.

| Codec | Names | Hash |
|---|---|---|
| `16#55` | one raw block (content of at most 256 KiB, or one chunk) | SHA-384 of the bytes |
| `16#56` | a manifest | SHA-384 of the manifest's canonical CBOR fields (`name`, `size`, `chunk_size`, `chunk_count`, `hash_algorithm`, `root_hash`) |

A manifest describes content larger than one chunk: its size, chunk size
(256 KiB), chunk count, each chunk's offset, size and hash, and the Merkle root
over the chunk hashes (`macula_manifest`). Every SDK builds the same manifest,
and so the same MCID, for the same bytes and name.

---

## The announcement (record type 0x11)

One announcement per content id per sharing node, signed by that node, stored
under `macula_record:content_key(MCID)`, so every sharer of one content id
shares a DHT slot (one entry per signer).

| Field | Type | Meaning |
|---|---|---|
| `announcer_node` | bytes, 32 | the sharer; the record's signer, which verification checks |
| `mcid` | bytes, 50 | the root content id |
| `realm_id` | bytes, 32 | the realm the content procedure is served in |
| `serving_station` | bytes, 32 | the station the sharer is reachable through now |
| `procedure` | text | the sharer's content procedure |
| `name`, `size`, `chunk_count` | optional | metadata |

- A verifier refuses an announcement without `realm_id`, `serving_station` or
  a non-empty `procedure`.
- A fetcher resolves `serving_station` to a dialable endpoint from that
  station's own signed `station_endpoint` record; it never dials an address
  the sharer wrote.
- A fetcher accepts only a `procedure` bound to the announcer: see below.
- The sharing SDK signs announcements for an hour and renews them at half
  that, announces again when its node moves to another station, and withdraws
  them (a tombstone signed by the node) when the content is unshared.

---

## The content procedure

A sharing node serves all its content in a realm on one `server_stream`
procedure of its own:

| Form | When |
|---|---|
| `~<node id as 64 lowercase hex>/content_v1` | the node's own namespace (D25 item 6), no org needed |
| `<org>/content_v1_<node id as 64 lowercase hex>` | under an org the node holds a delegation for |

The node id is in the name because a station routes a procedure to one
provider (macula-station#8): a name shared by several sharers would send
every fetch to whichever advertised last. A fetcher accepts an announcement
only when its `procedure` is one of these two forms for its own
`announcer_node` (and the org is not empty, not `_`, not `~...`, and has no
`/`), so an announcement cannot point a fetch at another node's procedure.

### One fetch, one stream

The stream's open-time args name one content id and what is wanted:

```
#{mcid => MCID, want => root | block}
```

`root` for the content id a fetcher was given; `block` for a chunk of a
manifest it holds. The sharer answers with **one DATA body, then the end of
the stream**:

| Body | When |
|---|---|
| `#{kind => block, mcid => MCID, bytes => Bytes}` | a raw root, or a chunk |
| `#{kind => manifest, mcid => MCID, manifest => Manifest}` | a manifest root |

or with the stream error `not_shared` (content it does not hold; a raw root is
not served as a chunk) or `malformed` (args that name no content id, or want
anything else). Keys and text values arrive as a station link delivers them,
tagged or not; both ends read them the way any wire payload is read.

---

## The fetch

1. Find the announcements under `content_key(MCID)`. Keep those that name
   the MCID, the realm asked for, a serving station and a bound procedure.
   None: `{error, not_shared}`.
2. Try the sharers one at a time, in a random order.
3. Resolve the serving station's endpoint and open the stream through it,
   pinned to the station's node id and targeted at the sharer, in the
   announced realm. **No advertisement or realm-key check applies**: content
   verifies itself by its content id, so a fetcher that trusts no realm can
   still fetch.
4. Ask for the root.
   - A block must hash to the MCID.
   - A manifest must match the MCID (`macula_manifest:verify_mcid/2`) before
     any of its sizes is read, then fit the caller's bounds before a chunk is
     asked for.
5. Ask for each chunk on its own stream, a bounded number at a time, and
   verify each against its own chunk MCID as it arrives; then assemble in
   order and verify the whole against the manifest's size and Merkle root.

A sharer that fails in any step moves the fetch to the next. When every one
has failed the answer is `{error, {unavailable, [{Sharer, Reason}]}}`, naming
each. Nothing of a failed attempt is kept, and nothing is resumed.

---

## Bounds

| Bound | Default |
|---|---|
| bytes of one DATA body | one chunk (a block larger than 256 KiB is refused) |
| content size (`max_bytes`) | 256 MiB |
| chunks per manifest (`max_chunks`) | 16,384 (4 GiB of 256 KiB chunks) |
| chunk streams open at once (`parallel`) | 4 |
| per-stream deadline (`chunk_timeout_ms`) | 15 s |

---

## Depends on

- macula-station 0.6.2 (macula-station#7) on the station a sharer is linked
  to: the content procedure's advertisement is renewed on the same
  connection, and an older station drops that renewal.
- For the `~<node id>/content_v1` form, macula-station 0.6.4 or later, which
  admits a node's own namespace.

---

## Reference

| Module | Role |
|---|---|
| `macula_content_sharer` | a pool's sharer: keeps, serves and announces |
| `macula_content_store` | the content a node shares, as blocks and manifests |
| `macula_content_serve` | answers one fetch on the content procedure |
| `macula_content_fetch` | finds sharers, fetches and verifies |
| `macula_manifest` | chunking, Merkle root, manifest MCID |

## See also

- [Content Guide](CONTENT_GUIDE.md): the calls applications use.
- [RPC Protocol](../rpc/RPC_PROTOCOL.md): direct dial, the same station
  endpoint resolution content uses.
