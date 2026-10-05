# Design: node-served content (D27)

This exists so a node can share bytes and any mesh node can fetch them while the sharer is online, with no
station keeping content.

Status: approved by Raf as written (2026-09-24); ships as macula 12.6.0, with Venus's three review changes folded in (realm in the
announcement, the station#7 dependency, no advertisement check on a fetch). Transport chosen by Raf: option A, content as a stream the
sharing node serves. Tracks macula#35. The Go SDK ports it (macula-go), it does not design its own.

## What changes and what does not

- **Removed from the SDK:** `macula_content_transfer`'s `_content.put_block`, `_content.get_block`,
  `_content.put_manifest` and `_content.get_manifest` calls, which went to a station content store that D27 removed
  (macula-station 647dfba). Every content path uses them today: `put_content/2`, `get_content/2`, the
  `*_station` forms, `macula_upload`/`macula_feeder`, `macula_download`, and direct dial's `content_get`.
- **Kept byte for byte:** content ids (tag 2, SHA-384, 50 bytes), `macula_manifest` (256 KiB chunks, the Merkle
  fold, the manifest wire shape), and the rule that a single-chunk blob is its own raw block.
- **No station change.** Fetches ride STREAM_OPEN, which stations already route, admit and relay. A content
  announcement needs only `announcer_node` and `mcid` to pass `macula_record:payload_ok/2`, which is what stations
  verify with, so the new fields below are admitted by stations running today.

## Who may share

Only a node that holds a procedure delegation from an org, under the realm's org directory: the same D25 chain every
procedure advertisement carries. This is intended. Every mcl-* service has one; a bare CLI or MCP node without an
org cannot share, and can still fetch. Sharing without an org is a possible later item, which would need either a
station change or a new advertisement form (the ADVERTISE form check refuses a name without an org namespace).

## The procedure

A sharing node serves its content under one procedure of its own:

    <org>/content_v1_<node_id as 64 lowercase hex>

- **Unique per sharing node.** A station keeps ONE registry entry per `(realm, procedure)`
  (`macula_remote_advertise_registry:lookup/3`) and routes by it, ignoring a request's target. A shared name like
  `<org>/content_v1` would send every fetch to whichever node of the org advertised last, which may not hold the
  content. Naming the node in the procedure makes the route exact.
- **Served by the pool, not by application code.** `macula:share_content/3` registers the procedure on first use,
  through the same `advertise_authorized` path as any provider (#29's per-link signing, #32's re-sign timer), and
  withdraws it when the node shares nothing any more.
- **Mode `server_stream`, one content id per stream.** The fetcher's stream args name the content id and what it
  wants; the sharer answers with DATA and closes. Several content ids are several streams, each admitted and capped
  by the existing session limits (16 per caller, 1,000 in all, `macula_stream_sessions`).

## The announcement (record 0x11)

One announcement per content id per sharing node, signed by the node (`announcer_node`), as today. The payload:

| Field | Type | Meaning |
|---|---|---|
| `announcer_node` | bytes, 32 | the sharer; the signer |
| `mcid` | bytes, 50 | the content id: a raw block, or a manifest |
| `realm_id` | bytes, 32 | the realm the sharer serves its procedure in, which a STREAM_OPEN routes by |
| `serving_station` | bytes, 32 | the station the sharer is reachable through now |
| `procedure` | text | the sharer's content procedure, as above |
| `size` | uint, optional | total bytes |
| `chunk_count` | uint, optional | chunks, for a manifest |
| `name` | text, optional | a display name |

- **`endpoint` (a text URL the sharer wrote) is dropped.** The fetcher resolves `serving_station` to a dialable
  endpoint from that station's own `station_endpoint` record, as direct dial already does, so it never trusts an
  address the sharer supplied.
- **The fetcher checks** that `procedure` equals `<org>/content_v1_<hex(announcer_node)>` for some org, and refuses
  the announcement otherwise. The announcement cannot point a fetch at another node's procedure. It opens the stream
  in the announcement's `realm_id`: the content key is realm-free, so without it a fetcher asking in another realm
  would get `unknown_next_peer` and could not tell why.
- **Lifetime:** at most 48 hours (the type's maximum), re-announced at half its life while shared, and at once when
  the pool's link to `serving_station` is lost and another link takes over. The DHT slot for a content id holds one
  entry per signer (D28), so a sharer names one station at a time.

## The fetch

1. **Find.** `find_records(Pool, macula_record:content_key(MCID), _)` returns the announcements for the content id.
   Keep those that verify, name a live `serving_station` and pass the procedure check. None: `{error, not_shared}`.
2. **Pick.** Try sharers in a random order, one at a time.
3. **Dial.** Direct-dial the sharer's `serving_station` (`call_stream_station/7`), pinned to its node id, with the
   stream target set to the sharer, in the announcement's realm. **No advertisement or realm-key check applies:** a
   procedure call checks the provider's advertisement against the realm key, but content verifies itself by its
   content id, so a fetcher that trusts no realm can still fetch. Do not add a realm-trust requirement here.
4. **Ask for the root.** Stream args: `#{mcid => MCID, want => root}`. The sharer answers with ONE DATA body:
   - for a raw block: `#{kind => block, mcid => MCID, bytes => Bytes}`;
   - for a manifest: `#{kind => manifest, mcid => MCID, manifest => ManifestWire}`.
5. **Verify the root before using it.** A block: SHA-384 of `bytes` must give `MCID`. A manifest: it must decode and
   `macula_manifest:verify_mcid/2` against `MCID` before any of its sizes is read. Then its `size` and chunk count are
   checked against the caller's bounds (below) before a single chunk is requested.
6. **Fetch the chunks.** One `server_stream` per chunk, args `#{mcid => ChunkMcid, want => block}`, a bounded number
   in parallel (default 4, as `DEFAULT_STREAM_COUNT` today). Each chunk is verified against its own content id as it
   arrives, and a chunk that fails is refused without being kept.
7. **Assemble** in manifest order, and `macula_manifest:verify/2` over the whole before returning it.

A DATA body is a map, keys and texts tagged as every wire payload is, no booleans. Errors come back as the stream's
own error with a code: `not_shared` (the sharer does not hold it any more), `too_large`, `busy`.

## Bounds

| Bound | Default | Where |
|---|---|---|
| bytes per DATA body | one chunk, 256 KiB plus the envelope | sharer sends, fetcher refuses larger |
| manifest size | 4 MiB, as the DATA frame's size | fetcher, on the frame before it reads the body (the body is decoded with the frame) |
| chunks per manifest | 16,384 (4 GiB at 256 KiB) | fetcher, after verify_mcid, before any request |
| per-fetch byte budget | caller's `max_bytes`, default 256 MiB | fetcher, checked against the manifest's size |
| parallel chunk streams | 4 | fetcher |
| per-chunk deadline | 15 s | fetcher, as `CONTENT_BLOCK_TIMEOUT_MS` today |
| whole-fetch deadline | caller's timeout | fetcher |

The fetcher never allocates for a size it has not bound: the manifest is length-checked before decoding and
MCID-checked before its fields are used, and each chunk is length-checked before it is hashed.

## When the sharer is offline

Content is available while its sharer is online (D27's stated consequence). A fetcher:

- gets `unknown_next_peer` from the station, or a dial failure, and moves to the next announcement;
- after the last one fails, returns `{error, {unavailable, [{Sharer, Reason}]}}`, naming each sharer and why;
- does not retry the same sharer inside one fetch, and keeps nothing of a partial fetch (no resume in this version).

An announcement outlives its sharer by up to its lifetime, so a fetcher meeting a dead one is the normal case, and
costs it one failed dial.

## Depends on

- **macula-station 0.6.2** (macula-station#7) on the station a sharer is linked to. The per-node procedure's
  advertisement lives at most 5 minutes and is renewed on the same connection; a 0.6.1 station drops that renewal,
  so the sharer becomes unreachable about 10 minutes in. A fetch failing there is the station, not content.
- **The SDK's advertisement renewal** (macula#32), so a sharer that only uses the facade stays routable.

## Follow-ups

- **Drop the per-node suffix** once stations keep several providers per `(realm, procedure)` and route by target
  (macula-station#8): all of an org's sharers then serve `<org>/content_v1`, and the announcement's
  `announcer_node` is the target.
- **Sharing without an org**, and **resuming a partial fetch**: not in this version.

## The API

- `macula:share_content(Pool, Realm, Bytes) -> {ok, MCID}`: chunks and stores locally, registers the procedure if
  needed, announces. Keeps the bytes in a node-local store (memory by default; a caller may pass a store module).
- `macula:unshare_content(Pool, Realm, MCID) -> ok`: tombstones the announcement, drops the bytes.
- `macula:get_content(Pool, Realm, MCID, Opts) -> {ok, Bytes} | {error, _}`: the fetch above.
- `put_content/2` and `get_content/2`, the `*_station` forms, and `macula_content_transfer`'s store calls are
  removed. `macula_upload`/`macula_feeder` and `macula_download` keep their behaviours and move onto
  share/get. No compatibility layer: nothing deployed uses them (mcl-tube is not deployed; its owner UI's
  `tube_content_get` moves to share/get or to its local store).

## Tests (red first)

- A two-node test over a real station link fixture: a sharer shares a raw block and a manifest; a fetcher gets both
  and the bytes are identical.
- A manifest whose MCID does not match, a chunk whose hash does not match, an oversize DATA body, a manifest past the
  chunk bound: each is refused, and nothing is requested after the refusal.
- An announcement naming another node's procedure is refused.
- A dead first sharer: the fetch moves to the second and succeeds; all dead: `{error, {unavailable, _}}` naming each.
- Two sharers of one org: each fetch reaches the node it names (the one-entry-per-procedure case).
- The Go SDK runs the same fixtures against the Erlang sharer before it ships.

## Open for review

1. The procedure form `<org>/content_v1_<node hex>`: acceptable, or should the node id go elsewhere?
2. Defaults: 256 MiB per fetch, 16,384 chunks, 4 MiB manifest.
3. No resume of partial fetches in this version.
