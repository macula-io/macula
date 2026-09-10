# Post-quantum signed frames and records

This exists so every signed object after the handshake, record or frame, verifies the same way in every stack, with the
signer's full key and nothing re-encoded.

By Mercury, 2026-09-11, reviewed by Mars, Neptune and Venus in two rounds. It details D13, D17 as revised on 2026-09-11,
D24, D25 and the D7 refinement in `PLAN_POST_QUANTUM_SECURITY.md`, for WP 1.3, WP 1.4 and each stack's Stage 4 work
package. The handshake itself is in `DESIGN_PQ_HANDSHAKE_FRAMES.md`.

## Encoding

- The codec, the version and the carried form of keys are those of `DESIGN_PQ_HANDSHAKE_FRAMES.md`: deterministic CBOR
  maps with text keys, and `version` 3. Verifiers never re-encode.
- **Signers** encode integers and lengths in their shortest form, map keys sorted by their encoded bytes, and floats as
  binary64. A manifest id depends on these bytes.
- **Decoding rule.** Every stack accepts exactly the same objects. A decoder refuses, as `malformed_frame`:
  - bytes after the top-level item, so decoding a `tbs` consumes exactly its length;
  - an indefinite length, a tag, or a simple value other than null;
  - text that is not valid UTF-8;
  - a map key that is not text or an integer;
  - a duplicate map key: two text keys with equal bytes, or two integer keys of equal value;
  - an item nested more than 64 levels deep, in payloads too;
  - a negative integer below -2^63;
  - in a signed structure, an unknown key, or a field of the wrong type or length.
- A frame over 16 MiB is refused as `frame_too_large`.
- A decoder accepts definite lengths in any width, map keys in any order, and floats (half, single or double) as
  values inside application payloads. No protocol field is a float, so a float there is refused by that field's
  type. The shortest form and the key order are the signer's duty.
- Protocol integers stay below 2^53. An application integer above 2^53 loses precision in a stack whose numbers are
  doubles.

### Peer-supplied maps (D26)

Accepted by Raf on 2026-09-11 for `macula` 11.0.0. The Erlang stacks deliver every map a peer supplies as application
data in one form, whatever a node has loaded.

- **Scope:** every such map at any depth: CALL, STREAM_OPEN, RESULT and STREAM_REPLY `payload`, STREAM_DATA `body` in
  both directions, and PUBLISH and EVENT `payload`. Record payloads already keep this form.
- **Rule:** inside those maps, a text key or value is delivered as `{text, Bin}`, a byte string as a binary, integers,
  floats and lists as decoded, and CBOR null as `undefined`. Nothing becomes an atom.
- **Envelope fields,** the fields a frame type defines, such as `version`, `frame_type`, `request_id` and `mode`,
  decode to atoms through a fixed table in the codec, never through `binary_to_existing_atom` on peer input. An
  unknown `frame_type` or `mode` value is `malformed_frame`.
- **Accessors on the facade:**
  - `macula:field(Name, Map)` returns a field's value, or `undefined`;
  - `macula:field(Name, Map, Default)` returns `Default` for a missing field;
  - `Name` is an atom or a binary, and the lookup tries `{text, NameBin}`, then the atom, then the binary, so a map
    handed over in process, with no codec in between, reads the same way;
  - `macula:text(Value)` returns the binary of a `{text, Bin}` value, returns a binary unchanged, and raises `badarg`
    for anything else.
  Handlers read fields through these, never by matching key forms.
- **Encoding:** a handler may return maps with atom, binary or `{text, Bin}` keys, and all go out as CBOR text keys;
  integer keys go out as integers, and `undefined` as null. The encoder refuses a map whose keys collide as text, such
  as `name` and `{text, <<"name">>}`, so the sender fails locally rather than the receiver refusing the frame.
- **Why:** a reader sees the same map on a node's first call as on every later one.
- **Test property:** the same frame, decoded on a fresh node with the absence of its field atoms asserted first and
  on a warm node, delivers identical maps.
- Sizes below are given as US / EU.

## Signed objects

- **Two shapes.** A signed object is a CBOR map:
  - `{key, tbs, signature}` when the signer's key travels with the object: records, requests, replies, relay errors,
    publications, and a provider's first frame on a stream;
  - `{tbs, signature}` when the verifier already holds the key: a provider's later frames on a stream, a caller's
    frames on a stream, and neighbour signatures.
  - A frame that holds the other shape than its state expects is `malformed_frame`.
- **What is signed:** `Label || 0x00 || SHA-384(key as carried) || tbs`. The key hash binds each signature to its key,
  also when the key does not travel in the object. The handshake's bindings sign `Label || 0x00 || tbs` instead,
  because their `tbs` names the node_id of the key in the same frame; the difference is deliberate.
- **How a verifier reads one,** in order:
  1. the map holds exactly the keys of its shape, each of type bytes;
  2. `key`, when present, is in the carried form of the verifier's profile;
  3. the signature verifies over the `tbs` bytes as received, with the verifier's profile algorithm;
  4. only then is `tbs` decoded, under the decoding rule;
  5. `alg` in `tbs` names the verifier's profile algorithm, `ML-DSA-87` or `ML-DSA-87-PS384`. It is checked, and it
     never selects an algorithm.
- A frame's `frame_type` equals the `frame_type` inside its `tbs`, wherever the `tbs` holds one.
- **Nothing is re-encoded.** Stations store and forward a signed object's bytes as received. A verifier that holds a
  key for a stream or a connection may keep that key's SHA-384, so the hash costs nothing per frame.
- **Key ids.** A key id is 32 bytes. For an identity key it is the node_id (D5). For any other key, such as an org or
  a foundation key, it is `SHA-256("MACULA-KEY-ID-V1" || 0x00 || len(profile) || profile || key as carried)`, bound to
  the profile as D5 is. Every 32-byte field that names a signer holds a key id, and a verifier compares it with the
  key id derived from the key that verified.
- **Key ids of domain types.** A record of a domain type (tags 0x20 to 0xFF) names its signer by the
  `MACULA-KEY-ID-V1` key id of `key`, whatever the key's purpose, since a station cannot tell a key's purpose from
  a domain type. A consumer that knows a signer only by node_id fetches the signer's node record first and derives
  the key id from its key.

### Labels

Each label is ASCII, unique, and never a prefix of another, and none equals a label of the handshake design.

| Signed structure | Label |
|---|---|
| Record | `MACULA-PQ-RECORD-V1` |
| CALL and STREAM_OPEN | `MACULA-PQ-REQUEST-V1` |
| RESULT and ERROR from a provider | `MACULA-PQ-REPLY-V1` |
| ERROR and STREAM_ERROR from a station | `MACULA-PQ-RELAY-ERROR-V1` |
| STREAM_DATA, STREAM_END, STREAM_ERROR and STREAM_REPLY from a provider | `MACULA-PQ-STREAM-V1` |
| STREAM_DATA, STREAM_END and STREAM_ERROR from a caller | `MACULA-PQ-CALLER-STREAM-V1` |
| Publisher signature on PUBLISH and EVENT | `MACULA-PQ-PUBLICATION-V1` |
| Neighbour signature on a control frame | `MACULA-PQ-NEIGHBOUR-V1` |

Two derivation labels sign nothing: `MACULA-PQ-STORAGE-KEY-V1` for DHT storage keys, and `MACULA-KEY-ID-V1` for key
ids.

## Records

A record is the signed object `{key, tbs, signature}`, with `key` the signer's key as carried. The fields of `tbs`:

| Key | Type | Content |
|---|---|---|
| `type` | unsigned | the record type tag |
| `alg` | text | `ML-DSA-87` or `ML-DSA-87-PS384` |
| `version` | bytes, 16 | UUIDv7 |
| `created_at`, `expires_at` | unsigned, ms | |
| `payload` | map | the type's fields, text keys |
| `subject` | bytes | domain types (tags 0x20 to 0xFF) only, and optional there |

About 7.3 KB / 8.3 KB before the payload: 2,592 / 3,118 bytes of key and 4,627 / 5,139 bytes of signature.

- **Size.** Every stack refuses a record whose wire form is larger than 256 KiB, before any other check.
- **Checks,** after the steps of a signed object:
  - `tbs` holds exactly these keys, with `subject` only where the type allows it;
  - `created_at` is at most 5 minutes ahead of the verifier's clock, and `expires_at` plus 5 minutes has not passed
    (D22);
  - the payload follows its type's rules; the owner of a domain type sets them.
- **Who signed.** A record names no signer beside `key`. Where a payload names the node a record is about, such as
  a node record's own node, that node_id must equal the key id of `key`.
- **Slot.** A station derives a record's storage key from the verified record on STORE and REPLICATE, never from a
  key a peer sends, and refuses a record whose sent key differs. A consumer checks that each record in VALUE derives
  to the key it asked for, and discards one that does not.
- **Replacement** by `version` compares only records under one storage key and one signer key id, so two signers
  never replace each other.

### Tombstones

A tombstone, type tag 0x0C, withdraws one record before that record expires.

- It is stored under the storage key of the record it withdraws, derived from the withdrawn type and the fields its
  payload names.
- Its payload names that record's type and version, and the fields its storage key derives from, so any holder can
  recompute the slot.
- It is signed by the same key id as the withdrawn record, with a later version.
- Within its slot a tombstone replaces only its signer's record with a lower version, and a later record from that
  signer, with a higher version, replaces the tombstone.
- A station stores or forwards a tombstone only after it verifies.
- Its `expires_at` is no earlier than the withdrawn record's, so a replayed copy of that record cannot return after the
  tombstone expires.

The tombstone payload holds exactly `withdrawn_type`, `withdrawn_version`, `reason`, the slot fields of the withdrawn
type, and optionally `detail`:

| Key | Type | Content |
|---|---|---|
| `withdrawn_type` | unsigned | the withdrawn record's type tag |
| `withdrawn_version` | bytes, 16 | the withdrawn record's `version` |
| `reason` | text | `shutdown`, `moved` or `revoked` |
| `detail` | text | optional, free text for people; no check reads it |

| Withdrawn type | Slot fields |
|---|---|
| node record, station endpoint, foundation seed list, foundation realm trust list | none |
| realm directory, realm stations | `realm_id`: bytes, 32 |
| realm member endorsement | `realm_id`: bytes, 32; `member_node`: bytes, 32 |
| procedure advertisement | `realm_id`: bytes, 32; `procedure`: text |
| foundation parameter | `param_name`: text |
| foundation T3 attestation | `station_id`: bytes, 32 |
| content announcement | `mcid`: bytes, 50 |
| org directory | `realm_id`: bytes, 32; `org_name`: text |
| procedure delegation | `advertiser`: bytes, 32 |
| domain type | `subject`: bytes, when the withdrawn record has one |

- Slot fields carry the names the withdrawn type's own payload uses.
- Where a storage key takes the signer's node_id or key id, as for a node record, a station endpoint, the foundation
  seed list, the foundation parameter, the foundation realm trust list, a procedure delegation and a domain type,
  that part is the tombstone's own key id, derived as the withdrawn type derives it, and the payload does not repeat
  it.
- `shutdown` withdraws a record because its signer stops serving, `moved` because a record in another slot replaces
  it, and `revoked` because its signer takes back what the record stated.

### Procedure advertisements

The payload of a procedure advertisement, type tag 0x06, holds exactly these keys:

| Key | Type | Content |
|---|---|---|
| `realm_id` | bytes, 32 | the realm id |
| `procedure` | text | the procedure name within the realm |
| `advertiser_node` | bytes, 32 | the provider's node_id, equal to the key id of `key` |
| `serving_station` | bytes, 32 | the node_id of the station that serves the provider |
| `authorization` | map | the provider authorization (D25 item 6): required with an org namespace, absent without |

- `authorization` holds either `org_directory` and `procedure_delegation`, each bytes, the wire form as received of
  the realm-signed org directory and of the org-signed procedure delegation that names the provider, or
  `certificate_chain`, an array of bytes, the provider's certificate chain in DER, leaf first. It holds nothing else.
- A verifier refuses an advertisement for a procedure with an org namespace that carries no `authorization`. An
  advertisement for a procedure without an org namespace carries none, since there is no delegation to check (D25);
  whether such procedures need an authorization of their own is open.
- The provider's signature covers `authorization`. The caller, and a serving station that gates a CALL, check each
  embedded record's own signature and validity, or the chain against the realm's trust anchor (D25 item 6).
- They also refuse an advertisement that expires later than the earliest expiry in its authorization: an embedded
  record's `expires_at`, or a certificate's notAfter. Renewing an authorization therefore means signing the
  advertisement again, at a new version.
- A station that stores or forwards an advertisement checks only the outer record, its signature and its own
  expiry, and never parses `authorization`.
- A consumer takes the realm and the procedure from these fields; no advertisement carries a procedure URI.

### Storage keys

Every DHT storage key is 32 bytes.

- A node record is stored under the signer's node_id, and a tombstone under the storage key of the record it withdraws.
- Every other record is stored under `SHA-256("MACULA-PQ-STORAGE-KEY-V1" || 0x00 || type || fields)`, with `type` one
  byte. Each 32-byte id enters as it is; each other field enters as a 4-byte big-endian length followed by its bytes.

| Type | Tag | Fields, in order |
|---|---|---|
| realm directory | 0x03 | realm id |
| realm stations | 0x04 | realm id |
| realm member endorsement | 0x05 | realm id, member node_id |
| procedure advertisement | 0x06 | realm id, procedure name |
| foundation seed list | 0x0D | foundation key id |
| foundation parameter | 0x0E | foundation key id, parameter name |
| foundation realm trust list | 0x0F | foundation key id |
| foundation T3 attestation | 0x10 | station node_id |
| content announcement | 0x11 | content id, 50 bytes and so length-prefixed |
| station endpoint | 0x12 | station node_id |
| org directory | 0x15 | realm id, org name |
| procedure delegation | 0x16 | org key id, advertiser node_id |
| domain type | 0x20 to 0xFF | signer key id, then the subject when present |

- A procedure name is the name within its realm; the realm enters only as the 32-byte realm id.
- A consumer computes a storage key from ids and names it already holds, before it holds the record.
- `macula-station` derives the same keys for procedure advertisements and content announcements (WP 1.6).

### Content ids

A content id is `<<Tag:8, Codec:8, Hash/binary>>` (D24). Tag 2 is SHA-384, with a 48-byte hash: 50 bytes in all.
Codec 0x55 names a single block, and codec 0x56 a manifest.

- **The post-quantum format has only tag 2.** A content id with any other tag is refused everywhere: on fetch, in
  a manifest, for a chunk, and in a content announcement.
- Every new block, manifest and chunk id is made with SHA-384, and a manifest names `sha384` as its hash algorithm,
  the only one it may name.
- A manifest's id is SHA-384 of the deterministic CBOR map of its canonical fields, with text keys: `name` as text,
  `size`, `chunk_size`, `chunk_count`, `hash_algorithm` as the text `sha384`, and `root_hash` as bytes. Every stack
  computes it from those values, whatever form the manifest arrived in.
- A block, a chunk and a reassembled whole are checked against the hash their own id names.
- The tag byte stays, so another algorithm can be added on purpose, under a new tag.

## Frame signatures

After HELLO, frames keep `version` 3 and a `frame_type`. A frame's signed parts are signed objects. Its routing fields,
which stations set or change per hop, stay outside them.

- Every field that named an Ed25519 key names a key id: `target` in SWIM, `origin` in DHT and overlay frames,
  `subscriber`, `advertiser` and `offending_hop`.
- Every `tbs` that belongs to one frame type holds `frame_type`, so a signature never serves another type.

### Neighbour signatures (D17)

| Frames | pq_pure | pq_hybrid |
|---|---|---|
| SWIM | none | neighbour |
| DHT protocol frames | none | neighbour |
| ADVERTISE and UNADVERTISE | none | neighbour |
| SUBSCRIBE and UNSUBSCRIBE | none | neighbour |
| The overlay relay envelope | none | neighbour |
| HyParView; Plumtree IHAVE, GRAFT and PRUNE; GOODBYE | none | neighbour |
| PUBLISH, EVENT and Plumtree GOSSIP | none | none |
| Content frames | none | none |
| Requests, replies, relay errors and stream frames | none | none |

- DHT protocol frames are PING, PONG, FIND_NODE, NODES, FIND_VALUE, VALUE, STORE, STORE_ACK, REPLICATE and
  REPLICATE_ACK. Content frames are WANT, HAVE, BLOCK, MANIFEST_REQ, MANIFEST_RES and CANCEL.
- HyParView, the Plumtree control frames and GOODBYE change membership, tree shape or a connection's lifecycle, so
  they are control frames. GOSSIP carries publications that are checked end to end, so it is data; its Plumtree
  routing fields stay unsigned in both profiles.
- A neighbour-signed frame is `{version, frame_type, neighbour}`. `neighbour` is `{tbs, signature}` under
  `MACULA-PQ-NEIGHBOUR-V1`, verified with the connection peer's identity key, and its `tbs` holds the frame's fields
  with `frame_type` and `alg`.
- A neighbour `tbs` also holds `connection`, the SHA-384 of the challenge frame's bytes from the connection's
  handshake, which both ends hold, and `seq`, 0 on the first neighbour-signed frame in each direction after HELLO
  and one more on each. A frame whose `connection` differs, or whose `seq` repeats or skips a number, is
  `malformed_frame`, and the connection closes. This adds about 55 bytes, so each signature serves once, on one
  connection, in one place.
- Neighbour-signed frames travel only on the connection's control stream, the first bidirectional stream, opened in
  the handshake. One arriving on any other stream is `malformed_frame`. Moving a control frame type to another
  stream needs a design change that puts the QUIC stream id in the neighbour `tbs` and counts `seq` per stream,
  because QUIC delivers streams independently.
- A frame whose type its profile signs is refused without `neighbour`, and a frame whose type its profile leaves
  unsigned is refused with it, both as `malformed_frame`.
- Records inside STORE, VALUE and REPLICATE keep their own signatures in both profiles.
- Requests, replies, relay errors, stream frames and publications carry their own signatures in both profiles.

### Requests: CALL and STREAM_OPEN

The frame is `{version, frame_type, request}`, with the routing fields `source_route` and `retry_budget` where they
apply. `request` is `{key, tbs, signature}` under `MACULA-PQ-REQUEST-V1`, and `key` is the caller's identity key.

| Key | Type | Content |
|---|---|---|
| `frame_type` | text | `call` or `stream_open` |
| `alg` | text | as for every signed object |
| `caller` | bytes, 32 | the caller's node_id, equal to the key id of `key` |
| `request_id` | bytes, 16 | the call id of a CALL, the stream id of a STREAM_OPEN |
| `realm` | bytes, 32 | the realm id |
| `procedure` | text | the procedure name within the realm |
| `target` | bytes, 32 | the provider's node_id, from its verified advertisement (D25 item 2) |
| `deadline` | unsigned, ms | absolute (D7) |
| `payload` | any | the arguments |
| `mode` | text | STREAM_OPEN only: `server_stream`, `client_stream` or `bidi` |
| `token` | bytes | optional: a capability token (WP 1.4) |

- **The request hash** is SHA-384 of `tbs`. It names the request in every reply and stream frame (D25 item 3).
  `tbs` holds `caller`, so two callers never share a request hash.
- **The deadline** is the request's acceptance deadline. A provider refuses to start a request after it, and refuses
  a request whose deadline lies more than 10 minutes past its clock; a procedure may set a lower maximum. The deadline
  is not a stream's lifetime: a stream lives on through its signed frames, their sequence numbers and its idle
  timeout. A station drops a CALL's forwarding state at `deadline` plus 5 minutes, so a reply arrives only for work
  that finishes by then; longer work belongs in a stream or an asynchronous pattern.
- **A provider** checks, in order, before any handler runs (D7 check 2):
  1. the signature;
  2. `caller` against the key id of `key`;
  3. `target` against its own node_id;
  4. `deadline` inside its clock minus 5 minutes and its clock plus 10 minutes;
  5. (`caller`, `request_id`) not seen before;
  6. the token's `aud` against `caller`;
  7. the token's chain, the costliest check, last.
- It keeps (`caller`, `request_id`) until `deadline` plus 5 minutes. A copy with the same request hash gets the stored
  signed reply, or the reply once the work finishes, along the path the copy came from. A copy with another request
  hash is refused. A provider bounds the stored reply bytes per caller; a reply beyond the bound is not kept, and a
  copy of its request is then refused.
- **A station** checks the signature and `caller`, and routes on `target`. It keeps forwarding state per connection
  the request was forwarded on and request hash, never per `request_id` alone, because past the first hop requests
  from many callers share one upstream connection. Entries per incoming connection have a configured maximum, and a
  request beyond it is not forwarded. An entry lives until `deadline` plus 5 minutes for a CALL, and until the
  stream ends or its idle limit passes for a stream.

### Replies: RESULT and ERROR from a provider

The frame is `{version, frame_type, reply}`, with the routing field `source_route_reverse`. `reply` is
`{key, tbs, signature}` under `MACULA-PQ-REPLY-V1`, and `key` is the provider's identity key.

| Key | Type | Content |
|---|---|---|
| `frame_type` | text | `result` or `error` |
| `alg` | text | |
| `request_id` | bytes, 16 | |
| `request_hash` | bytes, 48 | SHA-384 of the request's `tbs` |
| `responded_by` | bytes, 32 | the provider's node_id, equal to the key id of `key` |
| `payload` | any | RESULT only |
| `code`, `detail` | text | ERROR only; `detail` optional |

- The caller and every station on the path accept a reply only when `responded_by` equals the request's `target`, the
  signature verifies, and `request_hash` and `request_id` match the request (D25 item 4).

### Relay errors: ERROR and STREAM_ERROR from a station

The frame is `{version, frame_type, relay_error}`, with the routing field `source_route_partial`. `relay_error` is
`{key, tbs, signature}` under `MACULA-PQ-RELAY-ERROR-V1`, and `key` is the reporting station's identity key.

| Key | Type | Content |
|---|---|---|
| `frame_type` | text | `error` or `stream_error` |
| `alg` | text | |
| `request_id` | bytes, 16 | |
| `request_hash` | bytes, 48 | |
| `reported_by` | bytes, 32 | the station's node_id, equal to the key id of `key` |
| `code` | text | a relay error code, distinct from every provider code |
| `detail` | text | optional |
| `offending_hop` | bytes, 32 | optional |

- A station or the caller accepts a relay error only for a pending request whose `request_id` and `request_hash`
  match, arriving on the connection that request was forwarded on, or for the caller on its first-hop connection,
  and only when `reported_by` equals the key id of `key`.
- The relay codes are a closed set, disjoint from every provider code: `unknown_next_peer`. Each means the outcome
  is unknown, not that the request failed (D25 item 7), and adding a code changes this design. A station signs no
  RESULT.
- A station under load drops the requests it cannot forward. It sends no relay error for them, and the caller's
  deadline ends the call.
- `offending_hop` is advisory: a signed claim by `reported_by` that proves nothing about the named hop. A receiver
  logs it, and never lowers the named hop's standing on it.

### Provider stream frames: STREAM_DATA, STREAM_END, STREAM_ERROR and STREAM_REPLY

The frame is `{version, frame_type, stream}`. `stream` is under `MACULA-PQ-STREAM-V1` and signed by the provider's
identity key: `{key, tbs, signature}` on the provider's first frame of a stream, whatever its type, and
`{tbs, signature}` on every later one (D25 item 5).

| Key | Type | Content |
|---|---|---|
| `frame_type` | text | `stream_data`, `stream_end`, `stream_error` or `stream_reply` |
| `alg` | text | |
| `request_id` | bytes, 16 | the stream id |
| `request_hash` | bytes, 48 | SHA-384 of the STREAM_OPEN's `tbs` |
| `signer` | bytes, 32 | the provider's node_id |
| `seq` | unsigned | 0 on the provider's first frame, one more on each |
| `encoding`, `body` | text, any | STREAM_DATA only |
| `role` | text | STREAM_END only |
| `code`, `message` | text | STREAM_ERROR only |
| `payload` | any | STREAM_REPLY only |

- The first frame's key id equals `signer` and the STREAM_OPEN's `target`. Later frames verify with that key, which
  the stream's state holds; nothing is looked up.
- `seq` is the previous number plus one. STREAM_END is the provider's last frame, so no frame follows its number.
- A verifier that has not seen a stream's first provider frame refuses its later frames, and drops the key when the
  stream ends.
- A station on a stream's path holds the provider's and the caller's keys for each open stream, about 2.6 KB /
  3.1 KB each, and so bounds its open streams per connection.

### Caller stream frames: STREAM_DATA, STREAM_END and STREAM_ERROR from a caller

D17 as revised and accepted by Raf on 2026-09-11. Every frame a caller originates after STREAM_OPEN is signed, in
every stream mode: STREAM_DATA, STREAM_END and STREAM_ERROR in client_stream and bidi, and STREAM_END and
STREAM_ERROR in server_stream, where the caller sends no data. The frame is `{version, frame_type, caller_stream}`.
`caller_stream` is `{tbs, signature}` under `MACULA-PQ-CALLER-STREAM-V1`, verified with the caller's key from the
verified STREAM_OPEN, which is not carried again.

| Key | Type | Content |
|---|---|---|
| `frame_type` | text | `stream_data`, `stream_end` or `stream_error` |
| `alg` | text | |
| `request_id` | bytes, 16 | the stream id |
| `request_hash` | bytes, 48 | SHA-384 of the STREAM_OPEN's `tbs` |
| `signer` | bytes, 32 | the caller's node_id, equal to the STREAM_OPEN's `caller` |
| `seq` | unsigned | 0 on the caller's first frame, one more on each |
| `encoding`, `body` | text, any | STREAM_DATA only |
| `role` | text | STREAM_END only |
| `code`, `message` | text | STREAM_ERROR only |

- The caller's `seq` counts its own frames, apart from the provider's. STREAM_END is the caller's last frame, so no
  caller frame follows its number.
- The provider and every station refuse a caller frame whose number is not the previous one plus one.
- Each caller frame carries about 4.7 KB / 5.2 KB of signature, so callers send fewer, larger chunks.
- In a server_stream, a caller STREAM_DATA is `malformed_frame`.

### Publications: PUBLISH and EVENT

`publication` is `{key, tbs, signature}` under `MACULA-PQ-PUBLICATION-V1`, and `key` is the publisher's identity key.
PUBLISH is `{version, frame_type, publication}`; EVENT is `{version, frame_type, publication, delivered_via}`. The same
publication bytes ride in the PUBLISH and in every EVENT made from it, so `tbs` holds no `frame_type`.

| Key | Type | Content |
|---|---|---|
| `alg` | text | |
| `publisher` | bytes, 32 | the publisher's node_id, equal to the key id of `key` |
| `realm` | bytes, 32 | |
| `topic` | text | |
| `seq` | unsigned | |
| `published_at` | unsigned, ms | |
| `ttl_ms` | unsigned, ms | optional: how long the publication is delivered |
| `payload` | any | |

- The origin station verifies a publication before fan-out, and every subscriber verifies it before delivery (D17).
- A subscriber delivers a publication only when its `realm` equals a subscription's realm and its `topic` matches
  that subscription's topic or topic pattern.
- A subscriber delivers each publication at most once. It keeps the SHA-384 of each delivered publication's `tbs`
  until `published_at` plus `ttl_ms`, or 10 minutes, plus 5 minutes has passed.
- A verifier refuses a `published_at` more than 5 minutes ahead of its clock, and a publication whose `published_at`
  plus `ttl_ms` plus 5 minutes has passed. Without `ttl_ms`, a maximum age of 10 minutes applies.
- Plumtree deduplication and IHAVE bookkeeping key on the SHA-384 of the publication's `tbs`, never on an unsigned
  message id.
- `published_at` is part of the event an application receives.
- PUBLISH and SUBSCRIBE carry no capability token field (WP 1.4).

### Advertisements: ADVERTISE and UNADVERTISE

- ADVERTISE carries `advertisement`, the provider's signed procedure advertisement record (D25 item 1), whose payload
  holds the realm's provider authorization (item 6). Stations forward it unchanged, and a DHT VALUE returns it the
  same way.
- UNADVERTISE carries `withdrawal`, a tombstone signed by the provider in the advertisement's slot, with a later
  version (item 8). A lost withdrawal lasts at most until the advertisement's validity ends.

### Sizes

- A request, reply or relay error: about 7.3 KB / 8.3 KB before its payload. A token adds 11.2 KB / 12.9 KB (D7).
- Provider stream frames: the first about 7.3 KB / 8.3 KB, each later one about 4.7 KB / 5.2 KB.
- Caller stream frames: about 4.7 KB / 5.2 KB each.
- A publication: about 7.3 KB / 8.3 KB before its payload.
- A neighbour signature in pq_hybrid: about 5.2 KB per control frame.
