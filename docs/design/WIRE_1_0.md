# Wire 1.0

This exists so an operator runs a station for years on security fixes alone, and new mesh features reach it without
an upgrade or a partition: every SDK and station release proves it speaks wire 1.0 exactly, so an LTS station never
meets a peer that disagrees on a byte.

Design of record: macula#83 (decisions comment 6127410927, amendments 6127591849), referred to below as D1 to D10 and
A1 to A3. This page pins the bytes, names and orders those decisions leave to the spec. The conformance vectors in
`test/vectors/wire_1_0/` are its executable form; where this page and a vector disagree, that is a bug in one of
them, reported on macula#97.

Wire 1.0 keeps everything in `DESIGN_PQ_HANDSHAKE_FRAMES.md`, `DESIGN_NEIGHBOUR_CHANNEL_BINDING.md`,
`DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md` and `DESIGN_PQ_DHT_SLOTS_AND_BUDGET.md` that this page does not change.

## 1. The LTS clause

- The frozen core (D1): the handshake, the frame envelope, routing frames, DHT record types and their signing rules,
  and the zero-realm `_dht.*` procedures with their argument and reply shapes.
- **Bounds may tighten under LTS, never loosen.** A size, count or time bound may become stricter in a later wire 1.x
  release; it never becomes looser. In the vectors, only a case marked `bound` may turn from `accept` to `refuse`;
  no other expectation ever changes, and cases are only added.
- A handshake version bump is a wire major. There is no handshake v6 inside wire 1.x; features arrive as capability
  bits (section 4) and extensions (section 3).

## 2. Versions

- **Handshake version 5 only (D2).** Version 4 and proof V1 are removed. A v4 opener is answered with HELLO
  `unsupported_version` naming the station's ranges (section 4.4).
- **Frame version = handshake version (D3).** Every frame on a wire 1.0 connection, handshake frames and all frames
  after HELLO, carries `version` 5. The frame version 2 in the code and 3 in `DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md`
  are both retired. A frame with another `version` after HELLO is `malformed_frame`.
- The dead version 2 `connect` and `hello` frame types leave the post-handshake table: after HELLO they are unknown
  frame types (section 3.3).

## 3. Frame envelope (macula#99)

### 3.1 Shape

`<<Length:32/big, Cbor/binary>>`, `Cbor` one deterministic CBOR map with text keys, `Length` at most 16,777,215
(`frame_too_large`, read from the four header bytes alone). The handshake reads with a 65,536-byte cap.

Envelope keys, the same for every frame type:

| Key | Type | |
|---|---|---|
| `version` | unsigned | 5 |
| `frame_type` | text, 1 to 64 bytes | |
| `frame_id` | bytes, 16 | optional |
| `sent_at_ms` | unsigned | optional |
| `realm` | bytes, 32 | optional, where the frame type routes by realm |
| `call_id` | bytes, 16 | optional, where the frame type has one |
| `source_route` | array | optional, where the frame type has one |
| `ext` | map | optional, section 3.2 |

- **The unsigned per-frame `capabilities` header is dropped** (A, clarifications). A frame that carries it is
  `malformed_frame` under the extra-field rule.
- **Extra core field.** A key that is neither an envelope key nor a field of the frame type is `malformed_frame`.
  New fields arrive only in `ext`.

### 3.2 Extensions (D8)

- `ext` is a map from extension name (text, 1 to 64 bytes, `[a-z0-9_.]`) to `{critical, value}`: `critical` the
  unsigned 0 or 1, `value` any CBOR value the decoding rule accepts. At most 16 entries. An empty `ext` is
  `malformed_frame`: a frame with no extensions omits the key, so one frame has one encoding.
- In a signed object, `ext` sits **inside the `tbs`**, so `critical` is covered by the signature. Relays forward the
  signed bytes untouched. In an unsigned frame `ext` sits in the envelope and is per-hop.
- **Per-hop extensions on control frames are unauthenticated beyond the TLS channel** in both profiles (D8, A
  clarifications): the session proof binds the connection, not each frame's `ext`.
- A receiver that does not know an extension:
  - `critical` 1: refuses the frame, `extension_unsupported`. A signed object is refused by its verifier; an unsigned
    frame is dropped and counted, and on the control stream the link stays up.
  - `critical` 0: ignores it and counts `wire_ext_ignored`.
- An extension is announced by a capability bit; a node sends a critical extension only to a peer that declared the
  bit.

### 3.3 Unknown frame types (D5)

- A receiver decodes the envelope: the length bound, the CBOR rules and depth budget, `version` 5, and `frame_type`
  text of 1 to 64 bytes. A frame that fails any of these is `malformed_frame`, as today.
- A `frame_type` it does not know: the body is not validated, the frame is never acted on and never forwarded, and
  `wire_unknown_frame_type` is counted. The link stays up.
- On a dedicated stream (a stream opened for one exchange, such as a CALL stream), an unknown frame type aborts that
  stream with QUIC application code **5, `QUIC_CODE_UNKNOWN_FRAME_TYPE`**, distinct from 3 ("did not decode"). The
  link stays up.
- Capability bits gate what a node sends: a new frame type goes only to a peer that declared it.

### 3.4 Unknown enum values (D6)

An unknown value never closes a link.

| Enum | Where | Unknown value |
|---|---|---|
| HELLO `refusal_code` | per-hop | reads as `not_accepted` |
| HyParView NEIGHBOR `priority` | per-hop | reads as `low` |
| EVENT `delivered_via` | per-hop | reads as `direct` |
| SWIM membership `state` | per-hop | the entry is ignored, the rest of the frame is read |
| STREAM_OPEN `mode` | in the request `tbs` | the request is refused, `enum_unknown` naming `mode` |
| STREAM_DATA `encoding` | in the stream frame `tbs` | refused, `enum_unknown` naming `encoding` |
| STREAM_END `role` | in the stream frame `tbs` | refused, `enum_unknown` naming `role` |
| relay error `code` | in the relay error `tbs` | refused, `enum_unknown` naming `code` |

Each per-hop fallback counts `wire_enum_fallback` with the field's name. A refusal inside a `tbs` refuses that
signed object, by name, never the link.

## 4. Handshake (macula#98)

Unchanged from `DESIGN_NEIGHBOUR_CHANNEL_BINDING.md` except as follows.

### 4.1 Negotiation fields (D3, D7, A2)

- `versions`: an array `[min, max]` of unsigned integers, `min <= max`. Wire 1.0 sends `[5, 5]`.
- `profiles`: an array of 1 to 8 distinct profile names (text), in the sender's order of preference.
- **opener**: `{version 5, frame_type, versions, profiles}`, the client's ranges.
- **challenge**: its `version` is the chosen version and its `profile` the chosen profile; it adds `versions` and
  `profiles`, the station's ranges.
- **connect**: adds `versions` and `profiles`, equal to the opener's, and `profile`, the chosen profile.
- The station chooses the highest version in both ranges and the first profile in the client's list that it
  supports. Policy: a node supports N and N-1 of the versions it knows; in wire 1.0 that is 5 alone.

### 4.2 Proof V3 (D2, A1)

The client proof, by the CONNECT key, label `MACULA-PQ-CONNECT-PROOF-V3`, over the fixed-width concatenation:

```
MACULA-PQ-CONNECT-PROOF-V3 || 0x00
|| nonce (32)
|| station node_id (32)
|| client node_id (32)
|| SHA-384 of the station's leaf DER as received (48)
|| SHA-384 of the challenge frame's CBOR as received (48)
|| E, the session exporter value (32)
|| SHA-384 of the CONNECT tbs (48)
```

The **CONNECT tbs** is the deterministic CBOR of the CONNECT frame with `proof` set to the zero-length byte string,
without the length prefix. It covers the capabilities (no fixed width), the client's ranges, the chosen profile and
`member_endorsement`. The station rebuilds it from the CONNECT it received by replacing the `proof` value with
`h''` and re-encoding deterministically; a CONNECT whose received bytes are not deterministic CBOR is
`malformed_frame`, so the rebuild is exact.

### 4.3 Session proof

Unchanged, `MACULA-PQ-SESSION-PROOF-V1`: its `SHA-384(challenge)` covers the chosen version and profile and the
station's ranges, and its `SHA-384(CONNECT)` the client's. A tampered negotiation fails one of the two proofs.

### 4.4 Refusals before the challenge

When the ranges share no version, or the lists no profile, the station answers the opener with HELLO
`{version 5, frame_type hello, accepted 0, refusal_code, versions, profiles}`, the station's ranges, with
`refusal_code` `unsupported_version` or `profile_unsupported`, and closes. No challenge is sent.

HELLO `refusal_code` values: `unsupported_version`, `profile_unsupported`, `puzzle_invalid`, `session_proof_rate`,
`not_accepted`. An unknown value reads as `not_accepted` (section 3.4).

### 4.5 Local checks and close reasons

Added to the close reasons of the handshake design:

- `downgrade_detected`: the client finds that the challenge's chosen version or profile is not the highest common
  one of its own ranges and the station's repeated ranges.
- `negotiation_mismatch`: the station finds the CONNECT's `versions` or `profiles` differ from the opener's.
- `version_below_floor`: the chosen version is below the floor recorded for that node id (4.6).
- `profile_unsupported`: a CONNECT names a profile the station did not choose, or no verifier is registered for it.

### 4.6 Version floor

A node records, per peer node id, the highest handshake version it completed with, and never completes below it.
The record survives restarts. An SDK host with no writable store keeps it for the run (A clarifications).

### 4.7 Capabilities

- `capabilities` in CONNECT and HELLO is an unsigned integer below 2^53; bits 0 to 52 are the reserved range.
- Assigned: bit 0 `station`, bit 1 `swim_indirect`, bit 2 `relay_overloaded`. Bits never retire and are never
  reassigned.
- An unknown bit is ignored. Each capability states its behaviour towards a peer that lacks it and towards one that
  claims it falsely: waste, never loss, both ways (D2).

## 5. Records (macula#100)

### 5.1 The tbs

The record `tbs` of `DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md` gains two keys, required on every record:

| Key | Type | Content |
|---|---|---|
| `schema` | unsigned, 1 to 65535 | the per-type schema version; readers check it, custodians do not |
| `lifetime_class` | unsigned | a row of the frozen table below |

| `lifetime_class` | Maximum lifetime |
|---|---|
| 0 | 5 minutes |
| 1 | 30 minutes |
| 2 | 6 hours |
| 3 | 48 hours |
| 4 | 7 days |
| 5 | 30 days |

The table is frozen: rows are only added, and a row's maximum never grows. A known type names exactly its row:
node and content announcement 3; procedure advertisement and station endpoint 0; realm stations and org directory 2;
procedure delegation and app record 1; realm member endorsement 5; domain types 4; other built-in types 5. A
tombstone names the row of the type it withdraws.

`ext` (section 3.2) may appear in a record `tbs` like any other signed object.

### 5.2 Custodian and reader (D4)

A **custodian** (a station storing a record) runs, in order:

1. size: the wire form at most 256 KiB, else `record_too_large`;
2. envelope: `{key, tbs, signature}`, each bytes, `key` in a carried form of a registered profile, else `malformed`;
3. signature over the `tbs` as received, with the verifier `alg` selects, else `signature_invalid`, or
   `alg_unsupported` for an `alg` with no registered verifier;
4. the `tbs` under the decoding rule, with exactly the envelope keys (`type`, `alg`, `version`, `created_at`,
   `expires_at`, `schema`, `lifetime_class`, `payload`, and `subject` and `ext` where present); `payload` must
   decode but is not read; else `malformed`;
5. the clock: `not_yet_valid`, `expired`, with 5 minutes of tolerance;
6. the lifetime: `lifetime_reversed`, `lifetime_class_unknown`, `lifetime_too_long` (against the table row);
7. the slot: a known type's own rule; any other type needs `subject`, else `subject_missing`, and is placed at
   `H(type | signer key id | subject)` (A3).

A custodian stores what passes, opaquely, whatever the type: it never reads a payload and never checks `schema`.

A **reader** (a node acting on a record) runs the custodian checks, then:

8. the type is one it knows, else `type_unknown`; its `schema` is one it reads, else `schema_unsupported`; its
   `lifetime_class` is the type's row, else `lifetime_class_mismatch`;
9. the type's payload rules, else `malformed`; the signer named in the payload, else `key_id_mismatch`;
10. a content id whose tag it does not know: `mcid_tag_unknown` (custodians store such records).

### 5.3 Tags

- **Every post-1.0 built-in tag carries `subject`**, and its slot follows the domain-type rule, H(type | signer key
  id | subject) (A3). Types that need many signers under one subject are wire 2.0 records or mcl-* services.
- **0x13** `station_authorization` (macula-station#63) and the K > 1 advertisement shape (macula-station#55) land on
  these rules, in 1.0.
- **0x14 is reserved** for the successor / key-rotation record (D7, A clarifications); its design comes later, and
  until then a custodian stores it like any unknown built-in tag.

### 5.4 Content ids

`<<Tag:8, Codec:8, Digest/binary>>`. Tag 2 is SHA-384 (48-byte digest). Custodians store a record naming any tag;
readers refuse one they do not know (`mcid_tag_unknown`).

## 6. `_dht.*` procedures (macula#102)

Served in the zero realm by every station. One argument form: a CBOR map with text keys and a `v` field.

| Procedure | Arguments | Reply |
|---|---|---|
| `_dht.put_record` | `{v 1, record: bytes}`, the wire record | `{v 1, result "ok"}` or `{v 1, result "refused", reason: text}` |
| `_dht.find_record` | `{v 1, key: bytes 32}` | `{v 1, result "record", record: bytes}` or `{v 1, result "not_found"}` |
| `_dht.find_records` | `{v 1, key: bytes 32}` | `{v 1, result "records", records: [bytes]}` |
| `_dht.find_records_by_type` | `{v 1, type: 0..255}` | `{v 1, result "records", records: [bytes]}` |

- A station refuses arguments with another `v`, a missing or extra key, or a field of the wrong type or length as
  `malformed_args`, before any DHT work.
- A caller refuses a reply with an unknown `v` as `reply_version_unsupported`, and one with a missing or extra key as
  `malformed_reply`. `records` may be empty. A caller verifies every record it gets back as a reader (5.2) and drops
  those that fail.
- `reason` in a refused put is the custodian's refusal name (5.2), at most 64 bytes.

## 7. Algorithm agility (macula#101)

- CONNECT names its `profile` (4.1); the verifier dispatches on it from a registry; an unregistered profile is
  `profile_unsupported`.
- `alg` in a signed object selects its verifier; an unregistered `alg` is `alg_unsupported`.
- The seal `scheme` selects KEM, KDF, AEAD and the ciphertext layout; an unknown scheme is `sealed_refused`.
- No hash identifier: a new hash arrives under a new alg, profile or scheme name and a -V2 label. Hard-coded: SHA-256
  in NODE-ID-V1 and STORAGE-KEY-V1, the scheme-1 internals, the ML-DSA-87 sizes under their alg names, the 1-byte
  record type, the exporter label, the puzzle difficulty and the TLS groups.

## 8. Counters

| Counter | Labels |
|---|---|
| `wire_unknown_frame_type` | none |
| `wire_ext_ignored` | extension name, truncated to 64 bytes |
| `wire_enum_fallback` | field name |

## 9. Vectors

`test/vectors/wire_1_0/`, one file per class; the format is on macula#97 (comment 6128214679) and in
`test/vectors/wire_1_0/FORMAT.md`. Every stack's CI runs them, pinned by macula commit.
