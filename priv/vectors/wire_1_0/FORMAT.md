# Wire 1.0 vector format

This exists so every stack can run the same wire 1.0 cases and agree on every byte. The contract they test is
`docs/design/WIRE_1_0.md`.


## Where

`priv/vectors/wire_1_0/` in macula, one JSON file per class, plus `docs/design/WIRE_1_0.md` (the spec page: check orders, refusal names, enum fallbacks, the LTS bounds clause). The directory ships in the hex package: a consumer reads it from `code:priv_dir(macula)` (or the package's `priv/` in another language), so it runs exactly the vectors of the macula release it pins.

| File | Class |
|---|---|
| `keys.json` | the fixed public keys, per profile and role, with their node_id / key id |
| `tbs.json` | canonical tbs bytes and signing inputs, every signed structure and its label |
| `frames.json` | one valid wire 1.0 frame per frame type, accepted by the envelope and field rules |
| `verify.json` | signed objects to verify under the fixed keys: accept with fields read, or refuse by name |
| `check_order.json` | objects with several faults: the refusal is the first in the spec page's check order |
| `handshake.json` | opener-to-HELLO transcripts: negotiation, proof V3, session proof, persisted floor |
| `custodian.json` | the custodian check set (signature, envelope, size, lifetime class); separate from readers |
| `dht_procedures.json` | `_dht.put_record`, `find_record`, `find_records`, `find_records_by_type` argument and reply shapes |
| `enums.json` | per-hop unknown enum values: the fallback and its counter; the same value inside a tbs: refused |
| `envelope.json` | frame negatives: unknown critical extension refused, non-critical ignored, unknown frame type ignored and counted, extra core field refused, v4 refused |

## File header

```json
{"wire": "1.0", "class": "verify", "format": 1, "design": "macula#83",
 "generator": "scripts/generate_wire_1_0_vectors.erl", "cases": [ ... ]}
```

## Values (typed JSON)

A JSON number is a CBOR integer (always below 2^53 in magnitude), a string is CBOR text, `{"$hex": "..."}` is a CBOR byte string, an object is a map with text keys, an array is an array, `null` is null. No floats and no booleans, as the decoding rule says. Raw encoded bytes (frames, tbs, signatures, keys) are lowercase hex strings in fields whose name says so (`tbs`, `frame`, `signature`, `key`).

## A case

```json
{"id": "record.node_record.verify.ok",
 "profile": "pq_pure",
 "covers": ["D4", "A3"],
 "lands_with": ["macula#100"],
 "input": { ... class-specific ... },
 "expect": {"outcome": "refuse", "reason": "lifetime_too_long"}}
```

- `id`: dotted, stable, never reused or renamed. Runners report by id.
- `profile`: `pq_pure`, `pq_hybrid`, or `both` when no signature is involved.
- `covers`: decision numbers from 6127410927 (`D1` to `D10`) and amendments from 6127591849 (`A1` to `A3`).
- `lands_with`: a list of the issues whose code makes the case pass (a station behaviour names `macula-station#94` too); the runner for that class arrives with each.
- `expect.outcome`: one of `accept`, `refuse`, `ignore`, `fallback`, `store`, `abort_stream`.
  - `refuse` carries `reason`, the refusal name as text; in a handshake step also `wire_refusal`, the HELLO `refusal_code` a peer sees.
  - `accept` may carry `fields` (typed JSON) that a reader must get back, and `count`, the counter that goes up (a
    signed object that carries a non-critical extension this node does not know is accepted, and the extension is
    counted by its name).
  - `ignore` and `fallback` carry `count`, the counter that goes up; `fallback` carries `value`, what the field reads as.
  - `abort_stream` carries `quic_code`.
- `now_ms`: the verifier's clock, in every case with a time check.
- `bound`: `"upper"` or `"lower"` on a case sitting exactly at a size or count bound. Under the LTS clause (bounds tighten, never loosen) a later vector version may turn a `bound` case from `accept` into `refuse`, and nothing else ever changes an expectation. Cases are only added.

## Class inputs, in short

- **tbs:** `label`, `signer` (role), `fields` (typed JSON) and `form`: `keyed` (signed objects: the signing input is `label || 0x00 || SHA-384(key as carried) || tbs`) or `unkeyed` (the handshake's bindings and status statements: `label || 0x00 || tbs`). `expect` holds `tbs` (hex, the canonical encoding a stack must produce from `fields`), `signing_input` (hex) and one `signature` over it, which verifies under the signer's key.
- **frames:** one valid wire 1.0 frame per frame type (`frame_type`, `frame` hex), each accepted by the envelope and the frame type's field rules. Signed objects inside are well formed and signed, but their clocks and request context are not part of this class; `verify`, `enums` and `tbs` cover signatures. `frames.register_ack.*` (section 3.5, no constructor yet) add field-rule negatives, refused as `malformed_frame`.
- **verify / check_order:** `object` (hex, the wire bytes as received), `signer` (role in `keys.json`), `now_ms`, and `signature_valid` (1 or 0: whether the signature verifies over the `tbs` as received with the bare primitive, a check any runner can make before reaching wire 1.0 code). A `check_order` case's `expect` carries `role`, `custodian` or `reader`. `key_profile`, where present, says the object carries the signer's key of that other profile (in its carried form) while `alg` is the case's profile's. `label`, where present, names the label the object's signing input is built with, for an object that is not a record: a `frame.*` case's object is the signed structure a CALL, RESULT, ERROR or STREAM_ERROR carries, and its `tbs` alone does not say which label it signs under (a provider ERROR and a relay ERROR are both `error` frames). `request`, where present, is the hex of the signed request object the case answers, so a reply or stream case reaches its `request_id`, `request_hash` and `responded_by` checks without holding a call table.
- **envelope, STATUS cases:** `status_peer` holds what the handshake verified of the sender: `identity` (role in `keys.json`) and its TLS `binding` (`tbs`, `signature`, hex); the frame's statement verifies against them at `now_ms`.
- **custodian:** `object_of`, the id of the `verify` case whose object it stores (the bytes are not repeated), `signer`, `now_ms`. A stored record of a tag the custodian does not know carries `expect.slot` (hex), the storage key it is placed at (WIRE_1_0.md 5.3).
- **handshake:** `tls` (`leaf_der`, `exporter` E, both hex: the parts TLS would supply), `now_ms`, `station` and `client` (each: `versions`, `profiles`, `capabilities`, `floor` as a list of `{node_id, version}`; the station adds `puzzle` `{difficulty, mode}`, the client `expected_station_node_id`), and `steps`: `[{"from": "client", "frame": "<hex>", "expect": {...}}, ...]`. The receiver of each step judges it, holding the frames it sent earlier in the transcript as sent (the challenge's nonce is fixed), so a runner needs no private key. A CONNECT step carries `proof_message` and an accepted HELLO `session_proof_message`, the exact bytes signed. A refusal on the station's side carries `wire_refusal`, the HELLO `refusal_code` the client sees; a client's own close reads `refused:<code>` when it is a HELLO refusal. The case's own `expect` repeats the last step's.
- **dht_procedures:** `procedure`, and `args` (hex CBOR, judged by a station) or `reply` (hex CBOR, judged by a caller), `now_ms`. An accepted reply's `expect` names its `result`; a `records` reply also says how many records a reader keeps (`records_kept`).
- **enums:** as envelope, plus `context` (hex frames received before this one on the same stream, such as the STREAM_OPEN a stream frame belongs to) and `now_ms`. `expect.field` names the enum; a `fallback` says what it reads as in `value`, and a refused one inside a tbs says `"reason": "enum_unknown"` with `field`.
- **enums / envelope:** `frame` (hex, one frame without the 4-byte length prefix), `position` (`control_stream`, `dedicated_stream`, `per_hop` or `tbs`), `after_hello` 1. A length case carries `length_header` (hex, the 4 bytes) instead of `frame`. A refusal that drops a frame but keeps the link says `"link": "stays_up"`.
- **Not in the files:** bounds whose vector would be hundreds of KiB of hex (the 256 KiB record cap, the 16 MiB frame cap past its header); each stack tests those itself, as with `decoding_rule_v1.json`.

`keys.json` holds public keys only (carried form) per profile and role (`station_identity`, `client_identity`, `client_connect`, `realm`, `org`, `foundation`). Signatures are randomized, so the files are committed as generated, as the existing vectors are; the generator's test-only private keys (`test/fixtures/wire_1_0_private_test_keys.etf`, not in the package) keep the keys fixed across regenerations, and no runner needs them.

## Red first, and CI

The vectors land on main as soon as each class is complete, together with one always-green self-check (every file parses, ids are unique, every `tbs` meets the decoding rule, every signature verifies under `keys.json` with the bare primitive, and every `signing_input` recomputes). Each class's conformance runner arrives with its `lands_with` issue, seen red on that branch and green when it lands, so main never carries a red suite. #97 closes when every class's runner passes in CI.
