# Post-quantum handshake frames

This exists so every Macula node proves who it is on every connection with post-quantum keys, and every stack encodes
that proof byte for byte the same way.

Agreed by Mercury, Mars and Neptune on 2026-09-10, for WP 1.3 (the frame codec) and WP 1.5 (the handshake). It
specifies the frames of D16 and the status statements of D22 in `PLAN_POST_QUANTUM_SECURITY.md`.

## Encoding

- The codec: `<<Length:32/big, Cbor/binary>>`, where `Cbor` is one deterministic CBOR map (RFC 8949 section 4.2.1)
  with text keys.
- The post-quantum format is `version` 3. A frame with another version closes the handshake with
  `unsupported_version`.
- Sizes below are given as US / EU.
- **Signed structures travel as received bytes.** Each one is a CBOR map `{tbs: bstr, signature: bstr}`, where `tbs`
  is the deterministic CBOR encoding of the structure's fields. The signer signs `Label || 0x00 || tbs`. A verifier
  checks the signature over the `tbs` bytes it received, and only then decodes them. Nothing is re-encoded to verify,
  so stacks whose CBOR libraries order keys differently still agree.
- **Decoding rule.** A verifier refuses a frame or a `tbs` that has a duplicate map key, bytes after its top-level
  item, or a field of the wrong type or length (`node_id` 32 bytes, `binding_id` 16, `subject_hash` and `binding_hash`
  48), and closes with `malformed_frame`. CBOR decoders differ on duplicate keys, so without this two stacks could
  read different fields out of the same signed bytes. A verifier also refuses a key it does not know in a `tbs`: a
  new signed field needs a new label. Signers emit deterministic CBOR, and verifiers never re-encode CBOR to check it.
- The signature algorithm is the node key's: ML-DSA-87 in the US profile, Macula's composite ML-DSA-87-PS384 in the
  EU profile (D7).
- **Public keys travel in their carried form** (D13): 2,592 bytes / 3,118 bytes, the ML-DSA-87 key followed in the EU
  profile by the DER `RSAPublicKey`. A carried key has exactly one accepted encoding: the verifier decodes it,
  derives its carried form again, and refuses any other bytes with `malformed_frame`.

## Labels

Each structure has its own ASCII label, and none is a prefix of another.

| Structure | Label |
|---|---|
| TLS-key binding | `MACULA-PQ-BINDING-TLS-V1` |
| CONNECT-key binding | `MACULA-PQ-BINDING-CONNECT-V1` |
| Status statement | `MACULA-PQ-STATUS-V1` |
| CONNECT proof | `MACULA-PQ-CONNECT-PROOF-V1` |

Labels for relayed frames and publisher signatures follow in the frame signature work of WP 1.3, under the same
`MACULA-PQ-` prefix.

## Binding

Carried as `{tbs, signature}` in the challenge (TLS key) and in CONNECT (CONNECT key). Signed by the identity key.
The fields of `tbs`:

| Key | Type | Content |
|---|---|---|
| `label` | text | `MACULA-PQ-BINDING-TLS-V1` or `MACULA-PQ-BINDING-CONNECT-V1` |
| `node_id` | bytes, 32 | the issuer's node_id (D5) |
| `use` | text | `tls` or `connect`, matching the label |
| `subject_hash` | bytes, 48 | SHA-384 of the leaf DER as presented (`tls`), or of the carried CONNECT key (`connect`) |
| `binding_id` | bytes, 16 | random |
| `not_before`, `not_after` | unsigned, ms | at most 7 days apart (D22) |
| `hash_alg` | text | `SHA-384` |
| `sig_alg` | text | `ML-DSA-87` or `ML-DSA-87-PS384` |

`signature`: 4,627 / 5,139 bytes. About 4.8 KB / 5.3 KB in all.

- Neither subject is parsed: the client already hashes the leaf DER for the proof, and the carried CONNECT key has
  exactly one accepted encoding.
- **Leaf renewal and rotation.** A station makes a new leaf only when it rotates its TLS key, every 5 days (D22). Should
  its presented leaf change for any other reason, the same rule applies: the station issues the new TLS binding, and
  a status statement for it, before its listener presents that leaf. No connection ever sees a leaf without a
  matching, fresh binding.
- **Issuing.** A node issues a status statement every 15 minutes for each of its bindings whose `not_after` has not
  passed, each valid for 1 hour (D22). It rotates each TLS and CONNECT key every 5 days, and issues the new binding
  and a status statement for it before the new key is used. Across a rotation, a node keeps sending fresh statements
  for the old binding on the connections that use it, until that binding's `not_after`.
- **An open connection at its binding's `not_after`.** The connection closes with `binding_expired`. Its owner may
  dial a replacement with the new key before then, so a link need not have a gap; the old connection closes at
  `not_after` regardless.

## Status statement (D22)

Carried as `{tbs, signature}` in the challenge, in CONNECT and in the status frame. Signed by the identity key. The
fields of `tbs`:

| Key | Type | Content |
|---|---|---|
| `label` | text | `MACULA-PQ-STATUS-V1` |
| `node_id` | bytes, 32 | the issuer's node_id |
| `binding_hash` | bytes, 48 | SHA-384 of the binding's `tbs` bytes |
| `issued_at`, `expires_at` | unsigned, ms | at most 1 hour apart; reissued every 15 minutes |
| `sig_alg` | text | as in the binding |

`signature`: 4,627 / 5,139 bytes. About 4.7 KB / 5.2 KB in all.

- `binding_hash` covers `tbs` only. The signature is left out because ML-DSA and PSS signatures are randomized, so one
  binding can carry several valid signatures, and `binding_id` already makes each binding's `tbs` unique.
- The hash is taken over the same `tbs` bytes whose signature was just verified, never over a re-encoding, and the
  binding and its statement must verify under the same identity key with equal `node_id` fields.
- A verifier refuses a missing, expired or future-dated statement, with 5 minutes of tolerance, and a statement whose
  `binding_hash` does not match the binding it came with.

## Leaf bytes per stack

The TLS binding and the proof both hash the station's leaf certificate DER as presented. That only works if every
stack reads exactly those bytes, so:

- **Station certificates are strict DER.** A station hashes the leaf its listener presents on this connection, never a
  re-encoded file.
- **Erlang and `macula-rust`:** quinn's `Connection::peer_identity` returns the chain as received, leaf first ✅. The
  NIF exports the received leaf for client connections and the presented leaf for accepted connections (WP 1.2).
- **Go:** `PeerCertificates[0].Raw` holds the certificate as received ✅. A resumed session takes certificates from the
  stored session ✅, which is why no client session cache is set.
- **Python:** the aioquic patch keeps the raw leaf DER, byte-exact ✅ (V7). A resumed session has no peer certificate
  ✅, and resumption stays off.
- **.NET, out of the first switch (D10):** `QuicConnection.RemoteCertificate.RawData` is OpenSSL's re-serialization,
  equal to the received bytes only for a strict DER certificate ✅. Strict DER stations are what make it usable later.
- **A leaf that is not strict DER fails closed.** A stack that re-serializes would hash different bytes, so its
  binding check or the station's proof check fails; nothing is accepted on the strength of a wrong hash.
- **Shared vector (V16):** one strict DER station leaf and its SHA-384, which every stack's accessor must reproduce.

## Frames

### `opener`, client to station

The first frame on the control stream the client opens, in its first flight after the TLS handshake.

| Key | Content |
|---|---|
| `version`, `frame_type` | 3, `opener` |

Nothing else, and nothing that relates to identity. It is the station's cue to send the challenge on the same stream.

### `challenge`, station to client

| Key | Type | Content |
|---|---|---|
| `version`, `frame_type` | | 3, `challenge` |
| `nonce` | bytes, 32 | from a CSPRNG, fresh for this connection |
| `profile` | text | `pq_pure` or `pq_hybrid`; used only to name a refusal |
| `identity_key` | bytes | the station's identity key, carried form |
| `tls_binding` | map | `{tbs, signature}` for the leaf this connection presented |
| `tls_status` | map | `{tbs, signature}` status statement for that binding |

About 12.1 KB / 13.6 KB. Everything except the nonce is precomputed, so the station signs nothing per connection. The
station keeps the challenge frame's CBOR bytes, as sent, in the connection's state for the proof check. A client takes
its profile from its own configuration, never from this unsigned field.

### `connect`, client to station

| Key | Type | Content |
|---|---|---|
| `version`, `frame_type` | | 3, `connect` |
| `identity_key` | bytes | the client's identity key, carried form |
| `connect_key` | bytes | the client's CONNECT key, carried form |
| `connect_binding` | map | `{tbs, signature}` for that CONNECT key |
| `connect_status` | map | `{tbs, signature}` status statement for that binding |
| `proof` | bytes, 4,627 / 5,139 | by the CONNECT key, see below |
| `capabilities` | unsigned | the client's capability bits; the station stores them for its peer observer |

The proof signs this fixed-length concatenation, not a CBOR map:

```
MACULA-PQ-CONNECT-PROOF-V1 || 0x00
|| nonce (32)
|| station node_id (32)
|| client node_id (32)
|| SHA-384 of the station's leaf certificate DER as received in this TLS handshake (48)
|| SHA-384 of the challenge frame's CBOR bytes as received, without the length prefix (48)
```

- The station checks the proof against the leaf this connection presented, from the per-connection export of WP 1.2,
  never the currently configured certificate: a listener reloads its certificate on rotation (D22), and a connection
  can straddle a reload.
- The challenge hash binds the profile and the station's binding and status, not only the fields listed above.
- The station derives the client's node_id from `identity_key` (D5) and checks the puzzle on that node_id (WP 1.3)
  inside its CONNECT check, before any signature and before HELLO, under a mode of `off`, `log_only` or
  `enforce`: `enforce` refuses with `puzzle_invalid`, and `log_only` accepts and reports the unsolved puzzle.
- The version 2 fields `node_id`, `station_id`, `puzzle_evidence`, `realms`, `addresses`, `site` and `endorsements` are
  not part of CONNECT.

About 19.3 KB / 21.8 KB.

### `hello`, station to client

| Key | Type | Content |
|---|---|---|
| `version`, `frame_type` | | 3, `hello` |
| `accepted` | unsigned | 1 or 0 |
| `refusal_code` | text | only when `accepted` is 0, see below |
| `capabilities` | unsigned | the station's capability bits |

The version 2 fields `node_id`, `station_id`, `realms`, `addresses`, `site` and `negotiated_capabilities` are not part
of HELLO: both sides already derived the node_ids.

### `status`, either side on an open connection (D22)

`version` 3, `frame_type` `status`, and `statement`, a `{tbs, signature}` status statement. Sent at every reissue.
Each side keeps the peer's current expiry and a timer at expiry plus 5 minutes.
A status frame travels only on the control stream. It is not in the neighbour signature table of D17
(`DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md`), so it carries no neighbour signature in either profile and does not
count toward `seq`. When the timer fires before a fresh statement arrives, the connection closes with
`status_expired`; a statement that fails its checks closes it with that check's reason.

## Order, close reasons and refusal codes

- The client, after its opener, accepts only `challenge`, then only `hello`.
- The station accepts only `opener`, then only `connect`.
- Until HELLO, no frame other than the one expected next is acted on or queued for after the handshake. An unexpected
  or repeated frame closes the handshake at once with `unexpected_frame`, so an unproven peer cannot fill the handshake
  timeout with frames. The SDK and every port make this change together.
- The client runs every check on the challenge before it signs the proof, and closes without sending CONNECT if one
  fails.

**Close reasons are local.** Each side reports them to its owning process and to diagnostics, and never sends them:

- frames and format: `unexpected_frame`, `unsupported_version`, `malformed_frame`, `profile_mismatch`;
- identity:
  - `peer_identity_mismatch`, with the expected and the derived node_id, when a dial's expected node_id is not the
    station's derived node_id: not the station the dialer meant;
  - `node_id_mismatch`, when a binding or a statement names a node_id other than the one derived from the carried
    identity key: frames that are inconsistent with each other;
  - `key_purpose_reuse`, when one key would serve two purposes (D6, D16): the leaf carries the station's identity
    key or the client's CONNECT key, or a CONNECT key shares a half with its identity key;
  - `puzzle_invalid`;
- bindings: `binding_signature_invalid`, `binding_wrong_use`, `binding_key_mismatch`, `binding_expired`,
  `binding_not_yet_valid`;
- statements: `status_missing`, `status_expired`, `status_future_dated`, `status_signature_invalid`,
  `status_binding_mismatch`;
- proof: `proof_invalid`;
- lifecycle: `handshake_timeout`, `closed_during_handshake`, and `{refused, RefusalCode}` on a client that receives
  HELLO with `accepted` 0.

`binding_key_mismatch` means a TLS binding whose `subject_hash` differs from the hash of the leaf presented in this TLS
handshake, or a CONNECT binding whose `subject_hash` differs from the hash of `connect_key`. Transport failures, such as
a stream that cannot be opened or a failed send, stay outside this list.

**On the wire, a station that refuses CONNECT sends only HELLO with `accepted` 0 and one coarse refusal code**,
so a peer that has not proven its identity cannot learn which check failed:

| Refusal code | When |
|---|---|
| `unsupported_version` | the client's frames are not version 3 |
| `puzzle_invalid` | the client's derived node_id does not meet the puzzle, which the client can check itself |
| `not_accepted` | every other failed check on CONNECT |

## Sizes on the wire

- The challenge is not bound by QUIC Initial sizes or the anti-amplification limit: the station sends it only after
  the client's opener arrives in 1-RTT, when the address is validated (RFC 9000 section 8.1).
- Congestion control is the limit. quinn's initial window is min(10 x max_datagram_size, max(2 x max_datagram_size,
  14,720)), about 12 KB at 1,200-byte datagrams, and the station's certificate flight (about 7.4 KB leaf plus 4.6 KB
  CertificateVerify) already uses part of it. Expect the challenge to cost at most one extra round trip, and CONNECT
  probably one more.
- The frames fit the codec's 16 MiB cap, QUIC's stream and connection receive windows, and the 30-second handshake
  timeout.
- V9 measures round trips per profile with netem delay and a 1,200-byte path MTU.
