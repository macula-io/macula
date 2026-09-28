# DESIGN: Neighbour channel binding

**This exists so a pq_hybrid station stops paying one RSA-4096 signature per control frame, without losing the
classical-strength authenticity D17 bought with it.**

| | |
|---|---|
| Kind | CLAIM (an authentication argument), then BUILD |
| Amends | D17 (neighbour signatures); takes D18 (the EU session proof) from "if ever taken" to built |
| Milestone | M2 |
| Status | Draft (Venus, 2026-09-29), for Mercurius (owner), Mars (station), then one Fable round; Saturnus words the register |
| Written against | macula v13.1.0 (47ab941e), macula-pqc v0.3.0, macula-go v0.19.0 |

---

## 1. Why

Pluto measured a pq_hybrid neighbour signature at 6.3 ms of CPU on the fleet class, and every control frame pays one
(D17): about 34 CPU-seconds of signing per station in an N=40 boot. Almost all of it is the RSA-4096 private-key
operation. On msi00 (i7-7700HQ, the pinned CI image) a whole composite sign takes 9314 µs, the RSA-PSS-4096 sign
alone 9939 µs, and the RSA key's DER decode 8.1 µs. Caching the decoded key would save about 0.1%, so it is not built
(decided 2026-09-29). The only lever is to stop signing each hop frame.

## 2. What D17 buys today, and why pq_pure already does without it

- In both profiles the TLS handshake signs with ML-DSA-87 alone (`macula_crypto_profile`: `tls_signature_scheme =>
  mldsa87`), and the key exchange is hybrid and pinned: SecP384r1MLKEM1024, then SecP256r1MLKEM768, nothing classical
  (macula-pqc `provider()`, identical on client and server).
- pq_pure signs no hop frame (`macula_frame:neighbour_signed(pq_pure, _) -> false`): under the connection handshake a
  neighbour signature adds nothing (D17's own "why").
- pq_hybrid signs every control frame, 25 types (`?NEIGHBOUR_SIGNED`), for one reason, D17's words: "it keeps
  classical-strength authenticity for membership, routing, subscriptions and advertisements if ML-DSA were broken."
  If ML-DSA fell, an attacker could complete a TLS handshake as a station, since the TLS signature is ML-DSA only,
  and speak on its behalf; the per-frame composite signature (ML-DSA-87 + RSA-PSS-4096) still stops that.

So the property to keep is exactly this: **if ML-DSA-87 is broken, a pq_hybrid node still cannot be made to accept a
control frame from anyone but its authenticated neighbour.**

## 3. The design

Authenticate the connection once, with the hybrid signatures bound to this TLS session, and let the session's AEAD
authenticate every frame after it. This is D18's session proof, taken.

- **The exporter.** Both ends compute `E = TLS-Exporter("EXPORTER-macula-session-v1", context, 32)` (RFC 8446 §7.5)
  over the QUIC connection's TLS 1.3 session, with `context = initiator node_id || acceptor node_id` (D18's order).
  It is unique to this session, and both ends derive it from the hybrid key exchange's secret.
- **The client's half.** The CONNECT proof (already signed by the CONNECT key, composite in pq_hybrid) covers, in
  handshake version 5, also `E` and both ends' `capabilities`:
  `label || 0x00 || nonce || station node_id || client node_id || SHA-384(leaf DER) || SHA-384(challenge) || E ||
  client capabilities || station capabilities`. The label becomes `MACULA-PQ-CONNECT-PROOF-V2`. No new field.
- **The station's half.** HELLO, in version 5, carries `session_proof`: a signature by the station's identity key
  (composite in pq_hybrid) over `MACULA-PQ-SESSION-PROOF-V1 || 0x00 || E || SHA-384(challenge) || SHA-384(CONNECT) ||
  station node_id || client node_id || accepted capabilities`. The client verifies it before it acts on any frame.
- **After HELLO.** With both proofs verified, neither end signs or expects a neighbour signature: control frames
  travel as pq_pure's do, `{version, frame_type, ...fields}`, on the control stream. A neighbour signature on a v5
  connection is `malformed_frame`, as a missing one is on v4.
- **Everything else is unchanged.** Records, advertisements, withdrawals, publications, requests, replies, relay
  errors and stream frames keep their own signatures in both profiles (§5).

### Why the property holds

- An attacker who can forge ML-DSA-87 but not RSA-PSS-4096 can complete TLS as a station, but cannot produce the
  composite `session_proof` over this session's `E`: the RSA half fails. A client refuses the connection before any
  frame. Symmetrically, it cannot produce a client's composite CONNECT proof over `E`.
- A proof from another session is useless: `E` is bound to this session's key exchange, and both node_ids are in the
  exporter context and the signed message.
- After the handshake, a frame is accepted only if it decrypts under this session's QUIC keys, which only the two
  authenticated ends hold. Those keys come from the hybrid key exchange, so they hold if either P-384 ECDH or
  ML-KEM-1024 holds. That is the channel's integrity, not a classical-only fallback: Mars's hard condition, met by the
  pinned groups (§2), and asserted at startup (§6).
- Control frames travel on one QUIC stream, which is reliable and ordered, and QUIC's packet numbers and AEAD refuse a
  replayed, reordered or injected packet. That is what D17's `connection` hash and per-direction `seq` checked per
  frame (Mars's requirement 2): a frame from another connection cannot decrypt, and a replay or reorder cannot reach
  the stream. The connection still closes on any refusal, as now.
- The relay trust D17 carries over. A station takes a relayed HyParView or Plumtree control frame with the relay's
  origin as its sender (`relayed_without_signature/1`), because the relay authenticated the originating connection.
  It still does, now by the session proof instead of per-frame signatures.

## 4. Mixed fleet and the gate

Handshake frames decode strictly against fixed layouts, and the handshake version is an exact match today (`?VERSION
4`), so a new field or version is refused by an old peer. The gate is therefore the version itself:

- **Stations first** accept versions 4 and 5 (Mars's requirement 4). A v4 CONNECT gets today's handshake and
  per-frame D17; a v5 CONNECT gets the session proof and no per-frame signatures.
- **Clients** (every SDK, and a station dialling another) send v5. Refused `unsupported_version` by an old peer, they
  retry once with v4 and count it. That is safe: forcing the fallback only brings back v4's per-frame signatures,
  which are secure, so a downgrade costs CPU, never authenticity.
- A peer that offers neither is refused with the named reason `unsupported_version`, as now.
- **A later release drops v4**, once the old-path counter (§6) reads zero across the fleet.
- **In scope:** macula (Erlang), then every SDK: macula-go (and through libmacula TypeScript, Python and .NET), and
  macula-rust. Every stack has the exporter: quinn 0.11 `export_keying_material` (Erlang NIF, Rust) and quic-go's
  `ConnectionState().TLS.ExportKeyingMaterial` (Go). D18 was held back because aioquic and .NET QUIC had none; both
  bindings now run on libmacula.
- **macula_dist_tunnel** also uses `macula_handshake` over OTP `ssl`; it has no D17 path and stays on v4 until it
  opts in (OTP `ssl:export_key_materials/4` would serve it).

## 5. Per-frame table (Mars's requirement 3)

"Hop-only" frames lose their pq_hybrid neighbour signature on v5 and are authenticated by the session. None of them
carries an end-to-end signature today, so none loses one. Frames that carry their own signature keep it, unchanged.

| Frame | pq_hybrid v4 (today) | pq_hybrid v5 | Own end-to-end signature, unchanged |
|---|---|---|---|
| swim_ping, swim_ack, swim_suspect, swim_confirm | neighbour | session | none |
| ping, pong | neighbour | session | none |
| find_node, nodes, find_value | neighbour | session | none |
| value | neighbour | session | each record's own |
| store | neighbour | session | the record's own (MACULA-PQ-RECORD-V1) |
| store_ack | neighbour | session | none |
| advertise, unadvertise | neighbour | session | the advertisement's / withdrawal's own record signature |
| subscribe, unsubscribe | neighbour | session | none |
| overlay_relay | neighbour | session | the inner frame's own, where it has one |
| hyparview_* (six), plumtree_ihave, graft, prune | neighbour | session | a realm endorsement where carried |
| goodbye | neighbour | session | none |
| call, stream_open, result, error, stream frames | none | none | caller / provider / relay-error signatures |
| publish, event, plumtree_gossip | none | none | publisher signature |
| want, have, block, manifest_req, manifest_res, cancel | none | none | none (addressed by mcid) |
| status | its own statement | its own statement | the station's signed status statement |

**No frame loses an end-to-end signature it has today.** The register's "Signed frames, verified end to end" row
stays true: it is about those signatures. What changes is how a hop authenticates its neighbour: once per session
with hybrid signatures, instead of once per frame.

## 6. Station transparency and counters (Mars's requirement 5)

- Signing and verifying live in `macula_peering_conn` and `macula_frame`. A station sends through
  `macula_peering:send_frame/2` and receives verified frames as messages, so the station changes only its macula
  dependency.
- New counters in `macula_peering`: connections by handshake version (v4, v5), `session_proof` refusals by reason
  (`session_proof_invalid`, `session_proof_missing`, `exporter_unavailable`), v4 fallbacks after
  `unsupported_version`, and control frames received on v4 connections (the old-path counter).
- At startup, macula refuses to run pq_hybrid v5 unless the QUIC provider's key exchange groups are exactly
  macula-pqc's hybrid ML-KEM groups (read from the NIF), so a classical-only build can never carry v5.

## 7. Cost (Mars's requirement 6)

- **Per control frame:** the neighbour sign (about 6.3 ms on the fleet class) and verify (0.16 to 0.18 ms) are gone.
  What remains is QUIC's AEAD, which already covers every byte today. Target: at most 0.05 ms of protect plus verify
  per frame on the fleet class, measured by Pluto's harness before and after.
- **Per connection:** the station signs one composite `session_proof` (one composite sign, within Mars's budget) and
  verifies the client's proof as now. The client verifies one composite signature more. Two exporter computations.
- **Measured before release:** per-frame cost, per-connection handshake time, and Pluto's N=40 boot CPU.

## 8. Tests, red first

- A v5 connection carries control frames with no neighbour signature both ways; a neighbour signature on v5 is
  `malformed_frame`.
- A `session_proof` over another session's exporter, another node pair, or signed by another key: refused, and the
  connection closes. A v5 HELLO with no `session_proof` is refused.
- With the RSA half of the station's proof replaced by a valid signature from another RSA key (the ML-DSA half left
  valid), the client refuses: the "ML-DSA broken" case.
- A v4 client against a v5 station: v4 handshake, per-frame D17, counted. A v5 client against a v4-only station:
  refused, retries v4 once, counted.
- The Erlang, Go and Rust exporters agree byte for byte on a shared vector (label, context, session), and a
  cross-stack interop run connects each SDK to an Erlang station on v5, both profiles.
- A pq_hybrid v5 node refuses to start with a key exchange group list other than macula-pqc's.

## 9. Open questions for the reviewers

1. **pq_pure on v5?** This note gives v5 to both profiles (one handshake), so pq_pure also gets the session proof: one
   ML-DSA-87 sign per connection on the station (about 1.1 ms), and a binding to the session instead of only to the
   leaf certificate. The alternative keeps pq_pure on v4's layout with a profile-specific HELLO.
2. **The station's proof key:** the identity key (as here) or a per-station CONNECT-like key? D18 says "the CONNECT
   keys carry it"; a station has no CONNECT key today.
3. **The fallback retry** on `unsupported_version`: always, or only while a config flag says the fleet is mixed?
