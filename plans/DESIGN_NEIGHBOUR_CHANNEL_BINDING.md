# DESIGN: Neighbour channel binding

**This exists so a pq_hybrid station stops paying one RSA-4096 signature per control frame, without losing the
classical-strength authenticity D17 bought with it.**

| | |
|---|---|
| Kind | CLAIM (an authentication argument), then BUILD |
| Amends | D17 (neighbour signatures); takes D18 (the EU session proof) from "if ever taken" to built |
| Milestone | M2 |
| Status | Accepted (2026-09-29): Mars (station) and Mercurius (macula) approved 01ac730f after Fable round 1; register row by Saturnus; building in macula (Venus) |
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
- pq_hybrid signs every control frame, 27 types (`?NEIGHBOUR_SIGNED`), for one reason, D17's words: "it keeps
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
- **The client's half.** The CONNECT proof (signed by the client's CONNECT key, never its identity key: one key, one
  purpose, D6/D16; composite in pq_hybrid) covers, in
  handshake version 5, also `E` and the client's `capabilities`:
  `label || 0x00 || nonce || station node_id || client node_id || SHA-384(leaf DER) || SHA-384(challenge) || E ||
  client capabilities`. The label becomes `MACULA-PQ-CONNECT-PROOF-V2`. No new field. The station's capabilities are
  not in it: they first reach the client in HELLO, after it has signed, and the station's own proof covers them.
- **Encoding.** Every field in a signed concatenation has a fixed width: node_ids 32 bytes, hashes 48, `E` 32, the
  nonce 32, and a capabilities integer 8 bytes big-endian (below 2^53, the decoding rule). No length prefixes are
  needed and no two field sequences encode to the same bytes. The shared vectors (§8) fix this byte order.
- **The station's half.** HELLO, in version 5, gains one field, `session_proof`: a signature by the station's identity key
  (composite in pq_hybrid) over `MACULA-PQ-SESSION-PROOF-V1 || 0x00 || E || SHA-384(challenge) || SHA-384(CONNECT) ||
  station node_id || client node_id || station capabilities`. `SHA-384(CONNECT)` covers the client's
  capabilities. The client verifies it before it acts on any frame.
  The label gives the proof its own domain: no record, statement or frame signature uses it, so the identity key's
  signature over it can never be taken for one of those, or one of those for it.
- **After HELLO.** With both proofs verified, neither end signs or expects a neighbour signature: control frames
  travel as pq_pure's do, `{version, frame_type, ...fields}`, on the control stream. A neighbour signature on a v5
  connection is `malformed_frame`, as a missing one is on v4.
- **Liveness on v5 (Mars; decided by Mercurius).** Today `macula_peering_conn:send_liveness_probe/1` sends a
  signed `_macula.ping` CALL in the all-zero realm on every opted-in connection, and the peer answers with a signed
  `unknown_next_peer` relay error: two composite signs per connection per interval, which §5 would leave in place
  because CALL and relay errors keep their own signatures. v5 drops that probe. It gets its own two control frame
  types, `liveness_ping` and `liveness_pong` (v5 only, session-authenticated, no signature, a 16-byte nonce): the
  peer's peering conn process answers `liveness_ping` itself, and the probing conn consumes the `liveness_pong`;
  neither is passed to the DHT pid. That proves the peer's VM and connection are alive, which is all
  `peer_liveness_lost` needs; a dead VM has no conn process to answer.
- **The DHT's `ping` and `pong` are not touched.** They reach the DHT pid on v4 and v5 alike, and the peer's DHT
  answers them, because they are different evidence (Mercurius): a pong from the peer's DHT proves that DHT can
  still serve, which is what a routing-table entry's liveness means. A conn that answered DHT pings would keep a
  wedged DHT's routing entries alive while it answers no lookups. One pong per ping, from the right process, and
  the station code does not change.
- **Everything else is unchanged.** Records, advertisements, withdrawals, publications, requests, replies, relay
  errors and stream frames keep their own signatures in both profiles (§5).

### Why the property holds

- An attacker who can forge ML-DSA-87 but not RSA-PSS-4096 can complete TLS as a station, but cannot produce the
  composite `session_proof` over this session's `E`: the RSA half fails. A client refuses the connection before any
  frame. Symmetrically, it cannot produce a client's composite CONNECT proof over `E`.
- A proof from another session is useless: `E` is bound to this session's key exchange, and both node_ids are in the
  exporter context and the signed message.
- After the handshake, a frame is accepted only if it decrypts under this session's QUIC keys, which only the two
  authenticated ends hold. Those keys come from the hybrid key exchange, so they hold if either half of the
  negotiated group holds: P-384 ECDH or ML-KEM-1024, or, on the second pinned group, P-256 ECDH or ML-KEM-768. That is the channel's integrity, not a classical-only fallback: Mars's hard condition, met by the
  pinned groups (§2), and asserted at startup (§6).
- Control frames travel on the connection's QUIC streams, which are reliable and ordered, and QUIC's packet numbers
  and AEAD refuse a replayed, reordered or injected 1-RTT packet. That is what D17's `connection` hash and
  per-direction `seq` checked per frame (Mars's requirement 2): a frame from another connection cannot decrypt, and a
  replay or reorder cannot reach a stream. The connection still closes on any refusal, as now.
- **Only 1-RTT, asserted (Mercurius).** QUIC 0-RTT early data is replayable by design, so dropping D17's seq holds
  only if no frame ever travels in 0-RTT. Nothing in `native/macula_quic` enables early data and rustls defaults
  `max_early_data_size` to 0, so it is off today; v5 makes it an invariant. A v5 node refuses to start if its QUIC
  configuration offers or accepts 0-RTT, on both sides, beside the group check (§6), with a test.
- **No resumption (D16).** D16 decided against resumption, but `native/macula_quic` never turned off rustls's
  default, which resumes. v5 disables it on both sides (`Resumption::disabled()` on the client,
  `NoServerSessionStorage` on the server). Every v5 connection is a full handshake with
  certificates, and 0-RTT cannot arise without a ticket; the start check covers both.
- **A downgrade, and how it is closed (Mercurius).** The v4 fallback's trigger, `unsupported_version`, is a handshake
  refusal that arrives inside a TLS session before any session proof. An attacker who can forge ML-DSA-87 terminates
  that TLS session and can inject it. On v4 that attacker, holding both TLS sessions, cannot inject a control frame
  (the per-frame signatures stop it), but can READ every one: v5 is what defeats that reading. So a forced fallback
  would give back, against that attacker, the confidentiality of control traffic; per-frame authenticity survives it.
  v5 closes it: once a client has completed v5 with a station node_id, it refuses a later `unsupported_version` from
  that node_id for the rest of the run (no retry), counted as `v5_downgrade_refused` (§6), tested (§8). What remains:
  an attacker on the path can force **every connection to a node onto v4 until one v5 handshake with it completes**,
  and against an ML-DSA-breaking attacker those connections' control traffic is readable though still authentic.
  The register says so in those words. To make that loud, fallbacks are counted per node_id: the first is
  expected during the roll, and from the second to the same node_id in a run a warning is logged naming the node_id
  and the count, at most once a minute per node_id. No cap: a cap would refuse a genuinely old station and
  partition the fleet during the roll, the failure §4 exists to avoid.
- **A rollback meets the same refusal (Mars).** Rolling a station back below v5 (pinning the previous release) looks,
  to every peer that saw it on v5 in this run, exactly like the downgrade above: they refuse it with no v4 fallback,
  until they restart (their outbound links keep re-dialling on v5 and are refused each time, see below). The refusal is one-directional. Observed (Mars, a six-station test harness on one
  host, 2026-09-29, 90 s after the rollback): the rolled-back station's own v4 dials are accepted, since a v5
  station answers v4, and SWIM liveness, routing-table membership and DHT pings keep working both ways over the
  connections it opens; only its peers' dials are refused, and they keep retrying into that refusal, a small steady
  handshake load, until `forget_v5_peer/1` or their restart. A rolled-back station that dials no peers is cut off
  from them, and a client that saw it on v5 cannot reach it, since a station never dials clients (both by design;
  not measured). The protection stays "for the run"; nothing expires it on a timer, because a timer is also the
  attacker's wait. What keeps this from being a silent partition: every `v5_downgrade_refused` is logged with the
  node_id and the count, bounded like the fallback warning, not only counted; and the remedy is stated here and in
  the deployment guide when v5 ships: rolling a station back below v5 needs its peers restarted, or
  `macula_peering:forget_v5_peer/1` called on them for that node_id (an operator action, never automatic).
- **Signing cost as an attack surface (Mars).** The station verifies the client's CONNECT proof (one composite verify,
  about 0.17 ms) BEFORE it signs `session_proof` (one composite sign, 6 to 10 ms), so only a client that has just
  produced a valid composite proof over this session makes the station sign, once. That client paid the same RSA-4096
  sign to get there. For comparison, v4 signs nothing per connection in the handshake (its challenge material is
  precomputed by `macula_statement_issuer`), but signs every control frame it sends, so today a peer can make a
  station sign once per `ping` over one connection (each ping signed by the peer too). v5 removes that. Session proofs are also
  rate-limited per client node_id and in total, refused with the named reason `session_proof_rate` and counted (§6).
- The relay trust D17 carries over. A station takes a relayed HyParView or Plumtree control frame with the relay's
  origin as its sender (`relayed_without_signature/1`), because the relay authenticated the originating connection.
  It still does, now by the session proof instead of per-frame signatures.

## 4. Mixed fleet and the gate

Handshake frames decode strictly against fixed layouts, and the handshake version is an exact match today (`?VERSION
4`), so a new field or version is refused by an old peer. The gate is therefore the version itself, and it first
appears in CONNECT:

- **Opener and challenge stay version 4 on the wire.** A v4 station answers a wrong-version opener by closing the
  connection with no refusal (`opened({error, _}, ...)` in `macula_peering_conn`), so a v5 opener would give the
  client nothing to retry on. A wrong-version CONNECT, by contrast, is answered with the named refusal
  `unsupported_version`.
- **The client chooses in CONNECT:** version 5 with proof V2, or version 4 with proof V1. The station answers HELLO
  in the same version. A client that sent a v5 CONNECT refuses a v4 HELLO (`unsupported_version`, counted); it is
  never taken as a v4 connection.
- **Stations first** accept versions 4 and 5 in CONNECT (Mars's requirement 4). A v4 CONNECT gets today's
  handshake and per-frame D17; a v5 CONNECT gets the session proof and no per-frame signatures.
- **Clients** (every SDK, and a station dialling another) send a v5 CONNECT. Refused `unsupported_version` by an old
  peer, they retry once on a new connection with a v4 CONNECT, always, with no configuration flag, and count it per
  node_id (§3). Only `unsupported_version` triggers the retry; every other refusal is final. A node_id already seen on
  v5 in this run gets no retry (§3). After a fallback, the client dials that node_id with a v4 CONNECT directly for
  10 minutes, then tries v5 again (Mars): a slow roll with 0.7.1's lookup-only redials would otherwise pay a failed
  v5 handshake on every new connection. This does not widen the downgrade: it applies only to a node never seen on
  v5 in this run, which an attacker can already force onto v4 (§3). That is safe: forcing the fallback only brings back v4's per-frame signatures,
  which are secure, so a downgrade costs CPU, never authenticity (and, against an ML-DSA-breaking attacker, the
  confidentiality of that connection's control traffic, §3).
- A peer that offers neither is refused with the named reason `unsupported_version`, as now.
- **Rollback floor (Mercurius).** `forget_v5_peer/1` reaches only peers we operate. A third-party SDK client that saw
  a station on v5 refuses it after a rollback below v5 until that client restarts, and no operator can reach it.
  So once a station has run v5, its rollback target must be a release that still speaks v5: the first v5 release a
  station runs is its rollback floor. Rolling below it cuts that station off from every client that saw it on v5,
  until each client restarts, and from peers it does not dial itself (by design, §3; not measured), so it needs a
  stated decision, never a routine pin. The release notes of the first v5 macula and macula-station releases say so.
- **A later release drops v4**, once the old-path counter (§6) reads zero across the fleet AND `macula_dist_tunnel`
  runs v5 (Mercurius): the tunnel uses `macula_handshake` but has no D17 path, so it never shows in the counter, and
  dropping v4 before it moves would break every dist tunnel. It moves by computing `E` with OTP
  `ssl:export_key_materials/4`.
- **Why stations accept both:** the v3 bump refused mixed peers in both directions, so published clients were locked
  out of the fleet until they upgraded (a partition). Accepting v4 and v5 on stations first means no peer is refused
  during the roll.
- **In scope:** macula (Erlang), then every SDK: macula-go (and through libmacula TypeScript, Python and .NET), and
  macula-rust. Every stack has the exporter: quinn 0.11 `export_keying_material` (Rust, and the Erlang side through a
  new `macula_quic` NIF function, since no NIF exposes it today) and quic-go's
  `ConnectionState().TLS.ExportKeyingMaterial` (Go). D18 was held back because aioquic and .NET QUIC had none; both
  bindings now run on libmacula.
- **macula_dist_tunnel is parked, and would be the weakest path under this threat model.** Nothing in macula calls
  it today (Fable, via the register; checked: no caller in `src/`), and direct distribution needs
  `MACULA_DIST_UNIDENTIFIED_PEER=accept`. It uses `macula_handshake` over OTP `ssl` with no D17 path, so if it were
  carried, an ML-DSA-breaking attacker would face neither per-frame signatures nor a session proof on it. This change
  does not make that worse: the tunnel stays on v4 (its station session has no exporter, so it answers v5 as an old
  station does) and moves to v5 before v4 is dropped (above).

## 5. Per-frame table (Mars's requirement 3)

"Hop-only" frames lose their pq_hybrid neighbour signature on v5 and are authenticated by the session. None of them
carries an end-to-end signature today, so none loses one. Frames that carry their own signature keep it, unchanged.

| Frame | pq_hybrid v4 (today) | pq_hybrid v5 | Own end-to-end signature, unchanged |
|---|---|---|---|
| swim_ping, swim_ack, swim_suspect, swim_confirm | neighbour | session | none |
| ping, pong (the DHT's; answered by the DHT) | neighbour | session | none |
| liveness_ping, liveness_pong (new, v5 only; answered by the conn) | (CALL probe, signed) | session | none |
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
  (`session_proof_invalid`, `session_proof_missing`, `session_proof_rate`, `exporter_unavailable`),
  `v5_downgrade_refused` (an `unsupported_version` from a node_id already seen on v5; also logged with the node_id,
  bounded, §3), `v4_hello_to_v5_connect`,
  v4 fallbacks after `unsupported_version` per node_id with the bounded warning (§3), and control frames received on
  v4 connections (the old-path counter).
- At startup, macula refuses to run v5, in either profile, unless the QUIC provider's key exchange groups are exactly
  macula-pqc's hybrid ML-KEM groups (read from the NIF), so a classical-only build can never carry v5, and unless
  0-RTT is neither offered nor accepted and resumption is off (§3). This proves the configured posture; since the
  configuration offers only hybrid groups, a handshake cannot negotiate anything else, but the check does not read
  the group a given connection negotiated.

## 7. Cost (Mars's requirement 6)

- **Per control frame:** the neighbour sign (about 6.3 ms on the fleet class) and verify (0.16 to 0.18 ms) are gone.
  What remains is QUIC's AEAD, which already covers every byte today. Target: at most 0.05 ms of protect plus verify
  per frame on the fleet class, measured by Pluto's harness before and after.
- **Per connection:** the station signs one composite `session_proof` (one composite sign, within Mars's budget) and
  verifies the client's proof as now. The client verifies one composite signature more. Two exporter computations.
- **The liveness probe:** today interval x connections x 2 composite signs (the CALL and the relay error), 12 to 20 ms
  of signing per connection per interval, the largest steady-state signing on a station with about 40 connections
  once hop frames stop signing. With v5's `liveness_ping`/`liveness_pong` (§3): none.
- **During the roll, a failed v5 attempt:** a v5 client dialling a v4-only station pays one QUIC+TLS handshake, one
  composite CONNECT sign (about 9 ms), and the refusal before it redials on v4, then the v4 handshake in full. That is
  roughly double the handshake cost to each old station, at most once per client per 10 minutes (§4), until that
  station upgrades.
- **Measured before release:** per-frame cost, per-connection handshake time, and Pluto's N=40 boot CPU.
- **Connection churn (Mars).** macula-station 0.7.1 closes lookup-only DHT connections after about 30 s idle and dials
  them again later, so handshakes per station go up just as v5 moves the cost from frames to handshakes. The
  measurement records handshakes per station per minute at N=40 on 0.7.1, times the composite-sign cost, so the
  station's lookup-only window is set with that number in hand.

## 8. Tests, red first

- A v5 connection carries control frames with no neighbour signature both ways; a neighbour signature on v5 is
  `malformed_frame`.
- A `session_proof` over another session's exporter, another node pair, or signed by another key: refused, and the
  connection closes. A v5 HELLO with no `session_proof` is refused.
- With the RSA half of the station's proof replaced by a valid signature from another RSA key (the ML-DSA half left
  valid), the client refuses: the "ML-DSA broken" case.
- A v4 client against a v5 station: v4 handshake, per-frame D17, counted. A v5 client against a v4-only station:
  refused, retries v4 once, counted.
- The Erlang, Go and Rust exporters agree byte for byte on a shared vector (label, context `initiator || acceptor`,
  session), and the signed messages of both proofs agree byte for byte on shared vectors (field order, fixed widths,
  8-byte big-endian capabilities). A cross-stack interop run connects each SDK to an Erlang station on v5, both
  profiles.
- Opener and challenge carry version 4 in a v5 handshake. A v5 CONNECT answered with a v4 HELLO is refused and
  counted, never taken as a v4 connection.
- A second fallback to the same node_id logs one warning naming it; further fallbacks within a minute log none and
  are counted.
- After a fallback, the next connection to that node_id within 10 minutes sends a v4 CONNECT directly; after 10
  minutes it tries v5 again. A node_id seen on v5 is never dialled on v4.
- On v5 the liveness probe is `liveness_ping`, answered by the peer's conn process with `liveness_pong`, neither
  signed nor passed to the DHT pid; a peer whose conn process is gone misses it and the connection closes
  `peer_liveness_lost` after the configured misses. A peer with a wedged DHT passes the liveness probe and fails a
  DHT `ping`. The DHT's `ping`/`pong` reach the DHT pid on v4 and v5. A station-level test, with Mars: one pong per
  ping on each version, from the DHT, never two, never zero.
- A peer that saw a node on v5, then gets `unsupported_version` from it (a rollback), refuses, logs the node_id once
  (bounded) and counts; after `macula_peering:forget_v5_peer/1` for that node_id it falls back to v4 again.
- A pq_hybrid v5 node refuses to start with a key exchange group list other than macula-pqc's, and CI asserts that
  the release image starts, so a station that would refuse on the fleet is caught before it ships, not as an outage.
- The station signs `session_proof` only after the CONNECT proof verifies: an invalid proof gets a refusal and no
  signature. Session proofs past the per-node or total rate are refused `session_proof_rate` and counted.
- The v4 retry happens once, only after `unsupported_version`; any other refusal is not retried. After a completed v5
  handshake with a node_id, an `unsupported_version` from it is refused, not retried, and counted.
- A v5 node configured to offer or accept 0-RTT, or with resumption on, refuses to start, client side and server
  side. A second connection between the same two nodes is a full handshake (no ticket issued or used).

## 9. Decided in review (Mars, 2026-09-29)

1. **pq_pure moves to v5 too:** one handshake layout and one code path, and a session-bound proof is strictly better
   than a leaf-bound one. One ML-DSA-87 sign per connection on the station (about 1.1 ms).
2. **The station's proof key is its identity key**, domain-separated by `MACULA-PQ-SESSION-PROOF-V1` (§3). No new key.
3. **The v4 retry is always on,** once, only after `unsupported_version`, with no configuration flag: a downgrade
   costs CPU, never authenticity (and, against an ML-DSA-breaking attacker, the confidentiality of that connection's
   control traffic, §3), and a flag would drift across boxes.
4. **No explicit per-direction seq is kept:** the ordered control stream, AEAD and packet numbers cover D17's
   connection and seq checks.
