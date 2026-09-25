# Military grade: technical baseline and gap register

**This exists so Macula mesh and realm reach "technically military grade, or better, within our means", measured against named requirements rather than a slogan.**

**Status:** Planning. Classification: BUILD (a register, no claims). **Created:** 2026-09-25. **Owner:** Pluto.
**Certification is out of scope.** Nothing here says Macula meets, complies with or is certified against any rule
below. Public wording stays under D11 in [PLAN_POST_QUANTUM_SECURITY_DECISIONS.md][d11]. The bodies and processes
for the Belgian Defence (NVO/ANS CIS approval, CCB/CyFun, BOSA, e-Procurement, NATO) are in
[PLAN_MILITARY_GRADE.md][mg]; this document does not repeat them.

**Checked against:** `macula` origin/main bf1d732e (v12.5.0), `macula-station` release v0.6.2 (what the fleet runs)
and main 2cf0e8c, `macula-realm` origin/main efefaee, the wire measurement
`~/.claude/sessions/PQKX_MEASUREMENT_2026-09-24.md`, and the post review
`~/.claude/sessions/REVIEW_2026-09-25_architecting_survivability.md`. Public text derived from this register:
`macula-comm-docs/posts/architecting-survivability.v2.md`.

---

## 1. The yardstick

| Source | What it gives us | Where |
|---|---|---|
| NSA CNSA 2.0 advisory and FAQ | algorithm list (ML-KEM-1024, ML-DSA-87, AES-256, SHA-384/512) | summarised with quotes in [PLAN_POST_QUANTUM_SECURITY.md][pq] "Requirements that apply to both profiles" |
| BSI TR-02102-1 and TR-02102-2 | hybrid use, TLS groups (TR-02102-2 section 3.4.2), hybrid signature construction (TR-02102-1 section 5.3.4) | [PLAN_POST_QUANTUM_SECURITY.md][pq], D11 |
| EU Cyber Resilience Act, Regulation (EU) 2024/2847, Annex I | essential requirements for a product with digital elements: Part I (2)(d) access control, (e) confidentiality in transit, (f) integrity, (h) availability and DoS resilience, (j) attack surface, (l) security logging; Part II (1) SBOM, (3) security testing, (7) secure update distribution | EUR-Lex, text checked 2026-09-25 |
| NIS2, Directive (EU) 2022/2555, Article 21(2) | measures an essential entity must take: (d) supply chain security, (e) secure development and vulnerability handling, (h) cryptography policy | EUR-Lex, text checked 2026-09-25 |
| Military CIS properties: traffic-flow confidentiality, node capture, key management | expected by a national CIS accreditation (NVO/ANS in Belgium); the requirement texts are not public | [PLAN_MILITARY_GRADE.md][mg] section 2.1; the concrete tender sets the bar (section 1) |

A property whose requirement text is not public is marked **(source: accreditation, not public)**. Its row states
what a CIS accreditation typically examines, and the tender or the accreditor sets the exact bar.

## 2. Register, ranked by value per effort

Size: **S** at most a week of one engineer, **M** a few weeks, **L** a quarter or more. Cost is engineering time
unless stated; "out of our means" marks what needs money or an institution we do not have.

| # | Property | Requirement | Current state (citation) | Gap | Size / cost |
|---|---|---|---|---|---|
| 1 | Availability under attack, authorization at the edge | CRA I(2)(h), (d) | Station **main** has the ADVERTISE chain check, 16 providers per procedure and 1024 procedures per advertiser (macula-station 2cf0e8c, #6, #8, #10), and the D28 verification counter (`apps/macula_dht/src/macula_dht_budget.erl`, "Count one refusal on the connection's verification budget"). The running v0.6.2 has only the per-connection store budget (16 MiB, 1 MiB/s, same module) and says the charge "lands with the D28 verification budget" (`macula_station_peer_observer.erl`, `on_stream_verified/6`) | Built, not released | S / 1 to 2 days: release and roll the station |
| 2 | Supply chain: SBOM, signed images, build provenance | CRA II(1), II(7); NIS2 21(2)(d), (e) | No SBOM, signature or provenance step in any CI workflow of macula, macula-station or macula-realm (grep for cosign, syft, sbom, attest: none). Images are pinned by digest in macula-fleet | Generate an SBOM per image, sign images keylessly with the GitHub OIDC identity, attach build provenance, and have the fleet reconciler refuse an unsigned digest | S / 2 to 4 days, no licence cost |
| 3 | Cipher suites: AES-256 only | CNSA 2.0 (AES-256); [PLAN_POST_QUANTUM_SECURITY.md][pq] V13 | Every endpoint offers TLS_AES_128_GCM_SHA256 and TLS_CHACHA20_POLY1305_SHA256 next to TLS_AES_256_GCM_SHA384 (wire, 2026-09-25, PQKX_MEASUREMENT round 4 (b)). The list comes from `..rustls::crypto::aws_lc_rs::default_provider()` in macula-pqc v0.2.0 `provider()` (`macula-pqc/src/lib.rs:196-206`), shared by `client_builder()` and `server_builder()`. Stations select AES-256 today by order and preference, not by restriction | Set `cipher_suites` to TLS13_AES_256_GCM_SHA384 only in `provider()`, release macula-pqc, bump macula; test the list and a negative control. Filed as macula issue #39. QUIC Initial protection stays AES-128-GCM by RFC 9001 (V13) | S / 1 day |
| 4 | Revocation of a provider | CRA I(2)(d); key management (source: accreditation, not public) | The realm tombstones a withdrawn delegation at once (`ProviderAuthorization.revoke/4`, `provider_authorization.ex:156`), but callers trust the delegation embedded in an advertisement and read no tombstone, so a revoked provider stays callable up to **six hours** (`src/macula_direct_dial.erl` moduledoc lines 101 to 105; `REALM_AND_ORG_MAX_LIFETIME_MS`, `src/record/macula_record.erl:172`) | A stated, short revocation bound: a tombstone check, a shorter delegation lifetime, or a delegation status statement as D22 does for bindings. Filed as macula issue #38 | S to M |
| 5 | Measured post-quantum key exchange on every link | CNSA 2.0 / BSI via D11 ("nothing is claimed before its wire checks are green") | Measured on all six stations and on 7 station-to-station handshakes: SecP384r1MLKEM1024, classical-only refused (PQKX_MEASUREMENT). Client-to-station, from our own macula 12.5.1 client (2026-09-25): offers only the two hybrids with a SecP384r1MLKEM1024 share, no HelloRetryRequest, handshake completed, so SecP384r1MLKEM1024 is selected (inferred from TLS 1.3, ServerHello not decoded; round 4 (b)). Realm-to-station not yet observed: long-lived links, measured during the station 0.6.3 roll | A deliberate redial in the lab, or a client-side log of the negotiated group (rustls `negotiated_key_exchange_group`, already asserted in `native/macula_quic/src/config.rs` tests) | S / 1 to 2 days |
| 6 | A refused stored identity fails loudly | CRA I(2)(b) secure by default, (d) | `macula:connect/2` without `node_identity` loads the node's stored key (`filename:basedir(user_data, "macula")/identity.key`, `macula_node_keys:node_identity_path/0`). A key in another profile comes back as a bare `{error, {wrong_profile, pq_pure}}` naming no file, unlike a supplied key's `{node_identity, _}` (`macula_client:node_identity/2`). Seen on host00 2026-09-25 | Wrap and name the file and both profiles, and log it; decide whether a host running both profiles gets one stored key per profile. Filed as macula issue #40 | S |
| 7 | Closed membership: only admitted nodes connect | CRA I(2)(d); CIS closed-network expectation (source: accreditation, not public) | Joining is open: a station admits any node that proves its key and meets the node_id puzzle. D31 `invite_only` exists as a handshake field only: `member_endorsement` is "handed over, never checked here" (`src/peering/macula_handshake.erl:286`), and station v0.6.2 has no `invite_only` code | Build D31 in the station CONNECT check, per realm, off by default for the public realm and on for a closed one | M |
| 8 | Node capture: identity keys at rest | node capture (source: accreditation, not public); CRA I(2)(e) for stored data | Identity keys are plain files, mode 0600, not encrypted (`macula_node_keys`, `restrict/2` at `src/identity/macula_node_keys.erl:812`) | Step 1: keys encrypted at rest with an operator-held secret (M). Step 2: keys in a TPM or HSM through PKCS#11, never exported (L). D5/D24 still hold: no trust is granted to a key because of the hardware that holds it; this is storage only | M, then L |
| 9 | Security audit log | CRA I(2)(l); NIS2 21(2)(b) incident handling | The realm records admissions as events with `admitted_by` (`guide_realm_lifecycle/admit_realm_member/admit_realm_member_v1.ex`). Stations log refusals to the ordinary logger, some at debug level (`macula_station_peer_observer.erl`); there is no dedicated, tamper-evident security log and no export | A security event stream per station (connections refused, charged refusals, pauses, authorization failures) with hash chaining and syslog/SIEM export | M |
| 10 | DHT eclipse resistance with real diversity data | CRA I(2)(h) | Bucket diversity by ASN, country and tier is coded (`macula_dht_diversity.erl`) but has no data: "ASN 0 means UNKNOWN ... it is the value every entry carries today ... a peer's real ASN is never read back" (v0.6.2 `macula_dht_placement.erl:181-185`) | Learn a peer's real ASN and country (for example from a signed station record or an IP-to-ASN table) and read it back | M |
| 11 | DoS resilience, tested | CRA I(2)(h), II(3) | No flood or load test in any repo (review finding) | A repeatable DoS test campaign in the lab against one station: handshake floods, bad-signature floods, STORE floods, with the budgets from #1 | M |
| 12 | Other SDKs' key exchange | CNSA 2.0 / BSI via [PLAN_POST_QUANTUM_SECURITY.md][pq] ("No X25519MLKEM768"; BSI names SecP256r1MLKEM768 and SecP384r1MLKEM1024) | The published macula-mcp (TypeScript SDK on macula-go v0.7.1) offers X25519MLKEM768 and classical groups; stations force SecP256r1MLKEM768 (PQKX_MEASUREMENT, "Identified clients"). Current macula-go offers only the two BSI groups but still settles on SecP256r1MLKEM768, because Go 1.24+ picks its own key shares | Ship the current macula-go in the TypeScript SDK (M). Leading with SecP384r1MLKEM1024 in Go needs a crypto/tls change (L); both groups are hybrids, so low priority | M (L for the Go change) |
| 13 | Content identifiers on a listed hash | CNSA 2.0 SHA-384/512; BSI/ANSSI lists (D11 identifier qualifier) | Content identifiers use BLAKE3 (`macula_blake3_nif`), which none of the guidance lists; [PLAN_POST_QUANTUM_SECURITY.md][pq] already plans SHA-384 for content ids (D24) | Implement D24's SHA-384 content ids | M (a wire change) |
| 14 | End-to-end confidentiality | CRA I(2)(e); CIS end-to-end protection (source: accreditation, not public) | Confidentiality is per QUIC link. Stations relay CALL, EVENT and STREAM frames and see them in the clear; there is no payload encryption module (review finding 2). Authenticity is end to end (the SDK verifies requests, replies and events: `macula_station_link.erl` :2029, :2038, :2106) | Encrypt payloads to the receiving node: to the provider's key for a call, to the caller for its reply, and to a group key for events, with ML-KEM encapsulation and AES-256-GCM, under the existing signatures | L |
| 15 | Compromise of an identity key | key management (source: accreditation, not public) | D11: "Compromise of an identity key itself is handled separately". Binding revocation lands within 65 minutes (D22, status statements in `macula_statement_issuer.erl`), but a stolen identity key can mint new bindings | A realm-signed identity revocation list that stations and callers check, with a stated bound | L |
| 16 | Traffic-flow confidentiality | CIS traffic-flow confidentiality (source: accreditation, not public) | None. Stations are public hosts, and a station sees who calls whom and when. Macula makes no anonymity claim (post, "The limits, plainly") | Padding frames to fixed sizes and batching (M, partial). Full protection with cover traffic and mixing is **out of our means** at our scale and would conflict with latency | M partial; full: out of our means |
| 17 | Pure ML-KEM-1024 for a US National Security Systems profile | CNSA 2.0 ("will not require NSS developers to use hybrid"; "do not use a hybrid ... on NSS mission systems") | Both profiles offer only the two hybrids; D3 adds pure ML-KEM-1024 "if someone needs CNSA 2.0 alignment" | Add the group for `pq_pure` only | M; low value for a Belgian goal |
| 18 | Formal models of the DHT and realm state machines | assurance (source: accreditation, not public) | None | TLA+ models of slot admission, provider authorization and revocation, checked with TLC | M to L, within our means for the core protocols |
| 19 | FIPS 140-3 validation of the cryptographic modules | CMVP (NIST and the Canadian Centre for Cyber Security) | ML-DSA is `macula-mldsa`, not a validated module (macula README) | Validation of our own module: **out of our means** (laboratory fees and an accredited process). Within means: evaluate using an already validated provider for the primitives it covers | Own validation: out of our means; evaluation: S |
| 20 | Common Criteria evaluation | EUCC / Common Criteria | None | **Out of our means** without a sponsor and a funded evaluation | Out of our means |
| 21 | Belgian CIS approval | NVO/ANS CIS approval ([PLAN_MILITARY_GRADE.md][mg] section 2.1) | Not started | Needs a Defence sponsor and a concrete system; not a technical task | Out of our control |

## 3. Reading the ranking

- **#1 to #5 are cheap and close gaps we can state publicly afterwards.** #1 and #2 are mostly release and CI
  work. #3 is one line in macula-pqc. #4 turns a six-hour window into a stated bound. #5 turns "not measured" into
  "measured" for the last link type, realm to station.
- **#7 to #11 are the core of "military grade, within our means":** closed membership, keys that survive node
  capture better, an audit trail, real eclipse resistance, and DoS evidence.
- **#14 end-to-end confidentiality is the largest single gap** against any CIS accreditation, and the only L item
  with high value. It should be designed as its own plan before sizing it further.
- **#19 to #21 need money or institutions.** They are listed so nobody mistakes their absence for an oversight.

## 4. Next steps

- [ ] #1: station release carrying 2cf0e8c and the D28 counter (Mercurius or Neptunus lane).
- [ ] #2: SBOM, keyless signing and provenance in the macula-station and macula-realm image workflows; the
  reconciler refuses unsigned digests (Terra's lane for the reconciler).
- [ ] #3: AES-256-only cipher suites (macula issue #39).
- [ ] #4: decide the revocation bound in macula issue #38.
- [ ] #5: realm-to-station key exchange, captured during the station 0.6.3 roll (client-to-station is done).
- [ ] #6: a loud, named refusal for a stored identity in another profile (macula issue #40).
- [ ] #14: a design plan for end-to-end payload confidentiality.

[mg]: PLAN_MILITARY_GRADE.md
[pq]: PLAN_POST_QUANTUM_SECURITY.md
[d11]: PLAN_POST_QUANTUM_SECURITY_DECISIONS.md#d11-what-public-text-may-claim
