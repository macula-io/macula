# Plan: post-quantum security for Macula, part 2: stages and work packages

**Root document:** [PLAN_POST_QUANTUM_SECURITY.md](PLAN_POST_QUANTUM_SECURITY.md). The legend, owners, key model
and decisions are there. [PLAN_POST_QUANTUM_SECURITY_PART1.md](PLAN_POST_QUANTUM_SECURITY_PART1.md) has the
verified facts and the Stage 0 checks.

**Last Updated:** 2026-09-10

Each work package gives its owner, what it waits on, the files, the change, the test that must go red before the
change, the done criterion and the effort. The US profile goes first; the EU parts follow right after (D15).

---

## Stage 1: `macula` on branch `post-quantum`, and `macula-station`

### WP 1.1 Profile model in `macula`

- [ ] Profile definition and selection.
- **Owner:** Mercury.
- **Waiting on:** nothing.
- **Files:**
  - `src/crypto_profile/macula_crypto_profile.erl` (new)
  - `src/macula_app.erl` (start-up validation)
  - `config/test.sys.config` (new) and `rebar.config`: the profile for test runs
- **Change:**
  - two profiles (`us_national_security`, `eu`), mapping to the key exchange group, TLS signature scheme, cipher
    suite, and the identity, CONNECT proof and status statement algorithms in the crypto profiles table;
  - selection per D1; a station instance runs exactly one profile (D2);
  - start-up refuses a missing or unknown profile; there is no default.
- **Red first:** `test/macula_crypto_profile_tests.erl` asserts:
  - start-up fails without a profile, and a station instance configured with two profiles fails;
  - neither profile contains a classical-only algorithm, X25519MLKEM768 or EdDSA;
  - every post-quantum algorithm is at strength level 5;
  - the EU identity signature pairs ML-DSA-87 with a classical algorithm from BSI Table 5.3.

  Fails today: the module does not exist.
- **Done:** tests green.
- **Effort:** 1 to 2 days.

### WP 1.2 Rust transport in the `macula` NIF

- [ ] The NIF negotiates only profile algorithms and gives Erlang what the connection handshake needs.
- **Owner:** Neptune.
- **Waiting on:** WP 1.1.
- **Files:**
  - `native/macula_quic/Cargo.toml`
  - `native/macula_quic/src/config.rs`
  - `native/macula_quic/src/cert.rs`
  - `native/macula_quic/src/connection.rs`
  - `native/macula_quic/src/endpoint.rs`
  - `native/macula_quic/src/lib.rs` (exports: the peer leaf certificate DER, the station's presented leaf, the
    negotiated group and signature scheme)
  - `src/peering/macula_quic.erl`
  - `.github/workflows/build-nif.yml` (build toolchain per V4)
- **Change:**
  - switch the features from `ring` to aws-lc-rs per V4 (rustls 0.23.44 or later without `tls12`, rcgen and
    rustls-webpki), limited to the profile's group: ML-KEM-1024, or the custom SecP384r1MLKEM1024 group per D3;
  - negotiated suite AES-256-GCM only, with `with_initial` for Initial packets;
  - station certificates from `rcgen`: self-signed ML-DSA-87 on the TLS key, loaded as a PKCS#8 key through
    `with_single_cert` with no custom signing key (V4); the listener reloads its certificate when the TLS key
    rotates (D22);
  - one client verification mode (key model): the TLS 1.3 handshake signature is verified against the presented
    leaf with the provider's algorithms, any other key type is refused, a TLS 1.2 signature check returns an
    error, and the supported schemes are the profile's only;
  - the post-quantum build contains no WebPKI, key-pin or development verification mode, and
    `macula_quic:connect/4` takes no `verify` option;
  - no TLS 1.3 session tickets (`send_tls13_tickets = 0`), `NoServerSessionStorage` on stations,
    `Resumption::disabled()` on clients, early data off;
  - exports: the peer leaf certificate DER exactly as received, for client connections; the leaf the station
    presented, for accepted connections; the negotiated group and signature scheme;
  - the negotiated group comes from a per-process group recorder, which names a connection's group because each
    profile offers one group; the scheme and the leaf's SHA-384 come from a per-dial verifier recorder (V4);
  - every failed accept-side handshake is reported to the owning process with the remote address and a
    classified reason;
  - the Ed25519 OID `1.3.101.112` and the 32-byte checks go.
- **Red first:** `test/macula_quic_pq_handshake_tests.erl` (new), on loopback:
  - the negotiated group, scheme and suite equal the profile's, as reported by the NIF and confirmed by a second,
    independent view (V4, V10);
  - a client offering only X25519 is refused;
  - against a station whose handshake signature does not verify, connect returns an error and no connection
    handle;
  - a client offering a resumption PSK gets a full handshake with a certificate;
  - the exported leaf DER hashes to the same SHA-384 on both sides.

  Fails today: the NIF negotiates X25519.
- **Done:** green in both profiles; `ring` absent from `native/macula_quic/Cargo.toml`.
- **Effort:** 6 to 8 days (Neptune, after V4). V4 proved the custom group and the configuration and removed the
  custom signer; the NIF exports, the Erlang side, accept-side reporting, certificate reload, the removed modes and
  CI remain.

### WP 1.3 Identity core, bindings and handshake frames in `macula`

- [ ] Every signature in `macula` uses the profile's algorithm; bindings, status statements and the handshake
  frames exist.
- **Owner:** Mercury.
- **Waiting on:** nothing; for the EU profile V8.
- **Files:**
  - `src/identity/macula_identity.erl`
  - `src/identity/macula_crypto_nif.erl` and `native/macula_crypto_nif` (grinding with post-quantum keys)
  - `src/record/macula_record.erl`
  - `src/macula_content_transfer.erl` and `src/content/macula_manifest.erl` (D24)
  - `src/peering/macula_frame.erl`
  - `src/client/macula_client_replay.erl`
  - `src/client/macula_client_dedup.erl`
  - `src/overlay/hecate_pubsub_server.erl`
  - `src/overlay/hecate_pubsub_registry.erl`
  - `src/overlay/hecate_pubsub.erl`
  - `src/overlay/hecate_plumtree.erl`
  - `src/overlay/macula_hyparview_endorsement.erl`
  - `src/overlay/macula_hyparview_view.erl`
  - `src/overlay/macula_hyparview_proto.erl`
  - `src/pubsub/macula_pubsub.erl`
  - `src/macula.erl`
  - `src/macula_cert_system/macula_cert.erl`
  - `src/macula_foundation.erl`
  - `src/auth/macula_ucan_nif.erl` and `src/identity/macula_did_nif.erl` (signing moves into `macula_identity`)
  - `rebar.config` (OTP floor)
- **Change:**
  - identity keys per profile through OTP `crypto`: ML-DSA-87 in the US profile, the hybrid pair in the EU
    profile (D4), signing as the composite `id-MLDSA87-RSA4096-PSS-SHA512` (D7);
  - CONNECT keys and TLS keys, their bindings and their status (D22); key storage per D6, with a round trip on
    load and rotation every 5 days;
  - node_id per D5, through one identity function that every comparison uses; the puzzle works on node_id;
  - record `key` and `signature`, and frame signature fields, become algorithm-tagged and variable-length; records
    carry the signer's full public key or keys (D13);
  - every DHT storage key is SHA-256: a record stored under its signer uses the signer's node_id, and every other
    key is SHA-256 over a distinct type label and the record's fields;
  - the frame codec for the opener, the challenge, CONNECT with its proof, and HELLO, with every label distinct;
    the layout is specified here, with Mars and Neptune, before anyone writes handshake code;
  - one signed station record that carries the hostname and the dial endpoint under one signature, for directory
    rows (WP 3.3);
  - UCAN and DID signing and verification in `macula_identity` (D7);
  - every event carries a publisher signature; neighbour signatures per D17;
  - content ids named in signed records use SHA-384, and verifiers reject any other hash tag there; block and chunk
    checks on fetch (`macula_content_transfer:verify_block_hash/2`) and the manifest's hashes follow the content
    id's tag, and content id guards take the 50-byte SHA-384 form (D24);
  - the advertisement bundle with provider authorization, the caller-signed target in CALL and STREAM_OPEN, the
    request hash in provider replies, and the stream signer with its sequence numbers (D25);
  - every 32-byte and 64-byte guard is replaced by profile sizes;
  - remove the unused signing functions for SWIM membership updates (`sign_swim_update/2`,
    `verify_swim_update/1`, `verify_update_result/2`, `canonical_swim_update/1`, `?SWIM_UPDATE_DOMAIN`) and
    their tests, on this branch only; SWIM itself stays (decided by Raf, 2026-09-10). Done in `8cd60ee`.
- **Red first:** extend these tests, all of which fail today:
  - `test/macula_identity_tests.erl`: sign and verify per profile; an EU hybrid signature with one invalid half is
    rejected; an Ed25519 key is rejected; a key pair that fails the round trip on load is refused;
  - `test/macula_record_tests.erl`: the carried key must derive to the claimed node_id;
  - `test/macula_frame_tests.erl`: the handshake frames round-trip, and labels cannot be confused;
  - `test/macula_content_block_hash_tests.erl`: a SHA-384 block verifies on fetch, and a block whose content id
    names another hash is refused;
  - binding tests: an expired binding, a binding for another use, and a binding for another node_id are refused;
  - `test/macula_crypto_nif_tests.erl`, `test/macula_record_cert_chain_tests.erl`.

  Add `test/macula_no_classical_signing_tests.erl`, which fails while `eddsa` or `ed25519` appears in `src/`.
- **Done:** all green; the no-classical test guards against regressions.
- **Effort:** 12 to 16 days, plus handshake frames, bindings and status ⚠.

### WP 1.4 UCAN and DID encoding

- [ ] UCANs and DIDs carry the profile's algorithm.
- **Owner:** Neptune.
- **Waiting on:** WP 1.3.
- **Files:**
  - `native/macula_ucan_nif/Cargo.toml`
  - `native/macula_ucan_nif/src/lib.rs`
  - `native/macula_did_nif/Cargo.toml`
  - `native/macula_did_nif/src/lib.rs`
- **Change:**
  - the NIFs build the signing input and parse tokens and documents; `macula_identity` signs and verifies, so
    private keys never enter Rust;
  - `alg` and key encoding per D7: `ML-DSA-87` and `AKP` from RFC 9964 with the `mldsa-87-pub` multicodec in the US
    profile; the composite with Macula's own `alg` `ML-DSA-87-PS384` and key type in the EU profile;
  - `aud` names the audience by node_id; a proof's `aud` is matched against the node_id derived from the outer
    token's `iss` key, and the string form of that audience is set here without a `did:macula:` prefix (D7);
  - UCAN parent ids are SHA-384, and verifiers reject any other hash (D24);
  - `ed25519-dalek` removed.
- **Red first:** `test/macula_ucan_nif_tests.erl` and `test/macula_did_nif_tests.erl`: a post-quantum token and
  DID round-trip; an EdDSA token is rejected; a token whose `aud` is not the receiver's node_id, or whose proof's
  `aud` is not the node_id of the outer `iss` key, is refused. Fail today.
- **Done:** green.
- **Effort:** 2 to 3 days.

### WP 1.5 Connection handshake and dials in `macula`

- [ ] Every connection runs the connection handshake, and every dial carries an expected identity.
- **Owners:**
  - Mars: `src/peering/macula_peering_conn.erl` and `src/client/macula_station_link.erl`;
  - Neptune: the trust options and `station_seed/1` in `src/client/macula_client.erl`, `src/macula_direct_dial.erl`,
    `macula_download.erl`, `src/macula_feeder.erl`, `src/peering/macula_tls.erl` and the distribution dials.
- **Waiting on:** WP 1.2, WP 1.3.
- **Change:**
  - the handshake order and checks of the key model: opener, challenge, the client's checks, CONNECT, the
    station's checks, HELLO;
  - a frame table per role; any other frame closes the handshake with a distinct reason;
  - status statements in the challenge and in CONNECT, and a status frame on open connections, with one timer per
    connection at the statement's expiry plus 5 minutes (D22);
  - only the stream the client opened is the control stream;
  - `expected_node_id` is mandatory in every dial target, and a dial without one is refused when it starts;
  - one identity check per connection, through the identity function;
  - seeds carry node_id and profile; a directory row is used only when its signed record verifies and derives to
    the row's node_id;
  - a seed's own expected identity is never replaced by a pool-level value;
  - the caller resolves verified advertisements, signs the target, and accepts a reply only from the target and
    for its request; provider stream frames are checked against the stream's key and sequence (D25);
  - distribution dials use the same verification mode.
- **Red first:** `test/macula_peering_handshake_tests.erl` and the dial tests:
  - a proof carrying another station's leaf certificate hash is refused;
  - a dial without an expected identity refuses to start;
  - a binding that is expired or signed for another use is refused;
  - a reused nonce is refused;
  - a CONNECT sent before the challenge closes the handshake;
  - a stream opened by the station during the handshake is closed, not adopted;
  - an open connection whose peer sends no fresh status statement before expiry closes with its own reason;
  - a reply from a node other than the request's target, or for another request, is refused (D25);
  - a client refuses to send CONNECT when the challenge's binding does not match the verified leaf or the
    expected node_id.
- **Done:** green in both profiles.
- **Effort:** 2 to 3 days for the handshake checks, plus the dial options ⚠.

### WP 1.6 `macula-station`

- [ ] The station runs the post-quantum identity, handshake and profile model.
- **Owner:** Mars.
- **Waiting on:** WP 1.3, WP 1.5; D21 before any merge to `main`.
- **Files:**
  - the 41 source files in `apps/` that type node_id as a public key, up to 147 sites, across `macula_station`,
    `macula_dht`, `macula_routing`, `macula_swim`, `macula_bootstrap`, `macula_content` and `macula_handler`
  - `apps/macula_station/src/macula_station_config.erl`
  - `apps/macula_station/src/macula_station_app.erl`
  - `apps/macula_station/src/macula_station_listener.erl`
  - `apps/macula_station/src/macula_station_outbound_link.erl`
  - `apps/macula_station/src/macula_station_dht_dialer.erl`
  - `apps/macula_station/src/macula_station_peering_redundancy.erl`
  - `apps/macula_station/src/macula_station_peer_observer.erl`
  - `apps/macula_station/src/macula_station_route_pubsub_frames.erl`
  - `apps/macula_content/src/macula_content_hasher.erl`, `macula_content_store.erl`, `macula_content_manifest.erl`
    and `macula_content_dht.erl` (D24)
  - `Dockerfile` (builder `erlang:28-slim`, runner `debian:trixie-slim`, D8)
  - `rebar.config` (`macula` by git ref, D20)
- **Change:**
  - node_id and public key become separate types across those files;
  - frame checks take the signer's key from the connection or from the object (D13);
  - one profile per station instance (D2);
  - the station certificate is self-signed ML-DSA-87 on the TLS key, from the listener's self-signed certificate
    source, which configuration selects;
  - bootstrap `outbound_peers` carry a node_id, kept by `decode_outbound_peer/1`; peers learned later are dialled
    with the node_id they were learned under;
  - the DHT dialer puts `expected_node_id` in its dial target and leaves the only comparison to the handshake; it
    maps the identity-mismatch disconnect to its existing `{error, {node_id_mismatch, ...}}` reply;
  - every dial target, including redundancy candidates, carries an expected node_id;
  - neighbour signatures per D17, and publisher signatures verified at the origin station;
  - content announcements carry SHA-384 content ids; the content hasher's default in the post-quantum format is
    SHA-384, and the content store, the manifest and the content DHT key follow the content id's tag and its
    50-byte length (D24);
  - advertisement gossip forwards providers' signed advertisements unchanged and drops expired ones; routing
    follows the serving station; replies and stream frames are checked against the target, the request hash and
    the sequence; relay errors carry their own codes (D25);
  - station-side key storage per D6, with the certificate reloaded on rotation;
  - BEP44 bootstrap: its items are Ed25519 by specification. Open: Terra checks whether it runs anywhere. If it
    does, Raf decides between dropping it and a dedicated classical key with an exemption from the no-classical
    test.
- **Red first:**
  - every dial, including a redundancy dial, refuses an endpoint whose identity differs from the chosen node_id,
    and the DHT dialer returns its error reply;
  - the station handshake tests of WP 1.5 pass against a station instance.
- **Done:** green; the station builds against `macula` branch `post-quantum`.
- **Effort:** 11 to 22 days:
  - node_id and key types: 4 to 6 days;
  - frame checks: 1 to 2 days;
  - handshake binding check: 2 to 3 days;
  - station link capability checks: 1 to 2 days;
  - key storage: 1 day;
  - certificate source: 1 to 5 days;
  - BEP44: half a day if dropped, 2 to 3 days if a classical key is kept;
  - base image: half a day.

---

## Stage 2: Erlang-only test suite

### WP 2.1 Test harness in `macula-internal/macula-e2e`

- [ ] The suite fails when the fleet cannot be reached.
- **Owner:** Terra.
- **Change:**
  - an unreachable target or a refused handshake fails the suite, and never skips it;
  - failures name the reason, taken from the disconnect message;
  - the harness image runs OTP 28 or newer with OpenSSL 3.5 or newer.
- **Red first:** a run against an unreachable target fails.
- **Effort:** ⚠.

### WP 2.2 Erlang-to-Erlang wire checks and claim-gate tests

- [ ] Erlang clients against Erlang station instances, in both profiles.
- **Owner:** Terra.
- **Waiting on:** Stage 1, V10.
- **Change:**
  - the negotiated group, signature scheme and cipher suite checked two independent ways that agree (V4, V10);
  - a classical-only client and an X25519MLKEM768 client are refused;
  - the claim-gate tests of Stage 7 run end to end;
  - the cross-stack leaf-hash vector, Erlang side, through the real handshake and the same accessor the proof code
    uses, on both the client and the station side;
  - a smoke check with one dial from each source of an expected identity (a seed, a directory row, a direct dial),
    which records the disconnect reason and fails hard.
- **Red first:** the suite against today's releases shows X25519.
- **Done:** green, with captures stored as CI artifacts.
- **Effort:** 2 to 3 days for the Erlang share of the wire checks.

---

## Stage 3: post-quantum fleet, station directory and realm

### WP 3.1 Realm (`macula-realm`)

- [ ] The realm issues and checks only profile credentials.
- **Owner:** Mercury.
- **Waiting on:** D8, WP 1.3.
- **Files in `apps/issue_realm_certificates/lib/`:**
  - `issue_realm_certificates.ex` (the CSR-style issuer takes post-quantum keys)
  - under `issue_realm_certificates/`: `key_gen.ex`
  - `signer.ex`
  - `realm_ca.ex`
  - `org_ca.ex`
  - `builder.ex`
  - `certificate_issuance.ex`
- **Files in `apps/macula_realm/lib/macula_realm/`:**
  - `identity/device_key_ownership_proof.ex`
  - `identity/realm_ucan_issuer.ex`
  - `mesh/member_did.ex`
  - `mesh/membership_ucan_rpc_handlers.ex`
  - `mesh/certificate_rpc_handlers.ex`
  - `mesh/admin_rpc_handlers.ex`
  - `topology/station_links.ex`
  - `topology/directory.ex`
  - `overlay/peer_link_supervisor.ex`
  - `overlay/realm_stations.ex`
  - `overlay/peer_resolver.ex`
- **Other files:**
  - `apps/guide_realm_lifecycle/lib/guide_realm_lifecycle/admit_realm_member/`
  - `apps/project_realm_identities/lib/project_realm_identities/identity_listener.ex`
  - `Dockerfile.prod` (Debian 13 slim builder and runner, D8)
- **Change:**
  - Realm CA and Org CA sign with ML-DSA-87 in the US profile;
  - in the EU profile each credential carries the composite signature `id-MLDSA87-RSA4096-PSS-SHA512`, valid only
    if both halves verify (D4, D7); whether OTP `public_key` handles that identifier is not checked ⚠;
  - OTP 28.1.1 `public_key` signs and validates ML-DSA X.509 ✅, and OTP signs and verifies brainpool ECDSA ✅ and
    RSA-PSS ✅;
  - leaf issuance, ownership proofs and membership checks take post-quantum keys, carried in full (D13);
  - the realm carries its profile (D1);
  - a separate realm deployment named `io.macula`, in the EU profile, on the post-quantum fleet (D19), and a
    US-profile realm whose name is open (Raf);
  - revocation of a realm member's identity, with its freshness window sized against BSI's deactivation
    requirement and ANSSI's hard-fail recommendation (D22).
- **Red first:** realm tests:
  - issue a chain in each profile and validate it;
  - an EU credential with one invalid half is rejected;
  - an ownership proof with a post-quantum key verifies.

  Fail today: RSA, ECDSA and Ed25519 only.
- **Done:** green in `test.yml`.
- **Effort:** 7 to 10 days.

### WP 3.2 The post-quantum fleet

- [ ] Station instances run on the new fleet in their profile.
- **Owner:** Terra.
- **Waiting on:** Stage 2; D21 done first.
- **Change:**
  - pin the live fleet first (D21);
  - station instances on the new fleet, one per profile, US first, with hostnames distinct from the live fleet;
  - `stations.csv` gains node_id and profile columns, placed before the notes column;
  - seed lists are generated after each station instance's first boot, when its node_id exists, and before the
    client releases that compile them in; every compiled-in seed list is generated from the csv, including the one
    in `macula-e2e`;
  - station configurations, including bootstrap `outbound_peers` with node_ids, are generated from the csv. Before
    regenerating, the sync reaches every station (today it misses nl-ams, Toronto and Frankfurt), and hand-picked
    peer choices are pinned in the csv or the topology;
  - a separate realm deployment `io.macula` in the EU profile (D19), and the US-profile realm (name open);
  - `macula-station` builds against `macula` branch `post-quantum` (D20);
  - every fleet node runs chrony with NTS against at least two independent servers (D22); which servers is open.
- **Red first:** the Stage 2 smoke check against the new fleet fails before provisioning.
- **Done:** every station instance is reachable in its profile under the node_ids in its seeds.
- **Effort:** 3 to 5 days, plus configuration generation ⚠.

### WP 3.3 Station directory (`hecate-stations`)

- [ ] Directory rows are self-certifying and expire with their records.
- **Owner:** Terra.
- **Waiting on:** WP 1.3.
- **Files** in `apps/hecate_stations/src/`:
  - `station_read_model.erl`
  - `ingest_node_records.erl`
  - `list_stations.erl`
  - `hecate_stations_service.erl`
  - the `hecate_om` dependency, at 0.24.0 or newer for `read_model_ttl_sweep/0`
- **Change:**
  - rows store and return the station's raw signed record, so clients verify it themselves;
  - node_id comes from the shared identity function, never from a raw key, and the 32-byte guards go;
  - a row expires with its records' `expires_at`; each record type replaces its own fields instead of merging;
    tombstones stay as the fast path for a graceful shutdown; every write sets the expiry;
  - verification drops other than routine expiry are visible as a warning or a health counter, and health reports
    them;
  - `list_stations` stays bounded with `near` and `limit`, since one signed record is about 7.2 KB;
  - check first ⚠: that `subscribe_records` delivers the announcer's refreshes, not only the first publication.
- **Red first:**
  - a record with a short TTL is ingested, and `list_stations` stops returning it after expiry;
  - a row whose record does not verify is not served.
- **Effort:** ⚠.

---

## Stage 4: each other stack, with its suite against the fleet

Every stack runs the connection handshake, carries full keys (D13), binds replies to the target and the request
(D25), and passes its own wire checks against the new fleet (WP 4.5). Public claims are made per stack (D11).

### WP 4.1 `macula-rust`

- [ ] macula-rust negotiates only profile algorithms and runs the connection handshake.
- **Owner:** Neptune.
- **Waiting on:** WP 1.2, WP 1.3, WP 3.2.
- **Files:**
  - `Cargo.toml`
  - `src/cert.rs`
  - `src/transport.rs`
  - `src/connection.rs`
  - `src/pool.rs`
  - `src/identity.rs`
  - `src/cert_chain.rs`
  - `src/manifest.rs` and `src/content.rs` (D24)
  - `macula-rust-ffi/Cargo.toml` and `macula-rust-ffi/src/lib.rs`
  - the other files that use Ed25519 (13 in total ✅)
- **Change:**
  - the transport of WP 1.2: the aws-lc-rs features of V4, the profile's group, AES-256, one verification mode, no
    resumption, and the leaf through `peer_identity`;
  - `src/content.rs` checks every block and chunk against its content id's own hash tag, SHA-384 in the
    post-quantum format; `Mcid` takes the 50-byte form, and an unknown hash name is refused (D24);
  - one trust mode replaces `Trust::WebPki`, `Trust::Pinned` and `Trust::Insecure`; every dial carries an expected
    identity;
  - the handshake frames and identity per WP 1.3, with the EU classical half per D4 and V8.
- **Red first:** the WP 1.2 and WP 1.5 assertions, as integration tests against the new fleet.
- **Done:** green in CI; `ring` absent from `Cargo.toml`.
- **Effort:** the transport takes 3 to 4 days once WP 1.2's Rust code exists (Neptune's estimate); the identity work
  is part of the SDK identity estimate, and the D24 content work is not estimated ⚠.

### WP 4.2 `macula-go`, `macula-ts` and `macula-php`

- [ ] The Go SDK and both FFI layers run the post-quantum profile.
- **Owner:** Venus.
- **Waiting on:** V5, WP 1.3, WP 3.2; for the EU profile also V8.
- **First:** unify the two FFI layers so the identity ABI changes once. `macula-ts` builds a static library into a
  Node addon with five prebuilt binaries; `macula-php` loads a shared library through `FFI::cdef`. Every size
  mismatch across the FFI fails, and never truncates.
- **Files in `macula-go`:**
  - `go.mod`
  - `transport/transport.go`
  - `connection/connection.go`
  - `frame/hello.go`
  - `frame/publisher_sig.go`
  - `dht/record.go`
  - `pool/discovery.go`
  - `identity/identity.go` and the other 11 files that use Ed25519 ✅
  - certificate generation (location per V5)
- **Files in `macula-ts`:** `cabi` and the prebuild toolchain.
- **Files in `macula-php`:** the FFI definitions and the build toolchain.
- **Change:**
  - `go.mod` moves from `go 1.26.0` to `go 1.27`, because Go 1.26 has no ML-DSA ✅ and Go 1.27 has ML-KEM-1024,
    SecP384r1MLKEM1024 and ML-DSA-87 ✅;
  - `CurvePreferences` from the profile, no classical group ever offered, and the AES-256 suite;
  - the client verifies the station's handshake signature against the presented leaf, reads the leaf from
    `PeerCertificates[0].Raw`, and keeps no session cache;
  - the handshake frames and identity per WP 1.3; the hello, publisher signature and record checks take carried
    keys (D13);
  - the EU classical half per D4, from a constant-time implementation that cross-compiles into all five prebuilt
    binaries without extra C dependencies (V8).
- **Red first:** `transport/pq_handshake_test.go` (new) asserts `ConnectionState().TLS.CurveID`, the certificate
  signature algorithm and the cipher suite per profile, and refusal of a classical-only peer and of an unbound TLS
  key. An FFI size-mismatch test fails instead of truncating.

  Fails today: Go 1.26, hybrid X25519MLKEM768 by default with classical fallback.
- **Done:** green in CI (`ci.yml` reads the Go version from `go.mod` ✅).
- **Effort:** 2 to 4 days for the transport, plus the FFI unification ⚠; the identity work is part of the SDK
  identity estimate.

### WP 4.3 `macula-py`

- [ ] The Python SDK runs the post-quantum profile.
- **Owner:** Pluto.
- **Waiting on:** D10 (how the aioquic change ships), WP 1.3, WP 3.2.
- **Files:**
  - `src/macula_py/connection.py`
  - `src/macula_py/identity.py` (3 files use Ed25519 ✅)
  - `pyproject.toml`
  - the aioquic change (per D10)
  - `tests/test_pq_handshake.py` (new)
- **Change:**
  - the aioquic change, client side only, in `tls.py`, as proven in V7:
    - the profile's offered groups and signature algorithms;
    - ML-KEM-1024 and SecP384r1MLKEM1024 key exchange in the client hello and its handler, with the ECDH secret
      first for the hybrid;
    - no parameters for the ML-DSA-87 CertificateVerify;
    - the raw leaf DER kept and exposed;
    - no session tickets;
  - `Session.connect` takes an expected station identity; a seed list with node_ids (new code); a verifier for
    signed records (new code);
  - identity per WP 1.3, with the EU classical half per D4;
  - the `cryptography` floor raised to a version with ML-KEM and ML-DSA.
- **Red first:** `tests/test_pq_handshake.py`, against the new fleet: success in the client's profile, and failure
  against a classical-only station and against an unbound TLS key. Fails today: aioquic has no ML-KEM or ML-DSA.
- **Done:** green in CI.
- **Effort:** 6 to 9 days, plus the seed list and record verifier ⚠; the identity work is part of the SDK identity
  estimate.

### WP 4.4 `macula-dotnet`

- [ ] Not in the first switch (D10).
- **Owner:** Uranus.
- .NET programs keep working against the live fleet until it is switched off.
- **Recorded for when msquic supports post-quantum key exchange:**
  - msquic on OpenSSL 3.5 or newer;
  - the binding check after the handshake, against `QuicConnection.RemoteCertificate`;
  - an expected node id in `Seed`, and one trust mode replacing `Trust.Pinned`, `Trust.WebPki` and
    `Trust.Insecure`;
  - the connection handshake frames;
  - Linux CI on an image with OpenSSL 3.5 or newer, and a `windows-latest` runner for the Schannel result.
- **Files:**
  - `src/Macula/Macula.csproj`
  - `src/Macula/Connection/Session.cs`
  - `src/Macula/Connection/Trust.cs`
  - `src/Macula/Identity/KeyPair.cs`
  - `src/Macula/Dht/CertChain.cs`
  - `.github/workflows/ci.yml`
  - `tests/Macula.Tests/PqHandshakeTests.cs` (new)
- **Effort:** 4 to 8 days ⚠, later.

### WP 4.5 Wire checks per stack against the fleet

- [ ] Every client stack's cells are green against the new fleet.
- **Owners:** each stack owner; Terra for the harness and captures.
- **Waiting on:** the stack's work package, V10.
- **Change:**
  - each client stack against the new fleet's station instances, in its profile;
  - the group, signature scheme and cipher suite checked two independent ways that agree;
  - the connection handshake checked, and a classical-only client and X25519MLKEM768 refused;
  - the cross-stack leaf-hash vector through each stack's real handshake and accessor.
- **Done:** every cell green for its profile, with captures stored as CI artifacts. This is the evidence for any
  public claim about that stack.
- **Effort:** 2 to 3 days, together with WP 2.2.

---

## Stage 5: cutover of `macula-cli`, `macula-mcp` and `lazymesh`

### WP 5.1 Tools

- [ ] The tools run against the new fleet.
- **Owners:** Mars (`macula-cli`, `lazymesh`), Venus (`macula-mcp`).
- **Waiting on:** WP 4.2, WP 3.2; a tool that joins `io.macula` also waits on their EU parts (D19).
- **Change:**
  - new seeds with node_ids, including compiled-in or documented defaults, such as the default station of
    `macula-mcp`'s hello tool;
  - device proofs carry the full key (D13);
  - release on tag: goreleaser for `macula-cli`, npm for `macula-mcp`.
- **Red first:** each tool's connection test against the new fleet fails before its cutover.
- **Effort:** ⚠.

---

## Stage 6: cutover of the hecate services

### WP 6.1 hecate services

- [ ] hecate-om and the hecate services run post-quantum.
- **Owner:** Saturnus.
- **Waiting on:** V11, the finished Reckon post-quantum plan (D9), WP 1.3 and WP 3.2 with their EU parts, because
  the services live in `io.macula` (D19), and Stage 5 in Raf's order; technically the first four are enough.
- **Files:**
  - `hecate-services/hecate-om` `src/hecate_om_identity.erl`
  - `hecate-services/hecate-om` `src/hecate_om_ownership_proof.erl`
  - the Containerfiles and CI images of the hecate service repositories (V11)
- **Change:**
  - ownership proofs carry and verify post-quantum keys in full (D13);
  - builder and runtime images on OTP 28 with OpenSSL 3.5.0 or newer at build time; the hecate images still on
    OTP 27 move to OTP 28 here (D8);
  - data keyed by the hex node id (hecate-citizens, hecate-mail, hecate-graph) keeps its 64-character shape; the
    values change when nodes get new identities.
- **Red first:** hecate-om ownership-proof tests with a post-quantum key, and every service image passing the V2
  check. Fail today.
- **Done:** green.
- **Effort:** 3 to 5 days, plus the images: 13 already have ML-DSA and ML-KEM, and the hecate images on OTP 27
  need OTP 28 ⚠.

---

## Stage 7: quality and security gates

### WP 7.1 Gates

- **Owners:** Fable, Jupiter.
- Adversarial design gates have at most two rounds per design.
- **Claim gate before rollout.** These tests must pass, each written red first:
  - a proof carrying another station's leaf certificate hash is refused;
  - a dial without an expected identity refuses to start;
  - a binding that is expired or signed for another use is refused;
  - a reused nonce is refused;
  - a client that does not verify the station's handshake signature cannot complete a dial;
  - a CONNECT sent before the challenge closes the handshake;
  - a client offering a resumption PSK gets a full handshake with a certificate, or is refused;
  - the leaf-hash vector passes through every stack's real handshake and accessor, on the client and station side;
  - a missing, stale or future-dated status statement is refused, on a new and on an open connection;
  - a reply from anyone other than the request's target, or for another request, is refused;
  - a provider stream frame from another signer, out of sequence, or on a stream whose first provider frame was not
    seen is refused;
  - an advertisement without valid provider authorization, or expired, is never a target.
- **Public claims** follow D11, and only after the stack's wire checks are green.

---

## Release after Stage 6

1. `macula` 11.0.0 on hex. Raf publishes hex.
2. SDK releases on their publish triggers: `macula-go` tag, `macula-cli` (goreleaser on tag), `macula-ts` (npm on
   tag), `macula-mcp` (npm on tag), `macula-php`, `macula-rust` (cargo on tag), `macula-py` (PyPI on tag), and
   later `macula-dotnet` (NuGet on tag).
3. The live fleet is switched off.

---

## Files to create or change

### `macula` (Stage 1, branch `post-quantum`)

- WP 1.1:
  - `src/crypto_profile/macula_crypto_profile.erl` (new)
  - `src/macula_app.erl`
  - `config/test.sys.config` (new) and `rebar.config`
- WP 1.2:
  - `native/macula_quic/{Cargo.toml, src/config.rs, src/cert.rs}`
  - `native/macula_quic/{src/connection.rs, src/endpoint.rs, src/lib.rs}`
  - `src/peering/macula_quic.erl`
  - `.github/workflows/build-nif.yml`
- WP 1.3:
  - the identity, record, content, frame, client replay and dedup, overlay, pubsub and cert-system modules listed in
    WP 1.3
  - `native/macula_crypto_nif`
  - `rebar.config`
- WP 1.4:
  - `native/macula_ucan_nif`
  - `native/macula_did_nif`
- WP 1.5:
  - `src/peering/macula_peering_conn.erl`
  - `src/client/macula_station_link.erl`
  - `src/client/macula_client.erl`
  - `src/macula_direct_dial.erl`, `macula_download.erl`, `src/macula_feeder.erl`
  - `src/peering/macula_tls.erl`
- Tests:
  - `test/macula_crypto_profile_tests.erl` (new)
  - `test/macula_quic_pq_handshake_tests.erl` (new)
  - `test/macula_no_classical_signing_tests.erl` (new)
  - extended identity, record, content block hash, frame, handshake, crypto NIF, cert chain, UCAN and DID tests

### `macula-station` (WP 1.6)

- the files listed in WP 1.6

### `macula-internal/macula-e2e` (WP 2.1, WP 2.2, WP 4.5)

- the harness, the handshake and claim-gate suites, and the seed list generated from `stations.csv`

### `macula-realm` (WP 3.1)

- the files listed in WP 3.1

### `hecate-stations` (WP 3.3)

- the files listed in WP 3.3

### `macula-rust` (WP 4.1)

- `Cargo.toml`
- `src/cert.rs`
- `src/transport.rs`
- `src/connection.rs`
- `src/pool.rs`
- `src/identity.rs`
- `src/cert_chain.rs`
- `src/manifest.rs`
- `src/content.rs`
- `macula-rust-ffi/Cargo.toml`
- `macula-rust-ffi/src/lib.rs`

### `macula-go` (WP 4.2)

- `go.mod`
- `transport/transport.go`
- `connection/connection.go`
- `identity/identity.go`
- `transport/pq_handshake_test.go` (new)

### `macula-ts`, `macula-php` (WP 4.2)

- `cabi`, the FFI definitions, and the build and prebuild toolchains

### `macula-py` (WP 4.3)

- `src/macula_py/connection.py`
- `src/macula_py/identity.py`
- `pyproject.toml`
- `tests/test_pq_handshake.py` (new)

### `macula-dotnet` (WP 4.4, later)

- `src/Macula/Macula.csproj`
- `Connection/Session.cs`
- `Connection/Trust.cs`
- `Identity/KeyPair.cs`
- `Dht/CertChain.cs`
- `tests/Macula.Tests/PqHandshakeTests.cs` (new)

### `macula-cli`, `macula-mcp`, `lazymesh` (WP 5.1)

- seeds, compiled-in defaults and release configuration

### `hecate-services/hecate-om` and the hecate services (WP 6.1)

- `src/hecate_om_identity.erl`
- `src/hecate_om_ownership_proof.erl`
- Containerfiles and CI images (V11)
