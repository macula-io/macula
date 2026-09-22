# Plan: post-quantum security for Macula, part 2: stages and work packages

**Root document:** [PLAN_POST_QUANTUM_SECURITY.md](PLAN_POST_QUANTUM_SECURITY.md). The legend, owners, key model
and decisions are there. [PLAN_POST_QUANTUM_SECURITY_PART1.md](PLAN_POST_QUANTUM_SECURITY_PART1.md) has the
verified facts and the Stage 0 checks.

**Last Updated:** 2026-09-14

Each work package gives its owner, what it waits on, the files, the change, the test that must go red before the
change, the done criterion and the effort. The US profile goes first; the EU parts follow right after (D15).

---

## Stage 1: `macula` 11.0.0 and `macula-station`

### WP 1.1 Profile model in `macula`

- [ ] Profile definition and selection.
- **Owner:** Mercury.
- **Waiting on:** nothing.
- **Files:**
  - `src/crypto_profile/macula_crypto_profile.erl` (new)
  - `src/macula_app.erl` (start-up validation)
  - `config/test.sys.config` (new) and `rebar.config`: the profile for test runs
- **Change:**
  - two profiles (`pq_pure`, `pq_hybrid`), mapping to the key exchange group, TLS signature scheme, cipher
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
  - `.github/workflows/publish-hex.yml` (build toolchain per V4; the NIFs build from source in its jobs)
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
  - the listener keeps one server configuration per certificate generation and accepts each connection with the
    current one (quinn `Incoming::accept_with`), storing that generation's leaf on the connection handle, so a
    certificate reload never changes the leaf a live connection reports;
  - the negotiated group comes from a per-process group recorder, which names a connection's group because each
    profile offers one group; the scheme and the leaf's SHA-384 come from a per-dial verifier recorder (V4);
  - every failed accept-side handshake is reported to the owning process with the remote address and a
    classified reason;
  - receive-side flow control, once Raf decides (D28): credit per stream, where `setopt(Stream, active, N)`
    delivers at most N data messages and then one passive notice, and receive windows and the maximum of
    concurrent streams set per connection (`DESIGN_PQ_DHT_SLOTS_AND_BUDGET.md`, 3.4);
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
  - `src/identity/macula_node_keys.erl` (new: a node's keys per purpose and profile, stored per D6, signing per D4
    and D7, node_ids per D5)
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
  - `src/auth/macula_ucan.erl` (WP 1.4)
  - `rebar.config` (OTP floor)
- **Change:**
  - identity keys per profile: ML-DSA-87 through `macula-mldsa` in the US profile, the hybrid pair in the EU
    profile (D4), signing as the LAMPS composite `id-MLDSA87-RSA4096-PSS-SHA512` with its RSA-PSS half on OTP
    `crypto` (D7, amended 2026-09-22);
  - CONNECT keys and TLS keys, their bindings and their status (D22); key storage per D6, with a round trip on
    load and rotation every 5 days;
  - node_id per D5, through one identity function that every comparison uses; the puzzle works on node_id at 12
    leading zero bits, a constant (D30), and the 10.x `puzzle_difficulty` setting of the `macula` application goes;
    identity key generation regenerates only the ML-DSA-87 half of a pq_hybrid key;
  - record `key` and `signature`, and frame signature fields, become algorithm-tagged and variable-length; records
    carry the signer's full public key or keys (D13);
  - every DHT storage key is SHA-256: a record stored under its signer uses the signer's node_id, and every other
    key is SHA-256 over a distinct type label and the record's fields;
  - the frame codec for the opener, the challenge, CONNECT with its proof, HELLO and status, per
    `DESIGN_PQ_HANDSHAKE_FRAMES.md`: signed structures verified over the bytes as received, a strict decoding rule
    with `malformed_frame`, one accepted encoding per carried key, and every label distinct;
  - STREAM_OPEN carries a capability token as CALL does, so streams are authorized like calls, and CALL and
    STREAM_OPEN carry a signed deadline (WP 1.4, D7);
  - PUBLISH, SUBSCRIBE and cast carry no capability token field in the post-quantum format, unless a signed-request
    rule for them is set first (WP 1.4);
  - one signed station record that carries the hostname and the dial endpoint under one signature, for directory
    rows (WP 3.3);
  - UCAN and DID signing and verification in `macula_identity` (D7);
  - every event carries a publisher signature; neighbour signatures per D17;
  - content ids take a 50-byte form whose first byte is the hash tag, and the post-quantum format has only tag 2,
    SHA-384: blocks, chunks and manifests are made with SHA-384, a manifest names `sha384` as its only algorithm,
    and an id with any other tag is refused on fetch (`macula_content_transfer:verify_block_hash/2`), in manifests,
    for chunks and in announcements (D24);
  - `macula:put_content` and `macula:get_content` serve content from the node that shares it and fetch it from that
    node, through stations (D27);
  - the advertisement bundle with provider authorization, the caller-signed target in CALL and STREAM_OPEN, the
    request hash in provider replies, the stream signer with its sequence numbers (D25), and the caller's signature
    with its sequence numbers on its own stream frames (D17);
  - a procedure advertisement for a name without an org namespace is refused when it is made and when it is
    verified (D25);
  - every 32-byte and 64-byte guard is replaced by profile sizes;
  - peer-supplied maps are delivered in one key form and read through `macula:field/2,3` and `macula:text/1`, and
    the codec decodes a frame type's own fields through a fixed table (D26);
  - `test/vectors/decoding_rule_v1.json`: the shared vectors of the decoding rule, one entry per case with its name,
    its CBOR in hex and whether it is accepted, which every stack's CI runs pinned by commit (Stage 4);
  - the node side of `DESIGN_PQ_DHT_SLOTS_AND_BUDGET.md` (D28): the 7-day domain record lifetime and the trust list's
    pairs of realm id and realm key id in `macula_record`; STORE_ACK `signer` and `version`, FIND_VALUE `after`,
    VALUE `next` and the HyParView bounds in the frame codec; `verify_authorization/3` over the trust list's pairs;
    the Plumtree IHAVE allowance and GRAFT window; the HyParView placement allowance and solicited SHUFFLE_REPLY;
    record bytes paced in the DHT put path; decoding vectors for the new fields; and `scripts/bench-pq-verify.sh`
    for the verification cost per profile;
  - remove the unused signing functions for SWIM membership updates (`sign_swim_update/2`,
    `verify_swim_update/1`, `verify_update_result/2`, `canonical_swim_update/1`, `?SWIM_UPDATE_DOMAIN`) and
    their tests, on this branch only; SWIM itself stays (decided by Raf, 2026-09-10). Done in `8cd60ee`.
- **Red first:** extend these tests:
  - `test/macula_identity_tests.erl`: sign and verify per profile; an EU hybrid signature with one invalid half is
    rejected; an Ed25519 key is rejected; a key pair that fails the round trip on load is refused;
  - `test/macula_node_keys_tests.erl` (new): keys for each purpose and profile survive a save and load; a stored
    public key that differs from the one derived from its private key, a key saved for another purpose or
    profile, and an Ed25519 key file are refused;
  - `test/macula_node_keys_signing_tests.erl` (new): a US signature is ML-DSA-87 over the message; an EU signature is
    the LAMPS composite, whose halves both verify over M', ML-DSA-87 with the label as its context; a signature with
    one invalid half, a half on its own, a signature under the other profile and a non-canonical key encoding are
    refused;
  - `test/fixtures/lamps_mldsa87_rsa4096_pss_sha512/`: the draft's own vector, whose signature verifies and is
    refused when altered or made with a context, and whose key loads as a node key and signs; and
    `test/fixtures/lamps_composite_zero_dropped/`, a composite with its RSA half one byte short, refused;
  - `test/macula_node_keys_node_id_tests.erl` (new): the three D5 reference vectors, node_ids derived from the
    carried identity key, and no node_id for CONNECT or TLS keys;
  - `test/macula_record_tests.erl`: the carried key must derive to the claimed node_id;
  - `test/macula_frame_tests.erl`: the handshake frames round-trip, and labels cannot be confused; a frame decoded
    on a fresh node, with the absence of its field atoms asserted first, and on a warm node delivers identical
    payload maps (D26);
  - `test/macula_content_block_hash_tests.erl`: a SHA-384 block verifies on fetch, and a content id with any tag
    but 2 is refused;
  - binding tests: an expired binding, a binding for another use, and a binding for another node_id are refused;
  - `test/macula_crypto_nif_tests.erl`, `test/macula_record_cert_chain_tests.erl`;
  - the decoding rule vectors run against `macula_record_cbor:decode_strict/1`, one test per entry.

  Add `test/macula_no_classical_signing_tests.erl`, a ratchet over `src/` and `native/`: it fails on a classical
  signature made or checked, or an ML-DSA one through OTP `crypto` (D7, amended), that its known list does not
  hold. Each entry names the work item that removes it, a fix strikes its own entry, and while any is left only a
  pre-release `vsn` passes.
- **Done:** all green; the no-classical test guards against regressions.
- **Effort:** 12 to 16 days, plus handshake frames, bindings and status ⚠.

### WP 1.4 UCAN and DID encoding

- [ ] UCANs carry the profile's algorithm, and name their issuer by `did:key` (D7).
- **Owner:** Venus.
- **Waiting on:** WP 1.3.
- **Files:**
  - `src/auth/macula_ucan.erl`, which replaces `macula_ucan_nif` and its crate
  - `src/client/macula_station_link.erl`
- **Built (2026-09-22):** single tokens. `macula_ucan:create/4` signs with a node key; `authorize/3` verifies over the
  header and payload as received, then the issuer (by node_id for `ucan_required`, by key id for
  `realm_member_required`), `aud` as the caller's node_id, `exp` (required) and `nbf`, and the capability of a
  membership policy. The station link authorizes CALL and STREAM_OPEN through it. Still to build: delegation chains
  through `prf`, with narrowing, issuer scope and org keys through the org directory, and SHA-384 parent ids.
- **Change:**
  - `macula_did_nif` and its crate are removed (Raf, 2026-09-22): nothing called it in macula, macula-station,
    macula-realm, mcl-om or mcl-echo, and D7 retires the `did:macula:` names it built;
  - `macula_ucan` builds the signing input and parses tokens in Erlang, and signs and verifies through
    `macula_node_keys`, so ML-DSA is `macula-mldsa` in `macula_crypto_nif` and the EU composite's RSA-PSS half is
    OTP's (D7, amended 2026-09-22);
  - `alg` and key encoding per D7: `ML-DSA-87` and `AKP` from RFC 9964 with the `mldsa-87-pub` multicodec in the US
    profile; the LAMPS composite `id-MLDSA87-RSA4096-PSS-SHA512`, under the `alg` `ML-DSA-87-PS384`, with Macula's
    own key type in the EU profile, the private-use multicodec 0x300087, in `iss`'s `did:key`;
  - `aud` names the audience by node_id; a proof's `aud` is matched against the node_id derived from the outer
    token's `iss` key, and the string form of that audience is the node_id in lowercase hex, with no `did:macula:`
    prefix (D7);
  - UCAN parent ids are SHA-384, and verifiers reject any other hash (D24);
  - a provider authorizes a CALL or STREAM_OPEN per D7 check 2, before any handler runs: the frame signature
    against the caller key, the signed target against its own node_id, the token's `aud` against the node_id of
    that caller key, then the chain up to the policy's required issuer, with each token's signature checked before
    its claims, `exp` present, a capability for the procedure and realm, and SHA-384 parent ids; otherwise the
    request is refused with its own reason;
  - a request past its signed deadline plus the D22 tolerance is refused, and (caller, call id) deduplication holds
    until then; a nonce store exists only for tokens used outside a signed request;
  - the authorizing verify takes the verified caller key and the expected target; a chain-only check has its own
    name, which says it does not authorize;
  - every UCAN gate checks `aud` against the node_id of the verified caller, including an issuer-only policy
    (`ucan_required`);
  - a realm membership token names its device by node_id in `aud` (D7, V12), changed in the realm issuer (WP 3.1)
    and in every checker together;
  - streams are authorized by the same checks as calls;
  - a capability names what it grants in `with`: a realm, an org of that realm or a procedure of that realm, where
    an org is the org namespace of a procedure name as `DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md` defines it (D7);
  - along a chain every token names the same realm, each token's capability is covered by one of the token it
    proves from, and `can` is equal at every step: a realm grant covers realm, org and procedure grants, an org
    grant covers that org and its procedures, and a procedure grant only itself (D7);
  - an org key grants only org or procedure capabilities inside its own org, and only the realm key grants a realm
    capability (D7);
  - every policy refuses a procedure name without an org namespace (D25);
  - `ed25519-dalek` removed.
- **Red first:** `test/macula_ucan_tests.erl`: a post-quantum token round-trips; an EdDSA token is rejected. For a
  call and for a stream, a request is refused when its target is another node, its token's `aud` is not the node_id of
  the verified caller, a proof's `aud` is not the node_id of the next token's `iss` key, the chain does not root at
  the required issuer, a token has no `exp` or no capability for the procedure and realm, or the request is past its
  signed deadline plus the D22 tolerance; a repeated (caller, call id) within that window is refused as a duplicate. A
  request is also refused when a token grants more than the token it proves from, a chain changes realm or `can`, an
  org key grants outside its own org or grants a realm, or the procedure name has no org namespace.
- **Done:** green.
- **Effort:** 2 to 3 days, plus the token checks ⚠.

### WP 1.5 Connection handshake and dials in `macula`

- [ ] Every connection runs the connection handshake, and every dial carries an expected identity.
- **Owners:**
  - Mercury: `src/peering/macula_peering_conn.erl` and `src/client/macula_station_link.erl`;
  - Neptune: the trust options and `station_seed/1` in `src/client/macula_client.erl`, `src/macula_direct_dial.erl`,
    `macula_download.erl`, `src/macula_feeder.erl`, `src/peering/macula_tls.erl`, the distribution dials, and the
    distribution tunnels on both carriers (D29): the relay path in `macula_dist_relay_client.erl` and
    `macula_dist_relay_protocol.erl`, and the pool path in `macula_dist_pool.erl` and `macula_dist_bridge.erl`, all
    in `src/macula_dist_system/`.
- **Waiting on:** WP 1.2, WP 1.3.
- **Change:**
  - the handshake order and checks of the key model: opener, challenge, the client's checks, CONNECT, the
    station's checks, HELLO;
  - a frame table per role; any other frame closes the handshake, in the SDK and every port alike; close reasons stay
    local, and a refused client sees only HELLO with a coarse refusal code (`DESIGN_PQ_HANDSHAKE_FRAMES.md`);
  - status statements in the challenge and in CONNECT, and a status frame on open connections, with one timer per
    connection at the statement's expiry plus 5 minutes (D22);
  - only the stream the client opened is the control stream;
  - `expected_node_id` is mandatory in every dial target, and a dial without one is refused when it starts;
  - one identity check per connection, through the identity function;
  - seeds carry node_id and profile; a directory row is used only when its signed record verifies and derives to
    the row's node_id;
  - a seed's own expected identity is never replaced by a pool-level value;
  - the caller resolves verified advertisements, signs the target, and accepts a reply only from the target and
    for its request; provider stream frames are checked against the stream's key and sequence (D25), and a
    provider checks caller stream frames against the caller key and sequence (D17);
  - distribution dials use the same verification mode;
  - a distribution tunnel carried over the mesh runs the connection handshake end to end, with the accepting node in
    the station role, and its data travels only inside that TLS 1.3 session; the dialing node gives the peer's
    expected node_id (D29);
  - the carrier delivers a tunnel's data complete and in order, or the tunnel ends;
  - on the connection side (Mercury): the verification budget and the reading pause in `macula_peering_conn`, with
    the SWIM and stream idle timers for that peer extended by each pause (D28).
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
    expected node_id;
  - a distribution tunnel whose peer derives to a node_id other than the one dialled is refused;
  - replayed, reordered or altered tunnel data ends the tunnel;
  - on the pool path, a dropped or reordered carrier message ends the tunnel.
- **Done:** green in both profiles.
- **Effort:** 2 to 3 days for the handshake checks, plus the dial options ⚠, plus the distribution tunnels on both
  carriers ⚠.

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
    and `macula_content_dht.erl` (D24, D27)
  - `Dockerfile` (builder `erlang:28-slim`, runner `debian:trixie-slim`, D8)
  - `rebar.config` (`macula` `~> 11.0` from hex once Raf publishes, a local checkout before that, D20)
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
  - record fan-out, DHT handlers and content handlers read peer-supplied maps through the facade accessors (D26);
  - stations keep and serve verified records of any type, including domain types they don't know, until they
    expire, within the per-slot and per-class bounds; a test stores and fetches one (D23, D28);
  - the station's content store and the handlers that store pushed content are removed, not converted (D27);
  - wherever the station still reads content ids, as in pass-through and announcements, they have only tag 2,
    SHA-384, and an id with any other tag is refused (D24);
  - advertisement gossip forwards providers' signed advertisements unchanged and drops expired ones; routing
    follows the serving station; replies and stream frames are checked against the target, the request hash and
    the sequence; relay errors carry their own codes (D25);
  - the station neither stores nor forwards an advertisement for a procedure name without an org namespace (D25);
  - station-side key storage per D6, with the certificate reloaded on rotation;
  - the station side of `DESIGN_PQ_DHT_SLOTS_AND_BUDGET.md` (D28): slot bounds and class totals; slot admission,
    with the trust list refreshed in the background and embedded records verified once per hash; VALUE paging; the
    STORE_ACK fields; the replication rules and rate; expiry at STORE and VALUE; held bytes answered without
    verifying again; the STORE byte allowance; and the verification budget on station connections;
  - SUBSCRIBE entries per connection have a configured maximum;
  - a node that holds private keys runs with Erlang crash dumps disabled (`ERL_CRASH_DUMP_BYTES=0`), or written only
    to a private location readable by its own user;
  - BEP44 bootstrap: its items are Ed25519 by specification. Open: Terra checks whether it runs anywhere. If it
    does, Raf decides between dropping it and a dedicated classical key with an exemption from the no-classical
    test.
- **Red first:**
  - every dial, including a redundancy dial, refuses an endpoint whose identity differs from the chosen node_id,
    and the DHT dialer returns its error reply;
  - the station handshake tests of WP 1.5 pass against a station instance.
- **Tests to migrate when the station moves onto `macula` 12:** `macula` 12 has no `macula_identity`, no
  `macula_frame:sign/2` or `verify/2`, and no `puzzle_difficulty` setting (D30). These make peer ids with
  `macula_identity:generate/0` and `public/1`, and must make them with the node-key API instead
  (`macula_node_keys:generate/2` and `node_id/1`): `macula_station_outbound_links_sup_tests`,
  `macula_station_outbound_identity_SUITE`, `macula_station_overlay_relay_SUITE` (which also signs frames),
  `macula_station_handshake_timing_measurement` and `macula_swim_stale_conn_tests`. `macula_station_gated_call_SUITE`
  mints UCANs with `macula_ucan_nif`, which `macula` 12 replaces with `macula_ucan:create/4` over a node key; a
  policy names its issuer by node_id or realm key id. The station's `sys.config`,
  `test.sys.config`, `ct.sys.config`, `macula_station_test_cluster` and `fleet_SUITE` set `puzzle_difficulty`, which
  `macula` 12 refuses at start.
- **Done:** green; the station builds against a local checkout of `macula` 11.0.0 (D20).
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
  - content probes fetch content from the node that shares it, through stations (D27);
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
    which records the disconnect reason and fails hard;
  - an honest relay forwarding objects the next hop refuses for receiver-dependent reasons is never paused, and a
    checked advertisement gets a place in a slot whose unchecked places are full (D28).
- **Red first:** the suite against today's releases shows X25519.
- **Done:** green, with captures stored as CI artifacts.
- **Effort:** 2 to 3 days for the Erlang share of the wire checks.

---

## Stage 3: 11.0.0 stations, station directory and realm

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
  - in the EU profile each credential carries the LAMPS composite signature `id-MLDSA87-RSA4096-PSS-SHA512`,
    valid only if both halves verify (D4, D7 as amended 2026-09-22); how a credential carries it is set here ⚠;
  - D7's amendment of 2026-09-22 puts every ML-DSA signature on `macula-mldsa`; how the realm's X.509 signing,
    through OTP `public_key` today, moves onto it is not yet decided ⚠;
  - OTP 28.1.1 `public_key` signs and validates ML-DSA X.509 ✅, and OTP signs and verifies brainpool ECDSA ✅ and
    RSA-PSS ✅;
  - leaf issuance, ownership proofs and membership checks take post-quantum keys, carried in full (D13);
  - the realm key that signs realm records and each org key that signs procedure delegations (key purposes realm and
    org) are generated and stored per D6, and their custody is set here, including the `hecate` org key (D25);
  - an org directory entry for every publisher, so that every procedure carries a provider authorization; a
    publisher whose procedures have no org namespace today moves under one first (D25);
  - `issue_membership_ucan` names the device by node_id in `aud` (D7), in the same change as every checker
    (WP 1.4, WP 4.2): `MaculaRealm.Identity.RealmUcanIssuer` mints with `:macula_ucan.create/4` and the realm key,
    in place of `:macula_ucan_nif.create/5`, which `macula` 12 no longer has; a service names the realm's issuer by
    that key's key id (`realm_member_required`). This starts only after `macula` 12 lands (Raf's order);
  - the mint is the last of five steps, since the realm's device identity is Ed25519 in four places before it, and a
    `macula` 12 device, an ML-DSA node key named by node_id, is refused at the first (Saturnus, from macula-realm
    main, 2026-09-22):
    - `DeviceKeyOwnershipProof`, the device-tier RPC join, takes only a 32-byte key and verifies with Ed25519;
    - `JoinSession`'s changeset, the web join, accepts only a 32-byte Ed25519 public key;
    - `MembershipAdmission`'s allow and deny lists key on the hex Ed25519 key, so operators' lists move to
      node_ids;
    - `IssueRealmCertificates.issue_ed25519_app_cert` builds the device certificate around an Ed25519 key, under
      an RSA realm CA;
  - the realm carries its profile (D1);
  - the `io.macula` realm deployment in the EU profile, as a separate deployment on the post-quantum fleet
    (D14, D19), and a
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

### WP 3.2 The 11.0.0 stations

- [ ] The second post-quantum fleet's station instances run 11.0.0 in their profile, on reprovisioned boxes
  and hostnames next to the live fleet, before any consumer moves (D14 reconsidered, 2026-09-16).
- **Owner:** Terra.
- **Waiting on:** Stage 2; D21 done first.
- **Change:**
  - today's stations stay pinned to `macula-station` 311c0bf until the last consumer's cutover (D21);
  - the 2+2 split (decided 2026-09-16, `macula-demo` `plans/PLAN_MACULA_11_DEPLOY.md`): the live fleet ends at
    frankfurt + falkenstein, and the post-quantum fleet takes over the existing Hetzner boxes —
    `relays-hetzner-nuremberg` first as `pq.station-de-nuremberg`, `relays-hetzner-helsinki` second as
    `pq.station-fi-helsinki` — in their profile (D2), prepared US profile first (D15), zero new spend;
    consumers get the fleet's seeds and move over in their stage;
  - `stations.csv` gains node_id and profile columns, placed before the notes column;
  - seed lists are generated after each station instance's first boot, when its node_id exists, and before the
    client releases that compile them in; every compiled-in seed list is generated from the csv, including the one
    in `macula-e2e`;
  - station configurations, including bootstrap `outbound_peers` with node_ids, are generated from the csv. Before
    regenerating, the sync reaches every station, and hand-picked peer choices are pinned in the csv or the
    topology;
  - the `io.macula` realm deploys on the post-quantum fleet in its stage, in the EU profile (D19), and the
    US-profile realm (name open);
  - the distribution relay of WP 3.4, one instance per profile, deploys on the post-quantum fleet in the same
    stage;
  - `macula-station` builds against a local checkout of `macula` during development and moves to `macula` `~> 11.0`
    from hex once Raf publishes (D20);
  - every fleet node runs chrony with NTS against at least two independent servers (D22); which servers is open;

  - station instances start with puzzle enforcement in `log_only`; `enforce` follows in WP 4.5.
- **Red first:** the Stage 2 smoke check against the 11.0.0 station instances fails before they start.
  - station instances start with puzzle enforcement in `log_only`; `enforce` follows in WP 4.5 (D30).
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

### WP 3.4 Distribution relay (`macula-dist-relay`)

- [ ] Erlang distribution runs over the post-quantum transport in each profile.
- **Owner:** Neptune for the code; Terra deploys it (WP 3.2).
- **Waiting on:** WP 1.2, WP 1.5.
- **Files:**
  - `rebar.config` (`macula` `~> 11.0` from hex once Raf publishes, a local checkout before that, D20)
  - `Dockerfile` (builder and runtime per D8)
- **Change:**
  - the relay builds against `macula` 11.0.0 and uses its connection handshake and dials
    (WP 1.5), with one profile per instance, as station instances do (D2);
  - the relay client in `macula` dials with an expected identity, like every other dial (WP 1.5).
- **Red first:** two BEAM nodes reach each other through the 11.0.0 relay in each profile, a node that
  offers only classical algorithms is refused, and the relay sees no distribution plaintext (D29).
- **Done:** green against the 11.0.0 relay.
- **Effort:** ⚠.


### WP 3.5 Demonstration video plan

- [ ] A plan for a video that shows the 11.0.0 mesh at work, with its shot list and the claim each shot makes.
- **Owner:** Mercury.
- **Waiting on:** WP 3.2.
- **Files:**
  - `plans/PLAN_PQ_DEMO_VIDEO.md` in `macula-architecture`, which is private (new); it moves to `macula` only after
    Stage 6 is green and its wording has passed the D11 check
- **Change:**
  - the plan proposed in pull request 11 of `macula`, which Raf closed, is written again;
  - it takes Saturnus's corrections from the D11 check, and his two proposed D11 sentences go to Raf as plan
    decisions;
  - every claim in it names the WP 2.2 or WP 4.5 check that is its evidence;
  - the video, its footage and any sentence about it are published only after Stage 6 is green, and every public
    sentence passes the D11 check first.
- **Done:** the plan passes Saturnus's D11 check, and Raf has decided the two sentences.
- **Effort:** 1 day.

### WP 3.5 Demonstration video plan

- [ ] A plan for a video that shows the post-quantum fleet at work, with its shot list and the claim each shot makes.
- **Owner:** Mercury.
- **Waiting on:** WP 3.2.
- **Files:**
  - `plans/PLAN_PQ_DEMO_VIDEO.md` (new)
- **Change:**
  - the plan proposed in pull request 11 of `macula`, which Raf closed, is written again;
  - it takes Saturnus's corrections from the D11 check, and his two proposed D11 sentences go to Raf as plan
    decisions;
  - every claim in it names the WP 2.2 or WP 4.5 check that is its evidence;
  - the video, its footage and any sentence about it are published only after Stage 6 is green, and every public
    sentence passes the D11 check first.
- **Done:** the plan passes Saturnus's D11 check, and Raf has decided the two sentences.
- **Effort:** 1 day.

---

## Stage 4: each other stack, with its suite against the fleet

Every stack runs the connection handshake, carries full keys (D13), binds replies to the target and the request
(D25), and passes its own wire checks against the 11.0.0 stations (WP 4.5). Public claims are made per stack (D11).

Every stack also meets these, each red first:

- its decoder refuses exactly what the decoding rule lists and accepts the rest, proven by the shared vector set
  `test/vectors/decoding_rule_v1.json` in `macula`, which its CI runs pinned by commit; the set includes a text key
  in two widths, integer key 1 beside float key 1.0, and a byte string key;
- a frame whose signed object is refused is dropped and recorded, and the next frame on the same connection still
  arrives; a frame whose envelope is malformed closes the connection;
- the record and frame rules of `DESIGN_PQ_DHT_SLOTS_AND_BUDGET.md` (D28): a domain record whose lifetime passes 7 days
  is refused; STORE_ACK is matched on `key`, `signer` and `version`; FIND_VALUE `after` and VALUE `next` are
  followed under the consumer rules of section 1.5; the put path paces record bytes to at most 1 MiB per second
  after a 16 MiB burst; the Plumtree IHAVE and HyParView placement allowances hold where the stack runs them; and
  the decoding vectors cover the new fields;
- a stack that dials providers directly, before a request is sent, tries the next authorized advertisement for a
  procedure when the chosen provider's station endpoint does not resolve or the dial fails, and retries resolution
  when none qualifies, within the call's deadline (D25 item 9);
- a stack refuses to advertise a procedure name without an org namespace, and so does every policy it offers (D25).

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
  - `src/content.rs` makes and checks blocks and chunks with SHA-384; `Mcid` takes the 50-byte form with tag 2 only,
    and a manifest that names any other hash is refused (D24);
  - content is fetched from the node that shares it, through stations, and content this stack shares is served by
    the sharing node itself (D27);
  - the token checks of WP 1.4, for calls and streams;
  - one trust mode replaces `Trust::WebPki`, `Trust::Pinned` and `Trust::Insecure`; every dial carries an expected
    identity;
  - the handshake frames and identity per WP 1.3, with the EU classical half per D4 and V8.
- **Red first:**
  - the EU composite is the LAMPS `id-MLDSA87-RSA4096-PSS-SHA512` (D7, amended 2026-09-22): the draft's own
    vector, in `macula`'s `test/fixtures/lamps_mldsa87_rsa4096_pss_sha512/`, verifies; the draft's key signs
    composites that `macula` verifies; and `test/fixtures/lamps_composite_zero_dropped/sig.bin` is refused;
  - the WP 1.2 and WP 1.5 assertions, as integration tests against the 11.0.0 stations;
  - identity key generation runs the node_id puzzle loop, regenerating only the ML-DSA-87 half of a pq_hybrid key;
  - a provider verifies every inbound CALL's signature against its caller before the handler runs; an unverified
    CALL reaches no handler and gets no reply;
  - a caller verifies every RESULT and ERROR signature against `responded_by` or `reported_by` before returning
    it; an unverified reply is never returned;
  - a subscriber verifies each EVENT's signature, and its publisher signature, before delivery; an invalid event
    is dropped, in the core crate and through the FFI;
  - a provider verifies every inbound STREAM_OPEN's signature against its caller before accept returns it, and
    the caller handed to the handler is that verified key;
  - a chunked fetch accepts no chunk until the fetched manifest's recomputed content id equals the requested id;
    a self-consistent manifest for other content is refused;
  - event dedup keys on the SHA-384 of a publication's `tbs` bytes as received, runs only on publications that
    verified, and keeps each hash until the publication's `expires_at`;
  - a publisher-owned seq counter follows the seq rule of the signed frames design;
  - each event exposes realm, topic, `publisher` as the publisher's node_id, seq, `published_at` and
    `delivered_via`, taken from the verified publication, and `publication_hash` and `expires_at` where the stack
    deduplicates;
  - streams get an authorization policy hook like calls: a STREAM_OPEN carries a capability token, and the policy
    is enforced before accept returns, in the core crate and through the FFI `accept_stream`;
  - a provider's policy is an explicit argument of every serve and accept API, and an open policy is always
    chosen by name;
  - a UCAN on a CALL or STREAM_OPEN is authorized per D7 check 2: signed target, `aud` as the verified caller's
    node_id, and the chain to the required issuer;
  - every dial (pool, direct dial, FFI) carries an expected identity, and a HELLO naming any other node_id is
    refused;
  - every build verifies the handshake signature against the presented leaf with the profile's schemes, in every
    trust mode;
  - every stream frame (STREAM_DATA, STREAM_END, STREAM_ERROR, STREAM_REPLY), from the provider and from the
    caller, is delivered only after its signature verifies against the stream's peer, with its sequence number next
    in order;
  - a record from a DHT lookup is used only after its signature verifies against its carried key and it derives to
    the storage key that was asked for.
- **Done:** green in CI; `ring` absent from `Cargo.toml`.
- **Effort:** the transport takes 3 to 4 days once WP 1.2's Rust code exists (Neptune's estimate); the identity work
  is part of the SDK identity estimate, and the D24 content work is not estimated ⚠.

### WP 4.2 `macula-go`, `macula-ts` and `macula-php`

- [ ] The Go SDK and both FFI layers run the post-quantum profile.
- **Owner:** Venus.
- **Waiting on:** V5, WP 1.3, WP 3.2; for the EU profile also V8.
- **First:** unify the two FFI layers so the identity ABI changes once. `macula-ts` builds a static library into a
  Node addon with five prebuilt binaries; `macula-php` loads a shared library through `FFI::cdef`. Every size
  mismatch across the FFI fails, and never truncates. The merged C interface gives `macula-ts` caller and provider
  streams, which `macula-php`'s layer already has; a TS provider gets this package's STREAM_OPEN checks, so both are
  built together (Raf, 2026-09-10).
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
    binaries without extra C dependencies (V8);
  - the token checks of WP 1.4, for calls and streams, with an authorization policy that `macula-ts` can set and
    that `macula-php` applies per realm and procedure;
  - in the post-quantum formats, bytes for agents keep the 10.x rules (Raf, 2026-09-10):
    - a JSON object whose only key is `"$bytes"`, holding padded base64 (RFC 4648 section 4), becomes a CBOR byte
      string; any other value under that sole key is an error, never a map;
    - an object with more keys stays a map, and a plain string is always text: there is no `"0x"` input form;
    - tagged output is opt-in per call, subscription and serve, and `"0x"` with hex stays the default;
    - Go's FFI layer makes the output choice, because only Go knows which values were bytes.
- **Red first:**
  - the EU composite is the LAMPS `id-MLDSA87-RSA4096-PSS-SHA512` (D7, amended 2026-09-22): the draft's own
    vector, in `macula`'s `test/fixtures/lamps_mldsa87_rsa4096_pss_sha512/`, verifies; the draft's key signs
    composites that `macula` verifies; and `test/fixtures/lamps_composite_zero_dropped/sig.bin` is refused;
  - in `macula-go`, identity key generation runs the node_id puzzle loop, regenerating only
    the ML-DSA-87 half of a pq_hybrid key; `macula-ts` takes it through Go;
  - `transport/pq_handshake_test.go` (new) asserts `ConnectionState().TLS.CurveID`, the certificate signature
    algorithm and the cipher suite per profile, and refusal of a classical-only peer and of an unbound TLS key;
    an FFI size-mismatch test fails instead of truncating;
  - signatures on inbound frames, each checked end to end against its origin, not the forwarding station:
    - a provider runs a CALL handler only after the CALL's signature verifies against its caller;
    - a caller accepts a RESULT or ERROR only after its signature verifies against the answering node, and only
      when that node is the provider the call was addressed to (D25);
    - every stream frame (STREAM_OPEN, STREAM_DATA, STREAM_END, STREAM_ERROR, STREAM_REPLY) is delivered only
      after its signature verifies against the stream's peer;
    - an EVENT reaches a subscriber only after its publisher signature verifies, on every subscribe path: the
      callback subscriber, `RecvEvent` and the pool;
  - station identity:
    - a dial with an expected station identity closes the session before CONNECT is sent if the station's
      proven identity differs;
    - the CONNECT proof binds the station's proven identity to this handshake through the SHA-384 of the leaf
      certificate, and session resumption stays off, so the leaf is always this handshake's;
    - every dial path either validates the certificate or applies that binding;
    - the FFI marks the station node id as verified only for sessions where it was checked against an expected
      identity;
  - content:
    - a chunked fetch recomputes the manifest's id in `computeMcid`'s canonical form and refuses a manifest
      whose id is not the requested id;
    - a single-block fetch re-hashes the block and refuses one whose hash is not the requested id;
    - content ids have only tag 2, SHA-384, in `computeMcid` and in every content id check (D24);
    - content is fetched from the node that shares it, through stations, and content a node shares is served by
      that node itself (D27);
  - event dedup keys on the SHA-384 of a publication's `tbs` bytes as received, runs only on publications that
    verified, and keeps each hash until the publication's `expires_at`; the FFI event carries `publication_hash` and
    `expires_at`, because `macula-ts` deduplicates on the TypeScript side;
  - every publishing path follows the seq rule of the signed frames design, including `connection/publisher.go`'s
    meta-fact counter and the `cabi` publish counter that `macula-ts` publishes through;
  - each event exposes realm, topic, `publisher` as the publisher's node_id, seq, `published_at` and
    `delivered_via`, taken from the verified publication, and `publication_hash` and `expires_at` where the stack
    deduplicates;
  - authorization:
    - calls and streams both take an authorization policy, enforced before any handler runs; `macula-ts` can
      set it, and `macula-php`'s gated export applies it per realm and procedure;
    - a call or stream handler receives the caller's identity only after the caller's signature verifies;
    - a STREAM_OPEN naming a procedure this session did not advertise is refused before the application sees it;
    - a request is authorized per D7 check 2, in order: the frame signature against the caller key, the signed
      target against the provider's node_id, the token's `aud` against the node_id of that caller key, then the
      chain to the required issuer; each token's signature is checked before its claims, and a token without
      `exp`, without a capability for the procedure and realm, or with a delegation that does not verify is
      refused with its own error;
    - a request past its signed deadline plus the D22 tolerance is refused, and (caller, call id) deduplication
      holds until then;
    - the standalone token functions in Go, TS and PHP follow the same rule: the authorizing verify takes the
      verified caller key and the expected target, and a chain-only check has a name that says it does not
      authorize;
    - every UCAN gate checks `aud` against the node_id of the verified caller, including the issuer-only
      `RequireUcanIssuer`;
  - FFI: `macula_identity_sign` returns the whole signature for every profile, 4,627 bytes for ML-DSA-87, and
    never cuts it to a fixed-size buffer;
  - FFI values in `macula-ts` and `macula-php`: a value crosses the FFI unchanged or not at all; an integer outside
    the target language's exact range, a map or list the binding cannot represent, or a map key that is not text is
    an error to the caller, never a substitute value;
  - a record from a DHT lookup is used only after its signature verifies against its carried key and it derives to
    the storage key that was asked for.
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
  - the `cryptography` floor raised to a version with ML-KEM and ML-DSA;
  - the token checks of WP 1.4, for calls and streams.
- **Red first:**
  - the EU composite is the LAMPS `id-MLDSA87-RSA4096-PSS-SHA512` (D7, amended 2026-09-22): the draft's own
    vector, in `macula`'s `test/fixtures/lamps_mldsa87_rsa4096_pss_sha512/`, verifies; the draft's key signs
    composites that `macula` verifies; and `test/fixtures/lamps_composite_zero_dropped/sig.bin` is refused;
  - identity key generation runs the node_id puzzle loop, regenerating only the ML-DSA-87 half of a pq_hybrid key;
  - `tests/test_pq_handshake.py`, against the 11.0.0 stations: success in the client's profile, and failure against a
    classical-only station and against an unbound TLS key;
  - `Session.connect` takes the expected station identity from the seed list and refuses a station whose HELLO
    identity differs; the client refuses a station whose TLS leaf key is not bound to its HELLO identity, and no
    configuration skips that check; the CONNECT proof covers the SHA-384 of the station's leaf DER from the same
    session; the client never sets a session ticket or ticket handler, and refuses a session without a peer
    certificate;
  - an incoming CALL or STREAM_OPEN runs its handler only if its signature verifies against its caller;
    otherwise it is refused and the handler never runs;
  - a RESULT or ERROR is accepted only if its signature verifies against its responder and that responder is the
    provider the call was meant for (D25);
  - STREAM_DATA, STREAM_END, STREAM_REPLY and STREAM_ERROR are delivered only if they verify against the
    stream's peer, and the signer is kept on inbound frames;
  - an EVENT is delivered only if its publisher signature verifies, and every PUBLISH carries a publisher
    signature that test vectors from `macula` verify;
  - event dedup keys on the SHA-384 of a publication's `tbs` bytes as received, runs only on publications that
    verified, and keeps each hash until the publication's `expires_at`;
  - a publisher-owned seq counter follows the seq rule of the signed frames design, in place of a seq its caller
    passes;
  - each event exposes realm, topic, `publisher` as the publisher's node_id, seq, `published_at` and
    `delivered_via`, taken from the verified publication, and `publication_hash` and `expires_at` where the stack
    deduplicates;
  - call and stream handlers receive the verified caller identity, and a policy hook sees that identity and the
    token before any handler runs; a denial replies unauthorized without running the handler;
  - a chunked fetch recomputes the manifest id and refuses a manifest whose id differs from the requested id
    before fetching any chunk, and a single-block fetch checks the block hash against the requested id;
  - content ids have only tag 2, SHA-384, in every content id made and checked (D24);
  - content is fetched from the node that shares it, through stations, and content this stack shares is served by
    the sharing node itself (D27);
  - the token checks of WP 1.4;
  - a record from a DHT lookup is used only after its signature verifies against its carried key and it derives to
    the storage key that was asked for.
- **Done:** green in CI.
- **Effort:** 6 to 9 days, plus the seed list and record verifier ⚠; the identity work is part of the SDK identity
  estimate.

### WP 4.4 `macula-dotnet`

- [ ] Not in the first switch (D10).
- **Owner:** Uranus.
- .NET programs keep working against today's stations until the 11.0.0 deploy (D14).
- **Recorded for when msquic supports post-quantum key exchange:**
  - msquic on OpenSSL 3.5 or newer;
  - the binding check after the handshake, against `QuicConnection.RemoteCertificate`;
  - an expected node id in `Seed`, and one trust mode replacing `Trust.Pinned`, `Trust.WebPki` and
    `Trust.Insecure`;
  - the connection handshake frames;
  - identity key generation with the node_id puzzle loop, on the ML-DSA-87 half of a pq_hybrid key;
  - a field declared as text is read only from a text value, and a byte string is never read as text;
  - signed frames and records are verified per the design before delivery: an EVENT only after its publisher
    signature verifies, a reply only from the addressed provider, a record only after its signature and slot
    verify;
  - each event exposes realm, topic, `publisher` as the publisher's node_id, seq, `published_at` and
    `delivered_via`, taken from the verified publication, and `publication_hash` and `expires_at` where it
    deduplicates;
  - event dedup keys on the SHA-384 of a publication's `tbs` bytes as received, runs only on publications that
    verified, and keeps each hash until the publication's `expires_at`;
  - `SupervisedPubSub` and `RpcFacts` follow the seq rule of the signed frames design;
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

- [ ] Every client stack's cells are green against the 11.0.0 stations.
- **Owners:** each stack owner; Terra for the harness and captures.
- **Waiting on:** the stack's work package, V10.
- **Change:**
  - each client stack against the 11.0.0 station instances, in its profile;
  - the group, signature scheme and cipher suite checked two independent ways that agree;
  - the connection handshake checked, and a classical-only client and X25519MLKEM768 refused;
  - the cross-stack leaf-hash vector through each stack's real handshake and accessor;
  - once every stack generates puzzle-valid identity keys (D30), the fleet switches puzzle enforcement from
    `log_only` to `enforce`, and every cell passes again under `enforce`.
- **Done:** every cell green for its profile, with captures stored as CI artifacts. This is the evidence for any
  public claim about that stack.
- **Effort:** 2 to 3 days, together with WP 2.2.

---

## Stage 5: `macula-cli`, `macula-mcp` and `lazymesh` on 11.0.0

### WP 5.1 Tools


- [ ] The tools run against the post-quantum fleet's 11.0.0 stations (D14, reconsidered 2026-09-16).
- **Owners:** Venus (`macula-cli`, `macula-mcp`), Mars (`lazymesh`).
- [ ] The tools run against the new fleet.
- **Owners:** Venus (`macula-cli`, `macula-mcp`), Mars (`lazymesh`).
- **Waiting on:** WP 4.2, WP 3.2; a tool that joins `io.macula` also waits on their EU parts (D19).
- **Change:**
  - new seeds with node_ids, including compiled-in or documented defaults, such as the default station of
    `macula-mcp`'s hello tool;
  - device proofs carry the full key (D13);
  - `macula-cli serve --require-ucan-issuer` checks the token's `aud` against the node_id of the verified caller
    (D7);
  - agent stream tools in `macula-mcp`: open, bounded read and close, on the `macula-ts` streams of WP 4.2 (Raf,
    2026-09-10);
  - `macula-mcp` keeps the bytes-for-agents rules of WP 4.2 in the post-quantum formats, including `"0x"` with hex as
    its default output, which its internal decoders use;
  - `macula-mcp`'s `mesh_find_records_by_type` reads a procedure advertisement's realm and procedure from its
    `realm_id` and `procedure` fields (WP 1.3);
  - `macula-mcp`'s `mesh_put` and `mesh_get` serve content from the node that shares it and fetch it from that node,
    through stations (D27);
  - `macula-mcp`'s envelope attestation compares an envelope's `from` with the event's `publisher`, both as node_ids;
  - every procedure a tool advertises, and every procedure its examples and defaults name, has an org namespace
    (D25);
  - release on tag: goreleaser for `macula-cli`, npm for `macula-mcp`.
- **Red first:** each tool's connection test against 11.0.0 station instances fails before its change.
- **Effort:** ⚠.

---

## Stage 6: the hecate services on 11.0.0

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
    values change when nodes get new identities;
  - every handler that reads payload fields reads them through `macula:field/2,3` and `macula:text/1` (D26);
  - services read an event's meta through `realm`, `publisher` as the publisher's node_id, `seq`, `published_at`
    and `delivered_via`; every event a service receives is verified, so no service reads `publisher_verified` or a
    publisher signature field (hecate-graph, hecate-agora, hecate-mods);
  - a service event that recorded `publisher_verified` moves to a new event version without that field, and events
    already stored stay as they are (hecate-agora);
  - every hecate service procedure moves under the org namespace `hecate`, with a procedure delegation per service
    signed by the `hecate` org key of WP 3.1; SDK examples, `macula-mcp` and `macula-e2e` callers move with the
    rename (D25);

  - no hecate service advertises a name without an org namespace; the names to rename are listed with their owners
    (open item);
  - no hecate service advertises a name without an org namespace. The bare names found in use start the list:
    `rag_search`, `rag_contribute`, `reach_web`, `graph_learn`, `graph_ask_links`, `graph_ask_entity`,
    `hecate-nvidia-pair.chat`, `hecate-llm.stream_chat` and the tube procedures (Pluto's first pass, 2026-09-14; the
    full list is an open item);
  - services that share content keep it and serve it themselves (D27);
  - a node that holds private keys runs with Erlang crash dumps disabled (`ERL_CRASH_DUMP_BYTES=0`), or written only
    to a private location readable by its own user.
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
    seen is refused, and so is a caller stream frame from another signer or out of sequence (D17);
  - an advertisement without valid provider authorization, or expired, is never a target.
- **Public claims** follow D11, and only after the stack's wire checks are green.

---

## Release after Stage 6

`macula` 11.0.0 no longer waits on WP 3.1 for provider certificate chains: the certificate authorization form is
removed (D25 item 6, revised 2026-09-15), since the realm issues no X.509 certificates (design B1).
It lands together with the org namespace migration (`PLAN_11_ORG_NAMESPACE_MIGRATION.md`): no binary running at
core cutover advertises a procedure without an org or node namespace.

1. `macula` 11.0.0 on hex, the release that opens the post-quantum fleet to consumers. Raf publishes hex;
   consumers move to `~> 11.0` only then, and none commits a git or branch dependency on `macula` before (D20).
2. SDK releases on their publish triggers: `macula-go` tag, `macula-cli` (goreleaser on tag), `macula-ts` (npm on
   tag), `macula-mcp` (npm on tag), `macula-php`, `macula-rust` (cargo on tag), `macula-py` (PyPI on tag), and
   later `macula-dotnet` (NuGet on tag).
3. The post-quantum fleet runs next to the live one, consumers move over in their stages, and the 311c0bf mesh
   ends after the last cutover (D14, reconsidered 2026-09-16).

---

## Files to create or change

### `macula` (Stage 1)

- WP 1.1:
  - `src/crypto_profile/macula_crypto_profile.erl` (new)
  - `src/macula_app.erl`
  - `config/test.sys.config` (new) and `rebar.config`
- WP 1.2:
  - `native/macula_quic/{Cargo.toml, src/config.rs, src/cert.rs}`
  - `native/macula_quic/{src/connection.rs, src/endpoint.rs, src/lib.rs}`
  - `src/peering/macula_quic.erl`
  - `.github/workflows/publish-hex.yml`
- WP 1.3:
  - the identity, record, content, frame, client replay and dedup, overlay, pubsub and cert-system modules listed in
    WP 1.3
  - `native/macula_crypto_nif`
  - `rebar.config`
- WP 1.4:
  - `src/auth/macula_ucan.erl`
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
  - `test/vectors/decoding_rule_v1.json` (new)
  - extended identity, record, content block hash, frame, handshake, crypto NIF, cert chain, UCAN and DID tests

### `macula-station` (WP 1.6)

- the files listed in WP 1.6

### `macula-internal/macula-e2e` (WP 2.1, WP 2.2, WP 4.5)

- the harness, the handshake and claim-gate suites, and the seed list generated from `stations.csv`

### `macula-realm` (WP 3.1)

- the files listed in WP 3.1

### `hecate-stations` (WP 3.3)

- the files listed in WP 3.3

### `macula-dist-relay` (WP 3.4)

- `rebar.config`
- `Dockerfile`


### `macula-architecture` (WP 3.5)

- `plans/PLAN_PQ_DEMO_VIDEO.md` (new), until Stage 6 is green

### `macula` plans (WP 3.5)

- `plans/PLAN_PQ_DEMO_VIDEO.md` (new)

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


---

## 11.0.0 removals

What `macula` 11.0.0 removes, in one place. Each entry names what is deprecated, what replaces it, and what must
move first. An owner who deprecates something adds its entry here; some entries belong to changes that are not on
`main` yet. The `[11.0.0]` Removed section of the post-quantum CHANGELOG records each removal when it is made.

- `macula_frame:parse_stream/1` (Neptune).
  - Replacement: `macula_frame:parse_received/1`, which returns `{ok, Items, Tail}` or
    `{malformed, ItemsBefore, Reason}`.
  - Moves first: its two callers in `macula-station`, in the station's release B (Mars). The `-deprecated`
    attribute follows in the next `macula` minor, because the station's xref checks deprecated calls; until then
    the deprecation is in the documentation and the CHANGELOG only.
- `macula:get_cookie/0`, `macula:set_cookie/1`, `macula_cluster:get_cookie/0` and `macula_cluster:set_cookie/1`
  (Pluto).
  - Replacement: `erlang:get_cookie/0` and `erlang:set_cookie/1` on a distributed node, whose cookie comes from its
    owner-only cookie file or its release.
  - Moves first: `bc-gitops` (`bc_gitops_cluster` and `bc_gitops_vm_spawner`).
- `macula_direct_dial:resolve_content_provider/2`.
  - Replacement: `macula_direct_dial:fetch_content/4`.
  - Moves first: its caller in `macula-internal/macula-e2e` (`macula_e2e_duel`, Terra, WP 2.1).
- The `dht` and `mdns` clustering strategy values, with `macula_cluster_strategy`, `macula_dist_discovery` and
  `macula_dist_mdns_advertiser` (Neptune).
  - Replacement: the `gossip` strategy on a LAN, or `static` with a node list.
  - Moves first: nothing.
  - Noted in the [Clustering Guide](../docs/guides/CLUSTERING_GUIDE.md), in the READMEs of
    [`macula_cluster_system`](../src/macula_cluster_system/README.md) and
    [`macula_dist_system`](../src/macula_dist_system/README.md), and in `macula_dist_discovery`.
- `mdns` in the `optional_applications` of `src/macula.app.src`, which names no installed application.
  - Replacement: none. Moves first: nothing.
- The modules `macula_console` and `macula_cert_system` (Mercury).
  - Replacement: start `macula_trust_store` directly. Moves first: nothing; no caller in the workspace on
    2026-09-14.
- `macula_mri:index_descendants/3`, `index_insert/4`, `index_remove/3`, `index_size/1` and `is_valid/1`;
  `macula_names:local_node_id/0`; `macula_source_route:version/1`; `macula_quic:accept_stream/3`,
  `async_shutdown_connection/3` and `handoff_stream/3`; `macula_crypto_nif:blake3_streaming/1` and
  `blake3_verify/2`; `hecate_or_set:tombstones/1`; `macula_hyparview_view:contains/2` (Mercury).
  - Replacement: none. Moves first: nothing; no caller in the workspace on 2026-09-14.
- Exports that end while their modules keep the functions: `macula_mri:parent_type/1`,
  `macula_mri_registry:list_custom_types/0` and `macula_dist_relay_protocol:decode/1` (Mercury).
  - Replacement: none outside their modules. Moves first: nothing; no caller in the workspace on 2026-09-14.

---

## 11.0.0 removals

What `macula` 11.0.0 removes, in one place. Each entry names what is deprecated, what replaces it, and what must
move first. An owner who deprecates something adds its entry here; some entries belong to changes that are not on
`main` yet. The `[11.0.0]` Removed section of the post-quantum CHANGELOG records each removal when it is made.

- `macula_frame:parse_stream/1` (Neptune).
  - Replacement: `macula_frame:parse_received/1`, which returns `{ok, Items, Tail}` or
    `{malformed, ItemsBefore, Reason}`.
  - Moves first: its two callers in `macula-station`, in the station's release B (Mars). The `-deprecated`
    attribute follows in the next `macula` minor, because the station's xref checks deprecated calls; until then
    the deprecation is in the documentation and the CHANGELOG only.
- `macula:get_cookie/0`, `macula:set_cookie/1`, `macula_cluster:get_cookie/0` and `macula_cluster:set_cookie/1`
  (Pluto).
  - Replacement: `erlang:get_cookie/0` and `erlang:set_cookie/1` on a distributed node, whose cookie comes from its
    owner-only cookie file or its release.
  - Moves first: `bc-gitops` (`bc_gitops_cluster` and `bc_gitops_vm_spawner`).
- `macula_direct_dial:resolve_content_provider/2`.
  - Replacement: `macula_direct_dial:fetch_content/4`.
  - Moves first: nothing.
- The `dht` and `mdns` clustering strategy values, with `macula_cluster_strategy`, `macula_dist_discovery` and
  `macula_dist_mdns_advertiser` (Neptune).
  - Replacement: the `gossip` strategy on a LAN, or `static` with a node list.
  - Moves first: nothing.
  - Noted in the [Clustering Guide](../docs/guides/CLUSTERING_GUIDE.md), in the READMEs of
    [`macula_cluster_system`](../src/macula_cluster_system/README.md) and
    [`macula_dist_system`](../src/macula_dist_system/README.md), and in `macula_dist_discovery`.
- `mdns` in the `optional_applications` of `src/macula.app.src`, which names no installed application.
  - Replacement: none. Moves first: nothing.
- The modules `macula_console` and `macula_cert_system` (Mercury).
  - Replacement: start `macula_trust_store` directly. Moves first: nothing; no caller in the workspace on
    2026-09-14.
- `macula_mri:index_descendants/3`, `index_insert/4`, `index_remove/3`, `index_size/1` and `is_valid/1`;
  `macula_names:local_node_id/0`; `macula_source_route:version/1`; `macula_quic:accept_stream/3`,
  `async_shutdown_connection/3` and `handoff_stream/3`; `macula_crypto_nif:blake3_streaming/1` and
  `blake3_verify/2`; `hecate_or_set:tombstones/1`; `macula_hyparview_view:contains/2` (Mercury).
  - Replacement: none. Moves first: nothing; no caller in the workspace on 2026-09-14.
- Exports that end while their modules keep the functions: `macula_mri:parent_type/1`,
  `macula_mri_registry:list_custom_types/0` and `macula_dist_relay_protocol:decode/1` (Mercury).
  - Replacement: none outside their modules. Moves first: nothing; no caller in the workspace on 2026-09-14.
