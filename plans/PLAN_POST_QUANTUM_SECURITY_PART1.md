# Plan: post-quantum security for Macula, part 1: verified facts and Stage 0 checks

**Root document:** [PLAN_POST_QUANTUM_SECURITY.md](PLAN_POST_QUANTUM_SECURITY.md). The legend, owners, key model
and decisions are there. [PLAN_POST_QUANTUM_SECURITY_PART2.md](PLAN_POST_QUANTUM_SECURITY_PART2.md) has the stages
and work packages.

**Last Updated:** 2026-09-10

---

## Already verified

### Per TLS stack: summary

| Stack | Pure ML-KEM-1024 | SecP384r1MLKEM1024 | ML-DSA in TLS | Composite certs | Hash-based in TLS |
|---|---|---|---|---|---|
| rustls 0.23.44 | yes | no; custom group proven (V4) | yes | none | none |
| Go 1.27 | yes | yes | yes | none | none |
| OpenSSL 3.6.4 | yes | yes | yes | none | none |
| .NET 10 with msquic | no | no | no | none | none |
| aioquic 1.3.0 | no; patch proven (V7) | no; patch proven (V7) | no; patch proven (V7) | none | none |

### Per TLS stack: details

#### rustls 0.23.44 `aws-lc-rs` provider (macula NIF, stations, macula-rust)

- **Pure ML-KEM-1024:** yes ✅.
- **Hybrid groups:** X25519MLKEM768, SecP256r1MLKEM768 ✅.
  - **No SecP384r1MLKEM1024** ✅.
  - `SupportedKxGroup` and `ActiveKeyExchange` are public traits ✅, so it can be added outside rustls. V4 did so
    in about 90 lines over rustls's own SECP384R1 and MLKEM1024, with the ECDH share first as in Go 1.27 ✅.
- **ML-DSA certificate and handshake signature:**
  - codepoints ✅;
  - `rustls-webpki` 0.103.15 verifies ML-DSA and exports `ML_DSA_87` ✅;
  - the provider signs ML-DSA-44, 65 and 87 from rustls 0.23.44, loading ML-DSA PKCS#8 keys through
    `with_single_cert`, so no custom signing key is needed; 0.23.43 could not ✅ (V4);
  - `rcgen` 0.14.10 makes ML-DSA keys and certificates under `aws_lc_rs` ✅, through aws-lc-rs ML-DSA-87 signing ✅.
- **Client verification building block:** `rustls::crypto::verify_tls13_signature` checks a handshake signature
  against a presented certificate ✅.
- **Session tickets:** a server sends 2 TLS 1.3 tickets by default, and none with `send_tls13_tickets = 0` ✅.
  `NoServerSessionStorage` stores and returns no session ✅. A client with `Resumption::disabled()` offers none ✅.
  `max_early_data_size` defaults to 0 ✅.
- **Composite certificate signatures:** none ✅.
- **Hash-based signatures (SLH-DSA, LMS, XMSS):** none ✅.

quinn 0.11.11, which the NIF and macula-rust use:

- `Connection::peer_identity` returns the peer's certificate chain, leaf first ✅;
- `export_keying_material` exists ✅, and the NIF does not expose it;
- the peer learns of a new stream only once the opener writes to it ✅;
- the NIF creates a connection handle only after the handshake, including the server certificate verifier, has
  succeeded ✅.

#### Go 1.27 `crypto/tls` (macula-go, macula-ts, macula-php)

- **Pure ML-KEM-1024:** yes; pure ML-KEM-768 is refused ✅.
- **Hybrid groups:** SecP384r1MLKEM1024 and SecP256r1MLKEM768 ✅ (real handshakes with ML-DSA-87).
- **ML-DSA certificate and handshake signature:** works ✅ (real handshakes with ML-DSA-65 and ML-DSA-87).
  Go 1.26 has no ML-DSA ✅.
- **Handshake signature:** the TLS 1.3 client checks CertificateVerify unconditionally ✅.
- **Peer certificate:** `PeerCertificates[0].Raw` holds the certificate as received ✅. On a resumed session Go takes
  the certificates from the stored session ✅, so no client session cache is set.
- **Composite certificate signatures:** none ✅.
- **Hash-based signatures:** none ✅.

#### OpenSSL 3.6.4 (reference; .NET on Linux through msquic)

- **Pure ML-KEM-1024:** yes ✅.
- **Hybrid groups:** SecP384r1MLKEM1024 ✅ (real handshake with ML-DSA-87 and AES-256-GCM).
- **ML-DSA certificate and handshake signature:** works ✅.
- **Composite certificate signatures:** none ✅.
- **Hash-based signatures:** SLH-DSA primitives ✅; no TLS 1.3 signature scheme for them ✅.

#### .NET 10.0.11 and msquic (macula-dotnet, client only)

- **Unofficial.MsQuic 2.4.10:** embeds OpenSSL 1.1.1w, so no ML-KEM, no hybrid groups and no ML-DSA ✅.
- **Microsoft.Native.Quic.MsQuic.OpenSSL 2.6.0:** Windows DLLs only, built on quictls 3.1.7, with no ML-KEM or
  ML-DSA ✅.
- **msquic's `openssl` backend** (an external OpenSSL 3.5 or newer): sets the key exchange groups to secp256r1 and
  x25519 on every connection, in `src/platform/tls_openssl.c`, still in v2.6.1 ✅. `OPENSSL_CONF` group settings do
  not reach it, and no msquic or .NET 10 API sets the groups ✅. A .NET process could choose a profile only
  process-wide ✅.
- **Peer certificate:** `QuicConnection.RemoteCertificate` is set before `ConnectAsync` returns ✅. Its `RawData` is
  OpenSSL's re-serialization, equal to the received bytes for a strict DER certificate ✅.
- **Composite certificate signatures:** composite ML-DSA primitives, not in TLS ✅.
- **Hash-based signatures:** SLH-DSA primitives ✅.

#### aioquic 1.3.0 with cryptography 50.0.1 (macula-py)

- **Unpatched:** no ML-KEM, no hybrid groups and no ML-DSA handshake signature ✅. Groups and signature algorithms
  are fixed in `tls.Context` and cannot be set from `QuicConfiguration` ✅.
- **Client patch, proven (V7):** a patch to five client functions in aioquic's pure-Python `tls.py`, about 125
  lines, completes handshakes against a quic-go 0.62.0 server on Go 1.27 ✅:
  - US profile: group 0x0202 (ML-KEM-1024), CertificateVerify 0x0906 (ML-DSA-87) verified,
    TLS_AES_256_GCM_SHA384, not resumed;
  - EU profile: group 0x11ED (SecP384r1MLKEM1024), the same otherwise;
  - the unpatched client and a US client against an EU server are refused.
- **Cipher suite:** `QuicConfiguration.cipher_suites` accepts AES_256_GCM_SHA384 ✅.
- **cryptography 50.0.1** (bundled OpenSSL 4.0.2): ML-KEM-1024 and ML-DSA-87 work, including tamper rejection, and
  an ML-DSA-87 self-signed certificate builds, reloads and verifies ✅.
- **Peer certificate:** unpatched aioquic keeps only a parsed object in a private attribute. The patch keeps the raw
  leaf DER, byte-exact with the certificate the server holds ✅. A resumed session has no peer certificate ✅.
- **Composite certificate signatures:** none ✅.
- **Hash-based signatures:** none ✅.

#### OTP `crypto` and `public_key` (macula identity, realm CAs)

- **Pure ML-KEM-1024:** ML-KEM primitives in OTP 28 and 29 ✅.
- **Hybrid groups:** not applicable.
- **ML-DSA:** sign and verify ✅; OTP 28.1.1 validates and signs ML-DSA X.509 ✅.
- **ML-DSA-87 signing is randomized:** two signatures over one message with one key differ and both verify, with
  digest type `none` ✅ (OTP 28.1, OpenSSL 3.6.4).
- **RSA-PSS with SHA-384:** sign and verify ✅ (OTP 28.1, OpenSSL 3.6.4).
- **Composite certificate signatures:** none ✅.
- **Hash-based signatures:** SLH-DSA in OTP 28.4.2 and 29.0.6 ✅, not in OTP 28.1.1 ✅, all on OpenSSL 3.6.4 (V3).
- **OpenSSL floor:** OTP 28.1, 28.4.2 and 29.0.6 enable ML-DSA and ML-KEM only when compiled against OpenSSL
  headers 3.5.0 or newer ✅ (V3).

### Classical half and key binding, per stack

**ECDSA on brainpoolP384r1:**

- OTP 28.1.1: works ✅ (real sign and verify, 102 B signature).
- cryptography 50 (macula-py): works ✅ (real sign and verify).
- OpenSSL 3.6.4: curve present ✅.
- .NET 10.0.11: curve present ✅.
- Go 1.27 standard library: **no**. `crypto/ecdsa` implements only P-224, P-256, P-384, P-521 ✅.
  ProtonMail go-crypto, keybase and ebfe interoperate with OTP in both directions and build for all five
  macula-ts targets with cgo off ✅, but they sign through Go's deprecated custom-curve path on `math/big`, which is
  not constant-time, and fail in FIPS-only mode ✅.
- rustls, rustls-webpki, rcgen: **no** ✅ (rustls lists brainpool only as key exchange codepoints). No Rust crate in
  use or in the local registry has it ✅.

**RSA-PSS with SHA-384 (the EU classical half at 4096 bits, D4):**

- OTP 28.1 with OpenSSL 3.6.4: works ✅ (real sign and verify, randomized). Constant-time behaviour inside OpenSSL
  not verified from source ⚠.
- Go 1.27: `crypto/rsa` signs PSS through `crypto/internal/fips140/rsa`, whose private-key exponentiation uses the
  constant-time `bigmod` package ✅.
- Rust: through aws-lc-rs ⚠ (its source is not on this machine; V4).
- Python `cryptography` and .NET: through OpenSSL ⚠.

**TLS keying-material export:**

- OTP 28.1.1: not applicable (TLS lives in the Rust NIF).
- aioquic (macula-py): none ✅.
- OpenSSL 3.6.4: not checked.
- .NET 10.0.11: none in `System.Net.Quic` ✅.
- Go 1.27: `ConnectionState.ExportKeyingMaterial` ✅.
- rustls and quinn: `export_keying_material` ✅.

### Measured signature costs (this machine)

| Algorithm | OTP | Sign | Verify | Signature | Public key |
|---|---|---|---|---|---|
| ML-DSA-87 | 29 | 1.1 ms | 0.25 ms | 4,627 B | 2,592 B |
| ML-DSA-87 | 28.1 | 1.107 ms | 0.232 ms | 4,627 B | 2,592 B |
| ECDSA brainpoolP384r1 | 28.1 | 0.682 ms | 0.593 ms | 102 B (DER) | 97 B |
| RSA-3072 PSS, SHA-384 | 28.1 | 2.048 ms | 0.051 ms | 384 B | 384 B modulus |
| RSA-4096 PSS, SHA-384 | 28.1 | 4.216 ms | 0.075 ms | 512 B | 512 B modulus |
| SLH-DSA-SHA2-256f | 29 | 44 ms | 1.3 ms | 49,856 B | 64 B |
| SLH-DSA-SHA2-256s | 29 | 453 ms | 0.8 ms | 29,792 B | 64 B |

Notes on the table:

- The OTP 28.1 runs use OpenSSL 3.6.4, one core and a 512-byte message, over 200 to 300 signatures and 300 to 2,000
  verifications.
- RSA key generation, median of 8 runs: 95 ms at 3072 bits (41 to 250 ms) and 445 ms at 4096 bits (69 to 915 ms).
- SWIM sends about one signature per second per station ✅: a signed PING every 2 s by default, plus ACKs.

Ceilings from signing and verifying alone, per core, estimated from the table:

- **Publisher**, one publisher signature per event (D17): about 900 events per second in the US profile. In the EU
  profile about 560 with brainpoolP384r1, 317 with RSA-3072 and 188 with RSA-4096.
- **Station checking publisher signatures:** about 4,300 per second in the US profile. In the EU profile about 1,200
  with brainpoolP384r1, 3,500 with RSA-3072 and 3,260 with RSA-4096.

### Other facts the plan relies on

- An OpenSSL client with default settings **cannot** reach a server offering only pure ML-KEM-1024.
  An `OPENSSL_CONF` `Groups` setting fixes it ✅ (real test).
- OTP 27 has no ML-DSA and no ML-KEM even when linked to OpenSSL 3.6.4 ✅
  (tested at `~/.asdf/installs/erlang/27.2` and `27.3.4.3`).
  - OTP 28 and 29 have both ✅.
  - Every local OTP build links the system OpenSSL dynamically ✅.
- `macula_identity:sign/2` and `verify/3` use OTP `crypto` ✅. The UCAN NIF signs and verifies in Rust today, and
  the DID NIF builds documents without signing ✅ (D7 moves signing into `macula_identity`).
- `macula-station` builds on `erlang:28.1-slim`, `macula-realm` on OTP 28.1.1, and both run on Debian 12 images
  whose OpenSSL is 3.0 ✅ (V2).
- OTP 28.1.1 takes ML-DSA private keys as `{expandedkey, K}` or `{seed, S}`, and **cannot derive the public key
  from a seed** with `generate_key/3` ✅.
- OTP 28.4.2 and 29.0.6: `generate_key(mldsa87, [])` returns the 4,896-byte expanded private key and never the
  seed; `generate_key(mldsa87, [], K)` derives the public key from the expanded key; ML-DSA signing takes no options,
  so no context string ✅.
- quinn needs AES-128-GCM for QUIC Initial packets.
  - `with_initial` supplies it separately, so the negotiated handshake and application keys can be AES-256-only ✅.
  - rustls has `TLS13_AES_256_GCM_SHA384` and a server-order option ✅.
- A rustls server can choose a certificate per client (`ClientHello::signature_schemes()`), and so can a Go server
  (`GetCertificate` with `ClientHelloInfo.SignatureSchemes`) ✅.
- A station's listener presents one certificate, from its configured source ✅.
- DHT records carry the signer's public key in `key` ✅. Macula derives identifiers with SHA-256: the realm id is the
  SHA-256 of the realm name ✅. A DHT storage key is SHA-256 over a type label or tag and record fields, over the
  procedure URI, or over the content id; node records, realm directories, tombstones and domain records without a
  subject use their 32-byte `key` itself ✅ (`macula_record:storage_key/1`, the station's
  `macula_content_dht:dht_key/1`, `macula-rust` `src/dht.rs`). No storage key in `macula`, `macula-station` or
  `macula-rust` uses BLAKE3 ✅. Content identifiers use BLAKE3 by default, in a format that carries an algorithm
  tag ✅. The foundation's realm trust list names realms by their key ✅.
- A push to `macula-station` `main` builds and publishes its image, and watchtower rolls it ✅.
- There is no TLS 1.3 standard for hybrid authentication: "Pour l'instant, il n'existe pas de standards pour
  l'authentification hybride" (ANSSI) ✅.

---

## Stage 0: checks before building

- [x] V1 profile facts from primary sources (source review, spot-checked on this machine).
- [x] V1a BSI TR-02102-1 section 5.3.4 wording on hybrid signatures (re-found at p.54).
- [ ] All remaining V items closed, each with a recorded result.

| ID | Task | Owner | Status | Effort |
|---|---|---|---|---|
| V2 | OpenSSL in the runtime images | Terra | closed | 0.5 day |
| V3 | Lowest OpenSSL for OTP 28 ML-DSA | Terra | closed | 1 day |
| V4 | Rust transport spike | Neptune | passed; three points open | 4 days |
| V5 | Go QUIC spike | Venus | open | 1 day |
| V6 | .NET path | Uranus | closed | 2 days |
| V7 | Python path | Pluto | handshakes proven; shipping open (D10) | 3 days |
| V8 | Hybrid signature building blocks per stack | Saturnus, with stack owners | open; Go passed | 2 days |
| V9 | Handshake size | unassigned | open | 0.5 day |
| V10 | Capture method for the wire checks | Terra | open | 0.5 day |
| V11 | hecate services | Saturnus | open | 1 day |
| V12 | Identifier standards | Mercury | closed (D7) | 0.5 day |
| V13 | QUIC Initial packets | Saturnus | open | 0.5 day |
| V14 | US national-security deployability | Saturnus | open | 0.5 day |
| V15 | ANSSI's recognised classical signatures | Saturnus | closed | 0.5 day |
| V16 | Peer leaf certificate before CONNECT | Neptune, Venus, Uranus, Pluto | open for Python shipping | 0.5 day |
| V17 | Identifier hashes under CNSA 2.0 | Saturnus | answered; D5 pending Raf | 0.5 day |
| V18 | Revocation under BSI and ANSSI | Saturnus | answered; D22 pending Raf | 0.5 day |

#### V2 OpenSSL in the runtime images

- **Result:** the station `:main` image, its builder `erlang:28.1-slim` and the realm builder are Debian 12 with
  libssl3 3.0.17 or 3.0.20. Their OTP `crypto.so` (crypto-5.7) has no ML-DSA and no ML-KEM, while a local OTP 28.1 on
  OpenSSL 3.6.4 has both ✅. `crypto.so` is compiled in the builder image, so the builder changes too (D8).
- **Method caveat:** Docker could not fork on this machine, so the files were inspected from copies instead of
  running `erl` inside the images.
- **Result across the fleet's images** (Terra, 2026-09-10), from the strings in each image's OTP `crypto.so`, copied
  out of the image; the method was checked against local OTP 27, 28 and 29 ✅. The fleet declares 30 image
  references for 29 distinct images, because `macula-station` appears twice under two tags ✅:
  - ML-DSA and ML-KEM present (13): the hecate services agora, biotope, citizens, echo, mail, rag, search, society,
    spartan, stations, tube and turn-credentials, on Alpine 3.22 or 3.23 with OpenSSL 3.5.8 (crypto-5.8.3), and
    hecate-whiteboard on Debian 13.5 with OpenSSL 3.5.7 (crypto-5.7);
  - OTP 27 on OpenSSL 3.5.8, needing only OTP 28 (10): the hecate services archive, grid, llm, news, sentinel and
    warden, hecate-daemon, hecate-stub and macula-dist-relay (crypto-5.5.3.2), and macula-e2e, which moves to
    `erlang:28-alpine`;
  - Debian 12 with OpenSSL 3.0, needing a new base (4): macula-station and macula-realm (crypto-5.7), and
    macula-portal and macula-relay (crypto-5.5.1), which D8 leaves out of scope;
  - not BEAM (2): zot and hanko.
- **Base images** (Terra, 2026-09-10) ✅: `erlang:28-alpine` is Alpine 3.23.5 with OpenSSL 3.5.7 and `erlang:28-slim`
  is Debian 13.6 with OpenSSL 3.5.7, both with `crypto` built with ML-DSA and ML-KEM; `erlang:29-alpine` and
  `erlang:29-slim` have the same OpenSSL; `debian:trixie-slim` has OpenSSL 3.5.6 and `alpine:3.22` OpenSSL 3.5.7.

#### V3 Lowest OpenSSL for OTP 28 ML-DSA

- **What to verify:** the lowest OpenSSL version that gives OTP 28 `crypto` ML-DSA and ML-KEM.
- **Method:** OTP 28.1 in containers against candidate OpenSSL versions.
- **Done when:** the minimum version is recorded.
- **Result** (Terra, 2026-09-10), from the OTP sources at the erlang/otp tags instead of containers: OpenSSL 3.5.0 ✅.
  In OTP 28.1, 28.4.2 and 29.0.6, `lib/crypto/c_src/openssl_config.h` defines `HAVE_ML_KEM` and `HAVE_ML_DSA` only
  when `OPENSSL_VERSION_NUMBER` is 3.5.0 or newer, unless OpenSSL was built without them. The check is made at
  compile time, so the OpenSSL headers OTP is built against decide, not only the runtime library ✅. With OpenSSL
  3.6.4, `crypto:supports()` lists ML-DSA-44, 65 and 87 and ML-KEM-512, 768 and 1024 in OTP 28.1.1, 28.4.2 and
  29.0.6, and the 12 SLH-DSA variants in 28.4.2 and 29.0.6 only ✅.
- **Effort:** 1 day.

#### V4 Rust transport spike

- **Method:**
  - `aws-lc-rs` 1.18 builds for linux-glibc x86_64, linux-musl x86_64 (Alpine CI) and macOS arm64; what `aws-lc-sys`
    needs on `rust:alpine`, where `build-nif.yml` installs only `build-base`;
  - quinn loopback handshake with ML-KEM-1024 and with a custom SecP384r1MLKEM1024 group
    (codepoint 4589 ✅, ML-KEM-1024 plus P-384 from aws-lc-rs);
  - the custom group against Go 1.27 or OpenSSL 3.6.4 as well, not only on loopback, because rustls orders key shares
    per group and a wrong order passes every Rust-only test;
  - ML-DSA-87 certificate from `rcgen`, custom ML-DSA signing key, webpki ML-DSA verification;
  - AES-256-only negotiated suite with `with_initial`;
  - the negotiated group and signature scheme seen two independent ways that agree: recording wrappers (a group
    wrapper, and a per-dial verifier that records the scheme) and a rustls key log that decrypts a capture;
  - RSA-PSS signing through aws-lc-rs, and whether it is constant-time (D4).
- **Done when:** the spike passes in both profiles and fails with today's `ring` configuration; the custom group
  completes a handshake with Go or OpenSSL; both views of the negotiated algorithms agree in both profiles.
- **Note:** contributing a public accessor for the negotiated scheme to quinn or rustls is public and needs Raf's
  explicit yes.
- **Result** (Neptune, 2026-09-10), passed except the open points below:
  - quinn 0.11.11, rustls 0.23.44 and aws-lc-rs 1.18.1 with aws-lc-sys 0.45.0, and no `ring` in the tree ✅;
  - builds for linux-glibc x86_64 in 14 s cold, and for linux-musl x86_64, statically linked, in 28 s ✅;
  - rustls 0.23.44 loads ML-DSA PKCS#8 keys through `with_single_cert` and signs ML-DSA itself, and
    `rustls-webpki` 0.103.15 exports `ML_DSA_87`, so no custom signing key is needed and 0.23.44 is the minimum ✅;
  - the features move from `ring` to aws-lc-rs in `native/macula_quic` and `macula-rust`: rustls `ring` and `tls12`
    become `aws_lc_rs` at 0.23.44 or later, rcgen `ring` becomes `aws_lc_rs` (also in `macula-rust-ffi`), and
    rustls-webpki `ring` becomes `aws-lc-rs` ✅;
  - SecP384r1MLKEM1024 (4589) as a custom `SupportedKxGroup` of about 90 lines, ECDH share first as in Go 1.27,
    completes handshakes from a Rust client to a Go 1.27 quic-go server and from an OpenSSL 3.6.4 client to a Rust
    server, including HelloRetryRequest ✅;
  - pure ML-KEM-1024 with ML-DSA-87 works Rust to Rust, Rust to Go, and OpenSSL to Rust ✅;
  - `TLS_AES_256_GCM_SHA384` is the only traffic suite; AES-128 reaches quinn only through `with_initial`, for
    Initial packets ✅;
  - resumption is off (`Resumption::disabled()`, `send_tls13_tickets = 0`, `NoServerSessionStorage`), and so is
    early data (`enable_early_data = false`, `max_early_data_size = 0`) ✅;
  - quinn exposes neither the negotiated group nor the suite ✅. rustls key exchange groups are `&'static`, so a
    group recorder counts per process and group, and names a connection's group only when that connection offers
    one group. The verifier recorder is per dial and records the server name, the SHA-384 of the leaf DER as
    received, and the TLS 1.3 scheme ✅;
  - the second, independent view was OpenSSL `-trace` decoding the key share of the received ServerHello and
    HelloRetryRequest and "Peer signature type: mldsa87", which Jupiter accepted for V4; the capture method stays
    with V10;
  - the spike stays in Neptune's scratchpad and is not merged.
- **Open points** (Neptune):
  - RSA-PSS-4096 signing through aws-lc-rs: a real signing run, and a read of the AWS-LC RSA source for blinding
    and constant-time behaviour (D4, V8) ⚠;
  - the spike built with the `ring` features, to show it fails ⚠;
  - a macOS arm64 build ⚠.
- **Effort:** 4 days.

#### V5 Go QUIC spike

- **Method:** quic-go 0.62.0 on Go 1.27 with ML-KEM-1024 and SecP384r1MLKEM1024, and an ML-DSA-87 certificate.
  Assert `ConnectionState().TLS.CurveID` and the certificate signature algorithm.
- **Done when:** the spike test passes in both profiles.
- **Effort:** 1 day.

#### V6 .NET path

- **Result:** no msquic build today can offer ML-KEM ✅. The details are under .NET above. D10 leaves .NET out of
  the first switch.
- **Still to run, when .NET comes back in scope:** whether OpenSSL 3.5 accepts an ML-DSA-87 leaf through msquic, and
  the Windows Schannel result.

#### V7 Python path

- **Result:** a client-side patch to aioquic's `tls.py` completes both profiles' handshakes against a Go server ✅.
  The details are under aioquic above.
- **Not yet covered:** handshake size and round trips (V9), a Rust server, and a tampered CertificateVerify inside the
  handshake.
- **Packaging facts for D10** (the PyPI rule not re-checked ⚠):
  - PyPI refuses packages whose dependencies are git URLs, so a fork cannot be a git dependency of macula-py;
  - a fork that keeps the `aioquic` import name collides with upstream aioquic in the same environment;
  - aioquic ships C extensions in platform-specific wheels, so vendoring makes macula-py per-platform;
  - aioquic is BSD-3-Clause, so vendoring into Apache-2.0 macula-py is allowed if its notice is kept;
  - the `cryptography>=43.0.0` floor in `pyproject.toml` must rise to a version with ML-KEM and ML-DSA.

#### V8 Hybrid signature building blocks per stack (D4)

- **Method:**
  - Test vector: ML-DSA-87 concatenated with the classical half over the same bytes, produced in `macula` and
    verified in Go, Rust, Python and later .NET.
  - For each stack, the classical signer comes from the stack's usual crypto library, with no extra C dependency.
    It is constant-time, blinded, and protected against faults in signing and verification.
  - ML-DSA signing is the hedged, pure variant in every stack.
  - The Go implementation cross-compiles into all five macula-ts prebuilt binaries without extra C dependencies.
- **Results so far:**
  - OTP 28.1 with OpenSSL 3.6.4: ML-DSA-87 signing is randomized ✅; RSA-PSS with SHA-384 works ✅.
  - Go: `crypto/mldsa` interoperates with OTP ML-DSA-87 ✅; the brainpoolP384r1 libraries interoperate but are not
    constant-time ✅; `crypto/rsa` signs on constant-time big-number code ✅.
  - Go with the D7 composite `ML-DSA-87-PS384` (Venus, 2026-09-10): M' is byte-identical in OTP and Go, each side's
    composite verifies on the other, an altered half and a wrong label are refused, and the standard library builds
    for all five prebuilt targets with cgo off ✅. The RSA signer is constant-time and verifies after signing, with
    no blinding ✅; whether that meets D4's signer condition is open ⚠.
  - Erlang: `macula_node_keys` verifies both the OTP and the Go composite vectors, kept as test fixtures on the
    `post-quantum` branch ✅.
  - Rust: no brainpoolP384r1 ✅; RSA-PSS through aws-lc-rs ⚠ (V4).
- **Done when:** the vector verifies in every stack, with a named implementation that meets the conditions above.
- **Effort:** 2 days.

#### V9 Handshake size

- **Method:** the ML-DSA-87 certificate (2,592 B key, 4,627 B signature) plus CertificateVerify, against QUIC
  anti-amplification; then the challenge (about 12 KB) and CONNECT (about 19 KB in the US profile), each with its
  status statement (D22), against the congestion window; round trips per profile, with netem delay and a 1,200-byte
  path MTU.
- **Done when:** round trips measured per profile.
- **Effort:** 0.5 day.

#### V10 Capture method for the wire checks

- **Method:** `tshark` is not installed, `tcpdump` is ✅. Decrypting a capture with a TLS key log needs a tool that
  reads the key log. Choose between installing tshark (sudo script for Raf) and decoding in the test harness.
- **Done when:** the method is chosen and proven on one handshake.
- **Effort:** 0.5 day.

#### V11 hecate services

- **Result so far:** 32 repositories build from a `Containerfile`; 12 use `erlang:27-alpine`, and OTP 27 has no
  ML-DSA or ML-KEM ✅; 7 run CI on erlang:27; runtime images are Alpine 3.20, 3.22 and 3.23, plus Debian trixie. V2
  covers the fleet's images, and V3 gives the OpenSSL floor.
- **Also to list:** every Ed25519 guard outside macula (known: `hecate_om_identity`, `hecate_om_ownership_proof` ✅),
  and data keyed by the hex node id (hecate-citizens, hecate-mail, hecate-graph).
- **Done when:** the list is recorded.
- **Effort:** 1 day.

#### V12 Identifier standards

- **Method:**
  - ML-DSA algorithm names for UCAN and JOSE;
  - the `did:key` multicodec for ML-DSA;
  - whether the UCAN spec requires `aud` to be a DID (Macula membership UCANs use a raw hex key there ⚠).
- **Context:** a minimal post-quantum UCAN is about 16 KB, because `did:key` carries the full key in `iss` and `aud`.
- **Done when:** a written result with sources.
- **Result** (Mercury, 2026-09-10): in D7's two checks, with sources: the RFC 9964 names, the `mldsa-87-pub`
  multicodec, no composite JOSE, COSE or multicodec name for ML-DSA with RSA, and UCAN 0.10.0's `aud` rules.
- **Effort:** 0.5 day.

#### V13 QUIC Initial packets

- **Method:** confirm that AES-128-GCM protection of QUIC Initial packets (keys derived from the public connection
  ID ⚠) is acceptable under CNSA 2.0 while all negotiated keys are AES-256.
- **Done when:** a written result with source.
- **Effort:** 0.5 day.

#### V14 US national-security deployability

- **Method:** product validation needed on top of algorithms (NIAP or NSA validation per the source review ⚠), and
  what Macula may truthfully claim without it.
- **Done when:** the offer wording is agreed.
- **Effort:** 0.5 day.

#### V15 ANSSI's recognised classical signatures

- **Result:** in ANSSI PG-083 v3.00, RSA-SSA-PSS conforms with 3072 bits from 2031; ECDSA conforms on FRP256v1 and
  the FIPS 186-5 curves; brainpoolP256r1, brainpoolP384r1 and brainpoolP512r1 are conforming curves; the lists are
  non-exhaustive; ML-DSA alone does not conform ✅. The quotes are in the root under EU signature constraints.

#### V16 Peer leaf certificate before CONNECT

- **What to verify:** every client stack can read the station's leaf certificate DER as received, after the TLS
  handshake and before sending CONNECT (D16).
- **Result so far:**
  - Erlang: through quinn `peer_identity`, with a NIF export to build (WP 1.2) ✅;
  - macula-rust: `connection.peer_identity()` ✅;
  - Go: `PeerCertificates[0].Raw` ✅;
  - .NET: `QuicConnection.RemoteCertificate`, re-serialized DER that equals the received bytes for strict DER ✅;
  - Python: needs the aioquic patch, which V7 proved ✅.
- **Done when:** access confirmed in every client stack in the first switch, with the shared strict DER leaf vector
  passing.
- **Effort:** 0.5 day.

#### V17 Identifier hashes under CNSA 2.0

- **Result:** CNSA 2.0 defines its SHA row by function, "Algorithm for computing a condensed representation of
  information", with SHA-384 or SHA-512 for all classification levels ✅. Truncated or other hashes are acceptable
  only inside NSA-approved designs ✅. FIPS 180-4 section 7 and SP 800-107r1 allow leftmost truncation when it is
  unambiguous to all parties ✅. BSI Table 4.1 lists SHA-256, SHA-512/256, SHA-384, SHA-512 and SHA3-256, 384 and
  512 ✅; ANSSI and the ECCG recommend 384-bit hashes where post-quantum security is the goal ✅. BLAKE3 appears in
  none of these texts ✅. Decision: D5 and D11, pending Raf.

#### V18 Revocation under BSI and ANSSI

- **Result:** BSI TR-02102-1 p.28 requires that withheld certificate status does not go unnoticed, and separately a
  limited validity ✅. ANSSI's TLS recommendations R37 require hard-fail for security-first components ✅. Stapled
  status is preferred by ANSSI R35 and recommended by BSI TR-03116-4 ✅. No source treats short validity as a
  replacement for revocation ✅. Decision: D22, decided by Raf on 2026-09-10.
