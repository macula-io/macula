# Exploration: post-quantum feasibility for Macula

**This exists so Raf can say honestly, in a consortium partner offer, what post-quantum protection Macula has or
will have, and what it costs to get there.**

**Status:** Investigation only, no code changed. Classification: BUILD.
**Date:** 2026-09-10
**Legend:**

- ✅ checked against source or binaries on this machine, or measured here.
- ⚠ not verified, from memory only. Do not put a ⚠ item in an offer without checking it first.

---

## 1. Short answer

- **Today Macula has no post-quantum protection on any link.** ✅
- **Half of the fleet is already ready for it without knowing.** ✅
  - The Go SDK, and so also macula-ts and macula-php, already *offers* hybrid X25519MLKEM768 key exchange by
    default.
  - The stations do not accept it, so every link falls back to classical X25519.
- **Turning it on in the stations is a small change.**
  - The Rust transport uses the `ring` crypto provider in rustls, which has no post-quantum groups.
  - The `aws-lc-rs` provider in the same rustls version has them. ✅
  - The cost is a C build dependency, not new protocol work.
- **Gradual rollout works.** No flag day. Peers that know the hybrid use it, others fall back to X25519 in the
  same handshake. ✅
- **Signatures are a much bigger job** (Ed25519 is built into node identity, DHT records, frames, UCANs and DIDs).
  For an offer, post-quantum key exchange plus a documented signature migration plan is a defensible position.
  See section 4.

## 2. Transport: the harvest-now-decrypt-later risk (question 1)

Traffic recorded today could be decrypted later by a quantum computer. That risk is about **key exchange**, so it
is the part to fix first.

**What rustls offers (rustls 0.23.43, in the local cargo registry)** ✅

| Provider | Key exchange groups | Post-quantum |
|---|---|---|
| `ring` (Macula uses this today) | X25519, P-256, P-384 | none |
| `aws-lc-rs` with `prefer-post-quantum` | **X25519MLKEM768** first, then X25519, P-256, P-384 | yes |

- `X25519MLKEM768` is defined only in `src/crypto/aws_lc_rs/pq/`.
- Both `prefer-post-quantum` and `aws_lc_rs` are rustls **default** features.
- Macula turns defaults off and picks `ring` in `native/macula_quic/Cargo.toml` and in `macula-rust/Cargo.toml`.

**Fallback** ✅

- When rustls offers the hybrid, it also sends the plain X25519 part as a second key share "for free"
  (`src/client/hs.rs`, around line 280).
- Go's `crypto/tls` does the same ("the PQ hybrids sort first, and produce a fallback").
- A classical-only peer just answers with X25519. No extra round trip, no Macula protocol change.

**The trust model is not touched.**

- Macula pins peers by their Ed25519 public key in a self-signed certificate.
- Key exchange sits beside that, so pinning keeps working.
- ⚠ Still needs a test that pinned Ed25519 certificates verify under the `aws-lc-rs` provider.

**Cost of switching the Rust transport to aws-lc-rs**

- `aws-lc-rs` and `aws-lc-sys` are **not in the cargo registry** on this machine, so nothing here builds them yet. ✅
- The NIF is built from source by every consumer (`priv/build-nifs.sh` runs `cargo build`), with a precompiled
  download only for Linux x86_64 glibc and musl. ✅ So every developer machine would compile a C library.
- CI musl build runs in `rust:alpine` with only `apk add build-base`. ✅
  Whether `aws-lc-sys` also needs `cmake` there, and how long it adds to the build, is ⚠ not verified.
- Handshake gets bigger:
  - ML-KEM-768 adds 1184 bytes to the ClientHello and 1088 to the ServerHello. ✅ (sizes measured)
  - The ClientHello will span two QUIC Initial packets. ⚠ Expected to be handled by QUIC itself, not tested.
- CPU cost is negligible: one X25519 exchange took 0.118 ms here, one full ML-KEM-768 exchange 0.153 ms. ✅
  (measured, Go 1.27, amd64)

## 3. The other stacks (question 2)

For each stack: the QUIC/TLS library as used, hybrid key exchange today, and what it takes to enable it.

### macula (Erlang NIF, **stations**)

- **QUIC/TLS library, as used:** quinn 0.11, rustls 0.23 `ring`
- **Hybrid key exchange today:** ❌ ✅
- **To enable:** switch provider (section 2)

### macula-rust

- **QUIC/TLS library, as used:** quinn 0.11, rustls 0.23 `ring`
- **Hybrid key exchange today:** ❌ ✅
- **To enable:** same change

### macula-go

- **QUIC/TLS library, as used:** quic-go 0.62.0 on stdlib `crypto/tls`, Go 1.26 (`go.mod`, CI)
- **Hybrid key exchange today:** ✅ **offered by default** ✅
- **To enable:** nothing; already on

### macula-ts, macula-php

- **QUIC/TLS library, as used:** FFI over the macula-go C library
- **Hybrid key exchange today:** inherits Go ✅
- **To enable:** nothing

### macula-dotnet

- **QUIC/TLS library, as used:** `Unofficial.MsQuic` 2.4.10, which is **OpenSSL 1.1.1w+quic**
- **Hybrid key exchange today:** ❌ ✅ (OpenSSL 1.1.1 has no ML-KEM)
- **To enable:** move to an msquic built on OpenSSL 3.5 or newer ⚠

### macula-py

- **QUIC/TLS library, as used:** aioquic 1.3.0: X25519, X448, P-256, P-384 only
- **Hybrid key exchange today:** ❌ ✅
- **To enable:** add the group to aioquic; `cryptography` 50.0.1 already has ML-KEM ✅

Notes:

- quic-go 0.62.0 calls `tls.QUICClient`/`tls.QUICServer` from the standard library and does not override curve
  preferences; macula-go does not either, and has no `godebug` setting that would switch ML-KEM off. ✅
- On .NET, the host system has OpenSSL 3.6.4 with X25519MLKEM768 ✅, but the msquic package does not use it.
- Windows (Schannel) is ⚠ not checked.

**Gradual adoption: yes.** Upgrade stacks one by one in any order. Each new pair of upgraded peers gets the
hybrid, everything else stays classical.

## 4. Signatures and identity (question 3)

Ed25519 is built in deeply. ✅

| Place | What is fixed today |
|---|---|
| Node identity | `macula_identity:node_id/1` says "Phase 1: NodeId == public key", guarded to 32 bytes |
| Sybil puzzle | leading zero bits of SHA-256(public key), guarded to 32 bytes; keys are ground in the NIF |
| DHT records | `key` 256 bits, `signature` 512 bits; `macula_record.erl` checks 64-byte signature and 32-byte key |
| Frames (SWIM updates) | `signature => <<_:512>>`; `macula_frame.erl` checks 64 bytes |
| UCANs | `alg: "EdDSA"` hard-coded in `macula_ucan_nif` |
| DIDs | Ed25519 multicodec `0xed01`, `Ed25519VerificationKey2020` in `macula_did_nif` |
| Rust crates | `ed25519-dalek` 3.0 in the crypto, DID and UCAN NIFs |

**Correction to the starting brief:** the realm X.509 chain is **not** Ed25519. ✅

- Realm CA is RSA (10 years).
- Org CA is RSA-2048 (5 years).
- Freshly minted app certificates are ECDSA P-256.
- Only caller-supplied certificates carry Ed25519.

**Size and speed** (Go 1.27 source and measured on this machine) ✅

| | public key | signature | keygen | sign | verify |
|---|---|---|---|---|---|
| Ed25519 | 32 B | 64 B | 0.017 ms | 0.020 ms | 0.045 ms |
| ML-DSA-65 | 1952 B | 3309 B | 0.248 ms | 0.556 ms | 0.167 ms |

A signature is about **52 times bigger**. Every signed DHT record and every signed SWIM update grows by about
3.2 KB. Grinding the puzzle at difficulty 8 (about 256 keys) goes from about 4 ms to about 64 ms.

**Library support exists everywhere.**

- OTP 28 and 29 `crypto` have ML-DSA-44/65/87 and ML-KEM; OTP 27 has neither. ✅
- The realm runs OTP 28.1.1, whose `public_key` ships an ML-DSA X.509 ASN.1 module. ✅
  (End-to-end ML-DSA certificate signing ⚠ not tested.)
- Go 1.27 has `crypto/mldsa`; Go 1.26 does not. ✅
- .NET 10 has ML-DSA and ML-KEM types, and `BouncyCastle.Cryptography` 2.7.0, already a macula-dotnet dependency,
  has them too. ✅
- Macula signs DHT records and frames through OTP `crypto` (`macula_identity:sign/2` and `verify/3`), not a Rust
  NIF; the Rust NIFs cover UCANs, DIDs and puzzle grinding.
  - So every BEAM service that uses macula needs an OTP build whose `crypto` supports ML-DSA.
  - `macula-station` builds on OTP 28.1 and the realm on OTP 28.1.1; only `macula`'s own Dockerfiles use
    `erlang:27`. ✅

**Is key exchange plus a signature plan enough for an offer?** Yes, with reasons.

- Recorded traffic is the threat that exists *today*, and hybrid key exchange closes it.
- A signature can only be forged by someone who has a quantum computer *at the moment it is checked*, so
  signatures can follow on a published plan.
- The exception is long-lived trust anchors: the 10-year RSA Realm CA should be the first signature to move.

A likely migration shape, for the plan document, not decided here:

- keep `node_id` at 32 bytes by making it a hash of a hybrid Ed25519 + ML-DSA-65 key rather than the key itself;
- make record and frame signatures variable-length with an algorithm tag;
- version UCAN `alg` and the DID key type.

## 5. Effort and order (question 4)

Rough estimates, for planning only.

| # | Step | Effort |
|---|---|---|
| 1 | Rust transport | 2 to 4 days |
| 2 | Prove it on the wire | 1 day |
| 3 | .NET | 3 to 5 days ⚠ |
| 4 | Python | about 1 week |
| 5 | Signature migration plan document | 2 to 3 days |
| 6 | Signature migration implementation | several weeks, across six codebases |

### Step 1: Rust transport

- **Step:** rustls `aws-lc-rs` + `prefer-post-quantum` in `macula_quic` and `macula-rust`; fix CI and developer
  build; test pinned certs.
- **Effort:** 2 to 4 days.
- **Why this order:** unlocks hybrid on every link to a station, and Go clients get it immediately.

### Step 2: Prove it on the wire

- **Step:** capture handshakes showing X25519MLKEM768 for each stack pair.
- **Effort:** 1 day.
- **Why this order:** nothing goes in an offer without this.

### Step 3: .NET

- **Step:** replace `Unofficial.MsQuic` (OpenSSL 1.1.1) with an OpenSSL 3.5+ based msquic; check Windows.
- **Effort:** 3 to 5 days ⚠.
- **Why this order:** path not verified.

### Step 4: Python

- **Step:** X25519MLKEM768 in aioquic, upstream or local patch.
- **Effort:** about 1 week.
- **Why this order:** library change outside Macula.

### Step 5: Signature migration plan document

- **Step:** covering identity, puzzle, records, frames, UCAN, DID, X.509.
- **Effort:** 2 to 3 days.
- **Why this order:** enough for the offer.

### Step 6: Signature migration implementation

- **Step:** starting with the Realm CA.
- **Effort:** several weeks, across six codebases.
- **Why this order:** after the plan.

**For the offer:** steps 1 and 2 turn "no post-quantum protection" into "hybrid post-quantum key exchange on the
core mesh", in about one week.

## 6. Not verified (check before relying on it)

- What `aws-lc-sys` needs to build on musl, macOS and ARM, and the build time it adds.
- Whether an OpenSSL 3.x msquic negotiates X25519MLKEM768 by default; Windows Schannel.
- ClientHello spanning two QUIC Initial packets and the anti-amplification limit, in practice.
- Standard algorithm identifiers for ML-DSA in UCAN, JOSE/COSE and `did:key`.
- End-to-end ML-DSA X.509 signing in OTP `public_key`.
