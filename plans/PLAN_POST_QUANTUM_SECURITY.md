# Plan: post-quantum security for Macula

**This exists so the team can deliver post-quantum protection for Macula in a known order, with every step
testable, and Raf can point to a concrete plan in a partner offer.**

**Status:** Planning. Raf accepted the recommendations on 2026-09-10. Open items are marked with their owner.
**Created:** 2026-09-10
**Last Updated:** 2026-09-10
**Classification:** BUILD
**Reasons and measurements:** [EXPLORATION_POST_QUANTUM.md](EXPLORATION_POST_QUANTUM.md)

**This plan is split into four documents:**

- this root: overview, crypto profiles, key model and connection design, stages, decisions, open items, effort
  and success criteria;
- [PLAN_POST_QUANTUM_SECURITY_DECISIONS.md](PLAN_POST_QUANTUM_SECURITY_DECISIONS.md): the full text of each decision;
- [PLAN_POST_QUANTUM_SECURITY_PART1.md](PLAN_POST_QUANTUM_SECURITY_PART1.md): facts already verified, and the
  Stage 0 checks;
- [PLAN_POST_QUANTUM_SECURITY_PART2.md](PLAN_POST_QUANTUM_SECURITY_PART2.md): the work packages of Stages 1 to 7,
  and the files per repository.

**Profile sources:**

- NSA CNSA 2.0 advisory and FAQ
- NIST IR 8547 (initial public draft), FIPS 180-4, FIPS 204 and SP 800-107r1
- NIS Cooperation Group PQC roadmap and FAQ
- EU joint statement of 18 member states
- BSI TR-02102-1, TR-02102-2 and TR-03116-4
- ANSSI position papers, TLS 1.3 note, TLS recommendations v1.2, PA-079 v1.0 and PG-083 v3.00
- ECCG Agreed Cryptographic Mechanisms v2.0
- NL PQC Migration Handbook

**Legend:**

- ✅ verified on this machine (source code read, binary inspected, real run, or quote re-found in the
  primary-source text).
- ⚠ not verified here.
- **Waiting on:** a work package starts only when that item is closed.
- **Owners** are team sessions:
  - Mercury: identity core, profile model, realm, this plan;
  - Neptune: Rust, the NIF, and the client dial options;
  - Mars: station, peering handshake, `macula-cli`, `lazymesh`;
  - Terra: test suites, the post-quantum fleet, the station directory;
  - Venus: Go, TypeScript, PHP, `macula-mcp`;
  - Uranus: .NET;
  - Pluto: Python;
  - Saturnus: crypto profiles, compliance, hecate services;
  - Fable and Jupiter: quality and security gates.

---

## Overview

Macula becomes post-quantum on every link and every identity surface.
**There is no classical-only mode anywhere.**

### Two profiles

| Profile | Key exchange | Signatures |
|---|---|---|
| **US national security (CNSA 2.0)** | pure post-quantum | pure post-quantum |
| **EU** | hybrid: post-quantum plus classical | hybrid for every signature Macula makes (D4) |

Who each profile applies to:

- **US national security (CNSA 2.0):** US National Security Systems only.
  Other US federal systems follow NIST, which allows hybrids ⚠.
- **EU:** deployments following BSI, ANSSI and NIS Cooperation Group guidance.

**Why two profiles and not one.** The sources pull in opposite directions:

- NSA "will not require NSS developers to use hybrid" ✅, and says "Do not use a hybrid or other
  non-standardized QR solution on NSS mission systems except" for exceptions NSA recommends ✅.
- The EU joint statement says "strongly recommend to deploy PQC in hybrid" ✅.
- The NIS Cooperation Group recommends "standardised and tested hybrid solutions" ✅.
- BSI says the quantum-safe mechanisms "should be used in 'hybrid' form, i.e., in a suitable combination with a
  classical method" ✅.

No single configuration satisfies both.

### Requirements that apply to both profiles

- **Strength level 5:** ML-KEM-1024, ML-DSA-87, AES-256.
  - CNSA 2.0 requires ML-KEM-1024 "for all classification levels" ✅, ML-DSA-87 ✅ and AES-256 ✅.
  - The same level also meets the German and French guidance ⚠ (per the source review).
- **No X25519MLKEM768.**
  - It appears in none of the 12 documents of the first source review ✅.
  - BSI's TLS guidance names SecP256r1MLKEM768 and SecP384r1MLKEM1024 ✅.
- **Hash:** SHA-384 or SHA-512 ✅ (CNSA 2.0) in bindings, proofs and status statements, and SHA-384 for content
  ids and UCAN parent ids (D24). Identifier hashes: D5.
- **One purpose per key.** Identity, TLS and CONNECT keys are separate, and so are realm, org and foundation keys
  (key model below).
- **One profile per station instance** (D2).

### EU signature constraints, from the documents

BSI TR-02102-1:

- **Hybrid signature construction** ✅ (section 5.3.4, p.54):
  - "This Technical Guideline recommends the use of a quantum-safe signature scheme only in combination with a
    classic signature scheme."
  - The natural construction is concatenation, "so that the concatenated signature is accepted as valid if all
    individual signatures are valid".
  - It asks for dedicated key material: "Care should be taken here to generate key material for hybrid signatures
    specifically for this purpose and not to use it for non-hybrid signatures as well."
- **Hash-based signatures** can, "provided that the implementation security of stateful and stateless
  hash-based mechanisms is carefully considered, in principle also be used alone" ✅ (p.54).
- **Recommended classical signature algorithms** (Table 5.3) ✅:
  - RSA;
  - DSA, "only recommended until 2029" (footnote 1);
  - DSA variants on elliptic curves: ECDSA, ECKDSA/ECKCDSA, ECGDSA.
- All recommended mechanisms "can be used for signing data as well as for issuing certificates" ✅.
- **No EdDSA or Ed25519** appears in TR-02102-1 or TR-02102-2 ✅.
- **Recommended elliptic curves** (Table B.3) ✅:
  - brainpoolP256r1;
  - brainpoolP320r1;
  - brainpoolP384r1;
  - brainpoolP512r1.
- TR-02102-1 names no NIST P-curves ✅; TR-02102-2 names them for TLS 1.3 ✅.
- **RSA modulus** "should be at least 3000 bits" ✅ (section 5.3.1, p.52).
- **Instance authentication:** "a key used to generate signatures is not used for instance authentication" ✅
  (section 6.2, Remark 6.1, p.58). The remark goes on: "This must also be indicated in the corresponding
  certificates for the public keys." ✅
- **Certificates** ✅ (p.28):
  - "There should be possibilities for the deactivation of certificates in a timely manner and it should not be
    possible for an attacker to prevent a verifying party from having the information about the current status
    of a certificate available at the time of verification without being noticed."
  - "Certificates should only be issued with a limited validity period."

BSI TR-03116-4: "Für verschiedene Anwendungszwecke (Signatur, Verschlüsselung, Authentisierung, usw.) sollten
nach Möglichkeit verschiedene Schlüsselpaare generiert" werden ✅ (p.25).

ANSSI PG-083 v3.00 (2026-03-20):

- **ML-DSA alone:** "Utilisé sans hybridation et quel que soit le jeu de paramètres utilisé, le mécanisme de
  signature numérique ML-DSA ne respecte pas la règle RègleSécuAsym." ✅ (p.37)
- **Hybrid by concatenation:** "On considère la signature hybride valide si, et seulement si, la signature
  classique et la signature post-quantique sont toutes les deux valides." ✅ (p.26)
- **Classical signatures** ✅:
  - RSA-SSA-PSS conforms under the factorisation rules (p.36);
  - ECDSA and ECKCDSA conform on FRP256v1 and on the FIPS 186-5 curves P-256, P-384, P-521, B-283, B-409 and
    B-571 (p.36);
  - brainpoolP256r1, brainpoolP384r1 and brainpoolP512r1 are conforming curves (p.31);
  - the lists are illustrations, not exhaustive (section 1.5).
- **RSA modulus:** at least 2048 bits until the end of 2030, at least 3072 bits from 2031, and 3072 bits
  recommended now ✅ (p.27).
- **EdDSA and Ed25519:** not listed ✅.
- **The overall mechanism must be evaluated with care**, even when every primitive conforms ✅ (section 2.2.4,
  p.38, an information box rather than a rule).

ANSSI PA-079 v1.0 (2021): "une même clé ne doit jamais être utilisée par un mécanisme de signature et un mécanisme
d'authentification d'entité" ✅ (section 6.3).

ANSSI TLS recommendations v1.2 (2020):

- "Les composants logiciels TLS privilégiant la sécurité doivent réagir en hard-fail." ✅ (R37)
- Stapled revocation status is preferred ✅ (R35).

ANSSI (2023 follow-up):

- **Hybrid signature construction:** a concatenation is valid "if and only if all of them are valid" ✅.
- **Hybridation in an upper layer:** for platform products that give raw cryptography "to an upper (applicative)
  layer", plain post-quantum "can sometimes be relevant as hybridation will be part of upper user-oriented
  layers", with evaluation requirements attached ✅.

ECCG Agreed Cryptographic Mechanisms v2.0 (April 2025):

- ML-DSA "shouldn't be used in a standalone way" but "should be combined with a classicaly secure cryptographic
  mechanism" ✅ (Note 51).
- Concatenation, "the verification function accepting if and only if all signatures are correct" ✅ (Note 51).
- "A key must not be used with different mechanisms"; message signatures and authentication use different key
  pairs ✅ (Note 79).
- Agreed: RSA PSS with a modulus of at least 3000 bits; Brainpool P256r1, P384r1 and P512r1; NIST P-256, P-384
  and P-521 ✅.

Not verified here:

- whether BSI or ANSSI would assess Macula's design as hybrid ⚠. No agency has assessed it.

### Scope and delivery

**Scope.** Every stack:

- the Rust QUIC NIF in `macula` (which stations run);
- `macula-rust`, `macula-go`, `macula-dotnet`, `macula-py`;
- `macula-ts` and `macula-php` through `macula-go`.

Every identity surface: node_id and the Sybil puzzle, DHT records, frames, SWIM frames, UCANs, DIDs, the realm
certificate chain, ownership proofs, and the station directory. Stations are in scope as transport endpoints and
as identities.

**Not in scope:**

- no transport or handshake changes on the live fleet; the connection handshake of the post-quantum fleet (D16)
  replaces its dial behaviour at cutover;
- changes to the live fleet otherwise stay on the `macula` 10.x line on `main`;
- `reckon_gater` capability signing (D9).

**Delivery model.**

- Everything is a development environment.
- A second, post-quantum fleet runs next to the live fleet (D14). The two fleets cannot reach each other.
- Each consumer moves to the new fleet in its stage. The live fleet is switched off after the last cutover.
- `macula` develops on the git branch `post-quantum`, and `macula-station` and the new fleet build against that
  git ref. `macula` 11.0.0 goes to hex once proven, and only Raf publishes (D20).
- No compatibility with Ed25519, RSA or ECDSA-only keys on the new fleet.
- The US profile goes first in every stage, and the EU profile follows right after (D15).

**Nothing is claimed in public for a stack before its wire checks are green** (Stage 7, D11).

**Offer context.**

- CNSA 2.0 applies to new NSS acquisitions from 1 January 2027 ✅ ("by January 1, 2027, all new acquisitions
  for NSS will be required to be CNSA 2.0 compliant unless otherwise noted").
- The Digital Europe call closes on 14 January 2027.
- This is context for the offer, not a delivery date.

---

## Crypto profiles

| | `pq_pure` (CNSA 2.0 algorithm list) | `pq_hybrid` (EU) |
|---|---|---|
| Key exchange group | ML-KEM-1024 ✅ | SecP384r1MLKEM1024 (route: D3) |
| TLS certificate and handshake signature | ML-DSA-87 on the TLS key | ML-DSA-87 alone on the TLS key (D4) |
| Identity, CONNECT proof and status signatures | ML-DSA-87 | ML-DSA-87 plus RSA-PSS-4096 (D4) |
| Negotiated TLS cipher suite | TLS_AES_256_GCM_SHA384 (V13) | TLS_AES_256_GCM_SHA384 (V13) |
| Hash in bindings, proofs and statements | SHA-384 | SHA-384 |
| Content ids and UCAN parent ids | SHA-384 only (D24) | SHA-384 only (D24) |
| Classical-only fallback | none | none |

Notes on the table:

- The EU hybrid signature is a concatenation of ML-DSA-87 and RSA-PSS-4096, valid only if both verify, with key
  material dedicated to hybrid use (D4).
- Identity signatures cover records, origin signatures on relayed frames, publisher signatures, UCANs, DIDs,
  bindings, realm credentials and ownership proofs.
- The cipher suite row covers negotiated keys; QUIC Initial packets are V13.

---

## Key model and connection design

This is the design every stage builds on the post-quantum fleet. Decisions: D2, D6, D12, D13, D16, D17, D22.
The frames, bindings and status statements are laid out byte for byte in `DESIGN_PQ_HANDSHAKE_FRAMES.md`.

### Keys per node

| Role | US profile | EU profile |
|---|---|---|
| Station instance | 3 keys: identity, TLS, CONNECT | 5 private keys: identity pair, TLS key, CONNECT pair |
| Client-only node | 2 keys: identity, CONNECT | 4 private keys: identity pair, CONNECT pair |

- US keys are ML-DSA-87. An EU pair is ML-DSA-87 plus RSA-PSS-4096 (D4), used only together.
- The TLS key is ML-DSA-87 alone in both profiles.
- A box that serves both profiles runs one station instance per profile (D2), so it holds 8 private keys.

### What each key signs

- **Identity key:**
  - records;
  - publisher signatures;
  - origin signatures on relayed frames: CALL and STREAM_OPEN over the caller; RESULT, ERROR and STREAM_REPLY
    over the replier; stream frames that carry a signer;
  - UCANs and DIDs;
  - ownership and device proofs;
  - the binding of its TLS key, the binding of its CONNECT key, and their status statements (D22).

  It never signs anything fresh or unique to a session.
- **TLS key:** only the TLS 1.3 handshake, on station instances, with a self-signed certificate.
- **CONNECT key:** only the per-connection CONNECT proof. A key used for signatures is never also used for entity
  authentication (ANSSI PA-079 section 6.3, BSI Remark 6.1, ECCG Note 79) ✅.
- **Realm, org and foundation keys:** a realm key signs its realm's records, an org key signs its procedure
  delegations, and a foundation key signs foundation records. Each is a purpose of its own, which extends each key
  serving exactly one purpose (D6) beyond a node's own keys. Signing code refuses a key whose purpose does not fit
  the record type; that check is local, since no verifier can see a key's purpose.
- **node_id** is derived from the identity public key or keys (D5). The TLS and CONNECT keys are not part of it.

### Bindings

The identity key certifies the TLS key and the CONNECT key, each with a static binding that carries:

- a context label, distinct for each binding type and from every frame and proof label;
- the node_id;
- the SHA-384 of the certified subject: the leaf certificate DER as presented for the TLS key, the carried key for
  the CONNECT key;
- the permitted use: "TLS handshake only" or "CONNECT proof only";
- a binding id, not-before and not-after;
- the hash and signature algorithm tags.

Nothing in a binding is unique to a session. A binding is valid for 7 days, and the node rotates to a new key and
binding every 5 days (D22). A station instance's listener reloads its certificate on rotation. A station makes a new
leaf only when it rotates its TLS key, and issues the new binding and its status statement before its listener
presents that leaf.
The chain has at most three levels: realm, identity, then the TLS or CONNECT key.

A binding is revoked through status statements stapled into the handshake and re-presented on open
connections (D22).

### Connection handshake

1. **TLS.** A full TLS 1.3 handshake. The station presents its self-signed ML-DSA-87 certificate. The client
   presents no certificate.
2. **Opener.** The client opens the control stream and sends an unsigned opener in its first flight after the TLS
   handshake. The opener carries nothing that relates to identity.
3. **Challenge.** The station sends a nonce, its identity public key or keys, and its TLS-key binding.
4. **Client checks, before signing anything:**
   - the binding's signature, made by the station's identity key;
   - that the binding's subject hash equals the SHA-384 of the leaf certificate DER it verified;
   - the binding's validity and permitted use;
   - that the node_id derived from the identity keys equals the dial's expected node_id.

   If any check fails, the client closes without sending CONNECT.
5. **CONNECT.** The client sends its identity public key or keys, its CONNECT-key binding, its CONNECT public key,
   and the proof. The proof is a signature by the CONNECT key over:
   - a label distinct from every frame and binding label;
   - the nonce;
   - the station's node_id and its own node_id;
   - the SHA-384 over the DER encoding of the station's leaf certificate as received;
   - the SHA-384 of the challenge frame as received.
6. **Station checks:** the CONNECT-key binding's signature, validity and permitted use; and the proof, over its
   own nonce, its own node_id, the node_id derived from the client's identity keys, and the leaf certificate it
   presented in this session. The client's derived node_id becomes the connection's peer identity.
7. **HELLO.** The station sends its capabilities, and accepts or refuses.

**Requirements:**

- **Server certificate check.** The client verifies the TLS 1.3 handshake signature against the presented leaf,
  and refuses any key type other than the profile's. It checks no chain, name, CA or expiry, because station
  certificates are self-signed. The post-quantum build has no other client verification mode.
- **No resumption.** Stations send no TLS 1.3 session tickets and keep no session storage. Clients offer no
  resumption and keep no session cache. Early data stays off. Every connection is a full handshake, so there is
  always a verified leaf certificate.
- **Leaf bytes.** Both sides hash the leaf certificate's DER exactly as it went over the wire. Station
  certificates are strict DER. The station hashes the leaf its listener presents, never a re-encoded file. A leaf
  that is not strict DER fails closed.
- **Nonce.** 256 bits from a CSPRNG, fresh for each connection, held only in that connection's state. A proof
  that carries any other nonce is refused.
- **Frame order per role.** The client accepts only the challenge, then HELLO. The station accepts only the
  opener, then CONNECT. Any other or repeated frame closes the handshake with a distinct reason. Close reasons stay
  local; a refused client sees only HELLO with `accepted` 0 and a coarse refusal code.
- **Control stream.** Only the stream the client opened is the control stream. A stream the station opens during
  the handshake is closed.
- **Failures are visible.** A station reports every failed handshake to its owning process, with the remote
  address and a classified reason.
- **Cost.** One more round trip per new connection than a handshake of CONNECT then HELLO. Calls reuse
  connections. With D22's status statements the challenge is about 12 KB and CONNECT about 19 KB in the US profile,
  and each can cost one more round trip under QUIC congestion control. V9 measures the round trips.

### Dials

- **No dial without an expected identity.** Every dial target carries the node_id the dialer chose, and a dial
  without one refuses to start. The source does not change the check: a seed, a directory row, a station
  reference learned from the DHT, a redundancy candidate or a signed record. A wrong source can only cause a
  refused dial.
- **One identity check per connection,** through the identity function that derives node_ids.
- **First contact.** The seed list is the anchor. Each seed carries the station's node_id and profile. Seed
  lists are generated from `stations.csv`, for configuration and for compiled-in defaults.
- **Directory rows are self-certifying.** A row carries the station's own signed records, and their key must
  derive to the row's node_id. A hostname is a locator, never a trust input; nothing maps a name to a node_id.
- **Station to station.** Bootstrap peers carry a node_id in station configuration, generated from
  `stations.csv`. Peers learned later are dialled with the node_id they were learned under.
- **Endorsement:** no endorsement is needed to use a station (D23).

### Keys in relayed and stored objects (D13)

- A neighbour's identity key arrives in the challenge (station) or in CONNECT (client), and is bound to that
  connection.
- Relayed and stored objects carry the signer's full public key or keys. A verifier checks that they derive to
  the claimed node_id, then verifies the signature. This covers records, publisher signatures, origin signatures
  on relayed frames, UCANs, and ownership and device proofs.
- No signature check ever looks a key up over the network.
- A minimal post-quantum UCAN is about 11 KB in the US profile and 13 KB in the EU profile: `iss` carries the full
  key and `aud` the node_id (D7).
  Proofs are content ids of other tokens, so proof chains do not multiply that.
- Within one stream, the provider's key travels once, in its first frame (D25).

### Signatures between neighbours (D17)

Once the connection handshake authenticates both ends of every connection:

- Only signatures checked against the connection's node_id can drop. Origin signatures on relayed frames,
  publisher signatures, stream frame signatures and records always stay.
- **US profile:** no neighbour signature on SWIM, ADVERTISE and UNADVERTISE, SUBSCRIBE and UNSUBSCRIBE, PUBLISH,
  a relayed EVENT, the overlay relay envelope, DHT protocol frames and content frames.
- **EU profile:** hybrid neighbour signatures stay on control frames: SWIM, DHT protocol, ADVERTISE and
  UNADVERTISE, SUBSCRIBE and UNSUBSCRIBE, and the overlay relay envelope. They drop on data frames: PUBLISH, a
  relayed EVENT and content frames.
- **Stream frames are signed at both ends:** a provider's frames by the provider (D25), and every frame a caller
  originates after STREAM_OPEN by the caller, in every stream mode, under `MACULA-PQ-CALLER-STREAM-V1`, with the key
  from the verified STREAM_OPEN (revised by Raf on 2026-09-11).
- Every event carries a publisher signature, verified at the origin station.
- A reply is accepted only from the provider the caller signed as the target, and only for the request it answers
  (D25).
- Cost of the EU control frames: SWIM sends about one signature per second per station ✅, and a hybrid verify
  takes about 0.31 ms (D4).

---

## Stages

The order is Raf's. The US profile goes first in every stage, and the EU profile follows right after (D15).
Part 2 has the work packages.

| Stage | Scope | Owners | Waiting on |
|---|---|---|---|
| 0 | Checks before building (V items, Part 1) | per check | nothing |
| 1 | `macula` on branch `post-quantum`, and `macula-station` | Mercury, Neptune, Mars | nothing |
| 2 | Erlang-only test suite | Terra | Stage 1 |
| 3 | Post-quantum fleet, station directory, realm and distribution relay | Terra, Mercury, Neptune | Stage 2 |
| 4 | Each other stack, with its suite against the fleet | Neptune, Venus, Pluto; Uranus later | Stage 3 |
| 5 | Cutover of `macula-cli`, `macula-mcp` and `lazymesh` | Mars, Venus | Stage 4 (Go); EU parts for `io.macula` |
| 6 | Cutover of the hecate services | Saturnus | Stage 5, EU parts included; the Reckon plan (D9) |
| 7 | Quality and security gates | Fable, Jupiter | runs throughout |

- The EU parts of every stage also wait on V8.
- V2 and V3 are closed and V4 has passed; V4's three open points do not hold up the US parts of Stage 1 (Part 1).
- Stage 6 follows Stage 5 in Raf's order; technically it needs only Stage 3.
- `io.macula` runs the EU profile (D19). Stage 5 tools that join `io.macula` and the Stage 6 hecate services wait on
  the EU parts of the stages before them, not only the US parts. The US parts still go first (D15), against a
  US-profile realm whose name is open.
- After Stage 6, Raf publishes `macula` 11.0.0 on hex, and the live fleet is switched off (D14, D20).
- .NET programs keep working against the live fleet until it is switched off, then stop until msquic supports
  post-quantum key exchange (D10).
- Published SDK versions stay on hex, crates.io, NuGet, PyPI, npm and the Go module proxy and cannot be
  withdrawn. Old clients fail to connect to the new fleet, which is acceptable in a development environment.

---

## Decisions

Raf answered "go with the recommendations" on 2026-09-10.

- **Accepted** marks a decision his answer covers.
- **Recommended** marks a recommendation still to be confirmed.
- **Revision pending** marks a proposed change after later checks, waiting for Raf.

| ID | Decision | Answer in short | Status |
|---|---|---|---|
| D1 | Where the profile is chosen | Per realm | Accepted |
| D2 | Profiles per station | One station instance per profile | Accepted (revised) |
| D3 | EU key exchange at level 5 | Custom SecP384r1MLKEM1024 group in Rust | Accepted |
| D4 | EU signature form and classical half | ML-DSA-87 in TLS; hybrid elsewhere; RSA-PSS-4096 | Accepted (revised) |
| D5 | node_id derivation | SHA-256 over label, profile and identity keys | Accepted |
| D6 | How node keys are stored | One purpose per key, expanded ML-DSA keys, round trip on load | Accepted (revised) |
| D7 | Key encoding in UCANs and DIDs | Published names; composite `ML-DSA-87-PS384`; `aud` by node_id | Accepted |
| D8 | OpenSSL floor and base images | OpenSSL 3.5.0 or newer at build time; station and realm on Debian 13 | Accepted |
| D9 | `reckon_gater` capability signing | Separate plan in `reckon-db-org`, finished before Stage 6 | Accepted |
| D10 | A stack that cannot do its profile | .NET out of the first switch; Python ships a patch | Accepted (.NET) |
| D11 | What public text may claim | Alignment wording only, per the rules in D11 | Accepted |
| D12 | TLS key and its binding | Separate TLS key, binding in the challenge, new fleet only | Accepted |
| D13 | Where a verifier gets the full key | Handshake for neighbours, carried in objects, no lookups | Accepted |
| D14 | How the switch happens | A second post-quantum fleet next to the live one | Accepted |
| D15 | Profile order | US first, EU right after | Accepted |
| D16 | Connection handshake and client proof | Signed CONNECT proof, one dial mode, no resumption | Accepted |
| D17 | Signatures between neighbours | US drops most; EU keeps control frames; caller stream frames signed | Accepted |
| D18 | EU session proof | Fixed binding; exporter proof only when an offer needs it | Accepted |
| D19 | Realm on the new fleet | `io.macula` with the EU profile, separate realm deployment | Accepted |
| D20 | Branch and release | Branch `post-quantum`, then `macula` 11.0.0 | Accepted |
| D21 | Live fleet during the work | Pinned to a released station version | Accepted |
| D22 | Binding lifetime and revocation | 7 days, rotated every 5; stapled status statements | Accepted (revised) |
| D23 | Endorsement of stations | None to use a station; a later one is its own signed record | Accepted |
| D24 | Hashes under signatures | SHA-384 for content and UCAN parent ids; node ids stay SHA-256 | Accepted |
| D25 | Replies bound to provider and request | Caller-signed target, request hash, signed stream frames | Accepted |
| D26 | Peer-supplied maps | One key form in 11.0.0, read through the facade accessors | Accepted |
| D27 | Where content lives | The sharing node keeps and serves it; stations only pass it through | Accepted |

The full text of each decision is in
[PLAN_POST_QUANTUM_SECURITY_DECISIONS.md](PLAN_POST_QUANTUM_SECURITY_DECISIONS.md).

---

## Open items

| Item | Owner | State |
|---|---|---|
| Leaf certificate before CONNECT in Go, .NET and Python (V16) | Venus, Uranus, Pluto | Python needs its patch |
| NTS servers for fleet time synchronisation (D22) | unassigned | open |
| Authorization for procedures without an org namespace (D25) | Raf | open |
| Cross-profile federation: how realms of different profiles exchange calls and facts | Raf, with Jupiter | open |
| Name of the US-profile realm on the post-quantum fleet (D19) | Raf | open |
| Owner of the Reckon post-quantum plan, assigned when Stage 4 starts (D9) | Raf | open |
| Retiring `macula-portal`, `macula-relay`, `hecate-daemon` and `hecate-stub` (D8) | Raf | open |
| How the aioquic patch ships (D10) | Pluto | open |
| Unused signing functions for SWIM membership updates | Mercury | removed in `8cd60ee` on `post-quantum` |
| BEP44 bootstrap | Terra, then Raf | Terra checks whether it runs anywhere |
| Endorsement of stations (D23) | Raf | accepted on 2026-09-11 |
| DHT slot bounds to define: records per slot, VALUE paging (WP 1.3, WP 1.6) | Mars | open |
| Verification budget per connection, slowing reads instead of closing (WP 1.3, WP 1.6) | Mercury, Mars | open |

---

## Effort

Rough, for planning. Items marked ⚠ are not estimated yet.

| Stage | Work | Effort |
|---|---|---|
| 0 | Checks (V2, V3, V6, V15, V17 and V18 answered; V4 passed; V7 handshakes proven) | 17 to 20 days |
| 1 | Profile model | 1 to 2 days |
| 1 | Rust transport in the NIF | 6 to 8 days |
| 1 | Identity core | 12 to 16 days, plus handshake frames, bindings and status ⚠ |
| 1 | UCAN and DID encoding | 2 to 3 days |
| 1 | `macula-station` | 11 to 22 days |
| 2 | Erlang-only suite | 2 to 3 days, plus the harness change ⚠ |
| 3 | Realm | 7 to 10 days |
| 3 | Fleet and seeds | 3 to 5 days, plus configuration generation ⚠ |
| 3 | Station directory | ⚠ |
| 3 | Distribution relay | ⚠ |
| 4 | Go transport | 2 to 4 days, plus the FFI unification ⚠ |
| 4 | Python transport | 6 to 9 days, plus seeds and record verification ⚠ |
| 4 | Rust transport in `macula-rust` | 3 to 4 days |
| 4 | Identity in the SDKs | 13 to 17 days |
| 4 | .NET | not in the first switch |
| 5 | Cutovers | ⚠ |
| 6 | hecate services | 3 to 5 days, plus the images on OTP 27 ⚠ |
| 1, 4 | Reply binding to provider and request (D25) | ⚠ |
| 7 | Gates | throughout |
| | **Total** | **at least 88 to 128 person-days, plus the items marked ⚠** |

Stage 0 checks, the Stage 4 stacks and the SDK identity work can run in parallel across the team.

---

## Success criteria

- [ ] Every V item closed with a recorded result, and every decision taken.
- [ ] Every stack's wire checks green against the new fleet in its profile: group, signature scheme and cipher
  suite reported by two independent views that agree, and the connection handshake checked (D16).
- [ ] Every station instance refuses a classical-only client and X25519MLKEM768, in both profiles.
- [ ] The claim-gate tests pass (Stage 7).
- [ ] Replies are accepted only from the provider the caller targeted, and only for the request they answer (D25).
- [ ] EU hybrid signatures follow BSI TR-02102-1 section 5.3.4: concatenated, valid only if all parts verify, keys
  dedicated to hybrid use, classical half from BSI's lists, no Ed25519.
- [ ] No Ed25519, RSA or ECDSA-only signing path remains in `macula`, the SDKs, the realm or hecate-om, enforced by
  a failing test rather than by review.
- [ ] Every consumer cut over, the live fleet switched off, and `macula` 11.0.0 published by Raf.
- [ ] No public claim made for a stack before its wire checks are green.
