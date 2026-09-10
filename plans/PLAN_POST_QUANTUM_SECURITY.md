# Plan: post-quantum security for Macula

**This exists so the team can deliver post-quantum protection for Macula in a known order, with every step
testable, and Raf can point to a concrete plan in a partner offer.**

**Status:** Planning. Raf accepted the recommendations on 2026-09-10. Open items are marked with their owner.
**Created:** 2026-09-10
**Last Updated:** 2026-09-10
**Classification:** BUILD
**Reasons and measurements:** [EXPLORATION_POST_QUANTUM.md](EXPLORATION_POST_QUANTUM.md)

**This plan is split into three documents:**

- this root: overview, crypto profiles, key model and connection design, stages, decisions, open items, effort
  and success criteria;
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
- **One purpose per key.** Identity, TLS and CONNECT keys are separate (key model below).
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
- **Endorsement** of stations by a realm or a foundation is open (D23).

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
- **Stream frames are signed at both ends:** a provider's frames by the provider (D25), and a caller's frames in
  client_stream and bidi streams by the caller, under `MACULA-PQ-CALLER-STREAM-V1`, with the key from the verified
  STREAM_OPEN (revised by Raf on 2026-09-11).
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
| D23 | Endorsement of stations | Open | Open |
| D24 | Hashes under signatures | SHA-384 for content and UCAN parent ids; node ids stay SHA-256 | Accepted |
| D25 | Replies bound to provider and request | Caller-signed target, request hash, signed stream frames | Accepted |

### D1 Where the profile is chosen

- **Answer, accepted by Raf on 2026-09-10:** **One profile per realm.**
- **Why:** the realm is the trust domain. A US-profile peer (pure ML-KEM-1024) and an EU-profile peer
  (SecP384r1MLKEM1024) share no key exchange group, so they cannot complete a handshake. With D2, a realm's
  programs use the station instances of its profile.
- **Blocks:** WP 1.1, WP 3.1.

### D2 One station instance per profile

- **Answer:** a station instance speaks exactly one profile. A box that serves both runs two instances on
  separate ports, each with its own identity, TLS key, CONNECT key, seeds, directory rows, DHT membership and
  links.
- **Why:**
  - BSI asks for hybrid key material dedicated to hybrid use ✅, so an EU identity pair cannot also serve as a US
    identity key. Two identities are needed either way.
  - node_id, DHT membership and routing belong to one identity.
  - The port tells the station the profile before TLS starts.
  - It matches D1, and .NET's limit of one profile per process ✅.
- **Not taken:** one process holding both identities and choosing a binding from the negotiated key exchange
  group.
- **Cost:** two containers and two `stations.csv` rows per box that serves both profiles.
- **Status:** accepted 2026-09-10, revised from "one station speaks both profiles".
- **Blocks:** WP 1.1, WP 1.2, WP 1.6, WP 3.2.

### D3 EU key exchange at level 5, given rustls lacks SecP384r1MLKEM1024

- **Answer, accepted by Raf on 2026-09-10:** **Implement SecP384r1MLKEM1024 as a custom group in the Rust NIF and
  macula-rust, on the public rustls traits with aws-lc-rs primitives.**
- **Upstream:** offering the group to rustls was approved by Raf on 2026-09-10, to be offered after Stage 1 has
  proven it. Opening the contribution is a public act, so it gets Raf's word in the acting session at that time.
- **Why:** V4 built the group in about 90 lines on the public rustls traits, over rustls's own P-384 and
  ML-KEM-1024, and it interoperates with Go 1.27 and OpenSSL 3.6.4 in both directions, including
  HelloRetryRequest ✅. BSI names it ✅ and it matches the level-5 requirement. BSI intends to recommend it once its
  RFC is adopted ✅ (TR-02102-2 section 3.4.2).
- **Alternatives:**
  - wait for upstream rustls (unknown date ⚠);
  - or run the EU profile at SecP256r1MLKEM768, which rustls and Go have today ✅ and BSI names ✅, but which is
    below level 5 and below ANSSI's preference ⚠.
- **Blocks:** WP 1.1 and WP 1.2, EU parts.

### D4 EU signature form and its classical half

- **Recommendation:**
  - **Inside TLS:** ML-DSA-87 alone, on the TLS key, because no TLS 1.3 standard for hybrid authentication
    exists ✅.
  - **Every signature Macula makes** (bindings, status statements, CONNECT proofs, records, origin and publisher
    signatures, UCANs, DIDs, realm credentials): BSI's concatenation, ML-DSA-87 plus a classical half, valid only
    if both verify, with key material used only for these hybrid signatures.
  - **Classical half, decided by Raf on 2026-09-10:** RSA-PSS instead of ECDSA on brainpoolP384r1, with:
    - PSS per RFC 8017 section 8.1; public exponent 65537 and equal-size random primes (ANSSI PG-083
      RègleFactorisation.5, RecoFactorisation.2 and .3);
    - SHA-384 for both the message hash and MGF1;
    - a 48-byte salt, fresh for every signature, from a random generator that meets TR-02102-1;
    - a 4096-bit modulus for every EU hybrid key, identity and CONNECT keys alike, following BSI's advice for
      long-lived systems (3072 bits would also comply with all three texts);
    - no separate message-signing key;
    - in every stack, a signer whose private-key operation is constant-time (no key-dependent timing, branches or
      memory access), blinded where the library provides it, and which verifies each signature before releasing it
      (V8); revised and accepted by Raf on 2026-09-10. An unblinded signer, such as Go's, is weaker against a physical
      or differential attacker with known inputs, which the texts answer with certified hardware (BSI AIS 46).
- **Why the revision:**
  - Go libraries for brainpoolP384r1 interoperate with OTP, but sign through a path that is not constant-time, and
    Rust has no brainpoolP384r1 at all ✅ (V8). A long-term identity key needs a constant-time signer.
  - Go 1.27 `crypto/rsa` signs PSS through constant-time big-number code ✅. OTP signs and verifies RSA-PSS with
    SHA-384 through OpenSSL ✅. Whether OpenSSL, aws-lc-rs, Python `cryptography` and .NET meet the signer
    conditions is checked in V8 ⚠. aws-lc-rs is already a dependency of the post-quantum transport.
  - Every text accepts it: BSI Table 5.3 lists RSA, and section 5.3.1 asks for a modulus of at least 3000 bits ✅;
    ANSSI accepts RSA-SSA-PSS with 3072 bits from 2031 ✅; the ECCG list agrees RSA PSS with at least 3000 bits ✅.
  - BSI asks long-lived systems for at least 128 bits of security in every component, and puts 128 bits at about a
    3200-bit modulus ✅; an identity key lives as long as its node.
- **Cost, by classical half:**

  | | brainpoolP384r1 | RSA-3072 | RSA-4096 |
  |---|---|---|---|
  | Classical strength | about 192 bits ⚠ | about 128 bits ⚠ | above 128 bits ⚠ |
  | EU hybrid signature | 4,729 B | 5,011 B | 5,139 B |
  | EU hybrid identity public key | 2,689 B | about 2,980 B | about 3,110 B |
  | EU hybrid signing | 1.8 ms | 3.2 ms | 5.3 ms |
  | EU hybrid verification | 0.83 ms | 0.28 ms | 0.31 ms |
  | EU publisher ceiling from signing alone, per core | about 560 per second | about 317 | about 188 |

  - With RSA-4096, EU publishing is about 41% slower than with RSA-3072, counting signing alone. Verification is
    about 8% slower.
  - Key generation takes a median of 445 ms at 4096 bits, and up to 915 ms, against 95 ms at 3072 bits. Identity
    keys are generated once, and CONNECT keys every 5 days.
  - 188 events per second per core is above what current Macula publishers need, and signing spreads across
    cores. That is a judgment, not a measurement of real publishers ⚠.
- **Not taken:**
  - a constant-time brainpoolP384r1 written by the team in Go and Rust: new cryptographic code that needs an
    audit;
  - ECDSA on P-384: TR-02102-1 names no P-curves ✅;
  - a separate 3072-bit message-signing key certified by a 4096-bit identity key: every relayed or stored object
    would then also carry that key and its binding (D13), about 8 KB more per object.
- **Reasons that stand:** BSI recommends a quantum-safe signature only combined with a classic one ✅; Ed25519 is
  not in BSI's documents ✅; BSI allows hash-based signatures alone where their implementation security is
  carefully considered ✅, but they have no TLS 1.3 scheme anywhere ✅, are missing from rustls, Go and
  cryptography ✅, and cost 30 to 50 KB and up to 453 ms per signature ✅.
- **Open:** whether BSI and ANSSI would assess this design as hybrid ⚠.
- **Blocks:** EU parts of WP 1.1, WP 1.3, WP 1.4, WP 3.1 and WP 4.1 to 4.3.

### D5 node_id derivation

- **Recommendation:** **node_id is a 32-byte hash over an explicit derivation label, the profile, and the identity
  public key (US) or the concatenated hybrid identity public keys (EU).** The TLS and CONNECT keys are not part of
  node_id.
- **Hash, accepted by Raf on 2026-09-10:** SHA-256, with the identifier qualifier in the public wording (D11), under two
  rules (D24):
  - every verifier checks the full carried key (D13), never a node_id alone;
  - no trust is ever granted to a node_id because of a property of one particular key, such as attested
    hardware; a statement about one key names it by the SHA-384 of the key as carried (D13).
- **Constants** (WP 1.3, 2026-09-10):
  - node_id = SHA-256(Label || 0x00 || len(Profile) || Profile || IdentityKey);
  - Label: the 17 ASCII bytes `MACULA-NODE-ID-V1`; len(Profile): one byte; Profile: the ASCII name `pq_pure`
    or `pq_hybrid`;
  - IdentityKey: the identity public key as carried (D13): 2,592 bytes for `pq_pure`, and for `pq_hybrid` the
    ML-DSA-87 key followed by the DER `RSAPublicKey`, 3,118 bytes;
  - reference vectors, with byte i of the key being i mod 256, reproduced in Go, Rust and Python on 2026-09-10 ✅:
    - `pq_pure`, 2,592 bytes: `8c6a28c62bda0112065bccb0d8b02b18f46fef03d16e8b7ae91086025dd209ff`;
    - `pq_hybrid`, 3,118 bytes: `e9df1133a8238239c58fc7f886d9667e961449ee272c30e968a0eecbb6b0131c`;
    - `pq_hybrid` over the same 2,592 bytes: `4e79818f04bffbd7f2df71b82e3e543458d9f74cda64f88a80b352ed8e9af10b`.
- **Why:**
  - It keeps the DHT keyspace, routing, 32-byte id fields and the puzzle evidence. An ML-DSA-87 public key is
    2,592 bytes ✅, far too large to be the id. Signatures still grow by about 4.6 KB; only id fields keep their
    size.
  - The label and profile keep every derivation unambiguous, so no key has two ids.
  - Impersonating a node needs a key whose hash equals its node_id, which rests on second-preimage resistance:
    201 to 256 bits for SHA-256, the high end for short inputs such as a 3 KB key (SP 800-107r1 Table 1 ✅), so
    there is no mismatch with ML-DSA-87. Collision resistance does not matter under the two rules above.
  - CNSA 2.0 lists SHA-384 and SHA-512 by function ✅ (V17), so a SHA-256 identifier needs a qualifier in US
    wording. Macula derives other identifiers with SHA-256 too ✅: the realm id is the SHA-256 of the realm name,
    and every derived DHT storage key is SHA-256; the others are a record's 32-byte key itself, which is a node_id
    in the post-quantum format (WP 1.3). So a qualifier is needed whichever hash node_id uses, unless every
    identifier changes, which would also change the realm id that D19 keeps.
- **Not taken:**
  - a full 48-byte SHA-384 node_id: it moves the whole DHT keyspace in `macula`, `macula-station`, every SDK's
    DHT client and the station directory, about 10 to 20 more person-days ⚠, and still needs the qualifier;
  - SHA-384 truncated to 256 bits: the same size and nearly free, but the qualifier stays.
- **Blocks:** WP 1.3.

### D6 How node keys are stored

- **Answer, accepted by Raf on 2026-09-10, revised the same day for ML-DSA key storage:**
  - **per node:** the identity key or pair, the CONNECT key or pair, and on station instances the TLS key, with
    each ML-DSA-87 private key stored in its 4,896-byte expanded form next to its public key;
  - **each key serves exactly one purpose** (key model);
  - **on load, the public key of each ML-DSA-87 key is derived from the expanded key and must equal the stored
    public key, and then every key is checked with a sign-and-verify round trip by the whole key, so a hybrid key
    signs only its composite;**
  - **TLS and CONNECT keys and their bindings rotate every 5 days** (D22).
- **Why:** OTP generates ML-DSA-87 keys only in expanded form and cannot derive a public key from a seed, but derives
  it from the expanded key with `generate_key(mldsa87, [], K)` (OTP 28.4.2 and 29.0.6) ✅; BSI asks for hybrid key
  material dedicated to hybrid signatures ✅; the ECCG list requires different key pairs for message
  signatures and authentication ✅; ANSSI PA-079 section 6.3 and BSI TR-03116-4 ask for separate keys per
  purpose ✅.
- **Blocks:** WP 1.3, WP 1.6.

### D7 Key encoding in UCANs and DIDs

- **Answer, accepted by Raf on 2026-09-10 with two checks:** **Use published names and constructions where they
  exist, and Macula's own names only where none exists. UCAN and DID signing and verification move into
  `macula_identity`; the NIFs keep only the encoding. `aud` names the audience by node_id.**
- **Why:**
  - Every verifier is Macula's own code, and membership UCANs use a raw hex key as `aud` ⚠ (V12).
  - OTP signs and verifies both halves; with signing in Erlang, private keys never enter Rust.
  - Respect two existing rules: no `did:macula:` prefix, and no self-rooted identity without realm endorsement.
  - With `did:key` in both `iss` and `aud`, a minimal post-quantum UCAN is about 16 KB in the US profile and
    18.5 KB in the EU profile; naming `aud` by node_id brings that to about 11 and 13 KB (check 2).
- **Check 1, standard names** (2026-09-10):
  - **US:** RFC 9964 (May 2026, Standards Track) registers the JOSE `alg` `ML-DSA-87` and the COSE algorithm -50,
    with the key type `AKP`, whose `priv` is the 32-byte seed; the context string is empty, and HashML-DSA is not
    specified ✅. The multicodec table lists `mldsa-87-pub` as 0x1212, in draft status ✅. The US profile uses
    these names.
  - **EU, a standard construction exists:** `draft-ietf-lamps-pq-composite-sigs-19`, in the RFC Editor queue,
    defines `id-MLDSA87-RSA4096-PSS-SHA512` ✅. Its RSA-PSS half uses SHA-384, MGF1 with SHA-384 and a 48-byte
    salt, as D4 does. Both halves sign M' = Prefix || Label || len(ctx) || ctx || SHA-512(M), ML-DSA-87 with the
    label as its context. The public keys, with RSA as `RSAPublicKey`, and the signatures are concatenated; a
    signature is valid only if all component signatures verify; component keys must not be used in any other
    context ✅.
  - **EU, no JOSE or COSE name:** `draft-ietf-jose-pq-composite-sigs-03` (July 2026) pairs ML-DSA only with
    ECDSA and EdDSA ✅, and the multicodec table has no composite key ✅.
  - **Compatible with BSI:** TR-02102-1 section 5.3.4 calls "the concatenation of a quantum-safe signature with a
    classic signature so that the concatenated signature is accepted as valid if all individual signatures are
    valid" a natural and robust hybridisation, and asks for key material generated for hybrid signatures only ✅. The
    composite is that concatenation over one message representative, with dedicated keys, so it meets D11's
    preconditions 1 and 2. ML-DSA stays the pure variant (precondition 3): SHA-512 of the message is hashing at
    the application level, which FIPS 204 section 5.4 separates from HashML-DSA, and it meets that section's bar
    of 256 bits of collision and second-preimage strength for ML-DSA-87 ✅. Whether BSI and ANSSI would assess
    the design as hybrid stays open (D4) ⚠.
  - **Not buildable in OTP as specified:** OTP 28.4.2 and 29.0.6 sign ML-DSA without options, so without a context
    string ✅, and D7 keeps private keys out of Rust.
  - **Accepted by Raf on 2026-09-10:** every EU hybrid signature that D4 names is Macula's own composite on the
    LAMPS structure: the same M', the same RSA-PSS parameters from D4, concatenated keys and signatures, valid only
    if both verify, with ML-DSA-87 signing M' under an empty context. It has no LAMPS name or identifier; its `alg`
    is `ML-DSA-87-PS384`, after the JOSE composite draft's pattern (`ML-DSA-87-ES384`) and JOSE's `PS384`. EU keys
    in `did:key` use Macula's own key type until a multicodec exists ⚠.
  - **Constants** (set in WP 1.3, 2026-09-10): Prefix is the 32 ASCII bytes `CompositeAlgorithmSignatures2025`, as
    in LAMPS; Label is the 22 ASCII bytes `MACULA-ML-DSA-87-PS384`; ctx is empty for every Macula object, so
    len(ctx) is the single byte 0, and each object keeps its own domain label inside M; PH is SHA-512. RSA-PSS signs
    M' with SHA-384, MGF1 with SHA-384, a 48-byte salt and public exponent 65537. The signature is the 4,627-byte
    ML-DSA-87 signature followed by the 512-byte RSA-PSS signature, and the public key is the 2,592-byte ML-DSA-87
    key followed by the 526-byte DER `RSAPublicKey`, both without length prefixes.
  - **Upstream and switch:** Macula tracks erlang/otp #11589 (OTP-20368), the OTP team's own change that adds an
    ML-DSA context string to `crypto:sign/5` and `verify/6` ✅. Macula contributes no code to it; Raf may comment
    on it in his own words. If OTP ships it before `macula` 11.0.0, Macula switches to
    `id-MLDSA87-RSA4096-PSS-SHA512`.
- **Check 2, audience by node_id** (2026-09-10):
  - `iss` keeps the full key, which verifies the token (D13); `aud` carries the audience's node_id.
  - **Refined and accepted by Raf on 2026-09-10:** a token is presented by the node its `aud` names, inside a
    request that node signed and targeted at the verifier. Before any handler runs, the provider checks, in order:
    1. the frame signature against the caller key;
    2. the signed target equals its own node_id (D25 item 2);
    3. `aud` equals the node_id derived per D5 from that verified caller key;
    4. the chain: each proof's `aud` equals the node_id of the next token's `iss` key, the chain roots at the
       policy's required issuer, and capability, expiry and SHA-384 parent ids (D24) all check.
  - CALL and STREAM_OPEN carry a signed deadline in the post-quantum format. A provider refuses a request past that
    deadline plus the D22 tolerance and keeps its (caller, call id) deduplication until then, so there is no nonce
    store for CALL and STREAM_OPEN; a nonce store remains only for tokens used outside a signed request.
  - An authorizing verify takes the verified caller key and the expected target; a check of the chain alone has a
    name that says it does not authorize.
  - Membership tokens from the realm are presented only inside a CALL, so this rule needs no exception (Neptune,
    2026-09-10).
  - This is sound under D24's rules for node ids. Using a delegation needs a key that derives to the named
    node_id, a second preimage at the high end of SHA-256's 201 to 256 bits (D5). A collision only gives one party
    two keys for one node_id, and so a delegation it already holds; no issuer delegates to a node_id because of a
    property of one key. UCAN parent ids stay SHA-384 (D24).
  - UCAN 0.10.0 makes `aud` a DID, has the receiver match `aud` with its own DID, and requires the `aud` of every
    proof to match the outer `iss` (sections 3.2.2, 6.2 and 6.2.1) ✅. Matching by derived node_id departs from
    that string comparison; Macula's verifiers are its own code. WP 1.4 sets the string form of a node_id
    audience, without a `did:macula:` prefix.
  - **Size**, computed from the current token layout with keys and signatures of the profile sizes, one
    capability and no proofs:

    | | US | EU |
    |---|---|---|
    | `aud` as full key | 15.9 KB | 18.5 KB |
    | `aud` as node_id | 11.2 KB | 12.9 KB |
    | Chain of three tokens, full-key audiences | 47.9 KB | 55.7 KB |
    | Chain of three tokens, node_id audiences | 33.9 KB | 38.9 KB |

- **Blocks:** WP 1.3, WP 1.4, WP 3.1.

### D8 OpenSSL floor and base images for every BEAM service

- **Answer, accepted by Raf on 2026-09-10:**
  - **builder and runtime images both** move to bases whose OpenSSL is 3.5.0 or newer at build time (V3);
  - **`macula-station` and `macula-realm`** move to Debian 13 slim images from the official `erlang:28-slim` line,
    so only one thing changes at a time; the e2e harness image may stay on Alpine;
  - **`macula-portal`, `macula-relay`, `hecate-daemon` and `hecate-stub` are not upgraded:** they are out of scope
    and to be retired, which is Raf's call and not planned here;
  - **`macula-dist-relay`** carries Erlang distribution over QUIC, so it moves in Stage 3 with the station and realm
    (WP 3.4);
  - **the hecate images on OTP 27** move to OTP 28 in WP 6.1.
- **Why:** ML-DSA in OTP needs OTP 28 or newer ✅, compiled against OpenSSL headers 3.5.0 or newer: OTP 28.1,
  28.4.2 and 29.0.6 enable ML-DSA and ML-KEM only under that compile-time check ✅ (V3). OTP's `crypto.so` is
  compiled in the builder image, so replacing only the runtime image is not enough ✅. The station and realm images
  are Debian 12 with OpenSSL 3.0, whose OTP `crypto` has neither ✅. Among the fleet's images, 13 have both, 10 need
  only OTP 28 on the OpenSSL 3.5 they have, 4 are Debian 12 with OpenSSL 3.0, and 2 are not BEAM ✅ (V2).
- **Blocks:** WP 1.3, WP 1.6, WP 3.1, WP 6.1.

### D9 `reckon_gater` capability signing

- **Decision:** `reckon_gater` capability signing (Ed25519, used by hecate-om ✅).
- **Answer, accepted by Raf on 2026-09-10:** **Separate plan in `reckon-db-org`, finished before Stage 6 starts,
  because the hecate services use those capabilities.**
- **Why:** Reckon is kept independent of Macula.
- **Blocks:** WP 6.1.

### D10 A stack that cannot do its profile after Stage 0

- **.NET, accepted 2026-09-10 (option a):** .NET is left out of the first post-quantum switch. .NET programs keep
  working against the live fleet until it is switched off, then stop until msquic supports post-quantum key
  exchange.
  - Not chosen: patching msquic ourselves (only if a paying customer needs .NET), and our own msquic bindings.
  - Offering a fix upstream is public and needs Raf's explicit yes.
  - Why: no msquic build today can offer ML-KEM ✅ (V6).
- **Python:** V7 proved that a client-side patch to aioquic's `tls.py` gives both profiles against a Go server ✅,
  so Python stays in the programme. **Open:** how the patch ships: fork, vendor or upstream (Pluto). The packaging
  facts are in V7.
- **Any other stack:** decide after Stage 0. With no classical fallback, such a stack cannot connect at all.
- **Blocks:** WP 4.3, WP 4.4.

### D11 What public text may claim

This applies to every public text: README, website, grant applications and offers. Nothing is claimed for a stack
before its wire checks are green.

- **Accepted by Raf on 2026-09-10, with one check:** before any grant text or the website says anything about
  post-quantum, Saturnus reads that wording.
- **Never, in any profile:**
  - "certified", "validated", "assessed", or "conformant" or "compliant" with BSI, ANSSI, CNSA 2.0 or EUCC;
  - "hybrid TLS authentication";
  - "constant-time" or "side-channel resistant", without an evaluation;
  - that ML-DSA-87 alone is acceptable in Europe;
  - that Ed25519 is part of the EU profile;
  - that Macula's hybrid signature is the LAMPS composite or `id-MLDSA87-RSA4096-PSS-SHA512`: it is Macula's own
    composite, `ML-DSA-87-PS384` (D7).
- **US, when true:** "algorithms aligned with CNSA 2.0 (ML-KEM-1024, ML-DSA-87, AES-256, SHA-384)". Never imply
  deployability in National Security Systems, which also needs NIAP or NSA validation ⚠ (V14). Preconditions: V13
  and V14 closed.
  - Identifier qualifier, while D5 keeps SHA-256: "Algorithms are aligned with CNSA 2.0, except that identifiers
    (node, realm, DHT keys) use SHA-256." Texts about today's format also add that content identifiers use
    BLAKE3, which is not a NIST-standardised hash function.
  - Profile name qualifier: text that names the `pq_pure` profile says it uses the CNSA 2.0 algorithm list, and
    that Macula is not a National Security Systems product.
- **EU, when true:**
  - base: "TLS session authentication is ML-DSA-87; the identity-to-TLS-key binding is hybrid (ML-DSA-87 plus
    RSA-PSS-4096, built as described in BSI TR-02102-1 section 5.3.4)";
  - key exchange: "the transport uses the hybrid group SecP384r1MLKEM1024, which BSI TR-02102-2 section 3.4.2 names
    as one it intends to recommend once the RFC is adopted", never "BSI-recommended";
  - clients: "clients authenticate to stations with a hybrid signature over a station-chosen challenge and the
    station's TLS certificate; this proof is not bound to the TLS key exchange";
  - only with an exporter-bound session proof (D18): "each session is additionally authenticated by a hybrid
    signature bound to the TLS session";
  - identifier qualifier: "Identifiers (node, realm, DHT keys) use SHA-256, which BSI, ANSSI and the ECCG
    recommend." Texts about today's format also add that content identifiers use BLAKE3, which none of their
    guidance lists. Only if identifiers are called post-quantum, add "ANSSI and the ECCG recommend 384-bit
    hashes where post-quantum security is the goal".
  - classical half, when true: "RSA-PSS with a 4096-bit modulus and SHA-384".
- **Preconditions for the BSI section 5.3.4 phrase:**
  1. concatenation, valid only if every part verifies;
  2. hybrid key material dedicated to hybrid use;
  3. ML-DSA in the hedged, pure variant in every stack (V8);
  4. no identity key used for proof of presence, which holds by design (D16);
  5. bindings that are static and certificate-shaped, with a permitted use, a limited validity and revocation
     (D22).
- **Revocation** (D22): "A binding is accepted only with a status statement signed by its identity key and at
  most one hour old, with five minutes of clock tolerance. A revoked binding stops being accepted, in new and
  open connections, within 65 minutes. Compromise of an identity key itself is handled separately." Never an
  unqualified "revoked keys are rejected".
- **Blocks:** public text.

### D12 Separate TLS key and its binding, on the new fleet only

- **Answer:** every station instance has a TLS key used only for the TLS handshake, with a self-signed ML-DSA-87
  certificate. The identity key certifies it with a binding (key model). The station sends the binding in its
  challenge, and the client checks it against the leaf it verified before signing anything. This applies only on
  the post-quantum fleet.
- **Why:**
  - BSI asks for dedicated keys for hybrid signatures ✅, so in the EU profile the ML-DSA-87 key that signs alone
    inside TLS cannot also be part of the hybrid identity.
  - Signature keys and authentication keys stay apart (BSI Remark 6.1, ECCG Note 79, ANSSI PA-079 section 6.3) ✅.
    Remark 6.1 also asks that certificates indicate this; every binding names its key's `use`.
  - TLS stays identical in both profiles.
  - Public certificate authorities do not issue ML-DSA certificates ⚠.
- **Status:** accepted 2026-09-10.
- **Blocks:** WP 1.2, WP 1.3, WP 1.5, WP 1.6.

### D13 Where a verifier gets a signer's full public key

- **Answer:** a node's key travels to its neighbours when a connection opens (in the station's challenge and in
  CONNECT), is carried inside relayed and stored objects, and is never looked up.
- **Why:**
  - A post-quantum signature is already 4,627 B; carrying the 2,592 B key adds about 55% to an object that has
    already grown about 48-fold.
  - Checks stay local and deterministic, need nothing from the network, and the control plane does not depend on
    itself.
- **Not taken:** keys only per connection, which would leave relayed and stored objects without one; and looking
  keys up by node_id in the DHT.
- **Status:** accepted 2026-09-10.
- **Blocks:** WP 1.3 and everything after it.

### D14 A second post-quantum fleet

- **Answer:** the post-quantum fleet runs next to the live fleet, with its own station instances, seeds, station
  directory, DHT and realm deployment (D19), and hostnames distinct from the live fleet. The two fleets cannot
  reach each other. Each consumer moves over in its stage, and the live fleet is switched off after the last
  cutover.
- **Why:** with no classical fallback, switching the live fleet in place would cut off every consumer until its
  own stage.
- **Status:** accepted 2026-09-10.
- **Blocks:** Stages 3 to 6.

### D15 Profile order

- **Answer:** the US profile first in every stage, the EU profile right after.
- **Why:** the US profile needs no custom key exchange group (D3) and no classical half (D4, V8), so it is ready
  sooner. D3, D4, V8 and V15 trail it.
- **Status:** accepted 2026-09-10.

### D16 Connection handshake and client proof

- **Answer:** the connection handshake in the key model:
  - clients prove their identity with a CONNECT proof signed by a dedicated CONNECT key (hybrid in the EU
    profile), over a 256-bit station nonce, both node_ids and the hash of the station's leaf certificate;
  - the client always verifies the station's TLS handshake signature, and checks the station's binding before
    signing;
  - one client verification mode, and no dial without an expected identity;
  - no TLS session resumption;
  - any unexpected handshake frame closes the handshake.
- **Why:**
  - It works on every stack without TLS client certificates.
  - Keys stay separate: the identity key never proves presence.
  - The station's first message holds only precomputed material, a static binding and a random nonce, so a
    station makes no per-connection signature for anyone who merely connects.
- **Not taken:** TLS client certificates (mutual TLS).
- **Status:** accepted 2026-09-10. The frame layout is in `DESIGN_PQ_HANDSHAKE_FRAMES.md`, agreed by Mercury, Mars
  and Neptune on 2026-09-10 (WP 1.3).
- **Blocks:** WP 1.2 to WP 1.6, Stage 4.

### D17 Signatures between neighbours

- **Answer:** as in the key model. The US profile drops the neighbour signatures listed there. The EU profile
  keeps hybrid neighbour signatures on control frames. Origin signatures on relayed frames, publisher signatures
  and records always stay.
- **Revised and accepted by Raf on 2026-09-11:** caller frames in client_stream and bidi streams carry
  `{tbs, signature}` under `MACULA-PQ-CALLER-STREAM-V1`. The key is the caller's, from the verified STREAM_OPEN,
  and is not carried again. The signature covers the request hash and a sequence number, and the caller's stream
  end signs the last number, as provider frames do under D25. No station on the path can change, drop, reorder
  or add a caller's stream arguments. Cost: about 4.7 KB per caller frame in the US profile and 5.2 KB in the EU
  profile.
- **Why:** under the connection handshake a neighbour signature adds nothing in the US profile. In the EU profile
  it keeps classical-strength authenticity for membership, routing, subscriptions and advertisements if ML-DSA were
  broken, at a small cost because control frames are rare. Data stays hybrid end to end through publisher and
  origin signatures.
- **Measured cost:** ML-DSA-87 signs in 1.107 ms ✅, and a daemon's PUBLISH goes from two signatures to one.
- **Status:** accepted 2026-09-10; caller stream frames revised and accepted 2026-09-11.
- **Blocks:** WP 1.3, WP 1.6.

### D18 EU session proof

- **Answer:** the fixed binding. A per-session proof, a mutual hybrid signature over a TLS 1.3 exporter value
  (RFC 8446 section 7.5) with both node_ids as context, only when an offer needs its wording.
- **Why:** the per-session proof adds one sentence of public wording (D11), but needs an exporter in every EU
  stack. aioquic 1.3.0 ✅ and .NET QUIC ✅ have none; quinn has one ✅ that the NIF does not expose yet. No agency
  has assessed either construction.
- **If ever taken:** the CONNECT keys carry it, so no key is added. The exporter context order is initiator, then
  acceptor, with a red-first test that a reversed order is refused.
- **Status:** accepted 2026-09-10.

### D19 Realm on the post-quantum fleet

- **Answer, revised and accepted by Raf on 2026-09-10:** the realm name stays `io.macula`, so the realm id stays
  the same, on a separate realm deployment on the post-quantum fleet. `io.macula` runs the EU profile, `pq_hybrid`
  (D1).
- **Why:** programs keep the same realm id when they move over. The fleets cannot reach each other.
- **Consequence:** US-first work (D15) needs its own US-profile realm on the post-quantum fleet. Its name is open
  for Raf.
- **Status:** accepted 2026-09-10; revised the same day to give `io.macula` the EU profile.
- **Blocks:** WP 3.1, WP 3.2.

### D20 Branch and release

- **Answer:** `macula` develops the post-quantum work on the git branch `post-quantum`. `macula-station` and the
  post-quantum fleet build against that git ref instead of hex during development. Once proven, `macula` 11.0.0
  goes to hex, and only Raf publishes. Changes for the live fleet stay on `main`, the 10.x line.
- **Merging:** `post-quantum` takes `main` by merge commits after `main` releases, never by a rebase, because
  branches hang off it (agreed 2026-09-10).
- **Status:** accepted 2026-09-10.
- **Blocks:** Stage 1, WP 3.2, WP 3.4.

### D21 The live fleet during the work

- **Answer:** the live fleet is pinned to a released station version that matches exactly what its stations run.
  If `main` is ahead of the latest `v*` tag, the running image is pinned by digest, or Raf tags current `main`
  first. No station is downgraded. Owner: Terra.
- **Why:** a push to `macula-station` `main` builds and publishes the image the live stations follow ✅, so
  post-quantum work on the station would otherwise reach the live fleet.
- **Status:** accepted 2026-09-10.
- **Blocks:** merges in WP 1.6, WP 3.2.

### D22 Binding lifetime and revocation

- **Lifetime, accepted 2026-09-10:** a binding is valid for 7 days, and the node rotates to a new key and binding
  every 5 days.
- **Revocation, decided by Raf on 2026-09-10:** a stapled status statement. It replaces the first design (a signed
  revocation record, a local revocation set, and acceptance within the binding's validity when status cannot be
  obtained), which does not meet BSI's requirement that withheld status must not go unnoticed, nor ANSSI's
  hard-fail recommendation ✅ (V18):
  - for each binding, the identity key issues a statement carrying a label, the node_id, the SHA-384 of the
    binding, the issue time and the expiry, as a hybrid signature in the EU profile;
  - the station's challenge carries the statement for its TLS-key binding, and CONNECT carries the statement for
    the client's CONNECT-key binding;
  - statements are valid for 1 hour and reissued every 15 minutes, with 5 minutes of clock tolerance;
  - a missing, expired or future-dated statement is refused, which fails closed without anything from the network;
  - revoking a TLS or CONNECT key means issuing no more statements for its binding, then rotating.
- **Open connections:** each side sends a status frame with its fresh statement at every reissue. Each connection
  keeps the peer's current expiry and a timer at expiry plus 5 minutes. A refused statement, or no fresh one before
  the timer fires, closes the connection with a distinct reason, a diagnostic event and a notice to the owning
  process; calls in flight fail with the normal disconnect error, and a reconnect needs a full handshake with a
  fresh statement.
- **Consequences:**
  - the identity key signs a statement every 15 minutes, so it must be available on the node; an offline identity
    key is ruled out;
  - if a node's statement issuer stops, every peer drops that node within 65 minutes;
  - the verifier's clock is part of the security analysis: every fleet node runs chrony with NTS (RFC 8915) against
    at least two independent servers, and client-only nodes rely on their operating system's time synchronisation;
    a client-only node whose clock is set back can accept a revoked station binding for as long as its clock stays
    wrong; which NTS servers is open ⚠, and unassigned;
  - clock refusals have their own reasons, so a bad clock is visible;
  - a compromised identity key is not covered by statements: realm members go through realm revocation (WP 3.1),
    and stations are removed from seed lists and directory rows until D23 is decided.
- **Cost:**
  - a station issues 8 statements an hour, whatever its number of connections;
  - each handshake carries one more statement each way, about 4.7 KB in the US profile or 5.2 KB in the EU profile,
    and one verification;
  - each open connection receives one statement every 15 minutes; at 1,000 connections that is about 5 to 6 KB per
    second and about 0.3 ms of CPU per second.
- **Not taken:** capping connection lifetime at the statement window, which would redo the full post-quantum
  handshake on every connection every hour and break its streams, calls and subscriptions.
- **Blocks:** WP 1.3, WP 1.5, WP 1.6, and the revocation wording in D11.

### D23 Endorsement of stations

- **Question, open for Raf later:** should a station need endorsement by a realm or by a foundation before
  programs use it?
- **Facts:** `macula` has a foundation record type and verifier, with placeholder keys and no live custody ✅
  (`macula_foundation.erl`). Its design signs with FROST-Ed25519, which has no standardised post-quantum threshold
  equivalent ⚠; a post-quantum foundation list would need m-of-n independent ML-DSA signatures.
- **Relation:** endorsement answers whether to use a station, not which station a dial reaches (D16).

### D24 Hashes under signatures

- **Answer, accepted by Raf on 2026-09-10:**
  - **content ids and UCAN parent ids use SHA-384** in the post-quantum format;
  - **the post-quantum format has one hash tag for content ids, SHA-384:** an id with any other tag is refused on
    fetch, in manifests, for chunks and in announcements, and UCAN parent ids take no other hash, so no signer or
    sender can pick a weaker hash (narrowed to one tag on 2026-09-11);
  - **node ids stay SHA-256** (D5), under two rules: every verifier checks the full carried key (D13), and no
    trust is ever granted to a node id because of a property of one particular key;
  - **realm ids stay SHA-256**, under two rules: every trust decision about a realm checks the realm key, never
    the realm id alone, and no trust is ever granted to a realm id because of one particular name.
- **Where a signature covers a hash instead of the data** ✅:
  - a content announcement is a signed record whose payload names a content id, BLAKE3 by default today, and
    fetched content is checked against that id;
  - a UCAN's proof field names its parent tokens by an id that is the SHA-256 of the token.
- **Why:**
  - Where a signature authenticates data only through its id, collision resistance matters: whoever prepares the
    content, or an issuer who can make a collision, could swap the data behind a signed id. A signature is at most
    as strong as the collision strength of the hash it covers (SP 800-107r1 section 5.2, and FIPS 204 section 5.4
    for ML-DSA) ✅. SHA-256 gives 128 bits of collision strength and SHA-384 gives 192 (IR 8547 Table 7) ✅.
  - CNSA 2.0 lists SHA-384 and SHA-512 ✅. ANSSI recommends digests of at least 384 bits when post-quantum security
    is the goal (PG-083 RecoPQHachage, p.20), and asks that a hash before signing matches the signature's strength
    (p.36) ✅.
    BLAKE3 appears in none of the texts ✅.
  - Node ids rest on second-preimage resistance instead, as long as both rules hold: the high end of SHA-256's
    201 to 256 bits for a 3 KB input (D5).
  - Realm ids are like node ids, as Saturnus confirmed against the texts: a collision needs two names one party
    chose, which gains nothing, and taking over an existing realm id needs a second preimage. Third parties
    vouch for a realm by its key: the foundation's realm trust list names realm keys ✅.
  - DHT storage keys are covered by no signature; the record itself is checked. In the post-quantum format every
    storage key is SHA-256, a node_id or a SHA-256 derivation (WP 1.3), so the identifier qualifier in D11 holds.
- **Cost:** the content id's first byte becomes its hash tag, 2 for SHA-384, so a content id is 50 bytes, not 34;
  a UCAN parent id grows by the same 16 bytes before encoding; content hashing is slower than with BLAKE3, as local
  compute.
- **Public wording:** the identifier qualifier in D11 covers node, realm and DHT keys; the BLAKE3 clause drops from
  texts about the post-quantum format.
- **Blocks:** WP 1.3, WP 1.4, WP 1.6, WP 4.1.

### D25 A reply comes from the intended provider and answers the request

- **Answer, accepted by Raf on 2026-09-10:**
  1. **Provider advertisements travel signed by the provider.** The unit of advertisement gossip between stations is
     the provider's own signed procedure advertisement: procedure, realm, provider node_id, serving station, validity,
     and the provider's full key (D13), with the realm's provider authorization attached (item 6). Stations forward it
     unchanged; no station advertises in a provider's place. Routing follows the advertisement's serving station, not
     the station that gossiped it.
  2. **The caller signs the target.** The target is a mandatory field of CALL and STREAM_OPEN, covered by the caller's
     signature. A caller without a verified advertisement first obtains the provider's signed advertisement, then
     sends its request. No station sets or changes the target.
  3. **A reply names the request it answers.** RESULT, ERROR and STREAM_REPLY from a provider carry the SHA-384 of the
     canonical signed CALL or STREAM_OPEN inside their origin-signed fields. The caller compares it with its own
     request; stations keep forwarding state per origin connection and call id, and compare it too.
  4. **A reply must come from the target.** A provider reply is accepted only when responded_by equals the request's
     target and the origin signature verifies with a carried key that derives to that node_id. The caller and every
     station on the path check it.
  5. **Every provider stream frame is signed by the provider.** STREAM_DATA, STREAM_END, STREAM_ERROR and STREAM_REPLY
     from the provider carry a mandatory signer equal to the STREAM_OPEN's target.
     - The provider's first frame on a stream, whatever its type, carries the provider's full key, which must derive
       to the target. Later frames carry only the signature, checked against that key. A stream is the one place a
       key travels once rather than in every object (D13): the stream's own state holds it, and nothing is looked up.
     - Each signature covers the request hash and a frame sequence number, and STREAM_END signs the last number, so a
       frame cannot move to another stream, change place or go missing unnoticed.
     - A verifier that has not seen a stream's first provider frame refuses its later frames, and drops the key when
       the stream ends.
  6. **Only authorized providers can be targets.** A verifier accepts an advertisement as a target only if the realm's
     provider authorization verifies: the realm-signed org directory and the org-signed procedure delegation that
     names the provider, or the provider's service certificate chain to the realm CA with the org of the procedure
     name. The authorization travels with the advertisement, so no lookup sits in the check (D13). A caller may also
     pin the org key or the provider's node_id for a procedure.
  7. **Stations report transport failures only.** A station may sign an ERROR or STREAM_ERROR as reported_by only with
     a relay error code distinct from every provider result, and never a RESULT. A relay error means the outcome is
     unknown, not that the call failed. Providers deduplicate requests on caller and call id.
  8. **Advertisements expire, and only their provider withdraws them.** Each advertisement carries a signed validity,
     120 seconds today ✅, and verifiers refuse an expired one, with the clock tolerance of D22. Stations drop expired
     advertisements from gossip, and honour a withdrawal only under the provider's signature.
  9. **Retries.** A retry along another path keeps the call id and the target, so the provider's deduplication
     applies. Switching to another provider is a new, separately signed request, and the caller decides it, because
     the first request may already have run.
- **Answers to the design questions:**
  - **Who may provide a procedure:** the realm's provider authorization decides (item 6), and the caller, not a
    station, picks and signs the target among authorized providers (item 2). For procedures without an org
    namespace, such as the `_` namespace in use today ✅, there is no delegation to check, so any realm member is an
    authorized provider, and the binding proves only that the reply came from the node the caller chose. Whether such
    procedures need their own authorization is open for Raf.
  - **Replaying an old advertisement:** it fails once it has expired. While still valid it names its real provider and
    serving station, so a replay can only send requests towards that provider; it cannot make another node's reply
    acceptable, and a withdrawal counts only under the provider's signature.
  - **Several providers for one procedure:** item 9.
- **Signed fields:** a reply's origin-signed fields contain nothing that stations change hop by hop; a route that
  stations shorten per hop stays outside them.
- **Review:** one adversarial pass on 2026-09-10 found three required changes, all folded in: the caller signs the
  target (item 2), a reply names its request (item 3), and provider stream frames are signed (item 5).
- **Why:** origin signatures (D17) and carried keys (D13) prove who signed a reply; this decision ties that signer to
  the provider the caller meant, and the reply to the request the caller sent.
- **Cost, order of magnitude (estimate ⚠):**
  - **Advertisement gossip:** Frankfurt's view of the DHT holds 157 procedure advertisements today, for 117
    procedures from about 31 providers, each valid for 120 seconds ✅. A signed advertisement with the provider's key
    is about 7.4 KB in the US profile, and about 22 KB with the authorization attached. With 8 declared stations at up
    to 5 peer links each, about 20 links, re-gossiping every advertisement before it expires costs about 25 MB per
    120-second cycle without authorization and about 70 MB with it: 0.2 to 0.6 MB per second across the fleet, or
    10 to 30 KB per second per link. A 10-minute validity with signed withdrawals divides that by five.
  - **Replies:** 48 bytes more per reply for the request hash.
  - **Streams:** the provider's key travels once per stream, about 2.6 KB in the US profile and 3.1 KB in the EU
    profile. Every provider frame carries a signature, about 4.6 KB in the US profile and 5.1 KB in the EU profile, and
    costs the provider one signature. From signing alone a provider streams at most about 900 frames per second per
    core in the US profile and 188 in the EU profile; with 64 KB frames the signature adds 7 to 8 percent. A signed
    running hash at checkpoints would cost less, but leaves bytes unverified until the next checkpoint; it is not
    recommended unless a stream needs it.
  - **Callers:** one advertisement resolution per procedure, cached until it expires.
- **Blocks:** WP 1.3 (advertisement bundle, target and request hash fields, stream signer and sequence), WP 1.5
  (caller checks), WP 1.6 (station gossip, routing by serving station, reply and stream checks), Stage 4 (each SDK's
  caller checks).
- **Claim-gate tests:**
  - a reply signed by anyone other than the request's target is refused, at the caller and at the first station;
  - a reply whose request hash differs from the caller's request is refused;
  - a provider stream frame from another signer, out of sequence, or on a stream whose first provider frame was not
    seen is refused, and a stream whose STREAM_END does not sign the last sequence number fails;
  - an advertisement without valid provider authorization, or expired, is never a target;
  - a relay error never ends a call as failed.

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
| Endorsement of stations (D23) | Raf | later |

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
