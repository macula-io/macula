# Plan: post-quantum security for Macula, decisions

The full text of each decision of [PLAN_POST_QUANTUM_SECURITY.md](PLAN_POST_QUANTUM_SECURITY.md). The decisions
table there gives each decision's answer in short and its status.

### D1 Where the profile is chosen

- **Answer, accepted by Raf on 2026-09-10:** **One profile per realm.**
- **Why:** the realm is the trust domain. A US-profile peer and an EU-profile peer cannot verify each other's
  signatures: a `pq_pure` verifier takes ML-DSA-87 alone, and a `pq_hybrid` one takes the LAMPS composite, so a
  peer of the other profile is refused wherever a signature is checked. With D2, a realm's programs use the
  station instances of its profile.
- ⚠ **This rationale rested on key exchange until 2026-09-22**, when the profile stopped declaring a group (D3):
  both profiles now offer the same hybrids, so they share key exchange and are separated by signatures alone.
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
- **A PROFILE DECLARES NO KEY EXCHANGE GROUP, decided by Raf on 2026-09-22: "drop it".** The definition map
  carried a `key_exchange_group` that nothing read: `pq_hybrid` named the group every node offers anyway, and
  `pq_pure` named `mlkem1024`, a pure group nothing has ever offered, so the field claimed of one profile
  something that was never true, which is macula#15's shape. Both profiles use the groups `macula-pqc` offers,
  SecP384r1MLKEM1024 then SecP256r1MLKEM768. **Pure ML-KEM-1024 is added if someone needs CNSA 2.0 alignment, and
  adding it is ADDITIVE**: a further group offered beside the hybrids, so nothing breaks on the wire and a peer
  without it still agrees on a hybrid. Removed from `macula_crypto_profile` on 2026-09-22.
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
    each new ML-DSA-87 private key stored as its 32-byte seed next to its public key; a key already stored in its
    4,896-byte expanded form still loads and signs, so no node_id changes;
  - **each key serves exactly one purpose** (key model);
  - **on load, the public key of each ML-DSA-87 key is derived from its seed, or from its expanded form, and must
    equal the stored public key, and then every key is checked with a sign-and-verify round trip by the whole key,
    so a hybrid key signs only its composite;**
  - **TLS and CONNECT keys and their bindings rotate every 5 days** (D22);
  - D29 extends the TLS key beyond station instances: a node that accepts distribution tunnels also holds one.
- **Amended and accepted by Raf on 2026-09-22:** new ML-DSA-87 keys are stored as the 32-byte seed, not the
  expanded form, now that `macula-mldsa` signs every ML-DSA signature (D7, same date).
- **Why:** RFC 9964's `AKP` key stores the seed as `priv`, and Go 1.27 loads ML-DSA keys from the seed only, so seed
  key files cross Go and the BEAM ✅. `macula-mldsa` expands a seed, which OTP cannot, and signs from either form,
  so existing expanded keys keep their node_ids and no D25 grant is re-issued ✅. BSI asks for hybrid key
  material dedicated to hybrid signatures ✅; the ECCG list requires different key pairs for message
  signatures and authentication ✅; ANSSI PA-079 section 6.3 and BSI TR-03116-4 ask for separate keys per
  purpose ✅.
- **Blocks:** WP 1.3, WP 1.6.

### D7 Key encoding in UCANs and DIDs

- **Answer, accepted by Raf on 2026-09-10 with two checks, amended 2026-09-22:** **Use published names and
  constructions where they exist, and Macula's own names only where none exists. Every ML-DSA signature in the
  stack, UCAN included, is made and checked by `macula-mldsa` (the `macula-pqc` workspace). In `macula` it runs in
  `macula_crypto_nif`, and `macula_ucan` builds and parses tokens in Erlang and signs and verifies them through
  `macula_node_keys`, which also makes the composite's RSA-PSS half on OTP (built 2026-09-22; the Rust UCAN and DID
  NIFs are removed). `aud` names the audience by node_id.**
- **Amended and accepted by Raf on 2026-09-22,** three decisions:
  - **One ML-DSA everywhere:** `macula-mldsa`, verified byte-exact against NIST's ACVP vectors, signs and verifies
    ML-DSA in every Rust NIF and SDK, so private keys enter Rust. This replaces the rule, accepted on 2026-09-10,
    that UCAN and DID signing move into `macula_identity` on OTP `crypto` so that private keys never enter Rust.
    RSA-PSS, which `macula-mldsa` does not provide, stays on OTP.
  - **The EU hybrid is the standard LAMPS composite**, `id-MLDSA87-RSA4096-PSS-SHA512`, replacing Macula's own
    composite (see Check 1).
  - **New keys are stored as seeds** (D6, same date).
  - **Why:** OTP signs ML-DSA without a context string, which is the only reason Macula needed its own composite,
    and it cannot expand a seed; `macula-mldsa` does both. One implementation on every stack also means one set of
    vectors, one timing harness and one wiping test stand behind every ML-DSA signature.
- **Why:**
  - Every verifier is Macula's own code, and membership UCANs use a raw hex key as `aud` ⚠ (V12).
  - `macula-mldsa` makes and checks every ML-DSA signature, on every stack (amendment of 2026-09-22).
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
    string ✅. `macula-mldsa` takes one.
  - **Accepted by Raf on 2026-09-22, replacing Macula's own composite of 2026-09-10:** every EU hybrid signature
    that D4 names is `id-MLDSA87-RSA4096-PSS-SHA512` as the LAMPS draft defines it: ML-DSA-87 signs M' with the
    draft's label for that algorithm as its context string, RSA-PSS signs M' on OTP with D4's parameters, and keys
    and signatures are concatenated, valid only if both verify. It is proven against the draft's own test vectors
    before any node signs with it. Its JOSE `alg` stays `ML-DSA-87-PS384`, after the JOSE composite draft's pattern
    (`ML-DSA-87-ES384`) and JOSE's `PS384`, while JOSE has no name for an ML-DSA and RSA composite. EU keys in
    `did:key` use Macula's own key type until a multicodec exists ⚠.
  - **Constants** (set in WP 1.3, 2026-09-10; the label amended 2026-09-22): Prefix is the 32 ASCII bytes
    `CompositeAlgorithmSignatures2025`, as in LAMPS; Label is the draft's label for
    `id-MLDSA87-RSA4096-PSS-SHA512`, taken from the draft and confirmed by its test vectors, and ML-DSA-87 signs
    with it as its context string; ctx is empty for every Macula object, so len(ctx) is the single byte 0, and each
    object keeps its own domain label inside M; PH is SHA-512. RSA-PSS signs
    M' with SHA-384, MGF1 with SHA-384, a 48-byte salt and public exponent 65537. The signature is the 4,627-byte
    ML-DSA-87 signature followed by the 512-byte RSA-PSS signature, and the public key is the 2,592-byte ML-DSA-87
    key followed by the 526-byte DER `RSAPublicKey`, both without length prefixes.
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
  - **How a chain travels, decided by the Supervisor on 2026-09-22** (a wire detail inside decisions Raf has made):
    CALL and STREAM_OPEN carry a chain's proof tokens in a separate field `proofs`, inside the part the caller
    signs, next to `token`. `proofs` is an unordered set of JWTs: each proof is found by the SHA-384 content id a
    child's `prf` names (D24), never by position. Every proof must be referenced from the chain, so an unreferenced
    one is `malformed_frame`; the decoding rule bounds the count and the total bytes, with vectors in
    `test/vectors/decoding_rule_v1.json`. A request without delegation carries no `proofs` field, byte-identical
    to one without the rule.

  - **What a `prf` entry holds, and what a capability names, decided by the Supervisor on 2026-09-22** (wire
    details inside the decisions above; five SDK work packages copy them):
    - A `prf` entry is the **lowercase hex of the SHA-384 of the parent token's compact JWT bytes exactly as they
      travel**, the three base64url parts and their dots, never a re-encoded token. Hex, because `aud` is already
      a node_id in lowercase hex, so one convention covers both, and 96 characters against base64url's 64 is
      nothing on a token of 11 to 16 KB. A token names at most one parent: a chain, not a graph.
    - A capability's `with` is an MRI: a realm grant is `mri:realm:<realm>`, an org grant
      `mri:org:<realm>/<org>`, and a procedure grant `mri:proc:<realm>/<procedure>`, where `<procedure>` is the
      full procedure name and its org is the text before its first `/`
      (`DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md`, computed by `macula_record:procedure_org/1`). Anything else in
      `with` is refused.
    - **A REALM ID IS SHA-256 OVER ITS NORMALISED REALM NAME, and a grant names the realm by that name**
      (decided by the Supervisor on 2026-09-22, from three options Venus put). A verifier hashes the grant's name
      and compares it with the request's 32-byte realm id, so a grant is checked against a request with nothing
      looked up, which is what D13 asks of every check, and the readable name the MRI exists for is kept. The
      realm service mints ids this way from 12.0.0 (WP 3.1): a realm's id is no longer free to choose, and a
      rename is a new realm. 12 is a fresh fleet (D14), so no id migrates. Until now `macula_realm:id/1` in the
      SDK and the realm service's stored id were two different things under one name.
    - **The normalised form of a realm name**, load-bearing because the id is a hash of exactly those bytes: at
      least one segment; segments separated by single `.`; each segment non-empty and made of `a-z`, `0-9`, `-`
      or `_`. No uppercase, no leading or trailing `.`, no empty segment. A verifier compares bytes and never
      folds case: `io.example` and `IO.Example` are two realms, and only the first is a name at all.
      `macula_realm:validate/1` accepts uppercase, by its own tests, so the token check states its own form: a
      grant outside it is refused as `realm_name_not_canonical`, and a grant whose name does not hash to the
      request's realm id is refused as `wrong_realm`.
    - **The all-zero realm is the unnamed realm: no name hashes to it, so no capability can name it and no
      delegated grant covers a request in it.** A gated procedure lives in a named realm; the all-zero realm
      stays what it is today, the default of an ungated call. Vectors: a grant whose name hashes to the
      request's realm id, one that does not, one differing only by case, and one against an all-zero request
      realm.
    - **The narrowing matrix**, one row per case, each with a vector. A grant covers a request when the row says
      yes, and a child's grant is accepted only when the parent's grant covers it by the same table:

      | Grant in `with` | Request or child grant | Covered |
      |---|---|---|
      | `mri:realm:R` | anything in realm `R` | yes |
      | `mri:realm:R` | anything in another realm | no, `wrong_realm` |
      | `mri:org:R/O` | `mri:proc:R/O/p`, any procedure whose org is `O` | yes |
      | `mri:org:R/O` | `mri:proc:R/X/p`, a procedure of another org | no, `grants_more_than_proof` |
      | `mri:org:R/O` | `mri:realm:R` | no, `grants_more_than_proof` |
      | `mri:org:R/O` | `mri:org:R/O` | yes |
      | `mri:proc:R/O/p` | the same procedure name, same realm | yes |
      | `mri:proc:R/O/p` | another procedure name | no, `grants_more_than_proof` |
      | `mri:proc:R/O/p` | the same name in another realm | no, `wrong_realm` |
      | any | a procedure name with no org namespace | no, `procedure_without_org` |

      `can` is equal at every step (`can_changed` otherwise), and every token in a chain names the same realm.
      Issuer scope needs no separate rule inside a chain: a key's authority is what it was granted, so an org
      key cannot issue a realm grant or reach another org, both by the table. Only the chain's root grants
      freely, and the root must be the issuer the policy names (`not_the_issuer`), which for a membership policy
      is the realm key.

  - **Amended and accepted by Raf on 2026-09-14,** two rules for the capability check of step 4:
    - **Narrowing along a chain.** Every token in a chain names the same realm. A realm grant covers realm, org and
      procedure grants in that realm, an org grant covers that org and its procedures, and a procedure grant covers
      only that procedure. Each token's capability is covered by a capability of the token it proves from, and
      `can` is equal at every step.
    - **Issuer scope.** An org key, authorized through the realm-signed org directory (D25 item 6), grants only org
      or procedure capabilities inside its own org. Only the realm key grants a realm capability.
    - A grant's org is the org namespace of the procedure name, as `DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md` defines
      it for provider authorization, so the caller check and the provider check agree on who owns a name. `macula`
      10.x uses the same definition (Jupiter, 2026-09-14).
    - **Why:** without narrowing, a delegate could grant more than it was given; without issuer scope, a key trusted
      for one org could grant a whole realm. With both, authority follows the chain and the org directory.
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
  - ⛔ **`macula-dist-relay` does not move. Raf, 2026-09-23: "park it, pin it to 11.x".** It consumes
    `macula_tls:quic_server_opts/0`, which 12 deletes, so it does not build against 12 and is not being ported. It
    stays on `macula ~> 11.x` and is not a 12 consumer. Distribution over the mesh is a novelty rather than a
    product, so the port would buy nothing;
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
  working against today's stations until the 11.0.0 deploy replaces them (D14), then stop until msquic supports
  post-quantum key exchange.
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
  - that Macula's hybrid signature is the LAMPS composite or `id-MLDSA87-RSA4096-PSS-SHA512`, until a release that
    signs with it ships: `macula` main signs it and passes the draft's own vector, while every released version
    signs Macula's own composite (D7). Saturnus reads any new wording.
- **US, when true:** "algorithms aligned with CNSA 2.0 (ML-KEM-1024, ML-DSA-87, AES-256, SHA-384)". Never imply
  deployability in National Security Systems, which also needs NIAP or NSA validation ⚠ (V14). Preconditions: V13
  and V14 closed.
  - ⚠ **What that claim rests on, after D3's 2026-09-22 ruling:** ML-KEM-1024 is used INSIDE
    SecP384r1MLKEM1024, never alone, on every link in both profiles. The claim names algorithms and stays true;
    wording that implies a pure ML-KEM key exchange, or that a profile selects one, does not. Saturnus reads any
    public wording, as this decision already requires.
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

### D12 Separate TLS key and its binding

- **Answer:** every station instance has a TLS key used only for the TLS handshake, with a self-signed ML-DSA-87
  certificate. The identity key certifies it with a binding (key model). The station sends the binding in its
  challenge, and the client checks it against the leaf it verified before signing anything. This applies to every
  11.0.0 station instance.
- **Why:**
  - BSI asks for dedicated keys for hybrid signatures ✅, so in the EU profile the ML-DSA-87 key that signs alone
    inside TLS cannot also be part of the hybrid identity.
  - Signature keys and authentication keys stay apart (BSI Remark 6.1, ECCG Note 79, ANSSI PA-079 section 6.3) ✅.
    Remark 6.1 also asks that certificates indicate this; every binding names its key's `use`.
  - TLS stays identical in both profiles.
  - Public certificate authorities do not issue ML-DSA certificates ⚠.
- **Status:** accepted 2026-09-10.
- **Built in `macula` on 2026-09-22 (Venus):** a listener presents a self-signed ML-DSA-87 certificate made from
  its TLS key's seed (`macula_quic:generate_self_signed_cert/2`, through `macula-pqc` 0.2), and a dial verifies the
  listener's TLS 1.3 handshake signature under that certificate's key and nothing else (`macula-pqc`'s
  `KeyPossessionVerifier`). A classical certificate fails in either role. The identity key's binding over the TLS
  key travels in the station's challenge and the client checks it against the leaf THIS TLS handshake received
  (`macula_handshake:station_binding/1` against `macula_quic:peer_leaf/1`, since 2026-09-11), and the CONNECT proof
  covers that leaf, so a station dial is bound end to end.
- ⚠ **Except the two distribution dials**, which speak Erlang distribution over the `macula-dist` and
  `macula-dist-relay` ALPNs and never run the connection handshake. They verified a webpki chain by default until
  2026-09-22 and now verify key possession alone, which names nobody: an interim REGRESSION for that path, closed
  by WP 1.5's tunnels and D29. Nothing else on the mesh uses those ALPNs.
- **Raf, 2026-09-22: refuse unless switched on.** A node refuses to carry distribution over QUIC in `direct` or
  `dist_relay` mode unless `MACULA_DIST_UNIDENTIFIED_PEER=accept` is set, exactly that value, and the refusal
  names the limit and the setting (`macula_dist:listen/1`, `macula:join_dist_relay/1`). `relay` mode, over the
  station mesh, is not gated. The setting goes when the tunnel lands.
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

### D14 How the switch happens

- **Answer, reconsidered by Raf on 2026-09-16:** back to the 2026-09-10 answer — a second post-quantum fleet next
  to the live one, with its own station instances, seeds, station directory, DHT, realm deployment and hostnames,
  each consumer moving over in its stage and the live fleet switched off after the last cutover. The in-place
  answer is dropped.
- **Why:** the in-place answer folded every remaining stage into one blocking coordination tail: all consumers,
  the realm, the directory and every service ready and green on the same day, with no way to ship anything in
  between. The cost it avoided — new spend, or two station instances per box — is worth paying for a staggable
  migration, where the SDK and each consumer move to the post-quantum fleet when ready without waiting on any
  other consumer (Raf, 2026-09-16).
- **Superseded:** the answer accepted on 2026-09-15 (in place, all consumers at once).
- **Status:** accepted 2026-09-10; superseded by the in-place answer on 2026-09-15; reconsidered 2026-09-16 back
  to the 2026-09-10 answer.
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
- **Revised and accepted by Raf on 2026-09-11:** every frame a caller originates after STREAM_OPEN carries
  `{tbs, signature}` under `MACULA-PQ-CALLER-STREAM-V1`, in every stream mode: data, end and abort frames in
  client_stream and bidi, and end and abort frames in server_stream. The key is the caller's, from the verified
  STREAM_OPEN, and is not carried again. The signature covers the request hash and a sequence number, and the
  caller's stream end signs the last number, as provider frames do under D25. No station on the path can change,
  drop, reorder or add a caller's stream frames. Cost: about 4.7 KB per caller frame in the US profile and 5.2 KB in
  the EU profile.
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

### D19 Realm in 11.0.0

- **Answer, revised and accepted by Raf on 2026-09-10:** the realm name stays `io.macula`, so the realm id stays
  the same. `io.macula` runs the EU profile, `pq_hybrid` (D1). With D14 reconsidered (2026-09-16), the realm
  deploys as a separate deployment on the second post-quantum fleet again, in its stage, instead of moving in the
  same deploy.
- **Why:** programs keep the same realm id when they move over.
- **Consequence:** US-first work (D15) needs its own US-profile realm. Its name is open for Raf.
- **Status:** accepted 2026-09-10; revised the same day to give `io.macula` the EU profile, on 2026-09-15 to move
  the deployment in place (D14), and on 2026-09-16 back to a separate deployment when D14 was reconsidered.
- **Blocks:** WP 3.1, WP 3.2.

### D20 Branch and release

- **Answer, revised on 2026-09-15:** the `post-quantum` branch merges into `main` in one merge commit, and `macula`
  11.0.0 is developed on `main` from then on. There is no 10.x line and no intermediate release. `macula` 11.0.0 on
  hex is the release that opens the post-quantum fleet to consumers, and only Raf publishes. The live fleet is
  switched off after the last consumer's cutover (D14, reconsidered 2026-09-16), so the hex release no longer
  triggers a same-day fleet change. No repository commits a git or branch
  dependency on `macula`: development builds against a local checkout, and consumers move to `~> 11.0` from hex
  only after Raf publishes.
- **Merging, until that merge:** `post-quantum` took `main` by merge commits, never by a rebase, because branches
  hung off it (agreed 2026-09-10).
- **Status:** accepted 2026-09-10; revised on 2026-09-15 to one target, `macula` 11.0.0 on `main` (Raf).
- **Blocks:** Stage 1, WP 3.2, WP 3.4.

### D21 The live fleet during the work

- **Answer:** the live fleet, today's 9 stations (cut to 4 on 2026-09-16 when the five Linode nanodes were
  decommissioned, then to 2 at the 2+2 reprovisioning — live frankfurt+falkenstein, post-quantum nuremberg
  +helsinki, decided 2026-09-16, `macula-demo` `plans/PLAN_MACULA_11_DEPLOY.md`), is pinned to a released
  station version that matches exactly what its stations run, `macula-station` 311c0bf, until the last
  consumer has moved to the post-quantum fleet and the live fleet is switched off (D14, reconsidered
  2026-09-16). If `main` is ahead of the latest `v*` tag, the running image is pinned by digest, or Raf tags
  current `main` first. No station is downgraded. Owner: Terra.
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
    and stations are removed from seed lists and directory rows.
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

- **Question:** should a station need endorsement by a realm or by a foundation before programs use it?
- **Answer, accepted by Raf on 2026-09-11:** No endorsement is needed to use a station. If endorsement comes later, it
  is a separate signed record about a station, checked by the node that picks a station, under its own policy.
  Handshake frames carry no endorsement. Stations keep and serve verified records of any type until they expire,
  within the per-slot and per-class bounds (D28).
- **Refined on 2026-09-11, accepted by Raf:** the answer gained "within the per-slot and per-class bounds".
- Such a record fits a domain type (0x20 to 0xFF) with the station's node_id as its subject, so 11.0.0 needs no new
  field or tag for it.
- A node_id stays the same across the 5-day rotation, because only TLS and CONNECT keys rotate (D6), so an
  endorsement survives rotation.
- **Facts:** `macula` has a foundation record type and verifier, with placeholder keys and no live custody ✅
  (`macula_foundation.erl`). Its design signs with FROST-Ed25519, which has no standardised post-quantum threshold
  equivalent ⚠; a post-quantum foundation list would need m-of-n independent ML-DSA signatures.
- A foundation record carries one foundation signature in the signed-object format.
- **Relation:** endorsement answers whether to use a station, not which station a dial reaches (D16).
- ⚠ **Amended by Raf on 2026-09-22 (D31):** "Handshake frames carry no endorsement" no longer holds without
  qualification. A handshake frame MAY carry a realm MEMBERSHIP endorsement, about the connecting node and not about
  the station, and a station checks it only when `invite_only` is on, which it is not by default. The rest of D23
  stands: no endorsement is needed to USE a station, and station endorsement, if it ever comes, stays a separate
  record checked by the node that picks a station.

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
     - Revised 2026-09-15: the certificate form is gone. The realm issues no X.509 certificates (design B1), so only
       the org directory and the delegation authorize a provider, and a verifier refuses any other form as
       `authorization_form_unsupported`.
  7. **Stations report transport failures only.** A station may sign an ERROR or STREAM_ERROR as reported_by only with
     a relay error code distinct from every provider result, and never a RESULT. A relay error means the outcome is
     unknown, not that the call failed. Providers deduplicate requests on caller and call id.
  8. **Advertisements expire, and only their provider withdraws them.** Each advertisement carries a signed validity
     of at most 300 seconds, renewed at half that or sooner, and verifiers refuse an expired or longer one, with the
     clock tolerance of D22. Stations drop expired advertisements from gossip, and honour a withdrawal only under the
     provider's signature.
  9. **Retries.** A retry along another path keeps the call id and the target, so the provider's deduplication
     applies. Switching to another provider is a new, separately signed request, and the caller decides it, because
     the first request may already have run.
- **Answers to the design questions:**
  - **Who may provide a procedure:** the realm's provider authorization decides (item 6), and the caller, not a

    station, picks and signs the target among authorized providers (item 2). In 11.0.0 every procedure has an org
    namespace, so every provider carries an authorization (next answer).
  - **Procedures without an org namespace, decided by Raf on 2026-09-14:** `macula` 11.0.0 requires an org namespace
    on every procedure. A name whose first segment is `_`, a name without `/`, and a name that starts with `/` are
    refused when a procedure is advertised and by every authorization policy.
    - **Why:** every provider then carries a provider authorization (item 6), every capability grant has an org that
      owns it (D7), and the provider check and the caller check share one definition of who owns a name. Not taken:
      the realm owning `_` and signing its procedure delegations, which keeps today's names but makes the realm key a
      signer in regular use.
    - **Consequences:** the procedures in use without an org namespace are listed and renamed before 11.0.0, and
      every publisher gets an org directory entry (WP 3.1, WP 6.1, open items).
    station, picks and signs the target among authorized providers (item 2). In 11.0.0 every procedure has an org
    namespace, so every provider carries an authorization (next answer).
  - **Procedures without an org namespace, decided by Raf on 2026-09-14:** `macula` 11.0.0 requires an org namespace
    on every procedure. A name whose first segment is `_`, a name without `/`, and a name that starts with `/` are
    refused when a procedure is advertised and by every authorization policy.
    - **Why:** every provider then carries a provider authorization (item 6), every capability grant has an org that
      owns it (D7), and the provider check and the caller check share one definition of who owns a name. Under the
      earlier rule, any realm member could provide a procedure without an org namespace, and the reply binding
      proved only that the reply came from the node the caller chose. Not taken: the realm owning `_` and signing
      its procedure delegations, which keeps today's names but makes the realm key a signer in regular use.
    - **Consequences:** the procedures in use without an org namespace are listed and renamed before 11.0.0, and
      every publisher gets an org directory entry (WP 3.1, WP 6.1, open items).
  - **Hecate services, accepted by Raf on 2026-09-11:** in 11.0.0 every hecate service procedure has the org
    namespace `hecate`, as `DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md` defines it, with a procedure delegation per
    service (WP 6.1).
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
  caller checks), WP 1.4 and WP 3.1 (an org namespace on every procedure), WP 6.1 (renames).
- **Claim-gate tests:**
  - a reply signed by anyone other than the request's target is refused, at the caller and at the first station;
  - a reply whose request hash differs from the caller's request is refused;
  - a provider stream frame from another signer, out of sequence, or on a stream whose first provider frame was not
    seen is refused, and a stream whose STREAM_END does not sign the last sequence number fails;
  - an advertisement without valid provider authorization, or expired, is never a target;
  - an advertisement, or a request, for a procedure without an org namespace is refused;
  - a relay error never ends a call as failed.

### D26 One key form for peer-supplied maps

- **Answer, accepted by Raf on 2026-09-11:** in `macula` 11.0.0, every map a peer supplies as application data, at
  any depth, is delivered in one form: text as `{text, Bin}`, byte strings as binaries, integers, floats and lists as
  decoded, null as `undefined`, and no atoms. The fields a frame type defines decode to atoms through a fixed table,
  never from peer input. Handlers read fields through `macula:field/2`, `macula:field/3` and `macula:text/1`.
- **Why:** a reader sees the same map on a node's first call as on every later one, whatever the node has loaded.
- **Cost:** handlers that match atom or binary keys in payloads change, which is accepted for 11.0.0.
- **Details:** `DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md`, Peer-supplied maps.
- **Blocks:** WP 1.3 (the codec and the facade accessors, with `macula_manifest:from_wire/1` and the distribution
  pool reading through them), WP 1.6 (the station's record fan-out, DHT handlers and content handlers), WP 6.1
  (every hecate service handler that reads payload fields).

### D27 Where content lives

- **Answer, accepted by Raf on 2026-09-11:** a station does not keep content. The node that shares content keeps it
  and serves it, and stations only pass content through. When efficiency needs it, a station may hold traffic
  temporarily, only encrypted so that the station cannot read it, with the key held only by the endpoints, and
  bounded in time and size.
- **Consequence:** content is available while the sharing node is online.
- **Not decided:** a storage service that keeps content beyond the sender, outside stations, is a possible later item.
- **Blocks:** WP 1.3 (sharing and fetching content), WP 1.6 (the station's content store is removed), WP 2.1
  (content probes), WP 4.1, WP 4.2 and WP 4.3 (fetching content), WP 5.1 (`mesh_put` and `mesh_get`) and WP 6.1
  (services that share content).

### D28 Slot bounds, slot admission and the verification budget

- **Question:** how much does a DHT slot hold, who gets a place when it is full, and what does reading from a peer whose
  objects keep failing cost a node?
- **Answer, accepted by Raf on 2026-09-11:** the rules of
  `DESIGN_PQ_DHT_SLOTS_AND_BUDGET.md`:
  - a slot that signers share holds 64 entries, one per signer; a record from a new signer that finds no free place is
    not stored, and nothing held is evicted for it; built-in and domain types each have a station total; VALUE
    answers in pages of at most 256 KiB;
  - a slot a station can check through its realm trust list keeps 64 places for checked signers and 16 for everyone
    else; an advertisement authorized other than by the org directory and delegation pair gets no place, since the
    certificate-chain form is removed; a station never fetches anything during a STORE or shows callers which place an
    entry holds;
  - each connection has a budget of 32 tokens, refilled at 1 per second, spent only on refusals that every verifier
    reaches from the same bytes and on allowances passed; an empty budget pauses reading from that connection, from
    250 ms up to 4 seconds, and never closes it;
  - a domain record expires at most 7 days after its `created_at`, a rule of the record format that every verifier
    checks.
- **Waiting on:** receive-side flow control per stream, for the pause (Raf decides, Neptune builds); live foundation
  keys and a published realm trust list, before slot admission can check any slot (Raf).
- **Blocks:** WP 1.2 (credit per stream, receive windows), WP 1.3 (record rules, frame fields, the Plumtree and
  HyParView allowances, put pacing), WP 1.5 (the budget and the pause), WP 1.6 (slots, admission, paging,
  replication, the STORE allowance), WP 2.2 and Stage 4.

### D29 Distribution tunnel keys

- **Question:** which keys protect a distribution tunnel's data across the mesh, when an identity key never signs
  anything fresh to a session and every key serves one purpose (D6)?
- **Answer, decided by Raf on 2026-09-12:**
  - a distribution tunnel carried over the mesh runs the connection handshake end to end inside the tunnel;
  - the node that accepts the tunnel takes the station role, with its TLS key, certificate and TLS-key binding;
  - the dialing node sends CONNECT with its CONNECT key, against the peer's expected node_id;
  - tunnel data travels only in that TLS 1.3 session, under the profile both endpoints use (D2);
  - the carrier delivers the tunnel's data complete and in order, or the tunnel ends, on both carriers: the relay,
    and the pool path that carries the data as pubsub messages through stations.
- **Why:** it keeps one purpose per key (D6) and the key model unchanged, reuses the handshake WP 1.5 specifies,
  and takes per-direction keys, replay detection and rekeying from TLS 1.3.
- ✅ **The fallback is not needed, measured 2026-09-23 (Venus).** V19 showed OTP 28.4.2 completing a TLS 1.3
  handshake with an ML-DSA-87 certificate, but on a key made by OpenSSL, and OTP's own generated ML-DSA keys cannot
  sign. The open question was macula's own key. OTP signs a CertificateVerify with **the certificate and key
  `macula_quic:generate_self_signed_cert/2` returns**, the private key in RFC 9881's seed form, and carries data
  both ways. Not measured there: the negotiated group, which V19 covered; the dialling side accepts the leaf
  through a `verify_fun`, which is what the key model's client check does anyway, since nothing issues an
  ML-DSA-87 certificate for a chain to end at. Nothing is built toward the fallback.
- **Blocks:** WP 1.5 (distribution tunnels), WP 3.4.

### D30 Node_id puzzle difficulty

- **Question:** how many leading zero bits must a node_id's puzzle evidence have in `macula` 11.0.0, and what changes
  in 10.x?
- **Answer, decided by Raf on 2026-09-12:**
  - in 11.0.0, one difficulty for the whole fleet: 12 leading zero bits on the node_id (D5), a constant in every stack
    as D5's constants are, not a setting and not a `foundation_parameter` record;
  - identity key generation grinds to it once per identity; the TLS and CONNECT keys that rotate are not part of the
    node_id (D5), so rotation grinds nothing;
  - the rollout is the plan's own: station instances start in `log_only`, and the fleet switches to `enforce` in
    WP 4.5;
  - D30 applies to 11.0.0; node_ids made under 10.x do not change.
- **Why:**
  - the puzzle adds a fixed cost to each identity; slot admission (D28) decides which signers take places in the DHT;
  - on one core of this machine (OTP 28.4.2, OpenSSL 3.6.4), ML-DSA-87 key generation with the D5 node_id takes about
    0.28 ms, so 12 bits takes about 1.1 s per identity on average and 16 bits about 18 s ✅ (PART1);
  - one constant keeps `puzzle_invalid` something a client can check for itself (`DESIGN_PQ_HANDSHAKE_FRAMES.md`);
  - a `foundation_parameter` record would depend on live foundation keys, which D28 still waits on;
  - raising the difficulty in 10.x would invalidate most existing node_ids, and with them pinned seeds, DHT records
    and UCANs (D14).
- **Waiting on:** V20, before 12 is final. If generation at 12 bits is too slow on the slowest supported client
  device, the value goes back to Raf.
- **Blocks:** WP 4.5 (`enforce`).

### D31 Invite-only: membership checked at CONNECT, off by default

- **Question:** should a station accept a connection only from a node that was invited to a realm, and how is that
  turned on and off without breaking the wire?
- **Answer, decided by Raf on 2026-09-22:**
  - A station may require the connecting node to present a realm membership endorsement in CONNECT. It is checked in
    the station's CONNECT check, before HELLO.
  - ⛔ **Order inside the CONNECT check, and it is load-bearing: the endorsement is checked AFTER the CONNECT proof
    verifies, never beside the puzzle check.** The puzzle check runs deliberately before any signature, as a cheap
    filter. The endorsement cannot: `verify_endorsement/3` binds `member_node` to the client's node_id, and only the
    proof establishes that the connecting node holds that node_id. Checked earlier it would be matching a claim
    against a name nothing has proved, and would admit anyone who copied a member's endorsement. So: puzzle (cheap,
    pre-signature), then the CONNECT proof, then the endorsement. Caught by Venus on 2026-09-22 against an earlier
    wording of this decision that said "beside the puzzle check".
  - **`invite_only` is a station setting with the same three values the puzzle check already uses:** `off`,
    `log_only`, `enforce`. `enforce` refuses with `not_invited`; `log_only` admits and reports; `off` does not look
    at the field.
  - ⛔ **`off` is the default, and unset means open.** Turning it on is a deliberate per-deployment setting, on the
    station for enforcement and on the realm for admission.
  - **The `connect` frame always carries `member_endorsement`**, empty when the node has none, so changing the
    setting never changes the wire. It is NOT covered by the CONNECT proof, which signs a fixed-length
    concatenation, so adding the field does not alter the proof and WP 1.5 can ship before it.
  - **The endorsement is the one that already exists:** `realm_member_endorsement`, record type `0x05`, verified by
    `macula_hyparview_endorsement:verify_endorsement/3` against `#{profile, realm, realm_key_id}`. Nothing calls that
    function today. No new record type, no new verifier.
  - **Binding to the connection is free.** The verifier checks `member_node` against the node_id the CONNECT proof
    establishes, so an endorsement copied from another member is useless without that member's CONNECT key.
  - ⛔ **Admins always have access (Raf, 2026-09-22), and that is an invariant, not a default.** No setting, missing
    trust list, member list or expired endorsement may lock a realm admin out. An admin is a realm member
    endorsement carrying an admin role (`verify_endorsement/3` already returns the endorsed roles), issued by the
    realm from its own event store, where admins are appointed and dismissed
    (`realm_admin_appointed_v1` / `realm_admin_dismissed_v1`, projected to `realm_admins`). **A station pins its home
    realm's key id in local configuration**, so it verifies an admin endorsement on its own, without the published
    trust list D28 still waits on. There is NO station-local admin list: that would be a second source of truth that
    drifts from the realm's.
  - **Refusals say what is wrong without naming who may pass.** A node refused while the trust list is unpublished
    gets a reason that names the missing trust list; it never reveals the admins.
- **Why:**
  - It makes the mesh invitational without a second membership mechanism: the record, the verifier and the roles are
    already built, and the endorsement is bound to the connecting node by a check that already exists.
  - Default off, and a field that is always on the wire, mean the switch is a deployment decision rather than a wire
    version. The wire breaks once, in 12.0.0, not twice.
  - The pinned realm key is what keeps the admin invariant true at the moment it matters most, which is a station
    switched on before its realm's trust list exists.
- **Amends D23**, which said handshake frames carry no endorsement. See the note there.
- **The removal bound, decided by Raf on 2026-09-22: a long endorsement, and withdrawal checked after admitting.**
  An endorsement is valid for at most 24 hours and is reissued at half its life. **The expiry is not the removal
  bound.** The station looks up the member's endorsement slot AFTER admitting the connection, never during the
  handshake, and drops the connection when the realm key's tombstone says `withdrawn`; `slot_endorsement/4` already
  returns exactly that, and a tombstone outlives the endorsement's own expiry. The bound is how soon that lookup
  runs: **within 60 seconds of admitting, and every 15 minutes on an open connection thereafter**, which is the
  cadence D22 already uses for status on open connections. A removed member therefore keeps a connection for at most
  that interval, and cannot make a new one once the endorsement expires.
  - **Why not D22's 1-hour, 15-minute cadence for the endorsement itself**, which would have given a 65-minute bound
    in D22's own words: the endorsement is issued by the REALM, not by the node's own identity key, so that cadence
    would make every member refresh from the realm every 15 minutes and turn the realm's availability into every
    member's availability. That is the failure the fleet already has with `org_directory`, where a realm roll left
    services unable to advertise for up to two hours.
  - The 30-day cap in `macula_record:max_endorsement_window_ms/0` stays as the record layer's outer limit; 24 hours
    is this use's limit and the issuer's rule.
- **Consequences when `enforce` is on:** the all-zero realm demo path stops working, and with it `macula-mcp` device
  auto-join out of the box, the `mcl-*` services and `macula-e2e`, each of which needs an invite first. The other
  SDKs follow in tier 2.
- **A pinned home realm key, accepted by Raf on 2026-09-22, against Sprint A.** Sprint A (2026-04-15) reversed
  per-realm state on the station: stations are realm-agnostic infrastructure. This is a deliberate, narrow exception
  to that, put to Raf as such: ONE public key, used only to recognise admins, and no realm state, views or
  membership on the station. Without it a station switched on before the trust list exists can verify nobody,
  including its own admins, and the admin invariant above would not hold.
- ⚠ **Open: rotating the pinned key.** If a realm key is compromised or rotated, every station's pin moves. How it
  moves, and whether a station holding a stale pin admits admins or refuses them, is undecided.
- **Depends on:** the published realm trust list D28 waits on, for every non-admin check. Until it exists, a station
  switched to `enforce` admits admins and refuses everyone else, so the design must make that visible when the
  setting is turned on rather than leave it looking like "nobody can connect".
- **Status:** accepted by Raf on 2026-09-22, in four answers: build it in 12.0.0 with a toggle; the default is OFF;
  admins ALWAYS have access; and, of the two removal shapes above, the long endorsement with withdrawal checked
  after admitting. One point stays open, marked above: rotating a pinned realm key.
- **Blocks:** WP 1.3 (the CONNECT check), WP 1.5 (the `member_endorsement` field), WP 1.6, WP 3.1 (the realm issuing
  and withdrawing endorsements). The station check is built after macula 12 lands.
