# DESIGN: End-to-end payload confidentiality

**This exists so a station can relay a call, a stream or an event without being able to read what it carries.**

| | |
|---|---|
| Kind | CLAIM (a threat model and what defeats it), design only, no code |
| Closes | `PLAN_MILITARY_GRADE_BASELINE.md` #14, "End-to-end confidentiality" |
| Proposes | D33 (this document's decision, for Raf) |
| Status | Decided (Raf, 2026-09-26): §12 answered; build in §13's order |
| Written against | macula v12.7.0 (1c78d059) |

---

## 1. Where we are

Confidentiality today is per QUIC link. Every link type was measured selecting a hybrid ML-KEM key exchange (baseline
#5, PQKX_MEASUREMENT rounds 1 to 5): SecP384r1MLKEM1024 for stations, BEAM services and the realm, and SecP256r1MLKEM768
for Go-based clients. The record layer is AES-256-GCM, except for Go-based clients, which negotiate AES-128-GCM with
stations until those stations run macula 12.7.0 (baseline #3). So an observer on the wire reads nothing. **A station is not an observer on the wire, though: it is an endpoint of every link it
relays over.** It decrypts each frame from one link and encrypts it again for the next, and in between it holds the
plaintext.

Stations must parse every frame they relay, because authenticity is end to end and they check it before they route
(D17, D25). Every application payload sits inside a signed `tbs`, and a verifying hop decodes that tbs
(`macula_signed_object:verify/3`, `macula_record_cbor:decode_strict/1`):

| Frame | Signed object, label | Where the payload is |
|---|---|---|
| CALL, STREAM_OPEN | `request`, `MACULA-PQ-REQUEST-V1`, the caller's identity key | `payload` in the tbs |
| RESULT, provider ERROR | `reply`, `MACULA-PQ-REPLY-V1`, the provider's | `payload`, or `code` + `detail` |
| STREAM_DATA, _REPLY, _ERROR, _END | `stream` / `caller_stream`, `MACULA-PQ-(CALLER-)STREAM-V1` | `body`, `payload`, `code` + `message` |
| PUBLISH, EVENT, GOSSIP | `publication`, `MACULA-PQ-PUBLICATION-V1`, the publisher's | `payload` |

So a station today reads every payload, every error detail and every stream body it relays. That includes whatever a
compromised, coerced or curious station operator wants, and everything a recording station could keep until a quantum
computer breaks nothing (the links are already post-quantum; the station simply is not on the outside of them).

The only shared-secret payload encryption in the codebase is the distribution tunnel's cookie-derived AES key
(`macula_dist_pool:tunnel_key/0`, D29), which is out of scope here.

## 2. Threat model

**Protected against:**

- **T1, a relaying station reads payloads.** Any station on a call's, stream's or event's path, honest but curious or
  malicious, including one that records everything for later.
- **T2, a station tampers with payloads.** Already defeated by the signatures (D25, D17); this design must keep it so.
- **T3, a station splices one party's encrypted payload into another request**, to make a provider act on a payload
  under the wrong caller's authority.
- **T4, harvest now, decrypt later.** Every key agreement is post-quantum, hybrid where the profile is (D3, D4).
- **T5, a non-member reads a realm's events** by subscribing, as SUBSCRIBE is unsigned and unchecked today.

**Not protected against** (stated, so no text claims otherwise, D11):

- A compromised **endpoint**: the caller, the provider, a subscriber, or the realm service that hands out group keys.
- **Members of a group reading that group's events.** A pubsub group key is shared; a member who leaves keeps every
  event of the epochs it held keys for.
- **Traffic analysis.** See §9: a station still sees who talks to whom, when, how much, and under which names.
- A station **refusing or delaying** traffic. Availability is a different property.
- **Forward secrecy beyond the KEM key's lifetime.** A provider's KEM private key, stolen, opens every call sent to it
  while it was published. §7.3 bounds that window; it does not close it.

## 3. The shape: encrypt inside the signed tbs

Every design choice below follows from one rule:

> **The payload field is replaced by a sealed payload inside the same signed tbs.** Signatures, request_hash, routing
> fields and every check a station makes stay as they are. Only the bytes a station never needed become unreadable.

Consequences, all wanted:

- **Sign over ciphertext.** A station still verifies every frame before routing (D17, D25, D28's budget), with nothing
  to decrypt. A forged or altered ciphertext fails the signature as any altered tbs does.
- **`request_hash` covers the ciphertext**, so a RESULT stays bound to the exact request bytes (D25) with no new rule.
- **No second envelope.** One new tbs field per frame type, one new sealed shape. Verification tables grow by a field;
  routing code does not change.
- A sealed payload is an opaque binary to the CBOR decoder: a station's decode of a verifying hop gets cheaper, not
  dearer (D28).

### 3.1 The sealed payload

A new tbs field `sealed`, present **instead of** `payload` (or `body`, or `code`/`detail`/`message`), never beside it.
On a CALL or STREAM_OPEN the sealed plaintext also holds the request's `token` and `proofs` (the caller's UCAN and its
delegation chain), which only the provider reads (`macula_station_link:authorize_policy/3`), so a station no longer
reads a caller's capabilities either. On a call sent in the clear (§8.1), `token` and `proofs` stay in the clear tbs
where they are today.

```
sealed = #{
  scheme => 1,             % the suite below; the only one
  kem_ct => bytes,         % only on the frame that agrees a key: CALL, STREAM_OPEN
  key_id => bytes(8),      % which recipient key (§4) or group epoch (§6) this was sealed to
  nonce  => bytes(12),     % on a reply, a provider stream frame and an event (§5.1, §5.2, §6.2); derived elsewhere
  ct     => bytes          % AES-256-GCM ciphertext || 16-byte tag
}
```

A request and a caller's stream frame derive their nonce, from a key used once or from the caller's signed seq (§5). A
reply, a provider's stream frame and an event carry a random one, because nothing in the code guarantees their keys seal
only one message (§5.1, §5.2, §6.2).

### 3.2 The suite

| Part | pq_pure | pq_hybrid |
|---|---|---|
| KEM | ML-KEM-1024 (FIPS 203) | ML-KEM-1024 + ECDH P-384, combined (§3.3) |
| KDF | HKDF-SHA-384 (RFC 5869) | same |
| AEAD | AES-256-GCM, 96-bit derived nonce, 128-bit tag | same |

All three are what CNSA 2.0 lists and what the QUIC links already use (SecP384r1MLKEM1024, AES-256-GCM, D3). OTP 28.4.3
has ML-KEM-1024 in `crypto` (`crypto:generate_key(mlkem1024, [])`, `crypto:encapsulate_key/2`,
`crypto:decapsulate_key/3`; checked on 28.4.3: public key and ciphertext 1568 bytes each, shared secret 32), plus P-384 ECDH, HMAC-SHA-384
and AES-256-GCM, so the BEAM SDK needs no new NIF. Go has `crypto/mlkem` and `crypto/ecdh`; Rust has `macula-mlkem`.

### 3.3 The hybrid combiner (pq_hybrid)

For pq_hybrid (the EU profile, D3, D4), a key agreement is secure if **either** half is:

```
ss = HKDF-Extract(salt = "MACULA-E2E-HYBRID-V1",
                  ikm  = ss_mlkem || ss_ecdh || kem_ct_mlkem || ecdh_ephemeral_pub || recipient_kem_key_id)
```

where `recipient_kem_key_id` is the full 48-byte SHA-384 of the recipient's KEM key as carried. Binding both ciphertexts
and the recipient's key into the extract follows X-Wing and the TLS hybrid draft (draft-ietf-tls-hybrid-design): no half
can be swapped without changing `ss`. The "either half suffices" property rests on HMAC-SHA-384 as a PRF keyed by the
public salt over the IKM. The P-384 half requires point validation on the peer's point and refuses an identity (all-zero)
ECDH output. pq_pure uses the same IKM shape without the ECDH parts, and its own salt `"MACULA-E2E-PURE-V1"`, so
both profiles run one code path.

Every `||` in this document is over fixed-width fields or length-prefixed ones; the test vectors (§13 #1) define the
encoding as the deterministic CBOR array of the listed fields, so no two field sequences encode alike.

## 4. The keys

### 4.1 A new key purpose: `kem`

`macula_node_keys` gets a `kem` purpose beside `identity`, `connect` and `tls`: an ML-KEM-1024 keypair, plus a P-384
keypair in pq_hybrid. It is **separate from the identity key**: a signature key is never used to decrypt, and the KEM
key rotates on its own schedule (§7.3) while the node_id, derived from the identity key (D5), never changes.

### 4.2 Published as a record, signed by the identity key

A new record type, `node_kem_key` (the next free built-in tag), stored under a key derived from the node_id:

| Field | |
|---|---|
| `node_id` | the owner; the record's signer must derive to it |
| `kem_key` | the ML-KEM-1024 public key (1568 bytes), plus the P-384 point in pq_hybrid |
| `key_id` | the first 8 bytes of SHA-384 over the key as carried |
| `not_before`, `expires_at` | a lifetime of at most 7 days; §7.3 |

Its signature under the identity key is what makes it authentic: a station can withhold it or serve an expired one,
but cannot substitute its own. During a rotation's one-day overlap the record carries both the current and the next key,
each with its id, because the slot keeps one record per signer (D28): a caller whose advertisement names either finds it. The slot keeps one entry per signer (D28), so a node's newest record replaces its older
one.

**The provider's `procedure_advertisement` carries its current `kem_key_id`, signed.** That field, not whether a key
lookup succeeded, is what tells a caller the provider takes sealed calls (§8.1). A station can withhold a record, but it
cannot strip a signed field; and withholding the advertisement itself denies the call, which is availability, not T1.

## 5. Calls and streams

### 5.1 A call

The caller already knows its target: `target` is in every CALL's signed tbs (D25), so the caller has the node it must
encrypt to.

1. The caller has the target's `node_kem_key`, cached for its lifetime, or fetched from the DHT and verified.
2. It encapsulates to it (`kem_ct`, `ss`), and derives two keys:
   ```
   k_req, k_rep = HKDF-Expand(ss, info = "MACULA-E2E-CALL-V1" || request_id || caller || target, 64)
   ```
3. It seals the payload: `ct = AES-256-GCM(k_req, nonce = 0^96, aad = A, payload)`, and puts
   `sealed = #{scheme, kem_ct, key_id, ct}` in the CALL tbs in place of `payload`. It then signs as today.
4. The provider verifies as today, then decapsulates with its KEM private key for `key_id`, derives the same keys, and
   opens `ct`. A failure answers `sealed_refused` in the clear, naming the provider's current `key_id`: the caller
   re-resolves the key and never falls back to clear. It seals again under a **new request_id**: admission would
   refuse a new encapsulation under the same one as `request_id_reused`.
5. The provider seals its RESULT payload, or its ERROR `code` and `detail`, under `k_rep` with a **fresh random 96-bit
   nonce** carried in `sealed`. It does no KEM of its own.

The request's nonce is fixed (`0^96`), which is safe because `k_req` seals exactly one message: a fresh encapsulation
makes fresh keys for every CALL, and a caller never seals a second request under them.

The reply's key does **not** seal exactly one message. Request admission (`macula_request_admission:seen/5`) answers
a copy of an already-answered request, and a copy after its deadline, with refusals. A provider restart between two
copies of a D25 retry (the same signed bytes, sent over another path) answers twice. Under a fixed nonce, two replies
under one `k_rep` would leak their XOR and GCM's authentication key. Hence a random nonce per reply, and hence:

**Admission refusals are sent in the clear, from a closed set**, as the station's relay codes are (`RELAY_CODES`):
`sealed_refused`, `expired`, `not_yet_valid`, `request_id_reused`, `request_copy`, `reply_not_kept`, `caller_quota`,
`share_full`, `admission_full`. They carry no application data; `sealed_refused` names the provider's current key id in
its `detail`. The same set applies to a provider's STREAM_ERROR refusing a STREAM_OPEN before it is decrypted. A clear `code` outside that set, answering a sealed request, is refused as
`malformed_frame`. **A retry resends the same signed bytes (D25)**, never a re-encryption: admission would refuse a new
encapsulation under the same request_id as `request_id_reused`, and the random reply nonce is what makes resending safe.

**The AAD** binds the ciphertext to its request:

```
A = "MACULA-E2E-AAD-V1" || frame_type || realm || procedure || caller || target || request_id || deadline
```

and the KDF info in step 2 carries `frame_type` too, since CALL and STREAM_OPEN share it.

This is what defeats T3. A station, or a caller, that copies another caller's `sealed` into its own signed CALL
changes `caller` or `request_id`, so the AAD no longer matches and the provider refuses it. Without the AAD, the
provider would run someone else's request under the copier's authority, and the signature alone cannot see it, since
the copier signed its own tbs. The reply's AAD adds `request_hash` and `responded_by`.

### 5.2 A stream

A STREAM_OPEN agrees keys exactly as a CALL does. The STREAM_OPEN's own payload is sealed under `k_req` with nonce 0,
and it derives two stream keys:

```
k_c2p, k_p2c = HKDF-Expand(ss, "MACULA-E2E-STREAM-V1" || request_id || caller || target, 64)
```

Every later stream frame seals its `body`, `payload` or `code`/`message` under the key for its direction:

- **Caller to provider**: `nonce = seq as 96 bits`. The caller's frames are individually signed and numbered from 0,
  and one out of order is refused (`caller_checked/5`). `ss` lives only in the caller's stream state, and its seq
  counter never resets while that state lives, so the nonce never repeats under `k_c2p`.
- **Provider to caller**: a **random 96-bit nonce** in `sealed`, as a reply. A D25 retry of a STREAM_OPEN after a
  provider restart is admitted as new, decapsulates to the same `ss`, and the new provider instance numbers its frames
  from 0 again: a seq-derived nonce would repeat under `k_p2c`.

A stream frame's AAD is `"MACULA-E2E-STREAM-AAD-V1" || frame_type || request_id || seq || direction`, with
`direction` one byte (0 caller to provider, 1 provider to caller), encoded as §3.3 says for the test vectors.

A replayed or reordered frame is still refused by its signed seq before anything is decrypted. STREAM_END carries
nothing to seal.

The station's relay STREAM_ERROR (`unknown_next_peer`, #42) stays in the clear: it carries no payload, and the caller
needs it without holding any key.

### 5.3 Direct dial, D27 content, and pools

- **Direct dial** (`macula_direct_dial`) resolves the provider's advertisement already. It fetches the `node_kem_key`
  in the same step, and the advertisement's `kem_key_id` says whether the cached one still holds.
- **D27 content streams** are sealed like any stream, so a relaying station no longer reads the content it relays. The
  content itself stays public by design: anyone holding the MCID can ask the sharer for it. Sealing protects the
  transfer, not the content's secrecy, and the guide must say exactly that.
- **Several stations**: nothing changes. Only the endpoints hold keys, whatever path the frames take.

## 6. Pubsub: group keys and what fan-out costs

This is the hard part. A publisher does not know its subscribers: it targets `(realm, topic)`, and stations and
Plumtree own the subscriber index. So there is no recipient to encapsulate to.

### 6.1 The options

| | How | Per event | Per membership change | Verdict |
|---|---|---|---|---|
| A. Encrypt to each subscriber | the publisher asks who subscribes, seals n times | n KEM encapsulations, n ciphertexts | nothing | **Refused.** The publisher would have to learn the subscriber set, which stations hold. Per-event cost grows with n, and so does the event's size on every link. |
| B. One group key per group, epoch-rotated | a group key `K_g,e` seals each event once; each member pulls it from the distributor over a sealed call | one AEAD, ~50 bytes more | n sealed calls, each one signed RESULT (§6.3) | **Chosen.** Fan-out stays what it is today: one ciphertext, relayed and gossiped unchanged. |
| C. Sender keys, one per publisher (the Signal group model) | each publisher distributes its own key to the members | one AEAD | n wraps per publisher | **Refused.** It needs the member set per publisher, and m publishers multiply the wraps by m. |

### 6.2 B, in detail

- **A group** is `(realm, topic prefix)`, named by the application: `#{group => GroupId}` on publish and subscribe. A
  realm-wide group is the default.
- **The distributor** is the provider of an org-namespaced procedure, `<org>/group_keys_v1`, whose org owns the group.
  Its D25 authorization (org directory plus procedure delegation) is what makes it the group's legitimate distributor. A
  `~node_id` distributor is refused unless the member pinned it.
- **A member pulls the key over a sealed call** (§5.1): its CALL names the group and epoch, and the RESULT, sealed
  under `k_rep`, carries the epoch key. There is no separate wrap object: the call's own encapsulation is the wrap. Who
  may have the key is the distributor's decision, from the realm's member endorsement (0x05) or a UCAN, the checks that
  already exist.
- **An epoch** is a key id, a key, `publish_until` (always its issue time plus `rotate_after`, 15 minutes, whether or
  not a removal is pending) and `accept_until`
  (`publish_until` plus the 65-minute event life). A publisher re-pulls before sealing past `publish_until`: a "still
  current" answer is the same signed RESULT, priced in §6.3. A subscriber refuses an event whose epoch is past
  `accept_until`. A rotation notice on the group's topic may hasten the switch, but the lifetimes are the guarantee, since
  a station can withhold a notice.
- **Event keys and nonces.** Each publisher seals under its own subkey,
  `K_pub = HKDF-Expand(K_g,e, "MACULA-E2E-EVENT-V1" || publisher, 32)`. A subscriber derives it from the publication's
  signed `publisher` field and caches it. `sealed` carries `key_id = epoch id` and a **full 96-bit random nonce**. The
  publication `seq` is not used, since `macula_publication_seq` seeds it from the wall clock and can repeat one after a
  clock step and a restart. With a subkey per publisher, GCM's random-nonce bound (2^32 messages at a 2^-32 collision
  probability) applies to one publisher in one epoch, and no two publishers ever share a key.
- **Rotation herd.** The distributor issues epoch e+1 to members who ask during the last stretch of e, so a rotation is
  not n pulls in one instant, which admission's quotas would refuse.
- **Past epochs.** A member keeps each epoch key for the longest event life (60 min `ttl_ms` + 5 min tolerance,
  `verify_publication/3`), and the distributor serves past epochs to nodes that were members during them. So a subscriber
  offline across a rotation still opens what it was entitled to.
- **The AAD** is `realm || topic || publisher || seq || published_at`. The station reads all of these to route, and
  binding them stops a sealed payload being replayed under another topic.
- **Rotation** is batched: removals take effect at the next rotation, which runs at most every `rotate_after`
  (default 15 minutes) and at the latest every 24 hours. Publishers re-pull every `publish_until` regardless, which is
  what the removal bound rests on. **Removal bound:** a removed member
  can open events for at most **2 × `rotate_after` + 65 minutes** after its removal (the next rotation, every publisher
  moving off the old epoch by its `publish_until`, and the event life), in D31/D32's form.
- **A subscriber without the current key** asks the distributor for it, then opens the event. An event whose epoch it
  never had, it cannot open, and says so: it does not drop it silently.

### 6.3 What fan-out costs

- **Per event:** about 50 bytes more (the 16-byte tag, 12-byte nonce, 8-byte key id and their CBOR keys), and one
  AES-256-GCM pass at each end. **Relaying costs nothing new**: stations and Plumtree carry the same bytes as before,
  and the message id stays the SHA-384 of the tbs.
- **Per rotation of a group of n members**, each member pulls the new key over a sealed call. The distributor's cost
  per member is one decapsulation (191 µs measured, OTP 28.4.3 `crypto`, one core of the workstation host00, 1000-run
  mean, 2026-09-26), **one signed RESULT**, and one verify of the member's CALL (0.31 ms EU, D4) plus its proof chain
  or endorsement. The signature dominates: ML-DSA-87 at 1.107 ms (D17), the EU composite at 5.3 ms (D4). A carried
  reply is about 7.3 KB (US) or 8.3 KB (EU). The table counts the signatures; verification adds about 20% (US) or 10%
  (EU) plus the chain. Bytes received per pull (the CALL, the KEM ciphertext, a token of about 12 KB (D7) and proofs)
  are two to three times the bytes served.

  | n | Distributor CPU per rotation, US / EU | Bytes served |
  |---|---|---|
  | 1,000 | ~1.3 s / ~5.5 s | ~7.3 / 8.3 MB |
  | 10,000 | ~13 s / ~55 s | ~73 / 83 MB |
  | 100,000 | ~130 s / ~550 s | ~730 / 830 MB |

  At the 15-minute rotation bound, 10,000 members cost the distributor about 6% of one core in EU for signing, and
  publishers re-pulling once per epoch add their share at the same per-pull price. **The scaling
  limit** is about 100,000 members per group in EU at a one-hour rotation bound. Beyond it, groups are split by topic
  prefix, one distributor each. Each member also signs its own CALL (1.1 or 5.3 ms), spread across the members.
- **Distributor availability.** A distributor that is down stops new members and rotations; members already holding
  the current epoch go on. D31 refused making the realm a dependency of every member's liveness. Here the distributor
  is a dependency of publishing and subscribing to a sealed group (§8.2), never of other traffic.

### 6.4 SUBSCRIBE

SUBSCRIBE stays unsigned and readable, since stations need the topic to route. Confidentiality for pubsub comes from
the key, not from who may subscribe. So T5 is defeated because a non-member gets ciphertext, not because it cannot
subscribe.

## 7. Replay, binding, and key lifetime

### 7.1 Replay

Nothing here adds a replay path, and nothing relaxes an existing guard:

- A replayed CALL is caught by request admission, as today (request_id, deadline, the 12.5.1 admission set).
- Within a stream, the signed per-side seq refuses a replay before decryption.
- A replayed publication carries a seen `(publisher, seq)`: Plumtree's message id and the subscriber's reorder buffer
  already drop it.

### 7.2 Binding

Each sealed payload is bound three ways: by the signature over the tbs that holds it (T2), by the AAD to its routing
context (T3), and by the KDF info to its request's parties. A station that swaps `key_id` or `kem_ct` changes the tbs,
and the signature fails.

### 7.3 KEM key lifetime: bounding the forward-secrecy gap

A call is encrypted to the provider's long-lived KEM key, so that key, stolen, opens every call sent to it while it was
current. The design bounds that:

- `node_kem_key` lives at most **7 days**, and a node publishes its next key a day before the current one expires.
- A node **deletes a KEM private key** 24 hours after its record expires (the grace period covers calls in flight). So
  one stolen at time t opens at most about 8 days of traffic before t, and nothing that follows its deletion.
- Real per-message forward secrecy would need an interactive exchange, an extra round trip on every cold call. Not
  proposed. Streams could have one cheaply, as the first frame each way could carry an ephemeral KEM; that is noted for
  a later revision.

## 8. Mixed fleet, policy, and failure

### 8.1 Negotiation from signed state only

A station answers the sender's lookups, so the decision to seal must never depend on whether a lookup succeeded. Every
decision below is read from something signed that a station can withhold (a denial of service) but not alter (a
downgrade).

- **Calls.** Every call's target comes from a signed `procedure_advertisement` (D25).
  - If it names a `kem_key_id`, the caller seals. When it cannot get that key, it answers
    `{error, {confidentiality, no_kem_key}}` and **never sends in the clear to that target**.
  - Only an advertisement naming no key is called in the clear: that provider never opted in.
  - A `sealed_refused` naming a newer key id sends the caller to fetch that key, never to clear.
- **Explicit targets.** `macula:call_station/8` and `call_stream_station/7` take a node_id from the application.
  - They seal from a verified advertisement the application passes in the options.
  - With `confidential => required`, they seal or fail with `no_kem_key`.
  - With `confidential => off`, they send in the clear: the application's own decision, made without any lookup.
  - With none of these, they are refused as `{error, {confidentiality, no_signed_state}}`. Callers that rely on the
    old default must be updated: a breaking change the release names.
- **Groups.** A group's policy is a signed field of its distributor's own `procedure_advertisement` for
  `<org>/group_keys_v1`, which already carries the realm-signed org directory and delegation (D25 item 6) and lives at
  most 5 minutes (D25 item 8).
  - A node accepts it only when the advertisement's org is the topic's org segment (`{realm}/{org}/...`), or when it is
    the realm's own distributor for `_realm` topics.
  - Policy is keyed by **(realm, topic prefix)**, not by the group option a publisher passes, and is monotonic per
    node: once a node has seen `preferred` or `required` for a prefix, it never accepts `off` for it.
  - A node that has ever held an epoch key for a prefix never sends under it in the clear.
  - **`required` is enforced at receipt.** A node holding a `required` descriptor for a prefix refuses every clear
    event under that prefix, counts it and logs it at warning level, naming the publisher. A member, or a publisher on
    an older release, cannot publish clear under the prefix unnoticed.
  - A publish or subscribe that names a group whose distributor advertisement the node cannot get fails closed.
  - A publish that names **no group** is sent in the clear, as today: the application's decision, made without any
    lookup. A realm with no distributor therefore still has clear pubsub, and no sealed pubsub.
- **Opting in.** A provider that starts naming a key may still be served its last keyless advertisement, for at most 5
  minutes plus the D22 tolerance. It closes that window itself: once its last keyless advertisement has expired, which
  it knows, it refuses clear CALLs. So `preferred` at a provider is a transition, not a standing state.
- **Old receivers.** A receiver on a release without this code publishes no key and advertises none, so it is never
  sent `sealed`. It would refuse a tbs with an unknown field as `malformed_frame`.
- **Old stations.** An old station refuses a sealed frame as `malformed_frame`, and **charges** that refusal to the
  sender's D28 budget. Key presence says what the target can do, not what the path can. So every station must accept
  `sealed` (§13 #6) before any SDK sends it (§13 #3), and the release that ships sealing states the station floor.

### 8.2 Policy

- **Per advertisement:** a provider opts in by publishing its key and naming it (§4.2). With
  `confidential => required`, it also refuses every clear CALL.
- **Per call:** `confidential => required` refuses to call a target that names no key. The default, `preferred`,
  follows §8.1: sealed whenever the target named a key, clear only when it named none.
- **Per group:** the distributor's descriptor sets `required`, `preferred` or `off`, applied as §8.1 says.

Under any rule weaker than §8.1, T1 and T5 would be claims about a configuration, not about the mesh. That is why §8.1
admits no lookup-based fallback.

### 8.3 Errors

A provider's ERROR `code` and `detail` are sealed like a RESULT, with the exception of the closed set of admission
refusals (§5.1), which carry no payload. A station therefore sees that a reply is an error, and which admission refusal
it is, but never an application error's code or detail. The station's own relay errors stay in the clear.

## 9. What this does NOT hide

A station on the path still sees:

| Seen | Why it stays visible |
|---|---|
| **Payload sizes** | Ciphertext length is plaintext length + 16. Optional padding to size buckets (§12, question 3) can hide finer sizes; it cannot hide volume. |
| **Timing**, and the count of frames and events | inherent to relaying |
| **Who talks to whom** | `caller`, `target`, `publisher`, `responded_by` are signed routing fields |
| **Realm ids, procedure names, topic names, group ids** | a station routes on them |
| **Request ids, deadlines, stream seq, whether a reply is an error** | routing, admission and D25 state |
| **Every subscription** (node, realm, topic) | SUBSCRIBE is unsigned and plain |
| **DHT records**: advertisements, content announcements, node and KEM key records | public by design |
| **D27 content**, to anyone who asks the sharer for it | content is public; only the transfer is sealed |
| **Relay errors** (`unknown_next_peer`), and the closed set of admission refusals | the station makes the first; the second carry no payload (§5.1) |
| **Stream `mode`, `encoding`, `role`; `ttl_ms`, `published_at`; `retry_budget`, `source_route`** | signed routing and verification state |
| **An event's epoch key id**, and so when a group rotates, which is when its membership changes | routing needs no key id, but a member needs it to choose the key |
| **The profile** (pq_pure or pq_hybrid), from the length of `kem_ct` | inherent to the suite |
| **A station's own procedures'** payloads, until the station itself takes packages 2 and 3 | a station serving a procedure is its endpoint, not a relay |
| **Group membership**: which nodes pull an org's group keys, when they first join, and the rotation herd | a pull is a call to `<org>/group_keys_v1`, whose caller, target and timing a station sees |
| **Everything, in the clear, during the mixed-fleet period** toward nodes that publish no key | under `preferred`, clear is the default toward a node that never opted in, not an exception (§8.1) |

This design **makes no anonymity or traffic-flow claim.** `PLAN_MILITARY_GRADE_BASELINE.md` #18, traffic-flow confidentiality, keeps its own row. Public
text may say "stations relay payloads they cannot read" only once this is built, tested across SDKs, and measured (D11).

## 10. Cost summary (the KEM measured, the rest estimates to be measured)

| | Today | Sealed | Delta |
|---|---|---|---|
| CALL frame | ~7.2 KB (US) / ~8.3 KB (EU) of signature and carried key, plus the payload | + ML-KEM ct 1568 B (+97 B P-384 in hybrid), key id, tag, CBOR | **+~1.6 KB per call** (~+22% on a small call) |
| RESULT / ERROR | reply signature + payload | + tag, 12-byte nonce, CBOR | +~40 B |
| Stream frame | signature + body | + 16 tag | +16 B per frame |
| Event | publication signature + payload | + tag, nonce, key id, CBOR | +~50 B, unchanged fan-out |
| Caller CPU per call | one signature: 1.107 ms (D17) / 5.3 ms EU (D4) | + one encapsulation (75 µs measured), one HKDF, two AES-GCM | under 7% (US), under 2% (EU) |
| Provider CPU per call | one verify, one sign | + one decapsulation (191 µs measured) | about 17% of the sign (US), under 4% (EU) |
| Cold call to a new target | one advertisement lookup | + one `node_kem_key` lookup (none if the advertisement's key id matches the cache) | one DHT round trip, once per key lifetime |

## 11. Interaction with what exists

| | Effect |
|---|---|
| **D11** (claims) | No public confidentiality claim until built, cross-SDK tested and measured. §9's list goes into the guide verbatim. |
| **D17** (neighbour signatures) | Unchanged. Neighbour-signed control frames carry no application payload. |
| **D25** (reply binding) | Unchanged and strengthened: `request_hash` covers the ciphertext, and the reply AAD repeats the binding. |
| **D27** (content) | Content transfers are sealed like any stream; announcements stay public. |
| **D28** (verification budget) | Unchanged per frame; decoding a sealed payload at a station is cheaper than decoding a CBOR term. |
| **D31, D32** (realm as a dependency) | The group-key distributor is a dependency of pubsub confidentiality, not of delivery, stated in §6.3. |
| **D5, D12** (key separation) | A new, separate `kem` purpose; the node_id is unchanged. |
| **Other SDKs** | macula-go, macula-rust and the rest implement the same suite from a shared test-vector file. Nothing ships in one SDK before the vectors pass in all the BEAM stack's peers (house order: BEAM stack, then SDKs, then clients). |
| **macula-station** | Nothing to decrypt as a relay. Its verification tables accept the `sealed` field in each tbs, and relays stay byte-for-byte. Every station must be on that release before any SDK sends `sealed` (§8.1). A station's own procedures need packages 2 and 3 to be sealed. |

## 12. Decisions (Raf, 2026-09-26)

1. **The application distributes pubsub group keys.** The distributor is the provider of `<org>/group_keys_v1` for the
   org that owns the topic prefix (§6.2); the realm stays out of the data path, matching D31's reasoning.
2. **`preferred` is the default in the first release**, as §8.1 defines it: sealed whenever the target or group opted
   in, clear only toward nodes that never did, with no lookup-based fallback.
3. **No padding in v1.** Sizes stay visible, as §9 states.

## 13. Work, once approved (BUILD, sized)

| # | Package | Size |
|---|---|---|
| 1 | Test-vector file: the suite, the combiner, the CBOR encoding of every `||`, KDF labels, AAD, and one sealed CALL/RESULT/stream/event per profile. The contract every SDK passes. | S |
| 6 | macula-station accepts `sealed` in its verification tables, and does not charge it: in the request, reply and stream tables, **and in the publication table carried by EVENT and GOSSIP**, so #5 needs no second station roll. **Rolled to every station before #3 ships.** | S |
| 2 | `kem` key purpose, `node_kem_key` record, `kem_key_id` in the advertisement, rotation and deletion (§7.3) | M |
| 3 | Sealed CALL/RESULT/ERROR in `macula_frame` and the link, the clear refusal set, `sealed_refused`, the §8.1 rules, the policy option | M |
| 4 | Sealed streams | S |
| 5 | Group keys: the descriptor record, epochs, the distributor procedure, sealed publications, past-epoch retention | L |
| 7 | Measurement (sizes, CPU, rotation cost at n = 1k/10k, both profiles) and the guide text; the D11 claim only after | S |
| 8 | macula-go, then the other SDKs, against #1 | M each |

In that order: #1, then #6 on every station, then #2 to #4 and #7 give calls and streams end to end. #5 can follow in a
later release without changing any of them.

---

## Gate history

- **Round 1 (Fable), three required:**
  - a withheld key record forced cleartext under the default policy (now §4.2, §8.1);
  - the fixed reply nonce repeated under request admission's own rules (now §5.1);
  - the group-key cost omitted the reply signature and described two mechanisms (now §6.2, §6.3).
- **Round 2 (Fable), three required:**
  - the group descriptor and explicit-target calls decided from unbound state (now §8.1);
  - provider stream frames repeated a nonce across a provider restart (now §5.2);
  - no publisher was made to leave an old epoch, so the removal bound did not hold (now §6.2).
- Both rounds' observations were taken where they changed a claim; the rest are noted in the text. Two rounds is the
  cap: Raf decided the three open questions on 2026-09-26 (§12).
- **Final verdict (Fable): pass with conditions**, both for pubsub, now met:
  - a subkey per publisher and a full random nonce per event (§6.2);
  - the policy keyed by topic prefix and `required` enforced at receipt (§8.1).
  Also taken from the verdict:
  - `publish_until` is always 15 minutes (§6.2);
  - a re-seal takes a new request_id (§5.1);
  - the stream AAD is defined (§5.2);
  - clear calls keep `token` and `proofs` in the clear (§3.1);
  - stations accept `sealed` in events too (§13 #6);
  - two further rows in §9.
