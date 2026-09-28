# DESIGN: Sealed pubsub, the group keys package

**This exists so an application's events under a group are readable only by the members holding the group's current
key, while stations and Plumtree carry one ciphertext per event exactly as they carry the clear one today.**

| | |
|---|---|
| Kind | BUILD (package 5 of `DESIGN_E2E_PAYLOAD_CONFIDENTIALITY.md` §13) |
| Extends | `DESIGN_E2E_PAYLOAD_CONFIDENTIALITY.md` §6, §8.1 (groups), §8.2; `DESIGN_E2E_SEAL_REPORT.md` §7 (names) |
| Changes | §8.1's group policy moves from a field of the distributor's advertisement into its signed, sealed reply (§6 below) |
| Ships in | the next macula minor after the seal report; then macula-go, libmacula and the bindings |
| Status | Draft (Mercurius, 2026-09-28), for one Fable round (cap 2) |
| Written against | macula main 6675a64d, macula-station 0.7.0 (d13453b) |

---

## 1. What this settles

§6 chose the mechanism (one group key per epoch, pulled by each member over a sealed call) and priced it. It left
open who may be the distributor and how a member knows it has the right one, who gets a key, what an epoch id is,
what happens across a distributor restart, the wire of `<org>/group_keys_v1`, where the group policy travels, the
publish and subscribe API, and what a removed member can still read. This note settles each, and nothing else.
The event seal itself is already fixed by scheme 1 (`test/vectors/E2E_SEAL_V1.md`: `k_pub`, the event AAD, the
random nonce), and does not change.

## 2. The trust root: who may distribute a group's keys

A group is a topic prefix in a realm, and the prefix is org-namespaced: `<org>/<rest>`. The org segment names who
owns the group, the same way it names who owns a procedure.

**The chain a member checks, every link signed and already verified by macula today (D25):**

1. The **Realm Operator**, holding the realm key the member pinned, signs the org directory: org `<org>` is held by
   the org key with id `org_key` (`macula_record:org_directory/3`, at most 6 hours).
2. That org key signs a procedure delegation naming the distributor's node (`procedure_delegation/2`, at most 30
   minutes, D32: a revoked distributor is refused everywhere within 30 minutes).
3. The distributor advertises `<org>/group_keys_v1` carrying both records as its authorization, and names its KEM key
   (Amendment A1). `macula_record:verify_authorization/3` checks the chain against the pinned realm key.

**A member accepts a distributor only when** its advertisement verifies under that chain, its procedure is exactly
`<org>/group_keys_v1`, and `<org>` is the group prefix's org segment. An advertisement that names no KEM key is
refused as a distributor: the key pull must be sealed (§6.2), and a clear pull would hand the epoch key to every
station on the path.

**Realm-wide groups** (`_realm/<rest>`) follow the same chain: the Realm Operator holds the org `_realm` in its own
org directory, so its distributor is authorized like any other. There is no second mechanism.

**A `~<node_id>` distributor** (own namespace, no org) is refused unless the member pinned that node id for the
group: `#{group => Prefix, distributor => NodeId}`. Nothing in the realm vouches for it, so only the application can.

What this trusts, stated plainly: the Realm Operator decides which key holds an org, and the org decides which node
distributes its groups' keys. A Realm Operator acting in bad faith could hand an org to another key; that is the
existing trust statement of the security register, not a new one.

## 3. Who gets a key

The distributor decides, per call, from the caller's verified identity (the CALL is signed by the caller, D25) and
the proof the caller sends with it. The SDK serves the procedure and asks the application one question:

```erlang
admits(CallerNodeId, Prefix, AtMs, Proofs) -> true | false
```

The SDK's default `admits` accepts either proof:

- a **realm member endorsement** (record type 0x05, signed by the realm key, window at most 30 days) for the caller,
  whose roles include `<<"group:", Prefix/binary>>` and whose window covers `AtMs`
  (`macula_hyparview_endorsement:verify_endorsement/3`); or
- a **UCAN** issued by the org key (or delegated from it) granting `<org>/group_keys_v1` on resource `Prefix`, valid
  at `AtMs`.

`AtMs` is the current time for the current epoch, and the epoch's issue time for a past one (§4). An application
with its own member list passes its own `admits`.

**Removal is the distributor's act, not the proof's expiry.** An endorsement lives up to 30 days and a UCAN until its
own expiry, so a removed member's proof may still verify long after the application removed it. The SDK's default
`admits` therefore also consults a removed set the application maintains (`macula:remove_group_member(Distributor,
Prefix, NodeId)`), and refuses a node in it whatever proof it shows. **A member counts as removed from the moment the
distributor refuses it**, and §8's bound runs from then. An application that relies on proof expiry alone should know
its removal takes up to that expiry.

## 4. Epochs

- **The key** `k_g` is 32 bytes from `crypto:strong_rand_bytes/1`, independent of every other epoch's. Nothing chains
  one epoch's key to the next, so holding epoch e says nothing about e+1.
- **The epoch id** (`seal_key_id` on every event, `key_id` in its `sealed` map) is 8 bytes, drawn at random when the
  epoch is made, unique among the group's live epochs. Nothing derives it from the key: it is bound to the key only by
  the signed reply that carries both (§5), and a subscriber looks a key up by `(realm, prefix, id)`.
- **Its times**, as §6.2 set them: `issued_at`; `publish_until = issued_at + rotate_after` (15 minutes by default);
  `accept_until = publish_until + 65 minutes` (the 60-minute maximum event life plus the 5-minute tolerance).
- **Issuing ahead.** From `publish_until - rotate_after / 3` on, a pull returns the next epoch as well, so a rotation
  is spread over the last third of an epoch rather than asked for in one instant (§6.2, rotation herd).
- **Kept for** `accept_until`, by the distributor and by every member. After it, no event under the epoch is
  accepted, so its key has no use and is erased.
- **Across a distributor restart.** Epoch keys are kept in memory only: the SDK writes no group key to disk. A
  restarted distributor issues a new epoch at once. Members that hold the lost epochs keep reading under them until
  their `accept_until`. A member that was offline and never pulled a lost epoch cannot open events sealed under it,
  and says so (§7). This trades a narrow availability loss for keeping every epoch key off disk; §10 asks the reviewer
  whether to reverse it.

## 5. The procedure: `<org>/group_keys_v1`

A sealed call (§5.1 of the design, and A1: sealed to the KEM key the distributor's advertisement names). The CALL's
payload:

| Field | Type | Meaning |
|---|---|---|
| `prefix` | text | the group |
| `epoch` | `current` (text) or 8 bytes | the current epoch, or a past one by id |
| `proofs` | list of bytes | endorsement records and UCAN tokens, as §3 |

The RESULT (sealed under `k_rep`, signed by the distributor):

| Field | Type | Meaning |
|---|---|---|
| `prefix` | text | echoes the CALL, so a reply is never read for another group |
| `policy` | `required`, `preferred` or `off` (text) | §6 |
| `epochs` | list | the asked epoch, and the next one when §4's issuing-ahead window is open; each `#{id, key, issued_at, publish_until, accept_until}` |

Refusals are provider errors, sealed like any provider error on a sealed call: `not_a_member`, `unknown_epoch` (an id
the distributor does not hold, including one lost to a restart) and `epoch_expired` (past its `accept_until`).

## 6. Where the group policy travels (a change to §8.1)

§8.1 put the policy in "a signed field of its distributor's own `procedure_advertisement`". Every verifier refuses
that today: an advertisement's payload must hold exactly its four fields plus an optional authorization and an
optional KEM key pair (`macula_record` `advertisement_size_ok/3`, on macula main 6675a64d). macula-station admits a
record to its DHT only through `macula_record:verify/3` (`macula_dht_admission`, station 0.7.0), which applies that
shape check, so every station would refuse such an advertisement too. A new field would need a station release and every SDK's record verifier before any
distributor could publish one, or the distributor's advertisement would be refused everywhere.

**Instead, the policy is a field of the distributor's reply** (§5). That reply is signed by the provider whose
advertisement the member verified against the chain in §2, sealed to the member, and bound to its request, so it
comes from the same authority as the advertisement field would have, and cannot be forged or read by a station. What
changes:

- A node learns a group's policy by pulling its key, not by looking up the advertisement. Every node that publishes or
  subscribes under a group pulls its key anyway, so no node that acts on the policy lacks it.
- Freshness: the policy is as fresh as the last pull (at most one epoch, 15 minutes), not the advertisement's 5
  minutes. The policy is monotonic (below), so a stale answer can only be stricter than needed, never weaker.
- No record format change, no station release, no SDK verifier change.

§8.1's rules otherwise stand, keyed by `(realm, prefix)`:

- **Monotonic per node:** once a node has seen `preferred` or `required` for a prefix, it never accepts `off` for it,
  and a node that has ever held an epoch key for a prefix never publishes under it in the clear. The memory is per
  node run: a restarted node learns it again from its first pull.
- **`required` at receipt:** a node holding `required` for a prefix refuses every clear event under it, counts it and
  logs it at warning level, naming the publisher.
- **Fail closed:** a publish or subscribe that names a group whose distributor cannot be found or answer fails, as
  `{error, {group, no_distributor}}` or the distributor's refusal. A publish that names no group is clear, as today.

## 7. The API (Erlang; the other SDKs follow the same shape)

- `macula:publish(Pool, Realm, Topic, Payload, #{group => Prefix})` and
  `macula:subscribe(Pool, Realm, Topic, Pid, #{group => Prefix})`, with `distributor => NodeId` for §2's pinned own
  namespace. `Topic` must start with `Prefix`, and `Prefix` must be `<org>/...`: anything else is
  `{error, {invalid_option, group}}` before anything is sent.
- **Publishing** pulls the current epoch when it holds none that is still before its `publish_until`, then seals once
  under `k_pub` for its own node id with a fresh 96-bit random nonce and the scheme 1 event AAD.
- **Delivery** adds the seal report's names (`DESIGN_E2E_SEAL_REPORT.md` §7) to the event meta a subscriber already
  gets: `sealed` (0 or 1) and, when 1, `seal_key_id`; the publisher is the meta's existing `publisher`.
- **An event the subscriber cannot open** (an epoch it never had, a refused pull, a past `accept_until`, a tag that
  does not verify) is not dropped silently: the subscriber gets
  `{macula_event_unopened, SubRef, Topic, #{publisher, seal_key_id, reason}}` once per event, and nothing of the
  payload.

## 8. What a removed member can still read

A member removed from a group can still read:

- every event sealed under an epoch it already holds, until that epoch's `accept_until`; and
- events under an epoch that started while it was still a member, which it may still pull and read until that
  epoch's `accept_until`, because the distributor serves a past epoch to anyone who was a member when it was issued.

So it can read the group's events for **at most 2 × `rotate_after` + 65 minutes after its removal: 95 minutes with
the default 15-minute rotation.** "Removal" is the moment the distributor starts refusing it (§3). The latest epoch
it can hold was handed out at most `rotate_after` / 3 before that moment, and that epoch's `accept_until` is
`rotate_after` + 65 minutes after its issue, which stays within the bound. Nothing sealed under a later epoch is open
to it. It also keeps anything it already read: sealing cannot take back what a member has already opened.

(For the register, one sentence: "A member removed from a group keeps reading the group's events for at most 95
minutes after the group's distributor removes it (two rotations plus the event lifetime), and keeps what it already
read.")

## 9. Stations, and what is claimed when

- **No station change is expected.** macula 13's verified publication already carries `sealed`
  (`macula_frame` `verified_publication()`), sealing changes no field a station routes on, and station 0.7.0 refuses
  only to deliver a sealed event to its own consumers, which cannot open it.
- **Claimed only after it is measured:** "stations relay a sealed event without charging the publisher" is written
  nowhere until a fleet test has sent sealed events through every station and read the D28 budgets.
- **The register stays "off on the fleet"** for sealed pubsub until providers publish under groups; building this
  turns nothing on.

## 10. Vectors, measurement, tests

- **Vectors.** The event seal is profile-independent (AES-256-GCM under `k_pub`) and already in `e2e_seal_v1.json`
  (`events`). This package adds nothing to it except the `group_keys_v1` payloads' CBOR shape, pinned as a vector
  so every SDK's distributor and member agree byte for byte. The epoch id is random, so there is nothing to derive.
- **Measurement (§13 #7):** distributor CPU and bytes per rotation at n = 1,000 and 10,000 members, both profiles,
  against §6.3's estimates; publish and open cost per event. No claim before these numbers.
- **Tests, red first:** a member opens, a non-member gets `not_a_member`; a subscriber offline across a rotation
  pulls the past epoch and opens; an event past `accept_until` is refused and reported unopened; `off` after
  `preferred` is ignored; a clear event under a `required` prefix is refused and counted; a restarted distributor
  answers a lost epoch `unknown_epoch`; a distributor advertising no KEM key is refused; a `~node` distributor is
  refused unpinned; the fleet test of §9.

## 11. For the reviewer

1. §6 moves the policy from the advertisement into the reply, to avoid a record change and a station release. Is
   there a case where a node must know the policy without pulling the key?
2. §4 keeps epoch keys in memory only and accepts that a restart loses them. The alternative is an epoch store on the
   distributor's disk, encrypted under a key the application supplies. Which?
3. §3's default proof: an endorsement role `group:<prefix>` or an org-issued UCAN. Is a role string in the realm's
   endorsement the right place for group membership, given the Realm Operator issues endorsements and the org owns
   the group?
4. §3 makes removal the distributor's act (a removed set) because proofs outlive removals by up to 30 days. Should
   the default `admits` accept only short-lived proofs instead, so removal needs no list?
