# DESIGN: Sealed pubsub, the group keys package

**This exists so an application's events under a group are readable only by the members holding the group's current
key, while stations and Plumtree carry one ciphertext per event exactly as they carry the clear one today.**

| | |
|---|---|
| Kind | BUILD (package 5 of `DESIGN_E2E_PAYLOAD_CONFIDENTIALITY.md` §13) |
| Extends | `DESIGN_E2E_PAYLOAD_CONFIDENTIALITY.md` §6, §8.1 (groups), §8.2; `DESIGN_E2E_SEAL_REPORT.md` §7 (names) |
| Changes | §8.1's group policy moves from a field of the distributor's advertisement into its signed, sealed reply, with a re-pull rule that bounds its age (§6); §8.1's topic segment rule is made exact (§2); §6.2's issuing-ahead is made exact (§4) |
| Depends on | macula-realm#31 (the realm publishes endorsements and tombstones them on revoke) and `macula_record:realm_member_endorsement_key/2` (macula 13.1.0): §3 reads membership through them |
| Ships in | the macula minor after 13.1.0; then macula-go, libmacula and the bindings |
| Status | Fable round 1 answered (§11); for the Supervisor |
| Written against | macula main 6675a64d, macula-station 0.7.0 (d13453b) |

---

## 1. What this settles

§6 chose the mechanism (one group key per epoch, pulled by each member over a sealed call) and priced it. It left
open who may be the distributor and how a member knows it has the right one, who gets a key, what an epoch id is,
what happens across a distributor restart, the wire of `<org>/group_keys_v1`, where the group policy travels, the
publish and subscribe API, and what a removed member can still read. This note settles each, and nothing else.
The event seal itself is already fixed by scheme 1 (`test/vectors/E2E_SEAL_V1.md`: `k_pub`, the event AAD, the
random nonce), and does not change.

## 2. Groups, and who may distribute their keys

**A group is a topic prefix**, and topics are named as `macula_topic` names them
(`docs/guides/shared/TOPIC_NAMING_GUIDE.md`): `{realm}/{org}/{app}/{domain}/{name}_vN`. A group prefix is the first
two or more `/`-segments of such a topic. **Its org is its second segment**: `io.macula/acme/chat` is owned by `acme`.
A realm-wide group has the sentinel `_realm` in that slot (`io.macula/_realm/...`). **A topic is under a prefix** when
it equals the prefix or starts with the prefix followed by `/`, so `.../acme/chat` covers `.../acme/chat/room/said_v1`
and not `.../acme/chatter/...`.

**The chain a member checks, every link signed and already verified by macula today (D25):**

1. The **Realm Operator**, holding the realm key the member pinned, signs the org directory: org `<org>` is held by
   the org key with id `org_key` (`macula_record:org_directory/3`, at most 6 hours).
2. That org key signs a procedure delegation naming a node (`procedure_delegation/2`, at most 30 minutes, D32: a
   revoked node is refused everywhere within 30 minutes).
3. The distributor advertises `<org>/group_keys_v1` carrying both records as its authorization, and names its KEM key
   (Amendment A1). `macula_record:verify_authorization/3` checks the chain against the pinned realm key.

**A member accepts a distributor only when** its advertisement verifies under that chain, its procedure is exactly
`<org>/group_keys_v1`, and `<org>` is the group prefix's org. An advertisement that names no KEM key is refused as a
distributor: the key pull must be sealed (§6.2), and a clear pull would hand the epoch key to every station on the
path. Realm-wide groups follow the same chain: the Realm Operator holds the org `_realm` in its own org directory.

**What the delegation vouches for, as the record defines it.** A procedure delegation names the org key and the node,
not a procedure (`read_procedure_delegation/1`: `org_key`, `advertiser`). So **any node the org delegates, for any
procedure, may advertise `<org>/group_keys_v1` and be accepted as the org's distributor.** The org's trust in every node
it delegates is therefore also trust to hand out its groups' keys. In this release:

- **An org runs exactly one distributor per realm.** Two would split a group: publishers that resolved one seal under
  its epochs, members that resolved the other cannot open them.
- **A member uses the first trusted advertisement the resolver returns**, as a call does (`macula_direct_dial`), and
  keeps it for the group until its advertisement stops verifying.
- **`distributor => NodeId`** pins the distributor for a group: accepted only when the chain verifies and the
  advertisement's `advertiser_node` is `NodeId`. It is required for a `~<node_id>` distributor (own namespace, nothing
  in the realm vouches for it) and available for org groups, where it stops a second delegated node from being taken.
- An `unknown_epoch` under a group whose distributor answers is the org's misconfiguration (a second distributor, or
  a restart, §4), reported as `macula_event_unopened` (§7), never silent.

A distributor per prefix, rather than per org, needs the prefix in the procedure name, and is not in this release.

What this trusts, stated plainly: the Realm Operator decides which key holds an org, and the org decides which nodes
it delegates, each of which may distribute its groups' keys. A Realm Operator acting in bad faith could hand an org to
another key; that is the existing trust statement of the security register, not a new one.

## 3. Who gets a key

The distributor decides, per call, from the caller's verified identity (the CALL is signed by the caller, D25) and
the proofs the caller sends with it. The SDK serves the procedure and asks the application one question:

```erlang
admits(CallerNodeId, Prefix, AtMs, Proofs) -> true | false
```

**The SDK's default `admits` requires both:**

1. **An org grant:** a UCAN issued by the org key, or delegated from it, with
   `with = mri:proc:<realm>/<org>/group_keys_v1`, valid at `AtMs` (`macula_ucan`). The UCAN grammar has no topic
   prefix resource (`macula_ucan:grant/1` knows `mri:realm`, `mri:org` and `mri:proc`), so **in this release a grant
   admits its holder to every group of the org.** A per-prefix resource is its own later item, in every SDK's
   verifier at once.
2. **Live realm membership:** the caller's realm endorsement, read from the mesh, not from the proofs: a lookup of
   `macula_record:realm_member_endorsement_key(Realm, CallerNodeId)` and
   `macula_hyparview_endorsement:slot_endorsement/4` on what it returns, at `AtMs`. Only the realm key's entry counts,
   and a realm-key tombstone of the endorsement answers `withdrawn` (macula-realm#31 publishes both), so a member the
   realm revoked is refused as soon as the tombstone is stored. A presented endorsement blob is never accepted on its
   own (`verify_endorsement/3` checks the window only at the current time and consults no tombstone).

`AtMs` is the current time for the current epoch and the next, and the epoch's `issued_at` for a past one (§4). An
application with its own member list passes its own `admits`.

**The org removes a member with a removed set, absolute.** A UCAN lives until its own expiry, so the org's removal
cannot wait on it. `macula:remove_group_member(Distributor, Org, NodeId)` adds a node to a set the default `admits`
checks first: a node in it is refused every epoch, past or current, whatever it shows. The set is the application's
state, not the SDK's, and the application must keep it across a distributor restart; a set lost to a restart
re-admits every removed member whose grant still verifies. An application that issues only short-lived UCANs may run
without a set, and its removal bound is then the UCAN's lifetime plus §8's.

**A member counts as removed from the moment the distributor refuses it** (the removed set, or the realm's
tombstone), and §8's bound runs from then.

## 4. Epochs

- **The key** `k_g` is 32 bytes from `crypto:strong_rand_bytes/1`, independent of every other epoch's. Nothing chains
  one epoch's key to the next, so holding epoch e says nothing about e+1.
- **The epoch id** (`seal_key_id` on every event, `key_id` in its `sealed` map) is 8 bytes, drawn at random when the
  epoch is made, unique among the group's live epochs. Nothing derives it from the key: it is bound to the key only by
  the signed reply that carries both (§5), and a node looks a key up by `(realm, prefix, id)`.
- **Its times.** Epochs are contiguous: `issued_at(e+1) = publish_until(e)`. `publish_until = issued_at + rotate_after`
  (15 minutes by default). `accept_until = publish_until + 65 minutes` (the 60-minute maximum event life plus the
  5-minute tolerance). A distributor makes epoch e+1 when e's ahead window opens.
- **The ahead window** of epoch e runs from `publish_until(e) - rotate_after / 3` to `publish_until(e)`. A pull in it
  returns e and e+1.
- **Every node that holds a group's epoch re-pulls `current` at a uniformly random instant in its newest epoch's ahead
  window, whether or not it has seen an event.** This spreads the rotation over a third of an epoch instead of one
  instant (§6.2's rotation herd), keeps every holder's policy at most one epoch old (§6), and has each node holding e+1
  before e's `publish_until`.
- **A publisher seals under the newest epoch it holds whose `issued_at` has passed.** So from `publish_until(e)` on,
  every publisher that pulled in the ahead window seals under e+1, which every holder that pulled already has.
- **Kept for** `accept_until`, by the distributor and by every member. After it, no event under the epoch is
  accepted, so its key has no use and is erased. A node applies `accept_until` from the distributor with the same
  5-minute clock tolerance `verify_publication/3` applies.
- **Across a distributor restart.** Epoch keys are kept in memory only: the SDK writes no group key to disk. A
  restarted distributor makes a new current epoch at once. Members holding the lost epochs keep reading under them until
  their `accept_until`. A member that never pulled a lost epoch cannot open events sealed under it, and says so (§7).
  At most one rotation's events (about 80 minutes of delivery) are affected, and none silently.

## 5. The procedure: `<org>/group_keys_v1`

A sealed call (§5.1 of the design, and A1: sealed to the KEM key the distributor's advertisement names). The CALL's
payload:

| Field | Type | Meaning |
|---|---|---|
| `prefix` | text | the group |
| `epoch` | `current` (text) or 8 bytes | the current epoch, or a past one by id |
| `proofs` | list of bytes | UCAN tokens, as §3 |

The RESULT (sealed under `k_rep`, signed by the distributor):

| Field | Type | Meaning |
|---|---|---|
| `prefix` | text | echoes the CALL |
| `policy` | `required`, `preferred` or `off` (text) | §6 |
| `epochs` | list | the asked epoch, and the next one inside the ahead window; each `#{id, key, issued_at, publish_until, accept_until}` |

Under `off` the reply still carries epochs: `off` only means clear events are accepted too (§6). Refusals are provider
errors, sealed like any provider error on a sealed call: `not_a_member`, `unknown_epoch` (an id the distributor does
not hold, including one lost to a restart) and `epoch_expired` (past its `accept_until`). The CBOR shape of both
payloads is pinned as a vector (§10).

## 6. Where the group policy travels (a change to §8.1)

§8.1 put the policy in "a signed field of its distributor's own `procedure_advertisement`". Every verifier refuses
that today: an advertisement's payload must hold exactly its four fields plus an optional authorization and an
optional KEM key pair (`macula_record` `advertisement_size_ok/3`, on macula main 6675a64d). macula-station admits a
record to its DHT only through `macula_record:verify/3` (`macula_dht_admission`, station 0.7.0), which applies that
shape check, so every station would refuse such an advertisement too. The advertisement form would need a station
release and every SDK's record verifier first.

**Instead, the policy is a field of the distributor's reply** (§5). That reply is signed by the provider whose
advertisement the member verified against the chain in §2, sealed to the member and bound to its request, so it comes
from the same authority the advertisement field would have and cannot be forged or read by a station. What changes:

- A node learns a group's policy by pulling its key. Every node that publishes or subscribes under a group pulls its
  key, and §4 has every holder pull again each epoch, event or no event.
- **Age:** a holder's policy is at most one `rotate_after` old (§4's re-pull), against the advertisement's 5 minutes. A
  pull that fails keeps the last policy: a publisher then fails closed once its newest epoch passes `publish_until`
  (§7), and a subscriber keeps enforcing the last policy it had.
- No record format change, no station release, no SDK verifier change.

§8.1's rules otherwise stand, keyed by `(realm, prefix)`:

- **Monotonic per node:** once a node has seen `preferred` or `required` for a prefix, it never accepts `off` for it.
  A stale answer is therefore at most one `rotate_after` old, and never `off` after `preferred` or `required`. The
  memory is per node run: a restarted node learns it again from its first pull.
- **A node holding a group's key never publishes clear under its prefix.** A publish under the prefix that names no
  group is refused as `{error, {confidentiality, {group_held, Prefix}}}`, naming the prefix, rather than sent in the
  clear or silently sealed.
- **`required` at receipt:** a node holding `required` for a prefix refuses every clear event under it, counts it and
  logs it at warning level, naming the publisher.
- **Fail closed:** a publish or subscribe that names a group whose distributor cannot be found or answer fails, as
  `{error, {group, no_distributor}}` or the distributor's refusal. A publish that names no group, under a prefix whose
  key the node does not hold, is clear, as today.

## 7. The API (Erlang; the other SDKs follow the same shape)

- `macula:publish(Pool, Realm, Topic, Payload, #{group => Prefix})` and
  `macula:subscribe(Pool, Realm, Topic, Pid, #{group => Prefix})`, with `distributor => NodeId` (§2). `Topic`, or for a
  pattern subscription every topic it matches, must be under `Prefix` (§2), and `Prefix` must have an org segment:
  anything else is `{error, {invalid_option, group}}` before anything is sent.
- **Publishing** seals under §4's epoch with `k_pub` for its own node id, a fresh 96-bit random nonce and the scheme 1
  event AAD. A publisher whose newest epoch has passed `publish_until` and whose pull failed refuses the publish with
  the pull's error.
- **Delivery** adds the seal report's names (`DESIGN_E2E_SEAL_REPORT.md` §7) to the event meta a subscriber already
  gets: `sealed` (0 or 1) and, when 1, `seal_key_id`; the publisher is the meta's existing `publisher`.
- **An event the subscriber cannot open** is not dropped silently: the subscriber gets
  `{macula_event_unopened, SubRef, Topic, #{publisher, seal_key_id, reason}}` once per event, and nothing of the
  payload. `reason` is one of a closed set every SDK uses: `unknown_epoch`, `epoch_expired`, `not_a_member`,
  `no_distributor`, `no_group` (a sealed event on a subscription that named no group) and `tag_invalid`.
- **Unknown ids are bounded.** A subscriber pulls a given `(prefix, id)` at most once and remembers an
  `unknown_epoch` answer until that id could no longer be accepted anyway (65 minutes). It pulls for at most 3
  unknown ids per publisher per `rotate_after`; beyond that, events under further unknown ids from that publisher are
  reported `unknown_epoch` without a pull. A publisher sealing under random ids therefore costs the distributor at
  most 3 pulls per subscriber per epoch.

## 8. What a removed member can still read

A member removed from a group (§3: from the moment the distributor refuses it, at time T) can read only events sealed
under epochs it already holds, until those epochs' `accept_until`. It can pull nothing more, past or current.

The newest epoch it can hold was pulled no later than T. An epoch is handed out only once its predecessor's ahead
window is open, so its `issued_at` is at most T + `rotate_after` / 3, its `publish_until` at most
T + 4/3 × `rotate_after`, and its `accept_until` at most T + 4/3 × `rotate_after` + 65 minutes. **With the default
15-minute rotation: at most 85 minutes after its removal.** In practice what stops it earlier is honest publishers
leaving the epochs it holds by their `publish_until` (at most 20 minutes after T); the rest is the delivery of events
published before that. Nothing sealed under a later epoch is open to it. It also keeps anything it already read:
sealing cannot take back what a member has already opened.

(For the register, one sentence: "A member removed from a group keeps reading the group's events for at most 85
minutes after the group's distributor removes it, and keeps what it already read.")

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
  (`events`). This package adds the `group_keys_v1` CALL and RESULT payloads' CBOR shape, so every SDK's distributor and
  member agree byte for byte. The epoch id is random, so there is nothing to derive.
- **Measurement (§13 #7):** distributor CPU and bytes per rotation at n = 1,000 and 10,000 members, both profiles,
  against §6.3's estimates, with §4's jittered re-pull and request admission's quotas in place; publish and open cost
  per event. No claim before these numbers.
- **Tests, red first:** a member opens, a non-member gets `not_a_member`; a member the realm revoked (tombstone
  stored) gets `not_a_member`; a member in the removed set gets `not_a_member` for a past epoch too; a subscriber offline
  across a rotation pulls the past epoch and opens; a holder re-pulls in the ahead window with no events and picks up
  `required`; an event past `accept_until` is reported unopened; `off` after `preferred` is ignored; a clear event
  under a `required` prefix is refused and counted; a publish under a held prefix without `group` is refused; a
  restarted distributor answers a lost epoch `unknown_epoch`; a distributor advertising no KEM key is refused; a
  `~node` distributor is refused unpinned; a pinned org distributor refuses another delegated node's advertisement; a
  fourth unknown id from one publisher in one epoch causes no pull; the fleet test of §9.

## 11. Fable round 1 (2026-09-28), answered

Required:

1. A delegation names a node, not a procedure, so any delegated node was an accepted distributor, and several
   distributors split a group: §2 now states that trust, allows one distributor per org per realm, fixes the
   member's choice, extends `distributor => NodeId` to org groups and reports the split as unopened.
2. Nothing made a subscriber pull again, so a stale `preferred` could outlive a switch to `required` indefinitely: §4
   now has every holder re-pull each epoch, event or no event, and §6 states the age that follows.
3. The org segment was defined as the first segment, against `macula_topic`'s `{realm}/{org}/...`: §2 now takes the
   second segment, `_realm` in that slot for realm-wide groups, and matches prefixes on segment boundaries.

Taken from the observations: contiguous epochs and a jittered re-pull (§4), a publisher's epoch choice (§4), the
bound's derivation, now 85 minutes (§8), an absolute removed set kept by the application (§3), membership read through
the realm's tombstone-aware slot instead of a presented endorsement (§3, per the Supervisor and macula-realm#31), the
org UCAN admitting every group of the org until a prefix resource exists (§3), `off` replies and held-prefix publishes
(§5, §6), the closed unopened-reason set and the unknown-id bound (§7), clock tolerance (§4), pattern subscriptions
(§7).

Answers adopted: the policy stays in the reply; epoch keys stay in memory only; the org UCAN is the default grant; the
removed set is kept and absolute.
