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
| Status | Fable rounds 1 and 2 answered (§11), the cap reached; for the Supervisor |
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
  keeps it for the group until its advertisement stops verifying. A member that must not take another delegated node
  pins the distributor (below).
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
the UCAN the caller sends in the call's own `ucan_token`. The distributor (`macula_group_keys`) asks the application
two questions, each with a default:

```erlang
membership(CallerNodeId) -> ok | {error, not_a_member | membership_unknown}
removed(CallerNodeId) -> boolean()
```

**A pull is admitted only with both:**

1. **An org grant:** a UCAN issued by the org key, or delegated from it, carrying `can = "group_keys"` and
   `with = mri:proc:<realm>/<org>/group_keys_v1` (or `mri:org:<realm>/<org>`, which covers it), valid at the time of
   the call. The application advertises the procedure with the advertise policy `{realm_member_required, OrgKeyId,
   <<"group_keys">>}`, so macula checks the call's `ucan_token` with `macula_ucan:authorize/3` before the handler
   runs, and the handler never sees an unauthorized call: that form matches the issuer by key id, which is what an org key has
   (`macula_node_keys:key_id/2`); `{ucan_required, _}` matches identity keys by node id only and would refuse every
   org-issued token as `not_the_issuer`. The UCAN grammar has no topic prefix resource (`macula_ucan:grant/1` knows
   `mri:realm`, `mri:org` and `mri:proc`), so **in this release a grant admits its holder to every group of the org.** A
   per-prefix resource is its own later item, in every SDK's verifier at once.
2. **Live realm membership** (the default `membership`): the caller's realm endorsement, read from the mesh, never
   from anything the caller presents: a lookup of
   `macula_record:realm_member_endorsement_key(Realm, CallerNodeId)` and
   `macula_hyparview_endorsement:slot_endorsement/4` on what it returns. The slot's entry, window included, is verified
   at the time of the call, for a past epoch too: a subscriber catching up on an epoch it missed is still a member, and
   one that is not has no business reading the group. Only the realm key's entry counts, and a realm-key tombstone of the endorsement answers
   `withdrawn` (macula-realm#31 publishes both). A presented endorsement blob is never accepted on its own
   (`verify_endorsement/3` checks the window only at the current time and consults no tombstone).
   **What the tombstone bounds, honestly:** the lookup returns what the stations on its path serve, and a station can
   withhold the tombstone and serve the endorsement it still holds. So the tombstone shortens a revocation when the
   lookup path is honest; the guaranteed bound is the endorsement's own window (at most 30 days) or the org's removed
   set (below), whichever comes first. The distributor keeps no slot answer longer than one `rotate_after`, and a
   lookup that fails or times out refuses the pull as `membership_unknown` (its own code, not `not_a_member`), so the
   member retries (§4).
   Until macula-realm#31 has published an endorsement for every member, this check refuses everyone: the realm ships
   before any distributor uses the default `membership`. A member's endorsement stays readable for as long as it is a
   member, because the realm renews it before it expires (macula-realm#31's renewal sweep, 7 days ahead of
   `valid_until`) and never renews a revoked or resigned member.

An application with its own member list passes its own `membership`.

**The org removes a member with a removed set, absolute.** A UCAN lives until its own expiry, so the org's removal
cannot wait on it. The application's `removed` is asked before membership: a node in its removed set is refused every epoch, past or current, whatever it shows. The set is the application's
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
- **A pull that fails is retried until one succeeds.** A holder whose window pull failed (refused by quota, withheld,
  timed out, `membership_unknown`), or whose newest epoch has passed `publish_until` because it missed the window (a
  sleeping laptop), pulls at once and retries with backoff (doubling from 1 second, capped at `rotate_after` / 3)
  until a pull succeeds. One lost pull never ends a holder's pulling.
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

The RESULT (sealed under `k_rep`, signed by the distributor):

| Field | Type | Meaning |
|---|---|---|
| `prefix` | text | echoes the CALL |
| `policy` | `required`, `preferred` or `off` (text) | §6 |
| `epochs` | list | the asked epoch, and the next one inside the ahead window; each `#{id, key, issued_at, publish_until, accept_until}` |

Under `off` the reply still carries epochs: `off` only means clear events are accepted too (§6). A call without a valid org
grant is refused by macula before the handler (§3). The handler refuses with `{error, Reason}`, which macula answers
as a provider error with code `handler_error` and the reason as its detail, sealed like any provider error on a sealed
call: `not_a_member`, `membership_unknown` (the membership lookup failed or timed out; retry), `unknown_epoch` (an id
the distributor does not hold, including one lost to a restart), `epoch_expired` (past its `accept_until`) and
`unknown_group` (a prefix whose second segment is not the distributor's org). The CBOR shape of both
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
- **Age:** once pulls succeed, a holder's policy is at most one `rotate_after` old (§4's re-pull), against the
  advertisement's 5 minutes. While pulls fail, a publisher fails closed once its newest epoch passes `publish_until`
  (§7), and a subscriber keeps enforcing the last policy it had and keeps retrying (§4).
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
  `macula:subscribe(Pool, Realm, Topic, Pid, #{group => Prefix})`, with `distributor => NodeId` (§2) and
  `ucan_token => Token`, the org's grant (§3). `Topic`, or for a
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
  `membership_unknown`, `no_distributor`, `no_group` (a sealed event on a subscription that named no group) and
  `tag_invalid`.
- **Unknown ids are bounded.** A subscriber pulls a given `(prefix, id)` once per `unknown_epoch` answer and
  remembers that answer until the id could no longer be accepted anyway (65 minutes); a pull that failed for another
  reason is retried under §4's backoff. It pulls for at most 3
  unknown ids per publisher per `rotate_after`; beyond that, events under further unknown ids from that publisher are
  reported `unknown_epoch` without a pull. A publisher sealing under random ids therefore costs the distributor at
  most 3 pulls per subscriber per epoch.

## 8. What a removed member can still read

A member removed from a group (§3: from the moment the distributor refuses it, at time T) can read only events sealed
under epochs it already holds, until those epochs' `accept_until`. It can pull nothing more, past or current.

The newest epoch it can hold was pulled no later than T. An epoch is handed out only once its predecessor's ahead
window is open, so its `issued_at` is at most T + `rotate_after` / 3, its `publish_until` at most
T + 4/3 × `rotate_after`, and its `accept_until` at most T + 4/3 × `rotate_after` + 65 minutes, on the
distributor's clock. **With the default 15-minute rotation: at most 85 minutes after its removal, plus the clock skew
between the distributor and a publisher (at most the 5-minute tolerance).** In practice what stops it earlier is honest publishers
leaving the epochs it holds by their `publish_until` (at most 20 minutes after T); the rest is the delivery of events
published before that. Nothing sealed under a later epoch is open to it. It also keeps anything it already read:
sealing cannot take back what a member has already opened.

(For the register, one sentence: "A member removed from an org's groups keeps reading their events for at most 85
minutes after the org's distributor starts refusing it, and keeps what it already read. The org's removal takes
effect at once; a realm revocation takes effect when the distributor reads the realm's tombstone, and at the latest
when the member's endorsement expires (at most 30 days).")

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
  member agree byte for byte, and one org-issued `group_keys` UCAN per profile, so every SDK's distributor accepts
  every SDK's token. The epoch id is random, so there is nothing to derive.
- **Measurement (§13 #7):** distributor CPU and bytes per rotation at n = 1,000 and 10,000 members, both profiles,
  against §6.3's estimates, with §4's jittered re-pull and request admission's quotas in place, and including the
  membership lookup each pull costs the distributor (one DHT lookup, inside the call's deadline and against its own D28
  budget); publish and open cost per event. No claim before these numbers.
- **Tests, red first:** a member opens, a non-member gets `not_a_member`; a member the realm revoked (tombstone
  stored) gets `not_a_member`; a member in the removed set gets `not_a_member` for a past epoch too; a subscriber offline
  across a rotation pulls the past epoch and opens; a holder re-pulls in the ahead window with no events and picks up
  `required`; an event past `accept_until` is reported unopened; `off` after `preferred` is ignored; a clear event
  under a `required` prefix is refused and counted; a publish under a held prefix without `group` is refused; a
  restarted distributor answers a lost epoch `unknown_epoch`; a distributor advertising no KEM key is refused; a
  `~node` distributor is refused unpinned; a pinned org distributor refuses another delegated node's advertisement; a
  fourth unknown id from one publisher in one epoch causes no pull; a prefix another org owns gets `unknown_group`; the fleet
  test of §9.

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

## 12. Fable round 2 (2026-09-28), answered; the last

Round 1's three required changes were confirmed fixed. Required:

1. A failed or missed re-pull ended a holder's pulling, so one withheld reply made its policy age unbounded again: §4
   now retries a failed or missed pull with backoff until one succeeds, and §6 states the age for both cases.
2. The org grant named no policy form, and the obvious one (`{ucan_required, _}`) matches identity keys by node id,
   refusing every org-issued token: §3 now names `macula_ucan:authorize/3` with `{realm_member_required, OrgKeyId,
   <<"group_keys">>}`, the token's `can` and `with`, and §10 pins a token per profile.
3. "Refused as soon as the tombstone is stored" was a claim a station on the lookup path could falsify by withholding
   the tombstone: §3 now states the guaranteed bound (the endorsement window or the org's removed set), keeps no slot
   answer past one `rotate_after`, and refuses a failed lookup as `membership_unknown` so the member retries.

Taken from the observations: the slot verified at the current time with `AtMs` applied only to the window (§3), the
fleet order (realm first, §3), the lookup's cost in the measurement (§10), one pull per `unknown_epoch` answer (§7),
clock skew in the removal bound (§8), the pinning note (§2), and the register sentence naming the org's groups (§8).

## 13. Changes made while building (C2 to C4, 2026-09-28 and 29)

1. The org UCAN rides the call's own `ucan_token` and is checked by macula under the procedure's advertise policy
   before the handler runs, so the CALL has no `proofs` field (§3, §5).
2. Membership is read at the time of the call for every epoch, past ones included, and the handler takes no `AtMs`:
   a caller that is not a member now has no business reading a past epoch either (§3). This replaces round 2's
   observation that applied `AtMs` to the window.
3. Refusals are `{error, Reason}` answered as `handler_error` with the reason as detail, and `unknown_group` joins
   them for a prefix another org owns (§5). The removed set is the application's `removed` function (§3).
4. Built in C3 and C4 (2026-09-29):
   - The holder is `macula_group_keyring`, one per pool. It stores what it holds in a table read through a handle, so
     publishing, opening a held epoch and reading a policy never wait on a pull; only a pull goes through its process.
   - The link seals a publication, since the seal is bound to the `published_at` it signs, and delivers a sealed
     event unopened, its seal in the meta.
   - A group subscription's events are opened by a process of their own (`macula_group_opener`), because opening may
     pull a missed epoch over the pool, which must not wait on it.
   - `macula:call/6` takes `ucan_token`, the distributor pull's carrier.
