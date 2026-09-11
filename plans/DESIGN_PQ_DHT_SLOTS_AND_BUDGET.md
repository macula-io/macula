# DHT slots, slot admission and the verification budget

This exists so a station's record store and verification work stay bounded, an authorized signer can always get a place
in a slot, and an honest relay is never slowed.

By Mars and Mercury, 2026-09-11, from proposal A (Mars), proposal B (Mars and Mercury) and the options note on slot
admission, with Mercury's co-owner corrections and Jupiter's decisions. It details D28 and the D23 refinement in
`PLAN_POST_QUANTUM_SECURITY_DECISIONS.md`, for WP 1.2, WP 1.3, WP 1.5, WP 1.6, WP 2.2 and Stage 4. Records and frames
are in `DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md`.

Prerequisite for part 3: receive-side flow control per stream (section 3.4), which waits for Raf's decision.

## Part 1: Slot bounds

Terms: a slot is a storage key. An entry is one signer key id's current object in a slot, a record or a tombstone.
Replacement by version keeps one entry per signer key id.

### 1.1 Entries per slot

- Types whose storage key names the signer hold one entry per slot by construction: node record, station endpoint,
  foundation seed list, parameter and realm trust list, procedure delegation, domain types. One endorser's records
  across many slots are untouched by any per-slot bound.
- Types whose storage key does not name the signer share a slot among signers: realm directory, realm stations, realm
  member endorsement, procedure advertisement, foundation T3 attestation, content announcement, org directory. Part 2
  sets their places: 64 checked and 16 unchecked in a slot a station can check, and 64 in any other slot.
- Why 64: a caller needs one authorized provider and a fetcher a few sharers, so a slot is a sample, not a census. The
  plan counts 157 advertisements for 117 procedures in Frankfurt's view, about 1.3 per slot. 64 advertisements with
  authorization are 1.4 to 2.2 MB.

### 1.2 A full slot

- A record or tombstone with a higher version from a signer that already holds an entry always replaces that entry.
- An entry past its `expires_at` plus 5 minutes frees its place at the next STORE into its slot, and is never served.
- A record from a new signer that finds no free place of its kind is not stored. No held entry is evicted for it: a
  station keeps a verified entry until it expires, is replaced or is withdrawn (D23).
- Not taken: eviction by nearest expiry, because the signer sets `expires_at`; per-signer limits inside a slot,
  because one entry per signer already holds.

### 1.3 Station totals

- A station keeps two totals of entry bytes: built-in types (tags below 0x20, default 1 GiB) and domain types (default
  3 GiB), so entries of one class never refuse entries of the other.
- At its total, a class stores no new entry, and still takes a renewal that does not grow it.
- 300,000 domain records in the EU profile are about 2.5 GB and fit the default domain total.
- D23 reads: stations keep and serve verified records of any type until they expire, within the per-slot and
  per-class bounds.

### 1.4 STORE_ACK

- Fields: `key`; `signer` (bytes, 32: the key id of the record's `key`); `version` (bytes, 16: the version of the
  record in the STORE); `stored`.
- `stored` is 1 when the station holds this version or a later one from that signer in that slot, and 0 otherwise.
  No reason is sent.
- The sender matches an acknowledgement on `key`, `signer` and `version`, so concurrent STOREs of different signers'
  records into one slot on one connection each get their own answer.
- A STORE whose record bytes equal a held entry's bytes is answered from that entry without verifying again.

### 1.5 VALUE paging

- FIND_VALUE: `key`, `origin`, and optional `after` (bytes, 32), a signer key id.
- VALUE: `key`, `records`, and `next` (bytes, 32), present only when more entries follow.
- A station orders a slot's entries by signer key id, bytewise ascending, whatever their place. A page holds the
  entries after `after`, in that order, as many as fit in 256 KiB of record bytes, and at least one.
- `next` is the signer key id of the page's last entry. Every station orders a slot the same way, so a consumer may
  continue at another station that holds the slot.
- A miss without `after` answers NODES. With `after` and nothing past it, VALUE with no records and no `next`.
- Why 256 KiB: DHT frames travel on the control stream, and a page no larger than the largest single record adds no
  head-of-line delay beyond what one STORE already can. A page carries at least 7 advertisements with authorization or
  35 plain records; 64 advertisements take 6 to 10 pages.
- A consumer:
  - verifies each record and checks that it derives to the requested key;
  - discards an entry at or before `after`, or out of order, and stops reading that station;
  - follows `next` until it has what it needs or `next` is absent;
  - merges pages from several stations, keeping the highest version per signer;
  - reads at most 80 pages from one station per lookup.

### 1.6 Replication

- On storing a new entry or a higher version, a station sends it in one STORE to each of the k closest nodes it knows
  for the key, except the sender. Each receiving station decides the entry's place by its own checks.
- A target that answered `stored` 1 for a version is not sent that version again while it stays among the k closest.
- Every 5 minutes a repair pass sends each entry with more than 10 minutes of life left to the k closest that have not
  answered `stored` 1. Shorter-lived entries travel with their owners' renewals.
- Replication STOREs to one peer use at most a configured rate, default 512 KiB per second. An entry that expires
  before its turn leaves the queue. A new target for 2.5 GB of held entries fills in about 80 minutes.
- Why: resending every held entry to every target on each pass costs the held bytes times k every 5 minutes, about
  58 MB per second per station for 300,000 EU-profile records on an 8-station fleet.

### 1.7 Tests

- One signer's 100,000 domain records across 100,000 slots are all stored.
- In a slot no station can check, 64 signers fill it; a 65th signer's STORE answers `stored` 0 and changes nothing; a
  renewal from a held signer answers 1 and replaces.
- An entry past `expires_at` plus 5 minutes frees its place for the next newcomer and is not served in VALUE.
- A STORE of an equal or lower version answers `stored` 1 and replaces nothing.
- 64 concurrent STOREs into one slot on one connection each resolve to their own acknowledgement.
- A slot over 256 KiB returns pages of at most 256 KiB, each with at least one record, ascending by signer key id.
  Following `next` returns every entry exactly once. A `next` from one station continues at another holding the same
  entries. A 256 KiB record comes alone.
- A consumer discards an entry at or before `after` and a record that derives to another key, and stops following
  that station.
- At a class total, a new entry answers 0 and a same-size renewal answers 1. Domain entries at their total never
  refuse a node record.
- A target that answered 1 is not sent that version again. A node that enters the k closest receives the held
  entries. An entry with under 10 minutes left is not sent by the repair pass. Bytes to one peer stay within the rate
  over a pass with 10,000 held entries.
- Property: under random STOREs, tombstones and expiry, every slot keeps its place bounds and at most one entry per
  signer, each the highest version seen.

## Part 2: Slot admission

This exists so an authorized signer can always get a place in a slot that other keys can also write to.

### 2.1 Trust

- A foundation realm trust list (0x0F) pairs each trusted realm id with its realm key id, and is signed by a
  foundation key.
- A station holds the foundation key ids it is configured with, fetches the trust list signed by one of them, verifies
  it, and refreshes it in the background before it expires.
- A station with no trust list, or with one past its expiry, can check no realm-signed slot and no advertisement slot.

### 2.2 Checkable slots

A station can check a slot of:

- a realm-signed type (realm directory, realm stations, realm member endorsement, org directory) or an advertisement
  whose procedure has an org namespace, when the trust list pairs the slot's realm_id with a realm key id;
- a T3 attestation, when foundation key ids are configured.

Every entry in a slot shares the type and the fields its storage key derives from, the realm id among them, so one rule
applies to the whole slot.

### 2.3 Checked signers

- Realm-signed types: the signer's key id equals the realm key id the trust list pairs with the record's `realm_id`.
- T3 attestation: the signer's key id is a configured foundation key id.
- An advertisement whose procedure has an org namespace and whose authorization holds `org_directory` and
  `procedure_delegation`:
  - both embedded records decode under the decoding rule and verify;
  - the org directory's signer is the realm key id the trust list pairs with the advertisement's `realm_id`, and the
    directory names that `realm_id` and the org namespace byte for byte;
  - the delegation's signer is the `org_key` the directory names, and the delegation names the advertiser;
  - the advertisement expires no later than either.

  These are the checks of `verify_authorization/3`, taking the trust list's pairs instead of one realm key.
- A station verifies each embedded record once per hash of its bytes, as Plumtree does publications, and keeps the
  signature result until that record expires. The comparison with the trust list runs on every STORE, so a realm
  dropped from the list stops giving checked places at once.

### 2.4 Places

- A checkable slot keeps up to 64 places for checked signers and up to 16 for everyone else. An unchecked writer never
  takes a checked place, and nothing is evicted.
- Every other slot keeps 64 places.
- An advertisement authorized by a certificate chain gets an unchecked place.
- When a trust list change makes a slot checkable, or no longer checkable, held entries stay until they expire, are
  replaced or are withdrawn, and new entries follow the slot's current places. A renewal always replaces its signer's
  held entry and takes the place its signer qualifies for now.

### 2.5 What a station never does

- Parse a certificate chain.
- Show callers whether an entry holds a checked place. VALUE carries the records only, and callers run
  `verify_authorization/3` themselves, because a station's trust list can be stale.
- Fetch anything while it handles a STORE. The trust list refreshes in the background, and the embedded records ride
  inside the advertisement.

The Procedure advertisements section of `DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md` states the rule: "A station that
stores or forwards an advertisement verifies its embedded org directory and delegation, once per hash, to decide a
checked place, and never parses a certificate chain or shows that decision to callers."

### 2.6 Cost per STORE

- Realm-signed types and T3 attestations: a lookup and a 32-byte comparison, with no extra verification.
- An advertisement with an org namespace: two verifications the first time its embedded records' bytes are seen,
  about 0.5 ms (US) or 0.9 ms (EU), and a hash lookup on every renewal after that.
- One trust list verification per refresh.

### 2.7 What stays open

- Procedures without an org namespace (whether they need an authorization is Raf's open item, D25), providers
  authorized by a certificate chain, and content announcements (no authority exists for them, D27): their entries hold
  unchecked places only.
- Realms not on a trust list, and stations with no trust list.

### 2.8 Tests

- An advertisement with a valid org directory and delegation for a listed realm takes a checked place in a slot whose
  unchecked places are full.
- In a checkable slot, unchecked writers stop at 16 and never take a checked place.
- A realm-signed record signed by one listed realm's key for another listed realm's `realm_id` gets no checked place.
- Renewals carrying the same embedded record bytes cost no further verification.
- After a realm is removed from the trust list, its signers get no checked place on the next STORE.
- An advertisement authorized by a certificate chain gets an unchecked place, and the station never decodes the chain.
- A VALUE for a slot holding checked and unchecked entries carries only the records' bytes, in signer key id order.
- A station with no trust list keeps 64 places in every slot.
- A slot that becomes checkable keeps its held unchecked entries, and admits no new unchecked entry until fewer than
  16 remain.

## Part 3: Verification budget per connection

This exists so a peer whose objects fail at every hop is read more slowly, while an honest relay is never slowed.

### 3.1 What counts

Each connection has one budget. Each of these costs 1:

- An object refusal that every verifier reaches from the same bytes: the signed object's shape, the carried key's
  form, the signature, the decoding of `tbs`, its keys and field types, `alg`, a signer field that differs from the key
  id, a record over 256 KiB, and a domain record lifetime over 7 days (3.5). Every station verifies an object before it
  forwards it: a request before routing, a reply against the request's target, a relay error only for a pending
  request, a stream frame by key and sequence, a publication at every Plumtree node, and a record on STORE. So an honest
  relay never passes one on.
- A freshness refusal more than 10 minutes past the moment the object's own freshness rule starts refusing it, on
  either side. A relay whose clock is within 5 minutes of the receiver's cannot have accepted such an object, with
  5 minutes to spare.
- A STORE past the connection's record byte allowance (3.5).
- Announcements that add state before the announced object is verified, past the neighbour's allowance:
  - Plumtree IHAVE. A neighbour holds at most 1,024 open missing entries that it announced. An IHAVE past that is not
    recorded, gets no GRAFT, and costs 1. An IHAVE whose GRAFT that neighbour does not answer with a GOSSIP of that id
    within 10 seconds costs 1, and the neighbour is taken off that entry. A GOSSIP that arrives and is refused ends
    the entry and counts under its own refusal.
  - HyParView SHUFFLE, SHUFFLE_REPLY and FORWARD_JOIN, which put node_ids into the passive view before admission is
    checked. A neighbour places at most 20 node_ids per minute, the passive view's default size. Node_ids past that
    are not placed, and each frame that brings node_ids past it costs 1. A SHUFFLE_REPLY that answers no SHUFFLE this
    node sent in the last 30 seconds is not merged, and costs 1.
- Frame table rules, refused as `malformed_frame` like any envelope refusal: a `peer_sample` longer than 7 node_ids; a
  SHUFFLE or FORWARD_JOIN `ttl`, or a FORWARD_JOIN `arwl`, above 8; a `prwl` above `arwl`. A receiver compares a
  FORWARD_JOIN's `ttl` with its own PRWL, never the frame's.

Not counted:

- Refusals that depend on the receiver: freshness up to 10 minutes past the refusal moment, replay and deduplication,
  sequence, a request or stream match, payload rules, provider authorization, slot admission, a full slot or class
  total, and a lower version. These can reach anyone through honest relays.
- Content frames: they carry no signature and are checked by hash at the fetcher.
- Accepted objects. A receiver that already holds an object's exact bytes answers from what it holds without verifying
  again: a held record's bytes, an embedded record's hash, and a received publication's `tbs` hash.

### 3.2 Rate and budget

- One token bucket per connection: 32 tokens, refilled at 1 per second. An honest peer spends none.
- What is counted either costs a sender nothing to produce or breaks an allowance. Bytes that fail a signature need no
  signing, while a validly signed object costs its signer 1.1 ms in the US profile and 5.2 ms in the EU profile, the
  price of about 4 and 11 verifications.
- Verification cost, measured on one core through `macula_node_keys:verify/4` with `scripts/bench-pq-verify.sh`
  (AMD Ryzen 9 5950X, OTP 29, OpenSSL 3.6.4, mean of 2,000 runs):
  - ML-DSA-87, US profile: 0.24 ms at 512 bytes and at 8 KiB, 0.66 ms at 256 KiB;
  - composite ML-DSA-87-PS384, EU profile: 0.45 ms at 512 bytes, 0.46 ms at 8 KiB, 0.75 ms at 256 KiB.
- One connection forces at most about 8 ms (US) or 15 ms (EU) of verification at once for record-sized objects, up to
  24 ms for 256 KiB objects, and under 1 ms per second after that.
- The IHAVE and shuffle allowances sit well above honest traffic. An honest lazy neighbour announces only publications
  it has verified, and a node grafts at once, so its open entries stay near publication rate times one round trip:
  about 100 at 1,000 publications per second and 100 ms. A neighbour that sweeps a publication at expiry just after
  announcing it misses the GRAFT window, which costs 1 token of 32.
- Station links and client connections share these numbers. Counted costs never come through honest relays, so a busy
  station link spends nothing.

### 3.3 How reading slows

- A cost that finds the bucket empty pauses reading on every stream of that connection: 250 ms the first time,
  doubling with each further pause to at most 4 seconds, and back to 250 ms once the bucket has stayed full for
  60 seconds.
- During a pause the node keeps the connection and every request, stream, subscription and view membership on it,
  keeps sending, and sends nothing about the pause. The peer sees QUIC flow control.
- A node does not count silence it caused against the peer: its SWIM probe and suspect timers and its stream idle
  timers for that peer are extended by the pause.
- 4 seconds stays under the station's 6-second SWIM suspect timeout, so a pause makes this node at most suspect in the
  peer's view.
- Plumtree needs no extra rule: a slowed eager neighbour's copies arrive late as duplicates, and PRUNE moves it to lazy.
- Envelope refusals still close the connection.
- Logging about refused objects and announcements stays within one summary line per connection per minute, with a
  count per kind.

### 3.4 Receive-side flow control this needs

- Credit per stream: `setopt(Stream, active, N)` delivers at most N data messages, then one passive notice, and reads
  nothing more until re-armed.
- Unread bytes stay in quinn, so the stream and connection receive windows push back on the sender. Nothing queues in
  the NIF beyond one read chunk (64 KiB).
- Receive windows and the maximum of concurrent streams set per connection, so a paused connection parks at most its
  window of the peer's data (16 MiB per stream and 64 MiB per connection by default).
- A paused connection's process holds at most N unread chunks in its mailbox.

### 3.5 Record bytes per connection, and domain record lifetime

- STORE allowance: each connection has a bucket of record bytes, 16 MiB, refilled at 1 MiB per second, counted before
  verification. A STORE past it is answered `stored` 0 without being verified, and costs 1. New entries and renewals
  cannot be told apart before verification, so both count. An honest client's renewals are small: 30 advertisements
  of 25 KB renewed every minute are 12.5 KB per second.
- Every SDK's put path paces record bytes to at most 1 MiB per second after a 16 MiB burst: macula's DHT put path and
  each Stage 4 stack's. STORE_ACK carries no reason, so an SDK stays under the allowance rather than running into it.
- Why 1 MiB per second: it is twice the replication rate between stations (512 KiB per second, 1.6), so station links
  stay within it. 300,000 EU-profile records, about 2.5 GB, upload over one connection in about 40 minutes. It bounds
  verification from STOREs to about 130 to 140 per second per connection at record size.
- Domain record lifetime, a format rule in the Records section beside the 256 KiB rule: a domain record's `expires_at`
  is at most 7 days after its `created_at`, and every verifier refuses a longer one as malformed. With `created_at` at
  most 5 minutes ahead, no domain record a station accepts expires more than 7 days and 5 minutes after it arrives.
- Why 7 days: it bounds how long any domain entry holds space in a station's domain total, it matches the binding
  lifetime (D22), and renewal stays affordable for a large signer: re-signing 300,000 records once a week takes about
  26 minutes of one core in the EU profile and 6 minutes in the US profile, plus the upload.
- Against D23: the lifetime limit is a rule of the record format, checked by every verifier, not a station policy. A
  record over it never verifies, so no station accepts it, and every record a station does accept is kept and served
  until its own `expires_at`, within the per-slot and per-class bounds. Stations never shorten an accepted record's
  life, and stations and consumers agree on when a record expires. The cost falls on signers of long-lived domain
  records, who renew weekly.
- Not taken: a station that keeps a record at most 7 days after arrival while its `expires_at` says later. Stations
  and consumers would then disagree about expiry.
- Built-in types keep the lifetimes their own rules set.

### 3.6 Tests

- 33 objects with invalid signatures in one burst: all refused, the connection stays open, and the next frame is read
  no sooner than 250 ms later. Repeated bursts double the pause up to 4 seconds; after 60 seconds full it is 250 ms
  again.
- A station forwarding 10,000 objects that the next hop refuses for receiver-dependent reasons (freshness within the
  bound, replay, sequence) is never paused, and a CALL through it at the same time completes in normal time.
- A client sends invalid signatures through station A to station B: A refuses them, and B never pauses A.
- A record at `expires_at` plus 16 minutes costs 1, and at plus 6 minutes does not. A publication at `expires_at` plus
  11 minutes costs 1, and at plus 1 minute does not.
- 1,025 IHAVEs for unseen ids from one neighbour open 1,024 entries and send 1,024 GRAFTs, and the last one costs 1. An
  unanswered GRAFT costs 1 after 10 seconds. An honest lazy neighbour at 1,000 publications per second never spends.
- A neighbour's shuffles place at most 20 node_ids per minute. An unsolicited SHUFFLE_REPLY is not merged. A
  `peer_sample` of 8, or a `ttl` of 9, closes as `malformed_frame`. A FORWARD_JOIN whose `prwl` differs from the
  receiver's PRWL places its new member only when `ttl` equals the receiver's PRWL.
- 10,000 STOREs repeating a held record's bytes cost no verification.
- STOREs past 16 MiB at once, or past 1 MiB per second after that, are answered `stored` 0 without verification and
  cost 1. Replication between two stations at 512 KiB per second never is.
- An SDK bulk upload of valid records into slots with room is answered `stored` 1 throughout.
- A domain record whose `expires_at` is exactly 7 days after its `created_at` is accepted; one a millisecond longer is
  refused as malformed at a station and at a consumer, and costs 1.
- While paused, a peer sending 1 GiB leaves at most N chunks in the receiver's mailbox and its memory within a bound.
- A pause trips none of the receiver's SWIM or stream idle timers for that peer.

### 3.7 Decided

- Fairness of accepted verifications across connections is its own item, later.
- A peer whose clock is off by more than twice the tolerance is slowed.
- Station links and client connections share these numbers, until a measurement shows the need for a setting per role.

