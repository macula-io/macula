# Checklist: releasing `macula` 12.0.0

> **This exists so the things that happen ONCE, at the cutover, happen and are seen to happen.**

A checklist, not a plan. Every item is a thing done once, at or just before the tag, and every item is either done
or not. Where an item needs a reason, it links the decision rather than restating it: if you find yourself
explaining here, the explanation belongs in the decision and this line belongs to be shortened.

Owner: Venus. Raised because the git dependency in `macula-e2e` ends at this tag and there was nowhere to hang
that step.

## Before the tag

⚠ **A configuration describes intent, and only a reading describes the fleet.** Every item below was found by
someone going to the boxes, and each one contradicted what the configuration implied: six configs naming one
certificate path with six distinct certificates behind them; one "fleet memory limit" that is six limits on
differently sized hosts; a box that is the fleet's seed without its own configuration saying so; and — the
strongest of them — **four of six boxes whose configuration we cannot currently trace to any repository we own.**

⚠ **The fleet items below are a DIFFERENT SHAPE OF RISK from the consumer list, and scheduling them the same way
is the mistake this line exists to prevent.** A consumer break fails a build, in front of someone who is watching
and can retry. These stop a box BOOTING, or arrive on a schedule nobody set. They need to be done before the
rolling starts, not discovered during it.

- [x] **The classical-signing ratchet's exception list is empty.** `test/macula_no_classical_signing_tests.erl`,
      `known()` returns `[]`. Confirmed at the tag by RUNNING it rather than reading it, which is the point: the
      scan of `src/` and `native/` found nothing beyond the empty list, 4 of 4 green. The one sanctioned
      classical signature is `allowed()`'s RSA-PSS in `macula_node_keys`, the EU composite's second half.
- [x] **The wire negotiates a post-quantum key exchange group**, which is already proved and needs confirming
      rather than building. ⛔ **Do not try to ask a live QUIC connection what it negotiated.** `quinn` does not
      surface it: `rustls` has the accessor, but `quinn_proto`'s session holds its rustls connection in a private
      field. `macula_quic_pq_kx_tests` says so at length, and chasing it is a day someone can lose.
      The proof is split on purpose: the Rust test `negotiated_key_exchange_group_is_the_one_we_lead_with` in
      `native/macula_quic/src/config.rs` reads the negotiated group directly, and the Erlang side deduces it from
      a completed handshake plus an offer list holding nothing classical.
      ⚠ **So confirm the condition, not the conclusion:** that `macula-pqc`'s list is still exactly its two groups
      with no classical fallback. The moment a fallback is added, a completed handshake is consistent with X25519
      again and the Erlang half stops proving anything.
      **Confirmed at the tag**: `every_configuration_offers_exactly_macula_pqcs_groups` passes, and it asserts
      the exact list on BOTH the listener's and the dialler's provider, so neither can build its own.
- [x] **Invite-only is present and OFF**, for the half that lives here. D31: the field always travels, the
      setting defaults to `off`, and a station with no setting is open.
      `member_endorsement` is in `?CONNECT_KEYS` in `macula_handshake`, which the decoder matches EXACTLY, so it
      always travels; and this repo hands it over without ever checking it. ⚠ **The `invite_only` SETTING is not
      in this repo**: requiring an endorsement is the station's CONNECT check, so "defaults to off" is
      `macula-station`'s to confirm, not something this tag can carry.
- [ ] ⛔ **EVERY station on the fleet holds an ML-DSA-87 TLS certificate and key before it is rolled.** This is a
      cutover item, not a build item: a 12 station with any other leaf **refuses to start**, with
      `{listen_failed, "load private key: ... the PKCS#8 key is not ML-DSA-87"}`. **A Let's Encrypt leaf cannot
      be used.** No new material is needed: a station mints its own from the identity seed it already stores, via
      `macula_quic:generate_self_signed_cert/2`. ⚠ It stops a box BOOTING rather than failing a build, so it is
      the one item on this list that can take the fleet down if it is missed.
- [ ] ⚠ **Each box needs a COMPOSE CHANGE, but NOT the one first written here: a station on 12 does not want
      `/certs` at all.** The self-minting path writes the certificate AND the key into a per-process directory
      under the temp root (`write_temp_cert_pair` in `macula_station_listener`), so both halves name the same
      key by construction and neither goes near the mount. **So the change is "give the station a writable
      directory and stop pointing it at `/certs`", not "make `/certs` writable"**, and the read-only mount is
      not the obstacle. Neptunus, from the port; the temp-root shape is already on the station's main.
      ⚠ The compose files today name `/certs/.../wildcard_.macula.io.crt` through the LEGACY `certfile`/`keyfile`
      path, which 12 refuses outright, so leaving them as they are is what breaks a box, not the mount's mode.
      **What they hold today: six distinct certificates, five EC P-256 and one RSA-2048 on frankfurt, and ZERO
      ML-DSA-87.** No box is already right.

- [ ] ⛔ **A self-signed leaf minted from the node's IDENTITY key is refused, and the refusal is a SILENT STALL.**
      Minting it from the station's TLS key instead is what fixed it. Both halves are measured, from Neptunus's
      port.
      ⚠ **The symptom names nothing**: the dial returns `{ok, Pid}`, the worker never leaves `handshaking`, and
      the listener eventually cuts it as `too_slow`, which reads as a timeout to tune. Proved stuck rather than
      slow by raising the deadline to 60s and getting identical failures.
      ⚠ **THE MECHANISM IS NOT YET NAMED, AND THIS LINE DELIBERATELY DOES NOT GUESS AT ONE.** An earlier version
      of it said the leaf's key must equal the registered TLS key. That may not be the check: the handshake looks
      a leaf up by the SHA-384 of the certificate being SERVED, and what comes back carries no private key, so
      what must agree is the certificate served and the certificate registered. Neptunus is running a contained
      experiment to name the real check; the likeliest candidate, unconfirmed, is a rule that the leaf must not
      carry the identity key at all, which is D12's whole point.
      ⛔ **This goes into the WP 4.x porting notes, because every other-stack SDK meets the same inference from
      the same D6 sentence. It goes in as the OBSERVATION ONLY until the mechanism is named**: five SDK teams
      cannot check our reasoning, and a wrong mechanism stated confidently is worse to them than no mechanism.
- [ ] ⚠ **Do the cutover before the certificates renew, around early October.** Issued early August, expiring
      early November, and **each box runs its own ACME client writing its own certificate**: six independent
      certificates, not one shared file. A renewal rewrites the file under a listener still serving what it read
      at boot, so a cutover that has not happened by then meets a SCHEDULED disturbance rather than a random one.
      ⚠ The evidence for "independent" is **six distinct sha256s**, and it is worth keeping because the configs
      say the opposite: **a shared PATH in six configs is not a shared thing**, and anyone reading the configs
      alone would conclude it was.
      ⚠ **Do not use expiry order to plan the rolling order.** They do not expire together and the longest-lived
      is amsterdam, which carries nothing else that makes it special: expiry order says nothing about cutover
      order.
- [x] **Know what the handshake survives before anyone asks in an incident.** Measured, V21 in
      `PLAN_POST_QUANTUM_SECURITY_PART1.md`: **our client hello spans four to five datagrams**, and the
      handshake completes reliably up to 20% sustained datagram loss in both directions, degrading above that
      into dial timeouts rather than crashes. Recorded because someone will otherwise re-derive it at the worst
      possible moment, and because it is the fact that decides whether a foreign stack on a bad link can reach
      us at all.
- [ ] **The wire version is settled, and the fleet rolls together.** D31's `member_endorsement` forces a version
      bump whether or not anyone turns invite-only on: the frame decoder matches a frame's key set EXACTLY, so an
      added key is `malformed_frame` to a peer that does not know it, and a version bump turns that into
      `unsupported_version`, which says what to do. D31: the wire breaks once, in 12.0.0, not twice. **So no
      station, realm or service crosses the cutover alone**, and the last thing to confirm before the tag is that
      the version on the wire is the one 12 ships.
      **Confirmed at the tag: `?VERSION` is 4** in `macula_handshake`, against the 3 the fleet requires today.
      ⚠ **Unticked because the other half is not done**: no station, realm or service has crossed yet, so
      "the fleet rolls together" is still ahead of us and this item is settled only on the macula side.

- [ ] ⚠ **The six boxes are not interchangeable: frankfurt is the fleet's SEED NAME, and rolling it has a blast
      radius no other box has.** `station-de-frankfurt.macula.io` was retargeted onto it in July, and by
      macula-realm-compose's own comment every warden, the sentinel and capture-archive dial it. So restarting or
      rolling frankfurt removes the endpoint other things bootstrap from, which is true of no other box, and it
      also carries the realm and the portal. **Decide its place in the rolling order deliberately rather than
      alphabetically or by expiry.**
      ⚠ Same shape as the two items above: **a configuration describes intent, and only a reading describes the
      fleet.** Six boxes that look interchangeable in the configs, and one of them is the thing the others find
      each other through.
      ⛔ **And this one is worse than the other two: it is not visible in frankfurt's own configuration at all.**
      The box does not know it is the seed. The fact lives in a COMMENT in a compose file, describing who dials
      it. Reading every file on the box would not find it, so no amount of care on the box answers the question.
      ⛔ **There is ONE repository, `macula-portal-compose`, and a stale duplicate CHECKOUT of it under its
      former name.** `macula-realm-compose` was RENAMED to `macula-portal-compose`: both clones share root
      commit `5d3d795`, the GitHub API returns the new name for the old one, and the old-named clone is simply
      **two commits behind** (`2e0c94b`, the RpcAdvertiser switch, and `9793a4c`). Of their 25 shared file
      names, 23 are byte-identical and the 2 that differ are the files those commits touch. **Nothing forked.**
      ⚠ The hazard is still real and is sharper stated properly: **two working copies of ONE repository on one
      workstation, under two names, one of them behind, both containing frankfurt's station config**, drifting
      further every time the real one moves. A reading taken from the stale copy is right by luck.
      **Name the repo, and check it is current, when you quote either.**
      ⛔ **AND AN OPEN QUESTION THAT MATTERS MORE: a deploy directory name is not a repository name.** The six
      boxes deploy from four differently named directories, and `macula-relay-compose`, `macula-station-compose`
      and `station` **do not exist as repositories anywhere we own**. So for four of the six we cannot currently
      say which repository their configuration comes from. Terra is establishing it, one read-only connection
      per box. **This is the question, not the answer**: do not plan the compose change on an assumption about
      where those directories came from.

### Decided, with its risk: the station memory limits do not change for the cutover

**Not an action item, and it has no checkbox on purpose.** Raf decided on 2026-09-23, with the numbers below in
front of him, that the station memory limits stay as they are through the cutover.

The measured position, Terra on the boxes: **frankfurt runs at 99.99% of a 1 GiB limit, about 400 KB of
headroom**, with over a million cgroup ceiling events and no kills so far, because reclaim keeps winning.
Falkenstein is second at 83.7%. The limits are not uniform — 1.5 GiB on three, 1 GiB on two, 896 MiB on one —
and neither are the hosts. And **12 makes every station heavier**: ML-DSA-87 keys and signatures, a CONNECT of
about 30.8 KB against 21.8 KB, and a handshake of four to five datagrams instead of one.

**So: limits unchanged by decision. Frankfurt is the box to watch first during the cutover**, and it also
carries the realm and the portal. The expected failure mode there is a station that is killed and restarts by
policy, not one that stays down.

⛔ **This was decided, so it is not reopened by someone reading it and proposing a higher limit. Only a NEW
measurement reopens it.**

⚠ **And here is the measurement that would.** A restart of frankfurt's station before the cutover was under
consideration on 2026-09-23. **Its before-and-after would say whether that gigabyte is a WORKING SET or an
ACCUMULATION**, which is the difference between a limit that is merely tight and one that was always going to be
reached. **No such restart has happened and there is no result here to read.** If one is done, its numbers belong
in this line; until then this says only what would settle the question, not what the answer is.

## Every Erlang and Elixir consumer, ported and green

Each is its own repo, its own CI, and its own owner. **Green here means compiled against a `_checkouts` macula
and that repo's own suite passing — NOT an image build**, which cannot happen until the tag puts macula 12 on
hex. See the order under The release.

⚠ **If a break listed here turns out not to be real, SAY SO here rather than working around it or deleting the
line.** The next porter reads this list and not the thread that corrected it, and a list that warns about a
break that does not exist costs them the same hour as one that misses a break that does. Three references
misled a porter on 2026-09-23 alone, each caught by someone USING the document rather than auditing it.

- [ ] **`macula-station`**. ⚠ The `certfile`/`keyfile` break previously listed here **was not real**: the station
      already passes `cert` and `key`; those other names are its own internal map keys, translated in
      `macula_transport`. Left here as a correction rather than deleted, per the practice below.
- [ ] **`macula-realm`** — ⚠ device identity is Ed25519-only in four places (both join proofs, the admission
      lists, the device certificate) and is refused before any mint, so this is not a one-call swap. WP 3.1.
- [ ] **`mcl-om`** — ⚠ the `verify` option is gone from pool and per-call options and is refused by name.
- [ ] **`mcl-echo`**.
- [ ] **`macula-e2e`'s seam suite is green against 12.** It runs on 11.x today by design: the seam tests the stack
      that exists, and moving it to 12 tests the port rather than the harness. See
      `macula-e2e/plans/DESIGN_REALM_SEAM_E2E.md`.
      **State at the tag: green at steps 0 to 7, 4 of 4, against the 11.x stack.**
      ⛔ **And steps 8 to 10 are now blocked on the station's port, not on the realm.** The realm helper exists
      (`MaculaRealm.Testing`, branch `saturnus/macula-12`) and it is built on macula 12, while the seam pins
      `macula ~> 11.4` and the station apps at a sha that pins `~> 11.3`. **Those two cannot connect and are not
      meant to**: `?VERSION` is 3 at v11.5.0 and 4 here, which is the wire break this release exists to make. So
      the order is the station's port, then the seam repinned to 12, then steps 8 to 10 against the 12 realm.
      There is no arrangement of the current pins that runs them, and a seam that appeared to pass across that
      gap would be testing something other than what it claims.

⛔ **`macula-dist-relay` is NOT on this list and is not ported.** It stays on `macula ~> 11.x` (Raf, 2026-09-23).
It consumes `macula_tls:quic_server_opts/0`, which 12 deletes. Do not "finish" it.

## The dependency that ends here

- [ ] **`macula-e2e` moves off its git dependencies to the hex version of `macula`.** The exception that allowed
      git dependencies there, pinned to exact shas, exists only until this tag. Nothing fails when this is missed:
      a stale sha keeps the suite green against an old `macula`, which is why it is on a checklist rather than
      trusted to a check. The suite prints the versions it built against, so a green run names what it tested.

## The release

⛔ **The tag goes AHEAD OF the consumer list, by decision.** Raf, 2026-09-23, with the open items below in front
of him: the tag does not wait for the three unfinished ports or for the seam to be green against 12. The reason
is the order above rather than an exception to it, and it is the ⚠ two paragraphs up made deliberate: an SDK
defect that a port turns up afterwards becomes a 12.0.1, which is cheap because a tag publishes itself. **So the
unticked boxes below are not oversights and are not to be ticked to make the release look ready.**

- [x] **CHANGELOG**: the `12.0.0` section, dated, with `12.0.0-alpha.1` folded into it. One section, since the
      alpha was never published: no tag, never on hex. Its "not post-quantum authentication yet" preamble is
      gone rather than carried forward, because 12.0.0 is post-quantum on both halves and keeping it would have
      shipped a false caveat.
- [ ] **The tag `v12.0.0`.** The tag IS the release: a pushed `v*` tag publishes directly, with no reviewer, by
      design.

⛔ **The order matters and it is not the obvious one, because an image cannot be built before the tag.** A
consumer's image is built in ITS CI, its CI resolves dependencies from hex, and **macula 12 is not on hex until
the tag publishes it**. A committed git dependency on macula is forbidden, so there is no way to build a consumer
image first. The order is therefore:

1. **Consumers proven LOCALLY** against a `_checkouts` macula: compiled and their own suites green. That, and not
   an image build, is what "green against 12" means in the consumer list above.
2. **The tag**, which publishes to hex.
3. **Consumers bump to `~> 12.0`**, and their CI builds images against it for the first time.
4. **The fleet rolls**, with the certificate and compose work above already done.

⚠ **So an SDK defect can first appear at step 3, AFTER the tag.** That is expected rather than a failure of this
list, and it is cheap: it becomes a 12.0.1, and a tag publishes by itself. **Do not try to avoid it with a
release candidate on hex** — that cuts against no-intermediate-releases and leaves every consumer pinned to
something that has to move again.
- [ ] **One announcement, with the release.** No standalone post before it. The post-quantum wording is what D11
      gates, and the claim is what 12 actually does rather than what the plan intends.
