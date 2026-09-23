# Checklist: releasing `macula` 12.0.0

> **This exists so the things that happen ONCE, at the cutover, happen and are seen to happen.**

A checklist, not a plan. Every item is a thing done once, at or just before the tag, and every item is either done
or not. Where an item needs a reason, it links the decision rather than restating it: if you find yourself
explaining here, the explanation belongs in the decision and this line belongs to be shortened.

Owner: Venus. Raised because the git dependency in `macula-e2e` ends at this tag and there was nowhere to hang
that step.

## Before the tag

- [ ] **The classical-signing ratchet's exception list is empty.** `test/macula_no_classical_signing_tests.erl`,
      `known()` returns `[]`. It does today; confirm it at the tag, because the list is what a late fix is
      tempted to grow.
- [ ] **The wire negotiates a post-quantum key exchange group**, which is already proved and needs confirming
      rather than building. ⛔ **Do not try to ask a live QUIC connection what it negotiated.** `quinn` does not
      surface it: `rustls` has the accessor, but `quinn_proto`'s session holds its rustls connection in a private
      field. `macula_quic_pq_kx_tests` says so at length, and chasing it is a day someone can lose.
      The proof is split on purpose: the Rust test `negotiated_key_exchange_group_is_the_one_we_lead_with` in
      `native/macula_quic/src/config.rs` reads the negotiated group directly, and the Erlang side deduces it from
      a completed handshake plus an offer list holding nothing classical.
      ⚠ **So confirm the condition, not the conclusion:** that `macula-pqc`'s list is still exactly its two groups
      with no classical fallback. The moment a fallback is added, a completed handshake is consistent with X25519
      again and the Erlang half stops proving anything.
- [ ] **Invite-only is present and OFF.** D31: the field always travels, the setting defaults to `off`, and a
      station with no setting is open.
- [ ] ⛔ **EVERY station on the fleet holds an ML-DSA-87 TLS certificate and key before it is rolled.** This is a
      cutover item, not a build item: a 12 station with any other leaf **refuses to start**, with
      `{listen_failed, "load private key: ... the PKCS#8 key is not ML-DSA-87"}`. **A Let's Encrypt leaf cannot
      be used.** No new material is needed: a station mints its own from the identity seed it already stores, via
      `macula_quic:generate_self_signed_cert/2`. ⚠ It stops a box BOOTING rather than failing a build, so it is
      the one item on this list that can take the fleet down if it is missed.
- [ ] ⚠ **The `/certs` mount is READ-ONLY on every box checked, so minting is not enough: each box needs a
      COMPOSE CHANGE.** A station cannot write a self-minted certificate where it reads one. Measured by Terra
      on the boxes, 2026-09-23. **Five of six confirmed; amsterdam refused ssh and is UNKNOWN rather than
      assumed to match** — check it rather than inferring it from the other five.
- [ ] ⚠ **Do the cutover before the certificates renew, around early October.** Issued early August, expiring
      early November, and **each box runs its own ACME client writing its own certificate**: six independent
      certificates, not one shared file. A renewal rewrites the file under a listener still serving what it read
      at boot, so a cutover that has not happened by then meets a SCHEDULED disturbance rather than a random one.
      ⚠ The evidence for "independent" is **five distinct sha256s**, and it is worth keeping because the configs
      say the opposite: **a shared PATH in six configs is not a shared thing**, and anyone reading the configs
      alone would conclude it was.
- [ ] **Know what the handshake survives before anyone asks in an incident.** Measured, V21 in
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

## Every Erlang and Elixir consumer, ported and green

Each is its own repo, its own CI, and its own owner. Green means that repo's own suite, not that it compiles.

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

⛔ **`macula-dist-relay` is NOT on this list and is not ported.** It stays on `macula ~> 11.x` (Raf, 2026-09-23).
It consumes `macula_tls:quic_server_opts/0`, which 12 deletes. Do not "finish" it.

## The dependency that ends here

- [ ] **`macula-e2e` moves off its git dependencies to the hex version of `macula`.** The exception that allowed
      git dependencies there, pinned to exact shas, exists only until this tag. Nothing fails when this is missed:
      a stale sha keeps the suite green against an old `macula`, which is why it is on a checklist rather than
      trusted to a check. The suite prints the versions it built against, so a green run names what it tested.

## The release

- [ ] **CHANGELOG**: the `12.0.0` section, dated, with `12.0.0-alpha.1` folded into it.
- [ ] **The tag `v12.0.0`.** The tag IS the release: a pushed `v*` tag publishes directly, with no reviewer, by
      design. So everything above is true BEFORE the tag is pushed, not after.
- [ ] **One announcement, with the release.** No standalone post before it. The post-quantum wording is what D11
      gates, and the claim is what 12 actually does rather than what the plan intends.
