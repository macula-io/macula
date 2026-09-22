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
- [ ] **The wire negotiates a post-quantum key exchange group.** Not "the groups are configured": the negotiated
      group on a real handshake is one of `macula-pqc`'s two. `native/macula_quic/src/config.rs` holds the check;
      the fact to record is the group a live handshake settled on.
- [ ] **Invite-only is present and OFF.** D31: the field always travels, the setting defaults to `off`, and a
      station with no setting is open.
- [ ] **The wire version is settled, and the fleet rolls together.** D31's `member_endorsement` forces a version
      bump whether or not anyone turns invite-only on: the frame decoder matches a frame's key set EXACTLY, so an
      added key is `malformed_frame` to a peer that does not know it, and a version bump turns that into
      `unsupported_version`, which says what to do. D31: the wire breaks once, in 12.0.0, not twice. **So no
      station, realm or service crosses the cutover alone**, and the last thing to confirm before the tag is that
      the version on the wire is the one 12 ships.

## Every Erlang and Elixir consumer, ported and green

Each is its own repo, its own CI, and its own owner. Green means that repo's own suite, not that it compiles.

- [ ] **`macula-station`** — ⚠ its listener options changed shape: `macula_quic:listen/3` reads `cert` and `key`,
      and its tests carry the old shape too.
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
