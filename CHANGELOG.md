# Changelog

All notable changes to this package are documented here.

Format: [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
Versioning: [Semantic Versioning](https://semver.org/).

---

## [Unreleased] — v2 rebirth

V2 is a from-first-principles rewrite on the `v2` branch. Nothing below 2.0.0 is
released yet. See `plans/PLAN_MACULA_V2_ROOT.md` for scope.

### Added (in progress)

- Umbrella skeleton — 7 SDK apps (`macula`, `macula_identity`, `macula_record`,
  `macula_frame`, `macula_transport`, `macula_peering`, `macula_diagnostics`).
- Quinn QUIC NIF inherited from v1 at `native/macula_quic/`.
- Phase 6.1 — foundation record types (Part 6 §9.14–§9.17):
  `macula_record:foundation_seed_list/2,3`, `foundation_parameter/3,4`,
  `foundation_realm_trust_list/2,3`, `foundation_t3_attestation/3,4` with
  derived storage keys and wire roundtrip coverage.
- Phase 6.1 — `macula_foundation` trust anchor module: firmware-embedded
  pubkey resolution (`pubkeys/0`, `live_pubkeys/0`), `is_foundation/1`
  membership test, `verify_record/1` for the four foundation record
  types, and placeholder-mode detection.

---

## Prior versions

V1 (1.0.0 – 1.4.23) history lives on the `main` branch. See
`git log main -- CHANGELOG.md` for V1 entries.
