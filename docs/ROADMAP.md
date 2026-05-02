> ⚠️ **OUTDATED (2026-04-20)**: References archived repos
> (macula-neurolab, macula-tweann, macula-os, macula-os-nix,
> macula-nifs, macula-neuroevolution-{esdb,evoq}, macula-console,
> macula-arcade, macula-marketplace, macula-gitops).
> See current repos at rgfaber/faber-* and hecate-social/hecate-*.
> Kept for historical reference only.

# Macula SDK Roadmap

> **Last Updated:** 2026-04-09
> **Current Version:** v1.0.0
> **Status:** SDK/Relay split complete. 48-module client SDK published to hex.pm.

---

## v1.0.0 (Current) -- SDK/Relay Separation

- [x] Split macula into lean SDK (48 modules) + macula-relay (119 modules)
- [x] Absorb macula-nifs (crypto, UCAN, DID, MRI Rust NIFs) into SDK
- [x] Consolidate MRI into single canonical definition with NIF-accelerated trie index
- [x] Publish to hex.pm, archive macula-nifs and macula-ex
- [x] Update all consumers (hecate-daemon, hecate-stub, hecate-app-weather)

## v1.1.0 -- SDK Polish

- [ ] Precompiled NIF binaries for crypto/UCAN/DID/MRI (like Quinn QUIC NIF)
- [ ] Improve SDK documentation with more examples
- [ ] Connection health monitoring API
- [ ] Subscription persistence across reconnects

## v1.2.0 -- Multi-Relay Resilience

- [ ] Automatic failover to nearest relay on disconnect
- [ ] Multi-homed connections (connect to N relays simultaneously)
- [ ] RTT-based relay selection with periodic re-evaluation
- [ ] Connection quality metrics API

## Future

- [ ] Language-specific SDKs (Go, Rust, Python) via macula wire protocol
- [ ] WebSocket transport option (browser clients)
- [ ] End-to-end encryption (relay cannot read payload)

---

For relay server roadmap (DHT improvements, peering, SWIM, content transfer),
see macula-station.
