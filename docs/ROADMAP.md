> ⚠️ **OUTDATED (2026-04-20)**: References archived repos
> (macula-neurolab, macula-tweann, macula-os, macula-os-nix,
> macula-nifs, macula-neuroevolution-{esdb,evoq}, macula-console,
> macula-arcade, macula-marketplace, macula-gitops).
> See current repos at rgfaber/faber-* and hecate-social/hecate-*.
> Kept for historical reference only.

# Macula SDK Roadmap

> **See [CHANGELOG.md](../CHANGELOG.md) for the authoritative, up-to-date
> version history.** This file tracks forward-looking plans, not shipped
> versions — it drifts out of sync with actual releases faster than the
> changelog does, and had drifted badly before this rewrite (it previously
> claimed "Current Version: v1.0.0" and "48-module client SDK" long after
> the SDK had passed v9.0 and 100 modules). Current at time of this rewrite:
> v9.8.0, 103 modules under `src/`.

---

## Recently completed (moved out of "planned")

- **Direct-dial across all four supervised primitive pairs** — RPC
  (`macula_request`/`macula_response`), content download/upload
  (`macula_download`/`macula_feeder`), and streaming RPC
  (`macula_stream_sink`/`macula_streamer`) each gained a
  `start_link_direct`/`advertise_direct` mode: resolve a provider from a
  signed DHT record and dial it in one hop, instead of depending on
  advertise-gossip having propagated a route between arbitrary stations.
  Trust is enforced at the application layer — a production station's TLS
  cannot be pinned, since it's terminated by an unrelated PKI — with an
  opt-in X.509 cert-chain check available for managed realms. See the
  [RPC](guides/rpc/RPC_GUIDE.md), [Content](guides/content/CONTENT_GUIDE.md), and
  [Streaming](guides/streaming/STREAMING_GUIDE.md) guides.
- **Connection health / metrics API** — `macula_diagnostics`,
  `macula_metrics` (+ HTTP exporter) ship in the SDK.
- **Subscription persistence across reconnects** — `macula_client_replay`
  re-issues every tracked `(Realm, Topic)` subscription against a
  respawned link.
- **Multi-homed connections** — the pool model dials every configured seed
  simultaneously (N links per pool), not just one relay at a time.
- **Relay/station discovery** — `macula_relay_discovery` exists.

## Open / status unconfirmed

Carried over from an earlier version of this file. Nobody re-verified these
against current code as part of this rewrite — treat as genuinely open, not
as "probably done" just because other nearby items turned out to be done:

- [ ] Precompiled NIF binaries for crypto/UCAN/DID/MRI — the QUIC NIF
  deliberately builds from source rather than shipping a precompiled
  artifact (a bad one once hung every connect fleet-wide); unclear whether
  that reasoning extends to the other NIFs or whether they're precompiled
  today.
- [ ] RTT-based relay selection with periodic re-evaluation.
- [ ] Failover to the *nearest* relay specifically, as opposed to the
  existing first-healthy-link fan-out, which already provides failover —
  just not distance-aware.

## Future (unchanged, still not started)

- [ ] Language-specific SDKs (Go, Rust, Python) via the macula wire protocol
- [ ] WebSocket transport option (browser clients)
- [ ] End-to-end encryption (relay cannot read payload)

---

For relay server roadmap (DHT improvements, peering, SWIM, content transfer),
see macula-station.
