# macula

**Macula SDK v2** — protocol primitives for the Macula mesh.

---

## Status

| Phase | State |
|-------|-------|
| V1 (main branch) | 🧊 Frozen at 1.4.23 — legacy fleet continues running |
| V2 (this branch) | 🏗️ Bootstrapping from first principles |

This branch holds V2 of Macula. V1 is preserved on `main` for the legacy fleet; do not cross-pollinate.

---

## What this is

Macula SDK is the protocol layer. It provides:

- **Identity** — Ed25519 key pairs, S/Kademlia crypto-puzzle NodeIds.
- **Records** — PKARR-compatible signed DNS-like records (CBOR canonical encoding).
- **Frames** — BERT-encoded wire frames (CONNECT, HELLO, GOODBYE, CALL, …).
- **Transport** — QUIC via Quinn NIF (`native/macula_quic/`).
- **Peering** — symmetric CONNECTING → HANDSHAKING → CONNECTED state machine.
- **Diagnostics** — structured event emission + telemetry exports.

Anything that speaks Macula uses the SDK. Station implementations (e.g., [hecate-station](https://github.com/hecate-social/hecate-station)) layer DHT, SWIM, routing, handler dispatch, bootstrap, overlay, and realm directory on top.

---

## Repo layout

```
apps/
├── macula/              % facade (advertise, call, subscribe, connect)
├── macula_identity/     % Ed25519 + crypto puzzle
├── macula_record/       % PKARR + deterministic CBOR
├── macula_frame/        % BERT codec + source-route header primitives
├── macula_transport/    % Quinn NIF wrapper (dial + accept)
├── macula_peering/      % conn state machine (symmetric)
└── macula_diagnostics/  % structured events + telemetry

native/
└── macula_quic/         % Quinn QUIC NIF

plans/
└── PLAN_MACULA_V2_*.md  % design spec
```

---

## Build

```sh
rebar3 compile
rebar3 eunit
rebar3 ct
rebar3 dialyzer
```

The Quinn NIF builds automatically via `priv/build-macula-quic.sh` on first compile (requires Rust toolchain).

---

## Design spec

See `plans/PLAN_MACULA_V2_ROOT.md` and Parts 1–9. All architectural decisions trace back there.

---

## License

Apache-2.0 — see [`LICENSE`](LICENSE).
