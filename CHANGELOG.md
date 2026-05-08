# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [4.2.1] - 2026-05-08

### Changed

- **Bumped QUIC `idle_timeout_ms` and `keep_alive_interval_ms` defaults.**
  - `macula_quic:listen/3`: `idle 120_000 → 300_000`, `keep_alive 30_000 → 15_000`
  - `macula_quic:connect/4`: `idle 60_000 → 300_000`, `keep_alive 20_000 → 15_000`

  The realm's `MeshSubscriber` clients were dying with `:normal` every
  50-60 s and respawning. Each cycle barely completed the
  `find_records_by_type` snapshot RPC before the underlying QUIC
  conn closed peer-side, which left the topology dashboard sparse
  (3 of 10 stations advertised at any moment instead of all 10).

  Root cause: client-side idle was 60 s and snapshot ticks happen on
  a longer cadence, so post-snapshot the conn went idle long enough
  for Quinn's idle-close to fire. Higher idle + more frequent PINGs
  closes the loophole. PING traffic also resets the peer's idle
  timer, so connections survive on either side's headroom.

  Callers that explicitly pass `idle_timeout_ms` or
  `keep_alive_interval_ms` are unaffected.

---

## [4.2.0] - 2026-05-08

### Changed

- **`{macula_peering, handshake_complete, ...}` notification now
  carries the verified `peer_node_id`.** The message sent to a
  worker's `accept_owner' pid changed from
  `{macula_peering, handshake_complete, ConnPid}` to
  `{macula_peering, handshake_complete, ConnPid, PeerNodeId}`, where
  `PeerNodeId` is the Ed25519 pubkey extracted (and signature-
  verified) from the inbound CONNECT/HELLO frame.

  Lets accept-side listeners dedupe duplicate dials from the same
  peer identity. Without it, a peer that re-dials before its prior
  connection has been torn down (by client-side handshake timeout,
  network partition, or process restart) accumulates a fresh
  `connected'-state worker on every retry. Production stations have
  been observed at 99 stuck `connected' workers from a single
  sister-station because each dial completes the handshake, the
  prior worker holds its QUIC conn open until idle-timeout, and
  nothing dedupes them.

  See `macula-station' commit pairing this release for the
  listener-side dedupe consumer.

### Removed

- **Yggdrasil module + sovereign-overlay `{pubkey, ...}` dial form.**
  `macula_yggdrasil' and the `macula_quic:connect({pubkey, Pk}, ...)'
  / `macula_peering_conn:do_connect(#{pubkey := Pk})' clauses are
  gone. No callers remain in the codebase; `macula-net' replaces
  yggdrasil as the routing substrate. Self-signed pubkey-anchored
  cert generation (`macula_quic:generate_self_signed_cert/3') stays
  — it has live consumers in `macula_net_transport_quic' and
  `macula_station_listener' that wrap an Ed25519 keypair without
  any Yggdrasil-derived address.
- **Dead test files.**
  - `test/macula_quic_tests.erl' — tested the retired `quicer'-style
    API surface (`accept/2', `recv/2', `accept_stream/2', etc.) that
    the Quinn NIF does not expose.
  - `test/macula_quic_idle_timeout_tests.erl' — tested `quicer'
    proplist option format.
  - `test/macula_yggdrasil_tests.erl' — paired with the deleted
    module above.
  - `test/macula_client_test_server.erl' — helper used only by the
    gateway tests below.
  - `test/macula_gateway_system/' — entire directory, 13 test files,
    targeted the V1 gateway surface fully retired in 4.0.0.

### Breaking

- **`accept_owner` consumers must update their pattern.** Any code
  matching `{macula_peering, handshake_complete, Pid}` no longer
  matches; the message is now a 4-tuple. Match on
  `{macula_peering, handshake_complete, Pid, _PeerNodeId}` or use
  the `PeerNodeId` for dedupe.

  Only `macula_station_listener' in `macula-station' currently
  consumes this message; that consumer is updated in the paired
  release.

  No other behaviour change for callers that don't pass
  `accept_owner'.

---

## [4.1.1] - 2026-05-07

### Fixed

- **Handler returning `{error, _}` no longer crashes the peering
  gen_statem.** Pre-4.1.1, `safe_invoke_handler/4` in
  `macula_station_link` wrapped any non-crash return in a `RESULT`
  frame whose `payload` was the raw return value. When a handler
  returned `{error, Reason}` (e.g. `_dht.put_record` returning
  `{error, bad_signature}` for a record that fails verification),
  the resulting `RESULT` frame ended up at
  `macula_record_cbor:encode/1` with a tuple as a payload value;
  the encoder has no clause for raw tuples and the peering
  state-machine terminated with `error:function_clause` at
  frame-sign time. Every other multiplexed RPC on the same QUIC
  connection died with it. This bit production immediately when
  station↔station DHT replication started shipping records that
  failed downstream verification: each replication attempt killed
  the connection that any nearby caller (including realm
  topology subscribers) was multiplexed onto.

  Now `{error, Reason}` is funneled into a BOLT#4 `call_error`
  frame with `code = 0x0F unknown_error` and `detail` set to the
  `~p`-formatted Reason (capped at 256 bytes). Handler crashes
  continue to map to `code = 0x02 temporary_relay_failure`. The
  `normalise_reply/1` function lost its now-dead `{error, _}`
  clause.

  Existing test
  `inbound_call_handler_error_tuple_passes_through_as_result_test_`
  asserted the buggy shape and was renamed to
  `inbound_call_handler_error_tuple_emits_call_error_test_` with
  updated expectations: the test now demands a `call_error` frame
  with code `0x0F` and a binary `detail` that includes the
  formatted Reason. 35 station_link eunit tests still pass; full
  suite parity (1622 passed / 10 pre-existing failures, unchanged).

  Affected files:
  - `src/client/macula_station_link.erl` — `safe_invoke_handler/4`,
    `normalise_reply/1`, new helper `format_error_detail/1`
  - `test/macula_station_link_tests.erl` — test rename + body

---

## [4.1.0] - 2026-05-06

### Added

- **`accept_owner` opt on `macula_peering:accept/2` and `connect/1`** —
  optional pid that receives a single
  `{macula_peering, handshake_complete, ConnPid}` message the moment
  the worker transitions from `handshaking` to `connected`. Distinct
  from `controlling_pid`, which receives the
  `connected`/`frame`/`disconnected` event stream. Lets an accept-side
  listener cap concurrent *handshaking* workers separately from
  healthy connected peers — the original intent of the cap, before
  stub fan-out filled it with verified peers and starved
  station↔station handshakes (see macula-station 4beb2f5 for the
  matching cap-bump fallback).

### Notes

- Pure addition; no behaviour change for callers that don't pass
  `accept_owner`.

---

## [4.0.0] - 2026-05-06

Major release. **Breaking.** V1 surface fully retired; pool-aware
streaming RPC ships; the `macula_stream_v1` module renamed.

### Removed

- **macula_mesh_client** — V1 single-connection client. Deleted.
- **macula_multi_relay** — V1 multi-relay wrapper. Deleted.
- **V1 facade entry points on macula.erl** — every form taking a
  V1 client pid as its first argument:
  - `disconnect`
  - V1 client-pid forms of `subscribe`, `publish`,
    `unsubscribe`, `call`, `advertise`, `unadvertise`
  - V1 REMOTE forms of `call_stream` and `advertise_stream`
    (LOCAL in-process forms preserved)
  - V1 client-pid forms of `put_record`, `find_record`,
    `find_records_by_type`, `subscribe_records`,
    `unsubscribe_records` — replaced with V2-shaped entries on
    the same names (see *Changed*)
  - The entire V1 directed-RPC block: `call_node`, `resolve`,
    `list_nodes`
  - The `client/0` type alias
- **V1 carrier branch in macula_stream** — the `{remote, _, _}`
  peer shape, `attach_remote/3` export, and `send_remote/4`
  dispatch path are gone. The module now spans only LOCAL
  in-process pairs and V2 station-link carriers.
- **V1 test files**: `macula_mesh_client_validate_tests.erl`,
  `macula_multi_relay_tests.erl`, `macula_stream_remote_tests.erl`.

Net deletion: ~2700 LOC.

### Added — pool-aware streaming RPC (A4)

Streaming RPC now rides the V2 pool. Five new STREAM_* wire frames
(`stream_open`, `stream_data`, `stream_end`, `stream_error`,
`stream_reply`) in `macula_frame`, plus per-station and pool
surfaces:

- `macula:call_stream/5` — open a stream against a V2 pool.
  Sticky-to-link: the returned stream is bound to the link the
  pool picked; if that link dies the stream errors with
  `peer_down` and the caller re-opens.
- `macula:advertise_stream/5` — fan-out streaming-procedure
  registration across every healthy link in the pool. Replayed
  on link respawn.
- `macula:unadvertise_stream/3` — drop a streaming advertisement.
- Per-link API on `macula_station_link`: `call_stream/5`,
  `advertise_stream/5`, `unadvertise_stream/3`,
  `send_stream_frame/3`.
- Pool API on `macula_client`: `call_stream/5`,
  `advertise_stream/5`, `unadvertise_stream/3`. Plus an internal
  replay helper that re-issues stream advertisements when a link
  respawns.

29 new eunit tests cover frame round-trips, per-station gating,
pool fan-out, replay, and disconnect cleanup.

### Changed — DHT entries

`put_record / find_record / find_records_by_type /
subscribe_records / unsubscribe_records` keep their names but now
take a V2 pool as the first argument (was a V1 client pid). DHT
traffic travels under the all-zeros realm tag
(`?DHT_REALM = <<0:256>>`), the SDK convention for
protocol-internal infrastructure traffic.

### Changed — macula_stream rename

The `macula_stream_v1` module is renamed to `macula_stream`. The
"v1" suffix referred to the V1 wire format the gen_server originally
bridged via `macula_mesh_client`; A4 extended the same gen_server
to carry V2 streams as well, and the V1 retirement removed the
mesh_client carrier entirely. The module now spans LOCAL pairs and
V2 station-link pairs only — the suffix had become misleading.

External consumers using `macula_stream_v1:*` directly must rename
to `macula_stream:*`. No semantic change.

### Changed — macula_dist_relay ported to V2 pool

Erlang-distribution-over-mesh stays. Its plumbing moves from V1
`macula_mesh_client` / `macula_multi_relay` to the V2
`macula_client` pool.

- `register_mesh_client / get_mesh_client` on `macula_dist_relay`
  renamed to `register_mesh_pool / get_mesh_pool`.
- `persistent_term` key `macula_dist_mesh_client` →
  `macula_dist_mesh_pool`.
- `extract_payload` on `macula_dist_relay` deleted; V2 events
  deliver Payload directly in the message tuple, no map-or-binary
  unpacking needed.
- `macula_dist_bridge` state field `client / client_mon` →
  `pool / pool_mon`; args map key `client => Client` →
  `pool => Pool`

Realm tag: dist tunnel frames travel under the all-zeros realm
(matches the DHT convention; protocol-internal infrastructure).

### Migration

Workspace consumers that referenced V1 (hecate-daemon,
hecate-app-weather, mesh_chat) were ported in lockstep across
their respective repositories before this release; nothing in the
canonical workspace should break on the bump.

External consumers must:

1. Replace `macula:connect/2` call-sites that destructured the
   result as a V1 client. The handle is now a pool.
2. Add a 32-byte realm tag to every `subscribe`, `publish`,
   `call`, `advertise`, `unadvertise`, `call_stream`,
   `advertise_stream`, `unadvertise_stream` call-site. Use
   `macula_realm:id(BinaryName)` (SHA-256) or your own derivation.
3. Switch pubsub callbacks to pid-receivers. V2 delivers
   `{macula_event, SubRef, Topic, Payload, Meta}` to a pid; the
   former 1-arg callback shape is available via
   `macula:subscribe_callback/4` if you need to keep callback
   semantics.
4. Rename `macula_stream_v1:*` → `macula_stream:*` if your code
   reached past the facade.

See `docs/migrations/V1_TO_V2_PUBSUB.md` for detailed examples.

---

## [3.16.0] - 2026-05-06

Daemon-driven additive release. Five SDK gaps surfaced during the
hecate-daemon V1→V2 migration drafting (`PLAN_DAEMON_V2_MIGRATION.md`
in hecate-daemon) land here as purely additive APIs. No breaking
change; every 3.15.x consumer continues to work unchanged.

The remaining gap (pool-aware streaming RPC) is deferred to 3.17.0
along with the full SDK quality sweep. See
`docs/PLAN_SDK_3_17.md` for the deferred scope.

### Added

- `macula:status/1` and `macula_client:status/1` — aggregate health
  snapshot of a V2 pool. Returns a map with `seeds`, `healthy_links`,
  `failed_links`, `self_node_id`, and `subscriptions`. Single round-
  trip plus one `is_connected` probe per spawned link (each capped
  at 1s by the link's own gen_server). Suitable for `/health` or
  `/status` endpoints.

- `macula:subscribe_callback/4` and `macula_pubsub:subscribe_callback/4`
  — callback-shim atop the message-based `subscribe/4`. Spawns a
  small monitored receiver internally; invokes the callback once
  per inbound event. A crashing callback is logged and the next
  event is delivered (rationale: a transient bug in event handler N
  must not lose events N+1..M). Receiver exits when the caller dies
  or `unsubscribe/2` clears the sub.

- Pool-aware non-streaming RPC:
  - `macula:call/5` — first-success across the pool's healthy links.
    Returns `{error, no_healthy_station}` when no link has completed
    `CONNECT/HELLO`. Per-link errors fall through to the next.
  - `macula:advertise/5` — fan-out advertise on every healthy link
    AND store in pool state for replay on link respawn. Arity 5 to
    avoid colliding with the legacy V1 `advertise/4`.
  - `macula:unadvertise/3` — best-effort fan-out drop, always
    clears local state.
  - macula_client_replay:advs_to/2 — advs replay helper, mirrors
    the existing subs_to/2.

- `macula_client:opts()` type spec gained per-key documentation.
  V1-only opts (`relays`, `realm`, `site`, `connections`) trigger a
  one-shot `logger:notice` listing the silently-ignored keys when a
  caller passes them; the pool boots normally. See `macula:connect/2`
  for the full V1→V2 opts mapping.

- `macula_client` re-exports the `handler()` type. Avoids consumers
  reaching into the private `macula_station_link` module.

### Documentation

- `macula:connect/2` doc gained a "V1-only opts" section calling out
  each silently-ignored key with its V2 equivalent.
- `macula_client:opts()` and `macula_client:status/1` documented per
  key / per field.
- `macula_pubsub:subscribe_callback/4` documented including the
  callback-crash semantics.

### Tests

19 new eunit tests across `macula_client_tests` and
`macula_pubsub_tests`:
- 4 for `status/1` (empty pool, unreachable seeds, subscription
  count, facade delegation)
- 4 for `subscribe_callback/4` (happy path, callback-crash survival,
  arity guard, caller-death cleanup)
- 7 for pool RPC (`call/5`, `advertise/4`, `unadvertise/3`, facade
  delegation, handler-arity guard)
- 1 for V1-legacy opt warning
- 2 for dedup window/sweep tunable end-to-end

### Verification

- `rebar3 compile` — clean
- `rebar3 dialyzer` — clean (89 files)
- `rebar3 ex_doc` — exit 0 (2 cosmetic warnings about historical
  CHANGELOG entries with underscored module names tripping ex_doc's
  italic parser; they do not affect any post-3.11 entry or any API
  surface)
- All 743 baseline tests still pass; one pre-existing teardown flake
  (`macula_multi_relay_tests:status_test` / `stop_test/1`) unchanged.

## [3.15.3] - 2026-05-05

### Fixed

- `macula_peering_conn:on_handshake_enter_client/2` crashed with
  `badmatch` when `macula_quic:setopt/3` or `macula_quic:send/2`
  returned `{error, _}`. This is a normal race: the QUIC connection
  can die between `nif_connect` returning `{ok, Conn}` and the
  client gen_statem entering its `handshaking` state (peer closes,
  network drops, server sends `CONNECTION_CLOSE` after TLS but
  before the first stream). Pre-3.15.3 this crashed the
  peering_conn supervisor child with a `badmatch {error, <<"connection lost">>}`
  and dumped a stacktrace per attempt. Now: surface `disconnected`
  with a structured `{setopt_failed | send_connect_failed, Reason}`
  and let the caller schedule a reconnect via the standard backoff
  path.

  Discovered during BE station fleet on falkenstein
  (2026-05-05) — every concurrent outbound dial that completed the
  TLS handshake but then failed at the application layer crashed
  the gen_statem and accumulated SUPERVISOR crash reports.

- `macula_quic:setopt/3` spec widened from `ok` to
  `ok | {error, term()}`. The NIF surfaces errors when the stream
  handle is stale or invalid; the narrow spec made dialyzer
  reject defensive `{error, _}` matches in callers.

## [3.15.2] - 2026-05-05

### Fixed

- `macula_station_link` SDK specs widened to admit `{error, term()}`
  returns. The wrappers around `gen_server:call/3` (`subscribe/4`,
  `unsubscribe/2`, `advertise/4`, `unadvertise/3`) declared narrow
  return types (`{ok, reference()}` / `ok`) but in reality dispatch
  to an arbitrary `pid()` and surface `{error, unknown_call}` (or
  any other reply) when the target gen_server does not implement
  the call. Callers that pattern-matched only the success shape in
  a `try ... of` (no wildcard) crashed with `try_clause` — silent
  bug until consumers passed non-conforming pids alongside SDK
  link clients (e.g. macula-station's seed-dial outbound link
  workers). Now:

  ```erlang
  subscribe/4   -> {ok, reference()} | {error, term()}
  unsubscribe/2 -> ok | {error, term()}
  advertise/4   -> ok | {error, term()}
  unadvertise/3 -> ok | {error, term()}
  ```

  No runtime behaviour change — these are spec-only widenings.
  Consumers should add a wildcard `_Other -> ...` clause when
  pattern-matching the return value, since
  `try ... of {ok, X} -> ... catch _:_ -> ... end` does NOT catch
  the `try_clause` exception raised by an unmatched `of` pattern.

## [3.15.1] - 2026-05-02

### Fixed

- `macula_quic:nif_connect/8` rejected every call with `badarg`. The
  Rust signature took `verify_pubkey: Vec<u8>` but rustler's `Vec<T>`
  decoder only accepts list terms, never binaries — so every caller
  passing a binary (which is every caller) blew up at the decode
  boundary. Switched to ``Binary<`a>`` mirroring
  `cert.rs:nif_generate_self_signed_cert`. Affects every
  `macula_quic:connect/4` user, not just macula-net.
- `macula_net_transport_quic` ignored every inbound stream byte: the
  data-arrival pattern matched `{quic, data, Stream, Data}`, but the
  NIF emits `{quic, Binary, StreamRef, Flags}` (mirroring quicer's
  shape — see `native/macula_quic/src/message.rs`). Fixed the clause
  guard.

### Added

- `test/macula_net_transport_quic_e2e_tests.erl` — two-node QUIC
  envelope round-trip via `peer:start_link/1`. Catches both bugs above.
- `test/macula_net_full_stack_e2e_tests.erl` — full pipeline: node A
  `macula_route_packet:dispatch` → QUIC → node B
  `macula_deliver_packet:handle_envelope` → captured payload, asserted
  byte-identical.

## [3.15.0] - 2026-05-02

### Added — macula-net L3 substrate (Phase 1)

First slice of the sovereign-IPv6 substrate per `PLAN_MACULA_NET.md`
(macula-architecture). Macula now owns its own crypto-derived IPv6
addressing layer; identities (stations + daemons) become first-class
endpoints in the host's standard networking stack.

New slices in `src/`:

- `derive_address/macula_address` — pubkey -> IPv6 (BLAKE3, ULA prefix).
  Reuses `macula_blake3_nif`; no new NIF.
- `manage_tun_device/macula_tun` + `macula_tun_nif` — Linux TUN
  lifecycle + packet I/O via Rust NIF (`tun-rs`). Reader thread pumps
  packets to a registered BEAM Pid as `{macula_net_packet, ...}`
  messages.
- `route_packet/` — egress. `macula_route_packet_ipv6` parses the
  IPv6 fixed header; `macula_route_packet` looks up dst in a static
  station table and dispatches the CBOR envelope to the station's
  transport callback.
- `deliver_packet/macula_deliver_packet` — ingress. Decodes the CBOR
  envelope (via `macula_cbor_nif`), validates, writes inner IPv6
  packet to the local TUN if dst is local.
- `macula_net/` — facade + `macula_net_transport` behaviour +
  `macula_net_transport_quic` (Quinn-based, uses the SDK's existing
  `macula_quic` primitives — no new QUIC NIF).

New native crate: `native/macula_tun_nif/` (rustler 0.34, tun-rs 2).
Linux only for Phase 1.

29 new eunit tests across the slices; all existing tests pass.

Phase 1 simplifications (deferred to Phase 4 hardening): static station
table (no DHT yet — Phase 2), single-hop only, self-signed throwaway
TLS certs, ctrl/gossip envelope types accepted but not handled.

The repo `macula-io/macula-net` (where this work was prototyped) has
been folded into this SDK and deleted.

---

## [3.14.0] - 2026-05-02

### Added — Sovereign-overlay (Yggdrasil) building blocks

Phase 1 Tier 3 of the sovereign-overlay rollout — see
`PLAN_SOVEREIGN_OVERLAY_PHASE1.md` (macula-architecture) §4.2-§4.4.
This release delivers the SDK-side primitives that let stations
present, and daemons validate, a pubkey-anchored QUIC identity
with no DNS, no Let's Encrypt, no CA chain.

New module `macula_yggdrasil`:

- `address_for/1` — derive the Yggdrasil IPv6 (200::/7) from a
  raw 32-byte Ed25519 pubkey. Matches yggdrasil-go's
  `AddrForKey` reference exactly. Verified against the live
  3-relay fleet's pubkeys/addresses (Helsinki, Nuremberg, Paris).
- `format_address/1` — 16-byte IPv6 binary → canonical
  colon-separated string.
- `cert_for/1,2` — generate a self-signed X.509 cert wrapping an
  Ed25519 keypair. The derived Yggdrasil IPv6 lands as IP SAN;
  optional extra DNS SANs supported. Cert validity 10 years.

NIF additions in `macula_quic` (Quinn QUIC):

- `generate_self_signed_cert/3` via `rcgen` 0.13. Takes raw
  Ed25519 pubkey + secret seed + SAN list, returns
  `{ok, {CertPem, KeyPem}}`.
- `PubkeyPinVerifier` — rustls `ServerCertVerifier` impl that
  pins on the leaf cert's Ed25519 SubjectPublicKeyInfo rather
  than walking a CA chain. Equivalent of TLS RFC 7250
  raw-public-key without the wire-protocol change.
- `build_client_config` gains `Option<Vec<u8>> pinned_pubkey`.
  None preserves existing webpki/skip behaviour.

Erlang dial-target syntax extension:

- `macula_quic:connect/4` accepts `{pubkey, Pk32 :: binary()}`
  as a target in addition to the existing host string. Derives
  the Yggdrasil IPv6, sets the verify_pubkey opt, dispatches
  through the standard nif_connect path.
- `macula_peering_conn:do_connect` recognises the same shape
  via a `pubkey` key on the target map.

NIF connection layer:

- `nif_connect` now takes an additional `verify_pubkey: Vec<u8>`
  parameter (arity 7 → 8). Empty binary disables pinning.
- `[ipv6]:port` host strings are supported via bracket-stripping
  before `lookup_host` and SNI assignment.

### Notes for downstream consumers

- `macula_quic:connect/4` ABI is unchanged; the new
  `verify_pubkey` opt is opt-in, defaults to `<<>>`.
- `nif_connect` arity bumped 7 → 8. Anyone shipping a NIF .so
  built against the 3.13 Erlang module needs to ship the 3.14
  `.so` together. Mixing produces
  `{bad_lib, "Function not found macula_quic:nif_connect/7"}`
  on load.
- New crate deps in `macula_quic`: rcgen 0.13 (pem+ring),
  x509-parser 0.16, time 0.3.

---

## [3.13.0] - 2026-04-28

### Added — V2 ADVERTISE/UNADVERTISE wire frames + station_link advertise API

Closes the V2-fleet fresh-install blocker. macula-realm could not
register RPC procedures over the V2 wire because the protocol only
exposed CALL/RESULT/ERROR. Realms had to keep advertising via V1
`:macula.advertise`, but V1 frames are silently dropped by V2
listeners (visible as `_realm.membership.join_with_token_v1` hanging
on every fresh daemon's join).

`macula_frame` gains two new frame types:

- `advertise/1` — `(realm, procedure, advertiser, options)`, signed
  by the advertiser. The connected station registers
  `(realm, procedure)` in its per-connection routing table so
  inbound CALL frames matching that key are forwarded back across
  the advertiser's QUIC connection.
- `unadvertise/1` — `(realm, procedure, advertiser)`. Drops the
  registration. Idempotent. Implicit on peer disconnect (the
  station's `peer_observer` purges every entry whose `conn_pid`
  equals the dropped connection).

`macula_station_link` gains:

- `advertise/4` — `(Pid, Realm, Procedure, Handler)`. Registers the
  handler locally and sends an ADVERTISE frame on the wire. Queued
  until HELLO completes (drained on `connected` alongside pending
  subscribes). Handler signature mirrors `hecate_handler_dispatch`:
  `{ok, Reply}` / `{error, Reason}` / bare value, with crash trap
  mapping to BOLT#4 `temporary_relay_failure` (0x02).
- `unadvertise/3` — `(Pid, Realm, Procedure)`. Best-effort wire
  frame, always clears the local handler.
- Inbound CALL handling: `(realm, procedure)` matched against the
  local procedure map, dispatched, RESULT/ERROR shipped back. An
  unmatched procedure produces a signed `unknown_next_peer` (0x01)
  reply.
- Replay on reconnect: every advertised procedure re-emits ADVERTISE
  on `(Pid, connected, ...)`, mirroring `drain_pending_subscribes`.

Wire frame round-trip and SDK behaviour covered by 13 new tests
(7 station_link + 6 frame). All 122 frame tests + 26 station_link
tests pass; dialyzer clean.

The companion station-side routing lives in
hecate-station (renamed to macula-station 2026-04-30):
new `hecate_remote_advertise_registry` plus modifications to
`hecate_station_peer_observer` to forward CALLs across the
advertiser's connection and relay RESULT/ERROR back.

---

## [3.12.1] - 2026-04-28

### Fixed — macula_station_link:call/5 gated on completed handshake

The `{call, ...}` `gen_server` clause was gated on `peer_pid`, which
is set the moment `macula_peering:connect/1` returns — **before** the
peering worker has finished the CONNECT/HELLO handshake. The
matching `{publish, ...}` clause is correctly gated on `peer_node_id`
(set by the `{macula_peering, connected, ...}` notification after
HELLO).

The race: a caller (e.g. a freshly-spawned daemon stub) issues
`put_record/3` immediately after `start_link/1`. The link forwards
the call frame via `macula_peering:send_frame/2` =
`gen_statem:cast(PeerPid, {send_frame, Frame})` while the peering
worker is still in `handshaking`. The `handshaking` state has no
clause for `cast({send_frame, _})`, so the cast falls into
`drop_unexpected/4` and the frame is silently dropped. The caller's
deadline timer eventually fires and surfaces `{error, timeout}`,
even though the underlying QUIC connection is healthy and any
subsequent call (after the timer's wake-up) would have succeeded.

The fix gates `{call, ...}` on `peer_node_id` to match `{publish, ...}`.
Callers that issue a request before the handshake completes now get
`{error, not_connected}` immediately, matching the SDK's documented
contract for the disconnected case. Existing call sites (e.g.
`hecate_stub_daemon`) already handle `{error, not_connected}` with a
short backoff, so no consumer change is required.

Direct evidence of the bug from the production fleet — handshaking
peering_conn workers on relay boxes carry buffers that successfully
parse as V1 wire frames (a separate problem in `hecate-daemon`'s
unfixed realm-join path), but the V2-protocol stub workers also
showed timeout-then-recycle cycles on every put_record.

---

## [3.12.0] - 2026-04-28

### Added — `peers` opt on `node_record/4` for overlay topology

`macula_record:node_record_opts()` now accepts an optional `peers`
field — a list of 32-byte pubkey binaries identifying the stations
this node currently has an active overlay session with.

When non-empty (`undefined` or `[]` keep the field absent), the list
is dropped into the canonical CBOR payload at
`{text, <<"peers">>}` after `lists:usort/1` deduplication + sort. The
deterministic ordering preserves the signature-stable property of the
existing canonical form: the same set of peers always encodes to
identical bytes regardless of insertion order.

Records that omit the field (older publishers, daemons, anyone who
doesn't supply `peers`) round-trip exactly as before — the new
clause in `node_payload/5` is a no-op when the opt is absent.

Consumers (e.g. realm topology dashboards) join the list against
their station view to draw relay-to-relay edges without a
side-channel topology poll. `hecate-station 896d6b5+` populates the
field at announce time from each per-identity `hecate_station_peer_observer`.

---

## [3.11.1] - 2026-04-27

### Fixed — `macula_record_cbor:encode/1` accepts atoms

`encode/1` previously crashed with `function_clause` when handed a
map containing atom keys. In production this manifested when the
station's `_dht.put_record` handler called `macula_record:verify/1`
on a wire-decoded record:

  * macula_frame:from_wire_envelope/1 atomizes binary keys via
    `binary_to_existing_atom/1` (the safe variant — only known
    atoms become atoms; unknown ones stay as `{text, Bin}` or
    binary).
  * Recognised payload keys like `hostname`, `endpoint`, `kind`,
    `node_id`, `city`, `country`, `lat`, `lng`, `capabilities`
    are all SDK-level atoms (declared in `node_payload/5`), so
    they DID get atomized.
  * `verify/1` then re-encodes the envelope for signature check,
    walking the payload sub-map. The encoder's `function_clause`
    fired at the first atom key, the handler crashed, and the
    daemon's announcer saw `{call_error, 2, temporary_relay_failure}`
    on every refresh.

The fix adds a clause `encode(A) when is_atom(A) -> ...` that emits
the atom's UTF-8 name as a major-3 text string. By the symmetry of
`atom_to_binary/1` / `binary_to_existing_atom/1` the resulting wire
bytes are byte-for-byte identical to the original record's encoding,
so signature verification succeeds.

`null` retains its dedicated `<<16#F6>>` clause (major-7 simple
value); the atom clause is matched only after `null`.

### Tests

  * 4 new EUnit cases in `macula_record_cbor_tests`:
    - `encode_atom_emits_text_string_test`
    - `encode_atom_in_map_keys_test`
    - `encode_null_still_uses_simple_value_test`
    - `verify_round_trip_with_atomized_payload_test`
      (full `node_record` build → sign → atomize-keys (mimicking
       macula_frame:from_wire_envelope) → verify returns `{ok, _}`).

### Consumer impact

`hecate-station`, `hecate-daemon`, and `macula-realm` all pin
`{macula, "~> 3.11.0"}`, so 3.11.1 is auto-allowed; refresh each
consumer's lock (`rm rebar.lock` or `mix deps.update macula`) and
push to trigger a rebuild.

---

## [3.11.0] - 2026-04-27 — Phase 1 of `PLAN_V2_PARITY`

### Added — `macula_client` pool (canonical V2 client handle)

`src/client/macula_client.erl` is the new canonical SDK client. It
holds N peering links to N stations and routes ops with replication,
subscription replay, and inbound-event dedup. Apps no longer manage
individual `macula_station_link` workers — they call
`macula_client` (or the `macula` facade, which re-exports the same
surface).

Public API: `connect/2`, `close/1`, `child_spec/3`, `publish/5`,
`subscribe/5`, `unsubscribe/2`. See
`docs/guides/CONNECTING_GUIDE.md`.

The pool uses **one shared identity across all links**: stations see
the pool as a single peer (one pubkey across N links). Inbound
EVENT frames are deduped by `(Realm, Publisher, Seq)` over a
60s-default sliding window. `replication_factor` (default 1) fans
each PUBLISH to N healthy links — partial success counts as
success.

Decomposed across three files:
- `macula_client.erl`        — gen_server + public API + bookkeeping
- `macula_client_dedup.erl`  — ETS dedup keyed by `{realm, publisher, seq}`
- `macula_client_replay.erl` — sub replay on link respawn

### Added — `macula_pubsub` slice module

`src/pubsub/macula_pubsub.erl` is the pub/sub-specific surface:
`publish/4`, `publish/5`, `subscribe/4`, `subscribe/5`,
`unsubscribe/2`. Thin delegation over `macula_client` with
realm-per-call guards. Apps may import the slice directly or call
through the `macula` facade.

### Changed — realm-per-call (`macula_station_link`)

`macula_station_link` now requires the 32-byte realm tag per
operation rather than as a connect-time option. Stations are
realm-agnostic infrastructure; the realm travels in every wire
frame. API:

- `call/4` → `call/5` (Realm between Pid and Procedure)
- `subscribe/3` → `subscribe/4` (Realm between Pid and Topic)
- new `publish/4` (fire-and-forget, requires full handshake)
- DHT wrappers (`put_record`, `find_record`, `find_records_by_type`)
  keep their shape; route under the all-zeros realm tag internally.

This is a **breaking change** for any direct consumer of
`macula_station_link`. Pool consumers (`macula_client`) absorb the
change.

### Changed — `macula` facade V2 surface

The facade is rewired with V2 functions on the same surfaces that
were V1:

- `connect/2` — now returns a V2 pool (was: V1 `macula_mesh_client`)
- `publish/4` — now `(Pool, Realm, Topic, Payload)` (was: V1
  `(Client, Topic, Data, Opts)`)
- `unsubscribe/2` — now routes to `macula_client` (V2 pool)

New on the facade:
- `close/1`, `child_spec/3`
- `publish/5`, `subscribe/4`, `subscribe/5`

V1 facade surfaces are otherwise untouched: `subscribe/3`,
`publish/3`, `disconnect/1`, `call/3,4`, `advertise/3,4`,
`unadvertise/2`, `put_record/2`, `find_record/2`,
`find_records_by_type/2`, plus all stream + directed-RPC
operations.

### Renamed — `close/1` → `close_stream/1` for V1 streams

`macula:close/1` previously closed a V1 stream pid; in 3.11.0 it
closes a V2 pool. The V1 stream-close moves to
`macula:close_stream/1`. `macula:close_send/1` (half-close) is
unchanged. **Audit every callsite of `macula:close/1` before
upgrading** — the arity is identical so the compiler accepts both
shapes silently. See `docs/migrations/V1_TO_V2_PUBSUB.md`.

### Added — docs

- `docs/guides/CONNECTING_GUIDE.md` — pool model, seeds, identity,
  replication, lifecycle, `child_spec/3` integration.
- `docs/guides/PUBSUB_GUIDE.md` — rewritten for V2: realm-per-call
  subscribe/publish, dedup, EVENT delivery, message format.
- `docs/migrations/V1_TO_V2_PUBSUB.md` — what broke, before/after
  snippets, two migration paths (adopt V2 vs keep V1 via
  `macula_mesh_client` direct-module calls).

### Deferred to Phase 2 — `macula_auth`

The Phase 1 handover plan called for landing `macula_auth` types +
`{not_implemented, phase_2}` stubs. That conflicts with the SDK's
`CLAUDE.md` rule "NO TODO STUBS — Code Must Be Functional." Per
that rule, `macula_auth` is **not** included in 3.11.0 and is now
a hard gate item for Phase 2: full `mint`/`delegate`/`verify`/
`prove`/`list_capabilities`/`token_id` over `macula_ucan_nif`. See
`~/.claude/plans/PLAN_V2_PARITY.md` §15a for the deferral record.

### Tests

- 685 eunit / 0 fail (was 658 in 3.10.3).
- New: `macula_client_tests` (10 cases),
  `macula_client_dedup_tests` (8 cases), `macula_pubsub_tests`
  (4 cases), `macula_facade_tests` (4 cases).
- Updated: `macula_station_link_tests` — 19 cases (+4 new for
  realm isolation + publish/4 success + publish/4 not_connected
  guard).
- Removed three V1-facade test files superseded by the new V2
  tests: `macula_client_SUITE`, `macula_client_integration_SUITE`,
  `macula_client_pubsub_tests`. V1 still covered by direct-module
  tests `macula_mesh_client_validate_tests` +
  `macula_multi_relay_tests`.

---

## [3.10.3] - 2026-04-27

### Fixed — `handshaking` state now times out after 30s

`macula_peering_conn` added a `state_timeout` on the `handshaking`
state. If CONNECT/HELLO does not complete within 30 seconds the
worker emits a `_macula.peering.handshake_timeout` diagnostic and
exits cleanly.

Without this, peers speaking the wrong wire format (e.g. V1 daemon
clients dialling V2 stations) leave workers stuck in `handshaking`
indefinitely, accumulating bytes in the per-worker buffer that
never form a valid V2 frame. Production observed 1000+ such workers
per relay box (`PLAN_FLYING_RESTART`).

The diagnostic carries `role`, `buf_size`, `has_stream` and
`timeout_ms` so operators can correlate with V1/V2 protocol mismatch.

This pairs with the per-identity peering cap added on the
`hecate_station_listener` side (cap blocks unbounded NEW connections;
this timeout drains the EXISTING stuck pool).

---

## [3.10.2] - 2026-04-27

### Fixed — `subscribe/3` now queues until peering connects

`macula_station_client:subscribe/3` used to return
`{error, not_connected}` when called before the peering
CONNECT/HELLO completed — the typical pattern for any consumer
that subscribes immediately after `start_link/1`. The wire frame
never went out, the consumer's mailbox stayed silent, and the
station never saw the subscriber.

3.10.2 stores the subscription state immediately and returns
`{ok, SubRef}` regardless of connection state. The wire-level
SUBSCRIBE goes out either right then (already connected) or via
a drain on the `connected` peering event (handshake completes
later). Disconnect still drops every subscription the same way it
always did — the queue lives only across the handshake, not
across reconnects.

### Tests

  * 1 new EUnit case covering the subscribe-before-connect path:
    subscribe immediately after start_link, inject the connected
    event, capture the SUBSCRIBE frame on the wire.

---

## [3.10.1] - 2026-04-26

### Added — `kind` field on `node_record`

`macula_record:node_record/4` now accepts an optional `kind` opt,
emitted into the payload as `{text, <<"kind">>} => {text, Bin}`.
Stations set it to `<<"station">>`; daemons (Part 4 of the
DHT-first topology integration in hecate-station / hecate-daemon)
set it to `<<"daemon">>`. The discriminator lets subscribers route
presence facts on distinct mesh channels (`_mesh.station.*` vs
`_mesh.daemon.*`) without inferring actor type from capability
bits.

Records without `kind` predate the field. Consumers default the
missing field to `<<"station">>` since stations were the only
producers prior to 3.10.1.

### Tests

  * `macula_record_tests` now covers the `kind` field via two
    cases — `node_record_with_kind_field_test` (presence) and
    the existing `node_record_omits_unset_optional_fields_test`
    (absence). 67 cases total, all pass.

---

## [3.10.0] - 2026-04-26

### Added — streaming subscribe on `macula_station_client`

The station-client now exposes a pubsub surface alongside the
existing request/response (`call/4`, `put_record/2,3`,
`find_record/2,3`, `find_records_by_type/2,3`):

  * `subscribe/3` — sends a SUBSCRIBE frame to the connected station
    and registers a delivery pid. Returns `{ok, SubRef}`. The
    subscriber receives `{macula_event, SubRef, Topic, Payload, Meta}`
    for every matching EVENT frame the station fans out, and
    a single `{macula_event_gone, SubRef, Reason}` when the
    connection drops or the client stops.
  * `unsubscribe/2` — sends a best-effort UNSUBSCRIBE frame and
    clears local bookkeeping. Idempotent.

The client monitors each subscriber pid; if it dies the
subscription is cleaned up and a best-effort UNSUBSCRIBE goes on
the wire. On disconnect every active subscription receives one
`macula_event_gone` so consumers can react without polling
`is_connected/1`.

This unblocks topology aggregators (e.g. macula-realm) that need to
hear about new DHT records as they land, instead of polling
`find_records_by_type` and only ever seeing the seed station's
local cache.

### Tests

  * 5 new EUnit cases: `subscribe_sends_frame`,
    `event_frame_delivered_to_subscriber`,
    `unsubscribe_sends_frame_and_clears`,
    `subscriber_down_drops_subscription`,
    `disconnect_notifies_subscribers`.
  * Total `macula_station_client_tests` count: 15. All pass.

---

## [3.9.0] - 2026-04-26

### Added — DHT writes via V2 station-client

Round out `macula_station_client` so it can drive every DHT operation
a node needs against a V2 station, not just reads:

  * `put_record/2,3` — wraps `_dht.put_record`. Returns `ok` on a
    `RESULT(ok)` reply, `{error, {unexpected_reply, _}}` on any other
    payload, `{error, timeout}` / `{error, {disconnected, _}}` per the
    existing `call/4` taxonomy. Stations replicate the put across the
    K-nearest peers in their Kademlia routing table, so a single call
    against any one connected station propagates to the rest of the DHT.
  * `find_record/2,3` — wraps `_dht.find_record`. Returns
    `{ok, Record}` for a signed record map, `{error, not_found}` for
    a `RESULT(not_found)` reply.

This closes the gap that left node daemons unable to publish
`node_record` / domain-fact records into V2-only stations:
`macula_mesh_client` (V1) speaks the V1 wire and is rejected by
hecate-station's V2 peering listener, so before this release writes
silently dropped. Consumers (hecate-daemon, future SDK clients) now
have a single read+write path through `macula_station_client`.

### Tests

  * 4 new EUnit cases: `put_record_ok`,
    `put_record_unexpected_reply`, `find_record_ok`,
    `find_record_not_found`.
  * Total `macula_station_client_tests` count: 10. All pass.

---

## [3.8.0] - 2026-04-26

### Added — V2 station-client (`macula_station_client`)

A high-level outbound RPC client for V2 stations, built on top of the
`macula_peering` state machine and `macula_frame` CALL/RESULT/ERROR
frames vendored in 3.6.0–3.7.0.

  * `macula_station_client:start_link/1` — spawn a `gen_server` that
    owns one `macula_peering` connection to a single station endpoint
    and drives the CONNECT/HELLO handshake as the client side.
  * `macula_station_client:call/4` — issue a CALL frame and block
    until the station replies, the deadline elapses, or the connection
    drops. RESULT/ERROR frames are matched against pending callers via
    the 16-byte `call_id`.
  * `macula_station_client:find_records_by_type/2,3` — convenience
    wrapper for the `_dht.find_records_by_type` procedure that any
    station with the standard handler registry exposes.

This bridges a real protocol gap: V1 consumers (`macula_mesh_client`)
cannot drive V2 stations because V2 stations dispatch the QUIC
connection straight into `macula_peering:accept/2`, so V1 CONNECT
frames never reach the V2 handler registry. Until 3.8.0, an SDK user
who wanted to query a deployed station for its DHT records had to
re-implement the V2 client surface from scratch (the realm topology
subscriber in macula-realm hit exactly this).

### Tests

Six new EUnit tests cover seed parsing, CALL frame construction,
RESULT/ERROR matching by `call_id`, deadline expiry, and connection
drop. The live QUIC handshake against a real V2 station is exercised
in hecate-station's CT suites.

---

## [3.7.0] - 2026-04-26

### Added — peering state machine + diagnostics primitives

Two more modules vendored from hecate-station as the canonical SDK
implementation, finishing the V2 fork mop-up alongside `macula_frame`
in 3.6.0:

  * `macula_peering` + `macula_peering_conn` + `macula_peering_sup` +
    `macula_peering_conn_sup` — per-peer connection state machine
    (CONNECT / HELLO handshake, frame send/receive, GOODBYE drain).
    One `macula_peering_conn` gen_statem per peer, supervised by
    `macula_peering_conn_sup` under `macula_peering_sup`. The top
    supervisor is started by `macula_root` when the SDK boots, so
    `application:ensure_all_started(macula)` registers both
    `macula_peering_sup` and `macula_peering_conn_sup`.
  * `macula_diagnostics` — structured event emission via OTP `logger`
    + per-process counter / gauge metrics. Phase 1 implementation;
    upgrades to Prometheus / OpenTelemetry exporters land in Phase 7
    without changing the public surface.

### Changed — peering uses `macula_quic` directly

The vendored peering modules call `macula_quic` directly (positional
args + opts list) rather than going through an option-map adapter.
Peering's caller-facing `target` opt is still a map
(`#{host, port, alpn?, timeout_ms?}`), unpacked inside
`macula_peering_conn` before
dispatching to `macula_quic:connect/4`. Result: one transport layer
in the SDK, no adapter-on-adapter.

The hecate-station-internal `hecate_transport` adapter survives in
hecate-station for that repo's own listener / server modules — those
keep their option-map calling style.

### Fixed — EDoc cleanups in vendored modules

`rebar3 ex_doc` now runs clean. Affected modules vendored in 3.6.0
plus the new ones from 3.7.0:

  * Markdown-style paired backticks (`` `text` ``) replaced with the
    EDoc-native form (`` `text` ``) in `macula_frame`,
    `macula_source_route`, `macula_bolt4`, `macula_peering*` and
    `macula_diagnostics`. EDoc does not support markdown backticks.
  * Binary syntax (`` <<...>> ``) inside `<pre>` blocks in
    `macula_frame` HTML-escaped to `&lt;&lt;...&gt;&gt;` — the EDoc
    XML parser was consuming `<<` as the start of a tag.

---

## [3.6.0] - 2026-04-26

### Added — Macula V2 frame primitives (CBOR wire)

Three new modules vendored into the SDK as the canonical implementation
for hecate-station and any future Macula V2 service:

  * `macula_frame` — CONNECT / HELLO / GOODBYE, SWIM
    (ping / ack / suspect / confirm / update), DHT
    (ping / pong / find_node / nodes / find_value / value /
    store / store_ack / replicate / replicate_ack), CALL / RESULT /
    ERROR (Part 6 §5), HyParView, Plumtree, PubSub, content transfer.
    Length-prefixed deterministic CBOR (RFC 8949 §4.2.1) per Part 6 §3.
  * `macula_bolt4` — BOLT#4-style error-code taxonomy used by
    `macula_frame:call_error/1` and friends.
  * `macula_source_route` — onion-style source-route header builders
    plus the rotation helpers feature gates.

Atom-keyed in-process maps round-trip transparently:
  * Encode walks the map, converting atoms to text strings via
    `atom_to_binary/2`; floats stringify compactly; integers, binaries
    and lists pass through unchanged.
  * Decode walks the decoded CBOR term and restores atoms via
    `binary_to_existing_atom/2` (safe — never grows the atom table from
    untrusted input).
  * Records (`record`, `records` fields) delegate to
    `macula_record:encode/1` so the SDK's canonical CBOR shape is
    preserved verbatim across the wire.

This unifies the two parallel implementations that had diverged into
the deferred macula-v2 umbrella branch (`apps/macula_frame/`) and into
hecate-station (`apps/hecate_frame/`). Both implementations were
byte-identical BERT before this commit; both consumers now depend on
the SDK module instead.

PLAN_WIRE_CBOR.md (hecate-station) drove this — the macula 3.x mesh
client speaks CBOR per Part 6 §3 but hecate-station was on BERT, and
the wire incompatibility silently dropped every cross-codec frame.
With both sides on this `macula_frame`, station<->station and
station<->macula-client traffic share a single canonical wire codec.

### Tests — 116 macula_frame tests pass

Round-trip coverage for every frame family (handshake, SWIM, DHT,
CALL/RESULT/ERROR, HyParView, Plumtree, PubSub, content). 654 SDK
eunit tests pass overall.

---

## [3.5.0] - 2026-04-25

### Added — domain-defined record types via `macula_record:envelope/4`

The SDK now exposes its generic record builder as a public function so
domain code (realm-fact emitters, license registries, application-level
DHT-stored facts) can mint signed records without needing a per-type
constructor in the SDK.

  * `envelope(Type, SignerPubkey, Payload, Opts)` — returns an unsigned
    record map for any tag in `0x20-0xFF`. The reserved range
    `0x01-0x1F` stays owned by the SDK's typed constructors.
  * Optional `subject_id` opt → 32-byte arbitrary binary. Used by
    `storage_key/1` to derive a per-subject DHT slot
    (`BLAKE3-substituted SHA-256 of <<type, signer_key, subject_id>>`)
    so a single signer can publish many records under distinct slots
    (e.g., a realm admin signing one record per license).
  * Wire format adds an optional `u` (subject_id) CBOR field
    alongside the existing `t/k/v/c/x/p/s` envelope. Records produced
    under 3.4.0 still verify and decode unchanged; 3.5.0 records
    without `subject_id` are wire-identical to 3.4.0.

Drives `PLAN_DHT_FIRST.md` (macula-realm) — every realm fact becomes a
signed DHT record so stations stay realm-agnostic.

---

## [3.4.0] - 2026-04-25

### Added — `node_record` carries optional geo + reach metadata

Six new optional fields on `node_record`, settable via the
`macula_record:node_record/4` opts map:

- `hostname` — human-readable DNS name (e.g. `<<"relay-be-leuven.macula.io">>`)
- `endpoint` — full reach URL (e.g. `<<"quic://relay-be-leuven.macula.io:4433">>`)
- `city`, `country` — display location
- `lat`, `lng` — float or integer coordinates; encoded as CBOR text
  strings via `float_to_binary/2` (compact, 6 decimals) for
  cross-implementation determinism

Subscribers — particularly `macula-realm`'s topology dashboard —
read these straight from the record payload via `payload/1` +
`maps:get({text, <<"lat">>}, ...)`, eliminating the V1
`/topology` HTTP polling sidetrack.

The fields are **additive**: records produced with the 3.3.0 API
still verify and decode under 3.4.0 unchanged. Old subscribers
that aren't aware of the new fields ignore them harmlessly.

CBOR map keys are single-letter only on the wire spec sections that
explicitly demand it; the node_record envelope already uses
descriptive keys (`node_id`, `station_id`, `realms`, `capabilities`,
`caps_hint`, `display_name`), so the new fields use the same
descriptive style.

---

## [3.3.0] - 2026-04-25

### Changed (BREAKING) — record API now spec-compliant

3.2.0 shipped a `macula_record` module with an ad-hoc record format
(BLAKE3-of-content key, custom signing domain, opaque payload). It
was incompatible with the existing Macula V2 record spec
(`hecate_record` in hecate-station): different signing domain,
different key derivation, no per-type domain separation.

3.3.0 deletes that 3.2.0 module and replaces it with the
**spec-compliant** record implementation, vendored from
hecate-station. The SDK is now the canonical home for the record
API; downstream consumers (hecate-station, macula-realm) drop their
copies and depend on `macula` instead.

3.2.0 should not be used. Anyone who pulled it for the
`put_record`/`find_record` API: please skip directly to 3.3.0.

### `macula_record` — Macula V2 records (Part 6 §9)

PKARR-compatible CBOR records with single-letter keys (`t`, `k`,
`v`, `c`, `x`, `p`, `s`), signed with the domain-separated scheme
``"macula-v2-record\0" || canonical_cbor(unsigned)`` (Part 6 §10.2),
addressed by domain-separated storage keys (Part 3 §3.3).

Typed constructors for all 11 spec record types: `node_record/3,4`
(`type=0x01`), `realm_directory/3,4` (`type=0x03`),
`realm_stations/2,3` (`type=0x04`), `realm_member_endorsement/2,3`
(`type=0x05`), `procedure_advertisement/3,4` (`type=0x06`),
`tombstone/3,4` (`type=0x0C`), `foundation_seed_list/2,3`
(`type=0x0D`), `foundation_parameter/3,4` (`type=0x0E`),
`foundation_realm_trust_list/2,3` (`type=0x0F`),
`foundation_t3_attestation/3,4` (`type=0x10`),
`content_announcement/3,4` (`type=0x11`).

Plus the spec accessors: `sign/2`, `verify/1`, `refresh/2`,
`encode/1`, `decode/1`, `type/1`, `key/1`, `version/1`,
`created_at/1`, `expires_at/1`, `payload/1`, `signature/1`,
`storage_key/1`.

### `macula_record_uuid` — UUIDv7

Helper for record `version` fields. Time-ordered 128-bit identifiers,
unique within an Ed25519 signing key's record namespace.

### `macula_foundation` — foundation record helpers

Builders for the four foundation record types (`foundation_seed_list`,
`foundation_parameter`, `foundation_realm_trust_list`,
`foundation_t3_attestation`) plus verification. Used by the bootstrap
cascade's foundation tier.

### `macula` SDK surface — record RPC API (unchanged shape)

Same procedure namespace + topic shape as 3.2.0, with the
spec-compliant record payload:

- `macula:put_record/2` (`_dht.put_record`)
- `macula:find_record/2` (`_dht.find_record`) — key is
  `macula_record:storage_key/1` output
- `macula:find_records_by_type/2` (`_dht.find_records_by_type`)
- `macula:subscribe_records/3` / `unsubscribe_records/2`
  (`_dht.records.<type>.stored`)

### Backend requirements

The record API depends on the relay backend advertising the
`_dht.*` procedures and publishing on `_dht.records.<type>.stored`.
V1 macula-relay does not implement these — they are
hecate-station territory.

---

## [3.2.0] - 2026-04-25 — DO NOT USE

Shipped with a non-spec-compliant `macula_record`. Replaced by 3.3.0.

### Original (now-deleted) entry — for reference

Originally added a record API with BLAKE3-of-content keys and a
custom signing domain. The shape conflicted with the existing
hecate-station Macula V2 record spec implementation. Replaced
wholesale by 3.3.0; see that entry for the canonical API.

---

## [3.1.0] - 2026-04-25

### Added — crypto primitives consolidated into the SDK

Two crypto-adjacent modules previously vendored in `hecate-station` are
now part of the SDK proper. The architectural rule going forward is
**crypto primitives belong in the SDK**, not in consumers.

- **`macula_identity`** — Ed25519 keypair generation, sign/verify, public-key
  extraction, S/Kademlia crypto puzzle. Used by anything that signs
  records, frames, or session handshakes.
- **`macula_record_cbor`** — Pure-Erlang deterministic CBOR encoder/decoder
  (RFC 8949 §4.2.1). Distinct from `macula_cbor_nif`: this module is the
  *deterministic* canonicalization layer used for record signing where
  byte-for-byte stability is required across implementations. The NIF
  is for general/perf encoding; this module is for verifiable signing.

### Why

`hecate-station` was the only consumer that needed Ed25519 + record
canonicalization, but the underlying primitives are not station-specific
and would have to be re-implemented for any other consumer (clients
producing signed records, e.g. UCAN-style flows). Centralizing in the
SDK avoids fragmentation.

No breaking change — `macula 3.0.x` callers see new modules but no
existing API surface moves.

---

## [3.0.0] - 2026-04-23

### BREAKING — wire format switched from MessagePack to CBOR (RFC 8949)

The mesh wire protocol now uses CBOR for every frame's payload instead
of MessagePack. This is a hard wire-format break: every relay and every
SDK consumer must roll forward together. Greenfield migration — no
deprecation window.

### Why

CBOR was chosen because it composes natively with the rest of the
Macula identity + auth stack:

- **UCAN tokens** — already CBOR-serialized (DAG-CBOR via IPLD)
- **DIDs** — CBOR-serialized when signed
- **Ed25519/X25519 signatures** — COSE-CBOR is the canonical wrapper
- **Future WebAuthn integration** — CBOR-native

With CBOR as the wire format, signature payloads can be canonical-CBOR
encoded once and signed directly, removing the msgpack-vs-CBOR
double-encoding that previously sat between the protocol and auth
layers.

CBOR also brings:

- IETF standardization (RFC 8949) vs msgpack's GitHub-governed spec
- Deterministic encoding rules (RFC 8949 §4.2.1) — required for signed
  payloads
- IANA-registered tag types for typed data (UUID, datetime, big int)
- Indefinite-length items (streaming-friendly)

### Added

- **`macula_cbor_nif`** — new Erlang module + Rust NIF that pack/unpack
  Erlang terms to/from CBOR via the `ciborium` crate. Loaded
  automatically; no Erlang fallback (see "No fallback" below).
- **`native/macula_cbor_nif/`** — new Rust crate, ~150 lines, depends on
  `ciborium 0.2`. Built by `priv/build-nifs.sh` alongside the existing
  five NIFs.
- **`test/macula_cbor_nif_tests.erl`** — 20 tests covering primitive
  roundtrips (int/float/bin/bool/null/list/nested), map roundtrips
  (including the protocol payload shape), documented lossiness
  (atoms→binary, tuples→list), error paths (garbage/truncated/empty
  inputs), and RFC 8949 fixed-prefix self-checks (zero, empty array,
  empty map, true, false, null).

### Removed

- **`msgpack` hex package** dependency — removed from `rebar.config` and
  from the `applications` list in `macula.app.src`. The pure-Erlang
  msgpack implementation was the dominant cost in the per-frame
  serialization path; CBOR via Rust NIF replaces it with byte-identical
  semantics on the type shapes Macula actually uses.

### Migrated call sites (5)

| File | Change |
|---|---|
| `src/macula_protocol_encoder.erl:43` | `msgpack:pack/2 → macula_cbor_nif:pack/1` |
| `src/macula_protocol_decoder.erl:61` | `msgpack:unpack/2 → macula_cbor_nif:unpack/1`; error tuple is now `{cbor_decode_error, Reason}` |
| `src/macula_mesh_client.erl:777` | `args_payload/1` arbitrary-term branch uses `macula_cbor_nif:pack/1` |
| `src/macula_dist_system/macula_dist_relay_protocol.erl:50` | encode uses `macula_cbor_nif:pack/1` |
| `src/macula_dist_system/macula_dist_relay_protocol.erl:57` | decode uses `macula_cbor_nif:unpack/1`; error tuple is `{cbor_decode, Reason}` |

### Type mapping (Erlang ↔ CBOR)

```
Atom (true / false)    ↔ Bool
Atom (nil / undefined) ↔ Null  (decode always returns `nil`)
Atom (other)            → Text string  (LOSSY — decoder returns binary)
Binary                 ↔ Byte string
Integer                ↔ Integer  (uint or negative-int as appropriate)
Float                  ↔ Float
List                   ↔ Array
Tuple                   → Array  (LOSSY — decoder returns list)
Map                    ↔ Map
```

Atoms and tuples lose their type information across the wire — same
constraint as the previous msgpack-era protocol. Callers using maps
of binary keys (the protocol convention) are unaffected.

### No fallback

Unlike the crypto/DID/UCAN/MRI NIFs, `macula_cbor_nif` has no pure-Erlang
fallback. The protocol layer is in the same critical path as
`macula_quic` (which also has no Erlang fallback). Failing fast at
NIF-load time is the right behavior; a slow Erlang fallback would
silently halve throughput. If the NIF fails to load, every
`pack/unpack` call raises `{nif_error, nif_not_loaded}` — loud,
attributable, recoverable by fixing the build environment.

### Migration

For SDK consumers: this is wire-incompatible with v2.x. Daemons and
relays running v2.x cannot communicate with v3.x. Roll forward in
lockstep.

For any external code that called `macula:` API with binary args, no
change is needed — the SDK API surface is unchanged. Only the wire
encoding inside the SDK changed.

If you were using `msgpack` from your own application code that also
imported macula, you will need to add `msgpack` as your own direct
dependency (it is no longer transitively pulled in by macula).

---


## Pre-3.0 history

Releases prior to 3.0.0 are wire-incompatible (MessagePack era) and have been archived to [CHANGELOG_LEGACY.md](https://codeberg.org/macula-io/macula/src/branch/main/CHANGELOG_LEGACY.md) in the repository. They do not apply to current 3.x consumers.
