# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [4.5.0] - 2026-05-14

### Added

- **App-level liveness probe in `macula_station_link`.** Periodic
  `_macula.ping' CALL every 30s (`?LIVENESS_INTERVAL_MS') with reply
  tracking via `liveness_outstanding'. After
  `?LIVENESS_MAX_MISSES` (=2) consecutive misses, the link issues
  `macula_peering:close(PeerPid, app_liveness_lost)` which surfaces
  through the normal `disconnected` notify path → station_link
  stops → pool respawns with a fresh QUIC handshake.

  Closes the zombie-connection window that previously lasted up to
  the Quinn `max_idle_timeout' (5 minutes) — empirically observed
  going much longer in production because the server's Quinn keeps
  ACKing keep-alive PINGs at the transport layer even after the
  application-level peer has been wiped (e.g. station container
  restart). Reply matching consumes the call_id BEFORE the user-
  pending CALL machinery, so probes do not show up in caller-visible
  RESULT / ERROR streams.

- **`macula_peering:peer_capabilities/1` getter** and
  **`?CAP_STATION` capability bit** (1 bsl 0). Peers can now
  introspect the counterpart's capability bitmask post-handshake,
  letting relay stations tell direct daemon ADVERTISEs from
  station-to-station gossip relays at frame-dispatch time. Daemons
  leave the bit unset; relay stations OR it in. Existing peers
  default to 0 (treated as daemons) — full BC.

  Synchronous call into a non-`connected` state now replies
  `not_connected` immediately rather than blocking until the
  caller's own gen_statem timeout. Surfaced via the existing
  `drop_unexpected/4` clause; one-line behavioural improvement.

### Internal

- `#data{peer_capabilities :: undefined | non_neg_integer()}` field
  on `macula_peering_conn`. Populated in `absorb_peer_info/2' from
  the CONNECT / HELLO frame. No wire change (the field was already
  on the frame schema; we just record it).

- Fixed `macula_peering_handshake_tests`'
  `absorb_peer_info_populates_fields` test — was asserting
  `element(14, Data) =:= [Realm]` (a hardcoded offset) but the
  #data record grew several fields since the test was written, so
  it had been silently picking up the `quic_stream' reference. Now
  uses a structural search across all elements; resilient to
  future record additions.

---

## [4.4.9] - 2026-05-14

### Added

- **`signer` field on STREAM_DATA / STREAM_END / STREAM_ERROR frames.**
  The emitter (a `macula_station_link` instance) now stamps its Ed25519
  pubkey into the frame before signing. Stations can then verify the
  signature end-to-end against the claimed signer instead of the
  inbound conn's NodeId — necessary for multi-hop relays where the
  inbound conn is a peer station, not the originating daemon.

  Frame shape: `#{stream_id, seq, encoding, body, signer => <<_:256>>}'
  (analogous to `stream_reply`'s `responded_by` and CALL's `caller`).

  Wire-compat: `signer` is additive; old stations ignore it and keep
  verifying against inbound NodeId (single-hop only). Old SDKs don't
  emit it; new stations fall back to NodeId verify for frames missing
  the field. The two ends meet correctly after a coordinated rollout.

  Motivation: cross-station streaming RPC was failing because
  STREAM_DATA chunks emitted by daemon-A and relayed via station-A →
  station-B were verified at station-B against station-A's NodeId.
  The signature was made by daemon-A, so verify failed and station-B
  silently dropped every chunk. The chunks then never reached the
  caller daemon-C connected to station-B, and the caller's recv hit
  the 5s timeout. With `signer`, station-B verifies against daemon-A's
  pubkey and the chunks flow correctly.

  Stations on macula-station >= 4.4.9-aware will use claimed_signer
  for stream_data/end/error and claimed_replier for stream_reply.

---

## [4.4.8] - 2026-05-14

### Fixed

- **Pool gen_server serialisation under concurrent CALL/STREAM/PUBLISH.**
  `macula_client`'s `handle_call/3` for `{rpc_call, ...}`, `{rpc_call_stream, ...}`
  and `{publish, ...}` used to synchronously fan out to per-link
  `gen_server:call/3` and block the pool until each link replied. N
  concurrent callers all queued at the pool, capping concurrent
  throughput at 1. The harness's `many_concurrent_calls`,
  `many_concurrent_streams` and `multi_publisher_pubsub` cases were
  the visible failures — N=5 callers serialised through one
  gen_server with each link call timeoutable at 5s.

  Each handler now spawns a one-shot worker that does the fanout
  and replies via `gen_server:reply/2`. The pool returns to its
  mailbox immediately. N concurrent callers run in parallel.

  Behaviour from the caller's POV is unchanged — `macula:call/4` is
  still a sync gen_server:call against the pool; only the path
  inside the pool changed.

  Advertise/unadvertise/subscribe/unsubscribe stay synchronous since
  they mutate pool state (`procs` / `stream_procs` / `subs` map)
  before reaching out to links. The harness only fires these once
  per case so they're not on the contention path.

---

## [4.4.7] - 2026-05-14

### Added

- **`timing_enabled` option on `macula_peering_conn`.** When set,
  every inbound-frame notification carries an extra trailing
  `RecvAtUs :: integer()` element — `erlang:monotonic_time(microsecond)'
  captured the instant the frame finished decoding. Recipients can
  subtract their own monotonic clock at dispatch time to compute
  mailbox wait at the receiving gen_server.

  Wire shapes when enabled:
  ```
  {macula_peering, frame,        ConnPid, Frame, RecvAtUs}
  {macula_peering, dht_frame,    ConnPid, NodeId, Frame, RecvAtUs}
  {macula_peering, pubsub_frame, ConnPid, NodeId, Frame, RecvAtUs}
  ```

  Default is `false` for backward compatibility — recipients that
  have not been updated keep matching the legacy 4-/5-tuple shapes.
  `macula_station_link' has been extended to match both shapes so
  daemons receiving from a station that has opted in continue to
  work.

  Motivation: macula-station's `macula_station_peer_observer` runs a
  persistent mailbox depth of 200-400 frames under DHT load. Without
  a `RecvAtUs' stamp the mailbox wait is invisible to a downstream
  observer — `process_info(self(), message_queue_len)` at dispatch
  entry is a coarse proxy but doesn't tell you how old the frame at
  the head of the queue actually is. With `timing_enabled' the
  receiver gets per-frame wait + dispatch + forward latency, which
  is what we need to know whether peer_observer is still the
  bottleneck after 4.4.3/4.4.4's DHT and pubsub ETS bypasses.

---

## [4.4.6] - 2026-05-14

### Fixed

- **Same-pool streaming RPC, take two.** The 4.4.5 split of
  `streams` into `client_streams` / `server_streams` was necessary
  but not sufficient. The remaining failure: when the server-side
  handler called `macula:close_stream/1`, the outbound STREAM_END
  cast ran through `on_outbound_stream_frame/3` and called
  `drop_stream/2`, which cleared the Sid from BOTH maps. The
  relay's bounced-back STREAM_DATA chunks then arrived at the same
  link, hit `find_stream/2` with no entry, and were silently
  dropped. The caller's `recv` waiter timed out at 5s.

  Fix: `maybe_drop_outbound/2` skips the drop when the same Sid
  lives in both maps (the same-pool case). Inbound terminal frames
  (`deliver_stream_end` / `deliver_stream_error` /
  `deliver_stream_reply`) still call `drop_stream/2` and tear down
  both entries when the bounced terminal arrives — so the lifecycle
  closes cleanly without losing the chunks in between.

  Different-pool streaming is unaffected: each link holds only one
  side of the stream, so the same-Sid-in-both-maps test is always
  false and the outbound drop still runs as before.

---

## [4.4.5] - 2026-05-14

### Fixed

- **Same-pool streaming RPC.** Splits `macula_station_link`'s shared
  `streams` map into `client_streams` and `server_streams` so an
  `advertise_stream` + `call_stream` on the same pool no longer loses
  the caller's recv waiter. Previously, the relay bounced the
  STREAM_OPEN back over the same conn and `spawn_inbound_stream/6`
  overwrote the client entry under the same Sid, routing subsequent
  STREAM_DATA chunks to the server-side producer pid instead of the
  client's recv waiter. Caller hit the recv timeout (8s) with no
  chunks. After the split, inbound dispatch tries `client_streams`
  first (server_stream mode, the common case), falling through to
  `server_streams` for client_stream / bidi server-receive.

  Affects: `macula_e2e_probe:streaming_rpc/4` and
  `many_concurrent_streams` in the diagnostics harness — both were
  timing out at exactly 8003ms before this fix.

  Cross-station streaming (different pool, different bootstrap) is a
  separate bug at the station level (multi-hop STREAM_DATA verify
  fails against the inbound peer-link's NodeId rather than the
  end-to-end signer); tracked separately and requires a frame-schema
  bump on STREAM_DATA / END / ERROR / REPLY.

---

## [4.4.4] - 2026-05-13

### Added

- **`pubsub_recipient` option on `macula_peering_conn`.** Mirror of
  the 4.4.3 `dht_recipient` bypass — when set, pubsub-class frames
  (`subscribe`, `unsubscribe`, `publish`, `event`) go directly to
  that pid as `{macula_peering, pubsub_frame, ConnPid, PeerNodeId,
  Frame}` instead of through `controlling_pid`. All other frame types
  follow the existing path.

  Backward-compatible: `pubsub_recipient` defaults to undefined; in
  that case every frame keeps flowing through `controlling_pid`
  exactly as before.

  Motivation: after the DHT bypass shipped in 4.4.3, the dominant
  load on a station's `macula_station_peer_observer` mailbox shifted
  to pubsub `event` frames (suite measurement: 90% of post-bypass
  mailbox sample = `{frame, event}`, vs 85% `{frame, store/store_ack}`
  before). Each EVENT carries an Ed25519 publisher signature that
  needs verification before fan-out; under multi-publisher bursts
  the verify cost dominates and the same observer mailbox that
  serializes handler-dispatch and ADVERTISE / SUBSCRIBE propagation
  backs up again. Stations on macula >= 4.4.4 wire this opt to a
  dedicated `macula_station_pubsub_dispatcher` gen_server.

  See macula-station's `macula_station_pubsub_dispatcher` for the
  receiver-side implementation, plus the `pubsub_recipient` plumbing
  in `macula_station_listener:peering_opts/1` and
  `macula_station:compose_dial/2`.

### Internal

- The frame router in `macula_peering_conn` (`route_frame/2` +
  `route_by_category/4`) now classifies each parsed frame once and
  dispatches by category, instead of a per-recipient inline check.
  The category mapping (`dht` / `pubsub` / `other`) mirrors
  `macula_station_peer_observer:classify/1` — any new frame type
  added on the station side must be added on the SDK side too.

---

## [4.4.3] - 2026-05-13

### Added

- **`dht_recipient` option on `macula_peering_conn`.** When set to a
  pid, DHT-class frames (`ping`, `pong`, `find_node`, `nodes`,
  `find_value`, `value`, `store`, `store_ack`, `replicate`,
  `replicate_ack`) bypass `controlling_pid` and go straight to that
  pid as `{macula_peering, dht_frame, ConnPid, PeerNodeId, Frame}`.
  All other frame types continue to flow through `controlling_pid`
  in the existing `{macula_peering, frame, ConnPid, Frame}` form.

  Backward-compatible: when `dht_recipient` is unset (the default),
  every frame goes through `controlling_pid` exactly as before.
  No callers in the macula SDK itself set this; daemons and tests
  are unaffected.

  Motivation: in the deployed station fleet, ~85% of inbound frames
  per peering connection are DHT `store`/`store_ack` chatter from
  record replication. Funnelling them through the station's single
  `macula_station_peer_observer` gen_server meant every other inbound
  frame type (CALL, REPLY, ADVERTISE, SUBSCRIBE, PUBLISH, EVENT) sat
  behind a 200-400-deep mailbox of DHT pass-through work, adding
  700-1000 ms of dispatch latency per hop on the live Leuven fleet.
  Stations can now route DHT frames directly to their `macula_dht`
  server instead of stacking them in the observer's queue.

  See macula-station's `macula_station_listener:peering_opts/1` and
  `macula_station:compose_dial/2` for the station-side wire-up.

---

## [4.4.2] - 2026-05-13

### Added

- **Subscriber-side `publisher_sig` verification.** Step 4 of the
  pubsub Phase 2 redesign (see
  `macula-station/plans/PLAN_PUBSUB_E2E_SIGNED_EVENTS.md`). When
  `macula_station_link` delivers an inbound EVENT that carries a
  `publisher_sig`, it now verifies it (`macula_frame:verify_publisher/1`)
  against the EVENT's own `publisher` field before fanning it to
  subscribers. An EVENT with no `publisher_sig` is delivered as
  before (legacy / feature off everywhere). An EVENT whose
  `publisher_sig` is *present but invalid* is always logged at
  `warning`; it is **delivered anyway by default** (a relay bug
  should surface, not silently lose events, during the Phase 2
  rollout) and **dropped** only when the `macula` application env
  `pubsub_strict_publisher_sig` is `true`.

  No on-wire change: still nothing emits `publisher_sig` unless
  `pubsub_emit_publisher_sig` is enabled (4.4.1), so by default this
  is a no-op.

## [4.4.1] - 2026-05-13

### Added

- **Opt-in `publisher_sig` emission on outbound PUBLISH frames.** Step
  1b of the pubsub Phase 2 redesign (see
  `macula-station/plans/PLAN_PUBSUB_E2E_SIGNED_EVENTS.md`).
  `macula_station_link` now attaches a `publisher_sig`
  (`macula_frame:sign_publisher/2`) to each PUBLISH frame it sends —
  *only* when the `macula` application env `pubsub_emit_publisher_sig`
  is `true`. **Default `false`** — i.e., unchanged on-wire behaviour
  out of the box.

  **Do not enable until every relay (macula-station) is on macula
  >= 4.4.0.** A pre-4.4.0 relay's `canonical_unsigned/1` strips only
  `signature` (not `publisher_sig`) when checking a frame's per-hop
  signature, so it would reject a PUBLISH that carries `publisher_sig`.
  Rollout: macula >= 4.4.0 on the whole fleet → confirm → set
  `{macula, pubsub_emit_publisher_sig, true}` on the daemons → then
  macula-station's relay path carries `publisher_sig` onto the EVENT
  and verifies relayed EVENTs against the publisher (a later step).

  Read per publish (a fast env lookup), so the flag can be flipped at
  runtime without a daemon restart.

## [4.4.0] - 2026-05-12

### Added

- **Publisher-end-to-end pubsub signature (`publisher_sig`).** Step 1
  of the pubsub Phase 2 redesign (see
  `macula-station/plans/PLAN_PUBSUB_E2E_SIGNED_EVENTS.md`). PUBLISH and
  EVENT frames may now carry an optional `publisher_sig` field — the
  publisher's Ed25519 signature over the canonical, frame-type-
  independent tuple `(topic, realm, publisher, seq, payload)`, signed
  under the new `"macula-v2-event-pub\0"` domain. Because the signed
  content excludes header fields, `delivered_via`, and `ttl_ms`, the
  signature a publisher puts on its PUBLISH is still valid on the EVENT
  a relay station derives from it — so a relay can stop re-signing and
  consumers can verify authenticity against the *publisher* regardless
  of which relay delivered the event. New API: `macula_frame:sign_publisher/2`,
  `macula_frame:verify_publisher/1`. `macula_frame:publish/1` and
  `event/1` accept an optional `publisher_sig` in their spec.

  `publisher_sig` is excluded from the bytes covered by a frame's own
  per-hop `signature` (`canonical_unsigned/1` now strips both), so
  adding it never invalidates the per-hop signature.

  **Wire-safety / rollout note.** This release does *not* emit
  `publisher_sig` anywhere — the SDK's publish path is unchanged, so a
  4.4.0 node produces byte-identical frames to 4.3.1. The field is
  plumbed and ready; a later step has the publish path populate it.
  **That later step must not ship until every relay (macula-station)
  is on a 4.4.0-compatible build** — a pre-4.4.0 relay strips only
  `signature` (not `publisher_sig`) when checking a frame's per-hop
  signature, so a frame carrying `publisher_sig` would fail its
  verification. Order: SDK 4.4.0 everywhere → stations updated → then
  flip on `publisher_sig` emission.

## [4.3.1] - 2026-05-12

### Fixed

- **`macula_client` publish now selects only connected links.** The
  `{publish, ...}` pool handler took the first `replication`
  *spawned* link pids and published to them — including links still
  mid-handshake. A frame sent to a not-yet-connected link is dropped
  (unlike ADVERTISE, which the link replays on connect), so
  `macula_pubsub:publish/4,5` could return `{error, not_connected}`
  while other links in the pool were healthy. Now filtered through
  `macula_station_link:is_connected/1` (new `connected_link_pids/1`
  helper), matching how RPC (`call_first_success`) and streams
  (`stream_first_healthy`) already pick links. With no connected
  link the result is the existing transient
  `{error, {transient, no_healthy_station}}` (retryable) rather than
  `{error, not_connected}`.

---

## [4.3.0] - 2026-05-11

### Added

- **`macula_z32` codec module.** z-base-32 (Phil Zimmermann's
  "Human-Oriented Base-32 Encoding"; alphabet
  `ybndrfg8ejkmcpqxot1uwisza345h769`). Used for encoding 32-byte
  Ed25519 pubkeys as DNS-label-friendly strings (32 bytes → 52
  chars, fits the 63-char per-label cap). Same encoding used by
  PKARR and Pubky for the same reason. API: `encode/1`,
  `decode/1`, `is_valid_label/1`. Pure Erlang, no NIF; MSB-first
  bit packing. 18 eunit cases covering empty/round-trip/length
  contracts, hand-computed test vectors (zero32, ones32,
  small-mixed, single-byte), property-based round-trip over 200
  random samples per size class, alphabet-rejection, and
  `is_valid_label/1` guard cases.

- **`station` MRI type.** `mri:station:<52-char-z32-pubkey>`.
  Self-rooted (the realm field carries the pubkey directly; no
  reverse-domain notation; path must be empty). Validation routes
  through the new z32 codec rather than the reverse-domain regex.
  Required by `hecate-daemon`'s `serve_dns_over_mesh` slice for
  synthesising station qnames (e.g.,
  `<z32(pubkey)>._st.macula.io.`). Also added to
  `macula_mri_registry` builtin types list. 9 eunit cases covering
  parse/format/round-trip/new-via-general-constructor + four
  rejection cases (path present, short pubkey, invalid z32,
  uppercase pubkey).

### Notes

- 4.3.0 is purely additive over 4.2.x. No existing API changes;
  downgrade compiles cleanly. Downstream consumers
  (`hecate-daemon`, `macula-station`, `macula-realm`) can bump
  `~> 4.2.9` to `~> 4.3.0` whenever convenient; no coordinated
  upgrade required.

- `dane_pin` (record type 0x15) and `coverage_proof` (0x16)
  remain on the 4.4.0 candidate list. Neither is on the critical
  path for `serve_dns_over_mesh` Phase 1 (which falls back to
  SERVFAIL+EDE("coverage_unknown") for NXDOMAIN proofs and
  NOTIMP+EDE("tlsa_unsupported") for TLSA queries) or
  `serve_https_over_mesh` (which verifies station pubkeys via
  the leaf cert SAN OtherName extension, not via TLSA).

---

## [4.2.9] - 2026-05-10

### Fixed

- **`subscribe_records/3` now decodes the wire payload before
  invoking the user callback.** Previously the callback received
  the raw `macula_record:encode/1' binary; the documented contract
  said it would receive the decoded record map. The probe pair
  added in `macula-internal/macula-e2e@8831d1e` surfaced both
  this and the substrate-side topic mismatch (substrate publishes
  on `_dht.records.<type>.stored' as of `macula-internal/macula-station`
  recipient commit). Together the two changes make
  `subscribe_records/3` work end-to-end as documented.

  The wrapper accepts either binary (encoded) or map (already
  decoded) payloads — the latter for callers who feed records
  through alternate channels.

---

## [4.2.8] - 2026-05-09

### Fixed

- **`macula_blake3_nif:hash/1` now force-loads `macula_crypto_nif`
  before checking the NIF-loaded flag.** Pre-fix the function read
  `is_nif_loaded()` directly, which returns `false' until the
  `macula_crypto_nif' module is referenced for the first time
  (its `-on_load' callback writes the persistent_term flag). If a
  caller's first-ever NIF use went through `macula_blake3_nif:hash/1'
  rather than something that touched `macula_crypto_nif' first, the
  Erlang fallback fired — and that fallback is NOT plain
  `crypto:hash(sha256, _)': inputs over 1024 bytes are tree-hashed
  (1024-byte chunks SHA-256'd individually, chunk hashes pair-
  hashed), producing output that matches neither real BLAKE3 nor
  plain SHA-256.

  Surfaced by `macula:put_content/2' in v4.2.7: blobs > 1024 bytes
  computed an SDK-side MCID that no relay could verify, so every
  `_content.put_block' returned `hash_mismatch'. The four other
  hash entry points (`hash_streaming/1', `verify/2', `hash_hex/1')
  had the same bug; all are fixed in lock-step.

  `is_nif_loaded/0' is retained for diagnostic use but its docstring
  now warns that the answer reflects whatever has been observed so
  far. New private helper `ensure_crypto_nif_loaded/0' is the
  authoritative path.

## [4.2.7] - 2026-05-09

### Added

- **`macula:put_content/2` and `macula:get_content/2'** — content-
  addressed blob storage and retrieval over the relay. `put_content'
  computes the BLAKE3 hash of the bytes, packages them into an MCID
  (`<<1, 16#55, Hash:32/binary>>'), and ships the blob to the relay
  via a single `_content.put_block' RPC; the relay verifies the
  payload's hash matches the MCID before accepting. `get_content'
  fetches the blob back via `_content.get_block', returning
  `{error, not_found}' if no provider in the pool's reach holds a
  copy.

  v4.2.7 minimum-viable surface — single-block per blob, no
  client-side chunking, single-station semantics. Suitable for
  blobs in the kilobyte-to-low-megabyte range. Blobs larger than the
  relay's per-call payload budget will surface as a CALL-deadline
  timeout; chunked manifests + multi-provider parallel fetch land
  in a follow-up release. Cross-station discovery (writer + reader
  on different relays) requires the relay-side iterative-fetch
  fallback that already lands for `_dht.find_record' (commit
  c11226f in macula-station) — wire-symmetric for content once
  exposed.

  `mcid()' type added (`<<_:272>>' = 34 bytes). Hashing uses the
  existing `macula_blake3_nif' that was previously only consumed
  by the record-signing path.

## [4.2.6] - 2026-05-09

### Fixed

- **`macula_peering_conn:on_connect_verified/4` no longer crashes
  when `send_hello` returns `{error, _}`.** Previously asserted
  `ok = send_hello(Stream, NewData)' on the server-side handshake
  completion path; under teardown bursts (multiple peers closing
  pools simultaneously, e.g. e2e `end_per_suite' across a fleet),
  `macula_quic:send' could legitimately return
  `{error, "connection lost"}' between the CONNECT-verify and the
  HELLO write — the peer's QUIC stream was already gone. The
  badmatch crashed the peering_conn gen_statem worker; under
  load enough concurrent crashes tripped the parent supervisor's
  restart-intensity and forced a whole-station restart.

  Now mirrors the existing graceful handling on the client-side
  `send_connect' path (lines 232-240): emit a structured
  `{send_hello_failed, _}' disconnect notify and stop normally,
  so the supervisor cleans up without counting it as a crash.

  Surfaced 2026-05-09 by macula-station eager-replication-on-put
  load, which amplified the race; reverted at the station layer
  and shipped here so eager replication can be re-enabled cleanly
  after publishing.

## [4.2.5] - 2026-05-09

### Fixed

- **Pool fan-out (`macula_client`) no longer filters by
  `is_connected/1`.** The four fan-out helpers
  (`fanout_advertise/4`, `fanout_unadvertise/3`,
  `fanout_advertise_stream/5`, `fanout_unadvertise_stream/3`) used to
  skip pre-handshake links, which left the link's local `procedures'
  map out of sync with the pool's intent. A subsequent `unadvertise'
  on the same key would skip the link too — its local map kept the
  proc — and the link silently re-ADVERTISED on the next handshake,
  causing the relay station to register a stale procedure that
  nothing in the SDK would ever withdraw. The leak only resolved on
  daemon disconnect, when the station's `purge_conn' fired.

  Each fan-out now dispatches to every LIVE link (filtered by
  `is_process_alive/1' only). The link gen_server's `advertise' /
  `unadvertise' handlers update the local map regardless of
  connection state; the wire frame is best-effort inside
  `maybe_send_advertise' / `maybe_send_unadvertise' (no-op when
  pre-handshake). On the next handshake, `drain_pending_advertises/1'
  replays the now-correct map.

  Surfaced by 2026-05-09 mesh torture: `e2e.cross.echo.{N}' entries
  persisted on stations across rounds with `advertiser=PoolDaemonPubkey'
  even though `unadvertise/3' had returned `ok'. Tombstones in
  `macula_remote_advertise_registry' (macula-station `c7d8fe8') solve
  the gossip-vs-unadvertise race; this commit closes the
  pre-handshake-skip path that re-creates a fresh stale entry on
  every reconnect.

  Per-link errors are now wrapped in try/catch (`safe_link_advertise/4',
  `safe_link_unadvertise/3', stream variants) so a single dead pid
  cannot crash the whole pool gen_server.

## [4.2.4] - 2026-05-08

### Fixed

- **`macula_peering_conn` server-side handshaking now takes ownership
  of inbound streams.** When a server accepts a new conn and the
  client opens a stream, Quinn creates the `StreamResource' with its
  owner field set to whatever owns the conn AT THAT MOMENT. On the
  accept path that's still the listener — the conn-ownership transfer
  hasn't fired yet. Calling `setopt(Stream, active, true)' on its
  own does NOT change ownership; it only flips the active-delivery
  flag. Future `{quic, Bin, Stream, _Flags}' events therefore went
  to the listener's mailbox and got silently dropped by its wildcard
  `handle_info/2'. The worker sat in `handshaking' with `buf_size = 0'
  until its 30s timeout, even though 4.2.3's `awaiting_start'
  postpone clause + macula-station's stray-event forwarder both
  delivered the `new_stream' notification on time.

  Fix: call `macula_quic:controlling_process(Stream, self())' in the
  server-side `handshaking' new_stream handler before `setopt'. The
  worker is now the explicit stream owner, so subsequent
  `{quic, Bin, Stream, _}' events route to it directly.

  Pairs with macula-station's listener forwarding fix (commit
  `85dff3e' on macula-internal/macula-station): together they close
  the cross-station handshake race that was leaving every station
  with tens of stuck workers and partial bloom convergence.

## [4.2.3] - 2026-05-08

### Fixed

- **`macula_peering_conn` server-side `awaiting_start` no longer drops
  racing QUIC events.** `macula_peering:accept/2' transfers conn
  ownership and then casts `start_handshake'; if the QUIC NIF
  redelivers a buffered `{quic, new_stream, ...}' or `{quic, Bin, ...}'
  event to the worker before the cast lands in its mailbox, the
  worker is still in `awaiting_start'. The previous wildcard clause
  routed those events through `drop_unexpected/4' and the bytes were
  lost; the worker then sat in `handshaking' with an empty buffer
  until its 30s timeout, never reaching `transition_to_connected'.
  The peer's client-side worker meanwhile stayed `connected' (it
  received our HELLO) so QUIC keep-alive papered over the asymmetry,
  but the listener-side never registered the peer in its `peers' map
  and the controlling-pid's `connected' notification never fired —
  cross-station SUBSCRIBE / EVENT routing dead-ended.

  Verified live across the production Leuven mesh: every station had
  several stuck workers (`peer_node_id = undefined, buf_size = 0'),
  and three of centrum's outbound peers had no corresponding inbound
  registration on the peer's `peer_observer'. The race was
  particularly brutal under fleet-wide reconnect bursts (post-roll).

  Fix: postpone QUIC events received in `awaiting_start' so they
  re-deliver after the `start_handshake' transition into
  `handshaking', where the real handler consumes them.

## [4.2.2] - 2026-05-08

### Fixed

- **`macula:find_record/2` and `macula_station_link:find_record/3`** now
  pattern-match the wire-canonical `signature` field instead of the
  legacy `sig` field. The on-wire record format already used
  `signature` (see `macula_record:verify/1`,
  `macula_record:encode/1`, `macula_protocol_types:macula_record()`),
  so the SDK was rejecting every successful DHT find with
  `{error, {unexpected_reply, Record}}` even though the relay had
  returned a perfectly valid record.

  Found while standing up the macula-e2e suite against the Leuven
  topology — `dht_put_find` round-tripped end-to-end on the wire
  but the SDK swallowed the result.

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
