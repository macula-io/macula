# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [10.19.2] - 2026-09-05

### Fixed

- Removed `erl_opts` (`no_debug_info`, `deterministic`) from the `prod`
  profile in `rebar.config`. Confirmed live with a controlled A/B rebuild
  (a bare git dependency fetch of this exact repo, only variable changed
  was this profile's `erl_opts`): rebar3 merges a fetched dependency's
  `erl_opts` across *every* profile that dependency defines, not just
  whichever one the consuming project actually activated. That meant this
  library's `prod` profile — a local-dev-only convenience for running a
  leaner standalone macula node, not used anywhere in this repo's own
  CI/scripts — was silently stripping `debug_info` from every consumer's
  copy of macula, whether or not they ever selected `prod` themselves.
  Effect: `rebar3 dialyzer` with macula in `plt_extra_apps` failed for
  every consumer with "Could not get Core Erlang code," found
  independently by two sessions on three separate repos before being
  traced to this. The `prod` profile's `relx` settings (mode/dev_mode/
  include_src) are untouched — only the `erl_opts` override is gone; pass
  `--erl_opts +no_debug_info` on the `rebar3 as prod release` command
  line instead if that leaner local build is ever needed again.

## [10.19.1] - 2026-09-05

### Fixed

- Replaced 12 uses of the deprecated bare `catch Expr` prefix-operator form
  (`catch macula_stream:abort(...)`, etc.) with `try Expr catch _:_ -> ok
  end`, across `macula_pusher.erl`, `macula_feeder.erl`,
  `macula_content_transfer.erl`, `macula_download.erl`, and
  `client/macula_station_link.erl`. OTP 29 deprecates this syntax; under a
  consumer's own `warnings_as_errors` (a common convention across this
  org's repos) it becomes a hard compile failure, not a warning, so this
  repo's own OTP-29-clean commitment doesn't help a consumer building with
  a newer default toolchain than this repo pins. Found live 2026-09-05 by
  two independent sessions bumping separate downstream repos to 10.19.0
  under OTP 29. Behavior unchanged — every site was already a deliberate
  "best-effort, swallow any exception" idiom; the `try/catch` form
  preserves that exactly. Predates 10.19.0's own changes (oldest site:
  2026-08-21); unrelated to the `replication_factor` work, released
  separately since 10.19.0 was already tagged and published.

## [10.19.0] - 2026-09-05

### Changed

- `macula_client`'s `replication_factor` default is now `2`, not `1` (only
  matters for a pool with 2+ connected links — a single-seed pool is
  unaffected). Publish is fanned to `replication_factor` currently-connected
  links, and "connected" only means the app-liveness ping still answers —
  it has no way to know a link's station is silently relaying nowhere for
  some other reason (e.g. that station just doesn't serve/route the
  caller's realm). At the old default that single link was the entire
  story for every publish through the pool: total, silent data loss, with
  `ok` returned throughout, since a partial (here: complete) success still
  satisfies `publish/5`'s "first link to accept wins" contract. `2` is the
  minimum default that survives exactly that (one bad selected link no
  longer means zero delivery, as long as a second is live) without
  defaulting every publisher in the ecosystem to 3x baseline traffic for
  marginal extra protection past "survives one bad station" — callers with
  a reason to want the old single-link behavior (e.g. an already
  cost-conscious high-frequency publisher) can still pass
  `replication_factor => 1` explicitly.

  **Does not protect against a wrong `Realm` passed by the caller** — every
  replicated copy carries the identical `Realm` argument, so a
  publisher-side realm misconfiguration blackholes every selected link the
  same way regardless of factor. The gap this closes is the adjacent,
  genuinely link-local case: caller config correct, one specific station's
  relay path silently broken. (Found live 2026-09-05 investigating a
  warden whose presence heartbeat never reached the mesh despite a
  healthy-looking connection — its own publisher turned out to be
  configured with a stale realm id, which replication would not have
  fixed; tracing that incident is what surfaced this adjacent gap.)

  `status/1` now also reports the resolved `replication_factor` so a
  caller (or a test) can confirm what the pool actually applied, rather
  than trusting a doc claim. The selection math itself
  (`select_publish_targets/2`) is now a pure function with direct unit
  coverage instead of being inlined in `handle_call({publish, ...})`.

### Fixed

- Publish's fan-out worker ran each selected link's `publish/5` call
  unguarded in a plain list comprehension. A crash or exit from one
  link (a dead pid, a wedged connection hitting its 5s call timeout)
  skipped straight past `gen_server:reply/2`, so the caller got a hard
  timeout instead of the `ok` it should have received if an EARLIER
  link in the list had already accepted the frame — silently violating
  publish's own "partial success counts as success" contract. This
  exact ordering (success on link 1, failure on a later link) was
  structurally impossible at the old default `replication_factor=1`
  (never more than one link to fail "after"); raising the default to 2
  above makes it a real, common-path risk for the first time, so it's
  fixed in the same release. `advertise`'s own fan-out already guarded
  each per-link call this way (`safe_link_advertise/5`); publish's
  fan-out now does too, via the equivalent `safe_link_publish/5`.

## [10.18.0] - 2026-09-02

### Fixed

- `macula_station_link` ran every inbound CALL handler inside the link
  process itself, so the link could not read its own peering connection
  while a handler was running. A handler that touched the mesh through the
  pool, publishing a fact or making a call of its own, then waited on a
  reply that had to arrive over the very link it was blocking, until its own
  timeout fired: the pool's `advertise` and `publish` calls into that link
  timed out at 5 s, an outbound `call` at its full deadline. `macula_response`
  has published `rpc.received_v1` on every request since 9.2.0, so every
  hecate-om desk was exposed; the ones that make a mesh call inside the
  handler failed outright. Found live 2026-09-02 on hecate-rag: every
  semantic search waited 30 s on its `io.hecate.embed` call and crashed,
  the advertise republish for its other capabilities timed out meanwhile,
  and the service flickered out of the station's registry, so callers saw
  `unknown_next_peer` for a service that was up. Handlers now run in a
  process spawned per CALL, which is what the `advertise/4` doc had promised
  all along, and the RESULT or call_error frame is sent from there. Inbound
  calls on one link are therefore served concurrently rather than one at a
  time, and a slow handler no longer delays other calls, subscriptions,
  advertises or publishes on that link. Two new tests: a handler that calls
  back into its own link gets a RESULT instead of a 1 s timeout, and a fast
  call injected behind a 1.5 s one is answered at once. Both failed on the
  previous code.

## [10.17.0] - 2026-09-02

### Fixed

- A station restart silently blinded every `ordered` (the default) and
  `latest_only` subscriber to that station's own facts until the station's
  publish counter climbed back over its pre-restart value. Two halves, both
  fixed here:
  - `hecate_pubsub_server` seeded its `next_seq` from 0 at start. A
    macula-station publishes its own facts (`_dht.records.<type>.stored`,
    `_mesh.*`) through this server under the station's persistent identity,
    so every restart rewound that publisher's seq. `macula_client` had always
    seeded its own `publish_seq` from wall-clock microseconds for exactly this
    reason; the server now does the same (`erlang:system_time(microsecond)`),
    so a restart is a large FORWARD jump that `macula_pubsub_order` -- on
    every already-deployed SDK version too -- reads as a new epoch.
  - `macula_pubsub_order` only recognised a large forward jump as a publisher
    restart; a large BACKWARD jump fell through to the "already delivered,
    drop" clause, leaving the per-publisher watermark, buffer and skip
    counter untouched, so nothing in the pool state showed anything wrong.
    A backward jump wider than `EPOCH_JUMP` is now treated as a restart in
    both `ordered` (the old epoch's buffered tail is released first, then
    rebase) and `latest_only` (new high-water mark). A backstep within the
    threshold is still a late duplicate and is still dropped.
  Found live 2026-09-02: `hecate-stations`' read model stopped ingesting at
  the exact minute of a fleet-wide station rollout and stayed frozen for 10+
  hours while its link, wire subscriptions, dedup table and subscriber
  processes all looked healthy -- a throwaway subscription on the same pool
  received 27 facts in 25 s with seqs around 249k while the standing one
  expected the next seq after 589k. `hecate_stations.list_stations` reported
  2 of 8 stations as a result, and the same mechanism had blinded it after
  every earlier rollout for as long as the previous epoch had lasted. Four
  new tests cover both halves; all four failed on the previous code.

## [10.16.0] - 2026-09-01

### Added

- `macula_pubsub:subscribe/4,5`'s delivered `Meta` map now carries
  `publisher_verified` (`not_signed` | `true` | `false`), alongside the
  existing `realm`/`publisher`/`seq`/`delivered_via`. The verification
  outcome (whether an inbound EVENT's `publisher_sig` checked out) was
  already computed in `macula_station_link:on_inbound_event/5` before
  this -- `check_publisher_sig/1` calls `macula_frame:verify_publisher/1`
  and branches on `ok` / `{ok, _}` / `{error, _}` -- but that result was
  discarded before `deliver_event/4` (now `/5`) built `Meta`, so a
  subscriber could see `publisher` (the identity) but never learn
  whether its signature was actually valid, indistinguishable from
  "never signed." A lenient-mode delivery of an invalid signature (the
  default, so a relay bug surfaces rather than silently drops events)
  now reaches the subscriber tagged `false`, not folded into the same
  bucket as a genuinely unsigned event.

## [10.15.0] - 2026-09-01

### Added

- The inbound CALL frame's `caller` field (required, wire-authenticated —
  see `macula_frame`'s CALL spec) is now merged into `Payload` before an
  RPC handler runs, in `macula_station_link:handle_inbound_call/2`. Until
  now this field was decoded off the wire and then silently dropped: it
  never reached `Module:handle_request/2` (the callback every provider,
  `macula_response` included, calls with just `Payload` and `State`).
  Pub/sub already had the equivalent (`publisher` reaches a
  `subscribe_callback/4` handler via `Meta`); this closes the same gap
  for the request/reply RPC path.
- Deliberately **not** a `handle_request/2` arity change — that callback
  is a fixed contract every existing provider implements, and bumping it
  would break every one of them. `caller` is merged into the payload map
  instead (`Payload#{caller => Caller}`), so a handler that wants
  provenance reads it exactly like any other field
  (`hecate_om_wire:field(caller, Payload)`), and one that doesn't needs
  no change at all. The merge happens after the payload is fully decoded,
  so it deterministically overwrites any same-named key a caller's own
  payload might supply — the value a handler reads is always the wire-
  authenticated identity, never spoofable via the payload body.

## [10.14.5] - 2026-09-01

### Fixed

- `macula_response`/`macula_streamer`'s `existing_or_new_sup/1` reused a
  `reuse_sup` pid unconditionally, without checking it was still alive.
  A caller that periodically re-advertises with `reuse_sup` (the
  documented pattern for keeping one factory supervisor across ticks
  instead of leaking one per tick) can find that pid already dead --
  e.g. the caller itself crashed between ticks and, being linked to the
  factory sup it started via `start_link`, took it down too. Reusing the
  dead pid handed `dispatch/7` (or `dispatch/8`) a `Sup' that would
  `noproc` on its very first `supervisor:start_child`, silently breaking
  every inbound call for that procedure until a later re-advertise
  happened to land. Found live 2026-09-01 via hecate-rag:
  `hecate_om_capabilities` crashed on a timed-out advertise call and,
  through exactly this path, several unrelated capabilities
  (`search_chunks_semantic`/`answer_query`/`add_knowledge`) started
  failing every inbound call with `noproc` for the next few minutes.
  `existing_or_new_sup/1` now checks `erlang:is_process_alive/1` before
  reusing a pid and falls through to starting a fresh supervisor
  otherwise -- a pattern-matched predicate dispatch, not a try/catch.

## [10.14.4] - 2026-08-31

### Fixed

- `macula_direct_dial:discovery_uri/2` used `binary:encode_hex(Realm)` (lowercase)
  while the live fleet's DHT `procedure_advertisement` records carry uppercase hex
  in `procedure_uri`. Since `SHA-256(uppercase) != SHA-256(lowercase)`, the Go/Rust/.NET
  direct-dial resolvers were looking up the wrong DHT key — every direct-dial call failed
  with "procedure has no direct-dial advertisement". Changed to `binary:encode_hex(Realm, uppercase)`
  so the source matches what the fleet actually publishes.

## [10.14.3] - 2026-08-31

### Fixed

- `priv/build-nifs.sh` silently skipped building `macula_cbor_nif` when
  `cargo` wasn't on PATH -- a warning, not a failure, and the script still
  exited 0 ("All NIFs ready.") having built nothing. `macula_cbor_nif.erl`'s
  own moduledoc is explicit that this NIF has no Erlang fallback and
  "failing fast at NIF-load time is the right behavior" -- the build script
  did the opposite: a clean, green build that fails every caller at
  runtime with an opaque `nif_not_loaded` instead. Found live in a
  downstream consumer's CI (an `erlang:28` container with no Rust
  toolchain installed): `rebar3 compile` and dependency resolution both
  went green, then every test touching CBOR pack/unpack failed. `cargo`
  missing (or a build that reports success but produces no `.so`) is now
  a hard failure (exit 1) for `macula_cbor_nif` specifically, with a
  message naming why. The other four Rust NIFs `build-nifs.sh` builds
  (`macula_crypto_nif`/`macula_ucan_nif`/`macula_did_nif`/`macula_mri_nif`)
  keep the existing soft-skip -- each documents a real Erlang fallback in
  its own moduledoc, so a consumer without Rust still gets a working,
  if slower, build for those. Verified directly (isolated the `build_nif`
  function, three scenarios): cargo present builds `macula_cbor_nif`
  normally same as before; cargo absent now exits 1 with the new message;
  the four fallback-having NIFs still soft-skip and return 0 unchanged.

## [10.14.2] - 2026-08-30

### Fixed

- `macula_station_link:maybe_send_subscribe/3` gated a SUBSCRIBE frame's
  immediate send on `peer_pid` alone, unlike its two siblings
  (`maybe_send_advertise/3`/`maybe_send_unadvertise/3` in the same
  module), which correctly gate on `peer_node_id`. `peer_pid` is set the
  moment `macula_peering:connect/1` returns, before the CONNECT/HELLO
  handshake completes; `peer_node_id` is only set once it genuinely has.
  A SUBSCRIBE frame sent in that window landed on the wire while the
  peering statem was still in `handshaking`, which has no clause for
  `cast({send_frame, _})` and silently drops it via `drop_unexpected`
  (logged as `_macula.peering.unexpected_event`). Found live: a real
  deployment's logs showed this exact frame (topic
  `_dht.records.N.stored`) dropped on every reconnect. Harmless for a
  *stored* subscription — `drain_pending_subscribes/1` resends it once
  `connected` genuinely fires — but wasteful and alarming on every
  reconnect, and not harmless for a caller that assumed the frame had
  actually gone out. Regression test added
  (`subscribe_during_handshake_not_sent_early_test_`); confirmed it fails
  without the fix before confirming it passes with it.

## [10.14.1] - 2026-08-29

### Fixed

- Bumped `rand` 0.8.5 → 0.8.6 in `native/macula_ucan_nif/Cargo.lock` and
  `native/macula_crypto_nif/Cargo.lock` (Cargo.toml already permitted it —
  lockfile-only update). Closes GHSA-cq8v-f236-94qc, the two remaining
  Dependabot alerts on this repo (both low severity: the unsound path needs
  a custom `log` logger reading `rand::rng()` from inside the log call
  itself, which neither NIF crate does — no working exploit here, fixed
  anyway since a patched version was available with no other changes
  needed). `native/macula_quic/Cargo.lock` was already clear (rand 0.10.2).

## [10.14.0] - 2026-08-29

### Added

- `hecate_pubsub:patterns/1` and `hecate_pubsub_server:patterns/1` expose a
  subscriber's registered wildcard patterns (e.g. `<<"*/svc.do">>`) as their
  own list, separate from `topics/1`. This is the export macula-station's
  bloom-exchange gossip now reads to propagate wildcard-pattern interest
  mesh-wide, rather than the station-local-only matching that existed before.

## [10.13.2] - 2026-08-29

### Fixed

- `macula_record:read_node_record/1` silently dropped the `version` field
  `macula_station_announcer:inject_identity_metadata/1` has stamped onto
  every station's re-announce heartbeat since before this reader existed —
  write-only until now, so no consumer (including `hecate-stations`) could
  ever read a station's own reported build back out. Added `version` to the
  returned map.

## [10.13.1] - 2026-08-29

### Fixed — `call_station`/`call_stream_station`/`ensure_content_link` dialed a redundant duplicate connection to an already-connected station

`macula_client`'s link table is keyed by the literal seed STRING passed to
`ensure_link/3`. A direct-dial caller names its target by a URL it just
resolved (a `station_endpoint` record's `quic://[host]:port`), which very
often spells the SAME physical station differently than however the pool's
own configured seed (or an earlier direct-dial call to it) already named it.
A literal-string miss dialed a genuinely second, redundant connection to a
station the pool already held a live connection to — reproducible, live,
100% of the time, as literally the SECOND `call_station` from one pool to
the same station, regardless of realm or procedure: the station closed one
of the two duplicate connections, and whichever caller's next attempt
landed on the closed one failed with `{disconnected, {peer_closed,
"connection lost"}}`.

Fixed: a literal-key miss now checks whether the caller supplied
`expected_node_id` (every direct-dial caller does — it's the station
identity already resolved and verified via a signed DHT record before
reaching here) and, if so, scans this pool's existing links for one already
connected to that same identity before dialing fresh. Only runs on a
direct-dial literal-key miss — the pool's own plain seed-connect path is
unaffected, and any subsequent call to the same resolved URL hits the
ordinary literal-key match and skips the scan entirely.

## [10.13.0] - 2026-08-29

### Added — station-local wildcard pubsub subscriptions

`hecate_pubsub:subscribe/3` now treats a topic containing a literal `*`
segment as a wildcard pattern (matched via `macula_topic_pattern:matches/2`)
rather than an exact topic — kept in a separate internal map so a realm
with no wildcard subscribers pays no extra cost on delivery. A subscriber
on `realm/*/app/domain/name_v1` receives publishes to both
`realm/acme/app/domain/name_v1` and `realm/contoso/app/domain/name_v1`.

**Station-local only, by design, this release**: `topics/1` (which feeds
cross-station Bloom-gossip re-subscription in `macula-station`) still
returns exact topics only — a wildcard pattern is never propagated to
peer stations, since a Bloom filter tests exact-string membership and a
raw `*`-bearing string would be meaningless there. A wildcard subscriber
therefore only receives a publish that reaches this realm instance
directly (same-station publisher, or one already fanned here via the
ordinary exact-topic relay path) — not one arriving purely via gossip
from a peer that has no exact-topic overlap. Mesh-wide wildcard
subscription is a separate, larger piece of work, tracked in
`macula-station/plans/PLAN_ORG_SCOPED_DISPATCH_AND_WILDCARD_DISCOVERY.md`.

## [10.12.0] - 2026-08-29

### Added — `macula_topic_pattern:matches/2`

Segment-wise wildcard matching for hierarchical mesh addresses (pubsub
topics, RPC procedure names, capability advertisements): `*` matches
exactly one segment, in exactly that position. Deliberately arity-agnostic
— no assumption of a fixed segment count, so it serves both
`hecate_om_capabilities`'s 2-segment capability names (`org`, `name`) and
`macula_topic`'s 4-segment tiered topics (`org`-or-`_org`,
`app`-or-`_realm`, `domain`, `name`) with the same primitive. Building
block for wildcard capability discovery and wildcard pubsub subscription
matching (`hecate-services/hecate-om`,
`macula-station/plans/PLAN_ORG_SCOPED_DISPATCH_AND_WILDCARD_DISCOVERY.md`).

## [10.11.1] - 2026-08-29

### Fixed — `macula_direct_dial:publish_advertisement/5` silently dropped `ttl_ms`

`adv_opts/1` only ever forwarded `cert_chain` from a caller's `Opts` to
`macula_record:procedure_advertisement/4` — a single-clause match meant passing
`ttl_ms` alone (no `cert_chain`) produced `#{}`, silently discarding it, even
though `procedure_advertisement/4` has read `ttl_ms` from its own Opts all
along. Now forwards each recognized opt independently, so a caller of
`advertise_direct/7`/`publish_advertisement/5` can set a proportioned TTL on
the published `procedure_advertisement` instead of getting the envelope
default regardless of what it asked for. Found wiring a proportioned `ttl_ms`
through from `hecate_om_capabilities`.

## [10.11.0] - 2026-08-29

### Added — `purge_subscriber/2` on the pubsub overlay stack

New `hecate_pubsub:purge_subscriber/2`, `hecate_pubsub_server:purge_subscriber/2`,
and `hecate_pubsub_registry:purge_subscriber/2` (the last one fanning out across
every realm the registry currently holds a server for). Removes one subscriber
pubkey from every topic it was on, dropping any topic that empties out as a
result — the same `drop_or_keep` rule `unsubscribe/3` already applies to one
topic, generalized to "all topics this subscriber touched."

Closes the missing half of `macula-station/plans/DESIGN_SUBSCRIPTION_LIFECYCLE_GC.md`:
nothing in this SDK previously removed a subscriber's entries on connection loss,
only on an explicit UNSUBSCRIBE frame. A station never had this primitive to call
in the first place — `macula_station_peer_observer:on_disconnected/2` purges SWIM,
DHT, ADVERTISE and stream state on disconnect already, but had nothing to call for
pubsub, so a peer or daemon that vanished without unsubscribing left its topics
permanently registered as local interest, which `macula_station_peering_router`
then re-propagated to every other peer indefinitely. Wiring this into that
disconnect path is a `macula-station` change, not an SDK one; this release only
adds the primitive.

## [10.10.2] - 2026-08-28

### Fixed — a text payload VALUE matching an existing atom name silently arrived as that atom, not a binary

Real production crash: `hecate-stations` (a live directory service built on
this SDK) ingested a genuine `node_record` from `station-it-milan.macula.io`
via `find_records_by_type/2` and crashed a downstream RocksDB indexer,
repeatedly, on every restart. The record's `kind` field decoded as the
**atom** `station` instead of the binary `<<"station">>` — every other
field on the identical record (`hostname`, `city`, `country`) decoded as
an ordinary binary, which is what made it invisible until something
finally choked on the one field that happened to collide.

Root cause: `macula_frame:from_wire_envelope/1` — the RPC-response decode
path `find_record/2`/`find_records_by_type/2` return records through,
distinct from the DHT-storage wire codec (`encode/1`/`decode/1`) —
deliberately collapses a `{text, B}` VALUE into the atom `B` whenever `B`
already exists in the runtime's atom table (safe there: an undeclared
name harmlessly stays `{text, Bin}`). `"station"` is used as a literal
atom throughout this codebase, so it collided; `"Milan"` is not, so it
didn't. `macula_record:payload_field/2` already handled this exact
class of decode-path variance for KEYS (`{text, Name}` / bare `Name` /
`safe_atom(Name)`), but never accounted for it on the VALUE side.

`unwrap_text/1` now converts an atom value back to its binary form
before returning it from `payload_field/2` — used by every `read_*`
function in this module — except `true`/`false`/`undefined`/`null`,
which are left as atoms (no current reader expects a boolean, and
`null` is `read_tombstone/1`'s own explicit-absence marker for
`detail`).

## [10.10.1] - 2026-08-28

### Fixed — `read_node_record/1` silently dropped fields the writer already stores

`node_payload/5` has written `hostname`/`endpoint`/`city`/`country`/`lat`/
`lng`/`display_name`/`caps_hint`/`peers` into every `node_record` since
v3.4.0, but `read_node_record/1`'s typed-map reader stopped at `node_id`/
`station_id`/`realms`/`capabilities`/`kind` — the data was always on the
wire, just unreachable through the public API (`payload_field/2`, the
only thing that knows how to read either the canonical or wire-decoded
key shape, isn't exported). Found building `hecate-stations`, a directory
service that needs exactly these fields to answer "where is this
station".

`lat`/`lng` now come back as `float() | integer() | undefined`:
`with_geo/3` writes floats to 6 decimals but integers with no decimal
point at all, so a plain `binary_to_float/1` would crash on an
integer-valued coordinate — the new `parse_geo/1` tries float first and
falls back to integer.

### Added — `read_tombstone/1`, the typed reader for `tombstone` records

Same gap as `read_node_record/1` above, one type tag over: a `tombstone`
(`0x0C`) could be built, signed, and verified, but nothing let a
subscriber read `superseded_key`/`superseded_type`/`replaced_at`/
`reason`/`detail` back out without reaching for the unexported
`payload_field/2`. `macula_station_announcer` has published a signed
tombstone on every graceful shutdown since it existed; nothing outside
this module could ever read one. Also found building `hecate-stations`,
which needs to retire a station from its read model the moment its
`node_record` tombstone lands rather than waiting out the TTL.

`detail` comes back `undefined` rather than the wire's `null` —
`tombstone/3,4` always writes the key, present-but-empty, unlike every
other optional field in this module which is simply omitted when unset.

## [10.10.0] - 2026-08-27

### Fixed — `macula_diagnostics:event/2,3` was silently dropped everywhere, always

Root cause of a class of bug this session had already independently
rediscovered and worked around three separate times (10.5.5's
`drop_unexpected/4`, the listener's `maybe_emit_puzzle_invalid/3` and
`duplicate_replaced` warnings in macula-station) without ever tracing
it back to the actual source: `event/3` stamps every report with
`domain => [macula]` in its metadata. OTP's own stock `default` logger
handler — confirmed with a bare `erl`, no `sasl`, no project config —
ships `filter_default => stop` with only two explicit allows: events
whose domain is `[otp, sasl]` (or a sub-domain of it), and events with
no domain at all. `[macula]` matches neither. Every single call to
`macula_diagnostics:event/2,3`, in this SDK and in every consumer
(macula-station's announcer, outbound_link, health_publisher,
peer_observer's `relay_overlay`/`forward_overlay`, listener's
`cap_exceeded`), has been silently dropped before reaching any handler
output since the day this module shipped. Confirmed end-to-end with a
throwaway `default` handler pointed at a file: `info`-level events
never appeared until this fix was in place.

Fixed at the source, not by working around it at each call site again:
new `macula_diagnostics:install_domain_filter/0` adds an explicit
`{log, equal, [macula]}` allow filter to the `default` handler, called
once from `macula_app`'s own `start/2` — every consumer gets it for
free just by depending on `macula`, no per-release filter config to
remember. `macula_peering_conn`'s `drop_unexpected/4`, which carried
the `logger:warning/2` workaround and a comment explaining why, is
reverted back to `macula_diagnostics:event/2` (topic
`_macula.peering.unexpected_event`) now that the underlying mechanism
actually works. macula-station's own workarounds
(`maybe_emit_puzzle_invalid/3`, `duplicate_replaced`) are addressed in
that repo's own CHANGELOG once it picks up this version.

Verified with `test/macula_diagnostics_tests.erl`'s
`domain_filter_fixes_the_actual_drop_test/0`: reproduces the exact
production filter chain (including kernel's `logger_level => info`
override, since OTP's stock primary-level default of `notice` would
otherwise mask the same symptom for an unrelated reason) against a
temporary `default` handler backed by a real file, confirms the event
is silently absent before the fix and present after it. Confirmed RED
without the fix (`{error, undef}` on the not-yet-existing function) via
`git stash`, GREEN with it restored.

### Fixed — NIF discarded the real reason a QUIC stream closed

`native/macula_quic/src/stream.rs`'s recv-loop catch-all
(`Err(_e) => ... atoms::none() // simplified for now`) collapsed every
read error other than a peer `Reset` — connection loss, timeout,
anything else `quinn::ReadError` can return — into a bare `none`
atom. 10.9.1's `stream_closed`/`peer_send_shutdown` handling threads
this `Detail` straight into the `disconnected` notification's reason
(`{peer_closed, Detail}`, `{closed_during_handshake, Detail}`, etc.),
so every one of those reasons carried no actual diagnostic content.
Now formats the real `quinn::ReadError` via `format!("{}", e)`,
matching the existing pattern already used two lines away in the same
file for `reset`/`send` errors. No Erlang-side change needed — `Detail`
was always passed through opaquely, so it now just carries a real
string instead of always `none`.

## [10.9.1] - 2026-08-27

### Fixed — connections could sit "connected" forever after their transport actually died

Found while live-verifying 10.9.0's `reject/2` against the real fleet:
the surviving side of a rejected/closed connection sometimes logged
`[peering] unexpected state=handshaking event={quic, stream_closed,
...}` and never disconnected.

Root cause: `handshaking/3`, `connected/3`, and `draining/3` each had a
clause for `{quic, closed, Conn, Detail}` — an event **nothing ever
sends**. Verified directly in `native/macula_quic/src/atoms.rs`/
`stream.rs`/`connection.rs`: the `closed` atom exists and a
connection's own `closed` field is a purely-local `AtomicBool`, but no
code path anywhere calls `send_event` with it. What the recv loop
*actually* sends when the control stream dies for any reason — peer
reset, connection loss, timeout, everything `stream.rs`'s own
`Err(_e)` catch-all collapses under a `// simplified for now` comment
— is `{quic, stream_closed, Stream, Detail}`; a clean peer-initiated
half-close is `{quic, peer_send_shutdown, Stream, Detail}`. Neither was
handled anywhere in the state machine. Both `handshaking` and
`connected` had no other matching clause either, so every occurrence
fell through to `drop_unexpected/4`, which (correctly, since 10.5.6)
logs it and returns `{keep_state, Data}` — the connection just sits
there. No `disconnected` notification, no termination, indistinguishable
from healthy to `controlling_pid`/`accept_owner` until some unrelated
higher-layer liveness probe eventually notices. This is the same
failure class `project_station_dead_but_healthy_milan` documented at
the station-transport level, one layer down at the connection state
machine itself.

Replaced all three dead clauses with working ones for
`stream_closed`/`peer_send_shutdown` (guarded to the control stream,
`Data#data.quic_stream`, matching the codebase's own established
pattern). `draining/3`'s case was not a "stuck forever" bug (its
`state_timeout` already terminates unconditionally) — just a missed
opportunity to end the drain the instant the peer's transport
confirms closure instead of always waiting out the full
`?DRAIN_TIMEOUT_MS`.

**Verification, not assumption**: new
`peer_closing_notifies_the_surviving_side/1` test (real Quinn QUIC
pair) — reject one side, confirm the OTHER side (which did not
initiate anything) still gets `disconnected` and terminates, within
2s. Confirmed RED without the fix (reverted `macula_peering_conn.erl`
alone, reran — the new test failed exactly as expected) before trusting
GREEN with it restored. Broader suite (`macula_peering_conn`,
`macula_peering`, `macula_peering_handshake_tests` — now 8 tests,
`macula_peering_dial_trust_tests`, `macula_peering_recipient_tests`,
`macula_frame`, `macula_station_link_tests`, `macula_identity` — 275
tests total) passes clean.

## [10.9.0] - 2026-08-27

### Added — `macula_peering:reject/2`, closing the puzzle-enforcement drain window

Follow-up to the overlay_relay root cause ([10.5.9]). That incident's
mechanism was: a puzzle-invalid peer's connection promotes to
`connected` (the SDK's state machine has no knowledge of a station's
puzzle policy), and only afterward does `macula_station_listener`
decide to reject it — using `macula_peering:close/2`, which transitions
through `draining` for `?DRAIN_TIMEOUT_MS` (**5 seconds**), during
which `draining/3`'s "ignore late inbound during drain" clause silently
accepts and discards any further traffic by design. That's correct for
a peer whose session was genuinely trusted and is simply ending; it's
pure exposure for a peer that was never trusted in the first place —
flagged explicitly as real hardening material still worth doing, not
done in that release.

`reject/2` is `close/2`'s counterpart for exactly that case: no GOODBYE,
no `draining`, straight to `{stop, normal, Data}`. Added matching
`{reject, Reason}` clauses to every state (`connecting`,
`awaiting_start`, `handshaking`, `connected`, `draining`) — in every
state except `connected` this is identical to what `close/2` already
does there (nothing has been established yet, so immediate termination
was already correct); `connected` and `draining` are where the actual
fix lives. `macula_station_listener:reject_handshake/3` now calls
`reject/2` instead of `close/2` for `puzzle_invalid`. This narrows the
exposure window from the full 5s drain to, at most, whatever's already
in the connection process's mailbox at the moment of rejection — not a
mathematically perfect elimination (that would need synchronous
admission control gating the SDK's own `connected` transition, a much
larger change not currently justified) but a genuine, order-of-
magnitude reduction of a real, measured gap.

Also fixed: `macula_station_listener:maybe_emit_puzzle_invalid/3`
(macula-station) used `macula_diagnostics:event/2`, the same
domain-filter logging bug fixed elsewhere in the 10.5.x line — a
function whose whole purpose is letting an operator see rejection
volume before flipping `log_only` to `enforce` was silently
unobservable. Switched to `logger:warning/2`.

**Found and fixed a real, unrelated pre-existing bug while adding
tests**: `macula_peering_handshake_tests.erl`'s client `target` options
never set `verify`, which defaults to `webpki` (documented as "the
default since 5.0.0") — meaning every test in this file was rejecting
its own self-signed test certificate with `UnknownIssuer` and had
apparently been doing so for a long time, just never caught by a
careful full-file run. Added `verify => none`, matching the
established self-signed-test-cluster convention used everywhere else
in this codebase. All 7 tests in the file pass now, including the new
one — which also serves as direct proof of the fix: "close" takes
5.002s (still drains, unchanged), "reject" takes 0.002s (immediate,
the whole point).

**Verification**: new `reject_terminates_immediately/1` end-to-end test
(real Quinn QUIC pair, not a state-machine mock) confirms both the
`disconnected` notification and process exit land within 500ms, never
anywhere near the 5s drain window. Broader suite (`macula_peering_conn`,
`macula_peering`, `macula_peering_handshake_tests`,
`macula_peering_dial_trust_tests`, `macula_peering_recipient_tests`,
`macula_frame`, `macula_station_link_tests` — 254 tests) passes clean.

## [10.8.0] - 2026-08-27

### Changed — the live wire codec is now the native deterministic CBOR encoder

10.7.0 shipped `macula_cbor_nif:pack_deterministic/1` and
`unpack_deterministic/1` as an additive, differentially-tested-but-unused
capability. This release wires it in: all 9 real call sites across
`macula_frame.erl`, `macula_record.erl`, and `macula_manifest.erl` that
used to call `macula_record_cbor:encode/1` / `decode/1` now call
`macula_cbor_nif:pack_deterministic/1` / `unpack_deterministic/1`
instead — every frame sent or received on the mesh, every signed
record, and every content manifest hash now goes through the native
codec. `macula_record_cbor.erl` itself is untouched and stays live as
the differentially-tested reference implementation (still exercised by
its own full test suite); nothing calls it for real traffic anymore.

**Found and fixed a real performance bug before wiring anything in**:
the native decoder was originally *slower* than the pure-Erlang
reference for every payload size tested (15-43% slower, worst on
medium-sized frames) — it paid a fresh allocation + full copy
(`OwnedBinary::new` + `copy_from_slice`) for every byte-string/text-
string field, where Erlang's own `<<B:Len/binary, Rest/binary>>` sub-
binary pattern match pays neither for a refc binary. Fixed by threading
the original input `Binary<'a>` through the whole recursive descent and
using `Binary::make_subbinary/2` (a genuine zero-copy reference) instead.
Benchmarked before wiring in (representative small/medium/large
frame-shaped payloads, 50k iterations each, two independent runs):
encode 2.6-4.2x faster, decode 1.24-1.62x faster, both directions, both
runs — a real, reproducible net win, not a wash.

**Verification**: full targeted suite (`macula_frame`, `macula_record`,
`macula_record_cbor`, `macula_record_uuid`, `macula_record_cert_chain`,
`macula_record_content_announcement`, `macula_manifest`, `macula_identity`,
`macula_peering_conn`, `macula_peering`, `macula_cbor_nif`,
`macula_cbor_deterministic_diff_tests` — 455 tests) passes clean. Full project eunit suite: 1586 passed, 1
failed before the run cascaded/aborted (a pre-existing flake in
`macula_station_link_tests`, confirmed unrelated to this change by
running that module in isolation — 53/53 pass there, matching the
already-documented `macula_full_eunit_suite_flaky_under_load` pattern).
Rebuilt the NIF from scratch via the real `priv/build-nifs.sh`
pipeline (not a manual `cargo build` + copy) to confirm the hex-publish
path produces the same result a consumer's `rebar3 compile` would.

**No wire-format change**: this is the entire point of shipping 10.7.0
first and differentially testing it — the bytes this produces are
identical to what `macula_record_cbor` always produced, verified by
construction (65 differential tests including 3000+ randomized trials
across two seeds) rather than assumed. Any station or client on an
older macula version interoperates unchanged.

## [10.7.0] - 2026-08-27

### Added — native deterministic CBOR codec (additive, NOT wired into the live frame path)

`macula_cbor_nif` gains `pack_deterministic/1` and `unpack_deterministic/1`,
a from-scratch native implementation of `macula_record_cbor.erl`'s exact
RFC 8949 §4.2.1 deterministic subset — the codec every `macula_frame`
and `macula_record` encode/decode actually goes through today, and
therefore the thing every signature verification in the mesh depends
on producing byte-identical bytes. This is the headline NIF opportunity
identified while surveying the SDK for further native-acceleration
candidates: `macula_record_cbor` is pure Erlang despite this crate's
existing `nif_pack`/`nif_unpack` sitting right next to it — but that
existing pair goes through `ciborium::value::Value`, a generic,
non-deterministic representation (no canonical map-key order, no
forced integer/float widths, lossy atom/tuple handling), so it isn't a
drop-in for the wire protocol's actual requirements. The new functions
bypass `ciborium` entirely and operate directly on `rustler::Term`,
implementing the same value model by hand: non-negative integer ->
uint (major 0), negative integer -> major 1 (encoded count = `-1-N`,
full range down to `-(2^64)` via `i128`, unconditionally available in
rustler 0.34 with no feature flag), binary -> byte string, `{text,
Binary}` -> UTF-8 text string (bytes used as-is, no UTF-8 validation,
matching the Erlang encoder exactly), atom (encode-only, not `null`)
-> UTF-8 text via its own name, list -> array, map -> map with keys
sorted by the bytewise order of their own encoded bytes, `null` ->
simple null, float -> always binary64 on encode (decode accepts
16/32/64-bit).

Every decode path is on the hot path for untrusted, network-received
bytes and is written to never panic — no `unwrap`/`expect`/unchecked
slice indexing anywhere; every length and offset is bounds-checked
before use, and every "no matching clause" case in the Erlang reference
(major-7 additional info outside {22,25,26,27}, major-6 tags, trailing
bytes after the top-level value, NaN/infinity in a half-float) becomes
an explicit `rustler::Error::RaiseTerm` — genuinely raising, matching
`macula_record_cbor:decode/1`'s real crash-on-malformed-input contract,
not `Error::Term`'s different "return `{error,_}` normally" behavior.

Verified, not assumed: `test/macula_cbor_deterministic_diff_tests.erl`
differentially tests the new codec against `macula_record_cbor` across
the exact boundary vectors `macula_record_cbor_tests.erl` already
treats as load-bearing (uint/negative-int width boundaries, empty/full
binaries and text, nested maps, atom-as-map-key, the `-(2^64)` extreme),
plus a 3000-iteration seeded random generator (same style as
`macula_frame_tests`'s own `check_payload_soundness_holds_on_generated_terms_test_`)
covering deeply nested maps/arrays/mixed types, run against two
different seeds, plus 14 malformed/truncated input cases asserting the
decoder raises cleanly rather than panicking. All 65 tests pass; the
full macula_identity/peering_conn/peering/frame/record_cbor/cbor_nif
suite (326 tests) passes unchanged.

**Deliberately NOT done here**: `macula_frame.erl`/`macula_record.erl`
still call `macula_record_cbor`, unchanged. Swapping the live wire
codec is a separate, higher-stakes step — this release only makes the
native codec exist and prove itself byte-for-byte identical.

## [10.6.0] - 2026-08-27

### Added — native puzzle grinding in `macula_crypto_nif`

`macula_identity:generate(#{puzzle => true})` now grinds the S/Kademlia
identity puzzle (see [10.5.9]) natively via a new
`nif_grind_puzzle/1` in `macula_crypto_nif`, instead of looping
`crypto:generate_key/2` + `crypto:hash/2` one candidate at a time from
Erlang. Motivated directly by the overlay_relay incident: puzzle
enforcement is live on the real fleet, and a difficulty high enough to
matter as Sybil resistance is exactly the kind of long, CPU-bound
search a BEAM scheduler thread shouldn't run — the new NIF is
`schedule = "DirtyCpu"` so it doesn't block a normal scheduler either
way.

`macula_crypto_nif:grind_puzzle/1` follows the module's existing
NIF-with-Erlang-fallback pattern (`generate_keypair/0`,
`sha256/1`, etc.) — the fallback (`erlang_grind_puzzle/1`) is the same
loop `macula_identity` used to run itself, kept only for architectures
where the NIF fails to load. `macula_identity`'s own `grind/1` and
`grind_loop/3` are removed as dead code now that `generate/1` delegates
directly; `puzzle_valid/1,2`, `puzzle_evidence/1`, and
`has_leading_zero_bits/2` are unchanged and still the source of truth
the NIF's Rust implementation mirrors exactly (same evidence: SHA-256
of the raw 32-byte public key; same bit-prefix check).

Benchmarked at difficulty 16 (20 trials): ~998ms/grind natively vs.
~2470ms/grind via the old Erlang loop — about 2.5x. The native
implementation originally benchmarked *slower* than the Erlang loop
(`OsRng` draws hit the OS entropy source, a syscall, on every candidate
key); fixed by seeding a `StdRng` once from OS entropy and drawing from
that buffered CSPRNG for the whole grind, which is where essentially
all of the win comes from — worth remembering if grinding is ever
extended to run in parallel across dirty schedulers.

## [10.5.9] - 2026-08-27

### Root-caused: overlay_relay was never broken — the fleet's puzzle enforcement was rejecting test identities

The overlay_relay WAN-only vanishing-frame incident (opened at 10.5.0)
is closed. It was never a bug in `overlay_relay`, in the QUIC/Rust
layer, in frame codec/parsing, or in any relay logic. Confirmed by
resending the exact same reproduction with a puzzle-solving identity
(`macula_identity:generate(#{puzzle => true})`): the relay succeeds
end-to-end, with correct sender attribution and correct realm/payload
preservation. The chain that made non-puzzle-solving test identities
appear to break it:

1. `station-de-frankfurt.macula.io` runs with `puzzle_enforcement =
   enforce` (an operational config choice, not the SDK's `off`
   default).
2. A freshly-generated (non-puzzle-solving) identity's handshake is
   allowed to complete and reach `connected` — the SDK's own state
   machine has no knowledge of the puzzle check.
3. macula-station's `on_handshake_complete/3` independently validates
   the puzzle immediately after, and under `enforce` with an invalid
   puzzle, closes the connection (`macula_peering:close(Pid,
   puzzle_invalid)`).
4. That transitions `connected → draining`. `draining`'s handling of
   further inbound bytes is an intentional, by-design silent drop
   ("Ignore late inbound during drain") — no log, no counter. Any
   frame sent in the split-second before this closes — including the
   test's own overlay_relay frame — is silently discarded.
5. This is why it "only failed on the real fleet": local test stations
   never had `puzzle_enforcement` configured, so they default to
   `off` and the reject path never fires there. It was never a
   network-latency-sensitive race.

### Reverted — all temporary diagnostics from 10.5.1 through 10.5.8

`native/macula_quic/src/{stream,connection,message}.rs` and
`src/peering/macula_peering_conn.erl` are restored to their 10.5.0
state, **except** `drop_unexpected/4`, whose `logger:warning/2` fix
(10.5.6) is kept — that one was a real, pre-existing bug (unobservable
logging), not investigation-only instrumentation, and reverting it
would silently reintroduce it.

### Follow-ups (not done here, worth doing separately)

- The `connected → draining` window itself is real hardening
  material: a connection that has already been decided invalid can
  still accept and silently swallow traffic for one message before
  the close takes effect. Rejecting the puzzle check *before*
  promoting to `connected` would close that window rather than merely
  making it debuggable.
- `macula_station_listener.erl`'s `maybe_emit_puzzle_invalid/3` still
  uses `macula_diagnostics:event/2` and is silently dropped for the
  same domain-filter reason as everything else in this incident —
  worth the same one-line fix as `duplicate_replaced` got.
- `macula_diagnostics:event/2,3`'s `domain => [macula]` metadata being
  unconditionally dropped by the default `sasl`-enabled logger handler
  is a real, fleet-wide observability gap independent of this
  incident. Either fix the filter chain (allow `[macula]` explicitly)
  or stop using `domain` metadata in `macula_diagnostics` until
  Phase 7's real exporter lands.

## [10.5.8] - 2026-08-27

### Added — logs the close `Reason` on the `connected` → `draining` transition (TO BE REVERTED)

10.5.7 proved the overlay_relay frame is swallowed by `draining`'s
intentional, by-design silent late-inbound drop, on A's own connection
to the station, ~211ms after the frame was sent. The only transition
into `draining` from `connected` is `connected(cast, {close, Reason},
Data) -> {next_state, draining, Data}` — this logs `Reason` there
directly, to confirm (rather than infer by elimination) whether it is
`replaced_by_newer_handshake`, macula-station's own duplicate-handshake
guard (`macula_station_listener:maybe_close_old_worker/3`) — the only
close-call site whose timing profile fits a fresh connection being
closed within a few hundred ms of first use, rather than a dial
timeout or app-silence timeout.

**No functional change.** Follow-up patch removes all of 10.5.1's
through this logging once the incident is root-caused.

## [10.5.7] - 2026-08-27

### Added — diagnostic on the `draining` state's silent late-inbound drop (TO BE REVERTED)

10.5.6's `drop_unexpected/4` fix proved the overlay_relay frame's raw
`{quic, Bin, Stream, Flags}` message is never caught there either — no
`event_type=info` unexpected event ever fires for it, on top of
`connected/3`'s own frame-processing clause never matching it
(10.5.5's `parse_stream` diagnostic never once logged `bin_size=499`).
That leaves exactly one remaining code path: `draining(info, {quic, _,
_, _}, Data) -> {keep_state, Data}` — an intentional, by-design silent
drop ("Ignore late inbound during drain") with no logging at all. It
matches every symptom observed across 10.5.1-10.5.6: bytes read
correctly, delivered to the correct, unchanged-since-birth stream
owner's mailbox, and then gone without any trace, by design.

Logs `peer_node_id` and payload byte size whenever this clause fires,
using plain `logger:warning/2` (see 10.5.5/10.5.6 for why
`macula_diagnostics:event/2` would not work here).

**No functional change.** Follow-up patch removes all of 10.5.1's
through this logging once the incident is root-caused.

## [10.5.6] - 2026-08-27

### Fixed — `drop_unexpected/4`'s own logging was silently dropped, same as 10.5.4's

Not a temporary diagnostic this time — a real, pre-existing bugfix.
`drop_unexpected/4` is `macula_peering_conn`'s catch-all for any event a
connection's current state doesn't handle, and its entire purpose is
observability: log `_macula.peering.unexpected` and keep going. It used
`macula_diagnostics:event/2`, which suffers exactly the filter-chain
bug fixed for this incident's own diagnostics in 10.5.5 (see that
entry) — `domain => [macula]` metadata silently dropped by the default
`logger_std_h` handler on any release built with `sasl`. A function
whose only job is to be observed was, in practice, unobservable on
every station in the fleet. Switched to plain `logger:warning/2`.

Found while root-causing the overlay_relay WAN-only vanishing-frame
incident: 10.5.5's `parse_stream` diagnostic proved the frame's raw
bytes never reach `connected/3`'s frame-processing clause at all (its
`logger:info` firing correctly for every other frame type in the same
capture, but never once with the overlay_relay frame's exact byte
count) — `drop_unexpected/4` is the only remaining code path capable of
silently absorbing it, and its own logging bug meant nobody could have
seen it fire even if it did.

## [10.5.5] - 2026-08-27

### Fixed — 10.5.4's diagnostics were silently dropped by the default logger filter chain

Not a code-path bug: a logging-visibility one, found while trying to
read 10.5.4's output on the fleet. `macula_diagnostics:event/2,3`
stamps `domain => [macula]` on every event. The default `logger_std_h`
handler on any release that includes `sasl` (every macula-station box)
installs `filter_default => stop` plus filters that only explicitly
`log` two things: `[otp, sasl]`-domain reports and events with **no**
domain metadata at all. Anything else — including every single
`macula_diagnostics:event` call, this incident's temporary diagnostics
and macula-station's own pre-existing `overlay_relay_stats`-adjacent
events alike — falls through every filter unmatched and is dropped by
`filter_default => stop`. Confirmed directly on the live Frankfurt
node: a manually-triggered `logger:log` with `domain => [macula]`
metadata never reached `docker logs`; the identical report with no
domain metadata did.

Practical effect on this investigation: the three diagnostic cycles
between 10.5.3 and this one (peer_observer's `route/4` and `on_frame`
logging in macula-station, and this SDK's own `notify_frame`/
`parse_stream` logging added in 10.5.4) produced **no information at
all** — not a negative result, just silence, indistinguishable from
"never executed." The only diagnostics unaffected by this bug are the
`overlay_relay_stats/0` counters (`persistent_term`/`counters`, not the
logger) and the Rust-side `eprintln!` calls in `macula_quic` (bypass
Erlang's logger entirely).

10.5.4's `parse_stream` and `notify_frame` diagnostics now use plain
`logger:info/2` (no domain metadata) instead of
`macula_diagnostics:event/2`, so they are actually observable. This SDK
release does not fix the underlying filter-chain default itself (that
lives in macula-station's release config, not here) — it only makes
this incident's own temporary instrumentation visible again.

**No functional change.** Follow-up patch removes all of 10.5.1's
through this logging once the incident is root-caused.

## [10.5.4] - 2026-08-27

### Added — SDK-side send-path diagnostic in `macula_peering_conn` (TO BE REVERTED)

Follow-up to 10.5.3, and a change of layer. 10.5.1-10.5.3 proved the
overlay_relay bytes are read correctly, delivered to the correct,
unchanged-since-birth stream owner, and enqueued into that Erlang
process's mailbox (`send_and_clear` returns `Ok(())`). Two further
diagnostics added directly to macula-station's own
`macula_station_peer_observer.erl` (not requiring this SDK, since that
module is macula-station's own code) then showed something unexpected:
an unconditional log at the very entry of `on_frame/3` — which every
frame taking the "legacy controlling_pid" path must pass through —
never fired even once during a full reproduction capture, for this
frame or any other. That points further upstream than macula-station's
own dispatch logic, back into this SDK's own `macula_peering_conn`
module, which is the thing actually responsible for delivering
`{macula_peering, frame, ConnPid, Frame}` to `controlling_pid` in the
first place.

This adds three unconditional diagnostics: `connected/3`'s handling of
`{quic, Bin, Stream, Flags}` now logs `parse_stream/1`'s actual frame
count for each read (distinguishing a clean parse from a silent
`{more, _}` stall or a silently-swallowed `{error, bad_frame}`, both of
which look identical from outside); `notify_frame/2` and
`notify_bypass/5` now log the resolved `controlling_pid` target and
whether it was alive at send time, immediately before the `Pid ! Msg`
send that is the last SDK-owned step before the message left for
macula-station's mailbox.

**No functional change.** Follow-up patch removes all of 10.5.1's
through this logging once the incident is root-caused.

## [10.5.3] - 2026-08-27

### Added — stream-ownership identity diagnostic in `macula_quic` (TO BE REVERTED)

Follow-up to 10.5.2. 10.5.2 proved `message::send_data`'s
`env.send_and_clear(...)` returns `Ok(())` on the fleet for the exact
overlay_relay frame — the bytes are correctly read AND successfully
enqueued into a live Erlang process's mailbox, yet nothing downstream
ever observes the message, not even `macula_peering_conn`'s own
catch-all clause (`drop_unexpected/4`, which already logs and does not
fire). A message enqueued into a mailbox that nothing ever matches on
is consistent with delivery to the *wrong* (but still-alive) process —
so this instruments stream ownership identity itself.

Adds a `birth_owner: LocalPid` field to `StreamResource`, captured at
stream-creation time. `start_recv_loop`'s successful-read arm now logs
whether the stream's current owner still matches its birth owner
(`owner_unchanged_since_birth`); `nif_controlling_process` now logs
whether a reassignment actually changed the owner and whether the
prior owner was still the birth owner (`actually_changed`,
`was_birth_owner`). `rustler::LocalPid` has no `Debug` impl, so identity
is compared, not printed directly.

**No functional change.** Follow-up patch removes all of 10.5.1's,
10.5.2's, and this logging once the incident is root-caused.

## [10.5.2] - 2026-08-27

### Added — one more temporary diagnostic line in `macula_quic` (TO BE REVERTED)

Follow-up to 10.5.1. Deploying 10.5.1's tracing to the fleet proved the
overlay_relay bytes ARE correctly received by the Quinn/Rust layer
(`recv.read()` returns the exact expected byte count, loop continues
healthily, no error) — but the message never reaches ANY Erlang code:
neither the intended `macula_peering_conn` clause nor its own catch-all
(`drop_unexpected/4`, which already logs and did not fire). That points
at `message::send_data`'s `env.send_and_clear(...)` call itself, whose
`Result` was unconditionally discarded (`let _ = ...`) — the one part of
the whole path never actually checked. This logs that result.

**No functional change.** Follow-up patch removes both 10.5.1's and this
logging once the incident is root-caused.

## [10.5.1] - 2026-08-27

### Added — temporary `macula_quic` diagnostic logging (TO BE REVERTED)

`eprintln!`-based tracing in `native/macula_quic/src/{stream,connection}.rs`
at the NIF boundary: `nif_send`'s `write_all` call (stream id, byte length,
result), `start_recv_loop`'s `recv.read()` outcomes (bytes read, EOF,
reset, error), `nif_setopt_active`'s active-flag transitions, and stream
creation in `nif_open_stream`/`nif_async_accept_stream` (stream ids +
role). Lands on stderr, which `docker logs` captures on the fleet.

**Why:** `overlay_relay` (10.5.0, Layer 2 plan Phase 3.5) passes CI's
local test-cluster suite but silently fails to deliver on the real
7-station fleet. Every layer of `macula`'s own Erlang code (frame codec,
send path, receive dispatch) has been individually proven correct via
live `dbg`/`recon` tracing and a local reproduction against the actual
release binary — the failure is real-network-specific and below the
Erlang layer. This logging is the next diagnostic step, not a fix.
Neither `tc netem` delay (30ms±5ms) nor delay+loss (20ms±5ms + 1%)
reproduces it on loopback, so this instruments the one layer never
directly observed: the Quinn/Rust NIF boundary itself.

**No functional change.** Follow-up patch removes this logging once the
incident is root-caused.

## [10.5.0] - 2026-08-26

### Added — `overlay_relay` frame + `macula_station_link:send_overlay_frame/3`

Point-to-point overlay-frame delivery by NodeId (Layer 2 plan Phase 3.5).
`send_overlay_frame/2` (10.3.0) delivers only to whoever is on the other
end of one specific connection — correct for a direct station-to-station
link, but a realm member reachable through a relay had no way to actually
address a specific third-party peer. `send_overlay_frame/3(Client,
TargetPeer, Frame)` wraps `Frame` in a new `overlay_relay` envelope frame
(`peer` + opaque encoded `payload`) that a station forwards to whichever of
its *other* connections authenticates as `TargetPeer` — see
`macula-station`'s own changelog/commit for the relay side. `{error,
not_connected}` if the peering handshake to the station itself hasn't
completed; silently dropped by the station if `TargetPeer` isn't currently
connected there (HyParView's own shuffle/retry is the recovery path,
same as it already tolerates ordinary packet loss). `send_overlay_frame/2`
is unchanged.

### Added — `macula_record:read_node_record/1`

Mirrors the existing `read_station_endpoint/1` — turns a decoded
`node_record` payload back into a typed map (`node_id`, `station_id`,
`realms`, `capabilities`, `kind`). The one reader that was missing;
every other record type already had one.

### Fixed — `overlay_subscribe/3`'s `Meta.sender` was the wrong identity for a relayed frame

Delivering a frame that arrived via the new `overlay_relay` envelope now
stamps `Meta.sender` from the envelope's own `peer` field (the true
logical HyParView peer that originated it) instead of
`state.peer_node_id` (this connection's own directly-connected peer —
always a station's identity once a relay hop exists, never the actual
third-party sender). A real bug found while designing point-to-point
relay, not shipped before this version had a consumer that could
distinguish the two.

## [10.4.0] - 2026-08-26

### Added — HyParView + Plumtree overlay, absorbed from macula-hyparview/macula-plumtree

The standalone `macula-hyparview` and `macula-plumtree` packages — extracted from
`macula-station`'s `apps/hecate_overlay/` earlier the same day — are folded into
this SDK under `src/overlay/`, rather than published as two more loosely-coupled
repos. Both were only ever going to be consumed alongside `macula` itself (they
already depend on `macula_record`/`macula_frame`/`macula_identity`), and keeping
them separate meant a two-package version-coordination step before Phase 4 of
the realm-membership work (`macula-io/macula-realm-identity`) could even build
against the client-facing overlay transport this SDK shipped in 10.3.0.

Module names are unchanged on the move, matching this org's established
extraction convention (keep the name when there's an existing external caller by
that exact name, rename freely otherwise): `hecate_plumtree`, `hecate_pubsub`,
`hecate_pubsub_server`, `hecate_pubsub_registry`, and `hecate_or_set` keep their
`hecate_*` names because `macula-station`'s own `macula_station_sup` starts
`hecate_pubsub_registry` directly as its pubsub backbone — folding them in here
required no code changes in `macula-station` beyond its dependency declaration.
`macula_hyparview_view`, `macula_hyparview_proto`, and
`macula_hyparview_endorsement` already carried the `macula_` prefix from their
own earlier rename and needed none.

Not carried over: `macula_plumtree_app`/`macula_plumtree_sup`, the standalone
package's OTP application shell — an empty supervisor with no children (its own
moduledoc: "no children are owned here") that existed only so
`application:start/1` had something to call. `macula` already provides that.

New guides: [`docs/guides/overlay/HYPARVIEW_GUIDE.md`](docs/guides/overlay/HYPARVIEW_GUIDE.md)
and [`docs/guides/overlay/PLUMTREE_GUIDE.md`](docs/guides/overlay/PLUMTREE_GUIDE.md),
each with a new SVG diagram (`assets/hyparview_views.svg`,
`assets/plumtree_broadcast_tree.svg`). No supervised OTP wrapper exists yet for
either protocol (unlike RPC/PubSub/Content/Streaming), so each guide covers both
the "why" and the raw functional API, including the realm-gated admission flow
built on `macula_record:realm_member_endorsement/2,3` and this SDK's own
overlay transport (`macula_station_link:overlay_subscribe/3`,
`send_overlay_frame/2`, 10.3.0).

Verified: `rebar3 compile xref eunit ct dialyzer` clean (1940/1941 eunit —
the one failure is the pre-existing `macula_station_link_tests:
disconnect_notifies_subscribers_test_` Ref-comparison race noted in 10.3.0's
own development, unrelated to this change and untouched by it), `rebar3 as
lint lint` clean, `rebar3 ex_doc` builds with both new guide pages and both
new diagrams rendering correctly (checked in a real browser, not just
`xmllint`). `macula-station`'s dependency on the standalone `macula_plumtree`
git package is removed in the same pass — see its own CHANGELOG.

## [10.3.0] - 2026-08-26

### Added — `macula_station_link:overlay_subscribe/3`, `overlay_unsubscribe/2`, `send_overlay_frame/2`

A client (daemon, or `macula-io/macula-realm`) can now actually send and receive
overlay-protocol frames — HyParView `hyparview_*` (10.2.0), Plumtree `plumtree_*`,
and any future frame type the built-in call/event handling doesn't recognise.
Previously `on_frame/2`'s catch-all silently dropped every such frame; nothing in
the SDK's client-facing API could send or subscribe to one either. SWIM and
content-transfer frames are unaffected — they already have their own dedicated
paths and never reach the new catch-all-turned-fan-out clause.

`overlay_subscribe/3` registers interest per realm (no topic dimension, no
wire-level SUBSCRIBE/UNSUBSCRIBE round trip — these frames already arrive
addressed at a specific connection, not fanned out by topic like PUBLISH/EVENT)
and delivers `{macula_overlay_frame, SubRef, Frame, Meta}` (`Meta` carries
`sender`, the connected peer's NodeId — a frame doesn't self-identify its sender
at the application layer) or `{macula_overlay_gone, SubRef, Reason}` on
disconnect. `send_overlay_frame/2` is a raw transport primitive: the caller
builds and signs the frame itself (e.g. via `hecate_overlay_proto:build_join/1`),
this just puts it on the wire.

This is the piece `macula-station`'s `hecate_overlay` admission-gating fix
(10.2.0) needed a consumer for — the protocol logic and the endorsement wire
format existed, but nothing could actually drive a HyParView session from an
Elixir client until now.

### Fixed — elvis `no_deep_nesting` violations

Six pre-existing level-3 nestings, all the same shape: a spawned worker's
`fun() -> ... end` wrapping a `case`/`try` one level too deep for this repo's
`macula_min` ruleset (limit 2). Extracted each into a named top-level
function the spawn just calls, in `macula_content_transfer.erl` (two sites),
`macula_download.erl`, `macula_feeder.erl`, `macula_pusher.erl`,
`macula_dist_relay_client.erl`, and `client/macula_station_link.erl` — no
behavior change, `rebar3 as lint lint` now passes clean.

## [10.2.0] - 2026-08-26

### Added — HyParView `hyparview_join`/`hyparview_forward_join`/`hyparview_neighbor` can carry a `record`

The three HyParView admission-relevant frames (Part 3 §7.1, shipped wire-format-only
in the 5.x line) gain an optional `record` field carrying a `macula_record:m_record()`
— e.g. a signed `realm_member_endorsement` (Part 6 §9.6) proving the frame's subject
is authorised to join a realm's overlay. Reuses the existing generic `prepare_records/1`/
`restore_records/1` encode/decode machinery already used by `store`/`replicate` (same
field name, same automatic CBOR handling — no manual encode/decode needed by callers).
Backward compatible: omitting `record` produces the exact same frame shape as before.

This closes a real gap found downstream in `macula-io/macula-station`'s
`hecate_overlay` app: `hecate_realm_join:build_join/4` computed a signed endorsement
and then discarded it (no frame field existed to attach it to), so admission gating
could never actually verify anything. `hyparview_neighbor` needed the same field for
a separate reason — it can arrive unsolicited (shuffle-driven promotion), not only as
an ack to a JOIN the receiver itself initiated, so it's an admission event in its own
right and needed to be able to prove membership independently.

## [10.1.1] - 2026-08-23

### Fixed — `get_content/2`, `get_content_station/4,5`, and `macula_download` crashed on a malformed MCID

`macula_content_transfer:is_chunked/2` dispatches on an MCID's two-byte
codec prefix (`16#55` single-block, `16#56` chunked manifest) and has
no catch-all clause — any other shape raised `FunctionClauseError` in
the linked worker (and, for `get_content*`, in the calling process too,
via `gen_server:call(..., :await, :infinity)`). Reachable with
attacker- or corruption-supplied input on any path that decodes bytes
into an MCID before fetching it: a content-addressed image/file proxy
serving a `hex_string -> get_content` route, a stored reference that
got corrupted, or a share link.

`get_content/2`, `get_content_station/4,5`, and `macula_download`'s
`start_link*/4,5` (`init/1`) now validate the MCID's shape before
dispatching, returning `{error, invalid_mcid}` instead of crashing.
`macula_feeder`/`put_content*` were never affected — `is_chunked/2`'s
`put` clause dispatches on byte size, not shape.

## [10.1.0] - 2026-08-23

### Added — `reuse_sup` opt for `macula_streamer`/`macula_response` `advertise/6`

A station's wire-level registration for a procedure (its
`macula_remote_advertise_registry` entry) is tied to whichever
connection sent the `ADVERTISE` frame, and does not survive that
connection being replaced (reconnect, station-side eviction, a
newer handshake from the same identity superseding the old one).
Nothing previously re-sent that frame after the initial advertise,
so a long-running provider's registration could silently go stale
while its own local `advertised => true` bookkeeping never noticed.

A periodic re-advertise was the obvious fix, blocked by one thing:
plain `advertise/5,6` starts a brand new factory supervisor on every
call, so calling it on a timer leaked one orphaned supervisor per
tick. `reuse_sup => Sup` (the pid the first call returned) skips
that — re-sends the wire frame (and, via `advertise_direct/6,7`,
re-publishes the DHT record) against the existing supervisor
instead of starting a new one.

Confirmed live: this was the last piece of a long-running
`unknown_next_peer` investigation traced through hecate-tube,
macula-realm, and two macula-station bugs this same day — see
`macula` 10.0.1/10.0.2 and hecate_om 0.14.1/0.14.2's own CHANGELOG
entries for the rest of the chain.

- **`src/macula_streamer.erl`**, **`src/macula_response.erl`**:
  `advertise/6` accepts `reuse_sup` in `Opts`; `advertise_direct/6,7`
  forward it through (already forwarded `Opts` wholesale). Purely
  additive — omitting `reuse_sup` keeps the exact prior behavior.

## [10.0.2] - 2026-08-23

### Fixed — `macula_identity:save/2` crashed instead of returning `{error, Reason}`

`save/2`'s own `-spec` promises `ok | {error, term()}`, but the implementation
did `ok = filelib:ensure_dir(Path)` — a bare match that raised an unhandled
`MatchError` whenever `ensure_dir` failed (unwritable parent directory,
misconfigured path, etc.), instead of returning `{error, Reason}` like every
other failure branch in this function already does.

Found live: `macula-realm`'s `MaculaRealm.Mesh` calls `save/2` from inside a
required, supervised `GenServer`, reasonably trusting the documented
contract — no `try/catch` around it. The unhandled crash took the entire
hosting OTP application down with it (repeated `init` crashes exceeded the
supervisor's restart intensity), `Ecto.Repo` included, whenever the
configured `mesh_identity_path` wasn't writable — reproduced with a
non-writable default path on a box where it wasn't provisioned.

- **`src/identity/macula_identity.erl`**: `save/2` now dispatches on the
  `ensure_dir` result instead of asserting it, returning `{error, Reason}`
  on failure like the rest of the function already does. No behavior change
  on the success path.

## [10.0.1] - 2026-08-23

### Fixed — direct-dial advertisement publish failures were silently swallowed

`macula_streamer:advertise_direct/6,7` and `macula_response:advertise_direct/6,7`
discarded the result of `macula_direct_dial:publish_advertisement/5` with
`_ = ...`. The DHT publish is intentionally best-effort — a provider stays
reachable via the pooled path even if it fails — but "best-effort" was
implemented as "the caller never learns", not "the caller keeps working and
finds out". A provider whose publish failed once had `advertised => false`
forever, with nothing anywhere to say why, since nothing retries and nothing
logs.

Found live: a hecate-tube instance's `tube_mesh_providers` reached
`advertised => true` for the first time after an unrelated identity fix, then
direct-dial calls into it still failed with `{unresolved,
procedure_not_advertised}`. Tracing it back showed
`macula_direct_dial:publish_advertisement/5` returning `{error, timeout}` on
every call, three times in a row, well after any startup race could explain
it — and neither `advertise_direct/6,7` caller ever surfaced that.

- **`src/macula_streamer.erl`**, **`src/macula_response.erl`**: both
  `advertise_direct/7` now log a `?LOG_WARNING` when the publish fails,
  naming the procedure and the reason, instead of discarding it. The return
  contract is unchanged — `{ok, Sup}` still comes back even on publish
  failure, matching the documented best-effort design; only the silent
  discard is fixed.

This does not fix a `put_record` timeout itself, if the underlying DHT
publish is failing for some other reason — it makes that failure observable
instead of invisible.

## [10.0.0] - 2026-08-22

### Removed — macula-net L3 substrate

Deleted the sovereign-IPv6 overlay entirely: crypto-derived addressing,
the TUN device, DHT-backed station/address resolution, the hosted-identity
gateway, and their dedicated observability. Verified before removing —
see rationale below, not a routine cleanup.

- **Source**: `src/macula_net/`, `src/route_packet/`, `src/deliver_packet/`,
  `src/derive_address/`, `src/manage_tun_device/`, `src/advertise_station/`,
  `src/resolve_address/`, `src/cache_route/`, `src/host_identity/`,
  `src/attach_identity/`, `src/host_attach_controller/` — all 10 macula-net
  `src_dirs` entries, plus `src/observability/macula_metrics.erl`,
  `macula_metrics_http.erl`, `macula_packet_trace.erl` (their moduledocs
  named them macula-net-only; confirmed no other caller before deleting).
- **`macula_root.erl`**: dropped the `metrics_children`/`metrics_http_children`
  observability wiring — it started only those two now-removed workers.
- **`macula_record.erl`**: removed `address_pubkey_map/2,3`,
  `host_delegation/5,6`, `sign_host_delegation/2`, `verify_host_delegation/1`,
  `hosted_address_map/3,4` and their storage-key clauses (type tags `0x13`,
  `0x14` — retired, not reassigned, in case a station somewhere still holds
  a stored record under either). **Kept `station_endpoint/2,3` and
  `station_endpoint_key/1`/`read_station_endpoint/1` in full** — grep
  confirmed live callers in `macula_feeder`, `macula_pusher`,
  `macula_direct_dial`, and `macula_request`: every station publishes its
  own `station_endpoint` automatically and direct-dial (RPC/content/
  streaming) resolves through it. It only ever shared a directory with
  macula-net, not macula-net's logic. Its TTL macro survives renamed
  `?MACULA_NET_TTL_MS` → `?STATION_ENDPOINT_TTL_MS`.
- **Native**: deleted `native/macula_tun_nif/` (Rust, `tun-rs`) and its
  `priv/build-nifs.sh` build step. It was never in the hex package's own
  `files` list in `macula.app.src` (now removed there too) or in
  `rebar.config`'s `{hex, [{files, ...}]}` — the published package never
  shipped it.
- **`macula.app.src`**: ~~dropped `inets` from `applications`~~ — **kept.**
  The first pass assumed `macula_metrics_http` was `inets`'s only caller
  and dropped it; `rebar3 dialyzer` caught a live second caller before
  release (`macula_relay_discovery:fetch_topology/1`, bootstrap topology
  over HTTPS — unrelated to macula-net), so `inets` stays.
- **Tests**: 25 test files covering the substrate, its phases, and its
  e2e/bench suites.
- **Scripts**: 11 demo/soak scripts (`lan-demo.sh`, 5×`lan_demo_*.erl`,
  4×`netns*-demo.sh`, `soak.sh`).
- **Docs**: one table row in `docs/guides/DEVELOPMENT.md`.

### Why

Dormant since 2026-05-08 (Phase 4.7) — no commits in over three months
while the SDK shipped v3.15 through v9.13.8. Live in the code at removal
time: `macula_net_transport_quic.erl` generated a throwaway self-signed
cert with server-name verification explicitly skipped
(`%% Phase 1: skip server name verification — self-signed certs.`), and
`macula_deliver_packet.erl` still stubbed ctrl/gossip envelope handling
(`%% ctrl/gossip handlers land in Phase 1.5+.`) — both items Phase 1's own
changelog entry named as deferred to "Phase 4 hardening," which shipped
observability and benchmarking instead and never closed them. No multi-hop
routing was ever built. Never exposed through `macula.erl` (its own facade
was `macula_net.erl`), never in the README's feature list, never in the
hexdocs `extras` guide corpus — always a self-contained, undocumented
side subsystem, not part of what this SDK advertises.

Verified zero external dependents before removal: nothing in any other
repo in the workspace calls `macula_net`, `macula_route_packet`,
`macula_resolve_address`, `macula_advertise_station`, `macula_cache_route`,
`macula_host_identity`/`macula_attach_identity`/`macula_host_attach_controller`,
or `macula_tun*`. Two doc-comment mentions elsewhere (`macula-station`'s
listener, `hecate-daemon`'s DNS A-record synthesizer) are descriptive, not
calls — one of them literally documents that the listener does *not*
depend on macula-net. `macula-testkit`'s own spike concluded macula-net's
transport behaviour isn't reachable from the pub/sub path at all. The
planning corpus (`macula-io/macula-architecture/plans/PLAN_MACULA_NET*.md`)
drafted a Phase 5 (federation, deferred) and a Phase 6 (transport
pluggability — the actual off-grid-mesh rationale: BATMAN wifi, LoRa,
satellite as swappable transports), both `status: v1.0-draft`, never
implemented; that corpus is untouched by this change, since it lives in a
different repo and remains the record of what was intended.

### Bump rationale

MAJOR: `macula_net`, `macula_tun`, and every macula-net module were
exported, callable public modules, even though undocumented in the
README/guides. Removing them is a breaking change for any external
caller, however unlikely one is, given the verification above.

### Notes

- `rebar3 compile` clean (`warnings_as_errors` is on — nothing left
  referencing a removed module).
- `rebar3 eunit`: 1783 passed, 0 regressions attributable to this change.
  One test fails intermittently across repeated full-suite runs, but a
  *different* unrelated test each time (station-link disconnect handling
  on one run, seed-URL parsing on another) — a pre-existing timing flake
  in the suite, reproduced in isolation before this change and unrelated
  to anything touched here.

---

## [9.13.8] - 2026-08-21

### Removed (documentation)

- **`docs/BENCHMARKS.md`** — asked about directly as a follow-up to
  9.13.7's cleanup ("what about BENCHMARKS?"). Verified before removing:
  stale (single dated run, 2026-05-03, ~3.5 months old at removal time),
  workstation-only and explicitly self-caveated as not representative
  ("Re-run on production-class hardware... this MD just establishes the
  substrate baseline isn't absurd"), scoped narrowly to one internal
  subsystem (the macula-net L3 substrate specifically, not the SDK
  broadly), carries a dead cross-repo link (`macula-internal`, the
  pre-2026-07-26-rename org name, mixed with a Codeberg-style URL path on
  a github.com host), not published to hexdocs, and not cross-referenced
  from anywhere else in the live docs tree — an orphan file.

### Notes

- No `rebar.config` change needed — this file was never in `extras`.
- `rebar3 ex_doc`/`compile` clean; repo-wide grep confirms zero remaining
  live references outside `CHANGELOG.md`.

---

## [9.13.7] - 2026-08-21

### Removed (documentation)

- **Removed public-inappropriate and stale content flagged directly by the
  maintainer:** `docs/migrations/`, `docs/PLAN_SDK_3_16.md` /
  `PLAN_SDK_3_17.md` / `PLAN_SDK_3_17_PROGRESS.md`,
  `docs/HANDOVER_MULTIHOP_PUBSUB_PROPAGATION.md`, `docs/ROADMAP.md`,
  `docs/SOAK_2026-05-03_sanity.{csv,report.txt}`. Verified each rather than
  removing on request alone:
  - `docs/migrations/V1_TO_V2_PUBSUB.md` — its own text says "V1 is gone"
    as of 4.0.0. Current version is 9.13.7; nobody migrating onto a current
    SDK is coming from a pre-4.0.0 install.
  - `docs/PLAN_SDK_3_16.md`/`PLAN_SDK_3_17.md`/`PLAN_SDK_3_17_PROGRESS.md`
    — internal planning documents, and this repo already has an
    established `plans/PLAN_*.md` convention (used by
    `PLAN_PUSH_UPLOAD.md` etc.) that these predate and sit outside of.
  - `docs/HANDOVER_MULTIHOP_PUBSUB_PROPAGATION.md` — an internal incident
    investigation and handover document naming specific internal hosts and
    deployments (`parksim-leuven`, station names, live triage steps) —
    genuinely not public-facing material.
  - `docs/ROADMAP.md` — carried its own `⚠️ OUTDATED` banner and told
    readers to trust `CHANGELOG.md` instead; redundant with the file it
    was already deferring to.
  - `docs/SOAK_2026-05-03_sanity.csv` / `.report.txt` — raw output from one
    dated soak-test run, no narrative, not referenced from anywhere.
  - `docs/ROADMAP.md` and `docs/migrations/V1_TO_V2_PUBSUB.md` were both
    published to hexdocs (`rebar.config`'s `extras`) — removed those
    entries, plus every live cross-reference: `docs/README.md` (Quick
    Navigation row, the whole "Migrations" section, the Roadmap reference
    row), `docs/guides/shared/CONNECTING_GUIDE.md`,
    `docs/guides/pubsub/PUBSUB_GUIDE.md` (audience line + See also),
    `src/pubsub/macula_pubsub.erl`'s moduledoc. Also removed a
    long-dead `%% See architecture/ROADMAP.md` comment in `rebar.config`
    pointing at a directory this repo has never had.
  - `CHANGELOG.md`'s own prior entries mentioning these files are left
    untouched — historical record of what was true when written, per this
    project's standing convention, not live navigation.

### Notes

- All removed files stay recoverable via git history; nothing here is
  destructive at the version-control level, only removed from the current
  tree and the published hex package / hexdocs site going forward.
- Verified via a repo-wide grep after the edits: zero remaining live
  references to any removed path outside `CHANGELOG.md`/
  `CHANGELOG_LEGACY.md`. `rebar3 ex_doc` rebuilt clean (no missing-file
  errors, no orphaned pages); `rebar3 compile` clean; every remaining
  `.md`/anchor link in the repo (155 of them) re-checked against real
  rendered HTML.
- No code changes — documentation and `rebar.config` only, plus the one
  `.erl` moduledoc comment trim in `macula_pubsub.erl`.

---

## [9.13.6] - 2026-08-21

### Changed (documentation)

- **`PUBSUB_GUIDE.md`/`PUBSUB_PROTOCOL.md` restructured to match the
  Overview → `Supervised wrappers: X / Y` → [pair content] → `See also`
  shape RPC/Content/Streaming already use.** Flagged directly: "why does
  PUBSUB_GUIDE have a different structure from the rest?" 9.13.5's split
  carried PubSub's pre-existing skeleton over unchanged (`TL;DR` instead
  of `Overview`, independent top-level `## Subscribing`/`## Publishing`
  sections each mixing wrapper usage with deeper protocol semantics)
  rather than conforming it to the template RPC established. Considered
  and rejected splitting into separate `PUBLISH_GUIDE.md`/
  `SUBSCRIBE_GUIDE.md` files first — checked against the actual
  constraint ("shape must be the same"): a two-file PubSub would make its
  file topology diverge from RPC/Content/Streaming's one-Guide-one-Protocol
  shape, the exact inconsistency being removed, not a way to remove it.
  - `PUBSUB_GUIDE.md`: `TL;DR` → `Overview` (prose only, no inline code,
    matching the other three); `## Subscribing` + `## Publishing` merged
    into one `## Supervised wrappers: macula_subscriber / macula_publisher`
    section holding only wrapper-usage mechanics (subscriber side, then
    publisher side — subscriber first because it's the long-lived,
    passively-waiting side, the same role Provider plays in
    `macula_response`/`macula_streamer`/`macula_feeder`); the deeper
    protocol semantics that used to live inside those two sections —
    delivery ordering, dedup, delivery guarantees, subscription
    termination — promoted to their own top-level sections after it, in
    the slot RPC's `## Errors` occupies. `Three core ideas` moved to
    directly follow `Supervised wrappers`, same slot.
  - Corrected an accuracy gap surfaced while rewriting the wrapper intro
    paragraph: the old text implied `pubsub.*_v1` mesh facts fire "around
    every operation" on both sides — checked `macula_subscriber.erl`
    directly rather than assuming, and it publishes none at all. Only
    `macula_publisher` announces (`pubsub.publish_started_v1` /
    `pubsub.publish_completed_v1`); a subscription has no single "done"
    moment to announce. Fixed in both files.
  - `PUBSUB_PROTOCOL.md` reordered to match: `Subscribing` now includes
    its own `### Subscribing in a callback module` subsection immediately
    (previously separated from it by the terminal-message content), and
    `When the subscription ends` promoted to a top-level section between
    `Subscribing` and `Publishing`, mirroring the Guide.
  - Every `X / Y` wrapper-pair mention flipped from `macula_publisher /
    macula_subscriber` to `macula_subscriber / macula_publisher` in both
    files, matching the "passive/long-lived side named first" convention
    already consistent across RPC (`macula_response`/`macula_request`),
    Content (`macula_feeder`/`macula_download`), and Streaming
    (`macula_streamer`/`macula_stream_sink`).
  - Confirmed via source (`macula_publisher.erl`, `macula_subscriber.erl`)
    that neither wrapper has a `start_link_direct`/`advertise_direct`
    variant — PubSub genuinely has no direct-dial mode, so (unlike the
    other three pairs) the restructured `## Supervised wrappers` section
    has no `### Direct-dial` subsection. Not an oversight; confirmed
    absence, not assumed.

### Notes

- Zero content lost: every code block (20/20) and every table row (39/39)
  from the pre-restructure files is present verbatim in the restructured
  ones — scripted diff against the prior commit, not a visual skim.
- Every anchor link touched by the reorder (11 cross-references between
  the two files, plus this file's own internal links) re-verified against
  `id="..."` attributes in real `rebar3 ex_doc` output, not assumed correct
  from the rename.
- No code changes — documentation only.

---

## [9.13.5] - 2026-08-21

### Changed (documentation — supersedes 9.13.4's reorder)

- **Split each of the four primitive-pair guides into a daemon-facing Guide
  and a library-facing Protocol doc, and gave `docs/guides/` real
  subdirectory structure.** 9.13.4 reordered sections within one file per
  guide so the supervised wrapper came first; that turned out not to be
  enough — three of four guides still *opened* (TL;DR/Overview) with raw
  code before the reordered wrapper section, because reordering moved a
  section without touching what greeted the reader first. The real fix is
  a hard split: a Guide that only ever shows `macula_request`/
  `macula_publisher`/`macula_feeder`/`macula_download`/`macula_pusher`/
  `macula_upload`/`macula_streamer`/`macula_stream_sink`, and a Protocol
  doc holding every raw primitive, wire format, and internal-resolution
  detail the Guide used to carry.
  - New layout: `docs/guides/{rpc,pubsub,content,streaming}/*_GUIDE.md` +
    `*_PROTOCOL.md`, `docs/guides/shared/*_GUIDE.md` for the five
    cross-cutting guides (Connecting, Topic Naming, Authorization, MRI,
    Records), and `DIST_OVER_MESH_GUIDE.md`/`CLUSTERING_GUIDE.md`/
    `DEVELOPMENT.md` staying flat at `docs/guides/` (not primitive-pair
    material). All nine moves used `git mv` to preserve history.
  - **`docs/guides/content/CONTENT_GUIDE.md` gained the "Push/upload:
    `macula_pusher` / `macula_upload`" section, moved wholesale out of
    `STREAMING_GUIDE.md`.** It's `client_stream` mode with content's own
    integrity machinery bolted on, not really a streaming-primitives
    concern — it belongs next to `macula_feeder`/`macula_download`, the
    wrappers it borrows that machinery from.
  - Each new `*_PROTOCOL.md` opens with the raw primitive walkthrough that
    used to open its Guide (e.g. `macula:subscribe/5`/`publish/4`,
    `macula:call_stream_station/6`), plus everything genuinely raw-only:
    MCID wire format, BOLT#4 error tables, DHT resolution internals,
    `macula_content_transfer`'s real cancel/pause/resume/multi-stream API
    (none of which `macula_feeder`/`macula_download` expose — confirmed by
    reading their source, not assumed), and local in-process streams.
    Confirmed which PubSub options genuinely reach through
    `macula_subscriber`/`macula_publisher`'s own `Opts`/`Args` parameters
    by reading `macula_subscriber.erl`/`macula_publisher.erl` directly
    before deciding the split boundary — `subscribe`'s delivery-ordering
    options pass through the wrapper, `publish`'s `timeout_ms` does not.
  - `rebar.config` gained `groups_for_extras`, clustering the four Guides,
    four Protocols, and five Shared guides into their own hexdocs sidebar
    groups — mirrors 9.13.4's `groups_for_modules` but for the extras
    sidebar. Confirmed by inspecting `doc/dist/sidebar_items-*.js` for the
    actual group assignments, not just a clean build.
  - `README.md` and `docs/README.md`'s documentation tables gained a row
    per new Protocol doc.
- **Found and fixed a real anchor-slug bug in 9.13.4's own cross-references**
  while verifying this split's links against actual rendered HTML (not just
  file existence): ex_doc collapses ANY run of non-alphanumeric characters
  in a heading — an em dash, a slash, a colon, any combination — to exactly
  **one** hyphen, not one hyphen per character removed. `## Supervised
  wrappers: \`macula_response\` / \`macula_request\`` slugs to
  `supervised-wrappers-macula_response-macula_request` (one hyphen at the
  slash), not `...-macula_response--macula_request` (two) — 9.13.4's own
  RPC cross-references used the double-hyphen form and silently linked to a
  dead anchor on hexdocs, never caught because the build exits 0 and the
  href text looked plausible. Swept the whole `docs/` tree for the same
  double-hyphen pattern and fixed all nine instances found (RPC, Content,
  Streaming, PubSub), then re-verified every anchor link in the repo
  (34 of them) against `id="..."` attributes in the real built HTML, not
  the crude markdown-only slug approximation used earlier in the session.

### Notes

- Every split was verified content-preserving before commit, same method
  as 9.13.4: a scripted heading-set and code-block diff between each
  original file (via `git show HEAD:...`) and the union of its resulting
  Guide + Protocol files. Zero headings or code blocks lost across all
  four splits — RPC (13→14 code blocks, +1 deliberate new example),
  PubSub (21→20, 3 raw→wrapper example rewrites accounted for), Content
  (16→16, only heading-level promotions), Streaming (13→13, only heading
  additions).
- No code behavior changes — documentation and `rebar.config` only, plus
  two `.erl` moduledoc comments (`macula_topic.erl`, `macula_pubsub.erl`)
  updated to point at the new guide paths. `rebar3 compile` and `rebar3
  eunit` clean (1902/0 failed); `rebar3 ex_doc` exit 0 with only
  pre-existing CHANGELOG.md autolink warnings, unrelated to this change.

---

## [9.13.4] - 2026-08-21

### Changed (documentation)

- **The RPC, PubSub, Content, and Streaming guides now lead with the
  supervised wrappers, not the raw wire primitives.** Prompted by the design
  question "devs will use the high-level stuff — why do the guides teach the
  raw APIs first?" Checked the actual heading order rather than assuming:
  three of four guides buried their `macula_response`/`macula_publisher`/
  `macula_streamer`/`macula_feeder`-family sections 40-70% of the way
  through, after extensive raw-primitive walkthroughs, even though every
  guide's own `> **Audience:**` line says "applications" — not SDK
  contributors. Root cause: the raw primitives are original, the supervised
  behaviours arrived later in stages (macula_streamer/macula_feeder at
  9.2.0, macula_publisher as late as 9.4.0, macula_pusher/macula_upload
  today), and each got appended as a trailing section rather than
  triggering a reorder.
  - `RPC_GUIDE.md` — moved "Supervised wrappers: macula_response /
    macula_request" (plus its own direct-dial subsection) to right after
    Overview; the raw `advertise`/`call`/`call_station` walkthroughs now
    follow, explicitly marked as "reach for this directly only if you're
    building something the wrapper doesn't fit."
  - `PUBSUB_GUIDE.md` — moved "Subscribing with macula_subscriber
    (supervised)" and "Publishing with macula_publisher (supervised)" to
    immediately follow their raw call's signature, ahead of the deeper
    protocol mechanics (delivery ordering, dedup) and the hand-rolled
    gen_server pattern, which stayed in place with a "the raw pattern
    macula_subscriber wraps" framing note.
  - `STREAMING_GUIDE.md` — moved "Supervised wrappers: macula_streamer /
    macula_stream_sink" (and the "Push/upload" section that was already
    correctly positioned right after it) ahead of "Consumer side"/"Provider
    side"'s raw `call_stream`/`advertise_stream` walkthroughs.
  - `CONTENT_GUIDE.md` — already led with its wrapper section (added during
    this session's own PLAN_PUSH_UPLOAD.md work); only a short consistency
    callout added, matching the other three guides' framing.
  - Every heading's TEXT was left unchanged specifically to keep anchor IDs
    stable — verified with a repo-wide scan for cross-references into these
    four guides' sections (README.md, other guides, `.erl` moduledocs,
    `plans/`) before and after: zero external references existed to any of
    the moved sections, and the load-bearing internal ones were spot-checked
    against their real heading text after the move.
- **`rebar.config` gained `groups_for_modules`**, clustering the 10
  supervised-wrapper modules (`macula_request`, `macula_response`,
  `macula_publisher`, `macula_subscriber`, `macula_feeder`,
  `macula_download`, `macula_pusher`, `macula_upload`, `macula_streamer`,
  `macula_stream_sink`) under a "Supervised Wrappers" group on hexdocs'
  module sidebar — previously flat/alphabetical, with no signal
  distinguishing them from `macula_client`/`macula_station_link`/
  `macula_stream`/`macula_quic` and the rest of the ~60 other modules an
  application almost never touches directly. Confirmed rendering by
  inspecting the built `doc/dist/sidebar_items-*.js` output directly, not
  just a clean build exit code.

### Notes

- A deliberate alternative NOT taken: physically splitting the wrappers
  into a separate hex package (`macula-sdk`) or a `macula/sdk/`
  subdirectory. This codebase has tried variants of that split twice
  before and reverted both times — a standalone `macula-sdk` repo
  (deprecated, folded back in) and a `macula-v2` umbrella of separate apps
  (fully absorbed as of 3.7.0). Checked why: the wrappers reach past
  `macula.erl`'s facade into internal modules inconsistently even among
  themselves (`macula_streamer` calls `macula_stream` directly;
  `macula_stream_sink` goes through the `macula:` facade for the same
  kind of call; `macula_content_transfer` calls `macula_station_link` and
  `macula_quic` directly; `macula_upload` IS a `macula_streamer` callback
  module, not just a caller of its public API) — there is no clean
  dependency line to cut without first hardening an internal public
  boundary, which is real, separate work, not a file move. The
  `groups_for_modules` change above gets the actual discoverability
  benefit without that risk.
- No code behavior changes — documentation and `rebar.config` only. Full
  eunit suite unchanged at 1902/0 failed; `rebar3 xref`/`dialyzer` clean
  (same pre-existing warnings as before); `rebar3 ex_doc` exit 0, no new
  warnings, confirmed by building and inspecting the actual generated
  output, not just the exit code, for both the module grouping and every
  reordered guide.
- Every guide reorder was verified content-preserving before commit: every
  original heading still present exactly once, and every original code
  block byte-for-byte present somewhere in the new file (a scripted diff,
  not a visual skim) — matching this project's own "never delete
  features" rule applied to documentation, not just code.

---

## [9.13.3] - 2026-08-21

### Fixed (documentation)

- **README.md's "Latest" banner was 11 minor versions stale** (read
  "9.2.0"; actual 9.13.2 at the time) and didn't mention
  `macula_content_transfer`, `macula_pusher`/`macula_upload`, or any of
  the six PLAN_PUSH_UPLOAD.md phases shipped this session —
  `macula_publisher` wasn't mentioned in the README at all. Rewritten to
  describe the current state: every supervised primitive pair (RPC,
  pub/sub, content sharing, streaming RPC) complete and symmetric, each
  with a pooled and direct-dial mode, plus the new push-initiated
  transfer pair. Dependency-pin examples bumped `~> 9.2` → `~> 9.13`.
- **`STREAMING_GUIDE.md`'s "Push/upload" section had no diagram.** New
  `assets/push_upload.svg`, matching the visual language of the guide's
  other diagrams (`content_streaming.svg`, `content_sharing.svg`,
  `rpc_two_stations.svg`) — depicts the push (blue), the receiver's
  verify-then-reply (green), and the optional direct-dial resolve
  (purple dashed), plus the four-step flow from local manifest
  computation through the terminal reply.
- **`macula_streamer.erl`'s own moduledoc referenced a nonexistent
  `handle_open/3`** (four textual references, plus the module's own
  FIRST example using a three-argument `handle_open/3` clause head that
  would never match the real arity-2 callback) — a leftover from before
  the callback's real shape, never caught because `STREAMING_GUIDE.md`'s
  own example was always correct. Fixed all five, including rewriting
  the broken example to a genuinely arity-2, compilable shape.

### Notes

- No code behavior changes — this release is documentation/assets only,
  found and fixed during a release-readiness audit (see 9.13.2 for the
  one real code bug that audit also found, `macula_response:advertise_direct/7`).
  PATCH, not MINOR.
- Full eunit suite unchanged at 1902/0 failed; `rebar3 ex_doc` confirmed
  the new SVG builds into `doc/assets/push_upload.svg` with no new
  warnings.

---

## [9.13.2] - 2026-08-21

### Fixed

- **`macula_response:advertise_direct/7` now actually forwards `Opts` to
  the underlying advertise call — it silently didn't before.** Found while
  auditing release readiness against the identical bug just fixed in
  `macula_streamer:advertise_direct/7` (9.13.0) — checked every
  `advertise_direct/7` implementation in the SDK for the same pattern and
  found `macula_response` still had it. It called the arity-5 `advertise/5`
  (which always defaults `Opts` to `#{}`) instead of `advertise/6`, so
  `Opts => #{announce => false}` — or `auth`, or ANY override — was
  silently discarded for every direct-dial-advertised RPC procedure, with
  no error anywhere to say so. Fixed at the source, same one-line shape as
  the `macula_streamer` fix (`advertise(Pool, Realm, Procedure, Module,
  Args)` → `advertise(Pool, Realm, Procedure, Module, Args, Opts)`),
  confirmed safe for `macula_direct_dial:publish_advertisement/5` (already
  ignores option keys it doesn't recognize).

### Notes

- No public API changes — same signature, now honors `Opts` correctly.
  PATCH, not MINOR.
- `test/macula_response_tests.erl` gained a regression case
  (`advertise_direct_forwards_opts_to_advertise`), RED-verified against the
  reverted code before being confirmed to pass with the fix.
- Confirmed via `grep` that no other `advertise_direct/7` implementation in
  the SDK has this bug — `macula_streamer` (fixed 9.13.0), `macula_upload`
  (correct from the start, forwards to `macula_streamer:advertise_direct/7`),
  and now `macula_response` are the only three, all correct.

---

## [9.13.1] - 2026-08-21

### Fixed

- **`macula_mri_ets:related_to/2` and `related_from/2` no longer defeat their
  own type specs for dialyzer.** Both built their `ets:match/2` pattern via
  `#rel_entry{...}` record-construction syntax with ETS wildcard/capture
  atoms (`'_'`, `'$1'`) in fields the record declares as `binary()`/`map()`/
  `integer()` — e.g. `object = '$1'` where `object :: binary()`. Dialyzer
  checks a record construction against its own declared field types, finds
  no value can ever satisfy them, and concludes the function can never
  return normally (`success typing is (_,_) -> none()`), which then
  propagates into every caller: `instances_of/1`, `classes_of/1`,
  `subclasses/1`, `superclasses/1`, and transitively `instances_of_transitive/1`
  — 7 warnings from 2 root-cause functions. Fixed by building the pattern as
  a plain tagged tuple in the record's own field order instead of via
  `#rel_entry{...}` — the exact same term at runtime (records ARE tagged
  tuples; this changes nothing about matching behavior, confirmed by the
  existing `macula_mri_ets_tests`/`macula_mri_graph_tests` suites passing
  unmodified), just not run through dialyzer's record-field type check.
  RED-verified: reverting `related_to/2` alone reintroduced its own warning
  plus the two callers that depend solely on it (`classes_of/1`,
  `superclasses/1`), confirming both functions were independently
  necessary, not just one.

---

## [9.13.0] - 2026-08-21

### Added

- **`macula_pusher` / `macula_upload` — push-initiated content transfer.**
  PLAN_PUSH_UPLOAD.md Phase 6, the plan's final phase. `macula_pusher`
  (sender) chunks and hashes bytes with `macula_manifest:create/2`,
  opens a `client_stream` to the recipient's advertised upload
  procedure with the manifest riding the stream's open-time `Args`
  (out-of-band, not an in-band header chunk), sends every chunk in
  order, and blocks for the recipient's own verified terminal reply
  before delivering `{ok, Mcid} | {error, _}` to `handle_pushed/2`.
  `macula_upload` (receiver) advertises the procedure, accumulates
  pushed chunks (built directly on Phase 5's `handle_chunk/2` receive
  loop), and once the sender half-closes, reassembles and verifies
  with `macula_manifest:verify/2` — receiver-side, never
  sender-trusted — before delivering `{ok, Mcid, Bytes} | {error, _}`
  to `handle_uploaded/2`. Both publish `sharing.push_*_v1` /
  `sharing.upload_*_v1` mesh facts. `start_link`/`start_link_direct`
  and `advertise`/`advertise_direct` respectively — see the module
  docs' "correction from the plan's literal wording" sections for two
  places the plan's shorthand description didn't survive tracing the
  actual codebase: no multi-stream parallelism here (that's a
  content-sharing-only mechanism, per the plan's own scope-decision
  section — an earlier draft of the plan said otherwise), and
  `macula_upload`'s shape mirrors `macula_streamer`'s (a long-lived,
  advertised provider), not `macula_download`'s (a one-shot,
  caller-initiated fetch) — the plan named the right API
  (`advertise`/`advertise_direct`) but the wrong module to compare it to.
- **`macula_streamer` gained an optional `handle_eof/1` callback** — a
  `client_stream` provider's one chance to set the stream's terminal
  reply (`macula_stream:set_reply/2` for `{reply, {ok, Value}, State}`,
  `set_error/2` for `{reply, {error, Reason}, State}`) before it stops,
  called in place of the previous unconditional `{stop, normal, State}`
  on eof. A module that doesn't export it keeps the exact prior
  behavior. Needed to let `macula_upload` hand `macula_pusher` a
  verified outcome over `client_stream`'s own terminal-reply channel —
  the callback contract never exposes the raw stream pid to user code,
  so this had to live in the wrapper itself, at the one point
  (`handle_info(stream_eof, State)`) that still has it.

### Fixed

- **`macula_streamer:advertise_direct/7` now actually forwards `Opts`
  to the underlying advertise call — it silently didn't before.** Found
  while building `macula_upload`'s direct-dial path, not something
  either intentionally relied on `mode` being ignored. It called the
  arity-5 `advertise/5` (which always defaults `mode` to
  `server_stream`) instead of `advertise/6`, so `Opts => #{mode =>
  client_stream}` — or ANY `mode`/`announce` override — was silently
  discarded for every direct-dial advertisement, not just this
  session's. A `client_stream` provider that advertised directly would
  have been served as `server_stream` instead, with no error anywhere
  to say so. Fixed at the source, per this project's "fix bugs in
  owned libraries immediately" rule.

### Notes

- No public API changes to already-shipped modules beyond the new
  optional `handle_eof/1` callback (additive) and the
  `advertise_direct/7` bug fix (same signature, now honors `Opts`
  correctly) — MINOR, not MAJOR.
- New test files: `test/macula_pusher_tests.erl` (7 cases),
  `test/macula_upload_tests.erl` (5 cases, including a genuine
  receiver-side-verification-catches-tampering case and the
  too-many-chunks guard), `test/macula_streamer_eof_reply_tests.erl`
  (2 cases). `test/macula_streamer_tests.erl` gained a regression case
  for the `advertise_direct/7` fix.
- A design detail worth recording: `macula_upload`'s `handle_open/2`
  does NOT reject a manifest that fails to decode via
  `{stop, Reason, State}` — traced why that would be wrong: a
  `handle_open/2` stop makes the underlying `macula_streamer:init/1`
  itself return `{stop, Reason}`, a genuine gen_server init failure,
  and OTP never calls `terminate/2` for a process that failed to
  start. That would have silently dropped the push (no
  `handle_uploaded/2`, no `sharing.upload_completed_v1`) and left the
  sender's own `macula:await_reply/1` hanging or crashing, since
  nothing ever reaches `handle_eof/1` to set a reply either. Accepting
  the stream and stashing the decode error instead lets `handle_eof/1`
  — the one place already wired to set a terminal reply — report it
  correctly on both sides once the sender closes, exactly like any
  other failure. Caught by actually running the test before assuming
  the simpler design would work, not by reasoning it through in the
  abstract.

---

## [9.12.0] - 2026-08-21

### Added

- **`macula_streamer` now supports `client_stream` mode: an optional
  `handle_chunk/2` callback drives a linked-reader `recv/2` loop on the
  provider side, mirroring `macula_stream_sink`'s consumer-side callback of
  the same name.** PLAN_PUSH_UPLOAD.md Phase 5. Before this, `macula_streamer`
  only wrapped `send/2,3`/`close/1` — it fit `server_stream` (provider
  pushes) but had no receive path at all for a provider that needs to
  *receive* pushed chunks (a batch upload, `client_stream`'s whole reason
  for existing per `STREAMING_GUIDE.md`). A `server_stream`-mode module that
  doesn't export `handle_chunk/2` is unaffected — the reader is only spawned
  when the callback is present, gated by `erlang:function_exported/3`, the
  same mechanism `macula_stream_sink`'s optional `handle_close/2` already
  uses.

### Fixed

- **`macula_streamer`/`macula_stream_sink` now send the peer a genuine
  `macula_stream:abort/3` STREAM_ERROR on any non-`normal` termination,
  instead of an ordinary close (sink) or nothing at all (streamer).** Same
  bug class as Phase 1's content-transfer cancel fix, applied here: before,
  `macula_stream_sink:terminate/2` called `macula:close_stream/1`
  *unconditionally*, regardless of `Reason` — so a real failure (a `recv`
  error, the reader crashing) looked to the peer exactly like a clean
  end-of-stream, not a cancellation. `macula_streamer:terminate/2` was
  worse: it never closed or aborted its underlying `macula_stream` at all —
  a graceful stop (`Reason = normal`) orphaned that process forever (the
  link only propagates a *non-normal* exit to a non-trapping peer, and
  `macula_stream`'s own auto-stop is tied to its `owner`, an internal
  station-link stub process, not to `macula_streamer`), and an abnormal
  stop killed it via the ordinary link-crash cascade with no explicit
  protocol-level signal ever reaching the far side. Both modules now close
  cleanly (`macula_stream:close/1`) on a `normal` reason and abort
  (`<<"cancelled">>` code, the reason folded into the message) on anything
  else — the peer can now genuinely tell a cancellation/failure from an
  ordinary end-of-stream, for both roles.

### Notes

- No public API changes to `macula_stream_sink` (no new exports). New
  optional `handle_chunk/2` callback on `macula_streamer` — additive, so
  MINOR not MAJOR; bundled with the abort-wiring fix in the same release
  since both were the same phase's deliverable and neither changes any
  existing function signature.
- No pause primitive added for streaming RPC — per the plan's own scope
  decision (see PLAN_PUSH_UPLOAD.md): a QUIC stream is reliable and
  ordered, so a consumer that stops calling `recv/2` already backpressures
  the sender via QUIC's own flow control. Re-litigate only if a concrete
  need surfaces.
- New test file `test/macula_streamer_client_stream_tests.erl` — split out
  from `macula_streamer_tests.erl` because it needs a callback module that
  genuinely exports `handle_chunk/2`; that file's existing callback module
  deliberately does not (exporting it there would spawn a reader for every
  one of ITS tests too, since `function_exported/3` gating is module-wide,
  not per-test-case). Same reasoning Phase 3 used to split
  `macula_content_transfer_multi_stream_tests.erl` out for a similarly
  distinct-mock-shape need.

---

## [9.11.1] - 2026-08-21

### Fixed

- **`macula_feeder`/`macula_download`'s `cancel/1` now reaches the real,
  underlying transfer — previously it orphaned it.** PLAN_PUSH_UPLOAD.md
  Phase 4. Both modules used to run a blocking `macula:put_content/2`/
  `get_content/2` call in a linked worker; `cancel/1` (`gen_server:stop/1`)
  could only kill that local worker, never the `macula_content_transfer`
  it was blocked inside `await/1` on. Nothing links a `gen_server:call`
  caller's death to the callee, so a cancelled transfer kept running to
  completion — or, once resolved, sat alive forever, never reaped, leaking
  its `content_stream_bufs` entry on the link and its
  `macula_content_transfer_registry` entry for no purpose. Both modules now
  call `macula_content_transfer:start_put/3` (or `start_get/3`,
  `start_put_station/5`, `start_get_station/5` for direct-dial) directly
  from a lightweight resolve + await proxy, hold the resulting pid in their
  own state, and `terminate/2` cancels it for real — the same peer-visible
  QUIC RESET_STREAM abort `macula_content_transfer:cancel/1` always gave a
  direct caller, not a local kill with nothing downstream the wiser. The
  `share_id` each module already minted for its own `sharing.*` mesh facts
  is threaded through as `macula_content_transfer`'s own `share_id` too, so
  both layers resolve to the same id.
- Direct-dial (`start_link_direct/5,6` / `start_link_direct/4,5`) gets the
  same real-cancel fix — the resolve step (`macula_direct_dial:
  resolve_station_endpoint/2` / `resolve_content_provider/2`) stays a
  plain blocking DHT lookup exactly as before (nothing has ever needed to
  cancel mid-resolve), but the transfer itself is now addressable the same
  way pooled mode's is.

### Notes

- No public API changes — same `start_link/4,5`, `start_link_direct/5,6`
  (feeder) / `start_link_direct/4,5` (download), `cancel/1`, same
  `init/1`/`handle_fed/2`/`handle_downloaded/2` callback contract. PATCH,
  not MINOR: this fixes existing behavior rather than adding capability.
- `macula_feeder_tests.erl`/`macula_download_tests.erl` now mock at the
  `macula_client`/`macula_station_link` boundary (the same layer
  `macula_content_transfer_tests` mocks) instead of `macula:put_content/2`/
  `get_content/2` directly — a mechanical necessity, not a design choice:
  the internals no longer call those functions at all, so the old mocks
  would simply never fire. `Pool` is now a real pid in these tests (it's
  threaded down to `macula_content_transfer:start_put/3`'s own `is_pid`
  guard), not the placeholder atom `pool` the pre-Phase-4 suite used.
  Two new cases per module cover what Phase 4 actually fixes (asserting
  `abort_content_stream` is genuinely called on cancel, not just that the
  feeder/download reports `outcome => cancelled`) and direct-dial
  (previously untested).

## [9.11.0] - 2026-08-21

### Added

- **Multi-stream parallel chunk transfer for chunked content.**
  PLAN_PUSH_UPLOAD.md Phase 3. Chunks are distributed round-robin
  (`Index rem StreamCount`) across up to `stream_count` dedicated
  content streams on the same link (`Opts`'s `stream_count` key,
  default 4, always capped at the actual chunk count) — each stream
  runs its own independent chunk-by-chunk loop concurrently, all driven
  by the ONE `macula_content_transfer` gen_server via
  `handle_continue/2` (never by the streams' own one-call-and-report
  worker processes). The manifest is put (or, for a get, its chunks
  reassembled — by chunk INDEX, not arrival order, since different
  streams finish in whatever order their own network calls happen to
  complete in — and verified) only once every stream has drained its
  own share. A get doesn't know the chunk count, and therefore how many
  streams are worth opening, until its manifest is fetched, so it
  starts on the one stream the connect step already opened and expands
  once the count is known; a put knows upfront and opens every extra
  stream immediately. Opening an extra stream is best-effort — a
  failure degrades to fewer streams rather than failing the transfer
  (a single-stream transfer is still correct, just slower). A single
  stream's own chunk genuinely failing (an `{error, _}`, not a crash)
  fails the whole transfer exactly as a sequential one would: every
  other stream's in-flight work is killed and every stream reset before
  `await/1,2` sees the error.
- `pause/1`/`resume/1` (9.10.0) now gate every open stream uniformly,
  not just one — the same `paused` check, in the same place, whether
  there's one lane or several.
- `cancel/1,3` (9.9.0) now resets every currently-open content stream
  on cancellation, not just one.

### Changed

- `macula_content_transfer`'s single-stream chunk-loop internals
  (9.10.0's `dispatch_next_step`/`step_result`/etc., single `#chunk`
  fields `remaining`/`next_index`/`acc`) are replaced by a per-stream
  `#lane{}` model — each stream owns its own remaining-work queue,
  in-flight item, and worker. Single-block put/get is untouched — still
  one worker, connect through completion, exactly as 9.9.0 shipped it;
  there is no "another stream" for a one-round-trip transfer to use.

## [9.10.0] - 2026-08-21

### Added

- **`macula_content_transfer:pause/1`/`resume/1`** — real pause/resume for
  chunked put/get. PLAN_PUSH_UPLOAD.md Phase 2. The per-chunk step loop
  (put a chunk / get a chunk / put or get the manifest) now runs as a
  `handle_continue/2` step the gen_server re-triggers itself between
  chunks, checking a `paused` flag each time — `pause/1` stops the loop
  from advancing to the next chunk (the chunk already in flight, if any,
  still completes uninterrupted — its own round trip stays one blocking
  call, matching content's existing "a chunk is verified whole or not at
  all" model); `resume/1` re-arms it from exactly the next un-sent/
  un-fetched chunk, never from the start. Single-block content has no
  "between chunks" to pause at, so `pause/1` there is a harmless no-op —
  the transfer just runs to completion regardless.
- Each chunk step (one `_content.put_block`/`get_block`, or the
  manifest's `_content.put_manifest`/`get_manifest`) now runs in its own
  short-lived linked worker rather than inside one long recursive loop,
  so `cancel/1,3` can always kill whichever step is currently in flight —
  same guarantee Phase 1 gave the whole transfer, now granular to each
  chunk. Closed a real gap this uncovered: cancelling while genuinely
  paused between chunks means no step worker is alive at all
  (`worker = undefined`), which the Phase 1 cancel path didn't handle —
  fixed (`kill_worker/1` now treats `undefined` as nothing to kill,
  verified RED before GREEN: reverting the fix reproduces the exact
  `{badarg, [{erlang,unlink,[undefined]...` crash it prevents).

### Changed

- `macula_content_transfer`'s internal chunk-loop functions
  (`put_chunks`/`chunk_put_result`/`put_manifest`/`get_chunks`/
  `chunk_get_result`/etc., moved verbatim from `macula.erl` in 9.9.0)
  are replaced by the step-driven design above. Single-block put/get is
  untouched — still one worker, connect through completion, exactly as
  9.9.0 shipped it; there is no "between chunks" for it to participate in.

## [9.9.0] - 2026-08-21

### Added

- **`macula_content_transfer`** — addressable content-store put/get
  with a real, peer-visible cancel. PLAN_PUSH_UPLOAD.md Phase 1.
  `macula:put_content/2`/`get_content/2` (and the `_station` variants)
  were one opaque blocking call each: pick a link, open a dedicated
  content stream, run the transfer, close it — no handle existed
  mid-transfer, so cancelling meant killing whatever process was
  blocked in the call, which never touched the stream itself
  (`macula_station_link` owns it, not the killed caller) — leaking
  `content_stream_bufs`/`content_pending` state on the link until the
  next `content_call_timeout` fired against an already-dead caller.
  `start_put/2,3`, `start_put_station/4,5`, `start_get/2,3`,
  `start_get_station/4,5` return `{ok, Pid}` immediately; `await/1,2`
  blocks for the outcome; `cancel/1,3` tears the transfer down from
  any point in its lifecycle, resetting the open stream if one exists.
  `put_content/2`/`get_content/2` (+ `_station` variants) are now thin
  blocking wrappers over this — same public signature, no caller
  changes needed (verified: no direct callers in macula-station,
  macula-realm, or hecate-om).
- **`macula_quic:reset_stream/2`** — a genuine QUIC RESET_STREAM abort,
  new Rust NIF (`nif_reset_stream`, Quinn's `SendStream::reset`).
  Content-transfer's `cancel/3` needed a real, peer-visible signal —
  `macula_stream:abort/3` (streaming RPC's abort) doesn't apply here,
  it targets a `macula_stream` gen_server's own STREAM_ERROR framing,
  and a content-transfer stream is a raw QUIC dedicated stream with no
  such process. The peer's `RecvStream::read` now distinguishes a
  reset from every other read failure: `{quic, stream_closed,
  PeerStream, {reset, ErrorCode}}` instead of the same undifferentiated
  `none` reason every read error used to collapse into. Along the way,
  fixed a real pre-existing stub: `async_shutdown_stream/3` has taken
  `(Stream, Flag, Code)` since this module's msquic-era design but
  silently discarded `Code` and always did a graceful `close_stream/1`
  — it now genuinely resets with `Code` (zero callers anywhere in
  macula-station/macula-realm/hecate-om, confirmed before changing its
  behavior).
- **`macula_station_link:abort_content_stream/4`** — the real-abort
  counterpart to `close_content_stream/2`, used by
  `macula_content_transfer:cancel/3`.
- **`macula_content_transfer_registry`** — correlation-id → pid lookup
  for content transfers (ETS-backed, monitor-based cleanup), so a
  caller that only knows a transfer's `share_id` (from a published
  `sharing.*_started_v1` mesh fact) can still resolve it to `cancel/1,3`.

## [9.8.2] - 2026-08-21

### Fixed

- **`macula_client:connect/2` with no `identity` opt now defaults to a
  puzzle-hardened identity, not a plain one** (`macula_client.erl`,
  `init/1` + new `resolve_identity/1`). Stations may run
  `puzzle_enforcement: enforce` (S/Kademlia identity puzzle, rejects
  any peer whose `SHA-256(pubkey)` lacks the configured leading-zero
  bits). A caller who didn't pass `identity` used to get
  `macula_identity:generate()` — no puzzle grind — which fails that
  check. The failure is silent by construction: the QUIC/TLS transport
  still reports the link healthy, and `subscribe/5` still returns
  `{ok, _}` locally, because both succeed before the station's
  handshake rejection closes the connection. Confirmed live: a
  production consumer (`macula-realm`'s `MaculaRealm.Mesh`, connecting
  with `%{}` opts) sat fully connected-looking with zero events
  delivered on any subscription, across all 5 of its station links,
  for over an hour, before the identity itself turned out to be the
  cause. `resolve_identity/1` only changes the pool's own default
  identity resolution; it does not touch `macula_identity:generate/0`
  itself (still documented as "does not grind a puzzle" — callers who
  want a plain identity can still ask for one directly), and it's
  lazy where the old `maps:get/3` call was not (that evaluated its
  default argument, and therefore generated a throwaway keypair, on
  every single `connect/2` call regardless of whether the caller
  passed an identity).

## [9.8.1] - 2026-08-21

### Fixed

- **Peer-initiated dedicated stream: notify before enabling active
  delivery, not after** (`macula_peering_conn.erl`, `connected/3`'s
  `{quic, new_stream, ...}` clause). A new stream resource is created
  passive (`StreamResource::active: AtomicBool::new(false)` in the
  Rust NIF; its recv loop blocks on a `Notify` until `setopt(active,
  true)` wakes it), so nothing can be delivered to `controlling_pid`
  before that NIF call runs — except the old code called `setopt`
  *before* sending the `{macula_peering, new_dedicated_stream, ...}`
  notification. On a fast/near-zero-RTT path the peer's first frame
  (sent the instant it finishes opening the stream) could then reach
  `controlling_pid`'s mailbox before the notification did, and every
  dedicated-stream consumer keys its buffer off that notification
  (`stream_bufs` / `content_stream_bufs`), so the data landed nowhere
  and was silently dropped by whichever catch-all the consumer had.
  Reordering the two calls closes the race structurally: passive mode
  guarantees zero delivery until `setopt` runs, and by then the
  notification is already in the mailbox. Found via macula-station's
  cross-station streaming-RPC relay (a station relaying a STREAM_OPEN
  onto the next hop by opening a fresh dedicated stream), where it
  surfaced as a deterministic-then-intermittent timeout depending on
  which side of the relay owned the connection; local reproduction
  went from 100% failure (pre-existing bug this uncovered, see below)
  to 0% deterministic / ~11% intermittent after this fix alone. A
  second, independent bug on the macula-station side (outbound_link
  had no handling for the notification at all) accounted for the rest
  of the original 100% failure rate and is fixed separately, in
  macula-station.

## [9.8.0] - 2026-08-20

### Added

- **Direct-dial for streaming RPC** (`macula_stream_sink:start_link_direct/5,6`,
  `macula_streamer:advertise_direct/6,7`,
  `macula_direct_dial:call_stream/5,6`). The streaming counterpart to
  RPC direct-dial — and it turns out to need almost no new machinery:
  a `procedure_advertisement' does not distinguish RPC from streaming,
  only the eventual dial (`call_station/7' vs `call_stream_station/6')
  does, so `publish_advertisement/4,5' and `resolve_dial_url/4' are
  reused as-is by both. `macula_streamer:advertise_direct/6,7' is
  `advertise/5,6' plus the identical publish step
  `macula_response:advertise_direct/6,7' already does for plain RPC.

### Fixed

- **`macula_client:call_stream_station/6` now threads the per-call TLS
  trust override** (`verify`/`expected_node_id`/`pin_tls_cert`) through
  to the underlying dial, same as `call_station/8` — it predated that
  work and had no way to specify per-call trust at all, so a fresh dial
  from it always fell back to the pool's connect-time defaults. Against
  production this had the same TLS-pin problem the RPC family had
  before `pin_tls_cert` existed; now it can actually be direct-dialed.
  Removed `macula_client:ensure_link/2`, left unused once its only
  remaining caller moved to the 3-arity form that carries `LinkOpts`.

## [9.7.0] - 2026-08-20

### Added

- **Direct-dial for content upload** (`macula_feeder:start_link_direct/5,6`,
  `macula_direct_dial:put_content/4`, `resolve_station_endpoint/2`,
  `macula:put_content_station/4,5`). The PUT-side counterpart to
  download direct-dial. Unlike a GET, a PUT has no discovery step — the
  caller already knows (or is choosing) which station to seed, so it
  names `Station' directly rather than resolving one from an
  announcement. Reuses the exact `station_endpoint' resolve machinery
  RPC direct-dial already built for `serving_station' (same signer
  check, same stale-record retry), just exposed as a standalone public
  `resolve_station_endpoint/2'.
- **`macula:put_content_station/4,5`** — the content-transfer
  counterpart to `get_content_station/4,5`, symmetric in shape.

## [9.6.0] - 2026-08-20

### Added

- **Direct-dial for content download** (`macula_download:start_link_direct/4,5`,
  `macula_direct_dial:get_content/3`, `macula:get_content_station/4,5`,
  `macula_client:ensure_content_link/4`). Resolves a chunked MCID's
  provider from its signed `content_announcement` (published
  automatically by the provider's station on receipt — nothing new to
  advertise, no `macula_feeder`-side change needed) and dials that
  station directly for the fetch, instead of depending on the caller's
  own station being able to reach it via relay. Content's trust model is
  deliberately lighter than RPC's direct-dial: content is
  content-addressed and independently re-hashed client-side regardless
  of which peer serves it, so there is no cert-chain-equivalent opt here
  — see `macula_direct_dial`'s module doc, "Content" section, for why.
- **`macula:get_content_station/4,5`** — the content-transfer
  counterpart to `call_station/6,7`: dial a specific, already-resolved
  station directly for a `put_content`/`get_content`-shaped dedicated-
  stream transfer, with the same per-call `verify`/`expected_node_id`/
  `pin_tls_cert` trust override.

### Fixed

- **`find_content_providers/2` now checks the announcement's signer
  against its own claimed `announcer_node`**, not just that SOME valid
  signature is present. The public `content_announcement/3,4`
  constructor always keeps the two consistent, so this could only
  diverge via a hand-crafted record — exactly the malicious/non-SDK
  publisher case the check exists for. Same class of fix as
  `macula_direct_dial:verify_and_build/2` already applies to
  `station_endpoint`.
- **Single-block `get_content/2` now verifies the fetched bytes'
  BLAKE3 hash against the MCID client-side.** Chunked content already
  got this via `macula_manifest:verify/2` over the reassembled whole;
  single-block content had no client-side check at all, relying
  entirely on whichever station served the request having verified it
  once, at PUT time — not necessarily the station being fetched FROM,
  especially once `get_content_station/5` lets a caller deliberately
  dial a resolved, third-party provider.

## [9.5.0] - 2026-08-20

### Added

- **Direct-dial for RPC** (`macula_direct_dial`, `macula_request:start_link_direct/6,7,8`,
  `macula_response:advertise_direct/6,7`, `macula_client:call_station/8`,
  `macula:call_station/7`). An alternative to the gossip-routed
  `macula:call/5` path: the caller resolves a procedure's
  `procedure_advertisement` from the DHT, resolves and verifies that
  advertisement's `serving_station` to a dialable `station_endpoint`
  record, and dials it directly over one QUIC hop instead of depending on
  advertise-gossip having propagated a route between arbitrary stations.
- **`pin_tls_cert` connect/link opt** (`macula_peering_conn`,
  `macula_station_link`, `macula_client:call_station/8`,
  `macula:call_station/7`). Decouples `expected_node_id`'s two
  enforcement points, previously fused: pinning the QUIC/TLS
  certificate's own SPKI (`pin_tls_cert => true`, the default —
  correct only when the peer's TLS cert genuinely IS its macula
  identity, e.g. self-signed test clusters) versus the application-
  layer CONNECT/HELLO signature check (`bind_peer_identity/2`, always
  enforced when `expected_node_id` is set, regardless of
  `pin_tls_cert`). `pin_tls_cert => false` is required to direct-dial
  any station whose TLS is terminated by a PKI unrelated to its macula
  identity — a production station behind Let's Encrypt, for instance,
  where the cert's key has no relationship to the station's Ed25519
  identity and the pin can never succeed. Trust for such a dial rests
  entirely on the signed HELLO handshake instead, checked against the
  same pubkey the DHT chain resolved. Direct-dial (`macula_direct_dial`)
  always dials this way.
- **Mandatory advertisement signature verification + opt-in cert-chain
  check for direct-dial** (`macula_direct_dial`). Resolving a
  `procedure_advertisement` now discards any candidate record that
  fails Ed25519 signature verification before trusting its
  `serving_station` at all — previously the first DHT record found was
  trusted unconditionally, so any identity able to sign SOME record
  could point a caller at a real, legitimate station it had no
  authority to name. `call/6` and `publish_advertisement/5` additionally
  accept an opt-in `verify_cert_chain => {RealmCaPem, Org}` /
  `cert_chain => ChainPem` pair (managed realms only) that requires the
  advertisement's embedded X.509 service-cert chain to verify to the
  realm CA under the given org (Slice 7c Direction B, via the existing
  `macula_record:verify_advertisement_cert_chain/3`), proving the
  advertiser itself — not just the station it names — is an
  org/realm-authorized identity.

## [9.4.0] - 2026-08-20

### Added

- **`macula_publisher`** — the missing supervised behaviour for pubsub
  publishers. Every other primitive pair already had a supervised behaviour on
  both sides (`macula_request`/`macula_response` for RPC, `macula_streamer`/
  `macula_stream_sink` for streaming RPC, `macula_feeder`/`macula_download`
  for content sharing); pubsub had only the consumer side (`macula_subscriber`).
  `macula:publish/4` remains a plain blocking call with no addressable pid to
  cancel or observe from outside. `macula_publisher:start_link/5,6` returns
  immediately with a pid, runs the publish in a linked worker, delivers the
  outcome to `Module:handle_published/2`, and publishes
  `pubsub.publish_started_v1` / `pubsub.publish_completed_v1` mesh facts
  around the transfer — mirroring `macula_feeder`'s shape exactly.

## [9.3.1] - 2026-08-20

### Fixed

- `STREAMING_GUIDE.md`'s dedicated-stream claim (accurate since 9.3.0) was worded
  ambiguously enough to still read as the old shared-control-stream behavior it was
  meant to have moved past. States explicitly now that each streaming session gets
  its own QUIC stream, not multiplexed onto the connection's shared stream the way
  an ordinary CALL or PUBLISH is. Docs-only; no code changes.

## [9.3.0] - 2026-08-20

**Streaming RPC and content transfer now genuinely ride their own dedicated QUIC
streams, closing a real gap between the docs and the implementation.** Prior
releases' `STREAMING_GUIDE.md` claimed RPC, PubSub, streaming, and content each
used "independent multiplexed QUIC streams" with "per-stream flow control" — that
was false. `macula_peering_conn.erl` opened exactly one QUIC stream per peering
connection, and every frame type (CALL, RESULT, PUBLISH, STREAM_OPEN, content
put/get blocks, ...) was multiplexed onto it via application-level IDs. QUIC's
actual per-stream isolation was unused. This release wires the two workloads
where it matters most — streaming and content — onto real, separate QUIC streams.
See `plans/PLAN_PER_STREAM_QUIC_ISOLATION.md` for the full design record.

### Added

- **Streaming RPC dedicated streams.** `advertise_stream`/`call_stream` sessions
  each get their own QUIC stream, opened via new `macula_peering:
  open_dedicated_stream/1` and `send_on_stream/3` primitives. STREAM_OPEN/DATA/
  END/ERROR/REPLY no longer travel the shared control stream.
- **Content transfer dedicated streams.** `put_content`/`get_content` (both
  single-block and chunked) now pin one healthy pool link
  (`macula_client:pick_connected_link/1`, new) and run the whole transfer's
  block + manifest calls over one dedicated stream
  (`macula_station_link:open_content_stream/1` / `call_on_stream/6` /
  `close_content_stream/2`, new), instead of letting the pool re-pick a link
  per underlying CALL. A large blob transfer no longer head-of-line-blocks
  other RPC/PubSub traffic on the same connection.
- `CONTENT_GUIDE.md` documents the new per-transfer stream isolation.

### Fixed

- **`macula_peering_conn.erl`: the client role never started the QUIC bidi-
  stream accept loop** (`macula_quic:async_accept_stream/1`), only the server
  role did. Harmless in the pre-dedicated-stream world (a client only ever
  opened the one stream it used itself), but meant any client-role connection
  — a daemon dialing a station, or a station dialing another station — could
  open a dedicated stream outward but could never receive one opened *at* it.
  Found via live testing (`macula_station_call_stream_station_SUITE` in
  `macula-station`), not by inspection: the failure mode was total silence,
  since nothing errors when a peer simply never calls `accept_bi()`.
- **`macula_station_link.erl`: `fail_all_pending/2` still pattern-matched the
  pre-dedicated-stream 2-tuple shape** (`{Pid, Mon}`) for `client_streams` /
  `server_streams` entries, which had already grown a third element (the
  dedicated stream reference). Every disconnect would have crashed the link's
  gen_server instead of cleanly aborting open streams.

## [9.2.0] - 2026-08-20

**A supervised, fact-announcing primitive family sits on top of the four raw
mesh operations.** Every one of `advertise/5`, `call/5`, `advertise_stream/5`
/ `call_stream/5`, `put_content/2` / `get_content/2`, and `subscribe/5`
already spawns or blocks a bare process with no addressable pid — nothing to
cancel, nothing to supervise, nothing to observe from outside. This release
adds four symmetric provider/consumer pairs, each a proper `gen_server`
behaviour with a `simple_one_for_one` factory supervisor, publishing mesh
protocol facts around its own side of the operation:

### Added

- **`macula_feeder` / `macula_download`** — supervised wrappers around
  `put_content/2` / `get_content/2`. Publish `sharing.put_started_v1` /
  `sharing.put_completed_v1` and `sharing.get_started_v1` /
  `sharing.get_completed_v1`, carrying `chunked => true | false`. Replaces
  the unreleased, unpublished `macula_content_sharing` (deleted — nothing
  depended on it outside this repo).
- **`macula_streamer` / `macula_stream_sink`** — supervised wrappers around
  `advertise_stream/5` / `call_stream/5`. Publish `streaming.started_v1` /
  `streaming.completed_v1` from each side independently. `macula_streamer`
  is push-based: `Module:handle_open/2` registers `self()` however the
  application discovers it, then any process holding that pid drives the
  stream via `macula_streamer:send/2,3` / `close/1`.
- **`macula_response` / `macula_request`** — supervised wrappers around
  `advertise/5` / `call/5`. Publish `rpc.received_v1` / `rpc.replied_v1`
  (provider) and `rpc.sent_v1` / `rpc.completed_v1` (consumer, including
  `outcome => cancelled` when cancelled before a reply arrives).
- **`macula_subscriber`** — supervised wrapper around `subscribe/5`,
  threading `macula_event` / `macula_event_gone` dispatch into
  `Module:handle_event/4`.
- Every pair has a `_sup` factory (`macula_feeder_sup`,
  `macula_download_sup`, `macula_streamer_sup`, `macula_response_sup`,
  `macula_request_sup`): provider-side ones are started internally by
  `advertise/5,6` and hidden from the caller; consumer-side ones are meant
  to be embedded in the *caller's* own supervision tree, so a `cancel_*`
  command becomes `supervisor:terminate_child/2` (or `cancel/1` on the
  child pid directly) against a child the application already owns.
- `RPC_GUIDE.md`, `STREAMING_GUIDE.md`, `CONTENT_GUIDE.md`, and
  `PUBSUB_GUIDE.md` each gained a section introducing their pair, plus a
  `Reference` / `See also` row.

### Fixed

- `mcid/0` was used in three exported `-spec`s (`put_content/2`,
  `get_content/2`, `find_content_providers/2`) but never in
  `-export_type` — any external consumer's dialyzer run saw an unknown
  type. Now exported.
- `macula.app.src`'s `links` entry was labeled `"GitHub"` but pointed at
  `codeberg.org` — a leftover from the pre-2026-07-26 hosting arrangement.
  Now points at `github.com/macula-io/macula`.

No breaking changes — every addition is a new module; nothing existing
changed shape.

---

## [9.1.1] - 2026-08-20

**Every guide and the README, checked line-by-line against real source — not
a proofread, an audit.** 9.1.0 already fixed one stale README hero diagram;
publishing it surfaced a live `~> 8.8` dependency pin in the README's own
install snippet, which turned into a full sweep of every published guide.
The scope kept growing because the method kept finding real bugs: for every
function call, grep the real `-export()` list and arity; for every "the SDK
does X automatically" claim, grep for the actual call site rather than
trust the prose; for every return-shape example, read the function body,
not just its `-spec` (several were generic `term()` hiding a more specific
real shape). No runtime behavior changed anywhere in this release — every
fix is documentation catching up to code that was already correct.

### Fixed — fabricated or superseded content removed

- **`RPC_GUIDE.md` fully rewritten.** Described a nonexistent legacy API
  (`advertise/3` returning `{ok, Ref}`, `unadvertise/2`, `call/3`) and a
  "Call Flow" matching WAMP/Bondy-era relay-routed RPC with `gproc` local
  lookup — contradicting the guide's own callout that direct-dial is
  current. Rewritten against the real facade, including the handler-return
  contract traced from `macula_station_link:safe_invoke_handler/4`'s actual
  body and a verified `call_station/6` resolve-then-dial recipe.
- **`AUTHORIZATION_GUIDE.md`** invented four entire modules with zero code
  backing them (`macula_did_cache`, `macula_authorization`,
  `macula_ucan_revocation`, `macula_authorization_audit`) — DID-namespace
  gating, automatic `.public.` topic gating, UCAN revocation with a
  fabricated rate limit, audit logging with a fabricated LRU policy. None
  of it is real; the SDK's only enforced authorization is `advertise/5`'s
  per-procedure `{ucan_required, Issuer}`. Rewritten to only what's real.
  A second independent pass caught two more return-shape bugs: `encode/1`
  and `to_map/1` are bare `binary()`/`map()`, not `{ok, _}`-wrapped.
- **`PROTOCOL_GATEKEEPER_GUIDE.md` deleted.** Its entire premise —
  a `macula_protocol` behaviour, a `macula_gatekeeper` validator, a
  Portal/Console certificate hierarchy — belongs to the old, archived
  `macula-console`/`macula-portal` product, not this SDK.
- **`PUBSUB_GUIDE.md`** described the pre-8.8.0 ordering bug as current
  behavior ("no per-publisher order... dozens of inverted pairs"),
  contradicting the correct `ordered`-by-default content earlier in the
  same file. Also claimed a dead link sends `{macula_event_gone, SubRef,
  {disconnected, _}}`; the real behavior is a silent respawn + resubscribe,
  never an event.
- **`DIST_OVER_MESH_GUIDE.md`**: `join_mesh/1`'s options table claimed
  `realm` and `tls_verify` options that don't exist (only `relays` and
  `identity` are real); `DIST_TIMEOUT` was documented as 25000ms, the real
  value is 10000ms.
- **`CLUSTERING_GUIDE.md`**: the `mdns` strategy was documented as doing
  real mDNS/Bonjour discovery; it's accepted and logged but never branched
  on — `mdns` and `dht` currently do the identical DHT-based thing. An
  "Integration with bc_gitops" section referenced an unrelated external
  project, same contamination pattern found in `AUTHORIZATION_GUIDE.md`.
- **`CONNECTING_GUIDE.md`**, **`TOPIC_NAMING_GUIDE.md`**, **`DEVELOPMENT.md`**,
  **`docs/GLOSSARY.md`**, and **`docs/README.md`** all referenced a phantom
  `macula_mesh_client` module as current behavior — it's real, but v2.1.0-era
  history, superseded by the V2 pool (`macula_client`) well before 3.11.0.
  `TOPIC_NAMING_GUIDE.md` also falsely claimed topic validation is enforced
  automatically at publish/subscribe (it isn't — `macula_topic:validate/1`
  is never called on the SDK's send path) and had a fabricated "Wildcard
  Subscriptions" section with no supporting code anywhere.
- **Ten SVGs deleted**, each depicting one of the above rather than being
  merely unreferenced: `mesh-architecture.svg`, `pubsub_flow.svg`,
  `rpc_flow.svg`, `revocation_flow.svg`, `audit_system.svg`,
  `lru_eviction.svg`, `namespace_hierarchy.svg`,
  `gatekeeper_security_model.svg`, `protocol_callbacks.svg`,
  `gatekeeper_flow.svg`, `cluster_integration.svg`, `relay_failover.svg`.
  Three more still-embedded SVGs (`connect_flow.svg`, `dist_over_mesh.svg`,
  `mri-architecture.svg`) had the same phantom module or a wrong function
  name and were fixed in place rather than removed.
- **README.md**: the `rebar.config`/`mix.exs` install snippets pinned
  `{macula, "~> 8.8"}` — the actual code a new user copies, resolving to
  the 8.8.x line, not this package's current version. Also trimmed ~150
  lines that duplicated guides verbatim (the Four Interaction Patterns
  walkthroughs, Distribution, Clustering, MRI sections) down to the
  capability list + guide table that already did this job — the
  duplication is exactly why the stale hero diagram and the dependency
  pin went unnoticed for as long as they did.

### Added

- `docs/guides/RECORDS_GUIDE.md` — the raw DHT record API
  (`put_record/2`, `find_records_by_type/2`, `macula_record:envelope/4`
  for a custom record type) was exported, public API with no guide.

### Clarified

- Seed URL scheme (`quic://` vs `https://`) is a label, not a switch —
  `macula_station_link:parse_seed/1` dials over QUIC regardless of the
  scheme text, and both are genuinely seen in production. Documented
  instead of silently picking one.

---

## [9.1.0] - 2026-08-20

**OTP 29 readiness, plus a records guide and a stale-docs cleanup.**

### Added

- **`docs/guides/RECORDS_GUIDE.md`.** The raw DHT record API
  (`put_record/2`, `find_record/2`, `find_records/2`,
  `find_records_by_type/2`, `subscribe_records/3`,
  `unsubscribe_records/2`, and `macula_record:envelope/4` for defining
  your own record type in the `0x20`-`0xFF` tag range) was public,
  exported API with no guide — one line in a CONTENT_GUIDE comparison
  table was the only mention. Documents storage-key derivation
  (including `subject_id`) and the built-in-vs-domain-defined type split.
- `MRI_GUIDE.md` now embeds `mri_trie_index.svg` (previously README-only)
  next to the `build_index/1` / `index_children/3` example it illustrates.

### Changed

- **`assets/sdk_architecture.svg` regenerated.** The README's hero
  diagram still said "Macula SDK v1.0.0 — 48 Modules" and named modules
  that no longer exist (`macula_mesh_client`, `macula_multi_relay`,
  `macula_local_client`) — current is 9.x with 89 modules across a
  vertical-sliced tree that's been reorganized several times since that
  image was drawn. A module-inventory diagram tied to internal layout
  drifts every time a slice gets renamed or moved, which is often by
  design in this codebase. Replaced with a capability map keyed to the
  public facade and the guide table instead (PubSub, RPC, Content,
  Records, Streaming, Distribution over Mesh, Clustering, Authorization,
  MRI, over an Identity/Crypto + Wire Protocol substrate) — no module
  count, no version number baked into the image, so it can't go stale
  the same way. Restyled to match the light theme already used by the
  four interaction diagrams instead of its previous, inconsistent dark
  theme.

### Fixed

- **132 bare `catch Expr` sites rewritten to `try Expr catch _:_ -> ok
  end`** across 41 files. OTP 29 deprecates the bare form; combined with
  this project's `warnings_as_errors`, it was a hard compile failure.
  6 sites relied on `catch`'s special `{'EXIT', Reason}` return value
  being pattern-matched by the caller (hex/port/binary decoding in
  `macula_uri`, `macula_cert`, `macula_trust_store`, `macula_dist`,
  `macula_dist_relay_client`, `macula_cluster_gossip`) and were rewritten
  to preserve that exact shape rather than collapsing to `ok`.
- **`macula_record`'s `record()` type renamed to `m_record()`**
  (`macula:m_record()` in the facade). OTP 29 made `record()` a reserved
  built-in type name; a module declaring `-type record() :: ...` now
  fails to compile with "local redefinition of built-in type". Updated
  every consuming `-spec` across `macula_foundation`, `macula_frame`,
  `macula_advertise_station`, `macula_resolve_address`,
  `macula_host_identity`, and `macula_station_link`.
- `macula_manifest.erl`'s moduledoc referenced `macula_manifest:get_chunk_mcid/2`
  and `macula_manifest:decode/1`, neither of which exists (the real
  names are `chunk_mcid/3` and `from_wire/1`) — caught by `rebar3 ex_doc`
  while auditing docs for this release.
- Deleted three orphaned SVGs that no guide or the README ever
  referenced: `mesh-architecture.svg`, `pubsub_flow.svg`, `rpc_flow.svg`.
  Not just unused — actively wrong for the current architecture:
  `rpc_flow.svg` claimed "nodes never connect directly, all traffic
  flows through the relay mesh" (contradicted by the direct-dial
  `call_station/6` path this guide's own two-station diagram documents),
  and `pubsub_flow.svg` named three modules that don't exist
  (`macula_pubsub_handler`, `macula_gateway_pubsub`, `macula_pubsub_dht`)
  and cited unmeasured metrics ("Cache hit: ~98%").

None of this changes runtime behavior — the catch rewrite is a pure
syntax swap, `m_record()` is a type-only rename, and everything else is
documentation. OTP is still pinned to 28 in `.tool-versions`; this
clears the specific compile blocker for a future OTP 29 move but does
not make that move itself — a separate `record()`-built-in-type
collision in `-type record() :: #{...}` (now fixed) was the other half
of that blocker, also cleared here.

---

## [9.0.1] - 2026-08-20

### Fixed

- PUBSUB_GUIDE.md was missing its `pubsub_two_stations.svg` diagram — RPC,
  Content, and Streaming guides all embed their matching interaction-pattern
  diagram at the top; PubSub's was left out when those were added. Docs-only,
  no code change.

---

## [9.0.0] - 2026-08-20

**LAN clustering and distribution-over-mesh split into separate concerns.**
They were tangled together in one supervision tree and one source directory,
despite not depending on each other — one is same-subnet gossip/mDNS cluster
formation, the other is `net_adm:ping/1` across firewalls over the mesh. Now:
`macula_cluster_system/` (gossip, static, libcluster strategy) and
`macula_dist_system/` (the three dist-over-mesh transports: direct QUIC,
pool-tunneled via `join_mesh/1`, and the dedicated freight relay via
`join_dist_relay/1`) are independent. See `src/macula_cluster_system/README.md`
and `src/macula_dist_system/README.md`.

### Breaking

- **`macula_dist_relay` renamed to `macula_dist_pool`.** The DIST_OVER_MESH_GUIDE
  told readers to run `macula_dist_relay:get_tunnel_metrics()` directly for
  troubleshooting — that call now needs `macula_dist_pool:get_tunnel_metrics()`.
  `macula:join_mesh/1` and `macula:join_dist_relay/1` (the facade, what almost
  everyone should be calling) are unaffected — this only breaks code that
  called the renamed module directly.
- **The `auto_cluster` sys.config option is removed**, along with the
  `macula_dist_system` supervisor code that read it and conditionally started
  `macula_cluster_strategy` as one of its children. This was a **silent**
  behavior change before this release note: a consumer with `auto_cluster =>
  true` (or `application:set_env(macula, auto_cluster, true)`) simply stopped
  getting automatic LAN clustering, no crash, no warning. Start clustering
  explicitly instead: `macula_cluster:start_cluster/1` (see the
  [Clustering Guide](docs/guides/CLUSTERING_GUIDE.md)).
- `macula_cluster`, `macula_cluster_gossip`, `macula_cluster_static`,
  `macula_cluster_strategy` moved from `src/macula_dist_system/` to
  `src/macula_cluster_system/` (and their tests from `test/macula_dist_system/`
  to `test/macula_cluster_system/`). Module names are unchanged — this only
  affects anyone with a build script or `rebar3 eunit --dir=` command
  hard-coded to the old path.

---

## [8.10.0] - 2026-08-19

### Added

- **Chunked content sharing**, closing the "single block only" gap in
  `put_content/2` / `get_content/2` (unchanged since v4.2.7 for blobs that fit
  in one 256 KiB block — same MCID, same wire calls, fully backward compatible).
  Larger blobs now split client-side into fixed-size chunks
  (`macula_manifest:create/1`, a byte-for-byte port of macula-station's own
  chunking/Merkle/MCID algorithm — same BLAKE3 NIF, same deterministic CBOR
  encoder, so the two sides agree without either side changing), upload each
  chunk, then a `content_manifest` via the station's existing (unmodified)
  `_content.put_manifest` / `_content.get_manifest`; `get_content/2` fetches
  every chunk, reassembles, and Merkle-verifies against the manifest before
  returning.
- `macula:find_content_providers/2` — resolve every host currently announcing
  an MCID (`content_announcement` records, `macula_record:content_key/1`).
  Combine with the existing `call_station/6` to dial a specific announced host
  directly, guaranteeing reach regardless of the connected station's relay hop
  budget — the same value `call_station` already gives unary RPC.
- `macula_record`: fixed a real crash bug — `storage_key/1` had **no clause**
  for `content_announcement` (0x11, below `DOMAIN_TYPE_MIN`, so it never
  reached the generic domain-type fallback), so every `put_record` of one
  raised `function_clause`. macula-station's own `macula_content_announcer`
  auto-publishes a `content_announcement` on every stored manifest and has
  since it shipped — this crash silently broke that publish path end to end.
  Fixed by keying on `SHA-256(MCID)` (`content_key/1`, matching
  macula-station's independent `macula_content_dht:dht_key/1` formula) so
  multiple hosts announcing the same MCID land in one resolvable bag slot.
  Added `read_content_announcement/1`.
- Bounded, BOLT#4-aware retry for `_content.*` CALLs
  (`macula_bolt4:is_retryable/1` — e.g. `temporary_relay_failure` is rated
  `same_path_after_backoff`, its own documented retry contract). `_content.put_manifest`
  was observed to fail its first attempt against a freshly-started content
  store and succeed on retry; the station-side root cause is not yet
  diagnosed, but retrying is what the CALL's own error code prescribes
  regardless, so all four content operations do it uniformly (3 attempts,
  200ms linear backoff).

Verified end-to-end against a real station (macula-station
`macula_station_content_SUITE`): single-block regression, multi-chunk put/get
(including out-of-order-reassembly and empty-content edge cases), and
discovery (a chunked put's announcement resolves via `find_content_providers/2`;
single-block content, which is not announced, resolves to `{ok, []}` rather
than an error).

---

## [8.9.0] - 2026-08-19

### Added

- **Streaming direct-dial**: `macula_client:call_stream_station/6` and
  `macula:call_stream_station/6` — open a streaming RPC by dialing a *specific*
  station directly (ensure/reuse or dial the link, await the handshake, open the
  stream), the streaming analogue of `call_station/6` for unary RPC. Composes with
  DHT resolution (`find_records` → `read_procedure_advertisement` →
  `station_endpoint`) so a stream reaches its provider in one hop, exactly like a
  unary caller. `Opts` may set `dial_timeout_ms` (default 10_000).

Verified end-to-end against a real station (macula-station
`macula_station_call_stream_station_SUITE`): a provider advertises a
`server_stream`; a consumer pool seeded to nothing dials the station directly and
reads the pushed chunks to `eof`. A companion control case proves the pre-existing
station-routed `call_stream/5` path over the same setup — cross-connection
streaming relay was already correct; this release only adds the direct-dial entry
point on top of it.

---

## [8.8.0] - 2026-08-19

### Added

- Per-subscription pubsub **delivery ordering** (`macula_pubsub_order`). A
  publisher stamps every fact with a pool-monotonic `seq`, but the mesh sends
  copies down several links and `macula_client` deduped to the first arrival —
  which scrambled a single publisher's stream (diagnosed: per-publisher order was
  lost, not just total order). `subscribe/5` now takes a `delivery` option:
  - `ordered` (**new default**) — per-publisher FIFO by seq: out-of-order arrivals
    are buffered and released in order; a genuinely missing seq is skipped after
    `order_timeout_ms` (default 250ms). Buffer bounded in time (timeout) and count
    (`order_max_buffer`, default 1024) — over the cap, the head gap is skipped early.
  - `latest_only` — deliver only seqs newer than the highest seen for that
    publisher (drop stale); no buffering, no head-of-line delay. For state snapshots.
  - `as_arrives` — the previous behaviour: raw arrival order, consumer orders itself.
- `connect/2` options `order_timeout_ms` and `order_max_buffer`.
- `status/1` reports `pubsub_gap_skips` — the count of per-publisher gaps given up
  on after timeout, i.e. the genuine loss rate an `ordered` subscriber could not
  fill. Instruments whether eventual-delivery (Plumtree lazy-repair) hardening is
  warranted, from live data.

### Changed

- **Default pubsub delivery is now `ordered`** (was raw arrival order). A
  publish/subscribe API implies per-publisher order; consumers that assumed it no
  longer break quietly. Consumers that want the old behaviour pass
  `#{delivery => as_arrives}`; latency-sensitive state consumers pass
  `#{delivery => latest_only}`.

---

## [8.7.0] - 2026-08-19

### Added

- Direct-dial dual-trust, Direction B (Slice 7c) — managed-realm consumer→provider
  trust rooted in the realm CA via the X.509 service-cert chain, instead of the
  keyless realm tag. Findings that forced the pivot: the realm tag is
  `SHA-256(realm_name)` (no private key), and the realm holds no stable Ed25519
  signing key, so `verify_delegation_chain/4` could never be published against a
  real realm. The realm CA is the authority that actually exists and is already
  delivered to every member at issuance.
  - `macula_record:verify_advertisement_cert_chain/3` — verify a resolved
    `procedure_advertisement`'s embedded X.509 chain (leaf → org CA → realm CA)
    to a trusted realm CA: advertisement signature valid, leaf binds the
    advertiser's Ed25519 key, chain validates (`public_key:pkix_path_validation`),
    and the leaf's organization (O) RDN equals the URI's `<org>`. Any failure
    drops the advertisement as a squat.
  - `procedure_advertisement/4` gains a `cert_chain` opt (leaf ++ org CA, PEM),
    carried in the record payload and surfaced by `read_procedure_advertisement/1`,
    so verification is offline and rides the record's TTL — no side lookup.
- The 8.6.0 Ed25519 delegation records (`org_directory` / `procedure_delegation` /
  `verify_delegation_chain`) are retained but go unused on the live managed-realm
  path (superseded by the cert chain; not deleted).

---

## [8.6.0] - 2026-08-19

### Added

- Direct-dial dual-trust, consumer side (Slice 7c) — the realm → org → server
  delegation chain:
  - `macula_record:org_directory/3,4` (realm-signed `org-name → org-key`) and
    `procedure_delegation/2,3` (org-signed `server may serve org`).
  - `org_directory_key/2`, `procedure_delegation_key/2`, `read_org_directory/1`,
    `read_procedure_delegation/1`.
  - `verify_delegation_chain/4` — confirm an advertisement is legitimately
    authorized (realm signs the org key, org signs the server); any break is a squat.

### Fixed

- `macula_record:storage_key/1` for payload-field-keyed record types
  (`procedure_advertisement`, `realm_member_endorsement`, `foundation_parameter` /
  `t3_attestation`, address / hosted-address maps, plus the new 7c records) now
  reads the field via the robust getter, so it no longer **crashes when a record is
  put over the SDK** (`put_record/2`) and arrives wire-decoded with atomised keys.
  Previously such a put crashed the station's store handler with
  `temporary_relay_failure` — SDK `put_record` of these types was silently broken and
  had only ever been exercised via direct erpc puts (canonical keys). Found by the
  7c e2e; also fixes `procedure_advertisement` publishing from hecate-om (Slice 2).

---

## [8.5.0] - 2026-08-19

### Added

- Provider-side capability authorization (direct-dial dual-trust, Slice 7b):
  - `macula:advertise/5` honors an `auth` opt: `open` (default, serve any
    identified caller) or `{ucan_required, Issuer}` (gate the procedure). A gated
    procedure verifies the CALL's `ucan_token` against `Issuer` via
    `macula_ucan_nif:verify/2`, refusing absent/invalid with BOLT#4 `unauthorized`.
  - `macula:call_station/7` presents a `ucan_token` to a gated provider.
  - BOLT#4 code `unauthorized` (0x10). The wire already encodes the code as a full
    byte, so the code field is unchanged.
  - The CALL frame gains an optional `ucan_token` field (rides the generic codec).
- `macula_client:auth_policy/0` type.

All additive: `advertise/4`, `call_station/6`, `call/5` are unchanged and the
default policy is `open`.

---

## [8.4.1] - 2026-08-19

### Fixed

- `macula_record:read_procedure_advertisement/1` and `read_station_endpoint/1`
  now read payload fields whose keys were **atomised by the frame decoder** — the
  shape a record actually arrives in over the `find_records/2` / RPC-result path
  (`#{serving_station => ..., procedure_uri => {text, ...}}`). They previously
  handled only `{text, _}` and bare-binary keys, so a consumer resolving via the
  SDK got `undefined` fields and direct-dial resolution silently failed. Earlier
  tests used `find_value` over erpc (which keeps canonical `{text, _}` keys) and
  missed it; the Slice 5 `find_records` e2e caught it. Consumers on 8.2.0-8.4.0
  should upgrade.

---

## [8.4.0] - 2026-08-19

### Added

- `macula:connect/2` now forwards `verify` (`webpki` | `none`) and
  `expected_node_id` from its opts to every link the pool dials — seeds AND
  `call_station/6` targets. A pool can therefore dial a self-signed station
  (`verify => none`, dev/loopback) or pin a station's Ed25519 identity
  (`expected_node_id`, production). Direct-dial to a resolved serving_station
  needs this TLS-policy control; previously links were always `webpki`. Additive,
  default unchanged.

---

## [8.3.0] - 2026-08-19

### Added

- `macula:call_station/6` — issue a CALL to ONE specific station by seed URL,
  dialing it directly even if it is not in the pool's seed set. The pool reuses an
  existing link or dials + monitors a new one, waits for the handshake within the
  deadline, and calls there; returns `{error, not_connected}` if the handshake
  does not complete in time. This is the direct-dial data path (resolve a
  serving_station + endpoint, then reach it in one hop, no mesh relay).
- `macula_record:station_endpoint_key/1` — derive a station's endpoint storage key
  from its pubkey (for `find_record/2` before holding a record).
- `macula_record:read_station_endpoint/1` — read a `station_endpoint` record's
  `quic_port` + `host_advertised` as a typed map (robust to canonical vs
  wire-decoded payload keying).

Direct-dial discovery Slices 3 (station endpoints) + 4 (dynamic dial). See
macula-station `DESIGN_DIRECT_DIAL_DISCOVERY`.

---

## [8.2.0] - 2026-08-19

### Added

- `macula_record:read_procedure_advertisement/1` — read a procedure_advertisement
  record's fields (`procedure_uri`, `advertiser_node`, `serving_station`) as a
  typed map, robust to both canonical (`{text, _}`) and wire-decoded (bare binary)
  payload keying, so consumers never parse the CBOR shape themselves.
- `macula_record:procedure_key/1` — derive a procedure's DHT storage key
  (`SHA-256(procedure_uri)`) from the URI alone, for `find_records/2` before
  holding any record.

Both are the consumer-resolution surface for direct-dial discovery (macula-station
`DESIGN_DIRECT_DIAL_DISCOVERY` / plan Slice 2).

---

## [8.1.0] - 2026-08-19

### Added

- `macula:find_records/2` — multi-value DHT read returning EVERY record at a
  storage key (e.g. every provider that advertised one procedure_uri), where
  `find_record/2` returns only the first. Calls the new `_dht.find_records`
  relay procedure (served by macula-station). Additive; `find_record/2` is
  unchanged. Part of direct-dial discovery (macula-station
  `DESIGN_DIRECT_DIAL_DISCOVERY` §8.1 / plan Slice 1).

### Docs

- Clarified that pubsub does not preserve per-publisher delivery order.

---

## [8.0.2] - 2026-08-13

**Identical code to 8.0.1. Republished because 8.0.1 never became
resolvable.**

8.0.1 published successfully by every measure hex offers: the API lists
it, `has_docs` is true, and the tarball at
`repo.hex.pm/tarballs/macula-8.0.1.tar` downloads and is a valid hex
archive. But no resolver can see it — a throwaway project asking for
`{macula, "8.0.1"}` gets `Package not found in any repo`, more than an
hour after publication, from a clean cache.

It is specific to this package rather than hex being slow: `hecate_om
0.10.0`, published three minutes later, resolved normally in the same
clean-room test.

Rather than wait on a stuck registry entry with no ETA, this is the same
code under a version that gets a fresh one. **If you are already running
8.0.1 somehow, there is no reason to move.** Everything below is the
8.0.1 changelog, unchanged.

### Fixed

- `macula_quic:getstat/2` answered `{ok, [{send_cnt, 0}, ...]}` —
  well-formed, plausible and permanently zero, because the NIF binding
  was never written. It now answers `{error, not_implemented}`. Its own
  doc excused the zeros as "harmless (dist_util only uses these for
  liveness signals)", which is exactly the use a hardcoded zero
  destroys: frozen at zero, "nothing is moving" and "nobody implemented
  this" are the same reading.

- **A sick link can no longer kill the pool.** `macula_client`'s
  `status/1`, `links/1` and publish path each probed every link with
  `is_connected/1` or `peer_node_id/1` — both 1s `gen_server:call`s —
  from inside the pool's own process. `gen_server:call` exits its
  CALLER on both `{noproc, _}` and `{timeout, _}`, and the pool is the
  caller, so probing one sick link destroyed every subscription,
  advertisement and pending call the client held. The timeout path
  needs no race at all: a link merely alive and unresponsive for one
  second was enough, which is what a wedged station looks like from a
  client.

  `link_node_id/2` was worse than the other two — it called
  `peer_node_id/1` with no liveness guard at all, and matched only
  `{ok, _}` and `{error, not_connected}`, so a third reply shape was a
  `case_clause` in the pool. Same fatality, third route.

  Both guards answer `false` / `undefined`, so an unreachable link is
  reported truthfully and conservatively and can never read as healthy.

## [8.0.1] - 2026-08-13

**A counter that always answers zero is worse than no counter.**

### Fixed

`macula_quic:getstat/2` answered `{ok, [{send_cnt, 0}, {recv_cnt, 0}, ...]}` —
well-formed, plausible, and permanently zero, because the NIF binding was never
written. It now answers `{error, not_implemented}`.

Its own doc excused the zeros: *"harmless (dist_util only uses these for
liveness signals)"*. Liveness is exactly the use a hardcoded zero destroys. A
counter frozen at zero makes **"nothing is moving"** indistinguishable from
**"nobody implemented the counter"**, so a liveness check built on it reads
green forever and its author has no way to notice.

Not hypothetical. On 2026-08-13 a station received every packet sent to it,
answered none for thirty hours, and every signal derived from BEAM state read
healthy. Anyone reaching for a send-side counter to catch that would have found
this function, and it would have lied to them.

**No behaviour change for the only consumer.** `macula_dist:quic_getstat/1`
already had an `{error, _} -> {ok, 0, 0, 0}` branch, so it takes the same path
and produces the same result it did before. What changes is what the *next*
caller is told.

Quinn has the real numbers — `Connection::stats()` carries `udp_tx`, `udp_rx`
and `path{rtt, lost_packets, black_holes_detected}` — and `nif_max_datagram_size`
already calls `stats()` and keeps only `path.current_mtu`. Surfacing the rest is
an extension of a working function, tracked as commit 5 of
`macula-station/plans/PLAN_WIRE_LIVENESS_TRIPWIRE.md`.

---

## [8.0.0] - 2026-08-11

**A service can now say why it refused.**

### Breaking

`macula_station_link:call/5` no longer answers `{error, {call_error, 16#0F,
unknown_error}}` when a handler refuses. It answers the handler's own reason.

`0x0F` is the code this SDK stamps on the wire when a handler returns
`{error, Reason}`, so it never meant "unknown error" in practice: it meant a
handler had said no. The ERROR clause of `on_frame/2` read `code` and `name`
and dropped `detail` on the floor, so **every refusal in the world arrived as
the same three words** and no service could tell a caller anything.

```erlang
%% handler
handle(_) -> {error, <<"hold_full">>}.

%% caller, before
{error, {call_error, 15, unknown_error}}
%% caller, now
{error, <<"hold_full">>}
```

Every other code is the transport failing rather than a handler speaking, and
keeps the `{call_error, Code, Name}` shape. An ERROR frame with no `detail`,
from an older peer or from the two frames this SDK sends without one, falls
back to the tuple.

A binary reason now crosses the wire **verbatim**. Before, `format_error_detail/1`
put every reason through `~0p`, so `{error, <<"hold_full">>}` reached the frame
as `<<"<<\"hold_full\">>">>`, a rendering of a binary rather than the binary, and
no caller could compare against it. Reasons that are not binaries are still
rendered: a reason that crosses a wire crosses it as bytes, so a handler that
wants its caller to match on the reason should say it in a binary. Still capped
at 256 bytes.

This also settles a contradiction at the call site rather than in the spec
table. BOLT#4 rates `0x0F` `log_and_caution`, so `macula_bolt4:is_retryable/1`
answers `true` for it, which is right for a genuinely unknown error and wrong
for a handler that has just said no. The table is the spec's and is untouched; a
caller who gets the reason back does not have to ask.

Found by four services built against this SDK, none of which could tell a user
why an order was refused.

---

## [7.1.0] - 2026-07-26

**A restarting frame recipient no longer silently black-holes a connection's
pubsub and DHT traffic.** `dht_recipient` and `pubsub_recipient` now accept a
registered name as well as a pid, and the recipient is resolved on every frame
instead of being captured once in `init/1`.

The category-bypass guard was `is_pid(Pid)`, and `is_pid/1` is true for a
**dead** pid. When a station's frame dispatcher crash-restarted, every peering
connection established before the restart kept posting frames to the dead pid.
Messages to a dead pid are discarded by the VM, so those connections went
pubsub-silent (and DHT-silent) for the whole rest of their lives, with no error
logged at either end, no `disconnected` event, and no reconnect to trigger
recovery. The only cure was tearing the connection down.

Resolution now happens per frame:

- a registered name is re-resolved every time, so a supervisor restart is
  transparent — the name is re-pointed at the new pid and the next frame lands
  there;
- a local pid is liveness-checked;
- an unset, unregistered or dead recipient falls back to `controlling_pid` in
  the legacy pre-4.4.3/4.4.4 frame form, which still handles every category, so
  the bypass degrades to the slower path instead of dropping traffic.

Consumers should pass the registered name. Passing a pid keeps working and is
now liveness-checked, but a pid cannot follow a restart.

## [7.0.0] - 2026-07-26

**The canonical encoder now carries floats, so callers stop scaling around it.**
`macula_record_cbor` emits IEEE 754 binary64 (RFC 8949 major type 7, additional
info 27) and decodes all three widths a conforming peer may send. `to_wire/1`
no longer rewrites a float as six-decimal text, and `check_payload/1` no longer
rejects one.

This is a WIRE change and therefore a major: a 6.x peer decoding a float payload
finds no clause for major 7 and rejects the frame, so both ends must be on 7.x
before floats appear on a topic they share.

Why it took two majors to get here is the useful part. 6.0.0 rejected floats
loudly, which was right at the time and wrong as a destination: it left every
producer of real telemetry (hecate-victron publishes Victron voltages, currents
and state of charge, almost all floats) scaling to integers to smuggle a number
past our own codec. That is a workaround in consuming code for a gap in a
library we own. The gap was never CBOR's — RFC 8949 has had floats since 2013 —
it was that this encoder implemented a subset and nobody had needed the rest
yet.

Always binary64, never the shorter half or single forms. Determinism needs one
canonical encoding per value, not the shortest one; "shortest that round-trips"
would make the signed bytes depend on a width-selection rule every peer must
reproduce bit for bit. Nine bytes per float is the price of not having that
argument. Erlang floats are always finite, so there is no NaN canonicalisation
question on encode; NaN or infinity arriving from a foreign peer has no Erlang
representation, matches no clause, and is rejected as a bad frame.

`explain/1` no longer tells operators the mesh cannot carry floats. It can. The
6.0.0 text said otherwise and was itself a restatement of the wrong diagnosis
this release finishes correcting.

## [6.0.0] - 2026-07-26

**Breaking: `macula:publish/4,5` can now return `{error, Reason}` where it
previously returned `ok`.** The spec always allowed it; the behaviour is new.
Callers publishing raw floats, tuples, colliding map keys or oversized payloads
will see errors where they used to see success. Those publishes were not
working before, they were failing silently, so the error is the fix rather than
a regression. Major rather than minor because consumers pin `~> 5.x`: shipping
this as 5.3.0 would auto-upgrade a running fleet into publish rejections nobody
chose.

`macula_peering:send_frame/2` is a cast, so frames were encoded later, inside
the shared peering connection, with no try/catch around the encode. A term the
codec could not represent therefore did not fail its sender. It killed the
connection, took up to `?MAX_BATCH` queued frames from unrelated producers with
it, and the sender had already been told `ok`. That unfalsifiable `ok` is why
services downstream wrap publish in defensive catch-alls: it is the only sane
response to a success value that cannot fail.

The check now runs in `send_frame/2`, the last synchronous point before the cast
and the one seam every producer passes through: pubsub, RPC calls and results,
streaming, advertise and content. Guarding only `macula_client:publish/5` would
have left five of six verbs able to kill the link. `macula_frame:check_frame/1`
and `check_payload/1` return `{unsupported_payload_type, Type, Path}` naming the
offending value and its location, and `macula_frame:explain/1` renders that with
its remedy for logs. `encode_or_drop/2` in the connection is the backstop for
what cannot be known without encoding: one dropped frame and a loud log instead
of a dead connection.

Rejected, each for a demonstrated reason rather than on principle:

- **Floats.** `to_wire/1` silently rewrote them as six-decimal text, so a
  published `52.34` arrived as the string `"52.34"`, rounded, type changed, no
  error anywhere. `float_to_binary/2` also raises `badarg` at large magnitudes,
  so this closes a crash as well as a corruption. Scale to integers (micro-units)
  or send binary strings. This is not a CBOR limitation: RFC 8949 major type 7 is
  floats and `macula_cbor_nif` handles them natively. It is a limitation of the
  canonical envelope encoder, which payloads should not be traversing at all, and
  fixing that properly is a wire-format change.
- **Colliding wire keys.** An atom, a binary and a `{text, Binary}` of the same
  name are one key on the wire, so `#{foo => 1, <<"foo">> => 2}` shipped as two
  pairs and arrived as one, the loser chosen by sort order.
- **Oversized payloads**, via a lower-bound size estimate, sound for rejection.
- Tuples, bitstrings, out-of-range integers, improper lists, pids, refs, funs and
  ports, all of which crashed the encoder.

Also: `macula_record_cbor:is_encodable_int/1` is exported so the 64-bit bound
lives only where the constant does; RPC results the wire refuses now fault the
call with a BOLT#4 `call_error` instead of leaving the remote caller to burn its
deadline; and a refused publish no longer consumes a sequence number, which
would have faked a gap in the `(publisher, seq)` sequence station dedup keys on.

Known limits, stated rather than papered over. `check_frame/1` excludes
`record` / `records`, which go through `macula_record:encode/1`, so
record-bearing frames (STORE, REPLICATE, VALUE) are protected by the backstop
but do not get a structured reason. The receive side still wedges on a malformed
frame: `drain_step/2` returns the buffer unadvanced, so the prefix is re-parsed
forever. `{text, B}` is not validated as UTF-8, which is a deviation a strict
non-Erlang peer would reject.

## [5.2.2] - 2026-07-23

Fixed: pool-owned publish sequence. The outbound PUBLISH `seq` was a *per-link*
counter that reset to 0 whenever a station link respawned, while the publisher
identity (the pool's Ed25519 pubkey) stayed constant. The station-side
`(publisher, seq)` dedup keys on that pair, so a link flap re-issued seqs that
had already been seen — a latent duplicate/false-drop once that dedup becomes
authoritative (Phase 3 of `macula-station` `PLAN_PUBSUB_E2E_SIGNED_EVENTS`).
`macula_client` now owns a monotonic `publish_seq`, seeded from wall-clock
microseconds at init so a pool restart cannot re-issue seqs that collide with
the pre-restart tail still inside a station's dedup window, and stamps the same
seq on every replicated link via the new `macula_station_link:publish/5`.
`publish/4` retains its per-link counter for standalone (pool-less) link use.
No wire-format or public-API break. This lands the "add a per-pool seq counter
to `macula_client`" prerequisite the pubsub-e2e-signed-events plan calls for.

## [5.2.1] - 2026-07-15

Liveness/backoff tuning now falls back to the `macula` application env. 5.2.0
exposed the knobs as `start_link/1` opts, but a consumer creates links from
several places (the realm holds ~64 across its Mesh pool, DHT/directory
subscribers, and the topology pool), and only the explicitly-wired call site
picked up the widened values -- the rest kept the tight 30s/2 default and kept
flapping on overloaded stations. Now `macula_station_link` reads the `macula`
application env as the default when an opt is unset, so one line --
`config :macula, liveness_max_misses: N, liveness_interval_ms: Ms` -- widens
EVERY link regardless of which subsystem spawned it, and stays tunable from the
deployment (env-driven sys.config) without a code change. Explicit `start_link`
opts still win; the module `?DEFINE`s stay the ground default.

## [5.2.0] - 2026-07-15

Tunable station-link liveness. The app-level liveness probe in
`macula_station_link` was hardcoded at a 30 s interval with a 2-miss
teardown, so a client holding many links to variously-loaded stations
would recycle a link whenever a *busy-but-alive* station failed to answer
two consecutive `_macula.ping` CALLs in time — observed as ~1 link
flap/45 s on the realm's station pool when relay boxes ran hot (load ~6 on
2 vCPU), each flap triggering a full re-subscribe storm.

### Added
- `macula_station_link` `start_link/1` opts `liveness_interval_ms` and
  `liveness_max_misses`, each defaulting to the previous constants
  (30 000 / 2). A consumer with a pool of links to busy stations (the
  realm) can widen the tolerance so a slow-but-alive link is not recycled;
  the daemon and wardens keep the tight default for fast zombie detection.
- `macula_station_link` `start_link/1` opt `connect_retry_backoff_ms`
  (default 1_000) — the wait before re-dialling after a failed connect.
  Raise it on a pool that cycles links so torn links don't reconnect in
  lockstep and hammer the station with re-subscribe storms. Also documented
  the QUIC transport knobs (`idle_timeout_ms`, `keep_alive_interval_ms`,
  stream counts) already forwardable via the `seed` map.
  Backward compatible — unset opts preserve the prior behaviour exactly.

## [5.1.0] - 2026-07-09

Connect-reliability release. Fixes the root cause of the
`macula.io/clankercab` outage: on a long-lived client that holds many
station-links (the realm ran ~64 across several subsystems), every
`macula_quic:connect` would eventually hang forever, leaving the client
with zero live links. Two independent NIF defects plus a client-side
self-heal gap.

### Fixed — QUIC connect NIF (native/macula_quic)

- **The dial timeout now always fires.** `nif_connect` only wrapped the
  handshake in `tokio::time::timeout`, leaving `lookup_host` unbounded,
  and under runtime pressure even the handshake timeout would not fire —
  so a stalled/black-holed dial parked forever. The whole operation
  (DNS + endpoint + handshake) is now under a single deadline; a dial to
  an unreachable peer returns `{error, connection_timeout}` at
  `timeout_ms` instead of hanging. Verified: reachable station connects
  in ~75 ms; black-hole address returns `connection_timeout` at the
  deadline.
- **One shared client `Endpoint` per address family, not one per dial.**
  Each `connect` used to build a fresh `quinn::Endpoint` — a new UDP
  socket + a new endpoint-driver task on the shared 4-worker tokio
  runtime. On a client that reconnects continuously these accumulated
  hundreds of driver tasks/sockets and starved the runtime's
  reactor/timer (which is why the timeout stopped firing). Connections
  now multiplex over a single shared endpoint per family.
- **Blocking network NIFs moved to dirty-IO schedulers.** `nif_connect`,
  `nif_open_stream` and `nif_send` were scheduled `DirtyCpu` — there is
  one dirty-CPU scheduler per core (two on the realm box), so a couple of
  blocking dials pinned them all and starved every other QUIC operation.
  These are IO-bound and now run on the dirty-IO pool (far larger),
  matching their nature.

### Fixed — connect self-heal

- **`macula_station_link` connect watchdog.** A link whose peering
  worker connects at the transport layer but never delivers
  `{macula_peering, connected, ...}` — e.g. a QUIC dial NIF that hangs
  past its own `timeout_ms`, or a stalled CONNECT/HELLO exchange — used
  to sit `alive-but-not-connected` indefinitely: `peer_pid` set,
  `peer_node_id` undefined, subscriptions queued, no `disconnected`, no
  owner `:DOWN`, no retry. The app-liveness probe did not cover this
  (it only arms *after* `connected`), so there was no bound on the
  un-connected phase and no self-heal after mesh churn. A watchdog is
  now armed the moment the peering worker is spawned and cancelled on
  `connected`; if it fires while still un-connected it kills the wedged
  worker and stops the link so the owner respawns a fresh dial —
  bounded, automatic self-heal. Deadline is `connect_timeout_ms +
  10s`, overridable via the `connect_watchdog_ms` start opt.
  Live-diagnosed on the `macula.io/clankercab` outage (2026-07-09),
  where every realm subscriber link was wedged this way.

## [5.0.0] - 2026-06-10

Security release. Four findings from the 2026-06-10 transport-trust
audit, the first of which changes a default and makes this a major
version.

### Security — BREAKING

- **QUIC dials verify the server certificate by default.**
  `macula_quic:connect/4` now defaults `verify` to `webpki` (webpki
  roots + hostname check) instead of `none`. Previously every peering
  dial silently accepted any server certificate, allowing a network
  MITM to impersonate any peer. The production relay fleet serves
  Let's Encrypt `*.macula.io` certs and is unaffected.
  **Migration:** self-signed setups (local dev, lab clusters, e2e
  harnesses) must either pin the peer identity (preferred — see
  `expected_node_id` below) or opt out explicitly with
  `{verify, none}` / `#{verify => none}` in the seed/target. Every
  unverified dial now logs a warning.

### Security

- **Peer identity binding on the CONNECT/HELLO handshake.**
  `macula_peering_conn` targets (and `macula_station_link` map seeds)
  accept `expected_node_id => Pubkey`. When set, (a) the QUIC dial
  pins the server cert's Ed25519 SPKI to that key, and (b) the HELLO
  is rejected with `{peer_identity_mismatch, Expected, Got}` unless
  the peer's verified `node_id` equals it. Previously the handshake
  only proved the peer holds the key for whatever identity *it*
  claimed — no binding to the dialed target existed.
- **Topology bootstrap fetch now verifies TLS.**
  `macula_relay_discovery` fetched `/topology` with
  `{verify, verify_none}`, letting a MITM steer a bootstrapping node
  onto attacker relays. Now `verify_peer` against the OS trust store
  with wildcard-aware hostname checking.
- **DHT-sourced node info decoded with `binary_to_term(_, [safe])`.**
  `macula_dist_discovery` decoded DHT values (attacker-influenceable)
  unsafely — a crafted record could exhaust the atom table or
  allocate unbounded resources. Records are now shipped atom-free
  (`name`/`protocol` as binaries), decoded `[safe]`, and
  shape-validated; the node name is rebuilt from the caller's
  argument, never atomized from DHT bytes. Mixed-fleet note: 5.0.0
  nodes reject dist-discovery records written by older nodes (atom
  `name` fails the safe decode) until those nodes re-register on
  refresh with the new format.

---

## [4.8.0] - 2026-05-31

### Added

- **`macula:links/1` (and `macula_client:links/1`) — per-link pool
  snapshot.** Returns one `link_info()` map per spawned link, carrying
  `seed`, `host`, `pid`, `connected`, and the peer station's `node_id`
  (pubkey; `undefined` until CONNECT/HELLO completes). `status/1` only
  aggregates counts — this exposes the individual links so a caller can
  resolve a specific station (by pubkey or hostname) to its link for
  targeted, per-station operations.

### Test coverage

- `macula_client_tests`: empty pool → `[]`; one entry per spawned link
  with host parsed from URL and map seeds; unconnected links report
  `connected=false` / `node_id=undefined`.

---

## [4.7.1] - 2026-05-17

### Fixed

- **`macula_record_cbor`: encode + decode CBOR major type 1 (negative
  integers).** Both `encode/1` and `decode_value/3` lacked clauses for
  signed integers; any payload carrying a negative numeric value
  crashed the encoder with `function_clause` and tore down the peering
  connection with it. The new clauses mirror the positive-integer path
  across the full int64 range.
- **`macula_frame:to_wire/1` + `wire_key/1`: accept integers of any
  sign.** `to_wire` only passed non-negative integers through; negatives
  fell into the catch-all and reached the encoder unchanged (then
  crashed there). `wire_key` had no clause for integer keys at all.
  Both now accept integers, enabling payloads whose nested maps are
  indexed by integer (e.g. per-wall sub-maps in mpong game state).

### Test coverage

- `macula_record_cbor_tests`: negative-int round-trip across the mirror
  of the existing uint range, plus an int-key map round-trip.

### Backport

- Same fix shipped as `4.4.10` for the active 4.4.x line that
  hecate-daemon and friends still pin against.

---

## [4.7.0] - 2026-05-16

### Performance

- **Per-link send-frame batching in `macula_peering_conn`.** The
  `connected` state's `cast {send_frame, _}` handler now drains
  all queued `send_frame` casts (cap 64 per pass) and emits them
  in a single `macula_quic:send/2` NIF call. Cuts per-NIF overhead
  + gen_statem reduction-counter cost when many EVENT/PUBLISH
  frames burst together (pubsub flood, DHT batch put). The Quinn
  stream still handles MTU-level packetisation; this is purely an
  Erlang-side amortization. Single-frame fast path preserved.

- **SDK subscriber-side bulk fan-out in `macula_station_link`.**
  `handle_info({macula_peering, frame, ...})` now drains
  consecutive frame messages from the gen_server mailbox (cap 64
  per pass) and folds them through `on_frame/2` in arrival order.
  Removes per-frame context-switch + reduction-reset overhead on
  bursty inbound traffic.

- **Quinn flow-control window defaults bumped** (in
  `native/macula_quic/src/config.rs`):
  - `stream_receive_window`: default 1.25 MB → **16 MB**
  - `receive_window`: default 1.25 MB × streams → **64 MB**
  - `send_window`: default 8 MB → **64 MB**

  Macula peering uses one long-lived bidi stream per connection
  over which we multiplex pubsub EVENTs, CALL/REPLY, DHT records,
  blob streams. With many small frames in flight and the receiver
  doing per-frame Ed25519 verify (~200µs), the default 1.25 MB
  window exhausts long before the receiver acks consumed bytes —
  surfaces as receiver-bound throughput in pubsub flood torture.
  16 MB stream window absorbs ~100k 150-byte EVENT frames before
  backpressure. Applies symmetrically to client + server transport
  configs.

---

## [4.6.0] - 2026-05-15

### Changed

- **`pubsub_emit_publisher_sig` default flipped from `false` to
  `true`.** Publishers now attach an end-to-end publisher signature
  to every outbound PUBLISH/EVENT by default. Wire-format change is
  fully BC (the field has been parseable since 4.4.0 and excluded
  from `canonical_unsigned/1` so the per-hop relay signature stays
  valid). On the receiver side, stations on >= 4.4.0 already prefer
  `verify_publisher/1` for EVENTs that carry the signature (so they
  pass at any relay hop instead of only one), and the
  `macula_station_event_dedup` cache drops `{publisher, seq}`
  repeats.

  Effect: **multi-hop pubsub now works end-to-end.** Previously
  cross-station PubSub was bounded to 1 hop from origin because the
  relay re-signed EVENTs and the verify-mismatch at hop 2+ doubled
  as accidental loop kill. Publisher-end-to-end signatures + dedup
  removes that bound; loops are killed structurally by `(publisher,
  seq)` deduplication, not by signature failure.

  Operators on a fleet that still has pre-4.4.0 stations can opt
  back out: `application:set_env(macula, pubsub_emit_publisher_sig,
  false)` per node. Our deployed fleet is on macula-station with
  SDK 4.5.0 so this knob applies cleanly.

  Four prior incremental attempts (per-hop re-sign, 1-hop cap, two
  dedup variants) at this same problem all regressed cross-station
  traffic and were reverted. The wire foundation (`publisher_sig`
  field, dedup cache observe-only) was landed in 4.4.0; this
  release just flips the default so the foundation is exercised.

## [4.5.0] - 2026-05-14

### Added

- **App-level liveness probe in `macula_station_link`.** Periodic
  `_macula.ping` CALL every 30s (`?LIVENESS_INTERVAL_MS`) with reply
  tracking via `liveness_outstanding`. After
  `?LIVENESS_MAX_MISSES` (=2) consecutive misses, the link issues
  `macula_peering:close(PeerPid, app_liveness_lost)` which surfaces
  through the normal `disconnected` notify path → station_link
  stops → pool respawns with a fresh QUIC handshake.

  Closes the zombie-connection window that previously lasted up to
  the Quinn `max_idle_timeout` (5 minutes) — empirically observed
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
  on `macula_peering_conn`. Populated in `absorb_peer_info/2` from
  the CONNECT / HELLO frame. No wire change (the field was already
  on the frame schema; we just record it).

- Fixed `macula_peering_handshake_tests`'
  `absorb_peer_info_populates_fields` test — was asserting
  `element(14, Data) =:= [Realm]` (a hardcoded offset) but the
  #data record grew several fields since the test was written, so
  it had been silently picking up the `quic_stream` reference. Now
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

  Frame shape: `#{stream_id, seq, encoding, body, signer => <<_:256>>}`
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
  `RecvAtUs :: integer()` element — `erlang:monotonic_time(microsecond)`
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
  `macula_station_link` has been extended to match both shapes so
  daemons receiving from a station that has opted in continue to
  work.

  Motivation: macula-station's `macula_station_peer_observer` runs a
  persistent mailbox depth of 200-400 frames under DHT load. Without
  a `RecvAtUs` stamp the mailbox wait is invisible to a downstream
  observer — `process_info(self(), message_queue_len)` at dispatch
  entry is a coarse proxy but doesn't tell you how old the frame at
  the head of the queue actually is. With `timing_enabled` the
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
  the raw `macula_record:encode/1` binary; the documented contract
  said it would receive the decoded record map. The probe pair
  added in `macula-internal/macula-e2e@8831d1e` surfaced both
  this and the substrate-side topic mismatch (substrate publishes
  on `_dht.records.<type>.stored` as of `macula-internal/macula-station`
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
  `is_nif_loaded()` directly, which returns `false` until the
  `macula_crypto_nif` module is referenced for the first time
  (its `-on_load` callback writes the persistent_term flag). If a
  caller's first-ever NIF use went through `macula_blake3_nif:hash/1`
  rather than something that touched `macula_crypto_nif` first, the
  Erlang fallback fired — and that fallback is NOT plain
  `crypto:hash(sha256, _)`: inputs over 1024 bytes are tree-hashed
  (1024-byte chunks SHA-256'd individually, chunk hashes pair-
  hashed), producing output that matches neither real BLAKE3 nor
  plain SHA-256.

  Surfaced by `macula:put_content/2` in v4.2.7: blobs > 1024 bytes
  computed an SDK-side MCID that no relay could verify, so every
  `_content.put_block` returned `hash_mismatch`. The four other
  hash entry points (`hash_streaming/1`, `verify/2`, `hash_hex/1`)
  had the same bug; all are fixed in lock-step.

  `is_nif_loaded/0` is retained for diagnostic use but its docstring
  now warns that the answer reflects whatever has been observed so
  far. New private helper `ensure_crypto_nif_loaded/0` is the
  authoritative path.

## [4.2.7] - 2026-05-09

### Added

- **`macula:put_content/2` and `macula:get_content/2`** — content-
  addressed blob storage and retrieval over the relay. `put_content`
  computes the BLAKE3 hash of the bytes, packages them into an MCID
  (`<<1, 16#55, Hash:32/binary>>`), and ships the blob to the relay
  via a single `_content.put_block` RPC; the relay verifies the
  payload's hash matches the MCID before accepting. `get_content`
  fetches the blob back via `_content.get_block`, returning
  `{error, not_found}` if no provider in the pool's reach holds a
  copy.

  v4.2.7 minimum-viable surface — single-block per blob, no
  client-side chunking, single-station semantics. Suitable for
  blobs in the kilobyte-to-low-megabyte range. Blobs larger than the
  relay's per-call payload budget will surface as a CALL-deadline
  timeout; chunked manifests + multi-provider parallel fetch land
  in a follow-up release. Cross-station discovery (writer + reader
  on different relays) requires the relay-side iterative-fetch
  fallback that already lands for `_dht.find_record` (commit
  c11226f in macula-station) — wire-symmetric for content once
  exposed.

  `mcid()` type added (`<<_:272>>` = 34 bytes). Hashing uses the
  existing `macula_blake3_nif` that was previously only consumed
  by the record-signing path.

## [4.2.6] - 2026-05-09

### Fixed

- **`macula_peering_conn:on_connect_verified/4` no longer crashes
  when `send_hello` returns `{error, _}`.** Previously asserted
  `ok = send_hello(Stream, NewData)` on the server-side handshake
  completion path; under teardown bursts (multiple peers closing
  pools simultaneously, e.g. e2e `end_per_suite` across a fleet),
  `macula_quic:send` could legitimately return
  `{error, "connection lost"}` between the CONNECT-verify and the
  HELLO write — the peer's QUIC stream was already gone. The
  badmatch crashed the peering_conn gen_statem worker; under
  load enough concurrent crashes tripped the parent supervisor's
  restart-intensity and forced a whole-station restart.

  Now mirrors the existing graceful handling on the client-side
  `send_connect` path (lines 232-240): emit a structured
  `{send_hello_failed, _}` disconnect notify and stop normally,
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
  skip pre-handshake links, which left the link's local `procedures`
  map out of sync with the pool's intent. A subsequent `unadvertise`
  on the same key would skip the link too — its local map kept the
  proc — and the link silently re-ADVERTISED on the next handshake,
  causing the relay station to register a stale procedure that
  nothing in the SDK would ever withdraw. The leak only resolved on
  daemon disconnect, when the station's `purge_conn` fired.

  Each fan-out now dispatches to every LIVE link (filtered by
  `is_process_alive/1` only). The link gen_server's `advertise` /
  `unadvertise` handlers update the local map regardless of
  connection state; the wire frame is best-effort inside
  `maybe_send_advertise` / `maybe_send_unadvertise` (no-op when
  pre-handshake). On the next handshake, `drain_pending_advertises/1`
  replays the now-correct map.

  Surfaced by 2026-05-09 mesh torture: `e2e.cross.echo.{N}` entries
  persisted on stations across rounds with `advertiser=PoolDaemonPubkey`
  even though `unadvertise/3` had returned `ok`. Tombstones in
  `macula_remote_advertise_registry` (macula-station `c7d8fe8`) solve
  the gossip-vs-unadvertise race; this commit closes the
  pre-handshake-skip path that re-creates a fresh stale entry on
  every reconnect.

  Per-link errors are now wrapped in try/catch (`safe_link_advertise/4`,
  `safe_link_unadvertise/3`, stream variants) so a single dead pid
  cannot crash the whole pool gen_server.

## [4.2.4] - 2026-05-08

### Fixed

- **`macula_peering_conn` server-side handshaking now takes ownership
  of inbound streams.** When a server accepts a new conn and the
  client opens a stream, Quinn creates the `StreamResource` with its
  owner field set to whatever owns the conn AT THAT MOMENT. On the
  accept path that's still the listener — the conn-ownership transfer
  hasn't fired yet. Calling `setopt(Stream, active, true)` on its
  own does NOT change ownership; it only flips the active-delivery
  flag. Future `{quic, Bin, Stream, _Flags}` events therefore went
  to the listener's mailbox and got silently dropped by its wildcard
  `handle_info/2`. The worker sat in `handshaking` with `buf_size = 0`
  until its 30s timeout, even though 4.2.3's `awaiting_start`
  postpone clause + macula-station's stray-event forwarder both
  delivered the `new_stream` notification on time.

  Fix: call `macula_quic:controlling_process(Stream, self())` in the
  server-side `handshaking` new_stream handler before `setopt`. The
  worker is now the explicit stream owner, so subsequent
  `{quic, Bin, Stream, _}` events route to it directly.

  Pairs with macula-station's listener forwarding fix (commit
  `85dff3e` on macula-internal/macula-station): together they close
  the cross-station handshake race that was leaving every station
  with tens of stuck workers and partial bloom convergence.

## [4.2.3] - 2026-05-08

### Fixed

- **`macula_peering_conn` server-side `awaiting_start` no longer drops
  racing QUIC events.** `macula_peering:accept/2` transfers conn
  ownership and then casts `start_handshake`; if the QUIC NIF
  redelivers a buffered `{quic, new_stream, ...}` or `{quic, Bin, ...}`
  event to the worker before the cast lands in its mailbox, the
  worker is still in `awaiting_start`. The previous wildcard clause
  routed those events through `drop_unexpected/4` and the bytes were
  lost; the worker then sat in `handshaking` with an empty buffer
  until its 30s timeout, never reaching `transition_to_connected`.
  The peer's client-side worker meanwhile stayed `connected` (it
  received our HELLO) so QUIC keep-alive papered over the asymmetry,
  but the listener-side never registered the peer in its `peers` map
  and the controlling-pid's `connected` notification never fired —
  cross-station SUBSCRIBE / EVENT routing dead-ended.

  Verified live across the production Leuven mesh: every station had
  several stuck workers (`peer_node_id = undefined, buf_size = 0`),
  and three of centrum's outbound peers had no corresponding inbound
  registration on the peer's `peer_observer`. The race was
  particularly brutal under fleet-wide reconnect bursts (post-roll).

  Fix: postpone QUIC events received in `awaiting_start` so they
  re-deliver after the `start_handshake` transition into
  `handshaking`, where the real handler consumes them.

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
  worker's `accept_owner` pid changed from
  `{macula_peering, handshake_complete, ConnPid}` to
  `{macula_peering, handshake_complete, ConnPid, PeerNodeId}`, where
  `PeerNodeId` is the Ed25519 pubkey extracted (and signature-
  verified) from the inbound CONNECT/HELLO frame.

  Lets accept-side listeners dedupe duplicate dials from the same
  peer identity. Without it, a peer that re-dials before its prior
  connection has been torn down (by client-side handshake timeout,
  network partition, or process restart) accumulates a fresh
  `connected`-state worker on every retry. Production stations have
  been observed at 99 stuck `connected` workers from a single
  sister-station because each dial completes the handshake, the
  prior worker holds its QUIC conn open until idle-timeout, and
  nothing dedupes them.

  See `macula-station` commit pairing this release for the
  listener-side dedupe consumer.

### Removed

- **Yggdrasil module + sovereign-overlay `{pubkey, ...}` dial form.**
  `macula_yggdrasil` and the `macula_quic:connect({pubkey, Pk}, ...)`
  / `macula_peering_conn:do_connect(#{pubkey := Pk})` clauses are
  gone. No callers remain in the codebase; `macula-net` replaces
  yggdrasil as the routing substrate. Self-signed pubkey-anchored
  cert generation (`macula_quic:generate_self_signed_cert/3`) stays
  — it has live consumers in `macula_net_transport_quic` and
  `macula_station_listener` that wrap an Ed25519 keypair without
  any Yggdrasil-derived address.
- **Dead test files.**
  - `test/macula_quic_tests.erl` — tested the retired `quicer`-style
    API surface (`accept/2`, `recv/2`, `accept_stream/2`, etc.) that
    the Quinn NIF does not expose.
  - `test/macula_quic_idle_timeout_tests.erl` — tested `quicer`
    proplist option format.
  - `test/macula_yggdrasil_tests.erl` — paired with the deleted
    module above.
  - `test/macula_client_test_server.erl` — helper used only by the
    gateway tests below.
  - `test/macula_gateway_system/` — entire directory, 13 test files,
    targeted the V1 gateway surface fully retired in 4.0.0.

### Breaking

- **`accept_owner` consumers must update their pattern.** Any code
  matching `{macula_peering, handshake_complete, Pid}` no longer
  matches; the message is now a 4-tuple. Match on
  `{macula_peering, handshake_complete, Pid, _PeerNodeId}` or use
  the `PeerNodeId` for dedupe.

  Only `macula_station_listener` in `macula-station` currently
  consumes this message; that consumer is updated in the paired
  release.

  No other behaviour change for callers that don't pass
  `accept_owner`.

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
