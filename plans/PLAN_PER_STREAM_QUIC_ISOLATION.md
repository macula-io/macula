# Per-Stream QUIC Isolation

**Status:** Phase 1 (streaming RPC) COMPLETE. Phase 2 (content transfer) not started.
**Created:** 2026-08-20
**Last Updated:** 2026-08-20

## Scope correction (2026-08-20, same day)

This plan originally scoped Phase 1 as SDK-only: give `macula_peering_conn.erl` a
dedicated-stream primitive, have `call_stream`/`advertise_stream` use it, done. That
was wrong. **`macula-station` is not a passive frame-forwarder for streaming RPC** —
`macula_station_peer_observer.erl` (in the separate `macula-station` repo) actively
relays STREAM_OPEN/DATA/END/ERROR/REPLY between the caller's connection and the
advertiser's connection, tracking the session and forwarding frames both ways. Once
STREAM_OPEN moves off the shared control stream onto a dedicated QUIC stream, the
relay has to open its *own* dedicated stream toward the advertiser and bridge the two
— there is no shared stream left for it to pattern-match frames off. That's a second,
separate implementation surface in a separate repo, not a follow-on detail.

It surfaced only once the SDK-side work was live-tested end to end (see Testing
below) — a `macula_station_call_stream_station_SUITE` run against the rewritten SDK,
with `macula-station` still on its old shared-stream relay logic, went straight to
`{stuck, {error, timeout}, []}`: the caller's dedicated STREAM_OPEN reached the
station fine, but nothing forwarded it anywhere.

## Overview

Macula's docs (forum post, `STREAMING_GUIDE.md`) claim RPC, PubSub, and content streaming
each ride "independent multiplexed QUIC streams" with "per-stream flow control." That's
false. `macula_peering_conn.erl` opens exactly **one** QUIC stream per peering connection
(`quic_stream :: undefined | reference()`, opened once at handshake) and every frame type
— CALL, RESULT, ERROR, PUBLISH, SUBSCRIBE, EVENT, STREAM_OPEN, STREAM_DATA, content
put/get blocks — gets multiplexed onto that one stream via application-level IDs
(`call_id`, `stream_id`). QUIC's actual per-stream isolation (no head-of-line blocking
between independent streams) is not used anywhere, client or station side. The 16MB/64MB
flow-control window bump already in `native/macula_quic/src/config.rs` is a symptom
patch for contention on that one shared stream, not a fix.

This plan gives streaming RPC and content transfer genuine, dedicated QUIC streams, so
the documented claims become true instead of needing to be walked back.

No backward-compatibility constraints: zero external users, free to make a clean break
rather than versioning or negotiating anything.

## Why the Rust layer needs no changes

`native/macula_quic/src/stream.rs` (`StreamResource`) and `connection.rs`
(`nif_open_stream/1`, wrapping `connection.open_bi()`) already provide a fully general,
working, independent-QUIC-stream primitive — `macula_quic:open_stream(Conn)` returns a
brand new stream with its own send/recv/close/active-mode NIFs, right now, unused for
anything but the one handshake-time stream. The entire gap is in the Erlang
orchestration layer. This is a wiring project, not a transport-library project.

## Goals

- Streaming RPC sessions (`advertise_stream`/`call_stream`) get their own dedicated QUIC
  stream per open session.
- Content transfer (put/get, single-block and chunked) gets its own dedicated QUIC
  stream per transfer.
- The claims in `STREAMING_GUIDE.md` and the forum post become literally true, and are
  verified true by a test that demonstrates isolation, not just asserts it.

## Non-goals (this plan)

- Giving every individual unary RPC call (`advertise`/`call`) its own QUIC stream. This
  is a real, viable future option (mirrors how gRPC gives each call its own HTTP/2
  stream) but is a separate decision with its own overhead trade-offs — not bundled
  here.
- Moving PubSub off the shared control stream.
- Per-chunk parallel content transfer. This plan gives a content transfer its own
  stream (isolation from other connection traffic); making the chunks *within* one
  transfer concurrent is a distinct throughput optimization, tracked as a follow-on,
  not conflated with isolation here.
- 0-RTT, connection migration. Unrelated concerns from the same audit; not in scope.

## Architecture before this work (verified 2026-08-20)

- `src/peering/macula_peering_conn.erl`: single `quic_stream` field, opened once in the
  `handshaking` state via `macula_quic:open_stream/1`. `send_application_frame/2` and
  `send_application_frames/2` write every outbound frame onto that one stream. The
  `connected` state has **no** handler for an inbound `{quic, new_stream, Stream, Info}`
  event — it exists only in the `handshaking` state, for the one initial stream. A peer
  opening a second stream today falls through to the catch-all clause and is effectively
  ignored.
- `src/client/macula_station_link.erl`: `client_streams`/`server_streams` maps, keyed by
  a 16-byte application-level `stream_id`, not a QUIC stream reference. STREAM_OPEN /
  STREAM_DATA / STREAM_END / STREAM_ERROR / STREAM_REPLY are frame *types* dispatched by
  `on_frame/2`, arriving as bytes on the shared control stream like everything else.
- `macula-station/apps/macula_station/src/macula_station_peer_observer.erl`: STREAM_OPEN
  routes by procedure exactly like a CALL (via `remote_lookup/2`); once routed, the
  session is tracked as `{CallerOrigin, AdvertiserConn, TRef}` keyed by `stream_id`, and
  subsequent STREAM_DATA/END/ERROR/REPLY frames are relayed with
  `macula_peering:send_frame/2` — the shared control stream on **both** legs of the
  relay.
- `src/macula.erl`: `put_chunks/4` / `get_chunks/5` are simple recursive one-at-a-time
  loops (confirmed by the wire-protocol audit) — sequential regardless of transport, and
  currently also sharing the one connection stream. (Phase 2, untouched by this work.)

## Target architecture

### Phase 1 — streaming RPC gets a dedicated stream per session (SHIPPED)

**New primitive, `src/peering/`:**
- `macula_peering_conn.erl`: `connected/3` gained a clause for inbound
  `{quic, new_stream, Stream, _Info}` — takes stream custody, then hands it to
  `controlling_pid` as `{macula_peering, new_dedicated_stream, self(), Stream}` rather
  than keeping it. A new `{call, From}` clause, `{open_dedicated_stream, Owner}`, opens
  a stream via `macula_quic:open_stream/1` and hands it *directly* to `Owner` with no
  custody window (used for outbound-initiated dedicated streams).
- `macula_peering.erl`: two new exported functions wrapping the above —
  `open_dedicated_stream/1` (`gen_statem:call` to the clause above) and
  `send_on_stream/3` (encode + sign-if-unsigned + `macula_quic:send/2`, bypassing the
  peering_conn process entirely for the write path, same as `send_frame/2` does for
  the *check* but not the *transport*).

**Consumer/provider side, `src/client/macula_station_link.erl`** (not
`macula_stream.erl` — the plan's original text was wrong here; `macula_stream.erl` was
never touched, since `macula_station_link` already intermediated all stream I/O and
continues to, just over a different stream reference):
- `client_streams`/`server_streams` map values grew a third element, the dedicated
  `reference()`; a new `stream_bufs :: #{reference() => binary()}` field buffers partial
  frames per dedicated stream (mirrors the connection-level `buf` field
  `macula_peering_conn.erl` used to own alone).
- Outbound (`call_stream`): opens a dedicated stream via
  `macula_peering:open_dedicated_stream/1`, writes STREAM_OPEN as its first bytes via
  `send_on_stream/3`.
- Inbound (`advertise_stream` handler dispatch): the `new_dedicated_stream` notification
  seeds an empty buffer; the first decoded frame on it is dispatched exactly like the
  old `on_frame/2` STREAM_OPEN case, then every later frame on that stream routes by
  physical stream reference instead of application-level `stream_id`.
- Teardown (disconnect, session-drop, monitor-down) now also closes the dedicated
  stream (`macula_quic:close_stream/1`) and drops its buffer entry — a resource that
  didn't exist before this work and so didn't need reclaiming before.

**Relay side, `macula-station/apps/macula_station/src/macula_station_peer_observer.erl`**
(the scope this plan originally missed — see "Scope correction" above):
- The observer is a full participant on *both* dedicated streams of a relayed session,
  not a pass-through. On STREAM_OPEN (first frame on a fresh inbound dedicated stream,
  looked up by procedure exactly like a CALL), it opens its *own* dedicated stream
  toward the advertiser via `macula_peering:open_dedicated_stream/1`, forwards
  STREAM_OPEN onto it via `send_on_stream/3`, and records the pair both ways in a new
  `stream_route :: #{reference() => {OtherConnPid, OtherStream, stream_id()}}` map for
  O(1) relay lookup by physical stream reference. Every later frame on either side
  relays straight across via `send_on_stream/3` with no re-verification of the
  procedure.
- Needed the station's *own signing key pair* wired into the observer's state
  (`macula_station_app.erl`'s `observer_child/3` now passes `identity => Kp`, not just
  `self_id => pubkey`), because `send_on_stream/3` bypasses the peering_conn process
  that used to sign on the observer's behalf via `send_frame/2`. In practice this
  identity is unused on the hot path — every relayed frame already carries the
  original caller's/advertiser's end-to-end signature and `ensure_signed/2` is a no-op
  for an already-signed frame — but it's load-bearing for the station-originated
  `stream_error` replies (unknown procedure, failed to open the advertiser-side
  stream).
- Disconnect cleanup (`on_disconnected/2`) now also tears down every routed stream
  session with a leg on the dying connection, closing both dedicated streams rather
  than leaving them for the 5-minute TTL — genuine QUIC resources leak differently
  than the old map-only routing hint did.

**A load-bearing bug this surfaced, fixed as part of Phase 1 (not a pre-existing
issue that predates this plan):** `macula_peering_conn.erl` only started the Rust-side
QUIC bidi-stream accept loop (`macula_quic:async_accept_stream/1`) for the **server**
role, during handshake. The **client** role never started it. Harmless in the old
one-stream world (the client always opened the only stream that ever existed, so
nothing needed to *accept* anything). Fatal here: any client-role connection — a
daemon dialing a station, or a station dialing another station — could open dedicated
streams outward but could never receive one opened *at* it, e.g. by a relay forwarding
STREAM_OPEN toward it. Fixed by starting the accept loop in the client-role
`handshaking(enter, ...)` clause too, alongside its existing control-stream open. Found
via live CT testing (see Testing below), not by static review — the failure mode was
total silence with zero log output on either side, since nothing errors when a peer
simply never calls `accept_bi()`.

**Frame encoding:** unchanged from the plan — STREAM_DATA/END/ERROR/REPLY kept their
CBOR shape and the `stream_id` field, even though it's now redundant for routing on the
dedicated stream (kept for logging/debugging and because dropping it bought nothing).

### Phase 2 — content transfer gets a dedicated stream per transfer

Give each `put_content`/`get_content` call (single-block or the whole chunked
manifest-plus-blocks sequence) its own dedicated QUIC stream, opened the same way as
Phase 1, isolating a large blob transfer from concurrent RPC/PubSub traffic on the same
connection. Chunks within one transfer stay sequential in this phase — that's a
throughput question, not an isolation question, and is explicitly deferred (see
Non-goals) to avoid shipping two different kinds of change under one heading.

## Files to change

**Phase 1 (streaming RPC) — SHIPPED:**

| File | Change | Status |
|---|---|---|
| `macula` `src/peering/macula_peering_conn.erl` | new `connected/3` clauses for inbound `{quic, new_stream, ...}` and `{open_dedicated_stream, Owner}`; client role now also calls `async_accept_stream` at handshake-enter (the bug fix) | Done |
| `macula` `src/peering/macula_peering.erl` | new exports `open_dedicated_stream/1`, `send_on_stream/3` | Done |
| `macula` `src/client/macula_station_link.erl` | dedicated-stream-aware `client_streams`/`server_streams`/`stream_bufs`; STREAM_OPEN dispatch off the new inbound-stream path instead of `on_frame/2`; `fail_all_pending/2` fixed for the new tuple shape | Done |
| `macula` `test/macula_station_link_tests.erl` | streaming tests rewired to mock `macula_peering:open_dedicated_stream/1` / `send_on_stream/3`; each test wraps its body in `try/after` so a thrown assertion can't poison the next test's meck mock | Done |
| `macula-station` `apps/macula_station/src/macula_station_peer_observer.erl` | full relay rewrite: opens its own dedicated stream toward the advertiser, tracks `stream_route`/`stream_bufs` by physical stream reference, closes both legs on terminal frame / TTL / disconnect | Done |
| `macula-station` `apps/macula_station/src/macula_station_app.erl` | `observer_child/3` now passes the station's full `identity` key pair, not just `self_id` | Done |
| `macula-station` `apps/macula_station/test/macula_station_peer_observer_tests.erl` | two hardcoded positional `element()` indices into `#state{}` (`?IS_STATION_INDEX`, `forwarded_size/1`) updated for the grown record | Done |
| `native/macula_quic/src/*.rs` | none — primitives already existed, confirmed | N/A |

**Phase 2 (content transfer) — not started:**

| File | Change | Status |
|---|---|---|
| `src/macula_stream.erl` | untouched by Phase 1; content transfer may or may not need a change here depending on how `put_content`/`get_content` end up wired — re-evaluate at Phase 2 design time rather than carrying forward Phase 1's original (wrong) guess | Not started |
| `src/macula.erl` | `put_content/2`, `get_content/2`, `put_chunks/4`, `get_chunks/5` open a dedicated stream for the transfer | Not started |
| `macula-station` peer_observer | content transfer likely relays through the station the same way streaming RPC turned out to — budget for that scope from the start this time | Not started |
| `docs/guides/STREAMING_GUIDE.md` | streaming RPC claim is now true; content-transfer claim still false until Phase 2 ships | Partially blocked |
| `macula-comm-docs` forum post | same | Partially blocked |

## Testing plan

**Phase 1 — done:**
- `macula_station_link_tests.erl`: streaming session frames travel on a distinct mocked
  dedicated-stream reference, not the connection's shared control stream.
- The convincing proof, live against a real station cluster, no mocks:
  `macula-station/apps/macula_station/test/macula_station_call_stream_station_SUITE.erl`
  — both `stream_station_routed_control/1` (both endpoints seeded to the same station,
  the case that exercises cross-connection relay) and
  `stream_dials_outside_seed_set/1` open a `server_stream`, push three chunks, and
  assert the consumer receives all three in order over a real QUIC connection. This is
  what caught the `async_accept_stream` bug — the eunit mocks couldn't have, since they
  don't exercise real Rust-side stream acceptance.
- Regression bar held throughout: `macula` `rebar3 eunit` 1833/1833, `dialyzer` clean;
  `macula-station` `rebar3 eunit` 1146/1146, `rebar3 ct` 55/55 (8 pre-existing
  documented skips), `dialyzer` clean. Every claimed-clean baseline was verified by
  `git stash` A/B comparison against the pre-change tree, not assumed.
- Not yet done, and still the sharper version of the claim: open a streaming session
  that stalls or sends slowly, plus a concurrent unary RPC call on the same connection,
  and demonstrate the RPC reply is *not* delayed by the stalled stream. The current
  tests prove isolation exists (separate streams, separate resources); they don't yet
  prove head-of-line blocking is actually gone under contention.

**Phase 2 — not started.**

## Success criteria

- [x] Streaming RPC genuinely rides its own dedicated QUIC stream per session, both
      SDK-side and station-relay-side, verified live (not mocked) end to end.
- [ ] `STREAMING_GUIDE.md`'s and the forum post's "own QUIC stream" / "per-stream flow
      control" claims are fully true (blocked on Phase 2 for the content-transfer half)
      and a test proves it, not just restates it.
- [ ] A slow/stalled streaming session does not delay a concurrent unary RPC reply on
      the same connection, demonstrated live (see Testing — not yet attempted).
- [ ] Content transfer isolated from RPC/PubSub traffic the same way (Phase 2).
- [x] `rebar3 eunit`, `rebar3 dialyzer` clean on both `macula` and `macula-station` for
      Phase 1's scope, each verified against a stashed pre-change baseline.
- [ ] `rebar3 ex_doc` clean — `macula` is; `macula-station` currently fails on a
      pre-existing, unrelated `hecate_overlay_view.erl` EDoc XML error that blocks the
      whole run regardless of this work. Not caused by Phase 1; needs fixing
      separately before this box can be checked for the station repo.
