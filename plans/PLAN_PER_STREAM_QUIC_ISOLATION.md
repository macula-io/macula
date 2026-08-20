# Per-Stream QUIC Isolation

**Status:** Phase 1 (streaming RPC) COMPLETE. Phase 2 (content transfer) COMPLETE.
Remaining: update `STREAMING_GUIDE.md` + forum post; the sharper live head-of-line
demonstration; the pre-existing `macula-station` `ex_doc` blocker.
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

### Phase 2 — content transfer gets a dedicated stream per transfer (SHIPPED)

**Content transfer is architecturally different from streaming RPC, discovered before
writing any code this time (see the "scope correction" note at the top for how Phase 1
learned this lesson the expensive way).** `put_content`/`get_content` are not a
STREAM_OPEN session — they're a sequence of ordinary unary CALL/RESULT pairs (one per
block, plus one for the manifest on chunked content), issued via
`macula_client:call/5`, which independently picks a healthy pool link per call
(`call_first_success/5`). Content procedures are also never remote-advertised: every
station serves `_content.*` from its own local store (`macula_handler_registry`, not
`remote_advertise_registry`), so — unlike streaming RPC — there is no cross-connection
relay leg for the daemon-facing side at all.

**Design decision (asked, not assumed):** "one dedicated stream per transfer" only
means something if every call in that transfer's sequence rides the *same* stream on
the *same* link — which means pinning one link up front for the whole transfer,
instead of letting the pool freely re-pick per chunk the way it used to. The
alternative (a fresh dedicated stream per individual block/manifest CALL, keeping
today's per-chunk link-hopping) was rejected: it only isolates each call
individually, not the transfer as a whole, and costs one extra stream-open
round-trip per chunk. Link-pinning was chosen.

**New primitive, `macula_client.erl`:** `pick_connected_link/1` — returns one
currently-connected link pid without issuing a call, for a caller (content transfer)
that needs to pin one link across a sequence of related calls. `call/5` remains the
right choice for an ordinary one-off CALL and is unaffected.

**New primitive, `macula_station_link.erl`** (mirrors the Phase 1 SDK shape):
`open_content_stream/1` (opens a dedicated stream via `macula_peering:
open_dedicated_stream/1`), `call_on_stream/6` (sends a CALL on that stream, blocks for
RESULT/ERROR on the *same* stream — no `call_id` correlation needed, since a content
stream only ever has one outstanding call at a time by construction), and
`close_content_stream/2`. New state: `content_stream_bufs`, `content_pending`
(keyed by stream reference, not call id).

**`macula.erl`:** `put_content`/`get_content` (both single-block and chunked) now
call `with_content_stream/2`, which picks one link, opens one content stream, runs the
whole transfer's block+manifest calls through it via a rebuilt `call_on_stream_with_retry`
(replacing the old pool-routed `call_with_retry`, now dead and deleted), and closes the
stream when done. Per-chunk retry semantics are unchanged in spirit — a BOLT#4
`same_path_after_backoff` error retries the same CALL — just resent on the pinned
stream/link instead of possibly hopping to a different one.

**Station-side answering, `macula_station_peer_observer.erl`:** the existing
dedicated-stream dispatch (`dispatch_dedicated_frame/3`, built for STREAM_OPEN in
Phase 1) gained a `call` frame-type clause. Unlike STREAM_OPEN, a content stream is
never "routed" to another connection — every frame arriving on it (first one and every
one after, since the stream is reused across the whole transfer) is looked up in the
LOCAL `handler_registry` and replied to on that same stream. No new state needed;
`macula_handler_dispatch:dispatch_call/3` already produces a well-formed reply
(including the clean `unknown_next_peer` case) so a stray non-content CALL routed
through a dedicated stream fails cleanly rather than hanging.

**The scope this plan's Phase-1-era guess undersold, found the same way as the
`async_accept_stream` bug — by testing live, not by reading code:** a station's
station-to-station traffic (`macula_station_content_handlers.erl`'s eager block
replication on put, iterative multi-hop fanout on get) *also* calls `_content.put_block`
/ `_content.get_block`, but via `macula_station_outbound_link.erl` — a **separate,
parallel reimplementation** of the SDK's station-link protocol used specifically for
station-to-station connections (its own gen_server, own state, own `{call, ...}`
handler — documented in its own moduledoc as deliberately mirroring the SDK's surface
so callers holding an outbound-link pid can "drive pubsub + RPC exactly like an SDK
`macula_station_link` client"). It shares no code with `macula_station_link.erl`, so
Phase 1 and the first half of Phase 2 never touched it. `macula_station_outbound_link.erl`
now carries its own `open_content_stream/1`, `call_on_stream/6`, `close_content_stream/2`
— a straight mirror of the SDK implementation, using its own `conn_pid`/`identity`
fields. `macula_station_content_handlers.erl`'s `safe_replicate/3` and `safe_call/2`
were rewired onto the new primitive (via `safe_content_call/4`); calling
`macula_station_link:open_content_stream/1` against an `outbound_link` pid still works
by construction (both modules send the identical gen_server message shapes, matching
how the pre-existing `macula_station_link:call/5` already worked against outbound-link
pids before this change).

**A second real, pre-existing bug found writing the live test for the above (not
caused by this work, never previously exercised):**
`macula_station_peer_links:parse_host_port/1` crashed on an IPv6-bracketed URL
(`quic://[::1]:36422` — exactly what the test harness's loopback dials produce) via
`binary:split/2` cutting inside the address before the brackets were recognised,
taking down the entire `macula_station_peer_links` registry (not just the one entry)
on every crash. Production `outbound_peers` config is always DNS hostnames, and no
prior test both used an IPv6-bracketed dial *and* depended on `peer_links` succeeding,
so this shipped unnoticed. Fixed at the source in `macula_station_peer_links.erl`,
with two new eunit cases (`register_accepts_bracketed_ipv6_url_test_`,
`register_accepts_bare_bracketed_ipv6_url_test_`).

Chunks within one transfer stay sequential — that's a throughput question, not an
isolation question, and remains explicitly deferred (see Non-goals).

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

**Phase 2 (content transfer) — SHIPPED:**

| File | Change | Status |
|---|---|---|
| `macula` `src/client/macula_client.erl` | new export `pick_connected_link/1` + `handle_call(pick_connected_link, ...)` | Done |
| `macula` `src/client/macula_station_link.erl` | `open_content_stream/1`, `call_on_stream/6`, `close_content_stream/2`; new `content_stream_bufs`/`content_pending` state; `fail_all_pending/2` closes content streams on disconnect too | Done |
| `macula` `src/macula.erl` | `put_content`/`get_content` (single-block and chunked) pin one link + one dedicated stream for the whole transfer via `with_content_stream/2`; old pool-routed `call_with_retry` deleted (dead — every call site moved) | Done |
| `macula-station` `apps/macula_station/src/macula_station_peer_observer.erl` | `dispatch_dedicated_frame/3` gained a `call` frame-type clause — local handler dispatch, reply on the same stream, no cross-connection relay (unlike STREAM_OPEN) | Done |
| `macula-station` `apps/macula_station/src/macula_station_outbound_link.erl` | mirrors the SDK's three new functions + state, for the station-to-station calling leg — a separate module the SDK changes never reached (see design note above) | Done |
| `macula-station` `apps/macula_station/src/macula_station_content_handlers.erl` | `safe_replicate/3`, `safe_call/2` rewired onto `safe_content_call/4` (open + call + close a dedicated stream) instead of the old shared-stream `macula_station_link:call/5` | Done |
| `macula-station` `apps/macula_station/src/macula_station_peer_links.erl` | bug fix: `parse_host_port/1` no longer crashes the whole registry on an IPv6-bracketed URL (see design note above) — unrelated to dedicated streams but found while testing them live | Done |
| `macula-station` test files | `macula_station_content_SUITE.erl` gained two live station-to-station tests; `macula_station_peer_links_tests.erl` gained two IPv6-URL regression tests | Done |
| `docs/guides/STREAMING_GUIDE.md` | streaming RPC AND content transfer claims are now both true | Not yet updated |
| `macula-comm-docs` forum post | same | Not yet updated |

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

**Phase 2 — done:**
- `macula_station_content_SUITE.erl` (macula-station), live against real stations, no
  mocks — 7 tests total, all passing:
  - The 5 pre-existing daemon-facing tests (single-block, chunked, reassembly order,
    empty content, announcement) continued to pass unmodified, confirming
    `put_content`/`get_content`'s public behavior is unchanged even though every call
    underneath now rides a pinned dedicated stream instead of the shared one.
  - Two new tests for the station-to-station leg:
    `eager_replication_lands_on_peer_station` (2-station cluster; content put via A
    lands on B's own local store purely through eager replication, proving
    `safe_replicate/3`'s dedicated-stream CALL) and
    `iterative_get_reaches_two_hop_peer` (3-station chain A-B-C, A/C not peered;
    content put via A, fetched via C, which must iteratively fan out to its own peer
    B — proving `safe_call/2`'s dedicated-stream CALL on the get side, a genuinely
    different code path from the put-side test).
  - These two tests are what surfaced BOTH the `macula_station_outbound_link.erl` gap
    and the `parse_host_port/1` bug — via `{error, not_found}` and a crash report,
    respectively, not by inspection.
- Regression bar held: `macula` `rebar3 eunit` 1833/1833, `dialyzer` clean;
  `macula-station` `rebar3 eunit` 1148/1148 (2 more than Phase 1's count, from the new
  `peer_links` regression tests), `rebar3 ct` 57/57 (8 pre-existing documented skips,
  2 more tests than Phase 1's count), `dialyzer` clean.

## Success criteria

- [x] Streaming RPC genuinely rides its own dedicated QUIC stream per session, both
      SDK-side and station-relay-side, verified live (not mocked) end to end.
- [x] Content transfer genuinely rides one dedicated QUIC stream per whole transfer —
      daemon-facing AND station-to-station — verified live (not mocked) end to end,
      both the eager-replicate (put) and iterative-fanout (get) code paths.
- [x] `STREAMING_GUIDE.md`'s and the forum post's "own QUIC stream" / "per-stream flow
      control" claims are now fully true in the implementation. The docs themselves
      have not been edited yet — tracked below, not blocked on anything further.
- [ ] A slow/stalled streaming session does not delay a concurrent unary RPC reply on
      the same connection, demonstrated live. Still not attempted for either phase —
      the sharper, not-yet-tested version of the isolation claim.
- [x] `rebar3 eunit`, `rebar3 dialyzer` clean on both `macula` and `macula-station` for
      both phases' scope, each verified against a stashed pre-change baseline.
- [ ] `rebar3 ex_doc` clean — `macula` is; `macula-station` still fails on the same
      pre-existing, unrelated `hecate_overlay_view.erl` EDoc XML error from Phase 1,
      untouched by Phase 2 either. Needs fixing separately before this box can be
      checked for the station repo.
- [ ] `docs/guides/STREAMING_GUIDE.md` and the `macula-comm-docs` forum post updated
      to state the now-true claims plainly, and drop the caveats/TODOs that described
      the false state. Not started.
