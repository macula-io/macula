# Content Push/Upload (`macula_pusher` / `macula_upload`)

**Status:** Planning. Nothing implemented yet — this document is the output of a design
conversation, not a progress report.
**Created:** 2026-08-21
**Last Updated:** 2026-08-21

## Why this exists

So a Hecate service (or any macula consumer) can push a file at a specific peer, with
the same integrity guarantees `macula_feeder`/`macula_download` already give pull-based
transfers, without every future push-a-file-at-someone use case hand-rolling
chunking/hashing/cancellation against the raw streaming API itself.

## How this started

A session doing an unrelated fix (persistent `puzzle_enforcement` config in
macula-station, then tracing why `macula-realm`'s fleet dashboard went dark) ended by
publishing macula 9.8.2 and bumping macula-station + macula-realm to it. That prompted
the question this plan answers: given the SDK now has four complete, symmetric
supervised primitive pairs — pubsub (`macula_publisher`/`macula_subscriber`), content
sharing (`macula_feeder`/`macula_download`), RPC (`macula_request`/`macula_response`),
streaming RPC (`macula_streamer`/`macula_stream_sink`) — is there a real gap, or are
`macula_pusher`/`macula_upload` just other names for something that already exists?

Verified from source: `macula_feeder` is the *pull-served* upload side — CHANGELOG.md
itself glosses `macula_feeder:start_link_direct/5,6` as "Direct-dial for content
upload" — but it puts content into the mesh's content-addressed store for a downloader
to discover and pull later. There is no primitive for a sender to actively push a blob
at a specific, already-known recipient without that recipient first doing a DHT lookup.
`STREAMING_GUIDE.md` names `client_stream` mode ("consumer pushes many chunks, provider
reads") as the mechanism for exactly this ("an upload, a batch submit") — so the wire
capability exists, but nothing wraps it with `macula_feeder`/`macula_download`'s
integrity machinery (MCID, BLAKE3, manifest/Merkle chunking). Building that wrapping is
this plan's actual deliverable.

## Scope decisions made during design (read before touching any of this)

- **No backward-compatibility constraint anywhere in this plan.** Every consumer
  (macula-station, macula-realm, hecate-om) is ours, watchtower/hex-bump rolls them
  together, `rebar.lock`/`mix.lock` are both deliberately gitignored so nothing floats
  to a stale pin. `put_content/2`/`get_content/2` are free to change shape entirely —
  they do not need a blocking-compatible wrapper preserved around the new addressable
  primitive. Update every real caller in the same wave, same as the 9.8.1→9.8.2 bump.
- **Multi-stream parallel chunk transfer is a content-sharing-only concern.** It does
  NOT extend to `macula_streamer`/`macula_stream_sink`. Content sharing moves a static,
  fully-known-upfront blob whose manifest already gives every chunk an explicit
  `index`/`offset`/`hash` — chunks are positionally addressable and order-independent,
  exactly the shape multi-stream/parallel transfer exploits. Streaming RPC's chunks
  (`macula_stream:send/recv`) are anonymous, no index at all, because for its real use
  cases (log tail, query-yields-rows, batch upload) order is the entire point — losing
  or reordering a chunk corrupts the result. Do not "fix" this by adding an index; it's
  the correct shape for what streaming RPC is actually for.
- **Streaming RPC probably doesn't need an explicit pause primitive.** `macula_stream`
  rides a QUIC *stream* (reliable, ordered, retransmission not gaps — verified: the SDK
  exposes no application-level unreliable-datagram API, `macula_quic:max_datagram_size/1`
  is MTU introspection only, not a send/recv path). A consumer that simply stops calling
  `recv/1,2` already backpressures the sender via QUIC's own flow control — pause may be
  an application-level buffer-management concern, not something needing new SDK
  machinery. Re-litigate only if a concrete need surfaces; don't build it speculatively.
- **Genuine loss-tolerant real-time media transport is explicitly out of scope.** It
  would need QUIC DATAGRAM frames (RFC 9221) exposed as an application-facing API — new
  Rust NIF work, a different transport primitive entirely, not a tweak to
  `macula_streamer`. Don't let "but what about video" pull scope into this plan; it's a
  separate, currently-nonexistent capability.
- **Mesh fact topics stay `sharing.*`/`streaming.*`/`rpc.*`/`pubsub.*` — no leading
  underscore.** `_mesh.*` is not cosmetically "internal," it is wire-behaviorally
  special-cased: `macula_station_route_pubsub_frames:bloom_fan_extras/4` (in the
  **macula-station** repo, not this one) explicitly skips bloom-fan relay for any
  `<<"_mesh.", _/binary>>` topic, because those are continuously-refreshed
  infrastructure heartbeats (health, presence, bloom exchange) where direct-links-only
  reach is an acceptable, intentional tradeoff — the next beacon is seconds away.
  `sharing.*`/`rpc.*`/`streaming.*`/`pubsub.*` facts are one-shot lifecycle events
  ("this transfer started," "this call completed") with no next-beacon fallback; they
  need full bloom-fan mesh-wide relay to reach a subscriber that isn't directly
  connected to the station handling the transfer. Renaming them to `_sharing.*` etc.
  would silently degrade them to direct-links-only reach — the same failure mode that
  cost `macula-realm`'s fleet dashboard an hour earlier this same session
  (`_mesh.health.v1` never registering as a subscriber anywhere). Do not rename these.

## Architecture before this work (verified 2026-08-21)

Read directly from `src/macula.erl`, `src/macula_feeder.erl`, `src/macula_download.erl`,
`src/macula_streamer.erl`, `src/macula_stream_sink.erl`, `src/content/macula_manifest.erl`:

- `put_content/2`/`get_content/2` are ONE blocking call each: pick a link
  (`macula_client:pick_connected_link/1`), open ONE dedicated content stream for the
  whole transfer (`with_content_stream/2`), run the transfer to completion, close the
  stream, return `{ok, _} | {error, _}`. No addressable handle exists mid-transfer.
- Chunked transfers are strictly sequential: `put_chunks/5` is a tight recursive loop —
  put chunk N, block on the reply, only then put chunk N+1. No parallelism across
  chunks, no parallelism across providers (a download stays pinned to whichever single
  link it picked at the start, even though `find_content_providers/2` can return
  several).
- `macula_feeder`/`macula_download`'s `cancel/1` is `gen_server:stop/1`, whose
  `terminate/2` does `exit(Worker, kill)` — a blunt local kill. `macula_stream:abort/3`
  exists (an explicit, peer-visible abort with a reason code) but nothing in either
  module's cancel path calls it. The peer currently learns about a cancellation only by
  the connection going away, not by an explicit signal.
- `macula_manifest:create/2`, `:verify/2`, `:chunk_mcid/3`, `:from_wire/1` are pure,
  transport-agnostic functions — no dependency on the content-store RPCs
  (`_content.put_block`/`_content.put_manifest`/`_content.get_manifest`). Fully reusable
  for a push-initiated transfer without touching them.
- `macula_streamer` (provider side of streaming RPC) has NO receive-loop for
  `client_stream` mode. It only wraps `send/2,3`/`close/1` (fits `server_stream`,
  provider-pushes). `macula_stream_sink` (consumer side) already has the mirror-image
  pattern — a linked reader process driving `recv/2` in a loop, delivering
  `Module:handle_chunk/2` — `macula_streamer` never got the equivalent for a provider
  that needs to *receive* pushed chunks, because nothing needed it until now.
- `call_stream/5`'s `Args` parameter already is an out-of-band, open-time metadata
  channel — it arrives at the provider's `Module:handle_open(StreamArgs, State)` before
  any chunk. This is the channel a manifest should ride on for a push transfer; no new
  in-band "first chunk is secretly a header" framing needs inventing.

## Target architecture — phases

Each phase ships independently: full RED/GREEN on new tests, full `rebar3 eunit`, `rebar3
xref`, `rebar3 dialyzer`, CHANGELOG entry + version bump, before starting the next phase.

### Phase 1 — Addressable content transfer + real abort-based cancel

New module `macula_content_transfer` (`gen_server`, matching this codebase's existing
convention over `gen_statem` — every sibling primitive, `macula_feeder` through
`macula_stream_sink`, is a plain `gen_server`). Owns the picked link and the open
content stream directly (unlike today, where the stream is opened and closed entirely
inside the blocking worker call and never surfaced to a supervisor).

- `start_put(Pool, Bytes, Opts) -> {ok, Pid}` / `start_put_station/station-direct
  variant` / `start_get(Pool, Mcid, Opts) -> {ok, Pid}` / direct-dial variants —
  returns immediately, holding the pid gives you the live handle.
- `cancel(Pid, Code, Message)` calls `macula_stream:abort(Stream, Code, Message)` on
  the actual open stream before tearing the process down — the peer gets an explicit
  signal, not a dropped connection to infer from.
- `put_content/2`/`get_content/2` (and the `_station` variants) become — per the "no
  backward-compat constraint" scope decision above — `start_*` followed by a blocking
  `await/1,2`, OR are removed entirely in favor of always returning the pid, whichever
  reads cleaner once written. Update every real caller (macula-station's tests,
  `macula_feeder`/`macula_download`'s own worker spawn, anywhere else `put_content`/
  `get_content` is called) in the same commit wave.
- Correlation-id control registry: each transfer already mints a `share_id`
  (`crypto:strong_rand_bytes(16)`, already published in `sharing.put_started_v1` etc.)
  — add a small ETS-backed registry (`{share_id, pid}`, monitor-based cleanup on the
  owning process's exit, same idiom as `macula_station_peer_links` in the
  **macula-station** repo) so `cancel/pause/resume` are reachable by id, not just by
  pid, for a caller that only knows the id from a published mesh fact.

### Phase 2 — Pause/resume (content-sharing only)

Convert the chunk loop from a tight recursive function into a step the
`macula_content_transfer` process re-triggers itself between chunks (e.g. via
`handle_continue/2`), checking a `paused` flag before each step. `pause/1`/`resume/1`
become real: pause stops the loop between chunks without closing the stream; resume
re-triggers it from the next un-sent/un-fetched chunk. One chunk's own round-trip stays
a single blocking call underneath — the *loop between chunks* becomes controllable, not
each chunk's own transfer.

### Phase 3 — Multi-stream parallel chunk transfer (content-sharing only)

Split the chunk list across N dedicated content streams on the same link instead of one
sequential stream — round-robin or range-based distribution, each stream tracked to
completion independently, manifest put/verify only once every stream finishes. For
downloads, consider (separate, larger sub-step — may warrant its own follow-up plan)
resolving multiple `find_content_providers/2` results and splitting fetch across
distinct providers, not just distinct streams to one provider.

### Phase 4 — Retrofit `macula_feeder`/`macula_download`

Same public behaviour contract (`init/1`, `handle_fed/2` / `handle_downloaded/2`
unchanged) — internals now drive `macula_content_transfer` instead of spawning a
worker around the old blocking call. Existing consumers of the *behaviour* (not the
removed blocking functions) see no API change, just real pause/resume/cancel and
multi-stream underneath.

### Phase 5 — `macula_streamer` client_stream receive-loop

Add an optional `handle_chunk/2` callback (mirroring `macula_stream_sink`'s exactly).
When a `client_stream`-mode provider module exports it, spawn the same linked-reader
loop `macula_stream_sink` already has, applied to the provider side instead of the
consumer side. Backward compatible: `server_stream`-mode users who don't export
`handle_chunk/2` are unaffected. Also wire `macula_stream:abort/3` into
`macula_streamer`/`macula_stream_sink`'s existing cancel paths (same reasoning as
Phase 1, applies here too — this pair currently has the same blunt-kill cancel).
No pause work here per the scope decision above unless a concrete need surfaces first.

### Phase 6 — `macula_pusher` / `macula_upload`

Built on the now-complete foundation — inherits addressable pause/resume/cancel and
multi-stream from day one, nothing to retrofit later.

- **`macula_pusher`** (sender): `macula_manifest:create/2` to chunk+hash, opens a
  `client_stream` to the target's advertised upload procedure with the manifest passed
  as `Args` (the open-time out-of-band channel, not an in-band header chunk), sends
  chunks via the Phase 3 multi-stream engine, delivers `{ok, Mcid} | {error, _}` to
  `handle_pushed/2`. `start_link`/`start_link_direct`, mirroring `macula_feeder`'s
  shape exactly. Publishes `sharing.push_started_v1`/`sharing.push_completed_v1`.
- **`macula_upload`** (receiver): advertises a `client_stream` procedure (built on
  Phase 5's now-complete receive-loop), reads the manifest from `StreamArgs` at open,
  accumulates chunks, reassembles, verifies with `macula_manifest:verify/2` —
  receiver-side verification, never sender-trusted, matching content-sharing's existing
  "content is self-verifying by hash" model exactly. Delivers
  `{ok, Mcid, Bytes} | {error, _}` to `handle_uploaded/2`. `advertise`/`advertise_direct`,
  mirroring `macula_download`'s shape. Publishes
  `sharing.upload_started_v1`/`sharing.upload_completed_v1`.

## Files likely to change (all in this repo unless noted)

| File | Change |
|---|---|
| `src/macula_content_transfer.erl` | New — Phases 1-3 |
| `src/macula.erl` | `put_content`/`get_content` (+ `_station` variants) reshaped onto `macula_content_transfer` |
| `src/macula_feeder.erl` | Phase 4 retrofit |
| `src/macula_download.erl` | Phase 4 retrofit |
| `src/macula_streamer.erl` | Phase 5 — `handle_chunk/2`, abort-wired cancel |
| `src/macula_stream_sink.erl` | Phase 5 — abort-wired cancel |
| `src/macula_pusher.erl` | New — Phase 6 |
| `src/macula_upload.erl` | New — Phase 6 |
| `test/macula_content_transfer_tests.erl` | New |
| `test/macula_feeder_tests.erl`, `macula_download_tests.erl` | Updated for retrofit |
| `test/macula_streamer_tests.erl`, `macula_stream_sink_tests.erl` | Updated for abort + receive-loop |
| `test/macula_pusher_tests.erl`, `macula_upload_tests.erl` | New |
| `CHANGELOG.md`, `macula.app.src`, `CLAUDE.md` (version header) | Bumped per phase |
| `docs/guides/CONTENT_GUIDE.md`, `STREAMING_GUIDE.md` | Updated once the shipping phases land |
| `macula-station` (separate repo) | Any test/consumer callers of the old blocking `put_content`/`get_content` shape |
| `macula-realm`, `hecate-om` (separate repos) | Only if either turns out to call `put_content`/`get_content` directly — check before Phase 1 lands |

## Testing plan

Per phase: RED (new tests fail against pre-phase code, via `git stash` on the phase's
source changes) → GREEN (fix restored) → full `rebar3 eunit` → `rebar3 xref` →
`rebar3 dialyzer` — all must be clean, matching the discipline already established this
session for the puzzle-hardening and mesh-identity fixes. No phase lands with the
"cancelled tests" eunit quirk unexplained (confirmed pre-existing and unrelated to any
of this work, but re-verify per phase rather than assuming).

## Success criteria

- [ ] Phase 1: `macula_content_transfer` addressable, `cancel/3` provably calls
      `macula_stream:abort/3` (test asserts the peer sees an abort frame, not just a
      closed connection).
- [ ] Phase 2: a paused transfer sends zero further chunks until resumed; resume
      continues from the correct next chunk, not from the start.
- [ ] Phase 3: a large chunked transfer measurably completes faster with N>1 streams
      than with 1, and reassembly still verifies correctly regardless of which stream
      delivers which chunk first.
- [ ] Phase 4: `macula_feeder`/`macula_download`'s existing behaviour tests pass
      unmodified against the new internals (same callback contract).
- [ ] Phase 5: a `client_stream`-mode provider using `handle_chunk/2` receives every
      chunk a consumer sends, in order, with no hand-rolled `recv` loop in application
      code.
- [ ] Phase 6: a `macula_pusher` push to a `macula_upload` receiver whose received bytes
      are tampered with in transit (test-injected corruption) is caught by
      `macula_manifest:verify/2` and reported as a failure, never silently accepted.
