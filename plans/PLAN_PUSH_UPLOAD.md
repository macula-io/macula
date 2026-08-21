# Content Push/Upload (`macula_pusher` / `macula_upload`)

**Status:** Phases 1-5 SHIPPED (macula 9.12.0). Phase 6 not started.
**Created:** 2026-08-21
**Last Updated:** 2026-08-21 (Phases 1 through 5 all landed same day, in five
follow-up sessions to the one that wrote this plan)

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
  - **"But what about media streaming" was raised and is worth writing down so it
    doesn't get re-litigated:** it's tempting to think live video/audio justifies
    loss-tolerant, parallelizable, reorderable delivery for `macula_streamer`/
    `macula_stream_sink` — real RTP/WebRTC-style media transport does work that way.
    But `macula_stream` rides a QUIC *stream*, and QUIC streams are reliable and
    ordered by the protocol itself (the same guarantee class as a single TCP
    connection) — a "lost" packet triggers retransmission, never a gap the
    application sees. There is no glitch-and-move-on available at this layer; either
    the byte arrives, in order, or the stream/connection dies outright. So "media
    streaming" was actually a bad example to reach for either direction of this
    argument — as this SDK is built today, streaming a video over
    `macula_streamer`/`macula_stream_sink` would behave like piping it through TCP,
    not like RTP. The more useful, generalizable question this exchange surfaced:
    *does this workload need strict order and guaranteed delivery of every event,
    with potentially more than one interested consumer?* When yes, and the data
    represents a standalone fact rather than a private session, that's **pubsub's**
    job, not streaming RPC's — pubsub already has `macula_pubsub_order` (a
    per-publisher FIFO reorder buffer) precisely because its substrate is
    best-effort multi-hop relay and ordering gets layered back on top for
    consumers that want it, with multi-subscriber fan-out built in from the start.
    Streaming RPC's honest remaining niche, once "live lossy media" is taken out of
    it (because it can't do that anyway, today): point-to-point, session-scoped,
    reliable, ordered continuous data tied to one connection (a log tail for one
    debugging client, a query's rows for one caller, a batch upload for one
    transfer) — pubsub's mesh-wide relay/dedup overhead would be the wrong shape for
    something inherently private to one session. If genuine loss-tolerant real-time
    media transport is ever wanted, see the out-of-scope note below — it needs a
    transport capability this SDK doesn't have, not a change to how streaming RPC's
    existing chunks are ordered.
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

## Related finding, deliberately out of scope here

The design conversation that produced this plan started from a broader question: given
all four primitive pairs are now complete, does **hecate-om** (the shared Hecate service
scaffold, separate repo `hecate-services/hecate-om`) need extending to use them? Answer:
no, not structurally — `hecate_om:macula_client/0` already hands any service the raw
pool, and every behaviour module here just takes a `Pool`, so a service can call
`macula_subscriber:start_link/5` or `macula_feeder:start_link_direct/5` against it today
with zero hecate-om changes.

One real thing surfaced along the way, worth keeping so it isn't lost, but genuinely a
separate piece of work in a separate repo: `hecate_om_capabilities:call_capability/7`
(hecate-om's *one* existing RPC convenience — resolve providers from the DHT, verify
cert chains, resolve the serving station's endpoint, `macula:call_station/7`, manual
provider failover) is a hand-rolled, blocking, non-cancellable direct-dial
implementation that predates, and duplicates in spirit, what
`macula_request:start_link_direct/6,7` + `macula_response:advertise_direct/6,7` now
give for free — supervised, cancellable (once Phase 1's abort-based cancel exists),
and observable via `rpc.sent_v1`/`rpc.completed_v1` mesh facts. It carries real
hecate-specific logic worth preserving (cert-chain verification, UCAN gating,
multi-provider failover), so refactoring it onto `macula_request` is a genuine
redesign, not a drop-in swap — and it's not blocking anything here. Flagging it as a
candidate follow-up plan in `hecate-services/hecate-om/plans/`, not a phase of this one.

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

### Phase 1 — Addressable content transfer + real abort-based cancel — SHIPPED (9.9.0)

New module `macula_content_transfer` (`gen_server`, matching this codebase's existing
convention over `gen_statem` — every sibling primitive, `macula_feeder` through
`macula_stream_sink`, is a plain `gen_server`). Owns the picked link and the open
content stream directly (unlike before, where the stream was opened and closed
entirely inside the blocking worker call and never surfaced to a supervisor — that
gap was real: killing the blocking caller never ran `close_content_stream`, so
`content_stream_bufs`/`content_pending` leaked on the link until the eventual
`content_call_timeout` fired against an already-dead caller).

- `start_put/2,3`, `start_put_station/4,5`, `start_get/2,3`, `start_get_station/4,5`
  return `{ok, Pid}` immediately — holding the pid gives you the live handle. The
  connect (pick/dial link, open stream) and the transfer itself both run in a linked
  worker, which reports the opened `{LinkPid, Stream}` back to the gen_server as soon
  as it has them — so the gen_server stays free to answer `cancel` throughout, even
  while the worker is blocked mid-connect or mid-transfer.
- **Correction from the original design above, found only once actually building this:
  `cancel/3` does NOT call `macula_stream:abort/3`.** That targets a `macula_stream`
  gen_server's own STREAM_ERROR application framing (used by `macula_streamer`/
  `macula_stream_sink`, streaming RPC's pair). A content-transfer stream is a
  completely different thing — a raw QUIC dedicated stream (a `reference()` from
  `macula_station_link:open_content_stream/1`), wired for exactly two frame types
  (`result`/`error`, plain CALL/RESPONSE — verified in `dispatch_content_frame/3`).
  There is no `macula_stream` process to call `abort/3` on. Verified further: no
  peer-visible abort existed anywhere for content streams before this phase, at any
  layer — `macula_quic:async_shutdown_stream/3` already had the right shape
  (`Stream, Flag, Code`) but was a stub that silently discarded `Code` and always did
  a graceful Quinn `finish()`, and the Rust recv loop collapsed every read error
  (finish or reset alike) into one undifferentiated `none` reason. Real work landed
  instead: a new Rust NIF, `nif_reset_stream` (Quinn's `SendStream::reset`), wired
  through a new `macula_quic:reset_stream/2`; the recv loop now distinguishes
  `Err(ReadError::Reset(Code))` and delivers `{quic, stream_closed, PeerStream,
  {reset, Code}}` — genuinely peer-visible at the QUIC transport level, no
  application-layer framing needed. `macula_station_link:abort_content_stream/4` is
  the new counterpart to `close_content_stream/2` that calls it. Verified RED→GREEN
  with a real two-endpoint loopback test (`macula_quic_stream_reset_tests`, three
  cases: reset delivers the code, a graceful close is never confused with a reset,
  an out-of-range code is rejected before touching the wire) — confirmed the specific
  assertion fails (`{wrong_reset_detail, none}`) against the pre-fix recv loop, not
  just reasoned about it.
- `put_content/2`/`get_content/2` (and the `_station` variants) are now thin blocking
  wrappers — `start_*` + `await/1` + `cancel/1` to reap — over `macula_content_transfer`,
  same public signature as before. Checked macula-station, macula-realm, and hecate-om
  for direct callers of the old blocking shape per the plan's own checklist: none
  found in any of the three, so nothing needed updating downstream.
- Correlation-id control registry: **shipped as designed** — new
  `macula_content_transfer_registry` (ETS-backed, `{share_id, pid}`, monitor-based
  cleanup on the owning process's exit), started under `macula_root`. Each transfer
  mints a `share_id` (`crypto:strong_rand_bytes(16)`, overridable via `Opts` so a
  future wrapper — e.g. Phase 4's retrofitted `macula_feeder` — can keep the same id
  it already publishes in `sharing.put_started_v1`) so `cancel/1,3` is reachable by
  id, not just by pid, for a caller that only knows the id from a published mesh
  fact. `pause`/`resume` are NOT part of Phase 1 — that machinery doesn't exist until
  Phase 2's chunk loop restructuring; the registry itself is already general enough
  to carry them once Phase 2 adds the calls.

**A genuinely dumb bug worth recording so it isn't repeated:** the worker was
originally spawned as `spawn_link(fun() -> run(self(), ...) end)` — `self()` evaluated
*inside* the closure, which runs in the **new** process, so `Parent` was the worker's
own pid, not the gen_server's. Every message the worker sent went nowhere anyone was
listening; `await/1` hung until the caller's own timeout. Classic Erlang closure
gotcha — capture `Self = self()` *before* `spawn_link`, pass `Self` in. Caught by the
RED-before-GREEN discipline: the content_transfer eunit suite hung instead of failing
cleanly, which was itself the tell that something more fundamental than a wrong
assertion was wrong.

### Phase 2 — Pause/resume (content-sharing only) — SHIPPED (9.10.0)

Shipped as designed, no scope corrections needed this time (unlike Phase 1's
abort mechanism). Converted the chunk loop from a tight recursive function
into a step `macula_content_transfer` (the gen_server itself) re-triggers via
`handle_continue/2`, checking a `paused` flag before each step. `pause/1`/
`resume/1` are real: pause stops the loop from advancing to the next chunk
without closing the stream — the chunk already in flight, if any, is NOT
interrupted (its own round trip stays one uninterrupted blocking call,
consistent with content's "verified whole or not at all" model — pausing
mid-chunk would leave a half-sent block the station can't verify); resume
re-triggers the loop from exactly the next un-sent/un-fetched chunk, never
from the start.

Went further than a bare flag flip: each chunk step (one `_content.put_block`/
`get_block`, or the manifest's `put_manifest`/`get_manifest`) now runs in its
own short-lived linked worker, spawned fresh by `dispatch_next_step/1` for
that one step and nothing more, instead of one long-lived worker running the
whole multi-chunk loop to completion. This is what makes `paused` actually
enforceable at every chunk boundary — the gen_server itself decides whether
to spawn the next step's worker, rather than a loop already running inside
one worker deciding for itself — and it keeps `cancel/3`'s existing
guarantee (kill whatever's in flight, reset the stream) granular to
whichever single chunk is currently in flight, not the whole transfer.

Found one real gap this introduced that Phase 1's cancel path never had to
handle: paused between chunks means NO step worker is alive at all
(`worker = undefined` in state) — the original `unlink(Worker), exit(Worker,
kill)` call would `badarg` on that. Fixed (`kill_worker/1` treats `undefined`
as nothing to kill) and verified RED before GREEN: reverting the fix
reproduces the exact `{badarg, [{erlang,unlink,[undefined]...` crash it
prevents, via a dedicated test
(`cancel_while_paused_between_chunks_still_resets_the_stream`).

Single-block put/get is completely untouched by this phase — still one
worker, connect through completion, exactly as Phase 1 shipped it. There is
no "between chunks" for a one-round-trip transfer to participate in, so
`pause/1` on one is a harmless no-op (verified:
`pause_on_single_block_put_is_a_harmless_noop`).

### Phase 3 — Multi-stream parallel chunk transfer (content-sharing only) — SHIPPED (9.11.0)

Shipped as designed: chunks split round-robin (`Index rem StreamCount`) across N
dedicated content streams on the same link, each stream tracked to completion
independently, manifest put/verify only once every stream finishes. `stream_count`
defaults to 4, always capped at the actual chunk count, overridable per transfer via
`Opts`.

Restructured the whole chunk-driving model to get there: Phase 2's single `#chunk`
record (one `remaining`/`next_index`/`acc` for the whole transfer) became a per-stream
`#lane{}` (own `remaining`/`in_flight`/`worker`), with `dispatch_lanes_or_finish/1`
starting a fresh short-lived worker for every IDLE lane that still has work — called
redundantly-but-idempotently after every single lane's own completion, which is what
lets N lanes advance independently without any of them needing to know about the
others. `pause`/`resume`/`cancel` generalize the same way: pause/resume gate every
lane through the identical `paused` check (no per-lane pause state needed — one flag,
checked uniformly); cancel kills every lane's in-flight worker and resets every open
stream, not just one.

A get doesn't know its chunk count — and therefore how many streams are worth opening
— until the manifest is back, so it necessarily starts on the ONE stream the connect
step already opened (fetching the manifest) and only expands to more once the count is
known. A put knows upfront (`macula_manifest:create/1` gives the full chunk list before
any network call) and opens every extra stream immediately. Opening an extra stream is
best-effort — verified via a dedicated test
(`stream_open_failure_degrades_to_fewer_streams`) that a failed extra-stream open
degrades to fewer streams rather than failing the transfer.

One stream's chunk genuinely failing (`{error, _}`, not a crash) fails the whole
transfer — every OTHER lane's in-flight worker gets killed and every stream gets reset
via the existing `finalize/2` path, verified with a dedicated test
(`single_failed_chunk_fails_whole_transfer_and_kills_other_lanes`, using monitors to
confirm the other lanes' workers are actually dead, not just reasoned to be).

Get's reassembly reads fetched chunks back out by INDEX (`#chunk.acc`, a map keyed by
chunk index), not arrival order — necessary because different lanes finish in whatever
order their own network calls happen to complete in. Verified with a dedicated test
that releases chunks in the REVERSE of their arrival order and confirms the reassembled
bytes still match exactly.

**Considered but explicitly deferred, not part of this phase:** the plan's original
text floated splitting a GET's fetch across multiple distinct PROVIDERS (via
`find_content_providers/2`), not just multiple streams to the one provider a download
is already pinned to. Still out of scope, still flagged as a separate, larger follow-up
if it's ever wanted — this phase's multi-stream work is entirely against one already-
resolved link, matching what "split the chunk list across N dedicated content streams
on the same link" in the phase's own original description asked for.

**A design question resolved by tracing it through rather than guessing:** could a lane
worker's `{lane_step_result, ...}` message arrive AFTER an internal failure (a
different lane) has already finalized the transfer — e.g. because that worker sent its
result and then got killed by `fail_chunked/3`'s cleanup in the same narrow window?
Yes, this is reachable. Traced the consequence rather than assuming it needed a guard:
the FAILED lane's own `#lane.worker` field is never cleared (nothing in the failure
path touches the failing lane's own state, only the OTHER lanes'), so `lane_done/1`
permanently reads `false` for it — which means `finish_if_all_lanes_done/1` can never
observe "all lanes done" again after a failure, regardless of what stray messages
arrive afterward. A stray success message still mutates `state.chunk` harmlessly (nothing
external ever reads it once `state.result` is set) and can trigger one extra harmless
`{continue, next_step}` that finds nothing new to do. No duplicate network calls, no
wrong result ever reaches `await/1,2`. Left unguarded — the self-limiting behavior is a
consequence of the state machine's own structure, not code relying on timing, and adding
a guard for a scenario that provably can't produce a wrong answer would be exactly the
kind of defensive code this project's own conventions warn against.

### Phase 4 — Retrofit `macula_feeder`/`macula_download` — SHIPPED (9.11.1)

Same public behaviour contract as planned (`init/1`, `handle_fed/2` /
`handle_downloaded/2` unchanged, same `start_link/4,5` and `start_link_direct/5,6`
/ `start_link_direct/4,5` signatures) — internals now call
`macula_content_transfer:start_put/3` (or `start_get/3`, `start_put_station/5`,
`start_get_station/5` for direct-dial) directly from a lightweight resolve + await
proxy, instead of spawning a worker around the old blocking `macula:put_content/2`/
`get_content/2` call.

**Correction from the plan's success criterion, found only once actually building
this — the SAME kind of mismatch Phase 1's design had, worth flagging the same way:**
"existing behaviour tests pass unmodified" turned out to be impossible to satisfy
literally, and shipping it as written would have meant NOT fixing the actual bug
Phase 4 exists to fix. Traced why: the old tests mocked `macula:put_content/2`/
`get_content/2` directly, passing a placeholder atom `pool` (not a real pid) as
`Pool` — safe only because the mock intercepted the call before any real guard ran.
Once the internals call `macula_content_transfer:start_put/3` directly, its own
`is_pid(Pool)` guard is real and reachable, so a placeholder atom `Pool` crashes with
`function_clause` — confirmed this is genuinely mechanical, not a matter of
interpretation, by running the OLD test files unmodified against the new internals
first: all 8 cases failed identically. Fixed by mocking at the `macula_client`/
`macula_station_link` boundary instead (the same layer `macula_content_transfer_tests`
already mocks) and using a real pid for `Pool` — the ASSERTIONS (`outcome`, `mcid`,
`chunked`, topics) stayed conceptually identical, only the mock target and `Pool`
shape moved to match what's actually being called now.

**The bug this phase actually fixes, more precisely than the plan's own text stated
it:** `macula_feeder`/`macula_download`'s pre-Phase-4 `cancel/1` (`gen_server:stop/1`)
could only ever kill their OWN local worker process — the one blocked inside
`macula:put_content/2`'s call to `macula_content_transfer:await/1`. Since nothing
links a `gen_server:call` caller's death to the callee it was calling, killing that
worker left the underlying `macula_content_transfer` completely unaffected: it kept
running to completion, or — once resolved — sat alive forever, since nothing ever
called `cancel/1` on IT specifically (that call was the very next line after `await`,
inside the same now-dead worker, never reached). A cancelled feed/download was
silently leaking an orphaned `macula_content_transfer` process (with its
`content_stream_bufs` entry on the link and its `macula_content_transfer_registry`
entry) every single time, not just occasionally. Fixed by having both modules hold
the `macula_content_transfer` pid directly in their own state (reported back by the
resolve+await proxy as soon as it's known) and calling `macula_content_transfer:
cancel/1` on it explicitly from `terminate/2` — verified RED before GREEN: neutering
that one call reproduces exactly the "no abort reaches the stream" gap the new
`cancel_reaches_the_real_content_transfer_not_just_the_local_worker` test exists to
catch, for both modules.

**Direct-dial got the same fix, extending slightly beyond the plan's literal text:**
`macula_content_transfer` (Phases 1-3) only has a "dial an already-resolved station"
primitive (`start_put_station`/`start_get_station`), not the "resolve a pubkey/MCID
into a dialable endpoint, then dial" two-step `macula_direct_dial:put_content/4`/
`get_content/3` do. Rather than leave direct-dial mode on the old blocking path (which
would have meant Phase 4 quietly not applying to half of `macula_feeder`/
`macula_download`'s surface), split the two steps: the resolve step
(`macula_direct_dial:resolve_station_endpoint/2` / `resolve_content_provider/2`, both
already public exports, reused as-is) stays a plain blocking DHT lookup inside the
proxy — nothing has ever needed to cancel mid-resolve, and moving it into `init/1`
would have changed `start_link_direct`'s own blocking-ness, a real behavior change
this phase has no reason to make — and only the actual transfer afterward goes
through the addressable primitive. No previous test coverage existed for direct-dial
at all (checked before assuming otherwise); added one case per module.

Each module's own `share_id` (already minted for its `sharing.*_started_v1` mesh
fact, published before the transfer even starts) is now passed through as
`macula_content_transfer`'s own `share_id` too — exactly the cross-referencing
Phase 1's registry design anticipated ("a wrapper that already publishes it... can
keep the same id"), so `macula_content_transfer_registry:whereis_share/1` resolves
to the same id these mesh facts already carry.

Checked (again) for direct callers of `macula_feeder`/`macula_download` in
macula-station, macula-realm, and hecate-om before touching internals: none found —
consistent with Phase 1's earlier check of `put_content`/`get_content` itself.

### Phase 5 — `macula_streamer` client_stream receive-loop — SHIPPED (9.12.0)

Shipped exactly as designed: `macula_streamer` gained an optional `handle_chunk/2`
callback (mirroring `macula_stream_sink`'s consumer-side one verbatim, including the
`spawn_reader/1`/`reader_loop/2`/`dispatch_recv/3` helpers) gated by
`erlang:function_exported(Module, handle_chunk, 2)` — the same mechanism
`macula_stream_sink`'s own optional `handle_close/2` already used, so a
`server_stream`-mode module that doesn't export it spawns no reader and is completely
unaffected. Considered threading an explicit `Mode` parameter through
`advertise/6` → `dispatch/8` → `start_link/7` instead, but the `function_exported`
gate makes it redundant — a `server_stream` module has no reason to export
`handle_chunk/2` either way, so checking mode explicitly would only duplicate what the
export check already guarantees; skipped the extra plumbing.

**Both `macula_streamer` and `macula_stream_sink`'s cancel paths now call
`macula_stream:abort/3` on any non-`normal` termination reason, close cleanly
(`macula_stream:close/1`) on `normal`.** Unlike Phase 1's cancel gap — where NO
peer-visible abort mechanism existed at all, requiring new Rust NIF work
(`nif_reset_stream`) before anything could be wired up — `macula_stream:abort/3`
already existed, fully implemented (a genuine `STREAM_ERROR` frame via
`macula_station_link:send_stream_frame/3`), and was already used elsewhere in the
codebase (`macula_stream_local`, `macula_station_link`'s handler-crash and
disconnect-teardown paths) with an established `Code`/`Message` convention this phase
reused (`<<"cancelled">>` code, `iolist_to_binary(io_lib:format("~p", [Reason]))`
message). So Phase 5's fix was pure wiring, no new transport-layer capability needed —
a much smaller phase than Phase 1's equivalent, precisely because the plan's own
"Architecture before this work" section had already verified `macula_stream:abort/3`
existed and just wasn't being called from either wrapper's `terminate/2`.

**Traced (not assumed) that `macula_streamer`'s bug was worse than `macula_stream_sink`'s.**
`macula_stream_sink:terminate/2` already called `macula:close_stream/1`
*unconditionally* pre-Phase-5 — so its bug was purely a signal-quality problem (a real
failure looked identical to a clean end-of-stream to the peer). `macula_streamer:terminate/2`
did not close or abort its underlying `macula_stream` AT ALL before this phase — traced
through `macula_stream`'s own `owner`/monitor lifecycle (`macula_station_link`'s
server-side dispatch passes an internal `stream_host_loop/0` stub process as the
`owner`, not the `macula_streamer` wrapper itself) to confirm the underlying stream's
lifetime was never actually tied to the wrapper's: a *graceful* stop
(`Reason = normal`) orphaned it forever (a link only propagates a non-normal exit to a
non-trapping peer, and `owner`'s death — not the wrapper's — is what `macula_stream`
itself watches), and an *abnormal* stop killed it only via the ordinary link-crash
cascade, with no explicit protocol-level signal ever reaching the far side either way.
Fixed by adding `finish_stream/2` to `terminate/2` on both modules, verified RED before
GREEN for each (neutering the abort branch on either module reproduces the missing-call
exactly, caught by dedicated `meck:num_calls` assertions, not just an outcome check).

**One test-design pitfall found and worked around, worth recording:** an early version
of the new `macula_streamer_client_stream_tests` asserted `meck:num_calls` for
close/abort immediately after receiving the last expected `{chunk_seen, _}` message —
this raced against the reader process's own concurrent, asynchronous delivery of the
NEXT recv result (eof or an error), which drives `terminate/2` on a separate schedule
from when the test process happens to receive its last chunk notification. Passed when
run in isolation, failed intermittently when run alongside other test files (more
scheduler contention exposed the race window). Fixed by adding `terminate/2` to the
test callback module purely as a synchronization signal (`Parent ! {terminated, Reason}`,
mirroring `macula_streamer_tests`'s existing pattern) and waiting for it before
asserting on mock call counts — not a meck cross-file isolation problem as first
suspected; confirmed by running the new file alone (passed) vs. combined with the
others (failed identically both times), which pointed at a race inherent to the test's
own OWN synchronization, not shared mock state.

No pause work added, per the scope decision above — no concrete need surfaced.

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
| `src/macula_content_transfer.erl` | New — Phases 1-3, all shipped. Phase 1: addressable put/get, real cancel. Phase 2: chunk loop onto `handle_continue/2` + per-step workers, real `pause/1`/`resume/1`. Phase 3: single `#chunk` loop replaced by per-stream `#lane{}` model, `stream_count` option, concurrent dispatch |
| `src/macula_content_transfer_registry.erl` | New — Phase 1 shipped: share_id → pid registry |
| `src/macula.erl` | Phase 1 shipped: `put_content`/`get_content` (+ `_station` variants) reshaped onto `macula_content_transfer` as thin wrappers; ~200 lines of transfer internals moved out. Untouched by Phases 2-3. |
| `src/macula_root.erl` | Phase 1 shipped: `macula_content_transfer_registry` added as a supervised child |
| `src/client/macula_station_link.erl` | Phase 1 shipped: new `abort_content_stream/4`, `close_content_stream_state/2` refactored to share the teardown path with it. Untouched by Phases 2-3 — cancel's granularity (per-chunk-step, then per-lane) changed entirely inside `macula_content_transfer`, no change needed here. |
| `src/peering/macula_quic.erl` | Phase 1 shipped: new `reset_stream/2`; `async_shutdown_stream/3`'s previously-discarded `Code` param now genuinely used |
| `native/macula_quic/src/{atoms,lib,stream}.rs` | Phase 1 shipped: new `nif_reset_stream` NIF (Quinn `SendStream::reset`); recv loop distinguishes a peer reset from every other read error |
| `src/macula_feeder.erl` | Phase 4 shipped: internals call `macula_content_transfer:start_put/3`/`start_put_station/5` directly via a lightweight resolve+await proxy; `terminate/2` now reaps the real transfer, fixing the orphan-on-cancel bug |
| `src/macula_download.erl` | Phase 4 shipped, symmetric — `start_get/3`/`start_get_station/5` |
| `src/macula_streamer.erl` | Phase 5 shipped: optional `handle_chunk/2` + linked-reader loop for `client_stream` mode; `terminate/2` now closes (`normal`) or aborts (anything else) the underlying stream, previously did neither |
| `src/macula_stream_sink.erl` | Phase 5 shipped: `terminate/2` now closes on `normal`, aborts otherwise — previously called `close_stream` unconditionally regardless of reason |
| `src/macula_pusher.erl` | New — Phase 6 (not started) |
| `src/macula_upload.erl` | New — Phase 6 (not started) |
| `test/macula_content_transfer_tests.erl` | Phase 1 shipped: new, 7 cases, meck-based. Phase 2 shipped: +5 cases (pause/resume put and get, single-block no-op, cancel-while-paused). Phase 3: existing chunked cases pinned to `stream_count => 1` (the new default of 4 changed their behavior — they're specifically about single-stream sequential ordering) |
| `test/macula_content_transfer_multi_stream_tests.erl` | New — Phase 3 shipped, 6 cases: concurrent dispatch, order-independent reassembly, stream-count capping, degraded-stream fallback, failure kills other lanes, cancel aborts every stream |
| `test/macula_quic_stream_reset_tests.erl` | New — Phase 1 shipped, 3 cases, real two-endpoint loopback. Untouched since. |
| `test/macula_content_block_hash_tests.erl` | Phase 1 shipped: updated to call `macula_content_transfer:verify_block_hash/2` (moved from `macula:verify_block_hash/2`) |
| `test/macula_feeder_tests.erl`, `macula_download_tests.erl` | Phase 4 shipped: rewritten to mock `macula_client`/`macula_station_link` (mechanical necessity, not a design choice — see the corrected Phase 4 section); `Pool` is now a real pid, not the placeholder atom `pool`; +2 cases per module (real-cancel-reaches-the-stream, direct-dial — previously uncovered) |
| `test/macula_streamer_tests.erl` | Phase 5 shipped: `abort`/`close` mocked explicitly; existing normal-stop and abnormal-stop cases now assert which one fires |
| `test/macula_stream_sink_tests.erl` | Phase 5 shipped: `abort` mocked; +2 cases (`normal_stop_closes_not_aborts`, `abnormal_stop_aborts_not_closes`) |
| `test/macula_streamer_client_stream_tests.erl` | New — Phase 5 shipped, 2 cases: pushed chunks reach `handle_chunk/2` then eof closes; a `recv` error aborts instead. Split from `macula_streamer_tests.erl` because it needs a callback module that genuinely exports `handle_chunk/2` (see the Phase 5 section above) |
| `test/macula_pusher_tests.erl`, `macula_upload_tests.erl` | New (Phase 6, not started) |
| `CHANGELOG.md`, `macula.app.src`, `CLAUDE.md` (version header) | Phase 1 as 9.9.0, Phase 2 as 9.10.0, Phase 3 as 9.11.0, Phase 5 as 9.12.0 (all MINOR — new capability), Phase 4 as 9.11.1 (PATCH — fixes existing behavior, no new public API) |
| `docs/guides/CONTENT_GUIDE.md` | Phase 1: new "Real cancel: macula_content_transfer" section + Reference table rows. Phase 2: new "Real pause/resume for chunked transfers" section + row. Phase 3: new "Parallel multi-stream chunk transfer" section + rows. Phase 4: corrected the now-stale "not the blunt local kill macula_feeder/download's cancel/1 still is" line; Reference table rows updated to say `macula_content_transfer` instead of `put_content/2`/`get_content/2`. |
| `docs/guides/STREAMING_GUIDE.md` | Phase 5 shipped: new `client_stream` provider example + "Cancel" paragraph under "Supervised wrappers"; Reference table rows for `macula_streamer`/`macula_stream_sink` note the receive-loop + abort-wired cancel |
| `macula-station`, `macula-realm`, `hecate-om` (separate repos) | Checked for direct `put_content`/`get_content` callers before Phase 1 landed, and for direct `macula_feeder`/`macula_download` callers before Phase 4 — none found in any of the three, either time, nothing to update. |

## Testing plan

Per phase: RED (new tests fail against pre-phase code, via `git stash` on the phase's
source changes) → GREEN (fix restored) → full `rebar3 eunit` → `rebar3 xref` →
`rebar3 dialyzer` — all must be clean, matching the discipline already established this
session for the puzzle-hardening and mesh-identity fixes. No phase lands with the
"cancelled tests" eunit quirk unexplained (confirmed pre-existing and unrelated to any
of this work, but re-verify per phase rather than assuming).

## Success criteria

- [x] Phase 1: `macula_content_transfer` addressable, `cancel/3` provably resets the
      open content stream (`macula_quic:reset_stream/2`, not `macula_stream:abort/3` —
      see the corrected Phase 1 section above). Verified two ways: a meck-based test
      asserting `cancel/3` calls `macula_station_link:abort_content_stream/4` with the
      right stream/code/message, and a real two-endpoint loopback test asserting the
      PEER genuinely observes `{quic, stream_closed, _, {reset, Code}}`, not just a
      closed connection.
- [x] Phase 2: a paused transfer sends zero further chunks until resumed
      (`pause_stops_chunked_put_between_chunks`, `pause_stops_chunked_get_between_chunks`);
      resume continues from the correct next chunk, not from the start
      (`resume_continues_from_the_next_chunk_not_the_start` — asserts distinct
      chunk MCIDs per call, in order, ending at the correct final manifest MCID).
      Verified RED before GREEN on the `paused` gate itself (all 4 pause-dependent
      tests fail identically — `{unexpected_call_started, ...}` — with the gate
      removed) and on the `worker = undefined` cancel fix separately.
- [x] Phase 3: verified the MECHANISM a real speedup depends on rather than a literal
      wall-clock measurement (meaningless against a mocked link with no real network
      latency to save) — `put_dispatches_chunks_concurrently_across_streams` proves N
      chunk calls are genuinely in flight SIMULTANEOUSLY, collecting N `call_started`
      events before releasing any of them, which only succeeds if they were dispatched
      concurrently, not one at a time (confirmed via RED: a one-lane-at-a-time revert
      makes 5 of the 6 multi-stream tests fail identically). Reassembly verified correct
      regardless of which stream delivers which chunk first
      (`get_reassembles_correctly_regardless_of_arrival_order`, releasing chunks in the
      reverse of their arrival order; RED-verified against an arrival-order-keyed
      accumulator, which fails with `root_hash_mismatch` as expected).
- [x] Phase 4: same callback contract, same public `start_link`/`start_link_direct`/
      `cancel` signatures, verified with the SAME assertions the pre-Phase-4 tests
      made (outcome/mcid/chunked/topics) — but not literally the same test FILES
      unmodified, which turned out to be impossible without leaving the phase's
      actual bug unfixed (see the corrected Phase 4 section above for why). More
      important than the letter of the original criterion: added a test per module
      that asserts `cancel/1` genuinely calls `abort_content_stream` on the open
      stream, not just that `outcome => cancelled` gets published — RED-verified
      (neutering the new reap call reproduces exactly the gap this phase fixes) for
      both `macula_feeder` and `macula_download`.
- [x] Phase 5: a `client_stream`-mode provider using `handle_chunk/2` receives every
      chunk a consumer sends, in order, with no hand-rolled `recv` loop in application
      code (`pushed_chunks_reach_handle_chunk_then_eof_closes` — chunks arrive in send
      order, then eof closes cleanly). Abort-wiring verified RED before GREEN on both
      modules separately (neutering the abort branch reproduces the missing
      `macula_stream:abort/3` call, caught by dedicated `meck:num_calls` assertions,
      not just an outcome check) — extended beyond the plan's literal wording, which
      named only the receive-loop; the cancel fix was always part of the same phase per
      the plan's own body text above, the checklist item just hadn't named it.
- [ ] Phase 6: a `macula_pusher` push to a `macula_upload` receiver whose received bytes
      are tampered with in transit (test-injected corruption) is caught by
      `macula_manifest:verify/2` and reported as a failure, never silently accepted.
