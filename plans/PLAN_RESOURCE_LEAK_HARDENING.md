# PLAN_RESOURCE_LEAK_HARDENING.md

**Status:** Survey complete — hardening not started
**Created:** 2026-09-12
**Last Updated:** 2026-09-12

## Overview

Read-only survey of the macula SDK core (`src/`, ~35k lines / 96 modules) for
BEAM memory and resource leaks: process leaks, timer re-arm loops, mailbox
buildup, ETS/gproc growth, binary retention, NIF resources, monitor/link
cleanup. ~6,000 lines of the hot paths were read end to end
(`macula_client`, `macula_station_link`, `macula_peering_conn`,
`macula_quic`, `macula_stream` + the whole stream/transfer family, all
`*_sup.erl` factories, registries, dedup/order, dist/cluster/overlay) and
the remainder grep-audited. No code was changed.

General hygiene found to be GOOD and not repeated as findings: no `gproc`
usage anywhere; `macula_peering_conn` has state timeouts for every phase,
cancels its dial, monitors its controlling pid, fails all pending
dedicated-stream opens in `terminate/3` and closes the QUIC connection
(peering_conn:238-241, 854-859, 957-961); `macula_station_link` cancels
every pending-call timer on reply/timeout/disconnect and clears every map
in `fail_all_pending/2` (station_link:1649-1691); `macula_client` runs a
single self-rearming dedup sweep chain, a lazily-armed one-shot order-flush
timer, cancels before re-arming the discovery timer, demonitors dropped
subscriptions and gives up on never-connected discovered links
(client:1087-1101, 2224-2233, 865-879, 2128, 1784-1818);
`macula_content_transfer_registry` is monitor-based (entries vanish with
their owner); feeder/download/pusher/request/publisher all reap their
workers and underlying handles in `terminate/2`; `stream_sink`/`streamer`
kill their reader and close/abort the stream in `terminate/2`; the Rust
NIFs (`native/macula_quic`) implement `Drop` on their resources (the recv
task is aborted when an Erlang stream handle is collected) and the other
five NIFs are pure-value, no resource types. `apps/macula_transport` is a
leftover containing only cargo build artifacts; the active QUIC NIF source
is in-tree at `native/macula_quic`.

---

## Findings (ranked)

### CRITICAL

#### F1. Immortal host + server-side `macula_stream` per inbound streaming session (provider side)
`src/client/macula_station_link.erl:2727` and `:2739-2740`
(`spawn_inbound_stream/6`, `stream_host_loop/0`), combined with
`src/macula_stream.erl:489-496` (the stream stops ONLY when its owner
dies) and `:366-375`/`:293-297` (peer-end / close / abort drain waiters
but never stop the gen_server).

Every inbound STREAM_OPEN spawns a host process
(`spawn(fun stream_host_loop/0)`) that does nothing but `receive stop`,
and **nothing anywhere sends it `stop`** (verified by grep across
`src/`). That host is the server-side `macula_stream`'s owner, and the
stream process has exactly one exit path — owner DOWN (`{stop, normal, ...}`
in `macula_stream:handle_down/3`). Since the host never dies, the
server-side stream gen_server is immortal, and the terminal-frame path
(`deliver_stream_end` → `forget_on_full_close` → `drop_stream`,
station_link:2781-2814) removes the routing entry, demonitors, and closes
the dedicated QUIC stream — but leaves the two BEAM processes alive.

Consequence: 2 processes leaked per streaming session served, forever, for
the lifetime of the link/pool. A long-lived provider (log tailer, any
`advertise_stream` user) grows without bound. In `server_stream` mode a
`macula_streamer` child linked to the stream (streamer:330, and its
`handle_info({'EXIT', Stream, ...})` stop clause at :403-404, which never
fires) leaks alongside them — 3 processes per session.

Fix direction: send the stream a genuine `stop` signal on full close (or
have `macula_stream` stop itself on `{peer_end, both}` after draining),
and make the host die when its stream does (link the host to the stream,
or drop the host entirely and use the link as owner with an explicit
session-end signal).

#### F2. Untrusted manifest `chunk_count` drives an unbounded allocation on GET
`src/macula_content_transfer.erl:576-580` (`setup_get_lanes/4` →
`lists:seq(0, ChunkCount - 1)` → `distribute_lanes/3`) and
`src/content/macula_manifest.erl:190-226` (`from_wire/1` accepts any
`chunk_count`, no cross-check against `size`/`chunk_size`).

A fetched manifest is wire input from a station; anyone can store one
(`_content.put_manifest` does not validate) whose MCID recomputes from its
own canonical fields and then hand out that MCID. On GET, the transfer
allocates a list of `ChunkCount` integers plus per-lane remainder lists
before any network call. A hostile `chunk_count` (e.g. 10^7) OOMs the
transfer gen_server.

Fix direction: reject manifests whose `chunk_count` exceeds
`ceil(size / chunk_size)` (or a sane cap) in `from_wire/1` or in
`get_manifest_result/2`, before lane setup. (`macula_upload` already
bounds a hostile sender's chunk count; the content-get path does not.)

### HIGH

#### F3. Local streaming leaks a host process + server-side stream per call
`src/macula_stream_local.erl:172-176` (`self_host_pid/0`, `host_loop/0`).

Same shape as F1 on the LOCAL path (`macula:call_stream/3`,
`open_stream/3,4`, advertised-dispatch — used for tests and same-BEAM
dispatch): every local pair spawns a host waiting for a `stop` message
nothing ever sends, so the server-side `macula_stream` (which stops only
on owner DOWN) and the host both live forever. 2 processes per call,
unbounded for any long-lived process using local streaming.

Fix direction: same as F1 — stop the server stream on full close, and
terminate the host when the stream dies.

#### F4. `macula_stream` inbound queue is unbounded (no backpressure)
`src/macula_stream.erl:507-516` (`enqueue_or_deliver/3`) and
`:362-364` (`handle_cast({peer_chunk, ...})`).

Chunks arriving with no `recv/2` waiter waiting are appended to the
`inbox` queue with no cap. The QUIC receive window bounds transport
buffering, but application-level chunks accumulate without limit in a
stream process whose consumer reads slower than the provider pushes (or
never reads — e.g. a local pair whose caller dropped the returned stream
pid without closing). Combined with F1/F3 this is also where leaked
streams accumulate binaries forever.

Fix direction: cap the queue (drop oldest + count, or `{error, overflow}`
to the sender) or apply sender backpressure at the `send/2` boundary.

#### F5. Stuck `macula_response` handlers accumulate as live children
`src/macula_response.erl:210` (`gen_server:call(Pid, run, ?CALL_TIMEOUT)`)
and `:240-244` (`handle_call(run, ...)` runs `Module:handle_request/2`
inline in the child gen_server).

The dispatch caller (a transient process in `station_link`) times out
after 30 s and dies, but the response child is still inside the user
handler. It is a `temporary` child, so nothing restarts it — and nothing
kills it either. A handler that blocks forever leaves its gen_server and
its supervisor child entry in place for the lifetime of the factory sup;
repeated slow/stuck calls accumulate processes without bound, each still
running user code.

Fix direction: run the handler in a worker with a hard deadline (kill on
deadline), or make the child self-destruct when its dispatch caller dies
(monitor the `From` pid inside `run`).

### MEDIUM

#### F6. Unbounded per-inbound-CALL process fan-out
`src/client/macula_station_link.erl:2161-2167` (`handle_inbound_call/2`).

One plain `spawn` per inbound CALL, no concurrency cap and no
backpressure (the direct Erlang analogue of the dotnet survey's F7). A
CALL flood — or a pile of slow handlers — creates unbounded transient
processes, each holding the decoded payload, handler closure and reply
frame until the handler returns.

Fix direction: bounded dispatch (semaphore / counter per link) with
overflow mapped to a BOLT#4 error.

#### F7. Per-publisher ordering state never pruned
`src/pubsub/macula_pubsub_order.erl:231-241` (`flush_pub/5` keeps a
publisher with an empty buffer in the map) and `:263-264` (`put_pub/3`).

`pubs` is keyed by publisher identity; a `#pub{}` entry is created on the
publisher's first event and never removed, even after its buffer drains.
Each entry is small, but the count grows with the number of DISTINCT
publishers ever seen on a topic over the subscription's lifetime
(rotating identities, many ephemeral publishers) — unbounded for a
long-lived `ordered` subscription.

Fix direction: drop a `#pub{}` from `pubs` when its buffer is empty and
`next` is set (keep only while buffered or during first-fact hold).

#### F8. Dedup table: growth between sweeps + publisher-restart key collision
`src/client/macula_client_dedup.erl:19-22` (documented 3-tuple key
gap) and `:69-76`; sweep driven from `src/client/macula_client.erl:1087-1090`.

Two issues, both documented in-tree: (a) between the 30 s sweeps the
table grows at event rate × 60 s window — bounded in time but a memory
spike knob under sustained traffic (dotnet F8 equivalent); (b) the
`(Realm, Publisher, Seq)` key has no publisher session component, so a
publisher restart with a re-seeded counter can collide with a
not-yet-swept entry and its fresh events are dropped as duplicates
(correctness, and the exact gap the dotnet `EventDedup` doc flags against
this reference implementation).

Fix direction: add a `publisher_session_id` to the key (or wall-clock
re-seed check like `macula_pubsub_order`'s `?EPOCH_JUMP`); consider
bucketized sweeps for (a).

### LOW

#### F9. `macula_content_transfer` without `cancel/1` persists the handle + registry entry
`src/macula_content_transfer.erl:47-52` (documented contract) and
`:326-328` (process never self-terminates on completion).

The facade (`macula:put_content/2`, `get_content/2`) always reaps, and
feeder/download do too; only a direct `start_put/start_get` caller that
awaits and never cancels leaks the gen_server + its registry monitor
entry for the registry's lifetime. Documented behaviour, but worth an
optional idle self-termination (after result delivery) plus keeping the
cached result for a re-`await` grace window.

#### F10. `macula_streamer` dispatch failure abandons the inbound stream
`src/macula_streamer.erl:286-291` (`dispatch/7`).

If `supervisor:start_child` fails (e.g. `Module:init/1` returns
`{stop, _}`), the handler fun just returns `ok` — nothing aborts the
stream. The peer sees silence until its own `recv` timeout, and the
session's routing entry lives until that timeout closes the stream. The
rejected-open path (`abort_rejected_stream/2`, :349-351) already aborts
correctly; the start-child failure path should do the same.

#### F11. `macula_dist_discovery` subscriber monitors never demonitored
`src/macula_dist_system/macula_dist_discovery.erl:228-232` and
`:234-236`.

`subscribe/1` monitors the subscriber and discards the ref;
`unsubscribe/1` removes the pid from the list without demonitoring.
Repeated subscribe/unsubscribe cycles accumulate monitors in the
discovery gen_server for its whole lifetime (DOWN messages only ever
arrive on subscriber death). Deprecated module (removed in 11.0.0, not
started by the macula app), so LOW.

#### F12. `hecate_or_set` tombstones grow without GC
`src/overlay/hecate_or_set.erl:96-101` (`remove/2` unions every observed
tag into `tombstones`) and `:139-153` (`merge/2` unions tombstone sets).

Tombstones are monotonically non-decreasing by design; there is no
compaction, so a long-lived realm-shared OR-Set under churn (member
lists, directory metadata) accumulates 16-byte tags per remove forever.
Known CRDT property — LOW, but worth documenting a compaction strategy
(causal-stability-based or age-based).

#### F13. `macula_dist_relay_client` orphaned tunnel streams linger for the connection's lifetime
`src/macula_dist_system/macula_dist_relay_client.erl:532-538`
(`match_inbound(error, ...)` stashes into `orphan_streams`).

A stream whose tunnel control handshake never completes leaves its
`orphan_streams` entry (and the open QUIC stream) in place until the
relay connection ends — peer-triggerable. Bounded in practice by QUIC's
`peer_bidi_stream_count` (~100), so LOW.

#### F14. Subscriber delivery is unbounded plain `!` with no backpressure
`src/client/macula_client.erl:2203-2205` (`send_events/4`).

A slow consumer accumulates `{macula_event, ...}` messages (and their
payload binaries) in its own mailbox without limit — standard Erlang
delivery semantics, but worth documenting for payload-heavy topics
(recommend `latest_only` mode or a dedicated draining process).

#### F15. Auto-registered pubsub realm servers never retired
`src/overlay/hecate_pubsub_registry.erl:254-262` and `:286-292`.

With `default_identity` set, every previously-unseen realm tag
materialises a `hecate_pubsub_server` that is never torn down when idle;
`by_realm` grows per distinct realm tag seen for the registry's
lifetime. Bounded by how many realms actually carry traffic, but no
idle-reap exists.

---

## Cross-check with the SDK surveys

| SDK finding | Status here |
|---|---|
| F1 ContentTransfer never releases stream | **Not present** — every path closes/aborts all streams: success `close_all_streams/3` (content_transfer:761-777), cancel `abort_all_streams/4` (:428-435), single-block worker closes in try/catch (:459-463); facade always reaps |
| F2 AcceptAsync abandons stream on throw | **Not present (QUIC side)** — inbound dedicated streams are closed on session teardown (`drop_stream`, station_link:2584-2604) and on disconnect (`fail_all_pending`, :1670-1691); the residual is BEAM-process-level, captured as F1 here |
| F3 OpenAsync leaks stream if STREAM_OPEN write fails | **Not present** — `dedicated_open_result({error, _}, ...)` delivers a clean stream error and drops the entry (station_link:2506-2511); `send_on_stream` failures surface as `{quic, send_failed, ...}` → `end_sessions_on_stream` (:1292-1303) |
| F4 OCE misclassified as timeout | **N/A** — no async-exception model; the link distinguishes `{error, timeout}` vs `{error, {disconnected, Reason}}` explicitly (station_link:1570-1574, 1649-1691) |
| F5 GetAsync trusts manifest size | **Present** — see F2 here (chunk_count allocation on GET; `from_wire/1` performs no bound checks) |
| F6 No IDisposable safety net / buffer never shrinks | **Mostly absent** — Rust `Drop` on stream resources aborts the recv task when the Erlang handle is collected (stream.rs:311-317); buffer tails are partial frames only. No BEAM-side equivalent of the growing `_buf` |
| F7 Unbounded fan-out per inbound CALL | **Present** — see F6 here |
| F8 EventDedup growth between sweeps | **Present** — see F8 here (dedup table, sweep cadence) |
| F9 Fire-and-forget close task | **N/A** — `close_quic`/`close_dedicated_stream` are synchronous with try/catch, never fire-and-forget |
| F10 CTS ownership / unobserved callbacks | **N/A** |
| F11 Dead subscriptions retained by ended channel | **Not present** — `fail_all_pending` clears `subscriptions`/`topic_index` on disconnect; unsubscribe removes index entries (station_link:1688-1691, 1840-1853) |
| F12 Static OpenSessions registry | **N/A** — no static registry; pools are caller-owned processes |
| LOW dotnet items (CTS dispose, linkTasks, tmp files) | **N/A** |
| Dotnet note: Erlang dedup 3-tuple key is a live gap | **Confirmed live here** — see F8(b) |

---

## Phases

- [ ] Phase 1 — Stream-session lifecycle: make the server-side
      `macula_stream` stop on full close and its host die with it.
      Fixes F1 (station_link `spawn_inbound_stream`) and F3
      (`macula_stream_local`). Test: N sequential `call_stream` /
      local stream calls leave zero live stream/host processes.
- [ ] Phase 2 — Untrusted input bounds (F2): reject manifests whose
      `chunk_count` exceeds the size-derived bound before lane setup.
- [ ] Phase 3 — Backpressure (F4, F6): cap the `macula_stream` inbox;
      bounded dispatch for inbound CALLs.
- [ ] Phase 4 — Provider hardening (F5, F10): hard deadline on response
      handler execution; abort the stream when a streamer child fails to
      start.
- [ ] Phase 5 — Bookkeeping prunes (F7, F8, F11, F15): prune empty
      per-publisher state, dedup session-id key, demonitor on
      `unsubscribe/1`, idle-reap of pubsub realm servers.
- [ ] Phase 6 — Documentation + CRDT hygiene (F9, F12, F13, F14):
      document the cancel contract and delivery semantics; tombstone
      compaction design; orphan-stream cap.

## Files to Create/Modify

| File | Purpose | Status |
|------|---------|--------|
| `src/macula_stream.erl` | F1/F3 stop-on-close, F4 inbox cap | Not started |
| `src/client/macula_station_link.erl` | F1 host lifecycle, F6 bounded dispatch | Not started |
| `src/macula_stream_local.erl` | F3 host lifecycle | Not started |
| `src/macula_content_transfer.erl` | F2 chunk_count validation | Not started |
| `src/content/macula_manifest.erl` | F2 manifest bounds in `from_wire/1` | Not started |
| `src/macula_response.erl` | F5 handler deadline | Not started |
| `src/macula_streamer.erl` | F10 abort on start_child failure | Not started |
| `src/pubsub/macula_pubsub_order.erl` | F7 publisher-state pruning | Not started |
| `src/client/macula_client_dedup.erl` | F8 session-id key, sweep shape | Not started |
| `src/macula_dist_system/macula_dist_discovery.erl` | F11 demonitor on unsubscribe | Not started |
| `src/overlay/hecate_pubsub_registry.erl` | F15 idle realm-server reap | Not started |
| `src/macula_dist_system/macula_dist_relay_client.erl` | F13 orphan-stream cap | Not started |
| `src/overlay/hecate_or_set.erl` | F12 tombstone compaction (design) | Not started |

## Success Criteria

- [ ] A streaming provider serving N sequential sessions (pooled path)
      shows zero growth in live stream/host processes after each full
      close (repeatable 1000x).
- [ ] A local-pair loop (`call_stream/3` + close) leaves no host or
      server-side stream processes behind.
- [ ] A hostile manifest with `chunk_count` ≫ `size / chunk_size` fails
      fast without a large allocation.
- [ ] A provider whose handler blocks forever is reclaimed by a
      deadline; the response child does not accumulate.
- [ ] A `recv`-less stream stops accumulating chunks at the cap.
- [ ] An inbound CALL flood above the dispatch cap is answered with a
      structured error instead of unbounded spawns.
- [ ] `rebar3 eunit` green.
