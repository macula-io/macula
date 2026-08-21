# Macula SDK — Pub/Sub Guide

**Topic-based messaging over the relay mesh.**

![PubSub across Two Stations](assets/pubsub_two_stations.svg)

> **Audience:** Erlang/OTP applications publishing or subscribing to
> events. Building something the wrappers below don't fit? See
> [PUBSUB_PROTOCOL.md](PUBSUB_PROTOCOL.md) for the raw `macula:subscribe/4,5`
> / `macula:publish/4,5` primitives underneath.

---

## Overview

A subscriber **subscribes** to a topic and receives every matching event; a
publisher **publishes** an event to a topic — `macula_subscriber` and
`macula_publisher`, an addressable pid you can monitor and cancel.

Subscribing and publishing are independent capabilities, unlike RPC's
request/response or a single content transfer — an application might do
only one, the other, or both. Every operation on either side names an
explicit realm and topic; see [Three core ideas](#three-core-ideas), below,
before writing your first `macula_topic:app_fact/6` call.

See [Supervised wrappers](#supervised-wrappers-macula_subscriber-macula_publisher),
right below.

---

## Supervised wrappers: `macula_subscriber` / `macula_publisher`

`macula:subscribe/4,5` delivers events as raw messages to whatever pid you
pass it, and `macula:publish/4,5` is a plain blocking call — neither has an
addressable pid you can supervise, monitor, or cancel from outside.
`macula_subscriber` and `macula_publisher` wrap the same two primitives as
proper OTP behaviours. Only the publish side announces itself on the mesh —
`macula_publisher` publishes `pubsub.publish_started_v1` /
`pubsub.publish_completed_v1` around each transfer; `macula_subscriber` has
no mesh-fact equivalent, since a subscription has no single "done" moment to
announce.

Subscriber side — `start_link/5,6` opens the subscription in its own
`init/1` and threads `macula_event` / `macula_event_gone` dispatch into
`Module:handle_event/4` for you — no hand-rolled `handle_info` clauses
needed (see [PUBSUB_PROTOCOL.md](PUBSUB_PROTOCOL.md#subscribing-in-a-callback-module)
for the manual pattern this replaces, if you're building something the
wrapper doesn't fit). `handle_event/4` receives `Topic`, `Payload`, and
`Meta` — a map carrying delivery context:

| Key | Type | Meaning |
|---|---|---|
| `realm` | `<<_:256>>` | Realm tag (matches the subscribe call) |
| `publisher` | `binary()` | Publisher pubkey (the original publisher, not the relay) |
| `seq` | `non_neg_integer()` | Per-publisher monotonic sequence |
| `delivered_via` | `binary()` | Pubkey of the link/station that delivered this copy |

```erlang
-module(my_orders_listener).
-behaviour(macula_subscriber).
-export([init/1, handle_event/4]).

init(_Args) -> {ok, #{}}.

handle_event(_Topic, Payload, _Meta, State) ->
    on_order_placed(Payload),
    {noreply, State}.
```

```erlang
Topic = macula_topic:app_fact(Realm, my_org, my_app,
                              <<"orders">>, <<"placed">>, 1),
{ok, Pid} = macula_subscriber:start_link(my_orders_listener, Pool, Realm,
                                         Topic, []).
```

`Opts` (the arity-6 `start_link/6`) passes straight through to
`macula:subscribe/5` — the `delivery` option covered in
[Delivery ordering](#delivery-ordering), below, works exactly the same way
whether you call it raw or through the wrapper.

A `macula_event_gone` for this subscription stops the sink with that reason
(see [When the subscription ends](#when-the-subscription-ends), below). To
stop receiving events deliberately, stop the sink itself
(`gen_server:stop/1`, or let its supervisor terminate it) — the pool
monitors the subscriber pid directly and drops the wire-level subscription
automatically once it's gone, the same cleanup an explicit `unsubscribe`
gives a raw caller.

Publisher side — `start_link/5,6` returns immediately with a pid; the
publish runs in a linked worker and the outcome reaches
`Module:handle_published/2`:

```erlang
-module(status_publisher).
-behaviour(macula_publisher).
-export([init/1, handle_published/2]).

init(Parent) -> {ok, Parent}.

handle_published(Result, Parent) ->
    Parent ! {published, Result},
    {stop, normal, Parent}.
```

```erlang
{ok, Pid} = macula_publisher:start_link(status_publisher, Pool, Realm,
                                        Topic, Payload, self()).
```

| `Result` | Meaning |
|---|---|
| `ok` | At least one link accepted the PUBLISH frame |
| `{error, {transient, no_healthy_station}}` | The pool has zero spawned links — caller may retry |
| `{error, _}` | Other failures (validation, etc.) |

> **Partial success counts as success.** With `replication_factor > 1`,
> the publish resolves as soon as the first selected link accepts the
> frame. Subsequent links are best-effort.

`macula_publisher:cancel/1` stops it before the publish resolves,
delivering `outcome => cancelled` in the `pubsub.publish_completed_v1`
mesh fact — same shape as `macula_feeder`'s `sharing.put_started_v1` /
`sharing.put_completed_v1`.

Unlike `macula_subscriber`, `macula_publisher:start_link/6`'s last argument
is `Args` (for `Module:init/1`) — there's no way to pass `macula:publish/5`'s
own `Opts` (e.g. `timeout_ms`) through the wrapper. See
[PUBSUB_PROTOCOL.md](PUBSUB_PROTOCOL.md#publishing-with-options) if you
need that.

---

## Three core ideas

### 1. Realm-per-call

Every publish and every subscribe carries an explicit 32-byte
**realm** tag. There is no connect-time default realm. Realms are
how the mesh isolates traffic — a subscriber on Realm A never
receives an event published to Realm B, even if the topic strings
match exactly.

```erlang
%% Same topic, different realms — totally separate streams.
ok = macula:publish(Pool, RealmA, Topic, PayloadA),
ok = macula:publish(Pool, RealmB, Topic, PayloadB).
```

Realms are 32-byte binaries. Use `macula_realm:id/1` (SHA-256 of the
human-readable realm name) or your own realm registry to derive them.

### 2. Topics describe event types, not entities

> **Non-negotiable for scalable pub/sub.**

```
Topic   = WHAT happened (event type, immutable)
Payload = WHO/WHERE/WHEN it happened (entity details)
```

| Approach | 1M sensors | Topics | Result |
|---|---|---|---|
| ID in topic | 1M sensors | 1M topics | DHT explosion, memory exhaustion |
| ID in payload | 1M sensors | 1 topic | Scalable, efficient routing |

**Wrong** (topic explosion):

```erlang
%% DO NOT DO THIS — entity ID baked into topic
Topic = macula_topic:app_fact(Realm, Org, App,
                              <<"weather">>,
                              <<"manchester_main_street_wind_measured">>, 1),
macula:publish(Pool, Realm, Topic, #{speed => 42.5}).
```

**Right** (IDs in payload):

```erlang
Topic = macula_topic:app_fact(Realm, Org, App,
                              <<"weather">>, <<"wind_measured">>, 1),
macula:publish(Pool, Realm, Topic, #{
    station_id => <<"manchester-main-street">>,
    speed => 42.5,
    unit => <<"km/h">>,
    timestamp => erlang:system_time(millisecond)
}).
```

### 3. Topics are built — never hand-typed

```erlang
%% Always
Topic = macula_topic:app_fact(Realm, Org, App, Domain, Name, Version),

%% Never
Topic = <<"my.realm/my.org/my.app/orders/placed_v1">>.
```

`macula_topic` returns a canonical binary that the SDK and stations
agree on. Hand-rolled strings are rejected.

See **[TOPIC_NAMING_GUIDE.md](../shared/TOPIC_NAMING_GUIDE.md)** for the canonical
specification of the five-segment shape and the `realm_fact /
org_fact / app_fact` tier choice.

---

## When the subscription ends

The only way a live subscription produces a terminal message today is
the pool closing — delivered to `handle_event/4`'s raw equivalent as
`{macula_event_gone, SubRef, pool_closed}`, and to a `macula_subscriber`
sink as its own termination with that reason.

A link dying does **not** end the subscription — the pool logs
`_macula.client.link_down`, schedules a respawn, and silently
re-issues the subscription against the new link once it's up (see
[Connecting Guide](../shared/CONNECTING_GUIDE.md#lifecycle)). Neither layer
sees a gap-signaling message for that case, only a possible gap in
delivery itself, which `ordered` mode's `order_timeout_ms` skip
handles the same way it handles any other loss.

After the subscription ends, no further events come for it.

---

## Delivery ordering

The `delivery` option chooses how a **single publisher's** stream is
ordered on the way to your subscriber — pass it in `Opts` to
`macula_subscriber:start_link/6` the same way you would to raw
`macula:subscribe/5`. This matters because the mesh sends copies of a
fact down several links at once, and the pool dedups to the *first*
arrival — so without ordering, a single publisher's `seq 1, 2, 3` can
reach you as `1, 3, 2`. Each publisher's `seq` is pool-monotonic and
contiguous, which is exactly what makes ordered delivery possible.

| Mode | Behaviour | Use when |
|---|---|---|
| `ordered` (**default**) | Per-publisher FIFO by `seq`. Out-of-order arrivals are buffered and released in order; a genuinely missing seq is skipped after `order_timeout_ms`. | Event / delta streams where order matters. |
| `latest_only` | Deliver only seqs newer than the highest seen for that publisher (drop stale). No buffering, no head-of-line delay. | State snapshots — you want the freshest value, not every value. |
| `as_arrives` | Raw arrival order. Zero added latency; you order it yourself. | You have your own versioning, or you truly do not care. |

```erlang
%% default — per-publisher FIFO
{ok, Pid1} = macula_subscriber:start_link(my_orders_listener, Pool, Realm, Topic, []),

%% newest-wins, drop stale
{ok, Pid2} = macula_subscriber:start_link(my_orders_listener, Pool, Realm, Topic, [],
                                          #{delivery => latest_only}),

%% raw arrival order (the pre-8.8 behaviour)
{ok, Pid3} = macula_subscriber:start_link(my_orders_listener, Pool, Realm, Topic, [],
                                          #{delivery => as_arrives}).
```

**Ordered mode and loss.** `ordered` trades a bounded delay for order:
if `seq 2` never arrives, the buffer holds `3, 4, …` only until
`order_timeout_ms` elapses, then skips the gap and releases them. That
skip is the accepted "order-not-guaranteed delivery" trade for a lost
fact — a reorder buffer cannot invent a message the mesh dropped. Design
mesh facts to be **idempotent and version-stamped** so an occasional
skip washes out.

**Total order is not offered, by design.** `ordered` restores a *single*
publisher's order (cheap, over ordered transport). It does not impose a
total order across *different* publishers — that would need a single
sequencer (a consensus log) that the mesh deliberately does not have.
Cross-publisher order is not something a decentralised broadcast can give
you; carry a version or timestamp in the fact if a consumer needs to
relate two publishers' events.

### Pool-level tuning (`connect/2` options)

| Option | Default | Meaning |
|---|---|---|
| `order_timeout_ms` | `250` | How long an `ordered` sub waits for a missing seq before skipping the gap. Bounds head-of-line delay. |
| `order_max_buffer` | `1024` | Per-publisher reorder-buffer count cap. Over it, the head gap is skipped early (memory guard for a high-rate publisher gapping). |

### Telemetry — is loss real?

`macula:status/1` reports `pubsub_gap_skips`: the number of per-publisher
gaps given up on after the timeout, i.e. the genuine loss rate an
`ordered` subscriber could not fill. A near-zero value means the mesh is
delivering and `ordered` costs you almost nothing; a rising value is the
signal to look at delivery, not ordering.

```erlang
{ok, #{pubsub_gap_skips := Skips}} = macula:status(Pool).
```

---

## Dedup and delivery guarantees

`{publisher, seq}` is the dedup key. The pool guarantees you see each
`(Realm, Publisher, Seq)` tuple **at most once**, even when the same EVENT
arrives via multiple links (e.g. with `replication_factor > 1`). In
`ordered` and `latest_only` modes the delivery layer additionally uses the
seq to order or drop; in `as_arrives` the dedup layer is the only filter.

- **At-most-once** — fire and forget. No publisher-visible ack from
  subscribers.
- **Per-publisher delivery order** — `ordered` by default at the
  subscriber (see [Delivery ordering](#delivery-ordering) above):
  out-of-order arrivals are buffered and released in `seq` order, with a
  genuinely missing `seq` skipped after `order_timeout_ms`. The mesh
  itself does not guarantee arrival order — a relay spreads one
  publisher's burst across concurrent verify workers, and a receiver may
  admit an event by more than one path — the subscriber-side `ordered`
  buffer is what turns that into in-order delivery. Opt into `as_arrives`
  if you'd rather see raw arrival order and reorder yourself.
- **Cross-publisher ordering** — none, by design. Two publishers' events
  arrive in arbitrary interleaving; see "Total order is not offered, by
  design" above.
- **Cross-link dedup** — the pool dedupes by `(Realm, Publisher, Seq)`
  over a 60-second window (configurable; see `dedup_window_ms` in
  [CONNECTING_GUIDE.md](../shared/CONNECTING_GUIDE.md)).
- **Cross-station gossip** — default since 4.5.0. A daemon connected to
  station A and a daemon connected to station B see each other's
  publishes once subscription interest and the fact itself have gossiped
  between the stations; publisher-end-to-end signatures plus
  `(publisher, seq)` dedup at each hop is what makes this safe past one
  hop.

---

## Topic naming reference

Quick reference. Full specification:
[TOPIC_NAMING_GUIDE.md](../shared/TOPIC_NAMING_GUIDE.md).

Every topic is exactly five slash-separated segments:

```
{realm}/{publisher_org}/{publisher_app}/{domain}/{name}_v{N}
```

Pick a tier based on who owns the topic:

| Tier | Builder | Use when |
|---|---|---|
| Realm-level | `macula_topic:realm_fact/4`, `macula_topic:realm_hope/4` | Topic owned by the realm itself |
| Org-level | `macula_topic:org_fact/5`, `macula_topic:org_hope/5` | Topic owned by an organization within a realm |
| App-level | `macula_topic:app_fact/6`, `macula_topic:app_hope/6` | Topic owned by an application within an organization |

**Past tense for facts** (`order_placed`, `wind_measured`, `user_registered`).
**Present tense for hopes** (`order_place`, `payment_authorize`).

System topics (`_mesh.*`, `_macula.*`) are infrastructure-owned and
dot-separated. Do not publish to them from app code.

---

## Patterns

### Re-subscribe after pool restart

If your supervisor restarts the pool, your subscribers are not
automatically re-attached. Either:

- Restart your subscriber processes (or `macula_subscriber` sinks)
  alongside the pool (one supervisor with `rest_for_one` strategy), or
- Watch for the subscription ending with `pool_closed` and re-subscribe
  to the new pool.

### Multiple subscribers on one pool

A single pool can have arbitrarily many local subscribers — whether raw
processes or `macula_subscriber` sinks. The pool issues exactly one
wire-level SUBSCRIBE per `(Realm, Topic)`, multiplexes inbound events to
every local subscriber for that pair, and dedupes across links.

### Backpressure

Events are delivered as Erlang messages. If a subscriber is slow, its
mailbox grows. The pool itself never blocks. Apply your usual
mailbox-flow-control patterns (process throttling, batching, etc.).

### Idempotent handlers

Even though the pool dedupes by `(Realm, Publisher, Seq)`, network
weirdness across long restarts can theoretically allow a duplicate
sneak through after the dedup window expires. Make handlers
idempotent — match on a payload-level key (order id, sensor id +
timestamp, etc.) when correctness matters.

---

## Best practices

1. **IDs in payloads, not topics.** Always.
2. **Past-tense facts, present-tense hopes.** No CRUD verbs
   (`created`, `updated`, `deleted`).
3. **Build topics via `macula_topic`** — never inline strings.
4. **Include a timestamp** — `erlang:system_time(millisecond)` in
   every payload.
5. **Keep handlers fast** — spawn workers for heavy processing.
6. **Make handlers idempotent** — `(publisher, seq)` is a strong dedup
   key, but cross-restart edge cases exist; don't rely on
   exactly-once.

---

## Diagnostics

| Event topic | When | Meta |
|---|---|---|
| `_macula.client.link_down` | A pool link's worker died | `seed`, `pid`, `reason` |
| `_macula.peering.handshake_timeout` | A station handshake hung past `state_timeout` | `role`, `buf_size`, `has_stream`, `timeout_ms` |

These come through `macula_diagnostics:event/2`; wire them into your
observability layer.

---

## See also

- [PUBSUB_PROTOCOL.md](PUBSUB_PROTOCOL.md) — the raw primitives
  underneath, plus how relay-side routing works.
- [Connecting Guide](../shared/CONNECTING_GUIDE.md) — pool model, lifecycle, options
- [Topic Naming Guide](../shared/TOPIC_NAMING_GUIDE.md) — canonical topic shape
- [Authorization Guide](../shared/AUTHORIZATION_GUIDE.md) — UCAN/DID identity
- [`macula_pubsub`](https://hexdocs.pm/macula/macula_pubsub.html) — slice module
- [`macula_client`](https://hexdocs.pm/macula/macula_client.html) — pool implementation
- [`macula_subscriber`](https://hexdocs.pm/macula/macula_subscriber.html) — supervised subscriber behaviour
