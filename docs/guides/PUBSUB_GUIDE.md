# Macula SDK — Pub/Sub Guide

**Topic-based messaging over the relay mesh.**

> **Audience:** Erlang/OTP applications publishing or subscribing to
> events. Since v3.11.0 (V2 surface). For the V1 (pre-3.11.0)
> single-connection surface, see
> [../migrations/V1_TO_V2_PUBSUB.md](../migrations/V1_TO_V2_PUBSUB.md).

---

## TL;DR

```erlang
%% 1. Connect to the mesh — see CONNECTING_GUIDE.md.
{ok, Pool} = macula:connect(Seeds, #{}).

%% 2. Subscribe.
Topic = macula_topic:app_fact(Realm, Org, App,
                              <<"orders">>, <<"placed">>, 1),
{ok, Sub} = macula:subscribe(Pool, Realm, Topic, self()).

%% 3. Receive events.
receive
    {macula_event, Sub, Topic, Payload, Meta} ->
        handle(Topic, Payload, Meta)
end.

%% 4. Publish.
ok = macula:publish(Pool, Realm, Topic,
                    #{order_id => <<"ord-123">>, total => 4999}).

%% 5. Drop the sub.
ok = macula:unsubscribe(Pool, Sub).
```

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

See **[TOPIC_NAMING_GUIDE.md](TOPIC_NAMING_GUIDE.md)** for the canonical
specification of the five-segment shape and the `realm_fact /
org_fact / app_fact` tier choice.

---

## Subscribing

```erlang
{ok, SubRef} = macula:subscribe(Pool, Realm, Topic, Subscriber).
```

| Argument | Type | Notes |
|---|---|---|
| `Pool` | `pid()` | Returned by `macula:connect/2` |
| `Realm` | `<<_:256>>` | 32-byte realm tag |
| `Topic` | `binary()` | Built via `macula_topic` |
| `Subscriber` | `pid()` | Process that will receive events |

The subscriber receives:

```erlang
{macula_event, SubRef, Topic, Payload, Meta}
```

`Meta` is a map carrying delivery context:

| Key | Type | Meaning |
|---|---|---|
| `realm` | `<<_:256>>` | Realm tag (matches the subscribe call) |
| `publisher` | `binary()` | Publisher pubkey (the original publisher, not the relay) |
| `seq` | `non_neg_integer()` | Per-publisher monotonic sequence |
| `delivered_via` | `binary()` | Pubkey of the link/station that delivered this copy |

`{publisher, seq}` is the dedup key. The pool guarantees you see
each `(Realm, Publisher, Seq)` tuple **at most once**, even when the
same EVENT arrives via multiple links (e.g. with
`replication_factor > 1`).

### When the subscription ends

The subscriber receives **exactly one** terminal message:

```erlang
{macula_event_gone, SubRef, Reason}
```

`Reason` is one of:

| Reason | Cause |
|---|---|
| `pool_closed` | `macula:close(Pool)` was called |
| `{disconnected, _}` | The link supplying this sub was torn down (Phase 4 may quiet this when replay is in-flight) |

After `event_gone` arrives, no further events come for that `SubRef`.

### Subscribing with options

```erlang
{ok, SubRef} = macula:subscribe(Pool, Realm, Topic, Subscriber, Opts).
```

`Opts` is a forward-compatible map. Phase 1 honors no subscribe-time
options. Future phases (server-side filters, history replay) will
add named keys.

### Subscribing in a callback module

A common pattern: a `gen_server` subscribes in `init/1`, handles
events in `handle_info/2`.

```erlang
-module(my_orders_listener).
-behaviour(gen_server).

init(_Args) ->
    Pool  = my_app_mesh:pool(),
    Realm = my_app_mesh:realm(),
    Topic = macula_topic:app_fact(Realm, my_org, my_app,
                                  <<"orders">>, <<"placed">>, 1),
    {ok, Sub} = macula:subscribe(Pool, Realm, Topic, self()),
    {ok, #{sub => Sub}}.

handle_info({macula_event, Sub, _Topic, Payload, _Meta},
            #{sub := Sub} = S) ->
    on_order_placed(Payload),
    {noreply, S};
handle_info({macula_event_gone, Sub, Reason},
            #{sub := Sub} = S) ->
    %% Pool went away — supervisor will restart us.
    {stop, {pool_gone, Reason}, S}.
```

Pattern-match the `Sub` reference into the function head — that
keeps a process subscribing to multiple topics readable.

---

## Publishing

```erlang
ok = macula:publish(Pool, Realm, Topic, Payload).
```

| Argument | Type | Notes |
|---|---|---|
| `Pool` | `pid()` | The pool from `connect/2` |
| `Realm` | `<<_:256>>` | 32-byte realm tag |
| `Topic` | `binary()` | Built via `macula_topic` |
| `Payload` | `term()` | Encoded as MessagePack on the wire |

Returns:

| Return | Meaning |
|---|---|
| `ok` | At least one link accepted the PUBLISH frame |
| `{error, {transient, no_healthy_station}}` | The pool has zero spawned links — caller may retry |
| `{error, _}` | Other failures (validation, etc.) |

> **Partial success counts as success.** With `replication_factor > 1`,
> `publish/4` returns `ok` as soon as the first selected link accepts
> the frame. Subsequent links are best-effort.

### Publishing with options

```erlang
ok = macula:publish(Pool, Realm, Topic, Payload, #{timeout_ms => 1000}).
```

| Opt | Default | Meaning |
|---|---|---|
| `timeout_ms` | `5_000` | gen_server call timeout against the pool |

### Delivery guarantees

- **At-most-once** — fire and forget. No publisher-visible ack from
  subscribers.
- **Per-publisher ordering** — `seq` is monotonic per publisher per
  pool. Subscribers can detect gaps if they care.
- **Cross-publisher ordering** — none. Two publishers' events arrive
  in arbitrary interleaving.
- **Cross-link dedup** — the pool dedupes by `(Realm, Publisher,
  Seq)` over a 60-second window (configurable; see
  `dedup_window_ms` in [CONNECTING_GUIDE.md](CONNECTING_GUIDE.md)).
- **Cross-station gossip** — Phase 1 ships single-station fan-out
  only. A daemon connected to station A and a daemon connected to
  station B see each other only after Plumtree gossip lands (Phase 2
  / Plan C.2).

---

## Unsubscribing

```erlang
ok = macula:unsubscribe(Pool, SubRef).
```

Idempotent — unknown `SubRef` is a no-op. The subscriber pid does
**not** receive a `event_gone` message for an explicit
unsubscribe — `event_gone` is reserved for involuntary termination.

The wire-level subscription against the link persists for the pool's
lifetime. One wire sub per `(Realm, Topic)` is multiplexed across
local consumers; the pool drops the topic from its index when the
last local consumer leaves, but does not currently send UNSUBSCRIBE
on the wire (Phase 4 will tighten).

If the subscriber pid dies before calling `unsubscribe/2`, the pool
detects the `'DOWN'` and drops the sub spec automatically.

---

## Topic naming reference

Quick reference. Full specification:
[TOPIC_NAMING_GUIDE.md](TOPIC_NAMING_GUIDE.md).

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

- Restart your subscriber processes alongside the pool (one supervisor
  with `rest_for_one` strategy), or
- Watch for `{macula_event_gone, _, pool_closed}` and re-subscribe to
  the new pool.

### Multiple subscribers on one pool

A single pool can have arbitrarily many local subscribers. The pool
issues exactly one wire-level SUBSCRIBE per `(Realm, Topic)`,
multiplexes inbound events to every local subscriber for that pair,
and dedupes across links.

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
5. **Pattern-match the SubRef** in handlers when listening to
   multiple topics.
6. **Keep handlers fast** — spawn workers for heavy processing.
7. **Make handlers idempotent** — `(publisher, seq)` is a strong dedup
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

## How it works (relay side)

Routing behind the relay (DHT-based subscriber discovery, peering,
cross-station gossip, bloom filters, sticky-routing) is the relay's
concern. See
[hecate-station](https://github.com/hecate-social/hecate-station) for
the current relay implementation.

From the SDK side, you publish and subscribe; the pool handles the
rest.

---

## See also

- [Connecting Guide](CONNECTING_GUIDE.md) — pool model, lifecycle, options
- [Topic Naming Guide](TOPIC_NAMING_GUIDE.md) — canonical topic shape
- [V1 → V2 Migration](../migrations/V1_TO_V2_PUBSUB.md) — what changed in 3.11.0
- [Authorization Guide](AUTHORIZATION_GUIDE.md) — UCAN/DID identity
- [`macula_pubsub`](https://hexdocs.pm/macula/macula_pubsub.html) — slice module
- [`macula_client`](https://hexdocs.pm/macula/macula_client.html) — pool implementation
