# Macula SDK — Pub/Sub Protocol

**The raw wire primitives underneath `macula_publisher` / `macula_subscriber`.**

> **Audience:** building something the supervised wrappers don't fit —
> custom retry logic, observability, an SDK for another language. Most
> applications want the [PubSub Guide](PUBSUB_GUIDE.md) instead — it covers
> the same capability via `macula_publisher`/`macula_subscriber`, with an
> addressable pid, cancel, and mesh facts already wired in.

---

## Raw primitives, end to end

```erlang
%% 1. Connect to the mesh — see ../shared/CONNECTING_GUIDE.md.
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

This is what [`macula_subscriber` and `macula_publisher` wrap](PUBSUB_GUIDE.md) —
an addressable pid you can monitor and cancel, `pubsub.*_v1` mesh facts
around each operation. Reach for the raw calls below directly only if
you're building something the wrappers don't fit.

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

`Meta` is a map carrying delivery context — the same map
[`macula_subscriber:handle_event/4` receives as its third argument](PUBSUB_GUIDE.md#subscribing):

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

The only way a live subscription produces a terminal message today is
the pool closing:

```erlang
{macula_event_gone, SubRef, pool_closed}
```

A link dying does **not** send this — the pool logs
`_macula.client.link_down`, schedules a respawn, and silently
re-issues the subscription against the new link once it's up (see
[Connecting Guide](../shared/CONNECTING_GUIDE.md#lifecycle)). A subscriber sees
no gap-signaling message for that case, only a possible gap in
delivery itself, which `ordered` mode's `order_timeout_ms` skip
handles the same way it handles any other loss.

After `event_gone` arrives, no further events come for that `SubRef`.
`macula_subscriber` stops its sink with this same reason — see
[When the subscription ends](PUBSUB_GUIDE.md#when-the-subscription-ends) in
the Guide.

For `delivery` options (`ordered` / `latest_only` / `as_arrives`) and
`(publisher, seq)` dedup semantics, see the Guide's
[Subscribing with options](PUBSUB_GUIDE.md#subscribing-with-options-delivery-ordering) —
`Opts` is identical whether passed here or through
`macula_subscriber:start_link/6`.

### Subscribing in a callback module

The raw pattern [`macula_subscriber` wraps](PUBSUB_GUIDE.md#subscribing-with-macula_subscriber-supervised):
a `gen_server` subscribes in `init/1`, handles events in
`handle_info/2` by hand. Reach for this directly only if you're building
something the wrapper doesn't fit.

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

This is what [`macula_publisher` wraps](PUBSUB_GUIDE.md#publishing-with-macula_publisher-supervised) —
`start_link/5,6` returns an addressable pid instead of blocking the
caller, and delivers the same `ok | {error, term()}` outcome to
`Module:handle_published/2`.

### Publishing with options

```erlang
ok = macula:publish(Pool, Realm, Topic, Payload, #{timeout_ms => 1000}).
```

| Opt | Default | Meaning |
|---|---|---|
| `timeout_ms` | `5_000` | gen_server call timeout against the pool |

`macula_publisher:start_link/6`'s last argument is `Args` (for
`Module:init/1`), not `Opts` — there is no way to reach `timeout_ms`
through the wrapper. Call `macula:publish/5` directly if you need it.

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
detects the `'DOWN'` and drops the sub spec automatically — the same
cleanup a `macula_subscriber` sink gets for free when its own process
terminates (see [Subscribing with `macula_subscriber`](PUBSUB_GUIDE.md#subscribing-with-macula_subscriber-supervised)
in the Guide).

---

## How it works (relay side)

Routing behind the relay (DHT-based subscriber discovery, peering,
cross-station gossip, bloom filters, sticky-routing) is the relay's
concern. See macula-station for the current relay implementation.

From the SDK side, you publish and subscribe; the pool handles the
rest.

---

## See also

- [PUBSUB_GUIDE.md](PUBSUB_GUIDE.md) — the supervised wrappers most
  applications should use instead of these raw primitives.
