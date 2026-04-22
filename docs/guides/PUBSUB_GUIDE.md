# Macula SDK - Pub/Sub Guide

**Topic-based messaging over the relay mesh**

![Pub/Sub Architecture](assets/pubsub_flow.svg)

---

## Overview

Macula provides decentralized publish/subscribe messaging. SDK clients subscribe to topics and publish events through their relay connection. Relays handle cross-relay routing via DHT-based subscriber discovery.

From the SDK perspective, pub/sub is simple: subscribe, publish, unsubscribe.

---

## Core Principle: Event Types in Topics, IDs in Payloads

> **Non-negotiable for scalable pub/sub.**

```
Topic   = WHAT happened (event type)
Payload = WHO/WHERE/WHEN it happened (entity details)
```

| Approach | 1M Sensors | Topics Created | Result |
|----------|------------|----------------|--------|
| ID in topic | 1M sensors | 1M topics | DHT explosion, memory exhaustion |
| ID in payload | 1M sensors | 1 topic | Scalable, efficient routing |

**WRONG** (causes topic explosion):
```erlang
%% DO NOT DO THIS — ID in topic
Topic = macula_topic:app_fact(Realm, Org, App,
                              <<"weather">>,
                              <<"manchester_main_street_wind_measured">>, 1),
macula:publish(Client, Topic, #{speed => 42.5}).
```

**CORRECT** (IDs in payload):
```erlang
Topic = macula_topic:app_fact(Realm, Org, App,
                              <<"weather">>, <<"wind_measured">>, 1),
macula:publish(Client, Topic, #{
    station_id => <<"manchester-main-street">>,
    speed => 42.5,
    unit => <<"km/h">>,
    timestamp => erlang:system_time(millisecond)
}).
```

---

## Subscribing to Topics

Always build the topic via `macula_topic` — direct strings will be rejected by the client validator.

```erlang
Topic = macula_topic:app_fact(Realm, Org, App,
                              <<"sensors">>, <<"temperature_measured">>, 1),

%% Subscribe with a callback function
{ok, SubRef} = macula:subscribe(Client, Topic, fun(Msg) ->
    io:format("Temperature: ~p~n", [Msg])
end).

%% Subscribe with a process (receives {macula_event, Topic, Payload} messages)
{ok, SubRef} = macula:subscribe(Client, Topic, self()).

%% Unsubscribe
ok = macula:unsubscribe(Client, SubRef).
```

### Subscription Semantics

- Subscriptions are **per-connection** -- if the relay connection drops and reconnects, subscriptions are re-established automatically.
- Multiple callbacks can subscribe to the same topic.
- Callbacks execute in the connection process -- keep them fast. For heavy work, spawn a worker.

---

## Publishing Events

```erlang
%% Publish a map payload (encoded as MessagePack on the wire)
OrdersTopic = macula_topic:app_fact(Realm, Org, App,
                                    <<"orders">>, <<"placed">>, 1),
ok = macula:publish(Client, OrdersTopic, #{
    order_id => <<"ord-123">>,
    customer => <<"acme">>,
    total => 4999
}).

%% Publish binary payload
LogsTopic = macula_topic:app_fact(Realm, Org, App,
                                  <<"logs">>, <<"raw_recorded">>, 1),
ok = macula:publish(Client, LogsTopic, <<"2026-04-09 ERROR: disk full">>).
```

### Delivery Guarantees

- **At-most-once** -- fire and forget. No acknowledgment from subscribers.
- The relay forwards to all subscribers on all connected relays (via peering).
- If a subscriber's connection drops, they miss messages during the gap.

---

## Topic Naming Conventions

**See [TOPIC_NAMING_GUIDE.md](TOPIC_NAMING_GUIDE.md) — the canonical specification.**

Quick summary:
- Every topic is exactly 5 slash-separated segments: `{realm}/{publisher}/{publisher}/{domain}/{name}_v{N}`
- Pick a tier: `realm_fact/realm_hope`, `org_fact/org_hope`, or `app_fact/app_hope`
- Build via `macula_topic` — never inline strings
- Past tense for facts, present tense for hopes
- IDs in payload, never in topic
- System topics (`_mesh.*`) are infrastructure-owned and dot-separated; do not use in app code

---

## Error Handling

```erlang
case macula:subscribe(Client, Topic, Callback) of
    {ok, SubRef} ->
        %% Subscribed successfully
        SubRef;
    {error, not_connected} ->
        %% Client not connected to relay
        retry_later;
    {error, Reason} ->
        logger:warning("Subscribe failed: ~p", [Reason])
end.
```

---

## Best Practices

1. **Keep callbacks fast** -- spawn workers for heavy processing
2. **IDs in payloads, not topics** -- avoid topic explosion at scale
3. **Use business event names** -- `order_placed`, not `order_updated` (no CRUD)
4. **Include timestamps** -- `erlang:system_time(millisecond)` in every payload
5. **Idempotent subscribers** -- messages may arrive out of order across relays
6. **Monitor connection** -- re-subscribe after reconnection if needed

---

## How It Works (Relay Side)

For relay operators and contributors: pub/sub routing, DHT subscriber discovery,
bloom filter optimization, and cross-relay forwarding are documented in
[macula-relay](https://github.com/macula-io/macula-relay).
