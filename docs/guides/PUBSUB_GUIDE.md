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
%% DO NOT DO THIS
macula:publish(Client, <<"sensor.manchester.main-street.wind">>, #{speed => 42.5}).
```

**CORRECT** (IDs in payload):
```erlang
macula:publish(Client, <<"weather.wind_measured">>, #{
    station_id => <<"manchester-main-street">>,
    speed => 42.5,
    unit => <<"km/h">>,
    timestamp => erlang:system_time(millisecond)
}).
```

---

## Subscribing to Topics

```erlang
%% Subscribe with a callback function
{ok, SubRef} = macula:subscribe(Client, <<"sensors.temperature">>, fun(Msg) ->
    io:format("Temperature: ~p~n", [Msg])
end).

%% Subscribe with a process (receives {macula_event, Topic, Payload} messages)
{ok, SubRef} = macula:subscribe(Client, <<"sensors.temperature">>, self()).

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
ok = macula:publish(Client, <<"orders.placed">>, #{
    order_id => <<"ord-123">>,
    customer => <<"acme">>,
    total => 4999
}).

%% Publish binary payload
ok = macula:publish(Client, <<"logs.raw">>, <<"2026-04-09 ERROR: disk full">>).
```

### Delivery Guarantees

- **At-most-once** -- fire and forget. No acknowledgment from subscribers.
- The relay forwards to all subscribers on all connected relays (via peering).
- If a subscriber's connection drops, they miss messages during the gap.

---

## Topic Naming Conventions

Use dot-separated hierarchical names reflecting business domain:

```
weather.wind_measured         -- weather domain, wind measurement event
io.hecate.weather.{cc}.{city} -- namespaced with realm prefix
_mesh.node.up                 -- system topics (underscore prefix)
_mesh.relay.ping              -- relay infrastructure events
```

| Convention | Example | Use |
|------------|---------|-----|
| Business events | `orders.placed` | Application events |
| Namespaced | `io.macula.weather.de.berlin` | Multi-tenant isolation |
| System | `_mesh.node.up` | Infrastructure events (underscore prefix) |
| Wildcard-ready | `sensors.temperature` (not `sensors.temperature.sensor-1`) | ID in payload |

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
