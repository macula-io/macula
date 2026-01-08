# Macula HTTP/3 Mesh - Pub/Sub Guide

**Complete guide to decentralized publish/subscribe with DHT-based subscriber discovery**

![Pub/Sub Architecture](assets/pubsub_flow.svg)

**Status**: COMPLETE
**Last Updated**: 2025-11-28

---

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Subscribing to Topics](#subscribing-to-topics)
4. [Publishing Events](#publishing-events)
5. [Wildcard Subscriptions](#wildcard-subscriptions)
6. [Error Handling](#error-handling)
7. [Performance Optimization](#performance-optimization)
8. [Best Practices](#best-practices)
9. [Examples](#examples)
10. [Migration from WAMP](#migration-from-wamp)

---

## Overview

Macula provides **fully decentralized pub/sub** without requiring any central message broker. Subscribers advertise their interest to a **Kademlia DHT** (Distributed Hash Table), and publishers discover subscribers by querying the DHT.

### Key Features

- **Fully Decentralized** - No central broker, DHT-based subscriber discovery
- **Topic-Based Routing** - Events routed to all topic subscribers
- **Wildcard Support** - Subscribe to patterns like `sensor.*` or `#`
- **At-Least-Once Delivery** - Direct delivery via QUIC connections
- **Multi-Subscriber Fanout** - One publish reaches all matching subscribers
- **NAT-Friendly** - HTTP/3 QUIC works through firewalls

### How It Works

The diagram above illustrates the complete pub/sub flow:

1. **Subscription Registration** - Subscribers advertise to DHT
2. **Publishing Process** - Publishers query DHT and fanout to subscribers
3. **Message Delivery** - Direct QUIC streams to all matching subscribers
4. **Module Architecture** - Layered design from API to transport
5. **Wildcard Matching** - Single-level (`*`) and multi-level (`#`) patterns

---

## Architecture

### Components

#### `macula_pubsub_dht`

DHT integration for pub/sub:
- **Subscriber advertisement** - Store subscriptions in DHT
- **Subscriber discovery** - Query DHT for topic subscribers
- **Topic matching** - Support for exact and wildcard topics

**Key Functions**:
- `subscribe/3` - Advertise subscription to DHT
- `unsubscribe/2` - Remove subscription from DHT
- `find_subscribers/2` - Query DHT for topic subscribers
- `match_topic/2` - Match topic against patterns

#### `macula_pubsub_handler`

Connection-level pub/sub handler:
- Holds local subscriptions and callbacks
- Handles incoming `MSG_PUBLISH` messages
- Invokes subscriber callbacks

#### `macula_gateway_pubsub`

Gateway pub/sub routing:
- Routes messages to connected clients
- Handles wildcard matching
- Manages subscription state for gateway clients

#### `macula` (Public API)

The **only module** applications should import:
- `connect/2`, `connect_local/1` - Connect to mesh
- `subscribe/3` - Subscribe to a topic
- `unsubscribe/2` - Unsubscribe from a topic
- `publish/3,4` - Publish an event
- `disconnect/1` - Close connection

#### `macula_peer` (Internal)

Internal mesh participant module (called by `macula`).

### Data Flow

**Subscription**:
```
Application
   |
macula:subscribe(Client, Topic, Callback)
   |
macula_connection (gen_server:call)
   |
macula_pubsub_handler (store local callback)
   |
macula_pubsub_dht:subscribe(DhtPid, Topic, SubscriberInfo)
   |
macula_routing_dht (DHT storage at key=SHA256(Topic))
```

**Publishing**:
```
Application
   |
macula:publish(Client, Topic, Payload)
   |
macula_connection (gen_server:call)
   |
macula_pubsub_dht:find_subscribers(DhtPid, Topic)
   -> Returns list of subscriber endpoints
   |
For each subscriber:
   |
Send MSG_PUBLISH over HTTP/3 QUIC
   |
Subscriber's macula_pubsub_handler
   |
Invoke registered callback with event
```

---

## Subscribing to Topics

### Basic Subscription

```erlang
%% Erlang
Callback = fun(Event) ->
    #{topic := Topic, payload := Payload} = Event,
    io:format("Received ~s: ~p~n", [Topic, Payload])
end,

{ok, Ref} = macula:subscribe(
    Peer,
    <<"sensor.temperature">>,
    Callback
).
```

```elixir
# Elixir
callback = fn event ->
  %{topic: topic, payload: payload} = event
  IO.puts("Received #{topic}: #{inspect(payload)}")
end

{:ok, ref} = :macula.subscribe(
  client,
  "sensor.temperature",
  callback
)
```

### Callback Contract

Callbacks receive events as maps:

```erlang
-type event() :: #{
    topic := binary(),
    payload := term(),
    publisher_id => binary(),
    timestamp => integer()
}.

-type callback_fn() :: fun((event()) -> ok | {error, term()}).
```

**Callback Rules**:
- Input: Event map with `topic` and `payload`
- Output: `ok` or `{error, Reason}` (return value is ignored for at-least-once)
- Execution: Callbacks run in **spawned processes** (non-blocking)
- Errors: Callback crashes are logged but don't affect other subscribers

### Process-Based Subscription

Send events to a process instead of callback:

```erlang
%% Subscribe with process pid
{ok, Ref} = macula:subscribe(
    Peer,
    <<"sensor.temperature">>,
    self()  % Events sent as messages
).

%% Handle events in receive loop
receive
    {macula_event, Topic, Payload} ->
        handle_event(Topic, Payload)
end.
```

### Unsubscribing

```erlang
ok = macula:unsubscribe(Client, <<"sensor.temperature">>).
```

**Behavior**:
1. Removes local callback from subscription list
2. Attempts to remove from DHT (best-effort)
3. DHT entries expire naturally via TTL

---

## Publishing Events

### Basic Publishing

```erlang
%% Erlang
ok = macula:publish(
    Peer,
    <<"sensor.temperature">>,
    #{
        value => 21.5,
        unit => <<"celsius">>,
        timestamp => erlang:system_time(millisecond)
    }
).
```

```elixir
# Elixir
:ok = :macula.publish(
  client,
  "sensor.temperature",
  %{
    value: 21.5,
    unit: "celsius",
    timestamp: System.system_time(:millisecond)
  }
)
```

### Fire-and-Forget Semantics

Publishing is **asynchronous** by default:
- `publish/3` returns immediately after initiating delivery
- Delivery happens in background
- No confirmation that all subscribers received the message

### With Options

```erlang
ok = macula:publish(
    Peer,
    <<"sensor.temperature">>,
    Payload,
    #{
        exclude_self => true,    % Don't deliver to self
        retain => false          % Don't retain message (future feature)
    }
).
```

---

## Wildcard Subscriptions

### Wildcard Types

Macula supports two wildcard patterns:

| Pattern | Matches | Example |
|---------|---------|---------|
| `*` | Single segment | `sensor.*` matches `sensor.temperature`, `sensor.humidity` |
| `#` | Multiple segments | `sensor.#` matches `sensor.room1.temperature`, `sensor.room2.humidity.indoor` |

### Single-Level Wildcard (*)

```erlang
%% Subscribe to all sensors (one level)
{ok, _} = macula:subscribe(
    Peer,
    <<"sensor.*">>,
    fun(#{topic := Topic, payload := Payload}) ->
        io:format("Sensor event on ~s: ~p~n", [Topic, Payload])
    end
).

%% These will match:
macula:publish(Client, <<"sensor.temperature">>, #{value => 21.5}),
macula:publish(Client, <<"sensor.humidity">>, #{value => 65}),

%% These will NOT match:
macula:publish(Client, <<"sensor.room1.temperature">>, #{value => 22.0}),
macula:publish(Client, <<"device.sensor">>, #{value => 10}).
```

### Multi-Level Wildcard (#)

```erlang
%% Subscribe to all sensor events (any depth)
{ok, _} = macula:subscribe(
    Peer,
    <<"sensor.#">>,
    fun(#{topic := Topic, payload := Payload}) ->
        io:format("Sensor event on ~s: ~p~n", [Topic, Payload])
    end
).

%% All of these will match:
macula:publish(Client, <<"sensor.temperature">>, #{value => 21.5}),
macula:publish(Client, <<"sensor.room1.temperature">>, #{value => 22.0}),
macula:publish(Client, <<"sensor.building.floor3.room42.humidity">>, #{value => 55}).
```

### Topic Matching Algorithm

```erlang
%% Internal matching logic
match_topic(<<"sensor.*">>, <<"sensor.temperature">>) -> true;
match_topic(<<"sensor.*">>, <<"sensor.room1.temp">>) -> false;
match_topic(<<"sensor.#">>, <<"sensor.room1.temp">>) -> true;
match_topic(<<"sensor.*.temperature">>, <<"sensor.room1.temperature">>) -> true;
match_topic(Pattern, Topic) ->
    %% Split on dots and match segment by segment
    match_segments(binary:split(Pattern, <<".">>, [global]),
                   binary:split(Topic, <<".">>, [global])).
```

---

## Error Handling

### Subscription Errors

```erlang
case macula:subscribe(Client, Topic, Callback) of
    {ok, Ref} ->
        %% Successfully subscribed
        {ok, Ref};

    {error, invalid_topic} ->
        %% Topic format is invalid
        {error, bad_topic};

    {error, {dht_error, Reason}} ->
        %% Failed to advertise to DHT
        %% Subscription still works locally
        ?LOG_WARNING("DHT advertisement failed: ~p", [Reason]),
        {error, partial_subscription};

    {error, Reason} ->
        {error, Reason}
end.
```

### Publishing Errors

```erlang
case macula:publish(Client, Topic, Payload) of
    ok ->
        %% Published to at least one subscriber (or no subscribers exist)
        ok;

    {error, no_subscribers} ->
        %% No subscribers found for topic
        %% This may be expected behavior
        ok;

    {error, {partial_delivery, FailedNodes}} ->
        %% Some subscribers unreachable
        ?LOG_WARNING("Failed to deliver to: ~p", [FailedNodes]),
        ok;

    {error, Reason} ->
        {error, Reason}
end.
```

### Callback Error Handling

```erlang
%% Defensive callback implementation
Callback = fun(Event) ->
    try
        #{topic := Topic, payload := Payload} = Event,
        process_event(Topic, Payload),
        ok
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Callback error: ~p:~p~n~p",
                       [Class, Reason, Stacktrace]),
            {error, callback_failed}
    end
end.
```

---

## Performance Optimization

### Subscriber Caching

Publishers cache subscriber lists:
- **Cache TTL**: 60 seconds
- **DHT queries**: Only on cache miss
- **Cache hit ratio**: ~98% for frequent publishes

```
Example:
- Topic published 100 times/minute
- Cache TTL = 60 seconds
- DHT queries = ~2/minute
- Cache hit ratio = 98%
```

### Batching Strategy

For high-throughput scenarios, batch messages:

```erlang
%% Instead of many individual publishes
lists:foreach(fun(Value) ->
    macula:publish(Client, <<"sensor.data">>, #{value => Value})
end, Values),

%% Consider batching
macula:publish(Client, <<"sensor.data.batch">>, #{
    values => Values,
    count => length(Values),
    timestamp => erlang:system_time(millisecond)
}).
```

### Connection Pooling

The gateway maintains connection pools to subscribers:
- **Max connections**: 1,000 (configurable)
- **LRU eviction**: Least recently used connections evicted when pool is full
- **Connection reuse**: QUIC streams multiplexed on single connection

### Topic Design for Performance

**Avoid**:
```erlang
%% Too many unique topics (one per entity)
<<"sensor.", SensorId/binary, ".temperature">>  % Millions of topics
```

**Prefer**:
```erlang
%% Fewer topics with IDs in payload
<<"sensor.temperature">>  % Single topic
#{sensor_id => SensorId, value => Value}  % ID in payload
```

---

## Best Practices

### Topic Naming

Use **hierarchical naming** with dot-separated segments:

```erlang
%% Good - hierarchical, namespaced
<<"myapp.sensor.temperature">>
<<"myapp.user.status">>
<<"energy.home.consumption">>

%% Avoid
<<"temperature">>          % Not namespaced
<<"user-status">>          % Use dots, not dashes
<<"myapp_sensor_temp">>    % Use dots, not underscores
```

### Payload Design

**Include metadata in payloads**:
```erlang
#{
    %% Required data
    value => 21.5,

    %% Contextual metadata
    sensor_id => <<"sensor-001">>,
    location => <<"room-42">>,
    timestamp => erlang:system_time(millisecond),

    %% Optional metadata
    unit => <<"celsius">>,
    source => <<"direct">>
}
```

### Callback Design

**Keep callbacks simple and fast**:
```erlang
%% Good - simple, delegates heavy work
Callback = fun(Event) ->
    %% Quick validation
    case validate_event(Event) of
        ok ->
            %% Delegate to worker for heavy processing
            gen_server:cast(my_worker, {process_event, Event});
        {error, _} ->
            ok  % Ignore invalid events
    end
end.

%% Avoid - heavy processing in callback
Callback = fun(Event) ->
    %% Don't do database writes or HTTP calls here
    #{payload := Payload} = Event,
    database:insert(payload_table, Payload),  % Blocking!
    http_client:post(webhook_url, Payload)    % Blocking!
end.
```

### Subscription Lifecycle

**Subscribe early, unsubscribe on shutdown**:
```erlang
init([]) ->
    {ok, Client} = macula:connect(Endpoint, #{}),
    {ok, Ref} = macula:subscribe(Client, <<"events.#">>, self()),
    {ok, #{client => Client, subscription => Ref}}.

terminate(_Reason, #{client := Client}) ->
    %% Clean up subscription
    macula:unsubscribe(Client, <<"events.#">>),
    macula:disconnect(Client),
    ok.
```

### Error Recovery

**Handle reconnection gracefully**:
```erlang
handle_info({macula_disconnected, Reason}, State) ->
    ?LOG_WARNING("Disconnected: ~p, reconnecting...", [Reason]),
    timer:send_after(5000, reconnect),
    {noreply, State#{connected => false}};

handle_info(reconnect, #{endpoint := Endpoint} = State) ->
    case macula:connect(Endpoint, #{}) of
        {ok, Client} ->
            %% Re-subscribe to topics
            resubscribe(Client, State),
            {noreply, State#{client => Client, connected => true}};
        {error, _} ->
            timer:send_after(10000, reconnect),
            {noreply, State}
    end.
```

---

## Examples

### Example 1: Temperature Monitoring

```erlang
%% temperature_monitor.erl
-module(temperature_monitor).
-behaviour(gen_server).
-export([start_link/1, init/1, handle_info/2]).

start_link(Endpoint) ->
    gen_server:start_link(?MODULE, [Endpoint], []).

init([Endpoint]) ->
    {ok, Client} = macula:connect(Endpoint, #{}),

    %% Subscribe to all temperature sensors
    {ok, _} = macula:subscribe(
        Peer,
        <<"sensor.*.temperature">>,
        self()
    ),

    {ok, #{client => Client, readings => #{}}}.

handle_info({macula_event, Topic, Payload}, State) ->
    #{value := Value, sensor_id := SensorId} = Payload,

    %% Check for alerts
    case Value > 30.0 of
        true ->
            alert_high_temperature(SensorId, Value);
        false ->
            ok
    end,

    %% Store reading
    Readings = maps:put(SensorId, Value, maps:get(readings, State)),
    {noreply, State#{readings => Readings}}.

alert_high_temperature(SensorId, Value) ->
    io:format("ALERT: ~s temperature is ~.1f C!~n", [SensorId, Value]).
```

### Example 2: Event Bus Pattern

```erlang
%% event_bus.erl
-module(event_bus).
-export([start/1, subscribe/2, publish/2]).

start(Endpoint) ->
    {ok, Client} = macula:connect(Endpoint, #{}),
    register(?MODULE, spawn(fun() -> loop(Client) end)),
    {ok, Client}.

subscribe(Topic, Handler) ->
    ?MODULE ! {subscribe, Topic, Handler}.

publish(Topic, Event) ->
    ?MODULE ! {publish, Topic, Event}.

loop(Client) ->
    receive
        {subscribe, Topic, Handler} ->
            macula:subscribe(Client, Topic, Handler),
            loop(Client);

        {publish, Topic, Event} ->
            macula:publish(Client, Topic, Event),
            loop(Client)
    end.

%% Usage
event_bus:start(<<"https://localhost:9443">>),

%% Subscribe to user events
event_bus:subscribe(<<"user.#">>, fun(E) ->
    io:format("User event: ~p~n", [E])
end),

%% Publish events
event_bus:publish(<<"user.created">>, #{user_id => <<"123">>}),
event_bus:publish(<<"user.updated">>, #{user_id => <<"123">>, name => <<"Alice">>}).
```

### Example 3: Real-Time Dashboard

```elixir
# lib/my_app/realtime_dashboard.ex
defmodule MyApp.RealtimeDashboard do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    {:ok, client} = :macula.connect("https://localhost:9443", %{})

    # Subscribe to multiple topics
    topics = [
      "metrics.cpu",
      "metrics.memory",
      "metrics.disk",
      "alerts.#"
    ]

    Enum.each(topics, fn topic ->
      :macula.subscribe(client, topic, self())
    end)

    {:ok, %{client: client, metrics: %{}}}
  end

  def handle_info({:macula_event, topic, payload}, state) do
    # Broadcast to Phoenix channels
    MyAppWeb.Endpoint.broadcast!("dashboard:metrics", "update", %{
      topic: topic,
      data: payload,
      timestamp: DateTime.utc_now()
    })

    # Update local state
    metrics = Map.put(state.metrics, topic, payload)
    {:noreply, %{state | metrics: metrics}}
  end
end
```

### Example 4: Distributed Sensor Network

```erlang
%% sensor_node.erl - Runs on each sensor
-module(sensor_node).
-export([start/2]).

start(Gateway, SensorId) ->
    {ok, Client} = macula:connect(Gateway, #{}),
    publish_loop(Client, SensorId).

publish_loop(Client, SensorId) ->
    %% Read sensor value
    Value = read_temperature_sensor(),

    %% Publish reading
    Topic = <<"sensor.temperature">>,
    Payload = #{
        sensor_id => SensorId,
        value => Value,
        timestamp => erlang:system_time(millisecond),
        location => get_location()
    },

    macula:publish(Client, Topic, Payload),

    %% Wait and repeat
    timer:sleep(1000),
    publish_loop(Client, SensorId).

%% aggregator.erl - Runs on aggregation node
-module(aggregator).
-export([start/1]).

start(Gateway) ->
    {ok, Client} = macula:connect(Gateway, #{}),

    %% Subscribe to all sensors
    {ok, _} = macula:subscribe(
        Peer,
        <<"sensor.#">>,
        fun(Event) ->
            #{payload := #{sensor_id := Id, value := V}} = Event,
            store_reading(Id, V),
            update_dashboard(Id, V)
        end
    ),

    {ok, Client}.
```

---

## Migration from WAMP

### Key Differences

| Aspect | WAMP (Bondy) | Macula HTTP/3 |
|--------|--------------|---------------|
| **Discovery** | Centralized broker | DHT-based (decentralized) |
| **Transport** | WebSocket | HTTP/3 QUIC |
| **Subscribe** | `session.subscribe(Topic, Handler)` | `macula:subscribe(Client, Topic, Handler)` |
| **Publish** | `session.publish(Topic, Args)` | `macula:publish(Client, Topic, Payload)` |
| **Unsubscribe** | `session.unsubscribe(Subscription)` | `macula:unsubscribe(Client, Topic)` |
| **Event Format** | `[Args, Kwargs]` | `#{topic, payload}` map |
| **NAT Traversal** | Requires config | Built-in (QUIC) |

### Migration Steps

#### 1. Update Dependencies

```erlang
%% Before (WAMP)
{deps, [
    {bondy, {git, "https://github.com/bondy-io/bondy.git", {tag, "1.0.0"}}}
]}.

%% After (Macula)
{deps, [
    {macula, "0.10.0"}
]}.
```

#### 2. Convert Subscriptions

```erlang
%% Before (WAMP)
Handler = fun([Args, Kwargs]) ->
    Value = proplists:get_value(<<"value">>, Kwargs),
    process_value(Value)
end,
{ok, Subscription} = bondy:subscribe(Session, <<"sensor.temperature">>, Handler).

%% After (Macula)
Handler = fun(Event) ->
    #{payload := #{value := Value}} = Event,
    process_value(Value),
    ok
end,
{ok, Ref} = macula:subscribe(Client, <<"sensor.temperature">>, Handler).
```

#### 3. Convert Publishing

```erlang
%% Before (WAMP)
bondy:publish(Session, <<"sensor.temperature">>, [#{value => 21.5}]).

%% After (Macula)
macula:publish(Client, <<"sensor.temperature">>, #{value => 21.5}).
```

#### 4. Update Event Handlers

```erlang
%% Before (WAMP)
handle_event([Args, Kwargs]) ->
    Value = proplists:get_value(<<"value">>, Kwargs),
    Timestamp = proplists:get_value(<<"timestamp">>, Kwargs),
    {Value, Timestamp}.

%% After (Macula)
handle_event(Event) ->
    #{payload := Payload} = Event,
    #{value := Value, timestamp := Timestamp} = Payload,
    {Value, Timestamp}.
```

### Migration Checklist

- [ ] Update dependencies (WAMP -> Macula)
- [ ] Convert subscription calls
- [ ] Update event handler signatures
- [ ] Replace `bondy:publish/3` with `macula:publish/3`
- [ ] Replace `bondy:subscribe/3` with `macula:subscribe/3`
- [ ] Update error handling patterns
- [ ] Test wildcard subscriptions
- [ ] Verify event delivery under load
- [ ] Update monitoring and logging

---

## See Also

- [RPC Guide](RPC_GUIDE.md) - Remote procedure calls with DHT discovery
- [Quick Start](../user/QUICK_START.md) - Getting started tutorial
- [Hello World](../user/HELLO_WORLD.md) - Your first Macula application
- [Architecture](../../ARCHITECTURE.md) - System architecture overview

---

**Last Updated**: 2025-11-28
**Status**: Complete
