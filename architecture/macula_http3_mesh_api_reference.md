# Macula HTTP/3 Mesh - API Reference

**Complete Erlang/Elixir API documentation**

**Status**: 🚧 SKELETON - Needs completion
**Priority**: P1
**Estimated effort**: 12 hours

---

## Overview

⚠️ **TODO**: Introduction to Macula APIs, conventions, return value patterns.

---

## Core Module: `macula`

### `macula:start/0,1`

⚠️ **TODO**: Document node startup.

**Signature**:
```erlang
start() -> {ok, Pid} | {error, Reason}.
start(Opts) -> {ok, Pid} | {error, Reason}.
```

**Elixir**:
```elixir
Macula.start() :: {:ok, pid()} | {:error, reason()}
Macula.start(opts) :: {:ok, pid()} | {:error, reason()}
```

### `macula:stop/0`

⚠️ **TODO**: Document graceful shutdown.

### `macula:node_id/0`

⚠️ **TODO**: Get current node ID.

### `macula:version/0`

⚠️ **TODO**: Get Macula version.

### `macula:stats/0`

⚠️ **TODO**: Get node statistics.

---

## Pub/Sub Module: `macula_pubsub`

### `macula_pubsub:publish/2,3`

⚠️ **TODO**: Complete publish API.

**Signature**:
```erlang
publish(Topic, Payload) -> ok | {error, Reason}.
publish(Topic, Payload, Opts) -> ok | {error, Reason}.
```

**Options**:
- `acknowledge` - Wait for delivery confirmation
- `retain` - Store for late subscribers
- `ttl` - Message time-to-live

### `macula_pubsub:subscribe/1,2,3`

⚠️ **TODO**: Complete subscribe API.

### `macula_pubsub:unsubscribe/1,2`

⚠️ **TODO**: Complete unsubscribe API.

---

## RPC Module: `macula_client`

Macula provides **fully decentralized RPC** via DHT-based service advertisement. Services advertise their capabilities to the Kademlia DHT, and clients discover providers by querying the DHT with local caching for performance.

**Key Features**:
- ✅ **Fully Decentralized** - No central authority, DHT-based discovery
- ✅ **Local-First Optimization** - Zero-latency for local services
- ✅ **Smart Caching** - DHT results cached to reduce queries
- ✅ **Graceful Degradation** - Continues operation when DHT unavailable
- ✅ **Multiple Providers** - Automatic load balancing across providers

### Service Advertisement Flow

```
Provider Node                          DHT Network                    Consumer Node
     │                                      │                              │
     │  1. advertise(Procedure, Handler)    │                              │
     │─────────────────────────────────────>│                              │
     │                                      │                              │
     │  2. DHT stores: hash(Procedure)      │                              │
     │     → [node_id, endpoint, metadata]  │                              │
     │                                      │                              │
     │                                      │  3. call(Procedure, Args)    │
     │                                      │<─────────────────────────────│
     │                                      │                              │
     │                                      │  4. DHT query: providers?    │
     │                                      │  → [list of providers]       │
     │                                      │─────────────────────────────>│
     │                                      │                              │
     │  5. MSG_CALL over HTTP/3 QUIC        │  6. Cache result (60s TTL)  │
     │<────────────────────────────────────────────────────────────────────│
     │                                      │                              │
     │  7. Handler executes → MSG_REPLY     │                              │
     │─────────────────────────────────────────────────────────────────────>│
```

### `macula_client:advertise/3,4`

Advertise a service with a handler function. The service is registered locally and published to the DHT for discovery by other nodes.

**Signature**:
```erlang
advertise(Client, Procedure, Handler) -> {ok, Ref} | {error, Reason}.
advertise(Client, Procedure, Handler, Opts) -> {ok, Ref} | {error, Reason}.

%% Types
Client :: pid().
Procedure :: binary().  % Service identifier (e.g., <<"energy.home.get">>)
Handler :: fun((Args :: map()) -> {ok, Result :: term()} | {error, Reason :: term()}).
Opts :: #{
    metadata => map(),      % Custom metadata (version, description, etc.)
    ttl => pos_integer()    % DHT advertisement TTL in seconds (default: 300)
}.
Ref :: reference().
```

**Elixir**:
```elixir
@spec advertise(pid(), binary(), handler_fn(), map()) ::
    {:ok, reference()} | {:error, term()}

MaculaClient.advertise(client, "energy.home.get", handler_fn, %{
  metadata: %{version: "1.0.0"},
  ttl: 300
})
```

**Example (Erlang)**:
```erlang
%% Define handler function
Handler = fun(Args) ->
    case Args of
        #{user_id := UserId} ->
            %% Fetch user from database
            {ok, #{user_id => UserId, name => <<"Alice">>, role => <<"admin">>}};
        _ ->
            {error, invalid_args}
    end
end,

%% Advertise service
{ok, Ref} = macula_client:advertise(
    Client,
    <<"myapp.user.get">>,
    Handler,
    #{
        metadata => #{version => <<"1.0.0">>, description => <<"Get user by ID">>},
        ttl => 300  % Re-advertise every 5 minutes
    }
).
```

**Example (Elixir)**:
```elixir
handler = fn
  %{user_id: user_id} ->
    # Fetch user from database
    {:ok, %{user_id: user_id, name: "Alice", role: :admin}}

  _ ->
    {:error, :invalid_args}
end

{:ok, ref} = :macula_client.advertise(
  client,
  "myapp.user.get",
  handler,
  %{
    metadata: %{version: "1.0.0", description: "Get user by ID"},
    ttl: 300
  }
)
```

**Behavior**:
1. Handler stored locally in service registry
2. Provider info published to DHT at `key = SHA256(Procedure)`
3. DHT stores: `#{node_id, endpoint, metadata, advertised_at, ttl}`
4. If DHT unavailable, logs warning but continues (local-only mode)

**Notes**:
- Handlers execute in spawned processes (non-blocking)
- TTL default is 300 seconds (5 minutes)
- Re-advertisement required before TTL expiration (future: automatic)

### `macula_client:call/2,3`

Call a remote (or local) service procedure. Discovery follows a 4-tier hierarchy for optimal performance.

**Signature**:
```erlang
call(Client, Procedure, Args) -> {ok, Result} | {error, Reason}.
call(Client, Procedure, Args, Opts) -> {ok, Result} | {error, Reason}.

%% Types
Client :: pid().
Procedure :: binary().
Args :: map().
Opts :: #{
    timeout => pos_integer(),  % Call timeout in ms (default: 5000)
    force_refresh => boolean() % Skip cache, force DHT query (default: false)
}.
Result :: term().
Reason :: timeout | service_not_found | {handler_error, term()} | term().
```

**Elixir**:
```elixir
@spec call(pid(), binary(), map(), map()) :: {:ok, term()} | {:error, term()}

MaculaClient.call(client, "myapp.user.get", %{user_id: "user-123"}, %{timeout: 10_000})
```

**Discovery Hierarchy** (checked in order):
1. **Local handler** - Zero latency if service advertised locally
2. **Cache hit** - Fast retrieval from local cache (60s TTL)
3. **DHT query** - Query DHT for providers, cache result
4. **Direct call** - Fallback to connected endpoint

**Example (Erlang)**:
```erlang
%% Call a service (discovers via DHT if not local/cached)
{ok, User} = macula_client:call(
    Client,
    <<"myapp.user.get">>,
    #{user_id => <<"user-123">>},
    #{timeout => 10000}
),

%% User = #{user_id => <<"user-123">>, name => <<"Alice">>, role => <<"admin">>}
```

**Example (Elixir)**:
```elixir
# Call a service
{:ok, user} = :macula_client.call(
  client,
  "myapp.user.get",
  %{user_id: "user-123"},
  %{timeout: 10_000}
)

# user = %{user_id: "user-123", name: "Alice", role: :admin}
```

**Error Handling**:
```erlang
case macula_client:call(Client, <<"service.procedure">>, Args) of
    {ok, Result} ->
        %% Success
        handle_result(Result);

    {error, timeout} ->
        %% Call timed out
        handle_timeout();

    {error, service_not_found} ->
        %% No providers found
        handle_not_found();

    {error, {handler_error, Reason}} ->
        %% Handler returned error
        handle_handler_error(Reason);

    {error, Reason} ->
        %% Other error
        handle_error(Reason)
end.
```

### `macula_client:unadvertise/2`

Stop advertising a service. Removes the local handler and attempts to remove from DHT (best-effort).

**Signature**:
```erlang
unadvertise(Client, Procedure) -> ok | {error, Reason}.

%% Types
Client :: pid().
Procedure :: binary().
```

**Elixir**:
```elixir
@spec unadvertise(pid(), binary()) :: :ok | {:error, term()}

MaculaClient.unadvertise(client, "myapp.user.get")
```

**Example**:
```erlang
%% Stop advertising
ok = macula_client:unadvertise(Client, <<"myapp.user.get">>).
```

**Behavior**:
1. Removes local handler from service registry
2. Attempts DHT removal (best-effort, may fail gracefully)
3. DHT entries expire naturally via TTL anyway

**Notes**:
- DHT removal is best-effort (logged but doesn't fail)
- Entries expire automatically after TTL
- Cache on consumer nodes expires after 60 seconds

### Service Registry Module: `macula_service_registry`

Low-level registry operations (typically used internally by `macula_client`).

#### `macula_service_registry:publish_to_dht/5`

Publish service advertisement to DHT.

**Signature**:
```erlang
publish_to_dht(DhtPid, ServiceId, ProviderInfo, TTL, K) -> ok | {error, Reason}.

%% Types
DhtPid :: pid() | atom().  % DHT server PID or registered name
ServiceId :: binary().
ProviderInfo :: #{
    node_id := binary(),
    endpoint := binary(),
    metadata := map()
}.
TTL :: pos_integer().     % Seconds
K :: pos_integer().       % Kademlia K-value (typically 20)
```

**Example**:
```erlang
ProviderInfo = #{
    node_id => <<"node-abc-123">>,
    endpoint => <<"https://node1.local:9443">>,
    metadata => #{version => <<"1.0.0">>}
},

ok = macula_service_registry:publish_to_dht(
    macula_routing_server,
    <<"energy.home.get">>,
    ProviderInfo,
    300,  % 5 minutes
    20    % K-value
).
```

#### `macula_service_registry:query_dht_for_service/3`

Query DHT for service providers.

**Signature**:
```erlang
query_dht_for_service(DhtPid, ServiceId, K) -> {ok, [ProviderInfo]} | {error, Reason}.

%% Returns empty list if no providers found
```

**Example**:
```erlang
{ok, Providers} = macula_service_registry:query_dht_for_service(
    macula_routing_server,
    <<"energy.home.get">>,
    20
),

%% Providers = [
%%     #{node_id => <<"node-1">>, endpoint => <<"https://...">>, ...},
%%     #{node_id => <<"node-2">>, endpoint => <<"https://...">>, ...}
%% ]
```

#### `macula_service_registry:remove_from_dht/3`

Remove service advertisement from DHT (best-effort).

**Signature**:
```erlang
remove_from_dht(DhtPid, ServiceId, NodeId) -> ok | {error, Reason}.
```

---

## Connection Module: `macula_connection`

⚠️ **TODO**: Document connection management APIs.

---

## Topology Module: `macula_topology`

⚠️ **TODO**: Document topology APIs.

---

## Membership Module: `macula_membership`

⚠️ **TODO**: Document SWIM membership APIs.

---

## Routing Module: `macula_routing`

⚠️ **TODO**: Document DHT routing APIs.

---

## Gateway Module: `macula_gateway`

⚠️ **TODO**: Document gateway APIs for cross-realm communication.

---

## Configuration

⚠️ **TODO**: Document all configuration options.

**Sections needed**:
- Application environment variables
- Runtime configuration
- Config file format
- Default values

---

## Error Reference

⚠️ **TODO**: List all possible error returns and their meanings.

---

## Examples

⚠️ **TODO**: Add comprehensive code examples for common use cases.

---

**Last Updated**: 2025-01-08
**Contributors**: [Add names as sections are completed]
