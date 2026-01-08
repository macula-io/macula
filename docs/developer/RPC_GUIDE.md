# Macula HTTP/3 Mesh - RPC Guide

**Complete guide to decentralized RPC with DHT-based service discovery**

![RPC Architecture](artwork/rpc_flow.svg)

**Status**: ✅ COMPLETE
**Last Updated**: 2025-01-10

---

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Service Advertisement](#service-advertisement)
4. [Service Discovery](#service-discovery)
5. [Making RPC Calls](#making-rpc-calls)
6. [Error Handling](#error-handling)
7. [Performance Optimization](#performance-optimization)
8. [Best Practices](#best-practices)
9. [Examples](#examples)
10. [Migration from WAMP](#migration-from-wamp)

---

## Overview

Macula provides **fully decentralized RPC** without requiring any central registry or broker. Services advertise themselves to a **Kademlia DHT** (Distributed Hash Table), and consumers discover providers by querying the DHT.

### Key Features

✅ **Fully Decentralized** - No central authority, DHT-based discovery
✅ **Local-First Optimization** - Zero-latency for local services
✅ **Smart Caching** - DHT results cached (60s TTL) to reduce queries
✅ **Graceful Degradation** - Continues operation when DHT unavailable
✅ **Multiple Providers** - Automatic load balancing across providers
✅ **Fault Tolerant** - Provider failover if one becomes unavailable
✅ **NAT-Friendly** - HTTP/3 QUIC works through firewalls

### How It Works

The diagram above illustrates the complete RPC flow:

1. **Discovery Hierarchy** - 4-tier fallback: Local → Cache → DHT → Direct
2. **Service Advertisement** - Providers register to local registry and DHT
3. **RPC Call Flow** - Consumer discovers provider, sends MSG_CALL, receives MSG_REPLY
4. **Multi-Provider Load Balancing** - Round-robin selection with automatic failover

### Discovery Hierarchy

Macula uses a **4-tier fallback hierarchy** for optimal performance:

1. **Local Handler** ⚡ - Zero latency if service advertised locally
2. **Cache Hit** 🚀 - Fast retrieval from local cache (60s TTL)
3. **DHT Query** 🌐 - Query DHT for providers, cache result
4. **Direct Call** 🔗 - Fallback to connected endpoint

This design ensures:
- Local calls have **zero network overhead**
- Cached discoveries are **sub-millisecond**
- DHT queries happen only on **cache miss** (every 60 seconds per service)
- System **continues working** even if DHT is unavailable

---

## Architecture

### Components

#### `macula_service_registry`

Core registry module that manages:
- **Local services** - Handler functions for services this node provides
- **Discovery cache** - Cached provider lists from DHT queries (60s TTL)
- **DHT integration** - Publish/query/remove operations

**Key Functions**:
- `advertise_local/4` - Store handler locally
- `get_local_handler/2` - Retrieve local handler
- `discover_service/2,3` - Check cache for providers
- `cache_service/4` - Store DHT results in cache
- `publish_to_dht/5` - Publish service to DHT
- `query_dht_for_service/3` - Query DHT for providers
- `remove_from_dht/3` - Remove service from DHT

#### `macula_connection`

Connection gen_server that:
- Holds `service_registry` in state
- Handles `advertise`, `unadvertise`, `call` requests
- Executes handlers in spawned processes (non-blocking)
- Sends `MSG_CALL`, `MSG_REPLY`, `MSG_ERROR` messages

#### `macula` (Public API)

The **only module** applications should import. Delegates to internal modules:
- `connect/2`, `connect_local/1` - Connect to mesh
- `advertise/3,4` - Advertise a service
- `call/3,4` - Call a service
- `unadvertise/2` - Stop advertising
- `disconnect/1` - Close connection

#### `macula_peer` (Internal)

Internal mesh participant module (called by `macula`).

#### `macula_routing_dht`

DHT implementation (Kademlia):
- Pure functional DHT operations
- K-bucket routing table
- Store/find value operations
- K-value replication (typically 20)

### Data Flow

**Service Advertisement**:
```
Application
   ↓
macula:advertise(Client, Procedure, Handler, Opts)
   ↓
macula_connection (gen_server:call)
   ↓
macula_service_registry:advertise_local(Registry, Procedure, Handler, Metadata)
   ↓
macula_service_registry:publish_to_dht(DhtPid, Procedure, ProviderInfo, TTL, K)
   ↓
macula_routing_dht (DHT storage at key=SHA256(Procedure))
```

**Service Discovery and Call**:
```
Application
   ↓
macula:call(Client, Procedure, Args)
   ↓
macula_connection (gen_server:call)
   ↓
macula_service_registry:get_local_handler(Registry, Procedure)
   → Found? Execute locally (zero-latency path)
   ↓
macula_service_registry:discover_service(Registry, Procedure)
   → Cache hit? Use cached providers (fast path)
   ↓
macula_service_registry:query_dht_for_service(DhtPid, Procedure, K)
   → Query DHT, cache result
   ↓
Pick provider from list
   ↓
Send MSG_CALL over HTTP/3 QUIC to provider
   ↓
Provider executes handler → MSG_REPLY
   ↓
Application receives result
```

### Storage

**DHT Key**: `SHA256(Procedure)` (32-byte key)

**DHT Value**:
```erlang
#{
    node_id => binary(),         % 32-byte node identifier
    endpoint => binary(),        % Connection endpoint (e.g., <<"https://localhost:9443">>)
    metadata => map(),           % Custom metadata (version, description, etc.)
    advertised_at => integer(),  % Unix timestamp
    ttl => pos_integer()         % Seconds
}
```

**Local Cache Entry**:
```erlang
#{
    service_id => binary(),
    providers => [ProviderInfo],
    cached_at => integer(),      % Unix timestamp
    ttl => 60                    % Seconds (hard-coded)
}
```

---

## Service Advertisement

### Basic Advertisement

```erlang
%% Erlang
Handler = fun(Args) ->
    case Args of
        #{user_id := UserId} ->
            {ok, #{user_id => UserId, name => <<"Alice">>}};
        _ ->
            {error, invalid_args}
    end
end,

{ok, Ref} = macula:advertise(
    Peer,
    <<"myapp.user.get">>,
    Handler
).
```

```elixir
# Elixir
handler = fn
  %{user_id: user_id} ->
    {:ok, %{user_id: user_id, name: "Alice"}}

  _ ->
    {:error, :invalid_args}
end

{:ok, ref} = :macula.advertise(
  client,
  "myapp.user.get",
  handler
)
```

### With Options

```erlang
{ok, Ref} = macula:advertise(
    Peer,
    <<"myapp.user.get">>,
    Handler,
    #{
        metadata => #{
            version => <<"1.0.0">>,
            description => <<"Fetch user by ID">>,
            capabilities => [<<"read">>]
        },
        ttl => 300  % 5 minutes
    }
).
```

### Handler Function Contract

Handlers must follow this contract:

```erlang
-type handler_fn() :: fun((Args :: map()) -> {ok, Result :: term()} | {error, Reason :: term()}).
```

**Rules**:
- Input: Always a map (even if empty: `#{}`)
- Output: Tuple `{ok, Result}` or `{error, Reason}`
- Execution: Handlers run in **spawned processes** (non-blocking)
- Errors: Handler crashes are caught and returned as `{error, {handler_crash, Reason}}`

### TTL and Re-advertisement

**Default TTL**: 300 seconds (5 minutes)

Services must be **re-advertised** before TTL expiration to remain discoverable. Currently this is **manual** - future enhancement will add automatic periodic re-advertisement.

```erlang
%% Manual re-advertisement pattern
re_advertise_loop(Client, Procedure, Handler, Opts) ->
    TTL = maps:get(ttl, Opts, 300),
    {ok, _Ref} = macula:advertise(Client, Procedure, Handler, Opts),

    %% Re-advertise every 4 minutes (before 5-minute TTL expires)
    timer:sleep((TTL - 60) * 1000),
    re_advertise_loop(Client, Procedure, Handler, Opts).
```

### Unadvertising

```erlang
ok = macula:unadvertise(Client, <<"myapp.user.get">>).
```

**Behavior**:
1. Removes local handler from service registry
2. Attempts to remove from DHT (best-effort)
3. DHT entries expire naturally via TTL anyway

---

## Service Discovery

Service discovery happens **automatically** when calling a service via `macula:call/2,3`.

### Discovery Flow

```erlang
%% 1. Check local handler (zero-latency)
case macula_service_registry:get_local_handler(Registry, Procedure) of
    {ok, Handler} ->
        %% Execute locally - no network overhead
        Handler(Args);

    not_found ->
        %% 2. Check cache (fast path)
        case macula_service_registry:discover_service(Registry, Procedure) of
            {ok, Providers, Registry2} ->
                %% Cache hit - use cached providers
                pick_provider_and_call(Providers);

            {cache_miss, Registry2} ->
                %% 3. Query DHT (cache miss)
                case macula_service_registry:query_dht_for_service(DhtPid, Procedure, 20) of
                    {ok, Providers} when Providers =/= [] ->
                        %% Cache the result (60s TTL)
                        Registry3 = macula_service_registry:cache_service(
                            Registry2, Procedure, Providers, 60
                        ),
                        pick_provider_and_call(Providers);

                    {ok, []} ->
                        %% No providers found
                        {error, service_not_found};

                    {error, Reason} ->
                        %% DHT unavailable - fallback to direct call
                        direct_call_fallback()
                end
        end
end.
```

### Force Refresh

Skip cache and force DHT query:

```erlang
{ok, Result} = macula:call(
    Peer,
    <<"myapp.user.get">>,
    #{user_id => <<"user-123">>},
    #{force_refresh => true}
).
```

### Cache Management

**Cache TTL**: 60 seconds (hard-coded in `macula_service_registry`)

**Pruning**: Expired entries can be pruned manually:

```erlang
{Registry2, RemovedCount} = macula_service_registry:prune_expired(Registry).
```

**Clearing**: Clear all cache entries:

```erlang
Registry2 = macula_service_registry:clear_cache(Registry).
```

---

## Making RPC Calls

### Basic Call

```erlang
%% Erlang
{ok, User} = macula:call(
    Peer,
    <<"myapp.user.get">>,
    #{user_id => <<"user-123">>}
).
```

```elixir
# Elixir
{:ok, user} = :macula.call(
  client,
  "myapp.user.get",
  %{user_id: "user-123"}
)
```

### With Timeout

```erlang
{ok, Result} = macula:call(
    Peer,
    <<"slow.operation">>,
    #{data => SomeData},
    #{timeout => 30000}  % 30 seconds
).
```

### Local-First Pattern

If a service is advertised locally, the call has **zero network overhead**:

```erlang
%% Node A advertises service
{ok, _} = macula:advertise(Client, <<"calc.add">>, HandlerFn),

%% Node A calls the same service - executed locally (no network)
{ok, Result} = macula:call(Client, <<"calc.add">>, #{a => 10, b => 5}).
%% Result = #{result => 15}
```

This is extremely efficient for:
- Self-service calls
- Co-located services on the same node
- Testing and development

---

## Error Handling

### Error Types

```erlang
-type call_error() ::
    timeout |                          % Call timed out
    service_not_found |                % No providers found in DHT
    {handler_error, Reason :: term()} | % Handler returned {error, Reason}
    {handler_crash, Reason :: term()} | % Handler process crashed
    {connection_error, Reason :: term()} | % Network error
    term().                            % Other errors
```

### Comprehensive Error Handling

```erlang
case macula:call(Client, Procedure, Args, #{timeout => 10000}) of
    {ok, Result} ->
        %% Success
        process_result(Result);

    {error, timeout} ->
        %% Call timed out after 10 seconds
        ?LOG_WARNING("RPC call timed out: ~s", [Procedure]),
        {error, timeout};

    {error, service_not_found} ->
        %% No providers found in DHT
        ?LOG_ERROR("Service not available: ~s", [Procedure]),
        {error, unavailable};

    {error, {handler_error, Reason}} ->
        %% Handler explicitly returned {error, Reason}
        ?LOG_WARNING("Handler error for ~s: ~p", [Procedure, Reason]),
        {error, {business_logic_error, Reason}};

    {error, {handler_crash, Reason}} ->
        %% Handler process crashed
        ?LOG_ERROR("Handler crashed for ~s: ~p", [Procedure, Reason]),
        {error, internal_error};

    {error, {connection_error, Reason}} ->
        %% Network/transport error
        ?LOG_ERROR("Connection error calling ~s: ~p", [Procedure, Reason]),
        {error, network_error};

    {error, Reason} ->
        %% Catch-all for other errors
        ?LOG_ERROR("Unexpected error calling ~s: ~p", [Procedure, Reason]),
        {error, unknown_error}
end.
```

### Retry Pattern

```erlang
call_with_retry(Client, Procedure, Args, MaxRetries) ->
    call_with_retry(Client, Procedure, Args, MaxRetries, 0).

call_with_retry(_Client, _Procedure, _Args, MaxRetries, Attempt)
        when Attempt >= MaxRetries ->
    {error, max_retries_exceeded};

call_with_retry(Client, Procedure, Args, MaxRetries, Attempt) ->
    case macula:call(Client, Procedure, Args, #{timeout => 5000}) of
        {ok, Result} ->
            {ok, Result};

        {error, timeout} ->
            %% Retry on timeout
            timer:sleep(1000 * (Attempt + 1)),  % Exponential backoff
            call_with_retry(Client, Procedure, Args, MaxRetries, Attempt + 1);

        {error, {connection_error, _}} ->
            %% Retry on connection errors
            timer:sleep(1000 * (Attempt + 1)),
            call_with_retry(Client, Procedure, Args, MaxRetries, Attempt + 1);

        {error, Reason} ->
            %% Don't retry on business logic errors
            {error, Reason}
    end.
```

---

## Performance Optimization

### Local-First Optimization

**Zero-latency local calls**:
- Local handlers checked first
- No network overhead if service advertised locally
- Ideal for co-located services

### Caching Strategy

**60-second cache TTL**:
- DHT queries cached for 60 seconds
- Reduces DHT load
- Balances freshness vs performance

**Cache hit ratio**:
```
Cache Hit Ratio = (Cache Hits) / (Total Calls)

Example:
- Service called 100 times/minute
- Cache TTL = 60 seconds
- DHT queries = ~2/minute (every 60s)
- Cache hit ratio = 98%
```

### DHT Query Optimization

**Kademlia K-value**: 20 (standard)
- Stores service advertisement on 20 nodes
- High availability even if nodes fail
- Fast lookups (log N hops)

**SHA-256 Key Distribution**:
- Deterministic key generation
- Even distribution across DHT keyspace
- Prevents hotspots

### Provider Selection

**Current**: Simple first-provider selection

**Future enhancements**:
- Round-robin load balancing
- Random selection
- Least-loaded provider
- Geographic proximity
- Custom selection strategies

### Graceful Degradation

**DHT unavailable**:
- Logs warning but continues
- Falls back to direct call to connected endpoint
- Local services still work

**Network partitions**:
- Each partition has local DHT
- Services discoverable within partition
- Automatic recovery when partition heals

---

## Best Practices

### Service Naming

Use **hierarchical naming** with dot-separated segments:

```erlang
%% Good
<<"myapp.user.get">>
<<"energy.home.measure">>
<<"payment.invoice.create">>

%% Avoid
<<"get_user">>          % Not namespaced
<<"user">>              % Too generic
<<"user-get">>          % Use dots, not dashes
```

### Handler Design

**Keep handlers simple**:
```erlang
%% Good - simple, focused
Handler = fun(#{user_id := UserId}) ->
    case user_db:get(UserId) of
        {ok, User} -> {ok, User};
        not_found -> {error, not_found}
    end
end.

%% Avoid - complex logic in handler
Handler = fun(Args) ->
    %% Don't do heavy processing in handler
    %% Spawn workers if needed
    case Args of
        #{action := <<"create">>, data := Data} ->
            %% Heavy processing...
        #{action := <<"update">>, data := Data} ->
            %% More heavy processing...
        #{action := <<"delete">>, id := Id} ->
            %% Even more processing...
    end
end.
```

**Pattern matching on function heads**:
```erlang
%% Good - separate functions for different actions
handle_get(#{user_id := UserId}) ->
    user_db:get(UserId).

handle_create(#{name := Name, email := Email}) ->
    user_db:create(Name, Email).

Handler = fun(Args) ->
    case Args of
        #{action := <<"get">>} -> handle_get(Args);
        #{action := <<"create">>} -> handle_create(Args);
        _ -> {error, invalid_action}
    end
end.
```

### Metadata Usage

**Include useful metadata**:
```erlang
#{
    metadata => #{
        version => <<"1.2.3">>,              % Semantic version
        description => <<"User management">>, % Human-readable description
        capabilities => [<<"read">>, <<"write">>], % What it can do
        schema => #{                         % Input/output schema
            input => [user_id],
            output => [user_id, name, email]
        }
    }
}
```

### TTL Configuration

**Choose TTL based on service characteristics**:

```erlang
%% Long-lived services (rarely change)
#{ttl => 3600}  % 1 hour

%% Normal services
#{ttl => 300}   % 5 minutes (default)

%% Dynamic services (frequently changing)
#{ttl => 60}    % 1 minute
```

### Error Handling in Handlers

**Always return proper error tuples**:
```erlang
Handler = fun(Args) ->
    try
        Result = do_work(Args),
        {ok, Result}
    catch
        error:{badmatch, _} ->
            {error, invalid_args};
        error:database_error ->
            {error, service_unavailable};
        Class:Reason ->
            ?LOG_ERROR("Handler crashed: ~p:~p", [Class, Reason]),
            {error, internal_error}
    end
end.
```

### Monitoring

**Log important events**:
```erlang
%% On advertisement
?LOG_INFO("Advertised service ~s with metadata ~p", [Procedure, Metadata]),

%% On DHT publish success/failure
?LOG_INFO("Published service ~s to DHT", [Procedure]),
?LOG_WARNING("Failed to publish service ~s to DHT: ~p", [Procedure, Reason]),

%% On service calls
?LOG_DEBUG("Calling service ~s with args ~p", [Procedure, Args]),
?LOG_INFO("Service ~s completed in ~p ms", [Procedure, Duration]).
```

---

## Examples

### Example 1: Calculator Service

```erlang
%% calculator_service.erl
-module(calculator_service).
-export([start/1, advertise/1]).

start(Client) ->
    Handler = fun(Args) ->
        case Args of
            #{operation := <<"add">>, a := A, b := B} ->
                {ok, #{result => A + B}};
            #{operation := <<"subtract">>, a := A, b := B} ->
                {ok, #{result => A - B}};
            #{operation := <<"multiply">>, a := A, b := B} ->
                {ok, #{result => A * B}};
            #{operation := <<"divide">>, a := A, b := 0} ->
                {error, division_by_zero};
            #{operation := <<"divide">>, a := A, b := B} ->
                {ok, #{result => A / B}};
            _ ->
                {error, invalid_operation}
        end
    end,

    macula:advertise(
        Peer,
        <<"calculator.compute">>,
        Handler,
        #{metadata => #{version => <<"1.0.0">>}}
    ).

%% Client code
{ok, Client} = macula:connect(<<"https://localhost:9443">>, #{}),
{ok, _Ref} = calculator_service:advertise(Client),

%% Make calls
{ok, #{result := 15}} = macula:call(
    Peer, <<"calculator.compute">>,
    #{operation => <<"add">>, a => 10, b => 5}
).
```

### Example 2: User Service with Database

```erlang
%% user_service.erl
-module(user_service).
-export([start/1]).

start(Client) ->
    Handler = fun(Args) ->
        handle_request(Args)
    end,

    macula:advertise(
        Peer,
        <<"users.manage">>,
        Handler,
        #{
            metadata => #{
                version => <<"2.0.0">>,
                capabilities => [<<"read">>, <<"write">>, <<"delete">>]
            },
            ttl => 300
        }
    ).

handle_request(#{action := <<"get">>, user_id := UserId}) ->
    case user_db:fetch(UserId) of
        {ok, User} -> {ok, User};
        not_found -> {error, user_not_found}
    end;

handle_request(#{action := <<"create">>, name := Name, email := Email}) ->
    case user_db:create(#{name => Name, email => Email}) of
        {ok, UserId} ->
            {ok, #{user_id => UserId, name => Name, email => Email}};
        {error, Reason} ->
            {error, Reason}
    end;

handle_request(#{action := <<"delete">>, user_id := UserId}) ->
    case user_db:delete(UserId) of
        ok -> {ok, #{status => <<"deleted">>}};
        {error, Reason} -> {error, Reason}
    end;

handle_request(_) ->
    {error, invalid_action}.
```

### Example 3: Multi-Provider Discovery

```erlang
%% Provider Node 1
{ok, Client1} = macula:connect(<<"https://node1:9443">>, #{}),
{ok, _} = macula:advertise(Client1, <<"weather.get">>, Handler1),

%% Provider Node 2
{ok, Client2} = macula:connect(<<"https://node2:9443">>, #{}),
{ok, _} = macula:advertise(Client2, <<"weather.get">>, Handler2),

%% Consumer Node
{ok, Client3} = macula:connect(<<"https://node3:9443">>, #{}),

%% Call service - DHT returns both providers
%% One is selected automatically
{ok, Weather} = macula:call(
    Client3,
    <<"weather.get">>,
    #{city => <<"Brussels">>}
).
```

### Example 4: Elixir Phoenix Application

```elixir
# lib/my_app/macula_rpc.ex
defmodule MyApp.MaculaRPC do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    {:ok, client} = :macula.connect("https://localhost:9443", %{
      realm: "com.myapp"
    })

    # Advertise multiple services
    advertise_services(client)

    {:ok, %{client: client}}
  end

  defp advertise_services(client) do
    # User service
    user_handler = fn
      %{action: "get", user_id: user_id} ->
        case MyApp.Users.get(user_id) do
          {:ok, user} -> {:ok, user}
          _ -> {:error, :not_found}
        end

      %{action: "create", params: params} ->
        case MyApp.Users.create(params) do
          {:ok, user} -> {:ok, user}
          {:error, changeset} -> {:error, {:validation, changeset}}
        end
    end

    :macula.advertise(client, "myapp.users", user_handler, %{
      metadata: %{version: "1.0.0", description: "User management"}
    })

    # Post service
    post_handler = fn args -> MyApp.Posts.handle_rpc(args) end

    :macula.advertise(client, "myapp.posts", post_handler, %{
      metadata: %{version: "1.0.0"}
    })
  end

  # Client wrapper
  def call(procedure, args, opts \\ %{}) do
    client = GenServer.call(__MODULE__, :get_client)
    :macula.call(client, procedure, args, opts)
  end

  def handle_call(:get_client, _from, %{client: client} = state) do
    {:reply, client, state}
  end
end

# Usage in Phoenix controller
defmodule MyAppWeb.UserController do
  use MyAppWeb, :controller

  def show(conn, %{"id" => id}) do
    case MyApp.MaculaRPC.call("myapp.users", %{action: "get", user_id: id}) do
      {:ok, user} ->
        json(conn, user)

      {:error, :not_found} ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "User not found"})

      {:error, reason} ->
        conn
        |> put_status(:internal_server_error)
        |> json(%{error: inspect(reason)})
    end
  end
end
```

---

## Migration from WAMP

### Key Differences

| Aspect | WAMP (Bondy) | Macula HTTP/3 |
|--------|--------------|---------------|
| **Discovery** | Centralized registry | DHT-based (decentralized) |
| **Transport** | WebSocket | HTTP/3 QUIC |
| **Registration** | `session.register(Procedure, Handler)` | `macula:advertise(Client, Procedure, Handler)` |
| **RPC Call** | `session.call(Procedure, Args)` | `macula:call(Client, Procedure, Args)` |
| **Unregister** | `session.unregister(Registration)` | `macula:unadvertise(Client, Procedure)` |
| **Handler Args** | `[Args, Kwargs]` (positional + keyword) | `Args :: map()` (map only) |
| **NAT Traversal** | Requires special config | Built-in (QUIC) |

### Migration Steps

#### 1. Update Dependencies

```erlang
%% Before (WAMP)
{deps, [
    {bondy, {git, "https://github.com/bondy-io/bondy.git", {tag, "1.0.0"}}}
]}.

%% After (Macula)
{deps, [
    {macula, "0.6.6"}
]}.
```

#### 2. Convert Registration

```erlang
%% Before (WAMP)
Handler = fun([Args, Kwargs]) ->
    UserId = proplists:get_value(<<"user_id">>, Kwargs),
    {ok, #{user_id => UserId, name => <<"Alice">>}}
end,
{ok, Registration} = bondy:register(Session, <<"myapp.user.get">>, Handler).

%% After (Macula)
Handler = fun(Args) ->
    #{user_id := UserId} = Args,
    {ok, #{user_id => UserId, name => <<"Alice">>}}
end,
{ok, Ref} = macula:advertise(Client, <<"myapp.user.get">>, Handler).
```

#### 3. Convert RPC Calls

```erlang
%% Before (WAMP)
{ok, Result} = bondy:call(Session, <<"myapp.user.get">>, [#{user_id => <<"123">>}]).

%% After (Macula)
{ok, Result} = macula:call(Client, <<"myapp.user.get">>, #{user_id => <<"123">>}).
```

#### 4. Update Handler Signatures

```erlang
%% Before (WAMP) - separate positional and keyword args
Handler = fun([PositionalArgs, KeywordArgs]) ->
    UserId = proplists:get_value(<<"user_id">>, KeywordArgs),
    %% ...
end.

%% After (Macula) - single map argument
Handler = fun(Args) ->
    #{user_id := UserId} = Args,
    %% ...
end.
```

#### 5. Update Error Handling

```erlang
%% Before (WAMP)
case bondy:call(Session, Procedure, Args) of
    {ok, Result} -> handle_result(Result);
    {error, {wamp_error, Uri, Details, _Args}} -> handle_wamp_error(Uri)
end.

%% After (Macula)
case macula:call(Client, Procedure, Args) of
    {ok, Result} -> handle_result(Result);
    {error, timeout} -> handle_timeout();
    {error, service_not_found} -> handle_not_found();
    {error, {handler_error, Reason}} -> handle_business_error(Reason)
end.
```

### Migration Checklist

- [ ] Update dependencies (WAMP → Macula)
- [ ] Convert handler signatures (`[Args, Kwargs]` → `Args :: map()`)
- [ ] Replace `bondy:register/3` with `macula:advertise/3,4`
- [ ] Replace `bondy:call/3` with `macula:call/2,3`
- [ ] Replace `bondy:unregister/2` with `macula:unadvertise/2`
- [ ] Update error handling patterns
- [ ] Add periodic re-advertisement logic (if needed)
- [ ] Test with DHT unavailable (graceful degradation)
- [ ] Update monitoring and logging

---

## See Also

- [Quick Start Guide](../user/QUICK_START.md) - Getting started tutorial
- [PubSub Guide](PUBSUB_GUIDE.md) - Pub/Sub patterns and usage
- [Glossary](../GLOSSARY.md) - Terminology reference

---

**Last Updated**: 2025-11-30
**Status**: ✅ Complete
