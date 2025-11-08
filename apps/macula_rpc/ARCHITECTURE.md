# macula_rpc Architecture

## Overview

Implementation of request-response RPC (Remote Procedure Call) for the Macula mesh, integrated with Kademlia DHT for service discovery.

## RPC Design Principles

**Goal**: Enable distributed procedure calls with service discovery and load balancing

**Key Properties**:
- **Service Registry**: Register procedures at URIs
- **DHT Discovery**: Find service providers via DHT
- **Load Balancing**: Round-robin across multiple providers
- **Timeout Handling**: Configurable call timeouts
- **Error Propagation**: Structured error responses
- **At-Most-Once Semantics**: Request deduplication

## Architecture Components

### 1. Procedure URI (`macula_rpc_uri`)

URI-based procedure naming and validation.

```erlang
-type uri() :: binary().  % e.g., <<"be.cortexiq.home.get_measurement">>

%% URI validation
validate(Uri :: binary()) -> ok | {error, invalid_uri}.

%% URI matching (for router)
matches(Uri :: binary(), Pattern :: binary()) -> boolean().

%% Normalize URI
normalize(Uri :: binary()) -> binary().

%% Extract namespace
namespace(Uri :: binary()) -> binary().
```

**URI Conventions**:
- Reverse DNS notation: `org.domain.service.procedure`
- Examples:
  - `be.cortexiq.home.get_measurement`
  - `be.cortexiq.provider.calculate_price`
  - `be.cortexiq.market.get_spot_price`

**API**:
- `validate/1` - Validate URI syntax
- `matches/2` - Check URI pattern matching
- `normalize/1` - Normalize URI string
- `namespace/1` - Extract namespace prefix

### 2. Registration Registry (`macula_rpc_registry`)

Local registry mapping URIs to handler functions.

```erlang
-record(registration, {
    uri :: binary(),
    handler :: fun((Args :: map()) -> {ok, Result :: term()} | {error, Reason :: term()}),
    metadata :: map()  % Optional: rate limits, auth requirements, etc.
}).

-type registry() :: #{
    registrations := #{binary() => [registration()]},  % URI -> handlers
    invocation_strategy := round_robin | random | local_first
}.
```

**Registry Operations**:
- Register procedure (URI + handler function)
- Unregister procedure
- Find local handler for URI
- List registered procedures

**API**:
- `new/1` - Create empty registry with strategy
- `register/4` - Add registration (uri, handler_fn, metadata)
- `unregister/2` - Remove registration (uri, handler_fn)
- `find_local/2` - Find local handler for URI
- `list/1` - Get all registrations

### 3. Service Discovery (`macula_rpc_discovery`)

DHT integration for finding RPC service providers.

```erlang
%% Store registration in DHT
%% Key: hash(uri) -> 256-bit node ID
%% Value: {provider_node_id, provider_address, metadata, ttl}

-spec announce_registration(Uri :: binary(), NodeInfo :: node_info(), Metadata :: map()) -> ok.
%% Stores registration in DHT at k closest nodes to hash(uri)

-spec find_providers(Uri :: binary()) -> [provider_info()].
%% Queries DHT for nodes providing this procedure

-spec remove_registration(Uri :: binary()) -> ok.
%% Removes registration from DHT

-type provider_info() :: #{
    node_id := binary(),
    address := {inet:ip_address(), inet:port_number()},
    metadata := map(),
    last_seen := integer()
}.
```

**DHT Storage Strategy**:
- **Key**: `hash(uri)` (SHA-256) → 256-bit key
- **Value**: `{node_id, {ip, port}, metadata, ttl}`
- **TTL**: 3600 seconds (re-announce every hour)
- **Replication**: Stored at k closest nodes (default k=20)

**Discovery Flow**:
```
Provider Node                  DHT                    Caller Node
     |                           |                           |
     |--announce("proc.foo")---->|                           |
     |   store at k closest      |                           |
     |<--ack---------------------|                           |
     |                           |                           |
     |                           |<--find_providers----------|
     |                           |   ("proc.foo")            |
     |                           |                           |
     |                           |--[Provider's info]------->|
     |                           |                           |
     |<--call("proc.foo", args)---------------------------- |
     |                           |                           |
     |--result--------------------------------------------------->|
     |                           |                           |
```

**API**:
- `announce_registration/3` - Store registration in DHT
- `find_providers/1` - Query DHT for providers
- `remove_registration/1` - Remove from DHT

### 4. Call Router (`macula_rpc_router`)

Routes calls to appropriate provider (local or remote).

```erlang
-record(call_context, {
    call_id :: binary(),        % Unique call identifier
    uri :: binary(),
    args :: map(),
    caller_id :: binary(),
    timeout :: pos_integer(),
    timestamp :: integer()
}).

%% Routing strategies
-type routing_strategy() :: local_first | round_robin | random | closest.

-spec route_call(Uri :: binary(), Args :: map(), Strategy :: routing_strategy()) ->
    {local, HandlerFn :: function()} |
    {remote, Provider :: provider_info()} |
    {error, no_provider}.
```

**Routing Strategies**:
- **local_first**: Prefer local handler if available
- **round_robin**: Rotate through available providers
- **random**: Random provider selection
- **closest**: Use provider closest to caller (by XOR distance)

**API**:
- `route_call/3` - Select provider for call
- `get_next_provider/2` - Round-robin selection
- `get_random_provider/1` - Random selection
- `get_closest_provider/2` - Closest by XOR distance

### 5. Call Execution (`macula_rpc_executor`)

Execute local and remote calls with timeout handling.

```erlang
-record(call_state, {
    call_id :: binary(),
    caller_pid :: pid(),
    start_time :: integer(),
    timeout :: pos_integer(),
    timer_ref :: reference()
}).

%% Execute local call
-spec execute_local(Uri :: binary(), Args :: map(), Handler :: function(), Timeout :: pos_integer()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.

%% Execute remote call
-spec execute_remote(Uri :: binary(), Args :: map(), Provider :: provider_info(), Timeout :: pos_integer()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.

%% Async call with callback
-spec call_async(Uri :: binary(), Args :: map(), Callback :: pid()) -> {ok, CallId :: binary()}.
```

**Call Execution Flow**:
```
Caller                    Executor                Provider
  |                          |                        |
  |--call(uri, args)-------->|                        |
  |                          |                        |
  |                    [Route to provider]            |
  |                          |                        |
  |                    [Start timeout timer]          |
  |                          |                        |
  |                          |====invoke (QUIC)=====>|
  |                          |                        |
  |                          |                  [Execute handler]
  |                          |                        |
  |                          |<===result==============|
  |                          |                        |
  |                    [Cancel timer]                 |
  |                          |                        |
  |<--result-----------------|                        |
  |                          |                        |
```

**Timeout Handling**:
- Start timer on call
- Cancel on result
- Send `{error, timeout}` to caller on expiry
- Clean up pending call state

**API**:
- `execute_local/4` - Execute local handler
- `execute_remote/4` - Execute remote call via QUIC
- `call_async/3` - Async call with callback PID

### 6. Result Cache (`macula_rpc_cache`)

Optional caching for idempotent procedure results.

```erlang
-record(cache_entry, {
    uri :: binary(),
    args_hash :: binary(),      % Hash of arguments
    result :: term(),
    timestamp :: integer(),
    ttl :: pos_integer()
}).

-type cache() :: #{
    entries := #{binary() => cache_entry()},
    max_size := pos_integer()
}.
```

**Cache Strategy**:
- **Key**: `hash(uri, args)` (only for idempotent procedures)
- **TTL**: Configurable per registration (default: disabled)
- **Max Size**: 1000 entries (LRU eviction)
- **Invalidation**: TTL-based or manual

**API**:
- `new/1` - Create cache with max size
- `get/3` - Get cached result (uri, args, ttl)
- `put/4` - Cache result (uri, args, result, ttl)
- `invalidate/2` - Remove cached entry
- `clear/1` - Clear all cached results

### 7. RPC Server (`macula_rpc_server`)

GenServer managing registrations and calls.

```erlang
-record(state, {
    local_node_id :: binary(),
    registry :: registry(),
    cache :: cache(),
    dht_server :: pid(),        % macula_routing_server
    pending_calls :: #{binary() => call_state()},
    config :: #{
        default_timeout => 5000,      % Default call timeout (ms)
        cache_enabled => false,       % Enable result caching
        announce_interval => 3600,    % Re-announce interval
        routing_strategy => local_first
    }
}).
```

**GenServer API**:
- `start_link/2` - Start RPC server
- `register/4` - Register procedure (uri, handler_fn, metadata)
- `unregister/2` - Unregister procedure
- `call/3` - Synchronous call (uri, args, timeout)
- `call_async/3` - Asynchronous call (uri, args, callback_pid)
- `list_registrations/1` - List local registrations

**Message Handling**:
- Handle incoming calls (via QUIC)
- Handle call results (from remote providers)
- Handle timeout timers
- Re-announce registrations periodically

## Data Flow

### Synchronous Call Flow

```
Caller Node                    RPC Server                Provider Node
     |                             |                            |
     |--call(uri, args, timeout)-->|                            |
     |                             |                            |
     |                       [1. Check local registry]          |
     |                        (not found locally)               |
     |                             |                            |
     |                       [2. Check cache]                   |
     |                        (cache miss)                      |
     |                             |                            |
     |                       [3. Query DHT for providers]       |
     |                       find_providers(uri)                |
     |                             |                            |
     |                       [4. Route to provider]             |
     |                        (round-robin)                     |
     |                             |                            |
     |                       [5. Start timeout timer]           |
     |                             |                            |
     |                             |====call (QUIC)=========>   |
     |                             |                            |
     |                             |                      [Execute handler]
     |                             |                            |
     |                             |<===result==================|
     |                             |                            |
     |                       [6. Cancel timer]                  |
     |                       [7. Cache result (if enabled)]     |
     |                             |                            |
     |<--result--------------------|                            |
     |                             |                            |
```

### Registration Flow

```
Provider Node                  RPC Server                DHT Nodes
     |                             |                            |
     |--register(uri, handler)---->|                            |
     |                             |                            |
     |                       [1. Add to registry]               |
     |                             |                            |
     |                       [2. Announce to DHT]               |
     |                             |--store(uri, node)--------->|
     |                             |                            |
     |                             |<--ack----------------------|
     |                             |                            |
     |<--registration_ok-----------|                            |
     |                             |                            |
     |                       [Background: re-announce]          |
     |                       (every announce_interval)          |
```

## Configuration

```erlang
#{
    default_timeout => 5000,        % Default call timeout (ms)
    cache_enabled => false,         % Enable result caching
    cache_ttl => 60,                % Cache TTL (seconds)
    max_cache_size => 1000,         % Max cached results
    announce_interval => 3600,      % DHT re-announce interval
    routing_strategy => local_first,% local_first | round_robin | random
    max_pending_calls => 10000      % Concurrent call limit
}
```

## Error Handling

**Error Types**:
```erlang
-type rpc_error() ::
    {error, no_provider} |          % No provider found for URI
    {error, timeout} |              % Call timeout
    {error, invalid_args} |         % Invalid arguments
    {error, procedure_error, Reason :: term()} |  % Handler threw error
    {error, network_error, Reason :: term()}.      % QUIC error
```

**Error Propagation**:
- Local errors: Direct return from handler
- Remote errors: Serialized and sent back via QUIC
- Timeout errors: Generated by executor
- Network errors: QUIC layer errors

## Testing Strategy

### Unit Tests (EUnit)
- URI validation and pattern matching
- Registry add/remove/find operations
- Cache LRU eviction
- Call routing strategies

### Integration Tests (Common Test)
- 3-node RPC network
- Local and remote calls
- DHT-based discovery
- Timeout handling
- Error propagation

### Property Tests (PropEr)
- All calls return result or error
- No call duplication
- Timeout correctness
- Cache consistency

## Performance Targets

- **Local Call**: < 1ms (direct function call)
- **Remote Call**: < 50ms (DHT lookup + QUIC roundtrip)
- **Cached Remote**: < 10ms (skip DHT lookup)
- **Throughput**: 5,000 calls/sec per node
- **Concurrent Calls**: 10,000 pending calls

## Implementation Order (TDD)

1. **macula_rpc_uri** - URI validation and matching
2. **macula_rpc_registry** - Local registration registry
3. **macula_rpc_cache** - Result cache with LRU
4. **macula_rpc_discovery** - DHT integration
5. **macula_rpc_router** - Call routing strategies
6. **macula_rpc_executor** - Call execution and timeout
7. **macula_rpc_server** - GenServer integration
8. **Integration tests** - Multi-node RPC scenarios

Each module implemented with:
1. Write tests first (Red)
2. Implement minimal code (Green)
3. Refactor and add property tests
4. Verify 95%+ coverage

## WAMP Integration Notes

This RPC implementation is inspired by WAMP's RPC model but simplified:
- WAMP uses `wamp.registration.*` for registration management
- WAMP uses `wamp.error.*` for structured errors
- WAMP supports progressive call results (we don't)
- WAMP supports call cancellation (future feature)

Macula RPC is a lightweight, DHT-based alternative focused on:
- Decentralization (no central router required)
- Simple request-response semantics
- Integration with Kademlia DHT for discovery
