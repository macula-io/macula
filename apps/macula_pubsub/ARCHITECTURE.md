# macula_pubsub Architecture

## Overview

Implementation of topic-based publish-subscribe messaging for the Macula mesh, integrated with Kademlia DHT for distributed topic management.

## Pub/Sub Design Principles

**Goal**: Enable scalable, decentralized topic-based messaging across the mesh

**Key Properties**:
- **Topic-Based Routing**: Messages routed by topic patterns
- **Pattern Matching**: Wildcard subscriptions (e.g., `be.cortexiq.*.measured`)
- **DHT Integration**: Topic registrations stored in DHT
- **Local Caching**: Subscriber lists cached locally
- **Fan-Out**: Efficient message delivery to multiple subscribers
- **At-Most-Once Delivery**: Best-effort delivery semantics

## Architecture Components

### 1. Topic Namespace (`macula_pubsub_topic`)

Topic string utilities and pattern matching.

```erlang
-type topic() :: binary().  % e.g., <<"be.cortexiq.home.measured">>
-type pattern() :: binary(). % e.g., <<"be.cortexiq.*.measured">>

%% Topic validation
validate(Topic :: binary()) -> ok | {error, invalid_topic}.

%% Pattern matching
matches(Topic :: binary(), Pattern :: binary()) -> boolean().

%% Examples:
%% matches(<<"be.cortexiq.home.measured">>, <<"be.cortexiq.home.measured">>) -> true
%% matches(<<"be.cortexiq.home.measured">>, <<"be.cortexiq.*.measured">>) -> true
%% matches(<<"be.cortexiq.home.measured">>, <<"be.cortexiq.#">>) -> true
%% matches(<<"be.cortexiq.home.measured">>, <<"be.cortexiq.provider.*">>) -> false

%% Topic normalization
normalize(Topic :: binary()) -> binary().

%% Extract namespace
namespace(Topic :: binary()) -> binary().
```

**Pattern Syntax**:
- `.` - Separator
- `*` - Matches exactly one segment (e.g., `be.cortexiq.*.measured`)
- `#` - Matches zero or more segments (e.g., `be.cortexiq.#`)

**API**:
- `validate/1` - Validate topic syntax
- `matches/2` - Check if topic matches pattern
- `normalize/1` - Normalize topic string
- `namespace/1` - Extract namespace prefix

### 2. Subscription Registry (`macula_pubsub_registry`)

Local registry mapping patterns to subscribers.

```erlang
-record(subscription, {
    subscriber_id :: binary(),  % Local subscriber ID
    pattern :: binary(),        % Topic pattern
    callback :: pid()           % Process to notify
}).

-type registry() :: #{
    subscriptions := [subscription()],
    pattern_index := #{binary() => [subscription()]}  % Fast lookup
}.
```

**Registry Operations**:
- Add subscription (local)
- Remove subscription (local)
- Find matching subscriptions for topic
- List all patterns

**API**:
- `new/0` - Create empty registry
- `subscribe/3` - Add subscription (subscriber_id, pattern, callback_pid)
- `unsubscribe/2` - Remove subscription (subscriber_id, pattern)
- `match/2` - Find subscriptions matching topic
- `list_patterns/1` - Get all subscribed patterns

### 3. Topic Discovery (`macula_pubsub_discovery`)

DHT integration for finding publishers and subscribers.

```erlang
%% Store subscription in DHT
%% Key: hash(pattern) -> 256-bit node ID
%% Value: {subscriber_node_id, subscriber_address, ttl}

-spec announce_subscription(Pattern :: binary(), NodeInfo :: node_info()) -> ok.
%% Stores subscription in DHT at k closest nodes to hash(pattern)

-spec find_subscribers(Pattern :: binary()) -> [node_info()].
%% Queries DHT for nodes subscribed to pattern

-spec remove_subscription(Pattern :: binary()) -> ok.
%% Removes subscription from DHT
```

**DHT Storage Strategy**:
- **Key**: `hash(pattern)` (SHA-256) → 256-bit key
- **Value**: `{node_id, {ip, port}, ttl}`
- **TTL**: 3600 seconds (re-announce every hour)
- **Replication**: Stored at k closest nodes (default k=20)

**Discovery Flow**:
```
Subscriber A                    DHT                    Publisher B
     |                           |                           |
     |--announce("topic.*")----->|                           |
     |   store at k closest      |                           |
     |<--ack---------------------|                           |
     |                           |                           |
     |                           |<--find_subscribers--------|
     |                           |   ("topic.*")             |
     |                           |                           |
     |                           |--[A's node_info]--------->|
     |                           |                           |
     |<--publish(topic.foo, msg)---------------------------- |
     |                           |                           |
```

**API**:
- `announce_subscription/2` - Store subscription in DHT
- `find_subscribers/1` - Query DHT for subscribers
- `remove_subscription/1` - Remove from DHT

### 4. Message Delivery (`macula_pubsub_delivery`)

Efficient message routing and delivery.

```erlang
-record(publication, {
    topic :: binary(),
    payload :: term(),
    publisher_id :: binary(),
    timestamp :: integer()
}).

%% Local delivery
-spec deliver_local(Topic :: binary(), Payload :: term(), Registry :: registry()) -> ok.

%% Remote delivery
-spec deliver_remote(Topic :: binary(), Payload :: term(), Subscribers :: [node_info()]) -> ok.

%% Fan-out delivery
-spec fanout(Publication :: publication(), Subscribers :: [node_info()]) -> ok.
```

**Delivery Strategy**:
1. **Find Local Subscribers**: Check local registry for matching patterns
2. **Deliver Locally**: Send to local callback PIDs
3. **Find Remote Subscribers**: Query DHT or use cached subscriber list
4. **Deliver Remotely**: Fan-out via QUIC streams
5. **Cache Results**: Update local subscriber cache

**Message Flow**:
```
Publisher                  PubSub Server              Subscribers
    |                           |                           |
    |--publish(topic, msg)----->|                           |
    |                           |                           |
    |                      [Match local]                    |
    |                           |--msg------------------->  | (Local A)
    |                           |                           |
    |                      [Query DHT]                      |
    |                           |                           |
    |                      [Fan-out remote]                 |
    |                           |====msg (QUIC)=========>   | (Remote B)
    |                           |====msg (QUIC)=========>   | (Remote C)
    |                           |                           |
    |<--ack---------------------|                           |
```

**API**:
- `deliver_local/3` - Deliver to local subscribers
- `deliver_remote/3` - Deliver to remote subscribers
- `fanout/2` - Parallel fan-out delivery

### 5. Subscription Cache (`macula_pubsub_cache`)

Cache remote subscriber information for fast delivery.

```erlang
-record(cache_entry, {
    pattern :: binary(),
    subscribers :: [node_info()],
    last_updated :: integer(),
    ttl :: pos_integer()
}).

-type cache() :: #{
    entries := #{binary() => cache_entry()},
    max_size := pos_integer()
}.
```

**Cache Strategy**:
- **TTL**: 300 seconds (5 minutes)
- **Max Size**: 1000 entries (LRU eviction)
- **Invalidation**: On DHT updates or timeout
- **Refresh**: Background refresh every TTL/2

**API**:
- `new/1` - Create cache with max size
- `get/2` - Get cached subscribers (or fetch from DHT)
- `put/3` - Cache subscriber list
- `invalidate/2` - Remove cached entry
- `refresh/2` - Refresh stale entries

### 6. PubSub Server (`macula_pubsub_server`)

GenServer managing subscriptions and publications.

```erlang
-record(state, {
    local_node_id :: binary(),
    registry :: registry(),
    cache :: cache(),
    dht_server :: pid(),        % macula_routing_server
    config :: #{
        cache_ttl => 300,        % Cache TTL (seconds)
        announce_interval => 3600,% Re-announce subscriptions
        max_cache_size => 1000
    }
}).
```

**GenServer API**:
- `start_link/2` - Start pub/sub server
- `publish/3` - Publish message to topic
- `subscribe/3` - Subscribe to pattern (local callback)
- `unsubscribe/2` - Unsubscribe from pattern
- `list_subscriptions/1` - List local subscriptions

**Message Handling**:
- Handle incoming publications (via QUIC)
- Handle subscription announcements
- Handle cache refresh timers
- Update routing table on DHT changes

## Data Flow

### Publication Flow

```
Publisher Node                  PubSub Server              Subscriber Nodes
     |                               |                           |
     |--publish(topic, msg)--------->|                           |
     |                               |                           |
     |                         [1. Match local]                  |
     |                               |--deliver-------------->   | (Local)
     |                               |                           |
     |                         [2. Check cache]                  |
     |                          (cache miss)                     |
     |                               |                           |
     |                         [3. Query DHT]                    |
     |                       find_subscribers(topic)             |
     |                               |                           |
     |                         [4. Update cache]                 |
     |                               |                           |
     |                         [5. Fan-out remote]               |
     |                               |====msg (QUIC)========>    | (Remote A)
     |                               |====msg (QUIC)========>    | (Remote B)
     |                               |                           |
     |<--ack-------------------------|                           |
     |                               |                           |
```

### Subscription Flow

```
Subscriber Node                PubSub Server              DHT Nodes
     |                               |                           |
     |--subscribe(pattern, pid)----->|                           |
     |                               |                           |
     |                         [1. Add to registry]              |
     |                               |                           |
     |                         [2. Announce to DHT]              |
     |                               |--store(pattern, node)---->|
     |                               |                           |
     |                               |<--ack---------------------|
     |                               |                           |
     |<--subscription_id-------------|                           |
     |                               |                           |
     |                         [Background: re-announce]         |
     |                         (every announce_interval)         |
```

## Configuration

```erlang
#{
    cache_ttl => 300,              % Subscriber cache TTL (seconds)
    announce_interval => 3600,     % DHT re-announce interval (1 hour)
    max_cache_size => 1000,        % Max cached subscriber lists
    delivery_timeout => 5000,      % Remote delivery timeout
    max_fanout_concurrency => 100  % Max parallel deliveries
}
```

## Testing Strategy

### Unit Tests (EUnit)
- Topic pattern matching (wildcards)
- Registry add/remove/match operations
- Cache LRU eviction
- Local delivery

### Integration Tests (Common Test)
- 3-node pub/sub network
- Local and remote subscriptions
- DHT-based discovery
- Message delivery verification

### Property Tests (PropEr)
- All subscribers receive matching publications
- No message duplication
- Pattern matching correctness
- Cache consistency with DHT

## Performance Targets

- **Local Delivery**: < 1ms (direct process send)
- **Remote Delivery**: < 50ms (DHT lookup + QUIC delivery)
- **Cached Remote**: < 10ms (skip DHT lookup)
- **Throughput**: 10,000 messages/sec per node
- **Subscription Limit**: 10,000 patterns per node

## Implementation Order (TDD)

1. **macula_pubsub_topic** - Topic pattern matching
2. **macula_pubsub_registry** - Local subscription registry
3. **macula_pubsub_cache** - Subscriber cache with LRU
4. **macula_pubsub_discovery** - DHT integration
5. **macula_pubsub_delivery** - Message routing and delivery
6. **macula_pubsub_server** - GenServer integration
7. **Integration tests** - Multi-node pub/sub scenarios

Each module implemented with:
1. Write tests first (Red)
2. Implement minimal code (Green)
3. Refactor and add property tests
4. Verify 95%+ coverage
