# macula_routing Architecture

## Overview

Implementation of Kademlia DHT (Distributed Hash Table) for peer routing and content discovery in the Macula mesh.

## Kademlia DHT Basics

**Goal**: Efficiently route to any node in a distributed network using O(log N) hops

**Key Properties**:
- **XOR Distance Metric**: Distance between node IDs using bitwise XOR
- **K-Buckets**: Routing table organized by distance
- **Iterative Lookup**: Find closest nodes through successive queries
- **Redundancy**: Store values at k closest nodes

## Architecture Components

### 1. Node ID (`macula_routing_nodeid`)

256-bit node identifiers with XOR distance calculations.

```erlang
-type node_id() :: binary().  % 32 bytes (256 bits)

%% Distance calculation
distance(NodeId1, NodeId2) -> XorDistance :: binary().

%% Leading zeros (for bucket index)
leading_zeros(NodeId) -> Count :: 0..256.

%% Comparison
closer_to(Target, NodeA, NodeB) -> boolean().
```

**Features**:
- XOR distance metric for routing
- Leading zero count determines bucket index
- Distance comparison for iterative lookup

### 2. K-Bucket (`macula_routing_bucket`)

Stores k nodes at a specific distance range.

```erlang
-record(bucket, {
    nodes :: [node_info()],        % Up to k nodes
    last_changed :: integer(),     % Timestamp
    replacement_cache :: [node_info()]  % Failed node replacements
}).

-record(node_info, {
    node_id :: binary(),
    address :: {inet:ip_address(), inet:port_number()},
    last_seen :: integer()
}).
```

**K-Bucket Rules**:
- Maximum k nodes per bucket (typically k=20)
- Least-recently-seen eviction
- Replacement cache for failed nodes
- Bucket splitting when local bucket becomes full

**API**:
- `new/1` - Create bucket with capacity k
- `add_node/2` - Add or update node
- `remove_node/2` - Remove failed node
- `get_nodes/1` - Get all nodes in bucket
- `find_closest/3` - Find n closest to target

### 3. Routing Table (`macula_routing_table`)

Collection of k-buckets organized by distance.

```erlang
-record(routing_table, {
    local_node_id :: binary(),
    buckets :: #{bucket_index() => bucket()},  % 0..255
    k :: pos_integer()  % Bucket size (default: 20)
}).
```

**Bucket Index Calculation**:
```erlang
bucket_index(LocalNodeId, TargetNodeId) ->
    Distance = xor_distance(LocalNodeId, TargetNodeId),
    leading_zeros(Distance).  % 0..255
```

**API**:
- `new/2` - Create table with local node ID and k
- `add_node/3` - Add node to appropriate bucket
- `remove_node/2` - Remove node
- `find_closest/3` - Find k closest nodes to target
- `get_bucket/2` - Get bucket by index
- `size/1` - Total number of nodes

### 4. DHT Operations (`macula_routing_dht`)

Core DHT algorithms: iterative lookup, store, find.

**Iterative Lookup Algorithm**:
```erlang
iterative_lookup(Target, RoutingTable, Alpha, K) ->
    %% Alpha = concurrency (3)
    %% K = desired results (20)

    %% 1. Get k closest from routing table
    Closest = routing_table:find_closest(RoutingTable, Target, K),

    %% 2. Query alpha nodes in parallel
    %% 3. Add responses to closest set
    %% 4. Query next alpha unqueried nodes
    %% 5. Repeat until no closer nodes or all queried

    {ok, KClosestNodes}.
```

**DHT Operations**:
- `find_node/2` - Find k closest nodes to target
- `find_value/2` - Find value stored at key
- `store/3` - Store key-value at k closest nodes
- `lookup/3` - Iterative lookup algorithm

### 5. Message Handling (`macula_routing_protocol`)

Integration with macula_protocol DHT messages.

**Message Types** (already defined in macula_protocol):
- `find_node` - Request k closest nodes
- `find_node_reply` - Return k closest nodes
- `store` - Store key-value pair
- `find_value` - Request value by key

**Message Flow**:
```
Node A                          Node B
  |                               |
  |----find_node(Target)--------->|
  |   {target: 256-bit ID}        |
  |                               |
  |<---find_node_reply------------|
  |   {nodes: [node_info]}        |
  |                               |
```

### 6. DHT State Machine (`macula_routing_server`)

GenServer managing routing table and DHT operations.

```erlang
-record(state, {
    local_node_id :: binary(),
    routing_table :: routing_table(),
    pending_lookups :: #{lookup_id() => lookup_state()},
    storage :: #{binary() => term()},  % Local key-value store
    config :: #{
        k => 20,              % Bucket size
        alpha => 3,           % Lookup concurrency
        ttl => 86400,         % Value TTL (seconds)
        republish => 3600     % Republish interval
    }
}).
```

**GenServer API**:
- `start_link/2` - Start DHT server
- `add_node/2` - Add node to routing table
- `find_node/2` - Find k closest nodes (async)
- `find_value/2` - Find value by key (async)
- `store/3` - Store value at k closest nodes (async)
- `get_routing_table/1` - Get current routing table

**Message Handling**:
- Handle incoming find_node requests
- Handle incoming store requests
- Process lookup responses
- Update routing table on contact

## Data Flow

### Node Lookup (FIND_NODE)

```
Requesting Node                 Target Area Nodes
     |                              |
     |--find_node(Target)---------->| Node1 (closest known)
     |                              |
     |<--find_node_reply------------|
     |  [Node2, Node3, Node4]       |
     |                              |
     |--find_node(Target)---------->| Node2 (closer)
     |                              |
     |<--find_node_reply------------|
     |  [Node5, Node6, Node7]       |
     |                              |
     |--find_node(Target)---------->| Node5 (even closer)
     |                              |
     |<--find_node_reply------------|
     |  [Node8, Node9, Target!]     |
     |                              |
     v                              v
  Result: [Target, Node8, Node9, ...]
  (k closest nodes to Target)
```

### Value Storage (STORE)

```
Storing Node                    Closest Nodes
     |                              |
     |--find_node(Key)------------->|
     |<--[k closest nodes]----------|
     |                              |
     |--store(Key, Value, TTL)----->| Node1
     |--store(Key, Value, TTL)----->| Node2
     |--store(Key, Value, TTL)----->| ...
     |--store(Key, Value, TTL)----->| NodeK
     |                              |
     |<--ack------------------------|
```

### Value Retrieval (FIND_VALUE)

```
Requesting Node                 Storage Nodes
     |                              |
     |--find_node(Key)------------->|
     |<--[k closest nodes]----------|
     |                              |
     |--find_value(Key)------------>| Node1 (no value)
     |<--find_node_reply------------|
     |                              |
     |--find_value(Key)------------>| Node2 (HAS VALUE!)
     |<--{ok, Value}----------------|
     |                              |
     v                              v
  Result: Value
  (cache at closer nodes)
```

## Configuration

```erlang
#{
    k => 20,                  % Replication factor / bucket size
    alpha => 3,               % Lookup concurrency
    ttl => 86400,            % Value TTL (24 hours)
    republish => 3600,       % Republish interval (1 hour)
    refresh => 3600,         % Bucket refresh interval
    max_pending_lookups => 100  % Concurrent lookup limit
}
```

## Testing Strategy

### Unit Tests (EUnit)
- XOR distance calculations
- K-bucket operations (add, remove, evict)
- Routing table bucket selection
- Node proximity comparisons

### Integration Tests (Common Test)
- 3-node DHT network setup
- Iterative lookup convergence
- Value storage and retrieval
- Bucket refresh and maintenance

### Property Tests (PropEr)
- Lookup always returns k closest nodes
- XOR metric triangle inequality
- Value stored is always retrievable
- Routing table never exceeds k*256 nodes

## Performance Targets

- **Lookup Time**: O(log N) hops
- **Routing Table Size**: O(log N) nodes stored
- **Message Overhead**: Alpha concurrent requests per hop
- **Convergence**: < 5 hops for 10,000 node network

## Implementation Order (TDD)

1. **macula_routing_nodeid** - Node ID utilities and XOR distance
2. **macula_routing_bucket** - K-bucket with LRU eviction
3. **macula_routing_table** - Routing table management
4. **macula_routing_dht** - Core DHT algorithms (pure functions)
5. **macula_routing_protocol** - Message encoding/handling
6. **macula_routing_server** - GenServer integration
7. **Integration tests** - Multi-node DHT scenarios

Each module implemented with:
1. Write tests first (Red)
2. Implement minimal code (Green)
3. Refactor and add property tests
4. Verify 95%+ coverage
