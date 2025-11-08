# macula_membership Architecture

## Overview

Implementation of SWIM (Scalable Weakly-consistent Infection-style process group Membership) protocol for distributed failure detection and membership management in the Macula mesh.

## SWIM Protocol Basics

**Goal**: Detect node failures in a distributed system efficiently

**Key Properties**:
- **Scalability**: O(1) message load per node per protocol period
- **Weak Consistency**: Eventually consistent membership view
- **Infection-Style**: Gossip-based information dissemination

## Architecture Components

### 1. Member State (`macula_membership_member`)

Represents a single member in the cluster.

```erlang
-record(member, {
    node_id :: binary(),           % 32-byte node ID
    address :: {inet:ip_address(), inet:port_number()},
    status :: alive | suspect | dead,
    incarnation :: non_neg_integer(),  % Refute false suspicions
    metadata :: map()              % Custom data (realm, capabilities, etc.)
}).
```

**Status Transitions**:
- `alive` → `suspect` (timeout on ping)
- `suspect` → `alive` (refutation by node itself with higher incarnation)
- `suspect` → `dead` (timeout after suspicion)
- `dead` → removed (after dissemination)

### 2. Membership List (`macula_membership_list`)

Maintains the current view of cluster membership.

**API**:
- `new/1` - Create new membership list with local node
- `add_member/2` - Add new member (status: alive)
- `update_member/2` - Update member status/incarnation
- `get_member/2` - Lookup member by node_id
- `get_random_members/2` - Get N random members for gossip
- `get_alive_members/1` - Get all alive members
- `get_suspect_members/1` - Get all suspected members
- `size/1` - Number of members

**Storage**: ETS table for fast concurrent access

### 3. Failure Detector (`macula_membership_detector`)

Implements SWIM failure detection protocol.

**Protocol Period** (e.g., 1 second):
1. **Select Random Member**: Choose random member to probe
2. **Direct Ping**: Send `swim_ping` message
3. **Wait for Ack**: Timeout = protocol_period / 2
4. **Indirect Ping** (if no ack):
   - Select k random members
   - Ask them to ping target via `swim_ping_req`
   - Wait for acks
5. **Suspect**: Mark as suspect if no acks received
6. **Disseminate**: Piggyback membership changes on messages

**GenServer State**:
```erlang
-record(state, {
    local_node_id :: binary(),
    membership_list :: pid(),          % Membership list GenServer
    protocol_period :: pos_integer(),  % Milliseconds
    indirect_count :: pos_integer(),   % k in paper (default: 3)
    suspect_timeout :: pos_integer(),  % Before marking dead
    transport :: pid()                 % macula_quic connection pool
}).
```

**Messages**:
- `swim_ping` - Direct probe
- `swim_ack` - Response to ping
- `swim_ping_req` - Indirect probe request
- Uses macula_protocol message types

### 4. Gossip Dissemination (`macula_membership_gossip`)

Piggybacks membership updates on protocol messages.

**Gossip Payload** (attached to every message):
```erlang
-type gossip_updates() :: [{node_id(), status(), incarnation()}].
```

**Strategy**:
- Recent changes gossiped more frequently
- Exponential decay: log(N) messages per update
- Bounded size: max M updates per message (e.g., M=10)

**API**:
- `add_update/3` - Record status change for dissemination
- `get_updates/1` - Get updates to piggyback (bounded)
- `merge_updates/2` - Apply received updates to local view

### 5. Transport Integration (`macula_membership_transport`)

Wrapper around macula_quic for SWIM messages.

**Functions**:
- `send_ping/3` - Send swim_ping to target
- `send_ack/3` - Send swim_ack response
- `send_ping_req/4` - Send indirect ping request
- `broadcast/2` - Send message to all alive members

**Connection Pooling**:
- Maintain QUIC connections to recently contacted nodes
- Lazy connection establishment
- Connection timeout (e.g., 60 seconds idle)

## Data Flow

### Successful Ping (Happy Path)

```
Node A                  Node B
  |                       |
  |----swim_ping--------->|
  |   (incarnation: 5)    |
  |                       |
  |<---swim_ack-----------|
  |   (incarnation: 5)    |
  |                       |
  v                       v
Mark B as alive       Continue
```

### Failed Ping → Indirect Ping

```
Node A              Node C              Node B (suspected)
  |                   |                   |
  |----swim_ping-------------------X (timeout)
  |                   |                   |
  |--swim_ping_req--->|                   |
  |  (target: B)      |                   |
  |                   |----swim_ping----->|
  |                   |                   |
  |                   |<---swim_ack-------|
  |                   |                   |
  |<--swim_ack--------|                   |
  |  (from B via C)   |                   |
  v                   v                   v
Mark B as alive   Forward ack        Continue
```

### Refutation (False Positive)

```
Node A                  Node B
  |                       |
  |<--swim_ping-----------|
  |  (B suspects A)       |
  |                       |
  |---swim_ack----------->|
  | (incarnation: 6)      |
  | (was 5, refuted!)     |
  |                       |
  v                       v
Increment           Mark A as alive
incarnation         (higher incarnation wins)
```

## Configuration

```erlang
#{
    protocol_period => 1000,      % 1 second
    indirect_count => 3,          % k indirect probes
    suspect_timeout => 5000,      % 5 seconds before dead
    gossip_fanout => 10,          % Max updates per message
    connection_timeout => 60000   % 1 minute idle
}
```

## Testing Strategy

### Unit Tests (EUnit)
- Member state transitions
- Membership list operations
- Gossip update merging
- Incarnation comparison logic

### Integration Tests (Common Test)
- 3-node cluster failure detection
- Network partition scenarios
- Refutation of false suspicions
- Gossip convergence

### Property Tests (PropEr)
- Eventually consistent membership view
- No false negatives (alive nodes not marked dead)
- Bounded detection time
- Message count is O(1) per node

## Performance Targets

- **Detection Time**: < 5 seconds (suspect_timeout)
- **Message Load**: O(1) per node per protocol_period
- **Scalability**: Tested up to 100 nodes
- **Convergence**: log(N) * protocol_period for gossip

## Implementation Order (TDD)

1. **macula_membership_member** - Member record and state transitions
2. **macula_membership_list** - ETS-backed membership list
3. **macula_membership_gossip** - Gossip update tracking
4. **macula_membership_transport** - QUIC transport wrapper
5. **macula_membership_detector** - Main SWIM failure detector
6. **Integration tests** - Multi-node scenarios

Each module will be implemented with:
1. Write tests first (Red)
2. Implement minimal code (Green)
3. Refactor and add property tests
4. Verify 95%+ coverage
