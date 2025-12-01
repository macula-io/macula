# Platform System

The Platform System provides masterless state management using Conflict-free Replicated Data Types (CRDTs). As of v0.14.0, Macula uses CRDTs instead of Raft consensus for distributed state.

## Module Table

| Module | Purpose | LOC |
|--------|---------|-----|
| `macula_platform_system` | Supervisor for platform subsystem (currently empty) | 2.7k |
| `macula_crdt` | CRDT implementations (LWW-Register, OR-Set, G-Counter, PN-Counter) | 12k |

## Architecture Decision: No Raft

**v0.14.0**: Ra/Raft removed in favor of CRDTs.

> Raft adds operational complexity for consistency guarantees Macula doesn't need.
> - No quorum management
> - No leader election
> - State converges eventually (CRDTs + Gossip)

Macula operates in **eventually-consistent mode** (AP in CAP theorem):
- Nodes operate during network partitions
- No need for strong consensus
- CRDTs provide automatic conflict resolution
- Simpler operational model

## CRDT Types

### LWW-Register (Last-Writer-Wins Register)
Single value with timestamp-based conflict resolution:
```erlang
%% Create new register
Reg1 = macula_crdt:new_lww(<<"initial_value">>).

%% Update value (automatically timestamps)
Reg2 = macula_crdt:lww_set(Reg1, <<"new_value">>).

%% Get current value
{ok, Value} = macula_crdt:lww_get(Reg2).

%% Merge two registers (last writer wins)
Merged = macula_crdt:lww_merge(RegA, RegB).
```

### OR-Set (Observed-Remove Set)
Add/remove set with tombstone-based conflict resolution:
```erlang
%% Create new set
Set1 = macula_crdt:new_or_set().

%% Add elements
Set2 = macula_crdt:or_add(Set1, <<"element1">>).
Set3 = macula_crdt:or_add(Set2, <<"element2">>).

%% Remove element
Set4 = macula_crdt:or_remove(Set3, <<"element1">>).

%% Check membership
true = macula_crdt:or_contains(Set4, <<"element2">>).
false = macula_crdt:or_contains(Set4, <<"element1">>).

%% Get all elements
[<<"element2">>] = macula_crdt:or_elements(Set4).

%% Merge two sets
Merged = macula_crdt:or_merge(SetA, SetB).
```

### G-Counter (Grow-only Counter)
Counter that can only increment:
```erlang
%% Create new counter
Counter1 = macula_crdt:new_gcounter().

%% Increment (default by 1)
Counter2 = macula_crdt:gcounter_increment(Counter1).

%% Increment by specific amount
Counter3 = macula_crdt:gcounter_increment(Counter2, 5).

%% Get current value
6 = macula_crdt:gcounter_value(Counter3).

%% Merge counters
Merged = macula_crdt:gcounter_merge(CounterA, CounterB).
```

### PN-Counter (Positive-Negative Counter)
Counter that can increment and decrement:
```erlang
%% Create new counter
Counter1 = macula_crdt:new_pncounter().

%% Increment
Counter2 = macula_crdt:pncounter_increment(Counter1, 10).

%% Decrement
Counter3 = macula_crdt:pncounter_decrement(Counter2, 3).

%% Get current value
7 = macula_crdt:pncounter_value(Counter3).

%% Merge counters
Merged = macula_crdt:pncounter_merge(CounterA, CounterB).
```

## CRDT Properties

All CRDTs in Macula satisfy:

1. **Commutativity**: merge(A, B) = merge(B, A)
2. **Associativity**: merge(merge(A, B), C) = merge(A, merge(B, C))
3. **Idempotency**: merge(A, A) = A

These properties ensure:
- Order of merge operations doesn't matter
- Repeated merges are safe
- State converges to consistent value

## Test Coverage

| CRDT Type | Tests | Coverage |
|-----------|-------|----------|
| LWW-Register | 14 | Full API coverage |
| OR-Set | 17 | Add/remove/merge/concurrent ops |
| G-Counter | 9 | Increment/merge/value |
| PN-Counter | 8 | Inc/dec/merge/value |
| **Total** | **48** | |

## Supervision Tree

```
macula_platform_system (supervisor)
└── (empty - CRDTs are stateless)
```

The Platform System supervisor is intentionally empty. CRDTs are pure data structures - they don't need background processes. State is stored in application-level processes and merged on demand.

## Future Work (v0.14.1+)

- **Gossip Protocol**: Automatic CRDT state synchronization between nodes
- **DHT Integration**: Store CRDTs in the DHT for global access
- **Delta-CRDTs**: Optimized sync using deltas instead of full state

## Migration from Ra/Raft

v0.14.0 removed the following modules:
- `macula_leader_election.erl` (deleted)
- `macula_leader_machine.erl` (deleted)
- `ra` dependency removed from rebar.config

If your code referenced leader election:
```erlang
%% Old (pre-v0.14.0)
{ok, Leader} = macula_leader_election:get_leader().

%% New (v0.14.0+) - No leader, use local state
%% State converges via CRDT merge during gossip
```

## Related Documentation

- [Roadmap - SuperMesh v3.0](../../architecture/ROADMAP.md)
- [CRDT Foundation Architecture](../../architecture/v0.14.0-CRDT_FOUNDATION.md)
