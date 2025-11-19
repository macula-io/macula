# Protocol Design Practices

Best practices for designing RPC and PubSub protocols in Macula applications.

## AI-Assisted Protocol Brainstorming

When designing distributed protocols for Macula applications, use this structured approach with AI assistance to ensure robust, scalable designs.

### Step 1: Define the User Journey

Start by mapping out the complete user interaction flow:

```
Example: Game Matchmaking
0. PlayerX presses "Insert Coin"
1. Player joins matchmaking queue
2. System finds opponent
3. Game starts
4. Players exchange moves
5. Game ends with winner
```

**AI Prompt Template:**
> "I'm building [application type] on Macula. The user journey is:
> [list steps]
> Help me design the RPC and PubSub protocol following Macula's distributed system design principles."

### Step 2: Apply Naming Conventions

Categorize each interaction as event or command:

| Interaction | Type | Name (Past/Imperative) |
|------------|------|------------------------|
| Player joins queue | RPC | `register_player` |
| Player matched | Event | `match_found` |
| Game starts | Event | `game_started` |
| Player moves | RPC | `submit_action` |
| Game ends | Event | `game_ended` |

**Rules:**
- Events = Past tense (facts that happened)
- RPC methods = Imperative present tense (commands to execute)

### Step 3: Design for Massive Scale

Review each topic/procedure name:

**Checklist:**
- [ ] No entity IDs in topic names
- [ ] IDs are in payloads only
- [ ] Topics describe types, not instances
- [ ] Can handle 1M+ entities without topic explosion

**Bad vs Good Examples:**

| Bad (ID in topic) | Good (ID in payload) |
|-------------------|----------------------|
| `game.{id}.state` | `game.state_updated` + `{ game_id, ... }` |
| `player.{id}.matched` | `match_found` + `{ player_id, ... }` |
| `room.{id}.message` | `chat.message_sent` + `{ room_id, ... }` |

### Step 4: Choose Decentralized Patterns

For each protocol element, prefer decentralized solutions:

**Matchmaking Example:**

| Approach | Centralized | Decentralized |
|----------|-------------|---------------|
| Queue storage | Single coordinator | DHT with replication |
| Match selection | Coordinator decides | Deterministic algorithm (all nodes agree) |
| Conflict resolution | Lock-based | Timestamp ordering, first-wins |

**Decentralization Checklist:**
- [ ] No single point of failure
- [ ] Nodes can operate autonomously
- [ ] Eventual consistency is acceptable
- [ ] Deterministic decisions (same inputs → same outputs)

### Step 5: Handle Edge Cases

Brainstorm failure scenarios:

1. **Race conditions**: Two nodes try to match same player
2. **Network partitions**: Nodes can't communicate
3. **Node failures**: Coordinator crashes mid-operation
4. **Timeouts**: RPC calls don't return

**Resolution Patterns:**

| Scenario | Pattern |
|----------|---------|
| Duplicate matches | Deterministic selection (lowest timestamp wins) |
| Partition | Local operation continues, reconcile later |
| Node failure | TTL expiration, automatic cleanup |
| Timeout | Retry with idempotency keys |

### Step 6: Document the Protocol

Create a protocol specification document with:

1. **Overview**: What the protocol accomplishes
2. **Events**: All pub/sub events with payloads
3. **RPC Methods**: All procedures with args/returns
4. **State Machine**: Valid state transitions
5. **Error Handling**: Failure modes and recovery
6. **Examples**: Complete interaction sequences

## Protocol Specification Template

```markdown
# [Application] Protocol Specification

## Overview
[Brief description of what this protocol enables]

## Events (PubSub)

### `namespace.event_name`
**Published when:** [trigger condition]
**Payload:**
```json
{
  "entity_id": "string",
  "timestamp": "iso8601",
  "data": {}
}
```
**Subscribers:** [who listens and why]

## RPC Methods

### `namespace.method_name`
**Purpose:** [what this does]
**Arguments:**
```json
{
  "arg1": "type",
  "arg2": "type"
}
```
**Returns:**
```json
{
  "result": "type"
}
```
**Errors:** [possible error conditions]

## State Machine

```
[Initial] → event1 → [State1] → event2 → [State2] → ...
```

## Error Handling

| Error | Cause | Recovery |
|-------|-------|----------|
| ... | ... | ... |

## Example Sequence

1. Node A calls `method1`
2. Node B publishes `event1`
3. ...
```

## Common Protocol Patterns

### Distributed Queue Pattern

For matchmaking, job queues, resource allocation:

```
register_item → item_registered (broadcast)
find_items → returns list
claim_item → item_claimed (broadcast)
```

Key: Use deterministic claim resolution (timestamps, hashes).

### Request-Response Pattern

For synchronous operations:

```
submit_request → returns result
```

Key: Include request_id for idempotency.

### Saga Pattern

For multi-step distributed transactions:

```
step1_completed → triggers step2
step2_completed → triggers step3
step3_failed → triggers compensate_step2, compensate_step1
```

Key: Each step must be reversible.

### Observer Pattern

For state synchronization:

```
state_updated (broadcast) → all subscribers update local state
```

Key: Include full state or delta with sequence numbers.

## Validation Checklist

Before implementing, verify:

- [ ] All events use past tense
- [ ] All RPC methods use imperative present tense
- [ ] No IDs in topic/procedure names
- [ ] Decentralized where possible
- [ ] Failure modes documented
- [ ] Idempotency considered
- [ ] Timeouts defined
- [ ] Cleanup mechanisms exist (TTLs, garbage collection)

## References

- [Macula Distributed System Design Principles](../../../CLAUDE.md#distributed-system-design-principles)
- [Event-Driven Design Principles](../../../CLAUDE.md#event-driven-design-principles)
- [Topic Design Guidelines](../../../CLAUDE.md#topic-design-for-pubsub)
