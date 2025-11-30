# Ecosystem Libraries

> **Audience:** Developers, Technical Architects
> **Last Updated:** 2025-11-28

Macula integrates with several companion libraries that extend its capabilities for specific use cases. This section provides an overview of these related projects.

---

## Overview

The Macula ecosystem consists of libraries that work together to enable decentralized, adaptive systems:

| Library | Purpose | Language |
|---------|---------|----------|
| [Macula TWEANN](TWEANN.md) | Evolutionary neural networks | Erlang/Rust |
| [Reckon Architecture](RECKON.md) | Event sourcing patterns | Elixir |

---

## Macula TWEANN

**Topology and Weight Evolving Artificial Neural Networks**

Neural networks that evolve both their structure and weights through natural selection. Unlike traditional deep learning which requires centralized GPU clusters, TWEANN agents can evolve on edge devices and share successful mutations via the mesh.

**Key Features:**
- Topology evolution (add/remove neurons and connections)
- Weight optimization through selection
- Speciation for diversity preservation
- Multi-objective optimization
- Process-safe implementation with OTP supervision

**Macula Integration:**
```
Edge Node 1                 Edge Node 2
+-----------+               +-----------+
| TWEANN    |               | TWEANN    |
| Agent     |               | Agent     |
+-----+-----+               +-----+-----+
      |                           |
      |  Genome published via     |
      |  pub/sub when fitness     |
      |  threshold reached        |
      |                           |
+-----v---------------------------v-----+
|           Macula Mesh                 |
|   (pub/sub: "evolution.genome")       |
+---------------------------------------+
```

Nodes subscribe to genome updates and incorporate successful mutations into their local populations.

**[Read More: TWEANN Guide](TWEANN.md)**

---

## Reckon Architecture

**Event Sourcing Patterns for BEAM Applications**

Reckon provides architectural patterns for building event-sourced systems on the BEAM. While not a library in the traditional sense, it defines the conventions and structures that make Macula applications consistent and maintainable.

**Core Concepts:**
- **Vertical Slicing**: One directory per business operation
- **Event-Driven**: Events are immutable facts, not CRUD operations
- **CQRS**: Separate read models optimized for queries
- **Projections**: Transform events into read models

**Macula Integration:**

Events produced by Macula applications follow Reckon conventions:
- Events are past-tense facts (e.g., `player_joined`, not `join_player`)
- Commands are present-tense actions (e.g., `join_game`)
- Topics describe event types, not entity instances
- IDs belong in payloads, not topic names

**[Read More: Reckon Guide](RECKON.md)**

---

## Integration Patterns

### Distributed AI with TWEANN

```
                 Macula Mesh
                      |
    +-----------------+------------------+
    |                 |                  |
+---v---+         +---v---+          +---v---+
|Sensor |         | Robot |          | Drone |
|  Node |         |  Arm  |          |       |
+---+---+         +---+---+          +---+---+
    |                 |                  |
+---v---+         +---v---+          +---v---+
|TWEANN |         |TWEANN |          |TWEANN |
|Agent  |         |Agent  |          |Agent  |
+-------+         +-------+          +-------+

Each node:
1. Runs local TWEANN evolution
2. Publishes fit genomes to mesh
3. Subscribes to genome updates
4. Incorporates successful mutations
```

### Event-Sourced Services

```
Command                    Event                      Read Model
+----------+              +----------+               +----------+
| join     |  Handler     | player   |  Projection   | player   |
| queue    | -----------> | queued   | ------------> | queue    |
+----------+              +----------+               | summary  |
                                                     +----------+
     ^                         |
     |                         v
+----+----+              +-----+----+
| Macula  |  pub/sub     | Macula   |
| RPC     | <----------- | Pub/Sub  |
+---------+              +----------+
```

---

## Future Libraries

The following libraries are planned or in early development:

| Library | Purpose | Status |
|---------|---------|--------|
| macula_crdt | Conflict-free replicated data types | Planned |
| macula_consensus | Ra/Raft integration for coordination | v0.9.0 |
| macula_storage | Distributed storage abstraction | Planned |

---

## See Also

- [Use Cases](../business/USE_CASES.md) - Business applications using these libraries
- [Architecture Overview](../../ARCHITECTURE.md) - Technical architecture
- [Developer Guide](../developer/) - Building with Macula

