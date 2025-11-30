# Macula TWEANN

> **Audience:** Developers, AI/ML Engineers, Technical Architects
> **Last Updated:** 2025-11-28

Topology and Weight Evolving Artificial Neural Networks for distributed, evolutionary intelligence on the Macula mesh.

---

## What is TWEANN?

TWEANN (Topology and Weight Evolving Artificial Neural Network) is a neuroevolution paradigm where neural networks evolve both their structure (topology) and connection weights through natural selection, rather than gradient descent.

Unlike traditional deep learning:
- **No backpropagation** - evolution replaces gradient descent
- **No fixed architecture** - networks grow and adapt their structure
- **No centralized training** - agents evolve locally, share successes via mesh
- **No GPU clusters** - runs on commodity hardware at the edge

---

## Key Concepts

### Genotype vs Phenotype

```
Genotype (Blueprint)              Phenotype (Running Network)
+-------------------+             +-------------------+
| Sensor specs      |   spawn     | Sensor processes  |
| Neuron specs      | ---------> | Neuron processes  |
| Actuator specs    |             | Actuator processes|
| Connection weights|             | Message passing   |
+-------------------+             +-------------------+
       |                                   |
       |  stored in Mnesia                 |  OTP supervised
       v                                   v
   Persistent                          Ephemeral
```

- **Genotype**: The neural network blueprint stored in Mnesia
- **Phenotype**: The running network as Erlang processes

### Morphologies

A morphology defines the sensors and actuators for a problem domain:

| Morphology | Sensors | Actuators | Use Case |
|------------|---------|-----------|----------|
| xor_mimic | 2 inputs | 1 output | Function learning |
| pole_balancing | Cart state | Force | Control systems |
| discrete_tmaze | Vision | Move/Turn | Navigation |
| forex_trader | Market data | Buy/Sell/Hold | Trading |

### Evolution Operators

**Mutation Types:**
- Add/remove neurons
- Add/remove connections (links)
- Modify connection weights
- Add/remove sensors/actuators
- Change activation functions

**Selection:**
- Fitness-proportionate selection
- Tournament selection
- Pareto dominance (multi-objective)

**Speciation:**
- Groups similar topologies together
- Protects innovation from premature elimination
- Maintains population diversity

---

## Architecture

### Process Model

Each neural network runs as a supervision tree:

```
        +----------+
        | Exoself  |  (Lifecycle manager)
        +----+-----+
             |
        +----v-----+
        |  Cortex  |  (Coordinator)
        +----+-----+
             |
    +--------+--------+
    |        |        |
+---v---+ +--v--+ +---v----+
|Sensor | |Neuron| |Actuator|
+-------+ +--+--+ +--------+
             |
        +----v-----+
        |  Neuron  |
        +----+-----+
             |
        +----v-----+
        |  Neuron  |
        +----------+
```

**Cortex**: Coordinates signal flow through the network
**Sensor**: Reads environment, sends signals to neurons
**Neuron**: Processes signals, applies activation function
**Actuator**: Converts neural output to actions
**Exoself**: Manages lifecycle, handles crashes

### Module Organization

```
macula_tweann/
+-- genotype.erl        # Blueprint construction & Mnesia storage
+-- constructor.erl     # Phenotype spawning from genotype
+-- cortex.erl          # Network coordination process
+-- neuron.erl          # Neural computation process
+-- morphology.erl      # Problem domain definitions
+-- genome_mutator.erl  # Mutation operators
+-- selection_algorithm.erl  # Selection strategies
+-- population_monitor.erl   # Multi-agent evolution
```

---

## Macula Mesh Integration

### Distributed Evolution

TWEANN agents can share successful genomes across the Macula mesh:

```
Node A (Factory Floor)          Node B (Factory Floor)
+------------------+            +------------------+
| TWEANN Agent     |            | TWEANN Agent     |
| Fitness: 0.85    |            | Fitness: 0.72    |
+--------+---------+            +--------+---------+
         |                               ^
         | publish genome                | receive genome
         | (fitness > 0.80)              | incorporate
         v                               |
+--------+-------------------------------+---------+
|                Macula Pub/Sub                    |
|          Topic: "evolution.genome"               |
+--------------------------------------------------+
```

**Publishing genomes:**
```erlang
%% When agent exceeds fitness threshold
case Fitness > FitnessThreshold of
    true ->
        Genome = genotype:read(AgentId),
        macula:publish(
            Peer,
            <<"evolution.genome">>,
            term_to_binary({genome, Fitness, Genome})
        );
    false ->
        ok
end.
```

**Subscribing to genomes:**
```erlang
%% Subscribe to genome updates
macula:subscribe(
    Peer,
    <<"evolution.genome">>,
    fun({genome, Fitness, Genome}) ->
        %% Incorporate into local population
        population_monitor:incorporate_immigrant(Genome)
    end
).
```

### Use Cases with Macula

**1. Adaptive Manufacturing**
- Each production line runs TWEANN agents
- Agents optimize for local conditions (temperature, humidity, material batch)
- Successful optimizations shared across factory mesh
- Continuous improvement without central coordination

**2. Swarm Robotics**
- Each robot evolves control policies locally
- Successful behaviors propagate through swarm
- Collective intelligence emerges from mesh communication
- Fault-tolerant: any robot can be lost without losing learned behaviors

**3. Distributed Anomaly Detection**
- Sensors evolve detectors for local patterns
- Share detection models via mesh
- Collectively identify novel anomalies
- Adapt to changing conditions automatically

---

## Quick Start

### Installation

Add to your `rebar.config`:
```erlang
{deps, [{macula_tweann, "~> 0.8.8"}]}.
```

### Basic Usage

```erlang
%% Initialize Mnesia storage
genotype:init_db(),

%% Create agent with XOR morphology
Constraint = #constraint{morphology = xor_mimic},
{ok, AgentId} = genotype:construct_agent(Constraint),

%% Spawn phenotype
{ok, ExoselfPid} = constructor:spawn_agent(AgentId),

%% Mutate genotype
genome_mutator:mutate(AgentId),

%% Run evolution
population_monitor:evolve(#{
    morphology => xor_mimic,
    population_size => 100,
    generations => 1000
}).
```

### Creating Custom Morphologies

```erlang
-module(my_morphology).
-behavior(morphology).

sensors(_Agent) ->
    [
        #{name => temperature, vector_length => 1},
        #{name => pressure, vector_length => 1},
        #{name => flow_rate, vector_length => 1}
    ].

actuators(_Agent) ->
    [
        #{name => valve_position, vector_length => 1},
        #{name => pump_speed, vector_length => 1}
    ].
```

---

## References

- **Original Work**: Gene Sher, "Handbook of Neuroevolution Through Erlang" (2013)
- **NEAT Paper**: Stanley & Miikkulainen, "Evolving Neural Networks Through Augmenting Topologies" (2002)
- **Repository**: [github.com/macula-io/macula-tweann](https://github.com/macula-io/macula-tweann)
- **Hex Package**: [hex.pm/packages/macula_tweann](https://hex.pm/packages/macula_tweann)

---

## See Also

- [Use Cases: TWEANN-Based Systems](../business/USE_CASES.md#tweann-based-systems)
- [Ecosystem Overview](README.md)
- [Architecture Overview](../../ARCHITECTURE.md)

