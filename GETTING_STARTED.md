<div align="center">
  <img src="artwork/macula-alt-logo.svg" alt="Macula Logo" width="500"/>

  <h1>Macula HTTP/3 Mesh</h1>
  <p><em>Self-organizing distributed mesh for decentralized applications</em></p>

  <p>
    <a href="LICENSE"><img src="https://img.shields.io/badge/License-Apache%202.0-blue.svg" alt="License"/></a>
    <a href="https://www.erlang.org"><img src="https://img.shields.io/badge/Erlang%2FOTP-27+-brightgreen" alt="Erlang/OTP"/></a>
    <a href="https://hex.pm/packages/macula"><img src="https://img.shields.io/hexpm/v/macula.svg" alt="Hex.pm"/></a>
  </p>
</div>

---

<div align="center">
  <img src="artwork/macula-architecture-overview.svg" alt="Macula Architecture Overview" width="800"/>
</div>

<div align="center">
  <p><strong>BEAM-Native | HTTP/3 (QUIC) | Kademlia DHT | Direct P2P | Multi-Tenant | Platform Layer</strong></p>
</div>

---

## Table of Contents

- [Architecture Overview](ARCHITECTURE.md) - Visual guide with C4 diagrams
- [Quick Start](#quick-start) - Get started in minutes
- [Documentation](#documentation) - Full documentation index
- [Core Concepts](#core-concepts) - Understanding the mesh
- [API Overview](#api-overview) - Using Macula in your application
- [Changelog](CHANGELOG.md) - Version history and migration guides

---

## What is Macula?

Macula is infrastructure for building **decentralized applications and services** that operate autonomously at the edge, without dependency on centralized cloud infrastructure.

**Key Features:**

- **BEAM-native** - Erlang/Elixir OTP supervision and fault tolerance
- **HTTP/3 (QUIC)** - Modern, encrypted, NAT-friendly transport
- **Edge-first design** - Works through firewalls and NAT
- **Built-in pub/sub & RPC** - No external message broker needed
- **Multi-tenancy** - Realm isolation for SaaS and shared infrastructure
- **Self-organizing mesh** - DHT-based service discovery
- **Platform Layer** - Raft consensus and CRDT support (v0.9.0+)
- **Production-ready** - Memory management, comprehensive testing

---

## Documentation

| I want to... | Go to... |
|--------------|----------|
| Understand why Macula exists | [Platform Overview](docs/business/OVERVIEW.md) |
| Understand the socio-economic vision | [Motivation](docs/business/MOTIVATION.md) |
| Compare Macula to Kafka/RabbitMQ/NATS | [Technology Comparison](docs/business/COMPARISON.md) |
| Get started quickly | [Quick Start](docs/user/QUICK_START.md) |
| Build my first app | [Hello World Tutorial](docs/user/HELLO_WORLD.md) |
| Deploy to production | [Performance Guide](docs/operator/PERFORMANCE_GUIDE.md) |
| Understand RPC patterns | [RPC Guide](docs/developer/RPC_GUIDE.md) |
| Understand PubSub patterns | [PubSub Guide](docs/developer/PUBSUB_GUIDE.md) |
| Look up terminology | [Glossary](docs/GLOSSARY.md) |

**[Full Documentation Index](docs/README.md)**

---

## Architecture at a Glance

**System Context** - How your application uses Macula:

```
┌──────────────┐
│     Your     │
│ Application  │
└──────┬───────┘
       │ macula API
       ▼
┌──────────────┐     QUIC/HTTP3      ┌──────────────┐
│ Macula Peer  │◄───────────────────►│   Gateway    │
│ (Local Node) │    Or Direct P2P    │ (Relay Node) │
└──────┬───────┘                     └──────┬───────┘
       │                                    │
       └────────────► DHT ◄─────────────────┘
                 (Service Discovery)
```

**Message Flow** (Direct P2P):

```
Client ──1. Query DHT──► DHT (Find Service)
Client ◄─2. Endpoint──── DHT Returns "192.168.1.50:9443"
Client ──3. Direct────► Provider (1-hop, ~50ms)
Client ◄─4. Response─── Provider
```

**[See Full Architecture Guide](ARCHITECTURE.md)** with C4 diagrams, supervision trees, and deployment topologies.

---

## Installation

**Elixir (mix.exs):**

```elixir
def deps do
  [
    {:macula, "~> 0.10"}
  ]
end
```

**Erlang (rebar.config):**

```erlang
{deps, [
    {macula, "0.10.1"}
]}.
```

**Latest Release**: v0.10.1 (November 2025)

---

## Quick Start

### 1. Connect to a Gateway

```erlang
%% Connect to a remote gateway
{ok, Client} = macula:connect(<<"https://gateway.example.com:9443">>, #{
    realm => <<"com.example.app">>
}).

%% Or connect locally (same node)
{ok, Client} = macula:connect_local(#{
    realm => <<"com.example.app">>
}).
```

### 2. Publish/Subscribe

```erlang
%% Subscribe to events
{ok, SubRef} = macula:subscribe(Client, <<"sensor.temperature">>, fun(Event) ->
    #{celsius := Temp} = Event,
    io:format("Temperature: ~p C~n", [Temp])
end).

%% Publish an event
ok = macula:publish(Client, <<"sensor.temperature">>, #{
    device_id => <<"sensor-001">>,
    celsius => 21.5,
    timestamp => erlang:system_time(millisecond)
}).

%% Unsubscribe when done
ok = macula:unsubscribe(Client, SubRef).
```

### 3. RPC (Remote Procedure Calls)

```erlang
%% Call a remote service
{ok, Result} = macula:call(Client, <<"calculator.add">>, #{
    a => 5,
    b => 3
}).
%% Result: #{result => 8}
```

### 4. Advertise Services (Providers)

```erlang
%% Advertise a service handler
{ok, AdvRef} = macula:advertise(Client, <<"calculator.add">>, fun(Args) ->
    A = maps:get(a, Args),
    B = maps:get(b, Args),
    {ok, #{result => A + B}}
end).

%% Unadvertise when done
ok = macula:unadvertise(Client, AdvRef).
```

---

## Core Concepts

### Mesh Architecture

Macula creates a self-organizing mesh network where nodes communicate over HTTP/3 (QUIC). Each node can act as:

- **Peer** - Application client/server participating in the mesh
- **Gateway** - Relay node for NAT-traversed peers (optional)
- **Registry** - DHT participant storing service advertisements

### Multi-Tenancy via Realms

Realms provide logical isolation for different applications sharing the same physical mesh:

```erlang
%% App 1
{ok, Client1} = macula:connect_local(#{realm => <<"com.app1">>}).

%% App 2 (completely isolated from App 1)
{ok, Client2} = macula:connect_local(#{realm => <<"com.app2">>}).
```

### DHT-Based Service Discovery

Services are discovered via a Kademlia DHT with k=20 replication:

1. Provider advertises: `macula:advertise(Client, <<"my.service">>, Handler)`
2. DHT propagates to k=20 closest nodes
3. Consumer discovers: `macula:call(Client, <<"my.service">>, Args)`
4. Direct P2P connection established

### Platform Layer (v0.9.0+)

Distributed coordination primitives for workload applications:

```erlang
%% Leader election
{ok, LeaderNodeId} = macula:get_leader(Client).

%% CRDT state sharing
ok = macula:propose_crdt_update(Client, <<"counter">>, {increment, 1},
    #{crdt_type => pn_counter}).
```

---

## API Overview

### Main Module

**`macula`** - The public API (facade)

```erlang
%% Connection
macula:connect/2          %% Connect to remote gateway
macula:connect_local/1    %% Connect locally

%% Pub/Sub
macula:publish/3, /4      %% Publish event
macula:subscribe/3        %% Subscribe to topic
macula:unsubscribe/2      %% Unsubscribe

%% RPC
macula:call/3, /4         %% Call remote procedure
macula:advertise/3        %% Advertise service
macula:unadvertise/2      %% Remove advertisement

%% Platform Layer (v0.9.0+)
macula:get_leader/1       %% Get current leader
macula:propose_crdt_update/4  %% Update CRDT state
```

### Configuration Options

```erlang
Opts = #{
    realm => <<"com.example.app">>,        %% Required: Realm for isolation
    node_id => <<"my-node-001">>,          %% Optional: Custom node ID
    cert_file => "cert.pem",               %% Optional: TLS certificate
    key_file => "key.pem"                  %% Optional: TLS private key
}
```

---

## Development Setup

```bash
# Clone the repository
git clone https://github.com/macula-io/macula.git
cd macula

# Fetch dependencies
rebar3 get-deps

# Compile
rebar3 compile

# Run tests
rebar3 eunit

# Start a shell with Macula loaded
rebar3 shell
```

---

## Testing

```bash
# Run unit tests
rebar3 eunit

# Run dialyzer (type checking)
rebar3 dialyzer

# Generate documentation
rebar3 ex_doc
```

---

## Version History

| Version | Date | Key Features |
|---------|------|--------------|
| v0.10.x | Nov 2025 | Production hardening, memory management |
| v0.9.x | Nov 2025 | Platform Layer (Raft consensus, CRDTs) |
| v0.8.x | Nov 2025 | Direct P2P connections, DHT propagation |
| v0.7.x | Nov 2025 | Nomenclature refactoring |

See [CHANGELOG.md](CHANGELOG.md) for full version history.

---

## License

Macula is licensed under the Apache License 2.0. See [LICENSE](LICENSE) for details.

---

## Community & Support

- **Issues**: [GitHub Issues](https://github.com/macula-io/macula/issues)
- **Hex Package**: [hex.pm/packages/macula](https://hex.pm/packages/macula)
- **Source Code**: [github.com/macula-io/macula](https://github.com/macula-io/macula)

---

**Built for the BEAM community**
