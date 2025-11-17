<div align="center">
  <img src="artwork/macula-alt-logo.svg" alt="Macula Logo" width="500"/>

  <h1>Macula HTTP/3 Mesh</h1>
  <p><em>A distributed platform for decentralized applications</em></p>
</div>

<p align="center">
  <a href="LICENSE"><img src="https://img.shields.io/badge/License-Apache%202.0-blue.svg" alt="License"/></a>
  <a href="https://www.erlang.org"><img src="https://img.shields.io/badge/Erlang%2FOTP-26+-brightgreen" alt="Erlang/OTP"/></a>
</p>

---

## Table of Contents

- 🏗️ [Architecture Overview](ARCHITECTURE.md) - **Visual guide with diagrams** (C4, supervision trees, deployment topologies)
- 🚀 [Quick Start](#quick-start) - Get started in minutes
- 💡 [What's New in v0.8.0](#whats-new-in-v080) - Latest features
- 📚 [Core Concepts](#core-concepts) - Understanding the mesh
- 🔧 [API Overview](#api-overview) - Using Macula in your application
- 📄 [Changelog](CHANGELOG.md) - Version history and migration guides
- 🐛 [Issues](https://github.com/macula-io/macula/issues) - Report bugs and request features

---

## What is Macula?

Macula is infrastructure for building **decentralized applications and services** that operate autonomously at the edge, without dependency on centralized cloud infrastructure.

**Key Features:**

✅ **BEAM-native** (Erlang/Elixir OTP supervision and fault tolerance)
✅ **HTTP/3 (QUIC)** transport (modern, encrypted, NAT-friendly)
✅ **Edge-first design** (works through firewalls and NAT)
✅ **Built-in pub/sub & RPC** (no external message broker needed)
✅ **Multi-tenancy** (realm isolation for SaaS and shared infrastructure)
✅ **Self-organizing mesh** (DHT-based service discovery, O(log N) routing)
✅ **Production-ready patterns** (OTP behaviors, comprehensive testing, memory management)

---

## Architecture at a Glance

**System Context** - How your application uses Macula:

```
┌──────────────┐
│     Your     │
│ Application  │
└──────┬───────┘
       │ macula_peer API
       ▼
┌──────────────┐     QUIC/HTTP3      ┌──────────────┐
│ Macula Peer  │◄───────────────────►│   Gateway    │
│ (Local Node) │    Or Direct P2P    │ (Relay Node) │
└──────┬───────┘                     └──────┬───────┘
       │                                    │
       └────────────► DHT ◄─────────────────┘
                 (Service Discovery)
```

**Message Flow** (v0.8.0 Direct P2P):

```
Client ──1. Query DHT──► DHT (Find Service)
Client ◄─2. Endpoint──── DHT Returns "192.168.1.50:9443"
Client ──3. Direct────► Provider (1-hop, 50ms)
Client ◄─4. Response─── Provider (50% faster than relay!)
```

**📊 [See Full Architecture Guide](ARCHITECTURE.md)** with:
- C4 diagrams (context, container views)
- Deployment topologies (edge, microservices, hybrid)
- Supervision trees (OTP fault tolerance)
- DHT architecture (Kademlia routing)
- Performance characteristics
- When to use Macula

---

## Installation

**Elixir (mix.exs):**

```elixir
def deps do
  [
    {:macula, "~> 0.8"}
  ]
end
```

**Erlang (rebar.config):**

```erlang
{deps, [
    {macula, "0.8.1"}
]}.
```

**Latest Release**: v0.8.1 (2025-11-17) - Documentation improvements (v0.8.0: Direct P2P with DHT propagation)

---

##  What's New in v0.8.0

**Major Features:**
- ✅ Direct P2P QUIC connections via `macula_peer_connector`
- ✅ DHT propagation to k=20 closest nodes (Kademlia-based)
- ✅ RPC via direct P2P (50% latency improvement)
- ✅ PubSub via direct P2P (50% latency improvement)
- ✅ 21/21 integration tests passing (100% coverage)

**Performance:**
- 1-hop direct connections vs 2+ hop relay routing
- Reduced gateway load
- Better scalability for large meshes

**Breaking Changes:** None - fully backward compatible with v0.7.x

---

## Quick Start

### 1. Connect to a Gateway

```erlang
%% Start a peer connection
{ok, Peer} = macula_peer:start_link(<<"https://gateway.example.com:9443">>, #{
    realm => <<"com.example.app">>
}).
```

### 2. Publish/Subscribe

```erlang
%% Subscribe to events
ok = macula_peer:subscribe(Peer, <<"sensor.temperature">>, self()).

%% Publish an event
ok = macula_peer:publish(Peer, <<"sensor.temperature">>, #{
    device_id => <<"sensor-001">>,
    celsius => 21.5,
    timestamp => erlang:system_time(millisecond)
}).

%% Receive events
receive
    {macula_event, <<"sensor.temperature">>, Payload} ->
        io:format("Temperature: ~p°C~n", [maps:get(celsius, Payload)])
end.
```

### 3. RPC (Remote Procedure Calls)

```erlang
%% Call a remote service
{ok, Result} = macula_peer:call(Peer, <<"calculator.add">>, #{
    a => 5,
    b => 3
}).
%% Result: #{result => 8}
```

### 4. Advertise Services (Providers)

```erlang
%% Advertise a service handler
ok = macula_peer:advertise(Peer, <<"calculator.add">>, fun(Args) ->
    A = maps:get(a, Args),
    B = maps:get(b, Args),
    #{result => A + B}
end, #{ttl => 300}).
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
{ok, Peer1} = macula_peer:start_link(GatewayUrl, #{realm => <<"com.app1">>}).

%% App 2 (completely isolated from App 1)
{ok, Peer2} = macula_peer:start_link(GatewayUrl, #{realm => <<"com.app2">>}).
```

### DHT-Based Service Discovery
Services are discovered via a Kademlia DHT with k=20 replication:
1. Provider advertises: `advertise(<<"my.service">>, Handler)`
2. DHT propagates to k=20 closest nodes
3. Consumer discovers: `call(<<"my.service">>, Args)`
4. Direct P2P connection established (v0.8.0+)

### Direct P2P Connections (v0.8.0)
Instead of relaying through gateways, v0.8.0 establishes direct QUIC connections:
- Discovered endpoint → Direct connection
- 50% latency reduction (1-hop vs 2+ hops)
- Reduced gateway load

---

## API Overview

### Main Modules

**`macula_peer`** - High-level mesh participant API
- `start_link/2` - Connect to gateway
- `publish/3`, `subscribe/3` - Pub/sub messaging
- `call/3`, `advertise/4` - RPC and service registration

**`macula_gateway`** - Gateway/relay node
- Embedded or standalone gateway deployment
- Client lifecycle management
- Message routing and forwarding

**`macula_peer_connector`** - Direct P2P connections (v0.8.0)
- Establishes outbound QUIC connections
- Fire-and-forget message delivery

### Configuration Options

```erlang
Opts = #{
    realm => <<"com.example.app">>,        %% Required: Realm for isolation
    node_id => <<"my-node-001">>,         %% Optional: Custom node ID
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

# Run integration tests (requires Docker)
rebar3 ct --suite=test/integration/multi_hop_rpc_SUITE
rebar3 ct --suite=test/integration/multi_hop_pubsub_SUITE
```

---

## License

Macula is licensed under the Apache License 2.0. See [LICENSE](LICENSE) for details.

---

## Community & Support

- **Issues**: [GitHub Issues](https://github.com/macula-io/macula/issues)
- **Hex Package**: [hex.pm/packages/macula](https://hex.pm/packages/macula)
- **Source Code**: [github.com/macula-io/macula](https://github.com/macula-io/macula)

---

**Built with ❤️ for the BEAM community**
