# Macula Architecture Documentation Index

**Current Version**: v0.12.3
**Last Updated**: 2025-11-30

---

## Quick Links

- **Getting Started**: See the main README
- **API Reference**: See hex docs or `src/` module documentation

---

## Version-Specific Documentation

### v0.12.x (Current)

**Overview**: NATS-style Async RPC, NAT Traversal, Pull-based Discovery

📋 Core Documents:
- `v0.8.0-OVERVIEW.md` - Foundation architecture (Direct P2P with DHT propagation)
- `v0.8.0-CHANGELOG.md` - Changes from v0.7.x
- `ROADMAP.md` - Current roadmap

🏗️ Architecture:
- Direct P2P QUIC connections via `macula_peer_connector`
- DHT propagation to k=20 closest nodes
- RPC and PubSub via direct connections
- All node types run QUIC listeners
- NAT traversal with hole punching and relay fallback
- Async RPC with NATS-style request/reply

✅ Test Coverage: 70+ NAT tests, 22 async RPC tests

📦 Archived Development Docs: See `architecture/archive/` for development session notes and progress tracking.

### v0.7.x (Previous)

**Overview**: Gateway relay architecture with nomenclature refactoring

Key changes:
- Renamed `macula_connection` → `macula_peer` (facade)
- Kept `macula_connection` as transport layer
- Gateway refactoring into focused modules
- Memory management improvements

### v0.6.x and Earlier

**Overview**: Initial DHT implementation and mesh foundation

See git history for details.

---

## Topic Guides

### Core Concepts

- **DHT (Distributed Hash Table)**: Kademlia-based routing
  - Key concepts: XOR distance, k-buckets, replication factor k=20

- **Direct P2P Messaging**: Fire-and-forget QUIC connections
  - Module: `macula_peer_connector`

- **Service Discovery**: DHT-based registration and lookup
  - Advertisement, TTL, propagation

### Communication Patterns

#### RPC (Remote Procedure Call)
- **Module**: `macula_pubsub_dht.erl` (RPC functions)
- **Flow**: DHT lookup → Direct connection → Execute → Return result
- **Test Suite**: `test/integration/multi_hop_rpc_SUITE.erl`
- **Docs**: See module @doc comments

#### PubSub (Publish/Subscribe)
- **Module**: `macula_pubsub_dht.erl` (PubSub functions)
- **Flow**: Subscribe → Advertise in DHT → Publish → DHT lookup → Direct delivery
- **Test Suite**: `test/integration/multi_hop_pubsub_SUITE.erl`
- **Docs**: See module @doc comments

### Network & Infrastructure

#### NAT Traversal
- **Status**: Complete in v0.12.0
- **Approach**: Hole punching with relay fallback
- **Current**: 100% connectivity across NAT types (Full Cone, Restricted, Symmetric)

#### Multi-Tenancy
- **Mechanism**: Realm isolation
- **Security**: Per-realm routing tables
- **Docs**: See realm configuration in README

#### Deployment Patterns
- **Docker**: `docker/` directory - Bootstrap, Gateway, Edge nodes
- **Multi-node**: `docker-compose.multi-mode.yml`
- **NAT Test**: `docker/nat-test/` - 50-peer NAT simulation

---

## Reference Documentation

### API Documentation
- **High-level API**: `src/macula_peer.erl`
- **Transport**: `src/macula_connection.erl`
- **P2P Connector**: `src/macula_peer_connector.erl`
- **DHT**: `src/macula_routing_server.erl`
- **Gateway**: `src/macula_gateway.erl`

### Protocol Specifications
- **QUIC Transport**: Uses Microsoft MsQuic via `quicer`
- **Message Encoding**: MessagePack via `msgpack`
- **DHT Protocol**: Custom Kademlia implementation

### Configuration
- **Application**: `src/macula.app.src`
- **Runtime**: `sys.config` (not in repo - user-provided)
- **Docker**: Environment variables in docker-compose files

---

## Development Documentation

### Testing
- **Unit Tests**: `test/*_tests.erl` - EUnit tests
- **Integration Tests**: `test/integration/*_SUITE.erl` - Common Test suites
- **Running Tests**: `rebar3 eunit && rebar3 ct`

### Code Quality
- **Style Guide**: Idiomatic Erlang (see CLAUDE.md)
- **Test Coverage**: Tracked per module
- **Roadmap**: See `architecture/ROADMAP.md`

### Gateway Refactoring (Completed v0.7.x)
- 6 focused modules, supervision tree, comprehensive tests

### Memory Management (Completed v0.7.x)
- Bounded pools, TTL cleanup, no OOM crashes

---

## Historical Documentation

Historical planning documents are archived in `architecture/archive/`. These describe approaches superseded by the current direct P2P architecture.

---

## Comparison Documents

### vs. Other Technologies

See the comparisons documentation for comprehensive comparisons with:
- libp2p, Distributed Erlang, Akka Cluster, Kubernetes, WebRTC
- Business comparison with Kafka, RabbitMQ, NATS, MQTT

---

## Contributing

### How to Contribute Documentation
1. Follow the naming convention: `vX.Y.Z-DOCNAME.md` or `TOPIC-NAME.md`
2. Add entry to this INDEX.md
3. Keep docs focused and scannable
4. Include code examples where helpful
5. Link to related docs

### Documentation Standards
- **Markdown**: Use GitHub-flavored Markdown
- **Diagrams**: Use Mermaid or ASCII art
- **Code Examples**: Include working, tested examples
- **References**: Link to source files with line numbers where possible

---

## Quick Reference

### Key Files to Read First
1. Main README - Project overview
2. `v0.8.0-OVERVIEW.md` - Foundation architecture
3. `NATS_STYLE_ASYNC_RPC.md` - Async RPC design
4. `src/macula_peer.erl` - High-level API

### Common Tasks
- **Add a new feature**: See module @doc comments for API design patterns
- **Deploy to production**: See Docker files and compose configurations
- **Debug an issue**: See hex docs and integration test examples
- **Understand DHT**: See `macula_routing_server.erl` and `macula_routing_table.erl`
