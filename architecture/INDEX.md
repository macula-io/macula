# Macula Architecture Documentation Index

**Current Version**: v0.8.0
**Last Updated**: 2025-11-17

---

## Quick Links

- **Getting Started**: See `../README.md`
- **API Reference**: See hex docs or `src/` module documentation
- **Tutorial**: See `TUTORIAL.md` (coming soon)
- **FAQ**: See `FAQ.md`
- **Troubleshooting**: See `TROUBLESHOOTING.md`

---

## Version-Specific Documentation

### v0.8.0 (Current - Released 2025-11-17)

**Overview**: Direct P2P with DHT propagation

📋 Core Documents:
- [`v0.8.0-OVERVIEW.md`](v0.8.0-OVERVIEW.md) - Release overview and key achievements
- [`v0.8.0-CHANGELOG.md`](v0.8.0-CHANGELOG.md) - Detailed changes from v0.7.x
- [`v0.8.0-ROADMAP.md`](v0.8.0-ROADMAP.md) - Future plans (v0.9.0 and beyond)
- [`V0.8.0_COMPLETION_STATUS.md`](V0.8.0_COMPLETION_STATUS.md) - Final implementation status
- [`V0.8.0_IMPLEMENTATION_ROADMAP.md`](V0.8.0_IMPLEMENTATION_ROADMAP.md) - Original roadmap

🏗️ Architecture:
- Direct P2P QUIC connections via `macula_peer_connector`
- DHT propagation to k=20 closest nodes
- RPC and PubSub via direct connections
- All node types run QUIC listeners

✅ Test Coverage:
- 21/21 integration tests passing (100%)
- 11 RPC tests + 10 PubSub tests

📦 Archived Development Docs:
- [`archive/v0.8.0-development/`](archive/v0.8.0-development/) - Development session notes, progress tracking

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
  - See `ALGORITHMS-KADEMLIA.md` (coming soon)
  - Key concepts: XOR distance, k-buckets, replication factor k=20

- **Direct P2P Messaging**: Fire-and-forget QUIC connections
  - See `TECHNIQUES-DIRECT-P2P.md` (coming soon)
  - Module: `macula_peer_connector`

- **Service Discovery**: DHT-based registration and lookup
  - See `ALGORITHMS-SERVICE-DISCOVERY.md` (coming soon)
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
- **Status**: Planned for v0.9.0
- **Approach**: Opportunistic hole punching
- **Design**: [`NAT_TRAVERSAL_ROADMAP.md`](NAT_TRAVERSAL_ROADMAP.md)
- **Current**: 100% connectivity via relay fallback

#### Multi-Tenancy
- **Mechanism**: Realm isolation
- **Security**: Per-realm routing tables
- **Docs**: See realm configuration in README

#### Deployment Patterns
- **Docker**: `docker/` directory - Bootstrap, Gateway, Edge nodes
- **Multi-node**: `docker-compose.multi-mode.yml`
- **Test Topology**: 1 bootstrap + 1 gateway + 3 edge nodes

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
- **TODO Tracking**: See `TODO.md`

### Gateway Refactoring (Completed)
- **Plan**: [`gateway_refactoring_plan.md`](gateway_refactoring_plan.md)
- **Status**: Completed in v0.7.x
- **Result**: 6 focused modules, supervision tree, comprehensive tests

### Memory Management (Completed)
- **Overview**: [`memory_management/README.md`](memory_management/README.md)
- **Docs**: [`memory_management/`](memory_management/) directory
- **Status**: Completed in v0.7.x
- **Result**: Bounded pools, TTL cleanup, no OOM crashes

---

## Historical Documentation

### Architecture Evolution
- [`ARCHITECTURE_EVOLUTION.md`](ARCHITECTURE_EVOLUTION.md) - How we got here
- [`DOCUMENTATION_STATUS.md`](DOCUMENTATION_STATUS.md) - Doc tracking

### Old Planning (Pre-v0.8.0)
- [`dht_routed_rpc.md`](dht_routed_rpc.md) - Original DHT RPC plan (superseded)
- [`dht_routed_pubsub.md`](dht_routed_pubsub.md) - Original DHT PubSub plan (superseded)
- [`god_module_refactoring_plan.md`](god_module_refactoring_plan.md) - Old refactoring (superseded by v0.7.0 nomenclature)

**Note**: Many of these docs describe approaches that were superseded by v0.8.0's direct P2P architecture. They're kept for historical reference.

---

## Comparison Documents

### vs. Other Technologies
- [`macula_http3_mesh_vs_distributed_erlang.md`](macula_http3_mesh_vs_distributed_erlang.md)
- [`macula_http3_mesh_vs_libp2p.md`](macula_http3_mesh_vs_libp2p.md)
- [`macula_http3_mesh_vs_wamp.md`](macula_http3_mesh_vs_wamp.md)

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

## TODO

Future documentation to create:
- [ ] `TUTORIAL.md` - Complete tutorial with working examples
- [ ] `FAQ.md` - Frequently asked questions
- [ ] `TROUBLESHOOTING.md` - Common issues and solutions
- [ ] `ALGORITHMS-KADEMLIA.md` - DHT algorithm details
- [ ] `ALGORITHMS-SERVICE-DISCOVERY.md` - Service discovery flow
- [ ] `TECHNIQUES-DIRECT-P2P.md` - P2P connection patterns
- [ ] `v0.8.0-ARCHITECTURE.md` - Detailed architecture diagrams
- [ ] `v0.8.0-DECISIONS.md` - Architecture Decision Records (ADRs)

---

## Quick Reference

### Key Files to Read First
1. [`../README.md`](../README.md) - Project overview
2. [`v0.8.0-OVERVIEW.md`](v0.8.0-OVERVIEW.md) - Current version overview
3. [`v0.8.0-CHANGELOG.md`](v0.8.0-CHANGELOG.md) - What changed
4. [`src/macula_peer.erl`](../src/macula_peer.erl) - High-level API

### Common Tasks
- **Add a new feature**: See module @doc comments for API design patterns
- **Deploy to production**: See Docker files and compose configurations
- **Debug an issue**: See hex docs and integration test examples
- **Understand DHT**: See `macula_routing_server.erl` and `macula_routing_table.erl`

---

**Navigation**: [Back to Main README](../README.md) | [v0.8.0 Overview](v0.8.0-OVERVIEW.md) | [Changelog](v0.8.0-CHANGELOG.md) | [Roadmap](v0.8.0-ROADMAP.md)
