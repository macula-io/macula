# Macula Documentation

**Welcome to the Macula HTTP/3 Mesh documentation!**

Macula is a distributed platform for building decentralized applications using HTTP/3 (QUIC) transport with P2P mesh networking.

---

## 🚀 Getting Started

- **[Quick Start](../HELLO_WORLD.md)** - Build a distributed chat app in 30 minutes
- **[Architecture Overview](../architecture/macula_http3_mesh_root.md)** - Complete documentation hub
- **[Project Structure](../architecture/MACULA_PROJECT_STRUCTURE.md)** - Module organization

---

## 📚 Core Documentation

### Architecture & Design
- **[Kademlia DHT Architecture](KADEMLIA_DHT_ARCHITECTURE.md)** - Distributed hash table implementation
- **[QUIC/TLS Setup Guide](QUIC_TLS_GATEWAY_SETUP.md)** - Gateway TLS configuration
- **[Memory Management](../architecture/memory_management/README.md)** - Production-ready leak prevention
- **[C4 Diagrams](../architecture/macula_http3_mesh_c4_diagrams.md)** - Visual architecture overview

### Planned Improvements
- **[Peer-Connection Separation Plan](PEER_CONNECTION_SEPARATION_PLAN.md)** - v0.7.0 TDD refactoring (4.5 weeks)
- **[Nomenclature Proposal](NOMENCLATURE_PROPOSAL_CONNECTION_TO_PEER.md)** - connection → peer rename
- **[Peer vs Connection Analysis](PEER_VS_CONNECTION_ANALYSIS.md)** - Responsibility breakdown

---

## 🛠️ Development

### Development Guides
- **[Testing Guide](development/TESTING.md)** - Unit, integration, and E2E testing
- **[Docker Build Reference](development/DOCKER_BUILD_REFERENCE.md)** - Build best practices
- **[Hex Publication Guide](development/HEX_PUBLICATION_GUIDE.md)** - Publishing to Hex.pm
- **[mDNS Setup](development/MDNS_SETUP.md)** - Local service discovery configuration

### Planning Documents
- **[Implementation Plan](planning/IMPLEMENTATION_PLAN.md)** - Original 20-week roadmap
- **[Multi-Node Testing Plan](planning/MULTI_NODE_TESTING_PLAN.md)** - Testing strategy

---

## 📖 User Guides

- **[Quick Start](../architecture/macula_http3_mesh_quick_start.md)** - 15-minute mesh setup
- **[Hello World Tutorial](../architecture/macula_http3_mesh_hello_world.md)** - Build your first app
- **[RPC Guide](../architecture/macula_http3_mesh_rpc_guide.md)** - Remote procedure calls
- **[API Reference](../architecture/macula_http3_mesh_api_reference.md)** - Complete API docs

---

## 🔍 Reference

### Technical Deep Dives
- **[DHT Routed RPC](../architecture/dht_routed_rpc.md)** - Multi-hop Kademlia routing
- **[Pub/Sub Optimization](../architecture/pubsub_optimization_recommendations.md)** - 10,000+ msg/sec
- **[Isolation Mechanisms](../architecture/macula_http3_mesh_isolation_mechanisms.md)** - Multi-tenancy
- **[NAT Traversal](../architecture/macula_http3_mesh_nat_traversal.md)** - Firewall-friendly transport

### Comparisons
- **[vs libp2p](../architecture/macula_http3_mesh_vs_libp2p.md)** - Why Macula?
- **[vs WAMP](../architecture/macula_http3_mesh_vs_wamp.md)** - HTTP/3 mesh advantages
- **[vs Distributed Erlang](../architecture/macula_http3_mesh_vs_distributed_erlang.md)** - QUIC benefits

---

## 📦 Release Information

- **[Changelog](../CHANGELOG.md)** - Version history and breaking changes
- **[v0.6.0 Release Summary](V0.6.0_RELEASE_SUMMARY.md)** - Latest release details

---

## 📂 Archive & History

- **[Archive](archive/)** - Historical analysis and completed work (~40 documents)
- **[Sessions](sessions/)** - Development session summaries

---

## 🤝 Contributing

- **[Contributing Guide](../architecture/macula_http3_mesh_contributing.md)** - How to contribute
- **[Documentation Status](../architecture/DOCUMENTATION_STATUS.md)** - What's complete

---

## 📞 Community & Support

- **Issues**: [GitHub Issues](https://github.com/macula-io/macula/issues)
- **Examples**: [Examples Directory](../examples/)
- **License**: [Apache 2.0](../LICENSE)

---

**Built with ❤️ for the BEAM community**
