<div align="center">
  <img src="artwork/macula-alt-logo.svg" alt="Macula Logo" width="500"/>

  <h1>Macula HTTP/3 Mesh</h1>
  <p><em>Self-organizing distributed mesh for decentralized applications</em></p>

  <p>
    <a href="LICENSE"><img src="https://img.shields.io/badge/License-Apache%202.0-blue.svg" alt="License"/></a>
    <a href="https://www.erlang.org"><img src="https://img.shields.io/badge/Erlang%2FOTP-26+-brightgreen" alt="Erlang/OTP"/></a>
    <a href="https://hex.pm/packages/macula"><img src="https://img.shields.io/hexpm/v/macula.svg" alt="Hex.pm"/></a>
  </p>
</div>

---

<div align="center">
  <img src="assets/macula-architecture-overview.svg" alt="Macula Architecture Overview" width="800"/>
</div>

<div align="center">
  <p><strong>BEAM-Native • HTTP/3 (QUIC) • Kademlia DHT • Direct P2P • Multi-Tenant • 50% Faster (v0.8.0)</strong></p>
</div>

---

## Documentation

- 🚀 **[Getting Started](GETTING_STARTED.md)** - Installation, quick start, code examples
- 🏗️ **[Architecture Guide](ARCHITECTURE.md)** - Visual guide with C4 diagrams, deployment topologies
- 📄 **[Changelog](CHANGELOG.md)** - Version history and migration guides
- 🧪 **[Development Guide](docs/DEVELOPMENT.md)** - Contributing and testing
- 🌐 **[DHT Guide](docs/guides/DHT_GUIDE.md)** - Kademlia DHT architecture

---

## Latest Release: v0.10.1 (2025-11-26)

**Performance Optimizations & Documentation:**
- ✅ Subscriber caching with TTL-based expiration (50-200x publish speedup)
- ✅ Direct routing table for known endpoints (10-50x latency reduction)
- ✅ Rate-limited DHT discovery (prevents discovery storms)
- ✅ Comprehensive performance documentation with ASCII diagrams

**Platform Features (v0.9.0+):**
- ✅ Ra/Raft consensus for distributed coordination
- ✅ LWW-Register CRDT for eventually-consistent state
- ✅ Leader election and workload registration APIs

---

## Community

- **Hex**: [hex.pm/packages/macula](https://hex.pm/packages/macula)
- **GitHub**: [github.com/macula-io/macula](https://github.com/macula-io/macula)
- **Issues**: [github.com/macula-io/macula/issues](https://github.com/macula-io/macula/issues)

---

**Built with ❤️ for the BEAM community**
