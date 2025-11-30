# Macula Documentation

> **Audience:** All
> **Last Updated:** 2025-11-28
> **Version:** Macula v0.10.0+

Macula is a distributed platform for building decentralized applications using HTTP/3 (QUIC) transport with P2P mesh networking.

---

## Quick Navigation

| I want to... | Go to... |
|--------------|----------|
| Understand why Macula exists | [Business Overview](business/) |
| Understand the socio-economic vision | [Motivation](business/MOTIVATION.md) |
| Compare Macula to Kafka/RabbitMQ/NATS | [Technology Comparison](business/COMPARISON.md) |
| Get started quickly | [Quick Start](user/QUICK_START.md) |
| Build my first app | [Hello World Tutorial](user/HELLO_WORLD.md) |
| Deploy to production | [Operator Guide](operator/) |
| Understand the architecture | [Architecture Overview](../ARCHITECTURE.md) |
| Use TWEANN or event sourcing | [Ecosystem Libraries](ecosystem/) |
| Contribute to Macula | [Developer Guide](developer/) |
| Look up terminology | [Glossary](GLOSSARY.md) |

---

## Documentation by Audience

### For Business Leaders

Understand the strategic value of decentralized systems:

- **[Platform Overview](business/OVERVIEW.md)** - What Macula enables
- **[Why Decentralized?](business/WHY_DECENTRALIZED.md)** - The case for decentralization
- **[Why BEAM?](business/WHY_BEAM.md)** - Technology foundation
- **[Use Cases](business/USE_CASES.md)** - Business applications

### For Users (Application Developers)

Build applications on the Macula mesh:

- **[Quick Start](user/QUICK_START.md)** - 15-minute mesh setup
- **[Hello World](user/HELLO_WORLD.md)** - Your first distributed app
- **[RPC Guide](developer/RPC_GUIDE.md)** - Remote procedure calls
- **[PubSub Guide](developer/PUBSUB_GUIDE.md)** - Publish/subscribe messaging

### For Operators

Run Macula in production:

- **[Performance Guide](operator/PERFORMANCE_GUIDE.md)** - Tuning and optimization
- **[Monitoring Guide](operator/MONITORING_GUIDE.md)** - Metrics, logs, and alerting
- **[Troubleshooting Guide](operator/TROUBLESHOOTING_GUIDE.md)** - Common issues and solutions
- **[QUIC/TLS Setup](operator/QUIC_TLS_GATEWAY_SETUP.md)** - Gateway TLS configuration
- **[Memory Management](../architecture/memory_management/README.md)** - Production-ready leak prevention

### For Developers (Contributors)

Contribute to Macula core:

- **[Development Guide](developer/DEVELOPMENT.md)** - Local development setup
- **[RPC Guide](developer/RPC_GUIDE.md)** - Understanding RPC internals
- **[PubSub Guide](developer/PUBSUB_GUIDE.md)** - Understanding PubSub internals
- **[Project Structure](../architecture/MACULA_PROJECT_STRUCTURE.md)** - Module organization

---

## Guides (Deep-Dives)

### NAT Traversal
- **[NAT Types Explained](guides/NAT_TYPES_EXPLAINED.md)** - Understanding NAT classification
- **[NAT Developer Guide](guides/NAT_TRAVERSAL_DEVELOPER_GUIDE.md)** - API usage and code examples
- **[NAT Configuration](guides/NAT_CONFIGURATION.md)** - Configuration reference
- **[NAT Architecture](guides/NAT_ARCHITECTURE.md)** - Visual diagrams and flows

### DHT & Networking
- **[DHT Guide](guides/DHT_GUIDE.md)** - Kademlia DHT architecture

---

## Comparisons

- **[vs Distributed Erlang](comparisons/VS_DISTRIBUTED_ERLANG.md)** - QUIC benefits over inet_dist
- **[Comparisons Overview](comparisons/README.md)** - Technology comparison matrix

---

## Ecosystem Libraries

Related libraries that extend Macula's capabilities:

- **[Ecosystem Overview](ecosystem/)** - All related libraries
- **[Macula TWEANN](ecosystem/TWEANN.md)** - Evolutionary neural networks for distributed AI
- **[Reckon Architecture](ecosystem/RECKON.md)** - Event sourcing patterns for BEAM applications

---

## Architecture

Core architecture documentation lives in [`../architecture/`](../architecture/):

- **[Roadmap](../architecture/ROADMAP.md)** - Implementation roadmap
- **[C4 Diagrams](../architecture/C4_DIAGRAMS.md)** - Visual architecture overview
- **[Multi-Tenancy](../architecture/MULTI_TENANCY.md)** - Realm isolation mechanisms
- **[Module Dependencies](../architecture/MODULE_DEPENDENCIES.md)** - Module relationship map
- **[Memory Management](../architecture/memory_management/README.md)** - Production hardening

---

## Project Links

- **[Main README](../README.md)** - Project overview
- **[Getting Started](../GETTING_STARTED.md)** - Installation guide
- **[Changelog](../CHANGELOG.md)** - Version history
- **[Examples](../examples/)** - Demo applications
- **[GitHub Issues](https://github.com/macula-io/macula/issues)** - Bug reports and features

---

## Archive

Historical and superseded documentation:

- **[Archive Index](archive/)** - Code reviews, migration docs, historical analysis
- **[Sessions Archive](archive/sessions/)** - Development session summaries

---

**Built for the BEAM community**
