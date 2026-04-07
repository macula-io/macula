# Macula Documentation

> **Version:** v0.43.0+ (April 2026)

Macula is an Erlang/OTP library for building distributed applications over a **federated relay mesh** using HTTP/3 (QUIC) transport.

---

## Quick Navigation

| I want to... | Go to... |
|--------------|----------|
| Get started quickly | [Getting Started](getting-started.md) |
| Understand pub/sub messaging | [PubSub Guide](guides/PUBSUB_GUIDE.md) |
| Make RPC calls across the mesh | [RPC Guide](guides/RPC_GUIDE.md) |
| Connect nodes across firewalls | [Distribution Over Mesh](guides/DIST_OVER_MESH_GUIDE.md) |

| Form a LAN cluster | [Clustering Guide](guides/CLUSTERING_GUIDE.md) |
| Configure TLS | [TLS Guide](operator/TLS_GUIDE.md) |
| Deploy to production | [Operator Guides](operator/) |
| Look up terminology | [Glossary](GLOSSARY.md) |
| Contribute to Macula | [Development Guide](guides/DEVELOPMENT.md) |

---

## Guides

Feature guides for developers building on Macula:

| Guide | Description |
|-------|-------------|
| [PubSub](guides/PUBSUB_GUIDE.md) | Publish/subscribe messaging through the relay mesh |
| [RPC](guides/RPC_GUIDE.md) | Remote procedure calls with DHT-based discovery |
| [Distribution Over Mesh](guides/DIST_OVER_MESH_GUIDE.md) | Erlang distribution tunneled through relays |
| [Clustering](guides/CLUSTERING_GUIDE.md) | LAN cluster formation via gossip + static nodes |
| [Content Transfer](guides/CONTENT_TRANSFER_GUIDE.md) | Content-addressed artifact distribution |
| [DHT](guides/DHT_GUIDE.md) | Kademlia DHT between relay nodes |
| [Authorization](guides/AUTHORIZATION_GUIDE.md) | DID identities and UCAN capability tokens |
| [Protocol Gatekeeper](guides/PROTOCOL_GATEKEEPER_GUIDE.md) | Protocol-level security callbacks |
| [MRI](guides/MRI_GUIDE.md) | Macula Resource Identifiers |

## Operator Guides

| Guide | Description |
|-------|-------------|
| [TLS Setup](operator/TLS_GUIDE.md) | Production TLS with Let's Encrypt and QUIC |
| [Monitoring](operator/MONITORING_GUIDE.md) | Metrics, logs, and alerting |
| [Performance](operator/PERFORMANCE_GUIDE.md) | Tuning and optimization |
| [Troubleshooting](operator/TROUBLESHOOTING_GUIDE.md) | Common issues and solutions |
| [mDNS Setup](operator/MDNS_SETUP.md) | Optional LAN discovery |

## Architecture

| Document | Description |
|----------|-------------|
| [Supervision Tree](guides/FULL_SUPERVISION_TREE.md) | Complete OTP supervision hierarchy |
| [Roadmap](ROADMAP.md) | Implementation roadmap |
| [C4 Diagrams](C4_DIAGRAMS.md) | Visual architecture overview |

---

**Built for the BEAM community**
