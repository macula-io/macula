# Macula SDK Documentation

> **Version:** v1.1.0 (April 2026)

Macula SDK is an Erlang/OTP client library for connecting to a **federated relay mesh** over HTTP/3 (QUIC).

---

## Quick Navigation

| I want to... | Go to... |
|--------------|----------|
| Understand pub/sub messaging | [PubSub Guide](guides/PUBSUB_GUIDE.md) |
| Make RPC calls across the mesh | [RPC Guide](guides/RPC_GUIDE.md) |
| Connect nodes across firewalls | [Distribution Over Mesh](guides/DIST_OVER_MESH_GUIDE.md) |
| Form a LAN cluster | [Clustering Guide](guides/CLUSTERING_GUIDE.md) |
| Understand DID/UCAN security | [Authorization Guide](guides/AUTHORIZATION_GUIDE.md) |
| Work with resource identifiers | [MRI Guide](guides/MRI_GUIDE.md) |
| Look up terminology | [Glossary](GLOSSARY.md) |
| Contribute to Macula | [Development Guide](guides/DEVELOPMENT.md) |

---

## SDK Guides

| Guide | Description |
|-------|-------------|
| [PubSub](guides/PUBSUB_GUIDE.md) | Topic-based messaging through the relay mesh |
| [RPC](guides/RPC_GUIDE.md) | Request/response with DHT-based discovery |
| [Distribution Over Mesh](guides/DIST_OVER_MESH_GUIDE.md) | Erlang distribution tunneled through relays |
| [Clustering](guides/CLUSTERING_GUIDE.md) | LAN cluster formation via gossip |
| [Authorization](guides/AUTHORIZATION_GUIDE.md) | DID identities and UCAN capability tokens |
| [Protocol Gatekeeper](guides/PROTOCOL_GATEKEEPER_GUIDE.md) | Protocol-level security callbacks |
| [MRI](guides/MRI_GUIDE.md) | Macula Resource Identifiers |
| [Development](guides/DEVELOPMENT.md) | Building and testing |

## Reference

| Document | Description |
|----------|-------------|
| [Glossary](GLOSSARY.md) | Terminology reference |
| [Roadmap](ROADMAP.md) | SDK roadmap |

---

For relay server documentation (operator guides, DHT internals, peering, monitoring),
see [macula-relay](https://github.com/macula-io/macula-relay).
