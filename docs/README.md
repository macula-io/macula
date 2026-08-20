# Macula SDK Documentation

Macula SDK is an Erlang/OTP client library for connecting to a **federated relay mesh** over HTTP/3 (QUIC). See [CHANGELOG.md](../CHANGELOG.md) for the current version and release history.

---

## Quick Navigation

| I want to... | Go to... |
|--------------|----------|
| Connect to the mesh | [Connecting Guide](guides/CONNECTING_GUIDE.md) |
| Understand pub/sub messaging | [PubSub Guide](guides/PUBSUB_GUIDE.md) |
| Migrate from V1 (pre-3.11.0) | [V1 → V2 Migration](migrations/V1_TO_V2_PUBSUB.md) |
| Make RPC calls across the mesh | [RPC Guide](guides/RPC_GUIDE.md) |
| Share content-addressed blobs | [Content Guide](guides/CONTENT_GUIDE.md) |
| Store your own signed DHT facts | [Records Guide](guides/RECORDS_GUIDE.md) |
| Stream more than one request/response | [Streaming Guide](guides/STREAMING_GUIDE.md) |
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
| [Connecting](guides/CONNECTING_GUIDE.md) | Pool model, seeds, identity, replication, lifecycle |
| [PubSub](guides/PUBSUB_GUIDE.md) | Topic-based messaging through the relay mesh |
| [Topic Naming](guides/TOPIC_NAMING_GUIDE.md) | Canonical 5-segment topic shape |
| [RPC](guides/RPC_GUIDE.md) | Request/response; direct-dial via `call_station/6,7` or the supervised `start_link_direct`/`advertise_direct` |
| [Content](guides/CONTENT_GUIDE.md) | Content-addressed blobs (MCID), single-block or chunked; direct-dial fetch/seed |
| [Records](guides/RECORDS_GUIDE.md) | Signed, TTL'd facts in the DHT — your own record types |
| [Streaming](guides/STREAMING_GUIDE.md) | Streaming RPC (server / client / bidi); direct-dial via `call_stream_station/6` |
| [Distribution Over Mesh](guides/DIST_OVER_MESH_GUIDE.md) | Erlang distribution tunneled through relays |
| [Clustering](guides/CLUSTERING_GUIDE.md) | LAN cluster formation via gossip |
| [Authorization](guides/AUTHORIZATION_GUIDE.md) | DID identities and UCAN capability tokens |
| [MRI](guides/MRI_GUIDE.md) | Macula Resource Identifiers |
| [Development](guides/DEVELOPMENT.md) | Building and testing |

## Migrations

| Document | Description |
|----------|-------------|
| [V1 → V2 Pub/Sub](migrations/V1_TO_V2_PUBSUB.md) | Breaking facade changes in 3.11.0 |

## Reference

| Document | Description |
|----------|-------------|
| [Glossary](GLOSSARY.md) | Terminology reference |
| [Roadmap](ROADMAP.md) | SDK roadmap |

---

For relay server documentation (operator guides, DHT internals, peering, monitoring),
see macula-station.
