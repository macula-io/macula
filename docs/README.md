# Macula SDK Documentation

Macula SDK is an Erlang/OTP client library for connecting to a **federated relay mesh** over HTTP/3 (QUIC). See [CHANGELOG.md](../CHANGELOG.md) for the current version and release history.

---

## Quick Navigation

| I want to... | Go to... |
|--------------|----------|
| Connect to the mesh | [Connecting Guide](guides/shared/CONNECTING_GUIDE.md) |
| Understand pub/sub messaging | [PubSub Guide](guides/pubsub/PUBSUB_GUIDE.md) |
| Migrate from V1 (pre-3.11.0) | [V1 → V2 Migration](migrations/V1_TO_V2_PUBSUB.md) |
| Make RPC calls across the mesh | [RPC Guide](guides/rpc/RPC_GUIDE.md) |
| Share content-addressed blobs | [Content Guide](guides/content/CONTENT_GUIDE.md) |
| Store your own signed DHT facts | [Records Guide](guides/shared/RECORDS_GUIDE.md) |
| Stream more than one request/response | [Streaming Guide](guides/streaming/STREAMING_GUIDE.md) |
| Connect nodes across firewalls | [Distribution Over Mesh](guides/DIST_OVER_MESH_GUIDE.md) |
| Form a LAN cluster | [Clustering Guide](guides/CLUSTERING_GUIDE.md) |
| Understand DID/UCAN security | [Authorization Guide](guides/shared/AUTHORIZATION_GUIDE.md) |
| Work with resource identifiers | [MRI Guide](guides/shared/MRI_GUIDE.md) |
| Look up terminology | [Glossary](GLOSSARY.md) |
| Contribute to Macula | [Development Guide](guides/DEVELOPMENT.md) |

---

## SDK Guides

| Guide | Description |
|-------|-------------|
| [Connecting](guides/shared/CONNECTING_GUIDE.md) | Pool model, seeds, identity, replication, lifecycle |
| [PubSub](guides/pubsub/PUBSUB_GUIDE.md) | Topic-based messaging through the relay mesh |
| [Topic Naming](guides/shared/TOPIC_NAMING_GUIDE.md) | Canonical 5-segment topic shape |
| [RPC](guides/rpc/RPC_GUIDE.md) | Request/response; direct-dial via `call_station/6,7` or the supervised `start_link_direct`/`advertise_direct` |
| [Content](guides/content/CONTENT_GUIDE.md) | Content-addressed blobs (MCID), single-block or chunked, plus push/upload at a known recipient; direct-dial fetch/seed |
| [Records](guides/shared/RECORDS_GUIDE.md) | Signed, TTL'd facts in the DHT — your own record types |
| [Streaming](guides/streaming/STREAMING_GUIDE.md) | Streaming RPC (server / client / bidi); direct-dial via `call_stream_station/6` |
| [Distribution Over Mesh](guides/DIST_OVER_MESH_GUIDE.md) | Erlang distribution tunneled through relays |
| [Clustering](guides/CLUSTERING_GUIDE.md) | LAN cluster formation via gossip |
| [Authorization](guides/shared/AUTHORIZATION_GUIDE.md) | DID identities and UCAN capability tokens |
| [MRI](guides/shared/MRI_GUIDE.md) | Macula Resource Identifiers |
| [Development](guides/DEVELOPMENT.md) | Building and testing |

Each primitive pair (RPC, PubSub, Content, Streaming) also has a **Protocol**
doc — the raw wire primitives underneath its Guide, for anyone building
something the supervised wrapper doesn't fit: custom retry logic,
observability, an SDK for another language.

| Protocol | Description |
|----------|-------------|
| [RPC Protocol](guides/rpc/RPC_PROTOCOL.md) | Raw `advertise`/`call`, direct-dial resolution internals, BOLT#4 error codes |
| [PubSub Protocol](guides/pubsub/PUBSUB_PROTOCOL.md) | Raw `subscribe`/`publish`, hand-rolled callback pattern |
| [Content Protocol](guides/content/CONTENT_PROTOCOL.md) | Raw `put_content`/`get_content`, MCID wire format, discovery, pause/resume/multi-stream |
| [Streaming Protocol](guides/streaming/STREAMING_PROTOCOL.md) | Raw `call_stream`/`advertise_stream`, local in-process streams |

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
