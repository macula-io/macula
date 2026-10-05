# Macula SDK Documentation

Macula SDK is an Erlang/OTP client library for connecting to a **federated relay mesh** over HTTP/3 (QUIC). See [CHANGELOG.md](../CHANGELOG.md) for the current version and release history.

---

## Quick Navigation

| I want to... | Go to... |
|--------------|----------|
| Connect to the mesh | [Connecting Guide](guides/shared/CONNECTING_GUIDE.md) |
| Understand pub/sub messaging | [PubSub Guide](guides/pubsub/PUBSUB_GUIDE.md) |
| Make RPC calls across the mesh | [RPC Guide](guides/rpc/RPC_GUIDE.md) |
| Share content-addressed blobs | [Content Guide](guides/content/CONTENT_GUIDE.md) |
| Store your own signed DHT facts | [Records Guide](guides/shared/RECORDS_GUIDE.md) |
| Stream more than one request/response | [Streaming Guide](guides/streaming/STREAMING_GUIDE.md) |
| Maintain realm-scoped membership | [HyParView Guide](guides/overlay/HYPARVIEW_GUIDE.md) |
| Gossip realm-scoped messages/state | [Plumtree Guide](guides/overlay/PLUMTREE_GUIDE.md) |
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
| [RPC](guides/rpc/RPC_GUIDE.md) | Request/response; direct-dial via `call_station/8` (sealed from the provider's advertisement since 13.0.0) or the supervised `start_link_direct`/`advertise_direct` |
| [Content](guides/content/CONTENT_GUIDE.md) | Share blobs from your node and fetch them by content id (MCID), single-block or chunked, plus push/upload at a known recipient |
| [Records](guides/shared/RECORDS_GUIDE.md) | Signed, TTL'd facts in the DHT — your own record types |
| [Streaming](guides/streaming/STREAMING_GUIDE.md) | Streaming RPC (server / client / bidi); direct-dial via `call_stream_station/7` |
| [HyParView](guides/overlay/HYPARVIEW_GUIDE.md) | Bounded partial-view realm membership |
| [Plumtree](guides/overlay/PLUMTREE_GUIDE.md) | Epidemic broadcast trees, realm PubSub, OR-Set CRDT |
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
| [Content Protocol](guides/content/CONTENT_PROTOCOL.md) | The content id, the announcement record, the content procedure, the fetch and its bounds |
| [Streaming Protocol](guides/streaming/STREAMING_PROTOCOL.md) | Raw `call_stream`/`advertise_stream`, local in-process streams |

## Reference

| Document | Description |
|----------|-------------|
| [Glossary](GLOSSARY.md) | Terminology reference |
| [DECISIONS_POST_QUANTUM](design/DECISIONS_POST_QUANTUM.md) | Post-quantum decisions D1 onwards, the record the code and designs cite |
| [DESIGN_PQ_HANDSHAKE_FRAMES](design/DESIGN_PQ_HANDSHAKE_FRAMES.md) | The connection handshake, bindings and status statements, byte for byte |
| [DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS](design/DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md) | Signed records and frames after the handshake, and the decoding rule |
| [DESIGN_PQ_DHT_SLOTS_AND_BUDGET](design/DESIGN_PQ_DHT_SLOTS_AND_BUDGET.md) | DHT slot bounds, slot admission and the verification budget |
| [DESIGN_NEIGHBOUR_CHANNEL_BINDING](design/DESIGN_NEIGHBOUR_CHANNEL_BINDING.md) | Handshake v5: hop frames authenticated per session |
| [DESIGN_SWIM_INDIRECT_PROBE](design/DESIGN_SWIM_INDIRECT_PROBE.md) | SWIM indirect probe (PING-REQ) |
| [DESIGN_E2E_PAYLOAD_CONFIDENTIALITY](design/DESIGN_E2E_PAYLOAD_CONFIDENTIALITY.md) | Sealed calls, streams and events: stations relay payloads they cannot read |
| [DESIGN_E2E_SEAL_REPORT](design/DESIGN_E2E_SEAL_REPORT.md) | The caller's seal report |
| [DESIGN_E2E_SEALED_PUBSUB](design/DESIGN_E2E_SEALED_PUBSUB.md) | Sealed pubsub groups and their key distributor |
| [DESIGN_D27_NODE_SERVED_CONTENT](design/DESIGN_D27_NODE_SERVED_CONTENT.md) | Node-served content (D27) |
| [DESIGN_ORGLESS_SERVING](design/DESIGN_ORGLESS_SERVING.md) | Serving without an org: the `~<node_id>` namespace |

---

For relay server documentation (operator guides, DHT internals, peering, monitoring),
see macula-station.
