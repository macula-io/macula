# Macula SDK Glossary

**Terminology reference for the Macula SDK**

**Last Updated:** 2026-04-09
**Applies to:** Macula SDK v1.0.0+

---

## Architecture

### Relay
A stateless message router. Nodes connect outbound via QUIC. Relays route pub/sub events, RPC calls, and Erlang distribution traffic between nodes. Relays are run by macula-station.

### Relay Mesh
The federated network of relays that route messages between nodes. Relays peer with each other via Kademlia DHT for cross-relay discovery.

### Node
Any BEAM application using the Macula SDK. Connects outbound to a relay over QUIC. A node can subscribe to topics, publish events, advertise RPC procedures, and call remote procedures.

### Realm
An isolated namespace for multi-tenant applications. Format: reverse domain notation (e.g., `io.macula`, `io.example.myapp`). All communication is scoped to a realm.

### Cluster
A logical group of nodes that form an Erlang cluster. Can be formed via gossip (UDP multicast), static configuration, or mDNS. LAN clustering works independently of relay connections.

---

## Communication

### Pub/Sub
Topic-based event distribution. Publishers send events to topics; all subscribers on the mesh receive them. Topics are dot-separated strings (e.g., `sensors.temperature`). Entity IDs go in payloads, not topic names.

### RPC (Remote Procedure Call)
Request/response pattern. Providers advertise procedures; consumers call them. The relay mesh handles discovery via Kademlia DHT. Calls return `{ok, Result}` or `{error, Reason}`.

### Procedure
A named RPC endpoint (e.g., `math.add`, `weather.get_current`). Registered via `macula:advertise/3`, invoked via `macula:call/4`.

### Topic
A named pub/sub channel (e.g., `orders.placed`). Subscribed via `macula:subscribe/3`, published via `macula:publish/3`.

---

## Identity

### DID (Decentralized Identifier)
W3C-standard identifier for entities. Macula uses `did:macula:` method with hierarchical names (e.g., `did:macula:io.macula.acme`). Each level is controlled by its parent.

### UCAN (User Controlled Authorization Network)
Capability token based on JWT. Contains issuer, audience, capabilities, and optional proof chain. Used for delegated authorization without a central authority.

### Ed25519
Elliptic curve signature scheme used for all Macula cryptographic operations: node identity, UCAN signing, DID verification.

### BLAKE3
Fast cryptographic hash function used for content addressing (MCID). 32-byte output, ~20x faster than SHA-256 via Rust NIF.

---

## Resource Identification

### MRI (Macula Resource Identifier)
Typed, hierarchical resource identifier. Format: `mri:{type}:{realm}/{path}`.

Examples:
- `mri:realm:io.macula` -- a realm
- `mri:app:io.macula/acme/counter` -- an application
- `mri:device:io.macula/acme/sensor-1` -- a device

23 built-in types: realm, org, user, app, service, artifact, instance, license, cert, key, topic, proc, content, device, cluster, location, zone, network, model, dataset, config, class, taxonomy.

### MRI Type Registry
Runtime registry for MRI types. Built-in types are always valid. Custom types can be registered per-realm via `macula_mri_registry`.

---

## Transport

### QUIC
UDP-based transport protocol (RFC 9000) with built-in TLS 1.3. NAT-friendly (single UDP port), firewall-friendly (outbound only). Macula uses Quinn (Rust NIF) for QUIC transport.

### Wire Protocol
Binary message format using MessagePack encoding. Message types: CONNECT, SUBSCRIBE, PUBLISH, CALL, REPLY, REGISTER_PROCEDURE, PING/PONG.

### Tunnel
An encrypted Erlang distribution channel between two nodes routed through the relay mesh. Uses AES-256-GCM encryption (key derived from distribution cookie). The relay cannot read ETF content.

---

## Clustering

### Gossip Strategy
UDP multicast-based node discovery on `230.1.1.251:45892`. Nodes periodically announce themselves; peers join the cluster automatically. Optional HMAC authentication via shared secret.

### Cookie
Erlang distribution cookie. In Macula, also used as the AES-256-GCM key for tunnel encryption. Managed via `macula:get_cookie/0` and `macula:set_cookie/1`.

---

## System Topics

| Topic | Description |
|-------|-------------|
| `_mesh.node.up` | Node connected to relay |
| `_mesh.node.down` | Node disconnected |
| `_mesh.node.reroute` | Node switched relay (failover) |
| `_mesh.site.up` | First node of a site connected |
| `_mesh.site.down` | Last node of a site disconnected |
| `_mesh.relay.up` | Relay identity enabled |
| `_mesh.relay.down` | Relay identity disabled |
| `_mesh.relay.ping` | RTT measurement between relay and node |

---

For relay-specific terminology (DHT internals, peering, SWIM, bloom filters, relay boxes, virtual identities), see macula-station documentation.
