# Macula Glossary

**Definitive terminology reference for Macula platform**

**Last Updated:** 2026-04-06
**Applies to:** Macula v0.40.0+ (Federated Relay Mesh Architecture)

---

## Important: Federated Relay Mesh Architecture (v0.40.0+)

Macula is a **federated relay mesh** -- like email for machines. Nodes connect
outbound to relays via QUIC. Relays route messages between nodes. All WAN
communication flows through relays; LAN communication uses direct Erlang
distribution.

Key architectural properties:
- **Relay-routed WAN** -- Nodes never connect directly to each other over WAN
- **Outbound-only** -- Nodes initiate QUIC connections to relays (firewall/NAT friendly)
- **Stateless relays** -- Clients own state; re-declare subscriptions on reconnect
- **Full mesh peering** -- Relay boxes peer with each other (not spanning tree)
- **LAN unchanged** -- Local Erlang clustering works as before

---

## Quick Reference

| Term | One-Line Definition |
|------|---------------------|
| [Relay](#relay) | A stateless message router; nodes connect outbound via QUIC |
| [Relay Mesh](#relay-mesh) | The federated network of relays that route messages between nodes |
| [Relay Box](#relay-box) | Physical server hosting multiple virtual relay identities |
| [Virtual Relay Identity](#virtual-relay-identity) | A per-city relay identity with its own IPv6 address and DNS name |
| [Tunnel](#tunnel) | An encrypted distribution channel between two nodes through the relay mesh |
| [Bridge (dist)](#bridge-dist) | A gen_server bridging gen_tcp loopback sockets to relay pub/sub for Erlang distribution |
| [Node](#node) | A Macula instance with all subsystems |
| [Cluster](#cluster) | Logical group of cooperating nodes forming a local mesh |
| [Realm](#realm) | Isolated namespace for multi-tenant applications |
| [Gateway System](#gateway-system) | Subsystem for QUIC message handling (in every node) |
| [Bootstrap System](#bootstrap-system) | Subsystem for DHT operations (in every node) |
| [Peer](#peer) | Any connected node in the mesh |
| [Client](#client) | A handle (PID) for an active Macula connection |
| [DHT](#dht) | Distributed Hash Table for decentralized discovery |
| [Kademlia](#kademlia) | XOR-distance-based DHT algorithm used by Macula |
| [Topic](#topic) | Named channel for pub/sub message routing |
| [Procedure](#procedure) | Named endpoint for RPC calls |
| [MRI](#mri) | Macula Resource Identifier |
| [CRDT](#crdt) | Conflict-free Replicated Data Type for eventually-consistent state |
| [Gossip](#gossip) | Epidemic protocol for CRDT state replication |
| [Event](#event) | An immutable record of something that happened |
| [DID](#did) | Decentralized Identifier |
| [UCAN](#ucan) | User Controlled Authorization Network -- capability-based auth tokens |

---

## Relay Mesh Terms

### Relay

A **relay** is a stateless message router. Nodes connect to relays outbound
via QUIC. Relays forward messages between connected nodes and peer with other
relays to form the relay mesh.

**Key characteristics:**
- **Stateless** -- No subscriptions or routing state persisted; clients re-declare on reconnect
- **Outbound-only** -- Nodes initiate connections to relays (works behind NAT/firewalls)
- **WAN-only** -- LAN nodes communicate directly via Erlang distribution
- **Subscription-based forwarding** -- Only forwards messages that peers have subscribed to

**What a relay is NOT:**
- Not a message broker (no persistence, no delivery guarantees beyond best-effort)
- Not a peer (does not participate in DHT or application logic)
- Not required for LAN communication

---

### Relay Mesh

The **relay mesh** is the federated network of relays that routes messages
between nodes across the WAN. Relay boxes peer with each other in a full mesh
topology (every box connected to every other box).

```
  Node A ──QUIC──► Relay Box 1 ◄──peering──► Relay Box 2 ◄──QUIC── Node B
                       │                          │
                       └──────── peering ─────────┘
                                   │
                            Relay Box 3 ◄──QUIC── Node C
```

**Full mesh peering** means a single relay box failure does not split the
network -- nodes on the surviving boxes remain connected to each other.

---

### Relay Box

A **relay box** is a physical server (VPS) hosting multiple virtual relay
identities. Each box has its own IPv4 and IPv6 address and runs the relay
software.

**Current deployment (April 2026):**

| Box | Location | Virtual Identities |
|-----|----------|--------------------|
| box-hetzner-nuremberg | Nuremberg, DE | 75 (Western/Southern EU) |
| box-hetzner-helsinki | Helsinki, FI | 75 (Eastern/Northern EU) |

---

### Virtual Relay Identity

A **virtual relay identity** is a per-city relay identity hosted on a relay
box. Each virtual identity has:

- Its own DNS name: `relay-{cc}-{city}.macula.io` (e.g., `relay-de-munich.macula.io`)
- Its own IPv6 address (AAAA record)
- Its own QUIC listener

Virtual identities allow a small number of physical boxes to present many
geographic relay endpoints. Nodes connect to the identity closest to them
for lowest latency.

**Example:** `relay-fr-paris.macula.io` and `relay-de-munich.macula.io` may
both be hosted on box-hetzner-nuremberg, but each has a distinct IPv6 address
and DNS entry.

---

### Tunnel

A **tunnel** is an encrypted distribution channel between two nodes routed
through the relay mesh. Tunnels carry Erlang distribution traffic
(process messages, `gen_server:call`, `pg` group membership) over the mesh.

**Properties:**
- **AES-256-GCM encryption** -- Key derived from Erlang distribution cookie
- **Transparent** -- Once established, `Pid ! Msg` and `gen_server:call` work as normal
- **Relay-routed** -- Traffic flows through the relay mesh, not direct node-to-node
- **Cross-relay** -- Tunnels can span multiple relay boxes via peering

**See also:** [Bridge (dist)](#bridge-dist)

---

### Bridge (dist)

A **bridge** is a supervised `gen_server` that bridges `gen_tcp` loopback
sockets to relay pub/sub for Erlang distribution. Each tunnel has one bridge
process on each side.

**How it works:**
1. Erlang distribution writes to a local `gen_tcp` socket (loopback)
2. The bridge reads from that socket and publishes to a relay topic
3. The remote bridge subscribes to that topic and writes to its local socket
4. Erlang distribution reads from the remote socket

**Key modules:**
- `macula_dist_bridge.erl` -- gen_server per tunnel (reader + writer + metrics)
- `macula_dist_bridge_sup.erl` -- simple_one_for_one supervisor for bridge processes

**Features:**
- Per-tunnel metrics (bytes_in/out, msgs_in/out)
- Backpressure (pauses when relay queue exceeds high-water mark)
- Reconnection (bridge re-acquires relay client on disconnect, 30s retry window)

---

## Core Architecture Terms

### Node

A **node** is a single Macula instance. Every node has all subsystems:
- **Gateway System** -- QUIC message handling
- **Bootstrap System** -- DHT and peer discovery
- **Peer System** -- Connection management
- **Platform System** -- Distributed coordination (masterless CRDTs)

**Node ID:** A 32-byte (256-bit) identifier derived from the node's TLS
certificate fingerprint, used for DHT routing.

Nodes connect outbound to one relay for WAN communication. On disconnect,
they fail over to another relay and re-declare their subscriptions.

**Note:** "Node" may also refer to an Erlang/BEAM node (`node@host`). The
meaning is usually clear from context.

---

### Cluster

A **Macula Cluster** is a logical group of cooperating nodes, typically
co-located on the same LAN. Nodes within a cluster communicate directly
via Erlang distribution (no relay needed).

**Scale examples:**
- A home server (1-3 nodes)
- A Raspberry Pi or Nerves device (single node)
- A small office rack (3-10 nodes)

**Key characteristics:**
- **LAN scope** -- Nodes are co-located
- **Direct Erlang distribution** -- No relay for intra-cluster traffic
- **Common DHT** -- Peers discover each other locally
- **Minimum viable cluster** -- A single node is a cluster of one

---

### Realm

A **Realm** is a virtual namespace that defines identity and resource
boundaries -- analogous to a DNS domain. Realms are orthogonal to clusters:
they represent organizational/application boundaries, not infrastructure.

**Key characteristics:**
- **Virtual concept** -- Not tied to physical deployment
- **Spans clusters** -- Same realm can exist across multiple clusters
- **Identity boundary** -- Defines "who you are" in the mesh
- **Namespace isolation** -- Topics, procedures, subscriptions are realm-scoped
- **Multi-tenancy** -- Multiple realms can coexist on a single cluster

**Example:** `<<"com.mycompany.production">>` and `<<"com.mycompany.staging">>`
are separate realms that cannot see each other's messages, even on the same cluster.

**In code:**
```erlang
{ok, Client} = macula:connect_local(#{realm => <<"my.app.realm">>}).
```

---

### Peer

A **peer** is any node participating in the Macula mesh. Peers can:
- Publish and subscribe to topics
- Advertise and call RPC procedures
- Communicate with other nodes through the relay mesh (WAN) or directly (LAN)

---

### Client

A **client** is a handle (PID) returned when connecting to Macula. It
represents an active connection that can perform mesh operations.

```erlang
{ok, Client} = macula:connect_local(#{realm => <<"my.realm">>}).
%% Client is a pid() usable for publish/subscribe/call
```

---

### Gateway System

The **Gateway System** is a subsystem in every node that handles QUIC message
routing. In the relay mesh architecture, it manages the outbound QUIC
connection to the node's relay.

**Code location:** `src/macula_gateway_system/`

---

### Bootstrap System

The **Bootstrap System** is a subsystem in every node that handles DHT
operations and discovery.

**Responsibilities:**
- DHT queries: FIND_NODE, FIND_VALUE, STORE
- Peer registration and discovery

**Code location:** `src/macula_bootstrap_system/`

---

## Networking Terms

### QUIC

**QUIC** (Quick UDP Internet Connections) is the transport protocol used by
Macula. Benefits:
- **Multiplexed streams** -- Multiple logical channels over one connection
- **Built-in TLS 1.3** -- Mandatory encryption
- **Connection migration** -- Survives IP address changes
- **NAT-friendly** -- Works over UDP through firewalls
- **Single port** -- One UDP port for all traffic

**Implementation:** Macula uses [quicer](https://github.com/qzhuyan/quicer),
which wraps Microsoft's MsQuic library.

---

### DHT

A **Distributed Hash Table** provides decentralized key-value lookup across
the mesh. Macula uses a Kademlia-style DHT for:
- **Service discovery** -- Finding which node provides a procedure
- **Subscriber lookup** -- Finding who subscribes to a topic
- **Node routing** -- Finding paths to other nodes

**Key concepts:**
- **k-buckets** -- Routing table organized by XOR distance
- **Node ID** -- 256-bit identifier for each node
- **Replication factor (k)** -- Number of nodes storing each value (default: 20)

---

### Kademlia

**Kademlia** is the DHT algorithm used by Macula. It organizes nodes by XOR
distance and provides O(log N) lookup complexity. Kademlia operations:
- **FIND_NODE** -- Locate nodes closest to a given ID
- **FIND_VALUE** -- Look up a stored value by key
- **STORE** -- Store a key-value pair on the closest nodes

---

### mDNS

**Multicast DNS** (mDNS) discovers nodes on the local network without a
central DNS server. Used for:
- LAN peer discovery
- Zero-configuration networking
- Development/testing environments

---

## Messaging Terms

### Topic

A **topic** is a named channel for pub/sub messaging.

**Design principle: Event types in topics, IDs in payloads.**

```
Topic   = WHAT happened (event type)
Payload = WHO/WHERE/WHEN it happened (entity details)
```

| Approach | 1M Instances | Topics | Result |
|----------|--------------|--------|--------|
| ID in topic | 1M sensors | 1M topics | DHT explosion |
| ID in payload | 1M sensors | 1 topic | Scalable |

**Wildcards:**
- `*` -- Matches one segment: `<<"sensor.*.measured">>`
- `#` -- Matches zero or more segments: `<<"sensor.#">>`

---

### Procedure

A **procedure** is a named endpoint for RPC. Providers advertise procedures;
callers invoke them.

```erlang
macula:advertise(Client, <<"math.add">>, fun(#{a := A, b := B}) ->
    {ok, #{result => A + B}}
end).

{ok, #{result := 8}} = macula:call(Client, <<"math.add">>, #{a => 5, b => 3}).
```

---

### MRI

A **Macula Resource Identifier** is a structured URI for addressing resources
in the mesh. Format: `mri:type:realm/path`.

---

## Security Terms

### DID

A **Decentralized Identifier** (DID) is a self-sovereign identity that does
not depend on a central registry. Used for node and user identity in Macula.

---

### UCAN

**User Controlled Authorization Network** (UCAN) is a capability-based
authorization scheme using chained JWT-like tokens. UCANs allow delegated,
attenuated permissions without a central authority.

---

### TLS 1.3

**Transport Layer Security 1.3** secures all Macula connections. In QUIC,
TLS 1.3 is mandatory.

---

### Realm Isolation

**Realm isolation** ensures that nodes in different realms cannot see each
other's topics, call each other's procedures, or access each other's DHT
entries. Isolation is enforced cryptographically.

---

## Distributed State Terms

### CRDT

A **Conflict-free Replicated Data Type** is a data structure that can be
updated concurrently on multiple nodes and automatically converges to a
consistent state. Macula supports:
- **LWW-Register** -- Last-Write-Wins register
- **G-Counter** -- Grow-only counter
- **PN-Counter** -- Increment/decrement counter
- **OR-Set** -- Observed-Remove set

---

### Gossip

The **Gossip Protocol** synchronizes CRDT state across nodes using epidemic
propagation.

**Parameters:**
- Push interval: 1 second
- Anti-entropy: 30 seconds
- Fanout: 3 peers per round

**Benefits:**
- No leader election needed
- Partition tolerant (AP in CAP)
- State converges eventually

---

## Event Sourcing Terms

### Event

An **event** is an immutable record of something that happened. Events should:
- Be named in past tense: `user_registered`, `order_placed`
- Capture business intent, not CRUD operations
- Include all relevant data in the payload

**Anti-pattern:** `user_updated` (CRUD) vs `user_email_changed` (intent)

---

### Projection

A **projection** is a read model built by processing events. Projections
transform events into queryable state and can be rebuilt from event history.

---

## BEAM Terms

### BEAM

The **BEAM** is the virtual machine that runs Erlang and Elixir code.
Properties: lightweight processes (millions per node), preemptive scheduling,
hot code upgrades, built-in distribution.

---

### OTP

**Open Telecom Platform** provides standard library and design patterns:
supervision trees, behaviours (gen_server, gen_statem), and applications.

---

### Supervision Tree

A **supervision tree** is a hierarchical structure where supervisors monitor
workers and restart them on failure.

---

## Configuration Terms

### Environment Variables

| Variable | Description |
|----------|-------------|
| `MACULA_REALM` | Default realm for connections |
| `MACULA_QUIC_PORT` | QUIC listener port (default: 4433) |
| `MACULA_DIST_MODE` | Distribution mode: `direct` or `relay` |
| `MACULA_RELAY_URL` | Relay QUIC URL (e.g., `quic://relay-de-munich.macula.io:9443`) |
| `MACULA_TLS_MODE` | `production` or `development` |
| `MACULA_TLS_CACERTFILE` | CA bundle path (production mode) |
| `MACULA_TLS_CERTFILE` | Certificate path |
| `MACULA_TLS_KEYFILE` | Private key path |

---

## See Also

- [PubSub Guide](guides/PUBSUB_GUIDE.md)
- [RPC Guide](guides/RPC_GUIDE.md)
- [Dist Over Mesh Guide](guides/DIST_OVER_MESH_GUIDE.md)
- [Clustering Guide](guides/CLUSTERING_GUIDE.md)
- [TLS Guide](operator/TLS_GUIDE.md)
