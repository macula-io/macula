# Macula Glossary

**Definitive terminology reference for Macula platform**

**Last Updated:** 2025-12-01
**Applies to:** Macula v0.13.0+ (SuperMesh Architecture)

---

## Important: SuperMesh Architecture (v0.13.0+)

The December 2025 architecture refinement introduces:
- **Macula Cluster** - Deployment-agnostic logical grouping of nodes
- **Bridge Nodes** - Cross-Cluster federation for SuperMesh
- **CRDTs + Gossip** - Replaces Raft consensus for coordination
- **Federated Registry** - Secure application distribution

Every node within a Cluster has all capabilities (Gateway, Bootstrap, Peer systems).

---

## Quick Reference

| Term | One-Line Definition |
|------|---------------------|
| [Macula Cluster](#macula-cluster) | Logical group of cooperating nodes forming a local mesh |
| [Seed Node](#seed-node) | DHT entry point for new peers joining a Cluster |
| [Bridge Node](#bridge-node) | Connects multiple Clusters, routes cross-Cluster traffic |
| [SuperMesh](#supermesh) | Federation of Clusters connected via Bridge Nodes |
| [Node](#node) | A Macula instance with all capabilities |
| [Peer](#peer) | Any connected node in the mesh |
| [Gateway System](#gateway-system) | Subsystem for QUIC message routing (in every node) |
| [Bootstrap System](#bootstrap-system) | Subsystem for DHT operations (in every node) |
| [Realm](#realm) | Isolated namespace for multi-tenant applications |
| [DHT](#dht) | Distributed Hash Table for decentralized discovery |
| [Topic](#topic) | Named channel for pub/sub message routing |
| [Procedure](#procedure) | Named endpoint for RPC calls |
| [Registry](#registry) | Package repository for application distribution |
| [Cluster Controller](#cluster-controller) | Deploys apps from registry to Cluster |

---

## SuperMesh Architecture Terms

### Macula Cluster

A **Macula Cluster** is a small, local deployment of cooperating Macula nodes. Think of it as a "black box" at a specific location. Nodes within a Cluster form their own **intra-cluster mesh**.

**Scale examples:**
- A home server (1-3 nodes)
- A Raspberry Pi or Nerves device (single node)
- A small office rack (3-10 nodes)
- An edge deployment at a cell tower

**Key characteristics:**
- **Local scope** - Nodes are co-located (same home, office, device)
- **Intra-cluster mesh** - Nodes form a fully-connected mesh within the Cluster
- **Deployment-agnostic** - Can run on Kubernetes, Docker, systemd, Nerves, or single process
- **Common DHT** - Peers discover each other via local DHT
- **Direct P2P** - Nodes communicate directly within Cluster (low latency)
- **Typical size** - 1-10 nodes

**Intra-cluster communication:**
- Currently: Erlang distributed mesh (`node@host` connections)
- Future: QUIC Distribution when operational (see v1.1.0+ roadmap)

```
┌─────────────────────────────────────────────────────────────────┐
│              CLUSTER (e.g., Home Server)                        │
│                                                                  │
│   ┌─────┐◄────────────►┌─────┐◄────────────►┌─────┐            │
│   │Node │              │Node │              │Node │            │
│   │  1  │◄────────────►│  2  │◄────────────►│  3  │            │
│   └──┬──┘              └──┬──┘              └──┬──┘            │
│      │    Intra-cluster   │                    │               │
│      │    Erlang mesh     │                    │               │
│      └────────────────────┴────────────────────┘               │
│                           │                                     │
│              Local DHT + CRDT State                             │
│                           │                                     │
│                    Bridge Node ──► To Street Mesh               │
└─────────────────────────────────────────────────────────────────┘
```

**Minimum viable Cluster:** Single Macula node running all subsystems. It's a Cluster of one.

**Configuration:**
```erlang
%% Cluster ID determined by priority:
%% 1. MACULA_CLUSTER environment variable
%% 2. Config file cluster_id setting
%% 3. Derive from Seed Node's Cluster
```

**Note:** Replaces the earlier "MuC" (Macula Micro Center) concept.

---

### Bridge Node

A **Bridge Node** is a Macula node that connects its Cluster to the next level of the mesh hierarchy. Bridge Nodes at each level **form their own mesh** with a shared DHT.

**Key insight:** Bridge Nodes mesh among themselves at each level:

```
┌─────────────────────────────────────────────────────────────────┐
│                    STREET MESH (Bridge Layer)                   │
│                                                                  │
│   ┌────────┐◄──────────►┌────────┐◄──────────►┌────────┐       │
│   │Bridge  │            │Bridge  │            │Bridge  │       │
│   │(Home 1)│◄──────────►│(Home 2)│◄──────────►│(Home 3)│       │
│   └───┬────┘            └───┬────┘            └───┬────┘       │
│       │                     │                     │            │
│       │  Bridges form mesh + shared DHT at this level          │
│       │                                           │            │
└───────┼─────────────────────┼─────────────────────┼────────────┘
        ▼                     ▼                     ▼
   ┌─────────┐           ┌─────────┐           ┌─────────┐
   │ Cluster │           │ Cluster │           │ Cluster │
   │ (Home 1)│           │ (Home 2)│           │ (Home 3)│
   └─────────┘           └─────────┘           └─────────┘
```

**Responsibilities:**
- Form mesh with other Bridge Nodes at the same level
- Maintain DHT at that level (street DHT, city DHT, etc.)
- Forward DHT queries that can't be resolved locally
- Route cross-Cluster RPC/PubSub messages
- Enforce federation policies (what's exposed, what's blocked)

**DHT query escalation (locality-first):**
```
1. Query local Cluster DHT
2. If miss → query Street Mesh DHT (via Bridge)
3. If miss → query Neighborhood Mesh DHT
4. If miss → query City Mesh DHT
5. ... escalate until found or top level reached
6. Cache result at lower levels to avoid re-escalation
```

**Configuration:**
```erlang
%% Bridge discovery (priority order):
%% 1. MACULA_BRIDGES env var (explicit)
%% 2. DNS SRV lookup (_macula._udp.example.com)
%% 3. boot.macula.io directory (fallback)

%% Federation policy
{bridge_policy, [
    {allow, <<"services/*">>},
    {deny, <<"internal/*">>}
]}.
```

**What it is NOT:**
- NOT a Seed Node (different role)
- NOT required for intra-Cluster communication
- NOT a consensus participant

---

### SuperMesh

A **SuperMesh** is a federation of Clusters (or other SuperMeshes) connected via Bridge Nodes. SuperMeshes are **hierarchical and fractal** - they nest at any scale.

**Hierarchy example:**
```
Cluster (Home)
    └─► Street Mesh (neighbors)
            └─► Neighborhood Mesh
                    └─► City Mesh
                            └─► Province/State Mesh
                                    └─► Country Mesh
                                            └─► Region Mesh (EU, NA, APAC)
                                                    └─► Global Mesh
```

**Each level is a SuperMesh** - the term applies at any scale above Cluster:
- Street Mesh = SuperMesh of home Clusters on a street
- City Mesh = SuperMesh of neighborhood meshes
- Global Mesh = SuperMesh of regional meshes

**What it enables:**
- **Locality-first** - Most traffic stays local (street/neighborhood)
- **Hierarchical routing** - Queries escalate up only when needed
- **Organic growth** - Start with Cluster, naturally expand to street, city, etc.
- **Administrative boundaries** - Each level can have its own policies

```
┌─────────────────────────────────────────────────────────────────┐
│                      CITY MESH: Amsterdam                       │
│  ┌─────────────────────────┐  ┌─────────────────────────┐      │
│  │  NEIGHBORHOOD: Centrum   │  │  NEIGHBORHOOD: Zuid      │      │
│  │  ┌───────┐  ┌───────┐   │  │  ┌───────┐  ┌───────┐   │      │
│  │  │Street │  │Street │   │  │  │Street │  │Street │   │      │
│  │  │ Mesh  │  │ Mesh  │   │  │  │ Mesh  │  │ Mesh  │   │      │
│  │  └───┬───┘  └───┬───┘   │  │  └───┬───┘  └───┬───┘   │      │
│  │      └──────────┘       │  │      └──────────┘       │      │
│  └────────────┬────────────┘  └────────────┬────────────┘      │
│               └────────────────────────────┘                    │
│                       Bridge Nodes                              │
└─────────────────────────────────────────────────────────────────┘
```

**Trust Model:**
- Each organization maintains their own realm
- Bridge Nodes negotiate federation agreements at each level
- Cross-realm traffic is explicitly routed through bridges
- Federation is opt-in per service

---

### Registry

A **Registry** is a package repository for distributing Macula applications. Features:

- **Federated model** - Organizations run their own registries
- **Package signing** - Cryptographic verification
- **Capability declarations** - Apps declare what they need
- **Security scanning** - Static analysis before publishing

**Configuration:**
```erlang
{registries, [
    #{name => <<"internal">>,
      url => <<"https://registry.myorg.com">>,
      trust => full,
      pubkey => <<...>>},
    #{name => <<"macula-public">>,
      url => <<"https://boot.macula.io/registry">>,
      trust => verified,
      pubkey => <<...>>}
]}.
```

**Public registry:** boot.macula.io (one option among many)

---

### Cluster Controller

A **Cluster Controller** manages application deployment within a Cluster:

- Watches configured registries for updates
- Verifies package signatures before deployment
- Enforces local deployment policy
- Manages app lifecycle (deploy, upgrade, remove)

**Deployment policy:**
```erlang
{deployment_policy, [
    {allow_nifs, false},           % No native code from external
    {require_capabilities, true},  % Must declare what it needs
    {max_memory_mb, 512},          % Resource limits
    {allowed_ports, [80, 443]}     % Network restrictions
]}.
```

---

### Gossip Protocol

The **Gossip Protocol** synchronizes CRDT state across nodes in a Cluster using epidemic propagation.

**Parameters:**
- Push interval: 1 second (hot path)
- Anti-entropy: 30 seconds (consistency maintenance)
- Fanout: 3 peers per round

**Benefits:**
- No leader election needed
- Partition tolerant (AP in CAP)
- State converges eventually

---

## Core Architecture Terms

### Realm

A **Realm** is a virtual namespace that defines identity and resource boundaries in Macula - analogous to a DNS domain. Realms are **orthogonal to Clusters**: they represent organizational/application boundaries, not infrastructure boundaries.

**Key characteristics:**
- **Virtual concept** - Not tied to physical deployment
- **Spans Clusters** - Same realm can exist across multiple Clusters (geo-distribution)
- **Identity boundary** - Defines "who you are" in the mesh
- **Namespace isolation** - Topics, procedures, subscriptions are realm-scoped
- **Multi-tenancy** - Multiple realms can coexist on a single Cluster

**Realm vs Cluster vs SuperMesh:**

| Aspect | Cluster | SuperMesh | Realm |
|--------|---------|-----------|-------|
| Nature | Physical deployment | Hierarchical federation | Logical/virtual |
| Scope | Local (home, office, device) | Any scale above Cluster | Organization/application |
| Scale | 1-10 nodes typically | Fractal: street → city → country → global | Spans entire hierarchy |
| Purpose | Node coordination | Geographic distribution | Identity & namespace |

**Scale hierarchy (fractal):**
```
Cluster (Home)           ←── Smallest unit
    └─► Street Mesh
            └─► Neighborhood Mesh
                    └─► City Mesh
                            └─► Province Mesh
                                    └─► Country Mesh
                                            └─► Region Mesh
                                                    └─► Global Mesh
```

**Realm spans the entire hierarchy:**
```
┌─────────────────────────────────────────────────────────────────────┐
│                     REALM: com.acme.iot                              │
│                                                                      │
│  ┌────────────────────────────────────────────────────────────┐     │
│  │               COUNTRY MESH: Netherlands                     │     │
│  │  ┌────────────────────────┐  ┌────────────────────────┐    │     │
│  │  │   CITY: Amsterdam       │  │   CITY: Rotterdam       │    │     │
│  │  │  ┌────────┐ ┌────────┐ │  │  ┌────────┐            │    │     │
│  │  │  │Cluster │ │Cluster │ │  │  │Cluster │            │    │     │
│  │  │  │(Home 1)│ │(Home 2)│ │  │  │(Office)│            │    │     │
│  │  │  └────────┘ └────────┘ │  │  └────────┘            │    │     │
│  │  └────────────────────────┘  └────────────────────────┘    │     │
│  └────────────────────────────────────────────────────────────┘     │
│                              ▲                                       │
│                              │ Cross-Country Bridge                  │
│                              ▼                                       │
│  ┌────────────────────────────────────────────────────────────┐     │
│  │               COUNTRY MESH: Germany                         │     │
│  │  ┌────────────────────────┐                                 │     │
│  │  │   CITY: Berlin          │                                 │     │
│  │  │  ┌────────┐            │                                 │     │
│  │  │  │Cluster │            │                                 │     │
│  │  │  │(Home 3)│            │                                 │     │
│  │  │  └────────┘            │                                 │     │
│  │  └────────────────────────┘                                 │     │
│  └────────────────────────────────────────────────────────────┘     │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

**Relationships:**
- A **Cluster** is the smallest unit (home, edge device, office)
- **SuperMesh** is any federation level above Cluster (street, city, country, global)
- A **Realm** spans across the entire hierarchy (virtual namespace)
- **Bridge Nodes** connect meshes at each level of the hierarchy
- **Cross-realm** communication requires explicit federation agreements

**Example:** `<<"com.mycompany.production">>` and `<<"com.mycompany.staging">>` are separate realms that cannot see each other's messages, even if running on the same Cluster.

**In code:**
```erlang
{ok, Client} = macula:connect_local(#{realm => <<"my.app.realm">>}).
```

**See also:** [Multi-tenancy in PUBSUB_GUIDE](developer/PUBSUB_GUIDE.md)

---

### Peer

A **peer** is any node participating in the Macula mesh network. Peers can:
- Publish and subscribe to topics
- Advertise and call RPC procedures
- Communicate directly with other peers (P2P)

Internally implemented by `macula_peer.erl`.

**Related terms:** [Node](#node), [Client](#client), [Gateway](#gateway)

---

### Node

A **node** is a single Macula instance. Since v0.8.5, every node has ALL capabilities:
- **Gateway System** - QUIC message routing
- **Bootstrap System** - DHT and peer discovery
- **Peer System** - Connection management
- **Platform System** - Distributed coordination

```
┌──────────────────────────────────────────┐
│               Macula Node                │
├──────────────────────────────────────────┤
│  ┌──────────────┐  ┌─────────────────┐   │
│  │   Gateway    │  │    Bootstrap    │   │
│  │   System     │  │     System      │   │
│  └──────────────┘  └─────────────────┘   │
│  ┌──────────────────────────────────────┐│
│  │           Peer System                ││
│  └──────────────────────────────────────┘│
└──────────────────────────────────────────┘
```

**Node ID:** A 32-byte (256-bit) identifier used for DHT routing, generated from the node's TLS certificate fingerprint.

**Note:** In some contexts, "node" may also refer to an Erlang/BEAM node (`node@host`). The meaning is usually clear from context.

---

### Seed Node

A **seed node** (also called "bootstrap address") is a **well-known node address** that new nodes use for initial mesh discovery.

**Key points:**
- Seed nodes are **regular nodes** with no special code
- They are simply "the first nodes to start" whose addresses are shared
- Any node can be a seed node - it's a deployment choice, not a code difference
- Multiple seed nodes recommended for redundancy

**Configuration:**
```erlang
%% New node connects to seed node(s) for initial discovery
{bootstrap_addresses, ["quic://seed1.example.com:4433", "quic://seed2.example.com:4433"]}
```

**Environment variable:** `MACULA_BOOTSTRAP_PEERS`

**Note:** This was previously called "bootstrap node" in some documentation. "Seed node" is preferred to avoid confusion with the Bootstrap System.

---

### Client

A **client** is a handle (PID) returned when connecting to Macula. It represents an active connection that can perform mesh operations.

**In code:**
```erlang
{ok, Client} = macula:connect_local(#{realm => <<"my.realm">>}).
%% Client is now a pid() that can be used for publish/subscribe/call
```

**Note:** The public API uses the type `macula:client()` which is an alias for `pid()`.

---

### Gateway System

The **Gateway System** is a subsystem present in **every node** that handles QUIC message routing.

**Responsibilities:**
- QUIC listener management (`macula_gateway_quic_server`)
- Client connection handling (`macula_gateway_clients`)
- PubSub message routing (`macula_gateway_pubsub`, `macula_gateway_pubsub_router`)
- RPC request routing (`macula_gateway_rpc`, `macula_gateway_rpc_router`)
- Mesh connection pooling (`macula_gateway_mesh`)
- DHT query forwarding (`macula_gateway_dht`)
- Health monitoring (`macula_gateway_health`)

**Main module:** `macula_gateway.erl` - API facade and orchestrator

**Code location:** `src/macula_gateway_system/`

---

### Bootstrap System

The **Bootstrap System** is a subsystem present in **every node** that handles DHT operations and peer discovery.

**Responsibilities:**
- DHT queries: FIND_NODE, FIND_VALUE, STORE (`macula_bootstrap_server`)
- Peer registration and discovery (`macula_bootstrap_registry`)
- Health monitoring (`macula_bootstrap_health`)

**Code location:** `src/macula_bootstrap_system/`

**Note:** "Bootstrap System" refers to the subsystem. For the well-known entry point, see [Seed Node](#seed-node).

---

### Mesh

The **mesh** is the network topology formed by interconnected Macula peers. Unlike hub-and-spoke architectures, the mesh allows:
- Direct peer-to-peer communication
- Multiple paths between nodes
- No single point of failure

```
    ┌─────┐     ┌─────┐
    │Peer1│◄───►│Peer2│
    └──┬──┘     └──┬──┘
       │           │
       ▼           ▼
    ┌─────┐     ┌─────┐
    │Peer3│◄───►│Peer4│
    └─────┘     └─────┘
```

---

## Networking Terms

### QUIC

**QUIC** (Quick UDP Internet Connections) is the transport protocol underlying HTTP/3. Macula uses QUIC because it provides:
- **Multiplexed streams** - Multiple logical channels over one connection
- **Built-in TLS 1.3** - Mandatory encryption
- **Connection migration** - Survives IP address changes
- **NAT-friendly** - Works over UDP through firewalls

**Implementation:** Macula uses [quicer](https://github.com/qzhuyan/quicer), which wraps Microsoft's MsQuic library.

---

### HTTP/3

**HTTP/3** is the latest version of HTTP, built on QUIC instead of TCP. Macula uses HTTP/3 semantics for:
- Request/response patterns (RPC)
- Streaming (pub/sub delivery)
- Multiplexing (multiple topics on one connection)

**Note:** Macula uses HTTP/3 framing but with custom semantics optimized for mesh networking.

---

### NAT Traversal

**NAT traversal** refers to techniques for establishing connections between peers behind Network Address Translation (NAT) devices (home routers, corporate firewalls).

Macula's approach:
- **QUIC/UDP** - Better NAT compatibility than TCP
- **Gateway relay** - Fallback when direct P2P fails
- **Future:** STUN/TURN/ICE for hole punching

**See also:** NAT Traversal documentation in the guides section.

---

### DHT

A **Distributed Hash Table** is a decentralized system that provides key-value lookup across a network of peers. Macula uses a Kademlia-style DHT for:
- **Service discovery** - Finding which peer provides a procedure
- **Subscriber lookup** - Finding who subscribes to a topic
- **Peer routing** - Finding paths to other nodes

**Key concepts:**
- **k-buckets** - Routing table organized by XOR distance
- **Node ID** - 256-bit identifier for each peer
- **Replication factor (k)** - Number of nodes storing each value (default: 20)

---

### mDNS

**Multicast DNS** (mDNS) is a protocol for discovering services on local networks without a central DNS server. Macula uses mDNS for:
- Local peer discovery (same LAN)
- Zero-configuration networking
- Development/testing environments

**Implementation:** Uses [shortishly/mdns](https://github.com/shortishly/mdns) library.

---

## Messaging Terms

### Topic

A **topic** is a named channel for pub/sub messaging. Publishers send messages to topics; subscribers receive messages from topics they've subscribed to.

**Core Design Principle: Event Types in Topics, IDs in Payloads**

> This is a non-negotiable architectural principle for scalable pub/sub.

```
Topic   = WHAT happened (event type)
Payload = WHO/WHERE/WHEN it happened (entity details)
```

| Approach | 1M Instances | Topics | Result |
|----------|--------------|--------|--------|
| ID in topic | 1M sensors | 1M topics | 💥 DHT explosion |
| ID in payload | 1M sensors | 1 topic | ✅ Scalable |

**Example - Weather Service:**
```erlang
%% ❌ WRONG: ID in topic (causes topic explosion)
Topic = <<"com.ibm.weather.manchester.main-street.wind_measured">>.

%% ✅ CORRECT: Event type in topic, details in payload
Topic = <<"com.ibm.weather.wind_measured">>,
Payload = #{
    <<"location">> => #{<<"city">> => <<"manchester">>, <<"street">> => <<"main-street">>},
    <<"instance_id">> => <<"ibm-weather-manchester-001">>,
    <<"wind_speed_ms">> => 12.5
}.
```

**Subscriber filtering** is an application-level concern - filter in your callback.

**Wildcards:**
- `*` - Matches one segment: `<<"sensor.*.measured">>`
- `#` - Matches zero or more segments: `<<"sensor.#">>`

**See also:** [PubSub Guide](developer/PUBSUB_GUIDE.md)

---

### Procedure

A **procedure** is a named endpoint for RPC (Remote Procedure Call) operations. Providers advertise procedures; callers invoke them.

**Example:**
```erlang
%% Provider advertises
macula:advertise(Client, <<"math.add">>, fun(#{a := A, b := B}) ->
    {ok, #{result => A + B}}
end).

%% Caller invokes
{ok, #{result := 8}} = macula:call(Client, <<"math.add">>, #{a => 5, b => 3}).
```

**See also:** [RPC Guide](developer/RPC_GUIDE.md)

---

### Pub/Sub

**Publish/Subscribe** is a messaging pattern where:
- **Publishers** send messages to topics without knowing who receives them
- **Subscribers** receive messages from topics without knowing who sent them

This decouples senders from receivers, enabling scalable event-driven architectures.

---

### RPC

**Remote Procedure Call** is a request/response pattern where:
- **Caller** sends a request with arguments
- **Provider** executes a handler and returns a result

Unlike pub/sub, RPC is synchronous (caller waits for response).

---

### Subscription

A **subscription** is an active registration to receive messages from a topic. Represented by a reference that can be used to unsubscribe.

```erlang
{ok, SubRef} = macula:subscribe(Client, <<"events.#">>, fun(Event) ->
    handle_event(Event)
end).

%% Later
ok = macula:unsubscribe(Client, SubRef).
```

---

### Advertisement

An **advertisement** is a registration that makes a procedure available for RPC calls. Advertisements are:
- Stored in the local service registry
- Propagated to the DHT for discovery
- Refreshed periodically (TTL-based)

---

## Platform Layer Terms

### Platform Layer

The **Platform Layer** (v0.9.0+) provides distributed coordination primitives for workload applications:
- Leader election via Raft consensus
- Shared state via CRDTs
- Workload registration

**See also:** Platform Vision in the architecture documentation.

---

### Leader Election

**Leader election** is the process of selecting a single coordinator node from a group. Macula uses Ra (Raft) for:
- Consensus on who is leader
- Automatic failover if leader crashes
- Term numbers for consistency

```erlang
{ok, LeaderNodeId} = macula:get_leader(Client).
```

---

### CRDT

A **Conflict-free Replicated Data Type** is a data structure that can be updated concurrently on multiple nodes and automatically converges to a consistent state.

Macula supports:
- **LWW-Register** - Last-Write-Wins register
- **G-Counter** - Grow-only counter
- **PN-Counter** - Increment/decrement counter
- **G-Set** - Grow-only set
- **OR-Set** - Observed-Remove set

```erlang
ok = macula:propose_crdt_update(Client, <<"counter">>, {increment, 1},
    #{crdt_type => pn_counter}).
```

---

## BEAM Terms

### BEAM

The **BEAM** is the virtual machine that runs Erlang and Elixir code. BEAM provides:
- Lightweight processes (millions per node)
- Preemptive scheduling
- Hot code upgrades
- Built-in distribution

Macula is built entirely on BEAM technologies.

---

### OTP

**Open Telecom Platform** is the standard library and design patterns for Erlang/Elixir applications. Key concepts:
- **Supervision trees** - Hierarchical process monitoring
- **Behaviours** - Reusable patterns (gen_server, gen_statem)
- **Applications** - Packaged, startable components

---

### Supervision Tree

A **supervision tree** is a hierarchical structure of processes where supervisors monitor workers and restart them on failure.

```
        macula_sup
            │
    ┌───────┼───────┐
    │       │       │
gateway_sup dht_sup platform_sup
    │
┌───┴───┐
│       │
client  pubsub
manager handler
```

**Strategy:** `one_for_one` (restart failed child only) or `one_for_all` (restart all children).

---

### gen_server

A **gen_server** is an OTP behaviour for implementing client-server processes. Most Macula modules are gen_servers:
- `macula_gateway` - Main gateway process
- `macula_peer` - Client connection handler
- `macula_rpc_handler` - RPC call management

---

### Process

In BEAM, a **process** is a lightweight, isolated unit of execution. Processes:
- Share nothing (no shared memory)
- Communicate via message passing
- Can number in millions per node
- Crash independently (fault isolation)

---

## Event Sourcing Terms

### Event Sourcing

**Event sourcing** is a pattern where state changes are captured as immutable events. Instead of storing current state, you store the sequence of events that led to it.

**Benefits:**
- Complete audit trail
- Replay capability
- Temporal queries

**See also:** ReckonDB integration (see ecosystem documentation)

---

### Event

An **event** is an immutable record of something that happened. Events should:
- Be named in past tense: `user_registered`, `order_placed`
- Capture business intent, not CRUD operations
- Include all relevant data in the payload

**Anti-pattern:** `user_updated` (CRUD) vs `user_email_changed` (intent)

---

### Projection

A **projection** is a read model built by processing events. Projections:
- Transform events into queryable state
- Can be rebuilt from event history
- Optimized for specific read patterns

---

## Security Terms

### TLS 1.3

**Transport Layer Security 1.3** is the cryptographic protocol securing Macula connections. In QUIC/HTTP3, TLS 1.3 is mandatory (not optional).

**Benefits:**
- Faster handshake (1-RTT)
- Forward secrecy
- Simplified cipher suites

---

### Realm Isolation

**Realm isolation** ensures that peers in different realms cannot:
- See each other's topics
- Call each other's procedures
- Access each other's DHT entries

Isolation is enforced cryptographically, not just by naming conventions.

---

## Configuration Terms

### sys.config

The **sys.config** file is the standard Erlang configuration file for OTP applications. Macula settings are configured here:

```erlang
[
  {macula, [
    {quic_port, 4433},
    {max_clients, 10000},
    {rpc_timeout_ms, 5000}
  ]}
].
```

---

### Environment Variables

Key environment variables for Macula:

| Variable | Description |
|----------|-------------|
| `MACULA_BOOTSTRAP_PEERS` | Comma-separated list of bootstrap node URLs |
| `MACULA_REALM` | Default realm for connections |
| `MACULA_QUIC_PORT` | QUIC listener port (default: 4433) |
| `MACULA_NODE_ID` | Fixed node ID (otherwise auto-generated) |

---

## Deprecated Terminology

The following terms are **outdated** and should not be used in new documentation:

| Deprecated Term | Current Term | Notes |
|-----------------|--------------|-------|
| "MuC" (Macula Micro Center) | **Macula Cluster** | Cluster is deployment-agnostic, not K8s-specific |
| "Macula Cell" | **Macula Cluster** | Cluster better conveys "group of nodes" semantics |
| "BSN" (Bootstrap Replica Node) | **Seed Node** | Seeds are just discoverable peers, no Raft |
| "Gateway to SuperMesh" | **Bridge Node** | Separate role from bootstrap |
| "Gateway mode" | N/A | All nodes have Gateway System (v0.8.5+) |
| "Edge peer mode" | N/A | All nodes are identical (v0.8.5+) |
| "Start gateway" | N/A | Gateway System always starts |
| "Bootstrap mode" | N/A | All nodes have Bootstrap System |
| "The gateway" (as a special node) | Seed Node | If referring to the initial entry point |
| "Bootstrap node" (as special node) | Seed Node | Seed node is preferred to avoid confusion |
| `MACULA_START_GATEWAY` | N/A | Environment variable no longer needed |
| "reckon_db integration" | **CRDTs + Gossip** | No external event store dependency |
| "Raft consensus" (for coordination) | **CRDTs** | No consensus needed for coordination state |

### Historical Context

**Pre-v0.8.5:** Macula had two deployment modes (Gateway mode vs Edge peer mode). Removed in v0.8.5 for always-on architecture.

**Pre-v0.13.0:** Architecture planned to use reckon_db (event store) for Platform Layer. Replaced in December 2025 with CRDTs + Gossip approach - simpler, no external dependencies.

**Migration to v0.13.0+:**
- MuC → Macula Cluster (terminology)
- BSN → Seed Node (no Raft, just discoverable peer)
- Add Bridge Nodes for cross-Cluster federation
- ETS registries → CRDT-backed with gossip sync

---

## See Also

- [Platform Overview](business/OVERVIEW.md) - High-level introduction
- [Quick Start](user/QUICK_START.md) - Getting started guide
- [RPC Guide](developer/RPC_GUIDE.md) - RPC patterns and usage
- [PubSub Guide](developer/PUBSUB_GUIDE.md) - Pub/Sub patterns and usage
- [Architecture Overview](../architecture/v0.8.0-OVERVIEW.md) - Technical architecture
