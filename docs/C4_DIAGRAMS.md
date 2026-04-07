# Macula HTTP/3 Mesh: C4 Architecture Diagrams

This document presents the C4 model diagrams for Macula HTTP/3 Mesh networking infrastructure.

The C4 model provides a hierarchical set of diagrams for visualizing software architecture:
1. **Level 1 - System Context**: How the system fits in the world
2. **Level 2 - Container**: High-level technology choices
3. **Level 3 - Component**: Components within containers
4. **Level 4 - Code**: Detailed class/module diagrams

---

## Level 1: System Context Diagram

**Scope:** The entire Macula Mesh ecosystem
**Primary Elements:** People and software systems
**Intended Audience:** Everyone (technical and non-technical)

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         System Context Diagram                              │
│                                                                             │
│                                                                             │
│  ┌──────────────────┐                                                      │
│  │                  │                                                      │
│  │   Application    │  Uses Macula Mesh for                               │
│  │   Developer      │  distributed communication                          │
│  │                  │                                                      │
│  └────────┬─────────┘                                                      │
│           │                                                                │
│           │ Develops and deploys                                          │
│           │ distributed applications                                      │
│           ↓                                                                │
│  ┌─────────────────────────────────────────────────────────────┐          │
│  │                                                             │          │
│  │            Macula HTTP/3 Mesh Network                       │          │
│  │                                                             │          │
│  │  Distributed mesh networking infrastructure for BEAM       │          │
│  │  applications. Provides:                                   │          │
│  │  • Self-organizing mesh topology                           │          │
│  │  • NAT/firewall traversal                                  │          │
│  │  • Distributed Erlang semantics over HTTP/3                │          │
│  │  • Pub/sub messaging (WAMP-compatible)                     │          │
│  │  • RPC (remote procedure calls)                            │          │
│  │                                                             │          │
│  └──┬─────────────┬─────────────┬────────────────────┬────────┘          │
│     │             │             │                    │                    │
│     │             │             │                    │                    │
│     ↓             ↓             ↓                    ↓                    │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐      ┌─────────────┐            │
│  │          │ │          │ │          │      │             │            │
│  │  Edge    │ │  Cloud   │ │  Mobile  │      │  IoT        │            │
│  │  Nodes   │ │  Nodes   │ │  Devices │      │  Devices    │            │
│  │          │ │          │ │          │      │             │            │
│  └────┬─────┘ └────┬─────┘ └────┬─────┘      └──────┬──────┘            │
│       │            │            │                    │                    │
│       │            │            │                    │                    │
│       │            │            │                    │                    │
│       └────────────┴────────────┴────────────────────┘                    │
│                              │                                            │
│                              │ All communication via                      │
│                              │ HTTP/3 (QUIC over UDP)                     │
│                              ↓                                            │
│                    ┌──────────────────────┐                              │
│                    │                      │                              │
│                    │  Internet / WAN      │                              │
│                    │                      │                              │
│                    │  • Firewalls         │                              │
│                    │  • NAT gateways      │                              │
│                    │  • Mobile networks   │                              │
│                    │                      │                              │
│                    └──────────────────────┘                              │
│                                                                           │
│  External Systems:                                                        │
│                                                                           │
│  ┌──────────────────┐          ┌──────────────────┐                     │
│  │                  │          │                  │                     │
│  │  STUN Servers    │          │  DNS Servers     │                     │
│  │  (NAT Discovery) │          │  (Node Discovery)│                     │
│  │                  │          │                  │                     │
│  └──────────────────┘          └──────────────────┘                     │
│         ▲                               ▲                                │
│         │                               │                                │
│         │ Uses for NAT traversal        │ Uses for service discovery     │
│         │                               │                                │
│         └───────────┬───────────────────┘                                │
│                     │                                                    │
│              Used by Macula Mesh                                         │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────────┘

Key Relationships:
─────────────────
Application Developer → Macula Mesh: Develops distributed BEAM apps
Macula Mesh → Edge/Cloud/Mobile/IoT: Deploys to heterogeneous nodes
Macula Mesh → Internet/WAN: Communicates over HTTP/3 (port 443/UDP)
Macula Mesh → STUN Servers: Discovers public IP addresses
Macula Mesh → DNS Servers: Discovers peer nodes (SRV records, mDNS)
```

---

## Level 2: Container Diagram

**Scope:** Macula Mesh system
**Primary Elements:** Containers (applications, data stores)
**Intended Audience:** Technical people (developers, architects)

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          Container Diagram                                  │
│                     Macula HTTP/3 Mesh Network                              │
│                                                                             │
│  ┌───────────────────────────────────────────────────────────────────────┐ │
│  │                                                                       │ │
│  │  Macula Node (BEAM VM)                                                │ │
│  │  Technology: Erlang/OTP 26+                                           │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  Application Layer                                              │ │ │
│  │  │  Container: Erlang/Elixir Applications                          │ │ │
│  │  │                                                                 │ │ │
│  │  │  • User applications (GenServers, Supervisors, etc.)           │ │ │
│  │  │  • Uses standard distributed Erlang API                        │ │ │
│  │  │  • spawn/2, send/2, monitor/2, link/2                          │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                              ↕                                        │ │
│  │                    Transparent Distribution                           │ │
│  │                              ↕                                        │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  Macula Mesh Services                                           │ │ │
│  │  │  Container: Elixir Applications (OTP)                           │ │ │
│  │  │                                                                 │ │ │
│  │  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐         │ │ │
│  │  │  │ Discovery    │  │ Membership   │  │ Topology     │         │ │ │
│  │  │  │ Service      │  │ Service      │  │ Manager      │         │ │ │
│  │  │  │              │  │              │  │              │         │ │ │
│  │  │  │ Bootstrap    │  │ SWIM Gossip  │  │ k-regular    │         │ │ │
│  │  │  │ mDNS         │  │ Failure Det. │  │ graph        │         │ │ │
│  │  │  │ DNS-SD       │  │              │  │              │         │ │ │
│  │  │  └──────────────┘  └──────────────┘  └──────────────┘         │ │ │
│  │  │                                                                 │ │ │
│  │  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐         │ │ │
│  │  │  │ Routing      │  │ Pub/Sub      │  │ RPC          │         │ │ │
│  │  │  │ Service      │  │ Registry     │  │ Handler      │         │ │ │
│  │  │  │              │  │              │  │              │         │ │ │
│  │  │  │ DHT          │  │ Topic-based  │  │ Sync/Async   │         │ │ │
│  │  │  │ Kademlia     │  │ Routing      │  │ Calls        │         │ │ │
│  │  │  │              │  │              │  │              │         │ │ │
│  │  │  └──────────────┘  └──────────────┘  └──────────────┘         │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                              ↕                                        │ │
│  │                    Macula Distribution Protocol                       │ │
│  │                              ↕                                        │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  Protocol Layer                                                 │ │ │
│  │  │  Container: Erlang Application                                  │ │ │
│  │  │                                                                 │ │ │
│  │  │  • Wire protocol (framing, encoding)                           │ │ │
│  │  │  • Handshake & authentication                                  │ │ │
│  │  │  • Stream multiplexing (process ↔ stream)                     │ │ │
│  │  │  • Message types: SEND, LINK, MONITOR, RPC, etc.              │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                              ↕                                        │ │
│  │                         QUIC Streams                                  │ │
│  │                              ↕                                        │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  QUIC Transport                                                 │ │ │
│  │  │  Container: quicer (Erlang NIF)                                 │ │ │
│  │  │  Technology: C (MsQuic library)                                 │ │ │
│  │  │                                                                 │ │ │
│  │  │  • RFC 9000 QUIC implementation                                │ │ │
│  │  │  • TLS 1.3 encryption (integrated)                             │ │ │
│  │  │  • Connection management                                       │ │ │
│  │  │  • Stream multiplexing                                         │ │ │
│  │  │  • Flow control, congestion control                            │ │ │
│  │  │  • 0-RTT connection resumption                                 │ │ │
│  │  │  • Connection migration                                        │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                              ↕                                        │ │
│  │                         UDP Port 443                                  │ │
│  │                              ↕                                        │ │
│  └───────────────────────────────────────────────────────────────────────┘ │
│                                 │                                         │
│                                 │ HTTP/3 (QUIC/UDP)                       │
│                                 │ Encrypted, Multiplexed                  │
│                                 ↓                                         │
│  ┌──────────────────────────────────────────────────────────────────┐    │
│  │                                                                  │    │
│  │  Network Infrastructure                                          │    │
│  │                                                                  │    │
│  │  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐ │    │
│  │  │                 │  │                 │  │                 │ │    │
│  │  │  NAT Gateway    │  │  Firewall       │  │  Load Balancer  │ │    │
│  │  │                 │  │                 │  │                 │ │    │
│  │  │  UDP port       │  │  Allows UDP     │  │  QUIC-aware     │ │    │
│  │  │  mapping        │  │  port 443       │  │  routing        │ │    │
│  │  │                 │  │                 │  │                 │ │    │
│  │  └─────────────────┘  └─────────────────┘  └─────────────────┘ │    │
│  │                                                                  │    │
│  └──────────────────────────────────────────────────────────────────┘    │
│                                 │                                         │
│                                 ↓                                         │
│  ┌──────────────────────────────────────────────────────────────────┐    │
│  │                                                                  │    │
│  │  Supporting Services (External)                                  │    │
│  │                                                                  │    │
│  │  ┌─────────────────┐           ┌─────────────────┐             │    │
│  │  │                 │           │                 │             │    │
│  │  │  STUN Server    │           │  DNS Server     │             │    │
│  │  │                 │           │                 │             │    │
│  │  │  Discovers NAT  │           │  Node discovery │             │    │
│  │  │  public address │           │  via SRV/mDNS   │             │    │
│  │  │                 │           │                 │             │    │
│  │  │  stun.l.google  │           │  Authoritative  │             │    │
│  │  │  .com:19302     │           │  or local       │             │    │
│  │  │                 │           │                 │             │    │
│  │  └─────────────────┘           └─────────────────┘             │    │
│  │                                                                  │    │
│  └──────────────────────────────────────────────────────────────────┘    │
│                                                                           │
│  Data Stores (In-Memory):                                                │
│                                                                           │
│  ┌─────────────────────────────────────────────────────────────────┐    │
│  │                                                                 │    │
│  │  ETS Tables (Erlang Term Storage)                               │    │
│  │  Technology: In-memory key-value store                          │    │
│  │                                                                 │    │
│  │  • membership_table: Known nodes and their states               │    │
│  │  • routing_table: DHT routing information                       │    │
│  │  • stream_registry: Stream ID → PID mapping                    │    │
│  │  • subscription_registry: Topic → Subscriber PIDs               │    │
│  │  • connection_pool: Active QUIC connections                     │    │
│  │                                                                 │    │
│  └─────────────────────────────────────────────────────────────────┘    │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────────┘

Technology Stack:
─────────────────
• Runtime: Erlang/OTP 26+, Elixir 1.15+
• Transport: quicer (NIF) → MsQuic (C library) → UDP
• Protocol: Custom Macula wire protocol over QUIC
• Encryption: TLS 1.3 (integrated in QUIC)
• Storage: ETS (in-memory), no persistent database
• Discovery: mDNS (local), DNS-SD (cloud), Bootstrap nodes
```

---

## Level 3: Component Diagram - Mesh Services Container

**Scope:** Mesh Services container from Level 2
**Primary Elements:** Components (Erlang/Elixir modules and GenServers)
**Intended Audience:** Developers and architects

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          Component Diagram                                  │
│                      Mesh Services Container                                │
│                                                                             │
│  ┌───────────────────────────────────────────────────────────────────────┐ │
│  │                                                                       │ │
│  │  macula_sup (Supervisor)                                              │ │
│  │  Component: OTP Supervisor                                            │ │
│  │  Responsibility: Top-level supervision tree                           │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  Discovery Subsystem                                            │ │ │
│  │  │                                                                 │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  macula_discovery (GenServer)                            │  │ │ │
│  │  │  │  Module: macula_discovery.erl                            │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Responsibilities:                                       │  │ │ │
│  │  │  │  • Coordinate multi-strategy node discovery              │  │ │ │
│  │  │  │  • Periodic discovery (every 30 sec)                     │  │ │ │
│  │  │  │  • Merge results from all strategies                     │  │ │ │
│  │  │  │  • Notify membership service of new nodes                │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Uses:                                                   │  │ │ │
│  │  │  │  → macula_bootstrap                                      │  │ │ │
│  │  │  │  → macula_mdns                                           │  │ │ │
│  │  │  │  → macula_dns_srv                                        │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                          │                                     │ │ │
│  │  │                          ↓                                     │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │  macula_bootstrap                                        │  │ │ │
│  │  │  │  Module: macula_bootstrap.erl                            │  │ │ │
│  │  │  │  • Query bootstrap nodes (config or DNS)                 │  │ │ │
│  │  │  │  • Ping nodes to verify availability                     │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                                                                 │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │  macula_mdns                                             │  │ │ │
│  │  │  │  Module: macula_mdns.erl                                 │  │ │ │
│  │  │  │  • Send mDNS queries (_macula._udp.local)                │  │ │ │
│  │  │  │  • Listen for mDNS responses                             │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                                                                 │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │  macula_dns_srv                                          │  │ │ │
│  │  │  │  Module: macula_dns_srv.erl                              │  │ │ │
│  │  │  │  • Query DNS SRV records                                 │  │ │ │
│  │  │  │  • Parse SRV responses (host:port)                       │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  Membership Subsystem                                           │ │ │
│  │  │                                                                 │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  macula_membership (GenServer)                           │  │ │ │
│  │  │  │  Module: macula_membership.erl                           │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Responsibilities:                                       │  │ │ │
│  │  │  │  • Maintain cluster membership view                      │  │ │ │
│  │  │  │  • SWIM protocol tick (every 1 sec)                      │  │ │ │
│  │  │  │  • Failure detection (ping/ack)                          │  │ │ │
│  │  │  │  • Suspicion mechanism (avoid false positives)           │  │ │ │
│  │  │  │  • Gossip dissemination                                  │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  State:                                                  │  │ │ │
│  │  │  │  • local_member: This node's info                        │  │ │ │
│  │  │  │  • members: Map of node_id → member record               │  │ │ │
│  │  │  │  • protocol_period: 1000ms                               │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Uses:                                                   │  │ │ │
│  │  │  │  → macula_connection (for ping/gossip)                   │  │ │ │
│  │  │  │  → membership_table (ETS)                                │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                          │                                     │ │ │
│  │  │                          │ Updates                             │ │ │
│  │  │                          ↓                                     │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │  membership_table (ETS)                                  │  │ │ │
│  │  │  │  Type: set, public                                       │  │ │ │
│  │  │  │  Schema: {node_id, member_record}                        │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  Topology Subsystem                                             │ │ │
│  │  │                                                                 │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  macula_topology (GenServer)                             │  │ │ │
│  │  │  │  Module: macula_topology.erl                             │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Responsibilities:                                       │  │ │ │
│  │  │  │  • Maintain k-regular graph topology (k=6)               │  │ │ │
│  │  │  │  • Select neighbors via consistent hashing               │  │ │ │
│  │  │  │  • Connect to missing neighbors                          │  │ │ │
│  │  │  │  • Disconnect from extra neighbors                       │  │ │ │
│  │  │  │  • Periodic maintenance (every 5 sec)                    │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Algorithm:                                              │  │ │ │
│  │  │  │  1. Hash all members onto ring                           │  │ │ │
│  │  │  │  2. Select K/2 clockwise neighbors                       │  │ │ │
│  │  │  │  3. Select K/2 counter-clockwise neighbors               │  │ │ │
│  │  │  │  4. Establish/tear down connections                      │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Uses:                                                   │  │ │ │
│  │  │  │  → macula_membership (get members)                       │  │ │ │
│  │  │  │  → macula_connection (connect/disconnect)                │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  Routing Subsystem                                              │ │ │
│  │  │                                                                 │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  macula_routing (Module)                                 │  │ │ │
│  │  │  │  Module: macula_routing.erl                              │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Responsibilities:                                       │  │ │ │
│  │  │  │  • Route messages to destination nodes                   │  │ │ │
│  │  │  │  • DHT-based routing (Kademlia-inspired)                 │  │ │ │
│  │  │  │  • Find next hop via XOR distance metric                 │  │ │ │
│  │  │  │  • Iterative node lookup (find_node/1)                   │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Functions:                                              │  │ │ │
│  │  │  │  • route(DestNodeId, Message) → ok                       │  │ │ │
│  │  │  │  • find_next_hop(DestNodeId) → NextHopNodeId             │  │ │ │
│  │  │  │  • find_node(TargetId) → {ok, [NodeIds]}                 │  │ │ │
│  │  │  │  • xor_distance(A, B) → Integer                          │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Uses:                                                   │  │ │ │
│  │  │  │  → macula_topology (get neighbors)                       │  │ │ │
│  │  │  │  → macula_connection (send/forward)                      │  │ │ │
│  │  │  │  → routing_table (ETS)                                   │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                          │                                     │ │ │
│  │  │                          │ Updates                             │ │ │
│  │  │                          ↓                                     │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │  routing_table (ETS)                                     │  │ │ │
│  │  │  │  Type: ordered_set, public                               │  │ │ │
│  │  │  │  Schema: {distance, node_id, metadata}                   │  │ │ │
│  │  │  │  Purpose: k-buckets for DHT routing                      │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  Pub/Sub Subsystem                                              │ │ │
│  │  │                                                                 │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  macula_pubsub (GenServer)                               │  │ │ │
│  │  │  │  Module: macula_pubsub.erl                               │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Responsibilities:                                       │  │ │ │
│  │  │  │  • Manage topic subscriptions                            │  │ │ │
│  │  │  │  • Route publications to subscribers                     │  │ │ │
│  │  │  │  • Gossip subscription info across mesh                  │  │ │ │
│  │  │  │  • Pattern matching (exact, prefix, wildcard)            │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Functions:                                              │  │ │ │
│  │  │  │  • subscribe(Topic, Pid, Options) → SubscriptionId       │  │ │ │
│  │  │  │  • publish(Topic, Data) → PublicationId                  │  │ │ │
│  │  │  │  • unsubscribe(SubscriptionId) → ok                      │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Uses:                                                   │  │ │ │
│  │  │  │  → macula_topic_router (content-based routing)           │  │ │ │
│  │  │  │  → subscription_registry (ETS)                           │  │ │ │
│  │  │  │  → macula_routing (for remote delivery)                  │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                          │                                     │ │ │
│  │  │                          ↓                                     │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │  macula_topic_router (Module)                            │  │ │ │
│  │  │  │  • Trie-based topic matching                             │  │ │ │
│  │  │  │  • Find interested nodes for topic                       │  │ │ │
│  │  │  │  • Efficient prefix/wildcard matching                    │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                          │                                     │ │ │
│  │  │                          ↓                                     │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │  subscription_registry (ETS)                             │  │ │ │
│  │  │  │  Type: bag, public                                       │  │ │ │
│  │  │  │  Schema: {topic, subscriber_pid, options}                │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  RPC Subsystem                                                  │ │ │
│  │  │                                                                 │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  macula_rpc (Module)                                     │  │ │ │
│  │  │  │  Module: macula_rpc.erl                                  │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Responsibilities:                                       │  │ │ │
│  │  │  │  • Synchronous RPC calls                                 │  │ │ │
│  │  │  │  • Request/response matching via ReqId                   │  │ │ │
│  │  │  │  • Timeout handling                                      │  │ │ │
│  │  │  │  • Error propagation                                     │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Functions:                                              │  │ │ │
│  │  │  │  • call(Node, M, F, A) → Result | {error, Reason}        │  │ │ │
│  │  │  │  • call(Node, M, F, A, Timeout) → Result                 │  │ │ │
│  │  │  │  • handle_rpc(Request, FromNode) → ok                    │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Uses:                                                   │  │ │ │
│  │  │  │  → macula_routing (route RPC messages)                   │  │ │ │
│  │  │  │  → macula_protocol (encode RPC frames)                   │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  Connection Management Subsystem                                │ │ │
│  │  │                                                                 │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  macula_connection_sup (Supervisor)                      │  │ │ │
│  │  │  │  Type: simple_one_for_one                                │  │ │ │
│  │  │  │  Child: macula_connection (GenServer)                    │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Spawns one GenServer per remote node connection         │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                          │                                     │ │ │
│  │  │                          │ Supervises                          │ │ │
│  │  │                          ↓                                     │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  macula_connection (GenServer)                           │  │ │ │
│  │  │  │  Module: macula_connection.erl                           │  │ │ │
│  │  │  │  One instance per remote node                            │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Responsibilities:                                       │  │ │ │
│  │  │  │  • Manage QUIC connection to one remote node             │  │ │ │
│  │  │  │  • Stream pool management                                │  │ │ │
│  │  │  │  • Handshake on connect                                  │  │ │ │
│  │  │  │  • Heartbeat/keepalive                                   │  │ │ │
│  │  │  │  • Reconnection on failure                               │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  State:                                                  │  │ │ │
│  │  │  │  • node_id: Remote node identifier                       │  │ │ │
│  │  │  │  • quic_conn: QUIC connection handle                     │  │ │ │
│  │  │  │  • streams: Map of purpose → stream_id                   │  │ │ │
│  │  │  │  • state: connecting | active | closing                  │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Functions:                                              │  │ │ │
│  │  │  │  • connect(NodeId) → {ok, Pid} | {error, Reason}         │  │ │ │
│  │  │  │  • disconnect(NodeId) → ok                               │  │ │ │
│  │  │  │  • send(NodeId, Message) → ok                            │  │ │ │
│  │  │  │  • ping(NodeId, Timeout) → pong | timeout                │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Uses:                                                   │  │ │ │
│  │  │  │  → quicer (NIF for QUIC transport)                       │  │ │ │
│  │  │  │  → macula_protocol (framing)                             │  │ │ │
│  │  │  │  → macula_handshake (auth)                               │  │ │ │
│  │  │  │  → stream_registry (ETS)                                 │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                          │                                     │ │ │
│  │  │                          │ Updates                             │ │ │
│  │  │                          ↓                                     │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │  stream_registry (ETS)                                   │  │ │ │
│  │  │  │  Type: set, public                                       │  │ │ │
│  │  │  │  Schema: {stream_id, pid, purpose}                       │  │ │ │
│  │  │  │  Purpose: Map QUIC streams to Erlang processes           │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  └───────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│  Key Relationships:                                                        │
│  ───────────────────                                                       │
│  • Discovery → Membership: Notify of discovered nodes                     │
│  • Membership → Topology: Provide member list for neighbor selection      │
│  • Topology → Connection: Establish/teardown connections                  │
│  • Routing → Connection: Send messages via connections                    │
│  • PubSub → Routing: Route publications to remote subscribers             │
│  • RPC → Routing: Route RPC requests/responses                            │
│  • All subsystems → ETS tables: Read/write shared state                   │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Level 3: Component Diagram - Protocol Layer Container

**Scope:** Protocol Layer container from Level 2
**Primary Elements:** Protocol modules and data structures
**Intended Audience:** Developers implementing the protocol

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          Component Diagram                                  │
│                        Protocol Layer Container                             │
│                                                                             │
│  ┌───────────────────────────────────────────────────────────────────────┐ │
│  │                                                                       │ │
│  │  Protocol Layer                                                       │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  macula_protocol (Module)                                       │ │ │
│  │  │  Module: macula_protocol.erl                                    │ │ │
│  │  │                                                                 │ │ │
│  │  │  Responsibilities:                                              │ │ │
│  │  │  • Encode/decode wire protocol messages                         │ │ │
│  │  │  • Frame format: [Ver|Type|Flags|Len|Payload]                  │ │ │
│  │  │  • Message validation                                           │ │ │
│  │  │                                                                 │ │ │
│  │  │  Functions:                                                     │ │ │
│  │  │  • encode(Type, Payload) → Binary                              │ │ │
│  │  │  • decode(Binary) → {ok, Message, Rest} | {error, Reason}     │ │ │
│  │  │  • encode_send(FromPid, ToPid, Msg) → Binary                   │ │ │
│  │  │  • encode_link(Pid1, Pid2) → Binary                            │ │ │
│  │  │  • encode_monitor(Pid, Ref, MonPid) → Binary                   │ │ │
│  │  │                                                                 │ │ │
│  │  │  Message Types:                                                 │ │ │
│  │  │  ┌────────────────────────────────────────────────────────┐    │ │ │
│  │  │  │ Type   │ Name            │ Description                │    │ │ │
│  │  │  ├────────┼─────────────────┼────────────────────────────┤    │ │ │
│  │  │  │ 0x01   │ HANDSHAKE       │ Initial connection setup   │    │ │ │
│  │  │  │ 0x02   │ HEARTBEAT       │ Keepalive                  │    │ │ │
│  │  │  │ 0x03   │ SEND            │ Send to PID                │    │ │ │
│  │  │  │ 0x04   │ REG_SEND        │ Send to registered name    │    │ │ │
│  │  │  │ 0x05   │ EXIT            │ Process exit signal        │    │ │ │
│  │  │  │ 0x06   │ LINK            │ Link processes             │    │ │ │
│  │  │  │ 0x07   │ UNLINK          │ Unlink processes           │    │ │ │
│  │  │  │ 0x08   │ MONITOR         │ Monitor process            │    │ │ │
│  │  │  │ 0x09   │ DEMONITOR       │ Demonitor                  │    │ │ │
│  │  │  │ 0x0A   │ GROUP_LEADER    │ Group leader ops           │    │ │ │
│  │  │  │ 0x0B   │ RPC             │ RPC call                   │    │ │ │
│  │  │  │ 0x0C   │ SPAWN_REQUEST   │ Remote spawn               │    │ │ │
│  │  │  │ 0x0D   │ SPAWN_REPLY     │ Spawn result               │    │ │ │
│  │  │  └────────────────────────────────────────────────────────┘    │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  macula_handshake (Module)                                      │ │ │
│  │  │  Module: macula_handshake.erl                                   │ │ │
│  │  │                                                                 │ │ │
│  │  │  Responsibilities:                                              │ │ │
│  │  │  • Perform connection handshake                                 │ │ │
│  │  │  • Exchange capabilities                                        │ │ │
│  │  │  • Verify protocol compatibility                                │ │ │
│  │  │  • Challenge-response authentication                            │ │ │
│  │  │                                                                 │ │ │
│  │  │  Functions:                                                     │ │ │
│  │  │  • perform(Conn) → {ok, RemoteHandshake} | {error, Reason}    │ │ │
│  │  │  • accept(Conn) → {ok, RemoteHandshake} | {error, Reason}     │ │ │
│  │  │  • check_compatibility(Local, Remote) → ok | {error, Reason}  │ │ │
│  │  │                                                                 │ │ │
│  │  │  Handshake Record:                                              │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │ -record(handshake, {                                     │  │ │ │
│  │  │  │     version = 1,                                         │  │ │ │
│  │  │  │     node_name :: atom(),                                 │  │ │ │
│  │  │  │     node_id :: binary(),  % SHA256(cert)                 │  │ │ │
│  │  │  │     capabilities = [],    % [compression, rpc, ...]     │  │ │ │
│  │  │  │     creation :: integer(),% Node start time              │  │ │ │
│  │  │  │     challenge :: binary() % Random bytes for auth        │  │ │ │
│  │  │  │ }).                                                       │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                                                                 │ │ │
│  │  │  Handshake Flow:                                                │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  │  Client                         Server                   │  │ │ │
│  │  │  │    │                                │                    │  │ │ │
│  │  │  │    │  1. HANDSHAKE (client info)   │                    │  │ │ │
│  │  │  │    ├───────────────────────────────►│                    │  │ │ │
│  │  │  │    │                                │                    │  │ │ │
│  │  │  │    │  2. HANDSHAKE (server info)   │                    │  │ │ │
│  │  │  │    │◄───────────────────────────────┤                    │  │ │ │
│  │  │  │    │                                │                    │  │ │ │
│  │  │  │    │  3. ACK (challenge response)   │                    │  │ │ │
│  │  │  │    ├───────────────────────────────►│                    │  │ │ │
│  │  │  │    │                                │                    │  │ │ │
│  │  │  │    │  4. Connection ready           │                    │  │ │ │
│  │  │  │    │◄══════════════════════════════►│                    │  │ │ │
│  │  │  │                                                          │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  macula_stream_mux (Module)                                     │ │ │
│  │  │  Module: macula_stream_mux.erl                                  │ │ │
│  │  │                                                                 │ │ │
│  │  │  Responsibilities:                                              │ │ │
│  │  │  • Multiplex multiple logical streams over QUIC connection      │ │ │
│  │  │  • Map Erlang processes to QUIC streams                         │ │ │
│  │  │  • Stream lifecycle management                                  │ │ │
│  │  │                                                                 │ │ │
│  │  │  Functions:                                                     │ │ │
│  │  │  • open_stream(Conn, Pid) → {ok, StreamId}                     │ │ │
│  │  │  • close_stream(StreamId) → ok                                 │ │ │
│  │  │  • send_on_stream(StreamId, Data) → ok                         │ │ │
│  │  │  • recv_from_stream(StreamId, Timeout) → {ok, Data}           │ │ │
│  │  │  • route_stream_data(StreamId, Data) → ok                      │ │ │
│  │  │                                                                 │ │ │
│  │  │  Stream Purposes:                                               │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │ control   : Control messages (handshake, heartbeat)      │  │ │ │
│  │  │  │ messaging : Process messages (SEND, EXIT, etc.)          │  │ │ │
│  │  │  │ rpc       : RPC calls and responses                      │  │ │ │
│  │  │  │ pubsub    : Pub/sub events                               │  │ │ │
│  │  │  │ gossip    : Membership gossip                            │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  macula_identity (Module)                                       │ │ │
│  │  │  Module: macula_identity.erl                                    │ │ │
│  │  │                                                                 │ │ │
│  │  │  Responsibilities:                                              │ │ │
│  │  │  • Generate node identity (SHA256 of certificate)               │ │ │
│  │  │  • Manage node metadata                                         │ │ │
│  │  │  • Derive connection IDs                                        │ │ │
│  │  │                                                                 │ │ │
│  │  │  Functions:                                                     │ │ │
│  │  │  • node_id() → Binary  (SHA256 hash)                           │ │ │
│  │  │  • address() → {Host, Port}                                    │ │ │
│  │  │  • metadata() → Map                                            │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  macula_cert (Module)                                           │ │ │
│  │  │  Module: macula_cert.erl                                        │ │ │
│  │  │                                                                 │ │ │
│  │  │  Responsibilities:                                              │ │ │
│  │  │  • Generate TLS certificates                                    │ │ │
│  │  │  • Load certificates from disk                                  │ │ │
│  │  │  • Certificate rotation                                         │ │ │
│  │  │                                                                 │ │ │
│  │  │  Functions:                                                     │ │ │
│  │  │  • generate_self_signed(CommonName) → {Cert, Key}              │ │ │
│  │  │  • generate_node_cert(NodeName) → {Cert, Key}                  │ │ │
│  │  │  • load_cert(Path) → {ok, Cert} | {error, Reason}             │ │ │
│  │  │  • verify_cert(Cert) → ok | {error, Reason}                    │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  └───────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│  Data Flow Example (SEND message):                                        │
│  ──────────────────────────────────────                                   │
│                                                                             │
│  1. User code: RemotePid ! Message                                         │
│  2. macula_dist intercepts send                                            │
│  3. macula_protocol:encode_send(self(), RemotePid, Message) → Binary       │
│  4. macula_stream_mux:send_on_stream(messaging_stream, Binary)             │
│  5. quicer:send(Stream, Binary)                                            │
│  6. ═══► Network (QUIC/UDP/TLS 1.3)                                        │
│  7. Remote: quicer receives on stream                                      │
│  8. macula_stream_mux:route_stream_data(StreamId, Binary)                  │
│  9. macula_protocol:decode(Binary) → {ok, {send, FromPid, ToPid, Msg}}    │
│ 10. macula_dist:deliver(ToPid, FromPid, Msg)                               │
│ 11. ToPid receives Message in mailbox                                      │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Level 4: Code Diagram - Connection Management

**Scope:** macula_connection GenServer (one component from Level 3)
**Primary Elements:** Detailed class structure, functions, state
**Intended Audience:** Developers implementing this component

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                            Code Diagram                                     │
│                  macula_connection GenServer                                │
│                                                                             │
│  ┌───────────────────────────────────────────────────────────────────────┐ │
│  │                                                                       │ │
│  │  macula_connection                                                    │ │
│  │  File: macula_connection.erl                                          │ │
│  │  Type: gen_server                                                     │ │
│  │  Purpose: Manage QUIC connection to one remote node                   │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  State Record                                                   │ │ │
│  │  │  ────────────────                                               │ │ │
│  │  │                                                                 │ │ │
│  │  │  -record(state, {                                               │ │ │
│  │  │      node_id       :: binary(),     % Remote node ID (SHA256)  │ │ │
│  │  │      node_name     :: atom(),       % Remote node name          │ │ │
│  │  │      address       :: {string(), integer()}, % Host:Port        │ │ │
│  │  │      quic_conn     :: reference(),  % QUIC connection handle    │ │ │
│  │  │      streams = #{} :: #{atom() => reference()},                 │ │ │
│  │  │                       % Map: purpose → stream_id                │ │ │
│  │  │      state         :: connecting | active | closing,            │ │ │
│  │  │      created_at    :: integer(),    % Timestamp (millisec)      │ │ │
│  │  │      last_activity :: integer(),    % Last send/recv time       │ │ │
│  │  │      heartbeat_timer :: reference() | undefined,                │ │ │
│  │  │      reconnect_attempts = 0 :: non_neg_integer(),               │ │ │
│  │  │      stats = #{} :: #{atom() => integer()}                      │ │ │
│  │  │                       % Counters: bytes_sent, bytes_recv, etc.  │ │ │
│  │  │  }).                                                             │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  Public API                                                     │ │ │
│  │  │  ──────────                                                     │ │ │
│  │  │                                                                 │ │ │
│  │  │  -spec start_link(NodeId, Address) ->                          │ │ │
│  │  │      {ok, Pid} | {error, Reason}.                              │ │ │
│  │  │  %% Start connection GenServer for remote node                 │ │ │
│  │  │                                                                 │ │ │
│  │  │  -spec connect(NodeId) ->                                      │ │ │
│  │  │      {ok, Pid} | {error, Reason}.                              │ │ │
│  │  │  %% Connect to remote node (may start new GenServer)           │ │ │
│  │  │                                                                 │ │ │
│  │  │  -spec disconnect(NodeId) -> ok.                               │ │ │
│  │  │  %% Gracefully disconnect from remote node                     │ │ │
│  │  │                                                                 │ │ │
│  │  │  -spec send(NodeId, Message) ->                                │ │ │
│  │  │      ok | {error, Reason}.                                     │ │ │
│  │  │  %% Send message to remote node                                │ │ │
│  │  │                                                                 │ │ │
│  │  │  -spec ping(NodeId, Timeout) ->                                │ │ │
│  │  │      pong | timeout.                                           │ │ │
│  │  │  %% Ping remote node (SWIM protocol)                           │ │ │
│  │  │                                                                 │ │ │
│  │  │  -spec is_connected(NodeId) -> boolean().                      │ │ │
│  │  │  %% Check if connected to remote node                          │ │ │
│  │  │                                                                 │ │ │
│  │  │  -spec get_stats(NodeId) ->                                    │ │ │
│  │  │      {ok, #{atom() => integer()}} | {error, Reason}.           │ │ │
│  │  │  %% Get connection statistics                                  │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  gen_server Callbacks                                           │ │ │
│  │  │  ────────────────────                                           │ │ │
│  │  │                                                                 │ │ │
│  │  │  init([NodeId, Address]) ->                                    │ │ │
│  │  │      {ok, State} | {stop, Reason}.                             │ │ │
│  │  │                                                                 │ │ │
│  │  │  Actions:                                                       │ │ │
│  │  │  1. Initialize state record                                    │ │ │
│  │  │  2. Trigger async connect                                      │ │ │
│  │  │     gen_server:cast(self(), connect)                           │ │ │
│  │  │  3. Return {ok, State}                                         │ │ │
│  │  │                                                                 │ │ │
│  │  ├─────────────────────────────────────────────────────────────────┤ │ │
│  │  │                                                                 │ │ │
│  │  │  handle_call({send, Message}, From, State) ->                  │ │ │
│  │  │      {reply, Reply, NewState}.                                 │ │ │
│  │  │                                                                 │ │ │
│  │  │  Actions:                                                       │ │ │
│  │  │  1. Get or create messaging stream                             │ │ │
│  │  │  2. quicer:send(Stream, Message)                               │ │ │
│  │  │  3. Update stats (bytes_sent, messages_sent)                   │ │ │
│  │  │  4. Update last_activity timestamp                             │ │ │
│  │  │  5. Reply {ok, NewState}                                       │ │ │
│  │  │                                                                 │ │ │
│  │  ├─────────────────────────────────────────────────────────────────┤ │ │
│  │  │                                                                 │ │ │
│  │  │  handle_call(ping, From, State) ->                             │ │ │
│  │  │      {reply, Reply, NewState}.                                 │ │ │
│  │  │                                                                 │ │ │
│  │  │  Actions:                                                       │ │ │
│  │  │  1. Encode HEARTBEAT message                                   │ │ │
│  │  │  2. Send on control stream                                     │ │ │
│  │  │  3. Wait for response (1 sec timeout)                          │ │ │
│  │  │  4. Reply pong or timeout                                      │ │ │
│  │  │                                                                 │ │ │
│  │  ├─────────────────────────────────────────────────────────────────┤ │ │
│  │  │                                                                 │ │ │
│  │  │  handle_cast(connect, State) ->                                │ │ │
│  │  │      {noreply, NewState}.                                      │ │ │
│  │  │                                                                 │ │ │
│  │  │  Actions:                                                       │ │ │
│  │  │  1. Parse address (Host, Port)                                 │ │ │
│  │  │  2. Build QUIC connection options                              │ │ │
│  │  │     ConnOpts = #{                                              │ │ │
│  │  │         alpn => ["macula/1.0"],                                │ │ │
│  │  │         verify => verify_peer,                                 │ │ │
│  │  │         idle_timeout_ms => 30000                               │ │ │
│  │  │     }                                                           │ │ │
│  │  │  3. {ok, Conn} = quicer:connect(Host, Port, ConnOpts, 5000)   │ │ │
│  │  │  4. Perform handshake                                          │ │ │
│  │  │     {ok, RemoteHandshake} = macula_handshake:perform(Conn)     │ │ │
│  │  │  5. Create initial streams (control, messaging)                │ │ │
│  │  │  6. Start heartbeat timer (every 5 sec)                        │ │ │
│  │  │  7. Update state:                                              │ │ │
│  │  │     - quic_conn = Conn                                         │ │ │
│  │  │     - state = active                                           │ │ │
│  │  │     - created_at = now()                                       │ │ │
│  │  │  8. Notify topology manager (connected)                        │ │ │
│  │  │                                                                 │ │ │
│  │  ├─────────────────────────────────────────────────────────────────┤ │ │
│  │  │                                                                 │ │ │
│  │  │  handle_cast(disconnect, State) ->                             │ │ │
│  │  │      {noreply, NewState}.                                      │ │ │
│  │  │                                                                 │ │ │
│  │  │  Actions:                                                       │ │ │
│  │  │  1. Cancel heartbeat timer                                     │ │ │
│  │  │  2. Close all streams gracefully                               │ │ │
│  │  │     maps:foreach(fun(_Purpose, Stream) ->                      │ │ │
│  │  │         quicer:close_stream(Stream)                            │ │ │
│  │  │     end, State#state.streams)                                  │ │ │
│  │  │  3. Close QUIC connection                                      │ │ │
│  │  │     quicer:close_connection(State#state.quic_conn)             │ │ │
│  │  │  4. Update state:                                              │ │ │
│  │  │     - state = closing                                          │ │ │
│  │  │     - quic_conn = undefined                                    │ │ │
│  │  │  5. Notify topology manager (disconnected)                     │ │ │
│  │  │                                                                 │ │ │
│  │  ├─────────────────────────────────────────────────────────────────┤ │ │
│  │  │                                                                 │ │ │
│  │  │  handle_info(heartbeat, State) ->                              │ │ │
│  │  │      {noreply, NewState}.                                      │ │ │
│  │  │                                                                 │ │ │
│  │  │  Actions:                                                       │ │ │
│  │  │  1. Send HEARTBEAT message                                     │ │ │
│  │  │  2. Check last_activity                                        │ │ │
│  │  │     If idle > 60 sec:                                          │ │ │
│  │  │       Log warning, may initiate disconnect                     │ │ │
│  │  │  3. Schedule next heartbeat (5 sec)                            │ │ │
│  │  │                                                                 │ │ │
│  │  ├─────────────────────────────────────────────────────────────────┤ │ │
│  │  │                                                                 │ │ │
│  │  │  handle_info({quic, stream_closed, StreamId}, State) ->        │ │ │
│  │  │      {noreply, NewState}.                                      │ │ │
│  │  │                                                                 │ │ │
│  │  │  Actions:                                                       │ │ │
│  │  │  1. Find purpose of closed stream                              │ │ │
│  │  │  2. Remove from streams map                                    │ │ │
│  │  │  3. If critical stream (control), initiate reconnect           │ │ │
│  │  │                                                                 │ │ │
│  │  ├─────────────────────────────────────────────────────────────────┤ │ │
│  │  │                                                                 │ │ │
│  │  │  handle_info({quic, connection_closed}, State) ->              │ │ │
│  │  │      {noreply, NewState}.                                      │ │ │
│  │  │                                                                 │ │ │
│  │  │  Actions:                                                       │ │ │
│  │  │  1. Log connection closure                                     │ │ │
│  │  │  2. Clear all streams                                          │ │ │
│  │  │  3. Attempt reconnection (exponential backoff)                 │ │ │
│  │  │     Delay = min(1000 * 2^reconnect_attempts, 30000)           │ │ │
│  │  │     erlang:send_after(Delay, self(), reconnect)                │ │ │
│  │  │  4. Update state:                                              │ │ │
│  │  │     - state = connecting                                       │ │ │
│  │  │     - reconnect_attempts += 1                                  │ │ │
│  │  │                                                                 │ │ │
│  │  ├─────────────────────────────────────────────────────────────────┤ │ │
│  │  │                                                                 │ │ │
│  │  │  handle_info(reconnect, State) ->                              │ │ │
│  │  │      {noreply, NewState}.                                      │ │ │
│  │  │                                                                 │ │ │
│  │  │  Actions:                                                       │ │ │
│  │  │  1. Trigger async connect                                      │ │ │
│  │  │     gen_server:cast(self(), connect)                           │ │ │
│  │  │                                                                 │ │ │
│  │  ├─────────────────────────────────────────────────────────────────┤ │ │
│  │  │                                                                 │ │ │
│  │  │  terminate(Reason, State) -> ok.                               │ │ │
│  │  │                                                                 │ │ │
│  │  │  Actions:                                                       │ │ │
│  │  │  1. Close all streams                                          │ │ │
│  │  │  2. Close QUIC connection                                      │ │ │
│  │  │  3. Cancel timers                                              │ │ │
│  │  │  4. Log termination                                            │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  Private Helper Functions                                       │ │ │
│  │  │  ────────────────────────                                       │ │ │
│  │  │                                                                 │ │ │
│  │  │  get_or_create_stream(Purpose, State) ->                       │ │ │
│  │  │      {ok, StreamId, NewState}.                                 │ │ │
│  │  │                                                                 │ │ │
│  │  │  lookup_connection(NodeId) ->                                  │ │ │
│  │  │      {ok, Pid} | {error, not_found}.                           │ │ │
│  │  │                                                                 │ │ │
│  │  │  register_connection(NodeId, Pid) -> ok.                       │ │ │
│  │  │                                                                 │ │ │
│  │  │  update_stats(Stat, Value, State) ->                           │ │ │
│  │  │      NewState.                                                 │ │ │
│  │  │                                                                 │ │ │
│  │  │  parse_address({Host, Port}) ->                                │ │ │
│  │  │      {ok, Host, Port} | {error, Reason}.                       │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  Dependencies                                                   │ │ │
│  │  │  ────────────                                                   │ │ │
│  │  │                                                                 │ │ │
│  │  │  Uses:                                                          │ │ │
│  │  │  • quicer - QUIC transport (NIF)                                │ │ │
│  │  │  • macula_handshake - Connection handshake                      │ │ │
│  │  │  • macula_protocol - Message encoding/decoding                  │ │ │
│  │  │  • macula_identity - Node identity                              │ │ │
│  │  │                                                                 │ │ │
│  │  │  Used by:                                                       │ │ │
│  │  │  • macula_topology - Topology management                        │ │ │
│  │  │  • macula_routing - Message routing                             │ │ │
│  │  │  • macula_membership - SWIM ping/gossip                         │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │                                                                 │ │ │
│  │  │  State Transitions                                              │ │ │
│  │  │  ─────────────────                                              │ │ │
│  │  │                                                                 │ │ │
│  │  │     ┌──────────────┐                                            │ │ │
│  │  │     │              │  init/1                                    │ │ │
│  │  │     │  undefined   ├──────────────┐                             │ │ │
│  │  │     │              │              │                             │ │ │
│  │  │     └──────────────┘              ↓                             │ │ │
│  │  │                           ┌───────────────┐                     │ │ │
│  │  │                           │               │                     │ │ │
│  │  │              ┌────────────│  connecting   │◄─────────┐          │ │ │
│  │  │              │            │               │          │          │ │ │
│  │  │              │            └───────┬───────┘          │          │ │ │
│  │  │   connection │                   │ handshake        │          │ │ │
│  │  │     lost     │                   │ success          │          │ │ │
│  │  │              │                   ↓                  │ reconnect│ │ │
│  │  │              │            ┌──────────────┐          │          │ │ │
│  │  │              │            │              │          │          │ │ │
│  │  │              └───────────►│    active    │──────────┘          │ │ │
│  │  │                           │              │                     │ │ │
│  │  │                           └──────┬───────┘                     │ │ │
│  │  │                                  │ disconnect                  │ │ │
│  │  │                                  ↓                             │ │ │
│  │  │                           ┌──────────────┐                     │ │ │
│  │  │                           │              │                     │ │ │
│  │  │                           │   closing    │                     │ │ │
│  │  │                           │              │                     │ │ │
│  │  │                           └──────────────┘                     │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  └───────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Supplementary Diagram: Deployment View

**Scope:** Physical/logical deployment of Macula Mesh across infrastructure
**Primary Elements:** Nodes, networks, deployment units
**Intended Audience:** DevOps, infrastructure teams

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         Deployment Diagram                                  │
│                  Macula Mesh Network - Physical View                        │
│                                                                             │
│                                                                             │
│  ┌───────────────────────────────────────────────────────────────────────┐ │
│  │  Edge Location 1 (On-Premises / Home)                                │ │
│  │  Network: 192.168.1.0/24 (Private, behind NAT)                       │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │  NAT Router                                                     │ │ │
│  │  │  Public IP: 203.0.113.10                                        │ │ │
│  │  │  UDP Port 443 → 192.168.1.100:4433                              │ │ │
│  │  └─────────────────────┬───────────────────────────────────────────┘ │ │
│  │                        │                                             │ │
│  │                        ↓                                             │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │  Edge Node A                                                    │ │ │
│  │  │  Hostname: edge-a.home.local                                    │ │ │
│  │  │  Local IP: 192.168.1.100:4433                                   │ │ │
│  │  │                                                                 │ │ │
│  │  │  Hardware:                                                      │ │ │
│  │  │  • Raspberry Pi 4 (4GB RAM)                                     │ │ │
│  │  │  • Ubuntu Server 22.04                                          │ │ │
│  │  │                                                                 │ │ │
│  │  │  Software:                                                      │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │ Erlang/OTP 26.2                                          │  │ │ │
│  │  │  │  ┌────────────────────────────────────────────────────┐  │  │ │ │
│  │  │  │  │ Macula Mesh Node                                   │  │  │ │ │
│  │  │  │  │  • Role: Edge node                                 │  │  │ │ │
│  │  │  │  │  • Apps: IoT sensor aggregation                    │  │  │ │ │
│  │  │  │  └────────────────────────────────────────────────────┘  │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  └───────────────────────────────────────────────────────────────────────┘ │
│                                 │                                         │
│                                 │ HTTP/3 (QUIC)                           │
│                                 │ Over Internet                           │
│                                 ↓                                         │
│  ┌───────────────────────────────────────────────────────────────────────┐ │
│  │  Cloud Region 1 (AWS us-east-1)                                      │ │
│  │  VPC: 10.0.0.0/16                                                    │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │  Internet Gateway                                               │ │ │
│  │  │  Elastic IP: 54.123.45.67                                       │ │ │
│  │  └─────────────────────┬───────────────────────────────────────────┘ │ │
│  │                        │                                             │ │
│  │                        ↓                                             │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │  EC2 Instance (t3.medium)                                       │ │ │
│  │  │  Private IP: 10.0.1.10:4433                                     │ │ │
│  │  │  Public IP: 54.123.45.67:443                                    │ │ │
│  │  │                                                                 │ │ │
│  │  │  Software:                                                      │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │ Docker                                                   │  │ │ │
│  │  │  │  ┌────────────────────────────────────────────────────┐  │  │ │ │
│  │  │  │  │ macula/node:latest                                 │  │  │ │ │
│  │  │  │  │  • Erlang/OTP 26.2                                 │  │  │ │ │
│  │  │  │  │  • Macula Mesh                                     │  │  │ │ │
│  │  │  │  │  • Role: Hub node                                  │  │  │ │ │
│  │  │  │  │  • Apps: API gateway, analytics                    │  │  │ │ │
│  │  │  │  └────────────────────────────────────────────────────┘  │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  └───────────────────────────────────────────────────────────────────────┘ │
│                                 │                                         │
│                                 │ Private network                         │
│                                 │ (or HTTP/3)                             │
│                                 ↓                                         │
│  ┌───────────────────────────────────────────────────────────────────────┐ │
│  │  Edge Location 2 (Mobile Device)                                     │ │
│  │  Network: Cellular (dynamic IP, CGNAT)                               │ │
│  │                                                                       │ │
│  │  ┌─────────────────────────────────────────────────────────────────┐ │ │
│  │  │  Mobile Device (Android/iOS)                                    │ │ │
│  │  │  IP: 10.20.30.40 (carrier NAT, changes frequently)              │ │ │
│  │  │                                                                 │ │ │
│  │  │  Software:                                                      │ │ │
│  │  │  ┌──────────────────────────────────────────────────────────┐  │ │ │
│  │  │  │ Erlang/OTP 26 (via Termux or native)                     │  │ │ │
│  │  │  │  ┌────────────────────────────────────────────────────┐  │  │ │ │
│  │  │  │  │ Macula Mesh Node                                   │  │  │ │ │
│  │  │  │  │  • Role: Mobile edge node                          │  │  │ │ │
│  │  │  │  │  • Apps: Real-time chat, location sharing          │  │  │ │ │
│  │  │  │  │  • Connection migration: WiFi ↔ Cellular           │  │  │ │ │
│  │  │  │  └────────────────────────────────────────────────────┘  │  │ │ │
│  │  │  └──────────────────────────────────────────────────────────┘  │ │ │
│  │  │                                                                 │ │ │
│  │  └─────────────────────────────────────────────────────────────────┘ │ │
│  │                                                                       │ │
│  └───────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│                                                                             │
│  Supporting Infrastructure:                                                │
│  ──────────────────────────                                                │
│                                                                             │
│  ┌───────────────────────────────────────────────────────────────────────┐ │
│  │  Bootstrap Nodes (DNS SRV)                                            │ │
│  │  DNS: _macula._udp.example.com                                        │ │
│  │                                                                       │ │
│  │  SRV Records:                                                         │ │
│  │  • 10 10 4433 edge-hub-1.example.com  (54.123.45.67)                 │ │
│  │  • 10 10 4433 edge-hub-2.example.com  (52.98.76.54)                  │ │
│  │                                                                       │ │
│  └───────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│  ┌───────────────────────────────────────────────────────────────────────┐ │
│  │  STUN Server                                                          │ │
│  │  Server: stun.l.google.com:19302 (public)                            │ │
│  │                                                                       │ │
│  │  Purpose: NAT public address discovery                               │ │
│  │                                                                       │ │
│  └───────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│                                                                             │
│  Network Flows:                                                            │
│  ──────────────                                                            │
│                                                                             │
│  1. Edge Node A ──STUN───► stun.l.google.com                              │
│     Discovers public IP: 203.0.113.10                                     │
│                                                                             │
│  2. Edge Node A ──DNS SRV──► _macula._udp.example.com                     │
│     Gets bootstrap nodes: [54.123.45.67:4433, ...]                        │
│                                                                             │
│  3. Edge Node A ──HTTP/3──► Cloud Hub (54.123.45.67:443)                  │
│     Establishes QUIC connection                                            │
│     Handshake, join mesh                                                   │
│                                                                             │
│  4. Cloud Hub ──SWIM Gossip──► Edge Node A                                │
│     Shares membership: [Mobile Device, Edge Node B, ...]                   │
│                                                                             │
│  5. Edge Node A ──HTTP/3──► Mobile Device                                 │
│     Attempts direct connection (may use ICE/STUN)                          │
│     Falls back to relay via Cloud Hub if NAT too restrictive               │
│                                                                             │
│  6. Application message routing:                                           │
│     Edge A → Cloud Hub → Mobile (3 hops)                                  │
│     or                                                                     │
│     Edge A → Mobile (direct, 1 hop if NAT traversed)                      │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Conclusion

These C4 diagrams provide a comprehensive view of the Macula HTTP/3 Mesh architecture at multiple levels of abstraction:

1. **Level 1 (System Context)**: Shows how Macula Mesh fits in the broader ecosystem
2. **Level 2 (Container)**: Details the technology stack and major containers
3. **Level 3 (Component)**: Deep dive into mesh services and protocol layer components
4. **Level 4 (Code)**: Detailed implementation of the connection management component
5. **Supplementary (Deployment)**: Physical deployment across edge, cloud, and mobile

**Key Architectural Decisions:**

- **QUIC Transport**: HTTP/3 over UDP for NAT traversal and performance
- **Self-Organizing Mesh**: SWIM gossip + k-regular graph topology
- **O(log N) Routing**: Kademlia DHT for scalable message routing
- **Layered Design**: Clean separation between transport, protocol, mesh services, and applications
- **OTP Supervision**: Fault-tolerant supervision trees for all components
- **In-Memory State**: ETS tables for shared state (no persistent database)

---

**Document Version:** 1.0
**Created:** 2025-11-07
**Related Document:** `macula_http3_mesh_roadmap.md`
**Author:** Macula Architecture Team
