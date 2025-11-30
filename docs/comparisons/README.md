# How Macula Compares to Similar Systems

Distributed networking is not new, and several excellent projects tackle similar problems. Here's how Macula differs:

## vs. libp2p (IPFS Networking Stack)

**[libp2p](https://libp2p.io/)** is the modular networking stack behind IPFS and Filecoin.

- **What it is**: A comprehensive peer-to-peer networking library with many transport options, NAT traversal, and discovery mechanisms
- **Maintained by**: Protocol Labs
- **Key difference**: libp2p is a **library** you integrate into your application. Macula is a **platform** providing complete pub/sub and RPC primitives built specifically for the BEAM (Erlang/Elixir) ecosystem
- **When to use libp2p**: Building file-sharing applications or integrating with the IPFS ecosystem
- **When to use Macula**: Building business applications, IoT systems, or collaborative AI on Erlang/Elixir with built-in service discovery, multi-tenancy, and OTP supervision

## vs. Distributed Erlang

**[Distributed Erlang](https://www.erlang.org/doc/reference_manual/distributed.html)** is Erlang's built-in clustering.

- **What it is**: Native clustering for Erlang nodes with transparent process messaging
- **Maintained by**: Ericsson (part of Erlang/OTP)
- **Key difference**: Distributed Erlang requires **full mesh connectivity** (every node connects to every other node) and doesn't work through NAT/firewalls. Macula uses **HTTP/3 (QUIC)** for NAT-friendly transport and **Kademlia DHT routing** for O(log N) scalability without full mesh connectivity
- **When to use Distributed Erlang**: Datacenter deployments with full network control and trusted environments
- **When to use Macula**: Edge deployments, IoT networks, or any scenario involving NAT, firewalls, or untrusted networks

## vs. Akka Cluster (JVM)

**[Akka Cluster](https://doc.akka.io/docs/akka/current/typed/cluster.html)** provides distributed actor systems for the JVM.

- **What it is**: Clustering and distributed messaging for Scala/Java applications using the Actor model
- **Maintained by**: Lightbend
- **Key difference**: Akka runs on the **JVM** and uses TCP with gossip protocols. Macula runs on **BEAM** (Erlang VM) and uses **HTTP/3 (QUIC)** for modern, efficient transport with built-in encryption and NAT traversal
- **When to use Akka**: JVM-based applications requiring distributed actors
- **When to use Macula**: Erlang/Elixir applications requiring edge-friendly networking and standards-based transport

## vs. Kubernetes (Orchestration)

**[Kubernetes](https://kubernetes.io/)** orchestrates containerized applications at scale.

- **What it is**: Container orchestration platform for deploying and managing microservices
- **Maintained by**: Cloud Native Computing Foundation (CNCF)
- **Key difference**: Kubernetes **orchestrates centralized infrastructure** (datacenters). Macula **enables peer-to-peer decentralized networks** at the edge. They solve different problems
- **When to use Kubernetes**: Deploying microservices in datacenters or cloud environments
- **When to use Macula**: Building peer-to-peer applications where nodes discover and communicate directly, without central orchestration

## vs. WebRTC (Browser P2P)

**[WebRTC](https://webrtc.org/)** enables peer-to-peer communication in web browsers.

- **What it is**: Browser APIs for real-time video, audio, and data channels between peers
- **Maintained by**: W3C and browser vendors
- **Key difference**: WebRTC targets **browser-to-browser** communication for media streaming. Macula targets **server-to-server** and **device-to-device** communication for business applications, IoT, and AI systems
- **When to use WebRTC**: Real-time video/audio in web browsers
- **When to use Macula**: Backend services, IoT devices, and edge computing platforms

## Macula's Unique Position

Macula combines ideas from these systems but targets a specific niche:

✅ **BEAM-native** (Erlang/Elixir OTP supervision and fault tolerance)
✅ **HTTP/3 (QUIC)** transport (modern, encrypted, NAT-friendly)
✅ **Edge-first design** (works through firewalls and NAT)
✅ **Built-in pub/sub & RPC** (no external message broker needed)
✅ **Multi-tenancy** (realm isolation for SaaS and shared infrastructure)
✅ **Self-organizing mesh** (DHT-based service discovery, O(log N) routing)
✅ **Production-ready patterns** (OTP behaviors, comprehensive testing, memory management)

If you're building **decentralized Erlang/Elixir applications** that need to work in **real-world network conditions** (edge, IoT, hybrid cloud), Macula provides the infrastructure layer you need.

---

**[← Back to Documentation](../README.md)** | **[Use Cases →](../business/USE_CASES.md)**
