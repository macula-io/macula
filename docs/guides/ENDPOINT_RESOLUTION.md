# Endpoint Resolution Guide

How macula nodes discover and reach each other across LANs, NATs, and the internet.

![Endpoint Resolution](assets/endpoint_resolution.svg)

---

## The Core Problem

A macula node needs to tell other nodes "here's how to reach me." Today, we store a single endpoint URL in the DHT (e.g., `https://host00.lab:9444`). This breaks in three ways:

1. **LAN hostnames aren't resolvable** from other networks
2. **Nodes behind NAT** have private IPs unreachable from outside
3. **IP addresses change** (DHCP, mobile, ISP reassignment)

Every serious P2P system separates **identity** (who you are) from **reachability** (how to reach you right now). We need to do the same.

---

## Design: Identity vs Reachability

### Identity (Stable)

A node's identity is the SHA-256 hash of its Ed25519 public key. This is a 32-byte binary that serves as the Kademlia key. It never changes regardless of IP address, network location, or transport.

```
Node ID = SHA-256(Ed25519 public key)
        = 32-byte binary
        = Kademlia routing key
```

The node ID is already used throughout macula for DHT routing, peer tracking, and message addressing. No change needed here.

### Reachability (Transient)

A node can be reached via **multiple endpoints simultaneously**. Each endpoint is a transport-specific URL with a priority and TTL. The set of endpoints changes as the node moves between networks, discovers its public IP, or gains/loses relay connections.

```
Endpoints = [
    #{url => <<"quic://host00.lab:9444">>,    type => lan,    priority => 1, ttl => 300},
    #{url => <<"quic://192.168.1.5:9444">>,   type => lan,    priority => 2, ttl => 300},
    #{url => <<"quic://83.102.45.7:9444">>,   type => public, priority => 3, ttl => 300},
    #{url => <<"relay://boot.macula.io">>,     type => relay,  priority => 10, ttl => 600}
]
```

---

## DHT Store Format

### Current (Single Endpoint)

```erlang
#{
    node_id => <<32 bytes>>,
    endpoint => <<"https://host00.lab:9444">>,
    ttl => 300
}
```

### Proposed (Multiple Endpoints)

```erlang
#{
    node_id => <<32 bytes>>,
    endpoints => [
        #{url => <<"quic://host00.lab:9444">>,  type => lan,    priority => 1},
        #{url => <<"quic://83.102.45.7:9444">>, type => public, priority => 3},
        #{url => <<"relay://boot.macula.io">>,  type => relay,  priority => 10}
    ],
    ttl => 300
}
```

The `endpoint` field is replaced by `endpoints` (list). Consumers try all endpoints in priority order, racing the connections. First successful connection wins; others are cancelled.

---

## Endpoint Discovery Layers

A node discovers its own reachable endpoints through four mechanisms, layered from most local to most global.

### Layer 1: Local Configuration

The node knows its listen port and can resolve its hostname. This gives the first endpoint.

```
Source: net_adm:localhost(), MACULA_HOSTNAME, HOSTNAME env
Result: quic://host00.lab:9444
Scope:  LAN only
```

### Layer 2: mDNS / Erlang Distribution

For nodes on the same LAN segment, mDNS or Erlang distribution provides direct discovery without the DHT. These endpoints are added to the local list but not necessarily published to the DHT (they're only useful to same-segment peers).

```
Source: mDNS broadcast, Erlang net_kernel
Result: quic://192.168.1.5:9444
Scope:  LAN segment only
```

### Layer 3: Observed Address (like AutoNAT/STUN)

When a node connects to a public peer (e.g., the bootstrap), the remote peer sees the node's public IP. The remote peer reports this back in the PONG message or via a dedicated observed-address exchange.

```
Source: Remote peer reports "I see you as 83.102.45.7:9444"
Result: quic://83.102.45.7:9444
Scope:  Internet (if not behind symmetric NAT)
```

This is equivalent to STUN in WebRTC or AutoNAT in libp2p. Multiple peers should confirm the observed address to avoid poisoning.

### Layer 4: Relay Registration

If the node cannot establish direct connections (symmetric NAT, firewall), it registers with a relay-capable peer. The relay endpoint is added to the endpoint list as a fallback.

```
Source: Relay registration with publicly reachable peer
Result: relay://boot.macula.io/node-id
Scope:  Internet (guaranteed, but higher latency)
```

---

## Connect Race Strategy

When Peer A wants to reach Peer B, it looks up B's node ID in the DHT and gets B's endpoint list. A then attempts connections to ALL endpoints simultaneously:

```
1. Try quic://host00.lab:9444      (LAN — fastest if on same network)
2. Try quic://83.102.45.7:9444     (Public IP — works across internet)
3. Try relay://boot.macula.io      (Relay — guaranteed but slowest)
```

The first successful QUIC handshake wins. All other connection attempts are cancelled. This ensures:

- **LAN peers get LAN-speed connections** (no detour through bootstrap)
- **Internet peers get direct connections** when possible
- **NAT'd peers still work** via relay fallback
- **No single point of failure** — if one endpoint is down, others compensate

### Connection Timeout

Each endpoint gets a configurable timeout (default: 3 seconds for direct, 5 seconds for relay). If all endpoints fail, the connection attempt fails.

---

## Observed Address Protocol

The observed address is how a node behind NAT learns its public IP. The protocol is simple:

1. Node A connects to Node B (a public peer)
2. B sees A's source IP and port from the QUIC connection
3. B includes `observed_addr: "83.102.45.7:9444"` in the PONG response
4. A adds `quic://83.102.45.7:9444` to its endpoint list (type: public)
5. A re-publishes its subscription STOREs with the updated endpoint list

### Verification

A single peer's observation could be wrong (NAT hairpinning, malicious peer). Before adding an observed address to the DHT:

- Require at least 2 independent peers to report the same address
- Or verify via a dedicated probe (peer C tries to connect to the observed address)

This matches libp2p's AutoNAT approach, which achieved ~70% NAT traversal success in a 2025 study across 85,000 networks.

---

## Relay Protocol

When direct connections fail, traffic routes through a relay node. The relay never sees plaintext — QUIC encryption is end-to-end between the two peers.

### Relay Registration

```
1. Node A (behind NAT) connects to Relay R (publicly reachable)
2. A sends: RELAY_REGISTER {node_id: A, ttl: 600}
3. R stores: A is reachable through me
4. A adds relay://R/A-node-id to its endpoint list
```

### Relayed Connection

```
1. Node B wants to reach A
2. B tries direct endpoints (fail — A is behind NAT)
3. B connects to R: RELAY_CONNECT {target: A-node-id}
4. R bridges the QUIC streams: B <-> R <-> A
5. Both peers see a normal QUIC connection (higher latency)
```

### Relay Nodes

Any publicly reachable macula node can be a relay. The bootstrap server (`boot.macula.io`) is the default relay. In a mesh of 10,000+ nodes, many will be publicly reachable and can share the relay load.

Relay capacity is bounded per-node (max concurrent relays, max bandwidth) to prevent abuse.

---

## Comparison With Other Systems

| Aspect | libp2p (IPFS) | Yggdrasil | Tailscale | Macula (proposed) |
|--------|--------------|-----------|-----------|-------------------|
| Identity | PeerID = hash(pubkey) | IPv6 = f(pubkey) | WG pubkey | NodeID = hash(pubkey) |
| Address format | Multiaddr (composable) | IPv6 (overlay) | IP (assigned) | Endpoint list (simple) |
| Discovery | Kademlia DHT | Spanning tree + DHT | Coord server | Kademlia DHT |
| NAT traversal | AutoNAT + DCUtR + Relay | Overlay sidesteps NAT | DISCO + DERP | Observed addr + Relay |
| Central dependency | None | None | Coord server | None |
| Transport | TCP, QUIC, WebSocket | TCP/TLS overlay | WireGuard/UDP | QUIC (HTTP/3) |

### Why Not Full Overlay (Yggdrasil)?

Yggdrasil routes ALL traffic through a spanning tree overlay, which adds latency to every packet. Macula uses direct QUIC connections when possible (LAN, public IP) and only falls back to relay when necessary. This gives lower latency for the common case.

### Why Not Coordination Server (Tailscale)?

Tailscale's coordination server is centralized — it distributes keys and endpoint information. This contradicts macula's decentralization goal. The DHT provides the same functionality without a central authority.

---

## Migration Path

### Phase 1: Multi-Endpoint Store Format

Change subscription STORE values from single `endpoint` to `endpoints` list. Maintain backward compatibility by accepting both formats during transition.

### Phase 2: Observed Address

Add observed address reporting in PONG messages. Nodes learn their public IP and add it to their endpoint list.

### Phase 3: Connect Race

Implement parallel connection attempts to all endpoints. First QUIC handshake wins.

### Phase 4: Relay Protocol

Add relay registration and relayed connection support. Bootstrap server becomes the default relay.

---

## Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Address format | Simple endpoint list (not multiaddr) | Macula only uses QUIC — no need for multi-transport composability |
| Observed address verification | 2+ peers must agree | Prevents single-peer address poisoning |
| Relay selection | Closest publicly reachable peer | Minimizes relay latency |
| Endpoint TTL | 5 minutes (direct), 10 minutes (relay) | Balances freshness with DHT load |
| Race timeout | 3s direct, 5s relay | Aggressive enough for UX, patient enough for WAN |

---

## References

- [libp2p Addressing and Identify Protocol](https://docs.libp2p.io/concepts/fundamentals/addressing/)
- [libp2p AutoNAT](https://docs.libp2p.io/concepts/nat/autonat/)
- [libp2p Circuit Relay v2](https://libp2p.io/docs/circuit-relay/)
- [Large Scale NAT Traversal Study (2025)](https://arxiv.org/html/2510.27500v1) — 70% hole-punch success across 85,000 networks
- [Yggdrasil Addressing](https://yggdrasil-network.github.io/2018/07/28/addressing.html)
- [Tailscale: How it works](https://tailscale.com/blog/how-tailscale-works)
- [Kademlia Paper](https://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf)
