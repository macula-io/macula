# Erlang Distribution Over Relay Mesh

**Status:** EXPERIMENTAL (v0.40.0+)

Macula can tunnel OTP distribution through the relay mesh, enabling
`net_adm:ping`, `gen_server:call`, `pg` groups, and process monitoring
between nodes that only have outbound connectivity to a relay.

## When to Use This

| Scenario | Use |
|----------|-----|
| Nodes on same LAN | Direct QUIC (`-proto_dist macula`) |
| Nodes behind NATs/firewalls | **Relay mesh** (`MACULA_DIST_MODE=relay`) |
| Nodes in different datacenters | **Relay mesh** |
| Mnesia replication | Direct QUIC (relay adds latency) |

## Architecture

Each side creates a `gen_tcp` loopback socket pair. One end goes to
OTP's `dist_util` (which needs a real file descriptor). The other end
is bridged to the relay mesh via pub/sub topics.

![Relay Tunnel Architecture](assets/dist_relay_tunnel.svg)

### Data Flow

```
Node A (connecting)                    Node B (accepting)
                                       
dist_util                              dist_util
  |  f_send/f_recv                       |  f_send/f_recv
  v                                      v
DistSock (gen_tcp, {packet,2->4})      DistSock (gen_tcp, {packet,2->4})
  | TCP loopback                         | TCP loopback
  v                                      v
BridgeSock (gen_tcp, {packet,raw})     BridgeSock (gen_tcp, {packet,raw})
  |                                      |
  +-- bridge_reader --> relay pub/sub ---+-- bridge_writer --> BridgeSock
  +-- bridge_writer <-- relay pub/sub ---+-- bridge_reader --> relay pub/sub
```

### Packet Framing

During handshake, `dist_util` uses `{packet, 2}` (2-byte length prefix).
After handshake, it switches to `{packet, 4}`. The BridgeSock uses
`{packet, raw}` — a transparent byte pipe that forwards raw bytes
including length headers. This way the bridge doesn't need to know
which framing mode dist_util is currently using.

## Quick Start

### 1. Start with relay mode

```
erl -proto_dist macula \
    -no_epmd \
    -start_epmd false \
    -name mynode@myhost \
    -setcookie MYCOOKIE
```

### 2. In your application startup

```erlang
%% Connect to a relay
{ok, Client} = macula_relay_client:start_link(#{
    relays => [<<"https://relay.example.com:4433">>],
    realm => <<"my.realm">>,
    identity => <<"mynode">>,
    tls_verify => none
}),

%% Enable relay distribution
os:putenv("MACULA_DIST_MODE", "relay"),
macula_dist_relay:register_mesh_client(Client),
macula_dist_relay:advertise_dist_accept(),
```

### 3. Connect to a remote node

```erlang
net_adm:ping('othernode@otherhost').
%% => pong
```

## Tunnel Lifecycle

1. **Node A** calls `net_adm:ping('B@host')` which triggers `macula_dist:setup/5`
2. `macula_dist_relay:connect/3` sends a mesh RPC to `_dist.tunnel.B@host`
3. **Node B** receives the RPC, creates a tunnel with a unique ID
4. Both sides create `gen_tcp` loopback pairs and bridge processes
5. `dist_util` handshake flows through the relay tunnel (5 messages)
6. Post-handshake, the connection enters `con_loop` with tick keepalive
7. Distribution traffic (messages, calls, monitors) flows through the bridge

## Limitations

- **Latency**: Every message goes through the relay (2 extra hops)
- **Mnesia**: Not recommended over WAN latency
- **global module**: May have convergence issues over high-latency links
- **Throughput**: Limited by relay bandwidth and pub/sub overhead
- **Single relay**: Currently both nodes must be connected to the same relay mesh

## Security

Tunnel bytes are encrypted with AES-256-GCM. The key is derived from the
Erlang distribution cookie (`erlang:get_cookie()`), which both nodes must
share anyway for the dist handshake to succeed.

- Each message gets a random 12-byte nonce (prepended to ciphertext)
- The relay sees encrypted bytes — it cannot read ETF content
- A wrong cookie produces `decrypt_failed` warnings in the bridge logs

This is not a substitute for mutual TLS authentication. It protects against
a compromised or curious relay reading distribution traffic.

## Metrics

Each tunnel tracks byte and message counters:

```erlang
macula_dist_relay:get_tunnel_metrics().
%% => [{<<"abc123">>, #{bytes_out => 4096, bytes_in => 2048,
%%                       msgs_out => 12, msgs_in => 8}}]
```

## net_ticktime

The default `net_ticktime` of 60 seconds means OTP sends a tick every 15s
and declares a node DOWN after 4 missed ticks. Over a relay with WAN latency,
this can cause false disconnects.

**Recommendation:** increase to 120s for relay mode:

```
erl -kernel net_ticktime 120 -proto_dist macula ...
```

The `BRIDGE_RECV_TIMEOUT` (60s) should be >= `net_ticktime` to avoid the
bridge timing out before OTP detects the node is down.

## Troubleshooting

### ping returns pang

1. Check both nodes have `MACULA_DIST_MODE=relay` set
2. Check both nodes called `register_mesh_client/1` and `advertise_dist_accept/0`
3. Check both nodes are connected to the same relay
4. Check cookies match (`-setcookie`)
5. Check node names match exactly (case-sensitive)

### Connection drops after handshake

1. Check `bridge_reader_loop` timeout — default is 60s
2. Verify relay pub/sub is working (test with a regular subscribe/publish)
3. Check for firewall rules blocking localhost loopback

## Configuration

| Environment Variable | Default | Description |
|---------------------|---------|-------------|
| `MACULA_DIST_MODE` | (unset) | Set to `relay` to enable relay distribution |

| Module Define | Value | Description |
|---------------|-------|-------------|
| `DIST_TIMEOUT` | 25000ms | Tunnel negotiation RPC timeout |
| `BRIDGE_RECV_TIMEOUT` | 60000ms | Bridge reader socket timeout |
| `CONTROLLER_TIMEOUT` | 30000ms | Kernel controller assignment timeout |
