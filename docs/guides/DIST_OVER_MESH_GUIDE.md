# Erlang Distribution Over Relay Mesh

**Status:** Production-ready for messaging workloads (v0.42.0+)

Macula can tunnel OTP distribution through the relay mesh, enabling
`net_adm:ping`, `gen_server:call`, `pg` groups, and process monitoring
between nodes that only have outbound connectivity to a relay.

## When to Use This

| Scenario | Use |
|----------|-----|
| Nodes on same LAN | Direct QUIC (`-proto_dist macula`) |
| Nodes behind NATs/firewalls | **Relay mesh** (`MACULA_DIST_MODE=relay`) |
| Nodes in different datacenters | **Relay mesh** |
| Nodes on different relays | **Relay mesh** (cross-relay via peering) |
| Mnesia replication | Direct QUIC (relay adds latency) |

## Architecture

Each tunnel is managed by a supervised `macula_dist_bridge` gen_server
under `macula_dist_bridge_sup` (simple_one_for_one). The bridge owns a
gen_tcp loopback socket pair — one end goes to OTP's `dist_util`, the
other is bridged to the relay mesh via encrypted pub/sub topics.

![Relay Tunnel Architecture](assets/dist_relay_tunnel.svg)

### Supervision Tree

```
macula_dist_system (one_for_one)
  ├── macula_dist_bridge_sup (simple_one_for_one)
  │     └── macula_dist_bridge (gen_server, per tunnel, temporary)
  │           ├── owns BridgeSock (gen_tcp, {packet, raw})
  │           ├── linked reader process (gen_tcp:recv → encrypt → publish)
  │           ├── handle_info: tunnel_in → decrypt → gen_tcp:send
  │           ├── monitors relay client (reconnects on DOWN)
  │           └── per-tunnel counters (bytes/msgs in/out)
  ├── macula_dist_discovery (DHT node discovery)
  └── macula_cluster_strategy (optional auto-clustering)
```

When `macula_dist_system` is not started (standalone relay mode),
`advertise_dist_accept/0` starts `macula_dist_bridge_sup` on demand.

### Data Flow

```
Node A (connecting)                    Node B (accepting)

dist_util                              dist_util
  |  f_send/f_recv                       |  f_send/f_recv
  v                                      v
DistSock (gen_tcp, {packet,2→4})       DistSock (gen_tcp, {packet,2→4})
  | TCP loopback                         | TCP loopback
  v                                      v
BridgeSock (gen_tcp, {packet,raw})     BridgeSock (gen_tcp, {packet,raw})
  |                                      |
  +-- reader → encrypt → relay ─────────+── writer → decrypt → BridgeSock
  +-- writer ← decrypt ← relay ─────────+── reader → encrypt → relay
```

### Packet Framing

During handshake, `dist_util` uses `{packet, 2}` (2-byte length prefix).
After handshake, it switches to `{packet, 4}`. The BridgeSock uses
`{packet, raw}` — a transparent byte pipe that forwards raw bytes
including length headers. The bridge doesn't need to know which framing
mode dist_util is currently using.

### Encryption

All tunnel bytes are encrypted with AES-256-GCM before being published
to the relay. The key is derived from the Erlang distribution cookie:

```
Key = SHA-256("macula-dist-tunnel:" ++ cookie)
```

Each message gets a random 12-byte nonce (prepended to ciphertext).
The relay sees encrypted bytes — it cannot read ETF content. A wrong
cookie produces `decrypt_failed` warnings in the bridge logs.

This is not a substitute for mutual TLS authentication. It protects
against a compromised or curious relay reading distribution traffic.

## Quick Start

### 1. Start with relay mode

```
erl -proto_dist macula \
    -no_epmd \
    -start_epmd false \
    -kernel net_ticktime 120 \
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

%% All standard OTP distribution primitives work:
gen_server:call({Name, 'othernode@otherhost'}, Request).
Pid ! Message.
pg:join(Group, self()).
monitor(process, {Name, 'othernode@otherhost'}).
```

## Tunnel Lifecycle

1. **Node A** calls `net_adm:ping('B@host')` which triggers `macula_dist:setup/5`
2. `macula_dist_relay:connect/3` sends a mesh RPC to `_dist.tunnel.B@host`
3. For cross-relay: `call_any` tries each connected relay until one succeeds
4. **Node B** receives the RPC, creates a tunnel with a unique ID
5. Both sides create `gen_tcp` loopback pairs
6. A supervised `macula_dist_bridge` gen_server is started for each side
7. Socket ownership is transferred to the bridge and dist controller
8. `dist_util` handshake flows through the encrypted relay tunnel (5 messages)
9. Post-handshake, the connection enters `con_loop` with tick keepalive

## Cross-Relay Tunnels

Nodes on **different relays** can connect if the relays are peered.
When the tunnel RPC (`_dist.tunnel.{node}`) is not found on the local
relay, `macula_multi_relay:call_any/4` tries each connected relay in
sequence. The first success establishes the tunnel.

```
Node A → relay-it-milan (Nuremberg) ──peering──► relay-se-stockholm (Helsinki) → Node B
```

The tunnel data (pub/sub topics) also flows through relay peering — Bloom
filter routing ensures `_dist.data.*` topics are forwarded between relays.

## Relay Reconnection

![Relay Failover](assets/relay_failover.svg)

If the relay client process dies (QUIC drop, relay restart), the bridge
gen_server detects the `DOWN` monitor signal and:

1. Attempts to re-acquire a relay client from `persistent_term`
2. Re-subscribes to tunnel topics on the new client
3. Restarts the reader process with the new client
4. Retries every 2s, up to 15 attempts (30s window)

If the relay reconnects within `net_ticktime` (recommended: 120s),
the distribution connection survives transparently.

The reader handles publish failures with retry (3 attempts, 1s backoff).

## Metrics

Each tunnel tracks byte and message counters:

```erlang
macula_dist_relay:get_tunnel_metrics().
%% => [{<<"abc123">>, #{bytes_out => 4096, bytes_in => 2048,
%%                       msgs_out => 12, msgs_in => 8}}]

macula_dist_relay:get_tunnel_metrics(<<"abc123">>).
%% => #{bytes_out => 4096, bytes_in => 2048, ...}
```

## net_ticktime

The default `net_ticktime` of 60 seconds means OTP sends a tick every 15s
and declares a node DOWN after 4 missed ticks. Over a relay with WAN
latency, this can cause false disconnects.

**Recommendation:** increase to 120s for relay mode:

```
erl -kernel net_ticktime 120 -proto_dist macula ...
```

## Limitations

- **Latency**: Every message goes through the relay (2 extra hops)
- **Mnesia**: Not recommended over WAN latency
- **global module**: May have convergence issues over high-latency links
- **Throughput**: Limited by relay bandwidth and pub/sub overhead

## Troubleshooting

### ping returns pang

1. Check both nodes have `MACULA_DIST_MODE=relay` set
2. Check both nodes called `register_mesh_client/1` and `advertise_dist_accept/0`
3. Check both nodes are connected to relays in the same peered mesh
4. Check cookies match (`-setcookie`)
5. Check node names match exactly (case-sensitive)

### Connection drops after handshake

1. Increase `net_ticktime` to 120+ (`-kernel net_ticktime 120`)
2. Verify relay pub/sub is working (test with a regular subscribe/publish)
3. Check `macula_dist_relay:get_tunnel_metrics()` for active tunnels

### Bridge dies immediately (reader_exit, normal)

Socket ownership was not transferred. Ensure the process that creates
the loopback pair calls `gen_tcp:controlling_process/2` to transfer
both DistSock and BridgeSock before exiting.

## Configuration

| Environment Variable | Default | Description |
|---------------------|---------|-------------|
| `MACULA_DIST_MODE` | (unset) | Set to `relay` to enable relay distribution |

| Module Define | Value | Description |
|---------------|-------|-------------|
| `DIST_TIMEOUT` | 25000ms | Tunnel negotiation RPC timeout |
| `BRIDGE_RECV_TIMEOUT` | 60000ms | Bridge reader/writer timeout |
| `CONTROLLER_TIMEOUT` | 30000ms | Kernel controller assignment timeout |
| `BACKPRESSURE_HWM` | 64 | Relay client queue depth before pause |
| `RECONNECT_INTERVAL` | 2000ms | Retry interval when relay client is down |
| `RECONNECT_MAX_ATTEMPTS` | 15 | Max retries (15 × 2s = 30s window) |
