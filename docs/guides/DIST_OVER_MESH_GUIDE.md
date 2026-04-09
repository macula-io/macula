# The World Wide BEAM Computer

**Your BEAM nodes can find each other anywhere on the planet.**

Macula turns the internet into a single Erlang cluster. Any node
connected to the relay mesh can `net_adm:ping`, `gen_server:call`,
`pg:join`, and `monitor` any other node — across NATs, firewalls,
datacenters, and continents. Same OTP primitives you already know.

No VPN. No port forwarding. No static IPs. Just connect to a relay
and your node is part of the world-wide BEAM computer.

## 3 Lines to Join

Start your node with Macula as the distribution protocol:

```
erl -proto_dist macula -no_epmd -start_epmd false \
    -kernel net_ticktime 120 \
    -name mynode@myhost -setcookie MYSECRET
```

Then join the mesh:

```erlang
ok = macula:join_mesh(#{
    relays => [<<"https://relay-de-berlin.macula.io:4433">>]
}).
```

That's it. Now ping any other node on the mesh:

```erlang
pong = net_adm:ping('othernode@otherhost').

%% Everything works:
gen_server:call({my_server, 'othernode@otherhost'}, hello).
pg:join(my_group, self()).
monitor(process, {my_server, 'othernode@otherhost'}).
rpc:call('othernode@otherhost', erlang, node, []).
```

## How It Works

When you call `macula:join_mesh/1`, three things happen:

1. **Connect to relay** — your node establishes an outbound QUIC
   connection to a relay in the mesh (NAT-friendly, firewall-friendly)
2. **Enable relay distribution** — Erlang's distribution protocol is
   tunneled through the relay mesh instead of direct TCP
3. **Advertise** — your node registers itself so other nodes can find it

When you `net_adm:ping('other@host')`, Macula:

1. Sends a tunnel request via the relay mesh to `other@host`
2. Both sides create encrypted AES-256-GCM tunnels (key derived from cookie)
3. OTP's `dist_util` handshake flows through the tunnel
4. Post-handshake: standard distribution traffic, tick keepalive, everything

The relay sees encrypted bytes — it cannot read your Erlang terms.

![Relay Tunnel Architecture](assets/dist_over_mesh.svg)

## join_mesh Options

```erlang
macula:join_mesh(#{
    relays => [<<"https://relay-de-berlin.macula.io:4433">>,
               <<"https://relay-fi-helsinki.macula.io:4433">>],
    realm => <<"io.macula">>,           %% default
    identity => <<"myapp-prod-1">>,     %% default: node name
    tls_verify => none                  %% default: none (dev mode)
}).
```

| Option | Default | Description |
|--------|---------|-------------|
| `relays` | required | List of relay URLs to connect to |
| `realm` | `<<"io.macula">>` | Mesh realm (nodes must share a realm) |
| `identity` | node name | How this node identifies itself |
| `tls_verify` | `none` | `none` for dev, `verify_peer` for production |

## What Nodes Need to Share

| Requirement | Why |
|------------|-----|
| Same **cookie** | Erlang authentication + tunnel encryption key |
| Same **realm** | Relay mesh routing scope |
| Connected to **peered relays** | Relays must be able to reach each other |
| `-proto_dist macula` | Use Macula as the distribution protocol |

That's it. No shared network, no VPN, no port forwarding.

## When to Use This

| Scenario | Approach |
|----------|----------|
| Nodes on same LAN | Direct QUIC (`-proto_dist macula`) |
| Nodes behind NATs/firewalls | **Relay mesh** — `macula:join_mesh/1` |
| Nodes in different datacenters | **Relay mesh** |
| Nodes on different relays | **Relay mesh** (cross-relay via peering) |
| Mnesia replication | Direct QUIC (relay adds latency) |

## Cross-Relay Distribution

Nodes don't need to be on the same relay. If Node A is on
`relay-it-milan` (Nuremberg box) and Node B is on `relay-se-stockholm`
(Helsinki box), the relays forward tunnel traffic through their
peering connections.

```
Node A → relay-it-milan ──peering──► relay-se-stockholm → Node B
```

SWIM protocol detects relay health. Bloom filter routing ensures
tunnel topics are forwarded between relays.

## Architecture Details

### Tunnel Bridge

Each tunnel is managed by a supervised `macula_dist_bridge` gen_server.
The bridge owns a `gen_tcp` loopback socket pair — one end goes to
OTP's `dist_util`, the other is bridged to the relay mesh.

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

### Encryption

All tunnel bytes are encrypted with AES-256-GCM. The key is derived
from the Erlang distribution cookie:

```
Key = SHA-256("macula-dist-tunnel:" ++ cookie)
```

Each message gets a random 12-byte nonce (prepended to ciphertext).
A wrong cookie produces `decrypt_failed` warnings.

### Relay Reconnection

![Relay Failover](assets/relay_failover.svg)

If the relay drops (QUIC disconnect, relay restart), the bridge:

1. Detects `DOWN` monitor signal
2. Re-acquires relay client from `persistent_term`
3. Re-subscribes to tunnel topics
4. Retries every 2s, up to 15 attempts (30s window)

If reconnection completes within `net_ticktime` (default: 120s),
the distribution connection survives transparently.

## Metrics

```erlang
macula_dist_relay:get_tunnel_metrics().
%% => [{<<"abc123">>, #{bytes_out => 4096, bytes_in => 2048,
%%                       msgs_out => 12, msgs_in => 8}}]
```

## Limitations

- **Latency**: Every message goes through the relay (2 extra hops)
- **Mnesia**: Not recommended over WAN latency
- **global module**: May have convergence issues over high-latency links
- **Throughput**: Limited by relay bandwidth and pub/sub overhead

## Troubleshooting

### ping returns pang

1. Verify both nodes started with `-proto_dist macula -no_epmd`
2. Verify both called `macula:join_mesh/1` (or the manual setup)
3. Verify cookies match (`-setcookie`)
4. Verify relays are peered (check SWIM logs)
5. Node names are case-sensitive

### Connection drops after handshake

1. Increase `net_ticktime` to 120+ (`-kernel net_ticktime 120`)
2. Check `macula_dist_relay:get_tunnel_metrics()` for active tunnels

## Configuration Reference

| Env Variable | Default | Description |
|-------------|---------|-------------|
| `MACULA_DIST_MODE` | (unset) | Set to `relay` automatically by `join_mesh/1` |

| Module Define | Value | Description |
|---------------|-------|-------------|
| `DIST_TIMEOUT` | 25000ms | Tunnel negotiation timeout |
| `BRIDGE_RECV_TIMEOUT` | 60000ms | Bridge reader/writer timeout |
| `CONTROLLER_TIMEOUT` | 30000ms | Kernel controller timeout |
| `BACKPRESSURE_HWM` | 64 | Relay client queue depth before pause |
| `RECONNECT_INTERVAL` | 2000ms | Retry interval when relay client is down |
| `RECONNECT_MAX_ATTEMPTS` | 15 | Max retries (15 x 2s = 30s window) |
