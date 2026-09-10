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
    relays => [<<"https://relay-a.macula.io:4433">>]
}).
```

`relay-a` here is illustrative — see the live, current station list at
[macula.io/stations](https://macula.io/stations) rather than hardcoding a
specific hostname; city-coded relay names have not reliably matched the
box's actual physical location.

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

`join_mesh/1` honors exactly two keys — anything else in the map is
simply ignored, there's no validation error for a stray key:

```erlang
macula:join_mesh(#{
    relays   => [<<"https://relay-a.macula.io:4433">>,
                 <<"https://relay-b.macula.io:4433">>],
    identity => MyKeyPair                %% optional
}).
```

| Option | Default | Description |
|--------|---------|-------------|
| `relays` | required | List of relay URLs to connect to (the V2 pool's seeds) |
| `identity` | auto-generated | `macula_identity:key_pair()` — the pool's shared Ed25519 identity |

There is no `realm` option — dist tunnel frames travel under the
protocol-internal all-zeros realm regardless of any user realm. There
is no `tls_verify` option either, at `join_mesh/1` or anywhere in the
pool's `connect/2` options (see the [Connecting Guide](shared/CONNECTING_GUIDE.md)
for the real option set) — QUIC's TLS 1.3 is mandatory and not
independently togglable from the SDK side.

The dist relay client and direct dist dials check the relay's TLS
certificate against the QUIC library's built-in webpki roots and the host
they dial. A relay with a self-signed certificate is refused unless the
node sets development mode explicitly: `MACULA_TLS_MODE=development` (or
`dev`), or the `tls_mode` app env set to `development`. A CA file set
through `MACULA_TLS_CACERTFILE` or `tls_cacertfile` is not supported: it
makes `macula_tls:quic_client_opts/0,1` raise
`{tls_config_error, {cacertfile_not_supported, Path}}`.

## Dedicated Dist Relay

`macula:join_dist_relay/1` sends distribution traffic through a dedicated
`macula-dist-relay` server instead of the station mesh:

```erlang
ok = macula:join_dist_relay(#{url => <<"quic://dist-relay.example.com:4434">>}).
```

It returns `ok` or `{error, Reason}`, and `{error, macula_not_started}` when
the macula application is not running. The relay client runs as a temporary
child of `macula_root`.

The client does not reconnect. When the relay closes the connection, the
client exits with `{relay_closed, Reason}` and is not restarted, so
distribution over the relay stops. Monitor the client to notice that, and
call `join_dist_relay/1` again after a relay loss:

```erlang
{ok, Client} = macula:dist_relay_client(),
Ref = erlang:monitor(process, Client),
receive
    {'DOWN', Ref, process, Client, _Reason} ->
        macula:join_dist_relay(#{url => RelayUrl})
end.
```

`macula:dist_relay_client/0` returns `{error, not_joined}` when no client is
running.

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

Nodes don't need to be on the same relay. If Node A is on Relay A and
Node B is on a different, peered Relay B, the relays forward tunnel
traffic through their peering connection — see
[macula.io/stations](https://macula.io/stations) for the live topology.

```
Node A → Relay A ──peering──► Relay B → Node B
```

SWIM protocol detects relay health. Bloom filter routing ensures
tunnel topics are forwarded between relays.

## Architecture Details

### Tunnel Bridge

Each tunnel is managed by a supervised `macula_dist_bridge` gen_server.
The bridge owns a `gen_tcp` loopback socket pair — one end goes to
OTP's `dist_util`, the other is bridged to the relay mesh.

```
macula_root (one_for_one, the macula application supervisor)
  ├── ... (other SDK children)
  ├── macula_dist_bridge_sup (simple_one_for_one)
  │     └── macula_dist_bridge (gen_server, per tunnel, temporary)
  │           ├── owns BridgeSock (gen_tcp, {packet, raw})
  │           ├── linked reader process (gen_tcp:recv → encrypt → publish)
  │           ├── handle_info: tunnel_in → decrypt → gen_tcp:send
  │           ├── monitors relay client (reconnects on DOWN)
  │           └── per-tunnel counters (bytes/msgs in/out)
  └── macula_dist_relay_client (temporary, started by macula:join_dist_relay/1)
```

LAN clustering (the gossip and static strategies) lives in
`macula_cluster_system` and is not started by the macula application. See
the [Clustering Guide](CLUSTERING_GUIDE.md).

### Encryption

All tunnel bytes are encrypted with AES-256-GCM. The key is derived
from the Erlang distribution cookie:

```
Key = SHA-256("macula-dist-tunnel:" ++ cookie)
```

Each message gets a random 12-byte nonce (prepended to ciphertext).
A wrong cookie produces `decrypt_failed` warnings.

### Relay Reconnection

If the relay drops (QUIC disconnect, relay restart), the bridge:

1. Detects `DOWN` monitor signal
2. Re-acquires relay client from `persistent_term`
3. Re-subscribes to tunnel topics
4. Retries every 2s, up to 15 attempts (30s window)

If reconnection completes within `net_ticktime` (default: 120s),
the distribution connection survives transparently.

## Metrics

```erlang
macula_dist_pool:get_tunnel_metrics().
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
2. Check `macula_dist_pool:get_tunnel_metrics()` for active tunnels

## Configuration Reference

| Env Variable | Default | Description |
|-------------|---------|-------------|
| `MACULA_DIST_MODE` | (unset) | Set to `relay` by `join_mesh/1`, and to `dist_relay` by `join_dist_relay/1` |

| Module Define | Value | Description |
|---------------|-------|-------------|
| `DIST_TIMEOUT` | 10000ms | Tunnel RPC timeout |
| `BRIDGE_RECV_TIMEOUT` | 60000ms | Bridge reader/writer timeout |
| `CONTROLLER_TIMEOUT` | 30000ms | Kernel controller timeout |
| `BACKPRESSURE_HWM` | 64 | Relay client queue depth before pause |
| `RECONNECT_INTERVAL` | 2000ms | Retry interval when relay client is down |
| `RECONNECT_MAX_ATTEMPTS` | 15 | Max retries (15 x 2s = 30s window) |
