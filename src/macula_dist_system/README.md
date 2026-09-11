# Distribution System

Erlang distribution over the Macula mesh: three transports, one goal — `net_adm:ping/1` and the rest of OTP's distribution primitives working across NATs and firewalls with no VPN.

**LAN clustering (the gossip and static strategies) is a separate concern.** See `../macula_cluster_system/README.md` and `docs/guides/CLUSTERING_GUIDE.md`.

## Module Table

| Module | Purpose |
|--------|---------|
| `macula_dist` | `-proto_dist macula` driver — implements the OTP dist-carrier callback set (`listen/1`, `accept/1`, `accept_connection/5`, `setup/5`, `select/1`, ...) for all three transports below |
| `macula_dist_discovery` | Node discovery over mDNS, in a server started with `discovery_type => mdns`; the macula application does not start it. DHT discovery is not available: `dht` and `both` return `{error, {strategy_unavailable, dht}}`. Removed in 11.0.0 |
| `macula_dist_mdns_advertiser` | mDNS advertisement for `macula_dist_discovery`, used when the `macula_mdns` application runs. Removed in 11.0.0 |
| `macula_dist_pool` | Pool-tunneled transport — rides the ordinary mesh pool (stations) via pub/sub. Used by `macula:join_mesh/1` |
| `macula_dist_bridge` / `macula_dist_bridge_sup` | Per-tunnel gen_tcp loopback bridge + its supervisor, for the pool-tunneled transport. `macula_root` starts the supervisor |
| `macula_dist_relay_client` / `macula_dist_relay_protocol` | Client + wire protocol for the dedicated freight relay. Used by `macula:join_dist_relay/1`, talks to the separate `macula-dist-relay` server (raw QUIC stream forwarding, no pub/sub in the hot path). The client runs as a temporary child of `macula_root`; `macula:dist_relay_client/0` returns it for monitoring |

## The Three Transports

| Transport | Entry point | Data path |
|-----------|-------------|-----------|
| **Direct QUIC** | `-proto_dist macula` (no extra call) | Node to node directly, no relay |
| **Pool-tunneled** | `macula:join_mesh/1` | Rides the general mesh pool (stations), pub/sub-framed |
| **Freight relay** | `macula:join_dist_relay/1` | Dedicated `macula-dist-relay` server, raw QUIC stream forwarding |

## Quick Start

### Pool-tunneled (most common — works through NATs/firewalls)

```erlang
%% vm.args
-proto_dist macula -no_epmd -start_epmd false -kernel net_ticktime 120
```

```erlang
ok = macula:join_mesh(#{relays => [<<"https://relay-de-berlin.macula.io:4433">>]}).
pong = net_adm:ping('othernode@otherhost').
```

See `docs/guides/DIST_OVER_MESH_GUIDE.md` for the full guide, options, architecture details, and troubleshooting.

### Direct QUIC (same LAN, lowest latency)

```erlang
-proto_dist macula -no_epmd -start_epmd false
```

No `join_mesh/1` call needed — nodes dial each other directly.

### Freight relay (dedicated tunnel, no pub/sub overhead)

```erlang
ok = macula:join_dist_relay(#{url => <<"https://dist-relay.macula.io:4434">>}).
```

Talks to a `macula-io/macula-dist-relay` server instance, not an ordinary station.

## Status

`-proto_dist macula`, `macula:join_mesh/1` and `macula:join_dist_relay/1` ship in the published `macula` hex package.

- **Automated tests** (`rebar3 eunit --dir=test/macula_dist_system`) cover the driver's node name, address, frame and packet handling; the loopback socket pair and the bridge's tunnel I/O, encryption, backpressure and metrics; the tunnel RPC reply and arguments as a decoded frame delivers them; the relay protocol's frames; and `macula:join_dist_relay/1` against a loopback relay, including a relay that closes.
- **No automated test** runs Erlang distribution end to end, over the mesh pool or through a `macula-dist-relay` server. `hecate-social/hecate-stub` has a dist mode (`HECATE_STUB_MODE=dist`, `src/hecate_stub_dist.erl`) whose ping, rpc and burst scenarios are started through its admin endpoint; its CI builds container images and runs no tests.

## Testing

```bash
rebar3 eunit --dir=test/macula_dist_system
```

## Related Documentation

- [Dist Over Mesh Guide](../../docs/guides/DIST_OVER_MESH_GUIDE.md) — pool-tunneled transport, options, architecture, troubleshooting
- [Clustering Guide](../../docs/guides/CLUSTERING_GUIDE.md) — LAN clustering (separate module, `macula_cluster_system/`)
