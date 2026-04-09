# Macula Clustering Guide

This guide covers Macula's LAN clustering capabilities, including gossip-based discovery, the Cluster API, distribution management, and cookie management.

**Note:** Clustering via gossip/mDNS is designed for **LAN environments** (same subnet). For WAN connectivity across networks, use the relay mesh (see the Dist Over Mesh guide).

## Overview

The Macula Cluster API (`macula_cluster.erl`) provides a standardized interface for:

- **Cluster Formation** - Starting clusters with various discovery strategies
- **Distribution Management** - Starting and verifying Erlang distribution
- **Cookie Management** - Resolving, setting, and persisting cluster cookies
- **Node Monitoring** - Subscribing to node join/leave events

![Cluster API Integration](assets/cluster_integration.svg)

---

## Cluster Strategies

| Strategy | Discovery | Configuration | Network | Use Case |
|----------|-----------|---------------|---------|----------|
| **gossip** | Automatic (UDP multicast) | Zero-config | LAN multicast | Development, same-subnet production |
| **static** | Manual | Node list required | Any | Known node sets, cross-subnet |
| **mdns** | Automatic (mDNS/Bonjour) | Zero-config | LAN mDNS | macOS/Linux local development |

---

## Quick Start

### Via `macula_cluster` API (Recommended)

```erlang
%% Start with gossip strategy (default)
ok = macula_cluster:start_cluster(#{strategy => gossip}).

%% With shared secret for cluster isolation
ok = macula_cluster:start_cluster(#{
    strategy => gossip,
    secret => <<"my_cluster_secret">>
}).

%% With static node list
ok = macula_cluster:start_cluster(#{
    strategy => static,
    nodes => ['node1@host1', 'node2@host2']
}).
```

### Erlang (Direct Gossip)

```erlang
%% Start with defaults
{ok, _Pid} = macula_cluster_gossip:start_link(#{}).

%% With custom configuration
{ok, _Pid} = macula_cluster_gossip:start_link(#{
    multicast_addr => {230, 1, 1, 251},
    port => 45892,
    broadcast_interval => 1500,
    secret => <<"my_cluster_secret">>
}).
```

### Elixir (Phoenix Application)

```elixir
def start(_type, _args) do
  :macula_cluster.start_cluster(%{strategy: :gossip})

  children = [
    # ... your supervision tree
  ]

  Supervisor.start_link(children, strategy: :one_for_one)
end
```

---

## Gossip Clustering

The `macula_cluster_gossip` module provides automatic cluster discovery using UDP multicast, implemented natively in Erlang.

![Gossip Clustering](assets/gossip_clustering.svg)

### How It Works

1. **Join Multicast Group**: Each node joins the UDP multicast group (default: `230.1.1.251:45892`)
2. **Broadcast Heartbeats**: Nodes periodically broadcast their Erlang node name
3. **Discover Peers**: When a heartbeat is received from an unknown node, it's added to discovered set
4. **Connect via Distribution**: Newly discovered nodes are connected using `net_kernel:connect_node/1`
5. **Full Mesh**: All nodes eventually connect to form a full mesh cluster

### Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `multicast_addr` | `{A,B,C,D}` | `{230,1,1,251}` | Multicast group address |
| `port` | `integer()` | `45892` | UDP port for gossip |
| `broadcast_interval` | `integer()` | `1500` | Milliseconds between heartbeats |
| `multicast_ttl` | `integer()` | `1` | Time-to-live (1 = same subnet) |
| `secret` | `binary()` | `undefined` | Shared secret for HMAC authentication |
| `callback` | `pid() \| {M,F}` | `undefined` | Callback for cluster events |

### HMAC Authentication

When a `secret` is configured, all gossip packets include an HMAC-SHA256 signature:

```
MACULA_GOSSIP:node_name|<HMAC-SHA256>
```

- **Without secret**: Nodes accept any valid-looking gossip packet
- **With secret**: Nodes verify HMAC before accepting the packet

### Environment Variables

```bash
MACULA_GOSSIP_ADDR=230.1.1.251
MACULA_GOSSIP_PORT=45892
MACULA_GOSSIP_SECRET=my_cluster_secret
CLUSTER_STRATEGY=gossip
CLUSTER_SECRET=my_cluster_secret
```

### Gossip Query API

```erlang
%% Get all discovered nodes (may not be connected yet)
Discovered = macula_cluster_gossip:get_discovered().

%% Get connected nodes
Connected = macula_cluster_gossip:get_connected().

%% Force immediate broadcast (useful for testing)
ok = macula_cluster_gossip:broadcast_now().
```

---

## Distribution Management

### Ensure Distributed Mode

```erlang
ok = macula:ensure_distributed().
```

If the node is already distributed, returns `ok` immediately. Otherwise, starts distribution with a generated node name in the format `macula_host@hostname`.

---

## Cookie Management

### `macula:get_cookie/0`

Resolves the cookie from sources in priority order:

1. **Application env**: `{macula, [{cookie, CookieValue}]}`
2. **Environment variable**: `MACULA_COOKIE` or `RELEASE_COOKIE` or `ERLANG_COOKIE`
3. **Cookie file**: `~/.erlang.cookie`
4. **Auto-generated**: Creates and persists a new cookie

```erlang
Cookie = macula:get_cookie().
```

### `macula:set_cookie/1`

Sets the Erlang cookie for this node and persists it.

```erlang
ok = macula:set_cookie(my_secret_cookie).
ok = macula:set_cookie(<<"my_secret_cookie">>).
```

### Configuration

```erlang
%% sys.config
[{macula, [{cookie, 'my_cluster_cookie'}]}].
```

```bash
export MACULA_COOKIE="my_cluster_cookie"
export RELEASE_COOKIE="my_cluster_cookie"
```

---

## Node Monitoring

### Subscribe to Events

```erlang
ok = macula:monitor_nodes().

receive
    {nodeup, Node} -> io:format("Node joined: ~p~n", [Node]);
    {nodedown, Node} -> io:format("Node left: ~p~n", [Node])
end.
```

### Unsubscribe

```erlang
ok = macula:unmonitor_nodes().
```

### Gossip-Specific Callbacks

```erlang
%% Using a PID
{ok, _} = macula_cluster_gossip:start_link(#{callback => self()}).

receive
    {macula_cluster, nodeup, Node} -> handle_join(Node);
    {macula_cluster, nodedown, Node} -> handle_leave(Node)
end.

%% Using module/function callback
{ok, _} = macula_cluster_gossip:start_link(#{
    callback => {my_module, handle_cluster_event}
}).
```

---

## Integration with bc_gitops

bc_gitops uses an optional dependency pattern to delegate to Macula when available:

```erlang
%% In bc_gitops_cluster.erl
ensure_distributed() ->
    case macula_exports(ensure_distributed, 0) of
        true -> apply(macula, ensure_distributed, []);
        false -> do_ensure_distributed()
    end.

macula_exports(Function, Arity) ->
    macula_available() andalso
    erlang:function_exported(macula, Function, Arity).

macula_available() ->
    case code:ensure_loaded(macula) of
        {module, macula} -> true;
        {error, _} -> false
    end.
```

This ensures bc_gitops works standalone (without Macula) and delegates when Macula is present.

---

## Docker Compose Example

```yaml
services:
  node1:
    image: my-app:latest
    network_mode: host  # Required for UDP multicast
    environment:
      - RELEASE_NODE=node1@localhost
      - RELEASE_COOKIE=my_secret_cookie
      - CLUSTER_STRATEGY=gossip
      - CLUSTER_SECRET=demo_secret

  node2:
    image: my-app:latest
    network_mode: host
    environment:
      - RELEASE_NODE=node2@localhost
      - RELEASE_COOKIE=my_secret_cookie
      - CLUSTER_STRATEGY=gossip
      - CLUSTER_SECRET=demo_secret
```

---

## Network Requirements

### Firewall Rules

```bash
# Allow UDP multicast traffic
iptables -A INPUT -p udp --dport 45892 -j ACCEPT
iptables -A OUTPUT -p udp --dport 45892 -j ACCEPT

# For Erlang distribution (EPMD and distribution ports)
iptables -A INPUT -p tcp --dport 4369 -j ACCEPT
iptables -A INPUT -p tcp --dport 9100:9200 -j ACCEPT
```

### Docker Networking

**Host Networking (Recommended for development):**
```yaml
network_mode: host
```

**Macvlan (Production):**
```yaml
networks:
  macvlan_net:
    driver: macvlan
    driver_opts:
      parent: eth0
    ipam:
      config:
        - subnet: 192.168.1.0/24
```

---

## Troubleshooting

### Nodes Not Discovering Each Other

1. **Check multicast support**: `ping -c 3 230.1.1.251`
2. **Verify UDP port is open**: `ss -ulnp | grep 45892`
3. **Check Docker networking**: Must use host networking for multicast
4. **Verify cookie matches**: `erlang:get_cookie().`

### Authentication Failures

If nodes have mismatched secrets:
```
[warning] [macula_cluster_gossip] Invalid HMAC from <IP> (authentication failed)
```

Ensure all nodes use the same `CLUSTER_SECRET` environment variable.

### High CPU Usage

If `broadcast_interval` is too low, increase it:
```erlang
{ok, _} = macula_cluster_gossip:start_link(#{broadcast_interval => 5000}).
```

---

## Security Best Practices

1. **Always use a secret in production** - Prevents unauthorized nodes from joining
2. **Rotate secrets periodically** - Coordinate rotation across all nodes
3. **Use network segmentation** - Limit multicast scope with VLANs
4. **Set TTL appropriately** - `multicast_ttl => 1` limits to same subnet

---

## Testing

```bash
# Run gossip clustering tests
rebar3 eunit --module=macula_cluster_gossip_tests

# Run cluster API tests
rebar3 eunit --module=macula_cluster_tests

# Run full cluster test suite
rebar3 eunit --dir=test/macula_dist_system
```

---

## Related Documentation

- [Dist Over Mesh Guide](DIST_OVER_MESH_GUIDE.md) - WAN connectivity via relay mesh
- [Erlang Distribution Protocol](https://www.erlang.org/doc/reference_manual/distributed.html)
