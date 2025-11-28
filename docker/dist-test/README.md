# Macula QUIC Distribution Test Environment

This directory contains a Docker-based test environment for verifying that **QUIC-based Erlang distribution** works correctly, replacing the traditional EPMD + TCP approach.

## What This Tests

| Feature | Traditional | Macula QUIC |
|---------|------------|-------------|
| **Transport** | TCP | QUIC (UDP) |
| **Discovery** | EPMD daemon | DHT/mDNS |
| **Encryption** | Optional TLS | Built-in TLS 1.3 |
| **Ports** | EPMD (4369) + range | Single UDP port |
| **NAT** | Poor traversal | Good (UDP-based) |

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                   Docker Network                         │
│                   172.30.0.0/24                         │
├─────────────────────────────────────────────────────────┤
│                                                          │
│  ┌─────────────┐   ┌─────────────┐   ┌─────────────┐   │
│  │   node1     │   │   node2     │   │   node3     │   │
│  │ (bootstrap) │   │   (peer)    │   │   (peer)    │   │
│  │             │   │             │   │             │   │
│  │ 172.30.0.10 │   │ 172.30.0.11 │   │ 172.30.0.12 │   │
│  │ UDP:4433    │   │ UDP:4434    │   │ UDP:4435    │   │
│  └──────┬──────┘   └──────┬──────┘   └──────┬──────┘   │
│         │                 │                 │           │
│         └────────QUIC─────┴────────QUIC─────┘           │
│                    (not TCP!)                           │
│                                                          │
└─────────────────────────────────────────────────────────┘

No EPMD daemon anywhere!
```

## Quick Start

### 1. Deploy the Cluster

```bash
cd docker/dist-test
chmod +x deploy.sh test-cluster.sh
./deploy.sh
```

### 2. Run Tests

```bash
./test-cluster.sh
```

### 3. Watch Logs

```bash
docker compose logs -f
```

### 4. Stop Cluster

```bash
./deploy.sh down
```

## Node Names

Macula QUIC distribution uses a different node naming convention:

| Traditional | Macula QUIC |
|-------------|-------------|
| `node1@hostname` | `4433@172.30.0.10` |
| `node2@hostname` | `4434@172.30.0.11` |
| `node3@hostname` | `4435@172.30.0.12` |

The format is `port@ip` which directly encodes the QUIC endpoint.

## Configuration

Nodes are configured via `vm.args`:

```erlang
%% Use macula_dist as the distribution carrier
-proto_dist macula

%% Disable EPMD completely
-no_epmd
-start_epmd false

%% Node name (port@ip format)
-name 4433@172.30.0.10
```

## Manual Testing

### Connect to a Running Node

```bash
# Attach to node1's Erlang shell
docker exec -it macula-dist-node1 /opt/macula/bin/macula remote_console
```

### Check Connected Nodes

```erlang
%% In the Erlang shell
nodes().
%% Should return: ['4434@172.30.0.11', '4435@172.30.0.12']
```

### Ping Another Node

```erlang
net_adm:ping('4434@172.30.0.11').
%% Should return: pong
```

### RPC Call

```erlang
rpc:call('4434@172.30.0.11', erlang, node, []).
%% Should return: '4434@172.30.0.11'
```

### Test Process Groups (:pg)

```erlang
pg:start_link().
pg:join(test_group, self()).
pg:get_members(test_group).
%% Should show processes from all nodes
```

## Troubleshooting

### Nodes Not Connecting

1. **Check logs**: `docker compose logs node1`
2. **Verify QUIC port is open**: `nc -zu 172.30.0.10 4433`
3. **Check cookie matches**: All nodes must have same `ERLANG_COOKIE`

### Distribution Not Starting

1. **Check macula_dist is loaded**:
   ```erlang
   code:is_loaded(macula_dist).
   ```
2. **Check vm.args**: Verify `-proto_dist macula` is set
3. **Check certificates**: TLS certs must exist at configured path

### Connection Refused

QUIC uses UDP, so traditional TCP connection refused errors won't appear.
Check if the target node is actually running and the port is correct.

## Files

| File | Purpose |
|------|---------|
| `Dockerfile` | Builds Erlang node with macula_dist |
| `docker-compose.yml` | Defines 3-node cluster |
| `deploy.sh` | Deployment script |
| `test-cluster.sh` | Cluster verification tests |
| `entrypoint.sh` | Node startup script |
| `scripts/connect-nodes.escript` | Manual node connector |

## Next Steps

After verifying the cluster works:

1. **Test Mnesia replication**: Create tables that replicate over QUIC
2. **Test Horde**: Deploy distributed processes across nodes
3. **Benchmark**: Compare performance to inet_tcp_dist
4. **Integration**: Add to macula-arcade for real-world testing

## Related Documentation

- [v0.11.0 QUIC Distribution Vision](../../architecture/v0.11.0-QUIC_DISTRIBUTION.md)
- [Macula CLAUDE.md](../../CLAUDE.md)
- [quicer Library](https://github.com/emqx/quic)
