# Cluster System

LAN clustering: gossip/mDNS/static node discovery, cookie management, node monitoring. Erlang distribution over plain TCP or UDP multicast on a shared subnet — nothing here touches QUIC or the relay mesh.

**Not the same thing as distribution-over-mesh.** For nodes across NATs, firewalls, or the open internet, see `../macula_dist_system/README.md` and `docs/guides/DIST_OVER_MESH_GUIDE.md` instead.

## Module Table

| Module | Purpose |
|--------|---------|
| `macula_cluster` | Facade — cluster formation, distribution management, cookie resolution, node monitoring |
| `macula_cluster_gossip` | UDP multicast gossip discovery, zero-config, same-subnet |
| `macula_cluster_static` | Manual node-list clustering |
| `macula_cluster_strategy` | libcluster-compatible strategy adapter |

## Quick Start

```erlang
ok = macula_cluster:start_cluster(#{strategy => gossip}).
```

Full options, HMAC authentication, Docker/multicast networking notes, and troubleshooting: `docs/guides/CLUSTERING_GUIDE.md`.

## Consumers

`beam-campus/bc-gitops` depends on `macula_cluster:ensure_distributed/0` (and related cookie/monitor functions) via an optional-dependency pattern — it works standalone and delegates to Macula only when Macula is loaded. See `bc_gitops_cluster.erl`.

This module is started directly by whoever wants LAN clustering; it is not a child of `macula_dist_system`'s supervision tree.

## Testing

```bash
rebar3 eunit --dir=test/macula_cluster_system
```
