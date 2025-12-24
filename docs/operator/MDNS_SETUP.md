# mDNS Local Discovery Setup

This document explains how to enable optional mDNS (Multicast DNS) support for local node discovery in Macula.

## Overview

mDNS enables automatic discovery of Macula nodes on the local network without requiring manual configuration of seed nodes. When enabled, nodes can find each other via multicast DNS on the same subnet.

**Note:** mDNS is **optional**. Macula works perfectly without it using DHT-based discovery with configured seed nodes.

## When to Use mDNS

| Use Case | Recommended |
|----------|-------------|
| Development/testing | Yes - simplifies local multi-node setup |
| Home/small office mesh | Yes - no seed node configuration needed |
| Production clusters | No - use DHT with known seed nodes |
| Cross-subnet deployment | No - mDNS is LAN-only |

## Prerequisites

mDNS requires the [shortishly/mdns](https://github.com/shortishly/mdns) library, which uses erlang.mk. We use rebar3's `_checkouts` feature for integration.

## Setup Steps

### 1. Clone Dependencies

```bash
cd /path/to/your/macula/project
mkdir -p _checkouts
cd _checkouts

# Clone mdns
git clone https://github.com/shortishly/mdns.git

# Clone envy (mdns dependency)
git clone https://github.com/shortishly/envy.git
```

### 2. Create rebar3 Wrapper Configs

The macula repository includes pre-configured wrappers. If you need to create them manually:

**_checkouts/mdns/rebar.config:**
```erlang
%% -*- mode: erlang -*-
{erl_opts, [debug_info, warnings_as_errors]}.
{deps, [gproc]}.
```

**_checkouts/mdns/src/mdns.app.src:**
```erlang
{application, mdns, [
    {description, "Multicast DNS"},
    {vsn, "0.5.0"},
    {registered, []},
    {mod, {mdns_app, []}},
    {applications, [kernel, stdlib, crypto, envy, gproc]},
    {env, []},
    {modules, []},
    {licenses, ["Apache-2.0"]},
    {links, [{"GitHub", "https://github.com/shortishly/mdns"}]}
]}.
```

**_checkouts/envy/rebar.config:**
```erlang
%% -*- mode: erlang -*-
{erl_opts, [debug_info, warnings_as_errors]}.
{deps, [{any, "0.3.2"}]}.
```

**_checkouts/envy/src/envy.app.src:**
```erlang
{application, envy, [
    {description, "Wrapper prefixing os_env with application name"},
    {vsn, "git"},
    {registered, []},
    {applications, [kernel, stdlib, any]},
    {env, []},
    {modules, []},
    {licenses, ["Apache-2.0"]},
    {links, [{"GitHub", "https://github.com/shortishly/envy"}]}
]}.
```

### 3. Fix Deprecated Code (if needed)

If you see `crypto:rand_uniform/2` deprecation warnings, edit `_checkouts/mdns/src/mdns_advertise.erl`:

```erlang
%% Replace:
random_timeout(initial) ->
    crypto:rand_uniform(500, 1500).
random_timeout(announcements, TTL) ->
    crypto:rand_uniform(TTL * 500, TTL * 1000).

%% With:
random_timeout(initial) ->
    500 + rand:uniform(1000).
random_timeout(announcements, TTL) ->
    (TTL * 500) + rand:uniform(TTL * 500).
```

### 4. Configure mDNS Environment

Set environment variables before starting your Macula node:

```bash
# Enable mDNS advertising
export MDNS_CAN_ADVERTISE=true

# Enable mDNS discovery
export MDNS_CAN_DISCOVER=true

# Enable automatic mesh formation (optional)
export MDNS_CAN_MESH=true

# Start your node
rebar3 shell
```

### 5. Verify Setup

```bash
rebar3 compile
# Should compile macula + mdns + envy successfully
```

In the shell:
```erlang
%% Check if mDNS is running
whereis(mdns_advertise_sup).
%% Should return a PID, not 'undefined'
```

## How Macula Uses mDNS

When mDNS is available, Macula's discovery system:

1. **Advertising:** `macula_dist_discovery` announces the node via `macula_dist_mdns_advertiser`
2. **Discovery:** `macula_bridge_mesh` subscribes to `mdns:subscribe(advertisement)` events
3. **Graceful Fallback:** If mDNS is not available, these modules silently skip mDNS operations

### Service Type

Macula advertises with service type: `_macula-dist._udp.`

## Dependencies Tree

```
your_project
├── macula (hex package)
│   ├── quicer (QUIC transport)
│   ├── msgpack (serialization)
│   └── gproc (process registry)
└── _checkouts/
    ├── envy (env wrapper)
    │   └── any (hex package)
    └── mdns (mDNS discovery)
        ├── envy (from _checkouts)
        └── gproc (shared with macula)
```

## Troubleshooting

### mDNS Not Discovering Nodes

1. **Check firewall:** mDNS uses UDP port 5353
2. **Same subnet:** mDNS is multicast-based, requires same network segment
3. **Environment variables:** Verify `MDNS_CAN_ADVERTISE` and `MDNS_CAN_DISCOVER` are set

### Compilation Errors

1. **Missing `any` package:** Run `rebar3 get-deps` to fetch hex dependencies
2. **Stale build:** Run `rebar3 clean` then `rebar3 compile`

## Future Plans

We plan to fork shortishly/mdns and shortishly/envy to the macula-io organization and publish them to hex.pm for easier integration. Until then, the `_checkouts` approach provides a working solution.

## Related Documentation

- [Discovery Guide](../guides/DHT_GUIDE.md) - DHT-based discovery (no mDNS required)
- [Quick Start](../user/QUICK_START.md) - Getting started with Macula
- [Development Guide](../developer/DEVELOPMENT.md) - Development setup
