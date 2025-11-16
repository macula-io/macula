# mDNS Dependency Setup

This document explains the mDNS integration for Macula.

## Background

Macula uses mDNS (Multicast DNS) for local node discovery. The best available Erlang implementation is [shortishly/mdns](https://github.com/shortishly/mdns), which is pure Erlang but uses the erlang.mk build system instead of rebar3.

## Current Solution: Rebar3 Checkouts

We use rebar3's `_checkouts` feature to vendor the dependencies with rebar3 wrapper configs:

### Setup Steps

1. **Clone dependencies to _checkouts:**
   ```bash
   cd /home/rl/work/github.com/macula-io/macula
   mkdir -p _checkouts
   cd _checkouts

   # Clone mdns
   git clone https://github.com/shortishly/mdns.git

   # Clone envy (mdns dependency)
   git clone https://github.com/shortishly/envy.git
   ```

2. **Create rebar3 wrapper configs:**

   **_checkouts/mdns/rebar.config:**
   ```erlang
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

3. **Fix deprecated code in mdns:**

   Edit `_checkouts/mdns/src/mdns_advertise.erl` lines 95-99:
   ```erlang
   % OLD (deprecated):
   random_timeout(initial) ->
       crypto:rand_uniform(500, 1500).

   random_timeout(announcements, TTL) ->
       crypto:rand_uniform(TTL * 500, TTL * 1000).

   % NEW (modern):
   random_timeout(initial) ->
       500 + rand:uniform(1000).

   random_timeout(announcements, TTL) ->
       (TTL * 500) + rand:uniform(TTL * 500).
   ```

4. **Update main rebar.config:**
   ```erlang
   {deps, [
       {quicer, {git, "https://github.com/emqx/quic.git", {branch, "main"}}},

       %% mDNS dependencies (envy in _checkouts, mdns will auto-detect)
       {gproc, "0.9.1"},
       {envy, {git, "https://github.com/shortishly/envy.git", {branch, "master"}}}
   ]}.
   ```

## TODO: Long-term Solution

**Fork to macula-io organization:**

1. Create forks:
   - https://github.com/macula-io/mdns (fork of shortishly/mdns)
   - https://github.com/macula-io/envy (fork of shortishly/envy)

2. Add rebar3 support directly in forks:
   - Include rebar.config and .app.src files
   - Fix deprecated code
   - Maintain as rebar3-compatible branches

3. Update main rebar.config to use forks:
   ```erlang
   {deps, [
       {mdns, {git, "https://github.com/macula-io/mdns.git", {branch, "rebar3"}}},
       {envy, {git, "https://github.com/macula-io/envy.git", {branch, "rebar3"}}}
   ]}.
   ```

4. Consider contributing rebar3 support upstream.

## Verification

```bash
rebar3 compile
# Should compile all 12 macula apps + mdns + envy successfully
```

## Dependencies Tree

```
macula (umbrella)
├── quicer (QUIC transport)
├── gproc (process registry)
├── envy (_checkouts - env wrapper)
│   └── any (hex package)
└── mdns (_checkouts - mDNS discovery)
    ├── envy (from _checkouts)
    └── gproc
```
