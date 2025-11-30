# Macula Hex.pm Publication Guide

**Goal:** Publish Macula to Hex.pm with complete ExDoc-generated API documentation

**Status:** ⚠️ **Pre-publication** - Documentation in progress

---

## Why Hex.pm?

Publishing to Hex.pm provides:
- ✅ **Automatic documentation hosting** at hexdocs.pm/macula
- ✅ **Easy dependency management** for users
- ✅ **Version management** and semver compliance
- ✅ **Discoverability** in the Erlang/Elixir ecosystem
- ✅ **Professional appearance** for the project

Instead of maintaining a separate markdown API reference, we use:
- **Inline `@doc` attributes** in Erlang modules
- **ExDoc** to generate beautiful HTML documentation
- **Hex.pm** to host and distribute the package

---

## Current Status

### ✅ What's Already Done

1. **rebar3_hex plugin** - Configured in `rebar.config`
2. **ex_doc plugin** - Configured in `rebar.config`
3. **Package metadata** - Set in `src/macula.app.src`:
   - Description: "Macula HTTP/3 Mesh Platform - Complete distributed application framework"
   - Version: 0.4.4
   - Licenses: Apache-2.0
   - Links: GitHub repository

4. **Some modules documented** - At least 10+ modules have `@doc` attributes:
   - `macula_client.erl`
   - `macula_connection.erl`
   - `macula_gateway.erl`
   - ... and more

### ❌ What Needs To Be Done

1. **Complete inline documentation** for all public API modules (~68 modules)
2. **Generate and review** ExDoc output locally
3. **Create package description** and metadata
4. **Test package build** with `rebar3 hex build`
5. **Publish to Hex.pm** (requires Hex account)

---

## Step 1: Complete Inline Documentation

### Documentation Standards

Every **public API module** needs:

#### Module Documentation (`@doc` before `-module()`)

```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% Brief one-line description of the module.
%%%
%%% More detailed explanation of what this module does, its purpose,
%%% and how it fits into the overall architecture.
%%%
%%% == Quick Example ==
%%%
%%% ```
%%% % Example code showing basic usage
%%% {ok, Client} = macula_client:connect("https://localhost:4433", #{}).
%%% ok = macula_client:publish(Client, <<"my.topic">>, #{data => <<"hello">>}).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(my_module).
```

#### Function Documentation (`@doc` before each exported function)

```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% Brief description of what this function does.
%%%
%%% More detailed explanation, including behavior, error handling,
%%% and any important notes.
%%%
%%% == Parameters ==
%%%
%%% - `Client': Connected client PID
%%% - `Topic': Topic name as binary (e.g., `<<"my.topic">>')
%%% - `Data': Event payload as map
%%%
%%% == Returns ==
%%%
%%% - `ok' on success
%%% - `{error, Reason}' on failure
%%%
%%% == Example ==
%%%
%%% ```
%%% ok = macula_client:publish(Client, <<"chat.messages">>, #{
%%%     user => <<"Alice">>,
%%%     message => <<"Hello!">>
%%% }).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-spec publish(client(), topic(), event_data()) -> ok | {error, term()}.
publish(Client, Topic, Data) ->
    %% Implementation...
```

#### Type Documentation

```erlang
-type client() :: pid().
%% Reference to a connected Macula mesh client.

-type topic() :: binary().
%% Topic name for pub/sub (e.g., `<<"energy.home.measured">>').

-type event_data() :: map().
%% Event payload - arbitrary map with string or atom keys.
```

### Modules to Document

Run this command to see which modules need documentation:

```bash
cd /home/rl/work/github.com/macula-io/macula
for file in src/*.erl; do
    if ! grep -q "@doc" "$file"; then
        echo "Missing docs: $(basename $file)"
    fi
done
```

### Priority Modules (User-Facing APIs)

These are the most important to document first:

1. **`macula_client.erl`** - Main SDK API (ALREADY DOCUMENTED ✅)
2. **`macula_pubsub_server.erl`** - Pub/Sub operations
3. **`macula_rpc_server.erl`** - RPC operations
4. **`macula_gateway.erl`** - Gateway/connection management
5. **`macula_service_registry.erl`** - Service discovery
6. **`macula_routing_server.erl`** - DHT routing
7. **`macula.erl`** - Top-level application module

---

## Step 2: Generate ExDoc Locally

### Install ExDoc

```bash
# ExDoc is installed via rebar3_ex_doc plugin (already in rebar.config)
rebar3 get-deps
rebar3 compile
```

### Generate Documentation

```bash
rebar3 ex_doc
```

This generates HTML documentation in `doc/` directory.

### Preview Documentation

```bash
# Open in browser
xdg-open doc/index.html

# Or with Python HTTP server
cd doc
python3 -m http.server 8000
# Then visit http://localhost:8000
```

### Check for Warnings

ExDoc will warn about:
- Missing `@doc` attributes
- Broken links
- Invalid code examples
- Type specification issues

Fix all warnings before publishing!

---

## Step 3: Update Package Metadata

### Current Metadata (`src/macula.app.src`)

```erlang
{application, macula, [
    {description, "Macula HTTP/3 Mesh Platform - Complete distributed application framework"},
    {vsn, "0.4.4"},
    {registered, []},
    {mod, {macula_app, []}},
    {applications, [
        kernel,
        stdlib,
        crypto,
        public_key,
        ssl,
        quicer,
        msgpack,
        gproc
    ]},
    {env, []},
    {modules, []},
    {licenses, ["Apache-2.0"]},
    {links, [{"GitHub", "https://github.com/macula-io/macula"}]}
]}.
```

### Recommended Enhancements

Update `macula.app.src` to add:

```erlang
{application, macula, [
    {description, "Decentralized HTTP/3 mesh networking for BEAM - pub/sub, RPC, and service discovery"},
    {vsn, "0.4.4"},
    {registered, []},
    {mod, {macula_app, []}},
    {applications, [
        kernel,
        stdlib,
        crypto,
        public_key,
        ssl,
        quicer,
        msgpack,
        gproc
    ]},
    {env, []},
    {modules, []},
    {licenses, ["Apache-2.0"]},
    {links, [
        {"GitHub", "https://github.com/macula-io/macula"},
        {"Documentation", "https://hexdocs.pm/macula"},
        {"Changelog", "https://github.com/macula-io/macula/blob/main/CHANGELOG.md"}
    ]},
    {maintainers, ["Macula Team"]},
    {files, [
        "src",
        "include",
        "rebar.config",
        "rebar.lock",
        "README.md",
        "LICENSE",
        "CHANGELOG.md"
    ]},
    %% Hex-specific metadata
    {pkg_name, "macula"},
    {build_tools, ["rebar3"]},
    {extra, #{
        <<"contributors">> => [<<"Macula Team">>]
    }}
]}.
```

### Create CHANGELOG.md

Hex requires a CHANGELOG. Create one:

```bash
cat > CHANGELOG.md << 'EOF'
# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.4.4] - 2025-11-14

### Added
- Memory management fixes (5 critical fixes)
- Gateway refactoring (49 tests passing)
- DHT-based service discovery
- Production-ready memory leak prevention

### Fixed
- Bounded connection pool (max 1,000)
- Client connection limits (max 10,000)
- Service TTL with automatic cleanup
- Stream cleanup on disconnect
- Caller process monitoring

## [0.4.0] - 2025-01-08

### Added
- Initial HTTP/3 mesh implementation
- Pub/Sub messaging
- RPC with failover
- Kademlia DHT routing
- SWIM membership
- Multi-tenancy support

EOF
```

---

## Step 4: Test Package Build

### Build Package

```bash
rebar3 hex build
```

This creates a package tarball in `_build/default/lib/macula/hex/`.

### Inspect Package Contents

```bash
tar -tzf _build/default/lib/macula/hex/macula-*.tar
```

Verify it includes:
- ✅ All source files
- ✅ README.md
- ✅ LICENSE
- ✅ CHANGELOG.md
- ✅ rebar.config
- ❌ No test files (they should be excluded)
- ❌ No temp files or build artifacts

### Validate Package

```bash
rebar3 hex build --unpack
```

This unpacks the package for manual inspection.

---

## Step 5: Publish to Hex.pm

### Prerequisites

1. **Create Hex.pm account**: https://hex.pm/signup
2. **Authenticate rebar3_hex**:
   ```bash
   rebar3 hex user register  # If new user
   # OR
   rebar3 hex user auth      # If existing user
   ```

3. **Verify authentication**:
   ```bash
   rebar3 hex user whoami
   ```

### Pre-publication Checklist

- [ ] All public modules have `@doc` attributes
- [ ] ExDoc builds without warnings
- [ ] Documentation reviewed in browser
- [ ] Version bumped appropriately (semver)
- [ ] CHANGELOG.md updated
- [ ] Tests passing (`rebar3 eunit`)
- [ ] Package builds successfully (`rebar3 hex build`)
- [ ] README.md is clear and helpful
- [ ] LICENSE file present

### Publish (Dry Run First!)

```bash
# Dry run - see what would be published
rebar3 hex publish --dry-run

# Review output carefully!
```

### Actual Publication

```bash
# Publish to Hex.pm
rebar3 hex publish

# Follow prompts:
# - Confirm package name
# - Confirm version
# - Confirm files
# - Type version number to confirm
```

### Post-Publication

1. **Verify on Hex.pm**: https://hex.pm/packages/macula
2. **Verify Hexdocs**: https://hexdocs.pm/macula
3. **Update README.md** with Hex installation instructions:
   ```elixir
   # mix.exs
   {:macula, "~> 0.4.4"}
   ```
   ```erlang
   % rebar.config
   {deps, [
       {macula, "0.4.4"}
   ]}.
   ```
4. **Announce** on:
   - Elixir Forum
   - Erlang mailing list
   - Twitter/social media
   - GitHub release notes

---

## Documentation Best Practices

### Good Examples

```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% Publish an event to a topic.
%%%
%%% This function sends an event to all subscribers of the specified
%%% topic. Events are routed through the mesh network using HTTP/3
%%% QUIC transport.
%%%
%%% The event data must be serializable to MessagePack format.
%%%
%%% == Example ==
%%%
%%% ```
%%% ok = macula_client:publish(
%%%     Client,
%%%     <<"sensors.temperature">>,
%%%     #{
%%%         sensor_id => <<"kitchen-01">>,
%%%         temperature => 21.5,
%%%         unit => <<"celsius">>,
%%%         timestamp => erlang:system_time(millisecond)
%%%     }
%%% ).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-spec publish(client(), topic(), event_data()) -> ok | {error, term()}.
publish(Client, Topic, Data) when is_binary(Topic), is_map(Data) ->
    %% Implementation
    ok.
```

### Bad Examples (Avoid These)

```erlang
% No documentation at all
-spec publish(pid(), binary(), map()) -> ok | {error, term()}.
publish(Client, Topic, Data) ->
    ok.

% Too brief (not helpful)
%%% @doc Publishes stuff.
-spec publish(pid(), binary(), map()) -> ok | {error, term()}.
publish(Client, Topic, Data) ->
    ok.

% No code example
%%% @doc
%%% This function publishes events to topics in the mesh network.
%%% @end
-spec publish(pid(), binary(), map()) -> ok | {error, term()}.
publish(Client, Topic, Data) ->
    ok.
```

---

## Quick Reference Commands

```bash
# Generate docs locally
rebar3 ex_doc

# Build package
rebar3 hex build

# Publish (dry run)
rebar3 hex publish --dry-run

# Publish (for real)
rebar3 hex publish

# Check which modules lack docs
for f in src/*.erl; do grep -q "@doc" "$f" || echo "Missing: $f"; done

# Count documented vs undocumented modules
echo "Documented: $(grep -l "@doc" src/*.erl | wc -l)"
echo "Total: $(ls src/*.erl | wc -l)"
```

---

## Roadmap to Publication

### Week 1: Documentation Sprint

- [ ] **Day 1-2**: Document priority modules (macula_client, pubsub, rpc)
- [ ] **Day 3-4**: Document core modules (gateway, routing, service registry)
- [ ] **Day 5**: Document utility modules, review all docs

### Week 2: Polish & Publish

- [ ] **Day 1**: Generate ExDoc, fix all warnings
- [ ] **Day 2**: Create CHANGELOG.md, update README, test package build
- [ ] **Day 3**: Final review, publish to Hex.pm
- [ ] **Day 4-5**: Monitor feedback, create GitHub release, announce

**Total Estimated Time:** 10-12 days

---

## Resources

- **ExDoc Documentation**: https://hexdocs.pm/ex_doc/
- **Hex.pm Publishing Guide**: https://hex.pm/docs/publish
- **rebar3_hex Plugin**: https://github.com/erlef/rebar3_hex
- **rebar3_ex_doc Plugin**: https://github.com/starbelly/rebar3_ex_doc
- **EDoc vs ExDoc**: ExDoc is preferred for Hex packages (nicer UI, better Elixir integration)

---

**Last Updated:** 2025-11-14
**Status:** Ready to begin documentation sprint!
