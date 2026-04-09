# Development Guide

This guide covers setting up a development environment for contributing to Macula.

## Prerequisites

- **Erlang/OTP 26+** - [Installation Guide](https://www.erlang.org/downloads)
- **Rebar3** - Erlang build tool ([Installation](https://rebar3.org/docs/getting-started/))
- **Git** - Version control
- **Docker** (optional) - For multi-node testing

## Quick Setup

```bash
# Clone the repository
git clone https://github.com/macula-io/macula.git
cd macula

# Fetch dependencies
rebar3 get-deps

# Compile
rebar3 compile

# Run tests
rebar3 eunit

# Start a shell with Macula loaded
rebar3 shell
```

## Project Structure

Macula SDK is organized as a single Erlang/OTP library with 48 modules:

```
macula/
├── src/                          # Source code (48 .erl files)
│   ├── macula.erl               # Public facade (connect, subscribe, publish, call, advertise)
│   ├── macula_relay_client.erl  # QUIC relay connection
│   ├── macula_multi_relay.erl   # Multi-homed connections
│   ├── macula_quic.erl          # QUIC transport abstraction
│   ├── macula_protocol_*.erl    # Wire protocol encoding/decoding
│   ├── macula_crypto_nif.erl    # Ed25519, BLAKE3, SHA-256 (Rust NIF + Erlang fallback)
│   ├── macula_ucan_nif.erl      # UCAN token operations (Rust NIF + Erlang fallback)
│   ├── macula_did_nif.erl       # DID document operations (Rust NIF + Erlang fallback)
│   ├── macula_mri.erl           # Resource identifiers (parse, format, hierarchy, trie index)
│   ├── macula_cert_system/      # Ed25519 keypairs, cert generation, trust store
│   ├── macula_dist_system/      # Erlang distribution over relay mesh
│   └── macula_*.erl             # Utilities (id, names, node, realm, time, uri, cache)
├── native/                       # Rust NIF source (Quinn QUIC + crypto/UCAN/DID/MRI)
├── test/                         # EUnit tests
├── include/                      # Header files (.hrl)
├── docs/                         # SDK guides
├── priv/                         # Build scripts, precompiled NIFs
└── rebar.config                  # Build configuration
```

Server modules (gateway, DHT routing, RPC/PubSub routing, SWIM, peering, etc.) live in [macula-relay](https://github.com/macula-io/macula-relay).

## Running Tests

### All Tests
```bash
rebar3 eunit
```

### Specific Module Tests
```bash
rebar3 eunit --module=macula_mri_tests
rebar3 eunit --module=macula_crypto_nif_tests
```

### Test Coverage
```bash
rebar3 do eunit, cover
```

### Multi-Node Integration Tests
```bash
cd docker
docker compose -f docker-compose.multi-node-test.yml build --no-cache
docker compose -f docker-compose.multi-node-test.yml up
```

## Code Quality Standards

Macula follows **Idiomatic Erlang** principles:

### Core Principles
- ✅ **Pattern matching on function heads** - Avoid `if` and `cond`
- ✅ **Guards instead of case** - Use guards for simple conditions
- ✅ **Shallow nesting** - Keep nesting to 1-2 levels maximum
- ✅ **Let it crash** - Don't catch errors unless you can handle them meaningfully
- ✅ **OTP behaviors** - Use gen_server, gen_statem, supervisor where appropriate

### Example: Good vs. Bad

❌ **Bad:**
```erlang
process_message(Msg, State) ->
    if
        is_binary(Msg) ->
            case decode_message(Msg) of
                {ok, Data} ->
                    if
                        Data#data.type == request ->
                            handle_request(Data, State);
                        Data#data.type == response ->
                            handle_response(Data, State)
                    end
            end
    end.
```

✅ **Good:**
```erlang
%% Guard ensures binary input
process_message(Msg, State) when is_binary(Msg) ->
    case decode_message(Msg) of
        {ok, Data} -> handle_decoded_message(Data, State);
        {error, Reason} -> {error, Reason}
    end;
process_message(_Msg, _State) ->
    {error, invalid_message}.

%% Pattern match on data type
handle_decoded_message(#data{type = request} = Data, State) ->
    handle_request(Data, State);
handle_decoded_message(#data{type = response} = Data, State) ->
    handle_response(Data, State).
```

See CLAUDE.md (at repository root) for complete coding guidelines.

## Building Documentation

Macula uses ex_doc for documentation generation:

```bash
rebar3 ex_doc
```

Generated docs appear in `doc/` directory. Open `doc/index.html` in a browser.

## Docker Development

### Clean Build (Always After Code Changes)
```bash
# Prune cache and rebuild from scratch
docker builder prune -af
docker compose -f <compose-file> build --no-cache
```

**Why?** Docker build cache can use stale layers even after code changes. Always prune and rebuild when testing code changes.

### Multi-Node Test Environment
```bash
cd docker
docker-compose -f docker-compose.multi-node-test.yml up
```

This starts:
- 1 registry node (gateway)
- 3 provider nodes (advertise services)
- 1 client node (discovers and calls services)

## Memory Management

Macula implements comprehensive memory leak prevention. See [Memory Management](../../architecture/memory_management/README.md) for details.

**Key Mechanisms:**
- Bounded connection pools (max 1,000 connections, LRU eviction)
- Client connection limits (max 10,000 clients, configurable)
- Service TTL/cleanup (5-minute TTL, 60-second cleanup interval)
- Stream cleanup on disconnect
- Caller process monitoring for RPC handlers

**Monitoring:**
```erlang
%% Check connection pool size
macula_gateway_mesh:pool_size(GatewayPid).

%% Check client count
macula_gateway_client_manager:client_count(ManagerPid).

%% Check service registry size
macula_service_registry:service_count().
```

## Refactoring Status

### Gateway Refactoring (COMPLETED - Jan 2025)
The gateway module has been refactored into 6 focused modules with comprehensive tests:

- ✅ `macula_gateway_client_manager.erl` - Client lifecycle (24 tests)
- ✅ `macula_gateway_pubsub.erl` - Pub/Sub routing (31 tests)
- ✅ `macula_gateway_rpc.erl` - RPC handler management (20 tests)
- ✅ `macula_gateway_mesh.erl` - Mesh connection pooling (16 tests)
- ✅ `macula_gateway_dht.erl` - DHT query forwarding (stateless)
- ✅ `macula_gateway_rpc_router.erl` - Multi-hop RPC routing (17 tests)
- ✅ `macula_gateway_sup.erl` - Supervision tree (24 tests)

Total: 132 tests, all passing.

### Connection Refactoring (COMPLETED - Nov 2025)
The v0.7.0 nomenclature refactoring achieved separation of concerns:
- `macula_peer` - High-level API facade for mesh operations
- `macula_connection` - Low-level QUIC transport layer

See CLAUDE.md for current architecture details.

## Contributing Workflow

1. **Read the documentation**
   - README.md (at repository root) - Project overview
   - CLAUDE.md (at repository root) - Coding guidelines

2. **Create a feature branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```

3. **Write tests first** (TDD approach preferred)
   ```bash
   # Create test file
   touch test/macula_your_module_tests.erl

   # Write failing tests
   # Implement functionality
   # Verify tests pass
   rebar3 eunit --module=macula_your_module_tests
   ```

4. **Follow code quality standards**
   - Pattern matching over conditionals
   - Guards instead of case where possible
   - Maximum 1-2 levels of nesting
   - Comprehensive tests for new functionality

5. **Commit and push**
   ```bash
   git add .
   git commit -m "Add feature: your feature description"
   git push origin feature/your-feature-name
   ```

6. **Create pull request**
   - Describe what the PR does
   - Reference any related issues
   - Ensure all tests pass
   - Follow PR template guidelines

## Getting Help

- **Issues**: [GitHub Issues](https://github.com/macula-io/macula/issues)
- **Documentation**: See `architecture/` directory for detailed architecture docs

---

**[← Back to Documentation](../README.md)**
