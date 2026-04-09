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

## Rust NIFs

The SDK includes Rust NIFs for performance-critical operations. They build automatically via `priv/build-nifs.sh` during `rebar3 compile`. Requires a Rust toolchain (`cargo`). If Rust is not available, pure Erlang fallbacks are used.

| NIF Crate | Provides |
|-----------|----------|
| `native/macula_quic/` | Quinn QUIC transport (precompiled download available) |
| `native/macula_crypto_nif/` | Ed25519, BLAKE3, SHA-256 |
| `native/macula_ucan_nif/` | UCAN token create/verify |
| `native/macula_did_nif/` | DID document operations |
| `native/macula_mri_nif/` | MRI parsing, trie index |

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
