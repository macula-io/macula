# Development Guide

This guide covers setting up a development environment for contributing to Macula.

## Prerequisites

- **Erlang/OTP 28** (pinned in `.tool-versions`) - [Installation Guide](https://www.erlang.org/downloads)
- **Rebar3** - Erlang build tool ([Installation](https://rebar3.org/docs/getting-started/))
- **Git** - Version control
- **Docker** (optional) - `Dockerfile` / `Dockerfile.gateway` build deployment images; not required for local development or `rebar3 eunit`

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

Macula SDK is a single Erlang/OTP library, vertical-sliced by capability
rather than by technical layer — most `src/` subdirectories are one small
feature, not a horizontal `services/`/`utils/` split:

```
macula/
├── src/
│   ├── macula.erl               # Public facade (connect, subscribe, publish, call, advertise, ...)
│   ├── macula_topic.erl         # Topic/procedure naming builders (realm/org/app tiers)
│   ├── macula_realm.erl         # Realm tag derivation
│   ├── macula_id.erl, macula_names.erl, macula_node.erl, macula_time.erl, macula_uri.erl, macula_cache.erl
│   │                             # Small top-level utility modules
│   ├── client/                  # macula_client (pool), macula_station_link (per-seed worker)
│   ├── peering/                 # QUIC transport, wire frames, BOLT#4 error taxonomy, peer state machine
│   ├── pubsub/                  # Pub/sub delivery ordering
│   ├── record/                  # Signed DHT records (macula_record), CBOR codec
│   ├── content/                 # Content chunking/manifests (macula_manifest)
│   ├── mri/                     # Resource identifiers — parse, hierarchy, trie index
│   ├── identity/, auth/         # Ed25519 keys, UCAN tokens (Rust NIFs + Erlang fallback)
│   ├── macula_cert_system/      # Self-sovereign certs, trust store
│   ├── macula_dist_system/      # Erlang distribution over relay mesh (3 transports)
│   ├── macula_cluster_system/   # LAN clustering (gossip/static/libcluster) — separate from dist
│   └── (advertise_station/, host_identity/, resolve_address/, ...)
│                                 # Smaller single-purpose slices — one module or two each
├── native/                       # Rust NIF crates (macula_quic, macula_crypto_nif, macula_ucan_nif,
│                                 # macula_did_nif, macula_mri_nif, macula_cbor_nif, macula_tun_nif)
├── test/                         # EUnit tests, one file per module under test
├── include/                      # Header files (.hrl)
├── docs/                         # SDK guides
├── priv/                         # build-nifs.sh, precompiled .so fallbacks
└── rebar.config                  # Build configuration
```

Server modules (gateway, DHT routing, RPC/PubSub routing, SWIM, peering, etc.) live in macula-station.

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

Test files live under `test/`, plus `test/macula_dist_system/` and
`test/macula_cluster_system/` — both directories are registered in
`rebar.config`'s `eunit_opts` so a plain `rebar3 eunit` covers them too.

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
| `native/macula_cbor_nif/` | CBOR encode/decode |
| `native/macula_tun_nif/` | TUN device I/O |

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
- **Documentation**: `docs/guides/` — see the README's guide table for the full index

---

**[← Back to Documentation](../README.md)**
