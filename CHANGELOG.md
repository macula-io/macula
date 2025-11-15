# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.8.0] - TBD (Q2 2025)

### Planned - True Mesh Architecture
- **BREAKING**: Opportunistic NAT hole punching for direct peer-to-peer connections
  - 80% direct P2P connections (cone NAT, no firewall)
  - 20% gateway relay fallback (symmetric NAT, strict firewalls)
  - True mesh topology (no single point of failure)
  - New modules: `macula_nat_discovery`, `macula_hole_punch`, `macula_connection_upgrade`
  - Backward compatible with v0.7.x gateway relay architecture

**This will transform Macula from hub-and-spoke (star topology) to true decentralized mesh.**

See `architecture/NAT_TRAVERSAL_ROADMAP.md` for complete design.

---

## [0.7.4] - 2025-11-15

### Fixed
- **CRITICAL**: Configurable keep-alive mechanism to prevent QUIC connection timeouts
  - PING/PONG message support in `macula_connection`
  - Default keep-alive interval: 30 seconds (configurable)
  - Keep-alive enabled by default (can be disabled via options)
  - Automatic PONG response to incoming PING messages
  - Configuration via `macula_connection:default_config/0`
  - Prevents 2-minute connection timeout that broke distributed matchmaking
  - Added 6 tests for keep-alive functionality (all passing)

**This is a critical fix for production deployments where QUIC connections timeout after ~2 minutes of inactivity, breaking pub/sub and matchmaking.**

### Configuration

Enable/disable keep-alive:
```erlang
%% Enable with custom interval (milliseconds)
Opts = #{
    keepalive_enabled => true,
    keepalive_interval => 30000  %% 30 seconds
}.

%% Disable keep-alive
Opts = #{
    keepalive_enabled => false
}.

%% Use defaults (enabled, 30 second interval)
DefaultConfig = macula_connection:default_config().
```

### Architecture Note

**v0.7.4 maintains hub-and-spoke (star) topology**:
- Edge peers connect to gateway (not each other)
- Gateway routes all messages (relay architecture)
- Gateway is single point of failure (by design for now)
- DHT routing table exists but routing happens at gateway
- True peer-to-peer mesh deferred to v0.8.0 (NAT traversal required)

## [0.7.3] - 2025-11-15

### Fixed
- **CRITICAL**: Fixed DHT routing table address serialization crash in `macula_gateway_dht`
  - Bug: Gateway stored parsed address **tuples** `{{127,0,0,1}, 9443}` in DHT instead of binary strings
  - Impact: When FIND_VALUE replies tried to serialize node addresses, msgpack returned error `{:error, {:badarg, {{127,0,0,1}, 9443}}}`
  - Root cause: `macula_gateway.erl:522` used `Address` (tuple from `parse_endpoint/1`) instead of `Endpoint` (binary string)
  - Error chain: DHT stored tuples → encode_node_info extracted tuples → msgpack:pack failed → byte_size crashed
  - Symptoms: Gateway crashed with "ArgumentError: 1st argument not a bitstring" when peers queried DHT
  - Fix: Store original `Endpoint` binary string in DHT routing table instead of parsed tuple
  - Added test: `dht_address_serialization_test` documents bug and validates fix

**This is a critical fix for distributed matchmaking and service discovery. Without it, DHT queries crash the gateway.**

## [0.7.2] - 2025-11-15

### Fixed
- **CRITICAL**: Fixed gateway crash in `parse_endpoint/1` when DNS resolution fails
  - Bug: `inet:getaddr/2` error tuple was not handled, causing ArgumentError when passed to `byte_size/1`
  - Impact: Gateway crashed repeatedly, closing all client connections and preventing pub/sub communication
  - Symptoms: "Failed to publish to topic: :closed", "Failed to send STORE for subscription: :closed"
  - Fix: Added proper error handling with localhost fallback when DNS resolution fails
  - Now returns `{{127,0,0,1}, Port}` fallback instead of crashing

**This is a critical fix for production deployments where endpoint DNS resolution may fail.**

## [0.7.1] - 2025-11-15

### Fixed
- **CRITICAL**: Fixed ArithmeticError in `macula_pubsub_handler` message ID handling
  - Bug: Was assigning binary MsgId to counter instead of integer NewCounter
  - Impact: Caused pub/sub to crash on second publish attempt with "bad argument in arithmetic expression"
  - Fix: Corrected destructuring in line 300 to use `{_MsgId, NewCounter}` instead of `{MsgIdCounter, _}`
  - Now properly increments integer counter instead of trying to do arithmetic on binary

**This is a critical fix for anyone using pub/sub functionality in v0.7.0.**

## [0.7.0] - 2025-11-15

### Changed
- **BREAKING**: Major nomenclature refactoring for clarity and industry alignment
  - Renamed `macula_connection` → `macula_peer` (mesh participant facade - high-level API)
  - Renamed `macula_connection_manager` → `macula_connection` (QUIC transport layer - low-level)
  - Follows industry standards used by libp2p, IPFS, and BitTorrent
  - Clear separation: `macula_peer` = mesh participant, `macula_connection` = transport

### Added
- Comprehensive transport layer test coverage (36 tests total)
  - 11 new tests for message decoding, buffering, URL parsing, and realm normalization
  - All tests passing with zero regressions
- Complete v0.7.0 documentation in CLAUDE.md
  - Migration guide with specific API examples
  - Architecture rationale and benefits
  - Status tracking for implementation phases

### Migration Guide (0.6.x → 0.7.0)

**API Changes:**

All high-level mesh operations now use `macula_peer` instead of `macula_connection`:

```erlang
%% Before (0.6.x)
{ok, Client} = macula_connection:start_link(Url, Opts).
ok = macula_connection:publish(Client, Topic, Data).
{ok, SubRef} = macula_connection:subscribe(Client, Topic, Callback).
{ok, Result} = macula_connection:call(Client, Procedure, Args).

%% After (0.7.0)
{ok, Client} = macula_peer:start_link(Url, Opts).
ok = macula_peer:publish(Client, Topic, Data).
{ok, SubRef} = macula_peer:subscribe(Client, Topic, Callback).
{ok, Result} = macula_peer:call(Client, Procedure, Args).
```

**Why This Change?**

The original naming was confusing:
- ❌ `macula_connection` served both facade AND transport roles
- ❌ Mixed high-level mesh operations with low-level QUIC handling
- ❌ Not aligned with P2P industry standards

After v0.7.0:
- ✅ `macula_peer` = mesh participant (clear high-level API for pub/sub, RPC, DHT)
- ✅ `macula_connection` = QUIC transport (clear low-level transport layer)
- ✅ Follows libp2p/IPFS/BitTorrent naming conventions

**Note:** The `macula_client` wrapper module has been updated to use `macula_peer` internally, so if you're using `macula_client`, no changes are required.

## [0.6.7] - 2025-11-15

### Fixed
- **CRITICAL:** Fixed all installation examples to use Hex package references instead of git dependencies
  - README.md: Changed from git-based to `{:macula, "~> 0.6"}` (Elixir) and `{macula, "0.6.7"}` (Erlang)
  - HELLO_WORLD.md: Updated to use proper Hex package format
  - architecture/macula_http3_mesh_hello_world.md: Fixed tutorial installation examples
  - architecture/macula_http3_mesh_rpc_guide.md: Fixed migration guide examples
  - All code examples now show proper Hex.pm installation for published package

## [0.6.6] - 2025-11-15

### Fixed
- Fixed navigation links in documentation guides to use ex_doc HTML filenames
  - Changed GitHub-style relative paths (`../README.md`) to ex_doc HTML references (`readme.html`)
  - Fixed all navigation links in EXECUTIVE_SUMMARY.md, COMPARISONS.md, USE_CASES.md, and DEVELOPMENT.md
  - Links now work correctly in published Hexdocs without "page not found" errors

## [0.6.5] - 2025-11-15

### Changed
- Updated to modern alternative logo (macula-alt-logo.svg) in both README.md and ex_doc
- Changed tutorial greeting to brand-specific "Hello, Macula!" instead of generic greeting

### Fixed
- Replaced old color logo with cleaner, more modern alternative logo for better visual appeal

## [0.6.4] - 2025-11-15

### Changed
- **Documentation restructuring** - Split README.md into focused landing page with table of contents
  - Created `docs/EXECUTIVE_SUMMARY.md` - Why Macula and the case for decentralization
  - Created `docs/COMPARISONS.md` - How Macula compares to libp2p, Distributed Erlang, Akka, etc.
  - Created `docs/USE_CASES.md` - Real-world applications across business, IoT, and AI domains
  - Created `docs/DEVELOPMENT.md` - Complete development guide and coding standards
  - README.md now serves as concise landing page (119 lines vs 372 lines)
  - All detailed content accessible via clear table of contents
  - Removed Mermaid diagram from README.md (ex_doc doesn't support Mermaid - works on GitHub)

### Fixed
- ex_doc landing page uses HELLO_WORLD.md (tutorial-first approach, no multi-page split)
- Documentation properly links to all new guide documents
- Better first impression for Hex.pm users (logo, quick navigation)

## [0.6.3] - 2025-11-15

### Fixed
- Removed README.md from ex_doc extras to prevent multi-page split and broken landing page
- Documentation now correctly redirects to API reference page

## [0.6.2] - 2025-11-15

### Fixed
- ex_doc landing page configuration (`{main, "api-reference"}`) - resolved "readme.html not found" error

## [0.6.1] - 2025-11-15

### Added
- Professional documentation structure for Hex publication
  - Architecture diagram in README.md (Mermaid format) showing mesh topology
  - Organized documentation: moved 50+ files from root to docs/archive/, docs/development/, docs/planning/
  - Created docs/README.md navigation index
  - Logo and assets configuration for ex_doc
  - Comprehensive Hex package file list (artwork/, docs/, architecture/)

### Fixed
- README.md badge rendering (moved badges outside `<div>` tag for proper GitHub display)
- ex_doc assets configuration (deprecated warning resolved)
- ex_doc landing page configuration (changed `{main, "readme"}` to `{main, "api-reference"}` to fix "readme.html not found" error)
- Hex package configuration to include all necessary assets and documentation
- Documentation organization for professional first impression

## [0.6.0] - 2025-11-15

### Changed
- **BREAKING**: Renamed environment variable from `GATEWAY_REALM` to `MACULA_REALM` for better API consistency
  - All `MACULA_*` environment variables now follow consistent naming
  - Applies to both gateway mode and edge peer mode
  - Update your deployment configurations to use `MACULA_REALM` instead of `GATEWAY_REALM`

### Added
- Comprehensive Kademlia DHT architecture documentation (`docs/KADEMLIA_DHT_ARCHITECTURE.md`)
  - XOR distance metric explanation
  - K-bucket routing table details
  - DHT operations (PING, STORE, FIND_NODE, FIND_VALUE)
  - Iterative lookup algorithm
  - Macula-specific adaptations (realm-scoped DHT, HTTP/3 transport)
  - Performance characteristics and comparisons

### Fixed
- Updated documentation to reflect `MACULA_REALM` environment variable usage
- Updated `entrypoint.sh`, `Dockerfile.gateway`, and `config/sys.config` to use `MACULA_REALM`

### Upcoming in v0.7.0
- Architecture improvement: Separation of `macula_connection` into `macula_peer` (high-level mesh API) and `macula_connection` (low-level QUIC transport)
- See `docs/NOMENCLATURE_PROPOSAL_CONNECTION_TO_PEER.md` and `docs/PEER_CONNECTION_SEPARATION_PLAN.md` for details
- Expected timeline: 4-5 weeks after v0.6.0 release

### Migration Guide (0.5.0 → 0.6.0)

If you're using Macula in gateway mode or configuring realm multi-tenancy:

**Before (0.5.0):**
```bash
export GATEWAY_REALM=my-app
```

**After (0.6.0):**
```bash
export MACULA_REALM=my-app
```

**Elixir/Phoenix runtime.exs:**
```elixir
# Before (0.5.0)
System.put_env("GATEWAY_REALM", realm)

# After (0.6.0)
System.put_env("MACULA_REALM", realm)
```

## [0.5.0] - 2025-11-14

### Added
- Initial public release
- HTTP/3 (QUIC) mesh networking platform
- Gateway mode for accepting incoming connections
- Edge peer mode for mesh participation
- Multi-tenancy via realm isolation
- Pub/Sub messaging with wildcard support
- RPC (request/response) patterns
- Service discovery and advertisement
- mDNS local discovery support
- Process registry via gproc
- Comprehensive documentation

### Known Issues
- Gateway mode requires proper TLS certificate configuration
- Certificates must have Subject Alternative Name (SAN) extension
- Docker deployments require proper file ownership (`--chown=app:app`)

---

[0.7.0]: https://github.com/macula-io/macula/compare/v0.6.7...v0.7.0
[0.6.7]: https://github.com/macula-io/macula/compare/v0.6.6...v0.6.7
[0.6.0]: https://github.com/macula-io/macula/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/macula-io/macula/releases/tag/v0.5.0
