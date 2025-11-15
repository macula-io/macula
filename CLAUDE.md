# CLAUDE.md - Macula Project Guidelines

## ✅ v0.7.0 Nomenclature Refactoring (COMPLETED - Nov 2025)

**COMPLETED**: Macula v0.7.0 introduces clearer nomenclature following industry standards (libp2p, IPFS, BitTorrent):

**Module Renames:**
- `macula_connection` → `macula_peer` (mesh participant facade - high-level API)
- `macula_connection_manager` → `macula_connection` (QUIC transport layer - low-level)

**Why?** The original naming was confusing:
- ❌ `macula_connection` served both facade AND transport roles
- ❌ Mixed high-level mesh operations with low-level QUIC handling
- ❌ Not aligned with P2P industry standards

**After v0.7.0:**
- ✅ `macula_peer` = mesh participant (clear high-level API for pub/sub, RPC, DHT)
- ✅ `macula_connection` = QUIC transport (clear low-level transport layer)
- ✅ Follows libp2p/IPFS/BitTorrent naming conventions

**Status:**
- ✅ All modules renamed and tested (1,486 tests passing)
- ✅ Comprehensive test coverage for transport layer (36 tests)
- ✅ Clean separation of concerns achieved
- ✅ No regressions introduced

**Migration Guide:**
- Replace `macula_connection:start_link/2` → `macula_peer:start_link/2`
- Replace `macula_connection:publish/3` → `macula_peer:publish/3`
- Replace `macula_connection:subscribe/3` → `macula_peer:subscribe/3`
- Replace `macula_connection:call/3` → `macula_peer:call/3`
- All other high-level API calls follow the same pattern

**Note:** The refactoring maintains backward compatibility at the supervision tree level - internal modules continue to use `macula_connection` for QUIC transport operations.

## 🚧 DHT-Routed RPC Architecture (IN PROGRESS)

**IMPORTANT**: The RPC system is being refactored from direct client→provider connections to proper multi-hop Kademlia DHT routing. This is a fundamental architectural change to align with true P2P mesh principles.

📋 **See `architecture/dht_routed_rpc.md`** for complete design
📊 **See `architecture/dht_rpc_implementation_status.md`** for current status

**Why?** The original RPC used direct connections which violated mesh architecture:
- ❌ Client/server model (not true P2P)
- ❌ O(N²) connection scaling
- ❌ NAT traversal problems
- ❌ Connection idle timeout issues

**New Approach:** Messages hop through DHT using Kademlia XOR distance routing:
- ✅ True P2P mesh (no client/server distinction)
- ✅ O(log N) routing hops
- ✅ NAT-friendly (routes through existing connections)
- ✅ No new connections per RPC call

**Status:**
- ✅ Protocol type added (`rpc_route`)
- ✅ Encoding/decoding support
- ⏳ Routing logic module
- ⏳ Gateway integration
- ⏳ Connection integration
- ⏳ Multi-hop test topology

**DO NOT** rely on direct endpoint connections in new code - they will be removed.

## 🚀 NAT Traversal & P2P Connectivity Roadmap (v0.8.0 - v0.9.0)

**PLANNING**: Enabling direct peer-to-peer connections for edge nodes behind NAT/firewalls.

📋 **See `architecture/NAT_TRAVERSAL_ROADMAP.md`** for complete roadmap and technical design

**Problem**: Edge peers behind NAT cannot accept incoming connections, forcing all traffic through gateway relay (bottleneck).

**Solution Phases**:

### v0.8.0: Opportunistic Hole Punching (Q2 2025)
- **Goal**: 80% direct P2P + 20% relay fallback = 100% connectivity
- **New Modules**:
  - `macula_nat_discovery.erl` - Detect public IP and NAT type
  - `macula_hole_punch.erl` - Coordinate simultaneous connection attempts
  - `macula_connection_upgrade.erl` - Migrate relay → direct
- **Strategy**: Attempt direct while keeping relay as fallback
- **Timeline**: 6-8 weeks development

### v0.9.0: Full STUN/TURN/ICE (Q4 2025)
- **Goal**: 95% direct P2P + 5% relay = 100% connectivity
- **New Infrastructure**:
  - STUN server for address discovery
  - TURN relay for symmetric NAT
- **New Modules**:
  - `macula_stun_client.erl` - STUN address discovery
  - `macula_turn_client.erl` - TURN relay allocation
  - `macula_ice_agent.erl` - ICE-like candidate gathering
  - `macula_candidate_exchange.erl` - Exchange candidates via gateway
  - `macula_connection_strategy.erl` - Select optimal connection method
- **Strategy**: WebRTC-inspired NAT traversal (ICE/STUN/TURN)
- **Timeline**: 8-10 weeks development

**Why NOT WebTransport?**
- ❌ WebTransport is client→server, not peer↔peer
- ❌ Does NOT solve NAT traversal (peers behind NAT still can't accept connections)
- ❌ Designed for browsers connecting to servers, not symmetric P2P mesh
- ✅ We're using WebRTC NAT traversal principles instead

**Current State (v0.7.x)**:
- ✅ Gateway relay works 100% (universal fallback)
- ⚠️ All traffic goes through gateway (bottleneck)
- ⏳ No direct P2P yet

## Code Quality & Test Coverage

📋 **See `CODE_REVIEW_REPORT.md`** for comprehensive code quality analysis and improvement roadmap.

**Current Status:**
- **Test Coverage:** Improved with v0.7.0 refactoring (36 transport tests)
- **Health Score:** 6.2/10
- **Architecture:** Cleaner separation achieved in v0.7.0

**Before ANY refactoring:**
1. Read CODE_REVIEW_REPORT.md
2. Establish test coverage (Phase 1: 2-3 weeks)
3. Only then proceed with code improvements

## 🔧 God Module Refactoring (SUPERSEDED by v0.7.0 - Nov 2025)

**NOTE**: The original god module refactoring plan has been superseded by v0.7.0's nomenclature refactoring, which achieved similar goals through a different approach.

**Original Plan (Archived):**
- The plan called for refactoring `macula_connection.erl` (2,030 LOC) into 6 focused modules
- See `architecture/god_module_refactoring_plan.md` for historical reference
- See `architecture/macula_connection_behaviors.md` for behavior catalog

**What v0.7.0 Achieved Instead:**
- ✅ Clear separation: `macula_peer` (facade) vs `macula_connection` (transport)
- ✅ Supervision tree properly delegates to specialized handlers:
  - `macula_connection` - QUIC transport layer
  - `macula_pubsub_handler` - Pub/Sub operations
  - `macula_rpc_handler` - RPC operations
  - `macula_advertisement_manager` - DHT service advertisements
- ✅ Comprehensive test coverage for transport layer (36 tests)
- ✅ All tests passing (1,486 tests total)

**Result:** The v0.7.0 refactoring provides the clarity and maintainability originally sought, using a more pragmatic approach focused on nomenclature and API design rather than wholesale module extraction.

## 🔧 Gateway Refactoring (COMPLETED - Jan 2025)

**COMPLETED**: Successfully extracted 6 focused modules from `macula_gateway.erl` using TDD, created supervision tree, and integrated all modules. Gateway now properly delegates to child modules and acts as orchestrator rather than implementer.

**Why?** The gateway module had 6 mixed responsibilities requiring separation:
- ✅ Client lifecycle management - EXTRACTED & INTEGRATED
- ✅ Pub/Sub message routing - EXTRACTED & INTEGRATED
- ✅ RPC handler registration - EXTRACTED & INTEGRATED
- ✅ Mesh connection pooling - EXTRACTED & INTEGRATED
- ✅ DHT query forwarding - EXTRACTED & INTEGRATED
- ✅ Multi-hop RPC routing - EXTRACTED & INTEGRATED
- ✅ Supervision tree - CREATED & INTEGRATED
- ⏳ QUIC listener management (Phase 5 - deferred)

**Extracted & Integrated Modules:**
- ✅ `macula_gateway_client_manager.erl` - Client lifecycle (~235 LOC, 24 tests)
- ✅ `macula_gateway_pubsub.erl` - Pub/Sub routing with wildcards (~280 LOC, 31 tests)
- ✅ `macula_gateway_rpc.erl` - RPC handler management (~215 LOC, 20 tests)
- ✅ `macula_gateway_mesh.erl` - Mesh connection pooling (~295 LOC, 16 tests)
- ✅ `macula_gateway_dht.erl` - DHT query forwarding (~149 LOC, stateless)
- ✅ `macula_gateway_rpc_router.erl` - Multi-hop RPC routing (~265 LOC, 17 tests)

**Supervision Tree (Phase 6 - COMPLETED):**
- ✅ `macula_gateway_sup.erl` - Supervises all gateway workers (~113 LOC, 24 tests)
- Strategy: one_for_all (if any child crashes, restart all)
- Children: client_manager, pubsub, rpc, mesh
- All tests passing

**Integration & Cleanup (Phases 7, 11-12 - COMPLETED Jan 2025):**
- ✅ Gateway state refactored (removed duplicate fields, added child PIDs)
- ✅ Gateway init/1 starts supervisor and gets child PIDs
- ✅ All client lifecycle code delegates to client_manager
- ✅ All pub/sub operations delegate to pubsub module
- ✅ All RPC operations delegate to rpc module
- ✅ All mesh operations delegate to mesh module
- ✅ All DHT queries delegate to dht module
- ✅ All RPC routing delegates to rpc_router module
- ✅ Gateway terminate/2 properly stops supervisor
- ✅ Module documentation updated to reflect new architecture
- ✅ All gateway tests passing (49 tests, 0 failures)
- ✅ Gateway reduced from ~1500 LOC to ~879 LOC (clean orchestrator)

**Achievements:**
- Single Responsibility Principle: Each module has one clear purpose
- Idiomatic Erlang: Pattern matching, guards, no deep nesting
- Comprehensive tests: All tests passing
- Fault tolerance: Proper OTP supervision with one_for_all strategy
- Integration verified: Gateway properly delegates to child modules
- Clean facade pattern: Gateway orchestrates, child modules implement
- Modular architecture: Easy to test, maintain, and extend

**Architecture Summary:**
Gateway now acts as a **coordinator** with these responsibilities:
- QUIC listener management
- Message decoding & routing
- Supervisor coordination
- API facade

All business logic extracted to specialized modules following Single Responsibility Principle.

**Status:** Gateway refactoring complete. Phase 5 (QUIC listener extraction) deferred as current design is clean and maintainable.

## 🛡️ Memory Management & Leak Prevention (COMPLETED - Nov 2025)

**Status:** ✅ **PRODUCTION-READY** (Completed 2025-11-14)

Macula implements comprehensive memory management to prevent OOM crashes through **5 critical fixes** that bound memory usage and enable automatic cleanup.

📋 **See `architecture/memory_management/`** for complete documentation

**Problem Solved:** Platform experienced OOM crashes after 30-60 minutes due to unbounded data structures.

### 5 Critical Fixes Implemented

1. **Bounded Connection Pool** - `macula_gateway_mesh`
   - LRU eviction, max 1,000 connections
   - 22 tests passing
   - Prevents unbounded pool growth

2. **Client Connection Limits** - `macula_gateway_client_manager`
   - Backpressure, max 10,000 clients (configurable)
   - 30 tests passing
   - Graceful degradation under load

3. **Service TTL/Cleanup** - `macula_service_registry` + `macula_advertisement_manager`
   - 5-minute TTL, automatic cleanup every 60 seconds
   - 27 tests passing
   - Prevents stale service accumulation

4. **Stream Cleanup** - `macula_gateway_client_manager`
   - Coordinated map cleanup on disconnect
   - 32 tests passing
   - No orphaned stream entries

5. **Caller Process Monitoring** - `macula_rpc_handler`
   - Immediate cleanup on caller death
   - 27 tests passing
   - No 5-second timeout wait

**Total:** 138 tests (7 new tests added for memory leak fixes)

### Production Monitoring

Monitor these metrics to verify memory stability:
- **Connection pool size** - Should stay ≤ 1,000
- **Client count** - Should stay ≤ 10,000
- **Service registry size** - Should remain stable
- **Pending calls/queries** - Should trend to 0
- **Cleanup event frequency** - Check logs every 60s

**Key Logs:**
```erlang
[info] Service cleanup: removed 3 expired service(s)   % Normal
[debug] Service cleanup: no expired services           % Also normal
[warn] Client connection rejected: max_clients_reached % Monitor frequency
```

### Documentation

- **Overview:** `architecture/memory_management/README.md`
- **Diagrams:** `architecture/memory_management/diagrams/`
- **Implementation Details:** `architecture/memory_management/01_overview.md`
- **Housekeeping:** `architecture/memory_management/09_housekeeping_report.md`

### Code Quality

All fixes follow idiomatic Erlang patterns:
- ✅ Pattern matching on function heads (no `if` statements)
- ✅ Guards instead of `case` where possible
- ✅ Atomic state updates
- ✅ OTP best practices (process monitoring, timers)
- ✅ Comprehensive test coverage

**Before Fixes:**
- OOM crashes after 30-60 minutes
- Unbounded memory growth
- No cleanup mechanisms

**After Fixes:**
- Stable memory usage
- Bounded pools prevent growth
- Automatic cleanup maintains stability
- No OOM crashes observed

## Docker Build Best Practices

### Always Force Clean Builds After Code Changes

**Problem**: Docker build cache can use stale layers even after source code changes, leading to outdated code in the image.

**Solution**: Always prune build cache and rebuild from scratch when testing code changes:

```bash
# Clean approach - prune and rebuild
docker builder prune -af
docker compose -f docker-compose.multi-node-test.yml build --no-cache

# Or shorter version
docker system prune -af && docker compose -f <compose-file> build --no-cache
```

**Don't waste time**:
- ❌ Don't try to inspect Docker images to verify code
- ❌ Don't trust cached builds after code changes
- ❌ Don't use `docker run` to grep files inside images
- ✅ Just prune and rebuild from scratch

This ensures the Docker image always contains the latest compiled code.

## Testing Workflow

1. Make code changes
2. Compile: `rebar3 compile`
3. Prune Docker cache: `docker builder prune -af`
4. Rebuild image: `docker compose -f <compose-file> build --no-cache`
5. Run tests: `docker compose -f <compose-file> up`

## Multi-Node Testing

The `docker/docker-compose.multi-node-test.yml` file sets up a 4-node test environment:
- 1 registry node
- 3 provider nodes (advertise services)
- 1 client node (discovers and calls services)

Used for testing DHT service discovery, pubsub, and RPC functionality.

## Performance Optimization

For pub/sub throughput optimization recommendations, see:
- **`architecture/pubsub_optimization_recommendations.md`** - Comprehensive guide to improving pub/sub throughput from 500-2,000 msg/sec to 10,000+ msg/sec

Key optimizations (in priority order):
1. **DHT Result Caching** (5-10x improvement) - Cache discovered subscribers
2. **Direct Routing Table** (3-5x additional) - Route directly to known subscribers
3. **Adaptive Rate-Limiting** (2-3x for high-frequency) - Prevent discovery storms
4. **Persistent QUIC Streams** (1.5-2x) - Connection pooling
5. **Message Batching** (2x for bursts) - Batch publishes for same topic

## Coding Guidelines

Follow **Idiomatic Erlang** principles for all code:

### Core Principles
- **Avoid deep nesting** - Keep nesting to 1-2 levels maximum
- **Avoid `if` and `cond`** - Use pattern matching instead
- **Prefer multiple function clauses** over deep nesting
- **Prefer declarative style** over imperative style
- **Prefer guards** over `case` clauses
- **Avoid `try..catch`** - Not idiomatic Erlang (use pattern matching on return values)

### Examples

#### ❌ Bad: Using `if` and deep nesting
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
                            handle_response(Data, State);
                        true ->
                            {error, unknown_type}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        true ->
            {error, invalid_message}
    end.
```

#### ✅ Good: Multiple function clauses with pattern matching
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
    handle_response(Data, State);
handle_decoded_message(_Data, _State) ->
    {error, unknown_type}.
```

#### ❌ Bad: Using `try..catch`
```erlang
safe_divide(A, B) ->
    try
        Result = A / B,
        {ok, Result}
    catch
        error:badarith ->
            {error, division_by_zero}
    end.
```

#### ✅ Good: Pattern matching and guards
```erlang
safe_divide(_A, 0) ->
    {error, division_by_zero};
safe_divide(A, B) when is_number(A), is_number(B) ->
    {ok, A / B};
safe_divide(_A, _B) ->
    {error, invalid_arguments}.
```

#### ❌ Bad: `case` when guards would work
```erlang
validate_age(Age) ->
    case Age of
        N when N < 0 -> {error, negative_age};
        N when N > 150 -> {error, too_old};
        N -> {ok, N}
    end.
```

#### ✅ Good: Guards on function clauses
```erlang
validate_age(Age) when Age < 0 ->
    {error, negative_age};
validate_age(Age) when Age > 150 ->
    {error, too_old};
validate_age(Age) when is_integer(Age) ->
    {ok, Age};
validate_age(_Age) ->
    {error, invalid_age}.
```

### When `case` is Acceptable
Use `case` when you need to pattern match on a function result:

```erlang
process_request(Request, State) ->
    case validate_request(Request) of
        {ok, ValidRequest} -> handle_valid_request(ValidRequest, State);
        {error, Reason} -> {error, Reason}
    end.
```

### Key Takeaways
1. **Let it crash** - Don't catch errors unless you can handle them meaningfully
2. **Pattern match early** - Use function clause selection instead of conditionals
3. **Use guards liberally** - They're more readable than `case` or `if`
4. **Keep functions small** - Each function should do one thing
5. **Declarative > Imperative** - Express what you want, not how to get it
