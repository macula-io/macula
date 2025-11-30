# CLAUDE.md - Macula Project Guidelines

**Current Version**: v0.12.5 (November 2025)

## Version History

| Version | Date | Key Features |
|---------|------|--------------|
| v0.7.0 | Nov 2025 | Nomenclature refactoring (macula_peer/macula_connection) |
| v0.8.0 | Nov 2025 | Direct P2P via macula_peer_connector, DHT propagation |
| v0.9.0 | Nov 2025 | Platform Layer (Ra/Raft consensus, leader election) |
| v0.9.1 | Nov 2025 | LWW-Register CRDT foundation |
| v0.10.0 | Nov 2025 | Production hardening, stream caching, performance fixes |
| v0.11.0 | Nov 2025 | Security Hardening - TLS Certificate Verification (COMPLETED) |
| v0.11.1 | Nov 2025 | Hybrid Trust Model - Realm auth + TOFU fingerprints (24 tests) |
| v0.11.2 | Nov 2025 | Rate limiting + Audit logging for realm trust (37 tests) |
| v0.11.3 | Nov 2025 | DHT integration for fingerprint storage (41 tests) |
| v0.12.0 | Nov 2025 | NAT Traversal - Complete implementation (70 tests) - Hole punch, pooling, relay |
| v0.12.1 | Nov 2025 | NATS-style Async RPC - Direct P2P request/reply (14 tests) |
| v0.12.2 | Nov 2025 | Async DHT Discovery - Request queuing on cache miss (17 tests) |
| v0.12.3 | Nov 2025 | Pull-based Service Discovery - Prefetch on startup (22 tests) |
| v0.12.4 | Nov 2025 | Documentation fixes - Fixed 77 broken links in hexdocs (0 warnings) |
| v0.12.5 | Nov 2025 | PubSub delivery metrics, console colored output, bug fixes (gproc, quic errors) |

---

## Terminology (v0.8.5+ Always-On Architecture)

📋 **See `docs/GLOSSARY.md`** for complete terminology reference

Since v0.8.5, Macula uses an **always-on architecture** where every node has all capabilities. Key terms:

| Term | Definition |
|------|------------|
| **Node** | A Macula instance with all subsystems (Gateway, Bootstrap, Peer, Platform) |
| **Seed Node** | A well-known node address for initial mesh discovery (no special code) |
| **Peer** | Any connected node in the mesh |
| **Gateway System** | Subsystem for QUIC message routing (`src/macula_gateway_system/`) |
| **Bootstrap System** | Subsystem for DHT/discovery (`src/macula_bootstrap_system/`) |

**Deprecated Terms (pre-v0.8.5):**
- ❌ "Gateway mode" / "Edge peer mode" - No mode selection in v0.8.5+
- ❌ `MACULA_START_GATEWAY` - No longer used
- ❌ "The gateway" (as special node) - Use "seed node" instead

---

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

## ✅ v0.8.0 Direct P2P Architecture (COMPLETED - Nov 2025)

**COMPLETED**: v0.8.0 implemented a different approach than originally planned - direct P2P connections via `macula_peer_connector` with DHT used for service/subscriber discovery.

📋 **See `architecture/v0.8.0-OVERVIEW.md`** for current architecture
📋 **See `architecture/dht_routed_rpc.md`** for historical reference (planning doc)

**Original Plan (v0.7.x)**: Multi-hop Kademlia DHT routing
**Actual Implementation (v0.8.0)**: Direct P2P + DHT discovery

**Why the change?** Direct connections provide:
- ✅ Lower latency (1 hop vs O(log N) hops)
- ✅ Simpler debugging
- ✅ Better throughput
- ✅ NAT traversal via gateway relay (acceptable for v0.8.x-v0.10.x)

**Current Architecture (v0.8.0+)**:
```
RPC Flow:
  Client → DHT lookup → Direct QUIC → Provider → Response

PubSub Flow:
  Publisher → Bootstrap → DHT lookup → Direct to each Subscriber
```

**Key Module**: `macula_peer_connector.erl` - Fire-and-forget P2P QUIC connections

## ✅ v0.12.0 NAT Traversal - Complete Implementation (COMPLETED - Nov 2025)

**STATUS**: Core Implementation COMPLETE
**Hex Published**: v0.11.3
**Target**: Complete P2P mesh with 80%+ direct connections

📋 **See `architecture/V0.12.0_NAT_COMPLETE_PLAN.md`** for detailed implementation plan
📋 **See `architecture/NAT_TRAVERSAL_ROADMAP.md`** for status tracking

### v0.12.0 Scope (Consolidated from v0.12.x-v0.14.x)

v0.12.0 delivers **complete NAT traversal** in one release:

| Feature | Status | Impact |
|---------|--------|--------|
| Connection Pooling | ✅ Complete | 94.5% hit rate |
| Direct Hole Punching | ✅ Complete | Adaptive timing by NAT type |
| Hierarchical Relay | ✅ Complete | Load-based selection |

### What's Complete

**Phase 1 - Foundation:**
- ✅ NAT_PROBE/NAT_PROBE_REPLY message flow
- ✅ PUNCH_COORDINATE direct delivery
- ✅ Docker NAT simulation (Full Cone, Restricted, Symmetric)
- ✅ Chatter demo cross-NAT messaging

**Phase 2 - Bug Fixes:**
- ✅ Binary key handling in peer discovery
- ✅ gproc registration conflicts (peer_id uniqueness)
- ✅ REPLY message routing
- ✅ MACULA_HOSTNAME configuration

**Phase 3 - Connection Pooling:**
- ✅ `macula_peer_connection_pool.erl` - ETS-based pooling with 94.5% hit rate
- ✅ Pool integration in peer_connector
- ✅ LRU eviction

**Phase 4 - Complete Hole Punching:**
- ✅ `macula_nat_detector.erl` - Local port/IP/public IP detection fixed
- ✅ `macula_hole_punch.erl` - gen_server with cancellation + adaptive timing
- ✅ `macula_connection_upgrade.erl` - Relay → direct upgrade (10 tests)

**Phase 5 - Hierarchical Relay:**
- ✅ `macula_relay_registry.erl` - Relay tracking + selection (18 tests)
- ✅ Load-based relay selection integrated
- ✅ DHT integration for distributed relay discovery

### NAT Message Types (0x50-0x5F)

| Type | ID | Purpose |
|------|-----|---------|
| NAT_PROBE | 0x50 | Request reflexive address from observer |
| NAT_PROBE_REPLY | 0x51 | Return reflexive address to requester |
| PUNCH_REQUEST | 0x52 | Request hole punch coordination |
| PUNCH_COORDINATE | 0x53 | Synchronized punch timing info |
| PUNCH_RESULT | 0x55 | Report punch success/failure |
| RELAY_REQUEST | 0x56 | Request relay tunnel setup |
| RELAY_DATA | 0x57 | Relayed data frame |

### Key Files

**NAT System:**
- `src/macula_nat_system/` - NAT detection, coordination, hole punching
- `src/macula_gateway.erl` - NAT_PROBE and PUNCH_COORDINATE handlers
- `docker/nat-test/` - Docker NAT simulation

**Tests (70 NAT tests):**
- `test/macula_nat_integration_tests.erl` - 28 integration tests
- `test/macula_hole_punch_tests.erl` - 14 unit tests
- `test/macula_connection_upgrade_tests.erl` - 10 unit tests
- `test/macula_relay_registry_tests.erl` - 18 unit tests

### Success Metrics

| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| Connection pool hit rate | 94.5% | 95%+ | ✅ Close |
| Direct P2P | Pending | 80%+ | ⏳ TBD |
| Max nodes | ~100 | 1000+ | ⏳ TBD |

## ✅ v0.12.1/v0.12.2/v0.12.3 NATS-style Async RPC (COMPLETED - Nov 2025)

**STATUS**: ✅ COMPLETE (v0.12.3 adds pull-based service discovery)
**Tests**: 22 unit tests passing

📋 **See `architecture/NATS_STYLE_ASYNC_RPC.md`** for detailed design document

### What's Implemented

NATS-style asynchronous RPC as a **first-class citizen** in the macula library, with direct P2P delivery (not routed through Bootstrap pub/sub).

**Key Features:**
- Fire-and-forget request pattern with inbox callbacks
- Direct P2P delivery between peers
- Local handler registration for same-node optimization
- Protocol message types: `rpc_request` (0x24), `rpc_reply` (0x25)
- Callback patterns: function callbacks and PID-based callbacks
- **v0.12.2:** Async DHT discovery - requests on cache miss queue while DHT lookup runs
- **v0.12.3:** Pull-based service discovery - prefetch known services on startup

### API Usage

**Register a local procedure handler:**
```erlang
Handler = fun(Args) ->
    %% Process request, return response
    {ok, #{<<"result">> => <<"value">>}}
end,
macula_rpc_handler:register_local_procedure(RpcPid, <<"my.procedure">>, Handler).
```

**Make an async RPC request (auto-discover provider):**
```erlang
%% With function callback
Callback = fun(Result) ->
    case Result of
        {ok, Response} -> io:format("Got: ~p~n", [Response]);
        {error, Reason} -> io:format("Error: ~p~n", [Reason])
    end
end,
{ok, RequestId} = macula_rpc_handler:request(RpcPid, <<"my.procedure">>, Args, #{callback => Callback}).

%% With PID callback (receives {rpc_response, RequestId, Result})
{ok, RequestId} = macula_rpc_handler:request(RpcPid, <<"my.procedure">>, Args, #{callback => self()}).
```

**Make request to specific node:**
```erlang
{ok, RequestId} = macula_rpc_handler:request_to(RpcPid, TargetNodeId, <<"my.procedure">>, Args, #{callback => Callback}).
```

**Pull-based service discovery (v0.12.3):**
```erlang
%% Configure at init time - prefetch services when connection manager is set
{ok, RpcPid} = macula_rpc_handler:start_link(#{
    node_id => NodeId,
    realm => Realm,
    peer_id => PeerId,
    service_interests => [<<"ping.handler">>, <<"user.service">>]  %% Services to prefetch
}).

%% Or dynamically prefetch at runtime
macula_rpc_handler:prefetch_services(RpcPid, [<<"new.service">>]).

%% Query configured interests
Interests = macula_rpc_handler:get_service_interests(RpcPid).
%% => [<<"ping.handler">>, <<"user.service">>, <<"new.service">>]
```

### Protocol Message Types

| Type | ID | Purpose |
|------|-----|---------|
| RPC_REQUEST | 0x24 | Async request with procedure, args, from_node |
| RPC_REPLY | 0x25 | Response with result or error |

### Message Format

**rpc_request:**
```erlang
#{
    request_id => <<"unique-request-id">>,
    procedure => <<"service.method">>,
    args => #{...},
    from_node => <<"requester-node-id">>
}
```

**rpc_reply:**
```erlang
#{
    request_id => <<"unique-request-id">>,
    from_node => <<"provider-node-id">>,
    result => #{...}  %% or error => #{code => ..., message => ...}
}
```

### Key Files

**Implementation:**
- `src/macula_rpc_system/macula_rpc_handler.erl` - Core RPC handler with async support
- `src/macula_rpc_system/macula_service_registry.erl` - Local handler registry
- `src/macula_protocol_types.erl` - Protocol message type definitions (0x24, 0x25)
- `src/macula_protocol_encoder.erl` - Message validation and encoding
- `src/macula_ping_pong.erl` - Example implementation using async RPC

**Tests:**
- `test/macula_async_rpc_tests.erl` - 14 unit tests

### Test Coverage

| Test Category | Tests | Description |
|---------------|-------|-------------|
| Local handler | 7 | Handler registration, invocation, args, errors |
| Protocol types | 5 | Message type IDs, validation, binary keys |
| Async reply | 2 | Unknown request handling, unique IDs |

### Design Principles

1. **Local-first optimization** - Requests to locally-registered handlers execute immediately without network
2. **Direct P2P** - Replies route directly to requester, not through Bootstrap
3. **Callback flexibility** - Function callbacks for simple cases, PID callbacks for complex flows
4. **Request tracking** - Unique request IDs prevent response confusion
5. **Graceful degradation** - Unknown replies are logged and ignored (no crash)

### Integration with NAT Traversal

Async RPC integrates with v0.12.0 NAT traversal:
- Uses connection pool for P2P delivery
- Falls back to relay when direct connection fails
- Hole-punched connections preferred for low latency

## ✅ v0.9.0 Platform Layer (COMPLETED - Nov 2025)

**COMPLETED**: v0.9.0 added distributed coordination primitives for workload applications.

📋 **See `architecture/PLATFORM_VISION.md`** for complete Platform Layer vision
📋 **See `architecture/v0.8.0-ROADMAP.md`** for roadmap (file name is historical)

**Problem Solved**: Workload applications had no way to:
- Elect a single coordinator across peers
- Share state in eventually-consistent manner
- Coordinate distributed operations

**Platform Layer Features (v0.9.0+)**:
- **Ra/Raft Consensus** - Leader election via `macula_platform_ra`
- **LWW-Register CRDT** - Eventually-consistent state sharing (v0.9.1)
- **Workload Registration** - `macula_client:register_workload/2`
- **Leader Queries** - `macula_client:get_leader/1`

**Key Modules**:
- `macula_platform_system/` - Platform layer subsystem
- `macula_platform_ra.erl` - Ra/Raft cluster integration
- `macula_platform_api.erl` - Client-facing API

---

## v0.10.0 Architecture Diagrams

### PubSub Message Flow (Current Implementation)

```
┌─────────────┐                           ┌─────────────────┐
│  Publisher  │                           │    Bootstrap    │
│  (Peer 1)   │                           │    Gateway      │
└──────┬──────┘                           └────────┬────────┘
       │                                           │
       │ 1. PUBLISH(topic, msg)                    │
       │ ─────────────────────────────────────────>│
       │    (via macula_connection)                │
       │                                           │
       │                                    ┌──────▼──────┐
       │                                    │ DHT Lookup  │
       │                                    │ topic →     │
       │                                    │ subscribers │
       │                                    └──────┬──────┘
       │                                           │
       │                                    ┌──────▼──────┐
       │                                    │   For each  │
       │                                    │  subscriber │
       │                                    └──────┬──────┘
       │                                           │
       │                     ┌─────────────────────┼─────────────────────┐
       │                     │                     │                     │
       │              ┌──────▼──────┐       ┌──────▼──────┐       ┌──────▼──────┐
       │              │  Peer 2     │       │  Peer 3     │       │  Peer N     │
       │              │  endpoint   │       │  endpoint   │       │  endpoint   │
       │              └──────┬──────┘       └──────┬──────┘       └──────┬──────┘
       │                     │                     │                     │
       │              2. pubsub_route        2. pubsub_route       2. pubsub_route
       │              ───────┼────────>     ───────┼────────>     ───────┼────────>
       │                     │                     │                     │
       │              ┌──────▼──────┐       ┌──────▼──────┐       ┌──────▼──────┐
       │              │ Subscriber  │       │ Subscriber  │       │ Subscriber  │
       │              │ callback()  │       │ callback()  │       │ callback()  │
       │              └─────────────┘       └─────────────┘       └─────────────┘
```

### RPC Message Flow (Current Implementation)

```
┌─────────────┐                           ┌─────────────────┐
│   Client    │                           │    Bootstrap    │
│  (Peer 1)   │                           │    Gateway      │
└──────┬──────┘                           └────────┬────────┘
       │                                           │
       │ 1. RPC call(procedure, args)              │
       │ ─────────────────────────────────────────>│
       │    (via macula_connection)                │
       │                                           │
       │                                    ┌──────▼──────┐
       │                                    │ DHT Lookup  │
       │                                    │ procedure → │
       │                                    │  providers  │
       │                                    └──────┬──────┘
       │                                           │
       │                                    ┌──────▼──────┐
       │                                    │  Provider   │
       │                                    │  endpoint   │
       │                                    └──────┬──────┘
       │                                           │
       │                    2. RPC_CALL            │
       │                    ────────────────────── │ ────────────────────┐
       │                                           │                     │
       │                                           │              ┌──────▼──────┐
       │                                           │              │  Provider   │
       │                                           │              │  (Peer 2)   │
       │                                           │              └──────┬──────┘
       │                                           │                     │
       │                                           │              ┌──────▼──────┐
       │                                           │              │  Execute    │
       │                                           │              │  handler()  │
       │                                           │              └──────┬──────┘
       │                                           │                     │
       │                    3. RPC_REPLY           │                     │
       │<────────────────────────────────── ────── │ ────────────────────┘
       │                                           │
       │                                           │
```

### DHT Service Registration Flow

```
┌─────────────┐                           ┌─────────────────┐
│  Provider   │                           │    Bootstrap    │
│  (Peer 2)   │                           │    Gateway      │
└──────┬──────┘                           └────────┬────────┘
       │                                           │
       │ 1. register(procedure, handler)           │
       │ ─────────────────────────────────────────>│
       │    (via macula_service_registry)          │
       │                                           │
       │                                    ┌──────▼──────┐
       │                                    │  DHT STORE  │
       │                                    │  key: proc  │
       │                                    │  value: {   │
       │                                    │   node_id,  │
       │                                    │   endpoint  │
       │                                    │  }          │
       │                                    └──────┬──────┘
       │                                           │
       │                                    ┌──────▼──────┐
       │                                    │ Propagate   │
       │                                    │ to k=20     │
       │                                    │ closest     │
       │                                    │ nodes       │
       │                                    └─────────────┘
```

### Subscription Advertisement Flow

```
┌─────────────┐                           ┌─────────────────┐
│ Subscriber  │                           │    Bootstrap    │
│  (Peer 3)   │                           │    Gateway      │
└──────┬──────┘                           └────────┬────────┘
       │                                           │
       │ 1. subscribe(topic, callback)             │
       │ ─────────────────────────────────────────>│
       │    (via macula_pubsub_dht)                │
       │                                           │
       │                                    ┌──────▼──────┐
       │                                    │  DHT STORE  │
       │                                    │  key: topic │
       │                                    │  value: {   │
       │                                    │   node_id,  │
       │                                    │   endpoint  │
       │                                    │  }          │
       │                                    └──────┬──────┘
       │                                           │
       │                                    ┌──────▼──────┐
       │                                    │ Propagate   │
       │                                    │ to k=20     │
       │                                    │ closest     │
       │                                    │ nodes       │
       │                                    └─────────────┘
```

---

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

## ✅ v0.11.0 Security Hardening - TLS Certificate Verification (COMPLETED - Nov 2025)

**COMPLETED**: v0.11.0 implemented centralized TLS configuration with two-mode operation.

📋 **See `docs/operator/TLS_CONFIGURATION.md`** for operator documentation

**Problem Solved**: Previous TLS configuration used `{verify, none}` everywhere, accepting any certificate.

**Implementation:**
- Created `macula_tls.erl` - centralized TLS configuration module
- **Production mode**: Strict certificate verification with CA bundle, hostname verification
- **Development mode**: Self-signed certificates, no verification (default)
- Environment variable configuration: `MACULA_TLS_MODE`, `MACULA_TLS_CACERTFILE`, etc.

**Key Files:**
- `src/macula_tls.erl` - Centralized TLS configuration
- `config/sys.config` - TLS configuration options
- `scripts/setup-dev-tls.sh` - Development certificate generation
- `docs/operator/TLS_CONFIGURATION.md` - Operator documentation
- `test/macula_tls_tests.erl` - 29 tests passing

**TLS Configuration Options:**

| Environment Variable | Application Config | Description |
|---------------------|-------------------|-------------|
| `MACULA_TLS_MODE` | `{tls_mode, production}` | `production` or `development` |
| `MACULA_TLS_CACERTFILE` | `{tls_cacertfile, Path}` | CA bundle path |
| `MACULA_TLS_CERTFILE` | `{tls_certfile, Path}` | Certificate path |
| `MACULA_TLS_KEYFILE` | `{tls_keyfile, Path}` | Private key path |
| `MACULA_TLS_VERIFY_HOSTNAME` | `{tls_verify_hostname, true}` | Hostname verification |

**Quick Start:**
```erlang
%% Production
{macula, [{tls_mode, production}, {tls_cacertfile, "/etc/ssl/certs/ca-certificates.crt"}]}

%% Development (default)
{macula, [{tls_mode, development}]}
```

---

## 🚀 QUIC Distribution (DEFERRED - v1.1.0+)

**STATUS**: Deferred until after v1.0.0 is proven in production

📋 **See `architecture/archive/` for historical planning documents**

**Problem Solved**: Replace EPMD and TCP-based Erlang distribution with QUIC:
- ❌ EPMD is centralized (single point of failure)
- ❌ TCP requires multiple ports and doesn't traverse NAT well
- ❌ No built-in encryption (TLS is optional add-on)
- ❌ Edge/mobile deployment is nearly impossible

**Solution**: QUIC-native distribution with decentralized discovery:

```
Before (EPMD + TCP):           After (Macula QUIC):
┌─────────┐                    ┌─────────┐
│  EPMD   │◄──TCP:4369──►      │ Macula  │◄══QUIC/UDP══►
│(daemon) │                    │Discovery│  (single port)
└────┬────┘                    │ (DHT)   │
     │                         └────┬────┘
┌────▼────┐                    ┌────▼────┐
│inet_tcp │◄──TCP range──►     │macula   │◄══QUIC══►
│  _dist  │                    │  _dist  │
└─────────┘                    └─────────┘
```

**Implementation Status**:

| Module | Purpose | Status |
|--------|---------|--------|
| `macula_dist.erl` | QUIC carrier for Erlang distribution | ✅ Complete |
| `macula_dist_discovery.erl` | DHT-based node discovery (replaces EPMD) | ✅ Complete |
| `macula_cluster_strategy.erl` | libcluster-compatible strategy | ✅ Complete |
| `macula_dist_system.erl` | Supervisor for dist subsystem | ✅ Complete |

**Tests**: 31 tests passing

**Usage** (once stable):
```
%% vm.args
-proto_dist macula
-no_epmd
-start_epmd false
-macula_dist_port 4433
```

**Key Benefits**:
- ✅ Built-in TLS 1.3 (mandatory, not optional)
- ✅ Single UDP port (NAT-friendly)
- ✅ Decentralized discovery (no EPMD daemon)
- ✅ Connection migration (survives IP changes)
- ✅ Compatible with Horde, Swarm, Mnesia, :pg

**Next Steps**:
- Integration testing with multi-node clusters
- Performance benchmarking vs inet_tcp_dist
- Mnesia replication verification
- Documentation for ecosystem tools

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

## Bash Scripting Guidelines

### NEVER Use HEREDOC

**CRITICAL**: Do NOT use heredoc syntax (cat <<'EOF' ... EOF) in bash commands or git commit messages.

#### ❌ Bad: Using HEREDOC
```bash
git commit -m "$(cat <<'EOF'
Commit message here.

Co-Authored-By: Claude <noreply@anthropic.com>
EOF
)"
```

#### ✅ Good: Direct command with proper quoting
```bash
git commit -m "Commit message here.

Co-Authored-By: Claude <noreply@anthropic.com>"
```

**Why?**
- HEREDOC syntax is fragile and error-prone in automated contexts
- Can cause parsing issues with quotes and special characters
- Makes commands harder to read and maintain
- Not necessary when proper quoting works fine

**Key Rules:**
1. Use direct strings with proper quoting (single or double quotes)
2. Use -F flag for git commit if message is in a file
3. Prefer shell scripts over complex one-liners
4. Keep bash commands simple and readable

## Version Management & Documentation Guidelines

### ⚠️ CRITICAL: Documentation Updates Are MANDATORY

**Documentation is of PRIMORDIAL importance.** After ANY work session:

1. **ALWAYS update planning documents** - Roadmaps, architecture docs, status summaries
2. **ALWAYS update CLAUDE.md** - Version history, feature status, architecture changes
3. **ALWAYS sync documentation with code** - If code changed, docs must change
4. **NEVER leave stale documentation** - Outdated docs are worse than no docs

**Stakeholders depend on accurate documentation:**
- **Developers** need current architecture and API docs
- **Operators** need deployment and configuration guides
- **Product owners** need accurate roadmaps and status
- **Future Claude sessions** need updated context

**Before ending ANY session:**
- [ ] Planning docs updated (ROADMAP.md, architecture/*.md)
- [ ] CLAUDE.md version history updated
- [ ] Feature status marked complete/in-progress
- [ ] New files/modules documented
- [ ] Test counts updated

### ALWAYS Check Version Before Making Changes

Before implementing any feature or fix:
1. **Read CLAUDE.md** - Check current version in header
2. **Read ROADMAP.md** - Check current milestones and what's complete
3. **Determine next version** - Follow semver (MAJOR.MINOR.PATCH)

### Version Numbering

| Change Type | Version Bump | Example |
|-------------|--------------|---------|
| Breaking API change | MAJOR | v0.x.x → v1.0.0 |
| New feature (backwards compatible) | MINOR | v0.11.x → v0.12.0 |
| Bug fix, documentation, tests | PATCH | v0.11.1 → v0.11.2 |

### After Completing Work - ALWAYS Update

1. **CLAUDE.md Version History Table**
   - Add new version row with date and key features
   - Update "Current Version" in header

2. **ROADMAP.md**
   - Update current version in header
   - Mark completed items with [x] and status
   - Add new files/tests to file lists

3. **Sync All Documentation**
   - `docs/GLOSSARY.md` - If terminology changed
   - `docs/operator/*.md` - If operator-facing changes
   - ADRs in `architecture/decisions/` - If architectural decisions made

### Release Preparation Checklist

Before publishing a release:
- [ ] All tests passing (`rebar3 eunit`)
- [ ] Version updated in CLAUDE.md and ROADMAP.md
- [ ] New features documented with test counts
- [ ] ADRs created for architectural decisions
- [ ] GLOSSARY.md updated if terminology changed

### Example: Adding a Feature

```
1. Check: CLAUDE.md says "v0.11.1"
2. Plan: This is a new feature → bump to v0.11.2
3. Implement with TDD
4. Update CLAUDE.md: Add "v0.11.2 | Nov 2025 | Feature X (N tests)"
5. Update ROADMAP.md: Mark feature complete, update version
6. Commit with version in message
```
