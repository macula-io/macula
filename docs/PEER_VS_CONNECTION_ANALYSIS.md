# Peer vs Connection: Responsibility Analysis

**Date:** 2025-11-15
**Purpose:** Analyze whether to include connection→peer refactoring in v0.6.0

---

## Current Architecture

### `macula_connection` - Facade/Coordinator (270 LOC)

**Purpose:** SDK entry point and supervision tree coordinator

**Exports:**
```erlang
%% Lifecycle
start_link/2        %% Start connection to mesh
stop/1              %% Stop connection

%% Pub/Sub
publish/3           %% Publish event (no options)
publish/4           %% Publish event (with options)
subscribe/3         %% Subscribe to topic
unsubscribe/2       %% Unsubscribe from topic

%% RPC
call/3              %% RPC call (default timeout)
call/4              %% RPC call (with options)

%% Service Discovery
advertise/4         %% Advertise service handler
unadvertise/2       %% Stop advertising service
```

**Responsibilities:**
1. Start supervision tree (`macula_connection_sup`)
2. Delegate pub/sub to `macula_pubsub_handler`
3. Delegate RPC to `macula_rpc_handler`
4. Delegate service ads to `macula_advertisement_manager`
5. Provide facade API for applications

**State:**
```erlang
-record(state, {
    url :: binary(),
    realm :: binary(),
    node_id :: binary(),
    supervisor_pid :: pid(),
    connection_manager_pid :: pid(),
    pubsub_handler_pid :: pid(),
    rpc_handler_pid :: pid(),
    advertisement_manager_pid :: pid()
}).
```

---

### `macula_connection_manager` - QUIC Connection (350 LOC)

**Purpose:** Manage low-level QUIC connection lifecycle

**Exports:**
```erlang
start_link/2        %% Start QUIC connection manager
send_message/3      %% Send message via QUIC stream
get_status/1        %% Get connection status
```

**Responsibilities:**
1. Establish QUIC connection
2. Maintain QUIC stream
3. Send messages via QUIC
4. Receive and decode incoming messages
5. Route messages to handlers (pubsub, RPC)
6. Handle reconnection on failure

**State:**
```erlang
-record(state, {
    url :: binary(),
    opts :: map(),
    node_id :: binary(),
    realm :: binary(),
    connection :: pid() | undefined,  %% QUIC connection
    stream :: pid() | undefined,      %% QUIC stream
    status = connecting :: connecting | connected | disconnected | error,
    recv_buffer = <<>> :: binary()    %% Receive buffer
}).
```

---

## The Confusion

**Current Problem:**

```erlang
%% What does this represent?
macula_connection:start_link(URL, Opts)
```

**Developer might think:**
- ❌ Generic TCP/HTTP connection?
- ❌ Database connection?
- ❌ Any kind of connection abstraction?

**What it actually is:**
- ✅ A **peer participant** in the mesh network
- ✅ Handles pub/sub, RPC, service discovery
- ✅ Participates in DHT routing
- ✅ Has a node ID and realm membership

---

## Proposed Solution

### Rename: `macula_connection` → `macula_peer`

**Why?**
1. **Clarity**: "Peer" immediately communicates P2P mesh participation
2. **Industry Standard**: BitTorrent DHT, libp2p, IPFS all use "peer"
3. **Architectural Alignment**: Matches mesh networking terminology
4. **Consistency**: Gateway accepts connections from **peers**, not "connections"

---

## Responsibility Breakdown

### Option A: Simple Rename (Recommended for v0.6.0)

**Just rename the module, keep responsibilities identical:**

| Current Module | New Name | Responsibilities |
|---------------|----------|------------------|
| `macula_connection` | `macula_peer` | Facade, supervision, API |
| `macula_connection_manager` | `macula_peer_connection_manager` | QUIC connection lifecycle |
| `macula_connection_sup` | `macula_peer_sup` | Supervision tree |
| `macula_connection_pool` | `macula_peer_pool` | Connection pooling |

**API Changes:**
```erlang
%% Before
{ok, Conn} = macula_connection:start_link(URL, Opts),
macula_connection:publish(Conn, Topic, Data).

%% After
{ok, Peer} = macula_peer:start_link(URL, Opts),
macula_peer:publish(Peer, Topic, Data).
```

**Impact:**
- Simple find-and-replace rename
- No architectural changes
- No state/responsibility refactoring
- Low risk, high clarity benefit

---

### Option B: Separate Peer vs Connection (Future v0.7.0+)

**Split into two distinct concepts:**

#### `macula_peer` - Mesh Participant (High-Level)
```erlang
%% API
start_link/2        %% Join mesh as peer
stop/1              %% Leave mesh

%% Pub/Sub
publish/3           %% Publish to mesh
subscribe/3         %% Subscribe from mesh
unsubscribe/2       %% Unsubscribe

%% RPC
call/3              %% Call service in mesh
advertise/4         %% Advertise service
unadvertise/2       %% Stop advertising

%% DHT/Mesh Operations
discover_peers/1    %% Find peers in realm
get_peer_info/1     %% Get local peer info
```

**Responsibilities:**
- Mesh-level operations (pub/sub, RPC, service discovery)
- DHT participation
- Peer identity (node ID, realm)
- Application-facing API

#### `macula_connection` - QUIC Transport (Low-Level)
```erlang
%% API
connect/2           %% Establish QUIC connection
disconnect/1        %% Close connection
send/2              %% Send raw message
get_status/1        %% Connection status
```

**Responsibilities:**
- QUIC connection lifecycle
- Stream management
- Message encoding/decoding
- Reconnection logic
- Transport-level concerns

**Separation Benefits:**
1. **Single Responsibility**: Each module has ONE clear purpose
2. **Testability**: Can test mesh logic separate from transport
3. **Future-Proof**: Easy to add alternative transports (WebSocket, etc.)
4. **Clearer Architecture**: Peer = business logic, Connection = transport

**Separation Risks:**
1. More modules to maintain
2. Bigger API surface
3. More complex for simple use cases
4. Requires careful state management

---

## Recommendation for v0.6.0

### ✅ **Option A: Simple Rename**

**Rationale:**
1. **Low Risk**: Just renaming modules, no architectural changes
2. **High Value**: Massive clarity improvement for minimal effort
3. **Quick**: Can be done in 1-2 days with automated scripts
4. **Reversible**: Easy to adjust if issues found
5. **Foundation**: Sets up for Option B in future if needed

**Effort Estimate:**
- **Automated rename**: 2 hours (script + verify)
- **Update tests**: 2 hours
- **Update docs**: 2 hours
- **Integration testing**: 2 hours
- **Total**: ~1 day

**Files to Rename:**

| Current | New | Lines |
|---------|-----|-------|
| `src/macula_connection.erl` | `src/macula_peer.erl` | 270 |
| `src/macula_connection_manager.erl` | `src/macula_peer_connection_manager.erl` | 350 |
| `src/macula_connection_sup.erl` | `src/macula_peer_sup.erl` | 113 |
| `src/macula_connection_pool.erl` | `src/macula_peer_pool.erl` | 295 |
| `test/macula_connection_tests.erl` | `test/macula_peer_tests.erl` | 450 |
| `test/macula_connection_protocol_tests.erl` | `test/macula_peer_protocol_tests.erl` | 200 |
| (+ 124 other files with references) | | |

**Migration for Users:**

**Before (0.5.0):**
```erlang
{ok, Conn} = macula_connection:start_link(
    <<"https://gateway:4433">>,
    #{realm => <<"macula.arcade">>}
),
macula_connection:publish(Conn, <<"game.events">>, Data).
```

**After (0.6.0):**
```erlang
{ok, Peer} = macula_peer:start_link(
    <<"https://gateway:4433">>,
    #{realm => <<"macula.arcade">>}
),
macula_peer:publish(Peer, <<"game.events">>, Data).
```

**Deprecation Strategy (Optional):**
```erlang
%% macula_connection.erl (deprecated wrapper)
-module(macula_connection).
-deprecated([{start_link, 2}, {publish, 3}, ...]).

start_link(Url, Opts) ->
    io:format("WARNING: macula_connection is deprecated, use macula_peer~n"),
    macula_peer:start_link(Url, Opts).

publish(Peer, Topic, Data) ->
    io:format("WARNING: macula_connection is deprecated, use macula_peer~n"),
    macula_peer:publish(Peer, Topic, Data).
```

---

## Architecture After Rename

### Before (0.5.0):
```
┌─────────────────┐
│ macula_gateway  │ ← Gateway mode (central registry)
└─────────────────┘
         │
         │ What's the connection to?
         │
┌─────────────────┐
│macula_connection│ ← Unclear! Connection to what?
└─────────────────┘
```

### After (0.6.0):
```
┌─────────────────┐
│ macula_gateway  │ ← Central registry
└─────────────────┘
         │
         │ mesh connections
         │
┌─────────────────┐
│  macula_peer    │ ← Crystal clear: P2P mesh participant
└─────────────────┘
```

---

## Comparison: v0.6.0 vs v0.7.0 Scope

### v0.6.0 (Recommended):
- ✅ Rename modules (connection → peer)
- ✅ Update all references
- ✅ Update documentation
- ✅ Provide migration guide
- ✅ Optional: Deprecation wrappers
- ❌ No architectural changes
- ❌ No responsibility separation

**Breaking Changes:**
- Module names
- API calls
- Configuration keys
- Test names

**Timeline:** 1-2 days

---

### v0.7.0 (Future):
- ✅ All v0.6.0 changes
- ✅ Separate peer (mesh) from connection (transport)
- ✅ Refactor state management
- ✅ Split APIs into peer vs connection
- ✅ Support alternative transports?

**Breaking Changes:**
- Everything in v0.6.0
- API signatures
- State structures
- Supervision trees
- Internal protocols

**Timeline:** 2-3 weeks

---

## Decision Matrix

| Criteria | Option A (Rename) | Option B (Refactor) |
|----------|-------------------|---------------------|
| **Effort** | 1 day | 2-3 weeks |
| **Risk** | Low | Medium |
| **Clarity Gain** | High | Very High |
| **Breaking Changes** | Moderate | Severe |
| **User Impact** | Low (find-replace) | High (API redesign) |
| **Architectural Benefit** | Medium | High |
| **Timeline** | v0.6.0 (now) | v0.7.0 (future) |

---

## Recommendation

### ✅ **Include in v0.6.0: Simple Rename (Option A)**

**Why:**
1. Combines well with `MACULA_REALM` rename (both env/API consistency improvements)
2. Low risk, high clarity benefit
3. Can be done in 1 day
4. Sets foundation for future refactoring if needed
5. Users get clarity NOW instead of waiting months

**Version Bump:**
- Originally planned: 0.5.0 → 0.6.0 (MACULA_REALM only)
- New plan: 0.5.0 → 0.6.0 (MACULA_REALM + connection→peer)
- Still qualifies as 0.6.0 (breaking API changes, but not major architectural changes)

**CHANGELOG Entry:**
```markdown
## [0.6.0] - 2025-11-15

### Changed
- **BREAKING**: Renamed environment variable `GATEWAY_REALM` → `MACULA_REALM`
- **BREAKING**: Renamed module `macula_connection` → `macula_peer`
  - Better reflects peer-to-peer mesh architecture
  - Industry-standard terminology (BitTorrent DHT, libp2p, IPFS)
  - All `macula_connection_*` modules renamed to `macula_peer_*`

### Migration Guide
See `docs/MIGRATION_0.5_TO_0.6.md` for detailed migration steps
```

---

## Automated Rename Script

**Location:** `/home/rl/work/github.com/macula-io/macula/scripts/rename-connection-to-peer.sh`

**What it does:**
1. Rename all `macula_connection*.erl` → `macula_peer*.erl`
2. Update all module declarations (`-module(macula_connection)` → `-module(macula_peer)`)
3. Update all references in source files
4. Update all test files
5. Update documentation
6. Create backup of original files

**Verification:**
1. Compile: `rebar3 compile`
2. Run tests: `rebar3 eunit`
3. Check documentation: `rebar3 ex_doc`

---

## Conclusion

**Recommendation: YES, include connection→peer rename in v0.6.0**

This combines two breaking API improvements into one release:
1. `GATEWAY_REALM` → `MACULA_REALM` (consistency)
2. `macula_connection` → `macula_peer` (clarity)

Both changes improve API quality with minimal risk, and can be completed in 1-2 days.

**Next Steps:**
1. Get approval for combined v0.6.0 scope
2. Create automated rename script
3. Execute rename
4. Update tests
5. Update documentation
6. Publish to Hex
