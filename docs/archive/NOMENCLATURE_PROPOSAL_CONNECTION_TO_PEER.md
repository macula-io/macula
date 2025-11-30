# Nomenclature Proposal: Rename `macula_connection` to `macula_peer`

**Status:** 📋 Proposal
**Created:** 2025-11-15
**Target Version:** 0.7.0 (future major release)

## Problem Statement

The current naming `macula_connection` is **too generic** and doesn't clearly communicate its role in the mesh architecture. This causes confusion about the distinction between:

- **`macula_gateway`** - Central entry point accepting incoming connections
- **`macula_connection`** - Manages peer-to-peer mesh connections (unclear from name)

### Current Confusion

**What developers might think `macula_connection` does:**
- ❌ Generic connection management (TCP/QUIC sockets)
- ❌ HTTP client connections
- ❌ Database connections
- ❌ Any connection abstraction

**What it actually does:**
- ✅ Manages QUIC connections to **other mesh peers**
- ✅ Handles peer-to-peer pub/sub, RPC, service discovery
- ✅ Maintains routing table for DHT/Kademlia
- ✅ Forms the mesh topology at the edge

## Proposed Solution

Rename **`macula_connection`** → **`macula_peer`**

This makes the architecture **immediately clear**:

| Module | Role | Mode |
|--------|------|------|
| `macula_gateway` | Central registry & entry point | Gateway mode only |
| `macula_peer` | P2P mesh participant | Both modes |

### Clarity Benefits

**Before (unclear):**
```erlang
%% What does this connect to? A database? Another service?
macula_connection:start_link(Opts)
```

**After (crystal clear):**
```erlang
%% Obvious: this is a peer-to-peer mesh connection
macula_peer:start_link(Opts)
```

---

## Detailed Renaming Plan

### Modules to Rename

| Current Name | New Name | Purpose |
|-------------|----------|---------|
| `macula_connection.erl` | `macula_peer.erl` | Main peer connection logic |
| `macula_connection_manager.erl` | `macula_peer_manager.erl` | Lifecycle management |
| `macula_connection_pubsub.erl` | `macula_peer_pubsub.erl` | Peer pub/sub operations |
| `macula_connection_rpc.erl` | `macula_peer_rpc.erl` | Peer RPC handling |
| `macula_connection_advertisement.erl` | `macula_peer_advertisement.erl` | Service advertisement |
| `macula_connection_pool.erl` | `macula_peer_pool.erl` | Connection pooling |
| `macula_connection_provider.erl` | `macula_peer_provider.erl` | Provider selection |

### Test Files to Rename

| Current Name | New Name |
|-------------|----------|
| `macula_connection_tests.erl` | `macula_peer_tests.erl` |
| `macula_connection_protocol_tests.erl` | `macula_peer_protocol_tests.erl` |

### API Functions to Update

**Public API (SDK):**
```erlang
%% Before
macula_connection:connect(PeerURL, Realm, Opts)
macula_connection:disconnect(ConnPid)
macula_connection:publish(ConnPid, Topic, Msg)
macula_connection:subscribe(ConnPid, Topic, Handler)
macula_connection:call(ConnPid, Procedure, Args)

%% After
macula_peer:connect(PeerURL, Realm, Opts)
macula_peer:disconnect(PeerPid)
macula_peer:publish(PeerPid, Topic, Msg)
macula_peer:subscribe(PeerPid, Topic, Handler)
macula_peer:call(PeerPid, Procedure, Args)
```

---

## Architecture Clarity

### Before: Confusing Dual Names

```
┌─────────────────┐
│ macula_gateway  │ (Gateway mode - central registry)
└─────────────────┘
         │
         │ What's the difference?
         │
┌─────────────────┐
│ macula_connection│ (Edge peer? Connection? Unclear!)
└─────────────────┘
```

### After: Clear Hierarchy

```
┌─────────────────┐
│ macula_gateway  │ (Central registry - gateway mode)
└─────────────────┘
         │
         │ mesh connections
         │
┌─────────────────┐
│  macula_peer    │ (P2P mesh participant - edge mode)
└─────────────────┘
```

### Deployment Modes

**Gateway Mode:**
```
[macula_gateway] ← Accepts connections from many peers
        ↑
        │
        └─ Provides central registry
```

**Edge Peer Mode:**
```
[macula_peer] → Connects to gateway & other peers
        │
        └─ Participates in mesh DHT
```

**Hybrid Mode (Gateway + Peer):**
```
[macula_gateway] ← Accepts connections
        │
        └─ Also runs [macula_peer] → Participates in mesh
```

---

## Impact Analysis

### Breaking Changes

This is a **major breaking change** requiring:

1. **Version Bump:** 0.6.0 → **0.7.0** (major version)
2. **Migration Guide:** Document all renamed modules/functions
3. **Deprecation Period:** Optionally provide aliases for 1-2 releases

### Affected Code Locations

**Internal Code:**
- `src/macula_connection*.erl` → `src/macula_peer*.erl`
- `test/macula_connection*_tests.erl` → `test/macula_peer*_tests.erl`
- All references in `macula_sup.erl`, `macula_gateway.erl`, etc.

**External SDK Users:**
- Applications using `macula_connection:*` API calls
- Documentation, tutorials, examples
- Hex package description

**Estimated Files Affected:** ~30-40 files

---

## Migration Strategy

### Option 1: Hard Break (Recommended)

**Pros:**
- Clean codebase, no legacy baggage
- Forces users to update (clear signal)

**Cons:**
- All users must update code immediately

**Implementation:**
```erlang
%% Remove old module entirely
%% macula_connection.erl ❌ DELETED

%% New module
-module(macula_peer).
```

### Option 2: Deprecation with Aliases (Gentler)

**Pros:**
- Gradual migration path
- Existing code keeps working

**Cons:**
- Maintenance burden (two names for same thing)
- Confusion during transition period

**Implementation:**
```erlang
%% macula_connection.erl (deprecated wrapper)
-module(macula_connection).
-deprecated([{connect, 3}, {disconnect, 1}, ...]).

connect(URL, Realm, Opts) ->
    io:format("WARNING: macula_connection is deprecated, use macula_peer~n"),
    macula_peer:connect(URL, Realm, Opts).
```

**Recommendation:** Use **Option 1** (hard break) to avoid technical debt.

---

## Documentation Updates

### Files Requiring Updates

1. **README.md**
   - Replace all `macula_connection` references
   - Update code examples

2. **CHANGELOG.md**
   - Document breaking change
   - Provide migration examples

3. **Architecture Docs**
   - Update `docs/KADEMLIA_DHT_ARCHITECTURE.md`
   - Update `docs/QUIC_TLS_GATEWAY_SETUP.md`
   - Update any C4 diagrams

4. **API Documentation (ExDoc)**
   - Update module docstrings
   - Update function examples

5. **CLAUDE.md**
   - Update god module refactoring notes
   - Update nomenclature guidelines

---

## Timeline & Rollout Plan

### Phase 1: Proposal Review (1 week)
- ✅ Document proposal (this doc)
- [ ] Team review & approval
- [ ] Community feedback (GitHub issue?)

### Phase 2: Implementation (2 weeks)
- [ ] Rename all modules & tests
- [ ] Update internal references
- [ ] Run full test suite
- [ ] Update documentation

### Phase 3: Release (1 week)
- [ ] Version bump to 0.7.0
- [ ] Publish to Hex
- [ ] Announce breaking change
- [ ] Update examples & tutorials

**Total Time:** ~4 weeks

---

## Alternative Names Considered

| Name | Pros | Cons | Verdict |
|------|------|------|---------|
| `macula_peer` | ✅ Clear, concise | ❌ None | ✅ **RECOMMENDED** |
| `macula_mesh_peer` | ✅ Very explicit | ❌ Verbose | ⚠️ Acceptable |
| `macula_edge` | ✅ Short | ❌ Ambiguous (edge of what?) | ❌ Not recommended |
| `macula_node` | ✅ Common in P2P | ❌ Too generic | ❌ Not recommended |
| `macula_client` | ✅ Familiar | ❌ Misleading (not client/server!) | ❌ Not recommended |

**Decision:** `macula_peer` strikes the best balance of clarity and brevity.

---

## Backward Compatibility Strategy

### For SDK Users (Elixir/Phoenix apps)

**Before (0.6.0):**
```elixir
{:ok, conn} = MaculaConnection.connect(
  "https://gateway:4433",
  "macula.arcade",
  []
)

MaculaConnection.subscribe(conn, "game.events", &handle_event/1)
```

**After (0.7.0):**
```elixir
{:ok, peer} = MaculaPeer.connect(
  "https://gateway:4433",
  "macula.arcade",
  []
)

MaculaPeer.subscribe(peer, "game.events", &handle_event/1)
```

**Migration Tool (Optional):**
```bash
# Automated find-and-replace script
./scripts/migrate-connection-to-peer.sh
```

---

## Decision

**Status:** 📋 Awaiting Approval

**Recommendation:** **APPROVE** renaming for version 0.7.0

**Rationale:**
1. Dramatically improves code clarity
2. Aligns naming with actual architecture
3. Makes onboarding easier for new developers
4. Industry-standard terminology (peer-to-peer)

---

## References

- **BitTorrent DHT:** Uses `peer` terminology
- **libp2p:** Uses `peer` for mesh participants
- **Kademlia paper:** Refers to "nodes" and "peers" interchangeably
- **IPFS:** Uses `peer` consistently

---

**Authors:** Claude Code
**Reviewers:** [TBD]
**Approved:** [TBD]
