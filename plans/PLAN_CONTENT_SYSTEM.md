# Plan: Macula Content System

**Status:** Core Implementation Complete (Phases 1-5)
**Created:** 2026-01-14
**Last Updated:** 2026-01-14

**Next:** Phase 6 (NIF Optimization) and Phase 7 (bc-gitops + macula-console Integration)

## Implementation Status

| Phase | Module | Tests | Status |
|-------|--------|-------|--------|
| 1 | macula_content_hasher | 30 | ✅ Complete |
| 1 | macula_content_chunker | 28 | ✅ Complete |
| 1 | macula_content_manifest | 32 | ✅ Complete |
| 2 | macula_content_store | 23 | ✅ Complete |
| 3 | macula_content_transfer | 27 | ✅ Complete |
| 4 | macula_content_dht | 15 | ✅ Complete |
| 5 | macula_content (facade) | 11 | ✅ Complete |
| 5 | macula_content_system (supervisor) | 5 | ✅ Complete |
| **Total** | | **171** | 8/8 modules done |

### Wire Protocol Messages Added (0x90-0x9F)

| Type | ID | Status |
|------|-----|--------|
| `content_want` | `0x90` | ✅ Added |
| `content_have` | `0x91` | ✅ Added |
| `content_block` | `0x92` | ✅ Added |
| `content_manifest_req` | `0x93` | ✅ Added |
| `content_manifest_res` | `0x94` | ✅ Added |
| `content_cancel` | `0x95` | ✅ Added |

**Target Version:** v0.19.0

## Overview

Native BEAM content-addressed transfer system for P2P distribution of OTP releases and artifacts across the Macula mesh. Provides chunking, verification, and parallel download without external dependencies.

## Motivation

The Macula ecosystem needs a BEAM-native way to distribute artifacts (OTP releases, packages) across the mesh without relying on:
- Container registries (OCI/Docker)
- External protocols (IPFS, BitTorrent)
- Centralized package hosts

This enables:
- **bc-gitops** to fetch releases from mesh peers
- **macula-console marketplace** to distribute private packages locally
- Hybrid distribution: hex.pm for public, mesh for private/local

## Design Principles

1. **Native BEAM** - No external binaries or processes
2. **Content-addressed** - Same content = same identifier everywhere
3. **Verified** - Merkle tree ensures integrity
4. **Parallel** - Download from multiple providers simultaneously
5. **NAT-friendly** - Uses existing Macula mesh transport (no new NAT traversal)
6. **Borrowing from proven protocols** - IPFS Bitswap, BitTorrent BEP-52, BLAKE3

## Protocol Inspiration

| Source Protocol | What We Borrow |
|-----------------|----------------|
| IPFS Bitswap | Want/Have/Block exchange pattern |
| BitTorrent BEP-52 | Merkle tree piece verification |
| BLAKE3 | Tree-based hashing for parallel/streaming |
| IPFS CID | Content identifier structure |

---

## Architecture

### Module Structure

```
macula/src/macula_content_system/
├── macula_content_system.erl       # Supervisor (one_for_one)
├── macula_content.erl              # Public API facade
├── macula_content_store.erl        # Local block storage (ETS + disk)
├── macula_content_chunker.erl      # Chunking and reassembly
├── macula_content_hasher.erl       # BLAKE3/SHA256 hashing
├── macula_content_manifest.erl     # Manifest creation/parsing/verification
├── macula_content_transfer.erl     # Block exchange protocol (gen_server)
└── macula_content_dht.erl          # DHT announce/locate integration
```

### Supervision Tree

```
macula_content_system (supervisor, one_for_one)
├── macula_content_store (gen_server)
│   └── Manages block storage, garbage collection
├── macula_content_transfer (gen_server)
│   └── Handles want/have/block protocol
└── macula_content_dht (gen_server)
    └── DHT announcements and lookups
```

---

## Content Identifier (MCID)

### Format

```
┌─────────────────────────────────────────────────────────────────┐
│  Macula Content ID (MCID)                                       │
├─────────────────────────────────────────────────────────────────┤
│  Version (1 byte)  │  Codec (1 byte)  │  Hash (32 bytes)        │
│       0x01         │   0x55 (raw)     │   BLAKE3 or SHA256      │
└─────────────────────────────────────────────────────────────────┘
```

### String Representation

```
mcid1-raw-blake3-5d41402abc4b2a76b9719d911017c592
mcid1-raw-sha256-e3b0c44298fc1c149afbf4c8996fb924
```

### Erlang Type

```erlang
-type mcid() :: <<_:272>>.  %% 34 bytes: 1 + 1 + 32

-record(mcid, {
    version = 1 :: pos_integer(),
    codec = raw :: raw | manifest,
    hash_type :: blake3 | sha256,
    hash :: <<_:256>>  %% 32 bytes
}).
```

---

## Manifest Format

### Record Definition

```erlang
-record(content_manifest, {
    %% Identity
    mcid :: mcid(),                    %% Hash of canonical manifest encoding
    version = 1 :: pos_integer(),

    %% Metadata
    name :: binary(),                  %% "my_app-1.0.0.tar.gz"
    size :: pos_integer(),             %% Total size in bytes
    created :: pos_integer(),          %% Unix timestamp (seconds)

    %% Chunking parameters
    chunk_size = 262144 :: pos_integer(),  %% 256KB default
    chunk_count :: pos_integer(),

    %% Verification
    hash_algorithm = blake3 :: blake3 | sha256,
    root_hash :: binary(),             %% Merkle root (32 bytes)

    %% Chunk list (leaves of Merkle tree)
    chunks :: [#chunk_info{}],

    %% Optional signing
    signature :: binary() | undefined,     %% Ed25519 signature
    publisher_did :: binary() | undefined  %% did:macula:io.macula.publisher
}).

-record(chunk_info, {
    index :: non_neg_integer(),        %% 0-based chunk index
    mcid :: mcid(),                    %% Content ID of this chunk
    offset :: non_neg_integer(),       %% Byte offset in original file
    size :: pos_integer()              %% Chunk size (last may be smaller)
}).
```

### Canonical Encoding

Manifests are encoded to binary using MessagePack with sorted keys for deterministic hashing:

```erlang
encode_manifest(#content_manifest{} = M) ->
    Map = #{
        <<"chunks">> => encode_chunks(M#content_manifest.chunks),
        <<"chunk_count">> => M#content_manifest.chunk_count,
        <<"chunk_size">> => M#content_manifest.chunk_size,
        <<"created">> => M#content_manifest.created,
        <<"hash_algorithm">> => atom_to_binary(M#content_manifest.hash_algorithm),
        <<"name">> => M#content_manifest.name,
        <<"root_hash">> => M#content_manifest.root_hash,
        <<"size">> => M#content_manifest.size,
        <<"version">> => M#content_manifest.version
    },
    %% Add optional fields if present
    Map1 = add_optional(<<"signature">>, M#content_manifest.signature, Map),
    Map2 = add_optional(<<"publisher_did">>, M#content_manifest.publisher_did, Map1),
    msgpack:pack(Map2, [{map_format, map}, {pack_str, from_binary}]).
```

---

## Wire Protocol

### Message Type Range

Content system uses message types `0x70-0x7F`:

| Type | ID | Direction | Purpose |
|------|-----|-----------|---------|
| `content_want` | `0x70` | Request | Request blocks by MCID |
| `content_have` | `0x71` | Response | Announce available blocks |
| `content_block` | `0x72` | Response | Block data |
| `content_manifest_req` | `0x73` | Request | Request manifest |
| `content_manifest_res` | `0x74` | Response | Manifest data |
| `content_cancel` | `0x75` | Request | Cancel pending wants |

### Message Formats

#### CONTENT_WANT (0x70)

Request specific blocks from a peer:

```erlang
#{
    <<"request_id">> => binary(),      %% Unique request ID
    <<"wants">> => [
        #{
            <<"mcid">> => binary(),    %% Block MCID
            <<"priority">> => 1..255   %% Higher = more urgent
        }
    ],
    <<"max_blocks">> => pos_integer(), %% Flow control: max blocks in response
    <<"from_node">> => binary()        %% Requester node ID
}
```

#### CONTENT_HAVE (0x71)

Announce blocks we have (unsolicited or in response to want):

```erlang
#{
    <<"haves">> => [binary()],         %% List of MCIDs we have
    <<"manifest_mcid">> => binary(),   %% Optional: we have all chunks of this manifest
    <<"from_node">> => binary()
}
```

#### CONTENT_BLOCK (0x72)

Send block data:

```erlang
#{
    <<"request_id">> => binary(),      %% Correlates with want request
    <<"mcid">> => binary(),            %% Block MCID
    <<"data">> => binary(),            %% Raw block data (NOT base64)
    <<"from_node">> => binary()
}
```

#### CONTENT_MANIFEST_REQ (0x73)

Request a manifest:

```erlang
#{
    <<"request_id">> => binary(),
    <<"mcid">> => binary(),            %% Manifest MCID
    <<"from_node">> => binary()
}
```

#### CONTENT_MANIFEST_RES (0x74)

Respond with manifest:

```erlang
#{
    <<"request_id">> => binary(),
    <<"mcid">> => binary(),
    <<"manifest">> => binary(),        %% MessagePack-encoded manifest
    <<"from_node">> => binary()
}
```

#### CONTENT_CANCEL (0x75)

Cancel pending wants:

```erlang
#{
    <<"request_id">> => binary(),      %% Request to cancel
    <<"from_node">> => binary()
}
```

---

## Transfer Flow

### Single Provider

```
Requester                              Provider
    │                                      │
    │  1. content_manifest_req(mcid=X)     │
    │─────────────────────────────────────►│
    │                                      │
    │  2. content_manifest_res(manifest)   │
    │◄─────────────────────────────────────│
    │                                      │
    │  [Verify signature if present]       │
    │  [Check local store for chunks]      │
    │  [Build want list for missing]       │
    │                                      │
    │  3. content_want(wants=[c1,c2,c3])   │
    │─────────────────────────────────────►│
    │                                      │
    │  4. content_block(mcid=c1, data=...) │
    │◄─────────────────────────────────────│
    │  [Verify hash, store block]          │
    │                                      │
    │  5. content_block(mcid=c2, data=...) │
    │◄─────────────────────────────────────│
    │  [Verify hash, store block]          │
    │                                      │
    │  ... continue until all received     │
    │                                      │
    │  [Verify Merkle root]                │
    │  [Reassemble file]                   │
    │                                      │
```

### Parallel Multi-Provider

```
Requester
    │
    │  [Query DHT for manifest MCID providers]
    │  [Returns: Provider1, Provider2, Provider3]
    │
    ├─────► Provider1: want(chunks 0-9)
    │         │
    │         ◄── block, block, block...
    │
    ├─────► Provider2: want(chunks 10-19)
    │         │
    │         ◄── block, block, block...
    │
    └─────► Provider3: want(chunks 20-29)
              │
              ◄── block, block, block...

    [Reassemble when all chunks received]
    [Verify Merkle root]
```

---

## Merkle Tree Verification

### Tree Structure (BEP-52 inspired)

```
                    Root Hash
                   /         \
             Hash01           Hash23
            /      \         /      \
        Hash0    Hash1   Hash2    Hash3
          │        │       │        │
       Chunk0  Chunk1  Chunk2  Chunk3
```

### Verification Process

```erlang
%% Verify a single chunk
verify_chunk(Chunk, ExpectedMCID, HashAlgorithm) ->
    ActualHash = macula_content_hasher:hash(HashAlgorithm, Chunk),
    ActualMCID = make_mcid(HashAlgorithm, ActualHash),
    ActualMCID =:= ExpectedMCID.

%% Verify complete file against manifest
verify_manifest(FilePath, #content_manifest{} = M) ->
    %% 1. Verify each chunk hash
    ChunksOk = verify_all_chunks(FilePath, M),

    %% 2. Compute Merkle root from chunk hashes
    ChunkHashes = [C#chunk_info.mcid || C <- M#content_manifest.chunks],
    ComputedRoot = merkle_root(ChunkHashes, M#content_manifest.hash_algorithm),

    %% 3. Compare with manifest root
    RootOk = ComputedRoot =:= M#content_manifest.root_hash,

    ChunksOk andalso RootOk.
```

---

## Local Block Store

### Storage Layout

```
/var/lib/macula/content/           # Base directory (on /bulk drives)
├── blocks/
│   ├── 5d/                        # First 2 hex chars of hash
│   │   └── 5d41402abc4b2a76b9719d911017c592.blk
│   ├── a1/
│   │   └── a1b2c3d4e5f6...blk
│   └── ...
├── manifests/
│   ├── 7f83b1657ff1fc53b92dc18148a1d65dfc2d4b1fa3d677284addd200126d9069.man
│   └── ...
├── temp/                          # In-progress downloads
│   └── {request_id}/
│       ├── manifest.tmp
│       └── chunks/
└── index.dets                     # Block → manifest mapping, stats
```

### Store API

```erlang
-module(macula_content_store).

%% Block operations
-export([
    put_block/2,           %% (MCID, Data) -> ok | {error, term()}
    get_block/1,           %% (MCID) -> {ok, Data} | {error, not_found}
    has_block/1,           %% (MCID) -> boolean()
    delete_block/1         %% (MCID) -> ok
]).

%% Manifest operations
-export([
    put_manifest/1,        %% (Manifest) -> ok
    get_manifest/1,        %% (MCID) -> {ok, Manifest} | {error, not_found}
    list_manifests/0,      %% () -> [MCID]
    delete_manifest/1      %% (MCID) -> ok
]).

%% Maintenance
-export([
    gc/0,                  %% Garbage collect orphaned blocks
    stats/0,               %% () -> #{blocks => N, size => Bytes, ...}
    verify_integrity/0     %% Verify all stored blocks match their MCIDs
]).
```

---

## DHT Integration

### Announcements

When content is published locally:

```erlang
%% Announce manifest availability
announce_manifest(Client, ManifestMCID, Manifest) ->
    Procedure = <<"content.manifest.available.", ManifestMCID/binary>>,
    Handler = fun(_Args) ->
        {ok, #{
            mcid => ManifestMCID,
            name => Manifest#content_manifest.name,
            size => Manifest#content_manifest.size,
            chunk_count => Manifest#content_manifest.chunk_count
        }}
    end,
    macula:advertise(Client, Procedure, Handler).
```

### Discovery

To find providers of content:

```erlang
%% Locate providers of a manifest
locate_providers(Client, ManifestMCID) ->
    Procedure = <<"content.manifest.available.", ManifestMCID/binary>>,
    case macula:discover(Client, Procedure) of
        {ok, Providers} -> {ok, [P#provider.node_id || P <- Providers]};
        {error, Reason} -> {error, Reason}
    end.
```

---

## Public API

### Main Facade

```erlang
-module(macula_content).

%%% Publishing

%% @doc Publish a file to the content system.
%% Chunks the file, creates manifest, stores locally, announces to DHT.
-spec publish(FilePath) -> {ok, MCID} | {error, Reason} when
    FilePath :: file:filename(),
    MCID :: mcid(),
    Reason :: term().

%% @doc Publish with options.
-spec publish(FilePath, Opts) -> {ok, MCID} | {error, Reason} when
    FilePath :: file:filename(),
    Opts :: #{
        name => binary(),           %% Override filename
        chunk_size => pos_integer(),%% Default: 262144 (256KB)
        hash_algorithm => blake3 | sha256,  %% Default: blake3
        sign => boolean(),          %% Sign with node key (default: false)
        announce => boolean()       %% Announce to DHT (default: true)
    },
    MCID :: mcid(),
    Reason :: term().

%% @doc Remove content from local store and DHT.
-spec unpublish(MCID) -> ok | {error, Reason} when
    MCID :: mcid(),
    Reason :: term().

%%% Fetching

%% @doc Fetch content by MCID.
%% Locates providers via DHT, downloads chunks, verifies, reassembles.
-spec fetch(MCID) -> {ok, FilePath} | {error, Reason} when
    MCID :: mcid(),
    FilePath :: file:filename(),
    Reason :: term().

%% @doc Fetch with options.
-spec fetch(MCID, Opts) -> {ok, FilePath} | {error, Reason} when
    MCID :: mcid(),
    Opts :: #{
        output_dir => file:filename(),  %% Default: temp dir
        output_name => binary(),        %% Override output filename
        parallel => pos_integer(),      %% Max concurrent providers (default: 4)
        verify => boolean(),            %% Verify Merkle root (default: true)
        timeout => timeout()            %% Overall timeout (default: 5 min)
    },
    FilePath :: file:filename(),
    Reason :: term().

%% @doc Async fetch with progress callback.
-spec fetch_async(MCID, Callback) -> {ok, RequestId} when
    MCID :: mcid(),
    Callback :: fun((Progress) -> ok),
    Progress :: {started, TotalSize}
              | {chunk, Index, ChunkCount}
              | {completed, FilePath}
              | {error, Reason},
    RequestId :: binary().

%%% Queries

%% @doc Locate providers of content in the mesh.
-spec locate(MCID) -> {ok, [NodeId]} | {error, Reason} when
    MCID :: mcid(),
    NodeId :: binary(),
    Reason :: term().

%% @doc Get manifest for content (local or remote).
-spec stat(MCID) -> {ok, Manifest} | {error, Reason} when
    MCID :: mcid(),
    Manifest :: #content_manifest{},
    Reason :: not_found | term().

%% @doc List locally stored content.
-spec list_local() -> [MCID] when
    MCID :: mcid().

%% @doc Check if content is available locally.
-spec is_local(MCID) -> boolean() when
    MCID :: mcid().
```

---

## Hash Algorithm

### Choice: BLAKE3 with SHA256 Fallback

| Algorithm | Speed | Security | Tree Mode | HW Accel |
|-----------|-------|----------|-----------|----------|
| BLAKE3 | Very fast | Modern | Native | No (but fast in SW) |
| SHA256 | Good | Proven | No | Yes (Intel SHA-NI) |

**Primary: BLAKE3**
- Tree-based internally (parallel hashing)
- Streaming verification possible
- 2-4x faster than SHA256 in software

**Fallback: SHA256**
- Hardware acceleration on modern CPUs
- Wider compatibility
- Proven security

### Implementation

```erlang
-module(macula_content_hasher).

-export([hash/2, hash_file/2, verify/3]).

%% @doc Hash binary data.
-spec hash(Algorithm, Data) -> Hash when
    Algorithm :: blake3 | sha256,
    Data :: binary(),
    Hash :: <<_:256>>.
hash(blake3, Data) ->
    %% Try NIF first, fallback to pure Erlang
    try_nif_or_fallback(blake3, Data);
hash(sha256, Data) ->
    crypto:hash(sha256, Data).

%% @doc Hash a file in chunks (memory efficient).
-spec hash_file(Algorithm, FilePath) -> {ok, Hash} | {error, Reason} when
    Algorithm :: blake3 | sha256,
    FilePath :: file:filename(),
    Hash :: <<_:256>>,
    Reason :: term().

%% @doc Verify data against expected hash.
-spec verify(Algorithm, Data, ExpectedHash) -> boolean().

%% Internal: try NIF, fallback to pure Erlang
try_nif_or_fallback(blake3, Data) ->
    case erlang:function_exported(macula_nifs, blake3_hash, 1) of
        true -> macula_nifs:blake3_hash(Data);
        false -> blake3_pure:hash(Data)  %% Pure Erlang implementation
    end.
```

### NIF Optimization (macula-nifs/)

Future optimization in `macula-nifs/`:

```rust
// In macula-nifs/native/macula_nifs/src/content.rs

use rustler::{Binary, Env, NifResult};
use blake3::Hasher;

#[rustler::nif]
fn blake3_hash(data: Binary) -> NifResult<Binary> {
    let hash = blake3::hash(data.as_slice());
    Ok(hash.as_bytes().to_vec().into())
}

#[rustler::nif]
fn blake3_hash_streaming<'a>(env: Env<'a>, chunks: Vec<Binary>) -> NifResult<Binary<'a>> {
    let mut hasher = Hasher::new();
    for chunk in chunks {
        hasher.update(chunk.as_slice());
    }
    let hash = hasher.finalize();
    Ok(hash.as_bytes().to_vec().into())
}
```

---

## Configuration

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `MACULA_CONTENT_ENABLED` | `true` | Enable content system |
| `MACULA_CONTENT_STORE_PATH` | `/var/lib/macula/content` | Block storage directory |
| `MACULA_CONTENT_CHUNK_SIZE` | `262144` | Default chunk size (256KB) |
| `MACULA_CONTENT_HASH_ALGORITHM` | `blake3` | Default hash algorithm |
| `MACULA_CONTENT_MAX_PARALLEL` | `4` | Max parallel providers |
| `MACULA_CONTENT_GC_INTERVAL` | `3600` | GC interval in seconds |

### Application Config

```erlang
{macula, [
    {content_enabled, true},
    {content_store_path, "/var/lib/macula/content"},
    {content_chunk_size, 262144},
    {content_hash_algorithm, blake3},
    {content_max_parallel, 4},
    {content_gc_interval, 3600}
]}.
```

---

## Integration Points

### bc-gitops Integration

bc-gitops will use content system for fetching releases:

```erlang
%% In bc_gitops_runtime.erl
fetch_from_mesh(#app_spec{source = #{type := mesh, mcid := MCID}}) ->
    case macula_content:fetch(MCID, #{output_dir => releases_dir()}) of
        {ok, TarPath} ->
            extract_release(TarPath);
        {error, Reason} ->
            {error, {fetch_failed, Reason}}
    end.
```

### macula-console Marketplace

Marketplace will use content system for P2P artifact distribution:

```elixir
# In MaculaReleases (replaces MaculaPackages for releases)
defmodule MaculaReleases do
  def publish(release_path) do
    case :macula_content.publish(release_path, %{sign: true, announce: true}) do
      {:ok, mcid} -> {:ok, mcid}
      {:error, reason} -> {:error, reason}
    end
  end

  def fetch(mcid, opts \\ %{}) do
    :macula_content.fetch(mcid, opts)
  end
end
```

---

## Implementation Phases

### Phase 1: Core (Foundation)

- [ ] Add protocol message types to `macula_protocol_types.erl`
- [ ] Implement `macula_content_hasher.erl` (SHA256 first, BLAKE3 later)
- [ ] Implement `macula_content_chunker.erl` (chunk/reassemble)
- [ ] Implement `macula_content_manifest.erl` (create/parse/verify)
- [ ] Write unit tests (target: 30 tests)

### Phase 2: Storage

- [ ] Implement `macula_content_store.erl` (block storage)
- [ ] Implement index management (DETS)
- [ ] Implement garbage collection
- [ ] Write storage tests (target: 20 tests)

### Phase 3: Transfer

- [ ] Implement `macula_content_transfer.erl` (want/have/block)
- [ ] Integrate with `macula_gateway.erl` for message handling
- [ ] Implement parallel download from multiple providers
- [ ] Write transfer tests (target: 25 tests)

### Phase 4: DHT Integration

- [ ] Implement `macula_content_dht.erl` (announce/locate)
- [ ] Integrate with existing DHT infrastructure
- [ ] Write DHT tests (target: 15 tests)

### Phase 5: API & Supervision

- [x] Implement `macula_content.erl` (public API facade) - 11 tests
- [x] Implement `macula_content_system.erl` (supervisor) - 5 tests
- [ ] Add to macula supervision tree (deferred to Phase 7)
- [x] Write integration tests

### Phase 5.5: Documentation (macula-ecosystem/) - COMPLETE

- [x] Create animated SVG diagram for Content Transfer flow (like DHT PubSub/RPC)
- [x] Add "Macula Content Transfer" section to macula-ecosystem documentation
- [x] Document want/have/block protocol with visual flow
- [x] Document parallel multi-provider download visualization
- [x] Document MCID format and Merkle tree verification

**Files Created:**
- `macula-ecosystem/assets/content-transfer-flow.svg` - Animated SVG with:
  - DHT discovery animation (rotating rings)
  - WANT message flow (animated circles)
  - BLOCK data flow (animated rectangles)
  - Merkle tree verification visualization
  - Parallel download indicator
  - MCID format breakdown
- `macula-ecosystem/guides/content-transfer.md` - Complete documentation

### Phase 6: NIF Optimization (macula-nifs/)

- [ ] Add BLAKE3 NIF to macula-nifs
- [ ] Add parallel chunk hashing NIF
- [ ] Benchmark and optimize
- [ ] Update macula_content_hasher to use NIFs

### Phase 7: Integration

- [x] Update bc-gitops for mesh source type
  - Added `mesh` to source_spec type union
  - Added `mcid` field to source_spec record
  - Added `fetch_mesh_package/2` in bc_gitops_workspace.erl
  - Added telemetry events for mesh fetch (TELEMETRY_MESH_FETCH_START/STOP)
  - Added mesh source test (68 tests passing)
- [x] Update macula-console Content module
  - Created `MaculaCluster.Content` module (~320 LOC)
  - High-level Elixir interface to `:macula_content`
  - publish/2, store/2, fetch/2, locate/1, stat/1, is_local?/1
  - Runtime availability check (graceful degradation)
  - Compiles successfully (warnings expected - :macula_content not in deps)
- [ ] End-to-end testing
- [ ] Documentation

---

## Files to Create/Modify

### New Files (macula/)

| File | Purpose | LOC Est. |
|------|---------|----------|
| `src/macula_content_system/macula_content_system.erl` | Supervisor | ~80 |
| `src/macula_content_system/macula_content.erl` | Public API | ~200 |
| `src/macula_content_system/macula_content_store.erl` | Block storage | ~300 |
| `src/macula_content_system/macula_content_chunker.erl` | Chunking | ~150 |
| `src/macula_content_system/macula_content_hasher.erl` | Hashing | ~100 |
| `src/macula_content_system/macula_content_manifest.erl` | Manifests | ~250 |
| `src/macula_content_system/macula_content_transfer.erl` | Protocol | ~400 |
| `src/macula_content_system/macula_content_dht.erl` | DHT integration | ~150 |

### Modified Files (macula/)

| File | Changes |
|------|---------|
| `src/macula_protocol_types.erl` | Add message types 0x70-0x7F |
| `src/macula_protocol_encoder.erl` | Add validation for content messages |
| `src/macula_gateway.erl` | Handle content messages, delegate to transfer |
| `src/macula_root.erl` | Add content_system to supervision tree |

### New Files (macula-nifs/)

| File | Purpose |
|------|---------|
| `native/macula_nifs/src/blake3.rs` | BLAKE3 hashing NIF |
| `native/macula_nifs/src/content.rs` | Content system NIFs |

### Test Files

| File | Tests Est. | Actual |
|------|------------|--------|
| `test/macula_content_hasher_tests.erl` | 15 | 30 |
| `test/macula_content_chunker_tests.erl` | 15 | 28 |
| `test/macula_content_manifest_tests.erl` | 20 | 32 |
| `test/macula_content_store_tests.erl` | 20 | 23 |
| `test/macula_content_transfer_tests.erl` | 25 | 27 |
| `test/macula_content_dht_tests.erl` | 15 | 15 |
| `test/macula_content_system_tests.erl` | - | 5 |
| `test/macula_content_tests.erl` | - | 11 |

**Total: 171 tests** (exceeded estimate of ~130)

### Documentation Files (macula-ecosystem/)

| File | Purpose |
|------|---------|
| `assets/content-transfer-flow.svg` | Animated SVG showing want/have/block protocol |
| `assets/content-parallel-download.svg` | Multi-provider parallel download visualization |
| `assets/content-merkle-tree.svg` | Merkle tree verification diagram |
| `guides/content-transfer.md` | Macula Content Transfer documentation section |

---

## Success Criteria

1. **Functionality**
   - [ ] Can publish file and receive MCID
   - [ ] Can fetch file by MCID from remote peer
   - [ ] Merkle verification passes
   - [ ] Parallel download works with 4+ providers

2. **Performance**
   - [ ] 100MB file transfers in <30s on LAN
   - [ ] Chunking throughput >100MB/s
   - [ ] Hash verification throughput >200MB/s (with NIFs)

3. **Reliability**
   - [ ] Handles provider disconnection gracefully
   - [ ] Resumes partial downloads
   - [ ] Garbage collection keeps store bounded

4. **Integration**
   - [ ] bc-gitops can fetch releases via mesh
   - [ ] macula-console marketplace uses content system

---

## Open Questions

1. **Chunk deduplication** - Should we dedupe identical chunks across manifests?
   - Pro: Storage savings
   - Con: Complexity, GC harder

2. **Encryption** - Should chunks be encrypted at rest?
   - Pro: Security
   - Con: Performance, key management

3. **Compression** - Should chunks be compressed?
   - Pro: Smaller transfers
   - Con: CPU overhead, already-compressed content

4. **Cache eviction** - LRU vs LFU for block cache?
   - LRU simpler, LFU better for popular content

---

## References

- [IPFS Bitswap Spec](https://specs.ipfs.tech/bitswap-protocol/)
- [BitTorrent BEP-52 (Merkle)](https://www.bittorrent.org/beps/bep_0052.html)
- [BLAKE3 Spec](https://github.com/BLAKE3-team/BLAKE3-specs)
- [Content Addressable aRchive (CAR)](https://ipld.io/specs/transport/car/)
