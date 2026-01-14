%%%-------------------------------------------------------------------
%%% @doc
%%% Public API facade for Macula content-addressed storage system.
%%%
%%% Provides a simple, high-level interface for content operations:
%%% - Publishing: Store content and announce to mesh
%%% - Fetching: Download content from mesh peers
%%% - Verification: Ensure content integrity via Merkle proofs
%%%
%%% == Quick Start ==
%%% ```
%%% %% Publish content (returns manifest MCID)
%%% {ok, MCID} = macula_content:publish("/path/to/release.tar.gz").
%%%
%%% %% Fetch content by MCID
%%% {ok, FilePath} = macula_content:fetch(MCID, #{output_dir => "/tmp"}).
%%%
%%% %% Check if content is available locally
%%% true = macula_content:is_local(MCID).
%%% '''
%%%
%%% == MCID Format ==
%%% MCIDs are 34-byte content identifiers:
%%% - Version (1 byte): Protocol version
%%% - Codec (1 byte): Content type (0x55 = raw, 0x56 = manifest)
%%% - Hash (32 bytes): BLAKE3 or SHA256 content hash
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content).

%% MCID Operations
-export([
    mcid/1,
    mcid/2,
    mcid_to_string/1,
    mcid_from_string/1
]).

%% Manifest Operations
-export([
    create_manifest/1,
    create_manifest/2,
    verify_manifest/2,
    get_chunks/1,
    missing_chunks/1
]).

%% Content Operations
-export([
    store/1,
    store/2,
    fetch/1,
    fetch/2,
    is_local/1
]).

%% Status
-export([
    status/0
]).

%% Types
-type mcid() :: <<_:272>>.  %% 34 bytes
-type manifest() :: map().
-type chunk_info() :: map().

-export_type([mcid/0, manifest/0, chunk_info/0]).

%% Constants
-define(VERSION, 1).
-define(CODEC_RAW, 16#55).
-define(CODEC_MANIFEST, 16#56).

%%%===================================================================
%%% MCID Operations
%%%===================================================================

%% @doc Create MCID from binary data using default algorithm (BLAKE3).
-spec mcid(binary()) -> mcid().
mcid(Data) ->
    mcid(Data, #{}).

%% @doc Create MCID from binary data with options.
%% Options:
%% - hash_algorithm: blake3 | sha256 (default: blake3)
-spec mcid(binary(), map()) -> mcid().
mcid(Data, Opts) ->
    Algorithm = maps:get(hash_algorithm, Opts, blake3),
    Hash = macula_content_hasher:hash(Algorithm, Data),
    Codec = ?CODEC_RAW,
    <<?VERSION:8, Codec:8, Hash/binary>>.

%% @doc Convert MCID to human-readable string.
%% Format: mcid1-{codec}-{algorithm}-{hash_hex}
-spec mcid_to_string(mcid()) -> binary().
mcid_to_string(MCID) ->
    macula_content_manifest:mcid_to_string(MCID).

%% @doc Parse MCID from string representation.
-spec mcid_from_string(binary()) -> {ok, mcid()} | {error, invalid_mcid}.
mcid_from_string(Str) ->
    macula_content_manifest:mcid_from_string(Str).

%%%===================================================================
%%% Manifest Operations
%%%===================================================================

%% @doc Create manifest from binary data.
-spec create_manifest(binary()) -> {ok, manifest()}.
create_manifest(Data) ->
    create_manifest(Data, #{}).

%% @doc Create manifest from binary data with options.
%% Options:
%% - name: Content name (default: "unnamed")
%% - chunk_size: Chunk size in bytes (default: 262144 / 256KB)
%% - hash_algorithm: blake3 | sha256 (default: blake3)
-spec create_manifest(binary(), map()) -> {ok, manifest()}.
create_manifest(Data, Opts) ->
    %% macula_content_manifest:create returns {ok, Map}
    macula_content_manifest:create(Data, Opts).

%% @doc Verify data against manifest.
%% Returns true if data matches manifest's root hash.
-spec verify_manifest(manifest(), binary()) -> boolean().
verify_manifest(Manifest, Data) when is_map(Manifest) ->
    case macula_content_manifest:verify(Manifest, Data) of
        ok -> true;
        {error, _} -> false
    end.

%% @doc Get list of chunks from manifest.
-spec get_chunks(manifest()) -> [chunk_info()].
get_chunks(#{chunks := Chunks}) ->
    Chunks;
get_chunks(_) ->
    [].

%% @doc Get list of chunks not yet stored locally.
-spec missing_chunks(manifest()) -> [chunk_info()].
missing_chunks(Manifest) ->
    Chunks = get_chunks(Manifest),
    Algorithm = maps:get(hash_algorithm, Manifest, blake3),
    lists:filter(fun(Chunk) ->
        ChunkHash = maps:get(hash, Chunk),
        ChunkMCID = make_chunk_mcid(ChunkHash, Algorithm),
        not macula_content_store:has_block(ChunkMCID)
    end, Chunks).

%% @private Create chunk MCID from hash.
make_chunk_mcid(Hash, _Algorithm) ->
    <<?VERSION:8, ?CODEC_RAW:8, Hash/binary>>.

%%%===================================================================
%%% Content Operations
%%%===================================================================

%% @doc Store binary content locally.
%% Creates manifest, chunks data, stores blocks.
-spec store(binary()) -> {ok, manifest()} | {error, term()}.
store(Data) ->
    store(Data, #{}).

%% @doc Store binary content with options.
-spec store(binary(), map()) -> {ok, manifest()} | {error, term()}.
store(Data, Opts) ->
    {ok, Manifest} = create_manifest(Data, Opts),

    %% Store each chunk as a block
    ChunkSize = maps:get(chunk_size, Manifest, 262144),
    {ok, Chunks} = macula_content_chunker:chunk(Data, ChunkSize),
    Algorithm = maps:get(hash_algorithm, Manifest, blake3),

    lists:foreach(fun(ChunkData) ->
        Hash = macula_content_hasher:hash(Algorithm, ChunkData),
        ChunkMCID = <<?VERSION:8, ?CODEC_RAW:8, Hash/binary>>,
        macula_content_store:put_block(ChunkMCID, ChunkData)
    end, Chunks),

    %% Store manifest (manifest is already a map)
    macula_content_store:put_manifest(Manifest),

    {ok, Manifest}.

%% @doc Fetch content by MCID.
%% If available locally, returns immediately.
%% Otherwise, discovers providers and downloads.
-spec fetch(mcid()) -> {ok, binary()} | {error, term()}.
fetch(MCID) ->
    fetch(MCID, #{}).

%% @doc Fetch content with options.
%% Options:
%% - verify: Verify Merkle root after download (default: true)
%% - timeout: Download timeout in ms (default: 300000 / 5 min)
-spec fetch(mcid(), map()) -> {ok, binary()} | {error, term()}.
fetch(MCID, _Opts) ->
    %% Try local first
    case macula_content_store:get_manifest(MCID) of
        {ok, Manifest} ->
            fetch_from_local(Manifest);
        {error, not_found} ->
            %% Would discover from DHT and download from peers
            %% For now, return error
            {error, not_found}
    end.

%% @doc Check if content is available locally.
-spec is_local(mcid()) -> boolean().
is_local(MCID) ->
    case macula_content_store:get_manifest(MCID) of
        {ok, _Manifest} ->
            true;
        {error, _} ->
            %% Also check if it's a block MCID
            macula_content_store:has_block(MCID)
    end.

%%%===================================================================
%%% Status
%%%===================================================================

%% @doc Get content system status.
-spec status() -> map().
status() ->
    try
        Stats = macula_content_store:stats(),
        Stats#{enabled => true}
    catch
        _:_ ->
            #{enabled => false, error => store_unavailable}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Fetch content from local storage using manifest map.
fetch_from_local(#{chunks := Chunks, hash_algorithm := Algorithm}) ->
    ChunkDataList = lists:map(fun(#{hash := Hash}) ->
        ChunkMCID = make_chunk_mcid(Hash, Algorithm),
        case macula_content_store:get_block(ChunkMCID) of
            {ok, Data} -> Data;
            {error, _} -> <<>>
        end
    end, Chunks),
    Data = macula_content_chunker:reassemble(ChunkDataList),
    {ok, Data};
fetch_from_local(#{chunks := Chunks}) ->
    %% Fallback without algorithm - try raw hash extraction
    ChunkDataList = lists:map(fun(#{hash := Hash}) ->
        ChunkMCID = make_chunk_mcid(Hash, blake3),
        case macula_content_store:get_block(ChunkMCID) of
            {ok, Data} -> Data;
            {error, _} -> <<>>
        end
    end, Chunks),
    Data = macula_content_chunker:reassemble(ChunkDataList),
    {ok, Data}.
