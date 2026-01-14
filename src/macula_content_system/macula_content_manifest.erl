%%%-------------------------------------------------------------------
%%% @doc
%%% Content manifest module for Macula content-addressed storage.
%%%
%%% Manifests describe content: name, size, chunk information, and
%%% Merkle root hash for verification. They are identified by an MCID
%%% (Macula Content Identifier) which is a self-describing hash.
%%%
%%% == MCID Format ==
%%% MCIDs are 34-byte binary values:
%%% - Byte 0: Version (currently 0x01)
%%% - Byte 1: Codec (0x55=raw, 0x56=manifest)
%%% - Bytes 2-33: 32-byte hash (BLAKE3 or SHA256)
%%%
%%% == String Representation ==
%%% MCIDs can be converted to human-readable strings:
%%% `mcid1-manifest-blake3-5d41402abc4b2a76b9719d911017c592'
%%%
%%% == Example Usage ==
%%% ```
%%% %% Create manifest from binary data
%%% {ok, Manifest} = macula_content_manifest:create(Data, #{
%%%     name => <<"my_app-1.0.0.tar.gz">>,
%%%     chunk_size => 262144
%%% }),
%%%
%%% %% Get MCID as string
%%% MCIDString = macula_content_manifest:mcid_to_string(maps:get(mcid, Manifest)),
%%%
%%% %% Verify data matches manifest
%%% ok = macula_content_manifest:verify(Manifest, Data).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_manifest).

%% API
-export([
    create/2,
    encode/1,
    decode/1,
    verify/2,
    mcid_to_string/1,
    mcid_from_string/1,
    get_chunk_mcid/2,
    version/0
]).

%% Types
-type mcid() :: <<_:272>>.  %% 34 bytes
-type manifest() :: #{
    mcid := mcid(),
    version := pos_integer(),
    name := binary(),
    size := non_neg_integer(),
    created := pos_integer(),
    chunk_size := pos_integer(),
    chunk_count := non_neg_integer(),
    hash_algorithm := macula_content_hasher:algorithm(),
    root_hash := binary(),
    chunks := [macula_content_chunker:chunk_info()],
    signature => binary(),
    publisher_did => binary()
}.

-export_type([mcid/0, manifest/0]).

%% Constants
-define(VERSION, 1).
-define(CODEC_RAW, 16#55).
-define(CODEC_MANIFEST, 16#56).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Return the current manifest version.
-spec version() -> pos_integer().
version() ->
    ?VERSION.

%% @doc Create a manifest from binary data.
%% Options:
%% - name: binary() - Content name (default: <<"unnamed">>)
%% - chunk_size: pos_integer() - Chunk size (default: 262144)
%% - hash_algorithm: blake3 | sha256 - Hash algorithm (default: blake3)
-spec create(binary(), map()) -> {ok, manifest()}.
create(Data, Opts) ->
    Name = maps:get(name, Opts, <<"unnamed">>),
    ChunkSize = maps:get(chunk_size, Opts, macula_content_chunker:default_chunk_size()),
    Algorithm = maps:get(hash_algorithm, Opts, blake3),

    %% Chunk the data
    {ok, Chunks} = macula_content_chunker:chunk(Data, ChunkSize),
    ChunkInfos = macula_content_chunker:chunk_info(Chunks, Algorithm),

    %% Compute Merkle root
    RootHash = case ChunkInfos of
        [] -> macula_content_hasher:hash(Algorithm, <<>>);
        _ -> macula_content_chunker:merkle_root(ChunkInfos, Algorithm)
    end,

    %% Build manifest map (without MCID first)
    ManifestData = #{
        version => ?VERSION,
        name => Name,
        size => byte_size(Data),
        created => erlang:system_time(second),
        chunk_size => ChunkSize,
        chunk_count => length(ChunkInfos),
        hash_algorithm => Algorithm,
        root_hash => RootHash,
        chunks => ChunkInfos
    },

    %% Compute MCID from manifest content
    MCID = compute_mcid(ManifestData, Algorithm),

    {ok, ManifestData#{mcid => MCID}}.

%% @doc Encode manifest to binary (MessagePack).
-spec encode(manifest()) -> {ok, binary()} | {error, term()}.
encode(Manifest) ->
    %% Convert to encodable format
    Encodable = manifest_to_encodable(Manifest),
    case msgpack:pack(Encodable, [{map_format, map}]) of
        {error, Reason} -> {error, Reason};
        Packed -> {ok, Packed}
    end.

%% @doc Decode manifest from binary.
-spec decode(binary()) -> {ok, manifest()} | {error, invalid_manifest}.
decode(Binary) ->
    case msgpack:unpack(Binary, [{map_format, map}]) of
        {ok, Map} -> decode_manifest(Map);
        {error, _} -> {error, invalid_manifest}
    end.

%% @doc Verify that data matches the manifest.
-spec verify(manifest(), binary()) -> ok | {error, size_mismatch | root_hash_mismatch}.
verify(Manifest, Data) ->
    Size = maps:get(size, Manifest),
    case byte_size(Data) of
        Size ->
            verify_root_hash(Manifest, Data);
        _ ->
            {error, size_mismatch}
    end.

%% @doc Convert MCID to human-readable string.
-spec mcid_to_string(mcid()) -> binary().
mcid_to_string(<<Version:8, Codec:8, Hash:32/binary>>) ->
    VersionStr = integer_to_binary(Version),
    CodecStr = codec_to_string(Codec),
    AlgoStr = <<"blake3">>,  %% Assume blake3 for now
    HashHex = macula_content_hasher:hex_encode(Hash),
    <<"mcid", VersionStr/binary, "-", CodecStr/binary, "-",
      AlgoStr/binary, "-", HashHex/binary>>.

%% @doc Parse MCID from string.
-spec mcid_from_string(binary()) -> {ok, mcid()} | {error, invalid_mcid}.
mcid_from_string(String) ->
    case parse_mcid_string(String) of
        {ok, Version, Codec, Hash} ->
            {ok, <<Version:8, Codec:8, Hash/binary>>};
        error ->
            {error, invalid_mcid}
    end.

%% @doc Get MCID for a specific chunk.
-spec get_chunk_mcid(manifest(), non_neg_integer()) -> {ok, mcid()} | {error, invalid_index}.
get_chunk_mcid(Manifest, Index) ->
    Chunks = maps:get(chunks, Manifest),
    case Index < length(Chunks) of
        true ->
            ChunkInfo = lists:nth(Index + 1, Chunks),
            Hash = maps:get(hash, ChunkInfo),
            MCID = make_mcid(?VERSION, ?CODEC_RAW, Hash),
            {ok, MCID};
        false ->
            {error, invalid_index}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
compute_mcid(ManifestData, Algorithm) ->
    %% Create deterministic encoding of manifest (sorted keys, no timestamps for MCID)
    Canonical = #{
        <<"name">> => maps:get(name, ManifestData),
        <<"size">> => maps:get(size, ManifestData),
        <<"chunk_size">> => maps:get(chunk_size, ManifestData),
        <<"chunk_count">> => maps:get(chunk_count, ManifestData),
        <<"hash_algorithm">> => atom_to_binary(maps:get(hash_algorithm, ManifestData)),
        <<"root_hash">> => maps:get(root_hash, ManifestData)
    },
    Packed = msgpack:pack(Canonical, [{map_format, map}]),
    Hash = macula_content_hasher:hash(Algorithm, Packed),
    make_mcid(?VERSION, ?CODEC_MANIFEST, Hash).

%% @private
make_mcid(Version, Codec, Hash) ->
    <<Version:8, Codec:8, Hash/binary>>.

%% @private
codec_to_string(?CODEC_RAW) -> <<"raw">>;
codec_to_string(?CODEC_MANIFEST) -> <<"manifest">>;
codec_to_string(_) -> <<"unknown">>.

%% @private
string_to_codec(<<"raw">>) -> {ok, ?CODEC_RAW};
string_to_codec(<<"manifest">>) -> {ok, ?CODEC_MANIFEST};
string_to_codec(_) -> error.

%% @private
parse_mcid_string(<<"mcid", Rest/binary>>) ->
    case binary:split(Rest, <<"-">>, [global]) of
        [VersionStr, CodecStr, _AlgoStr, HashHex] ->
            Version = binary_to_integer(VersionStr),
            case string_to_codec(CodecStr) of
                {ok, Codec} ->
                    case macula_content_hasher:hex_decode(HashHex) of
                        {ok, Hash} when byte_size(Hash) =:= 32 ->
                            {ok, Version, Codec, Hash};
                        _ ->
                            error
                    end;
                error ->
                    error
            end;
        _ ->
            error
    end;
parse_mcid_string(_) ->
    error.

%% @private
manifest_to_encodable(Manifest) ->
    #{
        <<"mcid">> => maps:get(mcid, Manifest),
        <<"version">> => maps:get(version, Manifest),
        <<"name">> => maps:get(name, Manifest),
        <<"size">> => maps:get(size, Manifest),
        <<"created">> => maps:get(created, Manifest),
        <<"chunk_size">> => maps:get(chunk_size, Manifest),
        <<"chunk_count">> => maps:get(chunk_count, Manifest),
        <<"hash_algorithm">> => atom_to_binary(maps:get(hash_algorithm, Manifest)),
        <<"root_hash">> => maps:get(root_hash, Manifest),
        <<"chunks">> => [chunk_info_to_encodable(C) || C <- maps:get(chunks, Manifest)]
    }.

%% @private
chunk_info_to_encodable(ChunkInfo) ->
    #{
        <<"index">> => maps:get(index, ChunkInfo),
        <<"offset">> => maps:get(offset, ChunkInfo),
        <<"size">> => maps:get(size, ChunkInfo),
        <<"hash">> => maps:get(hash, ChunkInfo)
    }.

%% @private
decode_manifest(Map) when is_map(Map) ->
    case maps:is_key(<<"mcid">>, Map) of
        true ->
            {ok, #{
                mcid => maps:get(<<"mcid">>, Map),
                version => maps:get(<<"version">>, Map, 1),
                name => maps:get(<<"name">>, Map, <<"unnamed">>),
                size => maps:get(<<"size">>, Map, 0),
                created => maps:get(<<"created">>, Map, 0),
                chunk_size => maps:get(<<"chunk_size">>, Map, 262144),
                chunk_count => maps:get(<<"chunk_count">>, Map, 0),
                hash_algorithm => binary_to_existing_atom(
                    maps:get(<<"hash_algorithm">>, Map, <<"blake3">>), utf8),
                root_hash => maps:get(<<"root_hash">>, Map, <<>>),
                chunks => [decode_chunk_info(C) || C <- maps:get(<<"chunks">>, Map, [])]
            }};
        false ->
            {error, invalid_manifest}
    end;
decode_manifest(_) ->
    {error, invalid_manifest}.

%% @private
decode_chunk_info(Map) ->
    #{
        index => maps:get(<<"index">>, Map, 0),
        offset => maps:get(<<"offset">>, Map, 0),
        size => maps:get(<<"size">>, Map, 0),
        hash => maps:get(<<"hash">>, Map, <<>>)
    }.

%% @private
verify_root_hash(Manifest, Data) ->
    ChunkSize = maps:get(chunk_size, Manifest),
    Algorithm = maps:get(hash_algorithm, Manifest),
    ExpectedRoot = maps:get(root_hash, Manifest),

    %% Rechunk and compute root
    {ok, Chunks} = macula_content_chunker:chunk(Data, ChunkSize),
    ChunkInfos = macula_content_chunker:chunk_info(Chunks, Algorithm),
    ActualRoot = case ChunkInfos of
        [] -> macula_content_hasher:hash(Algorithm, <<>>);
        _ -> macula_content_chunker:merkle_root(ChunkInfos, Algorithm)
    end,

    case ActualRoot =:= ExpectedRoot of
        true -> ok;
        false -> {error, root_hash_mismatch}
    end.
