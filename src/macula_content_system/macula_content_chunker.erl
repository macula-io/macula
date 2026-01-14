%%%-------------------------------------------------------------------
%%% @doc
%%% Content chunking module for Macula content-addressed storage.
%%%
%%% Provides functions to split binary data into fixed-size chunks,
%%% reassemble chunks back into the original data, and compute
%%% Merkle tree root hashes for verification.
%%%
%%% == Chunk Size ==
%%% The default chunk size is 256KB (262144 bytes), which provides
%%% a good balance between:
%%% - Network efficiency (not too many small packets)
%%% - Memory usage (chunks fit in L2 cache)
%%% - Parallelism (enough chunks for multi-provider download)
%%%
%%% == Example Usage ==
%%% ```
%%% %% Split data into chunks
%%% {ok, Chunks} = macula_content_chunker:chunk(Data, 262144),
%%%
%%% %% Get chunk metadata with hashes
%%% Infos = macula_content_chunker:chunk_info(Chunks, sha256),
%%%
%%% %% Compute Merkle root
%%% Root = macula_content_chunker:merkle_root(Infos, sha256),
%%%
%%% %% Reassemble chunks
%%% Data = macula_content_chunker:reassemble(Chunks).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_chunker).

%% API
-export([
    chunk/2,
    reassemble/1,
    chunk_info/2,
    merkle_root/2,
    verify_chunk/3,
    default_chunk_size/0
]).

%% Types
-type chunk_info() :: #{
    index := non_neg_integer(),
    offset := non_neg_integer(),
    size := pos_integer(),
    hash := binary()
}.

-export_type([chunk_info/0]).

%% Default chunk size: 256KB
-define(DEFAULT_CHUNK_SIZE, 262144).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Return the default chunk size (256KB).
-spec default_chunk_size() -> pos_integer().
default_chunk_size() ->
    ?DEFAULT_CHUNK_SIZE.

%% @doc Split binary data into fixed-size chunks.
%% Returns a list of binaries, where all but the last are exactly
%% ChunkSize bytes. The last chunk may be smaller.
%% Empty input returns an empty list.
-spec chunk(binary(), pos_integer()) -> {ok, [binary()]}.
chunk(<<>>, _ChunkSize) ->
    {ok, []};
chunk(Data, ChunkSize) when is_binary(Data), ChunkSize > 0 ->
    {ok, do_chunk(Data, ChunkSize, [])}.

%% @doc Reassemble chunks back into original data.
-spec reassemble([binary()]) -> binary().
reassemble(Chunks) ->
    iolist_to_binary(Chunks).

%% @doc Create chunk info records with index, offset, size, and hash.
-spec chunk_info([binary()], macula_content_hasher:algorithm()) -> [chunk_info()].
chunk_info(Chunks, Algorithm) ->
    {Infos, _} = lists:foldl(
        fun(Chunk, {Acc, {Index, Offset}}) ->
            Size = byte_size(Chunk),
            Hash = macula_content_hasher:hash(Algorithm, Chunk),
            Info = #{
                index => Index,
                offset => Offset,
                size => Size,
                hash => Hash
            },
            {[Info | Acc], {Index + 1, Offset + Size}}
        end,
        {[], {0, 0}},
        Chunks
    ),
    lists:reverse(Infos).

%% @doc Compute Merkle tree root hash from chunk infos.
%% Uses a binary tree structure where leaf nodes are chunk hashes
%% and internal nodes are hash(left || right).
-spec merkle_root([chunk_info()], macula_content_hasher:algorithm()) -> binary().
merkle_root([], _Algorithm) ->
    <<0:256>>;  %% Empty root
merkle_root(Infos, Algorithm) ->
    Hashes = [maps:get(hash, I) || I <- Infos],
    compute_merkle_root(Hashes, Algorithm).

%% @doc Verify a chunk against its expected hash.
-spec verify_chunk(binary(), binary(), macula_content_hasher:algorithm()) -> boolean().
verify_chunk(Chunk, ExpectedHash, Algorithm) ->
    macula_content_hasher:verify(Algorithm, Chunk, ExpectedHash).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
do_chunk(<<>>, _ChunkSize, Acc) ->
    lists:reverse(Acc);
do_chunk(Data, ChunkSize, Acc) when byte_size(Data) =< ChunkSize ->
    lists:reverse([Data | Acc]);
do_chunk(Data, ChunkSize, Acc) ->
    <<Chunk:ChunkSize/binary, Rest/binary>> = Data,
    do_chunk(Rest, ChunkSize, [Chunk | Acc]).

%% @private
%% Compute Merkle root from list of hashes.
%% Pairs hashes and recursively combines until single root remains.
compute_merkle_root([Hash], _Algorithm) ->
    Hash;
compute_merkle_root(Hashes, Algorithm) ->
    Paired = pair_hashes(Hashes),
    Combined = lists:map(
        fun({Left, Right}) ->
            macula_content_hasher:hash(Algorithm, <<Left/binary, Right/binary>>)
        end,
        Paired
    ),
    compute_merkle_root(Combined, Algorithm).

%% @private
%% Pair up hashes for Merkle tree.
%% If odd number, last hash is paired with itself.
pair_hashes([]) ->
    [];
pair_hashes([H]) ->
    [{H, H}];  %% Duplicate for odd count
pair_hashes([H1, H2 | Rest]) ->
    [{H1, H2} | pair_hashes(Rest)].
