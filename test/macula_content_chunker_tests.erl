%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_content_chunker module.
%%% Tests chunking and reassembly functionality.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_chunker_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% chunk/2 Tests - Basic Chunking
%%%===================================================================

chunk_empty_data_test() ->
    {ok, Chunks} = macula_content_chunker:chunk(<<>>, 1024),
    ?assertEqual([], Chunks).

chunk_smaller_than_chunk_size_test() ->
    Data = <<"small data">>,
    {ok, Chunks} = macula_content_chunker:chunk(Data, 1024),
    ?assertEqual(1, length(Chunks)),
    ?assertEqual(Data, hd(Chunks)).

chunk_exact_chunk_size_test() ->
    Data = crypto:strong_rand_bytes(1024),
    {ok, Chunks} = macula_content_chunker:chunk(Data, 1024),
    ?assertEqual(1, length(Chunks)),
    ?assertEqual(Data, hd(Chunks)).

chunk_two_chunks_test() ->
    Data = crypto:strong_rand_bytes(1500),
    {ok, Chunks} = macula_content_chunker:chunk(Data, 1024),
    ?assertEqual(2, length(Chunks)),
    ?assertEqual(1024, byte_size(hd(Chunks))),
    ?assertEqual(476, byte_size(lists:last(Chunks))).

chunk_multiple_chunks_test() ->
    Data = crypto:strong_rand_bytes(10000),
    {ok, Chunks} = macula_content_chunker:chunk(Data, 1024),
    ?assertEqual(10, length(Chunks)),
    %% First 9 chunks should be 1024 bytes
    lists:foreach(
        fun(C) -> ?assertEqual(1024, byte_size(C)) end,
        lists:sublist(Chunks, 9)
    ),
    %% Last chunk should be 784 bytes (10000 - 9*1024)
    ?assertEqual(784, byte_size(lists:last(Chunks))).

chunk_preserves_data_test() ->
    Data = crypto:strong_rand_bytes(5000),
    {ok, Chunks} = macula_content_chunker:chunk(Data, 1024),
    Reassembled = iolist_to_binary(Chunks),
    ?assertEqual(Data, Reassembled).

%%%===================================================================
%%% chunk/2 Tests - Edge Cases
%%%===================================================================

chunk_1_byte_chunks_test() ->
    Data = <<"hello">>,
    {ok, Chunks} = macula_content_chunker:chunk(Data, 1),
    ?assertEqual(5, length(Chunks)),
    ?assertEqual([<<"h">>, <<"e">>, <<"l">>, <<"l">>, <<"o">>], Chunks).

chunk_large_chunk_size_test() ->
    Data = <<"small">>,
    {ok, Chunks} = macula_content_chunker:chunk(Data, 1000000),
    ?assertEqual(1, length(Chunks)),
    ?assertEqual(Data, hd(Chunks)).

chunk_default_256kb_test() ->
    Data = crypto:strong_rand_bytes(262144 * 3 + 100),  %% 3 full + partial
    {ok, Chunks} = macula_content_chunker:chunk(Data, 262144),
    ?assertEqual(4, length(Chunks)),
    ?assertEqual(262144, byte_size(hd(Chunks))),
    ?assertEqual(100, byte_size(lists:last(Chunks))).

%%%===================================================================
%%% reassemble/1 Tests
%%%===================================================================

reassemble_empty_list_test() ->
    ?assertEqual(<<>>, macula_content_chunker:reassemble([])).

reassemble_single_chunk_test() ->
    Data = <<"single chunk">>,
    ?assertEqual(Data, macula_content_chunker:reassemble([Data])).

reassemble_multiple_chunks_test() ->
    Chunks = [<<"chunk1">>, <<"chunk2">>, <<"chunk3">>],
    ?assertEqual(<<"chunk1chunk2chunk3">>, macula_content_chunker:reassemble(Chunks)).

reassemble_roundtrip_test() ->
    Original = crypto:strong_rand_bytes(10000),
    {ok, Chunks} = macula_content_chunker:chunk(Original, 1024),
    Reassembled = macula_content_chunker:reassemble(Chunks),
    ?assertEqual(Original, Reassembled).

reassemble_large_data_test() ->
    Original = crypto:strong_rand_bytes(1024 * 1024),  %% 1MB
    {ok, Chunks} = macula_content_chunker:chunk(Original, 262144),
    Reassembled = macula_content_chunker:reassemble(Chunks),
    ?assertEqual(Original, Reassembled).

%%%===================================================================
%%% chunk_info/3 Tests
%%%===================================================================

chunk_info_creates_info_list_test() ->
    Data = crypto:strong_rand_bytes(3000),
    {ok, Chunks} = macula_content_chunker:chunk(Data, 1024),
    Infos = macula_content_chunker:chunk_info(Chunks, sha256),
    ?assertEqual(3, length(Infos)).

chunk_info_has_correct_indices_test() ->
    Data = crypto:strong_rand_bytes(3000),
    {ok, Chunks} = macula_content_chunker:chunk(Data, 1024),
    Infos = macula_content_chunker:chunk_info(Chunks, sha256),
    Indices = [maps:get(index, I) || I <- Infos],
    ?assertEqual([0, 1, 2], Indices).

chunk_info_has_correct_offsets_test() ->
    Data = crypto:strong_rand_bytes(3000),
    {ok, Chunks} = macula_content_chunker:chunk(Data, 1024),
    Infos = macula_content_chunker:chunk_info(Chunks, sha256),
    Offsets = [maps:get(offset, I) || I <- Infos],
    ?assertEqual([0, 1024, 2048], Offsets).

chunk_info_has_correct_sizes_test() ->
    Data = crypto:strong_rand_bytes(3000),
    {ok, Chunks} = macula_content_chunker:chunk(Data, 1024),
    Infos = macula_content_chunker:chunk_info(Chunks, sha256),
    Sizes = [maps:get(size, I) || I <- Infos],
    ?assertEqual([1024, 1024, 952], Sizes).  %% 3000 - 2048 = 952

chunk_info_has_hashes_test() ->
    Data = crypto:strong_rand_bytes(3000),
    {ok, Chunks} = macula_content_chunker:chunk(Data, 1024),
    Infos = macula_content_chunker:chunk_info(Chunks, sha256),
    lists:foreach(
        fun(Info) ->
            Hash = maps:get(hash, Info),
            ?assertEqual(32, byte_size(Hash))
        end,
        Infos
    ).

chunk_info_hashes_match_chunks_test() ->
    Data = crypto:strong_rand_bytes(3000),
    {ok, Chunks} = macula_content_chunker:chunk(Data, 1024),
    Infos = macula_content_chunker:chunk_info(Chunks, sha256),
    lists:foreach(
        fun({Chunk, Info}) ->
            ExpectedHash = crypto:hash(sha256, Chunk),
            ?assertEqual(ExpectedHash, maps:get(hash, Info))
        end,
        lists:zip(Chunks, Infos)
    ).

%%%===================================================================
%%% merkle_root/2 Tests
%%%===================================================================

merkle_root_single_chunk_test() ->
    Data = <<"single">>,
    {ok, Chunks} = macula_content_chunker:chunk(Data, 1024),
    Infos = macula_content_chunker:chunk_info(Chunks, sha256),
    Root = macula_content_chunker:merkle_root(Infos, sha256),
    %% Single chunk = hash of that chunk
    ?assertEqual(crypto:hash(sha256, Data), Root).

merkle_root_two_chunks_test() ->
    Data = crypto:strong_rand_bytes(2000),
    {ok, Chunks} = macula_content_chunker:chunk(Data, 1024),
    Infos = macula_content_chunker:chunk_info(Chunks, sha256),
    Root = macula_content_chunker:merkle_root(Infos, sha256),
    %% Two chunks = hash of (hash1 || hash2)
    Hash1 = crypto:hash(sha256, hd(Chunks)),
    Hash2 = crypto:hash(sha256, lists:last(Chunks)),
    Expected = crypto:hash(sha256, <<Hash1/binary, Hash2/binary>>),
    ?assertEqual(Expected, Root).

merkle_root_deterministic_test() ->
    Data = crypto:strong_rand_bytes(10000),
    {ok, Chunks} = macula_content_chunker:chunk(Data, 1024),
    Infos = macula_content_chunker:chunk_info(Chunks, sha256),
    Root1 = macula_content_chunker:merkle_root(Infos, sha256),
    Root2 = macula_content_chunker:merkle_root(Infos, sha256),
    ?assertEqual(Root1, Root2).

merkle_root_different_data_different_root_test() ->
    Data1 = <<"data1">>,
    Data2 = <<"data2">>,
    {ok, Chunks1} = macula_content_chunker:chunk(Data1, 1024),
    {ok, Chunks2} = macula_content_chunker:chunk(Data2, 1024),
    Infos1 = macula_content_chunker:chunk_info(Chunks1, sha256),
    Infos2 = macula_content_chunker:chunk_info(Chunks2, sha256),
    Root1 = macula_content_chunker:merkle_root(Infos1, sha256),
    Root2 = macula_content_chunker:merkle_root(Infos2, sha256),
    ?assertNotEqual(Root1, Root2).

%%%===================================================================
%%% verify_chunk/3 Tests
%%%===================================================================

verify_chunk_correct_hash_test() ->
    Chunk = <<"test chunk data">>,
    Hash = crypto:hash(sha256, Chunk),
    ?assert(macula_content_chunker:verify_chunk(Chunk, Hash, sha256)).

verify_chunk_incorrect_hash_test() ->
    Chunk = <<"test chunk data">>,
    WrongHash = crypto:hash(sha256, <<"wrong">>),
    ?assertNot(macula_content_chunker:verify_chunk(Chunk, WrongHash, sha256)).

verify_chunk_blake3_test() ->
    Chunk = <<"test chunk data">>,
    Hash = macula_content_hasher:hash(blake3, Chunk),
    ?assert(macula_content_chunker:verify_chunk(Chunk, Hash, blake3)).

%%%===================================================================
%%% chunk_size/0 Tests
%%%===================================================================

default_chunk_size_test() ->
    ?assertEqual(262144, macula_content_chunker:default_chunk_size()).
