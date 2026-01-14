%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_content_hasher module.
%%% Tests hashing functionality for content-addressed storage.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_hasher_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% hash/2 Tests - SHA256
%%%===================================================================

hash_sha256_empty_binary_test() ->
    %% SHA256 of empty string is well-known
    Expected = crypto:hash(sha256, <<>>),
    ?assertEqual(Expected, macula_content_hasher:hash(sha256, <<>>)).

hash_sha256_hello_world_test() ->
    Data = <<"Hello, World!">>,
    Expected = crypto:hash(sha256, Data),
    ?assertEqual(Expected, macula_content_hasher:hash(sha256, Data)).

hash_sha256_binary_data_test() ->
    Data = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 0>>,
    Expected = crypto:hash(sha256, Data),
    ?assertEqual(Expected, macula_content_hasher:hash(sha256, Data)).

hash_sha256_large_binary_test() ->
    Data = crypto:strong_rand_bytes(1024 * 1024),  %% 1MB
    Expected = crypto:hash(sha256, Data),
    ?assertEqual(Expected, macula_content_hasher:hash(sha256, Data)).

hash_sha256_returns_32_bytes_test() ->
    Data = <<"test data">>,
    Hash = macula_content_hasher:hash(sha256, Data),
    ?assertEqual(32, byte_size(Hash)).

%%%===================================================================
%%% hash/2 Tests - BLAKE3
%%%===================================================================

hash_blake3_empty_binary_test() ->
    Hash = macula_content_hasher:hash(blake3, <<>>),
    ?assertEqual(32, byte_size(Hash)).

hash_blake3_hello_world_test() ->
    Hash = macula_content_hasher:hash(blake3, <<"Hello, World!">>),
    ?assertEqual(32, byte_size(Hash)).

hash_blake3_deterministic_test() ->
    Data = <<"deterministic test">>,
    Hash1 = macula_content_hasher:hash(blake3, Data),
    Hash2 = macula_content_hasher:hash(blake3, Data),
    ?assertEqual(Hash1, Hash2).

hash_blake3_different_data_different_hash_test() ->
    Hash1 = macula_content_hasher:hash(blake3, <<"data1">>),
    Hash2 = macula_content_hasher:hash(blake3, <<"data2">>),
    ?assertNotEqual(Hash1, Hash2).

hash_blake3_returns_32_bytes_test() ->
    Data = <<"test data">>,
    Hash = macula_content_hasher:hash(blake3, Data),
    ?assertEqual(32, byte_size(Hash)).

%%%===================================================================
%%% verify/3 Tests
%%%===================================================================

verify_sha256_correct_hash_test() ->
    Data = <<"verify me">>,
    Hash = macula_content_hasher:hash(sha256, Data),
    ?assert(macula_content_hasher:verify(sha256, Data, Hash)).

verify_sha256_incorrect_hash_test() ->
    Data = <<"verify me">>,
    WrongHash = crypto:hash(sha256, <<"wrong data">>),
    ?assertNot(macula_content_hasher:verify(sha256, Data, WrongHash)).

verify_blake3_correct_hash_test() ->
    Data = <<"verify me">>,
    Hash = macula_content_hasher:hash(blake3, Data),
    ?assert(macula_content_hasher:verify(blake3, Data, Hash)).

verify_blake3_incorrect_hash_test() ->
    Data = <<"verify me">>,
    WrongHash = macula_content_hasher:hash(blake3, <<"wrong data">>),
    ?assertNot(macula_content_hasher:verify(blake3, Data, WrongHash)).

verify_empty_data_test() ->
    Hash = macula_content_hasher:hash(sha256, <<>>),
    ?assert(macula_content_hasher:verify(sha256, <<>>, Hash)).

%%%===================================================================
%%% Algorithm Support Tests
%%%===================================================================

supported_algorithms_test() ->
    Algorithms = macula_content_hasher:supported_algorithms(),
    ?assert(lists:member(sha256, Algorithms)),
    ?assert(lists:member(blake3, Algorithms)).

is_supported_sha256_test() ->
    ?assert(macula_content_hasher:is_supported(sha256)).

is_supported_blake3_test() ->
    ?assert(macula_content_hasher:is_supported(blake3)).

is_supported_unknown_test() ->
    ?assertNot(macula_content_hasher:is_supported(md5)).

%%%===================================================================
%%% hash_size/1 Tests
%%%===================================================================

hash_size_sha256_test() ->
    ?assertEqual(32, macula_content_hasher:hash_size(sha256)).

hash_size_blake3_test() ->
    ?assertEqual(32, macula_content_hasher:hash_size(blake3)).

%%%===================================================================
%%% hash_streaming/3 Tests
%%%===================================================================

hash_streaming_sha256_single_chunk_test() ->
    Data = <<"streaming test">>,
    Hash1 = macula_content_hasher:hash(sha256, Data),
    Hash2 = macula_content_hasher:hash_streaming(sha256, [Data]),
    ?assertEqual(Hash1, Hash2).

hash_streaming_sha256_multiple_chunks_test() ->
    Chunks = [<<"chunk1">>, <<"chunk2">>, <<"chunk3">>],
    Combined = iolist_to_binary(Chunks),
    Hash1 = macula_content_hasher:hash(sha256, Combined),
    Hash2 = macula_content_hasher:hash_streaming(sha256, Chunks),
    ?assertEqual(Hash1, Hash2).

hash_streaming_blake3_multiple_chunks_test() ->
    Chunks = [<<"chunk1">>, <<"chunk2">>, <<"chunk3">>],
    Combined = iolist_to_binary(Chunks),
    Hash1 = macula_content_hasher:hash(blake3, Combined),
    Hash2 = macula_content_hasher:hash_streaming(blake3, Chunks),
    ?assertEqual(Hash1, Hash2).

hash_streaming_empty_list_test() ->
    Hash1 = macula_content_hasher:hash(sha256, <<>>),
    Hash2 = macula_content_hasher:hash_streaming(sha256, []),
    ?assertEqual(Hash1, Hash2).

%%%===================================================================
%%% hex_encode/1 and hex_decode/1 Tests
%%%===================================================================

hex_encode_test() ->
    Data = <<1, 2, 255, 0, 128>>,
    Hex = macula_content_hasher:hex_encode(Data),
    ?assertEqual(<<"0102ff0080">>, Hex).

hex_decode_test() ->
    Hex = <<"0102ff0080">>,
    {ok, Data} = macula_content_hasher:hex_decode(Hex),
    ?assertEqual(<<1, 2, 255, 0, 128>>, Data).

hex_roundtrip_test() ->
    Original = crypto:strong_rand_bytes(32),
    Hex = macula_content_hasher:hex_encode(Original),
    {ok, Decoded} = macula_content_hasher:hex_decode(Hex),
    ?assertEqual(Original, Decoded).

hex_decode_invalid_test() ->
    ?assertEqual({error, invalid_hex}, macula_content_hasher:hex_decode(<<"xyz">>)).

hex_decode_uppercase_test() ->
    Hex = <<"0102FF0080">>,
    {ok, Data} = macula_content_hasher:hex_decode(Hex),
    ?assertEqual(<<1, 2, 255, 0, 128>>, Data).
