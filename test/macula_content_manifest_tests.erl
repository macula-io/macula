%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_content_manifest module.
%%% Tests manifest creation, encoding, decoding, and verification.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_manifest_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% create/2 Tests
%%%===================================================================

create_from_binary_test() ->
    Data = <<"test content for manifest">>,
    {ok, Manifest} = macula_content_manifest:create(Data, #{}),
    ?assert(is_map(Manifest)).

create_has_mcid_test() ->
    Data = <<"test content">>,
    {ok, Manifest} = macula_content_manifest:create(Data, #{}),
    MCID = maps:get(mcid, Manifest),
    ?assertEqual(34, byte_size(MCID)).

create_has_correct_size_test() ->
    Data = crypto:strong_rand_bytes(12345),
    {ok, Manifest} = macula_content_manifest:create(Data, #{}),
    ?assertEqual(12345, maps:get(size, Manifest)).

create_has_name_test() ->
    Data = <<"test">>,
    {ok, Manifest} = macula_content_manifest:create(Data, #{name => <<"my_file.txt">>}),
    ?assertEqual(<<"my_file.txt">>, maps:get(name, Manifest)).

create_has_default_name_test() ->
    Data = <<"test">>,
    {ok, Manifest} = macula_content_manifest:create(Data, #{}),
    ?assertEqual(<<"unnamed">>, maps:get(name, Manifest)).

create_has_chunk_info_test() ->
    Data = crypto:strong_rand_bytes(3000),
    {ok, Manifest} = macula_content_manifest:create(Data, #{chunk_size => 1024}),
    Chunks = maps:get(chunks, Manifest),
    ?assertEqual(3, length(Chunks)).

create_has_root_hash_test() ->
    Data = <<"test content">>,
    {ok, Manifest} = macula_content_manifest:create(Data, #{}),
    RootHash = maps:get(root_hash, Manifest),
    ?assertEqual(32, byte_size(RootHash)).

create_has_created_timestamp_test() ->
    Data = <<"test">>,
    {ok, Manifest} = macula_content_manifest:create(Data, #{}),
    Created = maps:get(created, Manifest),
    Now = erlang:system_time(second),
    ?assert(Created =< Now),
    ?assert(Created > Now - 10).

create_with_custom_chunk_size_test() ->
    Data = crypto:strong_rand_bytes(5000),
    {ok, Manifest} = macula_content_manifest:create(Data, #{chunk_size => 512}),
    ?assertEqual(512, maps:get(chunk_size, Manifest)),
    Chunks = maps:get(chunks, Manifest),
    ?assertEqual(10, length(Chunks)).

create_with_sha256_test() ->
    Data = <<"test">>,
    {ok, Manifest} = macula_content_manifest:create(Data, #{hash_algorithm => sha256}),
    ?assertEqual(sha256, maps:get(hash_algorithm, Manifest)).

create_with_blake3_test() ->
    Data = <<"test">>,
    {ok, Manifest} = macula_content_manifest:create(Data, #{hash_algorithm => blake3}),
    ?assertEqual(blake3, maps:get(hash_algorithm, Manifest)).

create_deterministic_test() ->
    Data = <<"deterministic test">>,
    {ok, M1} = macula_content_manifest:create(Data, #{name => <<"test">>}),
    {ok, M2} = macula_content_manifest:create(Data, #{name => <<"test">>}),
    %% MCIDs should match (same content, same name)
    ?assertEqual(maps:get(mcid, M1), maps:get(mcid, M2)).

%%%===================================================================
%%% MCID Tests
%%%===================================================================

mcid_format_test() ->
    Data = <<"test">>,
    {ok, Manifest} = macula_content_manifest:create(Data, #{}),
    MCID = maps:get(mcid, Manifest),
    <<Version:8, Codec:8, _Hash:32/binary>> = MCID,
    ?assertEqual(1, Version),
    ?assert(Codec =:= 16#55 orelse Codec =:= 16#56).  %% raw or manifest codec

mcid_to_string_test() ->
    Data = <<"test">>,
    {ok, Manifest} = macula_content_manifest:create(Data, #{}),
    MCID = maps:get(mcid, Manifest),
    String = macula_content_manifest:mcid_to_string(MCID),
    ?assert(is_binary(String)),
    ?assertEqual(<<"mcid1-">>, binary:part(String, 0, 6)).

mcid_from_string_test() ->
    Data = <<"test">>,
    {ok, Manifest} = macula_content_manifest:create(Data, #{}),
    MCID = maps:get(mcid, Manifest),
    String = macula_content_manifest:mcid_to_string(MCID),
    {ok, Decoded} = macula_content_manifest:mcid_from_string(String),
    ?assertEqual(MCID, Decoded).

mcid_roundtrip_test() ->
    Data = crypto:strong_rand_bytes(1000),
    {ok, Manifest} = macula_content_manifest:create(Data, #{}),
    MCID = maps:get(mcid, Manifest),
    String = macula_content_manifest:mcid_to_string(MCID),
    {ok, Decoded} = macula_content_manifest:mcid_from_string(String),
    ?assertEqual(MCID, Decoded).

mcid_from_invalid_string_test() ->
    ?assertEqual({error, invalid_mcid}, macula_content_manifest:mcid_from_string(<<"invalid">>)).

%%%===================================================================
%%% encode/1 and decode/1 Tests
%%%===================================================================

encode_returns_binary_test() ->
    Data = <<"test">>,
    {ok, Manifest} = macula_content_manifest:create(Data, #{}),
    {ok, Encoded} = macula_content_manifest:encode(Manifest),
    ?assert(is_binary(Encoded)).

decode_returns_manifest_test() ->
    Data = <<"test">>,
    {ok, Manifest} = macula_content_manifest:create(Data, #{}),
    {ok, Encoded} = macula_content_manifest:encode(Manifest),
    {ok, Decoded} = macula_content_manifest:decode(Encoded),
    ?assert(is_map(Decoded)).

encode_decode_roundtrip_test() ->
    Data = crypto:strong_rand_bytes(5000),
    {ok, Manifest} = macula_content_manifest:create(Data, #{
        name => <<"test.tar.gz">>,
        chunk_size => 1024
    }),
    {ok, Encoded} = macula_content_manifest:encode(Manifest),
    {ok, Decoded} = macula_content_manifest:decode(Encoded),
    %% Key fields should match
    ?assertEqual(maps:get(mcid, Manifest), maps:get(mcid, Decoded)),
    ?assertEqual(maps:get(size, Manifest), maps:get(size, Decoded)),
    ?assertEqual(maps:get(name, Manifest), maps:get(name, Decoded)),
    ?assertEqual(maps:get(chunk_count, Manifest), maps:get(chunk_count, Decoded)).

decode_invalid_binary_test() ->
    ?assertEqual({error, invalid_manifest}, macula_content_manifest:decode(<<"not msgpack">>)).

%%%===================================================================
%%% verify/2 Tests
%%%===================================================================

verify_correct_data_test() ->
    Data = <<"verify this content">>,
    {ok, Manifest} = macula_content_manifest:create(Data, #{}),
    ?assertEqual(ok, macula_content_manifest:verify(Manifest, Data)).

verify_incorrect_data_test() ->
    Data = <<"original data">>,
    {ok, Manifest} = macula_content_manifest:create(Data, #{}),
    ?assertEqual({error, root_hash_mismatch},
                 macula_content_manifest:verify(Manifest, <<"modified data">>)).

verify_wrong_size_test() ->
    Data = <<"original">>,
    {ok, Manifest} = macula_content_manifest:create(Data, #{}),
    ?assertEqual({error, size_mismatch},
                 macula_content_manifest:verify(Manifest, <<"different size data">>)).

verify_large_data_test() ->
    Data = crypto:strong_rand_bytes(100000),
    {ok, Manifest} = macula_content_manifest:create(Data, #{chunk_size => 1024}),
    ?assertEqual(ok, macula_content_manifest:verify(Manifest, Data)).

%%%===================================================================
%%% Chunk Info Tests
%%%===================================================================

chunks_have_index_test() ->
    Data = crypto:strong_rand_bytes(3000),
    {ok, Manifest} = macula_content_manifest:create(Data, #{chunk_size => 1024}),
    Chunks = maps:get(chunks, Manifest),
    Indices = [maps:get(index, C) || C <- Chunks],
    ?assertEqual([0, 1, 2], Indices).

chunks_have_offset_test() ->
    Data = crypto:strong_rand_bytes(3000),
    {ok, Manifest} = macula_content_manifest:create(Data, #{chunk_size => 1024}),
    Chunks = maps:get(chunks, Manifest),
    Offsets = [maps:get(offset, C) || C <- Chunks],
    ?assertEqual([0, 1024, 2048], Offsets).

chunks_have_size_test() ->
    Data = crypto:strong_rand_bytes(3000),
    {ok, Manifest} = macula_content_manifest:create(Data, #{chunk_size => 1024}),
    Chunks = maps:get(chunks, Manifest),
    Sizes = [maps:get(size, C) || C <- Chunks],
    ?assertEqual([1024, 1024, 952], Sizes).  %% 3000 - 2048 = 952

chunks_have_hash_test() ->
    Data = crypto:strong_rand_bytes(3000),
    {ok, Manifest} = macula_content_manifest:create(Data, #{chunk_size => 1024}),
    Chunks = maps:get(chunks, Manifest),
    lists:foreach(
        fun(Chunk) ->
            Hash = maps:get(hash, Chunk),
            ?assertEqual(32, byte_size(Hash))
        end,
        Chunks
    ).

%%%===================================================================
%%% get_chunk_mcid/2 Tests
%%%===================================================================

get_chunk_mcid_test() ->
    Data = crypto:strong_rand_bytes(3000),
    {ok, Manifest} = macula_content_manifest:create(Data, #{chunk_size => 1024}),
    {ok, MCID} = macula_content_manifest:get_chunk_mcid(Manifest, 0),
    ?assertEqual(34, byte_size(MCID)).

get_chunk_mcid_invalid_index_test() ->
    Data = crypto:strong_rand_bytes(3000),
    {ok, Manifest} = macula_content_manifest:create(Data, #{chunk_size => 1024}),
    ?assertEqual({error, invalid_index}, macula_content_manifest:get_chunk_mcid(Manifest, 10)).

%%%===================================================================
%%% version/0 Tests
%%%===================================================================

manifest_version_test() ->
    ?assertEqual(1, macula_content_manifest:version()).
