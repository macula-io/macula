%%% @doc Tests for macula_manifest — client-side chunking,
%%% Merkle root, and manifest construction, ported byte-for-byte from
%%% macula-station's algorithm so a manifest built here decodes and
%%% verifies correctly against the station's (unmodified)
%%% `_content.put_manifest' / `_content.get_manifest'.
-module(macula_manifest_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Chunking
%%%===================================================================

single_chunk_when_data_fits_test() ->
    Data = <<"hello world">>,
    {ok, M, Chunks} = macula_manifest:create(Data, #{chunk_size => 1024}),
    ?assertEqual(1, maps:get(chunk_count, M)),
    ?assertEqual([Data], Chunks).

multi_chunk_splits_exactly_test() ->
    Data = crypto:strong_rand_bytes(1000),
    {ok, M, Chunks} = macula_manifest:create(Data, #{chunk_size => 300}),
    ?assertEqual(4, maps:get(chunk_count, M)),  %% 300*3 + 100
    ?assertEqual([300, 300, 300, 100], [byte_size(C) || C <- Chunks]),
    ?assertEqual(Data, iolist_to_binary(Chunks)).

empty_data_yields_zero_chunks_test() ->
    {ok, M, Chunks} = macula_manifest:create(<<>>, #{}),
    ?assertEqual(0, maps:get(chunk_count, M)),
    ?assertEqual(0, maps:get(size, M)),
    ?assertEqual([], Chunks).

chunk_offsets_are_contiguous_test() ->
    Data = crypto:strong_rand_bytes(700),
    {ok, #{chunks := Chunks}, _} =
        macula_manifest:create(Data, #{chunk_size => 200}),
    Offsets = [maps:get(offset, C) || C <- Chunks],
    ?assertEqual([0, 200, 400, 600], Offsets).

%%%===================================================================
%%% MCID
%%%===================================================================

%% A blob that fits in ONE chunk must produce the SAME MCID as the
%% existing single-block `put_content' formula (<<1,16#55,BLAKE3(Data)>>)
%% — this is what makes the single-block path a strict, unchanged
%% special case of chunked content, not a second, divergent format.
single_chunk_mcid_matches_raw_blake3_test() ->
    Data = <<"small blob">>,
    {ok, M, _Chunks} = macula_manifest:create(Data, #{chunk_size => 1024}),
    {ok, ChunkMcid} = macula_manifest:chunk_mcid(M, 0, blake3),
    ?assertEqual(<<1, 16#55, (macula_blake3_nif:hash(Data))/binary>>, ChunkMcid).

%% The manifest's OWN mcid is codec 0x56 (manifest), distinct from any
%% chunk's codec 0x55 (raw) — this is how `get_content' will later
%% dispatch single-block vs. chunked fetch by inspecting byte 1.
manifest_mcid_uses_manifest_codec_test() ->
    Data = crypto:strong_rand_bytes(1000),
    {ok, #{mcid := MCID}, _} =
        macula_manifest:create(Data, #{chunk_size => 300}),
    ?assertMatch(<<1, 16#56, _:32/binary>>, MCID).

mcid_is_deterministic_test() ->
    Data = crypto:strong_rand_bytes(500),
    {ok, M1, _} = macula_manifest:create(Data, #{chunk_size => 200}),
    {ok, M2, _} = macula_manifest:create(Data, #{chunk_size => 200}),
    ?assertEqual(maps:get(mcid, M1), maps:get(mcid, M2)).

%% `created' (a timestamp) and `chunks' are excluded from the MCID's
%% canonical form, so two manifests built moments apart from the SAME
%% bytes still address the same content.
mcid_excludes_created_timestamp_test() ->
    Data = crypto:strong_rand_bytes(500),
    {ok, M1, _} = macula_manifest:create(Data, #{chunk_size => 200}),
    timer:sleep(1100),  %% created/0 has second resolution
    {ok, M2, _} = macula_manifest:create(Data, #{chunk_size => 200}),
    ?assertNotEqual(maps:get(created, M1), maps:get(created, M2)),
    ?assertEqual(maps:get(mcid, M1), maps:get(mcid, M2)).

different_content_different_mcid_test() ->
    {ok, M1, _} = macula_manifest:create(<<"a">>, #{chunk_size => 1}),
    {ok, M2, _} = macula_manifest:create(<<"b">>, #{chunk_size => 1}),
    ?assertNotEqual(maps:get(mcid, M1), maps:get(mcid, M2)).

chunk_mcid_out_of_range_test() ->
    {ok, M, _} = macula_manifest:create(<<"x">>, #{}),
    ?assertEqual({error, invalid_index},
                 macula_manifest:chunk_mcid(M, 5, blake3)).

%%%===================================================================
%%% Merkle root — sensitivity + odd-count pairing
%%%===================================================================

root_hash_differs_for_different_chunk_order_test() ->
    A = <<0:2400>>,
    B = <<1:2400>>,
    {ok, M1, _} = macula_manifest:create(<<A/binary, B/binary>>, #{chunk_size => 300}),
    {ok, M2, _} = macula_manifest:create(<<B/binary, A/binary>>, #{chunk_size => 300}),
    ?assertNotEqual(maps:get(root_hash, M1), maps:get(root_hash, M2)).

%% Odd chunk count (5 chunks from 1000 bytes / 250 chunk_size = 4
%% exactly; force an odd count with 900/250 = 4 + partial = wait, use
%% an explicit odd split instead) exercises the last-hash-paired-with-
%% itself branch of the Merkle fold without crashing.
odd_chunk_count_does_not_crash_test() ->
    Data = crypto:strong_rand_bytes(500),  %% chunk_size 200 -> 3 chunks
    {ok, M, _} = macula_manifest:create(Data, #{chunk_size => 200}),
    ?assertEqual(3, maps:get(chunk_count, M)),
    ?assertEqual(32, byte_size(maps:get(root_hash, M))).

%%%===================================================================
%%% verify/2
%%%===================================================================

verify_accepts_matching_reassembly_test() ->
    Data = crypto:strong_rand_bytes(700),
    {ok, M, Chunks} = macula_manifest:create(Data, #{chunk_size => 200}),
    ?assertEqual(ok, macula_manifest:verify(M, iolist_to_binary(Chunks))).

verify_rejects_size_mismatch_test() ->
    Data = crypto:strong_rand_bytes(700),
    {ok, M, _} = macula_manifest:create(Data, #{chunk_size => 200}),
    ?assertEqual({error, size_mismatch},
                 macula_manifest:verify(M, <<"short">>)).

verify_rejects_tampered_bytes_same_size_test() ->
    Data = crypto:strong_rand_bytes(700),
    {ok, M, _} = macula_manifest:create(Data, #{chunk_size => 200}),
    <<_:8, Rest/binary>> = Data,
    Tampered = <<0, Rest/binary>>,
    ?assertEqual({error, root_hash_mismatch},
                 macula_manifest:verify(M, Tampered)).

%%%===================================================================
%%% sha256 algorithm option
%%%===================================================================

%%%===================================================================
%%% from_wire/1
%%%===================================================================

from_wire_atom_keys_round_trips_test() ->
    Data = crypto:strong_rand_bytes(500),
    {ok, M, _Chunks} = macula_manifest:create(Data, #{chunk_size => 200}),
    {ok, Read} = macula_manifest:from_wire(M),
    ?assertEqual(M, Read).

%% Defensive fallback: binary-string keys (the shape if the RPC codec
%% ever fails to preserve atoms for this manifest).
from_wire_binary_keys_test() ->
    Hash = crypto:strong_rand_bytes(32),
    Wire = #{<<"mcid">> => <<1, 16#56, Hash/binary>>,
             <<"version">> => 1, <<"name">> => <<"f">>, <<"size">> => 5,
             <<"created">> => 100, <<"chunk_size">> => 262144,
             <<"chunk_count">> => 1, <<"hash_algorithm">> => <<"blake3">>,
             <<"root_hash">> => Hash,
             <<"chunks">> => [#{<<"index">> => 0, <<"offset">> => 0,
                                <<"size">> => 5, <<"hash">> => Hash}]},
    {ok, Read} = macula_manifest:from_wire(Wire),
    ?assertEqual(<<1, 16#56, Hash/binary>>, maps:get(mcid, Read)),
    ?assertEqual(blake3, maps:get(hash_algorithm, Read)),
    ?assertEqual([#{index => 0, offset => 0, size => 5, hash => Hash}],
                 maps:get(chunks, Read)).

from_wire_missing_mcid_is_invalid_test() ->
    ?assertEqual({error, invalid_manifest},
                 macula_manifest:from_wire(#{chunks => []})).

from_wire_missing_chunks_is_invalid_test() ->
    ?assertEqual({error, invalid_manifest},
                 macula_manifest:from_wire(#{mcid => <<1,2,3>>})).

sha256_algorithm_produces_sha256_chunk_hashes_test() ->
    Data = <<"hello world">>,
    {ok, #{chunks := [Chunk]}, _} =
        macula_manifest:create(Data, #{chunk_size => 1024,
                                               hash_algorithm => sha256}),
    ?assertEqual(crypto:hash(sha256, Data), maps:get(hash, Chunk)).
