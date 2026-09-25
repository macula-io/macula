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
%% single-block `put_content' formula (<<2,16#55,SHA-384(Data)>>), so the
%% single-block path is a strict special case of chunked content, not a
%% second, divergent format.
single_chunk_mcid_matches_raw_sha384_test() ->
    Data = <<"small blob">>,
    {ok, M, _Chunks} = macula_manifest:create(Data, #{chunk_size => 1024}),
    {ok, ChunkMcid} = macula_manifest:chunk_mcid(M, 0),
    ?assertEqual(<<2, 16#55, (crypto:hash(sha384, Data))/binary>>, ChunkMcid).

%% The manifest's OWN mcid is codec 0x56 (manifest), distinct from any
%% chunk's codec 0x55 (raw) — this is how `get_content' will later
%% dispatch single-block vs. chunked fetch by inspecting byte 1.
manifest_mcid_uses_manifest_codec_test() ->
    Data = crypto:strong_rand_bytes(1000),
    {ok, #{mcid := MCID}, _} =
        macula_manifest:create(Data, #{chunk_size => 300}),
    ?assertMatch(<<2, 16#56, _:48/binary>>, MCID).

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
                 macula_manifest:chunk_mcid(M, 5)).

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
    ?assertEqual(48, byte_size(maps:get(root_hash, M))).

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
    <<First, Rest/binary>> = Data,
    Tampered = <<(First bxor 16#FF), Rest/binary>>,
    ?assertEqual({error, root_hash_mismatch},
                 macula_manifest:verify(M, Tampered)).

%%%===================================================================
%%% SHA-384, the only hash algorithm (D24)
%%%===================================================================

manifest_names_sha384_test() ->
    {ok, M, _} = macula_manifest:create(<<"hello world">>, #{}),
    ?assertEqual(sha384, maps:get(hash_algorithm, M)).

chunks_are_hashed_with_sha384_test() ->
    Data = <<"hello world">>,
    {ok, #{chunks := [Chunk]}, _} = macula_manifest:create(Data, #{chunk_size => 1024}),
    ?assertEqual(crypto:hash(sha384, Data), maps:get(hash, Chunk)).

every_chunk_id_verifies_as_a_block_test() ->
    Data = crypto:strong_rand_bytes(700),
    {ok, M, Chunks} = macula_manifest:create(Data, #{chunk_size => 200}),
    [begin
         {ok, ChunkMcid} = macula_manifest:chunk_mcid(M, I),
         ?assertEqual(<<2, 16#55, (crypto:hash(sha384, C))/binary>>, ChunkMcid)
     end || {I, C} <- lists:zip(lists:seq(0, length(Chunks) - 1), Chunks)].

another_hash_algorithm_is_refused_on_create_test_() ->
    [?_assertError(function_clause, macula_manifest:create(<<"hello world">>, #{hash_algorithm => Algorithm}))
     || Algorithm <- [blake3, sha256]].

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
    Hash = crypto:strong_rand_bytes(48),
    Wire = #{<<"mcid">> => <<2, 16#56, Hash/binary>>,
             <<"version">> => 1, <<"name">> => <<"f">>, <<"size">> => 5,
             <<"created">> => 100, <<"chunk_size">> => 262144,
             <<"chunk_count">> => 1, <<"hash_algorithm">> => <<"sha384">>,
             <<"root_hash">> => Hash,
             <<"chunks">> => [#{<<"index">> => 0, <<"offset">> => 0,
                                <<"size">> => 5, <<"hash">> => Hash}]},
    {ok, Read} = macula_manifest:from_wire(Wire),
    ?assertEqual(<<2, 16#56, Hash/binary>>, maps:get(mcid, Read)),
    ?assertEqual(sha384, maps:get(hash_algorithm, Read)),
    ?assertEqual([#{index => 0, offset => 0, size => 5, hash => Hash}],
                 maps:get(chunks, Read)).

from_wire_missing_mcid_is_invalid_test() ->
    ?assertEqual({error, invalid_manifest},
                 macula_manifest:from_wire(#{chunks => []})).

from_wire_missing_chunks_is_invalid_test() ->
    ?assertEqual({error, invalid_manifest},
                 macula_manifest:from_wire(#{mcid => <<1,2,3>>})).

%%%===================================================================
%%% verify_mcid/2: a manifest describes the MCID it is fetched under
%%%===================================================================

verify_mcid_accepts_the_manifest_it_created_test() ->
    {ok, M, _Chunks} = macula_manifest:create(crypto:strong_rand_bytes(700),
                                              #{chunk_size => 200}),
    ?assertEqual(ok, macula_manifest:verify_mcid(M, maps:get(mcid, M))).

%% The manifest a caller fetches has been through the frame codec, as the
%% payload of the provider's signed RESULT. The name crosses the wire as a
%% byte string and the MCID hashes it as text, so the check must still agree
%% after that round trip.
verify_mcid_accepts_a_manifest_after_the_frame_round_trip_test() ->
    {ok, M, _Chunks} = macula_manifest:create(crypto:strong_rand_bytes(700),
                                              #{chunk_size => 200,
                                                name => <<"report.pdf">>}),
    {Request, Provider} = manifest_request(),
    Frame = macula_frame:result(#{request => Request, payload => M}, Provider),
    {ok, Decoded, _Rest} = macula_frame:decode(macula_frame:encode(Frame)),
    {ok, #{payload := Wire}} = macula_frame:verify_reply(Decoded, Request, pq_pure),
    {ok, Read} = macula_manifest:from_wire(Wire),
    ?assertEqual(<<"report.pdf">>, maps:get(name, Read)),
    ?assertEqual(ok, macula_manifest:verify_mcid(Read, maps:get(mcid, M))).

verify_mcid_refuses_a_manifest_for_other_content_test() ->
    {ok, A, _} = macula_manifest:create(crypto:strong_rand_bytes(700), #{chunk_size => 200}),
    {ok, B, _} = macula_manifest:create(crypto:strong_rand_bytes(700), #{chunk_size => 200}),
    McidA = maps:get(mcid, A),
    ?assertEqual({error, manifest_mcid_mismatch},
                 macula_manifest:verify_mcid(B#{mcid => McidA}, McidA)).

verify_mcid_refuses_a_changed_canonical_field_test() ->
    {ok, M, _} = macula_manifest:create(crypto:strong_rand_bytes(700), #{chunk_size => 200}),
    Mcid = maps:get(mcid, M),
    [?assertEqual({error, manifest_mcid_mismatch},
                  macula_manifest:verify_mcid(Changed, Mcid))
     || Changed <- [M#{size := maps:get(size, M) + 1},
                    M#{chunk_count := maps:get(chunk_count, M) + 1},
                    M#{name := <<"renamed">>},
                    M#{root_hash := crypto:strong_rand_bytes(48)}]].

verify_mcid_refuses_a_malformed_manifest_test() ->
    {ok, M, _} = macula_manifest:create(crypto:strong_rand_bytes(700), #{chunk_size => 200}),
    Mcid = maps:get(mcid, M),
    ?assertEqual({error, manifest_mcid_mismatch},
                 macula_manifest:verify_mcid(M#{name := <<255, 254>>}, Mcid)),
    ?assertEqual({error, manifest_mcid_mismatch},
                 macula_manifest:verify_mcid(maps:remove(root_hash, M), Mcid)).

%% The post-quantum format has only tag 2 (D24): a BLAKE3 id, tag 1, never names a manifest.
verify_mcid_refuses_a_blake3_content_id_test() ->
    {ok, M, _} = macula_manifest:create(crypto:strong_rand_bytes(700), #{chunk_size => 200}),
    <<2, Codec, _:48/binary>> = maps:get(mcid, M),
    ?assertEqual({error, manifest_mcid_mismatch},
                 macula_manifest:verify_mcid(M, <<1, Codec, 0:256>>)).

%%%===================================================================
%%% from_wire/1: a manifest as the frame decoder leaves it
%%%===================================================================

%% The frame decoder resolves a key to an atom only when that atom already
%% exists, so in a node that has not loaded this module the field names
%% arrive as `{text, Bin}' keys. from_wire/1 reads them all the same.
from_wire_reads_text_keys_as_the_decoder_leaves_them_test() ->
    {ok, M, _} = macula_manifest:create(crypto:strong_rand_bytes(700),
                                        #{chunk_size => 200}),
    Wire = text_keys(M, [created, hash_algorithm, root_hash, chunk_count, chunk_size]),
    Chunks = [text_keys(C, [offset]) || C <- maps:get(chunks, M)],
    {ok, Read} = macula_manifest:from_wire(Wire#{chunks := Chunks}),
    ?assertEqual(M, Read),
    ?assertEqual(ok, macula_manifest:verify_mcid(Read, maps:get(mcid, M))).

%% A name or hash algorithm sent as text, or as the atom the decoder makes
%% of a text that names an existing atom, is read as its binary value.
from_wire_reads_a_name_and_hash_algorithm_sent_as_text_test() ->
    {ok, M, _} = macula_manifest:create(crypto:strong_rand_bytes(700),
                                        #{chunk_size => 200, name => <<"ok">>}),
    [begin
         {ok, Read} = macula_manifest:from_wire(
                        M#{name := Name, hash_algorithm := {text, <<"sha384">>}}),
         ?assertEqual(M, Read),
         ?assertEqual(ok, macula_manifest:verify_mcid(Read, maps:get(mcid, M)))
     end || Name <- [{text, <<"ok">>}, ok]].

from_wire_refuses_chunks_that_are_not_a_list_of_maps_test() ->
    {ok, M, _} = macula_manifest:create(crypto:strong_rand_bytes(700),
                                        #{chunk_size => 200}),
    ?assertEqual({error, invalid_manifest},
                 macula_manifest:from_wire(M#{chunks := not_a_list})),
    ?assertEqual({error, invalid_manifest},
                 macula_manifest:from_wire(M#{chunks := [not_a_map]})).

%% A manifest must name sha384. A missing hash algorithm, blake3, sha256 or
%% any other name is refused, in any of the shapes it can arrive in.
from_wire_refuses_any_hash_algorithm_but_sha384_test() ->
    {ok, M, _} = macula_manifest:create(crypto:strong_rand_bytes(700),
                                        #{chunk_size => 200}),
    ?assertEqual({error, invalid_manifest},
                 macula_manifest:from_wire(maps:remove(hash_algorithm, M))),
    [?assertEqual({error, invalid_manifest},
                  macula_manifest:from_wire(M#{hash_algorithm := Unknown}))
     || Name <- [<<"blake3">>, <<"sha256">>, <<"sha3">>],
        Unknown <- [Name, {text, Name}, binary_to_atom(Name)]].

verify_mcid_refuses_an_unknown_hash_algorithm_test() ->
    {ok, M, _} = macula_manifest:create(crypto:strong_rand_bytes(700),
                                        #{chunk_size => 200}),
    [?assertEqual({error, manifest_mcid_mismatch},
                  macula_manifest:verify_mcid(M#{hash_algorithm := Unknown}, maps:get(mcid, M)))
     || Unknown <- [sha3, blake3, sha256]].

%%%===================================================================
%%% A manifest that does not describe whole content is refused
%%%===================================================================

%% A chunk size of 0 would re-chunk forever. The manifest is made to match
%% its own MCID, as a peer could make it: from_wire/1 still refuses it, and
%% verify/2 and create/2 end at once. Both run with a capped heap, so a
%% regression fails here instead of filling memory.
a_chunk_size_of_zero_is_refused_everywhere_test_() ->
    {timeout, 5,
     fun() ->
         {ok, M, Chunks} = whole_manifest(),
         Data = iolist_to_binary(Chunks),
         Zero = macula_test_manifest:with_matching_mcid(M#{chunk_size := 0}),
         ?assertEqual(ok, macula_manifest:verify_mcid(Zero, maps:get(mcid, Zero))),
         ?assertEqual({error, invalid_manifest}, macula_manifest:from_wire(Zero)),
         ?assertEqual({returned, {error, invalid_manifest}},
                      macula_test_heap:capped(fun() -> macula_manifest:verify(Zero, Data) end)),
         ?assertEqual({raised, function_clause},
                      macula_test_heap:capped(fun() -> macula_manifest:create(Data, #{chunk_size => 0}) end))
     end}.

%% A chunk count other than the chunks listed, however far off, is refused
%% before anything is counted out by it.
a_chunk_count_other_than_the_chunks_listed_is_refused_test_() ->
    {timeout, 5,
     fun() ->
         {ok, M, _Chunks} = whole_manifest(),
         Count = maps:get(chunk_count, M),
         [begin
              Changed = macula_test_manifest:with_matching_mcid(M#{chunk_count := N}),
              ?assertEqual(ok, macula_manifest:verify_mcid(Changed, maps:get(mcid, Changed))),
              ?assertEqual({error, invalid_manifest}, macula_manifest:from_wire(Changed))
          end || N <- [Count + 1, Count - 1, 1_000_000_000_000, -1]]
     end}.

%% A size the chunks do not cover, including one too large to hold, is
%% refused.
a_size_the_chunks_do_not_cover_is_refused_test() ->
    {ok, M, _} = whole_manifest(),
    Size = maps:get(size, M),
    [?assertEqual({error, invalid_manifest},
                  macula_manifest:from_wire(macula_test_manifest:with_matching_mcid(M#{size := S})))
     || S <- [1 bsl 62, Size + 1, Size - 1, -1]].

%% Chunks out of order, with a gap, empty, larger than the chunk size, cut
%% short before the last, missing, or with a hash that is not 32 bytes, are
%% refused, as is a root hash that is not 32 bytes or a chunk size that is
%% not an integer. So is a chunk past the end of the content, even one whose
%% size makes the offsets add up: only the count the size allows refuses it.
%% A chunk whose index is not its place, and a negative size with no chunks,
%% are refused too. Empty content with no chunks is whole.
a_chunk_list_that_is_not_whole_is_refused_test() ->
    {ok, M, _} = whole_manifest(),
    [C0, C1, C2, C3] = maps:get(chunks, M),
    PastTheEnd = #{index => 4, offset => 800, size => -100, hash => <<0:256>>},
    Refused = fun(Changed) ->
                  ?assertEqual({error, invalid_manifest}, macula_manifest:from_wire(Changed))
              end,
    Refused(M#{chunks := [C1, C0, C2, C3]}),
    Refused(M#{chunks := [C0, C1#{offset := 250}, C2, C3]}),
    Refused(M#{chunks := [C0, C1, C2, C3#{size := 0}]}),
    Refused(M#{chunks := [C0, C1, C2, C3#{size := 201}]}),
    Refused(M#{chunks := [C0, C1#{size := 150}, C2, C3]}),
    Refused(M#{chunks := [C0, C1, C2]}),
    Refused(M#{chunks := [C0, C1#{hash := <<0:248>>}, C2, C3]}),
    Refused(M#{root_hash := <<0:248>>}),
    Refused(M#{chunk_size := <<"200">>}),
    Refused(M#{chunk_count := 5, chunks := [C0, C1, C2, C3, PastTheEnd]}),
    Refused(M#{chunks := [C0, C1#{index := 2}, C2, C3]}),
    Refused(M#{size := -150, chunk_count := 0, chunks := []}),
    {ok, Empty, []} = macula_manifest:create(<<>>, #{}),
    ?assertMatch({ok, _}, macula_manifest:from_wire(Empty)).

%% Chunks are not part of the MCID, so a whole chunk list swapped in for
%% another keeps the manifest's MCID; the bytes fetched through it are then
%% refused by the root hash.
a_substituted_chunk_list_is_refused_by_the_root_hash_test() ->
    DataB = crypto:strong_rand_bytes(700),
    {ok, A, _} = whole_manifest(),
    {ok, B, _} = macula_manifest:create(DataB, #{chunk_size => 200}),
    {ok, Read} = macula_manifest:from_wire(A#{chunks := maps:get(chunks, B)}),
    ?assertEqual(ok, macula_manifest:verify_mcid(Read, maps:get(mcid, A))),
    ?assertEqual({error, root_hash_mismatch}, macula_manifest:verify(Read, DataB)).

whole_manifest() ->
    macula_manifest:create(crypto:strong_rand_bytes(700), #{chunk_size => 200}).


%% End to end in a fresh node that has never loaded macula_manifest: the
%% manifest a provider returns in its signed RESULT decodes and verifies
%% there with text keys, and from_wire/1
%% still reads the manifest the MCID names. The node is checked first, so
%% the test cannot pass on a node that already has the field atoms.
from_wire_reads_a_manifest_decoded_in_a_fresh_node_test_() ->
    {timeout, 60, fun fresh_node_reads_the_manifest/0}.

fresh_node_reads_the_manifest() ->
    {ok, M, _} = macula_manifest:create(crypto:strong_rand_bytes(700),
                                        #{chunk_size => 200}),
    Mcid = maps:get(mcid, M),
    {Request, Provider} = manifest_request(),
    Bin = macula_frame:encode(macula_frame:result(#{request => Request, payload => M}, Provider)),
    Paths = lists:append([["-pa", P] || P <- code:get_path()]),
    {ok, Peer, _Node} = peer:start_link(#{connection => standard_io, args => Paths}),
    try
        ?assertMatch({'EXIT', _}, catch peer:call(Peer, erlang, binary_to_existing_atom,
                                                  [<<"root_hash">>, utf8])),
        {ok, Decoded, <<>>} = peer:call(Peer, macula_frame, decode, [Bin]),
        {ok, #{payload := Wire}} = peer:call(Peer, macula_frame, verify_reply, [Decoded, Request, pq_pure]),
        ?assert(maps:is_key({text, <<"root_hash">>}, Wire)),
        {ok, Read} = peer:call(Peer, macula_manifest, from_wire, [Wire]),
        ?assertEqual(M, Read),
        ?assertEqual(ok, peer:call(Peer, macula_manifest, verify_mcid, [Read, Mcid]))
    after
        peer:stop(Peer)
    end.

%% A verified CALL for a manifest and the provider key that answers it: the
%% manifest reaches the caller as the payload of the provider's signed
%% RESULT, which the caller verifies against its own request.
manifest_request() ->
    {ok, Caller} = macula_node_keys:generate(identity, pq_pure),
    {ok, Provider} = macula_node_keys:generate(identity, pq_pure),
    Spec = #{request_id => <<7:128>>, realm => <<0:256>>, procedure => <<"_content.get_manifest">>,
             target => macula_node_keys:key_id(Provider), deadline => 1789000600000, payload => <<"mcid">>},
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(macula_frame:call(Spec, Caller))),
    {ok, Request} = macula_frame:verify_request(Decoded, pq_pure),
    {Request, Provider}.

text_keys(Map, Keys) ->
    lists:foldl(fun(K, Acc) -> text_key(maps:take(K, Acc), K) end, Map, Keys).

text_key({V, Rest}, K) -> Rest#{{text, atom_to_binary(K)} => V}.
