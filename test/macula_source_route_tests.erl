%% EUnit tests for hecate_source_route.
-module(macula_source_route_tests).

-include_lib("eunit/include/eunit.hrl").

%%---------------------------------------------------------------------
%% Construction
%%---------------------------------------------------------------------

new_truncates_full_node_ids_to_16_bytes_test() ->
    Hop = crypto:strong_rand_bytes(32),
    H = macula_source_route:new([Hop], 12345),
    [Stored] = macula_source_route:hops(H),
    ?assertEqual(16, byte_size(Stored)),
    ?assertEqual(binary:part(Hop, 0, 16), Stored).

new_accepts_already_truncated_hop_ids_test() ->
    Hop = crypto:strong_rand_bytes(16),
    H = macula_source_route:new([Hop], 12345),
    ?assertEqual([Hop], macula_source_route:hops(H)).

new_default_current_hop_is_zero_test() ->
    H = macula_source_route:new([sample_hop()], 1),
    ?assertEqual(0, macula_source_route:current_hop(H)).

new_path_hash_matches_recompute_test() ->
    Hops = [sample_hop() || _ <- lists:seq(1, 4)],
    H = macula_source_route:new(Hops, 12345),
    ?assertEqual(ok, macula_source_route:verify(H)),
    ?assertEqual(16, byte_size(macula_source_route:path_hash(H))).

new_rejects_zero_hops_test() ->
    ?assertError(function_clause,
                 macula_source_route:new([], 1)).

new_rejects_more_than_eight_hops_test() ->
    Hops = [sample_hop() || _ <- lists:seq(1, 9)],
    ?assertError(function_clause,
                 macula_source_route:new(Hops, 1)).

new_rejects_current_hop_above_total_test() ->
    ?assertError(function_clause,
                 macula_source_route:new([sample_hop()], 1, 5)).

%%---------------------------------------------------------------------
%% Wire codec round-trip
%%---------------------------------------------------------------------

encode_has_expected_byte_count_test() ->
    Hops = [sample_hop() || _ <- lists:seq(1, 3)],
    H = macula_source_route:new(Hops, 1),
    Bin = macula_source_route:encode(H),
    %% 27 fixed overhead + 16 × 3 hops = 75 bytes
    ?assertEqual(27 + 16 * 3, byte_size(Bin)).

encode_decode_round_trip_test() ->
    Hops = [sample_hop() || _ <- lists:seq(1, 5)],
    H = macula_source_route:new(Hops, 999_888_777),
    {ok, Decoded} = macula_source_route:decode(macula_source_route:encode(H)),
    ?assertEqual(H, Decoded).

encode_decode_round_trip_after_advance_test() ->
    Hops = [sample_hop() || _ <- lists:seq(1, 4)],
    H1 = macula_source_route:advance(macula_source_route:new(Hops, 1)),
    H2 = macula_source_route:advance(H1),
    {ok, D} = macula_source_route:decode(macula_source_route:encode(H2)),
    ?assertEqual(2, macula_source_route:current_hop(D)).

decode_short_buffer_returns_truncated_test() ->
    ?assertEqual({error, truncated}, macula_source_route:decode(<<1, 2, 3>>)).

decode_rejects_bad_version_test() ->
    Hops = [sample_hop()],
    Bin = macula_source_route:encode(macula_source_route:new(Hops, 1)),
    Tampered = <<99, (binary:part(Bin, 1, byte_size(Bin) - 1))/binary>>,
    ?assertEqual({error, bad_version}, macula_source_route:decode(Tampered)).

decode_rejects_total_hops_above_max_test() ->
    %% Synthesise a header claiming 9 hops.
    Bin = <<1:8, 9:8, 0:8, 1:64/big-unsigned, 0:128,
            (crypto:strong_rand_bytes(9 * 16))/binary>>,
    ?assertEqual({error, bad_total_hops}, macula_source_route:decode(Bin)).

decode_rejects_current_hop_above_total_test() ->
    %% total_hops=2, current_hop=5 — invalid.
    Hops = [sample_hop(), sample_hop()],
    HopsBin = iolist_to_binary(Hops),
    PathHash = compute_hash_for(Hops),
    Bin = <<1:8, 2:8, 5:8, 1:64/big-unsigned, PathHash/binary, HopsBin/binary>>,
    ?assertEqual({error, bad_current_hop}, macula_source_route:decode(Bin)).

decode_rejects_path_hash_mismatch_test() ->
    Hops = [sample_hop(), sample_hop()],
    H = macula_source_route:new(Hops, 1),
    %% Mutate the first hop byte after encoding.
    Bin = macula_source_route:encode(H),
    Off = 27, %% start of hops
    <<Pre:Off/binary, _OldByte:8, Tail/binary>> = Bin,
    Tampered = <<Pre/binary, 16#FF:8, Tail/binary>>,
    ?assertEqual({error, path_hash_mismatch},
                 macula_source_route:decode(Tampered)).

decode_rejects_truncated_hops_test() ->
    %% Header claims 3 hops but only carries 2.
    Bin = <<1:8, 3:8, 0:8, 1:64/big-unsigned, 0:128,
            (crypto:strong_rand_bytes(2 * 16))/binary>>,
    ?assertEqual({error, truncated}, macula_source_route:decode(Bin)).

%%---------------------------------------------------------------------
%% verify/1
%%---------------------------------------------------------------------

verify_accepts_well_formed_test() ->
    H = macula_source_route:new([sample_hop()], 1),
    ?assertEqual(ok, macula_source_route:verify(H)).

verify_rejects_mutated_hops_test() ->
    H = macula_source_route:new([sample_hop(), sample_hop()], 1),
    Tampered = H#{hops := [sample_hop(), sample_hop()]},
    ?assertEqual({error, path_hash_mismatch},
                 macula_source_route:verify(Tampered)).

%%---------------------------------------------------------------------
%% advance/1
%%---------------------------------------------------------------------

advance_increments_current_hop_test() ->
    H = macula_source_route:new([sample_hop(), sample_hop()], 1),
    H1 = macula_source_route:advance(H),
    ?assertEqual(1, macula_source_route:current_hop(H1)),
    H2 = macula_source_route:advance(H1),
    ?assertEqual(2, macula_source_route:current_hop(H2)),
    ?assert(macula_source_route:is_complete(H2)).

advance_on_complete_path_errors_test() ->
    H = macula_source_route:new([sample_hop()], 1, 1),
    ?assertError(path_already_complete, macula_source_route:advance(H)).

%%---------------------------------------------------------------------
%% Position helpers
%%---------------------------------------------------------------------

current_hop_id_returns_current_hop_test() ->
    [A, B, C] = [sample_hop() || _ <- lists:seq(1, 3)],
    H = macula_source_route:new([A, B, C], 1),
    ?assertEqual({ok, A}, macula_source_route:current_hop_id(H)),
    H1 = macula_source_route:advance(H),
    ?assertEqual({ok, B}, macula_source_route:current_hop_id(H1)),
    H2 = macula_source_route:advance(H1),
    ?assertEqual({ok, C}, macula_source_route:current_hop_id(H2)),
    H3 = macula_source_route:advance(H2),
    ?assertEqual(error, macula_source_route:current_hop_id(H3)).

next_hop_id_returns_following_hop_test() ->
    [A, B, C] = [sample_hop() || _ <- lists:seq(1, 3)],
    H = macula_source_route:new([A, B, C], 1),
    ?assertEqual({ok, B}, macula_source_route:next_hop_id(H)),
    H1 = macula_source_route:advance(H),
    ?assertEqual({ok, C}, macula_source_route:next_hop_id(H1)),
    H2 = macula_source_route:advance(H1),
    %% Now at the final hop — no further next-hop.
    ?assertEqual(error, macula_source_route:next_hop_id(H2)).

is_final_hop_at_last_position_test() ->
    H = macula_source_route:new([sample_hop(), sample_hop()], 1),
    ?assertNot(macula_source_route:is_final_hop(H)),
    ?assert(macula_source_route:is_final_hop(
              macula_source_route:advance(H))).

%%---------------------------------------------------------------------
%% truncate_hop/1
%%---------------------------------------------------------------------

truncate_hop_keeps_first_16_bytes_of_node_id_test() ->
    NodeId = crypto:strong_rand_bytes(32),
    ?assertEqual(binary:part(NodeId, 0, 16),
                 macula_source_route:truncate_hop(NodeId)).

truncate_hop_passes_16_byte_input_through_test() ->
    Hop = crypto:strong_rand_bytes(16),
    ?assertEqual(Hop, macula_source_route:truncate_hop(Hop)).

%%---------------------------------------------------------------------
%% Helpers
%%---------------------------------------------------------------------

sample_hop() ->
    crypto:strong_rand_bytes(16).

compute_hash_for(Hops) ->
    Concat = iolist_to_binary(Hops),
    <<Trunc:16/binary, _/binary>> = crypto:hash(sha256, Concat),
    Trunc.
