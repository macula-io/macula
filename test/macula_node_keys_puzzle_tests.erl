%% EUnit tests for the node_id puzzle: macula_node_keys:generate/3 with a puzzle difficulty, and puzzle_solved/2.
%% A try makes a new ML-DSA-87 half; a hybrid identity key keeps its RSA-PSS half, and the node_id covers both.
-module(macula_node_keys_puzzle_tests).

-include_lib("eunit/include/eunit.hrl").

%% RSA-4096 key generation takes up to about a second per key.
-define(EU_TIMEOUT, 120).

%%------------------------------------------------------------------
%% puzzle_solved/2
%%------------------------------------------------------------------

zero_difficulty_is_always_solved_test() ->
    ?assert(macula_node_keys:puzzle_solved(<<16#FF, 0:248>>, 0)).

leading_zero_bits_are_counted_test_() ->
    %% 8 zero bits, then 0001 0000: 11 leading zero bits.
    NodeId = <<0:8, 2#00010000:8, 16#FF:8, 0:232>>,
    [?_assert(macula_node_keys:puzzle_solved(NodeId, 11)),
     ?_assertNot(macula_node_keys:puzzle_solved(NodeId, 12))].

all_zero_node_id_meets_the_largest_difficulty_test() ->
    ?assert(macula_node_keys:puzzle_solved(<<0:256>>, 256)).

difficulty_beyond_the_node_id_is_refused_test() ->
    ?assertError(function_clause, macula_node_keys:puzzle_solved(<<0:256>>, 257)).

node_id_of_another_length_is_refused_test() ->
    ?assertError(function_clause, macula_node_keys:puzzle_solved(<<0:248>>, 1)).

%%------------------------------------------------------------------
%% generate/3
%%------------------------------------------------------------------

pq_pure_puzzle_key_meets_its_difficulty_and_signs_as_a_whole_test_() ->
    {timeout, ?EU_TIMEOUT, fun() -> assert_puzzle_key(pq_pure, 8) end}.

pq_hybrid_puzzle_key_meets_its_difficulty_and_signs_as_a_whole_test_() ->
    {timeout, ?EU_TIMEOUT, fun() -> assert_puzzle_key(pq_hybrid, 6) end}.

each_try_changes_the_node_id_and_keeps_the_classical_half_test_() ->
    {timeout, ?EU_TIMEOUT, fun() -> [assert_try(Profile) || Profile <- [pq_pure, pq_hybrid]] end}.

keys_without_a_node_id_take_no_puzzle_test_() ->
    [?_assertEqual({error, not_an_identity_key},
                   macula_node_keys:generate(Purpose, pq_pure, #{puzzle_difficulty => 1}))
     || Purpose <- [connect, tls]].

no_options_generate_as_generate_2_test() ->
    ?assertMatch({ok, #{purpose := identity, profile := pq_pure, components := [#{algorithm := mldsa87}]}},
                 macula_node_keys:generate(identity, pq_pure, #{})).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

assert_puzzle_key(Profile, Difficulty) ->
    {ok, Key} = macula_node_keys:generate(identity, Profile, #{puzzle_difficulty => Difficulty}),
    {ok, NodeId} = macula_node_keys:node_id(Key),
    ?assert(macula_node_keys:puzzle_solved(NodeId, Difficulty)),
    Message = <<"signed by the whole puzzle key">>,
    Signature = macula_node_keys:sign(Message, Key),
    ?assert(macula_node_keys:verify(Message, Signature, macula_node_keys:public_key(Key), Profile)).

assert_try(Profile) ->
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    Next = macula_node_keys:puzzle_candidate(Key),
    [MlDsa | Classical] = maps:get(components, Key),
    [NextMlDsa | NextClassical] = maps:get(components, Next),
    ?assertEqual(Classical, NextClassical),
    ?assertNotEqual(MlDsa, NextMlDsa),
    ?assertNotEqual(macula_node_keys:node_id(Key), macula_node_keys:node_id(Next)).
