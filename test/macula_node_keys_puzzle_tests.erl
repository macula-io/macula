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
%% The difficulty is a constant, not a setting (D30)
%%------------------------------------------------------------------

%% puzzle_difficulty/0 is what every node grinds to and every station checks. The macula application's
%% puzzle_difficulty setting of 10.x would be read by nothing, so a node that sets it, to any value, is refused rather
%% than left believing it chose its difficulty.
no_puzzle_difficulty_setting_passes_the_check_test() ->
    ?assertEqual(undefined, application:get_env(macula, puzzle_difficulty)),
    ?assertEqual(ok, macula_node_keys:check_puzzle_difficulty()).

a_puzzle_difficulty_setting_is_refused_test_() ->
    [?_assertError({bad_config, {macula, puzzle_difficulty, {not_a_setting, Value}}},
                   with_puzzle_difficulty(Value, fun macula_node_keys:check_puzzle_difficulty/0))
     || Value <- [macula_node_keys:puzzle_difficulty(), 12, <<"12">>]].

%% The macula application checks it when it starts, on a fresh peer node; without the setting the peer starts macula,
%% which shows that it can.
application_start_refuses_a_puzzle_difficulty_setting_test_() ->
    {timeout, 200,
     [{"without the setting the macula application starts",
       {timeout, 90, fun() -> ?assertMatch({ok, _}, start_macula_with(#{})) end}},
      {"with the setting the macula application does not start",
       {timeout, 90, fun() ->
           Started = start_macula_with(#{puzzle_difficulty => 8}),
           ?assert(names(Started, {bad_config, {macula, puzzle_difficulty, {not_a_setting, 8}}}))
       end}}]}.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

with_puzzle_difficulty(Value, Fun) ->
    ok = application:set_env(macula, puzzle_difficulty, Value),
    try
        Fun()
    after
        application:unset_env(macula, puzzle_difficulty)
    end.

%% Starts macula on a fresh peer node with Env set in the macula application, and returns what
%% application:ensure_all_started/1 returned there.
start_macula_with(Env) ->
    Paths = lists:append([["-pa", P] || P <- code:get_path()]),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Peer, _Node} = peer:start_link(#{connection => standard_io, args => Paths}),
    try
        ok = peer:call(Peer, application, load, [macula]),
        %% A peer reads no test sys.config, and macula refuses to start without a profile: give it this VM's.
        ok = peer:call(Peer, application, set_env, [macula, crypto_profile, Profile]),
        [ok = peer:call(Peer, application, set_env, [macula, Key, Value]) || Key := Value <- Env],
        peer:call(Peer, application, ensure_all_started, [macula], 60_000)
    after
        peer:stop(Peer)
    end.

%% Whether Wanted appears anywhere inside Term.
names(Wanted, Wanted) -> true;
names(Tuple, Wanted) when is_tuple(Tuple) -> names(tuple_to_list(Tuple), Wanted);
names([Head | Tail], Wanted) -> names(Head, Wanted) orelse names(Tail, Wanted);
names(_Other, _Wanted) -> false.

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
