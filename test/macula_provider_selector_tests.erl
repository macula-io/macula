%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_provider_selector module.
%%% Tests provider selection strategies with actual code execution.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_provider_selector_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Helpers
%%%===================================================================

%% Create test providers
make_provider(NodeId, Endpoint) ->
    #{
        node_id => NodeId,
        endpoint => Endpoint,
        metadata => #{},
        advertised_at => 1234567890,
        ttl => 60000
    }.

%% Create empty state
new_state() ->
    #{counters => #{}}.

%% Create state with service ID
new_state(ServiceId) ->
    #{current_service_id => ServiceId, counters => #{}}.

%%%===================================================================
%%% Empty Provider List Tests
%%%===================================================================

empty_list_default_strategy_test() ->
    State = new_state(),
    Result = macula_provider_selector:select_provider([], State),
    ?assertEqual({error, no_providers}, Result).

empty_list_first_strategy_test() ->
    State = new_state(),
    Result = macula_provider_selector:select_provider([], first, State),
    ?assertEqual({error, no_providers}, Result).

empty_list_random_strategy_test() ->
    State = new_state(),
    Result = macula_provider_selector:select_provider([], random, State),
    ?assertEqual({error, no_providers}, Result).

empty_list_round_robin_strategy_test() ->
    State = new_state(),
    Result = macula_provider_selector:select_provider([], round_robin, State),
    ?assertEqual({error, no_providers}, Result).

%%%===================================================================
%%% First Strategy Tests
%%%===================================================================

first_strategy_single_provider_test() ->
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    State = new_state(),

    {ok, Selected, _NewState} = macula_provider_selector:select_provider([P1], first, State),
    ?assertEqual(P1, Selected).

first_strategy_multiple_providers_test() ->
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    P2 = make_provider(<<2>>, <<"endpoint2">>),
    P3 = make_provider(<<3>>, <<"endpoint3">>),
    State = new_state(),

    {ok, Selected, _NewState} = macula_provider_selector:select_provider([P1, P2, P3], first, State),
    ?assertEqual(P1, Selected).

first_strategy_always_first_test() ->
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    P2 = make_provider(<<2>>, <<"endpoint2">>),
    State = new_state(),

    %% Call multiple times
    {ok, S1, State2} = macula_provider_selector:select_provider([P1, P2], first, State),
    {ok, S2, State3} = macula_provider_selector:select_provider([P1, P2], first, State2),
    {ok, S3, _State4} = macula_provider_selector:select_provider([P1, P2], first, State3),

    %% Should always return first provider
    ?assertEqual(P1, S1),
    ?assertEqual(P1, S2),
    ?assertEqual(P1, S3).

first_strategy_state_unchanged_test() ->
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    State = new_state(),

    {ok, _Selected, NewState} = macula_provider_selector:select_provider([P1], first, State),
    ?assertEqual(State, NewState).

%%%===================================================================
%%% Random Strategy Tests
%%%===================================================================

random_strategy_single_provider_test() ->
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    State = new_state(),

    {ok, Selected, _NewState} = macula_provider_selector:select_provider([P1], random, State),
    ?assertEqual(P1, Selected).

random_strategy_returns_valid_provider_test() ->
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    P2 = make_provider(<<2>>, <<"endpoint2">>),
    P3 = make_provider(<<3>>, <<"endpoint3">>),
    Providers = [P1, P2, P3],
    State = new_state(),

    {ok, Selected, _NewState} = macula_provider_selector:select_provider(Providers, random, State),
    ?assert(lists:member(Selected, Providers)).

random_strategy_multiple_calls_test() ->
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    P2 = make_provider(<<2>>, <<"endpoint2">>),
    P3 = make_provider(<<3>>, <<"endpoint3">>),
    Providers = [P1, P2, P3],
    State = new_state(),

    %% Call 10 times, collect results
    Results = lists:map(fun(_) ->
        {ok, Selected, _} = macula_provider_selector:select_provider(Providers, random, State),
        Selected
    end, lists:seq(1, 10)),

    %% All results should be valid providers
    lists:foreach(fun(Result) ->
        ?assert(lists:member(Result, Providers))
    end, Results).

random_strategy_default_function_test() ->
    %% select_provider/2 uses random as default
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    P2 = make_provider(<<2>>, <<"endpoint2">>),
    Providers = [P1, P2],
    State = new_state(),

    {ok, Selected, _NewState} = macula_provider_selector:select_provider(Providers, State),
    ?assert(lists:member(Selected, Providers)).

random_strategy_state_unchanged_test() ->
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    State = new_state(),

    {ok, _Selected, NewState} = macula_provider_selector:select_provider([P1], random, State),
    ?assertEqual(State, NewState).

%%%===================================================================
%%% Round-Robin Strategy Tests
%%%===================================================================

round_robin_single_provider_test() ->
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    State = new_state(<<"service1">>),

    {ok, Selected, _NewState} = macula_provider_selector:select_provider([P1], round_robin, State),
    ?assertEqual(P1, Selected).

round_robin_two_providers_alternates_test() ->
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    P2 = make_provider(<<2>>, <<"endpoint2">>),
    State = new_state(<<"service1">>),

    {ok, S1, State2} = macula_provider_selector:select_provider([P1, P2], round_robin, State),
    {ok, S2, State3} = macula_provider_selector:select_provider([P1, P2], round_robin, State2),
    {ok, S3, State4} = macula_provider_selector:select_provider([P1, P2], round_robin, State3),
    {ok, S4, _State5} = macula_provider_selector:select_provider([P1, P2], round_robin, State4),

    %% Should alternate: P1, P2, P1, P2
    ?assertEqual(P1, S1),
    ?assertEqual(P2, S2),
    ?assertEqual(P1, S3),
    ?assertEqual(P2, S4).

round_robin_three_providers_rotates_test() ->
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    P2 = make_provider(<<2>>, <<"endpoint2">>),
    P3 = make_provider(<<3>>, <<"endpoint3">>),
    State = new_state(<<"service1">>),

    {ok, S1, State2} = macula_provider_selector:select_provider([P1, P2, P3], round_robin, State),
    {ok, S2, State3} = macula_provider_selector:select_provider([P1, P2, P3], round_robin, State2),
    {ok, S3, State4} = macula_provider_selector:select_provider([P1, P2, P3], round_robin, State3),
    {ok, S4, _State5} = macula_provider_selector:select_provider([P1, P2, P3], round_robin, State4),

    %% Should rotate: P1, P2, P3, P1
    ?assertEqual(P1, S1),
    ?assertEqual(P2, S2),
    ?assertEqual(P3, S3),
    ?assertEqual(P1, S4).

round_robin_counter_increments_test() ->
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    P2 = make_provider(<<2>>, <<"endpoint2">>),
    ServiceId = <<"service1">>,
    State = new_state(ServiceId),

    {ok, _S1, State2} = macula_provider_selector:select_provider([P1, P2], round_robin, State),
    {ok, _S2, State3} = macula_provider_selector:select_provider([P1, P2], round_robin, State2),
    {ok, _S3, State4} = macula_provider_selector:select_provider([P1, P2], round_robin, State3),

    %% Check counter incremented
    Counters = maps:get(counters, State4),
    Counter = maps:get(ServiceId, Counters),
    ?assertEqual(3, Counter).

round_robin_default_service_id_test() ->
    %% When no service_id in state, uses "default"
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    P2 = make_provider(<<2>>, <<"endpoint2">>),
    State = new_state(),  % No service_id

    {ok, S1, State2} = macula_provider_selector:select_provider([P1, P2], round_robin, State),
    {ok, S2, State3} = macula_provider_selector:select_provider([P1, P2], round_robin, State2),

    %% Should still alternate
    ?assertEqual(P1, S1),
    ?assertEqual(P2, S2),

    %% Check counter using default service ID
    Counters = maps:get(counters, State3),
    Counter = maps:get(<<"default">>, Counters),
    ?assertEqual(2, Counter).

round_robin_separate_service_counters_test() ->
    %% Different services should have independent counters
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    P2 = make_provider(<<2>>, <<"endpoint2">>),

    State1 = new_state(<<"service1">>),
    State2 = new_state(<<"service2">>),

    %% Service 1: Call twice
    {ok, _S1_1, State1_2} = macula_provider_selector:select_provider([P1, P2], round_robin, State1),
    {ok, _S1_2, State1_3} = macula_provider_selector:select_provider([P1, P2], round_robin, State1_2),

    %% Service 2: Call once
    {ok, _S2_1, State2_2} = macula_provider_selector:select_provider([P1, P2], round_robin, State2),

    %% Check counters are independent
    Counters1 = maps:get(counters, State1_3),
    Counter1 = maps:get(<<"service1">>, Counters1),
    ?assertEqual(2, Counter1),

    Counters2 = maps:get(counters, State2_2),
    Counter2 = maps:get(<<"service2">>, Counters2),
    ?assertEqual(1, Counter2).

round_robin_many_calls_test() ->
    %% Test with many calls to ensure counter doesn't break
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    P2 = make_provider(<<2>>, <<"endpoint2">>),
    P3 = make_provider(<<3>>, <<"endpoint3">>),
    Providers = [P1, P2, P3],
    State = new_state(<<"service1">>),

    %% Call 100 times
    {FinalState, Results} = lists:foldl(fun(_, {StateAcc, ResultsAcc}) ->
        {ok, Selected, NewStateAcc} = macula_provider_selector:select_provider(Providers, round_robin, StateAcc),
        {NewStateAcc, [Selected | ResultsAcc]}
    end, {State, []}, lists:seq(1, 100)),

    %% Check counter
    Counters = maps:get(counters, FinalState),
    Counter = maps:get(<<"service1">>, Counters),
    ?assertEqual(100, Counter),

    %% Verify all selected providers are valid
    lists:foreach(fun(Selected) ->
        ?assert(lists:member(Selected, Providers))
    end, Results).

%%%===================================================================
%%% State Management Tests
%%%===================================================================

state_with_empty_counters_test() ->
    State = #{counters => #{}},
    P1 = make_provider(<<1>>, <<"endpoint1">>),

    {ok, _Selected, NewState} = macula_provider_selector:select_provider([P1], round_robin, State),
    ?assert(maps:is_key(counters, NewState)).

state_with_existing_counter_test() ->
    ServiceId = <<"service1">>,
    State = #{
        current_service_id => ServiceId,
        counters => #{ServiceId => 5}
    },
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    P2 = make_provider(<<2>>, <<"endpoint2">>),

    %% Counter = 5, length = 2, so Index = (5 rem 2) + 1 = 2 (P2)
    {ok, Selected, NewState} = macula_provider_selector:select_provider([P1, P2], round_robin, State),
    ?assertEqual(P2, Selected),

    %% Counter should be 6 now
    Counters = maps:get(counters, NewState),
    Counter = maps:get(ServiceId, Counters),
    ?assertEqual(6, Counter).

state_preserved_across_strategies_test() ->
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    State = #{custom_field => custom_value, counters => #{}},

    %% First strategy doesn't modify state
    {ok, _Selected, NewState1} = macula_provider_selector:select_provider([P1], first, State),
    ?assertEqual(State, NewState1),

    %% Random strategy doesn't modify state
    {ok, _Selected2, NewState2} = macula_provider_selector:select_provider([P1], random, State),
    ?assertEqual(State, NewState2).

%%%===================================================================
%%% Provider Info Structure Tests
%%%===================================================================

provider_with_all_fields_test() ->
    Provider = #{
        node_id => <<1, 2, 3, 4>>,
        endpoint => <<"http://example.com:8080">>,
        metadata => #{key => value},
        advertised_at => 1234567890,
        ttl => 60000
    },
    State = new_state(),

    {ok, Selected, _NewState} = macula_provider_selector:select_provider([Provider], first, State),
    ?assertEqual(Provider, Selected).

provider_with_minimal_fields_test() ->
    Provider = #{
        node_id => <<1>>,
        endpoint => <<"endpoint">>,
        metadata => #{}
    },
    State = new_state(),

    {ok, Selected, _NewState} = macula_provider_selector:select_provider([Provider], first, State),
    ?assertEqual(Provider, Selected).

provider_map_preserved_test() ->
    %% Ensure the provider map is returned as-is, not modified
    Provider = #{
        node_id => <<16#aa, 16#bb>>,
        endpoint => <<"test">>,
        metadata => #{foo => bar, baz => qux}
    },
    State = new_state(),

    {ok, Selected, _NewState} = macula_provider_selector:select_provider([Provider], random, State),
    ?assertEqual(maps:get(node_id, Provider), maps:get(node_id, Selected)),
    ?assertEqual(maps:get(endpoint, Provider), maps:get(endpoint, Selected)),
    ?assertEqual(maps:get(metadata, Provider), maps:get(metadata, Selected)).

%%%===================================================================
%%% Edge Cases and Integration Tests
%%%===================================================================

large_provider_list_test() ->
    %% Create 100 providers
    Providers = lists:map(fun(N) ->
        make_provider(<<N>>, list_to_binary("endpoint" ++ integer_to_list(N)))
    end, lists:seq(1, 100)),

    State = new_state(),

    %% First strategy
    {ok, Selected1, _} = macula_provider_selector:select_provider(Providers, first, State),
    ?assertEqual(lists:nth(1, Providers), Selected1),

    %% Random strategy
    {ok, Selected2, _} = macula_provider_selector:select_provider(Providers, random, State),
    ?assert(lists:member(Selected2, Providers)),

    %% Round-robin strategy
    {ok, Selected3, State2} = macula_provider_selector:select_provider(Providers, round_robin, State),
    ?assertEqual(lists:nth(1, Providers), Selected3),
    {ok, Selected4, _State3} = macula_provider_selector:select_provider(Providers, round_robin, State2),
    ?assertEqual(lists:nth(2, Providers), Selected4).

round_robin_wraps_around_test() ->
    %% Test counter wrapping at very large values
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    P2 = make_provider(<<2>>, <<"endpoint2">>),
    ServiceId = <<"service1">>,

    %% Start with very large counter
    LargeCounter = 999999999999999999,
    State = #{
        current_service_id => ServiceId,
        counters => #{ServiceId => LargeCounter}
    },

    %% Index = (999999999999999999 rem 2) + 1 = 1 + 1 = 2 (P2)
    {ok, Selected1, State2} = macula_provider_selector:select_provider([P1, P2], round_robin, State),
    ?assertEqual(P2, Selected1),

    %% Next call: counter = 1000000000000000000, Index = 0 + 1 = 1 (P1)
    {ok, Selected2, _State3} = macula_provider_selector:select_provider([P1, P2], round_robin, State2),
    ?assertEqual(P1, Selected2).

strategies_work_with_same_providers_test() ->
    P1 = make_provider(<<1>>, <<"endpoint1">>),
    P2 = make_provider(<<2>>, <<"endpoint2">>),
    Providers = [P1, P2],
    State = new_state(<<"service1">>),

    %% All strategies should work with the same provider list
    {ok, SelectedFirst, _} = macula_provider_selector:select_provider(Providers, first, State),
    {ok, SelectedRandom, _} = macula_provider_selector:select_provider(Providers, random, State),
    {ok, SelectedRR, _} = macula_provider_selector:select_provider(Providers, round_robin, State),

    ?assertEqual(P1, SelectedFirst),
    ?assert(lists:member(SelectedRandom, Providers)),
    ?assertEqual(P1, SelectedRR).
