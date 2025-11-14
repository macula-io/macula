%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for macula_provider_selector module.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_provider_selector_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% Test empty provider list
empty_providers_test() ->
    State = #{strategy => random, counters => #{}},
    Result = macula_provider_selector:select_provider([], random, State),
    ?assertEqual({error, no_providers}, Result).

%% Test single provider (all strategies should return it)
single_provider_test() ->
    Provider = #{
        node_id => <<"node-1">>,
        endpoint => <<"https://node1:9443">>,
        metadata => #{},
        ttl => 300
    },
    Providers = [Provider],
    State = #{strategy => random, counters => #{}},

    %% Test each strategy
    {ok, P1, _} = macula_provider_selector:select_provider(Providers, first, State),
    ?assertEqual(Provider, P1),

    {ok, P2, _} = macula_provider_selector:select_provider(Providers, random, State),
    ?assertEqual(Provider, P2),

    {ok, P3, _} = macula_provider_selector:select_provider(Providers, round_robin, State),
    ?assertEqual(Provider, P3).

%% Test first strategy always picks first provider
first_strategy_test() ->
    P1 = #{node_id => <<"node-1">>, endpoint => <<"https://node1:9443">>, metadata => #{}},
    P2 = #{node_id => <<"node-2">>, endpoint => <<"https://node2:9443">>, metadata => #{}},
    P3 = #{node_id => <<"node-3">>, endpoint => <<"https://node3:9443">>, metadata => #{}},
    Providers = [P1, P2, P3],
    State = #{strategy => first, counters => #{}},

    %% All calls should return first provider
    {ok, Selected1, State2} = macula_provider_selector:select_provider(Providers, first, State),
    ?assertEqual(P1, Selected1),

    {ok, Selected2, _State3} = macula_provider_selector:select_provider(Providers, first, State2),
    ?assertEqual(P1, Selected2).

%% Test random strategy returns valid provider
random_strategy_test() ->
    P1 = #{node_id => <<"node-1">>, endpoint => <<"https://node1:9443">>, metadata => #{}},
    P2 = #{node_id => <<"node-2">>, endpoint => <<"https://node2:9443">>, metadata => #{}},
    P3 = #{node_id => <<"node-3">>, endpoint => <<"https://node3:9443">>, metadata => #{}},
    Providers = [P1, P2, P3],
    State = #{strategy => random, counters => #{}},

    %% Make several selections - should return valid providers
    {ok, Selected1, State2} = macula_provider_selector:select_provider(Providers, random, State),
    ?assert(lists:member(Selected1, Providers)),

    {ok, Selected2, State3} = macula_provider_selector:select_provider(Providers, random, State2),
    ?assert(lists:member(Selected2, Providers)),

    {ok, Selected3, _State4} = macula_provider_selector:select_provider(Providers, random, State3),
    ?assert(lists:member(Selected3, Providers)).

%% Test round-robin distributes calls evenly
round_robin_strategy_test() ->
    P1 = #{node_id => <<"node-1">>, endpoint => <<"https://node1:9443">>, metadata => #{}},
    P2 = #{node_id => <<"node-2">>, endpoint => <<"https://node2:9443">>, metadata => #{}},
    P3 = #{node_id => <<"node-3">>, endpoint => <<"https://node3:9443">>, metadata => #{}},
    Providers = [P1, P2, P3],
    State = #{
        strategy => round_robin,
        counters => #{},
        current_service_id => <<"test.service">>
    },

    %% First 3 calls should cycle through all providers
    {ok, Selected1, State2} = macula_provider_selector:select_provider(Providers, round_robin, State),
    ?assertEqual(P1, Selected1),  % Index 1

    {ok, Selected2, State3} = macula_provider_selector:select_provider(Providers, round_robin, State2),
    ?assertEqual(P2, Selected2),  % Index 2

    {ok, Selected3, State4} = macula_provider_selector:select_provider(Providers, round_robin, State3),
    ?assertEqual(P3, Selected3),  % Index 3

    %% 4th call should wrap around to first provider
    {ok, Selected4, _State5} = macula_provider_selector:select_provider(Providers, round_robin, State4),
    ?assertEqual(P1, Selected4).  % Index 1 again

%% Test round-robin with different services
round_robin_multiple_services_test() ->
    P1 = #{node_id => <<"node-1">>, endpoint => <<"https://node1:9443">>, metadata => #{}},
    P2 = #{node_id => <<"node-2">>, endpoint => <<"https://node2:9443">>, metadata => #{}},
    Providers = [P1, P2],

    %% Service A
    StateA1 = #{
        strategy => round_robin,
        counters => #{},
        current_service_id => <<"service.A">>
    },
    {ok, SelectedA1, StateA2} = macula_provider_selector:select_provider(Providers, round_robin, StateA1),
    ?assertEqual(P1, SelectedA1),

    %% Service B (different counter)
    StateB1 = StateA2#{current_service_id => <<"service.B">>},
    {ok, SelectedB1, StateB2} = macula_provider_selector:select_provider(Providers, round_robin, StateB1),
    ?assertEqual(P1, SelectedB1),  % Starts at P1 (counter=0)

    %% Service A again
    StateA3 = StateB2#{current_service_id => <<"service.A">>},
    {ok, SelectedA2, _StateA4} = macula_provider_selector:select_provider(Providers, round_robin, StateA3),
    ?assertEqual(P2, SelectedA2).  % Next in rotation for service A

%% Test default strategy (2-argument version)
default_strategy_test() ->
    P1 = #{node_id => <<"node-1">>, endpoint => <<"https://node1:9443">>, metadata => #{}},
    P2 = #{node_id => <<"node-2">>, endpoint => <<"https://node2:9443">>, metadata => #{}},
    Providers = [P1, P2],
    State = #{strategy => random, counters => #{}},

    %% 2-argument version should use random strategy
    {ok, Selected, _State2} = macula_provider_selector:select_provider(Providers, State),
    ?assert(lists:member(Selected, Providers)).

%% Test round-robin counter increments correctly
round_robin_counter_test() ->
    P1 = #{node_id => <<"node-1">>, endpoint => <<"https://node1:9443">>, metadata => #{}},
    P2 = #{node_id => <<"node-2">>, endpoint => <<"https://node2:9443">>, metadata => #{}},
    Providers = [P1, P2],

    State = #{
        strategy => round_robin,
        counters => #{},
        current_service_id => <<"test.service">>
    },

    %% After 1 call, counter should be 1
    {ok, _P1, State2} = macula_provider_selector:select_provider(Providers, round_robin, State),
    Counters2 = maps:get(counters, State2),
    ?assertEqual(1, maps:get(<<"test.service">>, Counters2)),

    %% After 2 calls, counter should be 2
    {ok, _P2, State3} = macula_provider_selector:select_provider(Providers, round_robin, State2),
    Counters3 = maps:get(counters, State3),
    ?assertEqual(2, maps:get(<<"test.service">>, Counters3)),

    %% After 3 calls, counter should be 3
    {ok, _P1again, State4} = macula_provider_selector:select_provider(Providers, round_robin, State3),
    Counters4 = maps:get(counters, State4),
    ?assertEqual(3, maps:get(<<"test.service">>, Counters4)).
