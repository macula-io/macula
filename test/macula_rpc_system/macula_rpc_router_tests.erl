%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_rpc_router module.
%%%
%%% Tests RPC call routing strategies.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_router_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% new_state/1 Tests
%%%===================================================================

new_state_creates_state_test() ->
    %% WHEN: Creating new router state
    State = macula_rpc_router:new_state(round_robin),

    %% THEN: Should have initial values
    ?assertEqual(#{strategy => round_robin, round_robin_index => 0}, State).

new_state_different_strategies_test() ->
    %% GIVEN: Different strategies
    Strategies = [local_first, round_robin, random, closest],

    %% WHEN: Creating states
    States = [macula_rpc_router:new_state(S) || S <- Strategies],

    %% THEN: All should be valid states
    ?assertEqual(4, length(States)).

%%%===================================================================
%%% select_local/1 Tests
%%%===================================================================

select_local_returns_first_handler_test() ->
    %% GIVEN: Multiple local handlers
    Handler1 = #{uri => <<"s1">>, handler => fun(_) -> {ok, 1} end, metadata => #{}},
    Handler2 = #{uri => <<"s1">>, handler => fun(_) -> {ok, 2} end, metadata => #{}},
    Handlers = [Handler1, Handler2],

    %% WHEN: Selecting local
    Result = macula_rpc_router:select_local(Handlers),

    %% THEN: Should return first handler
    ?assertEqual({ok, Handler1}, Result).

select_local_returns_not_found_for_empty_test() ->
    %% GIVEN: No handlers
    Handlers = [],

    %% WHEN: Selecting local
    Result = macula_rpc_router:select_local(Handlers),

    %% THEN: Should return not_found
    ?assertEqual(not_found, Result).

%%%===================================================================
%%% select_remote_random/1 Tests
%%%===================================================================

select_remote_random_returns_provider_test() ->
    %% GIVEN: Multiple remote providers
    Providers = [
        #{node_id => <<"node1">>, address => {{1,1,1,1}, 4433}},
        #{node_id => <<"node2">>, address => {{2,2,2,2}, 4433}},
        #{node_id => <<"node3">>, address => {{3,3,3,3}, 4433}}
    ],

    %% WHEN: Selecting random
    Result = macula_rpc_router:select_remote_random(Providers),

    %% THEN: Should return one of the providers
    ?assertMatch({ok, _}, Result),
    {ok, Selected} = Result,
    ?assert(lists:member(Selected, Providers)).

select_remote_random_returns_not_found_for_empty_test() ->
    %% GIVEN: No providers
    Providers = [],

    %% WHEN: Selecting random
    Result = macula_rpc_router:select_remote_random(Providers),

    %% THEN: Should return not_found
    ?assertEqual(not_found, Result).

%%%===================================================================
%%% select_provider/3 - local_first Tests
%%%===================================================================

select_provider_local_first_prefers_local_test() ->
    %% GIVEN: Both local and remote options
    LocalHandler = #{uri => <<"s1">>, handler => fun(_) -> {ok, 1} end, metadata => #{}},
    LocalHandlers = [LocalHandler],
    RemoteProviders = [#{node_id => <<"remote">>, address => {{10,0,0,1}, 4433}}],

    %% WHEN: Selecting with local_first
    Result = macula_rpc_router:select_provider(local_first, LocalHandlers, RemoteProviders),

    %% THEN: Should select local
    ?assertEqual({local, LocalHandler}, Result).

select_provider_local_first_falls_back_to_remote_test() ->
    %% GIVEN: No local handlers
    LocalHandlers = [],
    RemoteProvider = #{node_id => <<"remote">>, address => {{10,0,0,1}, 4433}},
    RemoteProviders = [RemoteProvider],

    %% WHEN: Selecting with local_first
    Result = macula_rpc_router:select_provider(local_first, LocalHandlers, RemoteProviders),

    %% THEN: Should select remote
    ?assertEqual({remote, RemoteProvider}, Result).

select_provider_local_first_no_provider_test() ->
    %% GIVEN: No handlers or providers
    LocalHandlers = [],
    RemoteProviders = [],

    %% WHEN: Selecting with local_first
    Result = macula_rpc_router:select_provider(local_first, LocalHandlers, RemoteProviders),

    %% THEN: Should return error
    ?assertEqual({error, no_provider}, Result).

%%%===================================================================
%%% select_provider/3 - random Tests
%%%===================================================================

select_provider_random_selects_remote_test() ->
    %% GIVEN: Remote providers (random ignores local)
    LocalHandlers = [#{uri => <<"s1">>, handler => fun(_) -> {ok, 1} end, metadata => #{}}],
    RemoteProvider = #{node_id => <<"remote">>, address => {{10,0,0,1}, 4433}},
    RemoteProviders = [RemoteProvider],

    %% WHEN: Selecting with random
    Result = macula_rpc_router:select_provider(random, LocalHandlers, RemoteProviders),

    %% THEN: Should select remote (random strategy ignores local)
    ?assertEqual({remote, RemoteProvider}, Result).

select_provider_random_no_provider_test() ->
    %% GIVEN: No remote providers
    LocalHandlers = [#{uri => <<"s1">>, handler => fun(_) -> {ok, 1} end, metadata => #{}}],
    RemoteProviders = [],

    %% WHEN: Selecting with random
    Result = macula_rpc_router:select_provider(random, LocalHandlers, RemoteProviders),

    %% THEN: Should return error (random only considers remote)
    ?assertEqual({error, no_provider}, Result).

%%%===================================================================
%%% select_provider/3 - round_robin and closest
%%%===================================================================

select_provider_round_robin_requires_stateful_api_test() ->
    %% GIVEN: Round robin strategy
    LocalHandlers = [],
    RemoteProviders = [#{node_id => <<"n1">>, address => {{1,1,1,1}, 4433}}],

    %% WHEN: Using stateless API for round_robin
    Result = macula_rpc_router:select_provider(round_robin, LocalHandlers, RemoteProviders),

    %% THEN: Should return error indicating use stateful API
    ?assertEqual({error, use_stateful_api}, Result).

select_provider_closest_requires_closest_api_test() ->
    %% GIVEN: Closest strategy
    LocalHandlers = [],
    RemoteProviders = [#{node_id => <<"n1">>, address => {{1,1,1,1}, 4433}}],

    %% WHEN: Using stateless API for closest
    Result = macula_rpc_router:select_provider(closest, LocalHandlers, RemoteProviders),

    %% THEN: Should return error indicating use closest API
    ?assertEqual({error, use_closest_api}, Result).

%%%===================================================================
%%% select_provider_stateful/3 - round_robin Tests
%%%===================================================================

select_provider_stateful_round_robin_cycles_test() ->
    %% GIVEN: Multiple remote providers
    Provider1 = #{node_id => <<"n1">>, address => {{1,1,1,1}, 4433}},
    Provider2 = #{node_id => <<"n2">>, address => {{2,2,2,2}, 4433}},
    Provider3 = #{node_id => <<"n3">>, address => {{3,3,3,3}, 4433}},
    RemoteProviders = [Provider1, Provider2, Provider3],
    LocalHandlers = [],
    State0 = macula_rpc_router:new_state(round_robin),

    %% WHEN: Selecting multiple times
    {{remote, R1}, State1} = macula_rpc_router:select_provider_stateful(State0, LocalHandlers, RemoteProviders),
    {{remote, R2}, State2} = macula_rpc_router:select_provider_stateful(State1, LocalHandlers, RemoteProviders),
    {{remote, R3}, State3} = macula_rpc_router:select_provider_stateful(State2, LocalHandlers, RemoteProviders),
    {{remote, R4}, _State4} = macula_rpc_router:select_provider_stateful(State3, LocalHandlers, RemoteProviders),

    %% THEN: Should cycle through providers
    ?assertEqual(Provider1, R1),
    ?assertEqual(Provider2, R2),
    ?assertEqual(Provider3, R3),
    ?assertEqual(Provider1, R4).  % Back to first

select_provider_stateful_round_robin_empty_test() ->
    %% GIVEN: No providers
    RemoteProviders = [],
    LocalHandlers = [],
    State = macula_rpc_router:new_state(round_robin),

    %% WHEN: Selecting
    {Result, _NewState} = macula_rpc_router:select_provider_stateful(State, LocalHandlers, RemoteProviders),

    %% THEN: Should return error
    ?assertEqual({error, no_provider}, Result).

select_provider_stateful_other_strategies_use_stateless_test() ->
    %% GIVEN: local_first strategy with state
    LocalHandler = #{uri => <<"s1">>, handler => fun(_) -> {ok, 1} end, metadata => #{}},
    LocalHandlers = [LocalHandler],
    RemoteProviders = [],
    State = macula_rpc_router:new_state(local_first),

    %% WHEN: Using stateful API for non-round_robin
    {Result, _NewState} = macula_rpc_router:select_provider_stateful(State, LocalHandlers, RemoteProviders),

    %% THEN: Should delegate to stateless and return local
    ?assertEqual({local, LocalHandler}, Result).

%%%===================================================================
%%% select_provider_closest/3 Tests
%%%===================================================================

select_provider_closest_prefers_local_test() ->
    %% GIVEN: Local handler available
    LocalNodeId = <<0:256>>,
    LocalHandler = #{uri => <<"s1">>, handler => fun(_) -> {ok, 1} end, metadata => #{}},
    LocalHandlers = [LocalHandler],
    RemoteProviders = [#{node_id => <<1:256>>, address => {{1,1,1,1}, 4433}}],

    %% WHEN: Selecting closest
    Result = macula_rpc_router:select_provider_closest(LocalNodeId, LocalHandlers, RemoteProviders),

    %% THEN: Should prefer local
    ?assertEqual({local, LocalHandler}, Result).

select_provider_closest_selects_nearest_remote_test() ->
    %% GIVEN: No local handlers, multiple remote with different distances
    LocalNodeId = <<0:256>>,
    LocalHandlers = [],
    %% Provider with smaller node_id has smaller XOR distance to 0
    CloserProvider = #{node_id => <<1:256>>, address => {{1,1,1,1}, 4433}},
    FartherProvider = #{node_id => <<255:256>>, address => {{2,2,2,2}, 4433}},
    RemoteProviders = [FartherProvider, CloserProvider],  % Listed in "wrong" order

    %% WHEN: Selecting closest
    Result = macula_rpc_router:select_provider_closest(LocalNodeId, LocalHandlers, RemoteProviders),

    %% THEN: Should select closer provider
    ?assertEqual({remote, CloserProvider}, Result).

select_provider_closest_no_provider_test() ->
    %% GIVEN: No handlers or providers
    LocalNodeId = <<0:256>>,
    LocalHandlers = [],
    RemoteProviders = [],

    %% WHEN: Selecting closest
    Result = macula_rpc_router:select_provider_closest(LocalNodeId, LocalHandlers, RemoteProviders),

    %% THEN: Should return error
    ?assertEqual({error, no_provider}, Result).
