%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_rpc_router module.
%%% Tests written FIRST (TDD red phase).
%%% Routing strategies for selecting RPC service providers.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_router_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Create test provider
test_provider(NodeId, Port) ->
    #{
        node_id => <<NodeId:256>>,
        address => {{127,0,0,1}, Port},
        metadata => #{},
        last_seen => erlang:system_time(millisecond)
    }.

%% Create test registration
test_registration(Uri, HandlerFn) ->
    #{
        uri => Uri,
        handler => HandlerFn,
        metadata => #{}
    }.

%%%===================================================================
%%% Select Provider Tests - Local First Strategy
%%%===================================================================

%% Test: local_first prefers local handler
select_provider_local_first_test() ->
    LocalHandlers = [test_registration(<<"uri">>, fun(_) -> ok end)],
    RemoteProviders = [test_provider(1, 8080)],

    Result = macula_rpc_router:select_provider(local_first, LocalHandlers, RemoteProviders),

    ?assertMatch({local, _}, Result).

%% Test: local_first falls back to remote when no local
select_provider_local_first_no_local_test() ->
    LocalHandlers = [],
    RemoteProviders = [test_provider(1, 8080), test_provider(2, 8081)],

    Result = macula_rpc_router:select_provider(local_first, LocalHandlers, RemoteProviders),

    ?assertMatch({remote, _}, Result).

%% Test: local_first returns error when no providers
select_provider_local_first_none_test() ->
    Result = macula_rpc_router:select_provider(local_first, [], []),

    ?assertEqual({error, no_provider}, Result).

%%%===================================================================
%%% Select Provider Tests - Round Robin Strategy
%%%===================================================================

%% Test: round_robin rotates through providers
select_provider_round_robin_test() ->
    State = macula_rpc_router:new_state(round_robin),
    RemoteProviders = [
        test_provider(1, 8080),
        test_provider(2, 8081),
        test_provider(3, 8082)
    ],

    %% First call
    {Result1, State2} = macula_rpc_router:select_provider_stateful(State, [], RemoteProviders),
    ?assertMatch({remote, #{node_id := <<1:256>>}}, Result1),

    %% Second call
    {Result2, State3} = macula_rpc_router:select_provider_stateful(State2, [], RemoteProviders),
    ?assertMatch({remote, #{node_id := <<2:256>>}}, Result2),

    %% Third call
    {Result3, State4} = macula_rpc_router:select_provider_stateful(State3, [], RemoteProviders),
    ?assertMatch({remote, #{node_id := <<3:256>>}}, Result3),

    %% Fourth call (wraps around)
    {Result4, _State5} = macula_rpc_router:select_provider_stateful(State4, [], RemoteProviders),
    ?assertMatch({remote, #{node_id := <<1:256>>}}, Result4).

%%%===================================================================
%%% Select Provider Tests - Random Strategy
%%%===================================================================

%% Test: random selects one of the providers
select_provider_random_test() ->
    RemoteProviders = [
        test_provider(1, 8080),
        test_provider(2, 8081),
        test_provider(3, 8082)
    ],

    Result = macula_rpc_router:select_provider(random, [], RemoteProviders),

    ?assertMatch({remote, _}, Result),
    {remote, Provider} = Result,
    NodeId = maps:get(node_id, Provider),
    ?assert(NodeId =:= <<1:256>> orelse NodeId =:= <<2:256>> orelse NodeId =:= <<3:256>>).

%%%===================================================================
%%% Select Provider Tests - Closest Strategy
%%%===================================================================

%% Test: closest selects provider with smallest XOR distance
select_provider_closest_test() ->
    LocalNodeId = <<100:256>>,
    RemoteProviders = [
        test_provider(150, 8080),  % Distance: 150 XOR 100 = 250
        test_provider(101, 8081),  % Distance: 101 XOR 100 = 1 (closest!)
        test_provider(200, 8082)   % Distance: 200 XOR 100 = 172
    ],

    Result = macula_rpc_router:select_provider_closest(LocalNodeId, [], RemoteProviders),

    ?assertMatch({remote, #{node_id := <<101:256>>}}, Result).

%% Test: closest handles single provider
select_provider_closest_single_test() ->
    LocalNodeId = <<100:256>>,
    RemoteProviders = [test_provider(200, 8080)],

    Result = macula_rpc_router:select_provider_closest(LocalNodeId, [], RemoteProviders),

    ?assertMatch({remote, #{node_id := <<200:256>>}}, Result).

%%%===================================================================
%%% Select Local Tests
%%%===================================================================

%% Test: select_local returns first handler
select_local_test() ->
    Handler1 = fun(_) -> {ok, result1} end,
    Handler2 = fun(_) -> {ok, result2} end,
    Registrations = [
        test_registration(<<"uri">>, Handler1),
        test_registration(<<"uri">>, Handler2)
    ],

    Result = macula_rpc_router:select_local(Registrations),

    ?assertMatch({ok, _}, Result),
    {ok, Reg} = Result,
    ?assertEqual(Handler1, maps:get(handler, Reg)).

%% Test: select_local returns not_found for empty
select_local_empty_test() ->
    ?assertEqual(not_found, macula_rpc_router:select_local([])).

%%%===================================================================
%%% Select Remote Tests
%%%===================================================================

%% Test: select_remote_random picks one provider
select_remote_random_test() ->
    Providers = [
        test_provider(1, 8080),
        test_provider(2, 8081)
    ],

    Result = macula_rpc_router:select_remote_random(Providers),

    ?assertMatch({ok, _}, Result).

%% Test: select_remote_random returns not_found for empty
select_remote_random_empty_test() ->
    ?assertEqual(not_found, macula_rpc_router:select_remote_random([])).
