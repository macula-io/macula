%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_routing_dht module.
%%% Tests written FIRST (TDD red phase).
%%% Core DHT algorithms: iterative lookup, store, find.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_dht_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Iterative Lookup Tests
%%%===================================================================

%% Test: iterative_find_node returns k closest nodes
iterative_find_node_returns_k_test() ->
    %% Setup: routing table with some nodes
    LocalNodeId = <<0:256>>,
    RoutingTable = macula_routing_table:new(LocalNodeId, 20),

    %% Add some nodes
    Nodes = [
        #{node_id => <<100:256>>, address => {{127,0,0,1}, 8080}},
        #{node_id => <<101:256>>, address => {{127,0,0,1}, 8081}},
        #{node_id => <<110:256>>, address => {{127,0,0,1}, 8082}},
        #{node_id => <<120:256>>, address => {{127,0,0,1}, 8083}},
        #{node_id => <<150:256>>, address => {{127,0,0,1}, 8084}}
    ],

    RoutingTable2 = lists:foldl(
        fun(Node, Acc) -> macula_routing_table:add_node(Acc, Node) end,
        RoutingTable,
        Nodes
    ),

    Target = <<105:256>>,
    K = 3,

    %% Mock query function (returns empty - we're testing initial seed)
    QueryFn = fun(_Node, _Target) -> {ok, []} end,

    Result = macula_routing_dht:iterative_find_node(RoutingTable2, Target, K, QueryFn),

    ?assertMatch({ok, _Closest}, Result),
    {ok, Closest} = Result,
    ?assert(length(Closest) >= 3 orelse length(Closest) =:= length(Nodes)).

%% Test: iterative_find_node returns nodes sorted by distance
iterative_find_node_sorted_test() ->
    LocalNodeId = <<0:256>>,
    RoutingTable = macula_routing_table:new(LocalNodeId, 20),

    Nodes = [
        #{node_id => <<110:256>>, address => {{127,0,0,1}, 8080}},  % Distance 10
        #{node_id => <<100:256>>, address => {{127,0,0,1}, 8081}},  % Distance 0
        #{node_id => <<101:256>>, address => {{127,0,0,1}, 8082}}   % Distance 1
    ],

    RoutingTable2 = lists:foldl(
        fun(Node, Acc) -> macula_routing_table:add_node(Acc, Node) end,
        RoutingTable,
        Nodes
    ),

    Target = <<100:256>>,
    QueryFn = fun(_Node, _Target) -> {ok, []} end,

    {ok, [First, Second, Third]} = macula_routing_dht:iterative_find_node(RoutingTable2, Target, 3, QueryFn),

    ?assertEqual(<<100:256>>, maps:get(node_id, First)),   % Closest
    ?assertEqual(<<101:256>>, maps:get(node_id, Second)),  % Next
    ?assertEqual(<<110:256>>, maps:get(node_id, Third)).   % Farthest

%% Test: iterative_find_node handles query function responses
iterative_find_node_with_responses_test() ->
    LocalNodeId = <<0:256>>,
    RoutingTable = macula_routing_table:new(LocalNodeId, 20),

    %% Add initial node
    InitialNode = #{node_id => <<100:256>>, address => {{127,0,0,1}, 8080}},
    RoutingTable2 = macula_routing_table:add_node(RoutingTable, InitialNode),

    Target = <<100:256>>,

    %% Mock query function returns additional nodes
    QueryFn = fun(_Node, _Target) ->
        {ok, [
            #{node_id => <<101:256>>, address => {{127,0,0,1}, 8081}},
            #{node_id => <<102:256>>, address => {{127,0,0,1}, 8082}}
        ]}
    end,

    {ok, Closest} = macula_routing_dht:iterative_find_node(RoutingTable2, Target, 3, QueryFn),

    %% Should have discovered nodes via query
    ?assert(length(Closest) >= 3).

%%%===================================================================
%%% Store Value Tests
%%%===================================================================

%% Test: store_value stores at k closest nodes
store_value_stores_at_k_nodes_test() ->
    LocalNodeId = <<0:256>>,
    RoutingTable = macula_routing_table:new(LocalNodeId, 20),

    Nodes = [
        #{node_id => <<100:256>>, address => {{127,0,0,1}, 8080}},
        #{node_id => <<101:256>>, address => {{127,0,0,1}, 8081}},
        #{node_id => <<110:256>>, address => {{127,0,0,1}, 8082}}
    ],

    RoutingTable2 = lists:foldl(
        fun(Node, Acc) -> macula_routing_table:add_node(Acc, Node) end,
        RoutingTable,
        Nodes
    ),

    Key = <<100:256>>,
    Value = <<"test_value">>,
    K = 2,

    %% Track which nodes received store requests
    StoreCount = ets:new(store_count, [set, public]),

    StoreFn = fun(Node, _Key, _Value) ->
        NodeId = maps:get(node_id, Node),
        ets:insert(StoreCount, {NodeId, true}),
        ok
    end,

    QueryFn = fun(_Node, _Target) -> {ok, []} end,

    Result = macula_routing_dht:store_value(RoutingTable2, Key, Value, K, QueryFn, StoreFn),

    ?assertEqual(ok, Result),

    %% Verify at least K nodes were contacted
    StoreList = ets:tab2list(StoreCount),
    ets:delete(StoreCount),
    ?assert(length(StoreList) >= K).

%%%===================================================================
%%% Find Value Tests
%%%===================================================================

%% Test: find_value returns value if found
find_value_returns_value_test() ->
    LocalNodeId = <<0:256>>,
    RoutingTable = macula_routing_table:new(LocalNodeId, 20),

    Node = #{node_id => <<100:256>>, address => {{127,0,0,1}, 8080}},
    RoutingTable2 = macula_routing_table:add_node(RoutingTable, Node),

    Key = <<100:256>>,
    ExpectedValue = <<"found_value">>,

    %% Mock query that returns value
    QueryFn = fun(_Node, _Key) ->
        {value, ExpectedValue}
    end,

    Result = macula_routing_dht:find_value(RoutingTable2, Key, 20, QueryFn),

    ?assertEqual({ok, ExpectedValue}, Result).

%% Test: find_value returns nodes if value not found
find_value_returns_nodes_test() ->
    LocalNodeId = <<0:256>>,
    RoutingTable = macula_routing_table:new(LocalNodeId, 20),

    Nodes = [
        #{node_id => <<100:256>>, address => {{127,0,0,1}, 8080}},
        #{node_id => <<101:256>>, address => {{127,0,0,1}, 8081}}
    ],

    RoutingTable2 = lists:foldl(
        fun(Node, Acc) -> macula_routing_table:add_node(Acc, Node) end,
        RoutingTable,
        Nodes
    ),

    Key = <<105:256>>,

    %% Mock query that doesn't find value
    QueryFn = fun(_Node, _Key) ->
        {nodes, [#{node_id => <<102:256>>, address => {{127,0,0,1}, 8082}}]}
    end,

    Result = macula_routing_dht:find_value(RoutingTable2, Key, 20, QueryFn),

    ?assertMatch({nodes, _ClosestNodes}, Result),
    {nodes, ClosestNodes} = Result,
    ?assert(length(ClosestNodes) > 0).

%%%===================================================================
%%% Closest Set Tests
%%%===================================================================

%% Test: update_closest maintains k closest nodes
update_closest_maintains_k_test() ->
    Target = <<100:256>>,
    K = 3,

    Initial = [
        #{node_id => <<110:256>>, address => {{127,0,0,1}, 8080}},  % Distance 10
        #{node_id => <<105:256>>, address => {{127,0,0,1}, 8081}},  % Distance 5
        #{node_id => <<103:256>>, address => {{127,0,0,1}, 8082}}   % Distance 3
    ],

    %% Add closer node
    NewNode = #{node_id => <<101:256>>, address => {{127,0,0,1}, 8083}},  % Distance 1

    Updated = macula_routing_dht:update_closest(Initial, [NewNode], Target, K),

    ?assertEqual(3, length(Updated)),

    %% Verify closest node is first
    [First | _] = Updated,
    ?assertEqual(<<101:256>>, maps:get(node_id, First)).

%% Test: update_closest removes duplicates
update_closest_removes_duplicates_test() ->
    Target = <<100:256>>,
    K = 3,

    Initial = [
        #{node_id => <<101:256>>, address => {{127,0,0,1}, 8080}}
    ],

    %% Add same node again (different address - testing node_id dedup)
    Duplicate = #{node_id => <<101:256>>, address => {{127,0,0,1}, 9999}},

    Updated = macula_routing_dht:update_closest(Initial, [Duplicate], Target, K),

    ?assertEqual(1, length(Updated)).

%%%===================================================================
%%% Alpha Concurrency Tests
%%%===================================================================

%% Test: select_alpha returns up to alpha unqueried nodes
select_alpha_returns_alpha_test() ->
    Closest = [
        #{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}},
        #{node_id => <<2:256>>, address => {{127,0,0,1}, 8081}},
        #{node_id => <<3:256>>, address => {{127,0,0,1}, 8082}},
        #{node_id => <<4:256>>, address => {{127,0,0,1}, 8083}},
        #{node_id => <<5:256>>, address => {{127,0,0,1}, 8084}}
    ],

    Queried = [<<1:256>>, <<2:256>>],  % Already queried
    Alpha = 3,

    ToQuery = macula_routing_dht:select_alpha(Closest, Queried, Alpha),

    ?assertEqual(3, length(ToQuery)),

    %% Verify none are in queried set
    ToQueryIds = [maps:get(node_id, N) || N <- ToQuery],
    ?assertNot(lists:member(<<1:256>>, ToQueryIds)),
    ?assertNot(lists:member(<<2:256>>, ToQueryIds)).

%% Test: select_alpha returns less than alpha if not enough nodes
select_alpha_returns_less_test() ->
    Closest = [
        #{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}},
        #{node_id => <<2:256>>, address => {{127,0,0,1}, 8081}}
    ],

    Queried = [<<1:256>>],
    Alpha = 5,  % Want more than available

    ToQuery = macula_routing_dht:select_alpha(Closest, Queried, Alpha),

    ?assertEqual(1, length(ToQuery)).  % Only one unqueried node left
