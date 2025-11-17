%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_routing_dht module.
%%% Tests Kademlia DHT iterative lookup algorithms.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_dht_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% Helper to create test node
sample_node(Suffix) ->
    NodeId = <<Suffix:256>>,
    #{
        node_id => NodeId,
        address => {{127, 0, 0, 1}, 4433 + Suffix}
    }.

%% Helper to create list of nodes
sample_nodes(Suffixes) ->
    [sample_node(S) || S <- Suffixes].

%%%===================================================================
%%% NOTE: deduplicate_nodes/1 is an internal helper function and is
%%% tested indirectly through the public API (update_closest/4).
%%%===================================================================

%%%===================================================================
%%% update_closest/4 Tests
%%%===================================================================

update_closest_empty_current_test() ->
    Target = <<100:256>>,
    NewNodes = sample_nodes([1, 2, 3]),
    Result = macula_routing_dht:update_closest([], NewNodes, Target, 5),
    ?assertEqual(3, length(Result)).

update_closest_empty_new_test() ->
    Target = <<100:256>>,
    CurrentClosest = sample_nodes([1, 2, 3]),
    Result = macula_routing_dht:update_closest(CurrentClosest, [], Target, 5),
    ?assertEqual(3, length(Result)).

update_closest_combines_lists_test() ->
    Target = <<100:256>>,
    CurrentClosest = sample_nodes([1, 2]),
    NewNodes = sample_nodes([3, 4]),
    Result = macula_routing_dht:update_closest(CurrentClosest, NewNodes, Target, 10),
    ?assertEqual(4, length(Result)).

update_closest_removes_duplicates_test() ->
    Target = <<100:256>>,
    CurrentClosest = sample_nodes([1, 2, 3]),
    NewNodes = sample_nodes([2, 3, 4]),
    Result = macula_routing_dht:update_closest(CurrentClosest, NewNodes, Target, 10),
    %% Should have 4 unique nodes (1, 2, 3, 4)
    ?assertEqual(4, length(Result)).

update_closest_limits_to_k_test() ->
    Target = <<100:256>>,
    CurrentClosest = sample_nodes([1, 2, 3, 4, 5]),
    NewNodes = sample_nodes([6, 7, 8, 9, 10]),
    Result = macula_routing_dht:update_closest(CurrentClosest, NewNodes, Target, 3),
    %% Should only return 3 closest
    ?assertEqual(3, length(Result)).

update_closest_sorted_by_distance_test() ->
    %% Create target and nodes with known distances
    Target = <<100:256>>,

    %% Add nodes with IDs that will have different distances
    CurrentClosest = sample_nodes([50, 200]),
    NewNodes = sample_nodes([99, 150]),

    Result = macula_routing_dht:update_closest(CurrentClosest, NewNodes, Target, 4),

    %% Verify sorted by XOR distance to target (99 should be closest to 100)
    ?assertEqual(4, length(Result)),

    %% Calculate distances for verification
    [First, Second, Third, Fourth] = Result,
    Dist1 = macula_routing_nodeid:distance(Target, maps:get(node_id, First)),
    Dist2 = macula_routing_nodeid:distance(Target, maps:get(node_id, Second)),
    Dist3 = macula_routing_nodeid:distance(Target, maps:get(node_id, Third)),
    Dist4 = macula_routing_nodeid:distance(Target, maps:get(node_id, Fourth)),

    ?assert(Dist1 =< Dist2),
    ?assert(Dist2 =< Dist3),
    ?assert(Dist3 =< Dist4).

update_closest_k_equals_1_test() ->
    Target = <<100:256>>,
    CurrentClosest = sample_nodes([1, 2]),
    NewNodes = sample_nodes([3, 4]),
    Result = macula_routing_dht:update_closest(CurrentClosest, NewNodes, Target, 1),
    ?assertEqual(1, length(Result)).

%%%===================================================================
%%% select_alpha/3 Tests
%%%===================================================================

select_alpha_empty_closest_test() ->
    Result = macula_routing_dht:select_alpha([], [], 3),
    ?assertEqual([], Result).

select_alpha_all_queried_test() ->
    Nodes = sample_nodes([1, 2, 3]),
    QueriedIds = [maps:get(node_id, N) || N <- Nodes],
    Result = macula_routing_dht:select_alpha(Nodes, QueriedIds, 3),
    ?assertEqual([], Result).

select_alpha_none_queried_test() ->
    Nodes = sample_nodes([1, 2, 3, 4, 5]),
    Result = macula_routing_dht:select_alpha(Nodes, [], 3),
    ?assertEqual(3, length(Result)).

select_alpha_some_queried_test() ->
    Nodes = sample_nodes([1, 2, 3, 4, 5]),
    Node1Id = maps:get(node_id, sample_node(1)),
    Node2Id = maps:get(node_id, sample_node(2)),
    QueriedIds = [Node1Id, Node2Id],
    Result = macula_routing_dht:select_alpha(Nodes, QueriedIds, 3),
    %% Should return 3 from remaining 3 unqueried (3, 4, 5)
    ?assertEqual(3, length(Result)).

select_alpha_limits_to_alpha_test() ->
    Nodes = sample_nodes([1, 2, 3, 4, 5, 6, 7, 8]),
    Result = macula_routing_dht:select_alpha(Nodes, [], 3),
    ?assertEqual(3, length(Result)).

select_alpha_fewer_than_alpha_available_test() ->
    Nodes = sample_nodes([1, 2]),
    Result = macula_routing_dht:select_alpha(Nodes, [], 5),
    ?assertEqual(2, length(Result)).

select_alpha_preserves_order_test() ->
    Nodes = sample_nodes([1, 2, 3, 4, 5]),
    Result = macula_routing_dht:select_alpha(Nodes, [], 3),

    %% Result should be first 3 nodes (in order)
    ?assertEqual(3, length(Result)),
    [First, Second, Third] = Result,
    ?assertEqual(maps:get(node_id, sample_node(1)), maps:get(node_id, First)),
    ?assertEqual(maps:get(node_id, sample_node(2)), maps:get(node_id, Second)),
    ?assertEqual(maps:get(node_id, sample_node(3)), maps:get(node_id, Third)).

select_alpha_filters_queried_test() ->
    Nodes = sample_nodes([1, 2, 3, 4, 5]),
    Node1Id = maps:get(node_id, sample_node(1)),
    Node3Id = maps:get(node_id, sample_node(3)),
    Node5Id = maps:get(node_id, sample_node(5)),
    QueriedIds = [Node1Id, Node3Id, Node5Id],

    Result = macula_routing_dht:select_alpha(Nodes, QueriedIds, 2),

    %% Should return 2 from unqueried (2, 4)
    ?assertEqual(2, length(Result)),

    %% Verify nodes are 2 and 4
    [First, Second] = Result,
    ?assertEqual(maps:get(node_id, sample_node(2)), maps:get(node_id, First)),
    ?assertEqual(maps:get(node_id, sample_node(4)), maps:get(node_id, Second)).

%%%===================================================================
%%% NOTE: found_closer_nodes/3 is an internal helper function and is
%%% tested indirectly through the higher-level DHT algorithms.
%%%===================================================================

%%%===================================================================
%%% Edge Cases and Property Tests
%%%===================================================================

update_closest_many_nodes_test() ->
    Target = <<100:256>>,
    %% Create 50 nodes
    CurrentClosest = sample_nodes(lists:seq(1, 25)),
    NewNodes = sample_nodes(lists:seq(26, 50)),

    Result = macula_routing_dht:update_closest(CurrentClosest, NewNodes, Target, 20),
    ?assertEqual(20, length(Result)).

select_alpha_with_alpha_zero_test() ->
    Nodes = sample_nodes([1, 2, 3]),
    %% Alpha = 0 should return empty list
    Result = macula_routing_dht:select_alpha(Nodes, [], 0),
    ?assertEqual([], Result).

update_closest_with_k_zero_test() ->
    Target = <<100:256>>,
    CurrentClosest = sample_nodes([1, 2, 3]),
    NewNodes = sample_nodes([4, 5, 6]),
    %% K = 0 should return empty list (though not realistic)
    Result = macula_routing_dht:update_closest(CurrentClosest, NewNodes, Target, 0),
    ?assertEqual([], Result).

%%%===================================================================
%%% Note: Higher-level functions (iterative_find_node, store_value,
%%% find_value) require routing table integration and will be tested
%%% in integration tests or when routing_table module has its tests.
%%%===================================================================
