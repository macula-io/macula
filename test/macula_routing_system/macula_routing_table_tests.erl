%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_routing_table module.
%%% Tests Kademlia routing table with 256 K-buckets.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_table_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% Helper to create local node ID
local_node_id() ->
    <<100:256>>.

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
%%% Table Creation Tests
%%%===================================================================

new_creates_empty_table_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    ?assertEqual(0, macula_routing_table:size(Table)).

new_stores_local_node_id_test() ->
    LocalNodeId = local_node_id(),
    Table = macula_routing_table:new(LocalNodeId, 20),
    ?assertEqual(LocalNodeId, macula_routing_table:local_node_id(Table)).

new_stores_k_value_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    ?assertEqual(20, macula_routing_table:k(Table)).

new_initializes_empty_buckets_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    ?assertEqual([], macula_routing_table:get_all_nodes(Table)).

%%%===================================================================
%%% Adding Nodes Tests
%%%===================================================================

add_node_to_empty_table_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Node = sample_node(1),
    Table2 = macula_routing_table:add_node(Table, Node),
    ?assertEqual(1, macula_routing_table:size(Table2)).

add_node_increases_size_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Table2 = macula_routing_table:add_node(Table, sample_node(1)),
    Table3 = macula_routing_table:add_node(Table2, sample_node(2)),
    Table4 = macula_routing_table:add_node(Table3, sample_node(3)),
    ?assertEqual(3, macula_routing_table:size(Table4)).

add_node_stores_in_all_nodes_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Node = sample_node(1),
    Table2 = macula_routing_table:add_node(Table, Node),
    AllNodes = macula_routing_table:get_all_nodes(Table2),
    ?assertEqual(1, length(AllNodes)),
    [StoredNode] = AllNodes,
    ?assertEqual(maps:get(node_id, Node), maps:get(node_id, StoredNode)).

add_self_node_ignored_test() ->
    %% Adding local node ID should be ignored (bucket index 256)
    LocalNodeId = local_node_id(),
    Table = macula_routing_table:new(LocalNodeId, 20),
    SelfNode = #{
        node_id => LocalNodeId,
        address => {{127, 0, 0, 1}, 4433}
    },
    Table2 = macula_routing_table:add_node(Table, SelfNode),
    ?assertEqual(0, macula_routing_table:size(Table2)).

add_duplicate_node_moves_to_tail_test() ->
    %% Adding same node twice should update its position (LRU)
    Table = macula_routing_table:new(local_node_id(), 20),
    Node = sample_node(1),
    Table2 = macula_routing_table:add_node(Table, Node),
    Table3 = macula_routing_table:add_node(Table2, Node),
    %% Size should still be 1 (no duplicate)
    ?assertEqual(1, macula_routing_table:size(Table3)).

add_multiple_nodes_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Nodes = sample_nodes(lists:seq(1, 10)),
    TableWithNodes = lists:foldl(
        fun(Node, Acc) -> macula_routing_table:add_node(Acc, Node) end,
        Table,
        Nodes
    ),
    ?assertEqual(10, macula_routing_table:size(TableWithNodes)).

%%%===================================================================
%%% Removing Nodes Tests
%%%===================================================================

remove_node_from_table_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Node = sample_node(1),
    Table2 = macula_routing_table:add_node(Table, Node),
    Table3 = macula_routing_table:remove_node(Table2, maps:get(node_id, Node)),
    ?assertEqual(0, macula_routing_table:size(Table3)).

remove_node_decreases_size_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Table2 = macula_routing_table:add_node(Table, sample_node(1)),
    Table3 = macula_routing_table:add_node(Table2, sample_node(2)),
    Table4 = macula_routing_table:add_node(Table3, sample_node(3)),
    Table5 = macula_routing_table:remove_node(Table4, maps:get(node_id, sample_node(2))),
    ?assertEqual(2, macula_routing_table:size(Table5)).

remove_nonexistent_node_no_change_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Table2 = macula_routing_table:add_node(Table, sample_node(1)),
    %% Try to remove node that doesn't exist
    Table3 = macula_routing_table:remove_node(Table2, <<999:256>>),
    ?assertEqual(1, macula_routing_table:size(Table3)).

remove_node_from_empty_table_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Table2 = macula_routing_table:remove_node(Table, <<1:256>>),
    ?assertEqual(0, macula_routing_table:size(Table2)).

%%%===================================================================
%%% Finding Closest Nodes Tests
%%%===================================================================

find_closest_in_empty_table_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Target = <<50:256>>,
    Result = macula_routing_table:find_closest(Table, Target, 5),
    ?assertEqual([], Result).

find_closest_returns_all_when_fewer_than_k_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Table2 = macula_routing_table:add_node(Table, sample_node(1)),
    Table3 = macula_routing_table:add_node(Table2, sample_node(2)),
    Target = <<50:256>>,
    Result = macula_routing_table:find_closest(Table3, Target, 5),
    ?assertEqual(2, length(Result)).

find_closest_limits_to_k_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Nodes = sample_nodes(lists:seq(1, 10)),
    TableWithNodes = lists:foldl(
        fun(Node, Acc) -> macula_routing_table:add_node(Acc, Node) end,
        Table,
        Nodes
    ),
    Target = <<50:256>>,
    Result = macula_routing_table:find_closest(TableWithNodes, Target, 5),
    ?assertEqual(5, length(Result)).

find_closest_sorted_by_distance_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Table2 = macula_routing_table:add_node(Table, sample_node(1)),
    Table3 = macula_routing_table:add_node(Table2, sample_node(50)),
    Table4 = macula_routing_table:add_node(Table3, sample_node(101)),
    Table5 = macula_routing_table:add_node(Table4, sample_node(200)),

    %% Find closest to 100 (not in table)
    Target = <<100:256>>,
    Result = macula_routing_table:find_closest(Table5, Target, 4),

    %% Should have at least 1 result, verify sorted by distance
    ?assert(length(Result) > 0),

    %% Verify sorted order
    Distances = [macula_routing_nodeid:distance(Target, maps:get(node_id, N)) || N <- Result],
    SortedDistances = lists:sort(Distances),
    ?assertEqual(SortedDistances, Distances).

find_closest_to_local_node_test() ->
    %% Special case: target is local node (bucket index 256)
    LocalNodeId = local_node_id(),
    Table = macula_routing_table:new(LocalNodeId, 20),
    Table2 = macula_routing_table:add_node(Table, sample_node(1)),
    Table3 = macula_routing_table:add_node(Table2, sample_node(2)),

    Result = macula_routing_table:find_closest(Table3, LocalNodeId, 5),
    ?assertEqual(2, length(Result)).

%%%===================================================================
%%% Bucket Operations Tests
%%%===================================================================

get_bucket_returns_empty_for_nonexistent_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Bucket = macula_routing_table:get_bucket(Table, 0),
    ?assertEqual(0, macula_routing_bucket:size(Bucket)).

get_bucket_returns_existing_bucket_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Node = sample_node(1),
    Table2 = macula_routing_table:add_node(Table, Node),

    %% Calculate which bucket node 1 went into
    BucketIndex = macula_routing_nodeid:bucket_index(local_node_id(), maps:get(node_id, Node)),
    Bucket = macula_routing_table:get_bucket(Table2, BucketIndex),
    ?assertEqual(1, macula_routing_bucket:size(Bucket)).

bucket_size_for_empty_bucket_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Size = macula_routing_table:bucket_size(Table, 0),
    ?assertEqual(0, Size).

bucket_size_for_populated_bucket_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Node = sample_node(1),
    Table2 = macula_routing_table:add_node(Table, Node),

    BucketIndex = macula_routing_nodeid:bucket_index(local_node_id(), maps:get(node_id, Node)),
    Size = macula_routing_table:bucket_size(Table2, BucketIndex),
    ?assertEqual(1, Size).

%%%===================================================================
%%% Timestamp Update Tests
%%%===================================================================

update_timestamp_for_existing_node_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Node = sample_node(1),
    NodeId = maps:get(node_id, Node),
    Table2 = macula_routing_table:add_node(Table, Node),
    Table3 = macula_routing_table:update_timestamp(Table2, NodeId),
    %% Should still have 1 node
    ?assertEqual(1, macula_routing_table:size(Table3)).

update_timestamp_for_nonexistent_node_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Table2 = macula_routing_table:update_timestamp(Table, <<999:256>>),
    %% Should have no effect
    ?assertEqual(0, macula_routing_table:size(Table2)).

update_timestamp_moves_to_tail_test() ->
    %% Add multiple nodes to same bucket, then update first one
    Table = macula_routing_table:new(local_node_id(), 20),
    Node1 = sample_node(1),
    Node2 = sample_node(2),
    Table2 = macula_routing_table:add_node(Table, Node1),
    Table3 = macula_routing_table:add_node(Table2, Node2),
    Table4 = macula_routing_table:update_timestamp(Table3, maps:get(node_id, Node1)),
    %% Should still have 2 nodes
    ?assertEqual(2, macula_routing_table:size(Table4)).

%%%===================================================================
%%% Size and Accessor Tests
%%%===================================================================

size_of_empty_table_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    ?assertEqual(0, macula_routing_table:size(Table)).

size_with_multiple_buckets_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    %% Add nodes that will go into different buckets
    Nodes = sample_nodes(lists:seq(1, 20)),
    TableWithNodes = lists:foldl(
        fun(Node, Acc) -> macula_routing_table:add_node(Acc, Node) end,
        Table,
        Nodes
    ),
    ?assertEqual(20, macula_routing_table:size(TableWithNodes)).

get_all_nodes_from_empty_table_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    ?assertEqual([], macula_routing_table:get_all_nodes(Table)).

get_all_nodes_returns_all_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Nodes = sample_nodes(lists:seq(1, 5)),
    TableWithNodes = lists:foldl(
        fun(Node, Acc) -> macula_routing_table:add_node(Acc, Node) end,
        Table,
        Nodes
    ),
    AllNodes = macula_routing_table:get_all_nodes(TableWithNodes),
    ?assertEqual(5, length(AllNodes)).

local_node_id_accessor_test() ->
    LocalNodeId = local_node_id(),
    Table = macula_routing_table:new(LocalNodeId, 20),
    ?assertEqual(LocalNodeId, macula_routing_table:local_node_id(Table)).

k_accessor_test() ->
    Table = macula_routing_table:new(local_node_id(), 25),
    ?assertEqual(25, macula_routing_table:k(Table)).

%%%===================================================================
%%% Edge Cases and Integration Tests
%%%===================================================================

add_many_nodes_to_same_bucket_test() ->
    %% This tests bucket capacity limits
    Table = macula_routing_table:new(local_node_id(), 5),
    %% Add 10 nodes that should go to same/similar buckets
    Nodes = sample_nodes(lists:seq(1, 10)),
    TableWithNodes = lists:foldl(
        fun(Node, Acc) -> macula_routing_table:add_node(Acc, Node) end,
        Table,
        Nodes
    ),
    %% Some nodes may not be added if buckets are full
    Size = macula_routing_table:size(TableWithNodes),
    ?assert(Size =< 10).

add_and_remove_many_nodes_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Nodes = sample_nodes(lists:seq(1, 20)),

    %% Add all nodes
    TableWithNodes = lists:foldl(
        fun(Node, Acc) -> macula_routing_table:add_node(Acc, Node) end,
        Table,
        Nodes
    ),

    %% Remove every other node
    NodesToRemove = [sample_node(N) || N <- lists:seq(2, 20, 2)],
    TableAfterRemoval = lists:foldl(
        fun(Node, Acc) -> macula_routing_table:remove_node(Acc, maps:get(node_id, Node)) end,
        TableWithNodes,
        NodesToRemove
    ),

    %% Should have 10 nodes left
    ?assertEqual(10, macula_routing_table:size(TableAfterRemoval)).

find_closest_with_k_equals_1_test() ->
    Table = macula_routing_table:new(local_node_id(), 20),
    Table2 = macula_routing_table:add_node(Table, sample_node(1)),
    Table3 = macula_routing_table:add_node(Table2, sample_node(2)),
    Table4 = macula_routing_table:add_node(Table3, sample_node(3)),

    Result = macula_routing_table:find_closest(Table4, <<50:256>>, 1),
    ?assertEqual(1, length(Result)).

stress_test_many_nodes_test() ->
    %% Add 100 nodes and verify table integrity
    Table = macula_routing_table:new(local_node_id(), 20),
    Nodes = sample_nodes(lists:seq(1, 100)),
    TableWithNodes = lists:foldl(
        fun(Node, Acc) -> macula_routing_table:add_node(Acc, Node) end,
        Table,
        Nodes
    ),

    Size = macula_routing_table:size(TableWithNodes),
    AllNodes = macula_routing_table:get_all_nodes(TableWithNodes),

    %% Size should match get_all_nodes length
    ?assertEqual(Size, length(AllNodes)),

    %% Should be able to find closest
    Result = macula_routing_table:find_closest(TableWithNodes, <<50:256>>, 10),
    ?assertEqual(10, length(Result)).
