%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_routing_bucket module.
%%% Tests K-bucket implementation for Kademlia routing.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_bucket_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% Helper to create test node info
sample_node(Suffix) ->
    NodeId = <<Suffix:256>>,  % Simple sequential IDs for testing
    #{
        node_id => NodeId,
        address => {{127, 0, 0, 1}, 4433 + Suffix}
    }.

sample_node_with_timestamp(Suffix, Timestamp) ->
    NodeId = <<Suffix:256>>,
    #{
        node_id => NodeId,
        address => {{127, 0, 0, 1}, 4433 + Suffix},
        last_seen => Timestamp
    }.

%%%===================================================================
%%% Bucket Creation Tests
%%%===================================================================

new_creates_empty_bucket_test() ->
    Bucket = macula_routing_bucket:new(20),
    ?assertEqual(0, macula_routing_bucket:size(Bucket)),
    ?assertEqual([], macula_routing_bucket:get_nodes(Bucket)).

new_stores_capacity_test() ->
    Bucket = macula_routing_bucket:new(20),
    ?assertEqual(20, macula_routing_bucket:capacity(Bucket)).

new_different_capacity_test() ->
    Bucket = macula_routing_bucket:new(10),
    ?assertEqual(10, macula_routing_bucket:capacity(Bucket)).

%%%===================================================================
%%% Adding Nodes Tests
%%%===================================================================

add_node_to_empty_bucket_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node = sample_node(1),
    Bucket2 = macula_routing_bucket:add_node(Bucket, Node),
    ?assertEqual(1, macula_routing_bucket:size(Bucket2)),
    ?assert(macula_routing_bucket:has_node(Bucket2, maps:get(node_id, Node))).

add_node_adds_timestamp_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node = sample_node(1),  % No timestamp
    Bucket2 = macula_routing_bucket:add_node(Bucket, Node),
    {ok, StoredNode} = macula_routing_bucket:find_node(Bucket2, maps:get(node_id, Node)),
    ?assert(maps:is_key(last_seen, StoredNode)),
    ?assert(is_integer(maps:get(last_seen, StoredNode))).

add_node_preserves_timestamp_test() ->
    Bucket = macula_routing_bucket:new(20),
    Timestamp = 1234567890,
    Node = sample_node_with_timestamp(1, Timestamp),
    Bucket2 = macula_routing_bucket:add_node(Bucket, Node),
    {ok, StoredNode} = macula_routing_bucket:find_node(Bucket2, maps:get(node_id, Node)),
    ?assertEqual(Timestamp, maps:get(last_seen, StoredNode)).

add_multiple_nodes_test() ->
    Bucket = macula_routing_bucket:new(20),
    Bucket2 = macula_routing_bucket:add_node(Bucket, sample_node(1)),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, sample_node(2)),
    Bucket4 = macula_routing_bucket:add_node(Bucket3, sample_node(3)),
    ?assertEqual(3, macula_routing_bucket:size(Bucket4)).

add_node_to_full_bucket_returns_error_test() ->
    %% Create bucket with capacity 2
    Bucket = macula_routing_bucket:new(2),
    Bucket2 = macula_routing_bucket:add_node(Bucket, sample_node(1)),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, sample_node(2)),
    %% Bucket is now full
    Result = macula_routing_bucket:add_node(Bucket3, sample_node(3)),
    ?assertEqual({error, bucket_full}, Result).

add_existing_node_moves_to_tail_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node1 = sample_node(1),
    Node2 = sample_node(2),
    Node3 = sample_node(3),

    %% Add nodes in order: 1, 2, 3
    Bucket2 = macula_routing_bucket:add_node(Bucket, Node1),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, Node2),
    Bucket4 = macula_routing_bucket:add_node(Bucket3, Node3),

    %% Re-add node 1 (should move to tail)
    Bucket5 = macula_routing_bucket:add_node(Bucket4, Node1),

    %% Get nodes (ordered oldest to newest)
    Nodes = macula_routing_bucket:get_nodes(Bucket5),
    ?assertEqual(3, length(Nodes)),

    %% Node 1 should now be at the tail (last element)
    LastNode = lists:last(Nodes),
    ?assertEqual(maps:get(node_id, Node1), maps:get(node_id, LastNode)).

add_existing_node_updates_timestamp_test() ->
    Bucket = macula_routing_bucket:new(20),
    OldTimestamp = 1000,
    Node = sample_node_with_timestamp(1, OldTimestamp),

    %% Add node with old timestamp
    Bucket2 = macula_routing_bucket:add_node(Bucket, Node),

    %% Wait a moment and re-add (should get new timestamp)
    timer:sleep(10),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, Node),

    {ok, UpdatedNode} = macula_routing_bucket:find_node(Bucket3, maps:get(node_id, Node)),
    %% Timestamp should still be the original (preserved from input)
    ?assertEqual(OldTimestamp, maps:get(last_seen, UpdatedNode)).

%%%===================================================================
%%% Removing Nodes Tests
%%%===================================================================

remove_existing_node_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node = sample_node(1),
    NodeId = maps:get(node_id, Node),

    Bucket2 = macula_routing_bucket:add_node(Bucket, Node),
    ?assertEqual(1, macula_routing_bucket:size(Bucket2)),

    Bucket3 = macula_routing_bucket:remove_node(Bucket2, NodeId),
    ?assertEqual(0, macula_routing_bucket:size(Bucket3)),
    ?assertNot(macula_routing_bucket:has_node(Bucket3, NodeId)).

remove_non_existent_node_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node = sample_node(1),
    Bucket2 = macula_routing_bucket:add_node(Bucket, Node),

    %% Try to remove node that doesn't exist
    NonExistentId = <<999:256>>,
    Bucket3 = macula_routing_bucket:remove_node(Bucket2, NonExistentId),

    %% Bucket should be unchanged
    ?assertEqual(1, macula_routing_bucket:size(Bucket3)).

remove_from_empty_bucket_test() ->
    Bucket = macula_routing_bucket:new(20),
    NodeId = <<1:256>>,
    Bucket2 = macula_routing_bucket:remove_node(Bucket, NodeId),
    ?assertEqual(0, macula_routing_bucket:size(Bucket2)).

remove_middle_node_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node1 = sample_node(1),
    Node2 = sample_node(2),
    Node3 = sample_node(3),

    Bucket2 = macula_routing_bucket:add_node(Bucket, Node1),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, Node2),
    Bucket4 = macula_routing_bucket:add_node(Bucket3, Node3),

    %% Remove middle node
    Bucket5 = macula_routing_bucket:remove_node(Bucket4, maps:get(node_id, Node2)),

    ?assertEqual(2, macula_routing_bucket:size(Bucket5)),
    ?assertNot(macula_routing_bucket:has_node(Bucket5, maps:get(node_id, Node2))),
    ?assert(macula_routing_bucket:has_node(Bucket5, maps:get(node_id, Node1))),
    ?assert(macula_routing_bucket:has_node(Bucket5, maps:get(node_id, Node3))).

%%%===================================================================
%%% Finding Nodes Tests
%%%===================================================================

find_existing_node_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node = sample_node(1),
    NodeId = maps:get(node_id, Node),

    Bucket2 = macula_routing_bucket:add_node(Bucket, Node),
    Result = macula_routing_bucket:find_node(Bucket2, NodeId),

    ?assertMatch({ok, _}, Result),
    {ok, FoundNode} = Result,
    ?assertEqual(NodeId, maps:get(node_id, FoundNode)).

find_non_existent_node_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node = sample_node(1),
    Bucket2 = macula_routing_bucket:add_node(Bucket, Node),

    NonExistentId = <<999:256>>,
    Result = macula_routing_bucket:find_node(Bucket2, NonExistentId),
    ?assertEqual(not_found, Result).

find_in_empty_bucket_test() ->
    Bucket = macula_routing_bucket:new(20),
    NodeId = <<1:256>>,
    Result = macula_routing_bucket:find_node(Bucket, NodeId),
    ?assertEqual(not_found, Result).

has_node_existing_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node = sample_node(1),
    NodeId = maps:get(node_id, Node),

    Bucket2 = macula_routing_bucket:add_node(Bucket, Node),
    ?assert(macula_routing_bucket:has_node(Bucket2, NodeId)).

has_node_non_existent_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node = sample_node(1),
    Bucket2 = macula_routing_bucket:add_node(Bucket, Node),

    NonExistentId = <<999:256>>,
    ?assertNot(macula_routing_bucket:has_node(Bucket2, NonExistentId)).

%%%===================================================================
%%% Finding Closest Nodes Tests
%%%===================================================================

find_closest_with_fewer_than_n_nodes_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node1 = sample_node(1),
    Node2 = sample_node(2),

    Bucket2 = macula_routing_bucket:add_node(Bucket, Node1),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, Node2),

    Target = <<100:256>>,
    Closest = macula_routing_bucket:find_closest(Bucket3, Target, 5),

    %% Should return all 2 nodes since we only have 2
    ?assertEqual(2, length(Closest)).

find_closest_with_more_than_n_nodes_test() ->
    Bucket = macula_routing_bucket:new(20),

    %% Add 5 nodes
    Bucket2 = macula_routing_bucket:add_node(Bucket, sample_node(1)),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, sample_node(2)),
    Bucket4 = macula_routing_bucket:add_node(Bucket3, sample_node(3)),
    Bucket5 = macula_routing_bucket:add_node(Bucket4, sample_node(4)),
    Bucket6 = macula_routing_bucket:add_node(Bucket5, sample_node(5)),

    Target = <<100:256>>,
    Closest = macula_routing_bucket:find_closest(Bucket6, Target, 3),

    %% Should return exactly 3 nodes
    ?assertEqual(3, length(Closest)).

find_closest_sorted_by_distance_test() ->
    Bucket = macula_routing_bucket:new(20),

    %% Create target and nodes with known distances
    Target = <<100:256>>,

    %% Add nodes with different IDs
    Bucket2 = macula_routing_bucket:add_node(Bucket, sample_node(1)),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, sample_node(50)),
    Bucket4 = macula_routing_bucket:add_node(Bucket3, sample_node(200)),

    Closest = macula_routing_bucket:find_closest(Bucket4, Target, 3),

    %% Verify they're sorted by distance (closest first)
    ?assertEqual(3, length(Closest)),

    %% Calculate distances to verify order
    [First, Second, Third] = Closest,
    Dist1 = macula_routing_nodeid:distance(Target, maps:get(node_id, First)),
    Dist2 = macula_routing_nodeid:distance(Target, maps:get(node_id, Second)),
    Dist3 = macula_routing_nodeid:distance(Target, maps:get(node_id, Third)),

    %% Distances should be in ascending order
    ?assert(Dist1 =< Dist2),
    ?assert(Dist2 =< Dist3).

find_closest_to_exact_match_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node = sample_node(100),
    NodeId = maps:get(node_id, Node),

    Bucket2 = macula_routing_bucket:add_node(Bucket, Node),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, sample_node(50)),
    Bucket4 = macula_routing_bucket:add_node(Bucket3, sample_node(200)),

    %% Find closest to node that's in the bucket
    Closest = macula_routing_bucket:find_closest(Bucket4, NodeId, 1),

    ?assertEqual(1, length(Closest)),
    [ClosestNode] = Closest,
    ?assertEqual(NodeId, maps:get(node_id, ClosestNode)).

find_closest_in_empty_bucket_test() ->
    Bucket = macula_routing_bucket:new(20),
    Target = <<100:256>>,
    Closest = macula_routing_bucket:find_closest(Bucket, Target, 5),
    ?assertEqual([], Closest).

%%%===================================================================
%%% Timestamp Update Tests
%%%===================================================================

update_timestamp_existing_node_test() ->
    Bucket = macula_routing_bucket:new(20),
    OldTimestamp = 1000,
    Node = sample_node_with_timestamp(1, OldTimestamp),
    NodeId = maps:get(node_id, Node),

    Bucket2 = macula_routing_bucket:add_node(Bucket, Node),

    %% Wait a bit and update timestamp
    timer:sleep(10),
    Bucket3 = macula_routing_bucket:update_timestamp(Bucket2, NodeId),

    {ok, UpdatedNode} = macula_routing_bucket:find_node(Bucket3, NodeId),
    UpdatedTimestamp = maps:get(last_seen, UpdatedNode),

    %% New timestamp should be greater than old
    ?assert(UpdatedTimestamp > OldTimestamp).

update_timestamp_moves_to_tail_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node1 = sample_node(1),
    Node2 = sample_node(2),
    Node3 = sample_node(3),

    %% Add nodes in order: 1, 2, 3
    Bucket2 = macula_routing_bucket:add_node(Bucket, Node1),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, Node2),
    Bucket4 = macula_routing_bucket:add_node(Bucket3, Node3),

    %% Update timestamp of node 1 (should move to tail)
    Bucket5 = macula_routing_bucket:update_timestamp(Bucket4, maps:get(node_id, Node1)),

    Nodes = macula_routing_bucket:get_nodes(Bucket5),
    LastNode = lists:last(Nodes),

    %% Node 1 should be at tail
    ?assertEqual(maps:get(node_id, Node1), maps:get(node_id, LastNode)).

update_timestamp_non_existent_node_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node = sample_node(1),
    Bucket2 = macula_routing_bucket:add_node(Bucket, Node),

    %% Update timestamp of non-existent node
    NonExistentId = <<999:256>>,
    Bucket3 = macula_routing_bucket:update_timestamp(Bucket2, NonExistentId),

    %% Bucket should be unchanged
    ?assertEqual(macula_routing_bucket:size(Bucket2), macula_routing_bucket:size(Bucket3)).

%%%===================================================================
%%% Bucket Properties Tests
%%%===================================================================

size_returns_correct_count_test() ->
    Bucket = macula_routing_bucket:new(20),
    ?assertEqual(0, macula_routing_bucket:size(Bucket)),

    Bucket2 = macula_routing_bucket:add_node(Bucket, sample_node(1)),
    ?assertEqual(1, macula_routing_bucket:size(Bucket2)),

    Bucket3 = macula_routing_bucket:add_node(Bucket2, sample_node(2)),
    ?assertEqual(2, macula_routing_bucket:size(Bucket3)),

    Bucket4 = macula_routing_bucket:remove_node(Bucket3, maps:get(node_id, sample_node(1))),
    ?assertEqual(1, macula_routing_bucket:size(Bucket4)).

get_nodes_returns_correct_order_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node1 = sample_node(1),
    Node2 = sample_node(2),
    Node3 = sample_node(3),

    %% Add in order: 1, 2, 3
    Bucket2 = macula_routing_bucket:add_node(Bucket, Node1),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, Node2),
    Bucket4 = macula_routing_bucket:add_node(Bucket3, Node3),

    Nodes = macula_routing_bucket:get_nodes(Bucket4),

    %% Should be ordered: oldest (1) to newest (3)
    ?assertEqual(3, length(Nodes)),
    [First, Second, Third] = Nodes,
    ?assertEqual(maps:get(node_id, Node1), maps:get(node_id, First)),
    ?assertEqual(maps:get(node_id, Node2), maps:get(node_id, Second)),
    ?assertEqual(maps:get(node_id, Node3), maps:get(node_id, Third)).

%%%===================================================================
%%% LRU Behavior Tests
%%%===================================================================

lru_order_maintained_test() ->
    Bucket = macula_routing_bucket:new(20),

    %% Add nodes 1, 2, 3
    Bucket2 = macula_routing_bucket:add_node(Bucket, sample_node(1)),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, sample_node(2)),
    Bucket4 = macula_routing_bucket:add_node(Bucket3, sample_node(3)),

    %% Touch node 1 (should move to tail)
    Bucket5 = macula_routing_bucket:add_node(Bucket4, sample_node(1)),

    %% Order should now be: 2, 3, 1
    Nodes = macula_routing_bucket:get_nodes(Bucket5),
    [First, Second, Third] = Nodes,

    ?assertEqual(maps:get(node_id, sample_node(2)), maps:get(node_id, First)),
    ?assertEqual(maps:get(node_id, sample_node(3)), maps:get(node_id, Second)),
    ?assertEqual(maps:get(node_id, sample_node(1)), maps:get(node_id, Third)).

lru_oldest_at_head_test() ->
    Bucket = macula_routing_bucket:new(20),

    %% Add multiple nodes
    Bucket2 = macula_routing_bucket:add_node(Bucket, sample_node(1)),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, sample_node(2)),
    Bucket4 = macula_routing_bucket:add_node(Bucket3, sample_node(3)),

    Nodes = macula_routing_bucket:get_nodes(Bucket4),
    OldestNode = hd(Nodes),

    %% Oldest should be node 1 (added first)
    ?assertEqual(maps:get(node_id, sample_node(1)), maps:get(node_id, OldestNode)).

lru_newest_at_tail_test() ->
    Bucket = macula_routing_bucket:new(20),

    %% Add multiple nodes
    Bucket2 = macula_routing_bucket:add_node(Bucket, sample_node(1)),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, sample_node(2)),
    Bucket4 = macula_routing_bucket:add_node(Bucket3, sample_node(3)),

    Nodes = macula_routing_bucket:get_nodes(Bucket4),
    NewestNode = lists:last(Nodes),

    %% Newest should be node 3 (added last)
    ?assertEqual(maps:get(node_id, sample_node(3)), maps:get(node_id, NewestNode)).

%%%===================================================================
%%% Edge Cases
%%%===================================================================

capacity_one_bucket_test() ->
    Bucket = macula_routing_bucket:new(1),
    Node1 = sample_node(1),
    Node2 = sample_node(2),

    Bucket2 = macula_routing_bucket:add_node(Bucket, Node1),
    ?assertEqual(1, macula_routing_bucket:size(Bucket2)),

    %% Adding second node should fail
    Result = macula_routing_bucket:add_node(Bucket2, Node2),
    ?assertEqual({error, bucket_full}, Result).

add_same_node_twice_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node = sample_node(1),

    Bucket2 = macula_routing_bucket:add_node(Bucket, Node),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, Node),

    %% Should still have only 1 node
    ?assertEqual(1, macula_routing_bucket:size(Bucket3)).

large_bucket_test() ->
    Bucket = macula_routing_bucket:new(100),

    %% Add 50 nodes
    BucketFilled = lists:foldl(
        fun(N, B) ->
            macula_routing_bucket:add_node(B, sample_node(N))
        end,
        Bucket,
        lists:seq(1, 50)
    ),

    ?assertEqual(50, macula_routing_bucket:size(BucketFilled)),
    ?assertEqual(100, macula_routing_bucket:capacity(BucketFilled)).
