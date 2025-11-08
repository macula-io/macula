%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_routing_bucket module.
%%% Tests written FIRST (TDD red phase).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_bucket_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Bucket Creation Tests
%%%===================================================================

%% Test: new creates empty bucket with capacity k
new_bucket_is_empty_test() ->
    Bucket = macula_routing_bucket:new(20),
    ?assertEqual(0, macula_routing_bucket:size(Bucket)),
    ?assertEqual(20, macula_routing_bucket:capacity(Bucket)).

%%%===================================================================
%%% Add Node Tests
%%%===================================================================

%% Test: add_node adds node to bucket
add_node_adds_test() ->
    Bucket = macula_routing_bucket:new(20),
    NodeInfo = #{
        node_id => <<1:256>>,
        address => {{127,0,0,1}, 8080}
    },
    Bucket2 = macula_routing_bucket:add_node(Bucket, NodeInfo),
    ?assertEqual(1, macula_routing_bucket:size(Bucket2)).

%% Test: add_node moves existing node to tail (most recent)
add_node_moves_to_tail_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node1 = #{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}},
    Node2 = #{node_id => <<2:256>>, address => {{127,0,0,1}, 8081}},

    Bucket2 = macula_routing_bucket:add_node(Bucket, Node1),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, Node2),

    %% Re-add Node1 (should move to tail)
    Bucket4 = macula_routing_bucket:add_node(Bucket3, Node1),

    %% Size should still be 2
    ?assertEqual(2, macula_routing_bucket:size(Bucket4)),

    %% Node1 should be at tail (most recently seen)
    Nodes = macula_routing_bucket:get_nodes(Bucket4),
    [Head | _] = Nodes,
    NodeId = maps:get(node_id, Head),
    ?assertEqual(<<2:256>>, NodeId).  % Node2 should be head (oldest)

%% Test: add_node rejects when bucket full (no eviction yet)
add_node_rejects_when_full_test() ->
    Bucket = macula_routing_bucket:new(2),  % Small bucket
    Node1 = #{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}},
    Node2 = #{node_id => <<2:256>>, address => {{127,0,0,1}, 8081}},
    Node3 = #{node_id => <<3:256>>, address => {{127,0,0,1}, 8082}},

    Bucket2 = macula_routing_bucket:add_node(Bucket, Node1),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, Node2),

    %% Bucket full, should reject
    ?assertEqual({error, bucket_full}, macula_routing_bucket:add_node(Bucket3, Node3)).

%%%===================================================================
%%% Remove Node Tests
%%%===================================================================

%% Test: remove_node removes node
remove_node_removes_test() ->
    Bucket = macula_routing_bucket:new(20),
    NodeInfo = #{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}},

    Bucket2 = macula_routing_bucket:add_node(Bucket, NodeInfo),
    ?assertEqual(1, macula_routing_bucket:size(Bucket2)),

    Bucket3 = macula_routing_bucket:remove_node(Bucket2, <<1:256>>),
    ?assertEqual(0, macula_routing_bucket:size(Bucket3)).

%% Test: remove_node handles non-existent node
remove_node_handles_not_found_test() ->
    Bucket = macula_routing_bucket:new(20),
    Bucket2 = macula_routing_bucket:remove_node(Bucket, <<1:256>>),
    ?assertEqual(Bucket, Bucket2).  % Unchanged

%%%===================================================================
%%% Get Nodes Tests
%%%===================================================================

%% Test: get_nodes returns all nodes
get_nodes_returns_all_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node1 = #{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}},
    Node2 = #{node_id => <<2:256>>, address => {{127,0,0,1}, 8081}},

    Bucket2 = macula_routing_bucket:add_node(Bucket, Node1),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, Node2),

    Nodes = macula_routing_bucket:get_nodes(Bucket3),
    ?assertEqual(2, length(Nodes)).

%% Test: get_nodes returns nodes in LRU order (oldest first)
get_nodes_lru_order_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node1 = #{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}},
    Node2 = #{node_id => <<2:256>>, address => {{127,0,0,1}, 8081}},
    Node3 = #{node_id => <<3:256>>, address => {{127,0,0,1}, 8082}},

    Bucket2 = macula_routing_bucket:add_node(Bucket, Node1),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, Node2),
    Bucket4 = macula_routing_bucket:add_node(Bucket3, Node3),

    Nodes = macula_routing_bucket:get_nodes(Bucket4),
    [First, Second, Third] = Nodes,

    ?assertEqual(<<1:256>>, maps:get(node_id, First)),   % Oldest
    ?assertEqual(<<2:256>>, maps:get(node_id, Second)),
    ?assertEqual(<<3:256>>, maps:get(node_id, Third)).   % Most recent

%%%===================================================================
%%% Find Node Tests
%%%===================================================================

%% Test: find_node returns node if present
find_node_returns_present_test() ->
    Bucket = macula_routing_bucket:new(20),
    NodeInfo = #{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}},

    Bucket2 = macula_routing_bucket:add_node(Bucket, NodeInfo),
    {ok, Found} = macula_routing_bucket:find_node(Bucket2, <<1:256>>),
    ?assertEqual(<<1:256>>, maps:get(node_id, Found)),
    ?assertEqual({{127,0,0,1}, 8080}, maps:get(address, Found)),
    ?assert(maps:is_key(last_seen, Found)).

%% Test: find_node returns not_found if absent
find_node_returns_not_found_test() ->
    Bucket = macula_routing_bucket:new(20),
    ?assertEqual(not_found, macula_routing_bucket:find_node(Bucket, <<1:256>>)).

%%%===================================================================
%%% Find Closest Tests
%%%===================================================================

%% Test: find_closest returns n closest nodes
find_closest_returns_n_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node1 = #{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}},
    Node2 = #{node_id => <<2:256>>, address => {{127,0,0,1}, 8081}},
    Node3 = #{node_id => <<3:256>>, address => {{127,0,0,1}, 8082}},

    Bucket2 = macula_routing_bucket:add_node(Bucket, Node1),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, Node2),
    Bucket4 = macula_routing_bucket:add_node(Bucket3, Node3),

    Target = <<100:256>>,
    Closest = macula_routing_bucket:find_closest(Bucket4, Target, 2),

    ?assertEqual(2, length(Closest)).

%% Test: find_closest returns nodes sorted by distance
find_closest_sorted_by_distance_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node1 = #{node_id => <<100:256>>, address => {{127,0,0,1}, 8080}},  % Distance 0
    Node2 = #{node_id => <<101:256>>, address => {{127,0,0,1}, 8081}},  % Distance 1
    Node3 = #{node_id => <<110:256>>, address => {{127,0,0,1}, 8082}},  % Distance 10

    Bucket2 = macula_routing_bucket:add_node(Bucket, Node3),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, Node1),
    Bucket4 = macula_routing_bucket:add_node(Bucket3, Node2),

    Target = <<100:256>>,
    [First, Second, Third] = macula_routing_bucket:find_closest(Bucket4, Target, 3),

    ?assertEqual(<<100:256>>, maps:get(node_id, First)),  % Closest (0)
    ?assertEqual(<<101:256>>, maps:get(node_id, Second)), % Next (1)
    ?assertEqual(<<110:256>>, maps:get(node_id, Third)).  % Farthest (10)

%%%===================================================================
%%% Has Node Tests
%%%===================================================================

%% Test: has_node returns true if present
has_node_returns_true_test() ->
    Bucket = macula_routing_bucket:new(20),
    NodeInfo = #{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}},
    Bucket2 = macula_routing_bucket:add_node(Bucket, NodeInfo),
    ?assert(macula_routing_bucket:has_node(Bucket2, <<1:256>>)).

%% Test: has_node returns false if absent
has_node_returns_false_test() ->
    Bucket = macula_routing_bucket:new(20),
    ?assertNot(macula_routing_bucket:has_node(Bucket, <<1:256>>)).

%%%===================================================================
%%% Update Timestamp Tests
%%%===================================================================

%% Test: update_timestamp moves node to tail
update_timestamp_moves_to_tail_test() ->
    Bucket = macula_routing_bucket:new(20),
    Node1 = #{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}},
    Node2 = #{node_id => <<2:256>>, address => {{127,0,0,1}, 8081}},

    Bucket2 = macula_routing_bucket:add_node(Bucket, Node1),
    Bucket3 = macula_routing_bucket:add_node(Bucket2, Node2),

    %% Update Node1 timestamp (should move to tail)
    Bucket4 = macula_routing_bucket:update_timestamp(Bucket3, <<1:256>>),

    Nodes = macula_routing_bucket:get_nodes(Bucket4),
    [Head | _] = Nodes,
    ?assertEqual(<<2:256>>, maps:get(node_id, Head)).  % Node2 now oldest

%% Test: update_timestamp handles non-existent node
update_timestamp_handles_not_found_test() ->
    Bucket = macula_routing_bucket:new(20),
    Bucket2 = macula_routing_bucket:update_timestamp(Bucket, <<1:256>>),
    ?assertEqual(Bucket, Bucket2).  % Unchanged
