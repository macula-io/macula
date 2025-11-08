%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_routing_table module.
%%% Tests written FIRST (TDD red phase).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_table_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Table Creation Tests
%%%===================================================================

%% Test: new creates empty routing table
new_table_is_empty_test() ->
    LocalNodeId = <<1:256>>,
    Table = macula_routing_table:new(LocalNodeId, 20),
    ?assertEqual(0, macula_routing_table:size(Table)).

%% Test: new stores local node ID and k
new_table_stores_config_test() ->
    LocalNodeId = <<1:256>>,
    K = 20,
    Table = macula_routing_table:new(LocalNodeId, K),
    ?assertEqual(LocalNodeId, macula_routing_table:local_node_id(Table)),
    ?assertEqual(K, macula_routing_table:k(Table)).

%%%===================================================================
%%% Add Node Tests
%%%===================================================================

%% Test: add_node adds node to correct bucket
add_node_adds_to_bucket_test() ->
    LocalNodeId = <<0:256>>,
    Table = macula_routing_table:new(LocalNodeId, 20),

    %% Node with 10 leading zeros in XOR distance
    NodeId = <<0:10, 1:1, 0:245>>,
    NodeInfo = #{node_id => NodeId, address => {{127,0,0,1}, 8080}},

    Table2 = macula_routing_table:add_node(Table, NodeInfo),
    ?assertEqual(1, macula_routing_table:size(Table2)).

%% Test: add_node calculates correct bucket index
add_node_correct_bucket_index_test() ->
    LocalNodeId = <<0:256>>,
    Table = macula_routing_table:new(LocalNodeId, 20),

    %% Node with distance having 5 leading zeros
    NodeId = <<0:5, 1:1, 0:250>>,
    NodeInfo = #{node_id => NodeId, address => {{127,0,0,1}, 8080}},

    Table2 = macula_routing_table:add_node(Table, NodeInfo),

    %% Check bucket 5 has the node
    ?assertEqual(1, macula_routing_table:bucket_size(Table2, 5)).

%% Test: add_node multiple nodes to different buckets
add_node_multiple_buckets_test() ->
    LocalNodeId = <<0:256>>,
    Table = macula_routing_table:new(LocalNodeId, 20),

    Node1 = #{node_id => <<0:5, 1:1, 0:250>>, address => {{127,0,0,1}, 8080}},   % Bucket 5
    Node2 = #{node_id => <<0:10, 1:1, 0:245>>, address => {{127,0,0,1}, 8081}},  % Bucket 10
    Node3 = #{node_id => <<0:15, 1:1, 0:240>>, address => {{127,0,0,1}, 8082}},  % Bucket 15

    Table2 = macula_routing_table:add_node(Table, Node1),
    Table3 = macula_routing_table:add_node(Table2, Node2),
    Table4 = macula_routing_table:add_node(Table3, Node3),

    ?assertEqual(3, macula_routing_table:size(Table4)),
    ?assertEqual(1, macula_routing_table:bucket_size(Table4, 5)),
    ?assertEqual(1, macula_routing_table:bucket_size(Table4, 10)),
    ?assertEqual(1, macula_routing_table:bucket_size(Table4, 15)).

%%%===================================================================
%%% Remove Node Tests
%%%===================================================================

%% Test: remove_node removes node from table
remove_node_removes_test() ->
    LocalNodeId = <<0:256>>,
    Table = macula_routing_table:new(LocalNodeId, 20),

    NodeId = <<0:10, 1:1, 0:245>>,
    NodeInfo = #{node_id => NodeId, address => {{127,0,0,1}, 8080}},

    Table2 = macula_routing_table:add_node(Table, NodeInfo),
    ?assertEqual(1, macula_routing_table:size(Table2)),

    Table3 = macula_routing_table:remove_node(Table2, NodeId),
    ?assertEqual(0, macula_routing_table:size(Table3)).

%%%===================================================================
%%% Find Closest Tests
%%%===================================================================

%% Test: find_closest returns k closest nodes
find_closest_returns_k_test() ->
    LocalNodeId = <<0:256>>,
    Table = macula_routing_table:new(LocalNodeId, 20),

    %% Add nodes at various distances
    Nodes = [
        #{node_id => <<100:256>>, address => {{127,0,0,1}, 8080}},
        #{node_id => <<101:256>>, address => {{127,0,0,1}, 8081}},
        #{node_id => <<110:256>>, address => {{127,0,0,1}, 8082}},
        #{node_id => <<120:256>>, address => {{127,0,0,1}, 8083}},
        #{node_id => <<150:256>>, address => {{127,0,0,1}, 8084}}
    ],

    Table2 = lists:foldl(
        fun(Node, Acc) -> macula_routing_table:add_node(Acc, Node) end,
        Table,
        Nodes
    ),

    Target = <<100:256>>,
    Closest = macula_routing_table:find_closest(Table2, Target, 3),

    ?assertEqual(3, length(Closest)).

%% Test: find_closest returns nodes sorted by distance
find_closest_sorted_test() ->
    LocalNodeId = <<0:256>>,
    Table = macula_routing_table:new(LocalNodeId, 20),

    Nodes = [
        #{node_id => <<110:256>>, address => {{127,0,0,1}, 8080}},  % Distance 10
        #{node_id => <<100:256>>, address => {{127,0,0,1}, 8081}},  % Distance 0
        #{node_id => <<101:256>>, address => {{127,0,0,1}, 8082}}   % Distance 1
    ],

    Table2 = lists:foldl(
        fun(Node, Acc) -> macula_routing_table:add_node(Acc, Node) end,
        Table,
        Nodes
    ),

    Target = <<100:256>>,
    [First, Second, Third] = macula_routing_table:find_closest(Table2, Target, 3),

    ?assertEqual(<<100:256>>, maps:get(node_id, First)),   % Closest
    ?assertEqual(<<101:256>>, maps:get(node_id, Second)),  % Next
    ?assertEqual(<<110:256>>, maps:get(node_id, Third)).   % Farthest

%%%===================================================================
%%% Get Bucket Tests
%%%===================================================================

%% Test: get_bucket returns bucket
get_bucket_returns_test() ->
    LocalNodeId = <<0:256>>,
    Table = macula_routing_table:new(LocalNodeId, 20),

    NodeId = <<0:10, 1:1, 0:245>>,
    NodeInfo = #{node_id => NodeId, address => {{127,0,0,1}, 8080}},

    Table2 = macula_routing_table:add_node(Table, NodeInfo),
    Bucket = macula_routing_table:get_bucket(Table2, 10),

    ?assertEqual(1, macula_routing_bucket:size(Bucket)).

%% Test: get_bucket creates empty bucket if not exists
get_bucket_creates_if_not_exists_test() ->
    LocalNodeId = <<0:256>>,
    Table = macula_routing_table:new(LocalNodeId, 20),

    Bucket = macula_routing_table:get_bucket(Table, 5),
    ?assertEqual(0, macula_routing_bucket:size(Bucket)).

%%%===================================================================
%%% Bucket Size Tests
%%%===================================================================

%% Test: bucket_size returns 0 for empty bucket
bucket_size_empty_test() ->
    LocalNodeId = <<0:256>>,
    Table = macula_routing_table:new(LocalNodeId, 20),
    ?assertEqual(0, macula_routing_table:bucket_size(Table, 5)).

%% Test: bucket_size returns correct count
bucket_size_correct_test() ->
    LocalNodeId = <<0:256>>,
    Table = macula_routing_table:new(LocalNodeId, 20),

    %% Both nodes have 10 leading zeros (different IDs, same bucket)
    Node1 = #{node_id => <<0:10, 1:1, 1:1, 0:244>>, address => {{127,0,0,1}, 8080}},
    Node2 = #{node_id => <<0:10, 1:1, 0:1, 0:244>>, address => {{127,0,0,1}, 8081}},

    Table2 = macula_routing_table:add_node(Table, Node1),
    Table3 = macula_routing_table:add_node(Table2, Node2),

    ?assertEqual(2, macula_routing_table:bucket_size(Table3, 10)).

%%%===================================================================
%%% Get All Nodes Tests
%%%===================================================================

%% Test: get_all_nodes returns all nodes from all buckets
get_all_nodes_test() ->
    LocalNodeId = <<0:256>>,
    Table = macula_routing_table:new(LocalNodeId, 20),

    Nodes = [
        #{node_id => <<0:5, 1:1, 0:250>>, address => {{127,0,0,1}, 8080}},   % Bucket 5
        #{node_id => <<0:10, 1:1, 0:245>>, address => {{127,0,0,1}, 8081}},  % Bucket 10
        #{node_id => <<0:15, 1:1, 0:240>>, address => {{127,0,0,1}, 8082}}   % Bucket 15
    ],

    Table2 = lists:foldl(
        fun(Node, Acc) -> macula_routing_table:add_node(Acc, Node) end,
        Table,
        Nodes
    ),

    AllNodes = macula_routing_table:get_all_nodes(Table2),
    ?assertEqual(3, length(AllNodes)).

%%%===================================================================
%%% Update Timestamp Tests
%%%===================================================================

%% Test: update_timestamp updates node in correct bucket
update_timestamp_test() ->
    LocalNodeId = <<0:256>>,
    Table = macula_routing_table:new(LocalNodeId, 20),

    NodeId = <<0:10, 1:1, 0:245>>,
    NodeInfo = #{node_id => NodeId, address => {{127,0,0,1}, 8080}},

    Table2 = macula_routing_table:add_node(Table, NodeInfo),
    Table3 = macula_routing_table:update_timestamp(Table2, NodeId),

    %% Verify node still exists (timestamp updated internally)
    ?assertEqual(1, macula_routing_table:size(Table3)).
