%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_routing_server module.
%%% Tests written FIRST (TDD red phase).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_server_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Server Lifecycle Tests
%%%===================================================================

%% Test: start_link starts server
start_link_test() ->
    LocalNodeId = <<1:256>>,
    Config = #{k => 20, alpha => 3},

    {ok, Pid} = macula_routing_server:start_link(LocalNodeId, Config),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),

    %% Cleanup
    gen_server:stop(Pid).

%% Test: stop stops server
stop_test() ->
    LocalNodeId = <<1:256>>,
    {ok, Pid} = macula_routing_server:start_link(LocalNodeId, #{}),

    ok = gen_server:stop(Pid),
    timer:sleep(10),
    ?assertNot(erlang:is_process_alive(Pid)).

%%%===================================================================
%%% Add Node Tests
%%%===================================================================

%% Test: add_node adds node to routing table
add_node_test() ->
    LocalNodeId = <<1:256>>,
    {ok, Pid} = macula_routing_server:start_link(LocalNodeId, #{}),

    NodeInfo = #{node_id => <<100:256>>, address => {{127,0,0,1}, 8080}},
    ok = macula_routing_server:add_node(Pid, NodeInfo),

    %% Verify node was added (check routing table size)
    Size = macula_routing_server:size(Pid),
    ?assertEqual(1, Size),

    gen_server:stop(Pid).

%% Test: add_node multiple nodes
add_multiple_nodes_test() ->
    LocalNodeId = <<1:256>>,
    {ok, Pid} = macula_routing_server:start_link(LocalNodeId, #{}),

    Nodes = [
        #{node_id => <<100:256>>, address => {{127,0,0,1}, 8080}},
        #{node_id => <<101:256>>, address => {{127,0,0,1}, 8081}},
        #{node_id => <<110:256>>, address => {{127,0,0,1}, 8082}}
    ],

    lists:foreach(fun(Node) -> macula_routing_server:add_node(Pid, Node) end, Nodes),

    ?assertEqual(3, macula_routing_server:size(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Find Closest Tests
%%%===================================================================

%% Test: find_closest returns k closest nodes
find_closest_test() ->
    LocalNodeId = <<0:256>>,
    {ok, Pid} = macula_routing_server:start_link(LocalNodeId, #{k => 20}),

    Nodes = [
        #{node_id => <<100:256>>, address => {{127,0,0,1}, 8080}},
        #{node_id => <<101:256>>, address => {{127,0,0,1}, 8081}},
        #{node_id => <<110:256>>, address => {{127,0,0,1}, 8082}}
    ],

    lists:foreach(fun(Node) -> macula_routing_server:add_node(Pid, Node) end, Nodes),

    Target = <<105:256>>,
    Closest = macula_routing_server:find_closest(Pid, Target, 2),

    ?assertEqual(2, length(Closest)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Store/Retrieve Tests
%%%===================================================================

%% Test: store_value stores locally
store_value_local_test() ->
    LocalNodeId = <<1:256>>,
    {ok, Pid} = macula_routing_server:start_link(LocalNodeId, #{}),

    Key = <<100:256>>,
    Value = <<"test_value">>,

    ok = macula_routing_server:store_local(Pid, Key, Value),

    %% Retrieve
    {ok, Retrieved} = macula_routing_server:get_local(Pid, Key),
    ?assertEqual(Value, Retrieved),

    gen_server:stop(Pid).

%% Test: get_local returns not_found for missing key
get_local_not_found_test() ->
    LocalNodeId = <<1:256>>,
    {ok, Pid} = macula_routing_server:start_link(LocalNodeId, #{}),

    Key = <<100:256>>,
    ?assertEqual(not_found, macula_routing_server:get_local(Pid, Key)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Routing Table Query Tests
%%%===================================================================

%% Test: get_routing_table returns routing table
get_routing_table_test() ->
    LocalNodeId = <<1:256>>,
    {ok, Pid} = macula_routing_server:start_link(LocalNodeId, #{}),

    Table = macula_routing_server:get_routing_table(Pid),
    ?assertEqual(LocalNodeId, macula_routing_table:local_node_id(Table)),

    gen_server:stop(Pid).

%% Test: size returns correct count
size_test() ->
    LocalNodeId = <<1:256>>,
    {ok, Pid} = macula_routing_server:start_link(LocalNodeId, #{}),

    ?assertEqual(0, macula_routing_server:size(Pid)),

    Node = #{node_id => <<100:256>>, address => {{127,0,0,1}, 8080}},
    macula_routing_server:add_node(Pid, Node),

    ?assertEqual(1, macula_routing_server:size(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Configuration Tests
%%%===================================================================

%% Test: config stores and retrieves correctly
config_test() ->
    LocalNodeId = <<1:256>>,
    Config = #{k => 30, alpha => 5},
    {ok, Pid} = macula_routing_server:start_link(LocalNodeId, Config),

    Table = macula_routing_server:get_routing_table(Pid),
    ?assertEqual(30, macula_routing_table:k(Table)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Message Handling Tests
%%%===================================================================

%% Test: handle_message with find_node request
handle_find_node_test() ->
    LocalNodeId = <<0:256>>,
    {ok, Pid} = macula_routing_server:start_link(LocalNodeId, #{}),

    %% Add some nodes
    Nodes = [
        #{node_id => <<100:256>>, address => {{127,0,0,1}, 8080}},
        #{node_id => <<101:256>>, address => {{127,0,0,1}, 8081}}
    ],
    lists:foreach(fun(Node) -> macula_routing_server:add_node(Pid, Node) end, Nodes),

    %% Create FIND_NODE message
    Target = <<100:256>>,
    Message = macula_routing_protocol:encode_find_node(Target),

    %% Handle message
    Reply = macula_routing_server:handle_message(Pid, Message),

    %% Should return FIND_NODE_REPLY with closest nodes
    ?assertMatch(#{type := find_node_reply, nodes := _}, Reply),

    gen_server:stop(Pid).

%% Test: handle_message with store request
handle_store_test() ->
    LocalNodeId = <<1:256>>,
    {ok, Pid} = macula_routing_server:start_link(LocalNodeId, #{}),

    Key = <<100:256>>,
    Value = <<"test_value">>,
    Message = macula_routing_protocol:encode_store(Key, Value),

    %% Handle message
    Reply = macula_routing_server:handle_message(Pid, Message),

    %% Should return success
    ?assertEqual(#{type => store_reply, result => ok}, Reply),

    %% Verify stored
    {ok, Retrieved} = macula_routing_server:get_local(Pid, Key),
    ?assertEqual(Value, Retrieved),

    gen_server:stop(Pid).

%% Test: handle_message with find_value request (value exists)
handle_find_value_exists_test() ->
    LocalNodeId = <<1:256>>,
    {ok, Pid} = macula_routing_server:start_link(LocalNodeId, #{}),

    Key = <<100:256>>,
    Value = <<"found_value">>,
    macula_routing_server:store_local(Pid, Key, Value),

    Message = macula_routing_protocol:encode_find_value(Key),
    Reply = macula_routing_server:handle_message(Pid, Message),

    %% Should return value
    ?assertEqual(#{type => find_value_reply, result => value, value => Value}, Reply),

    gen_server:stop(Pid).

%% Test: handle_message with find_value request (value not found)
handle_find_value_not_found_test() ->
    LocalNodeId = <<0:256>>,
    {ok, Pid} = macula_routing_server:start_link(LocalNodeId, #{}),

    %% Add some nodes to routing table
    Node = #{node_id => <<100:256>>, address => {{127,0,0,1}, 8080}},
    macula_routing_server:add_node(Pid, Node),

    Key = <<100:256>>,
    Message = macula_routing_protocol:encode_find_value(Key),
    Reply = macula_routing_server:handle_message(Pid, Message),

    %% Should return nodes
    ?assertMatch(#{type := find_value_reply, result := nodes, nodes := _}, Reply),

    gen_server:stop(Pid).
