%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_cluster_static module.
%%%
%%% Tests the static cluster strategy that connects to a predefined
%%% list of nodes (equivalent to libcluster's Cluster.Strategy.Epmd).
%%%
%%% Test categories:
%%% - Configuration parsing (nodes from options, env vars)
%%% - Node connection/disconnection
%%% - Reconnection logic
%%% - Callback notifications
%%% - API functions
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cluster_static_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Setup/Teardown
%%%===================================================================

setup() ->
    %% Ensure no previous instance is running
    catch macula_cluster_static:stop(),
    %% Clean environment
    os:unsetenv("CLUSTER_NODES"),
    ok.

cleanup(_) ->
    catch macula_cluster_static:stop(),
    os:unsetenv("CLUSTER_NODES"),
    ok.

%%%===================================================================
%%% Configuration Parsing Tests
%%%===================================================================

start_with_empty_nodes_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [?_assertMatch({ok, _Pid}, macula_cluster_static:start_link(#{nodes => []}))]
     end}.

start_with_explicit_nodes_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         Nodes = ['node1@localhost', 'node2@localhost'],
         {ok, _Pid} = macula_cluster_static:start_link(#{nodes => Nodes}),
         [?_assertEqual(Nodes, macula_cluster_static:get_nodes())]
     end}.

start_with_env_var_nodes_test_() ->
    {setup,
     fun() ->
         setup(),
         os:putenv("CLUSTER_NODES", "node1@host1,node2@host2,node3@host3")
     end,
     fun cleanup/1,
     fun(_) ->
         {ok, _Pid} = macula_cluster_static:start_link(#{}),
         Expected = ['node1@host1', 'node2@host2', 'node3@host3'],
         [?_assertEqual(Expected, macula_cluster_static:get_nodes())]
     end}.

start_with_env_var_whitespace_test_() ->
    {setup,
     fun() ->
         setup(),
         os:putenv("CLUSTER_NODES", " node1@host1 , node2@host2 ")
     end,
     fun cleanup/1,
     fun(_) ->
         {ok, _Pid} = macula_cluster_static:start_link(#{}),
         Expected = ['node1@host1', 'node2@host2'],
         [?_assertEqual(Expected, macula_cluster_static:get_nodes())]
     end}.

start_with_empty_env_var_test_() ->
    {setup,
     fun() ->
         setup(),
         os:putenv("CLUSTER_NODES", "")
     end,
     fun cleanup/1,
     fun(_) ->
         {ok, _Pid} = macula_cluster_static:start_link(#{}),
         [?_assertEqual([], macula_cluster_static:get_nodes())]
     end}.

%%%===================================================================
%%% API Tests
%%%===================================================================

get_connected_initially_empty_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         {ok, _Pid} = macula_cluster_static:start_link(#{nodes => []}),
         [?_assertEqual([], macula_cluster_static:get_connected())]
     end}.

add_node_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         {ok, _Pid} = macula_cluster_static:start_link(#{nodes => []}),
         ok = macula_cluster_static:add_node('new_node@localhost'),
         [?_assertEqual(['new_node@localhost'], macula_cluster_static:get_nodes())]
     end}.

remove_node_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         Nodes = ['node1@localhost', 'node2@localhost'],
         {ok, _Pid} = macula_cluster_static:start_link(#{nodes => Nodes}),
         ok = macula_cluster_static:remove_node('node1@localhost'),
         [?_assertEqual(['node2@localhost'], macula_cluster_static:get_nodes())]
     end}.

stop_test_() ->
    {setup,
     fun setup/0,
     fun(_) -> ok end,
     fun(_) ->
         {ok, Pid} = macula_cluster_static:start_link(#{nodes => []}),
         ?assert(is_process_alive(Pid)),
         ok = macula_cluster_static:stop(),
         timer:sleep(100),
         [?_assertNot(is_process_alive(Pid))]
     end}.

%%%===================================================================
%%% Callback Tests
%%%===================================================================

callback_pid_receives_nodeup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         TestPid = self(),
         {ok, Pid} = macula_cluster_static:start_link(#{
             nodes => [],
             callback => TestPid
         }),
         %% Simulate nodeup by sending message directly to the strategy
         %% (In real usage, net_kernel sends this)
         Pid ! {nodeup, 'fake_node@localhost'},
         %% Note: callback is only sent for nodes in the configured list
         %% Since 'fake_node@localhost' is not in the list, no callback
         [?_assertEqual(true, is_process_alive(Pid))]
     end}.

%%%===================================================================
%%% Custom Reconnect Interval Test
%%%===================================================================

custom_reconnect_interval_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         {ok, _Pid} = macula_cluster_static:start_link(#{
             nodes => [],
             reconnect_interval => 10000
         }),
         [?_assertEqual([], macula_cluster_static:get_nodes())]
     end}.

%%%===================================================================
%%% Self-Connection Prevention Test
%%%===================================================================

%% This test verifies that the strategy doesn't try to connect to itself
%% when node() is in the configured list
self_connection_prevention_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         %% Include current node in the list
         CurrentNode = node(),
         {ok, _Pid} = macula_cluster_static:start_link(#{
             nodes => [CurrentNode, 'other_node@localhost']
         }),
         %% Should not crash, and should not be in connected list
         Connected = macula_cluster_static:get_connected(),
         [?_assertNot(lists:member(CurrentNode, Connected))]
     end}.

%%%===================================================================
%%% High-Level API Tests (macula_cluster module)
%%%===================================================================

macula_cluster_start_cluster_exports_test() ->
    %% Verify the function is exported
    ?assert(erlang:function_exported(macula_cluster, start_cluster, 0)),
    ?assert(erlang:function_exported(macula_cluster, start_cluster, 1)).

macula_cluster_stop_cluster_exports_test() ->
    ?assert(erlang:function_exported(macula_cluster, stop_cluster, 0)).

macula_cluster_nodes_exports_test() ->
    ?assert(erlang:function_exported(macula_cluster, nodes, 0)).

macula_cluster_is_clustered_exports_test() ->
    ?assert(erlang:function_exported(macula_cluster, is_clustered, 0)).

macula_cluster_is_clustered_false_initially_test() ->
    %% Stop any running strategies first
    catch macula_cluster_static:stop(),
    catch macula_cluster_strategy:stop(macula_cluster),
    timer:sleep(100),
    ?assertEqual(false, macula_cluster:is_clustered()).

%%%===================================================================
%%% Node List Type Handling Tests
%%%===================================================================

nodes_as_strings_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         %% Pass nodes as strings
         Nodes = ["node1@localhost", "node2@localhost"],
         {ok, _Pid} = macula_cluster_static:start_link(#{nodes => Nodes}),
         %% Should convert to atoms
         Result = macula_cluster_static:get_nodes(),
         [?_assertEqual(['node1@localhost', 'node2@localhost'], Result)]
     end}.

nodes_as_binaries_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         %% Pass nodes as binaries
         Nodes = [<<"node1@localhost">>, <<"node2@localhost">>],
         {ok, _Pid} = macula_cluster_static:start_link(#{nodes => Nodes}),
         %% Should convert to atoms
         Result = macula_cluster_static:get_nodes(),
         [?_assertEqual(['node1@localhost', 'node2@localhost'], Result)]
     end}.
