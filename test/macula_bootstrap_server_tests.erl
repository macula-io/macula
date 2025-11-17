%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for macula_bootstrap_server.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_bootstrap_server_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

bootstrap_server_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Fixture) ->
         [
          {"handle FIND_NODE query", fun() -> test_find_node(Fixture) end},
          {"handle FIND_VALUE query", fun() -> test_find_value(Fixture) end},
          {"handle STORE query", fun() -> test_store(Fixture) end},
          {"get stats", fun() -> test_get_stats(Fixture) end},
          {"track query statistics", fun() -> test_query_stats(Fixture) end}
         ]
     end}.

%%%===================================================================
%%% Setup/Cleanup
%%%===================================================================

setup() ->
    %% Start routing server (dependency)
    LocalNodeId = crypto:hash(sha256, <<"test_bootstrap_node">>),
    RoutingConfig = #{k => 20, alpha => 3},
    {ok, RoutingPid} = macula_routing_server:start_link(LocalNodeId, RoutingConfig),

    %% Start bootstrap registry (dependency)
    {ok, RegistryPid} = macula_bootstrap_registry:start_link(#{}),

    %% Start bootstrap server
    ServerConfig = #{realm => <<"test.realm">>},
    {ok, ServerPid} = macula_bootstrap_server:start_link(ServerConfig),

    #{
        routing_pid => RoutingPid,
        registry_pid => RegistryPid,
        server_pid => ServerPid,
        local_node_id => LocalNodeId
    }.

cleanup(#{routing_pid := RoutingPid, registry_pid := RegistryPid, server_pid := ServerPid}) ->
    catch gen_server:stop(ServerPid),
    catch gen_server:stop(RegistryPid),
    catch gen_server:stop(RoutingPid),
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================

test_find_node(#{routing_pid := RoutingPid}) ->
    %% Add some nodes to routing table
    Node1 = #{node_id => crypto:hash(sha256, <<"node1">>), address => <<"127.0.0.1:9001">>},
    Node2 = #{node_id => crypto:hash(sha256, <<"node2">>), address => <<"127.0.0.1:9002">>},

    ok = macula_routing_server:add_node(RoutingPid, Node1),
    ok = macula_routing_server:add_node(RoutingPid, Node2),

    %% Query for closest nodes
    TargetId = crypto:hash(sha256, <<"target">>),
    {ok, Nodes} = macula_bootstrap_server:handle_dht_query(find_node, TargetId),

    %% Should return closest nodes
    ?assert(is_list(Nodes)),
    ?assert(length(Nodes) > 0).

test_find_value(_Fixture) ->
    %% Store a value in DHT
    Key = <<"test.service">>,
    Value = #{node_id => crypto:hash(sha256, <<"provider">>)},

    ok = macula_bootstrap_server:handle_dht_query(store, {Key, Value}),

    %% Find the value
    Result = macula_bootstrap_server:handle_dht_query(find_value, Key),

    %% Should return the value (DHT returns as list)
    case Result of
        {ok, [V]} when is_map(V) ->
            ?assertEqual(Value, V);
        {ok, V} when is_map(V) ->
            ?assertEqual(Value, V);
        Other ->
            ?debugFmt("Unexpected result: ~p", [Other]),
            ?assert(false)
    end.

test_store(_Fixture) ->
    %% Store a key-value pair
    Key = <<"test.key">>,
    Value = #{data => <<"test_data">>},

    Result = macula_bootstrap_server:handle_dht_query(store, {Key, Value}),

    ?assertEqual(ok, Result).

test_get_stats(_Fixture) ->
    %% Get initial stats
    {ok, Stats} = macula_bootstrap_server:get_stats(),

    %% Should have expected fields
    ?assertMatch(#{queries_handled := _, services_registered := _, uptime_seconds := _}, Stats),

    %% Uptime should be non-negative
    UptimeSeconds = maps:get(uptime_seconds, Stats),
    ?assert(UptimeSeconds >= 0).

test_query_stats(_Fixture) ->
    %% Get initial stats
    {ok, InitialStats} = macula_bootstrap_server:get_stats(),
    InitialQueries = maps:get(queries_handled, InitialStats),

    %% Perform a query
    TargetId = crypto:hash(sha256, <<"target">>),
    {ok, _} = macula_bootstrap_server:handle_dht_query(find_node, TargetId),

    %% Get updated stats
    {ok, UpdatedStats} = macula_bootstrap_server:get_stats(),
    UpdatedQueries = maps:get(queries_handled, UpdatedStats),

    %% Query count should have increased
    ?assertEqual(InitialQueries + 1, UpdatedQueries).
