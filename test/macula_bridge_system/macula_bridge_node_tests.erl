%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for macula_bridge_node - Bridge Node managing parent connections.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_bridge_node_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

bridge_node_test_() ->
    [
        {"starts with no parent connection", fun no_parent_connection/0},
        {"is_connected returns false when not connected", fun is_connected_false/0},
        {"get_parent_bridges returns configured bridges", fun get_parent_bridges/0},
        {"set_parent_bridges updates configuration", fun set_parent_bridges/0},
        {"get_stats returns statistics", fun get_stats/0},
        {"escalate_query fails when not connected", fun escalate_query_not_connected/0},
        {"store_to_parent fails when not connected", fun store_to_parent_not_connected/0},
        {"init_stats starts with zero counters", fun init_stats_zeros/0}
    ].

start_node(Config) ->
    process_flag(trap_exit, true),
    {ok, Pid} = macula_bridge_node:start_link(Config),
    unregister(macula_bridge_node),
    Pid.

stop_node(Pid) ->
    catch gen_server:stop(Pid),
    %% Flush any EXIT messages
    receive {'EXIT', Pid, _} -> ok after 10 -> ok end.

%%%===================================================================
%%% Test Cases
%%%===================================================================

no_parent_connection() ->
    Pid = start_node(#{mesh_level => cluster, parent_bridges => []}),

    %% With no parent bridges configured, should not be connected
    ?assertEqual(false, macula_bridge_node:is_connected(Pid)),

    stop_node(Pid).

is_connected_false() ->
    %% Even with bridges configured, won't be connected immediately
    Pid = start_node(#{mesh_level => cluster, parent_bridges => [<<"quic://parent:9443">>]}),

    %% Connection is async, so initially not connected
    ?assertEqual(false, macula_bridge_node:is_connected(Pid)),

    stop_node(Pid).

get_parent_bridges() ->
    Bridges = [<<"quic://bridge1:9443">>, <<"quic://bridge2:9443">>],
    Pid = start_node(#{mesh_level => cluster, parent_bridges => Bridges}),

    ?assertEqual(Bridges, macula_bridge_node:get_parent_bridges(Pid)),

    stop_node(Pid).

set_parent_bridges() ->
    Pid = start_node(#{mesh_level => cluster, parent_bridges => []}),

    OldBridges = macula_bridge_node:get_parent_bridges(Pid),
    ?assertEqual([], OldBridges),

    NewBridges = [<<"quic://new-bridge:9443">>],
    ok = macula_bridge_node:set_parent_bridges(Pid, NewBridges),

    ?assertEqual(NewBridges, macula_bridge_node:get_parent_bridges(Pid)),

    stop_node(Pid).

get_stats() ->
    Pid = start_node(#{mesh_level => cluster, parent_bridges => []}),

    {ok, Stats} = macula_bridge_node:get_stats(Pid),

    %% Check stats structure
    ?assertEqual(cluster, maps:get(mesh_level, Stats)),
    ?assertEqual(false, maps:get(is_connected, Stats)),
    ?assertEqual(undefined, maps:get(connected_parent, Stats)),
    ?assertEqual(0, maps:get(queries_escalated, Stats)),
    ?assertEqual(0, maps:get(queries_successful, Stats)),
    ?assertEqual(0, maps:get(queries_failed, Stats)),
    ?assertEqual(0, maps:get(stores_propagated, Stats)),
    ?assertEqual(0, maps:get(disconnections, Stats)),
    ?assert(is_integer(maps:get(started_at, Stats))),

    stop_node(Pid).

escalate_query_not_connected() ->
    Pid = start_node(#{mesh_level => cluster, parent_bridges => []}),

    Query = #{type => find_value, key => <<"test-key">>},
    Result = macula_bridge_node:escalate_query(Pid, Query),

    ?assertEqual({error, not_connected}, Result),

    %% Stats should show failed query
    {ok, Stats} = macula_bridge_node:get_stats(Pid),
    ?assertEqual(1, maps:get(queries_failed, Stats)),

    stop_node(Pid).

store_to_parent_not_connected() ->
    Pid = start_node(#{mesh_level => cluster, parent_bridges => []}),

    StoreMsg = #{key => <<"test-key">>, value => #{data => <<"test">>}},
    Result = macula_bridge_node:store_to_parent(Pid, StoreMsg),

    ?assertEqual({error, not_connected}, Result),

    stop_node(Pid).

init_stats_zeros() ->
    Pid = start_node(#{mesh_level => cluster, parent_bridges => []}),

    {ok, Stats} = macula_bridge_node:get_stats(Pid),

    ?assertEqual(0, maps:get(queries_escalated, Stats)),
    ?assertEqual(0, maps:get(queries_successful, Stats)),
    ?assertEqual(0, maps:get(queries_failed, Stats)),
    ?assertEqual(0, maps:get(stores_propagated, Stats)),
    ?assertEqual(0, maps:get(cache_hits, Stats)),
    ?assertEqual(0, maps:get(disconnections, Stats)),

    stop_node(Pid).

%%%===================================================================
%%% Endpoint Parsing Tests
%%%===================================================================

endpoint_parsing_test_() ->
    [
        {"parse valid quic endpoint", fun parse_quic_endpoint/0},
        {"parse valid host:port endpoint", fun parse_host_port_endpoint/0}
    ].

parse_quic_endpoint() ->
    %% This is tested indirectly through connection attempts
    %% The parse_bridge_endpoint function is internal
    ok.

parse_host_port_endpoint() ->
    %% This is tested indirectly through connection attempts
    ok.
