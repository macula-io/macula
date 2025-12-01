%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for macula_bridge_system - Bridge System Supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_bridge_system_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

bridge_system_test_() ->
    [
        {"bridge disabled starts no children", fun bridge_disabled/0},
        {"bridge enabled starts all children", fun bridge_enabled/0},
        {"get_bridge_pid returns pid when enabled", fun get_bridge_pid/0},
        {"get_mesh_pid returns pid when enabled", fun get_mesh_pid/0},
        {"is_bridge_enabled returns false when not connected", fun is_bridge_enabled/0},
        {"get_stats returns error when disabled", fun get_stats_disabled/0}
    ].

cleanup_system() ->
    process_flag(trap_exit, true),
    case whereis(macula_bridge_system) of
        undefined -> ok;
        Pid ->
            catch gen_server:stop(Pid),
            receive {'EXIT', Pid, _} -> ok after 50 -> ok end
    end.

%%%===================================================================
%%% Test Cases
%%%===================================================================

stop_system(Pid) ->
    catch gen_server:stop(Pid),
    receive {'EXIT', Pid, _} -> ok after 50 -> ok end.

bridge_disabled() ->
    cleanup_system(),
    Config = #{bridge_enabled => false},
    {ok, Pid} = macula_bridge_system:start_link(Config),

    %% Supervisor should start but with no children
    Children = supervisor:which_children(Pid),
    ?assertEqual([], Children),

    stop_system(Pid).

bridge_enabled() ->
    cleanup_system(),
    Config = #{
        bridge_enabled => true,
        mesh_level => cluster,
        parent_bridges => [],
        discovery_method => static
    },
    {ok, Pid} = macula_bridge_system:start_link(Config),

    %% Supervisor should start with all three children
    Children = supervisor:which_children(Pid),
    ?assertEqual(3, length(Children)),

    %% Check all expected children are present
    ChildIds = [Id || {Id, _, _, _} <- Children],
    ?assert(lists:member(bridge_node, ChildIds)),
    ?assert(lists:member(bridge_mesh, ChildIds)),
    ?assert(lists:member(bridge_cache, ChildIds)),

    stop_system(Pid).

get_bridge_pid() ->
    cleanup_system(),
    Config = #{
        bridge_enabled => true,
        mesh_level => cluster,
        parent_bridges => [],
        discovery_method => static
    },
    {ok, SupPid} = macula_bridge_system:start_link(Config),

    Result = macula_bridge_system:get_bridge_pid(),
    ?assertMatch({ok, _}, Result),
    {ok, BridgePid} = Result,
    ?assert(is_pid(BridgePid)),

    stop_system(SupPid).

get_mesh_pid() ->
    cleanup_system(),
    Config = #{
        bridge_enabled => true,
        mesh_level => cluster,
        parent_bridges => [],
        discovery_method => static
    },
    {ok, SupPid} = macula_bridge_system:start_link(Config),

    Result = macula_bridge_system:get_mesh_pid(),
    ?assertMatch({ok, _}, Result),
    {ok, MeshPid} = Result,
    ?assert(is_pid(MeshPid)),

    stop_system(SupPid).

is_bridge_enabled() ->
    cleanup_system(),
    Config = #{
        bridge_enabled => true,
        mesh_level => cluster,
        parent_bridges => [],  %% No parents = not connected
        discovery_method => static
    },
    {ok, SupPid} = macula_bridge_system:start_link(Config),

    %% Without parent bridges, bridge node won't be connected
    Result = macula_bridge_system:is_bridge_enabled(),
    ?assertEqual(false, Result),

    stop_system(SupPid).

get_stats_disabled() ->
    cleanup_system(),
    Config = #{bridge_enabled => false},
    {ok, SupPid} = macula_bridge_system:start_link(Config),

    Result = macula_bridge_system:get_stats(),
    ?assertEqual({error, not_started}, Result),

    stop_system(SupPid).

%%%===================================================================
%%% Mesh Level Configuration Tests
%%%===================================================================

mesh_level_test_() ->
    [
        {"cluster level configuration", fun cluster_level_config/0},
        {"street level configuration", fun street_level_config/0},
        {"city level configuration", fun city_level_config/0}
    ].

cluster_level_config() ->
    cleanup_system(),
    Config = #{
        bridge_enabled => true,
        mesh_level => cluster,
        parent_bridges => [],
        discovery_method => static
    },
    {ok, SupPid} = macula_bridge_system:start_link(Config),

    {ok, Stats} = macula_bridge_system:get_stats(),
    ?assertEqual(cluster, maps:get(mesh_level, Stats)),

    stop_system(SupPid).

street_level_config() ->
    cleanup_system(),
    Config = #{
        bridge_enabled => true,
        mesh_level => street,
        parent_bridges => [],
        discovery_method => static
    },
    {ok, SupPid} = macula_bridge_system:start_link(Config),

    {ok, Stats} = macula_bridge_system:get_stats(),
    ?assertEqual(street, maps:get(mesh_level, Stats)),

    stop_system(SupPid).

city_level_config() ->
    cleanup_system(),
    Config = #{
        bridge_enabled => true,
        mesh_level => city,
        parent_bridges => [],
        discovery_method => static
    },
    {ok, SupPid} = macula_bridge_system:start_link(Config),

    {ok, Stats} = macula_bridge_system:get_stats(),
    ?assertEqual(city, maps:get(mesh_level, Stats)),

    stop_system(SupPid).
