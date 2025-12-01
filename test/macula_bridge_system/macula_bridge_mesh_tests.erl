%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for macula_bridge_mesh - Mesh formation between peer bridges.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_bridge_mesh_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

bridge_mesh_test_() ->
    [
        {"starts with empty peers", fun empty_peers/0},
        {"add_peer adds to peer list", fun add_peer/0},
        {"remove_peer removes from peer list", fun remove_peer/0},
        {"get_peers returns all peers", fun get_peers/0},
        {"get_stats returns statistics", fun get_stats/0},
        {"static discovery method", fun static_discovery/0}
    ].

start_mesh(Config) ->
    process_flag(trap_exit, true),
    {ok, Pid} = macula_bridge_mesh:start_link(Config),
    unregister(macula_bridge_mesh),
    Pid.

stop_mesh(Pid) ->
    catch gen_server:stop(Pid),
    %% Flush any EXIT messages
    receive {'EXIT', Pid, _} -> ok after 10 -> ok end.

%%%===================================================================
%%% Test Cases
%%%===================================================================

empty_peers() ->
    Pid = start_mesh(#{mesh_level => cluster, discovery_method => static}),

    Peers = macula_bridge_mesh:get_peers(Pid),
    ?assertEqual([], Peers),

    stop_mesh(Pid).

add_peer() ->
    Pid = start_mesh(#{mesh_level => cluster, discovery_method => static}),

    PeerInfo = #{
        node_id => <<"peer-1">>,
        endpoint => <<"quic://peer1:9443">>,
        mesh_level => cluster
    },

    ok = macula_bridge_mesh:add_peer(Pid, PeerInfo),

    Peers = macula_bridge_mesh:get_peers(Pid),
    ?assertEqual(1, length(Peers)),

    [StoredPeer] = Peers,
    ?assertEqual(<<"peer-1">>, maps:get(node_id, StoredPeer)),

    stop_mesh(Pid).

remove_peer() ->
    Pid = start_mesh(#{mesh_level => cluster, discovery_method => static}),

    PeerInfo = #{
        node_id => <<"peer-1">>,
        endpoint => <<"quic://peer1:9443">>,
        mesh_level => cluster
    },

    ok = macula_bridge_mesh:add_peer(Pid, PeerInfo),
    ?assertEqual(1, length(macula_bridge_mesh:get_peers(Pid))),

    ok = macula_bridge_mesh:remove_peer(Pid, <<"peer-1">>),
    ?assertEqual(0, length(macula_bridge_mesh:get_peers(Pid))),

    stop_mesh(Pid).

get_peers() ->
    Pid = start_mesh(#{mesh_level => cluster, discovery_method => static}),

    %% Add multiple peers
    Peer1 = #{node_id => <<"peer-1">>, endpoint => <<"quic://peer1:9443">>, mesh_level => cluster},
    Peer2 = #{node_id => <<"peer-2">>, endpoint => <<"quic://peer2:9443">>, mesh_level => cluster},
    Peer3 = #{node_id => <<"peer-3">>, endpoint => <<"quic://peer3:9443">>, mesh_level => cluster},

    ok = macula_bridge_mesh:add_peer(Pid, Peer1),
    ok = macula_bridge_mesh:add_peer(Pid, Peer2),
    ok = macula_bridge_mesh:add_peer(Pid, Peer3),

    Peers = macula_bridge_mesh:get_peers(Pid),
    ?assertEqual(3, length(Peers)),

    %% Check all peers are present
    NodeIds = [maps:get(node_id, P) || P <- Peers],
    ?assert(lists:member(<<"peer-1">>, NodeIds)),
    ?assert(lists:member(<<"peer-2">>, NodeIds)),
    ?assert(lists:member(<<"peer-3">>, NodeIds)),

    stop_mesh(Pid).

get_stats() ->
    Pid = start_mesh(#{mesh_level => cluster, discovery_method => static}),

    %% Add a peer to generate some stats
    PeerInfo = #{node_id => <<"peer-1">>, endpoint => <<"quic://peer1:9443">>, mesh_level => cluster},
    ok = macula_bridge_mesh:add_peer(Pid, PeerInfo),

    {ok, Stats} = macula_bridge_mesh:get_stats(Pid),

    ?assertEqual(cluster, maps:get(mesh_level, Stats)),
    ?assertEqual(static, maps:get(discovery_method, Stats)),
    ?assertEqual(1, maps:get(peer_count, Stats)),
    ?assertEqual(1, maps:get(peers_added, Stats)),
    ?assertEqual(0, maps:get(peers_removed, Stats)),
    ?assert(is_integer(maps:get(started_at, Stats))),

    stop_mesh(Pid).

static_discovery() ->
    %% Static discovery means manual peer management only
    Pid = start_mesh(#{mesh_level => cluster, discovery_method => static}),

    {ok, Stats} = macula_bridge_mesh:get_stats(Pid),
    ?assertEqual(static, maps:get(discovery_method, Stats)),

    stop_mesh(Pid).

%%%===================================================================
%%% Mesh Level Tests
%%%===================================================================

mesh_level_test_() ->
    [
        {"street level mesh", fun street_level/0},
        {"neighborhood level mesh", fun neighborhood_level/0},
        {"city level mesh", fun city_level/0}
    ].

street_level() ->
    Pid = start_mesh(#{mesh_level => street, discovery_method => static}),

    {ok, Stats} = macula_bridge_mesh:get_stats(Pid),
    ?assertEqual(street, maps:get(mesh_level, Stats)),

    stop_mesh(Pid).

neighborhood_level() ->
    Pid = start_mesh(#{mesh_level => neighborhood, discovery_method => static}),

    {ok, Stats} = macula_bridge_mesh:get_stats(Pid),
    ?assertEqual(neighborhood, maps:get(mesh_level, Stats)),

    stop_mesh(Pid).

city_level() ->
    Pid = start_mesh(#{mesh_level => city, discovery_method => static}),

    {ok, Stats} = macula_bridge_mesh:get_stats(Pid),
    ?assertEqual(city, maps:get(mesh_level, Stats)),

    stop_mesh(Pid).
