%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_gateway_mesh module.
%%% Tests mesh connection pooling and lifecycle management - Phase 9 of gateway refactoring.
%%%
%%% TDD Approach:
%%% 1. Write failing tests first
%%% 2. Implement minimal functionality
%%% 3. Make tests pass incrementally
%%% 4. Refactor for idiomatic Erlang
%%%
%%% Responsibilities:
%%% - Pool QUIC connections to remote peers
%%% - Check connection liveness before reuse
%%% - Open new streams on pooled connections
%%% - Auto-cleanup dead connections
%%% - Cache connection metadata by node_id
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_mesh_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    %% Start with TLS cert paths in opts
    Opts = #{
        cert_file => "/opt/macula/certs/cert.pem",
        key_file => "/opt/macula/certs/key.pem"
    },
    {ok, Pid} = macula_gateway_mesh:start_link(Opts),
    Pid.

cleanup(Pid) ->
    case erlang:is_process_alive(Pid) of
        true -> macula_gateway_mesh:stop(Pid);
        false -> ok
    end.

%%%===================================================================
%%% Basic API Tests
%%%===================================================================

module_exports_test() ->
    Exports = macula_gateway_mesh:module_info(exports),

    ?assert(lists:member({start_link, 1}, Exports)),
    ?assert(lists:member({stop, 1}, Exports)),
    ?assert(lists:member({get_or_create_connection, 3}, Exports)),
    ?assert(lists:member({remove_connection, 2}, Exports)),
    ?assert(lists:member({get_connection_info, 2}, Exports)),
    ?assert(lists:member({list_connections, 1}, Exports)).

gen_server_callbacks_test() ->
    Exports = macula_gateway_mesh:module_info(exports),

    ?assert(lists:member({init, 1}, Exports)),
    ?assert(lists:member({handle_call, 3}, Exports)),
    ?assert(lists:member({handle_cast, 2}, Exports)),
    ?assert(lists:member({handle_info, 2}, Exports)),
    ?assert(lists:member({terminate, 2}, Exports)).

%%%===================================================================
%%% Startup/Shutdown Tests
%%%===================================================================

start_link_test() ->
    {ok, Pid} = macula_gateway_mesh:start_link(#{}),
    ?assert(erlang:is_process_alive(Pid)),
    macula_gateway_mesh:stop(Pid).

stop_test() ->
    {ok, Pid} = macula_gateway_mesh:start_link(#{}),
    ok = macula_gateway_mesh:stop(Pid),
    timer:sleep(50),
    ?assertNot(erlang:is_process_alive(Pid)).

%%%===================================================================
%%% Connection Creation Tests
%%%===================================================================

get_or_create_connection_creates_new_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        NodeId = crypto:strong_rand_bytes(32),
        Address = {{127,0,0,1}, 5001},

        %% Mock QUIC connection creation
        %% In real implementation, this would call macula_quic:connect/4
        %% For now, test will fail until implementation exists
        Result = macula_gateway_mesh:get_or_create_connection(Pid, NodeId, Address),

        %% Should return {ok, Stream} when connection created successfully
        [?_assertMatch({ok, _Stream}, Result)]
     end}.

get_or_create_connection_stores_mapping_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        NodeId = crypto:strong_rand_bytes(32),
        Address = {{127,0,0,1}, 5001},

        %% Create connection (will fail in TDD until implemented)
        _Result = macula_gateway_mesh:get_or_create_connection(Pid, NodeId, Address),

        %% Verify connection info stored
        InfoResult = macula_gateway_mesh:get_connection_info(Pid, NodeId),
        [?_assertMatch({ok, #{address := Address}}, InfoResult)]
     end}.

%%%===================================================================
%%% Connection Reuse Tests
%%%===================================================================

get_or_create_connection_reuses_alive_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        NodeId = crypto:strong_rand_bytes(32),
        Address = {{127,0,0,1}, 5001},

        %% Create connection first time
        {ok, Stream1} = macula_gateway_mesh:get_or_create_connection(Pid, NodeId, Address),

        %% Get connection second time - should reuse (open new stream)
        {ok, Stream2} = macula_gateway_mesh:get_or_create_connection(Pid, NodeId, Address),

        %% Should return different streams (new stream per request)
        [?_assertNot(Stream1 =:= Stream2)]
     end}.

get_or_create_connection_removes_dead_and_retries_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        NodeId = crypto:strong_rand_bytes(32),
        Address = {{127,0,0,1}, 5001},

        %% Create connection
        {ok, _Stream1} = macula_gateway_mesh:get_or_create_connection(Pid, NodeId, Address),

        %% Get connection info
        {ok, ConnInfo} = macula_gateway_mesh:get_connection_info(Pid, NodeId),
        #{connection := Conn} = ConnInfo,

        %% Kill the connection process
        exit(Conn, kill),
        timer:sleep(100),

        %% Try to get connection again - should detect dead, remove, and recreate
        Result = macula_gateway_mesh:get_or_create_connection(Pid, NodeId, Address),

        %% Should succeed with new connection
        [?_assertMatch({ok, _NewStream}, Result)]
     end}.

%%%===================================================================
%%% Connection Lifecycle Tests
%%%===================================================================

connection_auto_removed_on_death_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        NodeId = crypto:strong_rand_bytes(32),
        Address = {{127,0,0,1}, 5001},

        %% Create connection
        {ok, _Stream} = macula_gateway_mesh:get_or_create_connection(Pid, NodeId, Address),

        %% Get connection info
        {ok, ConnInfo} = macula_gateway_mesh:get_connection_info(Pid, NodeId),
        #{connection := Conn} = ConnInfo,

        %% Kill connection
        exit(Conn, kill),
        timer:sleep(150),

        %% Connection should be auto-removed (via monitor)
        [?_assertEqual(not_found, macula_gateway_mesh:get_connection_info(Pid, NodeId))]
     end}.

remove_connection_removes_mapping_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        NodeId = crypto:strong_rand_bytes(32),
        Address = {{127,0,0,1}, 5001},

        %% Create connection
        {ok, _Stream} = macula_gateway_mesh:get_or_create_connection(Pid, NodeId, Address),

        %% Explicitly remove
        ok = macula_gateway_mesh:remove_connection(Pid, NodeId),

        %% Should be gone
        [?_assertEqual(not_found, macula_gateway_mesh:get_connection_info(Pid, NodeId))]
     end}.

remove_connection_idempotent_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        NodeId = crypto:strong_rand_bytes(32),

        %% Remove non-existent connection
        ok = macula_gateway_mesh:remove_connection(Pid, NodeId),
        ok = macula_gateway_mesh:remove_connection(Pid, NodeId),

        [?_assertEqual(ok, ok)] % Should not crash
     end}.

%%%===================================================================
%%% Query Tests
%%%===================================================================

get_connection_info_not_found_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        NodeId = crypto:strong_rand_bytes(32),
        [?_assertEqual(not_found, macula_gateway_mesh:get_connection_info(Pid, NodeId))]
     end}.

get_connection_info_returns_metadata_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        NodeId = crypto:strong_rand_bytes(32),
        Address = {{127,0,0,1}, 5001},

        %% Create connection
        {ok, _Stream} = macula_gateway_mesh:get_or_create_connection(Pid, NodeId, Address),

        %% Get info
        {ok, ConnInfo} = macula_gateway_mesh:get_connection_info(Pid, NodeId),

        [
            ?_assertMatch(#{connection := _}, ConnInfo),
            ?_assertMatch(#{address := Address}, ConnInfo),
            ?_assertMatch(#{last_used := _}, ConnInfo)
        ]
     end}.

list_connections_empty_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        {ok, Connections} = macula_gateway_mesh:list_connections(Pid),
        [?_assertEqual([], Connections)]
     end}.

list_connections_multiple_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        NodeId1 = crypto:strong_rand_bytes(32),
        NodeId2 = crypto:strong_rand_bytes(32),
        Address1 = {{127,0,0,1}, 5001},
        Address2 = {{127,0,0,1}, 5002},

        %% Create two connections
        {ok, _} = macula_gateway_mesh:get_or_create_connection(Pid, NodeId1, Address1),
        {ok, _} = macula_gateway_mesh:get_or_create_connection(Pid, NodeId2, Address2),

        %% List connections
        {ok, Connections} = macula_gateway_mesh:list_connections(Pid),

        [
            ?_assertEqual(2, length(Connections)),
            ?_assert(lists:keymember(NodeId1, 1, Connections)),
            ?_assert(lists:keymember(NodeId2, 1, Connections))
        ]
     end}.

%%%===================================================================
%%% Edge Cases
%%%===================================================================

concurrent_connection_requests_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        NodeId = crypto:strong_rand_bytes(32),
        Address = {{127,0,0,1}, 5001},

        %% Make 10 concurrent connection requests
        Parent = self(),
        [spawn(fun() ->
            Result = macula_gateway_mesh:get_or_create_connection(Pid, NodeId, Address),
            Parent ! {connection, I, Result}
        end) || I <- lists:seq(1, 10)],

        %% Collect results
        Results = [receive {connection, I, R} -> R after 5000 -> timeout end
                   || I <- lists:seq(1, 10)],

        %% All should succeed
        SuccessCount = length([Res || {ok, _} = Res <- Results]),

        [?_assertEqual(10, SuccessCount)]
     end}.

connection_with_invalid_address_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        NodeId = crypto:strong_rand_bytes(32),
        InvalidAddress = {{0,0,0,0}, 0},

        %% Try to create connection with invalid address
        Result = macula_gateway_mesh:get_or_create_connection(Pid, NodeId, InvalidAddress),

        %% Should return error
        [?_assertMatch({error, _}, Result)]
     end}.

connection_to_unreachable_host_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        NodeId = crypto:strong_rand_bytes(32),
        %% Use address that will timeout/refuse
        UnreachableAddress = {{192,0,2,1}, 9999},  % TEST-NET-1 address

        %% Try to create connection
        Result = macula_gateway_mesh:get_or_create_connection(Pid, NodeId, UnreachableAddress),

        %% Should return error
        [?_assertMatch({error, _}, Result)]
     end}.

multiple_connections_crash_cleanup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        %% Create multiple connections
        Nodes = [{crypto:strong_rand_bytes(32), {{127,0,0,1}, 5000 + I}}
                 || I <- lists:seq(1, 5)],

        [macula_gateway_mesh:get_or_create_connection(Pid, NodeId, Addr)
         || {NodeId, Addr} <- Nodes],

        %% Get all connection PIDs
        {ok, AllConns} = macula_gateway_mesh:list_connections(Pid),
        ConnPids = [maps:get(connection, ConnInfo)
                    || {_NodeId, ConnInfo} <- AllConns],

        %% Kill all connections
        [exit(Conn, kill) || Conn <- ConnPids],
        timer:sleep(200),

        %% All should be cleaned up
        {ok, RemainingConns} = macula_gateway_mesh:list_connections(Pid),
        [?_assertEqual([], RemainingConns)]
     end}.

connection_with_undefined_placeholder_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        %% Test scenario where connection is stored as undefined (incoming connection)
        %% Then we need to create outbound connection
        NodeId = crypto:strong_rand_bytes(32),
        Address = {{127,0,0,1}, 5001},

        %% In real scenario, this would be populated by CONNECT handler
        %% For now, we test the get_or_create logic handles undefined connections

        %% Create connection (will be stored with real conn PID)
        {ok, _Stream} = macula_gateway_mesh:get_or_create_connection(Pid, NodeId, Address),

        %% Verify connection info has non-undefined connection
        {ok, ConnInfo} = macula_gateway_mesh:get_connection_info(Pid, NodeId),
        #{connection := Conn} = ConnInfo,

        [?_assertNot(Conn =:= undefined)]
     end}.

%%%===================================================================
%%% Connection Pool Limits Tests
%%%===================================================================

connection_pool_max_limit_test_() ->
    {setup,
     fun() ->
        %% Setup with small max_connections for testing
        Opts = #{
            max_mesh_connections => 3,
            cert_file => "/opt/macula/certs/cert.pem",
            key_file => "/opt/macula/certs/key.pem"
        },
        {ok, Pid} = macula_gateway_mesh:start_link(Opts),
        Pid
     end,
     fun cleanup/1,
     fun(Pid) ->
        %% Create 3 connections (fill pool)
        Node1 = crypto:strong_rand_bytes(32),
        Node2 = crypto:strong_rand_bytes(32),
        Node3 = crypto:strong_rand_bytes(32),
        Addr1 = {{127,0,0,1}, 5000},
        Addr2 = {{127,0,0,1}, 5001},
        Addr3 = {{127,0,0,1}, 5002},

        {ok, _} = macula_gateway_mesh:get_or_create_connection(Pid, Node1, Addr1),
        {ok, _} = macula_gateway_mesh:get_or_create_connection(Pid, Node2, Addr2),
        {ok, _} = macula_gateway_mesh:get_or_create_connection(Pid, Node3, Addr3),

        %% Verify pool size = 3
        {ok, Conns} = macula_gateway_mesh:list_connections(Pid),
        PoolSize1 = length(Conns),

        %% Create 4th connection (should evict oldest)
        Node4 = crypto:strong_rand_bytes(32),
        Addr4 = {{127,0,0,1}, 5003},
        {ok, _} = macula_gateway_mesh:get_or_create_connection(Pid, Node4, Addr4),

        %% Pool should still be 3 (evicted node1)
        {ok, Conns2} = macula_gateway_mesh:list_connections(Pid),
        PoolSize2 = length(Conns2),

        %% Node1 should be evicted (not in pool)
        Node1Info = macula_gateway_mesh:get_connection_info(Pid, Node1),

        %% Node4 should be in pool
        Node4Info = macula_gateway_mesh:get_connection_info(Pid, Node4),

        [
            ?_assertEqual(3, PoolSize1),
            ?_assertEqual(3, PoolSize2),
            ?_assertEqual(not_found, Node1Info),
            ?_assertMatch({ok, _}, Node4Info)
        ]
     end}.

connection_pool_lru_eviction_test_() ->
    {setup,
     fun() ->
        %% Setup with small max_connections for testing
        Opts = #{
            max_mesh_connections => 2,
            cert_file => "/opt/macula/certs/cert.pem",
            key_file => "/opt/macula/certs/key.pem"
        },
        {ok, Pid} = macula_gateway_mesh:start_link(Opts),
        Pid
     end,
     fun cleanup/1,
     fun(Pid) ->
        %% Create 2 connections
        Node1 = crypto:strong_rand_bytes(32),
        Node2 = crypto:strong_rand_bytes(32),
        Addr1 = {{127,0,0,1}, 5000},
        Addr2 = {{127,0,0,1}, 5001},

        {ok, _} = macula_gateway_mesh:get_or_create_connection(Pid, Node1, Addr1),
        timer:sleep(100),  % Ensure different timestamps
        {ok, _} = macula_gateway_mesh:get_or_create_connection(Pid, Node2, Addr2),

        %% Use node2 again (updates last_used)
        timer:sleep(100),
        {ok, _} = macula_gateway_mesh:get_or_create_connection(Pid, Node2, Addr2),

        %% Create node3 (should evict node1, not node2)
        Node3 = crypto:strong_rand_bytes(32),
        Addr3 = {{127,0,0,1}, 5002},
        {ok, _} = macula_gateway_mesh:get_or_create_connection(Pid, Node3, Addr3),

        %% Node1 evicted (oldest last_used)
        Node1Info = macula_gateway_mesh:get_connection_info(Pid, Node1),

        %% Node2 still in pool (recently used)
        Node2Info = macula_gateway_mesh:get_connection_info(Pid, Node2),

        %% Node3 in pool
        Node3Info = macula_gateway_mesh:get_connection_info(Pid, Node3),

        [
            ?_assertEqual(not_found, Node1Info),
            ?_assertMatch({ok, _}, Node2Info),
            ?_assertMatch({ok, _}, Node3Info)
        ]
     end}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================
%% (None currently needed)
