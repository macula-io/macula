%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_gateway_clients module.
%%% Tests client lifecycle management - Phase 2 of gateway refactoring.
%%%
%%% TDD Approach:
%%% 1. Write failing tests first
%%% 2. Implement minimal functionality
%%% 3. Make tests pass incrementally
%%% 4. Refactor for idiomatic Erlang
%%%
%%% Responsibilities:
%%% - Track connected clients
%%% - Monitor client processes
%%% - Store bidirectional streams
%%% - Clean up on disconnect
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_clients_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% Setup function for tests requiring a running client manager
setup() ->
    {ok, Pid} = macula_gateway_clients:start_link(#{}),
    Pid.

%% Cleanup function
cleanup(Pid) ->
    case erlang:is_process_alive(Pid) of
        true -> macula_gateway_clients:stop(Pid);
        false -> ok
    end.

%%%===================================================================
%%% Basic API Tests
%%%===================================================================

%% @doc Test module exports expected functions
module_exports_test() ->
    Exports = macula_gateway_clients:module_info(exports),

    ?assert(lists:member({start_link, 1}, Exports)),
    ?assert(lists:member({stop, 1}, Exports)),
    ?assert(lists:member({client_connected, 3}, Exports)),
    ?assert(lists:member({client_disconnected, 2}, Exports)),
    ?assert(lists:member({get_client_info, 2}, Exports)),
    ?assert(lists:member({get_all_clients, 1}, Exports)),
    ?assert(lists:member({is_client_alive, 2}, Exports)),
    ?assert(lists:member({store_client_stream, 3}, Exports)),
    ?assert(lists:member({get_client_stream, 2}, Exports)).

%% @doc Test gen_server callbacks are exported
gen_server_callbacks_test() ->
    Exports = macula_gateway_clients:module_info(exports),

    ?assert(lists:member({init, 1}, Exports)),
    ?assert(lists:member({handle_call, 3}, Exports)),
    ?assert(lists:member({handle_cast, 2}, Exports)),
    ?assert(lists:member({handle_info, 2}, Exports)),
    ?assert(lists:member({terminate, 2}, Exports)).

%%%===================================================================
%%% Startup/Shutdown Tests
%%%===================================================================

%% @doc Test manager starts successfully with default options
start_link_default_test() ->
    {ok, Pid} = macula_gateway_clients:start_link(#{}),
    ?assert(erlang:is_process_alive(Pid)),
    macula_gateway_clients:stop(Pid).

%% @doc Test manager starts with custom options
start_link_with_opts_test() ->
    Opts = #{realm => <<"test.realm">>, max_clients => 100},
    {ok, Pid} = macula_gateway_clients:start_link(Opts),
    ?assert(erlang:is_process_alive(Pid)),
    macula_gateway_clients:stop(Pid).

%% @doc Test manager stops gracefully
stop_test() ->
    {ok, Pid} = macula_gateway_clients:start_link(#{}),
    ok = macula_gateway_clients:stop(Pid),
    timer:sleep(50),
    ?assertNot(erlang:is_process_alive(Pid)).

%%%===================================================================
%%% Client Connection Tests
%%%===================================================================

%% @doc Test client_connected stores client information
client_connected_stores_info_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        ClientPid = spawn(fun() -> timer:sleep(1000) end),
        ClientInfo = #{
            realm => <<"test.realm">>,
            node_id => <<"node1">>,
            capabilities => [pubsub, rpc]
        },

        ok = macula_gateway_clients:client_connected(Pid, ClientPid, ClientInfo),

        {ok, Info} = macula_gateway_clients:get_client_info(Pid, ClientPid),
        [
            ?_assertEqual(<<"test.realm">>, maps:get(realm, Info)),
            ?_assertEqual(<<"node1">>, maps:get(node_id, Info)),
            ?_assertEqual([pubsub, rpc], maps:get(capabilities, Info))
        ]
     end}.

%% @doc Test client_connected monitors the client process
client_connected_monitors_process_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        ClientPid = spawn(fun() -> timer:sleep(100) end),
        ClientInfo = #{realm => <<"test">>, node_id => <<"node1">>},

        ok = macula_gateway_clients:client_connected(Pid, ClientPid, ClientInfo),

        %% Client should be in registry
        {ok, _Info} = macula_gateway_clients:get_client_info(Pid, ClientPid),

        %% Kill client
        exit(ClientPid, kill),
        timer:sleep(150),

        %% Client should be auto-removed after DOWN
        [?_assertEqual(not_found, macula_gateway_clients:get_client_info(Pid, ClientPid))]
     end}.

%% @doc Test client_connected with duplicate registration updates info
client_connected_duplicate_updates_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        ClientPid = spawn(fun() -> timer:sleep(1000) end),
        ClientInfo1 = #{realm => <<"test">>, node_id => <<"node1">>, version => 1},
        ClientInfo2 = #{realm => <<"test">>, node_id => <<"node1">>, version => 2},

        ok = macula_gateway_clients:client_connected(Pid, ClientPid, ClientInfo1),
        ok = macula_gateway_clients:client_connected(Pid, ClientPid, ClientInfo2),

        {ok, Info} = macula_gateway_clients:get_client_info(Pid, ClientPid),
        [?_assertEqual(2, maps:get(version, Info))]
     end}.

%%%===================================================================
%%% Client Disconnection Tests
%%%===================================================================

%% @doc Test client_disconnected removes client from registry
client_disconnected_removes_info_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        ClientPid = spawn(fun() -> timer:sleep(1000) end),
        ClientInfo = #{realm => <<"test">>, node_id => <<"node1">>},

        macula_gateway_clients:client_connected(Pid, ClientPid, ClientInfo),
        ok = macula_gateway_clients:client_disconnected(Pid, ClientPid),

        [?_assertEqual(not_found, macula_gateway_clients:get_client_info(Pid, ClientPid))]
     end}.

%% @doc Test client_disconnected is idempotent (safe to call multiple times)
client_disconnected_idempotent_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        ClientPid = spawn(fun() -> timer:sleep(1000) end),
        ClientInfo = #{realm => <<"test">>, node_id => <<"node1">>},

        macula_gateway_clients:client_connected(Pid, ClientPid, ClientInfo),
        ok = macula_gateway_clients:client_disconnected(Pid, ClientPid),
        ok = macula_gateway_clients:client_disconnected(Pid, ClientPid),

        [?_assertEqual(not_found, macula_gateway_clients:get_client_info(Pid, ClientPid))]
     end}.

%%%===================================================================
%%% Client Query Tests
%%%===================================================================

%% @doc Test get_client_info returns not_found for unknown client
get_client_info_not_found_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        UnknownPid = spawn(fun() -> ok end),
        [?_assertEqual(not_found, macula_gateway_clients:get_client_info(Pid, UnknownPid))]
     end}.

%% @doc Test get_all_clients returns empty list initially
get_all_clients_empty_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        {ok, Clients} = macula_gateway_clients:get_all_clients(Pid),
        [?_assertEqual([], Clients)]
     end}.

%% @doc Test get_all_clients returns all connected clients
get_all_clients_multiple_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Client1 = spawn(fun() -> timer:sleep(1000) end),
        Client2 = spawn(fun() -> timer:sleep(1000) end),

        macula_gateway_clients:client_connected(Pid, Client1,
            #{realm => <<"test">>, node_id => <<"node1">>}),
        macula_gateway_clients:client_connected(Pid, Client2,
            #{realm => <<"test">>, node_id => <<"node2">>}),

        {ok, Clients} = macula_gateway_clients:get_all_clients(Pid),
        [
            ?_assertEqual(2, length(Clients)),
            ?_assert(lists:keymember(Client1, 1, Clients)),
            ?_assert(lists:keymember(Client2, 1, Clients))
        ]
     end}.

%% @doc Test is_client_alive returns true for alive client
is_client_alive_true_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        ClientPid = spawn(fun() -> timer:sleep(1000) end),
        ClientInfo = #{realm => <<"test">>, node_id => <<"node1">>},

        macula_gateway_clients:client_connected(Pid, ClientPid, ClientInfo),

        [?_assertEqual(true, macula_gateway_clients:is_client_alive(Pid, ClientPid))]
     end}.

%% @doc Test is_client_alive returns false for dead client
is_client_alive_false_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        ClientPid = spawn(fun() -> ok end),
        timer:sleep(50), %% Ensure process dies

        [?_assertEqual(false, macula_gateway_clients:is_client_alive(Pid, ClientPid))]
     end}.

%%%===================================================================
%%% Client Stream Storage Tests
%%%===================================================================

%% @doc Test store_client_stream stores stream for node_id
store_client_stream_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        NodeId = <<"node123">>,
        Stream = spawn(fun() -> timer:sleep(1000) end),

        ok = macula_gateway_clients:store_client_stream(Pid, NodeId, Stream),

        {ok, StoredStream} = macula_gateway_clients:get_client_stream(Pid, NodeId),
        [?_assertEqual(Stream, StoredStream)]
     end}.

%% @doc Test get_client_stream returns not_found for unknown node
get_client_stream_not_found_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        [?_assertEqual(not_found, macula_gateway_clients:get_client_stream(Pid, <<"unknown">>))]
     end}.

%% @doc Test store_client_stream updates existing stream
store_client_stream_update_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        NodeId = <<"node123">>,
        Stream1 = spawn(fun() -> timer:sleep(1000) end),
        Stream2 = spawn(fun() -> timer:sleep(1000) end),

        ok = macula_gateway_clients:store_client_stream(Pid, NodeId, Stream1),
        ok = macula_gateway_clients:store_client_stream(Pid, NodeId, Stream2),

        {ok, StoredStream} = macula_gateway_clients:get_client_stream(Pid, NodeId),
        [?_assertEqual(Stream2, StoredStream)]
     end}.

%%%===================================================================
%%% Edge Cases and Error Handling
%%%===================================================================

%% @doc Test concurrent client connections don't race
concurrent_connections_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        %% Spawn multiple clients concurrently
        Clients = [spawn(fun() -> timer:sleep(1000) end) || _ <- lists:seq(1, 10)],

        %% Connect all concurrently
        [macula_gateway_clients:client_connected(Pid, C,
            #{realm => <<"test">>, node_id => integer_to_binary(I)})
         || {C, I} <- lists:zip(Clients, lists:seq(1, 10))],

        {ok, AllClients} = macula_gateway_clients:get_all_clients(Pid),
        [?_assertEqual(10, length(AllClients))]
     end}.

%% @doc Test manager handles client crash gracefully
client_crash_cleanup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        ClientPid = spawn(fun() -> timer:sleep(50) end),
        ClientInfo = #{realm => <<"test">>, node_id => <<"crasher">>},

        macula_gateway_clients:client_connected(Pid, ClientPid, ClientInfo),

        %% Kill client brutally
        exit(ClientPid, kill),
        timer:sleep(100),

        %% Should be cleaned up
        {ok, Clients} = macula_gateway_clients:get_all_clients(Pid),
        [?_assertEqual([], Clients)]
     end}.

%% @doc Test stream cleanup when client disconnects
stream_cleanup_on_disconnect_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        ClientPid = spawn(fun() -> timer:sleep(5000) end),
        NodeId = <<"node-with-stream">>,
        ClientInfo = #{realm => <<"test">>, node_id => NodeId},
        StreamPid = spawn(fun() -> timer:sleep(5000) end),

        %% Register client and store stream
        ok = macula_gateway_clients:client_connected(Pid, ClientPid, ClientInfo),
        ok = macula_gateway_clients:store_client_stream(Pid, NodeId, StreamPid),

        %% Verify stream exists
        {ok, _Stream} = macula_gateway_clients:get_client_stream(Pid, NodeId),

        %% Disconnect client (explicit disconnection)
        ok = macula_gateway_clients:client_disconnected(Pid, ClientPid),

        %% Stream should also be cleaned up
        [?_assertEqual(not_found, macula_gateway_clients:get_client_stream(Pid, NodeId))]
     end}.

%% @doc Test stream cleanup when client crashes
stream_cleanup_on_crash_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        ClientPid = spawn(fun() -> timer:sleep(100) end),
        NodeId = <<"node-crash-test">>,
        ClientInfo = #{realm => <<"test">>, node_id => NodeId},
        StreamPid = spawn(fun() -> timer:sleep(5000) end),

        %% Register client and store stream
        ok = macula_gateway_clients:client_connected(Pid, ClientPid, ClientInfo),
        ok = macula_gateway_clients:store_client_stream(Pid, NodeId, StreamPid),

        %% Verify stream exists
        {ok, _Stream} = macula_gateway_clients:get_client_stream(Pid, NodeId),

        %% Kill client process
        exit(ClientPid, kill),
        timer:sleep(150),

        %% Stream should be automatically cleaned up via DOWN message
        [?_assertEqual(not_found, macula_gateway_clients:get_client_stream(Pid, NodeId))]
     end}.

%%%===================================================================
%%% Client Limits Tests
%%%===================================================================

%% @doc Test max_clients limit enforcement
max_clients_limit_enforced_test_() ->
    {setup,
     fun() ->
        %% Setup with small max_clients for testing
        Opts = #{max_clients => 3},
        {ok, Pid} = macula_gateway_clients:start_link(Opts),
        Pid
     end,
     fun cleanup/1,
     fun(Pid) ->
        %% Create 3 clients (fill pool)
        Client1 = spawn(fun() -> timer:sleep(5000) end),
        Client2 = spawn(fun() -> timer:sleep(5000) end),
        Client3 = spawn(fun() -> timer:sleep(5000) end),

        ok = macula_gateway_clients:client_connected(Pid, Client1,
            #{realm => <<"test">>, node_id => <<"node1">>}),
        ok = macula_gateway_clients:client_connected(Pid, Client2,
            #{realm => <<"test">>, node_id => <<"node2">>}),
        ok = macula_gateway_clients:client_connected(Pid, Client3,
            #{realm => <<"test">>, node_id => <<"node3">>}),

        %% Verify pool size = 3
        {ok, Clients} = macula_gateway_clients:get_all_clients(Pid),
        PoolSize1 = length(Clients),

        %% Try to add 4th client (should be rejected)
        Client4 = spawn(fun() -> timer:sleep(5000) end),
        Result = macula_gateway_clients:client_connected(Pid, Client4,
            #{realm => <<"test">>, node_id => <<"node4">>}),

        %% Pool should still be 3 (4th client rejected)
        {ok, Clients2} = macula_gateway_clients:get_all_clients(Pid),
        PoolSize2 = length(Clients2),

        [
            ?_assertEqual(3, PoolSize1),
            ?_assertEqual({error, max_clients_reached}, Result),
            ?_assertEqual(3, PoolSize2)
        ]
     end}.

%% @doc Test updating existing client when pool is full
update_client_when_pool_full_test_() ->
    {setup,
     fun() ->
        Opts = #{max_clients => 2},
        {ok, Pid} = macula_gateway_clients:start_link(Opts),
        Pid
     end,
     fun cleanup/1,
     fun(Pid) ->
        %% Fill pool with 2 clients
        Client1 = spawn(fun() -> timer:sleep(5000) end),
        Client2 = spawn(fun() -> timer:sleep(5000) end),

        ok = macula_gateway_clients:client_connected(Pid, Client1,
            #{realm => <<"test">>, node_id => <<"node1">>}),
        ok = macula_gateway_clients:client_connected(Pid, Client2,
            #{realm => <<"test">>, node_id => <<"node2">>}),

        %% Update existing client (should work even when pool is full)
        UpdateResult = macula_gateway_clients:client_connected(Pid, Client1,
            #{realm => <<"updated">>, node_id => <<"node1_updated">>}),

        %% Verify update worked
        {ok, UpdatedInfo} = macula_gateway_clients:get_client_info(Pid, Client1),
        #{realm := UpdatedRealm} = UpdatedInfo,

        %% Pool should still be 2
        {ok, Clients} = macula_gateway_clients:get_all_clients(Pid),

        [
            ?_assertEqual(ok, UpdateResult),
            ?_assertEqual(<<"updated">>, UpdatedRealm),
            ?_assertEqual(2, length(Clients))
        ]
     end}.
