%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests for macula_connection module.
%%%
%%% These tests require Docker and a running QUIC gateway.
%%% Tests will be skipped if Docker is not available.
%%%
%%% To run:
%%%   docker compose -f docker/docker-compose.integration-test.yml up -d
%%%   rebar3 eunit --module=macula_connection_integration_tests
%%%   docker compose -f docker/docker-compose.integration-test.yml down
%%% @end
%%%-------------------------------------------------------------------
-module(macula_connection_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test fixtures
-define(TEST_REALM, <<"com.example.realm">>).
-define(GATEWAY_URL, <<"https://localhost:19443">>).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Helper function to safely stop a connection without blocking tests
%% Spawns stop in separate process with short timeout, then kills if needed
safe_stop(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        false ->
            ok;
        true ->
            %% Try to stop gracefully with short timeout
            spawn(fun() ->
                catch macula_connection:stop(Pid)
            end),
            timer:sleep(100),
            %% If still alive, kill it
            case is_process_alive(Pid) of
                true -> exit(Pid, kill), timer:sleep(10);
                false -> ok
            end
    end,
    ok;
safe_stop(_) ->
    ok.

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% Fixture for all integration tests - starts/stops Docker gateway
integration_test_() ->
    {setup,
     fun setup_all/0,
     fun cleanup_all/1,
     fun (SetupResult) ->
        case SetupResult of
            {ok, _} ->
                %% Docker is available and gateway started
                [
                    {"Connection lifecycle tests", connection_lifecycle_tests()},
                    {"Pub/sub tests", pubsub_tests()},
                    {"RPC tests", rpc_tests()}
                ];
            {skipped, Reason} ->
                %% Docker not available - skip tests
                {skipped, Reason}
        end
     end}.

setup_all() ->
    case macula_integration_test_helpers:is_docker_available() of
        false ->
            io:format("~nSkipping integration tests: Docker not available~n"),
            {skipped, docker_not_available};
        true ->
            io:format("~nStarting test gateway...~n"),
            case macula_integration_test_helpers:start_test_gateway() of
                ok ->
                    io:format("Test gateway ready!~n"),
                    {ok, gateway_started};
                {error, Reason} ->
                    io:format("Failed to start gateway: ~p~n", [Reason]),
                    {skipped, {gateway_start_failed, Reason}}
            end
    end.

cleanup_all({ok, _}) ->
    io:format("~nStopping test gateway...~n"),
    macula_integration_test_helpers:stop_test_gateway(),
    ok;
cleanup_all(_) ->
    ok.

%%%===================================================================
%%% Connection Lifecycle Tests
%%%===================================================================

connection_lifecycle_tests() ->
    [
        {"Connect and disconnect", fun test_connect_disconnect/0},
        {"Connection status after connect", fun test_connection_status_connected/0},
        {"Multiple connections", fun test_multiple_connections/0},
        {"Connection with custom node_id", fun test_connection_custom_node_id/0}
    ].

test_connect_disconnect() ->
    %% Create connection
    {ok, Conn} = macula_integration_test_helpers:create_test_connection(),
    ?assert(is_pid(Conn)),
    ?assert(is_process_alive(Conn)),

    %% Wait a bit for connection to establish
    timer:sleep(2000),

    %% Disconnect
    ok = safe_stop(Conn),
    timer:sleep(100),

    %% Process should be dead
    ?assertNot(is_process_alive(Conn)).

test_connection_status_connected() ->
    {ok, Conn} = macula_integration_test_helpers:create_test_connection(),

    %% Wait for connection to establish
    timer:sleep(3000),

    %% Connection should be alive
    ?assert(is_process_alive(Conn)),

    %% Cleanup
    safe_stop(Conn).

test_multiple_connections() ->
    %% Create multiple connections
    {ok, Conn1} = macula_integration_test_helpers:create_test_connection(#{
        node_id => <<"test-conn-1">>
    }),
    {ok, Conn2} = macula_integration_test_helpers:create_test_connection(#{
        node_id => <<"test-conn-2">>
    }),
    {ok, Conn3} = macula_integration_test_helpers:create_test_connection(#{
        node_id => <<"test-conn-3">>
    }),

    %% All should be alive
    ?assert(is_process_alive(Conn1)),
    ?assert(is_process_alive(Conn2)),
    ?assert(is_process_alive(Conn3)),

    %% All should be different processes
    ?assertNot(Conn1 =:= Conn2),
    ?assertNot(Conn2 =:= Conn3),
    ?assertNot(Conn1 =:= Conn3),

    %% Cleanup
    macula_connection:stop(Conn1),
    macula_connection:stop(Conn2),
    macula_connection:stop(Conn3).

test_connection_custom_node_id() ->
    CustomNodeId = <<"my-custom-node-123">>,

    {ok, Conn} = macula_integration_test_helpers:create_test_connection(#{
        node_id => CustomNodeId
    }),

    timer:sleep(2000),

    ?assert(is_process_alive(Conn)),

    %% Cleanup
    safe_stop(Conn).

%%%===================================================================
%%% Pub/Sub Tests (placeholders for now)
%%%===================================================================

pubsub_tests() ->
    [
        {"Publish to connected gateway", fun test_publish_connected/0},
        {"Subscribe and receive message", fun test_subscribe_receive/0}
    ].

test_publish_connected() ->
    {ok, Conn} = macula_integration_test_helpers:create_test_connection(),

    %% Wait for connection to be fully established
    ok = macula_integration_test_helpers:wait_for_connection_ready(Conn),

    %% Publish message
    Result = macula_connection:publish(Conn, <<"test.topic">>, #{
        data => <<"test message">>,
        timestamp => erlang:system_time(second)
    }),

    %% Should succeed (no subscribers, but publish should work)
    ?assertMatch(ok, Result),

    %% Cleanup
    safe_stop(Conn).

test_subscribe_receive() ->
    {ok, Conn} = macula_integration_test_helpers:create_test_connection(),

    %% Wait for connection to be fully established
    ok = macula_integration_test_helpers:wait_for_connection_ready(Conn),

    %% Subscribe to topic
    TestPid = self(),
    Callback = fun(Msg) ->
        TestPid ! {received, Msg},
        ok
    end,

    Result = macula_connection:subscribe(Conn, <<"test.integration.topic">>, Callback),

    %% Subscribe should succeed
    ?assertMatch({ok, _Ref}, Result),

    %% TODO: Publish from another connection and verify receipt
    %% For now, just verify subscription worked

    %% Cleanup
    safe_stop(Conn).

%%%===================================================================
%%% RPC Tests (placeholders for now)
%%%===================================================================

rpc_tests() ->
    [
        {"Call non-existent service", fun test_call_nonexistent_service/0}
        %% TODO: Add test with actual advertised service
    ].

test_call_nonexistent_service() ->
    {ok, Conn} = macula_integration_test_helpers:create_test_connection(),
    timer:sleep(3000),  % Wait for connection

    %% Call service that doesn't exist
    Result = macula_connection:call(Conn, <<"nonexistent.service">>, #{}, #{timeout => 5000}),

    %% Should get error (no providers)
    ?assertMatch({error, _}, Result),

    %% Cleanup
    safe_stop(Conn).
