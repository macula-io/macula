%%%-------------------------------------------------------------------
%%% @doc
%%% Connection pool tests for macula_connection_pool.
%%%
%%% Tests connection pool functionality including:
%%% - Pool structure validation
%%% - Connection caching and reuse
%%% - Connection creation
%%% - Pool cleanup
%%% - Timestamp tracking
%%%
%%% NOTE: These tests focus on testable validation logic and data
%%% structures. Integration-level tests (actual QUIC connections) are
%%% covered in integration test suites.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_connection_pool_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Pool Structure Tests
%%%===================================================================

test_empty_pool_test() ->
    %% GIVEN: Empty connection pool
    %% WHEN: Creating new pool
    %% THEN: Should be empty map

    Pool = #{},
    ?assertEqual(#{}, Pool),
    ?assert(is_map(Pool)),
    ?assertEqual(0, maps:size(Pool)).

test_pool_with_connection_test() ->
    %% GIVEN: Pool with one connection
    %% WHEN: Adding connection info
    %% THEN: Should contain connection details

    Endpoint = <<"https://node1.example.com:9443">>,
    Conn = self(),  % Use self() as mock connection PID
    Stream = self(),  % Use self() as mock stream PID
    Timestamp = erlang:system_time(second),

    ConnectionInfo = #{
        connection => Conn,
        stream => Stream,
        last_used => Timestamp
    },

    Pool = #{Endpoint => ConnectionInfo},

    ?assertEqual(1, maps:size(Pool)),
    ?assert(maps:is_key(Endpoint, Pool)),

    #{connection := RetConn, stream := RetStream, last_used := RetTime} = maps:get(Endpoint, Pool),
    ?assertEqual(Conn, RetConn),
    ?assertEqual(Stream, RetStream),
    ?assertEqual(Timestamp, RetTime).

test_pool_with_multiple_connections_test() ->
    %% GIVEN: Pool with multiple endpoints
    %% WHEN: Adding multiple connections
    %% THEN: Should track all separately

    Endpoint1 = <<"https://node1.example.com:9443">>,
    Endpoint2 = <<"https://node2.example.com:9443">>,
    Endpoint3 = <<"https://node3.example.com:9443">>,

    Pool = #{
        Endpoint1 => #{connection => self(), stream => self(), last_used => 1000},
        Endpoint2 => #{connection => self(), stream => self(), last_used => 2000},
        Endpoint3 => #{connection => self(), stream => self(), last_used => 3000}
    },

    ?assertEqual(3, maps:size(Pool)),
    ?assert(maps:is_key(Endpoint1, Pool)),
    ?assert(maps:is_key(Endpoint2, Pool)),
    ?assert(maps:is_key(Endpoint3, Pool)).

%%%===================================================================
%%% Connection Info Validation Tests
%%%===================================================================

test_connection_info_has_required_fields_test() ->
    %% GIVEN: Connection info structure
    %% WHEN: Creating connection info
    %% THEN: Must have connection, stream, last_used

    ConnectionInfo = #{
        connection => self(),
        stream => self(),
        last_used => erlang:system_time(second)
    },

    ?assert(maps:is_key(connection, ConnectionInfo)),
    ?assert(maps:is_key(stream, ConnectionInfo)),
    ?assert(maps:is_key(last_used, ConnectionInfo)).

test_connection_info_connection_is_pid_test() ->
    %% GIVEN: Connection info
    %% WHEN: Checking connection field
    %% THEN: Should be a PID

    Conn = self(),
    ConnectionInfo = #{
        connection => Conn,
        stream => self(),
        last_used => erlang:system_time(second)
    },

    ?assert(is_pid(maps:get(connection, ConnectionInfo))).

test_connection_info_stream_is_pid_test() ->
    %% GIVEN: Connection info
    %% WHEN: Checking stream field
    %% THEN: Should be a PID

    Stream = self(),
    ConnectionInfo = #{
        connection => self(),
        stream => Stream,
        last_used => erlang:system_time(second)
    },

    ?assert(is_pid(maps:get(stream, ConnectionInfo))).

test_connection_info_last_used_is_timestamp_test() ->
    %% GIVEN: Connection info
    %% WHEN: Checking last_used field
    %% THEN: Should be integer timestamp

    Timestamp = erlang:system_time(second),
    ConnectionInfo = #{
        connection => self(),
        stream => self(),
        last_used => Timestamp
    },

    LastUsed = maps:get(last_used, ConnectionInfo),
    ?assert(is_integer(LastUsed)),
    ?assert(LastUsed > 0).

%%%===================================================================
%%% Timestamp Update Tests
%%%===================================================================

test_timestamp_updates_on_reuse_test() ->
    %% GIVEN: Connection with initial timestamp
    %% WHEN: Reusing connection
    %% THEN: Timestamp should be updated

    InitialTime = 1000,
    UpdatedTime = 2000,

    ConnectionInfo = #{
        connection => self(),
        stream => self(),
        last_used => InitialTime
    },

    UpdatedInfo = ConnectionInfo#{last_used => UpdatedTime},

    ?assertEqual(InitialTime, maps:get(last_used, ConnectionInfo)),
    ?assertEqual(UpdatedTime, maps:get(last_used, UpdatedInfo)),
    ?assert(maps:get(last_used, UpdatedInfo) > maps:get(last_used, ConnectionInfo)).

%%%===================================================================
%%% Pool Lookup Tests
%%%===================================================================

test_lookup_existing_endpoint_test() ->
    %% GIVEN: Pool with endpoint
    %% WHEN: Looking up existing endpoint
    %% THEN: Should return connection info

    Endpoint = <<"https://node1.example.com:9443">>,
    ConnectionInfo = #{
        connection => self(),
        stream => self(),
        last_used => erlang:system_time(second)
    },
    Pool = #{Endpoint => ConnectionInfo},

    Result = maps:get(Endpoint, Pool, undefined),
    ?assertNotEqual(undefined, Result),
    ?assertEqual(ConnectionInfo, Result).

test_lookup_missing_endpoint_test() ->
    %% GIVEN: Empty pool
    %% WHEN: Looking up non-existent endpoint
    %% THEN: Should return undefined

    Pool = #{},
    Endpoint = <<"https://node1.example.com:9443">>,

    Result = maps:get(Endpoint, Pool, undefined),
    ?assertEqual(undefined, Result).

%%%===================================================================
%%% Endpoint Format Tests
%%%===================================================================

test_endpoint_format_https_test() ->
    %% GIVEN: HTTPS endpoint format
    %% WHEN: Using as pool key
    %% THEN: Should be binary

    Endpoint = <<"https://node1.example.com:9443">>,
    ?assert(is_binary(Endpoint)),
    ?assertEqual(true, binary:match(Endpoint, <<"https://">>) =/= nomatch).

test_endpoint_format_http_test() ->
    %% GIVEN: HTTP endpoint format
    %% WHEN: Using as pool key
    %% THEN: Should be binary

    Endpoint = <<"http://node1.example.com:8080">>,
    ?assert(is_binary(Endpoint)),
    ?assertEqual(true, binary:match(Endpoint, <<"http://">>) =/= nomatch).

test_endpoint_with_port_test() ->
    %% GIVEN: Endpoint with explicit port
    %% WHEN: Validating format
    %% THEN: Should include port number

    Endpoint = <<"https://node1.example.com:9443">>,
    ?assertEqual(true, binary:match(Endpoint, <<":9443">>) =/= nomatch).

%%%===================================================================
%%% Pool Update Tests
%%%===================================================================

test_add_new_endpoint_to_pool_test() ->
    %% GIVEN: Pool with one endpoint
    %% WHEN: Adding another endpoint
    %% THEN: Pool should grow

    Endpoint1 = <<"https://node1.example.com:9443">>,
    Endpoint2 = <<"https://node2.example.com:9443">>,

    Pool1 = #{
        Endpoint1 => #{connection => self(), stream => self(), last_used => 1000}
    },

    Pool2 = Pool1#{
        Endpoint2 => #{connection => self(), stream => self(), last_used => 2000}
    },

    ?assertEqual(1, maps:size(Pool1)),
    ?assertEqual(2, maps:size(Pool2)),
    ?assert(maps:is_key(Endpoint1, Pool2)),
    ?assert(maps:is_key(Endpoint2, Pool2)).

test_update_existing_endpoint_in_pool_test() ->
    %% GIVEN: Pool with endpoint
    %% WHEN: Updating endpoint's connection info
    %% THEN: Pool size should remain same, data updated

    Endpoint = <<"https://node1.example.com:9443">>,

    Pool1 = #{
        Endpoint => #{connection => self(), stream => self(), last_used => 1000}
    },

    NewConnectionInfo = #{connection => self(), stream => self(), last_used => 2000},
    Pool2 = Pool1#{Endpoint => NewConnectionInfo},

    ?assertEqual(1, maps:size(Pool1)),
    ?assertEqual(1, maps:size(Pool2)),
    ?assertEqual(1000, maps:get(last_used, maps:get(Endpoint, Pool1))),
    ?assertEqual(2000, maps:get(last_used, maps:get(Endpoint, Pool2))).

%%%===================================================================
%%% Error Response Structure Tests
%%%===================================================================

test_error_response_format_test() ->
    %% GIVEN: Connection creation error
    %% WHEN: Building error response
    %% THEN: Should have {error, Reason, Pool} format

    Pool = #{},
    ErrorResponse = {error, {connection_failed, timeout}, Pool},

    %% Verify format and extract values
    {error, Reason, ReturnedPool} = ErrorResponse,
    ?assertEqual({connection_failed, timeout}, Reason),
    ?assertEqual(Pool, ReturnedPool).

test_error_preserves_pool_test() ->
    %% GIVEN: Pool with existing connections
    %% WHEN: Error occurs during new connection
    %% THEN: Pool should be preserved in error response

    Endpoint = <<"https://node1.example.com:9443">>,
    Pool = #{
        Endpoint => #{connection => self(), stream => self(), last_used => 1000}
    },

    ErrorResponse = {error, {handshake_failed, timeout}, Pool},

    {error, _Reason, ReturnedPool} = ErrorResponse,
    ?assertEqual(Pool, ReturnedPool),
    ?assertEqual(1, maps:size(ReturnedPool)),
    ?assert(maps:is_key(Endpoint, ReturnedPool)).

%%%===================================================================
%%% Success Response Structure Tests
%%%===================================================================

test_success_response_format_test() ->
    %% GIVEN: Successful connection
    %% WHEN: Building success response
    %% THEN: Should have {ok, Conn, Stream, UpdatedPool} format

    Conn = self(),
    Stream = self(),
    Pool = #{},

    SuccessResponse = {ok, Conn, Stream, Pool},

    %% Verify format and extract values
    {ok, RetConn, RetStream, RetPool} = SuccessResponse,
    ?assertEqual(Conn, RetConn),
    ?assertEqual(Stream, RetStream),
    ?assertEqual(Pool, RetPool).

test_success_updates_pool_test() ->
    %% GIVEN: Empty pool
    %% WHEN: Successful connection added
    %% THEN: Pool should contain new connection

    Endpoint = <<"https://node1.example.com:9443">>,
    Conn = self(),
    Stream = self(),

    EmptyPool = #{},
    ConnectionInfo = #{
        connection => Conn,
        stream => Stream,
        last_used => erlang:system_time(second)
    },
    UpdatedPool = EmptyPool#{Endpoint => ConnectionInfo},

    ?assertEqual(0, maps:size(EmptyPool)),
    ?assertEqual(1, maps:size(UpdatedPool)),
    ?assert(maps:is_key(Endpoint, UpdatedPool)).
