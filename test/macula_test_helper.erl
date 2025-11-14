%%%-------------------------------------------------------------------
%%% @doc
%%% Test helper utilities for Macula test suites.
%%%
%%% Provides common setup/teardown, mock management, and test utilities
%%% for all Macula tests.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_test_helper).

-export([
    setup_test_env/0,
    cleanup_test_env/0,
    start_gproc/0,
    stop_gproc/0,
    setup_quic_mocks/0,
    cleanup_quic_mocks/0,
    mock_quic_connection/0,
    mock_quic_stream/0,
    generate_test_node_id/0,
    generate_test_message_id/0
]).

%%%===================================================================
%%% Test Environment Setup/Cleanup
%%%===================================================================

%% @doc Set up complete test environment (gproc + mocks)
setup_test_env() ->
    start_gproc(),
    setup_quic_mocks(),
    ok.

%% @doc Clean up complete test environment
cleanup_test_env() ->
    cleanup_quic_mocks(),
    stop_gproc(),
    ok.

%%%===================================================================
%%% gproc Management
%%%===================================================================

%% @doc Start gproc application if not already running
start_gproc() ->
    case application:start(gproc) of
        ok -> ok;
        {error, {already_started, gproc}} -> ok;
        {error, Reason} ->
            error_logger:warning_msg("Failed to start gproc: ~p", [Reason]),
            ok
    end.

%% @doc Stop gproc application
stop_gproc() ->
    case application:stop(gproc) of
        ok -> ok;
        {error, {not_started, gproc}} -> ok;
        {error, Reason} ->
            error_logger:warning_msg("Failed to stop gproc: ~p", [Reason]),
            ok
    end.

%%%===================================================================
%%% QUIC Mock Management
%%%===================================================================

%% @doc Set up mock for macula_quic module
%% Mocks all QUIC operations to return success without actual network I/O
setup_quic_mocks() ->
    %% Only mock if meck is available
    case code:is_loaded(meck) of
        {file, _} ->
            do_setup_quic_mocks();
        false ->
            %% meck not loaded, skip mocking
            ok
    end.

do_setup_quic_mocks() ->
    %% Clean up any existing mocks first
    catch meck:unload(macula_quic),

    %% Create new mock
    meck:new(macula_quic, [passthrough]),

    %% Mock connect/3 - returns a mock connection pid
    meck:expect(macula_quic, connect,
        fun(_Host, _Port, _Opts) ->
            {ok, mock_quic_connection()}
        end),

    %% Mock open_stream/2 - returns a mock stream pid
    meck:expect(macula_quic, open_stream,
        fun(_Connection, _Opts) ->
            {ok, mock_quic_stream()}
        end),

    %% Mock send/2 - always succeeds
    meck:expect(macula_quic, send,
        fun(_Stream, _Data) ->
            ok
        end),

    %% Mock close_stream/1 - always succeeds
    meck:expect(macula_quic, close_stream,
        fun(_Stream) ->
            ok
        end),

    %% Mock close_connection/1 - always succeeds
    meck:expect(macula_quic, close_connection,
        fun(_Connection) ->
            ok
        end),

    ok.

%% @doc Clean up QUIC mocks
cleanup_quic_mocks() ->
    case code:is_loaded(meck) of
        {file, _} ->
            catch meck:unload(macula_quic),
            ok;
        false ->
            ok
    end.

%%%===================================================================
%%% Mock Object Generators
%%%===================================================================

%% @doc Generate a mock QUIC connection pid
%% Returns a unique pid-like reference that can be used in tests
mock_quic_connection() ->
    spawn(fun() ->
        receive
            stop -> ok
        end
    end).

%% @doc Generate a mock QUIC stream pid
%% Returns a unique pid-like reference that can be used in tests
mock_quic_stream() ->
    spawn(fun() ->
        receive
            stop -> ok
        end
    end).

%%%===================================================================
%%% Test Data Generators
%%%===================================================================

%% @doc Generate a test node ID (32 bytes)
generate_test_node_id() ->
    crypto:strong_rand_bytes(32).

%% @doc Generate a test message ID
generate_test_message_id() ->
    integer_to_binary(erlang:unique_integer([positive])).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Note: Add more helper functions as needed during test development
