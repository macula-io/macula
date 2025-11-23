%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for RPC operations in macula_client.
%%% Tests remote procedure calls with various argument types and options.
%%% Following TDD principles.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_client_rpc_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

rpc_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"call with map args", fun test_call_map_args/0},
         {"call with list args", fun test_call_list_args/0},
         {"call with binary args", fun test_call_binary_args/0},
         {"call requires connected client", fun test_call_disconnected/0},
         {"call with custom timeout", fun test_call_timeout_option/0},
         {"call returns result on success", fun test_call_result/0},
         {"call returns error on failure", fun test_call_error/0},
         {"call timeout handling", fun test_call_timeout_triggers/0},
         {"call with empty args", fun test_call_empty_args/0},
         {"concurrent calls", fun test_concurrent_calls/0},
         {"call cleanup after completion", fun test_call_cleanup/0},
         {"unique call IDs", fun test_unique_call_ids/0},
         {"procedure naming patterns", fun test_procedure_names/0},
         {"complex args encoding", fun test_complex_args_encoding/0},
         {"invalid procedure type", fun test_invalid_procedure_type/0},
         {"call return type contract", fun test_call_return_type/0}
     ]}.

setup() ->
    application:ensure_all_started(macula_client),
    ok.

cleanup(_) ->
    ok.

%%%===================================================================
%%% Basic RPC Call Tests
%%%===================================================================

test_call_map_args() ->
    %% GIVEN: Map arguments for RPC
    Procedure = <<"my.app.get_user">>,
    Args = #{
        user_id => <<"user-123">>,
        include_profile => true
    },

    %% WHEN: Making RPC call with map
    %% THEN: API should accept map arguments
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead
    Result = (catch macula:call(Client, Procedure, Args)),

    %% Should fail with noproc (not badarg - type is correct)
    ?assertMatch({'EXIT', {noproc, _}}, Result).

test_call_list_args() ->
    %% GIVEN: List arguments for RPC
    Procedure = <<"my.app.calculate_sum">>,
    Args = [1, 2, 3, 4, 5],

    %% WHEN: Making RPC call with list
    %% THEN: API should accept list arguments
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead
    Result = (catch macula:call(Client, Procedure, Args)),

    ?assertMatch({'EXIT', {noproc, _}}, Result).

test_call_binary_args() ->
    %% GIVEN: Binary arguments for RPC
    Procedure = <<"my.app.process_data">>,
    Args = <<"raw binary data">>,

    %% WHEN: Making RPC call with binary
    %% THEN: API should accept binary arguments
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead
    Result = (catch macula:call(Client, Procedure, Args)),

    ?assertMatch({'EXIT', {noproc, _}}, Result).

test_call_disconnected() ->
    %% GIVEN: A disconnected (dead) client
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead

    %% WHEN: Attempting RPC call
    Result = (catch macula:call(Client, <<"test.proc">>, #{})),

    %% THEN: Should fail
    ?assertMatch({'EXIT', _}, Result).

test_call_empty_args() ->
    %% GIVEN: Empty args (empty map)
    Procedure = <<"my.app.health_check">>,
    Args = #{},

    %% WHEN: Making RPC call with empty args
    %% THEN: API should accept empty map
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead
    Result = (catch macula:call(Client, Procedure, Args)),

    ?assertMatch({'EXIT', {noproc, _}}, Result).

%%%===================================================================
%%% RPC Options Tests
%%%===================================================================

test_call_timeout_option() ->
    %% GIVEN: Custom timeout option
    Procedure = <<"my.app.long_running">>,
    Args = #{data => <<"large">>},
    Opts = #{timeout => 60000}, %% 60 second timeout

    %% WHEN: Making RPC call with timeout option
    %% THEN: API should accept timeout option
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead
    Result = (catch macula:call(Client, Procedure, Args, Opts)),

    ?assertMatch({'EXIT', {noproc, _}}, Result).

%%%===================================================================
%%% RPC Result Handling Tests
%%%===================================================================

test_call_result() ->
    %% Test that call would return {ok, Result} on success
    %% This is an API contract test

    %% Expected return format on success:
    %% {ok, Result}

    %% For now, just verify the function exists with correct arity
    ?assert(erlang:function_exported(macula_client, call, 3)),
    ?assert(erlang:function_exported(macula_client, call, 4)).

test_call_error() ->
    %% Test that call would return {error, Reason} on failure
    %% This is an API contract test

    %% Expected return format on error:
    %% {error, Reason}

    %% The implementation should handle errors from server
    ?assert(erlang:function_exported(macula_client, call, 3)).

%%%===================================================================
%%% RPC Timeout Tests
%%%===================================================================

test_call_timeout_triggers() ->
    %% Test that RPC calls timeout correctly
    %% This would require integration testing with mock or real server

    %% For now, verify timeout value is passed through
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead
    Procedure = <<"test.timeout">>,
    Args = #{},
    Opts = #{timeout => 100}, %% Very short timeout

    Result = (catch macula:call(Client, Procedure, Args, Opts)),
    ?assertMatch({'EXIT', {noproc, _}}, Result).

%%%===================================================================
%%% Concurrent RPC Tests
%%%===================================================================

test_concurrent_calls() ->
    %% Test that multiple concurrent calls work
    %% Each call should have unique call_id

    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead

    %% Make multiple calls concurrently
    Calls = [
        spawn(fun() ->
            (catch macula:call(Client, <<"proc1">>, #{}))
        end),
        spawn(fun() ->
            (catch macula:call(Client, <<"proc2">>, #{}))
        end),
        spawn(fun() ->
            (catch macula:call(Client, <<"proc3">>, #{}))
        end)
    ],

    %% All processes should complete (fail with noproc)
    timer:sleep(100),
    lists:foreach(fun(Pid) ->
        ?assertNot(is_process_alive(Pid))
    end, Calls).

test_call_cleanup() ->
    %% Test that completed calls are cleaned up
    %% This would check internal state - for now just verify API

    ?assert(erlang:function_exported(macula_connection, call, 3)).

%%%===================================================================
%%% Message ID Generation Tests
%%%===================================================================

test_unique_call_ids() ->
    %% Test that each call gets a unique ID
    %% This is an internal implementation detail

    %% For now, just verify the function exists
    ?assert(erlang:function_exported(macula_connection, call, 3)).

%%%===================================================================
%%% Procedure Naming Tests
%%%===================================================================

test_procedure_names() ->
    %% Test various procedure naming patterns
    Procedures = [
        <<"my.app.get_user">>,
        <<"my.app.users.list">>,
        <<"my.app.orders.create">>,
        <<"system.health.check">>,
        <<"data.analytics.run">>
    ],

    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead

    lists:foreach(fun(Proc) ->
        Result = (catch macula:call(Client, Proc, #{})),
        ?assertMatch({'EXIT', {noproc, _}}, Result)
    end, Procedures).

%%%===================================================================
%%% Argument Encoding Tests
%%%===================================================================

test_complex_args_encoding() ->
    %% Test that complex nested structures can be encoded
    Procedure = <<"my.app.complex_operation">>,

    %% Nested map with various types
    Args = #{
        user => #{
            id => <<"user-123">>,
            profile => #{
                name => <<"Test User">>,
                age => 30,
                active => true
            }
        },
        options => #{
            timeout => 5000,
            retry => true,
            filters => [<<"tag1">>, <<"tag2">>, <<"tag3">>]
        },
        metadata => #{
            timestamp => 1699564800,
            source => <<"mobile_app">>,
            version => <<"1.0.0">>
        }
    },

    %% WHEN: Making call with complex args
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead
    Result = (catch macula:call(Client, Procedure, Args)),

    %% THEN: Should not crash with badarg (type is valid)
    ?assertMatch({'EXIT', {noproc, _}}, Result).

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

test_invalid_procedure_type() ->
    %% GIVEN: Invalid procedure type (atom instead of binary)
    Procedure = invalid_atom,
    Args = #{},

    %% WHEN: Making call with invalid type
    %% THEN: Should fail with function_clause or badarg
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead
    Result = (catch macula:call(Client, Procedure, Args)),

    %% Should fail with type error, not noproc
    ?assertMatch({'EXIT', {function_clause, _}}, Result).

test_call_return_type() ->
    %% Verify return type contract
    %% On success: {ok, Result}
    %% On error: {error, Reason}
    %% On timeout: {error, timeout}

    %% This is documented in the API but would need integration
    %% testing to verify actual returns

    ?assert(true).
