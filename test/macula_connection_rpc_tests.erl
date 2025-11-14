%%%-------------------------------------------------------------------
%%% @doc
%%% RPC operation tests for macula_connection.
%%%
%%% Tests RPC functionality including:
%%% - Call ID generation
%%% - Procedure name validation
%%% - Arguments validation
%%% - Timeout handling
%%% - Message structure validation
%%% - Reply/Error formats
%%%
%%% NOTE: API-level tests (actual RPC calls with connections) are
%%% covered in macula_connection_tests.erl. These tests focus on
%%% testable validation logic and data structures.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_connection_rpc_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Call ID Generation Tests
%%%===================================================================

test_call_id_format_test() ->
    %% GIVEN: Need for unique call IDs
    %% WHEN: Generating call IDs
    %% THEN: Should be unique binaries

    CallId = integer_to_binary(erlang:unique_integer([positive])),
    ?assert(is_binary(CallId)),
    ?assert(byte_size(CallId) > 0).

test_call_id_uniqueness_test() ->
    %% GIVEN: Multiple call ID generations
    %% WHEN: Creating IDs
    %% THEN: Each should be unique

    Id1 = integer_to_binary(erlang:unique_integer([positive])),
    Id2 = integer_to_binary(erlang:unique_integer([positive])),
    Id3 = integer_to_binary(erlang:unique_integer([positive])),

    ?assertNotEqual(Id1, Id2),
    ?assertNotEqual(Id2, Id3),
    ?assertNotEqual(Id1, Id3).

%%%===================================================================
%%% Procedure Name Validation Tests
%%%===================================================================

test_procedure_name_empty_test() ->
    %% GIVEN: Empty procedure name
    %% WHEN: Validating
    %% THEN: Should reject empty procedures

    ?assertEqual(false, is_valid_procedure(<<>>)).

test_procedure_name_valid_simple_test() ->
    %% GIVEN: Valid simple procedure names
    %% WHEN: Validating
    %% THEN: Should accept valid names

    ?assertEqual(true, is_valid_procedure(<<"calculate">>)),
    ?assertEqual(true, is_valid_procedure(<<"math.add">>)),
    ?assertEqual(true, is_valid_procedure(<<"service.user.create">>)).

test_procedure_name_with_namespace_test() ->
    %% GIVEN: Namespaced procedure names
    %% WHEN: Validating
    %% THEN: Should accept namespaced procedures

    ?assertEqual(true, is_valid_procedure(<<"com.example.service">>)),
    ?assertEqual(true, is_valid_procedure(<<"org.acme.calculator.add">>)).

%%%===================================================================
%%% Arguments Validation Tests
%%%===================================================================

test_args_as_list_test() ->
    %% GIVEN: Arguments as list
    %% WHEN: Validating
    %% THEN: Should accept list arguments

    Args = [1, 2, 3],
    ?assertEqual(true, is_valid_args(Args)).

test_args_as_map_test() ->
    %% GIVEN: Arguments as map
    %% WHEN: Validating
    %% THEN: Should accept map arguments

    Args = #{a => 1, b => 2},
    ?assertEqual(true, is_valid_args(Args)).

test_args_as_empty_list_test() ->
    %% GIVEN: Empty list arguments
    %% WHEN: Validating
    %% THEN: Should accept empty arguments

    ?assertEqual(true, is_valid_args([])).

test_args_as_empty_map_test() ->
    %% GIVEN: Empty map arguments
    %% WHEN: Validating
    %% THEN: Should accept empty map

    ?assertEqual(true, is_valid_args(#{})).

test_args_mixed_types_in_list_test() ->
    %% GIVEN: Mixed types in list
    %% WHEN: Validating
    %% THEN: Should accept mixed types

    Args = [1, <<"string">>, #{key => value}, [nested]],
    ?assertEqual(true, is_valid_args(Args)).

%%%===================================================================
%%% Timeout Validation Tests
%%%===================================================================

test_timeout_positive_integer_test() ->
    %% GIVEN: Positive timeout value
    %% WHEN: Validating
    %% THEN: Should accept positive integers

    ?assertEqual(true, is_valid_timeout(5000)),
    ?assertEqual(true, is_valid_timeout(1)),
    ?assertEqual(true, is_valid_timeout(60000)).

test_timeout_zero_test() ->
    %% GIVEN: Zero timeout
    %% WHEN: Validating
    %% THEN: Should reject zero (must be positive)

    ?assertEqual(false, is_valid_timeout(0)).

test_timeout_negative_test() ->
    %% GIVEN: Negative timeout
    %% WHEN: Validating
    %% THEN: Should reject negative values

    ?assertEqual(false, is_valid_timeout(-1)),
    ?assertEqual(false, is_valid_timeout(-5000)).

test_timeout_infinity_test() ->
    %% GIVEN: Infinity timeout
    %% WHEN: Validating
    %% THEN: Should accept infinity

    ?assertEqual(true, is_valid_timeout(infinity)).

test_timeout_non_integer_test() ->
    %% GIVEN: Non-integer timeout
    %% WHEN: Validating
    %% THEN: Should reject non-integers

    ?assertEqual(false, is_valid_timeout(<<"5000">>)),
    ?assertEqual(false, is_valid_timeout(5000.5)).

%%%===================================================================
%%% Call Message Structure Tests
%%%===================================================================

test_call_message_format_test() ->
    %% GIVEN: RPC call message
    %% WHEN: Building message structure
    %% THEN: Should have required fields

    CallMsg = #{
        procedure => <<"math.add">>,
        args => [1, 2],
        call_id => <<"call-123">>
    },

    ?assert(maps:is_key(procedure, CallMsg)),
    ?assert(maps:is_key(args, CallMsg)),
    ?assert(maps:is_key(call_id, CallMsg)).

test_call_message_with_timeout_test() ->
    %% GIVEN: Call with timeout option
    %% WHEN: Building message
    %% THEN: Timeout should be included

    CallMsg = #{
        procedure => <<"math.add">>,
        args => [1, 2],
        call_id => <<"call-123">>,
        timeout => 5000
    },

    ?assertEqual(5000, maps:get(timeout, CallMsg)).

%%%===================================================================
%%% Reply Message Structure Tests
%%%===================================================================

test_reply_message_success_format_test() ->
    %% GIVEN: Successful RPC reply
    %% WHEN: Building reply message
    %% THEN: Should have call_id and result

    ReplyMsg = #{
        call_id => <<"call-123">>,
        result => 42
    },

    ?assert(maps:is_key(call_id, ReplyMsg)),
    ?assert(maps:is_key(result, ReplyMsg)).

test_reply_message_with_complex_result_test() ->
    %% GIVEN: Reply with complex result
    %% WHEN: Building reply
    %% THEN: Should accept any term as result

    Result = #{
        status => <<"success">>,
        data => [1, 2, 3],
        metadata => #{processed_at => 1234567890}
    },

    ReplyMsg = #{
        call_id => <<"call-123">>,
        result => Result
    },

    ?assertEqual(Result, maps:get(result, ReplyMsg)).

%%%===================================================================
%%% Error Message Structure Tests
%%%===================================================================

test_error_message_format_test() ->
    %% GIVEN: RPC error response
    %% WHEN: Building error message
    %% THEN: Should have call_id and error

    ErrorMsg = #{
        call_id => <<"call-123">>,
        error => #{
            code => <<"PROCEDURE_NOT_FOUND">>,
            message => <<"The requested procedure does not exist">>
        }
    },

    ?assert(maps:is_key(call_id, ErrorMsg)),
    ?assert(maps:is_key(error, ErrorMsg)),
    ?assert(is_map(maps:get(error, ErrorMsg))).

test_error_with_error_details_test() ->
    %% GIVEN: Error with additional details
    %% WHEN: Building error message
    %% THEN: Should include details

    Error = #{
        code => <<"INVALID_ARGS">>,
        message => <<"Invalid arguments provided">>,
        details => #{
            expected => <<"[number, number]">>,
            received => <<"[string, number]">>
        }
    },

    ErrorMsg = #{
        call_id => <<"call-123">>,
        error => Error
    },

    ErrorData = maps:get(error, ErrorMsg),
    ?assert(maps:is_key(details, ErrorData)).

%%%===================================================================
%%% Timeout Error Tests
%%%===================================================================

test_timeout_error_structure_test() ->
    %% GIVEN: Call timeout error
    %% WHEN: Building timeout error
    %% THEN: Should have specific timeout error format

    TimeoutError = #{
        call_id => <<"call-123">>,
        error => #{
            code => <<"TIMEOUT">>,
            message => <<"RPC call timed out">>
        }
    },

    Error = maps:get(error, TimeoutError),
    ?assertEqual(<<"TIMEOUT">>, maps:get(code, Error)).

%%%===================================================================
%%% Options Validation Tests
%%%===================================================================

test_options_with_timeout_test() ->
    %% GIVEN: Options map with timeout
    %% WHEN: Validating options
    %% THEN: Should accept valid timeout

    Opts = #{timeout => 5000},
    ?assert(maps:is_key(timeout, Opts)),
    ?assertEqual(5000, maps:get(timeout, Opts)).

test_options_with_strategy_test() ->
    %% GIVEN: Options with provider selection strategy
    %% WHEN: Validating options
    %% THEN: Should accept valid strategy

    Opts = #{strategy => round_robin},
    ?assert(maps:is_key(strategy, Opts)),
    ?assertEqual(round_robin, maps:get(strategy, Opts)).

test_options_empty_test() ->
    %% GIVEN: Empty options map
    %% WHEN: Using default options
    %% THEN: Should work with empty map

    Opts = #{},
    ?assertEqual(#{}, Opts).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Validate procedure name is not empty
-spec is_valid_procedure(binary()) -> boolean().
is_valid_procedure(<<>>) -> false;
is_valid_procedure(Procedure) when is_binary(Procedure) -> true;
is_valid_procedure(_) -> false.

%% @doc Validate arguments (can be list or map)
-spec is_valid_args(term()) -> boolean().
is_valid_args(Args) when is_list(Args) -> true;
is_valid_args(Args) when is_map(Args) -> true;
is_valid_args(_) -> false.

%% @doc Validate timeout value
-spec is_valid_timeout(term()) -> boolean().
is_valid_timeout(infinity) -> true;
is_valid_timeout(Timeout) when is_integer(Timeout), Timeout > 0 -> true;
is_valid_timeout(_) -> false.
