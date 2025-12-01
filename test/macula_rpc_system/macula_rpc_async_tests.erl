%%%-------------------------------------------------------------------
%%% @doc Unit tests for macula_rpc_async module.
%%%
%%% Tests cover:
%%% - Callback extraction and invocation
%%% - Request message building
%%% - Reply processing
%%% - RTT calculation
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_async_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Callback Management Tests
%%------------------------------------------------------------------------------

callback_test_() ->
    [
     {"get_callback returns pid callback when no callback in opts", fun test_get_callback_default/0},
     {"get_callback returns fun callback when function provided", fun test_get_callback_fun/0},
     {"invoke_callback sends message for pid callback", fun test_invoke_pid_callback/0},
     {"invoke_callback calls function for fun callback", fun test_invoke_fun_callback/0}
    ].

test_get_callback_default() ->
    CallerPid = self(),
    Opts = #{},
    Result = macula_rpc_async:get_callback(Opts, CallerPid),
    ?assertEqual({pid_cb, CallerPid}, Result).

test_get_callback_fun() ->
    CallerPid = self(),
    Fun = fun(_Result) -> ok end,
    Opts = #{callback => Fun},
    Result = macula_rpc_async:get_callback(Opts, CallerPid),
    ?assertMatch({fun_cb, _}, Result),
    {fun_cb, ExtractedFun} = Result,
    ?assert(is_function(ExtractedFun)).

test_invoke_pid_callback() ->
    RequestId = <<"test-request-123">>,
    ExpectedResult = {ok, <<"success">>},

    ok = macula_rpc_async:invoke_callback({pid_cb, self()}, RequestId, ExpectedResult),

    %% Should receive the message
    receive
        {rpc_reply, RecvRequestId, RecvResult} ->
            ?assertEqual(RequestId, RecvRequestId),
            ?assertEqual(ExpectedResult, RecvResult)
    after 1000 ->
        ?assert(false, "Did not receive rpc_reply message")
    end.

test_invoke_fun_callback() ->
    RequestId = <<"test-request-456">>,
    ExpectedResult = {ok, <<"callback-result">>},
    TestPid = self(),

    %% Function that sends result to test process
    Fun = fun(Result) ->
        TestPid ! {callback_invoked, Result}
    end,

    ok = macula_rpc_async:invoke_callback({fun_cb, Fun}, RequestId, ExpectedResult),

    %% Should receive callback invocation (spawned, so wait a bit)
    receive
        {callback_invoked, RecvResult} ->
            ?assertEqual(ExpectedResult, RecvResult)
    after 1000 ->
        ?assert(false, "Callback was not invoked")
    end.

%%------------------------------------------------------------------------------
%% Request Building Tests
%%------------------------------------------------------------------------------

request_building_test_() ->
    [
     {"build_request_message creates proper message structure", fun test_build_request_message/0},
     {"build_request_message includes timestamp", fun test_build_request_message_timestamp/0},
     {"get_local_endpoint returns valid endpoint format", fun test_get_local_endpoint/0}
    ].

test_build_request_message() ->
    RequestId = <<"req-001">>,
    Procedure = <<"math.add">>,
    EncodedArgs = <<"{\"a\": 1, \"b\": 2}">>,
    FromNodeId = <<"node-001">>,
    Realm = <<"test-realm">>,

    Result = macula_rpc_async:build_request_message(RequestId, Procedure, EncodedArgs, FromNodeId, Realm),

    ?assert(is_map(Result)),
    ?assertEqual(<<"rpc_request">>, maps:get(type, Result)),
    ?assertEqual(RequestId, maps:get(request_id, Result)),
    ?assertEqual(Procedure, maps:get(procedure, Result)),
    ?assertEqual(EncodedArgs, maps:get(args, Result)),
    ?assertEqual(FromNodeId, maps:get(from_node, Result)),
    ?assertEqual(Realm, maps:get(realm, Result)),
    ?assert(maps:is_key(from_endpoint, Result)),
    ?assert(maps:is_key(timestamp, Result)).

test_build_request_message_timestamp() ->
    Before = erlang:system_time(millisecond),
    Result = macula_rpc_async:build_request_message(<<"id">>, <<"proc">>, <<"{}">>, <<"node">>, <<"realm">>),
    After = erlang:system_time(millisecond),

    Timestamp = maps:get(timestamp, Result),
    ?assert(Timestamp >= Before),
    ?assert(Timestamp =< After).

test_get_local_endpoint() ->
    Endpoint = macula_rpc_async:get_local_endpoint(),
    ?assert(is_binary(Endpoint)),
    %% Should contain a colon (host:port format)
    ?assertMatch([_, _], binary:split(Endpoint, <<":">>)).

%%------------------------------------------------------------------------------
%% Reply Processing Tests
%%------------------------------------------------------------------------------

reply_processing_test_() ->
    [
     {"extract_result returns ok with result when present", fun test_extract_result_ok/0},
     {"extract_result returns ok with result using atom key", fun test_extract_result_ok_atom_key/0},
     {"extract_result returns error when no result", fun test_extract_result_error/0},
     {"extract_result returns error with error message", fun test_extract_result_error_with_msg/0},
     {"calculate_rtt returns positive value", fun test_calculate_rtt/0}
    ].

test_extract_result_ok() ->
    Msg = #{<<"result">> => <<"success">>},
    Result = macula_rpc_async:extract_result(Msg),
    ?assertMatch({ok, _}, Result).

test_extract_result_ok_atom_key() ->
    Msg = #{result => <<"atom-key-success">>},
    Result = macula_rpc_async:extract_result(Msg),
    ?assertMatch({ok, _}, Result).

test_extract_result_error() ->
    Msg = #{},
    Result = macula_rpc_async:extract_result(Msg),
    ?assertMatch({error, _}, Result).

test_extract_result_error_with_msg() ->
    ErrorMsg = <<"Service not found">>,
    Msg = #{<<"error">> => ErrorMsg},
    Result = macula_rpc_async:extract_result(Msg),
    ?assertEqual({error, ErrorMsg}, Result).

test_calculate_rtt() ->
    SentAt = erlang:system_time(millisecond) - 100,  %% 100ms ago
    RTT = macula_rpc_async:calculate_rtt(SentAt),
    ?assert(RTT >= 100),
    ?assert(RTT < 200).  %% Shouldn't take more than 100ms to calculate
