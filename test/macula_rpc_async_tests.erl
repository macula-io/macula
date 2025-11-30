%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for macula_rpc_async module.
%%% Tests async RPC helper functions including:
%%% - Callback extraction and invocation
%%% - Request message building
%%% - Reply processing
%%% - RTT calculation
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_async_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Callback Management Tests
%%%===================================================================

get_callback_test_() ->
    [
        {"returns pid callback when no callback in opts",
         fun() ->
             CallerPid = self(),
             Result = macula_rpc_async:get_callback(#{}, CallerPid),
             ?assertEqual({pid_cb, CallerPid}, Result)
         end},

        {"returns fun callback when callback function provided",
         fun() ->
             CallerPid = self(),
             Fun = fun(_Result) -> ok end,
             Result = macula_rpc_async:get_callback(#{callback => Fun}, CallerPid),
             ?assertMatch({fun_cb, _}, Result),
             {fun_cb, ExtractedFun} = Result,
             ?assert(is_function(ExtractedFun))
         end},

        {"returns pid callback for undefined callback option",
         fun() ->
             CallerPid = self(),
             Result = macula_rpc_async:get_callback(#{callback => undefined}, CallerPid),
             ?assertEqual({pid_cb, CallerPid}, Result)
         end}
    ].

invoke_callback_test_() ->
    [
        {"pid callback sends message to pid",
         fun() ->
             RequestId = <<"req-123">>,
             Result = {ok, <<"test result">>},
             ok = macula_rpc_async:invoke_callback({pid_cb, self()}, RequestId, Result),
             receive
                 {rpc_reply, RcvdId, RcvdResult} ->
                     ?assertEqual(RequestId, RcvdId),
                     ?assertEqual(Result, RcvdResult)
             after 100 ->
                 ?assert(false)
             end
         end},

        {"fun callback spawns and executes",
         fun() ->
             Self = self(),
             Fun = fun(R) -> Self ! {callback_received, R} end,
             Result = {ok, <<"callback result">>},
             ok = macula_rpc_async:invoke_callback({fun_cb, Fun}, <<"req-456">>, Result),
             receive
                 {callback_received, RcvdResult} ->
                     ?assertEqual(Result, RcvdResult)
             after 100 ->
                 ?assert(false)
             end
         end},

        {"fun callback error is caught (no crash)",
         fun() ->
             CrashingFun = fun(_R) -> error(intentional_crash) end,
             %% Should not crash, just log
             ok = macula_rpc_async:invoke_callback({fun_cb, CrashingFun}, <<"req-789">>, {ok, test}),
             timer:sleep(50),  %% Give spawned process time to execute
             ?assert(true)  %% If we get here, no crash occurred
         end}
    ].

%%%===================================================================
%%% Request Building Tests
%%%===================================================================

build_request_message_test_() ->
    [
        {"builds message with all required fields",
         fun() ->
             RequestId = <<"req-001">>,
             Procedure = <<"my.procedure">>,
             EncodedArgs = <<"encoded-args">>,
             FromNodeId = <<"node-123">>,
             Realm = <<"test.realm">>,
             Msg = macula_rpc_async:build_request_message(
                 RequestId, Procedure, EncodedArgs, FromNodeId, Realm),
             ?assertEqual(<<"rpc_request">>, maps:get(type, Msg)),
             ?assertEqual(RequestId, maps:get(request_id, Msg)),
             ?assertEqual(Procedure, maps:get(procedure, Msg)),
             ?assertEqual(EncodedArgs, maps:get(args, Msg)),
             ?assertEqual(FromNodeId, maps:get(from_node, Msg)),
             ?assertEqual(Realm, maps:get(realm, Msg)),
             ?assert(is_integer(maps:get(timestamp, Msg)))
         end},

        {"includes from_endpoint",
         fun() ->
             Msg = macula_rpc_async:build_request_message(
                 <<"req">>, <<"proc">>, <<"args">>, <<"node">>, <<"realm">>),
             Endpoint = maps:get(from_endpoint, Msg),
             ?assert(is_binary(Endpoint)),
             %% Should contain colon (hostname:port format)
             ?assert(binary:match(Endpoint, <<":">>) =/= nomatch)
         end}
    ].

get_local_endpoint_test_() ->
    [
        {"returns binary in host:port format",
         fun() ->
             Endpoint = macula_rpc_async:get_local_endpoint(),
             ?assert(is_binary(Endpoint)),
             ?assert(binary:match(Endpoint, <<":">>) =/= nomatch)
         end}
    ].

%%%===================================================================
%%% Reply Processing Tests
%%%===================================================================

extract_result_test_() ->
    [
        {"extracts ok result from reply with binary key",
         fun() ->
             Reply = #{<<"result">> => <<"success">>},
             ?assertEqual({ok, <<"success">>}, macula_rpc_async:extract_result(Reply))
         end},

        {"extracts ok result from reply with atom key",
         fun() ->
             Reply = #{result => <<"success">>},
             ?assertEqual({ok, <<"success">>}, macula_rpc_async:extract_result(Reply))
         end},

        {"extracts error from reply with binary key",
         fun() ->
             Reply = #{<<"error">> => <<"not found">>},
             ?assertEqual({error, <<"not found">>}, macula_rpc_async:extract_result(Reply))
         end},

        {"extracts error from reply with atom key",
         fun() ->
             Reply = #{error => <<"timeout">>},
             ?assertEqual({error, <<"timeout">>}, macula_rpc_async:extract_result(Reply))
         end},

        {"returns error for empty reply",
         fun() ->
             Reply = #{},
             ?assertEqual({error, <<"Unknown error">>}, macula_rpc_async:extract_result(Reply))
         end},

        {"decodes JSON result if present",
         fun() ->
             JsonValue = <<"{\"key\":\"value\"}">>,
             Reply = #{result => JsonValue},
             {ok, Decoded} = macula_rpc_async:extract_result(Reply),
             ?assertEqual(#{<<"key">> => <<"value">>}, Decoded)
         end},

        {"returns non-JSON result as-is",
         fun() ->
             Reply = #{result => <<"plain text">>},
             ?assertEqual({ok, <<"plain text">>}, macula_rpc_async:extract_result(Reply))
         end}
    ].

%%%===================================================================
%%% RTT Calculation Tests
%%%===================================================================

calculate_rtt_test_() ->
    [
        {"calculates positive RTT",
         fun() ->
             SentAt = erlang:system_time(millisecond) - 100,
             RTT = macula_rpc_async:calculate_rtt(SentAt),
             ?assert(RTT >= 100),
             ?assert(RTT < 200)  %% Should be around 100ms with small margin
         end},

        {"handles very recent timestamp",
         fun() ->
             SentAt = erlang:system_time(millisecond),
             RTT = macula_rpc_async:calculate_rtt(SentAt),
             ?assert(RTT >= 0),
             ?assert(RTT < 50)  %% Should be very small
         end}
    ].

