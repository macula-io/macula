%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_rpc_executor module.
%%%
%%% Tests RPC call execution with timeout handling.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_executor_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% execute_local/3 Tests
%%%===================================================================

execute_local_success_test() ->
    %% GIVEN: A handler that returns success
    Handler = fun(Args) ->
        Val = maps:get(<<"value">>, Args, 0),
        {ok, Val * 2}
    end,
    Args = #{<<"value">> => 21},

    %% WHEN: Executing locally
    Result = macula_rpc_executor:execute_local(Handler, Args, 5000),

    %% THEN: Should return handler result
    ?assertEqual({ok, 42}, Result).

execute_local_error_test() ->
    %% GIVEN: A handler that returns error
    Handler = fun(_Args) ->
        {error, not_found}
    end,

    %% WHEN: Executing locally
    Result = macula_rpc_executor:execute_local(Handler, #{}, 5000),

    %% THEN: Should return error
    ?assertEqual({error, not_found}, Result).

execute_local_timeout_test() ->
    %% GIVEN: A handler that takes too long
    Handler = fun(_Args) ->
        timer:sleep(1000),
        {ok, done}
    end,

    %% WHEN: Executing with short timeout
    Result = macula_rpc_executor:execute_local(Handler, #{}, 50),

    %% THEN: Should return timeout error
    ?assertEqual({error, timeout}, Result).

execute_local_exception_test() ->
    %% GIVEN: A handler that crashes
    Handler = fun(_Args) ->
        error(intentional_crash)
    end,

    %% WHEN: Executing
    Result = macula_rpc_executor:execute_local(Handler, #{}, 5000),

    %% THEN: Should return exception error
    ?assertMatch({error, {exception, error, intentional_crash, _}}, Result).

execute_local_throw_exception_test() ->
    %% GIVEN: A handler that throws
    Handler = fun(_Args) ->
        throw(my_throw)
    end,

    %% WHEN: Executing
    Result = macula_rpc_executor:execute_local(Handler, #{}, 5000),

    %% THEN: Should return exception error
    ?assertMatch({error, {exception, throw, my_throw, _}}, Result).

execute_local_with_complex_args_test() ->
    %% GIVEN: A handler with complex args
    Handler = fun(Args) ->
        List = maps:get(<<"items">>, Args, []),
        {ok, length(List)}
    end,
    Args = #{<<"items">> => [1, 2, 3, 4, 5]},

    %% WHEN: Executing
    Result = macula_rpc_executor:execute_local(Handler, Args, 5000),

    %% THEN: Should return correct result
    ?assertEqual({ok, 5}, Result).

%%%===================================================================
%%% execute_remote/5 Tests
%%%===================================================================

execute_remote_success_test() ->
    %% GIVEN: A send function that returns success
    SendFun = fun(_Uri, _Args, _Address, _Timeout) ->
        {ok, #{<<"status">> => <<"success">>}}
    end,
    Provider = #{
        node_id => <<"remote-node">>,
        address => {{10,0,0,1}, 4433}
    },

    %% WHEN: Executing remote call
    Result = macula_rpc_executor:execute_remote(
        <<"com.example.service">>,
        #{<<"param">> => <<"value">>},
        Provider,
        SendFun,
        5000
    ),

    %% THEN: Should return send function result
    ?assertEqual({ok, #{<<"status">> => <<"success">>}}, Result).

execute_remote_error_test() ->
    %% GIVEN: A send function that returns error
    SendFun = fun(_Uri, _Args, _Address, _Timeout) ->
        {error, connection_refused}
    end,
    Provider = #{
        node_id => <<"remote-node">>,
        address => {{10,0,0,1}, 4433}
    },

    %% WHEN: Executing remote call
    Result = macula_rpc_executor:execute_remote(
        <<"com.example.service">>,
        #{},
        Provider,
        SendFun,
        5000
    ),

    %% THEN: Should return error
    ?assertEqual({error, connection_refused}, Result).

execute_remote_passes_correct_params_test() ->
    %% GIVEN: A send function that captures parameters
    Self = self(),
    SendFun = fun(Uri, Args, Address, Timeout) ->
        Self ! {send_called, Uri, Args, Address, Timeout},
        {ok, done}
    end,
    Provider = #{
        node_id => <<"remote-node">>,
        address => {{192,168,1,100}, 9443}
    },

    %% WHEN: Executing remote call
    Uri = <<"com.example.rpc">>,
    Args = #{<<"key">> => <<"value">>},
    Timeout = 3000,
    macula_rpc_executor:execute_remote(Uri, Args, Provider, SendFun, Timeout),

    %% THEN: Should pass correct parameters
    receive
        {send_called, ReceivedUri, ReceivedArgs, ReceivedAddress, ReceivedTimeout} ->
            ?assertEqual(Uri, ReceivedUri),
            ?assertEqual(Args, ReceivedArgs),
            ?assertEqual({{192,168,1,100}, 9443}, ReceivedAddress),
            ?assertEqual(Timeout, ReceivedTimeout)
    after 100 ->
        ?assert(false)
    end.

%%%===================================================================
%%% generate_call_id/0 Tests
%%%===================================================================

generate_call_id_returns_binary_test() ->
    %% WHEN: Generating call ID
    CallId = macula_rpc_executor:generate_call_id(),

    %% THEN: Should be a 16-byte binary
    ?assert(is_binary(CallId)),
    ?assertEqual(16, byte_size(CallId)).

generate_call_id_is_unique_test() ->
    %% WHEN: Generating multiple call IDs
    Ids = [macula_rpc_executor:generate_call_id() || _ <- lists:seq(1, 100)],

    %% THEN: All should be unique
    UniqueIds = lists:usort(Ids),
    ?assertEqual(100, length(UniqueIds)).
