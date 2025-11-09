%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_rpc_executor module.
%%% Tests written FIRST (TDD red phase).
%%% RPC call execution with timeout handling.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_executor_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Handler that returns success
handler_success() ->
    fun(Args) ->
        {ok, maps:merge(#{result => <<"success">>}, Args)}
    end.

%% Handler that returns error
handler_error() ->
    fun(_Args) ->
        {error, not_implemented}
    end.

%% Handler that sleeps (for timeout tests)
handler_slow(SleepMs) ->
    fun(_Args) ->
        timer:sleep(SleepMs),
        {ok, <<"completed">>}
    end.

%%%===================================================================
%%% Execute Local Tests
%%%===================================================================

%% Test: execute_local calls handler and returns result
execute_local_success_test() ->
    Handler = handler_success(),
    Args = #{input => <<"test">>},

    Result = macula_rpc_executor:execute_local(Handler, Args, 5000),

    ?assertMatch({ok, _}, Result),
    {ok, Res} = Result,
    ?assertEqual(<<"success">>, maps:get(result, Res)),
    ?assertEqual(<<"test">>, maps:get(input, Res)).

%% Test: execute_local propagates handler errors
execute_local_error_test() ->
    Handler = handler_error(),
    Args = #{},

    Result = macula_rpc_executor:execute_local(Handler, Args, 5000),

    ?assertEqual({error, not_implemented}, Result).

%% Test: execute_local handles timeout
execute_local_timeout_test() ->
    Handler = handler_slow(200),  % Sleeps 200ms
    Args = #{},
    Timeout = 50,  % Timeout after 50ms

    Result = macula_rpc_executor:execute_local(Handler, Args, Timeout),

    ?assertEqual({error, timeout}, Result).

%% Test: execute_local succeeds when handler finishes before timeout
execute_local_no_timeout_test() ->
    Handler = handler_slow(50),  % Sleeps 50ms
    Args = #{},
    Timeout = 200,  % Timeout after 200ms

    Result = macula_rpc_executor:execute_local(Handler, Args, Timeout),

    ?assertMatch({ok, _}, Result).

%%%===================================================================
%%% Execute Remote Tests
%%%===================================================================

%% Test: execute_remote sends call and returns result
execute_remote_success_test() ->
    Uri = <<"be.cortexiq.home.get_measurement">>,
    Args = #{home_id => <<"home_001">>},
    Provider = #{
        node_id => <<1:256>>,
        address => {{127,0,0,1}, 8080}
    },

    %% Mock send function
    SendFun = fun(U, A, _Addr, _Timeout) ->
        ?assertEqual(Uri, U),
        ?assertEqual(Args, A),
        {ok, #{temperature => 22.5}}
    end,

    Result = macula_rpc_executor:execute_remote(Uri, Args, Provider, SendFun, 5000),

    ?assertMatch({ok, _}, Result),
    {ok, Res} = Result,
    ?assertEqual(22.5, maps:get(temperature, Res)).

%% Test: execute_remote propagates remote errors
execute_remote_error_test() ->
    Uri = <<"be.cortexiq.home.get_measurement">>,
    Args = #{},
    Provider = #{
        node_id => <<1:256>>,
        address => {{127,0,0,1}, 8080}
    },

    %% Mock send function that returns error
    SendFun = fun(_U, _A, _Addr, _Timeout) ->
        {error, not_found}
    end,

    Result = macula_rpc_executor:execute_remote(Uri, Args, Provider, SendFun, 5000),

    ?assertEqual({error, not_found}, Result).

%% Test: execute_remote handles network errors
execute_remote_network_error_test() ->
    Uri = <<"be.cortexiq.home.get_measurement">>,
    Args = #{},
    Provider = #{
        node_id => <<1:256>>,
        address => {{127,0,0,1}, 8080}
    },

    %% Mock send function that fails
    SendFun = fun(_U, _A, _Addr, _Timeout) ->
        {error, {network_error, connection_refused}}
    end,

    Result = macula_rpc_executor:execute_remote(Uri, Args, Provider, SendFun, 5000),

    ?assertMatch({error, {network_error, _}}, Result).

%%%===================================================================
%%% Call ID Generation Tests
%%%===================================================================

%% Test: generate_call_id creates unique IDs
generate_call_id_test() ->
    Id1 = macula_rpc_executor:generate_call_id(),
    Id2 = macula_rpc_executor:generate_call_id(),

    ?assert(is_binary(Id1)),
    ?assert(is_binary(Id2)),
    ?assertNotEqual(Id1, Id2),
    ?assertEqual(16, byte_size(Id1)),  % 128-bit UUID
    ?assertEqual(16, byte_size(Id2)).
