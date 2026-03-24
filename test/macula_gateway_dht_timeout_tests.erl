%%%-------------------------------------------------------------------
%%% @doc Tests for DHT timeout handling in gateway and peer modules.
%%%
%%% Verifies that gen_server:call timeouts to macula_routing_server
%%% are caught gracefully instead of crashing the caller process.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_dht_timeout_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% safe_find_value tests (macula_gateway_dht)
%%%===================================================================

safe_find_value_returns_timeout_when_routing_server_hangs_test_() ->
    %% gen_server:call timeout is 10s, so this test needs >10s
    {timeout, 15, fun() ->
        {ok, FakePid} = start_hanging_server(macula_routing_server),
        Result = macula_gateway_dht:safe_find_value(crypto:strong_rand_bytes(32)),
        ?assertEqual({error, timeout}, Result),
        stop_fake_server(FakePid, macula_routing_server)
    end}.

safe_find_value_returns_error_when_routing_server_not_running_test() ->
    %% Ensure no routing server is registered
    ensure_not_registered(macula_routing_server),
    Result = macula_gateway_dht:safe_find_value(crypto:strong_rand_bytes(32)),
    ?assertEqual({error, routing_server_not_running}, Result).

safe_find_value_returns_ok_when_value_found_test() ->
    %% Start a fake routing server that returns a value
    Value = [#{node_id => <<"test">>, endpoint => <<"https://test:9443">>}],
    {ok, FakePid} = start_replying_server(macula_routing_server, {ok, Value}),
    Key = crypto:strong_rand_bytes(32),
    Result = macula_gateway_dht:safe_find_value(Key),
    ?assertEqual({ok, Value}, Result),
    stop_fake_server(FakePid, macula_routing_server).

safe_find_value_returns_empty_list_on_not_found_test() ->
    {ok, FakePid} = start_replying_server(macula_routing_server, {error, not_found}),
    Key = crypto:strong_rand_bytes(32),
    Result = macula_gateway_dht:safe_find_value(Key),
    ?assertEqual({error, not_found}, Result),
    stop_fake_server(FakePid, macula_routing_server).

safe_find_value_handles_noproc_test() ->
    %% Register then kill the server so whereis returns pid but call gets noproc
    {ok, FakePid} = start_hanging_server(macula_routing_server),
    %% Kill it but don't unregister — brief window where whereis succeeds but call fails
    exit(FakePid, kill),
    timer:sleep(10),
    %% After kill + sleep, the name should be unregistered by the runtime
    %% so we should get routing_server_not_running
    Result = macula_gateway_dht:safe_find_value(crypto:strong_rand_bytes(32)),
    ?assertMatch({error, _}, Result),
    ensure_not_registered(macula_routing_server).

%%%===================================================================
%%% do_handle_find_value timeout tests
%%%===================================================================

do_handle_find_value_returns_nodes_on_timeout_test_() ->
    {timeout, 15, fun() ->
        {ok, FakePid} = start_hanging_server(macula_routing_server),
        MockStream = spawn(fun() -> receive _ -> ok end end),
        ?assertEqual(ok, catch macula_gateway_dht:handle_find_value(
            MockStream, #{<<"key">> => crypto:strong_rand_bytes(32)})),
        exit(MockStream, kill),
        stop_fake_server(FakePid, macula_routing_server)
    end}.

do_handle_find_value_undefined_key_test() ->
    Result = macula_gateway_dht:do_handle_find_value(undefined),
    ?assertMatch(#{type := error, reason := missing_key}, Result).

do_handle_find_value_non_binary_key_test() ->
    Result = macula_gateway_dht:do_handle_find_value(12345),
    ?assertMatch(#{type := error, reason := invalid_key}, Result).

%%%===================================================================
%%% lookup_value timeout tests
%%%===================================================================

lookup_value_returns_not_found_on_timeout_test_() ->
    {timeout, 15, fun() ->
        {ok, FakePid} = start_hanging_server(macula_routing_server),
        Key = crypto:strong_rand_bytes(32),
        Result = macula_gateway_dht:lookup_value(Key),
        ?assertMatch({error, _}, Result),
        stop_fake_server(FakePid, macula_routing_server)
    end}.

lookup_value_returns_value_when_found_test() ->
    Value = [#{node_id => <<"n1">>, endpoint => <<"https://n1:9443">>}],
    {ok, FakePid} = start_replying_server(macula_routing_server, {ok, Value}),
    Key = crypto:strong_rand_bytes(32),
    Result = macula_gateway_dht:lookup_value(Key),
    ?assertEqual({ok, Value}, Result),
    stop_fake_server(FakePid, macula_routing_server).

lookup_value_returns_not_found_when_server_missing_test() ->
    ensure_not_registered(macula_routing_server),
    Key = crypto:strong_rand_bytes(32),
    Result = macula_gateway_dht:lookup_value(Key),
    ?assertEqual({error, not_found}, Result).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% Start a gen_server that never replies (simulates timeout)
start_hanging_server(Name) ->
    ensure_not_registered(Name),
    Pid = spawn(fun() -> hanging_server_loop(Name) end),
    timer:sleep(50),
    {ok, Pid}.

hanging_server_loop(Name) ->
    register(Name, self()),
    hanging_server_receive().

hanging_server_receive() ->
    receive
        {'$gen_call', _From, _Msg} ->
            %% Never reply — simulates a hung gen_server
            hanging_server_receive();
        stop ->
            ok
    end.

%% Start a gen_server that replies with a fixed value
start_replying_server(Name, Reply) ->
    ensure_not_registered(Name),
    Pid = spawn(fun() -> replying_server_loop(Name, Reply) end),
    timer:sleep(50),
    {ok, Pid}.

replying_server_loop(Name, Reply) ->
    register(Name, self()),
    replying_server_receive(Reply).

replying_server_receive(Reply) ->
    receive
        {'$gen_call', From, _Msg} ->
            gen_server:reply(From, Reply),
            replying_server_receive(Reply);
        stop ->
            ok
    end.

stop_fake_server(Pid, Name) ->
    Pid ! stop,
    timer:sleep(50),
    ensure_not_registered(Name).

ensure_not_registered(Name) ->
    case whereis(Name) of
        undefined -> ok;
        Pid ->
            unregister(Name),
            exit(Pid, kill),
            timer:sleep(50)
    end.
