%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_rpc_server module.
%%% Tests written FIRST (TDD red phase).
%%% GenServer managing RPC registrations and calls.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_server_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Test handler that returns success
handler_success() ->
    fun(Args) ->
        {ok, maps:merge(#{result => <<"success">>}, Args)}
    end.

%% Test handler that returns error
handler_error() ->
    fun(_Args) ->
        {error, not_implemented}
    end.

%% Test handler that sleeps (for timeout tests)
handler_slow(SleepMs) ->
    fun(_Args) ->
        timer:sleep(SleepMs),
        {ok, <<"completed">>}
    end.

%% Mock DHT lookup that returns providers
mock_dht_lookup_success(_Uri) ->
    {ok, [
        #{
            node_id => <<1:256>>,
            address => {{127,0,0,1}, 8080},
            metadata => #{},
            last_seen => erlang:system_time(millisecond)
        }
    ]}.

%% Mock DHT lookup that returns empty
mock_dht_lookup_empty(_Uri) ->
    {ok, []}.

%% Mock send function for remote calls
mock_send_success(_Uri, Args, _Address, _Timeout) ->
    {ok, maps:merge(#{remote => true}, Args)}.

mock_send_error(_Uri, _Args, _Address, _Timeout) ->
    {error, network_error}.

%%%===================================================================
%%% Server Lifecycle Tests
%%%===================================================================

%% Test: start_link starts server successfully
start_link_test() ->
    LocalNodeId = <<123:256>>,
    Config = #{},

    {ok, Pid} = macula_rpc_server:start_link(LocalNodeId, Config),

    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    macula_rpc_server:stop(Pid).

%% Test: stop terminates server
stop_test() ->
    LocalNodeId = <<123:256>>,
    {ok, Pid} = macula_rpc_server:start_link(LocalNodeId, #{}),

    ok = macula_rpc_server:stop(Pid),

    timer:sleep(10),  % Give time for cleanup
    ?assertNot(is_process_alive(Pid)).

%%%===================================================================
%%% Registration Tests
%%%===================================================================

%% Test: register adds procedure to local registry
register_test() ->
    LocalNodeId = <<123:256>>,
    {ok, Pid} = macula_rpc_server:start_link(LocalNodeId, #{}),

    Uri = <<"be.cortexiq.home.get_measurement">>,
    Handler = handler_success(),
    Metadata = #{rate_limit => 100},

    ok = macula_rpc_server:register(Pid, Uri, Handler, Metadata),

    %% Verify registration exists
    Registrations = macula_rpc_server:list_registrations(Pid),
    ?assertEqual(1, length(Registrations)),
    [Reg] = Registrations,
    ?assertEqual(Uri, maps:get(uri, Reg)),
    ?assertEqual(Metadata, maps:get(metadata, Reg)),

    macula_rpc_server:stop(Pid).

%% Test: register same URI multiple times (allows multiple handlers)
register_multiple_test() ->
    LocalNodeId = <<123:256>>,
    {ok, Pid} = macula_rpc_server:start_link(LocalNodeId, #{}),

    Uri = <<"be.cortexiq.home.get_measurement">>,
    Handler1 = handler_success(),
    Handler2 = handler_success(),

    ok = macula_rpc_server:register(Pid, Uri, Handler1, #{}),
    ok = macula_rpc_server:register(Pid, Uri, Handler2, #{}),

    Registrations = macula_rpc_server:list_registrations(Pid),
    ?assertEqual(2, length(Registrations)),

    macula_rpc_server:stop(Pid).

%% Test: unregister removes specific registration
unregister_test() ->
    LocalNodeId = <<123:256>>,
    {ok, Pid} = macula_rpc_server:start_link(LocalNodeId, #{}),

    Uri = <<"be.cortexiq.home.get_measurement">>,
    Handler = handler_success(),

    ok = macula_rpc_server:register(Pid, Uri, Handler, #{}),

    Registrations1 = macula_rpc_server:list_registrations(Pid),
    ?assertEqual(1, length(Registrations1)),

    ok = macula_rpc_server:unregister(Pid, Uri, Handler),

    Registrations2 = macula_rpc_server:list_registrations(Pid),
    ?assertEqual(0, length(Registrations2)),

    macula_rpc_server:stop(Pid).

%% Test: unregister non-existent registration returns ok
unregister_not_found_test() ->
    LocalNodeId = <<123:256>>,
    {ok, Pid} = macula_rpc_server:start_link(LocalNodeId, #{}),

    Uri = <<"be.cortexiq.home.get_measurement">>,
    Handler = handler_success(),

    %% Unregister without registering first
    ok = macula_rpc_server:unregister(Pid, Uri, Handler),

    macula_rpc_server:stop(Pid).

%%%===================================================================
%%% Local Call Tests
%%%===================================================================

%% Test: call to local handler succeeds
call_local_success_test() ->
    LocalNodeId = <<123:256>>,
    {ok, Pid} = macula_rpc_server:start_link(LocalNodeId, #{}),

    Uri = <<"be.cortexiq.home.get_measurement">>,
    Handler = handler_success(),
    ok = macula_rpc_server:register(Pid, Uri, Handler, #{}),

    Args = #{input => <<"test">>},
    Result = macula_rpc_server:call(Pid, Uri, Args, 5000),

    ?assertMatch({ok, _}, Result),
    {ok, Res} = Result,
    ?assertEqual(<<"success">>, maps:get(result, Res)),
    ?assertEqual(<<"test">>, maps:get(input, Res)),

    macula_rpc_server:stop(Pid).

%% Test: call to local handler propagates errors
call_local_error_test() ->
    LocalNodeId = <<123:256>>,
    {ok, Pid} = macula_rpc_server:start_link(LocalNodeId, #{}),

    Uri = <<"be.cortexiq.home.get_measurement">>,
    Handler = handler_error(),
    ok = macula_rpc_server:register(Pid, Uri, Handler, #{}),

    Result = macula_rpc_server:call(Pid, Uri, #{}, 5000),

    ?assertEqual({error, not_implemented}, Result),

    macula_rpc_server:stop(Pid).

%% Test: call to local handler handles timeout
call_local_timeout_test() ->
    LocalNodeId = <<123:256>>,
    {ok, Pid} = macula_rpc_server:start_link(LocalNodeId, #{}),

    Uri = <<"be.cortexiq.home.get_measurement">>,
    Handler = handler_slow(200),
    ok = macula_rpc_server:register(Pid, Uri, Handler, #{}),

    Result = macula_rpc_server:call(Pid, Uri, #{}, 50),  % Timeout before handler completes

    ?assertEqual({error, timeout}, Result),

    macula_rpc_server:stop(Pid).

%%%===================================================================
%%% Remote Call Tests (with injected functions)
%%%===================================================================

%% Test: call to remote provider succeeds
call_remote_success_test() ->
    LocalNodeId = <<123:256>>,
    Config = #{
        dht_lookup_fun => fun mock_dht_lookup_success/1,
        send_fun => fun mock_send_success/4
    },
    {ok, Pid} = macula_rpc_server:start_link(LocalNodeId, Config),

    Uri = <<"be.cortexiq.home.get_measurement">>,
    Args = #{input => <<"remote">>},

    Result = macula_rpc_server:call(Pid, Uri, Args, 5000),

    ?assertMatch({ok, _}, Result),
    {ok, Res} = Result,
    ?assertEqual(true, maps:get(remote, Res)),
    ?assertEqual(<<"remote">>, maps:get(input, Res)),

    macula_rpc_server:stop(Pid).

%% Test: call with no local or remote providers
call_no_provider_test() ->
    LocalNodeId = <<123:256>>,
    Config = #{
        dht_lookup_fun => fun mock_dht_lookup_empty/1
    },
    {ok, Pid} = macula_rpc_server:start_link(LocalNodeId, Config),

    Uri = <<"be.cortexiq.home.get_measurement">>,

    Result = macula_rpc_server:call(Pid, Uri, #{}, 5000),

    ?assertEqual({error, no_provider}, Result),

    macula_rpc_server:stop(Pid).

%% Test: call to remote provider handles network error
call_remote_error_test() ->
    LocalNodeId = <<123:256>>,
    Config = #{
        dht_lookup_fun => fun mock_dht_lookup_success/1,
        send_fun => fun mock_send_error/4
    },
    {ok, Pid} = macula_rpc_server:start_link(LocalNodeId, Config),

    Uri = <<"be.cortexiq.home.get_measurement">>,

    Result = macula_rpc_server:call(Pid, Uri, #{}, 5000),

    ?assertEqual({error, network_error}, Result),

    macula_rpc_server:stop(Pid).

%%%===================================================================
%%% Caching Tests
%%%===================================================================

%% Test: call with caching enabled caches result
call_with_cache_test() ->
    LocalNodeId = <<123:256>>,
    Config = #{cache_enabled => true},
    {ok, Pid} = macula_rpc_server:start_link(LocalNodeId, Config),

    Uri = <<"be.cortexiq.home.get_measurement">>,
    Handler = handler_success(),
    Metadata = #{cache_ttl => 60},  % 60 second TTL
    ok = macula_rpc_server:register(Pid, Uri, Handler, Metadata),

    Args = #{input => <<"cached">>},

    %% First call (cache miss)
    Result1 = macula_rpc_server:call(Pid, Uri, Args, 5000),
    ?assertMatch({ok, _}, Result1),

    %% Second call (cache hit - should be faster)
    Result2 = macula_rpc_server:call(Pid, Uri, Args, 5000),
    ?assertMatch({ok, _}, Result2),

    %% Results should be identical
    ?assertEqual(Result1, Result2),

    macula_rpc_server:stop(Pid).

%% Test: cache disabled does not cache results
call_without_cache_test() ->
    LocalNodeId = <<123:256>>,
    Config = #{cache_enabled => false},
    {ok, Pid} = macula_rpc_server:start_link(LocalNodeId, Config),

    Uri = <<"be.cortexiq.home.get_measurement">>,
    Handler = handler_success(),
    ok = macula_rpc_server:register(Pid, Uri, Handler, #{}),

    Args = #{input => <<"nocache">>},

    %% Both calls should execute handler (no caching)
    Result1 = macula_rpc_server:call(Pid, Uri, Args, 5000),
    Result2 = macula_rpc_server:call(Pid, Uri, Args, 5000),

    ?assertMatch({ok, _}, Result1),
    ?assertMatch({ok, _}, Result2),

    macula_rpc_server:stop(Pid).

%%%===================================================================
%%% Routing Strategy Tests
%%%===================================================================

%% Test: local_first strategy prefers local handler
routing_local_first_test() ->
    LocalNodeId = <<123:256>>,
    Config = #{
        routing_strategy => local_first,
        dht_lookup_fun => fun mock_dht_lookup_success/1,
        send_fun => fun mock_send_success/4
    },
    {ok, Pid} = macula_rpc_server:start_link(LocalNodeId, Config),

    Uri = <<"be.cortexiq.home.get_measurement">>,
    Handler = handler_success(),
    ok = macula_rpc_server:register(Pid, Uri, Handler, #{}),

    Args = #{input => <<"local">>},
    Result = macula_rpc_server:call(Pid, Uri, Args, 5000),

    ?assertMatch({ok, _}, Result),
    {ok, Res} = Result,
    %% Should call local handler (no 'remote' key)
    ?assertEqual(<<"success">>, maps:get(result, Res)),
    ?assertNot(maps:is_key(remote, Res)),

    macula_rpc_server:stop(Pid).

%% Test: random strategy can call remote when local not available
routing_random_test() ->
    LocalNodeId = <<123:256>>,
    Config = #{
        routing_strategy => random,
        dht_lookup_fun => fun mock_dht_lookup_success/1,
        send_fun => fun mock_send_success/4
    },
    {ok, Pid} = macula_rpc_server:start_link(LocalNodeId, Config),

    Uri = <<"be.cortexiq.home.get_measurement">>,
    Args = #{input => <<"random">>},

    Result = macula_rpc_server:call(Pid, Uri, Args, 5000),

    ?assertMatch({ok, _}, Result),
    {ok, Res} = Result,
    %% Should call remote provider
    ?assertEqual(true, maps:get(remote, Res)),

    macula_rpc_server:stop(Pid).
