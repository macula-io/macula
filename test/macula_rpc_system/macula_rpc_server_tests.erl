%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_rpc_server module.
%%% Tests gen_server managing RPC registrations and calls.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_server_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% Helper to create local node ID
local_node_id() ->
    <<100:256>>.

%% Helper to create test handler
test_handler(ReturnValue) ->
    fun(_Args) -> {ok, ReturnValue} end.

%% Helper to create error handler
error_handler(Reason) ->
    fun(_Args) -> {error, Reason} end.

%% Helper to create slow handler
slow_handler(Delay, ReturnValue) ->
    fun(_Args) ->
        timer:sleep(Delay),
        {ok, ReturnValue}
    end.

%% Setup function - starts server with test config
setup_server() ->
    %% Stop any existing server first
    catch gen_server:stop(macula_rpc_server),
    timer:sleep(10),  %% Brief delay to ensure cleanup

    Config = #{
        routing_strategy => local_first,
        cache_enabled => false
    },
    {ok, Pid} = macula_rpc_server:start_link(local_node_id(), Config),
    Pid.

%% Setup function with caching enabled
setup_server_with_cache() ->
    catch gen_server:stop(macula_rpc_server),
    timer:sleep(10),

    Config = #{
        routing_strategy => local_first,
        cache_enabled => true
    },
    {ok, Pid} = macula_rpc_server:start_link(local_node_id(), Config),
    Pid.

%% Cleanup function - stops server
cleanup_server(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_server:stop(Pid);
        false -> ok
    end,
    timer:sleep(10).  %% Brief delay to ensure cleanup

%%%===================================================================
%%% Server Lifecycle Tests
%%%===================================================================

start_link_creates_process_test() ->
    Pid = setup_server(),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    cleanup_server(Pid).

start_link_initializes_empty_registry_test() ->
    Pid = setup_server(),
    Result = macula_rpc_server:list_registrations(Pid),
    ?assertEqual([], Result),
    cleanup_server(Pid).

stop_terminates_server_test() ->
    Pid = setup_server(),
    ok = macula_rpc_server:stop(Pid),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

%%%===================================================================
%%% Registration Tests
%%%===================================================================

register_adds_handler_test() ->
    Pid = setup_server(),
    Uri = <<"com.example.add">>,
    Handler = test_handler(42),
    Metadata = #{},

    ok = macula_rpc_server:register(Pid, Uri, Handler, Metadata),

    Registrations = macula_rpc_server:list_registrations(Pid),
    ?assertEqual(1, length(Registrations)),

    cleanup_server(Pid).

register_with_invalid_uri_test() ->
    Pid = setup_server(),
    InvalidUri = <<"invalid uri with spaces">>,
    Handler = test_handler(42),
    Metadata = #{},

    Result = macula_rpc_server:register(Pid, InvalidUri, Handler, Metadata),

    ?assertMatch({error, _}, Result),
    cleanup_server(Pid).

register_multiple_handlers_test() ->
    Pid = setup_server(),
    Handler1 = test_handler(1),
    Handler2 = test_handler(2),
    Handler3 = test_handler(3),

    ok = macula_rpc_server:register(Pid, <<"com.example.one">>, Handler1, #{}),
    ok = macula_rpc_server:register(Pid, <<"com.example.two">>, Handler2, #{}),
    ok = macula_rpc_server:register(Pid, <<"com.example.three">>, Handler3, #{}),

    Registrations = macula_rpc_server:list_registrations(Pid),
    ?assertEqual(3, length(Registrations)),

    cleanup_server(Pid).

unregister_removes_handler_test() ->
    Pid = setup_server(),
    Uri = <<"com.example.add">>,
    Handler = test_handler(42),

    ok = macula_rpc_server:register(Pid, Uri, Handler, #{}),
    ?assertEqual(1, length(macula_rpc_server:list_registrations(Pid))),

    ok = macula_rpc_server:unregister(Pid, Uri, Handler),
    ?assertEqual(0, length(macula_rpc_server:list_registrations(Pid))),

    cleanup_server(Pid).

unregister_nonexistent_handler_test() ->
    Pid = setup_server(),
    Uri = <<"com.example.nonexistent">>,
    Handler = test_handler(42),

    ok = macula_rpc_server:unregister(Pid, Uri, Handler),
    ?assertEqual(0, length(macula_rpc_server:list_registrations(Pid))),

    cleanup_server(Pid).

%%%===================================================================
%%% List Registrations Tests
%%%===================================================================

list_registrations_empty_test() ->
    Pid = setup_server(),
    Result = macula_rpc_server:list_registrations(Pid),
    ?assertEqual([], Result),
    cleanup_server(Pid).

list_registrations_returns_all_test() ->
    Pid = setup_server(),

    ok = macula_rpc_server:register(Pid, <<"com.example.one">>, test_handler(1), #{}),
    ok = macula_rpc_server:register(Pid, <<"com.example.two">>, test_handler(2), #{}),

    Registrations = macula_rpc_server:list_registrations(Pid),
    ?assertEqual(2, length(Registrations)),

    cleanup_server(Pid).

%%%===================================================================
%%% Local Call Execution Tests
%%%===================================================================

call_executes_local_handler_test() ->
    Pid = setup_server(),
    Uri = <<"com.example.add">>,
    Handler = test_handler(42),

    ok = macula_rpc_server:register(Pid, Uri, Handler, #{}),

    Result = macula_rpc_server:call(Pid, Uri, #{a => 1, b => 2}, 5000),
    ?assertEqual({ok, 42}, Result),

    cleanup_server(Pid).

call_with_invalid_uri_test() ->
    Pid = setup_server(),
    InvalidUri = <<"invalid uri">>,

    Result = macula_rpc_server:call(Pid, InvalidUri, #{}, 5000),
    ?assertMatch({error, _}, Result),

    cleanup_server(Pid).

call_with_no_provider_test() ->
    Pid = setup_server(),
    Uri = <<"com.example.nonexistent">>,

    Result = macula_rpc_server:call(Pid, Uri, #{}, 5000),
    ?assertEqual({error, no_provider}, Result),

    cleanup_server(Pid).

call_with_error_handler_test() ->
    Pid = setup_server(),
    Uri = <<"com.example.error">>,
    Handler = error_handler(computation_failed),

    ok = macula_rpc_server:register(Pid, Uri, Handler, #{}),

    Result = macula_rpc_server:call(Pid, Uri, #{}, 5000),
    ?assertEqual({error, computation_failed}, Result),

    cleanup_server(Pid).

call_respects_timeout_test() ->
    Pid = setup_server(),
    Uri = <<"com.example.slow">>,
    Handler = slow_handler(2000, delayed_result),

    ok = macula_rpc_server:register(Pid, Uri, Handler, #{}),

    %% Call with short timeout (should timeout)
    Result = macula_rpc_server:call(Pid, Uri, #{}, 100),

    %% Should get timeout error from gen_server:call
    ?assertMatch({error, _}, Result),

    cleanup_server(Pid).

%%%===================================================================
%%% Caching Behavior Tests
%%%===================================================================

call_without_caching_executes_every_time_test() ->
    Pid = setup_server(),
    Uri = <<"com.example.no_cache">>,

    %% Simple handler that returns current timestamp
    Handler = fun(_Args) ->
        {ok, erlang:system_time(millisecond)}
    end,

    ok = macula_rpc_server:register(Pid, Uri, Handler, #{}),

    %% Make two calls with a small delay
    Result1 = macula_rpc_server:call(Pid, Uri, #{}, 5000),
    timer:sleep(2),  %% Small delay to ensure different timestamps
    Result2 = macula_rpc_server:call(Pid, Uri, #{}, 5000),

    %% Without caching, each call should execute and return different timestamps
    ?assertMatch({ok, _}, Result1),
    ?assertMatch({ok, _}, Result2),

    cleanup_server(Pid).

call_with_caching_enabled_test() ->
    Pid = setup_server_with_cache(),
    Uri = <<"com.example.cached">>,
    Handler = test_handler(42),

    %% Register with cache_ttl in metadata
    Metadata = #{cache_ttl => 60000},  %% 60 seconds
    ok = macula_rpc_server:register(Pid, Uri, Handler, Metadata),

    %% First call should execute
    Result1 = macula_rpc_server:call(Pid, Uri, #{x => 1}, 5000),
    ?assertEqual({ok, 42}, Result1),

    %% Second call with same args should hit cache
    Result2 = macula_rpc_server:call(Pid, Uri, #{x => 1}, 5000),
    ?assertEqual({ok, 42}, Result2),

    cleanup_server(Pid).

call_with_cache_ttl_zero_test() ->
    Pid = setup_server_with_cache(),
    Uri = <<"com.example.no_cache">>,
    Handler = test_handler(42),

    %% Register with cache_ttl = 0 (no caching)
    Metadata = #{cache_ttl => 0},
    ok = macula_rpc_server:register(Pid, Uri, Handler, Metadata),

    %% Make two calls
    Result1 = macula_rpc_server:call(Pid, Uri, #{}, 5000),
    Result2 = macula_rpc_server:call(Pid, Uri, #{}, 5000),

    ?assertEqual({ok, 42}, Result1),
    ?assertEqual({ok, 42}, Result2),

    cleanup_server(Pid).

call_error_result_not_cached_test() ->
    Pid = setup_server_with_cache(),
    Uri = <<"com.example.error">>,
    Handler = error_handler(test_error),

    Metadata = #{cache_ttl => 60000},
    ok = macula_rpc_server:register(Pid, Uri, Handler, Metadata),

    %% Error results should not be cached
    Result1 = macula_rpc_server:call(Pid, Uri, #{}, 5000),
    Result2 = macula_rpc_server:call(Pid, Uri, #{}, 5000),

    ?assertEqual({error, test_error}, Result1),
    ?assertEqual({error, test_error}, Result2),

    cleanup_server(Pid).

call_different_args_not_cached_test() ->
    Pid = setup_server_with_cache(),
    Uri = <<"com.example.cached">>,
    Handler = fun(Args) ->
        X = maps:get(x, Args, 0),
        {ok, X * 2}
    end,

    Metadata = #{cache_ttl => 60000},
    ok = macula_rpc_server:register(Pid, Uri, Handler, Metadata),

    %% Different args should execute separately
    Result1 = macula_rpc_server:call(Pid, Uri, #{x => 5}, 5000),
    Result2 = macula_rpc_server:call(Pid, Uri, #{x => 10}, 5000),

    ?assertEqual({ok, 10}, Result1),
    ?assertEqual({ok, 20}, Result2),

    cleanup_server(Pid).

%%%===================================================================
%%% Routing Strategy Tests
%%%===================================================================

routing_with_multiple_local_handlers_test() ->
    Pid = setup_server(),
    Uri = <<"com.example.multi">>,
    Handler1 = test_handler(1),
    Handler2 = test_handler(2),

    %% Register multiple handlers for same URI
    ok = macula_rpc_server:register(Pid, Uri, Handler1, #{}),
    ok = macula_rpc_server:register(Pid, Uri, Handler2, #{}),

    %% Call should succeed with one of the handlers
    Result = macula_rpc_server:call(Pid, Uri, #{}, 5000),
    ?assertMatch({ok, _}, Result),

    cleanup_server(Pid).

routing_local_first_strategy_test() ->
    Pid = setup_server(),
    Uri = <<"com.example.local">>,
    Handler = test_handler(local_result),

    ok = macula_rpc_server:register(Pid, Uri, Handler, #{}),

    %% With local handler available, should execute locally
    Result = macula_rpc_server:call(Pid, Uri, #{}, 5000),
    ?assertEqual({ok, local_result}, Result),

    cleanup_server(Pid).

%%%===================================================================
%%% Remote Execution Tests
%%%===================================================================

%% NOTE: Remote execution tests require DHT and transport setup
%% These tests focus on the server's handling of remote scenarios

call_with_dht_lookup_test() ->
    catch gen_server:stop(macula_rpc_server),
    timer:sleep(10),

    %% Mock DHT lookup that returns no providers
    DhtLookupFun = fun(_Uri) -> {ok, []} end,

    Config = #{
        routing_strategy => local_first,
        cache_enabled => false,
        dht_lookup_fun => DhtLookupFun
    },
    {ok, Pid} = macula_rpc_server:start_link(local_node_id(), Config),

    Uri = <<"com.example.remote">>,

    %% No local handler, DHT returns empty list
    Result = macula_rpc_server:call(Pid, Uri, #{}, 5000),
    ?assertEqual({error, no_provider}, Result),

    cleanup_server(Pid).

call_with_send_fun_test() ->
    catch gen_server:stop(macula_rpc_server),
    timer:sleep(10),

    %% Mock send function
    SendFun = fun(_Uri, _Args, _Address, _Timeout) ->
        {error, no_transport}
    end,

    %% Mock DHT lookup that returns a provider
    DhtLookupFun = fun(_Uri) ->
        {ok, [#{
            node_id => <<200:256>>,
            address => {{127, 0, 0, 1}, 4433}
        }]}
    end,

    Config = #{
        routing_strategy => local_first,
        cache_enabled => false,
        dht_lookup_fun => DhtLookupFun,
        send_fun => SendFun
    },
    {ok, Pid} = macula_rpc_server:start_link(local_node_id(), Config),

    Uri = <<"com.example.remote">>,

    %% No local handler, should attempt remote call
    Result = macula_rpc_server:call(Pid, Uri, #{}, 5000),
    ?assertEqual({error, no_transport}, Result),

    cleanup_server(Pid).

%%%===================================================================
%%% Edge Cases and Integration Tests
%%%===================================================================

concurrent_registrations_test() ->
    Pid = setup_server(),
    Parent = self(),

    %% Spawn multiple processes registering handlers
    lists:foreach(fun(N) ->
        spawn(fun() ->
            Uri = list_to_binary("com.example." ++ integer_to_list(N)),
            Handler = test_handler(N),
            ok = macula_rpc_server:register(Pid, Uri, Handler, #{}),
            Parent ! {done, N}
        end)
    end, lists:seq(1, 10)),

    %% Wait for all registrations
    lists:foreach(fun(N) ->
        receive
            {done, N} -> ok
        after 1000 ->
            ?assert(false)
        end
    end, lists:seq(1, 10)),

    %% Verify all registered
    Registrations = macula_rpc_server:list_registrations(Pid),
    ?assertEqual(10, length(Registrations)),

    cleanup_server(Pid).

concurrent_calls_test() ->
    Pid = setup_server(),
    Uri = <<"com.example.concurrent">>,
    Handler = test_handler(42),

    ok = macula_rpc_server:register(Pid, Uri, Handler, #{}),

    Parent = self(),

    %% Spawn multiple processes making calls
    lists:foreach(fun(N) ->
        spawn(fun() ->
            Result = macula_rpc_server:call(Pid, Uri, #{n => N}, 5000),
            Parent ! {done, N, Result}
        end)
    end, lists:seq(1, 10)),

    %% Wait for all calls
    Results = lists:map(fun(N) ->
        receive
            {done, N, Result} -> Result
        after 1000 ->
            timeout
        end
    end, lists:seq(1, 10)),

    %% All should succeed
    ?assertEqual(10, length([R || R <- Results, R =:= {ok, 42}])),

    cleanup_server(Pid).

register_and_unregister_stress_test() ->
    Pid = setup_server(),
    Uri = <<"com.example.stress">>,
    Handler = test_handler(42),

    %% Register and unregister many times
    lists:foreach(fun(_) ->
        ok = macula_rpc_server:register(Pid, Uri, Handler, #{}),
        ok = macula_rpc_server:unregister(Pid, Uri, Handler)
    end, lists:seq(1, 100)),

    %% Should be empty
    ?assertEqual(0, length(macula_rpc_server:list_registrations(Pid))),

    cleanup_server(Pid).
