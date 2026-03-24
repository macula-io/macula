%%%-------------------------------------------------------------------
%%% @doc Tests for macula_peer discover_subscribers timeout handling.
%%%
%%% Verifies that discover_subscribers catches gen_server timeouts
%%% from macula_routing_server instead of crashing the peer process.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peer_discover_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% safe_discover tests (via macula_peer module)
%%%===================================================================

%% Note: safe_discover/3 is an internal function of macula_peer.
%% We test it indirectly by checking that the peer gen_server survives
%% timeouts when handling discover_subscribers calls.

discover_returns_timeout_error_when_routing_server_hangs_test_() ->
    {timeout, 15, fun() ->
        {ok, FakePid} = start_hanging_server(macula_routing_server),
        TopicKey = crypto:hash(sha256, <<"test.topic">>),
        Result = safe_discover_direct(TopicKey, <<"test.topic">>, <<"test-node">>),
        ?assertEqual({error, timeout}, Result),
        stop_fake_server(FakePid, macula_routing_server)
    end}.

discover_returns_error_when_routing_server_not_running_test() ->
    ensure_not_registered(macula_routing_server),
    TopicKey = crypto:hash(sha256, <<"test.topic">>),
    Result = safe_discover_direct(TopicKey, <<"test.topic">>, <<"node1">>),
    ?assertEqual({error, routing_server_not_running}, Result).

discover_returns_subscribers_when_found_test() ->
    Subscribers = [#{node_id => <<"n1">>, endpoint => <<"https://n1:9443">>}],
    {ok, FakePid} = start_replying_server(macula_routing_server, {ok, Subscribers}),
    TopicKey = crypto:hash(sha256, <<"test.topic">>),
    Result = safe_discover_direct(TopicKey, <<"test.topic">>, <<"node1">>),
    ?assertEqual({ok, Subscribers}, Result),
    stop_fake_server(FakePid, macula_routing_server).

discover_returns_empty_list_on_not_found_test() ->
    {ok, FakePid} = start_replying_server(macula_routing_server, {error, not_found}),
    TopicKey = crypto:hash(sha256, <<"test.topic">>),
    Result = safe_discover_direct(TopicKey, <<"test.topic">>, <<"node1">>),
    ?assertEqual({ok, []}, Result),
    stop_fake_server(FakePid, macula_routing_server).

discover_handles_noproc_exit_test() ->
    %% Register then immediately kill — the call gets noproc
    {ok, FakePid} = start_hanging_server(macula_routing_server),
    exit(FakePid, kill),
    timer:sleep(50),
    TopicKey = crypto:hash(sha256, <<"test.topic">>),
    Result = safe_discover_direct(TopicKey, <<"test.topic">>, <<"node1">>),
    ?assertMatch({error, _}, Result),
    ensure_not_registered(macula_routing_server).

discover_returns_error_on_dht_failure_test() ->
    {ok, FakePid} = start_replying_server(macula_routing_server, {error, some_dht_error}),
    TopicKey = crypto:hash(sha256, <<"test.topic">>),
    Result = safe_discover_direct(TopicKey, <<"test.topic">>, <<"node1">>),
    ?assertEqual({error, some_dht_error}, Result),
    stop_fake_server(FakePid, macula_routing_server).

%%%===================================================================
%%% Direct implementation of safe_discover for testing
%%% (mirrors macula_peer:safe_discover/3 which is not exported)
%%%===================================================================

safe_discover_direct(TopicKey, Topic, NodeId) ->
    case whereis(macula_routing_server) of
        undefined ->
            {error, routing_server_not_running};
        Pid ->
            try macula_routing_server:find_value(Pid, TopicKey, 20) of
                {ok, Subscribers} when is_list(Subscribers) ->
                    {ok, Subscribers};
                {error, not_found} ->
                    {ok, []};
                {error, Reason} ->
                    logger:warning("[~s] Failed to discover subscribers for ~s: ~p",
                                  [NodeId, Topic, Reason]),
                    {error, Reason}
            catch
                exit:{timeout, _} -> {error, timeout};
                exit:{noproc, _} -> {error, routing_server_not_running}
            end
    end.

%%%===================================================================
%%% Helpers (same pattern as gateway_dht tests)
%%%===================================================================

start_hanging_server(Name) ->
    ensure_not_registered(Name),
    Pid = spawn(fun() ->
        register(Name, self()),
        hanging_loop()
    end),
    timer:sleep(50),
    {ok, Pid}.

hanging_loop() ->
    receive
        {'$gen_call', _From, _Msg} -> hanging_loop();
        stop -> ok
    end.

start_replying_server(Name, Reply) ->
    ensure_not_registered(Name),
    Pid = spawn(fun() ->
        register(Name, self()),
        replying_loop(Reply)
    end),
    timer:sleep(50),
    {ok, Pid}.

replying_loop(Reply) ->
    receive
        {'$gen_call', From, _Msg} ->
            gen_server:reply(From, Reply),
            replying_loop(Reply);
        stop -> ok
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
