%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_pubsub_server module.
%%% Tests gen_server managing pub/sub subscriptions and message delivery.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_server_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% Helper to create test subscriber ID
subscriber_id(N) ->
    list_to_binary("subscriber_" ++ integer_to_list(N)).

%% Helper to create test pattern
pattern(Topic) ->
    Topic.

%% Helper to create test callback (stores messages in process)
test_callback() ->
    self().

%% Helper to create test message
test_message(Topic, Payload) ->
    #{
        topic => Topic,
        payload => Payload
    }.

%% Setup function - starts server with default config
setup_server() ->
    %% Stop any existing server first
    catch gen_server:stop(macula_pubsub_server),
    timer:sleep(10),  %% Brief delay to ensure cleanup
    {ok, Pid} = macula_pubsub_server:start_link(),
    Pid.

%% Setup function with custom options
setup_server_with_options(Options) ->
    catch gen_server:stop(macula_pubsub_server),
    timer:sleep(10),
    {ok, Pid} = macula_pubsub_server:start_link(Options),
    Pid.

%% Cleanup function - stops server
cleanup_server(Pid) ->
    case is_process_alive(Pid) of
        true -> macula_pubsub_server:stop(Pid);
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

start_link_with_options_test() ->
    Options = #{
        cache_size => 500,
        cache_ttl => 60
    },
    Pid = setup_server_with_options(Options),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    %% Verify cache stats reflect custom size
    Stats = macula_pubsub_server:cache_stats(Pid),
    ?assertEqual(500, maps:get(max_size, Stats)),

    cleanup_server(Pid).

stop_terminates_server_test() ->
    Pid = setup_server(),
    ok = macula_pubsub_server:stop(Pid),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

%%%===================================================================
%%% Subscription Management Tests
%%%===================================================================

subscribe_adds_subscription_test() ->
    Pid = setup_server(),
    SubscriberId = subscriber_id(1),
    Pattern = pattern(<<"test.topic">>),
    Callback = test_callback(),

    ok = macula_pubsub_server:subscribe(Pid, SubscriberId, Pattern, Callback),

    Subscriptions = macula_pubsub_server:list_subscriptions(Pid),
    ?assertEqual(1, length(Subscriptions)),

    cleanup_server(Pid).

subscribe_multiple_patterns_same_subscriber_test() ->
    Pid = setup_server(),
    SubscriberId = subscriber_id(1),
    Callback = test_callback(),

    ok = macula_pubsub_server:subscribe(Pid, SubscriberId, <<"topic.one">>, Callback),
    ok = macula_pubsub_server:subscribe(Pid, SubscriberId, <<"topic.two">>, Callback),
    ok = macula_pubsub_server:subscribe(Pid, SubscriberId, <<"topic.three">>, Callback),

    Count = macula_pubsub_server:subscription_count(Pid),
    ?assertEqual(3, Count),

    cleanup_server(Pid).

subscribe_multiple_subscribers_same_pattern_test() ->
    Pid = setup_server(),
    Pattern = <<"shared.topic">>,
    Callback = test_callback(),

    ok = macula_pubsub_server:subscribe(Pid, subscriber_id(1), Pattern, Callback),
    ok = macula_pubsub_server:subscribe(Pid, subscriber_id(2), Pattern, Callback),
    ok = macula_pubsub_server:subscribe(Pid, subscriber_id(3), Pattern, Callback),

    Count = macula_pubsub_server:subscription_count(Pid),
    ?assertEqual(3, Count),

    cleanup_server(Pid).

unsubscribe_removes_subscription_test() ->
    Pid = setup_server(),
    SubscriberId = subscriber_id(1),
    Pattern = <<"test.topic">>,
    Callback = test_callback(),

    ok = macula_pubsub_server:subscribe(Pid, SubscriberId, Pattern, Callback),
    ?assertEqual(1, macula_pubsub_server:subscription_count(Pid)),

    ok = macula_pubsub_server:unsubscribe(Pid, SubscriberId, Pattern),
    ?assertEqual(0, macula_pubsub_server:subscription_count(Pid)),

    cleanup_server(Pid).

unsubscribe_nonexistent_subscription_test() ->
    Pid = setup_server(),
    SubscriberId = subscriber_id(999),
    Pattern = <<"nonexistent.topic">>,

    ok = macula_pubsub_server:unsubscribe(Pid, SubscriberId, Pattern),
    ?assertEqual(0, macula_pubsub_server:subscription_count(Pid)),

    cleanup_server(Pid).

unsubscribe_one_pattern_leaves_others_test() ->
    Pid = setup_server(),
    SubscriberId = subscriber_id(1),
    Callback = test_callback(),

    ok = macula_pubsub_server:subscribe(Pid, SubscriberId, <<"topic.one">>, Callback),
    ok = macula_pubsub_server:subscribe(Pid, SubscriberId, <<"topic.two">>, Callback),
    ok = macula_pubsub_server:subscribe(Pid, SubscriberId, <<"topic.three">>, Callback),

    ok = macula_pubsub_server:unsubscribe(Pid, SubscriberId, <<"topic.two">>),

    ?assertEqual(2, macula_pubsub_server:subscription_count(Pid)),

    cleanup_server(Pid).

%%%===================================================================
%%% List Operations Tests
%%%===================================================================

list_subscriptions_empty_test() ->
    Pid = setup_server(),
    Subscriptions = macula_pubsub_server:list_subscriptions(Pid),
    ?assertEqual([], Subscriptions),
    cleanup_server(Pid).

list_subscriptions_returns_all_test() ->
    Pid = setup_server(),
    Callback = test_callback(),

    ok = macula_pubsub_server:subscribe(Pid, subscriber_id(1), <<"topic.one">>, Callback),
    ok = macula_pubsub_server:subscribe(Pid, subscriber_id(2), <<"topic.two">>, Callback),

    Subscriptions = macula_pubsub_server:list_subscriptions(Pid),
    ?assertEqual(2, length(Subscriptions)),

    cleanup_server(Pid).

list_patterns_returns_unique_test() ->
    Pid = setup_server(),
    Callback = test_callback(),

    ok = macula_pubsub_server:subscribe(Pid, subscriber_id(1), <<"topic.one">>, Callback),
    ok = macula_pubsub_server:subscribe(Pid, subscriber_id(2), <<"topic.one">>, Callback),
    ok = macula_pubsub_server:subscribe(Pid, subscriber_id(3), <<"topic.two">>, Callback),

    Patterns = macula_pubsub_server:list_patterns(Pid),
    ?assertEqual(2, length(Patterns)),

    cleanup_server(Pid).

subscription_count_test() ->
    Pid = setup_server(),
    Callback = test_callback(),

    ?assertEqual(0, macula_pubsub_server:subscription_count(Pid)),

    ok = macula_pubsub_server:subscribe(Pid, subscriber_id(1), <<"topic">>, Callback),
    ?assertEqual(1, macula_pubsub_server:subscription_count(Pid)),

    ok = macula_pubsub_server:subscribe(Pid, subscriber_id(2), <<"topic">>, Callback),
    ?assertEqual(2, macula_pubsub_server:subscription_count(Pid)),

    cleanup_server(Pid).

%%%===================================================================
%%% Publishing Tests
%%%===================================================================

publish_is_asynchronous_test() ->
    Pid = setup_server(),
    Message = test_message(<<"test.topic">>, <<"payload">>),

    %% Publish should return ok immediately (it's a cast)
    Result = macula_pubsub_server:publish(Pid, Message),
    ?assertEqual(ok, Result),

    cleanup_server(Pid).

publish_to_pattern_with_no_subscribers_test() ->
    Pid = setup_server(),
    Message = test_message(<<"nonexistent.topic">>, <<"payload">>),

    %% Should not crash, just return ok
    Result = macula_pubsub_server:publish(Pid, Message),
    ?assertEqual(ok, Result),

    cleanup_server(Pid).

publish_with_multiple_patterns_test() ->
    Pid = setup_server(),
    Callback = test_callback(),

    %% Subscribe to multiple patterns
    ok = macula_pubsub_server:subscribe(Pid, subscriber_id(1), <<"topic.one">>, Callback),
    ok = macula_pubsub_server:subscribe(Pid, subscriber_id(2), <<"topic.two">>, Callback),
    ok = macula_pubsub_server:subscribe(Pid, subscriber_id(3), <<"topic.three">>, Callback),

    %% Publish to one pattern
    Message = test_message(<<"topic.one">>, <<"test_payload">>),
    ok = macula_pubsub_server:publish(Pid, Message),

    %% Give time for async delivery
    timer:sleep(50),

    cleanup_server(Pid).

publish_multiple_messages_test() ->
    Pid = setup_server(),
    Callback = test_callback(),

    ok = macula_pubsub_server:subscribe(Pid, subscriber_id(1), <<"test.topic">>, Callback),

    %% Publish multiple messages
    ok = macula_pubsub_server:publish(Pid, test_message(<<"test.topic">>, <<"msg1">>)),
    ok = macula_pubsub_server:publish(Pid, test_message(<<"test.topic">>, <<"msg2">>)),
    ok = macula_pubsub_server:publish(Pid, test_message(<<"test.topic">>, <<"msg3">>)),

    %% Give time for async delivery
    timer:sleep(50),

    cleanup_server(Pid).

%%%===================================================================
%%% Options and Configuration Tests
%%%===================================================================

start_with_custom_cache_size_test() ->
    Options = #{cache_size => 2000},
    Pid = setup_server_with_options(Options),

    Stats = macula_pubsub_server:cache_stats(Pid),
    ?assertEqual(2000, maps:get(max_size, Stats)),

    cleanup_server(Pid).

start_with_custom_cache_ttl_test() ->
    Options = #{cache_ttl => 120},
    Pid = setup_server_with_options(Options),

    %% Just verify server started successfully
    ?assert(is_process_alive(Pid)),

    cleanup_server(Pid).

start_with_custom_functions_test() ->
    DiscoveryFun = fun(_Pattern) -> {ok, []} end,
    SendFun = fun(_Msg, _Addr) -> ok end,

    Options = #{
        discovery_fun => DiscoveryFun,
        send_fun => SendFun
    },

    Pid = setup_server_with_options(Options),
    ?assert(is_process_alive(Pid)),

    cleanup_server(Pid).

%%%===================================================================
%%% Cache Statistics Tests
%%%===================================================================

cache_stats_returns_correct_structure_test() ->
    Pid = setup_server(),

    Stats = macula_pubsub_server:cache_stats(Pid),

    ?assert(is_map(Stats)),
    ?assert(maps:is_key(size, Stats)),
    ?assert(maps:is_key(max_size, Stats)),

    cleanup_server(Pid).

cache_stats_reflects_state_test() ->
    Options = #{cache_size => 100},
    Pid = setup_server_with_options(Options),

    Stats = macula_pubsub_server:cache_stats(Pid),
    ?assertEqual(100, maps:get(max_size, Stats)),
    ?assertEqual(0, maps:get(size, Stats)),

    cleanup_server(Pid).

%%%===================================================================
%%% Edge Cases and Integration Tests
%%%===================================================================

concurrent_subscriptions_test() ->
    Pid = setup_server(),
    Parent = self(),
    Callback = test_callback(),

    %% Spawn multiple processes subscribing
    lists:foreach(fun(N) ->
        spawn(fun() ->
            ok = macula_pubsub_server:subscribe(
                Pid,
                subscriber_id(N),
                list_to_binary("topic." ++ integer_to_list(N)),
                Callback
            ),
            Parent ! {done, N}
        end)
    end, lists:seq(1, 10)),

    %% Wait for all subscriptions
    lists:foreach(fun(N) ->
        receive
            {done, N} -> ok
        after 1000 ->
            ?assert(false)
        end
    end, lists:seq(1, 10)),

    %% Verify all subscribed
    Count = macula_pubsub_server:subscription_count(Pid),
    ?assertEqual(10, Count),

    cleanup_server(Pid).

concurrent_publishes_test() ->
    Pid = setup_server(),
    Callback = test_callback(),

    ok = macula_pubsub_server:subscribe(Pid, subscriber_id(1), <<"test.topic">>, Callback),

    Parent = self(),

    %% Spawn multiple processes publishing
    lists:foreach(fun(N) ->
        spawn(fun() ->
            Message = test_message(<<"test.topic">>, integer_to_binary(N)),
            ok = macula_pubsub_server:publish(Pid, Message),
            Parent ! {done, N}
        end)
    end, lists:seq(1, 10)),

    %% Wait for all publishes
    lists:foreach(fun(N) ->
        receive
            {done, N} -> ok
        after 1000 ->
            ?assert(false)
        end
    end, lists:seq(1, 10)),

    %% Give time for async delivery
    timer:sleep(100),

    cleanup_server(Pid).

subscribe_and_publish_concurrently_test() ->
    Pid = setup_server(),
    Parent = self(),
    Callback = test_callback(),

    %% Spawn subscribers
    lists:foreach(fun(N) ->
        spawn(fun() ->
            ok = macula_pubsub_server:subscribe(
                Pid,
                subscriber_id(N),
                <<"concurrent.topic">>,
                Callback
            ),
            Parent ! {sub_done, N}
        end)
    end, lists:seq(1, 5)),

    %% Spawn publishers
    lists:foreach(fun(N) ->
        spawn(fun() ->
            Message = test_message(<<"concurrent.topic">>, integer_to_binary(N)),
            ok = macula_pubsub_server:publish(Pid, Message),
            Parent ! {pub_done, N}
        end)
    end, lists:seq(1, 5)),

    %% Wait for all operations
    lists:foreach(fun(N) ->
        receive
            {sub_done, N} -> ok
        after 1000 ->
            ?assert(false)
        end
    end, lists:seq(1, 5)),

    lists:foreach(fun(N) ->
        receive
            {pub_done, N} -> ok
        after 1000 ->
            ?assert(false)
        end
    end, lists:seq(1, 5)),

    %% Give time for async delivery
    timer:sleep(100),

    cleanup_server(Pid).

subscribe_unsubscribe_stress_test() ->
    Pid = setup_server(),
    SubscriberId = subscriber_id(1),
    Pattern = <<"stress.topic">>,
    Callback = test_callback(),

    %% Subscribe and unsubscribe many times
    lists:foreach(fun(_) ->
        ok = macula_pubsub_server:subscribe(Pid, SubscriberId, Pattern, Callback),
        ok = macula_pubsub_server:unsubscribe(Pid, SubscriberId, Pattern)
    end, lists:seq(1, 100)),

    %% Should be empty
    ?assertEqual(0, macula_pubsub_server:subscription_count(Pid)),

    cleanup_server(Pid).
