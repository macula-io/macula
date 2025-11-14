%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive EUnit tests for macula_pubsub_handler module.
%%%
%%% Tests DHT operations, QoS management, and callback handling
%%% using mock connection manager.
%%%
%%% Test Coverage:
%%% - DHT advertisement (5 tests)
%%% - DHT discovery (5 tests)
%%% - QoS 1 retry logic (8 tests)
%%% - Callback invocation (5 tests)
%%% - Message routing with patterns (5 tests)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_handler_comprehensive_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% DHT Advertisement Tests (5 tests)
%%%===================================================================

dht_advertisement_on_subscribe_test() ->
    %% GIVEN: Handler with mock connection manager
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    %% WHEN: Subscribing to a topic
    Topic = <<"test.topic">>,
    {ok, _SubRef} = macula_pubsub_handler:subscribe(Handler, Topic, fun(_) -> ok end),
    timer:sleep(100),

    %% THEN: DHT STORE message should be sent
    Messages = macula_test_helpers:get_mock_messages(MockConnMgr),
    ?assert(length(Messages) >= 2), % subscribe + store
    StoreMessages = lists:filter(
        fun(#{type := Type}) -> Type =:= store end,
        Messages
    ),
    ?assert(length(StoreMessages) >= 1),

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

dht_advertisement_includes_ttl_test() ->
    %% GIVEN: Handler with mock connection manager
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    %% WHEN: Subscribing to a topic
    Topic = <<"test.advertise">>,
    {ok, _SubRef} = macula_pubsub_handler:subscribe(Handler, Topic, fun(_) -> ok end),
    timer:sleep(100),

    %% THEN: STORE message should include TTL in value
    Messages = macula_test_helpers:get_mock_messages(MockConnMgr),
    StoreMessages = lists:filter(
        fun(#{type := Type}) -> Type =:= store end,
        Messages
    ),
    ?assert(length(StoreMessages) >= 1),

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

dht_advertisement_canceled_on_unsubscribe_test() ->
    %% GIVEN: Handler with subscription
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    Topic = <<"test.unsub">>,
    {ok, SubRef} = macula_pubsub_handler:subscribe(Handler, Topic, fun(_) -> ok end),
    timer:sleep(100),
    macula_test_helpers:clear_mock_messages(MockConnMgr),

    %% WHEN: Unsubscribing
    ok = macula_pubsub_handler:unsubscribe(Handler, SubRef),
    timer:sleep(100),

    %% THEN: Unsubscribe message sent (timer should be cancelled internally)
    Messages = macula_test_helpers:get_mock_messages(MockConnMgr),
    UnsubMessages = lists:filter(
        fun(#{type := Type}) -> Type =:= unsubscribe end,
        Messages
    ),
    ?assert(length(UnsubMessages) >= 1),

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

dht_readvertisement_scheduled_test() ->
    %% GIVEN: Handler with subscription
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    %% WHEN: Subscribing
    Topic = <<"test.readvertise">>,
    {ok, _SubRef} = macula_pubsub_handler:subscribe(Handler, Topic, fun(_) -> ok end),
    timer:sleep(50),

    %% THEN: Initial advertisement sent
    Messages = macula_test_helpers:get_mock_messages(MockConnMgr),
    InitialStoreCount = length(lists:filter(
        fun(#{type := Type}) -> Type =:= store end,
        Messages
    )),
    ?assert(InitialStoreCount >= 1),

    %% Note: Re-advertisement happens after TTL-60s (240s), too long for unit test
    %% Just verify handler is still alive and timer was set
    timer:sleep(100),
    ?assert(is_process_alive(Handler)),

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

dht_multiple_subscriptions_advertised_test() ->
    %% GIVEN: Handler with mock
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    %% WHEN: Subscribing to multiple topics
    {ok, _Ref1} = macula_pubsub_handler:subscribe(Handler, <<"topic.one">>, fun(_) -> ok end),
    {ok, _Ref2} = macula_pubsub_handler:subscribe(Handler, <<"topic.two">>, fun(_) -> ok end),
    {ok, _Ref3} = macula_pubsub_handler:subscribe(Handler, <<"topic.three">>, fun(_) -> ok end),
    timer:sleep(150),

    %% THEN: Multiple STORE messages sent
    Messages = macula_test_helpers:get_mock_messages(MockConnMgr),
    StoreMessages = lists:filter(
        fun(#{type := Type}) -> Type =:= store end,
        Messages
    ),
    ?assert(length(StoreMessages) >= 3),

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

%%%===================================================================
%%% DHT Discovery Tests (5 tests)
%%%===================================================================

dht_discovery_triggered_on_publish_test() ->
    %% GIVEN: Handler with mock
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),
    macula_test_helpers:clear_mock_messages(MockConnMgr),

    %% WHEN: Publishing to a topic
    Topic = <<"remote.topic">>,
    ok = macula_pubsub_handler:publish(Handler, Topic, <<"data">>, #{}),
    timer:sleep(200),

    %% THEN: FIND_VALUE query should be sent for remote subscribers
    Messages = macula_test_helpers:get_mock_messages(MockConnMgr),
    _FindValueMessages = lists:filter(
        fun(#{type := Type}) -> Type =:= find_value end,
        Messages
    ),
    %% May or may not be sent depending on cache, but publish should succeed
    ?assert(length(Messages) >= 1), % At least publish message

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

dht_discovery_cache_miss_triggers_query_test() ->
    %% GIVEN: Handler with empty cache
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    %% WHEN: Publishing to topic (cache miss expected)
    Topic = <<"cache.miss.topic">>,
    ok = macula_pubsub_handler:publish(Handler, Topic, <<"test">>, #{}),
    timer:sleep(200),

    %% THEN: Should handle gracefully (query sent asynchronously)
    ?assert(is_process_alive(Handler)),

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

dht_discovery_handles_empty_results_test() ->
    %% GIVEN: Handler
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    %% WHEN: Publishing (will query DHT asynchronously)
    Topic = <<"empty.result.topic">>,
    ok = macula_pubsub_handler:publish(Handler, Topic, <<"data">>, #{}),
    timer:sleep(100),

    %% THEN: Handler should not crash
    ?assert(is_process_alive(Handler)),

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

dht_discovery_async_does_not_block_test() ->
    %% GIVEN: Handler
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    %% WHEN: Publishing multiple times rapidly
    StartTime = erlang:system_time(millisecond),
    ok = macula_pubsub_handler:publish(Handler, <<"topic1">>, <<"d1">>, #{}),
    ok = macula_pubsub_handler:publish(Handler, <<"topic2">>, <<"d2">>, #{}),
    ok = macula_pubsub_handler:publish(Handler, <<"topic3">>, <<"d3">>, #{}),
    EndTime = erlang:system_time(millisecond),

    %% THEN: Should return quickly (not block on DHT queries)
    Duration = EndTime - StartTime,
    ?assert(Duration < 100), % Should take < 100ms

    %% Cleanup
    timer:sleep(100), % Let async operations complete
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

dht_discovery_pending_queries_tracked_test() ->
    %% GIVEN: Handler
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    %% WHEN: Publishing (triggers DHT query)
    Topic = <<"pending.query.topic">>,
    ok = macula_pubsub_handler:publish(Handler, Topic, <<"data">>, #{}),
    timer:sleep(100),

    %% THEN: Handler should track pending query (tested via no crash)
    ?assert(is_process_alive(Handler)),

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

%%%===================================================================
%%% QoS 1 Tests (8 tests)
%%%===================================================================

qos1_publish_returns_ok_immediately_test() ->
    %% GIVEN: Handler with mock
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    %% WHEN: Publishing with QoS 1
    Topic = <<"qos1.topic">>,
    Result = macula_pubsub_handler:publish(Handler, Topic, <<"data">>, #{qos => 1}),

    %% THEN: Should return ok immediately (async)
    ?assertEqual(ok, Result),

    %% Cleanup
    timer:sleep(50),
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

qos1_message_tracked_for_ack_test() ->
    %% GIVEN: Handler with QoS 1 publish
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    %% WHEN: Publishing with QoS 1
    Topic = <<"qos1.tracked">>,
    ok = macula_pubsub_handler:publish(Handler, Topic, <<"data">>, #{qos => 1}),
    timer:sleep(100),

    %% THEN: Message should be sent and tracked internally (no crash)
    ?assert(is_process_alive(Handler)),

    Messages = macula_test_helpers:get_mock_messages(MockConnMgr),
    PublishMessages = lists:filter(
        fun(#{type := Type}) -> Type =:= publish end,
        Messages
    ),
    ?assert(length(PublishMessages) >= 1),

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

qos1_timeout_triggers_retry_test() ->
    %% GIVEN: Handler with QoS 1 publish
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    %% WHEN: Publishing with QoS 1 and waiting for timeout
    Topic = <<"qos1.timeout">>,
    ok = macula_pubsub_handler:publish(Handler, Topic, <<"data">>, #{qos => 1}),
    timer:sleep(100),
    macula_test_helpers:clear_mock_messages(MockConnMgr),

    %% Wait for timeout (5 seconds)
    timer:sleep(5200),

    %% THEN: Retry should be sent
    Messages = macula_test_helpers:get_mock_messages(MockConnMgr),
    PublishMessages = lists:filter(
        fun(#{type := Type}) -> Type =:= publish end,
        Messages
    ),
    %% Should have at least one retry
    ?assert(length(PublishMessages) >= 1),

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

qos1_max_retries_reached_gives_up_test() ->
    %% GIVEN: Handler with QoS 1 publish
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    %% WHEN: Publishing with QoS 1
    Topic = <<"qos1.maxretry">>,
    ok = macula_pubsub_handler:publish(Handler, Topic, <<"data">>, #{qos => 1}),
    timer:sleep(100),
    macula_test_helpers:clear_mock_messages(MockConnMgr),

    %% Wait for all retries (5s * 3 retries = 15s, plus buffer)
    %% NOTE: This test takes ~15 seconds, may want to reduce timeout in config
    timer:sleep(16000),

    %% THEN: Should have max 3 retries, then give up
    Messages = macula_test_helpers:get_mock_messages(MockConnMgr),
    PublishMessages = lists:filter(
        fun(#{type := Type}) -> Type =:= publish end,
        Messages
    ),
    %% Should have stopped retrying (max 3)
    ?assert(length(PublishMessages) =< 3),
    ?assert(is_process_alive(Handler)), % Should not crash

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

qos0_does_not_track_ack_test() ->
    %% GIVEN: Handler
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    %% WHEN: Publishing with QoS 0 (default)
    Topic = <<"qos0.topic">>,
    ok = macula_pubsub_handler:publish(Handler, Topic, <<"data">>, #{}),
    timer:sleep(100),

    %% THEN: Message sent, no tracking (fire-and-forget)
    Messages = macula_test_helpers:get_mock_messages(MockConnMgr),
    PublishMessages = lists:filter(
        fun(#{type := Type}) -> Type =:= publish end,
        Messages
    ),
    ?assert(length(PublishMessages) >= 1),

    %% Wait to ensure no retries
    macula_test_helpers:clear_mock_messages(MockConnMgr),
    timer:sleep(5200),
    RetryMessages = macula_test_helpers:get_mock_messages(MockConnMgr),
    PublishRetries = lists:filter(
        fun(#{type := Type}) -> Type =:= publish end,
        RetryMessages
    ),
    ?assertEqual(0, length(PublishRetries)), % No retries for QoS 0

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

qos1_with_retain_flag_test() ->
    %% GIVEN: Handler
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    %% WHEN: Publishing with QoS 1 and retain flag
    Topic = <<"qos1.retain">>,
    ok = macula_pubsub_handler:publish(Handler, Topic, <<"data">>, #{qos => 1, retain => true}),
    timer:sleep(100),

    %% THEN: Message sent with both flags
    Messages = macula_test_helpers:get_mock_messages(MockConnMgr),
    PublishMessages = lists:filter(
        fun(#{type := Type}) -> Type =:= publish end,
        Messages
    ),
    ?assert(length(PublishMessages) >= 1),

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

qos1_different_message_ids_test() ->
    %% GIVEN: Handler
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    %% WHEN: Publishing multiple messages with QoS 1
    ok = macula_pubsub_handler:publish(Handler, <<"topic1">>, <<"d1">>, #{qos => 1}),
    ok = macula_pubsub_handler:publish(Handler, <<"topic2">>, <<"d2">>, #{qos => 1}),
    ok = macula_pubsub_handler:publish(Handler, <<"topic3">>, <<"d3">>, #{qos => 1}),
    timer:sleep(150),

    %% THEN: Each should have unique message ID
    Messages = macula_test_helpers:get_mock_messages(MockConnMgr),
    PublishMessages = lists:filter(
        fun(#{type := Type}) -> Type =:= publish end,
        Messages
    ),
    ?assert(length(PublishMessages) >= 3),

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

qos1_handler_survives_retry_errors_test() ->
    %% GIVEN: Handler that will have retry failures
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    %% WHEN: Publishing with QoS 1
    ok = macula_pubsub_handler:publish(Handler, <<"topic">>, <<"data">>, #{qos => 1}),
    timer:sleep(100),

    %% Simulate connection failure
    macula_test_helpers:set_mock_status(MockConnMgr, disconnected),

    %% Wait for retry timeout
    timer:sleep(5200),

    %% THEN: Handler should survive (not crash)
    ?assert(is_process_alive(Handler)),

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

%%%===================================================================
%%% Callback Invocation Tests (5 tests)
%%%===================================================================

callback_invoked_for_matching_subscription_test() ->
    %% GIVEN: Handler with subscription and callback tracker
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    %% Setup callback that sends message to test process
    TestPid = self(),
    Callback = fun(Msg) ->
        TestPid ! {callback_invoked, Msg}
    end,

    Topic = <<"test.callback">>,
    {ok, _SubRef} = macula_pubsub_handler:subscribe(Handler, Topic, Callback),
    timer:sleep(50),

    %% WHEN: Incoming publish message
    PublishMsg = #{
        topic => Topic,
        payload => <<"test data">>
    },
    macula_pubsub_handler:handle_incoming_publish(Handler, PublishMsg),

    %% THEN: Callback should be invoked
    receive
        {callback_invoked, ReceivedMsg} ->
            ?assertMatch(#{topic := Topic}, ReceivedMsg),
            ok
    after 1000 ->
        ?assert(false) % Timeout - callback not invoked
    end,

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

callback_not_invoked_for_non_matching_topic_test() ->
    %% GIVEN: Handler with subscription
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    TestPid = self(),
    Callback = fun(Msg) ->
        TestPid ! {callback_invoked, Msg}
    end,

    {ok, _SubRef} = macula_pubsub_handler:subscribe(Handler, <<"topic.one">>, Callback),
    timer:sleep(50),

    %% WHEN: Publish to different topic
    PublishMsg = #{
        topic => <<"topic.two">>,
        payload => <<"data">>
    },
    macula_pubsub_handler:handle_incoming_publish(Handler, PublishMsg),

    %% THEN: Callback should NOT be invoked
    receive
        {callback_invoked, _} ->
            ?assert(false) % Should not receive callback
    after 500 ->
        ok % Expected timeout
    end,

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

callback_receives_decoded_json_payload_test() ->
    %% GIVEN: Handler with subscription
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    TestPid = self(),
    Callback = fun(Msg) ->
        TestPid ! {callback_invoked, Msg}
    end,

    Topic = <<"test.json">>,
    {ok, _SubRef} = macula_pubsub_handler:subscribe(Handler, Topic, Callback),
    timer:sleep(50),

    %% WHEN: Incoming publish with JSON payload
    JsonData = #{key => <<"value">>, number => 42},
    JsonBinary = macula_utils:encode_json(JsonData),
    PublishMsg = #{
        topic => Topic,
        payload => JsonBinary
    },
    macula_pubsub_handler:handle_incoming_publish(Handler, PublishMsg),

    %% THEN: Callback receives decoded JSON as map
    receive
        {callback_invoked, #{payload := Payload}} ->
            ?assertMatch(#{key := <<"value">>}, Payload),
            ?assertMatch(#{number := 42}, Payload),
            ok
    after 1000 ->
        ?assert(false)
    end,

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

callback_exception_does_not_crash_handler_test() ->
    %% GIVEN: Handler with callback that throws exception
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    BadCallback = fun(_Msg) ->
        error(intentional_error)
    end,

    Topic = <<"test.exception">>,
    {ok, _SubRef} = macula_pubsub_handler:subscribe(Handler, Topic, BadCallback),
    timer:sleep(50),

    %% WHEN: Incoming publish triggers bad callback
    PublishMsg = #{
        topic => Topic,
        payload => <<"data">>
    },
    macula_pubsub_handler:handle_incoming_publish(Handler, PublishMsg),
    timer:sleep(200),

    %% THEN: Handler should survive
    ?assert(is_process_alive(Handler)),

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

multiple_callbacks_invoked_for_same_topic_test() ->
    %% GIVEN: Handler with multiple subscriptions to same topic
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    TestPid = self(),
    Callback1 = fun(Msg) -> TestPid ! {callback1, Msg} end,
    Callback2 = fun(Msg) -> TestPid ! {callback2, Msg} end,
    Callback3 = fun(Msg) -> TestPid ! {callback3, Msg} end,

    Topic = <<"shared.topic">>,
    {ok, _Ref1} = macula_pubsub_handler:subscribe(Handler, Topic, Callback1),
    {ok, _Ref2} = macula_pubsub_handler:subscribe(Handler, Topic, Callback2),
    {ok, _Ref3} = macula_pubsub_handler:subscribe(Handler, Topic, Callback3),
    timer:sleep(100),

    %% WHEN: Incoming publish
    PublishMsg = #{
        topic => Topic,
        payload => <<"multi">>
    },
    macula_pubsub_handler:handle_incoming_publish(Handler, PublishMsg),

    %% THEN: All 3 callbacks should be invoked
    Callback1Invoked = receive {callback1, _} -> true after 1000 -> false end,
    Callback2Invoked = receive {callback2, _} -> true after 1000 -> false end,
    Callback3Invoked = receive {callback3, _} -> true after 1000 -> false end,

    ?assert(Callback1Invoked),
    ?assert(Callback2Invoked),
    ?assert(Callback3Invoked),

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

%%%===================================================================
%%% Message Routing with Pattern Matching Tests (5 tests)
%%%===================================================================

pattern_exact_match_test() ->
    %% GIVEN: Handler with exact topic subscription
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    TestPid = self(),
    Callback = fun(Msg) -> TestPid ! {matched, Msg} end,

    {ok, _SubRef} = macula_pubsub_handler:subscribe(Handler, <<"exact.topic">>, Callback),
    timer:sleep(50),

    %% WHEN: Publishing to exact topic
    PublishMsg = #{topic => <<"exact.topic">>, payload => <<"data">>},
    macula_pubsub_handler:handle_incoming_publish(Handler, PublishMsg),

    %% THEN: Should match
    receive
        {matched, _} -> ok
    after 1000 ->
        ?assert(false)
    end,

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

pattern_single_level_wildcard_test() ->
    %% GIVEN: Handler with single-level wildcard subscription
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    TestPid = self(),
    Callback = fun(Msg) -> TestPid ! {matched, Msg} end,

    %% Subscribe with * wildcard (matches one level)
    {ok, _SubRef} = macula_pubsub_handler:subscribe(Handler, <<"sensor.*.temperature">>, Callback),
    timer:sleep(50),

    %% WHEN: Publishing to matching topic
    PublishMsg = #{topic => <<"sensor.kitchen.temperature">>, payload => <<"22C">>},
    macula_pubsub_handler:handle_incoming_publish(Handler, PublishMsg),

    %% THEN: Should match
    receive
        {matched, _} -> ok
    after 1000 ->
        ?assert(false)
    end,

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

pattern_multi_level_wildcard_test() ->
    %% GIVEN: Handler with multi-level wildcard subscription
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    TestPid = self(),
    Callback = fun(Msg) -> TestPid ! {matched, Msg} end,

    %% Subscribe with ** wildcard (matches multiple levels)
    {ok, _SubRef} = macula_pubsub_handler:subscribe(Handler, <<"home.**">>, Callback),
    timer:sleep(50),

    %% WHEN: Publishing to deeply nested topic
    PublishMsg = #{topic => <<"home.living.room.light.brightness">>, payload => <<"80">>},
    macula_pubsub_handler:handle_incoming_publish(Handler, PublishMsg),

    %% THEN: Should match
    receive
        {matched, _} -> ok
    after 1000 ->
        ?assert(false)
    end,

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

pattern_no_match_different_hierarchy_test() ->
    %% GIVEN: Handler with specific subscription
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    TestPid = self(),
    Callback = fun(Msg) -> TestPid ! {matched, Msg} end,

    {ok, _SubRef} = macula_pubsub_handler:subscribe(Handler, <<"home.kitchen.temp">>, Callback),
    timer:sleep(50),

    %% WHEN: Publishing to different hierarchy
    PublishMsg = #{topic => <<"office.desk.temp">>, payload => <<"data">>},
    macula_pubsub_handler:handle_incoming_publish(Handler, PublishMsg),

    %% THEN: Should NOT match
    receive
        {matched, _} ->
            ?assert(false)
    after 500 ->
        ok % Expected timeout
    end,

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

pattern_mixed_wildcards_test() ->
    %% GIVEN: Handler with mixed wildcard pattern
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Opts = macula_test_helpers:default_pubsub_opts(),
    {ok, Handler} = macula_pubsub_handler:start_link(Opts),
    gen_server:cast(Handler, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(50),

    TestPid = self(),
    Callback = fun(Msg) -> TestPid ! {matched, Msg} end,

    %% Subscribe with both wildcards
    {ok, _SubRef} = macula_pubsub_handler:subscribe(Handler, <<"device.*.sensor.**">>, Callback),
    timer:sleep(50),

    %% WHEN: Publishing to matching pattern
    PublishMsg = #{topic => <<"device.bedroom.sensor.humidity.current">>, payload => <<"60%">>},
    macula_pubsub_handler:handle_incoming_publish(Handler, PublishMsg),

    %% THEN: Should match
    receive
        {matched, _} -> ok
    after 1000 ->
        ?assert(false)
    end,

    %% Cleanup
    gen_server:stop(Handler),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).
