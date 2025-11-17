%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_pubsub_qos module.
%%%
%%% Tests QoS 1 (at-least-once delivery) management.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_qos_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% track_message/5 Tests
%%%===================================================================

track_qos0_message_test() ->
    %% GIVEN: Empty pending map
    Pending = #{},
    MsgId = <<"msg_001">>,
    Topic = <<"test.topic">>,
    Payload = <<"data">>,

    %% WHEN: Tracking QoS 0 message
    {ok, UpdatedPending} = macula_pubsub_qos:track_message(MsgId, Topic, Payload, 0, Pending),

    %% THEN: Should not track (QoS 0 has no acknowledgment)
    ?assertEqual(#{}, UpdatedPending).

track_qos1_message_test() ->
    %% GIVEN: Empty pending map
    Pending = #{},
    MsgId = <<"msg_002">>,
    Topic = <<"qos1.topic">>,
    Payload = <<"qos1_data">>,

    %% WHEN: Tracking QoS 1 message
    {ok, UpdatedPending} = macula_pubsub_qos:track_message(MsgId, Topic, Payload, 1, Pending),

    %% THEN: Message should be tracked
    ?assertMatch(#{<<"msg_002">> := {<<"qos1.topic">>, <<"qos1_data">>, 1, 0, _TimerRef}}, UpdatedPending),

    %% THEN: Timer reference should be valid
    {Topic, Payload, 1, 0, TimerRef} = maps:get(MsgId, UpdatedPending),
    ?assert(is_reference(TimerRef)).

track_multiple_messages_test() ->
    %% GIVEN: Pending map with one message
    Pending1 = #{},
    {ok, Pending2} = macula_pubsub_qos:track_message(<<"msg_1">>, <<"topic.1">>, <<"data1">>, 1, Pending1),

    %% WHEN: Tracking another message
    {ok, Pending3} = macula_pubsub_qos:track_message(<<"msg_2">>, <<"topic.2">>, <<"data2">>, 1, Pending2),

    %% THEN: Both messages should be tracked
    ?assertEqual(2, maps:size(Pending3)),
    ?assertMatch(#{<<"msg_1">> := _, <<"msg_2">> := _}, Pending3).

%%%===================================================================
%%% handle_ack/2 Tests
%%%===================================================================

handle_ack_removes_message_test() ->
    %% GIVEN: Pending map with tracked message
    Pending1 = #{},
    {ok, Pending2} = macula_pubsub_qos:track_message(<<"msg_ack">>, <<"topic">>, <<"data">>, 1, Pending1),

    %% WHEN: Handling acknowledgment
    Pending3 = macula_pubsub_qos:handle_ack(<<"msg_ack">>, Pending2),

    %% THEN: Message should be removed
    ?assertEqual(#{}, Pending3).

handle_ack_cancels_timer_test() ->
    %% GIVEN: Pending map with tracked message
    Pending1 = #{},
    {ok, Pending2} = macula_pubsub_qos:track_message(<<"msg_timer">>, <<"topic">>, <<"data">>, 1, Pending1),
    {_Topic, _Payload, _Qos, _Retry, TimerRef} = maps:get(<<"msg_timer">>, Pending2),

    %% WHEN: Handling acknowledgment
    _Pending3 = macula_pubsub_qos:handle_ack(<<"msg_timer">>, Pending2),

    %% THEN: Timer should be cancelled (time_left returns false)
    ?assertEqual(false, erlang:read_timer(TimerRef)).

handle_ack_nonexistent_message_test() ->
    %% GIVEN: Empty pending map
    Pending = #{},

    %% WHEN: Acknowledging nonexistent message
    UpdatedPending = macula_pubsub_qos:handle_ack(<<"nonexistent">>, Pending),

    %% THEN: Should return unchanged map
    ?assertEqual(#{}, UpdatedPending).

%%%===================================================================
%%% handle_timeout/3 Tests
%%%===================================================================

handle_timeout_not_found_test() ->
    %% GIVEN: Empty pending map
    Pending = #{},
    MockConnMgr = self(),

    %% WHEN: Handling timeout for nonexistent message
    {not_found, UpdatedPending} = macula_pubsub_qos:handle_timeout(<<"missing">>, MockConnMgr, Pending),

    %% THEN: Should return not_found
    ?assertEqual(#{}, UpdatedPending).

handle_timeout_retry_test() ->
    %% GIVEN: Pending map with message at retry 0
    Pending1 = #{},
    {ok, Pending2} = macula_pubsub_qos:track_message(<<"msg_retry">>, <<"topic">>, <<"data">>, 1, Pending1),
    MockConnMgr = self(),

    %% WHEN: Handling timeout (first retry)
    {retry, UpdatedPending, PublishMsg} = macula_pubsub_qos:handle_timeout(<<"msg_retry">>, MockConnMgr, Pending2),

    %% THEN: Should request retry
    ?assertMatch(#{topic := <<"topic">>, payload := <<"data">>, qos := 1}, PublishMsg),

    %% THEN: Retry count should be incremented
    {_Topic, _Payload, _Qos, RetryCount, _NewTimerRef} = maps:get(<<"msg_retry">>, UpdatedPending),
    ?assertEqual(1, RetryCount).

handle_timeout_max_retries_test() ->
    %% GIVEN: Pending map with message at max retries
    TimerRef = make_ref(),
    Pending = #{<<"msg_max">> => {<<"topic">>, <<"data">>, 1, 3, TimerRef}}, % 3 retries = max
    MockConnMgr = self(),

    %% WHEN: Handling timeout after max retries
    {give_up, UpdatedPending} = macula_pubsub_qos:handle_timeout(<<"msg_max">>, MockConnMgr, Pending),

    %% THEN: Should give up and remove message
    ?assertEqual(#{}, UpdatedPending).

%%%===================================================================
%%% get_pending/1 Tests
%%%===================================================================

get_pending_empty_test() ->
    %% GIVEN: Empty pending map
    Pending = #{},

    %% WHEN: Getting pending messages
    PendingList = macula_pubsub_qos:get_pending(Pending),

    %% THEN: Should return empty list
    ?assertEqual([], PendingList).

get_pending_multiple_test() ->
    %% GIVEN: Pending map with multiple messages
    Pending1 = #{},
    {ok, Pending2} = macula_pubsub_qos:track_message(<<"msg_1">>, <<"t1">>, <<"d1">>, 1, Pending1),
    {ok, Pending3} = macula_pubsub_qos:track_message(<<"msg_2">>, <<"t2">>, <<"d2">>, 1, Pending2),
    {ok, Pending4} = macula_pubsub_qos:track_message(<<"msg_3">>, <<"t3">>, <<"d3">>, 1, Pending3),

    %% WHEN: Getting pending messages
    PendingList = macula_pubsub_qos:get_pending(Pending4),

    %% THEN: Should return all message IDs
    ?assertEqual(3, length(PendingList)),
    ?assert(lists:member(<<"msg_1">>, PendingList)),
    ?assert(lists:member(<<"msg_2">>, PendingList)),
    ?assert(lists:member(<<"msg_3">>, PendingList)).
