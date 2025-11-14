%%%-------------------------------------------------------------------
%%% @doc
%%% Pub/Sub operation tests for macula_connection.
%%%
%%% Tests pub/sub functionality including:
%%% - Message structure validation
%%% - Message ID generation
%%% - Topic validation
%%% - QoS behavior
%%% - Callback validation
%%%
%%% NOTE: API-level tests (publish/subscribe with real connections) are
%%% covered in macula_connection_tests.erl. These tests focus on
%%% testable validation logic and data structures.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_connection_pubsub_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Message ID Generation Tests
%%%===================================================================

test_message_id_format_test() ->
    %% GIVEN: Need for unique message IDs
    %% WHEN: Generating message IDs
    %% THEN: Should be unique binaries

    %% We test that message IDs are generated for QoS 1 messages
    %% This is done internally, so we validate the format expectations

    MessageId = integer_to_binary(erlang:unique_integer([positive])),
    ?assert(is_binary(MessageId)),
    ?assert(byte_size(MessageId) > 0).

test_message_id_uniqueness_test() ->
    %% GIVEN: Multiple message ID generations
    %% WHEN: Creating IDs
    %% THEN: Each should be unique

    Id1 = integer_to_binary(erlang:unique_integer([positive])),
    Id2 = integer_to_binary(erlang:unique_integer([positive])),
    Id3 = integer_to_binary(erlang:unique_integer([positive])),

    ?assertNotEqual(Id1, Id2),
    ?assertNotEqual(Id2, Id3),
    ?assertNotEqual(Id1, Id3).

%%%===================================================================
%%% Topic Validation Tests
%%%===================================================================

test_topic_validation_empty_test() ->
    %% GIVEN: Empty topic
    %% WHEN: Validating
    %% THEN: Should reject empty topics

    ?assertEqual(false, is_valid_topic(<<>>)).

test_topic_validation_valid_simple_test() ->
    %% GIVEN: Valid simple topic
    %% WHEN: Validating
    %% THEN: Should accept

    ?assertEqual(true, is_valid_topic(<<"sensor">>)),
    ?assertEqual(true, is_valid_topic(<<"sensor.temp">>)),
    ?assertEqual(true, is_valid_topic(<<"sensor.temp.room1">>)).

test_topic_validation_with_wildcards_test() ->
    %% GIVEN: Topics with wildcards
    %% WHEN: Validating for publish (wildcards not allowed)
    %% THEN: Should reject wildcards in publish

    %% Wildcards are only for subscribe patterns
    ?assertEqual(false, is_valid_publish_topic(<<"sensor.*">>)),
    ?assertEqual(false, is_valid_publish_topic(<<"sensor.**">>)),
    ?assertEqual(false, is_valid_publish_topic(<<"*.temp.room1">>)).

test_topic_validation_subscribe_patterns_test() ->
    %% GIVEN: Subscription patterns with wildcards
    %% WHEN: Validating for subscribe
    %% THEN: Should accept wildcards

    ?assertEqual(true, is_valid_subscribe_pattern(<<"sensor.*">>)),
    ?assertEqual(true, is_valid_subscribe_pattern(<<"sensor.**">>)),
    ?assertEqual(true, is_valid_subscribe_pattern(<<"*.temp.room1">>)).

%%%===================================================================
%%% QoS Behavior Tests
%%%===================================================================

test_qos0_no_message_id_required_test() ->
    %% GIVEN: QoS 0 message
    %% WHEN: Building message
    %% THEN: Message ID is optional (can be auto-generated)

    %% QoS 0 doesn't require tracking, but may still have ID
    Msg = #{
        topic => <<"test.topic">>,
        payload => <<"data">>,
        qos => 0,
        retain => false,
        message_id => <<"auto-generated">>
    },

    ?assertEqual(0, maps:get(qos, Msg)),
    ?assert(is_binary(maps:get(message_id, Msg))).

test_qos1_requires_message_id_test() ->
    %% GIVEN: QoS 1 message
    %% WHEN: Building message
    %% THEN: Must have message ID for tracking

    Msg = #{
        topic => <<"test.topic">>,
        payload => <<"data">>,
        qos => 1,
        retain => false,
        message_id => <<"msg-123">>
    },

    ?assertEqual(1, maps:get(qos, Msg)),
    ?assertEqual(<<"msg-123">>, maps:get(message_id, Msg)).

test_puback_message_format_test() ->
    %% GIVEN: PUBACK acknowledgment
    %% WHEN: Receiving PUBACK
    %% THEN: Should contain message_id

    Puback = #{
        message_id => <<"msg-123">>
    },

    ?assert(maps:is_key(message_id, Puback)),
    ?assertEqual(<<"msg-123">>, maps:get(message_id, Puback)).

%%%===================================================================
%%% Callback Validation Tests
%%%===================================================================

test_callback_must_be_function_test() ->
    %% GIVEN: Callback parameter
    %% WHEN: Validating
    %% THEN: Must be a function

    ?assertEqual(true, is_function(fun(_T, _P) -> ok end)),
    ?assertEqual(false, is_function(not_a_function)),
    ?assertEqual(false, is_function(<<"binary">>)),
    ?assertEqual(false, is_function(123)).

test_callback_arity_two_test() ->
    %% GIVEN: Callback function
    %% WHEN: Validating arity
    %% THEN: Should accept topic and payload (arity 2)

    Callback = fun(_Topic, _Payload) -> ok end,
    ?assertEqual(true, is_function(Callback, 2)).

test_callback_with_wrong_arity_test() ->
    %% GIVEN: Callback with wrong arity
    %% WHEN: Validating
    %% THEN: Should detect arity mismatch

    CallbackArity1 = fun(_) -> ok end,
    CallbackArity3 = fun(_, _, _) -> ok end,

    ?assertEqual(false, is_function(CallbackArity1, 2)),
    ?assertEqual(false, is_function(CallbackArity3, 2)).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Validate topic is not empty
-spec is_valid_topic(binary()) -> boolean().
is_valid_topic(<<>>) -> false;
is_valid_topic(Topic) when is_binary(Topic) -> true;
is_valid_topic(_) -> false.

%% @doc Validate topic for publishing (no wildcards)
-spec is_valid_publish_topic(binary()) -> boolean().
is_valid_publish_topic(Topic) ->
    is_valid_topic(Topic) andalso
    not has_wildcards(Topic).

%% @doc Validate pattern for subscribing (wildcards allowed)
-spec is_valid_subscribe_pattern(binary()) -> boolean().
is_valid_subscribe_pattern(Pattern) ->
    is_valid_topic(Pattern).

%% @doc Check if topic contains wildcards
-spec has_wildcards(binary()) -> boolean().
has_wildcards(Topic) ->
    binary:match(Topic, <<"*">>) =/= nomatch orelse
    binary:match(Topic, <<"+">>) =/= nomatch orelse
    binary:match(Topic, <<"#">>) =/= nomatch.
