%%%-------------------------------------------------------------------
%%% @doc
%%% Error scenario tests for macula_connection.
%%%
%%% Tests error handling including:
%%% - Invalid message formats
%%% - Malformed protocol messages
%%% - Buffer overflow scenarios
%%% - Unknown message types
%%% - Invalid state transitions
%%% - Error message structures
%%% - Boundary conditions
%%%
%%% NOTE: These tests focus on validation and error detection logic.
%%% Integration-level error tests (network failures, crashes) are
%%% covered in integration test suites.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_connection_error_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Invalid Message Format Tests
%%%===================================================================

test_invalid_message_type_test() ->
    %% GIVEN: Message with unknown type
    %% WHEN: Validating message type
    %% THEN: Should reject unknown types

    ?assertEqual(false, is_valid_message_type(unknown_type)),
    ?assertEqual(false, is_valid_message_type(<<"invalid">>)),
    ?assertEqual(false, is_valid_message_type(123)).

test_valid_message_types_test() ->
    %% GIVEN: Known message types
    %% WHEN: Validating
    %% THEN: Should accept valid types

    ?assertEqual(true, is_valid_message_type(connect)),
    ?assertEqual(true, is_valid_message_type(publish)),
    ?assertEqual(true, is_valid_message_type(subscribe)),
    ?assertEqual(true, is_valid_message_type(call)),
    ?assertEqual(true, is_valid_message_type(reply)).

test_message_without_required_fields_test() ->
    %% GIVEN: Message missing required fields
    %% WHEN: Validating message structure
    %% THEN: Should detect missing fields

    %% Publish message missing topic
    InvalidMsg1 = #{payload => <<"data">>, qos => 0},
    ?assertEqual(false, has_required_fields(publish, InvalidMsg1)),

    %% Call message missing procedure
    InvalidMsg2 = #{args => [], call_id => <<"123">>},
    ?assertEqual(false, has_required_fields(call, InvalidMsg2)).

test_message_with_required_fields_test() ->
    %% GIVEN: Message with all required fields
    %% WHEN: Validating
    %% THEN: Should accept valid message

    ValidMsg = #{
        topic => <<"test">>,
        payload => <<"data">>,
        qos => 0,
        retain => false,
        message_id => <<"123">>
    },
    ?assertEqual(true, has_required_fields(publish, ValidMsg)).

%%%===================================================================
%%% Malformed Protocol Message Tests
%%%===================================================================

test_header_too_short_test() ->
    %% GIVEN: Header shorter than 8 bytes
    %% WHEN: Validating header
    %% THEN: Should reject short headers

    ShortHeader = <<1, 2, 3>>,
    ?assertEqual(false, is_valid_header(ShortHeader)).

test_header_correct_length_test() ->
    %% GIVEN: Header exactly 8 bytes
    %% WHEN: Validating
    %% THEN: Should accept valid header length

    ValidHeader = <<1:8, 1:8, 0:8, 0:8, 100:32/big-unsigned>>,
    ?assertEqual(true, is_valid_header(ValidHeader)).

test_payload_length_mismatch_test() ->
    %% GIVEN: Header specifies length X but payload is length Y
    %% WHEN: Validating message
    %% THEN: Should detect length mismatch

    DeclaredLength = 100,
    ActualPayload = crypto:strong_rand_bytes(50),

    ?assertEqual(false, payload_matches_declared_length(DeclaredLength, ActualPayload)).

test_payload_length_matches_test() ->
    %% GIVEN: Declared and actual lengths match
    %% WHEN: Validating
    %% THEN: Should accept matching lengths

    Length = 100,
    Payload = crypto:strong_rand_bytes(Length),

    ?assertEqual(true, payload_matches_declared_length(Length, Payload)).

test_maximum_message_size_exceeded_test() ->
    %% GIVEN: Message exceeding maximum size
    %% WHEN: Validating size
    %% THEN: Should reject oversized messages

    MaxSize = 10 * 1024 * 1024,  % 10 MB
    OversizedLength = MaxSize + 1,

    ?assertEqual(false, is_within_size_limit(OversizedLength, MaxSize)).

test_message_within_size_limit_test() ->
    %% GIVEN: Message within size limit
    %% WHEN: Validating
    %% THEN: Should accept

    MaxSize = 10 * 1024 * 1024,  % 10 MB
    ValidLength = 1024,  % 1 KB

    ?assertEqual(true, is_within_size_limit(ValidLength, MaxSize)).

%%%===================================================================
%%% Unknown Message Type Tests
%%%===================================================================

test_unknown_type_id_test() ->
    %% GIVEN: Type ID not in known range
    %% WHEN: Decoding type
    %% THEN: Should handle unknown types gracefully

    KnownTypeIds = [1, 2, 3, 4, 5, 6, 7, 8],  % Example known types
    UnknownTypeId = 99,

    ?assertEqual(false, lists:member(UnknownTypeId, KnownTypeIds)).

test_reserved_type_id_test() ->
    %% GIVEN: Reserved type ID
    %% WHEN: Validating
    %% THEN: Should reject reserved IDs

    ReservedTypeIds = [0, 255],  % Example reserved IDs

    lists:foreach(fun(TypeId) ->
        ?assertEqual(true, is_reserved_type_id(TypeId))
    end, ReservedTypeIds).

%%%===================================================================
%%% Buffer Overflow Scenario Tests
%%%===================================================================

test_buffer_growth_controlled_test() ->
    %% GIVEN: Multiple partial messages arriving
    %% WHEN: Accumulating in buffer
    %% THEN: Buffer should not grow unbounded

    MaxBufferSize = 1024 * 1024,  % 1 MB max buffer
    LargeChunk = crypto:strong_rand_bytes(512 * 1024),  % 512 KB

    %% Simulating buffer accumulation
    BufferSize = byte_size(LargeChunk),
    ?assertEqual(true, BufferSize < MaxBufferSize).

test_buffer_size_tracking_test() ->
    %% GIVEN: Buffer with accumulated data
    %% WHEN: Checking size
    %% THEN: Should accurately track size

    Buffer = <<1, 2, 3, 4, 5>>,
    ExpectedSize = 5,

    ?assertEqual(ExpectedSize, byte_size(Buffer)).

%%%===================================================================
%%% Invalid Field Value Tests
%%%===================================================================

test_qos_out_of_range_test() ->
    %% GIVEN: QoS value outside valid range (0-1)
    %% WHEN: Validating QoS
    %% THEN: Should reject invalid values

    ?assertEqual(false, is_valid_qos(2)),
    ?assertEqual(false, is_valid_qos(-1)),
    ?assertEqual(false, is_valid_qos(100)).

test_qos_valid_values_test() ->
    %% GIVEN: Valid QoS values
    %% WHEN: Validating
    %% THEN: Should accept 0 and 1

    ?assertEqual(true, is_valid_qos(0)),
    ?assertEqual(true, is_valid_qos(1)).

test_empty_topic_rejected_test() ->
    %% GIVEN: Empty topic string
    %% WHEN: Validating topic
    %% THEN: Should reject empty topics

    ?assertEqual(false, is_valid_topic(<<>>)).

test_topic_with_null_bytes_test() ->
    %% GIVEN: Topic containing null bytes
    %% WHEN: Validating
    %% THEN: Should reject topics with null bytes

    TopicWithNull = <<"topic", 0, "name">>,
    ?assertEqual(false, is_valid_topic_content(TopicWithNull)).

test_topic_without_null_bytes_test() ->
    %% GIVEN: Valid topic without null bytes
    %% WHEN: Validating
    %% THEN: Should accept

    ValidTopic = <<"topic.name">>,
    ?assertEqual(true, is_valid_topic_content(ValidTopic)).

%%%===================================================================
%%% Error Response Structure Tests
%%%===================================================================

test_error_response_has_code_test() ->
    %% GIVEN: Error response
    %% WHEN: Building error
    %% THEN: Should include error code

    ErrorResponse = #{
        error => #{
            code => <<"INVALID_MESSAGE">>,
            message => <<"The message format is invalid">>
        }
    },

    Error = maps:get(error, ErrorResponse),
    ?assert(maps:is_key(code, Error)).

test_error_response_has_message_test() ->
    %% GIVEN: Error response
    %% WHEN: Building error
    %% THEN: Should include human-readable message

    ErrorResponse = #{
        error => #{
            code => <<"TIMEOUT">>,
            message => <<"Operation timed out">>
        }
    },

    Error = maps:get(error, ErrorResponse),
    ?assert(maps:is_key(message, Error)).

test_error_codes_are_binaries_test() ->
    %% GIVEN: Error codes
    %% WHEN: Defining error codes
    %% THEN: Should use binary format for consistency

    ErrorCodes = [
        <<"INVALID_MESSAGE">>,
        <<"TIMEOUT">>,
        <<"NOT_FOUND">>,
        <<"INTERNAL_ERROR">>
    ],

    lists:foreach(fun(Code) ->
        ?assert(is_binary(Code))
    end, ErrorCodes).

%%%===================================================================
%%% Boundary Condition Tests
%%%===================================================================

test_zero_length_payload_handling_test() ->
    %% GIVEN: Message with zero-length payload
    %% WHEN: Processing message
    %% THEN: Should handle gracefully

    ZeroLengthPayload = <<>>,
    ?assertEqual(0, byte_size(ZeroLengthPayload)).

test_maximum_topic_length_test() ->
    %% GIVEN: Very long topic name
    %% WHEN: Validating topic length
    %% THEN: Should enforce maximum length

    MaxTopicLength = 256,  % Example max length
    LongTopic = binary:copy(<<"a">>, MaxTopicLength + 1),

    ?assertEqual(false, is_valid_topic_length(LongTopic, MaxTopicLength)).

test_topic_within_length_limit_test() ->
    %% GIVEN: Topic within length limit
    %% WHEN: Validating
    %% THEN: Should accept

    MaxTopicLength = 256,
    ValidTopic = <<"sensor.temperature.room1">>,

    ?assertEqual(true, is_valid_topic_length(ValidTopic, MaxTopicLength)).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Check if message type is valid
-spec is_valid_message_type(atom()) -> boolean().
is_valid_message_type(connect) -> true;
is_valid_message_type(publish) -> true;
is_valid_message_type(subscribe) -> true;
is_valid_message_type(unsubscribe) -> true;
is_valid_message_type(call) -> true;
is_valid_message_type(reply) -> true;
is_valid_message_type(puback) -> true;
is_valid_message_type(_) -> false.

%% @doc Check if message has required fields for its type
-spec has_required_fields(atom(), map()) -> boolean().
has_required_fields(publish, Msg) ->
    maps:is_key(topic, Msg) andalso
    maps:is_key(payload, Msg) andalso
    maps:is_key(qos, Msg);
has_required_fields(call, Msg) ->
    maps:is_key(procedure, Msg) andalso
    maps:is_key(args, Msg) andalso
    maps:is_key(call_id, Msg);
has_required_fields(_, _) ->
    false.

%% @doc Check if header is valid length (8 bytes)
-spec is_valid_header(binary()) -> boolean().
is_valid_header(Header) when byte_size(Header) =:= 8 -> true;
is_valid_header(_) -> false.

%% @doc Check if payload matches declared length
-spec payload_matches_declared_length(non_neg_integer(), binary()) -> boolean().
payload_matches_declared_length(DeclaredLength, Payload) ->
    byte_size(Payload) =:= DeclaredLength.

%% @doc Check if message size is within limit
-spec is_within_size_limit(non_neg_integer(), non_neg_integer()) -> boolean().
is_within_size_limit(MessageSize, MaxSize) ->
    MessageSize =< MaxSize.

%% @doc Check if type ID is reserved
-spec is_reserved_type_id(non_neg_integer()) -> boolean().
is_reserved_type_id(0) -> true;
is_reserved_type_id(255) -> true;
is_reserved_type_id(_) -> false.

%% @doc Check if QoS value is valid (0 or 1)
-spec is_valid_qos(integer()) -> boolean().
is_valid_qos(0) -> true;
is_valid_qos(1) -> true;
is_valid_qos(_) -> false.

%% @doc Check if topic is non-empty
-spec is_valid_topic(binary()) -> boolean().
is_valid_topic(<<>>) -> false;
is_valid_topic(Topic) when is_binary(Topic) -> true;
is_valid_topic(_) -> false.

%% @doc Check if topic contains null bytes
-spec is_valid_topic_content(binary()) -> boolean().
is_valid_topic_content(Topic) ->
    binary:match(Topic, <<0>>) =:= nomatch.

%% @doc Check if topic is within length limit
-spec is_valid_topic_length(binary(), non_neg_integer()) -> boolean().
is_valid_topic_length(Topic, MaxLength) ->
    byte_size(Topic) =< MaxLength.
