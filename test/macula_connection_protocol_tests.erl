%%%-------------------------------------------------------------------
%%% @doc
%%% Protocol tests for macula_connection_manager message encoding/decoding.
%%%
%%% Tests message protocol handling including:
%%% - Message encoding/decoding
%%% - Buffer management and partial messages
%%% - Large message handling
%%% - Invalid message formats
%%%
%%% Note: decode_messages/2 is exported from macula_connection_manager
%%% for testing purposes only.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_connection_protocol_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Message Decoding Tests
%%%===================================================================

test_decode_messages_empty_buffer_test() ->
    Buffer = <<>>,
    {Messages, RemainingBuffer} = macula_connection_manager:decode_messages(Buffer, []),
    ?assertEqual([], Messages),
    ?assertEqual(<<>>, RemainingBuffer).

test_decode_messages_partial_header_test() ->
    Buffer = <<1, 2, 3, 4>>,
    {Messages, RemainingBuffer} = macula_connection_manager:decode_messages(Buffer, []),
    ?assertEqual([], Messages),
    ?assertEqual(<<1, 2, 3, 4>>, RemainingBuffer).

test_decode_messages_partial_payload_test() ->
    PayloadLen = 100,
    PartialPayload = crypto:strong_rand_bytes(50),
    Header = <<1:8, 1:8, 0:8, 0:8, PayloadLen:32/big-unsigned>>,
    Buffer = <<Header/binary, PartialPayload/binary>>,
    {Messages, RemainingBuffer} = macula_connection_manager:decode_messages(Buffer, []),
    ?assertEqual([], Messages),
    ?assertEqual(Buffer, RemainingBuffer).

test_decode_messages_complete_message_test() ->
    TestMsg = #{
        topic => <<"test.topic">>,
        payload => <<"test payload">>,
        qos => 0,
        retain => false,
        message_id => <<"msg-001">>
    },
    Binary = macula_protocol_encoder:encode(publish, TestMsg),
    {Messages, RemainingBuffer} = macula_connection_manager:decode_messages(Binary, []),
    ?assertEqual(1, length(Messages)),
    ?assertEqual(<<>>, RemainingBuffer),
    [{Type, DecodedMsg}] = Messages,
    ?assertEqual(publish, Type),
    ?assert(is_map(DecodedMsg)).

test_decode_messages_multiple_in_buffer_test() ->
    Msg1 = #{topic => <<"topic1">>, payload => <<"payload1">>, qos => 0, retain => false, message_id => <<"m1">>},
    Msg2 = #{topic => <<"topic2">>, payload => <<"payload2">>, qos => 0, retain => false, message_id => <<"m2">>},
    Binary1 = macula_protocol_encoder:encode(publish, Msg1),
    Binary2 = macula_protocol_encoder:encode(publish, Msg2),
    Buffer = <<Binary1/binary, Binary2/binary>>,
    {Messages, RemainingBuffer} = macula_connection_manager:decode_messages(Buffer, []),
    ?assertEqual(2, length(Messages)),
    ?assertEqual(<<>>, RemainingBuffer),
    [{Type1, _Msg1Decoded}, {Type2, _Msg2Decoded}] = Messages,
    ?assertEqual(publish, Type1),
    ?assertEqual(publish, Type2).

test_decode_messages_invalid_decode_test() ->
    PayloadLen = 10,
    InvalidPayload = <<255, 255, 255, 255, 255, 255, 255, 255, 255, 255>>,
    Header = <<1:8, 1:8, 0:8, 0:8, PayloadLen:32/big-unsigned>>,
    Buffer = <<Header/binary, InvalidPayload/binary>>,
    {Messages, RemainingBuffer} = macula_connection_manager:decode_messages(Buffer, []),
    ?assertEqual([], Messages),
    ?assertEqual(<<>>, RemainingBuffer).

test_decode_messages_zero_length_payload_test() ->
    Header = <<1:8, 1:8, 0:8, 0:8, 0:32/big-unsigned>>,
    {_Messages, RemainingBuffer} = macula_connection_manager:decode_messages(Header, []),
    ?assertEqual(<<>>, RemainingBuffer).

test_decode_messages_large_payload_test() ->
    LargePayload = crypto:strong_rand_bytes(1572864),
    TestMsg = #{
        topic => <<"large.topic">>,
        payload => LargePayload,
        qos => 0,
        retain => false,
        message_id => <<"large-msg">>
    },
    Binary = macula_protocol_encoder:encode(publish, TestMsg),
    {Messages, RemainingBuffer} = macula_connection_manager:decode_messages(Binary, []),
    ?assertEqual(1, length(Messages)),
    ?assertEqual(<<>>, RemainingBuffer),
    [{Type, DecodedMsg}] = Messages,
    ?assertEqual(publish, Type),
    ?assert(is_map(DecodedMsg)).

%%%===================================================================
%%% Buffer Management Tests
%%%===================================================================

test_buffer_partial_then_complete_test() ->
    TestMsg = #{topic => <<"test">>, payload => <<"data">>, qos => 0, retain => false, message_id => <<"test">>},
    Binary = macula_protocol_encoder:encode(publish, TestMsg),
    SplitPoint = byte_size(Binary) div 2,
    <<Part1:SplitPoint/binary, Part2/binary>> = Binary,
    {Messages1, Buffer1} = macula_connection_manager:decode_messages(Part1, []),
    ?assertEqual([], Messages1),
    ?assertEqual(Part1, Buffer1),
    CompleteBuffer = <<Buffer1/binary, Part2/binary>>,
    {Messages2, Buffer2} = macula_connection_manager:decode_messages(CompleteBuffer, []),
    ?assertEqual(1, length(Messages2)),
    ?assertEqual(<<>>, Buffer2).

test_buffer_multiple_chunks_test() ->
    Msg1 = #{topic => <<"t1">>, payload => <<"d1">>, qos => 0, retain => false, message_id => <<"m1">>},
    Msg2 = #{topic => <<"t2">>, payload => <<"d2">>, qos => 0, retain => false, message_id => <<"m2">>},
    Msg3 = #{topic => <<"t3">>, payload => <<"d3">>, qos => 0, retain => false, message_id => <<"m3">>},
    Bin1 = macula_protocol_encoder:encode(publish, Msg1),
    Bin2 = macula_protocol_encoder:encode(publish, Msg2),
    Bin3 = macula_protocol_encoder:encode(publish, Msg3),
    AllBinary = <<Bin1/binary, Bin2/binary, Bin3/binary>>,
    Size = byte_size(AllBinary),
    Chunk1Size = Size div 3,
    Chunk2Size = Size div 2 - Chunk1Size,
    <<Chunk1:Chunk1Size/binary, Chunk2:Chunk2Size/binary, Chunk3/binary>> = AllBinary,
    {Messages1, Buffer1} = macula_connection_manager:decode_messages(Chunk1, []),
    {Messages2, Buffer2} = macula_connection_manager:decode_messages(<<Buffer1/binary, Chunk2/binary>>, []),
    {Messages3, Buffer3} = macula_connection_manager:decode_messages(<<Buffer2/binary, Chunk3/binary>>, []),
    TotalMessages = length(Messages1) + length(Messages2) + length(Messages3),
    ?assertEqual(3, TotalMessages),
    ?assertEqual(<<>>, Buffer3).

%%%===================================================================
%%% JSON Encoding/Decoding Tests
%%%===================================================================

test_encode_json_simple_map_test() ->
    Data = #{name => <<"John">>, age => 30},
    Encoded = macula_utils:encode_json(Data),
    ?assert(is_binary(Encoded)),
    ?assert(byte_size(Encoded) > 0).

test_encode_json_nested_structure_test() ->
    Data = #{
        user => #{
            name => <<"Alice">>,
            address => #{city => <<"Berlin">>, country => <<"Germany">>}
        },
        items => [<<"item1">>, <<"item2">>]
    },
    Encoded = macula_utils:encode_json(Data),
    ?assert(is_binary(Encoded)),
    ?assert(byte_size(Encoded) > 0).

test_decode_json_valid_test() ->
    Original = #{name => <<"Bob">>, value => 42},
    Encoded = macula_utils:encode_json(Original),
    Decoded = macula_utils:decode_json(Encoded),
    ?assert(is_map(Decoded)).

test_round_trip_encoding_test() ->
    Original = #{
        string => <<"test">>,
        number => 123,
        nested => #{inner => <<"value">>}
    },
    Encoded = macula_utils:encode_json(Original),
    Decoded = macula_utils:decode_json(Encoded),
    ?assert(is_map(Decoded)),
    ?assert(maps:is_key(<<"string">>, Decoded) orelse maps:is_key(string, Decoded)).
