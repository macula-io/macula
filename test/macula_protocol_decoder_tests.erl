%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_protocol_decoder module.
%%% Tests protocol message decoding from binary wire format.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_protocol_decoder_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% Helper to create encoded messages using encoder
encode_message(Type, Msg) ->
    macula_protocol_encoder:encode(Type, Msg).

%% Sample messages for testing
sample_ping_msg() ->
    #{timestamp => erlang:system_time(millisecond)}.

sample_pong_msg() ->
    #{
        timestamp => erlang:system_time(millisecond),
        server_time => erlang:system_time(millisecond)
    }.

sample_connect_msg() ->
    #{
        version => <<"1.0">>,
        node_id => crypto:strong_rand_bytes(32),
        realm_id => crypto:strong_rand_bytes(32),
        capabilities => [pubsub, rpc]
    }.

sample_disconnect_msg() ->
    #{
        reason => normal,
        message => <<"Goodbye">>
    }.

sample_publish_msg() ->
    #{
        topic => <<"test.topic">>,
        payload => <<"Hello, World!">>,
        qos => 1,
        retain => false,
        message_id => crypto:strong_rand_bytes(16)
    }.

sample_subscribe_msg() ->
    #{
        topics => [<<"test.topic">>, <<"test.#">>],
        qos => 1
    }.

sample_unsubscribe_msg() ->
    #{topics => [<<"test.topic">>]}.

%%%===================================================================
%%% Basic Decoding Tests
%%%===================================================================

decode_returns_ok_tuple_test() ->
    Binary = encode_message(ping, sample_ping_msg()),
    Result = macula_protocol_decoder:decode(Binary),
    ?assertMatch({ok, {ping, _}}, Result).

decode_returns_correct_type_test() ->
    Binary = encode_message(pong, sample_pong_msg()),
    {ok, {Type, _Msg}} = macula_protocol_decoder:decode(Binary),
    ?assertEqual(pong, Type).

decode_returns_map_test() ->
    Binary = encode_message(ping, sample_ping_msg()),
    {ok, {_Type, Msg}} = macula_protocol_decoder:decode(Binary),
    ?assert(is_map(Msg)).

%%%===================================================================
%%% Message Type Decoding Tests
%%%===================================================================

decode_connect_message_test() ->
    Binary = encode_message(connect, sample_connect_msg()),
    {ok, {Type, _Msg}} = macula_protocol_decoder:decode(Binary),
    ?assertEqual(connect, Type).

decode_disconnect_message_test() ->
    Binary = encode_message(disconnect, sample_disconnect_msg()),
    {ok, {Type, _Msg}} = macula_protocol_decoder:decode(Binary),
    ?assertEqual(disconnect, Type).

decode_ping_message_test() ->
    Binary = encode_message(ping, sample_ping_msg()),
    {ok, {Type, _Msg}} = macula_protocol_decoder:decode(Binary),
    ?assertEqual(ping, Type).

decode_pong_message_test() ->
    Binary = encode_message(pong, sample_pong_msg()),
    {ok, {Type, _Msg}} = macula_protocol_decoder:decode(Binary),
    ?assertEqual(pong, Type).

decode_publish_message_test() ->
    Binary = encode_message(publish, sample_publish_msg()),
    {ok, {Type, _Msg}} = macula_protocol_decoder:decode(Binary),
    ?assertEqual(publish, Type).

decode_subscribe_message_test() ->
    Binary = encode_message(subscribe, sample_subscribe_msg()),
    {ok, {Type, _Msg}} = macula_protocol_decoder:decode(Binary),
    ?assertEqual(subscribe, Type).

decode_unsubscribe_message_test() ->
    Binary = encode_message(unsubscribe, sample_unsubscribe_msg()),
    {ok, {Type, _Msg}} = macula_protocol_decoder:decode(Binary),
    ?assertEqual(unsubscribe, Type).

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

decode_empty_binary_returns_error_test() ->
    Binary = <<>>,
    Result = macula_protocol_decoder:decode(Binary),
    ?assertEqual({error, invalid_header}, Result).

decode_too_short_header_returns_error_test() ->
    Binary = <<1, 2, 3>>,  %% Only 3 bytes, need 8
    Result = macula_protocol_decoder:decode(Binary),
    ?assertEqual({error, invalid_header}, Result).

decode_wrong_version_returns_error_test() ->
    %% Create header with wrong version (2 instead of 1)
    Binary = <<2, 3, 0, 0, 0:32/big-unsigned>>,
    Result = macula_protocol_decoder:decode(Binary),
    ?assertEqual({error, {unsupported_version, 2}}, Result).

decode_unknown_type_returns_error_test() ->
    %% Create header with unknown type ID (99)
    Binary = <<1, 99, 0, 0, 0:32/big-unsigned>>,
    Result = macula_protocol_decoder:decode(Binary),
    ?assertEqual({error, {unknown_type, 99}}, Result).

decode_incomplete_payload_returns_error_test() ->
    %% Header claims 100 bytes payload but only provide 10
    Binary = <<1, 3, 0, 0, 100:32/big-unsigned, (crypto:strong_rand_bytes(10))/binary>>,
    Result = macula_protocol_decoder:decode(Binary),
    ?assertEqual({error, incomplete_payload}, Result).

decode_invalid_msgpack_returns_error_test() ->
    %% Valid header but invalid MessagePack payload
    InvalidPayload = <<255, 255, 255>>,  %% Invalid MessagePack
    PayloadLen = byte_size(InvalidPayload),
    Binary = <<1, 3, 0, 0, PayloadLen:32/big-unsigned, InvalidPayload/binary>>,
    Result = macula_protocol_decoder:decode(Binary),
    ?assertMatch({error, {msgpack_decode_error, _}}, Result).

%%%===================================================================
%%% Round-trip Tests (Encode → Decode)
%%%===================================================================

roundtrip_ping_preserves_data_test() ->
    OrigMsg = sample_ping_msg(),
    Encoded = encode_message(ping, OrigMsg),
    {ok, {ping, Decoded}} = macula_protocol_decoder:decode(Encoded),
    %% MessagePack converts atom keys to binary keys
    ?assertEqual(maps:get(timestamp, OrigMsg), maps:get(<<"timestamp">>, Decoded)).

roundtrip_pong_preserves_data_test() ->
    OrigMsg = sample_pong_msg(),
    Encoded = encode_message(pong, OrigMsg),
    {ok, {pong, Decoded}} = macula_protocol_decoder:decode(Encoded),
    ?assertEqual(maps:get(timestamp, OrigMsg), maps:get(<<"timestamp">>, Decoded)),
    ?assertEqual(maps:get(server_time, OrigMsg), maps:get(<<"server_time">>, Decoded)).

roundtrip_connect_preserves_data_test() ->
    OrigMsg = sample_connect_msg(),
    Encoded = encode_message(connect, OrigMsg),
    {ok, {connect, Decoded}} = macula_protocol_decoder:decode(Encoded),
    ?assertEqual(maps:get(version, OrigMsg), maps:get(<<"version">>, Decoded)),
    ?assertEqual(maps:get(node_id, OrigMsg), maps:get(<<"node_id">>, Decoded)),
    ?assertEqual(maps:get(realm_id, OrigMsg), maps:get(<<"realm_id">>, Decoded)).

roundtrip_disconnect_preserves_data_test() ->
    OrigMsg = sample_disconnect_msg(),
    Encoded = encode_message(disconnect, OrigMsg),
    {ok, {disconnect, Decoded}} = macula_protocol_decoder:decode(Encoded),
    ?assertEqual(maps:get(message, OrigMsg), maps:get(<<"message">>, Decoded)).

roundtrip_publish_preserves_data_test() ->
    OrigMsg = sample_publish_msg(),
    Encoded = encode_message(publish, OrigMsg),
    {ok, {publish, Decoded}} = macula_protocol_decoder:decode(Encoded),
    ?assertEqual(maps:get(topic, OrigMsg), maps:get(<<"topic">>, Decoded)),
    ?assertEqual(maps:get(payload, OrigMsg), maps:get(<<"payload">>, Decoded)),
    ?assertEqual(maps:get(qos, OrigMsg), maps:get(<<"qos">>, Decoded)),
    ?assertEqual(maps:get(message_id, OrigMsg), maps:get(<<"message_id">>, Decoded)).

roundtrip_subscribe_preserves_data_test() ->
    OrigMsg = sample_subscribe_msg(),
    Encoded = encode_message(subscribe, OrigMsg),
    {ok, {subscribe, Decoded}} = macula_protocol_decoder:decode(Encoded),
    ?assertEqual(maps:get(topics, OrigMsg), maps:get(<<"topics">>, Decoded)),
    ?assertEqual(maps:get(qos, OrigMsg), maps:get(<<"qos">>, Decoded)).

roundtrip_unsubscribe_preserves_data_test() ->
    OrigMsg = sample_unsubscribe_msg(),
    Encoded = encode_message(unsubscribe, OrigMsg),
    {ok, {unsubscribe, Decoded}} = macula_protocol_decoder:decode(Encoded),
    ?assertEqual(maps:get(topics, OrigMsg), maps:get(<<"topics">>, Decoded)).

%%%===================================================================
%%% Data Type Preservation Tests
%%%===================================================================

roundtrip_preserves_integers_test() ->
    Msg = #{count => 42, negative => -100, zero => 0},
    Encoded = encode_message(call, Msg),  %% Use 'call' type - no strict validation
    {ok, {call, Decoded}} = macula_protocol_decoder:decode(Encoded),
    ?assertEqual(42, maps:get(<<"count">>, Decoded)),
    ?assertEqual(-100, maps:get(<<"negative">>, Decoded)),
    ?assertEqual(0, maps:get(<<"zero">>, Decoded)).

roundtrip_preserves_binaries_test() ->
    Msg = #{data => <<1, 2, 3, 255>>, empty => <<>>},
    Encoded = encode_message(call, Msg),  %% Use 'call' type - no strict validation
    {ok, {call, Decoded}} = macula_protocol_decoder:decode(Encoded),
    ?assertEqual(<<1, 2, 3, 255>>, maps:get(<<"data">>, Decoded)),
    ?assertEqual(<<>>, maps:get(<<"empty">>, Decoded)).

roundtrip_preserves_lists_test() ->
    Msg = #{items => [1, 2, 3], empty => []},
    Encoded = encode_message(call, Msg),  %% Use 'call' type - no strict validation
    {ok, {call, Decoded}} = macula_protocol_decoder:decode(Encoded),
    ?assertEqual([1, 2, 3], maps:get(<<"items">>, Decoded)),
    ?assertEqual([], maps:get(<<"empty">>, Decoded)).

roundtrip_preserves_nested_maps_test() ->
    Msg = #{
        nested => #{inner => <<"value">>},
        deep => #{level1 => #{level2 => 123}}
    },
    Encoded = encode_message(call, Msg),  %% Use 'call' type - no strict validation
    {ok, {call, Decoded}} = macula_protocol_decoder:decode(Encoded),
    ?assertEqual(#{<<"inner">> => <<"value">>}, maps:get(<<"nested">>, Decoded)),
    ?assertEqual(#{<<"level1">> => #{<<"level2">> => 123}}, maps:get(<<"deep">>, Decoded)).

roundtrip_preserves_booleans_test() ->
    Msg = #{flag_true => true, flag_false => false},
    Encoded = encode_message(call, Msg),  %% Use 'call' type - no strict validation
    {ok, {call, Decoded}} = macula_protocol_decoder:decode(Encoded),
    ?assertEqual(true, maps:get(<<"flag_true">>, Decoded)),
    ?assertEqual(false, maps:get(<<"flag_false">>, Decoded)).

%%%===================================================================
%%% Edge Cases
%%%===================================================================

decode_with_extra_data_after_payload_test() ->
    %% Decoder should handle extra data after declared payload
    Binary = encode_message(ping, sample_ping_msg()),
    ExtraData = <<1, 2, 3, 4, 5>>,
    BinaryWithExtra = <<Binary/binary, ExtraData/binary>>,
    Result = macula_protocol_decoder:decode(BinaryWithExtra),
    ?assertMatch({ok, {ping, _}}, Result).

decode_empty_payload_test() ->
    %% Message with empty map payload
    Msg = #{},
    Encoded = encode_message(call, Msg),  %% Use 'call' type - no strict validation
    {ok, {call, Decoded}} = macula_protocol_decoder:decode(Encoded),
    ?assertEqual(#{}, Decoded).

decode_large_payload_test() ->
    %% Test with 64KB payload
    LargeData = crypto:strong_rand_bytes(65536),
    Msg = #{large_data => LargeData},
    Encoded = encode_message(call, Msg),  %% Use 'call' type - no strict validation
    {ok, {call, Decoded}} = macula_protocol_decoder:decode(Encoded),
    ?assertEqual(LargeData, maps:get(<<"large_data">>, Decoded)).

decode_many_fields_test() ->
    %% Message with many fields
    Msg = maps:from_list([{list_to_atom("field_" ++ integer_to_list(N)), N} || N <- lists:seq(1, 100)]),
    Encoded = encode_message(call, Msg),  %% Use 'call' type - no strict validation
    {ok, {call, Decoded}} = macula_protocol_decoder:decode(Encoded),
    %% Verify all fields present (keys are now binaries)
    ?assertEqual(100, maps:size(Decoded)).

%%%===================================================================
%%% Symmetry Tests (Decode should be inverse of Encode)
%%%===================================================================

encode_decode_symmetry_test() ->
    %% For any valid message, decode(encode(msg)) should preserve data
    Messages = [
        {ping, sample_ping_msg()},
        {pong, sample_pong_msg()},
        {connect, sample_connect_msg()},
        {disconnect, sample_disconnect_msg()},
        {publish, sample_publish_msg()},
        {subscribe, sample_subscribe_msg()},
        {unsubscribe, sample_unsubscribe_msg()}
    ],
    lists:foreach(fun({Type, Msg}) ->
        Encoded = encode_message(Type, Msg),
        {ok, {DecodedType, _DecodedMsg}} = macula_protocol_decoder:decode(Encoded),
        ?assertEqual(Type, DecodedType)
    end, Messages).
