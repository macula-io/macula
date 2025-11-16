%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_protocol_encoder module.
%%% Tests protocol message encoding to binary wire format.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_protocol_encoder_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% Sample test data
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

sample_ping_msg() ->
    #{timestamp => erlang:system_time(millisecond)}.

sample_pong_msg() ->
    #{
        timestamp => erlang:system_time(millisecond),
        server_time => erlang:system_time(millisecond)
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

sample_pubsub_route_msg() ->
    #{
        destination_node_id => crypto:strong_rand_bytes(32),
        source_node_id => crypto:strong_rand_bytes(32),
        hop_count => 0,
        max_hops => 10,
        topic => <<"test.topic">>,
        payload => sample_publish_msg()
    }.

%%%===================================================================
%%% Frame Format Tests
%%%===================================================================

encode_produces_binary_test() ->
    Msg = sample_connect_msg(),
    Result = macula_protocol_encoder:encode(connect, Msg),
    ?assert(is_binary(Result)).

frame_has_correct_version_test() ->
    Msg = sample_ping_msg(),
    <<Version:8, _/binary>> = macula_protocol_encoder:encode(ping, Msg),
    ?assertEqual(1, Version).

frame_has_correct_type_id_test() ->
    Msg = sample_ping_msg(),
    <<_Version:8, TypeId:8, _/binary>> = macula_protocol_encoder:encode(ping, Msg),
    %% Ping message type is 0x03
    ?assertEqual(3, TypeId).

frame_has_correct_flags_test() ->
    Msg = sample_ping_msg(),
    <<_Version:8, _TypeId:8, Flags:8, _/binary>> = macula_protocol_encoder:encode(ping, Msg),
    ?assertEqual(0, Flags).

frame_has_correct_reserved_byte_test() ->
    Msg = sample_ping_msg(),
    <<_Version:8, _TypeId:8, _Flags:8, Reserved:8, _/binary>> = macula_protocol_encoder:encode(ping, Msg),
    ?assertEqual(0, Reserved).

frame_header_is_8_bytes_test() ->
    Msg = sample_ping_msg(),
    Binary = macula_protocol_encoder:encode(ping, Msg),
    %% Frame should be at least 8 bytes (header)
    ?assert(byte_size(Binary) >= 8).

frame_payload_length_matches_actual_test() ->
    Msg = sample_ping_msg(),
    <<_Version:8, _TypeId:8, _Flags:8, _Reserved:8, PayloadLen:32/big-unsigned, Payload/binary>> =
        macula_protocol_encoder:encode(ping, Msg),
    ?assertEqual(PayloadLen, byte_size(Payload)).

%%%===================================================================
%%% Message Type Encoding Tests
%%%===================================================================

encode_connect_message_test() ->
    Msg = sample_connect_msg(),
    Result = macula_protocol_encoder:encode(connect, Msg),
    <<_Version:8, TypeId:8, _/binary>> = Result,
    %% Connect type ID is 0x01
    ?assertEqual(1, TypeId).

encode_disconnect_message_test() ->
    Msg = sample_disconnect_msg(),
    Result = macula_protocol_encoder:encode(disconnect, Msg),
    <<_Version:8, TypeId:8, _/binary>> = Result,
    %% Disconnect type ID is 0x02
    ?assertEqual(2, TypeId).

encode_ping_message_test() ->
    Msg = sample_ping_msg(),
    Result = macula_protocol_encoder:encode(ping, Msg),
    <<_Version:8, TypeId:8, _/binary>> = Result,
    %% Ping type ID is 0x03
    ?assertEqual(3, TypeId).

encode_pong_message_test() ->
    Msg = sample_pong_msg(),
    Result = macula_protocol_encoder:encode(pong, Msg),
    <<_Version:8, TypeId:8, _/binary>> = Result,
    %% Pong type ID is 0x04
    ?assertEqual(4, TypeId).

encode_publish_message_test() ->
    Msg = sample_publish_msg(),
    Result = macula_protocol_encoder:encode(publish, Msg),
    <<_Version:8, TypeId:8, _/binary>> = Result,
    %% Publish type ID is 0x10
    ?assertEqual(16#10, TypeId).

encode_subscribe_message_test() ->
    Msg = sample_subscribe_msg(),
    Result = macula_protocol_encoder:encode(subscribe, Msg),
    <<_Version:8, TypeId:8, _/binary>> = Result,
    %% Subscribe type ID is 0x11
    ?assertEqual(16#11, TypeId).

encode_unsubscribe_message_test() ->
    Msg = sample_unsubscribe_msg(),
    Result = macula_protocol_encoder:encode(unsubscribe, Msg),
    <<_Version:8, TypeId:8, _/binary>> = Result,
    %% Unsubscribe type ID is 0x12
    ?assertEqual(16#12, TypeId).

encode_pubsub_route_message_test() ->
    Msg = sample_pubsub_route_msg(),
    Result = macula_protocol_encoder:encode(pubsub_route, Msg),
    <<_Version:8, TypeId:8, _/binary>> = Result,
    %% PubSub Route type ID is 0x13
    ?assertEqual(16#13, TypeId).

%%%===================================================================
%%% Validation Tests - Connect Message
%%%===================================================================

connect_requires_version_test() ->
    Msg = maps:remove(version, sample_connect_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(connect, Msg)).

connect_requires_node_id_test() ->
    Msg = maps:remove(node_id, sample_connect_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(connect, Msg)).

connect_requires_realm_id_test() ->
    Msg = maps:remove(realm_id, sample_connect_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(connect, Msg)).

connect_requires_capabilities_test() ->
    Msg = maps:remove(capabilities, sample_connect_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(connect, Msg)).

%%%===================================================================
%%% Validation Tests - Disconnect Message
%%%===================================================================

disconnect_requires_reason_test() ->
    Msg = maps:remove(reason, sample_disconnect_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(disconnect, Msg)).

disconnect_requires_message_test() ->
    Msg = maps:remove(message, sample_disconnect_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(disconnect, Msg)).

%%%===================================================================
%%% Validation Tests - Ping/Pong Messages
%%%===================================================================

ping_requires_timestamp_test() ->
    Msg = maps:remove(timestamp, sample_ping_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(ping, Msg)).

pong_requires_timestamp_test() ->
    Msg = maps:remove(timestamp, sample_pong_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(pong, Msg)).

pong_requires_server_time_test() ->
    Msg = maps:remove(server_time, sample_pong_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(pong, Msg)).

%%%===================================================================
%%% Validation Tests - Publish Message
%%%===================================================================

publish_requires_topic_test() ->
    Msg = maps:remove(topic, sample_publish_msg()),
    ?assertError({case_clause, _}, macula_protocol_encoder:encode(publish, Msg)).

publish_requires_payload_test() ->
    Msg = maps:remove(payload, sample_publish_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(publish, Msg)).

publish_requires_qos_test() ->
    Msg = maps:remove(qos, sample_publish_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(publish, Msg)).

publish_requires_retain_test() ->
    Msg = maps:remove(retain, sample_publish_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(publish, Msg)).

publish_requires_message_id_test() ->
    Msg = maps:remove(message_id, sample_publish_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(publish, Msg)).

%% Test that publish accepts binary keys (from MessagePack decoding)
publish_accepts_binary_keys_test() ->
    Msg = #{
        <<"topic">> => <<"test.topic">>,
        <<"payload">> => <<"data">>,
        <<"qos">> => 1,
        <<"retain">> => false,
        <<"message_id">> => crypto:strong_rand_bytes(16)
    },
    Result = macula_protocol_encoder:encode(publish, Msg),
    ?assert(is_binary(Result)).

%%%===================================================================
%%% Validation Tests - Subscribe/Unsubscribe Messages
%%%===================================================================

subscribe_requires_topics_test() ->
    Msg = maps:remove(topics, sample_subscribe_msg()),
    ?assertError({case_clause, _}, macula_protocol_encoder:encode(subscribe, Msg)).

subscribe_requires_qos_test() ->
    Msg = maps:remove(qos, sample_subscribe_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(subscribe, Msg)).

subscribe_accepts_binary_keys_test() ->
    Msg = #{
        <<"topics">> => [<<"test.topic">>],
        <<"qos">> => 1
    },
    Result = macula_protocol_encoder:encode(subscribe, Msg),
    ?assert(is_binary(Result)).

unsubscribe_requires_topics_test() ->
    Msg = maps:remove(topics, sample_unsubscribe_msg()),
    ?assertError({case_clause, _}, macula_protocol_encoder:encode(unsubscribe, Msg)).

unsubscribe_accepts_binary_keys_test() ->
    Msg = #{<<"topics">> => [<<"test.topic">>]},
    Result = macula_protocol_encoder:encode(unsubscribe, Msg),
    ?assert(is_binary(Result)).

%%%===================================================================
%%% Validation Tests - PubSub Route Message
%%%===================================================================

pubsub_route_requires_destination_node_id_test() ->
    Msg = maps:remove(destination_node_id, sample_pubsub_route_msg()),
    ?assertError({case_clause, _}, macula_protocol_encoder:encode(pubsub_route, Msg)).

pubsub_route_requires_source_node_id_test() ->
    Msg = maps:remove(source_node_id, sample_pubsub_route_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(pubsub_route, Msg)).

pubsub_route_requires_hop_count_test() ->
    Msg = maps:remove(hop_count, sample_pubsub_route_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(pubsub_route, Msg)).

pubsub_route_requires_max_hops_test() ->
    Msg = maps:remove(max_hops, sample_pubsub_route_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(pubsub_route, Msg)).

pubsub_route_requires_topic_test() ->
    Msg = maps:remove(topic, sample_pubsub_route_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(pubsub_route, Msg)).

pubsub_route_requires_payload_test() ->
    Msg = maps:remove(payload, sample_pubsub_route_msg()),
    ?assertError({badmatch, _}, macula_protocol_encoder:encode(pubsub_route, Msg)).

pubsub_route_accepts_binary_keys_test() ->
    Msg = #{
        <<"destination_node_id">> => crypto:strong_rand_bytes(32),
        <<"source_node_id">> => crypto:strong_rand_bytes(32),
        <<"hop_count">> => 0,
        <<"max_hops">> => 10,
        <<"topic">> => <<"test.topic">>,
        <<"payload">> => sample_publish_msg()
    },
    Result = macula_protocol_encoder:encode(pubsub_route, Msg),
    ?assert(is_binary(Result)).

%%%===================================================================
%%% MessagePack Payload Tests
%%%===================================================================

payload_is_messagepack_encoded_test() ->
    Msg = sample_ping_msg(),
    <<_Version:8, _TypeId:8, _Flags:8, _Reserved:8, _PayloadLen:32/big-unsigned, Payload/binary>> =
        macula_protocol_encoder:encode(ping, Msg),
    %% Try to decode the payload with msgpack
    {ok, Decoded} = msgpack:unpack(Payload, [{map_format, map}]),
    ?assert(is_map(Decoded)).

payload_contains_original_data_test() ->
    Timestamp = 1234567890,
    Msg = #{timestamp => Timestamp},
    <<_Version:8, _TypeId:8, _Flags:8, _Reserved:8, _PayloadLen:32/big-unsigned, Payload/binary>> =
        macula_protocol_encoder:encode(ping, Msg),
    {ok, Decoded} = msgpack:unpack(Payload, [{map_format, map}]),
    ?assertEqual(Timestamp, maps:get(<<"timestamp">>, Decoded)).

%%%===================================================================
%%% Edge Cases and Property Tests
%%%===================================================================

encode_empty_publish_payload_test() ->
    Msg = sample_publish_msg(),
    Msg2 = Msg#{payload => <<>>},
    Result = macula_protocol_encoder:encode(publish, Msg2),
    ?assert(is_binary(Result)).

encode_large_publish_payload_test() ->
    LargePayload = crypto:strong_rand_bytes(65536),  %% 64KB
    Msg = sample_publish_msg(),
    Msg2 = Msg#{payload => LargePayload},
    Result = macula_protocol_encoder:encode(publish, Msg2),
    <<_Version:8, _TypeId:8, _Flags:8, _Reserved:8, PayloadLen:32/big-unsigned, _Payload/binary>> = Result,
    ?assert(PayloadLen > 65536).  %% MessagePack adds overhead

encode_multiple_topics_subscribe_test() ->
    Topics = [<<"topic", (integer_to_binary(N))/binary>> || N <- lists:seq(1, 100)],
    Msg = #{topics => Topics, qos => 1},
    Result = macula_protocol_encoder:encode(subscribe, Msg),
    ?assert(is_binary(Result)).

%%%===================================================================
%%% Different Message Types with Atom Keys
%%%===================================================================

encode_allows_unknown_message_types_test() ->
    %% For message types without explicit validation (e.g., future types),
    %% the encoder should allow any map
    Msg = #{some_field => <<"value">>},
    Result = macula_protocol_encoder:encode(call, Msg),
    ?assert(is_binary(Result)).

%%%===================================================================
%%% Round-trip Consistency Tests
%%%===================================================================

roundtrip_preserves_map_structure_test() ->
    OrigMsg = #{
        timestamp => 1234567890,
        nested => #{key => <<"value">>},
        list => [1, 2, 3]
    },
    <<_Version:8, _TypeId:8, _Flags:8, _Reserved:8, _PayloadLen:32/big-unsigned, Payload/binary>> =
        macula_protocol_encoder:encode(ping, OrigMsg),
    {ok, Decoded} = msgpack:unpack(Payload, [{map_format, map}]),
    %% Check that timestamp is preserved
    ?assertEqual(1234567890, maps:get(<<"timestamp">>, Decoded)),
    %% Check nested structure
    ?assertEqual(#{<<"key">> => <<"value">>}, maps:get(<<"nested">>, Decoded)),
    %% Check list
    ?assertEqual([1, 2, 3], maps:get(<<"list">>, Decoded)).
