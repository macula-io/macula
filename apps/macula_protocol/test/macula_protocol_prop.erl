%%%-------------------------------------------------------------------
%%% @doc
%%% PropEr property-based tests for macula_protocol.
%%% Tests encode/decode round-trips and protocol invariants.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_protocol_prop).
-include_lib("proper/include/proper.hrl").

%%%===================================================================
%%% Properties
%%%===================================================================

%% Property: Connect message round-trip
prop_connect_roundtrip() ->
    ?FORALL(
        ConnectMsg,
        connect_msg_gen(),
        begin
            Binary = macula_protocol_encoder:encode(connect, ConnectMsg),
            {ok, {connect, Decoded}} = macula_protocol_decoder:decode(Binary),

            %% MessagePack converts atom keys to binary keys
            maps:get(version, ConnectMsg) =:= maps:get(<<"version">>, Decoded) andalso
            maps:get(node_id, ConnectMsg) =:= maps:get(<<"node_id">>, Decoded) andalso
            maps:get(realm_id, ConnectMsg) =:= maps:get(<<"realm_id">>, Decoded)
        end
    ).

%% Property: Ping message round-trip
prop_ping_roundtrip() ->
    ?FORALL(
        PingMsg,
        ping_msg_gen(),
        begin
            Binary = macula_protocol_encoder:encode(ping, PingMsg),
            {ok, {ping, Decoded}} = macula_protocol_decoder:decode(Binary),
            maps:get(timestamp, PingMsg) =:= maps:get(<<"timestamp">>, Decoded)
        end
    ).

%% Property: Pong message round-trip
prop_pong_roundtrip() ->
    ?FORALL(
        PongMsg,
        pong_msg_gen(),
        begin
            Binary = macula_protocol_encoder:encode(pong, PongMsg),
            {ok, {pong, Decoded}} = macula_protocol_decoder:decode(Binary),
            maps:get(timestamp, PongMsg) =:= maps:get(<<"timestamp">>, Decoded) andalso
            maps:get(server_time, PongMsg) =:= maps:get(<<"server_time">>, Decoded)
        end
    ).

%% Property: Publish message round-trip
prop_publish_roundtrip() ->
    ?FORALL(
        PublishMsg,
        publish_msg_gen(),
        begin
            Binary = macula_protocol_encoder:encode(publish, PublishMsg),
            {ok, {publish, Decoded}} = macula_protocol_decoder:decode(Binary),
            maps:get(topic, PublishMsg) =:= maps:get(<<"topic">>, Decoded) andalso
            maps:get(payload, PublishMsg) =:= maps:get(<<"payload">>, Decoded) andalso
            maps:get(qos, PublishMsg) =:= maps:get(<<"qos">>, Decoded) andalso
            maps:get(retain, PublishMsg) =:= maps:get(<<"retain">>, Decoded) andalso
            maps:get(message_id, PublishMsg) =:= maps:get(<<"message_id">>, Decoded)
        end
    ).

%% Property: Subscribe message round-trip
prop_subscribe_roundtrip() ->
    ?FORALL(
        SubscribeMsg,
        subscribe_msg_gen(),
        begin
            Binary = macula_protocol_encoder:encode(subscribe, SubscribeMsg),
            {ok, {subscribe, Decoded}} = macula_protocol_decoder:decode(Binary),
            maps:get(qos, SubscribeMsg) =:= maps:get(<<"qos">>, Decoded)
            %% Note: lists may need special handling due to MessagePack encoding
        end
    ).

%% Property: Encoded messages always have 8-byte header
prop_encoded_has_header() ->
    ?FORALL(
        {Type, Msg},
        oneof([
            {connect, connect_msg_gen()},
            {ping, ping_msg_gen()},
            {pong, pong_msg_gen()},
            {publish, publish_msg_gen()}
        ]),
        begin
            Binary = macula_protocol_encoder:encode(Type, Msg),
            byte_size(Binary) >= 8
        end
    ).

%% Property: Encoded messages have version byte = 1
prop_encoded_version_byte() ->
    ?FORALL(
        {Type, Msg},
        oneof([
            {connect, connect_msg_gen()},
            {ping, ping_msg_gen()}
        ]),
        begin
            Binary = macula_protocol_encoder:encode(Type, Msg),
            <<Version:8, _Rest/binary>> = Binary,
            Version =:= 1
        end
    ).

%% Property: Payload length in header matches actual payload
prop_payload_length_correct() ->
    ?FORALL(
        {Type, Msg},
        oneof([
            {connect, connect_msg_gen()},
            {ping, ping_msg_gen()},
            {publish, publish_msg_gen()}
        ]),
        begin
            Binary = macula_protocol_encoder:encode(Type, Msg),
            <<_:32, PayloadLen:32/big-unsigned, Payload/binary>> = Binary,
            PayloadLen =:= byte_size(Payload)
        end
    ).

%%%===================================================================
%%% Generators
%%%===================================================================

%% Generator: Connect message
connect_msg_gen() ->
    ?LET({Version, NodeId, RealmId, Capabilities},
         {version_gen(), node_id_gen(), realm_id_gen(), capabilities_gen()},
         #{
             version => Version,
             node_id => NodeId,
             realm_id => RealmId,
             capabilities => Capabilities
         }).

%% Generator: Ping message
ping_msg_gen() ->
    ?LET(Timestamp, timestamp_gen(),
         #{timestamp => Timestamp}).

%% Generator: Pong message
pong_msg_gen() ->
    ?LET({Timestamp, ServerTime}, {timestamp_gen(), timestamp_gen()},
         #{timestamp => Timestamp, server_time => ServerTime}).

%% Generator: Publish message
publish_msg_gen() ->
    ?LET({Topic, Payload, QoS, Retain, MsgId},
         {topic_gen(), payload_gen(), qos_gen(), boolean(), message_id_gen()},
         #{
             topic => Topic,
             payload => Payload,
             qos => QoS,
             retain => Retain,
             message_id => MsgId
         }).

%% Generator: Subscribe message
subscribe_msg_gen() ->
    ?LET({Topics, QoS},
         {list(topic_gen()), qos_gen()},
         #{topics => Topics, qos => QoS}).

%% Generator: Protocol version
version_gen() ->
    <<"1.0">>.

%% Generator: 32-byte node ID
node_id_gen() ->
    binary(32).

%% Generator: 32-byte realm ID
realm_id_gen() ->
    binary(32).

%% Generator: Capabilities list
capabilities_gen() ->
    list(oneof([pubsub, rpc, routing, discovery, gateway])).

%% Generator: Timestamp (positive integer)
timestamp_gen() ->
    pos_integer().

%% Generator: Topic name (non-empty binary)
topic_gen() ->
    non_empty(binary()).

%% Generator: Message payload
payload_gen() ->
    binary().

%% Generator: QoS level (0, 1, or 2)
qos_gen() ->
    oneof([0, 1, 2]).

%% Generator: 16-byte message ID
message_id_gen() ->
    binary(16).
