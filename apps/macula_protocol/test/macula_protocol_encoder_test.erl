%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_protocol_encoder module.
%%% Tests written FIRST (TDD red phase).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_protocol_encoder_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Encode Connect Message Tests
%%%===================================================================

%% Test: encode connect message returns binary
encode_connect_returns_binary_test() ->
    Msg = #{
        version => <<"1.0">>,
        node_id => <<1:256>>,
        realm_id => <<2:256>>,
        capabilities => [pubsub]
    },
    Binary = macula_protocol_encoder:encode(connect, Msg),
    ?assert(is_binary(Binary)).

%% Test: encoded message starts with version byte
encode_connect_has_version_byte_test() ->
    Msg = #{
        version => <<"1.0">>,
        node_id => <<1:256>>,
        realm_id => <<2:256>>,
        capabilities => []
    },
    Binary = macula_protocol_encoder:encode(connect, Msg),
    <<Version:8, _Rest/binary>> = Binary,
    ?assertEqual(1, Version).

%% Test: encoded message has correct type byte
encode_connect_has_type_byte_test() ->
    Msg = #{
        version => <<"1.0">>,
        node_id => <<1:256>>,
        realm_id => <<2:256>>,
        capabilities => []
    },
    Binary = macula_protocol_encoder:encode(connect, Msg),
    <<_Version:8, Type:8, _Rest/binary>> = Binary,
    ?assertEqual(16#01, Type).  % connect = 0x01

%% Test: encoded message has 8-byte header
encode_connect_has_header_test() ->
    Msg = #{
        version => <<"1.0">>,
        node_id => <<1:256>>,
        realm_id => <<2:256>>,
        capabilities => []
    },
    Binary = macula_protocol_encoder:encode(connect, Msg),
    ?assert(byte_size(Binary) >= 8).

%% Test: header contains payload length
encode_connect_payload_length_test() ->
    Msg = #{
        version => <<"1.0">>,
        node_id => <<1:256>>,
        realm_id => <<2:256>>,
        capabilities => []
    },
    Binary = macula_protocol_encoder:encode(connect, Msg),
    <<_:32, PayloadLen:32, Payload/binary>> = Binary,
    ?assertEqual(PayloadLen, byte_size(Payload)).

%%%===================================================================
%%% Encode Ping Message Tests
%%%===================================================================

%% Test: encode ping message
encode_ping_test() ->
    Msg = #{timestamp => 1234567890},
    Binary = macula_protocol_encoder:encode(ping, Msg),
    ?assert(is_binary(Binary)),
    <<_Version:8, Type:8, _Rest/binary>> = Binary,
    ?assertEqual(16#03, Type).  % ping = 0x03

%%%===================================================================
%%% Encode Publish Message Tests
%%%===================================================================

%% Test: encode publish message
encode_publish_test() ->
    Msg = #{
        topic => <<"test/topic">>,
        payload => <<"hello world">>,
        qos => 1,
        retain => false,
        message_id => <<1:128>>
    },
    Binary = macula_protocol_encoder:encode(publish, Msg),
    ?assert(is_binary(Binary)),
    <<_Version:8, Type:8, _Rest/binary>> = Binary,
    ?assertEqual(16#10, Type).  % publish = 0x10

%%%===================================================================
%%% Error Cases
%%%===================================================================

%% Test: encode with invalid message type
encode_invalid_type_test() ->
    Msg = #{test => value},
    ?assertError(function_clause,
                 macula_protocol_encoder:encode(invalid_type, Msg)).

%% Test: encode with missing required fields
encode_missing_fields_test() ->
    %% Connect message requires version, node_id, realm_id, capabilities
    Msg = #{version => <<"1.0">>},  % Missing other fields
    ?assertError(_,
                 macula_protocol_encoder:encode(connect, Msg)).
