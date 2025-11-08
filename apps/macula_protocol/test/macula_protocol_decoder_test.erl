%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_protocol_decoder module.
%%% Tests written FIRST (TDD red phase).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_protocol_decoder_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Basic Decoder Tests
%%%===================================================================

%% Test: decode returns {ok, {Type, Msg}}
decode_connect_returns_tuple_test() ->
    %% First encode a message
    Msg = #{
        version => <<"1.0">>,
        node_id => <<1:256>>,
        realm_id => <<2:256>>,
        capabilities => [pubsub]
    },
    Binary = macula_protocol_encoder:encode(connect, Msg),

    %% Then decode it
    Result = macula_protocol_decoder:decode(Binary),
    ?assertMatch({ok, {connect, _}}, Result).

%% Test: decoded message matches original
decode_connect_matches_original_test() ->
    Original = #{
        version => <<"1.0">>,
        node_id => <<1:256>>,
        realm_id => <<2:256>>,
        capabilities => [pubsub, rpc]
    },
    Binary = macula_protocol_encoder:encode(connect, Original),
    {ok, {connect, Decoded}} = macula_protocol_decoder:decode(Binary),

    %% Note: MessagePack uses binary keys
    ?assertEqual(maps:get(version, Original), maps:get(<<"version">>, Decoded)),
    ?assertEqual(maps:get(node_id, Original), maps:get(<<"node_id">>, Decoded)),
    ?assertEqual(maps:get(realm_id, Original), maps:get(<<"realm_id">>, Decoded)),
    %% Note: capabilities may be decoded as list of binaries
    ?assert(is_list(maps:get(<<"capabilities">>, Decoded))).

%%%===================================================================
%%% Round-Trip Tests
%%%===================================================================

%% Test: encode-decode round-trip for ping
ping_roundtrip_test() ->
    Original = #{timestamp => 1234567890},
    Binary = macula_protocol_encoder:encode(ping, Original),
    {ok, {ping, Decoded}} = macula_protocol_decoder:decode(Binary),
    ?assertEqual(maps:get(timestamp, Original), maps:get(<<"timestamp">>, Decoded)).

%% Test: encode-decode round-trip for publish
publish_roundtrip_test() ->
    Original = #{
        topic => <<"test/topic">>,
        payload => <<"hello world">>,
        qos => 1,
        retain => false,
        message_id => <<1:128>>
    },
    Binary = macula_protocol_encoder:encode(publish, Original),
    {ok, {publish, Decoded}} = macula_protocol_decoder:decode(Binary),

    ?assertEqual(maps:get(topic, Original), maps:get(<<"topic">>, Decoded)),
    ?assertEqual(maps:get(payload, Original), maps:get(<<"payload">>, Decoded)),
    ?assertEqual(maps:get(qos, Original), maps:get(<<"qos">>, Decoded)),
    ?assertEqual(maps:get(retain, Original), maps:get(<<"retain">>, Decoded)),
    ?assertEqual(maps:get(message_id, Original), maps:get(<<"message_id">>, Decoded)).

%%%===================================================================
%%% Error Cases
%%%===================================================================

%% Test: decode with invalid version
decode_invalid_version_test() ->
    %% Manually create binary with bad version
    Binary = <<99:8, 16#01:8, 0:8, 0:8, 0:32>>,
    Result = macula_protocol_decoder:decode(Binary),
    ?assertMatch({error, {unsupported_version, 99}}, Result).

%% Test: decode with unknown message type
decode_unknown_type_test() ->
    %% Create binary with unknown type
    Binary = <<1:8, 255:8, 0:8, 0:8, 2:32, 1, 2>>,
    Result = macula_protocol_decoder:decode(Binary),
    ?assertMatch({error, {unknown_type, 255}}, Result).

%% Test: decode with too short binary
decode_too_short_test() ->
    Binary = <<1, 2, 3>>,  % Less than 8 bytes
    Result = macula_protocol_decoder:decode(Binary),
    ?assertMatch({error, invalid_header}, Result).

%% Test: decode with payload length mismatch
decode_length_mismatch_test() ->
    %% Header says 10 bytes payload, but only provide 5
    Binary = <<1:8, 16#01:8, 0:8, 0:8, 10:32, 1, 2, 3, 4, 5>>,
    Result = macula_protocol_decoder:decode(Binary),
    ?assertMatch({error, incomplete_payload}, Result).

%% Test: decode with corrupted msgpack payload
decode_corrupted_payload_test() ->
    %% Valid header but invalid msgpack
    Binary = <<1:8, 16#01:8, 0:8, 0:8, 3:32, 255, 255, 255>>,
    Result = macula_protocol_decoder:decode(Binary),
    ?assertMatch({error, {msgpack_decode_error, _}}, Result).
