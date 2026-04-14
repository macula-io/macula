%% EUnit tests for macula_record.
-module(macula_record_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------
%% node_record construction
%%------------------------------------------------------------------

build_node_record_envelope_test() ->
    Kp = macula_identity:generate(),
    NodeId = macula_identity:public(Kp),
    Realm  = crypto:strong_rand_bytes(32),
    R = macula_record:node_record(NodeId, [Realm], 1),
    ?assertEqual(16#01, macula_record:type(R)),
    ?assertEqual(NodeId, macula_record:key(R)),
    ?assertEqual(16, byte_size(macula_record:version(R))),
    ?assert(macula_record:expires_at(R) > macula_record:created_at(R)).

node_record_default_station_id_is_node_id_test() ->
    Kp = macula_identity:generate(),
    NodeId = macula_identity:public(Kp),
    R = macula_record:node_record(NodeId, [], 0),
    P = macula_record:payload(R),
    ?assertEqual(NodeId, maps:get({text, <<"station_id">>}, P)).

node_record_with_custom_station_id_test() ->
    Kp = macula_identity:generate(),
    NodeId    = macula_identity:public(Kp),
    StationId = crypto:strong_rand_bytes(32),
    R = macula_record:node_record(NodeId, [], 0, #{station_id => StationId}),
    P = macula_record:payload(R),
    ?assertEqual(StationId, maps:get({text, <<"station_id">>}, P)).

node_record_with_optional_text_fields_test() ->
    Kp = macula_identity:generate(),
    R = macula_record:node_record(
        macula_identity:public(Kp), [], 0,
        #{caps_hint => <<"hint">>, display_name => <<"Alice">>}),
    P = macula_record:payload(R),
    ?assertEqual({text, <<"hint">>}, maps:get({text, <<"caps_hint">>}, P)),
    ?assertEqual({text, <<"Alice">>}, maps:get({text, <<"display_name">>}, P)).

node_record_omits_unset_optional_fields_test() ->
    Kp = macula_identity:generate(),
    R = macula_record:node_record(macula_identity:public(Kp), [], 0),
    P = macula_record:payload(R),
    ?assertNot(maps:is_key({text, <<"caps_hint">>}, P)),
    ?assertNot(maps:is_key({text, <<"display_name">>}, P)).

%%------------------------------------------------------------------
%% Sign / verify
%%------------------------------------------------------------------

sign_attaches_signature_test() ->
    Kp = macula_identity:generate(),
    R  = macula_record:node_record(macula_identity:public(Kp), [], 0),
    Signed = macula_record:sign(R, Kp),
    ?assertEqual(64, byte_size(macula_record:signature(Signed))).

verify_signed_record_test() ->
    Kp = macula_identity:generate(),
    R  = macula_record:node_record(macula_identity:public(Kp), [], 0),
    Signed = macula_record:sign(R, Kp),
    ?assertMatch({ok, _}, macula_record:verify(Signed)).

verify_rejects_tampered_payload_test() ->
    Kp = macula_identity:generate(),
    R  = macula_record:node_record(macula_identity:public(Kp), [], 0),
    Signed = macula_record:sign(R, Kp),
    P = macula_record:payload(Signed),
    Tampered = Signed#{payload => P#{ {text, <<"capabilities">>} => 999 }},
    ?assertEqual({error, signature_invalid}, macula_record:verify(Tampered)).

verify_rejects_wrong_signer_test() ->
    Kp1 = macula_identity:generate(),
    Kp2 = macula_identity:generate(),
    %% Build record with Kp1's pubkey as `key` but sign with Kp2.
    R  = macula_record:node_record(macula_identity:public(Kp1), [], 0),
    Signed = macula_record:sign(R, Kp2),
    ?assertEqual({error, signature_invalid}, macula_record:verify(Signed)).

verify_rejects_expired_test() ->
    Kp = macula_identity:generate(),
    R  = macula_record:node_record(macula_identity:public(Kp), [], 0),
    Past = R#{expires_at => erlang:system_time(millisecond) - 1},
    Signed = macula_record:sign(Past, Kp),
    ?assertEqual({error, expired}, macula_record:verify(Signed)).

verify_rejects_record_without_signature_test() ->
    Kp = macula_identity:generate(),
    R  = macula_record:node_record(macula_identity:public(Kp), [], 0),
    ?assertEqual({error, bad_record}, macula_record:verify(R)).

%%------------------------------------------------------------------
%% Wire encode / decode
%%------------------------------------------------------------------

encode_decode_roundtrip_test() ->
    Kp = macula_identity:generate(),
    R = macula_record:node_record(
        macula_identity:public(Kp),
        [crypto:strong_rand_bytes(32), crypto:strong_rand_bytes(32)],
        16#DEADBEEF,
        #{caps_hint => <<"some hint">>, display_name => <<"a node">>}
    ),
    Signed = macula_record:sign(R, Kp),
    Wire = macula_record:encode(Signed),
    ?assertMatch({ok, _}, macula_record:decode(Wire)),
    {ok, Decoded} = macula_record:decode(Wire),
    %% Verify the decoded record (signature still valid over wire bytes).
    ?assertMatch({ok, _}, macula_record:verify(Decoded)).

decode_rejects_garbage_test() ->
    %% A non-CBOR sequence either fails to decode or yields a non-record value.
    Result = catch macula_record:decode(<<255, 255, 255, 255>>),
    case Result of
        {ok, _} -> ?assert(false);
        _       -> ok
    end.

decode_returns_missing_signature_when_unsigned_test() ->
    Map = #{
        {text, <<"t">>} => 1,
        {text, <<"k">>} => crypto:strong_rand_bytes(32),
        {text, <<"v">>} => crypto:strong_rand_bytes(16),
        {text, <<"c">>} => erlang:system_time(millisecond),
        {text, <<"x">>} => erlang:system_time(millisecond) + 60_000,
        {text, <<"p">>} => #{}
    },
    Wire = macula_record_cbor:encode(Map),
    ?assertEqual({error, missing_signature}, macula_record:decode(Wire)).

decode_rejects_short_signature_test() ->
    Map = #{
        {text, <<"t">>} => 1,
        {text, <<"k">>} => crypto:strong_rand_bytes(32),
        {text, <<"v">>} => crypto:strong_rand_bytes(16),
        {text, <<"c">>} => erlang:system_time(millisecond),
        {text, <<"x">>} => erlang:system_time(millisecond) + 60_000,
        {text, <<"p">>} => #{},
        {text, <<"s">>} => crypto:strong_rand_bytes(32)   %% wrong size
    },
    Wire = macula_record_cbor:encode(Map),
    ?assertEqual({error, bad_record}, macula_record:decode(Wire)).

%%------------------------------------------------------------------
%% Tombstone
%%------------------------------------------------------------------

build_tombstone_test() ->
    Kp = macula_identity:generate(),
    Pub = macula_identity:public(Kp),
    Tomb = macula_record:tombstone(Pub, 16#01, retired),
    ?assertEqual(16#0C, macula_record:type(Tomb)),
    ?assertEqual(Pub, macula_record:key(Tomb)).

sign_verify_tombstone_test() ->
    Kp = macula_identity:generate(),
    Pub = macula_identity:public(Kp),
    Tomb = macula_record:tombstone(Pub, 16#01, retired),
    Signed = macula_record:sign(Tomb, Kp),
    ?assertMatch({ok, _}, macula_record:verify(Signed)).

tombstone_default_detail_is_null_test() ->
    Kp = macula_identity:generate(),
    Pub = macula_identity:public(Kp),
    Tomb = macula_record:tombstone(Pub, 16#01, expired),
    P = macula_record:payload(Tomb),
    ?assertEqual(null, maps:get({text, <<"detail">>}, P)).

tombstone_with_detail_test() ->
    Kp = macula_identity:generate(),
    Pub = macula_identity:public(Kp),
    Tomb = macula_record:tombstone(Pub, 16#01, revoked,
                                   #{detail => <<"key compromise">>}),
    P = macula_record:payload(Tomb),
    ?assertEqual({text, <<"key compromise">>},
                 maps:get({text, <<"detail">>}, P)).

tombstone_reason_serialised_as_text_test() ->
    Kp = macula_identity:generate(),
    Pub = macula_identity:public(Kp),
    Tomb = macula_record:tombstone(Pub, 16#01, moved),
    P = macula_record:payload(Tomb),
    ?assertEqual({text, <<"moved">>},
                 maps:get({text, <<"reason">>}, P)).

tombstone_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    Pub = macula_identity:public(Kp),
    Tomb = macula_record:tombstone(Pub, 16#01, revoked,
                                   #{detail => <<"reason here">>}),
    Signed = macula_record:sign(Tomb, Kp),
    Wire = macula_record:encode(Signed),
    {ok, Decoded} = macula_record:decode(Wire),
    {ok, _} = macula_record:verify(Decoded),
    ?assertEqual(16#0C, macula_record:type(Decoded)),
    ?assertEqual(Pub, macula_record:key(Decoded)).
