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
    ?assertNot(maps:is_key({text, <<"display_name">>}, P)),
    ?assertNot(maps:is_key({text, <<"hostname">>}, P)),
    ?assertNot(maps:is_key({text, <<"endpoint">>}, P)),
    ?assertNot(maps:is_key({text, <<"city">>}, P)),
    ?assertNot(maps:is_key({text, <<"country">>}, P)),
    ?assertNot(maps:is_key({text, <<"lat">>}, P)),
    ?assertNot(maps:is_key({text, <<"lng">>}, P)),
    ?assertNot(maps:is_key({text, <<"kind">>}, P)).

node_record_with_kind_field_test() ->
    Kp = macula_identity:generate(),
    R = macula_record:node_record(
          macula_identity:public(Kp), [], 0,
          #{kind => <<"daemon">>}),
    P = macula_record:payload(R),
    ?assertEqual({text, <<"daemon">>},
                 maps:get({text, <<"kind">>}, P)).

node_record_with_geo_metadata_test() ->
    %% Subscribers (e.g. realm dashboards) read geo straight from
    %% the payload — no side-channel fetches. v3.4.0 added these
    %% optional fields so stations advertise their location with
    %% their identity record.
    Kp = macula_identity:generate(),
    R = macula_record:node_record(
          macula_identity:public(Kp), [], 0,
          #{hostname => <<"relay-be-leuven.macula.io">>,
            endpoint => <<"quic://relay-be-leuven.macula.io:4433">>,
            city     => <<"Leuven">>,
            country  => <<"BE">>,
            lat      => 50.8798,
            lng      => 4.7005}),
    P = macula_record:payload(R),
    ?assertEqual({text, <<"relay-be-leuven.macula.io">>},
                 maps:get({text, <<"hostname">>}, P)),
    ?assertEqual({text, <<"quic://relay-be-leuven.macula.io:4433">>},
                 maps:get({text, <<"endpoint">>}, P)),
    ?assertEqual({text, <<"Leuven">>},
                 maps:get({text, <<"city">>}, P)),
    ?assertEqual({text, <<"BE">>},
                 maps:get({text, <<"country">>}, P)),
    ?assertEqual({text, <<"50.8798">>},
                 maps:get({text, <<"lat">>}, P)),
    ?assertEqual({text, <<"4.7005">>},
                 maps:get({text, <<"lng">>}, P)),
    %% Round-trip through encode/decode preserves the geo fields.
    Signed   = macula_record:sign(R, Kp),
    Bytes    = macula_record:encode(Signed),
    {ok, R2} = macula_record:decode(Bytes),
    ?assertEqual({text, <<"50.8798">>},
                 maps:get({text, <<"lat">>},
                          macula_record:payload(R2))).

node_record_with_integer_geo_test() ->
    %% Integer 0 should encode as text "0", not be silently dropped.
    Kp = macula_identity:generate(),
    R = macula_record:node_record(
          macula_identity:public(Kp), [], 0,
          #{lat => 0, lng => 0}),
    P = macula_record:payload(R),
    ?assertEqual({text, <<"0">>}, maps:get({text, <<"lat">>}, P)),
    ?assertEqual({text, <<"0">>}, maps:get({text, <<"lng">>}, P)).

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

%%------------------------------------------------------------------
%% realm_directory
%%------------------------------------------------------------------

realm_directory_shape_test() ->
    Kp = macula_identity:generate(),
    RealmId = macula_identity:public(Kp),
    AdminKey = crypto:strong_rand_bytes(32),
    R = macula_record:realm_directory(RealmId, <<"my realm">>, AdminKey),
    ?assertEqual(16#03, macula_record:type(R)),
    ?assertEqual(RealmId, macula_record:key(R)),
    P = macula_record:payload(R),
    ?assertEqual(RealmId, maps:get({text, <<"realm_id">>}, P)),
    ?assertEqual({text, <<"my realm">>}, maps:get({text, <<"name">>}, P)),
    ?assertEqual(AdminKey, maps:get({text, <<"admin_key">>}, P)),
    ?assert(maps:is_key({text, <<"created_at">>}, P)),
    ?assertNot(maps:is_key({text, <<"policy_url">>}, P)).

realm_directory_with_policy_url_test() ->
    Kp = macula_identity:generate(),
    RealmId = macula_identity:public(Kp),
    AdminKey = crypto:strong_rand_bytes(32),
    R = macula_record:realm_directory(RealmId, <<"r">>, AdminKey,
                                      #{policy_url => <<"https://ex.io">>}),
    ?assertEqual({text, <<"https://ex.io">>},
                 maps:get({text, <<"policy_url">>}, macula_record:payload(R))).

realm_directory_sign_verify_roundtrip_test() ->
    Kp = macula_identity:generate(),
    RealmId = macula_identity:public(Kp),
    R = macula_record:realm_directory(RealmId, <<"r">>,
                                      crypto:strong_rand_bytes(32)),
    Signed = macula_record:sign(R, Kp),
    {ok, Decoded} = macula_record:decode(macula_record:encode(Signed)),
    {ok, _} = macula_record:verify(Decoded).

realm_directory_rejects_non_32_byte_realm_id_test() ->
    ?assertError(function_clause,
        macula_record:realm_directory(<<0:64>>, <<"r">>,
                                      crypto:strong_rand_bytes(32))).

%%------------------------------------------------------------------
%% realm_stations
%%------------------------------------------------------------------

realm_stations_shape_test() ->
    Kp = macula_identity:generate(),
    RealmId = macula_identity:public(Kp),
    S1 = crypto:strong_rand_bytes(32),
    S2 = crypto:strong_rand_bytes(32),
    Entries = [
        #{station_id => S1, roles => [<<"directory">>]},
        #{station_id => S2, roles => [<<"replica">>, <<"relay">>]}
    ],
    R = macula_record:realm_stations(RealmId, Entries),
    ?assertEqual(16#04, macula_record:type(R)),
    ?assertEqual(RealmId, macula_record:key(R)),
    P = macula_record:payload(R),
    [E1, E2] = maps:get({text, <<"stations">>}, P),
    ?assertEqual(S1, maps:get({text, <<"station_id">>}, E1)),
    ?assertEqual([{text, <<"replica">>}, {text, <<"relay">>}],
                 maps:get({text, <<"roles">>}, E2)).

realm_stations_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    RealmId = macula_identity:public(Kp),
    R = macula_record:realm_stations(
          RealmId,
          [#{station_id => crypto:strong_rand_bytes(32),
             roles      => [<<"directory">>]}]),
    Signed = macula_record:sign(R, Kp),
    {ok, Decoded} = macula_record:decode(macula_record:encode(Signed)),
    ?assertEqual(16#04, macula_record:type(Decoded)),
    {ok, _} = macula_record:verify(Decoded).

realm_stations_accepts_empty_list_test() ->
    Kp = macula_identity:generate(),
    R = macula_record:realm_stations(macula_identity:public(Kp), []),
    ?assertEqual([], maps:get({text, <<"stations">>},
                              macula_record:payload(R))).

%%------------------------------------------------------------------
%% realm_member_endorsement
%%------------------------------------------------------------------

realm_member_endorsement_shape_test() ->
    AdminKp = macula_identity:generate(),
    RealmId = macula_identity:public(AdminKp),
    Member  = crypto:strong_rand_bytes(32),
    R = macula_record:realm_member_endorsement(
          RealmId,
          #{realm => RealmId, member_node => Member,
            roles => [<<"peer">>]}),
    ?assertEqual(16#05, macula_record:type(R)),
    ?assertEqual(RealmId, macula_record:key(R)),
    P = macula_record:payload(R),
    ?assertEqual(RealmId, maps:get({text, <<"realm">>}, P)),
    ?assertEqual(Member,  maps:get({text, <<"member_node">>}, P)),
    ?assertEqual([{text, <<"peer">>}],
                 maps:get({text, <<"roles">>}, P)),
    ?assert(is_integer(maps:get({text, <<"valid_from">>}, P))),
    ?assert(is_integer(maps:get({text, <<"valid_until">>}, P))),
    ValidFrom  = maps:get({text, <<"valid_from">>}, P),
    ValidUntil = maps:get({text, <<"valid_until">>}, P),
    ?assert(ValidUntil > ValidFrom).

realm_member_endorsement_custom_validity_window_test() ->
    AdminKp = macula_identity:generate(),
    RealmId = macula_identity:public(AdminKp),
    Member  = crypto:strong_rand_bytes(32),
    R = macula_record:realm_member_endorsement(
          RealmId,
          #{realm => RealmId, member_node => Member, roles => []},
          #{valid_from => 1000, valid_until => 5000}),
    P = macula_record:payload(R),
    ?assertEqual(1000, maps:get({text, <<"valid_from">>}, P)),
    ?assertEqual(5000, maps:get({text, <<"valid_until">>}, P)).

realm_member_endorsement_sign_verify_roundtrip_test() ->
    AdminKp = macula_identity:generate(),
    RealmId = macula_identity:public(AdminKp),
    Member  = crypto:strong_rand_bytes(32),
    R = macula_record:realm_member_endorsement(
          RealmId,
          #{realm => RealmId, member_node => Member,
            roles => [<<"peer">>, <<"directory">>]}),
    Signed = macula_record:sign(R, AdminKp),
    {ok, Decoded} = macula_record:decode(macula_record:encode(Signed)),
    ?assertEqual(16#05, macula_record:type(Decoded)),
    {ok, _} = macula_record:verify(Decoded).

realm_member_endorsement_storage_key_binds_realm_and_member_test() ->
    AdminKp = macula_identity:generate(),
    RealmId = macula_identity:public(AdminKp),
    M1 = crypto:strong_rand_bytes(32),
    M2 = crypto:strong_rand_bytes(32),
    R1 = macula_record:realm_member_endorsement(
           RealmId,
           #{realm => RealmId, member_node => M1, roles => []}),
    R2 = macula_record:realm_member_endorsement(
           RealmId,
           #{realm => RealmId, member_node => M2, roles => []}),
    K1 = macula_record:storage_key(R1),
    K2 = macula_record:storage_key(R2),
    ?assertEqual(32, byte_size(K1)),
    ?assertEqual(32, byte_size(K2)),
    %% Different members → different keys even for same realm.
    ?assertNotEqual(K1, K2),
    %% Key differs from realm envelope key and from realm_stations hash.
    ?assertNotEqual(RealmId, K1),
    RStations = macula_record:realm_stations(RealmId, []),
    ?assertNotEqual(macula_record:storage_key(RStations), K1).

realm_member_endorsement_rejects_non_32_byte_member_test() ->
    RealmId = crypto:strong_rand_bytes(32),
    ?assertError(function_clause,
                 macula_record:realm_member_endorsement(
                   RealmId,
                   #{realm => RealmId, member_node => <<1,2,3>>,
                     roles => []})).

%%------------------------------------------------------------------
%% procedure_advertisement
%%------------------------------------------------------------------

procedure_advertisement_shape_test() ->
    Kp = macula_identity:generate(),
    NodeId = macula_identity:public(Kp),
    Station = crypto:strong_rand_bytes(32),
    R = macula_record:procedure_advertisement(NodeId,
                                              <<"mcp://weather/forecast">>,
                                              Station),
    ?assertEqual(16#06, macula_record:type(R)),
    ?assertEqual(NodeId, macula_record:key(R)),
    P = macula_record:payload(R),
    ?assertEqual({text, <<"mcp://weather/forecast">>},
                 maps:get({text, <<"procedure_uri">>}, P)),
    ?assertEqual(NodeId, maps:get({text, <<"advertiser_node">>}, P)),
    ?assertEqual(Station, maps:get({text, <<"serving_station">>}, P)).

procedure_advertisement_with_capacity_hints_test() ->
    Kp = macula_identity:generate(),
    Station = crypto:strong_rand_bytes(32),
    R = macula_record:procedure_advertisement(
          macula_identity:public(Kp),
          <<"mcp://x/y">>,
          Station,
          #{rate_limit_qps => 100, max_concurrency => 8}),
    P = macula_record:payload(R),
    ?assertEqual(100, maps:get({text, <<"rate_limit_qps">>}, P)),
    ?assertEqual(8,   maps:get({text, <<"max_concurrency">>}, P)).

procedure_advertisement_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    R = macula_record:procedure_advertisement(
          macula_identity:public(Kp),
          <<"mcp://weather/forecast">>,
          crypto:strong_rand_bytes(32)),
    Signed = macula_record:sign(R, Kp),
    {ok, Decoded} = macula_record:decode(macula_record:encode(Signed)),
    {ok, _} = macula_record:verify(Decoded).

%%------------------------------------------------------------------
%% storage_key/1 — DHT storage key derivation (Part 3 §3.3)
%%------------------------------------------------------------------

storage_key_node_record_is_envelope_key_test() ->
    Kp = macula_identity:generate(),
    NodeId = macula_identity:public(Kp),
    R = macula_record:node_record(NodeId, [], 0),
    ?assertEqual(NodeId, macula_record:storage_key(R)).

storage_key_realm_directory_is_realm_id_test() ->
    Kp = macula_identity:generate(),
    RealmId = macula_identity:public(Kp),
    R = macula_record:realm_directory(RealmId, <<"r">>,
                                      crypto:strong_rand_bytes(32)),
    ?assertEqual(RealmId, macula_record:storage_key(R)).

storage_key_realm_stations_is_hashed_test() ->
    Kp = macula_identity:generate(),
    RealmId = macula_identity:public(Kp),
    Expected = crypto:hash(sha256, <<"station_set", RealmId/binary>>),
    R = macula_record:realm_stations(RealmId, []),
    ?assertEqual(Expected, macula_record:storage_key(R)),
    ?assertNotEqual(RealmId, macula_record:storage_key(R)).

storage_key_procedure_advertisement_is_uri_hash_test() ->
    Kp = macula_identity:generate(),
    Uri = <<"mcp://weather/forecast">>,
    R = macula_record:procedure_advertisement(macula_identity:public(Kp),
                                              Uri,
                                              crypto:strong_rand_bytes(32)),
    ?assertEqual(crypto:hash(sha256, Uri), macula_record:storage_key(R)),
    %% The envelope key (advertiser NodeId) differs from the storage key.
    ?assertNotEqual(macula_record:key(R), macula_record:storage_key(R)).

storage_key_tombstone_is_superseded_key_test() ->
    Kp = macula_identity:generate(),
    Pub = macula_identity:public(Kp),
    Tomb = macula_record:tombstone(Pub, 16#01, revoked),
    ?assertEqual(Pub, macula_record:storage_key(Tomb)).

%%------------------------------------------------------------------
%% refresh/2 — owner republish
%%------------------------------------------------------------------

refresh_preserves_type_key_payload_test() ->
    Kp = macula_identity:generate(),
    R = macula_record:sign(
          macula_record:node_record(macula_identity:public(Kp), [], 7),
          Kp),
    timer:sleep(2),
    Fresh = macula_record:refresh(R, Kp),
    ?assertEqual(macula_record:type(R),    macula_record:type(Fresh)),
    ?assertEqual(macula_record:key(R),     macula_record:key(Fresh)),
    ?assertEqual(macula_record:payload(R), macula_record:payload(Fresh)).

refresh_bumps_version_and_timestamps_test() ->
    Kp = macula_identity:generate(),
    R = macula_record:sign(
          macula_record:node_record(macula_identity:public(Kp), [], 0),
          Kp),
    timer:sleep(2),
    Fresh = macula_record:refresh(R, Kp),
    ?assertNotEqual(macula_record:version(R), macula_record:version(Fresh)),
    ?assert(macula_record:created_at(Fresh) >= macula_record:created_at(R)),
    ?assert(macula_record:expires_at(Fresh) >= macula_record:expires_at(R)).

refresh_preserves_ttl_duration_test() ->
    Kp = macula_identity:generate(),
    R = macula_record:sign(
          macula_record:node_record(macula_identity:public(Kp), [], 0,
                                    #{ttl_ms => 60_000}),
          Kp),
    Fresh = macula_record:refresh(R, Kp),
    Ttl  = macula_record:expires_at(R)     - macula_record:created_at(R),
    Ttl2 = macula_record:expires_at(Fresh) - macula_record:created_at(Fresh),
    ?assertEqual(Ttl, Ttl2).

refresh_produces_verifiable_record_test() ->
    Kp = macula_identity:generate(),
    R = macula_record:sign(
          macula_record:node_record(macula_identity:public(Kp), [], 0),
          Kp),
    Fresh = macula_record:refresh(R, Kp),
    ?assertMatch({ok, _}, macula_record:verify(Fresh)).

refresh_round_trip_over_wire_test() ->
    Kp = macula_identity:generate(),
    R = macula_record:sign(
          macula_record:node_record(macula_identity:public(Kp), [], 0),
          Kp),
    Fresh = macula_record:refresh(R, Kp),
    {ok, Decoded} = macula_record:decode(macula_record:encode(Fresh)),
    ?assertEqual(Fresh, Decoded).

%%------------------------------------------------------------------
%% foundation_seed_list (§9.14)
%%------------------------------------------------------------------

foundation_seed_list_shape_test() ->
    Kp = macula_identity:generate(),
    Fk = macula_identity:public(Kp),
    Seed1 = #{node_id => crypto:strong_rand_bytes(32),
              addresses => [#{{text, <<"v6">>} => {text, <<"2a02::1">>},
                              {text, <<"port">>} => 7000}],
              tier => 4},
    Seed2 = #{node_id => crypto:strong_rand_bytes(32),
              addresses => [],
              tier => 3},
    R = macula_record:foundation_seed_list(Fk, [Seed1, Seed2]),
    ?assertEqual(16#0D, macula_record:type(R)),
    ?assertEqual(Fk, macula_record:key(R)),
    P = macula_record:payload(R),
    [E1, E2] = maps:get({text, <<"seeds">>}, P),
    ?assertEqual(4, maps:get({text, <<"tier">>}, E1)),
    ?assertEqual(3, maps:get({text, <<"tier">>}, E2)).

foundation_seed_list_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    R = macula_record:foundation_seed_list(
          macula_identity:public(Kp),
          [#{node_id => crypto:strong_rand_bytes(32),
             addresses => [], tier => 4}]),
    Signed = macula_record:sign(R, Kp),
    {ok, Decoded} = macula_record:decode(macula_record:encode(Signed)),
    ?assertEqual(16#0D, macula_record:type(Decoded)),
    {ok, _} = macula_record:verify(Decoded).

foundation_seed_list_rejects_wrong_tier_test() ->
    Kp = macula_identity:generate(),
    ?assertError(function_clause,
                 macula_record:foundation_seed_list(
                   macula_identity:public(Kp),
                   [#{node_id => crypto:strong_rand_bytes(32),
                      addresses => [], tier => 1}])).

%%------------------------------------------------------------------
%% foundation_parameter (§9.15)
%%------------------------------------------------------------------

foundation_parameter_shape_test() ->
    Kp = macula_identity:generate(),
    R = macula_record:foundation_parameter(
          macula_identity:public(Kp), <<"puzzle_difficulty">>, 8),
    ?assertEqual(16#0E, macula_record:type(R)),
    P = macula_record:payload(R),
    ?assertEqual({text, <<"puzzle_difficulty">>},
                 maps:get({text, <<"param_name">>}, P)),
    ?assertEqual(8, maps:get({text, <<"param_value">>}, P)),
    ?assertEqual(null, maps:get({text, <<"prior_version">>}, P)).

foundation_parameter_with_prior_version_test() ->
    Kp = macula_identity:generate(),
    Prior = macula_record_uuid:v7(erlang:system_time(millisecond) - 1),
    R = macula_record:foundation_parameter(
          macula_identity:public(Kp), <<"tRepublish_ms">>, 3_600_000,
          #{prior_version => Prior}),
    P = macula_record:payload(R),
    ?assertEqual(Prior, maps:get({text, <<"prior_version">>}, P)).

foundation_parameter_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    R = macula_record:foundation_parameter(
          macula_identity:public(Kp), <<"tExpire_ms">>, 86_400_000),
    Signed = macula_record:sign(R, Kp),
    {ok, Decoded} = macula_record:decode(macula_record:encode(Signed)),
    ?assertEqual(16#0E, macula_record:type(Decoded)),
    {ok, _} = macula_record:verify(Decoded).

%%------------------------------------------------------------------
%% foundation_realm_trust_list (§9.16)
%%------------------------------------------------------------------

foundation_realm_trust_list_shape_test() ->
    Kp = macula_identity:generate(),
    T1 = crypto:strong_rand_bytes(32),
    T2 = crypto:strong_rand_bytes(32),
    Rv = crypto:strong_rand_bytes(32),
    R = macula_record:foundation_realm_trust_list(
          macula_identity:public(Kp), [T1, T2],
          #{realms_revoked => [Rv]}),
    ?assertEqual(16#0F, macula_record:type(R)),
    P = macula_record:payload(R),
    ?assertEqual([T1, T2], maps:get({text, <<"realms_trusted">>}, P)),
    ?assertEqual([Rv],     maps:get({text, <<"realms_revoked">>}, P)).

foundation_realm_trust_list_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    R = macula_record:foundation_realm_trust_list(
          macula_identity:public(Kp),
          [crypto:strong_rand_bytes(32)]),
    Signed = macula_record:sign(R, Kp),
    {ok, Decoded} = macula_record:decode(macula_record:encode(Signed)),
    ?assertEqual(16#0F, macula_record:type(Decoded)),
    {ok, _} = macula_record:verify(Decoded).

%%------------------------------------------------------------------
%% foundation_t3_attestation (§9.17)
%%------------------------------------------------------------------

foundation_t3_attestation_shape_test() ->
    Kp = macula_identity:generate(),
    Station = crypto:strong_rand_bytes(32),
    Audit = erlang:system_time(millisecond),
    R = macula_record:foundation_t3_attestation(
          macula_identity:public(Kp), Station, Audit,
          #{notes => <<"audited Q2-2026">>}),
    ?assertEqual(16#10, macula_record:type(R)),
    P = macula_record:payload(R),
    ?assertEqual(Station, maps:get({text, <<"station_id">>}, P)),
    ?assertEqual(3, maps:get({text, <<"tier_attested">>}, P)),
    ?assertEqual({text, <<"audited Q2-2026">>},
                 maps:get({text, <<"notes">>}, P)).

foundation_t3_attestation_wire_roundtrip_test() ->
    Kp = macula_identity:generate(),
    R = macula_record:foundation_t3_attestation(
          macula_identity:public(Kp),
          crypto:strong_rand_bytes(32),
          erlang:system_time(millisecond)),
    Signed = macula_record:sign(R, Kp),
    {ok, Decoded} = macula_record:decode(macula_record:encode(Signed)),
    ?assertEqual(16#10, macula_record:type(Decoded)),
    {ok, _} = macula_record:verify(Decoded).

%%------------------------------------------------------------------
%% storage_key for foundation types
%%------------------------------------------------------------------

storage_key_foundation_seed_list_is_hashed_test() ->
    Kp = macula_identity:generate(),
    Fk = macula_identity:public(Kp),
    R = macula_record:foundation_seed_list(Fk, []),
    ?assertEqual(32, byte_size(macula_record:storage_key(R))),
    ?assertNotEqual(Fk, macula_record:storage_key(R)).

storage_key_foundation_parameter_varies_by_name_test() ->
    Kp = macula_identity:generate(),
    Fk = macula_identity:public(Kp),
    R1 = macula_record:foundation_parameter(Fk, <<"a">>, 1),
    R2 = macula_record:foundation_parameter(Fk, <<"b">>, 1),
    ?assertNotEqual(macula_record:storage_key(R1),
                    macula_record:storage_key(R2)).

storage_key_foundation_t3_attestation_varies_by_station_test() ->
    Kp = macula_identity:generate(),
    Fk = macula_identity:public(Kp),
    S1 = crypto:strong_rand_bytes(32),
    S2 = crypto:strong_rand_bytes(32),
    Now = erlang:system_time(millisecond),
    R1 = macula_record:foundation_t3_attestation(Fk, S1, Now),
    R2 = macula_record:foundation_t3_attestation(Fk, S2, Now),
    ?assertNotEqual(macula_record:storage_key(R1),
                    macula_record:storage_key(R2)).

%%------------------------------------------------------------------
%% Domain-defined record types (0x20-0xFF) — envelope/4
%%------------------------------------------------------------------

domain_envelope_round_trip_test() ->
    Kp     = macula_identity:generate(),
    Pub    = macula_identity:public(Kp),
    Type   = 16#20,
    Payload = #{
        {text, <<"realm">>}      => <<"io.macula">>,
        {text, <<"member_did">>} => <<"did:macula:abc">>
    },
    R0 = macula_record:envelope(Type, Pub, Payload, #{}),
    Signed = macula_record:sign(R0, Kp),
    {ok, Verified} = macula_record:verify(Signed),
    ?assertEqual(Type, macula_record:type(Verified)),
    ?assertEqual(Pub,  macula_record:key(Verified)),
    Wire = macula_record:encode(Signed),
    {ok, Decoded} = macula_record:decode(Wire),
    ?assertEqual(Type, macula_record:type(Decoded)),
    ?assertEqual(Payload, macula_record:payload(Decoded)).

domain_storage_key_without_subject_is_signer_pubkey_test() ->
    Kp  = macula_identity:generate(),
    Pub = macula_identity:public(Kp),
    R   = macula_record:envelope(16#42, Pub, #{}, #{}),
    ?assertEqual(Pub, macula_record:storage_key(R)).

domain_storage_key_with_subject_varies_test() ->
    %% One signer, two subjects → distinct DHT slots.
    Kp  = macula_identity:generate(),
    Pub = macula_identity:public(Kp),
    R1 = macula_record:envelope(16#22, Pub, #{}, #{subject_id => <<"license-aaa">>}),
    R2 = macula_record:envelope(16#22, Pub, #{}, #{subject_id => <<"license-bbb">>}),
    ?assertNotEqual(macula_record:storage_key(R1),
                    macula_record:storage_key(R2)),
    ?assertEqual(32, byte_size(macula_record:storage_key(R1))),
    ?assertEqual(32, byte_size(macula_record:storage_key(R2))).

domain_subject_id_round_trips_through_wire_test() ->
    Kp     = macula_identity:generate(),
    Pub    = macula_identity:public(Kp),
    Sid    = crypto:hash(sha256, <<"member-x@realm-y">>),
    R0     = macula_record:envelope(16#20, Pub, #{}, #{subject_id => Sid}),
    Signed = macula_record:sign(R0, Kp),
    {ok, V} = macula_record:verify(Signed),
    ?assertEqual(Sid, maps:get(subject_id, V)),
    Wire   = macula_record:encode(Signed),
    {ok, D} = macula_record:decode(Wire),
    ?assertEqual(Sid, maps:get(subject_id, D)),
    %% storage_key must agree on both sides
    ?assertEqual(macula_record:storage_key(Signed),
                 macula_record:storage_key(D)).

domain_envelope_rejects_bad_key_size_test() ->
    ShortKey = <<1, 2, 3>>,
    ?assertError(function_clause,
                 macula_record:envelope(16#20, ShortKey, #{}, #{})).
