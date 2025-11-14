%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_core_types module.
%%% Tests core type definitions and encoding/decoding functions.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_core_types_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% node_id/0 Tests
%%%===================================================================

node_id_returns_binary_test() ->
    NodeId = macula_core_types:node_id(),
    ?assert(is_binary(NodeId)).

node_id_is_32_bytes_test() ->
    NodeId = macula_core_types:node_id(),
    ?assertEqual(32, byte_size(NodeId)).

node_id_is_unique_test() ->
    %% Generate multiple node IDs and ensure they are different
    NodeId1 = macula_core_types:node_id(),
    NodeId2 = macula_core_types:node_id(),
    NodeId3 = macula_core_types:node_id(),

    ?assertNotEqual(NodeId1, NodeId2),
    ?assertNotEqual(NodeId2, NodeId3),
    ?assertNotEqual(NodeId1, NodeId3).

node_id_is_random_test() ->
    %% Generate many IDs and check they are all different (probabilistic test)
    Ids = [macula_core_types:node_id() || _ <- lists:seq(1, 10)],
    UniqueIds = lists:usort(Ids),

    %% All 10 IDs should be unique
    ?assertEqual(10, length(UniqueIds)).

node_id_uses_strong_randomness_test() ->
    %% Strong random bytes should not be all zeros
    NodeId = macula_core_types:node_id(),
    ?assertNotEqual(<<0:256>>, NodeId).

%%%===================================================================
%%% realm_id/1 Tests
%%%===================================================================

realm_id_returns_binary_test() ->
    RealmId = macula_core_types:realm_id(<<"test.realm">>),
    ?assert(is_binary(RealmId)).

realm_id_is_32_bytes_test() ->
    RealmId = macula_core_types:realm_id(<<"test.realm">>),
    ?assertEqual(32, byte_size(RealmId)).

realm_id_is_deterministic_test() ->
    %% Same realm name should produce same ID
    RealmName = <<"org.example.mesh">>,

    RealmId1 = macula_core_types:realm_id(RealmName),
    RealmId2 = macula_core_types:realm_id(RealmName),
    RealmId3 = macula_core_types:realm_id(RealmName),

    ?assertEqual(RealmId1, RealmId2),
    ?assertEqual(RealmId2, RealmId3).

realm_id_different_names_test() ->
    %% Different realm names should produce different IDs
    RealmId1 = macula_core_types:realm_id(<<"realm1">>),
    RealmId2 = macula_core_types:realm_id(<<"realm2">>),
    RealmId3 = macula_core_types:realm_id(<<"realm3">>),

    ?assertNotEqual(RealmId1, RealmId2),
    ?assertNotEqual(RealmId2, RealmId3),
    ?assertNotEqual(RealmId1, RealmId3).

realm_id_case_sensitive_test() ->
    %% Case differences should produce different IDs
    RealmId1 = macula_core_types:realm_id(<<"Realm">>),
    RealmId2 = macula_core_types:realm_id(<<"realm">>),
    RealmId3 = macula_core_types:realm_id(<<"REALM">>),

    ?assertNotEqual(RealmId1, RealmId2),
    ?assertNotEqual(RealmId2, RealmId3),
    ?assertNotEqual(RealmId1, RealmId3).

realm_id_empty_binary_test() ->
    %% Empty realm name should produce valid ID
    RealmId = macula_core_types:realm_id(<<>>),
    ?assert(is_binary(RealmId)),
    ?assertEqual(32, byte_size(RealmId)).

realm_id_special_characters_test() ->
    %% Realm names with special characters
    RealmId1 = macula_core_types:realm_id(<<"realm-with-dashes">>),
    RealmId2 = macula_core_types:realm_id(<<"realm_with_underscores">>),
    RealmId3 = macula_core_types:realm_id(<<"realm.with.dots">>),

    ?assert(is_binary(RealmId1)),
    ?assert(is_binary(RealmId2)),
    ?assert(is_binary(RealmId3)),

    ?assertNotEqual(RealmId1, RealmId2),
    ?assertNotEqual(RealmId2, RealmId3).

realm_id_long_name_test() ->
    %% Long realm name
    LongName = iolist_to_binary(lists:duplicate(100, <<"segment.">>)),
    RealmId = macula_core_types:realm_id(LongName),

    ?assert(is_binary(RealmId)),
    ?assertEqual(32, byte_size(RealmId)).

%%%===================================================================
%%% encode_address/1 - IPv4 Tests
%%%===================================================================

encode_address_ipv4_simple_test() ->
    %% Encode localhost:8080
    Address = {{127, 0, 0, 1}, 8080},
    Encoded = macula_core_types:encode_address(Address),

    %% Format: 1 byte version (4) + 4 bytes IP + 2 bytes port = 7 bytes
    ?assertEqual(7, byte_size(Encoded)),

    %% Verify structure: <<4:8, 127:8, 0:8, 0:8, 1:8, 8080:16>>
    <<Version:8, A:8, B:8, C:8, D:8, Port:16>> = Encoded,
    ?assertEqual(4, Version),
    ?assertEqual({127, 0, 0, 1}, {A, B, C, D}),
    ?assertEqual(8080, Port).

encode_address_ipv4_zero_test() ->
    %% Encode 0.0.0.0:0
    Address = {{0, 0, 0, 0}, 0},
    Encoded = macula_core_types:encode_address(Address),

    ?assertEqual(<<4:8, 0:8, 0:8, 0:8, 0:8, 0:16>>, Encoded).

encode_address_ipv4_max_values_test() ->
    %% Encode 255.255.255.255:65535
    Address = {{255, 255, 255, 255}, 65535},
    Encoded = macula_core_types:encode_address(Address),

    <<Version:8, A:8, B:8, C:8, D:8, Port:16>> = Encoded,
    ?assertEqual(4, Version),
    ?assertEqual({255, 255, 255, 255}, {A, B, C, D}),
    ?assertEqual(65535, Port).

encode_address_ipv4_common_ports_test() ->
    %% Test common port numbers
    Tests = [
        {{192, 168, 1, 1}, 80},      % HTTP
        {{10, 0, 0, 1}, 443},        % HTTPS
        {{172, 16, 0, 1}, 22},       % SSH
        {{8, 8, 8, 8}, 53}           % DNS
    ],

    lists:foreach(fun({IP, Port}) ->
        Encoded = macula_core_types:encode_address({IP, Port}),
        ?assertEqual(7, byte_size(Encoded)),
        <<4:8, _:4/binary, Port:16>> = Encoded
    end, Tests).

%%%===================================================================
%%% encode_address/1 - IPv6 Tests
%%%===================================================================

encode_address_ipv6_simple_test() ->
    %% Encode localhost IPv6 (::1) on port 8080
    Address = {{0, 0, 0, 0, 0, 0, 0, 1}, 8080},
    Encoded = macula_core_types:encode_address(Address),

    %% Format: 1 byte version (6) + 16 bytes IP + 2 bytes port = 19 bytes
    ?assertEqual(19, byte_size(Encoded)),

    <<Version:8, _IP:16/binary, Port:16>> = Encoded,
    ?assertEqual(6, Version),
    ?assertEqual(8080, Port).

encode_address_ipv6_zero_test() ->
    %% Encode :: (all zeros) on port 0
    Address = {{0, 0, 0, 0, 0, 0, 0, 0}, 0},
    Encoded = macula_core_types:encode_address(Address),

    ?assertEqual(19, byte_size(Encoded)),

    <<Version:8, A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16, Port:16>> = Encoded,
    ?assertEqual(6, Version),
    ?assertEqual({0, 0, 0, 0, 0, 0, 0, 0}, {A, B, C, D, E, F, G, H}),
    ?assertEqual(0, Port).

encode_address_ipv6_full_address_test() ->
    %% Encode 2001:0db8:85a3:0000:0000:8a2e:0370:7334
    Address = {{16#2001, 16#0db8, 16#85a3, 16#0000, 16#0000, 16#8a2e, 16#0370, 16#7334}, 443},
    Encoded = macula_core_types:encode_address(Address),

    ?assertEqual(19, byte_size(Encoded)),

    <<Version:8, A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16, Port:16>> = Encoded,
    ?assertEqual(6, Version),
    ?assertEqual({16#2001, 16#0db8, 16#85a3, 16#0000, 16#0000, 16#8a2e, 16#0370, 16#7334},
                 {A, B, C, D, E, F, G, H}),
    ?assertEqual(443, Port).

encode_address_ipv6_max_values_test() ->
    %% Encode ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff:65535
    Address = {{16#ffff, 16#ffff, 16#ffff, 16#ffff, 16#ffff, 16#ffff, 16#ffff, 16#ffff}, 65535},
    Encoded = macula_core_types:encode_address(Address),

    ?assertEqual(19, byte_size(Encoded)),
    <<6:8, _:16/binary, 65535:16>> = Encoded.

%%%===================================================================
%%% decode_address/1 - IPv4 Tests
%%%===================================================================

decode_address_ipv4_simple_test() ->
    %% Decode 127.0.0.1:8080
    Binary = <<4:8, 127:8, 0:8, 0:8, 1:8, 8080:16>>,
    Result = macula_core_types:decode_address(Binary),

    ?assertMatch({ok, {{127, 0, 0, 1}, 8080}}, Result).

decode_address_ipv4_zero_test() ->
    Binary = <<4:8, 0:8, 0:8, 0:8, 0:8, 0:16>>,
    Result = macula_core_types:decode_address(Binary),

    ?assertMatch({ok, {{0, 0, 0, 0}, 0}}, Result).

decode_address_ipv4_max_values_test() ->
    Binary = <<4:8, 255:8, 255:8, 255:8, 255:8, 65535:16>>,
    Result = macula_core_types:decode_address(Binary),

    ?assertMatch({ok, {{255, 255, 255, 255}, 65535}}, Result).

%%%===================================================================
%%% decode_address/1 - IPv6 Tests
%%%===================================================================

decode_address_ipv6_simple_test() ->
    %% Decode ::1:8080
    Binary = <<6:8, 0:16, 0:16, 0:16, 0:16, 0:16, 0:16, 0:16, 1:16, 8080:16>>,
    Result = macula_core_types:decode_address(Binary),

    ?assertMatch({ok, {{0, 0, 0, 0, 0, 0, 0, 1}, 8080}}, Result).

decode_address_ipv6_zero_test() ->
    Binary = <<6:8, 0:16, 0:16, 0:16, 0:16, 0:16, 0:16, 0:16, 0:16, 0:16>>,
    Result = macula_core_types:decode_address(Binary),

    ?assertMatch({ok, {{0, 0, 0, 0, 0, 0, 0, 0}, 0}}, Result).

decode_address_ipv6_full_address_test() ->
    Binary = <<6:8, 16#2001:16, 16#0db8:16, 16#85a3:16, 16#0000:16,
               16#0000:16, 16#8a2e:16, 16#0370:16, 16#7334:16, 443:16>>,
    Result = macula_core_types:decode_address(Binary),

    Expected = {{16#2001, 16#0db8, 16#85a3, 16#0000, 16#0000, 16#8a2e, 16#0370, 16#7334}, 443},
    ?assertMatch({ok, Expected}, Result).

%%%===================================================================
%%% decode_address/1 - Error Cases
%%%===================================================================

decode_address_invalid_version_test() ->
    %% Invalid version number (5)
    Binary = <<5:8, 127:8, 0:8, 0:8, 1:8, 8080:16>>,
    Result = macula_core_types:decode_address(Binary),

    ?assertEqual({error, invalid_address}, Result).

decode_address_too_short_test() ->
    %% Binary too short
    Binary = <<4:8, 127:8, 0:8>>,
    Result = macula_core_types:decode_address(Binary),

    ?assertEqual({error, invalid_address}, Result).

decode_address_empty_binary_test() ->
    Result = macula_core_types:decode_address(<<>>),
    ?assertEqual({error, invalid_address}, Result).

decode_address_malformed_ipv4_test() ->
    %% IPv4 marker but wrong length
    Binary = <<4:8, 127:8, 0:8, 0:8>>,
    Result = macula_core_types:decode_address(Binary),

    ?assertEqual({error, invalid_address}, Result).

decode_address_malformed_ipv6_test() ->
    %% IPv6 marker but wrong length
    Binary = <<6:8, 0:16, 0:16, 0:16>>,
    Result = macula_core_types:decode_address(Binary),

    ?assertEqual({error, invalid_address}, Result).

%%%===================================================================
%%% Roundtrip Tests
%%%===================================================================

roundtrip_ipv4_localhost_test() ->
    Address = {{127, 0, 0, 1}, 8080},
    Encoded = macula_core_types:encode_address(Address),
    {ok, Decoded} = macula_core_types:decode_address(Encoded),

    ?assertEqual(Address, Decoded).

roundtrip_ipv4_zero_test() ->
    Address = {{0, 0, 0, 0}, 0},
    Encoded = macula_core_types:encode_address(Address),
    {ok, Decoded} = macula_core_types:decode_address(Encoded),

    ?assertEqual(Address, Decoded).

roundtrip_ipv4_max_test() ->
    Address = {{255, 255, 255, 255}, 65535},
    Encoded = macula_core_types:encode_address(Address),
    {ok, Decoded} = macula_core_types:decode_address(Encoded),

    ?assertEqual(Address, Decoded).

roundtrip_ipv6_localhost_test() ->
    Address = {{0, 0, 0, 0, 0, 0, 0, 1}, 8080},
    Encoded = macula_core_types:encode_address(Address),
    {ok, Decoded} = macula_core_types:decode_address(Encoded),

    ?assertEqual(Address, Decoded).

roundtrip_ipv6_zero_test() ->
    Address = {{0, 0, 0, 0, 0, 0, 0, 0}, 0},
    Encoded = macula_core_types:encode_address(Address),
    {ok, Decoded} = macula_core_types:decode_address(Encoded),

    ?assertEqual(Address, Decoded).

roundtrip_ipv6_full_address_test() ->
    Address = {{16#2001, 16#0db8, 16#85a3, 16#0000, 16#0000, 16#8a2e, 16#0370, 16#7334}, 443},
    Encoded = macula_core_types:encode_address(Address),
    {ok, Decoded} = macula_core_types:decode_address(Encoded),

    ?assertEqual(Address, Decoded).

roundtrip_multiple_addresses_test() ->
    Addresses = [
        {{192, 168, 1, 1}, 80},
        {{10, 0, 0, 1}, 443},
        {{172, 16, 0, 1}, 22},
        {{8, 8, 8, 8}, 53},
        {{0, 0, 0, 0, 0, 0, 0, 1}, 8080},
        {{16#fe80, 0, 0, 0, 16#42, 16#acff, 16#fe11, 16#3}, 9000}
    ],

    lists:foreach(fun(Address) ->
        Encoded = macula_core_types:encode_address(Address),
        {ok, Decoded} = macula_core_types:decode_address(Encoded),
        ?assertEqual(Address, Decoded)
    end, Addresses).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

node_id_and_realm_id_different_test() ->
    %% Node IDs and realm IDs should be different formats
    NodeId = macula_core_types:node_id(),
    RealmId = macula_core_types:realm_id(<<"test">>),

    %% Both 32 bytes
    ?assertEqual(32, byte_size(NodeId)),
    ?assertEqual(32, byte_size(RealmId)),

    %% But different values (node ID is random, realm ID is deterministic)
    ?assertNotEqual(NodeId, RealmId).

address_encoding_format_test() ->
    %% Verify that IPv4 and IPv6 encodings are clearly distinguishable
    IPv4 = macula_core_types:encode_address({{127, 0, 0, 1}, 8080}),
    IPv6 = macula_core_types:encode_address({{0, 0, 0, 0, 0, 0, 0, 1}, 8080}),

    %% Different lengths
    ?assertEqual(7, byte_size(IPv4)),
    ?assertEqual(19, byte_size(IPv6)),

    %% Different version bytes
    <<4:8, _/binary>> = IPv4,
    <<6:8, _/binary>> = IPv6.

realm_id_collision_resistance_test() ->
    %% Generate many realm IDs and ensure no collisions
    RealmNames = [
        iolist_to_binary(io_lib:format("realm~p", [N]))
        || N <- lists:seq(1, 100)
    ],

    RealmIds = [macula_core_types:realm_id(Name) || Name <- RealmNames],
    UniqueIds = lists:usort(RealmIds),

    %% All 100 IDs should be unique
    ?assertEqual(100, length(UniqueIds)).
