%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_core_types module.
%%% Demonstrates TDD approach with pure function testing.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_core_types_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Node ID Tests
%%%===================================================================

%% Test: node_id should return a binary
node_id_returns_binary_test() ->
    NodeId = macula_core_types:node_id(),
    ?assert(is_binary(NodeId)).

%% Test: node_id should be 32 bytes (256 bits)
node_id_is_32_bytes_test() ->
    NodeId = macula_core_types:node_id(),
    ?assertEqual(32, byte_size(NodeId)).

%% Test: node_id should generate unique IDs
node_id_is_unique_test() ->
    Id1 = macula_core_types:node_id(),
    Id2 = macula_core_types:node_id(),
    ?assertNotEqual(Id1, Id2).

%% Test: multiple node_ids should all be unique
node_id_uniqueness_test() ->
    Ids = [macula_core_types:node_id() || _ <- lists:seq(1, 100)],
    UniqueIds = lists:usort(Ids),
    ?assertEqual(length(Ids), length(UniqueIds)).

%%%===================================================================
%%% Realm ID Tests
%%%===================================================================

%% Test: realm_id from string
realm_id_from_string_test() ->
    RealmId = macula_core_types:realm_id(<<"my.realm">>),
    ?assert(is_binary(RealmId)),
    ?assertEqual(32, byte_size(RealmId)).

%% Test: same realm name produces same ID
realm_id_deterministic_test() ->
    Id1 = macula_core_types:realm_id(<<"test.realm">>),
    Id2 = macula_core_types:realm_id(<<"test.realm">>),
    ?assertEqual(Id1, Id2).

%% Test: different realm names produce different IDs
realm_id_different_test() ->
    Id1 = macula_core_types:realm_id(<<"realm1">>),
    Id2 = macula_core_types:realm_id(<<"realm2">>),
    ?assertNotEqual(Id1, Id2).

%%%===================================================================
%%% Address Encoding Tests
%%%===================================================================

%% Test: IPv4 address encoding
encode_ipv4_address_test() ->
    Addr = {127, 0, 0, 1},
    Port = 4433,
    Encoded = macula_core_types:encode_address({Addr, Port}),
    ?assert(is_binary(Encoded)).

%% Test: IPv6 address encoding
encode_ipv6_address_test() ->
    Addr = {0, 0, 0, 0, 0, 0, 0, 1},  % ::1
    Port = 4433,
    Encoded = macula_core_types:encode_address({Addr, Port}),
    ?assert(is_binary(Encoded)).

%% Test: round-trip encoding/decoding IPv4
address_roundtrip_ipv4_test() ->
    Original = {{192, 168, 1, 100}, 8080},
    Encoded = macula_core_types:encode_address(Original),
    {ok, Decoded} = macula_core_types:decode_address(Encoded),
    ?assertEqual(Original, Decoded).

%% Test: round-trip encoding/decoding IPv6
address_roundtrip_ipv6_test() ->
    Original = {{16#2001, 16#0db8, 0, 0, 0, 0, 0, 1}, 443},
    Encoded = macula_core_types:encode_address(Original),
    {ok, Decoded} = macula_core_types:decode_address(Encoded),
    ?assertEqual(Original, Decoded).

%%%===================================================================
%%% Error Cases
%%%===================================================================

%% Test: invalid realm_id input
realm_id_invalid_input_test() ->
    ?assertError(function_clause, macula_core_types:realm_id(not_a_binary)).

%% Test: decode_address with invalid binary
decode_address_invalid_test() ->
    ?assertEqual({error, invalid_address},
                 macula_core_types:decode_address(<<1, 2, 3>>)).

%%%===================================================================
%%% Helper Functions (if needed for tests)
%%%===================================================================

%% None needed yet
