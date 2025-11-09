%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_realm module.
%%% Tests written FIRST (TDD red phase).
%%% Realm management and validation.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_realm_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Realm ID Tests
%%%===================================================================

%% Test: id generates deterministic 32-byte realm ID from name
id_test() ->
    RealmId1 = macula_realm:id(<<"my.realm">>),
    RealmId2 = macula_realm:id(<<"my.realm">>),

    ?assert(is_binary(RealmId1)),
    ?assertEqual(32, byte_size(RealmId1)),  % 256-bit SHA-256 hash
    ?assertEqual(RealmId1, RealmId2).  % Deterministic

%% Test: different realm names produce different IDs
id_different_names_test() ->
    RealmId1 = macula_realm:id(<<"realm.one">>),
    RealmId2 = macula_realm:id(<<"realm.two">>),

    ?assertNotEqual(RealmId1, RealmId2).

%%%===================================================================
%%% Realm Name Validation Tests
%%%===================================================================

%% Test: validate accepts valid realm names
validate_valid_test() ->
    ?assertEqual(ok, macula_realm:validate(<<"org.example.mesh">>)),
    ?assertEqual(ok, macula_realm:validate(<<"be.cortexiq.energy">>)),
    ?assertEqual(ok, macula_realm:validate(<<"io.macula">>)).

%% Test: validate rejects empty realm name
validate_empty_test() ->
    ?assertEqual({error, empty_realm}, macula_realm:validate(<<>>)).

%% Test: validate rejects realm with leading dot
validate_leading_dot_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<".example.org">>)).

%% Test: validate rejects realm with trailing dot
validate_trailing_dot_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"example.org.">>)).

%% Test: validate rejects realm with double dots
validate_double_dots_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"example..org">>)).

%% Test: validate rejects realm with invalid characters
validate_invalid_chars_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"example@org">>)),
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"example org">>)),
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"example/org">>)).

%% Test: validate accepts hyphens and underscores
validate_hyphens_underscores_test() ->
    ?assertEqual(ok, macula_realm:validate(<<"my-realm.example_org">>)).

%% Test: validate accepts numbers
validate_numbers_test() ->
    ?assertEqual(ok, macula_realm:validate(<<"realm1.example2">>)).

%%%===================================================================
%%% Realm Normalization Tests
%%%===================================================================

%% Test: normalize converts to lowercase
normalize_lowercase_test() ->
    ?assertEqual(<<"org.example.mesh">>,
                 macula_realm:normalize(<<"Org.Example.Mesh">>)).

%% Test: normalize trims whitespace
normalize_trim_test() ->
    ?assertEqual(<<"org.example.mesh">>,
                 macula_realm:normalize(<<"  org.example.mesh  ">>)).

%% Test: normalize handles already normalized names
normalize_idempotent_test() ->
    Name = <<"org.example.mesh">>,
    ?assertEqual(Name, macula_realm:normalize(Name)).

%%%===================================================================
%%% Realm Comparison Tests
%%%===================================================================

%% Test: equals compares realm IDs
equals_test() ->
    Realm1 = <<"my.realm">>,
    Realm2 = <<"my.realm">>,
    Realm3 = <<"other.realm">>,

    ?assert(macula_realm:equals(Realm1, Realm2)),
    ?assertNot(macula_realm:equals(Realm1, Realm3)).

%% Test: equals is case-insensitive (after normalization)
equals_case_insensitive_test() ->
    Realm1 = macula_realm:normalize(<<"My.Realm">>),
    Realm2 = macula_realm:normalize(<<"my.realm">>),

    ?assert(macula_realm:equals(Realm1, Realm2)).

%%%===================================================================
%%% Namespace Extraction Tests
%%%===================================================================

%% Test: namespace extracts top-level domain
namespace_test() ->
    ?assertEqual(<<"org">>, macula_realm:namespace(<<"org.example.mesh">>)),
    ?assertEqual(<<"be">>, macula_realm:namespace(<<"be.cortexiq.energy">>)),
    ?assertEqual(<<"io">>, macula_realm:namespace(<<"io.macula">>)).

%% Test: namespace returns full name for single segment
namespace_single_segment_test() ->
    ?assertEqual(<<"local">>, macula_realm:namespace(<<"local">>)).

%% Test: namespace returns empty for empty realm
namespace_empty_test() ->
    ?assertEqual(<<>>, macula_realm:namespace(<<>>)).

%%%===================================================================
%%% Encoding/Decoding Tests
%%%===================================================================

%% Test: to_binary encodes realm name
to_binary_test() ->
    RealmName = <<"org.example.mesh">>,
    Binary = macula_realm:to_binary(RealmName),

    ?assert(is_binary(Binary)).

%% Test: from_binary decodes realm name
from_binary_test() ->
    RealmName = <<"org.example.mesh">>,
    Binary = macula_realm:to_binary(RealmName),

    {ok, Decoded} = macula_realm:from_binary(Binary),

    ?assertEqual(RealmName, Decoded).

%% Test: from_binary validates decoded realm
from_binary_invalid_test() ->
    InvalidRealm = <<"..invalid..">>,
    Binary = macula_realm:to_binary(InvalidRealm),

    ?assertMatch({error, _}, macula_realm:from_binary(Binary)).

%% Test: from_binary handles corrupted data
from_binary_corrupted_test() ->
    CorruptedBinary = <<255, 255, 255, 255>>,

    ?assertMatch({error, _}, macula_realm:from_binary(CorruptedBinary)).
