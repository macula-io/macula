%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_realm module.
%%% Tests realm name validation, normalization, ID generation, and encoding.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_realm_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% id/1 Tests - Realm ID Generation
%%%===================================================================

id_returns_binary_test() ->
    RealmName = <<"org.example.mesh">>,
    RealmId = macula_realm:id(RealmName),
    ?assert(is_binary(RealmId)).

id_correct_length_test() ->
    RealmName = <<"org.example.mesh">>,
    RealmId = macula_realm:id(RealmName),
    ?assertEqual(32, byte_size(RealmId)).  % SHA-256 = 32 bytes

id_deterministic_test() ->
    RealmName = <<"org.example.mesh">>,
    Id1 = macula_realm:id(RealmName),
    Id2 = macula_realm:id(RealmName),
    ?assertEqual(Id1, Id2).

id_different_realms_different_ids_test() ->
    Realm1 = <<"org.example.mesh">>,
    Realm2 = <<"com.example.mesh">>,
    Id1 = macula_realm:id(Realm1),
    Id2 = macula_realm:id(Realm2),
    ?assertNot(Id1 =:= Id2).

id_case_sensitive_test() ->
    Realm1 = <<"org.Example.Mesh">>,
    Realm2 = <<"org.example.mesh">>,
    Id1 = macula_realm:id(Realm1),
    Id2 = macula_realm:id(Realm2),
    ?assertNot(Id1 =:= Id2).

id_empty_realm_test() ->
    RealmName = <<>>,
    RealmId = macula_realm:id(RealmName),
    ?assertEqual(32, byte_size(RealmId)).

id_single_segment_test() ->
    RealmName = <<"mesh">>,
    RealmId = macula_realm:id(RealmName),
    ?assertEqual(32, byte_size(RealmId)).

id_many_segments_test() ->
    RealmName = <<"org.example.subdomain.test.mesh">>,
    RealmId = macula_realm:id(RealmName),
    ?assertEqual(32, byte_size(RealmId)).

%%%===================================================================
%%% validate/1 Tests - Valid Realm Names
%%%===================================================================

validate_simple_realm_test() ->
    ?assertEqual(ok, macula_realm:validate(<<"org.example.mesh">>)).

validate_single_segment_test() ->
    ?assertEqual(ok, macula_realm:validate(<<"mesh">>)).

validate_two_segments_test() ->
    ?assertEqual(ok, macula_realm:validate(<<"example.mesh">>)).

validate_many_segments_test() ->
    ?assertEqual(ok, macula_realm:validate(<<"org.example.subdomain.test.mesh">>)).

validate_with_hyphens_test() ->
    ?assertEqual(ok, macula_realm:validate(<<"org.example-mesh.test">>)).

validate_with_underscores_test() ->
    ?assertEqual(ok, macula_realm:validate(<<"org.example_mesh.test">>)).

validate_with_numbers_test() ->
    ?assertEqual(ok, macula_realm:validate(<<"org.example123.mesh">>)).

validate_uppercase_test() ->
    ?assertEqual(ok, macula_realm:validate(<<"org.Example.Mesh">>)).

validate_mixed_case_test() ->
    ?assertEqual(ok, macula_realm:validate(<<"Org.EXAMPLE.mesh">>)).

validate_all_valid_chars_test() ->
    ?assertEqual(ok, macula_realm:validate(<<"org.example-test_123.mesh">>)).

%%%===================================================================
%%% validate/1 Tests - Invalid Realm Names
%%%===================================================================

validate_empty_realm_test() ->
    ?assertEqual({error, empty_realm}, macula_realm:validate(<<>>)).

validate_leading_dot_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<".org.example.mesh">>)).

validate_trailing_dot_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org.example.mesh.">>)).

validate_double_dot_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org..example.mesh">>)).

validate_multiple_double_dots_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org..example..mesh">>)).

validate_invalid_char_space_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org.example mesh">>)).

validate_invalid_char_slash_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org/example/mesh">>)).

validate_invalid_char_backslash_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org\\example\\mesh">>)).

validate_invalid_char_at_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org@example.mesh">>)).

validate_invalid_char_hash_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org#example.mesh">>)).

validate_invalid_char_dollar_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org$example.mesh">>)).

validate_invalid_char_percent_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org%example.mesh">>)).

validate_invalid_char_ampersand_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org&example.mesh">>)).

validate_invalid_char_asterisk_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org*example.mesh">>)).

validate_invalid_char_plus_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org+example.mesh">>)).

validate_invalid_char_equals_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org=example.mesh">>)).

validate_invalid_char_bracket_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org[example].mesh">>)).

validate_invalid_char_brace_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org{example}.mesh">>)).

validate_invalid_char_pipe_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org|example.mesh">>)).

validate_invalid_char_colon_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org:example.mesh">>)).

validate_invalid_char_semicolon_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org;example.mesh">>)).

validate_invalid_char_quote_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org\"example\".mesh">>)).

validate_invalid_char_apostrophe_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org'example'.mesh">>)).

validate_invalid_char_comma_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org,example,mesh">>)).

validate_invalid_char_question_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org?example.mesh">>)).

validate_invalid_char_exclamation_test() ->
    ?assertEqual({error, invalid_realm}, macula_realm:validate(<<"org!example.mesh">>)).

%%%===================================================================
%%% normalize/1 Tests
%%%===================================================================

normalize_lowercase_test() ->
    ?assertEqual(<<"org.example.mesh">>, macula_realm:normalize(<<"Org.Example.Mesh">>)).

normalize_uppercase_test() ->
    ?assertEqual(<<"org.example.mesh">>, macula_realm:normalize(<<"ORG.EXAMPLE.MESH">>)).

normalize_trim_leading_test() ->
    ?assertEqual(<<"org.example.mesh">>, macula_realm:normalize(<<"  org.example.mesh">>)).

normalize_trim_trailing_test() ->
    ?assertEqual(<<"org.example.mesh">>, macula_realm:normalize(<<"org.example.mesh  ">>)).

normalize_trim_both_test() ->
    ?assertEqual(<<"org.example.mesh">>, macula_realm:normalize(<<"  org.example.mesh  ">>)).

normalize_trim_tabs_test() ->
    ?assertEqual(<<"org.example.mesh">>, macula_realm:normalize(<<"\torg.example.mesh\t">>)).

normalize_trim_newlines_test() ->
    ?assertEqual(<<"org.example.mesh">>, macula_realm:normalize(<<"\norg.example.mesh\n">>)).

normalize_already_normalized_test() ->
    ?assertEqual(<<"org.example.mesh">>, macula_realm:normalize(<<"org.example.mesh">>)).

normalize_empty_test() ->
    ?assertEqual(<<>>, macula_realm:normalize(<<>>)).

normalize_whitespace_only_test() ->
    ?assertEqual(<<>>, macula_realm:normalize(<<"   ">>)).

normalize_preserves_hyphens_test() ->
    ?assertEqual(<<"org.example-mesh.test">>, macula_realm:normalize(<<"Org.Example-Mesh.Test">>)).

normalize_preserves_underscores_test() ->
    ?assertEqual(<<"org.example_mesh.test">>, macula_realm:normalize(<<"Org.Example_Mesh.Test">>)).

normalize_preserves_numbers_test() ->
    ?assertEqual(<<"org.example123.mesh">>, macula_realm:normalize(<<"Org.Example123.Mesh">>)).

%%%===================================================================
%%% equals/2 Tests
%%%===================================================================

equals_same_realm_test() ->
    Realm = <<"org.example.mesh">>,
    ?assert(macula_realm:equals(Realm, Realm)).

equals_identical_realms_test() ->
    Realm1 = <<"org.example.mesh">>,
    Realm2 = <<"org.example.mesh">>,
    ?assert(macula_realm:equals(Realm1, Realm2)).

equals_different_realms_test() ->
    Realm1 = <<"org.example.mesh">>,
    Realm2 = <<"com.example.mesh">>,
    ?assertNot(macula_realm:equals(Realm1, Realm2)).

equals_case_sensitive_test() ->
    Realm1 = <<"org.Example.mesh">>,
    Realm2 = <<"org.example.mesh">>,
    ?assertNot(macula_realm:equals(Realm1, Realm2)).

equals_empty_realms_test() ->
    Realm1 = <<>>,
    Realm2 = <<>>,
    ?assert(macula_realm:equals(Realm1, Realm2)).

equals_whitespace_matters_test() ->
    Realm1 = <<"org.example.mesh">>,
    Realm2 = <<"org.example.mesh ">>,
    ?assertNot(macula_realm:equals(Realm1, Realm2)).

%%%===================================================================
%%% namespace/1 Tests
%%%===================================================================

namespace_simple_test() ->
    ?assertEqual(<<"org">>, macula_realm:namespace(<<"org.example.mesh">>)).

namespace_single_segment_test() ->
    ?assertEqual(<<"mesh">>, macula_realm:namespace(<<"mesh">>)).

namespace_two_segments_test() ->
    ?assertEqual(<<"example">>, macula_realm:namespace(<<"example.mesh">>)).

namespace_many_segments_test() ->
    ?assertEqual(<<"org">>, macula_realm:namespace(<<"org.example.subdomain.test.mesh">>)).

namespace_empty_realm_test() ->
    ?assertEqual(<<>>, macula_realm:namespace(<<>>)).

namespace_with_numbers_test() ->
    ?assertEqual(<<"org123">>, macula_realm:namespace(<<"org123.example.mesh">>)).

namespace_with_hyphens_test() ->
    ?assertEqual(<<"org-example">>, macula_realm:namespace(<<"org-example.mesh.test">>)).

namespace_with_underscores_test() ->
    ?assertEqual(<<"org_example">>, macula_realm:namespace(<<"org_example.mesh.test">>)).

%%%===================================================================
%%% to_binary/1 and from_binary/1 Tests
%%%===================================================================

to_binary_simple_test() ->
    RealmName = <<"org.example.mesh">>,
    Binary = macula_realm:to_binary(RealmName),
    ExpectedLen = byte_size(RealmName),
    ?assertEqual(<<ExpectedLen:16, RealmName/binary>>, Binary).

to_binary_empty_test() ->
    RealmName = <<>>,
    Binary = macula_realm:to_binary(RealmName),
    ?assertEqual(<<0:16>>, Binary).

to_binary_single_segment_test() ->
    RealmName = <<"mesh">>,
    Binary = macula_realm:to_binary(RealmName),
    ExpectedLen = byte_size(RealmName),
    ?assertEqual(<<ExpectedLen:16, RealmName/binary>>, Binary).

to_binary_long_realm_test() ->
    RealmName = <<"org.example.subdomain.test.mesh.very.long.name">>,
    Binary = macula_realm:to_binary(RealmName),
    ExpectedLen = byte_size(RealmName),
    ?assertEqual(<<ExpectedLen:16, RealmName/binary>>, Binary).

from_binary_simple_test() ->
    RealmName = <<"org.example.mesh">>,
    Binary = macula_realm:to_binary(RealmName),
    ?assertEqual({ok, RealmName}, macula_realm:from_binary(Binary)).

from_binary_empty_test() ->
    Binary = <<0:16>>,
    ?assertEqual({error, empty_realm}, macula_realm:from_binary(Binary)).

from_binary_single_segment_test() ->
    RealmName = <<"mesh">>,
    Binary = macula_realm:to_binary(RealmName),
    ?assertEqual({ok, RealmName}, macula_realm:from_binary(Binary)).

from_binary_invalid_realm_test() ->
    %% Create binary with invalid realm (leading dot)
    InvalidRealm = <<".org.example.mesh">>,
    Len = byte_size(InvalidRealm),
    Binary = <<Len:16, InvalidRealm/binary>>,
    ?assertEqual({error, invalid_realm}, macula_realm:from_binary(Binary)).

from_binary_invalid_binary_test() ->
    %% Not enough bytes
    ?assertEqual({error, invalid_binary}, macula_realm:from_binary(<<5:16, "ab">>)).

from_binary_completely_invalid_test() ->
    ?assertEqual({error, invalid_binary}, macula_realm:from_binary(<<"garbage">>)).

from_binary_length_mismatch_test() ->
    %% Length says 10 but only 5 bytes follow
    ?assertEqual({error, invalid_binary}, macula_realm:from_binary(<<10:16, "hello">>)).

from_binary_zero_length_valid_test() ->
    %% Zero length with no data
    Binary = <<0:16>>,
    ?assertEqual({error, empty_realm}, macula_realm:from_binary(Binary)).

%%%===================================================================
%%% Roundtrip Tests
%%%===================================================================

roundtrip_simple_test() ->
    RealmName = <<"org.example.mesh">>,
    Binary = macula_realm:to_binary(RealmName),
    ?assertEqual({ok, RealmName}, macula_realm:from_binary(Binary)).

roundtrip_single_segment_test() ->
    RealmName = <<"mesh">>,
    Binary = macula_realm:to_binary(RealmName),
    ?assertEqual({ok, RealmName}, macula_realm:from_binary(Binary)).

roundtrip_many_segments_test() ->
    RealmName = <<"org.example.subdomain.test.mesh">>,
    Binary = macula_realm:to_binary(RealmName),
    ?assertEqual({ok, RealmName}, macula_realm:from_binary(Binary)).

roundtrip_with_hyphens_test() ->
    RealmName = <<"org.example-mesh.test">>,
    Binary = macula_realm:to_binary(RealmName),
    ?assertEqual({ok, RealmName}, macula_realm:from_binary(Binary)).

roundtrip_with_underscores_test() ->
    RealmName = <<"org.example_mesh.test">>,
    Binary = macula_realm:to_binary(RealmName),
    ?assertEqual({ok, RealmName}, macula_realm:from_binary(Binary)).

roundtrip_with_numbers_test() ->
    RealmName = <<"org.example123.mesh">>,
    Binary = macula_realm:to_binary(RealmName),
    ?assertEqual({ok, RealmName}, macula_realm:from_binary(Binary)).

roundtrip_uppercase_test() ->
    RealmName = <<"Org.Example.Mesh">>,
    Binary = macula_realm:to_binary(RealmName),
    ?assertEqual({ok, RealmName}, macula_realm:from_binary(Binary)).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

normalize_then_validate_test() ->
    Input = <<"  Org.Example.Mesh  ">>,
    Normalized = macula_realm:normalize(Input),
    ?assertEqual(ok, macula_realm:validate(Normalized)).

normalize_then_id_test() ->
    Input1 = <<"Org.Example.Mesh">>,
    Input2 = <<"org.example.mesh">>,
    Normalized1 = macula_realm:normalize(Input1),
    Normalized2 = macula_realm:normalize(Input2),
    Id1 = macula_realm:id(Normalized1),
    Id2 = macula_realm:id(Normalized2),
    ?assertEqual(Id1, Id2).

full_workflow_test() ->
    %% User input (messy)
    UserInput = <<"  Org.Example.Mesh  ">>,

    %% Normalize
    Normalized = macula_realm:normalize(UserInput),
    ?assertEqual(<<"org.example.mesh">>, Normalized),

    %% Validate
    ?assertEqual(ok, macula_realm:validate(Normalized)),

    %% Generate ID
    RealmId = macula_realm:id(Normalized),
    ?assertEqual(32, byte_size(RealmId)),

    %% Extract namespace
    Namespace = macula_realm:namespace(Normalized),
    ?assertEqual(<<"org">>, Namespace),

    %% Encode/decode
    Binary = macula_realm:to_binary(Normalized),
    ?assertEqual({ok, Normalized}, macula_realm:from_binary(Binary)).

validate_edge_cases_test() ->
    %% Test various edge cases
    EdgeCases = [
        {<<"org.example.mesh">>, ok},
        {<<"a">>, ok},
        {<<"a.b">>, ok},
        {<<"a.b.c.d.e.f.g">>, ok},
        {<<"org123.example456.mesh789">>, ok},
        {<<"org-mesh.test-env.prod">>, ok},
        {<<"org_mesh.test_env.prod">>, ok},
        {<<>>, {error, empty_realm}},
        {<<".org">>, {error, invalid_realm}},
        {<<"org.">>, {error, invalid_realm}},
        {<<"org..mesh">>, {error, invalid_realm}},
        {<<"org.mesh.">>, {error, invalid_realm}},
        {<<".org.mesh">>, {error, invalid_realm}}
    ],
    lists:foreach(fun({Realm, Expected}) ->
        ?assertEqual(Expected, macula_realm:validate(Realm))
    end, EdgeCases).

id_collision_resistance_test() ->
    %% Generate many realm IDs and ensure no collisions
    Realms = [
        <<"org.example.mesh1">>,
        <<"org.example.mesh2">>,
        <<"org.example.mesh3">>,
        <<"com.example.mesh">>,
        <<"net.example.mesh">>,
        <<"io.example.mesh">>,
        <<"dev.example.mesh">>,
        <<"test.example.mesh">>,
        <<"prod.example.mesh">>,
        <<"staging.example.mesh">>
    ],
    Ids = [macula_realm:id(R) || R <- Realms],
    UniqueIds = lists:usort(Ids),
    ?assertEqual(length(Realms), length(UniqueIds)).

namespace_extraction_test() ->
    %% Test namespace extraction from various realm formats
    Tests = [
        {<<"org.example.mesh">>, <<"org">>},
        {<<"com.example.mesh">>, <<"com">>},
        {<<"net.test.prod.mesh">>, <<"net">>},
        {<<"single">>, <<"single">>},
        {<<>>, <<>>}
    ],
    lists:foreach(fun({Realm, Expected}) ->
        ?assertEqual(Expected, macula_realm:namespace(Realm))
    end, Tests).

binary_encoding_lengths_test() ->
    %% Verify encoded length matches expected format
    Realms = [
        <<>>,
        <<"a">>,
        <<"org.mesh">>,
        <<"org.example.mesh">>,
        <<"very.long.realm.name.with.many.segments">>
    ],
    lists:foreach(fun(Realm) ->
        Binary = macula_realm:to_binary(Realm),
        ExpectedSize = 2 + byte_size(Realm),  % 2 bytes for length prefix
        ?assertEqual(ExpectedSize, byte_size(Binary))
    end, Realms).
