%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_uri module.
%%% Tests URI parsing and construction with actual code execution.
%%% Format: macula://realm/node_id_hex
%%% @end
%%%-------------------------------------------------------------------
-module(macula_uri_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% URI Construction Tests
%%%===================================================================

new_creates_valid_uri_test() ->
    Realm = <<"org.example.mesh">>,
    NodeId = <<16#ab, 16#cd, 16#ef, 16#12, 16#34, 16#56, 16#78, 16#90>>,
    Uri = macula_uri:new(Realm, NodeId),

    ?assert(is_binary(Uri)),
    ?assert(macula_uri:is_valid(Uri)).

new_includes_scheme_test() ->
    Realm = <<"test.realm">>,
    NodeId = <<1, 2, 3, 4>>,
    Uri = macula_uri:new(Realm, NodeId),

    %% binary:match returns {Offset, Length}, check it starts at position 0
    ?assertMatch({0, _}, binary:match(Uri, <<"macula://">>)),
    ?assert(binary:match(Uri, Realm) =/= nomatch).

new_converts_node_id_to_hex_test() ->
    Realm = <<"test">>,
    NodeId = <<16#ab, 16#cd>>,
    Uri = macula_uri:new(Realm, NodeId),

    %% Should contain hex representation "abcd"
    ?assert(binary:match(Uri, <<"abcd">>) =/= nomatch).

new_format_test() ->
    Realm = <<"my.realm">>,
    NodeId = <<16#01, 16#02, 16#03>>,
    Uri = macula_uri:new(Realm, NodeId),

    Expected = <<"macula://my.realm/010203">>,
    ?assertEqual(Expected, Uri).

new_with_empty_realm_test() ->
    Realm = <<>>,
    NodeId = <<1, 2, 3>>,
    Uri = macula_uri:new(Realm, NodeId),

    %% Should create URI but parsing may fail validation
    ?assert(is_binary(Uri)).

new_with_32_byte_node_id_test() ->
    Realm = <<"test">>,
    NodeId = crypto:strong_rand_bytes(32),  % Full 32-byte node ID
    Uri = macula_uri:new(Realm, NodeId),

    ?assert(is_binary(Uri)),
    ?assert(byte_size(Uri) > 64).  % At least 64 hex chars for 32 bytes

%%%===================================================================
%%% URI Parsing Tests
%%%===================================================================

parse_valid_uri_test() ->
    Realm = <<"org.example">>,
    NodeId = <<16#aa, 16#bb, 16#cc>>,
    Uri = macula_uri:new(Realm, NodeId),

    {ok, ParsedRealm, ParsedNodeId} = macula_uri:parse(Uri),
    ?assertEqual(Realm, ParsedRealm),
    ?assertEqual(NodeId, ParsedNodeId).

parse_roundtrip_test() ->
    OriginalRealm = <<"test.realm.example">>,
    OriginalNodeId = <<1, 2, 3, 4, 5, 6, 7, 8>>,

    Uri = macula_uri:new(OriginalRealm, OriginalNodeId),
    {ok, Realm, NodeId} = macula_uri:parse(Uri),

    ?assertEqual(OriginalRealm, Realm),
    ?assertEqual(OriginalNodeId, NodeId).

parse_with_random_node_id_test() ->
    OriginalRealm = <<"random.test">>,
    OriginalNodeId = crypto:strong_rand_bytes(32),

    Uri = macula_uri:new(OriginalRealm, OriginalNodeId),
    {ok, Realm, NodeId} = macula_uri:parse(Uri),

    ?assertEqual(OriginalRealm, Realm),
    ?assertEqual(OriginalNodeId, NodeId).

parse_invalid_scheme_test() ->
    Uri = <<"http://test/123">>,
    ?assertEqual({error, invalid_uri}, macula_uri:parse(Uri)).

parse_missing_scheme_test() ->
    Uri = <<"test/123">>,
    ?assertEqual({error, invalid_uri}, macula_uri:parse(Uri)).

parse_wrong_scheme_test() ->
    Uri = <<"macula:/test/123">>,  % Single slash
    ?assertEqual({error, invalid_uri}, macula_uri:parse(Uri)).

parse_missing_realm_test() ->
    Uri = <<"macula:///123">>,  % Empty realm
    ?assertEqual({error, invalid_uri}, macula_uri:parse(Uri)).

parse_missing_node_id_test() ->
    Uri = <<"macula://test.realm/">>,  % Empty node ID
    ?assertEqual({error, invalid_uri}, macula_uri:parse(Uri)).

parse_missing_slash_test() ->
    Uri = <<"macula://test.realm">>,  % No slash separator
    ?assertEqual({error, invalid_uri}, macula_uri:parse(Uri)).

parse_invalid_hex_in_node_id_test() ->
    Uri = <<"macula://test/xyz">>,  % Invalid hex
    ?assertEqual({error, invalid_uri}, macula_uri:parse(Uri)).

parse_odd_length_hex_test() ->
    Uri = <<"macula://test/abc">>,  % Odd number of hex digits
    ?assertEqual({error, invalid_uri}, macula_uri:parse(Uri)).

parse_non_binary_test() ->
    ?assertEqual({error, invalid_uri}, macula_uri:parse("not a binary")).

parse_empty_binary_test() ->
    ?assertEqual({error, invalid_uri}, macula_uri:parse(<<>>)).

parse_with_multiple_slashes_test() ->
    Uri = <<"macula://realm/path/nodeid">>,  % Extra slash
    ?assertEqual({error, invalid_uri}, macula_uri:parse(Uri)).

%%%===================================================================
%%% get_realm Tests
%%%===================================================================

get_realm_from_valid_uri_test() ->
    Realm = <<"org.example.test">>,
    NodeId = <<1, 2, 3, 4>>,
    Uri = macula_uri:new(Realm, NodeId),

    {ok, ExtractedRealm} = macula_uri:get_realm(Uri),
    ?assertEqual(Realm, ExtractedRealm).

get_realm_from_invalid_uri_test() ->
    Uri = <<"invalid">>,
    ?assertEqual({error, invalid_uri}, macula_uri:get_realm(Uri)).

get_realm_with_dots_test() ->
    Realm = <<"com.example.subdomain.service">>,
    NodeId = <<16#ff>>,
    Uri = macula_uri:new(Realm, NodeId),

    {ok, ExtractedRealm} = macula_uri:get_realm(Uri),
    ?assertEqual(Realm, ExtractedRealm).

get_realm_with_hyphens_test() ->
    Realm = <<"my-test-realm">>,
    NodeId = <<1, 2>>,
    Uri = macula_uri:new(Realm, NodeId),

    {ok, ExtractedRealm} = macula_uri:get_realm(Uri),
    ?assertEqual(Realm, ExtractedRealm).

%%%===================================================================
%%% get_node_id Tests
%%%===================================================================

get_node_id_from_valid_uri_test() ->
    Realm = <<"test">>,
    NodeId = <<16#aa, 16#bb, 16#cc, 16#dd>>,
    Uri = macula_uri:new(Realm, NodeId),

    {ok, ExtractedNodeId} = macula_uri:get_node_id(Uri),
    ?assertEqual(NodeId, ExtractedNodeId).

get_node_id_from_invalid_uri_test() ->
    Uri = <<"invalid">>,
    ?assertEqual({error, invalid_uri}, macula_uri:get_node_id(Uri)).

get_node_id_32_bytes_test() ->
    Realm = <<"test">>,
    NodeId = crypto:strong_rand_bytes(32),
    Uri = macula_uri:new(Realm, NodeId),

    {ok, ExtractedNodeId} = macula_uri:get_node_id(Uri),
    ?assertEqual(NodeId, ExtractedNodeId).

get_node_id_single_byte_test() ->
    Realm = <<"test">>,
    NodeId = <<16#42>>,
    Uri = macula_uri:new(Realm, NodeId),

    {ok, ExtractedNodeId} = macula_uri:get_node_id(Uri),
    ?assertEqual(NodeId, ExtractedNodeId).

%%%===================================================================
%%% is_valid Tests
%%%===================================================================

is_valid_with_valid_uri_test() ->
    Realm = <<"test">>,
    NodeId = <<1, 2, 3>>,
    Uri = macula_uri:new(Realm, NodeId),

    ?assert(macula_uri:is_valid(Uri)).

is_valid_with_invalid_scheme_test() ->
    Uri = <<"http://test/123">>,
    ?assertNot(macula_uri:is_valid(Uri)).

is_valid_with_missing_parts_test() ->
    Uri = <<"macula://">>,
    ?assertNot(macula_uri:is_valid(Uri)).

is_valid_with_empty_binary_test() ->
    ?assertNot(macula_uri:is_valid(<<>>)).

is_valid_with_malformed_hex_test() ->
    Uri = <<"macula://realm/xyz">>,
    ?assertNot(macula_uri:is_valid(Uri)).

is_valid_multiple_valid_uris_test() ->
    Uri1 = macula_uri:new(<<"realm1">>, <<1, 2>>),
    Uri2 = macula_uri:new(<<"realm2">>, <<3, 4>>),
    Uri3 = macula_uri:new(<<"realm3">>, <<5, 6>>),

    ?assert(macula_uri:is_valid(Uri1)),
    ?assert(macula_uri:is_valid(Uri2)),
    ?assert(macula_uri:is_valid(Uri3)).

%%%===================================================================
%%% equals Tests
%%%===================================================================

equals_same_uri_test() ->
    Realm = <<"test">>,
    NodeId = <<1, 2, 3>>,
    Uri = macula_uri:new(Realm, NodeId),

    ?assert(macula_uri:equals(Uri, Uri)).

equals_identical_uris_test() ->
    Realm = <<"test">>,
    NodeId = <<1, 2, 3>>,
    Uri1 = macula_uri:new(Realm, NodeId),
    Uri2 = macula_uri:new(Realm, NodeId),

    ?assert(macula_uri:equals(Uri1, Uri2)).

equals_different_realms_test() ->
    NodeId = <<1, 2, 3>>,
    Uri1 = macula_uri:new(<<"realm1">>, NodeId),
    Uri2 = macula_uri:new(<<"realm2">>, NodeId),

    ?assertNot(macula_uri:equals(Uri1, Uri2)).

equals_different_node_ids_test() ->
    Realm = <<"test">>,
    Uri1 = macula_uri:new(Realm, <<1, 2, 3>>),
    Uri2 = macula_uri:new(Realm, <<4, 5, 6>>),

    ?assertNot(macula_uri:equals(Uri1, Uri2)).

equals_empty_uris_test() ->
    ?assert(macula_uri:equals(<<>>, <<>>)).

equals_case_sensitive_realm_test() ->
    NodeId = <<1, 2>>,
    Uri1 = macula_uri:new(<<"Test">>, NodeId),
    Uri2 = macula_uri:new(<<"test">>, NodeId),

    ?assertNot(macula_uri:equals(Uri1, Uri2)).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

full_workflow_test() ->
    %% Create
    Realm = <<"org.example.production">>,
    NodeId = crypto:strong_rand_bytes(32),
    Uri = macula_uri:new(Realm, NodeId),

    %% Validate
    ?assert(macula_uri:is_valid(Uri)),

    %% Parse
    {ok, ParsedRealm, ParsedNodeId} = macula_uri:parse(Uri),
    ?assertEqual(Realm, ParsedRealm),
    ?assertEqual(NodeId, ParsedNodeId),

    %% Extract realm
    {ok, ExtractedRealm} = macula_uri:get_realm(Uri),
    ?assertEqual(Realm, ExtractedRealm),

    %% Extract node ID
    {ok, ExtractedNodeId} = macula_uri:get_node_id(Uri),
    ?assertEqual(NodeId, ExtractedNodeId),

    %% Compare
    Uri2 = macula_uri:new(Realm, NodeId),
    ?assert(macula_uri:equals(Uri, Uri2)).

multiple_uris_different_realms_test() ->
    NodeId = <<16#aa, 16#bb, 16#cc>>,

    Uri1 = macula_uri:new(<<"realm1.example">>, NodeId),
    Uri2 = macula_uri:new(<<"realm2.example">>, NodeId),
    Uri3 = macula_uri:new(<<"realm3.example">>, NodeId),

    ?assert(macula_uri:is_valid(Uri1)),
    ?assert(macula_uri:is_valid(Uri2)),
    ?assert(macula_uri:is_valid(Uri3)),

    ?assertNot(macula_uri:equals(Uri1, Uri2)),
    ?assertNot(macula_uri:equals(Uri2, Uri3)),
    ?assertNot(macula_uri:equals(Uri1, Uri3)).

realm_with_special_characters_test() ->
    %% Test realm with dots, hyphens, underscores
    Realm = <<"my-service_v1.prod.example.com">>,
    NodeId = <<1, 2, 3, 4>>,
    Uri = macula_uri:new(Realm, NodeId),

    ?assert(macula_uri:is_valid(Uri)),
    {ok, ParsedRealm, _} = macula_uri:parse(Uri),
    ?assertEqual(Realm, ParsedRealm).

long_realm_test() ->
    %% Test with very long realm name
    Realm = <<"com.example.very.long.domain.name.with.many.subdomains.for.testing">>,
    NodeId = <<16#ff>>,
    Uri = macula_uri:new(Realm, NodeId),

    ?assert(macula_uri:is_valid(Uri)),
    {ok, ParsedRealm, _} = macula_uri:parse(Uri),
    ?assertEqual(Realm, ParsedRealm).

short_realm_test() ->
    %% Test with single character realm
    Realm = <<"x">>,
    NodeId = <<1>>,
    Uri = macula_uri:new(Realm, NodeId),

    ?assert(macula_uri:is_valid(Uri)),
    {ok, ParsedRealm, _} = macula_uri:parse(Uri),
    ?assertEqual(Realm, ParsedRealm).

node_id_all_zeros_test() ->
    Realm = <<"test">>,
    NodeId = <<0, 0, 0, 0>>,
    Uri = macula_uri:new(Realm, NodeId),

    ?assert(macula_uri:is_valid(Uri)),
    {ok, _, ParsedNodeId} = macula_uri:parse(Uri),
    ?assertEqual(NodeId, ParsedNodeId).

node_id_all_ones_test() ->
    Realm = <<"test">>,
    NodeId = <<16#ff, 16#ff, 16#ff, 16#ff>>,
    Uri = macula_uri:new(Realm, NodeId),

    ?assert(macula_uri:is_valid(Uri)),
    {ok, _, ParsedNodeId} = macula_uri:parse(Uri),
    ?assertEqual(NodeId, ParsedNodeId).

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

parse_with_extra_data_test() ->
    Uri = <<"macula://realm/aabb/extra">>,
    %% Should fail - extra components
    ?assertEqual({error, invalid_uri}, macula_uri:parse(Uri)).

parse_with_query_string_test() ->
    Uri = <<"macula://realm/aabb?query=value">>,
    %% Should fail - query strings not supported
    ?assertEqual({error, invalid_uri}, macula_uri:parse(Uri)).

parse_with_fragment_test() ->
    Uri = <<"macula://realm/aabb#fragment">>,
    %% Should fail - fragments not supported
    ?assertEqual({error, invalid_uri}, macula_uri:parse(Uri)).

parse_scheme_case_sensitive_test() ->
    Uri = <<"MACULA://test/aabb">>,
    %% Scheme is case-sensitive, should fail
    ?assertEqual({error, invalid_uri}, macula_uri:parse(Uri)).
