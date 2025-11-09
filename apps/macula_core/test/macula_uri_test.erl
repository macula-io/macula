%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_uri module.
%%% Tests written FIRST (TDD red phase).
%%% Macula URI parsing and construction (macula://realm/node).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_uri_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% URI Construction Tests
%%%===================================================================

%% Test: new constructs URI from realm and node ID
new_test() ->
    Realm = <<"org.example.mesh">>,
    NodeId = macula_id:node_id(),

    Uri = macula_uri:new(Realm, NodeId),

    ?assert(is_binary(Uri)),
    ?assert(binary:match(Uri, <<"macula://">>) =/= nomatch),
    ?assert(binary:match(Uri, Realm) =/= nomatch).

%% Test: new produces deterministic URIs
new_deterministic_test() ->
    Realm = <<"org.example.mesh">>,
    NodeId = <<1:256>>,

    Uri1 = macula_uri:new(Realm, NodeId),
    Uri2 = macula_uri:new(Realm, NodeId),

    ?assertEqual(Uri1, Uri2).

%%%===================================================================
%%% URI Parsing Tests
%%%===================================================================

%% Test: parse extracts realm and node ID from valid URI
parse_test() ->
    Realm = <<"org.example.mesh">>,
    NodeId = macula_id:node_id(),
    Uri = macula_uri:new(Realm, NodeId),

    {ok, ParsedRealm, ParsedNodeId} = macula_uri:parse(Uri),

    ?assertEqual(Realm, ParsedRealm),
    ?assertEqual(NodeId, ParsedNodeId).

%% Test: parse rejects URI without macula:// scheme
parse_invalid_scheme_test() ->
    ?assertEqual({error, invalid_uri}, macula_uri:parse(<<"http://example.com">>)),
    ?assertEqual({error, invalid_uri}, macula_uri:parse(<<"example.com">>)).

%% Test: parse rejects malformed URI
parse_malformed_test() ->
    ?assertEqual({error, invalid_uri}, macula_uri:parse(<<"macula://">>)),
    ?assertEqual({error, invalid_uri}, macula_uri:parse(<<"macula://realm">>)),
    ?assertEqual({error, invalid_uri}, macula_uri:parse(<<"macula:///node">>)).

%%%===================================================================
%%% Realm Extraction Tests
%%%===================================================================

%% Test: get_realm extracts realm from URI
get_realm_test() ->
    Realm = <<"org.example.mesh">>,
    NodeId = macula_id:node_id(),
    Uri = macula_uri:new(Realm, NodeId),

    ?assertEqual({ok, Realm}, macula_uri:get_realm(Uri)).

%% Test: get_realm returns error for invalid URI
get_realm_invalid_test() ->
    ?assertEqual({error, invalid_uri}, macula_uri:get_realm(<<"not a uri">>)).

%%%===================================================================
%%% Node ID Extraction Tests
%%%===================================================================

%% Test: get_node_id extracts node ID from URI
get_node_id_test() ->
    Realm = <<"org.example.mesh">>,
    NodeId = macula_id:node_id(),
    Uri = macula_uri:new(Realm, NodeId),

    ?assertEqual({ok, NodeId}, macula_uri:get_node_id(Uri)).

%% Test: get_node_id returns error for invalid URI
get_node_id_invalid_test() ->
    ?assertEqual({error, invalid_uri}, macula_uri:get_node_id(<<"not a uri">>)).

%%%===================================================================
%%% Validation Tests
%%%===================================================================

%% Test: is_valid validates correct URIs
is_valid_test() ->
    Realm = <<"org.example.mesh">>,
    NodeId = macula_id:node_id(),
    Uri = macula_uri:new(Realm, NodeId),

    ?assert(macula_uri:is_valid(Uri)).

%% Test: is_valid rejects invalid URIs
is_valid_invalid_test() ->
    ?assertNot(macula_uri:is_valid(<<"not a uri">>)),
    ?assertNot(macula_uri:is_valid(<<"http://example.com">>)),
    ?assertNot(macula_uri:is_valid(<<"macula://">>)).

%%%===================================================================
%%% Comparison Tests
%%%===================================================================

%% Test: equals compares URIs
equals_test() ->
    Realm = <<"org.example.mesh">>,
    NodeId = macula_id:node_id(),

    Uri1 = macula_uri:new(Realm, NodeId),
    Uri2 = macula_uri:new(Realm, NodeId),
    Uri3 = macula_uri:new(Realm, macula_id:node_id()),

    ?assert(macula_uri:equals(Uri1, Uri2)),
    ?assertNot(macula_uri:equals(Uri1, Uri3)).
