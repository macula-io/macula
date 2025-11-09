%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_node module.
%%% Tests written FIRST (TDD red phase).
%%% Node identity and metadata management.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_node_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Node Creation Tests
%%%===================================================================

%% Test: new creates node with random ID
new_test() ->
    Node = macula_node:new(<<"my.realm">>),

    ?assert(is_map(Node)),
    ?assert(is_binary(maps:get(node_id, Node))),
    ?assertEqual(32, byte_size(maps:get(node_id, Node))),  % 256-bit ID
    ?assertEqual(<<"my.realm">>, maps:get(realm, Node)).

%% Test: new with metadata includes custom data
new_with_metadata_test() ->
    Metadata = #{region => <<"us-east">>, datacenter => <<"dc1">>},
    Node = macula_node:new(<<"my.realm">>, Metadata),

    ?assertEqual(Metadata, maps:get(metadata, Node)).

%% Test: two nodes have different IDs
new_unique_ids_test() ->
    Node1 = macula_node:new(<<"my.realm">>),
    Node2 = macula_node:new(<<"my.realm">>),

    Id1 = maps:get(node_id, Node1),
    Id2 = maps:get(node_id, Node2),

    ?assertNotEqual(Id1, Id2).

%%%===================================================================
%%% Node ID Tests
%%%===================================================================

%% Test: get_id returns node ID
get_id_test() ->
    Node = macula_node:new(<<"my.realm">>),
    Id = macula_node:get_id(Node),

    ?assertEqual(maps:get(node_id, Node), Id).

%% Test: get_realm returns realm
get_realm_test() ->
    Node = macula_node:new(<<"my.realm">>),
    Realm = macula_node:get_realm(Node),

    ?assertEqual(<<"my.realm">>, Realm).

%%%===================================================================
%%% Metadata Tests
%%%===================================================================

%% Test: get_metadata returns metadata
get_metadata_test() ->
    Metadata = #{key => <<"value">>},
    Node = macula_node:new(<<"my.realm">>, Metadata),

    ?assertEqual(Metadata, macula_node:get_metadata(Node)).

%% Test: get_metadata returns empty map for node without metadata
get_metadata_empty_test() ->
    Node = macula_node:new(<<"my.realm">>),

    ?assertEqual(#{}, macula_node:get_metadata(Node)).

%% Test: set_metadata updates metadata
set_metadata_test() ->
    Node = macula_node:new(<<"my.realm">>),
    NewMetadata = #{region => <<"eu-west">>},

    UpdatedNode = macula_node:set_metadata(Node, NewMetadata),

    ?assertEqual(NewMetadata, macula_node:get_metadata(UpdatedNode)).

%% Test: update_metadata merges metadata
update_metadata_test() ->
    Node = macula_node:new(<<"my.realm">>, #{key1 => <<"val1">>}),

    UpdatedNode = macula_node:update_metadata(Node, #{key2 => <<"val2">>}),

    Metadata = macula_node:get_metadata(UpdatedNode),
    ?assertEqual(<<"val1">>, maps:get(key1, Metadata)),
    ?assertEqual(<<"val2">>, maps:get(key2, Metadata)).

%%%===================================================================
%%% Address Tests
%%%===================================================================

%% Test: set_address stores address
set_address_test() ->
    Node = macula_node:new(<<"my.realm">>),
    Address = {{127,0,0,1}, 8080},

    UpdatedNode = macula_node:set_address(Node, Address),

    ?assertEqual(Address, macula_node:get_address(UpdatedNode)).

%% Test: get_address returns undefined when not set
get_address_unset_test() ->
    Node = macula_node:new(<<"my.realm">>),

    ?assertEqual(undefined, macula_node:get_address(Node)).

%%%===================================================================
%%% Encoding/Decoding Tests
%%%===================================================================

%% Test: to_binary encodes node
to_binary_test() ->
    Node = macula_node:new(<<"my.realm">>),
    Binary = macula_node:to_binary(Node),

    ?assert(is_binary(Binary)).

%% Test: from_binary decodes node
from_binary_test() ->
    Node = macula_node:new(<<"my.realm">>),
    Binary = macula_node:to_binary(Node),

    {ok, DecodedNode} = macula_node:from_binary(Binary),

    ?assertEqual(maps:get(node_id, Node), maps:get(node_id, DecodedNode)),
    ?assertEqual(maps:get(realm, Node), maps:get(realm, DecodedNode)).

%% Test: from_binary with metadata preserves metadata
from_binary_with_metadata_test() ->
    Metadata = #{key => <<"value">>},
    Node = macula_node:new(<<"my.realm">>, Metadata),
    Binary = macula_node:to_binary(Node),

    {ok, DecodedNode} = macula_node:from_binary(Binary),

    ?assertEqual(Metadata, maps:get(metadata, DecodedNode)).

%% Test: from_binary with invalid data returns error
from_binary_invalid_test() ->
    InvalidBinary = <<"invalid data">>,

    ?assertMatch({error, _}, macula_node:from_binary(InvalidBinary)).

%%%===================================================================
%%% Equality Tests
%%%===================================================================

%% Test: equals compares nodes by ID
equals_test() ->
    Node1 = macula_node:new(<<"my.realm">>),
    Node2 = Node1,  % Same node
    Node3 = macula_node:new(<<"my.realm">>),  % Different node

    ?assert(macula_node:equals(Node1, Node2)),
    ?assertNot(macula_node:equals(Node1, Node3)).
