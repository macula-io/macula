%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_node module.
%%%
%%% Tests node identity and metadata management.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_node_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Node Creation Tests
%%%===================================================================

new_with_realm_test() ->
    %% GIVEN: A realm
    Realm = <<"test.realm">>,

    %% WHEN: Creating a new node
    Node = macula_node:new(Realm),

    %% THEN: Should have valid structure
    ?assertEqual(Realm, macula_node:get_realm(Node)),
    NodeId = macula_node:get_id(Node),
    ?assertEqual(32, byte_size(NodeId)),  % 256-bit ID
    ?assertEqual(#{}, macula_node:get_metadata(Node)).

new_with_metadata_test() ->
    %% GIVEN: Realm and metadata
    Realm = <<"test.realm">>,
    Metadata = #{name => <<"node1">>, version => <<"1.0">>},

    %% WHEN: Creating a new node with metadata
    Node = macula_node:new(Realm, Metadata),

    %% THEN: Should have the metadata
    ?assertEqual(Metadata, macula_node:get_metadata(Node)).

new_unique_ids_test() ->
    %% GIVEN: Same realm
    Realm = <<"test.realm">>,

    %% WHEN: Creating multiple nodes
    Node1 = macula_node:new(Realm),
    Node2 = macula_node:new(Realm),
    Node3 = macula_node:new(Realm),

    %% THEN: All should have unique IDs
    Id1 = macula_node:get_id(Node1),
    Id2 = macula_node:get_id(Node2),
    Id3 = macula_node:get_id(Node3),
    ?assert(Id1 =/= Id2),
    ?assert(Id1 =/= Id3),
    ?assert(Id2 =/= Id3).

%%%===================================================================
%%% Getter Tests
%%%===================================================================

get_id_test() ->
    Node = macula_node:new(<<"realm">>),
    Id = macula_node:get_id(Node),
    ?assertEqual(32, byte_size(Id)),
    ?assert(is_binary(Id)).

get_realm_test() ->
    Realm = <<"my.app.realm">>,
    Node = macula_node:new(Realm),
    ?assertEqual(Realm, macula_node:get_realm(Node)).

get_metadata_empty_test() ->
    %% Node created without metadata
    Node = macula_node:new(<<"realm">>),
    ?assertEqual(#{}, macula_node:get_metadata(Node)).

get_metadata_no_key_test() ->
    %% Node without metadata key at all
    Node = #{node_id => crypto:strong_rand_bytes(32), realm => <<"realm">>},
    ?assertEqual(#{}, macula_node:get_metadata(Node)).

get_address_undefined_test() ->
    %% Node without address
    Node = macula_node:new(<<"realm">>),
    ?assertEqual(undefined, macula_node:get_address(Node)).

get_address_set_test() ->
    %% Node with address
    Node0 = macula_node:new(<<"realm">>),
    Address = {{192,168,1,100}, 9443},
    Node1 = macula_node:set_address(Node0, Address),
    ?assertEqual(Address, macula_node:get_address(Node1)).

%%%===================================================================
%%% Setter Tests
%%%===================================================================

set_metadata_test() ->
    %% GIVEN: Node with metadata
    Node0 = macula_node:new(<<"realm">>, #{old => <<"value">>}),

    %% WHEN: Setting new metadata
    NewMetadata = #{new => <<"data">>},
    Node1 = macula_node:set_metadata(Node0, NewMetadata),

    %% THEN: Old metadata should be replaced
    ?assertEqual(NewMetadata, macula_node:get_metadata(Node1)).

update_metadata_test() ->
    %% GIVEN: Node with metadata
    Node0 = macula_node:new(<<"realm">>, #{existing => <<"keep">>}),

    %% WHEN: Updating metadata
    Node1 = macula_node:update_metadata(Node0, #{new => <<"added">>}),

    %% THEN: Should merge metadata
    Expected = #{existing => <<"keep">>, new => <<"added">>},
    ?assertEqual(Expected, macula_node:get_metadata(Node1)).

update_metadata_overwrite_test() ->
    %% GIVEN: Node with metadata
    Node0 = macula_node:new(<<"realm">>, #{key => <<"old">>}),

    %% WHEN: Updating with same key
    Node1 = macula_node:update_metadata(Node0, #{key => <<"new">>}),

    %% THEN: Should overwrite the key
    ?assertEqual(#{key => <<"new">>}, macula_node:get_metadata(Node1)).

set_address_test() ->
    Node0 = macula_node:new(<<"realm">>),
    Address = {{10,0,0,1}, 5000},
    Node1 = macula_node:set_address(Node0, Address),
    ?assertEqual(Address, macula_node:get_address(Node1)).

%%%===================================================================
%%% Serialization Tests
%%%===================================================================

to_from_binary_roundtrip_test() ->
    %% GIVEN: A node
    Node0 = macula_node:new(<<"test.realm">>, #{key => <<"value">>}),
    Node1 = macula_node:set_address(Node0, {{192,168,1,1}, 9443}),

    %% WHEN: Converting to binary and back
    Binary = macula_node:to_binary(Node1),
    Node2 = macula_node:from_binary(Binary),

    %% THEN: Should be equal
    ?assertEqual(macula_node:get_id(Node1), macula_node:get_id(Node2)),
    ?assertEqual(macula_node:get_realm(Node1), macula_node:get_realm(Node2)),
    ?assertEqual(macula_node:get_metadata(Node1), macula_node:get_metadata(Node2)),
    ?assertEqual(macula_node:get_address(Node1), macula_node:get_address(Node2)).

to_binary_returns_binary_test() ->
    Node = macula_node:new(<<"realm">>),
    Binary = macula_node:to_binary(Node),
    ?assert(is_binary(Binary)).

%%%===================================================================
%%% equals Tests
%%%===================================================================

equals_same_node_test() ->
    Node = macula_node:new(<<"realm">>),
    ?assert(macula_node:equals(Node, Node)).

equals_same_id_different_metadata_test() ->
    %% GIVEN: Two nodes with same ID but different metadata
    NodeId = crypto:strong_rand_bytes(32),
    Node1 = #{node_id => NodeId, realm => <<"realm">>, metadata => #{a => 1}},
    Node2 = #{node_id => NodeId, realm => <<"realm">>, metadata => #{b => 2}},

    %% THEN: Should be equal (by ID)
    ?assert(macula_node:equals(Node1, Node2)).

equals_different_ids_test() ->
    Node1 = macula_node:new(<<"realm">>),
    Node2 = macula_node:new(<<"realm">>),
    ?assertNot(macula_node:equals(Node1, Node2)).

equals_different_realms_same_id_test() ->
    %% GIVEN: Same ID, different realms
    NodeId = crypto:strong_rand_bytes(32),
    Node1 = #{node_id => NodeId, realm => <<"realm1">>},
    Node2 = #{node_id => NodeId, realm => <<"realm2">>},

    %% THEN: Should be equal (equality is by ID only)
    ?assert(macula_node:equals(Node1, Node2)).

%%%===================================================================
%%% Edge Cases
%%%===================================================================

empty_realm_test() ->
    %% Empty realm should work
    Node = macula_node:new(<<>>),
    ?assertEqual(<<>>, macula_node:get_realm(Node)).

empty_metadata_test() ->
    Node = macula_node:new(<<"realm">>, #{}),
    ?assertEqual(#{}, macula_node:get_metadata(Node)).

metadata_with_complex_values_test() ->
    %% GIVEN: Complex metadata
    Metadata = #{
        list => [1, 2, 3],
        nested => #{inner => <<"value">>},
        tuple => {a, b, c},
        binary => <<1,2,3,4,5>>
    },

    %% WHEN: Creating node with complex metadata
    Node = macula_node:new(<<"realm">>, Metadata),

    %% THEN: Should preserve complex values
    ?assertEqual(Metadata, macula_node:get_metadata(Node)).

