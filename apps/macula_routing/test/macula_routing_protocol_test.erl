%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_routing_protocol module.
%%% Tests written FIRST (TDD red phase).
%%% Integration with macula_protocol for DHT message encoding/decoding.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_protocol_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% FIND_NODE Message Tests
%%%===================================================================

%% Test: encode_find_node creates valid message
encode_find_node_test() ->
    Target = <<100:256>>,
    Message = macula_routing_protocol:encode_find_node(Target),

    ?assertMatch(#{type := find_node, target := Target}, Message).

%% Test: decode_find_node extracts target
decode_find_node_test() ->
    Target = <<100:256>>,
    Message = #{type => find_node, target => Target},

    {ok, DecodedTarget} = macula_routing_protocol:decode_find_node(Message),
    ?assertEqual(Target, DecodedTarget).

%% Test: decode_find_node rejects invalid message
decode_find_node_invalid_test() ->
    InvalidMessage = #{type => find_value},  % Wrong type
    ?assertEqual({error, invalid_message}, macula_routing_protocol:decode_find_node(InvalidMessage)).

%%%===================================================================
%%% FIND_NODE_REPLY Message Tests
%%%===================================================================

%% Test: encode_find_node_reply creates valid message
encode_find_node_reply_test() ->
    Nodes = [
        #{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}},
        #{node_id => <<2:256>>, address => {{127,0,0,1}, 8081}}
    ],

    Message = macula_routing_protocol:encode_find_node_reply(Nodes),

    ?assertMatch(#{type := find_node_reply, nodes := Nodes}, Message).

%% Test: decode_find_node_reply extracts nodes
decode_find_node_reply_test() ->
    Nodes = [
        #{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}}
    ],
    Message = #{type => find_node_reply, nodes => Nodes},

    {ok, DecodedNodes} = macula_routing_protocol:decode_find_node_reply(Message),
    ?assertEqual(Nodes, DecodedNodes).

%%%===================================================================
%%% STORE Message Tests
%%%===================================================================

%% Test: encode_store creates valid message
encode_store_test() ->
    Key = <<100:256>>,
    Value = <<"test_value">>,

    Message = macula_routing_protocol:encode_store(Key, Value),

    ?assertMatch(#{type := store, key := Key, value := Value}, Message).

%% Test: decode_store extracts key and value
decode_store_test() ->
    Key = <<100:256>>,
    Value = <<"test_value">>,
    Message = #{type => store, key => Key, value => Value},

    {ok, DecodedKey, DecodedValue} = macula_routing_protocol:decode_store(Message),
    ?assertEqual(Key, DecodedKey),
    ?assertEqual(Value, DecodedValue).

%% Test: decode_store rejects invalid message
decode_store_invalid_test() ->
    InvalidMessage = #{type => find_node},
    ?assertEqual({error, invalid_message}, macula_routing_protocol:decode_store(InvalidMessage)).

%%%===================================================================
%%% FIND_VALUE Message Tests
%%%===================================================================

%% Test: encode_find_value creates valid message
encode_find_value_test() ->
    Key = <<100:256>>,
    Message = macula_routing_protocol:encode_find_value(Key),

    ?assertMatch(#{type := find_value, key := Key}, Message).

%% Test: decode_find_value extracts key
decode_find_value_test() ->
    Key = <<100:256>>,
    Message = #{type => find_value, key => Key},

    {ok, DecodedKey} = macula_routing_protocol:decode_find_value(Message),
    ?assertEqual(Key, DecodedKey).

%%%===================================================================
%%% FIND_VALUE_REPLY Message Tests
%%%===================================================================

%% Test: encode_find_value_reply with value
encode_find_value_reply_with_value_test() ->
    Value = <<"found_value">>,
    Message = macula_routing_protocol:encode_find_value_reply({value, Value}),

    ?assertMatch(#{type := find_value_reply, result := value, value := Value}, Message).

%% Test: encode_find_value_reply with nodes
encode_find_value_reply_with_nodes_test() ->
    Nodes = [
        #{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}}
    ],
    Message = macula_routing_protocol:encode_find_value_reply({nodes, Nodes}),

    ?assertMatch(#{type := find_value_reply, result := nodes, nodes := Nodes}, Message).

%% Test: decode_find_value_reply extracts value
decode_find_value_reply_value_test() ->
    Value = <<"found_value">>,
    Message = #{type => find_value_reply, result => value, value => Value},

    {ok, {value, DecodedValue}} = macula_routing_protocol:decode_find_value_reply(Message),
    ?assertEqual(Value, DecodedValue).

%% Test: decode_find_value_reply extracts nodes
decode_find_value_reply_nodes_test() ->
    Nodes = [
        #{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}}
    ],
    Message = #{type => find_value_reply, result => nodes, nodes => Nodes},

    {ok, {nodes, DecodedNodes}} = macula_routing_protocol:decode_find_value_reply(Message),
    ?assertEqual(Nodes, DecodedNodes).

%%%===================================================================
%%% Node Info Serialization Tests
%%%===================================================================

%% Test: encode_node_info serializes node
encode_node_info_test() ->
    NodeInfo = #{
        node_id => <<1:256>>,
        address => {{127,0,0,1}, 8080}
    },

    Encoded = macula_routing_protocol:encode_node_info(NodeInfo),

    ?assertMatch(#{node_id := <<1:256>>, address := {{127,0,0,1}, 8080}}, Encoded).

%% Test: decode_node_info deserializes node
decode_node_info_test() ->
    Encoded = #{
        node_id => <<1:256>>,
        address => {{127,0,0,1}, 8080}
    },

    {ok, NodeInfo} = macula_routing_protocol:decode_node_info(Encoded),

    ?assertEqual(<<1:256>>, maps:get(node_id, NodeInfo)),
    ?assertEqual({{127,0,0,1}, 8080}, maps:get(address, NodeInfo)).

%% Test: decode_node_info handles missing fields
decode_node_info_missing_fields_test() ->
    Invalid = #{node_id => <<1:256>>},  % Missing address
    ?assertEqual({error, invalid_node_info}, macula_routing_protocol:decode_node_info(Invalid)).

%%%===================================================================
%%% Message Type Detection Tests
%%%===================================================================

%% Test: is_find_node returns true for find_node message
is_find_node_test() ->
    Message = #{type => find_node, target => <<1:256>>},
    ?assert(macula_routing_protocol:is_find_node(Message)).

%% Test: is_find_node returns false for other types
is_find_node_false_test() ->
    Message = #{type => store, key => <<1:256>>, value => <<"val">>},
    ?assertNot(macula_routing_protocol:is_find_node(Message)).

%% Test: is_store returns true for store message
is_store_test() ->
    Message = #{type => store, key => <<1:256>>, value => <<"val">>},
    ?assert(macula_routing_protocol:is_store(Message)).

%% Test: is_find_value returns true for find_value message
is_find_value_test() ->
    Message = #{type => find_value, key => <<1:256>>},
    ?assert(macula_routing_protocol:is_find_value(Message)).
