%%%-------------------------------------------------------------------
%%% @doc
%%% DHT protocol message encoding/decoding.
%%% Maps DHT operations to/from message format.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_protocol).

%% API - Encoding
-export([
    encode_find_node/1,
    encode_find_node_reply/1,
    encode_store/2,
    encode_find_value/1,
    encode_find_value_reply/1,
    encode_node_info/1
]).

%% API - Decoding
-export([
    decode_find_node/1,
    decode_find_node_reply/1,
    decode_store/1,
    decode_find_value/1,
    decode_find_value_reply/1,
    decode_node_info/1
]).

%% API - Message type checks
-export([
    is_find_node/1,
    is_store/1,
    is_find_value/1
]).

%% Types
-type message() :: map().
-export_type([message/0]).

%%%===================================================================
%%% Encoding Functions
%%%===================================================================

%% @doc Encode FIND_NODE request.
-spec encode_find_node(binary()) -> message().
encode_find_node(Target) ->
    #{
        type => find_node,
        target => Target
    }.

%% @doc Encode FIND_NODE reply.
-spec encode_find_node_reply([macula_routing_bucket:node_info()]) -> message().
encode_find_node_reply(Nodes) ->
    #{
        type => find_node_reply,
        nodes => Nodes
    }.

%% @doc Encode STORE request.
-spec encode_store(binary(), term()) -> message().
encode_store(Key, Value) ->
    #{
        type => store,
        key => Key,
        value => Value
    }.

%% @doc Encode FIND_VALUE request.
-spec encode_find_value(binary()) -> message().
encode_find_value(Key) ->
    #{
        type => find_value,
        key => Key
    }.

%% @doc Encode FIND_VALUE reply.
-spec encode_find_value_reply({value, term()} | {nodes, [macula_routing_bucket:node_info()]}) -> message().
encode_find_value_reply({value, Value}) ->
    #{
        type => find_value_reply,
        result => value,
        value => Value
    };
encode_find_value_reply({nodes, Nodes}) ->
    #{
        type => find_value_reply,
        result => nodes,
        nodes => Nodes
    }.

%% @doc Encode node info (for transmission).
-spec encode_node_info(macula_routing_bucket:node_info()) -> map().
encode_node_info(NodeInfo) ->
    #{
        node_id => maps:get(node_id, NodeInfo),
        address => maps:get(address, NodeInfo)
    }.

%%%===================================================================
%%% Decoding Functions
%%%===================================================================

%% @doc Decode FIND_NODE request.
-spec decode_find_node(message()) -> {ok, binary()} | {error, invalid_message}.
decode_find_node(#{type := find_node, target := Target}) ->
    {ok, Target};
decode_find_node(_) ->
    {error, invalid_message}.

%% @doc Decode FIND_NODE reply.
-spec decode_find_node_reply(message()) -> {ok, [macula_routing_bucket:node_info()]} | {error, invalid_message}.
decode_find_node_reply(#{type := find_node_reply, nodes := Nodes}) ->
    {ok, Nodes};
decode_find_node_reply(_) ->
    {error, invalid_message}.

%% @doc Decode STORE request.
-spec decode_store(message()) -> {ok, binary(), term()} | {error, invalid_message}.
decode_store(#{type := store, key := Key, value := Value}) ->
    {ok, Key, Value};
decode_store(_) ->
    {error, invalid_message}.

%% @doc Decode FIND_VALUE request.
-spec decode_find_value(message()) -> {ok, binary()} | {error, invalid_message}.
decode_find_value(#{type := find_value, key := Key}) ->
    {ok, Key};
decode_find_value(_) ->
    {error, invalid_message}.

%% @doc Decode FIND_VALUE reply.
-spec decode_find_value_reply(message()) ->
    {ok, {value, term()} | {nodes, [macula_routing_bucket:node_info()]}} |
    {error, invalid_message}.
decode_find_value_reply(#{type := find_value_reply, result := value, value := Value}) ->
    {ok, {value, Value}};
decode_find_value_reply(#{type := find_value_reply, result := nodes, nodes := Nodes}) ->
    {ok, {nodes, Nodes}};
decode_find_value_reply(_) ->
    {error, invalid_message}.

%% @doc Decode node info.
-spec decode_node_info(map()) -> {ok, macula_routing_bucket:node_info()} | {error, invalid_node_info}.
decode_node_info(#{node_id := NodeId, address := Address}) ->
    {ok, #{
        node_id => NodeId,
        address => Address
    }};
decode_node_info(_) ->
    {error, invalid_node_info}.

%%%===================================================================
%%% Message Type Checks
%%%===================================================================

%% @doc Check if message is FIND_NODE.
-spec is_find_node(message()) -> boolean().
is_find_node(#{type := find_node}) -> true;
is_find_node(_) -> false.

%% @doc Check if message is STORE.
-spec is_store(message()) -> boolean().
is_store(#{type := store}) -> true;
is_store(_) -> false.

%% @doc Check if message is FIND_VALUE.
-spec is_find_value(message()) -> boolean().
is_find_value(#{type := find_value}) -> true;
is_find_value(_) -> false.
