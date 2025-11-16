%%%-------------------------------------------------------------------
%%% @doc
%%% Protocol message encoder for Macula mesh.
%%% Encodes message maps to binary wire format.
%%%
%%% Frame Format (8-byte header + payload):
%%%   - Version (1 byte): Protocol version (currently 0x01)
%%%   - Type (1 byte): Message type ID
%%%   - Flags (1 byte): Reserved for future use (0x00)
%%%   - Reserved (1 byte): Must be 0x00
%%%   - Payload Length (4 bytes): Big-endian uint32
%%%   - Payload (N bytes): MessagePack-encoded message data
%%% @end
%%%-------------------------------------------------------------------
-module(macula_protocol_encoder).

-export([encode/2]).

%%%===================================================================
%%% Constants
%%%===================================================================

-define(PROTOCOL_VERSION, 1).
-define(FLAGS_NONE, 0).
-define(RESERVED, 0).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Encode a message to binary format.
%% Returns a binary with 8-byte header + MessagePack payload.
%% @end
-spec encode(macula_protocol_types:message_type(), map()) -> binary().
encode(Type, Msg) when is_atom(Type), is_map(Msg) ->
    %% Validate message has required fields
    validate_message(Type, Msg),

    %% Get message type ID
    TypeId = macula_protocol_types:message_type_id(Type),

    %% Encode payload with MessagePack
    Payload = msgpack:pack(Msg, [{map_format, map}]),

    %% Build frame: 8-byte header + payload
    PayloadLen = byte_size(Payload),
    <<?PROTOCOL_VERSION:8,
      TypeId:8,
      ?FLAGS_NONE:8,
      ?RESERVED:8,
      PayloadLen:32/big-unsigned,
      Payload/binary>>.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Validate message contains required fields for its type.
%% Crashes with badarg if validation fails.
%% @end
-spec validate_message(atom(), map()) -> ok.
validate_message(connect, Msg) ->
    #{version := _, node_id := _, realm_id := _, capabilities := _} = Msg,
    ok;
validate_message(disconnect, Msg) ->
    #{reason := _, message := _} = Msg,
    ok;
validate_message(ping, Msg) ->
    #{timestamp := _} = Msg,
    ok;
validate_message(pong, Msg) ->
    #{timestamp := _, server_time := _} = Msg,
    ok;
validate_message(publish, Msg) ->
    %% Accept both atom and binary keys (from MessagePack decoding)
    case {maps:is_key(topic, Msg), maps:is_key(<<"topic">>, Msg)} of
        {true, _} -> #{topic := _, payload := _, qos := _, retain := _, message_id := _} = Msg;
        {_, true} -> #{<<"topic">> := _, <<"payload">> := _, <<"qos">> := _, <<"retain">> := _, <<"message_id">> := _} = Msg
    end,
    ok;
validate_message(subscribe, Msg) ->
    %% Accept both atom and binary keys
    case {maps:is_key(topics, Msg), maps:is_key(<<"topics">>, Msg)} of
        {true, _} -> #{topics := _, qos := _} = Msg;
        {_, true} -> #{<<"topics">> := _, <<"qos">> := _} = Msg
    end,
    ok;
validate_message(unsubscribe, Msg) ->
    %% Accept both atom and binary keys
    case {maps:is_key(topics, Msg), maps:is_key(<<"topics">>, Msg)} of
        {true, _} -> #{topics := _} = Msg;
        {_, true} -> #{<<"topics">> := _} = Msg
    end,
    ok;
validate_message(rpc_route, Msg) ->
    %% Accept both atom and binary keys
    case {maps:is_key(destination_node_id, Msg), maps:is_key(<<"destination_node_id">>, Msg)} of
        {true, _} -> #{destination_node_id := _, source_node_id := _, hop_count := _,
                       max_hops := _, payload_type := _, payload := _} = Msg;
        {_, true} -> #{<<"destination_node_id">> := _, <<"source_node_id">> := _,
                       <<"hop_count">> := _, <<"max_hops">> := _,
                       <<"payload_type">> := _, <<"payload">> := _} = Msg
    end,
    ok;
validate_message(pubsub_route, Msg) ->
    %% Accept both atom and binary keys
    case {maps:is_key(destination_node_id, Msg), maps:is_key(<<"destination_node_id">>, Msg)} of
        {true, _} -> #{destination_node_id := _, source_node_id := _, hop_count := _,
                       max_hops := _, topic := _, payload := _} = Msg;
        {_, true} -> #{<<"destination_node_id">> := _, <<"source_node_id">> := _,
                       <<"hop_count">> := _, <<"max_hops">> := _,
                       <<"topic">> := _, <<"payload">> := _} = Msg
    end,
    ok;
validate_message(_Type, _Msg) ->
    %% For message types not yet validated, allow anything
    ok.
