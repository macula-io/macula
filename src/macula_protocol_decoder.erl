%%%-------------------------------------------------------------------
%%% @doc
%%% Protocol message decoder for Macula mesh.
%%% Decodes binary wire format to message maps.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_protocol_decoder).

-export([decode/1]).

%%%===================================================================
%%% Constants
%%%===================================================================

-define(PROTOCOL_VERSION, 1).
-define(HEADER_SIZE, 8).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Decode a binary message to {Type, Msg} tuple.
%% Returns {ok, {Type, Msg}} on success or {error, Reason} on failure.
%% @end
-spec decode(binary()) -> {ok, {atom(), map()}} | {error, term()}.
decode(Binary) when byte_size(Binary) < ?HEADER_SIZE ->
    {error, invalid_header};

decode(<<?PROTOCOL_VERSION:8, TypeId:8, _Flags:8, _Reserved:8,
         PayloadLen:32/big-unsigned, Rest/binary>>) ->
    decode_with_type(TypeId, PayloadLen, Rest);
decode(<<Version:8, _TypeId:8, _Flags:8, _Reserved:8,
         _PayloadLen:32/big-unsigned, _Rest/binary>>) ->
    {error, {unsupported_version, Version}}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Decode payload with known type ID.
-spec decode_with_type(byte(), non_neg_integer(), binary()) ->
    {ok, {atom(), map()}} | {error, term()}.
decode_with_type(_TypeId, PayloadLen, Payload) when byte_size(Payload) < PayloadLen ->
    {error, incomplete_payload};
decode_with_type(TypeId, PayloadLen, Payload) ->
    <<PayloadBytes:PayloadLen/binary, _Rest/binary>> = Payload,
    TypeResult = macula_protocol_types:message_type_name(TypeId),
    do_decode_with_type(TypeResult, TypeId, PayloadBytes).

%% @private Type lookup succeeded
do_decode_with_type({ok, Type}, _TypeId, PayloadBytes) ->
    decode_payload(Type, PayloadBytes);
%% @private Type lookup failed
do_decode_with_type({error, unknown_type}, TypeId, _PayloadBytes) ->
    {error, {unknown_type, TypeId}}.

%% @doc Decode MessagePack payload.
%% Returns {error, Reason} for invalid msgpack data.
-spec decode_payload(atom(), binary()) -> {ok, {atom(), map()}} | {error, term()}.
decode_payload(Type, PayloadBytes) ->
    UnpackResult = msgpack:unpack(PayloadBytes, [{map_format, map}]),
    do_decode_payload(UnpackResult, Type).

%% @private Unpack succeeded
do_decode_payload({ok, Msg}, Type) ->
    {ok, {Type, Msg}};
%% @private Unpack failed
do_decode_payload({error, Reason}, _Type) ->
    {error, {msgpack_decode_error, Reason}}.
