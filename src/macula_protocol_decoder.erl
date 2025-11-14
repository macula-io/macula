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

decode(<<Version:8, TypeId:8, _Flags:8, _Reserved:8,
         PayloadLen:32/big-unsigned, Rest/binary>>) ->
    %% Validate version
    case Version of
        ?PROTOCOL_VERSION ->
            decode_with_type(TypeId, PayloadLen, Rest);
        Other ->
            {error, {unsupported_version, Other}}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Decode payload with known type ID.
-spec decode_with_type(byte(), non_neg_integer(), binary()) ->
    {ok, {atom(), map()}} | {error, term()}.
decode_with_type(TypeId, PayloadLen, Payload) ->
    %% Validate we have enough payload
    case byte_size(Payload) of
        ActualLen when ActualLen < PayloadLen ->
            {error, incomplete_payload};
        _ ->
            %% Extract exact payload
            <<PayloadBytes:PayloadLen/binary, _Rest/binary>> = Payload,

            %% Get message type name
            case macula_protocol_types:message_type_name(TypeId) of
                {ok, Type} ->
                    decode_payload(Type, PayloadBytes);
                {error, unknown_type} ->
                    {error, {unknown_type, TypeId}}
            end
    end.

%% @doc Decode MessagePack payload.
%% Returns {error, Reason} for invalid msgpack data.
-spec decode_payload(atom(), binary()) -> {ok, {atom(), map()}} | {error, term()}.
decode_payload(Type, PayloadBytes) ->
    case msgpack:unpack(PayloadBytes, [{map_format, map}]) of
        {ok, Msg} ->
            {ok, {Type, Msg}};
        {error, Reason} ->
            {error, {msgpack_decode_error, Reason}}
    end.
