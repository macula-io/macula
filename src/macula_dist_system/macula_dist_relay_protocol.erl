%%%-------------------------------------------------------------------
%%% @doc Control protocol encoder/decoder for dist relay.
%%%
%%% Stream 0 carries MessagePack-framed control messages. Each frame:
%%%
%%%   +----------+---------+
%%%   | Len (4B) | MsgPack |
%%%   +----------+---------+
%%%
%%% Len is big-endian uint32 of the msgpack payload size.
%%%
%%% Message types:
%%%   identify        → identified
%%%   tunnel_request  → tunnel_ok | tunnel_error
%%%   tunnel_close    → (no reply)
%%%   tunnel_notify   → (relay → target, informs of incoming tunnel)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_relay_protocol).

-include_lib("kernel/include/logger.hrl").

-export([encode/1, decode/1, decode_buffer/1]).

-type identify_msg() :: #{type := identify, node_name := binary()}.
-type identified_msg() :: #{type := identified, status := ok}.
-type tunnel_request_msg() :: #{type := tunnel_request, target := binary()}.
-type tunnel_ok_msg() :: #{type := tunnel_ok, tunnel_id := binary()}.
-type tunnel_error_msg() :: #{type := tunnel_error, reason := binary()}.
-type tunnel_close_msg() :: #{type := tunnel_close, tunnel_id := binary()}.
-type tunnel_notify_msg() :: #{type := tunnel_notify, tunnel_id := binary(), source := binary()}.

-type control_msg() ::
    identify_msg() |
    identified_msg() |
    tunnel_request_msg() |
    tunnel_ok_msg() |
    tunnel_error_msg() |
    tunnel_close_msg() |
    tunnel_notify_msg().

-export_type([control_msg/0]).

%%====================================================================
%% API
%%====================================================================

-spec encode(control_msg()) -> binary().
encode(Msg) when is_map(Msg) ->
    Payload = msgpack:pack(encode_map(Msg), [{map_format, map}]),
    PayloadBin = iolist_to_binary(Payload),
    Len = byte_size(PayloadBin),
    <<Len:32/big-unsigned, PayloadBin/binary>>.

-spec decode(binary()) -> {ok, control_msg()} | {error, term()}.
decode(PayloadBin) ->
    case msgpack:unpack(PayloadBin, [{map_format, map}]) of
        {ok, Map} -> decode_map(Map);
        {error, Reason} -> {error, {msgpack_decode, Reason}}
    end.

%% @doc Extract zero or more complete frames from a buffer.
%% Returns {Messages, Remaining} where Remaining is the leftover bytes.
-spec decode_buffer(binary()) -> {[control_msg()], binary()}.
decode_buffer(Buffer) ->
    decode_buffer(Buffer, []).

%%====================================================================
%% Internal — encode
%%====================================================================

encode_map(#{type := identify, node_name := Name}) ->
    #{<<"t">> => <<"id">>, <<"n">> => Name};
encode_map(#{type := identified, status := ok}) ->
    #{<<"t">> => <<"id_ok">>};
encode_map(#{type := tunnel_request, target := Target}) ->
    #{<<"t">> => <<"tun_req">>, <<"target">> => Target};
encode_map(#{type := tunnel_ok, tunnel_id := TId}) ->
    #{<<"t">> => <<"tun_ok">>, <<"tid">> => TId};
encode_map(#{type := tunnel_error, reason := Reason}) ->
    #{<<"t">> => <<"tun_err">>, <<"r">> => Reason};
encode_map(#{type := tunnel_close, tunnel_id := TId}) ->
    #{<<"t">> => <<"tun_close">>, <<"tid">> => TId};
encode_map(#{type := tunnel_notify, tunnel_id := TId, source := Src}) ->
    #{<<"t">> => <<"tun_notify">>, <<"tid">> => TId, <<"src">> => Src}.

%%====================================================================
%% Internal — decode
%%====================================================================

decode_map(#{<<"t">> := <<"id">>, <<"n">> := Name}) ->
    {ok, #{type => identify, node_name => Name}};
decode_map(#{<<"t">> := <<"id_ok">>}) ->
    {ok, #{type => identified, status => ok}};
decode_map(#{<<"t">> := <<"tun_req">>, <<"target">> := Target}) ->
    {ok, #{type => tunnel_request, target => Target}};
decode_map(#{<<"t">> := <<"tun_ok">>, <<"tid">> := TId}) ->
    {ok, #{type => tunnel_ok, tunnel_id => TId}};
decode_map(#{<<"t">> := <<"tun_err">>, <<"r">> := Reason}) ->
    {ok, #{type => tunnel_error, reason => Reason}};
decode_map(#{<<"t">> := <<"tun_close">>, <<"tid">> := TId}) ->
    {ok, #{type => tunnel_close, tunnel_id => TId}};
decode_map(#{<<"t">> := <<"tun_notify">>, <<"tid">> := TId, <<"src">> := Src}) ->
    {ok, #{type => tunnel_notify, tunnel_id => TId, source => Src}};
decode_map(Other) ->
    {error, {unknown_message, Other}}.

%%====================================================================
%% Internal — buffer
%%====================================================================

decode_buffer(<<Len:32/big-unsigned, Rest/binary>>, Acc)
  when byte_size(Rest) >= Len ->
    <<PayloadBin:Len/binary, Remaining/binary>> = Rest,
    case decode(PayloadBin) of
        {ok, Msg} ->
            decode_buffer(Remaining, [Msg | Acc]);
        {error, Reason} ->
            ?LOG_WARNING("[protocol] Skipping malformed frame: ~p", [Reason]),
            decode_buffer(Remaining, Acc)
    end;
decode_buffer(Buffer, Acc) ->
    {lists:reverse(Acc), Buffer}.
