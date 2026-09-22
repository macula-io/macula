%%%-------------------------------------------------------------------
%%% @doc Control protocol encoder/decoder for dist relay.
%%%
%%% Stream 0 carries CBOR control messages. Each frame:
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
%%%
%%% ⚠ A reader refuses rather than carries on, on both counts, because what is at the far end of this channel is a
%%% relay: a forwarder, and not a thing to be trusted with the reader's memory or with where frames begin.
%%%
%%% A LENGTH IS A PROMISE ABOUT BYTES THAT HAVE NOT ARRIVED. Without a cap, a length of 4 GiB is a reader that
%%% waits, holding everything that arrives meanwhile, and grows until the node dies. A control frame here carries
%%% at most a node name.
%%%
%%% A FRAME THAT DOES NOT DECODE MEANS THE TWO ENDS NO LONGER AGREE WHERE FRAMES BEGIN. Skipping it and reading on
%%% takes the middle of something else for a length, so one bad frame becomes an endless run of them while the
%%% channel looks alive. The connection ends instead.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_relay_protocol).

-export([encode/1, decode_buffer/1, max_frame_bytes/0]).

%% The largest control frame a reader accepts, as the handshake's own reader uses on a peering connection.
-define(MAX_FRAME_BYTES, 65_536).

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
    PayloadBin = macula_cbor_nif:pack(encode_map(Msg)),
    Len = byte_size(PayloadBin),
    <<Len:32/big-unsigned, PayloadBin/binary>>.

-spec decode(binary()) -> {ok, control_msg()} | {error, term()}.
decode(PayloadBin) ->
    case macula_cbor_nif:unpack(PayloadBin) of
        {ok, Map} -> decode_map(Map);
        {error, Reason} -> {error, {cbor_decode, Reason}}
    end.

%% @doc Extract zero or more complete frames from a buffer. `{ok, Messages, Remaining}' where Remaining is what is
%% left of a frame that has not all arrived, or `{error, Reason}', which ends the connection: a frame too large to
%% be one of ours, or one whose bytes do not decode to a frame this protocol knows.
-spec decode_buffer(binary()) -> {ok, [control_msg()], binary()} | {error, term()}.
decode_buffer(Buffer) ->
    decode_buffer(Buffer, []).

%% @doc The largest control frame a reader accepts.
-spec max_frame_bytes() -> pos_integer().
max_frame_bytes() ->
    ?MAX_FRAME_BYTES.

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

%% Judged on the header alone, so the bytes a too-large frame promises are never waited for.
decode_buffer(<<Len:32/big-unsigned, _Rest/binary>>, _Acc) when Len > ?MAX_FRAME_BYTES ->
    {error, frame_too_large};
decode_buffer(<<Len:32/big-unsigned, Rest/binary>>, Acc)
  when byte_size(Rest) >= Len ->
    <<PayloadBin:Len/binary, Remaining/binary>> = Rest,
    decoded(decode(PayloadBin), Remaining, Acc);
decode_buffer(Buffer, Acc) ->
    {ok, lists:reverse(Acc), Buffer}.

decoded({ok, Msg}, Remaining, Acc) ->
    decode_buffer(Remaining, [Msg | Acc]);
decoded({error, Reason}, _Remaining, _Acc) ->
    {error, Reason}.
