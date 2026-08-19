%%%-------------------------------------------------------------------
%%% @doc Pure-function tests for the dist relay client.
%%%
%%% The client's main behaviour requires a live QUIC connection to
%%% the dist relay — those are integration tests. Here we cover the
%%% parts that don't need a socket: URL parsing, status, and the
%%% protocol round-trip (which the client uses internally).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_relay_client_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Protocol round-trip (client-side encode/decode)
%%====================================================================

identify_roundtrip_test() ->
    Msg = #{type => identify, node_name => <<"alice@host00.lab">>},
    Encoded = macula_dist_relay_protocol:encode(Msg),
    {[Decoded], <<>>} = macula_dist_relay_protocol:decode_buffer(Encoded),
    ?assertEqual(Msg, Decoded).

tunnel_request_roundtrip_test() ->
    Msg = #{type => tunnel_request, target => <<"bob@host01.lab">>},
    Encoded = macula_dist_relay_protocol:encode(Msg),
    {[Decoded], <<>>} = macula_dist_relay_protocol:decode_buffer(Encoded),
    ?assertEqual(Msg, Decoded).

tunnel_close_roundtrip_test() ->
    Msg = #{type => tunnel_close, tunnel_id => <<"abc123def456abc123def456abc12345">>},
    Encoded = macula_dist_relay_protocol:encode(Msg),
    {[Decoded], <<>>} = macula_dist_relay_protocol:decode_buffer(Encoded),
    ?assertEqual(Msg, Decoded).

multi_frame_buffer_test() ->
    M1 = #{type => identify, node_name => <<"a@h">>},
    M2 = #{type => tunnel_request, target => <<"b@h">>},
    Buf = <<(macula_dist_relay_protocol:encode(M1))/binary,
            (macula_dist_relay_protocol:encode(M2))/binary>>,
    {[D1, D2], <<>>} = macula_dist_relay_protocol:decode_buffer(Buf),
    ?assertEqual(M1, D1),
    ?assertEqual(M2, D2).
