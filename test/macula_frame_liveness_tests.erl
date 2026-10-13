%% EUnit tests for the handshake v5 liveness probe's frames (docs/design/DESIGN_NEIGHBOUR_CHANNEL_BINDING.md section 3,
%% "Liveness on v5"): liveness_ping and liveness_pong carry a 16-byte nonce and nothing else, travel on the control
%% stream, and never carry a neighbour signature.
-module(macula_frame_liveness_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NONCE, <<7:128>>).

liveness_ping_round_trips_test() ->
    Ping = macula_frame:liveness_ping(#{nonce => ?NONCE}),
    ?assertEqual(liveness_ping, macula_frame:frame_type(Ping)),
    ?assertEqual(Ping, wire(Ping)).

liveness_pong_round_trips_test() ->
    Pong = macula_frame:liveness_pong(#{nonce => ?NONCE}),
    ?assertEqual(liveness_pong, macula_frame:frame_type(Pong)),
    ?assertEqual(Pong, wire(Pong)).

a_nonce_of_another_size_is_refused_test() ->
    ?assertError(function_clause, macula_frame:liveness_ping(#{nonce => <<7:120>>})),
    ?assertError(function_clause, macula_frame:liveness_pong(#{nonce => <<7:136>>})),
    Short = (macula_frame:liveness_ping(#{nonce => ?NONCE}))#{nonce => <<7:120>>},
    ?assertEqual({error, {invalid_frame, liveness_ping, nonce}}, macula_frame:decode(macula_frame:encode(Short))).

%% No frame carries a neighbour signature: the field is an extra one, and the frame is refused.
a_liveness_frame_carrying_neighbour_is_refused_test() ->
    <<_Length:32, Bytes/binary>> = macula_frame:encode(macula_frame:liveness_pong(#{nonce => ?NONCE})),
    {ok, Map} = macula_record_cbor:decode_strict(Bytes),
    Signed = Map#{{text, <<"neighbour">>} => #{{text, <<"tbs">>} => <<>>, {text, <<"signature">>} => <<>>}},
    ?assertMatch({error, _}, macula_frame:read_wire(Signed)).

wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.
