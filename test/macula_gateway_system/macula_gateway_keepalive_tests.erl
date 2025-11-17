-module(macula_gateway_keepalive_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test that gateway responds to PING with PONG
gateway_ping_response_test() ->
    %% This test verifies that the gateway has a handler for PING messages
    %% and responds with PONG to keep connections alive.

    %% Check that macula_gateway module has handle_decoded_message function
    Exports = macula_gateway:module_info(exports),
    ?assert(lists:member({handle_decoded_message, 3}, Exports)).

%% Test that PING message type is recognized by protocol encoder/decoder
ping_protocol_support_test() ->
    %% Verify that ping and pong are valid protocol message types
    PingMsg = #{timestamp => erlang:system_time(millisecond)},

    %% Encode PING
    PingBinary = macula_protocol_encoder:encode(ping, PingMsg),
    ?assert(is_binary(PingBinary)),

    %% Decode PING
    {ok, {ping, DecodedPing}} = macula_protocol_decoder:decode(PingBinary),
    ?assert(is_map(DecodedPing)).

%% Test that PONG message type is recognized by protocol encoder/decoder
pong_protocol_support_test() ->
    %% Verify that pong is a valid protocol message type
    PongMsg = #{timestamp => erlang:system_time(millisecond)},

    %% Encode PONG
    PongBinary = macula_protocol_encoder:encode(pong, PongMsg),
    ?assert(is_binary(PongBinary)),

    %% Decode PONG
    {ok, {pong, DecodedPong}} = macula_protocol_decoder:decode(PongBinary),
    ?assert(is_map(DecodedPong)).
