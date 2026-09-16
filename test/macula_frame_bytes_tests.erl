%% EUnit tests for the frame bytes of the post-quantum handshake: macula_frame:parse_stream_bytes/1 hands back each
%% frame's CBOR bytes exactly as received, without the length prefix, and macula_frame:encode_bytes/1 adds the prefix.
%% The connection proof hashes the challenge bytes as received, so the handshake never re-encodes a frame.
-module(macula_frame_bytes_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MAX_FRAME_BYTES, 16#FFFFFF).

frames_come_back_as_the_bytes_sent_test() ->
    Opener = macula_record_cbor:encode(#{{text, <<"version">>} => 3,
                                         {text, <<"frame_type">>} => {text, <<"opener">>}}),
    Other = <<"bytes the drain never decodes">>,
    Buffer = <<(macula_frame:encode_bytes(Opener))/binary, (macula_frame:encode_bytes(Other))/binary>>,
    ?assertEqual({ok, [Opener, Other], <<>>}, macula_frame:parse_stream_bytes(Buffer)).

encode_bytes_prefixes_the_big_endian_length_test() ->
    ?assertEqual(<<0, 0, 0, 3, "abc">>, macula_frame:encode_bytes(<<"abc">>)).

incomplete_frame_stays_in_the_rest_test() ->
    Whole = macula_frame:encode_bytes(<<"whole">>),
    Partial = binary:part(macula_frame:encode_bytes(<<"partial frame">>), 0, 9),
    ?assertEqual({ok, [<<"whole">>], Partial}, macula_frame:parse_stream_bytes(<<Whole/binary, Partial/binary>>)).

short_length_prefix_stays_in_the_rest_test() ->
    ?assertEqual({ok, [], <<0, 0>>}, macula_frame:parse_stream_bytes(<<0, 0>>)).

empty_buffer_drains_nothing_test() ->
    ?assertEqual({ok, [], <<>>}, macula_frame:parse_stream_bytes(<<>>)).

empty_body_is_a_frame_test() ->
    ?assertEqual({ok, [<<>>], <<>>}, macula_frame:parse_stream_bytes(<<0, 0, 0, 0>>)).

length_over_the_cap_is_refused_test() ->
    Whole = macula_frame:encode_bytes(<<"whole">>),
    ?assertEqual({error, frame_too_large},
                 macula_frame:parse_stream_bytes(<<Whole/binary, (?MAX_FRAME_BYTES + 1):32/big>>)).

encode_bytes_refuses_bytes_over_the_cap_test() ->
    ?assertError({frame_too_large, _}, macula_frame:encode_bytes(binary:copy(<<0>>, ?MAX_FRAME_BYTES + 1))).
