%% EUnit tests for macula_peering:send_on_stream/2. A dedicated stream's writer builds, signs and encodes its frames
%% itself, and send_on_stream/2 writes exactly the bytes it is given, with no check, encoding or signing of its own.
-module(macula_peering_send_on_stream_tests).

-include_lib("eunit/include/eunit.hrl").

send_on_stream_test_() ->
    {foreach, fun mocked_quic/0, fun unmocked_quic/1,
     [{"the bytes given are written as they are", fun the_bytes_given_are_written_as_they_are/0},
      {"a failed write returns its error", fun a_failed_write_returns_its_error/0},
      {"a frame map is not taken, and nothing is written", fun a_frame_map_is_not_taken/0}]}.

the_bytes_given_are_written_as_they_are() ->
    Stream = make_ref(),
    Bytes = <<"the bytes of a frame its writer built, signed and encoded">>,
    ?assertEqual(ok, macula_peering:send_on_stream(Stream, Bytes)),
    ?assert(meck:called(macula_quic, send, [Stream, Bytes])),
    ?assertEqual(1, meck:num_calls(macula_quic, send, '_')).

a_failed_write_returns_its_error() ->
    ok = meck:expect(macula_quic, send, fun(_Stream, _Data) -> {error, stream_closed} end),
    ?assertEqual({error, stream_closed}, macula_peering:send_on_stream(make_ref(), <<"bytes">>)).

a_frame_map_is_not_taken() ->
    ?assertError(function_clause, macula_peering:send_on_stream(make_ref(), #{frame_type => ping})),
    ?assertEqual(0, meck:num_calls(macula_quic, send, '_')).

mocked_quic() ->
    try meck:unload(macula_quic) catch _:_ -> ok end,
    ok = meck:new(macula_quic, [passthrough]),
    ok = meck:expect(macula_quic, send, fun(_Stream, _Data) -> ok end).

unmocked_quic(_) ->
    meck:unload(macula_quic).
