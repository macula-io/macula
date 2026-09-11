%%%-------------------------------------------------------------------
%%% @doc Tests for how macula_frame:parse_stream/1 reports bytes that do
%%% not decode.
%%%
%%% parse_stream/1 returns `{ok, Frames, Tail}' while every complete frame
%%% decodes, with `Tail' holding at most one incomplete frame. The first
%%% frame that does not decode ends the parse with
%%% `{malformed, FramesBefore, Reason}': a length header above the frame cap
%%% is `frame_too_large' as soon as its four bytes are there, and a complete
%%% frame that is not CBOR is `bad_frame'. Fed chunk by chunk, the tail a
%%% caller keeps never exceeds the frame cap plus the header.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_frame_malformed_tests).

-include_lib("eunit/include/eunit.hrl").

%% The frame cap in macula_frame, 16 MiB.
-define(MAX_FRAME_BYTES, 16#FFFFFF).
-define(HEADER_BYTES, 4).
-define(NOT_CBOR, <<10:32/big, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>).
-define(CHUNK_BYTES, 1_048_576).

parse_stream_test_() ->
    [{"complete frames and a partial one give ok, the frames and the partial tail",
      fun frames_and_partial_tail/0},
     {"a complete frame that is not CBOR is malformed with bad_frame", fun bad_frame/0},
     {"a length header above the cap is malformed with frame_too_large from its four bytes",
      fun oversize_header/0},
     {"the frames before one that does not decode come back with malformed, in order",
      fun frames_before_bad_frame/0},
     {"fed chunk by chunk past a bad frame, the kept tail stays within the cap plus the header",
      {timeout, 60, fun tail_stays_within_cap/0}}].

frames_and_partial_tail() ->
    Partial = binary:part(wire(hello), 0, 6),
    ?assertMatch({ok, [#{frame_type := connect}], Partial},
                 macula_frame:parse_stream(<<(wire(connect))/binary, Partial/binary>>)).

bad_frame() ->
    ?assertEqual({malformed, [], bad_frame}, macula_frame:parse_stream(?NOT_CBOR)).

oversize_header() ->
    ?assertEqual({malformed, [], frame_too_large},
                 macula_frame:parse_stream(<<(?MAX_FRAME_BYTES + 1):32/big>>)).

frames_before_bad_frame() ->
    Stream = <<(wire(connect))/binary, (wire(hello))/binary, ?NOT_CBOR/binary,
               (wire(connect))/binary>>,
    ?assertMatch({malformed, [#{frame_type := connect}, #{frame_type := hello}], bad_frame},
                 macula_frame:parse_stream(Stream)).

tail_stays_within_cap() ->
    Junk = binary:copy(<<0>>, ?MAX_FRAME_BYTES + ?CHUNK_BYTES),
    Stream = <<(wire(connect))/binary, (wire(hello))/binary, ?NOT_CBOR/binary, Junk/binary>>,
    ?assertEqual({malformed, bad_frame, within_cap},
                 feed(chunks(Stream, ?CHUNK_BYTES), <<>>, 0)).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% Parses each chunk after the tail kept from the ones before it, as a
%% connection does, and tracks the largest tail kept.
feed([], _Tail, Largest) ->
    {ended, within(Largest)};
feed([Chunk | Rest], Tail, Largest) ->
    fed(macula_frame:parse_stream(<<Tail/binary, Chunk/binary>>), Rest, Largest).

fed({malformed, _Frames, Reason}, _Rest, Largest) ->
    {malformed, Reason, within(Largest)};
fed(Parsed, Rest, Largest) ->
    Tail = element(tuple_size(Parsed), Parsed),
    feed(Rest, Tail, max(Largest, byte_size(Tail))).

within(Largest) when Largest =< ?MAX_FRAME_BYTES + ?HEADER_BYTES -> within_cap;
within(_Largest) -> over_cap.

chunks(Bin, Size) when byte_size(Bin) =< Size ->
    [Bin];
chunks(Bin, Size) ->
    <<Chunk:Size/binary, Rest/binary>> = Bin,
    [Chunk | chunks(Rest, Size)].

%% A signed frame on the wire.
wire(Type) ->
    Kp = macula_identity:generate(),
    macula_frame:encode(macula_frame:sign(frame(Type, macula_identity:public(Kp)), Kp)).

frame(connect, Pub) ->
    macula_frame:connect(#{
        node_id         => Pub,
        station_id      => Pub,
        realms          => [crypto:strong_rand_bytes(32)],
        capabilities    => 0,
        puzzle_evidence => macula_identity:puzzle_evidence(Pub)
    });
frame(hello, Pub) ->
    macula_frame:hello(#{
        node_id                 => Pub,
        station_id              => Pub,
        realms                  => [crypto:strong_rand_bytes(32)],
        capabilities            => 0,
        accepted                => true,
        negotiated_capabilities => 0
    }).
