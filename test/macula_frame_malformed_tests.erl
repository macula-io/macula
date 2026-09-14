%%%-------------------------------------------------------------------
%%% @doc Tests for how macula_frame reports bytes from a peer that do not
%%% decode, through parse_received/1 and the deprecated parse_stream/1.
%%%
%%% parse_received/1 returns `{ok, Items, Tail}' while every complete frame
%%% decodes, with `Tail' holding at most one incomplete frame. The first
%%% frame that does not decode ends the parse with
%%% `{malformed, ItemsBefore, Reason}': a length header above the frame cap
%%% is `frame_too_large' as soon as its four bytes are there, and a complete
%%% frame that is not CBOR is `bad_frame'. Fed chunk by chunk, the tail a
%%% caller keeps never exceeds the frame cap plus the header.
%%%
%%% parse_stream/1 keeps the `{Frames, Tail}' shape of 10.x. It returns only
%%% frames that pass validate_received/1, and the first frame that does not
%%% decode ends the parse with the frames before it and an empty tail, so a
%%% caller that keeps its tail stays within the cap as well.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_frame_malformed_tests).

-include_lib("eunit/include/eunit.hrl").

%% The frame cap in macula_frame, 16 MiB.
-define(MAX_FRAME_BYTES, 16#FFFFFF).
-define(HEADER_BYTES, 4).
-define(NOT_CBOR, <<10:32/big, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>).
-define(CHUNK_BYTES, 1_048_576).

parse_received_test_() ->
    [{"complete frames and a partial one give ok, the frames and the partial tail",
      fun frames_and_partial_tail/0},
     {"a complete frame that is not CBOR is malformed with bad_frame", fun bad_frame/0},
     {"a length header above the cap is malformed with frame_too_large from its four bytes",
      fun oversize_header/0},
     {"the frames before one that does not decode come back with malformed, in order",
      fun frames_before_bad_frame/0},
     {"fed chunk by chunk past a bad frame, the kept tail stays within the cap plus the header",
      {timeout, 60, fun tail_stays_within_cap/0}}].

parse_stream_test_() ->
    [{"complete frames and a partial one give the frames and the partial tail",
      fun stream_frames_and_partial_tail/0},
     {"a frame that is not CBOR ends the parse with the frames before it and an empty tail",
      fun stream_bad_frame_ends_parse/0},
     {"a length header above the cap ends the parse with an empty tail from its four bytes",
      fun stream_oversize_header/0},
     {"a frame whose fields are refused is dropped and the frames after it come back",
      fun stream_invalid_frame_dropped/0},
     {"fed chunk by chunk past a bad frame, the kept tail stays within the cap plus the header",
      {timeout, 60, fun stream_tail_stays_within_cap/0}}].

frames_and_partial_tail() ->
    Partial = binary:part(wire(hello), 0, 6),
    ?assertMatch({ok, [#{frame_type := connect}], Partial},
                 macula_frame:parse_received(<<(wire(connect))/binary, Partial/binary>>)).

bad_frame() ->
    ?assertEqual({malformed, [], bad_frame}, macula_frame:parse_received(?NOT_CBOR)).

oversize_header() ->
    ?assertEqual({malformed, [], frame_too_large},
                 macula_frame:parse_received(<<(?MAX_FRAME_BYTES + 1):32/big>>)).

frames_before_bad_frame() ->
    ?assertMatch({malformed, [#{frame_type := connect}, #{frame_type := hello}], bad_frame},
                 macula_frame:parse_received(stream_with_bad_frame())).

tail_stays_within_cap() ->
    ?assertEqual({malformed, bad_frame, within_cap},
                 feed(fun macula_frame:parse_received/1, junk_chunks(), <<>>, 0)).

stream_frames_and_partial_tail() ->
    Partial = binary:part(wire(hello), 0, 6),
    ?assertMatch({[#{frame_type := connect}], Partial},
                 macula_frame:parse_stream(<<(wire(connect))/binary, Partial/binary>>)).

stream_bad_frame_ends_parse() ->
    ?assertMatch({[#{frame_type := connect}, #{frame_type := hello}], <<>>},
                 macula_frame:parse_stream(stream_with_bad_frame())).

stream_oversize_header() ->
    ?assertEqual({[], <<>>}, macula_frame:parse_stream(<<(?MAX_FRAME_BYTES + 1):32/big>>)).

stream_invalid_frame_dropped() ->
    Stream = <<(wire(connect))/binary, (wire(connect_without_puzzle_evidence))/binary,
               (wire(hello))/binary>>,
    ?assertMatch({[#{frame_type := connect}, #{frame_type := hello}], <<>>},
                 macula_frame:parse_stream(Stream)).

stream_tail_stays_within_cap() ->
    ?assertEqual({ended, within_cap},
                 feed(fun macula_frame:parse_stream/1, junk_chunks(), <<>>, 0)).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% Two frames, one that is not CBOR, then a frame that would decode.
stream_with_bad_frame() ->
    <<(wire(connect))/binary, (wire(hello))/binary, ?NOT_CBOR/binary,
      (wire(connect))/binary>>.

%% Two frames, one that is not CBOR, then zero bytes past the frame cap,
%% cut into chunks.
junk_chunks() ->
    Junk = binary:copy(<<0>>, ?MAX_FRAME_BYTES + ?CHUNK_BYTES),
    Stream = <<(wire(connect))/binary, (wire(hello))/binary, ?NOT_CBOR/binary, Junk/binary>>,
    chunks(Stream, ?CHUNK_BYTES).

%% Parses each chunk after the tail kept from the ones before it, as a
%% connection does, and tracks the largest tail kept.
feed(_Parse, [], _Tail, Largest) ->
    {ended, within(Largest)};
feed(Parse, [Chunk | Rest], Tail, Largest) ->
    fed(Parse(<<Tail/binary, Chunk/binary>>), Parse, Rest, Largest).

fed({malformed, _Items, Reason}, _Parse, _Rest, Largest) ->
    {malformed, Reason, within(Largest)};
fed(Parsed, Parse, Rest, Largest) ->
    Tail = element(tuple_size(Parsed), Parsed),
    feed(Parse, Rest, Tail, max(Largest, byte_size(Tail))).

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
frame(connect_without_puzzle_evidence, Pub) ->
    maps:remove(puzzle_evidence, frame(connect, Pub));
frame(hello, Pub) ->
    macula_frame:hello(#{
        node_id                 => Pub,
        station_id              => Pub,
        realms                  => [crypto:strong_rand_bytes(32)],
        capabilities            => 0,
        accepted                => true,
        negotiated_capabilities => 0
    }).
