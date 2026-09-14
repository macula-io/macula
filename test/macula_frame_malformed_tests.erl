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
%%% A frame with more CBOR items than the element budget is
%%% `too_many_elements'. A STORE, REPLICATE or VALUE whose record bytes do
%%% not decode as a record, whether they are not CBOR or over the element
%%% budget, is an invalid frame named by its `record' or `records' field:
%%% decode/1 refuses it, and parse_received/1 reads on past it.
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
%% A frame cap below the frame cap, for parse_received/2.
-define(SMALL_CAP, 1024).

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

parse_received_with_cap_test_() ->
    [{"a length header above the given cap is malformed with frame_too_large from its four bytes",
      fun capped_header_above_the_cap/0},
     {"a frame exactly at the given cap decodes",
      fun capped_frame_at_the_cap/0}].

capped_header_above_the_cap() ->
    ?assertEqual({malformed, [], frame_too_large},
                 macula_frame:parse_received(<<(?SMALL_CAP + 1):32/big>>, ?SMALL_CAP)).

capped_frame_at_the_cap() ->
    Wire = wire(connect),
    <<Len:32/big, _/binary>> = Wire,
    ?assertMatch({ok, [#{frame_type := connect}], <<>>},
                 macula_frame:parse_received(Wire, Len)).

%% The element budget of macula_cbor_nif:unpack_deterministic/1.
-define(ELEMENT_BUDGET, 131072).

element_budget_test_() ->
    [{"a frame with more items than the element budget is malformed with too_many_elements",
      fun parse_over_the_element_budget/0},
     {"decode/1 refuses a frame with more items than the element budget as too_many_elements",
      fun decode_over_the_element_budget/0}].

parse_over_the_element_budget() ->
    ?assertEqual({malformed, [], too_many_elements},
                 macula_frame:parse_received(over_budget_frame())).

decode_over_the_element_budget() ->
    ?assertEqual({error, too_many_elements}, macula_frame:decode(over_budget_frame())).

%% A RESULT-shaped frame whose payload is an array of as many zeros as the
%% budget, so the frame holds more items than the budget allows.
over_budget_frame() ->
    Payload = over_budget_array(),
    Body = <<16#A2, 16#67, "payload", Payload/binary, 16#6A, "frame_type", 16#66, "result">>,
    <<(byte_size(Body)):32/big, Body/binary>>.

%% An array of as many zeros as the budget: with the array itself, one item
%% more than the budget allows.
over_budget_array() ->
    <<16#9A, ?ELEMENT_BUDGET:32/big, (binary:copy(<<0>>, ?ELEMENT_BUDGET))/binary>>.

record_field_test_() ->
    [{lists:concat([Type, " whose ", Field, " holds ", Kind]),
      [{"decode/1 refuses it as an invalid frame named by that field",
        ?_assertEqual({error, {invalid_frame, Type, Field}}, macula_frame:decode(Wire))},
       {"parse_received/1 gives it as an invalid frame and reads the frame after it",
        ?_assertMatch({ok, [{invalid_frame, Type, Field}, #{frame_type := connect}], <<>>},
                      macula_frame:parse_received(<<Wire/binary, (wire(connect))/binary>>))}]}
     || {Type, Field, Kind, Wire} <- record_field_frames()].

%% A STORE, a REPLICATE and a VALUE whose record bytes do not decode as a
%% record: bytes that are not CBOR, and an array over the element budget.
record_field_frames() ->
    Record = sample_record(),
    Frames = [{store, record, macula_frame:store(#{record => Record})},
              {replicate, record,
               macula_frame:replicate(#{record => Record, new_custodian => false})},
              {value, records,
               macula_frame:value(#{key => crypto:strong_rand_bytes(32), records => [Record]})}],
    [{Type, Field, Kind, wire_with(Frame, Field, field_value(Field, Bytes))}
     || {Type, Field, Frame} <- Frames,
        {Kind, Bytes} <- [{"bytes that are not CBOR", <<255, 255, 255, 255>>},
                          {"an array over the element budget", over_budget_array()}]].

field_value(records, Bytes) -> [Bytes];
field_value(record, Bytes) -> Bytes.

%% Frame on the wire with Field holding Value, which a builder would not put
%% there: the signed frame is encoded, and its field replaced in the CBOR map.
wire_with(Frame, Field, Value) ->
    Kp = macula_identity:generate(),
    <<_Len:32/big, Body/binary>> = macula_frame:encode(macula_frame:sign(Frame, Kp)),
    Map = macula_cbor_nif:unpack_deterministic(Body),
    Bytes = macula_cbor_nif:pack_deterministic(Map#{{text, atom_to_binary(Field)} := Value}),
    <<(byte_size(Bytes)):32/big, Bytes/binary>>.

sample_record() ->
    Kp = macula_identity:generate(),
    macula_record:sign(macula_record:node_record(macula_identity:public(Kp), [], 0), Kp).

%% A frame and the records in it share one element budget: a record gets
%% what the frame's own items and the records before it left.
shared_budget_test_() ->
    [{"a VALUE of two records of 70,000 items, each under the element budget, is invalid",
      ?_assertEqual({error, {invalid_frame, value, records}},
                    macula_frame:decode(value_wire([record_of_items(70000),
                                                    record_of_items(70000)])))},
     {"a VALUE of two records of 60,000 items decodes",
      ?_assertMatch({ok, #{frame_type := value, records := [#{}, #{}]}, <<>>},
                    macula_frame:decode(value_wire([record_of_items(60000),
                                                    record_of_items(60000)])))},
     {"a STORE whose own items and record together pass the element budget is invalid",
      ?_assertEqual({error, {invalid_frame, store, record}},
                    macula_frame:decode(padded_store_wire(70000, record_of_items(70000))))}].

%% A VALUE for one key holding Records, on the wire.
value_wire(Records) ->
    macula_frame:encode(macula_frame:value(#{key => <<6:256>>, records => Records})).

%% A STORE of Record whose frame also carries a list of Padding zeros, in a
%% field a STORE does not read, on the wire.
padded_store_wire(Padding, Record) ->
    Store = macula_frame:store(#{record => Record}),
    macula_frame:encode(Store#{padding => lists:duplicate(Padding, 0)}).

%% A record of exactly Items CBOR items: its payload holds a list of zeros
%% long enough to make up the count. Its signature is not checked here.
record_of_items(Items) ->
    record_with(lists:duplicate(Items - record_items(record_with([])), 0)).

record_with(List) ->
    #{type => 1, key => <<4:256>>, version => <<5:128>>, created_at => 1,
      expires_at => 2, payload => #{list => List}, signature => <<0:512>>}.

record_items(Record) ->
    count_items(macula_cbor_nif:unpack_deterministic(macula_record:encode(Record))).

%% Every CBOR item in a decoded term: a map or a list counts one, and so
%% does each key, value and element in it.
count_items(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) -> Acc + count_items(K) + count_items(V) end, 1, Map);
count_items(List) when is_list(List) ->
    lists:foldl(fun(E, Acc) -> Acc + count_items(E) end, 1, List);
count_items(_Scalar) ->
    1.

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
