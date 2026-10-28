%% Property tests for the frame codec (macula#151): what a constructor builds survives the wire, a frame's bytes cut
%% short are never read as a whole frame, and bytes no generator aimed at the decoder are answered rather than raised
%% on. The signed constructors are covered by the record properties, since their round-trip is the record round-trip;
%% these are the frames a station builds without a key.
-module(prop_frame).

-include_lib("proper/include/proper.hrl").

-define(MAX_FRAME_BYTES, 16#1000000).

%% A frame any node can build without signing anything.
frame() ->
    oneof([goodbye(), swim_ping(), swim_ack(), swim_ping_req(), swim_suspect(), ping(), pong(),
           liveness_ping(), find_node(), find_value(), subscribe(), unsubscribe(), store()]).

goodbye() ->
    ?LET({Reason, Detail}, {elements([normal, draining, shutdown, going_down, overloaded]),
                            oneof([undefined, text_binary()])},
         macula_frame:goodbye(Reason, Detail)).

swim_ping() ->
    ?LET({Round, Incarnation}, {count(), count()},
         macula_frame:swim_ping(#{round => Round, incarnation => Incarnation})).

swim_ack() ->
    ?LET({Round, Responder, Incarnation}, {count(), id(), count()},
         macula_frame:swim_ack(#{round => Round, responder => Responder, incarnation => Incarnation})).

swim_ping_req() ->
    ?LET({Round, Target}, {count(), id()},
         macula_frame:swim_ping_req(#{round => Round, target => Target})).

swim_suspect() ->
    ?LET({Target, Incarnation, By, Ttl}, {id(), count(), id(), count()},
         macula_frame:swim_suspect(#{target => Target, target_incarnation => Incarnation,
                                     suspected_by => By, ttl => Ttl})).

ping() ->
    ?LET(Nonce, nonce(), macula_frame:ping(#{nonce => Nonce})).

pong() ->
    ?LET(Nonce, nonce(), macula_frame:pong(#{nonce => Nonce})).

liveness_ping() ->
    ?LET(Nonce, nonce(), macula_frame:liveness_ping(#{nonce => Nonce})).

find_node() ->
    ?LET({Key, Origin, Depth}, {id(), id(), count()},
         macula_frame:find_node(#{key => Key, origin => Origin, depth => Depth})).

find_value() ->
    ?LET({Key, Origin}, {id(), id()}, macula_frame:find_value(#{key => Key, origin => Origin})).

subscribe() ->
    ?LET({Topic, Realm, Subscriber}, {topic(), id(), id()},
         macula_frame:subscribe(#{topic => Topic, realm => Realm, subscriber => Subscriber})).

unsubscribe() ->
    ?LET({Topic, Realm, Subscriber}, {topic(), id(), id()},
         macula_frame:unsubscribe(#{topic => Topic, realm => Realm, subscriber => Subscriber})).

store() ->
    ?LET(Record, sized_binary(256), macula_frame:store(#{record => Record})).

%%------------------------------------------------------------------
%% Generators
%%------------------------------------------------------------------

%% A node id, realm id or key: 32 bytes, as every frame field that names one demands.
id() ->
    ?LET(B, binary(32), B).

nonce() ->
    ?LET(B, binary(16), B).

count() ->
    oneof([0, 1, 23, 24, 255, 256, 65535, 65536, choose(0, 16#FFFFFFFF)]).

topic() ->
    ?LET(Segments, list(oneof([choose($a, $z), $-])), list_to_binary(Segments)).

sized_binary(Max) ->
    ?LET(B, binary(), ?LET(N, choose(0, Max), binary_part(B, 0, min(N, byte_size(B))))).

%% Text a frame's own fields accept, which is UTF-8 and none other: a goodbye's detail is refused as invalid_text
%% otherwise, and a property about the codec would be failing on its generator rather than on the codec.
text_binary() ->
    ?LET(Codepoints, ?LET(N, choose(0, 16), vector(N, codepoint())),
         unicode:characters_to_binary(Codepoints)).

codepoint() ->
    oneof([choose(0, 16#7F), choose(16#80, 16#7FF), choose(16#800, 16#D7FF),
           choose(16#E000, 16#FFFF), choose(16#10000, 16#10FFFF)]).

%%------------------------------------------------------------------
%% Properties
%%------------------------------------------------------------------

%% A frame a constructor builds decodes back from its own bytes, to the same frame type and the same bytes: what a peer
%% re-encodes after reading it is what it read.
prop_a_frame_survives_the_wire() ->
    ?FORALL(F, frame(),
            begin
                Bytes = macula_frame:encode(F),
                case macula_frame:decode(Bytes) of
                    {ok, Decoded, <<>>} ->
                        macula_frame:frame_type(Decoded) =:= macula_frame:frame_type(F)
                            andalso macula_frame:encode(Decoded) =:= Bytes;
                    _Other ->
                        false
                end
            end).

%% A frame a constructor builds is a frame the link may cast at a peering connection.
prop_a_built_frame_passes_its_own_check() ->
    ?FORALL(F, frame(), macula_frame:check_frame(F) =:= ok).

%% Cut a frame's bytes anywhere and the decoder says how many more it needs and nothing else. A prefix is never a whole
%% frame and never a refusal: a frame split across two reads is an ordinary segment boundary, and a decoder that refused
%% one would drop the connection on it.
prop_a_cut_short_frame_is_never_a_whole_frame() ->
    ?FORALL(Bytes, ?LET(F, frame(), macula_frame:encode(F)),
            ?FORALL(Cut, choose(0, byte_size(Bytes) - 1),
                    cut_short(macula_frame:decode(binary:part(Bytes, 0, Cut)), Cut, byte_size(Bytes)))).

%% What the decoder must answer, in the two shapes the length prefix has: under the four bytes of the prefix itself it
%% asks for those, and past them for exactly the bytes of the frame that have not arrived.
cut_short({more, Missing}, Cut, Size) ->
    Missing =:= expected_missing(Cut, Size);
cut_short(_NotMore, _Cut, _Size) ->
    false.

expected_missing(Cut, _Size) when Cut < 4 -> 4 - Cut;
expected_missing(Cut, Size) -> Size - Cut.

%% Whatever a peer sends, the decoder answers: a whole frame, a demand for more bytes, or a refusal. It never raises, so
%% a station reading a stranger's bytes cannot be crashed by them.
prop_any_bytes_are_answered() ->
    ?FORALL(B, binary(), answered(macula_frame:decode(B))).

answered({ok, Frame, Rest}) -> is_binary(Rest) andalso macula_frame:frame_type(Frame) =/= undefined;
answered({more, N}) -> is_integer(N) andalso N > 0;
answered({error, Refusal}) -> known_refusal(Refusal).

known_refusal(frame_too_large) -> true;
known_refusal(bad_frame) -> true;
known_refusal(too_many_elements) -> true;
known_refusal({unknown_frame_type, _}) -> true;
known_refusal({invalid_frame, _, _}) -> true;
known_refusal(_Other) -> false.
