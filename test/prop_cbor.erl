%% Property tests for the deterministic CBOR codec (macula#151). The live wire path is the NIF (`macula_cbor_nif`); the
%% pure-Erlang codec (`macula_record_cbor`) is kept as its differentially-tested reference. These hold both to what the
%% codec claims, over values nobody wrote a case for:
%%
%% - a value the decoder hands back is the value that was encoded;
%% - the canonical form does not move when its own bytes are decoded and encoded again;
%% - the two implementations agree byte for byte, which is what makes the reference an oracle;
%% - the strict decoder answers on bytes no generator aimed at it, rather than raising.
%%
%% Text is `{text, Binary}' throughout, because that is what the decoder returns: a bare atom encodes as its name and
%% comes back as text, so it is not part of a round-trip (its own property below holds it to encoding as its name).
-module(prop_cbor).

-include_lib("proper/include/proper.hrl").

-define(MAX_INT, 16#7FFFFFFFFFFFFFFF).
-define(MIN_INT, -16#8000000000000000).

%% The values the wire model claims: integers within 64 bits, floats, byte strings, text strings, arrays, maps and null.
%%
%% Depth and width are explicit rather than taken from PropEr's size parameter, which is what a first draft here did:
%% a resize/2 inside a lazy generator does not bound the subtree, and that draft spent minutes per case and then had
%% the decoder refuse a value past its element budget. Depth is at most four and every array and map holds at most
%% three items, so a case is a few hundred leaves at the very worst.
value() ->
    ?LET(Depth, choose(0, 4), value(Depth)).

value(0) ->
    scalar();
value(Depth) ->
    ?LAZY(oneof([scalar(),
                 ?LET(N, choose(0, 3), vector(N, value(Depth - 1))),
                 ?LET(Pairs, ?LET(N, choose(0, 3), vector(N, {value_key(), value(Depth - 1)})),
                      maps:from_list(Pairs))])).

scalar() ->
    oneof([an_int(), float(), bytes(), text(), null]).

%% The integers a strict decode accepts, with the widths where a CBOR head changes shape, and both ends of the range.
an_int() ->
    oneof([elements([0, 1, 23, 24, 255, 256, 65535, 65536, 16#FFFFFFFF, 16#100000000,
                     ?MAX_INT, -1, -24, -25, -256, -257, -65536, -65537, 16#FFFFFFFF + 1,
                     ?MIN_INT]),
           choose(?MIN_INT, ?MAX_INT)]).

bytes() ->
    ?LET(N, choose(0, 32), binary(N)).

text() ->
    ?LET(B, text_binary(), {text, B}).

%% What a text string holds: every width of UTF-8 encodable character, and never a lone surrogate, which no text
%% string can carry.
text_binary() ->
    ?LET(Codepoints, ?LET(N, choose(0, 12), vector(N, codepoint())),
         unicode:characters_to_binary(Codepoints)).

codepoint() ->
    oneof([choose(0, 16#7F), choose(16#80, 16#7FF), choose(16#800, 16#D7FF),
           choose(16#E000, 16#FFFF), choose(16#10000, 16#10FFFF)]).

%% A map key is a text string or an integer: the only two the strict decoding rule reads, and the only two a canonical
%% map can hold a decoder to.
value_key() ->
    oneof([an_int(), text()]).

%%------------------------------------------------------------------
%% Round-trip
%%------------------------------------------------------------------

%% What was encoded is what comes back, through the pure-Erlang reference.
prop_the_decoder_returns_what_was_encoded() ->
    ?FORALL(V, value(), macula_record_cbor:decode(macula_record_cbor:encode(V)) =:= V).

%% The same through the NIF, which is the codec the wire actually runs.
prop_the_live_codec_returns_what_was_encoded() ->
    ?FORALL(V, value(),
            macula_cbor_nif:unpack_deterministic(macula_cbor_nif:pack_deterministic(V)) =:= V).

%% The canonical form is a fixed point: bytes that came out of the encoder survive a decode and an encode unchanged. A
%% peer that re-encodes what it read produces the same bytes it signs or hashes.
prop_the_canonical_form_is_a_fixed_point() ->
    ?FORALL(V, value(),
            begin
                Encoding = macula_cbor_nif:pack_deterministic(V),
                Encoding =:= macula_cbor_nif:pack_deterministic(macula_cbor_nif:unpack_deterministic(Encoding))
            end).

%% A bare atom encodes as its UTF-8 name: the reason the frame decoder's atomization of payload keys re-encodes to the
%% bytes it read.
prop_an_atom_is_encoded_as_its_name() ->
    ?FORALL(A, atom(),
            macula_cbor_nif:pack_deterministic(A) =:= macula_cbor_nif:pack_deterministic({text, atom_to_binary(A, utf8)})).

%%------------------------------------------------------------------
%% The two codecs against each other
%%------------------------------------------------------------------

%% The NIF and the reference produce the same bytes for every value either claims. A divergence here is a live
%% wire-compatibility break, not a test to shrug off.
prop_the_two_codecs_encode_the_same_bytes() ->
    ?FORALL(V, value(), macula_cbor_nif:pack_deterministic(V) =:= macula_record_cbor:encode(V)).

%% And read the same value back.
prop_the_two_codecs_decode_to_the_same_value() ->
    ?FORALL(V, value(),
            macula_cbor_nif:unpack_deterministic(macula_cbor_nif:pack_deterministic(V))
                =:= macula_record_cbor:decode(macula_record_cbor:encode(V))).

%%------------------------------------------------------------------
%% The strict decoder
%%------------------------------------------------------------------

%% Its own canonical bytes are what a strict reader accepts, as what was encoded.
prop_the_strict_decoder_reads_the_canonical_bytes() ->
    ?FORALL(V, value(),
            macula_record_cbor:decode_strict(macula_cbor_nif:pack_deterministic(V)) =:= {ok, V}).

%% Whatever arrives, the strict decoder answers: a value or one of its refusals, never an exception escaping into the
%% process that read a peer's bytes.
prop_the_strict_decoder_answers_on_any_bytes() ->
    ?FORALL(B, binary(), strict_answer(macula_record_cbor:decode_strict(B))).

strict_answer({ok, _Value}) -> true;
strict_answer({error, Refusal}) -> lists:member(Refusal, refusals());
strict_answer(_Other) -> false.

refusals() ->
    [trailing_bytes, bad_key, duplicate_key, invalid_text, too_deep, integer_out_of_range,
     too_many_elements, malformed].
