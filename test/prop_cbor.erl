%% Property tests for the deterministic CBOR codec (macula#151). The live wire path is the NIF (`macula_cbor_nif`); the
%% pure-Erlang codec (`macula_record_cbor`) is kept as its differentially-tested reference. These hold both to what the
%% codec claims, over values nobody wrote a case for:
%%
%% - a value the decoder hands back is the value that was encoded;
%% - the canonical form does not move when its own bytes are decoded and encoded again;
%% - a map's entries are written in bytewise order of their own encoded keys, whatever order the map hands them out in;
%% - the two implementations agree byte for byte, which is what makes the reference an oracle;
%% - the strict decoder answers on bytes no generator aimed at it, rather than raising;
%% - a canonical encoding corrupted the way an attacker would corrupt it gets the answer that corruption owes, and the
%%   NIF decoder answers the same input without taking the VM down (macula#154).
%%
%% THE TWO DECODERS ARE NOT HELD TO EACH OTHER, and that is deliberate rather than an oversight (macula#154). The NIF is
%% the fast deterministic decoder: it bounds nesting at 128 and does not check for duplicate map keys, non-UTF-8 text,
%% keys that are neither text nor an integer, or integers past 2^63. The Erlang reference is the strict decoder: bound at
%% 64, and it refuses all four. They are different tools and this module tests each to its own contract.
%%
%% What keeps that from being a hole is that the lenient decoders are held off the wire, and today they are not all off
%% it: every peer's bytes are decoded by `macula_record_cbor:decode_strict/1' except the distribution relay's control
%% channel, which reads with `macula_cbor_nif:unpack/1' (macula_dist_relay_protocol:decode/1). That call is the one
%% recorded exemption in `test/macula_the_lenient_decoder_stays_off_the_wire_tests.erl', and it comes out when
%% macula-io/macula#160 lands. macula#154 asked for a property holding the two to the same refusals, which they do not
%% share; the measurement is on the issue. The guard is what makes any new wiring, and the removal of that one, a red
%% run rather than a quiet regression.
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

%% A map whose entries exercise the sort: the shapes the rest of the module generates, or a larger one past the 32 keys
%% where Erlang stops handing a map out in term order.
map_to_encode() ->
    oneof([?LET(Pairs, ?LET(N, choose(0, 3), vector(N, {value_key(), scalar()})), maps:from_list(Pairs)),
           big_map()]).

%% 80 entries of both kinds, keyed by a drawn base so the integer and text encodings change width from case to case.
big_map() ->
    ?LET(Base, choose(0, 16#FFFFFFFFFFFF),
         maps:from_list([{Key, Key} || Key <- big_keys(Base)])).

big_keys(Base) ->
    [Base + I || I <- lists:seq(0, 39)]
        ++ [{text, <<"k", (integer_to_binary(Base + I))/binary>>} || I <- lists:seq(0, 39)].

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
%% Canonical map order
%%------------------------------------------------------------------

%% A map's entries are written in bytewise order of their own encoded keys (RFC 8949 4.2.1), whatever order the map
%% hands them out in. That is checked against the bytes themselves: each key and value encoded alone, the pairs sorted by
%% their key bytes, and the whole compared with the body of the encoded map.
%%
%% This is the property a case list cannot hold, because Erlang iterates a map of up to 32 keys in term order and a
%% larger one in hash order: a codec that leaned on `maps:to_list/1' would come out right below that line and wrong
%% above it, which is why the larger map here carries 80 keys of both kinds.
prop_a_map_is_written_in_the_byte_order_of_its_keys() ->
    ?FORALL(M, map_to_encode(),
            begin
                Bytes = macula_cbor_nif:pack_deterministic(M),
                {Written, Body} = map_body(Bytes),
                Pairs = [{encoded(K), encoded(V)} || {K, V} <- maps:to_list(M)],
                Expected = iolist_to_binary([<<Key/binary, Value/binary>> || {Key, Value} <- lists:sort(Pairs)]),
                Written =:= map_size(M) andalso Body =:= Expected
            end).

%% The head and the body of an encoded map, apart: 0xA0..0xB7 for up to 23 entries and 0xB8 plus one byte above that,
%% which is the whole range these properties encode.
map_body(<<5:3, 24:5, N:8, Body/binary>>) -> {N, Body};
map_body(<<5:3, N:5, Body/binary>>) -> {N, Body}.

encoded(V) ->
    macula_cbor_nif:pack_deterministic(V).

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

%%------------------------------------------------------------------
%% The strict decoder against corrupted canonical bytes (macula#154)
%%------------------------------------------------------------------

%% A canonical encoding with one thing corrupted, which is what an attacker with a valid frame actually sends. This is a
%% different input class from `binary/0' above and the reason for it: on arbitrary bytes almost every case dies on the
%% first header byte, so that property pins the refusal list and never reaches the code that decides a duplicate key, a
%% wrong-width length, or a nesting bound. A mutation starts from bytes the codec produced, so it is well formed right up
%% to the point it is wrong and the decoder has to walk into it.
%%
%% Each case carries what the decoder owes it. Three mutations have one unambiguous answer, and those are required rather
%% than merely allowed: a duplicated entry is `duplicate_key', a byte after a whole item is `trailing_bytes', a chain
%% past the nesting bound is `too_deep'. Two have a known value: reordered entries and a length written in a wider form
%% than it needs are both accepted, and must decode to the value they encode, because CBOR maps are unordered and a
%% deterministic reader takes the shortest form for writing, not for reading. Truncation is the one whose exact refusal
%% is not fixed, because which refusal a short read produces is the decoder's business, but it is still a refusal: a
%% definite-length item is prefix-free and a canonical encoding never uses an indefinite length, so no proper prefix of
%% one is a whole item - the empty binary included - and a decoded value there would be a decoder reading a tail that
%% has not arrived.
prop_a_corrupted_canonical_encoding_gets_the_answer_its_corruption_owes() ->
    ?FORALL({_Kind, Owed, Bytes}, corrupted_encoding(),
            owed(Owed, macula_record_cbor:decode_strict(Bytes))).

%% The answer this module accepts, and only this: three corruptions owe one NAMED refusal, two owe a NAMED value, and
%% truncation owes a refusal without naming which. An answer that is merely "one of the refusals" is not enough for a
%% corruption whose answer is unambiguous - a decoder that answered `malformed' to a duplicated key would be green
%% under a looser rule, and that is the case the issue says the property exists to reach.
owed({refusal, Refusal}, {error, Refusal}) -> true;
owed({value, Value}, {ok, Value}) -> true;
owed(refused, {error, Refusal}) -> lists:member(Refusal, refusals());
owed(_Expectation, _Answer) -> false.

corrupted_encoding() ->
    oneof([duplicated_entry(), widened_length(), reordered_entries(),
           truncated_value(), appended_byte(), nested_past_the_bound()]).

%% A map with one entry repeated, and a head that counts it. The decoder cannot see the duplicate until it has read the
%% whole map, so this reaches `strict_put/3' rather than stopping at a header.
duplicated_entry() ->
    ?LET(M, nonempty_map(), duplicate_a_pair(M)).

%% The first pair repeated, and the head saying how many entries the map now holds.
duplicate_a_pair(M) ->
    [{K, V} | _] = maps:to_list(M),
    {duplicate_key, {refusal, duplicate_key},
     iolist_to_binary([map_head(map_size(M) + 1), map_body_bytes(M), pair_bytes(K, V)])}.

%% A two-entry map with its two pairs written in the other order.
reordered_entries() ->
    ?LET(M, two_entry_map(), {swapped_keys, {value, M}, swap_two_pairs(M)}).

swap_two_pairs(M) ->
    [A, B] = [pair_bytes(K, V) || {K, V} <- maps:to_list(M)],
    iolist_to_binary([map_head(2), B, A]).

%% A text string that fits in the five-bit length field, written with the one-byte form it does not need.
widened_length() ->
    ?LET(B, short_text(), {non_canonical_width, {value, {text, B}}, widen_text(B)}).

widen_text(B) when byte_size(B) =< 23 ->
    <<3:3, 24:5, (byte_size(B)):8, B/binary>>.

%% The last byte of a whole item, gone. Which refusal this owes is not fixed - that is the decoder's business - but it
%% owes a refusal, never a value.
truncated_value() ->
    ?LET(V, value(), {truncated, refused, drop_last(macula_cbor_nif:pack_deterministic(V))}).

drop_last(B) when byte_size(B) =< 1 -> <<>>;
drop_last(B) -> binary:part(B, 0, byte_size(B) - 1).

%% One byte after a whole item.
appended_byte() ->
    ?LET(V, value(),
         {appended_byte, {refusal, trailing_bytes},
          <<(macula_cbor_nif:pack_deterministic(V))/binary, 0>>}).

%% A chain of one-element arrays past the 64 levels a strict decode takes. Each level is one byte, so the whole input
%% stays tiny: the bound that this reaches is a count of depth, not of bytes.
nested_past_the_bound() ->
    ?LET(Depth, choose(65, 70), {nesting_past_the_bound, {refusal, too_deep}, nesting(Depth)}).

nesting(Depth) ->
    iolist_to_binary([binary:copy(<<4:3, 1:5>>, Depth), <<0>>]).

%% The same corrupted encodings, put to the NIF decoder. It owes LESS than the reference and this property says exactly
%% how much less, rather than pretending the two are held to the same line (they are not; see the module header):
%%
%% - where the reference ACCEPTS the bytes, the NIF must accept them too and return the same value, because a divergence
%%   there is a real disagreement about the data rather than about strictness;
%% - where the reference REFUSES, the NIF may return a value or raise, since it is the lenient decoder and refusing is
%%   not its job. What it may never do is take the VM down: a chain of one-byte array heads has already segfaulted this
%%   NIF once at depth 10,000 (test/macula_cbor_deterministic_diff_tests.erl), and that class of input is precisely what
%%   a mutation produces.
prop_the_nif_answers_a_corrupted_canonical_encoding() ->
    ?FORALL({_Kind, _Owed, Bytes}, corrupted_encoding(),
            agree_or_answer(macula_record_cbor:decode_strict(Bytes), nif_answer(Bytes))).

%% The NIF's answer as a value or as a caught raise. A VM abort cannot be caught, so an input that kills the emulator
%% fails this property by ending the run rather than by returning false.
nif_answer(Bytes) ->
    try macula_cbor_nif:unpack_deterministic(Bytes) of
        Value -> {value, Value}
    catch
        _Class:_Reason -> raised
    end.

agree_or_answer({ok, Value}, {value, NifValue}) -> Value =:= NifValue;
agree_or_answer({ok, _Value}, raised) -> false;
agree_or_answer({error, Refusal}, _NifAnswer) -> lists:member(Refusal, refusals());
agree_or_answer(_Other, _NifAnswer) -> false.

%%------------------------------------------------------------------
%% The pieces the mutations are built from
%%------------------------------------------------------------------

%% At most 23 entries, so the map head is the one-byte form and `map_head/1' below can rebuild it. Keys are consecutive
%% integers from a drawn base, so no two collide and the map really holds N of them.
nonempty_map() ->
    ?LET(Base, choose(0, 16#FFFF),
         ?LET(N, choose(1, 3),
              ?LET(Values, vector(N, scalar()),
                   maps:from_list(lists:zip(lists:seq(Base, Base + N - 1), Values))))).

two_entry_map() ->
    ?LET(Base, choose(0, 16#FFFF),
         ?LET({V1, V2}, {scalar(), scalar()},
              #{Base => V1, Base + 1 => V2})).

%% ASCII, so its byte length is its character count and stays inside the five-bit field.
short_text() ->
    ?LET(N, choose(0, 20),
         ?LET(Chars, vector(N, choose(16#41, 16#5A)), unicode:characters_to_binary(Chars))).

%% The head an encoded map of N entries carries, for the N this module builds.
map_head(N) when N =< 23 -> <<5:3, N:5>>;
map_head(N) -> <<5:3, 24:5, N:8>>.

%% The pairs of an encoded map without its head. One byte, because every map here holds at most 23 entries.
map_body_bytes(M) ->
    <<_:8, Body/binary>> = macula_cbor_nif:pack_deterministic(M),
    Body.

%% One map entry's bytes, without the head of the one-entry map that carries it.
pair_bytes(K, V) ->
    <<_:8, Pair/binary>> = macula_cbor_nif:pack_deterministic(#{K => V}),
    Pair.
