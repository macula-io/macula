%% @doc Deterministic CBOR encoder/decoder.
%%
%% Implements the subset of RFC 8949 needed by Macula records:
%% unsigned ints, negative ints, floats, byte strings, text strings,
%% arrays, maps, and `null'.
%%
%% Encoding follows RFC 8949 §4.2.1 (deterministic):
%% <ul>
%%   <li>Smallest length encoding.</li>
%%   <li>Definite lengths only (no indefinite items).</li>
%%   <li>Map keys sorted by bytewise lexicographic order of their
%%       deterministic encoding.</li>
%% </ul>
%%
%% Internal value representation:
%% <ul>
%%   <li>`non_neg_integer()' — uint (major 0)</li>
%%   <li>`neg_integer()' — negative integer (major 1)</li>
%%   <li>`binary()' — byte string (major 2)</li>
%%   <li>`{text, binary()}' — UTF-8 text string (major 3)</li>
%%   <li>`atom()' — UTF-8 text string (major 3) via
%%       `atom_to_binary/1'. Accepted for round-trip robustness:
%%       the frame decoder atomizes binary keys via
%%       binary_to_existing_atom/1, so a record decoded from
%%       the wire and re-encoded for sig verify carries atom keys
%%       inside the payload sub-map. Encoding atoms as text
%%       reproduces the original wire bytes byte-for-byte (atom
%%       names round-trip exactly through utf8). `null' has a
%%       dedicated clause and is NOT routed here.</li>
%%   <li>`[value()]' — array (major 4)</li>
%%   <li>`#{value() => value()}' — map (major 5)</li>
%%   <li>`null' — simple null (major 7, value 22)</li>
%%   <li>`float()' — IEEE 754 binary64 (major 7, value 27)</li>
%% </ul>
-module(macula_record_cbor).

-export([encode/1, decode/1, decode_strict/1, decode_strict/2, is_encodable_int/1]).
-export_type([value/0, strict_refusal/0]).

-type value() ::
    integer()
  | float()
  | binary()
  | {text, binary()}
  | [value()]
  | #{value() => value()}
  | null
  | atom().

-type strict_refusal() ::
    trailing_bytes | bad_key | duplicate_key | invalid_text | too_deep | integer_out_of_range
  | too_many_elements | malformed.

-define(MAX_UINT64, 16#FFFFFFFFFFFFFFFF).

%%------------------------------------------------------------------
%% Encode
%%------------------------------------------------------------------

%% @doc Can this integer be rendered as major 0 / major 1?
%%
%% Exported so callers that must decide admissibility BEFORE encoding
%% (see `macula_frame:check_payload/1') can ask rather than restate the
%% bound. A bignum past 64 bits matches no `encode/1' clause and would
%% otherwise crash whichever process happens to be encoding.
-spec is_encodable_int(integer()) -> boolean().
is_encodable_int(N) when is_integer(N), N >= 0, N =< ?MAX_UINT64 ->
    true;
is_encodable_int(N) when is_integer(N), N < 0, N >= -(?MAX_UINT64 + 1) ->
    true;
is_encodable_int(N) when is_integer(N) ->
    false.

-spec encode(value()) -> binary().
encode(N) when is_integer(N), N >= 0, N =< ?MAX_UINT64 ->
    head(0, N);
%% Negative integers — CBOR major type 1. The encoded count is `-1 - N'
%% (so -1 -> head(1, 0), -11 -> head(1, 10)). Bounded the same as the
%% positive branch but mirrored on the negative side.
encode(N) when is_integer(N), N < 0, N >= -(?MAX_UINT64 + 1) ->
    head(1, -1 - N);
%% Floats: major 7, additional info 27, IEEE 754 binary64 (§3.3).
%%
%% ALWAYS binary64, never the shorter half or single forms. Determinism
%% requires one canonical encoding per value, not the shortest one, and
%% picking "shortest that round-trips" would make the signed bytes depend on
%% a width-selection rule that every peer must reproduce bit-for-bit. Nine
%% bytes per float is the price of not having that argument.
%%
%% An Erlang float is always finite (arithmetic raises badarith rather than
%% producing NaN or infinity), so there is no NaN canonicalisation question
%% on the encode side.
encode(F) when is_float(F) ->
    <<7:3, 27:5, F:64/float>>;
encode({text, B}) when is_binary(B) ->
    <<(head(3, byte_size(B)))/binary, B/binary>>;
encode(B) when is_binary(B) ->
    <<(head(2, byte_size(B)))/binary, B/binary>>;
encode(L) when is_list(L) ->
    encode_array(L);
encode(M) when is_map(M) ->
    encode_map(M);
encode(null) ->
    <<16#F6>>;
%% Atoms encode as their UTF-8 name as a major-3 text string. This
%% makes the codec self-healing across the wire round-trip: the frame
%% decoder atomizes binary keys via binary_to_existing_atom/1, and
%% records re-encoded for signature verify hit those atoms here. By
%% the symmetry of atom_to_binary/1 / binary_to_existing_atom/1,
%% the resulting wire bytes match the original record exactly.
encode(A) when is_atom(A) ->
    Bin = atom_to_binary(A, utf8),
    <<(head(3, byte_size(Bin)))/binary, Bin/binary>>.

encode_array(L) ->
    Body = << <<(encode(E))/binary>> || E <- L >>,
    <<(head(4, length(L)))/binary, Body/binary>>.

encode_map(M) ->
    %% Encode each k/v independently, then sort by encoded key bytes
    %% (Erlang binary comparison is bytewise — exactly what the spec wants).
    Pairs = [ {encode(K), encode(V)} || {K, V} <- maps:to_list(M) ],
    Sorted = lists:sort(Pairs),
    Body = << <<K/binary, V/binary>> || {K, V} <- Sorted >>,
    <<(head(5, maps:size(M)))/binary, Body/binary>>.

%% Type byte + length prefix using the smallest encoding.
head(MT, N) when N =< 23 ->
    <<MT:3, N:5>>;
head(MT, N) when N =< 16#FF ->
    <<MT:3, 24:5, N:8>>;
head(MT, N) when N =< 16#FFFF ->
    <<MT:3, 25:5, N:16>>;
head(MT, N) when N =< 16#FFFFFFFF ->
    <<MT:3, 26:5, N:32>>;
head(MT, N) when N =< ?MAX_UINT64 ->
    <<MT:3, 27:5, N:64>>.

%%------------------------------------------------------------------
%% Decode
%%------------------------------------------------------------------

-spec decode(binary()) -> value().
decode(Bin) when is_binary(Bin) ->
    {V, <<>>, unbounded} = decode_one(Bin, lenient, unbounded),
    V.

%% @doc Decode one item under the post-quantum decoding rule of DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md, without
%% raising. It refuses bytes after the top-level item, a map key that is not text or an integer, a duplicate key
%% (equal after decoding, so a text key in two length widths is one key), text that is not valid UTF-8, nesting
%% deeper than 64 levels, a negative integer below -2^63, and malformed input such as an indefinite length, a tag
%% or a simple value other than null. It also refuses more CBOR items than the element budget of
%% `macula_cbor_nif:element_budget/0', 131,072, as `too_many_elements': every item counts once, map keys and array
%% elements included. `decode/1' keeps its behaviour: a duplicate key there still keeps the last value.
-spec decode_strict(binary()) -> {ok, value()} | {error, strict_refusal()}.
decode_strict(Bin) when is_binary(Bin) ->
    without_left(decode_strict(Bin, macula_cbor_nif:element_budget())).

without_left({ok, V, _Left}) -> {ok, V};
without_left({error, _Refusal} = Refused) -> Refused.

%% @doc `decode_strict/1' within `Budget' CBOR items, what a caller has left of an element budget, returning what is
%% left after the item as `{ok, Value, Left}'.
-spec decode_strict(binary(), non_neg_integer()) -> {ok, value(), non_neg_integer()} | {error, strict_refusal()}.
decode_strict(Bin, Budget) when is_binary(Bin), is_integer(Budget), Budget >= 0 ->
    try decode_one(Bin, {strict, 0}, Budget) of
        {V, <<>>, Left}        -> {ok, V, Left};
        {_V, _Trailing, _Left} -> {error, trailing_bytes}
    catch
        throw:Refusal when Refusal =:= bad_key; Refusal =:= duplicate_key; Refusal =:= invalid_text;
                           Refusal =:= too_deep; Refusal =:= integer_out_of_range;
                           Refusal =:= too_many_elements ->
            {error, Refusal};
        error:_ ->
            {error, malformed}
    end.

%% Major 7, value 22 = null.
decode_one(<<7:3, 22:5, R/binary>>, _Mode, Left) ->
    {null, R, counted(Left)};
%% Floats. We only ever EMIT binary64, but a conforming peer may send the
%% shorter forms, so all three are accepted. NaN and the infinities have no
%% Erlang float representation and match no clause here, so a strict decode
%% refuses them as `malformed', as the decoding rule requires: this codec
%% cannot faithfully hand such a value to a caller.
decode_one(<<7:3, 25:5, Half:16/bitstring, R/binary>>, _Mode, Left) ->
    {half_to_float(Half), R, counted(Left)};
decode_one(<<7:3, 26:5, F:32/float, R/binary>>, _Mode, Left) ->
    {F, R, counted(Left)};
decode_one(<<7:3, 27:5, F:64/float, R/binary>>, _Mode, Left) ->
    {F, R, counted(Left)};
decode_one(<<MT:3, AI:5, Rest/binary>>, Mode, Left) ->
    {N, R} = decode_count(AI, Rest),
    decode_value(MT, N, R, Mode, counted(Left)).

%% Each item takes one from what is left of the budget; a lenient decode has
%% none.
counted(unbounded) -> unbounded;
counted(0) -> throw(too_many_elements);
counted(Left) -> Left - 1.

decode_count(AI, R) when AI =< 23 -> {AI, R};
decode_count(24, <<N, R/binary>>) -> {N, R};
decode_count(25, <<N:16, R/binary>>) -> {N, R};
decode_count(26, <<N:32, R/binary>>) -> {N, R};
decode_count(27, <<N:64, R/binary>>) -> {N, R}.

%% A strict decode refuses a positive integer above 2^63-1, as it refuses a negative one below -2^63.
decode_value(0, N, _R, {strict, _Depth}, _Left) when N >= 1 bsl 63 ->
    throw(integer_out_of_range);
decode_value(0, N, R, _Mode, Left) ->
    {N, R, Left};
%% Negative integer (major 1) — the encoded count `N' represents the
%% integer `-1 - N'.
decode_value(1, N, _R, {strict, _Depth}, _Left) when N >= 1 bsl 63 ->
    throw(integer_out_of_range);
decode_value(1, N, R, _Mode, Left) ->
    {-1 - N, R, Left};
decode_value(2, Len, R, _Mode, Left) ->
    <<B:Len/binary, Rest/binary>> = R,
    {B, Rest, Left};
decode_value(3, Len, R, Mode, Left) ->
    <<B:Len/binary, Rest/binary>> = R,
    {{text, checked_text(Mode, B)}, Rest, Left};
decode_value(4, Len, R, Mode, Left) ->
    decode_array(Len, R, [], deeper(Mode), Left);
decode_value(5, Len, R, Mode, Left) ->
    decode_map(Len, R, #{}, deeper(Mode), Left).

%% A strict decode counts nesting: 64 levels of arrays and maps are accepted, and one more is refused.
deeper(lenient) -> lenient;
deeper({strict, Depth}) when Depth >= 64 -> throw(too_deep);
deeper({strict, Depth}) -> {strict, Depth + 1}.

%% A strict decode refuses text that is not valid UTF-8.
checked_text({strict, _Depth}, B) -> valid_text(unicode:characters_to_binary(B, utf8, utf8) =:= B, B);
checked_text(lenient, B) -> B.

valid_text(true, B) -> B;
valid_text(false, _B) -> throw(invalid_text).

decode_array(0, R, Acc, _Mode, Left) ->
    {lists:reverse(Acc), R, Left};
decode_array(N, R, Acc, Mode, Left) ->
    {V, R1, Left1} = decode_one(R, Mode, Left),
    decode_array(N - 1, R1, [V | Acc], Mode, Left1).

decode_map(0, R, Acc, _Mode, Left) ->
    {Acc, R, Left};
decode_map(N, R, Acc, Mode, Left) ->
    {K, R1, Left1} = decode_one(R, Mode, Left),
    {V, R2, Left2} = decode_one(R1, Mode, Left1),
    decode_map(N - 1, R2, put_key(Mode, K, V, Acc), Mode, Left2).

%% A strict decode takes only text and integer keys and refuses a key it has already seen; a lenient one keeps the
%% last value. The lookup keeps the check linear in the number of keys.
put_key({strict, _Depth}, {text, _} = K, V, Acc) -> strict_put(K, V, Acc);
put_key({strict, _Depth}, K, V, Acc) when is_integer(K) -> strict_put(K, V, Acc);
put_key({strict, _Depth}, _K, _V, _Acc) -> throw(bad_key);
put_key(lenient, K, V, Acc) -> Acc#{K => V}.

strict_put(K, _V, Acc) when is_map_key(K, Acc) -> throw(duplicate_key);
strict_put(K, V, Acc) -> Acc#{K => V}.

%% IEEE 754 binary16 -> Erlang float. Subnormals and zero fall out of the
%% same arithmetic; exponent 31 is NaN/infinity, which has no Erlang
%% representation, so it is left to fail the match above.
half_to_float(<<S:1, 0:5, Frac:10>>) ->
    sign(S) * math:pow(2, -14) * (Frac / 1024);
half_to_float(<<S:1, Exp:5, Frac:10>>) when Exp < 31 ->
    sign(S) * math:pow(2, Exp - 15) * (1 + Frac / 1024).

sign(0) -> 1.0;
sign(1) -> -1.0.
