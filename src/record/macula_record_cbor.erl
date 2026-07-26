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

-export([encode/1, decode/1, is_encodable_int/1]).
-export_type([value/0]).

-type value() ::
    integer()
  | float()
  | binary()
  | {text, binary()}
  | [value()]
  | #{value() => value()}
  | null
  | atom().

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
    {V, <<>>} = decode_one(Bin),
    V.

%% Major 7, value 22 = null.
decode_one(<<7:3, 22:5, R/binary>>) ->
    {null, R};
%% Floats. We only ever EMIT binary64, but a conforming peer may send the
%% shorter forms, so all three are accepted. NaN and the infinities have no
%% Erlang float representation and match no clause here; the frame decoder
%% already turns that into `bad_frame' rather than a crash, which is the
%% right answer for a value this codec cannot faithfully hand to a caller.
decode_one(<<7:3, 25:5, Half:16/bitstring, R/binary>>) ->
    {half_to_float(Half), R};
decode_one(<<7:3, 26:5, F:32/float, R/binary>>) ->
    {F, R};
decode_one(<<7:3, 27:5, F:64/float, R/binary>>) ->
    {F, R};
decode_one(<<MT:3, AI:5, Rest/binary>>) ->
    {N, R} = decode_count(AI, Rest),
    decode_value(MT, N, R).

decode_count(AI, R) when AI =< 23 -> {AI, R};
decode_count(24, <<N, R/binary>>) -> {N, R};
decode_count(25, <<N:16, R/binary>>) -> {N, R};
decode_count(26, <<N:32, R/binary>>) -> {N, R};
decode_count(27, <<N:64, R/binary>>) -> {N, R}.

decode_value(0, N, R) ->
    {N, R};
%% Negative integer (major 1) — the encoded count `N' represents the
%% integer `-1 - N'.
decode_value(1, N, R) ->
    {-1 - N, R};
decode_value(2, Len, R) ->
    <<B:Len/binary, Rest/binary>> = R,
    {B, Rest};
decode_value(3, Len, R) ->
    <<B:Len/binary, Rest/binary>> = R,
    {{text, B}, Rest};
decode_value(4, Len, R) ->
    decode_array(Len, R, []);
decode_value(5, Len, R) ->
    decode_map(Len, R, #{}).

decode_array(0, R, Acc) ->
    {lists:reverse(Acc), R};
decode_array(N, R, Acc) ->
    {V, R1} = decode_one(R),
    decode_array(N - 1, R1, [V | Acc]).

decode_map(0, R, Acc) ->
    {Acc, R};
decode_map(N, R, Acc) ->
    {K, R1} = decode_one(R),
    {V, R2} = decode_one(R1),
    decode_map(N - 1, R2, Acc#{K => V}).

%% IEEE 754 binary16 -> Erlang float. Subnormals and zero fall out of the
%% same arithmetic; exponent 31 is NaN/infinity, which has no Erlang
%% representation, so it is left to fail the match above.
half_to_float(<<S:1, 0:5, Frac:10>>) ->
    sign(S) * math:pow(2, -14) * (Frac / 1024);
half_to_float(<<S:1, Exp:5, Frac:10>>) when Exp < 31 ->
    sign(S) * math:pow(2, Exp - 15) * (1 + Frac / 1024).

sign(0) -> 1.0;
sign(1) -> -1.0.
