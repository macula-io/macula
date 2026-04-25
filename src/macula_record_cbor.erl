%% @doc Deterministic CBOR encoder/decoder.
%%
%% Implements the subset of RFC 8949 needed by Macula records:
%% unsigned ints, byte strings, text strings, arrays, maps, and `null'.
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
%%   <li>`binary()' — byte string (major 2)</li>
%%   <li>`{text, binary()}' — UTF-8 text string (major 3)</li>
%%   <li>`[value()]' — array (major 4)</li>
%%   <li>`#{value() => value()}' — map (major 5)</li>
%%   <li>`null' — simple null (major 7, value 22)</li>
%% </ul>
-module(macula_record_cbor).

-export([encode/1, decode/1]).
-export_type([value/0]).

-type value() ::
    non_neg_integer()
  | binary()
  | {text, binary()}
  | [value()]
  | #{value() => value()}
  | null.

-define(MAX_UINT64, 16#FFFFFFFFFFFFFFFF).

%%------------------------------------------------------------------
%% Encode
%%------------------------------------------------------------------

-spec encode(value()) -> binary().
encode(N) when is_integer(N), N >= 0, N =< ?MAX_UINT64 ->
    head(0, N);
encode({text, B}) when is_binary(B) ->
    <<(head(3, byte_size(B)))/binary, B/binary>>;
encode(B) when is_binary(B) ->
    <<(head(2, byte_size(B)))/binary, B/binary>>;
encode(L) when is_list(L) ->
    encode_array(L);
encode(M) when is_map(M) ->
    encode_map(M);
encode(null) ->
    <<16#F6>>.

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

%% Major 7, value 22 = null. Anything else with major 7 is unsupported.
decode_one(<<7:3, 22:5, R/binary>>) ->
    {null, R};
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
