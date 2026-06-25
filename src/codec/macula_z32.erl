%%% @doc z-base-32 codec (Phil Zimmermann's "Human-Oriented Base-32
%%% Encoding"). Alphabet: `ybndrfg8ejkmcpqxot1uwisza345h769'.
%%%
%%% Used by Macula to encode 32-byte Ed25519 pubkeys as DNS-label-
%%% friendly strings: 32 bytes → 52 ASCII characters, comfortably
%%% within DNS's 63-char per-label cap. PKARR (the
%%% public-key-addressable resource record convention) and projects
%%% like Pubky use this same encoding for the same reason.
%%%
%%% Encoding direction: bit stream is taken MSB-first from the
%%% input bytes, grouped into 5-bit chunks (the final chunk
%%% zero-padded on the right if the input length is not a multiple
%%% of 5 bits). Each chunk indexes into the alphabet.
%%%
%%% Decoding is the inverse: characters → 5-bit values → bit
%%% stream → 8-bit byte stream, dropping any trailing bits left
%%% over from the encoder's right-padding.
%%%
%%% Length convention (the cases that matter for Macula):
%%%   - 32 bytes (Ed25519 pubkey) → 52 chars
%%%   - 16 bytes (UUID-like) → 26 chars
%%%   - empty → empty
%%%
%%% Reference: <a href="https://philzimmermann.com/docs/human-oriented-base-32-encoding.txt">Phil Zimmermann's z-base-32 spec</a>.
%%% @end
-module(macula_z32).

-export([encode/1, decode/1, is_valid_label/1]).

%% z-base-32 alphabet, indexed 0..31.
-define(ALPHABET, <<"ybndrfg8ejkmcpqxot1uwisza345h769">>).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Encode a binary into its z-base-32 representation.
-spec encode(binary()) -> binary().
encode(<<>>) ->
    <<>>;
encode(Bytes) when is_binary(Bytes) ->
    BitCount = byte_size(Bytes) * 8,
    PaddedLen = ((BitCount + 4) div 5) * 5,
    PadBits = PaddedLen - BitCount,
    Padded = <<Bytes/bitstring, 0:PadBits>>,
    encode_chunks(Padded, []).

encode_chunks(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
encode_chunks(<<Chunk:5, Rest/bitstring>>, Acc) ->
    Char = binary:at(?ALPHABET, Chunk),
    encode_chunks(Rest, [Char | Acc]).

%% @doc Decode a z-base-32 string back into its original bytes.
%% Returns `{error, invalid_z32}' on any character outside the
%% alphabet, or when the input length is not a valid encoding of
%% a whole-byte payload (i.e., the bit length isn't a multiple of 8
%% within the rounding-up tolerance).
-spec decode(binary()) -> {ok, binary()} | {error, invalid_z32}.
decode(<<>>) ->
    {ok, <<>>};
decode(Z32) when is_binary(Z32) ->
    decode_chars(Z32, <<>>).

decode_chars(<<>>, BitAcc) ->
    %% BitAcc may be N bits long where N is a multiple of 5 but
    %% may not be a multiple of 8. We accept up to 4 trailing
    %% padding bits (those produced by encode/1 when the original
    %% input wasn't a multiple of 5 bits long); they MUST be zero.
    BitLen = bit_size(BitAcc),
    ByteLen = BitLen div 8,
    PadBits = BitLen rem 8,
    decode_padded(PadBits =< 4, BitAcc, ByteLen, PadBits);
decode_chars(<<Char, Rest/binary>>, BitAcc) ->
    decode_char(char_to_value(Char), Rest, BitAcc).

decode_padded(false, _BitAcc, _ByteLen, _PadBits) ->
    {error, invalid_z32};
decode_padded(true, BitAcc, ByteLen, PadBits) ->
    <<Bytes:ByteLen/binary, Pad:PadBits>> = BitAcc,
    decode_pad_value(Pad, Bytes).

decode_pad_value(0, Bytes) -> {ok, Bytes};
decode_pad_value(_, _Bytes) -> {error, invalid_z32}.

decode_char({ok, Value}, Rest, BitAcc) ->
    decode_chars(Rest, <<BitAcc/bitstring, Value:5>>);
decode_char(error, _Rest, _BitAcc) ->
    {error, invalid_z32}.

%% @doc Check whether a binary is a syntactically valid DNS label
%% under the z-base-32 alphabet (every character is in the
%% alphabet, length is between 1 and 63 octets, and it decodes
%% cleanly to a whole-byte payload).
-spec is_valid_label(binary()) -> boolean().
is_valid_label(Bin) when is_binary(Bin) ->
    Size = byte_size(Bin),
    Size >= 1 andalso Size =< 63 andalso
        case decode(Bin) of
            {ok, _}    -> true;
            {error, _} -> false
        end;
is_valid_label(_) ->
    false.

%%====================================================================
%% Alphabet lookup. Inline guard clauses for speed; the alphabet is
%% small enough that the compiler's case-tree is faster than a map.
%%====================================================================

char_to_value($y) -> {ok, 0};
char_to_value($b) -> {ok, 1};
char_to_value($n) -> {ok, 2};
char_to_value($d) -> {ok, 3};
char_to_value($r) -> {ok, 4};
char_to_value($f) -> {ok, 5};
char_to_value($g) -> {ok, 6};
char_to_value($8) -> {ok, 7};
char_to_value($e) -> {ok, 8};
char_to_value($j) -> {ok, 9};
char_to_value($k) -> {ok, 10};
char_to_value($m) -> {ok, 11};
char_to_value($c) -> {ok, 12};
char_to_value($p) -> {ok, 13};
char_to_value($q) -> {ok, 14};
char_to_value($x) -> {ok, 15};
char_to_value($o) -> {ok, 16};
char_to_value($t) -> {ok, 17};
char_to_value($1) -> {ok, 18};
char_to_value($u) -> {ok, 19};
char_to_value($w) -> {ok, 20};
char_to_value($i) -> {ok, 21};
char_to_value($s) -> {ok, 22};
char_to_value($z) -> {ok, 23};
char_to_value($a) -> {ok, 24};
char_to_value($3) -> {ok, 25};
char_to_value($4) -> {ok, 26};
char_to_value($5) -> {ok, 27};
char_to_value($h) -> {ok, 28};
char_to_value($7) -> {ok, 29};
char_to_value($6) -> {ok, 30};
char_to_value($9) -> {ok, 31};
char_to_value(_)  -> error.
