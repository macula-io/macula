%%%-------------------------------------------------------------------
%%% @doc Tests for the macula_z32 codec.
%%%
%%% Coverage:
%%%   - Empty round-trip
%%%   - Length contract (32 bytes → 52 chars)
%%%   - Hand-computed test vectors (zero32, ones32, small mixed)
%%%   - Round-trip property over arbitrary 32-byte payloads
%%%   - Reject characters outside the alphabet
%%%   - Reject malformed lengths (non-recoverable padding)
%%%   - is_valid_label/1 guard cases
%%% @end
%%%-------------------------------------------------------------------
-module(macula_z32_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Round-trip / length tests
%%%===================================================================

empty_roundtrip_test() ->
    ?assertEqual(<<>>, macula_z32:encode(<<>>)),
    ?assertEqual({ok, <<>>}, macula_z32:decode(<<>>)).

length_contract_for_32_bytes_test() ->
    %% 32 bytes (Ed25519 pubkey size) → 52 z32 chars (the
    %% load-bearing case for PKARR / Macula station qnames).
    Bytes = crypto:strong_rand_bytes(32),
    Encoded = macula_z32:encode(Bytes),
    ?assertEqual(52, byte_size(Encoded)),
    ?assertEqual({ok, Bytes}, macula_z32:decode(Encoded)).

length_contract_for_16_bytes_test() ->
    %% 16 bytes (UUID-like) → 26 chars.
    Bytes = crypto:strong_rand_bytes(16),
    Encoded = macula_z32:encode(Bytes),
    ?assertEqual(26, byte_size(Encoded)),
    ?assertEqual({ok, Bytes}, macula_z32:decode(Encoded)).

length_contract_for_random_sizes_test() ->
    %% Sample a few sizes; verify length formula
    %% chars = ceil(bytes * 8 / 5).
    lists:foreach(fun(N) ->
        Bytes = crypto:strong_rand_bytes(N),
        Encoded = macula_z32:encode(Bytes),
        Expected = (N * 8 + 4) div 5,
        ?assertEqual(Expected, byte_size(Encoded)),
        ?assertEqual({ok, Bytes}, macula_z32:decode(Encoded))
    end, [1, 2, 3, 5, 7, 11, 17, 23, 31, 32, 33, 64, 100]).

%%%===================================================================
%%% Hand-computed test vectors (independently verified)
%%%===================================================================

vector_zero_32_bytes_test() ->
    %% 32 zero bytes → 256 zero bits → padded to 260 → 52 chunks
    %% all zero → 52 'y' characters (alphabet[0] = 'y').
    Zero32 = binary:copy(<<0>>, 32),
    Expected = binary:copy(<<"y">>, 52),
    ?assertEqual(Expected, macula_z32:encode(Zero32)),
    ?assertEqual({ok, Zero32}, macula_z32:decode(Expected)).

vector_ones_32_bytes_test() ->
    %% 32 0xFF bytes → 256 ones → padded to 260 (4 zero pad bits).
    %% First 51 chunks: all 0b11111 = 31 → '9'.
    %% Last chunk: 1 followed by 4 zero pad bits = 0b10000 = 16 → 'o'.
    Ones32 = binary:copy(<<16#FF>>, 32),
    Expected = <<(binary:copy(<<"9">>, 51))/binary, "o">>,
    ?assertEqual(Expected, macula_z32:encode(Ones32)),
    ?assertEqual({ok, Ones32}, macula_z32:decode(Expected)).

vector_three_bytes_test() ->
    %% <<1,2,3>> = 24 bits = "00000001 00000010 00000011".
    %% Pad 1 zero bit → 25 bits.
    %% 5-bit chunks (MSB-first): 00000 00100 00001 00000 00110 →
    %%   0='y', 4='r', 1='b', 0='y', 6='g' → "yrbyg".
    ?assertEqual(<<"yrbyg">>, macula_z32:encode(<<1, 2, 3>>)),
    ?assertEqual({ok, <<1, 2, 3>>}, macula_z32:decode(<<"yrbyg">>)).

vector_single_byte_test() ->
    %% <<255>> = 8 bits = "11111111". Pad 2 zero bits → 10 bits →
    %% 5-bit chunks: 11111 11100 → 31, 28 → '9', 'h'.
    ?assertEqual(<<"9h">>, macula_z32:encode(<<255>>)),
    ?assertEqual({ok, <<255>>}, macula_z32:decode(<<"9h">>)).

%%%===================================================================
%%% Property: round-trip survives any 32-byte payload
%%%===================================================================

property_roundtrip_random_test_() ->
    {timeout, 10, fun() ->
        lists:foreach(fun(_N) ->
            Bytes = crypto:strong_rand_bytes(32),
            ?assertEqual({ok, Bytes},
                         macula_z32:decode(macula_z32:encode(Bytes)))
        end, lists:seq(1, 200))
    end}.

property_roundtrip_arbitrary_size_test_() ->
    {timeout, 10, fun() ->
        lists:foreach(fun(_N) ->
            Size = rand:uniform(128),
            Bytes = crypto:strong_rand_bytes(Size),
            ?assertEqual({ok, Bytes},
                         macula_z32:decode(macula_z32:encode(Bytes)))
        end, lists:seq(1, 200))
    end}.

%%%===================================================================
%%% Rejection tests
%%%===================================================================

rejects_chars_outside_alphabet_test() ->
    %% Note z-base-32 deliberately omits some characters that look
    %% confusingly similar to others in display fonts (l/1, 0/o
    %% are kept, but uppercase, '0' (zero), 'v', '2' are not in
    %% the alphabet).
    ?assertEqual({error, invalid_z32}, macula_z32:decode(<<"vvvv">>)),
    ?assertEqual({error, invalid_z32}, macula_z32:decode(<<"YBND">>)),  %% uppercase not allowed
    ?assertEqual({error, invalid_z32}, macula_z32:decode(<<"abc!def">>)),
    ?assertEqual({error, invalid_z32}, macula_z32:decode(<<"abc def">>)),
    ?assertEqual({error, invalid_z32}, macula_z32:decode(<<"0">>)).     %% zero not in alphabet

rejects_one_char_input_test() ->
    %% A single z32 char carries 5 bits, which can't form a whole
    %% byte; padding-bit count would be 5 (greater than the 4-bit
    %% maximum that valid encodings can produce).
    ?assertEqual({error, invalid_z32}, macula_z32:decode(<<"y">>)).

rejects_corrupted_padding_bits_test() ->
    %% encode(<<1,2,3>>) = "yrbyg". 25-bit stream: 24 input bits +
    %% 1 zero pad bit. The pad bit is the LSB of the last chunk
    %% (chunk 5 = 'g' = 0b00110, the trailing 0 is the pad).
    %% Replacing 'g' with 'b' (= 0b00001) sets the pad bit to 1;
    %% decode/1 must reject because non-zero padding is the
    %% telltale of a corrupted/forged encoding.
    ?assertEqual({error, invalid_z32}, macula_z32:decode(<<"yrbyb">>)).

%%%===================================================================
%%% is_valid_label/1
%%%===================================================================

is_valid_label_accepts_pubkey_length_test() ->
    Pubkey = crypto:strong_rand_bytes(32),
    Label = macula_z32:encode(Pubkey),
    ?assert(macula_z32:is_valid_label(Label)).

is_valid_label_rejects_empty_test() ->
    %% DNS labels must have non-zero length.
    ?assertNot(macula_z32:is_valid_label(<<>>)).

is_valid_label_rejects_oversize_test() ->
    %% DNS per-label cap is 63 octets.
    Long = binary:copy(<<"y">>, 64),
    ?assertNot(macula_z32:is_valid_label(Long)).

is_valid_label_rejects_invalid_chars_test() ->
    ?assertNot(macula_z32:is_valid_label(<<"abc!def">>)),
    ?assertNot(macula_z32:is_valid_label(<<"YBND">>)).

is_valid_label_rejects_non_binary_test() ->
    ?assertNot(macula_z32:is_valid_label("yndr")),  %% list, not binary
    ?assertNot(macula_z32:is_valid_label(undefined)).
