%% EUnit tests for macula_record_cbor.
-module(macula_record_cbor_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------
%% Round-trip
%%------------------------------------------------------------------

uint_roundtrip_test_() ->
    [?_assertEqual(N, roundtrip(N))
     || N <- [0, 1, 23, 24, 100, 255, 256, 65535, 65536,
              16#FFFFFFFF, 16#FFFFFFFF + 1, 16#FFFFFFFFFFFFFFFF]].

bytes_roundtrip_test() ->
    Bin = <<1, 2, 3, 4, 5>>,
    ?assertEqual(Bin, roundtrip(Bin)).

empty_bytes_roundtrip_test() ->
    ?assertEqual(<<>>, roundtrip(<<>>)).

text_roundtrip_test() ->
    T = {text, <<"hello macula">>},
    ?assertEqual(T, roundtrip(T)).

empty_text_roundtrip_test() ->
    ?assertEqual({text, <<>>}, roundtrip({text, <<>>})).

empty_array_test() ->
    ?assertEqual([], roundtrip([])).

mixed_array_roundtrip_test() ->
    L = [1, 2, {text, <<"three">>}, <<4, 5, 6>>, null],
    ?assertEqual(L, roundtrip(L)).

empty_map_test() ->
    ?assertEqual(#{}, roundtrip(#{})).

map_roundtrip_test() ->
    M = #{ {text, <<"a">>} => 1, {text, <<"b">>} => 2 },
    ?assertEqual(M, roundtrip(M)).

null_test() ->
    ?assertEqual(null, roundtrip(null)).

nested_map_test() ->
    M = #{
        {text, <<"t">>} => 1,
        {text, <<"k">>} => crypto:strong_rand_bytes(32),
        {text, <<"p">>} => #{
            {text, <<"foo">>} => [1, 2, 3],
            {text, <<"bar">>} => {text, <<"baz">>}
        }
    },
    ?assertEqual(M, roundtrip(M)).

%%------------------------------------------------------------------
%% Determinism — RFC 8949 §4.2.1
%%------------------------------------------------------------------

map_keys_sorted_independent_of_insertion_order_test() ->
    M1 = #{ {text, <<"c">>} => 3, {text, <<"a">>} => 1, {text, <<"b">>} => 2 },
    M2 = #{ {text, <<"a">>} => 1, {text, <<"b">>} => 2, {text, <<"c">>} => 3 },
    ?assertEqual(macula_record_cbor:encode(M1), macula_record_cbor:encode(M2)).

map_keys_sorted_by_encoded_bytes_test() ->
    %% Different key types compare by their CBOR encoding bytes.
    %% encode(1) = <<0>> ; encode({text, <<"a">>}) = <<97, 97>> (text, len 1, "a")
    %% So integer key 1 sorts before the text key.
    M = #{ {text, <<"a">>} => 1, 1 => 2 },
    Wire = macula_record_cbor:encode(M),
    %% First k/v pair should be (1, 2) — integer key first.
    %% map header: <<5:3, 2:5>> = 0xA2.
    %% 1 = <<0:3, 1:5>> = 0x01
    %% 2 = <<0:3, 2:5>> = 0x02
    ?assertEqual(<<16#A2, 16#01, 16#02, 16#61, "a", 1>>, Wire).

shortest_uint_encoding_test() ->
    ?assertEqual(<<0>>,                   macula_record_cbor:encode(0)),
    ?assertEqual(<<23>>,                  macula_record_cbor:encode(23)),
    ?assertEqual(<<24, 24>>,              macula_record_cbor:encode(24)),
    ?assertEqual(<<24, 255>>,             macula_record_cbor:encode(255)),
    ?assertEqual(<<25, 1, 0>>,            macula_record_cbor:encode(256)),
    ?assertEqual(<<25, 255, 255>>,        macula_record_cbor:encode(65535)),
    ?assertEqual(<<26, 0, 1, 0, 0>>,      macula_record_cbor:encode(65536)),
    ?assertEqual(<<27, 0, 0, 0, 1, 0, 0, 0, 0>>,
                 macula_record_cbor:encode(16#100000000)).

definite_lengths_only_test() ->
    %% Indefinite-length items would have AI=31 in the type byte.
    %% Confirm none of our encodings use that.
    Samples = [
        macula_record_cbor:encode([1, 2, 3]),
        macula_record_cbor:encode(#{ 1 => 2, 3 => 4 }),
        macula_record_cbor:encode({text, <<"x">>}),
        macula_record_cbor:encode(<<1, 2, 3>>)
    ],
    [?assert(no_indefinite(B)) || B <- Samples].

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

roundtrip(V) ->
    macula_record_cbor:decode(macula_record_cbor:encode(V)).

%% Walk a CBOR binary and assert no AI=31 (indefinite) markers.
no_indefinite(<<>>) -> true;
no_indefinite(<<_MT:3, 31:5, _/binary>>) -> false;
no_indefinite(<<_MT:3, _AI:5, Rest/binary>>) -> no_indefinite(Rest).
