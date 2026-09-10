%% EUnit tests for macula_record_cbor:decode_strict/1, the decoding rule of the post-quantum handshake frames: a
%% duplicate map key at any depth, bytes after the top-level item, and malformed input are refused, never raised on.
-module(macula_record_cbor_strict_tests).

-include_lib("eunit/include/eunit.hrl").

%% {"a": 1, "a": 2}
-define(DUPLICATE_TOP, <<16#A2, 16#61, $a, 16#01, 16#61, $a, 16#02>>).
%% {"b": {"a": 1, "a": 2}}
-define(DUPLICATE_NESTED, <<16#A1, 16#61, $b, 16#A2, 16#61, $a, 16#01, 16#61, $a, 16#02>>).
%% [{"a": 1, "a": 2}]
-define(DUPLICATE_IN_ARRAY, <<16#81, 16#A2, 16#61, $a, 16#01, 16#61, $a, 16#02>>).

duplicate_key_at_the_top_level_is_refused_test() ->
    ?assertEqual({error, duplicate_key}, macula_record_cbor:decode_strict(?DUPLICATE_TOP)).

duplicate_key_in_a_nested_map_is_refused_test() ->
    ?assertEqual({error, duplicate_key}, macula_record_cbor:decode_strict(?DUPLICATE_NESTED)).

duplicate_key_in_a_map_inside_an_array_is_refused_test() ->
    ?assertEqual({error, duplicate_key}, macula_record_cbor:decode_strict(?DUPLICATE_IN_ARRAY)).

bytes_after_the_top_level_item_are_refused_test() ->
    ?assertEqual({error, trailing_bytes}, macula_record_cbor:decode_strict(<<16#A1, 16#61, $a, 16#01, 16#00>>)).

truncated_input_is_refused_test() ->
    ?assertEqual({error, malformed}, macula_record_cbor:decode_strict(<<16#A2, 16#61, $a>>)).

empty_input_is_refused_test() ->
    ?assertEqual({error, malformed}, macula_record_cbor:decode_strict(<<>>)).

well_formed_map_decodes_test() ->
    Value = #{{text, <<"label">>} => {text, <<"MACULA-PQ-STATUS-V1">>},
              {text, <<"node_id">>} => <<0:256>>,
              {text, <<"issued_at">>} => 1789000000000},
    ?assertEqual({ok, Value}, macula_record_cbor:decode_strict(macula_record_cbor:encode(Value))).

lenient_decode_keeps_the_last_duplicate_as_before_test() ->
    ?assertEqual(#{{text, <<"a">>} => 2}, macula_record_cbor:decode(?DUPLICATE_TOP)).

%%------------------------------------------------------------------
%% The decoding rule of DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md
%%------------------------------------------------------------------

invalid_utf8_in_a_text_value_is_refused_test() ->
    ?assertEqual({error, invalid_text}, macula_record_cbor:decode_strict(<<16#A1, 16#61, $a, 16#61, 16#FF>>)).

invalid_utf8_in_a_text_key_is_refused_test() ->
    ?assertEqual({error, invalid_text}, macula_record_cbor:decode_strict(<<16#A1, 16#61, 16#FF, 16#01>>)).

valid_multibyte_text_is_accepted_test() ->
    Text = <<"caf", 16#C3, 16#A9>>,
    ?assertEqual({ok, #{{text, <<"k">>} => {text, Text}}},
                 macula_record_cbor:decode_strict(<<16#A1, 16#61, $k, 16#65, Text/binary>>)).

%% Map keys are text or integers only.
map_keys_other_than_text_or_integer_are_refused_test_() ->
    [?_assertEqual({error, bad_key}, macula_record_cbor:decode_strict(<<16#A1, Key/binary, 16#01>>))
     || Key <- [<<16#41, $a>>,                     %% byte string
                <<16#F9, 16#3F, 16#F0>>,           %% float 1.0
                <<16#80>>,                         %% array
                <<16#A0>>,                         %% map
                <<16#F6>>]].                       %% null

integer_map_keys_are_accepted_test() ->
    ?assertEqual({ok, #{1 => 2, -1 => 3}}, macula_record_cbor:decode_strict(<<16#A2, 16#01, 16#02, 16#20, 16#03>>)).

duplicate_integer_keys_are_refused_test() ->
    ?assertEqual({error, duplicate_key}, macula_record_cbor:decode_strict(<<16#A2, 16#01, 16#02, 16#01, 16#03>>)).

%% A text key in two length widths is still one key.
a_text_key_in_two_widths_is_a_duplicate_test() ->
    ?assertEqual({error, duplicate_key},
                 macula_record_cbor:decode_strict(<<16#A2, 16#61, $a, 16#01, 16#78, 1, $a, 16#02>>)).

floats_are_accepted_as_values_test() ->
    %% Half float 0x3E00: exponent 15, fraction 512, so 1.5.
    ?assertEqual({ok, [1.5]}, macula_record_cbor:decode_strict(<<16#81, 16#F9, 16#3E, 16#00>>)).

%% 64 nested containers are accepted; one more is refused.
nesting_up_to_64_levels_is_accepted_test() ->
    ?assertMatch({ok, _}, macula_record_cbor:decode_strict(nested_arrays(64))).

nesting_past_64_levels_is_refused_test() ->
    ?assertEqual({error, too_deep}, macula_record_cbor:decode_strict(nested_arrays(65))).

nesting_counts_maps_as_well_as_arrays_test() ->
    ?assertEqual({error, too_deep}, macula_record_cbor:decode_strict(<<16#A1, 16#61, $a, (nested_arrays(64))/binary>>)).

negative_integer_down_to_minus_2_pow_63_is_accepted_test() ->
    ?assertEqual({ok, -(1 bsl 63)}, macula_record_cbor:decode_strict(<<16#3B, ((1 bsl 63) - 1):64>>)).

negative_integer_below_minus_2_pow_63_is_refused_test_() ->
    [?_assertEqual({error, integer_out_of_range}, macula_record_cbor:decode_strict(<<16#3B, N:64>>))
     || N <- [1 bsl 63, (1 bsl 64) - 1]].

largest_unsigned_integer_is_accepted_test() ->
    ?assertEqual({ok, (1 bsl 64) - 1}, macula_record_cbor:decode_strict(<<16#1B, ((1 bsl 64) - 1):64>>)).

nested_arrays(0) -> <<16#00>>;
nested_arrays(N) -> <<16#81, (nested_arrays(N - 1))/binary>>.

%% Refusals the decoder already enforces, pinned here. The rest of the rule is pinned elsewhere: bytes after the
%% top-level item above, unknown keys and wrong field types in a signed structure in macula_key_bindings_tests and
%% macula_handshake_tests, and the 16 MiB frame cap in macula_frame_bytes_tests.
an_indefinite_length_is_refused_test_() ->
    [?_assertEqual({error, malformed}, macula_record_cbor:decode_strict(Bin))
     || Bin <- [<<16#5F, 16#41, $a, 16#FF>>,           %% indefinite byte string
                <<16#9F, 16#01, 16#FF>>,                %% indefinite array
                <<16#BF, 16#61, $a, 16#01, 16#FF>>]].   %% indefinite map

a_tag_is_refused_test_() ->
    [?_assertEqual({error, malformed}, macula_record_cbor:decode_strict(Bin))
     || Bin <- [<<16#C1, 16#1A, 0, 0, 0, 1>>,          %% tag 1, epoch time
                <<16#C2, 16#41, 16#01>>]].              %% tag 2, bignum

simple_values_other_than_null_are_refused_test_() ->
    [?_assertEqual({error, malformed}, macula_record_cbor:decode_strict(Bin))
     || Bin <- [<<16#F4>>, <<16#F5>>, <<16#F7>>, <<16#F8, 32>>]].   %% false, true, undefined, simple(32)

null_is_accepted_test() ->
    ?assertEqual({ok, null}, macula_record_cbor:decode_strict(<<16#F6>>)).
