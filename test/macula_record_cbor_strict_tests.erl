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
