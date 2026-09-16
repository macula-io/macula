%% EUnit tests for the facade accessors of D26: macula:field/2,3 and macula:text/1. A map a peer supplies arrives with
%% {text, Bin} keys; a map handed over in process may carry atom or binary keys. A handler reads both the same way.
-module(macula_field_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------
%% field/2,3
%%------------------------------------------------------------------

a_text_key_is_found_by_atom_or_binary_name_test_() ->
    Map = #{{text, <<"clip_id">>} => 7},
    [?_assertEqual(7, macula:field(clip_id, Map)),
     ?_assertEqual(7, macula:field(<<"clip_id">>, Map))].

an_atom_key_handed_over_in_process_is_found_test_() ->
    Map = #{clip_id => 7},
    [?_assertEqual(7, macula:field(clip_id, Map)),
     ?_assertEqual(7, macula:field(<<"clip_id">>, Map))].

a_binary_key_handed_over_in_process_is_found_test_() ->
    Map = #{<<"clip_id">> => 7},
    [?_assertEqual(7, macula:field(clip_id, Map)),
     ?_assertEqual(7, macula:field(<<"clip_id">>, Map))].

%% The lookup tries {text, Name}, then the atom, then the binary.
the_text_key_comes_first_test() ->
    ?assertEqual(1, macula:field(clip_id, #{{text, <<"clip_id">>} => 1, clip_id => 2, <<"clip_id">> => 3})).

the_atom_key_comes_before_the_binary_key_test() ->
    ?assertEqual(2, macula:field(clip_id, #{clip_id => 2, <<"clip_id">> => 3})).

a_missing_field_is_undefined_test() ->
    ?assertEqual(undefined, macula:field(clip_id, #{{text, <<"other">>} => 1})).

a_missing_field_takes_the_default_test() ->
    ?assertEqual(none, macula:field(clip_id, #{}, none)).

a_present_field_ignores_the_default_test() ->
    ?assertEqual(7, macula:field(clip_id, #{{text, <<"clip_id">>} => 7}, none)).

%% Looking up a binary name never creates an atom, so peer input cannot grow the atom table through the accessor.
a_binary_name_creates_no_atom_test() ->
    Name = <<"d26_field_name_that_names_no_atom">>,
    ?assertEqual(undefined, macula:field(Name, #{})),
    ?assertError(badarg, binary_to_existing_atom(Name)).

%%------------------------------------------------------------------
%% text/1
%%------------------------------------------------------------------

text_unwraps_a_text_value_test() ->
    ?assertEqual(<<"caf", 16#C3, 16#A9>>, macula:text({text, <<"caf", 16#C3, 16#A9>>})).

text_returns_a_binary_unchanged_test() ->
    ?assertEqual(<<"raw">>, macula:text(<<"raw">>)).

text_refuses_anything_else_test_() ->
    [?_assertError(badarg, macula:text(Value)) || Value <- [42, clip_id, [<<"a">>], {text, not_a_binary}]].
