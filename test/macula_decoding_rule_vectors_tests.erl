%% EUnit tests that run the shared decoding rule vectors, test/vectors/decoding_rule_v1.json, against
%% macula_record_cbor:decode_strict/1. Every stack's CI runs the same file, pinned by commit (Stage 4).
-module(macula_decoding_rule_vectors_tests).

-include_lib("eunit/include/eunit.hrl").

-define(VECTORS, "vectors/decoding_rule_v1.json").

vectors_test_() ->
    [{binary_to_list(Name), fun() -> check(Expect, binary:decode_hex(Hex)) end}
     || #{<<"name">> := Name, <<"cbor">> := Hex, <<"expect">> := Expect} <- entries()].

every_entry_has_a_distinct_name_and_a_verdict_test() ->
    Entries = entries(),
    Names = [Name || #{<<"name">> := Name} <- Entries],
    ?assertEqual(length(Names), length(lists:usort(Names))),
    ?assertEqual([], [E || #{<<"expect">> := Expect} = E <- Entries, Expect =/= <<"accept">>, Expect =/= <<"refuse">>]).

check(<<"accept">>, Bin) -> ?assertMatch({ok, _}, macula_record_cbor:decode_strict(Bin));
check(<<"refuse">>, Bin) -> ?assertMatch({error, _}, macula_record_cbor:decode_strict(Bin)).

entries() ->
    {ok, Json} = file:read_file(filename:join(filename:dirname(?FILE), ?VECTORS)),
    #{<<"version">> := 1, <<"entries">> := Entries} = json:decode(Json),
    Entries.
