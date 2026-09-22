%% EUnit tests that run the shared decoding rule vectors, test/vectors/decoding_rule_v1.json. Every stack's CI runs
%% the same file, pinned by commit (Stage 4).
%%
%% An entry names how it is read in `via'. `record', the default, decodes the bytes under the decoding rule itself
%% (macula_record_cbor:decode_strict/1). `request_fields' reads them as the fields of a CALL, under the rules its
%% field table gives them, which is where the bound on a delegation chain's `proofs' lives (D7, chain transport).
-module(macula_decoding_rule_vectors_tests).

-include_lib("eunit/include/eunit.hrl").

-define(VECTORS, "vectors/decoding_rule_v1.json").

vectors_test_() ->
    [{binary_to_list(Name), fun() -> check(maps:get(<<"via">>, Entry, <<"record">>), Expect, binary:decode_hex(Hex)) end}
     || #{<<"name">> := Name, <<"cbor">> := Hex, <<"expect">> := Expect} = Entry <- entries()].

every_entry_has_a_distinct_name_a_verdict_and_a_way_to_read_it_test() ->
    Entries = entries(),
    Names = [Name || #{<<"name">> := Name} <- Entries],
    ?assertEqual(length(Names), length(lists:usort(Names))),
    ?assertEqual([], [E || #{<<"expect">> := Expect} = E <- Entries, Expect =/= <<"accept">>, Expect =/= <<"refuse">>]),
    ?assertEqual([], [E || E <- Entries, not lists:member(maps:get(<<"via">>, E, <<"record">>),
                                                          [<<"record">>, <<"request_fields">>])]).

check(<<"record">>, <<"accept">>, Bin) ->
    ?assertMatch({ok, _}, macula_record_cbor:decode_strict(Bin));
check(<<"record">>, <<"refuse">>, Bin) ->
    ?assertMatch({error, _}, macula_record_cbor:decode_strict(Bin));
check(<<"request_fields">>, Expect, Bin) ->
    {ok, Wire} = macula_record_cbor:decode_strict(Bin),
    request_verdict(Expect, macula_frame:request_fields(call, Wire)).

request_verdict(<<"accept">>, Read) -> ?assertMatch({ok, _}, Read);
request_verdict(<<"refuse">>, Read) -> ?assertEqual(error, Read).

entries() ->
    {ok, Json} = file:read_file(filename:join(filename:dirname(?FILE), ?VECTORS)),
    #{<<"version">> := 1, <<"entries">> := Entries} = json:decode(Json),
    Entries.
