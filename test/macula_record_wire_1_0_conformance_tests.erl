%% The wire 1.0 record conformance runner (macula#100): every record case of the verify, custodian and check_order
%% vectors (test/vectors/wire_1_0/, docs/design/WIRE_1_0.md section 5) through macula_record. A reader case runs
%% verify/3, a custodian case custodian_verify/3, each under the case's profile and clock; the outcome and the refusal
%% name must be the file's.
-module(macula_record_wire_1_0_conformance_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LANDS_WITH, <<"macula#100">>).

reader_test_() ->
    [case_test(C, reader) || C <- record_cases(<<"verify">>)].

custodian_test_() ->
    Objects = maps:from_list([{maps:get(<<"id">>, C), object(C)} || C <- cases(<<"verify">>)]),
    [case_test(C#{object => maps:get(Ref, Objects)}, custodian)
     || #{<<"input">> := #{<<"object_of">> := Ref}} = C <- record_cases(<<"custodian">>)].

check_order_test_() ->
    [case_test(C, role(C)) || C <- record_cases(<<"check_order">>)].

case_test(#{<<"id">> := Id, <<"expect">> := Expect} = Case, Role) ->
    {binary_to_list(Id), fun() -> ?assertEqual(expected(Expect), outcome(Role, Case)) end}.

outcome(reader, Case) ->
    named(macula_record:verify(object(Case), profile(Case), now_ms(Case)));
outcome(custodian, Case) ->
    named(macula_record:custodian_verify(object(Case), profile(Case), now_ms(Case))).

named({ok, _Record}) -> ok;
named({error, Reason}) -> {refuse, atom_to_binary(Reason)}.

expected(#{<<"outcome">> := Outcome}) when Outcome =:= <<"accept">>; Outcome =:= <<"store">> -> ok;
expected(#{<<"outcome">> := <<"refuse">>, <<"reason">> := Reason}) -> {refuse, Reason}.

role(#{<<"expect">> := #{<<"role">> := <<"custodian">>}}) -> custodian;
role(#{<<"expect">> := #{<<"role">> := <<"reader">>}}) -> reader.

object(#{object := Bytes}) -> Bytes;
object(#{<<"input">> := #{<<"object">> := Hex}}) -> binary:decode_hex(Hex).

profile(#{<<"profile">> := Profile}) -> binary_to_existing_atom(Profile).

now_ms(#{<<"input">> := #{<<"now_ms">> := Now}}) -> Now.

record_cases(Class) ->
    [C || #{<<"lands_with">> := ?LANDS_WITH} = C <- cases(Class)].

cases(Class) ->
    maps:get(<<"cases">>, vectors(Class)).

vectors(Class) ->
    {ok, Bytes} = file:read_file(vector_file(<<Class/binary, ".json">>)),
    json:decode(Bytes).

%% The source tree's vector file, from the project root eunit runs in, or from the build tree's copy of the
%% application.
vector_file(Name) ->
    Rel = filename:join("wire_1_0", Name),
    first_existing([filename:join("test/vectors", Rel), filename:join("../../test/vectors", Rel)]
                   ++ [filename:join([Dir, "..", "..", "..", "..", "test", "vectors", Rel])
                       || Dir <- [code:lib_dir(macula)], is_list(Dir)]).

first_existing([F | Rest]) ->
    first_existing(filelib:is_regular(F), F, Rest).

first_existing(true, F, _Rest) -> F;
first_existing(false, _F, Rest) -> first_existing(Rest).
