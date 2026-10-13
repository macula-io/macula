%% The wire 1.0 record conformance runner (macula#100): every record case of the verify, custodian, check_order and tbs
%% vectors (priv/vectors/wire_1_0/, docs/design/WIRE_1_0.md section 5) through macula_record. A reader case runs
%% verify/3, a custodian case custodian_verify/3, each under the case's profile and clock; the outcome and the refusal
%% name must be the file's, and where a custodian case names its slot, the stored record's storage key. A tbs case's
%% bytes, under its signer's key and signature, are a record the reader accepts at its creation time.
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

tbs_test_() ->
    Keys = maps:get(<<"profiles">>, vectors(<<"keys">>)),
    [case_test(C#{object => tbs_object(C, Keys)}, reader) || C <- record_cases(<<"tbs">>)].

tbs_object(#{<<"profile">> := P, <<"input">> := #{<<"signer">> := Role},
             <<"expect">> := #{<<"tbs">> := Tbs, <<"signature">> := Sig}}, Keys) ->
    #{P := #{Role := #{<<"key">> := Key}}} = Keys,
    macula_signed_object:encode(#{key => binary:decode_hex(Key), tbs => binary:decode_hex(Tbs),
                                  signature => binary:decode_hex(Sig)}).

case_test(#{<<"id">> := Id, <<"expect">> := Expect} = Case, Role) ->
    {binary_to_list(Id), fun() -> ?assertEqual(expected(Expect), outcome(Role, Case)) end}.

outcome(reader, Case) ->
    named(macula_record:verify(object(Case), profile(Case), now_ms(Case)), Case);
outcome(custodian, Case) ->
    named(macula_record:custodian_verify(object(Case), profile(Case), now_ms(Case)), Case).

named({ok, Record}, #{<<"expect">> := #{<<"slot">> := _}}) -> {ok, macula_record:storage_key(Record)};
named({ok, _Record}, _Case) -> ok;
named({error, Reason}, _Case) -> {refuse, atom_to_binary(Reason)}.

expected(#{<<"slot">> := Slot}) -> {ok, binary:decode_hex(Slot)};
expected(#{<<"outcome">> := Outcome}) when Outcome =:= <<"accept">>; Outcome =:= <<"store">> -> ok;
expected(#{<<"outcome">> := <<"refuse">>, <<"reason">> := Reason}) -> {refuse, Reason}.

role(#{<<"expect">> := #{<<"role">> := <<"custodian">>}}) -> custodian;
role(#{<<"expect">> := #{<<"role">> := <<"reader">>}}) -> reader.

object(#{object := Bytes}) -> Bytes;
object(#{<<"input">> := #{<<"object">> := Hex}}) -> binary:decode_hex(Hex).

profile(#{<<"profile">> := Profile}) -> binary_to_existing_atom(Profile).

now_ms(#{<<"input">> := #{<<"now_ms">> := Now}}) -> Now;
now_ms(#{<<"input">> := #{<<"fields">> := #{<<"created_at">> := CreatedAt}}}) -> CreatedAt.

record_cases(Class) ->
    [C || #{<<"lands_with">> := LandsWith} = C <- cases(Class), lists:member(?LANDS_WITH, LandsWith)].

cases(Class) ->
    maps:get(<<"cases">>, vectors(Class)).

vectors(Class) ->
    {ok, Bytes} = file:read_file(vector_file(<<Class/binary, ".json">>)),
    json:decode(Bytes).

%% The vectors ship in priv, as a consumer of the hex package reads them.
vector_file(Name) ->
    filename:join([code:priv_dir(macula), "vectors", "wire_1_0", Name]).
