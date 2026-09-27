%% The keyed advertisement vectors (test/vectors/E2E_SEAL_V1.md, amendment A1): procedure advertisements a provider
%% signed once, in both crypto profiles, each with the verdict every SDK's record verifier must reach at the vector's
%% clock. The file is committed as generated, since ML-DSA-87 and RSA-PSS signing are randomized; this module
%% re-derives every verdict, and the key and id an accepted one names, from it on each run, so the file can never drift
%% from macula_record.
-module(macula_seal_advertisement_vectors_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CASES, [<<"keyed">>, <<"kem_key_alone">>, <<"kem_key_id_alone">>, <<"kem_key_id_of_another_key">>,
                <<"kem_key_of_no_profile_size">>]).

vectors_test_() ->
    Doc = vectors(),
    Profiles = maps:get(<<"profiles">>, Doc),
    [{"both profiles", ?_assertEqual([<<"pq_hybrid">>, <<"pq_pure">>], lists:sort(maps:keys(Profiles)))}]
    ++ [{binary_to_list(Name) ++ " cases", ?_assertEqual(lists:sort(?CASES), case_names(P))}
        || Name := P <- Profiles]
    ++ [{binary_to_list(<<Name/binary, " ", (maps:get(<<"name">>, C))/binary>>),
         fun() -> case_checked(Doc, profile(Name), P, C) end}
        || Name := P <- Profiles, C <- maps:get(<<"cases">>, P)]
    ++ [{binary_to_list(Name) ++ " only the keyed one is accepted", fun() -> verdicts_pinned(P) end}
        || Name := P <- Profiles].

%% A profile by its name in the file, never through binary_to_existing_atom: whether that atom exists yet depends on
%% which modules happen to have loaded.
profile(<<"pq_pure">>) -> pq_pure;
profile(<<"pq_hybrid">>) -> pq_hybrid.

case_names(#{<<"cases">> := Cases}) ->
    lists:sort([Name || #{<<"name">> := Name} <- Cases]).

%% The verdict macula_record reaches now is the one the file pins; every record is the signer's, an advertisement of
%% the file's realm, procedure and station.
case_checked(Doc, Profile, #{<<"now_ms">> := Now} = P, #{<<"record">> := Record, <<"verdict">> := Verdict} = C) ->
    Result = macula_record:verify(binary:decode_hex(Record), Profile, Now),
    ?assertEqual(Verdict, verdict(Result)),
    accepted_checked(Result, Doc, Profile, P, C).

verdict({ok, _Record}) -> <<"accepted">>;
verdict({error, Refusal}) -> atom_to_binary(Refusal).

accepted_checked({ok, Record}, Doc, Profile, P, C) ->
    #{realm_id := Realm, procedure := Procedure, serving_station := Station, advertiser_node := Advertiser,
      kem_key := KemKey, kem_key_id := KemKeyId} = macula_record:read_procedure_advertisement(Record),
    ?assertEqual(maps:get(<<"signer_public_key">>, P), hex(macula_record:key(Record))),
    ?assertEqual(maps:get(<<"signer_node_id">>, P), hex(Advertiser)),
    ?assertEqual(maps:get(<<"signer_node_id">>, P), hex(macula_node_keys:node_id(macula_record:key(Record), Profile))),
    ?assertEqual({maps:get(<<"realm_id">>, Doc), maps:get(<<"procedure">>, Doc), maps:get(<<"serving_station">>, Doc)},
                 {hex(Realm), Procedure, hex(Station)}),
    ?assertEqual({maps:get(<<"kem_key">>, C), maps:get(<<"kem_key_id">>, C)}, {hex(KemKey), hex(KemKeyId)}),
    ?assertEqual(macula_seal:carried_key_size(Profile), byte_size(KemKey)),
    ?assertEqual(maps:get(<<"kem_key_size">>, P), byte_size(KemKey)),
    ?assertEqual(KemKeyId, macula_seal:key_id(KemKey));
accepted_checked({error, _Refusal}, _Doc, _Profile, _P, C) ->
    ?assertNot(maps:is_key(<<"kem_key">>, C) orelse maps:is_key(<<"kem_key_id">>, C)).

%% The keyed advertisement is accepted and every other shape of the pair is refused as malformed.
verdicts_pinned(#{<<"cases">> := Cases}) ->
    ?assertEqual([{Name, expected(Name)} || Name <- lists:sort(?CASES)],
                 lists:sort([{Name, Verdict} || #{<<"name">> := Name, <<"verdict">> := Verdict} <- Cases])).

expected(<<"keyed">>) -> <<"accepted">>;
expected(_Refused) -> <<"malformed">>.

hex(Bin) -> binary:encode_hex(Bin, lowercase).

vectors() ->
    {ok, Bytes} = file:read_file(vector_file()),
    json:decode(Bytes).

%% The source tree's vector file, from the project root eunit runs in, or from the build tree's copy of the
%% application.
vector_file() ->
    Name = "e2e_seal_v1_advertisements.json",
    first_existing([filename:join("test/vectors", Name), filename:join("../../test/vectors", Name)]
                   ++ [filename:join([Dir, "..", "..", "..", "..", "test", "vectors", Name])
                       || Dir <- [code:lib_dir(macula)], is_list(Dir)]).

first_existing([F | Rest]) ->
    first_existing(filelib:is_regular(F), F, Rest).

first_existing(true, F, _Rest) -> F;
first_existing(false, _F, Rest) -> first_existing(Rest).
