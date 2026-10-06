%% The realm member endorsement vectors (test/vectors/realm_member_endorsement_v1.json): endorsements a realm key signed
%% once, in both crypto profiles, each with the member, the time and the verdict every SDK's verifier must reach. The
%% file is committed as generated, since ML-DSA-87 and RSA-PSS signing are randomized; this module re-derives every
%% verdict from it on each run through verify_endorsement/4, so the file can never drift from
%% macula_hyparview_endorsement.
-module(macula_realm_member_endorsement_vectors_tests).

-include_lib("eunit/include/eunit.hrl").

vectors_test_() ->
    #{<<"profiles">> := Profiles, <<"realm">> := Realm} = vectors(),
    [{"both profiles", ?_assertEqual([<<"pq_hybrid">>, <<"pq_pure">>], lists:sort(maps:keys(Profiles)))},
     {"every refusal the verifier names after a verified record, and admission, is covered",
      ?_assertEqual(lists:sort([<<"roles">>, <<"not_yet_valid">>, <<"endorsement_expired">>, <<"expired">>,
                                <<"wrong_member">>, <<"wrong_realm">>, <<"untrusted_signer">>, <<"wrong_type">>,
                                <<"endorsement_window_too_long">>, <<"endorsement_window_reversed">>,
                                <<"malformed">>]),
                    lists:usort([verdict_name(C) || _ := P <- Profiles, C <- maps:get(<<"cases">>, P)]))}]
    ++ [{binary_to_list(<<Name/binary, " ", (maps:get(<<"name">>, C))/binary>>),
         fun() -> case_checked(profile(Name), binary:decode_hex(Realm), P, C) end}
        || Name := P <- Profiles, C <- maps:get(<<"cases">>, P)].

profile(<<"pq_pure">>) -> pq_pure;
profile(<<"pq_hybrid">>) -> pq_hybrid.

verdict_name(#{<<"roles">> := _}) -> <<"roles">>;
verdict_name(#{<<"refused">> := Reason}) -> Reason.

%% The endorsement, checked for the file's member at the file's time against the realm key the file carries, reaches
%% the file's verdict.
case_checked(Profile, Realm, #{<<"realm_key">> := RealmKey},
             #{<<"record">> := Record, <<"member">> := Member, <<"now_ms">> := Now} = C) ->
    Trust = #{profile => Profile, realm => Realm,
              realm_key_id => macula_node_keys:key_id(binary:decode_hex(RealmKey), Profile)},
    ?assertEqual(expected(C),
                 macula_hyparview_endorsement:verify_endorsement(binary:decode_hex(Record), Trust,
                                                                 binary:decode_hex(Member), Now)).

expected(#{<<"roles">> := Roles}) -> {ok, Roles};
expected(#{<<"refused">> := Reason}) -> {error, binary_to_existing_atom(Reason)}.

vectors() ->
    {ok, Bytes} = file:read_file(vector_file()),
    json:decode(Bytes).

%% The source tree's vector file, from the project root eunit runs in, or from the build tree's copy of the
%% application.
vector_file() ->
    Name = "realm_member_endorsement_v1.json",
    first_existing([filename:join("test/vectors", Name), filename:join("../../test/vectors", Name)]
                   ++ [filename:join([Dir, "..", "..", "..", "..", "test", "vectors", Name])
                       || Dir <- [code:lib_dir(macula)], is_list(Dir)]).

first_existing([F | Rest]) ->
    first_existing(filelib:is_regular(F), F, Rest).

first_existing(true, F, _Rest) -> F;
first_existing(false, _F, Rest) -> first_existing(Rest).
