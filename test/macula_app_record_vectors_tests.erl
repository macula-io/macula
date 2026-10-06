%% Re-derives every outcome in test/vectors/app_record_v1.json (macula#75), the vector macula-go and macula-rust run
%% too: per crypto profile and case, verify/3 at the case's clock, verify_app/3 against the profile's realm key, and
%% read_app_record/1 of a record that verifies. The file is generated once by scripts/generate-app-record-vectors.sh;
%% signing is randomized, so the bytes are the vector and this test only reads them.
-module(macula_app_record_vectors_tests).

-include_lib("eunit/include/eunit.hrl").

every_case_reaches_its_outcome_test_() ->
    #{<<"profiles">> := Profiles, <<"realm_name">> := RealmName} = vectors(),
    [{<<Profile/binary, " ", Name/binary>>, fun() -> check(binary_to_atom(Profile), Section, Case, RealmName) end}
     || {Profile, #{<<"cases">> := Cases} = Section} <- maps:to_list(Profiles),
        #{<<"name">> := Name} = Case <- Cases].

the_vector_covers_both_profiles_and_every_refusal_test() ->
    #{<<"profiles">> := Profiles} = vectors(),
    ?assertEqual([<<"pq_hybrid">>, <<"pq_pure">>], lists:sort(maps:keys(Profiles))),
    [begin
         Outcomes = lists:usort([{V, A} || #{<<"verify">> := V, <<"verify_app">> := A} <- Cases]),
         [?assert(lists:member(Expected, Outcomes))
          || Expected <- [{<<"ok">>, <<"ok">>}, {<<"ok">>, <<"org_key_mismatch">>},
                          {<<"ok">>, <<"org_directory_wrong_realm">>}, {<<"ok">>, <<"org_directory_wrong_org">>},
                          {<<"ok">>, <<"authorization_outlived">>}, {<<"malformed">>, null}]]
     end || #{<<"cases">> := Cases} <- maps:values(Profiles)].

check(Profile, #{<<"realm_key">> := RealmKey, <<"app_key">> := AppKey}, Case, RealmName) ->
    #{<<"record">> := Record, <<"now_ms">> := Now, <<"verify">> := Verify, <<"verify_app">> := VerifyApp,
      <<"reading">> := Reading} = Case,
    Verified = macula_record:verify(binary:decode_hex(Record), Profile, Now),
    ?assertEqual(Verify, outcome(Verified)),
    checked(Verified, Profile, binary:decode_hex(RealmKey), Now, VerifyApp, Reading),
    ?assertEqual(binary:decode_hex(AppKey),
                 macula_record:app_key(macula_realm:id(RealmName), <<"acme">>, <<"weather">>)).

checked({ok, R}, Profile, RealmKey, Now, VerifyApp, Reading) ->
    ?assertEqual(VerifyApp, outcome(macula_record:verify_app(R, #{profile => Profile, realm_key => RealmKey}, Now))),
    ?assertEqual(Reading, as_json(macula_record:read_app_record(R)));
checked({error, _}, _Profile, _RealmKey, _Now, VerifyApp, Reading) ->
    ?assertEqual(null, VerifyApp),
    ?assertEqual(null, Reading).

outcome({ok, _}) -> <<"ok">>;
outcome(ok) -> <<"ok">>;
outcome({error, Reason}) -> atom_to_binary(Reason).

%% A reading as the JSON file holds it: bytes as lowercase hex, names as text, and the carried org directory
%% (already inside the record) by its sha256.
as_json(#{realm_id := RealmId, org_name := Org, app_name := App, version := Version, org_key := OrgKey,
          services := Services, org_directory := Directory}) ->
    #{<<"realm_id">> => hex(RealmId), <<"org_name">> => Org, <<"app_name">> => App, <<"version">> => Version,
      <<"org_key">> => hex(OrgKey), <<"org_directory_sha256">> => hex(crypto:hash(sha256, Directory)),
      <<"services">> => [#{<<"name">> => N, <<"procedures">> => Ps} || #{name := N, procedures := Ps} <- Services]}.

hex(Bin) -> binary:encode_hex(Bin, lowercase).

vectors() ->
    {ok, Bytes} = file:read_file(vector_file()),
    json:decode(Bytes).

%% The source tree's vector file, from the project root eunit runs in, or from the build tree's copy of the
%% application.
vector_file() ->
    Name = "app_record_v1.json",
    first_existing([filename:join("test/vectors", Name), filename:join("../../test/vectors", Name)]
                   ++ [filename:join([Dir, "..", "..", "..", "..", "test", "vectors", Name])
                       || Dir <- [code:lib_dir(macula)], is_list(Dir)]).

first_existing([F | Rest]) ->
    first_existing(filelib:is_regular(F), F, Rest).

first_existing(true, F, _Rest) -> F;
first_existing(false, _F, Rest) -> first_existing(Rest).
