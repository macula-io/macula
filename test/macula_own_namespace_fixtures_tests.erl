%% The own-namespace fixtures every SDK shares (test/fixtures/own_namespace/, README there): each signed
%% advertisement decodes and verifies under its profile at the recorded time, and macula reaches the recorded
%% verdicts, from `own_namespace/1' and from `verify_authorization/3' with no realm key. macula-go runs the same
%% files, so the two SDKs cannot drift on who may serve under `~<node_id>'.
-module(macula_own_namespace_fixtures_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DIR, "test/fixtures/own_namespace").

every_fixture_reaches_its_verdict_test_() ->
    Entries = entries(),
    [{binary_to_list(maps:get(<<"file">>, E)), fun() -> reaches_its_verdict(E) end} || E <- Entries].

%% Every case the rule distinguishes is there, under both profiles, so a regenerated set cannot quietly lose one.
the_fixtures_cover_every_verdict_test() ->
    Entries = entries(),
    ?assertEqual([<<"pq_hybrid">>, <<"pq_pure">>], lists:usort([maps:get(<<"profile">>, E) || E <- Entries])),
    ?assertEqual([<<"authorization_not_allowed">>, <<"malformed">>, <<"not_own_namespace">>, <<"ok">>],
                 lists:usort([maps:get(<<"own_namespace">>, E) || E <- Entries])).

reaches_its_verdict(#{<<"file">> := File, <<"profile">> := ProfileName, <<"now_ms">> := Now,
                      <<"procedure">> := Procedure,
                      <<"own_namespace">> := Own, <<"verify_authorization">> := Verified}) ->
    Profile = binary_to_existing_atom(ProfileName),
    {ok, Bytes} = file:read_file(filename:join(?DIR, File)),
    {ok, Record} = macula_record:verify(Bytes, Profile, Now),
    ?assertEqual(Procedure, maps:get(procedure, macula_record:read_procedure_advertisement(Record))),
    ?assertEqual(Own, verdict(macula_record:own_namespace(Record))),
    ?assertEqual(Verified, verdict(macula_record:verify_authorization(Record, #{profile => Profile}, Now))).

verdict(ok) -> <<"ok">>;
verdict({error, Reason}) -> atom_to_binary(Reason).

entries() ->
    {ok, Json} = file:read_file(filename:join(?DIR, "verdicts.json")),
    json:decode(Json).
