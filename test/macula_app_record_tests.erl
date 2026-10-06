%% EUnit tests for the app record (macula#75): an org's signed statement of one app, keyed by the app's MRI, carrying
%% the realm-signed org directory that authorizes the org. verify_app/3 is the caller's check, pure: the carried
%% directory is signed by the realm key, names the record's realm and org, holds the record's org key, and the record
%% expires no later than the directory. The shape rules (an org namespace, procedures in it only, bounded text) live in
%% the payload check, so sign/2 and verify/3 refuse the same records.
-module(macula_app_record_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MINUTE, 60000).
-define(HOUR, 3600000).

%%------------------------------------------------------------------
%% The record
%%------------------------------------------------------------------

an_app_record_is_stored_under_its_mri_test() ->
    #{app := App} = bundle(#{}),
    ?assertEqual(macula_record:app_key(realm_id(), <<"acme">>, <<"weather">>), macula_record:storage_key(App)),
    ?assertNotEqual(macula_record:app_key(realm_id(), <<"acme">>, <<"tides">>), macula_record:storage_key(App)).

read_app_record_returns_the_typed_payload_test() ->
    #{app := App, org := Org, org_dir := OrgDir} = bundle(#{}),
    ?assertEqual(#{realm_id => realm_id(), org_name => <<"acme">>, app_name => <<"weather">>,
                   version => <<"1.4.0">>, org_key => macula_node_keys:key_id(Org),
                   services => [#{name => <<"forecast">>,
                                  procedures => [<<"acme/get_forecast_v1">>, <<"acme/watch_forecast_v1">>]},
                                #{name => <<"alerts">>, procedures => []}],
                   org_directory => macula_record:encode(OrgDir)},
                 macula_record:read_app_record(verified(App))).

the_signer_must_be_the_org_key_it_names_test() ->
    Org = key(org),
    Unsigned = app_record(macula_node_keys:key_id(Org), <<"acme">>, services(), dir_bytes(Org, <<"acme">>), #{}),
    ?assertError({key_id_mismatch, _}, macula_record:sign(Unsigned, key(org))).

only_an_org_key_signs_an_app_record_test() ->
    Realm = key(realm),
    Unsigned = app_record(macula_node_keys:key_id(Realm), <<"acme">>, services(), dir_bytes(Realm, <<"acme">>), #{}),
    ?assertError({key_purpose_mismatch, _}, macula_record:sign(Unsigned, Realm)).

an_app_record_lives_at_most_thirty_minutes_test() ->
    Org = key(org),
    Unsigned = app_record(macula_node_keys:key_id(Org), <<"acme">>, services(), dir_bytes(Org, <<"acme">>),
                          #{ttl_ms => 31 * ?MINUTE}),
    ?assertError({lifetime_too_long, _}, macula_record:sign(Unsigned, Org)).

%% An app belongs to an org: no app in the unnamespaced `_' or in a node's own `~<node_id>' namespace.
an_app_outside_an_org_namespace_is_malformed_test() ->
    Org = key(org),
    [?assertError({malformed, _},
                  macula_record:sign(app_record(macula_node_keys:key_id(Org), OrgName, [], dir_bytes(Org, OrgName), #{}),
                                     Org))
     || OrgName <- [<<"_">>, <<"~", (binary:encode_hex(fill(1), lowercase))/binary>>, <<>>, <<"ac/me">>]].

%% THE BINDING: an org lists only procedures in its own namespace, or it could present another org's providers, or a
%% bare node's, as part of its app.
a_procedure_outside_the_orgs_namespace_is_malformed_test() ->
    Org = key(org),
    [?assertError({malformed, _},
                  macula_record:sign(app_record(macula_node_keys:key_id(Org), <<"acme">>,
                                                [#{name => <<"s">>, procedures => [Procedure]}],
                                                dir_bytes(Org, <<"acme">>), #{}), Org))
     || Procedure <- [<<"globex/get_forecast_v1">>, <<"_/x">>, <<"get_forecast_v1">>, <<"/x">>,
                      <<"~", (binary:encode_hex(fill(1), lowercase))/binary, "/x">>]].

unbounded_or_empty_text_is_malformed_test() ->
    Org = key(org),
    Dir = dir_bytes(Org, <<"acme">>),
    Long = binary:copy(<<"x">>, 65),
    [?assertError({malformed, _}, macula_record:sign(Unsigned, Org))
     || Unsigned <- [app_record(macula_node_keys:key_id(Org), <<"acme">>, services(), Dir, #{version => Long}),
                     app_record(macula_node_keys:key_id(Org), <<"acme">>, services(), Dir, #{version => <<>>}),
                     app_record(macula_node_keys:key_id(Org), <<"acme">>, [#{name => Long, procedures => []}], Dir, #{}),
                     app_record(macula_node_keys:key_id(Org), <<"acme">>, [#{name => <<>>, procedures => []}], Dir, #{})]].

two_services_of_one_name_are_malformed_test() ->
    Org = key(org),
    Twice = [#{name => <<"forecast">>, procedures => []}, #{name => <<"forecast">>, procedures => []}],
    ?assertError({malformed, _},
                 macula_record:sign(app_record(macula_node_keys:key_id(Org), <<"acme">>, Twice,
                                               dir_bytes(Org, <<"acme">>), #{}), Org)).

%% A verifier holds the same rules: a record that skipped the builder is refused at verify as at sign.
a_hand_built_foreign_procedure_is_refused_at_verify_test() ->
    Org = key(org),
    Good = app_record(macula_node_keys:key_id(Org), <<"acme">>, services(), dir_bytes(Org, <<"acme">>), #{}),
    Payload = (macula_record:payload(Good))#{{text, <<"services">>} =>
                                                 [#{{text, <<"name">>} => {text, <<"s">>},
                                                    {text, <<"procedures">>} => [{text, <<"globex/x">>}]}]},
    Fields = #{{text, <<"type">>} => 16#17, {text, <<"version">>} => macula_record:version(Good),
               {text, <<"created_at">>} => macula_record:created_at(Good),
               {text, <<"expires_at">>} => macula_record:expires_at(Good), {text, <<"payload">>} => Payload},
    Signed = macula_signed_object:sign(<<"MACULA-PQ-RECORD-V1">>, Fields, Org),
    ?assertEqual({error, malformed}, macula_record:verify(macula_signed_object:encode(Signed), pq_pure)).

%%------------------------------------------------------------------
%% verify_app/3: the carried org directory
%%------------------------------------------------------------------

a_valid_app_record_is_authorized_test() ->
    #{app := App, realm := Realm} = bundle(#{}),
    ?assertEqual(ok, authorize(App, Realm)).

a_valid_app_record_is_authorized_by_the_trust_lists_pairs_test() ->
    #{app := App, realm := Realm} = bundle(#{}),
    ?assertEqual(ok, macula_record:verify_app(verified(App),
                                              #{profile => pq_pure,
                                                realm_pairs => #{realm_id() => macula_node_keys:key_id(Realm)}},
                                              now_ms())).

an_org_directory_from_another_realm_key_is_refused_test() ->
    #{app := App, realm := Realm} = bundle(#{dir_signer => key(realm)}),
    ?assertEqual({error, org_directory_wrong_realm}, authorize(App, Realm)).

an_org_directory_for_another_realm_id_is_refused_test() ->
    #{app := App, realm := Realm} = bundle(#{dir_realm_id => fill(16#12)}),
    ?assertEqual({error, org_directory_wrong_realm}, authorize(App, Realm)).

an_org_directory_for_another_org_is_refused_test() ->
    #{app := App, realm := Realm} = bundle(#{dir_org => <<"acmecorp">>}),
    ?assertEqual({error, org_directory_wrong_org}, authorize(App, Realm)).

%% The org key the realm vouches for is not the key that signed the app.
an_app_signed_by_another_org_key_is_refused_test() ->
    #{app := App, realm := Realm} = bundle(#{dir_org_key => key(org)}),
    ?assertEqual({error, org_key_mismatch}, authorize(App, Realm)).

a_corrupted_org_directory_is_refused_test() ->
    #{app := App, realm := Realm} = bundle(#{corrupt_dir => true}),
    ?assertEqual({error, org_directory_invalid}, authorize(App, Realm)).

an_app_record_that_outlives_its_org_directory_is_refused_test() ->
    #{app := App, realm := Realm} = bundle(#{dir_ttl => 2 * ?MINUTE}),
    ?assertEqual({error, authorization_outlived}, authorize(App, Realm)).

an_app_record_needs_the_realm_key_test() ->
    #{app := App} = bundle(#{}),
    ?assertEqual({error, no_realm_key}, macula_record:verify_app(verified(App), #{profile => pq_pure}, now_ms())).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% A realm and an org, the realm-signed org directory and the org-signed app record carrying it. Overrides break one
%% link at a time.
bundle(Overrides) ->
    Realm = key(realm),
    Org = key(org),
    DirSigner = maps:get(dir_signer, Overrides, Realm),
    DirOrgKey = maps:get(dir_org_key, Overrides, Org),
    OrgDir = macula_record:sign(macula_record:org_directory(maps:get(dir_realm_id, Overrides, realm_id()),
                                                            maps:get(dir_org, Overrides, <<"acme">>),
                                                            macula_node_keys:key_id(DirOrgKey),
                                                            #{ttl_ms => maps:get(dir_ttl, Overrides, 6 * ?HOUR)}),
                                DirSigner),
    DirBytes = corrupt(maps:get(corrupt_dir, Overrides, false), macula_record:encode(OrgDir)),
    App = macula_record:sign(app_record(macula_node_keys:key_id(Org), <<"acme">>, services(), DirBytes, #{}), Org),
    #{realm => Realm, org => Org, org_dir => OrgDir, app => App}.

app_record(OrgKeyId, OrgName, Services, DirBytes, Opts) ->
    macula_record:app_record(OrgKeyId, realm_id(), OrgName, <<"weather">>,
                             maps:merge(#{version => <<"1.4.0">>, services => Services, org_directory => DirBytes,
                                          ttl_ms => 30 * ?MINUTE},
                                        Opts)).

services() ->
    [#{name => <<"forecast">>, procedures => [<<"acme/get_forecast_v1">>, <<"acme/watch_forecast_v1">>]},
     #{name => <<"alerts">>, procedures => []}].

dir_bytes(Signer, OrgName) ->
    macula_record:encode(macula_record:sign(macula_record:org_directory(realm_id(), OrgName,
                                                                        macula_node_keys:key_id(Signer)),
                                            key(realm))).

authorize(App, Realm) ->
    macula_record:verify_app(verified(App), #{profile => pq_pure, realm_key => macula_node_keys:public_key(Realm)},
                             now_ms()).

verified(Record) ->
    {ok, V} = macula_record:verify(macula_record:encode(Record), pq_pure),
    V.

%% Flip one byte inside the encoded record, so its signature no longer verifies.
corrupt(false, Bytes) ->
    Bytes;
corrupt(true, Bytes) ->
    Offset = byte_size(Bytes) - 5000,
    <<Head:Offset/binary, Byte, Tail/binary>> = Bytes,
    <<Head/binary, (Byte bxor 1), Tail/binary>>.

key(Purpose) ->
    {ok, Key} = macula_node_keys:generate(Purpose, pq_pure),
    Key.

realm_id() ->
    fill(16#11).

fill(Byte) ->
    binary:copy(<<Byte>>, 32).

now_ms() ->
    erlang:system_time(millisecond).
