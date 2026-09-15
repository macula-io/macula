%% EUnit tests for procedure advertisements as DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md pins them: the payload holds
%% realm_id, procedure, advertiser_node, serving_station and, for a procedure with an org namespace, the provider
%% authorization. A storing verifier checks only the record; verify_authorization/3 is the caller's check of the org
%% namespace, the authorization and its expiry. Certificate chains are in macula_record_cert_chain_tests.
-module(macula_record_advertisement_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LABEL, <<"MACULA-PQ-RECORD-V1">>).
-define(MINUTE, 60000).
-define(HOUR, 3600000).
-define(DAY, 86400000).

%%------------------------------------------------------------------
%% The pinned payload
%%------------------------------------------------------------------

the_payload_holds_realm_id_procedure_advertiser_and_serving_station_test() ->
    R = macula_record:procedure_advertisement(fill(1), realm_id(), <<"_/posts.get_page">>, fill(16#77)),
    ?assertEqual(#{{text, <<"realm_id">>} => realm_id(), {text, <<"procedure">>} => {text, <<"_/posts.get_page">>},
                   {text, <<"advertiser_node">>} => fill(1), {text, <<"serving_station">>} => fill(16#77)},
                 macula_record:payload(R)).

the_authorization_travels_inside_the_payload_test() ->
    #{adv := Adv, org_dir := OrgDir, delegation := Del} = delegation_bundle(<<"acme/get_forecast_v1">>, #{}),
    ?assertEqual(#{{text, <<"org_directory">>} => macula_record:encode(OrgDir),
                   {text, <<"procedure_delegation">>} => macula_record:encode(Del)},
                 maps:get({text, <<"authorization">>}, macula_record:payload(Adv))),
    Chain = [<<1, 2, 3>>, <<4, 5>>],
    R = macula_record:procedure_advertisement(fill(1), realm_id(), <<"acme/x">>, fill(2),
                                              #{authorization => #{certificate_chain => Chain}}),
    ?assertEqual(#{{text, <<"certificate_chain">>} => Chain},
                 maps:get({text, <<"authorization">>}, macula_record:payload(R))).

read_procedure_advertisement_returns_the_typed_payload_test() ->
    #{adv := Adv, org_dir := OrgDir, delegation := Del, advertiser := A} =
        delegation_bundle(<<"acme/get_forecast_v1">>, #{}),
    {ok, V} = verify(Adv),
    ?assertEqual(#{realm_id => realm_id(), procedure => <<"acme/get_forecast_v1">>,
                   advertiser_node => macula_node_keys:key_id(A), serving_station => fill(16#77),
                   authorization => #{org_directory => macula_record:encode(OrgDir),
                                      procedure_delegation => macula_record:encode(Del)}},
                 macula_record:read_procedure_advertisement(V)),
    Bare = macula_record:procedure_advertisement(fill(1), realm_id(), <<"echo.ping">>, fill(2)),
    ?assertEqual(undefined, maps:get(authorization, macula_record:read_procedure_advertisement(Bare))).

sign_refuses_an_advertisement_for_another_advertiser_test() ->
    A = key(identity),
    ?assertError({key_id_mismatch, _},
                 macula_record:sign(macula_record:procedure_advertisement(fill(9), realm_id(), <<"x.y">>, fill(2)), A)).

%%------------------------------------------------------------------
%% A storing verifier checks the record, never the authorization
%%------------------------------------------------------------------

a_storing_verifier_never_parses_the_authorization_test() ->
    A = key(identity),
    Opaque = #{authorization => #{org_directory => <<"not a record">>, procedure_delegation => <<>>}},
    R = macula_record:procedure_advertisement(macula_node_keys:key_id(A), realm_id(), <<"_/x">>, fill(2), Opaque),
    ?assertMatch({ok, _}, verify(macula_record:sign(R, A))).

advertisement_payloads_the_design_does_not_allow_are_malformed_test() ->
    A = key(identity),
    Good = #{{text, <<"realm_id">>} => realm_id(), {text, <<"procedure">>} => {text, <<"acme/x">>},
             {text, <<"advertiser_node">>} => macula_node_keys:key_id(A), {text, <<"serving_station">>} => fill(2)},
    Verify = fun(Payload) -> macula_record:verify(hand_signed(Payload, A), pq_pure) end,
    ?assertMatch({ok, _}, Verify(Good)),
    ?assertMatch({ok, _}, Verify(Good#{{text, <<"authorization">>} => #{{text, <<"anything">>} => 1}})),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"session_token_hint">>} => {text, <<"h">>}})),
    ?assertEqual({error, malformed}, Verify(maps:remove({text, <<"serving_station">>}, Good))),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"realm_id">>} := <<1:248>>})),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"procedure">>} := <<"acme/x">>})),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"authorization">>} => [1]})),
    ?assertEqual({error, key_id_mismatch}, Verify(Good#{{text, <<"advertiser_node">>} := fill(9)})).

%%------------------------------------------------------------------
%% The org namespace
%%------------------------------------------------------------------

the_org_namespace_is_the_text_before_the_first_slash_test() ->
    ?assertEqual({org, <<"acme">>}, macula_record:procedure_org(<<"acme/get_forecast_v1">>)),
    ?assertEqual({org, <<"acme">>}, macula_record:procedure_org(<<"acme/forecasts/get_v1">>)),
    ?assertEqual(none, macula_record:procedure_org(<<"_/posts.get_page">>)),
    ?assertEqual(none, macula_record:procedure_org(<<"echo.ping">>)),
    ?assertEqual({error, malformed}, macula_record:procedure_org(<<"/get_v1">>)).

%%------------------------------------------------------------------
%% verify_authorization/3: the org directory and the delegation
%%------------------------------------------------------------------

a_valid_delegation_authorizes_the_advertisement_test() ->
    #{adv := Adv, realm := Realm} = delegation_bundle(<<"acme/get_forecast_v1">>, #{}),
    ?assertEqual(ok, authorize(Adv, Realm)).

an_org_directory_from_another_realm_key_is_refused_test() ->
    #{adv := Adv, realm := Realm} = delegation_bundle(<<"acme/get_forecast_v1">>, #{dir_signer => key(realm)}),
    ?assertEqual({error, org_directory_wrong_realm}, authorize(Adv, Realm)).

an_org_directory_for_another_realm_id_is_refused_test() ->
    #{adv := Adv, realm := Realm} = delegation_bundle(<<"acme/get_forecast_v1">>, #{dir_realm_id => fill(16#12)}),
    ?assertEqual({error, org_directory_wrong_realm}, authorize(Adv, Realm)).

an_org_directory_for_another_org_is_refused_test() ->
    #{adv := Adv, realm := Realm} = delegation_bundle(<<"acme/get_forecast_v1">>, #{dir_org => <<"acmecorp">>}),
    ?assertEqual({error, org_directory_wrong_org}, authorize(Adv, Realm)).

a_delegation_from_another_org_key_is_refused_test() ->
    #{adv := Adv, realm := Realm} = delegation_bundle(<<"acme/get_forecast_v1">>, #{del_signer => key(org)}),
    ?assertEqual({error, delegation_mismatch}, authorize(Adv, Realm)).

a_delegation_for_another_advertiser_is_refused_test() ->
    #{adv := Adv, realm := Realm} = delegation_bundle(<<"acme/get_forecast_v1">>, #{del_advertiser => fill(9)}),
    ?assertEqual({error, delegation_mismatch}, authorize(Adv, Realm)).

a_corrupted_org_directory_is_refused_test() ->
    #{adv := Adv, realm := Realm} = delegation_bundle(<<"acme/get_forecast_v1">>, #{corrupt_dir => true}),
    ?assertEqual({error, org_directory_invalid}, authorize(Adv, Realm)).

an_expired_delegation_is_refused_test() ->
    #{adv := Adv, realm := Realm} = delegation_bundle(<<"acme/get_forecast_v1">>, #{del_ttl => ?MINUTE}),
    ?assertEqual({error, delegation_invalid}, authorize(Adv, Realm, now_ms() + 10 * ?MINUTE)).

an_advertisement_that_outlives_its_authorization_is_refused_test() ->
    #{adv := Adv, realm := Realm} = delegation_bundle(<<"acme/get_forecast_v1">>, #{del_ttl => 2 * ?MINUTE}),
    ?assertEqual({error, authorization_outlived}, authorize(Adv, Realm)).

the_delegation_form_needs_the_realm_key_test() ->
    #{adv := Adv} = delegation_bundle(<<"acme/get_forecast_v1">>, #{}),
    ?assertEqual({error, no_realm_key},
                 macula_record:verify_authorization(verified(Adv), #{profile => pq_pure}, now_ms())).

%%------------------------------------------------------------------
%% verify_authorization/3: presence follows the org namespace
%%------------------------------------------------------------------

a_procedure_with_an_org_namespace_needs_an_authorization_test() ->
    A = key(identity),
    R = macula_record:procedure_advertisement(macula_node_keys:key_id(A), realm_id(), <<"acme/x">>, fill(2)),
    ?assertEqual({error, no_authorization}, authorize(macula_record:sign(R, A), key(realm))).

a_procedure_without_an_org_namespace_needs_none_test() ->
    A = key(identity),
    [?assertEqual(ok, authorize(macula_record:sign(macula_record:procedure_advertisement(
                                    macula_node_keys:key_id(A), realm_id(), Procedure, fill(2)), A), key(realm)))
     || Procedure <- [<<"_/posts.get_page">>, <<"echo.ping">>]].

a_procedure_without_an_org_namespace_refuses_an_authorization_test() ->
    #{adv := Adv, realm := Realm} = delegation_bundle(<<"_/posts.get_page">>, #{}),
    ?assertEqual({error, authorization_not_allowed}, authorize(Adv, Realm)).

a_procedure_name_starting_with_a_slash_is_malformed_test() ->
    #{adv := Adv, realm := Realm} = delegation_bundle(<<"/get_forecast_v1">>, #{}),
    ?assertEqual({error, malformed}, authorize(Adv, Realm)).

an_authorization_in_neither_form_is_malformed_test() ->
    A = key(identity),
    #{org_dir := OrgDir, delegation := Del, realm := Realm} = delegation_bundle(<<"acme/x">>, #{}),
    Both = #{org_directory => macula_record:encode(OrgDir), procedure_delegation => macula_record:encode(Del),
             certificate_chain => [<<1>>]},
    [?assertEqual({error, malformed},
                  authorize(macula_record:sign(macula_record:procedure_advertisement(
                      macula_node_keys:key_id(A), realm_id(), <<"acme/x">>, fill(2), #{authorization => Auth}), A),
                      Realm))
     || Auth <- [Both, #{org_directory => macula_record:encode(OrgDir)}, #{certificate_chain => []}]].

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% A realm, an org and a provider, with the realm-signed org directory, the org-signed delegation and the provider's
%% advertisement carrying both. Overrides break one link at a time.
delegation_bundle(Procedure, Overrides) ->
    Realm = key(realm),
    Org = key(org),
    A = key(identity),
    DirSigner = maps:get(dir_signer, Overrides, Realm),
    DelSigner = maps:get(del_signer, Overrides, Org),
    DirOrg = maps:get(dir_org, Overrides, <<"acme">>),
    OrgDir = macula_record:sign(macula_record:org_directory(maps:get(dir_realm_id, Overrides, realm_id()), DirOrg,
                                                            macula_node_keys:key_id(Org)), DirSigner),
    Del = macula_record:sign(macula_record:procedure_delegation(
                                 macula_node_keys:key_id(DelSigner),
                                 maps:get(del_advertiser, Overrides, macula_node_keys:key_id(A)),
                                 #{ttl_ms => maps:get(del_ttl, Overrides, 6 * ?HOUR)}), DelSigner),
    DirBytes = corrupt(maps:get(corrupt_dir, Overrides, false), macula_record:encode(OrgDir)),
    Authorization = #{org_directory => DirBytes, procedure_delegation => macula_record:encode(Del)},
    Adv = macula_record:sign(macula_record:procedure_advertisement(
                                 macula_node_keys:key_id(A), realm_id(), Procedure, fill(16#77),
                                 #{authorization => Authorization, ttl_ms => 300_000}), A),
    #{realm => Realm, org => Org, advertiser => A, org_dir => OrgDir, delegation => Del, adv => Adv}.

authorize(Adv, Realm) ->
    authorize(Adv, Realm, now_ms()).

authorize(Adv, Realm, Now) ->
    macula_record:verify_authorization(verified(Adv),
                                       #{profile => pq_pure, realm_key => macula_node_keys:public_key(Realm)}, Now).

verified(Record) ->
    {ok, V} = verify(Record),
    V.

verify(Record) ->
    macula_record:verify(macula_record:encode(Record), pq_pure).

%% Flip one byte inside the encoded record, so its signature no longer verifies.
corrupt(false, Bytes) ->
    Bytes;
corrupt(true, Bytes) ->
    Offset = byte_size(Bytes) - 5000,
    <<Head:Offset/binary, Byte, Tail/binary>> = Bytes,
    <<Head/binary, (Byte bxor 1), Tail/binary>>.

hand_signed(Payload, Key) ->
    Now = now_ms(),
    Fields = #{{text, <<"type">>} => 16#06, {text, <<"version">>} => macula_record_uuid:v7_monotonic(Now),
               {text, <<"created_at">>} => Now, {text, <<"expires_at">>} => Now + 5 * ?MINUTE,
               {text, <<"payload">>} => Payload},
    macula_signed_object:sign(?LABEL, Fields, Key).

key(Purpose) ->
    {ok, Key} = macula_node_keys:generate(Purpose, pq_pure),
    Key.

realm_id() ->
    fill(16#11).

now_ms() ->
    erlang:system_time(millisecond).

fill(Byte) ->
    binary:copy(<<Byte>>, 32).
