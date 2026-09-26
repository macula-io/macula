%% How a call to an explicit target decides to seal (E2E design §8.1,
%% Amendment A1), from signed state only: from a verified advertisement of
%% THAT target, sealed when it names a KEM key and in the clear when it names
%% none; or `confidential => off', the application's own decision to send in
%% the clear; or `confidential => required', which resolves the target's
%% advertisement and fails closed. With none of these the call is refused:
%% `no_signed_state' (macula 13.0.0).
-module(macula_call_seal_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<5:256>>).
-define(PROC, <<"acme/echo_v1">>).

no_signed_state_is_refused_test() ->
    #{target := Target} = F = fixture(),
    ?assertEqual({error, {confidentiality, no_signed_state}}, seal(F, Target, #{})).

off_sends_in_the_clear_test() ->
    #{target := Target} = F = fixture(),
    ?assertEqual({ok, clear}, seal(F, Target, #{confidential => off})).

a_keyed_advertisement_seals_to_its_key_test() ->
    #{target := Target, kem_key := KemKey} = F = fixture(),
    ?assertEqual({ok, {sealed_to, KemKey}}, seal(F, Target, #{advertisement => keyed_ad(F)})).

a_keyless_advertisement_calls_in_the_clear_test() ->
    #{target := Target} = F = fixture(),
    ?assertEqual({ok, clear}, seal(F, Target, #{advertisement => keyless_ad(F)})).

required_refuses_a_keyless_advertisement_test() ->
    #{target := Target} = F = fixture(),
    ?assertEqual({error, {confidentiality, no_kem_key}},
                 seal(F, Target, #{advertisement => keyless_ad(F), confidential => required})).

an_advertisement_of_another_node_is_refused_test() ->
    F = fixture(),
    ?assertEqual({error, {confidentiality, not_the_target}}, seal(F, <<9:256>>, #{advertisement => keyed_ad(F)})).

an_advertisement_of_another_procedure_is_refused_test() ->
    #{target := Target} = F = fixture(),
    Other = macula_record:sign(macula_record:procedure_advertisement(Target, ?REALM, <<"acme/other_v1">>, <<1:256>>,
                                                                     #{}), maps:get(key, F)),
    ?assertEqual({error, {confidentiality, not_the_procedure}}, seal(F, Target, #{advertisement => Other})).

an_advertisement_for_another_realm_is_refused_test() ->
    #{target := Target} = F = fixture(),
    Other = macula_record:sign(macula_record:procedure_advertisement(Target, <<6:256>>, ?PROC, <<1:256>>, #{}),
                               maps:get(key, F)),
    ?assertEqual({error, {confidentiality, not_the_realm}}, seal(F, Target, #{advertisement => Other})).

%% A stale keyless advertisement of the target listed first does not hide its
%% keyed one: `required' seals to the key.
required_prefers_the_targets_keyed_advertisement_test() ->
    #{target := Target, kem_key := KemKey} = F = fixture(),
    Resolve = fun() -> {ok, [keyless_ad(F), keyed_ad(F)]} end,
    ?assertEqual({ok, {sealed_to, KemKey}},
                 macula:call_seal(Target, ?REALM, ?PROC, #{confidential => required}, Resolve)).

required_resolves_the_targets_advertisement_test() ->
    #{target := Target, kem_key := KemKey} = F = fixture(),
    Stranger = stranger_ad(),
    Resolve = fun() -> {ok, [Stranger, keyed_ad(F)]} end,
    ?assertEqual({ok, {sealed_to, KemKey}},
                 macula:call_seal(Target, ?REALM, ?PROC, #{confidential => required}, Resolve)).

required_with_no_advertisement_of_the_target_fails_closed_test() ->
    #{target := Target} = fixture(),
    Resolve = fun() -> {ok, [stranger_ad()]} end,
    ?assertEqual({error, {confidentiality, no_kem_key}},
                 macula:call_seal(Target, ?REALM, ?PROC, #{confidential => required}, Resolve)).

required_when_nothing_resolves_fails_closed_test() ->
    #{target := Target} = fixture(),
    Resolve = fun() -> {error, not_found} end,
    ?assertEqual({error, {confidentiality, no_kem_key}},
                 macula:call_seal(Target, ?REALM, ?PROC, #{confidential => required}, Resolve)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

seal(_F, Target, Opts) ->
    macula:call_seal(Target, ?REALM, ?PROC, Opts, fun() -> error(unexpected_resolve) end).

fixture() ->
    _ = application:load(macula),
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    {Public, _Private} = macula_seal:generate_key(profile()),
    #{key => Key, target => macula_node_keys:key_id(Key), kem_key => macula_seal:key_as_carried(Public)}.

keyed_ad(#{key := Key, target := Target, kem_key := KemKey}) ->
    macula_record:sign(macula_record:procedure_advertisement(Target, ?REALM, ?PROC, <<1:256>>, #{kem_key => KemKey}),
                       Key).

keyless_ad(#{key := Key, target := Target}) ->
    macula_record:sign(macula_record:procedure_advertisement(Target, ?REALM, ?PROC, <<1:256>>, #{}), Key).

stranger_ad() ->
    keyed_ad(fixture()).

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.
