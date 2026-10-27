%% How a call to an explicit target decides to seal (E2E design §8.1,
%% Amendment A1), from signed state only: from a verified advertisement of
%% THAT target, sealed when it names a KEM key and in the clear when it names
%% none; or `confidential => off', the application's own decision to send in
%% the clear; or `confidential => required', which resolves the target's
%% advertisement and fails closed. With none of these the call is refused:
%% `no_signed_state' (macula 13.0.0). A KEM key a link learnt from the
%% provider's own signed `sealed_required' refusal seals a call whose
%% advertisement names none (DESIGN_E2E_SEALED_BY_DEFAULT §3, release A0).
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
    Other = macula_record:sign(macula_record:procedure_advertisement(Target, ?REALM, <<"acme/other_v1">>, [<<1:256>>],
                                                                     #{}), maps:get(key, F)),
    ?assertEqual({error, {confidentiality, not_the_procedure}}, seal(F, Target, #{advertisement => Other})).

an_advertisement_for_another_realm_is_refused_test() ->
    #{target := Target} = F = fixture(),
    Other = macula_record:sign(macula_record:procedure_advertisement(Target, <<6:256>>, ?PROC, [<<1:256>>], #{}),
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

%% A key a link learnt from the provider's own signed `sealed_required' refusal seals a call whose advertisement names
%% none (DESIGN_E2E_SEALED_BY_DEFAULT §3, release A0): the advertisement may be keyless and still inside its lifetime
%% while the provider has since begun naming a key.
a_learnt_key_seals_a_keyless_advertisement_test() ->
    #{target := Target, kem_key := KemKey} = F = fixture(),
    ?assertEqual({ok, {sealed_to, KemKey}},
                 seal(F, Target, #{advertisement => keyless_ad(F), learnt_kem_key => KemKey})).

%% The advertisement's own key wins: it is signed state of the same provider, read after the refusal was.
the_advertisements_own_key_wins_over_a_learnt_one_test() ->
    #{target := Target, kem_key := KemKey} = F = fixture(),
    {Public, _} = macula_seal:generate_key(profile()),
    ?assertEqual({ok, {sealed_to, KemKey}},
                 seal(F, Target, #{advertisement => keyed_ad(F), learnt_kem_key => macula_seal:key_as_carried(Public)})).

%% A learnt key of another profile's size never fails a call that would go clear: it is ignored, and the keyless
%% advertisement's answer stands.
a_learnt_key_of_another_profile_is_ignored_test() ->
    #{target := Target} = F = fixture(),
    {Public, _} = macula_seal:generate_key(other_profile(profile())),
    ?assertEqual({ok, clear},
                 seal(F, Target, #{advertisement => keyless_ad(F),
                                   learnt_kem_key => macula_seal:key_as_carried(Public)})).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

seal(_F, Target, Opts) ->
    macula:call_seal(Target, ?REALM, ?PROC, Opts, fun() -> error(unexpected_resolve) end).

other_profile(pq_pure) -> pq_hybrid;
other_profile(pq_hybrid) -> pq_pure.

fixture() ->
    _ = application:load(macula),
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    {Public, _Private} = macula_seal:generate_key(profile()),
    #{key => Key, target => macula_node_keys:key_id(Key), kem_key => macula_seal:key_as_carried(Public)}.

keyed_ad(#{key := Key, target := Target, kem_key := KemKey}) ->
    macula_record:sign(macula_record:procedure_advertisement(Target, ?REALM, ?PROC, [<<1:256>>], #{kem_key => KemKey}),
                       Key).

keyless_ad(#{key := Key, target := Target}) ->
    macula_record:sign(macula_record:procedure_advertisement(Target, ?REALM, ?PROC, [<<1:256>>], #{}), Key).

stranger_ad() ->
    keyed_ad(fixture()).

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.
