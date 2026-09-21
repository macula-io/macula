%%% Tests for macula_crypto_profile: the post-quantum profile a node runs.
-module(macula_crypto_profile_tests).

-include_lib("eunit/include/eunit.hrl").

%% Key exchange groups, signature algorithms and cipher suites that no
%% profile may contain: classical-only, or post-quantum below level 5.
-define(FORBIDDEN, [x25519, x448, secp256r1, secp384r1, secp521r1,
                    x25519mlkem768, secp256r1mlkem768, mlkem512, mlkem768,
                    eddsa, ed25519, ed448, ecdsa, rsa_pkcs1v15,
                    mldsa44, mldsa65,
                    tls_aes_128_gcm_sha256, tls_chacha20_poly1305_sha256]).

%% Classical signature algorithms in BSI TR-02102-1 Table 5.3.
-define(BSI_TABLE_5_3, [rsa_pss, dsa, ecdsa, eckcdsa, ecgdsa]).

%%------------------------------------------------------------------
%% The two profiles
%%------------------------------------------------------------------

profiles_are_us_and_eu_test() ->
    ?assertEqual([pq_pure, pq_hybrid], macula_crypto_profile:profiles()).

every_profile_has_a_definition_test() ->
    [?assertMatch({ok, #{profile := P}}, macula_crypto_profile:definition(P))
     || P <- macula_crypto_profile:profiles()].

unknown_profile_has_no_definition_test() ->
    ?assertEqual({error, {crypto_profile_unknown, rsa_only}},
                 macula_crypto_profile:definition(rsa_only)).

%%------------------------------------------------------------------
%% Selecting the profile
%%------------------------------------------------------------------

missing_profile_is_refused_test() ->
    ?assertEqual({error, crypto_profile_missing},
                 macula_crypto_profile:validate(undefined)).

two_profiles_are_refused_test() ->
    Both = [pq_pure, pq_hybrid],
    ?assertEqual({error, {crypto_profile_not_single, Both}},
                 macula_crypto_profile:validate(Both)).

unknown_profile_is_refused_test() ->
    ?assertEqual({error, {crypto_profile_unknown, classical}},
                 macula_crypto_profile:validate(classical)).

known_profiles_are_accepted_test() ->
    ?assertEqual({ok, pq_pure},
                 macula_crypto_profile:validate(pq_pure)),
    ?assertEqual({ok, pq_hybrid}, macula_crypto_profile:validate(pq_hybrid)).

configured_profile_comes_from_the_application_environment_test_() ->
    {setup,
     fun save_profile_env/0,
     fun restore_profile_env/1,
     fun() ->
         ok = application:unset_env(macula, crypto_profile),
         ?assertEqual({error, crypto_profile_missing},
                      macula_crypto_profile:configured()),
         ok = application:set_env(macula, crypto_profile, pq_hybrid),
         ?assertEqual({ok, pq_hybrid}, macula_crypto_profile:configured())
     end}.

application_refuses_to_start_without_a_profile_test_() ->
    {setup,
     fun() -> {save_profile_env(), stop_macula()} end,
     fun({Saved, _}) -> restore_profile_env(Saved) end,
     fun() ->
         ok = application:unset_env(macula, crypto_profile),
         Result = application:ensure_all_started(macula),
         ?assertMatch({error, {macula, {crypto_profile_missing, _}}}, Result),
         ?assertEqual(false, lists:keymember(macula, 1,
                                             application:which_applications()))
     end}.

%%------------------------------------------------------------------
%% What the profiles contain
%%------------------------------------------------------------------

no_profile_contains_a_classical_only_or_weaker_algorithm_test() ->
    [?assertEqual([], [A || A <- algorithm_names(P), lists:member(A, ?FORBIDDEN)])
     || P <- macula_crypto_profile:profiles()].

every_post_quantum_algorithm_is_at_level_5_test() ->
    [begin
         {ok, D} = macula_crypto_profile:definition(P),
         ?assert(lists:member(maps:get(key_exchange_group, D),
                              [mlkem1024, secp384r1_mlkem1024])),
         ?assertEqual(mldsa87, maps:get(tls_signature_scheme, D)),
         [?assertEqual(mldsa87, hd(maps:get(K, D)))
          || K <- [identity_signature, connect_proof_signature]]
     end
     || P <- macula_crypto_profile:profiles()].

%% The mechanism that keeps the profile honest, rather than a note saying
%% it should be.
%%
%% Every field a profile declares must be read by something, so that a
%% profile cannot drift back into describing behaviour nobody implements.
%% `key_exchange_group' is the one deliberate exception: it declares the
%% target the EU profile aims at, nothing negotiates it, and it carries
%% that warning in `definition()'. Adding a field here without a reader,
%% or removing that warning, should turn this red.
%%
%% Five fields failed this test before it existed: `tls_cipher_suite',
%% `status_signature', `binding_digest', `content_id_digest' and
%% `node_id_digest'.
definition_declares_only_what_is_read_test() ->
    Read = [profile, tls_signature_scheme, identity_signature,
            connect_proof_signature],
    DeclaredButInert = [key_exchange_group],
    [begin
         {ok, D} = macula_crypto_profile:definition(P),
         ?assertEqual(lists:sort(Read ++ DeclaredButInert),
                      lists:sort(maps:keys(D)))
     end
     || P <- macula_crypto_profile:profiles()].

us_profile_is_post_quantum_only_test() ->
    {ok, D} = macula_crypto_profile:definition(pq_pure),
    ?assertEqual(mlkem1024, maps:get(key_exchange_group, D)),
    [?assertEqual([mldsa87], maps:get(K, D))
     || K <- [identity_signature, connect_proof_signature]].

eu_signatures_pair_mldsa87_with_a_bsi_classical_algorithm_test() ->
    {ok, D} = macula_crypto_profile:definition(pq_hybrid),
    ?assertEqual(secp384r1_mlkem1024, maps:get(key_exchange_group, D)),
    [begin
         [mldsa87, {Classical, _Params}] = maps:get(K, D),
         ?assert(lists:member(Classical, ?BSI_TABLE_5_3))
     end
     || K <- [identity_signature, connect_proof_signature]].

eu_classical_half_is_rsa_pss_4096_with_sha384_test() ->
    {ok, #{identity_signature := [mldsa87, {rsa_pss, Params}]}} =
        macula_crypto_profile:definition(pq_hybrid),
    ?assertEqual(#{modulus_bits => 4096, public_exponent => 65537,
                   digest => sha384, mgf1_digest => sha384, salt_bytes => 48},
                 Params).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

algorithm_names(Profile) ->
    {ok, D} = macula_crypto_profile:definition(Profile),
    Signatures = lists:append([maps:get(K, D) || K <- [identity_signature,
                                                       connect_proof_signature]]),
    [maps:get(key_exchange_group, D),
     maps:get(tls_signature_scheme, D)
     | [signature_name(S) || S <- Signatures]].

signature_name({Name, _Params}) -> Name;
signature_name(Name) when is_atom(Name) -> Name.

save_profile_env() ->
    application:get_env(macula, crypto_profile).

restore_profile_env({ok, Value}) ->
    application:set_env(macula, crypto_profile, Value);
restore_profile_env(undefined) ->
    application:unset_env(macula, crypto_profile).

stop_macula() ->
    _ = application:stop(macula),
    ok.
