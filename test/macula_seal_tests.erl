%% `macula_seal' on fresh keys and fresh randomness: what the fixed vectors
%% (macula_seal_vectors_tests) cannot show. A sender's secret reaches only
%% the recipient it was made for, and a malformed or tampered `kem_ct' is
%% refused.
-module(macula_seal_tests).

-include_lib("eunit/include/eunit.hrl").

a_sender_and_its_recipient_agree_in_each_profile_test() ->
    [begin
         {Public, Private} = kem_keys(Profile),
         {Secret, KemCt} = macula_seal:sender_secret(Profile, Public),
         ?assertEqual({ok, Secret},
                      macula_seal:recipient_secret(Profile, Private, macula_seal:key_as_carried(Public), KemCt))
     end || Profile <- [pq_pure, pq_hybrid]].

two_encapsulations_to_one_key_differ_test() ->
    {Public, _Private} = kem_keys(pq_hybrid),
    {S1, C1} = macula_seal:sender_secret(pq_hybrid, Public),
    {S2, C2} = macula_seal:sender_secret(pq_hybrid, Public),
    ?assertNotEqual(S1, S2),
    ?assertNotEqual(C1, C2).

%% Another recipient's private key recovers some secret (ML-KEM's implicit
%% rejection), never the sender's.
another_recipient_does_not_recover_the_secret_test() ->
    {Public, _} = kem_keys(pq_pure),
    {OtherPublic, OtherPrivate} = kem_keys(pq_pure),
    {Secret, KemCt} = macula_seal:sender_secret(pq_pure, Public),
    {ok, Recovered} = macula_seal:recipient_secret(pq_pure, OtherPrivate, macula_seal:key_as_carried(OtherPublic), KemCt),
    ?assertNotEqual(Secret, Recovered).

%% The combiner binds the recipient's key: the right private key, told the
%% wrong carried key, recovers a different secret.
the_secret_binds_the_recipients_key_test() ->
    {Public, Private} = kem_keys(pq_hybrid),
    {Secret, KemCt} = macula_seal:sender_secret(pq_hybrid, Public),
    {ok, Other} = macula_seal:recipient_secret(pq_hybrid, Private, <<"another key">>, KemCt),
    ?assertNotEqual(Secret, Other).

a_malformed_kem_ct_is_refused_test() ->
    {Public, Private} = kem_keys(pq_hybrid),
    Carried = macula_seal:key_as_carried(Public),
    {_Secret, <<MlkemCt:1568/binary, _EphPub:97/binary>> = KemCt} = macula_seal:sender_secret(pq_hybrid, Public),
    [?assertEqual({error, sealed_refused}, macula_seal:recipient_secret(pq_hybrid, Private, Carried, Bad))
     || Bad <- [<<>>, MlkemCt, binary:part(KemCt, 0, byte_size(KemCt) - 1),
                <<MlkemCt/binary, 4, 0:768>>,             % not a point on P-384
                <<MlkemCt/binary, 2, 0:768>>]],           % not an uncompressed point
    {PurePublic, PurePrivate} = kem_keys(pq_pure),
    ?assertEqual({error, sealed_refused},
                 macula_seal:recipient_secret(pq_pure, PurePrivate, macula_seal:key_as_carried(PurePublic), KemCt)).

a_stream_nonce_is_its_seq_and_stays_in_range_test() ->
    ?assertEqual(<<0:64, 1:32>>, macula_seal:stream_nonce(1)),
    ?assertError(function_clause, macula_seal:stream_nonce(1 bsl 64)),
    ?assertNotEqual(macula_seal:random_nonce(), macula_seal:random_nonce()).

a_sealed_payload_opens_only_as_sealed_test() ->
    Key = crypto:strong_rand_bytes(32),
    Nonce = macula_seal:random_nonce(),
    Sealed = macula_seal:seal(Key, Nonce, <<"aad">>, <<"payload">>),
    ?assertEqual({ok, <<"payload">>}, macula_seal:open(Key, Nonce, <<"aad">>, Sealed)),
    ?assertEqual({error, sealed_refused}, macula_seal:open(crypto:strong_rand_bytes(32), Nonce, <<"aad">>, Sealed)),
    ?assertEqual({error, sealed_refused}, macula_seal:open(Key, macula_seal:random_nonce(), <<"aad">>, Sealed)),
    ?assertEqual({error, sealed_refused}, macula_seal:open(Key, Nonce, <<"aad">>, <<"short">>)).

%%%===================================================================
%%% Helpers
%%%===================================================================

kem_keys(pq_pure) ->
    {Ek, Dk} = crypto:generate_key(mlkem1024, []),
    {#{mlkem_ek => Ek}, #{mlkem_dk => Dk}};
kem_keys(pq_hybrid) ->
    {Ek, Dk} = crypto:generate_key(mlkem1024, []),
    {P384Pub, P384Priv} = crypto:generate_key(ecdh, secp384r1),
    {#{mlkem_ek => Ek, p384_pub => P384Pub}, #{mlkem_dk => Dk, p384_priv => P384Priv}}.
