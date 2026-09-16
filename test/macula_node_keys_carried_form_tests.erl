%% EUnit tests for the carried form of node keys (D13), which the handshake checks before it uses a key: exactly one
%% encoding per profile, and the signature size per profile.
-module(macula_node_keys_carried_form_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

%% RSA key generation takes up to about a second per key.
-define(EU_TIMEOUT, 120).

pq_pure_carried_key_is_exactly_an_mldsa87_key_test() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Public = macula_node_keys:public_key(Key),
    ?assert(macula_node_keys:carried_key_well_formed(Public, pq_pure)),
    ?assertNot(macula_node_keys:carried_key_well_formed(binary:part(Public, 0, 2591), pq_pure)),
    ?assertNot(macula_node_keys:carried_key_well_formed(<<Public/binary, 0>>, pq_pure)).

pq_hybrid_carried_key_is_mldsa87_then_a_canonical_rsa_4096_key_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Key} = macula_node_keys:generate(identity, pq_hybrid),
        Public = macula_node_keys:public_key(Key),
        <<MlDsa:2592/binary, Der/binary>> = Public,
        ?assert(macula_node_keys:carried_key_well_formed(Public, pq_hybrid)),
        ?assertNot(macula_node_keys:carried_key_well_formed(Public, pq_pure)),
        ?assertNot(macula_node_keys:carried_key_well_formed(MlDsa, pq_hybrid)),
        ?assertNot(macula_node_keys:carried_key_well_formed(<<MlDsa/binary, (long_form_length(Der))/binary>>, pq_hybrid)),
        ?assertNot(macula_node_keys:carried_key_well_formed(<<MlDsa/binary, (rsa_der(3072, 65537))/binary>>, pq_hybrid)),
        ?assertNot(macula_node_keys:carried_key_well_formed(<<MlDsa/binary, (rsa_der(4096, 3))/binary>>, pq_hybrid))
    end}.

signature_bytes_match_real_signatures_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        ?assertEqual({4627, 5139},
                     {macula_node_keys:signature_bytes(pq_pure), macula_node_keys:signature_bytes(pq_hybrid)}),
        [assert_signature_size(Profile) || Profile <- [pq_pure, pq_hybrid]]
    end}.

assert_signature_size(Profile) ->
    {ok, Key} = macula_node_keys:generate(connect, Profile),
    ?assertEqual(macula_node_keys:signature_bytes(Profile), byte_size(macula_node_keys:sign(<<"message">>, Key))).

%% The same SEQUENCE with its length in a longer form than DER allows.
long_form_length(<<16#30, 16#82, Length:16, Body/binary>>) ->
    <<16#30, 16#83, 0, Length:16, Body/binary>>.

rsa_der(Bits, Exponent) ->
    {[E, N], _Private} = crypto:generate_key(rsa, {Bits, Exponent}),
    public_key:der_encode('RSAPublicKey', #'RSAPublicKey'{modulus = binary:decode_unsigned(N),
                                                          publicExponent = binary:decode_unsigned(E)}).
