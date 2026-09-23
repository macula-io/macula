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

%% The RSA half of a carried pq_hybrid key is well formed only as DER in its ONE canonical encoding, of a modulus of
%% exactly 4096 bits and exponent 65537. These pin the edges a cheaper check could get wrong: an INTEGER with a
%% redundant leading zero byte (valid BER, not DER), and moduli one bit either side of 4096.
an_rsa_modulus_with_a_redundant_leading_zero_is_refused_test() ->
    MlDsa = binary:copy(<<0>>, 2592),
    N = (1 bsl 4095) + 1,
    Canonical = rsa_der_of(N, 65537),
    ?assert(macula_node_keys:carried_key_well_formed(<<MlDsa/binary, Canonical/binary>>, pq_hybrid)),
    ?assertNot(macula_node_keys:carried_key_well_formed(<<MlDsa/binary, (padded_modulus(N, 65537))/binary>>,
                                                       pq_hybrid)).

a_modulus_one_bit_short_or_long_is_refused_test() ->
    MlDsa = binary:copy(<<0>>, 2592),
    Carried = fun(N) -> <<MlDsa/binary, (rsa_der_of(N, 65537))/binary>> end,
    ?assert(macula_node_keys:carried_key_well_formed(Carried((1 bsl 4095) + 1), pq_hybrid)),
    ?assert(macula_node_keys:carried_key_well_formed(Carried((1 bsl 4096) - 1), pq_hybrid)),
    ?assertNot(macula_node_keys:carried_key_well_formed(Carried((1 bsl 4094) + 1), pq_hybrid)),
    ?assertNot(macula_node_keys:carried_key_well_formed(Carried((1 bsl 4096) + 1), pq_hybrid)).

%% A verify rests on the same checks: a signature under a carried key whose modulus is re-encoded with a redundant
%% leading zero is refused, although the key is numerically the one that signed.
a_signature_under_a_non_canonical_rsa_encoding_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Key} = macula_node_keys:generate(identity, pq_hybrid),
        Public = macula_node_keys:public_key(Key),
        <<MlDsa:2592/binary, Der/binary>> = Public,
        #'RSAPublicKey'{modulus = N, publicExponent = E} = public_key:der_decode('RSAPublicKey', Der),
        Message = <<"a record to sign">>,
        Signature = macula_node_keys:sign(Message, Key),
        ?assert(macula_node_keys:verify(Message, Signature, Public, pq_hybrid)),
        ?assertNot(macula_node_keys:verify(Message, Signature, <<MlDsa/binary, (padded_modulus(N, E))/binary>>,
                                           pq_hybrid))
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

rsa_der_of(N, E) ->
    public_key:der_encode('RSAPublicKey', #'RSAPublicKey'{modulus = N, publicExponent = E}).

%% RSAPublicKey with the modulus INTEGER carrying one more leading zero byte than DER allows: the same number, a
%% second encoding of it.
padded_modulus(N, E) ->
    Modulus = binary:encode_unsigned(N),
    Padded = <<0, 0, Modulus/binary>>,
    Exponent = binary:encode_unsigned(E),
    Body = <<(tlv(2, Padded))/binary, (tlv(2, Exponent))/binary>>,
    tlv(16#30, Body).

tlv(Tag, Value) -> <<Tag, (der_length(byte_size(Value)))/binary, Value/binary>>.

der_length(L) when L < 128 -> <<L>>;
der_length(L) ->
    Bytes = binary:encode_unsigned(L),
    <<(16#80 bor byte_size(Bytes)), Bytes/binary>>.

rsa_der(Bits, Exponent) ->
    {[E, N], _Private} = crypto:generate_key(rsa, {Bits, Exponent}),
    public_key:der_encode('RSAPublicKey', #'RSAPublicKey'{modulus = binary:decode_unsigned(N),
                                                          publicExponent = binary:decode_unsigned(E)}).
