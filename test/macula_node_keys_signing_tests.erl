%% EUnit tests for signing with a node key: ML-DSA-87 alone in the US profile, and Macula's composite ML-DSA-87-PS384
%% in the EU profile (plan decisions D4 and D7).
-module(macula_node_keys_signing_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

%% RSA-4096 key generation takes up to about a second per key.
-define(EU_TIMEOUT, 120).
-define(PREFIX, "CompositeAlgorithmSignatures2025").
-define(LABEL, "MACULA-ML-DSA-87-PS384").
-define(PSS_OPTIONS, [{rsa_padding, rsa_pkcs1_pss_padding}, {rsa_pss_saltlen, 48}, {rsa_mgf1_md, sha384}]).

%%------------------------------------------------------------------
%% The composite's message representative
%%------------------------------------------------------------------

%% The same bytes were sent to the Go vector check; this pins the constants used below.
message_representative_matches_the_shared_vector_test() ->
    Expected = binary:decode_hex(<<"436f6d706f73697465416c676f726974686d5369676e61747572657332303235"
                                   "4d4143554c412d4d4c2d4453412d38372d5053333834"
                                   "00"
                                   "e4f23edffade3a0a47087a2f675e84d4ed9c126f824e93c09ae81c09033b82d3"
                                   "ef4b9c62d5bbc6238b99df1bec305ed30456cd776dca2e8182ecc35e4c72b7f7">>),
    ?assertEqual(Expected, representative(<<"macula-composite-vector">>)).

%%------------------------------------------------------------------
%% US profile: ML-DSA-87 alone
%%------------------------------------------------------------------

us_signing_test_() ->
    {setup, fun us_identity_key/0, fun(Key) ->
        Message = <<"a record to sign">>,
        Signature = macula_node_keys:sign(Message, Key),
        Public = macula_node_keys:public_key(Key),
        [?_assertEqual({4627, 2592}, {byte_size(Signature), byte_size(Public)}),
         ?_assert(crypto:verify(mldsa87, none, Message, Signature, Public)),
         ?_assert(macula_node_keys:verify(Message, Signature, Public, pq_pure)),
         ?_assertNot(macula_node_keys:verify(<<"another record">>, Signature, Public, pq_pure)),
         ?_assertNot(macula_node_keys:verify(Message, Signature, Public, pq_hybrid))]
    end}.

%%------------------------------------------------------------------
%% EU profile: Macula's composite ML-DSA-87-PS384
%%------------------------------------------------------------------

eu_signing_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun eu_identity_key/0, fun(Key) ->
        Message = <<"a record to sign">>,
        Signature = macula_node_keys:sign(Message, Key),
        Public = macula_node_keys:public_key(Key),
        <<MlDsaSignature:4627/binary, RsaSignature/binary>> = Signature,
        <<MlDsaPublic:2592/binary, RsaPublicDer/binary>> = Public,
        #'RSAPublicKey'{modulus = N, publicExponent = E} = public_key:der_decode('RSAPublicKey', RsaPublicDer),
        Representative = representative(Message),
        [?_assertEqual({5139, 3118}, {byte_size(Signature), byte_size(Public)}),
         ?_assertEqual(65537, E),
         %% Both halves sign the same message representative; ML-DSA-87 with an empty context.
         ?_assert(crypto:verify(mldsa87, none, Representative, MlDsaSignature, MlDsaPublic)),
         ?_assert(crypto:verify(rsa, sha384, Representative, RsaSignature, [E, N], ?PSS_OPTIONS)),
         ?_assert(macula_node_keys:verify(Message, Signature, Public, pq_hybrid)),
         ?_assertNot(macula_node_keys:verify(<<"another record">>, Signature, Public, pq_hybrid)),
         %% A signature with one invalid half is refused.
         ?_assertNot(macula_node_keys:verify(Message, flip_byte(Signature, 10), Public, pq_hybrid)),
         ?_assertNot(macula_node_keys:verify(Message, flip_byte(Signature, 4627 + 10), Public, pq_hybrid)),
         %% A half on its own is not a signature.
         ?_assertNot(macula_node_keys:verify(Message, MlDsaSignature, Public, pq_hybrid)),
         ?_assertNot(macula_node_keys:verify(Message, MlDsaSignature, MlDsaPublic, pq_pure)),
         %% The composite is not accepted under the other profile, nor with a non-canonical key encoding.
         ?_assertNot(macula_node_keys:verify(Message, Signature, Public, pq_pure)),
         ?_assertNot(macula_node_keys:verify(Message, Signature, <<Public/binary, 0>>, pq_hybrid))]
    end}}.

%% A composite is exactly 4627 + 512 bytes, its RSA half as long as the modulus, as the Go stack requires. One byte
%% short or long is refused, including the same RSA value with its leading zero byte dropped.
a_composite_of_another_length_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun eu_identity_key/0, fun(Key) ->
        Message = <<"a record to sign">>,
        Public = macula_node_keys:public_key(Key),
        <<MlDsaSignature:4627/binary, 0, RsaRest/binary>> = Leading = leading_zero_rsa_half(Message, Key, 4096),
        [?_assert(macula_node_keys:verify(Message, Leading, Public, pq_hybrid)),
         ?_assertNot(macula_node_keys:verify(Message, <<MlDsaSignature/binary, RsaRest/binary>>, Public, pq_hybrid)),
         ?_assertNot(macula_node_keys:verify(Message, binary:part(Leading, 0, 5138), Public, pq_hybrid)),
         ?_assertNot(macula_node_keys:verify(Message, <<Leading/binary, 0>>, Public, pq_hybrid))]
    end}}.

%% A composite signature of Message whose RSA half begins with a zero byte. A PSS salt is random, so signing again finds
%% one about once in 256 signatures.
leading_zero_rsa_half(Message, Key, Left) when Left > 0 ->
    leading_zero_half(macula_node_keys:sign(Message, Key), Message, Key, Left).

leading_zero_half(<<_:4627/binary, 0, _/binary>> = Signature, _Message, _Key, _Left) -> Signature;
leading_zero_half(_Signature, Message, Key, Left) -> leading_zero_rsa_half(Message, Key, Left - 1).

%%------------------------------------------------------------------
%% Malformed input
%%------------------------------------------------------------------

malformed_input_is_refused_without_raising_test_() ->
    [?_assertNot(macula_node_keys:verify(<<"m">>, <<>>, <<>>, pq_pure)),
     ?_assertNot(macula_node_keys:verify(<<"m">>, <<0:4627/unit:8>>, <<0:2592/unit:8>>, pq_pure)),
     ?_assertNot(macula_node_keys:verify(<<"m">>, <<0:5139/unit:8>>, <<0:3118/unit:8>>, pq_hybrid)),
     ?_assertNot(macula_node_keys:verify(<<"m">>, <<"sig">>, <<"key">>, rsa_only))].

ed25519_signature_is_refused_test() ->
    Ed25519 = macula_identity:generate(),
    Signature = macula_identity:sign(<<"m">>, Ed25519),
    ?assertNot(macula_node_keys:verify(<<"m">>, Signature, macula_identity:public(Ed25519), pq_pure)).

%%------------------------------------------------------------------
%% Cross-stack vectors: composites signed by OTP and by Go in the Go V8 check (2026-09-10)
%%------------------------------------------------------------------

cross_stack_composite_vectors_test_() ->
    Message = fixture("message.bin"),
    [{Signer, [?_assert(macula_node_keys:verify(Message, Signature, Public, pq_hybrid)),
               ?_assertNot(macula_node_keys:verify(<<Message/binary, 0>>, Signature, Public, pq_hybrid)),
               ?_assertNot(macula_node_keys:verify(Message, flip_byte(Signature, 10), Public, pq_hybrid)),
               ?_assertNot(macula_node_keys:verify(Message, flip_byte(Signature, 4627 + 10), Public, pq_hybrid)),
               ?_assertNot(macula_node_keys:verify(Message, Signature, Public, pq_pure))]}
     || Signer <- ["otp", "go"],
        Public <- [fixture(Signer ++ "_composite_pub.bin")],
        Signature <- [fixture(Signer ++ "_composite_sig.bin")]].

%% A composite that raw RSA-PSS accepts and every stack refuses: a valid composite over message.bin whose RSA half had
%% its leading zero byte dropped, 4627 + 511 bytes, kept as fixed bytes for the other stacks to check against.
cross_stack_zero_dropped_composite_is_refused_test() ->
    Message = fixture("message.bin"),
    Public = fixture("zero_dropped_composite_pub.bin"),
    Signature = fixture("zero_dropped_composite_sig.bin"),
    <<MlDsaSignature:4627/binary, RsaSignature/binary>> = Signature,
    <<MlDsaPublic:2592/binary, RsaPublicDer/binary>> = Public,
    #'RSAPublicKey'{modulus = N, publicExponent = E} = public_key:der_decode('RSAPublicKey', RsaPublicDer),
    Representative = representative(Message),
    ?assertEqual(511, byte_size(RsaSignature)),
    %% Each half verifies on its own, so only the composite's length refuses it.
    ?assert(crypto:verify(mldsa87, none, Representative, MlDsaSignature, MlDsaPublic)),
    ?assert(crypto:verify(rsa, sha384, Representative, RsaSignature, [E, N], ?PSS_OPTIONS)),
    ?assertNot(macula_node_keys:verify(Message, Signature, Public, pq_hybrid)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

us_identity_key() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Key.

eu_identity_key() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_hybrid),
    Key.

representative(Message) ->
    <<?PREFIX, ?LABEL, 0, (crypto:hash(sha512, Message))/binary>>.

flip_byte(Bin, Offset) ->
    <<Head:Offset/binary, Byte, Tail/binary>> = Bin,
    <<Head/binary, (Byte bxor 1), Tail/binary>>.

fixture(Name) ->
    Path = filename:join([filename:dirname(?FILE), "fixtures", "composite_ml_dsa_87_ps384", Name]),
    {ok, Bin} = file:read_file(Path),
    Bin.
