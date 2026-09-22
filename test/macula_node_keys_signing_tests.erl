%% EUnit tests for signing with a node key: ML-DSA-87 alone in the US profile, and the IETF LAMPS composite
%% id-MLDSA87-RSA4096-PSS-SHA512 in the EU profile (plan decisions D4 and D7), proven against the draft's own vector.
-module(macula_node_keys_signing_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

%% RSA-4096 key generation takes up to about a second per key.
-define(EU_TIMEOUT, 120).
-define(PREFIX, "CompositeAlgorithmSignatures2025").
-define(LABEL, "COMPSIG-MLDSA87-RSA4096-PSS-SHA512").
-define(PSS_OPTIONS, [{rsa_padding, rsa_pkcs1_pss_padding}, {rsa_pss_saltlen, 48}, {rsa_mgf1_md, sha384}]).

%%------------------------------------------------------------------
%% The composite's message representative
%%------------------------------------------------------------------

%% The draft's worked example of M', for id-MLDSA65-ECDSA-P256-SHA512 over the bytes 00 to 09 with an empty ctx
%% (src/messageFormatSample_noctx.md at the fixture's commit): this pins the prefix, the length byte and the
%% pre-hash that representative/2 builds, and so the M' the half checks below use.
message_representative_matches_the_drafts_worked_example_test() ->
    Expected = binary:decode_hex(<<"436f6d706f73697465416c676f726974686d5369676e61747572657332303235"
                                   "434f4d505349472d4d4c44534136352d45434453412d503235362d534841353132"
                                   "00"
                                   "0f89ee1fcb7b0a4f7809d1267a029719004c5a5e5ec323a7c3523a20974f9a3f"
                                   "202f56fadba4cd9e8d654ab9f2e96dc5c795ea176fa20ede8d854c342f903533">>),
    ?assertEqual(Expected, representative(<<"COMPSIG-MLDSA65-ECDSA-P256-SHA512">>, <<0,1,2,3,4,5,6,7,8,9>>)).

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
%% EU profile: the LAMPS composite id-MLDSA87-RSA4096-PSS-SHA512
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
         %% Both halves sign the same message representative; ML-DSA-87 with the label as its context string, which
         %% OTP cannot check, and not with an empty one.
         ?_assert(macula_crypto_nif:mldsa_verify(mldsa87, MlDsaPublic, Representative, MlDsaSignature, <<?LABEL>>)),
         ?_assertNot(macula_crypto_nif:mldsa_verify(mldsa87, MlDsaPublic, Representative, MlDsaSignature, <<>>)),
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
%% The draft's own vector (draft-ietf-lamps-pq-composite-sigs), written by scripts/fetch-lamps-composite-vector.sh
%%------------------------------------------------------------------

%% The draft's signature over its message, made by its reference implementation, verifies here; altered, it does not.
drafts_signature_verifies_test_() ->
    Message = draft("m.bin"),
    Public = draft("pk.bin"),
    Signature = draft("s.bin"),
    [?_assert(macula_node_keys:verify(Message, Signature, Public, pq_hybrid)),
     ?_assertNot(macula_node_keys:verify(<<Message/binary, 0>>, Signature, Public, pq_hybrid)),
     ?_assertNot(macula_node_keys:verify(Message, flip_byte(Signature, 10), Public, pq_hybrid)),
     ?_assertNot(macula_node_keys:verify(Message, flip_byte(Signature, 4627 + 10), Public, pq_hybrid)),
     ?_assertNot(macula_node_keys:verify(Message, Signature, Public, pq_pure))].

%% Every Macula object signs with an empty ctx, so the draft's signature made with a ctx is refused.
drafts_signature_with_a_context_is_refused_test() ->
    ?assertNot(macula_node_keys:verify(draft("m.bin"), draft("s_with_context.bin"), draft("pk.bin"), pq_hybrid)).

%% The draft's private key, the ML-DSA-87 seed followed by the DER RSAPrivateKey, is a node key: it loads, carries the
%% draft's public key, and signs a composite whose halves the draft's construction accepts.
drafts_key_signs_as_a_node_key_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        Message = draft("m.bin"),
        Public = draft("pk.bin"),
        Key = drafts_key(),
        ?assertEqual({ok, Key}, save_and_load(Key)),
        ?assertEqual(Public, macula_node_keys:public_key(Key)),
        Signature = macula_node_keys:sign(Message, Key),
        <<MlDsaSignature:4627/binary, RsaSignature:512/binary>> = Signature,
        <<MlDsaPublic:2592/binary, RsaPublicDer/binary>> = Public,
        #'RSAPublicKey'{modulus = N, publicExponent = E} = public_key:der_decode('RSAPublicKey', RsaPublicDer),
        Representative = representative(Message),
        ?assert(macula_crypto_nif:mldsa_verify(mldsa87, MlDsaPublic, Representative, MlDsaSignature, <<?LABEL>>)),
        ?assert(crypto:verify(rsa, sha384, Representative, RsaSignature, [E, N], ?PSS_OPTIONS)),
        ?assert(macula_node_keys:verify(Message, Signature, Public, pq_hybrid))
    end}.

%% A composite that raw RSA-PSS accepts and every stack refuses: a composite over the draft's message by the draft's
%% key whose RSA half had its leading zero byte dropped, 4627 + 511 bytes, kept as fixed bytes for the other stacks to
%% check against. Written by scripts/make-zero-dropped-composite.sh.
zero_dropped_composite_is_refused_test() ->
    Message = draft("m.bin"),
    Public = draft("pk.bin"),
    Signature = fixture("lamps_composite_zero_dropped", "sig.bin"),
    <<MlDsaSignature:4627/binary, RsaSignature/binary>> = Signature,
    <<MlDsaPublic:2592/binary, RsaPublicDer/binary>> = Public,
    #'RSAPublicKey'{modulus = N, publicExponent = E} = public_key:der_decode('RSAPublicKey', RsaPublicDer),
    Representative = representative(Message),
    ?assertEqual(511, byte_size(RsaSignature)),
    %% Each half verifies on its own, so only the composite's length refuses it.
    ?assert(macula_crypto_nif:mldsa_verify(mldsa87, MlDsaPublic, Representative, MlDsaSignature, <<?LABEL>>)),
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

%% M' = Prefix || Label || len(ctx) || ctx || SHA-512(M), with the empty ctx every Macula object signs with.
representative(Message) ->
    representative(<<?LABEL>>, Message).

representative(Label, Message) ->
    <<?PREFIX, Label/binary, 0, (crypto:hash(sha512, Message))/binary>>.

drafts_key() ->
    <<Seed:32/binary, RsaPrivateDer/binary>> = draft("sk.bin"),
    <<MlDsaPublic:2592/binary, RsaPublicDer/binary>> = draft("pk.bin"),
    #{purpose => identity, profile => pq_hybrid,
      components => [#{algorithm => mldsa87, public => MlDsaPublic, private => Seed},
                     #{algorithm => rsa_pss, public => RsaPublicDer, private => RsaPrivateDer}]}.

save_and_load(Key) ->
    macula_test_tmp:with_dir("macula_node_keys_signing_tests", fun(Dir) ->
        Path = filename:join(Dir, "identity.key"),
        ok = macula_node_keys:save(Path, Key),
        macula_node_keys:load(Path, identity, pq_hybrid)
    end).

flip_byte(Bin, Offset) ->
    <<Head:Offset/binary, Byte, Tail/binary>> = Bin,
    <<Head/binary, (Byte bxor 1), Tail/binary>>.

draft(Name) ->
    fixture("lamps_mldsa87_rsa4096_pss_sha512", Name).

fixture(Dir, Name) ->
    {ok, Bin} = file:read_file(filename:join([filename:dirname(?FILE), "fixtures", Dir, Name])),
    Bin.
