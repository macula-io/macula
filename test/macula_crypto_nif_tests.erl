%% @doc Tests for macula_crypto_nif module.
%% Tests both NIF and pure Erlang fallback implementations.
-module(macula_crypto_nif_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Generator
%%====================================================================

crypto_test_() ->
    [
     {"Key generation tests", fun keypair_tests/0},
     {"Signing tests", fun signing_tests/0},
     {"Verification tests", fun verification_tests/0},
     {"SHA-256 tests", fun sha256_tests/0},
     {"Base64 tests", fun base64_tests/0},
     {"Secure compare tests", fun secure_compare_tests/0},
     {"Error handling tests", fun error_handling_tests/0},
     {"Roundtrip tests", fun roundtrip_tests/0}
    ].

%%====================================================================
%% Key Generation Tests
%%====================================================================

keypair_tests() ->
    %% Test 1: Generate keypair returns proper format
    {ok, {PubKey, PrivKey}} = macula_crypto_nif:generate_keypair(),
    ?assertEqual(32, byte_size(PubKey)),
    ?assertEqual(32, byte_size(PrivKey)),

    %% Test 2: Each keypair is unique
    {ok, {PubKey2, PrivKey2}} = macula_crypto_nif:generate_keypair(),
    ?assertNotEqual(PubKey, PubKey2),
    ?assertNotEqual(PrivKey, PrivKey2),

    %% Test 3: Multiple generations don't fail
    Results = [macula_crypto_nif:generate_keypair() || _ <- lists:seq(1, 10)],
    ?assertEqual(10, length([R || {ok, _} = R <- Results])),

    %% Test 4: Public keys are all different
    PubKeys = [P || {ok, {P, _}} <- Results],
    UniquePubKeys = lists:usort(PubKeys),
    ?assertEqual(length(PubKeys), length(UniquePubKeys)),

    ok.

%%====================================================================
%% Signing Tests
%%====================================================================

signing_tests() ->
    {ok, {_PubKey, PrivKey}} = macula_crypto_nif:generate_keypair(),

    %% Test 5: Sign empty message
    {ok, Sig1} = macula_crypto_nif:sign(<<>>, PrivKey),
    ?assertEqual(64, byte_size(Sig1)),

    %% Test 6: Sign short message
    {ok, Sig2} = macula_crypto_nif:sign(<<"hello">>, PrivKey),
    ?assertEqual(64, byte_size(Sig2)),

    %% Test 7: Sign long message
    LongMsg = binary:copy(<<"a">>, 10000),
    {ok, Sig3} = macula_crypto_nif:sign(LongMsg, PrivKey),
    ?assertEqual(64, byte_size(Sig3)),

    %% Test 8: Same message produces same signature with same key
    {ok, Sig4} = macula_crypto_nif:sign(<<"hello">>, PrivKey),
    ?assertEqual(Sig2, Sig4),

    %% Test 9: Different messages produce different signatures
    {ok, Sig5} = macula_crypto_nif:sign(<<"world">>, PrivKey),
    ?assertNotEqual(Sig2, Sig5),

    %% Test 10: Different keys produce different signatures
    {ok, {_, PrivKey2}} = macula_crypto_nif:generate_keypair(),
    {ok, Sig6} = macula_crypto_nif:sign(<<"hello">>, PrivKey2),
    ?assertNotEqual(Sig2, Sig6),

    ok.

%%====================================================================
%% Verification Tests
%%====================================================================

verification_tests() ->
    {ok, {PubKey, PrivKey}} = macula_crypto_nif:generate_keypair(),
    Message = <<"test message">>,
    {ok, Signature} = macula_crypto_nif:sign(Message, PrivKey),

    %% Test 11: Valid signature verifies
    ?assertEqual(true, macula_crypto_nif:verify(Message, Signature, PubKey)),

    %% Test 12: Wrong message fails
    ?assertEqual(false, macula_crypto_nif:verify(<<"wrong">>, Signature, PubKey)),

    %% Test 13: Tampered signature fails
    <<First:8, Rest/binary>> = Signature,
    TamperedSig = <<(First bxor 255):8, Rest/binary>>,
    ?assertEqual(false, macula_crypto_nif:verify(Message, TamperedSig, PubKey)),

    %% Test 14: Wrong public key fails
    {ok, {WrongPubKey, _}} = macula_crypto_nif:generate_keypair(),
    ?assertEqual(false, macula_crypto_nif:verify(Message, Signature, WrongPubKey)),

    %% Test 15: Invalid signature length fails
    ?assertEqual(false, macula_crypto_nif:verify(Message, <<"short">>, PubKey)),

    %% Test 16: Invalid public key length fails
    ?assertEqual(false, macula_crypto_nif:verify(Message, Signature, <<"short">>)),

    ok.

%%====================================================================
%% SHA-256 Tests
%%====================================================================

sha256_tests() ->
    %% Test 17: Empty input
    Hash1 = macula_crypto_nif:sha256(<<>>),
    ?assertEqual(32, byte_size(Hash1)),
    %% Known SHA-256 of empty string
    ?assertEqual(<<227,176,196,66,152,252,28,20,154,251,244,200,153,111,185,36,
                   39,174,65,228,100,155,147,76,164,149,153,27,120,82,184,85>>, Hash1),

    %% Test 18: Simple message
    Hash2 = macula_crypto_nif:sha256(<<"hello">>),
    ?assertEqual(32, byte_size(Hash2)),
    %% Known SHA-256 of "hello" (2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824)
    ?assertEqual(<<44,242,77,186,95,176,163,14,38,232,59,42,197,185,226,158,
                   27,22,30,92,31,167,66,94,115,4,51,98,147,139,152,36>>, Hash2),

    %% Test 19: Same input produces same hash
    Hash3 = macula_crypto_nif:sha256(<<"hello">>),
    ?assertEqual(Hash2, Hash3),

    %% Test 20: Different inputs produce different hashes
    Hash4 = macula_crypto_nif:sha256(<<"world">>),
    ?assertNotEqual(Hash2, Hash4),

    %% Test 21: SHA-256 + base64
    B64Hash = macula_crypto_nif:sha256_base64(<<"hello">>),
    ?assertEqual(true, is_binary(B64Hash)),
    ?assertEqual(43, byte_size(B64Hash)), % 32 bytes base64 = 43 chars (no padding)

    ok.

%%====================================================================
%% Base64 Tests
%%====================================================================

base64_tests() ->
    %% Test 22: Encode empty
    ?assertEqual(<<>>, macula_crypto_nif:base64_encode(<<>>)),

    %% Test 23: Encode simple
    Encoded = macula_crypto_nif:base64_encode(<<"hello">>),
    ?assertEqual(<<"aGVsbG8">>, Encoded),

    %% Test 24: Decode simple
    {ok, Decoded} = macula_crypto_nif:base64_decode(<<"aGVsbG8">>),
    ?assertEqual(<<"hello">>, Decoded),

    %% Test 25: Roundtrip
    Original = <<"test data 123">>,
    Enc = macula_crypto_nif:base64_encode(Original),
    {ok, Dec} = macula_crypto_nif:base64_decode(Enc),
    ?assertEqual(Original, Dec),

    %% Test 26: URL-safe characters
    Data = <<255, 254, 253>>,
    Enc2 = macula_crypto_nif:base64_encode(Data),
    ?assertEqual(nomatch, binary:match(Enc2, <<"+">>)),
    ?assertEqual(nomatch, binary:match(Enc2, <<"/">>)),

    %% Test 27: Invalid base64
    ?assertEqual({error, invalid_base64}, macula_crypto_nif:base64_decode(<<"!!!invalid">>)),

    ok.

%%====================================================================
%% Secure Compare Tests
%%====================================================================

secure_compare_tests() ->
    %% Test 28: Equal binaries
    ?assertEqual(true, macula_crypto_nif:secure_compare(<<"hello">>, <<"hello">>)),

    %% Test 29: Different binaries
    ?assertEqual(false, macula_crypto_nif:secure_compare(<<"hello">>, <<"world">>)),

    %% Test 30: Different lengths
    ?assertEqual(false, macula_crypto_nif:secure_compare(<<"hello">>, <<"hi">>)),

    %% Test 31: Empty binaries
    ?assertEqual(true, macula_crypto_nif:secure_compare(<<>>, <<>>)),

    %% Test 32: One empty
    ?assertEqual(false, macula_crypto_nif:secure_compare(<<>>, <<"a">>)),

    %% Test 33: Single byte difference
    ?assertEqual(false, macula_crypto_nif:secure_compare(<<"hellp">>, <<"hello">>)),

    ok.

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_tests() ->
    %% Test 34: Sign with invalid key length
    ?assertEqual({error, invalid_private_key}, macula_crypto_nif:sign(<<"msg">>, <<"short">>)),

    %% Test 35: Sign with too long key
    ?assertEqual({error, invalid_private_key}, macula_crypto_nif:sign(<<"msg">>, binary:copy(<<0>>, 64))),

    ok.

%%====================================================================
%% Roundtrip Tests
%%====================================================================

roundtrip_tests() ->
    %% Test 36-40: Full crypto roundtrip multiple times
    lists:foreach(fun(_) ->
        {ok, {PubKey, PrivKey}} = macula_crypto_nif:generate_keypair(),
        Message = crypto:strong_rand_bytes(100),
        {ok, Signature} = macula_crypto_nif:sign(Message, PrivKey),
        ?assertEqual(true, macula_crypto_nif:verify(Message, Signature, PubKey))
    end, lists:seq(1, 5)),

    ok.
