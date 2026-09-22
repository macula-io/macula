%% @doc Tests for macula_crypto_nif's hashing, encoding and comparison. Its ML-DSA is macula_crypto_nif_mldsa_tests.
-module(macula_crypto_nif_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Generator
%%====================================================================

crypto_test_() ->
    [
     {"NIF loaded", fun nif_loaded_tests/0},
     {"SHA-256 tests", fun sha256_tests/0},
     {"Base64 tests", fun base64_tests/0},
     {"Secure compare tests", fun secure_compare_tests/0}
    ].

%%====================================================================
%% NIF Loading
%%====================================================================

nif_loaded_tests() ->
    %% If the NIF didn't load, every crypto function below silently
    %% falls back to the pure-Erlang path (see this module's own
    %% `is_nif_loaded/0` callers) and every other test in this suite
    %% would still pass without ever exercising the Rust NIF at all.
    %% Fail loudly here so that isn't a silent gap.
    ?assert(macula_crypto_nif:is_nif_loaded()).

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
