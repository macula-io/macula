%% @doc Tests for macula_ucan_nif module.
%% Tests UCAN token creation, verification, and manipulation.
-module(macula_ucan_nif_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Generator
%%====================================================================

ucan_test_() ->
    [
     {"Token creation tests", fun creation_tests/0},
     {"Token verification tests", fun verification_tests/0},
     {"Token decoding tests", fun decode_tests/0},
     {"Token extraction tests", fun extraction_tests/0},
     {"Expiration tests", fun expiration_tests/0},
     {"Error handling tests", fun error_handling_tests/0},
     {"Roundtrip tests", fun roundtrip_tests/0}
    ].

%%====================================================================
%% Helper Functions
%%====================================================================

generate_keypair() ->
    macula_crypto_nif:generate_keypair().

current_timestamp() ->
    erlang:system_time(second).

%%====================================================================
%% Token Creation Tests
%%====================================================================

creation_tests() ->
    {ok, {_PubKey, PrivKey}} = generate_keypair(),
    Issuer = <<"did:macula:io.macula.acme">>,
    Audience = <<"did:macula:io.macula.customer">>,
    Capabilities = [#{<<"with">> => <<"artifact:*">>, <<"can">> => <<"deploy">>}],

    %% Test 1: Create basic token
    {ok, Token} = macula_ucan_nif:create(Issuer, Audience, Capabilities, PrivKey),
    ?assertEqual(true, is_binary(Token)),
    ?assertEqual(2, length(binary:matches(Token, <<".">>))), % JWT format

    %% Test 2: Create token with expiration
    Exp = current_timestamp() + 3600,
    {ok, Token2} = macula_ucan_nif:create(Issuer, Audience, Capabilities, PrivKey, #{exp => Exp}),
    ?assertEqual(true, is_binary(Token2)),

    %% Test 3: Create token with nbf (not before)
    Nbf = current_timestamp() - 60,
    {ok, Token3} = macula_ucan_nif:create(Issuer, Audience, Capabilities, PrivKey, #{nbf => Nbf}),
    ?assertEqual(true, is_binary(Token3)),

    %% Test 4: Create token with facts
    Facts = #{<<"tier">> => <<"pro">>, <<"seats">> => 10},
    {ok, Token4} = macula_ucan_nif:create(Issuer, Audience, Capabilities, PrivKey, #{fct => Facts}),
    ?assertEqual(true, is_binary(Token4)),

    %% Test 5: Create token with proofs
    Proofs = [<<"cid1">>, <<"cid2">>],
    {ok, Token5} = macula_ucan_nif:create(Issuer, Audience, Capabilities, PrivKey, #{prf => Proofs}),
    ?assertEqual(true, is_binary(Token5)),

    %% Test 6: Create token with nonce
    {ok, Token6} = macula_ucan_nif:create(Issuer, Audience, Capabilities, PrivKey, #{nnc => <<"unique123">>}),
    ?assertEqual(true, is_binary(Token6)),

    %% Test 7: Create token with all options
    AllOpts = #{
        exp => current_timestamp() + 7200,
        nbf => current_timestamp() - 30,
        nnc => <<"nonce456">>,
        fct => #{<<"key">> => <<"value">>},
        prf => [<<"proof1">>]
    },
    {ok, Token7} = macula_ucan_nif:create(Issuer, Audience, Capabilities, PrivKey, AllOpts),
    ?assertEqual(true, is_binary(Token7)),

    ok.

%%====================================================================
%% Token Verification Tests
%%====================================================================

verification_tests() ->
    {ok, {PubKey, PrivKey}} = generate_keypair(),
    Issuer = <<"did:macula:io.macula.acme">>,
    Audience = <<"did:macula:io.macula.customer">>,
    Capabilities = [#{<<"with">> => <<"artifact:myapp">>, <<"can">> => <<"use">>}],

    %% Create valid token
    {ok, Token} = macula_ucan_nif:create(Issuer, Audience, Capabilities, PrivKey),

    %% Test 8: Verify valid token - returns decoded payload on success
    {ok, VerifiedPayload} = macula_ucan_nif:verify(Token, PubKey),
    ?assertEqual(Issuer, maps:get(<<"iss">>, VerifiedPayload)),
    ?assertEqual(Audience, maps:get(<<"aud">>, VerifiedPayload)),

    %% Test 9: Verify with wrong public key
    {ok, {WrongPubKey, _}} = generate_keypair(),
    ?assertEqual({error, invalid_signature}, macula_ucan_nif:verify(Token, WrongPubKey)),

    %% Test 10: Verify tampered token
    %% Note: "invalid" is not valid base64, so NIF returns invalid_token (can't decode payload)
    [Header, _PayloadB64, Sig] = binary:split(Token, <<".">>, [global]),
    TamperedPayload = <<"invalid">>,
    TamperedToken = <<Header/binary, ".", TamperedPayload/binary, ".", Sig/binary>>,
    ?assertEqual({error, invalid_token}, macula_ucan_nif:verify(TamperedToken, PubKey)),

    %% Test 11: Verify with valid expiration
    Exp = current_timestamp() + 3600,
    {ok, TokenWithExp} = macula_ucan_nif:create(Issuer, Audience, Capabilities, PrivKey, #{exp => Exp}),
    {ok, VerifiedPayload2} = macula_ucan_nif:verify(TokenWithExp, PubKey),
    ?assertEqual(Exp, maps:get(<<"exp">>, VerifiedPayload2)),

    %% Test 12: Verify invalid key length
    ?assertEqual({error, invalid_public_key}, macula_ucan_nif:verify(Token, <<"short">>)),

    ok.

%%====================================================================
%% Token Decoding Tests
%%====================================================================

decode_tests() ->
    {ok, {_PubKey, PrivKey}} = generate_keypair(),
    Issuer = <<"did:macula:io.macula.publisher">>,
    Audience = <<"did:macula:io.macula.subscriber">>,
    Capabilities = [#{<<"with">> => <<"resource:x">>, <<"can">> => <<"read">>}],

    {ok, Token} = macula_ucan_nif:create(Issuer, Audience, Capabilities, PrivKey),

    %% Test 13: Decode valid token (without verification)
    {ok, Payload} = macula_ucan_nif:decode(Token),
    ?assertEqual(true, is_map(Payload)),
    ?assertEqual(Issuer, maps:get(<<"iss">>, Payload)),
    ?assertEqual(Audience, maps:get(<<"aud">>, Payload)),

    %% Test 14: Decode token with all fields
    AllOpts = #{
        exp => current_timestamp() + 3600,
        nbf => current_timestamp(),
        nnc => <<"nonce789">>,
        fct => #{<<"tier">> => <<"basic">>},
        prf => [<<"proof1">>, <<"proof2">>]
    },
    {ok, Token2} = macula_ucan_nif:create(Issuer, Audience, Capabilities, PrivKey, AllOpts),
    {ok, Payload2} = macula_ucan_nif:decode(Token2),
    ?assertEqual(true, maps:is_key(<<"exp">>, Payload2)),
    ?assertEqual(true, maps:is_key(<<"nbf">>, Payload2)),
    ?assertEqual(<<"nonce789">>, maps:get(<<"nnc">>, Payload2)),

    %% Test 15: Decode malformed token
    ?assertEqual({error, invalid_token}, macula_ucan_nif:decode(<<"not.a.token">>)),

    %% Test 16: Decode token with wrong format
    ?assertEqual({error, invalid_token}, macula_ucan_nif:decode(<<"onlyonepart">>)),

    ok.

%%====================================================================
%% Token Extraction Tests
%%====================================================================

extraction_tests() ->
    {ok, {_PubKey, PrivKey}} = generate_keypair(),
    Issuer = <<"did:macula:io.macula.org1">>,
    Audience = <<"did:macula:io.macula.org2">>,
    Capabilities = [
        #{<<"with">> => <<"artifact:app1">>, <<"can">> => <<"deploy">>},
        #{<<"with">> => <<"artifact:app2">>, <<"can">> => <<"use">>}
    ],
    Exp = current_timestamp() + 7200,

    {ok, Token} = macula_ucan_nif:create(Issuer, Audience, Capabilities, PrivKey, #{exp => Exp}),

    %% Test 17: Get issuer
    {ok, ExtractedIssuer} = macula_ucan_nif:get_issuer(Token),
    ?assertEqual(Issuer, ExtractedIssuer),

    %% Test 18: Get audience
    {ok, ExtractedAudience} = macula_ucan_nif:get_audience(Token),
    ?assertEqual(Audience, ExtractedAudience),

    %% Test 19: Get capabilities
    {ok, ExtractedCaps} = macula_ucan_nif:get_capabilities(Token),
    ?assertEqual(2, length(ExtractedCaps)),

    %% Test 20: Get expiration
    {ok, ExtractedExp} = macula_ucan_nif:get_expiration(Token),
    ?assertEqual(Exp, ExtractedExp),

    %% Test 21: Get proofs (empty)
    {ok, Proofs} = macula_ucan_nif:get_proofs(Token),
    ?assertEqual([], Proofs),

    %% Test 22: Compute CID
    CID = macula_ucan_nif:compute_cid(Token),
    ?assertEqual(true, is_binary(CID)),
    ?assertEqual(43, byte_size(CID)), % SHA-256 base64 = 43 chars

    %% Test 23: CID is deterministic
    CID2 = macula_ucan_nif:compute_cid(Token),
    ?assertEqual(CID, CID2),

    %% Test 24: Different tokens have different CIDs
    {ok, Token2} = macula_ucan_nif:create(Issuer, Audience, Capabilities, PrivKey, #{nnc => <<"different">>}),
    CID3 = macula_ucan_nif:compute_cid(Token2),
    ?assertNotEqual(CID, CID3),

    ok.

%%====================================================================
%% Expiration Tests
%%====================================================================

expiration_tests() ->
    {ok, {PubKey, PrivKey}} = generate_keypair(),
    Issuer = <<"did:macula:io.macula.a">>,
    Audience = <<"did:macula:io.macula.b">>,
    Capabilities = [#{<<"with">> => <<"*">>, <<"can">> => <<"*">>}],

    %% Test 25: Token without expiration is not expired
    {ok, TokenNoExp} = macula_ucan_nif:create(Issuer, Audience, Capabilities, PrivKey),
    ?assertEqual(false, macula_ucan_nif:is_expired(TokenNoExp)),

    %% Test 26: Token with future expiration is not expired
    FutureExp = current_timestamp() + 3600,
    {ok, TokenFuture} = macula_ucan_nif:create(Issuer, Audience, Capabilities, PrivKey, #{exp => FutureExp}),
    ?assertEqual(false, macula_ucan_nif:is_expired(TokenFuture)),

    %% Test 27: Token with past expiration is expired
    PastExp = current_timestamp() - 3600,
    {ok, TokenPast} = macula_ucan_nif:create(Issuer, Audience, Capabilities, PrivKey, #{exp => PastExp}),
    ?assertEqual(true, macula_ucan_nif:is_expired(TokenPast)),

    %% Test 28: Verify expired token fails
    ?assertEqual({error, expired}, macula_ucan_nif:verify(TokenPast, PubKey)),

    %% Test 29: Token not yet valid (nbf in future)
    FutureNbf = current_timestamp() + 3600,
    {ok, TokenNotYetValid} = macula_ucan_nif:create(Issuer, Audience, Capabilities, PrivKey, #{nbf => FutureNbf}),
    ?assertEqual({error, not_yet_valid}, macula_ucan_nif:verify(TokenNotYetValid, PubKey)),

    ok.

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_tests() ->
    Issuer = <<"did:macula:io.macula.test">>,
    Audience = <<"did:macula:io.macula.test2">>,
    Capabilities = [#{<<"with">> => <<"x">>, <<"can">> => <<"y">>}],

    %% Test 30: Create with invalid private key
    ?assertEqual({error, invalid_private_key},
                 macula_ucan_nif:create(Issuer, Audience, Capabilities, <<"short">>)),

    %% Test 31: Get issuer from invalid token
    ?assertEqual({error, invalid_token}, macula_ucan_nif:get_issuer(<<"invalid">>)),

    %% Test 32: Get audience from invalid token
    ?assertEqual({error, invalid_token}, macula_ucan_nif:get_audience(<<"invalid">>)),

    %% Test 33: Get capabilities from invalid token
    ?assertEqual({error, invalid_token}, macula_ucan_nif:get_capabilities(<<"not.valid.token">>)),

    ok.

%%====================================================================
%% Roundtrip Tests
%%====================================================================

roundtrip_tests() ->
    %% Test 34-40: Full UCAN roundtrip multiple times
    lists:foreach(fun(N) ->
        {ok, {PubKey, PrivKey}} = generate_keypair(),
        Issuer = iolist_to_binary([<<"did:macula:io.macula.issuer">>, integer_to_binary(N)]),
        Audience = iolist_to_binary([<<"did:macula:io.macula.audience">>, integer_to_binary(N)]),
        Capabilities = [#{<<"with">> => <<"resource:", (integer_to_binary(N))/binary>>,
                         <<"can">> => <<"action">>}],

        %% Create token
        {ok, Token} = macula_ucan_nif:create(Issuer, Audience, Capabilities, PrivKey),

        %% Verify token - returns {ok, Payload} on success
        {ok, VerifiedPayload} = macula_ucan_nif:verify(Token, PubKey),
        ?assertEqual(Issuer, maps:get(<<"iss">>, VerifiedPayload)),
        ?assertEqual(Audience, maps:get(<<"aud">>, VerifiedPayload)),

        %% Decode and check fields
        {ok, Payload} = macula_ucan_nif:decode(Token),
        ?assertEqual(Issuer, maps:get(<<"iss">>, Payload)),
        ?assertEqual(Audience, maps:get(<<"aud">>, Payload)),

        %% Extract fields
        {ok, ExtIss} = macula_ucan_nif:get_issuer(Token),
        {ok, ExtAud} = macula_ucan_nif:get_audience(Token),
        ?assertEqual(Issuer, ExtIss),
        ?assertEqual(Audience, ExtAud)
    end, lists:seq(1, 7)),

    ok.
