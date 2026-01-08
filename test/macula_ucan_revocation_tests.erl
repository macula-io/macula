%% @doc Unit tests for macula_ucan_revocation module.
-module(macula_ucan_revocation_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup/Teardown
%%====================================================================

setup() ->
    %% Start the revocation server
    {ok, Pid} = macula_ucan_revocation:start_link(#{}),
    Pid.

cleanup(Pid) ->
    %% Clear cache and stop server
    macula_ucan_revocation:clear_cache(Pid),
    macula_ucan_revocation:stop(Pid),
    ok.

%%====================================================================
%% Test Generators
%%====================================================================

revocation_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"start and stop server", fun test_start_stop/0},
         {"revoke a UCAN", fun test_revoke_ucan/0},
         {"is_revoked returns false for non-revoked", fun test_is_revoked_false/0},
         {"is_revoked returns true for revoked", fun test_is_revoked_true/0},
         {"is_revoked returns false after expiry", fun test_is_revoked_expired/0},
         {"rate limiting works", fun test_rate_limit/0},
         {"handle valid revocation message", fun test_handle_valid_revocation/0},
         {"handle invalid revocation message (missing fields)", fun test_handle_invalid_msg/0},
         {"handle invalid signature", fun test_handle_invalid_signature/0},
         {"get_stats returns counts", fun test_get_stats/0},
         {"clear_cache removes all entries", fun test_clear_cache/0},
         {"compute_ucan_cid produces consistent hash", fun test_compute_cid/0},
         {"validate_revocation_signature checks format", fun test_validate_signature/0},
         {"multiple revocations from same issuer", fun test_multiple_revocations/0},
         {"revocations from different issuers", fun test_different_issuers/0}
     ]
    }.

%%====================================================================
%% Basic Functionality Tests
%%====================================================================

test_start_stop() ->
    %% Server is already started by setup, just verify it's running
    Stats = macula_ucan_revocation:get_stats(),
    ?assert(is_map(Stats)),
    ?assertEqual(0, maps:get(cache_size, Stats)).

test_revoke_ucan() ->
    IssuerDID = <<"did:macula:io.macula.test">>,
    UcanToken = <<"eyJ0eXAiOiJKV1QiLCJhbGciOiJFZDI1NTE5In0.eyJpc3MiOiJkaWQ6bWFjdWxhOmlvLm1hY3VsYS50ZXN0In0.signature">>,
    ExpiresAt = erlang:system_time(second) + 3600,  %% 1 hour from now

    {ok, CID} = macula_ucan_revocation:revoke(IssuerDID, UcanToken, ExpiresAt),

    ?assert(is_binary(CID)),
    ?assert(byte_size(CID) > 0),

    %% Should now be revoked
    ?assert(macula_ucan_revocation:is_revoked(IssuerDID, CID)).

test_is_revoked_false() ->
    IssuerDID = <<"did:macula:io.macula.unknown">>,
    CID = <<"nonexistent-cid">>,

    ?assertNot(macula_ucan_revocation:is_revoked(IssuerDID, CID)).

test_is_revoked_true() ->
    IssuerDID = <<"did:macula:io.macula.test2">>,
    UcanToken = <<"test-token-2">>,
    ExpiresAt = erlang:system_time(second) + 3600,

    {ok, CID} = macula_ucan_revocation:revoke(IssuerDID, UcanToken, ExpiresAt),

    ?assert(macula_ucan_revocation:is_revoked(IssuerDID, CID)).

test_is_revoked_expired() ->
    IssuerDID = <<"did:macula:io.macula.expired">>,
    UcanToken = <<"expired-token">>,
    ExpiresAt = erlang:system_time(second) - 1,  %% Already expired

    {ok, CID} = macula_ucan_revocation:revoke(IssuerDID, UcanToken, ExpiresAt),

    %% Should NOT be considered revoked because it's already expired
    ?assertNot(macula_ucan_revocation:is_revoked(IssuerDID, CID)).

%%====================================================================
%% Rate Limiting Tests
%%====================================================================

test_rate_limit() ->
    IssuerDID = <<"did:macula:io.macula.ratelimit">>,
    ExpiresAt = erlang:system_time(second) + 3600,

    %% Issue 10 revocations (should all succeed)
    Results1 = [macula_ucan_revocation:revoke(IssuerDID,
                 <<"token-", (integer_to_binary(N))/binary>>,
                 ExpiresAt) || N <- lists:seq(1, 10)],

    ?assert(lists:all(fun({ok, _}) -> true; (_) -> false end, Results1)),

    %% 11th should fail due to rate limit
    Result11 = macula_ucan_revocation:revoke(IssuerDID, <<"token-11">>, ExpiresAt),
    ?assertEqual({error, rate_limited}, Result11),

    %% Check stats show rate limit hit
    Stats = macula_ucan_revocation:get_stats(),
    ?assertEqual(1, maps:get(rate_limit_hits, Stats)).

%%====================================================================
%% Message Handling Tests
%%====================================================================

test_handle_valid_revocation() ->
    Msg = #{
        <<"issuer_did">> => <<"did:macula:io.macula.sender">>,
        <<"ucan_cid">> => <<"bafyrei123">>,
        <<"revoked_at">> => erlang:system_time(second),
        <<"expires_at">> => erlang:system_time(second) + 3600,
        <<"signature">> => crypto:strong_rand_bytes(64)
    },

    Result = macula_ucan_revocation:handle_revocation_message(Msg),
    ?assertEqual(ok, Result),

    %% Should now be revoked
    ?assert(macula_ucan_revocation:is_revoked(
        <<"did:macula:io.macula.sender">>,
        <<"bafyrei123">>
    )).

test_handle_invalid_msg() ->
    %% Missing issuer_did
    Msg1 = #{
        <<"ucan_cid">> => <<"bafyrei123">>,
        <<"revoked_at">> => 12345,
        <<"expires_at">> => 67890,
        <<"signature">> => crypto:strong_rand_bytes(64)
    },
    ?assertEqual({error, missing_issuer_did},
                 macula_ucan_revocation:handle_revocation_message(Msg1)),

    %% Missing ucan_cid
    Msg2 = #{
        <<"issuer_did">> => <<"did:macula:test">>,
        <<"revoked_at">> => 12345,
        <<"expires_at">> => 67890,
        <<"signature">> => crypto:strong_rand_bytes(64)
    },
    ?assertEqual({error, missing_ucan_cid},
                 macula_ucan_revocation:handle_revocation_message(Msg2)),

    %% Not a map
    ?assertEqual({error, not_a_map},
                 macula_ucan_revocation:handle_revocation_message(<<"not a map">>)).

test_handle_invalid_signature() ->
    %% Empty signature
    Msg1 = #{
        <<"issuer_did">> => <<"did:macula:io.macula.test">>,
        <<"ucan_cid">> => <<"bafyrei456">>,
        <<"revoked_at">> => erlang:system_time(second),
        <<"expires_at">> => erlang:system_time(second) + 3600,
        <<"signature">> => <<>>
    },
    ?assertEqual({error, invalid_signature},
                 macula_ucan_revocation:handle_revocation_message(Msg1)),

    %% Signature too short
    Msg2 = Msg1#{<<"signature">> => <<"tooshort">>},
    ?assertEqual({error, invalid_signature},
                 macula_ucan_revocation:handle_revocation_message(Msg2)).

%%====================================================================
%% Stats and Cache Tests
%%====================================================================

test_get_stats() ->
    IssuerDID = <<"did:macula:io.macula.stats">>,
    UcanToken = <<"stats-token">>,
    ExpiresAt = erlang:system_time(second) + 3600,

    %% Initial stats
    Stats1 = macula_ucan_revocation:get_stats(),
    ?assertEqual(0, maps:get(revocations_issued, Stats1)),

    %% Issue a revocation
    {ok, _} = macula_ucan_revocation:revoke(IssuerDID, UcanToken, ExpiresAt),

    Stats2 = macula_ucan_revocation:get_stats(),
    ?assertEqual(1, maps:get(revocations_issued, Stats2)),
    ?assertEqual(1, maps:get(cache_size, Stats2)).

test_clear_cache() ->
    IssuerDID = <<"did:macula:io.macula.clear">>,
    ExpiresAt = erlang:system_time(second) + 3600,

    %% Add some revocations
    {ok, CID1} = macula_ucan_revocation:revoke(IssuerDID, <<"token1">>, ExpiresAt),
    {ok, CID2} = macula_ucan_revocation:revoke(IssuerDID, <<"token2">>, ExpiresAt),

    ?assert(macula_ucan_revocation:is_revoked(IssuerDID, CID1)),
    ?assert(macula_ucan_revocation:is_revoked(IssuerDID, CID2)),

    %% Clear cache
    ok = macula_ucan_revocation:clear_cache(),

    %% Should no longer be revoked
    ?assertNot(macula_ucan_revocation:is_revoked(IssuerDID, CID1)),
    ?assertNot(macula_ucan_revocation:is_revoked(IssuerDID, CID2)),

    %% Stats reset
    Stats = macula_ucan_revocation:get_stats(),
    ?assertEqual(0, maps:get(cache_size, Stats)).

%%====================================================================
%% CID and Signature Tests
%%====================================================================

test_compute_cid() ->
    Token1 = <<"eyJhbGciOiJFZDI1NTE5IiwidHlwIjoiSldUIn0.payload1.sig1">>,
    Token2 = <<"eyJhbGciOiJFZDI1NTE5IiwidHlwIjoiSldUIn0.payload2.sig2">>,

    CID1a = macula_ucan_revocation:compute_ucan_cid(Token1),
    CID1b = macula_ucan_revocation:compute_ucan_cid(Token1),
    CID2 = macula_ucan_revocation:compute_ucan_cid(Token2),

    %% Same token should produce same CID
    ?assertEqual(CID1a, CID1b),

    %% Different tokens should produce different CIDs
    ?assertNotEqual(CID1a, CID2),

    %% CID should be base64url encoded
    ?assert(is_binary(CID1a)),
    ?assert(byte_size(CID1a) > 0),
    %% Should not contain padding or non-URL-safe chars
    ?assertEqual(nomatch, binary:match(CID1a, <<"=">>)),
    ?assertEqual(nomatch, binary:match(CID1a, <<"+">>)),
    ?assertEqual(nomatch, binary:match(CID1a, <<"/">>)).

test_validate_signature() ->
    %% Valid 64-byte signature
    ValidSig = crypto:strong_rand_bytes(64),
    ValidMsg = #{<<"signature">> => ValidSig},
    ?assertEqual(ok, macula_ucan_revocation:validate_revocation_signature(ValidMsg)),

    %% Missing signature
    ?assertEqual({error, missing_signature},
                 macula_ucan_revocation:validate_revocation_signature(#{})),

    %% Empty signature
    ?assertEqual({error, empty_signature},
                 macula_ucan_revocation:validate_revocation_signature(#{<<"signature">> => <<>>})),

    %% Too short signature (< 64 bytes)
    ?assertEqual({error, invalid_signature_format},
                 macula_ucan_revocation:validate_revocation_signature(
                     #{<<"signature">> => <<"short">>})).

%%====================================================================
%% Multiple Revocation Tests
%%====================================================================

test_multiple_revocations() ->
    IssuerDID = <<"did:macula:io.macula.multi">>,
    ExpiresAt = erlang:system_time(second) + 3600,

    %% Revoke multiple tokens from same issuer
    {ok, CID1} = macula_ucan_revocation:revoke(IssuerDID, <<"multi-1">>, ExpiresAt),
    {ok, CID2} = macula_ucan_revocation:revoke(IssuerDID, <<"multi-2">>, ExpiresAt),
    {ok, CID3} = macula_ucan_revocation:revoke(IssuerDID, <<"multi-3">>, ExpiresAt),

    %% All should be different CIDs
    ?assertNotEqual(CID1, CID2),
    ?assertNotEqual(CID2, CID3),
    ?assertNotEqual(CID1, CID3),

    %% All should be revoked
    ?assert(macula_ucan_revocation:is_revoked(IssuerDID, CID1)),
    ?assert(macula_ucan_revocation:is_revoked(IssuerDID, CID2)),
    ?assert(macula_ucan_revocation:is_revoked(IssuerDID, CID3)).

test_different_issuers() ->
    Issuer1 = <<"did:macula:io.macula.issuer1">>,
    Issuer2 = <<"did:macula:io.macula.issuer2">>,
    Token = <<"shared-token">>,  %% Same token text
    ExpiresAt = erlang:system_time(second) + 3600,

    {ok, CID1} = macula_ucan_revocation:revoke(Issuer1, Token, ExpiresAt),
    {ok, CID2} = macula_ucan_revocation:revoke(Issuer2, Token, ExpiresAt),

    %% Same token produces same CID
    ?assertEqual(CID1, CID2),

    %% But revocations are per-issuer
    ?assert(macula_ucan_revocation:is_revoked(Issuer1, CID1)),
    ?assert(macula_ucan_revocation:is_revoked(Issuer2, CID2)),

    %% Non-matching issuer should not show revoked
    ?assertNot(macula_ucan_revocation:is_revoked(
        <<"did:macula:io.macula.other">>, CID1)).
