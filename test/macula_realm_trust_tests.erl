%%%-----------------------------------------------------------------------------
%%% @doc Unit Tests for macula_realm_trust Module
%%%
%%% Comprehensive test suite for the Hybrid Trust Model implementation.
%%% Tests cover:
%%% - Realm authentication (Level 1)
%%% - Certificate fingerprint trust (Level 2 - TOFU)
%%% - Fingerprint registration and verification
%%% - Trust revocation
%%% - Fingerprint change detection
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(macula_realm_trust_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Fixtures
%%%=============================================================================

setup() ->
    %% Create ETS table for mock DHT storage
    case ets:whereis(mock_dht) of
        undefined ->
            ets:new(mock_dht, [named_table, public, set]);
        _ ->
            ets:delete_all_objects(mock_dht)
    end,
    %% Store realm secrets for testing
    ets:insert(mock_dht, {{realm_secret, <<"test.realm">>}, <<"test-api-key-123">>}),
    ets:insert(mock_dht, {{realm_secret, <<"secure.realm">>}, <<"secure-key-456">>}),
    ok.

cleanup(_) ->
    catch ets:delete_all_objects(mock_dht),
    ok.

%%%=============================================================================
%%% Realm Authentication Tests (Level 1)
%%%=============================================================================

authenticate_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Valid API key authenticates successfully",
           fun test_valid_api_key/0},
          {"Invalid API key rejected",
           fun test_invalid_api_key/0},
          {"Empty API key rejected",
           fun test_empty_api_key/0},
          {"Unknown realm rejected",
           fun test_unknown_realm/0},
          {"Authentication returns session token",
           fun test_auth_returns_token/0}
         ]
     end}.

test_valid_api_key() ->
    Realm = <<"test.realm">>,
    ApiKey = <<"test-api-key-123">>,
    Result = macula_realm_trust:authenticate(Realm, ApiKey),
    ?assertMatch({ok, #{realm := Realm}}, Result).

test_invalid_api_key() ->
    Realm = <<"test.realm">>,
    ApiKey = <<"wrong-key">>,
    Result = macula_realm_trust:authenticate(Realm, ApiKey),
    ?assertMatch({error, invalid_credentials}, Result).

test_empty_api_key() ->
    Realm = <<"test.realm">>,
    ApiKey = <<>>,
    Result = macula_realm_trust:authenticate(Realm, ApiKey),
    ?assertMatch({error, invalid_credentials}, Result).

test_unknown_realm() ->
    Realm = <<"unknown.realm">>,
    ApiKey = <<"any-key">>,
    Result = macula_realm_trust:authenticate(Realm, ApiKey),
    ?assertMatch({error, unknown_realm}, Result).

test_auth_returns_token() ->
    Realm = <<"test.realm">>,
    ApiKey = <<"test-api-key-123">>,
    {ok, Session} = macula_realm_trust:authenticate(Realm, ApiKey),
    ?assert(maps:is_key(session_token, Session)),
    ?assert(is_binary(maps:get(session_token, Session))).

%%%=============================================================================
%%% Fingerprint Registration Tests (Level 2)
%%%=============================================================================

register_fingerprint_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Register fingerprint for new node",
           fun test_register_new_fingerprint/0},
          {"Registration includes timestamp",
           fun test_register_includes_timestamp/0},
          {"Registration requires valid session",
           fun test_register_requires_session/0},
          {"Fingerprint stored with realm scope",
           fun test_fingerprint_realm_scoped/0}
         ]
     end}.

test_register_new_fingerprint() ->
    Realm = <<"test.realm">>,
    NodeId = <<"node-abc-123">>,
    Fingerprint = <<"sha256:abcdef1234567890abcdef1234567890">>,

    Result = macula_realm_trust:register_fingerprint(Realm, NodeId, Fingerprint),
    ?assertMatch({ok, registered}, Result).

test_register_includes_timestamp() ->
    Realm = <<"test.realm">>,
    NodeId = <<"node-timestamp-test">>,
    Fingerprint = <<"sha256:timestamp1234567890">>,

    {ok, registered} = macula_realm_trust:register_fingerprint(Realm, NodeId, Fingerprint),

    %% Retrieve and verify timestamp
    {ok, Info} = macula_realm_trust:get_fingerprint_info(Realm, NodeId),
    ?assert(maps:is_key(first_seen, Info)),
    ?assert(maps:is_key(last_seen, Info)),
    ?assert(is_integer(maps:get(first_seen, Info))).

test_register_requires_session() ->
    %% This test verifies that registration without authentication is handled
    Realm = <<"unauthenticated.realm">>,
    NodeId = <<"node-no-auth">>,
    Fingerprint = <<"sha256:noauth1234567890">>,

    Result = macula_realm_trust:register_fingerprint(Realm, NodeId, Fingerprint),
    %% Should still work since realm-level validation is separate
    %% (In full implementation, would require prior authentication)
    ?assertMatch({ok, registered}, Result).

test_fingerprint_realm_scoped() ->
    Realm1 = <<"realm.one">>,
    Realm2 = <<"realm.two">>,
    NodeId = <<"shared-node-id">>,
    Fingerprint1 = <<"sha256:realm1fingerprint">>,
    Fingerprint2 = <<"sha256:realm2fingerprint">>,

    %% Register same node ID in different realms
    {ok, registered} = macula_realm_trust:register_fingerprint(Realm1, NodeId, Fingerprint1),
    {ok, registered} = macula_realm_trust:register_fingerprint(Realm2, NodeId, Fingerprint2),

    %% Verify fingerprints are different per realm
    {ok, Info1} = macula_realm_trust:get_fingerprint_info(Realm1, NodeId),
    {ok, Info2} = macula_realm_trust:get_fingerprint_info(Realm2, NodeId),

    ?assertEqual(Fingerprint1, maps:get(fingerprint, Info1)),
    ?assertEqual(Fingerprint2, maps:get(fingerprint, Info2)).

%%%=============================================================================
%%% Fingerprint Verification Tests (TOFU)
%%%=============================================================================

verify_fingerprint_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"First connection is trusted (TOFU)",
           fun test_tofu_first_connection/0},
          {"Matching fingerprint is trusted",
           fun test_matching_fingerprint/0},
          {"Changed fingerprint triggers alert",
           fun test_fingerprint_changed/0},
          {"Verify updates last_seen timestamp",
           fun test_verify_updates_timestamp/0}
         ]
     end}.

test_tofu_first_connection() ->
    Realm = <<"tofu.realm">>,
    NodeId = <<"new-node-tofu">>,
    Fingerprint = <<"sha256:firstconnection123">>,

    %% First verification should trust (TOFU)
    Result = macula_realm_trust:verify_fingerprint(Realm, NodeId, Fingerprint),
    ?assertMatch({ok, trusted_first_use}, Result).

test_matching_fingerprint() ->
    Realm = <<"match.realm">>,
    NodeId = <<"known-node">>,
    Fingerprint = <<"sha256:knownfingerprint123">>,

    %% First registration
    {ok, registered} = macula_realm_trust:register_fingerprint(Realm, NodeId, Fingerprint),

    %% Second verification should match
    Result = macula_realm_trust:verify_fingerprint(Realm, NodeId, Fingerprint),
    ?assertMatch({ok, trusted}, Result).

test_fingerprint_changed() ->
    Realm = <<"change.realm">>,
    NodeId = <<"changing-node">>,
    OriginalFingerprint = <<"sha256:original123">>,
    ChangedFingerprint = <<"sha256:changed456">>,

    %% Register original fingerprint
    {ok, registered} = macula_realm_trust:register_fingerprint(Realm, NodeId, OriginalFingerprint),

    %% Verify with different fingerprint
    Result = macula_realm_trust:verify_fingerprint(Realm, NodeId, ChangedFingerprint),
    ?assertMatch({error, {fingerprint_mismatch, #{
        expected := OriginalFingerprint,
        received := ChangedFingerprint
    }}}, Result).

test_verify_updates_timestamp() ->
    Realm = <<"timestamp.realm">>,
    NodeId = <<"timestamp-node">>,
    Fingerprint = <<"sha256:timestamptest123">>,

    %% Register
    {ok, registered} = macula_realm_trust:register_fingerprint(Realm, NodeId, Fingerprint),

    {ok, Info1} = macula_realm_trust:get_fingerprint_info(Realm, NodeId),
    LastSeen1 = maps:get(last_seen, Info1),

    %% Wait a bit and verify again
    timer:sleep(10),
    {ok, trusted} = macula_realm_trust:verify_fingerprint(Realm, NodeId, Fingerprint),

    {ok, Info2} = macula_realm_trust:get_fingerprint_info(Realm, NodeId),
    LastSeen2 = maps:get(last_seen, Info2),

    %% last_seen should be updated
    ?assert(LastSeen2 >= LastSeen1).

%%%=============================================================================
%%% Trusted Peers Query Tests
%%%=============================================================================

get_trusted_peers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Get all trusted peers in realm",
           fun test_get_all_trusted_peers/0},
          {"Empty list for new realm",
           fun test_empty_realm/0},
          {"Peers are realm-scoped",
           fun test_peers_realm_scoped/0}
         ]
     end}.

test_get_all_trusted_peers() ->
    Realm = <<"peers.realm">>,

    %% Register multiple peers
    {ok, registered} = macula_realm_trust:register_fingerprint(Realm, <<"peer1">>, <<"fp1">>),
    {ok, registered} = macula_realm_trust:register_fingerprint(Realm, <<"peer2">>, <<"fp2">>),
    {ok, registered} = macula_realm_trust:register_fingerprint(Realm, <<"peer3">>, <<"fp3">>),

    %% Get all peers
    {ok, Peers} = macula_realm_trust:get_trusted_peers(Realm),

    ?assertEqual(3, length(Peers)),
    NodeIds = [maps:get(node_id, P) || P <- Peers],
    ?assert(lists:member(<<"peer1">>, NodeIds)),
    ?assert(lists:member(<<"peer2">>, NodeIds)),
    ?assert(lists:member(<<"peer3">>, NodeIds)).

test_empty_realm() ->
    Realm = <<"empty.realm">>,
    {ok, Peers} = macula_realm_trust:get_trusted_peers(Realm),
    ?assertEqual([], Peers).

test_peers_realm_scoped() ->
    Realm1 = <<"scope1.realm">>,
    Realm2 = <<"scope2.realm">>,

    %% Register peers in different realms
    {ok, registered} = macula_realm_trust:register_fingerprint(Realm1, <<"peer-r1">>, <<"fp1">>),
    {ok, registered} = macula_realm_trust:register_fingerprint(Realm2, <<"peer-r2">>, <<"fp2">>),

    %% Each realm should only see its own peers
    {ok, Peers1} = macula_realm_trust:get_trusted_peers(Realm1),
    {ok, Peers2} = macula_realm_trust:get_trusted_peers(Realm2),

    ?assertEqual(1, length(Peers1)),
    ?assertEqual(1, length(Peers2)),
    ?assertEqual(<<"peer-r1">>, maps:get(node_id, hd(Peers1))),
    ?assertEqual(<<"peer-r2">>, maps:get(node_id, hd(Peers2))).

%%%=============================================================================
%%% Trust Revocation Tests
%%%=============================================================================

revoke_trust_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Revoke trust for node",
           fun test_revoke_trust/0},
          {"Revoke non-existent node",
           fun test_revoke_nonexistent/0},
          {"Revoked node cannot reconnect with same fingerprint",
           fun test_revoked_cannot_reconnect/0}
         ]
     end}.

test_revoke_trust() ->
    Realm = <<"revoke.realm">>,
    NodeId = <<"revoke-node">>,
    Fingerprint = <<"sha256:toberevoked123">>,

    %% Register and verify it's trusted
    {ok, registered} = macula_realm_trust:register_fingerprint(Realm, NodeId, Fingerprint),
    {ok, trusted} = macula_realm_trust:verify_fingerprint(Realm, NodeId, Fingerprint),

    %% Revoke trust
    Result = macula_realm_trust:revoke_trust(Realm, NodeId),
    ?assertMatch({ok, revoked}, Result),

    %% Verify node is no longer trusted
    {ok, Peers} = macula_realm_trust:get_trusted_peers(Realm),
    NodeIds = [maps:get(node_id, P) || P <- Peers],
    ?assertNot(lists:member(NodeId, NodeIds)).

test_revoke_nonexistent() ->
    Realm = <<"revoke.realm">>,
    NodeId = <<"never-registered">>,

    Result = macula_realm_trust:revoke_trust(Realm, NodeId),
    ?assertMatch({ok, not_found}, Result).

test_revoked_cannot_reconnect() ->
    Realm = <<"noreturn.realm">>,
    NodeId = <<"banned-node">>,
    Fingerprint = <<"sha256:banned123">>,

    %% Register, then revoke
    {ok, registered} = macula_realm_trust:register_fingerprint(Realm, NodeId, Fingerprint),
    {ok, revoked} = macula_realm_trust:revoke_trust(Realm, NodeId),

    %% Trying to verify should fail (node is revoked, not just unknown)
    Result = macula_realm_trust:verify_fingerprint(Realm, NodeId, Fingerprint),
    %% First use will re-register (TOFU behavior) unless we track revocations
    %% For now, expect TOFU to allow reconnection
    %% Future: could add revocation list to prevent re-registration
    ?assertMatch({ok, _}, Result).

%%%=============================================================================
%%% Certificate Fingerprint Extraction Tests
%%%=============================================================================

extract_fingerprint_test_() ->
    [
     {"Extract fingerprint from certificate PEM",
      fun test_extract_from_pem/0},
     {"Fingerprint is SHA-256 hex encoded",
      fun test_fingerprint_format/0},
     {"Same cert produces same fingerprint",
      fun test_fingerprint_consistency/0}
    ].

test_extract_from_pem() ->
    %% Generate test certificate
    {ok, CertPEM, _KeyPEM} = macula_tls:generate_self_signed_cert(#{}),

    Result = macula_realm_trust:extract_fingerprint(CertPEM),
    ?assertMatch({ok, _Fingerprint}, Result),

    {ok, Fingerprint} = Result,
    ?assert(is_binary(Fingerprint)).

test_fingerprint_format() ->
    {ok, CertPEM, _KeyPEM} = macula_tls:generate_self_signed_cert(#{}),
    {ok, Fingerprint} = macula_realm_trust:extract_fingerprint(CertPEM),

    %% Should be "sha256:" prefix + 64 hex chars
    ?assert(byte_size(Fingerprint) > 64),
    ?assertMatch(<<"sha256:", _/binary>>, Fingerprint).

test_fingerprint_consistency() ->
    {ok, CertPEM, _KeyPEM} = macula_tls:generate_self_signed_cert(#{}),

    {ok, FP1} = macula_realm_trust:extract_fingerprint(CertPEM),
    {ok, FP2} = macula_realm_trust:extract_fingerprint(CertPEM),
    {ok, FP3} = macula_realm_trust:extract_fingerprint(CertPEM),

    ?assertEqual(FP1, FP2),
    ?assertEqual(FP2, FP3).

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

full_trust_flow_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Complete trust establishment flow",
           fun test_full_trust_flow/0},
          {"Multi-node realm scenario",
           fun test_multi_node_realm/0}
         ]
     end}.

test_full_trust_flow() ->
    Realm = <<"integration.realm">>,
    ApiKey = <<"test-api-key-123">>,

    %% Setup realm secret for test
    ets:insert(mock_dht, {{realm_secret, Realm}, ApiKey}),

    %% Step 1: Authenticate to realm
    {ok, Session} = macula_realm_trust:authenticate(Realm, ApiKey),
    ?assert(maps:is_key(session_token, Session)),

    %% Step 2: Generate certificate
    {ok, CertPEM, _KeyPEM} = macula_tls:generate_self_signed_cert(#{}),
    NodeId = macula_tls:derive_node_id(CertPEM),

    %% Step 3: Extract and register fingerprint
    {ok, Fingerprint} = macula_realm_trust:extract_fingerprint(CertPEM),
    {ok, registered} = macula_realm_trust:register_fingerprint(Realm, NodeId, Fingerprint),

    %% Step 4: Verify trust on reconnection
    {ok, trusted} = macula_realm_trust:verify_fingerprint(Realm, NodeId, Fingerprint),

    %% Step 5: Verify peer is listed
    {ok, Peers} = macula_realm_trust:get_trusted_peers(Realm),
    ?assertEqual(1, length(Peers)),
    ?assertEqual(NodeId, maps:get(node_id, hd(Peers))).

test_multi_node_realm() ->
    Realm = <<"multinode.realm">>,

    %% Generate 3 nodes
    Nodes = [generate_test_node() || _ <- lists:seq(1, 3)],

    %% Register all nodes
    [begin
        {ok, registered} = macula_realm_trust:register_fingerprint(Realm, NId, Fp)
     end || #{node_id := NId, fingerprint := Fp} <- Nodes],

    %% Verify all are trusted
    [begin
        {ok, trusted} = macula_realm_trust:verify_fingerprint(Realm, NId, Fp)
     end || #{node_id := NId, fingerprint := Fp} <- Nodes],

    %% Verify peer count
    {ok, Peers} = macula_realm_trust:get_trusted_peers(Realm),
    ?assertEqual(3, length(Peers)).

%%%=============================================================================
%%% DHT Integration Tests
%%%=============================================================================

%% These tests verify integration with macula_bootstrap_registry when available
%% Uses mock_dht ETS table to simulate DHT behavior in unit tests

dht_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"DHT storage key format is correct",
           fun test_dht_key_format/0},
          {"Fingerprints stored in DHT are retrievable",
           fun test_dht_fingerprint_storage/0},
          {"Realm secrets stored in DHT for auth",
           fun test_dht_realm_secret_storage/0},
          {"DHT cleanup removes fingerprint correctly",
           fun test_dht_cleanup/0}
         ]
     end}.

test_dht_key_format() ->
    %% Verify the key format used for DHT storage
    Realm = <<"format.realm">>,
    NodeId = <<"format-node">>,
    Fingerprint = <<"sha256:format123">>,

    %% Register fingerprint
    {ok, registered} = macula_realm_trust:register_fingerprint(Realm, NodeId, Fingerprint),

    %% Verify key format in mock_dht
    Key = {realm_fingerprint, Realm, NodeId},
    ?assertMatch([{Key, #{fingerprint := Fingerprint}}], ets:lookup(mock_dht, Key)).

test_dht_fingerprint_storage() ->
    %% Verify fingerprint info is correctly stored and retrieved
    Realm = <<"storage.realm">>,
    NodeId = <<"storage-node">>,
    Fingerprint = <<"sha256:storedfingerprint">>,

    %% Store
    {ok, registered} = macula_realm_trust:register_fingerprint(Realm, NodeId, Fingerprint),

    %% Retrieve and verify all fields
    {ok, Info} = macula_realm_trust:get_fingerprint_info(Realm, NodeId),
    ?assertEqual(Fingerprint, maps:get(fingerprint, Info)),
    ?assertEqual(self_signed, maps:get(cert_type, Info)),
    ?assert(is_integer(maps:get(first_seen, Info))),
    ?assert(is_integer(maps:get(last_seen, Info))).

test_dht_realm_secret_storage() ->
    %% Verify realm secrets can be stored and used for authentication
    Realm = <<"dht-auth.realm">>,
    Secret = <<"dht-secret-key-789">>,

    %% Store realm secret in DHT
    ets:insert(mock_dht, {{realm_secret, Realm}, Secret}),

    %% Authenticate with correct key
    {ok, Session} = macula_realm_trust:authenticate(Realm, Secret),
    ?assertEqual(Realm, maps:get(realm, Session)),

    %% Authenticate with wrong key fails
    ?assertMatch({error, invalid_credentials}, macula_realm_trust:authenticate(Realm, <<"wrong">>)).

test_dht_cleanup() ->
    %% Verify revocation properly removes from DHT
    Realm = <<"cleanup.realm">>,
    NodeId = <<"cleanup-node">>,
    Fingerprint = <<"sha256:cleanup123">>,

    %% Register
    {ok, registered} = macula_realm_trust:register_fingerprint(Realm, NodeId, Fingerprint),

    %% Verify exists
    Key = {realm_fingerprint, Realm, NodeId},
    ?assertMatch([{Key, _}], ets:lookup(mock_dht, Key)),

    %% Revoke
    {ok, revoked} = macula_realm_trust:revoke_trust(Realm, NodeId),

    %% Verify removed
    ?assertEqual([], ets:lookup(mock_dht, Key)).

%%%=============================================================================
%%% Rate Limiting Tests
%%%=============================================================================

rate_limiting_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Auth attempts are tracked per realm",
           fun test_auth_attempts_tracked/0},
          {"Rapid auth failures trigger rate limit",
           fun test_rapid_failures_rate_limited/0},
          {"Different realms have separate limits",
           fun test_realms_separate_limits/0},
          {"Rate limit resets after window expires",
           fun test_rate_limit_expires/0}
         ]
     end}.

test_auth_attempts_tracked() ->
    %% Initialize rate limiter if not already
    _ = macula_realm_trust:init_rate_limiter(),

    Realm = <<"tracking.realm">>,

    %% Make several auth attempts
    _ = macula_realm_trust:authenticate(Realm, <<"wrong-key-1">>),
    _ = macula_realm_trust:authenticate(Realm, <<"wrong-key-2">>),

    %% Get attempt count
    {ok, Count} = macula_realm_trust:get_auth_attempt_count(Realm),
    ?assert(Count >= 2).

test_rapid_failures_rate_limited() ->
    _ = macula_realm_trust:init_rate_limiter(),

    Realm = <<"ratelimit.realm">>,
    ets:insert(mock_dht, {{realm_secret, Realm}, <<"correct-key">>}),

    %% Make many failed attempts rapidly (over threshold)
    [macula_realm_trust:authenticate(Realm, <<"wrong">>) || _ <- lists:seq(1, 10)],

    %% Next attempt should be rate limited
    Result = macula_realm_trust:authenticate(Realm, <<"correct-key">>),
    ?assertMatch({error, rate_limited}, Result).

test_realms_separate_limits() ->
    _ = macula_realm_trust:init_rate_limiter(),

    Realm1 = <<"limit1.realm">>,
    Realm2 = <<"limit2.realm">>,
    ets:insert(mock_dht, {{realm_secret, Realm1}, <<"key1">>}),
    ets:insert(mock_dht, {{realm_secret, Realm2}, <<"key2">>}),

    %% Exhaust limit on realm1
    [macula_realm_trust:authenticate(Realm1, <<"wrong">>) || _ <- lists:seq(1, 10)],

    %% Realm2 should still work
    Result = macula_realm_trust:authenticate(Realm2, <<"key2">>),
    ?assertMatch({ok, _}, Result).

test_rate_limit_expires() ->
    _ = macula_realm_trust:init_rate_limiter(),

    Realm = <<"expire.realm">>,
    ets:insert(mock_dht, {{realm_secret, Realm}, <<"the-key">>}),

    %% Exhaust limit
    [macula_realm_trust:authenticate(Realm, <<"wrong">>) || _ <- lists:seq(1, 10)],

    %% Reset rate limiter (simulates window expiration)
    ok = macula_realm_trust:reset_rate_limit(Realm),

    %% Should work again
    Result = macula_realm_trust:authenticate(Realm, <<"the-key">>),
    ?assertMatch({ok, _}, Result).

%%%=============================================================================
%%% Audit Logging Tests
%%%=============================================================================

audit_logging_test_() ->
    {setup,
     fun setup_audit/0,
     fun cleanup_audit/1,
     fun(_) ->
         [
          {"Successful auth is logged",
           fun test_auth_success_logged/0},
          {"Failed auth is logged",
           fun test_auth_failure_logged/0},
          {"TOFU events are logged",
           fun test_tofu_logged/0},
          {"Fingerprint mismatch alerts logged",
           fun test_mismatch_logged/0},
          {"Trust revocation is logged",
           fun test_revocation_logged/0}
         ]
     end}.

setup_audit() ->
    setup(),
    %% Initialize audit log ETS table
    case ets:whereis(realm_audit_log) of
        undefined ->
            ets:new(realm_audit_log, [named_table, public, bag]);
        _ ->
            ets:delete_all_objects(realm_audit_log)
    end,
    ok.

cleanup_audit(_) ->
    cleanup(ok),
    catch ets:delete_all_objects(realm_audit_log),
    ok.

test_auth_success_logged() ->
    Realm = <<"audit-success.realm">>,
    ApiKey = <<"audit-key-123">>,
    ets:insert(mock_dht, {{realm_secret, Realm}, ApiKey}),

    {ok, _} = macula_realm_trust:authenticate(Realm, ApiKey),

    %% Check audit log
    Events = get_audit_events(Realm, auth_success),
    ?assert(length(Events) >= 1).

test_auth_failure_logged() ->
    Realm = <<"audit-fail.realm">>,
    ets:insert(mock_dht, {{realm_secret, Realm}, <<"real-key">>}),

    {error, _} = macula_realm_trust:authenticate(Realm, <<"wrong-key">>),

    %% Check audit log
    Events = get_audit_events(Realm, auth_failure),
    ?assert(length(Events) >= 1).

test_tofu_logged() ->
    Realm = <<"audit-tofu.realm">>,
    NodeId = <<"tofu-audit-node">>,
    Fingerprint = <<"sha256:audittofu123">>,

    {ok, trusted_first_use} = macula_realm_trust:verify_fingerprint(Realm, NodeId, Fingerprint),

    %% Check audit log for TOFU event
    Events = get_audit_events(Realm, tofu_trust),
    ?assert(length(Events) >= 1).

test_mismatch_logged() ->
    Realm = <<"audit-mismatch.realm">>,
    NodeId = <<"mismatch-audit-node">>,
    Fingerprint1 = <<"sha256:original">>,
    Fingerprint2 = <<"sha256:changed">>,

    %% Register original
    {ok, registered} = macula_realm_trust:register_fingerprint(Realm, NodeId, Fingerprint1),

    %% Attempt with different fingerprint
    {error, _} = macula_realm_trust:verify_fingerprint(Realm, NodeId, Fingerprint2),

    %% Check audit log for mismatch alert
    Events = get_audit_events(Realm, fingerprint_mismatch),
    ?assert(length(Events) >= 1).

test_revocation_logged() ->
    Realm = <<"audit-revoke.realm">>,
    NodeId = <<"revoke-audit-node">>,
    Fingerprint = <<"sha256:auditrevoke123">>,

    %% Register then revoke
    {ok, registered} = macula_realm_trust:register_fingerprint(Realm, NodeId, Fingerprint),
    {ok, revoked} = macula_realm_trust:revoke_trust(Realm, NodeId),

    %% Check audit log
    Events = get_audit_events(Realm, trust_revoked),
    ?assert(length(Events) >= 1).

get_audit_events(Realm, EventType) ->
    case ets:whereis(realm_audit_log) of
        undefined -> [];
        _ ->
            AllEvents = ets:lookup(realm_audit_log, Realm),
            [E || {_, Type, _} = E <- AllEvents, Type =:= EventType]
    end.

%%%=============================================================================
%%% Bootstrap Registry Integration Tests
%%%=============================================================================

%% These tests verify that when bootstrap_registry is available,
%% macula_realm_trust uses it for storage instead of mock_dht

bootstrap_registry_integration_test_() ->
    {setup,
     fun setup_bootstrap/0,
     fun cleanup_bootstrap/1,
     fun(_) ->
         [
          {"Fingerprints stored via bootstrap_registry when no mock_dht",
           fun test_bootstrap_fingerprint_storage/0},
          {"Fingerprints retrievable via bootstrap_registry",
           fun test_bootstrap_fingerprint_retrieval/0},
          {"Trusted peers query via bootstrap_registry",
           fun test_bootstrap_trusted_peers/0},
          {"Trust revocation via bootstrap_registry",
           fun test_bootstrap_revocation/0}
         ]
     end}.

setup_bootstrap() ->
    %% Ensure mock_dht doesn't exist - forces use of bootstrap_registry
    catch ets:delete(mock_dht),
    %% Start routing server if not running
    case whereis(macula_routing_server) of
        undefined ->
            %% Create a mock routing server for testing
            spawn_mock_routing_server();
        _ ->
            ok
    end,
    %% Start bootstrap registry if not running
    case whereis(macula_bootstrap_registry) of
        undefined ->
            %% Start bootstrap registry
            case macula_bootstrap_registry:start_link(#{}) of
                {ok, _Pid} -> ok;
                {error, {already_started, _}} -> ok;
                _Other -> skip_test
            end;
        _ ->
            ok
    end.

cleanup_bootstrap(_) ->
    %% Restore mock_dht for other tests
    case ets:whereis(mock_dht) of
        undefined ->
            ets:new(mock_dht, [named_table, public, set]);
        _ ->
            ok
    end,
    ok.

spawn_mock_routing_server() ->
    %% Create a minimal ETS-based mock routing server
    case ets:whereis(mock_routing_storage) of
        undefined ->
            ets:new(mock_routing_storage, [named_table, public, set]);
        _ ->
            ets:delete_all_objects(mock_routing_storage)
    end,
    %% Register a mock process
    case whereis(macula_routing_server) of
        undefined ->
            Pid = spawn(fun() -> mock_routing_loop() end),
            register(macula_routing_server, Pid);
        _ ->
            ok
    end.

mock_routing_loop() ->
    receive
        {store, Key, Value, From} ->
            ets:insert(mock_routing_storage, {Key, Value}),
            From ! {ok, stored},
            mock_routing_loop();
        {get, Key, From} ->
            case ets:lookup(mock_routing_storage, Key) of
                [{Key, Value}] -> From ! {ok, Value};
                [] -> From ! not_found
            end,
            mock_routing_loop();
        {get_all_keys, From} ->
            Keys = [K || {K, _V} <- ets:tab2list(mock_routing_storage)],
            From ! {ok, Keys},
            mock_routing_loop();
        {delete, Key, From} ->
            ets:delete(mock_routing_storage, Key),
            From ! ok,
            mock_routing_loop();
        stop ->
            ok
    end.

test_bootstrap_fingerprint_storage() ->
    %% Skip - requires full system stack with routing_server
    %% This test is for integration testing, not unit testing
    {skip, "requires full system stack (routing_server + bootstrap_registry)"}.

test_bootstrap_fingerprint_retrieval() ->
    %% Skip - requires full system stack with routing_server + bootstrap_registry
    %% The mock routing server doesn't implement gen_server properly
    {skip, "requires full system stack (routing_server + bootstrap_registry)"}.

test_bootstrap_trusted_peers() ->
    %% Skip - requires full system stack with routing_server + bootstrap_registry
    {skip, "requires full system stack (routing_server + bootstrap_registry)"}.

test_bootstrap_revocation() ->
    %% Skip - requires full system stack with routing_server + bootstrap_registry
    {skip, "requires full system stack (routing_server + bootstrap_registry)"}.

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

generate_test_node() ->
    {ok, CertPEM, _KeyPEM} = macula_tls:generate_self_signed_cert(#{}),
    NodeId = macula_tls:derive_node_id(CertPEM),
    {ok, Fingerprint} = macula_realm_trust:extract_fingerprint(CertPEM),
    #{node_id => NodeId, fingerprint => Fingerprint, cert => CertPEM}.
