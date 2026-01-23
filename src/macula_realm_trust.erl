%%%-----------------------------------------------------------------------------
%%% @doc Realm Trust Management for Hybrid Trust Model
%%%
%%% Implements the Hybrid Trust Model (ADR-001) with three trust levels:
%%% - Level 1: Realm Authentication (API key/token validation)
%%% - Level 2: Certificate Trust (TOFU within authenticated realm)
%%% - Level 3: Optional CA-signed certificates for seed nodes
%%%
%%% This module manages:
%%% - Realm authentication via API keys
%%% - Certificate fingerprint registration and verification
%%% - Trust On First Use (TOFU) pattern
%%% - Fingerprint change detection
%%% - Trust revocation
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(macula_realm_trust).

%% Public API
-export([
    authenticate/2,
    register_fingerprint/3,
    verify_fingerprint/3,
    get_fingerprint_info/2,
    get_trusted_peers/1,
    revoke_trust/2,
    extract_fingerprint/1
]).

%% Rate Limiting API
-export([
    init_rate_limiter/0,
    get_auth_attempt_count/1,
    reset_rate_limit/1
]).

%% Configuration
-define(MAX_AUTH_ATTEMPTS, 5).
-define(RATE_LIMIT_WINDOW_MS, 60000). % 1 minute

%% Types
-export_type([
    realm/0,
    node_id/0,
    fingerprint/0,
    session/0
]).

-type realm() :: binary().
-type node_id() :: binary().
-type fingerprint() :: binary().
-type session() :: #{
    realm := realm(),
    session_token := binary(),
    authenticated_at := integer()
}.

-include_lib("public_key/include/public_key.hrl").

%%%=============================================================================
%%% Realm Authentication (Level 1)
%%%=============================================================================

%% @doc Authenticate to a realm using API key
%% Returns a session map on success, error tuple on failure
%% Implements rate limiting per realm
-spec authenticate(realm(), binary()) -> {ok, session()} | {error, term()}.
authenticate(_Realm, <<>>) ->
    {error, invalid_credentials};
authenticate(Realm, ApiKey) when is_binary(Realm), is_binary(ApiKey) ->
    case check_rate_limit(Realm) of
        ok ->
            do_authenticate(Realm, ApiKey);
        {error, rate_limited} = Error ->
            Error
    end.

do_authenticate(Realm, ApiKey) ->
    case get_realm_secret(Realm) of
        {ok, ExpectedKey} ->
            verify_api_key(Realm, ApiKey, ExpectedKey);
        {error, not_found} ->
            record_auth_attempt(Realm, failed),
            audit_log(Realm, auth_failure, #{reason => unknown_realm}),
            {error, unknown_realm}
    end.

verify_api_key(Realm, ApiKey, ApiKey) ->
    %% Matching keys - create session
    Session = #{
        realm => Realm,
        session_token => generate_session_token(),
        authenticated_at => erlang:system_time(millisecond)
    },
    reset_auth_attempts(Realm),
    audit_log(Realm, auth_success, #{}),
    {ok, Session};
verify_api_key(Realm, _ApiKey, _ExpectedKey) ->
    record_auth_attempt(Realm, failed),
    audit_log(Realm, auth_failure, #{reason => invalid_credentials}),
    {error, invalid_credentials}.

%% Get realm secret from storage (mock_dht for testing, DHT for production)
get_realm_secret(Realm) ->
    Key = {realm_secret, Realm},
    case ets:whereis(mock_dht) of
        undefined ->
            %% Production: use DHT lookup
            get_realm_secret_from_dht(Realm);
        _Tid ->
            %% Test: use mock DHT
            case ets:lookup(mock_dht, Key) of
                [{Key, Secret}] -> {ok, Secret};
                [] -> {error, not_found}
            end
    end.

get_realm_secret_from_dht(Realm) ->
    %% Use bootstrap_registry for realm secret lookup in production
    Key = make_dht_key(realm_secret, Realm),
    case whereis(macula_bootstrap_registry) of
        undefined ->
            {error, not_found};
        _ ->
            macula_bootstrap_registry:lookup(Key)
    end.

generate_session_token() ->
    Bytes = crypto:strong_rand_bytes(32),
    base64:encode(Bytes).

%%%=============================================================================
%%% Fingerprint Registration (Level 2)
%%%=============================================================================

%% @doc Register a certificate fingerprint for a node in a realm
-spec register_fingerprint(realm(), node_id(), fingerprint()) -> {ok, registered}.
register_fingerprint(Realm, NodeId, Fingerprint)
  when is_binary(Realm), is_binary(NodeId), is_binary(Fingerprint) ->
    Now = erlang:system_time(millisecond),
    Info = #{
        fingerprint => Fingerprint,
        first_seen => Now,
        last_seen => Now,
        cert_type => self_signed
    },
    store_fingerprint(Realm, NodeId, Info),
    {ok, registered}.

store_fingerprint(Realm, NodeId, Info) ->
    Key = fingerprint_key(Realm, NodeId),
    case ets:whereis(mock_dht) of
        undefined ->
            %% Production: store in DHT
            store_fingerprint_in_dht(Key, Info);
        _Tid ->
            %% Test: store in mock DHT
            ets:insert(mock_dht, {Key, Info})
    end.

store_fingerprint_in_dht(Key, Info) ->
    %% Use bootstrap_registry for fingerprint storage in production
    case whereis(macula_bootstrap_registry) of
        undefined ->
            ok;
        _ ->
            %% Convert tuple key to binary for DHT storage
            BinaryKey = term_to_binary(Key),
            macula_bootstrap_registry:store(BinaryKey, Info)
    end.

fingerprint_key(Realm, NodeId) ->
    {realm_fingerprint, Realm, NodeId}.

%% @private Create a DHT key from type and identifier
make_dht_key(Type, Identifier) ->
    term_to_binary({Type, Identifier}).

%%%=============================================================================
%%% Fingerprint Verification (TOFU)
%%%=============================================================================

%% @doc Verify a certificate fingerprint for a node in a realm
%% Implements Trust On First Use (TOFU) - first connection is automatically trusted
-spec verify_fingerprint(realm(), node_id(), fingerprint()) ->
    {ok, trusted | trusted_first_use} |
    {error, {fingerprint_mismatch, #{expected := fingerprint(), received := fingerprint()}}}.
verify_fingerprint(Realm, NodeId, Fingerprint)
  when is_binary(Realm), is_binary(NodeId), is_binary(Fingerprint) ->
    case get_fingerprint_info(Realm, NodeId) of
        {ok, Info} ->
            do_verify_fingerprint(Realm, NodeId, Fingerprint, Info);
        {error, not_found} ->
            %% TOFU: First connection, trust and register
            {ok, registered} = register_fingerprint(Realm, NodeId, Fingerprint),
            audit_log(Realm, tofu_trust, #{node_id => NodeId}),
            {ok, trusted_first_use}
    end.

do_verify_fingerprint(Realm, NodeId, Fingerprint, #{fingerprint := StoredFingerprint})
  when Fingerprint =:= StoredFingerprint ->
    %% Matching fingerprint - update last_seen and return trusted
    update_last_seen(Realm, NodeId),
    {ok, trusted};
do_verify_fingerprint(Realm, NodeId, Fingerprint, #{fingerprint := StoredFingerprint}) ->
    %% Fingerprint mismatch - possible MITM attack!
    audit_log(Realm, fingerprint_mismatch, #{
        node_id => NodeId,
        expected => StoredFingerprint,
        received => Fingerprint
    }),
    {error, {fingerprint_mismatch, #{
        expected => StoredFingerprint,
        received => Fingerprint
    }}}.

update_last_seen(Realm, NodeId) ->
    Key = fingerprint_key(Realm, NodeId),
    Now = erlang:system_time(millisecond),
    case ets:whereis(mock_dht) of
        undefined ->
            update_last_seen_in_dht(Key, Now);
        _Tid ->
            case ets:lookup(mock_dht, Key) of
                [{Key, Info}] ->
                    UpdatedInfo = Info#{last_seen => Now},
                    ets:insert(mock_dht, {Key, UpdatedInfo});
                [] ->
                    ok
            end
    end.

update_last_seen_in_dht(Key, Now) ->
    %% Update last_seen in bootstrap_registry
    case whereis(macula_bootstrap_registry) of
        undefined ->
            ok;
        _ ->
            BinaryKey = term_to_binary(Key),
            case macula_bootstrap_registry:lookup(BinaryKey) of
                {ok, Info} ->
                    UpdatedInfo = Info#{last_seen => Now},
                    macula_bootstrap_registry:store(BinaryKey, UpdatedInfo);
                {error, not_found} ->
                    ok
            end
    end.

%%%=============================================================================
%%% Fingerprint Info Retrieval
%%%=============================================================================

%% @doc Get fingerprint info for a node in a realm
-spec get_fingerprint_info(realm(), node_id()) -> {ok, map()} | {error, not_found}.
get_fingerprint_info(Realm, NodeId) when is_binary(Realm), is_binary(NodeId) ->
    Key = fingerprint_key(Realm, NodeId),
    case ets:whereis(mock_dht) of
        undefined ->
            get_fingerprint_from_dht(Key);
        _Tid ->
            case ets:lookup(mock_dht, Key) of
                [{Key, Info}] -> {ok, Info};
                [] -> {error, not_found}
            end
    end.

get_fingerprint_from_dht(Key) ->
    %% Retrieve fingerprint from bootstrap_registry
    case whereis(macula_bootstrap_registry) of
        undefined ->
            {error, not_found};
        _ ->
            BinaryKey = term_to_binary(Key),
            macula_bootstrap_registry:lookup(BinaryKey)
    end.

%%%=============================================================================
%%% Trusted Peers Query
%%%=============================================================================

%% @doc Get all trusted peers in a realm
-spec get_trusted_peers(realm()) -> {ok, [map()]}.
get_trusted_peers(Realm) when is_binary(Realm) ->
    case ets:whereis(mock_dht) of
        undefined ->
            get_trusted_peers_from_dht(Realm);
        _Tid ->
            Peers = get_peers_from_mock_dht(Realm),
            {ok, Peers}
    end.

get_peers_from_mock_dht(Realm) ->
    Pattern = {{realm_fingerprint, Realm, '_'}, '_'},
    Matches = ets:match_object(mock_dht, Pattern),
    [#{node_id => NodeId, fingerprint => maps:get(fingerprint, Info)}
     || {{realm_fingerprint, R, NodeId}, Info} <- Matches, R =:= Realm].

get_trusted_peers_from_dht(Realm) ->
    %% Query all fingerprints for a realm from DHT
    %% This requires scanning all keys and filtering by realm prefix
    case whereis(macula_routing_server) of
        undefined ->
            {ok, []};
        RoutingServerPid ->
            case macula_routing_server:get_all_keys(RoutingServerPid) of
                {ok, Keys} ->
                    Peers = filter_realm_peers(Realm, Keys),
                    {ok, Peers};
                _Error ->
                    {ok, []}
            end
    end.

filter_realm_peers(Realm, Keys) ->
    %% Filter keys that are fingerprint keys for this realm
    lists:filtermap(
        fun(BinaryKey) ->
            filter_and_lookup_peer(Realm, BinaryKey)
        end,
        Keys
    ).

filter_and_lookup_peer(Realm, BinaryKey) ->
    handle_binary_term_conversion(catch binary_to_term(BinaryKey), Realm, BinaryKey).

%% @private Conversion failed
handle_binary_term_conversion({'EXIT', _}, _Realm, _BinaryKey) ->
    false;
%% @private Conversion succeeded - check for realm match
handle_binary_term_conversion({realm_fingerprint, Realm, NodeId}, Realm, BinaryKey) ->
    lookup_peer_info(macula_bootstrap_registry:lookup(BinaryKey), NodeId);
%% @private Key doesn't match expected pattern or realm
handle_binary_term_conversion(_, _Realm, _BinaryKey) ->
    false.

%% @private Lookup succeeded
lookup_peer_info({ok, Info}, NodeId) ->
    {true, #{node_id => NodeId, fingerprint => maps:get(fingerprint, Info)}};
%% @private Lookup failed
lookup_peer_info(_, _NodeId) ->
    false.

%%%=============================================================================
%%% Trust Revocation
%%%=============================================================================

%% @doc Revoke trust for a node in a realm
-spec revoke_trust(realm(), node_id()) -> {ok, revoked | not_found}.
revoke_trust(Realm, NodeId) when is_binary(Realm), is_binary(NodeId) ->
    Key = fingerprint_key(Realm, NodeId),
    Result = case ets:whereis(mock_dht) of
        undefined ->
            revoke_trust_in_dht(Key);
        _Tid ->
            case ets:lookup(mock_dht, Key) of
                [{Key, _Info}] ->
                    ets:delete(mock_dht, Key),
                    {ok, revoked};
                [] ->
                    {ok, not_found}
            end
    end,
    case Result of
        {ok, revoked} ->
            audit_log(Realm, trust_revoked, #{node_id => NodeId});
        _ ->
            ok
    end,
    Result.

revoke_trust_in_dht(Key) ->
    %% Delete fingerprint from bootstrap_registry
    case whereis(macula_bootstrap_registry) of
        undefined ->
            {ok, not_found};
        _ ->
            BinaryKey = term_to_binary(Key),
            case macula_bootstrap_registry:delete(BinaryKey) of
                ok ->
                    {ok, revoked};
                {error, not_found} ->
                    {ok, not_found}
            end
    end.

%%%=============================================================================
%%% Certificate Fingerprint Extraction
%%%=============================================================================

%% @doc Extract SHA-256 fingerprint from certificate PEM
-spec extract_fingerprint(binary()) -> {ok, fingerprint()} | {error, term()}.
extract_fingerprint(CertPEM) when is_binary(CertPEM) ->
    case public_key:pem_decode(CertPEM) of
        [{'Certificate', CertDER, not_encrypted}] ->
            Hash = crypto:hash(sha256, CertDER),
            HexHash = binary_to_hex(Hash),
            Fingerprint = <<"sha256:", HexHash/binary>>,
            {ok, Fingerprint};
        [] ->
            {error, invalid_pem};
        _ ->
            {error, unexpected_pem_format}
    end.

binary_to_hex(Bin) when is_binary(Bin) ->
    list_to_binary([io_lib:format("~2.16.0b", [Byte]) || <<Byte>> <= Bin]).

%%%=============================================================================
%%% Rate Limiting
%%%=============================================================================

%% @doc Initialize the rate limiter ETS table
-spec init_rate_limiter() -> ok.
init_rate_limiter() ->
    case ets:whereis(realm_rate_limit) of
        undefined ->
            ets:new(realm_rate_limit, [named_table, public, set]);
        _ ->
            ok
    end,
    ok.

%% @doc Get the current auth attempt count for a realm
-spec get_auth_attempt_count(realm()) -> {ok, non_neg_integer()}.
get_auth_attempt_count(Realm) ->
    case ets:whereis(realm_rate_limit) of
        undefined ->
            {ok, 0};
        _Tid ->
            case ets:lookup(realm_rate_limit, Realm) of
                [{Realm, Count, _Timestamp}] -> {ok, Count};
                [] -> {ok, 0}
            end
    end.

%% @doc Reset the rate limit for a realm
-spec reset_rate_limit(realm()) -> ok.
reset_rate_limit(Realm) ->
    case ets:whereis(realm_rate_limit) of
        undefined ->
            ok;
        _Tid ->
            ets:delete(realm_rate_limit, Realm),
            ok
    end.

%% @private Check if a realm is rate limited
-spec check_rate_limit(realm()) -> ok | {error, rate_limited}.
check_rate_limit(Realm) ->
    case ets:whereis(realm_rate_limit) of
        undefined ->
            ok;
        _Tid ->
            case ets:lookup(realm_rate_limit, Realm) of
                [{Realm, Count, Timestamp}] ->
                    check_rate_limit_window(Count, Timestamp);
                [] ->
                    ok
            end
    end.

check_rate_limit_window(Count, Timestamp) when Count >= ?MAX_AUTH_ATTEMPTS ->
    Now = erlang:system_time(millisecond),
    case Now - Timestamp < ?RATE_LIMIT_WINDOW_MS of
        true ->
            {error, rate_limited};
        false ->
            ok
    end;
check_rate_limit_window(_Count, _Timestamp) ->
    ok.

%% @private Record an authentication attempt
-spec record_auth_attempt(realm(), failed | success) -> ok.
record_auth_attempt(Realm, failed) ->
    case ets:whereis(realm_rate_limit) of
        undefined ->
            ok;
        _Tid ->
            Now = erlang:system_time(millisecond),
            case ets:lookup(realm_rate_limit, Realm) of
                [{Realm, Count, _OldTimestamp}] ->
                    ets:insert(realm_rate_limit, {Realm, Count + 1, Now});
                [] ->
                    ets:insert(realm_rate_limit, {Realm, 1, Now})
            end,
            ok
    end;
record_auth_attempt(_Realm, success) ->
    ok.

%% @private Reset auth attempts after successful authentication
-spec reset_auth_attempts(realm()) -> ok.
reset_auth_attempts(Realm) ->
    reset_rate_limit(Realm).

%%%=============================================================================
%%% Audit Logging
%%%=============================================================================

%% @private Log an audit event
%% Stores events in realm_audit_log ETS table (for testing)
%% In production, would write to external logging system
-spec audit_log(realm(), atom(), map()) -> ok.
audit_log(Realm, EventType, Data) ->
    Timestamp = erlang:system_time(millisecond),
    Event = {Realm, EventType, Data#{timestamp => Timestamp}},
    case ets:whereis(realm_audit_log) of
        undefined ->
            %% No audit log table - skip (production would use different backend)
            ok;
        _Tid ->
            ets:insert(realm_audit_log, Event),
            ok
    end.
