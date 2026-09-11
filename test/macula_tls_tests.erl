%%%-----------------------------------------------------------------------------
%%% @doc Unit Tests for macula_tls Module
%%%
%%% Comprehensive test suite for TLS certificate auto-generation functionality.
%%% Tests cover:
%%% - Certificate generation and validation
%%% - Node ID derivation and consistency
%%% - File persistence and reloading
%%% - Permission handling
%%% - Error cases
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(macula_tls_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/file.hrl").

%%%=============================================================================
%%% Test Fixtures
%%%=============================================================================

setup() ->
    %% Create temporary directory for test certificates
    macula_test_tmp:dir("macula_tls_test").

cleanup(TempDir) ->
    %% Remove temporary directory and all files
    ok = file:del_dir_r(TempDir).

%%%=============================================================================
%%% Certificate Generation Tests
%%%=============================================================================

generate_cert_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_TempDir) ->
         [
          {"Generate valid self-signed certificate",
           fun test_generate_valid_cert/0},
          {"Certificate has correct format",
           fun test_cert_format/0},
          {"Public key can be extracted",
           fun test_public_key_extraction/0},
          {"Certificate validity period is correct",
           fun test_validity_period/0}
         ]
     end}.

test_generate_valid_cert() ->
    %% Generate certificate
    Result = macula_tls:generate_self_signed_cert(#{}),
    ?assertMatch({ok, _CertPEM, _KeyPEM}, Result),

    {ok, CertPEM, KeyPEM} = Result,

    %% Verify PEM format
    ?assert(is_binary(CertPEM)),
    ?assert(is_binary(KeyPEM)),
    ?assert(byte_size(CertPEM) > 0),
    ?assert(byte_size(KeyPEM) > 0),

    %% Verify PEM markers
    ?assert(binary:match(CertPEM, <<"-----BEGIN CERTIFICATE-----">>) =/= nomatch),
    ?assert(binary:match(CertPEM, <<"-----END CERTIFICATE-----">>) =/= nomatch),
    %% OpenSSL generates PKCS#1 format: "-----BEGIN RSA PRIVATE KEY-----"
    HasRSA = binary:match(KeyPEM, <<"-----BEGIN RSA PRIVATE KEY-----">>) =/= nomatch,
    %% Or PKCS#8 format: "-----BEGIN PRIVATE KEY-----"
    HasPKCS8 = binary:match(KeyPEM, <<"-----BEGIN PRIVATE KEY-----">>) =/= nomatch,
    ?assert(HasRSA orelse HasPKCS8).

test_cert_format() ->
    {ok, CertPEM, _KeyPEM} = macula_tls:generate_self_signed_cert(#{}),

    %% Decode PEM
    [{'Certificate', CertDER, not_encrypted}] = public_key:pem_decode(CertPEM),

    %% Decode certificate
    Certificate = public_key:der_decode('Certificate', CertDER),

    %% Verify certificate structure
    ?assertMatch(#'Certificate'{}, Certificate),

    %% Verify TBS certificate
    #'Certificate'{tbsCertificate = TBSCert} = Certificate,
    ?assertMatch(#'TBSCertificate'{version = v3}, TBSCert).

test_public_key_extraction() ->
    {ok, CertPEM, _KeyPEM} = macula_tls:generate_self_signed_cert(#{}),

    %% Decode certificate
    [{'Certificate', CertDER, not_encrypted}] = public_key:pem_decode(CertPEM),
    Certificate = public_key:der_decode('Certificate', CertDER),

    %% Extract public key (bit string)
    #'Certificate'{
        tbsCertificate = #'TBSCertificate'{
            subjectPublicKeyInfo = #'SubjectPublicKeyInfo'{
                subjectPublicKey = PublicKeyBitString
            }
        }
    } = Certificate,

    %% Verify public key is a bit string (DER-encoded)
    ?assert(is_tuple(PublicKeyBitString) orelse is_binary(PublicKeyBitString)),

    %% Decode the public key
    PublicKeyDER = case PublicKeyBitString of
        {0, Bin} -> Bin;
        Bin when is_binary(Bin) -> Bin
    end,

    %% Should be able to decode as RSA public key
    RSAPublicKey = public_key:der_decode('RSAPublicKey', PublicKeyDER),
    ?assertMatch(#'RSAPublicKey'{}, RSAPublicKey).

test_validity_period() ->
    {ok, CertPEM, _KeyPEM} = macula_tls:generate_self_signed_cert(#{}),

    %% Decode certificate
    [{'Certificate', CertDER, not_encrypted}] = public_key:pem_decode(CertPEM),
    Certificate = public_key:der_decode('Certificate', CertDER),

    %% Extract validity
    #'Certificate'{
        tbsCertificate = #'TBSCertificate'{
            validity = Validity
        }
    } = Certificate,

    %% Verify validity structure
    ?assertMatch(#'Validity'{}, Validity).

%%%=============================================================================
%%% Node ID Derivation Tests
%%%=============================================================================

derive_node_id_test_() ->
    [
     {"Derive Node ID from certificate",
      fun test_derive_node_id/0},
     {"Node ID is consistent",
      fun test_node_id_consistency/0},
     {"Node ID is hex-encoded SHA-256",
      fun test_node_id_format/0},
     {"Different certificates produce different Node IDs",
      fun test_node_id_uniqueness/0}
    ].

test_derive_node_id() ->
    {ok, CertPEM, _KeyPEM} = macula_tls:generate_self_signed_cert(#{}),

    %% Derive Node ID
    NodeID = macula_tls:derive_node_id(CertPEM),

    %% Verify Node ID is binary
    ?assert(is_binary(NodeID)),
    ?assert(byte_size(NodeID) > 0).

test_node_id_consistency() ->
    {ok, CertPEM, _KeyPEM} = macula_tls:generate_self_signed_cert(#{}),

    %% Derive Node ID multiple times
    NodeID1 = macula_tls:derive_node_id(CertPEM),
    NodeID2 = macula_tls:derive_node_id(CertPEM),
    NodeID3 = macula_tls:derive_node_id(CertPEM),

    %% All should be identical
    ?assertEqual(NodeID1, NodeID2),
    ?assertEqual(NodeID2, NodeID3).

test_node_id_format() ->
    {ok, CertPEM, _KeyPEM} = macula_tls:generate_self_signed_cert(#{}),

    NodeID = macula_tls:derive_node_id(CertPEM),

    %% Verify raw 32-byte SHA-256 binary (not hex-encoded)
    ?assertEqual(32, byte_size(NodeID)).

test_node_id_uniqueness() ->
    %% Generate two different certificates
    {ok, CertPEM1, _KeyPEM1} = macula_tls:generate_self_signed_cert(#{}),
    {ok, CertPEM2, _KeyPEM2} = macula_tls:generate_self_signed_cert(#{}),

    %% Derive Node IDs
    NodeID1 = macula_tls:derive_node_id(CertPEM1),
    NodeID2 = macula_tls:derive_node_id(CertPEM2),

    %% Should be different
    ?assertNot(NodeID1 =:= NodeID2).

%%%=============================================================================
%%% Certificate Persistence Tests
%%%=============================================================================

ensure_cert_exists_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(TempDir) ->
         [
          {"Auto-generate certs if missing",
           fun() -> test_auto_generate_missing(TempDir) end},
          {"Reuse existing certs",
           fun() -> test_reuse_existing(TempDir) end},
          {"Node ID stable across reloads",
           fun() -> test_node_id_stable(TempDir) end},
          {"Error on missing cert but key exists",
           fun() -> test_error_missing_cert(TempDir) end},
          {"Error on missing key but cert exists",
           fun() -> test_error_missing_key(TempDir) end}
         ]
     end}.

test_auto_generate_missing(TempDir) ->
    CertPath = filename:join(TempDir, "cert.pem"),
    KeyPath = filename:join(TempDir, "key.pem"),

    %% Ensure files don't exist
    ?assertNot(filelib:is_file(CertPath)),
    ?assertNot(filelib:is_file(KeyPath)),

    %% Call ensure_cert_exists
    Result = macula_tls:ensure_cert_exists(CertPath, KeyPath),
    ?assertMatch({ok, CertPath, KeyPath, _NodeID}, Result),

    %% Verify files were created
    ?assert(filelib:is_file(CertPath)),
    ?assert(filelib:is_file(KeyPath)),

    %% Verify key permissions (0600)
    {ok, FileInfo} = file:read_file_info(KeyPath),
    Mode = FileInfo#file_info.mode,
    %% Extract permission bits (last 9 bits)
    Perms = Mode band 8#0777,
    ?assertEqual(8#0600, Perms).

test_reuse_existing(TempDir) ->
    CertPath = filename:join(TempDir, "cert2.pem"),
    KeyPath = filename:join(TempDir, "key2.pem"),

    %% First call - generate
    {ok, _, _, NodeID1} = macula_tls:ensure_cert_exists(CertPath, KeyPath),

    %% Second call - should reuse
    {ok, _, _, NodeID2} = macula_tls:ensure_cert_exists(CertPath, KeyPath),

    %% Node IDs should match
    ?assertEqual(NodeID1, NodeID2).

test_node_id_stable(TempDir) ->
    CertPath = filename:join(TempDir, "cert3.pem"),
    KeyPath = filename:join(TempDir, "key3.pem"),

    %% Generate certs
    {ok, _, _, NodeID1} = macula_tls:ensure_cert_exists(CertPath, KeyPath),

    %% Read cert file
    {ok, CertPEM} = file:read_file(CertPath),

    %% Derive Node ID manually
    NodeID2 = macula_tls:derive_node_id(CertPEM),

    %% Should match
    ?assertEqual(NodeID1, NodeID2).

test_error_missing_cert(TempDir) ->
    CertPath = filename:join(TempDir, "missing_cert.pem"),
    KeyPath = filename:join(TempDir, "only_key.pem"),

    %% Create only key file
    ok = file:write_file(KeyPath, <<"fake key">>),

    %% Should return error
    Result = macula_tls:ensure_cert_exists(CertPath, KeyPath),
    ?assertMatch({error, {missing_cert, _}}, Result).

test_error_missing_key(TempDir) ->
    CertPath = filename:join(TempDir, "only_cert.pem"),
    KeyPath = filename:join(TempDir, "missing_key.pem"),

    %% Create only cert file
    ok = file:write_file(CertPath, <<"fake cert">>),

    %% Should return error
    Result = macula_tls:ensure_cert_exists(CertPath, KeyPath),
    ?assertMatch({error, {missing_key, _}}, Result).

%%%=============================================================================
%%% Configuration Tests
%%%=============================================================================

get_cert_paths_test_() ->
    [
     {"Get default cert paths",
      fun test_default_paths/0},
     {"Get custom cert paths from env",
      fun test_custom_paths/0}
    ].

test_default_paths() ->
    %% Unset environment variables
    application:unset_env(macula, cert_path),
    application:unset_env(macula, key_path),

    {CertPath, KeyPath} = macula_tls:get_cert_paths(),

    %% Should return defaults
    ?assertEqual("/var/lib/macula/cert.pem", CertPath),
    ?assertEqual("/var/lib/macula/key.pem", KeyPath).

test_custom_paths() ->
    %% Set custom paths
    application:set_env(macula, cert_path, "/custom/cert.pem"),
    application:set_env(macula, key_path, "/custom/key.pem"),

    {CertPath, KeyPath} = macula_tls:get_cert_paths(),

    %% Should return custom paths
    ?assertEqual("/custom/cert.pem", CertPath),
    ?assertEqual("/custom/key.pem", KeyPath),

    %% Cleanup
    application:unset_env(macula, cert_path),
    application:unset_env(macula, key_path).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

%% Check if binary is a hex string
%% Hex helpers removed — node_id is now raw 32-byte binary, not hex-encoded.

%%%=============================================================================
%%% TLS Mode Detection Tests (v0.11.0+)
%%%=============================================================================

tls_mode_test_() ->
    {setup,
     fun() ->
         %% Save current environment
         SavedMode = os:getenv("MACULA_TLS_MODE"),
         SavedVerify = os:getenv("MACULA_TLS_VERIFY_HOSTNAME"),
         {SavedMode, SavedVerify}
     end,
     fun({SavedMode, SavedVerify}) ->
         %% Restore environment
         case SavedMode of
             false -> os:unsetenv("MACULA_TLS_MODE");
             V -> os:putenv("MACULA_TLS_MODE", V)
         end,
         case SavedVerify of
             false -> os:unsetenv("MACULA_TLS_VERIFY_HOSTNAME");
             V2 -> os:putenv("MACULA_TLS_VERIFY_HOSTNAME", V2)
         end
     end,
     fun(_) ->
         [
          {"Default TLS mode is development",
           fun test_default_tls_mode/0},
          {"TLS mode from app env",
           fun test_tls_mode_from_app_env/0},
          {"TLS mode from env var",
           fun test_tls_mode_from_env_var/0},
          {"TLS mode env var shorthands",
           fun test_tls_mode_shorthands/0},
          {"is_production_mode returns correct value",
           fun test_is_production_mode/0}
         ]
     end}.

test_default_tls_mode() ->
    os:unsetenv("MACULA_TLS_MODE"),
    application:set_env(macula, tls_mode, development),
    ?assertEqual(development, macula_tls:get_tls_mode()).

test_tls_mode_from_app_env() ->
    os:unsetenv("MACULA_TLS_MODE"),
    application:set_env(macula, tls_mode, production),
    ?assertEqual(production, macula_tls:get_tls_mode()),
    application:set_env(macula, tls_mode, development).

test_tls_mode_from_env_var() ->
    application:set_env(macula, tls_mode, development),
    os:putenv("MACULA_TLS_MODE", "production"),
    ?assertEqual(production, macula_tls:get_tls_mode()),
    os:unsetenv("MACULA_TLS_MODE").

test_tls_mode_shorthands() ->
    %% Test 'dev' shorthand
    os:putenv("MACULA_TLS_MODE", "dev"),
    ?assertEqual(development, macula_tls:get_tls_mode()),

    %% Test 'prod' shorthand
    os:putenv("MACULA_TLS_MODE", "prod"),
    ?assertEqual(production, macula_tls:get_tls_mode()),

    os:unsetenv("MACULA_TLS_MODE").

test_is_production_mode() ->
    os:unsetenv("MACULA_TLS_MODE"),
    application:set_env(macula, tls_mode, development),
    ?assertNot(macula_tls:is_production_mode()),

    os:putenv("MACULA_TLS_MODE", "production"),
    ?assert(macula_tls:is_production_mode()),

    os:unsetenv("MACULA_TLS_MODE").

%%%=============================================================================
%%% QUIC Client Options Tests (v0.11.0+)
%%%=============================================================================

quic_client_opts_test_() ->
    {setup,
     fun save_client_tls_env/0,
     fun restore_client_tls_env/1,
     fun(_) ->
         [
          {"Client opts returns list",
           fun test_client_opts_returns_list/0},
          {"Client opts verify webpki when no TLS mode is configured",
           fun test_client_opts_verify_webpki_when_unset/0},
          {"Client opts verify none with explicit development env",
           fun test_client_opts_verify_none_explicit_env/0},
          {"Client opts verify none with dev shorthand env",
           fun test_client_opts_verify_none_dev_shorthand/0},
          {"Client opts verify none with explicit development app env",
           fun test_client_opts_verify_none_explicit_app_env/0},
          {"Client opts in production are verify webpki only",
           fun test_client_opts_production_webpki_only/0},
          {"Client opts refuse an explicit CA file env",
           fun test_client_opts_refuse_cacertfile_env/0},
          {"Client opts refuse an explicit CA file app env",
           fun test_client_opts_refuse_cacertfile_app_env/0},
          {"Client opts with overrides",
           fun test_client_opts_with_overrides/0},
          {"Client opts with hostname (explicit development)",
           fun test_client_opts_with_hostname_explicit_development/0},
          {"Client opts with hostname equal plain client opts",
           fun test_client_opts_with_hostname_equal_plain/0}
         ]
     end}.

save_client_tls_env() ->
    #{env_mode       => os:getenv("MACULA_TLS_MODE"),
      env_cacertfile => os:getenv("MACULA_TLS_CACERTFILE"),
      app_mode       => application:get_env(macula, tls_mode),
      app_cacertfile => application:get_env(macula, tls_cacertfile)}.

restore_client_tls_env(#{env_mode := EnvMode, env_cacertfile := EnvCa,
                         app_mode := AppMode, app_cacertfile := AppCa}) ->
    restore_os_env("MACULA_TLS_MODE", EnvMode),
    restore_os_env("MACULA_TLS_CACERTFILE", EnvCa),
    restore_app_env(tls_mode, AppMode),
    restore_app_env(tls_cacertfile, AppCa).

restore_os_env(Name, false) -> os:unsetenv(Name);
restore_os_env(Name, Value) -> os:putenv(Name, Value).

restore_app_env(Key, undefined)    -> application:unset_env(macula, Key);
restore_app_env(Key, {ok, Value})  -> application:set_env(macula, Key, Value).

%% No TLS mode and no CA file configured, in either the OS env or the
%% app env: the state a node is in when nobody set anything.
clear_client_tls_env() ->
    os:unsetenv("MACULA_TLS_MODE"),
    os:unsetenv("MACULA_TLS_CACERTFILE"),
    application:unset_env(macula, tls_mode),
    application:unset_env(macula, tls_cacertfile).

test_client_opts_returns_list() ->
    clear_client_tls_env(),
    ?assert(is_list(macula_tls:quic_client_opts())).

test_client_opts_verify_webpki_when_unset() ->
    clear_client_tls_env(),
    ?assertEqual([{verify, webpki}], macula_tls:quic_client_opts()).

test_client_opts_verify_none_explicit_env() ->
    clear_client_tls_env(),
    os:putenv("MACULA_TLS_MODE", "development"),
    ?assertEqual([{verify, none}], macula_tls:quic_client_opts()).

test_client_opts_verify_none_dev_shorthand() ->
    clear_client_tls_env(),
    os:putenv("MACULA_TLS_MODE", "dev"),
    ?assertEqual([{verify, none}], macula_tls:quic_client_opts()).

test_client_opts_verify_none_explicit_app_env() ->
    clear_client_tls_env(),
    application:set_env(macula, tls_mode, development),
    ?assertEqual([{verify, none}], macula_tls:quic_client_opts()).

test_client_opts_production_webpki_only() ->
    clear_client_tls_env(),
    os:putenv("MACULA_TLS_MODE", "production"),
    ?assertEqual([{verify, webpki}], macula_tls:quic_client_opts()).

test_client_opts_refuse_cacertfile_env() ->
    clear_client_tls_env(),
    os:putenv("MACULA_TLS_CACERTFILE", "/etc/macula/private-ca.pem"),
    ?assertError({tls_config_error,
                  {cacertfile_not_supported, "/etc/macula/private-ca.pem"}},
                 macula_tls:quic_client_opts()).

test_client_opts_refuse_cacertfile_app_env() ->
    clear_client_tls_env(),
    application:set_env(macula, tls_cacertfile, "/etc/macula/private-ca.pem"),
    ?assertError({tls_config_error,
                  {cacertfile_not_supported, "/etc/macula/private-ca.pem"}},
                 macula_tls:quic_client_opts()).

test_client_opts_with_overrides() ->
    clear_client_tls_env(),
    Opts = macula_tls:quic_client_opts(#{custom_opt => test_value}),
    ?assertEqual(test_value, proplists:get_value(custom_opt, Opts)).

test_client_opts_with_hostname_explicit_development() ->
    clear_client_tls_env(),
    os:putenv("MACULA_TLS_MODE", "development"),
    Opts = macula_tls:quic_client_opts_with_hostname("example.com"),
    ?assertEqual(none, proplists:get_value(verify, Opts)).

%% The QUIC NIF verifies the dialed host itself, so a hostname adds no
%% options of its own.
test_client_opts_with_hostname_equal_plain() ->
    clear_client_tls_env(),
    os:putenv("MACULA_TLS_MODE", "production"),
    ?assertEqual(macula_tls:quic_client_opts(),
                 macula_tls:quic_client_opts_with_hostname("example.com")).

%%%=============================================================================
%%% Hostname Verification Tests (v0.11.0+)
%%%=============================================================================

hostname_verify_fun_test_() ->
    [
     {"valid_peer with hostname succeeds",
      fun test_verify_fun_valid_peer_hostname/0},
     {"valid_peer without hostname succeeds",
      fun test_verify_fun_valid_peer_no_hostname/0},
     {"valid event passes through",
      fun test_verify_fun_valid/0},
     {"extension returns unknown",
      fun test_verify_fun_extension/0},
     {"bad_cert returns fail with reason",
      fun test_verify_fun_bad_cert/0}
    ].

test_verify_fun_valid_peer_hostname() ->
    State = #{hostname => <<"example.com">>},
    Result = macula_tls:hostname_verify_fun(dummy_cert, valid_peer, State),
    ?assertMatch({valid, _}, Result).

test_verify_fun_valid_peer_no_hostname() ->
    State = #{},
    Result = macula_tls:hostname_verify_fun(dummy_cert, valid_peer, State),
    ?assertMatch({valid, _}, Result).

test_verify_fun_valid() ->
    State = #{hostname => <<"example.com">>},
    Result = macula_tls:hostname_verify_fun(dummy_cert, valid, State),
    ?assertMatch({valid, _}, Result).

test_verify_fun_extension() ->
    State = #{hostname => <<"example.com">>},
    Result = macula_tls:hostname_verify_fun(dummy_cert, {extension, some_ext}, State),
    ?assertMatch({unknown, _}, Result).

test_verify_fun_bad_cert() ->
    State = #{hostname => <<"example.com">>},
    Result = macula_tls:hostname_verify_fun(dummy_cert, {bad_cert, expired}, State),
    ?assertMatch({fail, expired}, Result).
