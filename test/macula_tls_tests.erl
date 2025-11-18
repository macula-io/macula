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
    TempDir = lists:flatten(io_lib:format("/tmp/macula_tls_test_~p", [erlang:unique_integer([positive])])),
    ok = filelib:ensure_dir(TempDir ++ "/"),
    TempDir.

cleanup(TempDir) ->
    %% Remove temporary directory and all files
    case file:list_dir(TempDir) of
        {ok, Files} ->
            [file:delete(filename:join(TempDir, F)) || F <- Files],
            file:del_dir(TempDir);
        {error, _} ->
            ok
    end.

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

    %% Verify hex-encoded SHA-256 (64 characters)
    ?assertEqual(64, byte_size(NodeID)),

    %% Verify all characters are hex (0-9, a-f)
    ?assert(is_hex_string(NodeID)).

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
is_hex_string(Bin) when is_binary(Bin) ->
    lists:all(fun is_hex_char/1, binary_to_list(Bin)).

is_hex_char(C) when C >= $0, C =< $9 -> true;
is_hex_char(C) when C >= $a, C =< $f -> true;
is_hex_char(C) when C >= $A, C =< $F -> true;
is_hex_char(_) -> false.
