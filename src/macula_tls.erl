%%%-----------------------------------------------------------------------------
%%% @doc TLS Certificate Auto-Generation Module
%%%
%%% This module provides zero-config TLS certificate management for Macula nodes.
%%% Certificates are auto-generated on first boot and persisted to disk for
%%% stable node identity across restarts.
%%%
%%% Key Features:
%%% - Auto-generate self-signed certificates on first boot
%%% - Derive stable Node ID from public key (SHA-256 hash)
%%% - Persist certificates to disk (survives restarts)
%%% - Proper file permissions (0600 for private key)
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(macula_tls).

%% API
-export([
    ensure_cert_exists/2,
    generate_self_signed_cert/1,
    derive_node_id/1,
    get_cert_paths/0
]).

-include_lib("public_key/include/public_key.hrl").

-define(DEFAULT_CERT_PATH, "/var/lib/macula/cert.pem").
-define(DEFAULT_KEY_PATH, "/var/lib/macula/key.pem").
-define(DEFAULT_VALIDITY_DAYS, 3650).  % 10 years
-define(DEFAULT_KEY_BITS, 2048).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Ensure TLS certificate exists, generate if missing.
%%
%% Checks if certificate and key files exist at the specified paths.
%% If they don't exist, generates new self-signed certificate and saves to disk.
%% Returns the paths and derived Node ID.
%%
%% @param CertPath Path to certificate file (PEM format)
%% @param KeyPath Path to private key file (PEM format)
%% @returns {ok, CertPath, KeyPath, NodeID} | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec ensure_cert_exists(CertPath :: file:filename(), KeyPath :: file:filename()) ->
    {ok, file:filename(), file:filename(), binary()} | {error, term()}.

ensure_cert_exists(CertPath, KeyPath) ->
    case {filelib:is_file(CertPath), filelib:is_file(KeyPath)} of
        {true, true} ->
            %% Both files exist - load and derive Node ID
            case file:read_file(CertPath) of
                {ok, CertPEM} ->
                    NodeID = derive_node_id(CertPEM),
                    {ok, CertPath, KeyPath, NodeID};
                {error, Reason} ->
                    {error, {read_cert_failed, Reason}}
            end;
        {false, false} ->
            %% Neither exists - generate new certificate
            case generate_and_save_cert(CertPath, KeyPath) of
                {ok, NodeID} -> {ok, CertPath, KeyPath, NodeID};
                {error, Reason} -> {error, Reason}
            end;
        {true, false} ->
            {error, {missing_key, KeyPath}};
        {false, true} ->
            {error, {missing_cert, CertPath}}
    end.

%%------------------------------------------------------------------------------
%% @doc Generate self-signed TLS certificate using OpenSSL.
%%
%% Creates a new RSA key pair and self-signed X.509 certificate with:
%% - RSA 2048-bit key
%% - 10-year validity period
%% - Subject: CN=macula-node
%% - Self-signed (issuer = subject)
%%
%% @param Opts Options map (currently unused, reserved for future extensions)
%% @returns {ok, CertPEM, KeyPEM} | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec generate_self_signed_cert(Opts :: map()) ->
    {ok, CertPEM :: binary(), KeyPEM :: binary()} | {error, term()}.

generate_self_signed_cert(_Opts) ->
    try
        %% Get configuration
        KeyBits = application:get_env(macula, cert_key_bits, ?DEFAULT_KEY_BITS),
        ValidityDays = application:get_env(macula, cert_validity_days, ?DEFAULT_VALIDITY_DAYS),

        %% Create temporary files for OpenSSL
        TempKeyPath = "/tmp/macula_temp_key_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".pem",
        TempCertPath = "/tmp/macula_temp_cert_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".pem",

        try
            %% Generate private key
            KeyCmd = lists:flatten(io_lib:format(
                "openssl genrsa -out ~s ~p 2>&1",
                [TempKeyPath, KeyBits]
            )),
            case os:cmd(KeyCmd) of
                "" -> ok;  %% OpenSSL may return empty on success
                KeyOutput ->
                    %% Check if file was created (success)
                    case filelib:is_file(TempKeyPath) of
                        true -> ok;
                        false ->
                            logger:error("Failed to generate key: ~s", [KeyOutput]),
                            throw({error, {key_generation_failed, KeyOutput}})
                    end
            end,

            %% Generate self-signed certificate
            CertCmd = lists:flatten(io_lib:format(
                "openssl req -new -x509 -key ~s -out ~s -days ~p "
                "-subj '/CN=macula-node' 2>&1",
                [TempKeyPath, TempCertPath, ValidityDays]
            )),
            case os:cmd(CertCmd) of
                "" -> ok;  %% OpenSSL may return empty on success
                CertOutput ->
                    %% Check if file was created (success)
                    case filelib:is_file(TempCertPath) of
                        true -> ok;
                        false ->
                            logger:error("Failed to generate certificate: ~s", [CertOutput]),
                            throw({error, {cert_generation_failed, CertOutput}})
                    end
            end,

            %% Read generated files
            {ok, KeyPEM} = file:read_file(TempKeyPath),
            {ok, CertPEM} = file:read_file(TempCertPath),

            {ok, CertPEM, KeyPEM}
        after
            %% Cleanup temporary files
            file:delete(TempKeyPath),
            file:delete(TempCertPath)
        end
    catch
        throw:{error, Reason} ->
            {error, Reason};
        Type:Error:Stacktrace ->
            logger:error("Failed to generate certificate: ~p:~p~n~p",
                        [Type, Error, Stacktrace]),
            {error, {cert_generation_failed, Error}}
    end.

%%------------------------------------------------------------------------------
%% @doc Derive Node ID from certificate public key.
%%
%% Extracts the public key from the PEM-encoded certificate and computes
%% SHA-256 hash to create a stable, cryptographically-derived Node ID.
%%
%% @param CertPEM PEM-encoded certificate binary
%% @returns NodeID Binary (32-byte SHA-256 hash, hex-encoded)
%% @end
%%------------------------------------------------------------------------------
-spec derive_node_id(CertPEM :: binary()) -> NodeID :: binary().

derive_node_id(CertPEM) when is_binary(CertPEM) ->
    %% Decode PEM - extract certificate (may contain other entries like private key)
    PemEntries = public_key:pem_decode(CertPEM),
    {'Certificate', CertDER, not_encrypted} = lists:keyfind('Certificate', 1, PemEntries),

    %% Decode certificate
    Certificate = public_key:der_decode('Certificate', CertDER),

    %% Extract public key (already in DER format as bit string)
    #'Certificate'{
        tbsCertificate = #'TBSCertificate'{
            subjectPublicKeyInfo = #'SubjectPublicKeyInfo'{
                subjectPublicKey = PublicKeyBitString
            }
        }
    } = Certificate,

    %% Convert bit string to binary
    %% The public key is stored as {Unused, Binary} where Unused is the number of unused bits
    PublicKeyDER = case PublicKeyBitString of
        {0, Bin} -> Bin;  %% Modern format: {UnusedBits, Binary}
        Bin when is_binary(Bin) -> Bin  %% Older format: just binary
    end,

    %% Compute SHA-256 hash
    Hash = crypto:hash(sha256, PublicKeyDER),

    %% Return hex-encoded hash
    binary:encode_hex(Hash, lowercase).

%%------------------------------------------------------------------------------
%% @doc Get default certificate paths from application environment.
%%
%% @returns {CertPath, KeyPath}
%% @end
%%------------------------------------------------------------------------------
-spec get_cert_paths() -> {file:filename(), file:filename()}.

get_cert_paths() ->
    CertPath = application:get_env(macula, cert_path, ?DEFAULT_CERT_PATH),
    KeyPath = application:get_env(macula, key_path, ?DEFAULT_KEY_PATH),
    {CertPath, KeyPath}.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Generate certificate and save to disk with proper permissions.
%% @private
%%------------------------------------------------------------------------------
-spec generate_and_save_cert(CertPath :: file:filename(), KeyPath :: file:filename()) ->
    {ok, NodeID :: binary()} | {error, term()}.

generate_and_save_cert(CertPath, KeyPath) ->
    logger:info("Auto-generating TLS certificate: ~s, ~s", [CertPath, KeyPath]),

    %% Ensure parent directories exist
    case ensure_parent_dir(CertPath) of
        ok -> ok;
        {error, Reason1} ->
            logger:error("Failed to create cert directory: ~p", [Reason1]),
            throw({error, {mkdir_failed, Reason1}})
    end,

    case ensure_parent_dir(KeyPath) of
        ok -> ok;
        {error, Reason2} ->
            logger:error("Failed to create key directory: ~p", [Reason2]),
            throw({error, {mkdir_failed, Reason2}})
    end,

    %% Generate certificate
    case generate_self_signed_cert(#{}) of
        {ok, CertPEM, KeyPEM} ->
            %% Save certificate
            case file:write_file(CertPath, CertPEM) of
                ok ->
                    %% Save private key with restricted permissions
                    case file:write_file(KeyPath, KeyPEM) of
                        ok ->
                            %% Set permissions: 0600 (owner read/write only)
                            case file:change_mode(KeyPath, 8#0600) of
                                ok ->
                                    NodeID = derive_node_id(CertPEM),
                                    logger:info("TLS certificate generated successfully. Node ID: ~s",
                                               [NodeID]),
                                    {ok, NodeID};
                                {error, Reason} ->
                                    logger:error("Failed to set key permissions: ~p", [Reason]),
                                    {error, {chmod_failed, Reason}}
                            end;
                        {error, Reason} ->
                            logger:error("Failed to write key file: ~p", [Reason]),
                            {error, {write_key_failed, Reason}}
                    end;
                {error, Reason} ->
                    logger:error("Failed to write cert file: ~p", [Reason]),
                    {error, {write_cert_failed, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc Ensure parent directory exists, create if needed.
%% @private
%%------------------------------------------------------------------------------
-spec ensure_parent_dir(FilePath :: file:filename()) -> ok | {error, term()}.

ensure_parent_dir(FilePath) ->
    ParentDir = filename:dirname(FilePath),
    %% filelib:ensure_dir/1 requires trailing separator for directories
    %% Use filename:join/2 to handle both binary and list paths correctly
    DirWithSeparator = filename:join(ParentDir, "dummy"),
    case filelib:ensure_dir(DirWithSeparator) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.
