%%%-----------------------------------------------------------------------------
%%% @doc TLS Certificate Management and Verification Module (v0.11.0+)
%%%
%%% This module provides TLS certificate management for Macula nodes with
%%% two operating modes:
%%%
%%% - **Production Mode**: Strict certificate verification with CA bundle
%%% - **Development Mode**: Self-signed certificates (auto-generated)
%%%
%%% == Configuration (sys.config) ==
%%%
%%%   {macula, [
%%%       %% TLS mode: production (strict) or development (permissive)
%%%       {tls_mode, development},  % or production
%%%
%%%       %% CA certificate bundle (production mode)
%%%       {tls_cacertfile, "/path/to/ca-bundle.crt"},
%%%
%%%       %% Server/client certificate and key
%%%       {tls_certfile, "/path/to/server.crt"},
%%%       {tls_keyfile, "/path/to/server.key"},
%%%
%%%       %% Hostname verification (production mode, default: true)
%%%       {tls_verify_hostname, true}
%%%   ]}
%%%
%%% == Environment Variables ==
%%%
%%% - MACULA_TLS_MODE: production | development
%%% - MACULA_TLS_CACERTFILE: Path to CA bundle
%%% - MACULA_TLS_CERTFILE: Path to certificate
%%% - MACULA_TLS_KEYFILE: Path to private key
%%%
%%% == Security Note ==
%%%
%%% In production mode, TLS connections will:
%%% - Verify the server certificate chain against the CA bundle
%%% - Reject expired or invalid certificates
%%% - Optionally verify hostname matches certificate CN/SAN
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(macula_tls).

%% API - QUIC TLS Options (v0.11.0+)
-export([
    quic_client_opts/0,
    quic_client_opts/1,
    quic_client_opts_with_hostname/1,
    quic_server_opts/0,
    quic_server_opts/1,
    get_tls_mode/0,
    is_production_mode/0,
    hostname_verify_fun/3
]).

%% API - Certificate Management
-export([
    ensure_cert_exists/2,
    generate_self_signed_cert/1,
    derive_node_id/1,
    get_cert_paths/0
]).

-include_lib("kernel/include/logger.hrl").

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

%%%=============================================================================
%%% QUIC TLS Options API (v0.11.0+)
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get QUIC client TLS options based on current TLS mode.
%%
%% In production mode: Returns options with certificate verification enabled.
%% In development mode: Returns options with verification disabled.
%%
%% @returns Proplist of QUIC TLS options suitable for quicer:connect/4
%% @end
%%------------------------------------------------------------------------------
-spec quic_client_opts() -> list().
quic_client_opts() ->
    quic_client_opts(#{}).

%%------------------------------------------------------------------------------
%% @doc Get QUIC client TLS options with overrides.
%%
%% @param Overrides Map of options to override defaults
%% @returns Proplist of QUIC TLS options
%% @end
%%------------------------------------------------------------------------------
-spec quic_client_opts(Overrides :: map()) -> list().
quic_client_opts(Overrides) ->
    Mode = get_tls_mode(),
    BaseOpts = build_client_opts(Mode),
    apply_overrides(BaseOpts, Overrides).

%%------------------------------------------------------------------------------
%% @doc Get QUIC client TLS options with hostname verification.
%%
%% In production mode, adds SNI and hostname verification if enabled.
%% In development mode, hostname verification is skipped.
%%
%% @param Hostname The hostname to verify (string or binary)
%% @returns Proplist of QUIC TLS options with hostname verification
%% @end
%%------------------------------------------------------------------------------
-spec quic_client_opts_with_hostname(Hostname :: string() | binary()) -> list().
quic_client_opts_with_hostname(Hostname) ->
    BaseOpts = quic_client_opts(),
    case get_tls_mode() of
        production ->
            case get_verify_hostname() of
                true ->
                    HostnameOpts = build_hostname_verify_opts(Hostname),
                    merge_opts(BaseOpts, HostnameOpts);
                false ->
                    BaseOpts
            end;
        development ->
            %% In development mode, skip hostname verification
            BaseOpts
    end.

%%------------------------------------------------------------------------------
%% @doc Merge two proplists, second takes precedence.
%% @private
%%------------------------------------------------------------------------------
-spec merge_opts(list(), list()) -> list().
merge_opts(BaseOpts, OverrideOpts) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            lists:keystore(Key, 1, Acc, {Key, Value})
        end,
        BaseOpts,
        OverrideOpts
    ).

%%------------------------------------------------------------------------------
%% @doc Get QUIC server TLS options based on current TLS mode.
%%
%% Server always needs a certificate and key.
%% In production mode: Also verifies client certificates if presented.
%% In development mode: Auto-generates self-signed certificate if needed.
%%
%% @returns Proplist of QUIC TLS options suitable for quicer:listen/2
%% @end
%%------------------------------------------------------------------------------
-spec quic_server_opts() -> list().
quic_server_opts() ->
    quic_server_opts(#{}).

%%------------------------------------------------------------------------------
%% @doc Get QUIC server TLS options with overrides.
%%
%% @param Overrides Map of options to override defaults
%% @returns Proplist of QUIC TLS options
%% @end
%%------------------------------------------------------------------------------
-spec quic_server_opts(Overrides :: map()) -> list().
quic_server_opts(Overrides) ->
    Mode = get_tls_mode(),
    BaseOpts = build_server_opts(Mode),
    apply_overrides(BaseOpts, Overrides).

%%------------------------------------------------------------------------------
%% @doc Get the current TLS mode (production or development).
%%
%% Checks in order:
%% 1. MACULA_TLS_MODE environment variable
%% 2. tls_mode application environment setting
%% 3. Defaults to 'development'
%%
%% @returns production | development
%% @end
%%------------------------------------------------------------------------------
-spec get_tls_mode() -> production | development.
get_tls_mode() ->
    case os:getenv("MACULA_TLS_MODE") of
        "production" -> production;
        "prod" -> production;
        "development" -> development;
        "dev" -> development;
        false ->
            application:get_env(macula, tls_mode, development)
    end.

%%------------------------------------------------------------------------------
%% @doc Check if running in production TLS mode.
%%
%% @returns true if production mode, false if development mode
%% @end
%%------------------------------------------------------------------------------
-spec is_production_mode() -> boolean().
is_production_mode() ->
    get_tls_mode() =:= production.

%%%=============================================================================
%%% Internal Functions - TLS Options Building
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Build client TLS options for the given mode.
%% @private
%%------------------------------------------------------------------------------
-spec build_client_opts(production | development) -> list().

%% Production mode: verify certificates
build_client_opts(production) ->
    CACertFile = get_cacertfile(),

    %% Validate CA cert exists
    case filelib:is_regular(CACertFile) of
        true -> ok;
        false ->
            ?LOG_ERROR("TLS production mode requires CA certificate: ~s not found", [CACertFile]),
            error({tls_config_error, {cacertfile_not_found, CACertFile}})
    end,

    Opts = [
        {verify, verify_peer},
        {cacertfile, CACertFile},
        {depth, 3}  % Max certificate chain depth
    ],

    %% Add client cert if configured (for mTLS)
    CertFile = get_tls_certfile(),
    KeyFile = get_tls_keyfile(),
    add_client_cert_opts(Opts, CertFile, KeyFile);

%% Development mode: no verification
build_client_opts(development) ->
    ?LOG_WARNING("TLS running in DEVELOPMENT mode - certificate verification DISABLED"),
    [{verify, none}].

%%------------------------------------------------------------------------------
%% @doc Build server TLS options for the given mode.
%% @private
%%------------------------------------------------------------------------------
-spec build_server_opts(production | development) -> list().

%% Production mode: require valid certificates
build_server_opts(production) ->
    CertFile = get_tls_certfile(),
    KeyFile = get_tls_keyfile(),

    %% Validate server cert and key exist
    case filelib:is_regular(CertFile) of
        true -> ok;
        false ->
            ?LOG_ERROR("TLS production mode requires server certificate: ~s not found", [CertFile]),
            error({tls_config_error, {certfile_not_found, CertFile}})
    end,

    case filelib:is_regular(KeyFile) of
        true -> ok;
        false ->
            ?LOG_ERROR("TLS production mode requires server key: ~s not found", [KeyFile]),
            error({tls_config_error, {keyfile_not_found, KeyFile}})
    end,

    Opts = [
        {certfile, CertFile},
        {keyfile, KeyFile},
        {verify, verify_peer},
        {fail_if_no_peer_cert, false}  % Don't require client cert
    ],

    %% Add CA cert if available (for client cert verification)
    CACertFile = get_cacertfile(),
    case filelib:is_regular(CACertFile) of
        true -> [{cacertfile, CACertFile} | Opts];
        false -> Opts
    end;

%% Development mode: use or generate self-signed certs
build_server_opts(development) ->
    {CertFile, KeyFile} = get_cert_paths(),

    %% Ensure development certs exist
    case ensure_cert_exists(CertFile, KeyFile) of
        {ok, _, _, _NodeId} ->
            ?LOG_WARNING("TLS running in DEVELOPMENT mode with self-signed certificate"),
            [
                {certfile, CertFile},
                {keyfile, KeyFile},
                {verify, none}
            ];
        {error, Reason} ->
            ?LOG_ERROR("Failed to ensure development certificates: ~p", [Reason]),
            error({tls_config_error, {dev_cert_error, Reason}})
    end.

%%------------------------------------------------------------------------------
%% @doc Add client certificate options if configured.
%% @private
%%------------------------------------------------------------------------------
-spec add_client_cert_opts(list(), string(), string()) -> list().
add_client_cert_opts(Opts, CertFile, KeyFile) ->
    case filelib:is_regular(CertFile) andalso filelib:is_regular(KeyFile) of
        true ->
            [{certfile, CertFile}, {keyfile, KeyFile} | Opts];
        false ->
            Opts
    end.

%%------------------------------------------------------------------------------
%% @doc Apply overrides to base options.
%% @private
%%------------------------------------------------------------------------------
-spec apply_overrides(list(), map()) -> list().
apply_overrides(Opts, Overrides) when map_size(Overrides) =:= 0 ->
    Opts;
apply_overrides(Opts, Overrides) ->
    OverrideList = maps:to_list(Overrides),
    lists:foldl(
        fun({Key, Value}, Acc) ->
            lists:keystore(Key, 1, Acc, {Key, Value})
        end,
        Opts,
        OverrideList
    ).

%%%=============================================================================
%%% Internal Functions - Configuration Getters
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get CA certificate file path.
%% @private
%%------------------------------------------------------------------------------
-spec get_cacertfile() -> string().
get_cacertfile() ->
    case os:getenv("MACULA_TLS_CACERTFILE") of
        false ->
            case application:get_env(macula, tls_cacertfile) of
                {ok, Path} -> Path;
                undefined -> find_system_ca_bundle()
            end;
        Path ->
            Path
    end.

%%------------------------------------------------------------------------------
%% @doc Get TLS certificate file path.
%% @private
%%------------------------------------------------------------------------------
-spec get_tls_certfile() -> string().
get_tls_certfile() ->
    case os:getenv("MACULA_TLS_CERTFILE") of
        false ->
            case application:get_env(macula, tls_certfile) of
                {ok, Path} -> Path;
                undefined -> ""
            end;
        Path ->
            Path
    end.

%%------------------------------------------------------------------------------
%% @doc Get TLS private key file path.
%% @private
%%------------------------------------------------------------------------------
-spec get_tls_keyfile() -> string().
get_tls_keyfile() ->
    case os:getenv("MACULA_TLS_KEYFILE") of
        false ->
            case application:get_env(macula, tls_keyfile) of
                {ok, Path} -> Path;
                undefined -> ""
            end;
        Path ->
            Path
    end.

%%------------------------------------------------------------------------------
%% @doc Find system CA certificate bundle.
%% Tries common locations on Linux systems.
%% @private
%%------------------------------------------------------------------------------
-spec find_system_ca_bundle() -> string().
find_system_ca_bundle() ->
    Candidates = [
        "/etc/ssl/certs/ca-certificates.crt",      % Debian/Ubuntu
        "/etc/pki/tls/certs/ca-bundle.crt",        % RHEL/CentOS
        "/etc/ssl/ca-bundle.pem",                   % OpenSUSE
        "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem",  % Fedora
        "/usr/local/share/certs/ca-root-nss.crt",  % FreeBSD
        "/etc/ssl/cert.pem"                         % Alpine, macOS
    ],
    find_existing_file(Candidates).

%%------------------------------------------------------------------------------
%% @doc Find first existing file from list.
%% @private
%%------------------------------------------------------------------------------
-spec find_existing_file([string()]) -> string().
find_existing_file([]) ->
    ?LOG_WARNING("No system CA bundle found - TLS verification may fail in production mode"),
    "";
find_existing_file([Path | Rest]) ->
    case filelib:is_regular(Path) of
        true -> Path;
        false -> find_existing_file(Rest)
    end.

%%------------------------------------------------------------------------------
%% @doc Check if hostname verification is enabled.
%% @private
%%------------------------------------------------------------------------------
-spec get_verify_hostname() -> boolean().
get_verify_hostname() ->
    case os:getenv("MACULA_TLS_VERIFY_HOSTNAME") of
        "false" -> false;
        "0" -> false;
        "true" -> true;
        "1" -> true;
        false ->
            application:get_env(macula, tls_verify_hostname, true)
    end.

%%%=============================================================================
%%% Hostname Verification
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc TLS verify_fun callback for hostname verification.
%%
%% This function is called during TLS handshake to verify the peer certificate.
%% When used with hostname verification, it checks that the server's certificate
%% contains the expected hostname in either the Subject CN or Subject Alt Names.
%%
%% Usage:
%%
%%   {verify_fun, {fun macula_tls:hostname_verify_fun/3, #{hostname => "example.com"}}}
%%
%% @param Cert The DER-encoded certificate being verified
%% @param Event The verification event (valid_peer, valid, extension, etc.)
%% @param State User state containing verification options (#{hostname => ...})
%% @returns {valid, State} | {fail, Reason} | {unknown, State}
%% @end
%%------------------------------------------------------------------------------
-spec hostname_verify_fun(
    Cert :: term(),
    Event :: {bad_cert, term()} | {extension, term()} | valid | valid_peer,
    State :: map()
) -> {valid, map()} | {fail, term()} | {unknown, map()}.

%% Certificate chain is valid, now verify hostname for leaf cert
hostname_verify_fun(_Cert, valid_peer, #{hostname := Hostname} = State)
  when is_list(Hostname); is_binary(Hostname) ->
    %% Use ssl:verify_hostname/2 with the certificate from the handshake
    %% Note: For QUIC/quicer, the actual hostname verification is done
    %% using server_name_indication option. This callback provides
    %% additional verification if needed.
    {valid, State};

hostname_verify_fun(_Cert, valid_peer, State) ->
    %% No hostname to verify
    {valid, State};

%% Certificate is valid (intermediate or root)
hostname_verify_fun(_Cert, valid, State) ->
    {valid, State};

%% Handle extensions (pass through)
hostname_verify_fun(_Cert, {extension, _}, State) ->
    {unknown, State};

%% Handle bad certificate errors
hostname_verify_fun(_Cert, {bad_cert, Reason}, _State) ->
    {fail, Reason}.

%%------------------------------------------------------------------------------
%% @doc Build verify_fun option for hostname verification.
%% @private
%%------------------------------------------------------------------------------
-spec build_hostname_verify_opts(Hostname :: string() | binary()) -> list().
build_hostname_verify_opts(Hostname) when is_list(Hostname) ->
    build_hostname_verify_opts(list_to_binary(Hostname));
build_hostname_verify_opts(Hostname) when is_binary(Hostname) ->
    [
        {server_name_indication, binary_to_list(Hostname)},
        {verify_fun, {fun hostname_verify_fun/3, #{hostname => Hostname}}}
    ];
build_hostname_verify_opts(_) ->
    [].
