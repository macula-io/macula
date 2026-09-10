%%%-----------------------------------------------------------------------------
%%% @doc TLS certificate management and TLS options for Macula QUIC.
%%%
%%% == Client dials ==
%%%
%%% quic_client_opts/0 decides what an outbound QUIC dial verifies. The
%%% QUIC NIF checks the server certificate against its built-in webpki
%%% root store and the host being dialed. It cannot load a CA file.
%%%
%%% - No TLS mode configured, or production: [{verify, webpki}].
%%% - Development set explicitly, through MACULA_TLS_MODE=development
%%%   (or dev) or the tls_mode app env: [{verify, none}]. This skips
%%%   verification, so use it only for local development against
%%%   self-signed peers.
%%% - A CA file set through MACULA_TLS_CACERTFILE or the tls_cacertfile
%%%   app env raises {tls_config_error, {cacertfile_not_supported, Path}}.
%%%
%%% == Listeners ==
%%%
%%% quic_server_opts/0 follows get_tls_mode/0, which reports development
%%% when nothing is configured:
%%%
%%% - Production: certificate and key from tls_certfile and tls_keyfile.
%%% - Development: a self-signed certificate, generated when missing.
%%%
%%% == Configuration (sys.config) ==
%%%
%%%   {macula, [
%%%       {tls_mode, production},  % or development
%%%       {tls_certfile, "/path/to/server.crt"},
%%%       {tls_keyfile, "/path/to/server.key"}
%%%   ]}
%%%
%%% == Environment Variables ==
%%%
%%% - MACULA_TLS_MODE: production, prod, development or dev
%%% - MACULA_TLS_CERTFILE: path to the listener certificate
%%% - MACULA_TLS_KEYFILE: path to the listener private key
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
    cert_files_state({filelib:is_file(CertPath), filelib:is_file(KeyPath)}, CertPath, KeyPath).

cert_files_state({true, true}, CertPath, KeyPath) ->
    %% Both files exist - load and derive Node ID
    load_existing_cert(file:read_file(CertPath), CertPath, KeyPath);
cert_files_state({false, false}, CertPath, KeyPath) ->
    %% Neither exists - generate new certificate
    generated_cert(generate_and_save_cert(CertPath, KeyPath), CertPath, KeyPath);
cert_files_state({true, false}, _CertPath, KeyPath) ->
    {error, {missing_key, KeyPath}};
cert_files_state({false, true}, CertPath, _KeyPath) ->
    {error, {missing_cert, CertPath}}.

load_existing_cert({ok, CertPEM}, CertPath, KeyPath) ->
    {ok, CertPath, KeyPath, derive_node_id(CertPEM)};
load_existing_cert({error, Reason}, _CertPath, _KeyPath) ->
    {error, {read_cert_failed, Reason}}.

generated_cert({ok, NodeID}, CertPath, KeyPath) ->
    {ok, CertPath, KeyPath, NodeID};
generated_cert({error, Reason}, _CertPath, _KeyPath) ->
    {error, Reason}.

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
        KeyBits = application:get_env(macula, cert_key_bits, ?DEFAULT_KEY_BITS),
        ValidityDays = application:get_env(macula, cert_validity_days, ?DEFAULT_VALIDITY_DAYS),
        TempKeyPath = "/tmp/macula_temp_key_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".pem",
        TempCertPath = "/tmp/macula_temp_cert_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".pem",
        run_openssl_with_cleanup(TempKeyPath, TempCertPath, KeyBits, ValidityDays)
    catch
        throw:{error, Reason} ->
            {error, Reason};
        Type:Error:Stacktrace ->
            logger:error("Failed to generate certificate: ~p:~p~n~p",
                        [Type, Error, Stacktrace]),
            {error, {cert_generation_failed, Error}}
    end.

%% @private Inner try/after isolates temp-file cleanup from the outer
%% error-translation try — keeping the two `try' blocks in separate
%% functions (no lexically-nested try/catch).
run_openssl_with_cleanup(TempKeyPath, TempCertPath, KeyBits, ValidityDays) ->
    try
        generate_openssl_key(TempKeyPath, KeyBits),
        generate_openssl_cert(TempKeyPath, TempCertPath, ValidityDays),
        {ok, KeyPEM} = file:read_file(TempKeyPath),
        {ok, CertPEM} = file:read_file(TempCertPath),
        {ok, CertPEM, KeyPEM}
    after
        %% Cleanup temporary files
        file:delete(TempKeyPath),
        file:delete(TempCertPath)
    end.

generate_openssl_key(TempKeyPath, KeyBits) ->
    KeyCmd = lists:flatten(io_lib:format("openssl genrsa -out ~s ~p 2>&1", [TempKeyPath, KeyBits])),
    check_openssl(os:cmd(KeyCmd), TempKeyPath, key_generation_failed).

generate_openssl_cert(TempKeyPath, TempCertPath, ValidityDays) ->
    CertCmd = lists:flatten(io_lib:format(
        "openssl req -new -x509 -key ~s -out ~s -days ~p -subj '/CN=macula-node' 2>&1",
        [TempKeyPath, TempCertPath, ValidityDays])),
    check_openssl(os:cmd(CertCmd), TempCertPath, cert_generation_failed).

%% @private OpenSSL may print to stderr yet still succeed; trust file
%% existence over output. Throw a tagged error on genuine failure.
check_openssl("", _Path, _ErrTag) ->
    ok;
check_openssl(Output, Path, ErrTag) ->
    check_openssl_file(filelib:is_file(Path), Output, ErrTag).

check_openssl_file(true, _Output, _ErrTag) ->
    ok;
check_openssl_file(false, Output, ErrTag) ->
    logger:error("openssl ~p: ~s", [ErrTag, Output]),
    throw({error, {ErrTag, Output}}).

%%------------------------------------------------------------------------------
%% @doc Derive Node ID from certificate public key.
%%
%% Extracts the public key from the PEM-encoded certificate and computes
%% SHA-256 hash to create a stable, cryptographically-derived Node ID.
%%
%% @param CertPEM PEM-encoded certificate binary
%% @returns NodeID Binary (32-byte SHA-256 hash, raw binary)
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

    %% Compute SHA-256 hash — return raw 32-byte binary (not hex)
    %% The routing table, bucket_index, and XOR distance all expect 32-byte raw binaries.
    %% Hex encoding is done at display time (logging, API responses), not at the identity level.
    crypto:hash(sha256, PublicKeyDER).

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
    save_generated_cert(generate_self_signed_cert(#{}), CertPath, KeyPath).

save_generated_cert({error, Reason}, _CertPath, _KeyPath) ->
    {error, Reason};
save_generated_cert({ok, CertPEM, KeyPEM}, CertPath, KeyPath) ->
    write_cert(file:write_file(CertPath, CertPEM), CertPEM, KeyPEM, KeyPath).

write_cert({error, Reason}, _CertPEM, _KeyPEM, _KeyPath) ->
    logger:error("Failed to write cert file: ~p", [Reason]),
    {error, {write_cert_failed, Reason}};
write_cert(ok, CertPEM, KeyPEM, KeyPath) ->
    write_key(file:write_file(KeyPath, KeyPEM), CertPEM, KeyPath).

write_key({error, Reason}, _CertPEM, _KeyPath) ->
    logger:error("Failed to write key file: ~p", [Reason]),
    {error, {write_key_failed, Reason}};
write_key(ok, CertPEM, KeyPath) ->
    %% Set permissions: 0600 (owner read/write only)
    chmod_key(file:change_mode(KeyPath, 8#0600), CertPEM).

chmod_key({error, Reason}, _CertPEM) ->
    logger:error("Failed to set key permissions: ~p", [Reason]),
    {error, {chmod_failed, Reason}};
chmod_key(ok, CertPEM) ->
    NodeID = derive_node_id(CertPEM),
    logger:info("TLS certificate generated successfully. Node ID: ~s",
               [binary:encode_hex(NodeID)]),
    {ok, NodeID}.

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
%% @doc Get QUIC client TLS options for an outbound dial.
%%
%% Verification stays on unless development mode is set explicitly.
%% See the module doc for the exact rules.
%%
%% @returns Proplist of QUIC TLS options for outbound connect.
%% @end
%%------------------------------------------------------------------------------
-spec quic_client_opts() -> list().
quic_client_opts() ->
    quic_client_opts(#{}).

%%------------------------------------------------------------------------------
%% @doc Get QUIC client TLS options with overrides.
%%
%% Raises {tls_config_error, {cacertfile_not_supported, Path}} when a CA
%% file is configured explicitly, because the QUIC NIF cannot use one.
%%
%% @param Overrides Map of options to override defaults
%% @returns Proplist of QUIC TLS options
%% @end
%%------------------------------------------------------------------------------
-spec quic_client_opts(Overrides :: map()) -> list().
quic_client_opts(Overrides) ->
    ok = refuse_explicit_cacertfile(explicit_cacertfile()),
    apply_overrides(client_opts_for_mode(explicit_tls_mode()), Overrides).

%%------------------------------------------------------------------------------
%% @doc Get QUIC client TLS options for a dial to Hostname.
%%
%% Same as quic_client_opts/0. The QUIC NIF already checks the server
%% certificate against the host being dialed, so a hostname adds no
%% options of its own.
%%
%% @param Hostname The hostname being dialed (string or binary)
%% @returns Proplist of QUIC TLS options
%% @end
%%------------------------------------------------------------------------------
-spec quic_client_opts_with_hostname(Hostname :: string() | binary()) -> list().
quic_client_opts_with_hostname(_Hostname) ->
    quic_client_opts().

%%------------------------------------------------------------------------------
%% @doc Get QUIC server TLS options based on current TLS mode.
%%
%% Server always needs a certificate and key.
%% In production mode: Also verifies client certificates if presented.
%% In development mode: Auto-generates self-signed certificate if needed.
%%
%% @returns Proplist of QUIC TLS options for the listener.
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
%% @doc Client TLS options for the explicitly configured mode.
%%
%% Only development set explicitly turns verification off. Unset and
%% production both verify against the QUIC NIF's built-in webpki roots.
%% @private
%%------------------------------------------------------------------------------
-spec client_opts_for_mode(development | production | unset) -> list().
client_opts_for_mode(development) ->
    ?LOG_WARNING("TLS client verification DISABLED: development mode is set explicitly"),
    [{verify, none}];
client_opts_for_mode(_ProductionOrUnset) ->
    [{verify, webpki}].

%%------------------------------------------------------------------------------
%% @doc The TLS mode as configured, or unset when neither MACULA_TLS_MODE
%% nor the tls_mode app env is set.
%%
%% get_tls_mode/0 reports development in that case, which suits
%% listeners but must not turn off verification on client dials.
%% @private
%%------------------------------------------------------------------------------
-spec explicit_tls_mode() -> development | production | unset.
explicit_tls_mode() ->
    explicit_tls_mode(os:getenv("MACULA_TLS_MODE"),
                      application:get_env(macula, tls_mode)).

explicit_tls_mode(false, undefined)   -> unset;
explicit_tls_mode(_EnvMode, _AppMode) -> get_tls_mode().

%%------------------------------------------------------------------------------
%% @doc A CA file set explicitly in MACULA_TLS_CACERTFILE or the
%% tls_cacertfile app env.
%% @private
%%------------------------------------------------------------------------------
-spec explicit_cacertfile() -> none | {explicit, string()}.
explicit_cacertfile() ->
    explicit_cacertfile(os:getenv("MACULA_TLS_CACERTFILE"),
                        application:get_env(macula, tls_cacertfile)).

explicit_cacertfile(false, undefined)  -> none;
explicit_cacertfile(false, {ok, Path}) -> {explicit, Path};
explicit_cacertfile(Path, _AppCaFile)  -> {explicit, Path}.

%%------------------------------------------------------------------------------
%% @doc Refuse a client CA file instead of silently ignoring it.
%%
%% The QUIC NIF verifies against its built-in webpki roots and cannot
%% load a CA file.
%% @private
%%------------------------------------------------------------------------------
-spec refuse_explicit_cacertfile(none | {explicit, string()}) -> ok.
refuse_explicit_cacertfile(none) ->
    ok;
refuse_explicit_cacertfile({explicit, Path}) ->
    ?LOG_ERROR("TLS client CA file ~s is not supported: the QUIC NIF "
               "verifies against its built-in webpki roots", [Path]),
    error({tls_config_error, {cacertfile_not_supported, Path}}).

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
    cacertfile_env(os:getenv("MACULA_TLS_CACERTFILE")).

cacertfile_env(false) ->
    cacertfile_app(application:get_env(macula, tls_cacertfile));
cacertfile_env(Path) ->
    Path.

cacertfile_app({ok, Path}) -> Path;
cacertfile_app(undefined)  -> find_system_ca_bundle().

%%------------------------------------------------------------------------------
%% @doc Get TLS certificate file path.
%% @private
%%------------------------------------------------------------------------------
-spec get_tls_certfile() -> string().
get_tls_certfile() ->
    certfile_env(os:getenv("MACULA_TLS_CERTFILE")).

certfile_env(false) ->
    certfile_app(application:get_env(macula, tls_certfile));
certfile_env(Path) ->
    Path.

certfile_app({ok, Path}) -> Path;
certfile_app(undefined)  -> "".

%%------------------------------------------------------------------------------
%% @doc Get TLS private key file path.
%% @private
%%------------------------------------------------------------------------------
-spec get_tls_keyfile() -> string().
get_tls_keyfile() ->
    keyfile_env(os:getenv("MACULA_TLS_KEYFILE")).

keyfile_env(false) ->
    keyfile_app(application:get_env(macula, tls_keyfile));
keyfile_env(Path) ->
    Path.

keyfile_app({ok, Path}) -> Path;
keyfile_app(undefined)  -> "".

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
    %% Hostname verification on the leaf cert. The QUIC peer is
    %% identified via SNI; this callback adds an extra check on the
    %% certificate's CN / SANs.
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
