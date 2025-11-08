%%%-------------------------------------------------------------------
%%% @doc
%%% Macula QUIC certificate utilities.
%%% Provides functions for generating and validating self-signed certificates
%%% for QUIC connections via OpenSSL command-line tool.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_cert).

-export([
    generate_self_signed/1,
    generate_self_signed/2,
    validate_files/2
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Generate a self-signed certificate and key in the given directory.
%% Returns {ok, {CertFile, KeyFile}} with paths to generated files.
%% @end
-spec generate_self_signed(file:filename()) -> {ok, {file:filename(), file:filename()}} | {error, term()}.
generate_self_signed(Dir) ->
    generate_self_signed(Dir, #{}).

%% @doc Generate a self-signed certificate with custom options.
%% Options:
%%   subject - Certificate subject (default: "/CN=macula.local")
%%   validity_days - Validity period in days (default: 365)
%% @end
-spec generate_self_signed(file:filename(), map()) -> {ok, {file:filename(), file:filename()}} | {error, term()}.
generate_self_signed(Dir, Opts) ->
    %% Ensure directory exists
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),

    %% Build file paths
    CertFile = filename:join(Dir, "cert.pem"),
    KeyFile = filename:join(Dir, "key.pem"),

    %% Extract options
    Subject = maps:get(subject, Opts, "/CN=macula.local"),
    ValidityDays = maps:get(validity_days, Opts, 365),

    %% Build openssl command
    %% Generate RSA private key (2048 bits) and self-signed certificate in one command
    Cmd = io_lib:format(
        "openssl req -x509 -newkey rsa:2048 -nodes "
        "-keyout ~s -out ~s -days ~p -subj '~s' 2>/dev/null",
        [KeyFile, CertFile, ValidityDays, Subject]
    ),

    %% Execute command
    case os:cmd(lists:flatten(Cmd)) of
        "" ->
            %% Success (openssl writes to stderr which we redirected)
            {ok, {CertFile, KeyFile}};
        Error ->
            {error, {openssl_failed, Error}}
    end.

%% @doc Validate that both certificate and key files exist and are readable.
-spec validate_files(file:filename(), file:filename()) -> ok | {error, term()}.
validate_files(CertFile, KeyFile) ->
    case filelib:is_file(CertFile) of
        false ->
            {error, {cert_not_found, CertFile}};
        true ->
            case filelib:is_file(KeyFile) of
                false ->
                    {error, {key_not_found, KeyFile}};
                true ->
                    ok
            end
    end.
