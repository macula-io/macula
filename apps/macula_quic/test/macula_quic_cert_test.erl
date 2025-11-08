%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_quic_cert module.
%%% Utility module for managing QUIC certificates.
%%% Tests written FIRST (TDD red phase).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_cert_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Certificate Generation via OpenSSL Tests
%%%===================================================================

%% Test: generate_self_signed creates cert and key files
generate_self_signed_creates_files_test() ->
    TempDir = "/tmp/macula_cert_test",
    file:make_dir(TempDir),

    Result = macula_quic_cert:generate_self_signed(TempDir),
    ?assertMatch({ok, {_CertFile, _KeyFile}}, Result),

    {ok, {CertFile, KeyFile}} = Result,
    ?assert(filelib:is_file(CertFile)),
    ?assert(filelib:is_file(KeyFile)),

    %% Cleanup
    file:delete(CertFile),
    file:delete(KeyFile),
    file:del_dir(TempDir).

%% Test: generated cert file contains PEM data
generate_self_signed_cert_is_pem_test() ->
    TempDir = "/tmp/macula_cert_test2",
    file:make_dir(TempDir),

    {ok, {CertFile, KeyFile}} = macula_quic_cert:generate_self_signed(TempDir),

    %% Read cert file
    {ok, CertData} = file:read_file(CertFile),
    ?assert(binary:match(CertData, <<"-----BEGIN CERTIFICATE-----">>) =/= nomatch),

    %% Cleanup
    file:delete(CertFile),
    file:delete(KeyFile),
    file:del_dir(TempDir).

%% Test: generated key file contains PEM data
generate_self_signed_key_is_pem_test() ->
    TempDir = "/tmp/macula_cert_test3",
    file:make_dir(TempDir),

    {ok, {CertFile, KeyFile}} = macula_quic_cert:generate_self_signed(TempDir),

    %% Read key file
    {ok, KeyData} = file:read_file(KeyFile),
    ?assert(binary:match(KeyData, <<"-----BEGIN">>) =/= nomatch),

    %% Cleanup
    file:delete(CertFile),
    file:delete(KeyFile),
    file:del_dir(TempDir).

%% Test: can generate with custom subject
generate_with_custom_subject_test() ->
    TempDir = "/tmp/macula_cert_test4",
    file:make_dir(TempDir),

    Opts = #{subject => "/CN=test.macula.local"},
    Result = macula_quic_cert:generate_self_signed(TempDir, Opts),
    ?assertMatch({ok, {_CertFile, _KeyFile}}, Result),

    {ok, {CertFile, KeyFile}} = Result,

    %% Cleanup
    file:delete(CertFile),
    file:delete(KeyFile),
    file:del_dir(TempDir).

%% Test: can generate with custom validity period
generate_with_custom_validity_test() ->
    TempDir = "/tmp/macula_cert_test5",
    file:make_dir(TempDir),

    Opts = #{validity_days => 30},
    Result = macula_quic_cert:generate_self_signed(TempDir, Opts),
    ?assertMatch({ok, {_CertFile, _KeyFile}}, Result),

    {ok, {CertFile, KeyFile}} = Result,

    %% Cleanup
    file:delete(CertFile),
    file:delete(KeyFile),
    file:del_dir(TempDir).

%%%===================================================================
%%% Certificate Validation Tests
%%%===================================================================

%% Test: validate_files checks both cert and key exist
validate_files_success_test() ->
    TempDir = "/tmp/macula_cert_test6",
    file:make_dir(TempDir),

    {ok, {CertFile, KeyFile}} = macula_quic_cert:generate_self_signed(TempDir),

    Result = macula_quic_cert:validate_files(CertFile, KeyFile),
    ?assertEqual(ok, Result),

    %% Cleanup
    file:delete(CertFile),
    file:delete(KeyFile),
    file:del_dir(TempDir).

%% Test: validate_files fails if cert file missing
validate_files_missing_cert_test() ->
    Result = macula_quic_cert:validate_files("/tmp/nonexistent_cert.pem", "/tmp/nonexistent_key.pem"),
    ?assertMatch({error, {cert_not_found, _}}, Result).

%% Test: validate_files fails if key file missing
validate_files_missing_key_test() ->
    TempDir = "/tmp/macula_cert_test7",
    file:make_dir(TempDir),

    {ok, {CertFile, KeyFile}} = macula_quic_cert:generate_self_signed(TempDir),

    %% Delete key file
    file:delete(KeyFile),

    Result = macula_quic_cert:validate_files(CertFile, KeyFile),
    ?assertMatch({error, {key_not_found, _}}, Result),

    %% Cleanup
    file:delete(CertFile),
    file:del_dir(TempDir).
