%%%-------------------------------------------------------------------
%%% @doc What the TLS options from macula_tls mean on a real handshake.
%%%
%%% macula_tls_tests pins the option list quic_client_opts/0 returns.
%%% These tests pin its effect: with no TLS mode configured, a dial
%%% refuses a self-signed listener; only an explicitly configured
%%% development mode skips verification and connects.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_tls_dial_verification_tests).

-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Test fixtures
%% =============================================================================

dial_verification_test_() ->
    {timeout, 30,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun(Ctx) ->
          [{"no TLS mode configured refuses a self-signed listener",
            fun() -> unset_mode_refuses_self_signed_listener(Ctx) end},
           {"explicit development mode connects to a self-signed listener",
            fun() -> explicit_development_connects(Ctx) end}]
      end}}.

%% =============================================================================
%% Setup: ephemeral self-signed listener on loopback
%% =============================================================================

setup() ->
    Saved = #{env_mode       => os:getenv("MACULA_TLS_MODE"),
              env_cacertfile => os:getenv("MACULA_TLS_CACERTFILE"),
              app_mode       => application:get_env(macula, tls_mode),
              app_cacertfile => application:get_env(macula, tls_cacertfile)},
    {Pub, Priv} = ephemeral_keypair(),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(
            Pub, Priv, [<<"localhost">>, <<"127.0.0.1">>]),
    Tmp  = lists:flatten(io_lib:format("/tmp/macula-tls-dial-verify-~p",
                                       [erlang:unique_integer([positive])])),
    Cert = Tmp ++ ".crt",
    Key  = Tmp ++ ".key",
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key,  KeyPem),
    Port = pick_free_port(),
    {ok, Listener} = macula_quic:listen(
        <<"127.0.0.1">>, Port,
        [{cert, Cert}, {key, Key},
         {alpn, [<<"macula">>]},
         {idle_timeout_ms, 30000},
         {keep_alive_interval_ms, 5000}]),
    ok = macula_quic:async_accept(Listener),
    Saved#{listener => Listener,
           port     => Port,
           cert     => Cert,
           key      => Key}.

cleanup(#{listener := L, cert := Cert, key := Key} = Ctx) ->
    try macula_quic:close_listener(L) catch _:_ -> ok end,
    file:delete(Cert),
    file:delete(Key),
    restore_client_tls_env(Ctx),
    drain_quic_messages(),
    ok.

%% =============================================================================
%% The assertions
%% =============================================================================

unset_mode_refuses_self_signed_listener(#{port := Port}) ->
    clear_client_tls_env(),
    Result = dial(Port, macula_tls:quic_client_opts()),
    close_if_connected(Result),
    ?assertMatch({error, _}, Result).

explicit_development_connects(#{port := Port}) ->
    clear_client_tls_env(),
    os:putenv("MACULA_TLS_MODE", "development"),
    Result = dial(Port, macula_tls:quic_client_opts()),
    close_if_connected(Result),
    ?assertMatch({ok, _}, Result).

%% =============================================================================
%% Helpers
%% =============================================================================

dial(Port, TlsOpts) ->
    macula_quic:connect(<<"127.0.0.1">>, Port,
                        [{alpn, [<<"macula">>]} | TlsOpts], 5000).

close_if_connected({ok, Conn}) -> macula_quic:close_connection(Conn);
close_if_connected(_Error)     -> ok.

clear_client_tls_env() ->
    os:unsetenv("MACULA_TLS_MODE"),
    os:unsetenv("MACULA_TLS_CACERTFILE"),
    application:unset_env(macula, tls_mode),
    application:unset_env(macula, tls_cacertfile).

restore_client_tls_env(#{env_mode := EnvMode, env_cacertfile := EnvCa,
                         app_mode := AppMode, app_cacertfile := AppCa}) ->
    restore_os_env("MACULA_TLS_MODE", EnvMode),
    restore_os_env("MACULA_TLS_CACERTFILE", EnvCa),
    restore_app_env(tls_mode, AppMode),
    restore_app_env(tls_cacertfile, AppCa).

restore_os_env(Name, false) -> os:unsetenv(Name);
restore_os_env(Name, Value) -> os:putenv(Name, Value).

restore_app_env(Key, undefined)   -> application:unset_env(macula, Key);
restore_app_env(Key, {ok, Value}) -> application:set_env(macula, Key, Value).

ephemeral_keypair() ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {iolist_to_binary(Pub), iolist_to_binary(Priv)}.

pick_free_port() ->
    {ok, S} = gen_udp:open(0, [binary, {ip, {127,0,0,1}}]),
    {ok, P} = inet:port(S),
    gen_udp:close(S),
    P.

drain_quic_messages() ->
    receive
        {quic, _, _, _} -> drain_quic_messages()
    after 0 -> ok
    end.
