%%%-------------------------------------------------------------------
%%% @doc macula:join_dist_relay/1 and macula:dist_relay_client/0 against a
%%% running macula application.
%%%
%%% The dist relay client runs as a temporary child of macula_root. It has
%%% no reconnect: when the relay closes the connection the client ends and
%%% is not restarted. A caller learns that by monitoring the pid that
%%% dist_relay_client/0 returns, and then calls join_dist_relay/1 again.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_join_dist_relay_tests).

-include_lib("eunit/include/eunit.hrl").

-define(RELAY_ALPN, <<"macula-dist">>).
%% Port 0 is refused by the client's URL parser before it dials.
-define(UNUSABLE_RELAY_URL, <<"quic://127.0.0.1:0">>).
-define(CLIENT, macula_dist_relay_client).
-define(EVENT_TIMEOUT_MS, 15000).
-define(TEST_TIMEOUT_S, 30).

%% =============================================================================
%% Fixtures
%% =============================================================================

join_dist_relay_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"an unusable relay URL returns an error",
       {timeout, ?TEST_TIMEOUT_S, fun unusable_url_returns_error/0}},
      {"a failed join can be tried again",
       {timeout, ?TEST_TIMEOUT_S, fun failed_join_can_be_tried_again/0}},
      {"a joined client is a temporary child of macula_root",
       {timeout, ?TEST_TIMEOUT_S, fun joined_client_is_temporary_root_child/0}},
      {"a relay loss ends the client visibly and a new join works",
       {timeout, ?TEST_TIMEOUT_S, fun relay_loss_is_visible_and_rejoin_works/0}}]}.

join_dist_relay_without_app_test_() ->
    {setup,
     fun stop_macula_if_running/0,
     fun restart_macula_if_it_was_running/1,
     [{"without the macula application it returns an error",
       fun without_app_returns_error/0}]}.

setup() ->
    Saved = #{env_tls_mode  => os:getenv("MACULA_TLS_MODE"),
              env_dist_mode => os:getenv("MACULA_DIST_MODE"),
              app_tls_mode  => application:get_env(macula, tls_mode),
              was_running   => macula_running()},
    {ok, _} = application:ensure_all_started(macula),
    %% The loopback relay presents a self-signed certificate.
    os:putenv("MACULA_TLS_MODE", "development"),
    Saved.

cleanup(Saved) ->
    _ = supervisor:terminate_child(macula_root, ?CLIENT),
    restore_os_env("MACULA_TLS_MODE", maps:get(env_tls_mode, Saved)),
    restore_os_env("MACULA_DIST_MODE", maps:get(env_dist_mode, Saved)),
    restore_app_env(tls_mode, maps:get(app_tls_mode, Saved)),
    stop_macula_unless(maps:get(was_running, Saved)),
    ok.

stop_macula_if_running() ->
    WasRunning = macula_running(),
    ok = stop_macula_unless(not WasRunning),
    WasRunning.

restart_macula_if_it_was_running(true) ->
    {ok, _} = application:ensure_all_started(macula),
    ok;
restart_macula_if_it_was_running(false) ->
    ok.

%% =============================================================================
%% The assertions
%% =============================================================================

unusable_url_returns_error() ->
    ?assertMatch({error, _}, macula:join_dist_relay(#{url => ?UNUSABLE_RELAY_URL})),
    ?assertEqual({error, not_joined}, macula:dist_relay_client()).

failed_join_can_be_tried_again() ->
    Root = whereis(macula_root),
    ?assertMatch({error, _}, macula:join_dist_relay(#{url => ?UNUSABLE_RELAY_URL})),
    ?assertMatch({error, _}, macula:join_dist_relay(#{url => ?UNUSABLE_RELAY_URL})),
    ?assertEqual(Root, whereis(macula_root)).

joined_client_is_temporary_root_child() ->
    Relay = start_relay(),
    Root = whereis(macula_root),

    ?assertEqual(ok, macula:join_dist_relay(#{url => relay_url(Relay)})),
    {ok, Client} = macula:dist_relay_client(),
    ?assertMatch({?CLIENT, Client, worker, _},
                 lists:keyfind(?CLIENT, 1, supervisor:which_children(macula_root))),
    ?assertMatch({ok, #{restart := temporary}},
                 supervisor:get_childspec(macula_root, ?CLIENT)),

    %% Joining again while the client runs keeps the same client.
    ?assertEqual(ok, macula:join_dist_relay(#{url => relay_url(Relay)})),
    ?assertEqual({ok, Client}, macula:dist_relay_client()),

    %% A client that ends leaves no child behind and the root keeps running.
    ok = supervisor:terminate_child(macula_root, ?CLIENT),
    ?assertEqual({error, not_joined}, macula:dist_relay_client()),
    ?assertEqual(false, lists:keyfind(?CLIENT, 1, supervisor:which_children(macula_root))),
    ?assertEqual(Root, whereis(macula_root)),
    stop_relay(Relay).

relay_loss_is_visible_and_rejoin_works() ->
    Relay = start_relay(),
    Root = whereis(macula_root),

    ?assertEqual(ok, macula:join_dist_relay(#{url => relay_url(Relay)})),
    RelaySideConn = accepted_connection(),
    {ok, Client} = macula:dist_relay_client(),
    MonRef = erlang:monitor(process, Client),

    ok = macula_quic:close_connection(RelaySideConn),
    ?assertMatch({relay_closed, _}, down_reason(MonRef, Client)),
    ?assertEqual({error, not_joined}, macula:dist_relay_client()),
    ?assertEqual(Root, whereis(macula_root)),

    ok = macula_quic:async_accept(maps:get(listener, Relay)),
    ?assertEqual(ok, macula:join_dist_relay(#{url => relay_url(Relay)})),
    ?assertMatch({ok, _}, macula:dist_relay_client()),
    ok = supervisor:terminate_child(macula_root, ?CLIENT),
    stop_relay(Relay).

without_app_returns_error() ->
    ?assertEqual({error, macula_not_started},
                 macula:join_dist_relay(#{url => ?UNUSABLE_RELAY_URL})).

%% =============================================================================
%% Loopback relay: a QUIC listener with the relay's ALPN and a self-signed
%% certificate. Started in the test process, which then receives its
%% connection events.
%% =============================================================================

start_relay() ->
    {Pub, Priv} = ephemeral_keypair(),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(
            Pub, Priv, [<<"localhost">>, <<"127.0.0.1">>]),
    Tmp  = lists:flatten(io_lib:format("/tmp/macula-join-dist-relay-~p",
                                       [erlang:unique_integer([positive])])),
    Cert = Tmp ++ ".crt",
    Key  = Tmp ++ ".key",
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key,  KeyPem),
    Port = pick_free_port(),
    {ok, Listener} = macula_quic:listen(
        <<"127.0.0.1">>, Port,
        [{cert, Cert}, {key, Key},
         {alpn, [?RELAY_ALPN]},
         {idle_timeout_ms, 30000},
         {keep_alive_interval_ms, 5000}]),
    ok = macula_quic:async_accept(Listener),
    #{listener => Listener, port => Port, cert => Cert, key => Key}.

stop_relay(#{listener := Listener, cert := Cert, key := Key}) ->
    try macula_quic:close_listener(Listener) catch _:_ -> ok end,
    file:delete(Cert),
    file:delete(Key),
    drain_quic_messages().

relay_url(#{port := Port}) ->
    iolist_to_binary(io_lib:format("quic://127.0.0.1:~p", [Port])).

accepted_connection() ->
    receive
        {quic, new_conn, Conn, _Info} -> Conn
    after ?EVENT_TIMEOUT_MS ->
        error(no_relay_side_connection)
    end.

down_reason(MonRef, Pid) ->
    receive
        {'DOWN', MonRef, process, Pid, Reason} -> Reason
    after ?EVENT_TIMEOUT_MS ->
        error(client_did_not_end)
    end.

%% =============================================================================
%% Helpers
%% =============================================================================

macula_running() ->
    lists:keymember(macula, 1, application:which_applications()).

stop_macula_unless(true) ->
    ok;
stop_macula_unless(false) ->
    application:stop(macula),
    ok.

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
