%%%-------------------------------------------------------------------
%%% @doc What a QUIC connection reports about the TLS leaf certificate of
%%% its own handshake.
%%%
%%% The post-quantum CONNECT proof hashes the station's leaf as the client
%%% received it, and the station checks that proof against the leaf this
%%% connection presented, never the certificate configured now
%%% (plans/DESIGN_PQ_HANDSHAKE_FRAMES.md). So a dialed connection reports
%%% the leaf it received, an accepted connection reports the leaf it
%%% presented, and a certificate reload on the listener never changes the
%%% leaf a live connection reports. A reload that cannot load, or whose key
%%% does not match its certificate, keeps the current certificate.
%%%
%%% Each test starts its own loopback listener, and keeps its connection
%%% handles referenced until it closes them: a collected connection handle
%%% closes its connection.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_connection_leaf_tests).

-include_lib("eunit/include/eunit.hrl").

connection_leaf_test_() ->
    {timeout, 60,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun(Ctx) ->
          [{"a dialed and an accepted connection report the same leaf, byte for byte",
            fun() -> dialed_and_accepted_report_the_same_leaf(Ctx) end},
           {"a reload between two accepts leaves each connection with the leaf it presented",
            fun() -> reload_leaves_each_connection_its_own_leaf(Ctx) end},
           {"a dialed connection presented no leaf, and an accepted one received none",
            fun() -> each_side_reports_only_its_own_leaf(Ctx) end},
           {"a reload that cannot read its files keeps the current certificate",
            fun() -> unreadable_reload_keeps_current_certificate(Ctx) end},
           {"a reload whose key does not match its certificate keeps the current certificate",
            fun() -> mismatched_reload_keeps_current_certificate(Ctx) end}]
      end}}.

%%%===================================================================
%%% Fixture: two self-signed identities, A and B
%%%===================================================================

setup() ->
    Dir = macula_test_tmp:dir("macula-quic-leaf"),
    #{dir => Dir, a => identity(Dir, "a"), b => identity(Dir, "b")}.

cleanup(#{dir := Dir}) ->
    ok = file:del_dir_r(Dir),
    drain_quic_messages(),
    ok.

identity(Dir, Name) ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    PubBin = iolist_to_binary(Pub),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(
            PubBin, iolist_to_binary(Priv), [<<"localhost">>, <<"127.0.0.1">>]),
    Cert = filename:join(Dir, Name ++ ".crt"),
    Key = filename:join(Dir, Name ++ ".key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    [{'Certificate', Der, not_encrypted}] = public_key:pem_decode(CertPem),
    #{pub => PubBin, cert => Cert, key => Key, der => Der}.

%%%===================================================================
%%% Test bodies
%%%===================================================================

dialed_and_accepted_report_the_same_leaf(#{a := #{der := DerA} = A}) ->
    {Listener, Port} = listen(A),
    {ok, Client} = dial(Port, A),
    Server = accepted(),
    ?assertEqual({ok, DerA}, macula_quic:peer_leaf(Client)),
    ?assertEqual({ok, DerA}, macula_quic:presented_leaf(Server)),
    close([Client, Server], Listener).

reload_leaves_each_connection_its_own_leaf(#{a := #{der := DerA} = A,
                                             b := #{der := DerB, cert := CertB, key := KeyB} = B}) ->
    {Listener, Port} = listen(A),
    {ok, ClientA} = dial(Port, A),
    ServerA = accepted(),
    ?assertEqual(ok, macula_quic:reload_certificate(Listener, CertB, KeyB)),
    {ok, ClientB} = dial(Port, B),
    ServerB = accepted(),
    ?assertEqual({ok, DerA}, macula_quic:presented_leaf(ServerA)),
    ?assertEqual({ok, DerB}, macula_quic:presented_leaf(ServerB)),
    ?assertEqual({ok, DerA}, macula_quic:peer_leaf(ClientA)),
    ?assertEqual({ok, DerB}, macula_quic:peer_leaf(ClientB)),
    %% A dial that pins A's key now fails: the listener presents B.
    ?assertMatch({error, _}, dial(Port, A)),
    close([ClientA, ServerA, ClientB, ServerB], Listener).

each_side_reports_only_its_own_leaf(#{a := A}) ->
    {Listener, Port} = listen(A),
    {ok, Client} = dial(Port, A),
    Server = accepted(),
    ?assertEqual({error, no_presented_leaf}, macula_quic:presented_leaf(Client)),
    ?assertEqual({error, no_peer_leaf}, macula_quic:peer_leaf(Server)),
    close([Client, Server], Listener).

unreadable_reload_keeps_current_certificate(#{a := #{cert := CertA, key := KeyA} = A}) ->
    {Listener, Port} = listen(A),
    ?assertMatch({error, _},
                 macula_quic:reload_certificate(Listener, CertA ++ ".missing", KeyA)),
    assert_new_connection_presents(Listener, Port, A).

mismatched_reload_keeps_current_certificate(#{a := #{key := KeyA} = A,
                                              b := #{cert := CertB}}) ->
    {Listener, Port} = listen(A),
    ?assertMatch({error, _}, macula_quic:reload_certificate(Listener, CertB, KeyA)),
    assert_new_connection_presents(Listener, Port, A).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% A new connection completes its handshake, and both of its ends report
%% Identity's leaf. Closes the listener afterwards.
assert_new_connection_presents(Listener, Port, #{der := Der} = Identity) ->
    {ok, Client} = dial(Port, Identity),
    Server = accepted(),
    ?assertEqual({ok, Der}, macula_quic:presented_leaf(Server)),
    ?assertEqual({ok, Der}, macula_quic:peer_leaf(Client)),
    close([Client, Server], Listener).

listen(#{cert := Cert, key := Key}) ->
    Port = pick_free_port(),
    {ok, Listener} = macula_quic:listen(<<"127.0.0.1">>, Port,
                                        [{cert, Cert}, {key, Key},
                                         {alpn, [<<"macula">>]},
                                         {idle_timeout_ms, 30000},
                                         {keep_alive_interval_ms, 5000}]),
    ok = macula_quic:async_accept(Listener),
    {Listener, Port}.

%% Dials the listener, pinning the key of the identity it should present.
dial(Port, #{pub := Pub}) ->
    macula_quic:connect(<<"127.0.0.1">>, Port,
                        [{verify_pubkey, Pub}, {alpn, [<<"macula">>]}], 5000).

accepted() ->
    receive
        {quic, new_conn, Conn, _Info} -> Conn
    after 5000 ->
        error(no_accepted_connection)
    end.

close(Connections, Listener) ->
    lists:foreach(fun macula_quic:close_connection/1, Connections),
    ok = macula_quic:close_listener(Listener).

pick_free_port() ->
    {ok, S} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}]),
    {ok, P} = inet:port(S),
    ok = gen_udp:close(S),
    P.

drain_quic_messages() ->
    receive
        {quic, _, _, _} -> drain_quic_messages()
    after 0 -> ok
    end.
