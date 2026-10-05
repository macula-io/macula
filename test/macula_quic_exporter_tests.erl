%%%-------------------------------------------------------------------
%%% @doc What a QUIC connection exports from its TLS 1.3 session (RFC 8446
%%% section 7.5).
%%%
%%% Handshake v5 binds both proofs to this value
%%% (docs/design/DESIGN_NEIGHBOUR_CHANNEL_BINDING.md section 3), so the two ends of
%%% one connection must export the same bytes, and any other connection,
%%% label or context must export different ones.
%%%
%%% Each test starts its own loopback listener, and keeps its connection
%%% handles referenced until it closes them: a collected connection handle
%%% closes its connection.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_exporter_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LABEL, <<"EXPORTER-macula-session-v1">>).
-define(CONTEXT, <<0:256, 1:256>>).

exporter_test_() ->
    {timeout, 60,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun(Ctx) ->
          [{"the dialed and the accepted end export the same 32 bytes",
            fun() -> both_ends_export_the_same_value(Ctx) end},
           {"two connections export different values",
            fun() -> two_connections_export_different_values(Ctx) end},
           {"another context or another label exports a different value",
            fun() -> context_and_label_change_the_value(Ctx) end},
           {"a length of zero is refused",
            fun() -> zero_length_is_refused(Ctx) end},
           {"a closed connection exports nothing",
            fun() -> closed_connection_exports_nothing(Ctx) end}]
      end}}.

%%%===================================================================
%%% Fixture: one self-signed identity
%%%===================================================================

setup() ->
    Dir = macula_test_tmp:dir("macula-quic-exporter"),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(macula_test_identity:tls_seed(), [<<"localhost">>, <<"127.0.0.1">>]),
    Cert = filename:join(Dir, "a.crt"),
    Key = filename:join(Dir, "a.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    #{dir => Dir, cert => Cert, key => Key}.

cleanup(#{dir := Dir}) ->
    ok = file:del_dir_r(Dir),
    drain_quic_messages(),
    ok.

%%%===================================================================
%%% Test bodies
%%%===================================================================

both_ends_export_the_same_value(Ctx) ->
    {Listener, Port} = listen(Ctx),
    {ok, Client} = dial(Port),
    Server = accepted(),
    {ok, Exported} = macula_quic:export_keying_material(Client, ?LABEL, ?CONTEXT, 32),
    ?assertEqual(32, byte_size(Exported)),
    ?assertEqual({ok, Exported}, macula_quic:export_keying_material(Server, ?LABEL, ?CONTEXT, 32)),
    close([Client, Server], Listener).

two_connections_export_different_values(Ctx) ->
    {Listener, Port} = listen(Ctx),
    {ok, Client1} = dial(Port),
    Server1 = accepted(),
    {ok, Client2} = dial(Port),
    Server2 = accepted(),
    {ok, First} = macula_quic:export_keying_material(Client1, ?LABEL, ?CONTEXT, 32),
    {ok, Second} = macula_quic:export_keying_material(Client2, ?LABEL, ?CONTEXT, 32),
    ?assertNotEqual(First, Second),
    close([Client1, Server1, Client2, Server2], Listener).

context_and_label_change_the_value(Ctx) ->
    {Listener, Port} = listen(Ctx),
    {ok, Client} = dial(Port),
    Server = accepted(),
    {ok, Base} = macula_quic:export_keying_material(Client, ?LABEL, ?CONTEXT, 32),
    {ok, Swapped} = macula_quic:export_keying_material(Client, ?LABEL, <<1:256, 0:256>>, 32),
    {ok, Other} = macula_quic:export_keying_material(Client, <<"EXPORTER-macula-other">>, ?CONTEXT, 32),
    ?assertNotEqual(Base, Swapped),
    ?assertNotEqual(Base, Other),
    close([Client, Server], Listener).

zero_length_is_refused(Ctx) ->
    {Listener, Port} = listen(Ctx),
    {ok, Client} = dial(Port),
    Server = accepted(),
    ?assertError(function_clause, macula_quic:export_keying_material(Client, ?LABEL, ?CONTEXT, 0)),
    close([Client, Server], Listener).

closed_connection_exports_nothing(Ctx) ->
    {Listener, Port} = listen(Ctx),
    {ok, Client} = dial(Port),
    Server = accepted(),
    ok = macula_quic:close_connection(Client),
    ?assertEqual({error, already_closed}, macula_quic:export_keying_material(Client, ?LABEL, ?CONTEXT, 32)),
    close([Server], Listener).

%%%===================================================================
%%% Helpers
%%%===================================================================

listen(#{cert := Cert, key := Key}) ->
    Port = pick_free_port(),
    {ok, Listener} = macula_quic:listen(<<"127.0.0.1">>, Port,
                                        [{cert, Cert}, {key, Key},
                                         {alpn, [<<"macula">>]},
                                         {idle_timeout_ms, 30000},
                                         {keep_alive_interval_ms, 5000}]),
    ok = macula_quic:async_accept(Listener),
    {Listener, Port}.

dial(Port) ->
    macula_quic:connect(<<"127.0.0.1">>, Port, [{alpn, [<<"macula">>]}], 5000).

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
