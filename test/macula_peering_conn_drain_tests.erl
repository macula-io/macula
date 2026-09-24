%%%-------------------------------------------------------------------
%%% @doc A connection that is draining keeps draining when the peer's
%%% control-stream FIN arrives, until its dedicated streams are idle or the
%%% drain times out (macula#36).
%%%
%%% When this side closes first it sends GOODBYE and enters `draining'; the
%%% peer's answer is its FIN. Stopping on that FIN closed the QUIC connection
%%% at once and cut this side's own in-flight dedicated streams, so a graceful
%%% drain depended on who closed first.
%%%
%%% The connection is a real one, held in its handshake by a bare listener
%%% that never answers (as in `macula_peering_conn_control_stream_tests'),
%%% then put into `draining' directly: what is under test is how `draining'
%%% treats the FIN, not how it was entered.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peering_conn_drain_tests).

-include_lib("eunit/include/eunit.hrl").

drain_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun(Peer) ->
          {"the peer's FIN during our own drain leaves the connection draining",
           {timeout, 30, fun() -> the_peers_fin_leaves_the_drain_running(Peer) end}}
      end,
      fun(Peer) ->
          {"after that FIN, idle dedicated streams end the drain",
           {timeout, 30, fun() -> idle_streams_end_the_drain_after_the_fin(Peer) end}}
      end,
      fun(Peer) ->
          {"a control stream closed with an error still ends the drain at once",
           {timeout, 30, fun() -> a_closed_control_stream_ends_the_drain(Peer) end}}
      end]}.

setup() ->
    {ok, _} = application:ensure_all_started(macula),
    Dir = macula_test_tmp:dir("macula-peering-drain"),
    {Listener, Port} = macula_peering_handshake_tests:start_listener(listener_identity(Dir)),
    {ok, Conn} = macula_peering:connect(conn_opts(self(), Port)),
    ServerConn = accepted(),
    #{dir => Dir, listener => Listener, conn => Conn, server_conn => ServerConn}.

cleanup(#{dir := Dir, listener := Listener, conn := Conn, server_conn := ServerConn}) ->
    _ = catch macula_peering:close(Conn),
    _ = catch macula_quic:close_connection(ServerConn),
    ok = macula_peering_handshake_tests:stop_listener(Listener),
    ok = file:del_dir_r(Dir),
    drain_mailbox(),
    ok.

%%%===================================================================
%%% Test bodies
%%%===================================================================

the_peers_fin_leaves_the_drain_running(#{conn := Conn}) ->
    Stream = draining(Conn),
    Conn ! {quic, peer_send_shutdown, Stream, undefined},
    _ = sys:get_state(Conn),
    ?assert(is_process_alive(Conn)),
    ?assertEqual(draining, state_name(Conn)).

idle_streams_end_the_drain_after_the_fin(#{conn := Conn}) ->
    Stream = draining(Conn),
    Mon = erlang:monitor(process, Conn),
    Conn ! {quic, peer_send_shutdown, Stream, undefined},
    _ = sys:get_state(Conn),
    ok = gen_statem:cast(Conn, dedicated_streams_idle),
    ?assertEqual(normal, down_within(Mon, 5_000)).

a_closed_control_stream_ends_the_drain(#{conn := Conn}) ->
    Stream = draining(Conn),
    Mon = erlang:monitor(process, Conn),
    Conn ! {quic, stream_closed, Stream, aborted},
    ?assertEqual(normal, down_within(Mon, 5_000)).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% Put the handshaking connection into `draining' and return its control
%% stream, the one the peer's FIN arrives on.
draining(Conn) ->
    ?assertEqual(handshaking, state_name(Conn)),
    _ = sys:replace_state(Conn, fun({_State, Data}) -> {draining, Data} end),
    ?assertEqual(draining, state_name(Conn)),
    {_State, Data} = sys:get_state(Conn),
    Stream = element(macula_peering_conn:state_field_index(quic_stream), Data),
    ?assertNotEqual(undefined, Stream),
    Stream.

state_name(Conn) ->
    {StateName, _Data} = sys:get_state(Conn),
    StateName.

down_within(Mon, TimeoutMs) ->
    receive
        {'DOWN', Mon, process, _Pid, Reason} -> Reason
    after TimeoutMs ->
        still_alive
    end.

listener_identity(Dir) ->
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(
            macula_test_identity:tls_seed(), [<<"localhost">>, <<"127.0.0.1">>]),
    Cert = filename:join(Dir, "peer.crt"),
    Key = filename:join(Dir, "peer.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    #{cert => Cert, key => Key}.

accepted() ->
    receive
        {quic, new_conn, Conn, _Info} -> Conn
    after 5000 ->
        error(no_accepted_connection)
    end.

conn_opts(Controller, Port) ->
    {ok, Identity} = macula_node_keys:generate(identity, pq_pure),
    #{identity        => Identity,
      issuer          => Controller,
      capabilities    => 0,
      controlling_pid => Controller,
      target          => #{host => <<"127.0.0.1">>, port => Port,
                           timeout_ms => 10_000, expected_node_id => <<0:256>>}}.

drain_mailbox() ->
    receive _ -> drain_mailbox()
    after 0 -> ok
    end.
