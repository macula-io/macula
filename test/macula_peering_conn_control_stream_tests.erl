%%%-------------------------------------------------------------------
%%% @doc A peering connection's CONTROL STREAM is the stream the client
%%% opened, and a stream the peer opens during the handshake is closed
%%% rather than adopted.
%%%
%%% macula#23. `handshaking(info, {quic, new_stream, ...})' matched every
%%% inbound stream with no role guard and no check that the control stream
%%% was already set, so a peer could replace the control channel
%%% mid-handshake. Every later handshaking clause matches on
%%% `#data{quic_stream = Stream}', so the frames the handshake reads would
%%% then be read from the peer's stream rather than the one this side
%%% opened.
%%%
%%% == How the connection is held in its handshake ==
%%%
%%% The peer here is a BARE `macula_quic' listener, not a peering listener.
%%% It completes the QUIC and TLS handshake and then says nothing at all, so
%%% the peering connection sends its opener, waits for a challenge that never
%%% comes, and sits in `handshaking' for the whole test. That is what makes
%%% these cases deterministic rather than a race against a handshake that
%%% would otherwise finish in microseconds.
%%%
%%% == What the unfixed code did, because it shapes the assertions ==
%%%
%%% The peer's stream was adopted as the control stream, the peer's bytes were
%%% then read as handshake frames, the frame check refused them, and THE
%%% CONNECTION DIED. So "still alive and still handshaking" is the sharpest
%%% observable difference, and it is what these cases assert.
%%%
%%% ⚠ TWO WAYS THIS TEST CAN LIE, and the first draft of it met both:
%%%
%%% <ol>
%%%   <li>WITHOUT A WRITE the stream never reaches the far side, so nothing
%%%       happens and "the control stream is unchanged" is vacuously true.
%%%       Every case here writes to the stream it opens.</li>
%%%   <li>ASSERTING ONLY THAT THE PEER'S STREAM CLOSED is satisfied by the
%%%       connection dying, which closes every stream on it — the OLD
%%%       behaviour passing a test written for the new one. So the close is
%%%       asserted together with the connection still being up.</li>
%%% </ol>
%%%
%%% The control stream itself is invisible from outside the connection, so it
%%% is read out of the gen_statem's data BY NAME through
%%% `macula_peering_conn:state_field_index/1'.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peering_conn_control_stream_tests).

-include_lib("eunit/include/eunit.hrl").

control_stream_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun(Peer) ->
          {"a stream the peer opens mid-handshake leaves the handshake running "
           "and the control stream untouched",
           {timeout, 30,
            fun() -> a_peer_opened_stream_leaves_the_handshake_running(Peer) end}}
      end,
      fun(Peer) ->
          {"that stream is CLOSED, and the connection is still up, which is "
           "what tells the fix apart from the connection dying",
           {timeout, 30,
            fun() -> a_peer_opened_stream_is_closed_and_the_connection_survives(Peer) end}}
      end]}.

%% ⚠ EVERYTHING A CASE USES IS STARTED HERE AND TORN DOWN IN `cleanup/1',
%% the connections included. A teardown written at the end of a test body does
%% not run when an assertion fails, so it passes today and leaks a listener, a
%% socket and two QUIC endpoints into whatever module eunit runs next.
setup() ->
    {ok, _} = application:ensure_all_started(macula),
    Dir = macula_test_tmp:dir("macula-peering-control-stream"),
    Identity = listener_identity(Dir),
    %% The listener helper is `macula_peering_handshake_tests''s, exported and
    %% already used across modules. One copy of it, not two.
    {Listener, Port} = macula_peering_handshake_tests:start_listener(Identity),
    {ok, Conn} = macula_peering:connect(conn_opts(self(), Port)),
    ServerConn = accepted(),
    #{dir => Dir, listener => Listener, conn => Conn, server_conn => ServerConn}.

cleanup(#{dir := Dir, listener := Listener, conn := Conn, server_conn := ServerConn}) ->
    _ = catch macula_peering:close(Conn),
    _ = catch macula_quic:close_connection(ServerConn),
    ok = macula_peering_handshake_tests:stop_listener(Listener),
    ok = file:del_dir_r(Dir),
    drain(),
    ok.

%%%===================================================================
%%% Test bodies
%%%===================================================================

%% The handshake carries on. Before the fix the peer's bytes were read as
%% handshake frames off the adopted stream, refused, and the connection was
%% torn down: a peer could end any handshake it liked by opening a stream and
%% writing one byte into it.
a_peer_opened_stream_leaves_the_handshake_running(Peer) ->
    {Conn, ServerConn} = handshaking(Peer),
    Before = control_stream(Conn),
    ?assertNotEqual(undefined, Before),

    ok = write_a_stream(ServerConn),
    ok = quiet(Conn),

    ?assert(is_process_alive(Conn)),
    ?assertEqual(handshaking, state_name(Conn)),
    ?assertEqual(Before, control_stream(Conn)).

%% Not adopting it is not enough: a stream nobody will ever read is one the
%% peer may keep writing into. It is closed, and the peer's own handle is
%% where that is observable.
%%
%% ⚠ THE SECOND ASSERTION IS WHAT MAKES THE FIRST MEAN ANYTHING. A dying
%% connection closes every stream on it, so "the peer's stream closed" is
%% exactly what the OLD behaviour produced as well. Only closed AND still
%% connected tells the two apart.
a_peer_opened_stream_is_closed_and_the_connection_survives(Peer) ->
    {Conn, ServerConn} = handshaking(Peer),
    {ok, PeerStream} = macula_quic:open_stream(ServerConn),
    ok = macula_quic:setopt(PeerStream, active, true),
    ok = macula_quic:send(PeerStream, <<"unread">>),

    ?assert(closed_within(PeerStream, 5_000)),
    ?assert(is_process_alive(Conn)),
    ?assertEqual(handshaking, state_name(Conn)).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% The pair the fixture started, checked to be where these cases need it: the
%% peer never answers, so the connection is still handshaking.
handshaking(#{conn := Conn, server_conn := ServerConn}) ->
    ?assertEqual(handshaking, state_name(Conn)),
    {Conn, ServerConn}.

state_name(Conn) ->
    {StateName, _Data} = sys:get_state(Conn),
    StateName.

control_stream(Conn) ->
    {_StateName, Data} = sys:get_state(Conn),
    element(macula_peering_conn:state_field_index(quic_stream), Data).

%% Opens a stream on `Conn' and writes to it. THE WRITE IS WHAT PUTS THE
%% STREAM ON THE WIRE: without one the far side never sees it, and a test
%% about what the far side does with it is testing nothing at all.
write_a_stream(Conn) ->
    {ok, Stream} = macula_quic:open_stream(Conn),
    macula_quic:send(Stream, <<"not the control stream">>).

%% Long enough for the stream and its bytes to cross loopback and be handled,
%% then a synchronous call so nothing is left unprocessed. The sleep is a
%% settling window, not an assertion about timing: what follows it asserts
%% STATE, and the old behaviour reached its end state in under 200 ms.
quiet(Conn) ->
    timer:sleep(500),
    _ = catch sys:get_state(Conn),
    ok.

closed_within(Stream, TimeoutMs) ->
    receive
        {quic, Closed, Stream, _Detail}
          when Closed =:= stream_closed; Closed =:= peer_send_shutdown -> true
    after TimeoutMs ->
        false
    end.

%%%===================================================================
%%% A bare listener that never answers
%%%===================================================================

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

drain() ->
    receive
        {quic, _, _, _} -> drain();
        {macula_peering, _, _, _} -> drain()
    after 0 -> ok
    end.
