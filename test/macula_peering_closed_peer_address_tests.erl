%%%-------------------------------------------------------------------
%%% @doc A peering connection that closes during its handshake names the
%%% address it was talking to.
%%%
%%% macula#24. `_macula.peering.closed' carried `role' and `reason' only. A
%%% connection refused at its first frame, as every 11.x peer is by a 12
%%% station (the opener carries the handshake version), has no peer identity
%%% yet: that arrives in CONNECT, which never happens. So a station refusing
%%% a whole fleet logged the same anonymous line over and over, and looked
%%% idle. The QUIC connection's remote address IS known before the handshake,
%%% so the event carries it as `peer'.
%%%
%%% == How the connection is closed ==
%%%
%%% A real peering connection dials a BARE `macula_quic' listener, the same
%%% harness as `macula_peering_conn_control_stream_tests'. The bare peer
%%% writes a length header above the handshake's 64 KiB cap onto the
%%% connection's control stream, and the connection closes `malformed_frame'
%%% from the header alone, through the same `closed/2' a version refusal
%%% goes through. The address in the event is the listener's, which is what
%%% the connection dialled.
%%%
%%% `bounded_event' logs this event at most once per 10 s node-wide, so the
%%% test first clears the event's window: another module's closed connection
%%% in the last 10 s would otherwise hold this one back, and the test would
%%% fail for a reason that has nothing to do with the address.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peering_closed_peer_address_tests).

-include_lib("eunit/include/eunit.hrl").

%% The capture handler's callback.
-export([log/2]).

-define(HANDLER, macula_peering_closed_capture).
-define(TOPIC, <<"_macula.peering.closed">>).

closed_peer_address_test_() ->
    %% `local': the bare peer's QUIC messages and the captured log lines go to
    %% the process that ran setup, so the test body has to be that process.
    {foreach, local, fun setup/0, fun cleanup/1,
     [fun(Ctx) ->
          {"a connection closed in its handshake names the address it dialled",
           {timeout, 30, fun() -> the_closed_event_names_the_peer(Ctx) end}}
      end]}.

setup() ->
    {ok, _} = application:ensure_all_started(macula),
    Dir = macula_test_tmp:dir("macula-peering-closed-address"),
    {Listener, Port} = macula_peering_handshake_tests:start_listener(listener_identity(Dir)),
    #{level := Primary} = logger:get_primary_config(),
    ok = logger:set_primary_config(level, all),
    ok = logger:add_handler(?HANDLER, ?MODULE, #{level => all, config => #{test => self()}}),
    _ = ets:delete(macula_diagnostics_bound, {topic, ?TOPIC}),
    {ok, Conn} = macula_peering:connect(conn_opts(self(), Port)),
    ServerConn = accepted(),
    %% The bare peer hears the streams the connection opens only once it asks.
    ok = macula_quic:async_accept_stream(ServerConn),
    #{dir => Dir, listener => Listener, port => Port, conn => Conn,
      server_conn => ServerConn, primary => Primary}.

cleanup(#{dir := Dir, listener := Listener, conn := Conn, server_conn := ServerConn,
          primary := Primary}) ->
    _ = logger:remove_handler(?HANDLER),
    ok = logger:set_primary_config(level, Primary),
    _ = catch macula_peering:close(Conn),
    _ = catch macula_quic:close_connection(ServerConn),
    ok = macula_peering_handshake_tests:stop_listener(Listener),
    ok = file:del_dir_r(Dir),
    drain(),
    ok.

the_closed_event_names_the_peer(#{port := Port, conn := Conn}) ->
    Control = control_stream(),
    Mon = erlang:monitor(process, Conn),
    %% 64 KiB + 1: refused from the four header bytes.
    ok = macula_quic:send(Control, <<(65536 + 1):32>>),
    receive {'DOWN', Mon, process, Conn, _} -> ok after 5_000 -> error(connection_did_not_close) end,
    #{role := client, reason := malformed_frame, peer := Peer} = closed_event(2_000),
    ?assertEqual(iolist_to_binary(["127.0.0.1:", integer_to_list(Port)]), Peer).

%%%===================================================================
%%% Helpers
%%%===================================================================

log(Event, #{config := #{test := Test}}) ->
    Test ! {captured, Event}.

closed_event(Timeout) ->
    receive
        {captured, #{msg := {report, #{event := ?TOPIC, properties := Props}}}} -> Props
    after Timeout ->
        error(no_closed_event)
    end.

%% The stream the connection opened for its handshake, as the bare peer sees
%% it arrive, made active so the bytes written to it go out.
control_stream() ->
    receive
        {quic, new_stream, Stream, _Props} ->
            ok = macula_quic:setopt(Stream, active, true),
            Stream
    after 5_000 ->
        error(no_control_stream)
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

drain() ->
    receive _ -> drain()
    after 0 -> ok
    end.
