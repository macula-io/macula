%%%-------------------------------------------------------------------
%%% @doc Tests for a client peering connection that is still dialing.
%%%
%%% While it dials, the connection handles close at once, and it stops
%%% dialing when its controlling process exits.
%%%
%%% The dial target is a UDP socket this test owns and never answers
%%% from, so the connection stays in its connecting state until it is
%%% closed or its dial times out.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peering_conn_connecting_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

close_while_dialing_stops_the_connection_test_() ->
    {timeout, 30, fun close_while_dialing_stops_the_connection/0}.

close_while_dialing_stops_the_connection() ->
    {ok, _} = application:ensure_all_started(macula),
    {Sock, Port} = silent_udp_socket(),
    {ok, Conn} = macula_peering:connect(conn_opts(self(), Port)),
    Mon = erlang:monitor(process, Conn),
    ok = first_datagram(Sock, 5_000),
    ok = macula_peering:close(Conn),
    Exited = exited_within(Mon, 1_000),
    ok = gen_udp:close(Sock),
    flush_peering_messages(Conn),
    ?assert(Exited).

controlling_process_exit_stops_the_dial_test_() ->
    {timeout, 30, fun controlling_process_exit_stops_the_dial/0}.

controlling_process_exit_stops_the_dial() ->
    {ok, _} = application:ensure_all_started(macula),
    {Sock, Port} = silent_udp_socket(),
    Test = self(),
    Controller = spawn(fun() ->
        {ok, C} = macula_peering:connect(conn_opts(self(), Port)),
        Test ! {conn, C},
        receive stop -> ok end
    end),
    Conn = receive {conn, C} -> C after 5_000 -> erlang:error(no_conn) end,
    Mon = erlang:monitor(process, Conn),
    ok = first_datagram(Sock, 5_000),
    exit(Controller, kill),
    Exited = exited_within(Mon, 1_000),
    %% A cancelled dial may send one closing packet; let it arrive before
    %% counting. A dial still running would resend within the window.
    timer:sleep(200),
    flush_datagrams(Sock),
    Later = datagrams_within(Sock, 3_000),
    ok = gen_udp:close(Sock),
    ?assert(Exited),
    ?assertEqual(0, Later).

%%%===================================================================
%%% Helpers
%%%===================================================================

conn_opts(Controller, Port) ->
    #{identity        => macula_identity:generate(),
      realms          => [],
      capabilities    => 0,
      controlling_pid => Controller,
      target          => #{host => <<"127.0.0.1">>, port => Port,
                           timeout_ms => 10_000, verify => none}}.

exited_within(Mon, TimeoutMs) ->
    receive
        {'DOWN', Mon, process, _Pid, _Reason} -> true
    after TimeoutMs ->
        false
    end.

%% The test process is the controlling process in the close test, so
%% the connection's own notifications come here. Leave none behind for
%% later tests in the same eunit process.
flush_peering_messages(Conn) ->
    receive
        {macula_peering, _Event, Conn, _Detail} -> flush_peering_messages(Conn)
    after 0 ->
        ok
    end.

silent_udp_socket() ->
    {ok, Sock} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}, {active, true}]),
    {ok, Port} = inet:port(Sock),
    {Sock, Port}.

first_datagram(Sock, TimeoutMs) ->
    receive
        {udp, Sock, _Ip, _Port, _Packet} -> ok
    after TimeoutMs ->
        erlang:error(no_datagram)
    end.

flush_datagrams(Sock) ->
    receive
        {udp, Sock, _Ip, _Port, _Packet} -> flush_datagrams(Sock)
    after 0 ->
        ok
    end.

datagrams_within(Sock, WindowMs) ->
    count_datagrams(Sock, erlang:monotonic_time(millisecond) + WindowMs, 0).

count_datagrams(Sock, Deadline, N) ->
    Left = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {udp, Sock, _Ip, _Port, _Packet} -> count_datagrams(Sock, Deadline, N + 1)
    after Left ->
        N
    end.
