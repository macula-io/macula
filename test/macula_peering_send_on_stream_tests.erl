%% EUnit tests for macula_peering:send_on_stream/2, over a loopback QUIC stream. A dedicated stream's writer builds, signs
%% and encodes its frames itself, and send_on_stream/2 writes exactly the bytes it is given, with no check, encoding or
%% signing of its own. The stream is real, so no test replaces macula_quic.
-module(macula_peering_send_on_stream_tests).

-include_lib("eunit/include/eunit.hrl").

-define(EVENT_MS, 5_000).

send_on_stream_test_() ->
    {timeout, 60,
     {setup,
      fun() -> {ok, _} = application:ensure_all_started(macula), ok end,
      fun(ok) -> ok end,
      [{"the bytes given arrive as they are", fun the_bytes_given_arrive_as_they_are/0},
       {"a write after the connection closed returns its error", fun a_write_after_the_connection_closed_fails/0},
       {"a frame map is not taken", fun a_frame_map_is_not_taken/0}]}}.

the_bytes_given_arrive_as_they_are() ->
    with_pair(fun(#{client_stream := Stream, server_stream := ServerStream}) ->
        ok = macula_quic:setopt(ServerStream, active, true),
        Bytes = <<"the bytes of a frame its writer built, signed and encoded">>,
        Sent = macula_peering:send_on_stream(Stream, Bytes),
        Expected = <<"open", Bytes/binary>>,
        Read = read_bytes(ServerStream, byte_size(Expected), <<>>, erlang:monotonic_time(millisecond) + ?EVENT_MS),
        ?assertEqual({ok, Expected}, {Sent, Read})
    end).

a_write_after_the_connection_closed_fails() ->
    with_pair(fun(#{client_conn := ClientConn, client_stream := Stream}) ->
        ok = macula_quic:close_connection(ClientConn),
        ?assertMatch({error, _}, macula_peering:send_on_stream(Stream, <<"bytes">>))
    end).

a_frame_map_is_not_taken() ->
    ?assertError(function_clause, macula_peering:send_on_stream(make_ref(), #{frame_type => ping})).

%%%===================================================================
%%% A loopback pair: one stream from the client, accepted on the server
%%%===================================================================

with_pair(Fun) ->
    Pair = pair(),
    try
        Fun(Pair)
    after
        stop_pair(Pair)
    end.

pair() ->
    Port = free_udp_port(),
    {ok, Listener} = macula_test_tmp:with_dir("macula-send-on-stream",
                                                        fun(Dir) -> listener(Dir, Port) end),
    ok = macula_quic:async_accept(Listener),
    {ok, ClientConn} = macula_quic:connect(<<"127.0.0.1">>, Port,
                                            [{alpn, [<<"macula">>]}],
                                            ?EVENT_MS),
    ServerConn = receive {quic, new_conn, C, _Info} -> C after ?EVENT_MS -> error(no_server_connection) end,
    ok = macula_quic:async_accept_stream(ServerConn),
    {ok, ClientStream} = macula_quic:open_stream(ClientConn),
    ok = macula_quic:send(ClientStream, <<"open">>),
    ServerStream = receive {quic, new_stream, S, _Props} -> S after ?EVENT_MS -> error(no_server_stream) end,
    #{listener => Listener, client_conn => ClientConn, server_conn => ServerConn,
      client_stream => ClientStream, server_stream => ServerStream}.

listener(Dir, Port) ->
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(macula_test_identity:tls_seed(), [<<"127.0.0.1">>]),
    Cert = filename:join(Dir, "listener.crt"),
    Key = filename:join(Dir, "listener.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    macula_quic:listen(<<"127.0.0.1">>, Port, [{cert, Cert}, {key, Key}, {alpn, [<<"macula">>]}]).

stop_pair(#{listener := Listener, client_conn := ClientConn, server_conn := ServerConn}) ->
    _ = (catch macula_quic:close_connection(ClientConn)),
    _ = (catch macula_quic:close_connection(ServerConn)),
    _ = (catch macula_quic:close_listener(Listener)),
    drain().

drain() ->
    receive
        {quic, _, _, _} -> drain()
    after 50 ->
        ok
    end.

read_bytes(_Stream, Wanted, Acc, _Deadline) when byte_size(Acc) >= Wanted ->
    Acc;
read_bytes(Stream, Wanted, Acc, Deadline) ->
    Wait = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {quic, Bin, Stream, _Flags} when is_binary(Bin) ->
            read_bytes(Stream, Wanted, <<Acc/binary, Bin/binary>>, Deadline)
    after Wait ->
        Acc
    end.

free_udp_port() ->
    {ok, Sock} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Sock),
    ok = gen_udp:close(Sock),
    Port.
