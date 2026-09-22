%% A loopback QUIC stream pair for tests: a listener, a dial to it, and one stream open between them, with the
%% accepting side's stream already read up to the byte that opened it.
%%
%% A distribution tunnel is a stream something else forwards, so a test of one needs a real stream and nothing more
%% around it. This makes that, and removes what it made.
-module(macula_test_quic_pair).

-export([open/1, close/1]).

-define(EVENT_MS, 10_000).

%% @doc A pair under Prefix, which names the temporary directory holding the listener's certificate. The directory
%% outlives the listener, since an accepting side reads the same certificate again when its TLS session starts.
-spec open(string()) -> map().
open(Prefix) ->
    Port = free_udp_port(),
    Dir = macula_test_tmp:dir(Prefix),
    {ok, Listener} = listener(Dir, Port),
    ok = macula_quic:async_accept(Listener),
    {ok, ClientConn} = macula_quic:connect(<<"127.0.0.1">>, Port, [{alpn, [<<"macula-dist-relay">>]}], ?EVENT_MS),
    ServerConn = receive {quic, new_conn, C, _Info} -> C after ?EVENT_MS -> error(no_server_connection) end,
    ok = macula_quic:async_accept_stream(ServerConn),
    {ok, ClientStream} = macula_quic:open_stream(ClientConn),
    ok = macula_quic:send(ClientStream, <<"open">>),
    ServerStream = receive {quic, new_stream, S, _Props} -> S after ?EVENT_MS -> error(no_server_stream) end,
    ok = read_opener(ServerStream),
    #{listener => Listener, client_conn => ClientConn, server_conn => ServerConn,
      client_stream => ClientStream, server_stream => ServerStream, dir => Dir,
      cert => filename:join(Dir, "tunnel.crt"), key => filename:join(Dir, "tunnel.key")}.

-spec close(map()) -> ok.
close(#{listener := Listener, client_conn := ClientConn, dir := Dir}) ->
    _ = macula_quic:close_connection(ClientConn),
    _ = macula_quic:close(Listener),
    _ = file:del_dir_r(Dir),
    ok.

%% The stream is opened with a byte, so the accepting side sees it at all. It is read off before anything else
%% starts, or a TLS session would find it at the head of its first record.
read_opener(Stream) ->
    ok = macula_quic:setopt(Stream, active, true),
    receive {quic, <<"open">>, Stream, _Flags} -> ok after ?EVENT_MS -> error(no_opener) end.

listener(Dir, Port) ->
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(macula_test_identity:tls_seed(), [<<"127.0.0.1">>]),
    Cert = filename:join(Dir, "tunnel.crt"),
    Key = filename:join(Dir, "tunnel.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    macula_quic:listen(<<"127.0.0.1">>, Port,
                       [{cert, Cert}, {key, Key}, {alpn, [<<"macula-dist-relay">>]}]).

free_udp_port() ->
    {ok, Socket} = gen_udp:open(0, [{ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Socket),
    ok = gen_udp:close(Socket),
    Port.
