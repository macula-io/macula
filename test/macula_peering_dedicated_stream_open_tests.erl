%%%-------------------------------------------------------------------
%%% @doc Tests for opening a dedicated stream on a peering connection when
%%% the peer allows no more streams.
%%%
%%% The listener side allows the client its control stream and one
%%% dedicated stream, and the client has opened that one. An open started
%%% with async_open_dedicated_stream/1 then returns at once, the connection
%%% keeps answering while it waits, and the stream arrives as a message
%%% once the peer allows it. open_dedicated_stream/1 returns
%%% {error, timeout} when no stream is allowed within its bound, and no
%%% stream reaches the caller afterwards. A connection that ends reports a
%%% waiting open as failed.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peering_dedicated_stream_open_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FAST_MS, 50).
-define(OPEN_BOUND_MS, 10_000).
-define(EVENT_TIMEOUT_MS, 5_000).
-define(QUIET_MS, 500).

dedicated_stream_open_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Ctx) ->
         [{"an async open at the stream limit returns at once; the connection keeps answering",
           {timeout, 30, fun() -> async_open_returns_at_once(Ctx) end}},
          {"open_dedicated_stream/1 past its bound returns {error, timeout} and nothing later",
           {timeout, 40, fun() -> open_times_out_cleanly(Ctx) end}},
          {"a connection that ends reports a waiting open as failed",
           {timeout, 30, fun() -> ended_connection_fails_the_open(Ctx) end}}]
     end}.

setup() ->
    {ok, _} = application:ensure_all_started(macula),
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(
            iolist_to_binary(Pub), iolist_to_binary(Priv), [<<"localhost">>, <<"127.0.0.1">>]),
    Dir = macula_test_tmp:dir("macula-peering-stream-open"),
    Cert = filename:join(Dir, "listener.crt"),
    Key = filename:join(Dir, "listener.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    #{dir => Dir, cert => Cert, key => Key}.

cleanup(#{dir := Dir}) ->
    ok = file:del_dir_r(Dir).

%%====================================================================
%% Tests
%%====================================================================

async_open_returns_at_once(Ctx) ->
    #{client := Client} = Pair = pair_at_stream_limit(Ctx),
    try
        {Micros, Ref} = timer:tc(macula_peering, async_open_dedicated_stream, [Client]),
        Answer = answer_within(fun() -> macula_peering:peer_capabilities(Client) end, ?FAST_MS),
        Early = open_result(Ref, ?QUIET_MS),
        ok = free_the_dedicated_stream(Pair),
        Usable = usable(open_result(Ref, ?EVENT_TIMEOUT_MS), Pair),
        ?assert(Micros < ?FAST_MS * 1_000),
        ?assertMatch({answered, _}, Answer),
        ?assertEqual(none, Early),
        ?assertEqual(usable, Usable)
    after
        stop_pair(Pair)
    end.

open_times_out_cleanly(Ctx) ->
    #{client := Client} = Pair = pair_at_stream_limit(Ctx),
    try
        {Micros, Result} = timer:tc(fun() -> open_or_exit(Client) end),
        ok = free_the_dedicated_stream(Pair),
        Late = open_result('_', ?QUIET_MS),
        ?assertEqual({error, timeout}, Result),
        ?assert(Micros >= ?OPEN_BOUND_MS * 1_000),
        ?assertEqual(none, Late)
    after
        stop_pair(Pair)
    end.

ended_connection_fails_the_open(Ctx) ->
    #{client := Client} = Pair = pair_at_stream_limit(Ctx),
    try
        Ref = macula_peering:async_open_dedicated_stream(Client),
        ok = macula_peering:reject(Client, test_end),
        ?assertMatch({failed, _}, open_result(Ref, ?EVENT_TIMEOUT_MS))
    after
        stop_pair(Pair)
    end.

%%====================================================================
%% Helpers
%%====================================================================

%% A client and a server peering connection over a listener that allows the
%% client its control stream and one dedicated stream, with that dedicated
%% stream opened by the client and handed to this process on both sides.
pair_at_stream_limit(#{cert := Cert, key := Key}) ->
    Port = free_udp_port(),
    {ok, Listener} = macula_quic:listen(<<"127.0.0.1">>, Port,
                                        [{cert, Cert}, {key, Key}, {alpn, [<<"macula">>]},
                                         {peer_bidi_stream_count, 2}]),
    ok = macula_quic:async_accept(Listener),
    {ok, Client} = macula_peering:connect(#{
        identity        => macula_identity:generate(),
        realms          => [],
        capabilities    => 0,
        controlling_pid => self(),
        target          => #{host => "127.0.0.1", port => Port,
                             timeout_ms => 5_000, verify => none}
    }),
    ServerConn = receive
        {quic, new_conn, C, _Info} -> C
    after ?EVENT_TIMEOUT_MS ->
        error(no_inbound_conn)
    end,
    {ok, Server} = macula_peering:accept(ServerConn, #{
        identity        => macula_identity:generate(),
        realms          => [],
        capabilities    => 0,
        controlling_pid => self()
    }),
    ok = connected(Client),
    ok = connected(Server),
    {ok, ClientStream} = macula_peering:open_dedicated_stream(Client),
    %% The server side learns of the stream from its first bytes.
    ok = macula_quic:send(ClientStream, <<"open">>),
    ServerStream = receive
        {macula_peering, new_dedicated_stream, Server, S} -> S
    after ?EVENT_TIMEOUT_MS ->
        error(no_dedicated_stream)
    end,
    #{listener => Listener, client => Client, server => Server,
      client_stream => ClientStream, server_stream => ServerStream}.

connected(Pid) ->
    receive
        {macula_peering, connected, Pid, _PeerNodeId} -> ok
    after ?EVENT_TIMEOUT_MS ->
        error({not_connected, Pid})
    end.

%% Ends the pair's dedicated stream on both sides, so the server side
%% allows the client another stream.
free_the_dedicated_stream(#{client_stream := ClientStream, server_stream := ServerStream}) ->
    ok = macula_quic:close_stream(ClientStream),
    ok = read_to_end(ServerStream),
    macula_quic:close_stream(ServerStream).

read_to_end(Stream) ->
    receive
        {quic, peer_send_shutdown, Stream, _} -> ok;
        {quic, Data, Stream, _Flags} when is_binary(Data) -> read_to_end(Stream)
    after ?EVENT_TIMEOUT_MS ->
        error(stream_not_finished)
    end.

%% The result of the open with reference `Ref', or of any open when `Ref'
%% is '_'.
open_result('_', Timeout) ->
    receive
        {macula_peering, dedicated_stream_opened, _Ref, Stream} -> {opened, Stream};
        {macula_peering, dedicated_stream_open_failed, _Ref, Reason} -> {failed, Reason}
    after Timeout ->
        none
    end;
open_result(Ref, Timeout) ->
    receive
        {macula_peering, dedicated_stream_opened, Ref, Stream} -> {opened, Stream};
        {macula_peering, dedicated_stream_open_failed, Ref, Reason} -> {failed, Reason}
    after Timeout ->
        none
    end.

%% Whether an opened stream carries data to the server side.
usable({opened, Stream}, #{server := Server}) ->
    ok = macula_quic:send(Stream, <<"x">>),
    receive
        {macula_peering, new_dedicated_stream, Server, _ServerStream} -> usable
    after ?EVENT_TIMEOUT_MS ->
        not_accepted
    end;
usable(Other, _Pair) ->
    Other.

open_or_exit(Client) ->
    try
        macula_peering:open_dedicated_stream(Client)
    catch
        exit:Reason -> {exit, Reason}
    end.

%% Runs `Fun' in a separate process and returns its value if it comes
%% within `Ms'.
answer_within(Fun, Ms) ->
    Test = self(),
    Asker = spawn(fun() -> Test ! {answer, self(), Fun()} end),
    receive
        {answer, Asker, Value} -> {answered, Value}
    after Ms ->
        exit(Asker, kill),
        no_answer
    end.

stop_pair(#{listener := Listener, client := Client, server := Server}) ->
    _ = [try macula_peering:reject(Pid, test_cleanup) catch _:_ -> ok end
         || Pid <- [Client, Server]],
    _ = try macula_quic:close_listener(Listener) catch _:_ -> ok end,
    drain().

drain() ->
    receive
        {macula_peering, _, _, _} -> drain();
        {quic, _, _, _} -> drain()
    after 100 ->
        ok
    end.

free_udp_port() ->
    {ok, Sock} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Sock),
    ok = gen_udp:close(Sock),
    Port.
