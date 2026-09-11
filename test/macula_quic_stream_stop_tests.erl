%%%-------------------------------------------------------------------
%%% @doc Real two-endpoint coverage for `macula_quic:stop_stream/2'.
%%%
%%% A refused inbound stream is aborted in both directions with one
%%% application error code: `reset_stream/2' ends its send side and
%%% `stop_stream/2' its receive side, with a QUIC STOP_SENDING frame. Quinn
%%% sends STOP_SENDING with code 0 on its own when an unread receive side is
%%% dropped, so a code of the caller's choosing reaches the peer only when
%%% the stream is stopped explicitly. This drives Quinn against itself on
%%% loopback: the peer's writes fail with `{stopped, Code}', the stopped
%%% side can still write, and none of the peer's later data reaches it.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_stream_stop_tests).

-include_lib("eunit/include/eunit.hrl").

stop_test_() ->
    {timeout, 60,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun(Ctx) ->
          [{"stop_stream/2 makes the peer's writes fail with {stopped, Code}",
            fun() -> peer_writes_fail_with_the_code(Ctx) end},
           {"stop_stream/2 leaves the send side open and delivers no more data",
            fun() -> send_side_stays_open(Ctx) end},
           {"an out-of-range error code is rejected before touching the wire",
            fun() -> out_of_range_code_is_rejected(Ctx) end}]
      end}}.

%%%===================================================================
%%% Fixture
%%%===================================================================

setup() ->
    {Pub, Priv} = ephemeral_keypair(),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(
            Pub, Priv, [<<"localhost">>, <<"127.0.0.1">>]),
    Dir  = macula_test_tmp:dir("macula-quic-stop"),
    Cert = filename:join(Dir, "listener.crt"),
    Key  = filename:join(Dir, "listener.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key,  KeyPem),
    #{dir => Dir, cert => Cert, key => Key}.

cleanup(#{dir := Dir}) ->
    ok = file:del_dir_r(Dir),
    drain_quic_messages(),
    ok.

%%%===================================================================
%%% Test bodies
%%%===================================================================

peer_writes_fail_with_the_code(Ctx) ->
    {ClientStream, ServerStream, Cleanup} = stream_pair(Ctx),
    ok = macula_quic:stop_stream(ServerStream, 2),
    ?assertEqual({error, {stopped, 2}}, send_until_refused(ClientStream, 50)),
    receive
        {quic, send_failed, ClientStream, Reason} ->
            ?assertEqual({stopped, 2}, Reason)
    after 5_000 ->
        erlang:error(no_send_failed_after_stop)
    end,
    Cleanup().

send_side_stays_open(Ctx) ->
    {ClientStream, ServerStream, Cleanup} = stream_pair(Ctx),
    ok = macula_quic:stop_stream(ServerStream, 2),
    ok = macula_quic:send(ServerStream, <<"still writing">>),
    receive
        {quic, <<"still writing">>, ClientStream, _} -> ok
    after 5_000 ->
        erlang:error(no_data_after_stop)
    end,
    _ = macula_quic:send(ClientStream, <<"not read">>),
    receive
        {quic, <<"not read">>, ServerStream, _} ->
            erlang:error(data_delivered_after_stop)
    after 500 ->
        ok
    end,
    ok = macula_quic:close_stream(ServerStream),
    receive
        {quic, peer_send_shutdown, ClientStream, _} -> ok
    after 5_000 ->
        erlang:error(no_peer_send_shutdown_after_close)
    end,
    Cleanup().

out_of_range_code_is_rejected(Ctx) ->
    {_ClientStream, ServerStream, Cleanup} = stream_pair(Ctx),
    %% QUIC VarInt tops out at 2^62 - 1.
    ?assertEqual({error, error_code_out_of_range},
                 macula_quic:stop_stream(ServerStream, 1 bsl 62)),
    Cleanup().

%%%===================================================================
%%% Helpers
%%%===================================================================

%% Sends a chunk every 100 ms until a send fails, at most Tries times. The
%% peer learns of the stop a moment after it is made, so the first sends may
%% still be accepted.
send_until_refused(_Stream, 0) ->
    still_accepted;
send_until_refused(Stream, Tries) ->
    sent_or_refused(macula_quic:send(Stream, <<"after stop">>), Stream, Tries).

sent_or_refused(ok, Stream, Tries) ->
    timer:sleep(100),
    send_until_refused(Stream, Tries - 1);
sent_or_refused(Refused, _Stream, _Tries) ->
    Refused.

%% Loopback listener + client connection + one bidi stream opened by the
%% client and accepted server-side, both sides set active. Returns
%% `{ClientStream, ServerStream, Cleanup}'.
stream_pair(Ctx) ->
    {ok, ClientConn, ServerConn, ConnCleanup} = setup_loopback_pair(Ctx),
    ok = macula_quic:async_accept_stream(ServerConn),
    {ok, ClientStream} = macula_quic:open_stream(ClientConn),
    %% Quinn's open_bi/1 only allocates LOCAL stream state; the peer's
    %% accept_bi/1 fires once a STREAM frame with bytes crosses the wire.
    ok = macula_quic:setopt(ClientStream, active, true),
    ok = macula_quic:send(ClientStream, <<"prime">>),
    ServerStream = receive
        {quic, new_stream, S, _Props} -> S
    after 5_000 ->
        erlang:error(no_server_stream)
    end,
    ok = macula_quic:setopt(ServerStream, active, true),
    receive
        {quic, <<"prime">>, ServerStream, _} -> ok
    after 5_000 ->
        erlang:error(no_prime_data)
    end,
    Cleanup = fun() ->
        catch macula_quic:close_stream(ClientStream),
        catch macula_quic:close_stream(ServerStream),
        ConnCleanup()
    end,
    {ClientStream, ServerStream, Cleanup}.

setup_loopback_pair(#{cert := Cert, key := Key}) ->
    Port = pick_free_port(),
    {ok, Listener} = macula_quic:listen(<<"127.0.0.1">>, Port,
                                        [{cert, Cert}, {key, Key},
                                         {alpn, [<<"macula-net">>]},
                                         {idle_timeout_ms, 30000},
                                         {keep_alive_interval_ms, 5000}]),
    ok = macula_quic:async_accept(Listener),
    {ok, ClientConn} = macula_quic:connect(<<"127.0.0.1">>, Port,
                                            [{verify, none},
                                             {alpn, [<<"macula-net">>]},
                                             {idle_timeout_ms, 30000},
                                             {keep_alive_interval_ms, 5000}],
                                            5000),
    ServerConn = receive
        {quic, new_conn, C, _Info} -> C
    after 5000 ->
        error(no_server_conn)
    end,
    Cleanup = fun() ->
        try macula_quic:close_connection(ClientConn) catch _:_ -> ok end,
        try macula_quic:close_connection(ServerConn) catch _:_ -> ok end,
        try macula_quic:close_listener(Listener) catch _:_ -> ok end,
        drain_quic_messages()
    end,
    {ok, ClientConn, ServerConn, Cleanup}.

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

ephemeral_keypair() ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {iolist_to_binary(Pub), iolist_to_binary(Priv)}.
