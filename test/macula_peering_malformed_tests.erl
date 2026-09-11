%%%-------------------------------------------------------------------
%%% @doc Tests for how macula_peering_conn ends a connection whose peer
%%% sends bytes that do not decode as frames.
%%%
%%% A server-role connection is driven by a raw QUIC peer on a loopback
%%% listener. During the handshake, before the peer has authenticated, and
%%% on the control stream once connected, a complete frame that is not CBOR
%%% ends the connection with the reason `{malformed, bad_frame}', and a
%%% length header above the frame cap ends it with
%%% `{malformed, frame_too_large}' as soon as the header arrives. The worker
%%% stops without waiting for more data or for the handshake timeout.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peering_malformed_tests).

-include_lib("eunit/include/eunit.hrl").

%% The frame cap in macula_frame, 16 MiB.
-define(MAX_FRAME_BYTES, 16#FFFFFF).
-define(NOT_CBOR, <<10:32/big, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>).
%% Well within the 30 s handshake timeout.
-define(PROMPT_MS, 3_000).
-define(EVENT_MS, 5_000).

malformed_test_() ->
    {timeout, 120,
     {setup, fun setup/0, fun cleanup/1,
      fun(Ctx) ->
          [{"a handshake frame that is not CBOR, followed by more data, ends the connection",
            {timeout, 30, fun() -> bad_handshake_frame(Ctx) end}},
           {"a handshake length header above the cap ends the connection from the header",
            {timeout, 30, fun() -> oversize_handshake_header(Ctx) end}},
           {"a control stream frame that is not CBOR, followed by more data, ends the connection",
            {timeout, 30, fun() -> bad_control_frame(Ctx) end}},
           {"a control stream length header above the cap ends the connection from the header",
            {timeout, 30, fun() -> oversize_control_header(Ctx) end}}]
      end}}.

%%%===================================================================
%%% Scenarios
%%%===================================================================

bad_handshake_frame(Ctx) ->
    with_server(Ctx, fun(Server, Stream) ->
        ok = macula_quic:send(Stream, ?NOT_CBOR),
        _ = macula_quic:send(Stream, binary:copy(<<0>>, 65_536)),
        ?assertEqual({malformed, bad_frame}, ended(Server))
    end).

oversize_handshake_header(Ctx) ->
    with_server(Ctx, fun(Server, Stream) ->
        ok = macula_quic:send(Stream, <<(?MAX_FRAME_BYTES + 1):32/big>>),
        ?assertEqual({malformed, frame_too_large}, ended(Server))
    end).

bad_control_frame(Ctx) ->
    with_server(Ctx, fun(Server, Stream) ->
        ok = macula_quic:send(Stream, signed_connect()),
        connected(Server),
        ok = macula_quic:send(Stream, ?NOT_CBOR),
        _ = macula_quic:send(Stream, binary:copy(<<0>>, 65_536)),
        ?assertEqual({malformed, bad_frame}, ended(Server))
    end).

oversize_control_header(Ctx) ->
    with_server(Ctx, fun(Server, Stream) ->
        ok = macula_quic:send(Stream, signed_connect()),
        connected(Server),
        ok = macula_quic:send(Stream, <<(?MAX_FRAME_BYTES + 1):32/big>>),
        ?assertEqual({malformed, frame_too_large}, ended(Server))
    end).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup() ->
    {ok, _} = application:ensure_all_started(macula),
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(
            iolist_to_binary(Pub), iolist_to_binary(Priv), [<<"localhost">>, <<"127.0.0.1">>]),
    #{cert_pem => CertPem, key_pem => KeyPem}.

cleanup(_Ctx) ->
    ok.

%% Runs Scenario with a server-role connection accepted from a raw QUIC
%% peer, and that peer's open stream. The listener is made in the test
%% process, which owns it and receives its connections.
with_server(#{cert_pem := CertPem, key_pem := KeyPem}, Scenario) ->
    macula_test_tmp:with_dir("macula-peering-malformed", fun(Dir) ->
        Cert = filename:join(Dir, "listener.crt"),
        Key = filename:join(Dir, "listener.key"),
        ok = file:write_file(Cert, CertPem),
        ok = file:write_file(Key, KeyPem),
        Port = free_udp_port(),
        {ok, Listener} = macula_quic:listen(<<"127.0.0.1">>, Port,
                                            [{cert, Cert}, {key, Key},
                                             {alpn, [<<"macula">>]},
                                             {idle_timeout_ms, 30_000},
                                             {keep_alive_interval_ms, 5_000}]),
        ok = macula_quic:async_accept(Listener),
        {ok, PeerConn} = macula_quic:connect("127.0.0.1", Port,
                                             [{alpn, [<<"macula">>]}, {verify, none}],
                                             ?EVENT_MS),
        {ok, Server} = macula_peering:accept(inbound_conn(), server_opts()),
        {ok, Stream} = macula_quic:open_stream(PeerConn),
        try
            Scenario(Server, Stream)
        after
            _ = (catch macula_peering:close(Server, test_cleanup)),
            _ = (catch macula_quic:close(PeerConn)),
            _ = (catch macula_quic:close_listener(Listener)),
            drain()
        end
    end).

inbound_conn() ->
    receive
        {quic, new_conn, Conn, _Info} -> Conn
    after ?EVENT_MS ->
        erlang:error(no_inbound_conn)
    end.

server_opts() ->
    #{identity        => macula_identity:generate(),
      realms          => [],
      capabilities    => 0,
      controlling_pid => self()}.

%% The CONNECT a peer sends to open the handshake.
signed_connect() ->
    Kp = macula_identity:generate(),
    Pub = macula_identity:public(Kp),
    Connect = macula_frame:connect(#{
        node_id         => Pub,
        station_id      => Pub,
        realms          => [],
        capabilities    => 0,
        puzzle_evidence => macula_identity:puzzle_evidence(Pub)
    }),
    macula_frame:encode(macula_frame:sign(Connect, Kp)).

connected(Server) ->
    receive
        {macula_peering, connected, Server, _PeerNodeId} -> ok
    after ?EVENT_MS ->
        erlang:error(not_connected)
    end.

%% The reason Server gave for ending the connection, once it has also
%% stopped; or what it did instead within PROMPT_MS.
ended(Server) ->
    Mon = erlang:monitor(process, Server),
    receive
        {macula_peering, disconnected, Server, Reason} -> stopped(Mon, Server, Reason)
    after ?PROMPT_MS ->
        no_disconnect
    end.

stopped(Mon, Server, Reason) ->
    receive
        {'DOWN', Mon, process, Server, _Exit} -> Reason
    after ?PROMPT_MS ->
        {still_running, Reason}
    end.

drain() ->
    receive
        {quic, _, _, _} -> drain();
        {macula_peering, _, _, _} -> drain()
    after 0 ->
        ok
    end.

free_udp_port() ->
    {ok, Sock} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Sock),
    ok = gen_udp:close(Sock),
    Port.
