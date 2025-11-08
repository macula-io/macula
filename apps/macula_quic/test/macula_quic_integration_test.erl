%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests for macula_protocol + macula_quic.
%%% Tests end-to-end message encoding, transport, and decoding.
%%% Tests written FIRST (TDD red phase).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_integration_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Basic QUIC Connection Tests
%%%===================================================================

%% Test: can start a QUIC listener
start_listener_test() ->
    %% Generate certs
    TempDir = "/tmp/macula_quic_integration",
    file:make_dir(TempDir),
    {ok, {CertFile, KeyFile}} = macula_quic_cert:generate_self_signed(TempDir),

    %% Start listener
    Port = 14567,
    ListenOpts = [
        {cert, CertFile},
        {key, KeyFile},
        {alpn, ["macula"]},
        {peer_unidi_stream_count, 3}
    ],

    Result = macula_quic:listen(Port, ListenOpts),
    ?assertMatch({ok, _ListenerPid}, Result),

    %% Cleanup
    {ok, ListenerPid} = Result,
    macula_quic:close(ListenerPid),
    file:delete(CertFile),
    file:delete(KeyFile),
    file:del_dir(TempDir).

%% Test: can connect to a QUIC listener
connect_to_listener_test() ->
    %% Generate certs
    TempDir = "/tmp/macula_quic_integration2",
    file:make_dir(TempDir),
    {ok, {CertFile, KeyFile}} = macula_quic_cert:generate_self_signed(TempDir),

    %% Start listener
    Port = 14568,
    ListenOpts = [
        {cert, CertFile},
        {key, KeyFile},
        {alpn, ["macula"]},
        {peer_unidi_stream_count, 3}
    ],
    {ok, ListenerPid} = macula_quic:listen(Port, ListenOpts),

    %% Connect client
    ConnectOpts = [
        {alpn, ["macula"]},
        {verify, none}  % Accept self-signed cert
    ],

    Result = macula_quic:connect("localhost", Port, ConnectOpts, 5000),
    ?assertMatch({ok, _ConnPid}, Result),

    %% Cleanup
    {ok, ConnPid} = Result,
    macula_quic:close(ConnPid),
    macula_quic:close(ListenerPid),
    file:delete(CertFile),
    file:delete(KeyFile),
    file:del_dir(TempDir).

%%%===================================================================
%%% Protocol + Transport Integration Tests
%%%===================================================================

%% Test: can send encoded ping message over QUIC
send_ping_over_quic_test() ->
    %% Generate certs
    TempDir = "/tmp/macula_quic_integration3",
    file:make_dir(TempDir),
    {ok, {CertFile, KeyFile}} = macula_quic_cert:generate_self_signed(TempDir),

    %% Start listener
    Port = 14569,
    ListenOpts = [
        {cert, CertFile},
        {key, KeyFile},
        {alpn, ["macula"]},
        {peer_unidi_stream_count, 3}
    ],
    {ok, ListenerPid} = macula_quic:listen(Port, ListenOpts),

    %% Connect client
    ConnectOpts = [
        {alpn, ["macula"]},
        {verify, none}
    ],
    {ok, ConnPid} = macula_quic:connect("localhost", Port, ConnectOpts, 5000),

    %% Encode ping message with macula_protocol
    PingMsg = #{timestamp => 123456789},
    Binary = macula_protocol_encoder:encode(ping, PingMsg),

    %% Open stream and send
    {ok, StreamPid} = macula_quic:open_stream(ConnPid),
    Result = macula_quic:send(StreamPid, Binary),
    ?assertEqual(ok, Result),

    %% Cleanup
    macula_quic:close(StreamPid),
    macula_quic:close(ConnPid),
    macula_quic:close(ListenerPid),
    file:delete(CertFile),
    file:delete(KeyFile),
    file:del_dir(TempDir).

%% Test: can receive and decode ping message over QUIC
receive_ping_over_quic_test() ->
    %% Generate certs
    TempDir = "/tmp/macula_quic_integration4",
    file:make_dir(TempDir),
    {ok, {CertFile, KeyFile}} = macula_quic_cert:generate_self_signed(TempDir),

    %% Start listener
    Port = 14570,
    ListenOpts = [
        {cert, CertFile},
        {key, KeyFile},
        {alpn, ["macula"]},
        {peer_unidi_stream_count, 3}
    ],
    {ok, ListenerPid} = macula_quic:listen(Port, ListenOpts),

    %% Spawn server to accept connection
    TestPid = self(),
    _ServerPid = spawn(fun() ->
        {ok, ConnPid} = macula_quic:accept(ListenerPid, 5000),
        {ok, StreamPid} = macula_quic:accept_stream(ConnPid, 5000),
        {ok, Binary} = macula_quic:recv(StreamPid, 5000),

        %% Decode message
        {ok, {ping, Msg}} = macula_protocol_decoder:decode(Binary),

        %% Send result back to test
        TestPid ! {received, Msg},

        macula_quic:close(StreamPid),
        macula_quic:close(ConnPid)
    end),

    %% Connect client
    ConnectOpts = [
        {alpn, ["macula"]},
        {verify, none}
    ],
    {ok, ConnPid} = macula_quic:connect("localhost", Port, ConnectOpts, 5000),

    %% Send ping
    PingMsg = #{timestamp => 987654321},
    Binary = macula_protocol_encoder:encode(ping, PingMsg),
    {ok, StreamPid} = macula_quic:open_stream(ConnPid),
    ok = macula_quic:send(StreamPid, Binary),

    %% Wait for server to receive and decode
    receive
        {received, DecodedMsg} ->
            ?assertEqual(987654321, maps:get(<<"timestamp">>, DecodedMsg))
    after 10000 ->
        ?assert(false, "Server did not receive message")
    end,

    %% Cleanup
    macula_quic:close(StreamPid),
    macula_quic:close(ConnPid),
    macula_quic:close(ListenerPid),
    file:delete(CertFile),
    file:delete(KeyFile),
    file:del_dir(TempDir).
