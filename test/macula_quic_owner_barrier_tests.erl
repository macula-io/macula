%%%-------------------------------------------------------------------
%%% @doc Tests that a change of owner is a barrier for what the QUIC NIF
%%% delivers.
%%%
%%% A client writes numbered chunks on a stream without pause while the
%%% stream's owner on the listener side changes many times: each owner hands
%%% the stream to a new process with controlling_process/2 and then sends
%%% itself a marker. No data reaches an owner after its marker, and the bytes
%%% the owners received, owner by owner, are the bytes the client wrote, in
%%% order. Over several rounds, a listener closed with close_listener/1 while
%%% clients connect delivers no new_conn after close_listener/1 returns. Each
%%% scenario runs in a peer node of its own.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_owner_barrier_tests).

-include_lib("eunit/include/eunit.hrl").

%% Scenarios, run in a peer node.
-export([owners_while_data_flows/0,
         close_listener_while_clients_connect/0]).

-define(ALPN, <<"macula">>).
-define(CHUNK_BYTES, 1024).
-define(CHUNKS, 20_000).
%% Bytes an owner takes before it hands the stream on.
-define(BYTES_PER_OWNER, 40 * ?CHUNK_BYTES).
%% How long a former owner watches for data after its marker.
-define(WATCH_MS, 100).
-define(CLIENTS, 100).
-define(ROUNDS, 8).
%% Connections that arrive before a round closes its listener.
-define(CLOSE_AFTER, 5).
-define(AFTER_CLOSE_MS, 2_000).
-define(EVENT_TIMEOUT_MS, 10_000).
-define(SCENARIO_TIMEOUT_MS, 120_000).

owner_barrier_test_() ->
    [{"after controlling_process/2 returns the former owner gets no data, and every byte arrives in order",
      {timeout, 180, fun no_data_after_the_handoff/0}},
     {"after close_listener/1 returns no new_conn arrives",
      {timeout, 150, fun no_new_conn_after_close_listener/0}}].

no_data_after_the_handoff() ->
    ?assertEqual({ok, {no_late_bytes, in_order}}, in_peer(owners_while_data_flows, [])).

no_new_conn_after_close_listener() ->
    ?assertEqual({ok, {new_conn_after_close, 0}},
                 in_peer(close_listener_while_clients_connect, [])).

%%%===================================================================
%%% Scenarios
%%%===================================================================

owners_while_data_flows() ->
    #{client_stream := ClientStream, server_stream := ServerStream} = Pair = pair(),
    Scenario = self(),
    First = spawn(fun() -> owner(Scenario, ServerStream, 1) end),
    ok = macula_quic:controlling_process(ServerStream, First),
    First ! {owned, ServerStream},
    ok = macula_quic:setopt(ServerStream, active, true),
    _Writer = spawn(fun() -> write_chunks(ClientStream, 1) end),
    kept(Pair, verdict(owner_reports(#{}, undefined))).

close_listener_while_clients_connect() ->
    {new_conn_after_close, lists:sum([late_new_conns() || _ <- lists:seq(1, ?ROUNDS)])}.

%% One round: a fresh listener, CLIENTS clients dialling it at once, and the
%% listener closed once CLOSE_AFTER of their connections arrived. Returns
%% the new_conn messages that arrive after close_listener/1 returned. A late
%% one from an earlier round can only add to the count.
late_new_conns() ->
    Port = free_udp_port(),
    {PubBin, {ok, Listener}} = macula_test_tmp:with_dir("macula-quic-owner-barrier",
                                                        fun(Dir) -> listener_in(Dir, Port) end),
    ok = macula_quic:async_accept(Listener),
    Scenario = self(),
    _ = [spawn(fun() -> Scenario ! {dialled, dial(Port, PubBin)} end)
         || _ <- lists:seq(1, ?CLIENTS)],
    ok = new_conns(?CLOSE_AFTER),
    ok = macula_quic:close_listener(Listener),
    self() ! listener_closed,
    until_closed().

new_conns(0) ->
    ok;
new_conns(Count) ->
    receive
        {quic, new_conn, _Conn, _Info} -> new_conns(Count - 1)
    after ?EVENT_TIMEOUT_MS ->
        error({new_conns_missing, Count})
    end.

%%%===================================================================
%%% Owners
%%%===================================================================

%% An owner takes data until it has BYTES_PER_OWNER, hands the stream to a
%% new owner, and reports the bytes it got before its marker and how many
%% came after.
owner(Scenario, Stream, N) ->
    receive
        {owned, Stream} -> take(Scenario, Stream, N, <<>>)
    after ?EVENT_TIMEOUT_MS ->
        Scenario ! {owner, N, <<>>, 0, never_owned}
    end.

take(Scenario, Stream, N, Taken) when byte_size(Taken) >= ?BYTES_PER_OWNER ->
    Next = spawn(fun() -> owner(Scenario, Stream, N + 1) end),
    ok = macula_quic:controlling_process(Stream, Next),
    self() ! {handed, N},
    Next ! {owned, Stream},
    {BeforeMarker, Ended} = until_marker(Stream, N, Taken, handed),
    Scenario ! {owner, N, BeforeMarker, late_bytes(Stream, 0), Ended};
take(Scenario, Stream, N, Taken) ->
    receive
        {quic, Data, Stream, _Flags} when is_binary(Data) ->
            take(Scenario, Stream, N, <<Taken/binary, Data/binary>>);
        {quic, peer_send_shutdown, Stream, _} ->
            Scenario ! {owner, N, Taken, 0, finished}
    after ?EVENT_TIMEOUT_MS ->
        Scenario ! {owner, N, Taken, 0, {stalled, byte_size(Taken)}}
    end.

%% The messages that were in the mailbox when the marker was sent, in order.
until_marker(Stream, N, Taken, Ended) ->
    receive
        {handed, N} ->
            {Taken, Ended};
        {quic, Data, Stream, _Flags} when is_binary(Data) ->
            until_marker(Stream, N, <<Taken/binary, Data/binary>>, Ended);
        {quic, peer_send_shutdown, Stream, _} ->
            until_marker(Stream, N, Taken, finished)
    end.

%% Bytes that reach a former owner after its marker.
late_bytes(Stream, Late) ->
    receive
        {quic, Data, Stream, _Flags} when is_binary(Data) ->
            late_bytes(Stream, Late + byte_size(Data))
    after ?WATCH_MS ->
        Late
    end.

%% Reports from every owner up to the one that saw the stream end.
owner_reports(Reports, Last) when is_integer(Last), map_size(Reports) >= Last ->
    Reports;
owner_reports(Reports, Last) ->
    receive
        {owner, N, Bytes, Late, finished} ->
            owner_reports(Reports#{N => {Bytes, Late}}, N);
        {owner, N, Bytes, Late, _Status} ->
            owner_reports(Reports#{N => {Bytes, Late}}, Last)
    after ?EVENT_TIMEOUT_MS ->
        {incomplete, map_size(Reports), Last}
    end.

verdict({incomplete, _Reported, _Last} = Incomplete) ->
    Incomplete;
verdict(Reports) ->
    Ordered = [maps:get(N, Reports) || N <- lists:seq(1, map_size(Reports))],
    Received = iolist_to_binary([Bytes || {Bytes, _Late} <- Ordered]),
    Late = lists:sum([L || {_Bytes, L} <- Ordered]),
    {late(Late), order(Received =:= expected())}.

late(0) -> no_late_bytes;
late(Bytes) -> {late_bytes, Bytes}.

order(true) -> in_order;
order(false) -> out_of_order.

expected() ->
    iolist_to_binary([<<"open">> | [chunk(Seq) || Seq <- lists:seq(1, ?CHUNKS)]]).

write_chunks(Stream, Seq) when Seq > ?CHUNKS ->
    macula_quic:close_stream(Stream);
write_chunks(Stream, Seq) ->
    ok = macula_quic:send(Stream, chunk(Seq)),
    write_chunks(Stream, Seq + 1).

chunk(Seq) ->
    <<Seq:32, (binary:copy(<<(Seq rem 256)>>, ?CHUNK_BYTES - 4))/binary>>.

%%%===================================================================
%%% Listener
%%%===================================================================

until_closed() ->
    receive
        listener_closed -> new_conns_until(erlang:monotonic_time(millisecond) + ?AFTER_CLOSE_MS, 0);
        {quic, new_conn, _Conn, _Info} -> until_closed();
        {dialled, _Result} -> until_closed()
    end.

new_conns_until(Deadline, Count) ->
    Left = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {quic, new_conn, _Conn, _Info} -> new_conns_until(Deadline, Count + 1);
        {dialled, _Result} -> new_conns_until(Deadline, Count)
    after Left ->
        Count
    end.

dial(Port, PubBin) ->
    macula_quic:connect(<<"127.0.0.1">>, Port, [{verify_pubkey, PubBin}, {alpn, [?ALPN]}], 5_000).

%% A listener, a client connection to it, and one stream the client opened
%% and the listener side accepted, not yet active.
pair() ->
    Port = free_udp_port(),
    {PubBin, {ok, Listener}} = macula_test_tmp:with_dir("macula-quic-owner-barrier",
                                                        fun(Dir) -> listener_in(Dir, Port) end),
    ok = macula_quic:async_accept(Listener),
    {ok, ClientConn} = dial(Port, PubBin),
    ServerConn = receive {quic, new_conn, C, _Info} -> C after ?EVENT_TIMEOUT_MS -> error(no_server_connection) end,
    ok = macula_quic:async_accept_stream(ServerConn),
    {ok, ClientStream} = macula_quic:open_stream(ClientConn),
    %% The accepting side learns of the stream from its first bytes.
    ok = macula_quic:send(ClientStream, <<"open">>),
    ServerStream = receive {quic, new_stream, S, _Props} -> S after ?EVENT_TIMEOUT_MS -> error(no_server_stream) end,
    #{listener => Listener, client_conn => ClientConn, server_conn => ServerConn,
      client_stream => ClientStream, server_stream => ServerStream}.

%% A listener on Port, and the public key a client pins. The listener reads
%% its certificate and key files when it starts listening.
listener_in(Dir, Port) ->
    {PubBin, Cert, Key} = identity_files(Dir),
    {PubBin, macula_quic:listen(<<"127.0.0.1">>, Port, [{cert, Cert}, {key, Key}, {alpn, [?ALPN]}])}.

identity_files(Dir) ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    PubBin = iolist_to_binary(Pub),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(PubBin, iolist_to_binary(Priv), [<<"127.0.0.1">>]),
    Cert = filename:join(Dir, "listener.crt"),
    Key = filename:join(Dir, "listener.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    {PubBin, Cert, Key}.

%% Returns Result with the pair's handles referenced until now.
kept(#{listener := _, client_conn := _, server_conn := _, server_stream := _}, Result) ->
    Result.

free_udp_port() ->
    {ok, Sock} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Sock),
    ok = gen_udp:close(Sock),
    Port.

%%%===================================================================
%%% Peer node
%%%===================================================================

in_peer(Scenario, Args) ->
    Started = peer:start_link(#{connection => standard_io,
                                args => ["-pa" | code:get_path()]}),
    Peer = element(2, Started),
    OsPid = peer:call(Peer, os, getpid, [], 5_000),
    try peer:call(Peer, ?MODULE, Scenario, Args, ?SCENARIO_TIMEOUT_MS) of
        Result -> {ok, Result}
    catch
        Class:Reason -> {error, {Class, Reason}}
    after
        _ = os:cmd("kill -9 " ++ OsPid),
        try peer:stop(Peer) catch _:_ -> ok end
    end.
