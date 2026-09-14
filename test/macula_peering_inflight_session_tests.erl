%%%-------------------------------------------------------------------
%%% @doc Reservations that travel beyond the reader: into a stream session's
%%% inbox, and out again on a relayed write.
%%%
%%% A chunk delivered to a macula_stream session with its reservation keeps
%%% the reservation while it waits in the inbox. It is released when recv
%%% takes the chunk, at once when a reader is already waiting for it, at once
%%% when the session has no room and ends with resource_exhausted, and when
%%% the session process ends with the chunk still queued. A frame relayed with
%%% async_send_on_stream/4, tagged with its reservation, stays reserved while
%%% its bytes wait in the send queue, and its holder releases it on the tag's
%%% send_complete or send_incomplete.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peering_inflight_session_tests).

-include_lib("eunit/include/eunit.hrl").

-define(KIB, 1024).
-define(MIB, (1 bsl 20)).
-define(WINDOW, 64 * ?KIB).
-define(EVENT_MS, 5_000).
-define(SESSION_BUDGET, max_served_inbox_bytes_per_caller).

session_test_() ->
    {timeout, 120,
     {setup,
      fun() -> {ok, _} = application:ensure_all_started(macula), ok end,
      fun(ok) -> ok end,
      [{"a queued chunk keeps its reservation until recv takes it",
        fun a_queued_chunk_is_released_when_recv_takes_it/0},
       {"a chunk for a reader already waiting is released at once",
        fun a_chunk_for_a_waiting_reader_is_released_at_once/0},
       {"a chunk the session has no room for is released when the session ends with resource_exhausted",
        fun a_refused_chunk_is_released_when_the_session_ends/0},
       {"chunks still queued are released when their session process ends",
        fun queued_chunks_are_released_when_their_session_process_ends/0},
       {"a relayed frame stays reserved until its bytes are written",
        {timeout, 30, fun a_relayed_frame_stays_reserved_until_written/0}}]}}.

%%%===================================================================
%%% Session inboxes
%%%===================================================================

a_queued_chunk_is_released_when_recv_takes_it() ->
    with_conn(fun(Conn) ->
        {Server, Client} = bounded_pair(4 * ?MIB),
        try
            Before = reserved(),
            Chunk = binary:copy(<<1>>, 64 * ?KIB),
            ok = macula_stream:deliver_chunk(Client, raw, Chunk, admitted(Conn, byte_size(Chunk))),
            Queued = eventually(fun reserved/0, fun(R) -> R =:= Before + byte_size(Chunk) end),
            {chunk, Chunk} = macula_stream:recv(Client, 1_000),
            ?assertEqual({Before + byte_size(Chunk), Before}, {Queued, reserved_settles_at(Before)})
        after
            end_streams([Server, Client])
        end
    end).

a_chunk_for_a_waiting_reader_is_released_at_once() ->
    with_conn(fun(Conn) ->
        {Server, Client} = bounded_pair(4 * ?MIB),
        try
            Before = reserved(),
            Test = self(),
            _Reader = spawn_link(fun() -> Test ! {read, macula_stream:recv(Client, ?EVENT_MS)} end),
            _ = eventually(fun() -> maps:get(waiters, macula_stream:info(Client)) end,
                           fun(W) -> W >= 1 end),
            ok = macula_stream:deliver_chunk(Client, raw, <<"chunk">>, admitted(Conn, ?KIB)),
            Read = receive {read, Result} -> Result after ?EVENT_MS -> no_read end,
            ?assertEqual({{chunk, <<"chunk">>}, Before}, {Read, reserved_settles_at(Before)})
        after
            end_streams([Server, Client])
        end
    end).

a_refused_chunk_is_released_when_the_session_ends() ->
    with_conn(fun(Conn) ->
        {Server, Client} = bounded_pair(1_000),
        try
            Before = reserved(),
            ok = macula_stream:deliver_chunk(Client, raw, binary:copy(<<1>>, 4_000), admitted(Conn, 4_000)),
            ?assertMatch({{told, {error, {<<"resource_exhausted">>, _}}}, Before},
                         {told(Client, ?EVENT_MS), reserved_settles_at(Before)})
        after
            end_streams([Server, Client])
        end
    end).

queued_chunks_are_released_when_their_session_process_ends() ->
    with_conn(fun(Conn) ->
        {Server, Client} = bounded_pair(4 * ?MIB),
        Before = reserved(),
        lists:foreach(fun(N) ->
                          Chunk = binary:copy(<<N>>, 16 * ?KIB),
                          ok = macula_stream:deliver_chunk(Client, raw, Chunk, admitted(Conn, byte_size(Chunk)))
                      end, [1, 2, 3]),
        Queued = eventually(fun reserved/0, fun(R) -> R =:= Before + 48 * ?KIB end),
        end_streams([Server, Client]),
        ?assertEqual({Before + 48 * ?KIB, Before}, {Queued, reserved_settles_at(Before)})
    end).

%%%===================================================================
%%% Relayed writes
%%%===================================================================

%% Eight 32 KiB frames relayed to a reader that does not read: those whose
%% bytes still wait in the send queue keep their reservations, which their
%% holder releases only on their tag notices, once the reader reads again.
a_relayed_frame_stays_reserved_until_written() ->
    with_conn(fun(Conn) ->
        with_stream_pair(fun(#{client_stream := Stream, server_stream := ServerStream}) ->
            Before = reserved(),
            Pending = [relayed(Conn, Stream, frame(32 * ?KIB)) || _ <- lists:seq(1, 8)],
            StillQueued = release_on_notices(Stream, Pending, 300),
            WhileStopped = reserved(),
            ok = macula_quic:setopt(ServerStream, active, true),
            [] = release_on_notices(Stream, StillQueued, ?EVENT_MS),
            ?assertEqual({true, true, Before},
                         {StillQueued =/= [], WhileStopped > Before, reserved_settles_at(Before)})
        end)
    end).

relayed(Conn, Stream, Frame) ->
    Reservation = admitted(Conn, byte_size(encode_signed(Frame))),
    ok = macula_peering:async_send_on_stream(Stream, Frame, macula_identity:generate(), Reservation),
    Reservation.

%% Releases each pending reservation when its tag notice arrives, for up to
%% Ms; returns those still pending.
release_on_notices(_Stream, [], _Ms) ->
    [];
release_on_notices(Stream, Pending, Ms) ->
    Deadline = erlang:monotonic_time(millisecond) + Ms,
    released_until(Stream, Pending, Deadline).

released_until(_Stream, [], _Deadline) ->
    [];
released_until(Stream, Pending, Deadline) ->
    Wait = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {quic, send_complete, Stream, R} ->
            release(R),
            released_until(Stream, lists:delete(R, Pending), Deadline);
        {quic, send_incomplete, Stream, {R, _Reason}} ->
            release(R),
            released_until(Stream, lists:delete(R, Pending), Deadline)
    after Wait ->
        Pending
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================

%% Runs Fun with a stand-in connection process opened for in-flight counting,
%% with a 256 KiB session budget so it opens.
with_conn(Fun) ->
    Old = application:get_env(macula, ?SESSION_BUDGET),
    ok = application:set_env(macula, ?SESSION_BUDGET, 256 * ?KIB),
    Conn = spawn(fun idle/0),
    try
        ok = macula_peering_inflight:open_connection(Conn, #{connection_bytes => 64 * ?MIB, role => client}),
        Fun(Conn)
    after
        exit(Conn, kill),
        restore_env(Old)
    end.

restore_env(undefined) -> application:unset_env(macula, ?SESSION_BUDGET);
restore_env({ok, Value}) -> application:set_env(macula, ?SESSION_BUDGET, Value).

admitted(Conn, Bytes) ->
    {ok, Reservation} = macula_peering_inflight:try_admit(Conn, stream, Bytes, stream_data),
    Reservation.

release(Reservation) ->
    done = macula_peering:handle_reserved(Reservation, fun() -> done end).

reserved() ->
    maps:get(reserved, macula_peering:inflight_usage()).

reserved_settles_at(Expected) ->
    eventually(fun reserved/0, fun(Seen) -> Seen =:= Expected end).

%% A server_stream pair, both owned by the test process, whose client stream
%% keeps at most Bytes no reader has taken.
bounded_pair(Bytes) ->
    Id = crypto:strong_rand_bytes(16),
    {ok, Client} = macula_stream:start_link(#{id => Id, role => client, mode => server_stream,
                                              owner => self(), max_inbox_bytes => Bytes}),
    {ok, Server} = macula_stream:start_link(#{id => Id, role => server, mode => server_stream,
                                              owner => self()}),
    ok = macula_stream:pair(Client, Server),
    {Server, Client}.

%% Ends the streams without taking the test process with them.
end_streams(Streams) ->
    lists:foreach(fun(S) -> true = unlink(S), exit(S, kill) end, Streams),
    drop_notices().

drop_notices() ->
    receive
        {macula_stream, ended, _Stream, _How} -> drop_notices()
    after 0 ->
        ok
    end.

told(Stream, Ms) ->
    receive
        {macula_stream, ended, Stream, How} -> {told, How}
    after Ms ->
        not_told
    end.

frame(PadBytes) ->
    macula_frame:call(#{
        call_id     => crypto:strong_rand_bytes(16),
        procedure   => <<"io.macula.test.inflight_session">>,
        realm       => <<0:256>>,
        payload     => #{<<"pad">> => binary:copy(<<0>>, PadBytes)},
        deadline_ms => 60_000,
        caller      => <<1:256>>
    }).

encode_signed(Frame) ->
    macula_frame:encode(macula_frame:sign(Frame, macula_identity:generate())).

%% Runs Fun with a loopback listener whose stream receive window is WINDOW, a
%% client connection to it, and one stream the client opened and this process
%% owns, whose listener side this process accepted and does not read.
with_stream_pair(Fun) ->
    Port = free_udp_port(),
    {PubBin, {ok, Listener}} = macula_test_tmp:with_dir("macula-inflight-session",
                                                        fun(Dir) -> windowed_listener(Dir, Port) end),
    ok = macula_quic:async_accept(Listener),
    {ok, ClientConn} = macula_quic:connect(<<"127.0.0.1">>, Port,
                                            [{verify_pubkey, PubBin}, {alpn, [<<"macula">>]}],
                                            ?EVENT_MS),
    ServerConn = receive {quic, new_conn, C, _Info} -> C after ?EVENT_MS -> error(no_server_connection) end,
    ok = macula_quic:async_accept_stream(ServerConn),
    {ok, ClientStream} = macula_quic:open_stream(ClientConn),
    ok = macula_quic:send(ClientStream, <<"open">>),
    ServerStream = receive {quic, new_stream, S, _Props} -> S after ?EVENT_MS -> error(no_server_stream) end,
    try
        Fun(#{client_stream => ClientStream, server_stream => ServerStream})
    after
        _ = (catch macula_quic:close_connection(ClientConn)),
        _ = (catch macula_quic:close_connection(ServerConn)),
        _ = (catch macula_quic:close_listener(Listener)),
        drain()
    end.

windowed_listener(Dir, Port) ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    PubBin = iolist_to_binary(Pub),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(PubBin, iolist_to_binary(Priv), [<<"127.0.0.1">>]),
    Cert = filename:join(Dir, "listener.crt"),
    Key = filename:join(Dir, "listener.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    {PubBin, macula_quic:listen(<<"127.0.0.1">>, Port,
                                [{cert, Cert}, {key, Key}, {alpn, [<<"macula">>]},
                                 {stream_receive_window, ?WINDOW},
                                 {receive_window, 4 * ?WINDOW}])}.

%% Fun's value once Pred accepts it, or its value after EVENT_MS.
eventually(Fun, Pred) ->
    eventually(Fun, Pred, erlang:monotonic_time(millisecond) + ?EVENT_MS).

eventually(Fun, Pred, Deadline) ->
    Value = Fun(),
    accepted(Pred(Value) orelse erlang:monotonic_time(millisecond) >= Deadline,
             Value, Fun, Pred, Deadline).

accepted(true, Value, _Fun, _Pred, _Deadline) ->
    Value;
accepted(false, _Value, Fun, Pred, Deadline) ->
    timer:sleep(10),
    eventually(Fun, Pred, Deadline).

drain() ->
    receive
        {quic, _, _, _} -> drain();
        {read, _} -> drain()
    after 50 ->
        ok
    end.

idle() ->
    receive stop -> ok end.

free_udp_port() ->
    {ok, Sock} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Sock),
    ok = gen_udp:close(Sock),
    Port.
