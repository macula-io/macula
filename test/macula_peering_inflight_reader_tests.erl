%%%-------------------------------------------------------------------
%%% @doc The reserving reader for dedicated and content streams, driven over
%%% real loopback QUIC streams whose sender writes CALL frames.
%%%
%%% A reader owned by the process that owns its stream reserves each frame's
%%% bytes as they arrive and its decode transient before decode, delivers the
%%% frame with its reservation, and pauses its stream when the connection's
%%% stream share or the node has no room, resuming when there is room again.
%%% A header above the stream's frame cap, or a body that stalls for a window
%%% of reading time, ends the stream as malformed. A passive stream handed to
%%% another process loses nothing before that process starts its reader.
%%% When a connection's stream share has stayed full past the pause limit,
%%% streams whose reservations did not shrink during the pause are reset with
%%% REFUSED_BUSY, largest first, or the largest overall when every one of them
%%% made progress, until the share is below its watermark; each reset stream's
%%% owner is told, and its reader answers reset from then on. A healthy stream
%%% sharing the connection with slow ones still completes.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peering_inflight_reader_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("macula/include/macula_quic_error_codes.hrl").

-define(KIB, 1024).
-define(MIB, (1 bsl 20)).
-define(EVENT_MS, 5_000).
-define(SESSION_BUDGET, max_served_inbox_bytes_per_caller).
-define(SETTINGS, [inflight_node_bytes, inflight_connection_bytes, inflight_station_reserve_bytes,
                   inflight_max_reservation_age_ms, ?SESSION_BUDGET]).

reader_test_() ->
    {timeout, 300,
     {setup,
      fun() -> {ok, _} = application:ensure_all_started(macula), ok end,
      fun(ok) -> ok end,
      [{"a reader reserves each frame it delivers until the frame is handled",
        {timeout, 30, fun a_reader_reserves_each_frame_until_handled/0}},
       {"a reader at its stream share pauses its stream, and resumes once there is room",
        {timeout, 30, fun a_reader_at_its_share_pauses_until_there_is_room/0}},
       {"a header above the stream's frame cap ends the stream as frame_too_large",
        {timeout, 30, fun a_header_above_the_stream_frame_cap_is_refused/0}},
       {"a stream body that stalls for a window of reading time ends the stream as malformed",
        {timeout, 30, fun a_stalled_stream_body_is_malformed/0}},
       {"a passive stream handed to another process loses no data before that process reads",
        {timeout, 30, fun a_passive_stream_handed_over_loses_nothing/0}},
       {"a stream the share rule resets tells its owner, answers reset, and leaves the connection up",
        {timeout, 30, fun a_reset_stream_tells_its_owner/0}},
       {"the share rule picks streams without progress largest first, stops below the watermark, and never picks while below it",
        fun the_share_rule_picks_streams_in_order/0},
       {"a healthy stream completes while slow streams on its connection are reset, each once",
        {timeout, 60, fun a_healthy_stream_completes_beside_slow_ones/0}}]}}.

%%%===================================================================
%%% Reading and pausing
%%%===================================================================

a_reader_reserves_each_frame_until_handled() ->
    with_env([], fun() ->
        with_link(64 * ?MIB, #{}, fun(Link) ->
            Before = reserved(),
            Owner = open_stream(Link, hold, [frame(16 * ?KIB) || _ <- lists:seq(1, 3)]),
            3 = delivered_count(Owner, 3),
            Held = reserved(),
            Released = release_held(Owner),
            ?assertEqual({true, 3, Before}, {Held > Before, Released, reserved_settles_at(Before)})
        end)
    end).

%% The share holds one delivered frame and one decode, not two decodes: the
%% second frame waits until the first is released.
a_reader_at_its_share_pauses_until_there_is_room() ->
    with_env([], fun() ->
        {Limit, Frames} = share_for_one_held_frame(),
        with_link(Limit, #{}, fun(Link) ->
            Owner = open_stream(Link, hold, Frames),
            1 = delivered_count(Owner, 1),
            Early = delivered_count(Owner, 1, 500),
            1 = release_held(Owner),
            ?assertEqual({0, 1}, {Early, delivered_count(Owner, 1)})
        end)
    end).

a_header_above_the_stream_frame_cap_is_refused() ->
    with_env([], fun() ->
        with_link(2 * ?MIB, #{}, fun(#{conn := Conn} = Link) ->
            Cap = macula_peering_inflight:frame_cap(Conn, stream),
            Owner = open_stream_with(Link, hold, <<(Cap + 1):32/big>>),
            ?assertEqual({malformed, frame_too_large}, ended_how(Owner))
        end)
    end).

a_stalled_stream_body_is_malformed() ->
    with_env([], fun() ->
        with_link(64 * ?MIB, #{}, fun(Link) ->
            Owner = open_stream_with(Link, hold, <<(64 * ?KIB):32/big, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>,
                                     #{body_window_ms => 300, body_floor_bytes => 64 * ?KIB}),
            ?assertEqual({malformed, body_stalled}, ended_how(Owner))
        end)
    end).

%% The frames reach the stream before its accepting process hands it on, and
%% all of them arrive at the process that starts the reader.
a_passive_stream_handed_over_loses_nothing() ->
    with_env([], fun() ->
        with_link(64 * ?MIB, #{}, fun(#{client_conn := ClientConn} = Link) ->
            {ok, ClientStream} = macula_quic:open_stream(ClientConn),
            ok = macula_quic:send(ClientStream, iolist_to_binary([encode_signed(frame(4 * ?KIB)) || _ <- lists:seq(1, 5)])),
            ServerStream = accepted_stream(),
            timer:sleep(200),
            Owner = start_owner(Link, hold, ServerStream, #{}),
            Delivered = delivered_count(Owner, 5),
            LeftHere = receive {quic, _Bin, ServerStream, _} -> data_here after 0 -> nothing end,
            _ = release_held(Owner),
            ?assertEqual({5, nothing}, {Delivered, LeftHere})
        end)
    end).

%%%===================================================================
%%% The share rule
%%%===================================================================

%% One delivered frame held, the next paused at the share, a 500 ms pause
%% limit: the paused stream made no progress, so it is reset, its owner is
%% told, and the sender sees REFUSED_BUSY. A new stream still carries a frame.
a_reset_stream_tells_its_owner() ->
    with_env([], fun() ->
        {Limit, Frames} = share_for_one_held_frame(),
        with_link(Limit, #{pause_limit_ms => 500}, fun(Link) ->
            #{client_stream := ClientStream} = Opened = open_stream_client(Link, hold, Frames),
            Owner = maps:get(owner, Opened),
            ok = macula_quic:setopt(ClientStream, active, true),
            1 = delivered_count(Owner, 1),
            Told = reset_told(Owner),
            Resumed = reader_answer(Owner, resume),
            SenderSaw = receive {quic, stream_closed, ClientStream, Flags} -> Flags after ?EVENT_MS -> nothing end,
            Next = open_stream(Link, release, [frame(4 * ?KIB)]),
            ?assertEqual({no_progress, {reset, no_progress}, {reset, ?QUIC_CODE_REFUSED_BUSY}, 1},
                         {Told, Resumed, SenderSaw, delivered_count(Next, 1)})
        end)
    end).

the_share_rule_picks_streams_in_order() ->
    Pick = fun macula_peering_inflight:streams_to_reset/2,
    Paused = fun(Stream, Now, AtPause) -> #{stream => Stream, reserved => Now, reserved_at_pause => AtPause} end,
    NoProgressFirst = Pick([Paused(a, 4, 4), Paused(b, 8, 9), Paused(c, 2, 1)],
                           #{share_used => 14, watermark => 3}),
    LargestWhenAllProgressed = Pick([Paused(a, 4, 5), Paused(b, 8, 9)],
                                    #{share_used => 12, watermark => 3}),
    StopsAtWatermark = Pick([Paused(a, 4, 4), Paused(c, 2, 2)],
                            #{share_used => 6, watermark => 3}),
    NotBelowWatermark = Pick([Paused(a, 4, 4)], #{share_used => 2, watermark => 3}),
    ?assertEqual({[a, c], [b], [a], []},
                 {NoProgressFirst, LargestWhenAllProgressed, StopsAtWatermark, NotBelowWatermark}).

%% Three streams hold what they receive and keep sending; a fourth releases
%% every frame at once. The healthy stream gets all its frames, no slow stream
%% is reset twice, and once the slow owners let go nothing stays reserved.
a_healthy_stream_completes_beside_slow_ones() ->
    with_env([], fun() ->
        Frame = frame(16 * ?KIB),
        Wire = wire(Frame),
        Share = macula_peering_inflight:decode_transient_bytes(Wire) + 6 * Wire,
        with_link(Share * 4 div 3 + 4, #{pause_limit_ms => 400}, fun(Link) ->
            Before = reserved(),
            Slow = [open_stream(Link, hold, lists:duplicate(12, Frame)) || _ <- lists:seq(1, 3)],
            Healthy = open_stream(Link, release, lists:duplicate(20, Frame)),
            HealthyGot = delivered_count(Healthy, 20, 30_000),
            Resets = [length(resets_of(Owner)) || Owner <- Slow],
            lists:foreach(fun stop_owner/1, [Healthy | Slow]),
            ?assertEqual({20, true, true, Before},
                         {HealthyGot, lists:all(fun(N) -> N =< 1 end, Resets), lists:sum(Resets) >= 1,
                          reserved_settles_at(Before)})
        end)
    end).

%%%===================================================================
%%% Reader owners
%%%===================================================================

%% A process that owns one stream, runs its reader, and either holds every
%% frame it receives or releases it at once. It tells the test what it was
%% delivered and how its stream ended, and releases what it holds when its
%% stream is reset or when it is stopped.
start_owner(#{conn := Conn}, Behaviour, Stream, Opts) ->
    Test = self(),
    Owner = spawn(fun() -> owner_waiting(Test, Behaviour) end),
    ok = macula_quic:controlling_process(Stream, Owner),
    Owner ! {read, Stream, Conn, Opts},
    receive {reading, Owner} -> Owner after ?EVENT_MS -> error(owner_not_reading) end.

owner_waiting(Test, Behaviour) ->
    receive
        {read, Stream, Conn, Opts} ->
            Reader = macula_peering:reader_new(Stream, Conn, Opts),
            Test ! {reading, self()},
            owner_loop(#{test => Test, behaviour => Behaviour, stream => Stream,
                         reader => Reader, held => []})
    end.

owner_loop(#{stream := Stream, reader := Reader} = O) ->
    receive
        {quic, Bin, Stream, _Flags} when is_binary(Bin) ->
            owner_handled(macula_peering:reader_data(Reader, Bin), O);
        {macula_peering_inflight, resume, Stream} ->
            owner_handled(macula_peering:reader_resume(Reader), O);
        {macula_peering_inflight, reader_timer, Stream, _Kind} = Timer ->
            owner_handled(macula_peering:reader_timeout(Reader, Timer), O);
        {macula_peering_inflight, stream_reset, Stream, Reason} ->
            owner_reset(Reason, O);
        {answer, resume, From} ->
            From ! {answer, self(), macula_peering:reader_resume(Reader)},
            owner_loop(O);
        {release_held, From} ->
            From ! {released, self(), release_all(O)},
            owner_loop(O#{held := []});
        {stop, From} ->
            _ = release_all(O),
            From ! {stopped, self()}
    end.

owner_handled({Items, Reader}, #{test := Test, behaviour := Behaviour, held := Held} = O) ->
    Reservations = [R || {frame, _Frame, R} <- Items],
    Test ! {delivered, self(), length(Reservations)},
    owner_loop(O#{reader := Reader, held := kept(Behaviour, Reservations) ++ Held});
owner_handled({malformed, Reason, _Reader}, #{test := Test} = O) ->
    _ = release_all(O),
    Test ! {ended, self(), {malformed, Reason}},
    owner_done();
owner_handled({reset, Reason}, O) ->
    owner_reset(Reason, O).

owner_reset(Reason, #{test := Test, reader := Reader} = O) ->
    _ = release_all(O),
    Test ! {reset, self(), Reason},
    owner_after_reset(Reader).

%% After a reset the owner still answers, so a test can ask its reader.
owner_after_reset(Reader) ->
    receive
        {answer, resume, From} ->
            From ! {answer, self(), macula_peering:reader_resume(Reader)},
            owner_after_reset(Reader);
        {stop, From} ->
            From ! {stopped, self()};
        _Other ->
            owner_after_reset(Reader)
    end.

owner_done() ->
    receive
        {stop, From} -> From ! {stopped, self()};
        _Other -> owner_done()
    end.

kept(hold, Reservations) ->
    Reservations;
kept(release, Reservations) ->
    lists:foreach(fun release/1, Reservations),
    [].

release_all(#{held := Held}) ->
    lists:foreach(fun release/1, Held),
    length(Held).

release(Reservation) ->
    done = macula_peering:handle_reserved(Reservation, fun() -> done end).

%% How many frames Owner reports delivered until Count, or within Ms.
delivered_count(Owner, Count) ->
    delivered_count(Owner, Count, ?EVENT_MS).

delivered_count(Owner, Count, Ms) ->
    count_delivered(Owner, Count, 0, erlang:monotonic_time(millisecond) + Ms).

count_delivered(_Owner, Count, Got, _Deadline) when Got >= Count ->
    Got;
count_delivered(Owner, Count, Got, Deadline) ->
    Wait = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {delivered, Owner, N} -> count_delivered(Owner, Count, Got + N, Deadline)
    after Wait ->
        Got
    end.

release_held(Owner) ->
    Owner ! {release_held, self()},
    receive {released, Owner, N} -> N after ?EVENT_MS -> error(owner_did_not_release) end.

ended_how(Owner) ->
    receive {ended, Owner, How} -> How after ?EVENT_MS -> not_ended end.

reset_told(Owner) ->
    receive {reset, Owner, Reason} -> Reason after ?EVENT_MS -> not_reset end.

resets_of(Owner) ->
    receive {reset, Owner, Reason} -> [Reason | resets_of(Owner)] after 0 -> [] end.

reader_answer(Owner, resume) ->
    Owner ! {answer, resume, self()},
    receive {answer, Owner, Answer} -> Answer after ?EVENT_MS -> no_answer end.

stop_owner(Owner) ->
    Owner ! {stop, self()},
    receive {stopped, Owner} -> ok after ?EVENT_MS -> exit(Owner, kill) end.

%%%===================================================================
%%% Link
%%%===================================================================

%% Runs Fun with a loopback listener, a client connection to it, and a
%% stand-in connection process opened for in-flight counting with Limit and
%% ConnOpts, whose streams the listener side accepts.
with_link(Limit, ConnOpts, Fun) ->
    Conn = spawn(fun idle/0),
    ok = macula_peering_inflight:open_connection(Conn, maps:merge(#{connection_bytes => Limit,
                                                                    role => client},
                                                                  ConnOpts)),
    Port = free_udp_port(),
    {PubBin, {ok, Listener}} = macula_test_tmp:with_dir("macula-inflight-reader",
                                                        fun(Dir) -> listener(Dir, Port) end),
    ok = macula_quic:async_accept(Listener),
    {ok, ClientConn} = macula_quic:connect(<<"127.0.0.1">>, Port,
                                            [{verify_pubkey, PubBin}, {alpn, [<<"macula">>]}],
                                            ?EVENT_MS),
    ServerConn = receive {quic, new_conn, C, _Info} -> C after ?EVENT_MS -> error(no_server_connection) end,
    ok = macula_quic:async_accept_stream(ServerConn),
    try
        Fun(#{conn => Conn, client_conn => ClientConn, server_conn => ServerConn})
    after
        _ = (catch macula_quic:close_connection(ClientConn)),
        _ = (catch macula_quic:close_connection(ServerConn)),
        _ = (catch macula_quic:close_listener(Listener)),
        exit(Conn, kill),
        drain()
    end.

listener(Dir, Port) ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    PubBin = iolist_to_binary(Pub),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(PubBin, iolist_to_binary(Priv), [<<"127.0.0.1">>]),
    Cert = filename:join(Dir, "listener.crt"),
    Key = filename:join(Dir, "listener.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    {PubBin, macula_quic:listen(<<"127.0.0.1">>, Port,
                                [{cert, Cert}, {key, Key}, {alpn, [<<"macula">>]}])}.

%% Opens a client stream, writes Frames on it, and starts an owner of
%% Behaviour for the stream the listener side accepts.
open_stream(Link, Behaviour, Frames) ->
    maps:get(owner, open_stream_client(Link, Behaviour, Frames)).

open_stream_client(#{client_conn := ClientConn} = Link, Behaviour, Frames) ->
    {ok, ClientStream} = macula_quic:open_stream(ClientConn),
    ok = macula_quic:send(ClientStream, iolist_to_binary([encode_signed(F) || F <- Frames])),
    #{owner => start_owner(Link, Behaviour, accepted_stream(), #{}), client_stream => ClientStream}.

open_stream_with(Link, Behaviour, Bytes) ->
    open_stream_with(Link, Behaviour, Bytes, #{}).

open_stream_with(#{client_conn := ClientConn} = Link, Behaviour, Bytes, ReaderOpts) ->
    {ok, ClientStream} = macula_quic:open_stream(ClientConn),
    ok = macula_quic:send(ClientStream, Bytes),
    start_owner(Link, Behaviour, accepted_stream(), ReaderOpts).

accepted_stream() ->
    receive {quic, new_stream, S, _Props} -> S after ?EVENT_MS -> error(no_server_stream) end.

%% A connection limit whose stream share holds one delivered 16 KiB frame and
%% one decode, but not a second decode, and two such frames.
share_for_one_held_frame() ->
    Frames = [frame(16 * ?KIB), frame(16 * ?KIB)],
    Wire = wire(hd(Frames)),
    Share = 2 * Wire + macula_peering_inflight:decode_transient_bytes(Wire) - 1,
    {Share * 4 div 3, Frames}.

%%%===================================================================
%%% Frames and settings
%%%===================================================================

frame(PadBytes) ->
    macula_frame:call(#{
        call_id     => crypto:strong_rand_bytes(16),
        procedure   => <<"io.macula.test.inflight_reader">>,
        realm       => <<0:256>>,
        payload     => #{<<"pad">> => binary:copy(<<0>>, PadBytes)},
        deadline_ms => 60_000,
        caller      => <<1:256>>
    }).

encode_signed(Frame) ->
    macula_frame:encode(macula_frame:sign(Frame, macula_identity:generate())).

wire(Frame) ->
    byte_size(encode_signed(Frame)).

reserved() ->
    maps:get(reserved, macula_peering:inflight_usage()).

reserved_settles_at(Expected) ->
    settle(Expected, erlang:monotonic_time(millisecond) + ?EVENT_MS).

settle(Expected, Deadline) ->
    settled(reserved(), Expected, Deadline).

settled(Expected, Expected, _Deadline) ->
    Expected;
settled(Seen, Expected, Deadline) ->
    retry_settle(erlang:monotonic_time(millisecond) >= Deadline, Seen, Expected, Deadline).

retry_settle(true, Seen, _Expected, _Deadline) ->
    Seen;
retry_settle(false, _Seen, Expected, Deadline) ->
    timer:sleep(10),
    settle(Expected, Deadline).

%% Runs Fun with the in-flight settings unset except for Settings. The session
%% budget is 256 KiB unless Settings name it, so connections of a few MiB open.
with_env(Settings, Fun) ->
    lists:foreach(fun(Key) -> application:unset_env(macula, Key) end, ?SETTINGS),
    Applied = maps:to_list(maps:merge(#{?SESSION_BUDGET => 256 * ?KIB}, maps:from_list(Settings))),
    lists:foreach(fun({Key, Value}) -> application:set_env(macula, Key, Value) end, Applied),
    try
        Fun()
    after
        lists:foreach(fun(Key) -> application:unset_env(macula, Key) end, ?SETTINGS)
    end.

drain() ->
    receive
        {quic, _, _, _} -> drain();
        {delivered, _, _} -> drain();
        {reset, _, _} -> drain();
        {ended, _, _} -> drain()
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
