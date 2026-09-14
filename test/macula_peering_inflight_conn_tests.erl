%%%-------------------------------------------------------------------
%%% @doc The in-flight bound on a connection's control stream, driven by a
%%% raw QUIC peer on a loopback listener against a server-role connection.
%%%
%%% A connection reserves the bytes it has received, not the size a length
%%% header claims, and reserves a frame's decode transient just before it
%%% decodes the frame; the reservation then settles to what the frame keeps.
%%% A length header above the largest frame that fits with its decode
%%% transient ends the connection as frame_too_large at once. While a frame
%%% body is incomplete it has to receive at least the floor in every window
%%% of reading time, or the connection closes as malformed, and time the
%%% connection spends paused by the limits does not count. A connection that
%%% does not fit its limit or the node limit reads nothing more until there is
%%% room. A client link's control stream paused past the pause limit closes
%%% with REFUSED_BUSY; a station link's control stream is never closed as
%%% busy, a pause at the ceiling is counted, and a control frame of at most
%%% 4 KiB at its head is still read at the ceiling, from the station reserve.
%%% set_role/2 after the handshake makes a link a station link. A frame the
%%% connection drops as invalid is released when it is reported. Without
%%% inflight opts, frames arrive as before and nothing is reserved.
%%%
%%% Frame sizes and limits are derived from decode_transient_bytes/1, since a
%%% frame's reservation before decode includes its transient.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peering_inflight_conn_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("macula/include/macula_quic_error_codes.hrl").

-define(KIB, 1024).
-define(MIB, (1 bsl 20)).
-define(EVENT_MS, 5_000).
-define(WINDOW_MS, 500).
-define(PAUSE_LIMIT_MS, 1_000).
-define(LIVENESS_MS, 500).
-define(SESSION_BUDGET, max_served_inbox_bytes_per_caller).
-define(SETTINGS, [inflight_node_bytes, inflight_connection_bytes, inflight_station_reserve_bytes,
                   inflight_max_reservation_age_ms, ?SESSION_BUDGET]).

conn_test_() ->
    {timeout, 420,
     {setup, fun setup/0, fun cleanup/1,
      fun(Ctx) ->
          [{"a large length header then silence reserves no more than the bytes sent",
            {timeout, 30, fun() -> header_then_silence_reserves_only_the_bytes_sent(Ctx) end}},
           {"a length header above the largest frame that fits ends the connection as frame_too_large",
            {timeout, 30, fun() -> header_over_the_frame_cap_is_refused(Ctx) end}},
           {"a body that receives nothing for a window of reading time closes as malformed, small claim",
            {timeout, 30, fun() -> stalled_body_closes(Ctx, ?KIB) end}},
           {"a body that receives nothing for a window of reading time closes as malformed, large claim",
            {timeout, 30, fun() -> stalled_body_closes(Ctx, 8 * ?MIB) end}},
           {"a body arriving steadily above the floor completes",
            {timeout, 30, fun() -> steady_body_completes(Ctx) end}},
           {"a pause longer than the window does not count toward it",
            {timeout, 30, fun() -> pause_does_not_count_toward_the_window(Ctx) end}},
           {"the decode transient is reserved before decode, and the reservation settles after it",
            {timeout, 30, fun() -> decode_transient_is_reserved_before_decode(Ctx) end}},
           {"a connection over its byte limit reads nothing more until a release",
            {timeout, 30, fun() -> connection_limit_holds_until_release(Ctx) end}},
           {"the node limit bounds reserved bytes across connections",
            {timeout, 30, fun() -> node_limit_holds_across_connections(Ctx) end}},
           {"a client link's control stream paused past the pause limit closes with REFUSED_BUSY",
            {timeout, 30, fun() -> paused_past_the_limit_closes_busy(Ctx) end}},
           {"a station link's control stream is never closed as busy, and a pause at the ceiling is counted",
            {timeout, 30, fun() -> station_control_stream_is_not_closed_busy(Ctx) end}},
           {"a small control frame at the head of a station link is read within 500 ms at the ceiling",
            {timeout, 30, fun() -> small_frame_is_read_at_the_ceiling(Ctx) end}},
           {"set_role/2 after the handshake lets a paused link read up to the ceiling",
            {timeout, 30, fun() -> set_role_after_the_handshake_uses_the_ceiling(Ctx) end}},
           {"an invalid frame is released when the connection reports it",
            {timeout, 30, fun() -> invalid_frame_is_released(Ctx) end}},
           {"without inflight opts, frames arrive in the current shapes and nothing is reserved",
            {timeout, 30, fun() -> without_inflight_frames_keep_their_shapes(Ctx) end}}]
      end}}.

%%%===================================================================
%%% Reserving what was delivered
%%%===================================================================

header_then_silence_reserves_only_the_bytes_sent(Ctx) ->
    with_env([], fun() ->
        with_raw_peer(Ctx, inflight(64 * ?MIB), fun(#{stream := Stream}) ->
            Before = reserved(),
            Sent = <<(8 * ?MIB):32/big, (binary:copy(<<0>>, ?KIB))/binary>>,
            ok = macula_quic:send(Stream, Sent),
            Held = eventually(fun reserved/0, fun(R) -> R > Before end),
            ?assertEqual({true, true}, {Held > Before, Held - Before =< byte_size(Sent)})
        end)
    end).

%% A 2 MiB connection reads control frames only as large as fit in 2 MiB with
%% their decode transient; a header one byte larger ends it at once.
header_over_the_frame_cap_is_refused(Ctx) ->
    with_env([], fun() ->
        with_raw_peer(Ctx, inflight(2 * ?MIB), fun(#{server := Server, stream := Stream}) ->
            Cap = eventually(fun() -> macula_peering_inflight:frame_cap(Server, control) end,
                             fun(C) -> is_integer(C) end),
            ok = macula_quic:send(Stream, <<(Cap + 1):32/big>>),
            ?assertEqual({malformed, frame_too_large}, ended(Server, ?WINDOW_MS))
        end)
    end).

stalled_body_closes(Ctx, Claim) ->
    with_env([], fun() ->
        with_raw_peer(Ctx, inflight(64 * ?MIB), fun(#{server := Server, stream := Stream}) ->
            Started = erlang:monotonic_time(millisecond),
            ok = macula_quic:send(Stream, <<Claim:32/big, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>),
            Reason = ended(Server, 4 * ?WINDOW_MS),
            Elapsed = erlang:monotonic_time(millisecond) - Started,
            ?assertEqual({{malformed, body_stalled}, true}, {Reason, Elapsed >= ?WINDOW_MS})
        end)
    end).

%% A 1 MiB frame in 64 KiB slices every quarter window: twice the floor rate.
steady_body_completes(Ctx) ->
    with_env([], fun() ->
        with_raw_peer(Ctx, inflight(64 * ?MIB), fun(#{server := Server, stream := Stream}) ->
            Frame = padded_call(?MIB),
            ok = send_paced(Stream, encode_signed(Frame), 64 * ?KIB, ?WINDOW_MS div 4),
            release(reserved_call(Server, Frame)),
            ?assert(is_process_alive(Server))
        end)
    end).

%% A blocker leaves the node room for only part of a 1 MiB body, so the
%% connection pauses mid-body for three windows, and still serves the frame
%% once the blocker is released.
pause_does_not_count_toward_the_window(Ctx) ->
    with_env([], fun() ->
        with_blocker(64 * ?MIB - 600 * ?KIB, fun(Blocker) ->
            Opts = inflight(64 * ?MIB, #{pause_limit_ms => 10 * ?WINDOW_MS}),
            with_raw_peer(Ctx, Opts, fun(#{server := Server, stream := Stream}) ->
                Frame = padded_call(?MIB),
                ok = macula_quic:send(Stream, encode_signed(Frame)),
                timer:sleep(3 * ?WINDOW_MS),
                Alive = is_process_alive(Server),
                Early = frame_within(Server, 0),
                release(Blocker),
                release(reserved_call(Server, Frame)),
                ?assertEqual({true, none}, {Alive, Early})
            end)
        end)
    end).

%% The node has room for the frame's wire bytes but not for its decode
%% transient while a blocker holds part of the limit, so the frame is not
%% delivered until the blocker is released. Once delivered and held, its
%% reservation is at least its wire bytes and less than wire plus transient.
decode_transient_is_reserved_before_decode(Ctx) ->
    Frame = many_items_call(100_000),
    Wire = byte_size(encode_signed(Frame)),
    Transient = macula_peering_inflight:decode_transient_bytes(Wire),
    with_env([{inflight_node_bytes, Wire + Transient + ?MIB}], fun() ->
        with_blocker(Transient div 2 + ?MIB, fun(Blocker) ->
            Opts = inflight(64 * ?MIB, #{pause_limit_ms => 10 * ?WINDOW_MS}),
            with_raw_peer(Ctx, Opts, fun(#{server := Server, stream := Stream}) ->
                Before = reserved() - (Transient div 2 + ?MIB),
                ok = macula_quic:send(Stream, encode_signed(Frame)),
                Early = frame_within(Server, ?WINDOW_MS),
                release(Blocker),
                Held = reserved_call(Server, Frame),
                Settled = reserved() - Before,
                release(Held),
                ?assertEqual({none, true, true}, {Early, Settled >= Wire, Settled < Wire + Transient})
            end)
        end)
    end).

%%%===================================================================
%%% Limits and pauses
%%%===================================================================

%% With a limit of 2W + T - 1 for frames of W wire bytes and T transient, the
%% first frame fits and, while it is held, the second cannot be decoded. The
%% second is sent once the first arrived, since bytes are reserved as they
%% come and a second frame already buffered would leave the first no room.
connection_limit_holds_until_release(Ctx) ->
    {First, Second, Limit} = two_frames_one_room(),
    with_env([], fun() ->
        Opts = inflight(Limit, #{pause_limit_ms => 10 * ?WINDOW_MS}),
        with_raw_peer(Ctx, Opts, fun(#{server := Server, stream := Stream}) ->
            ok = macula_quic:send(Stream, encode_signed(First)),
            Held = reserved_call(Server, First),
            ok = macula_quic:send(Stream, encode_signed(Second)),
            Early = frame_within(Server, ?WINDOW_MS),
            release(Held),
            release(reserved_call(Server, Second)),
            ?assertEqual(none, Early)
        end)
    end).

node_limit_holds_across_connections(Ctx) ->
    {AtA, AtB, Limit} = two_frames_one_room(),
    with_env([{inflight_node_bytes, Limit}], fun() ->
        Opts = inflight(64 * ?MIB, #{pause_limit_ms => 10 * ?WINDOW_MS}),
        with_raw_peer(Ctx, Opts, fun(#{server := ServerA, stream := StreamA}) ->
            with_raw_peer(Ctx, Opts, fun(#{server := ServerB, stream := StreamB}) ->
                ok = macula_quic:send(StreamA, encode_signed(AtA)),
                HeldA = reserved_call(ServerA, AtA),
                ok = macula_quic:send(StreamB, encode_signed(AtB)),
                Early = frame_within(ServerB, ?WINDOW_MS),
                release(HeldA),
                release(reserved_call(ServerB, AtB)),
                ?assertEqual(none, Early)
            end)
        end)
    end).

paused_past_the_limit_closes_busy(Ctx) ->
    {First, Second, Limit} = two_frames_one_room(),
    with_env([], fun() ->
        Opts = inflight(Limit, #{pause_limit_ms => ?PAUSE_LIMIT_MS,
                                 body_window_ms => 10 * ?PAUSE_LIMIT_MS}),
        with_raw_peer(Ctx, Opts, fun(#{server := Server, stream := Stream, peer_conn := PeerConn}) ->
            ok = macula_quic:send(Stream, encode_signed(First)),
            Held = reserved_call(Server, First),
            ok = macula_quic:send(Stream, encode_signed(Second)),
            Reason = ended(Server, 4 * ?PAUSE_LIMIT_MS),
            release(Held),
            PeerSees = eventually(fun() -> macula_quic:close_reason(PeerConn) end,
                                  fun(R) -> R =/= open end),
            ?assertMatch({busy, {application_closed, ?QUIC_CODE_REFUSED_BUSY, _}},
                         {Reason, PeerSees})
        end)
    end).

%% A node limit whose ceiling is 2W + T - 1: on a station link the first
%% frame fits, and the second pauses at the ceiling for three pause limits
%% without closing the link.
station_control_stream_is_not_closed_busy(Ctx) ->
    {First, Second, Room} = two_frames_one_room(),
    with_env([{inflight_node_bytes, Room * 4 div 5}], fun() ->
        Opts = inflight(64 * ?MIB, #{role => station, pause_limit_ms => ?PAUSE_LIMIT_MS,
                                     body_window_ms => 10 * ?PAUSE_LIMIT_MS}),
        with_raw_peer(Ctx, Opts, fun(#{server := Server, stream := Stream}) ->
            Before = usage_of(ceiling_pauses),
            ok = macula_quic:send(Stream, encode_signed(First)),
            Held = reserved_call(Server, First),
            ok = macula_quic:send(Stream, encode_signed(Second)),
            timer:sleep(3 * ?PAUSE_LIMIT_MS),
            Alive = is_process_alive(Server),
            Counted = usage_of(ceiling_pauses) - Before,
            release(Held),
            release(reserved_call(Server, Second)),
            ?assertEqual({true, true}, {Alive, Counted >= 1})
        end)
    end).

%% A blocker holds the node at its ceiling; a PING at the head of a station
%% link's control stream is still read and delivered within 500 ms, from the
%% station reserve.
small_frame_is_read_at_the_ceiling(Ctx) ->
    NodeLimit = 8 * ?MIB,
    with_env([{inflight_node_bytes, NodeLimit}, {inflight_station_reserve_bytes, 4 * ?MIB}], fun() ->
        with_blocker(NodeLimit * 5 div 4, station, fun(_Blocker) ->
            Opts = inflight(64 * ?MIB, #{role => station}),
            with_raw_peer(Ctx, Opts, fun(#{server := Server, stream := Stream}) ->
                Ping = macula_frame:ping(#{nonce => crypto:strong_rand_bytes(16)}),
                Sent = erlang:monotonic_time(millisecond),
                ok = macula_quic:send(Stream, encode_signed(Ping)),
                Delivered = reserved_of_type(Server, ping, ?LIVENESS_MS),
                Elapsed = erlang:monotonic_time(millisecond) - Sent,
                release_delivered(Delivered),
                ?assertMatch({{ok, _}, true}, {Delivered, Elapsed =< ?LIVENESS_MS})
            end)
        end)
    end).

%% A blocker holds the node limit but not the ceiling. A client link's frame
%% waits; once the node names the link a station link, the frame is read.
set_role_after_the_handshake_uses_the_ceiling(Ctx) ->
    NodeLimit = 32 * ?MIB,
    with_env([{inflight_node_bytes, NodeLimit}], fun() ->
        with_blocker(NodeLimit, fun(_Blocker) ->
            Opts = inflight(64 * ?MIB, #{pause_limit_ms => 10 * ?WINDOW_MS}),
            with_raw_peer(Ctx, Opts, fun(#{server := Server, stream := Stream}) ->
                Frame = padded_call(?KIB),
                ok = macula_quic:send(Stream, encode_signed(Frame)),
                Early = frame_within(Server, ?WINDOW_MS),
                ok = macula_peering:set_role(Server, station),
                release(reserved_call(Server, Frame)),
                ?assertEqual(none, Early)
            end)
        end)
    end).

%%%===================================================================
%%% Invalid frames and compatibility
%%%===================================================================

invalid_frame_is_released(Ctx) ->
    with_env([], fun() ->
        with_raw_peer(Ctx, inflight(64 * ?MIB), fun(#{server := Server, stream := Stream}) ->
            Before = reserved(),
            ok = macula_quic:send(Stream, encode_signed(maps:remove(caller, padded_call(?MIB)))),
            Reported = invalid_reported(Server),
            ?assertEqual({{invalid_frame, call, caller}, Before},
                         {Reported, eventually(fun reserved/0, fun(R) -> R =:= Before end)})
        end)
    end).

without_inflight_frames_keep_their_shapes(Ctx) ->
    with_env([], fun() ->
        with_raw_peer(Ctx, #{}, fun(#{server := Server, stream := Stream}) ->
            Before = reserved(),
            #{call_id := CallId} = Frame = padded_call(?KIB),
            ok = macula_quic:send(Stream, encode_signed(Frame)),
            Delivered = receive
                {macula_peering, frame, Server, #{frame_type := call, call_id := CallId}} -> current;
                {macula_peering, reserved_frame, Server, _Frame, _Reservation} -> reserved
            after ?EVENT_MS ->
                none
            end,
            ?assertEqual({current, Before}, {Delivered, reserved()})
        end)
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

%% Runs Scenario with a server-role connection started with ExtraOpts, after
%% a raw QUIC peer has sent it a signed CONNECT and it is connected. Scenario
%% gets the server pid, the peer's open control stream and its connection.
with_raw_peer(#{cert_pem := CertPem, key_pem := KeyPem}, ExtraOpts, Scenario) ->
    macula_test_tmp:with_dir("macula-peering-inflight", fun(Dir) ->
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
        {ok, Server} = macula_peering:accept(inbound_conn(), maps:merge(server_opts(), ExtraOpts)),
        {ok, Stream} = macula_quic:open_stream(PeerConn),
        ok = macula_quic:send(Stream, signed_connect()),
        try
            ok = connected(Server),
            Scenario(#{server => Server, stream => Stream, peer_conn => PeerConn})
        after
            _ = (catch macula_peering:close(Server, test_cleanup)),
            _ = (catch macula_quic:close(PeerConn)),
            _ = (catch macula_quic:close_listener(Listener)),
            drain()
        end
    end).

inflight(Limit) ->
    inflight(Limit, #{}).

inflight(Limit, Overrides) ->
    #{inflight => maps:merge(#{connection_bytes => Limit,
                               role => client,
                               pause_limit_ms => 10 * ?WINDOW_MS,
                               body_window_ms => ?WINDOW_MS,
                               body_floor_bytes => 64 * ?KIB},
                             Overrides)}.

%% Two 1 MiB CALLs, and the room that holds one of them decoded and held but
%% not the other while it decodes: 2W + T - 1 for W wire bytes and T transient.
two_frames_one_room() ->
    First = padded_call(?MIB),
    Second = padded_call(?MIB),
    Wire = byte_size(encode_signed(First)),
    {First, Second, 2 * Wire + macula_peering_inflight:decode_transient_bytes(Wire) - 1}.

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

with_blocker(Bytes, Fun) ->
    with_blocker(Bytes, client, Fun).

%% Runs Fun with Bytes of the node's room held by a stand-in connection of
%% Role, whose own limit is at least Bytes.
with_blocker(Bytes, Role, Fun) ->
    Conn = spawn(fun idle/0),
    ok = macula_peering_inflight:open_connection(Conn, #{connection_bytes => max(64 * ?MIB, Bytes),
                                                         role => Role}),
    {ok, Blocker} = macula_peering_inflight:try_admit(Conn, control, Bytes, call),
    try
        Fun(Blocker)
    after
        release(Blocker),
        stop_idle(Conn)
    end.

%%%===================================================================
%%% Frames
%%%===================================================================

server_opts() ->
    #{identity        => macula_identity:generate(),
      realms          => [],
      capabilities    => 0,
      controlling_pid => self()}.

inbound_conn() ->
    receive
        {quic, new_conn, Conn, _Info} -> Conn
    after ?EVENT_MS ->
        erlang:error(no_inbound_conn)
    end.

%% The CONNECT a peer sends to open the handshake.
signed_connect() ->
    Kp = macula_identity:generate(),
    Pub = macula_identity:public(Kp),
    Connect = macula_frame:connect(#{node_id         => Pub,
                                     station_id      => Pub,
                                     realms          => [],
                                     capabilities    => 0,
                                     puzzle_evidence => macula_identity:puzzle_evidence(Pub)}),
    macula_frame:encode(macula_frame:sign(Connect, Kp)).

%% A CALL as a station relays it, whose payload carries Bytes of padding.
padded_call(Bytes) ->
    call_with_payload(#{<<"pad">> => binary:copy(<<0>>, Bytes)}).

%% A CALL whose payload holds a list of N small integers, N CBOR items.
many_items_call(N) ->
    call_with_payload(#{<<"items">> => lists:duplicate(N, 1)}).

call_with_payload(Payload) ->
    macula_frame:call(#{
        call_id     => crypto:strong_rand_bytes(16),
        procedure   => <<"io.macula.test.inflight">>,
        realm       => <<0:256>>,
        payload     => Payload,
        deadline_ms => 60_000,
        caller      => <<1:256>>
    }).

encode_signed(Frame) ->
    macula_frame:encode(macula_frame:sign(Frame, macula_identity:generate())).

send_paced(Stream, Bin, Slice, _EveryMs) when byte_size(Bin) =< Slice ->
    macula_quic:send(Stream, Bin);
send_paced(Stream, Bin, Slice, EveryMs) ->
    <<Part:Slice/binary, Rest/binary>> = Bin,
    ok = macula_quic:send(Stream, Part),
    timer:sleep(EveryMs),
    send_paced(Stream, Rest, Slice, EveryMs).

%%%===================================================================
%%% Observations
%%%===================================================================

reserved() ->
    usage_of(reserved).

usage_of(Key) ->
    maps:get(Key, macula_peering:inflight_usage()).

release(Reservation) ->
    done = macula_peering:handle_reserved(Reservation, fun() -> done end).

release_delivered({ok, Reservation}) ->
    release(Reservation);
release_delivered(_NotDelivered) ->
    ok.

%% The reservation of the CALL Server delivers with Frame's call_id.
reserved_call(Server, #{call_id := CallId}) ->
    receive
        {macula_peering, reserved_frame, Server, #{frame_type := call, call_id := CallId}, Reservation} ->
            Reservation
    after ?EVENT_MS ->
        error({not_delivered, CallId})
    end.

%% A frame of Type that Server delivers within Ms, on any route.
reserved_of_type(Server, Type, Ms) ->
    receive
        {macula_peering, reserved_frame, Server, #{frame_type := Type}, Reservation} ->
            {ok, Reservation};
        {macula_peering, reserved_dht_frame, Server, _NodeId, #{frame_type := Type}, Reservation} ->
            {ok, Reservation}
    after Ms ->
        not_delivered
    end.

%% Whether Server delivers any CALL within Ms, in either message shape.
frame_within(Server, Ms) ->
    receive
        {macula_peering, reserved_frame, Server, #{call_id := CallId}, _Reservation} -> {delivered, CallId};
        {macula_peering, frame, Server, #{call_id := CallId}} -> {delivered, CallId}
    after Ms ->
        none
    end.

connected(Server) ->
    receive
        {macula_peering, connected, Server, _PeerNodeId} -> ok
    after ?EVENT_MS ->
        erlang:error(not_connected)
    end.

%% The reason Server gave for ending the connection within Ms, once it has
%% also stopped.
ended(Server, Ms) ->
    Mon = erlang:monitor(process, Server),
    receive
        {macula_peering, disconnected, Server, Reason} -> stopped(Mon, Server, Reason)
    after Ms ->
        erlang:demonitor(Mon, [flush]),
        no_disconnect
    end.

stopped(Mon, Server, Reason) ->
    receive
        {'DOWN', Mon, process, Server, _Exit} -> Reason
    after ?EVENT_MS ->
        {still_running, Reason}
    end.

invalid_reported(Server) ->
    receive
        {macula_peering, invalid_frame, Server, Type, Field} -> {invalid_frame, Type, Field}
    after ?EVENT_MS ->
        not_reported
    end.

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
        {macula_peering, _, _, _} -> drain();
        {macula_peering, _, _, _, _} -> drain();
        {macula_peering, _, _, _, _, _} -> drain();
        {quic, _, _, _} -> drain()
    after 100 ->
        ok
    end.

idle() ->
    receive stop -> ok end.

stop_idle(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! stop,
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after ?EVENT_MS ->
        erlang:demonitor(Ref, [flush]),
        exit({idle_not_stopping, Pid})
    end.

free_udp_port() ->
    {ok, Sock} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Sock),
    ok = gen_udp:close(Sock),
    Port.
