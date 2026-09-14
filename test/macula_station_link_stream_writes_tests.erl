%%%-------------------------------------------------------------------
%%% @doc Tests for a station link whose station stops reading one of its
%%% dedicated streams.
%%%
%%% A station run by this test stops reading one streaming session's
%%% stream, or a content stream with a large call pending. The link keeps
%%% answering is_connected/1 and publish/4 within 50 ms, a second session
%%% on another stream keeps flowing, and a second call on the stalled
%%% content stream returns {error, call_pending} and queues nothing. Calls
%%% that time out on the stalled content stream fill it until a call
%%% returns {error, busy}, and a busy call leaves no call pending. Once the
%%% station reads again, the stalled session's frames arrive in order,
%%% STREAM_OPEN first, and the content stream carries exactly the calls it
%%% took. A session whose process ends before its last frame
%%% has its stream reset with the session-ended code, and a session told
%%% that a write failed ends once, with a transport error. A session's
%%% status does not show the key that signs its frames. The scenarios
%%% with a station run in a peer node of their own.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_station_link_stream_writes_tests).

-include_lib("eunit/include/eunit.hrl").
-include("macula_quic_error_codes.hrl").

%% Scenarios, run in a peer node.
-export([session_stall/0,
         content_stall/0,
         session_ended_early/0]).

-define(REALM, <<0:256>>).
-define(WINDOW, 65_536).
-define(CHUNK_BYTES, 65_536).
-define(CHUNKS, 64).
-define(CALL_BYTES, 256 * 1024).
-define(FIRST_CALL_MS, 2_000).
-define(FILL_CALL_MS, 100).
-define(MAX_FILL_CALLS, 16).
-define(FAST_MS, 50).
-define(QUIET_MS, 500).
-define(EVENT_TIMEOUT_MS, 10_000).
-define(SCENARIO_TIMEOUT_MS, 90_000).

stream_writes_test_() ->
    [{"a stalled session leaves the link serving, and its frames later arrive in order",
      {timeout, 150, fun session_stall_keeps_the_link_serving/0}},
     {"a stalled content stream leaves the link serving, and refused calls queue nothing",
      {timeout, 150, fun content_stall_keeps_the_link_serving/0}},
     {"a session that ends before its last frame has its stream reset",
      {timeout, 120, fun early_end_resets_the_stream/0}},
     {"a session told that a write failed ends once, with a transport error",
      {timeout, 30, fun write_failure_ends_the_session_once/0}},
     {"a session's status does not show the key that signs its frames",
      fun status_hides_the_signing_key/0}].

session_stall_keeps_the_link_serving() ->
    ?assertEqual({ok, {answered, published, second_session_flows, stream_open_first,
                       all_in_order}},
                 in_peer(session_stall, [])).

content_stall_keeps_the_link_serving() ->
    ?assertEqual({ok, {answered, published, {error, call_pending}, {error, timeout}, busy,
                       {error, busy}, queued_calls_arrived}},
                 in_peer(content_stall, [])).

early_end_resets_the_stream() ->
    ?assertEqual({ok, {reset, ?QUIC_CODE_SESSION_ENDED}},
                 in_peer(session_ended_early, [])).

write_failure_ends_the_session_once() ->
    Sid = crypto:strong_rand_bytes(16),
    {ok, Session} = macula_stream:start_link(#{id => Sid, role => client, mode => bidi,
                                              owner => self()}),
    Link = spawn(fun() -> receive stop -> ok end end),
    ok = macula_stream:attach_to_link(Session, Link, Sid, macula_identity:generate()),
    Test = self(),
    _Reader = spawn(fun() -> Test ! {received, macula_stream:recv(Session, 5_000)} end),
    timer:sleep(100),
    Session ! {stream_write_failed, Sid, connection_lost},
    First = receive {received, Result} -> Result after 2_000 -> no_result end,
    Session ! {stream_write_failed, Sid, a_later_failure},
    Reply = maps:get(reply, macula_stream:info(Session)),
    Send = macula_stream:send(Session, <<"after the failure">>),
    Link ! stop,
    ?assertEqual({error, {transport, connection_lost}}, First),
    ?assertEqual({error, {transport, connection_lost}}, Reply),
    ?assertEqual({error, send_closed}, Send).

status_hides_the_signing_key() ->
    Sid = crypto:strong_rand_bytes(16),
    {ok, Session} = macula_stream:start_link(#{id => Sid, role => client, mode => bidi,
                                              owner => self()}),
    Link = spawn(fun() -> receive stop -> ok end end),
    #{private := Private} = Signer = macula_identity:generate(),
    ok = macula_stream:attach_to_link(Session, Link, Sid, Signer),
    Status = sys:get_status(Session),
    LastCall = macula_stream:format_status(
                 #{message => {'$gen_call', {self(), make_ref()},
                               {pair_via_link, Link, Sid, Signer}}}),
    Link ! stop,
    ?assertNot(contains(Status, Private)),
    ?assertNot(contains(LastCall, Private)).

%%%===================================================================
%%% Scenarios
%%%===================================================================

session_stall() ->
    #{link := Link, station := Station} = Env = station_and_link(),
    {ok, First} = macula_station_link:call_stream(Link, ?REALM, <<"stall.first">>, #{},
                                                  #{mode => bidi}),
    FirstStream = dedicated_stream(Station),
    ok = macula_quic:setopt(FirstStream, active, false),
    _Sender = spawn(fun() -> send_chunks(First, 1) end),
    timer:sleep(?QUIET_MS),
    Answer = answer_within(fun() -> macula_station_link:is_connected(Link) end, ?FAST_MS),
    Published = answer_within(
                  fun() -> macula_station_link:publish(Link, ?REALM, <<"still.here">>, <<"p">>) end,
                  ?FAST_MS),
    Second = second_session(answer_within(
                              fun() ->
                                  macula_station_link:call_stream(Link, ?REALM, <<"flow.second">>,
                                                                  #{}, #{mode => bidi})
                              end, 1_000),
                            Station),
    ok = macula_quic:setopt(FirstStream, active, true),
    Frames = frames_on(FirstStream, ?CHUNKS + 1),
    kept(Env, {answered(Answer), published(Published), Second, first_frame(Frames),
               in_order(Frames)}).

content_stall() ->
    #{link := Link, station := Station} = Env = station_and_link(),
    {ok, Content} = macula_station_link:open_content_stream(Link),
    Scenario = self(),
    Payload = #{payload => crypto:strong_rand_bytes(?CALL_BYTES)},
    _Caller = spawn(fun() ->
                        Scenario ! {first_call, macula_station_link:call_on_stream(
                                                  Link, Content, ?REALM, <<"_content.get_block">>,
                                                  Payload, ?FIRST_CALL_MS)}
                    end),
    StationStream = dedicated_stream(Station),
    ok = macula_quic:setopt(StationStream, active, false),
    timer:sleep(?QUIET_MS),
    Answer = answer_within(fun() -> macula_station_link:is_connected(Link) end, ?FAST_MS),
    Published = answer_within(
                  fun() -> macula_station_link:publish(Link, ?REALM, <<"still.here">>, <<"p">>) end,
                  ?FAST_MS),
    SecondCall = answer_within(fun() -> small_call(Link, Content) end, ?FAST_MS),
    FirstCall = receive {first_call, Result} -> Result after ?EVENT_TIMEOUT_MS -> no_answer end,
    {Queued, Busy} = calls_until_busy(Link, Content, Payload, 0),
    AfterBusy = answer_within(fun() -> small_call(Link, Content) end, ?FAST_MS),
    ok = macula_quic:setopt(StationStream, active, true),
    Calls = length([F || #{frame_type := call} = F <- frames_until_quiet(StationStream)]),
    kept(Env, {answered(Answer), published(Published), call_answer(SecondCall), FirstCall,
               Busy, call_answer(AfterBusy), calls_arrived(Calls, 1 + Queued)}).

session_ended_early() ->
    #{link := Link, station := Station} = Env = station_and_link(),
    Owner = spawn(fun() -> receive stop -> ok end end),
    {ok, Session} = macula_station_link:call_stream(Link, ?REALM, <<"end.early">>, #{},
                                                    #{mode => bidi, owner => Owner}),
    ok = macula_stream:send(Session, <<"before the end">>),
    StationStream = dedicated_stream(Station),
    exit(Owner, kill),
    kept(Env, stream_end(StationStream)).

%%%===================================================================
%%% The station
%%%===================================================================

%% A station run by this process: a listener with a 64 KiB stream window,
%% and a peering connection accepted from a station link that dialled it.
station_and_link() ->
    {ok, _} = application:ensure_all_started(macula),
    Port = free_udp_port(),
    {ok, Listener} = macula_test_tmp:with_dir("macula-link-stream-writes",
                                             fun(Dir) -> station_listener(Dir, Port) end),
    ok = macula_quic:async_accept(Listener),
    {ok, Link} = macula_station_link:start_link(#{
                   seed     => #{host => <<"127.0.0.1">>, port => Port, verify => none},
                   identity => macula_identity:generate()}),
    Conn = receive {quic, new_conn, C, _Info} -> C after ?EVENT_TIMEOUT_MS -> error(no_link_connection) end,
    {ok, Station} = macula_peering:accept(Conn, #{identity        => macula_identity:generate(),
                                                  realms          => [],
                                                  capabilities    => 0,
                                                  controlling_pid => self()}),
    ok = station_connected(Station),
    ok = link_connected(Link, ?EVENT_TIMEOUT_MS),
    #{listener => Listener, link => Link, station => Station}.

station_connected(Station) ->
    receive
        {macula_peering, connected, Station, _PeerNodeId} -> ok
    after ?EVENT_TIMEOUT_MS ->
        error(station_not_connected)
    end.

link_connected(_Link, Left) when Left =< 0 ->
    error(link_not_connected);
link_connected(Link, Left) ->
    link_connected(macula_station_link:is_connected(Link), Link, Left).

link_connected(true, _Link, _Left) ->
    ok;
link_connected(false, Link, Left) ->
    timer:sleep(10),
    link_connected(Link, Left - 10).

dedicated_stream(Station) ->
    receive
        {macula_peering, new_dedicated_stream, Station, Stream} -> Stream
    after ?EVENT_TIMEOUT_MS ->
        error(no_dedicated_stream)
    end.

%% The next `Count' frames on `Stream', or as many as arrive before the
%% event timeout.
frames_on(Stream, Count) ->
    collect_frames(Stream, Count, <<>>, [], ?EVENT_TIMEOUT_MS).

%% The frames on `Stream' until nothing arrives for a while.
frames_until_quiet(Stream) ->
    collect_frames(Stream, infinity, <<>>, [], ?QUIET_MS).

collect_frames(_Stream, Count, _Buffer, Acc, _WaitMs)
  when is_integer(Count), length(Acc) >= Count ->
    lists:reverse(Acc);
collect_frames(Stream, Count, Buffer, Acc, WaitMs) ->
    receive
        {quic, Data, Stream, _Flags} when is_binary(Data) ->
            {Frames, Rest} = macula_frame:parse_stream(<<Buffer/binary, Data/binary>>),
            collect_frames(Stream, Count, Rest, lists:reverse(Frames, Acc), WaitMs)
    after WaitMs ->
        lists:reverse(Acc)
    end.

stream_end(Stream) ->
    receive
        {quic, stream_closed, Stream, {reset, Code}} -> {reset, Code};
        {quic, stream_closed, Stream, Detail} -> {closed, Detail};
        {quic, peer_send_shutdown, Stream, _} -> finished;
        {quic, Data, Stream, _Flags} when is_binary(Data) -> stream_end(Stream)
    after ?EVENT_TIMEOUT_MS ->
        no_end
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================

send_chunks(_Session, Seq) when Seq > ?CHUNKS ->
    ok;
send_chunks(Session, Seq) ->
    ok = macula_stream:send(Session, chunk(Seq)),
    send_chunks(Session, Seq + 1).

chunk(Seq) ->
    binary:copy(<<Seq>>, ?CHUNK_BYTES).

second_session({answered, {ok, Second}}, Station) ->
    SecondStream = dedicated_stream(Station),
    ok = macula_stream:send(Second, <<"hello">>),
    chunk_arrived(frames_on(SecondStream, 2));
second_session(Other, _Station) ->
    {second_session_blocked, Other}.

chunk_arrived(Frames) ->
    arrived([B || #{frame_type := stream_data, body := B} <- Frames]).

arrived([<<"hello">>]) -> second_session_flows;
arrived(Bodies) -> {second_session_stalled, Bodies}.

first_frame([#{frame_type := stream_open} | _]) -> stream_open_first;
first_frame([#{frame_type := Type} | _]) -> {first_frame, Type};
first_frame([]) -> no_frames.

in_order(Frames) ->
    Bodies = [B || #{frame_type := stream_data, body := B} <- Frames],
    in_order_bodies(Bodies =:= [chunk(Seq) || Seq <- lists:seq(1, ?CHUNKS)], Bodies).

in_order_bodies(true, _Bodies) -> all_in_order;
in_order_bodies(false, Bodies) -> {not_in_order, length(Bodies)}.

answered({answered, _Value}) -> answered;
answered(no_answer) -> no_answer.

published({answered, ok}) -> published;
published(Other) -> {not_published, Other}.

call_answer({answered, Result}) -> Result;
call_answer(no_answer) -> no_answer.

calls_arrived(Expected, Expected) -> queued_calls_arrived;
calls_arrived(Calls, Expected) -> {calls, Calls, queued, Expected}.

%% A small call on the content stream, from a caller that finds it stalled.
small_call(Link, Content) ->
    macula_station_link:call_on_stream(Link, Content, ?REALM, <<"_content.get_block">>,
                                       #{payload => <<"small">>}, 5_000).

%% Large calls that time out on the stalled content stream, until one is
%% refused as busy: how many were queued, and how the filling ended.
calls_until_busy(_Link, _Content, _Payload, Queued) when Queued >= ?MAX_FILL_CALLS ->
    {Queued, {no_busy_after, Queued}};
calls_until_busy(Link, Content, Payload, Queued) ->
    fill_call(macula_station_link:call_on_stream(Link, Content, ?REALM, <<"_content.get_block">>,
                                                 Payload, ?FILL_CALL_MS),
              Link, Content, Payload, Queued).

fill_call({error, timeout}, Link, Content, Payload, Queued) ->
    calls_until_busy(Link, Content, Payload, Queued + 1);
fill_call({error, busy}, _Link, _Content, _Payload, Queued) ->
    {Queued, busy};
fill_call(Other, _Link, _Content, _Payload, Queued) ->
    {Queued, {unexpected, Other}}.

%% Whether `Term' appears anywhere inside `Container'.
contains(Term, Term) -> true;
contains(Container, Term) when is_tuple(Container) -> contains(tuple_to_list(Container), Term);
contains(Container, Term) when is_map(Container) -> contains(maps:to_list(Container), Term);
contains([Head | Tail], Term) -> contains(Head, Term) orelse contains(Tail, Term);
contains(_Other, _Term) -> false.

%% Runs `Fun' in a separate process and returns its value if it comes
%% within `Ms'.
answer_within(Fun, Ms) ->
    Scenario = self(),
    Asker = spawn(fun() -> Scenario ! {answer, self(), Fun()} end),
    receive
        {answer, Asker, Value} -> {answered, Value}
    after Ms ->
        exit(Asker, kill),
        no_answer
    end.

%% Returns Result with the station's handles referenced until now.
kept(#{listener := _, link := _, station := _}, Result) ->
    Result.

%% A station listener on `Port' with a 64 KiB stream window, whose
%% certificate and key live in `Dir' while listen reads them.
station_listener(Dir, Port) ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(iolist_to_binary(Pub), iolist_to_binary(Priv),
                                              [<<"localhost">>, <<"127.0.0.1">>]),
    Cert = filename:join(Dir, "station.crt"),
    Key = filename:join(Dir, "station.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    macula_quic:listen(<<"127.0.0.1">>, Port,
                       [{cert, Cert}, {key, Key}, {alpn, [<<"macula">>]},
                        {stream_receive_window, ?WINDOW},
                        {receive_window, 64 * ?WINDOW}]).

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
