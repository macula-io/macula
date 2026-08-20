%%%-------------------------------------------------------------------
%%% @doc Tests for macula_streamer.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_streamer_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_streamer).
-export([init/1, handle_open/2, terminate/2]).

%%%===================================================================
%%% Test callback module
%%%===================================================================

init(Parent) -> {ok, Parent}.

handle_open(StreamArgs, Parent) ->
    Parent ! {opened, StreamArgs, self()},
    {ok, Parent}.

terminate(Reason, Parent) ->
    Parent ! {terminated, Reason},
    ok.

%%%===================================================================
%%% Fixtures
%%%===================================================================

setup() ->
    meck:new(macula, [passthrough]),
    meck:expect(macula, advertise_stream,
                fun(_Pool, _Realm, _Proc, _Mode, Handler) ->
                    persistent_term:put({?MODULE, handler}, Handler), ok
                end),
    meck:expect(macula, unadvertise_stream, fun(_Pool, _Realm, _Proc) -> ok end),
    meck:expect(macula, publish, fun(_Pool, _Realm, _Topic, _Payload) -> ok end),
    meck:new(macula_stream, [passthrough]),
    meck:expect(macula_stream, send, fun(_Stream, _Chunk) -> ok end),
    meck:expect(macula_stream, close_send, fun(_Stream) -> ok end),
    ok.

teardown(_) ->
    persistent_term:erase({?MODULE, handler}),
    meck:unload(macula_stream),
    meck:unload(macula).

captured_handler() -> persistent_term:get({?MODULE, handler}).

stream_stub() -> receive stop -> ok end.

%%%===================================================================
%%% Tests
%%%===================================================================

streamer_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun opens_and_publishes_lifecycle/0,
      fun send_and_close_drive_the_stream/0,
      fun dead_stream_stops_the_streamer/0]}.

opens_and_publishes_lifecycle() ->
    process_flag(trap_exit, true),
    {ok, _Sup} = macula_streamer:advertise(pool, <<0:256>>, <<"logs.tail_v1">>,
                                           ?MODULE, self()),
    Handler = captured_handler(),
    StreamPid = spawn(fun stream_stub/0),
    ok = Handler(StreamPid, #{topic => <<"t">>}),
    ?assertMatch({opened, #{topic := <<"t">>}, _}, wait_msg()),
    ?assertEqual([<<"streaming.started_v1">>], topics()),
    StreamPid ! stop,
    ?assertMatch({terminated, _}, wait_msg()),
    ?assertEqual([<<"streaming.started_v1">>, <<"streaming.completed_v1">>], topics()).

send_and_close_drive_the_stream() ->
    process_flag(trap_exit, true),
    {ok, _Sup} = macula_streamer:advertise(pool, <<0:256>>, <<"logs.tail_v1">>,
                                           ?MODULE, self()),
    Handler = captured_handler(),
    StreamPid = spawn(fun stream_stub/0),
    ok = Handler(StreamPid, #{}),
    {opened, _, StreamerPid} = wait_msg(),
    ?assertEqual(ok, macula_streamer:send(StreamerPid, <<"line\n">>)),
    ?assertEqual(1, meck:num_calls(macula_stream, send, [StreamPid, <<"line\n">>])),
    ?assertEqual(ok, macula_streamer:close(StreamerPid)),
    ?assertEqual(1, meck:num_calls(macula_stream, close_send, [StreamPid])).

dead_stream_stops_the_streamer() ->
    process_flag(trap_exit, true),
    {ok, _Sup} = macula_streamer:advertise(pool, <<0:256>>, <<"logs.tail_v1">>,
                                           ?MODULE, self()),
    Handler = captured_handler(),
    StreamPid = spawn(fun stream_stub/0),
    ok = Handler(StreamPid, #{}),
    {opened, _, StreamerPid} = wait_msg(),
    Ref = monitor(process, StreamerPid),
    exit(StreamPid, boom),
    receive
        {'DOWN', Ref, process, StreamerPid, _} -> ok
    after 1000 -> ?assert(false)
    end,
    ?assertMatch({terminated, _}, wait_msg()),
    ?assertMatch(#{outcome := failed}, completed_payload()).

%%%===================================================================
%%% Helpers
%%%===================================================================

topics() ->
    [T || {_, {macula, publish, [_Pool, _Realm, T, _Payload]}, ok} <- meck:history(macula)].

completed_payload() ->
    [{_, {macula, publish, [_, _, _, Payload]}, ok}] =
        [E || {_, {macula, publish, [_, _, T, _]}, ok} = E <- meck:history(macula),
              T =:= <<"streaming.completed_v1">>],
    Payload.

wait_msg() ->
    receive
        Msg -> Msg
    after 1000 -> timeout
    end.
