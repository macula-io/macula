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
                fun(_Pool, _Realm, _Proc, Mode, Handler) ->
                    persistent_term:put({?MODULE, handler}, Handler),
                    persistent_term:put({?MODULE, advertised_mode}, Mode),
                    ok
                end),
    meck:expect(macula, unadvertise_stream, fun(_Pool, _Realm, _Proc) -> ok end),
    meck:expect(macula, publish, fun(_Pool, _Realm, _Topic, _Payload) -> ok end),
    meck:new(macula_stream, [passthrough]),
    meck:expect(macula_stream, send, fun(_Stream, _Chunk) -> ok end),
    meck:expect(macula_stream, close_send, fun(_Stream) -> ok end),
    meck:expect(macula_stream, close, fun(_Stream) -> ok end),
    meck:expect(macula_stream, abort, fun(_Stream, _Code, _Message) -> ok end),
    meck:new(macula_direct_dial, [passthrough]),
    meck:expect(macula_direct_dial, publish_advertisement,
                fun(_Pool, _Realm, _Proc, _Identity, _Opts) -> ok end),
    ok.

teardown(_) ->
    persistent_term:erase({?MODULE, handler}),
    persistent_term:erase({?MODULE, advertised_mode}),
    meck:unload(macula_stream),
    meck:unload(macula_direct_dial),
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
      fun dead_stream_stops_the_streamer/0,
      fun advertise_direct_forwards_mode_to_advertise_stream/0]}.

%% Regression test for a real bug found while building macula_upload
%% (PLAN_PUSH_UPLOAD.md Phase 6): `advertise_direct/7' used to call
%% `advertise/5' (the arity that always defaults `mode' to
%% `server_stream'), silently discarding whatever `mode' the caller
%% passed in `Opts' — a `client_stream' provider that advertised
%% directly would have been served as `server_stream' instead, with no
%% error anywhere to say so.
advertise_direct_forwards_mode_to_advertise_stream() ->
    Identity = macula_identity:generate(),
    {ok, _Sup} = macula_streamer:advertise_direct(pool, <<0:256>>, <<"bulk.ingest">>,
                                                  ?MODULE, self(), Identity,
                                                  #{mode => client_stream}),
    ?assertEqual(client_stream, persistent_term:get({?MODULE, advertised_mode})),
    ?assertEqual(1, meck:num_calls(macula_direct_dial, publish_advertisement,
                                   [pool, <<0:256>>, <<"bulk.ingest">>, Identity, '_'])).

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
    ?assertEqual([<<"streaming.started_v1">>, <<"streaming.completed_v1">>], topics()),
    ?assertEqual(1, meck:num_calls(macula_stream, close, [StreamPid])),
    ?assertEqual(0, meck:num_calls(macula_stream, abort, ['_', '_', '_'])).

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
    ?assertMatch(#{outcome := failed}, completed_payload()),
    ?assertEqual(1, meck:num_calls(macula_stream, abort, [StreamPid, <<"cancelled">>, '_'])),
    ?assertEqual(0, meck:num_calls(macula_stream, close, [StreamPid])).

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
