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
      fun advertise_direct_forwards_mode_to_advertise_stream/0,
      fun reuse_sup_resends_advertise_without_a_new_supervisor/0,
      fun reuse_sup_with_a_dead_pid_starts_a_fresh_supervisor/0]}.

%% A station's wire-level registration for a procedure is tied to the
%% connection that sent it, and does not survive that connection being
%% replaced -- a periodic re-advertise is the only way to keep it
%% current. `reuse_sup' is what makes that safe: without it, every
%% re-advertise call starts a fresh factory supervisor, leaking one per
%% tick forever.
reuse_sup_resends_advertise_without_a_new_supervisor() ->
    {ok, Sup1} = macula_streamer:advertise(pool, <<0:256>>, <<"bulk.ingest">>,
                                           ?MODULE, self()),
    {ok, Sup2} = macula_streamer:advertise(pool, <<0:256>>, <<"bulk.ingest">>,
                                           ?MODULE, self(), #{reuse_sup => Sup1}),
    ?assertEqual(Sup1, Sup2),
    ?assertEqual(2, meck:num_calls(macula, advertise_stream,
                                   [pool, <<0:256>>, <<"bulk.ingest">>, '_', '_'])).

%% Regression test for the identical noproc-on-first-dispatch bug fixed
%% in `macula_response' (found live 2026-09-01 via hecate-rag): see that
%% module's test of the same name for the full incident.
reuse_sup_with_a_dead_pid_starts_a_fresh_supervisor() ->
    DeadPid = spawn(fun() -> ok end),
    wait_until_dead(DeadPid),
    {ok, Sup} = macula_streamer:advertise(pool, <<0:256>>, <<"bulk.ingest">>,
                                          ?MODULE, self(), #{reuse_sup => DeadPid}),
    ?assert(is_pid(Sup)),
    ?assertNotEqual(DeadPid, Sup),
    ?assert(erlang:is_process_alive(Sup)).

wait_until_dead(Pid) ->
    wait_until_dead(Pid, erlang:is_process_alive(Pid)).

wait_until_dead(_Pid, false) -> ok;
wait_until_dead(Pid, true) -> timer:sleep(1), wait_until_dead(Pid, erlang:is_process_alive(Pid)).

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
