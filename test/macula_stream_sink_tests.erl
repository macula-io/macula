%%%-------------------------------------------------------------------
%%% @doc Tests for macula_stream_sink.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stream_sink_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_stream_sink).
-export([init/1, handle_chunk/2, handle_close/2]).

%%%===================================================================
%%% Test callback module (this module doubles as the sink under test)
%%%===================================================================

init(Parent) -> {ok, Parent}.

handle_chunk(Data, Parent) ->
    Parent ! {chunk_seen, Data},
    case Data of
        <<"stop">> -> {stop, normal, Parent};
        _ -> {noreply, Parent}
    end.

handle_close(Reason, Parent) ->
    Parent ! {closed, Reason},
    ok.

%%%===================================================================
%%% Fixtures
%%%===================================================================

setup() ->
    %% start_link/5 links the caller. Tests whose sink exits with a
    %% non-normal reason set trap_exit themselves, as the first thing
    %% in the test body — eunit's {foreach, ...} runs each test fun in
    %% a freshly spawned process (via eunit_proc:with_timeout), separate
    %% from whatever process ran this setup/0, so a flag set here never
    %% reaches the process the test actually executes in.
    meck:new(macula, [passthrough]),
    meck:expect(macula, call_stream,
                fun(_Pool, _Realm, _Proc, _Args, _Opts) -> {ok, self()} end),
    meck:expect(macula, close_stream, fun(_Stream) -> ok end),
    meck:expect(macula, publish, fun(_Pool, _Realm, _Topic, _Payload) -> ok end),
    ok.

teardown(_) ->
    meck:unload(macula).

recv_returning(Results) ->
    Counter = atomics:new(1, []),
    meck:expect(macula, recv, fun(_Stream, _Timeout) ->
        N = atomics:add_get(Counter, 1, 1),
        lists:nth(N, Results)
    end).

%%%===================================================================
%%% Tests
%%%===================================================================

sink_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun delivers_chunks_then_eof/0,
      fun surfaces_recv_error/0,
      fun callback_can_stop_early/0,
      fun init_stop_propagates/0]}.

delivers_chunks_then_eof() ->
    recv_returning([{chunk, <<"a">>}, {chunk, <<"b">>}, eof]),
    {ok, _Pid} = macula_stream_sink:start_link(?MODULE, pool, <<0:256>>, <<"p">>, self()),
    ?assertEqual({chunk_seen, <<"a">>}, wait_msg()),
    ?assertEqual({chunk_seen, <<"b">>}, wait_msg()),
    ?assertEqual({closed, normal}, wait_msg()),
    ?assertEqual([<<"streaming.started_v1">>, <<"streaming.completed_v1">>], topics()).

surfaces_recv_error() ->
    process_flag(trap_exit, true),
    recv_returning([{error, timeout}]),
    {ok, _Pid} = macula_stream_sink:start_link(?MODULE, pool, <<0:256>>, <<"p">>, self()),
    ?assertEqual({closed, timeout}, wait_msg()).

callback_can_stop_early() ->
    recv_returning([{chunk, <<"stop">>}, {chunk, <<"never seen">>}]),
    {ok, _Pid} = macula_stream_sink:start_link(?MODULE, pool, <<0:256>>, <<"p">>, self()),
    ?assertEqual({chunk_seen, <<"stop">>}, wait_msg()),
    ?assertEqual({closed, normal}, wait_msg()).

init_stop_propagates() ->
    process_flag(trap_exit, true),
    meck:expect(macula, call_stream,
                fun(_Pool, _Realm, _Proc, _Args, _Opts) -> {error, no_healthy_link} end),
    ?assertEqual({error, no_healthy_link},
                 macula_stream_sink:start_link(?MODULE, pool, <<0:256>>, <<"p">>, self())).

wait_msg() ->
    receive
        Msg -> Msg
    after 1000 -> timeout
    end.

topics() ->
    [T || {_, {macula, publish, [_Pool, _Realm, T, _Payload]}, ok} <- meck:history(macula)].
