%%%-------------------------------------------------------------------
%%% @doc Tests for macula_stream_sink.
%%%
%%% Each sink runs on stream I/O the test gives it through the
%%% stream_io start option, over a scripted stream, so no test
%%% replaces the macula module.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stream_sink_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_stream_sink).
-export([init/1, handle_chunk/2, handle_close/2]).

-define(REALM, <<0:256>>).
-define(PROCEDURE, <<"p">>).

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
%%% Stream I/O over a scripted stream
%%%===================================================================

%% recv/2 returns Results in order and then waits, as a stream with
%% nothing more to read does. The other four functions record their
%% calls, in order, in the table returned with them.
scripted_stream_io(Results) ->
    Calls = ets:new(stream_io_calls, [ordered_set, public]),
    Next = atomics:new(1, []),
    Record = fun(Call) ->
                     true = ets:insert(Calls, {erlang:unique_integer([monotonic]), Call}),
                     ok
             end,
    StreamIo = #{call_stream => fun(_Pool, _Realm, _Procedure, _Args, _Opts) ->
                                        ok = Record(call_stream),
                                        {ok, self()}
                                end,
                 recv => fun(_Stream, _Timeout) ->
                                 next_result(atomics:add_get(Next, 1, 1), Results)
                         end,
                 close_stream => fun(_Stream) -> Record(close_stream) end,
                 abort => fun(_Stream, Code, _Message) -> Record({abort, Code}) end,
                 publish => fun(_Pool, _Realm, Topic, _Payload) -> Record({publish, Topic}) end},
    {StreamIo, Calls}.

next_result(N, Results) when N =< length(Results) ->
    lists:nth(N, Results);
next_result(_N, _Results) ->
    receive after infinity -> ok end.

calls(Calls) ->
    [Call || {_, Call} <- ets:tab2list(Calls)].

count(Name, Calls) ->
    length([Call || Call <- calls(Calls), call_name(Call) =:= Name]).

call_name({Name, _}) -> Name;
call_name(Name) -> Name.

%% start_link/7 links the caller, so a test whose sink exits with a
%% non-normal reason sets trap_exit itself, first thing in the test.
start_sink(StreamIo) ->
    {ok, Sink} = macula_stream_sink:start_link(?MODULE, pool, ?REALM, ?PROCEDURE, self(), #{},
                                               #{stream_io => StreamIo}),
    Sink.

topics(Calls) ->
    [Topic || {publish, Topic} <- calls(Calls)].

%%%===================================================================
%%% Tests
%%%===================================================================

%% Each test runs in a process of its own.
sink_test_() ->
    [{spawn, Test}
     || Test <- [fun delivers_chunks_then_eof/0,
                 fun surfaces_recv_error/0,
                 fun callback_can_stop_early/0,
                 fun init_stop_propagates/0,
                 fun normal_stop_closes_not_aborts/0,
                 fun abnormal_stop_aborts_not_closes/0,
                 fun a_direct_sink_runs_on_the_stream_io_it_is_given/0,
                 fun without_stream_io_a_sink_dials_through_the_macula_facade/0,
                 fun a_stream_io_of_other_than_the_five_functions_is_refused/0]].

delivers_chunks_then_eof() ->
    {StreamIo, Calls} = scripted_stream_io([{chunk, <<"a">>}, {chunk, <<"b">>}, eof]),
    _ = start_sink(StreamIo),
    ?assertEqual({chunk_seen, <<"a">>}, wait_msg()),
    ?assertEqual({chunk_seen, <<"b">>}, wait_msg()),
    ?assertEqual({closed, normal}, wait_msg()),
    ?assertEqual([<<"streaming.started_v1">>, <<"streaming.completed_v1">>], topics(Calls)).

surfaces_recv_error() ->
    process_flag(trap_exit, true),
    {StreamIo, _Calls} = scripted_stream_io([{error, timeout}]),
    _ = start_sink(StreamIo),
    ?assertEqual({closed, timeout}, wait_msg()).

%% A clean eof (Reason = normal) closes the underlying stream both
%% sides — the pre-Phase-5 behaviour — and never sends an abort.
normal_stop_closes_not_aborts() ->
    {StreamIo, Calls} = scripted_stream_io([eof]),
    _ = start_sink(StreamIo),
    ?assertEqual({closed, normal}, wait_msg()),
    ?assertEqual(1, count(close_stream, Calls)),
    ?assertEqual(0, count(abort, Calls)).

%% A non-normal stop (here: the reader delivering a recv error) sends
%% the provider an explicit abort instead of an ordinary close — this
%% is the bug Phase 5 fixes: before, `terminate/2' called
%% `close_stream' unconditionally, so the provider could not tell a
%% real cancellation/failure from a clean end-of-stream.
abnormal_stop_aborts_not_closes() ->
    process_flag(trap_exit, true),
    {StreamIo, Calls} = scripted_stream_io([{error, timeout}]),
    _ = start_sink(StreamIo),
    ?assertEqual({closed, timeout}, wait_msg()),
    ?assertEqual([{abort, <<"cancelled">>}], [Call || {abort, _} = Call <- calls(Calls)]),
    ?assertEqual(0, count(close_stream, Calls)).

callback_can_stop_early() ->
    {StreamIo, _Calls} = scripted_stream_io([{chunk, <<"stop">>}, {chunk, <<"never seen">>}]),
    _ = start_sink(StreamIo),
    ?assertEqual({chunk_seen, <<"stop">>}, wait_msg()),
    ?assertEqual({closed, normal}, wait_msg()).

init_stop_propagates() ->
    process_flag(trap_exit, true),
    {StreamIo, _Calls} = scripted_stream_io([]),
    Refusing = StreamIo#{call_stream := fun(_Pool, _Realm, _Procedure, _Args, _Opts) ->
                                                {error, no_healthy_link}
                                        end},
    ?assertEqual({error, no_healthy_link},
                 macula_stream_sink:start_link(?MODULE, pool, ?REALM, ?PROCEDURE, self(), #{},
                                               #{stream_io => Refusing})).

%% Without stream_io a sink dials with macula:call_stream/5, whose
%% guard refuses a pool that is not a process.
without_stream_io_a_sink_dials_through_the_macula_facade() ->
    process_flag(trap_exit, true),
    ?assertMatch({error, {function_clause, [{macula, call_stream, _, _} | _]}},
                 macula_stream_sink:start_link(?MODULE, pool, ?REALM, ?PROCEDURE, self())).

a_direct_sink_runs_on_the_stream_io_it_is_given() ->
    {StreamIo, Calls} = scripted_stream_io([{chunk, <<"a">>}, eof]),
    {ok, _Sink} = macula_stream_sink:start_link_direct(?MODULE, pool, ?REALM, ?PROCEDURE, self(),
                                                       undefined, #{stream_io => StreamIo}),
    ?assertEqual({chunk_seen, <<"a">>}, wait_msg()),
    ?assertEqual({closed, normal}, wait_msg()),
    ?assertEqual([call_stream, {publish, <<"streaming.started_v1">>}, close_stream,
                  {publish, <<"streaming.completed_v1">>}], calls(Calls)).

a_stream_io_of_other_than_the_five_functions_is_refused() ->
    {StreamIo, _Calls} = scripted_stream_io([]),
    Start = fun(Given) ->
                    macula_stream_sink:start_link(?MODULE, pool, ?REALM, ?PROCEDURE, self(), #{},
                                                  #{stream_io => Given})
            end,
    ?assertError(function_clause, Start(maps:remove(abort, StreamIo))),
    ?assertError(function_clause, Start(StreamIo#{recv := fun(_Stream) -> eof end})),
    ?assertError(function_clause, Start(StreamIo#{extra => fun(_Stream) -> ok end})).

wait_msg() ->
    receive
        Msg -> Msg
    after 1000 -> timeout
    end.
