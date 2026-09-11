%%%-------------------------------------------------------------------
%%% @doc Tests for macula_stream_sink.
%%%
%%% Each sink runs on stream I/O and a fact publish the test gives it
%%% through the stream_io and fact_publish start options, over a
%%% scripted stream, so no test replaces the macula module.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stream_sink_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_stream_sink).
-export([init/1, handle_chunk/2, handle_close/2]).

-define(REALM, <<0:256>>).
-define(PROCEDURE, <<"p">>).
%% Sinks the reader lifetime test stops.
-define(STOPS, 500).
%% How long a start or a stop may take while the pool never answers.
-define(BOUND_MS, 1000).
%% Data a reason carries that must stay on this node.
-define(MARKER, <<"marker-3f9c-stays-on-this-node">>).

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

%% Start options over a scripted stream. recv/2 returns Results in order
%% and then waits, as a stream with nothing more to read does. The other
%% stream functions and fact_publish record their calls, in order, in the
%% table returned with the options; fact_publish also sends the test
%% {published, Topic, Payload}, and abort/3 sends it {aborted, Code, Message}.
scripted_opts(Results) ->
    Test = self(),
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
                 abort => fun(_Stream, Code, Message) ->
                                  Test ! {aborted, Code, Message},
                                  Record({abort, Code})
                          end},
    FactPublish = fun(_Pool, _Realm, Topic, Payload) ->
                          ok = Record({publish, Topic}),
                          Test ! {published, Topic, Payload},
                          ok
                  end,
    {#{stream_io => StreamIo, fact_publish => FactPublish}, Calls}.

%% Opts with its stream function Key given as Fun.
with_io(#{stream_io := StreamIo} = Opts, Key, Fun) ->
    Opts#{stream_io := StreamIo#{Key => Fun}}.

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
start_sink(Opts) ->
    {ok, Sink} = macula_stream_sink:start_link(?MODULE, pool, ?REALM, ?PROCEDURE, self(), #{},
                                               Opts),
    Sink.

%% Returns once Sink has exited.
wait_down(Sink) ->
    Ref = monitor(process, Sink),
    receive
        {'DOWN', Ref, process, Sink, _} -> ok
    after 5000 ->
        error(sink_did_not_exit)
    end.

next_published() ->
    receive
        {published, Topic, Payload} -> {Topic, Payload}
    after 5000 ->
        error(nothing_published)
    end.

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
                 fun a_stream_io_missing_a_function_or_of_another_shape_is_refused/0,
                 fun a_stream_io_may_carry_other_stream_functions/0,
                 {timeout, 60, fun a_stopped_sinks_reader_is_gone_before_the_sink_is/0},
                 fun a_sink_whose_pool_is_gone_still_reads_and_closes/0,
                 {timeout, 10, fun a_sink_whose_pool_never_answers_starts_and_stops_at_once/0},
                 {timeout, 10, fun a_killed_sinks_end_is_announced_with_the_kill_reason/0},
                 fun a_failed_streams_end_fact_and_abort_name_the_reason_only/0]].

delivers_chunks_then_eof() ->
    {Opts, _Calls} = scripted_opts([{chunk, <<"a">>}, {chunk, <<"b">>}, eof]),
    Sink = start_sink(Opts),
    ?assertEqual({chunk_seen, <<"a">>}, wait_msg()),
    ?assertEqual({chunk_seen, <<"b">>}, wait_msg()),
    ?assertEqual({closed, normal}, wait_msg()),
    wait_down(Sink),
    ?assertMatch({<<"streaming.started_v1">>, #{stream_id := _}}, next_published()),
    ?assertMatch({<<"streaming.completed_v1">>, #{outcome := completed}}, next_published()).

surfaces_recv_error() ->
    process_flag(trap_exit, true),
    {Opts, _Calls} = scripted_opts([{error, timeout}]),
    Sink = start_sink(Opts),
    ?assertEqual({closed, timeout}, wait_msg()),
    wait_down(Sink).

%% A clean eof (Reason = normal) closes the underlying stream both
%% sides — the pre-Phase-5 behaviour — and never sends an abort.
normal_stop_closes_not_aborts() ->
    {Opts, Calls} = scripted_opts([eof]),
    Sink = start_sink(Opts),
    ?assertEqual({closed, normal}, wait_msg()),
    wait_down(Sink),
    ?assertEqual(1, count(close_stream, Calls)),
    ?assertEqual(0, count(abort, Calls)).

%% A non-normal stop (here: the reader delivering a recv error) sends
%% the provider an explicit abort instead of an ordinary close — this
%% is the bug Phase 5 fixes: before, `terminate/2' called
%% `close_stream' unconditionally, so the provider could not tell a
%% real cancellation/failure from a clean end-of-stream.
abnormal_stop_aborts_not_closes() ->
    process_flag(trap_exit, true),
    {Opts, Calls} = scripted_opts([{error, timeout}]),
    Sink = start_sink(Opts),
    ?assertEqual({closed, timeout}, wait_msg()),
    wait_down(Sink),
    ?assertEqual([{abort, <<"cancelled">>}], [Call || {abort, _} = Call <- calls(Calls)]),
    ?assertEqual(0, count(close_stream, Calls)).

callback_can_stop_early() ->
    {Opts, _Calls} = scripted_opts([{chunk, <<"stop">>}, {chunk, <<"never seen">>}]),
    Sink = start_sink(Opts),
    ?assertEqual({chunk_seen, <<"stop">>}, wait_msg()),
    ?assertEqual({closed, normal}, wait_msg()),
    wait_down(Sink).

init_stop_propagates() ->
    process_flag(trap_exit, true),
    {Opts, _Calls} = scripted_opts([]),
    Refusing = with_io(Opts, call_stream, fun(_Pool, _Realm, _Procedure, _Args, _CallOpts) ->
                                                  {error, no_healthy_link}
                                          end),
    %% A failed start returns only once the sink is down: proc_lib waits
    %% for its DOWN and takes its EXIT message.
    ?assertEqual({error, no_healthy_link},
                 macula_stream_sink:start_link(?MODULE, pool, ?REALM, ?PROCEDURE, self(), #{},
                                               Refusing)).

%% Without stream_io a sink dials with macula:call_stream/5, whose
%% guard refuses a pool that is not a process. As with any failed
%% start, start_link/5 returns once the sink is down.
without_stream_io_a_sink_dials_through_the_macula_facade() ->
    process_flag(trap_exit, true),
    ?assertMatch({error, {function_clause, [{macula, call_stream, _, _} | _]}},
                 macula_stream_sink:start_link(?MODULE, pool, ?REALM, ?PROCEDURE, self())).

a_direct_sink_runs_on_the_stream_io_it_is_given() ->
    {Opts, Calls} = scripted_opts([{chunk, <<"a">>}, eof]),
    {ok, Sink} = macula_stream_sink:start_link_direct(?MODULE, pool, ?REALM, ?PROCEDURE, self(),
                                                      undefined, Opts),
    ?assertEqual({chunk_seen, <<"a">>}, wait_msg()),
    ?assertEqual({closed, normal}, wait_msg()),
    wait_down(Sink),
    ?assertEqual([call_stream, close_stream],
                 [Call || Call <- calls(Calls), call_name(Call) =/= publish]),
    ?assertMatch({<<"streaming.started_v1">>, _}, next_published()),
    ?assertMatch({<<"streaming.completed_v1">>, _}, next_published()).

a_stream_io_missing_a_function_or_of_another_shape_is_refused() ->
    {#{stream_io := StreamIo} = Opts, _Calls} = scripted_opts([]),
    Start = fun(Given) ->
                    macula_stream_sink:start_link(?MODULE, pool, ?REALM, ?PROCEDURE, self(), #{},
                                                  Given)
            end,
    ?assertError(function_clause, Start(Opts#{stream_io := maps:remove(abort, StreamIo)})),
    ?assertError(function_clause, Start(with_io(Opts, recv, fun(_Stream) -> eof end))),
    ?assertError(function_clause, Start(with_io(Opts, extra, fun(_Stream) -> ok end))),
    ?assertError(function_clause,
                 Start(Opts#{fact_publish := fun(_Pool, _Realm, _Topic) -> ok end})).

%% A stream_io may carry stream functions other wrappers call, such as
%% the send/3 and set_reply/2 a provider writes with.
a_stream_io_may_carry_other_stream_functions() ->
    {Opts, Calls} = scripted_opts([{chunk, <<"a">>}, eof]),
    Wider = with_io(with_io(Opts, send, fun(_Stream, _Chunk, _Encoding) -> ok end),
                    set_reply, fun(_Stream, _Value) -> ok end),
    Sink = start_sink(Wider),
    ?assertEqual({chunk_seen, <<"a">>}, wait_msg()),
    ?assertEqual({closed, normal}, wait_msg()),
    wait_down(Sink),
    ?assertEqual(1, count(close_stream, Calls)).

%% A stopped sink's reader has exited by the time the sink has, so it
%% never goes on calling recv/2 on a stream nobody reads. Each reader
%% here makes a table it owns and then keeps running inside recv/2, so
%% a kill reaches it only when it is next descheduled. A table goes
%% when its owner exits, before the owner's DOWN is sent, so a table
%% still there once its sink has exited belongs to a reader that
%% outlived its sink.
a_stopped_sinks_reader_is_gone_before_the_sink_is() ->
    Test = self(),
    {Opts, _Calls} = scripted_opts([]),
    Reading = with_io(Opts, recv, fun(_Stream, _Timeout) ->
                                          Test ! {reader_table, ets:new(reader_table, [public])},
                                          keep_running(0)
                                  end),
    Running = Reading#{fact_publish := fun(_Pool, _Realm, _Topic, _Payload) -> ok end},
    Tables = [table_once_its_sink_has_stopped(Running) || _ <- lists:seq(1, ?STOPS)],
    ?assertEqual(0, length([Table || {present, Table} <- Tables])).

table_once_its_sink_has_stopped(Opts) ->
    Sink = start_sink(Opts),
    Table = receive
                {reader_table, T} -> T
            after 5000 ->
                error(no_reader_table)
            end,
    ok = gen_server:stop(Sink),
    ?assertEqual({closed, normal}, wait_msg()),
    table_state(ets:info(Table, owner), Table).

table_state(undefined, Table) -> {gone, Table};
table_state(_Owner, Table) -> {present, Table}.

keep_running(N) ->
    keep_running(N + 1).

%% A pool that is gone makes publish exit. The sink still starts,
%% delivers, closes its stream and calls handle_close/2.
a_sink_whose_pool_is_gone_still_reads_and_closes() ->
    process_flag(trap_exit, true),
    {Opts, Calls} = scripted_opts([{chunk, <<"a">>}, eof]),
    Gone = Opts#{fact_publish := fun(_Pool, _Realm, _Topic, _Payload) ->
                                         exit({noproc, {gen_server, call, [pool, publish]}})
                                 end},
    Sink = start_sink(Gone),
    ?assertEqual({chunk_seen, <<"a">>}, wait_msg()),
    ?assertEqual({closed, normal}, wait_msg()),
    wait_down(Sink),
    ?assertEqual(1, count(close_stream, Calls)).

%% A pool that never answers holds only the announcer: the sink starts
%% and stops within the bound, and the announcer ends once each of its
%% two publishes is let through.
a_sink_whose_pool_never_answers_starts_and_stops_at_once() ->
    Test = self(),
    {Opts, _Calls} = scripted_opts([]),
    Held = Opts#{fact_publish := fun(_Pool, _Realm, Topic, _Payload) ->
                                         Test ! {publish_held, Topic, self()},
                                         receive release -> ok end
                                 end},
    {StartMs, Sink} = timer:tc(fun() -> start_sink(Held) end, millisecond),
    {StopMs, ok} = timer:tc(fun() -> gen_server:stop(Sink) end, millisecond),
    ?assertEqual({closed, normal}, wait_msg()),
    Announcer = release(<<"streaming.started_v1">>),
    Announcer = release(<<"streaming.completed_v1">>),
    wait_down(Announcer),
    ?assert(StartMs < ?BOUND_MS),
    ?assert(StopMs < ?BOUND_MS).

release(Topic) ->
    receive
        {publish_held, Topic, Announcer} ->
            Announcer ! release,
            Announcer
    after 5000 ->
        error({publish_not_held, Topic})
    end.

%% A sink killed before its terminate/2 runs still has its stream's end
%% announced, with outcome failed and the reason it went down for.
a_killed_sinks_end_is_announced_with_the_kill_reason() ->
    process_flag(trap_exit, true),
    {Opts, _Calls} = scripted_opts([]),
    Sink = start_sink(Opts),
    {<<"streaming.started_v1">>, #{stream_id := StreamId}} = next_published(),
    exit(Sink, kill),
    wait_down(Sink),
    ?assertEqual({<<"streaming.completed_v1">>,
                  #{stream_id => StreamId, outcome => failed, reason => <<"killed">>}},
                 next_published()).

%% A stream that fails for a reason carrying data ends with only the
%% reason's name, in its end fact and in the message of its abort.
a_failed_streams_end_fact_and_abort_name_the_reason_only() ->
    process_flag(trap_exit, true),
    {Opts, _Calls} = scripted_opts([{error, {bad_frame, ?MARKER}}]),
    Sink = start_sink(Opts),
    ?assertEqual({closed, {bad_frame, ?MARKER}}, wait_msg()),
    wait_down(Sink),
    ?assertEqual({aborted, <<"cancelled">>, <<"bad_frame">>}, next_aborted()),
    ?assertMatch({<<"streaming.started_v1">>, _}, next_published()),
    {<<"streaming.completed_v1">>, End} = next_published(),
    ?assertEqual(#{outcome => failed, reason => <<"bad_frame">>}, maps:remove(stream_id, End)),
    ?assertEqual(nomatch, binary:match(term_to_binary(End), ?MARKER)).

next_aborted() ->
    receive
        {aborted, _, _} = Aborted -> Aborted
    after 5000 ->
        error(nothing_aborted)
    end.

%% The next message from the sink's callbacks.
wait_msg() ->
    receive
        {chunk_seen, _} = Msg -> Msg;
        {closed, _} = Msg -> Msg
    after 1000 -> timeout
    end.
