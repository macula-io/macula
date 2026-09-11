%% EUnit tests for macula_diagnostics_bound and macula_diagnostics:bounded_event/3: a diagnostics event is logged at
%% most once per 10-second window per event name, node-wide, and the next line carries the latest properties with the
%% count held back. Callers update the counts table themselves, so a burst never queues on its owner.
-module(macula_diagnostics_bound_tests).

-include_lib("eunit/include/eunit.hrl").

-export([log/2]).

-define(TABLE, macula_diagnostics_bound_test).
-define(T0, 1_000_000).
-define(WINDOW, 10_000).
-define(PREFIX, "_macula.test.bounded").
-define(HANDLER, macula_diagnostics_bound_test_handler).

bound_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun the_first_event_of_a_name_is_logged_with_nothing_suppressed/1,
      fun an_event_inside_the_window_is_counted_and_not_logged/1,
      fun the_first_event_after_the_window_is_logged_with_the_count_held_back/1,
      fun two_names_keep_separate_windows/1,
      fun the_sweep_logs_the_count_a_burst_left_with_the_latest_properties/1,
      fun the_sweep_logs_nothing_for_a_window_with_nothing_held_back/1,
      fun many_concurrent_callers_in_one_window_log_one_line_and_the_right_count/1,
      fun a_burst_sends_the_owner_no_message/1,
      fun without_its_table_an_event_is_logged_as_it_is/1]}.

bounded_event_test_() ->
    {setup, fun started/0, fun(_) -> ok end,
     fun(_) -> [{"bounded_event/3 logs a name once per window through the application's table",
                 fun bounded_event_logs_once_per_window_through_the_application_table/0}] end}.

%%------------------------------------------------------------------
%% Cases
%%------------------------------------------------------------------

the_first_event_of_a_name_is_logged_with_nothing_suppressed(#{tab := Tab}) ->
    fun() ->
        _ = capture(),
        _ = owner(Tab),
        ok = event(<<?PREFIX ".a">>, #{n => 1}),
        ?assertEqual([{<<?PREFIX ".a">>, #{n => 1, suppressed => 0}}], logged())
    end.

an_event_inside_the_window_is_counted_and_not_logged(#{tab := Tab}) ->
    fun() ->
        _ = capture(),
        _ = owner(Tab),
        ok = event(<<?PREFIX ".a">>, #{n => 1}),
        set_time(Tab, ?T0 + ?WINDOW - 1),
        ok = event(<<?PREFIX ".a">>, #{n => 2}),
        ?assertEqual([{<<?PREFIX ".a">>, #{n => 1, suppressed => 0}}], logged())
    end.

the_first_event_after_the_window_is_logged_with_the_count_held_back(#{tab := Tab}) ->
    fun() ->
        _ = capture(),
        _ = owner(Tab),
        ok = event(<<?PREFIX ".a">>, #{n => 1}),
        ok = event(<<?PREFIX ".a">>, #{n => 2}),
        ok = event(<<?PREFIX ".a">>, #{n => 3}),
        set_time(Tab, ?T0 + ?WINDOW),
        ok = event(<<?PREFIX ".a">>, #{n => 4}),
        ?assertEqual([{<<?PREFIX ".a">>, #{n => 1, suppressed => 0}}, {<<?PREFIX ".a">>, #{n => 4, suppressed => 2}}],
                     logged())
    end.

two_names_keep_separate_windows(#{tab := Tab}) ->
    fun() ->
        _ = capture(),
        _ = owner(Tab),
        ok = event(<<?PREFIX ".a">>, #{n => 1}),
        ok = event(<<?PREFIX ".b">>, #{n => 1}),
        ok = event(<<?PREFIX ".a">>, #{n => 2}),
        ?assertEqual([{<<?PREFIX ".a">>, #{n => 1, suppressed => 0}}, {<<?PREFIX ".b">>, #{n => 1, suppressed => 0}}],
                     logged())
    end.

the_sweep_logs_the_count_a_burst_left_with_the_latest_properties(#{tab := Tab}) ->
    fun() ->
        _ = capture(),
        Owner = owner(Tab),
        ok = event(<<?PREFIX ".a">>, #{n => 1}),
        ok = event(<<?PREFIX ".a">>, #{n => 2}),
        ok = event(<<?PREFIX ".a">>, #{n => 3}),
        ok = macula_diagnostics_bound:sweep(Owner),
        set_time(Tab, ?T0 + ?WINDOW),
        ok = macula_diagnostics_bound:sweep(Owner),
        ok = macula_diagnostics_bound:sweep(Owner),
        set_time(Tab, ?T0 + ?WINDOW + 1),
        ok = event(<<?PREFIX ".a">>, #{n => 4}),
        ?assertEqual([{<<?PREFIX ".a">>, #{n => 1, suppressed => 0}}, {<<?PREFIX ".a">>, #{n => 3, suppressed => 2}}],
                     logged())
    end.

the_sweep_logs_nothing_for_a_window_with_nothing_held_back(#{tab := Tab}) ->
    fun() ->
        _ = capture(),
        Owner = owner(Tab),
        ok = event(<<?PREFIX ".a">>, #{n => 1}),
        set_time(Tab, ?T0 + 3 * ?WINDOW),
        ok = macula_diagnostics_bound:sweep(Owner),
        ?assertEqual([{<<?PREFIX ".a">>, #{n => 1, suppressed => 0}}], logged())
    end.

%% Every caller runs at once inside one window: one line, and the sweep after the window reports the others.
many_concurrent_callers_in_one_window_log_one_line_and_the_right_count(#{tab := Tab}) ->
    fun() ->
        _ = capture(),
        Owner = owner(Tab),
        Callers = 200,
        Test = self(),
        Pids = [spawn_monitor(fun() -> receive go -> ok = event(<<?PREFIX ".a">>, #{n => I}), Test ! done end end)
                || I <- lists:seq(1, Callers)],
        _ = [Pid ! go || {Pid, _Ref} <- Pids],
        _ = [receive {'DOWN', Ref, process, Pid, normal} -> ok after 5_000 -> erlang:error(caller_stuck) end
             || {Pid, Ref} <- Pids],
        [{<<?PREFIX ".a">>, #{suppressed := 0}}] = logged(),
        set_time(Tab, ?T0 + ?WINDOW),
        ok = macula_diagnostics_bound:sweep(Owner),
        ?assertMatch([{<<?PREFIX ".a">>, #{suppressed := 199}}], logged()),
        ?assertEqual(Callers, drained(done, 0))
    end.

%% The owner is traced for received messages through the burst: none arrives. Its sweep is manual here, so no timer
%% message arrives either.
a_burst_sends_the_owner_no_message(#{tab := Tab}) ->
    fun() ->
        _ = capture(),
        Owner = owner(Tab),
        1 = erlang:trace(Owner, true, ['receive', {tracer, self()}]),
        _ = [ok = event(<<?PREFIX ".a">>, #{n => I}) || I <- lists:seq(1, 1_000)],
        1 = erlang:trace(Owner, false, ['receive']),
        ?assertEqual([], received_by(Owner)),
        ?assertMatch([_], logged())
    end.

without_its_table_an_event_is_logged_as_it_is(_World) ->
    fun() ->
        _ = capture(),
        ok = macula_diagnostics_bound:event(macula_diagnostics_bound_no_table, warning, <<?PREFIX ".a">>, #{n => 1}),
        ok = macula_diagnostics_bound:event(macula_diagnostics_bound_no_table, warning, <<?PREFIX ".a">>, #{n => 2}),
        ?assertEqual([{<<?PREFIX ".a">>, #{n => 1}}, {<<?PREFIX ".a">>, #{n => 2}}], logged())
    end.

bounded_event_logs_once_per_window_through_the_application_table() ->
    Handler = capture(),
    Topic = <<?PREFIX ".application.", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    ok = macula_diagnostics:bounded_event(warning, Topic, #{n => 1}),
    ok = macula_diagnostics:bounded_event(warning, Topic, #{n => 2}),
    Logged = logged(),
    ok = logger:remove_handler(Handler),
    ?assertEqual([{Topic, #{n => 1, suppressed => 0}}], Logged).

%%------------------------------------------------------------------
%% Fixture
%%------------------------------------------------------------------

%% Each case adds the log handler and starts the table's owner itself: eunit runs setup in another process than the
%% case, and a missing owner then fails the case rather than the fixture.
setup() ->
    Tab = ets:new(bound_test_clock, [public, set]),
    set_time(Tab, ?T0),
    #{tab => Tab}.

teardown(#{tab := Tab}) ->
    _ = logger:remove_handler(?HANDLER),
    ok = stopped(ets:info(?TABLE, owner)),
    true = ets:delete(Tab),
    _ = logged(),
    ok.

owner(Tab) ->
    {ok, Owner} = macula_diagnostics_bound:start_link(#{table => ?TABLE, window_ms => ?WINDOW, sweep => manual,
                                                         clock => fun() -> ets:lookup_element(Tab, now, 2) end}),
    Owner.

stopped(undefined) -> ok;
stopped(Owner) -> gen_server:stop(Owner).

started() ->
    {ok, _} = application:ensure_all_started(macula),
    ok.

event(Topic, Properties) ->
    macula_diagnostics_bound:event(?TABLE, warning, Topic, Properties).

set_time(Tab, Ms) ->
    true = ets:insert(Tab, {now, Ms}).

capture() ->
    ok = logger:add_handler(?HANDLER, ?MODULE, #{config => #{test => self()}, level => all, filter_default => log}),
    ?HANDLER.

%% The logger handler that forwards this module's test events to the test process.
log(#{msg := {report, #{event := <<?PREFIX, _/binary>> = Topic, properties := Properties}}},
    #{config := #{test := Test}}) ->
    Test ! {logged, Topic, Properties};
log(_Event, _Config) ->
    ok.

%% The lines logged so far, in order.
logged() ->
    receive
        {logged, Topic, Properties} -> [{Topic, Properties} | logged()]
    after 200 ->
        []
    end.

%% The messages the traced owner received.
received_by(Owner) ->
    receive
        {trace, Owner, 'receive', Message} -> [Message | received_by(Owner)]
    after 200 ->
        []
    end.

drained(Message, Count) ->
    receive
        Message -> drained(Message, Count + 1)
    after 0 ->
        Count
    end.
