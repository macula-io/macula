%% Tests for the node's count of served stream sessions: the cap per verified
%% caller and node-wide, one place per admitted stream, a place freed when a
%% session's stream ends, counts that hold across a restart of the counter,
%% and refusals counted by reason and logged once per interval.
-module(macula_stream_sessions_tests).

-include_lib("eunit/include/eunit.hrl").

%% A caller past its cap is refused, while another caller is still admitted.
a_caller_past_its_session_cap_is_refused_test() ->
    with_limits(#{max_served_sessions_per_caller => 2}, fun(Base) ->
        [S1, S2, S3] = [park() || _ <- lists:seq(1, 3)],
        [Caller, Other] = [caller(), caller()],
        ?assertEqual(ok, macula_stream_sessions:admit(Caller, S1)),
        ?assertEqual(ok, macula_stream_sessions:admit(Caller, S2)),
        ?assertEqual({error, caller_limit}, macula_stream_sessions:admit(Caller, S3)),
        ?assertEqual(ok, macula_stream_sessions:admit(Other, S3)),
        end_all([S1, S2, S3], Base)
    end).

%% The node past its cap is refused, whatever caller asks.
the_node_past_its_session_cap_is_refused_test() ->
    Base = settled_sessions(),
    with_limits(#{max_served_sessions => Base + 2, max_served_sessions_per_caller => 100}, fun(_) ->
        [S1, S2, S3] = [park() || _ <- lists:seq(1, 3)],
        ?assertEqual(ok, macula_stream_sessions:admit(caller(), S1)),
        ?assertEqual(ok, macula_stream_sessions:admit(caller(), S2)),
        ?assertEqual({error, node_limit}, macula_stream_sessions:admit(caller(), S3)),
        end_all([S1, S2, S3], Base)
    end).

%% A stream admitted twice holds one place, and frees it when it ends.
a_stream_admitted_twice_holds_one_place_test() ->
    with_limits(#{max_served_sessions_per_caller => 2}, fun(Base) ->
        [S1, S2, S3] = [park() || _ <- lists:seq(1, 3)],
        Caller = caller(),
        ok = macula_stream_sessions:admit(Caller, S1),
        ?assertEqual(ok, macula_stream_sessions:admit(Caller, S1)),
        ?assertEqual(Base + 1, macula_stream_sessions:sessions()),
        ?assertEqual(ok, macula_stream_sessions:admit(Caller, S2)),
        end_process(S1, kill),
        ?assertEqual(Base + 1, sessions_back_to(Base + 1, 1_000)),
        ?assertEqual(ok, macula_stream_sessions:admit(Caller, S3)),
        end_all([S2, S3], Base)
    end).

%% A session's place frees when its stream process ends, however it ends.
an_ended_session_frees_its_place_test_() ->
    [{Name, fun() -> ended_session_frees_its_place(Exit) end}
     || {Name, Exit} <- [{"the stream ends normally", normal}, {"the stream is killed", kill}]].

ended_session_frees_its_place(Exit) ->
    with_limits(#{max_served_sessions_per_caller => 1}, fun(Base) ->
        [S1, S2] = [park() || _ <- lists:seq(1, 2)],
        Caller = caller(),
        ok = macula_stream_sessions:admit(Caller, S1),
        {error, caller_limit} = macula_stream_sessions:admit(Caller, S2),
        end_process(S1, Exit),
        ?assertEqual(ok, admitted_within(Caller, S2, 1_000)),
        end_all([S2], Base)
    end).

%% The counts outlive the counter: after it restarts, a caller at its cap is
%% still refused, and a session that ends still frees its place.
the_caps_hold_across_a_restart_of_the_counter_test() ->
    with_limits(#{max_served_sessions_per_caller => 2}, fun(Base) ->
        [S1, S2, S3] = [park() || _ <- lists:seq(1, 3)],
        Caller = caller(),
        ok = macula_stream_sessions:admit(Caller, S1),
        ok = macula_stream_sessions:admit(Caller, S2),
        Counter = whereis(macula_stream_sessions),
        exit(Counter, kill),
        ?assertNotEqual(Counter, restarted(macula_stream_sessions, Counter, 2_000)),
        ?assertEqual(Base + 2, macula_stream_sessions:sessions()),
        ?assertEqual({error, caller_limit}, macula_stream_sessions:admit(Caller, S3)),
        end_process(S1, kill),
        ?assertEqual(ok, admitted_within(Caller, S3, 1_000)),
        end_all([S2, S3], Base)
    end).

%% Admission fails closed: with the counter stopped, or held up past the
%% admission timeout, a session is refused as unavailable, and the one who
%% asks waits no longer than that timeout.
admission_without_the_counter_is_refused_test_() ->
    {timeout, 10, [fun admission_with_the_counter_stopped_is_refused/0,
                   fun admission_with_the_counter_held_up_is_refused/0]}.

admission_with_the_counter_stopped_is_refused() ->
    with_limits(#{}, fun(_Base) ->
        Stream = park(),
        ok = supervisor:terminate_child(macula_root, macula_stream_sessions),
        _ = restart_the_counter_later(),
        try
            ?assertEqual({error, unavailable}, macula_stream_sessions:admit(caller(), Stream))
        after
            _ = supervisor:restart_child(macula_root, macula_stream_sessions),
            exit(Stream, kill)
        end
    end).

admission_with_the_counter_held_up_is_refused() ->
    with_limits(#{}, fun(Base) ->
        Stream = park(),
        Counter = whereis(macula_stream_sessions),
        ok = sys:suspend(Counter),
        _ = resume_the_counter_later(Counter),
        try
            {Micros, Refused} = timer:tc(macula_stream_sessions, admit, [caller(), Stream]),
            ?assertEqual({error, unavailable}, Refused),
            ?assert(Micros < 2_000_000)
        after
            ok = sys:resume(Counter),
            end_all([Stream], Base)
        end
    end).

restart_the_counter_later() ->
    recover_later(fun() -> supervisor:restart_child(macula_root, macula_stream_sessions) end).

resume_the_counter_later(Counter) ->
    recover_later(fun() -> catch sys:resume(Counter) end).

%% Runs Recover once this test process ends or three seconds have passed,
%% whichever comes first, so a test cut off with the counter stopped or
%% suspended does not leave it that way for the tests after it.
recover_later(Recover) ->
    Test = self(),
    spawn(fun() -> recover_after(erlang:monitor(process, Test), Recover) end).

recover_after(Ref, Recover) ->
    receive
        {'DOWN', Ref, process, _Test, _Why} -> ok
    after 3_000 ->
        ok
    end,
    Recover().

%% Refusals are counted by reason, and however many there are, one warning is
%% logged per interval.
refusals_are_counted_and_logged_once_per_interval_test() ->
    with_limits(#{max_served_sessions_per_caller => 1,
                  served_session_refusal_log_interval_ms => 200}, fun(Base) ->
        timer:sleep(250),
        Log = macula_test_log:capture(),
        try
            [S1 | Others] = [park() || _ <- lists:seq(1, 6)],
            Caller = caller(),
            ok = macula_stream_sessions:admit(Caller, S1),
            Before = maps:get(caller_limit, macula_stream_sessions:refusals(), 0),
            [{error, caller_limit} = macula_stream_sessions:admit(Caller, S) || S <- Others],
            ?assertEqual(Before + 5, maps:get(caller_limit, macula_stream_sessions:refusals(), 0)),
            ?assertEqual(1, count_logged(<<"served stream sessions refused">>, 150)),
            end_all([S1 | Others], Base)
        after
            macula_test_log:release(Log)
        end
    end).

%% Runs Test with the given macula application env set, handing it the number
%% of sessions counted before, and restores the env after.
with_limits(Limits, Test) ->
    {ok, _} = application:ensure_all_started(macula),
    Old = [{Key, application:get_env(macula, Key)} || Key <- maps:keys(Limits)],
    [ok = application:set_env(macula, Key, Value) || {Key, Value} <- maps:to_list(Limits)],
    try Test(settled_sessions()) after [restore_env(Key, Was) || {Key, Was} <- Old] end.

restore_env(Key, undefined) -> application:unset_env(macula, Key);
restore_env(Key, {ok, Value}) -> application:set_env(macula, Key, Value).

%% The count once no stream process ended moments ago is still being released.
settled_sessions() ->
    {ok, _} = application:ensure_all_started(macula),
    timer:sleep(20),
    macula_stream_sessions:sessions().

caller() ->
    crypto:strong_rand_bytes(32).

park() ->
    spawn(fun() -> receive stop -> ok end end).

end_process(Pid, normal) -> Pid ! stop;
end_process(Pid, kill)   -> exit(Pid, kill).

%% The pid registered as Name once it is no longer Old.
restarted(Name, Old, Ms) ->
    restarted_as(whereis(Name), Name, Old, Ms).

restarted_as(Pid, _Name, Old, _Ms) when is_pid(Pid), Pid =/= Old -> Pid;
restarted_as(Pid, _Name, _Old, Ms) when Ms =< 0 -> Pid;
restarted_as(_Pid, Name, Old, Ms) ->
    timer:sleep(20),
    restarted(Name, Old, Ms - 20).

%% Ends the given processes and waits until the count is back at Base.
end_all(Pids, Base) ->
    [exit(Pid, kill) || Pid <- Pids],
    ?assertEqual(Base, sessions_back_to(Base, 1_000)).

sessions_back_to(Base, Ms) ->
    back_to(macula_stream_sessions:sessions(), Base, Ms).

back_to(Base, Base, _Ms) -> Base;
back_to(Count, _Base, Ms) when Ms =< 0 -> Count;
back_to(_Count, Base, Ms) ->
    timer:sleep(20),
    sessions_back_to(Base, Ms - 20).

admitted_within(Caller, Stream, Ms) ->
    admitted(macula_stream_sessions:admit(Caller, Stream), Caller, Stream, Ms).

admitted(ok, _Caller, _Stream, _Ms) -> ok;
admitted(Refused, _Caller, _Stream, Ms) when Ms =< 0 -> Refused;
admitted({error, _}, Caller, Stream, Ms) ->
    timer:sleep(20),
    admitted_within(Caller, Stream, Ms - 20).

count_logged(Part, Ms) ->
    count_logged(Part, erlang:monotonic_time(millisecond) + Ms, 0).

count_logged(Part, Deadline, Count) ->
    Left = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {macula_test_log, Text} -> count_logged(Part, Deadline, Count + matched(binary:match(Text, Part)))
    after Left ->
        Count
    end.

matched(nomatch) -> 0;
matched(_Found)  -> 1.
