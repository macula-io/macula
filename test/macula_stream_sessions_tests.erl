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

%% A served stream charges the bytes it keeps unread to its caller and the
%% node. A charge that would take its caller past the caller's budget is
%% refused, while another caller's charge is still taken.
a_caller_past_its_inbox_budget_is_refused_test() ->
    with_limits(#{max_served_inbox_bytes_per_caller => 1000,
                  max_served_inbox_bytes => 1000000}, fun(Base) ->
        [S1, S2, S3] = [park() || _ <- lists:seq(1, 3)],
        [Caller, Other] = [caller(), caller()],
        [ok = macula_stream_sessions:admit(C, S) || {C, S} <- [{Caller, S1}, {Caller, S2}, {Other, S3}]],
        Bytes = macula_stream_sessions:inbox_bytes(),
        ?assertEqual(ok, charge_as(S1, 600)),
        ?assertEqual({error, caller_budget}, charge_as(S2, 600)),
        ?assertEqual(ok, charge_as(S3, 600)),
        ?assertEqual(Bytes + 1200, macula_stream_sessions:inbox_bytes()),
        end_all([S1, S2, S3], Base),
        ?assertEqual(Bytes, inbox_bytes_back_to(Bytes, 1_000))
    end).

%% A charge that would take the node past its budget is refused whatever
%% caller asks, and the caller's share of that charge is taken back: once the
%% node has room, the same caller's charge up to its own budget is taken.
the_node_past_its_inbox_budget_is_refused_test() ->
    Bytes = settled_inbox_bytes(),
    with_limits(#{max_served_inbox_bytes_per_caller => 1000,
                  max_served_inbox_bytes => Bytes + 1000}, fun(Base) ->
        [S1, S2] = [park() || _ <- lists:seq(1, 2)],
        [Caller, Other] = [caller(), caller()],
        ok = macula_stream_sessions:admit(Caller, S1),
        ok = macula_stream_sessions:admit(Other, S2),
        ?assertEqual(ok, charge_as(S1, 700)),
        ?assertEqual({error, node_budget}, charge_as(S2, 700)),
        ok = release_as(S1, 700),
        ?assertEqual(ok, charge_as(S2, 1000)),
        end_all([S1, S2], Base)
    end).

%% Bytes a stream gives back when a reader takes them make room again.
a_released_charge_makes_room_test() ->
    with_limits(#{max_served_inbox_bytes_per_caller => 1000,
                  max_served_inbox_bytes => 1000000}, fun(Base) ->
        Stream = park(),
        ok = macula_stream_sessions:admit(caller(), Stream),
        ok = charge_as(Stream, 1000),
        {error, caller_budget} = charge_as(Stream, 1),
        ok = release_as(Stream, 400),
        ?assertEqual(ok, charge_as(Stream, 400)),
        end_all([Stream], Base)
    end).

%% What a stream still has charged comes back when its process ends, however
%% it ends.
an_ended_streams_charge_comes_back_test_() ->
    [{Name, fun() -> ended_streams_charge_comes_back(Exit) end}
     || {Name, Exit} <- [{"the stream ends normally", normal}, {"the stream is killed", kill}]].

ended_streams_charge_comes_back(Exit) ->
    with_limits(#{max_served_inbox_bytes_per_caller => 1000,
                  max_served_inbox_bytes => 1000000}, fun(Base) ->
        Bytes = macula_stream_sessions:inbox_bytes(),
        [S1, S2] = [park() || _ <- lists:seq(1, 2)],
        Caller = caller(),
        ok = macula_stream_sessions:admit(Caller, S1),
        ok = macula_stream_sessions:admit(Caller, S2),
        ok = charge_as(S1, 900),
        {error, caller_budget} = charge_as(S2, 900),
        end_process(S1, Exit),
        ?assertEqual(ok, charged_within(S2, 900, 1_000)),
        end_all([S2], Base),
        ?assertEqual(Bytes, inbox_bytes_back_to(Bytes, 1_000))
    end).

%% Only an admitted stream is charged.
a_stream_not_admitted_is_not_charged_test() ->
    with_limits(#{}, fun(_Base) ->
        Stream = park(),
        try
            ?assertEqual({error, not_admitted}, charge_as(Stream, 10))
        after
            exit(Stream, kill)
        end
    end).

%% The charges outlive the counter: after it restarts, a caller at its budget
%% is still refused, and a stream that ends still gives its charge back.
the_charges_hold_across_a_restart_of_the_counter_test() ->
    with_limits(#{max_served_inbox_bytes_per_caller => 1000,
                  max_served_inbox_bytes => 1000000}, fun(Base) ->
        Bytes = macula_stream_sessions:inbox_bytes(),
        [S1, S2] = [park() || _ <- lists:seq(1, 2)],
        Caller = caller(),
        ok = macula_stream_sessions:admit(Caller, S1),
        ok = macula_stream_sessions:admit(Caller, S2),
        ok = charge_as(S1, 900),
        Counter = whereis(macula_stream_sessions),
        exit(Counter, kill),
        ?assertNotEqual(Counter, restarted(macula_stream_sessions, Counter, 2_000)),
        ?assertEqual(Bytes + 900, macula_stream_sessions:inbox_bytes()),
        ?assertEqual({error, caller_budget}, charge_as(S2, 900)),
        end_process(S1, kill),
        ?assertEqual(ok, charged_within(S2, 900, 1_000)),
        end_all([S2], Base),
        ?assertEqual(Bytes, inbox_bytes_back_to(Bytes, 1_000))
    end).

%% A caller that holds no session holds no bytes: whatever its streams left
%% counted is cleared when its last session ends, inside the counter's
%% handling of that end. A session the same caller opens right after then
%% counts for exactly what it charges.
a_caller_with_no_session_holds_no_bytes_test() ->
    with_limits(#{max_served_inbox_bytes_per_caller => 1000,
                  max_served_inbox_bytes => 1000000}, fun(Base) ->
        [S1, S2] = [park() || _ <- lists:seq(1, 2)],
        Caller = caller(),
        ok = macula_stream_sessions:admit(Caller, S1),
        ok = charge_as(S1, 500),
        _ = ets:update_counter(macula_stream_sessions, {caller_bytes, Caller}, {2, 500}),
        end_all([S1], Base),
        ok = macula_stream_sessions:admit(Caller, S2),
        ok = charge_as(S2, 300),
        ?assertEqual(300, ets:lookup_element(macula_stream_sessions, {caller_bytes, Caller}, 2)),
        end_all([S2], Base)
    end).

%% A node that holds no session holds no bytes: whatever its streams left
%% counted is cleared when its last session ends. The node must hold no other
%% session when this runs.
the_node_with_no_session_holds_no_bytes_test() ->
    with_limits(#{max_served_inbox_bytes_per_caller => 1000,
                  max_served_inbox_bytes => 1000000}, fun(Base) ->
        ?assertEqual(0, Base),
        Stream = park(),
        ok = macula_stream_sessions:admit(caller(), Stream),
        ok = charge_as(Stream, 500),
        _ = ets:update_counter(macula_stream_sessions, inbox_bytes, {2, 500}),
        end_all([Stream], Base),
        ?assertEqual(0, macula_stream_sessions:inbox_bytes())
    end).

%% A refused charge is counted by its reason.
refused_charges_are_counted_test() ->
    with_limits(#{max_served_inbox_bytes_per_caller => 10,
                  max_served_inbox_bytes => 1000000}, fun(Base) ->
        Stream = park(),
        ok = macula_stream_sessions:admit(caller(), Stream),
        Before = maps:get(caller_budget, macula_stream_sessions:refusals(), 0),
        {error, caller_budget} = charge_as(Stream, 11),
        ?assertEqual(Before + 1, refusals_reach(caller_budget, Before + 1, 1_000)),
        end_all([Stream], Base)
    end).

%% A charge or release runs in the stream's own process, as a stream makes it.
charge_as(Stream, Bytes) ->
    as(Stream, fun() -> macula_stream_sessions:charge(Stream, Bytes) end).

release_as(Stream, Bytes) ->
    as(Stream, fun() -> macula_stream_sessions:release(Stream, Bytes) end).

as(Stream, Fun) ->
    Stream ! {run, self(), Fun},
    receive
        {ran, Stream, Result} -> Result
    after 1_000 ->
        erlang:error(stream_did_not_run)
    end.

charged_within(Stream, Bytes, Ms) ->
    charged(charge_as(Stream, Bytes), Stream, Bytes, Ms).

charged(ok, _Stream, _Bytes, _Ms) -> ok;
charged(Refused, _Stream, _Bytes, Ms) when Ms =< 0 -> Refused;
charged({error, _}, Stream, Bytes, Ms) ->
    timer:sleep(20),
    charged_within(Stream, Bytes, Ms - 20).

settled_inbox_bytes() ->
    {ok, _} = application:ensure_all_started(macula),
    timer:sleep(20),
    macula_stream_sessions:inbox_bytes().

inbox_bytes_back_to(Bytes, Ms) ->
    bytes_back_to(macula_stream_sessions:inbox_bytes(), Bytes, Ms).

bytes_back_to(Bytes, Bytes, _Ms) -> Bytes;
bytes_back_to(Now, _Bytes, Ms) when Ms =< 0 -> Now;
bytes_back_to(_Now, Bytes, Ms) ->
    timer:sleep(20),
    inbox_bytes_back_to(Bytes, Ms - 20).

refusals_reach(Reason, Count, Ms) ->
    reached(maps:get(Reason, macula_stream_sessions:refusals(), 0), Reason, Count, Ms).

reached(Count, _Reason, Count, _Ms) -> Count;
reached(Now, _Reason, _Count, Ms) when Ms =< 0 -> Now;
reached(_Now, Reason, Count, Ms) ->
    timer:sleep(20),
    refusals_reach(Reason, Count, Ms - 20).

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
    spawn(fun parked/0).

%% A stand-in for a stream process: it runs what it is handed in its own
%% process, as a stream charges and releases, until it is told to stop.
parked() ->
    receive
        {run, From, Fun} -> From ! {ran, self(), Fun()}, parked();
        stop -> ok
    end.

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
