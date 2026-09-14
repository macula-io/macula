%% Tests for the node's count of served stream sessions: the cap per link and
%% node-wide, a place freed when a session's stream ends, and refusals counted
%% by reason and logged once per interval.
-module(macula_stream_sessions_tests).

-include_lib("eunit/include/eunit.hrl").

%% A link past its cap is refused, while another link is still admitted.
a_link_past_its_session_cap_is_refused_test() ->
    with_limits(#{max_served_sessions_per_link => 2}, fun(Base) ->
        Link = park(),
        [S1, S2, S3] = [park() || _ <- lists:seq(1, 3)],
        Other = park(),
        ?assertEqual(ok, macula_stream_sessions:admit(Link, S1)),
        ?assertEqual(ok, macula_stream_sessions:admit(Link, S2)),
        ?assertEqual({error, link_limit}, macula_stream_sessions:admit(Link, S3)),
        ?assertEqual(ok, macula_stream_sessions:admit(Other, S3)),
        end_all([Link, Other, S1, S2, S3], Base)
    end).

%% The node past its cap is refused, whatever link asks.
the_node_past_its_session_cap_is_refused_test() ->
    Base = settled_sessions(),
    with_limits(#{max_served_sessions => Base + 2, max_served_sessions_per_link => 100}, fun(_) ->
        [L1, L2, L3, S1, S2, S3] = [park() || _ <- lists:seq(1, 6)],
        ?assertEqual(ok, macula_stream_sessions:admit(L1, S1)),
        ?assertEqual(ok, macula_stream_sessions:admit(L2, S2)),
        ?assertEqual({error, node_limit}, macula_stream_sessions:admit(L3, S3)),
        end_all([L1, L2, L3, S1, S2, S3], Base)
    end).

%% A session's place frees when its stream process ends, however it ends.
an_ended_session_frees_its_place_test_() ->
    [{Name, fun() -> ended_session_frees_its_place(Exit) end}
     || {Name, Exit} <- [{"the stream ends normally", normal}, {"the stream is killed", kill}]].

ended_session_frees_its_place(Exit) ->
    with_limits(#{max_served_sessions_per_link => 1}, fun(Base) ->
        [Link, S1, S2] = [park() || _ <- lists:seq(1, 3)],
        ok = macula_stream_sessions:admit(Link, S1),
        {error, link_limit} = macula_stream_sessions:admit(Link, S2),
        end_process(S1, Exit),
        ?assertEqual(ok, admitted_within(Link, S2, 1_000)),
        end_all([Link, S2], Base)
    end).

%% Refusals are counted by reason, and however many there are, one warning is
%% logged per interval.
refusals_are_counted_and_logged_once_per_interval_test() ->
    with_limits(#{max_served_sessions_per_link => 1,
                  served_session_refusal_log_interval_ms => 200}, fun(Base) ->
        timer:sleep(250),
        Log = macula_test_log:capture(),
        try
            [Link, S1 | Others] = [park() || _ <- lists:seq(1, 7)],
            ok = macula_stream_sessions:admit(Link, S1),
            Before = maps:get(link_limit, macula_stream_sessions:refusals(), 0),
            [{error, link_limit} = macula_stream_sessions:admit(Link, S) || S <- Others],
            ?assertEqual(Before + 5, maps:get(link_limit, macula_stream_sessions:refusals(), 0)),
            ?assertEqual(1, count_logged(<<"served stream sessions refused">>, 150)),
            end_all([Link, S1 | Others], Base)
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

park() ->
    spawn(fun() -> receive stop -> ok end end).

end_process(Pid, normal) -> Pid ! stop;
end_process(Pid, kill)   -> exit(Pid, kill).

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

admitted_within(Link, Stream, Ms) ->
    admitted(macula_stream_sessions:admit(Link, Stream), Link, Stream, Ms).

admitted(ok, _Link, _Stream, _Ms) -> ok;
admitted(Refused, _Link, _Stream, Ms) when Ms =< 0 -> Refused;
admitted({error, _}, Link, Stream, Ms) ->
    timer:sleep(20),
    admitted_within(Link, Stream, Ms - 20).

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
