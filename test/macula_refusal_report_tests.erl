%% EUnit tests for `macula_refusal_report'. Every refusal is counted by kind; each kind is reported at most once per
%% window, with the count since its last report, so a flood of refusals costs one report per window.
-module(macula_refusal_report_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NOW, 1789000000000).
-define(WINDOW, 60000).

the_first_refusal_of_a_kind_is_reported_test() ->
    ?assertMatch({report, 1, _}, macula_refusal_report:refused(report(), share_full, ?NOW)).

refusals_inside_the_window_are_counted_but_not_reported_test() ->
    {report, 1, R1} = macula_refusal_report:refused(report(), share_full, ?NOW),
    {quiet, R2} = macula_refusal_report:refused(R1, share_full, ?NOW + 1),
    {quiet, R3} = macula_refusal_report:refused(R2, share_full, ?NOW + ?WINDOW - 1),
    ?assertEqual(#{share_full => 3}, macula_refusal_report:counts(R3)).

the_first_refusal_after_the_window_reports_every_refusal_since_the_last_report_test() ->
    R = lists:foldl(fun(Offset, Acc) ->
                            report_of(macula_refusal_report:refused(Acc, share_full, ?NOW + Offset))
                    end, report(), [0, 10, 20, 30]),
    ?assertMatch({report, 4, _}, macula_refusal_report:refused(R, share_full, ?NOW + ?WINDOW)).

each_kind_is_reported_apart_test() ->
    {report, 1, R1} = macula_refusal_report:refused(report(), share_full, ?NOW),
    ?assertMatch({report, 1, _}, macula_refusal_report:refused(R1, caller_quota, ?NOW + 1)).

%% A flood of refusals in one window gives one report, and its counts cover all of them.
a_flood_in_one_window_gives_one_report_and_counts_it_all_test() ->
    {Reports, R} = lists:foldl(fun(N, {Seen, Acc}) ->
                                       reported(macula_refusal_report:refused(Acc, admission_full, ?NOW + N), Seen)
                               end, {0, report()}, lists:seq(1, 1000)),
    ?assertEqual(1, Reports),
    ?assertEqual(#{admission_full => 1000}, macula_refusal_report:counts(R)).

a_window_that_is_not_positive_is_refused_test() ->
    ?assertError(function_clause, macula_refusal_report:new(0)).

%%---------------------------------------------------------------------
%% Helpers
%%---------------------------------------------------------------------

report() ->
    macula_refusal_report:new(?WINDOW).

reported({report, _Count, R}, Seen) -> {Seen + 1, R};
reported({quiet, R}, Seen)          -> {Seen, R}.

report_of({report, _Count, R}) -> R;
report_of({quiet, R})          -> R.
