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

%% A refusal can name where it came from. A report lists the sources since the last report, most refused first, so
%% an operator reading a once-a-minute line sees who is being refused, not only how often (macula#34).
a_report_names_its_sources_most_refused_first_test() ->
    ?assertMatch({report, 1, [{a, 1}], _}, macula_refusal_report:refused(report(), caller_quota, a, ?NOW)),
    {report, 1, _, R1} = macula_refusal_report:refused(report(), caller_quota, a, ?NOW),
    R2 = lists:foldl(fun({Who, Offset}, Acc) ->
                             quiet_of(macula_refusal_report:refused(Acc, caller_quota, Who, ?NOW + Offset))
                     end, R1, [{b, 1}, {c, 2}, {c, 3}, {b, 4}, {c, 5}]),
    ?assertEqual({report, 6, [{c, 3}, {b, 2}, {a, 1}]},
                 without_report(macula_refusal_report:refused(R2, caller_quota, a, ?NOW + ?WINDOW))).

%% A report carries its top sources only, and a window keeps a bounded number of distinct sources: the rest are
%% counted together under `other', so a caller minting identities cannot grow the report without bound.
the_sources_of_a_window_are_bounded_test() ->
    {report, 1, _, R1} = macula_refusal_report:refused(report(), caller_quota, first, ?NOW),
    R2 = lists:foldl(fun(N, Acc) -> quiet_of(macula_refusal_report:refused(Acc, caller_quota, N, ?NOW + 1)) end,
                     R1, lists:seq(1, 1000)),
    {report, 1001, Sources, _} = macula_refusal_report:refused(R2, caller_quota, last, ?NOW + ?WINDOW),
    ?assert(length(Sources) =< 5),
    ?assertMatch({other, N} when N >= 1000 - 64, lists:keyfind(other, 1, Sources)).

%% The latest report's sources stay readable by kind, for status and health.
the_latest_sources_are_kept_by_kind_test() ->
    {report, 1, _, R1} = macula_refusal_report:refused(report(), caller_quota, a, ?NOW),
    {report, 1, _, R2} = macula_refusal_report:refused(R1, share_full, b, ?NOW),
    ?assertEqual(#{caller_quota => [{a, 1}], share_full => [{b, 1}]}, macula_refusal_report:last_sources(R2)).

a_window_that_is_not_positive_is_refused_test() ->
    ?assertError(function_clause, macula_refusal_report:new(0)).

%%---------------------------------------------------------------------
%% Helpers
%%---------------------------------------------------------------------

report() ->
    macula_refusal_report:new(?WINDOW).

reported({report, _Count, R}, Seen) -> {Seen + 1, R};
reported({quiet, R}, Seen)          -> {Seen, R}.

quiet_of({quiet, R}) -> R.

without_report({report, Count, Sources, _R}) -> {report, Count, Sources}.

report_of({report, _Count, R}) -> R;
report_of({quiet, R})          -> R.
