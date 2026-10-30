%% macula-station#129, phase 1: which links carry a subscription, and the
%% interruption and gap its subscribers are told when none does. Pure state,
%% no links: link pids are stand-ins.
-module(macula_client_coverage_tests).

-include_lib("eunit/include/eunit.hrl").

-define(KEY, {<<7:256>>, <<"acme.counted">>}).
-define(OTHER, {<<7:256>>, <<"acme.other">>}).

links() -> {spawn(fun() -> ok end), spawn(fun() -> ok end)}.

accepted(Link, AtMs, C) ->
    {none, C1} = macula_client_coverage:accepted(?KEY, Link, AtMs, C),
    C1.

the_last_link_ending_interrupts_from_when_it_was_last_heard_test() ->
    {A, _} = links(),
    C1 = macula_client_coverage:ending(A, <<"app_liveness_lost">>, 150, accepted(A, 100, macula_client_coverage:new())),
    ?assertMatch({[{?KEY, #{from_ms := 150, reason := <<"app_liveness_lost">>}}], _},
                 macula_client_coverage:link_ended(A, <<"normal">>, C1)).

a_link_that_gave_no_reason_is_named_by_its_exit_test() ->
    {A, _} = links(),
    ?assertMatch({[{?KEY, #{from_ms := 100, reason := <<"killed">>}}], _},
                 macula_client_coverage:link_ended(A, <<"killed">>, accepted(A, 100, macula_client_coverage:new()))).

another_link_still_carrying_means_nothing_was_missed_test() ->
    {A, B} = links(),
    C = accepted(B, 110, accepted(A, 100, macula_client_coverage:new())),
    ?assertMatch({[], _}, macula_client_coverage:link_ended(A, <<"closed">>, C)).

the_interruption_starts_at_the_latest_time_any_carrying_link_was_heard_test() ->
    {A, B} = links(),
    C0 = accepted(B, 110, accepted(A, 100, macula_client_coverage:new())),
    C1 = macula_client_coverage:ending(A, <<"closed">>, 500, C0),
    {[], C2} = macula_client_coverage:link_ended(A, <<"normal">>, C1),
    C3 = macula_client_coverage:ending(B, <<"app_liveness_lost">>, 300, C2),
    ?assertMatch({[{?KEY, #{from_ms := 500, reason := <<"app_liveness_lost">>}}], _},
                 macula_client_coverage:link_ended(B, <<"normal">>, C3)).

a_station_accepting_again_ends_the_interruption_with_one_gap_test() ->
    {A, B} = links(),
    {[_], C1} = macula_client_coverage:link_ended(A, <<"closed">>, accepted(A, 100, macula_client_coverage:new())),
    {Gap, C2} = macula_client_coverage:accepted(?KEY, B, 900, C1),
    ?assertEqual(#{from_ms => 100, to_ms => 900, reason => <<"closed">>}, Gap),
    ?assertMatch({none, _}, macula_client_coverage:accepted(?KEY, B, 950, C2)).

a_gap_never_comes_without_an_interruption_test() ->
    {A, B} = links(),
    ?assertMatch({none, _}, macula_client_coverage:accepted(?KEY, B, 110, accepted(A, 100, macula_client_coverage:new()))).

a_link_that_never_carried_the_subscription_interrupts_nothing_test() ->
    {A, B} = links(),
    C = macula_client_coverage:heard(B, 120, accepted(A, 100, macula_client_coverage:new())),
    ?assertMatch({[], _}, macula_client_coverage:link_ended(B, <<"closed">>, C)).

a_refusal_from_the_only_carrying_link_interrupts_test() ->
    {A, _} = links(),
    ?assertMatch({#{from_ms := 100, reason := <<"not_authorized">>}, _},
                 macula_client_coverage:refused(?KEY, A, <<"not_authorized">>, accepted(A, 100, macula_client_coverage:new()))).

a_refusal_from_a_link_that_never_carried_it_interrupts_nothing_test() ->
    {A, B} = links(),
    C = macula_client_coverage:heard(B, 105, accepted(A, 100, macula_client_coverage:new())),
    ?assertMatch({none, _}, macula_client_coverage:refused(?KEY, B, <<"refused">>, C)).

a_refusal_for_a_subscription_never_carried_interrupts_nothing_test() ->
    {A, _} = links(),
    ?assertMatch({none, _}, macula_client_coverage:refused(?OTHER, A, <<"refused">>, accepted(A, 100, macula_client_coverage:new()))).

a_withdrawn_subscription_gets_no_gap_test() ->
    {A, B} = links(),
    {[_], C1} = macula_client_coverage:link_ended(A, <<"closed">>, accepted(A, 100, macula_client_coverage:new())),
    C2 = macula_client_coverage:withdrawn(?KEY, C1),
    ?assertMatch({none, _}, macula_client_coverage:accepted(?KEY, B, 900, C2)).

one_link_ending_interrupts_each_subscription_it_was_last_to_carry_test() ->
    {A, B} = links(),
    C0 = accepted(A, 100, macula_client_coverage:new()),
    {none, C1} = macula_client_coverage:accepted(?OTHER, A, 100, C0),
    {none, C2} = macula_client_coverage:accepted(?OTHER, B, 100, C1),
    {Interrupted, _} = macula_client_coverage:link_ended(A, <<"closed">>, C2),
    ?assertEqual([?KEY], [Key || {Key, _} <- Interrupted]).

an_earlier_time_heard_changes_nothing_test() ->
    {A, _} = links(),
    C = macula_client_coverage:heard(A, 50, accepted(A, 100, macula_client_coverage:new())),
    ?assertMatch({[{?KEY, #{from_ms := 100}}], _}, macula_client_coverage:link_ended(A, <<"closed">>, C)).
