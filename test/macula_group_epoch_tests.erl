%%%-------------------------------------------------------------------
%%% @doc A sealed group's epochs (plans/DESIGN_E2E_SEALED_PUBSUB.md §4):
%%% independent random keys, random ids, contiguous times, the ahead
%%% window, the publisher's choice of epoch and the subscriber's
%%% acceptance of one.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_group_epoch_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MIN, 60000).
-define(R, (15 * ?MIN)).

an_epoch_carries_a_fresh_key_and_id_test() ->
    E = macula_group_epoch:new(1000, ?R),
    #{id := Id, key := Key} = E,
    ?assertEqual(8, byte_size(Id)),
    ?assertEqual(32, byte_size(Key)),
    #{id := Id2, key := Key2} = macula_group_epoch:new(1000, ?R),
    ?assertNotEqual(Id, Id2),
    ?assertNotEqual(Key, Key2).

an_epoch_publishes_for_rotate_after_and_is_accepted_65_minutes_more_test() ->
    E = macula_group_epoch:new(1000, ?R),
    ?assertMatch(#{issued_at := 1000, publish_until := P, accept_until := A}
                   when P =:= 1000 + ?R andalso A =:= P + 65 * ?MIN, E).

the_next_epoch_starts_where_this_one_stops_publishing_test() ->
    E = macula_group_epoch:new(1000, ?R),
    N = macula_group_epoch:next(E, ?R),
    ?assertEqual(maps:get(publish_until, E), maps:get(issued_at, N)),
    ?assertNotEqual(maps:get(key, E), maps:get(key, N)),
    ?assertNotEqual(maps:get(id, E), maps:get(id, N)).

the_ahead_window_is_the_last_third_before_publish_until_test() ->
    E = macula_group_epoch:new(0, ?R),
    ?assertEqual({?R - ?R div 3, ?R}, macula_group_epoch:ahead_window(E, ?R)),
    ?assertNot(macula_group_epoch:in_ahead_window(E, ?R, ?R - ?R div 3 - 1)),
    ?assert(macula_group_epoch:in_ahead_window(E, ?R, ?R - ?R div 3)),
    ?assert(macula_group_epoch:in_ahead_window(E, ?R, ?R - 1)),
    ?assertNot(macula_group_epoch:in_ahead_window(E, ?R, ?R)).

a_re_pull_falls_inside_the_ahead_window_at_the_given_fraction_test() ->
    E = macula_group_epoch:new(0, ?R),
    {From, To} = macula_group_epoch:ahead_window(E, ?R),
    ?assertEqual(From, macula_group_epoch:repull_at(E, ?R, 0.0)),
    ?assertEqual(From + (To - From) div 2, macula_group_epoch:repull_at(E, ?R, 0.5)),
    ?assert(macula_group_epoch:repull_at(E, ?R, 0.999999) < To).

%% A publisher seals under the newest epoch it holds whose issued_at has passed
%% and whose publish_until has not: e+1 from publish_until(e) on, and nothing
%% once its newest has stopped publishing.
a_publisher_takes_the_newest_epoch_that_has_started_test() ->
    E = macula_group_epoch:new(0, ?R),
    N = macula_group_epoch:next(E, ?R),
    Held = [N, E],
    ?assertEqual({ok, E}, macula_group_epoch:for_publish(Held, ?R - 1)),
    ?assertEqual({ok, N}, macula_group_epoch:for_publish(Held, ?R)),
    ?assertEqual({ok, N}, macula_group_epoch:for_publish(lists:reverse(Held), ?R + 1)).

a_publisher_holding_only_stopped_epochs_has_none_test() ->
    E = macula_group_epoch:new(0, ?R),
    ?assertEqual({error, no_current_epoch}, macula_group_epoch:for_publish([E], ?R)),
    ?assertEqual({error, no_current_epoch}, macula_group_epoch:for_publish([], 0)).

a_publisher_does_not_seal_under_an_epoch_that_has_not_started_test() ->
    E = macula_group_epoch:new(0, ?R),
    N = macula_group_epoch:next(E, ?R),
    ?assertEqual({error, no_current_epoch}, macula_group_epoch:for_publish([N], ?R - 1)).

%% A subscriber accepts an event under an epoch until its accept_until, with the
%% 5-minute clock tolerance a publication gets, and never after.
an_epoch_is_accepted_until_accept_until_plus_the_tolerance_test() ->
    E = macula_group_epoch:new(0, ?R),
    A = maps:get(accept_until, E),
    ?assert(macula_group_epoch:acceptable(E, A)),
    ?assert(macula_group_epoch:acceptable(E, A + 5 * ?MIN)),
    ?assertNot(macula_group_epoch:acceptable(E, A + 5 * ?MIN + 1)).

an_epoch_past_its_acceptance_can_be_erased_test() ->
    E = macula_group_epoch:new(0, ?R),
    A = maps:get(accept_until, E),
    ?assertEqual([], macula_group_epoch:live([E], A + 5 * ?MIN + 1)),
    ?assertEqual([E], macula_group_epoch:live([E], A)).

the_default_rotation_is_15_minutes_test() ->
    ?assertEqual(15 * ?MIN, macula_group_epoch:default_rotate_after_ms()).
