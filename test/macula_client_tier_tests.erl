%% EUnit tests for the two-tier rules of macula#201 step 1: which links an
%% ORIGINATING operation may use, which tier a pool is on, how the candidates
%% are ordered, and the redial schedule's bounds.
%%
%% These are the design's §9 scenarios where the rule is a pure function; the
%% ones that need a live station (a real publish reaching only public links, a
%% LAN caller resolving through the fallback) stay in the integration suites.
%% Every assertion here is on the RULE, not on a sampled value: the redial test
%% asserts the bound the design states, so it cannot pass by luck.
-module(macula_client_tier_tests).

-include_lib("eunit/include/eunit.hrl").

tier_filter_test_() ->
    [fun an_originating_operation_uses_only_the_active_tier/0,
     fun a_pool_with_no_fallback_list_keeps_every_link/0,
     fun a_pool_on_the_unknown_default_tier_uses_public_links/0].

active_tier_test_() ->
    [fun any_public_link_up_means_public/0,
     fun no_public_link_and_a_fallback_configured_means_fallback/0,
     fun no_public_link_and_no_fallback_configured_stays_public/0].

configuration_test_() ->
    [fun the_two_lists_become_one_seed_list_with_roles/0,
     fun a_station_named_in_both_lists_does_not_start_the_pool/0,
     fun the_same_station_under_two_spellings_is_one_station/0,
     fun an_empty_fallback_list_is_todays_behaviour/0].

ranking_test_() ->
    [fun the_lowest_measured_round_trip_goes_first/0,
     fun an_unmeasured_link_sorts_after_every_measured_one/0,
     fun discovery_never_chooses_random_for_the_caller/0].

redial_test_() ->
    [fun the_first_retry_waits_exactly_the_base/0,
     fun no_retry_waits_below_the_base_or_above_the_cap/0,
     fun every_later_attempt_may_wait_longer_than_the_first/0].

%%--------------------------------------------------------------------
%% §9 scenario 1 and 3, the rule itself
%%--------------------------------------------------------------------

%% A publish, a call, a station procedure and a replication copy all select
%% through this one filter, so an assertion here is an assertion about all four.
an_originating_operation_uses_only_the_active_tier() ->
    Tiers = #{pub_a => public, pub_b => public, lan => fallback},
    ?assertEqual([pub_a, pub_b], macula_client:tier_targets([pub_a, lan, pub_b], Tiers, public)),
    ?assertEqual([lan], macula_client:tier_targets([pub_a, lan, pub_b], Tiers, fallback)).

%% A pool that configured no fallback list has one tier and must behave exactly
%% as it did before the option existed.
a_pool_with_no_fallback_list_keeps_every_link() ->
    Tiers = #{a => public, b => public},
    ?assertEqual([a, b], macula_client:tier_targets([a, b], Tiers, public)).

%% A link the pool never classified (a direct dial, a discovered station) is
%% public, so a caller's ordinary traffic still uses it.
a_pool_on_the_unknown_default_tier_uses_public_links() ->
    ?assertEqual([unclassified], macula_client:tier_targets([unclassified], #{}, public)).

%%--------------------------------------------------------------------
%% §9 scenario 7, and the return on the handshake
%%--------------------------------------------------------------------

any_public_link_up_means_public() ->
    ?assertEqual(public, macula_client:tier_for(true, true)),
    ?assertEqual(public, macula_client:tier_for(true, false)).

no_public_link_and_a_fallback_configured_means_fallback() ->
    ?assertEqual(fallback, macula_client:tier_for(false, true)).

%% With no fallback configured there is only one tier, and "neither tier up"
%% must fail exactly as it always did rather than inventing a fallback.
no_public_link_and_no_fallback_configured_stays_public() ->
    ?assertEqual(public, macula_client:tier_for(false, false)).

%%--------------------------------------------------------------------
%% Configuration: the two lists, and the station named in both
%%--------------------------------------------------------------------

the_two_lists_become_one_seed_list_with_roles() ->
    {ok, Seeds, Roles} = macula_client:tier_roles(["https://a.example:4433"],
                                                  #{fallback_stations => ["https://lan.example:4433"]}),
    ?assertEqual(["https://a.example:4433", "https://lan.example:4433"], Seeds),
    ?assertEqual(public, maps:get("https://a.example:4433", Roles)),
    ?assertEqual(fallback, maps:get("https://lan.example:4433", Roles)).

%% One station holding two roles has no defined behaviour, so the pool does not
%% start and names the seed the caller wrote.
a_station_named_in_both_lists_does_not_start_the_pool() ->
    ?assertEqual({error, {seeds, {station_in_both_tiers, "https://a.example:4433"}}},
                 macula_client:tier_roles(["https://a.example:4433"],
                                          #{fallback_stations => ["https://a.example:4433"]})).

%% The refusal compares stations, not strings: the same host and port written
%% with a different case is one station, which is what the rest of the pool
%% already means by one peer (`normalize_seed/1').
the_same_station_under_two_spellings_is_one_station() ->
    ?assertMatch({error, {seeds, {station_in_both_tiers, _}}},
                 macula_client:tier_roles(["https://a.example:4433"],
                                          #{fallback_stations => ["https://A.Example:4433"]})).

an_empty_fallback_list_is_todays_behaviour() ->
    {ok, Seeds, Roles} = macula_client:tier_roles(["a:4433"], #{}),
    ?assertEqual(["a:4433"], Seeds),
    ?assertEqual(#{ "a:4433" => public}, Roles).

%%--------------------------------------------------------------------
%% §9 scenario 6: the ranking, with and without discovery
%%--------------------------------------------------------------------

the_lowest_measured_round_trip_goes_first() ->
    ?assertEqual([fast, middling, slow],
                 macula_client:rtt_ordered([slow, fast, middling],
                                           #{fast => 1_000, middling => 20_000, slow => 90_000})).

an_unmeasured_link_sorts_after_every_measured_one() ->
    ?assertEqual([measured, unmeasured],
                 macula_client:rtt_ordered([unmeasured, measured], #{measured => 50_000})),
    ?assertEqual([first, second], macula_client:rtt_ordered([first, second], #{})).

%% macula#202: nothing chooses `random' FOR a caller, discovery on or not.
discovery_never_chooses_random_for_the_caller() ->
    ?assertEqual(first_success, macula_client:default_link_selection(undefined)),
    ?assertEqual(first_success, macula_client:default_link_selection(macula_client:discovery_state())).

%%--------------------------------------------------------------------
%% §9 scenario 4: the redial schedule's floor, growth and cap
%%--------------------------------------------------------------------

schedule() ->
    %% The design's defaults, named once here so the assertions read as the rule.
    #{base_ms => 5_000, factor => 2, cap_ms => 300_000}.

%% The floor is exact, not merely a minimum: the first retry after a tier goes
%% down waits the base with no jitter at all.
the_first_retry_waits_exactly_the_base() ->
    #{base_ms := Base} = Schedule = schedule(),
    ?assertEqual(Base, macula_client:redial_wait_ms(0, Schedule)).

%% The bound the design states, asserted as a bound rather than a sampled value:
%% every wait is at or above the floor and below the ceiling that attempt earns.
no_retry_waits_below_the_base_or_above_the_cap() ->
    #{base_ms := Base, factor := Factor, cap_ms := Cap} = Schedule = schedule(),
    lists:foreach(fun(Attempt) ->
                          Wait = macula_client:redial_wait_ms(Attempt, Schedule),
                          Ceiling = min(Cap, Base * trunc(math:pow(Factor, Attempt))),
                          ?assert(Wait >= Base),
                          ?assert(Wait =< Ceiling)
                  end, lists:seq(0, 12)).

%% The schedule actually grows: an attempt well past the first may wait longer
%% than the floor, which is what makes a connect-then-die loop escalate.
every_later_attempt_may_wait_longer_than_the_first() ->
    #{base_ms := Base} = Schedule = schedule(),
    Waits = [macula_client:redial_wait_ms(Attempt, Schedule) || Attempt <- lists:seq(0, 12)],
    ?assertEqual(Base, hd(Waits)),
    ?assert(lists:max(Waits) > Base).
