%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_pubsub_subscription module.
%%%
%%% Tests subscription management and pattern matching.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_subscription_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% add_subscription/4 Tests
%%%===================================================================

add_subscription_test() ->
    %% GIVEN: Empty subscriptions map
    Subs = #{},
    Topic = <<"test.topic">>,
    Callback = fun(_) -> ok end,
    SubRef = make_ref(),

    %% WHEN: Adding subscription
    {ok, UpdatedSubs, ReturnedRef} = macula_pubsub_subscription:add_subscription(
        Topic, Callback, Subs, SubRef
    ),

    %% THEN: Should be stored with given reference
    ?assertEqual(SubRef, ReturnedRef),
    ?assertMatch(#{SubRef := {<<"test.topic">>, Callback}}, UpdatedSubs).

add_multiple_subscriptions_test() ->
    %% GIVEN: One subscription
    Subs1 = #{},
    Callback = fun(_) -> ok end,
    {ok, Subs2, _} = macula_pubsub_subscription:add_subscription(
        <<"topic1">>, Callback, Subs1, make_ref()
    ),

    %% WHEN: Adding another subscription
    {ok, Subs3, _} = macula_pubsub_subscription:add_subscription(
        <<"topic2">>, Callback, Subs2, make_ref()
    ),

    %% THEN: Both should be stored
    ?assertEqual(2, maps:size(Subs3)).

%%%===================================================================
%%% remove_subscription/2 Tests
%%%===================================================================

remove_subscription_success_test() ->
    %% GIVEN: Subscription exists
    SubRef = make_ref(),
    Topic = <<"remove.topic">>,
    Callback = fun(_) -> ok end,
    Subs = #{SubRef => {Topic, Callback}},

    %% WHEN: Removing subscription
    {ok, UpdatedSubs, ReturnedTopic} = macula_pubsub_subscription:remove_subscription(SubRef, Subs),

    %% THEN: Should be removed and topic returned
    ?assertEqual(Topic, ReturnedTopic),
    ?assertEqual(#{}, UpdatedSubs).

remove_subscription_not_found_test() ->
    %% GIVEN: Empty subscriptions
    Subs = #{},

    %% WHEN: Removing nonexistent subscription
    Result = macula_pubsub_subscription:remove_subscription(make_ref(), Subs),

    %% THEN: Should return error
    ?assertEqual({error, not_found}, Result).

%%%===================================================================
%%% find_matches/3 Tests
%%%===================================================================

find_matches_exact_test() ->
    %% GIVEN: Subscription with exact topic
    SubRef = make_ref(),
    Callback = fun(_) -> ok end,
    Subs = #{SubRef => {<<"sensor.temperature">>, Callback}},
    Config = #{topic_separator => <<".">>,
               topic_wildcard_single => <<"*">>,
               topic_wildcard_multi => <<"**">>},

    %% WHEN: Finding matches for exact topic
    Matches = macula_pubsub_subscription:find_matches(<<"sensor.temperature">>, Subs, Config),

    %% THEN: Should match
    ?assertEqual(1, length(Matches)),
    ?assertMatch([{SubRef, {<<"sensor.temperature">>, Callback}}], Matches).

find_matches_single_wildcard_test() ->
    %% GIVEN: Subscription with single-level wildcard
    SubRef = make_ref(),
    Callback = fun(_) -> ok end,
    Subs = #{SubRef => {<<"sensor.*.value">>, Callback}},
    Config = #{topic_separator => <<".">>,
               topic_wildcard_single => <<"*">>,
               topic_wildcard_multi => <<"**">>},

    %% WHEN: Finding matches for matching topic
    Matches = macula_pubsub_subscription:find_matches(<<"sensor.temperature.value">>, Subs, Config),

    %% THEN: Should match
    ?assertEqual(1, length(Matches)).

find_matches_single_wildcard_no_match_test() ->
    %% GIVEN: Subscription with single-level wildcard
    SubRef = make_ref(),
    Callback = fun(_) -> ok end,
    Subs = #{SubRef => {<<"sensor.*.value">>, Callback}},
    Config = #{topic_separator => <<".">>,
               topic_wildcard_single => <<"*">>,
               topic_wildcard_multi => <<"**">>},

    %% WHEN: Finding matches for non-matching topic (too many levels)
    Matches = macula_pubsub_subscription:find_matches(<<"sensor.room.temperature.value">>, Subs, Config),

    %% THEN: Should not match
    ?assertEqual(0, length(Matches)).

find_matches_multi_wildcard_test() ->
    %% GIVEN: Subscription with multi-level wildcard
    SubRef = make_ref(),
    Callback = fun(_) -> ok end,
    Subs = #{SubRef => {<<"sensor.**">>, Callback}},
    Config = #{topic_separator => <<".">>,
               topic_wildcard_single => <<"*">>,
               topic_wildcard_multi => <<"**">>},

    %% WHEN: Finding matches for any depth
    Matches1 = macula_pubsub_subscription:find_matches(<<"sensor.temperature">>, Subs, Config),
    Matches2 = macula_pubsub_subscription:find_matches(<<"sensor.room.temperature.current">>, Subs, Config),

    %% THEN: Both should match
    ?assertEqual(1, length(Matches1)),
    ?assertEqual(1, length(Matches2)).

find_matches_multiple_subscriptions_test() ->
    %% GIVEN: Multiple subscriptions
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    Ref3 = make_ref(),
    Callback = fun(_) -> ok end,
    Subs = #{
        Ref1 => {<<"sensor.temperature">>, Callback},
        Ref2 => {<<"sensor.*">>, Callback},
        Ref3 => {<<"device.light">>, Callback}
    },
    Config = #{topic_separator => <<".">>,
               topic_wildcard_single => <<"*">>,
               topic_wildcard_multi => <<"**">>},

    %% WHEN: Finding matches
    Matches = macula_pubsub_subscription:find_matches(<<"sensor.temperature">>, Subs, Config),

    %% THEN: Should match both sensor subscriptions (exact and wildcard)
    ?assertEqual(2, length(Matches)).

find_matches_no_matches_test() ->
    %% GIVEN: Subscriptions for different topics
    Subs = #{make_ref() => {<<"sensor.temperature">>, fun(_) -> ok end}},
    Config = #{topic_separator => <<".">>,
               topic_wildcard_single => <<"*">>,
               topic_wildcard_multi => <<"**">>},

    %% WHEN: Finding matches for unrelated topic
    Matches = macula_pubsub_subscription:find_matches(<<"device.light">>, Subs, Config),

    %% THEN: Should have no matches
    ?assertEqual(0, length(Matches)).

%%%===================================================================
%%% invoke_callbacks/4 Tests
%%%===================================================================

invoke_callbacks_empty_test() ->
    %% GIVEN: No matches
    Matches = [],

    %% WHEN: Invoking callbacks
    Result = macula_pubsub_subscription:invoke_callbacks(Matches, <<"topic">>, <<"data">>, <<"node">>),

    %% THEN: Should return ok
    ?assertEqual(ok, Result).

invoke_callbacks_single_test() ->
    %% GIVEN: One matching subscription
    TestPid = self(),
    Callback = fun(Data) ->
        TestPid ! {callback_invoked, Data},
        ok
    end,
    SubRef = make_ref(),
    Matches = [{SubRef, {<<"pattern">>, Callback}}],

    %% WHEN: Invoking callbacks
    ok = macula_pubsub_subscription:invoke_callbacks(
        Matches, <<"topic">>, <<"{\"value\":42}">>, <<"test_node">>
    ),

    %% THEN: Callback should be invoked
    receive
        {callback_invoked, Data} ->
            ?assertMatch(#{topic := <<"topic">>}, Data),
            ?assertMatch(#{matched_pattern := <<"pattern">>}, Data),
            ?assertMatch(#{payload := _}, Data)
    after 1000 ->
        ?assert(false)
    end.

invoke_callbacks_multiple_test() ->
    %% GIVEN: Multiple matching subscriptions
    TestPid = self(),
    Callback1 = fun(_) -> TestPid ! callback1, ok end,
    Callback2 = fun(_) -> TestPid ! callback2, ok end,
    Matches = [
        {make_ref(), {<<"pattern1">>, Callback1}},
        {make_ref(), {<<"pattern2">>, Callback2}}
    ],

    %% WHEN: Invoking callbacks
    ok = macula_pubsub_subscription:invoke_callbacks(Matches, <<"topic">>, <<"data">>, <<"node">>),

    %% THEN: Both callbacks should be invoked
    receive callback1 -> ok after 1000 -> ?assert(false) end,
    receive callback2 -> ok after 1000 -> ?assert(false) end.

invoke_callbacks_exception_test() ->
    %% GIVEN: Callback that throws exception
    Callback = fun(_) -> error(test_error) end,
    Matches = [{make_ref(), {<<"pattern">>, Callback}}],

    %% WHEN: Invoking callbacks (should not crash)
    Result = macula_pubsub_subscription:invoke_callbacks(Matches, <<"topic">>, <<"data">>, <<"node">>),

    %% THEN: Should return ok despite exception
    ?assertEqual(ok, Result),
    timer:sleep(100). % Give spawned process time to crash gracefully
