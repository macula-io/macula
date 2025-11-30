%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_pubsub_registry module.
%%%
%%% Tests local subscription registry for pub/sub.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Registry Creation Tests
%%%===================================================================

new_registry_test() ->
    %% GIVEN/WHEN: Creating a new registry
    Registry = macula_pubsub_registry:new(),

    %% THEN: Should be empty
    ?assertEqual(0, macula_pubsub_registry:size(Registry)),
    ?assertEqual([], macula_pubsub_registry:list_patterns(Registry)).

%%%===================================================================
%%% Subscribe Tests
%%%===================================================================

subscribe_single_test() ->
    %% GIVEN: An empty registry
    Registry0 = macula_pubsub_registry:new(),

    %% WHEN: Subscribing
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),

    %% THEN: Should have one subscription
    ?assertEqual(1, macula_pubsub_registry:size(Registry1)),
    ?assertEqual([<<"topic.a">>], macula_pubsub_registry:list_patterns(Registry1)).

subscribe_multiple_different_patterns_test() ->
    %% GIVEN: An empty registry
    Registry0 = macula_pubsub_registry:new(),

    %% WHEN: Subscribing to multiple patterns
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),
    Registry2 = macula_pubsub_registry:subscribe(Registry1, <<"sub2">>, <<"topic.b">>, self()),
    Registry3 = macula_pubsub_registry:subscribe(Registry2, <<"sub3">>, <<"topic.c">>, self()),

    %% THEN: Should have three subscriptions
    ?assertEqual(3, macula_pubsub_registry:size(Registry3)),
    Patterns = lists:sort(macula_pubsub_registry:list_patterns(Registry3)),
    ?assertEqual([<<"topic.a">>, <<"topic.b">>, <<"topic.c">>], Patterns).

subscribe_multiple_same_pattern_test() ->
    %% GIVEN: An empty registry
    Registry0 = macula_pubsub_registry:new(),

    %% WHEN: Multiple subscribers to same pattern
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),
    Registry2 = macula_pubsub_registry:subscribe(Registry1, <<"sub2">>, <<"topic.a">>, self()),

    %% THEN: Should have two subscriptions, one pattern
    ?assertEqual(2, macula_pubsub_registry:size(Registry2)),
    ?assertEqual([<<"topic.a">>], macula_pubsub_registry:list_patterns(Registry2)).

subscribe_updates_existing_test() ->
    %% GIVEN: Registry with existing subscription
    Registry0 = macula_pubsub_registry:new(),
    OldCallback = spawn(fun() -> receive _ -> ok end end),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, OldCallback),

    %% WHEN: Same subscriber re-subscribes with new callback
    NewCallback = self(),
    Registry2 = macula_pubsub_registry:subscribe(Registry1, <<"sub1">>, <<"topic.a">>, NewCallback),

    %% THEN: Should still have one subscription with updated callback
    ?assertEqual(1, macula_pubsub_registry:size(Registry2)),
    {ok, Sub} = macula_pubsub_registry:get_subscription(Registry2, <<"sub1">>, <<"topic.a">>),
    ?assertEqual(NewCallback, maps:get(callback, Sub)).

subscribe_wildcard_pattern_test() ->
    %% GIVEN: An empty registry
    Registry0 = macula_pubsub_registry:new(),

    %% WHEN: Subscribing to wildcard pattern
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.#">>, self()),

    %% THEN: Should have the wildcard subscription
    ?assertEqual(1, macula_pubsub_registry:size(Registry1)),
    ?assertEqual([<<"topic.#">>], macula_pubsub_registry:list_patterns(Registry1)).

%%%===================================================================
%%% Unsubscribe Tests
%%%===================================================================

unsubscribe_existing_test() ->
    %% GIVEN: Registry with subscription
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),

    %% WHEN: Unsubscribing
    Registry2 = macula_pubsub_registry:unsubscribe(Registry1, <<"sub1">>, <<"topic.a">>),

    %% THEN: Should be empty
    ?assertEqual(0, macula_pubsub_registry:size(Registry2)),
    ?assertEqual([], macula_pubsub_registry:list_patterns(Registry2)).

unsubscribe_nonexistent_test() ->
    %% GIVEN: Registry with subscription
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),

    %% WHEN: Unsubscribing from non-existent
    Registry2 = macula_pubsub_registry:unsubscribe(Registry1, <<"sub2">>, <<"topic.a">>),

    %% THEN: Should not change
    ?assertEqual(1, macula_pubsub_registry:size(Registry2)).

unsubscribe_wrong_pattern_test() ->
    %% GIVEN: Registry with subscription
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),

    %% WHEN: Unsubscribing from wrong pattern
    Registry2 = macula_pubsub_registry:unsubscribe(Registry1, <<"sub1">>, <<"topic.b">>),

    %% THEN: Should not change
    ?assertEqual(1, macula_pubsub_registry:size(Registry2)).

unsubscribe_one_of_many_test() ->
    %% GIVEN: Registry with multiple subscriptions to same pattern
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),
    Registry2 = macula_pubsub_registry:subscribe(Registry1, <<"sub2">>, <<"topic.a">>, self()),

    %% WHEN: Unsubscribing one
    Registry3 = macula_pubsub_registry:unsubscribe(Registry2, <<"sub1">>, <<"topic.a">>),

    %% THEN: Should have one remaining
    ?assertEqual(1, macula_pubsub_registry:size(Registry3)),
    ?assertEqual([<<"topic.a">>], macula_pubsub_registry:list_patterns(Registry3)).

unsubscribe_removes_pattern_key_test() ->
    %% GIVEN: Registry with single subscription to pattern
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),
    Registry2 = macula_pubsub_registry:subscribe(Registry1, <<"sub2">>, <<"topic.b">>, self()),

    %% WHEN: Unsubscribing the only subscriber from topic.a
    Registry3 = macula_pubsub_registry:unsubscribe(Registry2, <<"sub1">>, <<"topic.a">>),

    %% THEN: Pattern should be removed from index
    ?assertEqual([<<"topic.b">>], macula_pubsub_registry:list_patterns(Registry3)).

%%%===================================================================
%%% Match Tests
%%%===================================================================

match_exact_test() ->
    %% GIVEN: Registry with exact pattern subscription
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),

    %% WHEN: Matching exact topic
    Matches = macula_pubsub_registry:match(Registry1, <<"topic.a">>),

    %% THEN: Should find match
    ?assertEqual(1, length(Matches)),
    [Sub] = Matches,
    ?assertEqual(<<"sub1">>, maps:get(subscriber_id, Sub)).

match_no_match_test() ->
    %% GIVEN: Registry with subscription
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),

    %% WHEN: Matching different topic
    Matches = macula_pubsub_registry:match(Registry1, <<"topic.b">>),

    %% THEN: Should not find match
    ?assertEqual([], Matches).

match_wildcard_hash_test() ->
    %% GIVEN: Registry with # wildcard subscription
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.#">>, self()),

    %% WHEN: Matching multi-level topic
    Matches = macula_pubsub_registry:match(Registry1, <<"topic.a.b.c">>),

    %% THEN: Should match
    ?assertEqual(1, length(Matches)).

match_wildcard_star_test() ->
    %% GIVEN: Registry with * wildcard subscription
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.*">>, self()),

    %% WHEN: Matching single-level topic
    Matches = macula_pubsub_registry:match(Registry1, <<"topic.a">>),

    %% THEN: Should match
    ?assertEqual(1, length(Matches)).

match_wildcard_star_no_match_multilevel_test() ->
    %% GIVEN: Registry with * wildcard subscription
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.*">>, self()),

    %% WHEN: Matching multi-level topic
    Matches = macula_pubsub_registry:match(Registry1, <<"topic.a.b">>),

    %% THEN: Should NOT match (star is single level)
    ?assertEqual([], Matches).

match_multiple_subscribers_test() ->
    %% GIVEN: Registry with multiple subscribers to same pattern
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),
    Registry2 = macula_pubsub_registry:subscribe(Registry1, <<"sub2">>, <<"topic.a">>, self()),

    %% WHEN: Matching topic
    Matches = macula_pubsub_registry:match(Registry2, <<"topic.a">>),

    %% THEN: Should find both
    ?assertEqual(2, length(Matches)),
    SubscriberIds = lists:sort([maps:get(subscriber_id, S) || S <- Matches]),
    ?assertEqual([<<"sub1">>, <<"sub2">>], SubscriberIds).

match_mixed_patterns_test() ->
    %% GIVEN: Registry with different patterns
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),
    Registry2 = macula_pubsub_registry:subscribe(Registry1, <<"sub2">>, <<"topic.#">>, self()),
    Registry3 = macula_pubsub_registry:subscribe(Registry2, <<"sub3">>, <<"other.topic">>, self()),

    %% WHEN: Matching topic.a
    Matches = macula_pubsub_registry:match(Registry3, <<"topic.a">>),

    %% THEN: Should match both exact and wildcard
    ?assertEqual(2, length(Matches)),
    SubscriberIds = lists:sort([maps:get(subscriber_id, S) || S <- Matches]),
    ?assertEqual([<<"sub1">>, <<"sub2">>], SubscriberIds).

%%%===================================================================
%%% get_subscription Tests
%%%===================================================================

get_subscription_exists_test() ->
    %% GIVEN: Registry with subscription
    Registry0 = macula_pubsub_registry:new(),
    Callback = self(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, Callback),

    %% WHEN: Getting subscription
    Result = macula_pubsub_registry:get_subscription(Registry1, <<"sub1">>, <<"topic.a">>),

    %% THEN: Should return subscription
    ?assertMatch({ok, _}, Result),
    {ok, Sub} = Result,
    ?assertEqual(<<"sub1">>, maps:get(subscriber_id, Sub)),
    ?assertEqual(<<"topic.a">>, maps:get(pattern, Sub)),
    ?assertEqual(Callback, maps:get(callback, Sub)).

get_subscription_not_found_test() ->
    %% GIVEN: Empty registry
    Registry = macula_pubsub_registry:new(),

    %% WHEN: Getting non-existent subscription
    Result = macula_pubsub_registry:get_subscription(Registry, <<"sub1">>, <<"topic.a">>),

    %% THEN: Should return not_found
    ?assertEqual(not_found, Result).

get_subscription_wrong_subscriber_test() ->
    %% GIVEN: Registry with subscription
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),

    %% WHEN: Getting with wrong subscriber_id
    Result = macula_pubsub_registry:get_subscription(Registry1, <<"sub2">>, <<"topic.a">>),

    %% THEN: Should return not_found
    ?assertEqual(not_found, Result).

get_subscription_wrong_pattern_test() ->
    %% GIVEN: Registry with subscription
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),

    %% WHEN: Getting with wrong pattern
    Result = macula_pubsub_registry:get_subscription(Registry1, <<"sub1">>, <<"topic.b">>),

    %% THEN: Should return not_found
    ?assertEqual(not_found, Result).

%%%===================================================================
%%% Size Tests
%%%===================================================================

size_empty_test() ->
    Registry = macula_pubsub_registry:new(),
    ?assertEqual(0, macula_pubsub_registry:size(Registry)).

size_after_subscribe_test() ->
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),
    ?assertEqual(1, macula_pubsub_registry:size(Registry1)).

size_after_unsubscribe_test() ->
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),
    Registry2 = macula_pubsub_registry:unsubscribe(Registry1, <<"sub1">>, <<"topic.a">>),
    ?assertEqual(0, macula_pubsub_registry:size(Registry2)).

%%%===================================================================
%%% list_patterns Tests
%%%===================================================================

list_patterns_empty_test() ->
    Registry = macula_pubsub_registry:new(),
    ?assertEqual([], macula_pubsub_registry:list_patterns(Registry)).

list_patterns_unique_test() ->
    %% Even with multiple subscribers to same pattern, pattern appears once
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),
    Registry2 = macula_pubsub_registry:subscribe(Registry1, <<"sub2">>, <<"topic.a">>, self()),

    Patterns = macula_pubsub_registry:list_patterns(Registry2),
    ?assertEqual([<<"topic.a">>], Patterns).

list_patterns_multiple_test() ->
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"a">>, self()),
    Registry2 = macula_pubsub_registry:subscribe(Registry1, <<"sub2">>, <<"b">>, self()),
    Registry3 = macula_pubsub_registry:subscribe(Registry2, <<"sub3">>, <<"c">>, self()),

    Patterns = lists:sort(macula_pubsub_registry:list_patterns(Registry3)),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>], Patterns).

