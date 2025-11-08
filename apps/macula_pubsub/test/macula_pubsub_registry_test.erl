%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_pubsub_registry module.
%%% Tests written FIRST (TDD red phase).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_registry_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Registry Creation Tests
%%%===================================================================

%% Test: new creates empty registry
new_registry_is_empty_test() ->
    Registry = macula_pubsub_registry:new(),
    ?assertEqual(0, macula_pubsub_registry:size(Registry)).

%%%===================================================================
%%% Subscribe Tests
%%%===================================================================

%% Test: subscribe adds subscription
subscribe_adds_test() ->
    Registry = macula_pubsub_registry:new(),
    SubscriberId = <<"sub_001">>,
    Pattern = <<"be.cortexiq.*.measured">>,
    Callback = self(),

    Registry2 = macula_pubsub_registry:subscribe(Registry, SubscriberId, Pattern, Callback),
    ?assertEqual(1, macula_pubsub_registry:size(Registry2)).

%% Test: subscribe multiple patterns
subscribe_multiple_test() ->
    Registry = macula_pubsub_registry:new(),
    Sub1 = <<"sub_001">>,
    Sub2 = <<"sub_002">>,

    Registry2 = macula_pubsub_registry:subscribe(Registry, Sub1, <<"be.cortexiq.#">>, self()),
    Registry3 = macula_pubsub_registry:subscribe(Registry2, Sub2, <<"org.example.*">>, self()),

    ?assertEqual(2, macula_pubsub_registry:size(Registry3)).

%% Test: same subscriber can have multiple subscriptions
subscribe_same_subscriber_multiple_patterns_test() ->
    Registry = macula_pubsub_registry:new(),
    SubscriberId = <<"sub_001">>,

    Registry2 = macula_pubsub_registry:subscribe(Registry, SubscriberId, <<"be.cortexiq.#">>, self()),
    Registry3 = macula_pubsub_registry:subscribe(Registry2, SubscriberId, <<"org.example.*">>, self()),

    ?assertEqual(2, macula_pubsub_registry:size(Registry3)).

%% Test: duplicate subscription (same subscriber, same pattern) updates
subscribe_duplicate_updates_test() ->
    Registry = macula_pubsub_registry:new(),
    SubscriberId = <<"sub_001">>,
    Pattern = <<"be.cortexiq.#">>,
    Callback1 = spawn(fun() -> ok end),
    Callback2 = self(),

    Registry2 = macula_pubsub_registry:subscribe(Registry, SubscriberId, Pattern, Callback1),
    Registry3 = macula_pubsub_registry:subscribe(Registry2, SubscriberId, Pattern, Callback2),

    %% Should still be 1 subscription (updated)
    ?assertEqual(1, macula_pubsub_registry:size(Registry3)).

%%%===================================================================
%%% Unsubscribe Tests
%%%===================================================================

%% Test: unsubscribe removes subscription
unsubscribe_removes_test() ->
    Registry = macula_pubsub_registry:new(),
    SubscriberId = <<"sub_001">>,
    Pattern = <<"be.cortexiq.#">>,

    Registry2 = macula_pubsub_registry:subscribe(Registry, SubscriberId, Pattern, self()),
    ?assertEqual(1, macula_pubsub_registry:size(Registry2)),

    Registry3 = macula_pubsub_registry:unsubscribe(Registry2, SubscriberId, Pattern),
    ?assertEqual(0, macula_pubsub_registry:size(Registry3)).

%% Test: unsubscribe specific pattern keeps others
unsubscribe_keeps_others_test() ->
    Registry = macula_pubsub_registry:new(),
    SubscriberId = <<"sub_001">>,

    Registry2 = macula_pubsub_registry:subscribe(Registry, SubscriberId, <<"be.cortexiq.#">>, self()),
    Registry3 = macula_pubsub_registry:subscribe(Registry2, SubscriberId, <<"org.example.*">>, self()),

    %% Unsubscribe from one pattern
    Registry4 = macula_pubsub_registry:unsubscribe(Registry3, SubscriberId, <<"be.cortexiq.#">>),

    ?assertEqual(1, macula_pubsub_registry:size(Registry4)).

%% Test: unsubscribe non-existent does nothing
unsubscribe_nonexistent_test() ->
    Registry = macula_pubsub_registry:new(),
    Registry2 = macula_pubsub_registry:unsubscribe(Registry, <<"sub_001">>, <<"be.cortexiq.#">>),
    ?assertEqual(Registry, Registry2).

%%%===================================================================
%%% Match Tests
%%%===================================================================

%% Test: match returns matching subscriptions
match_returns_matches_test() ->
    Registry = macula_pubsub_registry:new(),

    Registry2 = macula_pubsub_registry:subscribe(Registry, <<"sub_001">>, <<"be.cortexiq.#">>, self()),
    Registry3 = macula_pubsub_registry:subscribe(Registry2, <<"sub_002">>, <<"org.example.*">>, self()),

    Topic = <<"be.cortexiq.home.measured">>,
    Matches = macula_pubsub_registry:match(Registry3, Topic),

    ?assertEqual(1, length(Matches)).

%% Test: match returns multiple matches
match_multiple_test() ->
    Registry = macula_pubsub_registry:new(),

    Registry2 = macula_pubsub_registry:subscribe(Registry, <<"sub_001">>, <<"be.cortexiq.#">>, self()),
    Registry3 = macula_pubsub_registry:subscribe(Registry2, <<"sub_002">>, <<"be.*.*.measured">>, self()),

    Topic = <<"be.cortexiq.home.measured">>,
    Matches = macula_pubsub_registry:match(Registry3, Topic),

    ?assertEqual(2, length(Matches)).

%% Test: match returns empty for no matches
match_no_matches_test() ->
    Registry = macula_pubsub_registry:new(),
    Registry2 = macula_pubsub_registry:subscribe(Registry, <<"sub_001">>, <<"org.example.*">>, self()),

    Topic = <<"be.cortexiq.home.measured">>,
    Matches = macula_pubsub_registry:match(Registry2, Topic),

    ?assertEqual([], Matches).

%%%===================================================================
%%% List Patterns Tests
%%%===================================================================

%% Test: list_patterns returns all patterns
list_patterns_test() ->
    Registry = macula_pubsub_registry:new(),

    Registry2 = macula_pubsub_registry:subscribe(Registry, <<"sub_001">>, <<"be.cortexiq.#">>, self()),
    Registry3 = macula_pubsub_registry:subscribe(Registry2, <<"sub_002">>, <<"org.example.*">>, self()),

    Patterns = macula_pubsub_registry:list_patterns(Registry3),

    ?assertEqual(2, length(Patterns)),
    ?assert(lists:member(<<"be.cortexiq.#">>, Patterns)),
    ?assert(lists:member(<<"org.example.*">>, Patterns)).

%% Test: list_patterns returns unique patterns
list_patterns_unique_test() ->
    Registry = macula_pubsub_registry:new(),

    %% Same pattern, different subscribers
    Registry2 = macula_pubsub_registry:subscribe(Registry, <<"sub_001">>, <<"be.cortexiq.#">>, self()),
    Registry3 = macula_pubsub_registry:subscribe(Registry2, <<"sub_002">>, <<"be.cortexiq.#">>, self()),

    Patterns = macula_pubsub_registry:list_patterns(Registry3),

    ?assertEqual(1, length(Patterns)).

%%%===================================================================
%%% Get Subscription Tests
%%%===================================================================

%% Test: get_subscription returns subscription info
get_subscription_test() ->
    Registry = macula_pubsub_registry:new(),
    SubscriberId = <<"sub_001">>,
    Pattern = <<"be.cortexiq.#">>,
    Callback = self(),

    Registry2 = macula_pubsub_registry:subscribe(Registry, SubscriberId, Pattern, Callback),

    {ok, Sub} = macula_pubsub_registry:get_subscription(Registry2, SubscriberId, Pattern),
    ?assertEqual(SubscriberId, maps:get(subscriber_id, Sub)),
    ?assertEqual(Pattern, maps:get(pattern, Sub)),
    ?assertEqual(Callback, maps:get(callback, Sub)).

%% Test: get_subscription returns not_found
get_subscription_not_found_test() ->
    Registry = macula_pubsub_registry:new(),
    ?assertEqual(not_found, macula_pubsub_registry:get_subscription(Registry, <<"sub_001">>, <<"be.cortexiq.#">>)).

%%%===================================================================
%%% Size Tests
%%%===================================================================

%% Test: size returns correct count
size_test() ->
    Registry = macula_pubsub_registry:new(),
    ?assertEqual(0, macula_pubsub_registry:size(Registry)),

    Registry2 = macula_pubsub_registry:subscribe(Registry, <<"sub_001">>, <<"be.cortexiq.#">>, self()),
    ?assertEqual(1, macula_pubsub_registry:size(Registry2)),

    Registry3 = macula_pubsub_registry:subscribe(Registry2, <<"sub_002">>, <<"org.example.*">>, self()),
    ?assertEqual(2, macula_pubsub_registry:size(Registry3)).
