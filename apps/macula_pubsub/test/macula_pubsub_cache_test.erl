%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_pubsub_cache module.
%%% Tests written FIRST (TDD red phase).
%%% LRU cache for remote subscriber lists.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_cache_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cache Creation Tests
%%%===================================================================

%% Test: new creates empty cache
new_cache_is_empty_test() ->
    Cache = macula_pubsub_cache:new(100),
    ?assertEqual(0, macula_pubsub_cache:size(Cache)).

%% Test: new sets max size
new_cache_sets_max_size_test() ->
    Cache = macula_pubsub_cache:new(100),
    ?assertEqual(100, macula_pubsub_cache:max_size(Cache)).

%%%===================================================================
%%% Put Tests
%%%===================================================================

%% Test: put adds entry
put_adds_entry_test() ->
    Cache = macula_pubsub_cache:new(100),
    Pattern = <<"be.cortexiq.#">>,
    Subscribers = [#{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}}],

    Cache2 = macula_pubsub_cache:put(Cache, Pattern, Subscribers),
    ?assertEqual(1, macula_pubsub_cache:size(Cache2)).

%% Test: put updates existing entry
put_updates_existing_test() ->
    Cache = macula_pubsub_cache:new(100),
    Pattern = <<"be.cortexiq.#">>,
    Subs1 = [#{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}}],
    Subs2 = [#{node_id => <<2:256>>, address => {{127,0,0,1}, 8081}}],

    Cache2 = macula_pubsub_cache:put(Cache, Pattern, Subs1),
    Cache3 = macula_pubsub_cache:put(Cache2, Pattern, Subs2),

    %% Should still be 1 entry
    ?assertEqual(1, macula_pubsub_cache:size(Cache3)),

    %% Should have updated value
    {ok, Cached, _Cache4} = macula_pubsub_cache:get(Cache3, Pattern),
    ?assertEqual(Subs2, Cached).

%% Test: put evicts oldest when cache full
put_evicts_oldest_test() ->
    Cache = macula_pubsub_cache:new(2),  % Small cache

    Cache2 = macula_pubsub_cache:put(Cache, <<"pattern1">>, []),
    Cache3 = macula_pubsub_cache:put(Cache2, <<"pattern2">>, []),
    Cache4 = macula_pubsub_cache:put(Cache3, <<"pattern3">>, []),  % Should evict pattern1

    ?assertEqual(2, macula_pubsub_cache:size(Cache4)),
    ?assertEqual(not_found, macula_pubsub_cache:get(Cache4, <<"pattern1">>)),
    ?assertMatch({ok, _, _}, macula_pubsub_cache:get(Cache4, <<"pattern2">>)),
    ?assertMatch({ok, _, _}, macula_pubsub_cache:get(Cache4, <<"pattern3">>)).

%%%===================================================================
%%% Get Tests
%%%===================================================================

%% Test: get returns cached value
get_returns_value_test() ->
    Cache = macula_pubsub_cache:new(100),
    Pattern = <<"be.cortexiq.#">>,
    Subscribers = [#{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}}],

    Cache2 = macula_pubsub_cache:put(Cache, Pattern, Subscribers),
    {ok, Cached, _Cache3} = macula_pubsub_cache:get(Cache2, Pattern),

    ?assertEqual(Subscribers, Cached).

%% Test: get returns not_found for missing entry
get_not_found_test() ->
    Cache = macula_pubsub_cache:new(100),
    ?assertEqual(not_found, macula_pubsub_cache:get(Cache, <<"be.cortexiq.#">>)).

%% Test: get moves entry to front (LRU)
get_moves_to_front_test() ->
    Cache = macula_pubsub_cache:new(2),

    Cache2 = macula_pubsub_cache:put(Cache, <<"pattern1">>, []),
    Cache3 = macula_pubsub_cache:put(Cache2, <<"pattern2">>, []),

    %% Access pattern1 (moves to front)
    {ok, _, Cache3b} = macula_pubsub_cache:get(Cache3, <<"pattern1">>),

    %% Add pattern3 (should evict pattern2, not pattern1)
    Cache4 = macula_pubsub_cache:put(Cache3b, <<"pattern3">>, []),

    ?assertMatch({ok, _, _}, macula_pubsub_cache:get(Cache4, <<"pattern1">>)),
    ?assertEqual(not_found, macula_pubsub_cache:get(Cache4, <<"pattern2">>)),
    ?assertMatch({ok, _, _}, macula_pubsub_cache:get(Cache4, <<"pattern3">>)).

%%%===================================================================
%%% Invalidate Tests
%%%===================================================================

%% Test: invalidate removes entry
invalidate_removes_test() ->
    Cache = macula_pubsub_cache:new(100),
    Pattern = <<"be.cortexiq.#">>,

    Cache2 = macula_pubsub_cache:put(Cache, Pattern, []),
    ?assertEqual(1, macula_pubsub_cache:size(Cache2)),

    Cache3 = macula_pubsub_cache:invalidate(Cache2, Pattern),
    ?assertEqual(0, macula_pubsub_cache:size(Cache3)).

%% Test: invalidate handles missing entry
invalidate_missing_test() ->
    Cache = macula_pubsub_cache:new(100),
    Cache2 = macula_pubsub_cache:invalidate(Cache, <<"be.cortexiq.#">>),
    ?assertEqual(Cache, Cache2).

%%%===================================================================
%%% Clear Tests
%%%===================================================================

%% Test: clear removes all entries
clear_removes_all_test() ->
    Cache = macula_pubsub_cache:new(100),

    Cache2 = macula_pubsub_cache:put(Cache, <<"pattern1">>, []),
    Cache3 = macula_pubsub_cache:put(Cache2, <<"pattern2">>, []),
    Cache4 = macula_pubsub_cache:put(Cache3, <<"pattern3">>, []),

    ?assertEqual(3, macula_pubsub_cache:size(Cache4)),

    Cache5 = macula_pubsub_cache:clear(Cache4),
    ?assertEqual(0, macula_pubsub_cache:size(Cache5)).

%%%===================================================================
%%% TTL Tests
%%%===================================================================

%% Test: is_expired returns true for expired entry
is_expired_test() ->
    Cache = macula_pubsub_cache:new(100),
    Pattern = <<"be.cortexiq.#">>,

    %% Put with custom timestamp (old)
    OldTimestamp = erlang:system_time(millisecond) - 400000,  % 400 seconds ago
    Cache2 = macula_pubsub_cache:put_with_timestamp(Cache, Pattern, [], OldTimestamp),

    TTL = 300,  % 300 seconds
    ?assert(macula_pubsub_cache:is_expired(Cache2, Pattern, TTL)).

%% Test: is_expired returns false for fresh entry
is_not_expired_test() ->
    Cache = macula_pubsub_cache:new(100),
    Pattern = <<"be.cortexiq.#">>,

    Cache2 = macula_pubsub_cache:put(Cache, Pattern, []),

    TTL = 300,
    ?assertNot(macula_pubsub_cache:is_expired(Cache2, Pattern, TTL)).

%% Test: is_expired returns true for missing entry
is_expired_missing_test() ->
    Cache = macula_pubsub_cache:new(100),
    TTL = 300,
    ?assert(macula_pubsub_cache:is_expired(Cache, <<"be.cortexiq.#">>, TTL)).

%%%===================================================================
%%% Size Tests
%%%===================================================================

%% Test: size returns correct count
size_test() ->
    Cache = macula_pubsub_cache:new(100),
    ?assertEqual(0, macula_pubsub_cache:size(Cache)),

    Cache2 = macula_pubsub_cache:put(Cache, <<"pattern1">>, []),
    ?assertEqual(1, macula_pubsub_cache:size(Cache2)),

    Cache3 = macula_pubsub_cache:put(Cache2, <<"pattern2">>, []),
    ?assertEqual(2, macula_pubsub_cache:size(Cache3)).
