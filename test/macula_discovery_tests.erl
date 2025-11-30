%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_discovery module.
%%%
%%% Tests generic DHT-based service discovery with cache integration.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_discovery_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% find Tests
%%%===================================================================

find_success_test() ->
    %% GIVEN: A lookup function that returns values
    Key = <<"service.key">>,
    Values = [#{node_id => <<"node1">>}, #{node_id => <<"node2">>}],
    LookupFun = fun(_K) -> {ok, Values} end,

    %% WHEN: Finding values
    Result = macula_discovery:find(Key, LookupFun),

    %% THEN: Should return the values
    ?assertEqual({ok, Values}, Result).

find_empty_test() ->
    %% GIVEN: A lookup function that returns empty list
    Key = <<"empty.key">>,
    LookupFun = fun(_K) -> {ok, []} end,

    %% WHEN: Finding values
    Result = macula_discovery:find(Key, LookupFun),

    %% THEN: Should return empty list
    ?assertEqual({ok, []}, Result).

find_error_test() ->
    %% GIVEN: A lookup function that returns error
    Key = <<"error.key">>,
    LookupFun = fun(_K) -> {error, not_found} end,

    %% WHEN: Finding values
    Result = macula_discovery:find(Key, LookupFun),

    %% THEN: Should return the error
    ?assertEqual({error, not_found}, Result).

find_uses_key_test() ->
    %% GIVEN: A lookup function that captures the key
    Self = self(),
    LookupFun = fun(K) ->
        Self ! {lookup, K},
        {ok, []}
    end,
    Key = <<"capture.key">>,

    %% WHEN: Finding values
    macula_discovery:find(Key, LookupFun),

    %% THEN: Should have passed the key to lookup
    receive
        {lookup, ReceivedKey} ->
            ?assertEqual(Key, ReceivedKey)
    after 100 ->
        ?assert(false)
    end.

%%%===================================================================
%%% find_with_cache Tests
%%%===================================================================

find_with_cache_miss_test() ->
    %% GIVEN: Empty cache and lookup function
    Cache = macula_cache:new(10),
    Key = <<"cache.miss">>,
    Values = [#{id => <<"v1">>}],
    LookupFun = fun(_K) -> {ok, Values} end,

    %% WHEN: Finding with cache (miss)
    Result = macula_discovery:find_with_cache(Key, Cache, LookupFun),

    %% THEN: Should return values and updated cache
    ?assertMatch({ok, _, _}, Result),
    {ok, ReturnedValues, UpdatedCache} = Result,
    ?assertEqual(Values, ReturnedValues),
    %% Cache should now have the entry
    ?assertMatch({ok, _, _}, macula_cache:get(UpdatedCache, Key)).

find_with_cache_hit_test() ->
    %% GIVEN: Cache with existing entry
    Cache0 = macula_cache:new(10),
    Key = <<"cache.hit">>,
    CachedValues = [#{id => <<"cached">>}],
    Cache1 = macula_cache:put(Cache0, Key, CachedValues),

    %% Lookup should NOT be called
    LookupFun = fun(_K) ->
        throw(should_not_be_called)
    end,

    %% WHEN: Finding with cache (hit)
    Result = macula_discovery:find_with_cache(Key, Cache1, LookupFun),

    %% THEN: Should return cached values
    ?assertMatch({ok, _, _}, Result),
    {ok, ReturnedValues, _UpdatedCache} = Result,
    ?assertEqual(CachedValues, ReturnedValues).

find_with_cache_custom_ttl_test() ->
    %% GIVEN: Cache and custom TTL
    Cache = macula_cache:new(10),
    Key = <<"custom.ttl">>,
    Values = [#{id => <<"v1">>}],
    LookupFun = fun(_K) -> {ok, Values} end,
    TTL = 600,  % 10 minutes

    %% WHEN: Finding with custom TTL
    Result = macula_discovery:find_with_cache(Key, Cache, LookupFun, TTL),

    %% THEN: Should succeed
    ?assertMatch({ok, _, _}, Result).

find_with_cache_error_test() ->
    %% GIVEN: Cache and failing lookup
    Cache = macula_cache:new(10),
    Key = <<"error.lookup">>,
    LookupFun = fun(_K) -> {error, network_error} end,

    %% WHEN: Finding with cache
    Result = macula_discovery:find_with_cache(Key, Cache, LookupFun),

    %% THEN: Should return error with cache
    ?assertMatch({error, _, _}, Result),
    {error, Reason, _UpdatedCache} = Result,
    ?assertEqual(network_error, Reason).

find_with_cache_default_ttl_test() ->
    %% GIVEN: Cache (default TTL = 300)
    Cache = macula_cache:new(10),
    Key = <<"default.ttl">>,
    Values = [],
    LookupFun = fun(_K) -> {ok, Values} end,

    %% WHEN: Finding with default TTL
    Result = macula_discovery:find_with_cache(Key, Cache, LookupFun),

    %% THEN: Should succeed
    ?assertMatch({ok, [], _}, Result).

%%%===================================================================
%%% announce Tests
%%%===================================================================

announce_success_test() ->
    %% GIVEN: A publish function that succeeds
    Key = <<"announce.key">>,
    PublishFun = fun(_K) -> ok end,

    %% WHEN: Announcing
    Result = macula_discovery:announce(Key, PublishFun),

    %% THEN: Should succeed
    ?assertEqual(ok, Result).

announce_error_test() ->
    %% GIVEN: A publish function that fails
    Key = <<"announce.error">>,
    PublishFun = fun(_K) -> {error, write_failed} end,

    %% WHEN: Announcing
    Result = macula_discovery:announce(Key, PublishFun),

    %% THEN: Should return error
    ?assertEqual({error, write_failed}, Result).

announce_uses_key_test() ->
    %% GIVEN: A publish function that captures key
    Self = self(),
    Key = <<"announce.capture">>,
    PublishFun = fun(K) ->
        Self ! {publish, K},
        ok
    end,

    %% WHEN: Announcing
    macula_discovery:announce(Key, PublishFun),

    %% THEN: Should have passed the key
    receive
        {publish, ReceivedKey} ->
            ?assertEqual(Key, ReceivedKey)
    after 100 ->
        ?assert(false)
    end.

%%%===================================================================
%%% unannounce Tests
%%%===================================================================

unannounce_success_test() ->
    %% GIVEN: An unpublish function that succeeds
    Key = <<"unannounce.key">>,
    UnpublishFun = fun(_K) -> ok end,

    %% WHEN: Unannouncing
    Result = macula_discovery:unannounce(Key, UnpublishFun),

    %% THEN: Should succeed
    ?assertEqual(ok, Result).

unannounce_error_test() ->
    %% GIVEN: An unpublish function that fails
    Key = <<"unannounce.error">>,
    UnpublishFun = fun(_K) -> {error, not_found} end,

    %% WHEN: Unannouncing
    Result = macula_discovery:unannounce(Key, UnpublishFun),

    %% THEN: Should return error
    ?assertEqual({error, not_found}, Result).

%%%===================================================================
%%% filter_by_age Tests
%%%===================================================================

filter_by_age_all_fresh_test() ->
    %% GIVEN: Items that are all fresh
    Now = erlang:system_time(millisecond),
    Items = [
        #{id => <<"1">>, last_seen => Now - 1000},      % 1 second old
        #{id => <<"2">>, last_seen => Now - 5000},      % 5 seconds old
        #{id => <<"3">>, last_seen => Now - 10000}      % 10 seconds old
    ],
    TTL = 60,  % 60 seconds

    %% WHEN: Filtering
    Filtered = macula_discovery:filter_by_age(Items, TTL, last_seen),

    %% THEN: All should be included
    ?assertEqual(3, length(Filtered)).

filter_by_age_some_expired_test() ->
    %% GIVEN: Items with mixed ages
    Now = erlang:system_time(millisecond),
    Items = [
        #{id => <<"fresh">>, last_seen => Now - 5000},    % 5 seconds old (fresh)
        #{id => <<"expired">>, last_seen => Now - 20000}  % 20 seconds old (expired)
    ],
    TTL = 10,  % 10 seconds

    %% WHEN: Filtering
    Filtered = macula_discovery:filter_by_age(Items, TTL, last_seen),

    %% THEN: Only fresh item should remain
    ?assertEqual(1, length(Filtered)),
    [Item] = Filtered,
    ?assertEqual(<<"fresh">>, maps:get(id, Item)).

filter_by_age_all_expired_test() ->
    %% GIVEN: Items that are all expired
    Now = erlang:system_time(millisecond),
    Items = [
        #{id => <<"1">>, last_seen => Now - 30000},   % 30 seconds old
        #{id => <<"2">>, last_seen => Now - 60000}    % 60 seconds old
    ],
    TTL = 10,  % 10 seconds

    %% WHEN: Filtering
    Filtered = macula_discovery:filter_by_age(Items, TTL, last_seen),

    %% THEN: All should be filtered out
    ?assertEqual([], Filtered).

filter_by_age_empty_items_test() ->
    %% GIVEN: Empty items list
    Items = [],
    TTL = 60,

    %% WHEN: Filtering
    Filtered = macula_discovery:filter_by_age(Items, TTL, last_seen),

    %% THEN: Should return empty
    ?assertEqual([], Filtered).

filter_by_age_missing_timestamp_test() ->
    %% GIVEN: Items without timestamp field
    Items = [
        #{id => <<"no_timestamp">>},
        #{id => <<"also_no_timestamp">>}
    ],
    TTL = 60,

    %% WHEN: Filtering
    Filtered = macula_discovery:filter_by_age(Items, TTL, last_seen),

    %% THEN: Items without timestamp should be considered expired
    ?assertEqual([], Filtered).

filter_by_age_custom_field_test() ->
    %% GIVEN: Items with custom timestamp field
    Now = erlang:system_time(millisecond),
    Items = [
        #{id => <<"1">>, updated_at => Now - 1000}
    ],
    TTL = 60,

    %% WHEN: Filtering with custom field
    Filtered = macula_discovery:filter_by_age(Items, TTL, updated_at),

    %% THEN: Should work with custom field
    ?assertEqual(1, length(Filtered)).

