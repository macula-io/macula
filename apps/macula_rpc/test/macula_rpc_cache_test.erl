%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_rpc_cache module.
%%% Tests written FIRST (TDD red phase).
%%% LRU cache for RPC procedure results (idempotent calls only).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_cache_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Create cache key from URI and args
cache_key(Uri, Args) ->
    macula_rpc_cache:make_key(Uri, Args).

%%%===================================================================
%%% Cache Creation Tests
%%%===================================================================

%% Test: new creates empty cache
new_cache_is_empty_test() ->
    Cache = macula_rpc_cache:new(100),
    ?assertEqual(0, macula_rpc_cache:size(Cache)).

%% Test: new sets max size
new_cache_sets_max_size_test() ->
    Cache = macula_rpc_cache:new(100),
    ?assertEqual(100, macula_rpc_cache:max_size(Cache)).

%%%===================================================================
%%% Key Generation Tests
%%%===================================================================

%% Test: make_key creates consistent keys
make_key_consistent_test() ->
    Uri = <<"be.cortexiq.home.get_measurement">>,
    Args = #{home_id => <<"home_001">>, timestamp => 12345},

    Key1 = cache_key(Uri, Args),
    Key2 = cache_key(Uri, Args),

    ?assertEqual(Key1, Key2).

%% Test: make_key creates different keys for different URIs
make_key_different_uri_test() ->
    Args = #{home_id => <<"home_001">>},

    Key1 = cache_key(<<"uri1">>, Args),
    Key2 = cache_key(<<"uri2">>, Args),

    ?assertNotEqual(Key1, Key2).

%% Test: make_key creates different keys for different args
make_key_different_args_test() ->
    Uri = <<"be.cortexiq.home.get_measurement">>,

    Key1 = cache_key(Uri, #{home_id => <<"home_001">>}),
    Key2 = cache_key(Uri, #{home_id => <<"home_002">>}),

    ?assertNotEqual(Key1, Key2).

%%%===================================================================
%%% Put Tests
%%%===================================================================

%% Test: put adds entry
put_adds_entry_test() ->
    Cache = macula_rpc_cache:new(100),
    Uri = <<"be.cortexiq.home.get_measurement">>,
    Args = #{home_id => <<"home_001">>},
    Result = #{temperature => 22.5, timestamp => 12345},

    Cache2 = macula_rpc_cache:put(Cache, Uri, Args, Result, 300),
    ?assertEqual(1, macula_rpc_cache:size(Cache2)).

%% Test: put updates existing entry
put_updates_existing_test() ->
    Cache = macula_rpc_cache:new(100),
    Uri = <<"be.cortexiq.home.get_measurement">>,
    Args = #{home_id => <<"home_001">>},
    Result1 = #{temperature => 22.5},
    Result2 = #{temperature => 23.0},

    Cache2 = macula_rpc_cache:put(Cache, Uri, Args, Result1, 300),
    Cache3 = macula_rpc_cache:put(Cache2, Uri, Args, Result2, 300),

    %% Should still be 1 entry
    ?assertEqual(1, macula_rpc_cache:size(Cache3)),

    %% Should have updated value
    {ok, Cached, _Cache4} = macula_rpc_cache:get(Cache3, Uri, Args),
    ?assertEqual(Result2, Cached).

%% Test: put evicts oldest when cache full
put_evicts_oldest_test() ->
    Cache = macula_rpc_cache:new(2),  % Small cache

    Cache2 = macula_rpc_cache:put(Cache, <<"uri1">>, #{}, <<"result1">>, 300),
    Cache3 = macula_rpc_cache:put(Cache2, <<"uri2">>, #{}, <<"result2">>, 300),
    Cache4 = macula_rpc_cache:put(Cache3, <<"uri3">>, #{}, <<"result3">>, 300),  % Should evict uri1

    ?assertEqual(2, macula_rpc_cache:size(Cache4)),
    ?assertEqual(not_found, macula_rpc_cache:get(Cache4, <<"uri1">>, #{})),
    ?assertMatch({ok, _, _}, macula_rpc_cache:get(Cache4, <<"uri2">>, #{})),
    ?assertMatch({ok, _, _}, macula_rpc_cache:get(Cache4, <<"uri3">>, #{})).

%%%===================================================================
%%% Get Tests
%%%===================================================================

%% Test: get returns cached value
get_returns_value_test() ->
    Cache = macula_rpc_cache:new(100),
    Uri = <<"be.cortexiq.home.get_measurement">>,
    Args = #{home_id => <<"home_001">>},
    Result = #{temperature => 22.5},

    Cache2 = macula_rpc_cache:put(Cache, Uri, Args, Result, 300),
    {ok, Cached, _Cache3} = macula_rpc_cache:get(Cache2, Uri, Args),

    ?assertEqual(Result, Cached).

%% Test: get returns not_found for missing entry
get_not_found_test() ->
    Cache = macula_rpc_cache:new(100),
    ?assertEqual(not_found, macula_rpc_cache:get(Cache, <<"uri">>, #{})).

%% Test: get moves entry to front (LRU)
get_moves_to_front_test() ->
    Cache = macula_rpc_cache:new(2),

    Cache2 = macula_rpc_cache:put(Cache, <<"uri1">>, #{}, <<"result1">>, 300),
    Cache3 = macula_rpc_cache:put(Cache2, <<"uri2">>, #{}, <<"result2">>, 300),

    %% Access uri1 (moves to front)
    {ok, _, Cache3b} = macula_rpc_cache:get(Cache3, <<"uri1">>, #{}),

    %% Add uri3 (should evict uri2, not uri1)
    Cache4 = macula_rpc_cache:put(Cache3b, <<"uri3">>, #{}, <<"result3">>, 300),

    ?assertMatch({ok, _, _}, macula_rpc_cache:get(Cache4, <<"uri1">>, #{})),
    ?assertEqual(not_found, macula_rpc_cache:get(Cache4, <<"uri2">>, #{})),
    ?assertMatch({ok, _, _}, macula_rpc_cache:get(Cache4, <<"uri3">>, #{})).

%%%===================================================================
%%% Invalidate Tests
%%%===================================================================

%% Test: invalidate removes entry
invalidate_removes_test() ->
    Cache = macula_rpc_cache:new(100),
    Uri = <<"be.cortexiq.home.get_measurement">>,
    Args = #{home_id => <<"home_001">>},

    Cache2 = macula_rpc_cache:put(Cache, Uri, Args, <<"result">>, 300),
    ?assertEqual(1, macula_rpc_cache:size(Cache2)),

    Cache3 = macula_rpc_cache:invalidate(Cache2, Uri, Args),
    ?assertEqual(0, macula_rpc_cache:size(Cache3)).

%% Test: invalidate handles missing entry
invalidate_missing_test() ->
    Cache = macula_rpc_cache:new(100),
    Cache2 = macula_rpc_cache:invalidate(Cache, <<"uri">>, #{}),
    ?assertEqual(Cache, Cache2).

%%%===================================================================
%%% Clear Tests
%%%===================================================================

%% Test: clear removes all entries
clear_removes_all_test() ->
    Cache = macula_rpc_cache:new(100),

    Cache2 = macula_rpc_cache:put(Cache, <<"uri1">>, #{}, <<"result1">>, 300),
    Cache3 = macula_rpc_cache:put(Cache2, <<"uri2">>, #{}, <<"result2">>, 300),
    Cache4 = macula_rpc_cache:put(Cache3, <<"uri3">>, #{}, <<"result3">>, 300),

    ?assertEqual(3, macula_rpc_cache:size(Cache4)),

    Cache5 = macula_rpc_cache:clear(Cache4),
    ?assertEqual(0, macula_rpc_cache:size(Cache5)).

%%%===================================================================
%%% TTL Tests
%%%===================================================================

%% Test: is_expired returns true for expired entry
is_expired_test() ->
    Cache = macula_rpc_cache:new(100),
    Uri = <<"be.cortexiq.home.get_measurement">>,
    Args = #{home_id => <<"home_001">>},

    %% Put with custom timestamp (old)
    OldTimestamp = erlang:system_time(millisecond) - 400000,  % 400 seconds ago
    Cache2 = macula_rpc_cache:put_with_timestamp(Cache, Uri, Args, <<"result">>, 300, OldTimestamp),

    ?assert(macula_rpc_cache:is_expired(Cache2, Uri, Args)).

%% Test: is_expired returns false for fresh entry
is_not_expired_test() ->
    Cache = macula_rpc_cache:new(100),
    Uri = <<"be.cortexiq.home.get_measurement">>,
    Args = #{home_id => <<"home_001">>},

    Cache2 = macula_rpc_cache:put(Cache, Uri, Args, <<"result">>, 300),

    ?assertNot(macula_rpc_cache:is_expired(Cache2, Uri, Args)).

%% Test: is_expired returns true for missing entry
is_expired_missing_test() ->
    Cache = macula_rpc_cache:new(100),
    ?assert(macula_rpc_cache:is_expired(Cache, <<"uri">>, #{})).

%%%===================================================================
%%% Size Tests
%%%===================================================================

%% Test: size returns correct count
size_test() ->
    Cache = macula_rpc_cache:new(100),
    ?assertEqual(0, macula_rpc_cache:size(Cache)),

    Cache2 = macula_rpc_cache:put(Cache, <<"uri1">>, #{}, <<"result1">>, 300),
    ?assertEqual(1, macula_rpc_cache:size(Cache2)),

    Cache3 = macula_rpc_cache:put(Cache2, <<"uri2">>, #{}, <<"result2">>, 300),
    ?assertEqual(2, macula_rpc_cache:size(Cache3)).
