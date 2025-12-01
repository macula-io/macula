%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for macula_bridge_cache - Bridge result caching.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_bridge_cache_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

bridge_cache_test_() ->
    [
        {"put and get value", fun put_and_get_value/0},
        {"get non-existent key returns not_found", fun get_non_existent/0},
        {"expired value returns expired", fun expired_value/0},
        {"delete removes value", fun delete_value/0},
        {"clear removes all values", fun clear_cache/0},
        {"size returns correct count", fun cache_size/0},
        {"get_stats returns statistics", fun get_stats/0},
        {"TTL per mesh level - cluster", fun ttl_cluster/0},
        {"TTL per mesh level - city", fun ttl_city/0},
        {"custom TTL overrides default", fun custom_ttl/0},
        {"LRU eviction when full", fun lru_eviction/0},
        {"hit count increments on access", fun hit_count_increments/0}
    ].

start_cache(Config) ->
    process_flag(trap_exit, true),
    {ok, Pid} = macula_bridge_cache:start_link(Config),
    unregister(macula_bridge_cache),
    Pid.

stop_cache(Pid) ->
    catch gen_server:stop(Pid),
    %% Flush any EXIT messages
    receive {'EXIT', Pid, _} -> ok after 10 -> ok end.

%%%===================================================================
%%% Test Cases
%%%===================================================================

put_and_get_value() ->
    Pid = start_cache(#{mesh_level => cluster, cache_max_size => 100}),
    Key = <<"test-key">>,
    Value = #{data => <<"test-value">>},

    ok = macula_bridge_cache:put(Pid, Key, Value),
    {ok, Value} = macula_bridge_cache:get(Pid, Key),

    stop_cache(Pid).

get_non_existent() ->
    Pid = start_cache(#{mesh_level => cluster, cache_max_size => 100}),
    Key = <<"non-existent-key">>,

    not_found = macula_bridge_cache:get(Pid, Key),

    stop_cache(Pid).

expired_value() ->
    %% Create cache with 1 second TTL
    Pid = start_cache(#{mesh_level => cluster, cache_ttl => 1}),

    Key = <<"expiring-key">>,
    Value = <<"expiring-value">>,

    ok = macula_bridge_cache:put(Pid, Key, Value),
    {ok, Value} = macula_bridge_cache:get(Pid, Key),

    %% Wait for expiration
    timer:sleep(1100),

    expired = macula_bridge_cache:get(Pid, Key),

    stop_cache(Pid).

delete_value() ->
    Pid = start_cache(#{mesh_level => cluster, cache_max_size => 100}),
    Key = <<"delete-key">>,
    Value = <<"delete-value">>,

    ok = macula_bridge_cache:put(Pid, Key, Value),
    {ok, Value} = macula_bridge_cache:get(Pid, Key),

    ok = macula_bridge_cache:delete(Pid, Key),
    not_found = macula_bridge_cache:get(Pid, Key),

    stop_cache(Pid).

clear_cache() ->
    Pid = start_cache(#{mesh_level => cluster, cache_max_size => 100}),

    %% Add multiple entries
    ok = macula_bridge_cache:put(Pid, <<"key1">>, <<"value1">>),
    ok = macula_bridge_cache:put(Pid, <<"key2">>, <<"value2">>),
    ok = macula_bridge_cache:put(Pid, <<"key3">>, <<"value3">>),

    ?assertEqual(3, macula_bridge_cache:size(Pid)),

    ok = macula_bridge_cache:clear(Pid),

    ?assertEqual(0, macula_bridge_cache:size(Pid)),

    stop_cache(Pid).

cache_size() ->
    Pid = start_cache(#{mesh_level => cluster, cache_max_size => 100}),

    ?assertEqual(0, macula_bridge_cache:size(Pid)),

    ok = macula_bridge_cache:put(Pid, <<"key1">>, <<"value1">>),
    ?assertEqual(1, macula_bridge_cache:size(Pid)),

    ok = macula_bridge_cache:put(Pid, <<"key2">>, <<"value2">>),
    ?assertEqual(2, macula_bridge_cache:size(Pid)),

    stop_cache(Pid).

get_stats() ->
    Pid = start_cache(#{mesh_level => cluster, cache_max_size => 100}),

    %% Generate some activity
    ok = macula_bridge_cache:put(Pid, <<"key1">>, <<"value1">>),
    {ok, _} = macula_bridge_cache:get(Pid, <<"key1">>),
    {ok, _} = macula_bridge_cache:get(Pid, <<"key1">>),
    not_found = macula_bridge_cache:get(Pid, <<"missing">>),

    {ok, Stats} = macula_bridge_cache:get_stats(Pid),

    ?assertEqual(2, maps:get(hits, Stats)),
    ?assertEqual(1, maps:get(misses, Stats)),
    ?assertEqual(1, maps:get(inserts, Stats)),
    ?assertEqual(cluster, maps:get(mesh_level, Stats)),
    ?assertEqual(1, maps:get(current_size, Stats)),
    ?assertEqual(100, maps:get(max_size, Stats)),

    stop_cache(Pid).

ttl_cluster() ->
    %% Cluster level should have 5 minute (300s) default TTL
    Pid = start_cache(#{mesh_level => cluster}),

    {ok, Stats} = macula_bridge_cache:get_stats(Pid),
    ?assertEqual(300, maps:get(default_ttl, Stats)),

    stop_cache(Pid).

ttl_city() ->
    %% City level should have 30 minute (1800s) default TTL
    Pid = start_cache(#{mesh_level => city}),

    {ok, Stats} = macula_bridge_cache:get_stats(Pid),
    ?assertEqual(1800, maps:get(default_ttl, Stats)),

    stop_cache(Pid).

custom_ttl() ->
    Pid = start_cache(#{mesh_level => cluster, cache_ttl => 600}),

    {ok, Stats} = macula_bridge_cache:get_stats(Pid),
    ?assertEqual(600, maps:get(default_ttl, Stats)),

    stop_cache(Pid).

lru_eviction() ->
    %% Create cache with max 5 entries
    Pid = start_cache(#{mesh_level => cluster, cache_max_size => 5}),

    %% Add 5 entries
    lists:foreach(fun(N) ->
        Key = list_to_binary("key" ++ integer_to_list(N)),
        ok = macula_bridge_cache:put(Pid, Key, <<"value">>)
    end, lists:seq(1, 5)),

    ?assertEqual(5, macula_bridge_cache:size(Pid)),

    %% Add 6th entry - should trigger eviction
    ok = macula_bridge_cache:put(Pid, <<"key6">>, <<"value6">>),

    %% Size should be at most 5 (eviction removes ~10% = 1 entry)
    Size = macula_bridge_cache:size(Pid),
    ?assert(Size =< 5),

    %% The newest entry should still be there
    {ok, <<"value6">>} = macula_bridge_cache:get(Pid, <<"key6">>),

    stop_cache(Pid).

hit_count_increments() ->
    Pid = start_cache(#{mesh_level => cluster, cache_max_size => 100}),
    Key = <<"hit-test-key">>,
    Value = <<"hit-test-value">>,

    ok = macula_bridge_cache:put(Pid, Key, Value),

    %% Access multiple times
    {ok, _} = macula_bridge_cache:get(Pid, Key),
    {ok, _} = macula_bridge_cache:get(Pid, Key),
    {ok, _} = macula_bridge_cache:get(Pid, Key),

    {ok, Stats} = macula_bridge_cache:get_stats(Pid),
    ?assertEqual(3, maps:get(hits, Stats)),

    stop_cache(Pid).
