%%%-------------------------------------------------------------------
%%% @doc Unit tests for macula_subscriber_cache module.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_subscriber_cache_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

cache_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            {"cache miss on empty cache", fun test_cache_miss/0},
            {"cache hit after store", fun test_cache_hit/0},
            {"cache invalidate removes entry", fun test_invalidate/0},
            {"cache invalidate_all clears all", fun test_invalidate_all/0},
            {"cache expires after TTL", fun test_ttl_expiration/0},
            {"cache stats tracking", fun test_stats/0},
            {"rate limiting - first query allowed", fun test_rate_limit_first_allowed/0},
            {"rate limiting - second query blocked", fun test_rate_limit_second_blocked/0},
            {"rate limiting - allowed after interval", fun test_rate_limit_allowed_after_interval/0},
            {"rate limiting - stats tracked", fun test_rate_limit_stats/0}
        ]
    }.

setup() ->
    %% Start cache with short TTL and min_discovery_interval for testing
    {ok, Pid} = macula_subscriber_cache:start_link(#{
        ttl_ms => 100,
        min_discovery_interval_ms => 100  %% Short interval for testing
    }),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid).

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_cache_miss() ->
    Topic = <<"test.topic.miss">>,
    Result = macula_subscriber_cache:lookup(Topic),
    ?assertMatch({miss, _}, Result).

test_cache_hit() ->
    Topic = <<"test.topic.hit">>,
    Subscribers = [#{node_id => <<"node1">>, endpoint => <<"https://localhost:4433">>}],

    %% Store subscribers
    ok = macula_subscriber_cache:store(Topic, Subscribers),

    %% Small delay for async store to complete
    timer:sleep(10),

    %% Lookup should hit cache
    ?assertEqual({ok, Subscribers}, macula_subscriber_cache:lookup(Topic)).

test_invalidate() ->
    Topic = <<"test.topic.invalidate">>,
    Subscribers = [#{node_id => <<"node2">>}],

    %% Store and verify
    ok = macula_subscriber_cache:store(Topic, Subscribers),
    timer:sleep(10),
    ?assertEqual({ok, Subscribers}, macula_subscriber_cache:lookup(Topic)),

    %% Invalidate and verify miss
    ok = macula_subscriber_cache:invalidate(Topic),
    timer:sleep(10),
    ?assertMatch({miss, _}, macula_subscriber_cache:lookup(Topic)).

test_invalidate_all() ->
    Topic1 = <<"test.topic.all1">>,
    Topic2 = <<"test.topic.all2">>,
    Subscribers = [#{node_id => <<"node3">>}],

    %% Store multiple topics
    ok = macula_subscriber_cache:store(Topic1, Subscribers),
    ok = macula_subscriber_cache:store(Topic2, Subscribers),
    timer:sleep(10),

    %% Verify both cached
    ?assertEqual({ok, Subscribers}, macula_subscriber_cache:lookup(Topic1)),
    ?assertEqual({ok, Subscribers}, macula_subscriber_cache:lookup(Topic2)),

    %% Invalidate all
    ok = macula_subscriber_cache:invalidate_all(),

    %% Verify both miss
    ?assertMatch({miss, _}, macula_subscriber_cache:lookup(Topic1)),
    ?assertMatch({miss, _}, macula_subscriber_cache:lookup(Topic2)).

test_ttl_expiration() ->
    Topic = <<"test.topic.ttl">>,
    Subscribers = [#{node_id => <<"node4">>}],

    %% Store subscribers
    ok = macula_subscriber_cache:store(Topic, Subscribers),
    timer:sleep(10),

    %% Should hit immediately
    ?assertEqual({ok, Subscribers}, macula_subscriber_cache:lookup(Topic)),

    %% Wait for TTL to expire (100ms + buffer)
    timer:sleep(150),

    %% Should miss after TTL
    ?assertMatch({miss, _}, macula_subscriber_cache:lookup(Topic)).

test_stats() ->
    Topic1 = <<"test.topic.stats1">>,
    Topic2 = <<"test.topic.stats2">>,
    Subscribers = [#{node_id => <<"node5">>}],

    %% Initial stats should show 0 hits/misses (or whatever from previous tests)
    InitStats = macula_subscriber_cache:stats(),
    InitHits = maps:get(hits, InitStats, 0),
    InitMisses = maps:get(misses, InitStats, 0),

    %% Cause a miss
    _ = macula_subscriber_cache:lookup(Topic1),
    timer:sleep(10),

    %% Cause a hit
    ok = macula_subscriber_cache:store(Topic2, Subscribers),
    timer:sleep(10),
    _ = macula_subscriber_cache:lookup(Topic2),
    timer:sleep(10),

    %% Check stats increased
    Stats = macula_subscriber_cache:stats(),
    ?assertEqual(InitHits + 1, maps:get(hits, Stats)),
    ?assertEqual(InitMisses + 1, maps:get(misses, Stats)).

%%%===================================================================
%%% Rate Limiting Test Cases
%%%===================================================================

test_rate_limit_first_allowed() ->
    Topic = <<"test.rate.first">>,
    %% First query should always be allowed
    ?assertEqual(true, macula_subscriber_cache:should_query_dht(Topic)).

test_rate_limit_second_blocked() ->
    Topic = <<"test.rate.second">>,
    %% First query allowed
    ?assertEqual(true, macula_subscriber_cache:should_query_dht(Topic)),
    %% Record the query
    ok = macula_subscriber_cache:record_dht_query(Topic),
    timer:sleep(10),  %% Small delay for async record
    %% Second query immediately should be blocked (within 100ms interval)
    ?assertEqual(false, macula_subscriber_cache:should_query_dht(Topic)).

test_rate_limit_allowed_after_interval() ->
    Topic = <<"test.rate.interval">>,
    %% First query allowed
    ?assertEqual(true, macula_subscriber_cache:should_query_dht(Topic)),
    %% Record the query
    ok = macula_subscriber_cache:record_dht_query(Topic),
    timer:sleep(10),
    %% Should be blocked immediately
    ?assertEqual(false, macula_subscriber_cache:should_query_dht(Topic)),
    %% Wait for interval to expire (100ms + buffer)
    timer:sleep(150),
    %% Should be allowed again
    ?assertEqual(true, macula_subscriber_cache:should_query_dht(Topic)).

test_rate_limit_stats() ->
    Topic = <<"test.rate.stats">>,
    %% Get initial rate_limited count
    InitStats = macula_subscriber_cache:stats(),
    InitRateLimited = maps:get(rate_limited, InitStats, 0),
    %% First query allowed - record it
    _ = macula_subscriber_cache:should_query_dht(Topic),
    ok = macula_subscriber_cache:record_dht_query(Topic),
    timer:sleep(10),
    %% Second query should be blocked and increment rate_limited counter
    ?assertEqual(false, macula_subscriber_cache:should_query_dht(Topic)),
    timer:sleep(10),  %% Wait for async cast
    %% Check rate_limited stat increased
    Stats = macula_subscriber_cache:stats(),
    ?assertEqual(InitRateLimited + 1, maps:get(rate_limited, Stats)).
