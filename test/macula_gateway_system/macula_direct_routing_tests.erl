%%%-------------------------------------------------------------------
%%% @doc Unit tests for macula_direct_routing module.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_direct_routing_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

routing_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            {"lookup miss on empty table", fun test_lookup_miss/0},
            {"lookup hit after store", fun test_lookup_hit/0},
            {"remove deletes entry", fun test_remove/0},
            {"clear_all clears all entries", fun test_clear_all/0},
            {"entry expires after TTL", fun test_ttl_expiration/0},
            {"stats tracking", fun test_stats/0},
            {"store_from_subscriber with atom keys", fun test_store_from_subscriber_atom/0},
            {"store_from_subscriber with binary keys", fun test_store_from_subscriber_binary/0},
            {"store_from_subscriber with missing fields", fun test_store_from_subscriber_missing/0}
        ]
    }.

setup() ->
    %% Start routing table with short TTL for testing
    {ok, Pid} = macula_direct_routing:start_link(#{
        ttl_ms => 100,
        cleanup_interval_ms => 50
    }),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid).

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_lookup_miss() ->
    NodeId = crypto:strong_rand_bytes(32),
    Result = macula_direct_routing:lookup(NodeId),
    ?assertEqual(miss, Result).

test_lookup_hit() ->
    NodeId = crypto:strong_rand_bytes(32),
    Endpoint = <<"https://192.168.1.100:4433">>,

    %% Store endpoint
    ok = macula_direct_routing:store(NodeId, Endpoint),

    %% Small delay for async store to complete
    timer:sleep(10),

    %% Lookup should hit
    ?assertEqual({ok, Endpoint}, macula_direct_routing:lookup(NodeId)).

test_remove() ->
    NodeId = crypto:strong_rand_bytes(32),
    Endpoint = <<"https://192.168.1.101:4433">>,

    %% Store and verify
    ok = macula_direct_routing:store(NodeId, Endpoint),
    timer:sleep(10),
    ?assertEqual({ok, Endpoint}, macula_direct_routing:lookup(NodeId)),

    %% Remove and verify miss
    ok = macula_direct_routing:remove(NodeId),
    timer:sleep(10),
    ?assertEqual(miss, macula_direct_routing:lookup(NodeId)).

test_clear_all() ->
    NodeId1 = crypto:strong_rand_bytes(32),
    NodeId2 = crypto:strong_rand_bytes(32),
    Endpoint = <<"https://192.168.1.102:4433">>,

    %% Store multiple entries
    ok = macula_direct_routing:store(NodeId1, Endpoint),
    ok = macula_direct_routing:store(NodeId2, Endpoint),
    timer:sleep(10),

    %% Verify both cached
    ?assertEqual({ok, Endpoint}, macula_direct_routing:lookup(NodeId1)),
    ?assertEqual({ok, Endpoint}, macula_direct_routing:lookup(NodeId2)),

    %% Clear all
    ok = macula_direct_routing:clear_all(),

    %% Verify both miss
    ?assertEqual(miss, macula_direct_routing:lookup(NodeId1)),
    ?assertEqual(miss, macula_direct_routing:lookup(NodeId2)).

test_ttl_expiration() ->
    NodeId = crypto:strong_rand_bytes(32),
    Endpoint = <<"https://192.168.1.103:4433">>,

    %% Store endpoint
    ok = macula_direct_routing:store(NodeId, Endpoint),
    timer:sleep(10),

    %% Should hit immediately
    ?assertEqual({ok, Endpoint}, macula_direct_routing:lookup(NodeId)),

    %% Wait for TTL to expire (100ms + buffer)
    timer:sleep(150),

    %% Should miss after TTL
    ?assertEqual(miss, macula_direct_routing:lookup(NodeId)).

test_stats() ->
    NodeId1 = crypto:strong_rand_bytes(32),
    NodeId2 = crypto:strong_rand_bytes(32),
    Endpoint = <<"https://192.168.1.104:4433">>,

    %% Get initial stats
    InitStats = macula_direct_routing:stats(),
    InitHits = maps:get(hits, InitStats, 0),
    InitMisses = maps:get(misses, InitStats, 0),
    InitStores = maps:get(stores, InitStats, 0),

    %% Cause a miss
    _ = macula_direct_routing:lookup(NodeId1),
    timer:sleep(10),

    %% Store and cause a hit
    ok = macula_direct_routing:store(NodeId2, Endpoint),
    timer:sleep(10),
    _ = macula_direct_routing:lookup(NodeId2),
    timer:sleep(10),

    %% Check stats increased
    Stats = macula_direct_routing:stats(),
    ?assertEqual(InitHits + 1, maps:get(hits, Stats)),
    ?assertEqual(InitMisses + 1, maps:get(misses, Stats)),
    ?assertEqual(InitStores + 1, maps:get(stores, Stats)).

test_store_from_subscriber_atom() ->
    NodeId = crypto:strong_rand_bytes(32),
    Endpoint = <<"https://192.168.1.105:4433">>,

    %% Subscriber map with atom keys (from local DHT storage)
    Subscriber = #{node_id => NodeId, endpoint => Endpoint},

    ok = macula_direct_routing:store_from_subscriber(Subscriber),
    timer:sleep(10),

    ?assertEqual({ok, Endpoint}, macula_direct_routing:lookup(NodeId)).

test_store_from_subscriber_binary() ->
    NodeId = crypto:strong_rand_bytes(32),
    Endpoint = <<"https://192.168.1.106:4433">>,

    %% Subscriber map with binary keys (from protocol)
    Subscriber = #{<<"node_id">> => NodeId, <<"endpoint">> => Endpoint},

    ok = macula_direct_routing:store_from_subscriber(Subscriber),
    timer:sleep(10),

    ?assertEqual({ok, Endpoint}, macula_direct_routing:lookup(NodeId)).

test_store_from_subscriber_missing() ->
    %% Subscriber map missing required fields - should not crash
    Subscriber1 = #{node_id => crypto:strong_rand_bytes(32)},  % Missing endpoint
    Subscriber2 = #{endpoint => <<"https://192.168.1.107:4433">>},  % Missing node_id
    Subscriber3 = #{},  % Empty map

    %% These should all return ok without storing anything
    ?assertEqual(ok, macula_direct_routing:store_from_subscriber(Subscriber1)),
    ?assertEqual(ok, macula_direct_routing:store_from_subscriber(Subscriber2)),
    ?assertEqual(ok, macula_direct_routing:store_from_subscriber(Subscriber3)).
