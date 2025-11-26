%%%-------------------------------------------------------------------
%%% @doc Unit tests for macula_peer_connection_pool module.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peer_connection_pool_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

pool_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            {"initial stats show zero counts", fun test_initial_stats/0},
            {"stats track misses", fun test_stats_misses/0},
            {"return and get connection", fun test_return_and_get/0},
            {"invalidate removes connection", fun test_invalidate/0},
            {"pool enforces max connections", fun test_max_connections/0}
        ]
    }.

setup() ->
    %% Start pool with small max for testing
    {ok, Pid} = macula_peer_connection_pool:start_link(#{
        max_connections => 3,
        idle_timeout_ms => 60000
    }),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid).

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_initial_stats() ->
    Stats = macula_peer_connection_pool:stats(),
    ?assertEqual(0, maps:get(hits, Stats)),
    ?assertEqual(0, maps:get(misses, Stats)),
    ?assertEqual(0, maps:get(evictions, Stats)),
    ?assertEqual(0, maps:get(pool_size, Stats)).

test_stats_misses() ->
    %% This will fail because no real endpoint exists, but it should count as a miss
    Endpoint = <<"127.0.0.1:9999">>,
    _Result = macula_peer_connection_pool:get_connection(Endpoint),

    Stats = macula_peer_connection_pool:stats(),
    ?assertEqual(1, maps:get(misses, Stats)).

test_return_and_get() ->
    %% Simulate returning a connection to the pool
    Endpoint = <<"test.endpoint:1234">>,
    MockConn = make_ref(),
    MockStream = make_ref(),

    %% Return connection to pool
    ok = macula_peer_connection_pool:return_connection(Endpoint, {MockConn, MockStream}),

    %% Small delay for async cast to complete
    timer:sleep(10),

    Stats = macula_peer_connection_pool:stats(),
    ?assertEqual(1, maps:get(pool_size, Stats)).

test_invalidate() ->
    %% Add a connection
    Endpoint = <<"test.invalidate:5678">>,
    MockConn = make_ref(),
    MockStream = make_ref(),

    ok = macula_peer_connection_pool:return_connection(Endpoint, {MockConn, MockStream}),
    timer:sleep(10),

    %% Verify it's in pool
    Stats1 = macula_peer_connection_pool:stats(),
    ?assertEqual(1, maps:get(pool_size, Stats1)),

    %% Invalidate it
    ok = macula_peer_connection_pool:invalidate(Endpoint),
    timer:sleep(10),

    %% Verify it's removed
    Stats2 = macula_peer_connection_pool:stats(),
    ?assertEqual(0, maps:get(pool_size, Stats2)).

test_max_connections() ->
    %% Add 4 connections to a pool with max 3
    Endpoints = [
        <<"endpoint1:1111">>,
        <<"endpoint2:2222">>,
        <<"endpoint3:3333">>,
        <<"endpoint4:4444">>
    ],

    lists:foreach(fun(Endpoint) ->
        MockConn = make_ref(),
        MockStream = make_ref(),
        ok = macula_peer_connection_pool:return_connection(Endpoint, {MockConn, MockStream}),
        timer:sleep(5)
    end, Endpoints),

    timer:sleep(20),

    Stats = macula_peer_connection_pool:stats(),
    %% Pool should have evicted oldest to stay at max 3
    ?assertEqual(3, maps:get(pool_size, Stats)),
    ?assert(maps:get(evictions, Stats) >= 1).
