%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_pubsub_discovery module.
%%% Tests written FIRST (TDD red phase).
%%% DHT integration for finding remote subscribers.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_discovery_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Mock DHT Setup
%%%===================================================================

%% Mock DHT that returns predefined subscribers for testing
mock_dht_lookup(_Pattern) ->
    {ok, [
        #{node_id => <<1:256>>, address => {{127,0,0,1}, 8080}},
        #{node_id => <<2:256>>, address => {{127,0,0,1}, 8081}}
    ]}.

mock_dht_lookup_empty(_Pattern) ->
    {ok, []}.

mock_dht_lookup_error(_Pattern) ->
    {error, timeout}.

%%%===================================================================
%%% Find Subscribers Tests
%%%===================================================================

%% Test: find_subscribers returns remote subscribers from DHT
find_subscribers_from_dht_test() ->
    Pattern = <<"be.cortexiq.#">>,

    Result = macula_pubsub_discovery:find_subscribers(Pattern, fun mock_dht_lookup/1),

    ?assertMatch({ok, [_, _]}, Result),
    {ok, Subscribers} = Result,
    ?assertEqual(2, length(Subscribers)).

%% Test: find_subscribers returns empty list when no remote subscribers
find_subscribers_empty_test() ->
    Pattern = <<"be.cortexiq.#">>,

    Result = macula_pubsub_discovery:find_subscribers(Pattern, fun mock_dht_lookup_empty/1),

    ?assertEqual({ok, []}, Result).

%% Test: find_subscribers propagates DHT errors
find_subscribers_error_test() ->
    Pattern = <<"be.cortexiq.#">>,

    Result = macula_pubsub_discovery:find_subscribers(Pattern, fun mock_dht_lookup_error/1),

    ?assertEqual({error, timeout}, Result).

%%%===================================================================
%%% Find with Cache Tests
%%%===================================================================

%% Test: find_with_cache checks cache first
find_with_cache_hit_test() ->
    Cache = macula_pubsub_cache:new(100),
    Pattern = <<"be.cortexiq.#">>,
    CachedSubs = [#{node_id => <<3:256>>, address => {{127,0,0,1}, 9000}}],

    %% Populate cache
    Cache2 = macula_pubsub_cache:put(Cache, Pattern, CachedSubs),

    %% Should return cached value, not call DHT
    {ok, Subscribers, _Cache3} = macula_pubsub_discovery:find_with_cache(
        Pattern, Cache2, fun(_) -> {error, should_not_be_called} end
    ),

    ?assertEqual(CachedSubs, Subscribers).

%% Test: find_with_cache queries DHT on cache miss
find_with_cache_miss_test() ->
    Cache = macula_pubsub_cache:new(100),
    Pattern = <<"be.cortexiq.#">>,

    %% Cache miss, should call DHT
    {ok, Subscribers, Cache2} = macula_pubsub_discovery:find_with_cache(
        Pattern, Cache, fun mock_dht_lookup/1
    ),

    ?assertEqual(2, length(Subscribers)),

    %% Should have cached the result
    ?assertEqual(1, macula_pubsub_cache:size(Cache2)).

%% Test: find_with_cache respects TTL expiration
find_with_cache_expired_test() ->
    Cache = macula_pubsub_cache:new(100),
    Pattern = <<"be.cortexiq.#">>,
    OldSubs = [#{node_id => <<3:256>>, address => {{127,0,0,1}, 9000}}],

    %% Put with old timestamp (400 seconds ago)
    OldTimestamp = erlang:system_time(millisecond) - 400000,
    Cache2 = macula_pubsub_cache:put_with_timestamp(Cache, Pattern, OldSubs, OldTimestamp),

    %% With TTL of 300 seconds, entry is expired
    TTL = 300,

    {ok, Subscribers, _Cache3} = macula_pubsub_discovery:find_with_cache(
        Pattern, Cache2, fun mock_dht_lookup/1, TTL
    ),

    %% Should have queried DHT, not used expired cache
    ?assertEqual(2, length(Subscribers)).

%% Test: find_with_cache uses fresh cache entry
find_with_cache_fresh_test() ->
    Cache = macula_pubsub_cache:new(100),
    Pattern = <<"be.cortexiq.#">>,
    FreshSubs = [#{node_id => <<3:256>>, address => {{127,0,0,1}, 9000}}],

    %% Put with recent timestamp
    Cache2 = macula_pubsub_cache:put(Cache, Pattern, FreshSubs),

    %% With TTL of 300 seconds, entry is fresh
    TTL = 300,

    {ok, Subscribers, _Cache3} = macula_pubsub_discovery:find_with_cache(
        Pattern, Cache2, fun(_) -> {error, should_not_be_called} end, TTL
    ),

    %% Should have used cache
    ?assertEqual(FreshSubs, Subscribers).

%%%===================================================================
%%% Announce Tests
%%%===================================================================

%% Test: announce publishes local subscription to DHT
announce_test() ->
    Pattern = <<"be.cortexiq.#">>,
    LocalNodeId = <<123:256>>,
    LocalAddress = {{192,168,1,100}, 8080},

    %% Mock DHT publish function
    PublishFun = fun(P, NodeId, Addr) ->
        ?assertEqual(Pattern, P),
        ?assertEqual(LocalNodeId, NodeId),
        ?assertEqual(LocalAddress, Addr),
        ok
    end,

    Result = macula_pubsub_discovery:announce(Pattern, LocalNodeId, LocalAddress, PublishFun),

    ?assertEqual(ok, Result).

%% Test: announce propagates DHT errors
announce_error_test() ->
    Pattern = <<"be.cortexiq.#">>,
    LocalNodeId = <<123:256>>,
    LocalAddress = {{192,168,1,100}, 8080},

    %% Mock DHT publish that fails
    PublishFun = fun(_, _, _) -> {error, network_error} end,

    Result = macula_pubsub_discovery:announce(Pattern, LocalNodeId, LocalAddress, PublishFun),

    ?assertEqual({error, network_error}, Result).

%%%===================================================================
%%% Unannounce Tests
%%%===================================================================

%% Test: unannounce removes local subscription from DHT
unannounce_test() ->
    Pattern = <<"be.cortexiq.#">>,
    LocalNodeId = <<123:256>>,

    %% Mock DHT unpublish function
    UnpublishFun = fun(P, NodeId) ->
        ?assertEqual(Pattern, P),
        ?assertEqual(LocalNodeId, NodeId),
        ok
    end,

    Result = macula_pubsub_discovery:unannounce(Pattern, LocalNodeId, UnpublishFun),

    ?assertEqual(ok, Result).

%% Test: unannounce propagates DHT errors
unannounce_error_test() ->
    Pattern = <<"be.cortexiq.#">>,
    LocalNodeId = <<123:256>>,

    %% Mock DHT unpublish that fails
    UnpublishFun = fun(_, _) -> {error, not_found} end,

    Result = macula_pubsub_discovery:unannounce(Pattern, LocalNodeId, UnpublishFun),

    ?assertEqual({error, not_found}, Result).

%%%===================================================================
%%% Refresh Tests
%%%===================================================================

%% Test: refresh_cache invalidates expired entries
refresh_cache_test() ->
    Cache = macula_pubsub_cache:new(100),
    Pattern1 = <<"be.cortexiq.#">>,
    Pattern2 = <<"org.example.*">>,

    %% Pattern1: expired (400 seconds ago)
    OldTimestamp = erlang:system_time(millisecond) - 400000,
    Cache2 = macula_pubsub_cache:put_with_timestamp(Cache, Pattern1, [], OldTimestamp),

    %% Pattern2: fresh (10 seconds ago)
    FreshTimestamp = erlang:system_time(millisecond) - 10000,
    Cache3 = macula_pubsub_cache:put_with_timestamp(Cache2, Pattern2, [], FreshTimestamp),

    ?assertEqual(2, macula_pubsub_cache:size(Cache3)),

    %% Refresh with TTL of 300 seconds
    TTL = 300,
    Cache4 = macula_pubsub_discovery:refresh_cache(Cache3, TTL),

    %% Should have removed expired Pattern1, kept fresh Pattern2
    ?assertEqual(1, macula_pubsub_cache:size(Cache4)),
    ?assertEqual(not_found, macula_pubsub_cache:get(Cache4, Pattern1)),
    ?assertMatch({ok, _, _}, macula_pubsub_cache:get(Cache4, Pattern2)).
