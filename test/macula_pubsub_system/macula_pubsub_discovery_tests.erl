%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_pubsub_discovery module.
%%%
%%% Tests DHT integration for finding remote subscribers.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_discovery_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% find_subscribers Tests
%%%===================================================================

find_subscribers_success_test() ->
    %% GIVEN: A DHT lookup function that returns subscribers
    Pattern = <<"sensors.#">>,
    Subscribers = [
        #{node_id => <<"node1">>, address => {{1,2,3,4}, 5000}},
        #{node_id => <<"node2">>, address => {{5,6,7,8}, 5001}}
    ],
    DhtLookupFun = fun(_P) -> {ok, Subscribers} end,

    %% WHEN: Finding subscribers
    Result = macula_pubsub_discovery:find_subscribers(Pattern, DhtLookupFun),

    %% THEN: Should return the subscribers
    ?assertEqual({ok, Subscribers}, Result).

find_subscribers_empty_test() ->
    %% GIVEN: A DHT lookup function that returns empty list
    Pattern = <<"empty.pattern">>,
    DhtLookupFun = fun(_P) -> {ok, []} end,

    %% WHEN: Finding subscribers
    Result = macula_pubsub_discovery:find_subscribers(Pattern, DhtLookupFun),

    %% THEN: Should return empty list
    ?assertEqual({ok, []}, Result).

find_subscribers_error_test() ->
    %% GIVEN: A DHT lookup function that returns error
    Pattern = <<"error.pattern">>,
    DhtLookupFun = fun(_P) -> {error, network_timeout} end,

    %% WHEN: Finding subscribers
    Result = macula_pubsub_discovery:find_subscribers(Pattern, DhtLookupFun),

    %% THEN: Should return the error
    ?assertEqual({error, network_timeout}, Result).

find_subscribers_uses_pattern_test() ->
    %% GIVEN: A DHT lookup function that captures the pattern
    Self = self(),
    DhtLookupFun = fun(P) ->
        Self ! {lookup, P},
        {ok, []}
    end,
    Pattern = <<"test.topic.pattern">>,

    %% WHEN: Finding subscribers
    macula_pubsub_discovery:find_subscribers(Pattern, DhtLookupFun),

    %% THEN: Should have passed the pattern to lookup
    receive
        {lookup, ReceivedPattern} ->
            ?assertEqual(Pattern, ReceivedPattern)
    after 100 ->
        ?assert(false)
    end.

%%%===================================================================
%%% find_with_cache Tests
%%%===================================================================

find_with_cache_miss_test() ->
    %% GIVEN: Empty cache and DHT lookup
    Cache = macula_cache:new(10),
    Pattern = <<"cache.miss">>,
    Subscribers = [#{node_id => <<"node1">>, address => {{1,2,3,4}, 5000}}],
    DhtLookupFun = fun(_P) -> {ok, Subscribers} end,

    %% WHEN: Finding with cache
    Result = macula_pubsub_discovery:find_with_cache(Pattern, Cache, DhtLookupFun),

    %% THEN: Should return subscribers and updated cache
    ?assertMatch({ok, _, _}, Result),
    {ok, ReturnedSubs, _UpdatedCache} = Result,
    ?assertEqual(Subscribers, ReturnedSubs).

find_with_cache_hit_test() ->
    %% GIVEN: Cache with existing entry
    Cache0 = macula_cache:new(10),
    Pattern = <<"cache.hit">>,
    Subscribers = [#{node_id => <<"cached">>, address => {{1,2,3,4}, 5000}}],
    Timestamp = erlang:system_time(millisecond),
    Cache1 = macula_cache:put(Cache0, Pattern, Subscribers, Timestamp),

    %% DHT should NOT be called
    DhtLookupFun = fun(_P) ->
        throw(should_not_be_called)
    end,

    %% WHEN: Finding with cache
    Result = macula_pubsub_discovery:find_with_cache(Pattern, Cache1, DhtLookupFun),

    %% THEN: Should return cached subscribers
    ?assertMatch({ok, _, _}, Result),
    {ok, ReturnedSubs, _UpdatedCache} = Result,
    ?assertEqual(Subscribers, ReturnedSubs).

find_with_cache_custom_ttl_test() ->
    %% GIVEN: Cache and custom TTL
    Cache = macula_cache:new(10),
    Pattern = <<"custom.ttl">>,
    Subscribers = [#{node_id => <<"node1">>, address => {{1,2,3,4}, 5000}}],
    DhtLookupFun = fun(_P) -> {ok, Subscribers} end,
    TTL = 600,  % 10 minutes

    %% WHEN: Finding with custom TTL
    Result = macula_pubsub_discovery:find_with_cache(Pattern, Cache, DhtLookupFun, TTL),

    %% THEN: Should succeed
    ?assertMatch({ok, _, _}, Result).

find_with_cache_error_test() ->
    %% GIVEN: Cache and failing DHT lookup
    Cache = macula_cache:new(10),
    Pattern = <<"error.lookup">>,
    DhtLookupFun = fun(_P) -> {error, dht_unavailable} end,

    %% WHEN: Finding with cache
    Result = macula_pubsub_discovery:find_with_cache(Pattern, Cache, DhtLookupFun),

    %% THEN: Should return error with cache
    ?assertMatch({error, _, _}, Result),
    {error, Reason, _UpdatedCache} = Result,
    ?assertEqual(dht_unavailable, Reason).

%%%===================================================================
%%% announce Tests
%%%===================================================================

announce_success_test() ->
    %% GIVEN: DHT publish function that succeeds
    Pattern = <<"announce.test">>,
    NodeId = <<"local-node">>,
    Address = {{192,168,1,100}, 9443},
    DhtPublishFun = fun(_P, _N, _A) -> ok end,

    %% WHEN: Announcing subscription
    Result = macula_pubsub_discovery:announce(Pattern, NodeId, Address, DhtPublishFun),

    %% THEN: Should succeed
    ?assertEqual(ok, Result).

announce_error_test() ->
    %% GIVEN: DHT publish function that fails
    Pattern = <<"announce.error">>,
    NodeId = <<"local-node">>,
    Address = {{192,168,1,100}, 9443},
    DhtPublishFun = fun(_P, _N, _A) -> {error, write_failed} end,

    %% WHEN: Announcing subscription
    Result = macula_pubsub_discovery:announce(Pattern, NodeId, Address, DhtPublishFun),

    %% THEN: Should return error
    ?assertEqual({error, write_failed}, Result).

announce_passes_args_test() ->
    %% GIVEN: DHT publish function that captures arguments
    Pattern = <<"announce.args">>,
    NodeId = <<"test-node-123">>,
    Address = {{10,0,0,1}, 5000},
    Self = self(),
    DhtPublishFun = fun(P, N, A) ->
        Self ! {publish, P, N, A},
        ok
    end,

    %% WHEN: Announcing subscription
    macula_pubsub_discovery:announce(Pattern, NodeId, Address, DhtPublishFun),

    %% THEN: Should pass correct arguments
    receive
        {publish, ReceivedPattern, ReceivedNodeId, ReceivedAddress} ->
            ?assertEqual(Pattern, ReceivedPattern),
            ?assertEqual(NodeId, ReceivedNodeId),
            ?assertEqual(Address, ReceivedAddress)
    after 100 ->
        ?assert(false)
    end.

%%%===================================================================
%%% unannounce Tests
%%%===================================================================

unannounce_success_test() ->
    %% GIVEN: DHT unpublish function that succeeds
    Pattern = <<"unannounce.test">>,
    NodeId = <<"local-node">>,
    DhtUnpublishFun = fun(_P, _N) -> ok end,

    %% WHEN: Unannouncing subscription
    Result = macula_pubsub_discovery:unannounce(Pattern, NodeId, DhtUnpublishFun),

    %% THEN: Should succeed
    ?assertEqual(ok, Result).

unannounce_error_test() ->
    %% GIVEN: DHT unpublish function that fails
    Pattern = <<"unannounce.error">>,
    NodeId = <<"local-node">>,
    DhtUnpublishFun = fun(_P, _N) -> {error, not_found} end,

    %% WHEN: Unannouncing subscription
    Result = macula_pubsub_discovery:unannounce(Pattern, NodeId, DhtUnpublishFun),

    %% THEN: Should return error
    ?assertEqual({error, not_found}, Result).

unannounce_passes_args_test() ->
    %% GIVEN: DHT unpublish function that captures arguments
    Pattern = <<"unannounce.args">>,
    NodeId = <<"test-node-456">>,
    Self = self(),
    DhtUnpublishFun = fun(P, N) ->
        Self ! {unpublish, P, N},
        ok
    end,

    %% WHEN: Unannouncing subscription
    macula_pubsub_discovery:unannounce(Pattern, NodeId, DhtUnpublishFun),

    %% THEN: Should pass correct arguments
    receive
        {unpublish, ReceivedPattern, ReceivedNodeId} ->
            ?assertEqual(Pattern, ReceivedPattern),
            ?assertEqual(NodeId, ReceivedNodeId)
    after 100 ->
        ?assert(false)
    end.

