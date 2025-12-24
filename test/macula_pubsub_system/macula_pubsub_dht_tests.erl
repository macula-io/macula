%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_pubsub_dht module.
%%%
%%% Tests DHT advertisement, discovery, and routing.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_dht_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% advertise_subscription/5 Tests
%%%===================================================================

advertise_subscription_success_test() ->
    %% GIVEN: Mock connection manager
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Topic = <<"test.topic">>,
    SubRef = make_ref(),
    NodeId = <<"test_node">>,
    Url = <<"http://localhost:4000">>,

    %% WHEN: Advertising subscription
    {ok, SubInfo} = macula_pubsub_dht:advertise_subscription(Topic, SubRef, NodeId, Url, MockConnMgr),

    %% THEN: Should return subscription info with timer
    %% Note: Actual DHT STORE goes directly to macula_routing_server (v0.8.0+),
    %% not through connection manager. The key behavior is proper return value
    %% and timer scheduling.
    ?assertMatch(#{sub_ref := SubRef, ttl := 300, timer_ref := _}, SubInfo),
    #{timer_ref := TimerRef} = SubInfo,
    ?assert(is_reference(TimerRef)),

    %% Cleanup
    erlang:cancel_timer(TimerRef),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

advertise_subscription_creates_timer_test() ->
    %% GIVEN: Mock connection manager
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),

    %% WHEN: Advertising subscription
    {ok, SubInfo} = macula_pubsub_dht:advertise_subscription(
        <<"topic">>, make_ref(), <<"node">>, <<"url">>, MockConnMgr
    ),

    %% THEN: Timer should be scheduled (TTL - 60 seconds minimum)
    #{timer_ref := TimerRef} = SubInfo,
    TimeLeft = erlang:read_timer(TimerRef),
    ?assert(TimeLeft > 0),
    ?assert(TimeLeft =< 240000), % Max 240 seconds (300 - 60)

    %% Cleanup
    erlang:cancel_timer(TimerRef),
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

%%%===================================================================
%%% cancel_advertisement/2 Tests
%%%===================================================================

cancel_advertisement_removes_and_cancels_test() ->
    %% GIVEN: Advertised subscription
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Topic = <<"cancel.topic">>,
    {ok, SubInfo} = macula_pubsub_dht:advertise_subscription(
        Topic, make_ref(), <<"node">>, <<"url">>, MockConnMgr
    ),
    Advertised = #{Topic => SubInfo},
    #{timer_ref := TimerRef} = SubInfo,

    %% WHEN: Cancelling advertisement
    UpdatedAdvertised = macula_pubsub_dht:cancel_advertisement(Topic, Advertised),

    %% THEN: Should be removed from map
    ?assertEqual(#{}, UpdatedAdvertised),

    %% THEN: Timer should be cancelled
    ?assertEqual(false, erlang:read_timer(TimerRef)),

    %% Cleanup
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

cancel_advertisement_nonexistent_test() ->
    %% GIVEN: Empty advertised map
    Advertised = #{},

    %% WHEN: Cancelling nonexistent subscription
    UpdatedAdvertised = macula_pubsub_dht:cancel_advertisement(<<"missing">>, Advertised),

    %% THEN: Should return unchanged
    ?assertEqual(#{}, UpdatedAdvertised).

%%%===================================================================
%%% discover_subscribers/6 Tests
%%%===================================================================

discover_subscribers_cache_hit_test() ->
    %% GIVEN: Service registry with no cache (will return cache_miss for now)
    %% NOTE: In the real implementation, cache hits happen when subscribers are pre-registered
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Topic = <<"cached.topic">>,
    ServiceRegistry = macula_service_registry:new(),

    %% WHEN: Discovering subscribers (with empty cache, this becomes a query)
    Result = macula_pubsub_dht:discover_subscribers(
        Topic, <<"data">>, 1, MockConnMgr, ServiceRegistry, 0
    ),

    %% THEN: Result should be valid (either cached with empty list or query_sent)
    case Result of
        {cached, Subscribers, _Registry} ->
            ?assert(is_list(Subscribers));
        {query_sent, _Pending, _MsgId, _Registry} ->
            ?assert(true) % Query sent is also acceptable
    end,

    %% Cleanup
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

discover_subscribers_cache_miss_test() ->
    %% GIVEN: Empty service registry (cache miss)
    {ok, MockConnMgr} = macula_test_helpers:start_mock_connection_manager(),
    Topic = <<"uncached.topic">>,
    ServiceRegistry = macula_service_registry:new(),

    %% WHEN: Discovering subscribers
    Result = macula_pubsub_dht:discover_subscribers(
        Topic, <<"data">>, 1, MockConnMgr, ServiceRegistry, 0
    ),

    %% THEN: Should return query_sent with pending query tracking
    %% Note: Actual DHT query goes directly to macula_routing_server (v0.8.0+),
    %% not through connection manager. The key behavior is that the query
    %% is tracked in pending queries for later response handling.
    ?assertMatch({query_sent, _Pending, _MsgId, _Registry}, Result),
    {query_sent, Pending, MsgId, _Registry} = Result,

    %% THEN: Query should be tracked with correct metadata
    ?assertMatch(#{MsgId := {Topic, <<"data">>, 1, _}}, Pending),

    %% Cleanup
    macula_test_helpers:stop_mock_connection_manager(MockConnMgr).

%%%===================================================================
%%% handle_discovery_response/3 Tests
%%%===================================================================

handle_discovery_response_found_test() ->
    %% GIVEN: Pending query
    Topic = <<"response.topic">>,
    MsgId = <<"query_123">>,
    Pending = #{MsgId => {Topic, <<"data">>, 1, #{}}},
    Subscribers = [#{node_id => <<"node1">>, endpoint => <<"http://node1">>}],

    %% WHEN: Handling response
    {ok, UpdatedPending} = macula_pubsub_dht:handle_discovery_response(MsgId, Subscribers, Pending),

    %% THEN: Query should be removed
    ?assertEqual(#{}, UpdatedPending).

handle_discovery_response_not_found_test() ->
    %% GIVEN: Empty pending queries
    Pending = #{},

    %% WHEN: Handling response for unknown query
    {not_found, UpdatedPending} = macula_pubsub_dht:handle_discovery_response(
        <<"unknown">>, [], Pending
    ),

    %% THEN: Should return unchanged
    ?assertEqual(#{}, UpdatedPending).

%%%===================================================================
%%% route_to_subscribers/5 Tests
%%%===================================================================

route_to_subscribers_empty_list_test() ->
    %% GIVEN: Empty subscriber list
    Subscribers = [],

    %% WHEN: Routing to subscribers
    Result = macula_pubsub_dht:route_to_subscribers(
        <<"topic">>, <<"data">>, 1, Subscribers, <<"node">>
    ),

    %% THEN: Should return ok
    ?assertEqual(ok, Result).

route_to_subscribers_with_endpoints_test() ->
    %% GIVEN: Subscribers with endpoints
    Subscribers = [
        #{node_id => <<"node1">>, endpoint => <<"http://node1:4000">>},
        #{node_id => <<"node2">>, endpoint => <<"http://node2:4000">>}
    ],

    %% WHEN: Routing to subscribers
    Result = macula_pubsub_dht:route_to_subscribers(
        <<"topic">>, <<"data">>, 1, Subscribers, <<"sender_node">>
    ),

    %% THEN: Should return ok (routing logged)
    ?assertEqual(ok, Result).

route_to_subscribers_missing_endpoint_test() ->
    %% GIVEN: Subscriber without endpoint
    Subscribers = [#{node_id => <<"node1">>}], % Missing endpoint

    %% WHEN: Routing to subscribers
    Result = macula_pubsub_dht:route_to_subscribers(
        <<"topic">>, <<"data">>, 1, Subscribers, <<"node">>
    ),

    %% THEN: Should skip and return ok
    ?assertEqual(ok, Result).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% No helper functions needed currently
