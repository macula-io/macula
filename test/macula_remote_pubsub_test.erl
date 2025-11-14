%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for remote pub/sub message routing.
%%% Tests routing of published messages to remote subscribers discovered via DHT.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_remote_pubsub_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

remote_routing_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"route message to single remote subscriber", fun test_route_to_single_subscriber/0},
         {"route message to multiple remote subscribers", fun test_route_to_multiple_subscribers/0},
         {"handle empty subscriber list gracefully", fun test_empty_subscribers/0},
         {"extract subscriber endpoint correctly", fun test_extract_endpoint/0},
         {"build remote publish message", fun test_build_remote_publish_message/0}
     ]}.

setup() ->
    application:ensure_all_started(macula),
    ok.

cleanup(_) ->
    ok.

%%%===================================================================
%%% Unit Tests
%%%===================================================================

test_route_to_single_subscriber() ->
    %% GIVEN: A single remote subscriber
    Subscriber = #{
        node_id => <<"remote-node-1">>,
        endpoint => <<"https://remote1:9443">>,
        ttl => 300
    },
    Subscribers = [Subscriber],

    %% WHEN: We extract routing information
    Endpoints = extract_endpoints(Subscribers),

    %% THEN: We should get the correct endpoint
    ?assertEqual([<<"https://remote1:9443">>], Endpoints).

test_route_to_multiple_subscribers() ->
    %% GIVEN: Multiple remote subscribers
    Subscribers = [
        #{node_id => <<"remote-node-1">>, endpoint => <<"https://remote1:9443">>, ttl => 300},
        #{node_id => <<"remote-node-2">>, endpoint => <<"https://remote2:9443">>, ttl => 300},
        #{node_id => <<"remote-node-3">>, endpoint => <<"https://remote3:9443">>, ttl => 300}
    ],

    %% WHEN: We extract routing information
    Endpoints = extract_endpoints(Subscribers),

    %% THEN: We should get all endpoints
    ?assertEqual(3, length(Endpoints)),
    ?assert(lists:member(<<"https://remote1:9443">>, Endpoints)),
    ?assert(lists:member(<<"https://remote2:9443">>, Endpoints)),
    ?assert(lists:member(<<"https://remote3:9443">>, Endpoints)).

test_empty_subscribers() ->
    %% GIVEN: Empty subscriber list
    Subscribers = [],

    %% WHEN: We extract routing information
    Endpoints = extract_endpoints(Subscribers),

    %% THEN: We should get empty list
    ?assertEqual([], Endpoints).

test_extract_endpoint() ->
    %% GIVEN: A subscriber map
    Subscriber = #{
        node_id => <<"test-node">>,
        endpoint => <<"https://test:9443">>,
        ttl => 300,
        metadata => #{}
    },

    %% WHEN: We extract the endpoint
    Endpoint = maps:get(endpoint, Subscriber),

    %% THEN: It should match expected value
    ?assertEqual(<<"https://test:9443">>, Endpoint).

test_build_remote_publish_message() ->
    %% GIVEN: Topic, payload, and QoS
    Topic = <<"sensor.temp.room1">>,
    Payload = <<"{\"temperature\": 22.5}">>,
    Qos = 0,
    MessageId = <<"msg-123">>,

    %% WHEN: We build a publish message
    PubMsg = build_publish_message(Topic, Payload, Qos, MessageId),

    %% THEN: It should have correct structure
    ?assertMatch(#{topic := _, payload := _, qos := _, message_id := _}, PubMsg),
    ?assertEqual(Topic, maps:get(topic, PubMsg)),
    ?assertEqual(Payload, maps:get(payload, PubMsg)),
    ?assertEqual(Qos, maps:get(qos, PubMsg)),
    ?assertEqual(MessageId, maps:get(message_id, PubMsg)).

%%%===================================================================
%%% Helper Functions (to be implemented in macula_connection.erl)
%%%===================================================================

%% @doc Extract endpoints from list of subscribers
-spec extract_endpoints(list()) -> list(binary()).
extract_endpoints(Subscribers) ->
    lists:map(fun(Sub) ->
        maps:get(endpoint, Sub)
    end, Subscribers).

%% @doc Build a publish message for remote routing
-spec build_publish_message(binary(), binary(), integer(), binary()) -> map().
build_publish_message(Topic, Payload, Qos, MessageId) ->
    #{
        topic => Topic,
        payload => Payload,
        qos => Qos,
        message_id => MessageId,
        retain => false
    }.
