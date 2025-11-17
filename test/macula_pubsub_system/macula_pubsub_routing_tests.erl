%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_pubsub_routing module.
%%% Tests pub/sub routing message handling (routed PUBLISH processing).
%%%
%%% TDD Approach:
%%% 1. Write failing tests first
%%% 2. Implement minimal functionality
%%% 3. Make tests pass incrementally
%%% 4. Refactor for idiomatic Erlang
%%%
%%% Responsibilities:
%%% - Wrap PUBLISH messages in pubsub_route envelopes
%%% - Determine if message should be delivered locally or forwarded
%%% - Route pubsub_route messages to next hop via XOR distance
%%% - Handle TTL exhaustion and no-route errors
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_routing_tests).

-include_lib("eunit/include/eunit.hrl").

%% gen_server callbacks for mock routing server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% No setup needed - functional module (stateless)

%%%===================================================================
%%% Basic API Tests
%%%===================================================================

module_exports_test() ->
    Exports = macula_pubsub_routing:module_info(exports),

    ?assert(lists:member({wrap_publish, 4}, Exports)),
    ?assert(lists:member({route_or_deliver, 3}, Exports)),
    ?assert(lists:member({should_deliver_locally, 2}, Exports)).

%%%===================================================================
%%% wrap_publish/4 Tests
%%%===================================================================

wrap_publish_creates_valid_envelope_test() ->
    SourceNodeId = crypto:strong_rand_bytes(32),
    DestNodeId = crypto:strong_rand_bytes(32),
    Topic = <<"test.topic">>,
    PublishMsg = #{
        <<"topic">> => Topic,
        <<"payload">> => <<"test payload">>,
        <<"qos">> => 1,
        <<"retain">> => false,
        <<"message_id">> => crypto:strong_rand_bytes(16)
    },
    MaxHops = 10,

    Result = macula_pubsub_routing:wrap_publish(SourceNodeId, DestNodeId, PublishMsg, MaxHops),

    ?assertEqual(SourceNodeId, maps:get(<<"source_node_id">>, Result)),
    ?assertEqual(DestNodeId, maps:get(<<"destination_node_id">>, Result)),
    ?assertEqual(0, maps:get(<<"hop_count">>, Result)),
    ?assertEqual(MaxHops, maps:get(<<"max_hops">>, Result)),
    ?assertEqual(Topic, maps:get(<<"topic">>, Result)),
    ?assertEqual(PublishMsg, maps:get(<<"payload">>, Result)).

wrap_publish_extracts_topic_from_publish_msg_test() ->
    SourceNodeId = crypto:strong_rand_bytes(32),
    DestNodeId = crypto:strong_rand_bytes(32),
    Topic = <<"arcade.matchmaking.snake">>,
    PublishMsg = #{
        <<"topic">> => Topic,
        <<"payload">> => <<"matchmaking data">>,
        <<"qos">> => 0,
        <<"retain">> => false,
        <<"message_id">> => crypto:strong_rand_bytes(16)
    },

    Result = macula_pubsub_routing:wrap_publish(SourceNodeId, DestNodeId, PublishMsg, 10),

    %% Topic should be extracted and placed at top level
    ?assertEqual(Topic, maps:get(<<"topic">>, Result)).

wrap_publish_initializes_hop_count_to_zero_test() ->
    SourceNodeId = crypto:strong_rand_bytes(32),
    DestNodeId = crypto:strong_rand_bytes(32),
    PublishMsg = #{
        <<"topic">> => <<"test">>,
        <<"payload">> => <<>>,
        <<"qos">> => 0,
        <<"retain">> => false,
        <<"message_id">> => crypto:strong_rand_bytes(16)
    },

    Result = macula_pubsub_routing:wrap_publish(SourceNodeId, DestNodeId, PublishMsg, 10),

    ?assertEqual(0, maps:get(<<"hop_count">>, Result)).

%%%===================================================================
%%% should_deliver_locally/2 Tests
%%%===================================================================

should_deliver_locally_returns_true_when_destination_matches_test() ->
    LocalNodeId = crypto:strong_rand_bytes(32),
    PubSubRouteMsg = #{
        <<"destination_node_id">> => LocalNodeId,
        <<"source_node_id">> => crypto:strong_rand_bytes(32),
        <<"hop_count">> => 0,
        <<"max_hops">> => 10,
        <<"topic">> => <<"test">>,
        <<"payload">> => #{}
    },

    Result = macula_pubsub_routing:should_deliver_locally(LocalNodeId, PubSubRouteMsg),

    ?assertEqual(true, Result).

should_deliver_locally_returns_false_when_destination_differs_test() ->
    LocalNodeId = crypto:strong_rand_bytes(32),
    DifferentNodeId = crypto:strong_rand_bytes(32),
    PubSubRouteMsg = #{
        <<"destination_node_id">> => DifferentNodeId,
        <<"source_node_id">> => crypto:strong_rand_bytes(32),
        <<"hop_count">> => 0,
        <<"max_hops">> => 10,
        <<"topic">> => <<"test">>,
        <<"payload">> => #{}
    },

    Result = macula_pubsub_routing:should_deliver_locally(LocalNodeId, PubSubRouteMsg),

    ?assertEqual(false, Result).

%%%===================================================================
%%% route_or_deliver/3 Tests - Local Delivery
%%%===================================================================

route_or_deliver_delivers_locally_when_destination_matches_test() ->
    LocalNodeId = crypto:strong_rand_bytes(32),
    SourceNodeId = crypto:strong_rand_bytes(32),
    Topic = <<"test.topic">>,
    PublishMsg = #{
        <<"topic">> => Topic,
        <<"payload">> => <<"test data">>,
        <<"qos">> => 1,
        <<"retain">> => false,
        <<"message_id">> => crypto:strong_rand_bytes(16)
    },

    PubSubRouteMsg = #{
        <<"destination_node_id">> => LocalNodeId,
        <<"source_node_id">> => SourceNodeId,
        <<"hop_count">> => 0,
        <<"max_hops">> => 10,
        <<"topic">> => Topic,
        <<"payload">> => PublishMsg
    },

    %% No routing server needed for local delivery
    Result = macula_pubsub_routing:route_or_deliver(LocalNodeId, PubSubRouteMsg, undefined),

    ?assertMatch({deliver, _, _}, Result),
    {deliver, ResultTopic, ResultPayload} = Result,
    ?assertEqual(Topic, ResultTopic),
    ?assertEqual(PublishMsg, ResultPayload).

route_or_deliver_delivers_locally_after_multiple_hops_test() ->
    LocalNodeId = crypto:strong_rand_bytes(32),
    SourceNodeId = crypto:strong_rand_bytes(32),
    Topic = <<"multi.hop.topic">>,
    PublishMsg = #{
        <<"topic">> => Topic,
        <<"payload">> => <<"final destination">>,
        <<"qos">> => 0,
        <<"retain">> => false,
        <<"message_id">> => crypto:strong_rand_bytes(16)
    },

    %% Simulate message that has hopped 3 times
    PubSubRouteMsg = #{
        <<"destination_node_id">> => LocalNodeId,
        <<"source_node_id">> => SourceNodeId,
        <<"hop_count">> => 3,
        <<"max_hops">> => 10,
        <<"topic">> => Topic,
        <<"payload">> => PublishMsg
    },

    Result = macula_pubsub_routing:route_or_deliver(LocalNodeId, PubSubRouteMsg, undefined),

    ?assertMatch({deliver, Topic, PublishMsg}, Result).

%%%===================================================================
%%% route_or_deliver/3 Tests - Forwarding
%%%===================================================================

route_or_deliver_forwards_to_next_hop_test_() ->
    {setup,
     fun() ->
        %% Start mock routing server
        DestNodeId = crypto:strong_rand_bytes(32),
        {ok, RoutingPid} = start_mock_routing_server(DestNodeId),
        #{routing => RoutingPid, dest => DestNodeId}
     end,
     fun(#{routing := RoutingPid}) ->
        catch gen_server:stop(RoutingPid),
        ok
     end,
     fun(#{routing := RoutingPid, dest := DestNodeId}) ->
        LocalNodeId = crypto:strong_rand_bytes(32),
        SourceNodeId = crypto:strong_rand_bytes(32),
        Topic = <<"forward.topic">>,
        PublishMsg = #{
            <<"topic">> => Topic,
            <<"payload">> => <<"forward me">>,
            <<"qos">> => 1,
            <<"retain">> => false,
            <<"message_id">> => crypto:strong_rand_bytes(16)
        },

        PubSubRouteMsg = #{
            <<"destination_node_id">> => DestNodeId,
            <<"source_node_id">> => SourceNodeId,
            <<"hop_count">> => 0,
            <<"max_hops">> => 10,
            <<"topic">> => Topic,
            <<"payload">> => PublishMsg
        },

        Result = macula_pubsub_routing:route_or_deliver(LocalNodeId, PubSubRouteMsg, RoutingPid),

        [
            ?_assertMatch({forward, _, _}, Result),
            fun() ->
                {forward, NextHop, UpdatedMsg} = Result,
                %% Next hop should be returned
                ?assertMatch(#{node_id := _}, NextHop),
                %% Hop count should be incremented
                ?assertEqual(1, maps:get(<<"hop_count">>, UpdatedMsg))
            end
        ]
     end}.

route_or_deliver_increments_hop_count_test_() ->
    {setup,
     fun() ->
        DestNodeId = crypto:strong_rand_bytes(32),
        {ok, RoutingPid} = start_mock_routing_server(DestNodeId),
        #{routing => RoutingPid, dest => DestNodeId}
     end,
     fun(#{routing := RoutingPid}) ->
        catch gen_server:stop(RoutingPid),
        ok
     end,
     fun(#{routing := RoutingPid, dest := DestNodeId}) ->
        LocalNodeId = crypto:strong_rand_bytes(32),
        Topic = <<"test">>,
        PublishMsg = #{
            <<"topic">> => Topic,
            <<"payload">> => <<>>,
            <<"qos">> => 0,
            <<"retain">> => false,
            <<"message_id">> => crypto:strong_rand_bytes(16)
        },

        %% Start at hop 2
        PubSubRouteMsg = #{
            <<"destination_node_id">> => DestNodeId,
            <<"source_node_id">> => crypto:strong_rand_bytes(32),
            <<"hop_count">> => 2,
            <<"max_hops">> => 10,
            <<"topic">> => Topic,
            <<"payload">> => PublishMsg
        },

        Result = macula_pubsub_routing:route_or_deliver(LocalNodeId, PubSubRouteMsg, RoutingPid),

        [
            fun() ->
                {forward, _NextHop, UpdatedMsg} = Result,
                %% Should increment to 3
                ?assertEqual(3, maps:get(<<"hop_count">>, UpdatedMsg))
            end
        ]
     end}.

%%%===================================================================
%%% route_or_deliver/3 Tests - TTL Exhaustion
%%%===================================================================

route_or_deliver_returns_error_when_max_hops_reached_test() ->
    LocalNodeId = crypto:strong_rand_bytes(32),
    DestNodeId = crypto:strong_rand_bytes(32),
    Topic = <<"ttl.expired">>,
    PublishMsg = #{
        <<"topic">> => Topic,
        <<"payload">> => <<"too many hops">>,
        <<"qos">> => 0,
        <<"retain">> => false,
        <<"message_id">> => crypto:strong_rand_bytes(16)
    },

    %% hop_count >= max_hops
    PubSubRouteMsg = #{
        <<"destination_node_id">> => DestNodeId,
        <<"source_node_id">> => crypto:strong_rand_bytes(32),
        <<"hop_count">> => 10,
        <<"max_hops">> => 10,
        <<"topic">> => Topic,
        <<"payload">> => PublishMsg
    },

    Result = macula_pubsub_routing:route_or_deliver(LocalNodeId, PubSubRouteMsg, undefined),

    ?assertMatch({error, max_hops_exceeded}, Result).

route_or_deliver_returns_error_when_max_hops_exceeded_test() ->
    LocalNodeId = crypto:strong_rand_bytes(32),
    DestNodeId = crypto:strong_rand_bytes(32),
    Topic = <<"ttl.expired">>,
    PublishMsg = #{
        <<"topic">> => Topic,
        <<"payload">> => <<"way too many hops">>,
        <<"qos">> => 0,
        <<"retain">> => false,
        <<"message_id">> => crypto:strong_rand_bytes(16)
    },

    %% hop_count > max_hops
    PubSubRouteMsg = #{
        <<"destination_node_id">> => DestNodeId,
        <<"source_node_id">> => crypto:strong_rand_bytes(32),
        <<"hop_count">> => 15,
        <<"max_hops">> => 10,
        <<"topic">> => Topic,
        <<"payload">> => PublishMsg
    },

    Result = macula_pubsub_routing:route_or_deliver(LocalNodeId, PubSubRouteMsg, undefined),

    ?assertMatch({error, max_hops_exceeded}, Result).

%%%===================================================================
%%% route_or_deliver/3 Tests - No Route
%%%===================================================================

route_or_deliver_returns_error_when_no_route_found_test_() ->
    {setup,
     fun() ->
        %% Start mock routing server that returns empty list
        {ok, RoutingPid} = start_mock_routing_server_no_route(),
        RoutingPid
     end,
     fun(RoutingPid) ->
        catch gen_server:stop(RoutingPid),
        ok
     end,
     fun(RoutingPid) ->
        LocalNodeId = crypto:strong_rand_bytes(32),
        DestNodeId = crypto:strong_rand_bytes(32),
        Topic = <<"no.route">>,
        PublishMsg = #{
            <<"topic">> => Topic,
            <<"payload">> => <<"isolated node">>,
            <<"qos">> => 0,
            <<"retain">> => false,
            <<"message_id">> => crypto:strong_rand_bytes(16)
        },

        PubSubRouteMsg = #{
            <<"destination_node_id">> => DestNodeId,
            <<"source_node_id">> => crypto:strong_rand_bytes(32),
            <<"hop_count">> => 0,
            <<"max_hops">> => 10,
            <<"topic">> => Topic,
            <<"payload">> => PublishMsg
        },

        Result = macula_pubsub_routing:route_or_deliver(LocalNodeId, PubSubRouteMsg, RoutingPid),

        [?_assertMatch({error, no_route}, Result)]
     end}.

%%%===================================================================
%%% Mock Routing Server
%%%===================================================================

%% Mock routing server that returns a next hop
start_mock_routing_server(DestNodeId) ->
    NextHop = #{
        node_id => DestNodeId,
        address => <<"192.168.1.10">>,
        port => 4443
    },
    gen_server:start_link(?MODULE, {next_hop, NextHop}, []).

%% Mock routing server that returns empty list (no route)
start_mock_routing_server_no_route() ->
    gen_server:start_link(?MODULE, no_route, []).

%% gen_server callbacks
init({next_hop, NextHop}) ->
    {ok, #{next_hop => NextHop}};
init(no_route) ->
    {ok, #{no_route => true}}.

handle_call({find_closest, _DestNodeId, _K}, _From, #{next_hop := NextHop} = State) ->
    {reply, [NextHop], State};
handle_call({find_closest, _DestNodeId, _K}, _From, #{no_route := true} = State) ->
    {reply, [], State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
