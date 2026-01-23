%%%-------------------------------------------------------------------
%%% @doc
%%% DHT operations for pub/sub - handles subscription advertisement and discovery.
%%%
%%% Responsibilities:
%%% - Advertise subscriptions in DHT with TTL
%%% - Schedule re-advertisement timers
%%% - Discover remote subscribers via DHT queries
%%% - Route messages to remote subscribers
%%% - Track pending DHT queries
%%%
%%% Extracted from macula_pubsub_handler.erl (Phase 3)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_dht).

-include_lib("kernel/include/logger.hrl").

%% API
-export([advertise_subscription/5, cancel_advertisement/2,
         discover_subscribers/6, handle_discovery_response/3,
         route_to_subscribers/5]).

-type topic() :: binary().
-type node_id() :: binary().
-type url() :: binary().
-type connection_manager_pid() :: pid().
-type subscription_ref() :: reference().
-type payload() :: binary().
-type qos() :: 0 | 1.

-type advertised_subscriptions() :: #{topic() => #{
    sub_ref := reference(),
    ttl := pos_integer(),
    timer_ref := reference()
}}.

-type pending_queries() :: #{binary() => {topic(), payload(), qos(), map()}}.

-export_type([advertised_subscriptions/0, pending_queries/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Advertise a subscription in the DHT.
%% Sends STORE message to DHT and schedules re-advertisement.
%% Returns {ok, SubInfo}.
-spec advertise_subscription(topic(), subscription_ref(), node_id(), url(), connection_manager_pid()) ->
    {ok, #{sub_ref := reference(), ttl := pos_integer(), timer_ref := reference()}} | {error, term()}.
advertise_subscription(Topic, SubRef, NodeId, Url, _ConnMgrPid) ->
    %% Default TTL for subscriptions is 300 seconds (5 minutes)
    TTL = 300,

    %% Create DHT key by hashing the topic
    TopicKey = crypto:hash(sha256, Topic),

    %% Create subscriber value with endpoint info for routing messages back
    SubscriberValue = #{
        node_id => NodeId,
        endpoint => Url,
        ttl => TTL
    },

    %% Store subscription in DHT with propagation to k closest nodes (v0.8.0+)
    ?LOG_INFO("[~s] Advertising subscription to topic ~s in DHT", [NodeId, Topic]),
    store_subscription_in_dht(NodeId, Topic, TopicKey, SubscriberValue),

    %% Schedule re-advertisement (TTL - 60 seconds, minimum 10 seconds)
    ResubInterval = max(10, TTL - 60) * 1000,  % milliseconds
    TimerRef = erlang:send_after(ResubInterval, self(), {resubscribe, Topic}),
    ?LOG_DEBUG("[~s] Scheduled re-subscription for ~s in ~p seconds",
              [NodeId, Topic, ResubInterval div 1000]),

    %% Return subscription info
    SubInfo = #{
        sub_ref => SubRef,
        ttl => TTL,
        timer_ref => TimerRef
    },
    {ok, SubInfo}.

%% @doc Cancel advertisement for a topic.
%% Cancels the re-advertisement timer.
%% Returns updated advertised_subscriptions map.
-spec cancel_advertisement(topic(), advertised_subscriptions()) -> advertised_subscriptions().
cancel_advertisement(Topic, AdvertisedSubscriptions) ->
    SubInfo = maps:get(Topic, AdvertisedSubscriptions, undefined),
    do_cancel_advertisement(SubInfo, Topic, AdvertisedSubscriptions).

%% @private Not advertised, return unchanged
do_cancel_advertisement(undefined, _Topic, AdvertisedSubscriptions) ->
    AdvertisedSubscriptions;
%% @private Cancel timer and remove from map
do_cancel_advertisement(SubInfo, Topic, AdvertisedSubscriptions) ->
    TimerRef = maps:get(timer_ref, SubInfo),
    erlang:cancel_timer(TimerRef),
    maps:remove(Topic, AdvertisedSubscriptions).

%% @doc Discover remote subscribers for a topic.
%% Checks cache first, queries DHT on cache miss.
%% Returns {cached, Subscribers, Registry} | {query_sent, Pending, MsgId, Registry}.
-spec discover_subscribers(topic(), payload(), qos(), connection_manager_pid(), term(), non_neg_integer()) ->
    {cached, list(), term()} | {query_sent, pending_queries(), binary(), term()}.
discover_subscribers(Topic, Payload, Qos, ConnMgrPid, ServiceRegistry, MsgIdCounter) ->
    %% Check subscriber cache first
    case macula_service_registry:discover_subscribers(ServiceRegistry, Topic) of
        {ok, Subscribers, UpdatedRegistry} ->
            %% Cache hit - return cached subscribers
            ?LOG_DEBUG("Cache hit for subscribers to topic: ~s (~p subscribers)",
                      [Topic, length(Subscribers)]),
            {cached, Subscribers, UpdatedRegistry};

        {cache_miss, UpdatedRegistry} ->
            %% Cache miss - query DHT asynchronously
            {MsgId, _NewCounter} = next_message_id(MsgIdCounter),
            query_dht_async(Topic, Payload, Qos, MsgId, ConnMgrPid),

            %% Track pending query
            Pending = #{MsgId => {Topic, Payload, Qos, #{}}},
            {query_sent, Pending, MsgId, UpdatedRegistry}
    end.

%% @doc Handle DHT discovery response.
%% Routes messages to discovered subscribers.
%% Returns updated pending queries map.
-spec handle_discovery_response(binary(), list(), pending_queries()) ->
    {ok, pending_queries()} | {not_found, pending_queries()}.
handle_discovery_response(MsgId, _Subscribers, PendingQueries) ->
    QueryInfo = maps:get(MsgId, PendingQueries, undefined),
    do_handle_discovery_response(QueryInfo, MsgId, PendingQueries).

%% @private Query not found (already handled or unknown)
do_handle_discovery_response(undefined, _MsgId, PendingQueries) ->
    {not_found, PendingQueries};
%% @private Query found - remove from pending
do_handle_discovery_response({_Topic, _Payload, _Qos, _Opts}, MsgId, PendingQueries) ->
    {ok, maps:remove(MsgId, PendingQueries)}.

%% @doc Route message to remote subscribers via direct P2P connections (v0.8.0+).
%% Wraps publish in pubsub_route envelope and sends directly to each subscriber.
%% Uses macula_peer_connector for direct QUIC connections to subscriber endpoints.
-spec route_to_subscribers(topic(), payload(), qos(), list(), node_id()) -> ok.
route_to_subscribers(_Topic, _Payload, _Qos, [], _NodeId) ->
    ok;
route_to_subscribers(Topic, Payload, Qos, Subscribers, SourceNodeId) ->
    ?LOG_INFO("[~s] Routing message to ~p remote subscriber(s) for topic: ~s via P2P",
             [SourceNodeId, length(Subscribers), Topic]),

    %% Route to each subscriber via direct P2P connection
    lists:foreach(
        fun(Subscriber) ->
            %% Extract subscriber node_id and endpoint
            NodeId = maps:get(node_id, Subscriber, maps:get(<<"node_id">>, Subscriber, undefined)),
            Endpoint = maps:get(endpoint, Subscriber, maps:get(<<"endpoint">>, Subscriber, undefined)),

            case {NodeId, Endpoint} of
                {undefined, _} ->
                    ?LOG_WARNING("[~s] Subscriber missing node_id, skipping", [SourceNodeId]);
                {_, undefined} ->
                    ?LOG_WARNING("[~s] Subscriber missing endpoint, skipping", [SourceNodeId]);
                {DestNodeId, DestEndpoint} ->
                    %% Build PUBLISH message
                    PublishMsg = #{
                        <<"topic">> => Topic,
                        <<"payload">> => Payload,
                        <<"qos">> => Qos,
                        <<"retain">> => false,
                        <<"message_id">> => crypto:strong_rand_bytes(16)
                    },

                    %% Wrap in pubsub_route envelope (MaxHops = 10)
                    PubSubRouteMsg = macula_pubsub_routing:wrap_publish(
                        SourceNodeId, DestNodeId, PublishMsg, 10
                    ),

                    %% Try direct send first, fall back to NAT-aware routing (v0.12.0+)
                    send_to_subscriber(SourceNodeId, DestNodeId, DestEndpoint, Topic, PubSubRouteMsg)
            end
        end,
        Subscribers
    ),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Store subscription in DHT
store_subscription_in_dht(NodeId, Topic, TopicKey, SubscriberValue) ->
    case whereis(macula_routing_server) of
        undefined ->
            ?LOG_WARNING("[~s] Routing server not running, cannot advertise subscription", [NodeId]);
        RoutingServerPid ->
            handle_store_result(
                macula_routing_server:store(RoutingServerPid, TopicKey, SubscriberValue),
                NodeId, Topic
            )
    end.

%% @private Handle DHT store result
handle_store_result(ok, NodeId, Topic) ->
    ?LOG_DEBUG("[~s] Successfully stored subscription for ~s in DHT", [NodeId, Topic]);
handle_store_result({error, StoreError}, NodeId, Topic) ->
    ?LOG_WARNING("[~s] Failed to store subscription ~s: ~p", [NodeId, Topic, StoreError]).

%% @doc Generate next message ID
-spec next_message_id(non_neg_integer()) -> {binary(), non_neg_integer()}.
next_message_id(Counter) ->
    macula_utils:next_message_id(Counter).

%% @doc Query DHT for subscribers and route message to them (v0.8.0+).
%% Directly queries local routing server and routes message to discovered subscribers.
%% NOTE: Despite the "async" name (kept for compatibility), this now performs
%% synchronous DHT lookup AND routing in one call.
-spec query_dht_async(topic(), payload(), qos(), binary(), connection_manager_pid()) -> ok.
query_dht_async(Topic, Payload, Qos, _MsgId, _ConnMgrPid) ->
    TopicKey = crypto:hash(sha256, Topic),
    ?LOG_DEBUG("Querying DHT for remote subscribers to topic: ~s", [Topic]),
    query_routing_server_for_subscribers(Topic, Payload, Qos, TopicKey),
    ok.

%% @private Query routing server for subscribers
query_routing_server_for_subscribers(Topic, Payload, Qos, TopicKey) ->
    case whereis(macula_routing_server) of
        undefined ->
            ?LOG_WARNING("Routing server not running, cannot query for subscribers to ~s", [Topic]);
        RoutingServerPid ->
            handle_dht_query_result(
                macula_routing_server:find_value(RoutingServerPid, TopicKey, 20),
                Topic, Payload, Qos
            )
    end.

%% @private Handle DHT query result
handle_dht_query_result({ok, Subscribers}, Topic, Payload, Qos) when is_list(Subscribers), length(Subscribers) > 0 ->
    ?LOG_INFO("Found ~p subscriber(s) for topic ~s in DHT, routing message",
              [length(Subscribers), Topic]),
    SourceNodeId = get_local_node_id(),
    route_to_subscribers(Topic, Payload, Qos, Subscribers, SourceNodeId);
handle_dht_query_result({ok, []}, Topic, _Payload, _Qos) ->
    ?LOG_DEBUG("No subscribers found for topic ~s in DHT", [Topic]);
handle_dht_query_result({ok, SingleSub}, Topic, Payload, Qos) when is_map(SingleSub) ->
    ?LOG_INFO("Found 1 subscriber for topic ~s in DHT, routing message", [Topic]),
    SourceNodeId = get_local_node_id(),
    route_to_subscribers(Topic, Payload, Qos, [SingleSub], SourceNodeId);
handle_dht_query_result({error, not_found}, Topic, _Payload, _Qos) ->
    ?LOG_DEBUG("No subscribers found for topic ~s in DHT", [Topic]);
handle_dht_query_result({error, QueryError}, Topic, _Payload, _Qos) ->
    ?LOG_WARNING("Failed to query DHT for topic ~s: ~p", [Topic, QueryError]).

%% @private Get local node ID for routing.
-spec get_local_node_id() -> node_id().
get_local_node_id() ->
    case whereis(macula_gateway) of
        undefined ->
            crypto:strong_rand_bytes(32);
        GatewayPid ->
            get_node_id_from_gateway(gen_server:call(GatewayPid, get_node_id, 1000))
    end.

%% @private Extract node ID from gateway response
get_node_id_from_gateway({ok, NodeId}) -> NodeId;
get_node_id_from_gateway(_) -> crypto:strong_rand_bytes(32).

%% @doc Send message to subscriber with NAT-aware fallback (v0.12.0+).
%% Tries direct connection first, then falls back to NAT-aware routing.
-spec send_to_subscriber(node_id(), node_id(), binary(), topic(), map()) -> ok.
send_to_subscriber(SourceNodeId, DestNodeId, DestEndpoint, Topic, PubSubRouteMsg) ->
    %% Try direct send first (works for public IPs and same-network peers)
    case macula_peer_connector:send_message(DestEndpoint, pubsub_route, PubSubRouteMsg) of
        ok ->
            ?LOG_DEBUG("[~s] Sent pubsub_route directly to subscriber ~s at ~s for topic ~s",
                      [SourceNodeId, binary:encode_hex(DestNodeId), DestEndpoint, Topic]);
        {error, Reason} ->
            ?LOG_DEBUG("[~s] Direct send to ~s failed (~p), trying NAT-aware routing",
                      [SourceNodeId, DestEndpoint, Reason]),
            %% Fall back to NAT-aware routing (hole punch, relay)
            send_to_subscriber_nat_aware(SourceNodeId, DestNodeId, DestEndpoint, Topic, PubSubRouteMsg)
    end.

%% @private Use NAT-aware routing to reach subscriber behind NAT.
-spec send_to_subscriber_nat_aware(node_id(), node_id(), binary(), topic(), map()) -> ok.
send_to_subscriber_nat_aware(SourceNodeId, DestNodeId, DestEndpoint, Topic, PubSubRouteMsg) ->
    %% Use NAT-aware connector with endpoint hint
    Opts = #{endpoint => DestEndpoint},
    case macula_peer_connector:send_message_nat_aware(SourceNodeId, DestNodeId, pubsub_route, PubSubRouteMsg, Opts) of
        ok ->
            ?LOG_INFO("[~s] NAT-aware send to subscriber ~s succeeded for topic ~s",
                     [SourceNodeId, binary:encode_hex(DestNodeId), Topic]);
        {error, Reason} ->
            ?LOG_ERROR("[~s] NAT-aware send to ~s failed: ~p (topic: ~s)",
                      [SourceNodeId, binary:encode_hex(DestNodeId), Reason, Topic])
    end.
