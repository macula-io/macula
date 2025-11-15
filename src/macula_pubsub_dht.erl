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
advertise_subscription(Topic, SubRef, NodeId, Url, ConnMgrPid) ->
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

    %% Send STORE message to DHT via connection manager
    ?LOG_INFO("[~s] Advertising subscription to topic ~s in DHT", [NodeId, Topic]),
    try
        StoreMsg = macula_routing_protocol:encode_store(TopicKey, SubscriberValue),
        case macula_connection:send_message(ConnMgrPid, store, StoreMsg) of
            ok ->
                ?LOG_DEBUG("[~s] Successfully stored subscription for ~s in DHT", [NodeId, Topic]);
            {error, SendError} ->
                ?LOG_WARNING("[~s] Failed to send STORE for subscription ~s: ~p",
                            [NodeId, Topic, SendError])
        end
    catch
        _:DhtError ->
            ?LOG_WARNING("[~s] Failed to advertise subscription ~s in DHT: ~p (continuing)",
                        [NodeId, Topic, DhtError])
    end,

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
    case maps:get(Topic, AdvertisedSubscriptions, undefined) of
        undefined ->
            %% Not advertised, return unchanged
            AdvertisedSubscriptions;
        SubInfo ->
            %% Cancel timer and remove from map
            TimerRef = maps:get(timer_ref, SubInfo),
            erlang:cancel_timer(TimerRef),
            maps:remove(Topic, AdvertisedSubscriptions)
    end.

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
    case maps:get(MsgId, PendingQueries, undefined) of
        undefined ->
            %% Query not found (already handled or unknown)
            {not_found, PendingQueries};
        {_Topic, _Payload, _Qos, _Opts} ->
            %% Query found - remove from pending
            %% Note: Actual routing happens in the caller
            {ok, maps:remove(MsgId, PendingQueries)}
    end.

%% @doc Route message to remote subscribers.
%% Sends publish message to each subscriber endpoint.
-spec route_to_subscribers(topic(), payload(), qos(), list(), node_id()) -> ok.
route_to_subscribers(_Topic, _Payload, _Qos, [], _NodeId) ->
    ok;
route_to_subscribers(Topic, _Payload, _Qos, Subscribers, NodeId) ->
    ?LOG_INFO("[~s] Routing message to ~p remote subscriber(s) for topic: ~s",
             [NodeId, length(Subscribers), Topic]),

    %% Send publish message to each subscriber endpoint
    lists:foreach(
        fun(Subscriber) ->
            %% Extract subscriber endpoint
            Endpoint = maps:get(endpoint, Subscriber, undefined),
            case Endpoint of
                undefined ->
                    ?LOG_WARNING("[~s] Subscriber missing endpoint, skipping", [NodeId]);
                _ ->
                    %% TODO: Route message to remote endpoint
                    %% This will be implemented when multi-endpoint connections are added
                    ?LOG_DEBUG("[~s] Would route to endpoint: ~s", [NodeId, Endpoint])
            end
        end,
        Subscribers
    ),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Generate next message ID
-spec next_message_id(non_neg_integer()) -> {binary(), non_neg_integer()}.
next_message_id(Counter) ->
    macula_utils:next_message_id(Counter).

%% @doc Query DHT for subscribers asynchronously
-spec query_dht_async(topic(), payload(), qos(), binary(), connection_manager_pid()) -> ok.
query_dht_async(Topic, _Payload, _Qos, MsgId, ConnMgrPid) ->
    %% Create DHT key from topic
    TopicKey = crypto:hash(sha256, Topic),

    ?LOG_DEBUG("Querying DHT for remote subscribers to topic: ~s (MsgId: ~s)", [Topic, MsgId]),

    %% Send FIND_VALUE query to DHT asynchronously (spawn to avoid blocking)
    spawn(fun() ->
        try
            FindValueMsg = macula_routing_protocol:encode_find_value(TopicKey),
            case macula_connection:send_message(ConnMgrPid, find_value, FindValueMsg) of
                ok ->
                    ?LOG_DEBUG("Sent FIND_VALUE for topic ~s", [Topic]);
                {error, SendError} ->
                    ?LOG_WARNING("Failed to send FIND_VALUE for topic ~s: ~p",
                                [Topic, SendError])
            end
        catch
            _:QueryError:Stack ->
                ?LOG_WARNING("Failed to query DHT for topic ~s: ~p~nStack: ~p",
                            [Topic, QueryError, Stack])
        end
    end),
    ok.
