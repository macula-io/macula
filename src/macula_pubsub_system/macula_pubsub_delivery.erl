%%%-------------------------------------------------------------------
%%% @doc
%%% Message routing and delivery to local and remote subscribers.
%%% Combines local registry and remote discovery for full fan-out.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_delivery).

%% API
-export([
    deliver_local/2,
    deliver_remote/3,
    publish/4,
    get_matching_patterns/2
]).

%% Types
-type message() :: #{
    topic := binary(),
    payload := term(),
    timestamp := integer()
}.

-type delivery_result() :: ok | {ok, term()} | {error, term()}.

-type discovery_fun() :: fun((binary()) -> {ok, [map()]} | {error, term()}).
-type send_fun() :: fun((message(), macula_pubsub_discovery:address()) -> ok | {error, term()}).

-export_type([message/0, delivery_result/0, discovery_fun/0, send_fun/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Deliver message to all matching local subscribers.
%% Crashes if subscriber callback fails - indicates dead subscriber process.
-spec deliver_local(message(), macula_pubsub_registry:registry()) -> [delivery_result()].
deliver_local(Message, Registry) ->
    Topic = maps:get(topic, Message),

    %% Find matching subscriptions
    Subscriptions = macula_pubsub_registry:match(Registry, Topic),

    %% Deliver to each callback (let it crash on dead subscribers)
    lists:map(
        fun(Sub) ->
            Callback = maps:get(callback, Sub),
            Callback ! Message,
            {ok, maps:get(subscriber_id, Sub)}
        end,
        Subscriptions
    ).

%% @doc Deliver message to remote subscribers via QUIC.
-spec deliver_remote(message(), [macula_pubsub_discovery:subscriber()], send_fun()) ->
    [delivery_result()].
deliver_remote(Message, RemoteSubscribers, SendFun) ->
    lists:map(
        fun(RemoteSub) ->
            Address = maps:get(address, RemoteSub),
            SendFun(Message, Address)
        end,
        RemoteSubscribers
    ).

%% @doc Publish message to both local and remote subscribers.
%% Returns {LocalResults, RemoteResults}.
-spec publish(message(), macula_pubsub_registry:registry(), discovery_fun(), send_fun()) ->
    {[delivery_result()], [delivery_result()]}.
publish(Message, Registry, DiscoveryFun, SendFun) ->
    Topic = maps:get(topic, Message),

    logger:info("[PubSub Delivery] Publishing to topic: ~p", [Topic]),

    %% Deliver to local subscribers
    LocalResults = deliver_local(Message, Registry),
    logger:info("[PubSub Delivery] Local delivery results: ~p subscriber(s)", [length(LocalResults)]),

    %% Find matching patterns for remote discovery
    MatchingPatterns = get_matching_patterns(Topic, Registry),
    logger:info("[PubSub Delivery] Matching patterns for remote discovery: ~p", [MatchingPatterns]),

    %% Discover remote subscribers for each matching pattern
    RemoteSubscribers = discover_remote_subscribers(MatchingPatterns, DiscoveryFun),
    logger:info("[PubSub Delivery] Found ~p remote subscriber(s)", [length(RemoteSubscribers)]),

    %% Deliver to remote subscribers
    RemoteResults = deliver_remote(Message, RemoteSubscribers, SendFun),
    logger:info("[PubSub Delivery] Remote delivery results: ~p", [length(RemoteResults)]),

    {LocalResults, RemoteResults}.

%% @doc Get all unique patterns that match the topic.
%% Used for remote subscriber discovery.
-spec get_matching_patterns(binary(), macula_pubsub_registry:registry()) -> [binary()].
get_matching_patterns(Topic, Registry) ->
    %% Get all patterns from registry
    AllPatterns = macula_pubsub_registry:list_patterns(Registry),

    %% Filter to matching patterns
    MatchingPatterns = lists:filter(
        fun(Pattern) ->
            macula_pubsub_topic:matches(Topic, Pattern)
        end,
        AllPatterns
    ),

    %% Return unique patterns
    lists:usort(MatchingPatterns).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Discover remote subscribers for multiple patterns.
-spec discover_remote_subscribers([binary()], discovery_fun()) ->
    [macula_pubsub_discovery:subscriber()].
discover_remote_subscribers(Patterns, DiscoveryFun) ->
    %% Query discovery for each pattern
    AllSubscribers = lists:flatmap(
        fun(Pattern) ->
            case DiscoveryFun(Pattern) of
                {ok, Subscribers} -> Subscribers;
                {error, _Reason} -> []  % Ignore discovery errors
            end
        end,
        Patterns
    ),

    %% Deduplicate by node_id
    deduplicate_by_node_id(AllSubscribers).

%% @doc Remove duplicate subscribers by node_id.
-spec deduplicate_by_node_id([macula_pubsub_discovery:subscriber()]) ->
    [macula_pubsub_discovery:subscriber()].
deduplicate_by_node_id(Subscribers) ->
    %% Build map: node_id -> subscriber
    SubscriberMap = lists:foldl(
        fun(Sub, Acc) ->
            NodeId = maps:get(node_id, Sub),
            Acc#{NodeId => Sub}
        end,
        #{},
        Subscribers
    ),

    %% Return unique subscribers
    maps:values(SubscriberMap).
