%%%-------------------------------------------------------------------
%%% @doc
%%% Pub/Sub Handler GenServer - manages topic subscriptions and message routing.
%%%
%%% Responsibilities:
%%% - Subscribe/unsubscribe streams to topics
%%% - Route published messages to matching subscribers
%%% - Support wildcard topics (* single-level, ** multi-level)
%%% - Track bidirectional mapping (topic ↔ stream)
%%% - Monitor stream processes for automatic cleanup
%%%
%%% Extracted from macula_gateway.erl (Phase 3)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_pubsub).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/1,
    stop/1,
    subscribe/3,
    unsubscribe/3,
    publish/3,
    deliver_local/3,
    get_subscribers/2,
    get_stream_topics/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Stream handle can be either a pid (process) or reference (QUIC stream)
-type stream_handle() :: pid() | reference().

-record(state, {
    opts :: map(),
    subscriptions :: #{binary() => [stream_handle()]},        % topic => [stream_handles]
    stream_subscriptions :: #{stream_handle() => [binary()]}, % stream_handle => [topics]
    monitors :: #{reference() => {stream_handle(), binary()}} % monitor_ref => {stream_handle, topic}
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the pub/sub handler with options.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Stop the pub/sub handler.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Subscribe a stream to a topic (supports wildcards).
%% Async (cast) to prevent blocking callers when PubSub is busy.
-spec subscribe(pid(), pid() | reference(), binary()) -> ok.
subscribe(Pid, Stream, Topic) ->
    gen_server:cast(Pid, {subscribe, Stream, Topic}).

%% @doc Unsubscribe a stream from a topic.
%% Async (cast) to prevent blocking callers when PubSub is busy.
-spec unsubscribe(pid(), pid() | reference(), binary()) -> ok.
unsubscribe(Pid, Stream, Topic) ->
    gen_server:cast(Pid, {unsubscribe, Stream, Topic}).

%% @doc Publish a message to a topic (routes to matching subscribers).
-spec publish(pid(), binary(), map()) -> ok.
publish(Pid, Topic, Payload) ->
    gen_server:call(Pid, {publish, Topic, Payload}).

%% @doc Deliver a message to LOCAL subscribers only (no remote routing).
%% Used by pubsub_route delivery to prevent message amplification.
%% When a message arrives from another node via pubsub_route, it should
%% only be delivered to local subscribers, NOT re-routed to remote subscribers.
-spec deliver_local(pid(), binary(), map()) -> ok.
deliver_local(Pid, Topic, Payload) ->
    gen_server:call(Pid, {deliver_local, Topic, Payload}).

%% @doc Get all subscribers for a topic (exact and wildcard matches).
-spec get_subscribers(pid(), binary()) -> {ok, [pid()]}.
get_subscribers(Pid, Topic) ->
    gen_server:call(Pid, {get_subscribers, Topic}).

%% @doc Get all topics a stream is subscribed to.
-spec get_stream_topics(pid(), pid()) -> {ok, [binary()]}.
get_stream_topics(Pid, Stream) ->
    gen_server:call(Pid, {get_stream_topics, Stream}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    ?LOG_INFO("Initializing pub/sub handler"),

    %% DEBUG: Log what's in Opts to verify node_id/url are present
    ?LOG_DEBUG("Opts map keys: ~p", [maps:keys(Opts)]),
    ?LOG_DEBUG("node_id in opts: ~p", [maps:get(node_id, Opts, not_found)]),
    ?LOG_DEBUG("url in opts: ~p", [maps:get(url, Opts, not_found)]),

    State = #state{
        opts = Opts,
        subscriptions = #{},
        stream_subscriptions = #{},
        monitors = #{}
    },

    %% Schedule re-advertisement of all subscriptions after DHT routing table is populated
    %% Initial subscriptions happen before bootstrap connects, so this ensures they propagate
    erlang:send_after(5000, self(), readvertise_all_subscriptions),

    ?LOG_INFO("Pub/sub handler initialized"),
    {ok, State}.

%% Subscribe is async (cast) to prevent blocking callers when PubSub is busy
handle_cast({subscribe, Stream, Topic}, State) when (is_pid(Stream) orelse is_reference(Stream)), is_binary(Topic) ->
    ?LOG_DEBUG("[PubSub] SUBSCRIBE cast received: Stream=~p, Topic=~s", [Stream, Topic]),
    CurrentTopics = maps:get(Stream, State#state.stream_subscriptions, []),
    NewState = do_subscribe(lists:member(Topic, CurrentTopics), Stream, Topic, CurrentTopics, State),
    {noreply, NewState};

%% Unsubscribe is async (cast) to prevent blocking callers when PubSub is busy
handle_cast({unsubscribe, Stream, Topic}, State) when (is_pid(Stream) orelse is_reference(Stream)), is_binary(Topic) ->
    %% Remove from stream → topics mapping
    CurrentTopics = maps:get(Stream, State#state.stream_subscriptions, []),
    NewTopics = lists:delete(Topic, CurrentTopics),

    NewStreamSubs = case NewTopics of
        [] -> maps:remove(Stream, State#state.stream_subscriptions);
        _ -> maps:put(Stream, NewTopics, State#state.stream_subscriptions)
    end,

    %% Remove from topic → streams mapping
    Subscribers = maps:get(Topic, State#state.subscriptions, []),
    NewSubscribers = lists:delete(Stream, Subscribers),

    NewSubscriptions = case NewSubscribers of
        [] -> maps:remove(Topic, State#state.subscriptions);
        _ -> maps:put(Topic, NewSubscribers, State#state.subscriptions)
    end,

    NewState = State#state{
        subscriptions = NewSubscriptions,
        stream_subscriptions = NewStreamSubs
    },
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({publish, Topic, Payload}, _From, State) when is_binary(Topic) ->
    LocalStreams = find_matching_subscribers(Topic, State),
    ?LOG_DEBUG("Publishing to topic ~s: found ~p local subscribers",
             [Topic, length(LocalStreams)]),
    LocalNodeId = maps:get(node_id, State#state.opts, <<"unknown">>),
    %% Full publish message with all required fields for protocol encoding
    PubMsg = #{
        <<"topic">> => Topic,
        <<"payload">> => Payload,
        <<"qos">> => 0,
        <<"retain">> => false,
        <<"message_id">> => crypto:strong_rand_bytes(16)
    },
    do_publish(whereis(macula_gateway_mesh), whereis(macula_gateway_clients),
               LocalStreams, PubMsg, LocalNodeId, Topic, Payload),
    {reply, ok, State};

%% @doc Deliver to LOCAL subscribers only - no remote routing.
%% Used for pubsub_route messages to prevent amplification loops.
handle_call({deliver_local, Topic, Payload}, _From, State) when is_binary(Topic) ->
    LocalStreams = find_matching_subscribers(Topic, State),
    ?LOG_DEBUG("Delivering locally to topic ~s: found ~p local subscribers",
             [Topic, length(LocalStreams)]),
    deliver_to_local_streams(LocalStreams, Topic, Payload),
    {reply, ok, State};

handle_call({get_subscribers, Topic}, _From, State) when is_binary(Topic) ->
    Subscribers = find_matching_subscribers(Topic, State),
    {reply, {ok, Subscribers}, State};

handle_call({get_stream_topics, Stream}, _From, State) when is_pid(Stream) orelse is_reference(Stream) ->
    Topics = maps:get(Stream, State#state.stream_subscriptions, []),
    {reply, {ok, Topics}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle stream process death - automatic cleanup.
handle_info({'DOWN', MonitorRef, process, StreamPid, _Reason}, State) ->
    Monitors = maps:remove(MonitorRef, State#state.monitors),
    Topics = maps:get(StreamPid, State#state.stream_subscriptions, []),
    NewSubscriptions = remove_stream_from_topics(StreamPid, Topics, State#state.subscriptions),
    NewStreamSubs = maps:remove(StreamPid, State#state.stream_subscriptions),
    NewState = State#state{
        subscriptions = NewSubscriptions,
        stream_subscriptions = NewStreamSubs,
        monitors = Monitors
    },
    {noreply, NewState};

%% @doc Re-advertise all existing subscriptions in the DHT.
%% Called after bootstrap connection to ensure subscriptions propagate.
handle_info(readvertise_all_subscriptions, State) ->
    Topics = maps:keys(State#state.subscriptions),
    ?LOG_INFO("Re-advertising ~p subscription(s) after DHT routing table populated",
             [length(Topics)]),
    lists:foreach(fun(Topic) ->
        advertise_subscription_in_dht(Topic, State)
    end, Topics),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Find all streams subscribed to topics matching the given topic.
%% Supports exact matches and wildcard patterns (* and **).
-spec find_matching_subscribers(binary(), #state{}) -> [pid()].
find_matching_subscribers(Topic, State) ->
    %% Get all subscription patterns
    AllPatterns = maps:keys(State#state.subscriptions),
    ?LOG_DEBUG("Finding subscribers for topic: ~s", [Topic]),
    ?LOG_DEBUG("All subscription patterns: ~p", [AllPatterns]),
    ?LOG_DEBUG("Subscriptions map: ~p", [State#state.subscriptions]),

    %% Find patterns that match the topic
    MatchingPatterns = lists:filter(fun(Pattern) ->
        Matches = topic_matches(Pattern, Topic),
        ?LOG_DEBUG("Pattern ~s matches topic ~s: ~p", [Pattern, Topic, Matches]),
        Matches
    end, AllPatterns),

    ?LOG_DEBUG("Matching patterns: ~p", [MatchingPatterns]),

    %% Collect all unique streams from matching patterns
    AllStreams = lists:flatmap(fun(Pattern) ->
        Streams = maps:get(Pattern, State#state.subscriptions, []),
        ?LOG_DEBUG("Pattern ~s has streams: ~p", [Pattern, Streams]),
        Streams
    end, MatchingPatterns),

    ?LOG_DEBUG("All streams before dedup: ~p", [AllStreams]),

    %% Remove duplicates
    Result = lists:usort(AllStreams),
    ?LOG_DEBUG("Final subscribers: ~p", [Result]),
    Result.

%% @doc Check if a topic pattern matches a concrete topic.
%% Supports:
%%   - Exact match: "foo.bar" matches "foo.bar"
%%   - Single-level wildcard: "foo.*.bar" matches "foo.xyz.bar"
%%   - Multi-level wildcard: "foo.**.bar" matches "foo.x.y.z.bar"
-spec topic_matches(binary(), binary()) -> boolean().
topic_matches(Pattern, Topic) ->
    %% Split into segments
    PatternParts = binary:split(Pattern, <<".">>, [global]),
    TopicParts = binary:split(Topic, <<".">>, [global]),

    parts_match(PatternParts, TopicParts).

%% @doc Match pattern parts against topic parts.
-spec parts_match([binary()], [binary()]) -> boolean().
parts_match([], []) ->
    true;
parts_match([<<"**">>], _) ->
    %% ** matches any remaining segments
    true;
parts_match([<<"**">> | PatternRest], TopicParts) ->
    %% ** can match 0 or more segments
    %% Try matching rest at every position
    try_multi_wildcard(PatternRest, TopicParts);
parts_match([<<"*">> | PatternRest], [_TopicPart | TopicRest]) ->
    %% * matches exactly one segment
    parts_match(PatternRest, TopicRest);
parts_match([PatternPart | PatternRest], [TopicPart | TopicRest]) when PatternPart =:= TopicPart ->
    %% Exact match
    parts_match(PatternRest, TopicRest);
parts_match(_, _) ->
    false.

%% @doc Try to match pattern after ** wildcard.
-spec try_multi_wildcard([binary()], [binary()]) -> boolean().
try_multi_wildcard(Pattern, Topic) ->
    try_multi_wildcard(Pattern, Topic, 0).

-spec try_multi_wildcard([binary()], [binary()], non_neg_integer()) -> boolean().
try_multi_wildcard(Pattern, Topic, Skip) ->
    case length(Topic) >= Skip of
        true ->
            TopicRest = lists:nthtail(Skip, Topic),
            case parts_match(Pattern, TopicRest) of
                true -> true;
                false -> try_multi_wildcard(Pattern, Topic, Skip + 1)
            end;
        false ->
            false
    end.

%% @private
%% @doc Advertise a subscription in the DHT so other gateways can discover it.
%% This enables cross-gateway pub/sub routing.
%% Also invalidates subscriber cache to ensure fresh lookups.
-spec advertise_subscription_in_dht(binary(), #state{}) -> ok.
advertise_subscription_in_dht(Topic, State) ->
    invalidate_subscriber_cache(Topic),
    NodeId = maps:get(node_id, State#state.opts, <<"unknown">>),
    Url = maps:get(url, State#state.opts, <<"unknown">>),
    ?LOG_DEBUG("Advertising subscription for topic ~s", [Topic]),
    ?LOG_DEBUG("NodeId from opts: ~p", [NodeId]),
    ?LOG_DEBUG("Url from opts: ~p", [Url]),
    TopicKey = crypto:hash(sha256, Topic),
    SubscriberValue = #{node_id => NodeId, endpoint => Url, ttl => 300},
    ?LOG_INFO("Advertising subscription to topic ~s in DHT", [Topic]),
    do_advertise_subscription(whereis(macula_routing_server), Topic, TopicKey, SubscriberValue),
    ok.

%% @private
%% @doc Invalidate subscriber cache for a topic.
%% Called when subscription changes to ensure fresh DHT lookups.
-spec invalidate_subscriber_cache(binary()) -> ok.
invalidate_subscriber_cache(Topic) ->
    do_invalidate_subscriber_cache(whereis(macula_subscriber_cache), Topic).

do_invalidate_subscriber_cache(undefined, _Topic) ->
    ok;
do_invalidate_subscriber_cache(_Pid, Topic) ->
    macula_subscriber_cache:invalidate(Topic).

%%%===================================================================
%%% Subscribe helpers
%%%===================================================================

%% @private Already subscribed - idempotent
do_subscribe(true, Stream, Topic, _CurrentTopics, State) ->
    ?LOG_DEBUG("Stream ~p already subscribed to ~s", [Stream, Topic]),
    State;
%% @private New subscription
do_subscribe(false, Stream, Topic, CurrentTopics, State) ->
    Subscribers = maps:get(Topic, State#state.subscriptions, []),
    NewSubscriptions = maps:put(Topic, [Stream | Subscribers], State#state.subscriptions),
    ?LOG_INFO("Added stream ~p to topic ~s (total subscribers: ~p)",
             [Stream, Topic, length([Stream | Subscribers])]),
    advertise_subscription_in_dht(Topic, State),
    NewTopics = [Topic | CurrentTopics],
    NewStreamSubs = maps:put(Stream, NewTopics, State#state.stream_subscriptions),
    NewMonitors = maybe_monitor_stream(Stream, CurrentTopics, State#state.monitors, Topic),
    ?LOG_DEBUG("Current subscriptions map: ~p", [NewSubscriptions]),
    State#state{
        subscriptions = NewSubscriptions,
        stream_subscriptions = NewStreamSubs,
        monitors = NewMonitors
    }.

%% @private Monitor stream on first subscription (pids only)
maybe_monitor_stream(Stream, [], Monitors, Topic) when is_pid(Stream) ->
    MonitorRef = erlang:monitor(process, Stream),
    ?LOG_DEBUG("Monitoring stream ~p (first subscription)", [Stream]),
    maps:put(MonitorRef, {Stream, Topic}, Monitors);
maybe_monitor_stream(_Stream, _CurrentTopics, Monitors, _Topic) ->
    Monitors.

%%%===================================================================
%%% Publish helpers
%%%===================================================================

%% @private Mesh not running - deliver locally only
do_publish(undefined, _, LocalStreams, _PubMsg, _LocalNodeId, Topic, Payload) ->
    ?LOG_WARNING("macula_gateway_mesh not running, cannot route remotely"),
    deliver_to_local_streams(LocalStreams, Topic, Payload);
%% @private Clients not running - deliver locally only
do_publish(_, undefined, LocalStreams, _PubMsg, _LocalNodeId, Topic, Payload) ->
    ?LOG_WARNING("macula_gateway_clients not running, cannot route remotely"),
    deliver_to_local_streams(LocalStreams, Topic, Payload);
%% @private Full routing via pubsub_router
do_publish(MeshPid, ClientsPid, LocalStreams, PubMsg, LocalNodeId, _Topic, _Payload) ->
    ?LOG_DEBUG("Using pubsub_router to distribute (mesh=~p, clients=~p)",
             [MeshPid, ClientsPid]),
    macula_gateway_pubsub_router:distribute(LocalStreams, PubMsg, LocalNodeId, MeshPid, ClientsPid).

%% @private Deliver to local streams
%% Handles both PID handlers and QUIC stream references
deliver_to_local_streams([], _Topic, _Payload) ->
    ok;
deliver_to_local_streams([Stream | Rest], Topic, Payload) ->
    deliver_to_stream(Stream, Topic, Payload),
    deliver_to_local_streams(Rest, Topic, Payload).

%% @private Deliver to PID handler (send Erlang message)
deliver_to_stream(Stream, Topic, Payload) when is_pid(Stream) ->
    case erlang:is_process_alive(Stream) of
        true ->
            ?LOG_DEBUG("Sending to local handler ~p", [Stream]),
            Stream ! {publish, Topic, Payload};
        false ->
            ok
    end;
%% @private Deliver to QUIC stream (send encoded message)
deliver_to_stream(Stream, Topic, Payload) when is_reference(Stream) ->
    ?LOG_DEBUG("Sending to QUIC stream ~p", [Stream]),
    %% Build complete PUBLISH message with all required protocol fields
    PubMsg = #{
        <<"topic">> => Topic,
        <<"payload">> => Payload,
        <<"qos">> => 0,
        <<"retain">> => false,
        <<"message_id">> => crypto:strong_rand_bytes(16)
    },
    Binary = macula_protocol_encoder:encode(publish, PubMsg),
    _ = macula_quic:send(Stream, Binary),
    ok;
deliver_to_stream(_Stream, _Topic, _Payload) ->
    ok.

%%%===================================================================
%%% Stream cleanup helpers
%%%===================================================================

%% @private Remove stream from all topic subscriptions
remove_stream_from_topics(StreamPid, Topics, Subscriptions) ->
    lists:foldl(fun(Topic, Acc) ->
        remove_stream_from_topic(StreamPid, Topic, Acc)
    end, Subscriptions, Topics).

remove_stream_from_topic(StreamPid, Topic, Subscriptions) ->
    Subscribers = maps:get(Topic, Subscriptions, []),
    NewSubscribers = lists:delete(StreamPid, Subscribers),
    update_or_remove_topic(NewSubscribers, Topic, Subscriptions).

update_or_remove_topic([], Topic, Subscriptions) ->
    maps:remove(Topic, Subscriptions);
update_or_remove_topic(Subscribers, Topic, Subscriptions) ->
    maps:put(Topic, Subscribers, Subscriptions).

%%%===================================================================
%%% DHT advertisement helpers
%%%===================================================================

%% @private Routing server not available
do_advertise_subscription(undefined, _Topic, _TopicKey, _SubscriberValue) ->
    ?LOG_WARNING("Routing server not running, cannot advertise subscription");
%% @private Store subscription in DHT
do_advertise_subscription(RoutingServerPid, Topic, TopicKey, SubscriberValue) ->
    Result = (catch macula_routing_server:store(RoutingServerPid, TopicKey, SubscriberValue)),
    log_store_result(Result, Topic).

log_store_result(ok, Topic) ->
    ?LOG_INFO("Successfully stored subscription for ~s in DHT", [Topic]);
log_store_result({error, StoreError}, Topic) ->
    ?LOG_ERROR("Failed to store subscription ~s: ~p", [Topic, StoreError]);
log_store_result({'EXIT', Reason}, Topic) ->
    ?LOG_ERROR("Failed to advertise subscription ~s in DHT: ~p (continuing)", [Topic, Reason]);
log_store_result(_Other, Topic) ->
    ?LOG_WARNING("Unexpected result storing subscription ~s", [Topic]).
