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

%% API
-export([
    start_link/1,
    stop/1,
    subscribe/3,
    unsubscribe/3,
    publish/3,
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
    io:format("[PubSub] Initializing pub/sub handler~n"),

    %% DEBUG: Log what's in Opts to verify node_id/url are present
    io:format("[PubSub DEBUG] Opts map keys: ~p~n", [maps:keys(Opts)]),
    io:format("[PubSub DEBUG] node_id in opts: ~p~n", [maps:get(node_id, Opts, not_found)]),
    io:format("[PubSub DEBUG] url in opts: ~p~n", [maps:get(url, Opts, not_found)]),

    State = #state{
        opts = Opts,
        subscriptions = #{},
        stream_subscriptions = #{},
        monitors = #{}
    },

    %% Schedule re-advertisement of all subscriptions after DHT routing table is populated
    %% Initial subscriptions happen before bootstrap connects, so this ensures they propagate
    erlang:send_after(5000, self(), readvertise_all_subscriptions),

    io:format("[PubSub] Pub/sub handler initialized~n"),
    {ok, State}.

%% Subscribe is async (cast) to prevent blocking callers when PubSub is busy
handle_cast({subscribe, Stream, Topic}, State) when (is_pid(Stream) orelse is_reference(Stream)), is_binary(Topic) ->
    io:format("[PubSub ~p] SUBSCRIBE called: Stream=~p, Topic=~s~n", [self(), Stream, Topic]),
    %% Check if already subscribed
    CurrentTopics = maps:get(Stream, State#state.stream_subscriptions, []),

    NewState = case lists:member(Topic, CurrentTopics) of
        true ->
            io:format("[PubSub ~p] Stream ~p already subscribed to ~s~n", [self(), Stream, Topic]),
            %% Already subscribed - idempotent
            State;
        false ->
            %% Add to topic → streams mapping
            Subscribers = maps:get(Topic, State#state.subscriptions, []),
            NewSubscriptions = maps:put(Topic, [Stream | Subscribers], State#state.subscriptions),
            io:format("[PubSub ~p] Added stream ~p to topic ~s (total subscribers: ~p)~n",
                     [self(), Stream, Topic, length([Stream | Subscribers])]),

            %% Advertise subscription in DHT for cross-gateway discovery
            advertise_subscription_in_dht(Topic, State),

            %% Add to stream → topics mapping
            NewTopics = [Topic | CurrentTopics],
            NewStreamSubs = maps:put(Stream, NewTopics, State#state.stream_subscriptions),

            %% Monitor stream if first subscription (only for pids - can't monitor references)
            NewMonitors = case {length(CurrentTopics), is_pid(Stream)} of
                {0, true} ->
                    MonitorRef = erlang:monitor(process, Stream),
                    io:format("[PubSub ~p] Monitoring stream ~p (first subscription)~n", [self(), Stream]),
                    maps:put(MonitorRef, {Stream, Topic}, State#state.monitors);
                _ ->
                    State#state.monitors
            end,

            io:format("[PubSub ~p] Current subscriptions map: ~p~n", [self(), NewSubscriptions]),

            State#state{
                subscriptions = NewSubscriptions,
                stream_subscriptions = NewStreamSubs,
                monitors = NewMonitors
            }
    end,
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
    %% Find all LOCAL matching subscribers (exact + wildcard patterns)
    LocalStreams = find_matching_subscribers(Topic, State),

    io:format("[PubSub ~p] Publishing to topic ~s: found ~p local subscribers~n",
             [self(), Topic, length(LocalStreams)]),

    %% Get node_id for distribute call
    LocalNodeId = maps:get(node_id, State#state.opts, <<"unknown">>),

    %% Create PUBLISH message map as expected by pubsub_router
    PubMsg = #{
        <<"topic">> => Topic,
        <<"payload">> => Payload
    },

    %% Use the pubsub_router to distribute to both local and remote subscribers
    %% This properly uses DHT routing + mesh connections + pubsub_route envelopes
    case {whereis(macula_gateway_mesh), whereis(macula_gateway_client_manager)} of
        {undefined, _} ->
            io:format("[PubSub ~p] WARNING: macula_gateway_mesh not running, cannot route remotely~n", [self()]),
            %% Fallback: at least deliver locally
            lists:foreach(fun(Stream) ->
                case erlang:is_process_alive(Stream) of
                    true ->
                        io:format("[PubSub ~p] Sending to local stream ~p~n", [self(), Stream]),
                        Stream ! {publish, Topic, Payload};
                    false ->
                        ok
                end
            end, LocalStreams);
        {_, undefined} ->
            io:format("[PubSub ~p] WARNING: macula_gateway_client_manager not running, cannot route remotely~n", [self()]),
            %% Fallback: at least deliver locally
            lists:foreach(fun(Stream) ->
                case erlang:is_process_alive(Stream) of
                    true ->
                        io:format("[PubSub ~p] Sending to local stream ~p~n", [self(), Stream]),
                        Stream ! {publish, Topic, Payload};
                    false ->
                        ok
                end
            end, LocalStreams);
        {MeshPid, ClientsPid} ->
            %% Use pubsub_router for proper DHT-based routing
            io:format("[PubSub ~p] Using pubsub_router to distribute (mesh=~p, clients=~p)~n",
                     [self(), MeshPid, ClientsPid]),
            macula_gateway_pubsub_router:distribute(
                LocalStreams,
                PubMsg,
                LocalNodeId,
                MeshPid,
                ClientsPid
            )
    end,

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
    %% Remove monitor reference
    Monitors = maps:remove(MonitorRef, State#state.monitors),

    %% Get all topics this stream was subscribed to
    Topics = maps:get(StreamPid, State#state.stream_subscriptions, []),

    %% Remove stream from all topic subscriptions
    NewSubscriptions = lists:foldl(fun(Topic, Acc) ->
        Subscribers = maps:get(Topic, Acc, []),
        NewSubscribers = lists:delete(StreamPid, Subscribers),
        case NewSubscribers of
            [] -> maps:remove(Topic, Acc);
            _ -> maps:put(Topic, NewSubscribers, Acc)
        end
    end, State#state.subscriptions, Topics),

    %% Remove stream from stream_subscriptions
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
    io:format("[PubSub] Re-advertising ~p subscription(s) after DHT routing table populated~n",
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
    io:format("[PubSub ~p] Finding subscribers for topic: ~s~n", [self(), Topic]),
    io:format("[PubSub ~p] All subscription patterns: ~p~n", [self(), AllPatterns]),
    io:format("[PubSub ~p] Subscriptions map: ~p~n", [self(), State#state.subscriptions]),

    %% Find patterns that match the topic
    MatchingPatterns = lists:filter(fun(Pattern) ->
        Matches = topic_matches(Pattern, Topic),
        io:format("[PubSub ~p] Pattern ~s matches topic ~s: ~p~n", [self(), Pattern, Topic, Matches]),
        Matches
    end, AllPatterns),

    io:format("[PubSub ~p] Matching patterns: ~p~n", [self(), MatchingPatterns]),

    %% Collect all unique streams from matching patterns
    AllStreams = lists:flatmap(fun(Pattern) ->
        Streams = maps:get(Pattern, State#state.subscriptions, []),
        io:format("[PubSub ~p] Pattern ~s has streams: ~p~n", [self(), Pattern, Streams]),
        Streams
    end, MatchingPatterns),

    io:format("[PubSub ~p] All streams before dedup: ~p~n", [self(), AllStreams]),

    %% Remove duplicates
    Result = lists:usort(AllStreams),
    io:format("[PubSub ~p] Final subscribers: ~p~n", [self(), Result]),
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
    %% Invalidate subscriber cache - subscription change means cached subscribers are stale
    invalidate_subscriber_cache(Topic),

    %% Get gateway node_id and endpoint from opts
    NodeId = maps:get(node_id, State#state.opts, <<"unknown">>),
    Url = maps:get(url, State#state.opts, <<"unknown">>),

    %% DEBUG: Log what we got from State#state.opts
    io:format("[GatewayPubSub DEBUG] Advertising subscription for topic ~s~n", [Topic]),
    io:format("[GatewayPubSub DEBUG] NodeId from opts: ~p~n", [NodeId]),
    io:format("[GatewayPubSub DEBUG] Url from opts: ~p~n", [Url]),

    %% Create DHT key by hashing the topic
    TopicKey = crypto:hash(sha256, Topic),

    %% Create subscriber value with endpoint info for routing messages back
    SubscriberValue = #{
        node_id => NodeId,
        endpoint => Url,
        ttl => 300  % 5 minutes TTL
    },

    %% Store subscription in DHT (will propagate to k closest nodes or bootstrap)
    io:format("[GatewayPubSub] Advertising subscription to topic ~s in DHT~n", [Topic]),
    try
        case whereis(macula_routing_server) of
            undefined ->
                io:format("[GatewayPubSub] Routing server not running, cannot advertise subscription~n");
            RoutingServerPid ->
                case macula_routing_server:store(RoutingServerPid, TopicKey, SubscriberValue) of
                    ok ->
                        io:format("[GatewayPubSub] Successfully stored subscription for ~s in DHT~n", [Topic]);
                    {error, StoreError} ->
                        io:format("[GatewayPubSub] Failed to store subscription ~s: ~p~n",
                                 [Topic, StoreError])
                end
        end
    catch
        _:DhtError ->
            io:format("[GatewayPubSub] Failed to advertise subscription ~s in DHT: ~p (continuing)~n",
                     [Topic, DhtError])
    end,
    ok.

%% @private
%% @doc Invalidate subscriber cache for a topic.
%% Called when subscription changes to ensure fresh DHT lookups.
-spec invalidate_subscriber_cache(binary()) -> ok.
invalidate_subscriber_cache(Topic) ->
    case whereis(macula_subscriber_cache) of
        undefined ->
            %% Cache not running yet - that's fine
            ok;
        _Pid ->
            macula_subscriber_cache:invalidate(Topic)
    end.
