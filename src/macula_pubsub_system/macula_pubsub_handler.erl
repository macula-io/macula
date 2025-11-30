%%%-------------------------------------------------------------------
%%% @doc
%%% PubSub handler GenServer - facade that orchestrates pub/sub operations.
%%%
%%% This module acts as a facade/coordinator, delegating business logic to:
%%% - macula_pubsub_subscription: Subscription storage, pattern matching, callbacks
%%% - macula_pubsub_dht: DHT advertisement, discovery, routing
%%% - macula_pubsub_qos: QoS 1 tracking and retry logic
%%%
%%% Responsibilities:
%%% - API facade for subscribe/unsubscribe/publish operations
%%% - Message routing coordination between specialized modules
%%% - GenServer lifecycle management
%%% - State management (delegates actual operations to modules)
%%%
%%% Extracted from macula_connection.erl (Phase 4)
%%% Refactored using TDD to extract god module (Phase 5)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_handler).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include("macula_config.hrl").

%% API
-export([start_link/1, subscribe/3, unsubscribe/2, publish/4, handle_incoming_publish/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    opts :: map(),
    node_id :: binary(),
    url :: binary(),
    realm :: binary(),

    %% Connection manager PID (looked up via gproc)
    connection_manager_pid :: pid() | undefined,

    %% Subscriptions: #{SubscriptionRef => {Topic, Callback}}
    subscriptions = #{} :: #{reference() => {binary(), fun((map()) -> ok)}},

    %% Advertised subscriptions with re-advertisement timers
    %% #{Topic => #{sub_ref, ttl, timer_ref}}
    advertised_subscriptions = #{} :: #{binary() => #{
        sub_ref := reference(),
        ttl := pos_integer(),
        timer_ref := reference()
    }},

    %% Pending publish acknowledgments for QoS 1
    %% #{MsgId => {Topic, Payload, QoS, RetryCount, TimerRef}}
    pending_pubacks = #{} :: #{binary() => {binary(), binary(), integer(), integer(), reference()}},

    %% Pending subscriber queries for DHT discovery
    %% #{MsgId => {Topic, Payload, QoS, Opts}}
    pending_subscriber_queries = #{} :: #{binary() => {binary(), binary(), integer(), map()}},

    %% Message ID counter
    msg_id_counter = 0 :: non_neg_integer(),

    %% Topic pattern matching configuration
    topic_separator = <<".">> :: binary(),
    topic_wildcard_single = <<"*">> :: binary(),
    topic_wildcard_multi = <<"**">> :: binary(),

    %% Service registry for DHT operations
    service_registry :: macula_service_registry:registry()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    %% No local registration - allows multiple connection instances
    gen_server:start_link(?MODULE, Opts, []).

-spec subscribe(pid(), binary() | list() | atom(), fun((map()) -> ok)) -> {ok, reference()} | {error, term()}.
subscribe(Pid, Topic, Callback) ->
    gen_server:call(Pid, {subscribe, Topic, Callback}, 5000).

-spec unsubscribe(pid(), reference()) -> ok | {error, term()}.
unsubscribe(Pid, SubRef) ->
    gen_server:call(Pid, {unsubscribe, SubRef}, 5000).

-spec publish(pid(), binary() | list() | atom(), term(), map()) -> ok | {error, term()}.
publish(Pid, Topic, Data, Opts) ->
    %% Use cast to avoid blocking on publish operations
    %% The internal handler will manage async delivery and QoS retries
    gen_server:cast(Pid, {publish_async, Topic, Data, Opts}),
    ok.

-spec handle_incoming_publish(pid(), map()) -> ok.
handle_incoming_publish(Pid, Msg) ->
    gen_server:cast(Pid, {incoming_publish, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    ?LOG_INFO("PubSub handler starting"),

    %% Extract required options
    NodeId = maps:get(node_id, Opts, generate_node_id()),
    Url = maps:get(url, Opts, <<"unknown">>),
    Realm = maps:get(realm, Opts, <<"default">>),
    PeerId = maps:get(peer_id, Opts, erlang:unique_integer([monotonic, positive])),

    %% Register in gproc for incoming publish message routing
    %% Use {Realm, PeerId} to support multiple peer connections per realm
    true = gproc:reg({n, l, {pubsub_handler, Realm, PeerId}}),
    ?LOG_INFO("PubSub handler registered in gproc for realm ~s, peer_id ~p", [Realm, PeerId]),

    %% connection_manager_pid will be set via cast message after init
    ConnMgrPid = undefined,

    %% Initialize service registry for DHT operations
    Registry = macula_service_registry:new(),

    %% Topic matching configuration (customizable)
    TopicSeparator = maps:get(topic_separator, Opts, <<".">>),
    WildcardSingle = maps:get(topic_wildcard_single, Opts, <<"*">>),
    WildcardMulti = maps:get(topic_wildcard_multi, Opts, <<"**">>),

    {ok, #state{
        opts = Opts,
        node_id = NodeId,
        url = Url,
        realm = Realm,
        connection_manager_pid = ConnMgrPid,
        service_registry = Registry,
        topic_separator = TopicSeparator,
        topic_wildcard_single = WildcardSingle,
        topic_wildcard_multi = WildcardMulti
    }}.

%%%===================================================================
%%% Subscribe/Unsubscribe
%%%===================================================================

handle_call({subscribe, Topic, Callback}, _From, State) ->
    %% Generate subscription reference
    SubRef = make_ref(),
    BinaryTopic = ensure_binary(Topic),

    %% Store subscription locally first (delegate to subscription module)
    {ok, UpdatedSubscriptions, SubRef} = macula_pubsub_subscription:add_subscription(
        BinaryTopic, Callback, State#state.subscriptions, SubRef
    ),
    State2 = State#state{subscriptions = UpdatedSubscriptions},

    %% Send subscribe message via connection manager ASYNC (fire-and-forget)
    %% This prevents blocking when the connection is busy or slow
    SubscribeMsg = #{
        topics => [BinaryTopic],
        qos => 0
    },
    macula_connection:send_message_async(State#state.connection_manager_pid, subscribe, SubscribeMsg),

    %% Advertise subscription in DHT (delegate to DHT module)
    {ok, SubInfo} = macula_pubsub_dht:advertise_subscription(
        BinaryTopic, SubRef, State#state.node_id, State#state.url,
        State#state.connection_manager_pid
    ),
    AdvertisedSubscriptions = State2#state.advertised_subscriptions,
    State3 = State2#state{
        advertised_subscriptions = AdvertisedSubscriptions#{BinaryTopic => SubInfo}
    },

    {reply, {ok, SubRef}, State3};

handle_call({unsubscribe, SubRef}, _From, State) ->
    %% Remove subscription (delegate to subscription module)
    case macula_pubsub_subscription:remove_subscription(SubRef, State#state.subscriptions) of
        {error, not_found} ->
            {reply, {error, not_subscribed}, State};
        {ok, UpdatedSubscriptions, Topic} ->
            %% Send unsubscribe message via connection manager ASYNC (fire-and-forget)
            %% This prevents blocking when the connection is busy or slow
            UnsubscribeMsg = #{
                topics => [Topic]
            },
            macula_connection:send_message_async(State#state.connection_manager_pid, unsubscribe, UnsubscribeMsg),

            %% Cancel DHT advertisement (delegate to DHT module)
            UpdatedAdvertised = macula_pubsub_dht:cancel_advertisement(
                Topic, State#state.advertised_subscriptions
            ),

            State2 = State#state{
                subscriptions = UpdatedSubscriptions,
                advertised_subscriptions = UpdatedAdvertised
            },
            {reply, ok, State2}
    end;

%%%===================================================================
%%% Publish
%%%===================================================================

handle_call({publish, Topic, Data, Opts}, _From, State) ->
    %% Check if we have a connection manager and are connected
    do_sync_publish(State#state.connection_manager_pid, Topic, Data, Opts, State);

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%%%===================================================================
%%% Async Publish Operations
%%%===================================================================

%% Set connection manager PID (sent by facade after init)
handle_cast({set_connection_manager_pid, Pid}, State) ->
    {noreply, State#state{connection_manager_pid = Pid}};

%% Async publish (fire-and-forget from caller's perspective)
%% This handles the {publish_async, ...} cast from the publish/4 API function
handle_cast({publish_async, Topic, Data, Opts}, State) ->
    ?LOG_INFO("[PubSubHandler] publish_async received: topic=~p", [Topic]),
    do_async_publish(State#state.connection_manager_pid, Topic, Data, Opts, State);

handle_cast({do_publish, PublishMsg, Qos, BinaryTopic, Payload, Opts, MsgId}, State) ->
    %% HYBRID: Send to gateway for routing AND discover via DHT
    %% This enables both gateway-centric and pure P2P topologies
    ?LOG_INFO("[PubSubHandler] do_publish: topic=~s, ConnMgr=~p",
              [BinaryTopic, State#state.connection_manager_pid]),

    %% Handle QoS 1 (at-least-once delivery) - delegate to QoS module
    {ok, UpdatedPendingPubacks} = macula_pubsub_qos:track_message(
        MsgId, BinaryTopic, Payload, Qos, State#state.pending_pubacks
    ),
    State2 = State#state{pending_pubacks = UpdatedPendingPubacks},

    %% Send publish to gateway for routing to connected subscribers
    send_publish_to_gateway(State#state.connection_manager_pid, PublishMsg, BinaryTopic),

    %% Also discover subscribers via DHT for pure P2P routing (if available)
    gen_server:cast(self(), {discover_subscribers, BinaryTopic, Payload, Qos, Opts}),

    {noreply, State2};

%% @private
%% Handle async discovery of remote subscribers (mesh-wide pub/sub)
%% Delegate to DHT module for discovery and routing
handle_cast({discover_subscribers, Topic, Payload, Qos, _Opts}, State) ->
    %% Delegate to DHT module for discovery (async)
    case macula_pubsub_dht:discover_subscribers(
        Topic, Payload, Qos,
        State#state.connection_manager_pid,
        State#state.service_registry,
        State#state.msg_id_counter
    ) of
        {cached, Subscribers, UpdatedRegistry} ->
            %% Cache hit - route to subscribers immediately
            macula_pubsub_dht:route_to_subscribers(
                Topic, Payload, Qos, Subscribers, State#state.node_id
            ),
            {noreply, State#state{service_registry = UpdatedRegistry}};

        {query_sent, PendingQueries, MsgId, UpdatedRegistry} ->
            %% Query sent - track it
            UpdatedPendingQueries = maps:merge(
                State#state.pending_subscriber_queries,
                PendingQueries
            ),
            {_MsgId, NewCounter} = macula_utils:next_message_id(State#state.msg_id_counter),
            State2 = State#state{
                pending_subscriber_queries = UpdatedPendingQueries,
                service_registry = UpdatedRegistry,
                msg_id_counter = NewCounter
            },
            ?LOG_DEBUG("[~s] DHT query sent for topic ~s (MsgId: ~s)",
                      [State#state.node_id, Topic, MsgId]),
            {noreply, State2}
    end;

%%%===================================================================
%%% Incoming Publish Routing
%%%===================================================================

handle_cast({incoming_publish, Msg}, State) ->
    %% Handle incoming publish (for subscriptions)
    %% Support both atom and binary keys from MessagePack decoding
    Topic = extract_topic(Msg),
    Payload = extract_payload(Msg),

    ?LOG_DEBUG("[~s] Received PUBLISH message: topic=~s, payload_size=~p bytes",
              [State#state.node_id, Topic, byte_size(Payload)]),

    %% Find matching subscriptions (delegate to subscription module)
    Config = #{
        topic_separator => State#state.topic_separator,
        topic_wildcard_single => State#state.topic_wildcard_single,
        topic_wildcard_multi => State#state.topic_wildcard_multi
    },
    SubscriptionMatches = macula_pubsub_subscription:find_matches(
        Topic, State#state.subscriptions, Config
    ),

    %% Invoke callbacks for matching subscriptions (delegate to subscription module)
    macula_pubsub_subscription:invoke_callbacks(
        SubscriptionMatches, Topic, Payload, State#state.node_id
    ),

    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%%===================================================================
%%% Timer Handlers
%%%===================================================================

handle_info({resubscribe, Topic}, State) ->
    SubInfo = maps:get(Topic, State#state.advertised_subscriptions, undefined),
    do_resubscribe(SubInfo, Topic, State);

handle_info({puback_timeout, MsgId}, State) ->
    %% Delegate QoS timeout handling to QoS module
    TimeoutResult = macula_pubsub_qos:handle_timeout(MsgId, State#state.connection_manager_pid, State#state.pending_pubacks),
    handle_puback_timeout_result(TimeoutResult, State);

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?LOG_INFO("PubSub handler terminating"),
    ok.


%%%===================================================================
%%% Internal functions - Helpers
%%%===================================================================

%% @doc Generate next message ID
-spec next_message_id(#state{}) -> {binary(), #state{}}.
next_message_id(State) ->
    Counter = State#state.msg_id_counter,
    {MsgId, NewCounter} = macula_utils:next_message_id(Counter),
    {MsgId, State#state{msg_id_counter = NewCounter}}.

%% @doc Ensure value is binary
-spec ensure_binary(binary() | list() | atom()) -> binary().
ensure_binary(Value) ->
    macula_utils:ensure_binary(Value).


%% @doc Encode map/list to JSON binary
-spec encode_json(map() | list()) -> binary().
encode_json(Data) ->
    macula_utils:encode_json(Data).

%% @doc Generate a random node ID
-spec generate_node_id() -> binary().
generate_node_id() ->
    macula_utils:generate_node_id().

%% @doc Encode payload data to binary (pattern matching on type)
-spec encode_payload(binary() | map() | list()) -> binary().
encode_payload(Data) when is_binary(Data) -> Data;
encode_payload(Data) when is_map(Data) -> encode_json(Data);
encode_payload(Data) when is_list(Data) -> list_to_binary(Data).

%% @doc Extract topic from message, supporting both atom and binary keys.
%% MessagePack decoding may use either key format.
-spec extract_topic(map()) -> binary().
extract_topic(#{topic := Topic}) -> Topic;
extract_topic(#{<<"topic">> := Topic}) -> Topic.

%% @doc Extract payload from message, supporting both atom and binary keys.
%% MessagePack decoding may use either key format.
-spec extract_payload(map()) -> binary().
extract_payload(#{payload := Payload}) -> Payload;
extract_payload(#{<<"payload">> := Payload}) -> Payload.

%%%===================================================================
%%% Publish helpers
%%%===================================================================

%% @private No connection manager - cannot publish
do_sync_publish(undefined, _Topic, _Data, _Opts, State) ->
    {reply, {error, not_connected}, State};
%% @private Connection manager available - check status
do_sync_publish(ConnMgrPid, Topic, Data, Opts, State) ->
    Status = macula_connection:get_status(ConnMgrPid),
    do_sync_publish_with_status(Status, Topic, Data, Opts, State).

%% @private Not connected
do_sync_publish_with_status(Status, _Topic, _Data, _Opts, State) when Status =/= connected ->
    {reply, {error, not_connected}, State};
%% @private Connected - build and send publish message
do_sync_publish_with_status(connected, Topic, Data, Opts, State) ->
    Qos = maps:get(qos, Opts, 0),
    Retain = maps:get(retain, Opts, false),

    {MsgId, State2} = next_message_id(State),
    BinaryTopic = ensure_binary(Topic),
    Payload = encode_payload(Data),

    PublishMsg = #{
        topic => BinaryTopic,
        payload => Payload,
        qos => Qos,
        retain => Retain,
        message_id => MsgId
    },

    %% Send publish message asynchronously using cast to self
    gen_server:cast(self(), {do_publish, PublishMsg, Qos, BinaryTopic, Payload, Opts, MsgId}),

    {reply, ok, State2}.

%% @private No connection manager - silently drop (fire-and-forget semantics)
do_async_publish(undefined, _Topic, _Data, _Opts, State) ->
    ?LOG_WARNING("[PubSubHandler] Publish dropped - no connection manager"),
    {noreply, State};
%% @private Connection manager available - send async
do_async_publish(_ConnMgrPid, Topic, Data, Opts, State) ->
    Qos = maps:get(qos, Opts, 0),
    Retain = maps:get(retain, Opts, false),

    {MsgId, State2} = next_message_id(State),
    BinaryTopic = ensure_binary(Topic),
    Payload = encode_payload(Data),

    PublishMsg = #{
        topic => BinaryTopic,
        payload => Payload,
        qos => Qos,
        retain => Retain,
        message_id => MsgId
    },

    ?LOG_INFO("[PubSubHandler] Sending to do_publish: topic=~s", [BinaryTopic]),
    gen_server:cast(self(), {do_publish, PublishMsg, Qos, BinaryTopic, Payload, Opts, MsgId}),

    {noreply, State2}.

%% @private No connection manager - log error
send_publish_to_gateway(undefined, _PublishMsg, _BinaryTopic) ->
    ?LOG_ERROR("[PubSubHandler] No connection manager for publish!");
%% @private Send publish message via the gateway connection ASYNC (fire-and-forget)
send_publish_to_gateway(ConnMgrPid, PublishMsg, BinaryTopic) ->
    ?LOG_INFO("[PubSubHandler] Calling send_message_async: pid=~p, topic=~s",
              [ConnMgrPid, BinaryTopic]),
    macula_connection:send_message_async(ConnMgrPid, publish, PublishMsg),
    ?LOG_INFO("[PubSubHandler] send_message_async returned").

%%%===================================================================
%%% Resubscription helpers
%%%===================================================================

%% @private Subscription was removed, don't re-advertise
do_resubscribe(undefined, Topic, State) ->
    ?LOG_DEBUG("[~s] Skipping re-subscription for ~s (no longer subscribed)",
              [State#state.node_id, Topic]),
    {noreply, State};
%% @private Re-advertise subscription
do_resubscribe(SubInfo, Topic, State) ->
    %% Cancel old timer
    OldTimerRef = maps:get(timer_ref, SubInfo),
    erlang:cancel_timer(OldTimerRef),

    %% Re-advertise (delegate to DHT module)
    SubRef = maps:get(sub_ref, SubInfo),
    {ok, UpdatedSubInfo} = macula_pubsub_dht:advertise_subscription(
        Topic, SubRef, State#state.node_id, State#state.url,
        State#state.connection_manager_pid
    ),

    %% Update advertised subscriptions map
    UpdatedAdvertised = (State#state.advertised_subscriptions)#{Topic => UpdatedSubInfo},
    State2 = State#state{advertised_subscriptions = UpdatedAdvertised},

    ?LOG_DEBUG("[~s] Re-advertised subscription for topic ~s",
              [State#state.node_id, Topic]),
    {noreply, State2}.

%%%===================================================================
%%% QoS timeout helpers
%%%===================================================================

%% @private Handle retry case - send retry message ASYNC
handle_puback_timeout_result({retry, UpdatedPending, PublishMsg}, State) ->
    macula_connection:send_message_async(State#state.connection_manager_pid, publish, PublishMsg),
    {noreply, State#state{pending_pubacks = UpdatedPending}};
%% @private Handle give up case
handle_puback_timeout_result({give_up, UpdatedPending}, State) ->
    {noreply, State#state{pending_pubacks = UpdatedPending}};
%% @private Handle not found case
handle_puback_timeout_result({not_found, UpdatedPending}, State) ->
    {noreply, State#state{pending_pubacks = UpdatedPending}}.

