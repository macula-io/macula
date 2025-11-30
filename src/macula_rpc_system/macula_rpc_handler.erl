%%%-------------------------------------------------------------------
%%% @doc
%%% RPC handler GenServer - manages RPC calls, replies, and failover.
%%%
%%% Responsibilities:
%%% - Execute RPC calls (local check first, then DHT discovery)
%%% - Handle incoming RPC replies from network
%%% - Manage call timeouts with automatic failover
%%% - Track pending calls with call IDs
%%% - Monitor caller processes for automatic cleanup
%%% - Provider selection strategies (random, round-robin, etc.)
%%%
%%% Memory Safety:
%%% - Monitors caller processes to prevent memory leaks
%%% - Cleans up immediately when caller dies (no waiting for timeout)
%%% - Cancels timers and removes pending entries on cleanup
%%%
%%% Extracted from macula_connection.erl (Phase 5)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_handler).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include("macula_config.hrl").

%% API
-export([start_link/1, call/4, register_handler/2, unregister_handler/1, register_local_procedure/3, handle_incoming_reply/2, handle_find_value_reply/2]).

%% Async RPC API (NATS-style)
-export([request/4, request_to/5, handle_async_reply/2]).

%% Service registry access
-export([get_service_registry/1]).

%% Pull-based service discovery
-export([prefetch_services/2, get_service_interests/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Pending async request record (NATS-style)
-record(pending_request, {
    id :: binary(),
    procedure :: binary(),
    callback :: {fun_cb, fun()} | {pid_cb, pid()} | {gen_server_cb, term()},
    timer_ref :: reference(),
    sent_at :: integer(),
    target_node :: binary() | undefined
}).

%% Pending async DHT discovery record
%% Queues async RPC requests while DHT discovery is in progress
-record(pending_async_discovery, {
    service_key :: binary(),
    procedure :: binary(),
    queued_requests :: [{binary(), term(), map(), {fun_cb, fun()} | {pid_cb, pid()}}],
    timer_ref :: reference(),
    started_at :: integer()
}).

-record(state, {
    opts :: map(),
    node_id :: binary(),
    realm :: binary(),

    %% Reference to connection manager for sending messages
    connection_manager_pid :: pid(),

    %% Pending RPC calls: #{CallId => {From, Timer, MonitorRef} | {From, Timer, MonitorRef, FailoverContext}}
    pending_calls = #{} :: #{binary() => {term(), reference(), reference()} | {term(), reference(), reference(), map()}},

    %% Pending DHT queries: #{QueryKey => {From, Procedure, Args, Opts, Registry, Timer, MonitorRef}}
    pending_queries = #{} :: #{binary() => {term(), binary(), term(), map(), term(), reference(), reference()}},

    %% Pending async requests (NATS-style): #{RequestId => #pending_request{}}
    pending_requests = #{} :: #{binary() => #pending_request{}},

    %% Pending async DHT discoveries: #{ServiceKey => #pending_async_discovery{}}
    %% Queues async RPC requests while DHT lookup is in progress
    pending_async_discoveries = #{} :: #{binary() => #pending_async_discovery{}},

    %% Caller monitors: #{MonitorRef => {call, CallId} | {query, QueryKey}}
    caller_monitors = #{} :: #{reference() => {atom(), binary()}},

    %% Message ID counter
    msg_id_counter = 0 :: non_neg_integer(),

    %% Service registry for DHT operations and local handler lookup
    service_registry :: macula_service_registry:registry(),

    %% Provider selection strategy state
    provider_selector_state = #{strategy => random} :: map(),

    %% Service interests for pull-based discovery on startup
    %% List of service names to prefetch when connection manager is ready
    service_interests = [] :: [binary()]
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    %% No local registration - allows multiple connection instances
    gen_server:start_link(?MODULE, Opts, []).

-spec call(pid(), binary() | list() | atom(), term(), map()) -> {ok, term()} | {error, term()}.
call(Pid, Procedure, Args, Opts) ->
    Timeout = maps:get(timeout, Opts, ?DEFAULT_CALL_TIMEOUT),
    gen_server:call(Pid, {call, Procedure, Args, Opts}, Timeout + 1000).

-spec register_handler(binary() | list() | atom(), fun((term()) -> {ok, term()} | {error, term()})) ->
    {ok, reference()} | {error, term()}.
register_handler(_Service, _Handler) ->
    
    %%  Handler registration involves DHT advertisement and re-advertisement timers
    %%  This belongs in the advertisement_manager module
    {error, not_implemented_use_advertisement_manager}.

-spec unregister_handler(binary() | list() | atom()) -> ok | {error, term()}.
unregister_handler(_Service) ->

    {error, not_implemented_use_advertisement_manager}.

%% @doc Register a local procedure handler (no DHT advertisement).
%%
%% This registers the handler function in the service registry so this RPC handler
%% can execute it locally when called. Unlike register_handler/2, this does NOT
%% advertise to the DHT - it's for purely local services.
-spec register_local_procedure(pid(), binary(), fun((term()) -> {ok, term()} | {error, term()})) -> ok.
register_local_procedure(Pid, Procedure, Handler) when is_pid(Pid), is_binary(Procedure), is_function(Handler) ->
    gen_server:cast(Pid, {register_local_procedure, Procedure, Handler}).

-spec handle_incoming_reply(pid(), map()) -> ok.
handle_incoming_reply(Pid, Msg) ->
    gen_server:cast(Pid, {incoming_reply, Msg}).

%% @doc Handle FIND_VALUE_REPLY from DHT query
-spec handle_find_value_reply(pid(), map()) -> ok.
handle_find_value_reply(Pid, Msg) ->
    gen_server:cast(Pid, {find_value_reply, Msg}).

%% @doc Get service registry (for local handler lookup by gateway).
-spec get_service_registry(pid()) -> macula_service_registry:registry().
get_service_registry(Pid) ->
    gen_server:call(Pid, get_service_registry).

%% @doc Dynamically prefetch services for pull-based discovery.
%% This allows adding service interests at runtime (after init).
%% Services are looked up via DHT and cached for faster first requests.
-spec prefetch_services(pid(), [binary() | atom() | string()]) -> ok.
prefetch_services(Pid, Services) ->
    gen_server:cast(Pid, {prefetch_services, Services}).

%% @doc Get the list of configured service interests.
-spec get_service_interests(pid()) -> [binary()].
get_service_interests(Pid) ->
    gen_server:call(Pid, get_service_interests).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    ?LOG_INFO("RPC handler starting"),

    %% Extract required options
    NodeId = maps:get(node_id, Opts, generate_node_id()),
    Realm = maps:get(realm, Opts, <<"default">>),

    %% Register this RPC handler with gproc so connection manager can find us
    %% CRITICAL: Use {Realm, NodeId} (not PeerId) so that replies arriving on ANY
    %% connection belonging to this node can find the RPC handler. Each peer has
    %% one RPC handler per realm, but may have multiple connections (outbound to
    %% gateway, inbound from gateway relay, etc.) - all must route to the same handler.
    %%
    %% NOTE: A node may have multiple peer connections (e.g., reconnecting after
    %% a disconnect), each with its own macula_peer_system supervisor. To avoid
    %% gproc registration conflicts, we check if a handler is already registered.
    %% If so, this new handler should exit - the existing one handles all connections.
    GprocKey = {n, l, {rpc_handler, Realm, NodeId}},
    case gproc:where(GprocKey) of
        undefined ->
            gproc:reg(GprocKey),
            ?LOG_INFO("RPC handler registered in gproc for realm ~s, node_id ~s",
                     [Realm, binary:encode_hex(NodeId)]),
            init_state(Opts, NodeId, Realm);
        ExistingPid ->
            %% Another RPC handler already exists for this node/realm
            %% Return ignore instead of stop - supervisor won't treat this as failure
            ?LOG_DEBUG("RPC handler already registered at ~p for realm ~s, node_id ~s - reusing existing",
                      [ExistingPid, Realm, binary:encode_hex(NodeId)]),
            ignore
    end.

%% @private Initialize the handler state after successful gproc registration
init_state(Opts, NodeId, Realm) ->
    %% connection_manager_pid will be set via cast message after init
    ConnMgrPid = undefined,

    %% Initialize service registry for DHT operations and local handlers
    Registry = macula_service_registry:new(),

    %% Provider selection strategy (random, round_robin, least_connections, etc.)
    Strategy = maps:get(provider_strategy, Opts, random),
    SelectorState = #{strategy => Strategy},

    %% Service interests for pull-based discovery (list of service names to prefetch)
    ServiceInterests = normalize_service_interests(maps:get(service_interests, Opts, [])),
    log_service_interests(ServiceInterests),

    {ok, #state{
        opts = Opts,
        node_id = NodeId,
        realm = Realm,
        connection_manager_pid = ConnMgrPid,
        service_registry = Registry,
        provider_selector_state = SelectorState,
        service_interests = ServiceInterests
    }}.

%%%===================================================================
%%% RPC Call
%%%===================================================================

handle_call({call, Procedure, Args, Opts}, From, State) ->
    Registry = State#state.service_registry,
    BinaryProcedure = ensure_binary(Procedure),

    %% Check if this is a local service first
    LocalResult = macula_service_registry:get_local_handler(Registry, BinaryProcedure),
    %% If not local, check if it's a gateway-direct procedure (_dht.*)
    do_call_maybe_direct(LocalResult, BinaryProcedure, Args, Opts, From, State);

%% Async RPC request with DHT discovery (NATS-style)
handle_call({async_request, Procedure, Args, Opts}, From, State) ->
    BinaryProcedure = ensure_binary(Procedure),
    do_async_request(BinaryProcedure, Args, Opts, From, State);

%% Async RPC request to specific node (skip DHT)
handle_call({async_request_to, TargetNodeId, Procedure, Args, Opts}, From, State) ->
    BinaryProcedure = ensure_binary(Procedure),
    do_async_request_to(TargetNodeId, BinaryProcedure, Args, Opts, From, State);

%% Get service registry for local handler lookup
handle_call(get_service_registry, _From, State) ->
    {reply, State#state.service_registry, State};

%% Get configured service interests
handle_call(get_service_interests, _From, State) ->
    {reply, State#state.service_interests, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Set connection manager PID (sent by facade after init)
%% Also triggers pull-based service discovery if service_interests configured
handle_cast({set_connection_manager_pid, Pid}, State) ->
    State2 = State#state{connection_manager_pid = Pid},
    %% Trigger pull-based discovery for configured service interests
    State3 = prefetch_service_interests(State2),
    {noreply, State3};

%% Register local procedure handler
handle_cast({register_local_procedure, Procedure, Handler}, State) ->
    Registry = State#state.service_registry,
    NewRegistry = macula_service_registry:advertise_local(Registry, Procedure, Handler, #{}),
    ?LOG_INFO("[~s] Registered local procedure: ~s", [State#state.node_id, Procedure]),
    {noreply, State#state{service_registry = NewRegistry}};

%% Dynamic prefetch services at runtime
handle_cast({prefetch_services, Services}, State) ->
    NormalizedServices = normalize_service_interests(Services),
    %% Add to service interests and trigger prefetch
    NewInterests = lists:usort(State#state.service_interests ++ NormalizedServices),
    State2 = State#state{service_interests = NewInterests},
    %% Prefetch the new services (reuses existing logic)
    State3 = lists:foldl(fun prefetch_single_service/2, State2, NormalizedServices),
    {noreply, State3};

handle_cast({incoming_reply, Msg}, State) ->
    %% Handle RPC reply from network
    %% MessagePack decoder returns binary keys
    CallId = maps:get(<<"call_id">>, Msg, maps:get(call_id, Msg, undefined)),
    do_incoming_reply(CallId, Msg, State);

handle_cast({find_value_reply, Msg}, State) ->
    %% Handle FIND_VALUE_REPLY from DHT query
    ?LOG_INFO("[~s] Received FIND_VALUE_REPLY: ~p", [State#state.node_id, Msg]),

    %% Decode the reply to get value or nodes
    DecodeResult = macula_routing_protocol:decode_find_value_reply(Msg),
    do_find_value_reply(DecodeResult, State);

%% Handle incoming async RPC reply (NATS-style)
handle_cast({async_rpc_reply, Msg}, State) ->
    RequestId = maps:get(<<"request_id">>, Msg, maps:get(request_id, Msg, undefined)),
    do_async_rpc_reply(RequestId, Msg, State);

handle_cast(_Msg, State) ->
    {noreply, State}.

%%%===================================================================
%%% Timeout Handlers
%%%===================================================================

handle_info({call_timeout, CallId}, State) ->
    %% RPC call timed out
    PendingInfo = maps:get(CallId, State#state.pending_calls, undefined),
    do_call_timeout(PendingInfo, CallId, State);

handle_info({find_value_timeout, ServiceKey}, State) ->
    %% DHT query timed out
    PendingInfo = maps:get(ServiceKey, State#state.pending_queries, undefined),
    do_find_value_timeout(PendingInfo, ServiceKey, State);

handle_info({'DOWN', MonitorRef, process, _Pid, _Reason}, State) ->
    %% Caller process died - clean up immediately
    MonitorInfo = maps:get(MonitorRef, State#state.caller_monitors, undefined),
    do_caller_down(MonitorInfo, MonitorRef, State);

%% Async RPC request timeout (NATS-style)
handle_info({request_timeout, RequestId}, State) ->
    PendingReq = maps:get(RequestId, State#state.pending_requests, undefined),
    do_request_timeout(PendingReq, RequestId, State);

%% Async DHT discovery timeout
handle_info({async_dht_discovery_timeout, ServiceKey}, State) ->
    PendingDiscovery = maps:get(ServiceKey, State#state.pending_async_discoveries, undefined),
    do_async_dht_discovery_timeout(PendingDiscovery, ServiceKey, State);

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?LOG_INFO("RPC handler terminating"),
    ok.

%%%===================================================================
%%% Async RPC API (NATS-Style Request/Reply)
%%%===================================================================

%% @doc Send async RPC request with callback (NATS-style).
%%
%% This is the primary API for async RPC. The caller does not block.
%% Response is delivered via callback function or process message.
%%
%% Opts:
%%   - callback => fun((Result) -> any()) - Called with {ok, Result} | {error, Reason}
%%   - timeout => integer() - Milliseconds before timeout (default: 5000)
%%
%% Returns {ok, RequestId} immediately, or {error, Reason} on send failure.
-spec request(pid(), binary(), term(), map()) -> {ok, binary()} | {error, term()}.
request(Pid, Procedure, Args, Opts) ->
    gen_server:call(Pid, {async_request, Procedure, Args, Opts}).

%% @doc Send async RPC request to specific node (skip DHT lookup).
%%
%% Use this when you already know the target node's ID (e.g., from a previous
%% DHT discovery or direct node advertisement).
-spec request_to(pid(), binary(), binary(), term(), map()) -> {ok, binary()} | {error, term()}.
request_to(Pid, TargetNodeId, Procedure, Args, Opts) ->
    gen_server:call(Pid, {async_request_to, TargetNodeId, Procedure, Args, Opts}).

%% @doc Handle incoming async RPC reply (called by message router).
-spec handle_async_reply(pid(), map()) -> ok.
handle_async_reply(Pid, Msg) ->
    gen_server:cast(Pid, {async_rpc_reply, Msg}).

%%%===================================================================
%%% Internal functions - RPC Call Logic
%%%===================================================================

%% @private Local handler found - execute directly
do_call_maybe_direct({ok, Handler}, BinaryProcedure, Args, Opts, From, State) ->
    do_call({ok, Handler}, BinaryProcedure, Args, Opts, From, State);
%% @private No local handler - check if it's a gateway-direct procedure
do_call_maybe_direct(not_found, <<"_dht.", _/binary>> = Procedure, Args, Opts, From, State) ->
    %% _dht.* procedures are handled directly by the gateway (no DHT lookup needed)
    do_gateway_direct_call(Procedure, Args, Opts, From, State);
%% @private No local handler, not gateway-direct - use normal DHT discovery
do_call_maybe_direct(not_found, BinaryProcedure, Args, Opts, From, State) ->
    do_call(not_found, BinaryProcedure, Args, Opts, From, State).

%% @private Send RPC directly to gateway (for _dht.* procedures)
do_gateway_direct_call(Procedure, _Args, _Opts, _From, #state{connection_manager_pid = undefined} = State) ->
    %% Connection manager not yet set - can't send
    ?LOG_WARNING("[~s] Gateway-direct RPC ~s failed: connection_manager not ready",
                [State#state.node_id, Procedure]),
    {reply, {error, connection_not_ready}, State};
do_gateway_direct_call(Procedure, Args, Opts, From, State) ->
    %% Generate call ID
    {CallId, State2} = next_message_id(State),

    %% Encode args
    EncodedArgs = encode_call_args(Args),

    %% Build direct call message (not routed via DHT)
    CallMsg = #{
        call_id => CallId,
        procedure => Procedure,
        args => EncodedArgs,
        realm => State#state.realm
    },

    %% Send call message directly via connection manager
    ConnMgrPid = State2#state.connection_manager_pid,
    case macula_connection:send_message(ConnMgrPid, call, CallMsg) of
        ok ->
            %% Set up timeout timer
            Timeout = maps:get(timeout, Opts, ?DEFAULT_CALL_TIMEOUT),
            Timer = erlang:send_after(Timeout, self(), {call_timeout, CallId}),

            %% Monitor caller process for automatic cleanup
            {CallerPid, _Tag} = From,
            MonitorRef = erlang:monitor(process, CallerPid),

            %% Store pending call (simplified - no failover context for gateway-direct)
            FailoverContext = #{ procedure => Procedure, args => Args, opts => Opts },
            PendingInfo = {From, Timer, MonitorRef, FailoverContext},
            PendingCalls = maps:put(CallId, PendingInfo, State2#state.pending_calls),

            %% Track monitor
            CallerMonitors = maps:put(MonitorRef, {call, CallId}, State2#state.caller_monitors),

            ?LOG_DEBUG("[~s] Gateway-direct RPC ~s sent (call_id: ~s)",
                      [State2#state.node_id, Procedure, binary:encode_hex(CallId)]),
            {noreply, State2#state{pending_calls = PendingCalls, caller_monitors = CallerMonitors}};

        {error, Reason} ->
            ?LOG_ERROR("[~s] Failed to send gateway-direct RPC ~s: ~p",
                      [State2#state.node_id, Procedure, Reason]),
            {reply, {error, {send_failed, Reason}}, State2}
    end.

%% @private Local handler found - execute directly
do_call({ok, Handler}, _BinaryProcedure, Args, _Opts, From, State) ->
    spawn(fun() ->
        try
            Result = Handler(Args),
            gen_server:reply(From, Result)
        catch
            _:Error ->
                gen_server:reply(From, {error, Error})
        end
    end),
    {noreply, State};
%% @private No local handler - try service discovery via DHT
do_call(not_found, BinaryProcedure, Args, Opts, From, State) ->
    Registry = State#state.service_registry,
    DiscoveryResult = macula_service_registry:discover_service(Registry, BinaryProcedure),
    do_call_discovery(DiscoveryResult, BinaryProcedure, Args, Opts, From, State).

%% @private Cache hit with providers - make remote call
do_call_discovery({ok, Providers, Registry2}, BinaryProcedure, Args, Opts, From, State)
    when Providers =/= [] ->
    State2 = State#state{service_registry = Registry2},
    do_remote_call(BinaryProcedure, Args, Opts, From, Providers, State2);
%% @private Cache hit but no providers
do_call_discovery({ok, [], Registry2}, _BinaryProcedure, _Args, _Opts, _From, State) ->
    State2 = State#state{service_registry = Registry2},
    {reply, {error, service_not_found}, State2};
%% @private Cache miss - send FIND_VALUE to DHT node (async)
do_call_discovery({cache_miss, Registry2}, BinaryProcedure, Args, Opts, From, State) ->
    ServiceKey = crypto:hash(sha256, BinaryProcedure),
    State2 = State#state{service_registry = Registry2},
    send_find_value_async(ServiceKey, BinaryProcedure, Args, Opts, From, State2).

%% @private No call_id in reply message
do_incoming_reply(undefined, _Msg, State) ->
    ?LOG_WARNING("[~s] Received REPLY without call_id", [State#state.node_id]),
    {noreply, State};
%% @private Valid call_id - handle the reply
do_incoming_reply(CallId, Msg, State) ->
    handle_rpc_reply(CallId, Msg, State).

%% @private DHT returned providers
do_find_value_reply({ok, {value, Providers}}, State) when is_list(Providers) ->
    ?LOG_INFO("[~s] DHT returned ~p provider(s)", [State#state.node_id, length(Providers)]),
    handle_dht_providers_found(Providers, State);
%% @private DHT returned nodes (no value found)
do_find_value_reply({ok, {nodes, _Nodes}}, State) ->
    ?LOG_DEBUG("[~s] DHT returned nodes (no value found)", [State#state.node_id]),
    {noreply, State};
%% @private DHT decode error
do_find_value_reply({error, Reason}, State) ->
    ?LOG_WARNING("[~s] Failed to decode find_value_reply: ~p", [State#state.node_id, Reason]),
    {noreply, State}.

%% @private Call not found (already completed)
do_call_timeout(undefined, _CallId, State) ->
    {noreply, State};
%% @private Call timed out - attempt failover
do_call_timeout({From, _Timer, MonitorRef, FailoverContext}, CallId, State) ->
    ?LOG_WARNING("[~s] RPC call ~s timed out, attempting failover",
                [State#state.node_id, binary:encode_hex(CallId)]),

    %% Demonitor caller and remove from tracking maps
    erlang:demonitor(MonitorRef, [flush]),
    PendingCalls = maps:remove(CallId, State#state.pending_calls),
    CallerMonitors = maps:remove(MonitorRef, State#state.caller_monitors),
    State2 = State#state{pending_calls = PendingCalls, caller_monitors = CallerMonitors},

    %% Extract failover context and retry
    do_call_failover(FailoverContext, From, State2).

%% @private Execute failover with updated excluded providers
%% Gateway-direct calls have simplified context (no providers/excluded/attempt)
do_call_failover(#{procedure := Procedure, args := _, opts := _} = Ctx, From, State)
  when not is_map_key(providers, Ctx) ->
    %% Gateway-direct call timed out - just return error (no failover available)
    ?LOG_WARNING("[~s] Gateway-direct RPC ~s timed out, no failover available",
                [State#state.node_id, Procedure]),
    gen_server:reply(From, {error, gateway_timeout}),
    {noreply, State};
%% Regular DHT-discovered calls have full failover context
do_call_failover(FailoverContext, From, State) ->
    #{
        procedure := Procedure,
        args := Args,
        opts := Opts,
        providers := Providers,
        excluded := ExcludedProviders,
        attempt := Attempt
    } = FailoverContext,

    %% Add current provider to excluded list (it timed out)
    TriedNodeId = maps:get(tried_node_id, FailoverContext, undefined),
    ExcludedProviders2 = add_to_excluded(TriedNodeId, ExcludedProviders),

    %% Retry with failover
    do_remote_call_with_failover(Procedure, Args, Opts, From, Providers,
                                ExcludedProviders2, Attempt + 1, State).

%% @private Add node to excluded list (if defined)
add_to_excluded(undefined, ExcludedProviders) -> ExcludedProviders;
add_to_excluded(NodeId, ExcludedProviders) -> [NodeId | ExcludedProviders].

%% @private Query not found (already completed)
do_find_value_timeout(undefined, _ServiceKey, State) ->
    {noreply, State};
%% @private Query timed out
do_find_value_timeout({From, Procedure, _Args, _Opts, _Registry, _Timer, MonitorRef}, ServiceKey, State) ->
    ?LOG_WARNING("[~s] FIND_VALUE timeout for service ~s",
                [State#state.node_id, Procedure]),
    %% Demonitor caller and remove from tracking maps
    erlang:demonitor(MonitorRef, [flush]),
    gen_server:reply(From, {error, dht_timeout}),
    PendingQueries = maps:remove(ServiceKey, State#state.pending_queries),
    CallerMonitors = maps:remove(MonitorRef, State#state.caller_monitors),
    {noreply, State#state{pending_queries = PendingQueries, caller_monitors = CallerMonitors}}.

%% @private Monitor not found (already cleaned up)
do_caller_down(undefined, _MonitorRef, State) ->
    {noreply, State};
%% @private Clean up pending RPC call
do_caller_down({call, CallId}, MonitorRef, State) ->
    PendingInfo = maps:get(CallId, State#state.pending_calls, undefined),
    do_cleanup_pending_call(PendingInfo, CallId, MonitorRef, State);
%% @private Clean up pending DHT query
do_caller_down({query, ServiceKey}, MonitorRef, State) ->
    PendingInfo = maps:get(ServiceKey, State#state.pending_queries, undefined),
    do_cleanup_pending_query(PendingInfo, ServiceKey, MonitorRef, State).

%% @private Call already removed
do_cleanup_pending_call(undefined, _CallId, MonitorRef, State) ->
    CallerMonitors = maps:remove(MonitorRef, State#state.caller_monitors),
    {noreply, State#state{caller_monitors = CallerMonitors}};
%% @private Cancel timer and remove call
do_cleanup_pending_call({_From, Timer, MonitorRef, _FailoverContext}, CallId, MonitorRef, State) ->
    ?LOG_DEBUG("[~s] Caller died for RPC call ~s, cleaning up",
              [State#state.node_id, binary:encode_hex(CallId)]),
    erlang:cancel_timer(Timer),
    PendingCalls = maps:remove(CallId, State#state.pending_calls),
    CallerMonitors = maps:remove(MonitorRef, State#state.caller_monitors),
    {noreply, State#state{pending_calls = PendingCalls, caller_monitors = CallerMonitors}}.

%% @private Query already removed
do_cleanup_pending_query(undefined, _ServiceKey, MonitorRef, State) ->
    CallerMonitors = maps:remove(MonitorRef, State#state.caller_monitors),
    {noreply, State#state{caller_monitors = CallerMonitors}};
%% @private Cancel timer and remove query
do_cleanup_pending_query({_From, Procedure, _Args, _Opts, _Registry, Timer, MonitorRef}, ServiceKey, MonitorRef, State) ->
    ?LOG_DEBUG("[~s] Caller died for DHT query ~s, cleaning up",
              [State#state.node_id, Procedure]),
    erlang:cancel_timer(Timer),
    PendingQueries = maps:remove(ServiceKey, State#state.pending_queries),
    CallerMonitors = maps:remove(MonitorRef, State#state.caller_monitors),
    {noreply, State#state{pending_queries = PendingQueries, caller_monitors = CallerMonitors}}.

%% @doc Send FIND_VALUE query to DHT asynchronously
-spec send_find_value_async(binary(), binary(), term(), map(), term(), #state{}) ->
    {noreply, #state{}}.
send_find_value_async(ServiceKey, Procedure, Args, Opts, From, State) ->
    %% Create FIND_VALUE message (let it crash on encoding errors)
    FindValueMsg = macula_routing_protocol:encode_find_value(ServiceKey),

    %% Send over QUIC via connection manager
    ConnMgrPid = State#state.connection_manager_pid,
    case macula_connection:send_message(ConnMgrPid, find_value, FindValueMsg) of
        ok ->
            %% Start timeout timer
            Timer = erlang:send_after(?DHT_QUERY_TIMEOUT, self(),
                                    {find_value_timeout, ServiceKey}),

            %% Monitor caller process for automatic cleanup
            {CallerPid, _Tag} = From,
            MonitorRef = erlang:monitor(process, CallerPid),

            %% Store pending query with RPC context
            QueryContext = {From, Procedure, Args, Opts, State#state.service_registry, Timer, MonitorRef},
            PendingQueries = maps:put(ServiceKey, QueryContext, State#state.pending_queries),

            %% Track monitor
            CallerMonitors = maps:put(MonitorRef, {query, ServiceKey}, State#state.caller_monitors),

            ?LOG_DEBUG("[~s] FIND_VALUE sent for service ~s (async, tracking reply)",
                      [State#state.node_id, Procedure]),
            {noreply, State#state{pending_queries = PendingQueries, caller_monitors = CallerMonitors}};

        {error, Reason} ->
            %% Network error - reply to caller immediately
            ?LOG_WARNING("[~s] Failed to send FIND_VALUE for service ~s: ~p",
                        [State#state.node_id, Procedure, Reason]),
            gen_server:reply(From, {error, Reason}),
            {noreply, State}
    end.

%% @doc Handle DHT providers found - complete pending RPC calls (sync and async)
-spec handle_dht_providers_found(list(), #state{}) -> {noreply, #state{}}.
handle_dht_providers_found(Providers, State) ->
    %% Try sync queries first, then async discoveries
    case maps:to_list(State#state.pending_queries) of
        [] ->
            %% No sync queries - check for async discoveries
            handle_async_dht_providers_found(Providers, State);
        [{ServiceKey, {From, Procedure, Args, Opts, _Registry, Timer, MonitorRef}} | _Rest] ->
            %% Cancel timeout timer
            erlang:cancel_timer(Timer),

            %% Remove monitor
            erlang:demonitor(MonitorRef, [flush]),

            %% Remove from pending
            PendingQueries = maps:remove(ServiceKey, State#state.pending_queries),
            CallerMonitors = maps:remove(MonitorRef, State#state.caller_monitors),

            State2 = State#state{
                pending_queries = PendingQueries,
                caller_monitors = CallerMonitors
            },

            ?LOG_INFO("[~s] Completing sync RPC call for ~s with ~p providers",
                     [State#state.node_id, Procedure, length(Providers)]),

            %% Continue with the actual RPC call using discovered providers
            do_remote_call(Procedure, Args, Opts, From, Providers, State2)
    end.

%% @doc Handle DHT providers found for async discoveries
-spec handle_async_dht_providers_found(list(), #state{}) -> {noreply, #state{}}.
handle_async_dht_providers_found(Providers, State) ->
    case maps:to_list(State#state.pending_async_discoveries) of
        [] ->
            ?LOG_WARNING("[~s] Received providers but no pending queries or discoveries", [State#state.node_id]),
            {noreply, State};
        [{ServiceKey, PendingDiscovery} | _Rest] ->
            %% Cancel timeout timer
            erlang:cancel_timer(PendingDiscovery#pending_async_discovery.timer_ref),

            %% Remove from pending
            PendingDiscoveries = maps:remove(ServiceKey, State#state.pending_async_discoveries),
            State2 = State#state{pending_async_discoveries = PendingDiscoveries},

            Procedure = PendingDiscovery#pending_async_discovery.procedure,
            QueuedRequests = PendingDiscovery#pending_async_discovery.queued_requests,

            ?LOG_INFO("[~s] Completing ~p async RPC requests for ~s with ~p providers",
                     [State#state.node_id, length(QueuedRequests), Procedure, length(Providers)]),

            %% Process all queued requests
            dispatch_queued_async_requests(Providers, QueuedRequests, State2)
    end.

%% @doc Dispatch all queued async requests with discovered providers
-spec dispatch_queued_async_requests(list(), list(), #state{}) -> {noreply, #state{}}.
dispatch_queued_async_requests([], QueuedRequests, State) ->
    %% No providers - notify all callbacks with error
    lists:foreach(fun({RequestId, _Args, _Opts, Callback}) ->
        invoke_async_callback(Callback, RequestId, {error, no_providers})
    end, QueuedRequests),
    {noreply, State};
dispatch_queued_async_requests(Providers, QueuedRequests, State) ->
    %% Select best provider
    [Provider | _] = Providers,
    #{node_id := TargetNodeId} = Provider,

    %% Dispatch each queued request
    State2 = lists:foldl(fun({RequestId, Args, Opts, Callback}, AccState) ->
        dispatch_single_async_request(TargetNodeId, RequestId, Args, Opts, Callback, AccState)
    end, State, QueuedRequests),

    {noreply, State2}.

%% @doc Dispatch a single async request to the discovered provider
-spec dispatch_single_async_request(binary(), binary(), term(), map(),
                                    {fun_cb, fun()} | {pid_cb, pid()}, #state{}) -> #state{}.
dispatch_single_async_request(TargetNodeId, RequestId, Args, Opts, Callback, State) ->
    %% Encode args
    EncodedArgs = encode_call_args(Args),

    %% Build RPC_REQUEST message (NATS-style)
    LocalNodeId = State#state.node_id,
    RequestMsg = #{
        type => <<"rpc_request">>,
        request_id => RequestId,
        procedure => maps:get(procedure, Opts, <<"unknown">>),
        args => EncodedArgs,
        from_node => LocalNodeId,
        realm => State#state.realm,
        timestamp => erlang:system_time(millisecond)
    },

    %% Send directly to target node via peer connector (NAT-aware)
    %% Pass endpoint option if provided for direct connection (skips NAT lookup)
    NatOpts = maps:with([endpoint], Opts),
    case macula_peer_connector:send_message_nat_aware(LocalNodeId, TargetNodeId, rpc_request, RequestMsg, NatOpts) of
        ok ->
            %% Set timeout
            Timeout = maps:get(timeout, Opts, 5000),
            TimerRef = erlang:send_after(Timeout, self(), {request_timeout, RequestId}),

            %% Track pending request
            PendingReq = #pending_request{
                id = RequestId,
                procedure = maps:get(procedure, Opts, <<"unknown">>),
                callback = Callback,
                timer_ref = TimerRef,
                sent_at = erlang:system_time(millisecond),
                target_node = TargetNodeId
            },

            PendingRequests = maps:put(RequestId, PendingReq, State#state.pending_requests),

            ?LOG_DEBUG("[~s] Dispatched async RPC to ~s (request_id: ~s)",
                      [State#state.node_id, binary:encode_hex(TargetNodeId),
                       binary:encode_hex(RequestId)]),

            State#state{pending_requests = PendingRequests};

        {error, Reason} ->
            %% Send failed - notify callback immediately
            ?LOG_WARNING("[~s] Async RPC dispatch failed: ~p", [State#state.node_id, Reason]),
            invoke_async_callback(Callback, RequestId, {error, {send_failed, Reason}}),
            State
    end.

%% @doc Make RPC call with failover support (entry point)
-spec do_remote_call(binary(), term(), map(), term(), list(), #state{}) ->
    {noreply, #state{}} | {reply, {error, term()}, #state{}}.
do_remote_call(Procedure, Args, Opts, From, Providers, State) ->
    do_remote_call_with_failover(Procedure, Args, Opts, From, Providers, [], 1, State).

%% @doc Make RPC call with failover support
-spec do_remote_call_with_failover(binary(), term(), map(), term(), list(), list(), pos_integer(), #state{}) ->
    {noreply, #state{}} | {reply, {error, term()}, #state{}}.
do_remote_call_with_failover(Procedure, Args, Opts, From, Providers, ExcludedProviders, Attempt, State) ->
    %% Get max attempts from options (default: 3 or number of providers, whichever is smaller)
    MaxAttempts = maps:get(max_attempts, Opts, min(3, length(Providers))),

    %% Filter out excluded providers
    AvailableProviders = filter_excluded_providers(Providers, ExcludedProviders),

    %% Check if we have any providers left
    case AvailableProviders of
        [] ->
            ?LOG_ERROR("[~s] All providers exhausted for service ~s after ~p attempts",
                      [State#state.node_id, Procedure, Attempt - 1]),
            {reply, {error, all_providers_failed}, State};

        _ when Attempt > MaxAttempts ->
            ?LOG_ERROR("[~s] Max attempts (~p) reached for service ~s",
                      [State#state.node_id, MaxAttempts, Procedure]),
            {reply, {error, max_attempts_exceeded}, State};

        _ ->
            %% Use provider selection strategy to pick a provider
            SelectorState = State#state.provider_selector_state,
            Strategy = maps:get(strategy, SelectorState, random),

            %% Set the current service ID for round-robin tracking
            SelectorState2 = SelectorState#{current_service_id => Procedure},

            case macula_provider_selector:select_provider(AvailableProviders, Strategy, SelectorState2) of
                {ok, Provider, SelectorState3} ->
                    #{node_id := NodeId} = Provider,

                    AttemptLog = format_attempt_log(Attempt, MaxAttempts),

                    ?LOG_INFO("[~s] Calling service ~s via DHT routing to provider ~s~s (strategy: ~p, ~p providers available)",
                             [State#state.node_id, Procedure, binary:encode_hex(NodeId),
                              AttemptLog, Strategy, length(AvailableProviders)]),

                    %% Update state with new selector state
                    State2 = State#state{provider_selector_state = SelectorState3},

                    %% Attempt the call and store failover context
                    do_remote_call_to_provider(Procedure, Args, Opts, From, Provider,
                                              Providers, ExcludedProviders, Attempt, State2);

                {error, no_providers} ->
                    ?LOG_ERROR("[~s] No providers available for service ~s",
                              [State#state.node_id, Procedure]),
                    {reply, {error, no_providers}, State}
            end
    end.

%% @doc Make RPC call to a specific provider
-spec do_remote_call_to_provider(binary(), term(), map(), term(), map(), list(), list(), pos_integer(), #state{}) ->
    {noreply, #state{}}.
do_remote_call_to_provider(Procedure, Args, Opts, From, Provider, AllProviders, ExcludedProviders, Attempt, State) ->
    %% Generate call ID
    {CallId, State2} = next_message_id(State),

    %% Encode args
    EncodedArgs = encode_call_args(Args),

    %% Build call message (RPC call)
    #{node_id := ProviderNodeId} = Provider,

    CallMsg = #{
        call_id => CallId,
        procedure => Procedure,
        args => EncodedArgs,
        realm => State#state.realm
    },

    %% Wrap call in DHT routing envelope
    LocalNodeId = State#state.node_id,
    RpcRouteMsg = macula_rpc_routing:wrap_call(LocalNodeId, ProviderNodeId, CallMsg, 10),

    %% Send DHT-routed call message via connection manager
    ConnMgrPid = State#state.connection_manager_pid,
    case macula_connection:send_message(ConnMgrPid, rpc_route, RpcRouteMsg) of
        ok ->
            %% Set up timeout timer
            Timeout = maps:get(timeout, Opts, ?DEFAULT_CALL_TIMEOUT),
            Timer = erlang:send_after(Timeout, self(), {call_timeout, CallId}),

            %% Monitor caller process for automatic cleanup
            {CallerPid, _Tag} = From,
            MonitorRef = erlang:monitor(process, CallerPid),

            %% Store failover context for retries
            FailoverContext = #{
                procedure => Procedure,
                args => Args,
                opts => Opts,
                providers => AllProviders,
                excluded => ExcludedProviders,
                attempt => Attempt,
                tried_node_id => ProviderNodeId
            },

            %% Store pending call
            PendingCalls = State2#state.pending_calls,
            PendingCalls2 = PendingCalls#{CallId => {From, Timer, MonitorRef, FailoverContext}},

            %% Track monitor
            CallerMonitors = maps:put(MonitorRef, {call, CallId}, State2#state.caller_monitors),

            ?LOG_DEBUG("[~s] RPC call sent: call_id=~s, procedure=~s, provider=~s",
                      [State2#state.node_id, binary:encode_hex(CallId), Procedure,
                       binary:encode_hex(ProviderNodeId)]),

            {noreply, State2#state{pending_calls = PendingCalls2, caller_monitors = CallerMonitors}};

        {error, SendError} ->
            ?LOG_ERROR("[~s] Failed to send RPC call for ~s: ~p",
                      [State2#state.node_id, Procedure, SendError]),
            {reply, {error, {send_failed, SendError}}, State2}
    end.

%%%===================================================================
%%% Internal functions - Reply Handling
%%%===================================================================

%% @doc Handle incoming RPC reply
-spec handle_rpc_reply(binary(), map(), #state{}) -> {noreply, #state{}}.
handle_rpc_reply(CallId, Msg, State) ->
    case maps:get(CallId, State#state.pending_calls, undefined) of
        undefined ->
            ?LOG_WARNING("[~s] Received REPLY for unknown call_id: ~p",
                        [State#state.node_id, binary:encode_hex(CallId)]),
            {noreply, State};

        {From, Timer, MonitorRef, _FailoverContext} ->
            %% Reply with failover context - no need to failover on success
            erlang:cancel_timer(Timer),
            erlang:demonitor(MonitorRef, [flush]),
            send_reply_to_caller(From, Msg, State),
            PendingCalls = maps:remove(CallId, State#state.pending_calls),
            CallerMonitors = maps:remove(MonitorRef, State#state.caller_monitors),
            {noreply, State#state{pending_calls = PendingCalls, caller_monitors = CallerMonitors}}
    end.

%% @doc Send reply to the original caller
-spec send_reply_to_caller(term(), map(), #state{}) -> ok.
send_reply_to_caller(From, Msg, State) ->
    case maps:get(<<"result">>, Msg, maps:get(result, Msg, undefined)) of
        undefined ->
            %% Error response
            Error = maps:get(<<"error">>, Msg, maps:get(error, Msg, <<"Unknown error">>)),
            ?LOG_DEBUG("[~s] RPC error response: ~p", [State#state.node_id, Error]),
            gen_server:reply(From, {error, Error});
        Result ->
            %% Success response
            DecodedResult = try
                decode_json(Result)
            catch
                _:_ -> Result
            end,
            ?LOG_DEBUG("[~s] RPC success response received", [State#state.node_id]),
            gen_server:reply(From, {ok, DecodedResult})
    end.

%%%===================================================================
%%% Internal functions - Async RPC (NATS-style)
%%%===================================================================

%% @private Handle async request with DHT discovery
-spec do_async_request(binary(), term(), map(), term(), #state{}) ->
    {reply, {ok, binary()} | {error, term()}, #state{}} | {noreply, #state{}}.
do_async_request(Procedure, Args, Opts, From, State) ->
    Registry = State#state.service_registry,

    %% Check local handler first
    case macula_service_registry:get_local_handler(Registry, Procedure) of
        {ok, Handler} ->
            %% Local handler - execute directly with async response
            do_async_request_local(Procedure, Args, Opts, From, Handler, State);
        not_found ->
            %% Need DHT discovery
            do_async_request_discover(Procedure, Args, Opts, From, State)
    end.

%% @private Execute local handler asynchronously
do_async_request_local(Procedure, Args, Opts, {CallerPid, _}, Handler, State) ->
    {RequestId, State2} = next_message_id(State),
    Callback = get_async_callback(Opts, CallerPid),

    %% Execute handler in spawned process and invoke callback
    spawn(fun() ->
        Result = try
            Handler(Args)
        catch
            _:Error -> {error, Error}
        end,
        invoke_async_callback(Callback, RequestId, Result)
    end),

    ?LOG_DEBUG("[~s] Async RPC ~s executed locally (request_id: ~s)",
              [State2#state.node_id, Procedure, binary:encode_hex(RequestId)]),
    {reply, {ok, RequestId}, State2}.

%% @private Discover providers via DHT and send request
do_async_request_discover(Procedure, Args, Opts, From, State) ->
    Registry = State#state.service_registry,
    case macula_service_registry:discover_service(Registry, Procedure) of
        {ok, Providers, Registry2} when Providers =/= [] ->
            State2 = State#state{service_registry = Registry2},
            %% Select best provider and send request
            [Provider | _] = Providers,
            #{node_id := TargetNodeId} = Provider,
            do_async_request_to(TargetNodeId, Procedure, Args, Opts, From, State2);
        {ok, [], Registry2} ->
            State2 = State#state{service_registry = Registry2},
            {reply, {error, no_providers}, State2};
        {cache_miss, Registry2} ->
            %% Initiate async DHT discovery and queue the request
            State2 = State#state{service_registry = Registry2},
            do_async_dht_discovery(Procedure, Args, Opts, From, State2)
    end.

%% @private Initiate async DHT discovery for a procedure
%% Queue the request and start DHT lookup if not already in progress
-spec do_async_dht_discovery(binary(), term(), map(), term(), #state{}) ->
    {reply, {ok, binary()} | {error, term()}, #state{}} | {noreply, #state{}}.
do_async_dht_discovery(Procedure, Args, Opts, {CallerPid, _} = _From, State) ->
    ServiceKey = crypto:hash(sha256, Procedure),

    %% Generate request ID now (we return it immediately)
    {RequestId, State2} = next_message_id(State),
    Callback = get_async_callback(Opts, CallerPid),

    %% Store procedure in opts for later use when dispatching
    Opts2 = Opts#{procedure => Procedure},

    %% Check if DHT discovery is already in progress for this service
    case maps:get(ServiceKey, State2#state.pending_async_discoveries, undefined) of
        undefined ->
            %% No discovery in progress - start one
            start_async_dht_discovery(ServiceKey, Procedure, RequestId, Args, Opts2, Callback, State2);
        PendingDiscovery ->
            %% Discovery already in progress - queue this request
            queue_async_request(ServiceKey, PendingDiscovery, RequestId, Args, Opts2, Callback, State2)
    end.

%% @private Start a new async DHT discovery
-spec start_async_dht_discovery(binary(), binary(), binary(), term(), map(),
                                 {fun_cb, fun()} | {pid_cb, pid()}, #state{}) ->
    {reply, {ok, binary()} | {error, term()}, #state{}} | {noreply, #state{}}.
start_async_dht_discovery(ServiceKey, Procedure, RequestId, Args, Opts, Callback, State) ->
    %% Create FIND_VALUE message
    FindValueMsg = macula_routing_protocol:encode_find_value(ServiceKey),

    %% Send over QUIC via connection manager
    ConnMgrPid = State#state.connection_manager_pid,
    case macula_connection:send_message(ConnMgrPid, find_value, FindValueMsg) of
        ok ->
            %% Start timeout timer for DHT discovery
            Timeout = maps:get(dht_timeout, Opts, ?DHT_QUERY_TIMEOUT),
            TimerRef = erlang:send_after(Timeout, self(), {async_dht_discovery_timeout, ServiceKey}),

            %% Create pending discovery record with first queued request
            QueuedRequest = {RequestId, Args, Opts, Callback},
            PendingDiscovery = #pending_async_discovery{
                service_key = ServiceKey,
                procedure = Procedure,
                queued_requests = [QueuedRequest],
                timer_ref = TimerRef,
                started_at = erlang:system_time(millisecond)
            },

            %% Store pending discovery
            PendingDiscoveries = maps:put(ServiceKey, PendingDiscovery,
                                          State#state.pending_async_discoveries),

            ?LOG_DEBUG("[~s] Started async DHT discovery for ~s (request_id: ~s)",
                      [State#state.node_id, Procedure, binary:encode_hex(RequestId)]),

            {reply, {ok, RequestId}, State#state{pending_async_discoveries = PendingDiscoveries}};

        {error, Reason} ->
            ?LOG_WARNING("[~s] Failed to start async DHT discovery for ~s: ~p",
                        [State#state.node_id, Procedure, Reason]),
            {reply, {error, {dht_discovery_failed, Reason}}, State}
    end.

%% @private Queue a request to an existing DHT discovery
-spec queue_async_request(binary(), #pending_async_discovery{}, binary(), term(), map(),
                          {fun_cb, fun()} | {pid_cb, pid()}, #state{}) ->
    {reply, {ok, binary()}, #state{}}.
queue_async_request(ServiceKey, PendingDiscovery, RequestId, Args, Opts, Callback, State) ->
    %% Add to queued requests
    QueuedRequest = {RequestId, Args, Opts, Callback},
    QueuedRequests = [QueuedRequest | PendingDiscovery#pending_async_discovery.queued_requests],

    %% Update pending discovery
    PendingDiscovery2 = PendingDiscovery#pending_async_discovery{queued_requests = QueuedRequests},
    PendingDiscoveries = maps:put(ServiceKey, PendingDiscovery2,
                                  State#state.pending_async_discoveries),

    ?LOG_DEBUG("[~s] Queued async request ~s for ~s (queue size: ~p)",
              [State#state.node_id, binary:encode_hex(RequestId),
               PendingDiscovery#pending_async_discovery.procedure, length(QueuedRequests)]),

    {reply, {ok, RequestId}, State#state{pending_async_discoveries = PendingDiscoveries}}.

%% @private Send async request to specific target node (direct P2P)
%% CRITICAL: Store pending request BEFORE sending to avoid race condition where
%% reply arrives before we're ready to receive it (discovered 2025-11-30).
-spec do_async_request_to(binary(), binary(), term(), map(), term(), #state{}) ->
    {reply, {ok, binary()} | {error, term()}, #state{}}.
do_async_request_to(TargetNodeId, Procedure, Args, Opts, {CallerPid, _}, State) ->
    %% Generate request ID
    {RequestId, State2} = next_message_id(State),
    T0 = erlang:system_time(millisecond),
    ReqIdHex = binary:encode_hex(RequestId),

    %% Extract callback (fun or caller pid)
    Callback = get_async_callback(Opts, CallerPid),

    %% Set timeout and track pending request BEFORE sending
    %% This prevents race condition where reply arrives before pending is stored
    Timeout = maps:get(timeout, Opts, 5000),
    TimerRef = erlang:send_after(Timeout, self(), {request_timeout, RequestId}),

    PendingReq = #pending_request{
        id = RequestId,
        procedure = Procedure,
        callback = Callback,
        timer_ref = TimerRef,
        sent_at = T0,
        target_node = TargetNodeId
    },
    PendingRequests = maps:put(RequestId, PendingReq, State2#state.pending_requests),
    State3 = State2#state{pending_requests = PendingRequests},

    ?LOG_WARNING("[TIMING ~s] T0=~p: REQUEST_CREATED+STORED target=~s",
                [ReqIdHex, T0, binary:encode_hex(TargetNodeId)]),

    %% Encode args
    EncodedArgs = encode_call_args(Args),

    %% Build RPC_REQUEST message (NATS-style)
    LocalNodeId = State3#state.node_id,
    LocalEndpoint = get_local_endpoint(),
    RequestMsg = #{
        type => <<"rpc_request">>,
        request_id => RequestId,
        procedure => Procedure,
        args => EncodedArgs,
        from_node => LocalNodeId,
        from_endpoint => LocalEndpoint,
        target_node => TargetNodeId,
        realm => State3#state.realm,
        timestamp => T0
    },

    %% Try direct P2P first, then fall back to gateway relay
    NatOpts = maps:with([endpoint], Opts),
    SendResult = case macula_peer_connector:send_message_nat_aware(LocalNodeId, TargetNodeId, rpc_request, RequestMsg, NatOpts) of
        ok -> ok;
        {error, DirectError} ->
            %% Direct failed - fall back to gateway routing
            ?LOG_DEBUG("[~s] Direct P2P failed (~p), trying gateway relay", [LocalNodeId, DirectError]),
            send_via_gateway(rpc_request, RequestMsg, State3)
    end,
    T1 = erlang:system_time(millisecond),
    ?LOG_WARNING("[TIMING ~s] T1=~p: SEND_COMPLETE (took ~pms)", [ReqIdHex, T1, T1-T0]),
    case SendResult of
        ok ->
            ?LOG_DEBUG("[~s] Async RPC ~s sent to ~s (request_id: ~s)",
                      [State3#state.node_id, Procedure,
                       binary:encode_hex(TargetNodeId), binary:encode_hex(RequestId)]),

            {reply, {ok, RequestId}, State3};

        {error, Reason} ->
            %% Send failed - clean up pending request
            erlang:cancel_timer(TimerRef),
            CleanedPending = maps:remove(RequestId, State3#state.pending_requests),
            ?LOG_WARNING("[~s] Async RPC ~s send failed: ~p",
                        [State3#state.node_id, Procedure, Reason]),
            {reply, {error, {send_failed, Reason}}, State3#state{pending_requests = CleanedPending}}
    end.

%% @private Handle incoming async RPC reply
-spec do_async_rpc_reply(binary() | undefined, map(), #state{}) -> {noreply, #state{}}.
do_async_rpc_reply(undefined, Msg, State) ->
    ?LOG_WARNING("[~s] ASYNC_REPLY: missing request_id, msg=~p", [State#state.node_id, Msg]),
    {noreply, State};
do_async_rpc_reply(RequestId, Msg, State) ->
    RequestIdHex = binary:encode_hex(RequestId),
    T_recv = erlang:system_time(millisecond),
    %% Extract original timestamp from message
    T_sent = maps:get(<<"timestamp">>, Msg, maps:get(timestamp, Msg, 0)),
    ?LOG_WARNING("[TIMING ~s] T_REPLY_RECV=~p: received (msg_timestamp=~p, total_transit=~pms)",
                [RequestIdHex, T_recv, T_sent, T_recv - T_sent]),
    ?LOG_WARNING("[~s] ASYNC_REPLY: received request_id=~s, pending_count=~p",
                [State#state.node_id, RequestIdHex, maps:size(State#state.pending_requests)]),
    case maps:take(RequestId, State#state.pending_requests) of
        {PendingReq, NewPendingRequests} ->
            ?LOG_WARNING("[~s] ASYNC_REPLY: FOUND pending request for ~s, procedure=~s",
                        [State#state.node_id, RequestIdHex, PendingReq#pending_request.procedure]),
            %% Cancel timeout timer
            erlang:cancel_timer(PendingReq#pending_request.timer_ref),

            %% Extract result from reply
            Result = case maps:get(<<"result">>, Msg, maps:get(result, Msg, undefined)) of
                undefined ->
                    Error = maps:get(<<"error">>, Msg, maps:get(error, Msg, <<"Unknown error">>)),
                    {error, Error};
                Value ->
                    %% Try to decode JSON result
                    DecodedValue = try decode_json(Value) catch _:_ -> Value end,
                    {ok, DecodedValue}
            end,

            ?LOG_WARNING("[~s] ASYNC_REPLY: invoking callback with result=~p", [State#state.node_id, Result]),
            %% Invoke callback
            invoke_async_callback(PendingReq#pending_request.callback, RequestId, Result),

            RTT = erlang:system_time(millisecond) - PendingReq#pending_request.sent_at,
            ?LOG_WARNING("[TIMING ~s] REPLY_SUCCESS: RTT=~pms", [RequestIdHex, RTT]),

            {noreply, State#state{pending_requests = NewPendingRequests}};

        error ->
            %% Already timed out or duplicate
            ?LOG_WARNING("[TIMING ~s] REPLY_LATE: arrived after timeout (transit=~pms)",
                        [RequestIdHex, T_recv - T_sent]),
            %% Log the pending request IDs for debugging
            PendingIds = [binary:encode_hex(K) || K <- maps:keys(State#state.pending_requests)],
            ?LOG_WARNING("[~s] ASYNC_REPLY: pending request_ids=~p", [State#state.node_id, PendingIds]),
            {noreply, State}
    end.

%% @private Handle async request timeout
-spec do_request_timeout(#pending_request{} | undefined, binary(), #state{}) -> {noreply, #state{}}.
do_request_timeout(undefined, _RequestId, State) ->
    %% Already received reply
    {noreply, State};
do_request_timeout(PendingReq, RequestId, State) ->
    RequestIdHex = binary:encode_hex(RequestId),
    T_timeout = erlang:system_time(millisecond),
    Elapsed = T_timeout - PendingReq#pending_request.sent_at,
    ?LOG_WARNING("[TIMING ~s] TIMEOUT_FIRED: T=~p, sent_at=~p, elapsed=~pms",
                [RequestIdHex, T_timeout, PendingReq#pending_request.sent_at, Elapsed]),

    %% Remove from pending
    NewPendingRequests = maps:remove(RequestId, State#state.pending_requests),

    %% Invoke callback with timeout error
    invoke_async_callback(PendingReq#pending_request.callback, RequestId, {error, timeout}),

    ?LOG_WARNING("[~s] Async RPC ~s timed out (request_id: ~s)",
                [State#state.node_id, PendingReq#pending_request.procedure,
                 RequestIdHex]),

    {noreply, State#state{pending_requests = NewPendingRequests}}.

%% @private Handle async DHT discovery timeout
-spec do_async_dht_discovery_timeout(#pending_async_discovery{} | undefined, binary(), #state{}) ->
    {noreply, #state{}}.
do_async_dht_discovery_timeout(undefined, _ServiceKey, State) ->
    %% Already completed
    {noreply, State};
do_async_dht_discovery_timeout(PendingDiscovery, ServiceKey, State) ->
    %% Remove from pending
    PendingDiscoveries = maps:remove(ServiceKey, State#state.pending_async_discoveries),
    State2 = State#state{pending_async_discoveries = PendingDiscoveries},

    %% Notify all queued requests with timeout error
    QueuedRequests = PendingDiscovery#pending_async_discovery.queued_requests,
    lists:foreach(fun({RequestId, _Args, _Opts, Callback}) ->
        invoke_async_callback(Callback, RequestId, {error, dht_discovery_timeout})
    end, QueuedRequests),

    ?LOG_WARNING("[~s] Async DHT discovery timed out for ~s (~p queued requests)",
                [State#state.node_id, PendingDiscovery#pending_async_discovery.procedure,
                 length(QueuedRequests)]),

    {noreply, State2}.

%% @private Get callback from opts or default to pid message
-spec get_async_callback(map(), pid()) -> {fun_cb, fun()} | {pid_cb, pid()}.
get_async_callback(Opts, CallerPid) ->
    case maps:get(callback, Opts, undefined) of
        undefined -> {pid_cb, CallerPid};
        Fun when is_function(Fun) -> {fun_cb, Fun}
    end.

%% @private Invoke async callback with result
-spec invoke_async_callback({fun_cb, fun()} | {pid_cb, pid()}, binary(), term()) -> ok.
invoke_async_callback({fun_cb, Fun}, _RequestId, Result) ->
    %% Spawn to avoid blocking the gen_server
    spawn(fun() -> Fun(Result) end),
    ok;
invoke_async_callback({pid_cb, Pid}, RequestId, Result) ->
    Pid ! {rpc_reply, RequestId, Result},
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

%% @doc Encode RPC call arguments to binary format.
%% Binary args pass through unchanged, other types are JSON-encoded.
-spec encode_call_args(term()) -> binary().
encode_call_args(Args) when is_binary(Args) ->
    Args;
encode_call_args(Args) when is_map(Args) ->
    encode_json(Args);
encode_call_args(Args) when is_list(Args) ->
    encode_json(Args);
encode_call_args(Args) ->
    encode_json(Args).

%% @doc Encode map/list to JSON binary
-spec encode_json(term()) -> binary().
encode_json(Data) ->
    macula_utils:encode_json(Data).

%% @doc Decode JSON binary to map/list
-spec decode_json(binary()) -> map() | list().
decode_json(Binary) ->
    macula_utils:decode_json(Binary).

%% @doc Filter out excluded providers from provider list.
%% Empty exclusion list returns providers unchanged.
-spec filter_excluded_providers(list(), list()) -> list().
filter_excluded_providers(Providers, []) ->
    Providers;
filter_excluded_providers(Providers, ExcludedProviders) ->
    lists:filter(fun(#{node_id := NodeId}) ->
        not lists:member(NodeId, ExcludedProviders)
    end, Providers).

%% @doc Format attempt number for logging.
%% First attempt returns empty string, subsequent attempts show attempt count.
-spec format_attempt_log(pos_integer(), pos_integer()) -> string().
format_attempt_log(1, _MaxAttempts) ->
    "";
format_attempt_log(Attempt, MaxAttempts) ->
    io_lib:format(" (attempt ~p/~p)", [Attempt, MaxAttempts]).

%% @doc Generate a random node ID
-spec generate_node_id() -> binary().
generate_node_id() ->
    macula_utils:generate_node_id().

%% @doc Get local endpoint from environment variables.
%% Used to include sender's endpoint in RPC requests so receivers can route replies back.
%% Format: "hostname:port" (e.g., "fc01:4433" in Docker)
-spec get_local_endpoint() -> binary().
get_local_endpoint() ->
    Hostname = get_hostname_for_endpoint(),
    Port = get_port_for_endpoint(),
    iolist_to_binary([Hostname, <<":">>, Port]).

%% @private Get hostname from environment variables.
-spec get_hostname_for_endpoint() -> binary().
get_hostname_for_endpoint() ->
    case os:getenv("MACULA_HOSTNAME") of
        false ->
            case os:getenv("HOSTNAME") of
                false -> <<"localhost">>;
                Hostname -> list_to_binary(Hostname)
            end;
        Hostname -> list_to_binary(Hostname)
    end.

%% @private Get QUIC port from environment or default.
-spec get_port_for_endpoint() -> binary().
get_port_for_endpoint() ->
    case os:getenv("QUIC_PORT") of
        false -> <<"4433">>;  %% Default QUIC port for Docker setup
        Port -> list_to_binary(Port)
    end.

%% @private Send message via gateway (relay fallback for cross-NAT).
%% Uses the existing connection to the gateway/bootstrap.
-spec send_via_gateway(atom(), map(), #state{}) -> ok | {error, term()}.
send_via_gateway(_MessageType, _Message, #state{connection_manager_pid = undefined}) ->
    ?LOG_WARNING("Cannot relay via gateway: connection_manager not set"),
    {error, connection_manager_not_set};
send_via_gateway(MessageType, Message, #state{connection_manager_pid = ConnMgrPid}) ->
    ?LOG_DEBUG("Relaying ~p message via gateway connection", [MessageType]),
    macula_connection:send_message(ConnMgrPid, MessageType, Message).

%%%===================================================================
%%% Internal functions - Pull-based Service Discovery
%%%===================================================================

%% @doc Normalize service interests to list of binaries.
%% Accepts various input formats: list of binaries/atoms/strings, or single value.
-spec normalize_service_interests(term()) -> [binary()].
normalize_service_interests(Interests) when is_list(Interests) ->
    lists:filtermap(fun normalize_single_interest/1, Interests);
normalize_service_interests(Interest) when is_binary(Interest) ->
    [Interest];
normalize_service_interests(Interest) when is_atom(Interest) ->
    [atom_to_binary(Interest, utf8)];
normalize_service_interests(Interest) when is_list(Interest) ->
    %% Could be a string
    case io_lib:printable_list(Interest) of
        true -> [list_to_binary(Interest)];
        false -> []
    end;
normalize_service_interests(_) ->
    [].

%% @doc Normalize a single service interest to binary.
%% Returns {true, Binary} for valid inputs, false for invalid.
-spec normalize_single_interest(term()) -> {true, binary()} | false.
normalize_single_interest(Interest) when is_binary(Interest), byte_size(Interest) > 0 ->
    {true, Interest};
normalize_single_interest(Interest) when is_atom(Interest), Interest =/= undefined ->
    {true, atom_to_binary(Interest, utf8)};
normalize_single_interest(Interest) when is_list(Interest), Interest =/= [] ->
    case io_lib:printable_list(Interest) of
        true -> {true, list_to_binary(Interest)};
        false -> false
    end;
normalize_single_interest(_) ->
    false.

%% @doc Log configured service interests during init
-spec log_service_interests([binary()]) -> ok.
log_service_interests([]) ->
    ok;
log_service_interests(Interests) ->
    ?LOG_INFO("RPC handler configured with ~p service interest(s) for pull-based discovery: ~p",
              [length(Interests), Interests]).

%% @doc Prefetch service providers for configured service interests.
%% Triggered when connection manager is set.
%% Uses fire-and-forget DHT lookups - results will be cached when they arrive.
-spec prefetch_service_interests(#state{}) -> #state{}.
prefetch_service_interests(#state{service_interests = []} = State) ->
    State;
prefetch_service_interests(#state{connection_manager_pid = undefined} = State) ->
    %% Connection manager not ready - can't prefetch yet
    State;
prefetch_service_interests(#state{service_interests = Interests} = State) ->
    ?LOG_INFO("[~s] Starting pull-based discovery for ~p service(s)",
              [State#state.node_id, length(Interests)]),
    %% Send DHT FIND_VALUE for each service interest
    lists:foldl(fun prefetch_single_service/2, State, Interests).

%% @doc Prefetch a single service via DHT lookup.
%% Uses fire-and-forget pattern - no callback, just populate cache.
-spec prefetch_single_service(binary(), #state{}) -> #state{}.
prefetch_single_service(ServiceName, State) ->
    ServiceKey = crypto:hash(sha256, ServiceName),

    %% Check if already in cache
    Registry = State#state.service_registry,
    case macula_service_registry:discover_service(Registry, ServiceName) of
        {ok, Providers, Registry2} when Providers =/= [] ->
            %% Already cached - no need to prefetch
            ?LOG_DEBUG("[~s] Service ~s already cached (~p providers)",
                      [State#state.node_id, ServiceName, length(Providers)]),
            State#state{service_registry = Registry2};
        {ok, [], Registry2} ->
            %% Empty cache entry - should refresh
            State2 = State#state{service_registry = Registry2},
            send_prefetch_find_value(ServiceKey, ServiceName, State2);
        {cache_miss, Registry2} ->
            %% Not in cache - send DHT query
            State2 = State#state{service_registry = Registry2},
            send_prefetch_find_value(ServiceKey, ServiceName, State2)
    end.

%% @doc Send FIND_VALUE for prefetch (fire-and-forget, no tracking).
%% Results will arrive via handle_cast(find_value_reply) and populate cache.
-spec send_prefetch_find_value(binary(), binary(), #state{}) -> #state{}.
send_prefetch_find_value(ServiceKey, ServiceName, State) ->
    %% Create FIND_VALUE message
    FindValueMsg = macula_routing_protocol:encode_find_value(ServiceKey),

    %% Send over QUIC via connection manager (fire-and-forget)
    ConnMgrPid = State#state.connection_manager_pid,
    case macula_connection:send_message(ConnMgrPid, find_value, FindValueMsg) of
        ok ->
            ?LOG_DEBUG("[~s] Prefetch FIND_VALUE sent for ~s",
                      [State#state.node_id, ServiceName]),
            State;
        {error, Reason} ->
            ?LOG_WARNING("[~s] Prefetch FIND_VALUE failed for ~s: ~p",
                        [State#state.node_id, ServiceName, Reason]),
            State
    end.
