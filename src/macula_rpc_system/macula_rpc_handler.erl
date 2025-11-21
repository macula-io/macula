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

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

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

    %% Caller monitors: #{MonitorRef => {call, CallId} | {query, QueryKey}}
    caller_monitors = #{} :: #{reference() => {atom(), binary()}},

    %% Message ID counter
    msg_id_counter = 0 :: non_neg_integer(),

    %% Service registry for DHT operations and local handler lookup
    service_registry :: macula_service_registry:registry(),

    %% Provider selection strategy state
    provider_selector_state = #{strategy => random} :: map()
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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    ?LOG_INFO("RPC handler starting"),

    %% Extract required options
    NodeId = maps:get(node_id, Opts, generate_node_id()),
    Realm = maps:get(realm, Opts, <<"default">>),

    %% connection_manager_pid will be set via cast message after init
    ConnMgrPid = undefined,

    %% Initialize service registry for DHT operations and local handlers
    Registry = macula_service_registry:new(),

    %% Provider selection strategy (random, round_robin, least_connections, etc.)
    Strategy = maps:get(provider_strategy, Opts, random),
    SelectorState = #{strategy => Strategy},

    {ok, #state{
        opts = Opts,
        node_id = NodeId,
        realm = Realm,
        connection_manager_pid = ConnMgrPid,
        service_registry = Registry,
        provider_selector_state = SelectorState
    }}.

%%%===================================================================
%%% RPC Call
%%%===================================================================

handle_call({call, Procedure, Args, Opts}, From, State) ->
    Registry = State#state.service_registry,
    BinaryProcedure = ensure_binary(Procedure),

    %% Check if this is a local service first
    case macula_service_registry:get_local_handler(Registry, BinaryProcedure) of
        {ok, Handler} ->
            %% Local service - execute directly without network call
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

        not_found ->
            %% Not local - try service discovery via DHT
            case macula_service_registry:discover_service(Registry, BinaryProcedure) of
                {ok, Providers, Registry2} when Providers =/= [] ->
                    %% Cache hit - we have providers
                    State2 = State#state{service_registry = Registry2},
                    do_remote_call(BinaryProcedure, Args, Opts, From, Providers, State2);

                {cache_miss, Registry2} ->
                    %% Cache miss - send FIND_VALUE to DHT node (async)
                    ServiceKey = crypto:hash(sha256, BinaryProcedure),
                    State2 = State#state{service_registry = Registry2},
                    send_find_value_async(ServiceKey, BinaryProcedure, Args, Opts, From, State2);

                {ok, [], Registry2} ->
                    %% No providers found in cache
                    State2 = State#state{service_registry = Registry2},
                    {reply, {error, service_not_found}, State2}
            end
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Set connection manager PID (sent by facade after init)
handle_cast({set_connection_manager_pid, Pid}, State) ->
    {noreply, State#state{connection_manager_pid = Pid}};

%% Register local procedure handler
handle_cast({register_local_procedure, Procedure, Handler}, State) ->
    Registry = State#state.service_registry,
    NewRegistry = macula_service_registry:advertise_local(Registry, Procedure, Handler, #{}),
    ?LOG_INFO("[~s] Registered local procedure: ~s", [State#state.node_id, Procedure]),
    {noreply, State#state{service_registry = NewRegistry}};

handle_cast({incoming_reply, Msg}, State) ->
    %% Handle RPC reply from network
    %% MessagePack decoder returns binary keys
    CallId = maps:get(<<"call_id">>, Msg, maps:get(call_id, Msg, undefined)),

    case CallId of
        undefined ->
            ?LOG_WARNING("[~s] Received REPLY without call_id", [State#state.node_id]),
            {noreply, State};
        _ ->
            handle_rpc_reply(CallId, Msg, State)
    end;

handle_cast({find_value_reply, Msg}, State) ->
    %% Handle FIND_VALUE_REPLY from DHT query
    ?LOG_INFO("[~s] Received FIND_VALUE_REPLY: ~p", [State#state.node_id, Msg]),

    %% Decode the reply to get value or nodes
    case macula_routing_protocol:decode_find_value_reply(Msg) of
        {ok, {value, Providers}} when is_list(Providers) ->
            ?LOG_INFO("[~s] DHT returned ~p provider(s)", [State#state.node_id, length(Providers)]),
            %% Find the pending query and complete the RPC call
            %% For now, we'll complete ALL pending queries with this result
            %% (In a real implementation, we'd match by service key)
            handle_dht_providers_found(Providers, State);
        {ok, {nodes, _Nodes}} ->
            ?LOG_DEBUG("[~s] DHT returned nodes (no value found)", [State#state.node_id]),
            {noreply, State};
        {error, Reason} ->
            ?LOG_WARNING("[~s] Failed to decode find_value_reply: ~p", [State#state.node_id, Reason]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%%%===================================================================
%%% Timeout Handlers
%%%===================================================================

handle_info({call_timeout, CallId}, State) ->
    %% RPC call timed out
    case maps:get(CallId, State#state.pending_calls, undefined) of
        undefined ->
            {noreply, State};

        {From, _Timer, MonitorRef, FailoverContext} ->
            %% Timeout with failover - try next provider
            ?LOG_WARNING("[~s] RPC call ~s timed out, attempting failover",
                        [State#state.node_id, binary:encode_hex(CallId)]),

            %% Demonitor caller and remove from tracking maps
            erlang:demonitor(MonitorRef, [flush]),
            PendingCalls = maps:remove(CallId, State#state.pending_calls),
            CallerMonitors = maps:remove(MonitorRef, State#state.caller_monitors),
            State2 = State#state{pending_calls = PendingCalls, caller_monitors = CallerMonitors},

            %% Extract failover context
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
            ExcludedProviders2 = case TriedNodeId of
                undefined -> ExcludedProviders;
                NodeId -> [NodeId | ExcludedProviders]
            end,

            %% Retry with failover
            do_remote_call_with_failover(Procedure, Args, Opts, From, Providers,
                                        ExcludedProviders2, Attempt + 1, State2)
    end;

handle_info({find_value_timeout, ServiceKey}, State) ->
    %% DHT query timed out
    case maps:get(ServiceKey, State#state.pending_queries, undefined) of
        undefined ->
            {noreply, State};
        {From, Procedure, _Args, _Opts, _Registry, _Timer, MonitorRef} ->
            ?LOG_WARNING("[~s] FIND_VALUE timeout for service ~s",
                        [State#state.node_id, Procedure]),
            %% Demonitor caller and remove from tracking maps
            erlang:demonitor(MonitorRef, [flush]),
            gen_server:reply(From, {error, dht_timeout}),
            PendingQueries = maps:remove(ServiceKey, State#state.pending_queries),
            CallerMonitors = maps:remove(MonitorRef, State#state.caller_monitors),
            {noreply, State#state{pending_queries = PendingQueries, caller_monitors = CallerMonitors}}
    end;

handle_info({'DOWN', MonitorRef, process, _Pid, _Reason}, State) ->
    %% Caller process died - clean up immediately
    case maps:get(MonitorRef, State#state.caller_monitors, undefined) of
        undefined ->
            %% Already cleaned up
            {noreply, State};

        {call, CallId} ->
            %% Clean up pending RPC call
            case maps:get(CallId, State#state.pending_calls, undefined) of
                undefined ->
                    %% Already removed
                    CallerMonitors = maps:remove(MonitorRef, State#state.caller_monitors),
                    {noreply, State#state{caller_monitors = CallerMonitors}};
                {_From, Timer, MonitorRef, _FailoverContext} ->
                    ?LOG_DEBUG("[~s] Caller died for RPC call ~s, cleaning up",
                              [State#state.node_id, binary:encode_hex(CallId)]),
                    %% Cancel timer and remove from both maps
                    erlang:cancel_timer(Timer),
                    PendingCalls = maps:remove(CallId, State#state.pending_calls),
                    CallerMonitors = maps:remove(MonitorRef, State#state.caller_monitors),
                    {noreply, State#state{pending_calls = PendingCalls, caller_monitors = CallerMonitors}}
            end;

        {query, ServiceKey} ->
            %% Clean up pending DHT query
            case maps:get(ServiceKey, State#state.pending_queries, undefined) of
                undefined ->
                    %% Already removed
                    CallerMonitors = maps:remove(MonitorRef, State#state.caller_monitors),
                    {noreply, State#state{caller_monitors = CallerMonitors}};
                {_From, Procedure, _Args, _Opts, _Registry, Timer, MonitorRef} ->
                    ?LOG_DEBUG("[~s] Caller died for DHT query ~s, cleaning up",
                              [State#state.node_id, Procedure]),
                    %% Cancel timer and remove from both maps
                    erlang:cancel_timer(Timer),
                    PendingQueries = maps:remove(ServiceKey, State#state.pending_queries),
                    CallerMonitors = maps:remove(MonitorRef, State#state.caller_monitors),
                    {noreply, State#state{pending_queries = PendingQueries, caller_monitors = CallerMonitors}}
            end
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?LOG_INFO("RPC handler terminating"),
    ok.

%%%===================================================================
%%% Internal functions - RPC Call Logic
%%%===================================================================

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

%% @doc Handle DHT providers found - complete pending RPC calls
-spec handle_dht_providers_found(list(), #state{}) -> {noreply, #state{}}.
handle_dht_providers_found(Providers, State) ->
    %% Get the first pending query (simplified - in real impl would match by key)
    case maps:to_list(State#state.pending_queries) of
        [] ->
            ?LOG_WARNING("[~s] Received providers but no pending queries", [State#state.node_id]),
            {noreply, State};
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

            ?LOG_INFO("[~s] Completing RPC call for ~s with ~p providers",
                     [State#state.node_id, Procedure, length(Providers)]),

            %% Continue with the actual RPC call using discovered providers
            do_remote_call(Procedure, Args, Opts, From, Providers, State2)
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
