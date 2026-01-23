%% @doc Local client for in-VM workloads to connect to macula_gateway
%%
%% This module provides process-to-process communication between workloads
%% running in the same BEAM VM as the Macula platform and the local gateway.
%% Unlike macula_peer which creates QUIC connections, this connects directly
%% to the local macula_gateway process.
%%
%% Architecture:
%%   Phoenix/Elixir App → macula_local_client → macula_gateway
%%                                               ↓ (QUIC)
%%                                          Other Peers
%%
%% @end
-module(macula_local_client).
-behaviour(gen_server).
-behaviour(macula_client_behaviour).

-include_lib("kernel/include/logger.hrl").

%% API - Connection management
-export([connect/2, connect_local/1, disconnect/1]).
%% API - Pub/Sub
-export([publish/3, publish/4, subscribe/3, unsubscribe/2, discover_subscribers/2]).
%% API - RPC
-export([call/3, call/4, advertise/3, advertise/4, unadvertise/2]).
%% API - Utility
-export([get_node_id/1]).
%% API - Platform Layer (v0.10.0+)
-export([
    register_workload/2,
    get_leader/1,
    subscribe_leader_changes/2,
    propose_crdt_update/3,
    propose_crdt_update/4,
    read_crdt/2
]).
%% Legacy API (kept for backward compatibility)
-export([start_link/1, stop/1, register_procedure/3, unregister_procedure/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    realm :: binary(),
    gateway_pid :: pid() | undefined,
    subscriptions = #{} :: #{reference() => {binary(), fun()}},  % SubRef -> {Topic, Callback}
    registrations = #{} :: #{binary() => fun()},
    event_handler :: pid() | undefined
}).

%%==============================================================================
%% API
%%==============================================================================

%%------------------------------------------------------------------------------
%% Connection Management
%%------------------------------------------------------------------------------

%% @doc Connect to remote gateway (not supported for local client)
%% For compatibility with macula_client_behaviour
-spec connect(binary() | string(), map()) -> {error, not_supported}.
connect(_Url, _Opts) ->
    {error, not_supported}.

%% @doc Create a local client connection to the gateway
-spec connect_local(map()) -> {ok, pid()} | {error, term()}.
connect_local(Opts) ->
    start_link(Opts).

%% @doc Disconnect the client
-spec disconnect(pid()) -> ok.
disconnect(Pid) ->
    stop(Pid).

%% @doc Start a local client connection to the gateway (legacy API)
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Stop the local client (legacy API)
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%------------------------------------------------------------------------------
%% Pub/Sub Operations
%%------------------------------------------------------------------------------

%% @doc Publish an event to a topic
-spec publish(pid(), binary(), map()) -> ok | {error, term()}.
publish(Pid, Topic, Payload) ->
    publish(Pid, Topic, Payload, #{}).

%% @doc Publish an event to a topic with options
-spec publish(pid(), binary(), map(), map()) -> ok | {error, term()}.
publish(Pid, Topic, Payload, Opts) ->
    gen_server:call(Pid, {publish, Topic, Payload, Opts}).

%% @doc Subscribe to a topic
-spec subscribe(pid(), binary(), pid()) -> {ok, reference()} | {error, term()}.
subscribe(Pid, Topic, HandlerPid) ->
    gen_server:call(Pid, {subscribe, Topic, HandlerPid}).

%% @doc Unsubscribe from a topic
-spec unsubscribe(pid(), reference()) -> ok | {error, term()}.
unsubscribe(Pid, SubRef) ->
    gen_server:call(Pid, {unsubscribe, SubRef}).

%% @doc Discover subscribers of a topic via DHT query
-spec discover_subscribers(pid(), binary()) -> {ok, [binary()]} | {error, term()}.
discover_subscribers(Pid, Topic) ->
    gen_server:call(Pid, {discover_subscribers, Topic}).

%%------------------------------------------------------------------------------
%% RPC Operations
%%------------------------------------------------------------------------------

%% @doc Call an RPC procedure with default options
-spec call(pid(), binary(), list()) -> {ok, term()} | {error, term()}.
call(Pid, Procedure, Args) ->
    call(Pid, Procedure, Args, #{}).

%% @doc Call an RPC procedure
-spec call(pid(), binary(), list(), map()) -> {ok, term()} | {error, term()}.
call(Pid, Procedure, Args, Opts) ->
    gen_server:call(Pid, {call, Procedure, Args, Opts}, 30000).

%% @doc Advertise an RPC service with default options
-spec advertise(pid(), binary(), fun()) -> {ok, reference()} | {error, term()}.
advertise(Pid, Procedure, Handler) ->
    advertise(Pid, Procedure, Handler, #{}).

%% @doc Advertise an RPC service with options
-spec advertise(pid(), binary(), fun(), map()) -> {ok, reference()} | {error, term()}.
advertise(Pid, Procedure, Handler, Opts) ->
    gen_server:call(Pid, {advertise, Procedure, Handler, Opts}).

%% @doc Unadvertise an RPC service
-spec unadvertise(pid(), binary()) -> ok | {error, term()}.
unadvertise(Pid, Procedure) ->
    gen_server:call(Pid, {unadvertise, Procedure}).

%% @doc Register an RPC procedure (legacy API, use advertise/3 instead)
-spec register_procedure(pid(), binary(), fun()) -> ok | {error, term()}.
register_procedure(Pid, Procedure, Handler) ->
    advertise(Pid, Procedure, Handler, #{}).

%% @doc Unregister an RPC procedure (legacy API, use unadvertise/2 instead)
-spec unregister_procedure(pid(), binary()) -> ok | {error, term()}.
unregister_procedure(Pid, Procedure) ->
    unadvertise(Pid, Procedure).

%%------------------------------------------------------------------------------
%% Utility Operations
%%------------------------------------------------------------------------------

%% @doc Get the node ID of the local gateway
-spec get_node_id(pid()) -> {ok, binary()} | {error, term()}.
get_node_id(Pid) ->
    gen_server:call(Pid, get_node_id).

%%------------------------------------------------------------------------------
%% Platform Layer Operations (v0.10.0+)
%%------------------------------------------------------------------------------

%% @doc Register this workload with the Platform Layer
-spec register_workload(pid(), map()) -> {ok, map()} | {error, term()}.
register_workload(Pid, Opts) ->
    gen_server:call(Pid, {register_workload, Opts}).

%% @doc Get the current Platform Layer leader node ID
-spec get_leader(pid()) -> {ok, binary()} | {error, no_leader | term()}.
get_leader(Pid) ->
    gen_server:call(Pid, get_leader).

%% @doc Subscribe to Platform Layer leader change notifications
-spec subscribe_leader_changes(pid(), fun((map()) -> ok)) ->
    {ok, reference()} | {error, term()}.
subscribe_leader_changes(Pid, Callback) ->
    gen_server:call(Pid, {subscribe_leader_changes, Callback}).

%% @doc Propose a CRDT update with default options (LWW-Register)
-spec propose_crdt_update(pid(), binary(), term()) -> ok | {error, term()}.
propose_crdt_update(Pid, Key, Value) ->
    propose_crdt_update(Pid, Key, Value, #{crdt_type => lww_register}).

%% @doc Propose a CRDT update with specific type
-spec propose_crdt_update(pid(), binary(), term(), map()) -> ok | {error, term()}.
propose_crdt_update(Pid, Key, Value, Opts) ->
    gen_server:call(Pid, {propose_crdt_update, Key, Value, Opts}).

%% @doc Read the current value of a CRDT-managed state entry
-spec read_crdt(pid(), binary()) -> {ok, term()} | {error, not_found | term()}.
read_crdt(Pid, Key) ->
    gen_server:call(Pid, {read_crdt, Key}).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================

init(Opts) ->
    Realm = maps:get(realm, Opts, <<"default">>),
    EventHandler = maps:get(event_handler, Opts, self()),

    ?LOG_INFO("Initializing local client for realm ~s", [Realm]),

    %% Find and connect to the local gateway process
    GatewayResult = find_gateway(),
    init_with_gateway(GatewayResult, Realm, EventHandler).

%% @private Gateway found - initialize state
init_with_gateway({ok, GatewayPid}, Realm, EventHandler) ->
    ?LOG_INFO("Connected to local gateway: ~p", [GatewayPid]),
    monitor(process, GatewayPid),
    State = #state{
        realm = Realm,
        gateway_pid = GatewayPid,
        event_handler = EventHandler
    },
    {ok, State};
%% @private Gateway not found - stop with error
init_with_gateway({error, Reason}, _Realm, _EventHandler) ->
    ?LOG_ERROR("Failed to find gateway: ~p", [Reason]),
    {stop, {gateway_not_found, Reason}}.

handle_call({publish, Topic, Payload, Opts}, _From, State) ->
    #state{gateway_pid = Gateway, realm = Realm} = State,

    %% Send publish request to gateway via gen_server:call
    %% Note: Opts are currently ignored for local clients
    _ = Opts,
    Result = case gen_server:call(Gateway, {local_publish, Realm, Topic, Payload}) of
        ok -> ok;
        {error, _} = Error -> Error
    end,
    {reply, Result, State};

handle_call({subscribe, Topic, Callback}, _From, State) ->
    #state{gateway_pid = Gateway, realm = Realm, subscriptions = Subs} = State,

    %% Subscribe via local gateway, passing our PID (not the callback)
    %% Gateway will send pubsub events to us, and we'll invoke the callback
    case gen_server:call(Gateway, {local_subscribe, Realm, Topic, self()}) of
        {ok, SubRef} ->
            %% Store callback for later invocation
            NewSubs = maps:put(SubRef, {Topic, Callback}, Subs),
            {reply, {ok, SubRef}, State#state{subscriptions = NewSubs}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({unsubscribe, SubRef}, _From, State) ->
    #state{gateway_pid = Gateway, subscriptions = Subs} = State,

    case gen_server:call(Gateway, {local_unsubscribe, SubRef}) of
        ok ->
            %% Remove from local callback tracking
            NewSubs = maps:remove(SubRef, Subs),
            {reply, ok, State#state{subscriptions = NewSubs}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({discover_subscribers, Topic}, _From, State) ->
    #state{gateway_pid = Gateway, realm = Realm} = State,

    %% Forward DHT query to gateway
    Result = gen_server:call(Gateway, {local_discover_subscribers, Realm, Topic}),
    {reply, Result, State};

handle_call({call, Procedure, Args, Opts}, _From, State) ->
    #state{gateway_pid = Gateway, realm = Realm} = State,

    %% Route RPC call through local gateway
    Result = gen_server:call(Gateway, {local_rpc_call, Realm, Procedure, Args, Opts}, 30000),
    {reply, Result, State};

handle_call({advertise, Procedure, Handler, Opts}, _From, State) ->
    #state{gateway_pid = Gateway, realm = Realm, registrations = Regs} = State,

    %% Forward advertise request to gateway
    case gen_server:call(Gateway, {local_advertise, Realm, Procedure, Handler, Opts}) of
        {ok, Ref} ->
            NewRegs = maps:put(Procedure, Handler, Regs),
            {reply, {ok, Ref}, State#state{registrations = NewRegs}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({unadvertise, Procedure}, _From, State) ->
    #state{gateway_pid = Gateway, registrations = Regs} = State,

    %% Forward unadvertise request to gateway
    case gen_server:call(Gateway, {local_unadvertise, Procedure}) of
        ok ->
            NewRegs = maps:remove(Procedure, Regs),
            {reply, ok, State#state{registrations = NewRegs}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({register, Procedure, Handler}, _From, State) ->
    #state{gateway_pid = Gateway, realm = Realm, registrations = Regs} = State,

    case gen_server:call(Gateway, {local_register_procedure, Realm, Procedure, Handler}) of
        ok ->
            NewRegs = maps:put(Procedure, Handler, Regs),
            {reply, ok, State#state{registrations = NewRegs}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({unregister, Procedure}, _From, State) ->
    #state{gateway_pid = Gateway, registrations = Regs} = State,

    case gen_server:call(Gateway, {local_unregister_procedure, Procedure}) of
        ok ->
            NewRegs = maps:remove(Procedure, Regs),
            {reply, ok, State#state{registrations = NewRegs}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call(get_node_id, _From, State) ->
    #state{gateway_pid = Gateway} = State,
    Result = gen_server:call(Gateway, local_get_node_id),
    {reply, Result, State};

%%------------------------------------------------------------------------------
%% Platform Layer Handlers (v0.14.0+ - Masterless/CRDT-based)
%%------------------------------------------------------------------------------

handle_call({register_workload, Opts}, _From, State) ->
    %% v0.14.0: Masterless architecture - no leader election
    %% Workloads register locally; state is CRDT-replicated
    WorkloadName = maps:get(workload_name, Opts, <<"unknown">>),
    Capabilities = maps:get(capabilities, Opts, []),

    ?LOG_INFO("Registered workload ~s with capabilities ~p (masterless mode)",
              [WorkloadName, Capabilities]),

    %% Return platform info (no leader in masterless design)
    Info = #{
        leader_node => undefined,  % No leader - masterless design
        cluster_size => 0,         % Cluster size via DHT peer count
        platform_version => <<"0.14.0">>,
        architecture => masterless
    },
    {reply, {ok, Info}, State};

handle_call(get_leader, _From, _State) ->
    %% v0.14.0: No leader in masterless design
    %% Return undefined - applications should use CRDTs for coordination
    {reply, {ok, undefined}, _State};

handle_call({subscribe_leader_changes, _Callback}, _From, State) ->
    %% v0.14.0: No leader changes in masterless design
    %% Return error - applications should use CRDT-based state instead
    {reply, {error, not_supported_in_masterless_architecture}, State};

handle_call({propose_crdt_update, Key, Value, Opts}, _From, State) ->
    CrdtType = maps:get(crdt_type, Opts, lww_register),
    Reply = do_propose_crdt_update(CrdtType, Key, Value),
    {reply, Reply, State};

handle_call({read_crdt, Key}, _From, State) ->
    Reply = do_read_crdt(Key),
    {reply, Reply, State}.

%% Async publish - fire-and-forget from caller's perspective
handle_cast({publish_async, Topic, Payload, Opts}, State) ->
    #state{gateway_pid = Gateway, realm = Realm} = State,

    %% Send publish request to gateway via gen_server:cast (async)
    %% Note: Opts are currently ignored for local clients
    _ = Opts,
    gen_server:cast(Gateway, {local_publish_async, Realm, Topic, Payload}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle pubsub events from gateway
%% Gateway sends messages in format: {publish, Topic, Payload}
handle_info({publish, Topic, Payload}, State) ->
    #state{subscriptions = Subs} = State,

    ?LOG_DEBUG("Received publish message for topic ~s", [Topic]),
    ?LOG_DEBUG("Current subscriptions: ~p", [Subs]),

    %% Find all callbacks for subscriptions matching this topic
    %% (since we store SubRef -> {Topic, Callback})
    MatchCount = maps:fold(fun(SubRef, {SubTopic, Callback}, Acc) ->
        ?LOG_DEBUG("Checking SubRef=~p, SubTopic=~p against Topic=~p",
                   [SubRef, SubTopic, Topic]),
        case SubTopic of
            Topic ->
                %% Topic matches exactly, invoke callback
                ?LOG_DEBUG("Match found, invoking callback for topic ~s", [Topic]),
                invoke_callback_safe(Callback, Payload, Topic),
                Acc + 1;
            _ ->
                %% Different topic, skip
                ?LOG_DEBUG("No match: ~p =/= ~p", [SubTopic, Topic]),
                Acc
        end
    end, 0, Subs),

    ?LOG_DEBUG("Total callbacks invoked for topic ~s: ~p", [Topic, MatchCount]),

    {noreply, State};

handle_info({'DOWN', _Ref, process, GatewayPid, Reason}, #state{gateway_pid = GatewayPid} = State) ->
    ?LOG_WARNING("Gateway down: ~p. Attempting reconnect...", [Reason]),
    GatewayResult = find_gateway(),
    handle_gateway_reconnect(GatewayResult, State);

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{subscriptions = Subs, registrations = Regs, gateway_pid = Gateway}) ->
    %% Clean up subscriptions (keys are SubRefs now)
    maps:foreach(fun(SubRef, _TopicCallback) ->
        gen_server:call(Gateway, {local_unsubscribe, SubRef})
    end, Subs),

    %% Clean up registrations (unadvertise all procedures)
    maps:foreach(fun(Procedure, _) ->
        gen_server:call(Gateway, {local_unadvertise, Procedure})
    end, Regs),

    ok.

%%==============================================================================
%% Internal functions
%%==============================================================================

%% @doc Find the local gateway process via whereis
-spec find_gateway() -> {ok, pid()} | {error, not_found}.
find_gateway() ->
    GatewayPid = whereis(macula_gateway),
    gateway_lookup_result(GatewayPid).

%% @private Gateway process found
gateway_lookup_result(Pid) when is_pid(Pid) ->
    {ok, Pid};
%% @private Gateway not registered
gateway_lookup_result(undefined) ->
    {error, not_found}.

%% NOTE: get_leader_reply/1 removed in v0.14.0 (masterless architecture)
%% Leader election is no longer used - applications use CRDTs for coordination

%% @private Gateway reconnect succeeded
handle_gateway_reconnect({ok, NewGateway}, State) ->
    ?LOG_INFO("Reconnected to gateway: ~p", [NewGateway]),
    monitor(process, NewGateway),
    {noreply, State#state{gateway_pid = NewGateway}};
%% @private Gateway reconnect failed
handle_gateway_reconnect({error, _}, State) ->
    ?LOG_ERROR("Gateway not available, stopping"),
    {stop, gateway_unavailable, State}.

%% @private LWW-Register CRDT update
do_propose_crdt_update(lww_register, Key, Value) ->
    Timestamp = erlang:system_time(millisecond),
    TableName = macula_crdt_storage,
    ensure_crdt_table_exists(TableName),
    ets:insert(TableName, {Key, {Value, Timestamp}}),
    ok;
%% @private Unsupported CRDT type
do_propose_crdt_update(CrdtType, _Key, _Value) ->
    {error, {unsupported_crdt_type, CrdtType}}.

%% @private Ensure ETS table exists (handles race condition)
ensure_crdt_table_exists(TableName) ->
    TableRef = ets:whereis(TableName),
    do_ensure_table(TableRef, TableName).

%% @private Table doesn't exist - create it
do_ensure_table(undefined, TableName) ->
    handle_table_creation(catch ets:new(TableName, [named_table, public, set]));
%% @private Table exists - nothing to do
do_ensure_table(_TableRef, _TableName) ->
    ok.

%% @private Read from CRDT storage
do_read_crdt(Key) ->
    TableName = macula_crdt_storage,
    TableRef = ets:whereis(TableName),
    do_crdt_lookup(TableRef, TableName, Key).

%% @private Table doesn't exist
do_crdt_lookup(undefined, _TableName, _Key) ->
    {error, not_found};
%% @private Table exists - perform lookup
do_crdt_lookup(_TableRef, TableName, Key) ->
    LookupResult = ets:lookup(TableName, Key),
    crdt_lookup_result(LookupResult).

%% @private Key found in CRDT table
crdt_lookup_result([{_Key, {Value, _Timestamp}}]) ->
    {ok, Value};
%% @private Key not found
crdt_lookup_result([]) ->
    {error, not_found}.

%% @private Handle ETS table creation result
handle_table_creation({'EXIT', {badarg, _}}) ->
    ok;  %% Table already exists (race condition)
handle_table_creation(_TableRef) ->
    ok.

%% @private Invoke callback safely with catch expression
invoke_callback_safe(Callback, Payload, Topic) ->
    handle_callback_result(catch Callback(Payload), Topic).

%% @private Handle callback result
handle_callback_result({'EXIT', {Reason, Stacktrace}}, Topic) ->
    ?LOG_ERROR("Callback error for topic ~s: ~p~nStacktrace: ~p",
               [Topic, Reason, Stacktrace]);
handle_callback_result({'EXIT', Reason}, Topic) ->
    ?LOG_ERROR("Callback error for topic ~s: ~p", [Topic, Reason]);
handle_callback_result(_Result, Topic) ->
    ?LOG_DEBUG("Callback completed successfully for topic ~s", [Topic]).
