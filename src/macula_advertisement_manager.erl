%%%-------------------------------------------------------------------
%%% @doc
%%% Advertisement manager GenServer - manages DHT service advertisements.
%%%
%%% Responsibilities:
%%% - Advertise services in DHT with periodic re-advertisement
%%% - Unadvertise services and cancel timers
%%% - Register handlers with local gateway
%%% - Manage service advertisement lifecycle
%%% - Periodic cleanup of expired local services (every 60s, TTL 300s default)
%%%
%%% Extracted from macula_connection.erl (Phase 6)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_advertisement_manager).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include("macula_config.hrl").

%% API
-export([
    start_link/1,
    advertise_service/4,
    unadvertise_service/2,
    get_active_advertisements/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    opts :: map(),
    node_id :: binary(),
    url :: binary(),
    connection_manager_pid :: pid(),
    advertised_services = #{} :: #{binary() => #{
        handler := fun((term()) -> term()),
        metadata := map(),
        ttl := pos_integer(),
        timer_ref := reference()
    }},
    service_registry :: macula_service_registry:registry()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    %% No local registration - allows multiple connection instances
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Advertise a service in DHT and local registry
-spec advertise_service(pid(), binary() | atom() | string(), fun((term()) -> term()), map()) ->
    {ok, reference()} | {error, term()}.
advertise_service(Pid, Procedure, Handler, Opts) ->
    gen_server:call(Pid, {advertise_service, Procedure, Handler, Opts}, 5000).

%% @doc Stop advertising a service
-spec unadvertise_service(pid(), binary() | atom() | string()) -> ok | {error, term()}.
unadvertise_service(Pid, Procedure) ->
    gen_server:call(Pid, {unadvertise_service, Procedure}, 5000).

%% @doc Get list of actively advertised services
-spec get_active_advertisements(pid()) -> {ok, [binary()]}.
get_active_advertisements(Pid) ->
    gen_server:call(Pid, get_active_advertisements, 5000).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    ?LOG_INFO("Advertisement manager starting"),

    %% Get node ID from opts
    NodeId = maps:get(node_id, Opts, macula_utils:generate_node_id()),

    %% Get URL for endpoint advertisement
    Url = maps:get(url, Opts, <<>>),

    %% connection_manager_pid will be set via cast message after init
    ConnMgrPid = undefined,

    %% Initialize service registry
    Registry = macula_service_registry:new(),

    %% Schedule periodic service registry cleanup (every 60 seconds)
    %% This prunes expired local services based on service_ttl (default 300s)
    erlang:send_after(60000, self(), cleanup_expired_services),

    State = #state{
        opts = Opts,
        node_id = NodeId,
        url = Url,
        connection_manager_pid = ConnMgrPid,
        advertised_services = #{},
        service_registry = Registry
    },

    {ok, State}.

handle_call({advertise_service, Procedure, Handler, Opts}, _From, State) ->
    BinaryProcedure = macula_utils:ensure_binary(Procedure),

    %% Check if already advertised and cancel existing timer
    cancel_existing_advertisement(BinaryProcedure, State#state.advertised_services),

    %% Advertise service locally
    Metadata = maps:get(metadata, Opts, #{}),
    Registry = State#state.service_registry,
    Registry2 = macula_service_registry:advertise_local(Registry, BinaryProcedure, Handler, Metadata),

    %% Register handler with local gateway (if it's running)
    case macula_gateway:register_handler(BinaryProcedure, Handler) of
        ok ->
            ?LOG_INFO("Registered handler for ~s with local gateway", [BinaryProcedure]);
        {error, no_gateway} ->
            ?LOG_DEBUG("No local gateway running, handler stored locally only")
    end,

    %% Store service in DHT for decentralized discovery
    TTL = maps:get(ttl, Opts, ?DEFAULT_TTL),
    ServiceKey = crypto:hash(sha256, BinaryProcedure),

    %% Allow custom advertise_endpoint for peer-to-peer (providers advertise their own URL)
    AdvertiseEndpoint = maps:get(advertise_endpoint, Opts, State#state.url),
    ServiceValue = #{
        node_id => State#state.node_id,
        endpoint => AdvertiseEndpoint,
        metadata => Metadata,
        ttl => TTL
    },

    %% Send STORE message to connected DHT node (distributed approach)
    ?LOG_INFO("Sending STORE to DHT for service ~s", [BinaryProcedure]),
    ?LOG_DEBUG("ServiceKey: ~p, ServiceValue: ~p", [ServiceKey, ServiceValue]),

    %% Send STORE to DHT if connection manager is available (best effort)
    %% This allows local-only operation for testing and offline scenarios
    ConnMgrPid = State#state.connection_manager_pid,
    send_store_to_dht(ConnMgrPid, ServiceKey, ServiceValue, BinaryProcedure, State),

    %% Schedule re-advertisement timer (TTL - 60 seconds to ensure no gap)
    %% Minimum interval is 10 seconds to avoid too frequent re-advertisements
    ReadvInterval = max(10, TTL - 60) * 1000,  % Convert to milliseconds
    TimerRef = erlang:send_after(ReadvInterval, self(), {readvertise, BinaryProcedure}),
    ?LOG_DEBUG("Scheduled re-advertisement for ~s in ~p seconds",
              [BinaryProcedure, ReadvInterval div 1000]),

    %% Store service info for re-advertisement
    ServiceInfo = #{
        handler => Handler,
        metadata => Metadata,
        ttl => TTL,
        timer_ref => TimerRef
    },
    AdvertisedServices = State#state.advertised_services,
    AdvertisedServices2 = AdvertisedServices#{BinaryProcedure => ServiceInfo},

    %% Generate reference for tracking
    Ref = make_ref(),

    State2 = State#state{
        service_registry = Registry2,
        advertised_services = AdvertisedServices2
    },

    {reply, {ok, Ref}, State2};

handle_call({unadvertise_service, Procedure}, _From, State) ->
    BinaryProcedure = macula_utils:ensure_binary(Procedure),

    %% Remove local service
    Registry = State#state.service_registry,
    Registry2 = macula_service_registry:unadvertise_local(Registry, BinaryProcedure),

    %% Unregister handler from local gateway
    macula_gateway:unregister_handler(BinaryProcedure),
    ?LOG_INFO("Unregistered handler for ~s from local gateway", [BinaryProcedure]),

    %% Send UNREGISTER message to gateway (best effort)
    UnregisterMsg = #{
        procedure => BinaryProcedure,
        node_id => State#state.node_id
    },

    %% Send to gateway if connection manager is available (allows local-only operation)
    ConnMgrPid = State#state.connection_manager_pid,
    send_unregister_to_gateway(ConnMgrPid, UnregisterMsg, BinaryProcedure, State),

    %% Cancel re-advertisement timer if exists
    AdvertisedServices = State#state.advertised_services,
    AdvertisedServices2 = cancel_advertisement_timer(BinaryProcedure, AdvertisedServices),

    State2 = State#state{
        service_registry = Registry2,
        advertised_services = AdvertisedServices2
    },

    {reply, ok, State2};

handle_call(get_active_advertisements, _From, State) ->
    Procedures = maps:keys(State#state.advertised_services),
    {reply, {ok, Procedures}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Set connection manager PID (sent by facade after init)
handle_cast({set_connection_manager_pid, Pid}, State) ->
    {noreply, State#state{connection_manager_pid = Pid}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle re-advertisement timer
handle_info({readvertise, Procedure}, State) ->
    AdvertisedServices = State#state.advertised_services,
    ServiceInfo = maps:get(Procedure, AdvertisedServices, undefined),
    do_readvertise(ServiceInfo, Procedure, State);

%% @doc Handle periodic service registry cleanup
handle_info(cleanup_expired_services, State) ->
    Registry = State#state.service_registry,
    {Registry2, RemovedCount} = macula_service_registry:prune_expired_local_services(Registry),
    log_cleanup_result(RemovedCount),
    erlang:send_after(60000, self(), cleanup_expired_services),
    {noreply, State#state{service_registry = Registry2}};

handle_info(_Info, State) ->
    ?LOG_DEBUG("Unhandled handle_info message: ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    ?LOG_INFO("Advertisement manager terminating"),

    %% Cancel all re-advertisement timers
    maps:foreach(
        fun(Procedure, #{timer_ref := TimerRef}) ->
            erlang:cancel_timer(TimerRef),
            ?LOG_DEBUG("Cancelled re-advertisement timer for ~s on shutdown", [Procedure])
        end,
        State#state.advertised_services
    ),

    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Send STORE to DHT if connection manager is available (best effort).
%% Pattern match on undefined - allows local-only operation.
send_store_to_dht(undefined, _ServiceKey, _ServiceValue, Procedure, _State) ->
    ?LOG_INFO("No connection manager, service ~s advertised locally only", [Procedure]),
    ok;
send_store_to_dht(ConnMgrPid, ServiceKey, ServiceValue, Procedure, _State) when is_pid(ConnMgrPid) ->
    %% Create STORE protocol message (let it crash on encoding errors)
    StoreMsg = macula_routing_protocol:encode_store(ServiceKey, ServiceValue),
    ?LOG_DEBUG("Encoded STORE message: ~p", [StoreMsg]),

    %% Send over QUIC to connected node (best effort - connection may not be established yet)
    case macula_connection:send_message(ConnMgrPid, store, StoreMsg) of
        ok ->
            ?LOG_INFO("Sent STORE to DHT for service ~s", [Procedure]);
        {error, Reason} ->
            ?LOG_WARNING("Failed to send STORE for service ~s: ~p (advertised locally only)",
                        [Procedure, Reason])
    end,
    ok.

%% @doc Send UNREGISTER to gateway if connection manager is available (best effort).
%% Pattern match on undefined - allows local-only operation.
send_unregister_to_gateway(undefined, _UnregisterMsg, Procedure, _State) ->
    ?LOG_INFO("No connection manager, service ~s unregistered locally only", [Procedure]),
    ok;
send_unregister_to_gateway(ConnMgrPid, UnregisterMsg, Procedure, _State) when is_pid(ConnMgrPid) ->
    %% Send UNREGISTER message (let it crash on send failures)
    case macula_connection:send_message(ConnMgrPid, unregister, UnregisterMsg) of
        ok ->
            ?LOG_INFO("Unregistered service ~s from gateway", [Procedure]);
        {error, Reason} ->
            ?LOG_WARNING("Failed to unregister service ~s from gateway: ~p",
                        [Procedure, Reason])
    end,
    ok.

%% @doc Cancel re-advertisement timer for a service (service not found).
-spec cancel_advertisement_timer(binary(), map()) -> map().
cancel_advertisement_timer(Procedure, AdvertisedServices) ->
    FindResult = maps:find(Procedure, AdvertisedServices),
    do_cancel_timer(FindResult, Procedure, AdvertisedServices).

%% @private Service not found
do_cancel_timer(error, _Procedure, AdvertisedServices) ->
    AdvertisedServices;
%% @private Service found - cancel timer and remove
do_cancel_timer({ok, #{timer_ref := TimerRef}}, Procedure, AdvertisedServices) ->
    erlang:cancel_timer(TimerRef),
    ?LOG_DEBUG("Cancelled re-advertisement timer for ~s", [Procedure]),
    maps:remove(Procedure, AdvertisedServices).

%% @private Service was unadvertised before timer fired
do_readvertise(undefined, Procedure, State) ->
    ?LOG_WARNING("Re-advertisement timer fired for unadvertised service: ~s", [Procedure]),
    {noreply, State};
%% @private Re-publish service to DHT and reschedule timer
do_readvertise(#{metadata := Metadata, ttl := TTL} = ServiceInfo, Procedure, State) ->
    ?LOG_DEBUG("Re-advertising service ~s", [Procedure]),

    %% Compute service key and value
    ServiceKey = crypto:hash(sha256, Procedure),
    ServiceValue = #{
        node_id => State#state.node_id,
        endpoint => State#state.url,
        metadata => Metadata,
        ttl => TTL
    },

    %% Send STORE to DHT (best effort)
    ConnMgrPid = State#state.connection_manager_pid,
    send_store_to_dht(ConnMgrPid, ServiceKey, ServiceValue, Procedure, State),

    %% Reschedule timer
    ReadvInterval = max(10, TTL - 60) * 1000,
    erlang:cancel_timer(maps:get(timer_ref, ServiceInfo)),
    NewTimerRef = erlang:send_after(ReadvInterval, self(), {readvertise, Procedure}),
    ?LOG_DEBUG("Rescheduled re-advertisement for ~s in ~p seconds",
              [Procedure, ReadvInterval div 1000]),

    %% Update timer_ref in state
    UpdatedServiceInfo = ServiceInfo#{timer_ref => NewTimerRef},
    AdvertisedServices = State#state.advertised_services,
    AdvertisedServices2 = AdvertisedServices#{Procedure => UpdatedServiceInfo},
    State2 = State#state{advertised_services = AdvertisedServices2},

    {noreply, State2}.

%% @private No expired services removed
log_cleanup_result(0) ->
    ?LOG_DEBUG("Service cleanup: no expired services");
%% @private N expired services removed
log_cleanup_result(N) ->
    ?LOG_INFO("Service cleanup: removed ~p expired service(s)", [N]).

%% @doc Cancel existing advertisement timer when re-advertising.
-spec cancel_existing_advertisement(binary(), map()) -> ok.
cancel_existing_advertisement(Procedure, AdvertisedServices) ->
    ServiceInfo = maps:get(Procedure, AdvertisedServices, undefined),
    do_cancel_existing_advertisement(ServiceInfo, Procedure).

%% @private Service not found - nothing to cancel
do_cancel_existing_advertisement(undefined, _Procedure) ->
    ok;
%% @private Service found - cancel old timer and log warning
do_cancel_existing_advertisement(#{timer_ref := OldTimerRef}, Procedure) ->
    ?LOG_WARNING("Service ~s already advertised, updating", [Procedure]),
    erlang:cancel_timer(OldTimerRef),
    ok.
