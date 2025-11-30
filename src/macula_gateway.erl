%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Gateway - HTTP/3 Message Router &amp; Orchestrator
%%%
%%% Main API module and coordinator for the Macula Gateway.
%%% The gateway can be embedded in applications or run standalone as a relay node.
%%%
%%% == Quick Start (Embedded Gateway) ==
%%%
%%% ```
%%% %% Start an embedded gateway
%%% {ok, Gateway} = macula_gateway:start_link([
%%%     {port, 9443},
%%%     {realm, <<"com.example.realm">>},
%%%     {cert_file, "cert.pem"},
%%%     {key_file, "key.pem"}
%%% ]).
%%%
%%% %% Register an RPC handler
%%% ok = macula_gateway:register_handler(Gateway, <<"calculator.add">>, fun(Args) ->
%%%     A = maps:get(a, Args),
%%%     B = maps:get(b, Args),
%%%     #{result => A + B}
%%% end).
%%% '''
%%%
%%% == Quick Start (Standalone Gateway) ==
%%%
%%% Configure `sys.config':
%%%
%%% ```
%%% [
%%%   {macula, [
%%%     {gateway_port, 9443},
%%%     {gateway_realm, <<"com.example.realm">>},
%%%     {cert_file, "/path/to/cert.pem"},
%%%     {key_file, "/path/to/key.pem"}
%%%   ]}
%%% ].
%%% '''
%%%
%%% Start application:
%%%
%%% ```
%%% application:start(macula).
%%% '''
%%%
%%% == Architecture (Modular Design - Refactored Jan 2025) ==
%%%
%%% Gateway (this module):
%%%   - QUIC Listener Management
%%%   - Message Decoding &amp; Routing
%%%   - Supervisor Coordination
%%%   - API Facade
%%%
%%% Child Modules (managed via macula_gateway_sup):
%%%   - `macula_gateway_client_manager': Client lifecycle management
%%%   - `macula_gateway_pubsub': Pub/Sub message routing with wildcards
%%%   - `macula_gateway_rpc': RPC handler registration &amp; invocation
%%%   - `macula_gateway_mesh': Mesh connection pooling
%%%
%%% Stateless Delegation Modules:
%%%   - `macula_gateway_dht': DHT query forwarding to routing server
%%%   - `macula_gateway_rpc_router': Multi-hop RPC routing via DHT
%%%
%%% Single Responsibility Principle:
%%%   Each module has one clear purpose and delegates to specialized
%%%   child modules. Gateway acts as orchestrator, not implementer.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include("macula_config.hrl").

%% API
-export([
    start_link/0,
    start_link/1,
    stop/1,
    get_stats/1,
    register_handler/2,
    unregister_handler/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_continue/2,
    handle_info/2,
    terminate/2
]).

-record(state, {
    port :: inet:port_number(),
    realm :: binary(),
    node_id :: binary() | undefined,              % 32-byte local node ID
    parent_sup :: pid() | undefined,              % Parent supervisor (macula_gateway_system)
    listener :: pid() | undefined,
    supervisor :: pid() | undefined,              % Supervisor PID
    client_manager :: pid() | undefined,          % Client manager child PID
    pubsub :: pid() | undefined,                  % Pub/Sub child PID
    rpc :: pid() | undefined,                     % RPC child PID
    mesh :: pid() | undefined,                    % Mesh connection manager child PID
    client_streams :: #{binary() => pid()}        % node_id => stream for bidirectional communication
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the gateway with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the gateway with custom options.
%% Options:
%%   {port, Port} - Listen port (default: 9443)
%%   {realm, Realm} - Default realm (default: "macula.default")
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, macula_gateway}, ?MODULE, Opts, []).

%% @doc Stop the gateway.
-spec stop(pid()) -> ok.
stop(Gateway) ->
    gen_server:stop(Gateway).

%% @doc Get gateway statistics.
-spec get_stats(pid()) -> map().
get_stats(Gateway) ->
    gen_server:call(Gateway, get_stats).

%% @doc Register a handler for a procedure.
-spec register_handler(binary(), fun()) -> ok | {error, term()}.
register_handler(Procedure, Handler) ->
    case whereis(macula_gateway) of
        undefined ->
            {error, no_gateway};
        Pid ->
            gen_server:call(Pid, {register_handler, Procedure, Handler})
    end.

%% @doc Unregister a handler for a procedure.
-spec unregister_handler(binary()) -> ok.
unregister_handler(Procedure) ->
    case whereis(macula_gateway) of
        undefined ->
            ok;
        Pid ->
            gen_server:call(Pid, {unregister_handler, Procedure})
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    Port = proplists:get_value(port, Opts, ?DEFAULT_PORT),
    Realm = proplists:get_value(realm, Opts, ?DEFAULT_REALM),

    ?LOG_INFO("Initializing gateway (supervised mode)"),
    ?LOG_INFO("Port: ~p, Realm: ~s", [Port, Realm]),

    %% Gateway is now supervised by macula_gateway_system (root supervisor)
    %% We need to find our sibling processes and wire ourselves together

    %% Step 1: Get parent supervisor
    case find_parent_supervisor() of
        {ok, ParentSup} ->
            init_with_supervisor(ParentSup, Port, Realm);
        {error, Reason} ->
            ?LOG_ERROR("Failed to find parent supervisor: ~p", [Reason]),
            {stop, {no_parent_supervisor, Reason}}
    end.

%% @private
%% @doc Find the parent supervisor (macula_gateway_system).
%% Uses process dictionary and fallback to supervisor tree traversal.
-spec find_parent_supervisor() -> {ok, pid()} | {error, term()}.
find_parent_supervisor() ->
    %% Try $ancestors first (standard OTP approach)
    case get('$ancestors') of
        [ParentSup | _] when is_pid(ParentSup) ->
            {ok, ParentSup};
        _ ->
            %% Fallback: find by traversing from macula_root
            find_gateway_system_via_root()
    end.

%% @private
%% @doc Fallback method to find macula_gateway_system by traversing from macula_root.
-spec find_gateway_system_via_root() -> {ok, pid()} | {error, term()}.
find_gateway_system_via_root() ->
    case whereis(macula_root) of
        undefined ->
            {error, no_macula_root};
        RootPid ->
            Children = supervisor:which_children(RootPid),
            find_gateway_system_in_children(Children)
    end.

%% @private
%% @doc Find macula_gateway_system in supervisor children list.
-spec find_gateway_system_in_children(list()) -> {ok, pid()} | {error, term()}.
find_gateway_system_in_children([{macula_gateway_system, Pid, supervisor, _} | _Rest]) when is_pid(Pid) ->
    {ok, Pid};
find_gateway_system_in_children([_Child | Rest]) ->
    find_gateway_system_in_children(Rest);
find_gateway_system_in_children([]) ->
    {error, gateway_system_not_found}.

%% @private
%% @doc Initialize gateway with supervisor context.
%% Defers sibling wiring to handle_continue to avoid initialization deadlock.
-spec init_with_supervisor(pid(), inet:port_number(), binary()) ->
    {ok, #state{}, {continue, wire_siblings}}.
init_with_supervisor(ParentSup, Port, Realm) ->
    ?LOG_DEBUG("Parent supervisor found: ~p", [ParentSup]),
    ?LOG_DEBUG("Deferring sibling wiring to handle_continue"),

    %% Build initial state and defer sibling finding to handle_continue
    %% This avoids deadlock where init/1 calls supervisor:which_children/1
    State = #state{
        port = Port,
        realm = Realm,
        parent_sup = ParentSup,
        client_streams = #{}
    },

    ?LOG_DEBUG("Returning from init with continue action"),
    {ok, State, {continue, wire_siblings}}.

%% @private
%% @doc Wire gateway to sibling processes after init completes.
%% Called via handle_continue to avoid supervisor initialization deadlock.
-spec wire_siblings(#state{}) -> #state{} | {stop, term()}.
wire_siblings(State) ->
    #state{parent_sup = ParentSup, port = Port, realm = Realm} = State,

    ?LOG_DEBUG("handle_continue: Wiring siblings..."),
    ?LOG_DEBUG("Step 1: Finding QUIC server sibling..."),
    case find_sibling(ParentSup, macula_gateway_quic_server) of
        {ok, QuicServerPid} ->
            ?LOG_DEBUG("Found QUIC server sibling: ~p", [QuicServerPid]),

            %% Step 3: Wire ourselves to QUIC server
            ?LOG_DEBUG("Step 3: Wiring gateway to QUIC server..."),
            ok = macula_gateway_quic_server:set_gateway(QuicServerPid, self()),
            ?LOG_DEBUG("Wired gateway to QUIC server"),

            %% Step 4: Find workers supervisor sibling
            ?LOG_DEBUG("Step 4: Finding workers supervisor..."),
            case find_sibling(ParentSup, macula_gateway_workers_sup) of
                {ok, WorkersSupPid} ->
                    ?LOG_DEBUG("Found workers supervisor sibling: ~p", [WorkersSupPid]),

                    %% Step 5: Get worker PIDs from workers supervisor
                    ?LOG_DEBUG("Step 5: Getting worker PIDs..."),
                    ?LOG_DEBUG("Getting clients PID..."),
                    {ok, ClientsPid} = macula_gateway_workers_sup:get_clients(WorkersSupPid),
                    ?LOG_DEBUG("Getting pubsub PID..."),
                    {ok, PubSubPid} = macula_gateway_workers_sup:get_pubsub(WorkersSupPid),
                    ?LOG_DEBUG("Getting rpc PID..."),
                    {ok, RpcPid} = macula_gateway_workers_sup:get_rpc(WorkersSupPid),
                    ?LOG_DEBUG("Getting mesh PID..."),
                    {ok, MeshPid} = macula_gateway_workers_sup:get_mesh(WorkersSupPid),

                    ?LOG_DEBUG("Worker PIDs retrieved:"),
                    ?LOG_DEBUG("  - Clients: ~p", [ClientsPid]),
                    ?LOG_DEBUG("  - PubSub: ~p", [PubSubPid]),
                    ?LOG_DEBUG("  - RPC: ~p", [RpcPid]),
                    ?LOG_DEBUG("  - Mesh: ~p", [MeshPid]),

                    %% Step 6: Start routing server for DHT operations
                    ?LOG_DEBUG("Step 6: Starting DHT routing server..."),
                    LocalNodeId = get_node_id(Realm, Port),
                    ?LOG_INFO("Using node ID: ~p", [binary:encode_hex(LocalNodeId)]),

                    RoutingConfig = #{
                        k => 20,      % Kademlia k-bucket size
                        alpha => 3    % Kademlia concurrency parameter
                    },

                    ?LOG_DEBUG("Calling macula_routing_server:start_link..."),
                    case macula_routing_server:start_link(LocalNodeId, RoutingConfig) of
                        {ok, _RoutingPid} ->
                            ?LOG_INFO("DHT routing server started"),
                            ok;
                        {error, {already_started, _}} ->
                            ?LOG_INFO("DHT routing server already running"),
                            ok;
                        {error, RoutingErr} ->
                            ?LOG_WARNING("Failed to start routing server: ~p", [RoutingErr]),
                            ok  % Continue without routing server
                    end,

                    %% Mark health server as ready (if running)
                    ?LOG_DEBUG("Step 7: Notifying health server..."),
                    notify_health_server_ready(),
                    ?LOG_DEBUG("Health server notified"),

                    %% Register diagnostics procedures (if running)
                    ?LOG_DEBUG("Step 8: Registering diagnostics..."),
                    register_diagnostics_procedures(self()),
                    ?LOG_DEBUG("Diagnostics registered"),

                    %% Step 7: Build final state
                    NewState = State#state{
                        node_id = LocalNodeId,
                        listener = QuicServerPid,  % QUIC server PID
                        supervisor = WorkersSupPid,  % Workers supervisor PID
                        client_manager = ClientsPid,
                        pubsub = PubSubPid,
                        rpc = RpcPid,
                        mesh = MeshPid
                    },

                    ?LOG_INFO("Initialization complete (supervised mode)"),
                    NewState;

                {error, WorkersSupErr} ->
                    ?LOG_ERROR("Failed to find workers supervisor: ~p", [WorkersSupErr]),
                    {stop, {no_workers_supervisor, WorkersSupErr}}
            end;

        {error, QuicServerErr} ->
            ?LOG_ERROR("Failed to find QUIC server: ~p", [QuicServerErr]),
            {stop, {no_quic_server, QuicServerErr}}
    end.

%% @private
%% @doc Find a sibling process by module name in parent supervisor's children.
-spec find_sibling(pid(), module()) -> {ok, pid()} | {error, term()}.
find_sibling(ParentSup, Module) ->
    Children = supervisor:which_children(ParentSup),
    find_sibling_in_children(Children, Module).

%% Pattern match on child list - found the module
find_sibling_in_children([{Module, Pid, _Type, _Modules} | _Rest], Module) when is_pid(Pid) ->
    {ok, Pid};
%% Pattern match on child list - keep searching
find_sibling_in_children([_Child | Rest], Module) ->
    find_sibling_in_children(Rest, Module);
%% Pattern match on empty list - not found
find_sibling_in_children([], Module) ->
    {error, {not_found, Module}}.


%% @private
%% @doc Get node ID from HOSTNAME env var (set by Docker) or generate from {Realm, Port}.
%% Returns a 32-byte binary (raw binary for Kademlia, never hex-encoded).
%% MUST match macula_gateway_system:get_node_id/2 exactly!
%%
%% Priority:
%% 1. NODE_NAME env var (explicit, highest priority)
%% 2. HOSTNAME env var (Docker sets this to container hostname - unique per container)
%% 3. Fallback to {Realm, Port} only (NO MAC - MAC is shared across Docker containers)
-spec get_node_id(binary(), inet:port_number()) -> binary().
get_node_id(Realm, Port) ->
    case os:getenv("NODE_NAME") of
        false ->
            %% No NODE_NAME, try HOSTNAME (Docker sets this to container hostname)
            case os:getenv("HOSTNAME") of
                false ->
                    %% No HOSTNAME either, use {Realm, Port} as last resort
                    %% Note: This WILL collide if multiple nodes share same realm+port
                    ?LOG_WARNING("No HOSTNAME or NODE_NAME set, using realm+port only"),
                    ?LOG_WARNING("This may cause node_id collisions in Docker!"),
                    crypto:hash(sha256, term_to_binary({Realm, Port}));
                Hostname when is_list(Hostname) ->
                    %% Use HOSTNAME from Docker - unique per container
                    ?LOG_INFO("Using HOSTNAME-based node ID: ~s, Realm=~s, Port=~p",
                             [Hostname, Realm, Port]),
                    crypto:hash(sha256, term_to_binary({Realm, list_to_binary(Hostname), Port}))
            end;
        NodeName when is_list(NodeName) ->
            %% Use NODE_NAME from environment - hash it to get 32-byte binary
            ?LOG_INFO("Using NODE_NAME from environment: ~s", [NodeName]),
            crypto:hash(sha256, list_to_binary(NodeName))
    end.


handle_call(get_stats, _From, State) ->
    ClientMgr = State#state.client_manager,
    Rpc = State#state.rpc,

    %% Query child modules for their stats
    {ok, AllClients} = macula_gateway_clients:get_all_clients(ClientMgr),
    {ok, AllHandlers} = macula_gateway_rpc:list_handlers(Rpc),

    Stats = #{
        port => State#state.port,
        realm => State#state.realm,
        clients => length(AllClients),
        registrations => length(AllHandlers)
    },
    {reply, Stats, State};

handle_call({register_handler, Procedure, Handler}, _From, State) ->
    ?LOG_DEBUG("Registering handler for procedure: ~s (delegating to rpc)", [Procedure]),
    Rpc = State#state.rpc,
    ok = macula_gateway_rpc:register_handler(Rpc, Procedure, Handler),
    {reply, ok, State};

handle_call({unregister_handler, Procedure}, _From, State) ->
    ?LOG_DEBUG("Unregistering handler for procedure: ~s (delegating to rpc)", [Procedure]),
    Rpc = State#state.rpc,
    ok = macula_gateway_rpc:unregister_handler(Rpc, Procedure),
    {reply, ok, State};

%% @doc Handle message routed from QUIC server (async).
%% Routes decoded QUIC messages to appropriate business logic handlers.
%% This is now a cast to prevent blocking the QUIC server on message processing.

%% Local client (in-VM) message handlers
handle_call({local_publish, _Realm, Topic, Payload}, _From, State) ->
    ?LOG_DEBUG("Local publish: ~s", [Topic]),

    %% Reply immediately to avoid blocking the caller
    %% Distribution happens asynchronously in a spawned process
    gen_server:cast(self(), {distribute_publish, Topic, Payload}),

    {reply, ok, State};

handle_call({local_subscribe, _Realm, Topic, HandlerPid}, _From, State) ->
    ?LOG_DEBUG("Local subscribe: ~s", [Topic]),
    PubSub = State#state.pubsub,
    ok = macula_gateway_pubsub:subscribe(PubSub, HandlerPid, Topic),
    SubRef = make_ref(),
    {reply, {ok, SubRef}, State};

handle_call({local_unsubscribe, _SubRef}, _From, State) ->
    ?LOG_DEBUG("Local unsubscribe"),
    %% Note: Current pubsub implementation doesn't track subscription refs
    %% This is a simplified implementation - proper ref tracking would be added in production
    {reply, ok, State};

handle_call({local_rpc_call, _Realm, Procedure, Args, _Opts}, _From, State) ->
    ?LOG_DEBUG("Local RPC call: ~s", [Procedure]),
    Rpc = State#state.rpc,
    Result = macula_gateway_rpc:invoke_handler(Rpc, Procedure, Args),
    {reply, Result, State};

handle_call({local_register_procedure, _Realm, Procedure, Handler}, _From, State) ->
    ?LOG_DEBUG("Local register procedure: ~s", [Procedure]),
    Rpc = State#state.rpc,
    ok = macula_gateway_rpc:register_handler(Rpc, Procedure, Handler),
    {reply, ok, State};

handle_call({local_unregister_procedure, Procedure}, _From, State) ->
    ?LOG_DEBUG("Local unregister procedure: ~s", [Procedure]),
    Rpc = State#state.rpc,
    ok = macula_gateway_rpc:unregister_handler(Rpc, Procedure),
    {reply, ok, State};

handle_call({local_advertise, _Realm, Procedure, Handler, Opts}, _From, State) ->
    ?LOG_DEBUG("Local advertise: ~s", [Procedure]),

    %% 1. Register handler locally with gateway RPC
    Rpc = State#state.rpc,
    ok = macula_gateway_rpc:register_handler(Rpc, Procedure, Handler),
    ?LOG_DEBUG("Registered handler for ~s with local RPC", [Procedure]),

    %% 2. Advertise to DHT for service discovery
    ServiceKey = crypto:hash(sha256, Procedure),
    TTL = maps:get(ttl, Opts, 300),  % Default 5 minutes
    Metadata = maps:get(metadata, Opts, #{}),

    %% Service value includes node ID and endpoint for P2P discovery
    ServiceValue = #{
        node_id => State#state.node_id,
        endpoint => <<"gateway://localhost">>,  % Local clients connect via gateway
        metadata => Metadata,
        ttl => TTL
    },

    %% Store in DHT (propagates to k closest nodes)
    case whereis(macula_routing_server) of
        undefined ->
            ?LOG_WARNING("Routing server not running, service ~s advertised locally only",
                     [Procedure]);
        RoutingServerPid ->
            case macula_routing_server:store(RoutingServerPid, ServiceKey, ServiceValue) of
                ok ->
                    ?LOG_INFO("Successfully stored service ~s in DHT", [Procedure]);
                {error, StoreError} ->
                    ?LOG_WARNING("Failed to store service ~s in DHT: ~p",
                             [Procedure, StoreError])
            end
    end,

    %% Return reference for tracking (matching behaviour spec)
    Ref = make_ref(),
    {reply, {ok, Ref}, State};

handle_call({local_unadvertise, Procedure}, _From, State) ->
    ?LOG_DEBUG("Local unadvertise: ~s", [Procedure]),
    Rpc = State#state.rpc,
    ok = macula_gateway_rpc:unregister_handler(Rpc, Procedure),
    {reply, ok, State};

handle_call({local_discover_subscribers, _Realm, Topic}, _From, State) ->
    ?LOG_DEBUG("Local discover subscribers for topic: ~s", [Topic]),
    %% Hash the topic to create DHT key
    TopicKey = crypto:hash(sha256, Topic),

    %% Query the routing server (DHT) for subscribers
    Result = case whereis(macula_routing_server) of
        undefined ->
            ?LOG_DEBUG("Routing server not running"),
            {error, routing_server_not_running};
        RoutingServerPid ->
            case macula_routing_server:find_value(RoutingServerPid, TopicKey, 20) of
                {ok, Subscribers} when is_list(Subscribers) ->
                    {ok, Subscribers};
                {ok, _Other} ->
                    {ok, []};
                {error, Reason} ->
                    {error, Reason}
            end
    end,
    {reply, Result, State};

handle_call(local_get_node_id, _From, State) ->
    ?LOG_DEBUG("Local get node ID"),
    {reply, {ok, State#state.node_id}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle rpc_route message forwarded from connection
handle_cast({process_rpc_route, RpcRouteMsg}, State) ->
    ?LOG_DEBUG("Processing rpc_route message from connection"),
    %% Extract the CALL payload from rpc_route envelope
    #{<<"payload">> := CallMsg} = RpcRouteMsg,
    %% Call handle_rpc_call_routed with Stream=undefined, CallMsg, RpcRouteMsg, State
    handle_rpc_call_routed(undefined, CallMsg, RpcRouteMsg, State);

%% @doc Handle async distribution of published messages
handle_cast({distribute_publish, Topic, Payload}, State) ->
    ?LOG_DEBUG("Async distributing ~s via DHT-based routing", [Topic]),
    PubSub = State#state.pubsub,

    %% Get local subscribers for the topic
    {ok, LocalSubscribers} = macula_gateway_pubsub:get_subscribers(PubSub, Topic),

    %% Create PUBLISH message for routing
    PubMsg = #{
        <<"topic">> => Topic,
        <<"payload">> => Payload,
        <<"qos">> => 0,
        <<"retain">> => false,
        <<"message_id">> => crypto:strong_rand_bytes(16)
    },

    %% Distribute to local + remote subscribers (runs in cast, so it's async)
    macula_gateway_pubsub_router:distribute(
        LocalSubscribers,
        PubMsg,
        State#state.node_id,
        State#state.mesh,
        State#state.client_manager
    ),

    %% Also relay to mesh peers (bootstrap and connected clients)
    %% This ensures local publishes reach remote subscribers via bootstrap
    relay_to_mesh_peers(State, Topic, Payload),

    {noreply, State};

%% @doc Handle message routed from QUIC server (async) - v0.12.0 format with peer address.
%% Routes decoded QUIC messages to appropriate business logic handlers.
%% PeerAddr is captured at receive time for NAT detection (stream may be closed by now).
handle_cast({route_message, MessageType, Message, Stream, PeerAddr}, State) ->
    ?LOG_DEBUG("Routing message type ~p from QUIC server (async)", [MessageType]),
    %% Store peer address in process dictionary for NAT probe handling
    %% This allows handle_nat_probe to use the address even if stream is closed
    put(current_peer_addr, PeerAddr),
    %% Use existing handle_decoded_message logic
    Result = handle_decoded_message({ok, {MessageType, Message}}, Stream, State),
    %% Extract new state from {noreply, NewState} tuple
    NewState = element(2, Result),
    erase(current_peer_addr),
    {noreply, NewState};

%% @doc Handle message routed from QUIC server (async) - legacy format without peer address.
%% For backward compatibility with older QUIC server versions.
handle_cast({route_message, MessageType, Message, Stream}, State) ->
    ?LOG_DEBUG("Routing message type ~p from QUIC server (async, legacy)", [MessageType]),
    %% Use existing handle_decoded_message logic
    Result = handle_decoded_message({ok, {MessageType, Message}}, Stream, State),
    %% Extract new state from {noreply, NewState} tuple
    NewState = element(2, Result),
    {noreply, NewState};

%% Async local publish - fire-and-forget from local client
handle_cast({local_publish_async, _Realm, Topic, Payload}, State) ->
    ?LOG_DEBUG("Local publish async: ~s", [Topic]),
    %% Distribute to subscribers asynchronously
    gen_server:cast(self(), {distribute_publish, Topic, Payload}),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%%%===================================================================
%%% Continue Callbacks (Post-Init Wiring)
%%%===================================================================

%% @doc Wire gateway to siblings after init completes.
%% This avoids initialization deadlock from calling supervisor:which_children/1
%% during init/1 before supervisor has finished starting all children.
handle_continue(wire_siblings, State) ->
    ?LOG_DEBUG("handle_continue(wire_siblings) called"),
    case wire_siblings(State) of
        #state{} = NewState ->
            ?LOG_DEBUG("Sibling wiring completed successfully"),
            {noreply, NewState};
        {stop, Reason} ->
            ?LOG_ERROR("Sibling wiring failed: ~p", [Reason]),
            {stop, Reason, State}
    end.

%%%===================================================================
%%% Info Callbacks
%%%===================================================================

%% Handle new stream created by peer (quicer message)
%% Client connected - delegate to clients module
handle_info({client_connected, ClientPid, ClientInfo}, State) ->
    ?LOG_INFO("Client connected: ~p (delegating to clients)", [ClientInfo]),
    ClientMgr = State#state.client_manager,
    ok = macula_gateway_clients:client_connected(ClientMgr, ClientPid, ClientInfo),
    {noreply, State};

%% Client disconnected - delegate cleanup to all child modules
handle_info({'DOWN', _Ref, process, ClientPid, _Reason}, State) ->
    ?LOG_INFO("Client disconnected: ~p (delegating cleanup)", [ClientPid]),

    ClientMgr = State#state.client_manager,
    PubSub = State#state.pubsub,
    Rpc = State#state.rpc,

    %% Clients module handles client removal
    macula_gateway_clients:client_disconnected(ClientMgr, ClientPid),

    %% Clean up subscriptions in pubsub
    case macula_gateway_pubsub:get_stream_topics(PubSub, ClientPid) of
        {ok, Topics} ->
            lists:foreach(fun(Topic) ->
                macula_gateway_pubsub:unsubscribe(PubSub, ClientPid, Topic)
            end, Topics);
        not_found ->
            ok
    end,

    %% Clean up RPC registrations
    {ok, Handlers} = macula_gateway_rpc:list_handlers(Rpc),
    lists:foreach(fun
        ({Proc, HandlerPid}) when HandlerPid =:= ClientPid ->
            macula_gateway_rpc:unregister_handler(Rpc, Proc);
        (_) ->
            ok
    end, Handlers),

    {noreply, State};

%% DHT query (find_node, find_value, store)
handle_info({dht_query, FromPid, QueryType, QueryData}, State) ->
    %% Delegate to DHT module
    _Result = macula_gateway_dht:handle_query(FromPid, QueryType, QueryData),
    {noreply, State};

handle_info(Info, State) ->
    ?LOG_WARNING("Unhandled handle_info message: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Gateway is now supervised - parent supervisor will handle cleanup
    %% No need to manually stop QUIC server or workers supervisor
    ?LOG_INFO("Shutting down (supervised mode)"),
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Handle CONNECT message from client.
handle_connect(Stream, ConnectMsg, #state{realm = Realm} = State) ->
    RealmId = maps:get(<<"realm_id">>, ConnectMsg),
    handle_connect_realm(RealmId =:= Realm, Stream, ConnectMsg, State).

%% @doc Process CONNECT with valid realm (pattern matching on boolean).
handle_connect_realm(true, Stream, ConnectMsg, State) ->
    RealmId = maps:get(<<"realm_id">>, ConnectMsg),
    NodeId = maps:get(<<"node_id">>, ConnectMsg),
    %% Extract endpoint from CONNECT message for peer-to-peer connections
    Endpoint = maps:get(<<"endpoint">>, ConnectMsg, undefined),

    _ClientInfo = #{
        realm => RealmId,
        node_id => NodeId,
        capabilities => maps:get(<<"capabilities">>, ConnectMsg, []),
        endpoint => Endpoint
    },
    ?LOG_WARNING("[Gateway] Client CONNECT: node_id=~s (raw: ~p), endpoint=~s",
                [binary:encode_hex(NodeId), NodeId, Endpoint]),

    %% HTTP/3 streams are bidirectional - we can send messages back on the same stream
    %% Store the client's incoming stream in CLIENT MANAGER for routing (enables client-only mode)
    ok = macula_gateway_clients:store_client_stream(State#state.client_manager, NodeId, Stream, Endpoint),
    ?LOG_WARNING("[Gateway] STORED client stream for node_id=~s (raw: ~p)",
                [binary:encode_hex(NodeId), NodeId]),

    %% Connection management now handled by macula_gateway_mesh module
    NewState = State,

    %% Add peer to DHT routing table with endpoint (binary string, not parsed tuple)
    %% IMPORTANT: DHT must store serializable values - binary strings, not tuples
    NodeInfo = #{
        node_id => NodeId,
        address => Endpoint  % Use binary endpoint string (msgpack can serialize this)
    },
    case whereis(macula_routing_server) of
        undefined ->
            ?LOG_WARNING("Routing server not running, cannot add peer");
        RoutingServerPid ->
            ?LOG_DEBUG("Adding peer to routing table: ~p", [NodeId]),
            macula_routing_server:add_node(RoutingServerPid, NodeInfo),
            ?LOG_DEBUG("Peer added to routing table")
    end,

    %% Send PONG acknowledgment back to keep stream alive for bidirectional communication
    PongMsg = #{
        timestamp => erlang:system_time(millisecond),
        server_time => erlang:system_time(millisecond)
    },
    PongBinary = macula_protocol_encoder:encode(pong, PongMsg),
    case macula_quic:send(Stream, PongBinary) of
        ok ->
            ?LOG_DEBUG("Sent PONG acknowledgment to client ~p",
                     [binary:encode_hex(NodeId)]);
        {error, PongErr} ->
            ?LOG_WARNING("Failed to send PONG: ~p", [PongErr])
    end,

    {noreply, NewState};
handle_connect_realm(false, Stream, ConnectMsg, State) ->
    RealmId = maps:get(<<"realm_id">>, ConnectMsg),
    ?LOG_WARNING("Realm mismatch: ~p != ~p", [RealmId, State#state.realm]),
    macula_quic:close(Stream),
    {noreply, State}.

%% @doc Handle DHT STORE message.
handle_dht_store(Stream, StoreMsg, State) ->
    %% Delegate to DHT module
    _Result = macula_gateway_dht:handle_store(Stream, StoreMsg),
    {noreply, State}.

%% @doc Handle DHT FIND_VALUE message.
handle_dht_find_value(Stream, FindValueMsg, State) ->
    %% Delegate to DHT module
    _Result = macula_gateway_dht:handle_find_value(Stream, FindValueMsg),
    {noreply, State}.

%% @doc Handle DHT FIND_NODE message.
handle_dht_find_node(Stream, FindNodeMsg, State) ->
    %% Delegate to DHT module
    _Result = macula_gateway_dht:handle_find_node(Stream, FindNodeMsg),
    {noreply, State}.

%% @doc Handle RPC call message received via DHT routing.
%% Processes the call locally and sends REPLY back via DHT routing.
handle_rpc_call_routed(_Stream, CallMsg, RpcRouteMsg, State) ->
    %% Delegate to RPC router module
    _Result = macula_gateway_rpc_router:handle_routed_call(
        CallMsg, RpcRouteMsg, State#state.node_id, State#state.rpc, State#state.mesh
    ),
    {noreply, State}.

%% @doc Handle routed REPLY message delivered locally.
%% Forward the REPLY to the local connection process.
handle_rpc_reply_routed(ReplyMsg, RpcRouteMsg, State) ->
    %% Delegate to RPC router module
    _Result = macula_gateway_rpc_router:handle_routed_reply(
        ReplyMsg, RpcRouteMsg, State#state.node_id, State#state.client_streams
    ),
    {noreply, State}.

%% @doc Handle routed PUBLISH message delivered locally.
%% Deliver to local pub/sub subscribers.
handle_pubsub_route_deliver(PublishMsg, State) ->
    %% Extract topic from publish message
    Topic = maps:get(<<"topic">>, PublishMsg),
    Payload = maps:get(<<"payload">>, PublishMsg),

    ?LOG_DEBUG("Delivering routed PUBLISH to topic ~p", [Topic]),

    %% Publish to local subscribers (delegate to pubsub module)
    macula_gateway_pubsub:publish(State#state.pubsub, Topic, Payload),
    {noreply, State}.

%% @doc Forward pubsub_route message to next hop through mesh.
%% Uses async (fire-and-forget) pattern to avoid blocking gateway.
%% Graceful error handling - logs errors but doesn't crash gateway.
forward_pubsub_route(NextHopNodeInfo, PubSubRouteMsg, MeshPid) ->
    ?LOG_DEBUG("Forwarding pubsub_route to next hop (async)"),

    %% Extract next hop info (routing_bucket:node_info uses atom keys, not binary)
    #{node_id := NextHopNodeId,
      address := Address} = NextHopNodeInfo,

    %% Encode message
    EncodedMsg = macula_protocol_encoder:encode(pubsub_route, PubSubRouteMsg),

    %% Send asynchronously - does NOT block the gateway
    %% Connection creation and sending happens in a spawned process
    macula_gateway_mesh:send_async(MeshPid, NextHopNodeId, Address, EncodedMsg),
    ?LOG_DEBUG("Queued pubsub_route for async send to ~s",
             [binary:encode_hex(NextHopNodeId)]),
    ok.

%% @doc Relay pub/sub event to all connected mesh peers.
%% This enables cross-peer event propagation for distributed applications.
relay_to_mesh_peers(State, Topic, Payload) ->
    %% Strategy: Relay via TWO mechanisms to form a mesh:
    %% 1. Broadcast to connected CLIENTS (applications/gateways connected TO this gateway)
    %% 2. Forward to connected BOOTSTRAP PEERS (gateways this gateway connected TO)

    %% 1. Broadcast to connected clients (existing implementation)
    PublishMsg = #{
        <<"topic">> => Topic,
        <<"payload">> => Payload,
        <<"qos">> => 0,
        <<"retain">> => false,
        <<"message_id">> => erlang:unique_integer([positive])
    },

    case macula_protocol_encoder:encode(publish, PublishMsg) of
        {error, Reason} ->
            ?LOG_ERROR("Failed to encode publish message: ~p", [Reason]),
            ok;
        EncodedMsg ->
            %% Broadcast to all connected clients (applications/child gateways)
            ClientManager = State#state.client_manager,
            macula_gateway_clients:broadcast(ClientManager, EncodedMsg),
            ?LOG_DEBUG("Broadcasted ~s to all clients", [Topic]),
            ok
    end,

    %% 2. Forward to bootstrap peers (parent gateways)
    %% Get peer system supervisors
    PeerSystems = macula_peers_sup:list_peers(),
    ?LOG_DEBUG("Peer systems: ~p", [PeerSystems]),

    %% Extract connection PIDs from each peer system supervisor
    ConnectionPids = lists:filtermap(fun(PeerSup) ->
        case supervisor:which_children(PeerSup) of
            Children when is_list(Children) ->
                %% Find the connection_manager PID (macula_connection)
                case lists:keyfind(connection_manager, 1, Children) of
                    {connection_manager, ConnPid, _Type, _Modules} when is_pid(ConnPid) ->
                        {true, ConnPid};
                    _ ->
                        false
                end;
            _ ->
                false
        end
    end, PeerSystems),

    ?LOG_DEBUG("Bootstrap connection PIDs: ~p", [ConnectionPids]),

    case ConnectionPids of
        [] ->
            ?LOG_DEBUG("No bootstrap peers to forward to"),
            ok;
        Connections ->
            %% Build publish message for forwarding
            ForwardMsg = #{
                <<"topic">> => Topic,
                <<"payload">> => Payload,
                <<"qos">> => 0,
                <<"retain">> => false,
                <<"message_id">> => erlang:unique_integer([positive])
            },

            %% Forward to each bootstrap peer via macula_connection:send_message
            lists:foreach(fun(ConnPid) ->
                case macula_connection:send_message(ConnPid, publish, ForwardMsg) of
                    ok ->
                        ?LOG_DEBUG("Forwarded ~s to bootstrap peer ~p", [Topic, ConnPid]);
                    {error, PeerReason} ->
                        ?LOG_WARNING("Failed to forward ~s to peer ~p: ~p",
                                 [Topic, ConnPid, PeerReason])
                end
            end, Connections),
            ?LOG_DEBUG("Forwarded ~s to ~p bootstrap peer(s)", [Topic, length(Connections)])
    end,
    ok.


%% @doc Handle RPC call message (legacy direct handling).
handle_rpc_call(Stream, CallMsg, State) ->
    Procedure = maps:get(<<"procedure">>, CallMsg),
    CallId = maps:get(<<"call_id">>, CallMsg),
    ?LOG_DEBUG("[Gateway] handle_rpc_call procedure=~p call_id=~p", [Procedure, CallId]),

    %% Special exception: handle DHT-related procedures
    case Procedure of
        <<"_dht.list_gateways">> ->
            ?LOG_DEBUG("[Gateway] handling _dht.list_gateways"),
            handle_dht_list_gateways(Stream, CallId, State);
        <<"_dht.store">> ->
            ?LOG_DEBUG("[Gateway] handling _dht.store"),
            handle_dht_store(Stream, CallId, CallMsg, State);
        _ ->
            ?LOG_WARNING("BOOTSTRAP-ONLY MODE: Rejecting RPC call to ~s (peers must communicate directly via DHT)", [Procedure]),

            %% Gateway is BOOTSTRAP-ONLY - it does NOT handle RPC calls!
            %% Peers must discover service providers via DHT and call them directly
            ErrorReply = #{
                call_id => CallId,
                error => #{
                    code => <<"gateway_bootstrap_only">>,
                    message => <<"Gateway is bootstrap-only. Discover service via DHT and call peer directly">>
                }
            },
            ReplyBinary = macula_protocol_encoder:encode(reply, ErrorReply),
            macula_quic:send(Stream, ReplyBinary),
            {noreply, State}
    end.

%% @private Handle _dht.list_gateways RPC for peer discovery
handle_dht_list_gateways(Stream, CallId, _State) ->
    ?LOG_DEBUG("Handling _dht.list_gateways RPC"),

    %% Query local DHT for all peer.gateway.* entries
    case whereis(macula_routing_server) of
        undefined ->
            ErrorReply = #{
                call_id => CallId,
                error => #{
                    code => <<"routing_server_not_found">>,
                    message => <<"Routing server not available">>
                }
            },
            ReplyBinary = macula_protocol_encoder:encode(reply, ErrorReply),
            macula_quic:send(Stream, ReplyBinary);
        RoutingServer ->
            %% Get all keys from DHT
            PeersList = case macula_routing_server:get_all_keys(RoutingServer) of
                {ok, Keys} ->
                    %% Filter for peer.gateway.* keys
                    GatewayKeys = lists:filter(fun(Key) ->
                        case Key of
                            <<"peer.gateway.", _/binary>> -> true;
                            _ -> false
                        end
                    end, Keys),

                    %% Get values for each gateway key
                    lists:filtermap(fun(Key) ->
                        case macula_routing_server:get_local(RoutingServer, Key) of
                            {ok, [Value|_]} -> {true, Value};
                            {ok, []} -> false;
                            _ -> false
                        end
                    end, GatewayKeys);
                _ ->
                    []
            end,

            ?LOG_INFO("[Gateway] Found ~p gateways from DHT", [length(PeersList)]),

            %% Send successful reply with peers list
            SuccessReply = #{
                call_id => CallId,
                result => #{
                    <<"peers">> => PeersList
                }
            },
            ReplyBinary = macula_protocol_encoder:encode(reply, SuccessReply),
            ?LOG_DEBUG("[Gateway] Sending reply (~p bytes) to Stream", [byte_size(ReplyBinary)]),
            SendResult = macula_quic:send(Stream, ReplyBinary),
            ?LOG_DEBUG("[Gateway] send result = ~p", [SendResult]),
            SendResult
    end,
    {noreply, _State}.

%% @private Handle _dht.store RPC for DHT sync from embedded gateways
handle_dht_store(Stream, CallId, CallMsg, _State) ->
    ?LOG_DEBUG("Handling _dht.store RPC"),

    %% Extract key and value from args
    Args = maps:get(<<"args">>, CallMsg, #{}),
    Key = maps:get(<<"key">>, Args, undefined),
    Value = maps:get(<<"value">>, Args, undefined),

    %% Validate parameters
    case {Key, Value} of
        {undefined, _} ->
            ErrorReply = #{
                call_id => CallId,
                error => #{
                    code => <<"invalid_args">>,
                    message => <<"Missing required parameter: key">>
                }
            },
            ReplyBinary = macula_protocol_encoder:encode(reply, ErrorReply),
            macula_quic:send(Stream, ReplyBinary);
        {_, undefined} ->
            ErrorReply = #{
                call_id => CallId,
                error => #{
                    code => <<"invalid_args">>,
                    message => <<"Missing required parameter: value">>
                }
            },
            ReplyBinary = macula_protocol_encoder:encode(reply, ErrorReply),
            macula_quic:send(Stream, ReplyBinary);
        {_, _} ->
            %% Forward to local routing server to store in DHT
            case whereis(macula_routing_server) of
                undefined ->
                    ErrorReply = #{
                        call_id => CallId,
                        error => #{
                            code => <<"routing_server_not_found">>,
                            message => <<"Routing server not available">>
                        }
                    },
                    ReplyBinary = macula_protocol_encoder:encode(reply, ErrorReply),
                    macula_quic:send(Stream, ReplyBinary);
                RoutingServer ->
                    ?LOG_DEBUG("Storing in DHT: key=~p", [Key]),
                    %% Store in DHT (will propagate to k-closest nodes)
                    gen_server:cast(RoutingServer, {store, Key, Value}),

                    %% Send success reply
                    SuccessReply = #{
                        call_id => CallId,
                        result => #{
                            <<"status">> => <<"ok">>
                        }
                    },
                    ReplyBinary = macula_protocol_encoder:encode(reply, SuccessReply),
                    macula_quic:send(Stream, ReplyBinary)
            end
    end,
    {noreply, _State}.

%% REMOVED: encode_json/1 - no longer needed since gateway is bootstrap-only
%% (was used for RPC reply encoding)

%%%===================================================================
%%% NATS-Style Async RPC Handlers
%%%===================================================================

%% @doc Handle incoming NATS-style async RPC request.
%% If target_node is specified and different from local, forward to target peer.
%% Otherwise, looks up local handler, executes it, and sends reply back to requester.
%% ReplyStream is the stream to use for replies - either the incoming stream
%% (for local handling) or the client's stream (for forwarding).
handle_rpc_request(ReplyStream, RequestMsg, State) ->
    RequestId = maps:get(<<"request_id">>, RequestMsg),
    Procedure = maps:get(<<"procedure">>, RequestMsg),
    Args = maps:get(<<"args">>, RequestMsg, <<>>),
    FromNode = maps:get(<<"from_node">>, RequestMsg),
    FromEndpoint = maps:get(<<"from_endpoint">>, RequestMsg, undefined),
    TargetNode = maps:get(<<"target_node">>, RequestMsg, maps:get(target_node, RequestMsg, undefined)),

    ?LOG_DEBUG("[Gateway] Handling RPC_REQUEST: procedure=~s, request_id=~s, from=~s, target=~p",
              [Procedure, binary:encode_hex(RequestId), binary:encode_hex(FromNode),
               case TargetNode of undefined -> <<"local">>; T -> binary:encode_hex(T) end]),

    %% Check if this request should be forwarded to a remote peer
    LocalNodeId = State#state.node_id,
    case should_forward_request(TargetNode, LocalNodeId) of
        true ->
            %% Forward to target peer via mesh
            forward_rpc_request_to_peer(RequestMsg, TargetNode, State);
        false ->
            %% Handle locally - reply goes back through the incoming stream
            handle_local_rpc_request(ReplyStream, RequestMsg, RequestId, Procedure, Args, FromNode, FromEndpoint, State)
    end.

%% @private Determine if request should be forwarded to remote peer
should_forward_request(undefined, _LocalNodeId) -> false;
should_forward_request(TargetNode, LocalNodeId) when TargetNode =:= LocalNodeId -> false;
should_forward_request(_TargetNode, _LocalNodeId) -> true.

%% @private Forward RPC request to target peer via connected client streams.
%% The gateway acts as a relay - peers connect to it, so we can forward through
%% the existing QUIC streams instead of trying to make new P2P connections.
forward_rpc_request_to_peer(RequestMsg, TargetNodeId, State) ->
    TargetNodeIdHex = binary:encode_hex(TargetNodeId),
    RequestId = maps:get(<<"request_id">>, RequestMsg),
    ReqIdHex = binary:encode_hex(RequestId),
    T_gw_recv = erlang:system_time(millisecond),
    T_orig = maps:get(<<"timestamp">>, RequestMsg, maps:get(timestamp, RequestMsg, 0)),
    ?LOG_WARNING("[TIMING ~s] GW_REQ_RECV: T=~p (orig_ts=~p, transit=~pms)",
                [ReqIdHex, T_gw_recv, T_orig, T_gw_recv - T_orig]),

    %% First try the client_manager's stream lookup (peers that have connected to us)
    ClientMgr = State#state.client_manager,

    %% Debug: Log all stored node IDs (raw binaries, not hex-encoded)
    AllNodeIds = macula_gateway_clients:get_all_node_ids(ClientMgr),
    ?LOG_DEBUG("[Gateway] DEBUG: Looking for target=~p (~s), stored_nodes=~p (count=~p)",
                 [TargetNodeId, TargetNodeIdHex, AllNodeIds, length(AllNodeIds)]),

    case lookup_client_stream(ClientMgr, TargetNodeId) of
        {ok, StreamPid} ->
            %% Forward through the connected stream
            forward_via_stream(StreamPid, RequestMsg, TargetNodeId, TargetNodeIdHex, ReqIdHex, ClientMgr);
        not_found ->
            %% Try the local client_streams map as fallback
            case maps:get(TargetNodeId, State#state.client_streams, undefined) of
                undefined ->
                    ?LOG_WARNING("[TIMING ~s] GW_TARGET_NOT_FOUND", [ReqIdHex]),
                    send_error_reply(RequestMsg, <<"target_peer_not_found">>, State);
                StreamPid2 ->
                    forward_via_stream(StreamPid2, RequestMsg, TargetNodeId, TargetNodeIdHex, ReqIdHex, ClientMgr)
            end
    end,
    {noreply, State}.

%% @private Look up client stream from client manager
lookup_client_stream(undefined, _TargetNodeId) ->
    not_found;
lookup_client_stream(ClientMgr, TargetNodeId) ->
    macula_gateway_clients:get_client_stream(ClientMgr, TargetNodeId).

%% @private Forward message through an existing QUIC stream.
%% On send failure (e.g., 'closed'), removes the stale stream from client manager
%% so clients can reconnect with fresh streams.
forward_via_stream(StreamPid, RequestMsg, TargetNodeId, TargetNodeIdHex, ReqIdHex, ClientMgr) ->
    %% Encode the message using Macula protocol
    EncodedMsg = macula_protocol_encoder:encode(rpc_request, RequestMsg),
    T_fwd = erlang:system_time(millisecond),
    ?LOG_WARNING("[TIMING ~s] GW_REQ_FWD: T=~p to ~s (~p bytes)",
                 [ReqIdHex, T_fwd, TargetNodeIdHex, byte_size(EncodedMsg)]),
    case quicer:send(StreamPid, EncodedMsg) of
        {ok, _} ->
            T_sent = erlang:system_time(millisecond),
            ?LOG_WARNING("[TIMING ~s] GW_REQ_SENT: T=~p (send_time=~pms)",
                        [ReqIdHex, T_sent, T_sent - T_fwd]);
        {error, Reason} ->
            ?LOG_WARNING("[TIMING ~s] GW_REQ_SEND_FAILED: ~p", [ReqIdHex, Reason]),
            %% Clean up stale stream so client can reconnect
            cleanup_stale_stream(ClientMgr, TargetNodeId, TargetNodeIdHex, Reason)
    end.

%% @private Send error reply back to requester via their client stream.
%% Uses the stored client stream instead of trying to create a new connection
%% (which would fail for NAT-ed clients).
send_error_reply(RequestMsg, ErrorReason, State) ->
    RequestId = maps:get(<<"request_id">>, RequestMsg),
    FromNode = maps:get(<<"from_node">>, RequestMsg),

    ReplyMsg = #{
        <<"type">> => <<"rpc_reply">>,
        <<"request_id">> => RequestId,
        <<"error">> => ErrorReason,
        <<"from_node">> => State#state.node_id,
        <<"timestamp">> => erlang:system_time(millisecond)
    },

    %% Look up the requester's client stream and send reply through it
    send_reply_via_client_stream(FromNode, ReplyMsg, State).

%% @private Handle RPC request locally (execute handler)
%% ReplyStream is the QUIC stream to send the reply through.
%% This is critical for cross-NAT relay: the target peer doesn't have the requester's
%% client stream, so we MUST send the reply back through the incoming stream.
handle_local_rpc_request(ReplyStream, _RequestMsg, RequestId, Procedure, Args, FromNode, _FromEndpoint, State) ->
    %% Look up local handler from peer system's RPC handler
    %% (NOT the gateway's RPC child - that's for gateway-specific handlers)
    %% Workload apps like macula_ping_pong register handlers with the peer's macula_rpc_handler
    Result = case get_rpc_handler_from_peers() of
        {error, Reason} ->
            ?LOG_DEBUG("[Gateway] No peer RPC handler: ~p", [Reason]),
            {error, <<"rpc_handler_not_available">>};
        {ok, RpcPid} ->
            %% Get service registry from RPC handler
            case macula_rpc_handler:get_service_registry(RpcPid) of
                {error, RegistryError} ->
                    ?LOG_DEBUG("[Gateway] Registry error: ~p", [RegistryError]),
                    {error, iolist_to_binary(io_lib:format("registry_error: ~p", [RegistryError]))};
                Registry ->
                    %% Lookup and execute local handler via registry
                    case macula_service_registry:get_local_handler(Registry, Procedure) of
                        {ok, Handler} ->
                            ?LOG_DEBUG("[Gateway] Found handler for ~s, executing", [Procedure]),
                            %% Execute handler
                            try
                                DecodedArgs = try macula_utils:decode_json(Args) catch _:_ -> Args end,
                                Handler(DecodedArgs)
                            catch
                                _:Error -> {error, iolist_to_binary(io_lib:format("~p", [Error]))}
                            end;
                        not_found ->
                            ?LOG_DEBUG("[Gateway] Handler not found for ~s", [Procedure]),
                            {error, <<"procedure_not_found">>}
                    end
            end
    end,

    %% Build reply message
    ReplyMsg = case Result of
        {ok, ResultValue} ->
            EncodedResult = try macula_utils:encode_json(ResultValue) catch _:_ -> ResultValue end,
            #{
                request_id => RequestId,
                result => EncodedResult,
                from_node => State#state.node_id,
                timestamp => erlang:system_time(millisecond)
            };
        {error, ErrorReason} ->
            #{
                request_id => RequestId,
                error => ErrorReason,
                from_node => State#state.node_id,
                timestamp => erlang:system_time(millisecond)
            }
    end,

    ?LOG_DEBUG("[Gateway] Handler result=~p", [Result]),

    %% Send reply directly through the incoming ReplyStream
    %% This is the ONLY way to reach NAT-ed requesters: we send through the same
    %% stream they used to send the request. The target peer does NOT have the
    %% requester's client stream stored - only the bootstrap has that.
    ?LOG_DEBUG("[Gateway] Sending RPC_REPLY back to ~s via incoming ReplyStream",
              [binary:encode_hex(FromNode)]),

    EncodedReply = macula_protocol_encoder:encode(rpc_reply, ReplyMsg),
    case quicer:send(ReplyStream, EncodedReply) of
        {ok, _} ->
            ?LOG_DEBUG("[Gateway] Successfully sent RPC_REPLY to ~s", [binary:encode_hex(FromNode)]);
        {error, SendError} ->
            ?LOG_WARNING("[Gateway] Failed to send RPC_REPLY to ~s: ~p",
                        [binary:encode_hex(FromNode), SendError])
    end,
    {noreply, State}.

%% @private Send reply via client stream (for cross-NAT relay)
send_reply_via_client_stream(FromNode, ReplyMsg, State) ->
    ClientMgr = State#state.client_manager,
    FromNodeHex = binary:encode_hex(FromNode),
    case lookup_client_stream(ClientMgr, FromNode) of
        {ok, StreamPid} ->
            send_reply_to_stream(StreamPid, ReplyMsg, FromNodeHex);
        not_found ->
            %% Also check local client_streams map
            case maps:get(FromNode, State#state.client_streams, undefined) of
                undefined ->
                    ?LOG_WARNING("[Gateway] Cannot find stream for reply target ~s", [FromNodeHex]);
                StreamPid2 ->
                    send_reply_to_stream(StreamPid2, ReplyMsg, FromNodeHex)
            end
    end.

%% @private Send an RPC reply through a stream
send_reply_to_stream(StreamPid, ReplyMsg, FromNodeHex) ->
    EncodedReply = macula_protocol_encoder:encode(rpc_reply, ReplyMsg),
    case quicer:send(StreamPid, EncodedReply) of
        {ok, _} ->
            ?LOG_DEBUG("[Gateway] Sent RPC_REPLY to ~s via client stream", [FromNodeHex]);
        {error, Reason} ->
            ?LOG_WARNING("[Gateway] Failed to send RPC_REPLY to ~s: ~p", [FromNodeHex, Reason])
    end.

%% @doc Handle incoming NATS-style async RPC reply.
%% Two cases:
%% 1. Reply has to_node - this is a relayed reply that needs to be forwarded to the original requester
%% 2. No to_node - this is a local reply, forward to peer's RPC handler for callback invocation
handle_rpc_reply(ReplyMsg, State) ->
    RequestId = maps:get(<<"request_id">>, ReplyMsg, maps:get(request_id, ReplyMsg, undefined)),
    RequestIdHex = case RequestId of undefined -> <<"undefined">>; _ -> binary:encode_hex(RequestId) end,
    ToNode = maps:get(<<"to_node">>, ReplyMsg, maps:get(to_node, ReplyMsg, undefined)),
    T_gw_reply = erlang:system_time(millisecond),
    T_orig = maps:get(<<"timestamp">>, ReplyMsg, maps:get(timestamp, ReplyMsg, 0)),
    ?LOG_WARNING("[TIMING ~s] GW_REPLY_RECV: T=~p (orig_ts=~p, transit=~pms)",
                [RequestIdHex, T_gw_reply, T_orig, T_gw_reply - T_orig]),

    case ToNode of
        undefined ->
            %% Local reply - forward to this node's RPC handler
            ?LOG_WARNING("[TIMING ~s] GW_REPLY_LOCAL", [RequestIdHex]),
            handle_local_rpc_reply(ReplyMsg);
        ToNode ->
            %% Relayed reply - forward to original requester via their client stream
            ?LOG_WARNING("[TIMING ~s] GW_REPLY_RELAY to ~s", [RequestIdHex, binary:encode_hex(ToNode)]),
            handle_relayed_rpc_reply(ToNode, ReplyMsg, State, RequestIdHex)
    end,
    {noreply, State}.

%% @private Handle local RPC reply - forward to peer's RPC handler
handle_local_rpc_reply(ReplyMsg) ->
    case get_rpc_handler_from_peers() of
        {ok, RpcPid} ->
            ?LOG_DEBUG("[Gateway] Forwarding local RPC_REPLY to peer RPC handler ~p", [RpcPid]),
            macula_rpc_handler:handle_async_reply(RpcPid, ReplyMsg);
        {error, Reason} ->
            ?LOG_WARNING("[Gateway] RPC_REPLY received but no peer RPC handler: ~p", [Reason])
    end.

%% @private Handle relayed RPC reply - forward to original requester via their client stream
handle_relayed_rpc_reply(ToNode, ReplyMsg, State, ReqIdHex) ->
    ToNodeHex = binary:encode_hex(ToNode),

    %% Look up the requester's client stream
    ClientMgr = State#state.client_manager,
    case lookup_client_stream(ClientMgr, ToNode) of
        {ok, StreamPid} ->
            ?LOG_WARNING("[TIMING ~s] GW_RELAY_FOUND stream for ~s", [ReqIdHex, ToNodeHex]),
            forward_rpc_reply_via_stream(StreamPid, ReplyMsg, ToNode, ToNodeHex, ReqIdHex, ClientMgr);
        not_found ->
            %% Also check local client_streams map
            case maps:get(ToNode, State#state.client_streams, undefined) of
                undefined ->
                    ?LOG_WARNING("[TIMING ~s] GW_RELAY_NOT_FOUND for ~s", [ReqIdHex, ToNodeHex]);
                StreamPid2 ->
                    ?LOG_WARNING("[TIMING ~s] GW_RELAY_FOUND_LOCAL stream for ~s", [ReqIdHex, ToNodeHex]),
                    forward_rpc_reply_via_stream(StreamPid2, ReplyMsg, ToNode, ToNodeHex, ReqIdHex, ClientMgr)
            end
    end.

%% @private Forward RPC reply through a client stream.
%% On send failure (e.g., 'closed'), removes the stale stream from client manager.
forward_rpc_reply_via_stream(StreamPid, ReplyMsg, ToNode, ToNodeHex, ReqIdHex, ClientMgr) ->
    T_fwd = erlang:system_time(millisecond),
    EncodedReply = macula_protocol_encoder:encode(rpc_reply, ReplyMsg),
    ?LOG_WARNING("[TIMING ~s] GW_REPLY_FWD: T=~p to ~s (~p bytes)",
                 [ReqIdHex, T_fwd, ToNodeHex, byte_size(EncodedReply)]),
    case quicer:send(StreamPid, EncodedReply) of
        {ok, BytesSent} ->
            T_sent = erlang:system_time(millisecond),
            ?LOG_WARNING("[TIMING ~s] GW_REPLY_SENT: T=~p (send_time=~pms, bytes=~p)",
                        [ReqIdHex, T_sent, T_sent - T_fwd, BytesSent]);
        {error, Reason} ->
            ?LOG_WARNING("[TIMING ~s] GW_REPLY_SEND_FAILED: ~p", [ReqIdHex, Reason]),
            %% Clean up stale stream so client can reconnect
            cleanup_stale_stream(ClientMgr, ToNode, ToNodeHex, Reason)
    end.

%% @private Clean up stale stream from client manager on send failure.
%% Only cleans up for connection-related errors (closed, stream_closed, etc.)
cleanup_stale_stream(undefined, _NodeId, _NodeIdHex, _Reason) ->
    ok;  %% No client manager - nothing to clean up
cleanup_stale_stream(ClientMgr, NodeId, NodeIdHex, Reason) when
        Reason =:= closed;
        Reason =:= stream_closed;
        Reason =:= connection_closed;
        Reason =:= stm_send_error ->
    ?LOG_WARNING("[Gateway] Cleaning up stale stream for ~s (reason: ~p)", [NodeIdHex, Reason]),
    macula_gateway_clients:remove_stale_stream(ClientMgr, NodeId);
cleanup_stale_stream(_ClientMgr, NodeIdHex, _NodeId, Reason) ->
    %% Non-connection error - don't remove stream (might be temporary)
    ?LOG_DEBUG("[Gateway] Send failed for ~s but not cleaning up (reason: ~p)", [NodeIdHex, Reason]),
    ok.

%%%===================================================================
%%% Mesh Connection Management
%%%===================================================================

%% Get or create a QUIC connection to a peer node for message forwarding.
%% Opens a NEW stream for each message (QUIC best practice).
%% Handle successful connection acceptance.
%% Register for incoming streams with active mode enabled.
%% Manual accept functions removed - quicer_server handles connections automatically

%% @doc Handle decoded protocol messages.
%% Dispatches to appropriate handlers based on message type.
handle_decoded_message({ok, {connect, ConnectMsg}}, Stream, State) ->
    ?LOG_DEBUG("Decoded CONNECT message"),
    handle_connect(Stream, ConnectMsg, State);

%% Handle decoded STORE message.
handle_decoded_message({ok, {store, StoreMsg}}, Stream, State) ->
    ?LOG_DEBUG("Received STORE message"),
    ?LOG_DEBUG("STORE message: ~p", [StoreMsg]),
    handle_dht_store(Stream, StoreMsg, State);

%% Handle decoded FIND_VALUE message.
handle_decoded_message({ok, {find_value, FindValueMsg}}, Stream, State) ->
    ?LOG_DEBUG("Received FIND_VALUE message"),
    ?LOG_DEBUG("FIND_VALUE message: ~p", [FindValueMsg]),
    handle_dht_find_value(Stream, FindValueMsg, State);

%% Handle decoded FIND_NODE message.
handle_decoded_message({ok, {find_node, FindNodeMsg}}, Stream, State) ->
    ?LOG_DEBUG("Received FIND_NODE message"),
    ?LOG_DEBUG("FIND_NODE message: ~p", [FindNodeMsg]),
    handle_dht_find_node(Stream, FindNodeMsg, State);

%% Handle RPC route message (multi-hop DHT routing).
handle_decoded_message({ok, {rpc_route, RpcRouteMsg}}, Stream, State) ->
    ?LOG_DEBUG("Received RPC_ROUTE message"),
    ?LOG_DEBUG("RPC route message: ~p", [RpcRouteMsg]),

    LocalNodeId = State#state.node_id,
    RoutingServerPid = whereis(macula_routing_server),

    case macula_rpc_routing:route_or_deliver(LocalNodeId, RpcRouteMsg, RoutingServerPid) of
        {deliver, <<"call">>, CallMsg} ->
            %% Unwrap and process CALL locally (MessagePack returns binary, not atom)
            ?LOG_DEBUG("RPC route: delivering CALL locally"),
            handle_rpc_call_routed(Stream, CallMsg, RpcRouteMsg, State);

        {deliver, <<"reply">>, ReplyMsg} ->
            %% Routed REPLY delivered - forward to connection for matching with pending call
            ?LOG_DEBUG("Routed REPLY delivered locally, forwarding to connection"),
            handle_rpc_reply_routed(ReplyMsg, RpcRouteMsg, State);

        {forward, NextHopNodeInfo, UpdatedRpcRouteMsg} ->
            %% Forward to next hop through mesh (delegate to RPC router)
            ?LOG_DEBUG("RPC route: forwarding to next hop"),
            _Result = macula_gateway_rpc_router:forward_rpc_route(
                NextHopNodeInfo, UpdatedRpcRouteMsg, State#state.mesh
            ),
            {noreply, State};

        {error, Reason} ->
            %% Routing error (max hops, no route, etc.)
            ?LOG_ERROR("RPC route error: ~p", [Reason]),
            {noreply, State}
    end;

%% Handle Pub/Sub route message (multi-hop DHT routing).
handle_decoded_message({ok, {pubsub_route, PubSubRouteMsg}}, _Stream, State) ->
    ?LOG_DEBUG("Received PUBSUB_ROUTE message"),
    ?LOG_DEBUG("Pub/Sub route message: ~p", [PubSubRouteMsg]),

    LocalNodeId = State#state.node_id,
    RoutingServerPid = whereis(macula_routing_server),

    case macula_pubsub_routing:route_or_deliver(LocalNodeId, PubSubRouteMsg, RoutingServerPid) of
        {deliver, Topic, PublishMsg} ->
            %% Unwrap and deliver PUBLISH locally to subscribers
            ?LOG_DEBUG("Pub/Sub route: delivering PUBLISH locally to topic ~p", [Topic]),
            handle_pubsub_route_deliver(PublishMsg, State);

        {forward, NextHopNodeInfo, UpdatedPubSubRouteMsg} ->
            %% Forward to next hop through mesh
            ?LOG_DEBUG("Pub/Sub route: forwarding to next hop"),
            forward_pubsub_route(NextHopNodeInfo, UpdatedPubSubRouteMsg, State#state.mesh),
            {noreply, State};

        {error, Reason} ->
            %% Routing error (max hops, no route, etc.)
            ?LOG_ERROR("Pub/Sub route error: ~p", [Reason]),
            {noreply, State}
    end;

%% Handle RPC call message (legacy direct call - will be deprecated).
handle_decoded_message({ok, {call, CallMsg}}, Stream, State) ->
    ?LOG_DEBUG("Received RPC CALL (DIRECT)"),
    ?LOG_DEBUG("Call message: ~p", [CallMsg]),
    handle_rpc_call(Stream, CallMsg, State);

%% Handle SUBSCRIBE message.
handle_decoded_message({ok, {subscribe, SubMsg}}, Stream, State) ->
    ?LOG_DEBUG("Received SUBSCRIBE message"),
    ?LOG_DEBUG("Subscribe message: ~p", [SubMsg]),
    handle_subscribe(Stream, SubMsg, State);

%% Handle UNSUBSCRIBE message.
handle_decoded_message({ok, {unsubscribe, UnsubMsg}}, Stream, State) ->
    ?LOG_DEBUG("Received UNSUBSCRIBE message"),
    ?LOG_DEBUG("Unsubscribe message: ~p", [UnsubMsg]),
    handle_unsubscribe(Stream, UnsubMsg, State);

%% Handle PUBLISH message.
handle_decoded_message({ok, {publish, PubMsg}}, Stream, State) ->
    ?LOG_DEBUG("Received PUBLISH message"),
    ?LOG_DEBUG("Publish message: ~p", [PubMsg]),
    handle_publish(Stream, PubMsg, State);

%% Handle PING message - respond with PONG to keep connection alive.
handle_decoded_message({ok, {ping, PingMsg}}, Stream, State) ->
    ?LOG_DEBUG("Received PING, responding with PONG"),
    Timestamp = maps:get(<<"timestamp">>, PingMsg, erlang:system_time(millisecond)),
    PongMsg = #{
        timestamp => Timestamp,
        server_time => erlang:system_time(millisecond)
    },
    PongBinary = macula_protocol_encoder:encode(pong, PongMsg),
    case macula_quic:send(Stream, PongBinary) of
        ok ->
            ?LOG_DEBUG("Sent PONG (keep-alive)"),
            {noreply, State};
        {error, SendErr} ->
            ?LOG_WARNING("Failed to send PONG: ~p", [SendErr]),
            {noreply, State}
    end;

%% Handle PONG message - connection keep-alive acknowledgment.
handle_decoded_message({ok, {pong, _PongMsg}}, _Stream, State) ->
    ?LOG_DEBUG("Received PONG - connection alive"),
    {noreply, State};

%% Handle NAT_PROBE message - respond with peer's reflexive address for NAT detection.
%% This enables NATCracker-style NAT type detection by providing the reflexive address.
handle_decoded_message({ok, {nat_probe, NatProbeMsg}}, Stream, State) ->
    ?LOG_DEBUG("[Gateway] Received NAT_PROBE message"),
    handle_nat_probe(Stream, NatProbeMsg, State);

%% Handle NAT_PROBE_REPLY message (client-side, for completeness).
%% Forwards to NAT detector for request_id correlation and observation processing.
handle_decoded_message({ok, {nat_probe_reply, NatProbeReplyMsg}}, _Stream, State) ->
    ?LOG_DEBUG("[Gateway] Received NAT_PROBE_REPLY: ~p", [NatProbeReplyMsg]),
    %% Forward to NAT detector if running
    case whereis(macula_nat_detector) of
        undefined ->
            ?LOG_DEBUG("[Gateway] NAT detector not running, ignoring reply");
        Pid ->
            ?LOG_DEBUG("[Gateway] Forwarding NAT_PROBE_REPLY to detector ~p", [Pid]),
            %% Forward complete reply message for correlation via request_id
            macula_nat_detector:handle_probe_reply(NatProbeReplyMsg)
    end,
    {noreply, State};

%% Handle PUNCH_COORDINATE message - triggers coordinated hole punch attempt.
handle_decoded_message({ok, {punch_coordinate, PunchCoordMsg}}, _Stream, State) ->
    ?LOG_DEBUG("Received PUNCH_COORDINATE message"),
    handle_punch_coordinate(PunchCoordMsg, State);

%% Handle RPC_REQUEST message (NATS-style async RPC) - execute local handler and send reply.
handle_decoded_message({ok, {rpc_request, RequestMsg}}, Stream, State) ->
    ?LOG_DEBUG("Received RPC_REQUEST (NATS-style async)"),
    handle_rpc_request(Stream, RequestMsg, State);

%% Handle RPC_REPLY message (NATS-style async RPC) - forward to requester's RPC handler.
handle_decoded_message({ok, {rpc_reply, ReplyMsg}}, _Stream, State) ->
    ?LOG_WARNING("[Gateway] RECEIVED RPC_REPLY (NATS-style async)"),
    handle_rpc_reply(ReplyMsg, State);

%% Handle other decoded message types.
handle_decoded_message({ok, {Type, Other}}, _Stream, State) ->
    ?LOG_DEBUG("Received message type ~p: ~p", [Type, Other]),
    {noreply, State};

%% Handle decode error.
handle_decoded_message({error, DecodeErr}, _Stream, State) ->
    ?LOG_ERROR("DECODE ERROR: ~p", [DecodeErr]),
    {noreply, State}.

%%%===================================================================
%%% Pub/Sub Handlers
%%%===================================================================

%% @doc Handle subscription to topics - delegate to pubsub module.
handle_subscribe(Stream, SubMsg, State) ->
    Topics = maps:get(<<"topics">>, SubMsg, []),
    ?LOG_DEBUG("Stream ~p subscribing to topics: ~p (delegating to pubsub)", [Stream, Topics]),

    PubSub = State#state.pubsub,

    %% Subscribe to each topic via pubsub module
    lists:foreach(fun(Topic) ->
        macula_gateway_pubsub:subscribe(PubSub, Stream, Topic)
    end, Topics),

    {noreply, State}.

%% @doc Handle unsubscribe from topics - delegate to pubsub module.
handle_unsubscribe(Stream, UnsubMsg, State) ->
    Topics = maps:get(<<"topics">>, UnsubMsg, []),
    ?LOG_DEBUG("Stream ~p unsubscribing from topics: ~p (delegating to pubsub)", [Stream, Topics]),

    PubSub = State#state.pubsub,

    %% Unsubscribe from each topic via pubsub module
    lists:foreach(fun(Topic) ->
        macula_gateway_pubsub:unsubscribe(PubSub, Stream, Topic)
    end, Topics),

    {noreply, State}.

%% @doc Handle publish message - distribute to all topic subscribers via DHT routing.
%% Uses multi-hop Kademlia routing for remote subscribers (v0.7.8+).
%% Delegates to macula_gateway_pubsub_router for distribution logic.
handle_publish(_PublisherStream, PubMsg, State) ->
    Topic = maps:get(<<"topic">>, PubMsg),
    Payload = maps:get(<<"payload">>, PubMsg),
    ?LOG_DEBUG("Routing publish to topic: ~s", [Topic]),

    %% Get local subscribers for this topic
    {ok, LocalSubscribers} = macula_gateway_pubsub:get_subscribers(State#state.pubsub, Topic),
    ?LOG_DEBUG("Found ~p local subscriber(s) for topic ~s", [length(LocalSubscribers), Topic]),

    %% Distribute to local and remote subscribers via the router
    macula_gateway_pubsub_router:distribute(
        LocalSubscribers,
        PubMsg,
        State#state.node_id,
        State#state.mesh,
        State#state.client_manager
    ),

    %% Relay to mesh peers (both clients and bootstrap peers)
    relay_to_mesh_peers(State, Topic, Payload),

    {noreply, State}.

%%%===================================================================
%%% Internal Helper Functions
%%%===================================================================

%% @private
%% @doc Notify health server that gateway is ready (if health server is running).
notify_health_server_ready() ->
    check_and_notify_health(whereis(macula_gateway_health)).

check_and_notify_health(undefined) ->
    ok;  % Health server not running (embedded mode)
check_and_notify_health(_Pid) ->
    macula_gateway_health:set_ready(true).

%% @private
%% @doc Register diagnostics procedures (if diagnostics service is running).
register_diagnostics_procedures(GatewayPid) ->
    check_and_register_diagnostics(whereis(macula_gateway_diagnostics), GatewayPid).

check_and_register_diagnostics(undefined, _GatewayPid) ->
    ok;  % Diagnostics service not running (embedded mode)
check_and_register_diagnostics(_Pid, GatewayPid) ->
    macula_gateway_diagnostics:register_procedures(GatewayPid).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% REMOVED: unwrap_result_to_map/1 - no longer needed since gateway is bootstrap-only
%% (was used for RPC result unwrapping)

%%%===================================================================
%%% NAT Traversal Functions
%%%===================================================================

%% @doc Handle NAT_PROBE message.
%% Gets the peer's reflexive address and sends NAT_PROBE_REPLY.
%% This enables NATCracker-style NAT type detection.
%%
%% v0.12.0 fix: Uses cached peer address from process dictionary (set by route_message)
%% because the stream may be closed by the time we process the message asynchronously.
-spec handle_nat_probe(term(), map(), #state{}) -> {noreply, #state{}}.
handle_nat_probe(Stream, NatProbeMsg, State) ->
    %% Extract node_id and request_id from probe message (support both atom and binary keys)
    NodeId = maps:get(<<"node_id">>, NatProbeMsg, maps:get(node_id, NatProbeMsg, undefined)),
    RequestId = maps:get(<<"request_id">>, NatProbeMsg, maps:get(request_id, NatProbeMsg, undefined)),

    NodeIdHex = case NodeId of undefined -> <<"unknown">>; N -> binary:encode_hex(N) end,
    ?LOG_DEBUG("[Gateway] Processing NAT_PROBE from node ~s", [NodeIdHex]),

    %% Get the peer's reflexive address - try cached address first (v0.12.0+)
    %% then fall back to calling peername on stream (may fail if stream closed)
    PeerAddr = get_peer_address(Stream),
    ?LOG_DEBUG("[Gateway] Peer address for NAT_PROBE: ~p", [PeerAddr]),
    send_nat_probe_reply(PeerAddr, RequestId, Stream, State),

    {noreply, State}.

%% @private Get peer address from cache (v0.12.0+) or stream.
-spec get_peer_address(term()) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, term()}.
get_peer_address(Stream) ->
    %% First try cached address from process dictionary (set by route_message v0.12.0+)
    case get(current_peer_addr) of
        {ok, {_IP, _Port}} = CachedAddr ->
            ?LOG_DEBUG("Using cached peer address for NAT_PROBE"),
            CachedAddr;
        _ ->
            %% Fall back to calling peername on stream (may fail if closed)
            macula_quic:peername(Stream)
    end.

%% @private Send NAT_PROBE_REPLY with reflexive address.
-spec send_nat_probe_reply({ok, {inet:ip_address(), inet:port_number()}} | {error, term()},
                           binary() | undefined, term(), #state{}) -> ok.
send_nat_probe_reply({ok, {IP, Port}}, RequestId, Stream, State) ->
    ?LOG_DEBUG("[Gateway] Sending NAT_PROBE_REPLY with reflexive ~p:~p", [IP, Port]),

    %% Convert IP to binary string for serialization
    IPBinary = format_ip_address(IP),

    %% Build NAT_PROBE_REPLY message with request_id for correlation
    ReplyMsg = #{
        node_id => State#state.node_id,  % Our node ID (the observer)
        request_id => RequestId,          % Echo back request_id for correlation
        reflexive_ip => IPBinary,
        reflexive_port => Port,
        server_time => erlang:system_time(millisecond)
    },

    %% Encode and send reply
    ReplyBinary = macula_protocol_encoder:encode(nat_probe_reply, ReplyMsg),
    case macula_quic:send(Stream, ReplyBinary) of
        ok ->
            ?LOG_DEBUG("[Gateway] Sent NAT_PROBE_REPLY to ~p:~p", [IP, Port]),
            ok;
        {error, SendErr} ->
            ?LOG_WARNING("[Gateway] Failed to send NAT_PROBE_REPLY: ~p", [SendErr]),
            ok
    end;

send_nat_probe_reply({error, Reason}, _RequestId, _Stream, _State) ->
    ?LOG_WARNING("Could not get peer address for NAT_PROBE: ~p", [Reason]),
    ok.

%% @doc Format IP address as binary string.
%% Handles both IPv4 and IPv6 addresses.
-spec format_ip_address(inet:ip_address()) -> binary().
format_ip_address({A, B, C, D}) ->
    %% IPv4: {192, 168, 1, 1} -> <<"192.168.1.1">>
    list_to_binary(io_lib:format("~p.~p.~p.~p", [A, B, C, D]));
format_ip_address({A, B, C, D, E, F, G, H}) ->
    %% IPv6: {0, 0, 0, 0, 0, 65535, 49320, 257} -> <<"::ffff:192.168.1.1">>
    list_to_binary(io_lib:format("~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B:~4.16.0B",
                                 [A, B, C, D, E, F, G, H])).

%% @doc Handle PUNCH_COORDINATE message.
%% Triggers coordinated hole punch attempt at the specified punch_time.
%% Reports result back to coordinator via NAT coordinator API.
-spec handle_punch_coordinate(map(), #state{}) -> {noreply, #state{}}.
handle_punch_coordinate(PunchCoordMsg, State) ->
    %% Extract fields (support both atom and binary keys from MessagePack)
    SessionId = get_punch_field(PunchCoordMsg, session_id, <<"session_id">>),
    PeerId = get_punch_field(PunchCoordMsg, peer_id, <<"peer_id">>),
    PeerHost = get_punch_field(PunchCoordMsg, peer_host, <<"peer_host">>),
    PeerPorts = get_punch_field(PunchCoordMsg, peer_ports, <<"peer_ports">>),
    PunchTime = get_punch_field(PunchCoordMsg, punch_time, <<"punch_time">>),
    Role = get_punch_field(PunchCoordMsg, role, <<"role">>),

    ?LOG_DEBUG("Starting hole punch to peer ~s, role=~p, punch_time=~p",
               [binary:encode_hex(PeerId), normalize_role(Role), PunchTime]),

    %% Build punch options
    PunchOpts = #{
        target_host => PeerHost,
        target_ports => ensure_port_list(PeerPorts),
        session_id => SessionId,
        punch_time => PunchTime,
        role => normalize_role(Role)
    },

    %% Spawn async process to execute hole punch and report result
    NodeId = State#state.node_id,
    spawn_link(fun() ->
        execute_and_report_punch(PeerId, PunchOpts, SessionId, NodeId)
    end),

    {noreply, State}.

%% @private
%% @doc Execute hole punch and report result back to coordinator.
-spec execute_and_report_punch(binary(), map(), binary(), binary()) -> ok.
execute_and_report_punch(PeerId, PunchOpts, SessionId, NodeId) ->
    Ref = macula_hole_punch:execute_async(PeerId, PunchOpts, self()),
    receive
        {hole_punch_result, Ref, {ok, _Conn}} ->
            ?LOG_INFO("Hole punch SUCCESS for session ~s",
                      [binary:encode_hex(SessionId)]),
            macula_nat_coordinator:report_result(SessionId, NodeId, success);
        {hole_punch_result, Ref, {error, Reason}} ->
            ?LOG_WARNING("Hole punch FAILED for session ~s: ~p",
                         [binary:encode_hex(SessionId), Reason]),
            macula_nat_coordinator:report_result(SessionId, NodeId, failure)
    after 10000 ->
        ?LOG_WARNING("Hole punch TIMEOUT for session ~s",
                     [binary:encode_hex(SessionId)]),
        macula_nat_coordinator:report_result(SessionId, NodeId, failure)
    end,
    ok.

%% @private
%% @doc Extract field from message supporting both atom and binary keys.
-spec get_punch_field(map(), atom(), binary()) -> term().
get_punch_field(Msg, AtomKey, BinaryKey) ->
    case maps:get(AtomKey, Msg, undefined) of
        undefined -> maps:get(BinaryKey, Msg, undefined);
        Value -> Value
    end.

%% @private
%% @doc Normalize role from binary or atom to atom.
-spec normalize_role(binary() | atom()) -> initiator | target.
normalize_role(<<"initiator">>) -> initiator;
normalize_role(<<"target">>) -> target;
normalize_role(initiator) -> initiator;
normalize_role(target) -> target;
normalize_role(_) -> initiator.  % Default to initiator

%% @private
%% @doc Ensure ports is a proper list of integers.
-spec ensure_port_list(term()) -> [inet:port_number()].
ensure_port_list(Ports) when is_list(Ports) -> Ports;
ensure_port_list(Port) when is_integer(Port) -> [Port];
ensure_port_list(_) -> [].

%%%===================================================================
%%% RPC Handler Lookup from Peer System
%%%===================================================================

%% @private
%% @doc Get RPC handler from the first available peer system.
%% This is used to find handlers registered by workload apps like macula_ping_pong.
%% The peer system's macula_rpc_handler is where application-level handlers are registered.
-spec get_rpc_handler_from_peers() -> {ok, pid()} | {error, term()}.
get_rpc_handler_from_peers() ->
    case whereis(macula_peers_sup) of
        undefined ->
            {error, no_peers_sup};
        _Pid ->
            case macula_peers_sup:list_peers() of
                [] ->
                    {error, no_peers};
                [PeerSystemPid | _] ->
                    get_rpc_from_peer_supervisor(PeerSystemPid)
            end
    end.

%% @private
%% @doc Extract RPC handler PID from peer system supervisor.
-spec get_rpc_from_peer_supervisor(pid()) -> {ok, pid()} | {error, term()}.
get_rpc_from_peer_supervisor(SupPid) ->
    case supervisor:which_children(SupPid) of
        Children when is_list(Children) ->
            find_rpc_handler_in_children(Children);
        _ ->
            {error, supervisor_error}
    end.

%% @private
%% @doc Find rpc_handler in supervisor children list.
%% The child ID is 'rpc_handler', not 'macula_rpc_handler'.
-spec find_rpc_handler_in_children([{term(), pid() | restarting | undefined, term(), [module()]}]) ->
    {ok, pid()} | {error, term()}.
find_rpc_handler_in_children([]) ->
    {error, rpc_handler_not_found};
find_rpc_handler_in_children([{rpc_handler, RpcPid, worker, _} | _]) when is_pid(RpcPid) ->
    {ok, RpcPid};
find_rpc_handler_in_children([_ | Rest]) ->
    find_rpc_handler_in_children(Rest).

