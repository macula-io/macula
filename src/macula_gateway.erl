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

    io:format("[Gateway] Initializing gateway (supervised mode)~n"),
    io:format("[Gateway] Port: ~p, Realm: ~s~n", [Port, Realm]),

    %% Gateway is now supervised by macula_gateway_system (root supervisor)
    %% We need to find our sibling processes and wire ourselves together

    %% Step 1: Get parent supervisor
    case find_parent_supervisor() of
        {ok, ParentSup} ->
            init_with_supervisor(ParentSup, Port, Realm);
        {error, Reason} ->
            io:format("[Gateway] Failed to find parent supervisor: ~p~n", [Reason]),
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
    io:format("[Gateway] Parent supervisor found: ~p~n", [ParentSup]),
    io:format("[Gateway] Deferring sibling wiring to handle_continue~n"),

    %% Build initial state and defer sibling finding to handle_continue
    %% This avoids deadlock where init/1 calls supervisor:which_children/1
    State = #state{
        port = Port,
        realm = Realm,
        parent_sup = ParentSup,
        client_streams = #{}
    },

    io:format("[Gateway] Returning from init with continue action~n"),
    {ok, State, {continue, wire_siblings}}.

%% @private
%% @doc Wire gateway to sibling processes after init completes.
%% Called via handle_continue to avoid supervisor initialization deadlock.
-spec wire_siblings(#state{}) -> #state{} | {stop, term()}.
wire_siblings(State) ->
    #state{parent_sup = ParentSup, port = Port, realm = Realm} = State,

    io:format("[Gateway] handle_continue: Wiring siblings...~n"),
    io:format("[Gateway] Step 1: Finding QUIC server sibling...~n"),
    case find_sibling(ParentSup, macula_gateway_quic_server) of
        {ok, QuicServerPid} ->
            io:format("[Gateway] Found QUIC server sibling: ~p~n", [QuicServerPid]),

            %% Step 3: Wire ourselves to QUIC server
            io:format("[Gateway] Step 3: Wiring gateway to QUIC server...~n"),
            ok = macula_gateway_quic_server:set_gateway(QuicServerPid, self()),
            io:format("[Gateway] Wired gateway to QUIC server~n"),

            %% Step 4: Find workers supervisor sibling
            io:format("[Gateway] Step 4: Finding workers supervisor...~n"),
            case find_sibling(ParentSup, macula_gateway_workers_sup) of
                {ok, WorkersSupPid} ->
                    io:format("[Gateway] Found workers supervisor sibling: ~p~n", [WorkersSupPid]),

                    %% Step 5: Get worker PIDs from workers supervisor
                    io:format("[Gateway] Step 5: Getting worker PIDs...~n"),
                    io:format("[Gateway] Getting clients PID...~n"),
                    {ok, ClientsPid} = macula_gateway_workers_sup:get_clients(WorkersSupPid),
                    io:format("[Gateway] Getting pubsub PID...~n"),
                    {ok, PubSubPid} = macula_gateway_workers_sup:get_pubsub(WorkersSupPid),
                    io:format("[Gateway] Getting rpc PID...~n"),
                    {ok, RpcPid} = macula_gateway_workers_sup:get_rpc(WorkersSupPid),
                    io:format("[Gateway] Getting mesh PID...~n"),
                    {ok, MeshPid} = macula_gateway_workers_sup:get_mesh(WorkersSupPid),

                    io:format("[Gateway] Worker PIDs retrieved:~n"),
                    io:format("[Gateway]   - Clients: ~p~n", [ClientsPid]),
                    io:format("[Gateway]   - PubSub: ~p~n", [PubSubPid]),
                    io:format("[Gateway]   - RPC: ~p~n", [RpcPid]),
                    io:format("[Gateway]   - Mesh: ~p~n", [MeshPid]),

                    %% Step 6: Start routing server for DHT operations
                    io:format("[Gateway] Step 6: Starting DHT routing server...~n"),
                    LocalNodeId = get_node_id(Realm, Port),
                    io:format("[Gateway] Using node ID: ~p~n", [binary:encode_hex(LocalNodeId)]),

                    RoutingConfig = #{
                        k => 20,      % Kademlia k-bucket size
                        alpha => 3    % Kademlia concurrency parameter
                    },

                    io:format("[Gateway] Calling macula_routing_server:start_link...~n"),
                    case macula_routing_server:start_link(LocalNodeId, RoutingConfig) of
                        {ok, _RoutingPid} ->
                            io:format("[Gateway] DHT routing server started~n"),
                            ok;
                        {error, {already_started, _}} ->
                            io:format("[Gateway] DHT routing server already running~n"),
                            ok;
                        {error, RoutingErr} ->
                            io:format("[Gateway] WARNING: Failed to start routing server: ~p~n", [RoutingErr]),
                            ok  % Continue without routing server
                    end,

                    %% Mark health server as ready (if running)
                    io:format("[Gateway] Step 7: Notifying health server...~n"),
                    notify_health_server_ready(),
                    io:format("[Gateway] Health server notified~n"),

                    %% Register diagnostics procedures (if running)
                    io:format("[Gateway] Step 8: Registering diagnostics...~n"),
                    register_diagnostics_procedures(self()),
                    io:format("[Gateway] Diagnostics registered~n"),

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

                    io:format("[Gateway] Initialization complete (supervised mode)~n"),
                    NewState;

                {error, WorkersSupErr} ->
                    io:format("[Gateway] Failed to find workers supervisor: ~p~n", [WorkersSupErr]),
                    {stop, {no_workers_supervisor, WorkersSupErr}}
            end;

        {error, QuicServerErr} ->
            io:format("[Gateway] Failed to find QUIC server: ~p~n", [QuicServerErr]),
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
                    io:format("[Gateway] WARNING: No HOSTNAME or NODE_NAME set, using realm+port only~n"),
                    io:format("[Gateway] This may cause node_id collisions in Docker!~n"),
                    crypto:hash(sha256, term_to_binary({Realm, Port}));
                Hostname when is_list(Hostname) ->
                    %% Use HOSTNAME from Docker - unique per container
                    io:format("[Gateway] Using HOSTNAME-based node ID: ~s, Realm=~s, Port=~p~n",
                             [Hostname, Realm, Port]),
                    crypto:hash(sha256, term_to_binary({Realm, list_to_binary(Hostname), Port}))
            end;
        NodeName when is_list(NodeName) ->
            %% Use NODE_NAME from environment - hash it to get 32-byte binary
            io:format("[Gateway] Using NODE_NAME from environment: ~s~n", [NodeName]),
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
    io:format("[Gateway] Registering handler for procedure: ~s (delegating to rpc)~n", [Procedure]),
    Rpc = State#state.rpc,
    ok = macula_gateway_rpc:register_handler(Rpc, Procedure, Handler),
    {reply, ok, State};

handle_call({unregister_handler, Procedure}, _From, State) ->
    io:format("[Gateway] Unregistering handler for procedure: ~s (delegating to rpc)~n", [Procedure]),
    Rpc = State#state.rpc,
    ok = macula_gateway_rpc:unregister_handler(Rpc, Procedure),
    {reply, ok, State};

%% @doc Handle message routed from QUIC server (async).
%% Routes decoded QUIC messages to appropriate business logic handlers.
%% This is now a cast to prevent blocking the QUIC server on message processing.

%% Local client (in-VM) message handlers
handle_call({local_publish, _Realm, Topic, Payload}, _From, State) ->
    io:format("[Gateway] Local publish: ~s~n", [Topic]),

    %% Reply immediately to avoid blocking the caller
    %% Distribution happens asynchronously in a spawned process
    gen_server:cast(self(), {distribute_publish, Topic, Payload}),

    {reply, ok, State};

handle_call({local_subscribe, _Realm, Topic, HandlerPid}, _From, State) ->
    io:format("[Gateway] Local subscribe: ~s~n", [Topic]),
    PubSub = State#state.pubsub,
    ok = macula_gateway_pubsub:subscribe(PubSub, HandlerPid, Topic),
    SubRef = make_ref(),
    {reply, {ok, SubRef}, State};

handle_call({local_unsubscribe, _SubRef}, _From, State) ->
    io:format("[Gateway] Local unsubscribe~n"),
    %% Note: Current pubsub implementation doesn't track subscription refs
    %% This is a simplified implementation - proper ref tracking would be added in production
    {reply, ok, State};

handle_call({local_rpc_call, _Realm, Procedure, Args, _Opts}, _From, State) ->
    io:format("[Gateway] Local RPC call: ~s~n", [Procedure]),
    Rpc = State#state.rpc,
    Result = macula_gateway_rpc:invoke_handler(Rpc, Procedure, Args),
    {reply, Result, State};

handle_call({local_register_procedure, _Realm, Procedure, Handler}, _From, State) ->
    io:format("[Gateway] Local register procedure: ~s~n", [Procedure]),
    Rpc = State#state.rpc,
    ok = macula_gateway_rpc:register_handler(Rpc, Procedure, Handler),
    {reply, ok, State};

handle_call({local_unregister_procedure, Procedure}, _From, State) ->
    io:format("[Gateway] Local unregister procedure: ~s~n", [Procedure]),
    Rpc = State#state.rpc,
    ok = macula_gateway_rpc:unregister_handler(Rpc, Procedure),
    {reply, ok, State};

handle_call({local_advertise, _Realm, Procedure, Handler, Opts}, _From, State) ->
    io:format("[Gateway] Local advertise: ~s~n", [Procedure]),

    %% 1. Register handler locally with gateway RPC
    Rpc = State#state.rpc,
    ok = macula_gateway_rpc:register_handler(Rpc, Procedure, Handler),
    io:format("[Gateway] Registered handler for ~s with local RPC~n", [Procedure]),

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
            io:format("[Gateway] WARNING: Routing server not running, service ~s advertised locally only~n",
                     [Procedure]);
        RoutingServerPid ->
            case macula_routing_server:store(RoutingServerPid, ServiceKey, ServiceValue) of
                ok ->
                    io:format("[Gateway] Successfully stored service ~s in DHT~n", [Procedure]);
                {error, StoreError} ->
                    io:format("[Gateway] WARNING: Failed to store service ~s in DHT: ~p~n",
                             [Procedure, StoreError])
            end
    end,

    %% Return reference for tracking (matching behaviour spec)
    Ref = make_ref(),
    {reply, {ok, Ref}, State};

handle_call({local_unadvertise, Procedure}, _From, State) ->
    io:format("[Gateway] Local unadvertise: ~s~n", [Procedure]),
    Rpc = State#state.rpc,
    ok = macula_gateway_rpc:unregister_handler(Rpc, Procedure),
    {reply, ok, State};

handle_call({local_discover_subscribers, _Realm, Topic}, _From, State) ->
    io:format("[Gateway] Local discover subscribers for topic: ~s~n", [Topic]),
    %% Hash the topic to create DHT key
    TopicKey = crypto:hash(sha256, Topic),

    %% Query the routing server (DHT) for subscribers
    Result = case whereis(macula_routing_server) of
        undefined ->
            io:format("[Gateway] Routing server not running~n"),
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
    io:format("[Gateway] Local get node ID~n"),
    {reply, {ok, State#state.node_id}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle rpc_route message forwarded from connection
handle_cast({process_rpc_route, RpcRouteMsg}, State) ->
    io:format("[Gateway] Processing rpc_route message from connection~n"),
    %% Extract the CALL payload from rpc_route envelope
    #{<<"payload">> := CallMsg} = RpcRouteMsg,
    %% Call handle_rpc_call_routed with Stream=undefined, CallMsg, RpcRouteMsg, State
    handle_rpc_call_routed(undefined, CallMsg, RpcRouteMsg, State);

%% @doc Handle async distribution of published messages
handle_cast({distribute_publish, Topic, Payload}, State) ->
    io:format("[Gateway] Async distributing ~s via DHT-based routing~n", [Topic]),
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

%% @doc Handle message routed from QUIC server (async).
%% Routes decoded QUIC messages to appropriate business logic handlers.
handle_cast({route_message, MessageType, Message, Stream}, State) ->
    io:format("[Gateway] Routing message type ~p from QUIC server (async)~n", [MessageType]),
    %% Use existing handle_decoded_message logic
    Result = handle_decoded_message({ok, {MessageType, Message}}, Stream, State),
    %% Extract new state from {noreply, NewState} tuple
    NewState = element(2, Result),
    {noreply, NewState};

%% Async local publish - fire-and-forget from local client
handle_cast({local_publish_async, _Realm, Topic, Payload}, State) ->
    io:format("[Gateway] Local publish async: ~s~n", [Topic]),
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
    io:format("[Gateway] handle_continue(wire_siblings) called~n"),
    case wire_siblings(State) of
        #state{} = NewState ->
            io:format("[Gateway] Sibling wiring completed successfully~n"),
            {noreply, NewState};
        {stop, Reason} ->
            io:format("[Gateway] Sibling wiring failed: ~p~n", [Reason]),
            {stop, Reason, State}
    end.

%%%===================================================================
%%% Info Callbacks
%%%===================================================================

%% Handle new stream created by peer (quicer message)
%% Client connected - delegate to clients module
handle_info({client_connected, ClientPid, ClientInfo}, State) ->
    io:format("[Gateway] Client connected: ~p (delegating to clients)~n", [ClientInfo]),
    ClientMgr = State#state.client_manager,
    ok = macula_gateway_clients:client_connected(ClientMgr, ClientPid, ClientInfo),
    {noreply, State};

%% Client disconnected - delegate cleanup to all child modules
handle_info({'DOWN', _Ref, process, ClientPid, _Reason}, State) ->
    io:format("[Gateway] Client disconnected: ~p (delegating cleanup)~n", [ClientPid]),

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
    io:format("[Gateway] WARNING: Unhandled handle_info message: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Gateway is now supervised - parent supervisor will handle cleanup
    %% No need to manually stop QUIC server or workers supervisor
    io:format("[Gateway] Shutting down (supervised mode)~n"),
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

    ClientInfo = #{
        realm => RealmId,
        node_id => NodeId,
        capabilities => maps:get(<<"capabilities">>, ConnectMsg, []),
        endpoint => Endpoint
    },
    io:format("[Gateway] Client connected: ~p~n", [ClientInfo]),

    %% HTTP/3 streams are bidirectional - we can send messages back on the same stream
    %% Store the client's incoming stream in CLIENT MANAGER for routing (enables client-only mode)
    ok = macula_gateway_clients:store_client_stream(State#state.client_manager, NodeId, Stream, Endpoint),
    io:format("[Gateway] Stored client stream for node ~p (endpoint: ~s) in client manager (bidirectional communication)~n",
             [binary:encode_hex(NodeId), Endpoint]),

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
            io:format("[Gateway] WARNING: Routing server not running, cannot add peer~n");
        RoutingServerPid ->
            io:format("[Gateway] Adding peer to routing table: ~p~n", [NodeId]),
            macula_routing_server:add_node(RoutingServerPid, NodeInfo),
            io:format("[Gateway] Peer added to routing table~n")
    end,

    %% Send PONG acknowledgment back to keep stream alive for bidirectional communication
    PongMsg = #{
        timestamp => erlang:system_time(millisecond),
        server_time => erlang:system_time(millisecond)
    },
    PongBinary = macula_protocol_encoder:encode(pong, PongMsg),
    case macula_quic:send(Stream, PongBinary) of
        ok ->
            io:format("[Gateway] Sent PONG acknowledgment to client ~p~n",
                     [binary:encode_hex(NodeId)]);
        {error, PongErr} ->
            io:format("[Gateway] WARNING: Failed to send PONG: ~p~n", [PongErr])
    end,

    {noreply, NewState};
handle_connect_realm(false, Stream, ConnectMsg, State) ->
    RealmId = maps:get(<<"realm_id">>, ConnectMsg),
    io:format("[Gateway] Realm mismatch: ~p != ~p~n", [RealmId, State#state.realm]),
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

    io:format("[Gateway] Delivering routed PUBLISH to topic ~p~n", [Topic]),

    %% Publish to local subscribers (delegate to pubsub module)
    macula_gateway_pubsub:publish(State#state.pubsub, Topic, Payload),
    {noreply, State}.

%% @doc Forward pubsub_route message to next hop through mesh.
%% Uses async (fire-and-forget) pattern to avoid blocking gateway.
%% Graceful error handling - logs errors but doesn't crash gateway.
forward_pubsub_route(NextHopNodeInfo, PubSubRouteMsg, MeshPid) ->
    io:format("[Gateway] Forwarding pubsub_route to next hop (async)~n"),

    %% Extract next hop info (routing_bucket:node_info uses atom keys, not binary)
    #{node_id := NextHopNodeId,
      address := Address} = NextHopNodeInfo,

    %% Encode message
    EncodedMsg = macula_protocol_encoder:encode(pubsub_route, PubSubRouteMsg),

    %% Send asynchronously - does NOT block the gateway
    %% Connection creation and sending happens in a spawned process
    macula_gateway_mesh:send_async(MeshPid, NextHopNodeId, Address, EncodedMsg),
    io:format("[Gateway] Queued pubsub_route for async send to ~s~n",
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
            io:format("[Gateway] Failed to encode publish message: ~p~n", [Reason]),
            ok;
        EncodedMsg ->
            %% Broadcast to all connected clients (applications/child gateways)
            ClientManager = State#state.client_manager,
            macula_gateway_clients:broadcast(ClientManager, EncodedMsg),
            io:format("[Gateway] Broadcasted ~s to all clients~n", [Topic]),
            ok
    end,

    %% 2. Forward to bootstrap peers (parent gateways)
    %% Get peer system supervisors
    PeerSystems = macula_peers_sup:list_peers(),
    io:format("[Gateway] Peer systems: ~p~n", [PeerSystems]),

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

    io:format("[Gateway] Bootstrap connection PIDs: ~p~n", [ConnectionPids]),

    case ConnectionPids of
        [] ->
            io:format("[Gateway] No bootstrap peers to forward to~n"),
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
                        io:format("[Gateway] Forwarded ~s to bootstrap peer ~p~n", [Topic, ConnPid]);
                    {error, PeerReason} ->
                        io:format("[Gateway] Failed to forward ~s to peer ~p: ~p~n",
                                 [Topic, ConnPid, PeerReason])
                end
            end, Connections),
            io:format("[Gateway] Forwarded ~s to ~p bootstrap peer(s)~n", [Topic, length(Connections)])
    end,
    ok.


%% @doc Handle RPC call message (legacy direct handling).
handle_rpc_call(Stream, CallMsg, State) ->
    Procedure = maps:get(<<"procedure">>, CallMsg),
    CallId = maps:get(<<"call_id">>, CallMsg),

    %% Special exception: handle DHT-related procedures
    case Procedure of
        <<"_dht.list_gateways">> ->
            handle_dht_list_gateways(Stream, CallId, State);
        <<"_dht.store">> ->
            handle_dht_store(Stream, CallId, CallMsg, State);
        _ ->
            io:format("[Gateway] BOOTSTRAP-ONLY MODE: Rejecting RPC call to ~s (peers must communicate directly via DHT)~n", [Procedure]),

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
    io:format("[Gateway] Handling _dht.list_gateways RPC~n"),

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

            io:format("[Gateway] Returning ~p registered gateway(s) from DHT~n", [length(PeersList)]),

            %% Send successful reply with peers list
            SuccessReply = #{
                call_id => CallId,
                result => #{
                    <<"peers">> => PeersList
                }
            },
            ReplyBinary = macula_protocol_encoder:encode(reply, SuccessReply),
            macula_quic:send(Stream, ReplyBinary)
    end,
    {noreply, _State}.

%% @private Handle _dht.store RPC for DHT sync from embedded gateways
handle_dht_store(Stream, CallId, CallMsg, _State) ->
    io:format("[Gateway] Handling _dht.store RPC~n"),

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
                    io:format("[Gateway] Storing in DHT: key=~p~n", [Key]),
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
    io:format("[Gateway] Decoded CONNECT message~n"),
    handle_connect(Stream, ConnectMsg, State);

%% Handle decoded STORE message.
handle_decoded_message({ok, {store, StoreMsg}}, Stream, State) ->
    io:format("[Gateway] *** RECEIVED STORE MESSAGE ***~n"),
    io:format("[Gateway] STORE message: ~p~n", [StoreMsg]),
    handle_dht_store(Stream, StoreMsg, State);

%% Handle decoded FIND_VALUE message.
handle_decoded_message({ok, {find_value, FindValueMsg}}, Stream, State) ->
    io:format("[Gateway] *** RECEIVED FIND_VALUE MESSAGE ***~n"),
    io:format("[Gateway] FIND_VALUE message: ~p~n", [FindValueMsg]),
    handle_dht_find_value(Stream, FindValueMsg, State);

%% Handle decoded FIND_NODE message.
handle_decoded_message({ok, {find_node, FindNodeMsg}}, Stream, State) ->
    io:format("[Gateway] *** RECEIVED FIND_NODE MESSAGE ***~n"),
    io:format("[Gateway] FIND_NODE message: ~p~n", [FindNodeMsg]),
    handle_dht_find_node(Stream, FindNodeMsg, State);

%% Handle RPC route message (multi-hop DHT routing).
handle_decoded_message({ok, {rpc_route, RpcRouteMsg}}, Stream, State) ->
    io:format("[Gateway] *** RECEIVED RPC_ROUTE MESSAGE ***~n"),
    io:format("[Gateway] RPC route message: ~p~n", [RpcRouteMsg]),

    LocalNodeId = State#state.node_id,
    RoutingServerPid = whereis(macula_routing_server),

    case macula_rpc_routing:route_or_deliver(LocalNodeId, RpcRouteMsg, RoutingServerPid) of
        {deliver, <<"call">>, CallMsg} ->
            %% Unwrap and process CALL locally (MessagePack returns binary, not atom)
            io:format("[Gateway] RPC route: delivering CALL locally~n"),
            handle_rpc_call_routed(Stream, CallMsg, RpcRouteMsg, State);

        {deliver, <<"reply">>, ReplyMsg} ->
            %% Routed REPLY delivered - forward to connection for matching with pending call
            io:format("[Gateway] Routed REPLY delivered locally, forwarding to connection~n"),
            handle_rpc_reply_routed(ReplyMsg, RpcRouteMsg, State);

        {forward, NextHopNodeInfo, UpdatedRpcRouteMsg} ->
            %% Forward to next hop through mesh (delegate to RPC router)
            io:format("[Gateway] RPC route: forwarding to next hop~n"),
            _Result = macula_gateway_rpc_router:forward_rpc_route(
                NextHopNodeInfo, UpdatedRpcRouteMsg, State#state.mesh
            ),
            {noreply, State};

        {error, Reason} ->
            %% Routing error (max hops, no route, etc.)
            io:format("[Gateway] RPC route error: ~p~n", [Reason]),
            {noreply, State}
    end;

%% Handle Pub/Sub route message (multi-hop DHT routing).
handle_decoded_message({ok, {pubsub_route, PubSubRouteMsg}}, _Stream, State) ->
    io:format("[Gateway] *** RECEIVED PUBSUB_ROUTE MESSAGE ***~n"),
    io:format("[Gateway] Pub/Sub route message: ~p~n", [PubSubRouteMsg]),

    LocalNodeId = State#state.node_id,
    RoutingServerPid = whereis(macula_routing_server),

    case macula_pubsub_routing:route_or_deliver(LocalNodeId, PubSubRouteMsg, RoutingServerPid) of
        {deliver, Topic, PublishMsg} ->
            %% Unwrap and deliver PUBLISH locally to subscribers
            io:format("[Gateway] Pub/Sub route: delivering PUBLISH locally to topic ~p~n", [Topic]),
            handle_pubsub_route_deliver(PublishMsg, State);

        {forward, NextHopNodeInfo, UpdatedPubSubRouteMsg} ->
            %% Forward to next hop through mesh
            io:format("[Gateway] Pub/Sub route: forwarding to next hop~n"),
            forward_pubsub_route(NextHopNodeInfo, UpdatedPubSubRouteMsg, State#state.mesh),
            {noreply, State};

        {error, Reason} ->
            %% Routing error (max hops, no route, etc.)
            io:format("[Gateway] Pub/Sub route error: ~p~n", [Reason]),
            {noreply, State}
    end;

%% Handle RPC call message (legacy direct call - will be deprecated).
handle_decoded_message({ok, {call, CallMsg}}, Stream, State) ->
    io:format("[Gateway] *** RECEIVED RPC CALL (DIRECT) ***~n"),
    io:format("[Gateway] Call message: ~p~n", [CallMsg]),
    handle_rpc_call(Stream, CallMsg, State);

%% Handle SUBSCRIBE message.
handle_decoded_message({ok, {subscribe, SubMsg}}, Stream, State) ->
    io:format("[Gateway] *** RECEIVED SUBSCRIBE MESSAGE ***~n"),
    io:format("[Gateway] Subscribe message: ~p~n", [SubMsg]),
    handle_subscribe(Stream, SubMsg, State);

%% Handle UNSUBSCRIBE message.
handle_decoded_message({ok, {unsubscribe, UnsubMsg}}, Stream, State) ->
    io:format("[Gateway] *** RECEIVED UNSUBSCRIBE MESSAGE ***~n"),
    io:format("[Gateway] Unsubscribe message: ~p~n", [UnsubMsg]),
    handle_unsubscribe(Stream, UnsubMsg, State);

%% Handle PUBLISH message.
handle_decoded_message({ok, {publish, PubMsg}}, Stream, State) ->
    io:format("[Gateway] *** RECEIVED PUBLISH MESSAGE ***~n"),
    io:format("[Gateway] Publish message: ~p~n", [PubMsg]),
    handle_publish(Stream, PubMsg, State);

%% Handle PING message - respond with PONG to keep connection alive.
handle_decoded_message({ok, {ping, PingMsg}}, Stream, State) ->
    io:format("[Gateway] Received PING, responding with PONG~n"),
    Timestamp = maps:get(<<"timestamp">>, PingMsg, erlang:system_time(millisecond)),
    PongMsg = #{
        timestamp => Timestamp,
        server_time => erlang:system_time(millisecond)
    },
    PongBinary = macula_protocol_encoder:encode(pong, PongMsg),
    case macula_quic:send(Stream, PongBinary) of
        ok ->
            io:format("[Gateway] Sent PONG (keep-alive)~n"),
            {noreply, State};
        {error, SendErr} ->
            io:format("[Gateway] WARNING: Failed to send PONG: ~p~n", [SendErr]),
            {noreply, State}
    end;

%% Handle PONG message - connection keep-alive acknowledgment.
handle_decoded_message({ok, {pong, _PongMsg}}, _Stream, State) ->
    io:format("[Gateway] Received PONG - connection alive~n"),
    {noreply, State};

%% Handle NAT_PROBE message - respond with peer's reflexive address for NAT detection.
%% This enables NATCracker-style NAT type detection by providing the reflexive address.
handle_decoded_message({ok, {nat_probe, NatProbeMsg}}, Stream, State) ->
    io:format("[Gateway] Received NAT_PROBE message~n"),
    handle_nat_probe(Stream, NatProbeMsg, State);

%% Handle NAT_PROBE_REPLY message (client-side, for completeness).
handle_decoded_message({ok, {nat_probe_reply, NatProbeReplyMsg}}, _Stream, State) ->
    io:format("[Gateway] Received NAT_PROBE_REPLY: ~p~n", [NatProbeReplyMsg]),
    %% Forward to NAT detector if running
    case whereis(macula_nat_detector) of
        undefined ->
            io:format("[Gateway] NAT detector not running, ignoring reply~n");
        _Pid ->
            %% Extract reflexive address and forward to detector
            ObserverId = State#state.node_id,
            ReflexiveIP = maps:get(<<"reflexive_ip">>, NatProbeReplyMsg),
            ReflexivePort = maps:get(<<"reflexive_port">>, NatProbeReplyMsg),
            macula_nat_detector:add_observation(ObserverId, {ReflexiveIP, ReflexivePort})
    end,
    {noreply, State};

%% Handle other decoded message types.
handle_decoded_message({ok, {Type, Other}}, _Stream, State) ->
    io:format("[Gateway] Received message type ~p: ~p~n", [Type, Other]),
    {noreply, State};

%% Handle decode error.
handle_decoded_message({error, DecodeErr}, _Stream, State) ->
    io:format("[Gateway] !!! DECODE ERROR: ~p !!!~n", [DecodeErr]),
    {noreply, State}.

%%%===================================================================
%%% Pub/Sub Handlers
%%%===================================================================

%% @doc Handle subscription to topics - delegate to pubsub module.
handle_subscribe(Stream, SubMsg, State) ->
    Topics = maps:get(<<"topics">>, SubMsg, []),
    io:format("[Gateway] Stream ~p subscribing to topics: ~p (delegating to pubsub)~n", [Stream, Topics]),

    PubSub = State#state.pubsub,

    %% Subscribe to each topic via pubsub module
    lists:foreach(fun(Topic) ->
        macula_gateway_pubsub:subscribe(PubSub, Stream, Topic)
    end, Topics),

    {noreply, State}.

%% @doc Handle unsubscribe from topics - delegate to pubsub module.
handle_unsubscribe(Stream, UnsubMsg, State) ->
    Topics = maps:get(<<"topics">>, UnsubMsg, []),
    io:format("[Gateway] Stream ~p unsubscribing from topics: ~p (delegating to pubsub)~n", [Stream, Topics]),

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
    io:format("[Gateway] Routing publish to topic: ~s~n", [Topic]),

    %% Get local subscribers for this topic
    {ok, LocalSubscribers} = macula_gateway_pubsub:get_subscribers(State#state.pubsub, Topic),
    io:format("[Gateway] Found ~p local subscriber(s) for topic ~s~n", [length(LocalSubscribers), Topic]),

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
%% Gets the peer's reflexive address from the QUIC stream and sends NAT_PROBE_REPLY.
%% This enables NATCracker-style NAT type detection.
-spec handle_nat_probe(term(), map(), #state{}) -> {noreply, #state{}}.
handle_nat_probe(Stream, NatProbeMsg, State) ->
    %% Extract node_id from probe message
    NodeId = maps:get(<<"node_id">>, NatProbeMsg, maps:get(node_id, NatProbeMsg, undefined)),

    io:format("[Gateway] Processing NAT_PROBE from node ~p~n",
              [case NodeId of undefined -> <<"unknown">>; _ -> binary:encode_hex(NodeId) end]),

    %% Get the peer's reflexive address from the QUIC stream
    case macula_quic:peername(Stream) of
        {ok, {IP, Port}} ->
            io:format("[Gateway] Peer reflexive address: ~p:~p~n", [IP, Port]),

            %% Convert IP to binary string for serialization
            IPBinary = format_ip_address(IP),

            %% Build NAT_PROBE_REPLY message
            ReplyMsg = #{
                node_id => NodeId,
                reflexive_ip => IPBinary,
                reflexive_port => Port,
                server_time => erlang:system_time(millisecond)
            },

            %% Encode and send reply
            ReplyBinary = macula_protocol_encoder:encode(nat_probe_reply, ReplyMsg),
            case macula_quic:send(Stream, ReplyBinary) of
                ok ->
                    io:format("[Gateway] Sent NAT_PROBE_REPLY to ~p:~p~n", [IP, Port]);
                {error, SendErr} ->
                    io:format("[Gateway] WARNING: Failed to send NAT_PROBE_REPLY: ~p~n", [SendErr])
            end;

        {error, Reason} ->
            io:format("[Gateway] WARNING: Could not get peer address: ~p~n", [Reason])
    end,

    {noreply, State}.

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

