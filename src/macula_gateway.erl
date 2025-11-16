%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Gateway - HTTP/3 Message Router &amp; Orchestrator
%%%
%%% Main API module and coordinator for the Macula Gateway.
%%% The gateway can be embedded in applications or run standalone.
%%%
%%% Architecture (Modular Design - Refactored Jan 2025):
%%% ========================================================
%%%
%%% Gateway (this module):
%%%   - QUIC Listener Management
%%%   - Message Decoding &amp; Routing
%%%   - Supervisor Coordination
%%%   - API Facade
%%%
%%% Child Modules (managed via macula_gateway_workers_sup):
%%%   - macula_gateway_clients: Client lifecycle management
%%%   - macula_gateway_pubsub: Pub/Sub message routing with wildcards
%%%   - macula_gateway_rpc: RPC handler registration &amp; invocation
%%%   - macula_gateway_mesh: Mesh connection pooling
%%%
%%% Stateless Delegation Modules:
%%%   - macula_gateway_dht: DHT query forwarding to routing server
%%%   - macula_gateway_rpc_router: Multi-hop RPC routing via DHT
%%%
%%% Single Responsibility Principle:
%%%   Each module has one clear purpose and delegates to specialized
%%%   child modules. Gateway acts as orchestrator, not implementer.
%%%
%%% Usage (Embedded):
%%% ```
%%% {ok, Pid} = macula_gateway:start_link([
%%%     {port, 9443},
%%%     {realm, &lt;&lt;"com.example.realm"&gt;&gt;}
%%% ]).
%%%
%%% %% Register RPC handler
%%% macula_gateway:register_handler(&lt;&lt;"add"&gt;&gt;, fun(#{a := A, b := B}) ->
%%%     #{result => A + B}
%%% end).
%%% '''
%%%
%%% Usage (Standalone):
%%% ```
%%% application:start(macula_gateway).
%%% '''
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
    handle_info/2,
    terminate/2
]).

-record(state, {
    port :: inet:port_number(),
    realm :: binary(),
    node_id :: binary(),                          % 32-byte local node ID
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
%% Finds sibling processes, wires them together, and starts routing server.
-spec init_with_supervisor(pid(), inet:port_number(), binary()) ->
    {ok, #state{}} | {stop, term()}.
init_with_supervisor(ParentSup, Port, Realm) ->
    io:format("[Gateway] Parent supervisor found: ~p~n", [ParentSup]),

    %% Step 2: Find sibling processes from parent supervisor
    case find_sibling(ParentSup, macula_gateway_quic_server) of
        {ok, QuicServerPid} ->
            io:format("[Gateway] Found QUIC server sibling: ~p~n", [QuicServerPid]),

            %% Step 3: Wire ourselves to QUIC server
            ok = macula_gateway_quic_server:set_gateway(QuicServerPid, self()),
            io:format("[Gateway] Wired gateway to QUIC server~n"),

            %% Step 4: Find workers supervisor sibling
            case find_sibling(ParentSup, macula_gateway_workers_sup) of
                {ok, WorkersSupPid} ->
                    io:format("[Gateway] Found workers supervisor sibling: ~p~n", [WorkersSupPid]),

                    %% Step 5: Get worker PIDs from workers supervisor
                    {ok, ClientsPid} = macula_gateway_workers_sup:get_clients(WorkersSupPid),
                    {ok, PubSubPid} = macula_gateway_workers_sup:get_pubsub(WorkersSupPid),
                    {ok, RpcPid} = macula_gateway_workers_sup:get_rpc(WorkersSupPid),
                    {ok, MeshPid} = macula_gateway_workers_sup:get_mesh(WorkersSupPid),

                    io:format("[Gateway] Worker PIDs retrieved:~n"),
                    io:format("[Gateway]   - Clients: ~p~n", [ClientsPid]),
                    io:format("[Gateway]   - PubSub: ~p~n", [PubSubPid]),
                    io:format("[Gateway]   - RPC: ~p~n", [RpcPid]),
                    io:format("[Gateway]   - Mesh: ~p~n", [MeshPid]),

                    %% Step 6: Start routing server for DHT operations
                    LocalNodeId = get_node_id(Realm, Port),
                    io:format("[Gateway] Using node ID: ~p~n", [binary:encode_hex(LocalNodeId)]),

                    RoutingConfig = #{
                        k => 20,      % Kademlia k-bucket size
                        alpha => 3    % Kademlia concurrency parameter
                    },

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
                    notify_health_server_ready(),

                    %% Register diagnostics procedures (if running)
                    register_diagnostics_procedures(self()),

                    %% Step 7: Build final state
                    State = #state{
                        port = Port,
                        realm = Realm,
                        node_id = LocalNodeId,
                        listener = QuicServerPid,  % QUIC server PID
                        supervisor = WorkersSupPid,  % Workers supervisor PID
                        client_manager = ClientsPid,
                        pubsub = PubSubPid,
                        rpc = RpcPid,
                        mesh = MeshPid,
                        client_streams = #{}
                    },

                    io:format("[Gateway] Initialization complete (supervised mode)~n"),
                    {ok, State};

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
%% @doc Get node ID from NODE_NAME environment variable or generate from realm/port.
%% Uses idiomatic Erlang pattern matching on function heads.
-spec get_node_id(binary(), inet:port_number()) -> binary().
get_node_id(Realm, Port) ->
    case os:getenv("NODE_NAME") of
        false ->
            %% No NODE_NAME set, generate hash for backward compatibility
            io:format("[Gateway] No NODE_NAME env var, generating node ID from realm/port~n"),
            crypto:hash(sha256, term_to_binary({Realm, Port}));
        NodeName when is_list(NodeName) ->
            %% Use NODE_NAME from environment (converted to binary)
            NodeNameBin = list_to_binary(NodeName),
            io:format("[Gateway] Using NODE_NAME from environment: ~s~n", [NodeName]),
            NodeNameBin
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

%% @doc Handle message routed from QUIC server.
%% Routes decoded QUIC messages to appropriate business logic handlers.
handle_call({route_message, MessageType, Message, Stream}, _From, State) ->
    io:format("[Gateway] Routing message type ~p from QUIC server~n", [MessageType]),
    %% Use existing handle_decoded_message logic
    Result = handle_decoded_message({ok, {MessageType, Message}}, Stream, State),
    %% Extract new state from {noreply, NewState} tuple
    NewState = element(2, Result),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle rpc_route message forwarded from connection
handle_cast({process_rpc_route, RpcRouteMsg}, State) ->
    io:format("[Gateway] Processing rpc_route message from connection~n"),
    %% Extract the CALL payload from rpc_route envelope
    #{<<"payload">> := CallMsg} = RpcRouteMsg,
    %% Call handle_rpc_call_routed with Stream=undefined, CallMsg, RpcRouteMsg, State
    handle_rpc_call_routed(undefined, CallMsg, RpcRouteMsg, State);

handle_cast(_Request, State) ->
    {noreply, State}.

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
    %% Store the client's incoming stream for sending replies (enables client-only mode)
    %% Also store endpoint address for creating mesh connections when needed
    ClientStreams = State#state.client_streams,
    NewClientStreams = ClientStreams#{NodeId => Stream},
    io:format("[Gateway] Stored client stream for node ~p (bidirectional communication)~n",
             [binary:encode_hex(NodeId)]),

    %% Connection management now handled by macula_gateway_mesh module
    NewState = State#state{client_streams = NewClientStreams},

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
forward_pubsub_route(NextHopNodeInfo, PubSubRouteMsg, MeshPid) ->
    io:format("[Gateway] Forwarding pubsub_route to next hop~n"),

    %% Extract next hop info
    #{<<"node_id">> := NextHopNodeId,
      <<"address">> := AddressBin,
      <<"port">> := Port} = NextHopNodeInfo,

    %% Parse address (inline implementation to avoid module dependency)
    Address = case inet:parse_address(binary_to_list(AddressBin)) of
        {ok, IpTuple} -> IpTuple;
        {error, _} -> binary_to_list(AddressBin)  % Treat as hostname
    end,

    %% Get or create mesh connection
    {ok, Stream} = macula_gateway_mesh:get_or_create_connection(MeshPid, NextHopNodeId, {Address, Port}),

    %% Encode and send pubsub_route message
    EncodedMsg = macula_protocol_encoder:encode(pubsub_route, PubSubRouteMsg),
    macula_quic:send(Stream, EncodedMsg),
    io:format("[Gateway] Forwarded pubsub_route successfully~n"),
    ok.

%% @doc Handle RPC call message (legacy direct handling).
handle_rpc_call(Stream, CallMsg, State) ->
    io:format("[Gateway] Processing RPC CALL message: ~p~n", [CallMsg]),

    %% Extract call data (CallMsg has binary keys from msgpack)
    Procedure = maps:get(<<"procedure">>, CallMsg),
    CallId = maps:get(<<"call_id">>, CallMsg),
    ArgsJson = maps:get(<<"args">>, CallMsg),

    io:format("[Gateway] Procedure: ~s, CallId: ~p~n", [Procedure, CallId]),

    %% Look up handler (delegate to rpc module)
    Rpc = State#state.rpc,
    case macula_gateway_rpc:get_handler(Rpc, Procedure) of
        not_found ->
            %% No handler registered
            io:format("[Gateway] No handler found for procedure: ~s~n", [Procedure]),
            ErrorReply = #{
                call_id => CallId,
                error => #{
                    code => <<"no_such_procedure">>,
                    message => <<"No handler registered for ", Procedure/binary>>
                }
            },
            ReplyBinary = macula_protocol_encoder:encode(reply, ErrorReply),
            macula_quic:send(Stream, ReplyBinary),
            {noreply, State};

        {ok, Handler} ->
            %% Decode args from JSON
            try
                Args = json:decode(ArgsJson),
                io:format("[Gateway] Decoded args: ~p~n", [Args]),

                %% Invoke handler
                io:format("[Gateway] Invoking handler~n"),
                Result = Handler(Args),
                io:format("[Gateway] Handler result: ~p~n", [Result]),

                %% Unwrap result tuple if needed
                ResultMap = unwrap_result_to_map(Result),

                %% Send reply (don't close stream - let connection idle timeout handle cleanup)
                Reply = #{
                    call_id => CallId,
                    result => encode_json(ResultMap)
                },
                SuccessReplyBinary = macula_protocol_encoder:encode(reply, Reply),
                SendResult = macula_quic:send(Stream, SuccessReplyBinary),
                io:format("[Gateway] Sent reply (result: ~p, stream: ~p, size: ~p bytes)~n",
                         [SendResult, Stream, byte_size(SuccessReplyBinary)]),
                {noreply, State}
            catch
                Class:Reason:Stacktrace ->
                    io:format("[Gateway] Handler error: ~p:~p~n~p~n", [Class, Reason, Stacktrace]),
                    ErrorReply = #{
                        call_id => CallId,
                        error => #{
                            code => <<"handler_error">>,
                            message => iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))
                        }
                    },
                    ErrorReplyBinary = macula_protocol_encoder:encode(reply, ErrorReply),
                    macula_quic:send(Stream, ErrorReplyBinary),
                    {noreply, State}
            end
    end.

%% @doc Encode map/list to JSON binary.
-spec encode_json(map() | list()) -> binary().
encode_json(Data) ->
    macula_utils:encode_json(Data).

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
    io:format("[Gateway] Publishing message to topic: ~s~n", [Topic]),

    LocalNodeId = State#state.node_id,
    Mesh = State#state.mesh,
    PubSub = State#state.pubsub,

    %% Get local subscribers and delegate distribution to pubsub_router
    {ok, LocalSubscribers} = macula_gateway_pubsub:get_subscribers(PubSub, Topic),
    macula_gateway_pubsub_router:distribute(LocalSubscribers, PubMsg, LocalNodeId, Mesh),

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

%% @doc Unwrap result to map format.
unwrap_result_to_map({ok, Map}) when is_map(Map) -> Map;
unwrap_result_to_map(Map) when is_map(Map) -> Map;
unwrap_result_to_map(Other) -> #{value => Other}.

