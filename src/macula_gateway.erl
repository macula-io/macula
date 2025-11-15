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
%%% Child Modules (managed via macula_gateway_sup):
%%%   - macula_gateway_client_manager: Client lifecycle management
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

%% Export for testing only
-ifdef(TEST).
-export([
    parse_endpoint/1,
    complete_handshake/1,
    accept_streams/1,
    register_next_connection/1,
    resolve_host/2
]).
-endif.

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

    %% Get TLS certificates
    %% Priority: 1. Environment variables (production)
    %%           2. Pre-generated certs (development)
    {CertFile, KeyFile} = get_tls_certificates(),

    io:format("Using TLS certificates:~n"),
    io:format("  Cert: ~s~n", [CertFile]),
    io:format("  Key:  ~s~n", [KeyFile]),

    %% Validate certificate files exist
    case macula_quic_cert:validate_files(CertFile, KeyFile) of
        ok ->
            start_quic_listener(Port, Realm, CertFile, KeyFile);
        {error, Reason} ->
            io:format("Certificate validation failed: ~p~n", [Reason]),
            {stop, {cert_validation_failed, Reason}}
    end.

%% @private
%% @doc Get TLS certificate paths from environment or use pre-generated ones.
get_tls_certificates() ->
    case {os:getenv("TLS_CERT_FILE"), os:getenv("TLS_KEY_FILE")} of
        {false, false} ->
            %% No env vars, use pre-generated certs
            io:format("Using pre-generated TLS certificates~n"),
            {"/opt/macula/certs/cert.pem", "/opt/macula/certs/key.pem"};
        {CertEnv, KeyEnv} when CertEnv =/= false andalso KeyEnv =/= false ->
            %% Use mounted certificates (production)
            io:format("Using mounted TLS certificates from environment~n"),
            {CertEnv, KeyEnv};
        _ ->
            %% Partial configuration, log warning and use defaults
            io:format("WARNING: Partial TLS environment config, using pre-generated certs~n"),
            {"/opt/macula/certs/cert.pem", "/opt/macula/certs/key.pem"}
    end.

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

%% @private
%% @doc Start the QUIC listener with given certificates.
start_quic_listener(Port, Realm, CertFile, KeyFile) ->
    %% Start QUIC listener using simple quicer API
    ListenOpts = [
        {cert, CertFile},
        {key, KeyFile},
        {alpn, ["macula"]},
        {peer_unidi_stream_count, 3},
        {peer_bidi_stream_count, 100}  % Allow clients to create bidirectional streams
    ],

    case macula_quic:listen(Port, ListenOpts) of
        {ok, Listener} ->
            io:format("Macula Gateway listening on port ~p (realm: ~s)~n", [Port, Realm]),

            %% Mark health server as ready (if running)
            notify_health_server_ready(),

            %% Register diagnostics procedures (if running)
            register_diagnostics_procedures(self()),

            %% Start async accept to receive connection events
            case quicer:async_accept(Listener, #{}) of
                {ok, Listener} ->
                    io:format("[Gateway] Async accept registered, ready for connections~n"),
                    ok;
                {error, AcceptErr} ->
                    io:format("[Gateway] WARNING: async_accept failed: ~p~n", [AcceptErr]),
                    ok  % Continue anyway, maybe we can still work
            end,

            %% Start routing server for DHT operations
            %% Use NODE_NAME from environment if available, otherwise generate hash
            LocalNodeId = get_node_id(Realm, Port),
            io:format("[Gateway] Using node ID: ~p (binary: ~p)~n",
                     [binary:encode_hex(LocalNodeId), LocalNodeId]),
            RoutingConfig = #{
                k => 20,      % Kademlia k-bucket size
                alpha => 3    % Kademlia concurrency parameter
            },
            case macula_routing_server:start_link(LocalNodeId, RoutingConfig) of
                {ok, _RoutingPid} ->
                    io:format("[Gateway] DHT routing server started with node ID: ~p~n",
                             [binary:encode_hex(LocalNodeId)]),
                    ok;
                {error, {already_started, _}} ->
                    io:format("[Gateway] DHT routing server already running~n"),
                    ok;
                {error, RoutingErr} ->
                    io:format("[Gateway] WARNING: Failed to start routing server: ~p~n", [RoutingErr]),
                    ok  % Continue without routing server
            end,

            %% Start supervisor with configuration
            Config = #{
                port => Port,
                realm => Realm,
                node_id => LocalNodeId
            },

            case macula_gateway_sup:start_link(Config) of
                {ok, SupPid} ->
                    io:format("[Gateway] Supervisor started: ~p~n", [SupPid]),

                    %% Get child PIDs from supervisor
                    {ok, ClientMgrPid} = macula_gateway_sup:get_client_manager(SupPid),
                    {ok, PubSubPid} = macula_gateway_sup:get_pubsub(SupPid),
                    {ok, RpcPid} = macula_gateway_sup:get_rpc(SupPid),
                    {ok, MeshPid} = macula_gateway_sup:get_mesh(SupPid),

                    io:format("[Gateway] Child modules started:~n"),
                    io:format("[Gateway]   - Client Manager: ~p~n", [ClientMgrPid]),
                    io:format("[Gateway]   - Pub/Sub: ~p~n", [PubSubPid]),
                    io:format("[Gateway]   - RPC: ~p~n", [RpcPid]),
                    io:format("[Gateway]   - Mesh: ~p~n", [MeshPid]),

                    State = #state{
                        port = Port,
                        realm = Realm,
                        node_id = LocalNodeId,
                        listener = Listener,
                        supervisor = SupPid,
                        client_manager = ClientMgrPid,
                        pubsub = PubSubPid,
                        rpc = RpcPid,
                        mesh = MeshPid,
                        client_streams = #{}
                    },

                    {ok, State};

                {error, SupErr} ->
                    io:format("[Gateway] Failed to start supervisor: ~p~n", [SupErr]),
                    {stop, {supervisor_failed, SupErr}}
            end;

        {error, Reason} ->
            io:format("QUIC listen failed: ~p~n", [Reason]),
            {stop, {listen_failed, Reason}};

        {error, Type, Details} ->
            io:format("QUIC listen failed: ~p ~p~n", [Type, Details]),
            {stop, {listen_failed, {Type, Details}}};

        Other ->
            io:format("QUIC listen unexpected result: ~p~n", [Other]),
            {stop, {listen_failed, Other}}
    end.

handle_call(get_stats, _From, State) ->
    ClientMgr = State#state.client_manager,
    Rpc = State#state.rpc,

    %% Query child modules for their stats
    {ok, AllClients} = macula_gateway_client_manager:get_all_clients(ClientMgr),
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
handle_info({quic, new_stream, Stream, StreamProps}, State) ->
    io:format("[Gateway] ========================================~n"),
    io:format("[Gateway] NEW STREAM RECEIVED!~n"),
    io:format("[Gateway] Stream: ~p~n", [Stream]),
    io:format("[Gateway] StreamProps: ~p~n", [StreamProps]),
    io:format("[Gateway] ========================================~n"),

    %% Set stream to active mode to receive data automatically
    case quicer:setopt(Stream, active, true) of
        ok ->
            io:format("[Gateway] Stream set to active mode~n"),
            {noreply, State};
        {error, Reason} ->
            io:format("[Gateway] Failed to set stream active: ~p~n", [Reason]),
            {noreply, State}
    end;

%% Handle data from QUIC stream (active mode)
handle_info({quic, Data, Stream, _Flags}, State) when is_binary(Data) ->
    io:format("[Gateway] ===== Received ~p bytes from stream ~p =====~n", [byte_size(Data), Stream]),
    io:format("[Gateway] Raw data (first 100 bytes): ~p~n", [binary:part(Data, 0, min(100, byte_size(Data)))]),
    DecodeResult = macula_protocol_decoder:decode(Data),
    handle_decoded_message(DecodeResult, Stream, State);

%% Client connected - delegate to client_manager
handle_info({client_connected, ClientPid, ClientInfo}, State) ->
    io:format("[Gateway] Client connected: ~p (delegating to client_manager)~n", [ClientInfo]),
    ClientMgr = State#state.client_manager,
    ok = macula_gateway_client_manager:client_connected(ClientMgr, ClientPid, ClientInfo),
    {noreply, State};

%% Client disconnected - delegate cleanup to all child modules
handle_info({'DOWN', _Ref, process, ClientPid, _Reason}, State) ->
    io:format("[Gateway] Client disconnected: ~p (delegating cleanup)~n", [ClientPid]),

    ClientMgr = State#state.client_manager,
    PubSub = State#state.pubsub,
    Rpc = State#state.rpc,

    %% Client manager handles client removal
    macula_gateway_client_manager:client_disconnected(ClientMgr, ClientPid),

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

%% QUIC control event: peer_needs_streams
handle_info({quic, peer_needs_streams, _Conn, _StreamType}, State) ->
    %% Peer is signaling it wants to open more streams (bidi_streams or uni_streams)
    %% This is normal - just acknowledge
    {noreply, State};

%% QUIC event: new_conn (connection established)
handle_info({quic, new_conn, Conn, ConnInfo}, State) ->
    io:format("[Gateway] ========================================~n"),
    io:format("[Gateway] NEW CONNECTION RECEIVED!~n"),
    io:format("[Gateway] Connection: ~p~n", [Conn]),
    io:format("[Gateway] Connection Info: ~p~n", [ConnInfo]),
    io:format("[Gateway] ========================================~n"),

    %% Extracted functions eliminate nested case statements
    complete_handshake(Conn),
    register_next_connection(State#state.listener),

    {noreply, State};

%% QUIC event: shutdown (connection shutting down)
handle_info({quic, shutdown, Conn, Reason}, State) ->
    io:format("[Gateway] QUIC shutdown: Conn=~p, Reason=~p~n", [Conn, Reason]),
    {noreply, State};

%% QUIC event: transport_shutdown (transport layer shutting down)
handle_info({quic, transport_shutdown, Conn, Reason}, State) ->
    io:format("[Gateway] QUIC transport_shutdown: Conn=~p, Reason=~p~n", [Conn, Reason]),
    {noreply, State};

%% DHT query (find_node, find_value, store)
handle_info({dht_query, FromPid, QueryType, QueryData}, State) ->
    %% Delegate to DHT module
    _Result = macula_gateway_dht:handle_query(FromPid, QueryType, QueryData),
    {noreply, State};

handle_info(Info, State) ->
    io:format("[Gateway] WARNING: Unhandled handle_info message: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, #state{listener = Listener, supervisor = SupPid}) ->
    %% Close listener
    case Listener of
        undefined -> ok;
        _ -> macula_quic:close(Listener)
    end,

    %% Stop supervisor (will stop all children: client_manager, pubsub, rpc)
    case SupPid of
        undefined -> ok;
        _ ->
            erlang:unlink(SupPid),
            exit(SupPid, shutdown)
    end,

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
    %% Extract and parse endpoint from CONNECT message for peer-to-peer connections
    Endpoint = maps:get(<<"endpoint">>, ConnectMsg, undefined),
    Address = parse_endpoint(Endpoint),

    ClientInfo = #{
        realm => RealmId,
        node_id => NodeId,
        capabilities => maps:get(<<"capabilities">>, ConnectMsg, []),
        endpoint => Endpoint,
        address => Address
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

%% @doc Handle publish message - distribute to all topic subscribers via pubsub module.
handle_publish(_PublisherStream, PubMsg, State) ->
    Topic = maps:get(<<"topic">>, PubMsg),
    io:format("[Gateway] Publishing message to topic: ~s (delegating to pubsub)~n", [Topic]),

    PubSub = State#state.pubsub,

    %% Get matching subscribers from pubsub module
    {ok, Subscribers} = macula_gateway_pubsub:get_subscribers(PubSub, Topic),

    io:format("[Gateway] Found ~p subscribers for topic ~s~n", [length(Subscribers), Topic]),

    %% Encode and send to each subscriber
    PubBinary = macula_protocol_encoder:encode(publish, PubMsg),
    lists:foreach(
        fun(SubscriberStream) ->
            case macula_quic:send(SubscriberStream, PubBinary) of
                ok ->
                    io:format("[Gateway] Successfully sent to stream ~p~n", [SubscriberStream]);
                {error, Reason} ->
                    io:format("[Gateway] Failed to send to stream ~p: ~p~n",
                              [SubscriberStream, Reason])
            end
        end,
        Subscribers
    ),

    io:format("[Gateway] Finished distributing message to ~p subscribers~n", [length(Subscribers)]),

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
%%% Connection Lifecycle Functions
%%%
%%% Extracted from handle_info({quic, new_conn...}) to eliminate
%%% nested case statements and follow idiomatic Erlang patterns.
%%%===================================================================

%% @doc Complete TLS handshake on new connection.
%% Calls accept_streams/1 on success, logs error on failure.
-spec complete_handshake(quicer:connection_handle()) -> ok.
complete_handshake(Conn) ->
    case quicer:handshake(Conn) of
        ok ->
            accept_streams(Conn);
        {ok, _} ->
            accept_streams(Conn);
        {error, Reason} ->
            io:format("[Gateway] Handshake failed: ~p~n", [Reason]),
            ok
    end.

%% @doc Start accepting streams on an established connection.
-spec accept_streams(quicer:connection_handle()) -> ok.
accept_streams(Conn) ->
    io:format("[Gateway] Connection handshake completed successfully~n"),
    case quicer:async_accept_stream(Conn, #{}) of
        {ok, Conn} ->
            io:format("[Gateway] Ready to accept streams on connection~n"),
            ok;
        {error, StreamAcceptErr} ->
            io:format("[Gateway] WARNING: async_accept_stream failed: ~p~n", [StreamAcceptErr]),
            ok
    end.

%% @doc Register listener for next incoming connection.
-spec register_next_connection(quicer:listener_handle()) -> ok.
register_next_connection(Listener) ->
    case quicer:async_accept(Listener, #{}) of
        {ok, _} ->
            io:format("[Gateway] Ready for next connection~n"),
            ok;
        {error, AcceptErr} ->
            io:format("[Gateway] WARNING: async_accept failed: ~p~n", [AcceptErr]),
            ok
    end.

%%%===================================================================
%%% Endpoint Parsing
%%%===================================================================

%% @doc Parse endpoint URL to address tuple.
%% Converts "https://host:port" to {{IP_tuple}, Port}
%% Returns placeholder on parsing errors instead of crashing.
-spec parse_endpoint(undefined | binary()) -> {{byte(), byte(), byte(), byte()}, inet:port_number()}.
parse_endpoint(undefined) ->
    {{0,0,0,0}, 0};
parse_endpoint(Endpoint) when is_binary(Endpoint) ->
    %% Parse URL using uri_string
    case uri_string:parse(Endpoint) of
        #{host := Host, port := Port} when is_integer(Port) ->
            resolve_host(Host, Port);
        #{host := Host} ->
            resolve_host(Host, 9443);  %% Default port
        _ ->
            io:format("[Gateway] Invalid endpoint URL format: ~s, using placeholder~n", [Endpoint]),
            {{0,0,0,0}, 0}
    end.

%% @doc Resolve hostname to IP address.
%% Returns localhost fallback on DNS resolution failure.
%% Extracted to eliminate duplicate DNS resolution logic.
-spec resolve_host(binary(), inet:port_number()) -> {{byte(), byte(), byte(), byte()}, inet:port_number()}.
resolve_host(Host, Port) when is_binary(Host), is_integer(Port) ->
    HostStr = binary_to_list(Host),
    case inet:getaddr(HostStr, inet) of
        {ok, IPTuple} ->
            {IPTuple, Port};
        {error, Reason} ->
            io:format("[Gateway] Failed to resolve host ~s: ~p, using localhost fallback~n",
                     [Host, Reason]),
            {{127,0,0,1}, Port}
    end.

%% @doc Unwrap result to map format.
unwrap_result_to_map({ok, Map}) when is_map(Map) -> Map;
unwrap_result_to_map(Map) when is_map(Map) -> Map;
unwrap_result_to_map(Other) -> #{value => Other}.

