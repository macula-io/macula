%%%-------------------------------------------------------------------
%%% @doc
%%% Message dispatch logic extracted from macula_connection.
%%%
%%% Contains all process_message/2 clauses and their helper functions
%%% for routing incoming protocol messages to the appropriate handlers.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_connection_dispatch).

-include_lib("kernel/include/logger.hrl").
-include("macula_connection.hrl").

%% Public API
-export([process_message/2, send_message_raw/3]).

%% Functions also used by macula_connection handle_info clauses
-export([add_discovered_peers/2,
         notify_mesh_lifecycle_observers/2,
         remove_peer_from_routing_table/1,
         normalize_peer_node_id/1]).

%% Test exports for bootstrap lookup helpers
-ifdef(TEST).
-export([extract_peer_info/2, make_peer_info/3, get_map_field/3, get_map_field/4]).
-endif.

%%%===================================================================
%%% Message Dispatch
%%%===================================================================

%% @doc Process a received message - route to appropriate handler.
-spec process_message({atom(), map()}, #state{}) -> #state{}.

%% Route PUBLISH messages to pub/sub handler
process_message({publish, Msg}, State) ->
    Topic = maps:get(<<"topic">>, Msg, maps:get(topic, Msg, <<>>)),
    ?LOG_INFO("[peer] Incoming PUBLISH topic=~s", [Topic]),
    %% Intercept internal mesh lifecycle events
    case handle_mesh_lifecycle_publish(Topic, Msg) of
        handled -> State;
        passthrough ->
            %% Route to pubsub handler for application-level subscribers
            case gproc:lookup_local_name({pubsub_handler, State#state.realm, State#state.peer_id}) of
                undefined ->
                    ?LOG_WARNING("PubSub handler not found for realm ~s, peer_id ~p",
                                [State#state.realm, State#state.peer_id]),
                    State;
                PubSubPid ->
                    macula_pubsub_handler:handle_incoming_publish(PubSubPid, Msg),
                    State
            end
    end;

%% Route REPLY messages to RPC handler
process_message({reply, Msg}, State) ->
    ?LOG_INFO("Connection manager routing message type: ~p", [reply]),
    %% Look up the RPC handler by node_id (not peer_id) so replies arriving on ANY
    %% connection belonging to this node route to the correct handler
    case gproc:lookup_local_name({rpc_handler, State#state.realm, State#state.node_id}) of
        undefined ->
            ?LOG_WARNING("RPC handler not found for realm ~s, node_id ~s when routing REPLY",
                        [State#state.realm, binary:encode_hex(State#state.node_id)]),
            State;
        RpcPid ->
            macula_rpc_handler:handle_incoming_reply(RpcPid, Msg),
            ?LOG_DEBUG("Routed REPLY to rpc_handler"),
            State
    end;

%% Handle CONNECTED acknowledgment
process_message({connected, Msg}, State) ->
    ?LOG_INFO("Received CONNECTED acknowledgment from server: ~p", [Msg]),
    State;

%% Handle PING message - respond with PONG
process_message({ping, Msg}, #state{stream = Stream} = State) ->
    ?LOG_DEBUG("Received PING, responding with PONG"),
    PongMsg = #{timestamp => maps:get(timestamp, Msg, 0)},
    _ = send_message_raw(pong, PongMsg, Stream),
    State;

%% Handle PONG message - keep-alive acknowledgment and server DHT registration
%% The first PONG after CONNECT contains server's node_id and endpoint for DHT routing
process_message({pong, PongMsg}, State) ->
    ?LOG_DEBUG("Received PONG - connection alive"),
    %% Check if PONG contains server's node_id (set by gateway on first response)
    maybe_register_server_in_dht(PongMsg, State),
    %% Check if PONG contains peers list (initial state on connect)
    maybe_populate_peers_from_pong(PongMsg, State),
    State;

%% Handle FIND_VALUE_REPLY message - route to RPC handler for DHT query results
process_message({find_value_reply, Msg}, State) ->
    ?LOG_DEBUG("Connection manager routing message type: ~p", [find_value_reply]),
    %% Look up the RPC handler by node_id (not peer_id) so replies arriving on ANY
    %% connection belonging to this node route to the correct handler
    case gproc:lookup_local_name({rpc_handler, State#state.realm, State#state.node_id}) of
        undefined ->
            ?LOG_WARNING("RPC handler not found for realm ~s, node_id ~s",
                        [State#state.realm, binary:encode_hex(State#state.node_id)]),
            State;
        RpcPid ->
            macula_rpc_handler:handle_find_value_reply(RpcPid, Msg),
            ?LOG_DEBUG("Routed FIND_VALUE_REPLY to rpc_handler"),
            State
    end;

%% Handle RPC_REQUEST message - execute local handler and send reply back via stream.
%% This is used for relay scenarios: bootstrap forwards RPC_REQUEST through client stream,
%% and we handle it locally and reply through the same stream.
%%
%% IMPORTANT: We search ALL RPC handlers in this realm for the procedure, not just
%% the one associated with this connection's peer_id. This is because:
%% - The procedure handler (e.g., ping.handler) is registered by macula_ping_pong
%% - macula_ping_pong uses the RPC handler from macula_peers_sup (outbound connection)
%% - But the RPC_REQUEST arrives via the gateway's inbound client stream (different peer_id)
%% - So we need to search all RPC handlers to find the procedure
process_message({rpc_request, Msg}, #state{stream = Stream} = State) ->
    ?LOG_WARNING("[Connection] PROCESSING RPC_REQUEST locally (relay scenario)"),
    RequestId = maps:get(<<"request_id">>, Msg, undefined),
    Procedure = maps:get(<<"procedure">>, Msg, undefined),
    Args = maps:get(<<"args">>, Msg, <<>>),
    FromNode = maps:get(<<"from_node">>, Msg, undefined),

    ?LOG_WARNING("[Connection] RPC_REQUEST details: procedure=~s, request_id=~p, from_node=~p",
                [Procedure, RequestId, FromNode]),

    %% Search ALL RPC handlers in this realm for the procedure
    %% This handles relay scenarios where handler is in a different peer system
    Result = find_and_execute_handler(State#state.realm, Procedure, Args),

    ?LOG_WARNING("[Connection] RPC_REQUEST processing result: ~p", [Result]),

    %% Build and send reply back through the same stream
    %% Include to_node (original requester) so bootstrap can forward the reply
    ReplyMsg = case Result of
        {ok, ResultValue} ->
            EncodedResult = try macula_utils:encode_json(ResultValue) catch _:_ -> ResultValue end,
            #{
                request_id => RequestId,
                result => EncodedResult,
                from_node => State#state.node_id,
                to_node => FromNode,  %% Original requester - bootstrap uses this to route reply
                timestamp => erlang:system_time(millisecond)
            };
        {error, ErrorReason} ->
            #{
                request_id => RequestId,
                error => ErrorReason,
                from_node => State#state.node_id,
                to_node => FromNode,  %% Original requester - bootstrap uses this to route reply
                timestamp => erlang:system_time(millisecond)
            }
    end,

    ?LOG_WARNING("[Connection] Sending RPC_REPLY back through stream: ~p", [ReplyMsg]),
    EncodedReply = macula_protocol_encoder:encode(rpc_reply, ReplyMsg),
    ?LOG_WARNING("[Connection] EncodedReply size: ~p bytes", [byte_size(EncodedReply)]),
    case macula_quic:send(Stream, EncodedReply) of
        ok ->
            ?LOG_WARNING("[Connection] Successfully sent RPC_REPLY");
        {error, SendError} ->
            ?LOG_WARNING("[Connection] Failed to send RPC_REPLY: ~p", [SendError])
    end,
    State;

%% Handle RPC_REPLY message - route to RPC handler for async callback invocation.
%% This handles replies that come back through the client connection stream.
%% IMPORTANT: Use handle_async_reply (not handle_incoming_reply) because
%% async RPC uses request_id field, while sync RPC uses call_id field.
%% CRITICAL: Look up by node_id (not peer_id) so replies arriving on ANY connection
%% belonging to this node route to the correct handler. This fixes the relay scenario
%% where request goes out via outbound connection but reply arrives via inbound stream.
process_message({rpc_reply, Msg}, State) ->
    RequestId = maps:get(<<"request_id">>, Msg, maps:get(request_id, Msg, undefined)),
    ?LOG_WARNING("[Connection] RECEIVED RPC_REPLY: request_id=~p, node_id=~s",
                [RequestId, binary:encode_hex(State#state.node_id)]),
    case gproc:lookup_local_name({rpc_handler, State#state.realm, State#state.node_id}) of
        undefined ->
            ?LOG_WARNING("[Connection] RPC_REPLY: RPC handler not found, realm=~p, node_id=~s",
                        [State#state.realm, binary:encode_hex(State#state.node_id)]),
            State;
        RpcPid ->
            ?LOG_WARNING("[Connection] RPC_REPLY: routing to RPC handler pid=~p", [RpcPid]),
            macula_rpc_handler:handle_async_reply(RpcPid, Msg),
            ?LOG_WARNING("[Connection] RPC_REPLY: delivered to RPC handler"),
            State
    end;

%% Handle FIND_NODE reply - add discovered peers to our routing table
process_message({find_node_reply, Msg}, State) ->
    Nodes = maps:get(<<"nodes">>, Msg, maps:get(nodes, Msg, [])),
    ?LOG_INFO("[Connection] FIND_NODE reply with ~p node(s)", [length(Nodes)]),
    add_discovered_peers(Nodes, State#state.node_id),
    State;

%% Handle unknown message types
process_message({Type, _Msg}, State) ->
    ?LOG_INFO("Connection manager routing message type: ~p", [Type]),
    ?LOG_WARNING("Unknown message type: ~p", [Type]),
    State.

%%%===================================================================
%%% Send Message
%%%===================================================================

%% @doc Send a protocol message through a stream (raw).
%% Crashes if message is invalid - this indicates a bug in the caller.
-spec send_message_raw(atom(), map(), reference()) -> ok | {error, term()}.
send_message_raw(Type, Msg, Stream) ->
    ?LOG_DEBUG("Type=~p, Msg=~p", [Type, Msg]),
    Binary = macula_protocol_encoder:encode(Type, Msg),
    macula_quic:async_send(Stream, Binary).

%%%===================================================================
%%% Mesh Lifecycle Helpers
%%%===================================================================

%% @doc Handle internal mesh lifecycle pub/sub events.
%% Intercepts _mesh.peer.connected and _mesh.peer.disconnected before
%% passing other publish messages to the application-level pubsub handler.
handle_mesh_lifecycle_publish(<<"_mesh.peer.connected">>, Msg) ->
    Payload = maps:get(<<"payload">>, Msg, Msg),
    self() ! {mesh_peer_connected, Payload},
    handled;
handle_mesh_lifecycle_publish(<<"_mesh.peer.disconnected">>, Msg) ->
    Payload = maps:get(<<"payload">>, Msg, Msg),
    self() ! {mesh_peer_disconnected, Payload},
    handled;
handle_mesh_lifecycle_publish(_, _) ->
    passthrough.

%% @doc Populate routing table from peers list in PONG (initial state on connect).
maybe_populate_peers_from_pong(PongMsg, #state{node_id = MyNodeId}) ->
    Peers = maps:get(<<"peers">>, PongMsg, maps:get(peers, PongMsg, undefined)),
    populate_peers(Peers, MyNodeId).

populate_peers(undefined, _) -> ok;
populate_peers(Peers, MyNodeId) when is_list(Peers), length(Peers) > 0 ->
    ?LOG_DEBUG("[Connection] PONG contains ~p peer(s), populating routing table", [length(Peers)]),
    add_discovered_peers(Peers, MyNodeId),
    notify_mesh_lifecycle_observers(mesh_peers_initial, #{peers => Peers});
populate_peers(_, _) -> ok.

%% @doc Notify external observers of mesh peer lifecycle events via pg group.
%% Any process can join the macula_mesh_lifecycle pg group to receive these.
-spec notify_mesh_lifecycle_observers(atom(), map()) -> ok.
notify_mesh_lifecycle_observers(EventType, PeerInfo) ->
    Members = pg:get_members(pg, macula_mesh_lifecycle),
    [Pid ! {macula_mesh_event, EventType, PeerInfo} || Pid <- Members],
    ok.

%% @doc Remove a peer from the local routing table on disconnect notification.
-spec remove_peer_from_routing_table(undefined | binary()) -> ok.
remove_peer_from_routing_table(undefined) -> ok;
remove_peer_from_routing_table(NodeId) when is_binary(NodeId) ->
    case whereis(macula_routing_server) of
        undefined -> ok;
        RoutingServer -> macula_routing_server:remove_node(RoutingServer, NodeId)
    end.

%%%===================================================================
%%% DHT Registration Helpers
%%%===================================================================

%% @doc Register connected server in DHT routing table using info from PONG response.
%% Called when PONG message contains server's real node_id and endpoint.
%% If PONG doesn't contain node_id (keep-alive pong), does nothing.
-spec maybe_register_server_in_dht(map(), #state{}) -> ok.
maybe_register_server_in_dht(PongMsg, State) ->
    %% Check for node_id in PONG (binary key from msgpack decoding)
    ServerNodeId = maps:get(<<"node_id">>, PongMsg, undefined),
    ServerEndpoint = maps:get(<<"endpoint">>, PongMsg, undefined),
    do_register_server_in_dht(ServerNodeId, ServerEndpoint, State).

%% @private Only register if we have the server's real node_id
do_register_server_in_dht(undefined, _Endpoint, _State) ->
    %% Keep-alive PONG - no server info to register
    ok;
do_register_server_in_dht(ServerNodeId, ServerEndpoint, State) when is_binary(ServerNodeId) ->
    %% First PONG after CONNECT - contains real server info
    ServerAddress = parse_server_endpoint(State#state.url),
    ServerNodeInfo = #{
        node_id => ServerNodeId,
        address => ServerAddress,
        endpoint => ServerEndpoint
    },
    ?LOG_DEBUG("[Connection] Registering server in DHT: node_id=~s, endpoint=~s",
              [binary:encode_hex(ServerNodeId), ServerEndpoint]),

    case macula_routing_server:add_node(macula_routing_server, ServerNodeInfo) of
        ok ->
            ?LOG_DEBUG("[Connection] Added server to DHT routing table: ~s",
                     [binary:encode_hex(ServerNodeId)]);
        {error, Reason} ->
            ?LOG_WARNING("DHT registration failed (expected if DHT not running): ~p", [Reason])
    end,
    ok.

%% @doc Parse server endpoint to extract address for DHT.
-spec parse_server_endpoint(binary()) -> binary().
parse_server_endpoint(Url) when is_binary(Url) ->
    parse_server_endpoint_str(binary_to_list(Url));
parse_server_endpoint(Url) ->
    Url.

parse_server_endpoint_str("https://" ++ Rest) ->
    parse_server_host_port(Rest);
parse_server_endpoint_str("http://" ++ Rest) ->
    parse_server_host_port(Rest);
parse_server_endpoint_str(Other) ->
    list_to_binary(Other).

parse_server_host_port(HostPort) ->
    case string:split(HostPort, ":") of
        [Host, PortStr] ->
            list_to_binary(Host ++ ":" ++ PortStr);
        [Host] ->
            list_to_binary(Host)
    end.

%%%===================================================================
%%% Peer Discovery Helpers
%%%===================================================================

%% @doc Add discovered peers from a FIND_NODE reply to our routing table.
-spec add_discovered_peers([map()], binary()) -> ok.
add_discovered_peers(Nodes, MyNodeId) ->
    add_discovered_peers_impl(whereis(macula_routing_server), Nodes, MyNodeId).

add_discovered_peers_impl(undefined, _Nodes, _MyNodeId) ->
    ?LOG_WARNING("[Connection] No routing server — cannot add discovered peers"),
    ok;
add_discovered_peers_impl(RoutingServer, Nodes, MyNodeId) ->
    ValidPeers = lists:filtermap(fun(NodeInfo) -> extract_peer_info(NodeInfo, MyNodeId) end, Nodes),
    lists:foreach(fun(Info) ->
        macula_routing_server:add_node(RoutingServer, Info)
    end, ValidPeers),
    ok.

%% @doc Extract peer info from a FIND_NODE reply node entry, skipping ourselves.
-spec extract_peer_info(map(), binary()) -> {true, map()} | false.
extract_peer_info(NodeInfo, MyNodeId) when is_map(NodeInfo) ->
    NodeId = get_map_field(NodeInfo, node_id, <<"node_id">>),
    Endpoint = get_map_field(NodeInfo, endpoint, <<"endpoint">>,
                  get_map_field(NodeInfo, address, <<"address">>)),
    make_peer_info(NodeId, Endpoint, MyNodeId);
extract_peer_info(_, _) ->
    false.

-spec make_peer_info(term(), term(), binary()) -> {true, map()} | false.
make_peer_info(undefined, _, _) -> false;
make_peer_info(MyId, _, MyId) -> false;
make_peer_info(NodeId, Endpoint, MyId) ->
    %% Normalize: hex-encoded (64 bytes) -> raw binary (32 bytes)
    RawNodeId = normalize_peer_node_id(NodeId),
    skip_if_self(RawNodeId, MyId, Endpoint).

skip_if_self(MyId, MyId, _) -> false;
skip_if_self(RawNodeId, _, Endpoint) ->
    {true, #{node_id => RawNodeId, address => Endpoint, endpoint => Endpoint}}.

-spec normalize_peer_node_id(binary()) -> binary().
normalize_peer_node_id(Bin) when byte_size(Bin) =:= 32 -> Bin;
normalize_peer_node_id(Hex) when byte_size(Hex) =:= 64 -> binary:decode_hex(Hex);
normalize_peer_node_id(Other) -> Other.

%% @doc Get a field from a map trying atom key first, then binary key.
-spec get_map_field(map(), atom(), binary()) -> term().
get_map_field(Map, AtomKey, BinKey) ->
    maps:get(AtomKey, Map, maps:get(BinKey, Map, undefined)).

-spec get_map_field(map(), atom(), binary(), term()) -> term().
get_map_field(Map, AtomKey, BinKey, Default) ->
    case maps:get(AtomKey, Map, maps:get(BinKey, Map, undefined)) of
        undefined -> Default;
        Value -> Value
    end.

%%%===================================================================
%%% RPC Handler Helpers
%%%===================================================================

%% @private Find and execute a procedure handler by searching all RPC handlers in the realm.
%% This is used for relay scenarios where the handler may be registered with a different
%% peer system than the one associated with the incoming connection.
-spec find_and_execute_handler(binary(), binary(), binary() | map()) ->
    {ok, term()} | {error, binary()}.
find_and_execute_handler(Realm, Procedure, Args) ->
    ?LOG_WARNING("[Connection] Searching ALL RPC handlers in realm ~s for procedure ~s",
                [Realm, Procedure]),

    %% Get all RPC handlers registered in gproc for this realm
    %% Pattern: {rpc_handler, Realm, _} matches any peer_id
    Pattern = {n, l, {rpc_handler, Realm, '_'}},
    RpcHandlers = gproc:lookup_pids(Pattern),

    ?LOG_WARNING("[Connection] Found ~p RPC handlers in realm", [length(RpcHandlers)]),

    %% Search each RPC handler's service registry for the procedure
    find_handler_in_registries(RpcHandlers, Procedure, Args).

%% @private Search through RPC handlers to find one that has the procedure registered.
-spec find_handler_in_registries([pid()], binary(), binary() | map()) ->
    {ok, term()} | {error, binary()}.
find_handler_in_registries([], Procedure, _Args) ->
    ?LOG_WARNING("[Connection] Procedure ~s not found in any RPC handler", [Procedure]),
    {error, <<"procedure_not_found">>};
find_handler_in_registries([RpcPid | Rest], Procedure, Args) ->
    ?LOG_WARNING("[Connection] Checking RPC handler ~p for procedure ~s", [RpcPid, Procedure]),
    case macula_rpc_handler:get_service_registry(RpcPid) of
        {error, _} ->
            %% Try next handler
            find_handler_in_registries(Rest, Procedure, Args);
        Registry ->
            case macula_service_registry:get_local_handler(Registry, Procedure) of
                {ok, Handler} ->
                    ?LOG_WARNING("[Connection] FOUND handler for ~s in RPC handler ~p",
                                [Procedure, RpcPid]),
                    execute_handler(Handler, Args);
                not_found ->
                    %% Try next handler
                    find_handler_in_registries(Rest, Procedure, Args)
            end
    end.

%% @private Execute a handler function with the provided arguments.
-spec execute_handler(fun((map()) -> {ok, term()} | {error, term()}), binary() | map()) ->
    {ok, term()} | {error, binary()}.
execute_handler(Handler, Args) ->
    DecodedArgs = safe_decode_json(Args),
    safe_execute_handler(Handler, DecodedArgs).

%% @private Safely decode JSON, falling back to original args on decode failure.
-spec safe_decode_json(binary() | map()) -> term().
safe_decode_json(Args) ->
    case catch macula_utils:decode_json(Args) of
        {'EXIT', _} -> Args;
        Decoded -> Decoded
    end.

%% @private Safely execute handler, returning error tuple on exception.
-spec safe_execute_handler(fun((map()) -> {ok, term()} | {error, term()}), term()) ->
    {ok, term()} | {error, binary()}.
safe_execute_handler(Handler, Args) ->
    handle_execution_result(catch Handler(Args)).

%% @private Handle handler execution result
handle_execution_result({'EXIT', Error}) ->
    ?LOG_WARNING("[Connection] Handler THREW error: ~p", [Error]),
    {error, iolist_to_binary(io_lib:format("~p", [Error]))};
handle_execution_result(HandlerResult) ->
    ?LOG_WARNING("[Connection] Handler executed, result=~p", [HandlerResult]),
    HandlerResult.
