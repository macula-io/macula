%%%-------------------------------------------------------------------
%%% @doc
%%% RPC Router Module - handles routed RPC messages (CALL/REPLY).
%%%
%%% Responsibilities:
%%% - Process routed CALL messages delivered locally
%%% - Process routed REPLY messages delivered locally
%%% - Send REPLY back via routing path
%%% - Forward rpc_route messages to next hop
%%% - Coordinate between RPC handler, mesh, and routing modules
%%%
%%% Pattern: Stateless delegation module
%%% - No GenServer (no state to manage)
%%% - Pure functions coordinating between modules
%%% - Consistent error handling ({ok, Result} | {error, Reason})
%%%
%%% Extracted from macula_gateway.erl (Phase 11)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_rpc_router).

%% API
-export([
    handle_routed_call/5,
    handle_routed_reply/4,
    send_reply_via_routing/4,
    forward_rpc_route/3
]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Handle routed CALL message delivered locally.
%% Looks up RPC handler, invokes it, sends reply via routing path.
-spec handle_routed_call(map(), map(), binary(), pid(), pid()) ->
    ok | {error, term()}.
handle_routed_call(CallMsg, RpcRouteMsg, NodeId, RpcPid, MeshPid) ->
    io:format("[RPC Router] Processing routed CALL message~n"),

    %% Extract source node ID from rpc_route envelope
    #{<<"source_node_id">> := SourceNodeId} = RpcRouteMsg,

    %% Extract call data
    #{<<"procedure">> := Procedure,
      <<"args">> := ArgsJson,
      <<"call_id">> := CallId} = CallMsg,

    %% Look up handler via macula_gateway_rpc
    case macula_gateway_rpc:get_handler(RpcPid, Procedure) of
        not_found ->
            io:format("[RPC Router] No handler for procedure: ~p~n", [Procedure]),
            ErrorReply = #{
                call_id => CallId,
                error => #{
                    code => <<"no_handler">>,
                    message => <<"No handler registered for ", Procedure/binary>>
                }
            },
            send_reply_via_routing(ErrorReply, SourceNodeId, NodeId, MeshPid);

        {ok, Handler} ->
            invoke_handler_and_reply(Handler, ArgsJson, CallId, SourceNodeId, NodeId, MeshPid)
    end.

%% @doc Handle routed REPLY message delivered locally.
%% Routes to connection via gproc (local node) or to client stream (remote client).
-spec handle_routed_reply(map(), map(), binary(), map()) ->
    ok | {error, term()}.
handle_routed_reply(ReplyMsg, RpcRouteMsg, NodeId, ClientStreams) ->
    io:format("[RPC Router] Processing routed REPLY message~n"),

    %% Extract destination node ID
    #{<<"destination_node_id">> := DestinationNodeId} = RpcRouteMsg,

    %% Check if this is for local node or remote client
    case DestinationNodeId of
        NodeId ->
            %% Local node - send to connection via gproc (send full rpc_route message)
            deliver_reply_to_local_connection(RpcRouteMsg);
        _RemoteNodeId ->
            %% Remote client - send to client stream
            deliver_reply_to_remote_client(ReplyMsg, RpcRouteMsg, ClientStreams)
    end.

%% @doc Send REPLY back via routing path.
%% Wraps reply in rpc_route envelope and routes to destination.
%% Crashes on routing failures - exposes mesh/routing issues immediately.
-spec send_reply_via_routing(map(), binary(), binary(), pid()) -> ok.
send_reply_via_routing(ReplyMsg, DestNodeId, NodeId, MeshPid) ->
    io:format("[RPC Router] Sending reply via routing to ~p~n",
             [binary:encode_hex(DestNodeId)]),

    %% Get routing server PID (let it crash if not available)
    RoutingServerPid = whereis(macula_routing_server),

    case RoutingServerPid of
        undefined ->
            io:format("[RPC Router] Routing server not available~n"),
            error(routing_server_not_available);

        _ ->
            %% Wrap reply in rpc_route envelope (with MaxHops = 10)
            RpcRouteMsg = macula_rpc_routing:wrap_reply(NodeId, DestNodeId, ReplyMsg, 10),

            %% Route the reply (let it crash on errors)
            case macula_rpc_routing:route_or_deliver(NodeId, RpcRouteMsg, RoutingServerPid) of
                {deliver, _, _} ->
                    %% Should not happen for replies going back, but handle it
                    io:format("[RPC Router] Reply is for local node (unexpected)~n"),
                    ok;

                {forward, NextHopNodeInfo, UpdatedRpcRouteMsg} ->
                    %% Forward to next hop (let it crash on errors)
                    forward_rpc_route(NextHopNodeInfo, UpdatedRpcRouteMsg, MeshPid);

                {error, Reason} ->
                    io:format("[RPC Router] Routing error: ~p~n", [Reason]),
                    error({routing_failed, Reason})
            end
    end.

%% @doc Forward rpc_route message to next hop.
%% Uses async (fire-and-forget) pattern to avoid blocking.
%% Graceful error handling - logs errors but doesn't crash gateway.
-spec forward_rpc_route(map(), map(), pid()) -> ok.
forward_rpc_route(NextHopNodeInfo, RpcRouteMsg, MeshPid) ->
    io:format("[RPC Router] Forwarding rpc_route to next hop (async)~n"),

    %% Extract next hop info (routing_bucket:node_info uses atom keys, not binary)
    #{node_id := NextHopNodeId,
      address := Address} = NextHopNodeInfo,

    %% Encode message
    EncodedMsg = macula_protocol_encoder:encode(rpc_route, RpcRouteMsg),

    %% Send asynchronously - does NOT block
    %% Connection creation and sending happens in a spawned process
    macula_gateway_mesh:send_async(MeshPid, NextHopNodeId, Address, EncodedMsg),
    io:format("[RPC Router] Queued rpc_route for async send to ~s~n",
             [binary:encode_hex(NextHopNodeId)]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Invoke RPC handler and send reply.
invoke_handler_and_reply(Handler, ArgsJson, CallId, SourceNodeId, NodeId, MeshPid) ->
    try
        %% Decode JSON args
        Args = json:decode(ArgsJson),

        %% Invoke handler
        Result = Handler(Args),

        %% Create success reply
        Reply = #{
            call_id => CallId,
            result => encode_json(Result)
        },

        io:format("[RPC Router] Handler invoked successfully~n"),
        send_reply_via_routing(Reply, SourceNodeId, NodeId, MeshPid)
    catch
        Class:Reason:Stacktrace ->
            io:format("[RPC Router] Handler error: ~p:~p~n~p~n",
                     [Class, Reason, Stacktrace]),

            %% Create error reply
            ErrorReply = #{
                call_id => CallId,
                error => #{
                    code => <<"handler_error">>,
                    message => format_error(Class, Reason)
                }
            },

            send_reply_via_routing(ErrorReply, SourceNodeId, NodeId, MeshPid)
    end.

%% @private
%% @doc Deliver reply to local connection via gproc.
%% Sends the full rpc_route message to all local connections.
deliver_reply_to_local_connection(RpcRouteMsg) ->
    io:format("[RPC Router] Delivering reply to local connections~n"),

    %% Look up all local macula_connection processes via gproc
    case gproc:lookup_pids({p, l, macula_connection}) of
        [] ->
            io:format("[RPC Router] WARNING: No local connection process found~n"),
            {error, connection_not_found};

        Pids ->
            io:format("[RPC Router] Found ~p local connection process(es)~n", [length(Pids)]),
            %% Send to all local connections (usually just one)
            lists:foreach(fun(Pid) ->
                gen_server:cast(Pid, {rpc_route_reply, RpcRouteMsg})
            end, Pids),
            ok
    end.

%% @private
%% @doc Deliver reply to remote client via stream.
%% Sends the full rpc_route message (not just the reply).
deliver_reply_to_remote_client(_ReplyMsg, RpcRouteMsg, ClientStreams) ->
    %% Extract connection ID from rpc_route
    case maps:get(<<"connection_id">>, RpcRouteMsg, undefined) of
        undefined ->
            io:format("[RPC Router] No connection_id in rpc_route~n"),
            {error, no_connection_id};

        ConnectionId ->
            %% Look up client stream
            case maps:get(ConnectionId, ClientStreams, undefined) of
                undefined ->
                    io:format("[RPC Router] Client stream not found for connection ~p~n",
                             [ConnectionId]),
                    {error, client_not_found};

                StreamPid ->
                    %% Encode and send the full rpc_route message
                    RpcRouteBinary = macula_protocol_encoder:encode(rpc_route, RpcRouteMsg),
                    macula_quic:send(StreamPid, RpcRouteBinary),
                    io:format("[RPC Router] Delivered reply to remote client~n"),
                    ok
            end
    end.

%% @private
%% @doc Encode result to JSON binary.
encode_json(Term) when is_binary(Term) ->
    Term;
encode_json(Term) ->
    macula_utils:encode_json(Term).

%% @private
%% @doc Format error for reply message.
format_error(Class, Reason) ->
    iolist_to_binary(io_lib:format("~p: ~p", [Class, Reason])).
