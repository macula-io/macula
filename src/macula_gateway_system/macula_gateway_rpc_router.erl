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

-include_lib("kernel/include/logger.hrl").

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
%% Authorization check added in v0.17.0.
-spec handle_routed_call(map(), map(), binary(), pid(), pid()) ->
    ok | {error, term()}.
handle_routed_call(CallMsg, RpcRouteMsg, NodeId, RpcPid, MeshPid) ->
    ?LOG_DEBUG("Processing routed CALL message"),

    %% Extract source node ID from rpc_route envelope
    #{<<"source_node_id">> := SourceNodeId} = RpcRouteMsg,

    %% Extract call data
    #{<<"procedure">> := Procedure,
      <<"args">> := ArgsJson,
      <<"call_id">> := CallId} = CallMsg,

    %% Authorization check (v0.17.0+)
    CallerDID = extract_caller_did(RpcRouteMsg, CallMsg),
    UcanToken = maps:get(<<"ucan_token">>, CallMsg, undefined),
    case macula_authorization:check_rpc_call(CallerDID, Procedure, UcanToken, #{}) of
        {ok, authorized} ->
            %% Authorized - look up handler
            case macula_gateway_rpc:get_handler(RpcPid, Procedure) of
                not_found ->
                    ?LOG_WARNING("No handler for procedure: ~p", [Procedure]),
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
            end;
        {error, Reason} ->
            ?LOG_WARNING("RPC call to ~s denied: ~p (caller: ~s)", [Procedure, Reason, CallerDID]),
            ErrorReply = #{
                call_id => CallId,
                error => #{
                    code => <<"unauthorized">>,
                    message => iolist_to_binary(io_lib:format("~p", [Reason]))
                }
            },
            send_reply_via_routing(ErrorReply, SourceNodeId, NodeId, MeshPid)
    end.

%% @doc Handle routed REPLY message delivered locally.
%% Routes to connection via gproc (local node) or to client stream (remote client).
-spec handle_routed_reply(map(), map(), binary(), map()) ->
    ok | {error, term()}.
handle_routed_reply(ReplyMsg, RpcRouteMsg, NodeId, ClientStreams) ->
    ?LOG_DEBUG("Processing routed REPLY message"),

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
    ?LOG_DEBUG("Sending reply via routing to ~p", [binary:encode_hex(DestNodeId)]),
    do_send_reply_via_routing(whereis(macula_routing_server), ReplyMsg, DestNodeId, NodeId, MeshPid).

%% @doc Forward rpc_route message to next hop.
%% Uses async (fire-and-forget) pattern to avoid blocking.
%% Graceful error handling - logs errors but doesn't crash gateway.
-spec forward_rpc_route(map(), map(), pid()) -> ok.
forward_rpc_route(NextHopNodeInfo, RpcRouteMsg, MeshPid) ->
    ?LOG_DEBUG("Forwarding rpc_route to next hop (async)"),

    %% Extract next hop info (routing_bucket:node_info uses atom keys, not binary)
    #{node_id := NextHopNodeId,
      address := Address} = NextHopNodeInfo,

    %% Encode message
    EncodedMsg = macula_protocol_encoder:encode(rpc_route, RpcRouteMsg),

    %% Send asynchronously - does NOT block
    %% Connection creation and sending happens in a spawned process
    macula_gateway_mesh:send_async(MeshPid, NextHopNodeId, Address, EncodedMsg),
    ?LOG_DEBUG("Queued rpc_route for async send to ~s",
             [binary:encode_hex(NextHopNodeId)]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Invoke RPC handler and send reply.
invoke_handler_and_reply(Handler, ArgsJson, CallId, SourceNodeId, NodeId, MeshPid) ->
    ExecutionResult = catch execute_rpc_handler(Handler, ArgsJson),
    handle_rpc_execution_result(ExecutionResult, CallId, SourceNodeId, NodeId, MeshPid).

%% @private Execute RPC handler with decoded args
execute_rpc_handler(Handler, ArgsJson) ->
    Args = json:decode(ArgsJson),
    Handler(Args).

%% @private Handle RPC execution result
handle_rpc_execution_result({'EXIT', {Reason, _Stack}}, CallId, SourceNodeId, NodeId, MeshPid) ->
    ?LOG_ERROR("Handler error: ~p", [Reason]),
    ErrorReply = #{
        call_id => CallId,
        error => #{
            code => <<"handler_error">>,
            message => format_error(error, Reason)
        }
    },
    send_reply_via_routing(ErrorReply, SourceNodeId, NodeId, MeshPid);
handle_rpc_execution_result({'EXIT', Reason}, CallId, SourceNodeId, NodeId, MeshPid) ->
    ?LOG_ERROR("Handler error: ~p", [Reason]),
    ErrorReply = #{
        call_id => CallId,
        error => #{
            code => <<"handler_error">>,
            message => format_error(error, Reason)
        }
    },
    send_reply_via_routing(ErrorReply, SourceNodeId, NodeId, MeshPid);
handle_rpc_execution_result(Result, CallId, SourceNodeId, NodeId, MeshPid) ->
    Reply = #{
        call_id => CallId,
        result => encode_json(Result)
    },
    ?LOG_DEBUG("Handler invoked successfully"),
    send_reply_via_routing(Reply, SourceNodeId, NodeId, MeshPid).

%% @private
%% @doc Deliver reply to local connection via gproc.
%% Sends the full rpc_route message to all local connections.
deliver_reply_to_local_connection(RpcRouteMsg) ->
    ?LOG_DEBUG("Delivering reply to local connections"),
    Pids = gproc:lookup_pids({p, l, macula_connection}),
    do_deliver_to_local_connections(Pids, RpcRouteMsg).

%% @private
%% @doc Deliver reply to remote client via stream.
%% Sends the full rpc_route message (not just the reply).
deliver_reply_to_remote_client(_ReplyMsg, RpcRouteMsg, ClientStreams) ->
    ConnectionId = maps:get(<<"connection_id">>, RpcRouteMsg, undefined),
    do_deliver_to_remote_client(ConnectionId, RpcRouteMsg, ClientStreams).

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

%%%===================================================================
%%% Routing reply helpers
%%%===================================================================

%% @private Routing server not available
do_send_reply_via_routing(undefined, _ReplyMsg, _DestNodeId, _NodeId, _MeshPid) ->
    ?LOG_ERROR("Routing server not available"),
    error(routing_server_not_available);
%% @private Route the reply
do_send_reply_via_routing(RoutingServerPid, ReplyMsg, DestNodeId, NodeId, MeshPid) ->
    RpcRouteMsg = macula_rpc_routing:wrap_reply(NodeId, DestNodeId, ReplyMsg, 10),
    handle_route_result(macula_rpc_routing:route_or_deliver(NodeId, RpcRouteMsg, RoutingServerPid), MeshPid).

handle_route_result({deliver, _, _}, _MeshPid) ->
    ?LOG_WARNING("Reply is for local node (unexpected)"),
    ok;
handle_route_result({forward, NextHopNodeInfo, UpdatedRpcRouteMsg}, MeshPid) ->
    forward_rpc_route(NextHopNodeInfo, UpdatedRpcRouteMsg, MeshPid);
handle_route_result({error, Reason}, _MeshPid) ->
    ?LOG_ERROR("Routing error: ~p", [Reason]),
    error({routing_failed, Reason}).

%%%===================================================================
%%% Local connection delivery helpers
%%%===================================================================

%% @private No local connections
do_deliver_to_local_connections([], _RpcRouteMsg) ->
    ?LOG_WARNING("No local connection process found"),
    {error, connection_not_found};
%% @private Deliver to all local connections
do_deliver_to_local_connections(Pids, RpcRouteMsg) ->
    ?LOG_DEBUG("Found ~p local connection process(es)", [length(Pids)]),
    lists:foreach(fun(Pid) -> gen_server:cast(Pid, {rpc_route_reply, RpcRouteMsg}) end, Pids),
    ok.

%%%===================================================================
%%% Remote client delivery helpers
%%%===================================================================

%% @private No connection_id in message
do_deliver_to_remote_client(undefined, _RpcRouteMsg, _ClientStreams) ->
    ?LOG_WARNING("No connection_id in rpc_route"),
    {error, no_connection_id};
%% @private Look up client stream
do_deliver_to_remote_client(ConnectionId, RpcRouteMsg, ClientStreams) ->
    StreamPid = maps:get(ConnectionId, ClientStreams, undefined),
    send_to_client_stream(StreamPid, RpcRouteMsg, ConnectionId).

send_to_client_stream(undefined, _RpcRouteMsg, ConnectionId) ->
    ?LOG_WARNING("Client stream not found for connection ~p", [ConnectionId]),
    {error, client_not_found};
send_to_client_stream(StreamPid, RpcRouteMsg, _ConnectionId) ->
    RpcRouteBinary = macula_protocol_encoder:encode(rpc_route, RpcRouteMsg),
    macula_quic:send(StreamPid, RpcRouteBinary),
    ?LOG_DEBUG("Delivered reply to remote client"),
    ok.

%%%===================================================================
%%% Authorization Helpers (v0.17.0+)
%%%===================================================================

%% @private
%% @doc Extract caller DID from rpc_route envelope or call message.
%% Priority: caller_did from CallMsg > from RpcRouteMsg > derive from source_node
-spec extract_caller_did(map(), map()) -> binary().
extract_caller_did(RpcRouteMsg, CallMsg) ->
    case maps:get(<<"caller_did">>, CallMsg, undefined) of
        undefined ->
            case maps:get(<<"caller_did">>, RpcRouteMsg, undefined) of
                undefined ->
                    %% Fallback: derive from source node ID (temporary until TLS cert extraction)
                    SourceNodeId = maps:get(<<"source_node_id">>, RpcRouteMsg, <<"unknown">>),
                    <<"did:macula:", SourceNodeId/binary>>;
                CallerDID ->
                    CallerDID
            end;
        CallerDID ->
            CallerDID
    end.
