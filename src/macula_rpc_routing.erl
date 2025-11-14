%%%-------------------------------------------------------------------
%%% @doc
%%% RPC routing for multi-hop DHT-routed RPC.
%%% Handles wrapping, unwrapping, and routing of RPC messages through
%%% the Kademlia DHT mesh.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_routing).

%% API
-export([
    wrap_call/4,
    wrap_reply/4,
    route_or_deliver/3,
    should_deliver_locally/2
]).

-include_lib("kernel/include/logger.hrl").
-include("macula_config.hrl").

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Wrap a CALL message in rpc_route envelope for DHT routing.
-spec wrap_call(binary(), binary(), macula_protocol_types:call_msg(), pos_integer()) ->
    macula_protocol_types:rpc_route_msg().
wrap_call(SourceNodeId, DestinationNodeId, CallMsg, MaxHops)
    when is_binary(SourceNodeId), is_binary(DestinationNodeId) ->
    #{
        <<"destination_node_id">> => DestinationNodeId,
        <<"source_node_id">> => SourceNodeId,
        <<"hop_count">> => 0,
        <<"max_hops">> => MaxHops,
        <<"payload_type">> => <<"call">>,
        <<"payload">> => CallMsg
    }.

%% @doc Wrap a REPLY message in rpc_route envelope for DHT routing back to caller.
-spec wrap_reply(binary(), binary(), macula_protocol_types:reply_msg(), pos_integer()) ->
    macula_protocol_types:rpc_route_msg().
wrap_reply(SourceNodeId, DestinationNodeId, ReplyMsg, MaxHops)
    when is_binary(SourceNodeId), is_binary(DestinationNodeId) ->
    #{
        <<"destination_node_id">> => DestinationNodeId,
        <<"source_node_id">> => SourceNodeId,
        <<"hop_count">> => 0,
        <<"max_hops">> => MaxHops,
        <<"payload_type">> => <<"reply">>,
        <<"payload">> => ReplyMsg
    }.

%% @doc Determine if this node should deliver the message locally or forward it.
-spec should_deliver_locally(binary(), macula_protocol_types:rpc_route_msg()) -> boolean().
should_deliver_locally(LocalNodeId, RpcRouteMsg) ->
    %% MessagePack decoder returns binary keys
    #{<<"destination_node_id">> := DestNodeId} = RpcRouteMsg,
    LocalNodeId =:= DestNodeId.

%% @doc Route an rpc_route message: either deliver locally or forward to next hop.
%% Returns one of:
%%   {deliver, PayloadType, Payload} - Message is for this node
%%   {forward, NextHopNodeInfo, UpdatedRpcRouteMsg} - Forward to next hop
%%   {error, Reason} - Cannot route (TTL exceeded, no route, etc.)
-spec route_or_deliver(binary(), macula_protocol_types:rpc_route_msg(), pid()) ->
    {deliver, call | reply, map()} |
    {forward, macula_routing_bucket:node_info(), macula_protocol_types:rpc_route_msg()} |
    {error, term()}.
route_or_deliver(LocalNodeId, RpcRouteMsg, RoutingServerPid) ->
    %% MessagePack decoder returns binary keys, not atoms
    #{<<"destination_node_id">> := DestNodeId,
      <<"source_node_id">> := SourceNodeId,
      <<"hop_count">> := HopCount,
      <<"max_hops">> := MaxHops,
      <<"payload_type">> := PayloadType,
      <<"payload">> := Payload} = RpcRouteMsg,

    %% Check TTL and route
    check_hop_count(HopCount >= MaxHops, LocalNodeId, DestNodeId, SourceNodeId,
                    HopCount, MaxHops, PayloadType, Payload, RpcRouteMsg, RoutingServerPid).

%% @doc Check if hop count exceeded.
check_hop_count(true, _LocalNodeId, DestNodeId, SourceNodeId, HopCount, MaxHops, _PayloadType, _Payload, _RpcRouteMsg, _RoutingServerPid) ->
    ?LOG_WARNING("RPC route exceeded max hops (~p >= ~p) from ~p to ~p",
                [HopCount, MaxHops, SourceNodeId, DestNodeId]),
    {error, max_hops_exceeded};
check_hop_count(false, LocalNodeId, DestNodeId, SourceNodeId, HopCount, _MaxHops, PayloadType, Payload, RpcRouteMsg, RoutingServerPid) ->
    check_local_delivery(LocalNodeId =:= DestNodeId, DestNodeId, SourceNodeId,
                        HopCount, PayloadType, Payload, RpcRouteMsg, RoutingServerPid).

%% @doc Check if this is local delivery.
check_local_delivery(true, _DestNodeId, SourceNodeId, HopCount, PayloadType, Payload, _RpcRouteMsg, _RoutingServerPid) ->
    %% Deliver locally
    ?LOG_DEBUG("RPC route delivering locally: ~p from ~p (hops: ~p)",
              [PayloadType, SourceNodeId, HopCount]),
    {deliver, PayloadType, Payload};
check_local_delivery(false, DestNodeId, _SourceNodeId, _HopCount, _PayloadType, _Payload, RpcRouteMsg, RoutingServerPid) ->
    %% Forward to next hop
    forward_to_next_hop(DestNodeId, RpcRouteMsg, RoutingServerPid).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Find next hop closer to destination and prepare forwarding.
-spec forward_to_next_hop(binary(), macula_protocol_types:rpc_route_msg(), pid()) ->
    {forward, macula_routing_bucket:node_info(), macula_protocol_types:rpc_route_msg()} |
    {error, term()}.
forward_to_next_hop(DestNodeId, RpcRouteMsg, RoutingServerPid) ->
    %% Query routing table for closest node to destination
    %% K=3 gives us some redundancy if first hop fails
    case macula_routing_server:find_closest(RoutingServerPid, DestNodeId, 3) of
        [] ->
            ?LOG_ERROR("No route to destination: ~p", [DestNodeId]),
            {error, no_route};
        [NextHop | _] ->
            %% Increment hop count (MessagePack uses binary keys)
            #{<<"hop_count">> := HopCount} = RpcRouteMsg,
            UpdatedMsg = RpcRouteMsg#{<<"hop_count">> => HopCount + 1},

            ?LOG_DEBUG("Forwarding RPC route to next hop ~p (destination: ~p, hops: ~p)",
                      [maps:get(node_id, NextHop), DestNodeId, HopCount + 1]),

            {forward, NextHop, UpdatedMsg}
    end.
