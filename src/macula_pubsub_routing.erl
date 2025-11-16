%%%-------------------------------------------------------------------
%%% @doc
%%% Pub/Sub routing for multi-hop DHT-routed pub/sub.
%%% Handles wrapping, unwrapping, and routing of PUBLISH messages through
%%% the Kademlia DHT mesh.
%%%
%%% Pattern: Clone of macula_rpc_routing for pub/sub messages
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_routing).

%% API
-export([
    wrap_publish/4,
    route_or_deliver/3,
    should_deliver_locally/2
]).

-include_lib("kernel/include/logger.hrl").
-include("macula_config.hrl").

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Wrap a PUBLISH message in pubsub_route envelope for DHT routing.
-spec wrap_publish(binary(), binary(), macula_protocol_types:publish_msg(), pos_integer()) ->
    macula_protocol_types:pubsub_route_msg().
wrap_publish(SourceNodeId, DestinationNodeId, PublishMsg, MaxHops)
    when is_binary(SourceNodeId), is_binary(DestinationNodeId) ->
    %% Extract topic from publish message
    Topic = maps:get(<<"topic">>, PublishMsg),
    #{
        <<"destination_node_id">> => DestinationNodeId,
        <<"source_node_id">> => SourceNodeId,
        <<"hop_count">> => 0,
        <<"max_hops">> => MaxHops,
        <<"topic">> => Topic,
        <<"payload">> => PublishMsg
    }.

%% @doc Determine if this node should deliver the message locally or forward it.
-spec should_deliver_locally(binary(), macula_protocol_types:pubsub_route_msg()) -> boolean().
should_deliver_locally(LocalNodeId, PubSubRouteMsg) ->
    %% MessagePack decoder returns binary keys
    #{<<"destination_node_id">> := DestNodeId} = PubSubRouteMsg,
    LocalNodeId =:= DestNodeId.

%% @doc Route a pubsub_route message: either deliver locally or forward to next hop.
%% Returns one of:
%%   {deliver, Topic, PublishMsg} - Message is for this node
%%   {forward, NextHopNodeInfo, UpdatedPubSubRouteMsg} - Forward to next hop
%%   {error, Reason} - Cannot route (TTL exceeded, no route, etc.)
-spec route_or_deliver(binary(), macula_protocol_types:pubsub_route_msg(), pid()) ->
    {deliver, binary(), map()} |
    {forward, macula_routing_bucket:node_info(), macula_protocol_types:pubsub_route_msg()} |
    {error, term()}.
route_or_deliver(LocalNodeId, PubSubRouteMsg, RoutingServerPid) ->
    %% MessagePack decoder returns binary keys, not atoms
    #{<<"destination_node_id">> := DestNodeId,
      <<"source_node_id">> := SourceNodeId,
      <<"hop_count">> := HopCount,
      <<"max_hops">> := MaxHops,
      <<"topic">> := Topic,
      <<"payload">> := Payload} = PubSubRouteMsg,

    %% Check TTL and route
    check_hop_count(HopCount >= MaxHops, LocalNodeId, DestNodeId, SourceNodeId,
                    HopCount, MaxHops, Topic, Payload, PubSubRouteMsg, RoutingServerPid).

%% @doc Check if hop count exceeded.
check_hop_count(true, _LocalNodeId, DestNodeId, SourceNodeId, HopCount, MaxHops, _Topic, _Payload, _PubSubRouteMsg, _RoutingServerPid) ->
    ?LOG_WARNING("Pub/Sub route exceeded max hops (~p >= ~p) from ~p to ~p",
                [HopCount, MaxHops, SourceNodeId, DestNodeId]),
    {error, max_hops_exceeded};
check_hop_count(false, LocalNodeId, DestNodeId, SourceNodeId, HopCount, _MaxHops, Topic, Payload, PubSubRouteMsg, RoutingServerPid) ->
    check_local_delivery(LocalNodeId =:= DestNodeId, DestNodeId, SourceNodeId,
                        HopCount, Topic, Payload, PubSubRouteMsg, RoutingServerPid).

%% @doc Check if this is local delivery.
check_local_delivery(true, _DestNodeId, SourceNodeId, HopCount, Topic, Payload, _PubSubRouteMsg, _RoutingServerPid) ->
    %% Deliver locally
    ?LOG_DEBUG("Pub/Sub route delivering locally: topic ~p from ~p (hops: ~p)",
              [Topic, SourceNodeId, HopCount]),
    {deliver, Topic, Payload};
check_local_delivery(false, DestNodeId, _SourceNodeId, _HopCount, _Topic, _Payload, PubSubRouteMsg, RoutingServerPid) ->
    %% Forward to next hop
    forward_to_next_hop(DestNodeId, PubSubRouteMsg, RoutingServerPid).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Find next hop closer to destination and prepare forwarding.
-spec forward_to_next_hop(binary(), macula_protocol_types:pubsub_route_msg(), pid()) ->
    {forward, macula_routing_bucket:node_info(), macula_protocol_types:pubsub_route_msg()} |
    {error, term()}.
forward_to_next_hop(DestNodeId, PubSubRouteMsg, RoutingServerPid) ->
    %% Query routing table for closest node to destination
    %% K=3 gives us some redundancy if first hop fails
    case macula_routing_server:find_closest(RoutingServerPid, DestNodeId, 3) of
        [] ->
            ?LOG_ERROR("No route to destination: ~p", [DestNodeId]),
            {error, no_route};
        [NextHop | _] ->
            %% Increment hop count (MessagePack uses binary keys)
            #{<<"hop_count">> := HopCount} = PubSubRouteMsg,
            UpdatedMsg = PubSubRouteMsg#{<<"hop_count">> => HopCount + 1},

            ?LOG_DEBUG("Forwarding Pub/Sub route to next hop ~p (destination: ~p, hops: ~p)",
                      [maps:get(node_id, NextHop), DestNodeId, HopCount + 1]),

            {forward, NextHop, UpdatedMsg}
    end.
