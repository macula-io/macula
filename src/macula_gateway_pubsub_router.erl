%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Gateway Pub/Sub Router - DHT-Routed Message Distribution
%%%
%%% Handles distribution of pub/sub messages to both local and remote
%%% subscribers using multi-hop Kademlia DHT routing (v0.7.8+).
%%%
%%% Responsibilities:
%%%   - Deliver messages to local subscribers via QUIC streams
%%%   - Query DHT for remote subscribers
%%%   - Route messages via DHT multi-hop (pubsub_route protocol)
%%%   - Wrap PUBLISH messages in pubsub_route envelopes
%%%
%%% Extracted from macula_gateway.erl (v0.7.9) for better separation of concerns.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_pubsub_router).

%% API
-export([
    distribute/4
]).

%% Exported for testing
-ifdef(TEST).
-export([
    deliver_to_local_subscribers/2,
    route_to_remote_subscribers/4
]).
-endif.

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Distribute pub/sub message to both local and remote subscribers.
%% Uses DHT routing for remote subscribers (multi-hop Kademlia).
-spec distribute(
    LocalSubscribers :: [quicer:stream_handle()],
    PubMsg :: map(),
    LocalNodeId :: binary(),
    Mesh :: pid()
) -> ok.
distribute(LocalSubscribers, PubMsg, LocalNodeId, Mesh) ->
    Topic = maps:get(<<"topic">>, PubMsg),

    %% Deliver to local and remote subscribers
    deliver_to_local_subscribers(LocalSubscribers, PubMsg),
    route_to_remote_subscribers(Topic, PubMsg, LocalNodeId, Mesh),

    io:format("[PubSubRouter] Finished distributing message to ~p local + DHT remote subscribers~n",
             [length(LocalSubscribers)]),
    ok.

%%%===================================================================
%%% Internal Functions - Local Delivery
%%%===================================================================

%% @private
%% @doc Deliver message to local subscribers via QUIC streams.
-spec deliver_to_local_subscribers([quicer:stream_handle()], map()) -> ok.
deliver_to_local_subscribers([], _PubMsg) ->
    ok;
deliver_to_local_subscribers(Subscribers, PubMsg) ->
    PubBinary = macula_protocol_encoder:encode(publish, PubMsg),
    io:format("[PubSubRouter] Found ~p local subscribers~n", [length(Subscribers)]),
    lists:foreach(fun(Stream) -> send_to_local_stream(Stream, PubBinary) end, Subscribers),
    ok.

%% @private
%% @doc Send message to a single local subscriber stream.
-spec send_to_local_stream(quicer:stream_handle(), binary()) -> ok.
send_to_local_stream(Stream, Binary) ->
    case macula_quic:send(Stream, Binary) of
        ok ->
            io:format("[PubSubRouter] Successfully sent to local stream ~p~n", [Stream]),
            ok;
        {error, Reason} ->
            io:format("[PubSubRouter] Failed to send to local stream ~p: ~p~n", [Stream, Reason]),
            ok
    end.

%%%===================================================================
%%% Internal Functions - DHT Remote Routing
%%%===================================================================

%% @private
%% @doc Route message to remote subscribers via DHT multi-hop routing.
-spec route_to_remote_subscribers(binary(), map(), binary(), pid()) -> ok.
route_to_remote_subscribers(Topic, PubMsg, LocalNodeId, Mesh) ->
    TopicKey = crypto:hash(sha256, Topic),
    case macula_gateway_dht:lookup_value(TopicKey) of
        {ok, RemoteSubscribers} ->
            io:format("[PubSubRouter] Found ~p remote subscriber(s) in DHT~n", [length(RemoteSubscribers)]),
            route_to_each_subscriber(RemoteSubscribers, Topic, PubMsg, LocalNodeId, Mesh);
        {error, not_found} ->
            io:format("[PubSubRouter] No remote subscribers found in DHT~n"),
            ok
    end.

%% @private
%% @doc Route message to each remote subscriber via DHT.
-spec route_to_each_subscriber([map()], binary(), map(), binary(), pid()) -> ok.
route_to_each_subscriber([], _Topic, _PubMsg, _LocalNodeId, _Mesh) ->
    ok;
route_to_each_subscriber([Subscriber | Rest], Topic, PubMsg, LocalNodeId, Mesh) ->
    route_to_single_subscriber(Subscriber, Topic, PubMsg, LocalNodeId, Mesh),
    route_to_each_subscriber(Rest, Topic, PubMsg, LocalNodeId, Mesh).

%% @private
%% @doc Route message to a single remote subscriber (if node_id present).
-spec route_to_single_subscriber(map(), binary(), map(), binary(), pid()) -> ok.
route_to_single_subscriber(#{<<"node_id">> := DestNodeId}, Topic, PubMsg, LocalNodeId, Mesh) ->
    Payload = maps:get(<<"payload">>, PubMsg),
    Qos = maps:get(<<"qos">>, PubMsg, 0),

    %% Create PUBLISH message for this subscriber
    PublishMsg = #{
        <<"topic">> => Topic,
        <<"payload">> => Payload,
        <<"qos">> => Qos,
        <<"retain">> => false,
        <<"message_id">> => crypto:strong_rand_bytes(16)
    },

    %% Wrap in pubsub_route envelope and send via DHT
    PubSubRouteMsg = macula_pubsub_routing:wrap_publish(LocalNodeId, DestNodeId, PublishMsg, 10),
    send_via_dht(DestNodeId, PubSubRouteMsg, Mesh);
route_to_single_subscriber(_Subscriber, _Topic, _PubMsg, _LocalNodeId, _Mesh) ->
    io:format("[PubSubRouter] Subscriber missing node_id, skipping~n"),
    ok.

%% @private
%% @doc Send pubsub_route message via DHT mesh connection.
-spec send_via_dht(binary(), map(), pid()) -> ok.
send_via_dht(DestNodeId, PubSubRouteMsg, Mesh) ->
    case macula_gateway_mesh:get_or_create_connection(Mesh, DestNodeId, undefined) of
        {ok, Stream} ->
            send_route_message(Stream, PubSubRouteMsg, DestNodeId);
        {error, Reason} ->
            io:format("[PubSubRouter] Failed to get connection for ~s: ~p~n",
                     [binary:encode_hex(DestNodeId), Reason]),
            ok
    end.

%% @private
%% @doc Send encoded pubsub_route message to stream.
-spec send_route_message(quicer:stream_handle(), map(), binary()) -> ok.
send_route_message(Stream, PubSubRouteMsg, DestNodeId) ->
    RouteMsg = macula_protocol_encoder:encode(pubsub_route, PubSubRouteMsg),
    case macula_quic:send(Stream, RouteMsg) of
        ok ->
            io:format("[PubSubRouter] Sent pubsub_route to ~s via DHT~n",
                     [binary:encode_hex(DestNodeId)]),
            ok;
        {error, Reason} ->
            io:format("[PubSubRouter] Failed to send pubsub_route: ~p~n", [Reason]),
            ok
    end.
