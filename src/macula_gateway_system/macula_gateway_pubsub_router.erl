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
    distribute/5
]).

%% Exported for testing
-ifdef(TEST).
-export([
    deliver_to_local_subscribers/2,
    route_to_remote_subscribers/5,
    parse_endpoint/1,
    parse_endpoint_host/2
]).
-endif.

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Distribute pub/sub message to both local and remote subscribers.
%% Uses DHT routing for remote subscribers (multi-hop Kademlia).
%% For connected clients, uses existing bidirectional streams instead of mesh connections.
-spec distribute(
    LocalSubscribers :: [quicer:stream_handle()],
    PubMsg :: map(),
    LocalNodeId :: binary(),
    Mesh :: pid(),
    Clients :: pid()
) -> ok.
distribute(LocalSubscribers, PubMsg, LocalNodeId, Mesh, Clients) ->
    Topic = maps:get(<<"topic">>, PubMsg),

    %% Deliver to local and remote subscribers
    deliver_to_local_subscribers(LocalSubscribers, PubMsg),
    route_to_remote_subscribers(Topic, PubMsg, LocalNodeId, Mesh, Clients),

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
%% Handles both QUIC stream references and handler PIDs.
-spec send_to_local_stream(quicer:stream_handle() | pid(), binary()) -> ok.
send_to_local_stream(Stream, Binary) when is_pid(Stream) ->
    %% Stream is a handler PID - send Erlang message
    %% Decode the binary back to map for Erlang message
    case macula_protocol_decoder:decode(Binary) of
        {ok, {publish, PubMsg}} ->
            Topic = maps:get(<<"topic">>, PubMsg),
            Payload = maps:get(<<"payload">>, PubMsg),
            Stream ! {publish, Topic, Payload},
            io:format("[PubSubRouter] Successfully sent Erlang message to handler PID ~p~n", [Stream]),
            ok;
        {error, DecodeReason} ->
            io:format("[PubSubRouter] Failed to decode message for handler PID ~p: ~p~n", [Stream, DecodeReason]),
            ok
    end;
send_to_local_stream(Stream, Binary) ->
    %% Stream is a QUIC reference - send via QUIC
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
%% @doc Route message to remote subscribers via bootstrap gateway.
%% Instead of querying local DHT (which doesn't have remote subscriptions),
%% we forward the PUBLISH to the bootstrap which has all subscriptions.
-spec route_to_remote_subscribers(binary(), map(), binary(), pid(), pid()) -> ok.
route_to_remote_subscribers(_Topic, PubMsg, _LocalNodeId, _Mesh, _Clients) ->
    %% Forward the PUBLISH to the bootstrap gateway
    %% The bootstrap has the complete DHT with all subscriptions from all peers
    %% It will look up subscribers and distribute the message
    case macula_gateway_dht:forward_publish_to_bootstrap(PubMsg) of
        ok ->
            io:format("[PubSubRouter] Forwarded PUBLISH to bootstrap for distribution~n"),
            ok;
        {error, no_connection} ->
            %% We're probably the bootstrap node - try local DHT lookup
            io:format("[PubSubRouter] No bootstrap connection (we may be bootstrap), trying local DHT~n"),
            route_via_local_dht(_Topic, PubMsg, _LocalNodeId, _Mesh, _Clients);
        {error, Reason} ->
            io:format("[PubSubRouter] Failed to forward to bootstrap: ~p~n", [Reason]),
            ok
    end.

%% @private
%% @doc Fallback: route via local DHT lookup (used by bootstrap node).
-spec route_via_local_dht(binary(), map(), binary(), pid(), pid()) -> ok.
route_via_local_dht(Topic, PubMsg, LocalNodeId, Mesh, Clients) ->
    TopicKey = crypto:hash(sha256, Topic),
    case macula_gateway_dht:lookup_value(TopicKey) of
        {ok, RemoteSubscribers} ->
            io:format("[PubSubRouter] Found ~p remote subscriber(s) in local DHT~n", [length(RemoteSubscribers)]),
            route_to_each_subscriber(RemoteSubscribers, Topic, PubMsg, LocalNodeId, Mesh, Clients);
        {error, not_found} ->
            io:format("[PubSubRouter] No remote subscribers found in local DHT~n"),
            ok
    end.

%% @private
%% @doc Route message to each remote subscriber via DHT.
-spec route_to_each_subscriber([map()], binary(), map(), binary(), pid(), pid()) -> ok.
route_to_each_subscriber([], _Topic, _PubMsg, _LocalNodeId, _Mesh, _Clients) ->
    io:format("[PubSubRouter] Finished routing to all subscribers~n"),
    ok;
route_to_each_subscriber([Subscriber | Rest], Topic, PubMsg, LocalNodeId, Mesh, Clients) ->
    io:format("[PubSubRouter] Processing subscriber ~p of ~p remaining, data: ~p~n",
             [1, length(Rest) + 1, Subscriber]),
    route_to_single_subscriber(Subscriber, Topic, PubMsg, LocalNodeId, Mesh, Clients),
    route_to_each_subscriber(Rest, Topic, PubMsg, LocalNodeId, Mesh, Clients).

%% @private
%% @doc Route message to a single remote subscriber (if node_id present).
%% Checks if subscriber is a connected client first - if so, uses existing bidirectional stream.
%% Otherwise creates mesh connection.
%% Handles both atom keys (from local DHT storage) and binary keys (from protocol).
-spec route_to_single_subscriber(map(), binary(), map(), binary(), pid(), pid()) -> ok.
route_to_single_subscriber(#{node_id := DestNodeId} = Subscriber, Topic, PubMsg, LocalNodeId, Mesh, Clients) ->
    %% Atom key version (from local DHT storage)
    route_to_subscriber_impl(DestNodeId, Subscriber, node_id, Topic, PubMsg, LocalNodeId, Mesh, Clients);
route_to_single_subscriber(#{<<"node_id">> := DestNodeId} = Subscriber, Topic, PubMsg, LocalNodeId, Mesh, Clients) ->
    %% Binary key version (from protocol)
    route_to_subscriber_impl(DestNodeId, Subscriber, <<"node_id">>, Topic, PubMsg, LocalNodeId, Mesh, Clients);
route_to_single_subscriber(Subscriber, _Topic, _PubMsg, _LocalNodeId, _Mesh, _Clients) ->
    io:format("[PubSubRouter] Subscriber missing node_id, skipping: ~p~n", [Subscriber]),
    ok.

%% @private
%% @doc Implementation of routing to a single subscriber.
-spec route_to_subscriber_impl(binary(), map(), atom() | binary(), binary(), map(), binary(), pid(), pid()) -> ok.
route_to_subscriber_impl(DestNodeId, _Subscriber, _EndpointKey, _Topic, _PubMsg, LocalNodeId, _Mesh, _Clients)
    when DestNodeId =:= LocalNodeId ->
    %% SKIP: This is our own node_id - already delivered locally
    io:format("[PubSubRouter] Skipping self-routing (destination = local node)~n"),
    ok;
route_to_subscriber_impl(DestNodeId, Subscriber, EndpointKey, Topic, PubMsg, LocalNodeId, Mesh, Clients) ->
    Payload = maps:get(<<"payload">>, PubMsg),
    Qos = maps:get(<<"qos">>, PubMsg, 0),

    %% DEBUG: Log the node_id from DHT (should be raw 32-byte binary)
    io:format("[PubSubRouter DEBUG] DestNodeId size: ~p bytes, hex: ~s~n",
             [byte_size(DestNodeId), binary:encode_hex(DestNodeId)]),

    %% Create PUBLISH message for this subscriber
    PublishMsg = #{
        <<"topic">> => Topic,
        <<"payload">> => Payload,
        <<"qos">> => Qos,
        <<"retain">> => false,
        <<"message_id">> => crypto:strong_rand_bytes(16)
    },

    %% Check if this is a connected client (has existing bidirectional stream)
    case macula_gateway_clients:get_client_stream(Clients, DestNodeId) of
        {ok, Stream} ->
            %% Subscriber is a connected client - send directly via their stream
            io:format("[PubSubRouter] ✅ Routing to connected client ~s via existing stream~n",
                     [binary:encode_hex(DestNodeId)]),
            send_to_client_stream(Stream, PublishMsg);
        not_found ->
            %% Not a connected client - must be remote gateway, use mesh connection
            io:format("[PubSubRouter] ❌ Client stream NOT FOUND for ~s, routing via mesh~n",
                     [binary:encode_hex(DestNodeId)]),

            %% DEBUG: List all stored client streams to compare
            io:format("[PubSubRouter DEBUG] Getting all client stream keys...~n"),
            AllKeys = macula_gateway_clients:get_all_node_ids(Clients),
            io:format("[PubSubRouter DEBUG] Stored client stream node_ids:~n"),
            lists:foreach(fun(KeyNodeId) ->
                io:format("  - ~s (size: ~p bytes)~n", [binary:encode_hex(KeyNodeId), byte_size(KeyNodeId)])
            end, AllKeys),
            io:format("[PubSubRouter DEBUG] Looking for: ~s (size: ~p bytes)~n",
                     [binary:encode_hex(DestNodeId), byte_size(DestNodeId)]),

            %% Get endpoint using same key type as node_id (atom or binary)
            EndpointUrl = case EndpointKey of
                endpoint -> maps:get(endpoint, Subscriber, undefined);
                <<"endpoint">> -> maps:get(<<"endpoint">>, Subscriber, undefined);
                node_id -> maps:get(endpoint, Subscriber, undefined);
                <<"node_id">> -> maps:get(<<"endpoint">>, Subscriber, undefined)
            end,
            PubSubRouteMsg = macula_pubsub_routing:wrap_publish(LocalNodeId, DestNodeId, PublishMsg, 10),
            send_via_dht(DestNodeId, EndpointUrl, PubSubRouteMsg, Mesh)
    end.

%% @private
%% @doc Send pubsub_route message via DHT mesh connection.
%% Parses endpoint URL to extract address and port.
-spec send_via_dht(binary(), binary(), map(), pid()) -> ok.
send_via_dht(DestNodeId, EndpointUrl, PubSubRouteMsg, Mesh) ->
    case parse_endpoint(EndpointUrl) of
        {ok, {Address, Port}} ->
            case macula_gateway_mesh:get_or_create_connection(Mesh, DestNodeId, {Address, Port}) of
                {ok, Stream} ->
                    send_route_message(Stream, PubSubRouteMsg, DestNodeId);
                {error, Reason} ->
                    io:format("[PubSubRouter] Failed to get connection for ~s: ~p~n",
                             [binary:encode_hex(DestNodeId), Reason]),
                    ok
            end;
        {error, ParseReason} ->
            io:format("[PubSubRouter] Failed to parse endpoint ~s: ~p~n",
                     [EndpointUrl, ParseReason]),
            ok
    end.

%% @private
%% @doc Send PUBLISH message directly to connected client stream.
-spec send_to_client_stream(quicer:stream_handle(), map()) -> ok.
send_to_client_stream(Stream, PublishMsg) ->
    PubBinary = macula_protocol_encoder:encode(publish, PublishMsg),
    case macula_quic:send(Stream, PubBinary) of
        ok ->
            io:format("[PubSubRouter] Sent PUBLISH to connected client stream~n"),
            ok;
        {error, Reason} ->
            io:format("[PubSubRouter] Failed to send to client stream: ~p~n", [Reason]),
            ok
    end.

%% @private
%% @doc Send encoded pubsub_route message to stream.
%% NOTE: We DON'T close streams here - the stream stays open for the
%% QUIC connection's lifetime. Closing a stream doesn't close the connection,
%% and quicer handles stream cleanup on connection close.
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

%%%===================================================================
%%% Node ID Helpers (Removed - not needed)
%%%===================================================================
%% Per Kademlia spec: node IDs are always 256-bit (32-byte) raw binary.
%% Hex encoding is ONLY for display/logging, never for internal storage.
%% DHT stores and returns raw binary node IDs.

%%%===================================================================
%%% Endpoint Parsing Helper
%%%===================================================================

%% @private
%% @doc Parse endpoint URL (e.g., "https://192.168.1.100:4433") to {Address, Port}.
%% Returns {ok, {Address, Port}} or {error, Reason}.
-spec parse_endpoint(binary()) -> {ok, {inet:ip_address() | list(), inet:port_number()}} | {error, term()}.
parse_endpoint(EndpointUrl) when is_binary(EndpointUrl) ->
    case uri_string:parse(EndpointUrl) of
        #{host := Host, port := Port} when is_integer(Port) ->
            parse_endpoint_host(Host, Port);
        #{host := Host} ->
            parse_endpoint_host(Host, 4433);  % Default QUIC port
        _Other ->
            {error, {invalid_endpoint_format, EndpointUrl}}
    end;
parse_endpoint(_Other) ->
    {error, invalid_endpoint_type}.

%% @private
%% Parse hostname to IP address tuple.
parse_endpoint_host(Host, Port) when is_list(Host) ->
    case inet:parse_address(Host) of
        {ok, IpTuple} ->
            {ok, {IpTuple, Port}};
        {error, _} ->
            %% Not an IP address, treat as hostname
            {ok, {Host, Port}}
    end;
parse_endpoint_host(Host, Port) when is_binary(Host) ->
    parse_endpoint_host(binary_to_list(Host), Port).
