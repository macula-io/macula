%%%-------------------------------------------------------------------
%%% @doc
%%% DHT Query Handler Module - handles DHT message forwarding to routing server.
%%%
%%% Responsibilities:
%%% - Forward DHT STORE messages to routing server
%%% - Forward DHT FIND_VALUE messages to routing server, send encoded replies
%%% - Forward DHT FIND_NODE messages to routing server, send encoded replies
%%% - Handle DHT queries from process messages
%%% - Encode replies using protocol encoder
%%% - Handle errors gracefully
%%%
%%% Pattern: Stateless delegation module
%%% - No GenServer (no state to manage)
%%% - Pure functions forwarding to routing server
%%% - Consistent error handling ({ok, Result} | {error, Reason})
%%%
%%% Extracted from macula_gateway.erl (Phase 10)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_dht).

%% API
-export([
    handle_store/2,
    handle_find_value/2,
    handle_find_node/2,
    handle_query/3,
    lookup_value/1,
    forward_publish_to_bootstrap/1,
    send_to_peer/3,
    query_peer/3
]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Handle DHT STORE message.
%% Forwards to routing server (no reply sent back).
%% Crashes on routing server failures - this exposes DHT issues immediately.
-spec handle_store(pid(), map()) -> ok.
handle_store(_Stream, StoreMsg) ->
    io:format("[DHT] Processing STORE message: ~p~n", [StoreMsg]),
    %% Forward to routing server (let it crash on errors)
    _Reply = macula_routing_server:handle_message(macula_routing_server, StoreMsg),
    io:format("[DHT] STORE processed successfully~n"),
    %% STORE doesn't require a response to be sent back
    ok.

%% @doc Handle DHT FIND_VALUE message.
%% Forwards to routing server and sends encoded reply over stream.
%% Crashes on routing server or encoding failures - exposes DHT/protocol bugs.
-spec handle_find_value(pid(), map()) -> ok.
handle_find_value(Stream, FindValueMsg) ->
    io:format("[DHT] Processing FIND_VALUE message: ~p~n", [FindValueMsg]),
    %% Forward to routing server (let it crash on errors)
    Reply = macula_routing_server:handle_message(macula_routing_server, FindValueMsg),
    io:format("[DHT] FIND_VALUE processed, reply: ~p~n", [Reply]),

    %% Send reply back over stream (let it crash on errors)
    ReplyBinary = macula_protocol_encoder:encode(find_value_reply, Reply),
    macula_quic:send(Stream, ReplyBinary),
    ok.

%% @doc Handle DHT FIND_NODE message.
%% Forwards to routing server and sends encoded reply over stream.
%% Crashes on routing server or encoding failures - exposes DHT/protocol bugs.
-spec handle_find_node(pid(), map()) -> ok.
handle_find_node(Stream, FindNodeMsg) ->
    io:format("[DHT] Processing FIND_NODE message: ~p~n", [FindNodeMsg]),
    %% Forward to routing server (let it crash on errors)
    Reply = macula_routing_server:handle_message(macula_routing_server, FindNodeMsg),
    io:format("[DHT] FIND_NODE processed, reply: ~p~n", [Reply]),

    %% Send reply back over stream (let it crash on errors)
    ReplyBinary = macula_protocol_encoder:encode(find_node_reply, Reply),
    macula_quic:send(Stream, ReplyBinary),
    ok.

%% @doc Handle DHT query from process message.
%% Decodes query, forwards to routing server, encodes reply, sends to requesting process.
%% Crashes on decode or routing failures - exposes protocol/DHT bugs.
-spec handle_query(pid(), atom(), binary()) -> ok.
handle_query(FromPid, _QueryType, QueryData) ->
    io:format("[DHT] Processing query from process ~p~n", [FromPid]),
    %% Decode the query message (let it crash on decode errors)
    {ok, {MessageType, Message}} = macula_protocol_decoder:decode(QueryData),

    %% Forward to DHT routing server (let it crash on errors)
    Reply = macula_routing_server:handle_message(macula_routing_server, Message),

    %% Encode reply based on message type (let it crash on encoding errors)
    ReplyData = encode_reply_by_type(MessageType, Reply),

    %% Send reply back to requesting process
    FromPid ! {dht_reply, ReplyData},
    ok.

%% @doc Look up a value from the DHT by key.
%% Synchronous lookup from local DHT storage.
%% NOTE: For pubsub, use forward_publish_to_bootstrap/2 instead of looking up
%% subscribers locally - the bootstrap has the complete subscriber list.
%% Returns list of subscribers for the given key.
-spec lookup_value(binary()) -> {ok, list()} | {error, not_found}.
lookup_value(Key) ->
    case whereis(macula_routing_server) of
        undefined ->
            io:format("[DHT] Routing server not running~n"),
            {error, not_found};
        RoutingServerPid ->
            %% K=20 is the standard Kademlia replication factor
            case macula_routing_server:find_value(RoutingServerPid, Key, 20) of
                {ok, []} ->
                    {error, not_found};
                {ok, Value} when is_list(Value) ->
                    {ok, Value};
                {ok, Value} ->
                    %% Single value, wrap in list
                    {ok, [Value]};
                {error, Reason} ->
                    io:format("[DHT] Lookup error: ~p~n", [Reason]),
                    {error, Reason}
            end
    end.

%% @doc Forward a PUBLISH message to the bootstrap gateway for distribution.
%% The bootstrap has all DHT subscriptions and can distribute to all subscribers.
%% This avoids the problem of local DHT not having remote peer subscriptions.
-spec forward_publish_to_bootstrap(map()) -> ok | {error, term()}.
forward_publish_to_bootstrap(PubMsg) ->
    %% Find the connection PID (connects to bootstrap)
    Realm = application:get_env(macula, realm, <<"default">>),
    case gproc:lookup_local_name({connection, Realm}) of
        undefined ->
            io:format("[DHT] No connection to bootstrap found~n"),
            {error, no_connection};
        ConnPid ->
            io:format("[DHT] Forwarding PUBLISH to bootstrap for distribution~n"),
            case macula_connection:send_message(ConnPid, publish, PubMsg) of
                ok ->
                    io:format("[DHT] PUBLISH forwarded to bootstrap successfully~n"),
                    ok;
                {error, Reason} ->
                    io:format("[DHT] Failed to forward PUBLISH to bootstrap: ~p~n", [Reason]),
                    {error, Reason}
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Send DHT message to remote peer (fire-and-forget).
%% Used for STORE operations that don't need a response.
-spec send_to_peer(map(), atom(), map()) -> ok | {error, term()}.
send_to_peer(NodeInfo, MessageType, Message) ->
    %% Extract endpoint - either from endpoint field or construct from address
    Endpoint = case maps:get(endpoint, NodeInfo, undefined) of
        undefined ->
            %% No endpoint, try to construct from address tuple
            case maps:get(address, NodeInfo, undefined) of
                undefined ->
                    undefined;
                {Host, Port} when is_integer(Port) ->
                    %% Construct "host:port" string from address tuple
                    %% Note: peer_connector expects "host:port" format, NOT URL
                    HostBin = case Host of
                        {_, _, _, _} -> list_to_binary(inet:ntoa(Host));  % IPv4
                        {_, _, _, _, _, _, _, _} -> list_to_binary(inet:ntoa(Host));  % IPv6
                        _ when is_list(Host) -> list_to_binary(Host);
                        _ when is_binary(Host) -> Host
                    end,
                    PortBin = integer_to_binary(Port),
                    <<HostBin/binary, ":", PortBin/binary>>;
                HostPortStr when is_binary(HostPortStr) ->
                    %% Address is already a "host:port" string
                    HostPortStr;
                HostPortStr when is_list(HostPortStr) ->
                    %% Address is a "host:port" string (as list), convert to binary
                    list_to_binary(HostPortStr);
                _Other ->
                    undefined
            end;
        Ep -> Ep
    end,

    case Endpoint of
        undefined ->
            {error, no_endpoint};
        _ ->
            %% Send directly via peer connector (establishes QUIC connection)
            io:format("[DHT] Sending ~p to peer ~p~n", [MessageType, Endpoint]),
            macula_peer_connector:send_message(Endpoint, MessageType, Message)
    end.

%% @doc Query remote peer and wait for response.
%% Used for FIND_NODE and FIND_VALUE operations.
-spec query_peer(map(), atom(), map()) -> {ok, term()} | {error, term()}.
query_peer(NodeInfo, MessageType, Message) ->
    %% For now, use send_to_peer (fire-and-forget)
    %% TODO(v0.9.0): Implement request/response pattern with timeout - see TODO.md
    io:format("[DHT] query_peer not yet implemented for ~p, using send_to_peer~n", [MessageType]),
    send_to_peer(NodeInfo, MessageType, Message).

%% @private
%% @doc Encode reply based on message type.
encode_reply_by_type(find_node, Reply) ->
    macula_protocol_encoder:encode(find_node_reply, Reply);
encode_reply_by_type(find_value, Reply) ->
    macula_protocol_encoder:encode(find_value_reply, Reply);
encode_reply_by_type(store, Reply) ->
    macula_protocol_encoder:encode(store, Reply);
encode_reply_by_type(_UnknownType, _Reply) ->
    macula_protocol_encoder:encode(reply, #{error => <<"Unknown DHT message type">>}).
