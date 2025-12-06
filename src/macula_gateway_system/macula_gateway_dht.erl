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

-include_lib("kernel/include/logger.hrl").

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
%% Forwards to routing server asynchronously (fire-and-forget, no reply needed).
%% Uses async handler to prevent blocking the gateway on DHT operations.
-spec handle_store(pid(), map()) -> ok.
handle_store(_Stream, StoreMsg) ->
    %% Forward to routing server asynchronously (fire-and-forget)
    %% STORE doesn't require a response, so we don't wait
    ?LOG_DEBUG("Processing STORE message, forwarding to routing_server (async)"),
    macula_routing_server:handle_message_async(macula_routing_server, StoreMsg),
    ok.

%% @doc Handle DHT FIND_VALUE message.
%% Extracts the key and performs local storage lookup, returning result over stream.
%% The message format from protocol decoder contains a binary key field.
-spec handle_find_value(pid(), map()) -> ok.
handle_find_value(Stream, FindValueMsg) ->
    %% Extract key from message - supports both <<"key">> (protocol) and key (atom)
    Key = maps:get(<<"key">>, FindValueMsg, maps:get(key, FindValueMsg, undefined)),
    ?LOG_INFO("[Gateway DHT] FIND_VALUE request for key_prefix=~p",
              [case Key of
                   undefined -> undefined;
                   K when is_binary(K) -> binary:part(K, 0, min(8, byte_size(K)));
                   _ -> Key
               end]),
    Reply = do_handle_find_value(Key),
    ReplyBinary = macula_protocol_encoder:encode(find_value_reply, Reply),
    macula_quic:send(Stream, ReplyBinary),
    ok.

%% @private Handle FIND_VALUE with undefined key
do_handle_find_value(undefined) ->
    ?LOG_WARNING("[Gateway DHT] FIND_VALUE with undefined key"),
    #{type => error, reason => missing_key};
%% @private Handle FIND_VALUE with valid key
do_handle_find_value(Key) when is_binary(Key) ->
    %% Use routing server's find_value API with k=20
    case macula_routing_server:find_value(macula_routing_server, Key, 20) of
        {ok, Values} when is_list(Values), Values =/= [] ->
            ?LOG_INFO("[Gateway DHT] FIND_VALUE found ~p value(s)", [length(Values)]),
            macula_routing_protocol:encode_find_value_reply({value, Values});
        {ok, []} ->
            ?LOG_DEBUG("[Gateway DHT] FIND_VALUE: no values found"),
            macula_routing_protocol:encode_find_value_reply({nodes, []});
        {error, not_found} ->
            ?LOG_DEBUG("[Gateway DHT] FIND_VALUE: not found"),
            macula_routing_protocol:encode_find_value_reply({nodes, []});
        {error, Reason} ->
            ?LOG_WARNING("[Gateway DHT] FIND_VALUE error: ~p", [Reason]),
            #{type => error, reason => Reason}
    end;
do_handle_find_value(Key) ->
    ?LOG_WARNING("[Gateway DHT] FIND_VALUE with non-binary key: ~p", [Key]),
    #{type => error, reason => invalid_key}.

%% @doc Handle DHT FIND_NODE message.
%% Forwards to routing server and sends encoded reply over stream.
%% Crashes on routing server or encoding failures - exposes DHT/protocol bugs.
-spec handle_find_node(pid(), map()) -> ok.
handle_find_node(Stream, FindNodeMsg) ->
    %% Forward to routing server (let it crash on errors)
    Reply = macula_routing_server:handle_message(macula_routing_server, FindNodeMsg),
    %% Send reply back over stream (let it crash on errors)
    ReplyBinary = macula_protocol_encoder:encode(find_node_reply, Reply),
    macula_quic:send(Stream, ReplyBinary),
    ok.

%% @doc Handle DHT query from process message.
%% Decodes query, forwards to routing server, encodes reply, sends to requesting process.
%% Crashes on decode or routing failures - exposes protocol/DHT bugs.
-spec handle_query(pid(), atom(), binary()) -> ok.
handle_query(FromPid, _QueryType, QueryData) ->
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
%% Subscriptions are replicated via DHT propagation to k closest nodes,
%% so local lookup returns subscribers from the replicated DHT data.
%% Returns list of subscribers for the given key.
-spec lookup_value(binary()) -> {ok, list()} | {error, not_found}.
lookup_value(Key) ->
    ?LOG_DEBUG("lookup_value called with key hash: ~p", [Key]),
    do_lookup_value(whereis(macula_routing_server), Key).

do_lookup_value(undefined, _Key) ->
    ?LOG_ERROR("routing_server not found!"),
    {error, not_found};
do_lookup_value(RoutingServerPid, Key) ->
    %% K=20 is the standard Kademlia replication factor
    ?LOG_DEBUG("Calling routing_server:find_value..."),
    Result = macula_routing_server:find_value(RoutingServerPid, Key, 20),
    ?LOG_DEBUG("find_value result: ~p", [Result]),
    normalize_find_value_result(Result).

normalize_find_value_result({ok, []}) ->
    {error, not_found};
normalize_find_value_result({ok, Value}) when is_list(Value) ->
    {ok, Value};
normalize_find_value_result({ok, Value}) ->
    %% Single value, wrap in list
    {ok, [Value]};
normalize_find_value_result({error, Reason}) ->
    {error, Reason}.

%% @doc Forward a PUBLISH message to the bootstrap gateway for distribution.
%% @deprecated v0.14.0+ uses direct P2P routing via local DHT lookup.
%% Bootstrap is NOT a broker - use route_via_local_dht in pubsub_router instead.
%% This function remains for backwards compatibility but should not be used.
-spec forward_publish_to_bootstrap(map()) -> ok | {error, term()}.
forward_publish_to_bootstrap(PubMsg) ->
    Realm = application:get_env(macula, realm, <<"default">>),
    do_forward_to_bootstrap(gproc:lookup_local_name({connection, Realm}), PubMsg).

do_forward_to_bootstrap(undefined, _PubMsg) ->
    {error, no_connection};
do_forward_to_bootstrap(ConnPid, PubMsg) ->
    macula_connection:send_message(ConnPid, publish, PubMsg).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Send DHT message to remote peer (fire-and-forget).
%% Used for STORE operations that don't need a response.
-spec send_to_peer(map(), atom(), map()) -> ok | {error, term()}.
send_to_peer(NodeInfo, MessageType, Message) ->
    Endpoint = extract_endpoint(NodeInfo),
    ?LOG_INFO("[DHT] send_to_peer: NodeInfo=~p, Endpoint=~p, Type=~p",
              [maps:get(node_id, NodeInfo, unknown), Endpoint, MessageType]),
    do_send_to_peer(Endpoint, MessageType, Message).

do_send_to_peer(undefined, MessageType, _Message) ->
    ?LOG_WARNING("[DHT] send_to_peer: no endpoint for message type ~p", [MessageType]),
    {error, no_endpoint};
do_send_to_peer(Endpoint, MessageType, Message) ->
    %% Send directly via peer connector (establishes QUIC connection)
    Result = macula_peer_connector:send_message(Endpoint, MessageType, Message),
    ?LOG_INFO("[DHT] send_to_peer result to ~s: ~p", [Endpoint, Result]),
    Result.

%% @private
%% @doc Extract endpoint from node info, constructing from address if needed.
extract_endpoint(NodeInfo) ->
    extract_endpoint_from_field(maps:get(endpoint, NodeInfo, undefined), NodeInfo).

extract_endpoint_from_field(undefined, NodeInfo) ->
    construct_endpoint_from_address(maps:get(address, NodeInfo, undefined));
extract_endpoint_from_field(Endpoint, _NodeInfo) ->
    Endpoint.

construct_endpoint_from_address(undefined) ->
    undefined;
construct_endpoint_from_address({Host, Port}) when is_integer(Port) ->
    HostBin = format_host_to_binary(Host),
    PortBin = integer_to_binary(Port),
    <<HostBin/binary, ":", PortBin/binary>>;
construct_endpoint_from_address(HostPortStr) when is_binary(HostPortStr) ->
    HostPortStr;
construct_endpoint_from_address(HostPortStr) when is_list(HostPortStr) ->
    list_to_binary(HostPortStr);
construct_endpoint_from_address(_Other) ->
    undefined.

format_host_to_binary({_, _, _, _} = IPv4) ->
    list_to_binary(inet:ntoa(IPv4));
format_host_to_binary({_, _, _, _, _, _, _, _} = IPv6) ->
    list_to_binary(inet:ntoa(IPv6));
format_host_to_binary(Host) when is_list(Host) ->
    list_to_binary(Host);
format_host_to_binary(Host) when is_binary(Host) ->
    Host.

%% @doc Query remote peer and wait for response.
%% Used for FIND_NODE and FIND_VALUE operations.
%% Currently uses fire-and-forget delivery. For request/response patterns,
%% use macula_rpc_handler:request/4 which provides NATS-style async RPC
%% with callbacks (available since v0.12.1).
-spec query_peer(map(), atom(), map()) -> {ok, term()} | {error, term()}.
query_peer(NodeInfo, MessageType, Message) ->
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
