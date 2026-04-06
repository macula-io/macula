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
    do_handle_find_value/1,
    forward_publish_to_bootstrap/1,
    send_to_peer/3,
    send_and_wait/4,
    query_peer/3
]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Handle DHT STORE message.
%% Forwards to routing server asynchronously (fire-and-forget, no reply needed).
%% Uses async handler to prevent blocking the gateway on DHT operations.
-spec handle_store(reference(), map()) -> ok.
handle_store(_Stream, StoreMsg) ->
    %% Forward to routing server asynchronously (fire-and-forget)
    %% STORE doesn't require a response, so we don't wait
    ?LOG_DEBUG("Processing STORE message, forwarding to routing_server (async)"),
    macula_routing_server:handle_message_async(macula_routing_server, StoreMsg),
    ok.

%% @doc Handle DHT FIND_VALUE message.
%% Extracts the key and performs local storage lookup, returning result over stream.
%% The message format from protocol decoder contains a binary key field.
-spec handle_find_value(reference(), map()) -> ok.
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
%% @private Handle FIND_VALUE with valid key.
%% Uses fast ETS local-only lookup (no gen_server, no network query).
do_handle_find_value(Key) when is_binary(Key) ->
    case macula_routing_server:find_value_local(Key, 20) of
        {error, not_found} -> macula_routing_protocol:encode_find_value_reply({nodes, []});
        Result -> do_handle_find_value_result(Result)
    end;
do_handle_find_value(Key) ->
    ?LOG_WARNING("[Gateway DHT] FIND_VALUE with non-binary key: ~p", [Key]),
    #{type => error, reason => invalid_key}.

do_handle_find_value_result({ok, Values}) when is_list(Values), Values =/= [] ->
    ?LOG_INFO("[Gateway DHT] FIND_VALUE found ~p value(s)", [length(Values)]),
    macula_routing_protocol:encode_find_value_reply({value, Values});
do_handle_find_value_result({ok, []}) ->
    macula_routing_protocol:encode_find_value_reply({nodes, []});
do_handle_find_value_result({error, _}) ->
    macula_routing_protocol:encode_find_value_reply({nodes, []}).

%% @doc Handle DHT FIND_NODE message.
%% The message is already decoded by the gateway — it's the payload without the type field.
%% We extract the target and query the routing table directly instead of going through
%% handle_message (which re-classifies and fails because the type field is stripped).
-spec handle_find_node(reference(), map()) -> ok.
handle_find_node(Stream, FindNodeMsg) ->
    Target = maps:get(<<"target">>, FindNodeMsg, maps:get(target, FindNodeMsg, undefined)),
    Closest = find_closest_nodes(Target),
    Reply = macula_routing_protocol:encode_find_node_reply(Closest),
    ReplyBinary = macula_protocol_encoder:encode(find_node_reply, Reply),
    macula_quic:send(Stream, ReplyBinary),
    ok.

find_closest_nodes(undefined) ->
    [];
find_closest_nodes(Target) ->
    macula_routing_server:find_closest(macula_routing_server, Target, 20).

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

%% @doc Look up ALL providers for a key — merges local + network results.
%% Used by pubsub discovery where we need ALL subscribers, not just the first.
-spec lookup_value(binary()) -> {ok, list()} | {error, not_found}.
lookup_value(Key) ->
    %% Get local results (fast, no gen_server)
    LocalValues = case macula_routing_server:find_value_local(Key, 20) of
        {ok, L} when is_list(L) -> L;
        _ -> []
    end,
    %% Also query peers for their stored values
    RemoteValues = case whereis(macula_routing_server) of
        undefined -> [];
        Pid ->
            case safe_find_value(Pid, Key) of
                {ok, R} when is_list(R) -> R;
                _ -> []
            end
    end,
    %% Merge and deduplicate by node_id
    Merged = merge_providers(LocalValues ++ RemoteValues),
    case Merged of
        [] -> {error, not_found};
        _ -> {ok, Merged}
    end.

%% @private Deduplicate providers by node_id
merge_providers(Providers) ->
    lists:foldl(fun(P, Acc) ->
        NodeId = get_provider_node_id(P),
        case lists:any(fun(A) -> get_provider_node_id(A) =:= NodeId end, Acc) of
            true -> Acc;
            false -> [P | Acc]
        end
    end, [], Providers).

get_provider_node_id(#{node_id := N}) -> N;
get_provider_node_id(#{<<"node_id">> := N}) -> N;
get_provider_node_id(_) -> make_ref().

%% @private Safe find_value via gen_server — catches timeouts
safe_find_value(Pid, Key) ->
    try macula_routing_server:find_value(Pid, Key, 20)
    catch
        exit:{timeout, _} -> {error, timeout}
    end.

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

%% @private Safely call routing_server:find_value, catching gen_server timeouts.
%% The routing server's find_value uses a 10s gen_server:call which can exit

%% @doc Send DHT message to remote peer (fire-and-forget).
%% Used for STORE operations that don't need a response.
%% @doc Send a DHT message to a peer and wait for response.
%% Used by macula_routing_server for iterative FIND_VALUE lookups.
-spec send_and_wait(map(), atom(), map(), timeout()) -> {ok, term()} | {error, term()}.
send_and_wait(NodeInfo, MessageType, Message, Timeout) ->
    Endpoint = extract_endpoint(NodeInfo),
    case Endpoint of
        undefined -> {error, no_endpoint};
        _ -> macula_peer_connector:send_message_and_wait(Endpoint, MessageType, Message, Timeout)
    end.

-spec send_to_peer(map(), atom(), map()) -> ok | {error, term()}.
send_to_peer(NodeInfo, MessageType, Message) ->
    Endpoint = extract_endpoint(NodeInfo),
    ?LOG_DEBUG("[DHT] send_to_peer: NodeInfo=~p, Endpoint=~p, Type=~p",
              [maps:get(node_id, NodeInfo, unknown), Endpoint, MessageType]),
    do_send_to_peer(Endpoint, MessageType, Message).

do_send_to_peer(undefined, MessageType, _Message) ->
    ?LOG_WARNING("[DHT] send_to_peer: no endpoint for message type ~p", [MessageType]),
    {error, no_endpoint};
do_send_to_peer(Endpoint, MessageType, Message) ->
    %% Send directly via peer connector (establishes QUIC connection)
    Result = macula_peer_connector:send_message(Endpoint, MessageType, Message),
    ?LOG_DEBUG("[DHT] send_to_peer result to ~s: ~p", [Endpoint, Result]),
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
