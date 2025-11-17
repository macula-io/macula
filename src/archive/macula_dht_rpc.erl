%%%-------------------------------------------------------------------
%%% @doc
%%% DHT RPC client for querying remote nodes.
%%% Provides callback functions for macula_routing_dht algorithms.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dht_rpc).

%% API
-export([
    query_find_node/2,
    query_find_value/2,
    query_store/3,
    make_query_fn/0,
    make_store_fn/0
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Query remote node for closest nodes to target.
%% Returns {ok, [NodeInfo]} or {error, Reason}.
-spec query_find_node(macula_routing_bucket:node_info(), binary()) ->
    {ok, [macula_routing_bucket:node_info()]} | {error, term()}.
query_find_node(NodeInfo, Target) ->
    %% Build FIND_NODE message
    Message = macula_routing_protocol:encode_find_node(Target),

    %% Send query to remote node
    case send_dht_query(NodeInfo, find_node, Message) of
        {ok, Reply} ->
            %% Decode reply
            case macula_routing_protocol:decode_find_node_reply(Reply) of
                {ok, Nodes} ->
                    {ok, Nodes};
                {error, Reason} ->
                    {error, {decode_error, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Query remote node for value by key.
%% Returns {value, Value} if found, {nodes, [NodeInfo]} if not found, or {error, Reason}.
-spec query_find_value(macula_routing_bucket:node_info(), binary()) ->
    {value, term()} | {nodes, [macula_routing_bucket:node_info()]} | {error, term()}.
query_find_value(NodeInfo, Key) ->
    %% Build FIND_VALUE message
    Message = macula_routing_protocol:encode_find_value(Key),

    %% Send query to remote node
    case send_dht_query(NodeInfo, find_value, Message) of
        {ok, Reply} ->
            %% Decode reply
            case macula_routing_protocol:decode_find_value_reply(Reply) of
                {ok, {value, Value}} ->
                    {value, Value};
                {ok, {nodes, Nodes}} ->
                    {nodes, Nodes};
                {error, Reason} ->
                    {error, {decode_error, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Store key-value pair on remote node.
%% Returns ok or {error, Reason}.
-spec query_store(macula_routing_bucket:node_info(), binary(), term()) ->
    ok | {error, term()}.
query_store(NodeInfo, Key, Value) ->
    %% Build STORE message
    Message = macula_routing_protocol:encode_store(Key, Value),

    %% Send query to remote node
    case send_dht_query(NodeInfo, store, Message) of
        {ok, _Reply} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Create a query_fn callback for DHT algorithms.
-spec make_query_fn() -> fun((macula_routing_bucket:node_info(), binary()) ->
    {ok, [macula_routing_bucket:node_info()]} |
    {value, term()} |
    {nodes, [macula_routing_bucket:node_info()]} |
    {error, term()}).
make_query_fn() ->
    fun(NodeInfo, Target) ->
        %% Try FIND_VALUE first (for value lookups), fall back to FIND_NODE
        case query_find_value(NodeInfo, Target) of
            {value, _} = Result ->
                Result;
            {nodes, Nodes} ->
                {ok, Nodes};
            {error, _Reason} ->
                %% Fallback to FIND_NODE
                query_find_node(NodeInfo, Target)
        end
    end.

%% @doc Create a store_fn callback for DHT algorithms.
-spec make_store_fn() -> fun((macula_routing_bucket:node_info(), binary(), term()) ->
    ok | {error, term()}).
make_store_fn() ->
    fun(NodeInfo, Key, Value) ->
        query_store(NodeInfo, Key, Value)
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Send DHT query to remote node and wait for reply.
-spec send_dht_query(macula_routing_bucket:node_info(), atom(), map()) ->
    {ok, map()} | {error, term()}.
send_dht_query(NodeInfo, MessageType, Message) ->
    %% Get endpoint from node info
    Endpoint = maps:get(endpoint, NodeInfo, undefined),

    case Endpoint of
        undefined ->
            {error, no_endpoint};
        _ when Endpoint == <<"unknown">> ->
            {error, no_endpoint};
        _ ->
            %% Connect to remote node and send query
            send_query_via_connection(Endpoint, MessageType, Message)
    end.

%% @doc Send query via macula_client connection.
-spec send_query_via_connection(binary(), atom(), map()) ->
    {ok, map()} | {error, term()}.
send_query_via_connection(_Endpoint, _MessageType, _Message) ->
    %% For now, we'll use the simplest approach:
    %% Try to send via an existing connection, or return error if no connection exists.
    %%
    %% Future improvement: maintain a pool of connections or create connections on-demand.

    %% TODO: Implement actual connection lookup and message sending
    %% For now, return error to indicate DHT queries via RPC are not fully connected yet
    {error, not_implemented}.

    %% Future implementation:
    %% 1. Look up or create connection to Endpoint
    %% 2. Send EncodedMsg via connection
    %% 3. Wait for reply with timeout
    %% 4. Decode and return reply
