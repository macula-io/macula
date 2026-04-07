%%%-------------------------------------------------------------------
%%% @doc
%%% Network transport and escalation operations for DHT routing.
%%% Extracted from macula_routing_server to reduce module size.
%%% Handles peer-to-peer store propagation, network queries,
%%% bridge escalation, and value replication.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_network).

-include_lib("kernel/include/logger.hrl").

%% Store propagation
-export([
    propagate_store_to_peers/4,
    send_store_to_peer/2,
    replicate_to_new_peer/4
]).

%% Network queries
-export([
    network_query_find_value/2,
    find_value_via_dht/3,
    find_value_with_escalation/5
]).

%% Bridge/escalation
-export([
    check_bridge_cache/1,
    maybe_escalate_query/2,
    escalate_to_bridge/2
]).

%%%===================================================================
%%% Store Propagation
%%%===================================================================

%% @doc Propagate STORE message to peers (called in spawned process).
-spec propagate_store_to_peers(list(), map(), binary(), term()) -> ok.
propagate_store_to_peers([], _StoreMsg, Key, Value) ->
    ?LOG_DEBUG("[DHT] No peers in routing table, forwarding store to bootstrap"),
    forward_store_to_bootstrap(Key, Value);
propagate_store_to_peers(ClosestNodes, StoreMsg, _Key, _Value) ->
    ?LOG_DEBUG("[DHT] Propagating store to ~p peer(s)", [length(ClosestNodes)]),
    lists:foreach(fun(NodeInfo) ->
        send_store_to_peer(NodeInfo, StoreMsg)
    end, ClosestNodes),
    ok.

%% @doc Send store message to a peer (best effort).
-spec send_store_to_peer(map(), map()) -> ok.
send_store_to_peer(NodeInfo, StoreMsg) ->
    ?LOG_DEBUG("[DHT] Sending store to peer ~p", [NodeInfo]),
    Transport = persistent_term:get(macula_dht_transport, macula_gateway_dht),
    try Transport:send_to_peer(NodeInfo, store, StoreMsg) of
        Result -> handle_peer_send_result(Result, NodeInfo)
    catch
        error:function_clause:Stacktrace ->
            ?LOG_WARNING("[DHT] Store send function_clause to ~p:~n  ~p", [NodeInfo, Stacktrace]);
        Class:Error:Stacktrace ->
            ?LOG_WARNING("[DHT] Store send failed to ~p: ~p:~p~n  ~p",
                         [NodeInfo, Class, Error, Stacktrace])
    end.

%% @doc Replicate stored values to a newly joined peer.
-spec replicate_to_new_peer(map(), map(), macula_routing_table:routing_table(), map()) -> ok.
replicate_to_new_peer(NewNodeInfo, Storage, Table, Config) ->
    K = maps:get(k, Config, 20),
    NewNodeId = maps:get(node_id, NewNodeInfo, undefined),
    Replicated = maps:fold(fun(Key, Providers, Acc) ->
        ClosestNodes = macula_routing_table:find_closest(Table, Key, K),
        ClosestIds = [maps:get(node_id, N, undefined) || N <- ClosestNodes],
        case lists:member(NewNodeId, ClosestIds) of
            true ->
                ProviderList = macula_routing_storage:ensure_provider_list(Providers),
                lists:foreach(fun(Provider) ->
                    StoreMsg = macula_routing_protocol:encode_store(Key, Provider),
                    send_store_to_peer(NewNodeInfo, StoreMsg)
                end, ProviderList),
                Acc + length(ProviderList);
            false ->
                Acc
        end
    end, 0, Storage),
    case Replicated > 0 of
        true ->
            ?LOG_DEBUG("[RoutingServer] Replicated ~p stored value(s) to new peer ~s",
                      [Replicated, macula_routing_storage:format_node_id(NewNodeId)]);
        false ->
            ok
    end,
    ok.

%%%===================================================================
%%% Network Queries
%%%===================================================================

%% @doc Network query function for FIND_VALUE.
-spec network_query_find_value(map(), binary()) -> {value, term()} | {nodes, list()}.
network_query_find_value(NodeInfo, Key) ->
    Endpoint = macula_routing_storage:get_node_endpoint(NodeInfo),
    do_network_query_find_value(Endpoint, NodeInfo, Key).

%% @doc Query the DHT network for a value.
-spec find_value_via_dht(binary(), pos_integer(), macula_routing_table:routing_table()) ->
    {ok, term()} | {error, term()}.
find_value_via_dht(Key, K, Table) ->
    InitialClosest = macula_routing_table:find_closest(Table, Key, K),
    ?LOG_DEBUG("[DHT] find_value_via_dht: routing_table has ~p nodes for key lookup", [length(InitialClosest)]),
    lists:foreach(fun(NodeInfo) ->
        ?LOG_DEBUG("[DHT] find_value_via_dht: will query node ~p at ~p",
                  [maps:get(node_id, NodeInfo, unknown), macula_routing_storage:get_node_endpoint(NodeInfo)])
    end, InitialClosest),

    QueryFn = fun(NodeInfo, QueryKey) -> network_query_find_value(NodeInfo, QueryKey) end,
    case macula_routing_dht:find_value(Table, Key, K, QueryFn) of
        {ok, Value} ->
            ?LOG_DEBUG("[DHT] find_value_via_dht: found ~p subscriber(s)", [length(Value)]),
            {ok, Value};
        {nodes, Nodes} ->
            ?LOG_DEBUG("[DHT] find_value_via_dht: not found, got ~p nodes", [length(Nodes)]),
            {ok, []};
        {error, Reason} ->
            ?LOG_WARNING("[DHT] find_value_via_dht: error ~p", [Reason]),
            {error, Reason}
    end.

%% @doc Find value with escalation support.
-spec find_value_with_escalation(binary(), pos_integer(), map(),
                                  macula_routing_table:routing_table(), map()) ->
    {ok, term()} | {error, term()}.
find_value_with_escalation(Key, K, Storage, Table, Config) ->
    case check_bridge_cache(Key) of
        {ok, CachedValue} ->
            ?LOG_DEBUG("find_value: cache hit for key ~p", [Key]),
            {ok, CachedValue};
        not_found ->
            LocalValues = case maps:get(Key, Storage, undefined) of
                undefined -> [];
                Value when is_list(Value) -> Value;
                Value -> [Value]
            end,
            find_value_with_local(LocalValues, Key, K, Table, Config)
    end.

%%%===================================================================
%%% Bridge / Escalation
%%%===================================================================

%% @doc Check bridge cache for value.
-spec check_bridge_cache(binary()) -> {ok, term()} | not_found.
check_bridge_cache(Key) ->
    case whereis(macula_bridge_cache) of
        undefined -> not_found;
        CachePid ->
            case macula_bridge_cache:get(CachePid, Key) of
                {ok, Value} -> {ok, Value};
                _ -> not_found
            end
    end.

%% @doc Escalate query to parent bridge if enabled.
-spec maybe_escalate_query(binary(), map()) -> {ok, term()} | {error, term()}.
maybe_escalate_query(Key, Config) ->
    case maps:get(escalation_enabled, Config, true) of
        false -> {ok, []};
        true -> escalate_to_bridge(Key, Config)
    end.

%% @doc Escalate query to parent bridge.
-spec escalate_to_bridge(binary(), map()) -> {ok, term()} | {error, term()}.
escalate_to_bridge(Key, Config) ->
    case whereis(macula_bridge_node) of
        undefined ->
            ?LOG_DEBUG("find_value: no bridge node available for escalation"),
            {ok, []};
        BridgePid ->
            Timeout = maps:get(escalation_timeout, Config, 5000),
            Query = #{type => find_value, key => Key},
            ?LOG_DEBUG("find_value: escalating query for key ~p to bridge", [Key]),
            case macula_bridge_node:escalate_query(BridgePid, Query, Timeout) of
                {ok, Value} ->
                    ?LOG_DEBUG("find_value: escalation successful, got value"),
                    {ok, Value};
                {error, not_connected} ->
                    ?LOG_DEBUG("find_value: bridge not connected to parent"),
                    {ok, []};
                {error, Reason} ->
                    ?LOG_WARNING("find_value: escalation failed: ~p", [Reason]),
                    {ok, []}
            end
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Handle peer send result
handle_peer_send_result({error, Reason}, NodeInfo) ->
    ?LOG_DEBUG("[DHT] Store send error to ~p: ~p", [NodeInfo, Reason]);
handle_peer_send_result(Result, _NodeInfo) ->
    ?LOG_DEBUG("[DHT] Store send result: ~p", [Result]).

%% @private Forward DHT STORE to bootstrap gateway via RPC.
forward_store_to_bootstrap(Key, Value) ->
    RpcHandler = whereis(macula_rpc_handler),
    do_forward_store(RpcHandler, Key, Value).

do_forward_store(undefined, _Key, _Value) ->
    ok;
do_forward_store(_RpcHandler, Key, Value) ->
    Procedure = <<"_dht.store">>,
    Args = #{<<"key">> => Key, <<"value">> => Value},
    spawn(fun() -> forward_store_via_local_client(Procedure, Args) end),
    ok.

forward_store_via_local_client(Procedure, Args) ->
    LocalClient = whereis(macula_local_client),
    do_forward_via_client(LocalClient, Procedure, Args).

do_forward_via_client(undefined, _Procedure, _Args) ->
    ok;
do_forward_via_client(LocalClient, Procedure, Args) ->
    handle_forward_result(catch macula:call(LocalClient, Procedure, Args, #{timeout => 5000})).

handle_forward_result({'EXIT', _}) -> ok;
handle_forward_result(_Result) -> ok.

%% @private
do_network_query_find_value(undefined, NodeInfo, _Key) ->
    ?LOG_WARNING("[DHT] network_query_find_value: no endpoint for node ~p", [NodeInfo]),
    {nodes, []};
do_network_query_find_value(Endpoint, NodeInfo, Key) ->
    FindValueMsg = #{<<"key">> => Key},
    ?LOG_DEBUG("[DHT] network_query_find_value: querying ~s for key", [Endpoint]),
    Transport = persistent_term:get(macula_dht_transport, macula_gateway_dht),
    case Transport:send_and_wait(NodeInfo, find_value, FindValueMsg, 5000) of
        {ok, {find_value_reply, Response}} ->
            Result = decode_find_value_response(Response),
            ?LOG_DEBUG("[DHT] network_query_find_value: got reply from ~s: ~p", [Endpoint, Result]),
            Result;
        {ok, {OtherType, _Response}} ->
            ?LOG_WARNING("[DHT] network_query_find_value: unexpected reply type ~p from ~s", [OtherType, Endpoint]),
            {nodes, []};
        {error, timeout} ->
            ?LOG_WARNING("[DHT] network_query_find_value: timeout querying ~s", [Endpoint]),
            {nodes, []};
        {error, Reason} ->
            ?LOG_WARNING("[DHT] network_query_find_value: error ~p querying ~s", [Reason, Endpoint]),
            {nodes, []}
    end.

decode_find_value_response(Response) ->
    case macula_routing_protocol:decode_find_value_reply(Response) of
        {ok, {value, Value}} -> {value, Value};
        {ok, {nodes, Nodes}} -> {nodes, Nodes};
        {error, _Reason} -> {nodes, []}
    end.

%% @private Return local values immediately if found, else query network
find_value_with_local(LocalValues, _Key, _K, _Table, _Config) when LocalValues =/= [] ->
    ?LOG_DEBUG("[DHT] find_value: returning ~p local value(s)", [length(LocalValues)]),
    {ok, LocalValues};
find_value_with_local([], Key, K, Table, Config) ->
    ?LOG_DEBUG("[DHT] find_value: no local values, querying network"),
    NetworkValues = case find_value_via_dht(Key, K, Table) of
        {ok, RemoteList} when is_list(RemoteList) -> RemoteList;
        {ok, RemoteSingle} -> [RemoteSingle];
        _ -> []
    end,
    ?LOG_DEBUG("[DHT] find_value: network returned ~p value(s)", [length(NetworkValues)]),
    case NetworkValues of
        [] -> maybe_escalate_query(Key, Config);
        _ -> {ok, NetworkValues}
    end.
