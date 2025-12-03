%%%-------------------------------------------------------------------
%%% @doc
%%% GenServer managing Kademlia DHT routing table and operations.
%%% Integrates all routing components: table, DHT algorithms, protocol.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_server).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/2,
    add_node/2,
    find_closest/3,
    store_local/3,
    store/3,
    get_local/2,
    get_all_keys/1,
    delete_local/3,
    find_value/3,
    get_routing_table/1,
    size/1,
    handle_message/2,
    handle_message_async/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% State
-record(state, {
    local_node_id :: binary(),
    routing_table :: macula_routing_table:routing_table(),
    storage :: #{binary() => term()},  % Local key-value storage
    config :: #{
        k => pos_integer(),
        alpha => pos_integer(),
        escalation_enabled => boolean(),
        escalation_timeout => pos_integer()
    }
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start routing server with registered name macula_routing_server.
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(LocalNodeId, Config) ->
    gen_server:start_link({local, macula_routing_server}, ?MODULE, {LocalNodeId, Config}, []).

%% @doc Add node to routing table (async - does not block caller).
-spec add_node(pid(), macula_routing_bucket:node_info()) -> ok.
add_node(Pid, NodeInfo) ->
    gen_server:cast(Pid, {add_node, NodeInfo}).

%% @doc Find k closest nodes to target.
-spec find_closest(pid(), binary(), pos_integer()) -> [macula_routing_bucket:node_info()].
find_closest(Pid, Target, K) ->
    gen_server:call(Pid, {find_closest, Target, K}).

%% @doc Store value locally.
-spec store_local(pid(), binary(), term()) -> ok.
store_local(Pid, Key, Value) ->
    gen_server:call(Pid, {store_local, Key, Value}).

%% @doc Store value in DHT by propagating to k closest nodes.
%% Stores locally first, then sends STORE messages to k closest peers.
-spec store(pid(), binary(), term()) -> ok.
store(Pid, Key, Value) ->
    gen_server:call(Pid, {store, Key, Value}, 10000).

%% @doc Get value from local storage.
-spec get_local(pid(), binary()) -> {ok, term()} | not_found.
get_local(Pid, Key) ->
    gen_server:call(Pid, {get_local, Key}).

%% @doc Get all keys from local storage.
-spec get_all_keys(pid()) -> {ok, [binary()]} | {error, term()}.
get_all_keys(Pid) ->
    gen_server:call(Pid, get_all_keys).

%% @doc Delete value from local storage.
-spec delete_local(pid(), binary(), binary()) -> ok.
delete_local(Pid, Key, NodeId) ->
    gen_server:call(Pid, {delete_local, Key, NodeId}).

%% @doc Find value in DHT using iterative lookup.
%% Returns {ok, Value} if found, {nodes, Nodes} if not found.
-spec find_value(pid(), binary(), pos_integer()) ->
    {ok, term()} | {nodes, [macula_routing_bucket:node_info()]} | {error, term()}.
find_value(Pid, Key, K) ->
    gen_server:call(Pid, {find_value, Key, K}, 10000).

%% @doc Get routing table snapshot.
-spec get_routing_table(pid()) -> macula_routing_table:routing_table().
get_routing_table(Pid) ->
    gen_server:call(Pid, get_routing_table).

%% @doc Get number of nodes in routing table.
-spec size(pid()) -> non_neg_integer().
size(Pid) ->
    gen_server:call(Pid, size).

%% @doc Handle incoming DHT message and return reply.
-spec handle_message(pid(), map()) -> map().
handle_message(Pid, Message) ->
    gen_server:call(Pid, {handle_message, Message}).

%% @doc Handle incoming DHT message asynchronously (fire-and-forget).
%% Use for STORE messages where no reply is needed.
-spec handle_message_async(pid(), map()) -> ok.
handle_message_async(Pid, Message) ->
    gen_server:cast(Pid, {handle_message_async, Message}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({LocalNodeId, Config}) ->
    K = maps:get(k, Config, 20),
    Alpha = maps:get(alpha, Config, 3),
    EscalationEnabled = maps:get(escalation_enabled, Config, true),
    EscalationTimeout = maps:get(escalation_timeout, Config, 5000),

    State = #state{
        local_node_id = LocalNodeId,
        routing_table = macula_routing_table:new(LocalNodeId, K),
        storage = #{},
        config = #{
            k => K,
            alpha => Alpha,
            escalation_enabled => EscalationEnabled,
            escalation_timeout => EscalationTimeout
        }
    },

    {ok, State}.

%% @private
handle_call({find_closest, Target, K}, _From, #state{routing_table = Table} = State) ->
    Closest = macula_routing_table:find_closest(Table, Target, K),
    {reply, Closest, State};

handle_call({store_local, Key, Value}, _From, #state{storage = Storage} = State) ->
    ExistingProviders = maps:get(Key, Storage, []),
    ProviderList = ensure_provider_list(ExistingProviders),
    NodeId = maps:get(node_id, Value, undefined),
    UpdatedProviders = upsert_provider(NodeId, Value, ProviderList),
    NewStorage = Storage#{Key => UpdatedProviders},
    {reply, ok, State#state{storage = NewStorage}};

handle_call({store, Key, Value}, _From, #state{routing_table = Table, config = Config} = State) ->
    %% 1. Store locally first
    NewState = case handle_call({store_local, Key, Value}, _From, State) of
        {reply, ok, S} -> S;
        _ -> State  %% Shouldn't happen but be safe
    end,

    %% 2. Find k closest nodes to Key
    K = maps:get(k, Config, 20),
    ClosestNodes = macula_routing_table:find_closest(Table, Key, K),

    %% 3. Send STORE message to each node (fire-and-forget, non-blocking)
    %% Spawn the sends to avoid blocking the routing server on network I/O
    StoreMsg = macula_routing_protocol:encode_store(Key, Value),
    spawn(fun() -> propagate_store_to_peers(ClosestNodes, StoreMsg, Key, Value) end),

    {reply, ok, NewState};

handle_call({get_local, Key}, _From, #state{storage = Storage} = State) ->
    Reply = format_storage_value(maps:get(Key, Storage, undefined)),
    {reply, Reply, State};

handle_call(get_all_keys, _From, #state{storage = Storage} = State) ->
    Keys = maps:keys(Storage),
    {reply, {ok, Keys}, State};

handle_call(get_routing_table, _From, #state{routing_table = Table} = State) ->
    {reply, Table, State};

handle_call(size, _From, #state{routing_table = Table} = State) ->
    Size = macula_routing_table:size(Table),
    {reply, Size, State};

handle_call({delete_local, Key, NodeId}, _From, #state{storage = Storage} = State) ->
    NewStorage = delete_provider_from_storage(Key, NodeId, Storage),
    {reply, ok, State#state{storage = NewStorage}};

handle_call({find_value, Key, K}, _From, #state{routing_table = Table, storage = Storage,
                                              config = Config} = State) ->
    ?LOG_DEBUG("find_value: key=~p, storage_size=~p", [Key, maps:size(Storage)]),
    ?LOG_DEBUG("find_value: storage keys=~p", [maps:keys(Storage)]),
    Reply = find_value_with_escalation(Key, K, Storage, Table, Config),
    {reply, Reply, State};

handle_call({handle_message, Message}, _From, State) ->
    {Reply, NewState} = process_dht_message(Message, State),
    {reply, Reply, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
%% Add node to routing table (async operation)
handle_cast({add_node, NodeInfo}, #state{routing_table = Table} = State) ->
    NewTable = macula_routing_table:add_node(Table, NodeInfo),
    {noreply, State#state{routing_table = NewTable}};

%% @private
%% Handle async store (from _dht.store RPC) - stores locally only without propagation
%% This is used by bootstrap gateway when receiving store requests from peers
handle_cast({store, Key, Value}, #state{storage = Storage} = State) when is_binary(Key) ->
    ?LOG_DEBUG("Async STORE received - key hash prefix: ~p, value: ~p",
              [binary:part(Key, 0, min(8, byte_size(Key))), Value]),
    ExistingProviders = maps:get(Key, Storage, []),
    ProviderList = ensure_provider_list(ExistingProviders),
    NodeId = get_node_id_from_value(Value),
    UpdatedProviders = upsert_provider(NodeId, Value, ProviderList),
    NewStorage = Storage#{Key => UpdatedProviders},
    ?LOG_DEBUG("Stored value for key, now have ~p provider(s)", [length(UpdatedProviders)]),
    {noreply, State#state{storage = NewStorage}};

handle_cast({store, Key, _Value}, State) ->
    ?LOG_ERROR("Async STORE received with non-binary key: ~p", [Key]),
    {noreply, State};

%% @private
%% Handle async DHT message processing (fire-and-forget)
%% Used by gateway to avoid blocking on STORE operations
handle_cast({handle_message_async, Message}, State) ->
    {_Reply, NewState} = process_dht_message(Message, State),
    {noreply, NewState};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Propagate STORE message to peers (called in spawned process).
%% Non-blocking: network I/O happens in separate process, won't block routing server.
-spec propagate_store_to_peers(list(), map(), binary(), term()) -> ok.
propagate_store_to_peers([], _StoreMsg, Key, Value) ->
    %% No nodes in routing table - forward to bootstrap gateway via RPC
    ?LOG_INFO("[DHT] No peers in routing table, forwarding store to bootstrap"),
    forward_store_to_bootstrap(Key, Value);
propagate_store_to_peers(ClosestNodes, StoreMsg, _Key, _Value) ->
    ?LOG_INFO("[DHT] Propagating store to ~p peer(s)", [length(ClosestNodes)]),
    lists:foreach(fun(NodeInfo) ->
        %% Best effort - don't fail if send fails
        try
            ?LOG_DEBUG("[DHT] Sending store to peer ~p", [NodeInfo]),
            Result = macula_gateway_dht:send_to_peer(NodeInfo, store, StoreMsg),
            ?LOG_DEBUG("[DHT] Store send result: ~p", [Result])
        catch
            Class:Error ->
                ?LOG_WARNING("[DHT] Store send failed: ~p:~p", [Class, Error])
        end
    end, ClosestNodes),
    ok.

%% @doc Forward DHT STORE to bootstrap gateway via RPC when routing table is empty.
%% This allows embedded gateways to propagate subscriptions to the central bootstrap DHT.
-spec forward_store_to_bootstrap(binary(), term()) -> ok.
forward_store_to_bootstrap(Key, Value) ->
    %% Call the _dht.store RPC procedure on the bootstrap gateway
    %% This is a fire-and-forget operation - we don't wait for response
    case whereis(macula_rpc_handler) of
        undefined ->
            ok;
        _RpcHandler ->
            %% Call _dht.store(Key, Value) on the bootstrap gateway
            %% Using cast (fire-and-forget) since we don't need the response
            try
                Procedure = <<"_dht.store">>,
                Args = #{
                    <<"key">> => Key,
                    <<"value">> => Value
                },
                %% Use macula module's call function which handles bootstrap routing
                spawn(fun() ->
                    case whereis(macula_local_client) of
                        undefined ->
                            ok;
                        LocalClient ->
                            _ = macula:call(LocalClient, Procedure, Args, #{timeout => 5000}),
                            ok
                    end
                end),
                ok
            catch
                _:_Error ->
                    ok
            end
    end.

%% @doc Extract node_id from value map, handling both atom and binary keys.
-spec get_node_id_from_value(map()) -> binary() | undefined.
get_node_id_from_value(#{node_id := NodeId}) -> NodeId;
get_node_id_from_value(#{<<"node_id">> := NodeId}) -> NodeId;
get_node_id_from_value(_Value) -> undefined.

%% @doc Find index of provider with matching node_id in provider list.
-spec find_provider_index(binary(), [map()]) -> pos_integer() | not_found.
find_provider_index(NodeId, ProviderList) ->
    find_provider_index(NodeId, ProviderList, 1).

find_provider_index(_NodeId, [], _Index) ->
    not_found;
find_provider_index(NodeId, [Provider | Rest], Index) ->
    ProviderNodeId = get_node_id_from_value(Provider),
    check_provider_match(NodeId, ProviderNodeId, Rest, Index).

check_provider_match(NodeId, NodeId, _Rest, Index) -> Index;
check_provider_match(NodeId, _Other, Rest, Index) ->
    find_provider_index(NodeId, Rest, Index + 1).

%% @doc Ensure value is a list for multi-provider storage.
-spec ensure_provider_list(term()) -> [term()].
ensure_provider_list(List) when is_list(List) -> List;
ensure_provider_list(Value) -> [Value].

%% @doc Insert or update a provider in the list.
-spec upsert_provider(binary() | undefined, map(), [map()]) -> [map()].
upsert_provider(undefined, Value, ProviderList) ->
    [Value | ProviderList];
upsert_provider(NodeId, Value, ProviderList) ->
    upsert_by_index(find_provider_index(NodeId, ProviderList), Value, ProviderList).

upsert_by_index(not_found, Value, ProviderList) ->
    [Value | ProviderList];
upsert_by_index(Index, Value, ProviderList) ->
    lists:sublist(ProviderList, Index - 1) ++ [Value] ++ lists:nthtail(Index, ProviderList).

%% @doc Format storage value for get_local response.
-spec format_storage_value(undefined | term()) -> not_found | {ok, [term()]}.
format_storage_value(undefined) -> not_found;
format_storage_value(Value) when is_list(Value) -> {ok, Value};
format_storage_value(Value) -> {ok, [Value]}.

%% @doc Delete a specific provider from storage by node_id.
-spec delete_provider_from_storage(binary(), binary(), map()) -> map().
delete_provider_from_storage(Key, NodeId, Storage) ->
    case maps:get(Key, Storage, undefined) of
        undefined -> Storage;
        Providers when is_list(Providers) ->
            delete_provider_by_node_id(Key, NodeId, Providers, Storage);
        _SingleValue -> maps:remove(Key, Storage)
    end.

delete_provider_by_node_id(Key, NodeId, Providers, Storage) ->
    FilterFn = fun(P) -> get_node_id_from_value(P) =/= NodeId end,
    UpdatedProviders = lists:filter(FilterFn, Providers),
    update_or_remove_key(Key, UpdatedProviders, Storage).

update_or_remove_key(Key, [], Storage) -> maps:remove(Key, Storage);
update_or_remove_key(Key, Providers, Storage) -> Storage#{Key => Providers}.

%% @doc Find value with escalation support.
%% For pub/sub scenarios, we need ALL subscribers (local + remote).
%% Queries both local storage AND DHT network, then merges results.
-spec find_value_with_escalation(binary(), pos_integer(), map(),
                                  macula_routing_table:routing_table(), map()) ->
    {ok, term()} | {error, term()}.
find_value_with_escalation(Key, K, Storage, Table, Config) ->
    %% 1. Check bridge cache first (fastest)
    case check_bridge_cache(Key) of
        {ok, CachedValue} ->
            ?LOG_DEBUG("find_value: cache hit for key ~p", [Key]),
            {ok, CachedValue};
        not_found ->
            %% 2. Get local values (if any)
            LocalValues = case maps:get(Key, Storage, undefined) of
                undefined -> [];
                Value when is_list(Value) -> Value;
                Value -> [Value]
            end,
            %% 3. ALWAYS query DHT network for remote values (critical for pub/sub)
            %% This ensures we get subscribers from ALL nodes, not just local
            NetworkValues = case find_value_via_dht(Key, K, Table) of
                {ok, RemoteList} when is_list(RemoteList) -> RemoteList;
                {ok, RemoteSingle} -> [RemoteSingle];
                _ -> []
            end,
            %% 4. Merge and deduplicate by node_id
            AllValues = merge_providers(LocalValues, NetworkValues),
            ?LOG_INFO("[DHT] find_value: local=~p, network=~p, merged=~p",
                       [length(LocalValues), length(NetworkValues), length(AllValues)]),
            case AllValues of
                [] ->
                    %% 5. Try escalation if nothing found
                    maybe_escalate_query(Key, Config);
                _ ->
                    {ok, AllValues}
            end
    end.

%% @doc Merge two lists of providers, deduplicating by node_id.
%% Prefers the entry from the second list (network) over the first (local).
-spec merge_providers(list(), list()) -> list().
merge_providers(LocalProviders, NetworkProviders) ->
    %% Build a map keyed by node_id for deduplication
    LocalMap = lists:foldl(fun(P, Acc) ->
        NodeId = get_node_id_from_value(P),
        Acc#{NodeId => P}
    end, #{}, LocalProviders),
    MergedMap = lists:foldl(fun(P, Acc) ->
        NodeId = get_node_id_from_value(P),
        Acc#{NodeId => P}
    end, LocalMap, NetworkProviders),
    maps:values(MergedMap).

find_value_via_dht(Key, K, Table) ->
    %% Log routing table state for debugging
    InitialClosest = macula_routing_table:find_closest(Table, Key, K),
    ?LOG_INFO("[DHT] find_value_via_dht: routing_table has ~p nodes for key lookup", [length(InitialClosest)]),
    lists:foreach(fun(NodeInfo) ->
        ?LOG_INFO("[DHT] find_value_via_dht: will query node ~p at ~p",
                  [maps:get(node_id, NodeInfo, unknown), get_node_endpoint(NodeInfo)])
    end, InitialClosest),

    QueryFn = fun(NodeInfo, QueryKey) -> network_query_find_value(NodeInfo, QueryKey) end,
    case macula_routing_dht:find_value(Table, Key, K, QueryFn) of
        {ok, Value} ->
            ?LOG_INFO("[DHT] find_value_via_dht: found ~p subscriber(s)", [length(Value)]),
            {ok, Value};
        {nodes, Nodes} ->
            ?LOG_INFO("[DHT] find_value_via_dht: not found, got ~p nodes", [length(Nodes)]),
            {ok, []};
        {error, Reason} ->
            ?LOG_WARNING("[DHT] find_value_via_dht: error ~p", [Reason]),
            {error, Reason}
    end.

%% @private Network query function for FIND_VALUE.
%% Sends FIND_VALUE request to a peer and waits for response.
%% Returns {value, Value} if peer has the value, {nodes, []} otherwise.
-spec network_query_find_value(map(), binary()) -> {value, term()} | {nodes, list()}.
network_query_find_value(NodeInfo, Key) ->
    Endpoint = get_node_endpoint(NodeInfo),
    do_network_query_find_value(Endpoint, NodeInfo, Key).

do_network_query_find_value(undefined, NodeInfo, _Key) ->
    ?LOG_WARNING("[DHT] network_query_find_value: no endpoint for node ~p", [NodeInfo]),
    {nodes, []};
do_network_query_find_value(Endpoint, _NodeInfo, Key) ->
    %% Build FIND_VALUE message (the protocol expects raw Key, not encoded)
    FindValueMsg = #{<<"key">> => Key},
    ?LOG_INFO("[DHT] network_query_find_value: querying ~s for key", [Endpoint]),

    %% Send request and wait for response (synchronous with 5s timeout)
    case macula_peer_connector:send_message_and_wait(Endpoint, find_value, FindValueMsg, 5000) of
        {ok, {find_value_reply, Response}} ->
            Result = decode_find_value_response(Response),
            ?LOG_INFO("[DHT] network_query_find_value: got reply from ~s: ~p", [Endpoint, Result]),
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

%% @private Decode FIND_VALUE reply content.
decode_find_value_response(Response) ->
    case macula_routing_protocol:decode_find_value_reply(Response) of
        {ok, {value, Value}} ->
            {value, Value};
        {ok, {nodes, Nodes}} ->
            {nodes, Nodes};
        {error, _Reason} ->
            {nodes, []}
    end.

%% @private Extract endpoint from node info.
get_node_endpoint(#{endpoint := Endpoint}) when is_binary(Endpoint) ->
    Endpoint;
get_node_endpoint(#{address := {Host, Port}}) when is_integer(Port) ->
    iolist_to_binary([format_host(Host), <<":">>, integer_to_binary(Port)]);
get_node_endpoint(#{<<"endpoint">> := Endpoint}) when is_binary(Endpoint) ->
    Endpoint;
get_node_endpoint(_) ->
    undefined.

%% @private Format host for URL.
format_host({A, B, C, D}) when is_integer(A) ->
    io_lib:format("~B.~B.~B.~B", [A, B, C, D]);
format_host(Host) when is_list(Host) ->
    Host;
format_host(Host) when is_binary(Host) ->
    binary_to_list(Host).

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
    EscalationEnabled = maps:get(escalation_enabled, Config, true),
    case EscalationEnabled of
        false ->
            {ok, []};
        true ->
            escalate_to_bridge(Key, Config)
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

%% @doc Process incoming DHT message and generate reply.
-spec process_dht_message(map(), #state{}) -> {map(), #state{}}.
process_dht_message(Message, State) ->
    MessageType = classify_dht_message(Message),
    dispatch_dht_message(MessageType, Message, State).

classify_dht_message(Message) ->
    case macula_routing_protocol:is_find_node(Message) of
        true -> find_node;
        false -> classify_non_find_node(Message)
    end.

classify_non_find_node(Message) ->
    case macula_routing_protocol:is_store(Message) of
        true -> store;
        false -> classify_find_value(Message)
    end.

classify_find_value(Message) ->
    case macula_routing_protocol:is_find_value(Message) of
        true -> find_value;
        false -> unknown
    end.

dispatch_dht_message(find_node, Message, State) ->
    handle_find_node(Message, State);
dispatch_dht_message(store, Message, State) ->
    handle_store(Message, State);
dispatch_dht_message(find_value, Message, State) ->
    handle_find_value(Message, State);
dispatch_dht_message(unknown, _Message, State) ->
    {#{type => error, reason => unknown_message}, State}.

%% @doc Handle FIND_NODE request.
-spec handle_find_node(map(), #state{}) -> {map(), #state{}}.
handle_find_node(Message, #state{routing_table = Table, config = Config} = State) ->
    {ok, Target} = macula_routing_protocol:decode_find_node(Message),
    K = maps:get(k, Config, 20),

    %% Find k closest nodes
    Closest = macula_routing_table:find_closest(Table, Target, K),

    %% Encode reply
    Reply = macula_routing_protocol:encode_find_node_reply(Closest),

    {Reply, State}.

%% @doc Handle STORE request.
-spec handle_store(map(), #state{}) -> {map(), #state{}}.
handle_store(Message, #state{storage = Storage} = State) ->
    {ok, Key, Value} = macula_routing_protocol:decode_store(Message),
    ?LOG_INFO("[RoutingServer] STORE received: key_prefix=~p, value_node_id=~p",
             [binary:part(Key, 0, min(8, byte_size(Key))), maps:get(node_id, Value, maps:get(<<"node_id">>, Value, unknown))]),
    ?LOG_DEBUG("STORE: key=~p, value=~p", [Key, Value]),
    ExistingProviders = maps:get(Key, Storage, []),
    ProviderList = ensure_provider_list(ExistingProviders),
    NodeId = get_node_id_from_value(Value),
    UpdatedProviders = upsert_provider(NodeId, Value, ProviderList),
    NewStorage = Storage#{Key => UpdatedProviders},
    ?LOG_DEBUG("STORE complete: new_storage_size=~p, key_prefix=~p",
              [maps:size(NewStorage), binary:part(Key, 0, min(8, byte_size(Key)))]),
    Reply = #{type => store_reply, result => ok},
    {Reply, State#state{storage = NewStorage}}.

%% @doc Handle FIND_VALUE request.
-spec handle_find_value(map(), #state{}) -> {map(), #state{}}.
handle_find_value(Message, #state{storage = Storage, routing_table = Table, config = Config} = State) ->
    {ok, Key} = macula_routing_protocol:decode_find_value(Message),
    StorageValue = maps:get(Key, Storage, undefined),
    K = maps:get(k, Config, 20),
    ?LOG_INFO("[RoutingServer] FIND_VALUE: key_prefix=~p, storage_size=~p, found=~p",
             [binary:part(Key, 0, min(8, byte_size(Key))), maps:size(Storage), StorageValue =/= undefined]),
    Reply = encode_find_value_result(StorageValue, Key, K, Table),
    {Reply, State}.

encode_find_value_result(undefined, Key, K, Table) ->
    Closest = macula_routing_table:find_closest(Table, Key, K),
    macula_routing_protocol:encode_find_value_reply({nodes, Closest});
encode_find_value_result(Value, _Key, _K, _Table) when is_list(Value) ->
    macula_routing_protocol:encode_find_value_reply({value, Value});
encode_find_value_result(Value, _Key, _K, _Table) ->
    macula_routing_protocol:encode_find_value_reply({value, Value}).
