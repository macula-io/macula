%%%-------------------------------------------------------------------
%%% @doc
%%% GenServer managing Kademlia DHT routing table and operations.
%%% Integrates all routing components: table, DHT algorithms, protocol.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_server).
-behaviour(gen_server).

%% API
-export([
    start_link/2,
    add_node/2,
    find_closest/3,
    store_local/3,
    store/3,
    get_local/2,
    get_all_keys/1,
    find_value/3,
    get_routing_table/1,
    size/1,
    handle_message/2
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
        alpha => pos_integer()
    }
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start routing server with registered name macula_routing_server.
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(LocalNodeId, Config) ->
    gen_server:start_link({local, macula_routing_server}, ?MODULE, {LocalNodeId, Config}, []).

%% @doc Add node to routing table.
-spec add_node(pid(), macula_routing_bucket:node_info()) -> ok.
add_node(Pid, NodeInfo) ->
    gen_server:call(Pid, {add_node, NodeInfo}).

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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({LocalNodeId, Config}) ->
    K = maps:get(k, Config, 20),
    Alpha = maps:get(alpha, Config, 3),

    State = #state{
        local_node_id = LocalNodeId,
        routing_table = macula_routing_table:new(LocalNodeId, K),
        storage = #{},
        config = #{
            k => K,
            alpha => Alpha
        }
    },

    {ok, State}.

%% @private
handle_call({add_node, NodeInfo}, _From, #state{routing_table = Table} = State) ->
    NewTable = macula_routing_table:add_node(Table, NodeInfo),
    {reply, ok, State#state{routing_table = NewTable}};

handle_call({find_closest, Target, K}, _From, #state{routing_table = Table} = State) ->
    Closest = macula_routing_table:find_closest(Table, Target, K),
    {reply, Closest, State};

handle_call({store_local, Key, Value}, _From, #state{storage = Storage} = State) ->
    %% Store providers as a list to support multiple providers per service
    ExistingProviders = maps:get(Key, Storage, []),

    %% Ensure we're working with a list (for backward compatibility)
    ProviderList = case is_list(ExistingProviders) of
        true -> ExistingProviders;
        false -> [ExistingProviders]  %% Legacy: convert single value to list
    end,

    %% Get node_id from the new provider
    NodeId = maps:get(node_id, Value, undefined),

    %% Update or append provider
    UpdatedProviders = case NodeId of
        undefined ->
            %% No node_id, just append (shouldn't happen in practice)
            [Value | ProviderList];
        _ ->
            %% Check if this provider already exists by comparing node_ids
            ExistingIndex = find_provider_index(NodeId, ProviderList),
            case ExistingIndex of
                not_found ->
                    %% Append new provider
                    io:format("[DHT] store_local: Adding new subscriber (was ~p, now ~p)~n",
                             [length(ProviderList), length(ProviderList) + 1]),
                    [Value | ProviderList];
                Index ->
                    %% Update existing provider (replace at index)
                    io:format("[DHT] store_local: Updating existing subscriber at index ~p~n", [Index]),
                    lists:sublist(ProviderList, Index - 1) ++
                        [Value] ++
                        lists:nthtail(Index, ProviderList)
            end
    end,

    NewStorage = Storage#{Key => UpdatedProviders},
    {reply, ok, State#state{storage = NewStorage}};

handle_call({store, Key, Value}, _From, #state{routing_table = Table, config = Config} = State) ->
    io:format("[DHT] store: Propagating key ~p to k closest nodes~n", [Key]),

    %% 1. Store locally first
    NewState = case handle_call({store_local, Key, Value}, _From, State) of
        {reply, ok, S} -> S;
        _ -> State  %% Shouldn't happen but be safe
    end,

    %% 2. Find k closest nodes to Key
    K = maps:get(k, Config, 20),
    ClosestNodes = macula_routing_table:find_closest(Table, Key, K),
    io:format("[DHT] store: Found ~p closest nodes for key~n", [length(ClosestNodes)]),

    %% 3. Send STORE message to each node (fire-and-forget)
    StoreMsg = macula_routing_protocol:encode_store(Key, Value),
    lists:foreach(fun(NodeInfo) ->
        io:format("[DHT] store: Sending STORE to node ~p~n", [maps:get(node_id, NodeInfo, unknown)]),
        %% Best effort - don't fail if send fails
        case macula_gateway_dht:send_to_peer(NodeInfo, store, StoreMsg) of
            ok -> ok;
            {error, Reason} ->
                io:format("[DHT] store: Failed to send to peer: ~p~n", [Reason]),
                ok
        end
    end, ClosestNodes),

    {reply, ok, NewState};

handle_call({get_local, Key}, _From, #state{storage = Storage} = State) ->
    Reply = case maps:get(Key, Storage, undefined) of
        undefined ->
            not_found;
        Value when is_list(Value) ->
            %% Return the list of providers (may be empty list)
            {ok, Value};
        Value ->
            %% Legacy: single value, convert to list
            {ok, [Value]}
    end,
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
    %% Remove specific provider by node_id from the provider list
    NewStorage = case maps:get(Key, Storage, undefined) of
        undefined ->
            %% Key doesn't exist, nothing to delete
            Storage;
        Providers when is_list(Providers) ->
            %% Filter out the provider with matching node_id
            UpdatedProviders = lists:filter(fun(P) ->
                maps:get(node_id, P, undefined) =/= NodeId
            end, Providers),
            case UpdatedProviders of
                [] ->
                    %% No providers left, remove the key entirely
                    maps:remove(Key, Storage);
                _ ->
                    %% Update with remaining providers
                    Storage#{Key => UpdatedProviders}
            end;
        _SingleValue ->
            %% Legacy: single value, just remove the key
            maps:remove(Key, Storage)
    end,
    {reply, ok, State#state{storage = NewStorage}};

handle_call({find_value, Key, K}, _From, #state{routing_table = Table, storage = Storage} = State) ->
    %% First check local storage
    Reply = case maps:get(Key, Storage, undefined) of
        undefined ->
            io:format("[DHT] find_value: Key not found in local storage~n"),
            %% Not found locally - use DHT iterative lookup
            QueryFn = fun(_NodeInfo, _QueryKey) ->
                %% TODO: For now, we can't directly query remote nodes without a connection
                %% This would need to send FIND_VALUE RPC over QUIC to remote node
                %% Return nodes so the algorithm continues
                {nodes, [_NodeInfo]}
            end,

            case macula_routing_dht:find_value(Table, Key, K, QueryFn) of
                {ok, Value} ->
                    {ok, Value};
                {nodes, _Nodes} ->
                    %% Value not found, return empty list for service registry compatibility
                    {ok, []};
                {error, Reason} ->
                    {error, Reason}
            end;
        Value when is_list(Value) ->
            io:format("[DHT] find_value: Found ~p subscriber(s) in local storage~n", [length(Value)]),
            {ok, Value};
        Value ->
            io:format("[DHT] find_value: Found 1 subscriber (legacy single value)~n"),
            {ok, [Value]}
    end,
    {reply, Reply, State};

handle_call({handle_message, Message}, _From, State) ->
    {Reply, NewState} = process_dht_message(Message, State),
    {reply, Reply, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
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

%% @doc Find index of provider with matching node_id in provider list.
-spec find_provider_index(binary(), [map()]) -> pos_integer() | not_found.
find_provider_index(NodeId, ProviderList) ->
    find_provider_index(NodeId, ProviderList, 1).

find_provider_index(_NodeId, [], _Index) ->
    not_found;
find_provider_index(NodeId, [Provider | Rest], Index) ->
    %% Handle both atom and binary keys from MessagePack
    ProviderNodeId = case maps:get(node_id, Provider, undefined) of
        undefined -> maps:get(<<"node_id">>, Provider, undefined);
        Id -> Id
    end,
    case ProviderNodeId of
        NodeId -> Index;
        _ -> find_provider_index(NodeId, Rest, Index + 1)
    end.

%% @doc Process incoming DHT message and generate reply.
-spec process_dht_message(map(), #state{}) -> {map(), #state{}}.
process_dht_message(Message, State) ->
    case macula_routing_protocol:is_find_node(Message) of
        true ->
            handle_find_node(Message, State);
        false ->
            case macula_routing_protocol:is_store(Message) of
                true ->
                    handle_store(Message, State);
                false ->
                    case macula_routing_protocol:is_find_value(Message) of
                        true ->
                            handle_find_value(Message, State);
                        false ->
                            {#{type => error, reason => unknown_message}, State}
                    end
            end
    end.

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

    %% DEBUG: Log the Value to see its structure
    io:format("[DHT] handle_store: Value = ~p~n", [Value]),
    io:format("[DHT] handle_store: Value keys = ~p~n", [maps:keys(Value)]),

    %% Store using multi-value logic (same as store_local)
    ExistingProviders = maps:get(Key, Storage, []),

    %% Ensure we're working with a list (for backward compatibility)
    ProviderList = case is_list(ExistingProviders) of
        true -> ExistingProviders;
        false -> [ExistingProviders]  %% Legacy: convert single value to list
    end,

    %% Get node_id from the new provider (handle both atom and binary keys from MessagePack)
    NodeId = case maps:get(node_id, Value, undefined) of
        undefined -> maps:get(<<"node_id">>, Value, undefined);
        Id -> Id
    end,

    %% Update or append provider
    UpdatedProviders = case NodeId of
        undefined ->
            %% No node_id, just append (shouldn't happen in practice)
            io:format("[DHT] handle_store: Appending provider without node_id~n"),
            [Value | ProviderList];
        _ ->
            %% Check if this provider already exists by comparing node_ids
            ExistingIndex = find_provider_index(NodeId, ProviderList),
            case ExistingIndex of
                not_found ->
                    %% Append new provider
                    io:format("[DHT] handle_store: Adding new subscriber (was ~p, now ~p)~n",
                             [length(ProviderList), length(ProviderList) + 1]),
                    [Value | ProviderList];
                Index ->
                    %% Update existing provider (replace at index)
                    io:format("[DHT] handle_store: Updating existing subscriber at index ~p~n", [Index]),
                    lists:sublist(ProviderList, Index - 1) ++
                        [Value] ++
                        lists:nthtail(Index, ProviderList)
            end
    end,

    NewStorage = Storage#{Key => UpdatedProviders},

    %% Return success
    Reply = #{type => store_reply, result => ok},

    {Reply, State#state{storage = NewStorage}}.

%% @doc Handle FIND_VALUE request.
-spec handle_find_value(map(), #state{}) -> {map(), #state{}}.
handle_find_value(Message, #state{storage = Storage, routing_table = Table, config = Config} = State) ->
    {ok, Key} = macula_routing_protocol:decode_find_value(Message),

    %% Check local storage first
    Reply = case maps:get(Key, Storage, undefined) of
        undefined ->
            %% Value not found locally, return closest nodes
            io:format("[DHT] handle_find_value: Key not found, returning nodes~n"),
            K = maps:get(k, Config, 20),
            Closest = macula_routing_table:find_closest(Table, Key, K),
            macula_routing_protocol:encode_find_value_reply({nodes, Closest});

        Value when is_list(Value) ->
            %% Value found (multi-value list)
            io:format("[DHT] handle_find_value: Returning ~p subscriber(s)~n", [length(Value)]),
            macula_routing_protocol:encode_find_value_reply({value, Value});

        Value ->
            %% Value found (legacy single value)
            io:format("[DHT] handle_find_value: Returning 1 subscriber (legacy)~n"),
            macula_routing_protocol:encode_find_value_reply({value, Value})
    end,

    {Reply, State}.
