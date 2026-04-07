%%%-------------------------------------------------------------------
%%% @doc
%%% GenServer managing Kademlia DHT routing table and operations.
%%% Integrates all routing components: table, DHT algorithms, protocol.
%%%
%%% Pure functional helpers are in macula_routing_storage.
%%% Network transport and escalation are in macula_routing_network.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_server).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/2,
    add_node/2,
    remove_node/2,
    find_closest/3,
    store_local/3,
    store/3,
    get_local/2,
    get_all_keys/1,
    delete_local/3,
    find_value/3,
    find_value_local/2,
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

%% Stale peer eviction interval (60 seconds)
-define(EVICT_INTERVAL_MS, 60000).
%% Peers not seen in 5 minutes are considered stale
-define(STALE_THRESHOLD_MS, 300000).
%% DHT value expiry interval (30 seconds)
-define(VALUE_EXPIRY_INTERVAL_MS, 30000).

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

%% @doc Remove node from routing table (async - does not block caller).
-spec remove_node(pid(), binary()) -> ok.
remove_node(Pid, NodeId) ->
    gen_server:cast(Pid, {remove_node, NodeId}).

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

%% @doc Find value in DHT -- goes through gen_server for full iterative lookup.
%% For local-only reads (answering peer queries), use find_value_local/2.
-spec find_value(pid(), binary(), pos_integer()) ->
    {ok, term()} | {nodes, [macula_routing_bucket:node_info()]} | {error, term()}.
find_value(Pid, Key, K) ->
    gen_server:call(Pid, {find_value, Key, K}, 10000).

%% @doc Fast local-only lookup via ETS -- for answering incoming FIND_VALUE from peers.
%% Does NOT query the network. Returns only locally stored values.
-spec find_value_local(binary(), pos_integer()) ->
    {ok, term()} | {error, not_found}.
find_value_local(Key, _K) ->
    case ets:info(macula_dht_storage) of
        undefined -> {error, not_found};
        _ ->
            case ets:lookup(macula_dht_storage, Key) of
                [{_, Values}] when is_list(Values), Values =/= [] -> {ok, Values};
                _ -> {error, not_found}
            end
    end.

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

    %% Named ETS table for concurrent DHT storage reads (bypasses gen_server mailbox)
    StorageEts = ets:new(macula_dht_storage, [named_table, set, public, {read_concurrency, true}]),

    State = #state{
        local_node_id = LocalNodeId,
        routing_table = macula_routing_table:new(LocalNodeId, K),
        storage = #{},
        config = #{
            k => K,
            alpha => Alpha,
            escalation_enabled => EscalationEnabled,
            escalation_timeout => EscalationTimeout,
            storage_ets => StorageEts
        }
    },

    erlang:send_after(?EVICT_INTERVAL_MS, self(), evict_stale_peers),
    erlang:send_after(?VALUE_EXPIRY_INTERVAL_MS, self(), expire_stale_values),

    {ok, State}.

%% @private
handle_call({find_closest, Target, K}, _From, #state{routing_table = Table} = State) ->
    Closest = macula_routing_table:find_closest(Table, Target, K),
    {reply, Closest, State};

handle_call({store_local, Key, Value}, _From, #state{storage = Storage, config = Config} = State) ->
    ExistingProviders = maps:get(Key, Storage, []),
    ProviderList = macula_routing_storage:ensure_provider_list(ExistingProviders),
    NodeId = maps:get(node_id, Value, undefined),
    UpdatedProviders = macula_routing_storage:upsert_provider(NodeId, Value, ProviderList),
    NewStorage = Storage#{Key => UpdatedProviders},
    Ets = maps:get(storage_ets, Config, undefined),
    macula_routing_storage:mirror_to_ets(Ets, Key, UpdatedProviders),
    {reply, ok, State#state{storage = NewStorage}};

handle_call({store, Key, Value}, _From, #state{routing_table = Table, config = Config} = State) ->
    %% 1. Store locally first
    NewState = case handle_call({store_local, Key, Value}, _From, State) of
        {reply, ok, S} -> S;
        _ -> State
    end,

    %% 2. Find k closest nodes to Key
    K = maps:get(k, Config, 20),
    ClosestNodes = macula_routing_table:find_closest(Table, Key, K),

    %% 3. Send STORE message to each node (fire-and-forget, non-blocking)
    StoreMsg = macula_routing_protocol:encode_store(Key, Value),
    spawn(fun() -> macula_routing_network:propagate_store_to_peers(ClosestNodes, StoreMsg, Key, Value) end),

    {reply, ok, NewState};

handle_call({get_local, Key}, _From, #state{storage = Storage} = State) ->
    Reply = macula_routing_storage:format_storage_value(maps:get(Key, Storage, undefined)),
    {reply, Reply, State};

handle_call(get_all_keys, _From, #state{storage = Storage} = State) ->
    {reply, {ok, maps:keys(Storage)}, State};

handle_call(get_routing_table, _From, #state{routing_table = Table} = State) ->
    {reply, Table, State};

handle_call(size, _From, #state{routing_table = Table} = State) ->
    {reply, macula_routing_table:size(Table), State};

handle_call({delete_local, Key, NodeId}, _From, #state{storage = Storage, config = Config} = State) ->
    NewStorage = macula_routing_storage:delete_provider_from_storage(Key, NodeId, Storage),
    Ets = maps:get(storage_ets, Config, undefined),
    macula_routing_storage:mirror_delete_to_ets(Ets, Key, NewStorage),
    {reply, ok, State#state{storage = NewStorage}};

handle_call({find_value, Key, K}, From, #state{routing_table = Table, storage = Storage,
                                              config = Config} = State) ->
    ?LOG_DEBUG("find_value: key=~p, storage_size=~p", [Key, maps:size(Storage)]),
    LocalValues = case maps:get(Key, Storage, undefined) of
        undefined -> [];
        V when is_list(V) -> V;
        V -> [V]
    end,
    %% Always query the network -- local values may be incomplete (e.g. pubsub subscribers).
    %% Spawn to avoid blocking the routing server.
    spawn(fun() ->
        RemoteResult = macula_routing_network:find_value_with_escalation(Key, K, Storage, Table, Config),
        RemoteValues = case RemoteResult of
            {ok, R} when is_list(R) -> R;
            _ -> []
        end,
        AllValues = macula_routing_storage:deduplicate_providers(LocalValues ++ RemoteValues),
        Reply = case AllValues of
            [] -> {error, not_found};
            _ -> {ok, AllValues}
        end,
        gen_server:reply(From, Reply)
    end),
    {noreply, State};

handle_call({handle_message, Message}, _From, State) ->
    {Reply, NewState} = process_dht_message(Message, State),
    {reply, Reply, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({add_node, NodeInfo}, #state{routing_table = Table, storage = Storage,
                                         config = Config} = State) ->
    OldSize = macula_routing_table:size(Table),
    NewTable = macula_routing_table:add_node(Table, NodeInfo),
    NewSize = macula_routing_table:size(NewTable),
    NodeId = maps:get(node_id, NodeInfo, undefined),
    ?LOG_DEBUG("[RoutingServer] add_node: node_id=~s, table_size: ~p -> ~p",
              [case NodeId of B when is_binary(B), byte_size(B) =:= 32 -> binary:encode_hex(B);
                              B when is_binary(B) -> B; _ -> <<"?">> end,
               OldSize, NewSize]),
    case NewSize > OldSize of
        true ->
            spawn(fun() -> macula_routing_network:replicate_to_new_peer(NodeInfo, Storage, NewTable, Config) end);
        false ->
            ok
    end,
    {noreply, State#state{routing_table = NewTable}};

handle_cast({remove_node, NodeId}, #state{routing_table = Table} = State) ->
    OldSize = macula_routing_table:size(Table),
    NewTable = macula_routing_table:remove_node(Table, NodeId),
    NewSize = macula_routing_table:size(NewTable),
    ?LOG_DEBUG("[RoutingServer] remove_node: node_id=~s, table_size: ~p -> ~p",
              [binary:encode_hex(NodeId), OldSize, NewSize]),
    {noreply, State#state{routing_table = NewTable}};

%% @private
%% Handle async store (from _dht.store RPC) - stores locally only without propagation
handle_cast({store, Key, Value}, #state{storage = Storage, config = Config} = State) when is_binary(Key) ->
    ?LOG_DEBUG("Async STORE received - key hash prefix: ~p, value: ~p",
              [binary:part(Key, 0, min(8, byte_size(Key))), Value]),
    ExistingProviders = maps:get(Key, Storage, []),
    ProviderList = macula_routing_storage:ensure_provider_list(ExistingProviders),
    NodeId = macula_routing_storage:get_node_id_from_value(Value),
    UpdatedProviders = macula_routing_storage:upsert_provider(NodeId, Value, ProviderList),
    NewStorage = Storage#{Key => UpdatedProviders},
    Ets = maps:get(storage_ets, Config, undefined),
    macula_routing_storage:mirror_to_ets(Ets, Key, UpdatedProviders),
    ?LOG_DEBUG("Stored value for key, now have ~p provider(s)", [length(UpdatedProviders)]),
    {noreply, State#state{storage = NewStorage}};

handle_cast({store, Key, _Value}, State) ->
    ?LOG_ERROR("Async STORE received with non-binary key: ~p", [Key]),
    {noreply, State};

handle_cast({handle_message_async, Message}, State) ->
    {_Reply, NewState} = process_dht_message(Message, State),
    {noreply, NewState};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(evict_stale_peers, #state{routing_table = Table} = State) ->
    StaleThreshold = erlang:system_time(millisecond) - ?STALE_THRESHOLD_MS,
    OldSize = macula_routing_table:size(Table),
    NewTable = macula_routing_table:evict_stale(Table, StaleThreshold),
    NewSize = macula_routing_table:size(NewTable),
    case OldSize - NewSize of
        0 -> ok;
        Evicted ->
            ?LOG_INFO("[RoutingServer] Evicted ~p stale peer(s) (table: ~p -> ~p)",
                      [Evicted, OldSize, NewSize])
    end,
    erlang:send_after(?EVICT_INTERVAL_MS, self(), evict_stale_peers),
    {noreply, State#state{routing_table = NewTable}};

handle_info(expire_stale_values, #state{storage = Storage, config = Config} = State) ->
    Now = erlang:system_time(millisecond),
    {NewStorage, Expired} = maps:fold(fun(Key, Providers, {StorageAcc, ExpiredCount}) ->
        Fresh = [P || P <- Providers, not macula_routing_storage:is_provider_expired(P, Now)],
        case {Fresh, length(Providers) - length(Fresh)} of
            {[], Removed} ->
                {maps:remove(Key, StorageAcc), ExpiredCount + Removed};
            {Kept, 0} ->
                {StorageAcc#{Key => Kept}, ExpiredCount};
            {Kept, Removed} ->
                {StorageAcc#{Key => Kept}, ExpiredCount + Removed}
        end
    end, {Storage, 0}, Storage),
    case Expired of
        0 -> ok;
        N -> ?LOG_INFO("[RoutingServer] Expired ~p stale DHT value(s)", [N])
    end,
    %% Mirror to ETS
    case Expired > 0 of
        true ->
            case maps:get(storage_ets, Config, undefined) of
                undefined -> ok;
                Ets ->
                    ets:delete_all_objects(Ets),
                    maps:foreach(fun(K, V) -> ets:insert(Ets, {K, V}) end, NewStorage)
            end;
        false -> ok
    end,
    erlang:send_after(?VALUE_EXPIRY_INTERVAL_MS, self(), expire_stale_values),
    {noreply, State#state{storage = NewStorage}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal Functions - DHT Message Processing
%%%===================================================================

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
    TableSize = macula_routing_table:size(Table),
    Closest = macula_routing_table:find_closest(Table, Target, K),
    ?LOG_DEBUG("[RoutingServer] FIND_NODE: target=~s, table_size=~p, found=~p",
              [binary:encode_hex(Target), TableSize, length(Closest)]),
    Reply = macula_routing_protocol:encode_find_node_reply(Closest),
    {Reply, State}.

%% @doc Handle STORE request.
-spec handle_store(map(), #state{}) -> {map(), #state{}}.
handle_store(Message, #state{storage = Storage} = State) ->
    {ok, Key, Value} = macula_routing_protocol:decode_store(Message),
    ?LOG_DEBUG("[RoutingServer] STORE received: key_prefix=~p, value_node_id=~p",
             [binary:part(Key, 0, min(8, byte_size(Key))), maps:get(node_id, Value, maps:get(<<"node_id">>, Value, unknown))]),
    ?LOG_DEBUG("STORE: key=~p, value=~p", [Key, Value]),
    ExistingProviders = maps:get(Key, Storage, []),
    ProviderList = macula_routing_storage:ensure_provider_list(ExistingProviders),
    NodeId = macula_routing_storage:get_node_id_from_value(Value),
    UpdatedProviders = macula_routing_storage:upsert_provider(NodeId, Value, ProviderList),
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
    ?LOG_DEBUG("[RoutingServer] FIND_VALUE: key_prefix=~p, storage_size=~p, found=~p",
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
