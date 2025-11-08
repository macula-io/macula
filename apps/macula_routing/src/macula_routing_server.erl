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
    get_local/2,
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

%% @doc Start routing server.
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(LocalNodeId, Config) ->
    gen_server:start_link(?MODULE, {LocalNodeId, Config}, []).

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

%% @doc Get value from local storage.
-spec get_local(pid(), binary()) -> {ok, term()} | not_found.
get_local(Pid, Key) ->
    gen_server:call(Pid, {get_local, Key}).

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
    NewStorage = Storage#{Key => Value},
    {reply, ok, State#state{storage = NewStorage}};

handle_call({get_local, Key}, _From, #state{storage = Storage} = State) ->
    Reply = case maps:get(Key, Storage, undefined) of
        undefined -> not_found;
        Value -> {ok, Value}
    end,
    {reply, Reply, State};

handle_call(get_routing_table, _From, #state{routing_table = Table} = State) ->
    {reply, Table, State};

handle_call(size, _From, #state{routing_table = Table} = State) ->
    Size = macula_routing_table:size(Table),
    {reply, Size, State};

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

    %% Store locally
    NewStorage = Storage#{Key => Value},

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
            K = maps:get(k, Config, 20),
            Closest = macula_routing_table:find_closest(Table, Key, K),
            macula_routing_protocol:encode_find_value_reply({nodes, Closest});

        Value ->
            %% Value found
            macula_routing_protocol:encode_find_value_reply({value, Value})
    end,

    {Reply, State}.
