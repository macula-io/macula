%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Bridge Node - Manages connection to parent mesh level.
%%%
%%% The Bridge Node is responsible for:
%%% - Connecting to parent mesh (street to neighborhood to city to etc.)
%%% - Escalating DHT queries when local DHT misses
%%% - Caching results from parent queries locally
%%% - Maintaining connection health to parent bridges
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_bridge_node).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/1,
    escalate_query/2,
    escalate_query/3,
    store_to_parent/2,
    is_connected/1,
    get_stats/1,
    get_parent_bridges/1,
    set_parent_bridges/2
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
    mesh_level :: atom(),                    % cluster | street | neighborhood | city | etc.
    parent_bridges :: [binary()],            % List of parent bridge endpoints
    connected_parent :: binary() | undefined, % Currently connected parent bridge
    connection_pid :: pid() | undefined,     % PID of QUIC connection to parent
    escalation_timeout :: pos_integer(),     % Timeout for parent queries (ms)
    stats :: map()                           % Statistics
}).

-define(DEFAULT_ESCALATION_TIMEOUT, 5000).
-define(RECONNECT_INTERVAL, 5000).
-define(HEALTH_CHECK_INTERVAL, 30000).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start bridge node with registered name.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Escalate a DHT query to parent level.
%% Called when local DHT lookup fails.
-spec escalate_query(pid(), map()) -> {ok, term()} | {error, term()}.
escalate_query(Pid, Query) ->
    escalate_query(Pid, Query, ?DEFAULT_ESCALATION_TIMEOUT).

-spec escalate_query(pid(), map(), pos_integer()) -> {ok, term()} | {error, term()}.
escalate_query(Pid, Query, Timeout) ->
    gen_server:call(Pid, {escalate_query, Query}, Timeout + 1000).

%% @doc Store value to parent DHT (for advertisement propagation).
-spec store_to_parent(pid(), map()) -> ok | {error, term()}.
store_to_parent(Pid, StoreMsg) ->
    gen_server:call(Pid, {store_to_parent, StoreMsg}, 10000).

%% @doc Check if connected to parent bridge.
-spec is_connected(pid()) -> boolean().
is_connected(Pid) ->
    gen_server:call(Pid, is_connected).

%% @doc Get bridge node statistics.
-spec get_stats(pid()) -> {ok, map()}.
get_stats(Pid) ->
    gen_server:call(Pid, get_stats).

%% @doc Get list of parent bridge endpoints.
-spec get_parent_bridges(pid()) -> [binary()].
get_parent_bridges(Pid) ->
    gen_server:call(Pid, get_parent_bridges).

%% @doc Update parent bridge endpoints (for dynamic configuration).
-spec set_parent_bridges(pid(), [binary()]) -> ok.
set_parent_bridges(Pid, Bridges) ->
    gen_server:call(Pid, {set_parent_bridges, Bridges}).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init(Config) ->
    MeshLevel = maps:get(mesh_level, Config, cluster),
    ParentBridges = maps:get(parent_bridges, Config, []),
    EscalationTimeout = maps:get(escalation_timeout, Config, ?DEFAULT_ESCALATION_TIMEOUT),

    ?LOG_INFO("[BridgeNode] Starting at level ~p with parent bridges: ~p",
              [MeshLevel, ParentBridges]),

    State = #state{
        mesh_level = MeshLevel,
        parent_bridges = ParentBridges,
        connected_parent = undefined,
        connection_pid = undefined,
        escalation_timeout = EscalationTimeout,
        stats = init_stats()
    },

    %% Schedule initial connection attempt if we have parent bridges
    schedule_connect_if_needed(ParentBridges),

    %% Schedule periodic health checks
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), health_check),

    {ok, State}.

handle_call({escalate_query, Query}, _From, State) ->
    {Reply, NewState} = do_escalate_query(Query, State),
    {reply, Reply, NewState};

handle_call({store_to_parent, StoreMsg}, _From, State) ->
    {Reply, NewState} = do_store_to_parent(StoreMsg, State),
    {reply, Reply, NewState};

handle_call(is_connected, _From, #state{connected_parent = Parent} = State) ->
    {reply, Parent =/= undefined, State};

handle_call(get_stats, _From, #state{stats = Stats, mesh_level = Level,
                                      connected_parent = Parent} = State) ->
    FullStats = Stats#{
        mesh_level => Level,
        connected_parent => Parent,
        is_connected => Parent =/= undefined
    },
    {reply, {ok, FullStats}, State};

handle_call(get_parent_bridges, _From, #state{parent_bridges = Bridges} = State) ->
    {reply, Bridges, State};

handle_call({set_parent_bridges, Bridges}, _From, State) ->
    ?LOG_INFO("[BridgeNode] Updating parent bridges to: ~p", [Bridges]),
    NewState = State#state{parent_bridges = Bridges},
    schedule_connect_if_needed(Bridges),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(connect_to_parent, #state{parent_bridges = []} = State) ->
    %% No parent bridges configured - nothing to connect to
    {noreply, State};

handle_info(connect_to_parent, #state{parent_bridges = Bridges,
                                       connected_parent = undefined} = State) ->
    NewState = try_connect_to_parent(Bridges, State),
    {noreply, NewState};

handle_info(connect_to_parent, State) ->
    %% Already connected
    {noreply, State};

handle_info(health_check, State) ->
    NewState = perform_health_check(State),
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), health_check),
    {noreply, NewState};

handle_info({'DOWN', _Ref, process, Pid, Reason},
            #state{connection_pid = Pid} = State) ->
    ?LOG_WARNING("[BridgeNode] Parent connection lost: ~p", [Reason]),
    NewState = State#state{
        connected_parent = undefined,
        connection_pid = undefined,
        stats = increment_stat(disconnections, State#state.stats)
    },
    %% Schedule reconnection
    erlang:send_after(?RECONNECT_INTERVAL, self(), connect_to_parent),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Initialize statistics map.
-spec init_stats() -> map().
init_stats() ->
    #{
        queries_escalated => 0,
        queries_successful => 0,
        queries_failed => 0,
        stores_propagated => 0,
        cache_hits => 0,
        disconnections => 0,
        started_at => erlang:system_time(second)
    }.

%% @doc Increment a statistic counter.
-spec increment_stat(atom(), map()) -> map().
increment_stat(Key, Stats) ->
    maps:update_with(Key, fun(V) -> V + 1 end, 1, Stats).

%% @doc Schedule connection attempt if bridges are configured.
-spec schedule_connect_if_needed([binary()]) -> ok.
schedule_connect_if_needed([]) -> ok;
schedule_connect_if_needed(_Bridges) ->
    erlang:send_after(100, self(), connect_to_parent),
    ok.

%% @doc Try to connect to one of the parent bridges.
-spec try_connect_to_parent([binary()], #state{}) -> #state{}.
try_connect_to_parent([], State) ->
    ?LOG_WARNING("[BridgeNode] Failed to connect to any parent bridge"),
    erlang:send_after(?RECONNECT_INTERVAL, self(), connect_to_parent),
    State;
try_connect_to_parent([Bridge | Rest], State) ->
    ?LOG_INFO("[BridgeNode] Attempting to connect to parent bridge: ~p", [Bridge]),
    case connect_to_bridge(Bridge) of
        {ok, ConnPid} ->
            ?LOG_INFO("[BridgeNode] Connected to parent bridge: ~p", [Bridge]),
            erlang:monitor(process, ConnPid),
            State#state{
                connected_parent = Bridge,
                connection_pid = ConnPid
            };
        {error, Reason} ->
            ?LOG_WARNING("[BridgeNode] Failed to connect to ~p: ~p", [Bridge, Reason]),
            try_connect_to_parent(Rest, State)
    end.

%% @doc Establish QUIC connection to parent bridge.
-spec connect_to_bridge(binary()) -> {ok, pid()} | {error, term()}.
connect_to_bridge(BridgeEndpoint) ->
    %% Parse endpoint (format: "quic://host:port" or "host:port")
    case parse_bridge_endpoint(BridgeEndpoint) of
        {ok, Host, Port} ->
            %% Use macula_connection to establish QUIC connection
            case macula_connection:connect(Host, Port, #{}) of
                {ok, ConnPid} -> {ok, ConnPid};
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Parse bridge endpoint string.
-spec parse_bridge_endpoint(binary()) -> {ok, string(), pos_integer()} | {error, term()}.
parse_bridge_endpoint(Endpoint) when is_binary(Endpoint) ->
    parse_bridge_endpoint(binary_to_list(Endpoint));
parse_bridge_endpoint("quic://" ++ Rest) ->
    parse_host_port(Rest);
parse_bridge_endpoint(HostPort) ->
    parse_host_port(HostPort).

parse_host_port(HostPort) ->
    case string:split(HostPort, ":") of
        [Host, PortStr] ->
            case catch list_to_integer(PortStr) of
                Port when is_integer(Port), Port > 0, Port < 65536 ->
                    {ok, Host, Port};
                _ ->
                    {error, invalid_port}
            end;
        _ ->
            {error, invalid_endpoint}
    end.

%% @doc Perform DHT query escalation to parent.
-spec do_escalate_query(map(), #state{}) -> {{ok, term()} | {error, term()}, #state{}}.
do_escalate_query(_Query, #state{connected_parent = undefined} = State) ->
    {{error, not_connected}, increment_stats(queries_failed, State)};
do_escalate_query(Query, #state{connection_pid = ConnPid,
                                 escalation_timeout = Timeout} = State) ->
    Stats1 = increment_stat(queries_escalated, State#state.stats),

    %% Send query to parent bridge via QUIC connection
    case send_dht_query(ConnPid, Query, Timeout) of
        {ok, Result} ->
            %% Cache result locally via bridge_cache
            cache_result(Query, Result),
            Stats2 = increment_stat(queries_successful, Stats1),
            {{ok, Result}, State#state{stats = Stats2}};
        {error, Reason} ->
            ?LOG_WARNING("[BridgeNode] Query escalation failed: ~p", [Reason]),
            Stats2 = increment_stat(queries_failed, Stats1),
            {{error, Reason}, State#state{stats = Stats2}}
    end.

%% @doc Increment stats helper.
-spec increment_stats(atom(), #state{}) -> #state{}.
increment_stats(Key, #state{stats = Stats} = State) ->
    State#state{stats = increment_stat(Key, Stats)}.

%% @doc Send DHT query via QUIC connection.
-spec send_dht_query(pid(), map(), pos_integer()) -> {ok, term()} | {error, term()}.
send_dht_query(ConnPid, Query, Timeout) ->
    %% Encode query as DHT protocol message
    QueryType = maps:get(type, Query, find_value),
    Key = maps:get(key, Query, <<>>),

    %% Use internal RPC to query parent's DHT
    Procedure = <<"_dht.find_value">>,
    Args = #{<<"key">> => Key, <<"query_type">> => QueryType},

    case macula_connection:call(ConnPid, Procedure, Args, #{timeout => Timeout}) of
        {ok, #{<<"value">> := Value}} -> {ok, Value};
        {ok, #{<<"nodes">> := Nodes}} -> {nodes, Nodes};
        {error, _} = Error -> Error
    end.

%% @doc Cache query result locally.
-spec cache_result(map(), term()) -> ok.
cache_result(Query, Result) ->
    case whereis(macula_bridge_cache) of
        undefined -> ok;
        CachePid ->
            Key = maps:get(key, Query, <<>>),
            macula_bridge_cache:put(CachePid, Key, Result)
    end.

%% @doc Store value to parent DHT.
-spec do_store_to_parent(map(), #state{}) -> {ok | {error, term()}, #state{}}.
do_store_to_parent(_StoreMsg, #state{connected_parent = undefined} = State) ->
    {{error, not_connected}, State};
do_store_to_parent(StoreMsg, #state{connection_pid = ConnPid} = State) ->
    Key = maps:get(key, StoreMsg, <<>>),
    Value = maps:get(value, StoreMsg, #{}),

    %% Use internal RPC to store in parent's DHT
    Procedure = <<"_dht.store">>,
    Args = #{<<"key">> => Key, <<"value">> => Value},

    case macula_connection:call(ConnPid, Procedure, Args, #{timeout => 10000}) of
        {ok, _} ->
            Stats = increment_stat(stores_propagated, State#state.stats),
            {ok, State#state{stats = Stats}};
        {error, _} = Error ->
            {Error, State}
    end.

%% @doc Perform periodic health check on parent connection.
-spec perform_health_check(#state{}) -> #state{}.
perform_health_check(#state{connected_parent = undefined} = State) ->
    State;
perform_health_check(#state{connection_pid = ConnPid} = State) ->
    case is_process_alive(ConnPid) of
        true -> State;
        false ->
            ?LOG_WARNING("[BridgeNode] Parent connection process dead, reconnecting"),
            erlang:send_after(100, self(), connect_to_parent),
            State#state{
                connected_parent = undefined,
                connection_pid = undefined
            }
    end.
