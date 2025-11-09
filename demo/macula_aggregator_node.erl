%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Aggregator Node - GenServer Demo
%%%
%%% Collects temperature readings from sensor nodes and provides:
%%% - Real-time statistics (min, max, average)
%%% - Historical data storage
%%% - RPC query interface
%%% - Pub/sub subscription to sensor topics
%%%
%%% Demonstrates subscriber pattern in distributed mesh applications.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_aggregator_node).

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    stop/1,
    get_statistics/1,
    get_readings/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {
    node :: macula_node:macula_node(),
    pubsub_registry :: macula_pubsub_registry:registry(),
    rpc_registry :: macula_rpc_registry:registry(),
    routing_table :: macula_routing_table:routing_table(),
    readings = [] :: [map()],  % Store last 100 readings
    total_readings = 0 :: non_neg_integer(),
    sensors_seen = #{} :: #{binary() => non_neg_integer()}
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start an aggregator node
-spec start_link(binary()) -> {ok, pid()}.
start_link(Realm) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Realm, []).

%% @doc Stop the aggregator node
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Get current statistics
-spec get_statistics(pid()) -> map().
get_statistics(Pid) ->
    gen_server:call(Pid, get_statistics).

%% @doc Get recent readings
-spec get_readings(pid()) -> [map()].
get_readings(Pid) ->
    gen_server:call(Pid, get_readings).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Realm) ->
    %% Create macula node
    Metadata = #{
        name => <<"aggregator">>,
        type => aggregator
    },
    Node = macula_node:new(Realm, Metadata),

    %% Initialize registries
    PubSubReg = macula_pubsub_registry:new(),
    RpcReg = macula_rpc_registry:new(),
    RoutingTable = macula_routing_table:new(macula_node:get_id(Node), 20),

    %% Subscribe to all sensor temperature readings
    PubSubReg2 = macula_pubsub_registry:subscribe(
        PubSubReg,
        <<"aggregator">>,
        <<"sensors.temperature.*">>,  % Wildcard: all sensor IDs
        self()
    ),

    %% Register RPC handlers
    RpcReg2 = register_rpc_handlers(RpcReg),

    State = #state{
        node = Node,
        pubsub_registry = PubSubReg2,
        rpc_registry = RpcReg2,
        routing_table = RoutingTable,
        readings = [],
        total_readings = 0,
        sensors_seen = #{}
    },

    io:format("[Aggregator] Started and subscribed to sensors.temperature.*~n"),

    {ok, State}.

handle_call(get_statistics, _From, State) ->
    Stats = calculate_statistics(State),
    {reply, Stats, State};

handle_call(get_readings, _From, State) ->
    {reply, State#state.readings, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({sensor_reading, Reading}, State) ->
    %% Process incoming sensor reading
    NewState = process_reading(Reading, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[Aggregator] Stopping~n"),
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Register RPC handlers for queries
register_rpc_handlers(RpcReg) ->
    %% Statistics query handler
    StatsHandler = fun(_Args) ->
        case get_statistics(self()) of
            {error, _} = Error -> Error;
            Stats -> {ok, Stats}
        end
    end,

    RpcReg2 = macula_rpc_registry:register(
        RpcReg,
        <<"aggregator.stats">>,
        StatsHandler,
        #{description => <<"Get aggregated statistics">>}
    ),

    %% Readings query handler
    ReadingsHandler = fun(Args) ->
        Limit = maps:get(<<"limit">>, Args, 10),
        Readings = get_readings(self()),
        LimitedReadings = lists:sublist(Readings, Limit),
        {ok, #{readings => LimitedReadings}}
    end,

    macula_rpc_registry:register(
        RpcReg2,
        <<"aggregator.readings">>,
        ReadingsHandler,
        #{description => <<"Get recent readings">>}
    ).

%% Process a new sensor reading
process_reading(Reading, State) ->
    SensorId = maps:get(sensor_id, Reading, <<"unknown">>),
    Temperature = maps:get(temperature, Reading, 0.0),

    %% Update sensor count
    SensorCount = maps:get(SensorId, State#state.sensors_seen, 0),
    NewSensorsSeen = maps:put(SensorId, SensorCount + 1, State#state.sensors_seen),

    %% Store reading (keep last 100)
    NewReadings = lists:sublist([Reading | State#state.readings], 100),

    io:format("[Aggregator] Received: ~s = ~.1f°C (total: ~p readings)~n",
              [SensorId, Temperature, State#state.total_readings + 1]),

    State#state{
        readings = NewReadings,
        total_readings = State#state.total_readings + 1,
        sensors_seen = NewSensorsSeen
    }.

%% Calculate statistics from stored readings
calculate_statistics(State) when State#state.total_readings =:= 0 ->
    #{
        total_readings => 0,
        sensors_count => 0,
        min_temp => undefined,
        max_temp => undefined,
        avg_temp => undefined
    };

calculate_statistics(State) ->
    Temps = [maps:get(temperature, R) || R <- State#state.readings],

    MinTemp = lists:min(Temps),
    MaxTemp = lists:max(Temps),
    AvgTemp = lists:sum(Temps) / length(Temps),

    #{
        total_readings => State#state.total_readings,
        sensors_count => maps:size(State#state.sensors_seen),
        sensor_counts => State#state.sensors_seen,
        min_temp => MinTemp,
        max_temp => MaxTemp,
        avg_temp => AvgTemp,
        recent_readings => length(State#state.readings)
    }.
