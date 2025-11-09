%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Sensor Node - GenServer Demo
%%%
%%% Demonstrates a realistic distributed application:
%%% - Sensor nodes that publish temperature readings
%%% - Aggregator node that collects and processes readings
%%% - RPC-based query service
%%% - DHT-based service discovery
%%%
%%% This shows how real applications would be built on the Macula platform.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_sensor_node).

-behaviour(gen_server).

%% API
-export([
    start_link/3,
    stop/1,
    get_stats/1
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
    sensor_id :: binary(),
    location :: binary(),
    pubsub_registry :: macula_pubsub_registry:registry(),
    rpc_registry :: macula_rpc_registry:registry(),
    routing_table :: macula_routing_table:routing_table(),
    reading_count = 0 :: non_neg_integer(),
    last_reading :: float() | undefined
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start a sensor node
-spec start_link(binary(), binary(), binary()) -> {ok, pid()}.
start_link(Realm, SensorId, Location) ->
    gen_server:start_link(?MODULE, {Realm, SensorId, Location}, []).

%% @doc Stop a sensor node
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Get statistics from sensor node
-spec get_stats(pid()) -> map().
get_stats(Pid) ->
    gen_server:call(Pid, get_stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Realm, SensorId, Location}) ->
    %% Create macula node
    Metadata = #{
        sensor_id => SensorId,
        location => Location,
        type => sensor
    },
    Node = macula_node:new(Realm, Metadata),

    %% Initialize registries
    PubSubReg = macula_pubsub_registry:new(),
    RpcReg = macula_rpc_registry:new(),
    RoutingTable = macula_routing_table:new(macula_node:get_id(Node), 20),

    %% Register RPC handler for queries
    RpcReg2 = register_query_handler(RpcReg, SensorId),

    %% Schedule first reading
    schedule_reading(),

    State = #state{
        node = Node,
        sensor_id = SensorId,
        location = Location,
        pubsub_registry = PubSubReg,
        rpc_registry = RpcReg2,
        routing_table = RoutingTable,
        reading_count = 0,
        last_reading = undefined
    },

    io:format("[~s] Sensor node started at ~s~n", [SensorId, Location]),

    {ok, State}.

handle_call(get_stats, _From, State) ->
    Stats = #{
        sensor_id => State#state.sensor_id,
        location => State#state.location,
        reading_count => State#state.reading_count,
        last_reading => State#state.last_reading,
        node_id => macula_id:to_hex(macula_node:get_id(State#state.node))
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(take_reading, State) ->
    %% Simulate temperature reading (15-30°C)
    Temperature = 15.0 + rand:uniform() * 15.0,

    %% Publish reading via pub/sub
    Topic = sensor_topic(State#state.sensor_id),
    Payload = #{
        sensor_id => State#state.sensor_id,
        location => State#state.location,
        temperature => Temperature,
        timestamp => macula_time:timestamp(),
        unit => <<"celsius">>
    },

    io:format("[~s] Reading: ~.1f°C (published to ~s)~n",
              [State#state.sensor_id, Temperature, Topic]),

    %% In a real system, this would publish over the mesh via QUIC
    %% For this demo, send directly to aggregator if registered
    case whereis(macula_aggregator_node) of
        undefined -> ok;
        Pid -> gen_server:cast(Pid, {sensor_reading, Payload})
    end,

    %% Schedule next reading
    schedule_reading(),

    NewState = State#state{
        reading_count = State#state.reading_count + 1,
        last_reading = Temperature
    },

    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    io:format("[~s] Sensor node stopping~n", [State#state.sensor_id]),
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Register RPC handler for sensor queries
register_query_handler(RpcReg, SensorId) ->
    QueryHandler = fun(Args) ->
        case Args of
            #{<<"sensor_id">> := SensorId} ->
                {ok, #{
                    sensor_id => SensorId,
                    status => <<"online">>,
                    message => <<"Sensor responding">>
                }};
            _ ->
                {error, sensor_id_mismatch}
        end
    end,

    Uri = <<<<"sensor.query.">>/binary, SensorId/binary>>,
    macula_rpc_registry:register(
        RpcReg,
        Uri,
        QueryHandler,
        #{description => <<"Sensor query endpoint">>}
    ).

%% Generate topic for sensor readings
sensor_topic(SensorId) ->
    <<<<"sensors.temperature.">>/binary, SensorId/binary>>.

%% Schedule next reading (every 2 seconds)
schedule_reading() ->
    erlang:send_after(2000, self(), take_reading).
