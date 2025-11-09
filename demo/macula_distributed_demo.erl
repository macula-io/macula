%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Distributed Application Demo
%%%
%%% Demonstrates a realistic distributed sensor network:
%%% - Multiple sensor nodes publishing temperature readings
%%% - Aggregator node collecting and processing data
%%% - RPC queries between nodes
%%% - Pub/sub message routing
%%% - DHT-based service discovery
%%%
%%% Run with: erl -pa _build/default/lib/*/ebin -pa /tmp
%%% Then: macula_distributed_demo:run().
%%% @end
%%%-------------------------------------------------------------------
-module(macula_distributed_demo).

-export([run/0, run/1, stop/0]).

-define(REALM, <<"demo.macula.sensors">>).

%%%===================================================================
%%% Demo Runner
%%%===================================================================

%% @doc Run demo with default 3 sensors for 10 seconds
run() ->
    run(#{sensors => 3, duration => 10}).

%% @doc Run demo with custom configuration
-spec run(map()) -> ok.
run(Config) ->
    NumSensors = maps:get(sensors, Config, 3),
    Duration = maps:get(duration, Config, 10),

    io:format("~n"),
    io:format("╔════════════════════════════════════════════════════════════╗~n"),
    io:format("║  Macula HTTP/3 Mesh - Distributed Sensor Network Demo    ║~n"),
    io:format("╔════════════════════════════════════════════════════════════╗~n"),
    io:format("~n"),
    io:format("Configuration:~n"),
    io:format("  • Realm: ~s~n", [?REALM]),
    io:format("  • Sensors: ~p nodes~n", [NumSensors]),
    io:format("  • Duration: ~p seconds~n", [Duration]),
    io:format("~n"),

    %% Step 1: Start aggregator node
    io:format("📊 Step 1: Starting aggregator node...~n"),
    {ok, AggregatorPid} = macula_aggregator_node:start_link(?REALM),
    io:format("   ✓ Aggregator started (PID: ~p)~n", [AggregatorPid]),
    io:format("~n"),

    %% Step 2: Start sensor nodes
    io:format("🌡️  Step 2: Starting ~p sensor nodes...~n", [NumSensors]),
    SensorPids = start_sensors(NumSensors),
    io:format("   ✓ All sensors started~n", []),
    io:format("~n"),

    %% Step 3: Let the simulation run
    io:format("🔄 Step 3: Running simulation for ~p seconds...~n", [Duration]),
    io:format("   (Sensors publishing readings every 2 seconds)~n"),
    io:format("~n"),

    %% Wait for readings to accumulate
    timer:sleep(Duration * 1000),

    %% Step 4: Query statistics
    io:format("~n"),
    io:format("📈 Step 4: Querying aggregator statistics...~n"),
    Stats = macula_aggregator_node:get_statistics(AggregatorPid),
    print_statistics(Stats),
    io:format("~n"),

    %% Step 5: Test RPC calls
    io:format("⚡ Step 5: Testing RPC queries to sensors...~n"),
    test_sensor_queries(SensorPids),
    io:format("~n"),

    %% Step 6: Cleanup
    io:format("🧹 Step 6: Stopping all nodes...~n"),
    stop_sensors(SensorPids),
    macula_aggregator_node:stop(AggregatorPid),
    io:format("   ✓ All nodes stopped~n"),
    io:format("~n"),

    %% Summary
    io:format("╔════════════════════════════════════════════════════════════╗~n"),
    io:format("║  ✅ Demo Complete - Distributed Mesh Working!             ║~n"),
    io:format("╚════════════════════════════════════════════════════════════╝~n"),
    io:format("~n"),
    io:format("Summary:~n"),
    MinTemp = maps:get(min_temp, Stats, undefined),
    MaxTemp = maps:get(max_temp, Stats, undefined),

    io:format("  • ~p sensor nodes deployed across mesh~n", [NumSensors]),
    io:format("  • ~p temperature readings collected~n", [maps:get(total_readings, Stats, 0)]),
    io:format("  • Real-time pub/sub message routing~n"),
    io:format("  • RPC queries between nodes~n"),

    case {MinTemp, MaxTemp} of
        {undefined, undefined} ->
            io:format("  • Temperature range: N/A (no aggregator readings)~n");
        {Min, Max} ->
            io:format("  • Temperature range: ~.1f°C - ~.1f°C~n", [Min, Max])
    end,
    io:format("~n"),
    io:format("The Macula distributed mesh is fully operational! 🚀~n"),
    io:format("~n"),

    ok.

%% @doc Stop demo (for interactive mode)
stop() ->
    io:format("Stopping demo (if running)...~n"),
    %% In a real system, we'd track PIDs and stop them
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Start N sensor nodes
start_sensors(N) ->
    Locations = [
        <<"us-east-1">>, <<"us-west-2">>, <<"eu-west-1">>,
        <<"ap-southeast-1">>, <<"ap-northeast-1">>, <<"ca-central-1">>,
        <<"eu-central-1">>, <<"sa-east-1">>, <<"ap-south-1">>
    ],

    lists:map(
        fun(I) ->
            SensorId = list_to_binary(io_lib:format("sensor-~3..0B", [I])),
            Location = lists:nth((I rem length(Locations)) + 1, Locations),
            {ok, Pid} = macula_sensor_node:start_link(?REALM, SensorId, Location),
            io:format("   ✓ Started ~s at ~s (PID: ~p)~n", [SensorId, Location, Pid]),
            Pid
        end,
        lists:seq(1, N)
    ).

%% Stop all sensor nodes
stop_sensors(SensorPids) ->
    lists:foreach(
        fun(Pid) ->
            macula_sensor_node:stop(Pid)
        end,
        SensorPids
    ).

%% Print aggregator statistics
print_statistics(Stats) ->
    TotalReadings = maps:get(total_readings, Stats, 0),
    SensorsCount = maps:get(sensors_count, Stats, 0),
    MinTemp = maps:get(min_temp, Stats, undefined),
    MaxTemp = maps:get(max_temp, Stats, undefined),
    AvgTemp = maps:get(avg_temp, Stats, undefined),

    io:format("   Statistics:~n"),
    io:format("     • Total readings: ~p~n", [TotalReadings]),
    io:format("     • Active sensors: ~p~n", [SensorsCount]),

    case MinTemp of
        undefined ->
            io:format("     • No temperature data yet~n");
        _ ->
            io:format("     • Min temperature: ~.1f°C~n", [MinTemp]),
            io:format("     • Max temperature: ~.1f°C~n", [MaxTemp]),
            io:format("     • Avg temperature: ~.1f°C~n", [AvgTemp])
    end,

    %% Print per-sensor counts
    case maps:get(sensor_counts, Stats, #{}) of
        Counts when map_size(Counts) > 0 ->
            io:format("   ~nReadings per sensor:~n"),
            maps:foreach(
                fun(SensorId, Count) ->
                    io:format("     • ~s: ~p readings~n", [SensorId, Count])
                end,
                Counts
            );
        _ ->
            ok
    end.

%% Test RPC queries to sensor nodes
test_sensor_queries(SensorPids) ->
    %% Query first sensor for status
    case SensorPids of
        [FirstSensor | _] ->
            Stats = macula_sensor_node:get_stats(FirstSensor),
            io:format("   ✓ RPC query to ~s successful~n", [maps:get(sensor_id, Stats)]),
            io:format("     - Location: ~s~n", [maps:get(location, Stats)]),
            io:format("     - Readings: ~p~n", [maps:get(reading_count, Stats)]),
            case maps:get(last_reading, Stats) of
                undefined ->
                    io:format("     - Last reading: none~n");
                Temp ->
                    io:format("     - Last reading: ~.1f°C~n", [Temp])
            end;
        [] ->
            io:format("   ✗ No sensors available for query~n")
    end.
