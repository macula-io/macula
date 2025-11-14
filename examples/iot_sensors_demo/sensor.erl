#!/usr/bin/env escript
%%% @doc
%%% Macula IoT Sensor Demo - Sensor Node
%%%
%%% Simulates an IoT sensor that publishes environmental data
%%% (temperature, humidity, light) to the Macula mesh.
%%%
%%% Sensors can be deployed behind NAT/firewalls and will
%%% publish data to any subscribed dashboard.
%%%
%%% Usage:
%%%   ./sensor.erl <sensor_id> <location>
%%%
%%% Examples:
%%%   ./sensor.erl sensor-01 "Living Room"
%%%   ./sensor.erl sensor-02 "Bedroom"
%%%   ./sensor.erl sensor-03 "Kitchen"
%%% @end

-mode(compile).

main([]) ->
    io:format("Usage: ./sensor.erl <sensor_id> <location>~n"),
    io:format("Example: ./sensor.erl sensor-01 \"Living Room\"~n"),
    halt(1);

main([SensorId, Location]) ->
    io:format("~n=== Macula IoT Sensor Demo ===~n"),
    io:format("Sensor: ~s~n", [SensorId]),
    io:format("Location: ~s~n~n", [Location]),

    %% Start required applications
    application:ensure_all_started(crypto),
    application:ensure_all_started(ssl),
    application:ensure_all_started(macula),

    %% Connect to gateway
    io:format("[~s] Connecting to Macula Gateway...~n", [SensorId]),
    GatewayUrl = <<"https://localhost:9443">>,
    Realm = <<"com.example.iot">>,

    case macula_client:connect(GatewayUrl, #{
        realm => Realm,
        node_id => list_to_binary(SensorId)
    }) of
        {ok, Client} ->
            io:format("[~s] Connected! Publishing to realm: ~s~n", [SensorId, Realm]),
            io:format("[~s] Press Ctrl+C to stop~n~n", [SensorId]),

            %% Start publishing loop
            publish_loop(Client, list_to_binary(SensorId), list_to_binary(Location), initial_state());
        {error, Reason} ->
            io:format("[~s] Connection error: ~p~n", [SensorId, Reason]),
            halt(1)
    end.

%% @doc Initial sensor state with baseline values
initial_state() ->
    #{
        temperature => 20.0 + rand:uniform() * 5.0,  % 20-25°C
        humidity => 40.0 + rand:uniform() * 20.0,     % 40-60%
        light => 300.0 + rand:uniform() * 200.0       % 300-500 lux
    }.

%% @doc Main publishing loop - sends readings every 2 seconds
publish_loop(Client, SensorId, Location, State) ->
    %% Generate new readings with small random variations
    NewState = update_readings(State),

    %% Create sensor reading event
    Reading = #{
        sensor_id => SensorId,
        location => Location,
        temperature => format_float(maps:get(temperature, NewState)),
        humidity => format_float(maps:get(humidity, NewState)),
        light => format_float(maps:get(light, NewState)),
        timestamp => erlang:system_time(second),
        unit_temp => <<"celsius">>,
        unit_humidity => <<"percent">>,
        unit_light => <<"lux">>
    },

    %% Publish to sensor.reading topic
    Topic = <<"sensor.reading">>,
    case macula_client:publish(Client, Topic, Reading) of
        ok ->
            io:format("[~s] Published: ~.1f°C, ~.1f%%, ~.0f lux~n",
                     [SensorId,
                      maps:get(temperature, NewState),
                      maps:get(humidity, NewState),
                      maps:get(light, NewState)]);
        {error, Error} ->
            io:format("[~s] Publish error: ~p~n", [SensorId, Error])
    end,

    %% Wait 2 seconds before next reading
    timer:sleep(2000),
    publish_loop(Client, SensorId, Location, NewState).

%% @doc Update readings with small random variations to simulate real sensors
update_readings(State) ->
    #{
        temperature => update_value(maps:get(temperature, State), 0.5, 15.0, 30.0),
        humidity => update_value(maps:get(humidity, State), 2.0, 30.0, 70.0),
        light => update_value(maps:get(light, State), 20.0, 100.0, 800.0)
    }.

%% @doc Update a single value with random walk, bounded by min/max
update_value(Current, MaxChange, Min, Max) ->
    %% Random change between -MaxChange and +MaxChange
    Change = (rand:uniform() * 2.0 - 1.0) * MaxChange,
    NewValue = Current + Change,

    %% Clamp to min/max bounds
    clamp(NewValue, Min, Max).

%% @doc Clamp value between min and max
clamp(Value, Min, _Max) when Value < Min -> Min;
clamp(Value, _Min, Max) when Value > Max -> Max;
clamp(Value, _Min, _Max) -> Value.

%% @doc Format float to 1 decimal place
format_float(Value) ->
    list_to_binary(io_lib:format("~.1f", [Value])).
