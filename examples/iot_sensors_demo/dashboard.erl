#!/usr/bin/env escript
%%% @doc
%%% Macula IoT Sensor Demo - Dashboard
%%%
%%% Subscribes to sensor readings from all IoT sensors
%%% and displays them in a real-time dashboard format.
%%%
%%% Demonstrates many-to-one pub/sub pattern where multiple
%%% sensors (behind NAT/firewalls) publish to a single dashboard.
%%%
%%% Usage:
%%%   ./dashboard.erl
%%% @end

-mode(compile).

main(_Args) ->
    io:format("~n=== Macula IoT Sensor Dashboard ===~n~n"),

    %% Start required applications
    application:ensure_all_started(crypto),
    application:ensure_all_started(ssl),
    application:ensure_all_started(macula),

    %% Connect to gateway
    io:format("[Dashboard] Connecting to Macula Gateway...~n"),
    GatewayUrl = <<"https://localhost:9443">>,
    Realm = <<"com.example.iot">>,

    case macula_client:connect(GatewayUrl, #{
        realm => Realm,
        node_id => <<"dashboard-main">>
    }) of
        {ok, Client} ->
            io:format("[Dashboard] Connected! Realm: ~s~n", [Realm]),
            io:format("[Dashboard] Subscribing to sensor.reading...~n~n"),

            %% Subscribe to sensor readings
            CallbackFun = fun(Reading) -> display_reading(Reading) end,
            case macula_client:subscribe(Client, <<"sensor.reading">>, CallbackFun) of
                {ok, _SubRef} ->
                    io:format("~n╔════════════════════════════════════════════════════════════════╗~n"),
                    io:format("║                  LIVE SENSOR DASHBOARD                         ║~n"),
                    io:format("╚════════════════════════════════════════════════════════════════╝~n~n"),
                    io:format("Listening for sensor readings... (Press Ctrl+C to exit)~n~n"),

                    %% Keep process alive
                    timer:sleep(infinity);
                {error, SubError} ->
                    io:format("[Dashboard] Subscribe error: ~p~n", [SubError]),
                    halt(1)
            end;
        {error, Reason} ->
            io:format("[Dashboard] Connection error: ~p~n", [Reason]),
            halt(1)
    end.

%% @doc Display a sensor reading in dashboard format
display_reading(Reading) ->
    SensorId = maps:get(<<"sensor_id">>, Reading, <<"unknown">>),
    Location = maps:get(<<"location">>, Reading, <<"unknown">>),
    Temp = maps:get(<<"temperature">>, Reading, <<"0.0">>),
    Humidity = maps:get(<<"humidity">>, Reading, <<"0.0">>),
    Light = maps:get(<<"light">>, Reading, <<"0.0">>),
    Timestamp = maps:get(<<"timestamp">>, Reading, 0),

    %% Format timestamp
    TimeStr = format_timestamp(Timestamp),

    %% Display with nice formatting
    io:format("┌──────────────────────────────────────────────────────────────┐~n"),
    io:format("│ ~s @ ~s~n", [pad_right(binary_to_list(SensorId), 20),
                                 pad_right(binary_to_list(Location), 30)]),
    io:format("│ Time: ~s~n", [TimeStr]),
    io:format("├──────────────────────────────────────────────────────────────┤~n"),
    io:format("│  🌡️  Temperature: ~s°C~n", [pad_right(binary_to_list(Temp), 6)]),
    io:format("│  💧  Humidity:    ~s%%~n", [pad_right(binary_to_list(Humidity), 6)]),
    io:format("│  💡  Light:       ~s lux~n", [pad_right(binary_to_list(Light), 6)]),
    io:format("└──────────────────────────────────────────────────────────────┘~n~n"),
    ok.

%% @doc Format Unix timestamp to readable string
format_timestamp(UnixSeconds) when is_integer(UnixSeconds) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:now_to_datetime({UnixSeconds div 1000000,
                                  UnixSeconds rem 1000000,
                                  0}),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                  [Year, Month, Day, Hour, Minute, Second]);
format_timestamp(_) ->
    "Invalid timestamp".

%% @doc Pad string to the right with spaces
pad_right(Str, Width) ->
    Len = length(Str),
    if
        Len >= Width -> Str;
        true -> Str ++ lists:duplicate(Width - Len, $\s)
    end.
