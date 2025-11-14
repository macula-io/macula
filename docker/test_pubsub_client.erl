#!/usr/bin/env escript

-mode(compile).

main(_Args) ->
    %% Add all library paths
    [code:add_pathz(P) || P <- filelib:wildcard("/macula/_build/default/lib/*/ebin")],

    io:format("~n==> Starting Pub/Sub test client...~n"),

    %% Get registry URL from environment
    RegistryUrl = list_to_binary(os:getenv("REGISTRY_URL", "https://registry.macula.test:9443")),

    io:format("~n==> Connecting to registry~n"),
    {ok, Client} = macula_connection:start_link(RegistryUrl, #{
        realm => <<"com.example.realm">>,
        node_id => <<"pubsub_client">>,
        capabilities => [pubsub],
        start_gateway => false  %% Don't start gateway in embedded mode
    }),

    %% Wait for connection
    timer:sleep(2000),

    io:format("~n==> TEST 1: Subscribe to topic~n~n"),

    %% Create a subscriber that collects messages
    ReceiverPid = self(),
    Callback = fun(Msg) ->
        io:format("[Subscriber] Received message: ~p~n", [Msg]),
        ReceiverPid ! {pubsub_message, Msg},
        ok
    end,

    %% Subscribe to test topic
    Topic = <<"test.events">>,
    {ok, SubRef} = macula_connection:subscribe(Client, Topic, Callback),
    io:format("Subscribed to topic: ~s (ref: ~p)~n", [Topic, SubRef]),

    timer:sleep(1000),

    io:format("~n==> TEST 2: Publish messages~n~n"),

    %% Publish 3 messages
    Messages = [
        #{event => <<"sensor.reading">>, value => 42, sensor_id => <<"temp1">>},
        #{event => <<"sensor.reading">>, value => 38, sensor_id => <<"temp2">>},
        #{event => <<"alert">>, level => <<"warning">>, message => <<"Temperature high">>}
    ],

    lists:foreach(fun(Msg) ->
        io:format("Publishing: ~p~n", [Msg]),
        case macula_connection:publish(Client, Topic, Msg) of
            ok ->
                io:format("  -> Published successfully~n");
            {error, Reason} ->
                io:format("  -> ERROR: ~p~n", [Reason])
        end,
        timer:sleep(500)
    end, Messages),

    io:format("~n==> Waiting for messages (5 seconds)...~n"),

    %% Collect received messages
    ReceivedCount = receive_messages(0, 5000),

    io:format("~n==> TEST 3: Unsubscribe~n~n"),
    ok = macula_connection:unsubscribe(Client, SubRef),
    io:format("Unsubscribed from topic: ~s~n", [Topic]),

    timer:sleep(1000),

    io:format("~n==> All tests complete!~n"),
    io:format("    Received ~p messages~n", [ReceivedCount]),

    macula_connection:stop(Client),

    %% Exit with success
    halt(0).

receive_messages(Count, Timeout) ->
    receive
        {pubsub_message, Msg} ->
            io:format("[Main] Got message ~p: ~p~n", [Count + 1, Msg]),
            receive_messages(Count + 1, Timeout)
    after Timeout ->
        Count
    end.
