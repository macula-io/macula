#!/usr/bin/env escript

-mode(compile).

main(_Args) ->
    %% Add all library paths
    [code:add_pathz(P) || P <- filelib:wildcard("/macula/_build/default/lib/*/ebin")],

    io:format("~n==> Starting Publisher node...~n"),

    %% Get configuration from environment
    RegistryUrl = list_to_binary(os:getenv("REGISTRY_URL", "https://registry.macula.test:9443")),
    NodeId = list_to_binary(os:getenv("NODE_NAME", "publisher")),

    io:format("~n==> Connecting to registry: ~s~n", [RegistryUrl]),
    {ok, Client} = macula_connection:start_link(RegistryUrl, #{
        realm => <<"com.example.realm">>,
        node_id => NodeId,
        capabilities => [pubsub],
        start_gateway => false
    }),

    %% Wait for connection
    timer:sleep(2000),

    io:format("~n==> Publishing messages to topic test.events~n~n"),

    Topic = <<"test.events">>,

    %% Publish messages in a loop
    publish_loop(Client, Topic, 1).

publish_loop(Client, Topic, Counter) ->
    Msg = #{
        event => <<"sensor.reading">>,
        value => 20 + (Counter rem 30),
        sensor_id => <<"temp1">>,
        timestamp => erlang:system_time(second),
        sequence => Counter
    },

    io:format("[~s] Publishing message #~p: ~p~n",
              [calendar:system_time_to_rfc3339(erlang:system_time(second)), Counter, Msg]),

    case macula_connection:publish(Client, Topic, Msg) of
        ok ->
            io:format("  -> Published successfully~n");
        {error, Reason} ->
            io:format("  -> ERROR: ~p~n", [Reason])
    end,

    timer:sleep(3000),  %% Publish every 3 seconds
    publish_loop(Client, Topic, Counter + 1).
