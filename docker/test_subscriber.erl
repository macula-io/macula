#!/usr/bin/env escript

-mode(compile).

main(_Args) ->
    %% Add all library paths
    [code:add_pathz(P) || P <- filelib:wildcard("/macula/_build/default/lib/*/ebin")],

    io:format("~n==> Starting Subscriber node...~n"),

    %% Get configuration from environment
    RegistryUrl = list_to_binary(os:getenv("REGISTRY_URL", "https://registry.macula.test:9443")),
    NodeId = list_to_binary(os:getenv("NODE_NAME", "subscriber")),

    %% Get node hostname for gateway endpoint
    NodeHost = list_to_binary(os:getenv("NODE_HOST", "subscriber.macula.test")),
    GatewayPort = list_to_integer(os:getenv("GATEWAY_PORT", "9443")),
    SubscriberEndpoint = <<"https://", NodeHost/binary, ":", (integer_to_binary(GatewayPort))/binary>>,

    io:format("~n==> Connecting to registry: ~s~n", [RegistryUrl]),
    io:format("~n==> Subscriber endpoint: ~s~n", [SubscriberEndpoint]),

    {ok, Client} = macula_connection:start_link(RegistryUrl, #{
        realm => <<"com.example.realm">>,
        node_id => NodeId,
        capabilities => [pubsub],
        start_gateway => true,
        gateway_port => GatewayPort,
        url => SubscriberEndpoint
    }),

    %% Wait for connection
    timer:sleep(2000),

    io:format("~n==> Subscribing to topic test.events~n~n"),

    %% Create subscriber callback
    Callback = fun(Msg) ->
        io:format("[~s] [~s] Received message: ~p~n",
                  [calendar:system_time_to_rfc3339(erlang:system_time(second)), NodeId, Msg]),
        ok
    end,

    Topic = <<"test.events">>,
    {ok, SubRef} = macula_connection:subscribe(Client, Topic, Callback),
    io:format("[~s] Subscribed to topic: ~s (ref: ~p)~n", [NodeId, Topic, SubRef]),

    io:format("~n==> Listening for messages (press Ctrl+C to stop)...~n~n"),

    %% Keep process alive
    receive
        stop -> ok
    end.
