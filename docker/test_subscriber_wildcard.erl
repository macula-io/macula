#!/usr/bin/env escript
%%% Wildcard Pub/Sub Test - Subscriber
%%% Subscribes with different topic patterns

-mode(compile).

main([Topic, SubType]) ->
    %% Add all library paths
    [code:add_pathz(P) || P <- filelib:wildcard("/macula/_build/default/lib/*/ebin")],

    io:format("~n==> Starting Wildcard Subscriber Test (~s)~n", [SubType]),

    %% Get environment
    RegistryUrl = list_to_binary(os:getenv("REGISTRY_URL", "https://registry.macula.test:9443")),
    RealmId = list_to_binary(os:getenv("REALM_ID", "com.example.realm")),
    NodeName = list_to_binary(os:getenv("NODE_NAME", "subscriber")),

    io:format("    Registry: ~s~n", [RegistryUrl]),
    io:format("    Realm: ~s~n", [RealmId]),
    io:format("    Topic pattern: ~s~n", [Topic]),

    %% Start macula without gateway (client-only mode)
    application:set_env(macula, start_gateway, false),
    application:ensure_all_started(macula),

    %% Connect to registry
    io:format("~n==> Connecting to registry...~n"),
    ConnOpts = #{
        realm => RealmId,
        node_id => NodeName
    },

    {ok, Conn} = macula_connection:start_link(RegistryUrl, ConnOpts),
    io:format("    Connected!~n"),

    %% Subscribe to topic pattern
    io:format("~n==> Subscribing to: ~s~n", [Topic]),
    TopicBin = list_to_binary(Topic),

    Callback = fun(Msg) ->
        TestNum = maps:get(<<"test">>, Msg, <<"unknown">>),
        Value = maps:get(<<"value">>, Msg, <<"N/A">>),
        io:format("~n[~s / ~s] RECEIVED: test=~s, value=~p, full_msg=~p~n",
                  [NodeName, SubType, TestNum, Value, Msg])
    end,

    {ok, _SubRef} = macula_connection:subscribe(Conn, TopicBin, Callback),
    io:format("    Subscribed!~n"),

    io:format("~n==> Waiting for messages...~n"),

    %% Keep alive for test duration
    receive
        after 60000 -> ok
    end,

    io:format("~n==> Subscriber (~s) test complete!~n", [SubType]),
    init:stop();

main(_) ->
    io:format("Usage: test_subscriber_wildcard.erl <topic> <type>~n"),
    halt(1).
