#!/usr/bin/env escript
%%% Wildcard Pub/Sub Test - Publisher
%%% Publishes messages to various topics to test wildcard matching

-mode(compile).

main(_) ->
    %% Add all library paths
    [code:add_pathz(P) || P <- filelib:wildcard("/macula/_build/default/lib/*/ebin")],

    io:format("~n==> Starting Wildcard Publisher Test~n"),

    %% Get environment
    RegistryUrl = list_to_binary(os:getenv("REGISTRY_URL", "https://registry.macula.test:9443")),
    RealmId = list_to_binary(os:getenv("REALM_ID", "com.example.realm")),

    io:format("    Registry: ~s~n", [RegistryUrl]),
    io:format("    Realm: ~s~n", [RealmId]),

    %%  Start macula without gateway (client-only mode)
    application:set_env(macula, start_gateway, false),
    application:ensure_all_started(macula),

    %% Connect to registry
    io:format("~n==> Connecting to registry...~n"),
    ConnOpts = #{
        realm => RealmId,
        node_id => <<"publisher">>
    },

    {ok, Conn} = macula_connection:start_link(RegistryUrl, ConnOpts),
    io:format("    Connected!~n"),

    %% Wait for subscribers to be ready
    io:format("~n==> Waiting for subscribers to connect...~n"),
    timer:sleep(5000),

    %% Test Plan (dot-notation):
    %% Topic                      | subscriber1 (sensor.temp.room1) | subscriber2 (sensor.*.room1) | subscriber3 (sensor.**)
    %% --------------------------|----------------------------------|------------------------------|-------------------
    %% sensor.temp.room1         | ✓                                | ✓                            | ✓
    %% sensor.humidity.room1     | ✗                                | ✓                            | ✓
    %% sensor.temp.room2         | ✗                                | ✗                            | ✓
    %% other.topic               | ✗                                | ✗                            | ✗

    io:format("~n==> TEST 1: Publish to sensor.temp.room1 (should match all 3 subscribers)~n"),
    macula_connection:publish(Conn, <<"sensor.temp.room1">>, #{
        value => 23.5,
        unit => <<"celsius">>,
        test => <<"test1">>
    }),
    timer:sleep(2000),

    io:format("~n==> TEST 2: Publish to sensor.humidity.room1 (should match subscribers 2 and 3 only)~n"),
    macula_connection:publish(Conn, <<"sensor.humidity.room1">>, #{
        value => 65,
        unit => <<"%">>,
        test => <<"test2">>
    }),
    timer:sleep(2000),

    io:format("~n==> TEST 3: Publish to sensor.temp.room2 (should match subscriber 3 only)~n"),
    macula_connection:publish(Conn, <<"sensor.temp.room2">>, #{
        value => 21.0,
        unit => <<"celsius">>,
        test => <<"test3">>
    }),
    timer:sleep(2000),

    io:format("~n==> TEST 4: Publish to other.topic (should match no subscribers)~n"),
    macula_connection:publish(Conn, <<"other.topic">>, #{
        value => <<"test">>,
        test => <<"test4">>
    }),
    timer:sleep(2000),

    io:format("~n==> TEST 5: Publish to sensor.temp.room1.extra (should match subscriber 3 only - multi-level wildcard)~n"),
    macula_connection:publish(Conn, <<"sensor.temp.room1.extra">>, #{
        value => 24.0,
        unit => <<"celsius">>,
        test => <<"test5">>
    }),
    timer:sleep(2000),

    io:format("~n==> All messages published! Waiting for processing...~n"),
    timer:sleep(10000),

    io:format("~n==> Publisher test complete!~n"),
    init:stop().
