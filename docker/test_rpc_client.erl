#!/usr/bin/env escript
%%% RPC Client Test
%%% Makes multiple RPC calls to test service discovery, round-robin, and failover

-mode(compile).

main(_) ->
    %% Add all library paths
    [code:add_pathz(P) || P <- filelib:wildcard("/macula/_build/default/lib/*/ebin")],

    io:format("~n==> Starting RPC Client Test~n"),

    %% Get environment
    RegistryUrl = list_to_binary(os:getenv("REGISTRY_URL", "https://registry.macula.test:9443")),
    RealmId = list_to_binary(os:getenv("REALM_ID", "com.example.realm")),

    io:format("    Registry: ~s~n", [RegistryUrl]),
    io:format("    Realm: ~s~n", [RealmId]),

    %% Start macula without gateway (client-only mode)
    application:set_env(macula, start_gateway, false),
    application:ensure_all_started(macula),

    %% Connect to registry
    io:format("~n==> Connecting to registry...~n"),
    ConnOpts = #{
        realm => RealmId,
        node_id => <<"client">>
    },

    {ok, Conn} = macula:connect(RegistryUrl, ConnOpts),
    io:format("    Connected!~n"),

    %% Wait for providers to advertise
    io:format("~n==> Waiting for providers to advertise services...~n"),
    timer:sleep(8000),

    %% Test Plan: 6 RPC calls to test round-robin distribution
    io:format("~n==> TEST 1: Round-robin distribution (6 calls)~n~n"),

    lists:foreach(
        fun(N) ->
            io:format("Call #~p:~n", [N]),
            Args = #{x => N},

            case macula:call(Conn, <<"test.calculator">>, Args, #{timeout => 5000}) of
                {ok, Result} ->
                    Provider = maps:get(<<"provider">>, Result, <<"unknown">>),
                    Value = maps:get(<<"result">>, Result, 0),
                    Timestamp = maps:get(<<"timestamp">>, Result, 0),
                    io:format("  Success: provider=~s, result=~p, timestamp=~p~n",
                             [Provider, Value, Timestamp]),
                    timer:sleep(1000);
                {error, Reason} ->
                    io:format("  Error: ~p~n", [Reason]),
                    timer:sleep(1000)
            end
        end,
        lists:seq(1, 6)
    ),

    io:format("~n==> RPC client test complete!~n"),
    timer:sleep(2000),
    init:stop().
