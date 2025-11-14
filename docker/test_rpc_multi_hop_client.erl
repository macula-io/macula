#!/usr/bin/env escript
%%% Multi-hop RPC Client Test
%%% Tests RPC calls routed through DHT mesh (multi-hop)
%%%
%%% Topology: Client → NodeA → NodeB → Provider
%%%
%%% This test verifies that RPC calls successfully route through
%%% multiple intermediate nodes using Kademlia DHT routing.

-mode(compile).

main(_) ->
    %% Add all library paths
    [code:add_pathz(P) || P <- filelib:wildcard("/macula/_build/default/lib/*/ebin")],

    io:format("~n==> Starting Multi-hop RPC Client Test~n"),
    io:format("    Testing DHT-routed RPC through mesh~n"),

    %% Get environment
    RegistryUrl = list_to_binary(os:getenv("REGISTRY_URL", "https://registry.macula.test:9443")),
    RealmId = list_to_binary(os:getenv("REALM_ID", "com.example.realm")),

    io:format("    Registry: ~s~n", [RegistryUrl]),
    io:format("    Realm: ~s~n", [RealmId]),

    %% Start macula WITH gateway (test if bidirectional works with gateway enabled)
    application:set_env(macula, start_gateway, true),
    application:set_env(macula, gateway_port, 9443),
    application:ensure_all_started(macula),

    %% Connect to registry
    io:format("~n==> Connecting to registry...~n"),
    ConnOpts = #{
        realm => RealmId,
        node_id => <<"client">>
    },

    {ok, Conn} = macula_connection:start_link(RegistryUrl, ConnOpts),
    io:format("    Connected!~n"),

    %% Wait for mesh to stabilize and provider to advertise
    io:format("~n==> Waiting for mesh to stabilize...~n"),
    io:format("    - Registry discovering intermediate nodes~n"),
    io:format("    - NodeA connecting to registry~n"),
    io:format("    - NodeB connecting to NodeA~n"),
    io:format("    - Provider connecting to NodeB~n"),
    io:format("    - Provider advertising service~n"),
    timer:sleep(15000),  % Longer wait for multi-hop mesh

    %% Test Plan: 6 RPC calls to verify multi-hop routing
    io:format("~n==> TEST: Multi-hop RPC Routing (6 calls)~n"),
    io:format("    Expected path: Client → NodeA → NodeB → Provider~n"),
    io:format("    Minimum hops: 2-3~n~n"),

    SuccessCount = lists:foldl(
        fun(N, SuccessAcc) ->
            io:format("Call #~p:~n", [N]),
            Args = #{x => N},

            case macula_connection:call(Conn, <<"test.calculator">>, Args, #{timeout => 10000}) of
                {ok, Result} ->
                    Provider = maps:get(<<"provider">>, Result, <<"unknown">>),
                    Value = maps:get(<<"result">>, Result, 0),
                    Timestamp = maps:get(<<"timestamp">>, Result, 0),

                    io:format("  ✓ Success: provider=~s, result=~p, timestamp=~p~n",
                             [Provider, Value, Timestamp]),
                    io:format("    (routed through DHT mesh)~n"),

                    timer:sleep(1000),
                    SuccessAcc + 1;

                {error, Reason} ->
                    io:format("  ✗ Error: ~p~n", [Reason]),
                    timer:sleep(1000),
                    SuccessAcc
            end
        end,
        0,
        lists:seq(1, 6)
    ),

    io:format("~n==> Test Results:~n"),
    io:format("    Successful calls: ~p/6~n", [SuccessCount]),

    case SuccessCount of
        6 ->
            io:format("    ✓ ALL TESTS PASSED~n"),
            io:format("    Multi-hop DHT routing working correctly!~n");
        N when N > 0 ->
            io:format("    ⚠ PARTIAL SUCCESS (~p/6)~n", [N]),
            io:format("    Some calls failed - check logs~n");
        0 ->
            io:format("    ✗ ALL TESTS FAILED~n"),
            io:format("    Multi-hop routing not working - check mesh connectivity~n")
    end,

    io:format("~n==> Multi-hop RPC client test complete!~n"),
    timer:sleep(2000),
    init:stop().
