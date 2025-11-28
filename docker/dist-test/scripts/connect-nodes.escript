#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% Connect to other nodes in the QUIC distribution cluster
%%
%% Usage (inside container):
%%   escript /opt/macula/scripts/connect-nodes.escript
%%

-mode(compile).

main(_Args) ->
    io:format("~n=== Macula QUIC Distribution Node Connector ===~n~n"),

    %% Get our node name
    Self = node(),
    io:format("This node: ~p~n", [Self]),

    %% Define other nodes to connect to
    Nodes = [
        '4433@172.30.0.10',
        '4434@172.30.0.11',
        '4435@172.30.0.12'
    ],

    %% Filter out ourselves
    OtherNodes = [N || N <- Nodes, N =/= Self],

    io:format("Attempting to connect to: ~p~n~n", [OtherNodes]),

    %% Try to connect to each node
    Results = lists:map(
        fun(Node) ->
            io:format("Connecting to ~p... ", [Node]),
            case net_kernel:connect_node(Node) of
                true ->
                    io:format("SUCCESS~n"),
                    {Node, connected};
                false ->
                    io:format("FAILED~n"),
                    {Node, failed};
                ignored ->
                    io:format("IGNORED (distribution not started)~n"),
                    {Node, ignored}
            end
        end,
        OtherNodes
    ),

    io:format("~n=== Connection Results ===~n"),
    lists:foreach(
        fun({Node, Status}) ->
            io:format("  ~p: ~p~n", [Node, Status])
        end,
        Results
    ),

    %% Show connected nodes
    io:format("~n=== Currently Connected Nodes ===~n"),
    Connected = nodes(),
    case Connected of
        [] ->
            io:format("  (none)~n");
        _ ->
            lists:foreach(
                fun(N) -> io:format("  ~p~n", [N]) end,
                Connected
            )
    end,

    io:format("~n"),
    ok.
