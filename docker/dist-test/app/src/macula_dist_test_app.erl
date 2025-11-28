%%%-------------------------------------------------------------------
%%% @doc Macula QUIC Distribution Test Application.
%%%
%%% This application tests QUIC-based Erlang distribution by:
%%%   - Starting with macula_dist as the distribution carrier
%%%   - Automatically connecting to other cluster nodes
%%%   - Reporting cluster status via health endpoints
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_test_app).

-behaviour(application).

-export([start/2, stop/1]).

%%====================================================================
%% Application callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
    io:format("~n"),
    io:format("==================================================~n"),
    io:format("  Macula QUIC Distribution Test Node~n"),
    io:format("==================================================~n"),
    io:format("  Node:       ~p~n", [node()]),
    io:format("  Protocol:   ~p~n", [init:get_argument(proto_dist)]),
    io:format("  Cookie:     ~p~n", [erlang:get_cookie()]),
    io:format("==================================================~n"),
    io:format("~n"),

    macula_dist_test_sup:start_link().

stop(_State) ->
    io:format("[App] Stopping macula_dist_test~n"),
    ok.
