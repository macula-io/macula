%%%-------------------------------------------------------------------
%%% @doc Tests for the discovery strategies that cannot run.
%%%
%%% DHT discovery needs a DHT module that no macula release provides, so
%%% macula_cluster:start_cluster/1 with the dht strategy, and a discovery
%%% server started with discovery_type dht or both, return
%%% {error, {strategy_unavailable, dht}}. The mdns strategy needs a running
%%% discovery server, so start_cluster/1 with mdns returns
%%% {error, {strategy_unavailable, mdns}} when none runs. A refused
%%% start_cluster/1 leaves the node undistributed, and a discovery server
%%% started with discovery_type mdns still starts. Each scenario runs in a
%%% peer node of its own.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_discovery_unavailable_tests).

-include_lib("eunit/include/eunit.hrl").

%% Scenarios, run in a peer node.
-export([start_cluster_with/1,
         start_discovery_with/1]).

-define(SCENARIO_TIMEOUT_MS, 30_000).

discovery_unavailable_test_() ->
    [{"start_cluster/1 with dht returns strategy_unavailable and starts no distribution",
      {timeout, 60, fun cluster_refuses_dht/0}},
     {"start_cluster/1 with mdns and no discovery server returns strategy_unavailable",
      {timeout, 60, fun cluster_refuses_mdns_without_discovery/0}},
     {"a discovery server started with dht returns strategy_unavailable",
      {timeout, 60, fun discovery_refuses_dht/0}},
     {"a discovery server started with both returns strategy_unavailable",
      {timeout, 60, fun discovery_refuses_both/0}},
     {"a discovery server started with mdns starts",
      {timeout, 60, fun discovery_starts_with_mdns/0}}].

cluster_refuses_dht() ->
    ?assertEqual({ok, {{error, {strategy_unavailable, dht}}, not_distributed}},
                 in_peer(start_cluster_with, [dht])).

cluster_refuses_mdns_without_discovery() ->
    ?assertEqual({ok, {{error, {strategy_unavailable, mdns}}, not_distributed}},
                 in_peer(start_cluster_with, [mdns])).

discovery_refuses_dht() ->
    ?assertEqual({ok, {error, {strategy_unavailable, dht}}},
                 in_peer(start_discovery_with, [dht])).

discovery_refuses_both() ->
    ?assertEqual({ok, {error, {strategy_unavailable, dht}}},
                 in_peer(start_discovery_with, [both])).

discovery_starts_with_mdns() ->
    ?assertEqual({ok, started}, in_peer(start_discovery_with, [mdns])).

%%%===================================================================
%%% Scenarios
%%%===================================================================

%% The start_cluster/1 result, or the exit it raised, and whether the node
%% was distributed afterwards.
start_cluster_with(Strategy) ->
    process_flag(trap_exit, true),
    Result = try
                 macula_cluster:start_cluster(#{strategy => Strategy})
             catch
                 Class:Reason -> {raised, Class, Reason}
             end,
    {Result, distributed(macula_cluster:is_distributed())}.

%% The discovery server's start result, and for a server that started,
%% what registering a node answers.
start_discovery_with(DiscoveryType) ->
    process_flag(trap_exit, true),
    started(macula_dist_discovery:start_link(#{discovery_type => DiscoveryType})).

started({ok, _Pid}) ->
    started;
started(Other) ->
    Other.

distributed(true) -> distributed;
distributed(false) -> not_distributed.

%%%===================================================================
%%% Peer node
%%%===================================================================

%% The peer gets its own cookie, so a start_cluster/1 that starts
%% distribution never reads or writes the user's ~/.erlang.cookie.
in_peer(Scenario, Args) ->
    Started = peer:start_link(#{connection => standard_io,
                                args => ["-pa" | code:get_path()],
                                env => [{"MACULA_COOKIE", "macula_discovery_unavailable_tests"}]}),
    Peer = element(2, Started),
    OsPid = peer:call(Peer, os, getpid, [], 5_000),
    try peer:call(Peer, ?MODULE, Scenario, Args, ?SCENARIO_TIMEOUT_MS) of
        Result -> {ok, Result}
    catch
        Class:Reason -> {error, {Class, Reason}}
    after
        _ = os:cmd("kill -9 " ++ OsPid),
        try peer:stop(Peer) catch _:_ -> ok end
    end.
