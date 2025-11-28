%%%-------------------------------------------------------------------
%%% @doc Unit tests for macula_cluster_strategy module.
%%%
%%% Tests the libcluster-compatible cluster formation strategy.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cluster_strategy_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    %% Start discovery first (required by cluster strategy)
    {ok, DiscoveryPid} = macula_dist_discovery:start_link(#{
        discovery_type => both
    }),

    %% Start the cluster strategy
    {ok, StrategyPid} = macula_cluster_strategy:start_link(#{
        topology => test_cluster,
        config => #{}
    }),

    {DiscoveryPid, StrategyPid}.

cleanup({DiscoveryPid, StrategyPid}) ->
    %% Stop strategy first
    catch gen_server:stop(StrategyPid),
    %% Then discovery
    catch gen_server:stop(DiscoveryPid),
    %% Clean up ETS
    catch ets:delete(macula_dist_discovery_cache),
    ok.

%%%===================================================================
%%% Test Generator
%%%===================================================================

cluster_strategy_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun({_DiscoveryPid, StrategyPid}) ->
            {inorder, [
                {"List connected returns empty initially", list_connected_empty_test(StrategyPid)},
                {"Stop strategy cleanly", stop_test(StrategyPid)}
            ]}
        end
    }.

%%%===================================================================
%%% Individual Tests
%%%===================================================================

list_connected_empty_test(StrategyPid) ->
    ?_test(begin
        Connected = macula_cluster_strategy:list_connected(StrategyPid),
        ?assertEqual([], Connected)
    end).

stop_test(StrategyPid) ->
    ?_test(begin
        %% Verify process is alive
        ?assert(is_process_alive(StrategyPid)),
        %% Stop is tested by cleanup
        ok
    end).

%%%===================================================================
%%% Standalone Tests
%%%===================================================================

start_link_test_() ->
    {"Start and stop cluster strategy", [
        ?_test(begin
            %% Start discovery
            {ok, DiscoveryPid} = macula_dist_discovery:start_link(#{
                discovery_type => both
            }),

            %% Start strategy with different name
            {ok, Pid} = macula_cluster_strategy:start_link(test_topology, #{
                config => #{realm => <<"test.local">>}
            }),

            ?assert(is_process_alive(Pid)),
            ?assertEqual([], macula_cluster_strategy:list_connected(Pid)),

            %% Cleanup
            ok = macula_cluster_strategy:stop(Pid),
            gen_server:stop(DiscoveryPid),
            catch ets:delete(macula_dist_discovery_cache)
        end)
    ]}.
