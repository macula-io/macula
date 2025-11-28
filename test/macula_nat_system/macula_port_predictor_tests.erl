%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_port_predictor module.
%%% Tests port prediction algorithms for different NAT allocation policies
%%% and historical data tracking.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_port_predictor_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup/Teardown
%%%===================================================================

%% @doc Fixture to start and stop the port predictor for each test
port_predictor_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun predict_pp_with_base_port_test/1,
         fun predict_pp_with_history_test/1,
         fun predict_pc_with_delta_test/1,
         fun predict_rd_with_stats_test/1,
         fun predict_unknown_fallback_test/1,
         fun record_port_builds_history_test/1,
         fun record_port_calculates_deltas_test/1,
         fun get_history_returns_stored_data_test/1,
         fun get_stats_calculates_correctly_test/1,
         fun clear_removes_history_test/1,
         fun confidence_varies_by_policy_test/1,
         fun history_size_limit_enforced_test/1
     ]}.

setup() ->
    %% Start the port predictor server
    {ok, Pid} = macula_port_predictor:start_link(#{
        port_history_size => 5,
        port_prediction_count => 5
    }),
    Pid.

cleanup(Pid) ->
    %% Stop the port predictor server
    gen_server:stop(Pid).

%%%===================================================================
%%% Test Cases
%%%===================================================================

predict_pp_with_base_port_test(_Pid) ->
    fun() ->
        NodeId = <<"node-pp-001">>,

        %% Predict for PP (Port Preservation) with known base port
        Prediction = macula_port_predictor:predict(NodeId, pp, #{base_port => 50000}),

        %% Should return the base port with high confidence
        ?assertEqual([50000], maps:get(ports, Prediction)),
        ?assertEqual(pp, maps:get(strategy, Prediction)),
        ?assert(maps:get(confidence, Prediction) >= 0.9)
    end.

predict_pp_with_history_test(_Pid) ->
    fun() ->
        NodeId = <<"node-pp-002">>,

        %% Record some port history
        macula_port_predictor:record_port(NodeId, 40000, pp),
        timer:sleep(10),

        %% Predict without base port - should use history
        Prediction = macula_port_predictor:predict(NodeId, pp),

        %% Should return the last recorded port
        ?assertEqual([40000], maps:get(ports, Prediction)),
        ?assertEqual(pp, maps:get(strategy, Prediction))
    end.

predict_pc_with_delta_test(_Pid) ->
    fun() ->
        NodeId = <<"node-pc-001">>,

        %% Record sequential ports to establish delta pattern
        %% Delta of +10 between each port
        macula_port_predictor:record_port(NodeId, 50000, pc),
        timer:sleep(5),
        macula_port_predictor:record_port(NodeId, 50010, pc),
        timer:sleep(5),
        macula_port_predictor:record_port(NodeId, 50020, pc),
        timer:sleep(5),
        macula_port_predictor:record_port(NodeId, 50030, pc),
        timer:sleep(10),

        %% Predict next ports
        Prediction = macula_port_predictor:predict(NodeId, pc, #{base_port => 50030, count => 5}),

        %% Should use pc strategy and predict around the base port
        ?assertEqual(pc, maps:get(strategy, Prediction)),
        Ports = maps:get(ports, Prediction),
        ?assert(length(Ports) >= 3),

        %% The delta should be included in the prediction
        ?assert(maps:is_key(delta, Prediction))
    end.

predict_rd_with_stats_test(_Pid) ->
    fun() ->
        NodeId = <<"node-rd-001">>,

        %% Record random-ish ports
        macula_port_predictor:record_port(NodeId, 45000, rd),
        timer:sleep(5),
        macula_port_predictor:record_port(NodeId, 48000, rd),
        timer:sleep(5),
        macula_port_predictor:record_port(NodeId, 46000, rd),
        timer:sleep(5),
        macula_port_predictor:record_port(NodeId, 47000, rd),
        timer:sleep(10),

        %% Predict ports
        Prediction = macula_port_predictor:predict(NodeId, rd, #{count => 5}),

        %% Should use rd strategy and include stats
        ?assertEqual(rd, maps:get(strategy, Prediction)),
        ?assert(maps:is_key(stats, Prediction)),

        %% Stats should have mean, stddev, min, max
        Stats = maps:get(stats, Prediction),
        ?assert(maps:is_key(mean, Stats)),
        ?assert(maps:is_key(stddev, Stats)),
        ?assert(maps:is_key(min, Stats)),
        ?assert(maps:is_key(max, Stats))
    end.

predict_unknown_fallback_test(_Pid) ->
    fun() ->
        NodeId = <<"node-unknown-001">>,

        %% Predict with unknown policy and no history
        Prediction = macula_port_predictor:predict(NodeId, unknown),

        %% Should fall back to common NAT range
        ?assertEqual(fallback, maps:get(strategy, Prediction)),
        ?assert(maps:get(confidence, Prediction) =< 0.5),

        %% Should return multiple ports
        Ports = maps:get(ports, Prediction),
        ?assert(length(Ports) >= 3)
    end.

record_port_builds_history_test(_Pid) ->
    fun() ->
        NodeId = <<"node-history-001">>,

        %% Record multiple ports
        macula_port_predictor:record_port(NodeId, 30000, pp),
        timer:sleep(5),
        macula_port_predictor:record_port(NodeId, 30001, pp),
        timer:sleep(5),
        macula_port_predictor:record_port(NodeId, 30002, pp),
        timer:sleep(10),

        %% Get history
        {ok, History} = macula_port_predictor:get_history(NodeId),

        %% Should have the ports in reverse order (most recent first)
        Ports = maps:get(ports, History),
        ?assertEqual(3, length(Ports)),
        ?assertEqual(30002, hd(Ports))
    end.

record_port_calculates_deltas_test(_Pid) ->
    fun() ->
        NodeId = <<"node-delta-001">>,

        %% Record ports with known delta
        macula_port_predictor:record_port(NodeId, 20000, pc),
        timer:sleep(5),
        macula_port_predictor:record_port(NodeId, 20005, pc),  % delta = +5
        timer:sleep(5),
        macula_port_predictor:record_port(NodeId, 20010, pc),  % delta = +5
        timer:sleep(10),

        %% Get history
        {ok, History} = macula_port_predictor:get_history(NodeId),

        %% Should have calculated deltas
        Deltas = maps:get(deltas, History),
        ?assertEqual(2, length(Deltas)),
        ?assertEqual(5, hd(Deltas))  % Most recent delta
    end.

get_history_returns_stored_data_test(_Pid) ->
    fun() ->
        NodeId = <<"node-get-history-001">>,

        %% Initially no history
        ?assertEqual(not_found, macula_port_predictor:get_history(NodeId)),

        %% Record a port
        macula_port_predictor:record_port(NodeId, 25000, pp),
        timer:sleep(10),

        %% Now should have history
        {ok, History} = macula_port_predictor:get_history(NodeId),
        ?assertEqual(NodeId, maps:get(node_id, History)),
        ?assert(maps:is_key(updated_at, History))
    end.

get_stats_calculates_correctly_test(_Pid) ->
    fun() ->
        NodeId = <<"node-stats-001">>,

        %% Record some ports: 100, 200, 300 -> mean = 200
        macula_port_predictor:record_port(NodeId, 10000, rd),
        timer:sleep(5),
        macula_port_predictor:record_port(NodeId, 10200, rd),
        timer:sleep(5),
        macula_port_predictor:record_port(NodeId, 10400, rd),
        timer:sleep(10),

        %% Get stats
        {ok, Stats} = macula_port_predictor:get_stats(NodeId),

        %% Mean should be 10200
        Mean = maps:get(mean, Stats),
        ?assert(abs(Mean - 10200.0) < 1.0),

        %% Min and max
        ?assertEqual(10000, maps:get(min, Stats)),
        ?assertEqual(10400, maps:get(max, Stats)),
        ?assertEqual(3, maps:get(count, Stats))
    end.

clear_removes_history_test(_Pid) ->
    fun() ->
        NodeId = <<"node-clear-001">>,

        %% Record a port
        macula_port_predictor:record_port(NodeId, 35000, pp),
        timer:sleep(10),

        %% Verify it exists
        ?assertMatch({ok, _}, macula_port_predictor:get_history(NodeId)),

        %% Clear it
        macula_port_predictor:clear(NodeId),
        timer:sleep(10),

        %% Should be gone
        ?assertEqual(not_found, macula_port_predictor:get_history(NodeId))
    end.

confidence_varies_by_policy_test(_Pid) ->
    fun() ->
        %% PP should have highest confidence
        PredPP = macula_port_predictor:predict(<<"conf-pp">>, pp, #{base_port => 1000}),
        ConfPP = maps:get(confidence, PredPP),

        %% RD should have lowest confidence
        PredRD = macula_port_predictor:predict(<<"conf-rd">>, rd),
        ConfRD = maps:get(confidence, PredRD),

        %% PP confidence should be higher than RD
        ?assert(ConfPP > ConfRD)
    end.

history_size_limit_enforced_test(_Pid) ->
    fun() ->
        NodeId = <<"node-limit-001">>,

        %% Record more than max history size (5)
        lists:foreach(
            fun(I) ->
                macula_port_predictor:record_port(NodeId, 60000 + I, pc),
                timer:sleep(5)
            end,
            lists:seq(1, 10)
        ),
        timer:sleep(10),

        %% Get history
        {ok, History} = macula_port_predictor:get_history(NodeId),

        %% Should be limited to 5 entries
        Ports = maps:get(ports, History),
        ?assertEqual(5, length(Ports)),

        %% Most recent port should be 60010
        ?assertEqual(60010, hd(Ports))
    end.
