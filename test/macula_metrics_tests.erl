%%%-------------------------------------------------------------------
%%% @doc Eunit for macula_metrics — Phase 4.1.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_metrics_tests).

-include_lib("eunit/include/eunit.hrl").

%% Each test gets its own metrics gen_server; tear down between.
metrics_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
        fun counter_increments_via_telemetry/1,
        fun counter_can_be_incremented_directly/1,
        fun gauge_set_and_read/1,
        fun histogram_observes_latency/1,
        fun reset_clears_values_keeps_meta/1,
        fun unknown_telemetry_event_is_safe/1
     ]}.

setup() ->
    {ok, _} = application:ensure_all_started(telemetry),
    {ok, Pid} = macula_metrics:start_link(#{install_default_gauges => false}),
    macula_metrics:reset_all(),
    Pid.

teardown(Pid) ->
    case is_process_alive(Pid) of
        true  -> macula_metrics:stop();
        false -> ok
    end.

counter_increments_via_telemetry(_Pid) ->
    fun() ->
        telemetry:execute([macula, net, egress, dispatched],
                          #{latency_us => 250},
                          #{kind => <<"data">>}),
        telemetry:execute([macula, net, egress, dispatched],
                          #{latency_us => 800},
                          #{kind => <<"data">>}),
        Snap = macula_metrics:gather(),
        Counter = pick(<<"macula_net_envelopes_forwarded_total">>, Snap),
        ?assertMatch(#{type := counter}, Counter),
        Egress = [V || #{labels := L, value := V} <- maps:get(samples, Counter),
                       proplists:get_value(direction, L) =:= <<"egress">>],
        ?assertEqual([2], Egress)
    end.

counter_can_be_incremented_directly(_Pid) ->
    fun() ->
        ok = macula_metrics:inc_counter(
               <<"macula_net_envelopes_forwarded_total">>,
               [{kind, <<"data">>}, {direction, <<"egress">>}], 5),
        Snap = macula_metrics:gather(),
        Counter = pick(<<"macula_net_envelopes_forwarded_total">>, Snap),
        Egress = [V || #{labels := L, value := V} <- maps:get(samples, Counter),
                       proplists:get_value(direction, L) =:= <<"egress">>],
        ?assertEqual([5], Egress)
    end.

gauge_set_and_read(_Pid) ->
    fun() ->
        ok = macula_metrics:set_gauge(<<"macula_net_attach_active">>, [], 7),
        Snap = macula_metrics:gather(),
        G = pick(<<"macula_net_attach_active">>, Snap),
        ?assertMatch(#{type := gauge,
                       samples := [#{labels := [], value := 7}]}, G),
        ok = macula_metrics:set_gauge(<<"macula_net_attach_active">>, [], 12),
        Snap2 = macula_metrics:gather(),
        G2 = pick(<<"macula_net_attach_active">>, Snap2),
        ?assertMatch(#{samples := [#{value := 12}]}, G2)
    end.

histogram_observes_latency(_Pid) ->
    fun() ->
        %% Two observations: 0.002s, 0.07s — buckets cover both.
        telemetry:execute([macula, net, resolve, complete],
                          #{latency_us => 2000},
                          #{outcome => <<"hit">>}),
        telemetry:execute([macula, net, resolve, complete],
                          #{latency_us => 70000},
                          #{outcome => <<"hit">>}),
        Snap = macula_metrics:gather(),
        H = pick(<<"macula_net_resolve_latency_seconds">>, Snap),
        ?assertMatch(#{type := histogram}, H),
        [Sample] = maps:get(samples, H),
        ?assertEqual(2, maps:get(count, Sample)),
        SumS = maps:get(sum, Sample),
        ?assert(SumS >= 0.071 andalso SumS =< 0.073),
        Buckets = maps:get(buckets, Sample),
        %% Cumulative: bucket 0.005 should hold 1, bucket 0.1 should hold 2.
        ?assertEqual(1, proplists:get_value(0.005, Buckets)),
        ?assertEqual(2, proplists:get_value(0.1,   Buckets))
    end.

reset_clears_values_keeps_meta(_Pid) ->
    fun() ->
        ok = macula_metrics:inc_counter(
               <<"macula_net_envelopes_forwarded_total">>,
               [{kind, <<"data">>}, {direction, <<"egress">>}], 3),
        ok = macula_metrics:reset_all(),
        Snap = macula_metrics:gather(),
        Counter = pick(<<"macula_net_envelopes_forwarded_total">>, Snap),
        ?assertEqual([], maps:get(samples, Counter))
    end.

unknown_telemetry_event_is_safe(_Pid) ->
    fun() ->
        %% Topic that no handler clause matches.
        telemetry:execute([macula, net, totally, made_up],
                          #{latency_us => 0}, #{}),
        ?assert(is_process_alive(whereis(macula_metrics)))
    end.

%% --- helpers --------------------------------------------------------

pick(Name, Snap) ->
    [M] = [M || #{name := N} = M <- Snap, N =:= Name],
    M.
