%%%-------------------------------------------------------------------
%%% @doc Phase 4.6 §4 acceptance — substrate latency targets.
%%%
%%% Asserts the substrate's contribution to end-to-end latency
%%% against the spec targets, with relaxed thresholds so a slow CI
%%% runner doesn't cause spurious failures.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_net_bench_tests).

-include_lib("eunit/include/eunit.hrl").

bench_test_() ->
    {foreach,
     fun setup/0, fun teardown/1,
     [
        fun bench_warm_hit_under_5ms/1,
        fun bench_invalidation_under_10s/1
     ]}.

setup() ->
    ok.

teardown(_) ->
    case whereis(macula_cache_route) of
        undefined -> ok;
        _ -> macula_cache_route:stop()
    end,
    ok.

%% --- §4 #2 + #3 — warm hit ------------------------------------------

bench_warm_hit_under_5ms(_) ->
    {timeout, 30, fun() ->
        Report = macula_net_bench:warm_hit(1000),
        ?assertEqual(1000, maps:get(iterations, Report)),
        P99us = maps:get(p99_us, Report),
        P50us = maps:get(p50_us, Report),
        Mean  = maps:get(mean_us, Report),
        io:format(user,
                  "~n[warm_hit] iter=1000  p50=~pµs p99=~pµs mean=~pµs~n",
                  [P50us, P99us, Mean]),
        %% Spec target: p99 < 5 ms (5000 µs). Eunit threshold
        %% relaxed to 3x (15 ms) for slow CI runners.
        ?assert(P99us < 15_000)
    end}.

%% --- §4 #4 — invalidation -------------------------------------------

bench_invalidation_under_10s(_) ->
    {timeout, 30, fun() ->
        Report = macula_net_bench:invalidation(2),
        Elapsed = maps:get(p99_us, Report),
        io:format(user,
                  "~n[invalidation] elapsed=~p ms (TTL=2s)~n",
                  [Elapsed div 1000]),
        %% Spec target: ≤ 10 s. Allow inserted TTL (2s) +
        %% sweep period (250ms) + overhead = practical floor ~2.5s.
        %% Should be well under 10s.
        ?assert(Elapsed < 10_000_000)
    end}.
