%%%-------------------------------------------------------------------
%%% @doc Phase 4.6 — substrate latency benchmarks.
%%%
%%% In-process measurements of macula-net hot paths. Excludes real
%%% network RTT, QUIC handshake, and DHT network jitter — those are
%%% operator-environment factors the substrate cannot control. The
%%% numbers here are the floor: end-to-end latency is `bench result
%%% + RTT-stack overhead'.
%%%
%%% Scope (deliberately narrow):
%%%
%%% <ul>
%%%   <li>{@link warm_hit/0} — cache-hit dispatch through
%%%       {@link macula_route_packet:dispatch/1}. Pure substrate
%%%       overhead per egress. Target: p99 ≤ 5 ms.</li>
%%%   <li>{@link invalidation/0} — TTL-driven cache eviction. Insert
%%%       a route with TtlSec, poll until lookup reports gone.
%%%       Target: ≤ 10 s.</li>
%%% </ul>
%%%
%%% Cold-resolve (target: end-to-end <200ms) is an operator-network
%%% number, not a substrate-code number. Validated in Phase 4.7 soak,
%%% not here.
%%%
%%% Run:
%%%
%%% ```
%%%   rebar3 as test shell --eval 'macula_net_bench:all().'
%%% '''
%%%
%%% Or eunit-asserted via {@link macula_net_bench_tests}.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_net_bench).

-export([
    all/0,
    warm_hit/0,     warm_hit/1,
    invalidation/0, invalidation/1
]).

-export_type([report/0]).

-type report() :: #{
    iterations := non_neg_integer(),
    mean_us    := float(),
    p50_us     := non_neg_integer(),
    p95_us     := non_neg_integer(),
    p99_us     := non_neg_integer(),
    max_us     := non_neg_integer()
}.

-define(OWN,    <<16#fd, 1:40, 16#01:80>>).
-define(REMOTE, <<16#fd, 1:40, 16#aa:80>>).

%% =============================================================================
%% Public entry
%% =============================================================================

%% @doc Run all benchmarks and print a markdown table.
-spec all() -> ok.
all() ->
    Warm = warm_hit(),
    Inv  = invalidation(),
    print_table([
        {<<"warm_hit">>,     Warm},
        {<<"invalidation">>, Inv}
    ]).

%% --- warm hit -------------------------------------------------------

warm_hit() -> warm_hit(1000).

%% @doc Drive N cache-hit dispatches via {@link
%% macula_route_packet:dispatch/1} in static mode. Static mode is
%% the right model here: it isolates substrate overhead from the
%% resolver path (the resolver's cost is dominated by Ed25519 verify,
%% an operator-environment number we don't control).
-spec warm_hit(pos_integer()) -> report().
warm_hit(N) when N > 0 ->
    setup_static_mode(),
    Pkt = ipv6_packet(?OWN, ?REMOTE),
    Run = fun() ->
        T0 = erlang:monotonic_time(microsecond),
        {ok, _} = macula_route_packet:dispatch(Pkt),
        erlang:monotonic_time(microsecond) - T0
    end,
    Samples = [Run() || _ <- lists:seq(1, N)],
    report(Samples).

%% --- invalidation --------------------------------------------------

invalidation() -> invalidation(2).

%% @doc Insert a cache_route entry with TtlSec TTL, poll until it's
%% gone. Returns the elapsed-time-to-eviction in microseconds.
-spec invalidation(pos_integer()) -> report().
invalidation(TtlSec) when TtlSec > 0 ->
    ensure_cache_route(),
    macula_cache_route:invalidate(?REMOTE),
    NowMs  = erlang:system_time(millisecond),
    Expiry = NowMs + (TtlSec * 1000),
    Entry  = #{station_pubkey => <<2:256>>,
               host           => <<"127.0.0.1">>,
               port           => 4400,
               expires_at     => Expiry},
    ok = macula_cache_route:insert(?REMOTE, Entry),
    T0 = erlang:monotonic_time(microsecond),
    Elapsed = wait_until_invalidated(?REMOTE, T0, 10_000),
    #{iterations => 1,
      mean_us    => float(Elapsed),
      p50_us     => Elapsed,
      p95_us     => Elapsed,
      p99_us     => Elapsed,
      max_us     => Elapsed}.

wait_until_invalidated(Addr, T0, MaxMs) ->
    case macula_cache_route:lookup(Addr) of
        {ok, _}  ->
            ElapsedMs = (erlang:monotonic_time(microsecond) - T0) div 1000,
            decide_wait(ElapsedMs >= MaxMs, Addr, T0, MaxMs);
        _Other ->
            erlang:monotonic_time(microsecond) - T0
    end.

decide_wait(true, _Addr, T0, _MaxMs) ->
    erlang:monotonic_time(microsecond) - T0;
decide_wait(false, Addr, T0, MaxMs) ->
    timer:sleep(50),
    wait_until_invalidated(Addr, T0, MaxMs).

%% =============================================================================
%% Reporting
%% =============================================================================

-spec report([non_neg_integer()]) -> report().
report(Samples) ->
    Sorted = lists:sort(Samples),
    #{iterations => length(Sorted),
      mean_us    => mean(Samples),
      p50_us     => percentile(Sorted, 50),
      p95_us     => percentile(Sorted, 95),
      p99_us     => percentile(Sorted, 99),
      max_us     => lists:last(Sorted)}.

mean([])      -> 0.0;
mean(Samples) -> lists:sum(Samples) / length(Samples).

percentile(Sorted, P) when P > 0, P =< 100 ->
    N = length(Sorted),
    Idx = max(1, (P * N + 99) div 100),
    lists:nth(min(Idx, N), Sorted).

print_table(Rows) ->
    io:format("~n| benchmark      | iter | p50 (µs) | p95 (µs) | p99 (µs) |~n"),
    io:format("|----------------|------|----------|----------|----------|~n"),
    lists:foreach(fun({Name, R}) ->
        io:format("| ~-14s | ~4w | ~8w | ~8w | ~8w |~n",
                  [Name,
                   maps:get(iterations, R),
                   maps:get(p50_us, R),
                   maps:get(p95_us, R),
                   maps:get(p99_us, R)])
    end, Rows),
    io:format("~n").

%% =============================================================================
%% Setup
%% =============================================================================

setup_static_mode() ->
    SendFn = fun(_StationId, _Cbor) -> ok end,
    Stations = [#{address => ?REMOTE, station => <<"bench-station">>,
                  send => SendFn}],
    ok = macula_route_packet:configure(
           #{own_address => ?OWN, stations => Stations}).

ensure_cache_route() ->
    case whereis(macula_cache_route) of
        undefined ->
            {ok, _} = macula_cache_route:start_link(#{sweep_ms => 250}),
            ok;
        _ -> ok
    end.

%% =============================================================================
%% Helpers
%% =============================================================================

ipv6_packet(Src, Dst) ->
    Header = <<6:4, 0:8, 0:20, 8:16, 59:8, 64:8, Src/binary, Dst/binary>>,
    <<Header/binary, 1, 2, 3, 4, 5, 6, 7, 8>>.
