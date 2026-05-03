# macula-net benchmarks

Substrate-only latency benchmarks for the macula-net L3 substrate.
All measurements are in-process: no real network RTT, no QUIC
handshake, no DHT round-trips. Numbers represent the substrate's
floor contribution; end-to-end latency adds whatever the operator's
RTT/DHT stack contributes on top.

See [PLAN_MACULA_NET_PHASE4_6_BENCHMARKS.md](https://codeberg.org/macula-internal/macula-architecture/src/branch/main/plans/PLAN_MACULA_NET_PHASE4_6_BENCHMARKS.md)
for scope rationale.

## Spec targets

| Target | Spec value | Substrate-only translation |
|--------|------------|----------------------------|
| Cold-start latency | < 200 ms (end-to-end) | not measured here — operator/network number, validated in Phase 4.7 soak |
| Warm hit latency | < 5 ms above QUIC RTT | substrate p99 ≤ 5 ms |
| Cache invalidation latency | < 10 s | TTL eviction observed within (TtlSec + 250 ms sweep period) |

## Latest measured (2026-05-03, workstation)

| Benchmark | Iterations | p50 (µs) | p95 (µs) | p99 (µs) | Margin vs target |
|-----------|-----------:|---------:|---------:|---------:|------------------|
| `warm_hit` | 1000 | 4 | 6 | 8 | p99 ≈ 0.16% of 5 ms target |
| `invalidation` | 1 | 2,040,057 | — | — | TTL=2s, eviction within ~50 ms of expiry → 24% of 10 s target if TTL were 8 s |

`warm_hit` measures the cost of `macula_route_packet:dispatch/1`
through static-mode lookup + envelope encapsulation + a no-op send
callback. End-to-end p99 of 8 µs covers: IPv6 header parse, ETS
lookup, CBOR encode, function dispatch.

`invalidation` measures end-to-end TTL eviction: insert with
TtlSec, poll until lookup reports gone. Eviction lag above the
configured TTL is bounded by `cache_route`'s sweep period
(default 30 s in production, 250 ms in the bench).

## How to reproduce

```
rebar3 eunit -m macula_net_bench_tests
```

For raw numbers without eunit's relaxed thresholds:

```
rebar3 as test shell --eval 'macula_net_bench:all(), halt(0).'
```

## Hardware / environment

- Workstation: x86-64 Linux 6.19, BEAM 26 + Quinn NIF (Rust 1.90)
- All benches single-threaded (eunit default)
- No CPU pinning, no scheduler tuning

Re-run on production-class hardware (beam02 fleet) to land harder
numbers; this MD just establishes the substrate baseline isn't
absurd.

## Out of scope

- End-to-end cold-start latency: requires a real DHT cluster +
  QUIC handshake. Validated indirectly in Phase 4.7 soak by
  observing real-traffic p99 latency.
- Memory benchmarks: latency only for MVP.
- Performance regression detection: would need CI baseline
  + delta tracking. Manually refresh this document on each
  release; automated regression tracking is a follow-up.
