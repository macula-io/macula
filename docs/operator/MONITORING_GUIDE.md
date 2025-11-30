# Macula Monitoring Guide

**Complete guide to monitoring Macula mesh deployments**

**Audience**: Operators
**Last Updated**: 2025-11-28

---

## Table of Contents

1. [Overview](#overview)
2. [Key Metrics](#key-metrics)
3. [Log Monitoring](#log-monitoring)
4. [Health Checks](#health-checks)
5. [Alerting](#alerting)
6. [Dashboards](#dashboards)
7. [Capacity Planning](#capacity-planning)

---

## Overview

Macula provides several observable metrics and log messages for production monitoring. This guide covers what to monitor, how to interpret metrics, and when to alert.

### Monitoring Philosophy

```
                    ┌─────────────────────────────────────┐
                    │         Observability Layers        │
                    └─────────────────────────────────────┘
                                      │
        ┌─────────────────────────────┼─────────────────────────────┐
        │                             │                             │
        ▼                             ▼                             ▼
┌───────────────┐           ┌─────────────────┐           ┌─────────────────┐
│    Metrics    │           │      Logs       │           │     Traces      │
│  (Numerical)  │           │   (Textual)     │           │  (Distributed)  │
├───────────────┤           ├─────────────────┤           ├─────────────────┤
│ • Pool sizes  │           │ • Cleanup events│           │ • RPC call flow │
│ • Latencies   │           │ • Rejections    │           │ • PubSub fanout │
│ • Throughput  │           │ • Errors/Warns  │           │ • DHT queries   │
│ • Memory      │           │ • State changes │           │                 │
└───────────────┘           └─────────────────┘           └─────────────────┘
```

---

## Key Metrics

### Memory Management Metrics

These metrics indicate the health of Macula's bounded resource pools.

| Metric | Module | Threshold | Action |
|--------|--------|-----------|--------|
| Connection Pool Size | `macula_gateway_mesh` | > 800 (of 1,000) | Scale horizontally |
| Client Count | `macula_gateway_client_manager` | > 8,000 (of 10,000) | Scale or rate-limit |
| Service Registry Size | `macula_service_registry` | Increasing trend | Check for stale services |
| Pending RPC Calls | `macula_rpc_handler` | > 100 sustained | Check handler latency |
| Pending DHT Queries | `macula_rpc_handler` | > 50 sustained | Check DHT health |

### Querying Metrics

#### Connection Pool (via Erlang shell)

```erlang
%% Get connection pool stats
macula_gateway_mesh:get_stats().
%% Returns: #{connections => 245, max => 1000, lru_evictions => 12}

%% Check if pool is near capacity
case macula_gateway_mesh:get_stats() of
    #{connections := C, max := Max} when C > Max * 0.8 ->
        io:format("WARNING: Pool at ~p% capacity~n", [C * 100 div Max]);
    _ ->
        ok
end.
```

#### Client Count

```erlang
%% Get client manager stats
macula_gateway_client_manager:get_stats().
%% Returns: #{clients => 1234, max => 10000, streams => 3456}

%% Check rejection rate (if tracked)
macula_gateway_client_manager:get_rejection_count().
```

#### Service Registry

```erlang
%% Get service count
macula_service_registry:count_services().
%% Returns: 45

%% List all registered services (debug only)
macula_service_registry:list_services().
```

### Performance Metrics

| Metric | Target | Warning | Critical |
|--------|--------|---------|----------|
| RPC Latency (p50) | < 10ms | > 50ms | > 200ms |
| RPC Latency (p99) | < 50ms | > 200ms | > 1000ms |
| PubSub Throughput | > 1,000 msg/s | < 500 msg/s | < 100 msg/s |
| DHT Query Time | < 100ms | > 200ms | > 500ms |
| Cache Hit Rate | > 90% | < 80% | < 50% |

---

## Log Monitoring

### Log Levels

Macula uses standard OTP log levels:

| Level | Usage | Action Required |
|-------|-------|-----------------|
| `debug` | Detailed operational info | None (high volume) |
| `info` | Normal operations | None |
| `notice` | Significant events | Review if unusual |
| `warning` | Potential issues | Investigate |
| `error` | Operation failures | Fix required |

### Critical Log Patterns

#### Memory Management (Normal Operation)

```erlang
%% Service cleanup - runs every 60 seconds
[info] Service cleanup: removed 3 expired service(s)
[debug] Service cleanup: no expired services

%% Connection pool LRU eviction
[debug] Evicted LRU connection: NodeId=abc123

%% Stream cleanup on disconnect
[debug] Cleaned up streams for disconnected client: NodeId=xyz789
```

#### Warning Signs

```erlang
%% Client rejection - monitor frequency
[warning] Client connection rejected: max_clients_reached

%% RPC timeout
[warning] RPC call timed out: Procedure=energy.home.get, CallId=call-123

%% DHT query failure
[warning] DHT query failed: Key=energy.home.get, Reason=timeout
```

#### Errors Requiring Action

```erlang
%% Gateway crash
[error] Gateway process crashed: Reason={badmatch, undefined}

%% QUIC connection failure
[error] QUIC handshake failed: Endpoint=192.168.1.100:4433, Reason=tls_alert

%% Memory pressure
[error] Memory threshold exceeded: Current=85%, Threshold=80%
```

### Log Aggregation Queries

#### Grafana Loki / Elasticsearch

```
# Client rejections in last hour
{app="macula"} |= "max_clients_reached" | count_over_time([1h])

# RPC timeouts by procedure
{app="macula"} |= "RPC call timed out" | regexp "Procedure=(?P<proc>[^,]+)" | by (proc)

# Service cleanup activity
{app="macula"} |= "Service cleanup" | rate([5m])
```

---

## Health Checks

### HTTP Health Endpoint

If using `macula_gateway` with HTTP enabled:

```bash
# Basic health check
curl http://localhost:4433/health

# Detailed status
curl http://localhost:4433/status
```

### Erlang Health Functions

```erlang
%% Check gateway is alive
is_pid(whereis(macula_gateway)).

%% Check all supervisors
[{Name, is_pid(whereis(Name))} || Name <- [
    macula_sup,
    macula_gateway_sup,
    macula_connection_sup
]].

%% Check DHT connectivity
macula_dht:ping().
%% Returns: pong | {error, Reason}
```

### Kubernetes Probes

```yaml
# Liveness probe - is the process running?
livenessProbe:
  exec:
    command:
      - /opt/macula/bin/macula
      - eval
      - "is_pid(whereis(macula_gateway))."
  initialDelaySeconds: 30
  periodSeconds: 10

# Readiness probe - is it accepting traffic?
readinessProbe:
  exec:
    command:
      - /opt/macula/bin/macula
      - eval
      - "macula_gateway:is_ready()."
  initialDelaySeconds: 10
  periodSeconds: 5
```

---

## Alerting

### Alert Priority Matrix

| Severity | Response Time | Examples |
|----------|---------------|----------|
| P1 (Critical) | < 15 min | Gateway down, OOM, all clients disconnected |
| P2 (High) | < 1 hour | 80%+ capacity, sustained errors |
| P3 (Medium) | < 4 hours | Elevated latency, cache miss rate |
| P4 (Low) | Next business day | Warnings, cleanup anomalies |

### Recommended Alerts

#### Critical (P1)

```yaml
- alert: MaculaGatewayDown
  expr: up{job="macula"} == 0
  for: 1m
  labels:
    severity: critical
  annotations:
    summary: "Macula gateway is down"

- alert: MaculaOOMRisk
  expr: process_resident_memory_bytes{job="macula"} > 8e9  # 8GB
  for: 5m
  labels:
    severity: critical
  annotations:
    summary: "Macula memory exceeds 8GB - OOM risk"
```

#### High (P2)

```yaml
- alert: MaculaClientPoolNearCapacity
  expr: macula_peers_current / macula_peers_max > 0.8
  for: 10m
  labels:
    severity: high
  annotations:
    summary: "Client pool at {{ $value | humanizePercentage }} capacity"

- alert: MaculaHighRejectionRate
  expr: rate(macula_peer_rejections_total[5m]) > 10
  for: 5m
  labels:
    severity: high
  annotations:
    summary: "High client rejection rate: {{ $value }}/sec"
```

#### Medium (P3)

```yaml
- alert: MaculaElevatedLatency
  expr: histogram_quantile(0.99, macula_rpc_latency_bucket) > 0.5
  for: 15m
  labels:
    severity: medium
  annotations:
    summary: "RPC p99 latency elevated: {{ $value }}s"

- alert: MaculaLowCacheHitRate
  expr: macula_cache_hits / (macula_cache_hits + macula_cache_misses) < 0.8
  for: 30m
  labels:
    severity: medium
  annotations:
    summary: "Cache hit rate below 80%"
```

---

## Dashboards

### Essential Dashboard Panels

#### 1. Resource Utilization

```
┌─────────────────────────────────────────────────────────────┐
│  Connection Pool        │  Client Pool          │  Memory   │
│  ┌───────────────────┐  │  ┌─────────────────┐  │  ┌─────┐  │
│  │▓▓▓▓▓▓▓░░░░░░░░░░░│  │  │▓▓▓▓▓▓▓▓▓▓░░░░░░│  │  │▓▓▓▓░│  │
│  │ 245/1000 (24%)   │  │  │ 6,234/10,000    │  │  │ 2.1G│  │
│  └───────────────────┘  │  └─────────────────┘  │  └─────┘  │
└─────────────────────────────────────────────────────────────┘
```

#### 2. Throughput

```
┌─────────────────────────────────────────────────────────────┐
│  RPC Calls/sec                                              │
│  2500 ┤                    ╭─╮                               │
│  2000 ┤               ╭────╯ ╰────╮                          │
│  1500 ┤          ╭────╯           ╰────╮                     │
│  1000 ┤     ╭────╯                     ╰────                 │
│   500 ┼─────╯                                                │
│       └─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴────  │
│        00:00 03:00 06:00 09:00 12:00 15:00 18:00 21:00       │
├─────────────────────────────────────────────────────────────┤
│  PubSub Events/sec                                          │
│  5000 ┤          ╭────────────────╮                          │
│  4000 ┤     ╭────╯                ╰────────╮                 │
│  3000 ┤╭────╯                              ╰────             │
│  2000 ┼╯                                                     │
│       └─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴────  │
└─────────────────────────────────────────────────────────────┘
```

#### 3. Latency Distribution

```
┌─────────────────────────────────────────────────────────────┐
│  RPC Latency (ms)                                           │
│                                                             │
│  p50:  ████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  8ms       │
│  p90:  ██████████████████░░░░░░░░░░░░░░░░░░░░░░  25ms      │
│  p99:  ██████████████████████████████░░░░░░░░░░  45ms      │
│  max:  ████████████████████████████████████████  120ms     │
│                                                             │
│        0    25    50    75    100   125   150              │
└─────────────────────────────────────────────────────────────┘
```

### Grafana Dashboard JSON

A basic dashboard template:

```json
{
  "title": "Macula Overview",
  "panels": [
    {
      "title": "Client Pool Utilization",
      "type": "gauge",
      "targets": [{"expr": "macula_peers_current / macula_peers_max * 100"}],
      "fieldConfig": {
        "defaults": {
          "thresholds": {
            "steps": [
              {"color": "green", "value": 0},
              {"color": "yellow", "value": 70},
              {"color": "red", "value": 90}
            ]
          }
        }
      }
    },
    {
      "title": "RPC Throughput",
      "type": "graph",
      "targets": [{"expr": "rate(macula_rpc_calls_total[5m])"}]
    },
    {
      "title": "Memory Usage",
      "type": "graph",
      "targets": [{"expr": "process_resident_memory_bytes{job=\"macula\"}"}]
    }
  ]
}
```

---

## Capacity Planning

### Resource Sizing

| Deployment Size | Clients | Memory | CPU | Notes |
|-----------------|---------|--------|-----|-------|
| Small (Dev) | < 100 | 512MB | 1 core | Single node |
| Medium | 100-1,000 | 2GB | 2 cores | Typical production |
| Large | 1,000-10,000 | 8GB | 4 cores | High availability |
| XL | 10,000+ | 16GB+ | 8+ cores | Multi-gateway |

### Scaling Triggers

| Metric | Threshold | Action |
|--------|-----------|--------|
| Client pool | > 80% for 1 hour | Add gateway node |
| Memory | > 70% sustained | Increase memory or add node |
| CPU | > 80% sustained | Add CPU or optimize handlers |
| RPC latency p99 | > 200ms sustained | Profile handlers, check DHT |
| Connection churn | > 1000/min | Check client stability |

### Horizontal Scaling

Macula supports horizontal scaling via multiple gateway nodes:

```
                    ┌─────────────────┐
                    │  Load Balancer  │
                    │  (DNS/HAProxy)  │
                    └────────┬────────┘
                             │
        ┌────────────────────┼────────────────────┐
        │                    │                    │
        ▼                    ▼                    ▼
┌───────────────┐    ┌───────────────┐    ┌───────────────┐
│   Gateway 1   │    │   Gateway 2   │    │   Gateway 3   │
│  10k clients  │    │  10k clients  │    │  10k clients  │
└───────┬───────┘    └───────┬───────┘    └───────┬───────┘
        │                    │                    │
        └────────────────────┼────────────────────┘
                             │
                    ┌────────▼────────┐
                    │   DHT Network   │
                    │  (Shared State) │
                    └─────────────────┘
```

Each gateway operates independently with shared DHT for service discovery.

---

## See Also

- [Performance Guide](PERFORMANCE_GUIDE.md) - Optimization techniques
- [Troubleshooting Guide](TROUBLESHOOTING_GUIDE.md) - Common issues
