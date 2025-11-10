# Macula Gateway Health Endpoints

The Macula Gateway provides HTTP health check endpoints for Kubernetes liveness/readiness probes and Prometheus monitoring.

## Overview

All health endpoints are exposed on **port 8080** (separate from the QUIC gateway port 9443).

```
┌─────────────────────────────────────┐
│   Macula Gateway                     │
│                                      │
│  Port 9443 (UDP/QUIC)               │
│  └─> Client connections             │
│                                      │
│  Port 8080 (TCP/HTTP)               │
│  ├─> /health   - Overall health     │
│  ├─> /ready    - Readiness check    │
│  ├─> /live     - Liveness check     │
│  └─> /metrics  - Prometheus metrics │
└─────────────────────────────────────┘
```

## Endpoints

### 1. `/health` - Overall Health Status

Returns overall health status with uptime information.

**Method:** `GET`
**Port:** `8080`
**Response Type:** `application/json`

**Success Response (200 OK):**
```json
{
  "status": "healthy",
  "ready": true,
  "uptime": 3600
}
```

**Error Response (503 Service Unavailable):**
```json
{
  "status": "unavailable"
}
```

**Use Case:**
- Quick health check
- Human-readable status
- API health endpoints

**Example:**
```bash
curl http://localhost:8080/health
```

---

### 2. `/ready` - Readiness Probe

Indicates whether the gateway is ready to accept traffic. Returns 200 only after the QUIC listener has successfully started.

**Method:** `GET`
**Port:** `8080`
**Response Type:** `text/plain`

**Success Response (200 OK):**
```
Ready
```

**Not Ready Response (503 Service Unavailable):**
```
Not ready
```

**Use Case:**
- Kubernetes readiness probes
- Load balancer health checks
- Determines if pod should receive traffic

**Kubernetes Configuration:**
```yaml
readinessProbe:
  httpGet:
    path: /ready
    port: 8080
  initialDelaySeconds: 10
  periodSeconds: 5
  timeoutSeconds: 3
  failureThreshold: 3
```

**Example:**
```bash
curl http://localhost:8080/ready
```

---

### 3. `/live` - Liveness Probe

Indicates whether the gateway process is alive and responsive. Always returns 200 if the health server can process the request.

**Method:** `GET`
**Port:** `8080`
**Response Type:** `text/plain`

**Response (200 OK):**
```
Live
```

**Use Case:**
- Kubernetes liveness probes
- Detect deadlocked processes
- Trigger pod restart if unresponsive

**Kubernetes Configuration:**
```yaml
livenessProbe:
  httpGet:
    path: /live
    port: 8080
  initialDelaySeconds: 30
  periodSeconds: 10
  timeoutSeconds: 5
  failureThreshold: 3
```

**Example:**
```bash
curl http://localhost:8080/live
```

---

### 4. `/metrics` - Prometheus Metrics

Returns comprehensive metrics in Prometheus exposition format.

**Method:** `GET`
**Port:** `8080`
**Response Type:** `text/plain; version=0.0.4`

**Response (200 OK):**
```prometheus
# HELP macula_gateway_uptime_seconds Gateway uptime in seconds
# TYPE macula_gateway_uptime_seconds gauge
macula_gateway_uptime_seconds 3600

# HELP macula_gateway_ready Gateway ready status (1=ready, 0=not ready)
# TYPE macula_gateway_ready gauge
macula_gateway_ready 1

# HELP macula_gateway_process_count Number of Erlang processes
# TYPE macula_gateway_process_count gauge
macula_gateway_process_count 245

# HELP macula_gateway_memory_bytes Total memory used by the VM in bytes
# TYPE macula_gateway_memory_bytes gauge
macula_gateway_memory_bytes 45678912

# HELP macula_gateway_process_memory_bytes Memory used by Erlang processes in bytes
# TYPE macula_gateway_process_memory_bytes gauge
macula_gateway_process_memory_bytes 12345678

# HELP macula_gateway_diagnostics_available Diagnostics service availability (1=available, 0=unavailable)
# TYPE macula_gateway_diagnostics_available gauge
macula_gateway_diagnostics_available 1

# HELP macula_gateway_clients_total Number of connected clients
# TYPE macula_gateway_clients_total gauge
macula_gateway_clients_total 5

# HELP macula_gateway_subscriptions_total Number of active subscriptions
# TYPE macula_gateway_subscriptions_total gauge
macula_gateway_subscriptions_total 12

# HELP macula_gateway_registrations_total Number of registered procedures
# TYPE macula_gateway_registrations_total gauge
macula_gateway_registrations_total 8
```

**Use Case:**
- Prometheus monitoring
- Grafana dashboards
- Alerting on metric thresholds
- Capacity planning
- Performance tracking

**Prometheus Configuration:**
```yaml
scrape_configs:
  - job_name: 'macula-gateway'
    metrics_path: '/metrics'
    scrape_interval: 30s
    static_configs:
      - targets:
        - macula-gateway:8080
    relabel_configs:
      - source_labels: [__address__]
        target_label: instance
      - source_labels: [__meta_kubernetes_pod_name]
        target_label: pod
```

**Example:**
```bash
curl http://localhost:8080/metrics
```

---

## Available Metrics

### System Metrics

| Metric | Type | Description |
|--------|------|-------------|
| `macula_gateway_uptime_seconds` | gauge | Gateway uptime in seconds since last restart |
| `macula_gateway_ready` | gauge | Readiness status (1=ready, 0=not ready) |
| `macula_gateway_process_count` | gauge | Number of Erlang processes in the VM |
| `macula_gateway_memory_bytes` | gauge | Total memory used by the Erlang VM |
| `macula_gateway_process_memory_bytes` | gauge | Memory used by Erlang processes |

### Service Metrics

| Metric | Type | Description |
|--------|------|-------------|
| `macula_gateway_diagnostics_available` | gauge | Diagnostics service availability (1=up, 0=down) |
| `macula_gateway_clients_total` | gauge | Number of currently connected clients |
| `macula_gateway_subscriptions_total` | gauge | Number of active pub/sub subscriptions |
| `macula_gateway_registrations_total` | gauge | Number of registered RPC procedures |

---

## Configuration

### Environment Variables

```bash
# Health check server port (default: 8080)
HEALTH_PORT=8080
```

### sys.config

```erlang
[
  {macula, [
    {health_port, 8080}
  ]}
].
```

### Docker

```bash
# Expose both QUIC and health ports
docker run -p 9443:9443 -p 8080:8080 macula/macula-gateway:latest
```

### Kubernetes

```yaml
apiVersion: v1
kind: Service
metadata:
  name: macula-gateway
spec:
  selector:
    app: macula-gateway
  ports:
    - name: quic
      port: 9443
      protocol: UDP
    - name: health
      port: 8080
      protocol: TCP
---
apiVersion: v1
kind: Service
metadata:
  name: macula-gateway-metrics
  labels:
    app: macula-gateway
  annotations:
    prometheus.io/scrape: "true"
    prometheus.io/port: "8080"
    prometheus.io/path: "/metrics"
spec:
  selector:
    app: macula-gateway
  ports:
    - name: metrics
      port: 8080
      protocol: TCP
```

---

## Monitoring Setup

### 1. Prometheus ServiceMonitor (Recommended)

```yaml
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: macula-gateway
  namespace: macula-system
spec:
  selector:
    matchLabels:
      app: macula-gateway
  endpoints:
    - port: health
      path: /metrics
      interval: 30s
      scrapeTimeout: 10s
```

### 2. Prometheus Scrape Config (Manual)

```yaml
# prometheus.yml
scrape_configs:
  - job_name: 'macula-gateway'
    kubernetes_sd_configs:
      - role: pod
        namespaces:
          names:
            - macula-system
    relabel_configs:
      - source_labels: [__meta_kubernetes_pod_label_app]
        action: keep
        regex: macula-gateway
      - source_labels: [__meta_kubernetes_pod_ip]
        target_label: __address__
        replacement: '$1:8080'
```

### 3. Alertmanager Rules

```yaml
# alerts.yml
groups:
  - name: macula-gateway
    interval: 30s
    rules:
      - alert: MaculaGatewayDown
        expr: up{job="macula-gateway"} == 0
        for: 2m
        labels:
          severity: critical
        annotations:
          summary: "Macula Gateway is down"
          description: "Gateway {{ $labels.instance }} has been down for more than 2 minutes"

      - alert: MaculaGatewayNotReady
        expr: macula_gateway_ready == 0
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Macula Gateway not ready"
          description: "Gateway {{ $labels.instance }} is not ready for {{ $value }} minutes"

      - alert: MaculaGatewayHighMemory
        expr: macula_gateway_memory_bytes > 500000000
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "Macula Gateway high memory usage"
          description: "Gateway {{ $labels.instance }} is using {{ humanize $value }} bytes of memory"

      - alert: MaculaGatewayHighProcessCount
        expr: macula_gateway_process_count > 10000
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "Macula Gateway high process count"
          description: "Gateway {{ $labels.instance }} has {{ $value }} processes"
```

---

## Grafana Dashboard

### Example Queries

**Uptime:**
```promql
macula_gateway_uptime_seconds
```

**Memory Usage:**
```promql
macula_gateway_memory_bytes / 1024 / 1024
```

**Connected Clients:**
```promql
sum(macula_gateway_clients_total)
```

**Active Subscriptions:**
```promql
sum(macula_gateway_subscriptions_total)
```

**Process Count:**
```promql
macula_gateway_process_count
```

### Dashboard Template

See [GRAFANA_DASHBOARD.json](./GRAFANA_DASHBOARD.json) for a complete Grafana dashboard template.

---

## Troubleshooting

### Health Endpoints Not Responding

**Check port is accessible:**
```bash
kubectl port-forward -n macula-system pod/macula-gateway-xxx 8080:8080
curl http://localhost:8080/health
```

**Check logs:**
```bash
kubectl logs -n macula-system pod/macula-gateway-xxx | grep -i health
```

Expected output:
```
Starting health check server on port 8080
Health check server listening on port 8080
```

### `/ready` Always Returns 503

This indicates the QUIC listener hasn't started successfully. Check gateway logs:
```bash
kubectl logs -n macula-system pod/macula-gateway-xxx | grep -i "listening\|error"
```

Expected output:
```
Macula Gateway listening on port 9443 (realm: be.cortexiq.energy)
```

### Metrics Showing Zero Values

This may indicate:
1. No clients connected (expected on fresh start)
2. Diagnostics service not running (check logs for "Starting diagnostics service")
3. Gateway not fully initialized

Check service status:
```bash
# Check all services started
kubectl logs -n macula-system pod/macula-gateway-xxx | grep "Starting"
```

---

## Performance Impact

### Health Endpoint Overhead

- **`/health`**: < 1ms response time, negligible CPU
- **`/ready`**: < 1ms response time, checks boolean flag
- **`/live`**: < 1ms response time, immediate response
- **`/metrics`**: < 5ms response time, collects system stats

### Prometheus Scrape Impact

At default 30-second scrape interval:
- **Network**: ~2 KB per scrape = ~4 KB/minute
- **CPU**: < 0.1% per scrape
- **Memory**: Negligible (metrics generated on-demand)

**Recommendation:** 30-60 second scrape intervals are optimal for most use cases.

---

## See Also

- [Diagnostics Service](DIAGNOSTICS.md) - RPC-based diagnostics procedures
- [Prometheus Documentation](https://prometheus.io/docs/introduction/overview/)
- [Kubernetes Health Probes](https://kubernetes.io/docs/tasks/configure-pod-container/configure-liveness-readiness-startup-probes/)
- [Grafana Dashboards](https://grafana.com/docs/grafana/latest/dashboards/)
