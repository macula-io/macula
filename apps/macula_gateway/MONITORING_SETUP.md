# Macula Gateway Monitoring Setup

This document explains the automatic monitoring configuration for the Macula Gateway.

## Overview

The Macula Gateway is now configured for **automatic discovery** by Prometheus and Grafana. No manual configuration required!

## What Was Configured

### 1. Prometheus Auto-Discovery

The gateway deployment includes Prometheus scrape annotations that enable automatic metrics collection:

```yaml
annotations:
  prometheus.io/scrape: "true"
  prometheus.io/port: "8080"
  prometheus.io/path: "/metrics"
```

**Location:** `/home/rl/work/github.com/macula-io/cortex-iq-deploy/base/macula-gateway/deployment.yaml`

Prometheus is configured to automatically discover and scrape any pod with these annotations.

### 2. Metrics Endpoint

The gateway exposes 9 comprehensive metrics on port 8080:

| Metric | Type | Description |
|--------|------|-------------|
| `macula_gateway_uptime_seconds` | gauge | Gateway uptime in seconds |
| `macula_gateway_ready` | gauge | Readiness status (1=ready, 0=not ready) |
| `macula_gateway_process_count` | gauge | Number of Erlang processes |
| `macula_gateway_memory_bytes` | gauge | Total VM memory usage |
| `macula_gateway_process_memory_bytes` | gauge | Process memory usage |
| `macula_gateway_diagnostics_available` | gauge | Diagnostics service availability |
| `macula_gateway_clients_total` | gauge | Number of connected clients |
| `macula_gateway_subscriptions_total` | gauge | Active subscriptions |
| `macula_gateway_registrations_total` | gauge | Registered RPC procedures |

### 3. Grafana Dashboard

A pre-configured dashboard is available showing:
- Gateway status and uptime
- Memory usage trends
- Erlang process count
- Client activity (connections, subscriptions, registrations)
- Diagnostics service health

**Location:** `/home/rl/work/github.com/macula-io/cortex-iq-deploy/base/macula-gateway/grafana-dashboard.yaml`

## Verification

### Check Prometheus is Scraping

1. Port-forward to Prometheus:
   ```bash
   kubectl --context kind-macula-hub port-forward -n observability svc/prometheus 9090:9090
   ```

2. Open http://localhost:9090/targets

3. Look for the `kubernetes-pods` job and find the macula-gateway target

4. Verify it shows as "UP"

### Query Metrics in Prometheus

1. Go to http://localhost:9090/graph

2. Try these queries:
   ```promql
   macula_gateway_uptime_seconds
   macula_gateway_ready
   macula_gateway_clients_total
   macula_gateway_memory_bytes / 1024 / 1024  # Memory in MB
   ```

### Access Grafana Dashboard

1. Port-forward to Grafana:
   ```bash
   kubectl --context kind-macula-hub port-forward -n observability svc/grafana 3000:3000
   ```

2. Open http://localhost:3000

3. Default credentials:
   - Username: `admin`
   - Password: (check the Grafana secret or ConfigMap)

4. Navigate to **Dashboards → Browse** and look for "Macula Gateway"

## Prometheus Scrape Configuration

The Prometheus instance is configured with a `kubernetes-pods` job that uses service discovery:

```yaml
- job_name: 'kubernetes-pods'
  kubernetes_sd_configs:
    - role: pod
  relabel_configs:
    # Only scrape pods with prometheus.io/scrape: "true"
    - source_labels: [__meta_kubernetes_pod_annotation_prometheus_io_scrape]
      action: keep
      regex: true
    # Use custom path if specified
    - source_labels: [__meta_kubernetes_pod_annotation_prometheus_io_path]
      action: replace
      target_label: __metrics_path__
      regex: (.+)
    # Use custom port if specified
    - source_labels: [__address__, __meta_kubernetes_pod_annotation_prometheus_io_port]
      action: replace
      regex: ([^:]+)(?::\d+)?;(\d+)
      replacement: $1:$2
      target_label: __address__
```

This configuration is already present in the Prometheus ConfigMap at:
`observability/prometheus-config`

## Alerting (Optional Future Enhancement)

Example Prometheus alerting rules that could be added:

```yaml
groups:
  - name: macula-gateway
    rules:
      - alert: MaculaGatewayDown
        expr: up{job="kubernetes-pods",app="macula-gateway"} == 0
        for: 2m
        labels:
          severity: critical
        annotations:
          summary: "Macula Gateway is down"

      - alert: MaculaGatewayNotReady
        expr: macula_gateway_ready == 0
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Macula Gateway not ready"

      - alert: MaculaGatewayHighMemory
        expr: macula_gateway_memory_bytes > 3000000000  # 3GB
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "Macula Gateway high memory usage"
```

## How It Works

1. **Pod Annotations**: The gateway deployment includes Prometheus annotations
2. **Service Discovery**: Prometheus continuously watches for pods with `prometheus.io/scrape: "true"`
3. **Automatic Scraping**: When discovered, Prometheus starts scraping the `/metrics` endpoint on port 8080
4. **Dashboard Loading**: Grafana loads dashboards from ConfigMaps with the label `grafana_dashboard: "1"`

## Troubleshooting

### Metrics not appearing in Prometheus

1. Check pod has annotations:
   ```bash
   kubectl --context kind-macula-hub get pod <pod-name> -n macula-system -o jsonpath='{.metadata.annotations}' | jq
   ```

2. Check Prometheus targets:
   ```bash
   kubectl --context kind-macula-hub port-forward -n observability svc/prometheus 9090:9090
   # Visit http://localhost:9090/targets
   ```

3. Test metrics endpoint directly:
   ```bash
   POD_IP=$(kubectl --context kind-macula-hub get pod <pod-name> -n macula-system -o jsonpath='{.status.podIP}')
   kubectl --context kind-macula-hub run -n macula-system test --image=curlimages/curl:latest --rm -i --restart=Never -- curl http://$POD_IP:8080/metrics
   ```

### Dashboard not appearing in Grafana

1. Check ConfigMap exists:
   ```bash
   kubectl --context kind-macula-hub get configmap macula-gateway-dashboard -n observability
   ```

2. Check ConfigMap has correct label:
   ```bash
   kubectl --context kind-macula-hub get configmap macula-gateway-dashboard -n observability -o jsonpath='{.metadata.labels}'
   ```

3. Restart Grafana to reload dashboards:
   ```bash
   kubectl --context kind-macula-hub delete pod -n observability -l app=grafana
   ```

## References

- [Prometheus Kubernetes SD](https://prometheus.io/docs/prometheus/latest/configuration/configuration/#kubernetes_sd_config)
- [Prometheus Metric Types](https://prometheus.io/docs/concepts/metric_types/)
- [Grafana Dashboard Provisioning](https://grafana.com/docs/grafana/latest/administration/provisioning/#dashboards)
- [Macula Gateway Health Endpoints](./HEALTH.md)
- [Macula Gateway Diagnostics](./DIAGNOSTICS.md)
