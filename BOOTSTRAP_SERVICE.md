# Macula Bootstrap Service (Revised)

A lightweight, headless deployment of Macula configured for **bootstrap-only mode**.

## Architecture Discovery

After investigating the codebase, **bootstrap is already implemented** and happens via **QUIC/HTTP3**, not a separate HTTP API!

### How Bootstrap Currently Works:

```
┌──────────────┐                                  ┌────────────────────┐
│  Edge Peer   │    QUIC/HTTP3 (port 4433)       │  Gateway (Bootstrap│
│              │──────────────────────────────────│      Mode)         │
│ MACULA_      │   DHT Protocol Messages:         │                    │
│ BOOTSTRAP_   │   - FIND_NODE                    │  macula_bootstrap_ │
│ REGISTRY=    │   - FIND_VALUE                   │  system supervisor │
│ https://     │   - STORE                        │  ├── registry      │
│ gateway:4433 │                                  │  ├── server        │
│              │                                  │  └── health        │
└──────────────┘                                  └────────────────────┘
```

### Existing Code:

✅ **`src/macula_bootstrap_system/`** - Already exists!
  - `macula_bootstrap_system.erl` - Supervisor
  - `macula_bootstrap_server.erl` - Handles DHT queries (FIND_NODE, FIND_VALUE, STORE)
  - `macula_bootstrap_registry.erl` - ETS-based service registry
  - `macula_bootstrap_health.erl` - Health monitoring

✅ **Bootstrap mode** - Gateway can run in bootstrap-only mode (from `src/macula_root.erl`):
```erlang
maybe_start_bootstrap(bootstrap) -> get_bootstrap_specs();
maybe_start_bootstrap(hybrid) -> get_bootstrap_specs();
```

✅ **Peer connection** - Peers specify gateway URL via environment:
```bash
MACULA_BOOTSTRAP_REGISTRY="https://arcade-gateway:4433"
```

## What Bootstrap Service Actually Is

The "bootstrap service" is simply a **lightweight gateway deployment** configured for:
- **No message relaying** (peers communicate directly via DHT)
- **Bootstrap-only** mode (just DHT operations)
- **Minimal resources** (no pub/sub routing, no RPC bridging)

## Deployment Options

### Option 1: Standalone Docker Container (RECOMMENDED)

Create a minimal Dockerfile that runs macula in bootstrap-only mode:

```dockerfile
# Dockerfile.bootstrap
FROM erlang:27-alpine AS builder

WORKDIR /build
COPY . .

# Build release
RUN rebar3 as prod release

# Runtime image
FROM alpine:3.19

RUN apk add --no-cache \
    libstdc++ \
    ncurses-libs \
    openssl

WORKDIR /opt/macula

# Copy release
COPY --from=builder /build/_build/prod/rel/macula ./

# Expose QUIC/HTTP3 port
EXPOSE 4433

# Health check (HTTP endpoint if enabled, or use ping)
HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
    CMD /opt/macula/bin/macula ping || exit 1

# Run as non-root
RUN addgroup -g 1000 macula && \
    adduser -D -u 1000 -G macula macula && \
    chown -R macula:macula /opt/macula

USER macula

CMD ["/opt/macula/bin/macula", "foreground"]
```

### Environment Configuration (Bootstrap Mode)

```yaml
# docker-compose.bootstrap.yml
version: '3.8'

services:
  bootstrap:
    build:
      context: .
      dockerfile: Dockerfile.bootstrap
    container_name: macula-bootstrap
    hostname: macula-bootstrap
    environment:
      # Bootstrap-only mode
      MACULA_MODE: "bootstrap"
      MACULA_REALM: "macula.bootstrap"
      MACULA_GATEWAY_PORT: "4433"

      # TLS certificates
      MACULA_CERT_PATH: "/opt/macula/certs/cert.pem"
      MACULA_KEY_PATH: "/opt/macula/certs/key.pem"

      # Logging
      LOG_LEVEL: "info"

      # Health check interval
      BOOTSTRAP_HEALTH_INTERVAL: "60000"  # 60 seconds
    ports:
      - "4433:4433"   # QUIC/HTTP3
    volumes:
      - bootstrap-certs:/opt/macula/certs
    networks:
      macula-mesh:
        ipv4_address: 172.30.0.10
    healthcheck:
      test: ["/opt/macula/bin/macula", "ping"]
      interval: 30s
      timeout: 3s
      retries: 3
      start_period: 20s

volumes:
  bootstrap-certs:

networks:
  macula-mesh:
    driver: bridge
    ipam:
      config:
        - subnet: 172.30.0.0/24
```

### Option 2: Configuration via sys.config

Create `config/bootstrap.sys.config`:

```erlang
[
    {macula, [
        {mode, bootstrap},  % Bootstrap-only mode
        {realm, <<"macula.bootstrap">>},
        {gateway_port, 4433},
        {cert_file, "/opt/macula/certs/cert.pem"},
        {key_file, "/opt/macula/certs/key.pem"},
        {bootstrap_health_interval, 60000},  % 60 seconds
        {log_level, info}
    ]},
    {kernel, [
        {logger_level, info},
        {logger, [
            {handler, default, logger_std_h, #{
                formatter => {logger_formatter, #{
                    single_line => true,
                    template => [time," [",level,"] ",msg,"\n"]
                }}
            }}
        ]}
    ]}
].
```

## Bootstrap-Only Mode Behavior

From `src/macula_gateway.erl`, gateway in bootstrap mode:

1. **✅ Accepts DHT queries** - FIND_NODE, FIND_VALUE, STORE
2. **❌ Rejects RPC calls** - Returns error: `gateway_bootstrap_only`
3. **❌ Rejects pub/sub** - Returns error (peers communicate directly)
4. **✅ Maintains DHT routing table** - Via `macula_routing_server`
5. **✅ Stores service registrations** - Via `macula_bootstrap_registry`
6. **✅ Health monitoring** - Via `macula_bootstrap_health`

### Example Error (Bootstrap-Only Mode):

```erlang
%% From src/macula_gateway.erl line 656
#{
    error => #{
        code => <<"gateway_bootstrap_only">>,
        message => <<"Gateway is bootstrap-only. Discover service via DHT and call peer directly">>
    }
}
```

## API (QUIC/HTTP3 Protocol)

Bootstrap service responds to DHT protocol messages over QUIC:

### 1. FIND_NODE Query
Find K closest nodes to target ID.

**Request:**
```erlang
{dht_query, find_node, TargetNodeId}
```

**Response:**
```erlang
{ok, [
    #{node_id => NodeId1, endpoint => <<"https://peer1:4433">>},
    #{node_id => NodeId2, endpoint => <<"https://peer2:4433">>},
    ...
]}
```

### 2. FIND_VALUE Query
Lookup service by key.

**Request:**
```erlang
{dht_query, find_value, <<"service.calculator.add">>}
```

**Response:**
```erlang
{ok, #{
    node_id => ProviderId,
    endpoint => <<"https://provider:4433">>,
    handler => <<"calculator_handler">>
}}
```

### 3. STORE Operation
Store service registration.

**Request:**
```erlang
{dht_query, store, {
    <<"service.calculator.add">>,
    #{
        node_id => MyNodeId,
        endpoint => <<"https://my-peer:4433">>,
        handler => <<"calculator_handler">>
    }
}}
```

**Response:**
```erlang
ok
```

## Storage (ETS - In-Memory)

From `src/macula_bootstrap_system/macula_bootstrap_registry.erl`:

- **ETS table**: In-memory storage (lost on restart)
- **Stateless**: Peers re-register services on startup
- **Lightweight**: No database dependency
- **Fast**: ETS lookup performance

## Deployment Scenarios

### Scenario 1: Single Bootstrap Node (Development)

```bash
docker compose -f docker-compose.bootstrap.yml up
```

Peers connect to: `https://localhost:4433`

### Scenario 2: Multiple Bootstrap Nodes (Production)

Deploy 3+ bootstrap containers behind load balancer:

```yaml
services:
  bootstrap1:
    # ... config ...
    hostname: bootstrap1.example.com
    ports:
      - "4433:4433"

  bootstrap2:
    # ... config ...
    hostname: bootstrap2.example.com
    ports:
      - "4434:4433"  # Different host port

  bootstrap3:
    # ... config ...
    hostname: bootstrap3.example.com
    ports:
      - "4435:4433"
```

Peers can specify multiple bootstrap nodes:
```bash
MACULA_BOOTSTRAP_REGISTRY="https://bootstrap1.example.com:4433,https://bootstrap2.example.com:4434"
```

### Scenario 3: Kubernetes (Production)

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: macula-bootstrap
spec:
  replicas: 3  # Multiple for HA
  selector:
    matchLabels:
      app: macula-bootstrap
  template:
    metadata:
      labels:
        app: macula-bootstrap
    spec:
      containers:
      - name: bootstrap
        image: macula/macula:latest
        env:
        - name: MACULA_MODE
          value: "bootstrap"
        - name: MACULA_REALM
          value: "macula.bootstrap"
        - name: MACULA_GATEWAY_PORT
          value: "4433"
        ports:
        - containerPort: 4433
          name: quic
          protocol: UDP  # QUIC uses UDP
        livenessProbe:
          exec:
            command: ["/opt/macula/bin/macula", "ping"]
          initialDelaySeconds: 10
          periodSeconds: 30
        resources:
          requests:
            memory: "128Mi"
            cpu: "200m"
          limits:
            memory: "256Mi"
            cpu: "500m"
---
apiVersion: v1
kind: Service
metadata:
  name: macula-bootstrap
spec:
  selector:
    app: macula-bootstrap
  ports:
  - port: 4433
    targetPort: 4433
    protocol: UDP  # QUIC uses UDP
  type: LoadBalancer
```

## Benefits of Bootstrap-Only Mode

1. ✅ **Lightweight** - No message relaying overhead
2. ✅ **Stateless** - ETS-based, no persistent storage
3. ✅ **Fast** - Direct DHT queries, no routing
4. ✅ **Scalable** - Multiple bootstrap nodes, load balanced
5. ✅ **Simple** - Same codebase as full gateway, different config
6. ✅ **Native Protocol** - QUIC/HTTP3, not REST API
7. ✅ **Already Implemented** - Code exists, just needs deployment config

## Integration with Console

The console can monitor bootstrap nodes via:

1. **Health checks** - Ping bootstrap nodes periodically
2. **Statistics** - Query `macula_bootstrap_server:get_stats()`
3. **Registry inspection** - View registered services
4. **DHT visualization** - Show routing table, k-buckets

### Console API (Optional HTTP Wrapper)

If console needs HTTP API for monitoring, we can add a **separate HTTP endpoint** (port 4100) for management:

```erlang
%% Optional HTTP monitoring API (separate from QUIC)
%% GET /stats
macula_bootstrap_server:get_stats()

%% GET /services
macula_bootstrap_registry:list_all()

%% GET /health
macula_bootstrap_health:get_status()
```

But this is **optional** - core bootstrap happens via QUIC.

## Summary

**Bootstrap service is NOT a separate HTTP API service.**

It's a **lightweight gateway deployment** in bootstrap-only mode:
- ✅ Runs existing `macula_bootstrap_system` code
- ✅ Listens on QUIC port 4433
- ✅ Handles DHT protocol messages
- ✅ No message relaying (peers communicate directly)
- ✅ Can be deployed as standalone Docker container
- ✅ Multiple instances for HA
- ✅ Already implemented, just needs deployment configuration

## Next Steps

1. ✅ Create `Dockerfile.bootstrap` for standalone container
2. ✅ Add `docker-compose.bootstrap.yml` for local testing
3. ✅ Add `config/bootstrap.sys.config` for bootstrap-only mode
4. ✅ Test with arcade demo (peers connecting to bootstrap)
5. ✅ Document deployment guide
6. ✅ (Optional) Add HTTP monitoring API for console integration
