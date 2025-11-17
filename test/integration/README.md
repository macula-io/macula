# Macula Integration Tests

**Purpose**: End-to-end and integration testing of Macula multi-mode deployment

---

## Test Environment

### Docker Compose Topology

```
Bootstrap (172.20.0.10) - DHT entry point
├── Gateway (172.20.0.20) - Relay node
│   ├── Edge1 (172.20.0.31) - Pure P2P peer
│   └── Edge2 (172.20.0.32) - Pure P2P peer (RPC client)
└── Edge3 (172.20.0.33) - Pure P2P peer (RPC provider)
```

### Deployment Modes Tested

- **Bootstrap Mode**: DHT entry point, service registry, health monitoring
- **Gateway Mode**: QUIC listener, message relay, NAT fallback
- **Edge Mode**: Pure P2P peers with DHT-routed communication

---

## Quick Start

### Option 1: Shell Script (Recommended for CI/CD)

```bash
# Build and run all tests
./docker/test-multi-mode.sh --build

# Run with existing images
./docker/test-multi-mode.sh

# Clean up after testing
./docker/test-multi-mode.sh --build --clean
```

### Option 2: Common Test Suite

```bash
# Start Docker Compose environment
docker-compose -f docker/docker-compose.multi-mode.yml up -d

# Wait for services to be healthy
docker ps --filter "name=macula-"

# Run Common Test suite
rebar3 ct --suite=test/integration/multi_hop_rpc_SUITE

# Stop environment
docker-compose -f docker/docker-compose.multi-mode.yml down
```

---

## Test Suites

### 1. multi_hop_rpc_SUITE.erl

**Purpose**: Test multi-hop RPC routing via DHT

**Test Cases**:
- ✅ `test_bootstrap_node_healthy` - Bootstrap node health check
- ✅ `test_gateway_node_healthy` - Gateway node health check
- ✅ `test_edge_nodes_healthy` - Edge nodes health checks
- ✅ `test_dht_peer_discovery` - DHT routing table population
- ✅ `test_service_registration_in_dht` - Service registration
- ✅ `test_service_discovery_via_dht` - Service discovery
- ⏳ `test_single_hop_rpc_call` - Direct RPC call (TODO)
- ⏳ `test_multi_hop_rpc_call` - Multi-hop RPC via DHT (TODO)
- ⏳ `test_rpc_timeout_handling` - RPC timeout (TODO)
- ⏳ `test_rpc_provider_not_found` - Provider not found (TODO)
- ⏳ `test_rpc_max_hops_exceeded` - Max hops exceeded (TODO)

**Status**: 6/11 tests implemented

---

### 2. multi_hop_pubsub_SUITE.erl (TODO)

**Purpose**: Test multi-hop pub/sub routing via DHT

**Test Cases (Planned)**:
- Subscriber registration in DHT
- Publisher discovers subscribers via DHT
- Multi-hop publish routing
- Wildcard topic subscriptions
- Pub/sub fanout to multiple subscribers
- QoS handling

**Status**: Not implemented yet

---

## Manual Testing

### Access Containers

```bash
# Bootstrap node
docker exec -it macula-bootstrap bin/macula remote_console

# Gateway node
docker exec -it macula-gateway bin/macula remote_console

# Edge nodes
docker exec -it macula-edge1 bin/macula remote_console
docker exec -it macula-edge2 bin/macula remote_console
docker exec -it macula-edge3 bin/macula remote_console
```

### Test Commands

#### Check DHT Routing Table

```erlang
% Get routing server PID
{ok, Pid} = whereis(macula_routing_server).

% Get routing table
RoutingTable = macula_routing_server:get_routing_table(Pid).

% Count peers
length(RoutingTable).
```

#### Register RPC Service

```erlang
% Get routing server PID
{ok, Pid} = whereis(macula_routing_server).

% Register service in DHT
ServiceKey = <<"test.calculator.add">>.
NodeId = crypto:strong_rand_bytes(32).
ServiceValue = #{node_id => NodeId, endpoint => <<"172.20.0.33:9443">>}.
ok = macula_routing_server:store_local(Pid, ServiceKey, ServiceValue).
```

#### Discover RPC Service

```erlang
% Get routing server PID
{ok, Pid} = whereis(macula_routing_server).

% Lookup service in DHT
ServiceKey = <<"test.calculator.add">>.
{ok, Provider} = macula_routing_server:get_local(Pid, ServiceKey).
```

---

## Test Scenarios

### Scenario 1: DHT Bootstrap and Discovery

**Steps**:
1. Bootstrap node starts and initializes DHT
2. Gateway connects to bootstrap, joins DHT
3. Edge nodes connect to bootstrap, join DHT
4. All nodes discover each other via DHT

**Verification**:
- Each node has >= 2 peers in routing table
- DHT queries find registered services

### Scenario 2: Multi-Hop RPC Call

**Flow**: Edge2 (client) → Edge1 → Gateway → Edge3 (provider)

**Steps**:
1. Edge3 registers RPC service "test.calculator.add" in DHT
2. Edge2 discovers service via DHT FIND_VALUE query
3. Edge2 wraps RPC call in `rpc_route` envelope
4. Message hops through mesh using XOR distance routing
5. Edge3 receives call, processes, sends reply
6. Reply routes back to Edge2

**Verification**:
- Call completes within timeout (5 seconds)
- Reply contains correct result
- Hop count <= max_hops (10)

### Scenario 3: Multi-Hop Pub/Sub

**Flow**: Edge2 (publisher) → DHT → Edge1, Edge3 (subscribers)

**Steps**:
1. Edge1 and Edge3 subscribe to topic "test.events"
2. Subscriptions stored in DHT
3. Edge2 publishes to topic
4. Edge2 discovers subscribers via DHT FIND_VALUE
5. Edge2 wraps publish in `pubsub_route` envelopes
6. Messages hop through mesh to each subscriber

**Verification**:
- All subscribers receive message
- Publish completes within timeout
- No duplicate messages

---

## Troubleshooting

### Services Not Healthy

```bash
# Check container status
docker ps --filter "name=macula-"

# View logs
docker logs macula-bootstrap
docker logs macula-gateway

# Check health endpoints
curl http://localhost:8080/health  # Bootstrap
curl http://localhost:8081/health  # Gateway
```

### DHT Not Populated

```bash
# Check routing table from edge node
docker exec macula-edge2 bin/macula eval "
    {ok, Pid} = whereis(macula_routing_server),
    RoutingTable = macula_routing_server:get_routing_table(Pid),
    io:format('Peers: ~p~n', [length(RoutingTable)]).
"
```

### Service Not Found in DHT

```bash
# Check if service was registered
docker exec macula-edge3 bin/macula eval "
    {ok, Pid} = whereis(macula_routing_server),
    ServiceKey = <<\"test.calculator.add\">>,
    Result = macula_routing_server:get_local(Pid, ServiceKey),
    io:format('Service lookup: ~p~n', [Result]).
"
```

---

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Integration Tests

on: [push, pull_request]

jobs:
  integration-tests:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      - name: Set up Docker
        uses: docker/setup-buildx-action@v2

      - name: Run integration tests
        run: |
          cd macula
          ./docker/test-multi-mode.sh --build --clean

      - name: Upload logs on failure
        if: failure()
        uses: actions/upload-artifact@v3
        with:
          name: docker-logs
          path: |
            docker-compose.log
```

---

## Development Workflow

1. **Make code changes**
2. **Rebuild Docker images**: `./docker/test-multi-mode.sh --build`
3. **Run tests**: `rebar3 ct --suite=test/integration/multi_hop_rpc_SUITE`
4. **Debug**: Access containers with `docker exec -it macula-edge2 bin/macula remote_console`
5. **Clean up**: `docker-compose -f docker/docker-compose.multi-mode.yml down -v`

---

## Next Steps

1. ✅ Implement remaining RPC test cases
2. ⏳ Create `multi_hop_pubsub_SUITE.erl`
3. ⏳ Add performance tests (throughput, latency)
4. ⏳ Add chaos tests (node failures, network partitions)
5. ⏳ Add load tests (1000+ concurrent RPC calls)

---

**Last Updated**: 2025-11-17
**Status**: Integration framework complete, test implementation in progress
