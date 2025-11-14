# Multi-Node DHT Testing Guide

This guide covers multiple approaches for testing Macula's DHT-based service discovery across multiple nodes.

## Quick Comparison

| Approach | Speed | Realism | Ease | Best For |
|----------|-------|---------|------|----------|
| **Local Erlang Nodes** | ⚡⚡⚡ Fast | 🌐 Low | ✅ Easy | Quick validation, debugging |
| **Docker Compose** | ⚡⚡ Medium | 🌐🌐 Medium | ✅ Easy | Isolated testing, CI/CD |
| **K3s (beam cluster)** | ⚡ Slow | 🌐🌐🌐 High | 🔧 Moderate | Production-like testing |
| **Manual beam*.lab** | ⚡ Slow | 🌐🌐🌐 High | 🔧🔧 Complex | Real hardware testing |

---

## Option 1: Local Erlang Distributed Nodes (Recommended First)

**Pros**: Fastest, easy debugging, see all logs in real-time
**Cons**: Not testing real network separation

### Run the Test

```bash
cd /home/rl/work/github.com/macula-io/macula
chmod +x scripts/test-multi-node.sh
./scripts/test-multi-node.sh
```

### What It Does

1. **Node 1 (Provider)**: Advertises `test.calculator` service
2. **Node 2 (Consumer)**: Discovers and calls the service via DHT
3. **Node 3 (Second Provider)**: Also advertises `test.calculator` (multi-provider scenario)

All nodes run on localhost with different Erlang node names.

### Expected Output

```
=== Multi-Node DHT Service Discovery Test ===

Compiling Macula...
Starting Node 1 (Service Provider)...
Starting Node 2 (Service Consumer)...
Starting Node 3 (Second Service Provider)...

All nodes started!

=== Node 1 Log (Provider) ===
Node 1 started: 'macula_node1@127.0.0.1'
Node 1 connected
Published service test.calculator to DHT
Node 1 advertised service 'test.calculator'
Scheduled re-advertisement for test.calculator in 240 seconds

=== Node 2 Log (Consumer) ===
Node 2 started: 'macula_node2@127.0.0.1'
Node 2 connected
Node 2 calling service 'test.calculator' (discovering via DHT)...
Found 2 providers for service test.calculator in DHT
[Node 1 Handler] Received call with args: #{operation => <<"add">>, a => 10, b => 5}
Node 2 SUCCESS! Received: #{result => 15, node => <<"node-1">>}

=== Node 3 Log (Second Provider) ===
Node 3 started: 'macula_node3@127.0.0.1'
Node 3 connected
Published service test.calculator to DHT
Node 3 advertised service 'test.calculator'
```

### Debugging

View individual logs:
```bash
tail -f /tmp/macula_node1.log
tail -f /tmp/macula_node2.log
tail -f /tmp/macula_node3.log
```

Stop all nodes:
```bash
pkill -f "macula_node"
```

---

## Option 2: Docker Compose

**Pros**: Isolated containers, closer to production, good for CI/CD
**Cons**: Slower than local nodes, requires Docker

### Prerequisites

```bash
# Ensure Docker and Docker Compose are installed
docker --version
docker-compose --version
```

### Run the Test

```bash
cd /home/rl/work/github.com/macula-io/macula
chmod +x scripts/test-multi-node-docker.sh
./scripts/test-multi-node-docker.sh
```

### What It Does

- Builds a Docker image with Macula compiled
- Starts 3 containers on a Docker bridge network (172.20.0.0/16)
- Each container runs an Erlang node with Macula
- Node 1 & 3 advertise `math.add` service
- Node 2 discovers and calls it via DHT

### View Container Logs

```bash
docker logs macula_node1
docker logs macula_node2
docker logs macula_node3
```

### Cleanup

```bash
docker-compose -f docker-compose.test.yml down
```

---

## Option 3: K3s on beam00.lab (Production-Like)

**Pros**: Real k3s cluster, production-like environment, tests GitOps workflow
**Cons**: Slower, requires access to beam cluster, more complex

### Prerequisites

```bash
# Ensure you can reach beam00.lab
ping -c 1 beam00.lab

# Ensure kubeconfig exists
ls ~/.kube/beam-clusters/beam00.yaml

# Ensure local registry is accessible
curl http://registry.macula.local:5000/v2/_catalog
```

### Run the Test

```bash
cd /home/rl/work/github.com/macula-io/macula
chmod +x scripts/test-multi-node-k3s.sh
./scripts/test-multi-node-k3s.sh
```

### What It Does

1. Builds Docker image with cache-bust
2. Tags as `registry.macula.local:5000/macula-test:latest`
3. Pushes to local registry
4. Creates namespace `macula-test` on beam00
5. Deploys 3 pods (node1, node2, node3)
6. Waits for pods to start
7. Shows logs from all pods

### Monitor the Test

```bash
# Watch pod status
kubectl --kubeconfig ~/.kube/beam-clusters/beam00.yaml get pods -n macula-test -w

# Follow logs
kubectl --kubeconfig ~/.kube/beam-clusters/beam00.yaml logs -f macula-node1 -n macula-test
kubectl --kubeconfig ~/.kube/beam-clusters/beam00.yaml logs -f macula-node2 -n macula-test
kubectl --kubeconfig ~/.kube/beam-clusters/beam00.yaml logs -f macula-node3 -n macula-test
```

### Cleanup

```bash
kubectl --kubeconfig ~/.kube/beam-clusters/beam00.yaml delete namespace macula-test
```

---

## Option 4: Manual Deployment to beam*.lab Hardware

**Pros**: Real hardware, real network, most realistic testing
**Cons**: Most complex, manual setup, requires SSH access

### Setup

1. **Build Release**:
```bash
cd /home/rl/work/github.com/macula-io/macula
rebar3 as prod tar
```

2. **Copy to beam nodes**:
```bash
# Copy to beam00
scp _build/prod/rel/macula/macula-*.tar.gz rl@beam00.lab:/tmp/

# Copy to beam01
scp _build/prod/rel/macula/macula-*.tar.gz rl@beam01.lab:/tmp/

# Copy to beam02
scp _build/prod/rel/macula/macula-*.tar.gz rl@beam02.lab:/tmp/
```

3. **Extract and start on each node**:

**On beam00.lab**:
```bash
ssh rl@beam00.lab
cd /tmp
mkdir -p macula-test
tar -xzf macula-*.tar.gz -C macula-test
cd macula-test
./bin/macula start
./bin/macula remote_console
```

Then in the console:
```erlang
{ok, Client} = macula_client:connect(<<"https://beam00.lab:9443">>, #{
  realm => <<"com.test.beam">>,
  node_id => <<"beam00-node">>
}),

Handler = fun(Args) ->
  #{a := A, b := B} = Args,
  {ok, #{result => A + B, node => <<"beam00">>}}
end,

macula_client:advertise(Client, <<"math.add">>, Handler, #{ttl => 300}).
```

**On beam01.lab**:
```bash
ssh rl@beam01.lab
cd /tmp/macula-test
./bin/macula start
./bin/macula remote_console
```

Then:
```erlang
{ok, Client} = macula_client:connect(<<"https://beam01.lab:9443">>, #{
  realm => <<"com.test.beam">>,
  node_id => <<"beam01-node">>
}),

%% Discover and call service
macula_client:call(Client, <<"math.add">>, #{a => 50, b => 7}).
```

---

## Recommended Testing Sequence

### Phase 1: Quick Validation
```bash
./scripts/test-multi-node.sh
```
- Verify DHT publish/query works
- Check re-advertisement timers
- Confirm multi-provider discovery
- Debug any issues with easy log access

### Phase 2: Containerized Testing
```bash
./scripts/test-multi-node-docker.sh
```
- Verify network isolation works
- Test with containerized environment
- Ensure QUIC/HTTP3 works across containers

### Phase 3: Production-Like Testing
```bash
./scripts/test-multi-node-k3s.sh
```
- Test on real k3s cluster
- Verify registry workflow
- Check pod networking
- Monitor resource usage

### Phase 4: Real Hardware (Optional)
- Manual deployment to beam00-03
- Test across physical nodes
- Measure real-world latency
- Validate NAT traversal

---

## What to Verify

For each test approach, check:

### ✅ Service Advertisement
- [ ] Service published to DHT successfully
- [ ] Re-advertisement timer scheduled
- [ ] Re-advertisement happens before TTL expiry
- [ ] Multiple providers can advertise same service

### ✅ Service Discovery
- [ ] Consumer finds provider via DHT query
- [ ] Discovery result cached (60s TTL)
- [ ] Cache hit on subsequent calls
- [ ] Multiple providers returned from DHT

### ✅ RPC Execution
- [ ] Call reaches provider
- [ ] Handler executes successfully
- [ ] Response returned to caller
- [ ] Error handling works

### ✅ Re-advertisement
- [ ] Timer fires at correct interval
- [ ] Service re-published to DHT
- [ ] Next timer scheduled
- [ ] Continues indefinitely until unadvertise

### ✅ Cleanup
- [ ] Unadvertise removes from DHT
- [ ] Timer cancelled on unadvertise
- [ ] No orphaned processes or timers

---

## Troubleshooting

### Issue: "DHT not available"

**Symptom**: Logs show `Failed to publish service X to DHT: dht_not_available`

**Cause**: `macula_routing_server` not started or not registered

**Fix**:
```erlang
%% Ensure DHT server is started
{ok, DhtPid} = macula_routing_server:start_link(),
register(macula_routing_server, DhtPid).
```

### Issue: "Service not found in DHT"

**Symptom**: Consumer gets `{error, service_not_found}`

**Cause**: Service not yet propagated to DHT, or DHT network partitioned

**Fix**:
- Wait a few seconds after advertise
- Check provider logs to confirm DHT publish succeeded
- Verify all nodes connected to same realm

### Issue: "No providers found"

**Symptom**: DHT query returns `{ok, []}`

**Cause**: TTL expired, or service never advertised

**Fix**:
- Check re-advertisement timer is firing
- Verify TTL > 60 seconds
- Confirm service still advertised locally

### Issue: Containers can't reach each other

**Symptom**: Docker containers timeout or connection refused

**Cause**: QUIC/UDP port not exposed or network misconfigured

**Fix**:
```yaml
# In docker-compose.yml
ports:
  - "9443:9443/udp"  # Ensure UDP protocol
```

### Issue: K3s pods crash

**Symptom**: Pods in `CrashLoopBackOff`

**Cause**: Application startup failure, missing dependencies

**Fix**:
```bash
# Check pod logs
kubectl --kubeconfig ~/.kube/beam-clusters/beam00.yaml logs macula-node1 -n macula-test

# Check pod events
kubectl --kubeconfig ~/.kube/beam-clusters/beam00.yaml describe pod macula-node1 -n macula-test
```

---

## Performance Metrics to Collect

During testing, gather:

- **DHT Query Latency**: Time from `query_dht_for_service` to response
- **Cache Hit Ratio**: `cache_hits / total_calls`
- **Re-advertisement Reliability**: Timer fires at expected intervals
- **Multi-provider Selection**: Which provider chosen for each call
- **Network Latency**: Round-trip time for RPC calls
- **Memory Usage**: ETS table sizes, process counts

---

## Next Steps After Testing

Once multi-node testing validates the DHT integration:

1. **Provider Selection Strategies**
   - Implement round-robin
   - Add random selection
   - Consider least-loaded algorithm

2. **Failover Logic**
   - Retry failed providers
   - Remove failed providers from cache
   - Re-query DHT on all failures

3. **Performance Optimization**
   - Tune cache TTL
   - Adjust re-advertisement interval
   - Optimize DHT replication (K-value)

4. **Production Deployment**
   - GitOps manifests
   - Monitoring and alerting
   - Load testing

---

**Last Updated**: 2025-01-10
**Status**: Ready for multi-node testing
