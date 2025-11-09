# Bondy/WAMP to Macula Mesh Migration Plan

## Executive Summary

**Project**: Replace Bondy/WAMP with Macula HTTP/3 Mesh in CortexIQ Energy Mesh PoC
**Duration**: 16 weeks (4 phases)
**Team Size**: 2-3 engineers
**Risk Level**: Medium (mitigated by incremental approach)
**Go/No-Go Decision Points**: 3 gates (Weeks 3, 7, 12)

**Strategic Rationale**: Macula Mesh IS the platform product. CortexIQ demonstrates platform capabilities. This migration validates production readiness and enables smartphone P2P (impossible with WAMP).

**Success Metrics**:
- Latency: < 30ms (vs ~50ms WAMP baseline)
- Reliability: 99.95% uptime
- Battery efficiency: 80% improvement (smartphone clients)
- Zero downtime migration

---

## Current Architecture Analysis

### Bondy/WAMP Stack (Before)

```
┌─────────────────────────────────────────────────────────────┐
│ CortexIQ Application Layer (Elixir/Phoenix)                 │
├─────────────────────────────────────────────────────────────┤
│ • cortex_iq_dashboard_web (Phoenix LiveView)                │
│ • cortex_iq_homes (GenServers - 400 home bots)              │
│ • cortex_iq_utilities (GenServers - 12 provider bots)       │
│ • cortex_iq_projections (Event aggregation)                 │
│ • cortex_iq_queries (RPC services)                          │
│ • cortex_iq_simulation (Time clock)                         │
└─────────────────────────────────────────────────────────────┘
                            ↓ WAMP Client (Wampex)
┌─────────────────────────────────────────────────────────────┐
│ Bondy Router (Erlang/OTP)                                   │
├─────────────────────────────────────────────────────────────┤
│ • WebSocket transport                                        │
│ • Pub/Sub broker                                            │
│ • RPC router                                                │
│ • Realm: be.cortexiq.energy                                 │
└─────────────────────────────────────────────────────────────┘
                            ↓ WebSocket
┌─────────────────────────────────────────────────────────────┐
│ Kubernetes Network (KinD)                                    │
├─────────────────────────────────────────────────────────────┤
│ • 1 Hub Cluster (bondy + dashboard + projections + queries) │
│ • 4 Edge Clusters (homes + utilities distributed)           │
└─────────────────────────────────────────────────────────────┘
```

**WAMP Protocol Usage**:
- `PUBLISH`: Homes/utilities publish measurements, contract offers
- `SUBSCRIBE`: Dashboard/projections subscribe to event streams
- `CALL`: Queries call into homes/utilities for data
- `REGISTER`: Homes/utilities register RPC handlers

**Topic Examples**:
```
be.cortexiq.home.measured
be.cortexiq.provider.contract.offered
be.cortexiq.simulation.time.advanced
```

**Current Pain Points**:
1. Bondy is a single point of failure (even with clustering)
2. WebSocket doesn't handle mobile network switches well
3. All traffic flows through broker (unnecessary latency)
4. Cannot do smartphone P2P
5. Dependency on external project (Bondy development pace)

---

## Target Architecture (After)

### Macula Mesh Stack

```
┌─────────────────────────────────────────────────────────────┐
│ CortexIQ Application Layer (Elixir/Phoenix)                 │
├─────────────────────────────────────────────────────────────┤
│ • cortex_iq_dashboard_web (Phoenix LiveView)                │
│ • cortex_iq_homes (GenServers)                              │
│ • cortex_iq_utilities (GenServers)                          │
│ • cortex_iq_projections                                     │
│ • cortex_iq_queries                                         │
│ • cortex_iq_simulation                                      │
└─────────────────────────────────────────────────────────────┘
                            ↓ MaculaClient (Elixir SDK)
┌─────────────────────────────────────────────────────────────┐
│ Macula Mesh Core (Erlang/OTP - embedded)                    │
├─────────────────────────────────────────────────────────────┤
│ • :macula_core - Node/Realm/ID management                   │
│ • :macula_routing - Kademlia DHT                            │
│ • :macula_pubsub - Topic pattern matching                   │
│ • :macula_rpc - Service registry                            │
│ • :macula_protocol - MessagePack encoding                   │
│ • :macula_quic - HTTP/3 transport                           │
│ • :macula_membership - SWIM gossip                          │
└─────────────────────────────────────────────────────────────┘
                            ↓ HTTP/3/QUIC
┌─────────────────────────────────────────────────────────────┐
│ Kubernetes Mesh Network                                      │
├─────────────────────────────────────────────────────────────┤
│ • Each pod is a mesh node (no separate broker!)             │
│ • DHT routing table bootstrapped from K8s DNS               │
│ • Direct pod-to-pod communication                           │
│ • Self-healing topology                                     │
└─────────────────────────────────────────────────────────────┘
                            ↓ HTTP/3/QUIC
┌─────────────────────────────────────────────────────────────┐
│ Smartphone Clients (Optional)                                │
├─────────────────────────────────────────────────────────────┤
│ • React Native app (iOS/Android)                            │
│ • Direct P2P to homes/utilities                             │
│ • Native HTTP/3 support (URLSession/Cronet)                 │
└─────────────────────────────────────────────────────────────┘
```

**Key Architectural Changes**:
1. **No Broker**: Each service embeds Macula Mesh client
2. **Peer-to-Peer**: DHT routing enables direct communication
3. **HTTP/3/QUIC**: Modern transport with 0-RTT, connection migration
4. **Self-Healing**: SWIM membership detects failures automatically
5. **Smartphone-Ready**: Mobile devices join as first-class mesh nodes

---

## Phase-by-Phase Migration Plan

### Phase 1: Foundation (Weeks 1-3)

**Goal**: Create Elixir SDK with API compatibility

**Deliverables**:
1. `macula_client_ex` - Elixir wrapper library
2. API compatibility layer (matches Wampex API)
3. Integration tests
4. Documentation

**Tasks**:

**Week 1: Project Setup**
- [ ] Create `apps/macula_client_ex` in Macula monorepo
- [ ] Define Elixir API matching Wampex interface
- [ ] Set up CI/CD for Elixir tests
- [ ] Create ExDoc documentation site

**Week 2: Core API Implementation**
- [ ] `MaculaClient.connect/1` - Session management
- [ ] `MaculaClient.publish/3` - Pub/sub publishing
- [ ] `MaculaClient.subscribe/3` - Topic subscriptions
- [ ] `MaculaClient.call/3` - RPC client
- [ ] `MaculaClient.register/3` - RPC server

**Week 3: Testing & Documentation**
- [ ] Unit tests (target: 100 tests)
- [ ] Integration tests with Erlang Macula modules
- [ ] Example application (mini sensor demo)
- [ ] API documentation
- [ ] Performance benchmarks vs Wampex

**Success Criteria (Gate 1)**:
- ✅ All API functions implemented
- ✅ 100+ passing tests
- ✅ Latency < 5ms (local Erlang module calls)
- ✅ Example app working

**Go/No-Go Decision**: If tests fail or performance is poor, evaluate Erlang/Elixir interop bottlenecks.

---

### Phase 2: Dashboard Migration (Weeks 4-7)

**Goal**: Migrate Dashboard only, keep other services on WAMP

**Architecture**:
```
Dashboard (Macula) ← Bridge → Bondy ← WAMP → Homes/Utilities
```

**Deliverables**:
1. Macula-enabled Dashboard deployment
2. WAMP ↔ Macula bridge service
3. Parallel deployment (A/B testing)
4. Monitoring/observability

**Tasks**:

**Week 4: Bridge Service**
- [ ] Create `cortex_iq_bridge` service
- [ ] Subscribe to WAMP topics → Publish to Macula
- [ ] Subscribe to Macula topics → Publish to WAMP
- [ ] Topic mapping configuration
- [ ] Health checks

**Week 5: Dashboard Migration**
- [ ] Replace Wampex with MaculaClient in dashboard
- [ ] Update `cortex_iq_dashboard/lib/cortex_iq_dashboard/wamp_subscriber.ex`
- [ ] Update environment configuration
- [ ] Create Dockerfile with Macula embedded
- [ ] K8s deployment manifests

**Week 6: Testing**
- [ ] Functional tests (dashboard still receives all events)
- [ ] Load testing (same throughput as WAMP baseline)
- [ ] Latency measurements
- [ ] UI regression testing

**Week 7: Production Rollout**
- [ ] Deploy to staging cluster
- [ ] A/B test (50% traffic to Macula dashboard)
- [ ] Monitor metrics (latency, errors, resource usage)
- [ ] Full rollout if successful

**Success Criteria (Gate 2)**:
- ✅ Dashboard functional with Macula
- ✅ All real-time metrics updating correctly
- ✅ Latency < 50ms (same as WAMP baseline)
- ✅ Zero dashboard errors in 48-hour soak test

**Go/No-Go Decision**: If dashboard shows degraded performance, investigate bridge bottlenecks. May need to optimize message routing.

**Rollback Plan**: Switch K8s deployment back to WAMP-based dashboard image.

---

### Phase 3: Full Migration (Weeks 8-12)

**Goal**: Migrate all services, remove Bondy entirely

**Services to Migrate**:
1. `cortex_iq_homes` (Week 8-9)
2. `cortex_iq_utilities` (Week 9-10)
3. `cortex_iq_projections` (Week 10)
4. `cortex_iq_queries` (Week 11)
5. `cortex_iq_simulation` (Week 11)
6. Remove bridge + Bondy (Week 12)

**Tasks**:

**Week 8-9: Homes Migration**
- [ ] Update `cortex_iq_homes/lib/cortex_iq_homes/application.ex`
  - Replace Wampex session with MaculaClient
  - Update supervision tree
- [ ] Update all publish calls (home.measured, home.contract.signed)
- [ ] Update subscribe patterns (provider.contract.offered)
- [ ] Deploy to edge-01 cluster (100 homes)
- [ ] Monitor for 48 hours
- [ ] Deploy to remaining edge clusters

**Week 9-10: Utilities Migration**
- [ ] Update `cortex_iq_utilities/lib/cortex_iq_utilities/application.ex`
- [ ] Migrate publish calls (provider.contract.offered)
- [ ] Migrate RPC registrations (query endpoints)
- [ ] Deploy incrementally (3 providers at a time)

**Week 10: Projections Migration**
- [ ] Update `cortex_iq_projections/lib/cortex_iq_projections/calculate_system_totals/system.ex`
- [ ] Migrate subscribe patterns (all event types)
- [ ] Update aggregation logic (no changes expected)
- [ ] Deploy to hub cluster

**Week 11: Queries & Simulation Migration**
- [ ] Update `cortex_iq_queries` RPC registrations
- [ ] Update `cortex_iq_simulation` time broadcast
- [ ] Deploy to hub cluster

**Week 12: Cleanup**
- [ ] Remove `cortex_iq_bridge` service
- [ ] Remove Bondy deployment from K8s
- [ ] Update all documentation
- [ ] Update GitOps manifests
- [ ] Clean up old WAMP configuration

**Success Criteria (Gate 3)**:
- ✅ All services on Macula Mesh
- ✅ Bondy completely removed
- ✅ System stability > 99.95% over 1 week
- ✅ Latency improved by 20-40% (no broker hop)
- ✅ Zero data loss during migration

**Go/No-Go Decision**: If any service shows instability, roll back that service to WAMP via bridge. Investigate root cause before proceeding.

**Rollback Plan**:
- Keep Bondy deployment available (scaled to 0)
- Keep bridge service available (scaled to 0)
- Can scale up and revert any service to WAMP within 5 minutes

---

### Phase 4: Smartphone Demo (Weeks 13-16)

**Goal**: Demonstrate smartphone P2P capability

**Deliverables**:
1. React Native demo app (iOS + Android)
2. Smartphone mesh node implementation
3. Direct smartphone → home communication
4. Demo video for marketing

**Tasks**:

**Week 13: Mobile SDK**
- [ ] Create React Native binding for Macula
- [ ] HTTP/3 transport via native modules
  - iOS: URLSession with HTTP/3
  - Android: Cronet library
- [ ] Basic connect/disconnect
- [ ] NAT traversal testing

**Week 14: Demo App UI**
- [ ] Home energy dashboard screen
- [ ] Real-time energy production/consumption charts
- [ ] Contract switching interface
- [ ] Direct messaging with homes

**Week 15: P2P Integration**
- [ ] Subscribe to home.measured events (direct P2P)
- [ ] Call home RPC endpoints (accept_contract)
- [ ] Latency measurements
- [ ] Offline mode (local WiFi P2P)

**Week 16: Polish & Demo**
- [ ] UI polish
- [ ] Performance optimization
- [ ] Demo video production
- [ ] Blog post: "Smartphone P2P Energy Trading"
- [ ] Marketing materials

**Success Criteria**:
- ✅ Smartphone app connects to mesh
- ✅ Real-time updates from homes (< 100ms latency)
- ✅ Direct P2P communication verified (no broker)
- ✅ Works on both iOS and Android
- ✅ Demo ready for investors/customers

---

## Technical Implementation Details

### API Compatibility Layer

**Wampex (Before)**:
```elixir
{:ok, session} = Wampex.start_link(
  url: "ws://bondy.macula-system.svc.cluster.local:18080/ws",
  realm: "be.cortexiq.energy"
)

Wampex.publish(session, "be.cortexiq.home.measured", [payload])

Wampex.subscribe(session, "be.cortexiq.provider.contract.offered", fn event ->
  handle_contract_offer(event)
end)

{:ok, result} = Wampex.call(session, "be.cortexiq.home.get_status", [args])
```

**MaculaClient (After - Same API!)**:
```elixir
{:ok, session} = MaculaClient.start_link(
  realm: "be.cortexiq.energy",
  bootstrap_nodes: ["dashboard.macula-hub.svc.cluster.local:4433"]
)

MaculaClient.publish(session, "be.cortexiq.home.measured", [payload])

MaculaClient.subscribe(session, "be.cortexiq.provider.contract.offered", fn event ->
  handle_contract_offer(event)
end)

{:ok, result} = MaculaClient.call(session, "be.cortexiq.home.get_status", [args])
```

**Key Point**: Application code remains identical. Only configuration changes.

---

### Kubernetes Deployment Changes

**Before (Bondy)**:
```yaml
# Separate Bondy deployment
apiVersion: apps/v1
kind: Deployment
metadata:
  name: bondy
spec:
  replicas: 3
  template:
    spec:
      containers:
      - name: bondy
        image: registry.macula.local:5000/bondy:latest
        ports:
        - containerPort: 18080  # WebSocket
---
# Dashboard connects to Bondy
apiVersion: apps/v1
kind: Deployment
metadata:
  name: cortex-iq-dashboard
spec:
  template:
    spec:
      containers:
      - name: dashboard
        env:
        - name: BONDY_URL
          value: "ws://bondy.macula-system.svc.cluster.local:18080/ws"
```

**After (Macula Mesh)**:
```yaml
# No separate Bondy deployment - mesh embedded in each service!

# Dashboard embeds Macula Mesh
apiVersion: apps/v1
kind: Deployment
metadata:
  name: cortex-iq-dashboard
spec:
  template:
    spec:
      containers:
      - name: dashboard
        image: registry.macula.local:5000/cortex-iq-dashboard:macula
        env:
        - name: MACULA_REALM
          value: "be.cortexiq.energy"
        - name: MACULA_BOOTSTRAP_NODES
          value: "cortex-iq-dashboard-0.cortex-iq-dashboard.macula-hub.svc.cluster.local:4433,cortex-iq-homes-0.cortex-iq-homes.macula-edge-01.svc.cluster.local:4433"
        ports:
        - containerPort: 4433  # QUIC mesh port
---
# Headless service for DHT peer discovery
apiVersion: v1
kind: Service
metadata:
  name: cortex-iq-dashboard
spec:
  clusterIP: None  # Headless
  selector:
    app: cortex-iq-dashboard
  ports:
  - port: 4433
    name: mesh-quic
```

**Key Changes**:
1. No separate broker deployment
2. Each service gets QUIC port (4433)
3. Headless services for DNS-based peer discovery
4. Bootstrap nodes configured via env vars

---

### Code Changes by Module

#### 1. Dashboard: `cortex_iq_dashboard/lib/cortex_iq_dashboard/wamp_subscriber.ex`

**Before**:
```elixir
defmodule CortexIqDashboard.WampSubscriber do
  use GenServer

  def init(_) do
    {:ok, session} = Wampex.start_link(
      url: System.get_env("BONDY_URL"),
      realm: "be.cortexiq.energy"
    )

    Wampex.subscribe(session, "be.cortexiq.home.measured", fn event ->
      handle_home_measured(event)
    end)

    {:ok, %{session: session}}
  end
end
```

**After**:
```elixir
defmodule CortexIqDashboard.WampSubscriber do
  use GenServer

  def init(_) do
    {:ok, session} = MaculaClient.start_link(
      realm: "be.cortexiq.energy",
      bootstrap_nodes: parse_bootstrap_nodes()
    )

    MaculaClient.subscribe(session, "be.cortexiq.home.measured", fn event ->
      handle_home_measured(event)
    end)

    {:ok, %{session: session}}
  end

  defp parse_bootstrap_nodes do
    System.get_env("MACULA_BOOTSTRAP_NODES", "")
    |> String.split(",")
    |> Enum.map(&String.trim/1)
  end
end
```

**Changes**: 3 lines changed (Wampex → MaculaClient)

#### 2. Homes: `cortex_iq_homes/lib/cortex_iq_homes/application.ex`

**Before**:
```elixir
def start(_type, _args) do
  children = [
    {Wampex, [
      url: System.get_env("BONDY_URL"),
      realm: "be.cortexiq.energy",
      name: :wamp_session
    ]},
    {CortexIqHomes.HomeSupervisor, []}
  ]

  Supervisor.start_link(children, strategy: :one_for_one)
end
```

**After**:
```elixir
def start(_type, _args) do
  children = [
    {MaculaClient, [
      realm: "be.cortexiq.energy",
      bootstrap_nodes: parse_bootstrap_nodes(),
      name: :macula_session
    ]},
    {CortexIqHomes.HomeSupervisor, []}
  ]

  Supervisor.start_link(children, strategy: :one_for_one)
end
```

**Changes**: 2 lines changed

#### 3. Homes: `cortex_iq_homes/lib/cortex_iq_homes/home_bot.ex`

**Before**:
```elixir
def handle_info(:publish_measurement, state) do
  Wampex.publish(:wamp_session, "be.cortexiq.home.measured", [
    %{
      home_id: state.home_id,
      production_w: state.production,
      consumption_w: state.consumption
    }
  ])

  {:noreply, state}
end
```

**After**:
```elixir
def handle_info(:publish_measurement, state) do
  MaculaClient.publish(:macula_session, "be.cortexiq.home.measured", [
    %{
      home_id: state.home_id,
      production_w: state.production,
      consumption_w: state.consumption
    }
  ])

  {:noreply, state}
end
```

**Changes**: 1 line changed (Wampex → MaculaClient)

**Total Code Changes Across All Services**: ~50-100 lines (mostly find/replace)

---

## Risk Assessment & Mitigation

### High Risks

**Risk 1: Erlang/Elixir Interop Performance**
- **Impact**: High (could block entire migration)
- **Probability**: Low (Erlang/Elixir share same VM)
- **Mitigation**:
  - Phase 1 includes performance benchmarks
  - Target: < 5ms overhead vs direct Erlang calls
  - Use NIFs if needed (unlikely)

**Risk 2: QUIC Implementation Bugs**
- **Impact**: High (connectivity failures)
- **Probability**: Medium (QUIC is newer than TCP)
- **Mitigation**:
  - Use mature `gun` HTTP/3 client
  - Extensive integration testing
  - Fallback to HTTP/2 if QUIC fails
  - TLS 1.3 required (already in use)

**Risk 3: DHT Bootstrap Failures**
- **Impact**: High (mesh won't form)
- **Probability**: Low (K8s DNS is reliable)
- **Mitigation**:
  - Static bootstrap node list
  - Health checks on bootstrap nodes
  - Exponential backoff retry
  - Alert if > 50% nodes can't join mesh

### Medium Risks

**Risk 4: Topic Semantics Mismatch**
- **Impact**: Medium (message routing errors)
- **Probability**: Low (pattern matching is similar)
- **Mitigation**:
  - Comprehensive integration tests
  - Log all subscribe patterns during migration
  - Validate pattern matching behavior

**Risk 5: Message Ordering Guarantees**
- **Impact**: Medium (could cause state inconsistencies)
- **Probability**: Low (pub/sub is best-effort in both)
- **Mitigation**:
  - Document ordering semantics
  - Use sequence numbers in payloads if needed
  - Idempotent event handlers

**Risk 6: Resource Usage Increase**
- **Impact**: Medium (higher K8s costs)
- **Probability**: Medium (embedded mesh vs separate broker)
- **Mitigation**:
  - Resource profiling in Phase 2
  - Set memory limits per pod
  - Monitor CPU/memory usage

### Low Risks

**Risk 7: Team Learning Curve**
- **Impact**: Low (delays timeline)
- **Probability**: Medium (new technology)
- **Mitigation**:
  - Training sessions (DHT, QUIC, Mesh concepts)
  - Documentation and examples
  - Pair programming during Phase 1

---

## Rollback Procedures

### Phase 2 Rollback (Dashboard Only)
**Trigger**: Dashboard errors > 1% OR latency > 100ms
**Procedure**:
1. Scale up old dashboard deployment
2. Route traffic via K8s service switch
3. Scale down Macula dashboard
4. Keep bridge running
**Time**: < 5 minutes
**Data Loss**: None

### Phase 3 Rollback (Partial Services)
**Trigger**: Service errors > 0.5% OR stability < 99.5%
**Procedure**:
1. Scale up Bondy deployment
2. Scale up bridge service
3. Revert specific service to WAMP-based image
4. Update K8s service to point to old deployment
**Time**: < 10 minutes per service
**Data Loss**: None (events are ephemeral)

### Phase 3 Rollback (Complete)
**Trigger**: Multiple services failing OR mesh instability
**Procedure**:
1. Scale up full Bondy cluster (3 replicas)
2. Revert all services to WAMP-based images
3. Update GitOps manifests to previous commit
4. Flux reconciles within 2 minutes
**Time**: < 15 minutes
**Data Loss**: None

---

## Performance Expectations

### Latency Improvements

| Scenario | WAMP (Baseline) | Macula (Target) | Improvement |
|----------|----------------|-----------------|-------------|
| Pub/Sub (same cluster) | 5-10ms | 2-5ms | 50% |
| Pub/Sub (cross-cluster) | 20-30ms | 10-15ms | 50% |
| RPC (same cluster) | 10-15ms | 5-8ms | 40% |
| RPC (cross-cluster) | 40-60ms | 20-30ms | 50% |
| Dashboard update | 50-80ms | 30-50ms | 38% |
| Smartphone P2P | N/A (impossible) | 20-40ms | ∞ |

**Explanation**: Eliminating broker hop reduces latency by ~50%. HTTP/3 0-RTT helps on reconnects.

### Resource Usage

| Metric | WAMP (Baseline) | Macula (Estimate) | Change |
|--------|----------------|-------------------|--------|
| Bondy CPU | 2 cores | 0 (removed) | -2 cores |
| Bondy Memory | 4GB | 0 (removed) | -4GB |
| Per-service overhead | 50MB (WAMP client) | 80MB (embedded mesh) | +30MB |
| Total Memory (5 services) | 4.25GB | 400MB | -90% |
| Network bandwidth | 10 Mbps | 10 Mbps | 0% |

**Explanation**: Removing dedicated broker saves more resources than embedding mesh client in each service.

### Scalability

| Dimension | WAMP (Baseline) | Macula (Target) | Improvement |
|-----------|----------------|-----------------|-------------|
| Max homes | 1,000 | 10,000+ | 10x |
| Max events/sec | 5,000 | 50,000+ | 10x |
| Mesh nodes | N/A | 1,000+ | N/A |
| Smartphone clients | 0 (not supported) | 10,000+ | ∞ |

---

## Success Metrics & KPIs

### Phase 1 (SDK)
- ✅ 100+ passing tests
- ✅ API coverage: 100%
- ✅ Latency overhead: < 5ms
- ✅ Documentation completeness: 100%

### Phase 2 (Dashboard)
- ✅ Dashboard error rate: < 0.1%
- ✅ Latency: < 50ms (same as baseline)
- ✅ Uptime: > 99.9%
- ✅ Bridge throughput: > 5,000 events/sec

### Phase 3 (Full Migration)
- ✅ System reliability: > 99.95%
- ✅ Latency improvement: > 20%
- ✅ Zero data loss
- ✅ Bondy removed: Yes

### Phase 4 (Smartphone)
- ✅ Smartphone app working: Yes
- ✅ P2P latency: < 100ms
- ✅ Battery efficiency: > 80% improvement
- ✅ iOS + Android: Both working

---

## Budget & Resource Allocation

### Team Composition
- 1 Senior Elixir/Erlang Engineer (Phases 1-3)
- 1 DevOps Engineer (Phases 2-3)
- 1 Mobile Engineer (Phase 4)

### Time Allocation
- Phase 1: 120 hours (3 weeks × 40 hours)
- Phase 2: 160 hours (4 weeks × 40 hours)
- Phase 3: 200 hours (5 weeks × 40 hours)
- Phase 4: 160 hours (4 weeks × 40 hours)
- **Total**: 640 hours (~4 person-months)

### Infrastructure Costs
- Staging cluster: $200/month × 4 months = $800
- Testing tools (load testing): $500
- **Total**: ~$1,300

---

## Decision Gates

### Gate 1 (Week 3): SDK Validation
**Go Criteria**:
- All API functions work
- Tests passing (> 95%)
- Performance acceptable (< 5ms overhead)
- Team confident in Elixir/Erlang interop

**No-Go Actions**:
- Investigate performance bottlenecks
- Consider FFI/NIFs if needed
- Re-evaluate feasibility (delay 2 weeks)

---

### Gate 2 (Week 7): Dashboard Migration
**Go Criteria**:
- Dashboard stable for 48 hours
- Latency same or better than WAMP
- No errors in production logs
- Bridge handling load correctly

**No-Go Actions**:
- Roll back dashboard to WAMP
- Optimize bridge performance
- Re-test for 1 more week

---

### Gate 3 (Week 12): Full Migration
**Go Criteria**:
- All services on Macula
- System reliability > 99.95%
- Latency improved by > 20%
- No data loss or corruption

**No-Go Actions**:
- Roll back problematic services
- Keep Bondy as fallback
- Extend timeline by 4 weeks

---

## Final Recommendation

**PROCEED with migration starting Q2 2025**

**Confidence Level**: 85% (High)

**Key Success Factors**:
1. Incremental migration (reduces risk)
2. API compatibility layer (zero code changes initially)
3. 3 decision gates (can abort if needed)
4. Rollback plans at each phase
5. Strong strategic rationale (smartphone P2P)

**This migration is not just technical debt payoff - it's the foundation of Macula's competitive advantage as a mesh-native IoT platform.**

---

## Appendix A: References

- Macula HTTP/3 Mesh: `/home/rl/work/github.com/macula-io/macula`
- CortexIQ Energy Mesh PoC: `/home/rl/work/github.com/macula-io/macula-energy-mesh-poc`
- WAMP Protocol Spec: https://wamp-proto.org
- HTTP/3 RFC: https://www.rfc-editor.org/rfc/rfc9114
- QUIC RFC: https://www.rfc-editor.org/rfc/rfc9000
- Kademlia DHT Paper: https://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf

## Appendix B: Glossary

- **DHT**: Distributed Hash Table (Kademlia routing)
- **WAMP**: Web Application Messaging Protocol
- **QUIC**: Quick UDP Internet Connections (HTTP/3 transport)
- **RPC**: Remote Procedure Call
- **Pub/Sub**: Publish-Subscribe messaging pattern
- **0-RTT**: Zero Round-Trip Time (QUIC fast reconnection)
- **P2P**: Peer-to-Peer (direct communication)
- **NAT**: Network Address Translation
