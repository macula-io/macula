# CortexIQ Module Analysis for Macula Mesh Migration

## Executive Summary

**Total Scope**: 101 files requiring changes
**Total Lines of Code**: ~9,200 LOC
**Estimated Effort**: 6-7 weeks (2 engineers)
**Risk Level**: Medium (mitigated by clean architecture)

**Key Finding**: The CortexIQ codebase follows excellent vertical slicing principles. The migration will be surgical - updating communication interfaces without touching business logic.

---

## Module-by-Module Analysis

### 1. MaculaSdk (WAMP Infrastructure) ⚠️ CRITICAL

**Location**: `system/macula_client/`

**Current State**: Implements WAMP client over WebSocket

| File | LOC | Description | Migration |
|------|-----|-------------|-----------|
| `lib/macula_client/wamp/client.ex` | 450 | Core WAMP client GenServer | Replace with Macula Mesh client |
| `lib/macula_client/wamp/connection.ex` | 380 | WebSocket connection management | Replace with HTTP/3/QUIC |
| `lib/macula_client/wamp/protocol.ex` | 320 | WAMP protocol encoding/decoding | Replace with Macula protocol |
| `lib/macula_client/wamp/pool.ex` | 250 | Connection pooling | Adapt for Macula sessions |
| `lib/macula_client/wamp/subscriber.ex` | 100 | Generic subscriber behavior | Minimal changes |

**Total**: 5 files, ~1,500 LOC

**Dependencies**: WebSocket, Jason, Poolboy

**Migration Strategy**:
1. Create new `lib/macula_client/mesh/` directory
2. Implement Macula client wrapping Erlang modules
3. Keep same API surface (Wampex-compatible)
4. Use feature flag to switch between WAMP and Macula

**Code Changes**:

**Before** (`macula_client/wamp/client.ex`):
```elixir
defmodule MaculaSdk.Wamp.Client do
  use GenServer

  def connect(url, realm) do
    {:ok, pid} = GenServer.start_link(__MODULE__, {url, realm})
    {:ok, pid}
  end

  def publish(client, topic, payload) do
    GenServer.call(client, {:publish, topic, payload})
  end

  def init({url, realm}) do
    {:ok, conn} = WebSockex.start_link(url, __MODULE__, %{realm: realm})
    {:ok, %{conn: conn, realm: realm, subscriptions: %{}}}
  end

  # ... WebSocket message handling
end
```

**After** (`macula_client/mesh/client.ex`):
```elixir
defmodule MaculaSdk.Mesh.Client do
  use GenServer

  def connect(realm, bootstrap_nodes) do
    {:ok, pid} = GenServer.start_link(__MODULE__, {realm, bootstrap_nodes})
    {:ok, pid}
  end

  def publish(client, topic, payload) do
    GenServer.call(client, {:publish, topic, payload})
  end

  def init({realm, bootstrap_nodes}) do
    # Create Macula node
    realm_bin = :binary.list_to_bin(realm)
    macula_node = :macula_node.new(realm_bin, %{})
    node_id = :macula_node.get_id(macula_node)

    # Initialize registries
    pubsub_registry = :macula_pubsub_registry.new()
    rpc_registry = :macula_rpc_registry.new()
    routing_table = :macula_routing_table.new(node_id, 20)

    # TODO: Bootstrap DHT from bootstrap_nodes

    {:ok, %{
      node: macula_node,
      pubsub_registry: pubsub_registry,
      rpc_registry: rpc_registry,
      routing_table: routing_table,
      subscriptions: %{}
    }}
  end

  # ... Macula mesh message handling
end
```

**Complexity**: HIGH (foundation for all other changes)
**Estimated Effort**: 1-2 weeks

---

### 2. CortexIQ Homes (Edge Payload)

**Location**: `system/cortex_iq_homes/`

**Current Architecture**:
```
HomeBot (GenServer)
    ↓ Phoenix.PubSub (local)
    ↓
Subscriber Systems (consume local PubSub)
    → WAMP Client (publish to mesh)

Publisher Systems (subscribe to WAMP)
    → Phoenix.PubSub (publish locally)
    → HomeBot receives
```

**Key Insight**: HomeBot is CLEAN - only uses Phoenix.PubSub locally! Migration only touches the subscriber/publisher systems.

#### Subscriber Systems (Subscribe from Mesh)

| File | LOC | Subscribes To | Complexity |
|------|-----|---------------|------------|
| `lib/cortex_iq_homes/subscribe_provider_contract_proposed/system.ex` | 180 | `be.cortexiq.provider.contract.proposed` | MEDIUM |
| `lib/cortex_iq_homes/subscribe_simulation_time_advanced/system.ex` | 135 | `be.cortexiq.simulation.time.advanced` | MEDIUM |
| `lib/cortex_iq_homes/subscribe_spot_price_updated/system.ex` | 110 | `be.cortexiq.provider.spot_price.updated` | LOW |

**Migration Pattern**:
```elixir
# Before (WAMP)
defmodule CortexIqHomes.SubscribeProviderContractProposed.System do
  use GenServer

  def init(_) do
    {:ok, client} = MaculaSdk.Wamp.Client.connect(bondy_url(), "be.cortexiq.energy")

    MaculaSdk.Wamp.Client.subscribe(client, "be.cortexiq.provider.contract.proposed", fn event ->
      handle_contract_proposed(event)
    end)

    {:ok, %{client: client}}
  end
end

# After (Macula Mesh)
defmodule CortexIqHomes.SubscribeProviderContractProposed.System do
  use GenServer

  def init(_) do
    {:ok, client} = MaculaSdk.Mesh.Client.connect("be.cortexiq.energy", bootstrap_nodes())

    MaculaSdk.Mesh.Client.subscribe(client, "be.cortexiq.provider.contract.proposed", fn event ->
      handle_contract_proposed(event)
    end)

    {:ok, %{client: client}}
  end
end
```

**Changes Required**:
- Replace `Wamp.Client` → `Mesh.Client`
- Replace `bondy_url()` → `bootstrap_nodes()`
- **Business logic untouched!**

**Total Subscriber Files**: 7
**Estimated Effort**: 2-3 days

#### Publisher Systems (Publish to Mesh)

| File | LOC | Publishes To | Event Volume |
|------|-----|--------------|--------------|
| `lib/cortex_iq_homes/publish_home_measured/publisher.ex` | 95 | `be.cortexiq.home.measured` | HIGH (5/sec per home) |
| `lib/cortex_iq_homes/publish_home_traded/publisher.ex` | 90 | `be.cortexiq.home.traded` | HIGH (varies) |
| `lib/cortex_iq_homes/publish_home_initialized/publisher.ex` | 75 | `be.cortexiq.home.initialized` | LOW (once per home) |
| `lib/cortex_iq_homes/publish_home_connected/publisher.ex` | 70 | `be.cortexiq.home.connected` | LOW (once per home) |
| `lib/cortex_iq_homes/publish_home_contract_signed/publisher.ex` | 85 | `be.cortexiq.home.contract.signed` | LOW (rare) |

**Migration Pattern**:
```elixir
# Before (WAMP)
defmodule CortexIqHomes.PublishHomeMeasured.Publisher do
  def publish(measurement) do
    MaculaSdk.Wamp.Client.publish(
      :wamp_client,
      "be.cortexiq.home.measured",
      measurement
    )
  end
end

# After (Macula Mesh)
defmodule CortexIqHomes.PublishHomeMeasured.Publisher do
  def publish(measurement) do
    MaculaSdk.Mesh.Client.publish(
      :mesh_client,
      "be.cortexiq.home.measured",
      measurement
    )
  end
end
```

**Total Publisher Files**: 10
**Estimated Effort**: 2-3 days

**Total for Homes Module**: 17 files, ~1,200 LOC, 4-6 days

---

### 3. CortexIQ Utilities (Edge Payload)

**Location**: `system/cortex_iq_utilities/`

| File | LOC | Description | Complexity |
|------|-----|-------------|------------|
| `lib/cortex_iq_utilities/provider_bot.ex` | 620 | Provider business logic | HIGH |
| `lib/cortex_iq_utilities/application.ex` | 50 | App setup (WAMP client) | LOW |
| `lib/cortex_iq_utilities/subscribe_simulation_time_advanced/system.ex` | 30 | Subscribe to time | LOW |

**Critical Component**: `provider_bot.ex`

**Current WAMP Usage**:
1. Publishes contract offers (periodic)
2. Publishes spot prices (periodic)
3. **Registers RPC handler** for contract signing (synchronous!)

**Before**:
```elixir
defmodule CortexIqUtilities.ProviderBot do
  def init(_) do
    {:ok, client} = MaculaSdk.Wamp.Client.connect(bondy_url(), "be.cortexiq.energy")

    # Publish offers periodically
    schedule_publish_offers()

    # Register RPC for contract signing (CRITICAL!)
    MaculaSdk.Wamp.Client.register(client, "be.cortexiq.provider.sign_contract", fn args ->
      home_id = args["home_id"]
      contract_id = args["contract_id"]

      # Business logic: validate and sign contract
      result = sign_contract(home_id, contract_id)

      {:ok, result}
    end)

    {:ok, %{client: client, provider_id: provider_id}}
  end

  defp handle_info(:publish_offers, state) do
    offer = generate_contract_offer()

    MaculaSdk.Wamp.Client.publish(
      state.client,
      "be.cortexiq.provider.contract.proposed",
      offer
    )

    schedule_publish_offers()
    {:noreply, state}
  end
end
```

**After**:
```elixir
defmodule CortexIqUtilities.ProviderBot do
  def init(_) do
    {:ok, client} = MaculaSdk.Mesh.Client.connect("be.cortexiq.energy", bootstrap_nodes())

    schedule_publish_offers()

    # Register RPC for contract signing (CRITICAL!)
    MaculaSdk.Mesh.Client.register(client, "be.cortexiq.provider.sign_contract", fn args ->
      home_id = args["home_id"]
      contract_id = args["contract_id"]

      result = sign_contract(home_id, contract_id)

      {:ok, result}
    end)

    {:ok, %{client: client, provider_id: provider_id}}
  end

  defp handle_info(:publish_offers, state) do
    offer = generate_contract_offer()

    MaculaSdk.Mesh.Client.publish(
      state.client,
      "be.cortexiq.provider.contract.proposed",
      offer
    )

    schedule_publish_offers()
    {:noreply, state}
  end
end
```

**Changes**: Minimal (Wamp → Mesh client swap)
**Complexity**: HIGH (business-critical RPC must be reliable)
**Estimated Effort**: 3-4 days (includes thorough testing of RPC)

---

### 4. CortexIQ Simulation (Hub Service)

**Location**: `system/cortex_iq_simulation/`

| File | LOC | Description | Criticality |
|------|-----|-------------|-------------|
| `lib/cortex_iq_simulation/simulation_clock.ex` | 495 | Broadcasts time 1/sec | ⚠️ CRITICAL |
| `lib/cortex_iq_simulation/pause_simulation/system.ex` | 75 | RPC handler (pause) | MEDIUM |
| `lib/cortex_iq_simulation/resume_simulation/system.ex` | 75 | RPC handler (resume) | MEDIUM |
| `lib/cortex_iq_simulation/reset_simulation/system.ex` | 80 | RPC handler (reset) | MEDIUM |
| `lib/cortex_iq_simulation/set_simulation_speed/system.ex` | 75 | RPC handler (speed) | MEDIUM |

**Critical Component**: `SimulationClock`

**Current Implementation**:
```elixir
defmodule CortexIqSimulation.SimulationClock do
  use GenServer

  @publish_interval 1_000  # Publish every 1 second

  def init(_) do
    {:ok, client} = MaculaSdk.Wamp.Client.connect(bondy_url(), "be.cortexiq.energy")

    schedule_tick()

    {:ok, %{
      client: client,
      current_time: start_time(),
      speed: 105_120,  # 1 real sec = 29.2 sim hours
      paused: false
    }}
  end

  defp handle_info(:tick, state) do
    unless state.paused do
      # Advance simulation time
      new_time = advance_time(state.current_time, state.speed)

      # CRITICAL: Broadcast time to entire mesh (1/sec)
      MaculaSdk.Wamp.Client.publish(
        state.client,
        "be.cortexiq.simulation.time.advanced",
        %{
          simulation_time: new_time,
          speed: state.speed,
          timestamp: System.system_time(:millisecond)
        }
      )

      schedule_tick()
      {:noreply, %{state | current_time: new_time}}
    else
      schedule_tick()
      {:noreply, state}
    end
  end
end
```

**Migration Risk**: This broadcasts 1/sec to ALL nodes (homes, utilities, dashboard, projections). Latency here is critical.

**After**:
```elixir
defmodule CortexIqSimulation.SimulationClock do
  use GenServer

  def init(_) do
    {:ok, client} = MaculaSdk.Mesh.Client.connect("be.cortexiq.energy", bootstrap_nodes())

    schedule_tick()

    {:ok, %{
      client: client,
      current_time: start_time(),
      speed: 105_120,
      paused: false
    }}
  end

  defp handle_info(:tick, state) do
    unless state.paused do
      new_time = advance_time(state.current_time, state.speed)

      # CRITICAL: Broadcast via Macula Mesh
      MaculaSdk.Mesh.Client.publish(
        state.client,
        "be.cortexiq.simulation.time.advanced",
        %{
          simulation_time: new_time,
          speed: state.speed,
          timestamp: System.system_time(:millisecond)
        }
      )

      schedule_tick()
      {:noreply, %{state | current_time: new_time}}
    else
      schedule_tick()
      {:noreply, state}
    end
  end
end
```

**Performance Requirement**: Publish latency < 10ms (vs ~20-30ms WAMP)

**Total for Simulation**: 5 files, ~800 LOC, 2-3 days

---

### 5. CortexIQ Projections (Hub Service)

**Location**: `system/cortex_iq_projections/`

**Architecture**:
- Shared WAMP client pool (1 pool for all projections)
- 22 projection systems (only 2 enabled due to connection load!)
- Each projection subscribes to events and updates database

**Current Pool Setup** (`application.ex`):
```elixir
defmodule CortexIqProjections.Application do
  def start(_type, _args) do
    children = [
      # Shared WAMP connection pool
      {MaculaSdk.Wamp.Pool, [
        name: :projections_wamp_pool,
        size: 5,
        max_overflow: 10,
        url: bondy_url(),
        realm: "be.cortexiq.energy"
      ]},

      # Only 2 enabled due to connection load!
      CortexIqProjections.CalculateSystemTotals.System,
      CortexIqProjections.CalculateCityMeasured.System

      # 20 more disabled...
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
```

**Problem**: Connection pool limits prevent enabling all projections!

**After (Macula Mesh)**:
```elixir
defmodule CortexIqProjections.Application do
  def start(_type, _args) do
    children = [
      # Single Macula Mesh client (shared by all)
      {MaculaSdk.Mesh.Client, [
        name: :projections_mesh_client,
        realm: "be.cortexiq.energy",
        bootstrap_nodes: bootstrap_nodes()
      ]},

      # Enable ALL 22 projections! (Mesh can handle it)
      CortexIqProjections.CalculateSystemTotals.System,
      CortexIqProjections.CalculateCityMeasured.System,
      # ... all 20 others
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
```

**Benefit**: Macula Mesh eliminates connection limits - all 22 projections can run!

#### Projection System Pattern

**Before**:
```elixir
defmodule CortexIqProjections.CalculateSystemTotals.System do
  use GenServer

  def init(_) do
    # Get connection from pool
    client = MaculaSdk.Wamp.Pool.checkout(:projections_wamp_pool)

    MaculaSdk.Wamp.Client.subscribe(client, "be.cortexiq.home.measured", fn event ->
      handle_home_measured(event)
    end)

    {:ok, %{client: client}}
  end

  defp handle_home_measured(event) do
    # Calculate totals
    totals = calculate_totals(event)

    # Update database
    Repo.update(totals)
  end
end
```

**After**:
```elixir
defmodule CortexIqProjections.CalculateSystemTotals.System do
  use GenServer

  def init(_) do
    # Use shared mesh client
    client = :projections_mesh_client

    MaculaSdk.Mesh.Client.subscribe(client, "be.cortexiq.home.measured", fn event ->
      handle_home_measured(event)
    end)

    {:ok, %{client: client}}
  end

  defp handle_home_measured(event) do
    totals = calculate_totals(event)
    Repo.update(totals)
  end
end
```

**Total Projection Systems**: 22 files
**Estimated Effort**: 1 week (repetitive pattern, but many files)

---

### 6. CortexIQ Queries (Hub Service)

**Location**: `system/cortex_iq_queries/`

**Purpose**: RPC server for read queries (dashboard requests data)

| File | LOC | Description |
|------|-----|-------------|
| `lib/cortex_iq_queries/application.ex` | 120 | RPC registration |
| `lib/cortex_iq_queries/home_queries.ex` | 80 | Home data queries |

**Before**:
```elixir
defmodule CortexIqQueries.Application do
  def start(_type, _args) do
    {:ok, client} = MaculaSdk.Wamp.Client.connect(bondy_url(), "be.cortexiq.energy")

    # Register RPC handlers
    MaculaSdk.Wamp.Client.register(client, "be.cortexiq.queries.get_home", fn args ->
      home = CortexIqQueries.HomeQueries.get_home(args["home_id"])
      {:ok, home}
    end)

    MaculaSdk.Wamp.Client.register(client, "be.cortexiq.queries.list_homes", fn _args ->
      homes = CortexIqQueries.HomeQueries.list_homes()
      {:ok, homes}
    end)

    # ... more RPC handlers

    {:ok, self()}
  end
end
```

**After**:
```elixir
defmodule CortexIqQueries.Application do
  def start(_type, _args) do
    {:ok, client} = MaculaSdk.Mesh.Client.connect("be.cortexiq.energy", bootstrap_nodes())

    MaculaSdk.Mesh.Client.register(client, "be.cortexiq.queries.get_home", fn args ->
      home = CortexIqQueries.HomeQueries.get_home(args["home_id"])
      {:ok, home}
    end)

    MaculaSdk.Mesh.Client.register(client, "be.cortexiq.queries.list_homes", fn _args ->
      homes = CortexIqQueries.HomeQueries.list_homes()
      {:ok, homes}
    end)

    {:ok, self()}
  end
end
```

**Complexity**: LOW (straightforward RPC registration)
**Estimated Effort**: 1 day

---

### 7. CortexIQ Dashboard (Hub Service)

**Location**: `system/cortex_iq_dashboard_umbrella/apps/cortex_iq_dashboard/`

**Current Architecture**:
- 12 event subscribers (for real-time visualization)
- 4 additional subscribers (for metrics aggregation)
- 2 RPC clients (simulation control + queries)
- 3 local state aggregators

#### Event Subscribers

| File | LOC | Subscribes To | Updates |
|------|-----|---------------|---------|
| `lib/cortex_iq_dashboard/subscribe_home_measured/subscriber.ex` | 85 | `be.cortexiq.home.measured` | Live chart |
| `lib/cortex_iq_dashboard/subscribe_home_traded/subscriber.ex` | 75 | `be.cortexiq.home.traded` | Trade feed |
| `lib/cortex_iq_dashboard/subscribe_contract_proposed/subscriber.ex` | 70 | `be.cortexiq.provider.contract.proposed` | Contract list |
| `lib/cortex_iq_dashboard/subscribe_simulation_time/subscriber.ex` | 60 | `be.cortexiq.simulation.time.advanced` | Clock display |

**Pattern**:
```elixir
# Before
defmodule CortexIqDashboard.SubscribeHomeMeasured.Subscriber do
  use GenServer

  def init(_) do
    {:ok, client} = MaculaSdk.Wamp.Client.connect(bondy_url(), "be.cortexiq.energy")

    MaculaSdk.Wamp.Client.subscribe(client, "be.cortexiq.home.measured", fn event ->
      # Broadcast to LiveView via Phoenix.PubSub
      Phoenix.PubSub.broadcast(
        CortexIqDashboard.PubSub,
        "dashboard:home_measured",
        {:home_measured, event}
      )
    end)

    {:ok, %{client: client}}
  end
end

# After
defmodule CortexIqDashboard.SubscribeHomeMeasured.Subscriber do
  use GenServer

  def init(_) do
    {:ok, client} = MaculaSdk.Mesh.Client.connect("be.cortexiq.energy", bootstrap_nodes())

    MaculaSdk.Mesh.Client.subscribe(client, "be.cortexiq.home.measured", fn event ->
      Phoenix.PubSub.broadcast(
        CortexIqDashboard.PubSub,
        "dashboard:home_measured",
        {:home_measured, event}
      )
    end)

    {:ok, %{client: client}}
  end
end
```

#### RPC Clients (LiveView Controllers)

**Before**:
```elixir
defmodule CortexIqDashboardWeb.SimulationControlLive do
  def handle_event("pause", _params, socket) do
    # Call RPC to pause simulation
    {:ok, _result} = MaculaSdk.Wamp.Client.call(
      :dashboard_wamp_client,
      "be.cortexiq.simulation.pause",
      %{}
    )

    {:noreply, socket}
  end
end
```

**After**:
```elixir
defmodule CortexIqDashboardWeb.SimulationControlLive do
  def handle_event("pause", _params, socket) do
    {:ok, _result} = MaculaSdk.Mesh.Client.call(
      :dashboard_mesh_client,
      "be.cortexiq.simulation.pause",
      %{}
    )

    {:noreply, socket}
  end
end
```

**Total Dashboard Files**: 20+ files, ~1,800 LOC
**Estimated Effort**: 1 week

---

## Migration Timeline

### Phase 1: Infrastructure (Week 1-2)

**Goal**: Create Macula Mesh client SDK

- [ ] Week 1: Implement `MaculaSdk.Mesh.Client`
  - Core GenServer with Macula Erlang module calls
  - Pub/sub API (publish, subscribe, unsubscribe)
  - RPC API (call, register, unregister)
  - Connection management

- [ ] Week 2: Testing and Documentation
  - Unit tests (100+ tests)
  - Integration tests with Macula mesh
  - API documentation
  - Migration guide

**Deliverable**: `MaculaSdk.Mesh.Client` ready for use

---

### Phase 2: Critical Services (Week 3)

**Goal**: Migrate time-sensitive services

- [ ] Day 1-2: CortexIqSimulation.SimulationClock
  - Migrate clock
  - Performance testing (must maintain 1/sec broadcast)

- [ ] Day 3-4: CortexIqQueries
  - Migrate RPC registration
  - Test all query endpoints

- [ ] Day 5: CortexIqProjections.Application
  - Update pool configuration
  - Enable 2 projections initially

**Deliverable**: Core services on Macula Mesh, others still on WAMP

---

### Phase 3: Producers (Week 4)

**Goal**: Migrate event publishers

- [ ] Day 1-3: CortexIqHomes
  - 7 subscriber systems
  - 10 publisher systems
  - Integration testing

- [ ] Day 4-5: CortexIqUtilities
  - Provider bots
  - Contract signing RPC (thorough testing!)

**Deliverable**: All homes and utilities on Macula Mesh

---

### Phase 4: Consumers (Week 5-6)

**Goal**: Migrate event consumers

- [ ] Week 5: CortexIqProjections
  - Enable all 22 projection systems
  - Load testing (can Macula handle it?)
  - Database performance monitoring

- [ ] Week 6: CortexIqDashboard
  - 12 event subscribers
  - 4 metric subscribers
  - 2 RPC clients
  - End-to-end testing with real data

**Deliverable**: All services on Macula Mesh

---

### Phase 5: Validation and Cleanup (Week 7)

**Goal**: Remove WAMP, production-ready

- [ ] Day 1-2: Remove Bondy deployment
  - Update Kubernetes manifests
  - Remove WAMP client code
  - Clean up configuration

- [ ] Day 3-4: Performance testing
  - Load test (5000+ events/sec)
  - Latency measurements
  - Resource usage profiling

- [ ] Day 5: Documentation and handoff
  - Updated architecture docs
  - Runbooks
  - Troubleshooting guide

**Deliverable**: Production-ready Macula Mesh deployment

---

## Risk Mitigation

### Risk 1: SimulationClock Latency

**Impact**: HIGH (entire system depends on 1/sec time broadcast)

**Mitigation**:
- Performance test Macula publish latency
- Implement timeout monitoring
- Keep WAMP as fallback during Phase 2

---

### Risk 2: High-Volume Topics

**Topics**: `home.measured` (400 homes × 5/sec = 2000 events/sec)

**Mitigation**:
- Load test Macula pub/sub throughput
- Monitor backpressure
- Implement circuit breakers

---

### Risk 3: RPC Reliability

**Critical RPC**: `provider.sign_contract` (synchronous, business-critical)

**Mitigation**:
- Extensive RPC testing
- Implement retries with idempotency
- Monitor RPC latency/errors

---

### Risk 4: Projection Connection Limits

**Current Problem**: Only 2/22 projections enabled due to WAMP connection limits

**Mitigation**:
- Macula Mesh eliminates connection limits
- Enable projections incrementally
- Monitor mesh routing table performance

---

## Success Metrics

| Metric | WAMP Baseline | Macula Target | How to Measure |
|--------|---------------|---------------|----------------|
| Pub/sub latency (local) | 20-30ms | < 10ms | Timestamp diff in event |
| Pub/sub latency (remote) | 50-80ms | < 30ms | Cross-cluster measurement |
| RPC latency | 40-60ms | < 30ms | Dashboard query times |
| Events/sec throughput | 2,000 | 5,000+ | Prometheus metrics |
| Active projections | 2 / 22 (9%) | 22 / 22 (100%) | Configuration |
| Connection count | 50+ (pooling) | 1 per service | Process count |
| Memory usage | ~500MB (Bondy) | ~100MB (embedded) | Pod metrics |

---

## Configuration Changes

### Before (WAMP)

**Environment Variables**:
```bash
BONDY_URL=ws://bondy.macula-system.svc.cluster.local:18080/ws
BONDY_REALM=be.cortexiq.energy
```

**Kubernetes Deployment**:
```yaml
env:
- name: BONDY_URL
  value: "ws://bondy:18080/ws"
- name: BONDY_REALM
  value: "be.cortexiq.energy"
```

---

### After (Macula Mesh)

**Environment Variables**:
```bash
MACULA_REALM=be.cortexiq.energy
MACULA_BOOTSTRAP_NODES=cortex-iq-dashboard-0.cortex-iq-dashboard:4433,cortex-iq-homes-0.cortex-iq-homes:4433
```

**Kubernetes Deployment**:
```yaml
env:
- name: MACULA_REALM
  value: "be.cortexiq.energy"
- name: MACULA_BOOTSTRAP_NODES
  value: "dashboard-0.dashboard:4433,homes-0.homes:4433"
ports:
- containerPort: 4433
  name: mesh-quic
  protocol: UDP
```

---

## Code Change Summary

| Module | Files | LOC Changed | Effort | Risk |
|--------|-------|-------------|--------|------|
| MaculaSdk | 5 | 1,500 | 2 weeks | HIGH |
| CortexIqHomes | 17 | 1,200 | 4-6 days | MEDIUM |
| CortexIqUtilities | 3 | 700 | 3-4 days | HIGH |
| CortexIqSimulation | 5 | 800 | 2-3 days | HIGH |
| CortexIqProjections | 45+ | 3,000 | 1 week | MEDIUM |
| CortexIqQueries | 2 | 200 | 1 day | LOW |
| CortexIqDashboard | 20+ | 1,800 | 1 week | MEDIUM |
| **TOTAL** | **101** | **~9,200** | **6-7 weeks** | **MEDIUM** |

---

## Conclusion

The CortexIQ codebase is well-architected for this migration:

**Strengths**:
1. ✅ Clean vertical slicing (business logic isolated)
2. ✅ Uniform patterns (subscribers/publishers)
3. ✅ Phoenix.PubSub as abstraction layer
4. ✅ Minimal WAMP-specific code

**Challenges**:
1. ⚠️ High-volume topics (2000+ events/sec)
2. ⚠️ Synchronous RPC for contracts
3. ⚠️ Simulation clock timing (1/sec broadcast)

**Recommendation**: Proceed with migration. The clean architecture minimizes risk, and Macula Mesh will enable all 22 projections (currently only 2 run due to WAMP connection limits).

**Expected Outcome**: 20-40% latency improvement, 100% projection utilization, foundation for smartphone P2P.
