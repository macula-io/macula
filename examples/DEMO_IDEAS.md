# Macula Demo Ideas

This document tracks potential demonstration scenarios for the Macula platform, ordered by priority and implementation complexity.

## 🎯 Implemented Demos

### ✅ Chat Demo (`examples/chat_demo/`)
**Status**: Complete
**Scenario**: Two users chatting through firewalls
**Demonstrates**: Peer-to-peer pub/sub, NAT traversal, real-time messaging
**Complexity**: Simple

---

## 📋 Planned Demos

### 1️⃣ IoT Sensor Network (Priority: HIGH)
**Status**: In Progress
**Complexity**: Simple (~200 lines)
**Timeline**: Week 1

**Scenario**: Multiple IoT sensors publishing data, central dashboard consuming
```
Sensors (Home 1) → Macula Mesh ← Sensors (Home 2)
                        ↓
                   Dashboard (Office)
```

**What it demonstrates:**
- Many-to-one pub/sub pattern
- Sensors behind home routers (NAT)
- Real-time data aggregation
- Multiple realms (different buildings)
- Time-series data streaming

**Files:**
- `iot_sensors_demo/sensor.erl` - Publishes temperature/humidity/light
- `iot_sensors_demo/dashboard.erl` - Subscribes and displays live data
- `iot_sensors_demo/README.md` - Setup instructions

**Use Cases:**
- Smart home monitoring
- Industrial sensor networks
- Environmental monitoring
- Building management systems

---

### 2️⃣ Multiplayer Game (Priority: HIGH)
**Complexity**: Medium (~300 lines)
**Timeline**: Week 2

**Scenario**: Simple turn-based game (rock-paper-scissors or tic-tac-toe)
```
Player 1 ← Macula Mesh → Player 2
           ↓
      Spectators
```

**What it demonstrates:**
- Low-latency messaging
- Shared state synchronization
- Multiple subscribers (spectators)
- Interactive and fun!

**Files:**
- `game_demo/player.erl` - Interactive game client
- `game_demo/spectator.erl` - Watch-only mode
- `game_demo/README.md`

**Use Cases:**
- Online gaming
- Real-time collaboration
- Interactive applications

---

### 3️⃣ Distributed Task Queue (Priority: HIGH)
**Complexity**: Medium (~400 lines)
**Timeline**: Week 3

**Scenario**: Job producers and workers across different networks
```
Producer (Laptop) → [job.created] → Workers (Cloud + Home Office)
Workers → [job.completed] → Producer
```

**What it demonstrates:**
- RPC + pub/sub combination
- Workers behind firewalls
- Load balancing without central broker
- Real-world CI/CD or data processing

**Files:**
- `task_queue_demo/producer.erl` - Submit jobs
- `task_queue_demo/worker.erl` - Process jobs
- `task_queue_demo/monitor.erl` - Watch progress
- `task_queue_demo/README.md`

**Use Cases:**
- Distributed computing
- CI/CD pipelines
- Video/image processing
- ETL workflows

---

### 4️⃣ Service Discovery & RPC (Priority: MEDIUM)
**Complexity**: Medium (~350 lines)
**Timeline**: Week 4

**Scenario**: Microservices discovering and calling each other
```
User Service ─RPC─> Auth Service
     │                    │
     └─────> Payment Service
```

**What it demonstrates:**
- DHT-based service discovery
- RPC between services
- No service registry needed
- Microservices through firewalls

**Files:**
- `microservices_demo/auth_service.erl`
- `microservices_demo/user_service.erl`
- `microservices_demo/payment_service.erl`
- `microservices_demo/client.erl`
- `microservices_demo/README.md`

**Use Cases:**
- Microservices architecture
- Service mesh alternative
- Edge computing services

---

### 5️⃣ Real-Time Monitoring Dashboard (Priority: MEDIUM)
**Complexity**: Medium (~300 lines)
**Timeline**: Later

**Scenario**: Distributed apps reporting metrics, central monitoring
```
App Instances (Various Networks) → Metrics → Monitor
                                      ↓
                                   Alerts
```

**What it demonstrates:**
- Time-series data streaming
- Multiple publishers, single subscriber
- Alert routing
- DevOps use case

**Files:**
- `monitoring_demo/app_instance.erl` - Publishes metrics
- `monitoring_demo/monitor.erl` - Displays metrics + alerts
- `monitoring_demo/README.md`

**Use Cases:**
- Application monitoring
- Infrastructure observability
- DevOps dashboards

---

### 6️⃣ File Sharing P2P (Priority: LOW)
**Complexity**: Advanced (~600 lines)
**Timeline**: Future

**Scenario**: Direct file transfer between peers through firewalls
```
Alice (file.pdf) ──chunks──> Bob
                              └─> Progress updates
```

**What it demonstrates:**
- Large data transfer over QUIC
- Chunked streaming
- Progress tracking
- Direct P2P (impressive!)

**Files:**
- `file_share_demo/sender.erl`
- `file_share_demo/receiver.erl`
- `file_share_demo/README.md`

**Use Cases:**
- P2P file sharing
- Content distribution
- Backup systems

---

### 7️⃣ Multi-Tenant SaaS Backend (Priority: LOW)
**Complexity**: Medium (~250 lines)
**Timeline**: Future

**Scenario**: Show realm isolation for different customers
```
Customer A Apps ← realm: customer-a → Macula
Customer B Apps ← realm: customer-b → Macula
```

**What it demonstrates:**
- Strong isolation (realms)
- SaaS multi-tenancy pattern
- Enterprise use case

**Files:**
- `multitenant_demo/tenant_app.erl`
- `multitenant_demo/admin.erl`
- `multitenant_demo/README.md`

**Use Cases:**
- SaaS platforms
- Multi-tenant applications
- Enterprise isolation

---

### 8️⃣ Event Sourcing Demo (Priority: LOW)
**Complexity**: Advanced (~500 lines)
**Timeline**: Future

**Scenario**: Show event-driven architecture patterns
```
Command → Aggregate → Events → Projections → Read Model
```

**What it demonstrates:**
- Event sourcing pattern
- CQRS architecture
- How to build event-driven apps on Macula

**Files:**
- `event_sourcing_demo/command_handler.erl`
- `event_sourcing_demo/event_store.erl`
- `event_sourcing_demo/projection.erl`
- `event_sourcing_demo/README.md`

**Use Cases:**
- Event-driven architecture
- CQRS systems
- Audit trails
- Time travel debugging

---

## 🎬 Implementation Strategy

**Phase 1 - Core Patterns** (Weeks 1-4):
1. IoT Sensors (many-to-one pub/sub)
2. Multiplayer Game (many-to-many pub/sub)
3. Task Queue (RPC + pub/sub)
4. Service Discovery (DHT + RPC)

**Phase 2 - Advanced Use Cases** (Later):
5. Monitoring Dashboard
6. File Sharing P2P
7. Multi-Tenant SaaS
8. Event Sourcing

---

## 📊 Demo Comparison Matrix

| Demo | Complexity | Wow Factor | Enterprise Value | Lines of Code |
|------|------------|------------|------------------|---------------|
| Chat | Simple | Medium | Medium | ~100 |
| IoT Sensors | Simple | High | High | ~200 |
| Game | Medium | Very High | Low | ~300 |
| Task Queue | Medium | Medium | Very High | ~400 |
| Service Discovery | Medium | Medium | Very High | ~350 |
| Monitoring | Medium | Medium | High | ~300 |
| File Sharing | Advanced | Very High | Medium | ~600 |
| Multi-Tenant | Medium | Low | Very High | ~250 |
| Event Sourcing | Advanced | Medium | Very High | ~500 |

---

## 💡 Notes

- All demos should be self-contained in `examples/` directory
- Each demo should have clear README with architecture diagrams
- Use consistent naming: `{name}_demo/`
- Include setup scripts where helpful
- Focus on demonstrating ONE key capability clearly
- Keep code simple and well-commented for learning

---

**Last Updated**: 2025-11-10
**Next Demo**: IoT Sensors Network
