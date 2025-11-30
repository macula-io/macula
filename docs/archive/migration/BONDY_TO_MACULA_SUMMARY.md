# Bondy/WAMP to Macula Mesh Migration - Summary

## Overview

This directory contains comprehensive documentation for migrating the CortexIQ Energy Mesh PoC from Bondy/WAMP to Macula HTTP/3 Mesh.

## Documents

### 1. [BONDY_TO_MACULA_MIGRATION.md](./BONDY_TO_MACULA_MIGRATION.md)
**Complete 16-week migration plan**

- 4 phases with decision gates
- Step-by-step tasks and timelines
- Risk assessment and rollback procedures
- Performance expectations
- Budget and resource allocation

**Key Takeaways**:
- 16 weeks, 2-3 engineers
- Incremental migration (zero downtime)
- 3 decision gates for go/no-go
- API compatibility layer (zero code changes initially)

---

### 2. [SMARTPHONE_P2P_ARCHITECTURE.md](./SMARTPHONE_P2P_ARCHITECTURE.md)
**Smartphone peer-to-peer architecture**

- Why WAMP cannot do P2P (broker architecture)
- How Macula Mesh enables direct smartphone → device communication
- iOS and Android implementation examples
- Network scenarios (local WiFi, remote, offline)
- Security model and performance characteristics

**Key Takeaways**:
- 80% battery savings (0-RTT reconnection)
- 10-30ms latency (vs 50-100ms through broker)
- Works offline on local WiFi
- **This is the killer feature** - impossible with WAMP

---

### 3. [CORTEXIQ_MODULE_ANALYSIS.md](./CORTEXIQ_MODULE_ANALYSIS.md)
**Detailed analysis of CortexIQ codebase**

- All 101 files requiring changes
- Module-by-module breakdown
- Code examples (before/after)
- Migration timeline and effort estimates
- Risk mitigation strategies

**Key Takeaways**:
- ~9,200 LOC to change
- 6-7 weeks effort
- Clean architecture minimizes risk
- Currently only 2/22 projections enabled (WAMP connection limits)
- Macula Mesh will enable all 22 projections

---

## Quick Reference

### Migration Phases

| Phase | Duration | Focus | Risk |
|-------|----------|-------|------|
| 1. SDK | 2-3 weeks | Build Elixir wrapper | HIGH |
| 2. Dashboard | 4 weeks | Migrate one service | MEDIUM |
| 3. Full Migration | 5 weeks | All services | MEDIUM |
| 4. Smartphone | 4 weeks | Mobile apps | LOW |

### Code Changes

| Component | Files | LOC | Effort |
|-----------|-------|-----|--------|
| MaculaSdk | 5 | 1,500 | 2 weeks |
| Homes | 17 | 1,200 | 5 days |
| Utilities | 3 | 700 | 4 days |
| Simulation | 5 | 800 | 3 days |
| Projections | 45 | 3,000 | 1 week |
| Queries | 2 | 200 | 1 day |
| Dashboard | 20 | 1,800 | 1 week |
| **Total** | **101** | **9,200** | **6-7 weeks** |

### Performance Targets

| Metric | WAMP | Macula | Improvement |
|--------|------|--------|-------------|
| Latency (local) | 20-30ms | < 10ms | 50-70% |
| Latency (remote) | 50-80ms | 20-40ms | 50% |
| Events/sec | 2,000 | 5,000+ | 150% |
| Projections enabled | 2/22 (9%) | 22/22 (100%) | 1000% |
| Battery (smartphone) | Baseline | -80% | N/A |

## Key Decisions

### ✅ Recommended: Proceed with Migration

**Confidence**: 85% (High)

**Why?**
1. **Strategic**: Macula Mesh IS the product platform
2. **Differentiation**: Smartphone P2P is impossible with WAMP
3. **Technical**: Clean architecture minimizes risk
4. **Performance**: 20-50% latency improvement expected
5. **Scalability**: Eliminates connection pool limits

**When?**
- Start: Q2 2025
- Complete: Q3 2025 (16 weeks)

### Decision Gates

**Gate 1 (Week 3)**: SDK Complete
- Go if: Tests passing, performance < 5ms overhead
- No-go if: Erlang/Elixir interop issues

**Gate 2 (Week 7)**: Dashboard Migrated
- Go if: Stable for 48 hours, latency same or better
- No-go if: Dashboard errors > 1%

**Gate 3 (Week 12)**: Full Migration
- Go if: All services stable, latency improved
- No-go if: System reliability < 99.5%

## Implementation Notes

### API Compatibility

**Before (Wampex)**:
```elixir
{:ok, session} = Wampex.start_link(
  url: "ws://bondy:18080/ws",
  realm: "be.cortexiq.energy"
)

Wampex.publish(session, "topic", payload)
Wampex.subscribe(session, "pattern", callback)
{:ok, result} = Wampex.call(session, "uri", args)
```

**After (MaculaClient)** - Same API!:
```elixir
{:ok, session} = MaculaClient.start_link(
  realm: "be.cortexiq.energy",
  bootstrap_nodes: ["node1:4433"]
)

MaculaClient.publish(session, "topic", payload)
MaculaClient.subscribe(session, "pattern", callback)
{:ok, result} = MaculaClient.call(session, "uri", args)
```

**Zero application code changes** until Phase 3!

### Deployment Changes

**Before**: Separate Bondy deployment (broker)
**After**: Embedded mesh client in each service

```yaml
# Before
- Bondy deployment (3 replicas)
- Services connect to Bondy

# After
- No Bondy
- Each service embeds Macula Mesh
- Headless services for DNS discovery
```

## Risks and Mitigation

### High Risks

1. **Erlang/Elixir Interop Performance**
   - Mitigation: Phase 1 benchmarks, < 5ms target
   - Fallback: NIFs if needed (unlikely)

2. **QUIC Implementation Bugs**
   - Mitigation: Use mature `gun` library
   - Fallback: HTTP/2 if QUIC fails

3. **DHT Bootstrap Failures**
   - Mitigation: Static bootstrap list, health checks
   - Fallback: Retry with exponential backoff

### Medium Risks

4. **Topic Semantics Mismatch**
   - Mitigation: Integration tests, pattern validation

5. **Message Ordering**
   - Mitigation: Document semantics, sequence numbers if needed

6. **Resource Usage Increase**
   - Mitigation: Resource profiling in Phase 2

## Success Criteria

✅ **Phase 1 (SDK)**:
- 100+ tests passing
- Latency overhead < 5ms
- API coverage 100%

✅ **Phase 2 (Dashboard)**:
- Dashboard stable 48 hours
- Error rate < 0.1%
- Latency ≤ baseline

✅ **Phase 3 (Full Migration)**:
- System reliability > 99.95%
- Latency improved 20%+
- Bondy removed
- All 22 projections enabled

✅ **Phase 4 (Smartphone)**:
- iOS + Android apps working
- P2P latency < 100ms
- 80% battery improvement
- Demo ready for investors

## Next Steps

1. **Review Documentation** (This week)
   - Get stakeholder buy-in
   - Finalize timeline

2. **Set Up Project** (Week 1)
   - Create feature branch
   - Set up CI/CD for Elixir SDK
   - Reserve staging cluster

3. **Start Phase 1** (Week 1-3)
   - Implement MaculaClient SDK
   - Write tests
   - Create examples

4. **Decision Gate 1** (Week 3)
   - Review SDK performance
   - Go/no-go decision

## Questions?

For questions or clarifications:
1. Read the detailed migration plan
2. Check smartphone P2P architecture
3. Review module analysis for specific code changes
4. Reach out to Macula platform team

---

**Last Updated**: 2025-11-08
**Version**: 1.0
**Status**: Ready for Review
