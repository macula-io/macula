# Macula Documentation Audit - 2025-11-17

## Executive Summary

**PROBLEM**: Multiple conflicting roadmaps and unclear gateway responsibilities are causing confusion about what v0.7.x actually is and what v0.8.0 should be.

**IMPACT**:
- Developers unsure whether to use gateway relay or DHT-routed pub/sub
- Conflicting statements about what's implemented vs planned
- Documentation suggests features exist that may not work correctly

## Document Inventory

### Roadmaps (2 documents - CONFLICT!)

1. **`macula_http3_mesh_roadmap.md`**
   - 20-week roadmap (Weeks 1-20)
   - Phase 4 (Weeks 13-16): WAMP Layer (Pub/Sub, RPC)
   - Phase 5 (Weeks 17-20): Production Hardening
   - **Says Phase 4 is complete** (✅ marks)
   - **Does NOT mention v0.7 vs v0.8 distinction**
   - Implies current implementation = Phase 4 complete

2. **`NAT_TRAVERSAL_ROADMAP.md`**
   - v0.8.0: Opportunistic Hole Punching (80% direct P2P)
   - v0.9.0: Full STUN/TURN/ICE (95% direct P2P)
   - 6-8 weeks for v0.8.0, 8-10 weeks for v0.9.0
   - **Focused only on NAT traversal**

3. **`ARCHITECTURE_EVOLUTION.md`**
   - **Says v0.7.4 is "not yet a mesh"**
   - Calls current architecture "centralized message relay"
   - v0.8.0 = "Architectural shift to hybrid mesh (P2P begins)"
   - v0.9.0 = "Architectural completion to full mesh"
   - **CONFLICTS with roadmap.md which says mesh features are done**

### Gateway Documents (6 documents - SCATTERED!)

1. **`macula_http3_mesh_gateway_ops.md`** - Operational guide
2. **`gateway_behavior_catalog.md`** - Behavior analysis
3. **`gateway_integration_plan.md`** - Integration planning
4. **`gateway_refactoring_phase1_summary.md`** - Refactoring history
5. **`gateway_refactoring_plan.md`** - Refactoring design
6. **`macula_gateway_behaviors.md`** - More behavior documentation

**PROBLEM**: Gateway responsibilities split across 6 files. No single source of truth for "what is a gateway for?"

### DHT/Routing Documents (7 documents - OVERLAPPING!)

1. **`dht_routed_rpc.md`** - DHT-routed RPC design (marked IN PROGRESS in CLAUDE.md)
2. **`dht_rpc_implementation_status.md`** - Implementation tracking
3. **`dht_rpc_reply_implementation.md`** - Reply mechanism
4. **`dht_routed_pubsub.md`** - DHT-routed pub/sub (says v0.7.8!)
5. **`hybrid_routing_strategy.md`** - Hybrid approach?
6. **`pubsub_optimization_recommendations.md`** - Performance tuning
7. **`dht_cache_performance_guide.md`** - Caching strategies

**PROBLEM**: Unclear which routing strategy is current, which is planned, which is implemented.

## Key Conflicts Identified

### Conflict 1: What is v0.7.x?

**`macula_http3_mesh_roadmap.md` says:**
> Phase 4 Success Criteria:
> - ✅ Pub/Sub Works
> - ✅ RPC Works
> - ✅ WAMP Compatible

**`ARCHITECTURE_EVOLUTION.md` says:**
> Macula v0.7.4 is **not yet a mesh** - it's a centralized message relay with a DHT-based routing table. The mesh functionality is **planned for v0.8.0+**.

**`dht_routed_pubsub.md` says:**
> Solution: Multi-Hop DHT Routing (v0.7.8)

**Which is it?**

### Conflict 2: Gateway Role

**Some docs say:** Gateway is just bootstrap + DHT participation
**Other docs say:** Gateway routes all pub/sub messages
**Code reality:** Gateway has `macula_gateway_pubsub_router.erl` - clearly routing!

**No single doc clearly states:**
- What MUST go through gateway (bootstrap, realm bridging?)
- What SHOULD be P2P (intra-realm pub/sub, RPC?)
- What is current behavior vs future plan?

### Conflict 3: DHT-Routed Pub/Sub Status

**`dht_routed_pubsub.md` says:** Implemented in v0.7.8
**Code reality:** `macula_pubsub_routing.erl` exists with `wrap_publish/4`
**Macula-arcade reality:** Matchmaking broken, peers can't find each other

**Is it implemented? Is it enabled? Is it working?**

## Recommendations

### 1. Create Single Authoritative Roadmap

**File:** `ROADMAP.md` (new, top-level)

Content should clearly state:
- **v0.7.x Current State**: Gateway relay + DHT routing infrastructure
- **v0.8.x Goal**: NAT traversal + opportunistic P2P
- **v0.9.x Goal**: Full mesh with STUN/TURN

Archive old roadmaps to `architecture/archive/`

### 2. Create Gateway Responsibilities Doc

**File:** `GATEWAY_RESPONSIBILITIES.md` (new)

Clear sections:
- **MUST do**: Bootstrap, realm bridging, DHT participation
- **SHOULD NOT do**: Intra-realm message routing (future P2P)
- **CURRENT behavior**: Hub-and-spoke relay (temporary)
- **FUTURE behavior**: Bootstrap-only (v0.8+)

Consolidate 6 gateway docs into this + archive old ones.

### 3. Create Routing Strategy Doc

**File:** `ROUTING_STRATEGY.md` (new)

Clear timeline:
- **v0.7.x**: Gateway relay (current)
- **v0.7.8**: DHT-routed pub/sub infrastructure (code exists, not used?)
- **v0.8.x**: Enable DHT routing + add NAT traversal
- **v0.9.x**: Full P2P mesh

Archive conflicting routing docs.

### 4. Update CLAUDE.md

Remove contradictions, point to new authoritative docs:
- Link to ROADMAP.md for version planning
- Link to GATEWAY_RESPONSIBILITIES.md for gateway role
- Link to ROUTING_STRATEGY.md for routing behavior

## Action Items

- [ ] Create ROADMAP.md (consolidate 3 conflicting roadmaps)
- [ ] Create GATEWAY_RESPONSIBILITIES.md (consolidate 6 gateway docs)
- [ ] Create ROUTING_STRATEGY.md (consolidate 7 routing/DHT docs)
- [ ] Update CLAUDE.md to reference new docs
- [ ] Archive old conflicting docs to `architecture/archive/`
- [ ] Test and document what actually works in v0.7.28

## Questions to Answer First

Before writing new docs, we need to know:

1. **Does DHT-routed pub/sub actually work in v0.7.28?**
   - Code exists (`macula_pubsub_routing.erl`)
   - Is it enabled?
   - Why is matchmaking broken?

2. **What is the ACTUAL current architecture?**
   - Gateway relay only?
   - DHT routing available but not used?
   - Hybrid?

3. **What is the ACTUAL plan for v0.8.0?**
   - Just NAT traversal?
   - Or also enabling DHT routing?
   - Or both?

## Next Steps

**Immediate**: Answer the 3 questions above by:
1. Reading current code to understand what's implemented
2. Testing what actually works
3. Talking to you to clarify the vision

**Then**: Write the 3 new authoritative docs based on reality + vision.
