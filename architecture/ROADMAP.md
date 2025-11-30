# Macula Roadmap

> **Last Updated:** 2025-11-28
> **Current Version:** v0.11.2
> **Status:** This is a realistic roadmap based on source code analysis

---

## Executive Summary

This roadmap is based on an honest assessment of the codebase as of November 2025. It prioritizes completing partially-implemented features before adding new ones.

**Key Finding:** The documentation claims features that are only partially implemented. This roadmap focuses on completing what exists before expanding scope.

---

## Current State (v0.10.1)

### What Actually Works

| Component | Status | Evidence |
|-----------|--------|----------|
| QUIC Transport | **Working** | `macula_connection.erl` - Full gen_server, QUIC via quicer |
| PubSub (local) | **Working** | 11 files in `macula_pubsub_system/` |
| RPC (local) | **Working** | 9 files in `macula_rpc_system/` |
| DHT Kademlia | **Working** | 6 files in `macula_routing_system/`, k-bucket routing |
| Gateway System | **Working** | 13 files in `macula_gateway_system/` |
| Bootstrap System | **Working** | 4 files in `macula_bootstrap_system/` |
| Memory Management | **Working** | Bounded pools, TTL cleanup |
| Performance Cache | **Working** | Subscriber cache, direct routing |
| TLS Configuration | **Working** | Two-mode system: production/development |
| Hybrid Trust Model | **Working** | Realm auth + TOFU + rate limit + audit (37 tests) |

### What Is Incomplete

| Component | Claimed Status | Actual Status |
|-----------|----------------|---------------|
| Platform Layer | "Raft consensus" | **Single-node only** - Line 177 in `macula_leader_election.erl`: `initial_members => [ServerId], % Single node for now` |
| CRDTs | "LWW, G-Counter, PN-Counter, etc." | **Only LWW-Register** - Others are `%% Future:` comments |
| Distributed CRDTs | "Eventually consistent" | **Local ETS only** - No replication across nodes |
| TLS Verification | **COMPLETED** | Two-mode system: production (strict verification) / development (self-signed) |
| NAT Traversal | "Hole punching" | **Incomplete** - TODOs in detector, only relay works |
| QUIC Distribution | "31 tests passing" | **Not integrated** - Standalone module, not used by mesh |

---

## Architectural Decision: reckon_db as Platform Service (Future)

> **Decision Date:** 2025-11-28
> **Status:** Planned for v0.14.0+
> **Prerequisite:** reckon_db (pure Erlang event store) must be developed first

### Problem

The current Platform Layer uses Ra directly for leader election and local ETS for CRDT storage.
If workloads also use an event store (which uses Khepri/Ra), there would be:
- Two competing Ra clusters on the same BEAM node
- Resource contention (disk I/O, memory, consensus traffic)
- Potential data directory collisions
- Redundant consensus mechanisms

### Solution (Deferred)

Integrate **reckon_db** (pure Erlang rewrite of ex_esdb) as the Platform Layer's storage backend.

**Why reckon_db instead of ex_esdb?**
- Macula is pure Erlang - avoid Elixir dependency
- reckon_db will be a clean, focused implementation
- Ecosystem alignment: reckon_db_gater, reckon_db_commanded adapters

```
┌─────────────────────────────────────────────────────────────┐
│                   Workload Application                       │
├─────────────────────────────────────────────────────────────┤
│           Macula Platform Layer (API Facade)                 │
│  ┌─────────────┬──────────────┬────────────────────────┐    │
│  │   Leader    │    CRDT      │    Service             │    │
│  │  Election   │    State     │    Registry            │    │
│  └──────┬──────┴───────┬──────┴───────────┬────────────┘    │
│         │              │                  │                  │
│         ▼              ▼                  ▼                  │
│  ┌──────────────────────────────────────────────────────┐   │
│  │              reckon_db (Platform Service)             │   │
│  │  Event Streams:                                       │   │
│  │  - $platform.leader_elections                         │   │
│  │  - $platform.crdt_updates                             │   │
│  │  - $platform.node_membership                          │   │
│  │                        │                              │   │
│  │                    Khepri/Ra                          │   │
│  │              (Single Consensus Layer)                 │   │
│  └──────────────────────────────────────────────────────┘   │
├─────────────────────────────────────────────────────────────┤
│                  Macula Mesh (Transport)                     │
│         QUIC/HTTP3  │  DHT  │  PubSub  │  RPC               │
└─────────────────────────────────────────────────────────────┘
```

### Benefits (When Implemented)

| Benefit | Description |
|---------|-------------|
| Single Consensus | One Ra cluster (via Khepri) for all coordination |
| Event Sourced Platform | Leader elections, CRDTs become auditable events |
| Replay & Recovery | Platform state can be rebuilt from event log |
| Pure Erlang | No Elixir dependency in Macula core |
| Ecosystem Parity | reckon_db_gater, reckon_db_commanded alignment |

### Prerequisites

Before Platform Layer integration (v0.14.0+):
1. **reckon_db** - Pure Erlang event store (rewrite of ex_esdb)
2. **reckon_db_gater** - Gateway/proxy for reckon_db
3. **reckon_db_commanded** - Commanded adapter (optional, Elixir workloads)

### Current State (v0.10.1)

Platform Layer works for **single-node demos**:
- Leader election: Single-node Ra cluster (functional but not distributed)
- CRDTs: Local ETS storage (not replicated)
- Sufficient for development and testing

**No changes needed until reckon_db is ready.**

---

## Priority 1: Security First

### v0.11.0 - Security Hardening

**Goal:** Fix critical security gaps before any production use.

**Rationale:** TLS verification is more urgent than Platform Layer distribution.
The current Platform Layer works for demos; insecure TLS does not work for anything.

#### P1.1 TLS Certificate Verification (COMPLETED - Nov 2025)

**Status:** Complete

**Implementation:**
- Created `macula_tls.erl` - centralized TLS configuration module
- Two TLS modes: `production` (strict verification) and `development` (self-signed)
- Environment variable configuration: `MACULA_TLS_MODE`, `MACULA_TLS_CACERTFILE`, etc.

**Completed:**
- [x] Add CA bundle configuration option
- [x] Implement certificate chain verification
- [x] Add hostname verification
- [x] Provide self-signed cert generation for development (`scripts/setup-dev-tls.sh`)
- [x] Document TLS configuration in operator guide (`docs/operator/TLS_CONFIGURATION.md`)

**Files Modified:**
- `src/macula_tls.erl` (new - centralized TLS config)
- `src/macula_connection.erl` (uses macula_tls)
- `src/macula_connection_pool.erl` (uses macula_tls)
- `src/macula_dist.erl` (uses macula_tls)
- `config/sys.config` (TLS options)
- `scripts/setup-dev-tls.sh` (new - cert generation)
- `docs/operator/TLS_CONFIGURATION.md` (new - documentation)
- `test/macula_tls_tests.erl` (29 tests passing)

**Acceptance Criteria Met:**
- Production mode rejects invalid certificates
- Development mode allows self-signed with explicit opt-in
- Hostname verification validates CN/SAN against connection hostname

#### P1.2 Hybrid Trust Model (COMPLETED - Nov 2025)

**Status:** Complete (Core Implementation)

**Problem Solved:** TLS certificates required on every node creates friction for plug-and-play mesh.

**Solution:** Hybrid Trust Model with three levels:
- **Level 1**: Realm Authentication (API key validation)
- **Level 2**: Certificate Trust (TOFU within authenticated realm)
- **Level 3**: Optional CA-signed certificates for seed nodes

**Implementation:**
- Created `macula_realm_trust.erl` - realm-scoped trust management
- TOFU (Trust On First Use) pattern for certificate fingerprints
- Fingerprint storage with realm scoping
- Trust revocation support
- 24 tests passing

**Completed:**
- [x] Architecture Decision Record (`architecture/decisions/ADR-001-HYBRID_TRUST_MODEL.md`)
- [x] Realm authentication API (`authenticate/2`)
- [x] Fingerprint registration (`register_fingerprint/3`)
- [x] TOFU verification (`verify_fingerprint/3`)
- [x] Trusted peers query (`get_trusted_peers/1`)
- [x] Trust revocation (`revoke_trust/2`)
- [x] Certificate fingerprint extraction (`extract_fingerprint/1`)

**Files Added:**
- `src/macula_realm_trust.erl` (new - 200+ LOC)
- `test/macula_realm_trust_tests.erl` (new - 24 tests)
- `architecture/decisions/ADR-001-HYBRID_TRUST_MODEL.md` (new - ADR)

**Completed (v0.11.2):**
- [x] Rate limiting per realm (`init_rate_limiter/0`, `reset_rate_limit/1`)
- [x] Audit logging for authentication events (auth_success, auth_failure, tofu_trust, fingerprint_mismatch, trust_revoked)
- [x] 37 tests passing (13 new tests for DHT, rate limiting, audit logging)

**Remaining (Future):**
- [ ] DHT integration for fingerprint storage (currently mock ETS - production uses bootstrap_registry)
- [ ] macula_tls integration for automatic TOFU verification

---

## Priority 2: Core Improvements

### v0.12.0 - DHT Improvements

#### P2.1 DHT Request/Response Pattern

**Current:** `macula_gateway_dht.erl` uses fire-and-forget.

**Required:**
- [ ] Add correlation IDs to DHT queries
- [ ] Implement timeout and retry logic
- [ ] Add response tracking for observability

**Files Affected:**
- `src/macula_gateway_system/macula_gateway_dht.erl`

#### P2.2 K-Bucket Splitting

**Current:** `macula_routing_table.erl` - buckets have max capacity but don't split.

**Required:**
- [ ] Implement bucket splitting when full
- [ ] Maintain routing table invariants
- [ ] Add routing table health metrics

**Files Affected:**
- `src/macula_routing_system/macula_routing_table.erl`

---

### v0.13.0 - NAT Traversal (Optional)

**Note:** Only implement if targeting edge deployment. Gateway relay works for cloud deployments.

#### P3.1 Complete NAT Detection

**Current:** `macula_nat_detector.erl` has TODO stubs.

**Required:**
- [ ] Implement NAT_PROBE message handling
- [ ] Detect NAT type (EI, HD, PP, PC, RD)
- [ ] Cache NAT type per connection

#### P3.2 STUN/TURN Support

**Required:**
- [ ] Add STUN client for address discovery
- [ ] Integrate TURN relay as fallback
- [ ] Hole punch coordination protocol

**Acceptance Criteria:**
- 80%+ success rate for EI/PP NAT types
- Automatic fallback to relay for restrictive NAT

---

## Priority 3: Ecosystem Contributions

### v1.0.0 - Production Ready

**Goal:** Complete feature set for production use.

#### Checklist
- [ ] All P1 and P2 items complete
- [ ] E2E test suite with multi-node scenarios
- [ ] Production deployment guide
- [ ] Monitoring and alerting documentation
- [ ] Performance benchmarks documented

---

### v1.1.0+ - QUIC Distribution (Ecosystem Contribution)

**Note:** This is a BEAM ecosystem contribution, not core Macula functionality. Only pursue after v1.0.0 is proven in production.

**Current State:** `macula_dist.erl` exists but is standalone.

**Why Defer:**
1. Macula mesh works fine over HTTP/3 without this
2. Requires TLS verification to be complete first
3. Integration with Ra cluster requires multi-node Platform Layer
4. Better to contribute after proving the core works

**When Ready:**
- [ ] Integrate with existing Macula discovery
- [ ] Add libcluster strategy
- [ ] Test with Horde, Mnesia, :pg
- [ ] Publish as separate hex package
- [ ] Submit to OTP for consideration

---

## Technical Debt

### Code Cleanup

| Issue | Priority | Location |
|-------|----------|----------|
| Remove TODO comments | Low | Various |
| Fix test harness instability | Medium | Tests cancel unexpectedly |
| Update documentation to match reality | High | GETTING_STARTED.md done, others pending |

### Testing

| Area | Current | Target |
|------|---------|--------|
| Unit tests | 44 passing (unstable) | 100+ stable |
| Integration tests | Minimal | Multi-node scenarios |
| E2E tests | None | Full workflow tests |

---

## Version Timeline

| Version | Focus | Prerequisites |
|---------|-------|---------------|
| v0.11.0 | Security Hardening (TLS) | None |
| v0.12.0 | DHT Improvements | v0.11.0 |
| v0.13.0 | NAT Traversal (optional) | v0.11.0 |
| v0.14.0 | Platform Layer + reckon_db | reckon_db ready |
| v1.0.0 | Production Ready | v0.11.0, v0.12.0 |
| v1.1.0+ | QUIC Distribution | v1.0.0 proven |

### External Dependencies

| Component | Status | Repository |
|-----------|--------|------------|
| **reckon_db** | Planned | Pure Erlang event store (rewrite of ex_esdb) |
| **reckon_db_gater** | Planned | Gateway/proxy for reckon_db |
| **reckon_db_commanded** | Planned | Commanded adapter (Elixir workloads) |

**Note:** Platform Layer integration (v0.14.0) is blocked until reckon_db ecosystem is ready.

---

## Archived Documents

Previous planning documents are preserved in `architecture/archive/planning-2025-11/`:
- `ROADMAP.md` - Original 107KB roadmap
- `v0.8.0-ROADMAP.md` - v0.8.0 planning
- `v1.0.0-ROADMAP.md` - v1.0.0 planning
- `PLATFORM_VISION.md` - Platform vision document
- `v0.11.0-QUIC_DISTRIBUTION.md` - QUIC distribution vision
- Others...

---

## References

- Source code analysis: 102 `.erl` files, 52 test files
- TODOs found: ~10 critical, ~20 minor
- Test status: 44 passing (harness unstable)

---

**Document Version:** 2.0 (Reality-based)
**Author:** Source code analysis, November 2025
