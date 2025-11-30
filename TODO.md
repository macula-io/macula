# Macula TODO List

> **Last Updated:** 2025-11-28
> **Current Version:** v0.10.1
> **Status:** Prioritized based on source code analysis

This document tracks known limitations and planned improvements, organized by priority.

---

## Critical: Documentation vs Reality Gaps

These items represent features that are **documented as working** but are **incomplete or single-node only**:

| Issue | Claimed | Actual | Priority |
|-------|---------|--------|----------|
| Platform Layer | Distributed Raft | Single-node only | **P0** |
| CRDTs | Replicated state | Local ETS only | **P0** |
| TLS Verification | Secure transport | `{verify, none}` | **P0** |

---

## v0.11.0 - Security Hardening

> **Priority:** TLS verification is more urgent than Platform Layer distribution.
> The current Platform Layer works for demos; insecure TLS does not work for anything.

### P0: TLS Certificate Verification

**Locations:**
- `src/macula_connection.erl:115` - `{verify, none}`
- `src/macula_connection_pool.erl:88` - `{verify, none}`
- `src/macula_dist_system/macula_dist.erl:381` - `{verify, none}`

**Problem:** All TLS connections accept any certificate, vulnerable to MITM attacks.

**Required Changes:**
- [ ] Add CA bundle configuration option
- [ ] Implement certificate chain verification
- [ ] Add hostname verification
- [ ] Provide self-signed cert generation for development
- [ ] Document TLS configuration in operator guide

**Files Affected:**
- `src/macula_connection.erl`
- `src/macula_connection_pool.erl`
- `src/macula_peer_system/macula_peer_connector.erl`
- `config/sys.config` (add TLS options)

**Acceptance Criteria:**
- Production connections reject invalid certificates
- Development mode allows self-signed with explicit opt-in
- Expired certificates are rejected

---

### P1: Realm Authentication

**Problem:** Realms provide isolation but no authentication.

**Required Changes:**
- [ ] Add API key validation for realm access
- [ ] Rate limiting per realm
- [ ] Audit logging for authentication events

**Acceptance Criteria:**
- Clients must provide valid API key to join realm
- Failed auth attempts are logged
- Rate limits prevent brute force

---

## v0.12.0 - DHT Improvements

### P1: DHT Request/Response Pattern

**Location:** `src/macula_gateway_system/macula_gateway_dht.erl`

**Current Behavior:** Fire-and-forget DHT operations

**Required Changes:**
- [ ] Add correlation IDs to DHT queries
- [ ] Implement timeout and retry logic
- [ ] Add response tracking for observability

---

### P1: K-Bucket Splitting

**Location:** `src/macula_routing_system/macula_routing_table.erl`

**Current Behavior:** Buckets have max capacity but don't split

**Required Changes:**
- [ ] Implement bucket splitting when full
- [ ] Maintain routing table invariants
- [ ] Add routing table health metrics

---

## v0.13.0 - NAT Traversal (Optional)

> **Note:** Only implement if targeting edge deployment. Gateway relay works for cloud deployments.

### P3: Complete NAT Detection

**Location:** `src/macula_peer_system/macula_nat_detector.erl`

**Current:** TODO stubs for NAT_PROBE message handling

**Required Changes:**
- [ ] Implement NAT_PROBE message handling
- [ ] Detect NAT type (EI, HD, PP, PC, RD)
- [ ] Cache NAT type per connection

---

### P3: STUN/TURN Support

**Required Changes:**
- [ ] Add STUN client for address discovery
- [ ] Integrate TURN relay as fallback
- [ ] Hole punch coordination protocol

**Acceptance Criteria:**
- 80%+ success rate for EI/PP NAT types
- Automatic fallback to relay for restrictive NAT

---

## v1.1.0+ - QUIC Distribution (Deferred)

> **Note:** This is a BEAM ecosystem contribution, not core Macula functionality. Only pursue after v1.0.0 is proven in production.

**Location:** `src/macula_dist_system/macula_dist.erl` (606 LOC, 31 tests passing)

**Current State:** Standalone module, not integrated with mesh

**Why Defer:**
1. Macula mesh works fine over HTTP/3 without this
2. Requires TLS verification to be complete first (v0.11.0)
3. Integration with Ra cluster requires multi-node Platform Layer (v0.14.0)
4. Better to contribute after proving the core works

**When Ready:**
- [ ] Integrate with existing Macula discovery
- [ ] Add libcluster strategy
- [ ] Test with Horde, Mnesia, :pg
- [ ] Publish as separate hex package
- [ ] Submit to OTP for consideration

---

## Low Priority: Code Cleanup

### Stub TODOs to Remove

| Location | Issue | Priority |
|----------|-------|----------|
| `macula_pubsub_qos.erl` | Placeholder TODO comments | Low |
| `macula_pubsub_subscription.erl` | Placeholder TODO comments | Low |
| `macula_rpc_handler.erl` | Phase 6 refactoring notes | Low |

---

## Testing Debt

| Area | Current | Target |
|------|---------|--------|
| Unit tests | 44 passing (harness unstable) | 100+ stable |
| Integration tests | Minimal | Multi-node scenarios |
| E2E tests | None | Full workflow tests |

**Known Issue:** Test harness is unstable - tests sometimes get cancelled unexpectedly.

---

## References

- **Roadmap:** `architecture/ROADMAP.md`
- **Source Analysis:** 102 `.erl` files, 52 test files examined
- **TODOs Found:** ~10 critical, ~20 minor

---

## Contributing

When adding TODOs to code:
1. Add entry to this document with priority level
2. Link to GitHub issue if exists
3. Specify target version
4. Include acceptance criteria

---

**Document Version:** 2.0 (Reality-based)
**Last Analysis:** November 2025
