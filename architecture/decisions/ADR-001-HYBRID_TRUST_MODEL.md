# ADR-001: Hybrid Trust Model for Plug-and-Play Mesh

**Status:** Accepted
**Date:** 2025-11-28
**Decision Makers:** Architecture Team

## Context

Macula v0.11.0 completed TLS certificate verification with two modes:
- **Production mode**: Strict CA verification
- **Development mode**: Self-signed certificates allowed

However, a critical friction point remains for the plug-and-play mesh vision:

**Problem**: Every node in the mesh requires valid TLS certificates for the Gateway System
(which runs on ALL nodes since v0.8.5). This creates significant friction:

1. Users must generate/obtain TLS certificates before joining
2. Certificate management complexity grows with mesh size
3. CA-signed certificates have cost/logistics overhead
4. Self-signed certificates provide no actual security guarantee

**Goal**: Enable plug-and-play mesh joining while maintaining meaningful security.

## Decision

Implement a **Hybrid Trust Model** with three trust levels:

### Level 1: Realm Authentication (Required)
- All nodes must authenticate to join a realm
- Authentication via API key, token, or pre-shared secret
- Realm owners control who can join their namespace
- Realm authentication is the **gate** - no mesh access without it

### Level 2: Certificate Trust (TOFU within Realm)
- Once authenticated to a realm, certificate fingerprints are tracked
- First connection = trust on first use (TOFU)
- Certificate fingerprint stored in DHT with realm-scoped key
- Subsequent connections verify fingerprint matches
- Alerts/rejection if fingerprint changes unexpectedly

### Level 3: CA-Signed Certificates (Optional, for Seed Nodes)
- Public-facing seed nodes SHOULD use CA-signed certificates
- Provides verifiable identity for initial mesh entry point
- Let's Encrypt or organizational CA acceptable
- Regular nodes can use self-signed with TOFU

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                        Trust Verification Flow                       │
└─────────────────────────────────────────────────────────────────────┘

New Node Joining Mesh:

    ┌──────────────┐
    │   New Node   │
    │ (self-signed │
    │   cert)      │
    └──────┬───────┘
           │
           │ 1. Connect to Seed Node
           │    (verify CA cert if production seed)
           ▼
    ┌──────────────┐
    │  Seed Node   │
    │ (CA-signed   │
    │  optional)   │
    └──────┬───────┘
           │
           │ 2. Realm Authentication
           │    (API key / token / secret)
           ▼
    ┌──────────────┐    ┌──────────────────────────────────────┐
    │ Realm Auth   │───►│ Reject if auth fails                 │
    │ Gateway      │    │ No mesh access without realm auth    │
    └──────┬───────┘    └──────────────────────────────────────┘
           │
           │ 3. Auth succeeded
           │    Store cert fingerprint in DHT
           ▼
    ┌──────────────┐
    │    DHT       │
    │ realm.node   │
    │ .certs       │
    │ {NodeId,     │
    │  Fingerprint,│
    │  FirstSeen,  │
    │  LastSeen}   │
    └──────┬───────┘
           │
           │ 4. Node can now participate
           │    Other nodes verify fingerprint via DHT
           ▼
    ┌──────────────┐
    │ Mesh Member  │
    │ (trusted     │
    │  within      │
    │  realm)      │
    └──────────────┘


Peer-to-Peer Connection (within Realm):

    ┌──────────────┐                    ┌──────────────┐
    │   Node A     │                    │   Node B     │
    │ (realm auth  │                    │ (realm auth  │
    │  verified)   │                    │  verified)   │
    └──────┬───────┘                    └──────┬───────┘
           │                                   │
           │ 1. QUIC connect (TLS handshake)   │
           │──────────────────────────────────►│
           │                                   │
           │ 2. Extract peer cert fingerprint  │
           │◄──────────────────────────────────│
           │                                   │
           │ 3. DHT lookup: realm.node.certs   │
           │    Does fingerprint match?        │
           │         │                         │
           │         ▼                         │
           │   ┌─────────────┐                 │
           │   │   Match?    │                 │
           │   └─────┬───────┘                 │
           │         │                         │
           │    Yes  │  No                     │
           │    ┌────┴────┐                    │
           │    ▼         ▼                    │
           │ Continue   Alert!                 │
           │ normal     Possible               │
           │ operation  MITM                   │
           │                                   │
           └───────────────────────────────────┘
```

## Implementation Plan

### Phase 1: Realm Trust Module
New module: `macula_realm_trust.erl`

```erlang
-module(macula_realm_trust).
-export([
    authenticate/2,          %% Authenticate to realm
    register_fingerprint/3,  %% Store cert fingerprint in DHT
    verify_fingerprint/3,    %% Verify peer fingerprint
    get_trusted_peers/1,     %% List trusted peers in realm
    revoke_trust/2           %% Revoke trust for a node
]).
```

### Phase 2: Integration Points
- `macula_tls.erl` - Add TOFU verification mode
- `macula_connection.erl` - Fingerprint extraction on connect
- `macula_bootstrap_server.erl` - Realm auth enforcement
- `macula_gateway_client_manager.erl` - Client realm verification

### Phase 3: DHT Schema
Fingerprint storage key pattern:
```erlang
%% DHT key: {realm, Realm, node_certs, NodeId}
%% DHT value: #{
%%     fingerprint => <<Fingerprint/binary>>,
%%     first_seen => Timestamp,
%%     last_seen => Timestamp,
%%     cert_type => self_signed | ca_signed
%% }
```

## Consequences

### Benefits
- **Plug-and-play**: Self-signed certs work within authenticated realms
- **Security layering**: Realm auth + TOFU is more secure than either alone
- **Flexibility**: CA certs optional for seed nodes, not required everywhere
- **Auditability**: DHT tracks all trusted fingerprints
- **MITM detection**: Fingerprint changes trigger alerts

### Trade-offs
- First connection is vulnerable (inherent to TOFU)
- Realm auth becomes critical security boundary
- DHT fingerprint data must be protected
- More complex than simple CA verification

### Mitigations
- Seed nodes SHOULD use CA certs (verified first contact)
- Realm secrets should be managed securely
- Fingerprint changes should alert operators
- Consider certificate pinning for critical paths

## Alternatives Considered

### 1. Full CA-Only (Rejected)
- Too much friction for plug-and-play
- Certificate costs/logistics prohibitive at scale
- Not aligned with decentralized vision

### 2. No Verification (Rejected)
- No security guarantee
- Vulnerable to MITM attacks
- Not acceptable for production use

### 3. Web of Trust (Deferred)
- Complex to implement correctly
- Requires existing peer network
- May be added as enhancement later

### 4. ACME Auto-Provisioning (Deferred)
- Requires HTTP challenge server
- External dependency (Let's Encrypt)
- May be added for seed nodes later

## References

- RFC 8555 (ACME Protocol)
- SSH known_hosts (TOFU precedent)
- Macula Glossary: `docs/GLOSSARY.md`
- TLS Configuration: `docs/operator/TLS_CONFIGURATION.md`
- Macula Roadmap: `architecture/ROADMAP.md`
