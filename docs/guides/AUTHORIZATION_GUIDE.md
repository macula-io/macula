# Macula Authorization Guide

This guide provides a comprehensive overview of the Macula mesh authorization system, including the underlying concepts, implementation details, and best practices.

## Overview

Macula implements **decentralized authorization** using industry-standard cryptographic primitives. Unlike traditional client-server authorization where a central authority validates requests, Macula's authorization is:

- **Self-sovereign**: Identity controlled by the owner's keypair
- **Cryptographically verifiable**: No network calls needed for validation
- **Capability-based**: Fine-grained permissions via UCAN tokens
- **Offline-capable**: All validation happens locally

![Authorization Flow](assets/authorization_flow.svg)

## Core Concepts

### Decentralized Identifiers (DIDs)

A **DID** (Decentralized Identifier) is a globally unique identifier that enables verifiable, decentralized digital identity. DIDs are defined by the [W3C DID Core Specification](https://www.w3.org/TR/did-core/).

![DID Structure](assets/did_structure.svg)

#### Macula DID Format

```
did:macula:io.macula.rgfaber
│   │      └─────────────────── Method-specific identifier (namespace)
│   └──────────────────────────── Method (macula)
└──────────────────────────────── Scheme (always "did")
```

**Key Properties:**

| Property | Description |
|----------|-------------|
| **Self-sovereign** | Controlled by owner's Ed25519 keypair |
| **Human-readable** | Hierarchical namespace format |
| **Resolvable** | Method defines how to obtain public key |
| **Cryptographically verifiable** | Ownership proven via signature |

#### DID Resolution

When a DID is encountered, Macula resolves it to extract:

1. **Method**: `macula` (defines resolution rules)
2. **Identity**: `io.macula.rgfaber` (namespace path)
3. **Parts**: `["io", "macula", "rgfaber"]` (hierarchy)
4. **Depth**: `3` (hierarchy level)

The `macula_did_cache` module provides high-performance caching using Erlang's `persistent_term`:

```erlang
%% Parse and cache a DID (or retrieve from cache)
{ok, Parsed} = macula_did_cache:get_or_parse(<<"did:macula:io.macula.rgfaber">>).
%% => {ok, #{<<"method">> => <<"macula">>,
%%           <<"identity">> => <<"io.macula.rgfaber">>,
%%           <<"parts">> => [<<"io">>, <<"macula">>, <<"rgfaber">>],
%%           <<"depth">> => 3}}
```

**Performance**: `persistent_term` provides O(1) lookups with zero garbage collection impact, making it ideal for frequently-accessed identity data.

**Further Reading:**
- [W3C DID Core 1.0](https://www.w3.org/TR/did-core/)
- [DID Method Registry](https://w3c.github.io/did-spec-registries/)

---

### User Controlled Authorization Networks (UCANs)

**UCAN** (User Controlled Authorization Networks) is a capability-based authorization system built on JWT (JSON Web Tokens). UCANs enable **delegation chains** where permissions can be granted and re-delegated without involving a central authority.

![UCAN Token Structure](assets/ucan_token_structure.svg)

#### UCAN Token Structure

A UCAN is a JWT with specific claims:

| Claim | Description |
|-------|-------------|
| `iss` | **Issuer DID** - Who created and signed this token |
| `aud` | **Audience DID** - Who this token is granted to |
| `exp` | **Expiration** - Unix timestamp when token expires |
| `nbf` | **Not Before** - Token valid only after this time |
| `cap` | **Capabilities** - Array of permission grants |
| `prf` | **Proofs** - Chain of parent UCANs (for delegation) |

#### Capability Format

Capabilities define what actions are permitted on which resources:

```erlang
#{
    <<"with">> => <<"io.macula.rgfaber.*">>,   % Resource pattern
    <<"can">> => <<"mesh:call">>               % Operation
}
```

**Supported Operations:**

| Operation | Description |
|-----------|-------------|
| `mesh:call` | RPC procedure invocation |
| `mesh:publish` | Publish to topic |
| `mesh:subscribe` | Subscribe to topic |
| `mesh:*` | All mesh operations |

**Wildcards:**

- `io.macula.rgfaber.*` matches any resource in namespace
- `mesh:*` matches any mesh operation

#### Delegation Chains

UCANs support **attenuated delegation**: permissions can be re-delegated but only with equal or narrower scope.

```
Alice (owns io.macula.alice.*)
  │
  └──► grants Bob: mesh:call on io.macula.alice.api.*
        │
        └──► Bob grants Carol: mesh:call on io.macula.alice.api.read_only
              (valid - narrower scope)

        └──► Bob grants Dave: mesh:call on io.macula.alice.*
              (INVALID - broader than Bob received)
```

**Further Reading:**
- [UCAN Specification](https://ucan.xyz/)
- [Fission UCAN Explainer](https://fission.codes/blog/auth-without-backend/)
- [Brooklyn Zelenka's UCAN Paper](https://github.com/ucan-wg/spec)

---

### Namespace Ownership Model

Macula uses a **hierarchical namespace ownership model** where DIDs map directly to namespaces they control:

![Namespace Hierarchy](assets/namespace_hierarchy.svg)

#### DID to Namespace Mapping

```
DID                              Owns Namespace
───────────────────────────────  ─────────────────────────────
did:macula:io.macula             io.macula.* (realm root)
did:macula:io.macula.rgfaber     io.macula.rgfaber.*
did:macula:io.macula.ibm         io.macula.ibm.*
did:macula:io.macula.ibm.watson  io.macula.ibm.watson.*
```

#### Hierarchical Access Rules

1. **Owner access**: A DID can always access its own namespace
2. **Parent access**: Parent DIDs can access child namespaces
3. **Sibling isolation**: Siblings cannot access each other's namespaces

```erlang
%% Examples
did:macula:io.macula.ibm         → CAN access io.macula.ibm.watson.*
did:macula:io.macula.ibm.watson  → CANNOT access io.macula.ibm.*
did:macula:io.macula.rgfaber     → CANNOT access io.macula.ibm.*
```

---

## Authorization Flow

When a mesh operation (RPC call, publish, subscribe) is requested, Macula follows this authorization flow:

### Step 1: Extract Caller DID

The caller's DID is extracted from:
- **TLS Certificate SAN** (Subject Alternative Name URI field)
- **Message Header** (for explicit caller identification)

### Step 2: Extract Namespace

The namespace is derived from the topic/procedure:

```erlang
io.macula.rgfaber.place_order → io.macula.rgfaber
```

### Step 3: Check Ownership

```erlang
case macula_authorization:check_namespace_ownership(CallerDID, Namespace) of
    {ok, owner} -> authorized;
    {ok, ancestor} -> authorized;  % Parent can access child
    {error, not_owner} -> check_ucan()
end.
```

### Step 4: Validate UCAN (if needed)

If the caller doesn't own the namespace, a valid UCAN token is required:

```erlang
case macula_authorization:validate_ucan_for_operation(UcanToken, CallerDID, Resource, Operation) of
    {ok, authorized} -> proceed;
    {error, Reason} -> deny
end.
```

---

## Public Topics

Topics containing `.public.` are **world-readable** without ownership or UCAN:

```
io.macula.rgfaber.public.announcements  → Anyone can subscribe
io.macula.public.news                    → Anyone can subscribe
```

**Note**: Publishing to public topics still requires ownership or a valid UCAN grant.

---

## Revocation

UCANs can be revoked before expiration using the revocation system:

![Revocation Flow](assets/revocation_flow.svg)

### Revocation Process

1. **Issuer initiates**: Only the token issuer can revoke
2. **Compute CID**: SHA-256 hash of the UCAN → base64url encoded
3. **Sign revocation**: Ed25519 signature proves issuer authority
4. **Broadcast**: Published to `io.macula.system.ucan_revoked` topic
5. **Cache locally**: Each node stores in ETS with TTL

### Safeguards

| Safeguard | Implementation |
|-----------|----------------|
| **Issuer-only** | Signature validated against issuer's public key |
| **Rate limiting** | Max 10 revocations per issuer per minute |
| **Auto-expiry** | Revocation cached until original UCAN `exp` time |
| **Ed25519 required** | 64-byte signature format enforced |

### Revocation API

```erlang
%% Revoke a UCAN
{ok, CID} = macula_ucan_revocation:revoke(IssuerDID, UcanToken, ExpiresAt).

%% Check if revoked
true = macula_ucan_revocation:is_revoked(IssuerDID, CID).
```

---

## Audit Logging

All authorization decisions are logged via the audit system:

![Audit System](assets/audit_system.svg)

### Telemetry Events

Authorization results emit telemetry events for observability:

| Event | Description |
|-------|-------------|
| `[macula, authorization, allowed]` | Authorization succeeded |
| `[macula, authorization, denied]` | Authorization denied |
| `[macula, authorization, error]` | Authorization check failed |

### Event Metadata

```erlang
#{
    operation => call | publish | subscribe | announce,
    caller => <<"did:macula:io.macula.rgfaber">>,
    resource => <<"io.macula.other.service">>,
    timestamp => 1704672000,
    reason => unauthorized | invalid_ucan | expired_ucan | ...  % if denied
}
```

### ETS Storage (Optional)

The audit module optionally stores entries in ETS for debugging:

```erlang
%% Enable/disable storage
macula_authorization_audit:enable().
macula_authorization_audit:disable().

%% Query recent entries
Entries = macula_authorization_audit:get_recent(100).

%% Query by caller
CallerEntries = macula_authorization_audit:get_by_caller(CallerDID, 50).

%% Statistics
Stats = macula_authorization_audit:get_stats().
%% => #{allowed_count => 1000, denied_count => 5, error_count => 0, ...}
```

### LRU Eviction

The audit log uses **Least Recently Used (LRU)** eviction to bound memory:

![LRU Eviction](assets/lru_eviction.svg)

When the log exceeds `max_entries`, the oldest entries (by timestamp) are evicted. This ensures bounded memory usage while retaining the most recent authorization decisions for debugging.

**Configuration:**

```erlang
%% Set maximum entries (default: 10,000)
macula_authorization_audit:set_max_entries(5000).

%% Set retention period (default: 3600 seconds)
macula_authorization_audit:set_retention(7200).
```

---

## API Reference

### Core Authorization

```erlang
%% Check RPC call authorization
macula_authorization:check_rpc_call(CallerDID, Procedure, UcanToken, Opts).
%% => {ok, authorized} | {error, unauthorized | invalid_ucan | ...}

%% Check publish authorization
macula_authorization:check_publish(CallerDID, Topic, UcanToken, Opts).

%% Check subscribe authorization
macula_authorization:check_subscribe(CallerDID, Topic, Opts).
macula_authorization:check_subscribe(CallerDID, Topic, UcanToken, Opts).

%% Check announce authorization (ownership required, no UCAN)
macula_authorization:check_announce(CallerDID, Procedure, Opts).
```

### Namespace Operations

```erlang
%% Extract namespace from topic/procedure
macula_authorization:extract_namespace(<<"io.macula.rgfaber.service">>).
%% => <<"io.macula.rgfaber">>

%% Check ownership
macula_authorization:check_namespace_ownership(CallerDID, Namespace).
%% => {ok, owner | ancestor} | {error, not_owner}

%% Check if public
macula_authorization:is_public_topic(<<"io.macula.rgfaber.public.news">>).
%% => true
```

### DID Cache

```erlang
%% Parse with caching
macula_did_cache:get_or_parse(DID).

%% Invalidate entry
macula_did_cache:invalidate(DID).

%% Clear cache
macula_did_cache:clear().

%% Get cache size
macula_did_cache:cache_size().
```

### UCAN Revocation

```erlang
%% Start revocation server
{ok, Pid} = macula_ucan_revocation:start_link().

%% Revoke a UCAN
{ok, CID} = macula_ucan_revocation:revoke(IssuerDID, UcanToken, ExpiresAt).

%% Check revocation status
macula_ucan_revocation:is_revoked(IssuerDID, CID).

%% Statistics
macula_ucan_revocation:get_stats().
```

### Audit Logging

```erlang
%% Start audit server
{ok, Pid} = macula_authorization_audit:start_link().

%% Log (called automatically by authorization checks)
macula_authorization_audit:log_authorized(Operation, CallerDID, Resource).
macula_authorization_audit:log_denied(Operation, CallerDID, Resource, Reason).

%% Query
macula_authorization_audit:get_recent(Limit).
macula_authorization_audit:get_by_caller(CallerDID, Limit).
macula_authorization_audit:get_by_resource(Resource, Limit).

%% Configuration
macula_authorization_audit:enable().
macula_authorization_audit:disable().
macula_authorization_audit:set_max_entries(N).
macula_authorization_audit:set_retention(Seconds).
```

---

## Best Practices

### Token Lifetime Guidelines

| Use Case | Recommended Lifetime |
|----------|---------------------|
| API calls | 1-24 hours |
| Treatment periods | Days to weeks |
| Long-term partnerships | Months (narrow scope) |
| Sensitive operations | Always short |

### Security Recommendations

1. **Use short-lived tokens** for sensitive operations
2. **Narrow capability scope** - grant only what's needed
3. **Monitor audit logs** for unusual patterns
4. **Enable telemetry** for real-time observability
5. **Revoke promptly** when relationships end

### Performance Tips

1. **DID cache** handles repeated lookups efficiently
2. **Ownership check first** - faster than UCAN validation
3. **ETS audit storage** can be disabled in production
4. **Telemetry handlers** should be non-blocking

---

## Glossary

| Term | Definition |
|------|------------|
| **DID** | Decentralized Identifier - globally unique, self-sovereign identity |
| **UCAN** | User Controlled Authorization Network - capability-based auth token |
| **Capability** | Permission grant with resource and operation |
| **Namespace** | Hierarchical identifier path (e.g., `io.macula.rgfaber`) |
| **CID** | Content Identifier - cryptographic hash of content |
| **LRU** | Least Recently Used - cache eviction strategy |
| **Ed25519** | Elliptic curve signature algorithm |

---

## References

### Standards

- [W3C DID Core 1.0](https://www.w3.org/TR/did-core/) - Decentralized Identifiers specification
- [UCAN Specification](https://ucan.xyz/) - User Controlled Authorization Networks
- [RFC 7519 - JWT](https://www.rfc-editor.org/rfc/rfc7519) - JSON Web Token specification
- [RFC 8032 - Ed25519](https://www.rfc-editor.org/rfc/rfc8032) - Edwards-Curve Digital Signature Algorithm

### Academic Papers

- Sporny, M., et al. (2022). *Decentralized Identifiers (DIDs) v1.0*. W3C Recommendation.
- Zelenka, B. (2021). *UCAN: Trustless, Server-Free Authorization*. Fission.
- Bernstein, D.J., et al. (2012). *High-speed high-security signatures*. Journal of Cryptographic Engineering.

### Implementation Resources

- [persistent_term Documentation](https://www.erlang.org/doc/man/persistent_term.html) - Erlang cache module
- [ETS Documentation](https://www.erlang.org/doc/man/ets.html) - Erlang Term Storage
- [Telemetry Documentation](https://hexdocs.pm/telemetry/) - Observability library
