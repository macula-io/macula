# Macula Authorization Guide

This guide provides a comprehensive overview of Macula's authorization system, including decentralized identity, capability-based permissions, self-sovereign certificates, and UCAN-based licensing.

## Overview

Macula implements **decentralized authorization** using industry-standard cryptographic primitives. Unlike traditional client-server authorization where a central authority validates requests, Macula's authorization is:

- **Self-sovereign**: Identity controlled by the owner's keypair
- **Cryptographically verifiable**: No network calls needed for validation
- **Capability-based**: Fine-grained permissions via UCAN tokens
- **Offline-capable**: All validation happens locally

![Authorization Flow](assets/authorization_flow.svg)

---

## Core Concepts

### Decentralized Identifiers (DIDs)

A **DID** (Decentralized Identifier) is a globally unique identifier that enables verifiable, decentralized digital identity. DIDs are defined by the [W3C DID Core Specification](https://www.w3.org/TR/did-core/).

![DID Structure](assets/did_structure.svg)

#### Macula DID Format

```
did:macula:io.macula.rgfaber
|   |      +------------------ Method-specific identifier (namespace)
|   +-------------------------- Method (macula)
+------------------------------- Scheme (always "did")
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

#### DID to Common Name Conversion

```erlang
%% DID to CN (reverse the parts)
CN = macula_cert:did_to_cn(<<"did:macula:io.example.org.app.node01">>).
%% Result: <<"app.node01.io.example.org">>

%% CN to DID
DID = macula_cert:cn_to_did(<<"app.node01.io.example.org">>).
%% Result: <<"did:macula:io.example.org.app.node01">>
```

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
  |
  +---> grants Bob: mesh:call on io.macula.alice.api.*
        |
        +---> Bob grants Carol: mesh:call on io.macula.alice.api.read_only
              (valid - narrower scope)

        +---> Bob grants Dave: mesh:call on io.macula.alice.*
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
-------------------------------  -----------------------------
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
did:macula:io.macula.ibm         -> CAN access io.macula.ibm.watson.*
did:macula:io.macula.ibm.watson  -> CANNOT access io.macula.ibm.*
did:macula:io.macula.rgfaber     -> CANNOT access io.macula.ibm.*
```

---

## Self-Sovereign Certificates

Macula uses Ed25519-based certificates anchored to DIDs, enabling identity without external certificate authorities.

### Certificate Hierarchy

```
Realm Certificate (self-signed)
did:macula:io.customer.org
     |
     +-- Instance Certificate (signed by realm)
     |   did:macula:io.customer.org.app1.node01
     |
     +-- Instance Certificate (signed by realm)
     |   did:macula:io.customer.org.app1.node02
     |
     +-- Instance Certificate (signed by realm)
         did:macula:io.customer.org.app2.node01
```

### Certificate Structure

```erlang
#{
    subject => #{
        did => <<"did:macula:io.customer.org.app.node01">>,
        common_name => <<"app.node01.io.customer.org">>
    },
    issuer => #{
        did => <<"did:macula:io.customer.org">>,
        common_name => <<"io.customer.org">>
    },
    not_before => 1704067200,
    not_after => 1735689600,
    public_key => <<...>>,   %% Ed25519 (32 bytes)
    signature => <<...>>     %% Ed25519 (64 bytes)
}
```

### Certificate API

#### Generate Keypair

```erlang
{PubKey, PrivKey} = macula_cert:generate_keypair().
%% PubKey: 32 bytes, PrivKey: 64 bytes (seed + public key)
```

#### Create Realm Certificate (Self-Signed)

```erlang
RealmDID = <<"did:macula:io.example.org">>,
{PubKey, PrivKey} = macula_cert:generate_keypair(),

{ok, RealmCert} = macula_cert:generate_realm_cert(RealmDID, PubKey, PrivKey).

%% Or with custom validity
{ok, RealmCert} = macula_cert:generate_realm_cert(RealmDID, PubKey, PrivKey, 365).
```

#### Create Instance Certificate (Signed by Realm)

```erlang
InstanceDID = <<"did:macula:io.example.org.app.node01">>,
{InstancePubKey, _} = macula_cert:generate_keypair(),

{ok, InstanceCert} = macula_cert:generate_instance_cert(
    InstanceDID, InstancePubKey, RealmCert, RealmPrivKey
).

%% Or with custom validity (90 days)
{ok, InstanceCert} = macula_cert:generate_instance_cert(
    InstanceDID, InstancePubKey, RealmCert, RealmPrivKey, 90
).
```

#### Verify Certificates

```erlang
ok = macula_cert:verify_self_signed(RealmCert).
ok = macula_cert:verify_cert(InstanceCert, RealmCert).
true = macula_cert:is_valid_now(InstanceCert).
```

#### Encode/Decode

```erlang
{ok, Binary} = macula_cert:encode(Cert).
{ok, Cert} = macula_cert:decode(Binary).
Map = macula_cert:to_map(Cert).
{ok, Cert} = macula_cert:from_map(Map).
```

### Trust Store

The trust store manages trusted realm certificates.

```erlang
{ok, _Pid} = macula_trust_store:start_link().

%% Add a trusted realm
ok = macula_trust_store:add_trusted_realm(RealmDID, RealmCert).

%% Verify instance cert chains back to a trusted realm
ok = macula_trust_store:verify_instance_cert(InstanceCert).

%% Query trust store
true = macula_trust_store:is_trusted(RealmDID).
{ok, RealmCert} = macula_trust_store:get_realm_cert(RealmDID).
TrustedRealms = macula_trust_store:list_trusted().
```

### Certificate Security Considerations

- Private keys should never leave the generating node
- Instance certificates should have shorter validity (30-90 days)
- Realm certificates can have longer validity (1-5 years)
- Implement automatic renewal before expiration

| Aspect | Self-Sovereign | Traditional PKI |
|--------|----------------|-----------------|
| Trust root | Realm certificate | External CA |
| Issuance | Instant, local | Requires CA interaction |
| Cost | Free | Often paid |
| Privacy | No third party | CA sees all certs |
| Revocation | Limited | CRL/OCSP |
| Interop | Macula ecosystem | Web browsers, etc. |

---

## UCAN-Based Licensing

Macula uses UCAN tokens for software licensing in the marketplace, providing decentralized, capability-based license management.

### License Models

| Model | Description | UCAN Configuration |
|-------|-------------|-------------------|
| **Free** | No restrictions | Auto-granted, no expiration |
| **Trial** | Time-limited evaluation | Short `exp` (14-30 days) |
| **Perpetual** | One-time purchase | No expiration |
| **Subscription** | Recurring payment | `exp` set to subscription end |
| **Per-seat** | Limited installations | `nb.max_nodes` claim |

### License Token Structure

```json
{
  "iss": "did:macula:io.publisher.org",
  "aud": "did:macula:io.customer.org",
  "exp": 1735689600,
  "nbf": 1704067200,
  "att": [
    {
      "with": "did:macula:io.publisher.org/app/my_app",
      "can": "artifact/run"
    },
    {
      "with": "did:macula:io.publisher.org/app/my_app",
      "can": "artifact/update"
    }
  ],
  "fct": {
    "license_model": "subscription",
    "max_nodes": 5
  }
}
```

### License Capability Types

| Capability | Description |
|------------|-------------|
| `artifact/run` | Permission to run the application |
| `artifact/update` | Permission to update to new versions |
| `artifact/install` | Permission to install the application |

### License Validation

```erlang
case bc_gitops_license:validate(LicenseSpec, #{app_name => Name}) of
    ok -> deploy(AppSpec);
    {warning, expiring_soon} -> log_warning("License expiring soon"), deploy(AppSpec);
    {error, expired} -> {error, {license_validation_failed, expired}};
    {error, invalid_signature} -> {error, {license_validation_failed, invalid_signature}};
    {error, node_limit_exceeded} -> {error, {license_validation_failed, node_limit_exceeded}}
end.
```

### License Procurement Flows

**Free Applications:** User installs, UCAN auto-granted with no expiration.

**Trial Applications:** User starts trial, UCAN generated with 14-day expiration.

**Paid Applications:** User installs, payment required, publisher issues UCAN after payment, user retries install.

### License Renewal

1. User/system requests renewal from publisher
2. Payment processed (if subscription)
3. Publisher issues new UCAN with extended expiration
4. Update app.config with new UCAN
5. bc-gitops reconciles with new license

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
io.macula.rgfaber.place_order -> io.macula.rgfaber
```

### Step 3: Check Ownership

```erlang
case macula_authorization:check_namespace_ownership(CallerDID, Namespace) of
    {ok, owner} -> authorized;
    {ok, ancestor} -> authorized;
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
io.macula.rgfaber.public.announcements  -> Anyone can subscribe
io.macula.public.news                   -> Anyone can subscribe
```

**Note**: Publishing to public topics still requires ownership or a valid UCAN grant.

---

## Revocation

UCANs can be revoked before expiration using the revocation system:

![Revocation Flow](assets/revocation_flow.svg)

### Revocation Process

1. **Issuer initiates**: Only the token issuer can revoke
2. **Compute CID**: SHA-256 hash of the UCAN, base64url encoded
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
{ok, CID} = macula_ucan_revocation:revoke(IssuerDID, UcanToken, ExpiresAt).
true = macula_ucan_revocation:is_revoked(IssuerDID, CID).
```

---

## Audit Logging

All authorization decisions are logged via the audit system:

![Audit System](assets/audit_system.svg)

### Telemetry Events

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
    reason => unauthorized | invalid_ucan | expired_ucan | ...
}
```

### ETS Storage (Optional)

```erlang
macula_authorization_audit:enable().
macula_authorization_audit:disable().
Entries = macula_authorization_audit:get_recent(100).
CallerEntries = macula_authorization_audit:get_by_caller(CallerDID, 50).
Stats = macula_authorization_audit:get_stats().
```

### LRU Eviction

![LRU Eviction](assets/lru_eviction.svg)

When the log exceeds `max_entries`, the oldest entries (by timestamp) are evicted, ensuring bounded memory usage while retaining the most recent authorization decisions.

```erlang
macula_authorization_audit:set_max_entries(5000).
macula_authorization_audit:set_retention(7200).
```

---

## API Reference

### Core Authorization

```erlang
macula_authorization:check_rpc_call(CallerDID, Procedure, UcanToken, Opts).
macula_authorization:check_publish(CallerDID, Topic, UcanToken, Opts).
macula_authorization:check_subscribe(CallerDID, Topic, Opts).
macula_authorization:check_subscribe(CallerDID, Topic, UcanToken, Opts).
macula_authorization:check_announce(CallerDID, Procedure, Opts).
```

### Namespace Operations

```erlang
macula_authorization:extract_namespace(<<"io.macula.rgfaber.service">>).
macula_authorization:check_namespace_ownership(CallerDID, Namespace).
macula_authorization:is_public_topic(<<"io.macula.rgfaber.public.news">>).
```

### DID Cache

```erlang
macula_did_cache:get_or_parse(DID).
macula_did_cache:invalidate(DID).
macula_did_cache:clear().
macula_did_cache:cache_size().
```

### UCAN Revocation

```erlang
{ok, Pid} = macula_ucan_revocation:start_link().
{ok, CID} = macula_ucan_revocation:revoke(IssuerDID, UcanToken, ExpiresAt).
macula_ucan_revocation:is_revoked(IssuerDID, CID).
macula_ucan_revocation:get_stats().
```

### Audit Logging

```erlang
{ok, Pid} = macula_authorization_audit:start_link().
macula_authorization_audit:log_authorized(Operation, CallerDID, Resource).
macula_authorization_audit:log_denied(Operation, CallerDID, Resource, Reason).
macula_authorization_audit:get_recent(Limit).
macula_authorization_audit:get_by_caller(CallerDID, Limit).
macula_authorization_audit:get_by_resource(Resource, Limit).
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
| Trial licenses | 14-30 days |
| Subscription licenses | Match billing period |

### Security Recommendations

1. **Use short-lived tokens** for sensitive operations
2. **Narrow capability scope** - grant only what's needed
3. **Monitor audit logs** for unusual patterns
4. **Enable telemetry** for real-time observability
5. **Revoke promptly** when relationships end
6. **Store UCAN tokens securely** (encrypted at rest, treat as credentials)
7. **Always verify Ed25519 signatures** on license tokens
8. **Protect private keys** - never leave the generating node

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
| **Realm Certificate** | Self-signed root certificate for a Macula realm |
| **Instance Certificate** | Certificate signed by a realm certificate for a specific node |
| **Trust Store** | Local store of trusted realm certificates |

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

### Related Guides

- [DHT Guide](DHT_GUIDE.md) - Distributed certificate discovery
- [Clustering Guide](CLUSTERING_GUIDE.md) - Using certificates in clusters
