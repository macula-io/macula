# Macula Authorization Guide

This guide covers Macula's identity and authorization primitives: decentralized
identifiers, self-sovereign certificates, and UCAN capability tokens.

## Overview

Macula's authorization is:

- **Self-sovereign**: Identity controlled by the owner's Ed25519 keypair
- **Cryptographically verifiable**: No network calls needed for validation
- **Capability-based**: Fine-grained permissions via UCAN tokens
- **Offline-capable**: All validation happens locally

![Authorization Flow](assets/authorization_flow.svg)

> **What's actually gated today.** The SDK's only enforced authorization
> point is per-procedure: `macula:advertise/5`'s `auth` opt takes `open`
> (default — serve any identified caller; every QUIC session is Ed25519
> peer-bound, so "open" is not "anonymous") or `{ucan_required, Issuer}`
> (a caller must present a valid UCAN, checked via `call_station/7`'s
> `ucan_token` opt). There is no automatic DID-namespace-ownership check on
> publish/subscribe/call — the primitives below (DIDs, certs, UCANs) are
> what you build a stronger policy from, not a policy the SDK enforces on
> its own. See [Direct-Dial Dual-Trust](#direct-dial-dual-trust) for the
> one place the SDK does enforce something end-to-end: cert-chain
> verification against squatted advertisements.

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
| **Cryptographically verifiable** | Ownership proven via signature |

#### DID ⇄ Common Name conversion

Certificates carry a common name (CN) rather than a DID directly; `macula_cert`
converts between the two by reversing the dot-separated segments:

```erlang
CN = macula_cert:did_to_cn(<<"did:macula:io.example.org.app.node01">>).
%% Result: <<"node01.app.org.example.io">>

DID = macula_cert:cn_to_did(<<"node01.app.org.example.io">>).
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

`macula_ucan_nif:create/4,5` builds a JWT with these claims:

| Claim | Description |
|-------|-------------|
| `iss` | **Issuer DID** - Who created and signed this token |
| `aud` | **Audience DID** - Who this token is granted to |
| `exp` | **Expiration** (optional) - Unix timestamp when token expires |
| `nbf` | **Not Before** (optional) - Token valid only after this time |
| `cap` | **Capabilities** - Array of permission grants |
| `prf` | **Proofs** - Chain of parent UCANs (for delegation) |
| `nnc` | **Nonce** (optional) - for uniqueness |
| `fct` | **Facts** (optional) - metadata |

```erlang
{ok, Token}   = macula_ucan_nif:create(IssuerDID, AudienceDID, Capabilities, PrivKey),
{ok, Payload} = macula_ucan_nif:verify(Token, IssuerPubKey),

{ok, Issuer}   = macula_ucan_nif:get_issuer(Token),
{ok, Audience} = macula_ucan_nif:get_audience(Token),
{ok, Caps}     = macula_ucan_nif:get_capabilities(Token),
false          = macula_ucan_nif:is_expired(Token).
```

**Further Reading:**
- [UCAN Specification](https://ucan.xyz/)
- [Fission UCAN Explainer](https://fission.codes/blog/auth-without-backend/)
- [Brooklyn Zelenka's UCAN Paper](https://github.com/ucan-wg/spec)

---

## Self-Sovereign Certificates

Macula uses Ed25519-based certificates anchored to DIDs, enabling identity without external certificate authorities. `macula_cert` and `macula_trust_store` implement this; both are real, standalone modules — nothing in the connect/publish/call path invokes them automatically, you call them directly.

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

### Certificate structure

`#macula_cert{}` (`include/macula_cert.hrl`) is a flat record; `to_map/1`
produces the same fields as a map, not a nested `subject`/`issuer` shape:

```erlang
#{
    version     => 1,
    serial      => <<...>>,          %% 16 random bytes
    subject_did => <<"did:macula:io.customer.org.app.node01">>,
    subject_cn  => <<"node01.app.org.customer.io">>,
    issuer_did  => <<"did:macula:io.customer.org">>,
    issuer_cn   => <<"org.customer.io">>,
    not_before  => 1704067200,
    not_after   => 1735689600,
    public_key  => <<...>>,          %% Ed25519 (32 bytes)
    signature   => <<...>>,          %% Ed25519 (64 bytes)
    extensions  => #{}               %% reserved for future use
}
```

### Certificate API

#### Generate keypair

```erlang
{PubKey, PrivKey} = macula_cert:generate_keypair().
%% PubKey: 32 bytes, PrivKey: 64 bytes (seed + public key)
```

#### Create a realm certificate (self-signed)

```erlang
RealmDID = <<"did:macula:io.example.org">>,
{PubKey, PrivKey} = macula_cert:generate_keypair(),

{ok, RealmCert} = macula_cert:generate_realm_cert(RealmDID, PubKey, PrivKey).

%% Or with custom validity (days)
{ok, RealmCert} = macula_cert:generate_realm_cert(RealmDID, PubKey, PrivKey, 365).
```

#### Create an instance certificate (signed by the realm)

```erlang
InstanceDID = <<"did:macula:io.example.org.app.node01">>,
{InstancePubKey, _} = macula_cert:generate_keypair(),

{ok, InstanceCert} = macula_cert:generate_instance_cert(
    InstanceDID, InstancePubKey, RealmCert, RealmPrivKey
).

%% Or with custom validity (days)
{ok, InstanceCert} = macula_cert:generate_instance_cert(
    InstanceDID, InstancePubKey, RealmCert, RealmPrivKey, 90
).
```

#### Verify certificates

```erlang
ok   = macula_cert:verify_self_signed(RealmCert),
ok   = macula_cert:verify_cert(InstanceCert, RealmCert),
true = macula_cert:is_valid_now(InstanceCert).
```

#### Encode/decode

```erlang
{ok, Binary} = macula_cert:encode(Cert),
{ok, Cert}   = macula_cert:decode(Binary),
Map          = macula_cert:to_map(Cert),
{ok, Cert}   = macula_cert:from_map(Map).
```

### Trust store

```erlang
{ok, _Pid} = macula_trust_store:start_link().

%% Add a trusted realm
ok = macula_trust_store:add_trusted_realm(RealmDID, RealmCert).

%% Verify an instance cert chains back to a trusted realm
ok = macula_trust_store:verify_instance_cert(InstanceCert).

%% Query the trust store
true          = macula_trust_store:is_trusted(RealmDID),
{ok, RealmCert} = macula_trust_store:get_realm_cert(RealmDID),
TrustedRealms = macula_trust_store:list_trusted().
```

### Certificate security considerations

- Private keys should never leave the generating node
- Instance certificates should have shorter validity (30-90 days)
- Realm certificates can have longer validity (1-5 years)
- Implement your own renewal before expiration — nothing in the SDK does this for you

| Aspect | Self-Sovereign | Traditional PKI |
|--------|----------------|-----------------|
| Trust root | Realm certificate | External CA |
| Issuance | Instant, local | Requires CA interaction |
| Cost | Free | Often paid |
| Privacy | No third party | CA sees all certs |
| Interop | Macula ecosystem | Web browsers, etc. |

---

## Direct-Dial Dual-Trust

Direct-dial RPC (a consumer resolves a `procedure_advertisement` and dials the
provider's station) collapses the path to one QUIC/TLS session between two
sovereign identities — the natural place for a **mutual** check. Trust is
bidirectional, unlike the one-directional server-authenticates-client of classic
RPC:

- **consumer → provider** — is this the legitimate server of the procedure, not a
  squatter who wrote an advertisement next to the real one?
- **provider → consumer** — should I serve *this* caller at all? Direct-dial makes
  every station a public front door, so the provider decides who it answers.

Both stay compatible with fully-open, permissionless discovery: the discovery
layer is always open, and each endpoint independently chooses what it checks.

### consumer → provider (managed realms): realm-CA cert chain

In a managed realm the trust root is the **realm CA**, and it already reaches
every member: a service is issued an Ed25519 leaf cert chaining
`realm CA → org CA → leaf`, and receives the realm CA at issuance. A provider
**embeds its cert chain** (leaf + org CA) in its `procedure_advertisement`. A
verifying consumer holds the realm CA and checks a resolved advertisement:

1. the advertisement signature is valid for the advertiser key;
2. the leaf cert binds that same advertiser key;
3. the leaf chains `leaf → org CA → realm CA` (X.509 path validation);
4. the leaf's organization matches the `<org>` in the procedure URI.

Any failure drops the advertisement as a squat — a squatter cannot obtain a
realm-CA-issued cert binding their key to someone else's org.

```erlang
%% consumer side (the SDK helper the resolution runs)
ok = macula_record:verify_advertisement_cert_chain(RealmCaPem, Advertisement, Org).
```

> Note on the realm tag: the 32-byte realm tag is `SHA-256(realm_name)` — a
> keyless label, not a signing key. Trust therefore roots in the realm **CA**
> (a real key the realm holds and distributes at issuance), not the tag.

### provider → consumer: UCAN-gated procedures

A bare advertisement serves any *identified* caller (every QUIC session is
Ed25519 peer-bound, so "open" is not "anonymous"). A provider can instead require
a UCAN per procedure via `advertise/5`'s `#{auth => {ucan_required, Issuer}}` —
a caller presents a `ucan_token` on the CALL (`call_station/7`'s `Opts`), and a
caller without a valid one is refused with a BOLT#4 `unauthorized` code rather
than a timeout. The token is verified offline against the chain the provider
recognises — no live authority in the path.

Managed realms are the first target for this model; the fully-open public realm
keeps discovery permissionless and layers authorization on top only where a
provider opts in.

---

## Best Practices

### Token lifetime guidelines

| Use Case | Recommended Lifetime |
|----------|---------------------|
| API calls | 1-24 hours |
| Long-term partnerships | Months (narrow scope) |
| Sensitive operations | Always short |

### Security recommendations

1. **Use short-lived tokens** for sensitive operations
2. **Narrow capability scope** — grant only what's needed
3. **Store UCAN tokens securely** (encrypted at rest, treat as credentials)
4. **Protect private keys** — never leave the generating node

---

## Glossary

| Term | Definition |
|------|------------|
| **DID** | Decentralized Identifier - globally unique, self-sovereign identity |
| **UCAN** | User Controlled Authorization Network - capability-based auth token |
| **Capability** | Permission grant with resource and operation |
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

### Related Guides

- [RPC Guide](RPC_GUIDE.md) - direct-dial, `advertise/5`'s `auth` opt, `call_station/7`'s `ucan_token`
- [MRI Guide](MRI_GUIDE.md) - typed, hierarchical resource identifiers (a separate feature from DID namespaces)
