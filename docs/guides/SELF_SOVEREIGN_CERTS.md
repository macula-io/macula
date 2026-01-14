# Self-Sovereign Certificates Guide

This guide covers the self-sovereign certificate system in Macula, enabling DID-anchored identity without external certificate authorities.

## Overview

Macula uses Ed25519-based certificates anchored to DIDs (Decentralized Identifiers). This provides:

- **Self-sovereignty**: No external CA required
- **DID anchoring**: Certificates tied to verifiable identities
- **Hierarchical trust**: Realm certificates sign instance certificates
- **Lightweight**: Ed25519 keys are 32 bytes vs 256+ for RSA

## Certificate Hierarchy

```
┌─────────────────────────────────────────────────────────────────────────┐
│                       CERTIFICATE HIERARCHY                              │
│                                                                          │
│  Realm Certificate (self-signed)                                         │
│  did:macula:io.customer.org                                              │
│       │                                                                  │
│       ├── Instance Certificate (signed by realm)                         │
│       │   did:macula:io.customer.org.app1.node01                         │
│       │                                                                  │
│       ├── Instance Certificate (signed by realm)                         │
│       │   did:macula:io.customer.org.app1.node02                         │
│       │                                                                  │
│       └── Instance Certificate (signed by realm)                         │
│           did:macula:io.customer.org.app2.node01                         │
└─────────────────────────────────────────────────────────────────────────┘
```

## Certificate Structure

### Macula Certificate Format

```erlang
#{
    %% Subject identity
    subject => #{
        did => <<"did:macula:io.customer.org.app.node01">>,
        common_name => <<"app.node01.io.customer.org">>
    },

    %% Issuer identity (realm for instance certs, self for realm certs)
    issuer => #{
        did => <<"did:macula:io.customer.org">>,
        common_name => <<"io.customer.org">>
    },

    %% Validity period (Unix timestamps)
    not_before => 1704067200,  %% 2024-01-01 00:00:00 UTC
    not_after => 1735689600,   %% 2025-01-01 00:00:00 UTC

    %% Ed25519 public key (32 bytes)
    public_key => <<...>>,

    %% Ed25519 signature (64 bytes)
    signature => <<...>>
}
```

## Using macula_cert

### Generate Keypair

```erlang
{PubKey, PrivKey} = macula_cert:generate_keypair().
%% PubKey: 32 bytes
%% PrivKey: 64 bytes (seed + public key)
```

### Create Realm Certificate (Self-Signed)

```erlang
RealmDID = <<"did:macula:io.example.org">>,
{PubKey, PrivKey} = macula_cert:generate_keypair(),

{ok, RealmCert} = macula_cert:generate_realm_cert(
    RealmDID,
    PubKey,
    PrivKey
).

%% Or with custom validity
{ok, RealmCert} = macula_cert:generate_realm_cert(
    RealmDID,
    PubKey,
    PrivKey,
    365  %% 365 days validity
).
```

### Create Instance Certificate (Signed by Realm)

```erlang
InstanceDID = <<"did:macula:io.example.org.app.node01">>,
{InstancePubKey, _} = macula_cert:generate_keypair(),

{ok, InstanceCert} = macula_cert:generate_instance_cert(
    InstanceDID,
    InstancePubKey,
    RealmCert,
    RealmPrivKey
).

%% Or with custom validity
{ok, InstanceCert} = macula_cert:generate_instance_cert(
    InstanceDID,
    InstancePubKey,
    RealmCert,
    RealmPrivKey,
    90  %% 90 days validity
).
```

### Verify Certificate

```erlang
%% Verify self-signed certificate
ok = macula_cert:verify_self_signed(RealmCert).

%% Verify certificate against issuer
ok = macula_cert:verify_cert(InstanceCert, RealmCert).

%% Check if certificate is currently valid
true = macula_cert:is_valid_now(InstanceCert).

%% Check if certificate is expired
false = macula_cert:is_expired(InstanceCert).
```

### Encode/Decode Certificates

```erlang
%% Encode to binary
{ok, Binary} = macula_cert:encode(Cert).

%% Decode from binary
{ok, Cert} = macula_cert:decode(Binary).

%% Convert to/from map
Map = macula_cert:to_map(Cert),
{ok, Cert} = macula_cert:from_map(Map).
```

## Trust Store

The trust store manages trusted realm certificates.

### Add Trusted Realm

```erlang
%% Start trust store (usually done by application)
{ok, _Pid} = macula_trust_store:start_link().

%% Add a trusted realm
ok = macula_trust_store:add_trusted_realm(RealmDID, RealmCert).

%% Add with custom trust level
ok = macula_trust_store:add_trusted_realm(RealmDID, RealmCert, high).
```

### Verify Instance Certificate

```erlang
%% Verify instance cert chains back to a trusted realm
ok = macula_trust_store:verify_instance_cert(InstanceCert).
```

### Query Trust Store

```erlang
%% Check if a realm is trusted
true = macula_trust_store:is_trusted(RealmDID).

%% Get realm certificate
{ok, RealmCert} = macula_trust_store:get_realm_cert(RealmDID).

%% List all trusted realms
TrustedRealms = macula_trust_store:list_trusted().
```

## DID Format

Macula DIDs follow a hierarchical namespace pattern:

```
did:macula:<reversed-domain>.<app>.<instance>

Examples:
- did:macula:io.example          (realm)
- did:macula:io.example.app      (application)
- did:macula:io.example.app.node01  (instance)
```

### DID to Common Name Conversion

```erlang
%% DID to CN (reverse the parts)
CN = macula_cert:did_to_cn(<<"did:macula:io.example.org.app.node01">>).
%% Result: <<"app.node01.io.example.org">>

%% CN to DID
DID = macula_cert:cn_to_did(<<"app.node01.io.example.org">>).
%% Result: <<"did:macula:io.example.org.app.node01">>
```

## Integration with bc-gitops

bc-gitops uses the certificate system for marketplace applications:

```erlang
%% In app.config
#{
    name => marketplace_app,
    certificate => #{
        generate => true,
        instance_did => <<"did:macula:io.customer.org.app.node01">>,
        realm_did => <<"did:macula:io.customer.org">>,
        validity_days => 90
    }
}.
```

### Certificate Generation in Deploy Flow

```erlang
%% bc_gitops_certificate:validate_and_prepare/1
case CertSpec#cert_spec.generate of
    true ->
        %% Generate new certificate
        {ok, #{cert_pem := CertPem, key_pem := KeyPem}} =
            bc_gitops_certificate:generate_certificate(CertSpec),
        %% Update app spec with generated cert
        UpdatedSpec = AppSpec#app_spec{
            certificate = CertSpec#cert_spec{
                cert_pem = CertPem,
                key_pem = KeyPem
            }
        },
        {ok, UpdatedSpec};
    false ->
        %% Use provided certificate
        {ok, AppSpec}
end.
```

## Security Considerations

### Key Storage

- Private keys should never leave the generating node
- Store keys in secure, encrypted storage
- Consider hardware security modules (HSM) for realm keys

### Certificate Rotation

- Instance certificates should have shorter validity (30-90 days)
- Realm certificates can have longer validity (1-5 years)
- Implement automatic renewal before expiration

### Revocation

Certificate revocation is not yet implemented. Current options:
- Short validity periods (forces re-issuance)
- Trust store removal (prevents verification)
- Future: CRL or OCSP-like mechanism via DHT

## Comparison with Traditional PKI

| Aspect | Self-Sovereign | Traditional PKI |
|--------|----------------|-----------------|
| Trust root | Realm certificate | External CA |
| Issuance | Instant, local | Requires CA interaction |
| Cost | Free | Often paid |
| Privacy | No third party | CA sees all certs |
| Revocation | Limited | CRL/OCSP |
| Interop | Macula ecosystem | Web browsers, etc. |

## Related Guides

- [Authorization Guide](AUTHORIZATION_GUIDE.md) - UCAN and capability-based auth
- [DHT Guide](DHT_GUIDE.md) - Distributed certificate discovery
- [Cluster API Guide](CLUSTER_API_GUIDE.md) - Using certificates in clusters
