# License System Guide (UCAN-based)

This guide covers the UCAN-based license system used for Macula marketplace applications.

## Overview

Macula uses UCAN (User Controlled Authorization Networks) tokens for software licensing. UCANs provide:

- **Capability-based**: Fine-grained permissions
- **Delegatable**: Licenses can be sub-licensed
- **Decentralized**: No central license server required
- **Cryptographically secure**: Ed25519 signatures
- **Self-contained**: All info in the token

## License Models

| Model | Description | UCAN Configuration |
|-------|-------------|-------------------|
| **Free** | No restrictions | Auto-granted, no expiration |
| **Trial** | Time-limited evaluation | Short `exp` (14-30 days) |
| **Perpetual** | One-time purchase | No expiration |
| **Subscription** | Recurring payment | `exp` set to subscription end |
| **Per-seat** | Limited installations | `nb.max_nodes` claim |

## UCAN Structure

### JWT Format

```
eyJhbGciOiJFZERTQSIsInR5cCI6IkpXVCJ9.
eyJpc3MiOiJkaWQ6bWFjdWxhOmlvLnB1Ymxpc2hlci5vcmciLCJhdWQiOiJkaWQ6bWFjdWxhOmlvLmN1c3RvbWVyLm9yZyIsImV4cCI6MTczNTY4OTYwMCwiYXR0IjpbeyJ3aXRoIjoiZGlkOm1hY3VsYTppby5wdWJsaXNoZXIub3JnL2FwcC9teV9hcHAiLCJjYW4iOiJhcnRpZmFjdC9ydW4ifV19.
<signature>
```

### Decoded Payload

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

### Field Definitions

| Field | Description |
|-------|-------------|
| `iss` | Issuer DID (publisher) |
| `aud` | Audience DID (licensee) |
| `exp` | Expiration timestamp (Unix) |
| `nbf` | Not before timestamp (Unix) |
| `att` | Attenuations (capabilities granted) |
| `fct` | Facts (additional claims like max_nodes) |

## Capability Types

### artifact/run

Permission to run the application:

```json
{
  "with": "did:macula:io.publisher.org/app/my_app",
  "can": "artifact/run"
}
```

### artifact/update

Permission to update to new versions:

```json
{
  "with": "did:macula:io.publisher.org/app/my_app",
  "can": "artifact/update"
}
```

### artifact/install

Permission to install the application:

```json
{
  "with": "did:macula:io.publisher.org/app/my_app",
  "can": "artifact/install"
}
```

## Integration with bc-gitops

### License Spec in app.config

```erlang
#{
    name => my_app,
    version => <<"1.0.0">>,
    source => #{type => mesh, mcid => <<"bafk...">>},
    license => #{
        ucan => <<"eyJhbGciOiJFZERTQSIsInR5cCI6IkpXVCJ9...">>,
        model => subscription,
        expires_at => 1735689600,
        max_nodes => 5
    }
}.
```

### bc_gitops_license Module

```erlang
%% Validate license before deployment
case bc_gitops_license:validate(LicenseSpec, #{app_name => Name}) of
    ok ->
        %% License is valid, proceed
        deploy(AppSpec);
    {warning, expiring_soon} ->
        %% License valid but expiring within 7 days
        log_warning("License expiring soon"),
        deploy(AppSpec);
    {error, expired} ->
        %% License has expired
        {error, {license_validation_failed, expired}};
    {error, invalid_signature} ->
        %% UCAN signature verification failed
        {error, {license_validation_failed, invalid_signature}};
    {error, node_limit_exceeded} ->
        %% Too many nodes for this license
        {error, {license_validation_failed, node_limit_exceeded}}
end.
```

### License Status Tracking

The license status is stored in the application state:

```erlang
#app_state{
    name = my_app,
    license_status = valid,  %% valid | expired | invalid | unlicensed
    ...
}.
```

## License Procurement Flow

### Free Applications

```
User clicks Install
    ↓
LicenseService.procure_license(app, user, [])
    ↓
Auto-grant UCAN with no expiration
    ↓
Return {:ok, license}
```

### Trial Applications

```
User clicks "Start Trial"
    ↓
LicenseService.procure_license(app, user, [trial: true])
    ↓
Generate UCAN with 14-day expiration
    ↓
Return {:ok, license}
```

### Paid Applications

```
User clicks Install
    ↓
LicenseService.procure_license(app, user, [])
    ↓
Return {:error, :payment_required}
    ↓
User completes payment
    ↓
Payment system issues UCAN
    ↓
User retries install with UCAN
```

## Validation Details

### Macula Authorization Integration

When `macula_authorization` is available, bc-gitops delegates validation:

```erlang
validate_with_macula(UcanBinary, Context) ->
    case macula_authorization:validate_ucan_for_operation(
        UcanBinary,
        <<"artifact/run">>,
        maps:get(app_name, Context),
        #{}
    ) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.
```

### Fallback Validation

Without macula, bc-gitops performs basic JWT validation:

```erlang
validate_fallback(LicenseSpec) ->
    %% Check expiration
    case check_expiry(LicenseSpec) of
        ok -> ok;
        {warning, _} = W -> W;
        {error, _} = E -> E
    end.
```

## Node Limit Enforcement

For per-seat licenses:

```erlang
%% Check if adding another node exceeds the limit
check_node_limit(LicenseSpec, CurrentNodeCount) ->
    case LicenseSpec#license_spec.max_nodes of
        undefined ->
            %% No limit
            ok;
        MaxNodes when CurrentNodeCount < MaxNodes ->
            %% Within limit
            ok;
        MaxNodes ->
            %% At or over limit
            {error, {node_limit_exceeded, MaxNodes}}
    end.
```

## Expiration Handling

### Warning Period

A warning is issued 7 days before expiration:

```erlang
-define(EXPIRY_WARNING_DAYS, 7).

check_expiry(#license_spec{expires_at = undefined}) ->
    ok;  %% Perpetual license
check_expiry(#license_spec{expires_at = ExpiresAt}) ->
    Now = erlang:system_time(second),
    WarningThreshold = Now + (?EXPIRY_WARNING_DAYS * 86400),
    if
        ExpiresAt < Now ->
            {error, expired};
        ExpiresAt < WarningThreshold ->
            DaysRemaining = (ExpiresAt - Now) div 86400,
            {warning, {expiring_soon, DaysRemaining}};
        true ->
            ok
    end.
```

### Renewal

License renewal requires obtaining a new UCAN:

1. User/system requests renewal from publisher
2. Payment processed (if subscription)
3. Publisher issues new UCAN with extended expiration
4. Update app.config with new UCAN
5. bc-gitops reconciles with new license

## Security Considerations

### Token Storage

- Store UCAN tokens securely (encrypted at rest)
- Tokens contain capabilities - treat as credentials
- Don't log full tokens (truncate in logs)

### Signature Verification

- Always verify Ed25519 signatures
- Check issuer DID matches expected publisher
- Verify audience DID matches your realm

### Revocation

UCAN revocation is handled via:

1. **Short expiration**: Forces periodic revalidation
2. **Revocation list**: `macula_ucan_revocation` module
3. **DHT-based CRL**: Future enhancement

## Telemetry Events

bc-gitops emits telemetry for license operations:

| Event | Description |
|-------|-------------|
| `[bc_gitops, license, validate, start]` | License validation started |
| `[bc_gitops, license, validate, stop]` | License validation completed |
| `[bc_gitops, license, expired]` | License expiration detected |
| `[bc_gitops, license, expiring_soon]` | Approaching expiration |

## Related Guides

- [Self-Sovereign Certs](SELF_SOVEREIGN_CERTS.md) - Certificate system
- [Authorization Guide](AUTHORIZATION_GUIDE.md) - Full authorization system
- [Mesh Source Guide](../../bc-gitops/guides/mesh_source.md) - Using mesh sources
