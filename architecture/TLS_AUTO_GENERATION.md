# Macula TLS Auto-Generation Strategy

**Date:** 2025-11-18
**Updated:** 2025-11-28
**Status:** Phase 1 COMPLETED (v0.11.0), Phases 2-4 Planned
**Context:** Zero-config TLS for scalable P2P mesh deployment

> **Note:** Phase 1 (Basic Auto-Generation) was implemented in v0.11.0.
> See `docs/operator/TLS_CONFIGURATION.md` for operator documentation.
> See `src/macula_tls.erl` for implementation.

---

## Problem

Every Macula node needs TLS certificates for QUIC connections. Manual certificate management doesn't scale:
- ❌ Manual cert generation for each node (hundreds/thousands of nodes)
- ❌ Certificate distribution and rotation
- ❌ CA infrastructure for private meshes
- ❌ DNS requirements for Let's Encrypt
- ❌ Public internet access requirements

---

## Solution: Auto-Generated Self-Signed Certificates

**Strategy:**
1. Node generates self-signed certificate on first boot
2. Certificate persisted to disk (survives restarts)
3. Node ID = hash of public key (cryptographic identity)
4. Trust model: opt-in verification (default accepts all, can pin trusted peers)

---

## Implementation Design

### 1. Certificate Generation (First Boot)

```erlang
%% On startup, check if cert exists
CertPath = application:get_env(macula, cert_path, "/var/lib/macula/cert.pem"),
KeyPath = application:get_env(macula, key_path, "/var/lib/macula/key.pem"),

case {filelib:is_file(CertPath), filelib:is_file(KeyPath)} of
    {true, true} ->
        %% Certificates exist, load them
        {ok, CertPath, KeyPath};

    {false, false} ->
        %% No certificates, generate new ones
        {Cert, Key} = macula_tls:generate_self_signed_cert(#{
            common_name => generate_node_name(),
            validity_days => 3650,  % 10 years
            key_type => rsa,
            key_bits => 2048
        }),

        %% Persist to disk
        ok = filelib:ensure_dir(CertPath),
        ok = file:write_file(CertPath, Cert),
        ok = file:write_file(KeyPath, Key),

        %% Set proper permissions (readable only by owner)
        ok = file:change_mode(KeyPath, 8#00600),

        io:format("Generated new TLS certificate: ~s~n", [CertPath]),
        {ok, CertPath, KeyPath};

    _ ->
        %% Inconsistent state (only one file exists)
        {error, inconsistent_cert_state}
end
```

### 2. Node ID Derivation

```erlang
%% Node ID = SHA-256 hash of public key
%% This provides:
%% - Stable identity (same cert = same ID)
%% - Cryptographic verifiability
%% - No coordination needed

-spec derive_node_id(binary()) -> binary().
derive_node_id(CertPEM) ->
    %% Extract public key from certificate
    {ok, Cert} = public_key:pem_decode(CertPEM),
    PublicKey = extract_public_key(Cert),

    %% Hash public key to create Node ID
    crypto:hash(sha256, PublicKey).

%% Example Node ID: <<"a3f5e8d2c9b4...">> (32 bytes)
```

### 3. Certificate Storage

```
Default paths (Linux):
  /var/lib/macula/cert.pem  (certificate)
  /var/lib/macula/key.pem   (private key, mode 0600)

Docker/Container:
  /opt/macula/certs/cert.pem
  /opt/macula/certs/key.pem
  (mount volume to persist across restarts)

Environment variable override:
  MACULA_CERT_PATH=/custom/path/cert.pem
  MACULA_KEY_PATH=/custom/path/key.pem
```

### 4. Certificate Rotation (Optional, Future)

```erlang
%% v1.0.0: Automatic rotation before expiry
%% Check cert expiry on startup
case macula_tls:check_cert_expiry(CertPath) of
    {ok, DaysRemaining} when DaysRemaining < 30 ->
        %% Cert expires in <30 days, rotate
        io:format("Certificate expires in ~p days, rotating...~n", [DaysRemaining]),
        macula_tls:rotate_certificate(CertPath, KeyPath);

    {ok, DaysRemaining} ->
        io:format("Certificate valid for ~p more days~n", [DaysRemaining]),
        ok;

    {error, expired} ->
        %% Already expired, generate new
        io:format("Certificate expired, generating new...~n"),
        macula_tls:rotate_certificate(CertPath, KeyPath)
end
```

---

## Trust Model

### Default: Accept All (SSH-style "first connect")

```erlang
%% On first connection to peer, accept any certificate
%% Similar to SSH on first connect:
%%   "The authenticity of host '...' can't be established.
%%    ECDSA key fingerprint is SHA256:...
%%    Are you sure you want to continue connecting (yes/no)?"

%% For Macula:
%% - First connect: Accept and optionally pin
%% - Subsequent connects: Verify against pinned cert
```

**Pros:**
- Zero configuration
- Works out of the box
- No external dependencies

**Cons:**
- Vulnerable to MITM on first connect (like SSH)
- Acceptable for private meshes
- Can upgrade to pinning for critical peers

### Optional: Pin Trusted Peers

```erlang
%% Pin specific peer certificates (like SSH known_hosts)
PinnedPeers = #{
    %% NodeID => PublicKeyHash
    <<"a3f5e8d2...">> => <<"cert_hash_1">>,
    <<"b7c2f9a4...">> => <<"cert_hash_2">>
},

%% On connection, verify:
verify_peer_cert(NodeID, PeerCert, PinnedPeers) ->
    case maps:get(NodeID, PinnedPeers, undefined) of
        undefined ->
            %% Not pinned, accept (first connect)
            {ok, accept_and_pin};

        ExpectedHash ->
            %% Pinned, verify hash
            ActualHash = crypto:hash(sha256, extract_public_key(PeerCert)),
            case ActualHash of
                ExpectedHash -> {ok, verified};
                _ -> {error, cert_mismatch}
            end
    end.
```

**Configuration:**
```erlang
%% sys.config
{macula, [
    {peer_pinning, enabled},  % or disabled
    {pinned_peers, [
        {<<"bootstrap-node">>, <<"cert_hash_base64">>}
    ]}
]}
```

### Optional: DHT-Based Verification (Future)

```erlang
%% Store public keys in DHT
%% Verify peer certificates against DHT records

%% On peer advertisement:
macula_dht:store(NodeID, #{
    public_key_hash => Hash,
    advertised_at => Timestamp
}),

%% On connection:
case macula_dht:lookup(PeerNodeID) of
    {ok, #{public_key_hash := ExpectedHash}} ->
        verify_cert_hash(PeerCert, ExpectedHash);
    {error, not_found} ->
        %% Not in DHT, accept (first connect)
        {ok, accept}
end
```

**Pros:** Decentralized verification
**Cons:** DHT must be bootstrapped first (chicken-and-egg)

---

## Certificate Properties

### Generated Certificate Details

```
Subject: CN=macula-node-<random-id>
Issuer: CN=macula-node-<random-id> (self-signed)
Validity: 10 years (3650 days)
Key Type: RSA 2048-bit (configurable)
Extensions:
  - Subject Alternative Name: DNS:localhost, IP:127.0.0.1
  - Key Usage: Digital Signature, Key Encipherment
  - Extended Key Usage: Server Authentication, Client Authentication
```

### Why 10 Years Validity?

- Long enough to avoid frequent rotation
- Short enough to force eventual rotation
- Rotation happens before expiry (auto-renew at 9 years)

### Key Type Options

```erlang
%% RSA 2048 (default - widely compatible)
{key_type, rsa},
{key_bits, 2048}

%% RSA 4096 (more secure, slower)
{key_type, rsa},
{key_bits, 4096}

%% ECDSA P-256 (fast, smaller keys)
{key_type, ecdsa},
{curve, secp256r1}

%% ECDSA P-384 (more secure)
{key_type, ecdsa},
{curve, secp384r1}
```

**Recommendation:** RSA 2048 for v0.9.0 (compatibility), ECDSA P-256 for v1.0.0 (performance)

---

## Security Considerations

### Attack Vectors

1. **MITM on First Connect**
   - **Risk:** Attacker intercepts first connection, presents fake cert
   - **Mitigation:** Pin bootstrap nodes, verify via out-of-band channel
   - **Severity:** Medium (mitigated by pinning critical peers)

2. **Node ID Collision**
   - **Risk:** Two nodes generate same Node ID (SHA-256 collision)
   - **Mitigation:** SHA-256 collision resistance (2^256 space)
   - **Severity:** Negligible (astronomically unlikely)

3. **Private Key Compromise**
   - **Risk:** Attacker steals private key from disk
   - **Mitigation:** File permissions (0600), encrypted storage (future)
   - **Severity:** High (requires physical/root access)

4. **Certificate Spoofing**
   - **Risk:** Attacker generates cert with same Node ID
   - **Mitigation:** Node ID = hash of public key (impossible to spoof)
   - **Severity:** None (cryptographically impossible)

### Security vs. Usability Trade-off

```
┌────────────────────┬──────────────┬────────────┬──────────────┐
│ Trust Model        │ Security     │ Usability  │ Use Case     │
├────────────────────┼──────────────┼────────────┼──────────────┤
│ Accept All         │ Low          │ High       │ Dev/Private  │
│ (default)          │              │            │ mesh         │
├────────────────────┼──────────────┼────────────┼──────────────┤
│ Pin Critical       │ Medium       │ Medium     │ Production   │
│ (bootstrap nodes)  │              │            │ mesh         │
├────────────────────┼──────────────┼────────────┼──────────────┤
│ Pin All Peers      │ High         │ Low        │ Sensitive    │
│ (full pinning)     │              │            │ data         │
├────────────────────┼──────────────┼────────────┼──────────────┤
│ DHT Verification   │ Medium       │ High       │ Large mesh   │
│ (future)           │              │            │ (1000+ nodes)│
└────────────────────┴──────────────┴────────────┴──────────────┘
```

---

## Implementation Plan

### Phase 1: Basic Auto-Generation (COMPLETED - v0.11.0)

**Status:** IMPLEMENTED

**Implementation (v0.11.0):**
- ✅ Auto-generate self-signed certs on first boot
- ✅ Persist to disk (configurable path)
- ✅ Derive Node ID from public key hash
- ✅ Two TLS modes: `development` (no verification) and `production` (strict verification)
- ✅ CA bundle configuration for production mode
- ✅ Hostname verification in production mode
- ✅ Environment variable configuration (MACULA_TLS_MODE, MACULA_TLS_CACERTFILE, etc.)

**Module:** `macula_tls.erl`
**Functions:**
- `generate_self_signed_cert/1` → `{Cert, Key}`
- `derive_node_id/1` → `NodeID`
- `ensure_cert_exists/2` → `{ok, CertPath, KeyPath} | {error, Reason}`
- `get_tls_mode/0` → `production | development`
- `quic_client_opts/0` → QUIC client TLS options
- `quic_server_opts/0` → QUIC server TLS options
- `quic_client_opts_with_hostname/1` → Hostname-aware client TLS options

**Tests:** 29 tests passing (`macula_tls_tests.erl`)

**Documentation:**
- `docs/operator/TLS_CONFIGURATION.md` - Operator guide
- `scripts/setup-dev-tls.sh` - Development certificate generation

---

### Phase 2: Certificate Pinning (v0.9.0 or v1.0.0)

**Scope:**
- ✅ Pin specific peer certificates
- ✅ Verify pinned peers on connect
- ✅ Reject cert mismatches
- ✅ Configuration via sys.config

**Module:** `macula_tls_pinning.erl`
**Functions:**
- `pin_peer/2` → `ok`
- `verify_peer/3` → `{ok, verified} | {error, mismatch}`
- `list_pinned_peers/0` → `[{NodeID, Hash}]`

**Configuration:**
```erlang
{macula, [
    {peer_pinning, enabled},
    {pinned_peers, [
        {<<"bootstrap-1">>, <<"hash1">>},
        {<<"bootstrap-2">>, <<"hash2">>}
    ]}
]}
```

---

### Phase 3: Certificate Rotation (v1.0.0)

**Scope:**
- ✅ Check cert expiry on startup
- ✅ Auto-rotate if <30 days remaining
- ✅ Notify peers of new certificate
- ✅ Graceful transition (accept old cert for N days)

**Module:** `macula_tls_rotation.erl`
**Functions:**
- `check_expiry/1` → `{ok, DaysRemaining} | {error, expired}`
- `rotate_certificate/2` → `{ok, NewCert, NewKey}`
- `notify_peers_of_rotation/1` → `ok`

---

### Phase 4: DHT-Based Verification (v1.0.0+)

**Scope:**
- ✅ Store public key hashes in DHT
- ✅ Verify peer certs against DHT
- ✅ Handle DHT unavailability gracefully
- ✅ Cache DHT results (TTL)

**Module:** `macula_tls_dht.erl`
**Functions:**
- `advertise_cert/2` → `ok`
- `verify_via_dht/2` → `{ok, verified} | {error, mismatch}`
- `cache_dht_result/3` → `ok`

---

## Configuration Reference

### Environment Variables

```bash
# Certificate paths (auto-generate if missing)
MACULA_CERT_PATH=/var/lib/macula/cert.pem
MACULA_KEY_PATH=/var/lib/macula/key.pem

# Certificate generation options
MACULA_CERT_VALIDITY_DAYS=3650    # 10 years
MACULA_CERT_KEY_TYPE=rsa          # rsa|ecdsa
MACULA_CERT_KEY_BITS=2048         # 2048|4096 (RSA) or curve (ECDSA)

# Trust model
MACULA_PEER_PINNING=disabled      # enabled|disabled
MACULA_DHT_VERIFICATION=disabled  # enabled|disabled (future)
```

### Application Config (sys.config)

```erlang
{macula, [
    %% Certificate paths
    {cert_path, "/var/lib/macula/cert.pem"},
    {key_path, "/var/lib/macula/key.pem"},

    %% Certificate generation
    {cert_validity_days, 3650},
    {cert_key_type, rsa},
    {cert_key_bits, 2048},

    %% Trust model
    {peer_pinning, disabled},
    {pinned_peers, []},
    {dht_verification, disabled}  % Future
]}
```

---

## Docker/Container Deployment

### Volume Mount for Cert Persistence

```yaml
services:
  macula-node:
    image: macula:latest
    volumes:
      - macula-certs:/opt/macula/certs  # Persist certificates
    environment:
      MACULA_CERT_PATH: /opt/macula/certs/cert.pem
      MACULA_KEY_PATH: /opt/macula/certs/key.pem

volumes:
  macula-certs:
```

**First Start:**
- Volume is empty → certs auto-generated → Node ID assigned

**Subsequent Starts:**
- Volume has certs → certs loaded → same Node ID (stable identity)

**Container Recreation:**
- Volume persists → same certs → same Node ID

---

## Migration Path

### From Manual Certs to Auto-Generated

```erlang
%% Detect existing manual cert
case {filelib:is_file(CertPath), filelib:is_file(KeyPath)} of
    {true, true} ->
        %% Check if manually created or auto-generated
        case file:read_file_info(CertPath) of
            {ok, #file_info{mtime = MTime}} ->
                %% If cert is old (manual), keep it
                %% If cert is new (auto-generated), use it
                io:format("Using existing certificate: ~s~n", [CertPath]),
                {ok, CertPath, KeyPath}
        end;

    _ ->
        %% No cert, auto-generate
        generate_self_signed_cert(...)
end
```

### Backward Compatibility

- v0.8.x: Requires manual cert configuration
- v0.9.0+: Auto-generates if missing, uses existing if present
- No breaking changes (existing deployments continue working)

---

## Testing Strategy

### Unit Tests
- Certificate generation (valid PEM format)
- Node ID derivation (consistent, deterministic)
- Persistence and reload
- Error handling (corrupted certs, missing files)

### Integration Tests
- Two nodes connect with auto-generated certs
- Node restart preserves identity (same Node ID)
- Pinned peer verification
- Cert mismatch rejection

### End-to-End Tests
- Multi-node mesh with auto-generated certs
- Node joins mesh without manual cert config
- Bootstrap node with pinned cert

---

## Performance Impact

### Startup Overhead

```
First boot (generate cert):
  - RSA 2048: ~100-200ms
  - RSA 4096: ~500ms-1s
  - ECDSA P-256: ~10-50ms

Subsequent boots (load cert):
  - File I/O: ~1-5ms
  - Negligible impact
```

### Runtime Overhead

- Zero overhead (certs loaded once at startup)
- Same as manual cert configuration

---

## Related Documents

- `architecture/FULL_SUPERVISION_TREE.md` - Complete supervision hierarchy
- `architecture/NAT_TRAVERSAL_ROADMAP.md` - P2P connectivity
- `architecture/v0.9.0-CONSISTENCY-CONCERNS.md` - Correlation mechanisms

---

## Summary

**v0.9.0 Auto-Generation Strategy:**

✅ **Zero-config deployment** - No manual cert management
✅ **Stable identity** - Node ID derived from public key
✅ **Persistent across restarts** - Certs stored on disk
✅ **Scalable** - Works for 1 node or 10,000 nodes
✅ **Secure by default** - TLS encryption always enabled
✅ **Opt-in verification** - Pin critical peers if needed
✅ **Container-friendly** - Volume mount for persistence
✅ **Backward compatible** - Uses existing certs if present

**Trust model progression:**
- v0.9.0: Accept all (like SSH first connect)
- v1.0.0: Pin critical peers (bootstrap nodes)
- v1.0.0+: DHT verification (optional)

This provides the perfect balance of **usability** (zero-config) and **security** (TLS encryption + opt-in pinning).
