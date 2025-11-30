# QUIC/TLS Setup for Macula

> **Note (v0.11.0+):** For centralized TLS configuration using the new `macula_tls` module,
> see `TLS_CONFIGURATION.md` in this directory.

> **IMPORTANT (v0.8.5+):** This document contains some **outdated terminology** from before the
> always-on architecture. See the [Glossary](../GLOSSARY.md) for current terminology.

## Overview

Macula uses QUIC over HTTP/3 for mesh networking. QUIC has TLS 1.3 built into the protocol, which means proper TLS certificate configuration is essential.

## Architecture Change (v0.8.5+)

**Before v0.8.5:** Macula had separate "Gateway mode" and "Edge peer mode".

**After v0.8.5:** All nodes have identical capabilities. Every node runs:
- **Gateway System** - QUIC message routing
- **Bootstrap System** - DHT and peer discovery
- **Peer System** - Connection management

The distinction between "gateway" and "peer" no longer exists at the code level. However, you may still designate certain nodes as **seed nodes** - well-known entry points for initial mesh discovery.

### Seed Node (formerly "Gateway")
- A node whose address is known in advance
- New nodes connect here for initial DHT discovery
- **Same code as any other node** - just a deployment choice
- Requires:
  - TLS certificates (cert + key)
  - Open QUIC port (default: 4433)
  - Stable, well-known address

### Regular Node
- Connects to seed node(s) for initial discovery
- After joining mesh, functions identically to seed nodes
- Requires:
  - TLS certificates (auto-generated if not provided)
  - `MACULA_BOOTSTRAP_PEERS` pointing to seed node(s)

## TLS Certificate Requirements for QUIC

QUIC (RFC 9000) requires TLS 1.3 with specific certificate requirements:

### Required Certificate Extensions
1. **Subject Alternative Name (SAN)** - MANDATORY
   - DNS names and/or IP addresses
   - Example: `DNS:gateway.example.com,DNS:localhost,IP:127.0.0.1`

2. **Key Usage** - Server authentication

3. **Extended Key Usage** - TLS Web Server Authentication

### Certificate Generation

#### For Development/Testing
```bash
# Generate private key
openssl genrsa -out server-key.pem 2048

# Generate self-signed certificate with SAN
openssl req -new -x509 -key server-key.pem \
  -out server-cert.pem \
  -days 365 \
  -subj "/CN=macula-gateway/O=Your Org/C=US" \
  -addext "subjectAltName=DNS:macula-gateway,DNS:localhost,IP:127.0.0.1"
```

#### For Production
Use certificates from a trusted CA (Let's Encrypt, etc.) with proper SAN configuration.

## Configuration Pattern (Elixir/Phoenix)

### Runtime Configuration (config/runtime.exs)

```elixir
if config_env() == :prod do
  # Gateway mode toggle
  start_gateway = System.get_env("MACULA_START_GATEWAY") == "true"

  # Certificate paths (only used if start_gateway=true)
  cert_path = System.get_env("MACULA_CERT_PATH") || "/opt/macula/certs/cert.pem"
  key_path = System.get_env("MACULA_KEY_PATH") || "/opt/macula/certs/key.pem"

  config :macula,
    # Gateway mode: accepts incoming connections, provides registry
    # Edge peer mode: only makes outgoing connections
    start_gateway: start_gateway,

    # Bootstrap registry URL for initial mesh discovery
    # Gateway mode: can be omitted or point to another gateway
    # Edge peer mode: MUST point to a gateway
    bootstrap_registry: System.get_env("MACULA_BOOTSTRAP_REGISTRY") || "https://localhost:4433",

    # Realm for multi-tenancy
    realm: System.get_env("MACULA_REALM") || "default",

    # Gateway listen port (only used if start_gateway=true)
    gateway_port: String.to_integer(System.get_env("MACULA_GATEWAY_PORT") || "4433"),

    # TLS certificate paths (only used if start_gateway=true)
    certfile: cert_path,
    keyfile: key_path
end
```

### Environment Variables

#### Seed Node (v0.8.5+)
```bash
MACULA_QUIC_PORT=4433
MACULA_REALM=my-app
MACULA_TLS_CERTFILE=/opt/macula/certs/cert.pem
MACULA_TLS_KEYFILE=/opt/macula/certs/key.pem
# Seed nodes can optionally point to other seed nodes for redundancy
# MACULA_BOOTSTRAP_PEERS=quic://other-seed:4433
```

#### Regular Node (v0.8.5+)
```bash
MACULA_BOOTSTRAP_PEERS=quic://seed-hostname:4433
MACULA_REALM=my-app
# TLS certs auto-generated if not provided
```

#### Legacy Environment Variables (pre-v0.8.5, deprecated)
```bash
# These are no longer needed in v0.8.5+
# MACULA_START_GATEWAY=true/false  # No longer used
# MACULA_BOOTSTRAP_REGISTRY=...    # Use MACULA_BOOTSTRAP_PEERS instead
```

## Docker Setup

### Dockerfile
```dockerfile
# Copy TLS certificates to expected location
# Macula expects certificates at /opt/macula/certs/
RUN mkdir -p /opt/macula/certs
COPY priv/certs/server-cert.pem /opt/macula/certs/cert.pem
COPY priv/certs/server-key.pem /opt/macula/certs/key.pem
RUN chmod 644 /opt/macula/certs/cert.pem && chmod 600 /opt/macula/certs/key.pem
```

### Docker Compose Example (v0.8.5+)
```yaml
version: '3.8'
services:
  # Seed node - first to start, well-known entry point
  seed:
    image: my-app:latest
    environment:
      - MACULA_QUIC_PORT=4433
      - MACULA_REALM=my-app
      # TLS certs auto-generated if not provided
    ports:
      - "4433:4433"  # QUIC port
      - "4000:4000"  # Phoenix HTTP port
    networks:
      - app-mesh

  # Regular nodes - connect to seed for initial discovery
  node1:
    image: my-app:latest
    environment:
      - MACULA_BOOTSTRAP_PEERS=quic://seed:4433
      - MACULA_REALM=my-app
    ports:
      - "4001:4000"
    networks:
      - app-mesh
    depends_on:
      - seed

  node2:
    image: my-app:latest
    environment:
      - MACULA_BOOTSTRAP_PEERS=quic://seed:4433
      - MACULA_REALM=my-app
    ports:
      - "4002:4000"
    networks:
      - app-mesh
    depends_on:
      - seed

networks:
  app-mesh:
    driver: bridge
```

**Note:** In v0.8.5+, all nodes are identical. The "seed" label is just for clarity - after joining the mesh, node1 and node2 have the same capabilities as seed.

## Known Issues and Troubleshooting

### Issue: `QUIC listen failed: config_error tls_error`

**Symptoms**: Gateway fails to start with TLS configuration error

**Root Causes** (RESOLVED):

**Two critical configuration issues were identified and fixed:**

1. **Missing OS Environment Variables** (FIXED in config/runtime.exs)
   - The Erlang Macula gateway reads `TLS_CERT_FILE` and `TLS_KEY_FILE` via `os:getenv/1`
   - Elixir `config :macula, certfile: ...` does NOT set OS environment variables
   - **Fix**: Add explicit `System.put_env/2` calls in `config/runtime.exs`

2. **Certificate File Ownership** (FIXED in Dockerfile) - **MOST COMMON**
   - Certificate files copied into Docker image were owned by root:root
   - Application runs as user `app`, which couldn't read the private key
   - **Fix**: Add `--chown=app:app` to COPY commands in Dockerfile

**Other Possible Causes**:
3. Missing Subject Alternative Name (SAN) extension in certificate
4. Certificate/key mismatch
5. Incorrect file permissions on certificate files (644 for cert, 600 for key)

**Debugging Steps**:
```bash
# 1. Verify SAN extension
openssl x509 -in cert.pem -text -noout | grep -A2 "Subject Alternative Name"

# 2. Verify certificate/key match
openssl x509 -noout -modulus -in cert.pem | openssl md5
openssl rsa -noout -modulus -in key.pem | openssl md5
# (Should produce identical hashes)

# 3. Check file permissions
ls -la /opt/macula/certs/
# cert.pem should be 644, key.pem should be 600

# 4. CRITICAL: Check file ownership (Docker)
docker run --rm your-image ls -la /opt/macula/certs/
# Files should be owned by 'app:app', NOT 'root:root'

# 5. Verify app user can read the key
docker run --rm --user app your-image cat /opt/macula/certs/key.pem | head -3
# Should display "-----BEGIN PRIVATE KEY-----", NOT "Permission denied"
```

**Solutions** (VERIFIED):

**Fix 1: Set OS Environment Variables** (config/runtime.exs)

When using Elixir/Phoenix with Macula, you must explicitly set OS environment variables in `config/runtime.exs`:

```elixir
if config_env() == :prod do
  cert_path = System.get_env("MACULA_CERT_PATH") || "/opt/macula/certs/cert.pem"
  key_path = System.get_env("MACULA_KEY_PATH") || "/opt/macula/certs/key.pem"
  realm = System.get_env("MACULA_REALM") || "macula.arcade"

  # CRITICAL: Set OS environment variables for Erlang Macula code
  System.put_env("TLS_CERT_FILE", cert_path)
  System.put_env("TLS_KEY_FILE", key_path)
  System.put_env("MACULA_REALM", realm)

  config :macula,
    start_gateway: true,
    gateway_port: 4433,
    realm: realm,
    # ... other config
end
```

The Macula gateway (`macula_gateway.erl`) and supervisor (`macula_sup.erl`) read configuration via `os:getenv/1`:
- `TLS_CERT_FILE` and `TLS_KEY_FILE` - TLS certificate paths
- `MACULA_REALM` - Realm for multi-tenancy isolation

These environment variables are NOT read from Elixir Application configuration.

**Fix 2: Set Certificate File Ownership** (Dockerfile)

When copying TLS certificates into Docker images, **you must set the ownership to the application user**:

```dockerfile
# Create app user (typically done in earlier stage)
RUN useradd --create-home app
WORKDIR /home/app

# Copy release and certificates with proper ownership
COPY --from=builder --chown=app:app /app/_build/prod/rel/your_app ./

# CRITICAL: Set ownership when copying certificates
RUN mkdir -p /opt/macula/certs
COPY --from=builder --chown=app:app /app/priv/certs/server-cert.pem /opt/macula/certs/cert.pem
COPY --from=builder --chown=app:app /app/priv/certs/server-key.pem /opt/macula/certs/key.pem
RUN chmod 644 /opt/macula/certs/cert.pem && chmod 600 /opt/macula/certs/key.pem

# Switch to app user
USER app
```

**Why this matters**: The application runs as user `app`, but files copied without `--chown` are owned by `root`. Even with 600 permissions, the app user cannot read root-owned files, causing the TLS error.

### Note: Single-Container Deployment (v0.8.5+)

For single-container deployments that don't require multi-node mesh networking, Macula still starts all subsystems but simply won't have any peers to connect to:

```elixir
config :macula,
  realm: "my-app"
  # TLS certs auto-generated
  # No bootstrap peers needed for single-node
```

In v0.8.5+, a single node operates normally with all subsystems running. Phoenix.PubSub handles local event distribution, and the mesh features become active when peers connect.

## References

- QUIC Protocol: RFC 9000
- TLS 1.3: RFC 8446
- Subject Alternative Names: RFC 5280
- Macula Documentation: https://github.com/macula-io/macula
- quicer (Erlang QUIC): https://github.com/qzhuyan/quicer

## Next Steps

1. ✅ ~~Investigate quicer/MsQuic TLS requirements~~ - RESOLVED
2. ✅ ~~Review Macula Gateway System TLS configuration code~~ - ROOT CAUSE FOUND
3. Test with CA-signed certificates (production deployment)
4. Test multi-node mesh networking
5. Verify cross-container service discovery

---

**Last Updated**: 2025-11-28
**Macula Version**: v0.11.0
**Status**: ✅ RESOLVED - TLS issues fixed; terminology updated for v0.8.5+ architecture

**Historical Note** (pre-v0.8.5):
The original TLS issues were:
1. **Missing OS Environment Variables**: Elixir `config :macula, certfile: ...` did not set OS environment variables
2. **Certificate File Ownership**: Docker COPY without `--chown` caused permission issues

**Current Status** (v0.8.5+):
- TLS certificates auto-generated if not provided
- All nodes have identical capabilities (no gateway/peer mode distinction)
- See `TLS_CONFIGURATION.md` for comprehensive TLS setup guide
