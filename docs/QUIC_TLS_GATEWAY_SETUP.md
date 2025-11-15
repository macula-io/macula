# QUIC/TLS Gateway Setup for Macula

## Overview

Macula uses QUIC over HTTP/3 for mesh networking. QUIC has TLS 1.3 built into the protocol, which means proper TLS certificate configuration is essential for gateway mode.

## Gateway vs Edge Peer Modes

### Gateway Mode
- **Purpose**: Central connection point for mesh discovery and registry
- **Requirements**:
  - TLS certificates (cert + key)
  - Open QUIC port (default: 4433)
  - `start_gateway: true` configuration

### Edge Peer Mode
- **Purpose**: Connect to existing mesh via gateway
- **Requirements**:
  - Bootstrap registry URL pointing to gateway
  - No TLS certificates needed
  - `start_gateway: false` configuration

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

#### Gateway Container
```bash
MACULA_START_GATEWAY=true
MACULA_GATEWAY_PORT=4433
MACULA_REALM=my-app
MACULA_CERT_PATH=/opt/macula/certs/cert.pem
MACULA_KEY_PATH=/opt/macula/certs/key.pem
```

#### Edge Peer Container
```bash
MACULA_START_GATEWAY=false
MACULA_BOOTSTRAP_REGISTRY=https://gateway-hostname:4433
MACULA_REALM=my-app
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

### Docker Compose Example
```yaml
version: '3.8'
services:
  gateway:
    image: my-app:latest
    environment:
      - MACULA_START_GATEWAY=true
      - MACULA_GATEWAY_PORT=4433
      - MACULA_REALM=my-app
    ports:
      - "4433:4433"  # QUIC gateway port
      - "4000:4000"  # Phoenix HTTP port
    networks:
      - app-mesh

  peer1:
    image: my-app:latest
    environment:
      - MACULA_START_GATEWAY=false
      - MACULA_BOOTSTRAP_REGISTRY=https://gateway:4433
      - MACULA_REALM=my-app
    ports:
      - "4001:4000"
    networks:
      - app-mesh
    depends_on:
      - gateway

networks:
  app-mesh:
    driver: bridge
```

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

### Workaround: Single-Container Deployment

For applications that don't require multi-container mesh networking, use edge peer mode without gateway:

```elixir
config :macula,
  start_gateway: false,
  # No certificates needed in edge peer mode
  realm: "my-app"
```

This configuration works perfectly for single-container deployments where Phoenix.PubSub handles local event distribution.

## References

- QUIC Protocol: RFC 9000
- TLS 1.3: RFC 8446
- Subject Alternative Names: RFC 5280
- Macula Documentation: https://github.com/macula-io/macula
- quicer (Erlang QUIC): https://github.com/qzhuyan/quicer

## Next Steps

1. ✅ ~~Investigate quicer/MsQuic TLS requirements~~ - RESOLVED
2. ✅ ~~Review Macula gateway TLS configuration code~~ - ROOT CAUSE FOUND
3. Test with CA-signed certificates (production deployment)
4. Test multi-container gateway/edge peer mesh networking
5. Verify cross-container matchmaking functionality

---

**Last Updated**: 2025-11-15
**Macula Version**: 0.5.0
**Status**: ✅ RESOLVED - Two TLS configuration issues identified and fixed

**Root Causes**:
1. **Missing OS Environment Variables**: Elixir `config :macula, certfile: ...` does not set OS environment variables. The Erlang Macula gateway code reads `TLS_CERT_FILE` and `TLS_KEY_FILE` via `os:getenv/1`, requiring explicit `System.put_env/2` calls in Elixir's runtime configuration.

2. **Certificate File Ownership**: When copying TLS certificates into Docker images without `--chown=app:app`, files are owned by root:root. The application runs as user `app`, which cannot read root-owned files even with correct permissions, causing `config_error tls_error`.
