# Macula TLS Guide

This guide covers TLS configuration for Macula's QUIC connections, including development setup, production deployment, and troubleshooting.

## Overview

Macula uses QUIC over HTTP/3 for mesh networking. QUIC has TLS 1.3 built into the protocol, which means proper TLS certificate configuration is essential.

Since v0.8.5, Macula uses an **always-on architecture** where every node has identical capabilities. The distinction between "gateway" and "peer" no longer exists at the code level. Certain nodes may be designated as **seed nodes** (well-known entry points for initial mesh discovery), but they run the same code.

---

## Quick Start

### Development Mode (Default)

Development mode uses self-signed certificates with no verification:

```erlang
%% config/sys.config
{macula, [
    {tls_mode, development}
]}
```

Or via environment variable:
```bash
export MACULA_TLS_MODE=development
```

Macula automatically generates self-signed certificates on first startup if they don't exist.

### Production Mode

Production mode enables strict certificate verification:

```erlang
%% config/sys.config
{macula, [
    {tls_mode, production},
    {tls_cacertfile, "/etc/ssl/certs/ca-certificates.crt"},
    {tls_certfile, "/etc/macula/server.crt"},
    {tls_keyfile, "/etc/macula/server.key"},
    {tls_verify_hostname, true}
]}
```

Or via environment variables:
```bash
export MACULA_TLS_MODE=production
export MACULA_TLS_CACERTFILE=/etc/ssl/certs/ca-certificates.crt
export MACULA_TLS_CERTFILE=/etc/macula/server.crt
export MACULA_TLS_KEYFILE=/etc/macula/server.key
export MACULA_TLS_VERIFY_HOSTNAME=true
```

---

## Configuration Reference

### TLS Mode

| Option | Description |
|--------|-------------|
| `development` | Self-signed certificates, no verification (default) |
| `production` | Strict certificate verification, requires CA bundle |

**Environment Variable**: `MACULA_TLS_MODE` (shortcuts: `dev`, `prod`)

### All Configuration Options

| Environment Variable | Application Config | Description | Default |
|---------------------|-------------------|-------------|---------|
| `MACULA_TLS_MODE` | `{tls_mode, atom()}` | `production` or `development` | `development` |
| `MACULA_TLS_CACERTFILE` | `{tls_cacertfile, Path}` | CA bundle path | System default |
| `MACULA_TLS_CERTFILE` | `{tls_certfile, Path}` | Certificate path | Auto-generated |
| `MACULA_TLS_KEYFILE` | `{tls_keyfile, Path}` | Private key path | Auto-generated |
| `MACULA_TLS_VERIFY_HOSTNAME` | `{tls_verify_hostname, bool()}` | Hostname verification | `true` in production |

### CA Certificate Bundle

If not specified in production mode, Macula checks these system default locations:
- `/etc/ssl/certs/ca-certificates.crt` (Debian/Ubuntu)
- `/etc/pki/tls/certs/ca-bundle.crt` (RHEL/CentOS)
- `/etc/ssl/cert.pem` (Alpine/macOS)

---

## Certificate Requirements for QUIC

QUIC (RFC 9000) requires TLS 1.3 with specific certificate requirements:

### RSA Keys Required

> **CRITICAL**: MsQuic (the QUIC implementation used by Macula) does **NOT** support ECDSA certificates. Always use RSA keys. Let's Encrypt switched to ECDSA by default in late 2024, so you must explicitly request RSA when using certbot.

### Required Certificate Extensions

1. **Subject Alternative Name (SAN)** - MANDATORY
   - DNS names and/or IP addresses
   - Example: `DNS:gateway.example.com,DNS:localhost,IP:127.0.0.1`
2. **Key Usage** - digitalSignature, keyEncipherment
3. **Extended Key Usage** - serverAuth, clientAuth (for mTLS)

---

## Development Certificate Setup

### Auto-Generation

Macula automatically generates self-signed certificates on first startup in development mode.

### Manual Generation

```bash
# Using the provided script
sudo ./scripts/setup-dev-tls.sh

# Options:
#   --output-dir DIR   Output directory (default: /var/lib/macula)
#   --validity DAYS    Certificate validity (default: 3650)
#   --hostname NAME    Primary hostname (default: macula-node)
```

Or manually with OpenSSL:

```bash
# Generate RSA private key (NOT ECDSA)
openssl genrsa -out server-key.pem 2048

# Generate self-signed certificate with SAN
openssl req -new -x509 -key server-key.pem \
  -out server-cert.pem \
  -days 365 \
  -subj "/CN=macula-gateway/O=Your Org/C=US" \
  -addext "subjectAltName=DNS:macula-gateway,DNS:localhost,IP:127.0.0.1"
```

---

## Production Deployment

### Obtaining Certificates

**Let's Encrypt (recommended for public-facing nodes):**
```bash
# IMPORTANT: Use --key-type rsa (ECDSA not supported by MsQuic)
certbot certonly --standalone -d your-node.example.com \
  --key-type rsa --rsa-key-size 2048
```

**Verify certificate is RSA:**
```bash
openssl x509 -in /path/to/cert.pem -noout -text | grep "Public Key Algorithm"
# Must show: rsaEncryption (NOT id-ecPublicKey)
```

To re-issue with RSA if you have an ECDSA certificate:
```bash
certbot certonly --standalone -d your-node.example.com \
  --key-type rsa --rsa-key-size 2048 --force-renewal
```

**Internal CA** (for private mesh networks): Generate certificates with your organization's CA.

### Certificate Rotation

To rotate certificates without downtime:

1. Generate new certificate with same or new key
2. Update the certificate files
3. Restart nodes one at a time (rolling restart)

---

## Node Configuration

### Seed Node

```bash
MACULA_QUIC_PORT=4433
MACULA_REALM=my-app
MACULA_TLS_CERTFILE=/opt/macula/certs/cert.pem
MACULA_TLS_KEYFILE=/opt/macula/certs/key.pem
```

Requirements:
- TLS certificates (cert + key)
- Open QUIC port (default: 4433)
- Stable, well-known address

### Regular Node

```bash
MACULA_BOOTSTRAP_PEERS=quic://seed-hostname:4433
MACULA_REALM=my-app
# TLS certs auto-generated if not provided
```

### Docker Port Mapping (v0.20.6+)

When the container's QUIC listen port differs from the host-exposed port, set `MACULA_ADVERTISE_PORT` so the gateway advertises the correct externally-reachable port in the DHT:

```bash
MACULA_QUIC_PORT=4433
MACULA_ADVERTISE_PORT=443
MACULA_HOSTNAME=boot.example.com
```

```yaml
services:
  boot:
    environment:
      MACULA_QUIC_PORT: "4433"
      MACULA_ADVERTISE_PORT: "443"
      MACULA_HOSTNAME: boot.example.com
    ports:
      - "443:4433/udp"
```

---

## Docker Setup

### Dockerfile

```dockerfile
RUN mkdir -p /opt/macula/certs

# CRITICAL: Set ownership to application user
COPY --from=builder --chown=app:app /app/priv/certs/server-cert.pem /opt/macula/certs/cert.pem
COPY --from=builder --chown=app:app /app/priv/certs/server-key.pem /opt/macula/certs/key.pem
RUN chmod 644 /opt/macula/certs/cert.pem && chmod 600 /opt/macula/certs/key.pem
```

### Docker Compose (Multi-Node)

```yaml
services:
  seed:
    image: my-app:latest
    environment:
      - MACULA_QUIC_PORT=4433
      - MACULA_REALM=my-app
    ports:
      - "4433:4433/udp"
      - "4000:4000"
    networks:
      - app-mesh

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

networks:
  app-mesh:
    driver: bridge
```

### Elixir/Phoenix Integration

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
    realm: realm
end
```

The Macula gateway reads `TLS_CERT_FILE` and `TLS_KEY_FILE` via `os:getenv/1`. Elixir `config :macula, certfile: ...` does NOT set OS environment variables.

### Kubernetes

```yaml
apiVersion: v1
kind: Secret
metadata:
  name: macula-tls
type: kubernetes.io/tls
data:
  tls.crt: <base64-encoded-cert>
  tls.key: <base64-encoded-key>
  ca.crt: <base64-encoded-ca>
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: macula-node
spec:
  template:
    spec:
      containers:
      - name: macula
        env:
        - name: MACULA_TLS_MODE
          value: "production"
        - name: MACULA_TLS_CERTFILE
          value: "/etc/macula/tls/tls.crt"
        - name: MACULA_TLS_KEYFILE
          value: "/etc/macula/tls/tls.key"
        - name: MACULA_TLS_CACERTFILE
          value: "/etc/macula/tls/ca.crt"
        volumeMounts:
        - name: tls-certs
          mountPath: /etc/macula/tls
          readOnly: true
      volumes:
      - name: tls-certs
        secret:
          secretName: macula-tls
```

---

## mTLS (Mutual TLS)

For additional security, configure mutual TLS where both client and server present certificates:

```erlang
{macula, [
    {tls_mode, production},
    {tls_cacertfile, "/etc/ssl/certs/ca-certificates.crt"},
    {tls_certfile, "/etc/macula/node.crt"},
    {tls_keyfile, "/etc/macula/node.key"},
    {tls_verify_hostname, true}
]}
```

With mTLS, all nodes in the mesh authenticate each other, preventing unauthorized nodes from joining.

---

## API Reference

The `macula_tls` module provides these functions:

| Function | Description |
|----------|-------------|
| `get_tls_mode/0` | Returns current TLS mode (`production` or `development`) |
| `is_production_mode/0` | Returns `true` if in production mode |
| `quic_client_opts/0` | Returns QUIC client TLS options |
| `quic_client_opts/1` | Returns QUIC client TLS options with overrides |
| `quic_client_opts_with_hostname/1` | Returns QUIC client TLS options with hostname verification |
| `quic_server_opts/0` | Returns QUIC server TLS options |
| `quic_server_opts/1` | Returns QUIC server TLS options with overrides |
| `hostname_verify_fun/3` | Hostname verification callback for OTP ssl |

---

## Troubleshooting

### `QUIC listen failed: config_error tls_error`

**Most common causes:**

1. **Certificate file ownership (Docker)**: Files copied without `--chown` are owned by `root`. The app user cannot read them.
   ```bash
   docker run --rm your-image ls -la /opt/macula/certs/
   # Files should be owned by 'app:app', NOT 'root:root'
   ```

2. **Missing OS environment variables (Elixir/Phoenix)**: Elixir `config :macula` does not set OS environment variables. Add `System.put_env/2` calls for `TLS_CERT_FILE` and `TLS_KEY_FILE`.

3. **ECDSA certificate**: MsQuic does not support ECDSA. Re-issue with RSA.
   ```bash
   openssl x509 -in cert.pem -noout -text | grep "Public Key Algorithm"
   # Must show: rsaEncryption
   ```

4. **Missing SAN extension**: QUIC requires Subject Alternative Name.

5. **Certificate/key mismatch**.

### `certificate verify failed`

- Ensure CA bundle includes the CA that signed the peer's certificate
- Check certificate chain is complete
- Verify `tls_cacertfile` path is correct

### `hostname verification failed`

- Certificate CN or SAN must match the hostname used to connect
- Set `tls_verify_hostname` to `false` temporarily to diagnose
- Regenerate certificate with correct SAN entries

### Debug Logging

```erlang
logger:set_primary_config(level, debug).
```

Look for log messages with `[TLS]` prefix.

### Verification Commands

```bash
# Check certificate details
openssl x509 -in /path/to/cert.pem -noout -text

# Verify certificate chain
openssl verify -CAfile /path/to/ca-bundle.crt /path/to/cert.pem

# Check certificate matches key
openssl x509 -noout -modulus -in cert.pem | openssl md5
openssl rsa -noout -modulus -in key.pem | openssl md5
# Both should produce the same MD5 hash

# Verify SAN extension
openssl x509 -in cert.pem -text -noout | grep -A2 "Subject Alternative Name"

# Check file permissions
ls -la /opt/macula/certs/
# cert.pem: 644, key.pem: 600
```

---

## Security Best Practices

1. **Always enable hostname verification** in production
2. **Use RSA keys**: RSA 2048+ (ECDSA not supported by MsQuic/QUIC)
3. **Rotate certificates regularly**: Annual rotation recommended
4. **Protect private keys**: Use file permissions 600
5. **Monitor certificate expiry**: Set up alerts before expiration
6. **Use short-lived certificates** when possible (e.g., Let's Encrypt)
7. **Never use development mode in production** - provides no MITM protection

---

## References

- QUIC Protocol: [RFC 9000](https://www.rfc-editor.org/rfc/rfc9000)
- TLS 1.3: [RFC 8446](https://www.rfc-editor.org/rfc/rfc8446)
- Subject Alternative Names: [RFC 5280](https://www.rfc-editor.org/rfc/rfc5280)
- quicer (Erlang QUIC): https://github.com/qzhuyan/quicer
