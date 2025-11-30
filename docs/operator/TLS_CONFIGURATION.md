# TLS Configuration Guide

Macula v0.11.0+ includes comprehensive TLS support for QUIC connections. This guide covers configuration for both development and production environments.

## Quick Start

### Development Mode (Default)

Development mode uses self-signed certificates with no verification. This is the default behavior:

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

Macula will automatically generate self-signed certificates on first startup if they don't exist.

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

## Configuration Options

### TLS Mode

| Option | Description |
|--------|-------------|
| `development` | Self-signed certificates, no verification (default) |
| `production` | Strict certificate verification, requires CA bundle |

**Environment Variable**: `MACULA_TLS_MODE`
**Shortcuts**: `dev`, `prod`

### CA Certificate Bundle

Path to the CA certificate bundle for verifying peer certificates in production mode.

**Application Config**: `{tls_cacertfile, "/path/to/ca-bundle.crt"}`
**Environment Variable**: `MACULA_TLS_CACERTFILE`

If not specified, Macula checks these system default locations:
- `/etc/ssl/certs/ca-certificates.crt` (Debian/Ubuntu)
- `/etc/pki/tls/certs/ca-bundle.crt` (RHEL/CentOS)
- `/etc/ssl/cert.pem` (Alpine/macOS)

### Server/Client Certificate

Path to PEM-encoded certificate for this node. Required for servers, optional for clients (mTLS).

**Application Config**: `{tls_certfile, "/path/to/cert.pem"}`
**Environment Variable**: `MACULA_TLS_CERTFILE`

### Private Key

Path to PEM-encoded private key. Must match the certificate.

**Application Config**: `{tls_keyfile, "/path/to/key.pem"}`
**Environment Variable**: `MACULA_TLS_KEYFILE`

### Hostname Verification

Verify that the server's certificate CN or SAN matches the hostname.

**Application Config**: `{tls_verify_hostname, true}`
**Environment Variable**: `MACULA_TLS_VERIFY_HOSTNAME`
**Default**: `true` (in production mode)

## Development Certificate Setup

For development, you can pre-generate certificates using the provided script:

```bash
sudo ./scripts/setup-dev-tls.sh
```

This creates:
- `/var/lib/macula/cert.pem` - Self-signed certificate
- `/var/lib/macula/key.pem` - Private key

### Script Options

```bash
./scripts/setup-dev-tls.sh [options]

Options:
  --output-dir DIR   Output directory (default: /var/lib/macula)
  --validity DAYS    Certificate validity in days (default: 3650)
  --hostname NAME    Primary hostname for certificate (default: macula-node)
  --help             Show help message
```

### Environment Variables for Script

```bash
MACULA_TLS_CERT_DIR=/custom/path  # Override output directory
MACULA_TLS_VALIDITY=365           # Override validity period
MACULA_TLS_HOSTNAME=mynode        # Override hostname
```

## Production Deployment

### Obtaining Certificates

For production, obtain certificates from a trusted CA:

1. **Let's Encrypt** (recommended for public-facing nodes)
   ```bash
   certbot certonly --standalone -d your-node.example.com
   ```

2. **Internal CA** (for private mesh networks)
   Generate certificates with your organization's CA

3. **Self-signed with custom CA** (for testing production config)
   Create your own CA and sign node certificates

### Certificate Requirements

- **Format**: PEM-encoded
- **Key Type**: RSA 2048+ or ECDSA P-256+
- **Extensions**:
  - Key Usage: digitalSignature, keyEncipherment
  - Extended Key Usage: serverAuth, clientAuth (for mTLS)
  - Subject Alternative Names: Include all node hostnames/IPs

### Kubernetes Deployment

For Kubernetes, use Secrets to mount certificates:

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

### Certificate Rotation

To rotate certificates without downtime:

1. Generate new certificate with same or new key
2. Update the certificate files
3. Restart nodes one at a time (rolling restart)

For automated rotation, consider using cert-manager in Kubernetes.

## Troubleshooting

### Common Errors

**"certificate verify failed"**
- Ensure CA bundle includes the CA that signed the peer's certificate
- Check certificate chain is complete
- Verify `tls_cacertfile` path is correct

**"hostname verification failed"**
- Certificate CN or SAN must match the hostname used to connect
- Set `tls_verify_hostname` to `false` temporarily to diagnose
- Regenerate certificate with correct SAN entries

**"unable to get local issuer certificate"**
- CA bundle is missing or incomplete
- Check `tls_cacertfile` path
- Verify CA bundle contains the root CA

### Debug Logging

Enable TLS debug logging:

```erlang
%% In shell
logger:set_primary_config(level, debug).
```

Look for log messages with `[TLS]` prefix.

### Verification Commands

Check certificate details:
```bash
openssl x509 -in /path/to/cert.pem -noout -text
```

Verify certificate chain:
```bash
openssl verify -CAfile /path/to/ca-bundle.crt /path/to/cert.pem
```

Check certificate matches key:
```bash
openssl x509 -noout -modulus -in cert.pem | openssl md5
openssl rsa -noout -modulus -in key.pem | openssl md5
# Both should produce the same MD5 hash
```

## Security Considerations

### Development Mode Warnings

Development mode logs a warning on startup:
```
TLS running in DEVELOPMENT mode - certificate verification DISABLED
```

Never use development mode in production. It provides no protection against MITM attacks.

### Production Best Practices

1. **Always enable hostname verification** in production
2. **Use strong key sizes**: RSA 2048+ or ECDSA P-256+
3. **Rotate certificates regularly**: Annual rotation recommended
4. **Protect private keys**: Use file permissions 600
5. **Monitor certificate expiry**: Set up alerts before expiration
6. **Use short-lived certificates** when possible (e.g., Let's Encrypt)

### mTLS (Mutual TLS)

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
