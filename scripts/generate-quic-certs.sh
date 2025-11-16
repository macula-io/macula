#!/usr/bin/env bash

# Generate TLS certificates optimized for QUIC/MsQuic
# Based on research into quicer and MsQuic requirements
#
# Key requirements:
# - RSA 4096-bit (or Ed25519 for future)
# - X509v3 extensions: KeyUsage, ExtendedKeyUsage, SubjectAltName
# - Separate cert and key files
# - Self-signed but properly formatted

set -e

OUTPUT_DIR="${1:-./ certs}"

echo "==> Generating QUIC TLS certificates in: ${OUTPUT_DIR}"

# Create output directory
mkdir -p "${OUTPUT_DIR}"

# Certificate details
SUBJECT="/C=US/ST=State/L=City/O=Macula/OU=Test/CN=*.macula.test"
DAYS=365

# Generate RSA private key (4096-bit for security)
echo "==> Generating RSA 4096-bit private key..."
openssl genrsa -out "${OUTPUT_DIR}/key.pem" 4096 2>/dev/null

# Create OpenSSL config for certificate extensions
cat > "${OUTPUT_DIR}/cert.conf" <<EOF
[req]
distinguished_name = req_distinguished_name
req_extensions = v3_req
x509_extensions = v3_req
prompt = no

[req_distinguished_name]
C = US
ST = State
L = City
O = Macula
OU = Test
CN = *.macula.test

[v3_req]
basicConstraints = CA:TRUE
keyUsage = critical, digitalSignature, keyEncipherment, keyCertSign
extendedKeyUsage = serverAuth, clientAuth
subjectAltName = @alt_names

[alt_names]
DNS.1 = *.macula.test
DNS.2 = *.arcade.local
DNS.3 = arcade-gateway
DNS.4 = arcade-peer1
DNS.5 = arcade-peer2
DNS.6 = arcade-peer3
DNS.7 = registry.macula.test
DNS.8 = localhost
EOF

# Generate self-signed certificate with proper X509v3 extensions
echo "==> Generating X509 certificate with required extensions..."
openssl req -new -x509 \
    -key "${OUTPUT_DIR}/key.pem" \
    -out "${OUTPUT_DIR}/cert.pem" \
    -days "${DAYS}" \
    -config "${OUTPUT_DIR}/cert.conf" \
    -sha256

# Verify certificate
echo ""
echo "==> Certificate generated successfully!"
echo "    Certificate: ${OUTPUT_DIR}/cert.pem"
echo "    Private Key: ${OUTPUT_DIR}/key.pem"
echo ""

# Display certificate details
echo "==> Certificate Details:"
openssl x509 -in "${OUTPUT_DIR}/cert.pem" -noout -subject -dates -ext subjectAltName,keyUsage,extendedKeyUsage

# Verify key matches cert
echo ""
echo "==> Verifying key matches certificate..."
CERT_MODULUS=$(openssl x509 -in "${OUTPUT_DIR}/cert.pem" -noout -modulus | openssl md5)
KEY_MODULUS=$(openssl rsa -in "${OUTPUT_DIR}/key.pem" -noout -modulus 2>/dev/null | openssl md5)

if [ "$CERT_MODULUS" = "$KEY_MODULUS" ]; then
    echo "✅ Certificate and key match!"
else
    echo "❌ ERROR: Certificate and key do NOT match!"
    exit 1
fi

# Clean up config file
rm "${OUTPUT_DIR}/cert.conf"

echo ""
echo "==> ✅ Certificate generation complete!"
echo ""
echo "To use with QUIC/quicer:"
echo "  ListenerOpts = ["
echo "    {certfile, \"${OUTPUT_DIR}/cert.pem\"},"
echo "    {keyfile, \"${OUTPUT_DIR}/key.pem\"},"
echo "    {alpn, [\"h3\"]},"
echo "    ..."
echo "  ]"
