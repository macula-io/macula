#!/bin/sh
# Generate self-signed TLS certificates for Macula testing

CERT_DIR="/opt/macula/certs"

# Create certificate directory
mkdir -p "$CERT_DIR"

# Generate self-signed certificate valid for 365 days
openssl req -x509 -newkey rsa:4096 -keyout "$CERT_DIR/key.pem" -out "$CERT_DIR/cert.pem" \
    -days 365 -nodes \
    -subj "/C=US/ST=Test/L=Test/O=Macula/OU=Test/CN=*.macula.test" \
    -addext "subjectAltName=DNS:*.macula.test,DNS:registry.macula.test,DNS:provider1.macula.test,DNS:provider2.macula.test,DNS:provider3.macula.test,DNS:client.macula.test"

echo "Self-signed certificates generated at $CERT_DIR"
ls -la "$CERT_DIR"
