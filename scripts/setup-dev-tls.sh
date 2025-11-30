#!/usr/bin/env bash
#
# Setup TLS certificates for Macula development mode.
#
# This script creates self-signed certificates at the default paths
# expected by macula_tls.erl in development mode:
#   - Certificate: /var/lib/macula/cert.pem
#   - Private Key: /var/lib/macula/key.pem
#
# The macula_tls module will automatically generate these certs at runtime
# if they don't exist, but this script allows pre-generation with custom
# options (e.g., longer validity, custom hostnames).
#
# Usage:
#   sudo ./scripts/setup-dev-tls.sh [options]
#
# Options:
#   --output-dir DIR   Output directory (default: /var/lib/macula)
#   --validity DAYS    Certificate validity in days (default: 3650)
#   --hostname NAME    Primary hostname for certificate (default: macula-node)
#   --help             Show this help message
#
# Environment Variables:
#   MACULA_TLS_CERT_DIR   Override output directory
#   MACULA_TLS_VALIDITY   Override validity period
#   MACULA_TLS_HOSTNAME   Override hostname
#

set -e

# Default values
DEFAULT_OUTPUT_DIR="/var/lib/macula"
DEFAULT_VALIDITY="3650"  # 10 years
DEFAULT_HOSTNAME="macula-node"
KEY_BITS="2048"

# Parse environment variables
OUTPUT_DIR="${MACULA_TLS_CERT_DIR:-${DEFAULT_OUTPUT_DIR}}"
VALIDITY="${MACULA_TLS_VALIDITY:-${DEFAULT_VALIDITY}}"
HOSTNAME="${MACULA_TLS_HOSTNAME:-${DEFAULT_HOSTNAME}}"

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --output-dir)
            OUTPUT_DIR="$2"
            shift 2
            ;;
        --validity)
            VALIDITY="$2"
            shift 2
            ;;
        --hostname)
            HOSTNAME="$2"
            shift 2
            ;;
        --help)
            head -35 "$0" | tail -30
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

CERT_PATH="${OUTPUT_DIR}/cert.pem"
KEY_PATH="${OUTPUT_DIR}/key.pem"

echo "==> Macula TLS Development Certificate Setup"
echo "    Output Directory: ${OUTPUT_DIR}"
echo "    Certificate: ${CERT_PATH}"
echo "    Private Key: ${KEY_PATH}"
echo "    Validity: ${VALIDITY} days"
echo "    Hostname: ${HOSTNAME}"
echo ""

# Check if certs already exist
if [[ -f "${CERT_PATH}" ]] && [[ -f "${KEY_PATH}" ]]; then
    echo "Warning: Certificate files already exist!"
    read -p "Overwrite? [y/N] " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Aborted."
        exit 0
    fi
fi

# Create output directory with proper permissions
echo "==> Creating directory: ${OUTPUT_DIR}"
mkdir -p "${OUTPUT_DIR}"
chmod 755 "${OUTPUT_DIR}"

# Generate RSA private key
echo "==> Generating RSA ${KEY_BITS}-bit private key..."
openssl genrsa -out "${KEY_PATH}" "${KEY_BITS}" 2>/dev/null

# Set restrictive permissions on private key
chmod 600 "${KEY_PATH}"

# Create temporary config file for certificate extensions
TEMP_CONF=$(mktemp)
cat > "${TEMP_CONF}" <<EOF
[req]
distinguished_name = req_distinguished_name
req_extensions = v3_req
x509_extensions = v3_req
prompt = no

[req_distinguished_name]
CN = ${HOSTNAME}

[v3_req]
basicConstraints = CA:FALSE
keyUsage = critical, digitalSignature, keyEncipherment
extendedKeyUsage = serverAuth, clientAuth
subjectAltName = @alt_names

[alt_names]
DNS.1 = ${HOSTNAME}
DNS.2 = localhost
DNS.3 = *.macula.local
DNS.4 = *.macula.test
IP.1 = 127.0.0.1
IP.2 = ::1
EOF

# Generate self-signed certificate
echo "==> Generating self-signed X509 certificate..."
openssl req -new -x509 \
    -key "${KEY_PATH}" \
    -out "${CERT_PATH}" \
    -days "${VALIDITY}" \
    -config "${TEMP_CONF}" \
    -sha256

# Set permissions on certificate
chmod 644 "${CERT_PATH}"

# Cleanup temp file
rm -f "${TEMP_CONF}"

# Verify certificate
echo ""
echo "==> Certificate Details:"
openssl x509 -in "${CERT_PATH}" -noout -subject -dates 2>/dev/null

# Verify key matches cert
echo ""
echo "==> Verifying key matches certificate..."
CERT_MODULUS=$(openssl x509 -in "${CERT_PATH}" -noout -modulus 2>/dev/null | openssl md5)
KEY_MODULUS=$(openssl rsa -in "${KEY_PATH}" -noout -modulus 2>/dev/null | openssl md5)

if [ "${CERT_MODULUS}" = "${KEY_MODULUS}" ]; then
    echo "    Key and certificate match."
else
    echo "ERROR: Certificate and key do NOT match!"
    exit 1
fi

echo ""
echo "==> Development TLS setup complete!"
echo ""
echo "Files created:"
echo "    ${CERT_PATH}"
echo "    ${KEY_PATH}"
echo ""
echo "To use with Macula:"
echo "    1. Start Macula in development mode (default)"
echo "    2. Or set: MACULA_TLS_MODE=development"
echo ""
echo "For production, see: docs/operator/TLS_CONFIGURATION.md"
