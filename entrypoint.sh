#!/bin/sh
#
# Macula Gateway Entrypoint Script
# Provides environment configuration and controlled startup
#

set -e

# Default values
GATEWAY_PORT="${GATEWAY_PORT:-9443}"
MACULA_REALM="${MACULA_REALM:-com.example.realm}"
RELEASE_COOKIE="${RELEASE_COOKIE:-macula-gateway-cookie}"
NODE_NAME="${NODE_NAME:-macula@127.0.0.1}"

# Export environment variables for the release
export GATEWAY_PORT
export MACULA_REALM
export RELEASE_COOKIE
export RELEASE_NODE="${NODE_NAME}"

# Log startup information
echo "=================================================="
echo "Starting Macula Gateway"
echo "=================================================="
echo "Node Name:    ${NODE_NAME}"
echo "Gateway Port: ${GATEWAY_PORT}"
echo "Realm:        ${MACULA_REALM}"
echo "Release Path: /opt/macula"
echo "=================================================="

# Check if the release exists
if [ ! -f "/opt/macula/bin/macula" ]; then
    echo "ERROR: Macula release not found at /opt/macula/bin/macula"
    exit 1
fi

# Display Erlang/OTP version
echo "Erlang/OTP version:"
erl -noshell -eval 'erlang:display(erlang:system_info(otp_release)), halt().' || echo "Unable to determine OTP version"

echo "=================================================="

# Start the release in foreground mode
exec /opt/macula/bin/macula foreground
