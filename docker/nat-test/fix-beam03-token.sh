#!/bin/bash
# Fix beam03 k3s empty token issue
# Run with: sshpass -p 'rl' ssh rl@192.168.1.13 'bash -s' < fix-beam03-token.sh

set -e

echo "=== Fixing beam03 k3s token ==="

# Stop k3s first
echo "Stopping k3s..."
sudo systemctl stop k3s

# Generate a new random token (simple password format)
NEW_TOKEN=$(openssl rand -hex 32)
echo "Generated new token: ${NEW_TOKEN:0:8}..."

# Write to token file
echo "Writing token to /fast/k3s-data/server/token..."
echo "$NEW_TOKEN" | sudo tee /fast/k3s-data/server/token > /dev/null
sudo chmod 600 /fast/k3s-data/server/token

# Verify
echo "Token file size: $(sudo wc -c < /fast/k3s-data/server/token) bytes"

# Start k3s
echo "Starting k3s..."
sudo systemctl start k3s

# Wait for startup
echo "Waiting for k3s to start (30 seconds)..."
sleep 30

# Check status
echo "=== k3s status ==="
sudo systemctl is-active k3s || true
kubectl get nodes 2>&1 || echo "Still starting..."

echo "=== Done ==="
