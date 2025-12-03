#!/bin/bash
# Reset beam03 k3s completely (WARNING: destroys all k3s data)
# Run with: sshpass -p 'rl' ssh rl@192.168.1.13 'sudo bash -s' < reset-beam03-k3s.sh

set -e

echo "=== Resetting beam03 k3s (FULL RESET) ==="

# Stop k3s
echo "Stopping k3s..."
systemctl stop k3s || true

# Kill any remaining processes
echo "Killing remaining k3s processes..."
pkill -9 -f k3s || true
pkill -9 containerd-shim || true

# Uninstall k3s to clean up everything
echo "Running k3s uninstall..."
/usr/local/bin/k3s-uninstall.sh || true

# Clean up data directory
echo "Cleaning /fast/k3s-data..."
rm -rf /fast/k3s-data/*

# Reinstall k3s with same configuration
echo "Reinstalling k3s..."
curl -sfL https://get.k3s.io | INSTALL_K3S_VERSION="v1.33.5+k3s1" sh -s - server \
    --data-dir=/fast/k3s-data \
    --disable=traefik \
    --write-kubeconfig-mode=644

# Wait for startup
echo "Waiting for k3s to start (60 seconds)..."
sleep 60

# Check status
echo "=== k3s status ==="
systemctl is-active k3s || echo "Not active yet"
kubectl get nodes 2>&1 || echo "Still starting..."

# Update kubeconfig permissions
echo "Setting kubeconfig permissions..."
chmod 644 /etc/rancher/k3s/k3s.yaml || true

echo "=== Reset complete ==="
echo "Don't forget to update ~/.kube/beam-clusters/beam03.yaml on the workstation!"
