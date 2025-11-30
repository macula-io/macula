#!/bin/bash
# Chatter Demo Runner
# Starts multiple peer nodes behind different NAT types

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

COMPOSE_FILE="docker-compose.chatter.yml"

echo "======================================"
echo "  Macula Chatter Demo"
echo "  7 peers across 3 NAT types"
echo "======================================"
echo ""

# Clean up any existing containers
echo "[1/4] Cleaning up existing containers..."
docker compose -f "$COMPOSE_FILE" down --remove-orphans 2>/dev/null || true

# Build the image
echo "[2/4] Building Macula NAT test image..."
docker compose -f "$COMPOSE_FILE" build bootstrap

# Start routers first
echo "[3/4] Starting NAT routers..."
docker compose -f "$COMPOSE_FILE" up -d \
    nat_full_cone_router \
    nat_restricted_router \
    nat_symmetric_router

sleep 3

# Start bootstrap
echo "[4/4] Starting bootstrap and chatters..."
docker compose -f "$COMPOSE_FILE" up -d bootstrap
sleep 5

# Start all chatters
docker compose -f "$COMPOSE_FILE" up -d \
    chatter_alice \
    chatter_bob \
    chatter_charlie \
    chatter_diana \
    chatter_eve \
    chatter_frank \
    chatter_grace

echo ""
echo "======================================"
echo "  All containers started!"
echo "======================================"
echo ""
echo "Chatters:"
echo "  Full Cone NAT:    alice, bob, charlie (172.31.1.x)"
echo "  Restricted NAT:   diana, eve (172.32.1.x)"
echo "  Symmetric NAT:    frank, grace (172.33.1.x)"
echo ""
echo "Commands:"
echo "  Watch all logs:   docker compose -f $COMPOSE_FILE logs -f"
echo "  Watch one peer:   docker logs -f chatter-alice"
echo "  Check stats:      docker exec chatter-alice sh -c 'cd /app && erl ...'"
echo "  Stop all:         docker compose -f $COMPOSE_FILE down"
echo ""
echo "Waiting for chatters to connect and start messaging..."
sleep 10

echo ""
echo "======================================"
echo "  Recent messages from all chatters:"
echo "======================================"
for container in chatter-alice chatter-bob chatter-charlie chatter-diana chatter-eve chatter-frank chatter-grace; do
    echo ""
    echo "=== $container ==="
    docker logs "$container" 2>&1 | grep -E "(Chatter|Broadcast|Received)" | tail -5
done

echo ""
echo "======================================"
echo "  Demo running! Chatters broadcast every 10 seconds."
echo "  Use 'docker compose -f $COMPOSE_FILE logs -f' to watch live."
echo "======================================"
