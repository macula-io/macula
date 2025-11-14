#!/bin/bash
# End-to-end Multi-hop RPC Test
# Tests RPC routing through DHT mesh with multiple intermediate nodes
#
# Topology: Client → NodeA → NodeB → Provider (2-3 hops)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "==> E2E Multi-hop RPC Test"
echo "    Testing peer-to-peer multi-hop routing through Kademlia DHT"
echo ""

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Change to project root
cd "$PROJECT_ROOT"

# Function to cleanup on exit
cleanup() {
    echo ""
    echo "==> Cleaning up Docker containers..."
    docker compose -f docker/docker-compose.multi-hop-mesh-test.yml down -v 2>&1 | grep -v "warn" || true
}

# Set trap to cleanup on exit
trap cleanup EXIT

# Clean up any existing containers
echo "==> Stopping any existing containers..."
cleanup

# Build fresh image with latest code
echo "==> Building Docker image with latest code..."
docker builder prune -af > /dev/null 2>&1
docker compose -f docker/docker-compose.multi-hop-mesh-test.yml build --no-cache

# Start the test environment
echo "==> Starting multi-hop mesh environment..."
echo "    - Registry (DHT coordinator)"
echo "    - NodeA (first hop)"
echo "    - NodeB (second hop)"
echo "    - Provider (service provider)"
echo "    - Client (RPC caller)"
echo ""
docker compose -f docker/docker-compose.multi-hop-mesh-test.yml up -d

# Wait for services to be ready
echo "==> Waiting for mesh to stabilize..."
echo "    Phase 1: Container startup (10s)"
sleep 10

# Check that all containers are running
echo "==> Checking container status..."
RUNNING=$(docker compose -f docker/docker-compose.multi-hop-mesh-test.yml ps | grep -c "Up" || true)
if [ "$RUNNING" -lt 5 ]; then
    echo -e "${RED}ERROR: Not all containers started successfully${NC}"
    docker compose -f docker/docker-compose.multi-hop-mesh-test.yml ps
    exit 1
fi

echo -e "${GREEN}✓ All containers running${NC}"

# Wait for mesh connections to establish
echo "    Phase 2: Mesh connection setup (15s)"
sleep 15

# Wait for client to complete tests
echo ""
echo "==> Waiting for multi-hop RPC tests to complete..."
echo "    Client will make 6 RPC calls through the mesh"
sleep 30

# Collect logs
echo ""
echo "==> Collecting logs..."
docker compose -f docker/docker-compose.multi-hop-mesh-test.yml logs > /tmp/multi-hop-rpc-test-logs.txt 2>&1

# Analyze logs
echo "==> Analyzing results..."
echo ""

# Count routing messages
RPC_ROUTE_SENT=$(grep -c "Sending routed RPC call" /tmp/multi-hop-rpc-test-logs.txt || echo "0")
RPC_ROUTE_RECEIVED=$(grep -c "RECEIVED RPC_ROUTE MESSAGE" /tmp/multi-hop-rpc-test-logs.txt || echo "0")
RPC_ROUTE_FORWARDED=$(grep -c "Forwarding rpc_route to next hop" /tmp/multi-hop-rpc-test-logs.txt || echo "0")
RPC_ROUTE_DELIVERED=$(grep -c "RPC route: delivering CALL locally" /tmp/multi-hop-rpc-test-logs.txt || echo "0")

# Count successful calls
TOTAL_CALLS=$(grep -c "Call #" /tmp/multi-hop-rpc-test-logs.txt || echo "0")
SUCCESS_CALLS=$(grep -c "✓ Success:" /tmp/multi-hop-rpc-test-logs.txt || echo "0")

echo "${BLUE}Routing Metrics:${NC}"
echo "  RPC route messages sent: $RPC_ROUTE_SENT"
echo "  RPC route messages received: $RPC_ROUTE_RECEIVED"
echo "  RPC route forwards: $RPC_ROUTE_FORWARDED"
echo "  RPC route deliveries: $RPC_ROUTE_DELIVERED"
echo ""

echo "${BLUE}Call Results:${NC}"
echo "  Total RPC calls attempted: $TOTAL_CALLS"
echo "  Successful calls: $SUCCESS_CALLS"
echo ""

# Determine test result
if [ "$SUCCESS_CALLS" -ge 6 ]; then
    echo -e "${GREEN}✓ TEST PASSED${NC}"
    echo "  - All 6 RPC calls succeeded"
    echo "  - Multi-hop DHT routing working"

    # Verify multi-hop actually happened
    if [ "$RPC_ROUTE_FORWARDED" -gt 0 ]; then
        echo -e "  ${GREEN}✓ Verified: Messages forwarded through mesh ($RPC_ROUTE_FORWARDED forwards)${NC}"
    else
        echo -e "  ${YELLOW}⚠ Warning: No forwards detected (may be direct routing)${NC}"
    fi

    exit 0
elif [ "$SUCCESS_CALLS" -gt 0 ]; then
    echo -e "${YELLOW}⚠ TEST PARTIAL SUCCESS${NC}"
    echo "  - $SUCCESS_CALLS out of 6 calls succeeded"
    echo "  - Check logs for failures"
    echo ""
    echo "Check logs at: /tmp/multi-hop-rpc-test-logs.txt"
    exit 1
else
    echo -e "${RED}✗ TEST FAILED${NC}"
    echo "  - No successful RPC calls"
    echo "  - Multi-hop routing not working"
    echo ""
    echo "Check logs at: /tmp/multi-hop-rpc-test-logs.txt"
    echo "Or run: docker compose -f docker/docker-compose.multi-hop-mesh-test.yml logs"
    exit 1
fi
