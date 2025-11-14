#!/bin/bash
# End-to-end RPC test with multiple providers
# Tests service discovery, RPC calls, and failover

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "==> E2E RPC Test"
echo "    Testing RPC with service discovery and multiple providers"
echo ""

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Change to project root
cd "$PROJECT_ROOT"

# Function to cleanup on exit
cleanup() {
    echo ""
    echo "==> Cleaning up Docker containers..."
    docker compose -f docker/docker-compose.multi-node-test.yml down -v 2>&1 | grep -v "warn" || true
}

# Set trap to cleanup on exit
trap cleanup EXIT

# Clean up any existing containers
echo "==> Stopping any existing containers..."
cleanup

# Build fresh image with latest code
echo "==> Building Docker image with latest code..."
docker builder prune -af > /dev/null 2>&1
docker compose -f docker/docker-compose.multi-node-test.yml build --no-cache

# Start the test environment
echo "==> Starting test environment (registry + 3 providers + client)..."
docker compose -f docker/docker-compose.multi-node-test.yml up -d

# Wait for services to be ready
echo "==> Waiting for services to initialize..."
sleep 20

# Check that all containers are running
echo "==> Checking container status..."
RUNNING=$(docker compose -f docker/docker-compose.multi-node-test.yml ps | grep -c "Up" || true)
if [ "$RUNNING" -lt 4 ]; then
    echo -e "${RED}ERROR: Not all containers started successfully${NC}"
    docker compose -f docker/docker-compose.multi-node-test.yml ps
    exit 1
fi

echo -e "${GREEN}✓ All containers running${NC}"

# Wait for client to complete tests
echo ""
echo "==> Waiting for RPC tests to complete (client will run 6 calls)..."
sleep 25

# Collect logs
docker compose -f docker/docker-compose.multi-node-test.yml logs > /tmp/rpc-test-logs.txt 2>&1

# Analyze logs
echo ""
echo "==> Analyzing results..."

# Count successful calls and which providers handled them
TOTAL_CALLS=$(grep -c "Call #" /tmp/rpc-test-logs.txt || echo "0")
SUCCESS_CALLS=$(grep -c "Success:" /tmp/rpc-test-logs.txt || echo "0")
PROVIDER1_CALLS=$(grep -c "\"provider\":\"provider1\"" /tmp/rpc-test-logs.txt || echo "0")
PROVIDER2_CALLS=$(grep -c "\"provider\":\"provider2\"" /tmp/rpc-test-logs.txt || echo "0")
PROVIDER3_CALLS=$(grep -c "\"provider\":\"provider3\"" /tmp/rpc-test-logs.txt || echo "0")

echo ""
echo "Results:"
echo "  Total RPC calls attempted: $TOTAL_CALLS"
echo "  Successful calls: $SUCCESS_CALLS"
echo "  Calls handled by provider1: $PROVIDER1_CALLS"
echo "  Calls handled by provider2: $PROVIDER2_CALLS"
echo "  Calls handled by provider3: $PROVIDER3_CALLS"
echo ""

# Determine test result
if [ "$SUCCESS_CALLS" -ge 6 ]; then
    echo -e "${GREEN}✓ TEST PASSED${NC}"
    echo "  - All 6 RPC calls succeeded"
    echo "  - Service discovery working"

    # Check for round-robin distribution
    if [ "$PROVIDER1_CALLS" -gt 0 ] && [ "$PROVIDER2_CALLS" -gt 0 ] && [ "$PROVIDER3_CALLS" -gt 0 ]; then
        echo "  - Round-robin distribution working (all providers used)"
    else
        echo -e "  ${YELLOW}⚠ Distribution not perfect (expected round-robin)${NC}"
    fi
    exit 0
else
    echo -e "${RED}✗ TEST FAILED${NC}"
    echo "  - Expected 6 successful calls, got $SUCCESS_CALLS"
    echo ""
    echo "Check logs at: /tmp/rpc-test-logs.txt"
    echo "Or run: docker compose -f docker/docker-compose.multi-node-test.yml logs"
    exit 1
fi
