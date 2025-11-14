#!/bin/bash
# End-to-end pub/sub message flow test
# Tests that messages published on one node are received by subscribers on other nodes

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "==> E2E Pub/Sub Test"
echo "    Testing message flow across nodes"
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
    docker compose -f docker/docker-compose.pubsub-test.yml down -v 2>&1 | grep -v "warn" || true
}

# Set trap to cleanup on exit
trap cleanup EXIT

# Clean up any existing containers
echo "==> Stopping any existing containers..."
cleanup

# Build fresh image with latest code
echo "==> Building Docker image with latest code..."
docker builder prune -af > /dev/null 2>&1
docker compose -f docker/docker-compose.pubsub-test.yml build --no-cache

# Start the test environment
echo "==> Starting test environment (registry + publisher + 2 subscribers)..."
docker compose -f docker/docker-compose.pubsub-test.yml up -d

# Wait for services to be ready
echo "==> Waiting for services to initialize..."
sleep 15

# Check that all containers are running
echo "==> Checking container status..."
RUNNING=$(docker compose -f docker/docker-compose.pubsub-test.yml ps | grep -c "Up" || true)
if [ "$RUNNING" -lt 4 ]; then
    echo -e "${RED}ERROR: Not all containers started successfully${NC}"
    docker compose -f docker/docker-compose.pubsub-test.yml ps
    exit 1
fi

echo -e "${GREEN}✓ All containers running${NC}"

# Monitor logs for message flow
echo ""
echo "==> Monitoring pub/sub message flow (30 seconds)..."
echo "    Looking for messages published and received..."

# Collect logs
timeout 30 docker compose -f docker/docker-compose.pubsub-test.yml logs -f > /tmp/pubsub-test-logs.txt 2>&1 || true

# Analyze logs
echo ""
echo "==> Analyzing results..."

PUBLISHED=$(grep -c "Published successfully" /tmp/pubsub-test-logs.txt || echo "0")
RECEIVED_SUB1=$(grep -c "\[subscriber1\] RECEIVED MESSAGE" /tmp/pubsub-test-logs.txt || echo "0")
RECEIVED_SUB2=$(grep -c "\[subscriber2\] RECEIVED MESSAGE" /tmp/pubsub-test-logs.txt || echo "0")

echo ""
echo "Results:"
echo "  Messages published: $PUBLISHED"
echo "  Messages received by subscriber1: $RECEIVED_SUB1"
echo "  Messages received by subscriber2: $RECEIVED_SUB2"
echo ""

# Determine test result
if [ "$PUBLISHED" -gt 0 ] && [ "$RECEIVED_SUB1" -gt 0 ] && [ "$RECEIVED_SUB2" -gt 0 ]; then
    echo -e "${GREEN}✓ TEST PASSED${NC}"
    echo "  - Publisher successfully published $PUBLISHED messages"
    echo "  - Subscriber1 received $RECEIVED_SUB1 messages"
    echo "  - Subscriber2 received $RECEIVED_SUB2 messages"
    echo "  - Message flow working across nodes!"
    exit 0
else
    echo -e "${RED}✗ TEST FAILED${NC}"
    if [ "$PUBLISHED" -eq 0 ]; then
        echo "  - No messages were published"
    fi
    if [ "$RECEIVED_SUB1" -eq 0 ]; then
        echo "  - Subscriber1 did not receive any messages"
    fi
    if [ "$RECEIVED_SUB2" -eq 0 ]; then
        echo "  - Subscriber2 did not receive any messages"
    fi
    echo ""
    echo "Check logs at: /tmp/pubsub-test-logs.txt"
    echo "Or run: docker compose -f docker/docker-compose.pubsub-test.yml logs"
    exit 1
fi
