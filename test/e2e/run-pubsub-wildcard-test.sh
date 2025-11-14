#!/bin/bash
# End-to-end wildcard pub/sub matching test
# Tests single-level (*) and multi-level (**) wildcard topic matching

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "==> E2E Wildcard Pub/Sub Test"
echo "    Testing wildcard topic matching across subscribers"
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
    docker compose -f docker/docker-compose.pubsub-wildcard-test.yml down -v 2>&1 | grep -v "warn" || true
}

# Set trap to cleanup on exit
trap cleanup EXIT

# Clean up any existing containers
echo "==> Stopping any existing containers..."
cleanup

# Build fresh image with latest code
echo "==> Building Docker image with latest code..."
docker builder prune -af > /dev/null 2>&1
docker compose -f docker/docker-compose.pubsub-wildcard-test.yml build --no-cache

# Start the test environment
echo "==> Starting test environment (registry + publisher + 3 subscribers with different patterns)..."
docker compose -f docker/docker-compose.pubsub-wildcard-test.yml up -d

# Wait for services to be ready
echo "==> Waiting for services to initialize..."
sleep 15

# Check that all containers are running
echo "==> Checking container status..."
RUNNING=$(docker compose -f docker/docker-compose.pubsub-wildcard-test.yml ps | grep -c "Up" || true)
if [ "$RUNNING" -lt 5 ]; then
    echo -e "${RED}ERROR: Not all containers started successfully${NC}"
    docker compose -f docker/docker-compose.pubsub-wildcard-test.yml ps
    exit 1
fi

echo -e "${GREEN}✓ All containers running${NC}"

# Monitor logs for message flow
echo ""
echo "==> Monitoring wildcard pub/sub message flow (30 seconds)..."
echo "    Looking for pattern-matched messages..."

# Collect logs
timeout 30 docker compose -f docker/docker-compose.pubsub-wildcard-test.yml logs -f > /tmp/pubsub-wildcard-test-logs.txt 2>&1 || true

# Analyze logs
echo ""
echo "==> Analyzing results..."

# Count published tests
PUBLISHED_TEST1=$(grep -c "TEST 1: Publish to sensor.temp.room1" /tmp/pubsub-wildcard-test-logs.txt || echo "0")
PUBLISHED_TEST2=$(grep -c "TEST 2: Publish to sensor.humidity.room1" /tmp/pubsub-wildcard-test-logs.txt || echo "0")
PUBLISHED_TEST3=$(grep -c "TEST 3: Publish to sensor.temp.room2" /tmp/pubsub-wildcard-test-logs.txt || echo "0")
PUBLISHED_TEST4=$(grep -c "TEST 4: Publish to other.topic" /tmp/pubsub-wildcard-test-logs.txt || echo "0")
PUBLISHED_TEST5=$(grep -c "TEST 5: Publish to sensor.temp.room1.extra" /tmp/pubsub-wildcard-test-logs.txt || echo "0")

# Count messages received by each subscriber (looking for test field in payload)
# subscriber1 (exact: sensor.temp.room1) should receive test1 only
SUB1_TEST1=$(grep "/ exact\].*RECEIVED.*test=test1" /tmp/pubsub-wildcard-test-logs.txt | wc -l)
SUB1_TEST2=$(grep "/ exact\].*RECEIVED.*test=test2" /tmp/pubsub-wildcard-test-logs.txt | wc -l)
SUB1_TEST3=$(grep "/ exact\].*RECEIVED.*test=test3" /tmp/pubsub-wildcard-test-logs.txt | wc -l)
SUB1_TEST5=$(grep "/ exact\].*RECEIVED.*test=test5" /tmp/pubsub-wildcard-test-logs.txt | wc -l)

# subscriber2 (single wildcard: sensor.*.room1) should receive test1 and test2
SUB2_TEST1=$(grep "/ single-wildcard\].*RECEIVED.*test=test1" /tmp/pubsub-wildcard-test-logs.txt | wc -l)
SUB2_TEST2=$(grep "/ single-wildcard\].*RECEIVED.*test=test2" /tmp/pubsub-wildcard-test-logs.txt | wc -l)
SUB2_TEST3=$(grep "/ single-wildcard\].*RECEIVED.*test=test3" /tmp/pubsub-wildcard-test-logs.txt | wc -l)
SUB2_TEST5=$(grep "/ single-wildcard\].*RECEIVED.*test=test5" /tmp/pubsub-wildcard-test-logs.txt | wc -l)

# subscriber3 (multi wildcard: sensor.**) should receive test1, test2, test3, test5
SUB3_TEST1=$(grep "/ multi-wildcard\].*RECEIVED.*test=test1" /tmp/pubsub-wildcard-test-logs.txt | wc -l)
SUB3_TEST2=$(grep "/ multi-wildcard\].*RECEIVED.*test=test2" /tmp/pubsub-wildcard-test-logs.txt | wc -l)
SUB3_TEST3=$(grep "/ multi-wildcard\].*RECEIVED.*test=test3" /tmp/pubsub-wildcard-test-logs.txt | wc -l)
SUB3_TEST5=$(grep "/ multi-wildcard\].*RECEIVED.*test=test5" /tmp/pubsub-wildcard-test-logs.txt | wc -l)

echo ""
echo "Published tests:"
echo "  Test 1 (sensor.temp.room1): $PUBLISHED_TEST1"
echo "  Test 2 (sensor.humidity.room1): $PUBLISHED_TEST2"
echo "  Test 3 (sensor.temp.room2): $PUBLISHED_TEST3"
echo "  Test 4 (other.topic): $PUBLISHED_TEST4"
echo "  Test 5 (sensor.temp.room1.extra): $PUBLISHED_TEST5"
echo ""
echo "Subscriber1 (exact: sensor.temp.room1):"
echo "  Received test1: $SUB1_TEST1 (expected: 1)"
echo "  Received test2: $SUB1_TEST2 (expected: 0)"
echo "  Received test3: $SUB1_TEST3 (expected: 0)"
echo "  Received test5: $SUB1_TEST5 (expected: 0)"
echo ""
echo "Subscriber2 (single wildcard: sensor.*.room1):"
echo "  Received test1: $SUB2_TEST1 (expected: 1)"
echo "  Received test2: $SUB2_TEST2 (expected: 1)"
echo "  Received test3: $SUB2_TEST3 (expected: 0)"
echo "  Received test5: $SUB2_TEST5 (expected: 0)"
echo ""
echo "Subscriber3 (multi wildcard: sensor.**):"
echo "  Received test1: $SUB3_TEST1 (expected: 1)"
echo "  Received test2: $SUB3_TEST2 (expected: 1)"
echo "  Received test3: $SUB3_TEST3 (expected: 1)"
echo "  Received test5: $SUB3_TEST5 (expected: 1)"
echo ""

# Determine test result
FAILURES=0

# Check all tests were published
if [ "$PUBLISHED_TEST1" -eq 0 ] || [ "$PUBLISHED_TEST2" -eq 0 ] || [ "$PUBLISHED_TEST3" -eq 0 ] || [ "$PUBLISHED_TEST4" -eq 0 ] || [ "$PUBLISHED_TEST5" -eq 0 ]; then
    echo -e "${RED}✗ TEST FAILED${NC}"
    echo "  - Not all test messages were published"
    FAILURES=1
fi

# Check subscriber1 (exact match) - should only receive test1
if [ "$SUB1_TEST1" -ne 1 ] || [ "$SUB1_TEST2" -ne 0 ] || [ "$SUB1_TEST3" -ne 0 ] || [ "$SUB1_TEST5" -ne 0 ]; then
    if [ "$FAILURES" -eq 0 ]; then
        echo -e "${RED}✗ TEST FAILED${NC}"
    fi
    echo "  - Subscriber1 (exact match) received incorrect messages"
    FAILURES=1
fi

# Check subscriber2 (single wildcard) - should receive test1 and test2
if [ "$SUB2_TEST1" -ne 1 ] || [ "$SUB2_TEST2" -ne 1 ] || [ "$SUB2_TEST3" -ne 0 ] || [ "$SUB2_TEST5" -ne 0 ]; then
    if [ "$FAILURES" -eq 0 ]; then
        echo -e "${RED}✗ TEST FAILED${NC}"
    fi
    echo "  - Subscriber2 (single wildcard) received incorrect messages"
    FAILURES=1
fi

# Check subscriber3 (multi wildcard) - should receive test1, test2, test3, test5
if [ "$SUB3_TEST1" -ne 1 ] || [ "$SUB3_TEST2" -ne 1 ] || [ "$SUB3_TEST3" -ne 1 ] || [ "$SUB3_TEST5" -ne 1 ]; then
    if [ "$FAILURES" -eq 0 ]; then
        echo -e "${RED}✗ TEST FAILED${NC}"
    fi
    echo "  - Subscriber3 (multi wildcard) received incorrect messages"
    FAILURES=1
fi

if [ "$FAILURES" -eq 0 ]; then
    echo -e "${GREEN}✓ TEST PASSED${NC}"
    echo "  - All 5 test messages published successfully"
    echo "  - Exact match pattern (sensor.temp.room1) working correctly"
    echo "  - Single-level wildcard (sensor.*.room1) working correctly"
    echo "  - Multi-level wildcard (sensor.**) working correctly"
    echo "  - Wildcard message routing working across nodes!"
    exit 0
else
    echo ""
    echo "Check logs at: /tmp/pubsub-wildcard-test-logs.txt"
    echo "Or run: docker compose -f docker/docker-compose.pubsub-wildcard-test.yml logs"
    exit 1
fi
