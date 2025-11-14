#!/bin/bash
# Master script to run all E2E tests and measure coverage
# This exercises the macula_connection code through real multi-node scenarios

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo ""
echo "======================================================================"
echo "  Macula E2E Test Suite"
echo "======================================================================"
echo ""
echo "This suite tests macula_connection through real multi-node scenarios:"
echo "  1. Pub/Sub message flow (1 publisher + 2 subscribers)"
echo "  2. Wildcard Pub/Sub matching (1 publisher + 3 subscribers with patterns)"
echo "  3. RPC with service discovery (3 providers + 1 client)"
echo ""
echo "These tests exercise actual QUIC connections, DHT operations,"
echo "message routing, wildcard matching, and will contribute to code coverage."
echo ""
echo "======================================================================"
echo ""

# Track results
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Change to project root
cd "$PROJECT_ROOT"

# Make scripts executable
chmod +x "$SCRIPT_DIR"/*.sh

echo -e "${BLUE}[1/3] Running Pub/Sub E2E Test...${NC}"
echo ""
if "$SCRIPT_DIR/run-pubsub-test.sh"; then
    TESTS_PASSED=$((TESTS_PASSED + 1))
    echo -e "${GREEN}✓ Pub/Sub test passed${NC}"
else
    TESTS_FAILED=$((TESTS_FAILED + 1))
    echo -e "${RED}✗ Pub/Sub test failed${NC}"
fi
TESTS_RUN=$((TESTS_RUN + 1))

echo ""
echo "======================================================================"
echo ""

echo -e "${BLUE}[2/3] Running Wildcard Pub/Sub E2E Test...${NC}"
echo ""
if "$SCRIPT_DIR/run-pubsub-wildcard-test.sh"; then
    TESTS_PASSED=$((TESTS_PASSED + 1))
    echo -e "${GREEN}✓ Wildcard Pub/Sub test passed${NC}"
else
    TESTS_FAILED=$((TESTS_FAILED + 1))
    echo -e "${RED}✗ Wildcard Pub/Sub test failed${NC}"
fi
TESTS_RUN=$((TESTS_RUN + 1))

echo ""
echo "======================================================================"
echo ""

echo -e "${BLUE}[3/3] Running RPC E2E Test...${NC}"
echo ""
if "$SCRIPT_DIR/run-rpc-test.sh"; then
    TESTS_PASSED=$((TESTS_PASSED + 1))
    echo -e "${GREEN}✓ RPC test passed${NC}"
else
    TESTS_FAILED=$((TESTS_FAILED + 1))
    echo -e "${RED}✗ RPC test failed${NC}"
fi
TESTS_RUN=$((TESTS_RUN + 1))

echo ""
echo "======================================================================"
echo "  E2E Test Suite Results"
echo "======================================================================"
echo ""
echo "Tests run: $TESTS_RUN"
echo -e "Tests passed: ${GREEN}$TESTS_PASSED${NC}"
if [ "$TESTS_FAILED" -gt 0 ]; then
    echo -e "Tests failed: ${RED}$TESTS_FAILED${NC}"
else
    echo -e "Tests failed: $TESTS_FAILED"
fi
echo ""

if [ "$TESTS_FAILED" -eq 0 ]; then
    echo -e "${GREEN}======================================================================"
    echo "  ALL E2E TESTS PASSED!"
    echo -e "======================================================================${NC}"
    echo ""
    echo "These tests exercise:"
    echo "  - macula_connection lifecycle (connect/disconnect)"
    echo "  - Pub/sub message publishing and routing"
    echo "  - Service advertisement to DHT"
    echo "  - Service discovery from DHT"
    echo "  - RPC calls with provider selection"
    echo "  - Multi-node message flow"
    echo ""
    echo "To measure code coverage impact:"
    echo "  1. Run: rebar3 eunit"
    echo "  2. Run: rebar3 cover --verbose"
    echo "  3. Compare coverage before/after E2E tests"
    echo ""
    exit 0
else
    echo -e "${RED}======================================================================"
    echo "  SOME TESTS FAILED"
    echo -e "======================================================================${NC}"
    echo ""
    echo "Check logs in /tmp/ for details:"
    echo "  /tmp/pubsub-test-logs.txt"
    echo "  /tmp/rpc-test-logs.txt"
    echo ""
    exit 1
fi
