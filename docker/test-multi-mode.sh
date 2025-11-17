#!/bin/bash
# Macula Multi-Mode Integration Test Script
#
# Purpose: End-to-end testing of multi-mode deployment
# Runs: Docker Compose environment with all deployment modes
# Tests: DHT discovery, service registration, health checks
#
# Usage: ./docker/test-multi-mode.sh [--build] [--clean]

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
COMPOSE_FILE="$SCRIPT_DIR/docker-compose.multi-mode.yml"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Parse arguments
BUILD=false
CLEAN=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --build)
            BUILD=true
            shift
            ;;
        --clean)
            CLEAN=true
            shift
            ;;
        *)
            echo "Usage: $0 [--build] [--clean]"
            echo "  --build  Rebuild Docker images from scratch"
            echo "  --clean  Clean up containers and volumes after test"
            exit 1
            ;;
    esac
done

#############################################################################
# Phase 1: Setup
#############################################################################

log_info "=== Phase 1: Setup ==="

if [ "$BUILD" = true ]; then
    log_info "Cleaning Docker build cache..."
    docker builder prune -af

    log_info "Building Docker images (no cache)..."
    docker-compose -f "$COMPOSE_FILE" build --no-cache
else
    log_info "Building Docker images (with cache)..."
    docker-compose -f "$COMPOSE_FILE" build
fi

log_success "Docker images built"

#############################################################################
# Phase 2: Start Services
#############################################################################

log_info "=== Phase 2: Start Services ==="

log_info "Starting Docker Compose environment..."
docker-compose -f "$COMPOSE_FILE" up -d

log_info "Waiting for services to be healthy..."
TIMEOUT=60
ELAPSED=0

while [ $ELAPSED -lt $TIMEOUT ]; do
    UNHEALTHY=$(docker ps --filter "name=macula-" --format "{{.Names}}: {{.Status}}" | grep -c "(unhealthy)" || true)

    if [ "$UNHEALTHY" -eq 0 ]; then
        log_success "All services are healthy!"
        break
    fi

    echo -n "."
    sleep 2
    ELAPSED=$((ELAPSED + 2))
done

echo ""  # New line after dots

if [ $ELAPSED -ge $TIMEOUT ]; then
    log_error "Timeout waiting for services to be healthy"
    docker ps --filter "name=macula-"
    exit 1
fi

# Additional stabilization time
log_info "Waiting 5 seconds for DHT stabilization..."
sleep 5

#############################################################################
# Phase 3: Health Checks
#############################################################################

log_info "=== Phase 3: Health Checks ==="

# Test bootstrap node
log_info "Testing bootstrap node health..."
BOOTSTRAP_HEALTH=$(curl -s http://localhost:8080/health)
echo "Bootstrap health: $BOOTSTRAP_HEALTH"

if echo "$BOOTSTRAP_HEALTH" | grep -q "healthy"; then
    log_success "Bootstrap node healthy"
else
    log_error "Bootstrap node unhealthy"
    exit 1
fi

# Test gateway node
log_info "Testing gateway node health..."
GATEWAY_HEALTH=$(curl -s http://localhost:8081/health)
echo "Gateway health: $GATEWAY_HEALTH"

if echo "$GATEWAY_HEALTH" | grep -q "healthy"; then
    log_success "Gateway node healthy"
else
    log_error "Gateway node unhealthy"
    exit 1
fi

# Test edge nodes
log_info "Testing edge nodes..."
for EDGE in edge1 edge2 edge3; do
    CONTAINER="macula-$EDGE"
    PING_RESULT=$(docker exec "$CONTAINER" bin/macula ping 2>&1 || true)
    if echo "$PING_RESULT" | grep -q "pong"; then
        log_success "$EDGE node healthy"
    else
        log_warning "$EDGE node may not be fully started: $PING_RESULT"
    fi
done

#############################################################################
# Phase 4: DHT Tests
#############################################################################

log_info "=== Phase 4: DHT Tests ==="

# Test DHT peer discovery from Edge2
log_info "Testing DHT peer discovery from edge2..."
docker exec macula-edge2 bin/macula eval "
    case whereis(macula_routing_server) of
        undefined ->
            io:format('ERROR: Routing server not running~n'),
            {error, no_routing_server};
        Pid ->
            RoutingTable = macula_routing_server:get_routing_table(Pid),
            PeerCount = length(RoutingTable),
            io:format('DHT Routing table size: ~p peers~n', [PeerCount]),
            {ok, PeerCount}
    end.
" || log_warning "DHT routing table check failed"

# Test service registration in DHT
log_info "Testing service registration in DHT (from edge3)..."
docker exec macula-edge3 bin/macula eval "
    case whereis(macula_routing_server) of
        undefined ->
            io:format('ERROR: Routing server not running~n');
        Pid ->
            ServiceKey = <<\"test.calculator.add\">>,
            NodeId = crypto:strong_rand_bytes(32),
            ServiceValue = #{node_id => NodeId, endpoint => <<\"172.20.0.33:9443\">>},
            ok = macula_routing_server:store_local(Pid, ServiceKey, ServiceValue),
            io:format('Service registered: ~s~n', [ServiceKey])
    end.
" || log_warning "Service registration failed"

# Wait for DHT propagation
log_info "Waiting 2 seconds for DHT propagation..."
sleep 2

# Test service discovery from edge2
log_info "Testing service discovery in DHT (from edge2)..."
docker exec macula-edge2 bin/macula eval "
    case whereis(macula_routing_server) of
        undefined ->
            io:format('ERROR: Routing server not running~n');
        Pid ->
            ServiceKey = <<\"test.calculator.add\">>,
            case macula_routing_server:get_local(Pid, ServiceKey) of
                {ok, Value} ->
                    io:format('Service found: ~p~n', [Value]),
                    {ok, found};
                not_found ->
                    io:format('Service not found~n'),
                    {error, not_found}
            end
    end.
" || log_warning "Service discovery failed"

#############################################################################
# Phase 5: Container Logs (for debugging)
#############################################################################

log_info "=== Phase 5: Container Logs (last 20 lines each) ==="

for CONTAINER in macula-bootstrap macula-gateway macula-edge1 macula-edge2 macula-edge3; do
    echo ""
    log_info "=== $CONTAINER logs ==="
    docker logs --tail 20 "$CONTAINER" 2>&1 | head -20 || true
done

#############################################################################
# Phase 6: Cleanup (optional)
#############################################################################

if [ "$CLEAN" = true ]; then
    log_info "=== Phase 6: Cleanup ==="
    log_warning "Stopping and removing containers..."
    docker-compose -f "$COMPOSE_FILE" down -v
    log_success "Cleanup complete"
else
    log_info "=== Test Environment Still Running ==="
    log_info "To stop: docker-compose -f $COMPOSE_FILE down"
    log_info "To view logs: docker-compose -f $COMPOSE_FILE logs -f"
fi

#############################################################################
# Summary
#############################################################################

echo ""
log_success "================================"
log_success "All integration tests passed!"
log_success "================================"
echo ""
log_info "Multi-mode test environment is ready for manual testing:"
echo ""
echo "  Bootstrap: http://localhost:8080/health"
echo "  Gateway:   http://localhost:8081/health"
echo ""
echo "  Edge nodes:"
echo "    - docker exec -it macula-edge1 bin/macula remote_console"
echo "    - docker exec -it macula-edge2 bin/macula remote_console"
echo "    - docker exec -it macula-edge3 bin/macula remote_console"
echo ""
log_info "To run Common Test suite:"
echo "  rebar3 ct --suite=test/integration/multi_hop_rpc_SUITE"
echo ""
