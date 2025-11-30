#!/bin/bash
# 50 Chatter Peers - Mixed NAT Environment
#
# Usage:
#   ./run-50-chatters.sh start     - Build and start all 50 chatters
#   ./run-50-chatters.sh stop      - Stop all containers
#   ./run-50-chatters.sh logs      - Follow all logs
#   ./run-50-chatters.sh stats     - Show message counts from each peer
#   ./run-50-chatters.sh status    - Show container status
#   ./run-50-chatters.sh rebuild   - Clean rebuild and start

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPOSE_FILE="$SCRIPT_DIR/docker-compose.50-chatters.yml"
PROJECT_NAME="chatter50"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_section() {
    echo ""
    echo -e "${BLUE}=== $1 ===${NC}"
}

cmd_start() {
    log_section "Starting 50 Chatter Peers"

    # Stop any existing containers first
    docker compose -f "$COMPOSE_FILE" -p "$PROJECT_NAME" down 2>/dev/null || true

    log_info "Building Docker images..."
    docker compose -f "$COMPOSE_FILE" -p "$PROJECT_NAME" build

    log_info "Starting bootstrap node..."
    docker compose -f "$COMPOSE_FILE" -p "$PROJECT_NAME" up -d bootstrap

    log_info "Waiting for bootstrap to be ready (15s)..."
    sleep 15

    log_info "Starting NAT routers..."
    docker compose -f "$COMPOSE_FILE" -p "$PROJECT_NAME" up -d router-fullcone router-restricted router-symmetric

    log_info "Waiting for routers to configure (5s)..."
    sleep 5

    log_info "Starting Full Cone NAT peers (fc01-fc17)..."
    docker compose -f "$COMPOSE_FILE" -p "$PROJECT_NAME" up -d \
        fc01 fc02 fc03 fc04 fc05 fc06 fc07 fc08 fc09 fc10 \
        fc11 fc12 fc13 fc14 fc15 fc16 fc17

    log_info "Waiting 5s before starting next batch..."
    sleep 5

    log_info "Starting Restricted NAT peers (rc01-rc17)..."
    docker compose -f "$COMPOSE_FILE" -p "$PROJECT_NAME" up -d \
        rc01 rc02 rc03 rc04 rc05 rc06 rc07 rc08 rc09 rc10 \
        rc11 rc12 rc13 rc14 rc15 rc16 rc17

    log_info "Waiting 5s before starting next batch..."
    sleep 5

    log_info "Starting Symmetric NAT peers (sy01-sy16)..."
    docker compose -f "$COMPOSE_FILE" -p "$PROJECT_NAME" up -d \
        sy01 sy02 sy03 sy04 sy05 sy06 sy07 sy08 sy09 sy10 \
        sy11 sy12 sy13 sy14 sy15 sy16

    log_section "All 50 Chatters Started!"
    echo ""
    echo "NAT Distribution:"
    echo "  - Full Cone NAT:     fc01-fc17 (17 peers)"
    echo "  - Restricted NAT:    rc01-rc17 (17 peers)"
    echo "  - Symmetric NAT:     sy01-sy16 (16 peers)"
    echo "  - Total:             50 peers"
    echo ""
    echo "Commands:"
    echo "  $0 logs    - Follow all chat messages"
    echo "  $0 stats   - Show message statistics"
    echo "  $0 status  - Show container status"
    echo "  $0 stop    - Stop all containers"
}

cmd_stop() {
    log_section "Stopping 50 Chatter Peers"
    docker compose -f "$COMPOSE_FILE" -p "$PROJECT_NAME" down
    log_info "All containers stopped"
}

cmd_logs() {
    log_section "Following Chat Logs (Ctrl+C to exit)"
    echo "Filter: Showing only [Chatter] messages"
    echo ""
    docker compose -f "$COMPOSE_FILE" -p "$PROJECT_NAME" logs -f 2>&1 | grep -E "\[Chatter|Broadcast:|Received from"
}

cmd_stats() {
    log_section "Message Statistics"
    echo ""

    CONTAINERS=$(docker ps --filter "name=chatter50-" --format "{{.Names}}" | grep -v router | sort)

    printf "%-25s %10s %10s %s\n" "PEER" "SENT" "RECEIVED" "NAT TYPE"
    printf "%-25s %10s %10s %s\n" "----" "----" "--------" "--------"

    for container in $CONTAINERS; do
        # Extract stats from logs
        SENT=$(docker logs "$container" 2>&1 | grep -c "Broadcast:" || echo "0")
        RECEIVED=$(docker logs "$container" 2>&1 | grep -c "Received from" || echo "0")

        # Determine NAT type from container name
        if [[ "$container" == chatter50-fc* ]]; then
            NAT_TYPE="Full Cone"
        elif [[ "$container" == chatter50-rc* ]]; then
            NAT_TYPE="Restricted"
        elif [[ "$container" == chatter50-sy* ]]; then
            NAT_TYPE="Symmetric"
        elif [[ "$container" == chatter50-bootstrap* ]]; then
            NAT_TYPE="Public"
        else
            NAT_TYPE="Unknown"
        fi

        printf "%-25s %10s %10s %s\n" "$container" "$SENT" "$RECEIVED" "$NAT_TYPE"
    done

    echo ""
    log_info "Stats collected at $(date)"
}

cmd_status() {
    log_section "Container Status"
    echo ""

    echo "Bootstrap:"
    docker ps --filter "name=chatter50-bootstrap" --format "  {{.Names}}: {{.Status}}" 2>/dev/null || echo "  Not running"

    echo ""
    echo "NAT Routers:"
    docker ps --filter "name=chatter50-router" --format "  {{.Names}}: {{.Status}}" 2>/dev/null || echo "  Not running"

    echo ""
    echo "Full Cone NAT Peers (17):"
    docker ps --filter "name=chatter50-fc" --format "  {{.Names}}: {{.Status}}" 2>/dev/null || echo "  Not running"

    echo ""
    echo "Restricted NAT Peers (17):"
    docker ps --filter "name=chatter50-rc" --format "  {{.Names}}: {{.Status}}" 2>/dev/null || echo "  Not running"

    echo ""
    echo "Symmetric NAT Peers (16):"
    docker ps --filter "name=chatter50-sy" --format "  {{.Names}}: {{.Status}}" 2>/dev/null || echo "  Not running"

    echo ""
    RUNNING=$(docker ps --filter "name=chatter50-" --format "{{.Names}}" | wc -l)
    log_info "Total running containers: $RUNNING"
}

cmd_rebuild() {
    log_section "Clean Rebuild"

    log_info "Stopping all containers..."
    docker compose -f "$COMPOSE_FILE" -p "$PROJECT_NAME" down 2>/dev/null || true

    log_info "Pruning Docker build cache..."
    docker builder prune -af 2>/dev/null || true

    log_info "Rebuilding images..."
    docker compose -f "$COMPOSE_FILE" -p "$PROJECT_NAME" build --no-cache

    log_info "Starting containers..."
    cmd_start
}

cmd_nat_profiles() {
    log_section "NAT Profiles (Sample)"
    echo ""
    echo "Checking NAT profiles for sample peers from each NAT type..."
    echo ""

    # Sample one peer from each NAT type
    for peer in fc01 rc01 sy01; do
        container="chatter50-$peer"
        if docker ps --format "{{.Names}}" | grep -q "^${container}$"; then
            echo "=== $peer NAT Profile ==="
            docker logs "$container" 2>&1 | grep -i "nat.*profile\|mapping_policy\|filtering_policy" | tail -5
            echo ""
        fi
    done
}

# Main
case "${1:-help}" in
    start)
        cmd_start
        ;;
    stop)
        cmd_stop
        ;;
    logs)
        cmd_logs
        ;;
    stats)
        cmd_stats
        ;;
    status)
        cmd_status
        ;;
    rebuild)
        cmd_rebuild
        ;;
    nat-profiles)
        cmd_nat_profiles
        ;;
    *)
        echo "Usage: $0 {start|stop|logs|stats|status|rebuild|nat-profiles}"
        echo ""
        echo "Commands:"
        echo "  start        - Build and start all 50 chatters"
        echo "  stop         - Stop all containers"
        echo "  logs         - Follow chat message logs"
        echo "  stats        - Show message counts per peer"
        echo "  status       - Show container status"
        echo "  rebuild      - Clean rebuild and start"
        echo "  nat-profiles - Show NAT profiles for sample peers"
        exit 1
        ;;
esac
