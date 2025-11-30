#!/bin/bash
# 50 Peer PING/PONG - Mixed NAT Environment
#
# Usage:
#   ./run-50-ping-pong.sh start     - Build and start all 50 peers
#   ./run-50-ping-pong.sh stop      - Stop all containers
#   ./run-50-ping-pong.sh logs      - Follow PING/PONG logs
#   ./run-50-ping-pong.sh stats     - Show RTT statistics from each peer
#   ./run-50-ping-pong.sh status    - Show container status
#   ./run-50-ping-pong.sh rebuild   - Clean rebuild and start

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPOSE_FILE="$SCRIPT_DIR/docker-compose.50-ping-pong.yml"
PROJECT_NAME="pingpong50"

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
    log_section "Starting 50 PING/PONG Peers"

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

    log_section "All 50 PING/PONG Peers Started!"
    echo ""
    echo "NAT Distribution:"
    echo "  - Full Cone NAT:     fc01-fc17 (17 peers)"
    echo "  - Restricted NAT:    rc01-rc17 (17 peers)"
    echo "  - Symmetric NAT:     sy01-sy16 (16 peers)"
    echo "  - Total:             50 peers"
    echo ""
    echo "PING/PONG Configuration:"
    echo "  - Ping Interval:     5000ms"
    echo "  - Ping Timeout:      3000ms"
    echo ""
    echo "Commands:"
    echo "  $0 logs    - Follow PING/PONG messages"
    echo "  $0 stats   - Show RTT statistics"
    echo "  $0 status  - Show container status"
    echo "  $0 stop    - Stop all containers"
}

cmd_stop() {
    log_section "Stopping 50 PING/PONG Peers"
    docker compose -f "$COMPOSE_FILE" -p "$PROJECT_NAME" down
    log_info "All containers stopped"
}

cmd_logs() {
    log_section "Following PING/PONG Logs (Ctrl+C to exit)"
    echo "Filter: Showing only [PingPong] messages"
    echo ""
    docker compose -f "$COMPOSE_FILE" -p "$PROJECT_NAME" logs -f 2>&1 | grep -E "\[PingPong|PING|PONG|RTT"
}

cmd_stats() {
    log_section "RTT Statistics by NAT Type"
    echo ""

    # Aggregate stats by NAT type
    echo "=== Full Cone NAT (fc01-fc17) ==="
    for peer in fc01 fc05 fc10 fc15; do
        container="pingpong50-$peer"
        if docker ps --format "{{.Names}}" | grep -q "^${container}$"; then
            PINGS=$(docker logs "$container" 2>&1 | grep -c "PING ->" || echo "0")
            PONGS=$(docker logs "$container" 2>&1 | grep -c "PONG <-" || echo "0")
            TIMEOUTS=$(docker logs "$container" 2>&1 | grep -c "PING timeout" || echo "0")

            # Extract RTT values
            RTTS=$(docker logs "$container" 2>&1 | grep "PONG <-" | grep -oP '\d+ms RTT' | grep -oP '\d+' || true)
            if [ -n "$RTTS" ]; then
                AVG_RTT=$(echo "$RTTS" | awk '{sum+=$1; count++} END {if(count>0) printf "%.1f", sum/count; else print "N/A"}')
            else
                AVG_RTT="N/A"
            fi

            printf "  %-10s PINGs: %4s  PONGs: %4s  Timeouts: %4s  Avg RTT: %sms\n" \
                   "$peer" "$PINGS" "$PONGS" "$TIMEOUTS" "$AVG_RTT"
        fi
    done

    echo ""
    echo "=== Restricted NAT (rc01-rc17) ==="
    for peer in rc01 rc05 rc10 rc15; do
        container="pingpong50-$peer"
        if docker ps --format "{{.Names}}" | grep -q "^${container}$"; then
            PINGS=$(docker logs "$container" 2>&1 | grep -c "PING ->" || echo "0")
            PONGS=$(docker logs "$container" 2>&1 | grep -c "PONG <-" || echo "0")
            TIMEOUTS=$(docker logs "$container" 2>&1 | grep -c "PING timeout" || echo "0")

            RTTS=$(docker logs "$container" 2>&1 | grep "PONG <-" | grep -oP '\d+ms RTT' | grep -oP '\d+' || true)
            if [ -n "$RTTS" ]; then
                AVG_RTT=$(echo "$RTTS" | awk '{sum+=$1; count++} END {if(count>0) printf "%.1f", sum/count; else print "N/A"}')
            else
                AVG_RTT="N/A"
            fi

            printf "  %-10s PINGs: %4s  PONGs: %4s  Timeouts: %4s  Avg RTT: %sms\n" \
                   "$peer" "$PINGS" "$PONGS" "$TIMEOUTS" "$AVG_RTT"
        fi
    done

    echo ""
    echo "=== Symmetric NAT (sy01-sy16) ==="
    for peer in sy01 sy05 sy10 sy15; do
        container="pingpong50-$peer"
        if docker ps --format "{{.Names}}" | grep -q "^${container}$"; then
            PINGS=$(docker logs "$container" 2>&1 | grep -c "PING ->" || echo "0")
            PONGS=$(docker logs "$container" 2>&1 | grep -c "PONG <-" || echo "0")
            TIMEOUTS=$(docker logs "$container" 2>&1 | grep -c "PING timeout" || echo "0")

            RTTS=$(docker logs "$container" 2>&1 | grep "PONG <-" | grep -oP '\d+ms RTT' | grep -oP '\d+' || true)
            if [ -n "$RTTS" ]; then
                AVG_RTT=$(echo "$RTTS" | awk '{sum+=$1; count++} END {if(count>0) printf "%.1f", sum/count; else print "N/A"}')
            else
                AVG_RTT="N/A"
            fi

            printf "  %-10s PINGs: %4s  PONGs: %4s  Timeouts: %4s  Avg RTT: %sms\n" \
                   "$peer" "$PINGS" "$PONGS" "$TIMEOUTS" "$AVG_RTT"
        fi
    done

    echo ""
    log_info "Stats collected at $(date)"
    echo ""
    echo "Note: Showing sample of 4 peers per NAT type. Use 'logs' for full details."
}

cmd_status() {
    log_section "Container Status"
    echo ""

    echo "Bootstrap:"
    docker ps --filter "name=pingpong50-bootstrap" --format "  {{.Names}}: {{.Status}}" 2>/dev/null || echo "  Not running"

    echo ""
    echo "NAT Routers:"
    docker ps --filter "name=pingpong50-router" --format "  {{.Names}}: {{.Status}}" 2>/dev/null || echo "  Not running"

    echo ""
    echo "Full Cone NAT Peers (17):"
    docker ps --filter "name=pingpong50-fc" --format "  {{.Names}}: {{.Status}}" 2>/dev/null || echo "  Not running"

    echo ""
    echo "Restricted NAT Peers (17):"
    docker ps --filter "name=pingpong50-rc" --format "  {{.Names}}: {{.Status}}" 2>/dev/null || echo "  Not running"

    echo ""
    echo "Symmetric NAT Peers (16):"
    docker ps --filter "name=pingpong50-sy" --format "  {{.Names}}: {{.Status}}" 2>/dev/null || echo "  Not running"

    echo ""
    RUNNING=$(docker ps --filter "name=pingpong50-" --format "{{.Names}}" | wc -l)
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
    *)
        echo "Usage: $0 {start|stop|logs|stats|status|rebuild}"
        echo ""
        echo "Commands:"
        echo "  start        - Build and start all 50 PING/PONG peers"
        echo "  stop         - Stop all containers"
        echo "  logs         - Follow PING/PONG message logs"
        echo "  stats        - Show RTT statistics per NAT type"
        echo "  status       - Show container status"
        echo "  rebuild      - Clean rebuild and start"
        exit 1
        ;;
esac
