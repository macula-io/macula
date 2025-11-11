#!/bin/bash

# Multi-Node RPC Testing Script
# Tests provider selection, failover, and multi-endpoint routing

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/.."

echo "==> Multi-Node Macula RPC Testing"
echo ""

# Function to show usage
usage() {
    cat <<EOF
Usage: $0 [command]

Commands:
    start       Start all nodes and run tests
    stop        Stop all nodes
    logs        Show logs from all containers
    client      Show client logs only
    clean       Stop and remove all containers
    status      Show status of all containers

Examples:
    $0 start       # Start and run tests
    $0 logs        # Watch all logs
    $0 client      # Watch client test output
    $0 stop        # Stop all nodes

EOF
    exit 1
}

CMD="${1:-start}"

case "$CMD" in
    start)
        echo "==> Starting multi-node test environment..."
        docker-compose -f docker/docker-compose.multi-node-test.yml up --abort-on-container-exit
        ;;

    stop)
        echo "==> Stopping all nodes..."
        docker-compose -f docker/docker-compose.multi-node-test.yml stop
        ;;

    logs)
        echo "==> Showing logs from all containers..."
        docker-compose -f docker/docker-compose.multi-node-test.yml logs -f
        ;;

    client)
        echo "==> Showing client logs..."
        docker-compose -f docker/docker-compose.multi-node-test.yml logs -f macula-client
        ;;

    provider1)
        echo "==> Showing provider1 logs..."
        docker-compose -f docker/docker-compose.multi-node-test.yml logs -f macula-provider1
        ;;

    provider2)
        echo "==> Showing provider2 logs..."
        docker-compose -f docker/docker-compose.multi-node-test.yml logs -f macula-provider2
        ;;

    provider3)
        echo "==> Showing provider3 logs..."
        docker-compose -f docker/docker-compose.multi-node-test.yml logs -f macula-provider3
        ;;

    clean)
        echo "==> Cleaning up..."
        docker-compose -f docker/docker-compose.multi-node-test.yml down -v
        echo "==> Done!"
        ;;

    status)
        echo "==> Container status:"
        docker-compose -f docker/docker-compose.multi-node-test.yml ps
        ;;

    *)
        echo "Unknown command: $CMD"
        usage
        ;;
esac
