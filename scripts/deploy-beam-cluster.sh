#!/bin/bash
#
# Deploy Macula Cross-Gateway Test to Beam Cluster
#
# This script deploys a 4-node macula mesh across the beam cluster:
#   - beam00 (192.168.1.10): Registry/Seed node
#   - beam01 (192.168.1.11): Gateway with subscriber
#   - beam02 (192.168.1.12): Gateway with publisher
#   - beam03 (192.168.1.13): Additional gateway (observer)
#
# Prerequisites:
#   - SSH access to beam00-03.lab (user: rl, password: rl)
#   - Docker installed on all beam nodes
#   - Network connectivity between nodes on port 9443 (QUIC)
#
# Usage:
#   ./scripts/deploy-beam-cluster.sh build    # Build and push image
#   ./scripts/deploy-beam-cluster.sh deploy   # Deploy containers
#   ./scripts/deploy-beam-cluster.sh logs     # Show logs from all nodes
#   ./scripts/deploy-beam-cluster.sh stop     # Stop all containers
#   ./scripts/deploy-beam-cluster.sh status   # Check status
#

set -e

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MACULA_DIR="$(dirname "$SCRIPT_DIR")"
IMAGE_NAME="macula:beam-test"
IMAGE_TAR="/tmp/macula-beam-test.tar"

# Beam cluster nodes
BEAM00="rl@192.168.1.10"
BEAM01="rl@192.168.1.11"
BEAM02="rl@192.168.1.12"
BEAM03="rl@192.168.1.13"

# Container names
CONTAINER_REGISTRY="macula-registry"
CONTAINER_SUBSCRIBER="macula-subscriber"
CONTAINER_PUBLISHER="macula-publisher"
CONTAINER_OBSERVER="macula-observer"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
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

# Function to run command on remote host via SSH
ssh_cmd() {
    local host=$1
    shift
    sshpass -p "rl" ssh -o StrictHostKeyChecking=no "$host" "$@"
}

# Function to copy file to remote host
scp_file() {
    local src=$1
    local host=$2
    local dst=$3
    sshpass -p "rl" scp -o StrictHostKeyChecking=no "$src" "$host:$dst"
}

build_image() {
    log_info "Building Docker image..."

    # Clean build cache
    docker builder prune -af 2>/dev/null || true

    # Build from macula directory (Dockerfile is at root level)
    docker build -t "$IMAGE_NAME" -f "$MACULA_DIR/Dockerfile" "$MACULA_DIR"

    log_info "Saving image to tar file..."
    docker save "$IMAGE_NAME" -o "$IMAGE_TAR"

    log_info "Image built and saved to $IMAGE_TAR"
}

push_image() {
    log_info "Pushing image to beam cluster nodes..."

    for node in $BEAM00 $BEAM01 $BEAM02 $BEAM03; do
        log_info "Copying image to $node..."
        scp_file "$IMAGE_TAR" "$node" "/tmp/macula-beam-test.tar"

        log_info "Loading image on $node..."
        ssh_cmd "$node" "docker load -i /tmp/macula-beam-test.tar && rm /tmp/macula-beam-test.tar"
    done

    log_info "Image pushed to all nodes"
}

deploy_registry() {
    log_info "Deploying registry on beam00 (192.168.1.10)..."

    # Stop existing container
    ssh_cmd "$BEAM00" "docker rm -f $CONTAINER_REGISTRY 2>/dev/null || true"

    # Run registry
    ssh_cmd "$BEAM00" "docker run -d \
        --name $CONTAINER_REGISTRY \
        --hostname registry.macula.beam \
        --network host \
        -e NODE_TYPE=registry \
        -e NODE_NAME=registry \
        -e NODE_HOST=192.168.1.10 \
        -e COOKIE=macula_beam_cluster \
        -e GATEWAY_PORT=9443 \
        $IMAGE_NAME"

    log_info "Registry started on beam00"
}

deploy_subscriber() {
    log_info "Deploying subscriber gateway on beam01 (192.168.1.11)..."

    # Stop existing container
    ssh_cmd "$BEAM01" "docker rm -f $CONTAINER_SUBSCRIBER 2>/dev/null || true"

    # Run subscriber gateway
    ssh_cmd "$BEAM01" "docker run -d \
        --name $CONTAINER_SUBSCRIBER \
        --hostname subscriber.macula.beam \
        --network host \
        -e NODE_TYPE=gateway_subscriber \
        -e NODE_NAME=subscriber \
        -e NODE_HOST=192.168.1.11 \
        -e COOKIE=macula_beam_cluster \
        -e REGISTRY_ENDPOINT=https://192.168.1.10:9443 \
        -e GATEWAY_PORT=9443 \
        -e SUBSCRIBE_TOPIC=beam.cluster.events \
        $IMAGE_NAME"

    log_info "Subscriber gateway started on beam01"
}

deploy_publisher() {
    log_info "Deploying publisher gateway on beam02 (192.168.1.12)..."

    # Stop existing container
    ssh_cmd "$BEAM02" "docker rm -f $CONTAINER_PUBLISHER 2>/dev/null || true"

    # Run publisher gateway
    ssh_cmd "$BEAM02" "docker run -d \
        --name $CONTAINER_PUBLISHER \
        --hostname publisher.macula.beam \
        --network host \
        -e NODE_TYPE=gateway_publisher \
        -e NODE_NAME=publisher \
        -e NODE_HOST=192.168.1.12 \
        -e COOKIE=macula_beam_cluster \
        -e REGISTRY_ENDPOINT=https://192.168.1.10:9443 \
        -e GATEWAY_PORT=9443 \
        -e PUBLISH_TOPIC=beam.cluster.events \
        $IMAGE_NAME"

    log_info "Publisher gateway started on beam02"
}

deploy_observer() {
    log_info "Deploying observer gateway on beam03 (192.168.1.13)..."

    # Stop existing container
    ssh_cmd "$BEAM03" "docker rm -f $CONTAINER_OBSERVER 2>/dev/null || true"

    # Run observer gateway (subscriber to same topic)
    ssh_cmd "$BEAM03" "docker run -d \
        --name $CONTAINER_OBSERVER \
        --hostname observer.macula.beam \
        --network host \
        -e NODE_TYPE=gateway_subscriber \
        -e NODE_NAME=observer \
        -e NODE_HOST=192.168.1.13 \
        -e COOKIE=macula_beam_cluster \
        -e REGISTRY_ENDPOINT=https://192.168.1.10:9443 \
        -e GATEWAY_PORT=9443 \
        -e SUBSCRIBE_TOPIC=beam.cluster.events \
        $IMAGE_NAME"

    log_info "Observer gateway started on beam03"
}

show_logs() {
    log_info "Showing logs from all nodes..."
    echo ""

    echo "=== beam00 (registry) ==="
    ssh_cmd "$BEAM00" "docker logs --tail 30 $CONTAINER_REGISTRY 2>&1" || echo "No container"
    echo ""

    echo "=== beam01 (subscriber) ==="
    ssh_cmd "$BEAM01" "docker logs --tail 30 $CONTAINER_SUBSCRIBER 2>&1" || echo "No container"
    echo ""

    echo "=== beam02 (publisher) ==="
    ssh_cmd "$BEAM02" "docker logs --tail 30 $CONTAINER_PUBLISHER 2>&1" || echo "No container"
    echo ""

    echo "=== beam03 (observer) ==="
    ssh_cmd "$BEAM03" "docker logs --tail 30 $CONTAINER_OBSERVER 2>&1" || echo "No container"
}

follow_logs() {
    local node=$1
    local container=$2
    local host=""

    case $node in
        beam00) host=$BEAM00; container=$CONTAINER_REGISTRY ;;
        beam01) host=$BEAM01; container=$CONTAINER_SUBSCRIBER ;;
        beam02) host=$BEAM02; container=$CONTAINER_PUBLISHER ;;
        beam03) host=$BEAM03; container=$CONTAINER_OBSERVER ;;
        *) log_error "Unknown node: $node"; exit 1 ;;
    esac

    log_info "Following logs on $node..."
    ssh_cmd "$host" "docker logs -f $container"
}

stop_all() {
    log_info "Stopping all containers..."

    ssh_cmd "$BEAM00" "docker rm -f $CONTAINER_REGISTRY 2>/dev/null" || true
    ssh_cmd "$BEAM01" "docker rm -f $CONTAINER_SUBSCRIBER 2>/dev/null" || true
    ssh_cmd "$BEAM02" "docker rm -f $CONTAINER_PUBLISHER 2>/dev/null" || true
    ssh_cmd "$BEAM03" "docker rm -f $CONTAINER_OBSERVER 2>/dev/null" || true

    log_info "All containers stopped"
}

check_status() {
    log_info "Checking container status..."
    echo ""

    for node_info in "beam00:$BEAM00:$CONTAINER_REGISTRY" \
                     "beam01:$BEAM01:$CONTAINER_SUBSCRIBER" \
                     "beam02:$BEAM02:$CONTAINER_PUBLISHER" \
                     "beam03:$BEAM03:$CONTAINER_OBSERVER"; do
        IFS=':' read -r name host container <<< "$node_info"
        status=$(ssh_cmd "$host" "docker ps --filter name=$container --format '{{.Status}}'" 2>/dev/null || echo "unreachable")
        if [ -n "$status" ] && [ "$status" != "unreachable" ]; then
            echo -e "$name: ${GREEN}$status${NC}"
        else
            echo -e "$name: ${RED}not running${NC}"
        fi
    done
}

check_connectivity() {
    log_info "Checking QUIC connectivity between nodes..."
    echo ""

    # Check from beam01 to beam00 (registry)
    log_info "beam01 -> beam00 (registry):"
    ssh_cmd "$BEAM01" "timeout 3 nc -zvu 192.168.1.10 9443 2>&1" || echo "  Port check failed"

    # Check from beam02 to beam00 (registry)
    log_info "beam02 -> beam00 (registry):"
    ssh_cmd "$BEAM02" "timeout 3 nc -zvu 192.168.1.10 9443 2>&1" || echo "  Port check failed"

    # Check from beam02 to beam01 (cross-gateway)
    log_info "beam02 -> beam01 (cross-gateway):"
    ssh_cmd "$BEAM02" "timeout 3 nc -zvu 192.168.1.11 9443 2>&1" || echo "  Port check failed"
}

# Main command handler
case "${1:-help}" in
    build)
        build_image
        push_image
        ;;
    deploy)
        deploy_registry
        sleep 5  # Wait for registry to start
        deploy_subscriber
        sleep 3
        deploy_publisher
        sleep 3
        deploy_observer
        log_info "Deployment complete!"
        log_info "Wait 30 seconds for all nodes to connect, then run: $0 logs"
        ;;
    logs)
        if [ -n "$2" ]; then
            follow_logs "$2"
        else
            show_logs
        fi
        ;;
    stop)
        stop_all
        ;;
    status)
        check_status
        ;;
    connectivity)
        check_connectivity
        ;;
    full)
        build_image
        push_image
        stop_all
        deploy_registry
        sleep 5
        deploy_subscriber
        sleep 3
        deploy_publisher
        sleep 3
        deploy_observer
        log_info "Full deployment complete!"
        log_info "Wait 30 seconds, then run: $0 logs"
        ;;
    *)
        echo "Macula Beam Cluster Deployment Script"
        echo ""
        echo "Usage: $0 <command>"
        echo ""
        echo "Commands:"
        echo "  build        Build Docker image and push to all beam nodes"
        echo "  deploy       Deploy containers to beam cluster"
        echo "  logs         Show logs from all nodes (or: logs beam01)"
        echo "  stop         Stop all containers"
        echo "  status       Check container status on all nodes"
        echo "  connectivity Check QUIC port connectivity between nodes"
        echo "  full         Full deployment: build, push, stop, deploy"
        echo ""
        echo "Topology:"
        echo "  beam00 (192.168.1.10) - Registry/Seed node"
        echo "  beam01 (192.168.1.11) - Subscriber gateway"
        echo "  beam02 (192.168.1.12) - Publisher gateway"
        echo "  beam03 (192.168.1.13) - Observer gateway"
        echo ""
        echo "Expected flow:"
        echo "  Publisher (beam02) -> Registry (beam00) -> DHT lookup"
        echo "                                         -> Subscriber (beam01)"
        echo "                                         -> Observer (beam03)"
        ;;
esac
