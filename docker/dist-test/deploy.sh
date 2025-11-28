     STDIN
   1 #!/bin/bash
   2 set -e
   3 
   4 # Macula QUIC Distribution Test - Deploy Script
   5 #
   6 # Builds and starts the 3-node test cluster.
   7 
   8 SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
   9 PROJECT_ROOT="${SCRIPT_DIR}/../.."
  10 
  11 echo "==================================================="
  12 echo "  Macula QUIC Distribution Test - Deploy"
  13 echo "==================================================="
  14 
  15 cd "${PROJECT_ROOT}"
  16 
  17 # Clean up any existing containers
  18 echo "Cleaning up existing containers..."
  19 docker compose -f docker/dist-test/docker-compose.yml down -v 2>/dev/null || true
  20 
  21 # Prune build cache for fresh build
  22 echo "Pruning Docker build cache..."
  23 docker builder prune -af
  24 
  25 # Build the image
  26 echo "Building Docker image..."
  27 docker compose -f docker/dist-test/docker-compose.yml build --no-cache
  28 
  29 # Start the cluster
  30 echo "Starting 3-node cluster..."
  31 docker compose -f docker/dist-test/docker-compose.yml up -d
  32 
  33 echo ""
  34 echo "==================================================="
  35 echo "  Cluster Started"
  36 echo "==================================================="
  37 echo ""
  38 echo "Health endpoints:"
  39 echo "  Node 1: http://localhost:9101/health"
  40 echo "  Node 2: http://localhost:9102/health"
  41 echo "  Node 3: http://localhost:9103/health"
  42 echo ""
  43 echo "View logs:"
  44 echo "  docker compose -f docker/dist-test/docker-compose.yml logs -f"
  45 echo ""
  46 echo "Stop cluster:"
  47 echo "  docker compose -f docker/dist-test/docker-compose.yml down"
  48 echo ""
