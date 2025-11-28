     STDIN
   1 #!/bin/bash
   2 set -e
   3 
   4 # Macula QUIC Distribution Test - Test Script
   5 #
   6 # Verifies that the 3-node cluster is working correctly.
   7 
   8 echo "==================================================="
   9 echo "  Macula QUIC Distribution Test - Verification"
  10 echo "==================================================="
  11 
  12 # Wait for cluster to stabilize
  13 echo "Waiting for cluster to stabilize (20 seconds)..."
  14 sleep 20
  15 
  16 # Check health endpoints
  17 echo ""
  18 echo "Checking health endpoints..."
  19 echo ""
  20 
  21 for port in 9101 9102 9103; do
  22     echo "=== Node on port ${port} ==="
  23     response=$(curl -s "http://localhost:${port}/health" 2>/dev/null || echo "FAILED")
  24     echo "${response}"
  25     echo ""
  26 done
  27 
  28 # Check status endpoints for cluster size
  29 echo "Checking cluster status..."
  30 echo ""
  31 
  32 for port in 9101 9102 9103; do
  33     echo "=== Status from port ${port} ==="
  34     response=$(curl -s "http://localhost:${port}/status" 2>/dev/null || echo "FAILED")
  35     echo "${response}"
  36     echo ""
  37 done
  38 
  39 # Verify cluster size = 3
  40 echo "==================================================="
  41 echo "  Verification Results"
  42 echo "==================================================="
  43 
  44 node1_size=$(curl -s "http://localhost:9101/health" 2>/dev/null | grep -o '"cluster_size":[0-9]*' | grep -o '[0-9]*' || echo "0")
  45 node2_size=$(curl -s "http://localhost:9102/health" 2>/dev/null | grep -o '"cluster_size":[0-9]*' | grep -o '[0-9]*' || echo "0")
  46 node3_size=$(curl -s "http://localhost:9103/health" 2>/dev/null | grep -o '"cluster_size":[0-9]*' | grep -o '[0-9]*' || echo "0")
  47 
  48 echo "Node 1 cluster size: ${node1_size}"
  49 echo "Node 2 cluster size: ${node2_size}"
  50 echo "Node 3 cluster size: ${node3_size}"
  51 echo ""
  52 
  53 if [ "${node1_size}" = "3" ] && [ "${node2_size}" = "3" ] && [ "${node3_size}" = "3" ]; then
  54     echo "SUCCESS: All nodes report cluster size = 3"
  55     echo "QUIC distribution is working correctly!"
  56     exit 0
  57 else
  58     echo "WARNING: Not all nodes have cluster size = 3"
  59     echo "Check the logs for more information:"
  60     echo "  docker compose -f docker/dist-test/docker-compose.yml logs"
  61     exit 1
  62 fi
