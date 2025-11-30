# NAT Traversal Test Environment

Docker-based NAT simulation for testing Macula mesh networking across different NAT types.

## Overview

This test environment simulates real-world NAT conditions using Docker networks with iptables-based NAT routers. It allows testing of:

- Direct P2P connections between peers
- NAT hole punching
- Relay fallback for symmetric NAT
- DHT discovery across NAT boundaries
- Async RPC (NATS-style request/reply)

## NAT Types Simulated

| Type | Difficulty | Docker Network | Characteristics |
|------|------------|----------------|-----------------|
| Full Cone | Easy | `nat_full_cone` (172.31.x.x) | Any external host can send to mapped port |
| Restricted | Moderate | `nat_restricted` (172.32.x.x) | Only hosts peer has contacted can send |
| Symmetric | Hard | `nat_symmetric` (172.33.x.x) | Different mapping per destination (requires relay) |

## Quick Start

### Simple 3-Peer Test (Flat Network)
```bash
# Simple ping-pong test without NAT complexity
docker compose -f docker-compose.simple-pingpong.yml up -d
docker logs -f pingpong-fc01
```

### NAT-Separated Test (3 Peers)
```bash
# Tests with actual NAT routers between networks
docker compose -f docker-compose.yml up -d
docker logs -f nat-test-full-cone
```

### 50-Peer Stress Test
```bash
# Full scale test with 50 peers across all NAT types
./run-50-ping-pong.sh rebuild    # Build and start
./run-50-ping-pong.sh stats      # Show RTT statistics
./run-50-ping-pong.sh logs       # Follow messages
./run-50-ping-pong.sh stop       # Cleanup
```

## Test Environments

### docker-compose.simple-pingpong.yml
- 3 peers on single flat network (172.41.0.0/24)
- No NAT complexity - for basic functionality testing
- Uses PING/PONG demo mode

### docker-compose.yml
- 3 peers behind different NAT types
- Bootstrap on public network (10.100.0.0/24)
- Uses Chatter demo mode (pub/sub)

### docker-compose.50-ping-pong.yml
- 50 peers (17 FC, 17 RC, 16 SY)
- Full NAT simulation with routers
- Uses PING/PONG demo mode
- Bootstrap on 10.101.0.0/24

## Network Architecture

```
                    Public Network (10.10x.0.0/24)
                              │
              ┌───────────────┼───────────────┐
              │               │               │
         FC Router       RC Router       SY Router
         (10.x.0.20)     (10.x.0.21)     (10.x.0.22)
              │               │               │
              │               │               │
    FC Network         RC Network        SY Network
    172.31.x.0/24     172.32.x.0/24     172.33.x.0/24
         │                 │                 │
    FC Peers          RC Peers          SY Peers
```

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `NODE_TYPE` | peer | `bootstrap` or `peer` |
| `NODE_ID` | test-node | Unique node identifier |
| `BOOTSTRAP_HOST` | - | Bootstrap IP address |
| `BOOTSTRAP_PORT` | 4433 | Bootstrap QUIC port |
| `DEMO_MODE` | chatter | `chatter` (pub/sub) or `ping_pong` (RPC) |
| `PING_INTERVAL` | 5000 | Ping interval in ms (ping_pong mode) |
| `PING_TIMEOUT` | 3000 | Ping timeout in ms (ping_pong mode) |

## Troubleshooting

### Peers can't reach bootstrap
The `run-test.sh` script automatically derives the bootstrap network from `BOOTSTRAP_HOST`:
```bash
# Route is added dynamically based on BOOTSTRAP_HOST
BOOTSTRAP_NETWORK=$(echo $BOOTSTRAP_HOST | cut -d. -f1-3).0/24
ip route add $BOOTSTRAP_NETWORK via $ROUTER_IP
```

Check routing with:
```bash
docker exec <container> ip route
docker exec <container> ping <bootstrap-ip>
```

### DHT discovery timeout
- Ensure bootstrap is reachable first
- Check that `connection_manager_pid` is set in RPC handler
- Verify QUIC connections are established

### View detailed logs
```bash
docker logs <container> 2>&1 | grep -E "PING|PONG|DHT|error"
```

## Demo Modes

### Chatter (Pub/Sub)
- Broadcasts messages to `chat.room.global` topic
- All peers subscribe and receive messages
- Tests DHT-based subscriber discovery

### Ping/Pong (Async RPC)
- Each peer picks random target and sends PING
- Target responds with PONG via async RPC
- Tests DHT-based service discovery
- Measures RTT for latency analysis

## Test Results (v0.12.3)

50-peer test results:
- **100% success rate** - All pings received pongs
- **0 timeouts** - No DHT discovery failures
- **Sub-millisecond RTT** - 0-1ms average
- **Cross-NAT working** - FC, RC, SY all communicating

## Files

| File | Description |
|------|-------------|
| `Dockerfile` | Builds Macula test image |
| `run-test.sh` | Container entrypoint script |
| `run-50-ping-pong.sh` | 50-peer test orchestration |
| `run-50-chatters.sh` | 50-peer chatter test |
| `docker-compose.*.yml` | Various test configurations |
