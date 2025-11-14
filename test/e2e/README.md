# End-to-End (E2E) Test Suite

This directory contains automated end-to-end tests for the Macula HTTP/3 mesh network. These tests exercise real multi-node scenarios with actual QUIC connections, DHT operations, and message routing.

## Overview

The E2E test suite validates macula_connection functionality through real-world scenarios:
- **Pub/Sub**: Message publishing and subscription across nodes
- **RPC**: Service discovery, remote procedure calls, and failover
- **DHT**: Service advertisement and discovery
- **Multi-node**: Real network communication between containers

## Test Scripts

### `run-pubsub-test.sh`
Tests publish/subscribe message flow across 4 nodes:
- 1 registry node (DHT coordinator)
- 1 publisher node (publishes messages every 3 seconds)
- 2 subscriber nodes (receive messages)

**Validates:**
- Messages are published successfully
- Subscribers receive all published messages
- DHT-based subscription advertisement works
- Peer-to-peer message routing works

**Duration:** ~2-3 minutes

### `run-pubsub-wildcard-test.sh`
Tests wildcard topic matching across 5 nodes:
- 1 registry node (DHT coordinator)
- 1 publisher node (publishes to 5 different topics)
- 3 subscriber nodes with different patterns:
  - `subscriber1`: Exact match (`sensor.temp.room1`)
  - `subscriber2`: Single-level wildcard (`sensor.*.room1`)
  - `subscriber3`: Multi-level wildcard (`sensor.**`)

**Validates:**
- Exact topic matching works correctly
- Single-level wildcard (`*`) matches one segment
- Multi-level wildcard (`**`) matches multiple segments
- Messages only route to matching subscribers
- Wildcard pattern filtering at mesh level

**Test Scenarios:**
1. `sensor.temp.room1` → all 3 subscribers (exact + both wildcards match)
2. `sensor.humidity.room1` → subscribers 2 & 3 only (wildcards match)
3. `sensor.temp.room2` → subscriber 3 only (multi-level wildcard matches)
4. `other.topic` → no subscribers (no patterns match)
5. `sensor.temp.room1.extra` → subscriber 3 only (multi-level wildcard matches)

**Duration:** ~2-3 minutes

### `run-rpc-test.sh`
Tests RPC with service discovery across 5 nodes:
- 1 registry node (DHT coordinator)
- 3 provider nodes (advertise `test.calculator` service)
- 1 client node (makes 6 RPC calls)

**Validates:**
- Services are advertised to DHT
- Service discovery finds all providers
- All RPC calls succeed
- Round-robin distribution works across providers

**Duration:** ~2-3 minutes

### `run-all-e2e-tests.sh`
Master test runner that executes all E2E tests sequentially:
1. Basic pub/sub message flow
2. Wildcard pub/sub pattern matching
3. RPC with service discovery

**Duration:** ~7-9 minutes

## Usage

### Prerequisites
- Docker installed and running
- Docker Compose available
- Sufficient disk space for building images

### Running Tests

**Run all E2E tests:**
```bash
./test/e2e/run-all-e2e-tests.sh
```

**Run individual tests:**
```bash
./test/e2e/run-pubsub-test.sh
./test/e2e/run-rpc-test.sh
```

### From project root:
```bash
make e2e-test  # If Makefile target exists
# or
bash test/e2e/run-all-e2e-tests.sh
```

## Test Output

### Success Output
```
✓ TEST PASSED
  - Publisher successfully published 11 messages
  - Subscriber1 received 11 messages
  - Subscriber2 received 11 messages
  - Message flow working across nodes!
```

### Failure Output
```
✗ TEST FAILED
  - No messages were published
  - Check logs at: /tmp/pubsub-test-logs.txt
```

## Logs

Test logs are saved to `/tmp/` for debugging:
- `/tmp/pubsub-test-logs.txt` - Pub/sub test output
- `/tmp/rpc-test-logs.txt` - RPC test output

**View logs:**
```bash
cat /tmp/pubsub-test-logs.txt
cat /tmp/rpc-test-logs.txt
```

## Coverage Impact

These E2E tests exercise significant portions of `macula_connection.erl`:

**Code paths exercised:**
- Connection lifecycle (start_link, init, connect)
- QUIC connection establishment
- Message framing and encoding
- Pub/sub operations (publish, subscribe, message routing)
- RPC operations (call, service discovery, provider selection)
- DHT operations (advertise, query, store)
- Error handling and retries

**Expected coverage increase:** 10-15% on macula_connection

**Measure coverage:**
```bash
# Run E2E tests
./test/e2e/run-all-e2e-tests.sh

# Then run unit tests with coverage
rebar3 eunit
rebar3 cover --verbose | grep macula_connection
```

## Architecture

### Docker Compose Files
- `docker/docker-compose.pubsub-test.yml` - Pub/sub environment
- `docker/docker-compose.multi-node-test.yml` - RPC environment

### Test Flow
1. **Build**: Fresh Docker image with latest code
2. **Start**: Launch multi-node environment
3. **Wait**: Allow services to initialize (15-20 seconds)
4. **Monitor**: Collect logs for analysis
5. **Analyze**: Parse logs for success criteria
6. **Report**: Display results and metrics
7. **Cleanup**: Stop and remove containers

## Troubleshooting

### Test Hangs
**Symptom:** Test doesn't complete after 5 minutes

**Solutions:**
- Check Docker daemon is running: `docker info`
- Check available disk space: `df -h`
- Manually stop containers: `docker compose -f docker/docker-compose.pubsub-test.yml down`

### Test Fails Immediately
**Symptom:** "Not all containers started successfully"

**Solutions:**
- Check Docker logs: `docker compose -f docker/docker-compose.pubsub-test.yml logs`
- Rebuild image: `docker compose -f docker/docker-compose.pubsub-test.yml build --no-cache`
- Check port conflicts: `lsof -i :9443`

### Messages Not Received
**Symptom:** Publisher sends but subscribers don't receive

**Debugging:**
```bash
# Check if containers are running
docker ps --filter "name=macula"

# Check registry logs
docker logs macula-registry

# Check subscriber logs
docker logs macula-subscriber1

# Check network connectivity
docker network inspect docker_macula_test
```

### RPC Calls Fail
**Symptom:** "No providers found" errors

**Debugging:**
```bash
# Check provider logs
docker logs macula-provider1

# Check if services are advertised
docker logs macula-registry | grep "STORE"

# Check client logs
docker logs macula-client
```

## CI/CD Integration

### GitHub Actions Example
```yaml
name: E2E Tests

on: [push, pull_request]

jobs:
  e2e:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      - name: Run E2E Tests
        run: |
          chmod +x test/e2e/*.sh
          ./test/e2e/run-all-e2e-tests.sh

      - name: Upload logs on failure
        if: failure()
        uses: actions/upload-artifact@v3
        with:
          name: e2e-logs
          path: /tmp/*-test-logs.txt
```

### GitLab CI Example
```yaml
e2e-tests:
  stage: test
  image: docker:latest
  services:
    - docker:dind
  script:
    - chmod +x test/e2e/*.sh
    - ./test/e2e/run-all-e2e-tests.sh
  artifacts:
    when: on_failure
    paths:
      - /tmp/*-test-logs.txt
```

## Extending Tests

### Adding New E2E Tests

1. **Create test script** in `test/e2e/run-<scenario>-test.sh`
2. **Follow naming convention**: `run-<scenario>-test.sh`
3. **Use existing scripts as templates**
4. **Include cleanup trap**: Ensure containers are stopped on exit
5. **Update master runner**: Add to `run-all-e2e-tests.sh`

### Example Template
```bash
#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cleanup() {
    docker compose -f docker/your-test.yml down -v 2>&1 | grep -v "warn" || true
}
trap cleanup EXIT

cd "$PROJECT_ROOT"

# Your test logic here

exit 0
```

## Performance

### Build Times
- **First build:** 3-5 minutes (compiles all dependencies)
- **Subsequent builds:** 2-3 minutes (cached layers)
- **No-cache build:** 3-5 minutes (ensures fresh code)

### Test Times
- **Pub/Sub test:** 2-3 minutes
- **RPC test:** 2-3 minutes
- **All tests:** 5-6 minutes

### Resource Usage
- **CPU:** 2-4 cores during build
- **Memory:** 2-4 GB for containers
- **Disk:** ~1 GB for images

## Related Documentation

- [Project Structure](../../architecture/MACULA_PROJECT_STRUCTURE.md)
- [Quick Start Guide](../../architecture/macula_http3_mesh_quick_start.md)
- [Code Review Report](../../CODE_REVIEW_REPORT.md)
- [Docker Infrastructure](../../docker/README.md)

## Support

For issues or questions:
1. Check troubleshooting section above
2. Review logs in `/tmp/`
3. Check Docker container status
4. Open an issue on GitHub
