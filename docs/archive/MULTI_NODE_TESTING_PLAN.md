# Multi-Node Testing Plan

**Date**: 2025-01-10
**Objective**: Verify multi-endpoint RPC, provider selection, and failover across real distributed nodes

---

## Testing Environments

We have 3 testing environments available:

### 1. Docker Compose (Quickest - Local)
- **Purpose**: Quick verification of multi-endpoint connectivity
- **Setup Time**: 5 minutes
- **Nodes**: 3 Erlang nodes in Docker containers
- **Network**: Docker bridge network
- **Use Case**: Verify basic multi-endpoint RPC, connection caching, provider selection

### 2. KinD Clusters (Kubernetes - Local)
- **Purpose**: Kubernetes-native testing with GitOps
- **Setup Time**: 15 minutes
- **Nodes**: 4 KinD clusters (macula-hub + 3 Belgian regions)
- **Network**: Docker networks with port mapping
- **Use Case**: Verify GitOps deployment, service mesh behavior, Kubernetes integration

### 3. beam**.lab Nodes (Real Hardware - Distributed)
- **Purpose**: Production-like testing on real hardware
- **Setup Time**: 30 minutes
- **Nodes**: 4 physical Intel Celeron mini PCs (beam00-03)
- **Network**: LAN (192.168.1.10-13)
- **Use Case**: Real-world performance, network latency, hardware failover

---

## Test Scenarios

### Scenario 1: Basic Multi-Endpoint RPC
**Goal**: Verify calls route to different provider endpoints

**Steps**:
1. Deploy 3 nodes (node1, node2, node3)
2. Each node advertises service `test.calculator`
3. Client connects to node1
4. Client calls `test.calculator` 6 times
5. Verify calls distributed across all 3 nodes

**Expected Results**:
- Round-robin: 2 calls to each node
- Random: All calls succeed (distribution varies)
- Logs show connections to all 3 endpoints

### Scenario 2: Automatic Failover
**Goal**: Verify failover to alternate providers on timeout

**Steps**:
1. Deploy 3 nodes advertising `test.calculator`
2. Client configures `max_attempts => 3`
3. Stop node1 (kill process)
4. Client calls `test.calculator`
5. Verify call succeeds via node2 or node3

**Expected Results**:
- Attempt 1: Timeout on node1 (connection_failed or timeout)
- Attempt 2: Success on node2
- Total latency < 35 seconds (with 30s timeout)

### Scenario 3: Connection Caching
**Goal**: Verify connections are reused

**Steps**:
1. Deploy 2 nodes advertising `test.service`
2. Client calls service 10 times
3. Monitor connection creation logs

**Expected Results**:
- First call to each node: Log "Creating connection to endpoint"
- Subsequent calls: No connection creation logs
- Connection count stays at 2 (not 10)

### Scenario 4: Provider Selection Strategies
**Goal**: Verify all 3 strategies work correctly

**Steps**:
1. Deploy 3 nodes advertising `test.service`
2. Test `first` strategy: All calls go to node1
3. Test `random` strategy: Calls distributed randomly
4. Test `round_robin` strategy: Even distribution (3-3-3 for 9 calls)

**Expected Results**:
- First: Logs show only node1 called
- Random: All nodes called (distribution varies)
- Round-robin: Exactly 3 calls to each node

---

## Environment 1: Docker Compose Setup

### Files to Create

**`docker/docker-compose.multi-node.yml`**:
```yaml
version: '3.8'

services:
  # Node 1 - Provider
  macula-node1:
    image: erlang:26
    container_name: macula-node1
    hostname: node1.macula.local
    volumes:
      - ../:/macula
    working_dir: /macula
    command: >
      bash -c "
        rebar3 compile &&
        erl -pa _build/default/lib/*/ebin \
            -name node1@node1.macula.local \
            -setcookie macula_test \
            -eval '
              application:ensure_all_started(macula),
              {ok, Client} = macula_client:start_link(<<\"https://0.0.0.0:9001\">>, #{realm => <<\"test.realm\">>}),
              Handler = fun(Args) ->
                io:format(\"[NODE1] Handling call: ~p~n\", [Args]),
                {ok, #{node => <<\"node1\">>, result => maps:get(<<\"x\">>, Args, 0) * 2}}
              end,
              macula_client:advertise(Client, <<\"test.calculator\">>, Handler),
              io:format(\"Node1 ready at https://node1.macula.local:9001~n\"),
              timer:sleep(infinity)
            '
      "
    ports:
      - "9001:9001"
    networks:
      macula_test:
        ipv4_address: 172.20.0.2

  # Node 2 - Provider
  macula-node2:
    image: erlang:26
    container_name: macula-node2
    hostname: node2.macula.local
    volumes:
      - ../:/macula
    working_dir: /macula
    command: >
      bash -c "
        rebar3 compile &&
        erl -pa _build/default/lib/*/ebin \
            -name node2@node2.macula.local \
            -setcookie macula_test \
            -eval '
              application:ensure_all_started(macula),
              {ok, Client} = macula_client:start_link(<<\"https://0.0.0.0:9002\">>, #{realm => <<\"test.realm\">>}),
              Handler = fun(Args) ->
                io:format(\"[NODE2] Handling call: ~p~n\", [Args]),
                {ok, #{node => <<\"node2\">>, result => maps:get(<<\"x\">>, Args, 0) * 3}}
              end,
              macula_client:advertise(Client, <<\"test.calculator\">>, Handler),
              io:format(\"Node2 ready at https://node2.macula.local:9002~n\"),
              timer:sleep(infinity)
            '
      "
    ports:
      - "9002:9002"
    networks:
      macula_test:
        ipv4_address: 172.20.0.3

  # Node 3 - Provider
  macula-node3:
    image: erlang:26
    container_name: macula-node3
    hostname: node3.macula.local
    volumes:
      - ../:/macula
    working_dir: /macula
    command: >
      bash -c "
        rebar3 compile &&
        erl -pa _build/default/lib/*/ebin \
            -name node3@node3.macula.local \
            -setcookie macula_test \
            -eval '
              application:ensure_all_started(macula),
              {ok, Client} = macula_client:start_link(<<\"https://0.0.0.0:9003\">>, #{realm => <<\"test.realm\">>}),
              Handler = fun(Args) ->
                io:format(\"[NODE3] Handling call: ~p~n\", [Args]),
                {ok, #{node => <<\"node3\">>, result => maps:get(<<\"x\">>, Args, 0) * 4}}
              end,
              macula_client:advertise(Client, <<\"test.calculator\">>, Handler),
              io:format(\"Node3 ready at https://node3.macula.local:9003~n\"),
              timer:sleep(infinity)
            '
      "
    ports:
      - "9003:9003"
    networks:
      macula_test:
        ipv4_address: 172.20.0.4

  # Client - Calls services
  macula-client:
    image: erlang:26
    container_name: macula-client
    hostname: client.macula.local
    volumes:
      - ../:/macula
    working_dir: /macula
    command: >
      bash -c "
        sleep 10 &&
        rebar3 compile &&
        erl -pa _build/default/lib/*/ebin \
            -name client@client.macula.local \
            -setcookie macula_test \
            -eval '
              application:ensure_all_started(macula),
              {ok, Client} = macula_client:start_link(<<\"https://node1.macula.local:9001\">>, #{
                realm => <<\"test.realm\">>,
                provider_selection_strategy => round_robin
              }),
              timer:sleep(5000),
              io:format(\"Client ready, calling test.calculator 6 times...~n\"),
              lists:foreach(fun(I) ->
                {ok, Result} = macula_client:call(Client, <<\"test.calculator\">>, #{<<\"x\">> => I}),
                io:format(\"Call ~p: ~p~n\", [I, Result]),
                timer:sleep(1000)
              end, lists:seq(1, 6)),
              io:format(\"All calls complete!~n\"),
              timer:sleep(infinity)
            '
      "
    networks:
      macula_test:
        ipv4_address: 172.20.0.10
    depends_on:
      - macula-node1
      - macula-node2
      - macula-node3

networks:
  macula_test:
    driver: bridge
    ipam:
      config:
        - subnet: 172.20.0.0/24
```

### Usage

```bash
cd /home/rl/work/github.com/macula-io/macula

# Start all nodes
docker-compose -f docker/docker-compose.multi-node.yml up

# Watch logs
docker-compose -f docker/docker-compose.multi-node.yml logs -f macula-client

# Stop and clean up
docker-compose -f docker/docker-compose.multi-node.yml down
```

---

## Environment 2: KinD Clusters Setup

### Approach

Use the existing GitOps repository (`cortex-iq-deploy`) with modifications for testing Macula RPC.

### Steps

1. **Build Macula Docker Image**:
```bash
cd /home/rl/work/github.com/macula-io/macula

# Create Dockerfile
cat > Dockerfile <<'EOF'
FROM erlang:26-alpine

WORKDIR /app

# Install build dependencies
RUN apk add --no-cache git rebar3

# Copy source code
COPY . /app/

# Compile
RUN rebar3 compile

# Default command (override in K8s)
CMD ["erl", "-pa", "_build/default/lib/*/ebin", "-eval", "application:ensure_all_started(macula), timer:sleep(infinity)"]
EOF

# Build image
docker build -t registry.macula.local:5000/macula/macula-node:test --build-arg CACHE_BUST=$(date +%s) .

# Push to local registry
docker push registry.macula.local:5000/macula/macula-node:test
```

2. **Create Test Manifests**:

Add to `cortex-iq-deploy/base/macula-test-node/`:

```yaml
# base/macula-test-node/deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: macula-test-node
spec:
  replicas: 1
  selector:
    matchLabels:
      app: macula-test-node
  template:
    metadata:
      labels:
        app: macula-test-node
    spec:
      containers:
      - name: macula-node
        image: registry.macula.local:5000/macula/macula-node:test
        ports:
        - containerPort: 9443
          name: quic
        env:
        - name: NODE_NAME
          valueFrom:
            fieldRef:
              fieldPath: metadata.name
        - name: POD_IP
          valueFrom:
            fieldRef:
              fieldPath: status.podIP
        command:
        - erl
        - -pa
        - _build/default/lib/*/ebin
        - -name
        - macula@$(NODE_NAME)
        - -setcookie
        - macula_test
        - -eval
        - |
          application:ensure_all_started(macula),
          {ok, Client} = macula_client:start_link(<<"https://0.0.0.0:9443">>, #{realm => <<"test.realm">>}),
          Handler = fun(Args) ->
            io:format("[~p] Handling call: ~p~n", [node(), Args]),
            {ok, #{node => atom_to_binary(node()), result => maps:get(<<"x">>, Args, 0) * 2}}
          end,
          macula_client:advertise(Client, <<"test.calculator">>, Handler),
          timer:sleep(infinity).
---
apiVersion: v1
kind: Service
metadata:
  name: macula-test-node
spec:
  selector:
    app: macula-test-node
  ports:
  - port: 9443
    targetPort: 9443
    name: quic
```

3. **Deploy to Clusters**:

```bash
cd /home/rl/work/github.com/macula-io/cortex-iq-deploy

# Add kustomization to each cluster
# clusters/be-antwerp/apps/macula-test/kustomization.yaml
# clusters/be-brabant/apps/macula-test/kustomization.yaml
# clusters/be-east-flanders/apps/macula-test/kustomization.yaml

# Commit and push
git add .
git commit -m "Add Macula RPC test nodes"
git push

# Flux will auto-deploy to all clusters
```

---

## Environment 3: beam**.lab Physical Nodes

### SSH Access

```bash
# Access nodes
ssh rl@beam00.lab  # 192.168.1.10
ssh rl@beam01.lab  # 192.168.1.11
ssh rl@beam02.lab  # 192.168.1.12
ssh rl@beam03.lab  # 192.168.1.13

# Password: rl
```

### Deployment Approach

**Option A: Docker on each node** (Simplest):
```bash
# On each beam node
ssh rl@beam00.lab "
  cd /bulk0/macula-test &&
  docker run -d --name macula-node1 \
    -p 9443:9443 \
    -v /bulk0/macula:/macula \
    erlang:26 \
    bash -c 'cd /macula && rebar3 compile && erl...'
"
```

**Option B: K3s deployment** (Production-like):
```bash
# Use GitOps repository
cd /home/rl/work/github.com/macula-io/cortex-iq-deploy

# Create beam-cluster overlays
# clusters/beam00/apps/macula-test/...
# clusters/beam01/apps/macula-test/...
# etc.

# Deploy via GitOps
git add . && git commit -m "Deploy Macula RPC test to beam clusters" && git push
```

### Test Script for beam** Nodes

**`scripts/test-beam-cluster-rpc.sh`**:
```bash
#!/bin/bash

# Deploy and test Macula RPC on beam**.lab nodes

set -e

NODES=("beam00.lab" "beam01.lab" "beam02.lab")

echo "==> Building Macula on all nodes..."
for NODE in "${NODES[@]}"; do
  echo "Building on $NODE..."
  ssh rl@$NODE "
    cd /bulk0/macula &&
    git pull &&
    rebar3 compile
  " &
done
wait

echo "==> Starting Macula nodes..."
for i in "${!NODES[@]}"; do
  NODE="${NODES[$i]}"
  PORT=$((9001 + i))

  echo "Starting node$((i+1)) on $NODE:$PORT..."

  ssh rl@$NODE "
    pkill -f 'macula_test_node' || true
    cd /bulk0/macula &&
    nohup erl -pa _build/default/lib/*/ebin \
      -name node$((i+1))@$NODE \
      -setcookie macula_test \
      -eval '
        application:ensure_all_started(macula),
        {ok, Client} = macula_client:start_link(<<\"https://0.0.0.0:$PORT\">>, #{realm => <<\"test.realm\">>}),
        Handler = fun(Args) ->
          io:format(\"[NODE$((i+1))] Handling call: ~p~n\", [Args]),
          {ok, #{node => <<\"node$((i+1))\">>, result => maps:get(<<\"x\">>, Args, 0) * $((i+2))}}
        end,
        macula_client:advertise(Client, <<\"test.calculator\">>, Handler),
        io:format(\"Node$((i+1)) ready at https://$NODE:$PORT~n\"),
        timer:sleep(infinity)
      ' > /tmp/macula_node$((i+1)).log 2>&1 &

    echo \$! > /tmp/macula_node$((i+1)).pid
  " &
done
wait

echo "==> All nodes started!"
echo ""
echo "Node URLs:"
for i in "${!NODES[@]}"; do
  echo "  node$((i+1)): https://${NODES[$i]}:$((9001 + i))"
done
echo ""
echo "==> Running client test on beam00..."

ssh rl@beam00.lab "
  cd /bulk0/macula &&
  erl -pa _build/default/lib/*/ebin \
    -name client@beam00.lab \
    -setcookie macula_test \
    -eval '
      application:ensure_all_started(macula),
      {ok, Client} = macula_client:start_link(<<\"https://beam00.lab:9001\">>, #{
        realm => <<\"test.realm\">>,
        provider_selection_strategy => round_robin
      }),
      timer:sleep(5000),
      io:format(\"Client ready, calling test.calculator 6 times...~n\"),
      lists:foreach(fun(I) ->
        {ok, Result} = macula_client:call(Client, <<\"test.calculator\">>, #{<<\"x\">> => I}),
        io:format(\"Call ~p: ~p~n\", [I, Result]),
        timer:sleep(1000)
      end, lists:seq(1, 6)),
      io:format(\"All calls complete!~n\"),
      init:stop()
    ' -noshell
"

echo "==> Test complete!"
```

---

## Test Execution Plan

### Phase 1: Docker Compose (30 minutes)
1. Create docker-compose.multi-node.yml
2. Start 3 nodes + client
3. Verify all 4 scenarios
4. Collect logs and metrics

### Phase 2: KinD Clusters (1 hour)
1. Build Macula Docker image
2. Create K8s manifests
3. Deploy via GitOps
4. Verify cross-cluster RPC
5. Test Kubernetes-specific features

### Phase 3: beam**.lab Physical Hardware (1 hour)
1. Deploy to beam00-02
2. Run test script
3. Verify real network latency
4. Measure actual failover time
5. Stress test with load

---

## Success Criteria

✅ **Multi-Endpoint RPC**:
- Calls route to different provider endpoints
- Logs show "Creating connection to endpoint" for each provider

✅ **Connection Caching**:
- Subsequent calls reuse cached connections
- No redundant connection creation

✅ **Provider Selection**:
- `first`: All calls to node1
- `random`: All nodes receive calls
- `round_robin`: Even distribution (6 calls → 2-2-2)

✅ **Automatic Failover**:
- Node1 down → Call succeeds via node2/node3
- Max 2 timeouts before success
- Total time < 65 seconds (2 × 30s timeout + call)

✅ **Performance**:
- First call latency: < 500ms (includes connection setup)
- Cached call latency: < 100ms
- Failover latency: < 35 seconds per attempt

---

## Next Steps

1. **Create Docker Compose setup** (quickest validation)
2. **Build and test locally**
3. **Deploy to KinD** (if Docker succeeds)
4. **Deploy to beam** (final production-like test)
5. **Document results and metrics**

---

**Ready to proceed with Docker Compose setup!**
