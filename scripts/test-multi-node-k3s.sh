#!/bin/bash
# Multi-node DHT test on beam cluster (k3s)
# Deploys 3 Macula pods to beam00.lab k3s cluster

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "=== Multi-Node DHT Test (beam00.lab k3s cluster) ==="
echo ""

# Check if we can reach beam00.lab
if ! ping -c 1 beam00.lab &> /dev/null; then
    echo "ERROR: Cannot reach beam00.lab"
    echo "Make sure you're on the local network and beam00.lab is accessible"
    exit 1
fi

# Kubeconfig for beam00
KUBECONFIG="$HOME/.kube/beam-clusters/beam00.yaml"
if [ ! -f "$KUBECONFIG" ]; then
    echo "ERROR: Kubeconfig not found at $KUBECONFIG"
    exit 1
fi

echo "Using kubeconfig: $KUBECONFIG"
echo ""

# Build Docker image with cache-bust
echo "Building Macula Docker image..."
CACHE_BUST=$(date +%s)
cd "$PROJECT_ROOT"

cat > Dockerfile.test <<'EOF'
FROM erlang:26-alpine

RUN apk add --no-cache git rebar3 make gcc musl-dev openssl-dev

WORKDIR /app
COPY . .
RUN rebar3 compile

EXPOSE 9443/udp

ENTRYPOINT ["erl"]
CMD ["-pa", "_build/default/lib/*/ebin", "-noshell"]
EOF

docker build --build-arg CACHE_BUST="$CACHE_BUST" -t macula-test:latest -f Dockerfile.test .

# Tag for local registry
docker tag macula-test:latest registry.macula.local:5000/macula-test:latest

# Push to registry
echo "Pushing to registry.macula.local:5000..."
docker push registry.macula.local:5000/macula-test:latest

# Create Kubernetes manifests
cat > /tmp/macula-test-deployment.yaml <<'EOF'
apiVersion: v1
kind: Namespace
metadata:
  name: macula-test
---
apiVersion: v1
kind: Pod
metadata:
  name: macula-node1
  namespace: macula-test
  labels:
    app: macula-test
    role: provider
spec:
  containers:
  - name: macula
    image: registry.macula.local:5000/macula-test:latest
    imagePullPolicy: Always
    ports:
    - containerPort: 9443
      protocol: UDP
    command: ["erl"]
    args:
      - "-pa"
      - "_build/default/lib/*/ebin"
      - "-name"
      - "macula_node1@macula-node1.macula-test.svc.cluster.local"
      - "-setcookie"
      - "macula_k3s_test"
      - "-noshell"
      - "-eval"
      - |
        application:ensure_all_started(macula),
        io:format("Node 1 started on k3s~n"),
        {ok, Client} = macula_client:connect(<<"https://0.0.0.0:9443">>, #{
          realm => <<"com.test.k3s">>,
          node_id => <<"k3s-node-1">>
        }),
        Handler = fun(Args) ->
          io:format("[K3S Node 1] Call received: ~p~n", [Args]),
          #{a := A, b := B} = Args,
          {ok, #{result => A + B, node => <<"k3s-node-1">>}}
        end,
        macula_client:advertise(Client, <<"math.add">>, Handler, #{ttl => 60}),
        io:format("Service advertised on k3s node 1~n"),
        receive stop -> ok end.
---
apiVersion: v1
kind: Pod
metadata:
  name: macula-node2
  namespace: macula-test
  labels:
    app: macula-test
    role: consumer
spec:
  containers:
  - name: macula
    image: registry.macula.local:5000/macula-test:latest
    imagePullPolicy: Always
    command: ["sh", "-c"]
    args:
      - |
        sleep 10 &&
        erl -pa _build/default/lib/*/ebin \
            -name macula_node2@macula-node2.macula-test.svc.cluster.local \
            -setcookie macula_k3s_test \
            -noshell \
            -eval '
              application:ensure_all_started(macula),
              io:format("Node 2 (consumer) started on k3s~n"),
              {ok, Client} = macula_client:connect(<<"https://0.0.0.0:9443">>, #{
                realm => <<"com.test.k3s">>,
                node_id => <<"k3s-node-2">>
              }),
              timer:sleep(5000),
              io:format("Calling math.add via DHT...~n"),
              case macula_client:call(Client, <<"math.add">>, #{a => 100, b => 23}) of
                {ok, Result} -> io:format("SUCCESS from k3s: ~p~n", [Result]);
                {error, Reason} -> io:format("ERROR: ~p~n", [Reason])
              end,
              receive stop -> ok end.
            '
---
apiVersion: v1
kind: Pod
metadata:
  name: macula-node3
  namespace: macula-test
  labels:
    app: macula-test
    role: provider
spec:
  containers:
  - name: macula
    image: registry.macula.local:5000/macula-test:latest
    imagePullPolicy: Always
    command: ["sh", "-c"]
    args:
      - |
        sleep 10 &&
        erl -pa _build/default/lib/*/ebin \
            -name macula_node3@macula-node3.macula-test.svc.cluster.local \
            -setcookie macula_k3s_test \
            -noshell \
            -eval '
              application:ensure_all_started(macula),
              io:format("Node 3 (second provider) started on k3s~n"),
              {ok, Client} = macula_client:connect(<<"https://0.0.0.0:9443">>, #{
                realm => <<"com.test.k3s">>,
                node_id => <<"k3s-node-3">>
              }),
              Handler = fun(Args) ->
                io:format("[K3S Node 3] Call received: ~p~n", [Args]),
                #{a := A, b := B} = Args,
                {ok, #{result => A + B, node => <<"k3s-node-3">>}}
              end,
              macula_client:advertise(Client, <<"math.add">>, Handler, #{ttl => 60}),
              io:format("Service advertised on k3s node 3~n"),
              receive stop -> ok end.
            '
EOF

echo "Applying Kubernetes manifests..."
kubectl --kubeconfig="$KUBECONFIG" apply -f /tmp/macula-test-deployment.yaml

echo ""
echo "Waiting for pods to start..."
kubectl --kubeconfig="$KUBECONFIG" wait --for=condition=Ready pod/macula-node1 -n macula-test --timeout=60s || true
kubectl --kubeconfig="$KUBECONFIG" wait --for=condition=Ready pod/macula-node2 -n macula-test --timeout=60s || true
kubectl --kubeconfig="$KUBECONFIG" wait --for=condition=Ready pod/macula-node3 -n macula-test --timeout=60s || true

echo ""
echo "=== Pod Status ==="
kubectl --kubeconfig="$KUBECONFIG" get pods -n macula-test

echo ""
echo "=== Logs from Node 1 (Provider) ==="
kubectl --kubeconfig="$KUBECONFIG" logs macula-node1 -n macula-test || echo "No logs yet"

echo ""
echo "=== Logs from Node 2 (Consumer) ==="
kubectl --kubeconfig="$KUBECONFIG" logs macula-node2 -n macula-test || echo "No logs yet"

echo ""
echo "=== Logs from Node 3 (Second Provider) ==="
kubectl --kubeconfig="$KUBECONFIG" logs macula-node3 -n macula-test || echo "No logs yet"

echo ""
echo "Test pods deployed to beam00.lab k3s cluster"
echo ""
echo "To view logs:"
echo "  kubectl --kubeconfig=$KUBECONFIG logs -f macula-node1 -n macula-test"
echo "  kubectl --kubeconfig=$KUBECONFIG logs -f macula-node2 -n macula-test"
echo "  kubectl --kubeconfig=$KUBECONFIG logs -f macula-node3 -n macula-test"
echo ""
echo "To cleanup:"
echo "  kubectl --kubeconfig=$KUBECONFIG delete namespace macula-test"
echo ""

# Cleanup temp files
rm -f "$PROJECT_ROOT/Dockerfile.test"
rm -f /tmp/macula-test-deployment.yaml
