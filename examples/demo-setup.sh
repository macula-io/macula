#!/usr/bin/env bash
set -e

# Macula Decentralized Chat Demo Setup
# This sets up everything needed for the two-client chat demo

echo "=== Macula Decentralized Chat Demo Setup ==="
echo

# 1. Check if gateway is running
echo "Step 1: Checking Macula Gateway..."
if kubectl --context kind-macula-hub get pod -n macula-system -l app=macula-gateway | grep -q Running; then
    echo "✓ Gateway is running in Kubernetes"
else
    echo "✗ Gateway is not running!"
    echo "  Please deploy the gateway first"
    exit 1
fi

# 2. Set up port forwarding
echo
echo "Step 2: Setting up port forwarding..."
pkill -f "port-forward.*macula-gateway" 2>/dev/null || true
kubectl --context kind-macula-hub port-forward -n macula-system svc/macula-gateway 9443:9443 &
PF_PID=$!
echo "✓ Port forwarding started (PID: $PF_PID)"
echo "  Gateway accessible at https://localhost:9443"

# Wait for port forward to be ready
sleep 2

# 3. Test connectivity
echo
echo "Step 3: Testing connectivity..."
if kubectl --context kind-macula-hub run test-connectivity \
    --image=curlimages/curl:latest \
    --rm -i --restart=Never -n macula-system \
    -- curl -s --max-time 5 http://macula-gateway:8080/health 2>/dev/null | grep -q "ok"; then
    echo "✓ Gateway health check passed"
else
    echo "⚠ Gateway health check failed (may still work)"
fi

# 4. Compile the demo
echo
echo "Step 4: Compiling Macula..."
cd "$(dirname "$0")/.."
if rebar3 compile > /dev/null 2>&1; then
    echo "✓ Macula compiled successfully"
else
    echo "✗ Compilation failed"
    kill $PF_PID 2>/dev/null || true
    exit 1
fi

# 5. Create run scripts
echo
echo "Step 5: Creating client run scripts..."

cat > examples/run-alice.sh <<'EOF'
#!/usr/bin/env bash
cd "$(dirname "$0")/.."
echo "Starting Alice's chat client..."
echo "Connecting to gateway at https://localhost:9443"
echo
erl -pa _build/default/lib/*/ebin \
    -eval "
    application:ensure_all_started(ssl),
    io:format(\"Alice's Client Ready!~n\"),
    io:format(\"Type messages to chat with Bob~n~n\"),
    shell:start_interactive()
    " \
    -noshell
EOF

cat > examples/run-bob.sh <<'EOF'
#!/usr/bin/env bash
cd "$(dirname "$0")/.."
echo "Starting Bob's chat client..."
echo "Connecting to gateway at https://localhost:9443"
echo
erl -pa _build/default/lib/*/ebin \
    -eval "
    application:ensure_all_started(ssl),
    io:format(\"Bob's Client Ready!~n\"),
    io:format(\"Type messages to chat with Alice~n~n\"),
    shell:start_interactive()
    " \
    -noshell
EOF

chmod +x examples/run-alice.sh examples/run-bob.sh

echo "✓ Client scripts created:"
echo "  - examples/run-alice.sh"
echo "  - examples/run-bob.sh"

# 6. Print instructions
echo
echo "=== Setup Complete! ==="
echo
echo "To run the demo:"
echo
echo "Terminal 1 (Alice):"
echo "  cd examples && ./run-alice.sh"
echo
echo "Terminal 2 (Bob):"
echo "  cd examples && ./run-bob.sh"
echo
echo "In each terminal, you can run:"
echo "  1> {ok, C} = macula_connection:start_link(<<\"https://localhost:9443\">>, #{realm => <<\"com.example.chat\">>})."
echo "  2> macula_connection:subscribe(C, <<\"chat.messages\">>, fun(Msg) -> io:format(\"Message: ~p~n\", [Msg]) end)."
echo "  3> macula_connection:publish(C, <<\"chat.messages\">>, #{msg => <<\"Hello!\">>})."
echo
echo "Gateway logs:"
echo "  kubectl --context kind-macula-hub logs -n macula-system -l app=macula-gateway -f"
echo
echo "To stop port forwarding:"
echo "  kill $PF_PID"
echo
