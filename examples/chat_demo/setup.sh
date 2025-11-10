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
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"
if rebar3 compile > /dev/null 2>&1; then
    echo "✓ Macula compiled successfully"
else
    echo "✗ Compilation failed"
    kill $PF_PID 2>/dev/null || true
    exit 1
fi

# 5. Create run scripts in chat_demo directory
echo
echo "Step 5: Creating client run scripts..."

cat > "$SCRIPT_DIR/run-alice.sh" <<'EOF'
#!/usr/bin/env bash
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"
echo "Starting Alice's interactive shell..."
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

cat > "$SCRIPT_DIR/run-bob.sh" <<'EOF'
#!/usr/bin/env bash
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"
echo "Starting Bob's interactive shell..."
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

chmod +x "$SCRIPT_DIR/run-alice.sh" "$SCRIPT_DIR/run-bob.sh" "$SCRIPT_DIR/chat_demo.erl"

echo "✓ Client scripts created:"
echo "  - $SCRIPT_DIR/run-alice.sh"
echo "  - $SCRIPT_DIR/run-bob.sh"
echo "  - $SCRIPT_DIR/chat_demo.erl"

# 6. Print instructions
echo
echo "=== Setup Complete! ==="
echo
echo "To run the demo:"
echo
echo "OPTION 1: Automated Chat (Easiest)"
echo "  Terminal 1: cd examples/chat_demo && ./chat_demo.erl alice"
echo "  Terminal 2: cd examples/chat_demo && ./chat_demo.erl bob"
echo "  Then type messages in either terminal!"
echo
echo "OPTION 2: Interactive Shell (Advanced)"
echo "  Terminal 1: cd examples/chat_demo && ./run-alice.sh"
echo "  Terminal 2: cd examples/chat_demo && ./run-bob.sh"
echo
echo "  In each terminal, run:"
echo "    1> {ok, C} = macula_connection:start_link(<<\"https://localhost:9443\">>, #{realm => <<\"com.example.chat\">>})."
echo "    2> macula_connection:subscribe(C, <<\"chat.messages\">>, fun(Msg) -> io:format(\"Message: ~p~n\", [Msg]) end)."
echo "    3> macula_connection:publish(C, <<\"chat.messages\">>, #{msg => <<\"Hello!\">>})."
echo
echo "Gateway logs:"
echo "  kubectl --context kind-macula-hub logs -n macula-system -l app=macula-gateway -f"
echo
echo "To stop port forwarding:"
echo "  kill $PF_PID"
echo
