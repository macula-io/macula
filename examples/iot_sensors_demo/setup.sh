#!/usr/bin/env bash
set -e

# Macula IoT Sensors Demo Setup
# Sets up everything needed for the IoT sensors demo

echo "=== Macula IoT Sensors Demo Setup ==="
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

# 4. Compile Macula
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

# 5. Make scripts executable
echo
echo "Step 5: Making scripts executable..."
chmod +x "$SCRIPT_DIR/sensor.erl" "$SCRIPT_DIR/dashboard.erl"
echo "✓ Scripts are executable"

# 6. Print instructions
echo
echo "=== Setup Complete! ==="
echo
echo "To run the demo:"
echo
echo "Terminal 1 (Dashboard):"
echo "  cd examples/iot_sensors_demo && ./dashboard.erl"
echo
echo "Terminal 2 (Sensor 1):"
echo "  cd examples/iot_sensors_demo && ./sensor.erl sensor-01 \"Living Room\""
echo
echo "Terminal 3 (Sensor 2):"
echo "  cd examples/iot_sensors_demo && ./sensor.erl sensor-02 \"Bedroom\""
echo
echo "Terminal 4 (Sensor 3):"
echo "  cd examples/iot_sensors_demo && ./sensor.erl sensor-03 \"Kitchen\""
echo
echo "You'll see real-time sensor data flowing to the dashboard!"
echo
echo "Gateway logs:"
echo "  kubectl --context kind-macula-hub logs -n macula-system -l app=macula-gateway -f"
echo
echo "To stop port forwarding:"
echo "  kill $PF_PID"
echo
