# Macula IoT Sensors Demo

This demo showcases **many-to-one pub/sub messaging** where multiple IoT sensors (behind NAT/firewalls) publish environmental data to a central dashboard.

## What Makes This Special?

### Traditional IoT Architecture:
```
Sensors → Cloud Server → Dashboard
         (centralized,
          single point
          of failure)
```
- ✗ All data goes through cloud server
- ✗ Sensors need public IPs or VPN
- ✗ Cloud server sees all data
- ✗ Privacy concerns

### Macula IoT Architecture:
```
Sensor 1 (Home 1) ─┐
Sensor 2 (Home 1) ─┼─→ Macula Gateway ──→ Dashboard
Sensor 3 (Home 2) ─┘   (bootstrap only)    (Office)
```
- ✓ Gateway only for discovery
- ✓ Sensors behind NAT work seamlessly
- ✓ HTTP/3 (QUIC) penetrates firewalls
- ✓ Topic-based routing
- ✓ Multiple sensors, single dashboard

## Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                    Macula HTTP/3 Mesh                         │
│                                                               │
│  ┌──────────┐   ┌──────────┐   ┌──────────┐                │
│  │ Sensor 1 │   │ Sensor 2 │   │ Sensor 3 │                │
│  │ Living   │   │ Bedroom  │   │ Kitchen  │                │
│  │ Room     │   │          │   │          │                │
│  └────┬─────┘   └────┬─────┘   └────┬─────┘                │
│       │              │              │                        │
│       └──────────────┼──────────────┘                        │
│                      ▼                                        │
│           ┌─────────────────────┐                           │
│           │  Macula Gateway     │                           │
│           │  sensor.reading     │                           │
│           │  topic routing      │                           │
│           └──────────┬──────────┘                           │
│                      │                                        │
│                      ▼                                        │
│              ┌──────────────┐                                │
│              │  Dashboard   │                                │
│              │  (Subscriber)│                                │
│              └──────────────┘                                │
└──────────────────────────────────────────────────────────────┘
```

## What It Demonstrates

✓ **Many-to-One Pub/Sub**: Multiple publishers, single subscriber
✓ **NAT Traversal**: Sensors behind home routers work seamlessly
✓ **Real-Time Streaming**: 2-second interval sensor readings
✓ **Topic-Based Routing**: All sensors publish to `sensor.reading`
✓ **Simple Integration**: < 200 lines of code total

## Sensor Data Format

Each sensor publishes readings with:
- **Temperature**: 15-30°C with realistic drift
- **Humidity**: 30-70% with realistic variations
- **Light**: 100-800 lux (ambient light levels)
- **Timestamp**: Unix timestamp
- **Metadata**: sensor_id, location

## Running the Demo

### Quick Start

```bash
# 1. Setup (one time)
cd /home/rl/work/github.com/macula-io/macula/examples/iot_sensors_demo
./setup.sh

# 2. Terminal 1 - Start Dashboard
./dashboard.erl

# 3. Terminal 2 - Start Sensor 1
./sensor.erl sensor-01 "Living Room"

# 4. Terminal 3 - Start Sensor 2
./sensor.erl sensor-02 "Bedroom"

# 5. Terminal 4 - Start Sensor 3
./sensor.erl sensor-03 "Kitchen"
```

You'll see the dashboard update in real-time as sensors publish data every 2 seconds!

### Example Output

**Dashboard:**
```
╔════════════════════════════════════════════════════════════════╗
║                  LIVE SENSOR DASHBOARD                         ║
╚════════════════════════════════════════════════════════════════╝

Listening for sensor readings... (Press Ctrl+C to exit)

┌──────────────────────────────────────────────────────────────┐
│ sensor-01            @ Living Room
│ Time: 2025-11-10 21:45:32
├──────────────────────────────────────────────────────────────┤
│  🌡️  Temperature: 22.3°C
│  💧  Humidity:    54.2%
│  💡  Light:       456 lux
└──────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────┐
│ sensor-02            @ Bedroom
│ Time: 2025-11-10 21:45:32
├──────────────────────────────────────────────────────────────┤
│  🌡️  Temperature: 20.1°C
│  💧  Humidity:    48.7%
│  💡  Light:       234 lux
└──────────────────────────────────────────────────────────────┘
```

**Sensor:**
```
=== Macula IoT Sensor Demo ===
Sensor: sensor-01
Location: Living Room

[sensor-01] Connected! Publishing to realm: com.example.iot
[sensor-01] Press Ctrl+C to stop

[sensor-01] Published: 22.3°C, 54.2%, 456 lux
[sensor-01] Published: 22.4°C, 54.5%, 462 lux
[sensor-01] Published: 22.2°C, 53.8%, 451 lux
```

## Use Cases

This pattern enables:

- **Smart Home Monitoring**: Temperature, humidity, motion sensors
- **Industrial IoT**: Manufacturing sensor networks
- **Environmental Monitoring**: Air quality, weather stations
- **Building Management**: HVAC, lighting, occupancy sensors
- **Agriculture**: Soil moisture, greenhouse monitoring
- **Data Center Monitoring**: Temperature, humidity, power

## Technical Details

### Topic Design
- **Topic**: `sensor.reading` (event type)
- **Not**: `sensor.01.reading` (entity ID in topic ❌)
- IDs belong in payload, not topics

### Message Format
```erlang
#{
    sensor_id => <<"sensor-01">>,
    location => <<"Living Room">>,
    temperature => <<"22.3">>,
    humidity => <<"54.2">>,
    light => <<"456">>,
    timestamp => 1699650332,
    unit_temp => <<"celsius">>,
    unit_humidity => <<"percent">>,
    unit_light => <<"lux">>
}
```

### Realm
- All sensors and dashboard connect to: `com.example.iot`
- Isolated from other realms
- Multi-tenancy support

## Extending the Demo

### Add More Sensor Types
```erlang
%% In sensor.erl, add to Reading map:
co2 => format_float(maps:get(co2, NewState)),
pressure => format_float(maps:get(pressure, NewState))
```

### Add Alerting
```erlang
%% In dashboard.erl, add threshold checks:
case Temp > 28.0 of
    true -> io:format("🚨 HIGH TEMPERATURE ALERT!~n");
    false -> ok
end
```

### Add Historical Storage
Subscribe and store readings in database for time-series analysis.

### Add Multiple Dashboards
Run multiple dashboard instances - all will receive the same data.

## Architecture Patterns

This demo demonstrates:

1. **Publisher-Subscriber Pattern**: Decoupled sensors and dashboards
2. **Time-Series Data**: Continuous stream of measurements
3. **Telemetry**: Remote monitoring without direct connectivity
4. **Fan-Out Messaging**: One sensor → many dashboards (if needed)

## Troubleshooting

### Gateway not accessible
```bash
kubectl --context kind-macula-hub logs -n macula-system -l app=macula-gateway
```

### Port forwarding failed
```bash
pkill -f "port-forward.*macula-gateway"
./setup.sh
```

### Sensor not publishing
- Check gateway connectivity
- Verify realm name matches
- Check for error messages

## Next Steps

- Add more sensor types (CO2, pressure, motion)
- Implement alerting thresholds
- Store historical data
- Create charts/graphs
- Add authentication
- Deploy on real IoT hardware

## Learn More

- [Macula Architecture](../../architecture/macula_http3_mesh_root.md)
- [Chat Demo](../chat_demo/README.md)
- [HTTP/3 RFC](https://www.rfc-editor.org/rfc/rfc9114.html)
