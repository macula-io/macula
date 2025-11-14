# Macula Examples

This directory contains user-facing examples demonstrating how to build applications using the Macula platform.

## Available Examples

### chat_demo/
**Interactive peer-to-peer chat demonstration**

Two clients chatting through firewalls without a central server in the data path. Shows:
- HTTP/3 (QUIC) connectivity through NAT/firewalls
- Real-time pub/sub messaging
- Realm-based multi-tenancy
- Simple Macula client API

See [`chat_demo/README.md`](./chat_demo/README.md) for setup and usage instructions.

### iot_sensors_demo/
**IoT sensor network with real-time dashboard**

Multiple IoT sensors (behind NAT/firewalls) publishing environmental data to a central dashboard. Shows:
- Many-to-one pub/sub pattern
- Sensors behind home routers working seamlessly
- Real-time time-series data streaming
- Topic-based routing
- Smart home / industrial IoT patterns

See [`iot_sensors_demo/README.md`](./iot_sensors_demo/README.md) for setup and usage instructions.

## Other Demos

For technical integration demos and mesh platform testing, see the **`/demo/`** directory at the project root.
