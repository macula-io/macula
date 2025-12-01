# Macula PubSub Subsystem

This directory contains all modules related to the publish/subscribe messaging subsystem.

## Overview

The PubSub subsystem provides:
- Topic-based publish/subscribe messaging
- DHT-based subscriber discovery
- Multiple QoS levels (0, 1, 2)
- Message retention support
- Wildcard topic matching
- Delivery metrics and monitoring

## Modules

| Module | Purpose |
|--------|---------|
| `macula_pubsub_handler.erl` | Main pub/sub handler - manages subscriptions and publishes |
| `macula_pubsub_server.erl` | Gen_server for pub/sub operations |
| `macula_pubsub_dht.erl` | DHT integration for subscriber discovery |
| `macula_pubsub_discovery.erl` | Subscriber endpoint discovery |
| `macula_pubsub_delivery.erl` | Message delivery with metrics tracking |
| `macula_pubsub_routing.erl` | Multi-hop message routing |
| `macula_pubsub_subscription.erl` | Subscription management |
| `macula_pubsub_topic.erl` | Topic validation and wildcard matching |
| `macula_pubsub_qos.erl` | Quality of service implementation |
| `macula_pubsub_cache.erl` | Subscriber cache for performance |
| `macula_pubsub_registry.erl` | Local subscription registry |

## Architecture

```
┌─────────────────────────────────────────────────┐
│              macula_pubsub_handler              │
│                 (gen_server)                    │
└──────────────────────┬──────────────────────────┘
                       │
       ┌───────────────┼───────────────┐
       │               │               │
┌──────▼──────┐ ┌──────▼──────┐ ┌──────▼──────┐
│ pubsub_dht  │ │ pubsub_cache │ │pubsub_topic │
│ (discovery) │ │   (LRU)     │ │ (matching)  │
└─────────────┘ └─────────────┘ └─────────────┘
       │
       ▼
┌─────────────┐
│   DHT/      │
│ routing_    │
│  server     │
└─────────────┘
```

## Message Flow

### Subscribe
```
1. Client sends SUBSCRIBE(topic)
2. Handler registers local subscription
3. Advertisement stored in DHT: topic → {node_id, endpoint}
4. DHT propagates to k=20 closest nodes
```

### Publish
```
1. Publisher sends PUBLISH(topic, payload)
2. Handler queries DHT for subscribers
3. For each subscriber:
   - Direct QUIC connection (via peer_connector)
   - Send pubsub_route message
4. Track delivery metrics
```

## QoS Levels

| Level | Description |
|-------|-------------|
| 0 | At most once (fire-and-forget) |
| 1 | At least once (with ACK) |
| 2 | Exactly once (four-way handshake) |

## Topic Wildcards

| Pattern | Matches |
|---------|---------|
| `+` | Single level: `sensor/+/temp` matches `sensor/1/temp` |
| `#` | Multi-level: `sensor/#` matches `sensor/1/temp/celsius` |

## Delivery Metrics

The delivery module tracks:
- Messages sent/received
- Delivery success/failure rates
- Latency percentiles
- Retry counts

Access metrics via:
```erlang
macula_pubsub_delivery:get_metrics().
```

## Configuration

| Key | Default | Description |
|-----|---------|-------------|
| `pubsub_cache_size` | 10000 | Max cached subscribers |
| `pubsub_cache_ttl` | 300 | Cache TTL in seconds |
| `default_qos` | 0 | Default QoS level |

## Tests

See: `test/macula_pubsub_system/`

Run pubsub tests:
```bash
rebar3 eunit --module=macula_pubsub_handler_tests
rebar3 eunit --module=macula_pubsub_delivery_tests
rebar3 eunit --module=macula_pubsub_topic_tests
```
