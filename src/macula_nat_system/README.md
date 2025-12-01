# NAT System

The NAT System handles Network Address Translation detection and traversal, enabling direct peer-to-peer connections across NAT boundaries.

## Module Table

| Module | Purpose | LOC |
|--------|---------|-----|
| `macula_nat_system` | Supervisor for NAT subsystem | 4.4k |
| `macula_nat_detector` | Detects local NAT type (Full Cone, Restricted, Symmetric) | 32k |
| `macula_nat_coordinator` | Coordinates hole-punch timing between peers | 28k |
| `macula_nat_cache` | Caches NAT detection results with TTL | 18k |
| `macula_nat_connector` | NAT-aware connection establishment | 17k |
| `macula_hole_punch` | Performs UDP/QUIC hole punching | 16k |
| `macula_port_predictor` | Predicts symmetric NAT port allocations | 23k |
| `macula_connection_upgrade` | Upgrades relay connections to direct | 9.5k |
| `macula_relay_node` | Peer relay functionality for unreachable peers | 19k |
| `macula_relay_registry` | Tracks available relay nodes with load balancing | 13k |

## NAT Type Detection Flow

```
1. Client starts macula_nat_detector
2. Detector sends NAT_PROBE to multiple observers (bootstrap nodes)
3. Each observer replies with reflexive address (public IP:port seen)
4. Detector compares:
   - Same IP:port from all observers → Full Cone NAT
   - Same IP, different ports → Address Restricted or Symmetric
   - Different IP:port pairs → Symmetric NAT
5. Result cached in macula_nat_cache (TTL: 5 minutes)
```

## Hole Punching Sequence

```
Peer A (behind NAT)              Coordinator              Peer B (behind NAT)
       |                              |                         |
       |--- PUNCH_REQUEST(B) -------->|                         |
       |                              |<---- PUNCH_REQUEST(A) --|
       |                              |                         |
       |<-- PUNCH_COORDINATE ---------|---- PUNCH_COORDINATE -->|
       |   (B's reflexive addr,       |   (A's reflexive addr,  |
       |    punch_at: T+100ms)        |    punch_at: T+100ms)   |
       |                              |                         |
       |============ At time T+100ms, both peers punch ==========|
       |                                                         |
       |<========== Direct QUIC connection established =========>|
```

### Adaptive Timing by NAT Type

| NAT Type | Initial Delay | Retry Interval | Max Retries |
|----------|---------------|----------------|-------------|
| Full Cone | 50ms | 100ms | 3 |
| Restricted | 100ms | 200ms | 5 |
| Symmetric | 150ms | 300ms | 7 |

## Configuration Options

Environment variables for runtime configuration:

| Variable | Default | Description |
|----------|---------|-------------|
| `MACULA_NAT_CACHE_TTL` | 300 | Cache TTL in seconds |
| `MACULA_NAT_CACHE_MAX` | 10000 | Maximum cache entries |
| `MACULA_NAT_PROBE_TIMEOUT` | 5000 | Probe timeout in ms |
| `MACULA_RELAY_AUTO_ENABLE` | false | Auto-enable relay on capable nodes |
| `RELAY_ENABLED` | false | Enable relay functionality |

### Programmatic Configuration

```erlang
%% Start NAT system with custom options
macula_nat_system:start_link(#{
    cache_max_entries => 5000,
    cache_ttl_seconds => 600,
    detection_timeout_ms => 3000
}).

%% Detect local NAT type
{ok, NATType} = macula_nat_detector:detect().
%% Returns: full_cone | address_restricted | port_restricted | symmetric

%% Request hole punch to peer
{ok, SessionId} = macula_hole_punch:start_punch(TargetNodeId).

%% Enable relay capability
macula_relay_node:enable(#{
    node_id => NodeId,
    endpoint => {<<"192.168.1.100">>, 4433},
    capacity => 100
}).
```

## Message Types (0x50-0x5F)

| Type | ID | Purpose |
|------|-----|---------|
| NAT_PROBE | 0x50 | Request reflexive address from observer |
| NAT_PROBE_REPLY | 0x51 | Return reflexive address to requester |
| PUNCH_REQUEST | 0x52 | Request hole punch coordination |
| PUNCH_COORDINATE | 0x53 | Synchronized punch timing info |
| PUNCH_RESULT | 0x55 | Report punch success/failure |
| RELAY_REQUEST | 0x56 | Request relay tunnel setup |
| RELAY_DATA | 0x57 | Relayed data frame |

## Supervision Tree

```
macula_nat_system (supervisor)
├── nat_cache (macula_nat_cache)
├── nat_detector (macula_nat_detector)
├── nat_coordinator (macula_nat_coordinator)
├── hole_punch (macula_hole_punch)
├── connection_upgrade (macula_connection_upgrade)
├── port_predictor (macula_port_predictor)
├── relay_registry (macula_relay_registry)
└── relay_node (macula_relay_node)
```

## Performance Metrics

| Metric | Target | Description |
|--------|--------|-------------|
| NAT detection time | < 2s | Time to detect local NAT type |
| Hole punch success rate | > 80% | Direct connection establishment |
| Connection upgrade time | < 5s | Relay to direct upgrade |
| Cache hit rate | > 95% | NAT cache effectiveness |

## Related Documentation

- [NAT Types Explained](../../docs/guides/NAT_TYPES_EXPLAINED.md)
- [NAT Traversal Developer Guide](../../docs/guides/NAT_TRAVERSAL_DEVELOPER_GUIDE.md)
- [v0.12.0 NAT Complete Plan](../../architecture/V0.12.0_NAT_COMPLETE_PLAN.md)
