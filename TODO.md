# Macula TODO List

This document tracks known limitations and planned improvements.

## v0.9.0 - Security & Production Hardening

### TLS Certificate Verification
**Status**: Deferred to v0.9.0
**Modules Affected**:
- `macula_connection.erl:115` - QUIC connection TLS verification
- `macula_connection_pool.erl:88` - Connection pool TLS verification
- `macula_peer_connector.erl:82` - Peer connector certificate verification

**Current Behavior**: `{verify, none}` - accepts any certificate
**Planned Fix**: Implement proper certificate validation with CA bundle
**Priority**: High (security)

### DHT Request/Response Pattern
**Status**: Deferred to v0.9.0
**Module**: `macula_gateway_dht.erl:various`

**Current Behavior**: Fire-and-forget DHT operations
**Planned Fix**: Implement request/response with timeout and correlation IDs
**Priority**: Medium (observability)

### DHT Routing Table Bucket Management
**Status**: Deferred to v0.9.0
**Module**: `macula_routing_table.erl:various`

**Current Behavior**: Buckets don't split when full
**Limitation**: Documented in code - buckets have max capacity
**Planned Fix**: Implement k-bucket splitting algorithm
**Priority**: Low (scalability improvement)

### Remote DHT Node Queries
**Status**: Documented limitation
**Module**: `macula_routing_server.erl:various`

**Current Behavior**: Cannot directly query remote nodes without connection
**Workaround**: Use gateway relay for remote queries
**Planned Fix**: Implement direct peer-to-peer DHT queries
**Priority**: Medium (performance)

---

## v0.8.1 - Quality of Life Improvements

### PubSub Helper Functions
**Module**: `macula_pubsub_qos.erl`, `macula_pubsub_subscription.erl`

**Status**: Stub TODOs to remove
**Action**: Add internal helper functions or remove placeholder comments
**Priority**: Low (cleanup)

### RPC Handler Advertisement Manager
**Module**: `macula_rpc_handler.erl`

**Status**: Phase 6 notes from old refactoring
**Action**: Remove obsolete Phase 6 comments
**Priority**: Low (cleanup)

---

## Version History

- **v0.8.0**: Direct P2P with DHT propagation, RPC and PubSub via peer_connector
- **v0.7.x**: Gateway relay architecture, nomenclature refactoring
- **v0.6.x**: Initial DHT implementation

---

## Contributing

When adding TODOs to code:
1. Add entry to this document
2. Link to GitHub issue if exists
3. Specify target version
4. Include priority level
