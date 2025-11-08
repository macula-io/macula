# Macula HTTP/3 Mesh - Wire Protocol Specification

**Complete wire protocol documentation for interoperability**

**Status**: 🚧 SKELETON - Needs completion
**Priority**: P1
**Estimated effort**: 8 hours

---

## Overview

⚠️ **TODO**: Add overview of the Macula wire protocol, its goals, and design principles.

**Key points to cover**:
- Protocol layering (QUIC → Framing → Messages)
- Design goals (efficiency, extensibility, compatibility)
- Version negotiation strategy

---

## Packet Format and Framing

### Packet Structure

⚠️ **TODO**: Define the complete packet structure.

**Outline**:
```
[Version (1 byte)][Type (1 byte)][Flags (2 bytes)][Length (4 bytes)][Payload (N bytes)]
```

**Details needed**:
- Endianness (network byte order)
- Maximum packet size
- Fragmentation strategy for large messages
- Checksums/CRC

### Frame Types

⚠️ **TODO**: Document all frame types with hex codes.

**Placeholder list**:
- `0x01` - HANDSHAKE
- `0x02` - HANDSHAKE_ACK
- `0x03` - HEARTBEAT
- `0x04` - PING
- `0x05` - PONG
- `0x10` - PUBLISH
- `0x11` - SUBSCRIBE
- `0x12` - UNSUBSCRIBE
- `0x13` - EVENT
- `0x20` - RPC_CALL
- `0x21` - RPC_RESULT
- `0x22` - RPC_ERROR
- `0xF0` - ERROR
- `0xFF` - CLOSE

---

## Message Types

### HANDSHAKE (0x01)

⚠️ **TODO**: Complete HANDSHAKE message specification.

**Payload structure**:
```erlang
#{
  version => integer(),
  node_id => binary(),
  realm => binary(),
  capabilities => map(),
  % ... more fields
}
```

### PUBLISH (0x10)

⚠️ **TODO**: Define PUBLISH message format.

### SUBSCRIBE (0x11)

⚠️ **TODO**: Define SUBSCRIBE message format.

### RPC_CALL (0x20)

⚠️ **TODO**: Define RPC_CALL message format.

---

## Encoding/Decoding Rules

⚠️ **TODO**: Specify encoding format (Erlang term format, MessagePack, etc.).

**Sections needed**:
- Binary encoding scheme
- String encoding (UTF-8)
- Map/array encoding
- Performance considerations

---

## Protocol Flow Diagrams

### Connection Establishment

⚠️ **TODO**: Add complete connection flow diagram.

```
Client                         Server
  │                              │
  ├──── QUIC Connection ────────→│
  │                              │
  ├──── HANDSHAKE ──────────────→│
  │                              │
  │←──── HANDSHAKE_ACK ──────────┤
  │                              │
  │        (Connected)           │
```

### Pub/Sub Flow

⚠️ **TODO**: Add pub/sub message flow.

### RPC Flow

⚠️ **TODO**: Add RPC call/response flow.

---

## Error Handling

⚠️ **TODO**: Define error codes and handling.

**Error codes to define**:
- Protocol errors
- Version mismatch
- Authentication failures
- Rate limiting
- Resource exhaustion

---

## Protocol Versioning

⚠️ **TODO**: Define versioning strategy and compatibility matrix.

---

## Security Considerations

⚠️ **TODO**: Document TLS integration, encryption, authentication.

---

## Performance Characteristics

⚠️ **TODO**: Add benchmarks and overhead analysis.

---

## Implementation Notes

⚠️ **TODO**: Tips for implementers, edge cases, gotchas.

---

**Last Updated**: 2025-01-08
**Contributors**: [Add names as sections are completed]
