# RPC Reply Flow Diagram

## Multi-Hop RPC Message Flow

This diagram shows the complete request/response flow for a multi-hop RPC call through the DHT mesh.

```
┌────────────────────────────────────────────────────────────────────────────┐
│                         RPC CALL Flow (Request)                             │
└────────────────────────────────────────────────────────────────────────────┘

    Client                Registry              NodeA                 Provider
      │                      │                    │                       │
      │ 1. RPC Call          │                    │                       │
      │ procedure="test"     │                    │                       │
      │ args={x: 1}          │                    │                       │
      │                      │                    │                       │
      │ 2. Wrap in           │                    │                       │
      │ rpc_route envelope   │                    │                       │
      │ dest=provider        │                    │                       │
      │ source=client        │                    │                       │
      │ hop_count=0          │                    │                       │
      │                      │                    │                       │
      │──rpc_route(CALL)────>│                    │                       │
      │                      │                    │                       │
      │                      │ 3. Query DHT       │                       │
      │                      │ find_closest(      │                       │
      │                      │   provider, k=3)   │                       │
      │                      │                    │                       │
      │                      │ 4. Forward to      │                       │
      │                      │ next hop           │                       │
      │                      │ hop_count=1        │                       │
      │                      │                    │                       │
      │                      │──rpc_route(CALL)──>│                       │
      │                      │                    │                       │
      │                      │                    │ 5. Query DHT          │
      │                      │                    │ find_closest(         │
      │                      │                    │   provider, k=3)      │
      │                      │                    │                       │
      │                      │                    │ 6. Forward to         │
      │                      │                    │ provider              │
      │                      │                    │ hop_count=2           │
      │                      │                    │                       │
      │                      │                    │──rpc_route(CALL)─────>│
      │                      │                    │                       │
      │                      │                    │                       │ 7. Deliver locally
      │                      │                    │                       │ dest==local_node_id
      │                      │                    │                       │
      │                      │                    │                       │ 8. Extract CALL
      │                      │                    │                       │ payload
      │                      │                    │                       │
      │                      │                    │                       │ 9. Execute handler
      │                      │                    │                       │ result = x * 2


┌────────────────────────────────────────────────────────────────────────────┐
│                        RPC REPLY Flow (Response)                            │
└────────────────────────────────────────────────────────────────────────────┘

    Provider              NodeA                Registry              Client
      │                    │                       │                    │
      │ 10. Create REPLY   │                       │                    │
      │ call_id=<same>     │                       │                    │
      │ result={result:2}  │                       │                    │
      │                    │                       │                    │
      │ 11. Wrap in        │                       │                    │
      │ rpc_route envelope │                       │                    │
      │ dest=client        │                       │                    │
      │ source=provider    │                       │                    │
      │ hop_count=0        │                       │                    │
      │                    │                       │                    │
      │──rpc_route(REPLY)─>│                       │                    │
      │                    │                       │                    │
      │                    │ 12. Query DHT         │                    │
      │                    │ find_closest(         │                    │
      │                    │   client, k=3)        │                    │
      │                    │                       │                    │
      │                    │ 13. Forward to        │                    │
      │                    │ registry              │                    │
      │                    │ hop_count=1           │                    │
      │                    │                       │                    │
      │                    │──rpc_route(REPLY)────>│                    │
      │                    │                       │                    │
      │                    │                       │ 14. Query DHT      │
      │                    │                       │ find_closest(      │
      │                    │                       │   client, k=3)     │
      │                    │                       │                    │
      │                    │                       │ 15. Forward to     │
      │                    │                       │ client gateway     │
      │                    │                       │ hop_count=2        │
      │                    │                       │                    │
      │                    │                       │──rpc_route(REPLY)─>│
      │                    │                       │                    │
      │                    │                       │                    │ 16. Deliver locally
      │                    │                       │                    │ dest==local_node_id
      │                    │                       │                    │
      │                    │                       │                    │ 17. Gateway forwards
      │                    │                       │                    │ to local connection
      │                    │                       │                    │ via gproc lookup
      │                    │                       │                    │
      │                    │                       │     ┌───────────┐  │
      │                    │                       │     │Connection │  │
      │                    │                       │ 18. │Process    │<─┤
      │                    │                       │     │           │  │ gen_server:cast
      │                    │                       │     │           │  │
      │                    │                       │     │ 19. Match │  │
      │                    │                       │     │ call_id   │  │
      │                    │                       │     │ with      │  │
      │                    │                       │     │ pending_  │  │
      │                    │                       │     │ calls     │  │
      │                    │                       │     │           │  │
      │                    │                       │     │ 20. Reply │  │
      │                    │                       │     │ to caller │  │
      │                    │                       │     └───────────┘  │
```

## Key Components

### 1. RPC Route Envelope

```erlang
#{
  <<"destination_node_id">> => <<NodeId>>,    % Target node
  <<"source_node_id">> => <<NodeId>>,          % Origin node
  <<"hop_count">> => Integer,                  % Current hop count
  <<"max_hops">> => 10,                        % Max allowed hops
  <<"payload_type">> => <<"call">> | <<"reply">>,
  <<"payload">> => Map                         % CALL or REPLY message
}
```

### 2. CALL Payload

```erlang
#{
  call_id => <<UUID>>,                % Unique call identifier
  procedure => <<"test.calculator">>, % RPC procedure name
  args => <<JSON>>                    % Procedure arguments
}
```

### 3. REPLY Payload

```erlang
#{
  <<"call_id">> => <<UUID>>,          % Matches CALL call_id
  <<"result">> => <<JSON>> | undefined,
  <<"error">> => Binary | undefined
}
```

## Gateway REPLY Forwarding Logic

```
┌─────────────────────────────────────────────────────────────┐
│  Gateway receives rpc_route with payload_type=<<"reply">>   │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
         ┌─────────────────────────────┐
         │ destination_node_id ==      │
         │ local_node_id?              │
         └──────┬─────────────┬────────┘
                │             │
           YES  │             │  NO
                │             │
                ▼             ▼
     ┌──────────────┐  ┌────────────────┐
     │ Local        │  │ Remote Client  │
     │ Connection   │  │                │
     └──────┬───────┘  └────────┬───────┘
            │                   │
            ▼                   ▼
   ┌────────────────┐   ┌──────────────┐
   │ gproc:lookup_  │   │ Get stream   │
   │ pids({p, l,    │   │ from         │
   │ macula_        │   │ client_      │
   │ connection})   │   │ streams map  │
   └────────┬───────┘   └──────┬───────┘
            │                  │
            ▼                  ▼
   ┌────────────────┐   ┌──────────────┐
   │ gen_server:    │   │ macula_quic: │
   │ cast(Pid,      │   │ send(Stream, │
   │ {rpc_route_    │   │ RpcRoute)    │
   │ reply, Msg})   │   │              │
   └────────────────┘   └──────────────┘
```

## Connection REPLY Processing

```
┌───────────────────────────────────────────────────────┐
│  Connection receives {rpc_route_reply, RpcRouteMsg}   │
└──────────────────────┬────────────────────────────────┘
                       │
                       ▼
          ┌────────────────────────┐
          │ Extract payload_type   │
          │ and payload            │
          └────────────┬───────────┘
                       │
                       ▼
          ┌────────────────────────┐
          │ payload_type ==        │
          │ <<"reply">>?           │
          └────────┬───────────────┘
                   │ YES
                   ▼
          ┌────────────────────────┐
          │ Extract call_id        │
          │ (binary key!)          │
          └────────────┬───────────┘
                       │
                       ▼
          ┌────────────────────────┐
          │ Lookup in              │
          │ State#state.           │
          │ pending_calls          │
          └────────┬───────────────┘
                   │
         ┌─────────┴─────────┐
         │                   │
    Found│              Not  │Found
         │                   │
         ▼                   ▼
┌────────────────┐   ┌──────────────┐
│ Cancel timer   │   │ Log warning  │
│ Extract result │   │ (unknown     │
│ or error       │   │ call_id)     │
│                │   └──────────────┘
│ gen_server:    │
│ reply(From,    │
│ {ok, Result})  │
│                │
│ Remove from    │
│ pending_calls  │
└────────────────┘
```

## Performance Characteristics

### Latency Breakdown

```
Total RTT = DHT_lookup1 + Forward_hops1 + Handler + DHT_lookup2 + Forward_hops2

Where:
  DHT_lookup1  = 10-50ms   (client → provider discovery)
  Forward_hops1 = 5-20ms/hop (CALL routing)
  Handler      = 1-10ms    (provider execution)
  DHT_lookup2  = 10-50ms   (provider → client discovery)
  Forward_hops2 = 5-20ms/hop (REPLY routing)

Example (2 hops each direction):
  10ms + (2×15ms) + 5ms + 10ms + (2×15ms) = 85ms RTT
```

### Message Size

```
RPC Route Envelope:
  destination_node_id: 32 bytes
  source_node_id:      32 bytes
  hop_count:           ~1 byte
  max_hops:            ~1 byte
  payload_type:        ~10 bytes
  payload:             Variable
  MessagePack framing: ~20 bytes
  ────────────────────────────
  Total overhead:      ~96 bytes

Typical CALL payload:    50-500 bytes
Typical REPLY payload:   50-500 bytes
```

## Error Handling

```
┌──────────────────────────┐
│ Error Conditions         │
└──────────────────────────┘

1. Max Hops Exceeded
   hop_count >= max_hops
   → {error, max_hops_exceeded}

2. No Route to Destination
   find_closest() returns []
   → {error, no_route}

3. Timeout (Client Side)
   No REPLY within timeout
   → {error, timeout}

4. Handler Error (Provider)
   Handler crashes or returns error
   → REPLY with {error, ...}

5. Unknown call_id
   REPLY doesn't match pending call
   → Log warning, drop message
```

## Binary Key Convention

**IMPORTANT:** All MessagePack-decoded messages use binary keys!

```erlang
% ❌ WRONG - Atom keys
#{call_id := CallId} = Msg,
maps:get(result, Msg)

% ✅ CORRECT - Binary keys
#{<<"call_id">> := CallId} = Msg,
maps:get(<<"result">>, Msg)
```

This applies to:
- `rpc_route` envelope fields
- `CALL` message fields
- `REPLY` message fields
- All other MessagePack messages

## Testing

### Test Topology

```
Registry (172.22.0.2)
    │
    ├─→ NodeA (172.22.0.3)
    │      │
    │      └─→ NodeB (172.22.0.4)
    │             │
    │             └─→ Provider (172.22.0.5)
    │
    └─→ Client (172.22.0.10)
```

### Test Flow

```
Client: Call test.calculator(x=1)
  ↓
Registry: Route via DHT
  ↓
NodeA: Forward
  ↓
NodeB: Forward
  ↓
Provider: Execute (result = 1 * 2 = 2)
  ↓
Provider: Send REPLY via DHT
  ↓
NodeA: Forward REPLY
  ↓
Registry: Forward REPLY
  ↓
Client Gateway: Deliver via gproc
  ↓
Client Connection: Match call_id, reply to caller
  ↓
Client: Receives {ok, #{result => 2}}
```

### Success Criteria

- ✅ All 6 RPC calls complete successfully
- ✅ No timeouts or errors
- ✅ Correct routing through multi-hop mesh
- ✅ Proper hop counting (2-3 hops each direction)
- ✅ REPLY messages delivered to correct clients

## Related Documentation

- `architecture/dht_rpc_reply_implementation.md` - Complete implementation guide
- `CHANGELOG_RPC_REPLY.md` - Quick reference for changes
- `architecture/dht_routed_rpc.md` - Original design document
