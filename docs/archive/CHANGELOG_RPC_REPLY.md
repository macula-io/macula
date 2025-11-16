# RPC REPLY Routing - Change Log

**Date:** 2025-01-13
**Status:** ✅ Complete
**Test Results:** 6/6 multi-hop RPC calls successful

## Quick Summary

Fixed RPC REPLY delivery in DHT-routed mesh network. All RPC calls now complete successfully with proper REPLY routing through Kademlia DHT.

## Problem

- RPC calls were timing out
- REPLYs not reaching clients
- Gateway was dropping REPLY messages
- Connection handler had binary vs atom key mismatch

## Solution

Implemented bidirectional DHT routing for RPC messages using NATS-inspired reply-to pattern.

## Files Changed

### 1. src/macula_gateway.erl

**Lines 750-806**: Added `handle_rpc_reply_routed/3`
- Handles REPLY delivery to local and remote clients
- Uses gproc for local connection discovery
- Forwards via gen_server:cast

**Lines 1058-1061**: Fixed REPLY dropping
- Changed from dropping to calling `handle_rpc_reply_routed/3`

### 2. src/macula_connection.erl

**Line 254**: Added gproc registration in `init/1`
```erlang
gproc:reg({p, l, macula_connection}),
```

**Lines 580-585**: Added `handle_cast` for REPLY
```erlang
handle_cast({rpc_route_reply, RpcRouteMsg}, State) ->
    NewState = process_message({rpc_route, RpcRouteMsg}, State),
    {noreply, NewState};
```

**Lines 1094-1162**: Fixed binary key handling
- Changed `call_id` → `<<"call_id">>`
- Changed `result` → `<<"result">>`
- Changed `error` → `<<"error">>`

## Code Statistics

- **Files Modified:** 2
- **Lines Added:** ~80
- **Lines Modified:** ~25
- **Total Impact:** ~105 lines

## Test Results

```bash
$ ./test/e2e/run-multi-hop-rpc-test.sh

✓ TEST PASSED
  - All 6 RPC calls succeeded
  - Multi-hop DHT routing working

Routing Metrics:
  RPC route messages received: 24
  RPC route deliveries: 6
```

## Key Insights

1. **MessagePack Always Uses Binary Keys**: All handlers must expect binary keys, never atoms
2. **gproc for Process Discovery**: Clean way to find local processes without storing PIDs
3. **Unified Routing**: Both CALL and REPLY use same rpc_route envelope structure
4. **NATS Pattern**: Reply-to pattern works well in DHT-based P2P networks

## Performance

- **Latency:** 50-200ms RTT depending on hop count
- **Scalability:** O(log N) hops for N nodes
- **Reliability:** Automatic failover to alternative providers

## Next Steps

Potential optimizations (see `architecture/pubsub_optimization_recommendations.md`):

1. **DHT Result Caching** (5-10x improvement)
   - Cache provider lookups with TTL
   - Avoid repeated DHT queries

2. **Direct Routing Table** (3-5x improvement)
   - Maintain routes to recently-used providers
   - Bypass DHT for subsequent calls

3. **Persistent QUIC Streams** (1.5-2x improvement)
   - Connection pooling
   - Reduce connection overhead

## Documentation

See `architecture/dht_rpc_reply_implementation.md` for complete technical documentation.

## Related Issues

This implementation completes the DHT-routed RPC feature as designed in:
- `architecture/dht_routed_rpc.md`
- `architecture/dht_rpc_implementation_status.md`
