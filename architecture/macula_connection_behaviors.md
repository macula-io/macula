# macula_connection.erl Behavior Documentation

**Status:** 📝 Documentation Phase
**Date:** 2025-01-13
**Purpose:** Complete catalog of all behaviors, edge cases, and test scenarios for `macula_connection.erl`
**Module Size:** 2,030 lines
**Related:** `god_module_refactoring_plan.md`, `CODE_REVIEW_REPORT.md`

## Overview

This document systematically catalogs all behaviors implemented in `macula_connection.erl` to serve as the foundation for comprehensive test coverage before refactoring. Each behavior is documented with its implementation details, edge cases, and test scenarios.

---

## 1. Connection Lifecycle Management

### 1.1. Initialization (`init/1`)

**Location:** Line 202

**Behavior:**
- Parses connection URL
- Extracts realm, host, port from URL
- Generates unique node ID
- Initializes service registry with DHT caching
- Initializes provider selector with configured strategy
- Schedules cache pruning timer (every 60 seconds)
- Sends self() a `connect` message to start connection
- Returns `{ok, State}` with status=connecting

**State Initialized:**
```erlang
#state{
    url = Url,
    opts = Opts,
    realm = Realm,
    node_id = NodeId,
    status = connecting,
    subscriptions = #{},
    pending_calls = #{},
    pending_queries = #{},
    pending_pubacks = #{},
    msg_id_counter = 0,
    recv_buffer = <<>>,
    service_registry = Registry,
    advertised_services = #{},
    advertised_subscriptions = #{},
    endpoint_connections = #{},
    provider_selector_state = SelectorState
}
```

**Edge Cases:**
- [ ] Invalid URL format
- [ ] Missing realm in URL
- [ ] Invalid host/port
- [ ] Provider selector initialization failure
- [ ] Service registry initialization failure

**Test Scenarios:**
- [ ] Valid HTTP/3 URL with all components
- [ ] Invalid URL formats (missing scheme, host, port)
- [ ] Different provider selector strategies (random, round-robin, least-connections)
- [ ] Custom options (timeout, strategy, max_attempts)
- [ ] Verify cache pruning timer is scheduled

---

### 1.2. Connection Establishment (`handle_info(connect, State)`, `do_connect/1`)

**Location:** Lines 594, 855

**Behavior:**
- Extracts host and port from URL
- Opens QUIC connection using `macula_quic:connect/3`
- Opens bidirectional stream
- Sends CONNECT message with node_id and realm
- Transitions to connected status
- Logs connection success

**QUIC Connection Setup:**
- Uses server certificates if provided in opts
- Configures ALPN (Application-Layer Protocol Negotiation)
- Sets stream options (active mode, binary)

**Edge Cases:**
- [ ] Connection timeout
- [ ] Invalid server certificate
- [ ] QUIC connection refused
- [ ] Stream creation failure
- [ ] Network unreachable
- [ ] DNS resolution failure

**Test Scenarios:**
- [ ] Successful connection to valid gateway
- [ ] Connection timeout after N seconds
- [ ] TLS certificate validation failure
- [ ] Server unreachable (network error)
- [ ] Connection established but stream fails
- [ ] Concurrent connection attempts
- [ ] Reconnection after disconnect

---

### 1.3. Connection Termination (`stop/1`, `terminate/2`)

**Location:** Lines 145, 832

**Behavior:**
- Closes all endpoint connections
- Closes primary QUIC stream
- Closes primary QUIC connection
- Cancels all pending RPC call timers
- Cancels all pending DHT query timers
- Cancels all puback retry timers
- Cancels all re-advertisement timers
- Cancels all re-subscription timers
- Logs termination reason

**Cleanup Process:**
- Iterates through `endpoint_connections` map
- Calls `macula_quic:close_stream/1` for each
- Calls `macula_quic:close_connection/1` for primary connection

**Edge Cases:**
- [ ] Terminate called while connection is pending
- [ ] Terminate with active RPC calls
- [ ] Terminate with active subscriptions
- [ ] Terminate with pending advertisements
- [ ] Already disconnected when terminate called

**Test Scenarios:**
- [ ] Clean shutdown with no pending operations
- [ ] Shutdown with pending RPC calls (should timeout)
- [ ] Shutdown with active subscriptions (should clean up)
- [ ] Shutdown with endpoint connections (should close all)
- [ ] Crash scenarios (ensure supervisor restarts)

---

## 2. Message Protocol Handling

### 2.1. Message Encoding/Decoding

**Functions:**
- `encode_json/1` (Line 1528)
- `decode_json/1` (Line 1533)

**Behavior:**
- Uses `macula_protocol_encoder:encode_json/1` for encoding
- Uses `macula_protocol_decoder:decode_json/1` for decoding
- Handles JSON to Erlang map conversion

**Edge Cases:**
- [ ] Invalid JSON syntax
- [ ] Deeply nested JSON structures
- [ ] Very large JSON payloads (>1MB)
- [ ] Unicode/UTF-8 handling
- [ ] Binary vs string values
- [ ] Null values in JSON

**Test Scenarios:**
- [ ] Encode/decode simple maps
- [ ] Encode/decode nested structures
- [ ] Encode/decode lists
- [ ] Encode/decode binary values
- [ ] Round-trip encoding (encode → decode should equal original)
- [ ] Error handling for invalid JSON

---

### 2.2. Message Sending (`send_message/3`, `send_message_raw/4`)

**Location:** Lines 947, 952

**Behavior:**
- `send_message/3`: Delegates to `send_message_raw/4` with current stream
- `send_message_raw/4`:
  - Encodes message using `macula_protocol_encoder:encode/2`
  - Sends encoded binary via `macula_quic:send/2`
  - Returns `ok` or `{error, Reason}`

**Message Types:**
- `connect`
- `publish`
- `subscribe`
- `unsubscribe`
- `call`
- `reply`
- `rpc_route`
- `find_value`
- `store`
- `puback`

**Edge Cases:**
- [ ] Send on closed stream
- [ ] Send on disconnected connection
- [ ] Send with invalid message type
- [ ] Send with oversized payload
- [ ] Network congestion/backpressure
- [ ] Encoding failure

**Test Scenarios:**
- [ ] Send all message types successfully
- [ ] Send when connection is disconnected (should fail)
- [ ] Send when stream is closed (should fail)
- [ ] Concurrent sends on same stream
- [ ] Send large messages (test limits)
- [ ] Send message encoding error handling

---

### 2.3. Message Receiving (`handle_info({quic, Data, Stream, _Props}, State)`, `handle_received_data/2`)

**Location:** Lines 605, 999

**Behavior:**
- Receives binary data from QUIC stream
- Appends to receive buffer (for partial messages)
- Decodes complete messages from buffer
- Processes each decoded message
- Updates receive buffer with remaining bytes

**Message Decoding (`decode_messages/2`):**
- Checks for complete message header (8 bytes)
- Extracts version, type_id, flags, length
- Checks if full message available
- Decodes MessagePack payload
- Returns list of decoded messages + remaining buffer

**Edge Cases:**
- [ ] Partial message received (wait for more data)
- [ ] Multiple messages in single receive
- [ ] Message split across multiple receives
- [ ] Invalid message header
- [ ] MessagePack decode failure
- [ ] Buffer overflow (very large messages)
- [ ] Out-of-order message delivery

**Test Scenarios:**
- [ ] Receive complete message in one chunk
- [ ] Receive message split across 2-3 chunks
- [ ] Receive multiple messages in single chunk
- [ ] Receive invalid message (decode error)
- [ ] Large message handling (>1MB)
- [ ] Concurrent message processing

---

### 2.4. Message Processing (`process_message/2`)

**Location:** Lines 1038+

**Behavior:**
Dispatches based on message type:

**`{publish, Msg}`** (Line 1038):
- Extracts topic, payload, qos, from_node_id
- Finds matching subscriptions (pattern matching)
- Invokes each subscription callback
- Sends PUBACK if QoS=1
- Logs delivery or errors

**`{call, Msg}`** (Line 1092):
- Extracts procedure, args, call_id
- Looks up handler in `advertised_services`
- Invokes handler function with args
- Sends REPLY with result or error
- Logs execution

**`{reply, Msg}`** (Line 1119):
- Extracts call_id, result/error
- Looks up in `pending_calls`
- Cancels timeout timer
- Replies to caller with result
- Removes from pending_calls
- Logs success/timeout

**`{rpc_route, Msg}`** (Line 1194):
- Handles DHT-routed RPC messages
- Checks destination node_id
- If local: delivers to connection or handler
- If remote: routes to next hop
- Manages hop count, max_hops

**`{find_value_reply, Msg}`** (Line 1235):
- Handles DHT query responses
- Extracts providers or subscribers
- Continues pending RPC or publish operation
- Caches results in service registry

**`{store_reply, Msg}`** (Line 1240):
- Acknowledges DHT STORE operations
- Logs success/failure

**`{puback, Msg}`** (Line 1244):
- Acknowledges QoS=1 publish delivery
- Cancels retry timer
- Removes from `pending_pubacks`
- Logs acknowledgment

**`{pong, Msg}`** (Line 1190):
- Responds to keepalive PING messages
- Updates connection health

**Edge Cases:**
- [ ] Unknown message type
- [ ] Missing required fields
- [ ] Invalid field types
- [ ] Message for unknown call_id
- [ ] Handler crash during execution
- [ ] Callback crash during delivery
- [ ] Timeout after message processed

**Test Scenarios:**
- [ ] Process each message type successfully
- [ ] Handle missing fields gracefully
- [ ] Handler/callback crash handling
- [ ] Unknown message type (should log warning)
- [ ] Race condition: timeout then reply arrives
- [ ] Concurrent message processing

---

## 3. Pub/Sub Operations

### 3.1. Subscribe (`handle_call({subscribe, Topic, Callback}, _From, State)`)

**Location:** Line 296

**Behavior:**
- Validates connection status (must be connected)
- Creates subscription reference
- Stores {Topic, Callback} in `subscriptions` map
- Sends SUBSCRIBE message to gateway
- Advertises subscription in DHT (optional)
- Schedules re-subscription timer (if TTL configured)
- Returns `{reply, {ok, SubRef}, State}`

**DHT Advertisement (`advertise_subscription_in_dht/3`):**
- Creates subscriber key: `macula:sub:Topic`
- Creates subscriber value: NodeId + endpoint
- Sends STORE message to DHT
- Caches in service registry
- Schedules re-advertisement timer

**Edge Cases:**
- [ ] Subscribe when disconnected
- [ ] Subscribe to same topic multiple times
- [ ] Callback is not a function
- [ ] Invalid topic format
- [ ] DHT STORE fails
- [ ] Re-subscription timer fires after unsubscribe

**Test Scenarios:**
- [ ] Subscribe to simple topic
- [ ] Subscribe to wildcard topic (`energy.*.measured`)
- [ ] Multiple subscriptions to same topic
- [ ] Subscribe while disconnected (should queue or error)
- [ ] Callback invocation on message delivery
- [ ] DHT advertisement success/failure
- [ ] Re-subscription timer behavior

---

### 3.2. Unsubscribe (`handle_call({unsubscribe, SubRef}, _From, State)`)

**Location:** Line 325

**Behavior:**
- Looks up subscription by SubRef
- Removes from `subscriptions` map
- Sends UNSUBSCRIBE message to gateway
- Cancels re-subscription timer if exists
- Unadvertises from DHT (sends STORE with TTL=0)
- Returns `{reply, ok, State}`

**Cleanup:**
- Removes from `advertised_subscriptions` map
- Cancels timer_ref
- Logs unsubscription

**Edge Cases:**
- [ ] Unsubscribe with unknown SubRef
- [ ] Unsubscribe when disconnected
- [ ] Unsubscribe after already unsubscribed
- [ ] Timer fires after unsubscribe
- [ ] DHT unadvertise fails

**Test Scenarios:**
- [ ] Unsubscribe valid subscription
- [ ] Unsubscribe unknown SubRef (should fail gracefully)
- [ ] Unsubscribe all subscriptions on disconnect
- [ ] Timer cleanup verification
- [ ] DHT unadvertisement

---

### 3.3. Publish (`handle_call({publish, Topic, Data, Opts}, _From, State)`)

**Location:** Line 265

**Behavior:**
- Validates connection status
- Encodes data to JSON
- Determines QoS level (0 or 1)
- If QoS=0: Sends message directly
- If QoS=1: Stores in `pending_pubacks`, schedules retry
- Discovers remote subscribers via DHT
- Routes to local and remote subscribers

**QoS 0 (At-most-once):**
- Sends PUBLISH message
- No acknowledgment required
- No retry

**QoS 1 (At-least-once):**
- Generates message ID
- Sends PUBLISH message
- Adds to `pending_pubacks` with retry timer
- Waits for PUBACK
- Retries if no PUBACK received (up to max_retries)

**Remote Subscriber Discovery (`discover_remote_subscribers/5`):**
- Checks subscriber cache first
- If cache miss: Queries DHT for subscribers
- Caches discovered subscribers (TTL=60s)
- Routes message to each remote subscriber

**Routing (`route_to_remote_subscribers/5`):**
- Iterates through subscriber list
- Sends PUBLISH to each remote node's endpoint
- Uses existing endpoint connections or creates new ones

**Edge Cases:**
- [ ] Publish when disconnected
- [ ] Invalid topic format
- [ ] JSON encoding failure
- [ ] No subscribers found
- [ ] PUBACK timeout (QoS=1)
- [ ] Retry exhausted
- [ ] DHT query timeout
- [ ] Endpoint connection failure

**Test Scenarios:**
- [ ] Publish QoS=0 (fire-and-forget)
- [ ] Publish QoS=1 with acknowledgment
- [ ] Publish QoS=1 with retry on timeout
- [ ] Publish to topic with no subscribers
- [ ] Publish to topic with remote subscribers
- [ ] Concurrent publishes to same topic
- [ ] DHT cache hit vs miss scenarios
- [ ] Endpoint connection reuse

---

### 3.4. Topic Pattern Matching (`topic_matches/3`)

**Location:** Line 1520

**Behavior:**
- Compares subscription pattern with published topic
- Supports exact match: `energy.home.measured`
- Supports wildcard match: `energy.*.measured` matches `energy.home.measured`
- Split topics by `.` and compare segments
- `*` wildcard matches any single segment

**Edge Cases:**
- [ ] Exact match
- [ ] Single wildcard
- [ ] Multiple wildcards
- [ ] Empty topic
- [ ] Topic with trailing/leading dots
- [ ] Pattern longer than topic
- [ ] Topic longer than pattern

**Test Scenarios:**
- [ ] `energy.home.measured` matches `energy.home.measured`
- [ ] `energy.*.measured` matches `energy.home.measured`
- [ ] `energy.*.measured` matches `energy.solar.measured`
- [ ] `energy.*.*` matches `energy.home.measured`
- [ ] `energy.home` does NOT match `energy.home.measured`
- [ ] `*.*.*` matches `energy.home.measured`

---

## 4. RPC Operations

### 4.1. RPC Call (`handle_call({call, Procedure, Args, Opts}, From, State)`)

**Location:** Line 373

**Behavior:**
- Validates connection status
- Queries DHT for service providers
- If no providers: Returns `{error, no_providers}`
- If providers found: Initiates remote call with failover

**DHT Service Discovery (`send_find_value_async/5`):**
- Creates service key: `macula:svc:Procedure`
- Sends FIND_VALUE message to gateway
- Stores query in `pending_queries` with timeout timer
- Waits for FIND_VALUE_REPLY

**Provider Cache:**
- Checks `service_registry` cache first
- If cache hit: Uses cached providers
- If cache miss: Queries DHT

**Edge Cases:**
- [ ] Call when disconnected
- [ ] No providers advertised
- [ ] DHT query timeout
- [ ] All providers fail
- [ ] Provider cache stale
- [ ] Invalid procedure name
- [ ] Invalid arguments

**Test Scenarios:**
- [ ] Call with providers available
- [ ] Call with no providers (error)
- [ ] DHT cache hit vs miss
- [ ] DHT query timeout
- [ ] Provider discovery then call

---

### 4.2. Remote Call Execution (`do_remote_call_with_failover/8`)

**Location:** Line 1574

**Behavior:**
- Implements retry logic with multiple providers
- Filters excluded providers (previous failures)
- Selects provider using configured strategy
- Attempts call to selected provider
- On failure: Excludes provider and retries with next
- Max attempts: Configurable (default 3)

**Provider Selection Strategies:**
1. **Random:** Randomly select from available providers
2. **Round-robin:** Rotate through providers in order
3. **Least-connections:** Select provider with fewest active calls
4. **Weighted:** Select based on provider weights
5. **First:** Always use first provider
6. **Local-first:** Prefer local providers
7. **Latency-based:** Select provider with lowest latency
8. **Custom:** User-defined selection function

**Failover Logic:**
1. Select provider
2. Attempt call
3. On error:
   - Add provider to exclusion list
   - Increment attempt counter
   - If attempts < max_attempts: Retry with different provider
   - If attempts >= max_attempts: Return error

**Edge Cases:**
- [ ] All providers excluded
- [ ] Max attempts exceeded
- [ ] Provider crashes during call
- [ ] Network partition during call
- [ ] Timeout on specific provider
- [ ] Provider becomes unavailable mid-call

**Test Scenarios:**
- [ ] Successful call on first attempt
- [ ] Failover to second provider after first fails
- [ ] Failover to third provider after two failures
- [ ] All providers fail (max attempts exceeded)
- [ ] Each provider selection strategy
- [ ] Concurrent calls with different strategies

---

### 4.3. Direct Call to Provider (`do_remote_call_to_provider/9`)

**Location:** Line 1686

**Behavior:**
- Gets or creates endpoint connection to provider
- Generates unique call ID
- Creates CALL message with procedure, args, call_id
- Sends message to provider via endpoint connection
- Stores in `pending_calls` with timeout timer
- Returns `{noreply, State}` (async)

**Endpoint Connection Management (`get_or_create_endpoint_connection/2`):**
- Checks `endpoint_connections` map
- If exists: Reuses connection
- If not: Creates new QUIC connection to endpoint
- Stores in map for future reuse

**Timeout Handling:**
- Configurable timeout (default 30s)
- Timer message: `{call_timeout, CallId}`
- On timeout: Remove from pending_calls, reply with `{error, timeout}`

**Edge Cases:**
- [ ] Endpoint connection fails
- [ ] Message send fails
- [ ] Timeout before provider responds
- [ ] Response after timeout
- [ ] Concurrent calls to same provider
- [ ] Provider endpoint unreachable

**Test Scenarios:**
- [ ] Successful call with response
- [ ] Call timeout (no response)
- [ ] Endpoint connection creation
- [ ] Endpoint connection reuse
- [ ] Concurrent calls to multiple providers
- [ ] Response arrives after timeout (should be ignored)

---

### 4.4. Reply Handling (`process_message({reply, Msg}, State)`)

**Location:** Line 1119

**Behavior:**
- Extracts call_id, result, error from message
- Looks up call_id in `pending_calls`
- If found:
  - Cancels timeout timer
  - Replies to caller with `{ok, Result}` or `{error, Error}`
  - Removes from pending_calls
  - Logs success
- If not found:
  - Logs warning (likely timeout or duplicate)

**Edge Cases:**
- [ ] Reply for unknown call_id
- [ ] Reply after timeout
- [ ] Duplicate reply
- [ ] Reply with both result and error
- [ ] Reply with neither result nor error

**Test Scenarios:**
- [ ] Successful reply with result
- [ ] Error reply with error reason
- [ ] Reply after timeout (should be ignored)
- [ ] Unknown call_id (should log warning)

---

## 5. Service Advertisement

### 5.1. Advertise Service (`handle_call({advertise, Procedure, Handler, Opts}, _From, State)`)

**Location:** Line 415

**Behavior:**
- Validates handler is a function
- Stores handler in `advertised_services` map
- Creates service metadata (node_id, endpoint, metadata)
- Sends STORE to DHT with service key
- Schedules re-advertisement timer (based on TTL)
- Returns `{reply, ok, State}`

**DHT STORE:**
- Service key: `macula:svc:Procedure`
- Service value: `#{node_id, endpoint, metadata, ttl}`
- TTL: Configurable (default 300s)

**Re-advertisement Timer:**
- Fires before TTL expires (TTL * 0.8)
- Sends STORE again to refresh DHT entry
- Reschedules timer

**Edge Cases:**
- [ ] Handler is not a function
- [ ] Invalid procedure name
- [ ] DHT STORE fails
- [ ] Re-advertisement timer fires after unadvertise
- [ ] Concurrent advertisements of same procedure
- [ ] Advertisement while disconnected

**Test Scenarios:**
- [ ] Advertise service successfully
- [ ] Advertisement with custom metadata
- [ ] Advertisement with custom TTL
- [ ] Re-advertisement timer behavior
- [ ] DHT STORE failure handling
- [ ] Handler invocation on incoming call

---

### 5.2. Unadvertise Service (`handle_call({unadvertise, Procedure}, _From, State)`)

**Location:** Line 492

**Behavior:**
- Looks up procedure in `advertised_services`
- If found:
  - Cancels re-advertisement timer
  - Sends STORE to DHT with TTL=0 (removes entry)
  - Removes from `advertised_services` map
  - Returns `{reply, ok, State}`
- If not found:
  - Returns `{reply, {error, not_found}, State}`

**Edge Cases:**
- [ ] Unadvertise unknown procedure
- [ ] Unadvertise after already unadvertised
- [ ] Timer fires after unadvertise
- [ ] DHT STORE (removal) fails

**Test Scenarios:**
- [ ] Unadvertise advertised service
- [ ] Unadvertise unknown procedure
- [ ] Timer cleanup verification
- [ ] DHT entry removal

---

### 5.3. Re-advertisement Timer (`handle_info({readvertise, Procedure}, State)`)

**Location:** Line 697

**Behavior:**
- Looks up procedure in `advertised_services`
- If found:
  - Sends STORE to DHT to refresh entry
  - Reschedules timer for next re-advertisement
  - Updates timer_ref in map
- If not found:
  - Logs warning (service was unadvertised)

**Edge Cases:**
- [ ] Service unadvertised before timer fires
- [ ] DHT STORE fails on re-advertisement
- [ ] Multiple re-advertisements in quick succession

**Test Scenarios:**
- [ ] Re-advertisement timer fires and refreshes DHT
- [ ] Timer fires after service unadvertised (should skip)
- [ ] Multiple re-advertisements over time

---

## 6. Provider Selection

### 6.1. Provider Selector Integration

**Initialization:**
- Module: `macula_provider_selector`
- State: `provider_selector_state`
- Strategy: Configurable (random, round-robin, least-connections, etc.)

**Selection (`macula_provider_selector:select_provider/3`):**
- Input: Available providers, Strategy, Selector state
- Output: `{ok, Provider, NewSelectorState}` or `{error, Reason}`

**Strategies:**
1. **Random:** `crypto:rand_uniform/2`
2. **Round-robin:** Index counter incremented each call
3. **Least-connections:** Track active calls per provider
4. **Weighted:** Provider weights (metadata field)
5. **First:** Always first provider in list
6. **Local-first:** Prefer local node_id, fallback to remote
7. **Latency-based:** Track call latency, prefer fastest
8. **Custom:** User-provided function

**State Management:**
- Each strategy maintains its own state
- Round-robin: `#{round_robin_index => Integer}`
- Least-connections: `#{connection_counts => #{NodeId => Count}}`
- Latency-based: `#{latencies => #{NodeId => AvgLatency}}`

**Edge Cases:**
- [ ] Empty provider list
- [ ] Strategy not implemented
- [ ] State corruption
- [ ] All providers have equal metrics
- [ ] Provider removed mid-selection

**Test Scenarios:**
- [ ] Random selection distribution
- [ ] Round-robin ordering
- [ ] Least-connections selects lowest
- [ ] Weighted selection respects weights
- [ ] First always selects first
- [ ] Local-first prefers local node
- [ ] Latency-based selects fastest
- [ ] Custom strategy invocation

---

## 7. Connection Pooling

### 7.1. Endpoint Connection Management

**State:**
```erlang
endpoint_connections :: #{endpoint() => connection_info()}

connection_info() = #{
    connection := pid(),
    stream := pid(),
    status := connected | error,
    created_at := erlang:timestamp()
}
```

**Get or Create (`get_or_create_endpoint_connection/2`):**
- Checks if endpoint exists in map
- If yes: Returns existing connection_info
- If no: Calls `create_endpoint_connection/2`

**Create (`create_endpoint_connection/2`):**
- Parses endpoint URL
- Opens QUIC connection to endpoint
- Opens bidirectional stream
- Sends CONNECT message
- Stores in `endpoint_connections` map
- Returns connection_info

**Edge Cases:**
- [ ] Connection creation fails
- [ ] Connection exists but is dead
- [ ] Concurrent creation of same endpoint
- [ ] Endpoint URL parsing fails
- [ ] Stream creation fails
- [ ] Connection timeout

**Test Scenarios:**
- [ ] Create new endpoint connection
- [ ] Reuse existing endpoint connection
- [ ] Connection creation failure
- [ ] Multiple endpoints managed
- [ ] Connection cleanup on termination

---

### 7.2. Connection Health Tracking

**Not currently implemented, but planned:**
- Periodic health checks (PING/PONG)
- Connection timeout detection
- Automatic reconnection
- Connection pool size limits
- Idle connection cleanup

**Test Scenarios (Future):**
- [ ] Health check PING/PONG
- [ ] Detect dead connections
- [ ] Automatic reconnection
- [ ] Pool size enforcement
- [ ] Idle timeout

---

## 8. Cache Management

### 8.1. Service Registry Caching

**Initialization:**
```erlang
Registry = macula_service_registry:new()
```

**Cache Operations:**
- `cache_services/4`: Cache discovered providers
- `cache_subscribers/4`: Cache discovered subscribers
- `discover_services/2`: Check cache for providers
- `discover_subscribers/2`: Check cache for subscribers

**TTL Management:**
- Default TTL: 60 seconds
- Automatic expiry on cache miss
- Periodic pruning every 60 seconds

**Edge Cases:**
- [ ] Cache hit within TTL
- [ ] Cache miss after TTL expiry
- [ ] Cache corruption
- [ ] Very large cache (memory pressure)

**Test Scenarios:**
- [ ] Cache provider discovery
- [ ] Cache subscriber discovery
- [ ] Cache hit within TTL
- [ ] Cache miss after expiry
- [ ] Manual cache invalidation
- [ ] Concurrent cache access

---

### 8.2. Cache Pruning (`handle_info(prune_caches, State)`)

**Location:** Line 805

**Behavior:**
- Calls `macula_service_registry:prune_expired/1`
- Removes expired cache entries (TTL exceeded)
- Logs pruning statistics
- Reschedules timer for next prune (60s)

**Edge Cases:**
- [ ] Nothing to prune
- [ ] Large number of expired entries
- [ ] Prune during active cache access

**Test Scenarios:**
- [ ] Prune timer fires and removes expired entries
- [ ] No-op when cache is empty
- [ ] Prune statistics logging

---

## 9. QoS and Reliability

### 9.1. At-Most-Once (QoS 0)

**Behavior:**
- Send message once
- No acknowledgment required
- No retry
- Fire-and-forget

**Use Cases:**
- High-frequency sensor data
- Metrics/telemetry
- Non-critical notifications

**Test Scenarios:**
- [ ] Publish QoS 0 message
- [ ] Verify no PUBACK expected
- [ ] Verify no retry on failure

---

### 9.2. At-Least-Once (QoS 1)

**Behavior:**
- Send message with unique message ID
- Wait for PUBACK acknowledgment
- Retry if no PUBACK within timeout
- Maximum retries configurable

**Retry Logic (`handle_info({puback_timeout, MsgId}, State)`):**
- Looks up message in `pending_pubacks`
- If retry_count < max_retries:
  - Resend message
  - Increment retry_count
  - Reschedule timer
- If retry_count >= max_retries:
  - Log error (delivery failed)
  - Remove from pending_pubacks

**Edge Cases:**
- [ ] PUBACK received before timeout
- [ ] PUBACK timeout, retry succeeds
- [ ] Max retries exceeded
- [ ] Duplicate PUBACK
- [ ] PUBACK for unknown message

**Test Scenarios:**
- [ ] Publish QoS 1, receive PUBACK
- [ ] Timeout, retry, receive PUBACK
- [ ] Max retries exceeded (failure)
- [ ] Duplicate PUBACK handling

---

## 10. Error Handling and Edge Cases

### 10.1. Connection Errors

**Scenarios:**
- Connection timeout during establishment
- Connection closed by server
- Network partition
- DNS resolution failure
- TLS/certificate errors

**Handling:**
- Log error with reason
- Set status to `error` or `disconnected`
- Notify pending callers
- Clean up resources

**Test Scenarios:**
- [ ] Connection timeout
- [ ] Server closes connection
- [ ] Network unreachable
- [ ] Certificate validation failure

---

### 10.2. Message Processing Errors

**Scenarios:**
- Invalid message format
- Unknown message type
- Missing required fields
- MessagePack decode failure
- Handler crash
- Callback crash

**Handling:**
- Log error with details
- Send error reply if applicable
- Continue processing other messages
- Don't crash connection process

**Test Scenarios:**
- [ ] Invalid MessagePack
- [ ] Unknown message type
- [ ] Missing required field
- [ ] Handler throws exception
- [ ] Callback throws exception

---

### 10.3. Timeout Scenarios

**Types:**
- RPC call timeout
- DHT query timeout
- PUBACK timeout
- Connection timeout

**Handling:**
- Cancel timer on success
- On timeout: Reply with `{error, timeout}`
- Remove from pending operations
- Log timeout

**Test Scenarios:**
- [ ] RPC call timeout
- [ ] DHT query timeout
- [ ] PUBACK timeout
- [ ] Connection establishment timeout

---

## 11. Concurrency and Race Conditions

### 11.1. Concurrent Operations

**Scenarios:**
- Multiple RPC calls in parallel
- Concurrent publishes to different topics
- Subscribe/unsubscribe while messages arriving
- Advertise/unadvertise during active calls
- Connection close during active operations

**GenServer Serialization:**
- All API calls serialized through gen_server:call
- State updates are atomic
- No race conditions within single process

**Multi-Process Races:**
- Endpoint connections (separate processes)
- QUIC stream send/receive
- Timer messages

**Test Scenarios:**
- [ ] Concurrent RPC calls (100+)
- [ ] Concurrent publishes
- [ ] Subscribe during message delivery
- [ ] Connection close during RPC call

---

### 11.2. State Consistency

**Invariants:**
- `pending_calls` keys match active timers
- `pending_queries` keys match active timers
- `pending_pubacks` keys match active timers
- `advertised_services` procedures have active timers
- `advertised_subscriptions` topics have active timers
- `endpoint_connections` contain valid connection pids

**Test Scenarios:**
- [ ] Verify state consistency after operations
- [ ] Check timer cleanup on completion
- [ ] Validate no orphaned timers

---

## 12. Utility Functions

### 12.1. URL Parsing (`parse_url/1`)

**Behavior:**
- Parses HTTP/3 URL: `h3://hostname:port/realm`
- Extracts scheme, host, port, realm
- Returns `{Realm, Host, Port}`

**Edge Cases:**
- [ ] Missing scheme
- [ ] Missing port (use default)
- [ ] Missing realm
- [ ] Invalid URL format
- [ ] IPv6 addresses
- [ ] Hostname vs IP

**Test Scenarios:**
- [ ] Valid URL with all components
- [ ] URL with default port
- [ ] URL without realm
- [ ] IPv4 and IPv6 addresses
- [ ] Invalid URL formats

---

### 12.2. Node ID Generation (`generate_node_id/0`)

**Behavior:**
- Generates unique 256-bit node ID
- Uses `crypto:strong_rand_bytes(32)`
- Returns binary

**Test Scenarios:**
- [ ] Generate multiple node IDs
- [ ] Verify uniqueness
- [ ] Verify length (32 bytes)

---

### 12.3. Message ID Generation (`next_message_id/1`)

**Behavior:**
- Increments `msg_id_counter`
- Converts to binary
- Returns `{MsgId, NewState}`

**Edge Cases:**
- [ ] Counter overflow (wraps around)

**Test Scenarios:**
- [ ] Generate sequential message IDs
- [ ] Verify uniqueness within session

---

### 12.4. Binary Conversion (`ensure_binary/1`)

**Behavior:**
- Converts atom, list, or binary to binary
- Preserves existing binaries

**Test Scenarios:**
- [ ] Convert atom to binary
- [ ] Convert list to binary
- [ ] Binary passthrough
- [ ] Invalid types

---

### 12.5. Provider Normalization (`normalize_provider/1`)

**Behavior:**
- Ensures provider map has required fields
- Extracts node_id, endpoint from various formats
- Returns normalized provider map

**Test Scenarios:**
- [ ] Normalize full provider map
- [ ] Normalize minimal provider
- [ ] Invalid provider format

---

## Summary of Test Requirements

### Test Coverage Goals

| Area | Target Coverage | Priority |
|------|----------------|----------|
| Connection Lifecycle | 95% | CRITICAL |
| Message Protocol | 90% | CRITICAL |
| Pub/Sub Operations | 90% | HIGH |
| RPC Operations | 90% | HIGH |
| Service Advertisement | 85% | MEDIUM |
| Provider Selection | 85% | MEDIUM |
| Connection Pooling | 80% | MEDIUM |
| Cache Management | 85% | MEDIUM |
| QoS/Reliability | 90% | HIGH |
| Error Handling | 85% | HIGH |
| Utility Functions | 90% | LOW |

### Total Test Scenarios

- **Connection Lifecycle:** 25 scenarios
- **Message Protocol:** 30 scenarios
- **Pub/Sub:** 35 scenarios
- **RPC:** 40 scenarios
- **Service Advertisement:** 20 scenarios
- **Provider Selection:** 30 scenarios
- **Connection Pooling:** 15 scenarios
- **Cache Management:** 15 scenarios
- **QoS:** 10 scenarios
- **Error Handling:** 20 scenarios
- **Concurrency:** 15 scenarios
- **Utilities:** 20 scenarios

**Total:** ~275 test scenarios

---

## Next Steps

1. Use this document to create comprehensive test suites
2. Organize tests by responsibility area (matches future module extraction)
3. Follow TDD: Write tests first (Red), then verify existing code passes (Green)
4. Document any discovered behaviors not listed here
5. Use test failures to identify bugs or edge cases in current implementation

---

**Document Version:** 1.0
**Last Updated:** 2025-01-13
**Next Review:** After test suite implementation
