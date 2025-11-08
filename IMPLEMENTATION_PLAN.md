# Macula Implementation Plan

**Current Status**: Foundation complete, ready for core implementation

**Next Implementation**: macula_protocol (Wire Protocol Layer)

---

## Why macula_protocol First?

### Dependencies

```
macula_protocol (wire protocol)
    ↓
    ├── macula_pubsub (needs message encoding)
    ├── macula_rpc (needs message encoding)
    ├── macula_membership (needs SWIM messages)
    └── macula_routing (needs DHT messages)
```

**Rationale**: All higher-level features need to serialize/deserialize messages. Implement the protocol layer first with TDD, then other modules can use it.

---

## Phase 1: macula_protocol (Week 1-2)

### Objectives

1. Define message types for all Macula operations
2. Implement efficient binary encoding/decoding
3. Support message framing over QUIC streams
4. Version negotiation for protocol evolution
5. Achieve 95% test coverage

### Core Message Types

#### 1. Control Messages

```erlang
%% Connection lifecycle
-type connect_msg() :: #{
    version := binary(),           % Protocol version "1.0"
    node_id := binary(),           % 32-byte node ID
    realm_id := binary(),          % 32-byte realm ID
    capabilities := [atom()]       % [pubsub, rpc, routing, gateway]
}.

-type disconnect_msg() :: #{
    reason := atom(),              % normal | error | timeout
    message := binary()            % Human-readable reason
}.

-type ping_msg() :: #{
    timestamp := integer(),        % Monotonic timestamp
    payload := binary()            % Optional echo data
}.

-type pong_msg() :: #{
    timestamp := integer(),        % Original ping timestamp
    payload := binary()            % Echo back payload
}.
```

#### 2. Pub/Sub Messages

```erlang
-type publish_msg() :: #{
    topic := binary(),             % Topic name
    payload := binary(),           % Message payload
    qos := 0 | 1 | 2,             % Quality of service
    retain := boolean(),           % Retain flag
    message_id := binary()         % 16-byte unique ID
}.

-type subscribe_msg() :: #{
    topics := [binary()],          % List of topic patterns
    qos := 0 | 1 | 2              % Requested QoS
}.

-type unsubscribe_msg() :: #{
    topics := [binary()]           % Topics to unsubscribe from
}.
```

#### 3. RPC Messages

```erlang
-type call_msg() :: #{
    call_id := binary(),           % 16-byte unique call ID
    module := binary(),            % Target module
    function := binary(),          % Target function
    args := [term()],              % Function arguments (encoded)
    timeout := integer()           % Timeout in milliseconds
}.

-type reply_msg() :: #{
    call_id := binary(),           % Matches original call_id
    result := {ok, term()} | {error, term()}
}.

-type cast_msg() :: #{
    module := binary(),            % Target module
    function := binary(),          % Target function
    args := [term()]              % Function arguments (encoded)
}.
```

#### 4. Membership Messages (SWIM)

```erlang
-type swim_ping_msg() :: #{
    target := binary(),            % Target node ID
    incarnation := integer(),      % Incarnation number
    members := [member_info()]     % Piggybacked member list
}.

-type swim_ack_msg() :: #{
    source := binary(),            % Source node ID
    incarnation := integer()       % Incarnation number
}.

-type swim_ping_req_msg() :: #{
    target := binary(),            % Node to ping indirectly
    via := [binary()]             % Nodes to route through
}.
```

#### 5. Routing Messages (Kademlia DHT)

```erlang
-type find_node_msg() :: #{
    target := binary(),            % Target node ID (256-bit)
    sender := binary()            % Sender node ID
}.

-type find_node_reply_msg() :: #{
    nodes := [node_info()],        % K closest nodes
    sender := binary()            % Responder node ID
}.

-type store_msg() :: #{
    key := binary(),               % Key (256-bit)
    value := binary(),             % Value to store
    ttl := integer()              % Time-to-live in seconds
}.

-type find_value_msg() :: #{
    key := binary(),               % Key to look up
    sender := binary()            % Sender node ID
}.
```

### Binary Encoding Format

#### Message Frame Structure

```
┌─────────────────────────────────────────┐
│ Version (1 byte)                        │  Always 0x01 for v1.0
├─────────────────────────────────────────┤
│ Message Type (1 byte)                   │  See type enum below
├─────────────────────────────────────────┤
│ Flags (1 byte)                          │  Reserved for future use
├─────────────────────────────────────────┤
│ Reserved (1 byte)                       │  Must be 0x00
├─────────────────────────────────────────┤
│ Payload Length (4 bytes, big-endian)   │  Length of payload
├─────────────────────────────────────────┤
│ Payload (N bytes)                       │  MessagePack-encoded data
└─────────────────────────────────────────┘

Total header: 8 bytes
Max payload: 2^32 - 1 bytes (4 GB, but enforce 1 MB practical limit)
```

#### Message Type Enum

```erlang
-define(MSG_CONNECT,        16#01).
-define(MSG_DISCONNECT,     16#02).
-define(MSG_PING,           16#03).
-define(MSG_PONG,           16#04).
-define(MSG_PUBLISH,        16#10).
-define(MSG_SUBSCRIBE,      16#11).
-define(MSG_UNSUBSCRIBE,    16#12).
-define(MSG_CALL,           16#20).
-define(MSG_REPLY,          16#21).
-define(MSG_CAST,           16#22).
-define(MSG_SWIM_PING,      16#30).
-define(MSG_SWIM_ACK,       16#31).
-define(MSG_SWIM_PING_REQ,  16#32).
-define(MSG_FIND_NODE,      16#40).
-define(MSG_FIND_NODE_REPLY,16#41).
-define(MSG_STORE,          16#42).
-define(MSG_FIND_VALUE,     16#43).
```

#### Payload Encoding: MessagePack

**Why MessagePack?**
- Fast binary serialization
- Compact (smaller than JSON)
- Schema-less (flexible)
- Excellent Erlang library: [msgpack-erlang](https://github.com/msgpack/msgpack-erlang)

**Example**:
```erlang
Msg = #{
    version => <<"1.0">>,
    node_id => <<1:256>>,
    realm_id => <<2:256>>,
    capabilities => [pubsub, rpc]
},
Encoded = msgpack:pack(Msg).
```

### Implementation Files

```
apps/macula_protocol/
├── src/
│   ├── macula_protocol.erl              % Main API
│   ├── macula_protocol_types.erl        % Type definitions
│   ├── macula_protocol_encoder.erl      % Binary encoding
│   ├── macula_protocol_decoder.erl      % Binary decoding
│   ├── macula_protocol_frame.erl        % Frame handling
│   └── macula_protocol_version.erl      % Version negotiation
└── test/
    ├── macula_protocol_test.erl         % EUnit tests
    ├── macula_protocol_encoder_test.erl % Encoding tests
    ├── macula_protocol_decoder_test.erl % Decoding tests
    ├── macula_protocol_prop.erl         % PropEr properties
    └── macula_protocol_SUITE.erl        % Common Test suite
```

### TDD Workflow for macula_protocol

#### Step 1: Define Types (Red)

```erlang
%% test/macula_protocol_encoder_test.erl
encode_connect_msg_test() ->
    Msg = #{
        version => <<"1.0">>,
        node_id => <<1:256>>,
        realm_id => <<2:256>>,
        capabilities => [pubsub]
    },
    Binary = macula_protocol_encoder:encode(connect, Msg),
    ?assert(is_binary(Binary)),
    ?assertEqual(<<1>>, binary:part(Binary, 0, 1)). % Version byte
```

**Expected**: Test fails (undefined function)

#### Step 2: Implement Minimal Encoding (Green)

```erlang
%% src/macula_protocol_encoder.erl
-module(macula_protocol_encoder).
-export([encode/2]).

encode(connect, Msg) ->
    Version = 1,
    Type = 16#01,
    Flags = 0,
    Reserved = 0,
    Payload = msgpack:pack(Msg),
    PayloadLen = byte_size(Payload),
    <<Version:8, Type:8, Flags:8, Reserved:8, PayloadLen:32, Payload/binary>>.
```

**Expected**: Test passes

#### Step 3: Add Round-Trip Property (Refactor)

```erlang
%% test/macula_protocol_prop.erl
prop_connect_roundtrip() ->
    ?FORALL(
        ConnectMsg,
        connect_msg_gen(),
        begin
            Encoded = macula_protocol_encoder:encode(connect, ConnectMsg),
            {ok, {connect, Decoded}} = macula_protocol_decoder:decode(Encoded),
            maps:without([timestamp], Decoded) =:=
                maps:without([timestamp], ConnectMsg)
        end
    ).
```

#### Step 4: Verify Coverage

```bash
$ rebar3 do eunit --app macula_protocol, cover -v
|  macula_protocol_encoder  |      100%  |
```

### Dependencies

Add to `apps/macula_protocol/rebar.config`:

```erlang
{deps, [
    {msgpack, "1.0.1"}  % MessagePack encoding
]}.
```

### Success Criteria

- [ ] All message types defined with clear specs
- [ ] Encoding/decoding functions for each type
- [ ] Round-trip property tests pass (1000+ test cases)
- [ ] 95% code coverage
- [ ] Frame parsing handles partial messages
- [ ] Version negotiation logic complete
- [ ] Comprehensive error handling
- [ ] Performance benchmarks (encode/decode <10μs per message)

### Estimated Time

- **Message type definitions**: 4 hours
- **Encoder implementation**: 8 hours
- **Decoder implementation**: 8 hours
- **Frame handling**: 4 hours
- **Tests + properties**: 8 hours
- **Documentation**: 2 hours

**Total**: 34 hours (~1.5 weeks with TDD)

---

## Phase 2: macula_quic (Week 2-3)

Once protocol layer is done, implement QUIC transport:

### Objectives

1. Wrap quicer library with clean API
2. Connection management (lifecycle, pools)
3. Stream multiplexing
4. Certificate generation/validation
5. Connection migration support

### Files

```
apps/macula_quic/
├── src/
│   ├── macula_quic.erl              % Main API
│   ├── macula_quic_listener.erl     % Server listening
│   ├── macula_quic_connection.erl   % Connection GenServer
│   ├── macula_quic_stream.erl       % Stream handler
│   └── macula_quic_cert.erl         % Certificate utilities
└── test/
    ├── macula_quic_test.erl
    ├── macula_quic_connection_test.erl
    └── macula_quic_SUITE.erl
```

### Integration with Protocol

```erlang
%% Send a message over QUIC stream
{ok, Stream} = macula_quic:open_stream(Conn),
Binary = macula_protocol:encode(ping, #{timestamp => now()}),
ok = macula_quic:send(Stream, Binary).

%% Receive and decode
{ok, Binary} = macula_quic:recv(Stream),
{ok, {ping, Msg}} = macula_protocol:decode(Binary).
```

---

## Phase 3: macula_membership (Week 4-5)

Implement SWIM gossip protocol using protocol + transport layers.

---

## Phase 4: macula_routing (Week 6-8)

Implement Kademlia DHT using protocol + membership.

---

## Phase 5: macula_pubsub + macula_rpc (Week 9-12)

High-level messaging APIs using all lower layers.

---

## Current Priorities

1. ✅ Foundation complete (build system, dependencies, testing)
2. **→ Next**: Implement macula_protocol with TDD (this document)
3. Then: macula_quic
4. Then: macula_membership
5. Then: macula_routing
6. Then: macula_pubsub + macula_rpc

---

**Ready to start implementing macula_protocol?**

Let me know and I'll begin with TDD approach:
1. Write failing tests for message encoding
2. Implement minimal encoder
3. Add decoder with round-trip tests
4. Property-based testing
5. Verify 95% coverage
