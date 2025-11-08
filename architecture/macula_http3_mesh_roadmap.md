# Macula HTTP/3 Mesh: Comprehensive Technical Roadmap

## Executive Summary

**Vision:** Macula enables distributed BEAM applications to form encrypted, self-healing mesh networks over HTTP/3, with zero infrastructure dependencies. Works behind NATs, scales to thousands of nodes, native distributed Erlang semantics - all over standard HTTPS.

**The "Wow" Factor:**
- Distributed Erlang over HTTP/3 (nobody else does this!)
- Works through corporate firewalls (it's just HTTPS!)
- Self-healing mesh topology (no central coordinator)
- Built-in NAT traversal (QUIC magic)
- Native BEAM implementation (minimal NIFs)

**Timeline:** 20 weeks (5 months)

**Outcome:** Production-ready mesh networking infrastructure for edge BEAM applications.

---

## Table of Contents

1. [Understanding QUIC and HTTP/3](#understanding-quic-and-http3)
2. [QUIC/HTTP/3 Libraries for BEAM](#quic-http3-libraries-for-beam)
3. [Architecture Overview](#architecture-overview)
4. [Detailed Roadmap](#detailed-roadmap)
5. [Architecture Diagrams](#architecture-diagrams)
6. [Technical Deep Dives](#technical-deep-dives)
7. [Success Metrics](#success-metrics)

---

## Understanding QUIC and HTTP/3

### What is QUIC?

**QUIC (Quick UDP Internet Connections)** is a modern transport protocol developed by Google and standardized by IETF as RFC 9000. It's designed to replace TCP for web traffic.

#### Key Characteristics

**1. UDP-Based Transport**
```
Traditional:  HTTP/2 → TLS → TCP → IP
Modern:       HTTP/3 → QUIC (includes TLS 1.3) → UDP → IP
```

QUIC runs over UDP instead of TCP, which provides several advantages:
- Faster connection establishment (0-RTT and 1-RTT)
- No head-of-line blocking across streams
- Better performance on lossy networks
- Easier NAT traversal (UDP is simpler than TCP for NAT)

**2. Built-in Encryption**
- TLS 1.3 is integrated into QUIC (not layered on top)
- All packets are encrypted (except initial handshake metadata)
- Forward secrecy by default
- Connection migration (can change IP addresses mid-connection)

**3. Multiplexed Streams**
```
┌─────────────────────────────────────┐
│ QUIC Connection                     │
│  ┌───────────┐ ┌───────────┐       │
│  │ Stream 0  │ │ Stream 1  │  ...  │
│  │ (Ordered) │ │ (Ordered) │       │
│  └───────────┘ └───────────┘       │
│                                     │
│  Streams are independent!           │
│  Loss in Stream 0 doesn't block     │
│  Stream 1 (unlike TCP)              │
└─────────────────────────────────────┘
```

**4. Connection Establishment**

Traditional TCP + TLS:
```
Client → Server:  SYN                        (1 RTT)
Server → Client:  SYN-ACK
Client → Server:  ACK
Client → Server:  ClientHello (TLS)          (2 RTT)
Server → Client:  ServerHello + Certificate
Client → Server:  Finished
Server → Client:  Finished
Client → Server:  HTTP Request               (3 RTT)
                  Total: 3 Round Trips
```

QUIC (first connection):
```
Client → Server:  Initial (ClientHello)      (1 RTT)
Server → Client:  Handshake (ServerHello)
Client → Server:  HTTP Request               (1 RTT)
                  Total: 1 Round Trip
```

QUIC (resumed connection):
```
Client → Server:  0-RTT Data + HTTP Request  (0 RTT!)
                  Total: 0 Round Trips
```

**5. Loss Recovery**
- Per-stream reliability (not per-connection like TCP)
- More sophisticated than TCP (monotonically increasing packet numbers)
- Better handling of spurious retransmissions
- Pluggable congestion control

**6. Connection Migration**
```
Mobile device scenario:
WiFi (IP: 192.168.1.100) → Cellular (IP: 10.20.30.40)

TCP:  Connection breaks, must reconnect (new handshake)
QUIC: Connection continues seamlessly (connection ID stays same)
```

### What is HTTP/3?

**HTTP/3** is the third major version of HTTP, using QUIC as its transport instead of TCP.

#### HTTP Evolution

```
HTTP/1.1 (1997)
  ↓
  - Text-based protocol
  - One request per connection (or pipelining)
  - Head-of-line blocking

HTTP/2 (2015)
  ↓
  - Binary framing
  - Multiplexing over single TCP connection
  - Header compression (HPACK)
  - Still suffers from TCP head-of-line blocking

HTTP/3 (2022)
  ↓
  - Same semantics as HTTP/2
  - QUIC transport (UDP-based)
  - No head-of-line blocking
  - 0-RTT connection resumption
  - Better mobile performance
```

#### HTTP/3 Frame Types

HTTP/3 uses similar frames to HTTP/2 but adapted for QUIC:

```
Frame Types:
- DATA:        Application data (response body)
- HEADERS:     HTTP headers (compressed with QPACK)
- PRIORITY:    Stream priority hints
- CANCEL_PUSH: Cancel server push
- SETTINGS:    Connection parameters
- PUSH_PROMISE: Server push announcement
- GOAWAY:      Graceful shutdown
- MAX_PUSH_ID: Limit server push
```

#### QPACK Header Compression

HTTP/3 uses QPACK (QUIC-aware header compression) instead of HPACK:
- Dynamic table updates on dedicated stream
- Prevents head-of-line blocking from header compression
- Better performance on lossy networks

### Why QUIC/HTTP/3 for Macula Mesh?

#### 1. **NAT Traversal**
UDP is much easier to punch through NATs than TCP:
- Simpler state machines in NAT devices
- Easier simultaneous open
- Better compatibility with STUN/TURN

#### 2. **Multiplexing Without Head-of-Line Blocking**
Perfect for distributed Erlang:
```
Process A → Stream 0:  send(...) → [packet lost!] → retransmit
Process B → Stream 1:  send(...) → delivered immediately!

With TCP: Process B would be blocked waiting for Process A's retransmit
With QUIC: Process B's stream is independent
```

#### 3. **Connection Migration**
Edge devices often change networks:
```
IoT device switches from WiFi to cellular:
  - TCP: Connection lost, full reconnect
  - QUIC: Seamless migration, no interruption
```

#### 4. **Firewall Friendly**
- Uses port 443 (standard HTTPS)
- Looks like HTTPS to middleboxes
- No special firewall rules needed

#### 5. **0-RTT Resumption**
Reconnecting nodes don't waste time:
```
Node rejoins mesh after brief disconnect:
  - TCP + TLS: 3 RTT to re-establish
  - QUIC 0-RTT: Immediate data transmission
```

#### 6. **Built-in Encryption**
- TLS 1.3 integrated (not optional)
- Perfect forward secrecy
- No configuration needed

### QUIC Protocol Details

#### Connection ID

QUIC uses Connection IDs instead of 4-tuple (src IP, src port, dst IP, dst port):

```
┌─────────────────────────────────────┐
│ QUIC Packet                         │
├─────────────────────────────────────┤
│ Header:                             │
│  - Connection ID: 0x1a2b3c4d...     │ ← Identifies connection
│  - Packet Number: 42                │
│  - Flags: ...                       │
├─────────────────────────────────────┤
│ Encrypted Payload                   │
└─────────────────────────────────────┘

Benefits:
- Connection survives IP address changes
- Load balancers can route without decryption
- NAT rebinding doesn't break connection
```

#### Stream Management

QUIC streams are lightweight:

```erlang
% Each Erlang process can have its own stream
spawn(fun() ->
    {ok, StreamId} = quic:open_stream(Conn),
    quic:send(StreamId, Data),
    receive
        {stream_data, StreamId, Response} -> handle(Response)
    end,
    quic:close_stream(StreamId)
end).

% Streams are cheap! Can create thousands
% No overhead like TCP sockets
```

#### Congestion Control

QUIC implements multiple congestion control algorithms:

- **Reno**: Traditional TCP-like
- **Cubic**: Linux default (aggressive)
- **BBR**: Google's Bottleneck Bandwidth and RTT
- **Custom**: Can implement your own!

For Macula Mesh:
```erlang
% Could implement edge-optimized congestion control
-module(macula_congestion).

estimate_bandwidth(Samples) ->
    % Use local measurements instead of RTT
    % Better for edge networks with variable latency
    ...
```

#### Flow Control

QUIC has flow control at two levels:

1. **Stream-level**: Each stream has credit
2. **Connection-level**: Overall connection credit

```
┌─────────────────────────────────────┐
│ Connection Credit: 1 MB             │
├─────────────────────────────────────┤
│ Stream 0:  256 KB credit            │
│ Stream 1:  512 KB credit            │
│ Stream 2:  256 KB credit            │
└─────────────────────────────────────┘

Prevents any single stream from starving the connection
```

---

## QUIC/HTTP/3 Libraries for BEAM

### 1. quicer (Recommended)

**Repository:** https://github.com/emqx/quic
**License:** Apache 2.0
**Maintainer:** EMQX Team (Erlang Solutions)
**Language:** Erlang + C (NIF wrapper)
**Backend:** Microsoft MsQuic

#### Overview

`quicer` is an Erlang NIF binding for Microsoft's MsQuic library. MsQuic is a production-grade QUIC implementation used by Windows, Azure, and various Microsoft services.

#### Architecture

```
┌─────────────────────────────────────┐
│ Erlang Application                  │
├─────────────────────────────────────┤
│ quicer (Erlang API)                 │
│  - quicer:listen/2                  │
│  - quicer:connect/3                 │
│  - quicer:send/2                    │
│  - quicer:recv/2                    │
├─────────────────────────────────────┤
│ quicer NIF (C)                      │
│  - Erlang ↔ MsQuic bridge          │
│  - Resource management              │
│  - Callback handling                │
├─────────────────────────────────────┤
│ MsQuic (C)                          │
│  - RFC 9000 QUIC implementation     │
│  - TLS 1.3 integration              │
│  - Platform-specific optimizations  │
├─────────────────────────────────────┤
│ OS Network Stack (UDP)              │
└─────────────────────────────────────┘
```

#### Features

✅ **Supported:**
- QUIC v1 (RFC 9000)
- TLS 1.3
- 0-RTT connection resumption
- Connection migration
- Multiple streams per connection
- Flow control and congestion control
- Both client and server modes

❌ **Limitations:**
- NIF dependency (requires C compilation)
- Tied to MsQuic release cycle
- Platform-specific quirks

#### API Examples

```erlang
%% Server
start_server() ->
    %% Load certificate
    {ok, Cert} = file:read_file("server.crt"),
    {ok, Key} = file:read_file("server.key"),

    %% Listen options
    ListenOpts = #{
        cert => Cert,
        key => Key,
        alpn => ["macula/1.0"],
        peer_unidi_stream_count => 10,
        peer_bidi_stream_count => 10
    },

    %% Start listener
    {ok, Listener} = quicer:listen("0.0.0.0", 4433, ListenOpts),

    %% Accept loop
    accept_loop(Listener).

accept_loop(Listener) ->
    {ok, Conn} = quicer:accept(Listener, [], 5000),
    {ok, Conn} = quicer:handshake(Conn),

    %% Spawn handler
    spawn(fun() -> handle_connection(Conn) end),

    %% Continue accepting
    accept_loop(Listener).

handle_connection(Conn) ->
    %% Accept stream
    {ok, Stream} = quicer:accept_stream(Conn, []),

    %% Receive data
    {ok, Data} = quicer:recv(Stream, 0),

    %% Process and respond
    Response = process(Data),
    ok = quicer:send(Stream, Response),

    %% Close stream
    quicer:close_stream(Stream).

%% Client
start_client() ->
    %% Connect options
    ConnOpts = #{
        alpn => ["macula/1.0"],
        verify => verify_peer,
        cacertfile => "ca.crt"
    },

    %% Connect
    {ok, Conn} = quicer:connect("server.example.com", 4433, ConnOpts, 5000),

    %% Open stream
    StreamOpts = #{active => false},
    {ok, Stream} = quicer:start_stream(Conn, StreamOpts),

    %% Send data
    ok = quicer:send(Stream, <<"Hello, QUIC!">>),

    %% Receive response
    {ok, Response} = quicer:recv(Stream, 0),
    io:format("Received: ~p~n", [Response]),

    %% Close
    quicer:close_stream(Stream),
    quicer:close_connection(Conn).
```

#### Production Usage

**EMQX:** Used in EMQX 5.0+ for MQTT over QUIC
- Handles millions of concurrent connections
- Production-tested at scale
- Good performance characteristics

**RabbitMQ:** Experimental QUIC support via quicer
- AMQP 1.0 over QUIC transport
- Still in development

#### Pros & Cons

**Pros:**
- ✅ Battle-tested (MsQuic used in Windows, Azure)
- ✅ Actively maintained
- ✅ Good documentation
- ✅ Performance optimized
- ✅ Cross-platform (Linux, macOS, Windows)
- ✅ Production-ready (used in EMQX)

**Cons:**
- ❌ NIF dependency (requires compilation)
- ❌ Tied to MsQuic (external C library)
- ❌ Breaking changes between MsQuic versions
- ❌ Limited control over low-level behavior

**Verdict:** ⭐⭐⭐⭐⭐ **Best choice for Macula Mesh**

### 2. xquic (Alternative)

**Repository:** https://github.com/alibaba/xquic
**License:** Apache 2.0
**Maintainer:** Alibaba Cloud
**Language:** C + Erlang bindings
**Backend:** xquic (Alibaba's QUIC)

#### Overview

xquic is Alibaba's in-house QUIC implementation, used in their edge CDN and cloud services.

#### Features

- QUIC v1 + IETF draft-29
- HTTP/3 support
- QUIC multipath extension
- BBR congestion control
- High performance (optimized for Alibaba scale)

#### Erlang Bindings

Erlang bindings exist but are less mature than quicer:
- https://github.com/emqx/xquic-erl (community maintained)

#### Pros & Cons

**Pros:**
- ✅ Very high performance
- ✅ Multipath QUIC support
- ✅ Used at Alibaba scale

**Cons:**
- ❌ Less mature Erlang bindings
- ❌ Documentation mostly in Chinese
- ❌ Smaller community
- ❌ Not as widely tested outside Alibaba

**Verdict:** ⭐⭐⭐ **Good but less accessible**

### 3. quinn (Rust, via Rustler)

**Repository:** https://github.com/quinn-rs/quinn
**License:** Apache 2.0 / MIT
**Language:** Rust
**Bindings:** Could use Rustler for Erlang

#### Overview

quinn is a pure Rust QUIC implementation, considered one of the best non-C QUIC libraries.

#### Hypothetical Erlang Integration

```
┌─────────────────────────────────────┐
│ Erlang Application                  │
├─────────────────────────────────────┤
│ quinn_nif (Rustler)                 │
│  - Erlang ↔ Rust bridge            │
├─────────────────────────────────────┤
│ quinn (Rust)                        │
│  - Pure Rust QUIC implementation    │
├─────────────────────────────────────┤
│ tokio (Rust async runtime)          │
└─────────────────────────────────────┘
```

#### Pros & Cons

**Pros:**
- ✅ Pure Rust (memory safe)
- ✅ Excellent performance
- ✅ Active development
- ✅ Clean API

**Cons:**
- ❌ No official Erlang bindings
- ❌ Would need to build Rustler wrapper
- ❌ Rust async runtime complexity
- ❌ Additional development effort

**Verdict:** ⭐⭐ **Interesting but requires work**

### 4. Pure Erlang QUIC (Hypothetical)

**Status:** Doesn't exist
**Effort:** 6-12 months of development

#### Why Pure Erlang?

**Pros:**
- ✅ No NIFs (easier deployment)
- ✅ Full control over implementation
- ✅ Could optimize for BEAM semantics
- ✅ Easier debugging

**Cons:**
- ❌ Huge development effort
- ❌ Likely slower than C/Rust
- ❌ Hard to match performance of tuned C libs
- ❌ Cryptography still needs NIFs

#### Feasibility Analysis

```
Components needed:
1. UDP socket handling         ✅ (gen_udp)
2. TLS 1.3 implementation     ⚠️  (ssl app, but need low-level access)
3. Packet parsing             ✅ (binary pattern matching)
4. Connection state machine   ✅ (gen_statem)
5. Stream multiplexing        ✅ (processes)
6. Flow control               ✅ (credits/backpressure)
7. Congestion control         ✅ (algorithms in Erlang)
8. Loss detection             ✅ (timers + state)
9. 0-RTT resumption          ⚠️  (needs crypto primitives)

Estimated effort: 3-6 person-months for basic implementation
                 12+ person-months for production-ready
```

**Verdict:** ⭐ **Not practical for Phase 1**

### Library Comparison Matrix

| Feature                  | quicer | xquic | quinn | Pure Erlang |
|--------------------------|--------|-------|-------|-------------|
| **Maturity**            | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐ | ❌ |
| **Erlang Integration**  | ⭐⭐⭐⭐⭐ | ⭐⭐ | ❌ | ⭐⭐⭐⭐⭐ |
| **Performance**         | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |
| **Documentation**       | ⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐⭐⭐ | N/A |
| **Production Ready**    | ✅ | ✅ | ✅ | ❌ |
| **Cross-platform**      | ✅ | ✅ | ✅ | ✅ |
| **NIF Required**        | Yes | Yes | Yes | No |
| **Development Effort**  | Low | Medium | High | Very High |
| **Community Support**   | Strong | Moderate | Strong | N/A |

### Recommendation: quicer

For Macula Mesh Phase 1, **quicer is the clear choice**:

1. **Production-ready**: Used in EMQX with millions of connections
2. **Well-documented**: Good examples and API docs
3. **Actively maintained**: Regular updates, responsive maintainers
4. **Erlang-native**: Designed for BEAM from the ground up
5. **MsQuic backend**: Battle-tested in Microsoft services

**Migration Path:**
- Phase 1: Use quicer (proven, fast path to PoC)
- Phase 2: Optimize (profile, tune, maybe contribute improvements)
- Phase 3: Evaluate alternatives (if needed, could switch to pure Erlang or Rust)

---

## Architecture Overview

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    Macula Mesh Network                  │
│                                                         │
│  ┌────────┐      ┌────────┐      ┌────────┐           │
│  │ Node A │◄────►│ Node B │◄────►│ Node C │           │
│  │        │      │        │      │        │           │
│  └────┬───┘      └────┬───┘      └────┬───┘           │
│       │               │               │                │
│       └───────────────┼───────────────┘                │
│                       │                                │
│                  HTTP/3 (QUIC)                         │
│              (Encrypted, Multiplexed)                  │
└─────────────────────────────────────────────────────────┘
```

### Layered Architecture

```
┌─────────────────────────────────────────────────────────┐
│ Layer 5: Application                                    │
│  • Elixir/Erlang Applications                          │
│  • Standard distributed Erlang API                     │
│  • spawn/2, send/2, monitor/2, etc.                   │
└─────────────────────────────────────────────────────────┘
              ↓ (transparent to application)
┌─────────────────────────────────────────────────────────┐
│ Layer 4: WAMP Protocol (Optional Compatibility)        │
│  • publish(Topic, Data)                                │
│  • subscribe(Topic, Handler)                           │
│  • call(Procedure, Args)                               │
│  • register(Procedure, Handler)                        │
└─────────────────────────────────────────────────────────┘
              ↓
┌─────────────────────────────────────────────────────────┐
│ Layer 3: Mesh Routing                                  │
│  • Node discovery (bootstrap, mDNS, DHT)               │
│  • Membership management (SWIM gossip)                 │
│  • Topology management (k-regular graph)               │
│  • Message routing (DHT-based, O(log n) hops)         │
└─────────────────────────────────────────────────────────┘
              ↓
┌─────────────────────────────────────────────────────────┐
│ Layer 2: Macula Distribution Protocol                  │
│  • Message framing (wire protocol)                     │
│  • Handshake and authentication                        │
│  • Process messaging (SEND, LINK, MONITOR, etc.)      │
│  • Stream multiplexing (process ↔ stream mapping)     │
└─────────────────────────────────────────────────────────┘
              ↓
┌─────────────────────────────────────────────────────────┐
│ Layer 1: QUIC Transport (HTTP/3)                       │
│  • quicer (Erlang NIF)                                 │
│  • MsQuic (C library)                                  │
│  • UDP sockets                                         │
│  • TLS 1.3 encryption                                  │
│  • NAT traversal (STUN/ICE)                           │
└─────────────────────────────────────────────────────────┘
```

### Component Architecture

```
┌─────────────────────────────────────────────────────────┐
│ Macula Node (Erlang/OTP Application)                   │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  ┌──────────────────────────────────────────────────┐  │
│  │ Supervision Tree                                 │  │
│  │                                                  │  │
│  │  macula_sup (top-level supervisor)              │  │
│  │    ├─ macula_connection_sup (connections)       │  │
│  │    ├─ macula_membership (SWIM gossip)           │  │
│  │    ├─ macula_topology (neighbor management)     │  │
│  │    ├─ macula_routing (DHT routing)              │  │
│  │    ├─ macula_discovery (node discovery)         │  │
│  │    ├─ macula_pubsub (pub/sub registry)          │  │
│  │    └─ macula_dist (distribution driver)         │  │
│  └──────────────────────────────────────────────────┘  │
│                                                         │
│  ┌──────────────────────────────────────────────────┐  │
│  │ Connection Pool (one per remote node)            │  │
│  │                                                  │  │
│  │  connection_1 (QUIC, 5 streams active)          │  │
│  │  connection_2 (QUIC, 3 streams active)          │  │
│  │  connection_3 (QUIC, 8 streams active)          │  │
│  └──────────────────────────────────────────────────┘  │
│                                                         │
│  ┌──────────────────────────────────────────────────┐  │
│  │ ETS Tables (shared state)                        │  │
│  │                                                  │  │
│  │  - membership_table (alive nodes)                │  │
│  │  - routing_table (DHT entries)                   │  │
│  │  - stream_registry (stream_id → pid)            │  │
│  │  - subscription_registry (topic → [pids])       │  │
│  └──────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────┘
```

---

## Detailed Roadmap

### Phase 1: Foundation (Weeks 1-4)

**Goal:** Prove HTTP/3 Can Carry Erlang Distribution Traffic

#### Week 1-2: QUIC Transport Layer

**Objectives:**
1. Set up quicer dependency
2. Create basic QUIC server/client
3. Implement bidirectional streaming
4. Build connection management

**Deliverables:**

```erlang
%% File: macula_quic_echo.erl
%% Simple QUIC echo server/client to validate transport

-module(macula_quic_echo).
-export([start_server/1, start_client/2]).

start_server(Port) ->
    %% Generate self-signed cert for testing
    {Cert, Key} = macula_cert:generate_self_signed("localhost"),

    ListenOpts = #{
        cert => Cert,
        key => Key,
        alpn => ["macula/1.0"],
        idle_timeout_ms => 10000,
        peer_unidi_stream_count => 10,
        peer_bidi_stream_count => 100
    },

    {ok, Listener} = quicer:listen(Port, ListenOpts),
    io:format("QUIC server listening on port ~p~n", [Port]),

    accept_loop(Listener).

accept_loop(Listener) ->
    case quicer:accept(Listener, [], infinity) of
        {ok, Conn} ->
            %% Complete handshake
            {ok, Conn} = quicer:handshake(Conn),

            %% Get peer info
            {ok, PeerAddr} = quicer:getopt(Conn, peer_addr),
            io:format("Accepted connection from ~p~n", [PeerAddr]),

            %% Spawn connection handler
            spawn_link(fun() -> handle_connection(Conn) end),

            %% Continue accepting
            accept_loop(Listener);
        {error, Reason} ->
            io:format("Accept error: ~p~n", [Reason]),
            timer:sleep(1000),
            accept_loop(Listener)
    end.

handle_connection(Conn) ->
    %% Accept streams
    stream_accept_loop(Conn).

stream_accept_loop(Conn) ->
    case quicer:accept_stream(Conn, []) of
        {ok, Stream} ->
            %% Spawn stream handler
            spawn_link(fun() -> echo_stream(Stream) end),
            stream_accept_loop(Conn);
        {error, closed} ->
            io:format("Connection closed~n");
        {error, Reason} ->
            io:format("Stream accept error: ~p~n", [Reason])
    end.

echo_stream(Stream) ->
    case quicer:recv(Stream, 0) of
        {ok, Data} ->
            io:format("Received: ~p~n", [Data]),
            quicer:send(Stream, Data),
            echo_stream(Stream);
        {error, closed} ->
            quicer:close_stream(Stream);
        {error, Reason} ->
            io:format("Recv error: ~p~n", [Reason]),
            quicer:close_stream(Stream)
    end.

start_client(Host, Port) ->
    ConnOpts = #{
        alpn => ["macula/1.0"],
        verify => verify_none  %% For testing only!
    },

    io:format("Connecting to ~s:~p~n", [Host, Port]),
    {ok, Conn} = quicer:connect(Host, Port, ConnOpts, 5000),

    %% Open stream
    {ok, Stream} = quicer:start_stream(Conn, #{active => false}),

    %% Send message
    Message = <<"Hello, QUIC!">>,
    ok = quicer:send(Stream, Message),
    io:format("Sent: ~p~n", [Message]),

    %% Receive echo
    {ok, Echo} = quicer:recv(Stream, 0),
    io:format("Received echo: ~p~n", [Echo]),

    %% Close
    quicer:close_stream(Stream),
    quicer:close_connection(Conn).
```

**Testing:**
```bash
# Terminal 1: Start server
$ erl -pa _build/default/lib/*/ebin
1> macula_quic_echo:start_server(4433).

# Terminal 2: Connect client
$ erl -pa _build/default/lib/*/ebin
1> macula_quic_echo:start_client("localhost", 4433).
```

**Success Criteria:**
- ✅ Server accepts QUIC connections
- ✅ Client can connect and exchange data
- ✅ Multiple streams work concurrently
- ✅ Connection survives stream closure

---

#### Week 3: Message Framing Protocol

**Objectives:**
1. Design Macula wire protocol v1
2. Implement message encoding/decoding
3. Define message types for distribution

**Wire Protocol Specification:**

```
Macula Wire Protocol v1
=======================

All integers are big-endian.

Packet Format:
┌──────┬──────┬───────┬────────┬─────────┐
│ Ver  │ Type │ Flags │ Length │ Payload │
│ (1B) │ (1B) │ (2B)  │ (4B)   │ (N B)   │
└──────┴──────┴───────┴────────┴─────────┘

Version (1 byte):
  - 0x01: Version 1

Type (1 byte):
  - 0x01: HANDSHAKE
  - 0x02: HEARTBEAT
  - 0x03: SEND           (send message to process)
  - 0x04: REG_SEND       (send to registered name)
  - 0x05: EXIT           (process exit signal)
  - 0x06: LINK           (link processes)
  - 0x07: UNLINK         (unlink processes)
  - 0x08: MONITOR        (monitor process)
  - 0x09: DEMONITOR      (demonitor)
  - 0x0A: GROUP_LEADER   (group leader operations)
  - 0x0B: RPC            (remote procedure call)
  - 0x0C: SPAWN_REQUEST  (spawn on remote node)
  - 0x0D: SPAWN_REPLY    (spawn result)

Flags (2 bytes):
  Bit 0: COMPRESSED (payload is compressed)
  Bit 1: FRAGMENTED (part of fragmented message)
  Bit 2-15: Reserved

Length (4 bytes):
  - Payload length in bytes (max 16 MB)

Payload (N bytes):
  - Message-type specific data
  - Encoded using Erlang External Term Format (EETF)
```

**Implementation:**

```erlang
%% File: macula_protocol.erl
-module(macula_protocol).
-export([encode/2, decode/1]).

%% Protocol version
-define(VERSION, 1).

%% Message types
-define(MSG_HANDSHAKE, 16#01).
-define(MSG_HEARTBEAT, 16#02).
-define(MSG_SEND, 16#03).
-define(MSG_REG_SEND, 16#04).
-define(MSG_EXIT, 16#05).
-define(MSG_LINK, 16#06).
-define(MSG_UNLINK, 16#07).
-define(MSG_MONITOR, 16#08).
-define(MSG_DEMONITOR, 16#09).
-define(MSG_GROUP_LEADER, 16#0A).
-define(MSG_RPC, 16#0B).
-define(MSG_SPAWN_REQUEST, 16#0C).
-define(MSG_SPAWN_REPLY, 16#0D).

%% Flags
-define(FLAG_COMPRESSED, 16#0001).
-define(FLAG_FRAGMENTED, 16#0002).

%% Encode message
encode(Type, Payload) when is_integer(Type), Type >= 0, Type =< 255 ->
    encode(Type, 0, Payload).

encode(Type, Flags, Payload) ->
    %% Serialize payload
    PayloadBin = term_to_binary(Payload, [compressed]),
    Length = byte_size(PayloadBin),

    %% Check size limit (16 MB)
    if
        Length > 16#1000000 ->
            {error, payload_too_large};
        true ->
            %% Build packet
            <<?VERSION:8, Type:8, Flags:16, Length:32, PayloadBin/binary>>
    end.

%% Decode message
decode(<<?VERSION:8, Type:8, Flags:16, Length:32, PayloadBin:Length/binary, Rest/binary>>) ->
    %% Deserialize payload
    Payload = binary_to_term(PayloadBin),

    {ok, #{
        type => Type,
        flags => Flags,
        payload => Payload
    }, Rest};
decode(Bin) when byte_size(Bin) < 8 ->
    {error, insufficient_data};
decode(<<Version:8, _/binary>>) when Version =/= ?VERSION ->
    {error, {unsupported_version, Version}};
decode(_) ->
    {error, invalid_packet}.

%% Helper: Encode SEND message
encode_send(FromPid, ToPid, Message) ->
    encode(?MSG_SEND, {FromPid, ToPid, Message}).

%% Helper: Encode LINK message
encode_link(Pid1, Pid2) ->
    encode(?MSG_LINK, {Pid1, Pid2}).

%% Helper: Encode MONITOR message
encode_monitor(Pid, Ref, MonitoredPid) ->
    encode(?MSG_MONITOR, {Pid, Ref, MonitoredPid}).

%% Helper: Encode SPAWN_REQUEST message
encode_spawn_request(ReqId, Module, Function, Args) ->
    encode(?MSG_SPAWN_REQUEST, {ReqId, Module, Function, Args}).
```

**Handshake Protocol:**

```erlang
%% File: macula_handshake.erl
-module(macula_handshake).
-export([perform/1, accept/1]).

-record(handshake, {
    version = ?MACULA_VERSION,
    node_name :: atom(),
    node_id :: binary(),      %% SHA256(certificate)
    capabilities = [] :: [atom()],
    creation :: integer(),    %% Node start time
    challenge :: binary()     %% Random bytes for auth
}).

%% Initiate handshake (client side)
perform(Conn) ->
    %% Generate our handshake
    Handshake = #handshake{
        node_name = node(),
        node_id = macula_identity:node_id(),
        capabilities = [compression, rpc, monitoring, streams],
        creation = erlang:system_time(millisecond),
        challenge = crypto:strong_rand_bytes(32)
    },

    %% Send handshake
    Packet = macula_protocol:encode(?MSG_HANDSHAKE, Handshake),
    {ok, Stream} = quicer:start_stream(Conn, #{active => false}),
    ok = quicer:send(Stream, Packet),

    %% Receive remote handshake
    {ok, ResponsePacket} = quicer:recv(Stream, 0),
    {ok, #{payload := RemoteHandshake}, _} = macula_protocol:decode(ResponsePacket),

    %% Verify compatibility
    case check_compatibility(Handshake, RemoteHandshake) of
        ok ->
            %% Send acknowledgment
            Ack = macula_protocol:encode(?MSG_HANDSHAKE, {ack, Handshake#handshake.challenge}),
            ok = quicer:send(Stream, Ack),
            {ok, RemoteHandshake};
        {error, Reason} ->
            {error, Reason}
    end.

%% Accept handshake (server side)
accept(Conn) ->
    %% Accept stream
    {ok, Stream} = quicer:accept_stream(Conn, []),

    %% Receive handshake
    {ok, Packet} = quicer:recv(Stream, 0),
    {ok, #{payload := RemoteHandshake}, _} = macula_protocol:decode(Packet),

    %% Generate our handshake
    Handshake = #handshake{
        node_name = node(),
        node_id = macula_identity:node_id(),
        capabilities = [compression, rpc, monitoring, streams],
        creation = erlang:system_time(millisecond),
        challenge = crypto:strong_rand_bytes(32)
    },

    %% Verify compatibility
    case check_compatibility(Handshake, RemoteHandshake) of
        ok ->
            %% Send our handshake
            Response = macula_protocol:encode(?MSG_HANDSHAKE, Handshake),
            ok = quicer:send(Stream, Response),

            %% Wait for acknowledgment
            {ok, AckPacket} = quicer:recv(Stream, 0),
            {ok, #{payload := {ack, Challenge}}, _} = macula_protocol:decode(AckPacket),

            %% Verify challenge
            if
                Challenge =:= Handshake#handshake.challenge ->
                    {ok, RemoteHandshake};
                true ->
                    {error, invalid_challenge}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

check_compatibility(Local, Remote) ->
    %% Check version
    if
        Local#handshake.version =/= Remote#handshake.version ->
            {error, version_mismatch};
        true ->
            %% Check capabilities
            CommonCaps = sets:intersection(
                sets:from_list(Local#handshake.capabilities),
                sets:from_list(Remote#handshake.capabilities)
            ),
            if
                sets:size(CommonCaps) > 0 -> ok;
                true -> {error, no_common_capabilities}
            end
    end.
```

---

#### Week 4: Basic Distribution Protocol

**Objectives:**
1. Implement net_kernel distribution driver
2. Support basic message sending
3. Enable process spawning

**Distribution Driver:**

```erlang
%% File: macula_dist.erl
%% Erlang distribution protocol driver for Macula
%%
%% This module implements the callbacks required by Erlang's net_kernel
%% to use Macula as a custom distribution protocol.

-module(macula_dist).

%% Distribution driver callbacks
-export([
    listen/1,
    accept/1,
    accept_connection/5,
    setup/5,
    close/1,
    select/1,
    is_node_name/1,
    address/0
]).

%% Internal API
-export([
    send/2,
    send/3,
    recv/2
]).

-record(macula_dist_state, {
    listener,      %% QUIC listener
    connection,    %% QUIC connection
    node,          %% Remote node name
    streams = #{}  %% Map: purpose => stream
}).

%% Listen for incoming connections
listen(Name) ->
    %% Extract port from name (e.g., node@host:4433)
    Port = extract_port(Name, 4433),

    %% Generate certificate
    {Cert, Key} = macula_cert:generate_node_cert(Name),

    ListenOpts = #{
        cert => Cert,
        key => Key,
        alpn => ["macula-dist/1.0"],
        peer_bidi_stream_count => 100
    },

    case quicer:listen(Port, ListenOpts) of
        {ok, Listener} ->
            {ok, {Listener, #macula_dist_state{listener = Listener}}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Accept incoming connection
accept(Listen) ->
    Listener = element(1, Listen),
    case quicer:accept(Listener, [], infinity) of
        {ok, Conn} ->
            %% Perform handshake
            case macula_handshake:accept(Conn) of
                {ok, RemoteHandshake} ->
                    RemoteNode = RemoteHandshake#handshake.node_name,
                    State = #macula_dist_state{
                        connection = Conn,
                        node = RemoteNode
                    },
                    {ok, Conn, State};
                {error, Reason} ->
                    quicer:close_connection(Conn),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Accept connection (post-handshake setup)
accept_connection(AcceptPid, DistCtrl, MyNode, Allowed, SetupTime) ->
    %% This is called by net_kernel after accept/1 succeeds
    gen_server:call(DistCtrl, {accept_connection, AcceptPid, MyNode, Allowed, SetupTime}).

%% Setup outgoing connection
setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    %% Extract host and port
    {Host, Port} = parse_node_name(Node),

    %% Connect
    ConnOpts = #{
        alpn => ["macula-dist/1.0"],
        verify => verify_peer
    },

    case quicer:connect(Host, Port, ConnOpts, 5000) of
        {ok, Conn} ->
            %% Perform handshake
            case macula_handshake:perform(Conn) of
                {ok, RemoteHandshake} ->
                    State = #macula_dist_state{
                        connection = Conn,
                        node = Node
                    },
                    {ok, Conn, State};
                {error, Reason} ->
                    quicer:close_connection(Conn),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Close connection
close(Conn) ->
    quicer:close_connection(Conn),
    ok.

%% Select (for epmd compatibility)
select(Node) ->
    %% Macula doesn't use epmd
    {ok, Node}.

%% Check if valid node name
is_node_name(Node) when is_atom(Node) ->
    case atom_to_list(Node) of
        [$@ | _] -> false;
        Name ->
            case string:chr(Name, $@) of
                0 -> false;
                _ -> true
            end
    end;
is_node_name(_) ->
    false.

%% Get address (for net_kernel)
address() ->
    %% Return local node address
    {ok, {0, 0, 0, 0}}.

%% Send message to process on remote node
send(Conn, Pid, Message) ->
    %% Encode SEND message
    Packet = macula_protocol:encode_send(self(), Pid, Message),

    %% Get or create stream for this message
    {ok, Stream} = get_or_create_stream(Conn, control),

    %% Send
    quicer:send(Stream, Packet).

send(Conn, Name, Message) when is_atom(Name) ->
    %% Send to registered name
    Packet = macula_protocol:encode(?MSG_REG_SEND, {self(), Name, Message}),
    {ok, Stream} = get_or_create_stream(Conn, control),
    quicer:send(Stream, Packet).

%% Receive message
recv(Conn, Timeout) ->
    %% Accept next stream with data
    case quicer:accept_stream(Conn, [], Timeout) of
        {ok, Stream} ->
            case quicer:recv(Stream, 0) of
                {ok, Packet} ->
                    {ok, Msg, _} = macula_protocol:decode(Packet),
                    {ok, Msg};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%% Internal functions

get_or_create_stream(Conn, Purpose) ->
    %% Look up existing stream for this purpose
    %% If not found, create new stream
    case ets:lookup(macula_streams, {Conn, Purpose}) of
        [{_, Stream}] ->
            {ok, Stream};
        [] ->
            {ok, Stream} = quicer:start_stream(Conn, #{active => false}),
            ets:insert(macula_streams, {{Conn, Purpose}, Stream}),
            {ok, Stream}
    end.

parse_node_name(Node) when is_atom(Node) ->
    case string:split(atom_to_list(Node), "@") of
        [_Name, HostPort] ->
            case string:split(HostPort, ":") of
                [Host, Port] -> {Host, list_to_integer(Port)};
                [Host] -> {Host, 4433}  %% Default port
            end;
        _ ->
            {error, invalid_node_name}
    end.

extract_port(Name, Default) ->
    case string:split(atom_to_list(Name), ":") of
        [_, Port] -> list_to_integer(Port);
        _ -> Default
    end.
```

**Testing:**

```bash
# Terminal 1: Start first node
$ erl -name node1@localhost:4433 -proto_dist macula -pa _build/default/lib/*/ebin

# Terminal 2: Start second node
$ erl -name node2@localhost:4434 -proto_dist macula -pa _build/default/lib/*/ebin

# In node2:
(node2@localhost:4434)1> net_kernel:connect_node('node1@localhost:4433').
true

(node2@localhost:4434)2> nodes().
['node1@localhost:4433']

(node2@localhost:4434)3> {node1, 'node1@localhost:4433'} ! {hello, from, node2}.
{hello, from, node2}

# In node1, check process mailbox:
(node1@localhost:4433)1> receive Msg -> Msg end.
{hello, from, node2}

# Spawn remote process:
(node2@localhost:4434)4> spawn('node1@localhost:4433', fun() ->
    io:format("Running on ~p!~n", [node()])
end).
<12345.67.0>

# On node1, see output:
Running on 'node1@localhost:4433'!
```

**Success Criteria:**
- ✅ Two nodes can connect via Macula
- ✅ `nodes()` shows connected nodes
- ✅ Messages can be sent between nodes
- ✅ Remote spawning works
- ✅ Basic distributed Erlang operations function

---

### Phase 2: Mesh Topology (Weeks 5-8)

**Goal:** Move Beyond Point-to-Point to Self-Organizing Mesh

#### Week 5-6: Node Discovery and Membership

**Objectives:**
1. Implement multi-strategy node discovery
2. Build SWIM-based membership protocol
3. Handle node joins, leaves, failures

**Node Discovery:**

```erlang
%% File: macula_discovery.erl
-module(macula_discovery).
-behaviour(gen_server).

-export([start_link/0, discover/0, get_known_nodes/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    bootstrap_nodes = [],
    known_nodes = sets:new(),
    discovery_interval = 30000  %% 30 seconds
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

discover() ->
    gen_server:call(?MODULE, discover).

get_known_nodes() ->
    gen_server:call(?MODULE, get_known_nodes).

init([]) ->
    %% Load bootstrap nodes from config
    Bootstrap = application:get_env(macula, bootstrap_nodes, []),

    %% Start discovery timer
    erlang:send_after(1000, self(), discover),

    {ok, #state{bootstrap_nodes = Bootstrap}}.

handle_call(discover, _From, State) ->
    %% Run all discovery strategies in parallel
    Self = self(),
    spawn(fun() -> Self ! {discovered, discover_via_bootstrap(State#state.bootstrap_nodes)} end),
    spawn(fun() -> Self ! {discovered, discover_via_mdns()} end),
    spawn(fun() -> Self ! {discovered, discover_via_dns_srv()} end),

    {reply, ok, State};

handle_call(get_known_nodes, _From, State) ->
    Nodes = sets:to_list(State#state.known_nodes),
    {reply, Nodes, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(discover, State) ->
    %% Trigger discovery
    discover(),

    %% Schedule next discovery
    erlang:send_after(State#state.discovery_interval, self(), discover),
    {noreply, State};

handle_info({discovered, Nodes}, State) ->
    %% Merge discovered nodes
    KnownNodes = lists:foldl(
        fun(Node, Acc) -> sets:add_element(Node, Acc) end,
        State#state.known_nodes,
        Nodes
    ),

    %% Notify membership protocol
    macula_membership:discovered_nodes(Nodes),

    {noreply, State#state{known_nodes = KnownNodes}}.

%%% Discovery Strategies

%% Strategy 1: Bootstrap Nodes
discover_via_bootstrap(Bootstrap) ->
    %% Try to connect to bootstrap nodes
    lists:filtermap(fun(Node) ->
        case macula_connection:ping(Node, 1000) of
            pong -> {true, Node};
            timeout -> false
        end
    end, Bootstrap).

%% Strategy 2: mDNS (local network)
discover_via_mdns() ->
    %% Send mDNS query for _macula._udp.local
    case macula_mdns:discover("_macula._udp.local", 2000) of
        {ok, Nodes} -> Nodes;
        {error, _} -> []
    end.

%% Strategy 3: DNS SRV Records
discover_via_dns_srv() ->
    %% Query DNS SRV for _macula._udp.example.com
    Domain = application:get_env(macula, dns_domain, "macula.local"),
    SRVName = "_macula._udp." ++ Domain,

    case inet_res:lookup(SRVName, in, srv) of
        [] -> [];
        Records ->
            %% Extract host:port from SRV records
            lists:map(fun({_Priority, _Weight, Port, Host}) ->
                list_to_atom(atom_to_list(node()) ++ "@" ++ Host ++ ":" ++ integer_to_list(Port))
            end, Records)
    end.
```

**SWIM Membership Protocol:**

```erlang
%% File: macula_membership.erl
%% SWIM: Scalable Weakly-consistent Infection-style Process Group Membership Protocol
-module(macula_membership).
-behaviour(gen_server).

-export([start_link/0, join/1, leave/0, get_members/0, discovered_nodes/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(member, {
    node_id,
    address,
    state = alive,      %% alive | suspect | dead
    incarnation = 0,    %% For conflict resolution
    last_seen,          %% Timestamp
    metadata = #{}      %% Arbitrary key-value data
}).

-record(state, {
    local_member,
    members = #{},       %% node_id => member
    protocol_period = 1000,  %% 1 second
    suspect_timeout = 5000,  %% 5 seconds
    ping_targets = []
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

join(BootstrapNode) ->
    gen_server:call(?MODULE, {join, BootstrapNode}).

leave() ->
    gen_server:call(?MODULE, leave).

get_members() ->
    gen_server:call(?MODULE, get_members).

discovered_nodes(Nodes) ->
    gen_server:cast(?MODULE, {discovered_nodes, Nodes}).

init([]) ->
    %% Create local member
    Local = #member{
        node_id = macula_identity:node_id(),
        address = macula_identity:address(),
        state = alive,
        incarnation = 0,
        last_seen = erlang:system_time(millisecond),
        metadata = #{
            node_name => node(),
            started_at => erlang:system_time(millisecond)
        }
    },

    %% Start protocol tick
    erlang:send_after(1000, self(), protocol_tick),

    {ok, #state{local_member = Local}}.

handle_call({join, BootstrapNode}, _From, State) ->
    %% Contact bootstrap node and exchange membership
    case macula_connection:connect(BootstrapNode) of
        {ok, Conn} ->
            %% Send join request
            Request = {join, State#state.local_member},
            case macula_rpc:call(Conn, ?MODULE, handle_join, [Request]) of
                {ok, RemoteMembers} ->
                    %% Merge members
                    Members = maps:merge(State#state.members, RemoteMembers),
                    {reply, ok, State#state{members = Members}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_members, _From, State) ->
    Members = maps:values(State#state.members),
    AliveMembers = lists:filter(fun(M) -> M#member.state =:= alive end, Members),
    {reply, AliveMembers, State};

handle_call(leave, _From, State) ->
    %% Broadcast leave message
    Members = maps:values(State#state.members),
    Msg = {leave, State#state.local_member},
    lists:foreach(fun(Member) ->
        macula_connection:send(Member#member.node_id, Msg)
    end, Members),

    {stop, normal, ok, State}.

handle_cast({discovered_nodes, Nodes}, State) ->
    %% Add discovered nodes to members (if not already known)
    NewMembers = lists:foldl(fun(Node, Acc) ->
        NodeId = macula_identity:node_id(Node),
        case maps:is_key(NodeId, Acc) of
            true -> Acc;
            false ->
                Member = #member{
                    node_id = NodeId,
                    address = Node,
                    state = alive,
                    last_seen = erlang:system_time(millisecond)
                },
                maps:put(NodeId, Member, Acc)
        end
    end, State#state.members, Nodes),

    {noreply, State#state{members = NewMembers}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(protocol_tick, State) ->
    %% SWIM protocol tick
    NewState = swim_tick(State),

    %% Schedule next tick
    erlang:send_after(State#state.protocol_period, self(), protocol_tick),

    {noreply, NewState};

handle_info({ping, From, Incarnation}, State) ->
    %% Respond to ping
    macula_connection:send(From, {ack, State#state.local_member#member.incarnation}),
    {noreply, State};

handle_info({ack, _Incarnation}, State) ->
    %% Received ack from ping
    {noreply, State};

handle_info({ping_req, Target, From}, State) ->
    %% Indirect ping request
    case macula_connection:ping(Target, 500) of
        pong ->
            macula_connection:send(From, {ping_req_ack, Target});
        timeout ->
            macula_connection:send(From, {ping_req_timeout, Target})
    end,
    {noreply, State}.

%%% SWIM Protocol Implementation

swim_tick(State) ->
    %% 1. Select random member to ping
    case select_random_member(State#state.members) of
        {ok, Target} ->
            case direct_ping(Target) of
                pong ->
                    %% Update last_seen
                    update_member_state(Target#member.node_id, alive, State);
                timeout ->
                    %% Try indirect ping via other members
                    case indirect_ping(Target, State) of
                        ok ->
                            update_member_state(Target#member.node_id, alive, State);
                        failed ->
                            %% Mark as suspect
                            State1 = update_member_state(Target#member.node_id, suspect, State),
                            %% Schedule suspicion timeout
                            erlang:send_after(State#state.suspect_timeout, self(),
                                {suspect_timeout, Target#member.node_id}),
                            State1
                    end
            end;
        error ->
            State
    end,

    %% 2. Gossip membership changes
    gossip_changes(State),

    State.

direct_ping(Target) ->
    case macula_connection:ping(Target#member.node_id, 1000) of
        pong -> pong;
        _ -> timeout
    end.

indirect_ping(Target, State) ->
    %% Select K random members for indirect ping
    K = 3,
    Members = maps:values(State#state.members),
    Proxies = select_random_n(Members, K),

    %% Send ping_req to proxies
    Ref = make_ref(),
    lists:foreach(fun(Proxy) ->
        macula_connection:send(Proxy#member.node_id, {ping_req, Target, self(), Ref})
    end, Proxies),

    %% Wait for responses
    wait_for_ping_req_responses(Ref, K, 2000).

wait_for_ping_req_responses(_Ref, 0, _Timeout) ->
    failed;
wait_for_ping_req_responses(Ref, Remaining, Timeout) ->
    receive
        {ping_req_ack, _Target, Ref} ->
            ok;
        {ping_req_timeout, _Target, Ref} ->
            wait_for_ping_req_responses(Ref, Remaining - 1, Timeout)
    after Timeout ->
        failed
    end.

gossip_changes(State) ->
    %% Select random peers for gossip
    Members = maps:values(State#state.members),
    GossipTargets = select_random_n(Members, 3),

    %% Get recent changes
    Changes = get_recent_changes(State),

    %% Send to targets
    lists:foreach(fun(Target) ->
        macula_connection:send(Target#member.node_id, {gossip, Changes})
    end, GossipTargets).

get_recent_changes(State) ->
    %% Get members that changed state recently (last 10 seconds)
    Now = erlang:system_time(millisecond),
    RecentWindow = 10000,

    maps:filter(fun(_NodeId, Member) ->
        (Now - Member#member.last_seen) < RecentWindow
    end, State#state.members).

update_member_state(NodeId, NewState, State) ->
    case maps:find(NodeId, State#state.members) of
        {ok, Member} ->
            UpdatedMember = Member#member{
                state = NewState,
                last_seen = erlang:system_time(millisecond)
            },
            Members = maps:put(NodeId, UpdatedMember, State#state.members),
            State#state{members = Members};
        error ->
            State
    end.

select_random_member(Members) when map_size(Members) > 0 ->
    List = maps:values(Members),
    {ok, lists:nth(rand:uniform(length(List)), List)};
select_random_member(_) ->
    error.

select_random_n(List, N) when length(List) =< N ->
    List;
select_random_n(List, N) ->
    %% Shuffle and take N
    Shuffled = [X || {_, X} <- lists:sort([{rand:uniform(), E} || E <- List])],
    lists:sublist(Shuffled, N).
```

---

#### Week 7: Topology Management and Routing

**Objectives:**
1. Implement k-regular graph topology
2. Build DHT-based routing
3. Optimize for low diameter

**Topology Manager:**

```erlang
%% File: macula_topology.erl
%% Manages connection topology using k-regular graph
%% Each node maintains K connections to neighbors on consistent hash ring

-module(macula_topology).
-behaviour(gen_server).

-export([start_link/0, maintain/0, get_neighbors/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(K_NEIGHBORS, 6).  %% Number of neighbors to maintain

-record(state, {
    neighbors = [],         %% Current neighbor connections
    desired_neighbors = [], %% Neighbors we should connect to
    ring_position          %% Our position on hash ring
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

maintain() ->
    gen_server:call(?MODULE, maintain).

get_neighbors() ->
    gen_server:call(?MODULE, get_neighbors).

init([]) ->
    %% Calculate our position on hash ring
    NodeId = macula_identity:node_id(),
    Position = crypto:hash(sha256, NodeId),

    %% Start maintenance timer
    erlang:send_after(5000, self(), maintain),

    {ok, #state{ring_position = Position}}.

handle_call(get_neighbors, _From, State) ->
    {reply, State#state.neighbors, State};

handle_call(maintain, _From, State) ->
    NewState = maintain_topology(State),
    {reply, ok, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(maintain, State) ->
    NewState = maintain_topology(State),
    erlang:send_after(5000, self(), maintain),
    {noreply, NewState}.

maintain_topology(State) ->
    %% Get all known members
    Members = macula_membership:get_members(),

    %% Select desired neighbors using consistent hashing
    DesiredNeighbors = select_neighbors(Members, ?K_NEIGHBORS, State#state.ring_position),

    %% Current connections
    CurrentNeighbors = State#state.neighbors,

    %% Find missing connections
    Missing = DesiredNeighbors -- CurrentNeighbors,

    %% Find extra connections (if we're over K neighbors)
    Extra = CurrentNeighbors -- DesiredNeighbors,

    %% Connect to missing
    lists:foreach(fun(NodeId) ->
        case macula_connection:connect(NodeId) of
            {ok, _Conn} ->
                io:format("Connected to neighbor: ~p~n", [NodeId]);
            {error, Reason} ->
                io:format("Failed to connect to ~p: ~p~n", [NodeId, Reason])
        end
    end, Missing),

    %% Disconnect extra (if too many connections)
    if
        length(CurrentNeighbors) > ?K_NEIGHBORS * 2 ->
            lists:foreach(fun(NodeId) ->
                macula_connection:disconnect(NodeId)
            end, Extra);
        true ->
            ok
    end,

    %% Update state
    NewNeighbors = (CurrentNeighbors ++ Missing) -- Extra,
    State#state{
        neighbors = NewNeighbors,
        desired_neighbors = DesiredNeighbors
    }.

select_neighbors(Members, K, MyPosition) ->
    %% Place all members on hash ring
    Ring = lists:map(fun(Member) ->
        NodeId = Member#member.node_id,
        Position = crypto:hash(sha256, NodeId),
        {Position, NodeId}
    end, Members),

    %% Sort by position
    SortedRing = lists:sort(Ring),

    %% Find our position
    MyIndex = find_position(MyPosition, SortedRing),

    %% Select K clockwise neighbors (for redundancy, select K/2 clockwise + K/2 counter-clockwise)
    ClockwiseCount = K div 2,
    CounterClockwiseCount = K - ClockwiseCount,

    Clockwise = select_clockwise(MyIndex, ClockwiseCount, SortedRing),
    CounterClockwise = select_counter_clockwise(MyIndex, CounterClockwiseCount, SortedRing),

    Clockwise ++ CounterClockwise.

find_position(MyPosition, Ring) ->
    find_position(MyPosition, Ring, 0).

find_position(_MyPosition, [], _Index) ->
    0;
find_position(MyPosition, [{Position, _NodeId} | _Rest], Index) when Position >= MyPosition ->
    Index;
find_position(MyPosition, [_H | Rest], Index) ->
    find_position(MyPosition, Rest, Index + 1).

select_clockwise(MyIndex, Count, Ring) ->
    RingSize = length(Ring),
    Indices = [(MyIndex + I) rem RingSize || I <- lists:seq(1, Count)],
    [NodeId || {Idx, {_Pos, NodeId}} <- lists:zip(Indices, Ring), Idx =:= element(1, lists:nth(Idx + 1, lists:zip(lists:seq(0, RingSize - 1), Ring)))].

select_counter_clockwise(MyIndex, Count, Ring) ->
    RingSize = length(Ring),
    Indices = [(MyIndex - I + RingSize) rem RingSize || I <- lists:seq(1, Count)],
    [NodeId || {Idx, {_Pos, NodeId}} <- lists:zip(Indices, Ring), Idx =:= element(1, lists:nth(Idx + 1, lists:zip(lists:seq(0, RingSize - 1), Ring)))].
```

**DHT Routing:**

```erlang
%% File: macula_routing.erl
%% Kademlia-inspired DHT routing for mesh

-module(macula_routing).
-export([route/2, find_node/1, find_closest_nodes/2]).

-define(K, 20).  %% Replication factor
-define(ALPHA, 3).  %% Concurrency parameter

%% Route message to destination node
route(DestNodeId, Message) ->
    case macula_connection:is_connected(DestNodeId) of
        true ->
            %% Direct connection, send immediately
            macula_connection:send(DestNodeId, Message);
        false ->
            %% Find next hop via DHT
            NextHop = find_next_hop(DestNodeId),
            forward(NextHop, DestNodeId, Message)
    end.

%% Find next hop closer to destination
find_next_hop(DestNodeId) ->
    MyNodeId = macula_identity:node_id(),

    %% Get connected neighbors
    Neighbors = macula_topology:get_neighbors(),

    %% Calculate XOR distance from each neighbor to destination
    Distances = lists:map(fun(NeighborId) ->
        Dist = xor_distance(NeighborId, DestNodeId),
        {Dist, NeighborId}
    end, Neighbors),

    %% Sort by distance (closest first)
    Sorted = lists:sort(Distances),

    %% Return closest neighbor
    case Sorted of
        [{_Dist, NextHop} | _] ->
            %% Check if NextHop is closer than us
            MyDist = xor_distance(MyNodeId, DestNodeId),
            if
                _Dist < MyDist -> NextHop;
                true -> DestNodeId  %% We're closest, destination must be dead
            end;
        [] ->
            %% No neighbors, can't route
            {error, no_route}
    end.

%% Forward message to next hop
forward(NextHop, FinalDest, Message) ->
    ForwardMsg = {forward, FinalDest, Message},
    macula_connection:send(NextHop, ForwardMsg).

%% XOR distance metric (like Kademlia)
xor_distance(A, B) when is_binary(A), is_binary(B) ->
    crypto:bytes_to_integer(crypto:exor(A, B));
xor_distance(A, B) ->
    xor_distance(term_to_binary(A), term_to_binary(B)).

%% Find node by ID (iterative lookup)
find_node(TargetId) ->
    find_node(TargetId, [], [macula_identity:node_id()]).

find_node(TargetId, Queried, Closest) ->
    %% Select ALPHA closest unqueried nodes
    ToQuery = select_unqueried(Closest, Queried, ?ALPHA),

    case ToQuery of
        [] ->
            %% No more nodes to query, return closest
            {ok, lists:sublist(Closest, ?K)};
        _ ->
            %% Query nodes in parallel
            Results = query_nodes(ToQuery, TargetId),

            %% Merge results
            NewClosest = merge_and_sort(Closest, Results, TargetId),
            NewQueried = Queried ++ ToQuery,

            %% Check if we found target
            case lists:member(TargetId, NewClosest) of
                true -> {ok, TargetId};
                false -> find_node(TargetId, NewQueried, NewClosest)
            end
    end.

query_nodes(Nodes, TargetId) ->
    %% Query each node for closer nodes
    lists:flatmap(fun(NodeId) ->
        case macula_rpc:call(NodeId, ?MODULE, find_closest_nodes, [TargetId, ?K], 1000) of
            {ok, Nodes} -> Nodes;
            {error, _} -> []
        end
    end, Nodes).

find_closest_nodes(TargetId, K) ->
    %% Return K closest known nodes to TargetId
    Members = macula_membership:get_members(),
    Distances = [{xor_distance(M#member.node_id, TargetId), M#member.node_id} || M <- Members],
    Sorted = lists:sort(Distances),
    {ok, [NodeId || {_Dist, NodeId} <- lists:sublist(Sorted, K)]}.

select_unqueried(Closest, Queried, Alpha) ->
    Unqueried = Closest -- Queried,
    lists:sublist(Unqueried, Alpha).

merge_and_sort(Closest, New, TargetId) ->
    All = lists:usort(Closest ++ New),
    Distances = [{xor_distance(NodeId, TargetId), NodeId} || NodeId <- All],
    Sorted = lists:sort(Distances),
    [NodeId || {_Dist, NodeId} <- Sorted].
```

---

(Continuing in next section due to length...)

### Phase 3: NAT Traversal (Weeks 9-12)

### Phase 4: WAMP Layer (Weeks 13-16)

### Phase 5: Production Hardening (Weeks 17-20)

---

## Architecture Diagrams

### 1. System Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                         Macula Mesh Network                         │
│                                                                     │
│  Internet / WAN                                                     │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │                                                              │  │
│  │  ┌─────────┐          ┌─────────┐          ┌─────────┐     │  │
│  │  │ Node A  │◄────────►│ Node B  │◄────────►│ Node C  │     │  │
│  │  │ (USA)   │          │ (Europe)│          │ (Asia)  │     │  │
│  │  └────┬────┘          └────┬────┘          └────┬────┘     │  │
│  │       │                    │                    │          │  │
│  │       └────────────────────┼────────────────────┘          │  │
│  │                            │                                │  │
│  │                   HTTP/3 (QUIC/UDP)                         │  │
│  │                     Port 443/UDP                            │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                     │
│  NAT/Firewall Traversal:                                           │
│  • STUN for public address discovery                               │
│  • ICE for connectivity checks                                     │
│  • UDP hole punching                                               │
│  • TURN relay as fallback                                          │
└─────────────────────────────────────────────────────────────────────┘
```

### 2. Node Internal Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ Macula Node (BEAM VM)                                               │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌────────────────────────────────────────────────────────────┐    │
│  │ Application Layer                                          │    │
│  │  • Elixir/Erlang Apps                                      │    │
│  │  • spawn/2, send/2, monitor/2                             │    │
│  │  • Transparent distribution                                │    │
│  └────────────────────────────────────────────────────────────┘    │
│                             ↓                                       │
│  ┌────────────────────────────────────────────────────────────┐    │
│  │ WAMP Compatibility Layer (Optional)                        │    │
│  │  • publish/subscribe                                       │    │
│  │  • call/register (RPC)                                     │    │
│  └────────────────────────────────────────────────────────────┘    │
│                             ↓                                       │
│  ┌────────────────────────────────────────────────────────────┐    │
│  │ Mesh Services                                              │    │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐    │    │
│  │  │ Discovery    │  │ Membership   │  │ Topology     │    │    │
│  │  │ (Bootstrap,  │  │ (SWIM        │  │ (k-regular   │    │    │
│  │  │  mDNS, DNS)  │  │  Gossip)     │  │  graph)      │    │    │
│  │  └──────────────┘  └──────────────┘  └──────────────┘    │    │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐    │    │
│  │  │ Routing      │  │ Pub/Sub      │  │ RPC          │    │    │
│  │  │ (DHT,        │  │ (Topic-based │  │ (Request/    │    │    │
│  │  │  Kademlia)   │  │  Registry)   │  │  Response)   │    │    │
│  │  └──────────────┘  └──────────────┘  └──────────────┘    │    │
│  └────────────────────────────────────────────────────────────┘    │
│                             ↓                                       │
│  ┌────────────────────────────────────────────────────────────┐    │
│  │ Macula Distribution Protocol                               │    │
│  │  • Message framing (wire protocol)                         │    │
│  │  • Handshake & authentication                              │    │
│  │  • Stream multiplexing                                     │    │
│  │  • Process ↔ Stream mapping                               │    │
│  └────────────────────────────────────────────────────────────┘    │
│                             ↓                                       │
│  ┌────────────────────────────────────────────────────────────┐    │
│  │ QUIC Transport (via quicer NIF)                            │    │
│  │  ┌──────────────────────────────────────────────────┐     │    │
│  │  │ MsQuic (C library)                                │     │    │
│  │  │  • RFC 9000 QUIC implementation                   │     │    │
│  │  │  • TLS 1.3 integrated                             │     │    │
│  │  │  • Streams, flow control, congestion control      │     │    │
│  │  └──────────────────────────────────────────────────┘     │    │
│  └────────────────────────────────────────────────────────────┘    │
│                             ↓                                       │
│  ┌────────────────────────────────────────────────────────────┐    │
│  │ UDP Sockets (OS Network Stack)                             │    │
│  └────────────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────────────┘
```

### 3. Message Flow Diagram

```
Process A (Node 1)                                  Process B (Node 2)
      │                                                    │
      │ Pid ! Message                                     │
      ├──────────────────────┐                            │
      │                      │                            │
      ▼                      │                            │
 macula_dist                 │                            │
      │                      │                            │
      │ encode_send()        │                            │
      ├─────────────┐        │                            │
      │             │        │                            │
      ▼             │        │                            │
 macula_protocol    │        │                            │
      │             │        │                            │
      │ Frame:      │        │                            │
      │ [Ver|Type|  │        │                            │
      │  Flags|Len| │        │                            │
      │  Payload]   │        │                            │
      ├─────────────┘        │                            │
      │                      │                            │
      ▼                      │                            │
 macula_connection           │                            │
      │                      │                            │
      │ Get/Create Stream    │                            │
      ├─────────────┐        │                            │
      │             │        │                            │
      ▼             │        │                            │
 quicer (NIF)       │        │                            │
      │             │        │                            │
      │ quicer:send()        │                            │
      ├─────────────┘        │                            │
      │                      │                            │
      ▼                      │                            │
   MsQuic (C)                │                            │
      │                      │                            │
      │ QUIC Packet          │                            │
      │ (encrypted)          │                            │
      ├──────────────────────┘                            │
      │                                                   │
      ▼                                                   │
   UDP Socket                                             │
      │                                                   │
      │ ═════════════════════════════════════════════►   │
      │           Network (Internet)                     │
      │                                                  │
      │                                            UDP Socket
      │                                                  │
      │                                                  ▼
      │                                             MsQuic (C)
      │                                                  │
      │                                                  │ Decrypt
      │                                                  │ Reassemble
      │                                                  ├─────────┐
      │                                                  │         │
      │                                                  ▼         │
      │                                            quicer (NIF)    │
      │                                                  │         │
      │                                                  │ quicer:recv()
      │                                                  ├─────────┘
      │                                                  │
      │                                                  ▼
      │                                         macula_connection
      │                                                  │
      │                                                  │ Stream → Pid lookup
      │                                                  ├─────────┐
      │                                                  │         │
      │                                                  ▼         │
      │                                          macula_protocol   │
      │                                                  │         │
      │                                                  │ decode()│
      │                                                  ├─────────┘
      │                                                  │
      │                                                  ▼
      │                                            macula_dist
      │                                                  │
      │                                                  │ Deliver to process
      │                                                  ├────────┐
      │                                                  │        │
      │                                                  ▼        │
      │                                            Process B      │
      │                                                  │        │
      │                                                  │ receive │
      │                                                  │   Message
      │                                                  ├────────┘
      │                                                  │
      │                                                  ▼
```

### 4. Mesh Topology Diagram

```
┌──────────────────────────────────────────────────────────────────┐
│ Consistent Hash Ring (k-regular graph, k=6)                     │
│                                                                  │
│                          Node 3                                 │
│                            ●                                    │
│                       ╱         ╲                               │
│                  ╱                  ╲                            │
│             ╱                           ╲                        │
│        ╱                                    ╲                    │
│   Node 2 ●─────────────────────────────●  Node 4               │
│       │   ╲                         ╱   │                       │
│       │       ╲                 ╱       │                       │
│       │           ╲         ╱           │                       │
│       │               ● Node 5          │                       │
│       │           ╱         ╲           │                       │
│       │       ╱                 ╲       │                       │
│       │   ╱                         ╲   │                       │
│   Node 1 ●─────────────────────────────● Node 6                │
│        ╲                                    ╱                    │
│             ╲                           ╱                        │
│                  ╲                  ╱                            │
│                       ╲         ╱                               │
│                            ●                                    │
│                        Node 7                                   │
│                                                                  │
│  Each node connects to K neighbors (K=6 in this example)       │
│  • K/2 clockwise neighbors (3)                                 │
│  • K/2 counter-clockwise neighbors (3)                         │
│                                                                  │
│  Properties:                                                    │
│  • Low diameter (O(log n) hops between any two nodes)         │
│  • High fault tolerance (multiple paths)                       │
│  • Scalable (each node has fixed K connections)               │
└──────────────────────────────────────────────────────────────────┘
```

### 5. NAT Traversal Flow

```
┌──────────────────────────────────────────────────────────────────┐
│ NAT Traversal Using STUN + ICE                                   │
│                                                                  │
│  Node A (behind NAT)          STUN Server         Node B (public)│
│       │                            │                      │      │
│       │  1. STUN Binding Request   │                      │      │
│       ├───────────────────────────►│                      │      │
│       │                            │                      │      │
│       │  2. STUN Response          │                      │      │
│       │     (Public IP: 1.2.3.4)   │                      │      │
│       │◄───────────────────────────┤                      │      │
│       │                            │                      │      │
│       │  3. Register with Signaling Server               │      │
│       │     POST /register                                │      │
│       │     { candidates: [                               │      │
│       │       {type: "host", addr: "192.168.1.100"},      │      │
│       │       {type: "srflx", addr: "1.2.3.4"}            │      │
│       │     ]}                                             │      │
│       ├───────────────────────────────────────────────────►│      │
│       │                                                    │      │
│       │  4. Query for Node B candidates                   │      │
│       │     GET /lookup/node_b                            │      │
│       ├───────────────────────────────────────────────────►│      │
│       │                                                    │      │
│       │  5. Receive Node B candidates                     │      │
│       │     { candidates: [{type: "host", addr: "5.6.7.8"}]}    │
│       │◄───────────────────────────────────────────────────┤      │
│       │                                                    │      │
│       │  6. Connectivity Checks (ICE)                     │      │
│       │     Send STUN probes to all candidate pairs       │      │
│       ├───────────────────────────────────────────────────►│      │
│       │◄───────────────────────────────────────────────────┤      │
│       │                                                    │      │
│       │  7. Select best candidate pair                    │      │
│       │     (Direct: 1.2.3.4 ↔ 5.6.7.8)                  │      │
│       │                                                    │      │
│       │  8. Establish QUIC Connection                     │      │
│       │═══════════════════════════════════════════════════►│      │
│       │                                                    │      │
│       │  9. Communication over QUIC/UDP                   │      │
│       │◄══════════════════════════════════════════════════►│      │
│       │                                                    │      │
└──────────────────────────────────────────────────────────────────┘
```

### 6. Development Roadmap Gantt Chart

```
Week  Phase   Milestone
────────────────────────────────────────────────────────────────────
 1-2  │■■■■■■│ QUIC Transport Layer
      │      │ • quicer integration
      │      │ • Basic client/server
      │      │ • Bidirectional streams
────────────────────────────────────────────────────────────────────
 3    │  ■■■ │ Message Framing Protocol
      │      │ • Wire protocol spec
      │      │ • Encode/decode
      │      │ • Handshake
────────────────────────────────────────────────────────────────────
 4    │   ■■■│ Distribution Protocol
      │      │ • net_kernel driver
      │      │ • Basic messaging
      │      │ • Remote spawn
────────────────────────────────────────────────────────────────────
 5-6  │    ■■■■■│ Node Discovery
      │        │ • Bootstrap
      │        │ • mDNS
      │        │ • SWIM membership
────────────────────────────────────────────────────────────────────
 7    │       ■■■│ Topology & Routing
      │          │ • k-regular graph
      │          │ • DHT routing
────────────────────────────────────────────────────────────────────
 8    │        ■■│ Testing & Validation
      │          │ • Chaos testing
      │          │ • Benchmarks
────────────────────────────────────────────────────────────────────
 9-10 │         ■■■■│ NAT Traversal
      │             │ • STUN client
      │             │ • ICE implementation
────────────────────────────────────────────────────────────────────
11-12 │           ■■■■│ Hole Punching
      │               │ • UDP hole punch
      │               │ • TURN relay
────────────────────────────────────────────────────────────────────
13-14 │             ■■■■│ Distributed Pub/Sub
      │                 │ • Topic registry
      │                 │ • Content routing
────────────────────────────────────────────────────────────────────
15-16 │               ■■■■│ RPC Layer
      │                   │ • Sync RPC
      │                   │ • WAMP compat
────────────────────────────────────────────────────────────────────
17    │                 ■■│ Security
      │                   │ • TLS certs
      │                   │ • Rate limiting
────────────────────────────────────────────────────────────────────
18    │                  ■■│ Monitoring
      │                    │ • Metrics
      │                    │ • Visualization
────────────────────────────────────────────────────────────────────
19    │                   ■■│ Performance
      │                     │ • Optimization
      │                     │ • Benchmarks
────────────────────────────────────────────────────────────────────
20    │                    ■■│ Documentation
      │                      │ • Arch guide
      │                      │ • API docs
────────────────────────────────────────────────────────────────────

Legend:
■ = Active development
```

---

## Technical Deep Dives

### Deep Dive 1: QUIC vs TCP for Distributed Erlang

**Why QUIC is Better for Distributed Erlang:**

#### 1. Head-of-Line Blocking

**TCP Problem:**
```
Process A sends: ──[Packet 1]──[Packet 2]──[Packet 3*LOST]──[Packet 4]──►
                                          ▲
                                          │
                        All packets 4+ blocked until 3 retransmitted!

Process B waiting to receive Packet 4:  ⏳ BLOCKED
```

**QUIC Solution:**
```
Stream 0 (Process A): ──[Pkt 1]──[Pkt 2]──[Pkt 3*LOST]──►
                                          ▲ Retransmit only this
                                          │
Stream 1 (Process B): ──[Pkt 1]──[Pkt 2]──[Pkt 3]──► ✓ NOT BLOCKED!

Independent streams = No cross-stream blocking
```

#### 2. Connection Migration

**TCP Problem:**
```
Mobile device moves from WiFi to Cellular:

WiFi IP:     192.168.1.100:5000
             ↓ [Connection established]
             ↓ [Active transfers]
             ↓ [Network switches]
Cellular IP: 10.20.30.40:6000
             ↓ [TCP connection LOST]
             ↓ [Must re-establish: 3 RTT]
             ↓ [Resume transfers]
```

**QUIC Solution:**
```
WiFi IP:     192.168.1.100:5000
             ↓ [Connection ID: 0x1a2b3c4d]
             ↓ [Active transfers]
             ↓ [Network switches]
Cellular IP: 10.20.30.40:6000
             ↓ [Same Connection ID!]
             ↓ [Continue immediately: 0 RTT]
             ↓ [Transfers uninterrupted]
```

#### 3. 0-RTT Resumption

**TCP + TLS 1.2:**
```
Client → Server: SYN                      RTT 1
Server → Client: SYN-ACK
Client → Server: ACK
Client → Server: ClientHello              RTT 2
Server → Client: ServerHello + Certificate
Client → Server: Finished
Server → Client: Finished
Client → Server: HTTP Request             RTT 3

Total: 3 RTT before application data
```

**QUIC (with 0-RTT token):**
```
Client → Server: Initial + 0-RTT Data + App Request   RTT 0

Total: 0 RTT! Data sent immediately
```

This is HUGE for edge devices that frequently reconnect!

#### 4. Multiplexing Efficiency

**HTTP/2 over TCP:**
```
┌──────────────────────────────────────┐
│ TCP Connection                       │
│  ┌──────────────────────────────┐   │
│  │ HTTP/2 Multiplexing          │   │
│  │  Stream 1, Stream 2, ...     │   │
│  └──────────────────────────────┘   │
│                                      │
│ Problem: TCP sees bytes, not streams │
│ Single packet loss blocks ALL streams│
└──────────────────────────────────────┘
```

**HTTP/3 over QUIC:**
```
┌──────────────────────────────────────┐
│ QUIC Connection                      │
│  Stream 1 (independent)              │
│  Stream 2 (independent)              │
│  Stream 3 (independent)              │
│  ...                                 │
│                                      │
│ QUIC understands streams natively!   │
│ Packet loss only affects one stream  │
└──────────────────────────────────────┘
```

For Erlang distribution with millions of processes, this is critical!

---

### Deep Dive 2: SWIM Gossip Protocol

**SWIM: Scalable Weakly-consistent Infection-style Process Group Membership**

#### Why SWIM?

Traditional heartbeat protocols don't scale:
```
N nodes sending heartbeats to all others:
  Network load: O(N²) messages per period

Example: 1000 nodes, 1 sec heartbeat
  = 1,000,000 messages/sec
  = NOT SCALABLE
```

SWIM uses gossip:
```
Each node:
  - Pings 1 random member per period
  - Gossips to K random members

Network load: O(N) messages per period

Example: 1000 nodes, K=3
  = 1000 pings + 3000 gossip = 4000 messages/sec
  = SCALABLE!
```

#### SWIM Algorithm

```
Every protocol period (1 second):

1. SELECT random member M
2. PING M (wait for ACK)
   ├─ If ACK received → M is alive
   └─ If timeout → INDIRECT PING

3. INDIRECT PING:
   ├─ Select K random members (e.g., K=3)
   ├─ Ask each to ping M on your behalf
   └─ If any ACK → M is alive
       If all timeout → M is SUSPECT

4. SUSPECT handling:
   ├─ Don't immediately mark M as dead
   ├─ Give time for refutation (5 seconds)
   ├─ M can increase its "incarnation number" to refute
   └─ If no refutation → M is DEAD

5. GOSSIP:
   ├─ Select K random members
   └─ Send recent membership changes
       (new members, state changes, etc.)
```

#### Suspicion Mechanism

```
Timeline:

T+0s:  Node fails to respond to ping
       │
       ▼
       Mark as SUSPECT (not dead!)
       │
       ├─ Broadcast "Node X is suspect"
       │  via gossip
       │
T+1s:  │  Other nodes hear rumor
       │  └─► Try to ping Node X
       │       └─► Some may succeed!
       │
T+2s:  │  Node X hears it's suspected
       │  └─► Refutes by increasing
       │       incarnation number
       │
T+5s:  ▼
       If no refutation → Mark as DEAD
       │
       └─ Broadcast "Node X is dead"
```

This prevents false positives from temporary network glitches!

#### Gossip Dissemination

```
Epidemic-style spread:

T+0: Node A detects change (Node X joined)
     ┌───────┐
     │   A   │ knows: X joined
     └───────┘

T+1: A gossips to B, C, D (K=3 random)
     ┌───────┐   ┌───────┐   ┌───────┐   ┌───────┐
     │   A   │──►│   B   │──►│   C   │──►│   D   │
     └───────┘   └───────┘   └───────┘   └───────┘
                  knows       knows       knows

T+2: B, C, D gossip to 3 others each (exponential spread)
     9 nodes know

T+3: 27 nodes know

T+log₃(N): All nodes know!
```

Convergence time: **O(log N)**

---

### Deep Dive 3: Kademlia DHT for Routing

**Why DHT (Distributed Hash Table)?**

In large mesh networks, full mesh (N² connections) doesn't scale:
```
Nodes  Connections  Problem
  10       45       OK
 100     4,950      Getting expensive
1000   499,500      IMPOSSIBLE
```

DHT enables O(log N) routing:
```
Nodes  Hops (log₂ N)
  10        3
 100        6
1000       10
```

#### Kademlia Basics

**XOR Distance Metric:**
```
Node IDs are 256-bit hashes (SHA256)

Node A: 0x3a7f...
Node B: 0x8c12...

Distance = A XOR B
         = 0x3a7f... XOR 0x8c12...
         = 0xb66d...

Properties:
  • d(A, B) = d(B, A)  (symmetric)
  • d(A, A) = 0
  • d(A, B) + d(B, C) >= d(A, C)  (triangle inequality)
```

**k-buckets:**
```
Each node maintains k-buckets for distance ranges:

Bucket 0:  Distance 2^0  to 2^1   (1 hop away)
Bucket 1:  Distance 2^1  to 2^2   (2 hops)
Bucket 2:  Distance 2^2  to 2^3   (4 hops)
...
Bucket 255: Distance 2^255 to 2^256 (very far)

Each bucket stores up to K nodes (e.g., K=20)
```

**Routing:**
```
To send message to target T:

1. Calculate distance: d = XOR(my_id, T)
2. Find bucket for distance d
3. Select closest node N from bucket
4. Forward to N
5. Repeat until reached T

Max hops: log₂(total_nodes)
```

#### Iterative Node Lookup

```erlang
find_node(TargetId) ->
    % Start with K closest known nodes
    Closest = get_k_closest(TargetId, K),
    find_node_iter(TargetId, Closest, [], K).

find_node_iter(TargetId, Closest, Queried, K) ->
    % Select ALPHA unqueried nodes to ask
    ToQuery = select_closest_unqueried(Closest, Queried, ALPHA),

    if
        ToQuery =:= [] ->
            % No more to query, return result
            lists:sublist(Closest, K);
        true ->
            % Query nodes in parallel
            Results = pmap(fun(Node) ->
                rpc:call(Node, kademlia, get_closest, [TargetId, K])
            end, ToQuery),

            % Merge results and sort by distance
            NewClosest = merge_and_sort(Closest, lists:flatten(Results), TargetId),
            NewQueried = Queried ++ ToQuery,

            % Check if target found
            case lists:member(TargetId, NewClosest) of
                true -> {found, TargetId};
                false -> find_node_iter(TargetId, NewClosest, NewQueried, K)
            end
    end.
```

Complexity: **O(log N)** lookups, **O(ALPHA * log N)** messages

---

## Success Metrics

### Phase 1 Success Criteria

- ✅ **QUIC Transport Works**
  - Server accepts connections
  - Client can connect
  - Bidirectional streams function
  - Connection survives stream closure

- ✅ **Wire Protocol Implemented**
  - Messages encode/decode correctly
  - Handshake completes successfully
  - All message types supported

- ✅ **Basic Distribution Functions**
  - `net_kernel:connect_node/1` works
  - `nodes()` shows connected nodes
  - Message sending: `{Name, Node} ! Msg`
  - Remote spawn: `spawn(Node, Fun)`
  - Process linking works
  - Monitoring works

**Deliverable:** Two Erlang nodes communicating over HTTP/3

---

### Phase 2 Success Criteria

- ✅ **Node Discovery Works**
  - Bootstrap discovery functional
  - mDNS discovery works locally
  - DNS SRV discovery works

- ✅ **Membership Protocol Stable**
  - SWIM protocol running
  - Failures detected within 10 seconds
  - Gossip converges in O(log N) time
  - No false positives in stable network

- ✅ **Topology Self-Organizes**
  - Nodes form k-regular graph
  - New nodes join smoothly
  - Departed nodes removed from topology
  - Healing after network partition

- ✅ **Routing Functions**
  - Messages route through mesh
  - DHT lookup finds nodes
  - O(log N) hops for routing

**Deliverable:** 20+ node mesh that self-heals from failures

---

### Phase 3 Success Criteria

- ✅ **NAT Traversal Works**
  - STUN discovers public address
  - ICE negotiates connectivity
  - UDP hole punching succeeds (>80%)
  - TURN relay works as fallback

- ✅ **Real-World Scenarios**
  - Home router NAT traversed
  - Corporate firewall traversed
  - Mobile hotspot NAT traversed
  - Symmetric NAT handled

**Deliverable:** Nodes behind different NATs forming mesh

---

### Phase 4 Success Criteria

- ✅ **Pub/Sub Works**
  - Topic subscriptions work
  - Messages routed by topic
  - Pattern matching (prefix/wildcard)
  - Scalable (not flooding all nodes)

- ✅ **RPC Works**
  - Synchronous RPC calls
  - Timeouts handled correctly
  - Error propagation works

- ✅ **WAMP Compatible**
  - Existing WAMP clients can connect
  - Subscribe/Publish semantics match
  - Call/Register semantics match

**Deliverable:** CortexIQ PoC runs on Macula Mesh

---

### Phase 5 Success Criteria

- ✅ **Security Hardened**
  - TLS certificates managed
  - Message authentication
  - Rate limiting prevents DoS
  - Access control enforced

- ✅ **Production Monitoring**
  - Prometheus metrics exported
  - Topology visualization works
  - Alerts fire on anomalies
  - Distributed tracing available

- ✅ **Performance Acceptable**
  - <10ms latency (local)
  - <100ms latency (internet)
  - >10,000 msg/sec throughput
  - Scales to 1000+ nodes

- ✅ **Documentation Complete**
  - Architecture guide published
  - API reference docs
  - Deployment guide
  - Migration from Bondy guide

**Deliverable:** Production-ready Macula Mesh 1.0

---

## Conclusion

Macula HTTP/3 Mesh represents a unique opportunity to build **world-class distributed infrastructure for the BEAM ecosystem**. By leveraging QUIC's modern transport capabilities, we can create a mesh network that:

1. **Works anywhere** - Through NATs, firewalls, mobile networks
2. **Scales effortlessly** - O(log N) routing, not O(N²) connections
3. **Feels native** - Standard Erlang distribution semantics
4. **Performs brilliantly** - 0-RTT reconnection, no head-of-line blocking
5. **Stands out** - Nobody else has this for BEAM

This is a **20-week journey** that will culminate in a "Wow, how do they do it?" moment.

**Next Steps:**
1. Set up development environment
2. Integrate quicer dependency
3. Build Week 1 deliverable (QUIC echo server/client)
4. Start the journey! 🚀

---

## References

### QUIC & HTTP/3

- [RFC 9000: QUIC Transport Protocol](https://www.rfc-editor.org/rfc/rfc9000.html)
- [RFC 9001: Using TLS to Secure QUIC](https://www.rfc-editor.org/rfc/rfc9001.html)
- [RFC 9114: HTTP/3](https://www.rfc-editor.org/rfc/rfc9114.html)
- [MsQuic Documentation](https://github.com/microsoft/msquic/tree/main/docs)

### Libraries

- [quicer (Erlang NIF)](https://github.com/emqx/quic)
- [xquic (Alibaba)](https://github.com/alibaba/xquic)
- [quinn (Rust)](https://github.com/quinn-rs/quinn)

### Algorithms

- [SWIM: Scalable Membership Protocol](https://www.cs.cornell.edu/projects/Quicksilver/public_pdfs/SWIM.pdf)
- [Kademlia: A Peer-to-peer Information System](https://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf)
- [Consistent Hashing](https://www.cs.princeton.edu/courses/archive/fall09/cos518/papers/chash.pdf)

### Related Work

- [Partisan: Flexible Distributed Erlang](https://github.com/lasp-lang/partisan)
- [Bondy: Distributed WAMP Router](https://github.com/bondy-io/bondy)
- [Riak Core: Distributed Systems Framework](https://github.com/basho/riak_core)

---

**Document Version:** 1.0
**Last Updated:** 2025-11-07
**Author:** Macula Architecture Team
**Status:** Proposal
