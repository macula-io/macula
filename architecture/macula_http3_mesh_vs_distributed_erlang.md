# Macula HTTP/3 Mesh vs Distributed Erlang

**Version**: 1.0
**Date**: November 11, 2025

## Quick Answer

**No, Macula does not augment Distributed Erlang. It replaces it.**

Macula provides an alternative approach to building distributed Erlang/Elixir systems using **HTTP/3/QUIC** instead of Distributed Erlang's built-in clustering protocol.

---

## Why Macula Doesn't Use Distributed Erlang

### Distributed Erlang's Strengths
Distributed Erlang (disterl) is powerful for tightly-coupled clusters:
- **Transparent messaging**: Send messages to `{Name, Node}` tuples
- **Process monitoring**: `monitor/2` works across nodes
- **Global process registry**: Register names visible cluster-wide
- **Code loading**: Hot code upgrades across clusters
- **BEAM-native**: Zero-overhead local communication

### Distributed Erlang's Limitations for Macula's Use Case

#### 1. **Security Model**
```erlang
%% Distributed Erlang: Cookie-based authentication
-setcookie my_secret_cookie

%% Problem: All-or-nothing trust
%% - A node with the cookie has FULL cluster access
%% - Can execute ANY code on ANY node
%% - Cannot isolate tenants/realms
```

**Macula's Solution**:
```erlang
%% TLS certificate-based authentication
%% - Per-connection authentication
%% - Realm-based isolation
%% - No code execution privileges by default
```

---

#### 2. **Network Assumptions**
Distributed Erlang assumes:
- **Fully connected mesh**: Every node connects to every other node
- **Low latency, reliable links**: Designed for LAN
- **Static topology**: Cluster membership relatively stable
- **No NAT/firewalls**: Requires all ports open between nodes

**Macula's Reality**:
- **Internet-scale deployment**: Nodes across WAN/Internet
- **NAT traversal**: Works behind home routers, corporate firewalls
- **Dynamic topology**: Nodes join/leave frequently (mobile devices, IoT)
- **Single port**: HTTP/3 on port 443 (or 9443), firewall-friendly

---

#### 3. **Multi-Tenancy**
```erlang
%% Distributed Erlang: No namespace isolation
%% All nodes in cluster share:
%% - Global process registry
%% - Same security domain
%% - Cannot isolate tenant A from tenant B
```

**Macula's Solution**:
```erlang
%% Realm-based isolation
{ok, Conn} = macula_connection:start_link(Url, #{
    realm => <<"tenant-a">>,  % Isolated from tenant-b
    node_id => <<"sensor-1">>
}).

%% Messages in realm "tenant-a" never leak to "tenant-b"
```

---

#### 4. **Scalability**
Distributed Erlang:
- **Recommended limit**: ~50-200 nodes depending on configuration
- **Full mesh**: N² connections as cluster grows
- **Network partitions**: Difficult to handle (require `net_kernel` configuration)
- **DNS dependency**: Nodes must resolve each other's hostnames

**Macula's Approach**:
- **Kademlia DHT**: Logarithmic routing (O(log N) hops)
- **Selective connections**: Only connect to needed peers
- **Partition tolerance**: Built-in eventual consistency
- **mDNS discovery**: Zero-configuration local discovery

---

#### 5. **Explicit vs Implicit Communication**

**Distributed Erlang** (implicit):
```erlang
%% "Magic" remote execution
rpc:call('node2@host', Module, Function, Args).

%% Sends Erlang term directly to remote process
{SomePid, remote_node} ! {message, Data}.
```

**Macula** (explicit):
```erlang
%% Explicit RPC over network
{ok, Result} = macula_connection:call(Conn, "math.add", [1, 2]).

%% Explicit pub/sub
ok = macula_connection:publish(Conn, <<"sensor.temp">>, #{value => 23.5}).
```

**Benefits of Explicit**:
- **Network boundaries clear**: Know when crossing network
- **Serialization explicit**: Must encode to JSON/MessagePack
- **Failure handling explicit**: Network errors are different from process crashes
- **Versioning easier**: Wire format independent of BEAM term format

---

## Architectural Comparison

### Distributed Erlang Architecture
```
┌──────────────────────────────────────────────────────┐
│                 Erlang Cluster                       │
│  ┌─────────┐      ┌─────────┐      ┌─────────┐     │
│  │ Node 1  │◄────►│ Node 2  │◄────►│ Node 3  │     │
│  │         │      │         │      │         │     │
│  │ cookie: │      │ cookie: │      │ cookie: │     │
│  │ "secret"│      │ "secret"│      │ "secret"│     │
│  └─────────┘      └─────────┘      └─────────┘     │
│       ▲                ▲                ▲           │
│       │                │                │           │
│       └────────────────┴────────────────┘           │
│            Fully Connected Mesh                     │
│         (Erlang Distribution Protocol)              │
└──────────────────────────────────────────────────────┘

Characteristics:
- All nodes trust each other (same cookie)
- All nodes can execute code on any other node
- Single security domain
- Fully connected mesh (N² connections)
- LAN-optimized
```

### Macula HTTP/3 Mesh Architecture
```
┌──────────────────────────────────────────────────────┐
│              Realm: "tenant-a"                       │
│  ┌─────────┐      ┌─────────┐      ┌─────────┐     │
│  │ Node 1  │      │ Node 2  │      │ Registry│     │
│  │ (edge)  │─HTTP3─►(edge)  │─HTTP3─►(hub)   │     │
│  │ cert1   │      │ cert2   │      │ cert-CA │     │
│  └─────────┘      └─────────┘      └─────────┘     │
└──────────────────────────────────────────────────────┘
                           │
                           │ (isolated)
                           ▼
┌──────────────────────────────────────────────────────┐
│              Realm: "tenant-b"                       │
│  ┌─────────┐      ┌─────────┐                       │
│  │ Node 4  │─HTTP3─►Registry │                      │
│  │ (edge)  │      │ (hub)    │                      │
│  │ cert4   │      │ cert-CA  │                      │
│  └─────────┘      └─────────┘                       │
└──────────────────────────────────────────────────────┘

Characteristics:
- Realm-based isolation (no cross-realm communication)
- TLS certificate authentication per connection
- Selective connectivity (connect only to needed services)
- DHT-based service discovery
- WAN-optimized (NAT-friendly, single port)
```

---

## When to Use Each

### Use Distributed Erlang When:
✅ **Tightly-coupled cluster**
- All nodes in same data center
- Low latency, reliable network
- Single security domain (all nodes trust each other)
- Need transparent remote process messaging
- <50 nodes

✅ **Development/Testing**
- Local development clusters
- Integration testing
- Quick prototypes

✅ **Traditional Erlang Applications**
- Mnesia clustering
- Global process registry
- `:rpc` module usage
- Existing disterl-based systems

---

### Use Macula When:
✅ **Internet-Scale Distribution**
- Nodes across WAN/Internet
- NAT traversal required (home routers, corporate firewalls)
- High latency, unreliable connections
- Dynamic node membership (mobile, IoT)

✅ **Multi-Tenancy**
- SaaS applications with isolated tenants
- Multiple security domains
- Per-tenant resource limits
- Realm-based message routing

✅ **Service-Oriented Architecture**
- Microservices with RPC/pub-sub
- Explicit service boundaries
- API versioning important
- Different services in different languages (via HTTP/3)

✅ **Edge Computing**
- IoT devices
- Mobile applications
- Edge-to-cloud communication
- Offline-first applications

✅ **Large-Scale Clusters**
- >100 nodes
- Logarithmic routing (DHT)
- Selective connectivity

---

## Can You Use Both?

**Yes, but typically you pick one.**

### Hybrid Approach
```erlang
%% Within a data center: Distributed Erlang
%% (3-5 closely-coupled nodes)

%% Across data centers or to edge: Macula
%% (internet-facing, multi-tenant)

┌───────────────────────────┐
│   Data Center A           │
│  ┌───────────────────┐    │
│  │ Disterl Cluster   │    │
│  │ ┌────┐  ┌────┐   │    │
│  │ │ N1 │◄─►│ N2 │   │    │
│  │ └────┘  └────┘   │    │
│  └──────┬────────────┘    │
│         │ Macula          │
│         │ Gateway         │
└─────────┼─────────────────┘
          │
    HTTP/3│QUIC
          │
┌─────────┼─────────────────┐
│         ▼                 │
│   Data Center B           │
│  ┌───────────────────┐    │
│  │ Disterl Cluster   │    │
│  │ ┌────┐  ┌────┐   │    │
│  │ │ N3 │◄─►│ N4 │   │    │
│  │ └────┘  └────┘   │    │
│  └───────────────────┘    │
└───────────────────────────┘
```

**Use Case**: Internal services use disterl for low latency, external/edge clients use Macula for security and NAT traversal.

---

## Code Comparison

### Distributed Erlang
```erlang
%% Start cluster
%% $ erl -name node1@host1 -setcookie secret
%% $ erl -name node2@host2 -setcookie secret

%% Connect nodes
net_kernel:connect_node('node2@host2').

%% Call remote function
Result = rpc:call('node2@host2', math, add, [1, 2]).

%% Send message to remote process
{my_process, 'node2@host2'} ! {hello, "from node1"}.

%% Monitor remote process
Ref = monitor(process, {my_process, 'node2@host2'}).
```

### Macula
```erlang
%% Start connection to remote node
{ok, Conn} = macula_connection:start_link(
    <<"https://node2.example.com:9443">>,
    #{
        realm => <<"production">>,
        node_id => <<"node1">>,
        capabilities => [rpc, pubsub]
    }
).

%% Call remote procedure (explicit RPC)
{ok, Result} = macula_connection:call(Conn, "math.add", [1, 2]).

%% Publish message (pub/sub pattern)
ok = macula_connection:publish(Conn, <<"events.hello">>, #{
    from => "node1",
    message => "Hello from node1"
}).

%% Subscribe to messages
{ok, _Sub} = macula_connection:subscribe(Conn, <<"events.hello">>, fun(Msg) ->
    io:format("Received: ~p~n", [Msg])
end).

%% No process monitoring - use application-level heartbeats instead
```

---

## Key Differences Summary

| Aspect | Distributed Erlang | Macula HTTP/3 Mesh |
|--------|-------------------|-------------------|
| **Transport** | Erlang Distribution Protocol (TCP) | HTTP/3 (QUIC/UDP) |
| **Security** | Cookie-based (all-or-nothing) | TLS certificates (per-connection) |
| **Multi-tenancy** | ❌ No isolation | ✅ Realm-based isolation |
| **NAT Traversal** | ❌ Requires all ports open | ✅ Single port, firewall-friendly |
| **Scalability** | ~50-200 nodes (full mesh) | 1000s of nodes (DHT routing) |
| **Code Execution** | ✅ Remote code execution | ❌ No code execution |
| **Process Monitoring** | ✅ Built-in | ❌ Use application-level heartbeats |
| **Network Model** | Fully connected mesh | Selective connectivity |
| **Service Discovery** | DNS/explicit connection | Kademlia DHT + mDNS |
| **Wire Format** | Erlang term format | MessagePack (language-neutral) |
| **Failure Model** | Same as local (transparent) | Explicit network failures |
| **Use Case** | Tightly-coupled clusters | Loosely-coupled services |

---

## Migration Path

If you have an existing Distributed Erlang system:

### Step 1: Identify Boundaries
```
Internal tight cluster → Keep disterl
External services → Migrate to Macula
Edge devices → Migrate to Macula
Multi-tenant parts → Migrate to Macula
```

### Step 2: Add Macula Gateway
```erlang
%% Add Macula to existing disterl node
{ok, _} = macula:start(#{
    port => 9443,
    realm => <<"production">>,
    cert_file => "cert.pem",
    key_file => "key.pem"
}).

%% Register existing functions as RPC procedures
macula_connection:register(Conn, "user.get", fun(Args) ->
    UserId = maps:get(<<"id">>, Args),
    User = my_user_server:get_user(UserId),
    {ok, User}
end).
```

### Step 3: Migrate Clients Incrementally
```
External API → Use Macula
Mobile apps → Use Macula
IoT devices → Use Macula
Internal services → Keep disterl or migrate based on needs
```

---

## Summary

Macula **replaces** Distributed Erlang for scenarios where:
- You need NAT traversal
- You want realm-based multi-tenancy
- You're building service-oriented architectures
- You need to scale beyond typical disterl limits
- Security requires per-connection authentication

Macula **complements** your Erlang/Elixir stack by:
- Providing a modern HTTP/3 transport option
- Enabling broader connectivity (browsers, mobile, IoT)
- Making network boundaries explicit
- Supporting language-agnostic clients (any language with HTTP/3 support)

**The Choice**: Use the right tool for each part of your system. Disterl for tight internal clusters, Macula for loosely-coupled distributed services.
