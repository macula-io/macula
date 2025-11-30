# Macula Platform Vision

**Macula has evolved from a library to a distributed application platform.**

This document describes the architectural vision of Macula as a **platform** that hosts Line-of-Business, IoT, and TWEANN workloads in a decentralized mesh network.

---

## Platform Architecture Overview

<p align="center">
  <img src="../artwork/macula-platform-vision.svg" alt="Macula Platform Architecture" width="100%">
</p>

---

## Platform vs Library: The Evolution

### Library Approach (Legacy)

In the early days, Macula was conceived as a **library** that applications would import and use directly:

```erlang
%% Application directly uses Macula internals
-module(my_app).

start() ->
    %% App manages connection lifecycle
    {ok, Conn} = macula_connection:start_link(Url, Opts),

    %% App deals with low-level primitives
    macula_connection:publish(Conn, Topic, Payload),
    macula_gateway:route_message(Msg, Recipients),
    macula_routing_bucket:find_closest_nodes(NodeID).
```

**Problems with the library approach:**

- **Tight coupling** - Applications depend on Macula internals
- **Duplication** - Each app manages connections, handles failures
- **Complexity** - Business logic mixed with mesh infrastructure
- **Versioning** - Hard to upgrade Macula without breaking apps
- **Testing** - Apps need to mock mesh internals

### Platform Approach (Current Vision)

Macula v0.8.5+ operates as a **standalone OTP application** that provides services to workloads:

```erlang
%% Application uses clean SDK API
-module(my_app).

start() ->
    %% Platform handles all mesh infrastructure
    %% App just uses high-level operations

    %% Publish events
    macula:publish(<<"orders.created">>, OrderData),

    %% Subscribe to topics
    macula:subscribe(<<"inventory.*">>, fun handle_inventory/1),

    %% RPC to services
    {ok, Price} = macula:call(<<"pricing.calculate">>, Request),

    %% Advertise services
    macula:advertise(<<"orders.process">>, fun process_order/1).
```

**Benefits of the platform approach:**

- **Clean separation** - Business logic isolated from infrastructure
- **Simple API** - High-level operations, no internals exposed
- **Independent deployment** - Upgrade platform without touching apps
- **Shared infrastructure** - Multiple apps share one Macula instance
- **Testability** - Apps test business logic, not mesh behavior

---

## Workload Types

Macula supports three primary workload categories:

### 1. Line-of-Business (LoB) Applications

Traditional business applications that require distributed communication:

**Examples:**
- Order management systems
- Inventory tracking
- Customer relationship management
- Financial transaction processing
- Supply chain coordination

**Characteristics:**
- Request/response patterns (RPC)
- Event-driven workflows (pub/sub)
- Multi-service orchestration
- Data consistency requirements

**Architecture Pattern:**
```
┌─────────────────────────────────┐
│   Order Service                 │
│   ┌─────────────────────────┐   │
│   │  Business Logic         │   │
│   │  - Validate order       │   │
│   │  - Check inventory      │   │
│   │  - Process payment      │   │
│   └────────────┬────────────┘   │
│                │ SDK            │
└────────────────┼────────────────┘
                 ↓
         [Macula Platform]
                 ↓
    ┌────────────┼────────────┐
    ↓            ↓            ↓
[Inventory]  [Payment]  [Shipping]
```

### 2. IoT (Internet of Things) Workloads

Edge computing and sensor network applications:

**Examples:**
- Smart home controllers
- Industrial sensor networks
- Agricultural monitoring
- Fleet management
- Environmental sensors

**Characteristics:**
- High-frequency data streams
- Low-latency requirements
- Resource-constrained devices
- Local-first processing
- Eventual consistency

**Architecture Pattern:**
```
┌─────────────────────────────────┐
│   Smart Home Hub                │
│   ┌─────────────────────────┐   │
│   │  Device Controller      │   │
│   │  - Aggregate sensors    │   │
│   │  - Control actuators    │   │
│   │  - Local automation     │   │
│   └────────────┬────────────┘   │
│                │ SDK            │
└────────────────┼────────────────┘
                 ↓
         [Macula Platform]
                 ↓
    ┌────────────┼────────────┐
    ↓            ↓            ↓
[Sensors]   [Actuators]  [Cloud]
```

### 3. TWEANN (Topology & Weight Evolving Artificial Neural Networks)

Distributed evolutionary computation and neural network training:

**Examples:**
- NEAT (NeuroEvolution of Augmenting Topologies)
- Genetic algorithm populations
- Distributed machine learning
- Hyperparameter optimization
- Swarm intelligence

**Characteristics:**
- Parallel evaluation
- Population distribution
- Fitness aggregation
- Genome broadcasting
- Generation synchronization

**Architecture Pattern:**
```
┌─────────────────────────────────┐
│   NEAT Coordinator              │
│   ┌─────────────────────────┐   │
│   │  Evolution Engine       │   │
│   │  - Select parents       │   │
│   │  - Crossover/mutate     │   │
│   │  - Aggregate fitness    │   │
│   └────────────┬────────────┘   │
│                │ SDK            │
└────────────────┼────────────────┘
                 ↓
         [Macula Platform]
                 ↓
    ┌────────────┼────────────┐
    ↓            ↓            ↓
[Evaluator] [Evaluator] [Evaluator]
   Node 1      Node 2      Node N
```

---

## Platform Components

### Core Infrastructure (Always Running)

These components start automatically when Macula boots:

| Component | Purpose | Process Count |
|-----------|---------|---------------|
| `macula_routing_server` | Kademlia DHT routing | 1 |
| `macula_bootstrap_system` | Peer discovery, service registry | 3 |
| `macula_gateway_system` | QUIC transport, message routing | 9 |
| `macula_peers_sup` | Dynamic peer connections | 1 + 4/peer |

**Total base processes:** 17 (v0.8.5+)

### SDK Layer

The SDK provides the public API for applications:

```erlang
%% Core Operations
macula:publish(Topic, Payload) -> ok | {error, Reason}.
macula:subscribe(Pattern, Handler) -> {ok, SubId} | {error, Reason}.
macula:unsubscribe(SubId) -> ok.
macula:call(Service, Request) -> {ok, Response} | {error, Reason}.
macula:call(Service, Request, Timeout) -> {ok, Response} | {error, Reason}.
macula:advertise(Service, Handler) -> {ok, AdvId} | {error, Reason}.
macula:unadvertise(AdvId) -> ok.

%% Discovery
macula:discover(ServicePattern) -> {ok, [Service]} | {error, Reason}.
macula:node_id() -> NodeId.
macula:peers() -> [PeerId].

%% Health
macula:health() -> #{status => ok | degraded, ...}.
```

### Multi-Tenancy via Realms

Realms provide isolation between different applications or environments:

```
Realm: com.example.production
├── Order Service
├── Inventory Service
└── Payment Service

Realm: com.example.staging
├── Order Service (test)
├── Inventory Service (test)
└── Payment Service (test)

Realm: com.acme.iot
├── Sensor Network
├── Control Hub
└── Analytics
```

**Isolation guarantees:**
- Messages don't cross realm boundaries
- Services only visible within their realm
- DHT routing scoped to realm
- Connection pooling per realm

---

## Deployment Models

### Model 1: Embedded (Development)

Macula and application in the same BEAM VM:

```erlang
%% sys.config
[
  {macula, [
    {quic_port, 4433},
    {realm, <<"com.example.dev">>}
  ]},
  {my_app, [
    %% app config
  ]}
].
```

**Use cases:**
- Local development
- Testing
- Single-node deployments

### Model 2: Sidecar (Production)

Macula runs as a sidecar container:

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: order-service
spec:
  containers:
  - name: macula
    image: macula:0.8.5
    ports:
    - containerPort: 4433
      protocol: UDP
    env:
    - name: MACULA_QUIC_PORT
      value: "4433"
    - name: MACULA_REALM
      value: "com.example.production"
    volumeMounts:
    - name: certs
      mountPath: /var/lib/macula

  - name: order-service
    image: order-service:latest
    env:
    - name: MACULA_URL
      value: "https://localhost:4433"

  volumes:
  - name: certs
    persistentVolumeClaim:
      claimName: macula-certs
```

**Use cases:**
- Kubernetes deployments
- Microservices architectures
- Language-agnostic applications

### Model 3: Mesh Node (Edge)

Dedicated Macula nodes forming mesh backbone:

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│  Macula     │────▶│  Macula     │────▶│  Macula     │
│  Node 1     │◀────│  Node 2     │◀────│  Node 3     │
└──────┬──────┘     └──────┬──────┘     └──────┬──────┘
       │                   │                   │
       ↓                   ↓                   ↓
   [IoT Devices]      [LoB Apps]         [TWEANN Workers]
```

**Use cases:**
- Edge computing
- IoT gateways
- Distributed AI training

---

## Communication Patterns

### Pattern 1: Event-Driven (Pub/Sub)

Asynchronous event broadcasting:

```erlang
%% Publisher
handle_order_created(Order) ->
    macula:publish(<<"orders.created">>, #{
        order_id => Order#order.id,
        customer => Order#order.customer,
        items => Order#order.items,
        total => Order#order.total
    }).

%% Subscriber
init([]) ->
    {ok, _} = macula:subscribe(<<"orders.*">>, fun handle_order_event/1),
    {ok, #state{}}.

handle_order_event(#{topic := <<"orders.created">>, payload := Order}) ->
    update_inventory(Order);
handle_order_event(#{topic := <<"orders.shipped">>, payload := Shipment}) ->
    notify_customer(Shipment).
```

**Topic patterns:**
- `orders.created` - Exact match
- `orders.*` - Single-level wildcard
- `orders.>` - Multi-level wildcard (future)

### Pattern 2: Request/Response (RPC)

Synchronous service calls:

```erlang
%% Service provider
init([]) ->
    {ok, _} = macula:advertise(<<"pricing.calculate">>, fun calculate_price/1),
    {ok, #state{}}.

calculate_price(#{items := Items, customer := Customer}) ->
    BasePrice = sum_items(Items),
    Discount = get_discount(Customer),
    {ok, #{price => BasePrice * (1 - Discount)}}.

%% Service consumer
get_order_total(Items, Customer) ->
    Request = #{items => Items, customer => Customer},
    case macula:call(<<"pricing.calculate">>, Request, 5000) of
        {ok, #{price := Price}} -> {ok, Price};
        {error, timeout} -> {error, pricing_unavailable}
    end.
```

**Routing strategies:**
- DHT-based service discovery
- Load balancing across providers
- Automatic failover

### Pattern 3: Discovery

Dynamic service and peer discovery:

```erlang
%% Find services
{ok, Services} = macula:discover(<<"pricing.*">>),
%% Returns: [<<"pricing.calculate">>, <<"pricing.convert">>]

%% Find peers
Peers = macula:peers(),
%% Returns: [<<"abc123...">>, <<"def456...">>]

%% Get local node ID
NodeId = macula:node_id(),
%% Returns: <<"789xyz...">>
```

---

## Best Practices

### 1. Design for Failure

```erlang
%% Good: Handle failures gracefully
process_order(Order) ->
    case macula:call(<<"inventory.check">>, Order, 3000) of
        {ok, Available} when Available ->
            proceed_with_order(Order);
        {ok, _NotAvailable} ->
            {error, out_of_stock};
        {error, timeout} ->
            %% Retry or use cached data
            check_cached_inventory(Order)
    end.

%% Bad: Assume success
process_order(Order) ->
    {ok, Available} = macula:call(<<"inventory.check">>, Order),
    proceed_with_order(Order).
```

### 2. Use Meaningful Topics

```erlang
%% Good: Business-meaningful topics
macula:publish(<<"orders.confirmed">>, Order),
macula:publish(<<"inventory.depleted">>, Item),
macula:publish(<<"payment.captured">>, Payment).

%% Bad: Generic/technical topics
macula:publish(<<"event">>, #{type => order_confirmed, ...}),
macula:publish(<<"update">>, #{entity => inventory, ...}).
```

### 3. Keep Payloads Small

```erlang
%% Good: Reference large data
macula:publish(<<"documents.uploaded">>, #{
    document_id => DocId,
    storage_url => Url,
    size_bytes => Size
}).

%% Bad: Embed large data
macula:publish(<<"documents.uploaded">>, #{
    document_id => DocId,
    content => LargeBinaryData  %% Don't do this!
}).
```

### 4. Scope by Realm

```erlang
%% Production
{macula, [{realm, <<"com.example.prod">>}]}

%% Staging
{macula, [{realm, <<"com.example.staging">>}]}

%% Development
{macula, [{realm, <<"com.example.dev">>}]}
```

### 5. Monitor Health

```erlang
%% Periodic health check
check_platform_health() ->
    case macula:health() of
        #{status := ok} -> ok;
        #{status := degraded, reason := Reason} ->
            logger:warning("Macula degraded: ~p", [Reason]),
            alert_ops(Reason)
    end.
```

---

## Example: Complete LoB Application

Here's a complete example of an order processing service:

```erlang
-module(order_service).
-behaviour(gen_server).

%% API
-export([start_link/0, create_order/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    pending_orders = #{}
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_order(OrderData) ->
    gen_server:call(?MODULE, {create_order, OrderData}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Subscribe to relevant events
    {ok, _} = macula:subscribe(<<"payments.captured">>,
                               fun(Msg) -> ?MODULE ! {payment, Msg} end),
    {ok, _} = macula:subscribe(<<"inventory.reserved">>,
                               fun(Msg) -> ?MODULE ! {inventory, Msg} end),

    %% Advertise our services
    {ok, _} = macula:advertise(<<"orders.create">>, fun handle_create/1),
    {ok, _} = macula:advertise(<<"orders.status">>, fun handle_status/1),

    {ok, #state{}}.

handle_call({create_order, OrderData}, _From, State) ->
    %% Validate order
    case validate_order(OrderData) of
        {ok, Order} ->
            %% Check inventory via RPC
            case macula:call(<<"inventory.check">>, Order, 5000) of
                {ok, #{available := true}} ->
                    %% Reserve inventory
                    {ok, _} = macula:call(<<"inventory.reserve">>, Order, 5000),

                    %% Process payment
                    {ok, _} = macula:call(<<"payments.charge">>, Order, 10000),

                    %% Publish order created event
                    macula:publish(<<"orders.created">>, Order),

                    %% Store pending order
                    NewState = store_pending(Order, State),
                    {reply, {ok, Order#order.id}, NewState};

                {ok, #{available := false}} ->
                    {reply, {error, out_of_stock}, State};

                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;

        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({payment, #{payload := Payment}}, State) ->
    %% Payment captured, confirm order
    OrderId = maps:get(order_id, Payment),
    case get_pending(OrderId, State) of
        {ok, Order} ->
            %% Publish confirmed event
            macula:publish(<<"orders.confirmed">>, Order),

            %% Request shipment
            macula:call(<<"shipping.schedule">>, Order, 5000),

            NewState = remove_pending(OrderId, State),
            {noreply, NewState};

        not_found ->
            {noreply, State}
    end;

handle_info({inventory, #{payload := Reservation}}, State) ->
    %% Inventory reserved, continue processing
    logger:info("Inventory reserved: ~p", [Reservation]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%====================================================================
%% Internal functions
%%====================================================================

handle_create(Request) ->
    gen_server:call(?MODULE, {create_order, Request}).

handle_status(#{order_id := OrderId}) ->
    %% Return order status
    case get_order(OrderId) of
        {ok, Order} -> {ok, Order};
        not_found -> {error, not_found}
    end.

validate_order(Data) ->
    %% Validation logic
    {ok, #order{id = generate_id(), data = Data}}.

store_pending(Order, #state{pending_orders = Pending} = State) ->
    State#state{pending_orders = Pending#{Order#order.id => Order}}.

get_pending(OrderId, #state{pending_orders = Pending}) ->
    case maps:get(OrderId, Pending, not_found) of
        not_found -> not_found;
        Order -> {ok, Order}
    end.

remove_pending(OrderId, #state{pending_orders = Pending} = State) ->
    State#state{pending_orders = maps:remove(OrderId, Pending)}.

get_order(OrderId) ->
    %% Database lookup
    {ok, #order{id = OrderId}}.

generate_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).
```

---

## Future Directions

### v0.9.0: NAT Traversal

Direct P2P connections between peers behind NAT:
- STUN/TURN/ICE-like connectivity
- 95% direct connections
- 5% relay fallback

### v1.0.0: Production Ready

Full platform stability:
- Comprehensive monitoring
- Horizontal scaling
- Geographic distribution
- Enterprise features

### Beyond v1.0.0

- **Persistent streams** - Long-lived data channels
- **Consensus** - Distributed agreement protocols
- **Storage** - Integrated distributed storage
- **Compute** - Edge function execution

---

## Summary

Macula is a **distributed application platform** that provides:

| Capability | Description |
|------------|-------------|
| **Mesh Networking** | HTTP/3 (QUIC) transport with DHT routing |
| **Pub/Sub** | Event-driven messaging with wildcards |
| **RPC** | Service discovery and remote calls |
| **Multi-Tenancy** | Realm-based isolation |
| **Zero-Config** | Auto TLS, stable Node IDs |
| **Fault Tolerance** | OTP supervision, automatic recovery |

**Platform Benefits:**
- Clean separation between infrastructure and business logic
- Simple SDK API for applications
- Independent deployment and scaling
- Support for LoB, IoT, and TWEANN workloads

**Vision:** A decentralized, self-organizing mesh that enables distributed applications to communicate seamlessly across network boundaries.

---

## Related Documents

- [Getting Started](../GETTING_STARTED.md) - Quick start guide
- [Architecture Overview](../ARCHITECTURE.md) - Technical architecture
- [Supervision Tree](FULL_SUPERVISION_TREE.md) - OTP process hierarchy
- [Memory Management](memory_management/README.md) - Resource management
- [v0.8.5 Release](../CHANGELOG.md) - Latest release notes
