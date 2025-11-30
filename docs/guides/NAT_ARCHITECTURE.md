# NAT Traversal System Architecture

Visual architecture documentation for the Macula NAT traversal system.

---

## Supervision Tree

The NAT system uses a `one_for_one` supervision strategy where each child is independent.

```
                    ┌────────────────────────┐
                    │   macula_nat_system    │
                    │     (supervisor)       │
                    │                        │
                    │  strategy: one_for_one │
                    │  intensity: 5          │
                    │  period: 60s           │
                    └───────────┬────────────┘
                                │
        ┌───────────┬───────────┼───────────┬───────────┬───────────┐
        │           │           │           │           │           │
        ▼           ▼           ▼           ▼           ▼           ▼
   ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐
   │  nat_   │ │  nat_   │ │  nat_   │ │  port_  │ │ relay_  │ │ relay_  │
   │  cache  │ │detector │ │coordi-  │ │predictor│ │registry │ │  node   │
   │         │ │         │ │ nator   │ │         │ │         │ │         │
   │ (worker)│ │ (worker)│ │ (worker)│ │ (worker)│ │ (worker)│ │ (worker)│
   └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘
        │           │           │           │           │           │
   Cache NAT   Detect NAT  Coordinate   Predict     Register    Relay
   profiles    type using  hole punch   ports for   relays in   traffic
   with TTL    observers   attempts     punching    DHT         between
                                                                peers
```

---

## Module Dependencies

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           APPLICATION LAYER                                  │
│                                                                             │
│    ┌─────────────────┐                                                      │
│    │ macula_nat_     │  High-level API for connection establishment         │
│    │ connector       │  Auto-selects: direct → hole_punch → relay           │
│    └────────┬────────┘                                                      │
│             │                                                               │
│             │ uses                                                          │
│             ▼                                                               │
├─────────────────────────────────────────────────────────────────────────────┤
│                          COORDINATION LAYER                                  │
│                                                                             │
│    ┌─────────────┐     ┌─────────────┐     ┌─────────────┐                 │
│    │  nat_       │     │  nat_       │     │  hole_      │                 │
│    │coordinator  │────▶│  cache      │     │  punch      │                 │
│    │             │     │             │     │  executor   │                 │
│    └──────┬──────┘     └──────┬──────┘     └──────┬──────┘                 │
│           │                   │                   │                        │
│           │ determines        │ stores/           │ executes               │
│           │ strategy          │ fetches           │ punch                  │
│           ▼                   ▼                   ▼                        │
├─────────────────────────────────────────────────────────────────────────────┤
│                          DETECTION LAYER                                     │
│                                                                             │
│    ┌─────────────┐     ┌─────────────┐                                     │
│    │  nat_       │────▶│  port_      │  Uses detection results             │
│    │  detector   │     │  predictor  │  for prediction                     │
│    └──────┬──────┘     └─────────────┘                                     │
│           │                                                                 │
│           │ probes external observers                                       │
│           ▼                                                                 │
├─────────────────────────────────────────────────────────────────────────────┤
│                           RELAY LAYER                                        │
│                                                                             │
│    ┌─────────────┐     ┌─────────────┐                                     │
│    │  relay_     │────▶│  relay_     │  Registry finds relays              │
│    │  registry   │     │  node       │  Node relays traffic                │
│    └─────────────┘     └─────────────┘                                     │
│                                                                             │
├─────────────────────────────────────────────────────────────────────────────┤
│                        INFRASTRUCTURE LAYER                                  │
│                                                                             │
│    ┌─────────────┐     ┌─────────────┐     ┌─────────────┐                 │
│    │   quicer    │     │ routing_    │     │    DHT      │                 │
│    │   (QUIC)    │     │  server     │     │  (storage)  │                 │
│    └─────────────┘     └─────────────┘     └─────────────┘                 │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Connection Flow Diagrams

### Direct Connection Flow

When target has public IP or Full Cone NAT:

```
┌─────────────┐                                        ┌─────────────┐
│   Peer A    │                                        │   Peer B    │
│ (initiator) │                                        │   (target)  │
└──────┬──────┘                                        └──────┬──────┘
       │                                                      │
       │  1. connect(PeerB_NodeId)                            │
       ├─────────────────────────────────────────────────────▶│
       │     Request to macula_nat_connector                  │
       │                                                      │
       │  2. Get strategy from coordinator                    │
       │     coordinator returns: direct                      │
       │                                                      │
       │  3. DHT lookup for PeerB endpoint                    │
       │     Returns: {IP, Port}                              │
       │                                                      │
       │  4. QUIC connect directly                            │
       │─────────────────────────────────────────────────────▶│
       │                                                      │
       │  5. Connection established                           │
       │◀─────────────────────────────────────────────────────│
       │     {ok, Conn, direct}                               │
       │                                                      │
```

### Hole Punch Connection Flow

When both peers are behind NAT with compatible types:

```
┌─────────────┐          ┌─────────────┐          ┌─────────────┐
│   Peer A    │          │  Bootstrap  │          │   Peer B    │
│ (initiator) │          │  (gateway)  │          │   (target)  │
└──────┬──────┘          └──────┬──────┘          └──────┬──────┘
       │                        │                        │
       │ 1. connect(PeerB)      │                        │
       ├───────────────────────▶│                        │
       │                        │                        │
       │ 2. Get NAT profiles    │                        │
       │    from cache/DHT      │                        │
       │                        │                        │
       │ 3. Determine strategy: │                        │
       │    hole_punch          │                        │
       │                        │                        │
       │ 4. Request punch       │ 5. Notify PeerB        │
       │    coordination        ├───────────────────────▶│
       │                        │    to initiate punch   │
       │                        │                        │
       │                        │                        │
       │            ┌───────────┴───────────┐            │
       │            │   T=0: Simultaneous   │            │
       │            │   punch starts        │            │
       │            └───────────────────────┘            │
       │                        │                        │
       │ 6. Send UDP to PeerB's │ 7. Send UDP to PeerA's │
       │    predicted endpoint  │    predicted endpoint  │
       │────────────────────────┼───────────────────────▶│
       │◀───────────────────────┼────────────────────────│
       │                        │                        │
       │                  ┌─────┴─────┐                  │
       │                  │ NAT holes │                  │
       │                  │  created  │                  │
       │                  └─────┬─────┘                  │
       │                        │                        │
       │ 8. Establish QUIC over │                        │
       │    punched hole        │                        │
       │◀───────────────────────┼───────────────────────▶│
       │                        │                        │
       │ 9. {ok, Conn, hole_punch}                       │
       │                        │                        │
```

### Relay Connection Flow

When hole punching fails or NATs are incompatible:

```
┌─────────────┐          ┌─────────────┐          ┌─────────────┐
│   Peer A    │          │   Relay     │          │   Peer B    │
│ (initiator) │          │   Node      │          │   (target)  │
└──────┬──────┘          └──────┬──────┘          └──────┬──────┘
       │                        │                        │
       │ 1. Hole punch failed   │                        │
       │    Fall back to relay  │                        │
       │                        │                        │
       │ 2. Find relay from     │                        │
       │    relay_registry      │                        │
       │                        │                        │
       │ 3. QUIC connect        │                        │
       │────────────────────────▶                        │
       │                        │                        │
       │ 4. Request relay       │ 5. QUIC connect to     │
       │    to PeerB            │    PeerB               │
       │                        ├───────────────────────▶│
       │                        │                        │
       │                        │ 6. Relay established   │
       │                        │◀───────────────────────│
       │                        │                        │
       │ 7. Tunnel confirmed    │                        │
       │◀───────────────────────│                        │
       │    {ok, Conn, relay}   │                        │
       │                        │                        │
       │ ═══════════════════════╪════════════════════════│
       │       Data flows through relay                  │
       │ ═══════════════════════╪════════════════════════│
       │                        │                        │
       │ 8. send(data)          │ 9. forward(data)       │
       │────────────────────────▶────────────────────────▶│
       │                        │                        │
       │ 11. forward(response)  │ 10. send(response)     │
       │◀───────────────────────◀────────────────────────│
       │                        │                        │
```

---

## NAT Detection Sequence

```
┌─────────────┐          ┌─────────────┐          ┌─────────────┐
│   Local     │          │ Observer 1  │          │ Observer 2  │
│   Node      │          │  (gateway)  │          │  (peer)     │
└──────┬──────┘          └──────┬──────┘          └──────┬──────┘
       │                        │                        │
       │ 1. NAT_PROBE           │                        │
       │────────────────────────▶                        │
       │                        │                        │
       │ 2. NAT_PROBE_RESPONSE  │                        │
       │    reflexive_addr:     │                        │
       │    203.0.113.5:40000   │                        │
       │◀───────────────────────│                        │
       │                        │                        │
       │ 3. NAT_PROBE                                    │
       │────────────────────────────────────────────────▶│
       │                                                 │
       │ 4. NAT_PROBE_RESPONSE                           │
       │    reflexive_addr:                              │
       │    203.0.113.5:40000   (same = EI mapping)      │
       │◀────────────────────────────────────────────────│
       │                        │                        │
       │                        │                        │
       ▼                        │                        │
  ┌─────────────────────────────────────────────────────────────┐
  │                    Classification Logic                      │
  │                                                             │
  │  Same IP + Same Port for both observers → EI mapping        │
  │  Same IP + Different Port → HD mapping                      │
  │  Different IP → PD mapping (or multiple NATs)               │
  │                                                             │
  │  Filtering determined by response to unsolicited packets    │
  │  Allocation determined by port delta patterns               │
  └─────────────────────────────────────────────────────────────┘
```

---

## Deployment Scenarios

### Scenario 1: Edge Devices Behind Residential NAT

```
                              Internet
                                 │
                    ┌────────────┴────────────┐
                    │                         │
             ┌──────▼──────┐          ┌──────▼──────┐
             │  Bootstrap  │          │  Bootstrap  │
             │  Gateway 1  │          │  Gateway 2  │
             │ (public IP) │          │ (public IP) │
             └──────┬──────┘          └──────┬──────┘
                    │                         │
          ┌─────────┴─────────┐               │
          │                   │               │
   ┌──────▼──────┐     ┌──────▼──────┐        │
   │  Home NAT   │     │  Home NAT   │        │
   │  (EI, PD)   │     │  (EI, HD)   │        │
   └──────┬──────┘     └──────┬──────┘        │
          │                   │               │
   ┌──────▼──────┐     ┌──────▼──────┐ ┌──────▼──────┐
   │   Edge      │     │   Edge      │ │   Edge      │
   │  Device A   │     │  Device B   │ │  Device C   │
   │             │     │             │ │ (public IP) │
   └─────────────┘     └─────────────┘ └─────────────┘

   Connection Matrix:
   ┌─────────┬───────────┬───────────┬───────────┐
   │         │  Device A │  Device B │  Device C │
   ├─────────┼───────────┼───────────┼───────────┤
   │Device A │     -     │   PUNCH   │  DIRECT   │
   │Device B │   PUNCH   │     -     │  DIRECT   │
   │Device C │  DIRECT   │  DIRECT   │     -     │
   └─────────┴───────────┴───────────┴───────────┘
```

### Scenario 2: Enterprise with CGNAT/Symmetric NAT

```
                              Internet
                                 │
                    ┌────────────┴────────────┐
                    │                         │
             ┌──────▼──────┐          ┌──────▼──────┐
             │  Bootstrap  │          │   Relay     │
             │   Gateway   │          │   Server    │
             │ (public IP) │          │ (public IP) │
             └──────┬──────┘          └──────┬──────┘
                    │                         │
          ┌─────────┴─────────┐               │
          │                   │               │
   ┌──────▼──────┐     ┌──────▼──────┐        │
   │  Corporate  │     │   CGNAT     │        │
   │  Firewall   │     │   (ISP)     │        │
   │  (PD, PD)   │     │  (PD, PD)   │        │
   └──────┬──────┘     └──────┬──────┘        │
          │                   │               │
   ┌──────▼──────┐     ┌──────▼──────┐        │
   │  Employee   │     │   Mobile    │        │
   │  Laptop     │     │   Device    │        │
   └─────────────┘     └─────────────┘        │

   Connection Matrix:
   ┌──────────┬───────────┬───────────┬───────────┐
   │          │  Laptop   │  Mobile   │  Gateway  │
   ├──────────┼───────────┼───────────┼───────────┤
   │  Laptop  │     -     │   RELAY   │  DIRECT   │
   │  Mobile  │   RELAY   │     -     │  DIRECT   │
   │  Gateway │  DIRECT   │  DIRECT   │     -     │
   └──────────┴───────────┴───────────┴───────────┘

   Note: Symmetric NATs (PD+PD+RD) require relay fallback
```

### Scenario 3: Hybrid Cloud Deployment

```
                    ┌─────────────────────────────────────────┐
                    │            Public Cloud                  │
                    │                                         │
                    │  ┌─────────────┐   ┌─────────────┐     │
                    │  │  Bootstrap  │   │   Relay     │     │
                    │  │  Gateway    │   │   Cluster   │     │
                    │  │             │   │             │     │
                    │  │  - DHT root │   │  - 3 nodes  │     │
                    │  │  - Observer │   │  - 10Gbps   │     │
                    │  └──────┬──────┘   └──────┬──────┘     │
                    │         │                 │            │
                    └─────────┼─────────────────┼────────────┘
                              │                 │
                    ══════════╪═════════════════╪═══════════════
                              │    Internet     │
                    ══════════╪═════════════════╪═══════════════
                              │                 │
        ┌─────────────────────┼─────────────────┼─────────────────┐
        │                     │                 │                 │
 ┌──────▼──────┐       ┌──────▼──────┐   ┌──────▼──────┐   ┌──────▼──────┐
 │  On-Prem    │       │  Home       │   │  Mobile     │   │  IoT        │
 │  Datacenter │       │  Office     │   │  Workers    │   │  Devices    │
 │             │       │             │   │             │   │             │
 │  NAT: None  │       │  NAT: EI,HD │   │  NAT: Varies│   │  NAT: EI,PD │
 │  Direct ✓   │       │  Punch ✓    │   │  Relay ↔    │   │  Punch ✓    │
 └─────────────┘       └─────────────┘   └─────────────┘   └─────────────┘
```

---

## C4 Context Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              CONTEXT LEVEL                                   │
└─────────────────────────────────────────────────────────────────────────────┘

                           ┌───────────────────┐
                           │                   │
                           │   External User   │
                           │   Applications    │
                           │                   │
                           └─────────┬─────────┘
                                     │
                                     │ Uses Macula SDK
                                     │ for P2P communication
                                     ▼
    ┌──────────────────────────────────────────────────────────────────────┐
    │                                                                      │
    │                        MACULA MESH PLATFORM                          │
    │                                                                      │
    │   ┌──────────────────────────────────────────────────────────────┐  │
    │   │                                                              │  │
    │   │                    NAT TRAVERSAL SYSTEM                      │  │
    │   │                                                              │  │
    │   │  Enables P2P communication regardless of NAT configuration   │  │
    │   │  using detection, hole punching, and relay fallback          │  │
    │   │                                                              │  │
    │   └──────────────────────────────────────────────────────────────┘  │
    │                                                                      │
    └───────────────────────────────┬──────────────────────────────────────┘
                                    │
              ┌─────────────────────┼─────────────────────┐
              │                     │                     │
              ▼                     ▼                     ▼
    ┌─────────────────┐   ┌─────────────────┐   ┌─────────────────┐
    │                 │   │                 │   │                 │
    │  Bootstrap      │   │    Relay        │   │    Peer         │
    │  Gateways       │   │    Servers      │   │    Nodes        │
    │                 │   │                 │   │                 │
    │  DHT bootstrap  │   │  Traffic relay  │   │  Mesh network   │
    │  NAT detection  │   │  for symmetric  │   │  participants   │
    │  observers      │   │  NATs           │   │                 │
    │                 │   │                 │   │                 │
    └─────────────────┘   └─────────────────┘   └─────────────────┘
```

---

## C4 Container Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              CONTAINER LEVEL                                 │
│                         (NAT Traversal System)                              │
└─────────────────────────────────────────────────────────────────────────────┘

    ┌─────────────────────────────────────────────────────────────────────────┐
    │                           macula_nat_system                             │
    │                            (Supervisor)                                 │
    └───────────────────────────────────┬─────────────────────────────────────┘
                                        │
          ┌──────────┬──────────┬───────┴───────┬──────────┬──────────┐
          │          │          │               │          │          │
          ▼          ▼          ▼               ▼          ▼          ▼
    ┌──────────┐┌──────────┐┌──────────┐ ┌──────────┐┌──────────┐┌──────────┐
    │          ││          ││          │ │          ││          ││          │
    │   NAT    ││   NAT    ││   NAT    │ │   Port   ││  Relay   ││  Relay   │
    │  Cache   ││ Detector ││Coordin-  │ │Predictor ││ Registry ││   Node   │
    │          ││          ││  ator    │ │          ││          ││          │
    │  [ETS]   ││  [UDP]   ││  [FSM]   │ │  [Algo]  ││  [DHT]   ││  [QUIC]  │
    │          ││          ││          │ │          ││          ││          │
    └────┬─────┘└────┬─────┘└────┬─────┘ └────┬─────┘└────┬─────┘└────┬─────┘
         │          │          │             │          │          │
         │          │          │             │          │          │
         ▼          ▼          ▼             ▼          ▼          ▼
    ┌──────────────────────────────────────────────────────────────────────┐
    │                                                                      │
    │                      macula_nat_connector                            │
    │                      (Application API)                               │
    │                                                                      │
    │   connect/2  →  Strategy Selection  →  Execute  →  Connection        │
    │                                                                      │
    └──────────────────────────────────────────────────────────────────────┘
```

---

## Data Flow Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              DATA FLOW                                       │
└─────────────────────────────────────────────────────────────────────────────┘

                    ┌──────────────────────────────────────┐
                    │         NAT Profile Data             │
                    │                                      │
                    │  {                                   │
                    │    node_id: binary(),                │
                    │    mapping: ei | hd | pd,            │
                    │    filtering: ei | hd | pd,          │
                    │    allocation: pp | pc | rd,         │
                    │    reflexive_address: {IP, Port},    │
                    │    detected_at: timestamp(),         │
                    │    confidence: high | medium | low   │
                    │  }                                   │
                    └───────────────┬──────────────────────┘
                                    │
                    ┌───────────────┼───────────────┐
                    │               │               │
                    ▼               ▼               ▼
           ┌───────────────┐ ┌───────────────┐ ┌───────────────┐
           │   Detection   │ │    Caching    │ │  DHT Storage  │
           │               │ │               │ │               │
           │ Observer1 ───▶│ │ ETS Table ───▶│ │ Kademlia ────▶│
           │ Observer2 ───▶│ │ TTL: 300s    │ │ k=20 nodes    │
           │               │ │ Max: 10000   │ │               │
           └───────┬───────┘ └───────┬───────┘ └───────┬───────┘
                   │                 │                 │
                   │                 │                 │
                   └────────────┬────┴─────────────────┘
                                │
                                ▼
                    ┌───────────────────────────────┐
                    │     Connection Strategy       │
                    │                               │
                    │  Input: Local + Remote NAT    │
                    │                               │
                    │  ┌─────────────────────────┐  │
                    │  │ NAT Compatibility Table │  │
                    │  │                         │  │
                    │  │ EI+EI → direct/punch    │  │
                    │  │ EI+HD → punch           │  │
                    │  │ EI+PD → punch (maybe)   │  │
                    │  │ PD+PD → relay           │  │
                    │  └─────────────────────────┘  │
                    │                               │
                    │  Output: direct | punch |     │
                    │          relay                │
                    └───────────────────────────────┘
```

---

## See Also

- [NAT Types Explained](NAT_TYPES_EXPLAINED.md) - Background on NAT types
- [NAT Traversal Developer Guide](NAT_TRAVERSAL_DEVELOPER_GUIDE.md) - API usage
- [NAT Configuration Reference](NAT_CONFIGURATION.md) - Configuration options
