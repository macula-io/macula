# NAT Guide

Macula's connectivity model, NAT detection, and configuration reference.

---

## 1. Overview: Macula's Connectivity Model

Macula is a **federated relay mesh**, not a peer-to-peer network. Nodes do not connect
directly to each other as a primary strategy. Instead, Macula uses a layered connectivity
model with three tiers:

| Tier | Transport | When Used | Priority |
|------|-----------|-----------|----------|
| **Relay mesh** | Outbound QUIC to relay | Always (WAN default) | Primary |
| **LAN direct** | QUIC + mDNS discovery | Same local network | Optimization |
| **Hole punching** | UDP NAT traversal | Edge cases, legacy | Fallback |

### Relay-First Architecture

Every node maintains ONE outbound QUIC connection to a relay. Relays route all
inter-node messages. This design eliminates the need for NAT traversal in the
common case:

```
  Node A ──QUIC──> Relay 1 <──QUIC── Node B
                     |
                  Routes A<->B
                  messages
```

Nodes never need to accept inbound connections from the internet. The relay mesh
handles routing, failover, and rerouting on disconnect.

### When NAT Detection Matters

NAT detection remains useful for:

1. **LAN optimization** -- determining if two nodes share a local network where
   direct QUIC is faster than relay-routed traffic.
2. **Relay selection** -- understanding the local network environment helps choose
   the best relay and predict connectivity quality.
3. **Hole punching** (legacy/edge) -- for scenarios where relay bandwidth is
   constrained and both nodes have compatible NAT types.

---

## 2. NAT Types Explained

Understanding NAT behavior helps operators diagnose connectivity issues and
predict which LAN optimizations or hole-punch fallbacks will succeed.

Macula uses the NATCracker 3-policy classification model. Each NAT is described
by three independent policies:

### 2.1 Mapping Policy (How NAT assigns external addresses)

| Policy | Code | Behavior | Prevalence |
|--------|------|----------|------------|
| **Endpoint-Independent** | EI | Same external addr for all destinations | ~52% |
| **Host-Dependent** | HD | Different external addr per destination host | ~12% |
| **Port-Dependent** | PD | Different external addr per destination host:port | ~36% |

**Endpoint-Independent (EI):**
```
Local: 192.168.1.10:5000
  -> Destination A (8.8.8.8:53)   => NAT: 203.0.113.5:40000
  -> Destination B (1.1.1.1:53)   => NAT: 203.0.113.5:40000  (same!)
```

**Port-Dependent (PD):**
```
Local: 192.168.1.10:5000
  -> Destination A (8.8.8.8:53)   => NAT: 203.0.113.5:40000
  -> Destination B (1.1.1.1:53)   => NAT: 203.0.113.5:40001  (different!)
```

### 2.2 Filtering Policy (What incoming traffic NAT accepts)

| Policy | Code | Behavior | Security |
|--------|------|----------|----------|
| **Endpoint-Independent** | EI | Accepts from any source | Low |
| **Host-Dependent** | HD | Accepts from hosts we've contacted | Medium |
| **Port-Dependent** | PD | Accepts from exact host:port we've contacted | High |

### 2.3 Allocation Policy (How NAT chooses external ports)

| Policy | Code | Behavior | Predictability |
|--------|------|----------|----------------|
| **Port-Preservation** | PP | external_port = local_port | High |
| **Port-Contiguity** | PC | external_port = last_port + delta | Medium |
| **Random** | RD | No predictable pattern | None |

### 2.4 Common Combinations

| Type | Mapping | Filtering | Allocation | Prevalence | Hole Punch? |
|------|---------|-----------|------------|------------|-------------|
| Full Cone | EI | EI | PP | 15% | Yes (trivial) |
| Restricted Cone | EI | HD | PP | 37% | Yes |
| Port Restricted | EI | PD | PP | 20% | Yes |
| Symmetric | PD | PD | RD | 12% | No (relay required) |
| CGNAT | varies | PD | varies | 16% | Usually no |

### 2.5 CGNAT (Carrier-Grade NAT)

ISPs increasingly add a second NAT layer between the home router and the internet:

```
Device (192.168.1.10) -> Home NAT (10.0.0.50) -> CGNAT (203.0.113.5) -> Internet
```

CGNAT reduces hole-punch success to ~40%. This is a primary reason Macula uses
relay-first architecture -- relays work regardless of NAT type.

### Further Reading

- [RFC 5780 - NAT Behavior Discovery](https://tools.ietf.org/html/rfc5780)
- [NATCracker Paper](https://www.usenix.org/conference/nsdi21/presentation/tang) - 27 NAT type classification

---

## 3. NAT Detection

The `macula_nat_detector` module classifies the local NAT environment by probing
external observers (relays or well-known public endpoints).

### 3.1 Detection Algorithm

1. Send `NAT_PROBE` (0x50) to primary observer (relay or gateway).
2. Receive `NAT_PROBE_REPLY` (0x51) containing reflexive address (public IP:port
   as seen from outside).
3. Send `NAT_PROBE` to secondary observer (different relay or public endpoint).
4. Compare reflexive addresses:
   - Same IP + same port for both observers -> **EI mapping**
   - Same IP + different port -> **HD mapping**
   - Different IP -> **PD mapping** (or multiple NATs)

Filtering and allocation are determined by response patterns to unsolicited
packets and port delta analysis.

### 3.2 NAT Profile

Detection produces a NAT profile map:

```erlang
#{
    mapping => ei,                  %% ei | hd | pd
    filtering => pd,                %% ei | hd | pd
    allocation => pp,               %% pp | pc | rd
    public_ip => {203, 0, 113, 5},
    public_port => 4433,
    detected_at => 1700000000,
    confidence => high              %% high | medium | low
}
```

### 3.3 API

```erlang
%% Detect NAT type (returns cached if available)
{ok, Profile} = macula_nat_detector:detect().

%% Force fresh detection (bypasses cache)
{ok, Profile} = macula_nat_detector:detect(#{force => true}).

%% Get cached local profile (no network call)
{ok, Profile} = macula_nat_detector:get_local_profile().

%% Add external observation for refinement
macula_nat_detector:add_observation({198, 51, 100, 1}, 5000).

%% Trigger background refresh (useful after network change)
macula_nat_detector:refresh().

%% Subscribe to profile changes
macula_nat_detector:subscribe(self()).
%% Receives: {nat_profile_changed, OldProfile, NewProfile}
```

### 3.4 NAT Cache

`macula_nat_cache` stores NAT profiles with TTL and stale-while-revalidate:

```erlang
%% Fresh hit
{ok, Profile} = macula_nat_cache:get(NodeId).

%% Stale but usable (background refresh triggered)
{stale, Profile} = macula_nat_cache:get(NodeId).

%% Not cached
{error, not_found} = macula_nat_cache:get(UnknownNodeId).

%% Store with default TTL (300s) or custom TTL
ok = macula_nat_cache:put(NodeId, Profile).
ok = macula_nat_cache:put(NodeId, Profile, 600).

%% Invalidate
ok = macula_nat_cache:invalidate(NodeId).

%% Statistics
#{size := _, hits := _, misses := _, stale_hits := _, evictions := _}
    = macula_nat_cache:stats().
```

### 3.5 Supervision Tree

The NAT subsystem runs under `macula_nat_system` (one_for_one):

| Child | Purpose |
|-------|---------|
| `macula_nat_cache` | ETS-based profile cache with TTL |
| `macula_nat_detector` | Local NAT classification via observers |
| `macula_nat_coordinator` | Coordinates hole punch attempts |
| `macula_port_predictor` | Predicts external ports for PC-allocation NATs |
| `macula_relay_registry` | Tracks available relays in DHT |
| `macula_relay_node` | Relay server functionality (if enabled) |

---

## 4. Configuration

### 4.1 Application Configuration (sys.config)

```erlang
{macula, [
    {nat, [
        %% Enable/disable NAT subsystem
        {enabled, true},

        {detection, [
            {timeout, 5000},            %% Detection timeout (ms)
            {min_observers, 2},         %% Minimum observers needed
            {auto_detect, true},        %% Detect on startup
            {refresh_interval, 300000}  %% Background refresh (ms, 0 = manual)
        ]},

        {cache, [
            {max_entries, 10000},
            {ttl, 300},                 %% Seconds
            {stale_grace, 60},          %% Stale-while-revalidate (seconds)
            {cleanup_interval, 60000},  %% Expired entry cleanup (ms)
            {publish_to_dht, true}      %% Publish local profile to DHT
        ]},

        {hole_punch, [
            {enabled, true},
            {max_attempts, 3},
            {attempt_timeout, 5000},
            {coordination_timeout, 10000},
            {port_prediction, true},
            {simultaneous_open, true}
        ]},

        {relay, [
            {enabled, false},           %% Enable relay server (nodes only)
            {max_sessions, 100},
            {bandwidth_limit, 1048576}, %% Per-session (bytes/s)
            {total_bandwidth_limit, 10485760},
            {session_timeout, 1800000}, %% 30 minutes
            {auto_register, true}
        ]},

        {connection, [
            {prefer_direct, true},      %% NOTE: relay is always primary for WAN
            {allow_relay, true},
            {timeout, 15000},
            {max_retries, 3},
            {strategy_order, [direct, punch, relay]}
        ]}
    ]}
]}.
```

### 4.2 Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `MACULA_NAT_ENABLED` | `true` | Enable NAT subsystem |
| `MACULA_NAT_DETECTION_TIMEOUT` | `5000` | Detection timeout (ms) |
| `MACULA_NAT_CACHE_TTL` | `300` | Cache TTL (seconds) |
| `MACULA_NAT_CACHE_MAX_ENTRIES` | `10000` | Max cached profiles |
| `MACULA_NAT_PUNCH_ENABLED` | `true` | Enable hole punching |
| `MACULA_NAT_PUNCH_MAX_ATTEMPTS` | `3` | Max punch attempts |
| `MACULA_NAT_RELAY_ENABLED` | `false` | Enable relay server |
| `MACULA_NAT_RELAY_MAX_SESSIONS` | `100` | Max relay sessions |
| `MACULA_NAT_RELAY_BANDWIDTH` | `1048576` | Bandwidth limit (bytes/s) |

### 4.3 Configuration Profiles

**Edge Node (typical -- behind residential/mobile NAT):**

```erlang
{macula, [{nat, [
    {detection, [{auto_detect, true}, {refresh_interval, 300000}]},
    {hole_punch, [{enabled, true}]},
    {relay, [{enabled, false}]}
]}]}.
```

**Relay Server (public IP, serves as relay):**

```erlang
{macula, [{nat, [
    {detection, [{auto_detect, false}]},
    {relay, [
        {enabled, true},
        {max_sessions, 500},
        {bandwidth_limit, 5242880},
        {total_bandwidth_limit, 104857600},
        {auto_register, true}
    ]}
]}]}.
```

**Enterprise Node (behind corporate firewall):**

```erlang
{macula, [{nat, [
    {detection, [{auto_detect, true}, {timeout, 10000}]},
    {hole_punch, [{enabled, true}, {max_attempts, 5}]},
    {connection, [{prefer_direct, false}, {timeout, 30000}]}
]}]}.
```

**Mobile Device (frequent network changes):**

```erlang
{macula, [{nat, [
    {detection, [{auto_detect, true}, {refresh_interval, 60000}]},
    {cache, [{ttl, 120}]},
    {connection, [{timeout, 20000}, {max_retries, 5}]}
]}]}.
```

### 4.4 Runtime Configuration

```erlang
%% Update at runtime
macula_config:set(nat_cache_ttl, 600).
macula_config:set(nat_relay_enabled, true).

%% Per-connection overrides
{ok, Conn} = macula_nat_connector:connect(TargetNodeId, #{
    timeout => 30000,
    prefer_direct => false,
    max_attempts => 5
}).
```

---

## 5. LAN Optimization

When two nodes are on the same local network, direct QUIC connections bypass
the relay mesh for lower latency. This is a **performance optimization**, not
the primary connectivity path.

### How LAN Discovery Works

1. Nodes announce via mDNS on the local network.
2. `macula_nat_detector` identifies the local NAT profile.
3. If two nodes detect they share a LAN (same public IP, mDNS-visible), they
   establish a direct QUIC connection.
4. Relay connection remains as fallback if the LAN path fails.

### When LAN Direct Is Used

| Scenario | Path |
|----------|------|
| Two nodes on same home/office network | LAN direct (mDNS) |
| Two nodes on different networks | Relay mesh |
| LAN direct fails or times out | Automatic relay fallback |

---

## 6. Advanced: Hole Punching

Hole punching is a **legacy capability** retained for edge cases where relay
bandwidth is constrained and both nodes have compatible NAT types. In the
relay-first architecture, hole punching is rarely needed.

### When Hole Punching Is Attempted

The `macula_nat_coordinator` only attempts hole punching when:

1. Both nodes have EI or HD mapping (predictable external addresses).
2. At least one has PP or PC allocation (predictable ports).
3. The relay path has been identified as a bottleneck.

If either node has symmetric NAT (PD+PD+RD), hole punching is skipped entirely
and the relay handles routing.

### Hole Punch Flow

1. Coordinator fetches both nodes' NAT profiles from cache/DHT.
2. Determines compatibility (see table in Section 2.4).
3. Signals both nodes simultaneously via relay to begin punch.
4. Both nodes send UDP packets to each other's predicted external address.
5. NAT mappings are created; QUIC session established over the punched hole.
6. If punch fails after `max_attempts`, relay remains the path.

### NAT Compatibility for Hole Punching

| Local | Remote | Strategy |
|-------|--------|----------|
| EI+EI | EI+EI | Direct or simple punch |
| EI+* | HD+* | Punch with timing |
| EI+* | PD+PP | Punch with port prediction |
| PD+PD+RD | any | Relay only (no punch) |
| any | PD+PD+RD | Relay only (no punch) |

### Protocol Messages (0x50-0x5F)

| Type | ID | Purpose |
|------|-----|---------|
| NAT_PROBE | 0x50 | Request reflexive address from observer |
| NAT_PROBE_REPLY | 0x51 | Return reflexive address |
| PUNCH_REQUEST | 0x52 | Request hole punch coordination |
| PUNCH_COORDINATE | 0x53 | Synchronized punch timing |
| PUNCH_RESULT | 0x55 | Report punch success/failure |
| RELAY_REQUEST | 0x56 | Request relay tunnel |
| RELAY_DATA | 0x57 | Relayed data frame |

---

## 7. Monitoring and Troubleshooting

### Metrics

```erlang
#{
    detection => #{total => 150, successful => 145, failed => 5, avg_latency_ms => 250},
    cache => #{size => 500, hits => 10000, misses => 200, hit_ratio => 0.98},
    punch => #{attempts => 1000, successful => 850, failed => 150, success_rate => 0.85},
    relay => #{active_sessions => 25, bytes_relayed => 1073741824, sessions_total => 500}
} = macula_nat_system:metrics().
```

### Performance Reference

| Operation | Typical Latency |
|-----------|-----------------|
| Cached profile lookup | < 1ms |
| Fresh detection (2 observers) | 200-400ms |
| DHT profile fetch | 100-300ms |
| Hole punch coordination | 200-500ms |

### Common Issues

| Issue | Cause | Solution |
|-------|-------|----------|
| Detection always fails | No reachable observers | Verify relay/gateway connectivity |
| High relay usage | Symmetric NAT or CGNAT | Expected behavior -- relay-first is correct |
| Cache misses | TTL too short | Increase `cache.ttl` |
| Memory growth | Cache too large | Reduce `cache.max_entries` |

### Debug Logging

```erlang
logger:set_module_level(macula_nat_detector, debug).
logger:set_module_level(macula_nat_cache, debug).
logger:set_module_level(macula_nat_coordinator, debug).
logger:set_module_level(macula_relay_node, debug).
```
