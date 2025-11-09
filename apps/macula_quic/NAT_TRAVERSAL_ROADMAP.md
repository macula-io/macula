# NAT Traversal Roadmap for Macula QUIC

**Status**: Phase 4-5 Feature (Post-MVP)
**Priority**: Medium (Required for true P2P mesh without hub)

---

## Current Status

### ✅ What Works Today

**Hub-Spoke Architecture** (Current PoC):
- Apps connect TO hub (outbound connections)
- Hub has public IP/DNS endpoint
- NAT traversal NOT required
- All edge nodes behind NAT can connect fine

**Connection Model**:
```
[Edge App (NAT'd)] --outbound--> [Hub (Public IP)] <--outbound-- [Edge App (NAT'd)]
```

### ❌ What Doesn't Work Today

**True Peer-to-Peer** (No Hub):
- Direct connections between NAT'd peers
- Symmetric NAT scenarios
- Restrictive firewall environments

**Connection Model** (Blocked):
```
[Peer A (NAT'd)] <--X--> [Peer B (NAT'd)]  ❌ Cannot establish direct connection
```

---

## Problem Statement

**QUIC uses UDP**, which faces NAT challenges:

1. **Inbound UDP Blocking**: Many NATs block unsolicited inbound UDP packets
2. **Port Mapping**: NATs may change source ports unpredictably
3. **Symmetric NAT**: Most restrictive - different mapping per destination
4. **Firewall Rules**: Corporate/ISP firewalls may block UDP entirely

**WebSocket (TCP) handled this differently**:
- Outbound TCP connections usually allowed
- Relied on central router (Bondy) for all routing
- No peer-to-peer, so no NAT traversal needed

**For true P2P mesh**, we need NAT traversal solutions.

---

## Solution Approaches

### Option 1: ICE-like Protocol (Best for P2P)

**Inspired by WebRTC ICE (Interactive Connectivity Establishment)**

**How it works**:
1. **STUN (Session Traversal Utilities for NAT)**:
   - Discover public IP:port mapping
   - Detect NAT type
   - Exchange candidates

2. **TURN (Traversal Using Relays around NAT)**:
   - Fallback relay server when direct connection fails
   - Acts as proxy for traffic

3. **Connection Negotiation**:
   - Try direct connection first
   - Fall back to relay if needed

**Components to implement**:
```erlang
%% apps/macula_nat/src/macula_stun.erl
-module(macula_stun).
-export([discover_mapping/1, get_candidates/1]).

%% Discover public IP:port via STUN server
discover_mapping(StunServer) ->
    {ok, {PublicIP, PublicPort}}.

%% apps/macula_nat/src/macula_ice.erl
-module(macula_ice).
-export([negotiate/2, gather_candidates/0]).

%% Gather connection candidates (host, srflx, relay)
gather_candidates() ->
    [
        {host, LocalIP, LocalPort},
        {srflx, PublicIP, PublicPort},  % Server reflexive (via STUN)
        {relay, RelayIP, RelayPort}     % Via TURN
    ].

%% Negotiate best connection path
negotiate(LocalCandidates, RemoteCandidates) ->
    %% Try candidates in priority order
    %% Return best working path
    {ok, Connection}.
```

**Configuration**:
```erlang
%% In macula_sdk or macula_gateway config
{nat_traversal, [
    {stun_servers, [
        "stun.macula.io:3478",
        "stun.l.google.com:19302"  % Fallback
    ]},
    {turn_servers, [
        {url, "turn.macula.io:3478"},
        {username, "macula"},
        {credential, "secret"}
    ]},
    {ice_timeout_ms, 5000}
]}.
```

**Pros**:
- Industry standard (proven in WebRTC)
- Handles most NAT scenarios
- Graceful fallback to relay

**Cons**:
- Complex implementation
- Requires infrastructure (STUN/TURN servers)
- Additional latency during negotiation

---

### Option 2: Relay Nodes (Simpler, Macula-native)

**Inspired by Tailscale/Nebula**

**How it works**:
- Some nodes act as relays (public IPs or already-connected peers)
- NAT'd peers route through relay nodes
- Hub can act as default relay

**Components to implement**:
```erlang
%% apps/macula_routing/src/macula_relay.erl
-module(macula_relay).
-export([register_relay/1, route_via_relay/3]).

%% Register as relay node
register_relay(NodeCapabilities) ->
    case is_publicly_reachable() of
        true ->
            advertise_relay_capability(),
            {ok, relay};
        false ->
            {ok, client}
    end.

%% Route message via relay
route_via_relay(Destination, Message, RelayNode) ->
    %% Forward through relay
    ok.
```

**Connection Strategy**:
```
1. Try direct connection
2. If timeout, query for relay nodes
3. Establish path: Peer A → Relay → Peer B
4. Monitor connection, upgrade to direct if NAT opens
```

**Pros**:
- Simpler than ICE
- No external infrastructure needed (use existing nodes)
- Integrates with existing Macula topology

**Cons**:
- Higher latency (extra hop)
- Relay nodes need bandwidth
- Single point of failure per relay

---

### Option 3: Hybrid Hub-Relay

**Current hub + relay capabilities**

**How it works**:
- Hub always available as relay
- Peers can also become relays
- Automatic fallback hierarchy

**Implementation**:
```erlang
%% Connection priority:
%% 1. Direct connection (if both have public IPs)
%% 2. Direct via STUN hole-punch (if both support STUN)
%% 3. Relay via peer relay node (if available)
%% 4. Relay via hub (always available)

{connection_strategy, [
    {direct, #{priority => 100, timeout => 2000}},
    {stun, #{priority => 80, timeout => 3000}},
    {peer_relay, #{priority => 60, timeout => 2000}},
    {hub_relay, #{priority => 40, always_available => true}}
]}.
```

**Pros**:
- Best of both worlds
- Graceful degradation
- Works in all scenarios

**Cons**:
- Most complex
- Requires implementing both approaches

---

## Recommended Implementation Plan

### Phase 1: Hub Relay (Simplest, Immediate)

**Timeline**: 1-2 days
**Effort**: Low
**Value**: High (enables current PoC)

**What to build**:
1. Hub always acts as relay for NAT'd peers
2. Update `macula_gateway` to forward messages between peers
3. Update `macula_sdk` to route via hub when direct fails

**Code**:
```erlang
%% In macula_gateway
handle_info({forward_message, From, To, Message}, State) ->
    %% Look up destination peer
    case maps:get(To, State#state.peers) of
        undefined ->
            {error, peer_not_found};
        ToPid ->
            ToPid ! {message, From, Message},
            {noreply, State}
    end.
```

### Phase 2: STUN Support (Enable Direct P2P)

**Timeline**: 3-5 days
**Effort**: Medium
**Value**: High (true P2P when possible)

**What to build**:
1. Create `apps/macula_nat` app
2. Implement STUN client (RFC 5389)
3. Integrate with `macula_quic` connection establishment
4. Deploy public STUN server (or use Google's)

**Libraries to consider**:
- Use existing Erlang STUN libraries if available
- Or implement minimal subset (BINDING request/response)

### Phase 3: TURN Support (Full NAT Traversal)

**Timeline**: 5-7 days
**Effort**: High
**Value**: Medium (handles restrictive NATs)

**What to build**:
1. Implement TURN client (RFC 5766)
2. Deploy TURN server infrastructure
3. Credential management
4. Bandwidth accounting

### Phase 4: Connection Negotiation (ICE-like)

**Timeline**: 7-10 days
**Effort**: High
**Value**: High (production-ready)

**What to build**:
1. Candidate gathering
2. Priority calculation
3. Connectivity checks
4. Best-path selection

---

## Testing Strategy

### Test Scenarios

1. **Both peers public** → Direct connection
2. **One NAT'd** → Direct to public peer, relay from NAT'd
3. **Both NAT'd (same network)** → Local network direct
4. **Both NAT'd (different networks)** → STUN or relay
5. **Symmetric NAT** → TURN relay
6. **UDP blocked** → Fallback to TCP (future)

### Test Infrastructure

```
┌─────────────────────────────────────────────┐
│ Test Environment                            │
│                                             │
│  [Public Server]  [NAT Router 1]  [NAT 2]  │
│        │                │            │      │
│        │          [Peer A]      [Peer B]    │
│        │                                    │
│  [STUN Server]   [TURN Server]             │
└─────────────────────────────────────────────┘
```

---

## Infrastructure Requirements

### For Phase 2-3 (STUN/TURN)

**STUN Server**:
- Low bandwidth (~1KB per request)
- Can use public servers (Google, etc.)
- Or deploy coturn server

**TURN Server**:
- High bandwidth (relays all traffic)
- Needs authentication
- Should be geographically distributed
- **Cost**: ~$50-100/month for small deployment

**Deployment**:
```yaml
# Kubernetes deployment for coturn
apiVersion: apps/v1
kind: Deployment
metadata:
  name: macula-stun-turn
spec:
  replicas: 2
  template:
    spec:
      containers:
      - name: coturn
        image: coturn/coturn:latest
        ports:
        - containerPort: 3478  # STUN/TURN
          protocol: UDP
        - containerPort: 3478
          protocol: TCP
```

---

## Metrics to Track

Once implemented, monitor:

1. **Connection Success Rate**:
   - Direct connections: %
   - STUN connections: %
   - TURN relayed: %
   - Failed: %

2. **Latency**:
   - Direct: < 50ms (target)
   - STUN: < 100ms (target)
   - TURN: < 200ms (target)

3. **Bandwidth**:
   - TURN relay bandwidth usage
   - Cost per GB relayed

4. **NAT Types Encountered**:
   - Full cone
   - Restricted cone
   - Port-restricted
   - Symmetric

---

## Decision Matrix

| Scenario | Current (Hub) | +STUN | +TURN |
|----------|--------------|-------|-------|
| Both public IPs | Relay via hub | ✅ Direct | ✅ Direct |
| One NAT'd | Relay via hub | ✅ Direct | ✅ Direct |
| Both NAT'd (cone) | Relay via hub | ✅ STUN direct | ✅ STUN direct |
| Both NAT'd (symmetric) | Relay via hub | Relay via hub | ✅ TURN relay |
| UDP blocked | ❌ Fails | ❌ Fails | ✅ TCP fallback* |

\* TCP fallback = future enhancement

---

## References

- **RFC 5389**: STUN (Session Traversal Utilities for NAT)
- **RFC 5766**: TURN (Traversal Using Relays around NAT)
- **RFC 8445**: ICE (Interactive Connectivity Establishment)
- **WebRTC NAT Traversal**: https://webrtc.org/getting-started/peer-connections
- **Tailscale NAT Traversal**: https://tailscale.com/blog/how-nat-traversal-works/
- **QUIC NAT Considerations**: https://www.rfc-editor.org/rfc/rfc9308.html

---

## Current Action Items

- [ ] Phase 1: Implement hub relay (basic message forwarding)
- [ ] Deploy STUN server (or use public)
- [ ] Phase 2: STUN client implementation
- [ ] Phase 3: TURN server deployment
- [ ] Phase 4: Full ICE implementation

**Next Step**: Continue with current hub-based architecture (naturally NAT-friendly) ✅
