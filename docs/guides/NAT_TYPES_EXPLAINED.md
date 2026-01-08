# NAT Types Explained

Understanding NAT (Network Address Translation) types is essential for building P2P applications that work across different network environments. This guide explains the NAT classification model used in Macula based on the NATCracker methodology.

![NAT Types Overview](assets/nat_types.svg)

---

## Why NAT Matters for P2P

The challenge with P2P: Neither peer can initiate a connection to the other because NATs block unsolicited incoming traffic. Macula solves this by detecting NAT characteristics and choosing optimal traversal strategies.

---

## NAT Policy Classification

Macula uses the NATCracker 3-policy classification model to characterize NAT behavior. Each NAT is described by three policies:

### 1. Mapping Policy (How NAT assigns external addresses)

| Policy | Code | Behavior | Prevalence |
|--------|------|----------|------------|
| **Endpoint-Independent** | EI | Same external addr for all destinations | ~52% |
| **Host-Dependent** | HD | Different external addr per destination host | ~12% |
| **Port-Dependent** | PD | Different external addr per destination host:port | ~36% |

**Example - Endpoint-Independent (EI):**
```
Local: 192.168.1.10:5000
  -> Destination A: 8.8.8.8:53     => NAT maps to 203.0.113.5:40000
  -> Destination B: 1.1.1.1:53     => NAT maps to 203.0.113.5:40000  (same!)
```

**Example - Port-Dependent (PD):**
```
Local: 192.168.1.10:5000
  -> Destination A: 8.8.8.8:53     => NAT maps to 203.0.113.5:40000
  -> Destination B: 1.1.1.1:53     => NAT maps to 203.0.113.5:40001  (different!)
```

### 2. Filtering Policy (What incoming traffic NAT accepts)

| Policy | Code | Behavior | Security |
|--------|------|----------|----------|
| **Endpoint-Independent** | EI | Accepts from any source | Low |
| **Host-Dependent** | HD | Accepts from hosts we've contacted | Medium |
| **Port-Dependent** | PD | Accepts from host:port we've contacted | High |

**Example - Port-Dependent filtering:**
```
Local 192.168.1.10:5000 sends to 8.8.8.8:53
NAT now accepts incoming on 203.0.113.5:40000 ONLY from 8.8.8.8:53
  -> 8.8.8.8:53     => ALLOWED
  -> 8.8.8.8:80     => BLOCKED (wrong port)
  -> 1.1.1.1:53     => BLOCKED (wrong host)
```

### 3. Allocation Policy (How NAT chooses external ports)

| Policy | Code | Behavior | Predictability |
|--------|------|----------|----------------|
| **Port-Preservation** | PP | external_port = local_port | High |
| **Port-Contiguity** | PC | external_port = last_port + delta | Medium |
| **Random** | RD | No predictable pattern | None |

**Example - Port-Preservation (PP):**
```
Local: 192.168.1.10:5000  => NAT: 203.0.113.5:5000  (same port!)
```

**Example - Port-Contiguity (PC):**
```
Local: 192.168.1.10:5000  => NAT: 203.0.113.5:40000
Local: 192.168.1.10:5001  => NAT: 203.0.113.5:40001  (delta = 1)
```

---

## Common NAT Type Combinations

Based on NATCracker research across millions of NATs:

| Type | Mapping | Filtering | Allocation | Prevalence | Direct P2P |
|------|---------|-----------|------------|------------|------------|
| Full Cone | EI | EI | PP | 15% | Yes |
| Restricted Cone | EI | HD | PP | 37% | With punch |
| Port Restricted | EI | PD | PP | 20% | With punch |
| Symmetric | PD | PD | RD | 12% | No (relay) |
| CGNAT | varies | PD | varies | 16% | Usually relay |

### Full Cone NAT (EI, EI, PP) - Best Case

Any external host can send to this address after ANY outbound packet from local peer. **Direct P2P: YES** - Any peer can connect directly.

### Restricted Cone NAT (EI, HD, PP) - Good

Only hosts we've contacted can send back (but from any port). **Direct P2P: YES** with hole punching.

### Symmetric NAT (PD, PD, RD) - Worst Case

Each destination gets different external address. External port is random and unpredictable. **Direct P2P: NO** - must use relay.

---

## NAT Detection in Macula

Macula detects NAT type automatically using `macula_nat_detector`:

```erlang
%% Get local NAT profile
{ok, Profile} = macula_nat_detector:get_local_profile().

%% Profile contains:
#{
    mapping => ei,           % Endpoint-Independent
    filtering => pd,         % Port-Dependent
    allocation => pp,        % Port-Preservation
    public_ip => {203,0,113,5},
    public_port => 5000,
    detected_at => 1700000000
}
```

### Detection Algorithm

1. **Send NAT_PROBE to primary observer** (gateway/public peer)
   - Receive reflexive address (your public IP:port as seen from outside)

2. **Send NAT_PROBE to secondary observer** (different public peer)
   - Compare reflexive addresses

3. **Classification:**
   - Same address for both observers -> EI mapping
   - Same IP, different port -> HD mapping
   - Different IP -> PD mapping (or multiple NATs)

---

## Connection Strategy Decision Tree

The diagram at the top of this document shows the connection strategy matrix. Macula's `macula_nat_coordinator` follows this logic:

1. **Either has public IP?** → Direct connection to public peer
2. **Both have EI mapping?** → Hole punching possible
   - EI filtering → Simple hole punch
   - PD filtering → Coordinated hole punch
3. **Either has PD+PD+RD (symmetric)?** → Must use relay
4. **Otherwise** → Try hole punch with port prediction

---

## Hole Punching Explained

Hole punching creates NAT mappings that allow peers to communicate:

| Time | Event | Result |
|------|-------|--------|
| T0 | Initial state | Peer A and B have no mappings to each other |
| T1 | Coordinator signal | Both peers simultaneously send packets to each other's predicted external address |
| T2 | NAT processing | Both NATs create outbound mappings for the other peer's address |
| T3 | Connection established | Subsequent packets pass through created mappings |

**Requirements for successful hole punch:**
- Both NATs have EI or HD mapping (predictable external address)
- At least one has PP or PC allocation (predictable port)
- Timing coordination within ~100ms

---

## CGNAT (Carrier-Grade NAT)

ISPs increasingly use CGNAT, adding another NAT layer:

| Layer | Address | Role |
|-------|---------|------|
| Your Device | 192.168.1.10 | Local network address |
| Home Router NAT | → 10.0.0.50 | ISP private address (RFC 1918) |
| CGNAT | → 203.0.113.5 | Public address (shared with other customers) |
| Internet | | Global routing |

CGNAT complications:
- Multiple customers share same public IP
- Often uses PD filtering (restrictive)
- Hole punching success rate drops to ~40%
- Relay fallback frequently needed

---

## Best Practices

### For Application Developers

1. **Always have relay fallback** - Some NATs cannot be traversed
2. **Detect NAT type early** - Cache profile at peer startup
3. **Prefer EI-mapping peers as coordinators** - Better success rate

### For Network Operators

1. **Use Full Cone or Restricted Cone NAT** - Best P2P compatibility
2. **Enable UPnP/NAT-PMP** - Allows applications to request mappings
3. **Avoid Symmetric NAT** - Breaks most P2P protocols

---

## Further Reading

- [RFC 5780 - NAT Behavior Discovery](https://tools.ietf.org/html/rfc5780)
- [NATCracker Paper](https://www.usenix.org/conference/nsdi21/presentation/tang) - 27 NAT type classification

---

**See Also:**
- [NAT Traversal Developer Guide](NAT_TRAVERSAL_DEVELOPER_GUIDE.md) - API usage and code examples
