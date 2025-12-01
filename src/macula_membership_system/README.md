# Macula Membership Subsystem

This directory contains all modules related to the SWIM-based membership and failure detection subsystem.

## Overview

The membership subsystem implements:
- SWIM protocol for failure detection
- Gossip-based membership propagation
- Node health monitoring
- Membership list management
- Failure detection with configurable timeouts

## Modules

| Module | Purpose |
|--------|---------|
| `macula_membership_list.erl` | Membership list management (alive, suspect, dead nodes) |
| `macula_membership_gossip.erl` | Gossip protocol for membership updates |
| `macula_membership_detector.erl` | Failure detection using SWIM pings |
| `macula_membership_member.erl` | Individual member state and metadata |

## Architecture

```
┌─────────────────────────────────────────────────┐
│            macula_membership_list               │
│               (gen_server)                      │
└──────────────────────┬──────────────────────────┘
                       │
       ┌───────────────┼───────────────┐
       │               │               │
┌──────▼──────┐ ┌──────▼──────┐ ┌──────▼──────┐
│  detector   │ │   gossip    │ │   member    │
│ (SWIM ping) │ │ (propagate) │ │  (state)    │
└─────────────┘ └─────────────┘ └─────────────┘
```

## SWIM Protocol

The Scalable Weakly-consistent Infection-style Membership (SWIM) protocol:

1. **Direct Ping**: Probe random member
2. **Indirect Ping**: If direct fails, ask other members to probe
3. **Suspect**: Mark unresponsive nodes as suspect
4. **Dead**: After timeout, mark suspect nodes as dead

## Message Types

| Type | ID | Purpose |
|------|-----|---------|
| SWIM_PING | 0x30 | Direct health check |
| SWIM_ACK | 0x31 | Response to ping |
| SWIM_PING_REQ | 0x32 | Indirect ping request |

## Member States

```
┌─────────┐  timeout  ┌──────────┐  timeout  ┌──────┐
│  ALIVE  │─────────→│ SUSPECT  │──────────→│ DEAD │
└────┬────┘          └────┬─────┘           └──────┘
     │                    │
     │◄───────────────────┘
          ACK received
```

## Configuration

| Key | Default | Description |
|-----|---------|-------------|
| `ping_interval` | 1000 | Ping interval (ms) |
| `ping_timeout` | 500 | Ping timeout (ms) |
| `suspect_timeout` | 3000 | Time before suspect → dead (ms) |
| `indirect_ping_count` | 3 | Members to ask for indirect ping |

## Usage

### Add a member
```erlang
macula_membership_list:add_member(ListPid, MemberInfo).
```

### Check membership
```erlang
Members = macula_membership_list:get_alive_members(ListPid).
```

### Get member state
```erlang
{ok, State} = macula_membership_list:get_member_state(ListPid, NodeId).
%% State :: alive | suspect | dead
```

## Integration

The membership subsystem integrates with:
- **Routing System**: Updates routing table when members change
- **Gateway System**: Tracks connected peers
- **DHT**: Node discovery and routing

## Tests

See: `test/macula_membership_system/`

Run membership tests:
```bash
rebar3 eunit --module=macula_membership_list_tests
rebar3 eunit --module=macula_membership_detector_tests
```
