# Macula HTTP/3 Mesh - API Reference

**Complete Erlang/Elixir API documentation**

**Status**: 🚧 SKELETON - Needs completion
**Priority**: P1
**Estimated effort**: 12 hours

---

## Overview

⚠️ **TODO**: Introduction to Macula APIs, conventions, return value patterns.

---

## Core Module: `macula`

### `macula:start/0,1`

⚠️ **TODO**: Document node startup.

**Signature**:
```erlang
start() -> {ok, Pid} | {error, Reason}.
start(Opts) -> {ok, Pid} | {error, Reason}.
```

**Elixir**:
```elixir
Macula.start() :: {:ok, pid()} | {:error, reason()}
Macula.start(opts) :: {:ok, pid()} | {:error, reason()}
```

### `macula:stop/0`

⚠️ **TODO**: Document graceful shutdown.

### `macula:node_id/0`

⚠️ **TODO**: Get current node ID.

### `macula:version/0`

⚠️ **TODO**: Get Macula version.

### `macula:stats/0`

⚠️ **TODO**: Get node statistics.

---

## Pub/Sub Module: `macula_pubsub`

### `macula_pubsub:publish/2,3`

⚠️ **TODO**: Complete publish API.

**Signature**:
```erlang
publish(Topic, Payload) -> ok | {error, Reason}.
publish(Topic, Payload, Opts) -> ok | {error, Reason}.
```

**Options**:
- `acknowledge` - Wait for delivery confirmation
- `retain` - Store for late subscribers
- `ttl` - Message time-to-live

### `macula_pubsub:subscribe/1,2,3`

⚠️ **TODO**: Complete subscribe API.

### `macula_pubsub:unsubscribe/1,2`

⚠️ **TODO**: Complete unsubscribe API.

---

## RPC Module: `macula_rpc`

### `macula_rpc:call/2,3,4`

⚠️ **TODO**: Complete RPC call API.

### `macula_rpc:register/2,3`

⚠️ **TODO**: Complete RPC registration API.

### `macula_rpc:unregister/1`

⚠️ **TODO**: Complete RPC unregistration API.

---

## Connection Module: `macula_connection`

⚠️ **TODO**: Document connection management APIs.

---

## Topology Module: `macula_topology`

⚠️ **TODO**: Document topology APIs.

---

## Membership Module: `macula_membership`

⚠️ **TODO**: Document SWIM membership APIs.

---

## Routing Module: `macula_routing`

⚠️ **TODO**: Document DHT routing APIs.

---

## Gateway Module: `macula_gateway`

⚠️ **TODO**: Document gateway APIs for cross-realm communication.

---

## Configuration

⚠️ **TODO**: Document all configuration options.

**Sections needed**:
- Application environment variables
- Runtime configuration
- Config file format
- Default values

---

## Error Reference

⚠️ **TODO**: List all possible error returns and their meanings.

---

## Examples

⚠️ **TODO**: Add comprehensive code examples for common use cases.

---

**Last Updated**: 2025-01-08
**Contributors**: [Add names as sections are completed]
