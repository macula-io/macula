# Macula RPC Subsystem

This directory contains all modules related to the remote procedure call (RPC) subsystem.

## Overview

The RPC subsystem provides:
- Procedure registration and discovery via DHT
- Synchronous CALL with timeout
- Asynchronous CAST (fire-and-forget)
- NATS-style async RPC with callbacks
- Service provider load balancing
- Procedure caching for performance

## Modules

| Module | Purpose |
|--------|---------|
| `macula_rpc_handler.erl` | Main RPC handler - manages procedures and calls |
| `macula_rpc_server.erl` | Gen_server for RPC operations |
| `macula_rpc_dht.erl` | DHT integration for service discovery |
| `macula_rpc_router.erl` | RPC message routing |
| `macula_rpc_routing.erl` | Multi-hop RPC routing via DHT |
| `macula_rpc_executor.erl` | Local procedure execution |
| `macula_rpc_registry.erl` | Local procedure registry (macula_service_registry) |
| `macula_rpc_cache.erl` | Service provider cache |
| `macula_rpc_names.erl` | Procedure naming conventions |

## Architecture

```
┌─────────────────────────────────────────────────┐
│               macula_rpc_handler                │
│                 (gen_server)                    │
└──────────────────────┬──────────────────────────┘
                       │
       ┌───────────────┼───────────────┐
       │               │               │
┌──────▼──────┐ ┌──────▼──────┐ ┌──────▼──────┐
│   rpc_dht   │ │ rpc_registry │ │ rpc_executor │
│ (discovery) │ │  (local)    │ │   (invoke)   │
└─────────────┘ └─────────────┘ └──────────────┘
       │
       ▼
┌─────────────┐
│   DHT/      │
│ routing_    │
│  server     │
└─────────────┘
```

## Message Types

| Type | ID | Purpose |
|------|-----|---------|
| CALL | 0x20 | Synchronous procedure call |
| REPLY | 0x21 | Response to CALL |
| CAST | 0x22 | Async procedure call (no reply) |
| RPC_ROUTE | 0x23 | Multi-hop routed RPC |
| RPC_REQUEST | 0x24 | NATS-style async request |
| RPC_REPLY | 0x25 | NATS-style async response |

## Usage

### Register a procedure
```erlang
Handler = fun(Args) ->
    %% Process args, return result
    {ok, #{result => calculate(Args)}}
end,
macula_rpc_handler:register_local_procedure(RpcPid, <<"my.procedure">>, Handler).
```

### Make a synchronous call
```erlang
{ok, Result} = macula_rpc_handler:call(RpcPid, <<"my.procedure">>, Args, #{timeout => 5000}).
```

### Make an async request (NATS-style)
```erlang
Callback = fun(Result) ->
    case Result of
        {ok, Response} -> handle_response(Response);
        {error, Reason} -> handle_error(Reason)
    end
end,
{ok, RequestId} = macula_rpc_handler:request(RpcPid, <<"my.procedure">>, Args, #{callback => Callback}).
```

### Fire-and-forget cast
```erlang
ok = macula_rpc_handler:cast(RpcPid, <<"my.procedure">>, Args).
```

## Service Discovery

Procedure providers are discovered via DHT:

1. **Registration**: Provider stores `procedure → {node_id, endpoint}` in DHT
2. **Discovery**: Caller queries DHT for procedure providers
3. **Connection**: Direct QUIC connection to provider
4. **Caching**: Provider endpoints cached locally

## Error Handling

| Error | Description |
|-------|-------------|
| `{error, not_found}` | No provider for procedure |
| `{error, timeout}` | Call exceeded timeout |
| `{error, provider_error}` | Provider returned error |

## Configuration

| Key | Default | Description |
|-----|---------|-------------|
| `rpc_timeout` | 5000 | Default call timeout (ms) |
| `service_ttl` | 300 | Service advertisement TTL (s) |

## Tests

See: `test/macula_rpc_system/`

Run RPC tests:
```bash
rebar3 eunit --module=macula_rpc_handler_tests
rebar3 eunit --module=macula_async_rpc_tests
rebar3 eunit --module=macula_service_registry_tests
```
