# Macula SDK

**Client library for building distributed applications on the Macula HTTP/3 mesh platform.**

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](../../LICENSE)
[![Erlang/OTP](https://img.shields.io/badge/Erlang%2FOTP-26+-brightgreen)](https://www.erlang.org)

## Overview

The Macula SDK provides a simple, elegant API for connecting to Macula mesh networks over HTTP/3 (QUIC) transport. It enables applications written in Erlang or Elixir to:

- **Publish** events to topics (pub/sub)
- **Subscribe** to topics and receive events
- **Call** remote procedures (RPC)
- **Authenticate** using API keys or other methods
- **Handle** connection resilience and reconnection

## Features

- ✅ HTTP/3 (QUIC) transport for NAT/firewall-friendly connectivity
- ✅ Pub/Sub with topic-based routing
- ✅ RPC with request/response pattern
- ✅ Connection pooling for high throughput
- ✅ Automatic reconnection with exponential backoff
- ✅ Multi-tenancy via realm isolation
- ✅ Works in both Erlang and Elixir applications

## Installation

### Erlang (rebar3)

Add to your `rebar.config`:

```erlang
{deps, [
    {macula_sdk, {git, "https://github.com/macula-io/macula.git", {branch, "main"}}}
]}.
```

### Elixir (mix)

Add to your `mix.exs`:

```elixir
def deps do
  [
    {:macula_sdk, github: "macula-io/macula", sparse: "apps/macula_sdk"}
  ]
end
```

## Quick Start

### Erlang

```erlang
%% Connect to a Macula mesh
{ok, Client} = macula_sdk:connect(<<"https://mesh.example.com:443">>, #{
    realm => <<"my.app.realm">>,
    auth => #{api_key => <<"your-api-key">>}
}).

%% Publish an event
ok = macula_sdk:publish(Client, <<"my.app.user.registered">>, #{
    user_id => <<"user-123">>,
    email => <<"user@example.com">>,
    timestamp => erlang:system_time(millisecond)
}).

%% Subscribe to events
{ok, SubRef} = macula_sdk:subscribe(Client, <<"my.app.user.registered">>,
    fun(Event) ->
        io:format("User registered: ~p~n", [Event]),
        ok
    end).

%% Make an RPC call
{ok, User} = macula_sdk:call(Client, <<"my.app.get_user">>, #{
    user_id => <<"user-123">>
}).

%% Disconnect when done
ok = macula_sdk:disconnect(Client).
```

### Elixir

```elixir
# Connect to a Macula mesh
{:ok, client} = :macula_sdk.connect("https://mesh.example.com:443", %{
  realm: "my.app.realm",
  auth: %{api_key: "your-api-key"}
})

# Publish an event
:ok = :macula_sdk.publish(client, "my.app.user.registered", %{
  user_id: "user-123",
  email: "user@example.com",
  timestamp: System.system_time(:millisecond)
})

# Subscribe to events
{:ok, sub_ref} = :macula_sdk.subscribe(client, "my.app.user.registered", fn event ->
  IO.puts("User registered: #{inspect(event)}")
  :ok
end)

# Make an RPC call
{:ok, user} = :macula_sdk.call(client, "my.app.get_user", %{user_id: "user-123"})

# Disconnect when done
:ok = :macula_sdk.disconnect(client)
```

## Topic Design Principles

**IMPORTANT:** Follow these topic design rules for scalable pub/sub:

❌ **WRONG - Entity IDs in topics:**
```erlang
%% BAD: Creates topic explosion as users scale
macula_sdk:publish(Client, <<"my.app.user.123.registered">>, Data).
macula_sdk:publish(Client, <<"my.app.user.456.registered">>, Data).
```

✅ **CORRECT - Event types in topics, IDs in payload:**
```erlang
%% GOOD: Single topic, ID in payload
macula_sdk:publish(Client, <<"my.app.user.registered">>, #{
    user_id => <<"123">>,  %% ← ID goes in payload
    data => ...
}).
```

**Why?**
- Topics describe EVENT TYPES, not entity instances
- Prevents topic explosion (1000 users = 1000 topics ❌)
- Allows subscribing to "all events of this type" easily
- Better router performance

## Architecture

The SDK is built on Macula's core libraries:

- `macula_quic` - HTTP/3/QUIC transport layer
- `macula_protocol` - Mesh protocol implementation
- `macula_sdk` - High-level client API (this library)

```
Application
    ↓
macula_sdk (API)
    ↓
macula_sdk_client (connection management)
    ↓
macula_quic (HTTP/3 transport)
    ↓
Macula Mesh Network
```

## Configuration

### Connection Options

```erlang
#{
    %% Required
    realm => <<"my.app.realm">>,  % Realm identifier

    %% Optional
    auth => #{
        api_key => <<"secret-key">>  % API key authentication
    },
    timeout => 5000,              % Connection timeout (ms)
    pool_size => 5,               % Connection pool size
    reconnect => true,            % Auto-reconnect on disconnect
    reconnect_delay => 1000       % Initial reconnect delay (ms)
}
```

## Development Status

**Current Phase:** Foundation - API design and skeleton complete

**Roadmap:**
- [ ] Phase 1: HTTP/3 connection establishment
- [ ] Phase 2: Pub/sub operations
- [ ] Phase 3: RPC operations
- [ ] Phase 4: Connection pooling
- [ ] Phase 5: Authentication
- [ ] Phase 6: Reconnection logic
- [ ] Phase 7: Metrics and telemetry

## Testing

```bash
# Run tests
rebar3 eunit

# Run with coverage
rebar3 cover
```

## Documentation

Generate full documentation:

```bash
rebar3 ex_doc
```

## License

Apache License 2.0 - See [LICENSE](../../LICENSE) for details.

## Related Projects

- [macula](https://github.com/macula-io/macula) - HTTP/3 mesh platform
- [macula_quic](../macula_quic) - QUIC transport library
- [macula-architecture](https://github.com/macula-io/macula-architecture) - Platform architecture docs
