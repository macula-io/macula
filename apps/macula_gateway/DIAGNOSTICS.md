# Macula Gateway Diagnostics Service

The Macula Gateway includes a built-in diagnostics service that provides simple RPC procedures for testing connectivity and monitoring gateway health.

## Available Procedures

### 1. `com.macula.diagnostics.hello`

A simple "hello world" procedure that returns basic gateway information.

**Arguments:** Empty map `{}`

**Returns:**
```json
{
  "message": "Hello from Macula Gateway!",
  "gateway": "macula@127.0.0.1",
  "realm": "be.cortexiq.energy",
  "uptime_seconds": 42,
  "timestamp": 1699612800,
  "version": "0.1.0",
  "protocol": "HTTP/3 (QUIC)"
}
```

**Use Case:** Quick connectivity test, verify gateway is reachable

---

### 2. `com.macula.diagnostics.echo`

Echoes back any arguments sent by the client.

**Arguments:** Any map

**Returns:**
```json
{
  "echo": { /* your arguments */ },
  "timestamp": 1699612800
}
```

**Example:**
```elixir
# Send
%{"message" => "test", "count" => 123}

# Receive
%{
  "echo" => %{"message" => "test", "count" => 123},
  "timestamp" => 1699612800
}
```

**Use Case:** Test serialization/deserialization, verify arguments are transmitted correctly

---

### 3. `com.macula.diagnostics.info`

Returns detailed gateway and system information.

**Arguments:** Empty map `{}`

**Returns:**
```json
{
  "gateway": {
    "node": "macula@127.0.0.1",
    "realm": "be.cortexiq.energy",
    "uptime_seconds": 42,
    "version": "0.1.0"
  },
  "system": {
    "otp_version": "27",
    "erts_version": "15.2.7.2",
    "process_count": 123,
    "memory_bytes": 12345678,
    "process_memory_bytes": 1234567
  },
  "timestamp": 1699612800
}
```

**Use Case:** Monitoring, debugging, capacity planning

---

## Usage Examples

### Elixir (with macula_sdk)

```elixir
# Connect to gateway
{:ok, client} = MaculaSdk.Client.start_link(
  url: "https://localhost:9443",
  realm: "be.cortexiq.energy",
  node_id: "test-client"
)

# Call hello procedure
{:ok, result} = MaculaSdk.Client.call(
  client,
  "com.macula.diagnostics.hello",
  %{}
)

IO.inspect(result)
# => %{
#   "message" => "Hello from Macula Gateway!",
#   "gateway" => "macula@127.0.0.1",
#   ...
# }

# Call echo procedure
{:ok, result} = MaculaSdk.Client.call(
  client,
  "com.macula.diagnostics.echo",
  %{"test" => "data", "number" => 42}
)

IO.inspect(result["echo"])
# => %{"test" => "data", "number" => 42}

# Call info procedure
{:ok, info} = MaculaSdk.Client.call(
  client,
  "com.macula.diagnostics.info",
  %{}
)

IO.puts("Gateway uptime: #{info["gateway"]["uptime_seconds"]} seconds")
IO.puts("Process count: #{info["system"]["process_count"]}")
```

### Erlang (with macula_sdk)

```erlang
%% Connect to gateway
{ok, Client} = macula_sdk:connect([
    {url, <<"https://localhost:9443">>},
    {realm, <<"be.cortexiq.energy">>},
    {node_id, <<"test-client">>}
]).

%% Call hello procedure
{ok, Result} = macula_sdk:call(
    Client,
    <<"com.macula.diagnostics.hello">>,
    #{}
).

io:format("Message: ~s~n", [maps:get(<<"message">>, Result)]).

%% Call info procedure
{ok, Info} = macula_sdk:call(
    Client,
    <<"com.macula.diagnostics.info">>,
    #{}
).

Gateway = maps:get(<<"gateway">>, Info),
Uptime = maps:get(<<"uptime_seconds">>, Gateway),
io:format("Gateway uptime: ~p seconds~n", [Uptime]).
```

## Testing

### Quick Test with Docker

```bash
# Start the gateway
docker run -p 9443:9443 -p 8080:8080 macula/macula-gateway:latest

# In another terminal, connect a client and test
# (See SDK documentation for client setup)
```

### Health Check Integration

The diagnostics service complements the HTTP health endpoints:

- **HTTP Health Endpoints** (`/health`, `/ready`, `/live`) - For Kubernetes/infrastructure monitoring
- **RPC Diagnostics Procedures** - For application-level testing and client verification

Both provide different perspectives on gateway health:
- HTTP endpoints: "Can the gateway process accept connections?"
- RPC diagnostics: "Can the gateway route messages and execute procedures?"

## Automatic Registration

The diagnostics service automatically registers its procedures when the gateway starts. No configuration is required.

You should see these log messages on startup:
```
Starting diagnostics service for realm: be.cortexiq.energy
Registered 3 diagnostic procedures
Diagnostic procedure registered: com.macula.diagnostics.hello
Diagnostic procedure registered: com.macula.diagnostics.echo
Diagnostic procedure registered: com.macula.diagnostics.info
```

## Implementation Details

- **Module:** `macula_gateway_diagnostics`
- **Behavior:** `gen_server`
- **Supervised:** Yes (started by `macula_sup`)
- **RPC Handler:** Implements gateway's `{invoke, ...}` message protocol
- **Dependencies:** None (pure Erlang/OTP)

## See Also

- [Macula SDK Documentation](../../macula_sdk/README.md)
- [Gateway Architecture](../../ARCHITECTURE.md)
- [Health Endpoints](HEALTH.md)
