# Macula HTTP/3 Mesh - Quick Start Guide

**Get a 3-node mesh running in 15 minutes**

![Macula Overview](assets/macula_overview.svg)

---

## Prerequisites

### Required Software

1. **Erlang/OTP 26.0 or later**
   ```bash
   # Check version
   erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell
   ```

   Install from:
   - Ubuntu/Debian: `sudo apt-get install erlang`
   - macOS: `brew install erlang`
   - From source: https://www.erlang.org/downloads

2. **Elixir 1.15 or later** (optional, for Elixir examples)
   ```bash
   # Check version
   elixir --version
   ```

   Install from:
   - Ubuntu/Debian: `sudo apt-get install elixir`
   - macOS: `brew install elixir`
   - From source: https://elixir-lang.org/install.html

3. **Git**
   ```bash
   git --version
   ```

4. **C Compiler** (for building quicer NIF)
   - Ubuntu/Debian: `sudo apt-get install build-essential cmake`
   - macOS: `xcode-select --install`

### System Requirements

- **OS**: Linux (Ubuntu 20.04+), macOS 11+, or Windows WSL2
- **RAM**: 512 MB minimum per node (2 GB recommended for development)
- **Network**: UDP port access (default: 4433)
- **Disk**: 100 MB for Macula + dependencies

---

## Step 1: Download and Build Macula

### Clone the Repository

```bash
cd ~/projects
git clone https://github.com/macula-io/macula.git
cd macula
```

### Install Dependencies

```bash
# For Erlang (Rebar3)
rebar3 get-deps

# For Elixir (Mix)
mix deps.get
```

### Build quicer (QUIC Library)

The `quicer` library includes native code and may take a few minutes to compile:

```bash
# Rebar3
rebar3 compile

# Mix
mix compile
```

**Expected output**:
```
===> Fetching quicer (from {git,"https://github.com/emqx/quic.git",...})
===> Compiling quicer
     ...
     [100%] Built target msquic
===> Compiled quicer
```

### Verify Installation

```bash
# Erlang
rebar3 shell
> macula:version().
{ok, "0.1.0"}

# Elixir
iex -S mix
iex> Macula.version()
{:ok, "0.1.0"}
```

---

## Step 2: Start Node 1 (Bootstrap Node)

### Create Configuration File

Create `config/node1.config`:

```erlang
%% config/node1.config
[
 {macula, [
   {node_id, <<"node1">>},
   {realm, <<"org.example.mesh">>},
   {listen_port, 4433},
   {listen_address, "0.0.0.0"},

   %% Discovery
   {discovery, [
     {methods, [static]},  % Use static bootstrap for this example
     {static_nodes, []}    % First node has no bootstrap peers
   ]},

   %% Topology
   {topology, [
     {type, k_regular},
     {k, 2}  % Each node connects to 2 peers
   ]},

   %% TLS/Certificates (auto-generate for demo)
   {cert_mode, auto_generate},

   %% Logging
   {log_level, info}
 ]}
].
```

### Start Node 1

```bash
# Erlang
erl -config config/node1 -pa _build/default/lib/*/ebin -eval 'application:ensure_all_started(macula).'

# Elixir
iex --name node1@127.0.0.1 --cookie macula_demo -S mix run -e 'Application.ensure_all_started(:macula)' -- --config config/node1.config
```

**Expected output**:
```
[info] Macula node started: node1
[info] Listening on 0.0.0.0:4433 (UDP)
[info] Node ID: a3f5b2e1c4d8a7f9...
[info] Realm: org.example.mesh
[info] Topology: k_regular (k=2)
[info] Discovery: static
[info] Ready to accept connections
```

**Keep this terminal open** - Node 1 is now running.

---

## Step 3: Start Node 2 (Join the Mesh)

### Create Configuration File

Create `config/node2.config`:

```erlang
%% config/node2.config
[
 {macula, [
   {node_id, <<"node2">>},
   {realm, <<"org.example.mesh">>},
   {listen_port, 4434},  % Different port
   {listen_address, "0.0.0.0"},

   %% Discovery - bootstrap from Node 1
   {discovery, [
     {methods, [static]},
     {static_nodes, [
       {"127.0.0.1", 4433}  % Node 1's address
     ]}
   ]},

   %% Topology
   {topology, [
     {type, k_regular},
     {k, 2}
   ]},

   %% TLS/Certificates
   {cert_mode, auto_generate},

   %% Logging
   {log_level, info}
 ]}
].
```

### Start Node 2 (in new terminal)

```bash
# Open new terminal
cd ~/projects/macula

# Erlang
erl -config config/node2 -pa _build/default/lib/*/ebin -eval 'application:ensure_all_started(macula).'

# Elixir
iex --name node2@127.0.0.1 --cookie macula_demo -S mix run -e 'Application.ensure_all_started(:macula)' -- --config config/node2.config
```

**Expected output**:
```
[info] Macula node started: node2
[info] Listening on 0.0.0.0:4434 (UDP)
[info] Node ID: b7c3d8e2f5a9b4c1...
[info] Realm: org.example.mesh
[info] Connecting to bootstrap node 127.0.0.1:4433...
[info] Connected to node1 (a3f5b2e1c4d8a7f9...)
[info] SWIM membership: 2 nodes alive
[info] Mesh topology established
```

**In Node 1's terminal**, you should see:
```
[info] New connection from 127.0.0.1:xxxxx
[info] Handshake complete: node2 (b7c3d8e2f5a9b4c1...)
[info] SWIM membership: 2 nodes alive
```

---

## Step 4: Start Node 3 (Expand the Mesh)

### Create Configuration File

Create `config/node3.config`:

```erlang
%% config/node3.config
[
 {macula, [
   {node_id, <<"node3">>},
   {realm, <<"org.example.mesh">>},
   {listen_port, 4435},
   {listen_address, "0.0.0.0"},

   %% Discovery - can bootstrap from either node
   {discovery, [
     {methods, [static]},
     {static_nodes, [
       {"127.0.0.1", 4433},  % Node 1
       {"127.0.0.1", 4434}   % Node 2
     ]}
   ]},

   %% Topology
   {topology, [
     {type, k_regular},
     {k, 2}
   ]},

   %% TLS/Certificates
   {cert_mode, auto_generate},

   %% Logging
   {log_level, info}
 ]}
].
```

### Start Node 3 (in new terminal)

```bash
# Open new terminal
cd ~/projects/macula

# Erlang
erl -config config/node3 -pa _build/default/lib/*/ebin -eval 'application:ensure_all_started(macula).'

# Elixir
iex --name node3@127.0.0.1 --cookie macula_demo -S mix run -e 'Application.ensure_all_started(:macula)' -- --config config/node3.config
```

**Expected output**:
```
[info] Macula node started: node3
[info] Listening on 0.0.0.0:4435 (UDP)
[info] Node ID: c8d4e9f3a6b2c7d1...
[info] Realm: org.example.mesh
[info] Connecting to bootstrap nodes...
[info] Connected to node1 (a3f5b2e1c4d8a7f9...)
[info] Connected to node2 (b7c3d8e2f5a9b4c1...)
[info] SWIM membership: 3 nodes alive
[info] Mesh topology: k_regular (k=2)
[info] Routing table: 3 nodes
```

**Congratulations!** You now have a 3-node mesh network running.

---

## Step 5: Verify Mesh Topology

### Check Membership (on any node)

In any node's console:

```erlang
% Erlang
macula_membership:get_members().

% Expected output:
[
  #{node_id => <<"a3f5b2e1...">>, state => alive, ...},
  #{node_id => <<"b7c3d8e2...">>, state => alive, ...},
  #{node_id => <<"c8d4e9f3...">>, state => alive, ...}
]
```

```elixir
# Elixir
Macula.Membership.get_members()

# Expected output:
[
  %{node_id: "a3f5b2e1...", state: :alive, ...},
  %{node_id: "b7c3d8e2...", state: :alive, ...},
  %{node_id: "c8d4e9f3...", state: :alive, ...}
]
```

### Check Connections

```erlang
% Erlang
macula_topology:get_connections().

% Expected output:
[
  #{peer_id => <<"b7c3d8e2...">>, state => active, rtt_ms => 1.2},
  #{peer_id => <<"c8d4e9f3...">>, state => active, rtt_ms => 1.5}
]
```

### Visualize Topology (ASCII Art)

```erlang
% Erlang
macula_topology:print_topology().
```

**Expected output**:
```
Mesh Topology (k-regular, k=2)
==============================

node1 (a3f5...) ←─→ node2 (b7c3...)
  ↑                     ↑
  └────────────→ node3 (c8d4...)
                       ↑
                       └────────→ node1

3 nodes, 3 connections
Average RTT: 1.3ms
```

---

## Step 6: Send Your First Message (Pub/Sub)

### Subscribe to a Topic (on Node 3)

In Node 3's console:

```erlang
% Erlang
Subscriber = spawn(fun() ->
  receive
    {event, Topic, Msg} ->
      io:format("Received on ~s: ~p~n", [Topic, Msg])
  end
end).

macula_pubsub:subscribe(<<"hello.world">>, Subscriber).
```

```elixir
# Elixir
pid = spawn(fn ->
  receive do
    {:event, topic, msg} ->
      IO.puts("Received on #{topic}: #{inspect(msg)}")
  end
end)

Macula.PubSub.subscribe("hello.world", pid)
```

**Expected output**:
```
[info] Subscribed to org.example.mesh.hello.world
ok
```

### Publish a Message (on Node 1)

In Node 1's console:

```erlang
% Erlang
macula_pubsub:publish(<<"hello.world">>, #{
  message => <<"Hello from Node 1!">>,
  timestamp => erlang:system_time(millisecond)
}).
```

```elixir
# Elixir
Macula.PubSub.publish("hello.world", %{
  message: "Hello from Node 1!",
  timestamp: System.system_time(:millisecond)
})
```

**Expected output on Node 1**:
```
[info] Published to org.example.mesh.hello.world
ok
```

**Expected output on Node 3** (subscriber):
```
Received on org.example.mesh.hello.world: #{
  message => <<"Hello from Node 1!">>,
  timestamp => 1704723456789,
  publisher => <<"a3f5b2e1...">>
}
```

**Message flow**: Node 1 → QUIC/HTTP3 → Node 3 (may route via Node 2 depending on topology)

---

## Step 7: Make Your First RPC Call

### Register RPC Endpoint (on Node 2)

In Node 2's console:

```erlang
% Erlang
EchoHandler = fun(Args) ->
  {ok, #{echo => Args, node => node()}}
end.

macula_rpc:register(<<"echo_service">>, EchoHandler).
```

```elixir
# Elixir
echo_handler = fn args ->
  {:ok, %{echo: args, node: Node.self()}}
end

Macula.RPC.register("echo_service", echo_handler)
```

**Expected output**:
```
[info] Registered RPC endpoint: org.example.mesh.echo_service
ok
```

### Call RPC (from Node 1)

In Node 1's console:

```erlang
% Erlang
macula_rpc:call(<<"echo_service">>, #{
  test => <<"Hello RPC!">>,
  value => 42
}, 5000).  % 5 second timeout
```

```elixir
# Elixir
Macula.RPC.call("echo_service", %{
  test: "Hello RPC!",
  value: 42
}, 5000)
```

**Expected output on Node 1**:
```
{ok, #{
  echo => #{test => <<"Hello RPC!">>, value => 42},
  node => 'node2@127.0.0.1'
}}
```

**Expected output on Node 2** (handler):
```
[info] RPC call received: echo_service
[info] Args: #{test => <<"Hello RPC!">>, value => 42}
```

**RPC flow**: Node 1 → finds registration via DHT → routes to Node 2 → executes handler → returns result

---

## Step 8: Test Fault Tolerance

### Stop Node 2

In Node 2's terminal, press `Ctrl+C` twice to stop the node.

**Expected output on Node 1 and Node 3**:
```
[warning] Connection lost to node2 (b7c3d8e2...)
[info] SWIM detected failure: node2
[info] SWIM membership: 2 nodes alive, 1 suspect
[info] Topology reconfiguring...
[info] New connection established: node1 ←→ node3
[info] SWIM membership: 2 nodes alive
```

### Verify Mesh Adapted

On Node 1 or Node 3:

```erlang
% Erlang
macula_topology:get_connections().

% Expected output (now only 1 connection):
[
  #{peer_id => <<"c8d4e9f3...">>, state => active, rtt_ms => 1.1}
]
```

The mesh **automatically adapts** - Node 1 and Node 3 now connect directly.

### Restart Node 2

Restart Node 2 (using the same command from Step 3).

**Expected output**:
```
[info] Macula node started: node2
[info] Reconnecting to mesh...
[info] SWIM membership: 3 nodes alive
[info] Topology restored
```

The mesh **self-heals** automatically.

---

## Common Operations

### List All Nodes in Mesh

```erlang
% Erlang
macula_membership:list_nodes().
```

```elixir
# Elixir
Macula.Membership.list_nodes()
```

### Get Node Statistics

```erlang
% Erlang
macula:stats().

% Output:
#{
  messages_sent => 1543,
  messages_received => 1687,
  bytes_sent => 245678,
  bytes_received => 267890,
  active_connections => 2,
  routing_table_size => 3,
  uptime_seconds => 3600
}
```

### Subscribe with Pattern Matching

```erlang
% Erlang - Subscribe to all topics starting with "sensor."
macula_pubsub:subscribe(<<"sensor.*">>, Pid, #{match => prefix}).

% Matches: sensor.temperature, sensor.humidity, etc.
```

### Publish with Options

```erlang
% Erlang - Publish with acknowledgment
macula_pubsub:publish(<<"important.event">>, Data, #{
  acknowledge => true,  % Wait for delivery confirmation
  retain => true        % Store for late subscribers
}).
```

---

## Troubleshooting

### Problem: "Port already in use"

**Error**:
```
{error, eaddrinuse}
```

**Solution**: Change the `listen_port` in your config file to an unused port (e.g., 4436, 4437).

---

### Problem: Nodes can't discover each other

**Symptoms**: Node 2 or 3 logs show "Connection timeout" or "No route to bootstrap node"

**Checks**:
1. **Firewall**: Ensure UDP port 4433-4435 are not blocked
   ```bash
   # Ubuntu/Debian
   sudo ufw allow 4433:4435/udp

   # macOS
   # Check System Preferences → Security & Privacy → Firewall
   ```

2. **Correct IP address**: If running on different machines, replace `127.0.0.1` with actual IP
   ```bash
   # Find your IP
   ip addr show  # Linux
   ifconfig      # macOS
   ```

3. **Same realm**: All nodes must have the same `realm` in config

---

### Problem: "Certificate validation failed"

**Error**:
```
{error, {tls_alert, "certificate unknown"}}
```

**Cause**: Certificate mismatch (usually in manual cert mode)

**Solution**: Use `{cert_mode, auto_generate}` for development, or ensure all nodes trust the same CA.

---

### Problem: High latency or packet loss

**Check network conditions**:
```erlang
% Erlang
macula_connection:ping(<<"node2_id">>).

% Output:
{ok, 1.2}  % RTT in milliseconds
```

If RTT > 100ms on localhost, check:
- System load (CPU usage)
- Other applications using network
- Docker/VM networking overhead

---

## Next Steps

Congratulations! You've successfully:
- ✅ Built Macula from source
- ✅ Started a 3-node mesh network
- ✅ Verified mesh topology
- ✅ Sent pub/sub messages across the mesh
- ✅ Made RPC calls between nodes
- ✅ Tested fault tolerance and self-healing

### Learn More

- **[Hello World Tutorial](HELLO_WORLD.md)** - Build a complete application
- **[RPC Guide](../developer/RPC_GUIDE.md)** - Complete RPC documentation
- **[PubSub Guide](../developer/PUBSUB_GUIDE.md)** - Pub/Sub patterns
- **[Development Guide](../developer/DEVELOPMENT.md)** - Contributing to Macula

### Try More Advanced Features

1. **Realm isolation**: Start nodes in different realms and use gateways
2. **NAT traversal**: Run nodes on different networks (home, cloud, mobile)
3. **Large mesh**: Scale to 10+ nodes and observe routing behavior
4. **Persistence**: Add event sourcing with persistent subscriptions
5. **Monitoring**: Set up Prometheus metrics and Grafana dashboards

### Join the Community

- **GitHub**: https://github.com/macula-io/macula
- **Discord**: https://discord.gg/macula
- **Docs**: https://docs.macula.io

---

**Happy meshing!** 🎉
