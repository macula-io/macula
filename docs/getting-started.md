# Getting Started with Macula

**Connect to the relay mesh in 5 minutes**

---

## 1. Installation

### Rebar3 (Erlang)

Add to your `rebar.config`:

```erlang
{deps, [
    {macula, "0.42.7"}
]}.
```

Then fetch and compile:

```bash
rebar3 get-deps
rebar3 compile
```

### Mix (Elixir)

Add to your `mix.exs`:

```elixir
defp deps do
  [
    {:macula, "~> 0.42.7"}
  ]
end
```

Then fetch and compile:

```bash
mix deps.get
mix compile
```

### Prerequisites

- **Erlang/OTP 26+** (QUIC NIF requires OTP 26 or later)
- **C compiler** for building the quicer NIF (`build-essential cmake` on Debian/Ubuntu, `xcode-select --install` on macOS)
- **OS**: Linux, macOS, or Windows WSL2

---

## 2. Connect to the Mesh

Every Macula node connects to a **relay** via a single outbound QUIC connection.
No open ports, no firewall rules, no VPN required.

### Erlang

```erlang
%% Start the macula application
application:ensure_all_started(macula).

%% Connect to a relay
{ok, Client} = macula_relay_client:start_link(#{
    relays => [<<"quic://relay.example.com:9443">>],
    realm  => <<"io.example.myapp">>,
    identity => <<"node-001">>
}).
```

### Elixir

```elixir
# Start the macula application
Application.ensure_all_started(:macula)

# Connect to a relay
{:ok, client} = :macula_relay_client.start_link(%{
  relays: ["quic://relay.example.com:9443"],
  realm: "io.example.myapp",
  identity: "node-001"
})
```

The client maintains a persistent connection to the relay and automatically
fails over to the next relay in the list if the current one goes down.
All subscriptions and procedure registrations are replayed on reconnect.

---

## 3. Pub/Sub Messaging

### Subscribe to a topic

```erlang
Callback = fun(Msg) ->
    io:format("Received: ~p~n", [Msg])
end,
{ok, SubRef} = macula_relay_client:subscribe(Client, <<"sensors.temperature">>, Callback).
```

```elixir
callback = fn msg ->
  IO.inspect(msg, label: "Received")
end

{:ok, sub_ref} = :macula_relay_client.subscribe(client, "sensors.temperature", callback)
```

### Publish a message

```erlang
ok = macula_relay_client:publish(Client, <<"sensors.temperature">>, #{
    value => 23.5,
    unit => <<"celsius">>,
    timestamp => erlang:system_time(millisecond)
}).
```

```elixir
:macula_relay_client.publish(client, "sensors.temperature", %{
  value: 23.5,
  unit: "celsius",
  timestamp: System.system_time(:millisecond)
})
```

Messages flow through the relay to all subscribers on that topic, regardless
of which relay each subscriber is connected to (relays peer with each other).

### Unsubscribe

```erlang
ok = macula_relay_client:unsubscribe(Client, SubRef).
```

---

## 4. RPC (Advertise + Call)

### Advertise a procedure

Any node can advertise a callable procedure:

```erlang
Handler = fun(#{<<"a">> := A, <<"b">> := B}) ->
    {ok, #{result => A + B}}
end,
{ok, _Ref} = macula_relay_client:advertise(Client, <<"math.add">>, Handler).
```

```elixir
handler = fn %{"a" => a, "b" => b} ->
  {:ok, %{result: a + b}}
end

{:ok, _ref} = :macula_relay_client.advertise(client, "math.add", handler)
```

### Call a procedure

From any other node connected to the mesh:

```erlang
{ok, #{result := 5}} = macula_relay_client:call(Client, <<"math.add">>, #{a => 2, b => 3}, 5000).
```

```elixir
{:ok, %{result: 5}} = :macula_relay_client.call(client, "math.add", %{a: 2, b: 3}, 5000)
```

The relay routes the call to a node that has advertised `math.add` and
returns the result. The last argument is the timeout in milliseconds.

---

## 5. Erlang Distribution Over Relay (The Killer Feature)

Full OTP distribution tunneled through the relay mesh. `net_adm:ping/1`,
`gen_server:call/2`, `pg` groups, `Pid ! Msg` -- everything works across
firewalls and NATs without VPNs or open ports.

### Enable relay distribution

```erlang
%% Set the distribution mode to relay
os:putenv("MACULA_DIST_MODE", "relay"),

%% Register the relay client with the distribution layer
macula_dist_relay:register_mesh_client(Client),

%% Advertise that this node accepts distribution connections
macula_dist_relay:advertise_dist_accept(),
```

### Use standard OTP distribution

Once enabled, every OTP distribution primitive works transparently:

```erlang
%% Ping a remote node across firewalls
net_adm:ping('other@remote-host').
%% => pong

%% Call a gen_server on the remote node
gen_server:call({my_server, 'other@remote-host'}, get_status).

%% Send a message to a remote pid
RemotePid ! {hello, from, node()}.

%% pg groups work across the mesh
pg:join(my_group, self()).
```

Each tunnel is encrypted with AES-256-GCM (key derived from the Erlang
distribution cookie). The relay cannot read the traffic.

| Feature | Detail |
|---------|--------|
| **Encryption** | AES-256-GCM per tunnel, relay-opaque |
| **Supervision** | gen_server per tunnel under simple_one_for_one |
| **Cross-relay** | Nodes on different relays connect via relay peering |
| **Reconnection** | Bridge re-acquires relay client after restart |
| **Metrics** | Per-tunnel byte and message counters |

---

## 6. LAN Clustering (Zero Configuration)

On a local network, nodes can find each other without any relay using
UDP multicast gossip:

```erlang
ok = macula_cluster:start_cluster(#{
    strategy => gossip,
    secret => <<"my_cluster_secret">>   %% optional HMAC auth
}).

%% Nodes auto-discover via multicast 230.1.1.251:45892
```

This complements relay connectivity -- nodes on the same LAN cluster
directly while also maintaining their relay connections for WAN reach.

---

## Next Steps

| Guide | Description |
|-------|-------------|
| [Distribution Over Mesh](guides/DIST_OVER_MESH_GUIDE.md) | Relay-tunneled Erlang distribution in depth |
| [Gossip Clustering](guides/GOSSIP_CLUSTERING_GUIDE.md) | Zero-config LAN discovery |
| [Content Transfer](guides/CONTENT_TRANSFER_GUIDE.md) | Mesh artifact distribution |
| [Cluster API](guides/CLUSTER_API_GUIDE.md) | Clustering and distribution API |
| [NAT Traversal](guides/NAT_TRAVERSAL_DEVELOPER_GUIDE.md) | NAT techniques and relay fallback |
| [DHT Guide](guides/DHT_GUIDE.md) | Kademlia DHT internals |
| [Authorization](guides/AUTHORIZATION_GUIDE.md) | DID/UCAN security model |
| [TLS Configuration](operator/TLS_CONFIGURATION.md) | Production TLS setup |
