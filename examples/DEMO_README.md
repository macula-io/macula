# Macula Decentralized Chat Demo

This demo showcases Macula's ability to enable **peer-to-peer communication between applications behind firewalls** without requiring a central server in the data path.

## What Makes This Special?

### Traditional Approach (Central Server):
```
Alice <--HTTP--> Central Server <--HTTP--> Bob
      (blocked)                   (blocked)
```
- ✗ All messages go through central server
- ✗ Central server is single point of failure
- ✗ Central server sees all messages
- ✗ Doesn't work behind corporate firewalls/NAT

### Macula Approach (HTTP/3 Mesh):
```
Alice <--HTTP/3 (QUIC)-->  Gateway  <--HTTP/3--> Bob
      (works!)           (bootstrap  (works!)
                          only)
```
- ✓ Gateway only for bootstrap & service discovery
- ✓ Messages flow via pub/sub (can be decentralized)
- ✓ HTTP/3 (QUIC) penetrates NAT/firewalls
- ✓ DHT-based service discovery
- ✓ Works from anywhere (home, office, mobile)

## Key Technologies

1. **HTTP/3 (QUIC)**: Modern transport protocol that works through NAT/firewalls
2. **DHT (Kademlia)**: Decentralized service discovery
3. **Pub/Sub**: Topic-based messaging
4. **Multi-tenancy**: Realm isolation
5. **Self-signed TLS**: No certificate authorities needed

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                   Macula HTTP/3 Mesh                     │
│                                                          │
│  ┌──────────┐                            ┌──────────┐   │
│  │ Alice    │─────┐                 ┌────│ Bob      │   │
│  │ (Behind  │     │                 │    │ (Behind  │   │
│  │ Firewall)│     ▼                 ▼    │ Firewall)│   │
│  └──────────┘  ┌─────────────────────┐  └──────────┘   │
│                │   Macula Gateway    │                  │
│                │  ┌─────────────┐    │                  │
│                │  │ DHT (Discovery)  │                  │
│                │  │ Pub/Sub Router   │                  │
│                │  │ RPC Registry     │                  │
│                │  └─────────────┘    │                  │
│                └─────────────────────┘                  │
└─────────────────────────────────────────────────────────┘
```

## How It Works

### Phase 1: Bootstrap
1. Alice connects to gateway via HTTP/3 (QUIC)
2. Bob connects to gateway via HTTP/3 (QUIC)
3. Both authenticate to realm `com.example.chat`

### Phase 2: Discovery
1. Alice subscribes to topic `chat.messages`
2. Gateway registers subscription in DHT
3. Bob subscribes to same topic
4. Both clients can now discover each other via DHT

### Phase 3: Communication
1. Alice publishes message to `chat.messages`
2. Gateway routes message to all subscribers (Bob)
3. Bob receives message instantly
4. Bob can reply the same way

## What This Demonstrates

✓ **Firewall-Friendly**: HTTP/3 (QUIC) works through NAT
✓ **Decentralized Discovery**: DHT instead of central registry
✓ **Topic-Based Routing**: Pub/Sub pattern
✓ **Multi-Tenancy**: Realm isolation
✓ **Real-Time**: Low-latency messaging
✓ **Simple API**: Clean Erlang/Elixir SDK

## Running the Demo

### Quick Start (Automated Chat)

The easiest way to run the demo is using the automated chat script:

```bash
# 1. Setup (one time)
cd /home/rl/work/github.com/macula-io/macula/examples
./demo-setup.sh

# 2. Terminal 1 - Start Alice's chat
./chat_demo.erl alice

# 3. Terminal 2 - Start Bob's chat
./chat_demo.erl bob

# 4. Type messages in either terminal!
alice> Hello Bob!
bob> Hi Alice, how are you?
alice> Great! This is peer-to-peer through firewalls!
```

The chat demo automatically:
- Connects to the gateway at https://localhost:9443
- Joins the `com.example.chat` realm
- Subscribes to the `chat.messages` topic
- Displays incoming messages from the other person
- Type `/quit` to exit

### Alternative: Interactive Erlang Shell

For more control and experimentation, use the interactive shell:

```bash
# 1. Setup (one time)
cd /home/rl/work/github.com/macula-io/macula/examples
./demo-setup.sh

# 2. Terminal 1 - Start Alice
./run-alice.sh

# 3. Terminal 2 - Start Bob
./run-bob.sh

# 4. In each terminal, connect and subscribe
1> {ok, C} = macula_connection:start_link(
      <<"https://localhost:9443">>,
      #{realm => <<"com.example.chat">>}
   ).

2> macula_connection:subscribe(
      C,
      <<"chat.messages">>,
      fun(Msg) ->
          io:format("~nReceived: ~p~n> ", [Msg])
      end
   ).

# 5. Send messages!
3> macula_connection:publish(
      C,
      <<"chat.messages">>,
      #{from => <<"alice">>, msg => <<"Hello Bob!">>}
   ).
```

### Manual Setup

If you want to set things up manually:

```bash
# 1. Ensure gateway is running
kubectl --context kind-macula-hub get pods -n macula-system

# 2. Port forward gateway
kubectl --context kind-macula-hub port-forward \
    -n macula-system svc/macula-gateway 9443:9443 &

# 3. Compile Macula
cd /home/rl/work/github.com/macula-io/macula
rebar3 compile

# 4. Start Erlang shell
erl -pa _build/default/lib/*/ebin
```

## Use Cases

This pattern enables:

- **IoT Devices**: Devices behind home routers communicating
- **Mobile Apps**: Apps communicating without central server
- **Corporate Apps**: Apps across different offices/VPNs
- **Peer-to-Peer**: Direct device-to-device communication
- **Edge Computing**: Edge nodes coordinating
- **Multi-Cloud**: Services across cloud providers

## Comparison to Other Solutions

### vs WebSockets
- ✓ HTTP/3 works better through NAT/firewalls
- ✓ Built-in multiplexing (no head-of-line blocking)
- ✓ Connection migration (survives IP changes)

### vs WAMP
- ✓ No WebSocket requirement
- ✓ Works on UDP (QUIC)
- ✓ Better NAT traversal

### vs libp2p
- ✓ Simpler (just HTTP/3)
- ✓ Standard protocol (not custom)
- ✓ Works with existing infrastructure

### vs NATS/Redis
- ✓ Decentralized (no central broker required)
- ✓ Firewall-friendly (HTTP/3)
- ✓ Multi-tenancy built-in

## Next Steps

1. **Try the demo**: Follow the instructions above
2. **Explore the code**: Look at `src/macula_gateway.erl`
3. **Add features**: Try RPC calls, multiple topics, etc.
4. **Deploy**: Run on actual separate machines/networks
5. **Scale**: Add more gateways, create a mesh

## Troubleshooting

### Gateway not accessible
```bash
kubectl --context kind-macula-hub logs -n macula-system -l app=macula-gateway
```

### Port forwarding failed
```bash
pkill -f "port-forward.*macula-gateway"
kubectl --context kind-macula-hub port-forward -n macula-system svc/macula-gateway 9443:9443 &
```

### QUIC connection issues
- Ensure port 9443 is not blocked
- Check firewall settings
- Verify gateway is listening on correct port

## Learn More

- [Macula Architecture](../architecture/macula_http3_mesh_root.md)
- [HTTP/3 RFC](https://www.rfc-editor.org/rfc/rfc9114.html)
- [QUIC RFC](https://www.rfc-editor.org/rfc/rfc9000.html)
- [DHT (Kademlia)](https://en.wikipedia.org/wiki/Kademlia)
