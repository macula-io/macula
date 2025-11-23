# Workload→Platform API Architecture

**Document Version**: 1.0
**Date**: 2025-11-23
**Status**: Design Proposal

## Executive Summary

This document defines the clean, single-entry-point API for workload applications (like macula-arcade) to interact with the Macula platform. The goal is architectural clarity: workloads should NEVER call low-level platform modules directly.

**Architectural Principle**: Workload → `macula_client` → Platform Internals

## Current State Analysis

### What's Working Well ✅

**macula-arcade already uses a facade pattern:**

```
Phoenix App
    ↓
NodeManager (Elixir wrapper)
    ↓
:macula_client (Erlang API module)
    ↓
Platform internals (macula_peer, macula_local_client, etc.)
```

The `macula_client` module (src/macula_client.erl) provides a clean API with:
- Connection management (`connect/2`, `connect_local/1`, `disconnect/1`)
- Pub/Sub operations (`publish/3`, `subscribe/3`, `unsubscribe/2`)
- RPC operations (`call/3`, `advertise/3`, `unadvertise/2`)
- DHT queries (`discover_subscribers/2`)
- Node identity (`get_node_id/1`)

### Current Architecture (v0.9.2)

```
┌─────────────────────────────────────────────────┐
│           Workload Layer (Elixir/Phoenix)       │
│  ┌──────────────────────────────────────────┐  │
│  │  NodeManager (app-specific wrapper)     │  │
│  └──────────────┬───────────────────────────┘  │
└─────────────────┼───────────────────────────────┘
                  │
┌─────────────────┼───────────────────────────────┐
│  Platform API   │                                │
│  ┌──────────────▼───────────────────────────┐  │
│  │  macula_client (public SDK)              │  │
│  └──────────────┬───────────────────────────┘  │
└─────────────────┼───────────────────────────────┘
                  │
┌─────────────────┼───────────────────────────────┐
│  Platform Layer │                                │
│  ┌──────────────▼───────────────────────────┐  │
│  │  macula_peer (QUIC client facade)        │  │
│  │  macula_local_client (in-VM gateway)     │  │
│  └──────────────┬───────────────────────────┘  │
│                 │                                │
│  ┌──────────────▼───────────────────────────┐  │
│  │  Infrastructure Layer                    │  │
│  │  - macula_gateway                        │  │
│  │  - macula_connection (QUIC transport)    │  │
│  │  - macula_pubsub_handler                 │  │
│  │  - macula_rpc_handler                    │  │
│  │  - macula_gateway_dht                    │  │
│  │  - macula_leader_election                │  │
│  └──────────────────────────────────────────┘  │
└─────────────────────────────────────────────────┘
```

### Problems Identified ❌

1. **NodeManager is redundant** - It's just a GenServer wrapper around `:macula_client` calls
2. **No type safety** - Elixir apps lose the benefit of Erlang typespecs
3. **No Platform Layer API exposure** - v0.9.x added Platform Layer (leader election, CRDTs) but no workload API
4. **Documentation scattered** - API docs in macula_client.erl, usage examples in NodeManager

## Proposed Clean Architecture

### Design Principles

1. **Single Entry Point**: `macula_client` is THE ONLY module workloads call
2. **No Low-Level Access**: Workloads NEVER import `macula_peer`, `macula_gateway`, etc.
3. **Stable API**: Version guarantees - v0.9.x compatible within minor versions
4. **Language Bridges**: Elixir apps call `:macula_client`, future languages use their own thin wrappers
5. **Feature Flags**: New Platform Layer features exposed through `macula_client` API

### Simplified Architecture

```
┌─────────────────────────────────────────────────┐
│           Workload Layer (Elixir/Phoenix)       │
│  ┌──────────────────────────────────────────┐  │
│  │  MaculaArcade.Coordinator                │  │
│  │  - Calls :macula_client directly         │  │
│  │  - No NodeManager wrapper needed         │  │
│  └──────────────┬───────────────────────────┘  │
└─────────────────┼───────────────────────────────┘
                  │
┌─────────────────┼───────────────────────────────┐
│  Platform API   │ (PUBLIC - STABLE)              │
│  ┌──────────────▼───────────────────────────┐  │
│  │  macula_client                           │  │
│  │  - connect_local/1                       │  │
│  │  - publish/subscribe/call/advertise      │  │
│  │  - discover_subscribers/2                │  │
│  │  - get_node_id/1                         │  │
│  │  + register_workload/2       [NEW v0.10] │  │
│  │  + get_leader/1              [NEW v0.10] │  │
│  │  + propose_crdt_update/3     [NEW v0.10] │  │
│  └──────────────────────────────────────────┘  │
└─────────────────────────────────────────────────┘
                  │
┌─────────────────┼───────────────────────────────┐
│  Platform Layer │ (PRIVATE - INTERNAL ONLY)      │
│  - macula_peer, macula_local_client            │
│  - macula_gateway + subsystems                 │
│  - macula_leader_election, macula_crdt         │
│  - macula_connection (QUIC transport)          │
└─────────────────────────────────────────────────┘
```

## Proposed API Extensions for v0.10.0

### 1. Workload Registration

**Problem**: Platform Layer (leader election, CRDTs) exists but no workload API to use it.

**Proposal**: Add workload registration to `macula_client`:

```erlang
%% @doc Register this workload with the platform layer.
%%
%% Returns information about the platform cluster state, including
%% the current leader node for coordination tasks.
%%
%% Options:
%% - workload_name: Binary name for this workload type (e.g., <<"macula_arcade">>)
%% - realm: Binary realm identifier (inherited from connect_local if not specified)
%% - capabilities: List of workload capabilities (e.g., [coordinator, game_server])
%%
%% Returns: {ok, #{leader_node => NodeId, cluster_size => N}}
-spec register_workload(Client :: client(), Opts :: options()) ->
    {ok, map()} | {error, Reason :: term()}.
register_workload(Client, Opts) when is_pid(Client), is_map(Opts) ->
    macula_local_client:register_workload(Client, Opts).
```

**Use Case** (Elixir):
```elixir
{:ok, client} = :macula_client.connect_local(%{realm: "macula.arcade.dev"})
{:ok, %{leader_node: leader, cluster_size: size}} =
    :macula_client.register_workload(client, %{
        workload_name: "macula_arcade",
        capabilities: [:coordinator, :game_server]
    })
```

### 2. Leader Election Access

**Problem**: Coordinator needs to know if it's the leader for centralized matchmaking decisions.

**Proposal**:

```erlang
%% @doc Get the current Platform Layer leader node.
%%
%% Returns the node ID of the current leader, or {error, no_leader}
%% if leader election is in progress.
-spec get_leader(Client :: client()) ->
    {ok, binary()} | {error, Reason :: term()}.
get_leader(Client) when is_pid(Client) ->
    macula_local_client:get_leader(Client).

%% @doc Subscribe to leader change notifications.
%%
%% Callback receives: #{old_leader => NodeId | undefined, new_leader => NodeId}
-spec subscribe_leader_changes(Client :: client(), Callback :: fun()) ->
    {ok, subscription_ref()} | {error, Reason :: term()}.
subscribe_leader_changes(Client, Callback) when is_pid(Client), is_function(Callback, 1) ->
    macula_local_client:subscribe_leader_changes(Client, Callback).
```

**Use Case** (Elixir):
```elixir
# Check if we're the leader before making coordinator decisions
case :macula_client.get_leader(client) do
  {:ok, ^our_node_id} ->
    # We're leader - coordinate matchmaking
    Logger.info("We are coordinator leader")
    coordinate_global_matchmaking()

  {:ok, _other_node} ->
    # Another node is leader - defer to them
    Logger.info("Another node is leader, deferring coordination")
    :ok
end

# Subscribe to leader changes
:macula_client.subscribe_leader_changes(client, fn change ->
  Logger.info("Leader changed: #{inspect(change)}")
  handle_leadership_transition(change)
end)
```

### 3. CRDT Operations

**Problem**: CRDTs exist (`macula_crdt`) but no workload API to use them.

**Proposal**:

```erlang
%% @doc Propose a CRDT update to platform-managed shared state.
%%
%% Updates are replicated across the platform cluster using CRDTs
%% for conflict-free convergence.
%%
%% CRDT Types:
%% - lww_register: Last-Write-Wins Register (value with timestamp)
%% - g_counter: Grow-only Counter
%% - pn_counter: Positive-Negative Counter
%% - g_set: Grow-only Set
%% - or_set: Observed-Remove Set
%%
%% Options:
%% - crdt_type: Atom specifying CRDT type (default: lww_register)
%% - key: Binary key for the shared state entry
%% - value: Term to store (serialized via term_to_binary)
%%
%% Returns: {ok, updated_value} after convergence
-spec propose_crdt_update(Client :: client(), Key :: binary(), Value :: term()) ->
    {ok, term()} | {error, Reason :: term()}.
propose_crdt_update(Client, Key, Value) when is_pid(Client), is_binary(Key) ->
    propose_crdt_update(Client, Key, Value, #{crdt_type => lww_register}).

-spec propose_crdt_update(Client :: client(), Key :: binary(), Value :: term(), Opts :: options()) ->
    {ok, term()} | {error, Reason :: term()}.
propose_crdt_update(Client, Key, Value, Opts) when is_pid(Client), is_binary(Key), is_map(Opts) ->
    macula_local_client:propose_crdt_update(Client, Key, Value, Opts).

%% @doc Read the current value of a CRDT-managed shared state entry.
-spec read_crdt(Client :: client(), Key :: binary()) ->
    {ok, term()} | {error, not_found} | {error, Reason :: term()}.
read_crdt(Client, Key) when is_pid(Client), is_binary(Key) ->
    macula_local_client:read_crdt(Client, Key).
```

**Use Case** (Elixir):
```elixir
# Store global matchmaking queue size
:macula_client.propose_crdt_update(
  client,
  "arcade.snake.queue_size",
  42,
  %{crdt_type: :pn_counter}
)

# Read current queue size from any node
{:ok, queue_size} = :macula_client.read_crdt(client, "arcade.snake.queue_size")
```

## Implementation Phases for v0.10.0

### Phase 1: Extend macula_client API (Week 1-2)
- Add `register_workload/2`
- Add `get_leader/1` and `subscribe_leader_changes/2`
- Add `propose_crdt_update/3,4` and `read_crdt/2`
- Update `macula_local_client` to delegate to platform modules

### Phase 2: Update macula-arcade (Week 3)
- Remove `NodeManager` wrapper
- Call `:macula_client` directly from `Coordinator`
- Use leader election for coordinator selection
- Use CRDTs for global queue state

### Phase 3: Documentation & Examples (Week 4)
- Update Hex docs with Platform Layer APIs
- Add usage examples to macula_client.erl
- Update macula-arcade README with new patterns
- Create tutorial: "Building Distributed Apps with Macula"

## API Contract (Public Stability Guarantee)

### Stable APIs (v0.9.2 - v1.0.0)

**Module**: `macula_client`

**Functions** (backward compatible):
- `connect/2` - Connect to remote mesh
- `connect_local/1` - Connect to local gateway (in-VM)
- `disconnect/1` - Close connection
- `publish/3,4` - Pub/Sub publish
- `subscribe/3` - Pub/Sub subscribe
- `unsubscribe/2` - Unsubscribe from topic
- `call/3,4` - RPC call
- `advertise/3,4` - Advertise RPC service
- `unadvertise/2` - Stop advertising service
- `discover_subscribers/2` - DHT query for subscribers
- `get_node_id/1` - Get client's node ID

**New in v0.10.0** (additive only):
- `register_workload/2` - Platform Layer workload registration
- `get_leader/1` - Platform Layer leader query
- `subscribe_leader_changes/2` - Platform Layer leader notifications
- `propose_crdt_update/3,4` - Platform Layer CRDT writes
- `read_crdt/2` - Platform Layer CRDT reads

### Internal APIs (No Stability Guarantee)

**Modules** (workloads MUST NOT import these):
- `macula_peer` - QUIC client facade (internal)
- `macula_local_client` - In-VM gateway client (internal)
- `macula_gateway*` - Gateway subsystems (internal)
- `macula_connection` - QUIC transport layer (internal)
- `macula_leader_election` - Platform coordination (internal)
- `macula_crdt` - Platform shared state (internal)

## Benefits of Clean API Architecture

### For Workload Developers ✅

1. **Simple mental model** - Only learn `macula_client` API
2. **Stability** - Platform internals can change without breaking workloads
3. **Discoverability** - All features in one module with comprehensive docs
4. **Type safety** - Erlang typespecs provide compile-time checks
5. **Future-proof** - New platform features added to `macula_client` without breaking changes

### For Platform Developers ✅

1. **Refactoring freedom** - Internal modules can be refactored without affecting workloads
2. **Clear boundaries** - Public API surface is small and well-defined
3. **Version management** - Breaking changes only occur in major versions
4. **Testing** - Mock `macula_client` for workload unit tests
5. **Observability** - Single entry point makes instrumentation easier

## Anti-Patterns to Avoid ❌

### 1. Direct Platform Module Calls

```elixir
# ❌ BAD - Calling internal platform modules directly
:macula_peer.start_link(url, opts)
:macula_gateway_dht.query(key)
:macula_leader_election.get_leader()
```

```elixir
# ✅ GOOD - Using macula_client API
:macula_client.connect_local(opts)
:macula_client.discover_subscribers(client, topic)
:macula_client.get_leader(client)
```

### 2. Wrapping macula_client in GenServer

```elixir
# ❌ BAD - Unnecessary wrapper (current NodeManager pattern)
defmodule NodeManager do
  def publish(topic, data) do
    GenServer.call(__MODULE__, {:publish, topic, data})
  end

  def handle_call({:publish, topic, data}, _from, %{client: client} = state) do
    result = :macula_client.publish(client, topic, data)
    {:reply, result, state}
  end
end
```

```elixir
# ✅ GOOD - Call macula_client directly
defmodule Coordinator do
  def init(opts) do
    {:ok, client} = :macula_client.connect_local(%{realm: "my.app"})
    {:ok, %{client: client}}
  end

  def handle_call({:publish, topic, data}, _from, %{client: client} = state) do
    result = :macula_client.publish(client, topic, data)
    {:reply, result, state}
  end
end
```

### 3. Importing Platform Types

```elixir
# ❌ BAD - Depending on internal platform types
defmodule MyApp do
  @type connection :: :macula_peer.peer()  # Internal type
end
```

```elixir
# ✅ GOOD - Using public API types
defmodule MyApp do
  @type connection :: :macula_client.client()  # Public type
end
```

## Migration Guide: v0.8.x → v0.10.0

### Step 1: Update Dependency

```elixir
# mix.exs
def deps do
  [
    {:macula, "~> 0.10.0"}  # Was: "~> 0.8.25"
  ]
end
```

### Step 2: Remove NodeManager Wrapper

**Before (v0.8.x)**:
```elixir
# lib/my_app/mesh/node_manager.ex
defmodule MyApp.Mesh.NodeManager do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def publish(topic, data) do
    GenServer.call(__MODULE__, {:publish, topic, data})
  end

  def init(_opts) do
    {:ok, client} = :macula_client.connect_local(%{realm: "my.app"})
    {:ok, %{client: client}}
  end

  def handle_call({:publish, topic, data}, _from, %{client: client} = state) do
    result = :macula_client.publish(client, topic, data)
    {:reply, result, state}
  end
end
```

**After (v0.10.0)**:
```elixir
# lib/my_app/application.ex
defmodule MyApp.Application do
  def start(_type, _args) do
    children = [
      # Connect once at startup, store client in Registry or persistent_term
      {Task, fn -> connect_to_platform() end},
      MyApp.Coordinator
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end

  defp connect_to_platform do
    {:ok, client} = :macula_client.connect_local(%{realm: "my.app"})
    :persistent_term.put({MyApp, :macula_client}, client)
    :ok
  end
end

# lib/my_app/coordinator.ex
defmodule MyApp.Coordinator do
  def publish(topic, data) do
    client = :persistent_term.get({MyApp, :macula_client})
    :macula_client.publish(client, topic, data)
  end
end
```

### Step 3: Use Platform Layer APIs

**Before (v0.8.x)** - Manual leader election:
```elixir
defmodule MyApp.Coordinator do
  def init(_opts) do
    # Manual leader election via RPC
    {:ok, client} = :macula_client.connect_local(%{realm: "my.app"})
    leader = elect_leader_via_rpc()
    {:ok, %{client: client, leader: leader}}
  end

  defp elect_leader_via_rpc do
    # Complex RPC-based leader election logic
    # Prone to split-brain scenarios
    ...
  end
end
```

**After (v0.10.0)** - Platform Layer leader election:
```elixir
defmodule MyApp.Coordinator do
  def init(_opts) do
    {:ok, client} = :macula_client.connect_local(%{realm: "my.app"})
    {:ok, %{leader_node: leader}} = :macula_client.register_workload(client, %{
      workload_name: "my_app_coordinator"
    })

    # Subscribe to leader changes
    :macula_client.subscribe_leader_changes(client, fn change ->
      send(self(), {:leader_changed, change})
    end)

    {:ok, %{client: client, leader: leader}}
  end

  def handle_info({:leader_changed, %{new_leader: new_leader}}, state) do
    {:noreply, %{state | leader: new_leader}}
  end
end
```

## Testing with Clean API

### Unit Testing Workloads

Mock `macula_client` for isolated testing:

```elixir
# test/support/macula_client_mock.ex
defmodule MaculaClientMock do
  def connect_local(_opts), do: {:ok, self()}
  def publish(_client, _topic, _data), do: :ok
  def subscribe(_client, _topic, _callback), do: {:ok, make_ref()}
  def get_leader(_client), do: {:ok, "test_leader_node"}
end

# test/my_app/coordinator_test.exs
defmodule MyApp.CoordinatorTest do
  use ExUnit.Case

  setup do
    # Replace :macula_client with mock
    Application.put_env(:my_app, :macula_client_module, MaculaClientMock)
    :ok
  end

  test "coordinator publishes events" do
    assert :ok = MyApp.Coordinator.publish("test.topic", %{data: "test"})
  end
end
```

### Integration Testing with Real Platform

```elixir
# test/integration/mesh_test.exs
defmodule MyApp.MeshIntegrationTest do
  use ExUnit.Case

  @tag :integration
  test "full mesh communication flow" do
    # Start real Macula platform
    {:ok, _gateway} = :macula_gateway.start_link(%{realm: "test.realm"})

    # Connect workload
    {:ok, client} = :macula_client.connect_local(%{realm: "test.realm"})

    # Test pub/sub
    assert :ok = :macula_client.publish(client, "test.topic", %{msg: "hello"})

    # Cleanup
    :macula_client.disconnect(client)
  end
end
```

## Conclusion

**Key Takeaways**:

1. ✅ `macula_client` is the ONLY public API workloads should use
2. ✅ Platform internals can evolve without breaking workloads
3. ✅ v0.10.0 will expose Platform Layer features (leader election, CRDTs) through `macula_client`
4. ✅ Remove wrapper GenServers like `NodeManager` - call `:macula_client` directly
5. ✅ Test workloads by mocking `macula_client`, not internal modules

**Next Steps**:

1. Implement Platform Layer APIs in `macula_client` (v0.10.0)
2. Refactor macula-arcade to remove NodeManager wrapper
3. Document best practices in Hex docs
4. Create tutorial for building distributed workloads

---

**Questions? Feedback?**

File issues at: https://github.com/macula-io/macula/issues
