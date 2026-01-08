# NAT Traversal Developer Guide

This guide covers the Macula NAT traversal system API, code examples in both Erlang and Elixir, and integration patterns.

![NAT Traversal Flow](artwork/nat_traversal_flow.svg)

---

## Overview

The NAT traversal system consists of these modules:

| Module | Purpose |
|--------|---------|
| `macula_nat_system` | Supervisor for NAT subsystem |
| `macula_nat_detector` | Detects local NAT type |
| `macula_nat_cache` | Caches NAT profiles with TTL |
| `macula_nat_coordinator` | Coordinates hole punching |
| `macula_nat_connector` | Intelligent connection establishment |
| `macula_relay_registry` | Distributed relay node registry |
| `macula_relay_node` | Relay server functionality |

---

## Quick Start

### Detecting Your NAT Type

**Erlang:**
```erlang
%% Detect NAT type (async, returns cached if available)
{ok, Profile} = macula_nat_detector:detect().

%% Force fresh detection (bypasses cache)
{ok, Profile} = macula_nat_detector:detect(#{force => true}).

%% Get cached local profile (fast, no network)
{ok, Profile} = macula_nat_detector:get_local_profile().
```

**Elixir:**
```elixir
# Detect NAT type (async, returns cached if available)
{:ok, profile} = :macula_nat_detector.detect()

# Force fresh detection (bypasses cache)
{:ok, profile} = :macula_nat_detector.detect(%{force: true})

# Get cached local profile (fast, no network)
{:ok, profile} = :macula_nat_detector.get_local_profile()
```

### Connecting to a Peer

**Erlang:**
```erlang
%% Connect using optimal strategy (automatic)
{ok, Connection} = macula_nat_connector:connect(TargetNodeId, #{
    timeout => 10000,          % 10 second timeout
    prefer_direct => true      % Try direct before relay
}).

%% The connector automatically:
%% 1. Fetches target's NAT profile from DHT
%% 2. Determines best connection strategy
%% 3. Attempts hole punching if feasible
%% 4. Falls back to relay if needed
```

**Elixir:**
```elixir
# Connect using optimal strategy (automatic)
{:ok, connection} = :macula_nat_connector.connect(target_node_id, %{
  timeout: 10_000,           # 10 second timeout
  prefer_direct: true        # Try direct before relay
})

# The connector automatically:
# 1. Fetches target's NAT profile from DHT
# 2. Determines best connection strategy
# 3. Attempts hole punching if feasible
# 4. Falls back to relay if needed
```

---

## NAT Detection API

### macula_nat_detector

#### detect/0, detect/1

Detect local NAT characteristics.

**Erlang:**
```erlang
-spec detect() -> {ok, nat_profile()} | {error, term()}.
-spec detect(map()) -> {ok, nat_profile()} | {error, term()}.

%% Options:
%%   force => boolean()     - Bypass cache, force fresh detection
%%   timeout => integer()   - Detection timeout in ms (default 5000)
%%   observers => [endpoint()] - Custom observer endpoints

%% Example
{ok, #{
    mapping => ei,           % ei | hd | pd
    filtering => pd,         % ei | hd | pd
    allocation => pp,        % pp | pc | rd
    public_ip => {203,0,113,5},
    public_port => 4433,
    detected_at => 1700000000,
    confidence => high       % high | medium | low
}} = macula_nat_detector:detect().
```

**Elixir:**
```elixir
# Options:
#   force: boolean()     - Bypass cache, force fresh detection
#   timeout: integer()   - Detection timeout in ms (default 5000)
#   observers: [endpoint()] - Custom observer endpoints

# Example
{:ok, %{
  mapping: :ei,           # :ei | :hd | :pd
  filtering: :pd,         # :ei | :hd | :pd
  allocation: :pp,        # :pp | :pc | :rd
  public_ip: {203, 0, 113, 5},
  public_port: 4433,
  detected_at: 1_700_000_000,
  confidence: :high       # :high | :medium | :low
}} = :macula_nat_detector.detect()
```

#### get_local_profile/0

Get cached local NAT profile (no network call).

**Erlang:**
```erlang
-spec get_local_profile() -> {ok, nat_profile()} | {error, not_detected}.

%% Returns immediately with cached profile
{ok, Profile} = macula_nat_detector:get_local_profile().
```

**Elixir:**
```elixir
# Returns immediately with cached profile
{:ok, profile} = :macula_nat_detector.get_local_profile()
```

#### add_observation/2

Add external observation for detection refinement.

**Erlang:**
```erlang
-spec add_observation(inet:ip_address(), inet:port_number()) -> ok.

%% Called when receiving reflexive address from external peer
macula_nat_detector:add_observation({198,51,100,1}, 5000).
```

**Elixir:**
```elixir
# Called when receiving reflexive address from external peer
:macula_nat_detector.add_observation({198, 51, 100, 1}, 5000)
```

#### refresh/0

Trigger background NAT profile refresh.

**Erlang:**
```erlang
-spec refresh() -> ok.

%% Useful after network change (IP change, reconnect)
macula_nat_detector:refresh().
```

**Elixir:**
```elixir
# Useful after network change (IP change, reconnect)
:macula_nat_detector.refresh()
```

---

## NAT Cache API

### macula_nat_cache

Caches NAT profiles with TTL and stale-while-revalidate semantics.

#### get/1

Get cached NAT profile for a node.

**Erlang:**
```erlang
-spec get(binary()) -> {ok, nat_profile()} |
                       {stale, nat_profile()} |
                       {error, not_found}.

%% Fresh cache hit
{ok, Profile} = macula_nat_cache:get(NodeId).

%% Stale but usable (background refresh triggered)
{stale, Profile} = macula_nat_cache:get(NodeId).

%% Not in cache
{error, not_found} = macula_nat_cache:get(UnknownNodeId).
```

**Elixir:**
```elixir
# Fresh cache hit
{:ok, profile} = :macula_nat_cache.get(node_id)

# Stale but usable (background refresh triggered)
{:stale, profile} = :macula_nat_cache.get(node_id)

# Not in cache
{:error, :not_found} = :macula_nat_cache.get(unknown_node_id)
```

#### get_from_dht/1

Fetch NAT profile from DHT (with caching).

**Erlang:**
```erlang
-spec get_from_dht(binary()) -> {ok, nat_profile()} | {error, term()}.

%% Fetches from DHT if not in local cache
{ok, Profile} = macula_nat_cache:get_from_dht(NodeId).
```

**Elixir:**
```elixir
# Fetches from DHT if not in local cache
{:ok, profile} = :macula_nat_cache.get_from_dht(node_id)
```

#### put/2, put/3

Store NAT profile in cache.

**Erlang:**
```erlang
-spec put(binary(), nat_profile()) -> ok.
-spec put(binary(), nat_profile(), pos_integer()) -> ok.

%% Store with default TTL (300 seconds)
ok = macula_nat_cache:put(NodeId, Profile).

%% Store with custom TTL
ok = macula_nat_cache:put(NodeId, Profile, 600).  % 10 minutes
```

**Elixir:**
```elixir
# Store with default TTL (300 seconds)
:ok = :macula_nat_cache.put(node_id, profile)

# Store with custom TTL
:ok = :macula_nat_cache.put(node_id, profile, 600)  # 10 minutes
```

#### invalidate/1

Remove profile from cache.

**Erlang:**
```erlang
-spec invalidate(binary()) -> ok.

%% Force re-fetch on next access
ok = macula_nat_cache:invalidate(NodeId).
```

**Elixir:**
```elixir
# Force re-fetch on next access
:ok = :macula_nat_cache.invalidate(node_id)
```

#### stats/0

Get cache statistics.

**Erlang:**
```erlang
-spec stats() -> map().

#{
    size => 150,
    hits => 1000,
    misses => 50,
    stale_hits => 25,
    evictions => 10
} = macula_nat_cache:stats().
```

**Elixir:**
```elixir
%{
  size: 150,
  hits: 1000,
  misses: 50,
  stale_hits: 25,
  evictions: 10
} = :macula_nat_cache.stats()
```

---

## Connection Coordination API

### macula_nat_coordinator

Coordinates hole punching between peers.

#### request_connection/2, request_connection/3

Request connection to a target peer.

**Erlang:**
```erlang
-spec request_connection(binary(), map()) ->
    {ok, direct, Connection} |
    {ok, punched, Connection} |
    {ok, relayed, Connection} |
    {error, term()}.

%% Simple request
{ok, Method, Conn} = macula_nat_coordinator:request_connection(
    TargetNodeId,
    #{}
).

%% With options
{ok, Method, Conn} = macula_nat_coordinator:request_connection(
    TargetNodeId,
    #{
        timeout => 15000,
        max_punch_attempts => 3,
        allow_relay => true
    }
).

%% Method indicates how connection was established:
%%   direct  - Direct connection (target has public IP or Full Cone NAT)
%%   punched - Hole punching succeeded
%%   relayed - Using relay node
```

**Elixir:**
```elixir
# Simple request
{:ok, method, conn} = :macula_nat_coordinator.request_connection(
  target_node_id,
  %{}
)

# With options
{:ok, method, conn} = :macula_nat_coordinator.request_connection(
  target_node_id,
  %{
    timeout: 15_000,
    max_punch_attempts: 3,
    allow_relay: true
  }
)

# Method indicates how connection was established:
#   :direct  - Direct connection (target has public IP or Full Cone NAT)
#   :punched - Hole punching succeeded
#   :relayed - Using relay node
```

#### coordinate_punch/3

Low-level hole punch coordination (usually internal).

**Erlang:**
```erlang
-spec coordinate_punch(binary(), binary(), map()) ->
    {ok, reference()} | {error, term()}.

%% Returns reference for tracking punch attempt
{ok, Ref} = macula_nat_coordinator:coordinate_punch(
    PeerA_NodeId,
    PeerB_NodeId,
    #{predicted_ports => {PeerA_Port, PeerB_Port}}
).
```

**Elixir:**
```elixir
# Returns reference for tracking punch attempt
{:ok, ref} = :macula_nat_coordinator.coordinate_punch(
  peer_a_node_id,
  peer_b_node_id,
  %{predicted_ports: {peer_a_port, peer_b_port}}
)
```

### macula_nat_connector

High-level connection establishment with automatic strategy selection.

#### connect/2, connect/3

Establish connection to peer using optimal strategy.

**Erlang:**
```erlang
-spec connect(binary(), map()) -> {ok, connection()} | {error, term()}.

%% Connect with defaults
{ok, Conn} = macula_nat_connector:connect(TargetNodeId, #{}).

%% Connect with options
{ok, Conn} = macula_nat_connector:connect(TargetNodeId, #{
    timeout => 20000,
    prefer_direct => true,
    fallback_relay => true,
    max_attempts => 3
}).
```

**Elixir:**
```elixir
# Connect with defaults
{:ok, conn} = :macula_nat_connector.connect(target_node_id, %{})

# Connect with options
{:ok, conn} = :macula_nat_connector.connect(target_node_id, %{
  timeout: 20_000,
  prefer_direct: true,
  fallback_relay: true,
  max_attempts: 3
})
```

---

## Relay System API

### macula_relay_registry

Distributed registry of relay-capable nodes.

#### register/2

Register as relay-capable node.

**Erlang:**
```erlang
-spec register(binary(), binary()) -> ok | {error, term()}.

%% Register self as relay with endpoint
ok = macula_relay_registry:register(MyNodeId, MyEndpoint).
```

**Elixir:**
```elixir
# Register self as relay with endpoint
:ok = :macula_relay_registry.register(my_node_id, my_endpoint)
```

#### find_relay/1

Find suitable relay for target peer.

**Erlang:**
```erlang
-spec find_relay(binary()) -> {ok, relay_info()} | {error, no_relay}.

%% Find relay to reach target
{ok, #{
    node_id => RelayNodeId,
    endpoint => RelayEndpoint,
    latency_ms => 25,
    load => 0.3
}} = macula_relay_registry:find_relay(TargetNodeId).
```

**Elixir:**
```elixir
# Find relay to reach target
{:ok, %{
  node_id: relay_node_id,
  endpoint: relay_endpoint,
  latency_ms: 25,
  load: 0.3
}} = :macula_relay_registry.find_relay(target_node_id)
```

### macula_relay_node

Relay server functionality.

#### enable/0, enable/1

Enable relay functionality on this node.

**Erlang:**
```erlang
-spec enable() -> ok.
-spec enable(map()) -> ok.

%% Enable with defaults
ok = macula_relay_node:enable().

%% Enable with custom limits
ok = macula_relay_node:enable(#{
    max_sessions => 200,
    bandwidth_limit => 2097152,  % 2 MB/s per session
    session_timeout => 1800000   % 30 minutes
}).
```

**Elixir:**
```elixir
# Enable with defaults
:ok = :macula_relay_node.enable()

# Enable with custom limits
:ok = :macula_relay_node.enable(%{
  max_sessions: 200,
  bandwidth_limit: 2_097_152,  # 2 MB/s per session
  session_timeout: 1_800_000   # 30 minutes
})
```

#### disable/0

Disable relay functionality.

**Erlang:**
```erlang
-spec disable() -> ok.

ok = macula_relay_node:disable().
```

**Elixir:**
```elixir
:ok = :macula_relay_node.disable()
```

#### request_relay/2

Request relay session to target.

**Erlang:**
```erlang
-spec request_relay(binary(), map()) ->
    {ok, relay_session()} | {error, term()}.

{ok, Session} = macula_relay_node:request_relay(TargetNodeId, #{}).
```

**Elixir:**
```elixir
{:ok, session} = :macula_relay_node.request_relay(target_node_id, %{})
```

---

## Integration Examples

### Example 1: P2P Chat Application

**Erlang:**
```erlang
-module(chat_client).
-export([connect_to_peer/1, send_message/2]).

connect_to_peer(PeerNodeId) ->
    %% NAT-aware connection - automatically handles traversal
    case macula_nat_connector:connect(PeerNodeId, #{timeout => 15000}) of
        {ok, Conn} ->
            log_connection_method(Conn),
            {ok, Conn};
        {error, Reason} ->
            {error, {connection_failed, Reason}}
    end.

log_connection_method(Conn) ->
    Method = macula_connection:get_info(Conn, connection_method),
    io:format("Connected via: ~p~n", [Method]).

send_message(Conn, Message) ->
    macula_connection:send(Conn, {chat_message, Message}).
```

**Elixir:**
```elixir
defmodule ChatClient do
  def connect_to_peer(peer_node_id) do
    # NAT-aware connection - automatically handles traversal
    case :macula_nat_connector.connect(peer_node_id, %{timeout: 15_000}) do
      {:ok, conn} ->
        log_connection_method(conn)
        {:ok, conn}

      {:error, reason} ->
        {:error, {:connection_failed, reason}}
    end
  end

  defp log_connection_method(conn) do
    method = :macula_connection.get_info(conn, :connection_method)
    IO.puts("Connected via: #{inspect(method)}")
  end

  def send_message(conn, message) do
    :macula_connection.send(conn, {:chat_message, message})
  end
end
```

### Example 2: Monitoring NAT Changes

**Erlang:**
```erlang
-module(nat_monitor).
-behaviour(gen_server).

init([]) ->
    %% Subscribe to NAT profile changes
    ok = macula_nat_detector:subscribe(self()),
    {ok, #{}}.

handle_info({nat_profile_changed, OldProfile, NewProfile}, State) ->
    io:format("NAT changed: ~p -> ~p~n", [
        maps:get(mapping, OldProfile),
        maps:get(mapping, NewProfile)
    ]),
    %% Notify application of network change
    notify_network_change(NewProfile),
    {noreply, State}.
```

**Elixir:**
```elixir
defmodule NatMonitor do
  use GenServer

  def init(_opts) do
    # Subscribe to NAT profile changes
    :ok = :macula_nat_detector.subscribe(self())
    {:ok, %{}}
  end

  def handle_info({:nat_profile_changed, old_profile, new_profile}, state) do
    IO.puts("""
    NAT changed: #{inspect(old_profile.mapping)} -> #{inspect(new_profile.mapping)}
    """)

    # Notify application of network change
    notify_network_change(new_profile)
    {:noreply, state}
  end

  defp notify_network_change(profile) do
    # Custom notification logic
    Phoenix.PubSub.broadcast(MyApp.PubSub, "nat:changes", {:nat_changed, profile})
  end
end
```

### Example 3: Running a Relay Node

**Erlang:**
```erlang
-module(relay_server).
-export([start/0, stop/0]).

start() ->
    %% Enable relay with monitoring
    ok = macula_relay_node:enable(#{
        max_sessions => 500,
        bandwidth_limit => 5242880,  % 5 MB/s
        on_session_start => fun log_session_start/1,
        on_session_end => fun log_session_end/1
    }),

    %% Register in distributed registry
    {ok, NodeId} = macula:get_node_id(),
    {ok, Endpoint} = macula:get_public_endpoint(),
    ok = macula_relay_registry:register(NodeId, Endpoint),

    io:format("Relay server started~n").

stop() ->
    ok = macula_relay_node:disable(),
    io:format("Relay server stopped~n").

log_session_start(#{peer_a := A, peer_b := B}) ->
    io:format("Relay session: ~s <-> ~s~n", [A, B]).

log_session_end(#{peer_a := A, peer_b := B, bytes_transferred := Bytes}) ->
    io:format("Session ended: ~s <-> ~s (~p bytes)~n", [A, B, Bytes]).
```

**Elixir:**
```elixir
defmodule RelayServer do
  require Logger

  def start do
    # Enable relay with monitoring
    :ok = :macula_relay_node.enable(%{
      max_sessions: 500,
      bandwidth_limit: 5_242_880,  # 5 MB/s
      on_session_start: &log_session_start/1,
      on_session_end: &log_session_end/1
    })

    # Register in distributed registry
    {:ok, node_id} = :macula.get_node_id()
    {:ok, endpoint} = :macula.get_public_endpoint()
    :ok = :macula_relay_registry.register(node_id, endpoint)

    Logger.info("Relay server started")
  end

  def stop do
    :ok = :macula_relay_node.disable()
    Logger.info("Relay server stopped")
  end

  defp log_session_start(%{peer_a: a, peer_b: b}) do
    Logger.info("Relay session: #{a} <-> #{b}")
  end

  defp log_session_end(%{peer_a: a, peer_b: b, bytes_transferred: bytes}) do
    Logger.info("Session ended: #{a} <-> #{b} (#{bytes} bytes)")
  end
end
```

### Example 4: Phoenix LiveView Integration

**Elixir:**
```elixir
defmodule MyAppWeb.ConnectionLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    if connected?(socket) do
      # Subscribe to NAT changes
      :macula_nat_detector.subscribe(self())

      # Get initial NAT profile
      {:ok, profile} = :macula_nat_detector.get_local_profile()

      {:ok,
       socket
       |> assign(:nat_profile, profile)
       |> assign(:connection_status, :disconnected)
       |> assign(:peers, [])}
    else
      {:ok,
       socket
       |> assign(:nat_profile, nil)
       |> assign(:connection_status, :loading)
       |> assign(:peers, [])}
    end
  end

  def handle_event("connect", %{"peer_id" => peer_id}, socket) do
    socket = assign(socket, :connection_status, :connecting)

    Task.async(fn ->
      :macula_nat_connector.connect(peer_id, %{timeout: 15_000})
    end)

    {:noreply, socket}
  end

  def handle_info({ref, {:ok, conn, method}}, socket) when is_reference(ref) do
    Process.demonitor(ref, [:flush])

    {:noreply,
     socket
     |> assign(:connection_status, {:connected, method})
     |> update(:peers, fn peers -> [conn | peers] end)}
  end

  def handle_info({ref, {:error, reason}}, socket) when is_reference(ref) do
    Process.demonitor(ref, [:flush])

    {:noreply,
     socket
     |> assign(:connection_status, {:error, reason})}
  end

  def handle_info({:nat_profile_changed, _old, new_profile}, socket) do
    {:noreply, assign(socket, :nat_profile, new_profile)}
  end

  def render(assigns) do
    ~H"""
    <div class="nat-status">
      <h3>NAT Status</h3>
      <%= if @nat_profile do %>
        <dl>
          <dt>Mapping</dt>
          <dd><%= @nat_profile.mapping %></dd>
          <dt>Filtering</dt>
          <dd><%= @nat_profile.filtering %></dd>
          <dt>Allocation</dt>
          <dd><%= @nat_profile.allocation %></dd>
          <dt>Public IP</dt>
          <dd><%= format_ip(@nat_profile.public_ip) %></dd>
        </dl>
      <% else %>
        <p>Detecting NAT type...</p>
      <% end %>

      <h3>Connection Status: <%= inspect(@connection_status) %></h3>

      <form phx-submit="connect">
        <input type="text" name="peer_id" placeholder="Peer Node ID" />
        <button type="submit">Connect</button>
      </form>
    </div>
    """
  end

  defp format_ip({a, b, c, d}), do: "#{a}.#{b}.#{c}.#{d}"
end
```

### Example 5: GenServer-based Connection Manager

**Elixir:**
```elixir
defmodule MyApp.ConnectionManager do
  use GenServer
  require Logger

  defstruct [:node_id, :connections, :nat_profile]

  # Client API

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def connect(peer_id), do: GenServer.call(__MODULE__, {:connect, peer_id})
  def disconnect(peer_id), do: GenServer.cast(__MODULE__, {:disconnect, peer_id})
  def list_connections, do: GenServer.call(__MODULE__, :list)
  def get_nat_profile, do: GenServer.call(__MODULE__, :nat_profile)

  # Server Callbacks

  @impl true
  def init(opts) do
    node_id = Keyword.fetch!(opts, :node_id)

    # Detect NAT type on startup
    {:ok, profile} = :macula_nat_detector.detect()

    # Subscribe to profile changes
    :macula_nat_detector.subscribe(self())

    state = %__MODULE__{
      node_id: node_id,
      connections: %{},
      nat_profile: profile
    }

    Logger.info("ConnectionManager started with NAT profile: #{inspect(profile)}")
    {:ok, state}
  end

  @impl true
  def handle_call({:connect, peer_id}, _from, state) do
    case :macula_nat_connector.connect(state.node_id, peer_id, %{timeout: 10_000}) do
      {:ok, conn, method} ->
        Logger.info("Connected to #{peer_id} via #{method}")
        connections = Map.put(state.connections, peer_id, {conn, method})
        {:reply, {:ok, method}, %{state | connections: connections}}

      {:error, reason} = error ->
        Logger.warning("Failed to connect to #{peer_id}: #{inspect(reason)}")
        {:reply, error, state}
    end
  end

  def handle_call(:list, _from, state) do
    {:reply, Map.keys(state.connections), state}
  end

  def handle_call(:nat_profile, _from, state) do
    {:reply, state.nat_profile, state}
  end

  @impl true
  def handle_cast({:disconnect, peer_id}, state) do
    case Map.pop(state.connections, peer_id) do
      {{conn, _method}, connections} ->
        :macula_nat_connector.disconnect(conn)
        {:noreply, %{state | connections: connections}}

      {nil, _} ->
        {:noreply, state}
    end
  end

  @impl true
  def handle_info({:nat_profile_changed, old, new}, state) do
    Logger.info("NAT profile changed: #{inspect(old.mapping)} -> #{inspect(new.mapping)}")

    # Optionally reconnect peers if NAT type changed significantly
    if needs_reconnection?(old, new) do
      reconnect_all_peers(state)
    end

    {:noreply, %{state | nat_profile: new}}
  end

  defp needs_reconnection?(old, new) do
    old.mapping != new.mapping or old.filtering != new.filtering
  end

  defp reconnect_all_peers(state) do
    for {peer_id, {conn, _}} <- state.connections do
      :macula_nat_connector.disconnect(conn)
      send(self(), {:reconnect, peer_id})
    end
  end
end
```

---

## Error Handling

### Common Errors

| Error | Cause | Solution |
|-------|-------|----------|
| `{:error, :detection_timeout}` | NAT detection timed out | Check network, retry with longer timeout |
| `{:error, :no_observers}` | No public peers for detection | Connect to gateway first |
| `{:error, :symmetric_nat}` | Both peers have symmetric NAT | Relay will be used automatically |
| `{:error, :punch_failed}` | Hole punching failed | Falls back to relay automatically |
| `{:error, :no_relay}` | No relay available | Enable relay on more nodes |

### Handling Connection Failures

**Erlang:**
```erlang
connect_with_fallback(TargetNodeId) ->
    case macula_nat_connector:connect(TargetNodeId, #{
        timeout => 10000,
        fallback_relay => true
    }) of
        {ok, Conn} ->
            {ok, Conn};
        {error, no_relay} ->
            %% No relay available, try direct with longer timeout
            macula_nat_connector:connect(TargetNodeId, #{
                timeout => 30000,
                fallback_relay => false,
                max_attempts => 5
            });
        {error, Reason} ->
            {error, Reason}
    end.
```

**Elixir:**
```elixir
def connect_with_fallback(target_node_id) do
  case :macula_nat_connector.connect(target_node_id, %{
    timeout: 10_000,
    fallback_relay: true
  }) do
    {:ok, conn} ->
      {:ok, conn}

    {:error, :no_relay} ->
      # No relay available, try direct with longer timeout
      :macula_nat_connector.connect(target_node_id, %{
        timeout: 30_000,
        fallback_relay: false,
        max_attempts: 5
      })

    {:error, reason} ->
      {:error, reason}
  end
end
```

---

## Performance Considerations

### NAT Detection Timing

| Operation | Typical Latency |
|-----------|-----------------|
| Cached profile lookup | < 1ms |
| Fresh detection (2 observers) | 200-400ms |
| DHT profile fetch | 100-300ms |
| Hole punch coordination | 200-500ms |

### Caching Strategy

**Erlang:**
```erlang
%% Pre-warm cache for known peers at startup
prewarm_nat_cache(KnownPeers) ->
    lists:foreach(
        fun(PeerId) ->
            spawn(fun() -> macula_nat_cache:get_from_dht(PeerId) end)
        end,
        KnownPeers
    ).
```

**Elixir:**
```elixir
# Pre-warm cache for known peers at startup
def prewarm_nat_cache(known_peers) do
  known_peers
  |> Task.async_stream(fn peer_id ->
    :macula_nat_cache.get_from_dht(peer_id)
  end, max_concurrency: 10)
  |> Stream.run()
end
```

---

## Debugging

### Logging NAT Events

**Erlang:**
```erlang
%% Enable debug logging for NAT system
logger:set_module_level(macula_nat_detector, debug),
logger:set_module_level(macula_nat_coordinator, debug),
logger:set_module_level(macula_nat_connector, debug).
```

**Elixir:**
```elixir
# Enable debug logging for NAT system
Logger.configure(level: :debug)

# Or configure specific modules in config/dev.exs
config :logger, :console,
  metadata: [:module],
  format: "$time $metadata[$level] $message\n"
```

### Inspecting NAT State

**Erlang:**
```erlang
%% Get current NAT profile
{ok, Profile} = macula_nat_detector:get_local_profile(),
io:format("NAT Profile: ~p~n", [Profile]).

%% Check cache stats
Stats = macula_nat_cache:stats(),
io:format("Cache Stats: ~p~n", [Stats]).

%% List pending punch attempts
Pending = macula_nat_coordinator:get_pending(),
io:format("Pending Punches: ~p~n", [Pending]).
```

**Elixir:**
```elixir
# Get current NAT profile
{:ok, profile} = :macula_nat_detector.get_local_profile()
IO.inspect(profile, label: "NAT Profile")

# Check cache stats
stats = :macula_nat_cache.stats()
IO.inspect(stats, label: "Cache Stats")

# List pending punch attempts
pending = :macula_nat_coordinator.get_pending()
IO.inspect(pending, label: "Pending Punches")
```

---

## See Also

- [NAT Types Explained](NAT_TYPES_EXPLAINED.md) - Background on NAT types
