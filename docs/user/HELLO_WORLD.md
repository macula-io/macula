# Macula HTTP/3 Mesh - Hello World Tutorial

**Build your first distributed application on Macula**

---

## What We'll Build

A **distributed chat application** where:
- Multiple nodes can join a chat room
- Users can send messages that appear on all nodes
- Messages are routed via the Macula mesh (pub/sub)
- Users can query "who's online" (RPC call)
- Graceful handling of nodes joining/leaving

**Time to complete**: 30 minutes

**Prerequisites**:
- Completed [Quick Start Guide](QUICK_START.md)
- Basic Erlang or Elixir knowledge
- Macula installed and working

---

## Project Structure

We'll create a new Mix (Elixir) or Rebar3 (Erlang) project:

```
macula_chat/
├── config/
│   └── config.exs          # Application configuration
├── lib/
│   ├── macula_chat.ex      # Application entry point
│   ├── chat_room.ex        # Chat room GenServer
│   └── chat_client.ex      # User client
├── mix.exs                 # Project definition
└── README.md
```

---

## Step 1: Create New Project

### Using Mix (Elixir)

```bash
mix new macula_chat --sup
cd macula_chat
```

### Using Rebar3 (Erlang)

```bash
rebar3 new app macula_chat
cd macula_chat
```

---

## Step 2: Add Macula Dependency

### Mix (Elixir)

Edit `mix.exs`:

```elixir
defmodule MaculaChat.MixProject do
  use Mix.Project

  def project do
    [
      app: :macula_chat,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {MaculaChat.Application, []}
    ]
  end

  defp deps do
    [
      {:macula, "~> 0.6"}
    ]
  end
end
```

### Rebar3 (Erlang)

Edit `rebar.config`:

```erlang
{erl_opts, [debug_info]}.

{deps, [
    {macula, "0.6.6"}
]}.

{shell, [
    {apps, [macula_chat]}
]}.
```

### Install Dependencies

```bash
# Mix
mix deps.get

# Rebar3
rebar3 get-deps
```

---

## Step 3: Configure Macula

### Mix Configuration

Create `config/config.exs`:

```elixir
import Config

config :macula,
  realm: "io.macula.chat",
  listen_port: System.get_env("MACULA_PORT", "4433") |> String.to_integer(),
  discovery: [
    methods: [:static, :mdns],
    static_nodes: []  # Add bootstrap nodes via env var
  ],
  topology: [
    type: :k_regular,
    k: 2
  ],
  cert_mode: :auto_generate,
  log_level: :info

# Chat-specific config
config :macula_chat,
  username: System.get_env("CHAT_USER", "Anonymous"),
  room: System.get_env("CHAT_ROOM", "general")
```

### Rebar3 Configuration

Create `config/sys.config`:

```erlang
[
 {macula, [
   {realm, <<"io.macula.chat">>},
   {listen_port, 4433},
   {discovery, [
     {methods, [static, mdns]},
     {static_nodes, []}
   ]},
   {topology, [
     {type, k_regular},
     {k, 2}
   ]},
   {cert_mode, auto_generate},
   {log_level, info}
 ]},

 {macula_chat, [
   {username, <<"Anonymous">>},
   {room, <<"general">>}
 ]}
].
```

---

## Step 4: Implement Chat Room

### Elixir Implementation

Create `lib/chat_room.ex`:

```elixir
defmodule MaculaChat.ChatRoom do
  @moduledoc """
  Chat room GenServer that handles:
  - Subscribing to chat messages
  - Publishing messages to the room
  - Tracking online users
  """

  use GenServer
  require Logger

  @topic_prefix "io.macula.chat.room"

  ## Client API

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Send a message to the chat room"
  def send_message(message) do
    GenServer.cast(__MODULE__, {:send_message, message})
  end

  @doc "Get list of online users (RPC)"
  def get_online_users do
    GenServer.call(__MODULE__, :get_online_users)
  end

  @doc "Join a chat room"
  def join_room(room_name) do
    GenServer.call(__MODULE__, {:join_room, room_name})
  end

  ## Server Callbacks

  def init(opts) do
    username = Keyword.get(opts, :username, "Anonymous")
    room = Keyword.get(opts, :room, "general")

    state = %{
      username: username,
      room: room,
      topic: "#{@topic_prefix}.#{room}",
      presence_topic: "#{@topic_prefix}.#{room}.presence",
      online_users: %{}
    }

    # Subscribe to room messages
    :ok = Macula.PubSub.subscribe(state.topic, self())

    # Subscribe to presence (join/leave notifications)
    :ok = Macula.PubSub.subscribe(state.presence_topic, self())

    # Register RPC endpoint for "who's online"
    rpc_name = "chat.#{room}.users"
    :ok = Macula.RPC.register(rpc_name, fn _ ->
      {:ok, Map.keys(state.online_users)}
    end)

    # Announce presence
    announce_join(state)

    # Schedule periodic presence heartbeat
    schedule_heartbeat()

    Logger.info("Joined chat room: #{room} as #{username}")

    {:ok, state}
  end

  def handle_call(:get_online_users, _from, state) do
    users = Map.keys(state.online_users)
    {:reply, {:ok, users}, state}
  end

  def handle_call({:join_room, new_room}, _from, state) do
    # Unsubscribe from old room
    Macula.PubSub.unsubscribe(state.topic, self())
    Macula.PubSub.unsubscribe(state.presence_topic, self())

    # Announce leave
    announce_leave(state)

    # Update state
    new_state = %{state |
      room: new_room,
      topic: "#{@topic_prefix}.#{new_room}",
      presence_topic: "#{@topic_prefix}.#{new_room}.presence",
      online_users: %{}
    }

    # Subscribe to new room
    :ok = Macula.PubSub.subscribe(new_state.topic, self())
    :ok = Macula.PubSub.subscribe(new_state.presence_topic, self())

    # Announce join
    announce_join(new_state)

    Logger.info("Switched to chat room: #{new_room}")

    {:reply, :ok, new_state}
  end

  def handle_cast({:send_message, message}, state) do
    # Publish message to room
    payload = %{
      username: state.username,
      message: message,
      timestamp: System.system_time(:millisecond),
      node_id: Macula.node_id()
    }

    :ok = Macula.PubSub.publish(state.topic, payload)

    {:noreply, state}
  end

  def handle_info({:event, _topic, %{type: :message} = event}, state) do
    # Received chat message
    username = event.username
    message = event.message
    timestamp = event.timestamp

    # Format timestamp
    {:ok, dt} = DateTime.from_unix(timestamp, :millisecond)
    time_str = Calendar.strftime(dt, "%H:%M:%S")

    # Print to console
    IO.puts("[#{time_str}] <#{username}> #{message}")

    {:noreply, state}
  end

  def handle_info({:event, _topic, %{type: :join} = event}, state) do
    # User joined
    username = event.username
    node_id = event.node_id

    state = put_in(state.online_users[username], node_id)

    Logger.info("#{username} joined the room")
    IO.puts("*** #{username} joined the room")

    {:noreply, state}
  end

  def handle_info({:event, _topic, %{type: :leave} = event}, state) do
    # User left
    username = event.username

    {_node_id, state} = pop_in(state.online_users[username])

    Logger.info("#{username} left the room")
    IO.puts("*** #{username} left the room")

    {:noreply, state}
  end

  def handle_info({:event, _topic, %{type: :heartbeat} = event}, state) do
    # Presence heartbeat
    username = event.username
    node_id = event.node_id

    state = put_in(state.online_users[username], node_id)

    {:noreply, state}
  end

  def handle_info(:send_heartbeat, state) do
    announce_heartbeat(state)
    schedule_heartbeat()
    {:noreply, state}
  end

  ## Private Functions

  defp announce_join(state) do
    Macula.PubSub.publish(state.presence_topic, %{
      type: :join,
      username: state.username,
      node_id: Macula.node_id(),
      timestamp: System.system_time(:millisecond)
    })
  end

  defp announce_leave(state) do
    Macula.PubSub.publish(state.presence_topic, %{
      type: :leave,
      username: state.username,
      node_id: Macula.node_id(),
      timestamp: System.system_time(:millisecond)
    })
  end

  defp announce_heartbeat(state) do
    Macula.PubSub.publish(state.presence_topic, %{
      type: :heartbeat,
      username: state.username,
      node_id: Macula.node_id(),
      timestamp: System.system_time(:millisecond)
    })
  end

  defp schedule_heartbeat do
    Process.send_after(self(), :send_heartbeat, 30_000)  # Every 30 seconds
  end
end
```

### Erlang Implementation

Create `src/chat_room.erl`:

```erlang
-module(chat_room).
-behaviour(gen_server).

-export([start_link/1, send_message/1, get_online_users/0, join_room/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(TOPIC_PREFIX, <<"io.macula.chat.room">>).

%% Client API

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

send_message(Message) ->
    gen_server:cast(?MODULE, {send_message, Message}).

get_online_users() ->
    gen_server:call(?MODULE, get_online_users).

join_room(RoomName) ->
    gen_server:call(?MODULE, {join_room, RoomName}).

%% Server Callbacks

init(Opts) ->
    Username = proplists:get_value(username, Opts, <<"Anonymous">>),
    Room = proplists:get_value(room, Opts, <<"general">>),

    Topic = <<?TOPIC_PREFIX/binary, ".", Room/binary>>,
    PresenceTopic = <<Topic/binary, ".presence">>,

    State = #{
        username => Username,
        room => Room,
        topic => Topic,
        presence_topic => PresenceTopic,
        online_users => #{}
    },

    %% Subscribe to room messages
    ok = macula_pubsub:subscribe(Topic, self()),
    ok = macula_pubsub:subscribe(PresenceTopic, self()),

    %% Register RPC endpoint
    RpcName = <<"chat.", Room/binary, ".users">>,
    ok = macula_rpc:register(RpcName, fun(_Args) ->
        {ok, maps:keys(maps:get(online_users, State))}
    end),

    %% Announce presence
    announce_join(State),

    %% Schedule heartbeat
    schedule_heartbeat(),

    logger:info("Joined chat room: ~s as ~s", [Room, Username]),

    {ok, State}.

handle_call(get_online_users, _From, State) ->
    Users = maps:keys(maps:get(online_users, State)),
    {reply, {ok, Users}, State};

handle_call({join_room, NewRoom}, _From, State) ->
    %% Unsubscribe from old room
    macula_pubsub:unsubscribe(maps:get(topic, State), self()),
    macula_pubsub:unsubscribe(maps:get(presence_topic, State), self()),

    %% Announce leave
    announce_leave(State),

    %% Update state
    NewTopic = <<?TOPIC_PREFIX/binary, ".", NewRoom/binary>>,
    NewPresenceTopic = <<NewTopic/binary, ".presence">>,

    NewState = State#{
        room => NewRoom,
        topic => NewTopic,
        presence_topic => NewPresenceTopic,
        online_users => #{}
    },

    %% Subscribe to new room
    ok = macula_pubsub:subscribe(NewTopic, self()),
    ok = macula_pubsub:subscribe(NewPresenceTopic, self()),

    %% Announce join
    announce_join(NewState),

    logger:info("Switched to chat room: ~s", [NewRoom]),

    {reply, ok, NewState}.

handle_cast({send_message, Message}, State) ->
    Payload = #{
        type => message,
        username => maps:get(username, State),
        message => Message,
        timestamp => erlang:system_time(millisecond),
        node_id => macula:node_id()
    },

    ok = macula_pubsub:publish(maps:get(topic, State), Payload),

    {noreply, State}.

handle_info({event, _Topic, #{type := message} = Event}, State) ->
    Username = maps:get(username, Event),
    Message = maps:get(message, Event),
    Timestamp = maps:get(timestamp, Event),

    %% Print to console
    {{Y,M,D},{H,Min,S}} = calendar:system_time_to_universal_time(Timestamp, millisecond),
    io:format("[~2..0B:~2..0B:~2..0B] <~s> ~s~n", [H, Min, S, Username, Message]),

    {noreply, State};

handle_info({event, _Topic, #{type := join} = Event}, State) ->
    Username = maps:get(username, Event),
    NodeId = maps:get(node_id, Event),

    OnlineUsers = maps:get(online_users, State),
    NewOnlineUsers = maps:put(Username, NodeId, OnlineUsers),

    io:format("*** ~s joined the room~n", [Username]),

    {noreply, State#{online_users => NewOnlineUsers}};

handle_info({event, _Topic, #{type := leave} = Event}, State) ->
    Username = maps:get(username, Event),

    OnlineUsers = maps:get(online_users, State),
    NewOnlineUsers = maps:remove(Username, OnlineUsers),

    io:format("*** ~s left the room~n", [Username]),

    {noreply, State#{online_users => NewOnlineUsers}};

handle_info({event, _Topic, #{type := heartbeat} = Event}, State) ->
    Username = maps:get(username, Event),
    NodeId = maps:get(node_id, Event),

    OnlineUsers = maps:get(online_users, State),
    NewOnlineUsers = maps:put(Username, NodeId, OnlineUsers),

    {noreply, State#{online_users => NewOnlineUsers}};

handle_info(send_heartbeat, State) ->
    announce_heartbeat(State),
    schedule_heartbeat(),
    {noreply, State}.

%% Private Functions

announce_join(State) ->
    macula_pubsub:publish(maps:get(presence_topic, State), #{
        type => join,
        username => maps:get(username, State),
        node_id => macula:node_id(),
        timestamp => erlang:system_time(millisecond)
    }).

announce_leave(State) ->
    macula_pubsub:publish(maps:get(presence_topic, State), #{
        type => leave,
        username => maps:get(username, State),
        node_id => macula:node_id(),
        timestamp => erlang:system_time(millisecond)
    }).

announce_heartbeat(State) ->
    macula_pubsub:publish(maps:get(presence_topic, State), #{
        type => heartbeat,
        username => maps:get(username, State),
        node_id => macula:node_id(),
        timestamp => erlang:system_time(millisecond)
    }).

schedule_heartbeat() ->
    erlang:send_after(30000, self(), send_heartbeat).
```

---

## Step 5: Update Application Supervisor

### Elixir

Edit `lib/macula_chat/application.ex`:

```elixir
defmodule MaculaChat.Application do
  use Application

  @impl true
  def start(_type, _args) do
    # Get config
    username = Application.get_env(:macula_chat, :username, "Anonymous")
    room = Application.get_env(:macula_chat, :room, "general")

    children = [
      # Start Macula mesh
      {Macula, []},

      # Start chat room
      {MaculaChat.ChatRoom, [username: username, room: room]}
    ]

    opts = [strategy: :one_for_one, name: MaculaChat.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

### Erlang

Edit `src/macula_chat_app.erl`:

```erlang
-module(macula_chat_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Get config
    {ok, Username} = application:get_env(macula_chat, username),
    {ok, Room} = application:get_env(macula_chat, room),

    Children = [
        %% Start Macula mesh
        #{
            id => macula,
            start => {macula, start_link, []},
            restart => permanent,
            type => supervisor
        },

        %% Start chat room
        #{
            id => chat_room,
            start => {chat_room, start_link, [[{username, Username}, {room, Room}]]},
            restart => permanent,
            type => worker
        }
    ],

    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},

    supervisor:start_link({local, macula_chat_sup}, ?MODULE, {SupFlags, Children}).

stop(_State) ->
    ok.
```

---

## Step 6: Create Interactive Client

### Elixir

Create `lib/chat_client.ex`:

```elixir
defmodule MaculaChat.Client do
  @moduledoc """
  Interactive chat client - run from IEx
  """

  @doc "Send a message to the chat room"
  def say(message) when is_binary(message) do
    MaculaChat.ChatRoom.send_message(message)
    :ok
  end

  @doc "List who's online"
  def who do
    {:ok, users} = MaculaChat.ChatRoom.get_online_users()
    IO.puts("\nOnline users (#{length(users)}):")
    Enum.each(users, fn user ->
      IO.puts("  - #{user}")
    end)
    :ok
  end

  @doc "Switch to different room"
  def join(room_name) when is_binary(room_name) do
    :ok = MaculaChat.ChatRoom.join_room(room_name)
    IO.puts("Joined room: #{room_name}")
    :ok
  end

  @doc "Show help"
  def help do
    IO.puts("""

    Macula Chat Client Commands:
    =============================

    Chat.say("message")     - Send a message
    Chat.who()              - List online users
    Chat.join("room")       - Switch to different room
    Chat.help()             - Show this help

    Examples:
      Chat.say("Hello world!")
      Chat.who()
      Chat.join("random")

    """)
    :ok
  end
end

# Alias for convenience
alias MaculaChat.Client, as: Chat
```

### Erlang

Create `src/chat_client.erl`:

```erlang
-module(chat_client).
-export([say/1, who/0, join/1, help/0]).

say(Message) when is_binary(Message) ->
    chat_room:send_message(Message),
    ok.

who() ->
    {ok, Users} = chat_room:get_online_users(),
    io:format("~nOnline users (~p):~n", [length(Users)]),
    lists:foreach(fun(User) ->
        io:format("  - ~s~n", [User])
    end, Users),
    ok.

join(RoomName) when is_binary(RoomName) ->
    ok = chat_room:join_room(RoomName),
    io:format("Joined room: ~s~n", [RoomName]),
    ok.

help() ->
    io:format("~n~s~n", [
        "Macula Chat Client Commands:\n"
        "=============================\n"
        "\n"
        "chat_client:say(<<\"message\">>)     - Send a message\n"
        "chat_client:who()                   - List online users\n"
        "chat_client:join(<<\"room\">>)        - Switch to different room\n"
        "chat_client:help()                  - Show this help\n"
        "\n"
        "Examples:\n"
        "  chat_client:say(<<\"Hello world!\">>).\n"
        "  chat_client:who().\n"
        "  chat_client:join(<<\"random\">>).\n"
    ]),
    ok.
```

---

## Step 7: Run the Chat Application

### Terminal 1: User "Alice"

```bash
# Elixir
CHAT_USER=Alice CHAT_ROOM=general MACULA_PORT=4433 iex -S mix

# Erlang
CHAT_USER=Alice CHAT_ROOM=general MACULA_PORT=4433 rebar3 shell

# You should see:
[info] Macula node started
[info] Joined chat room: general as Alice
```

### Terminal 2: User "Bob"

```bash
# Elixir
CHAT_USER=Bob CHAT_ROOM=general MACULA_PORT=4434 iex -S mix

# Erlang
CHAT_USER=Bob CHAT_ROOM=general MACULA_PORT=4434 rebar3 shell

# Both terminals show:
*** Bob joined the room
```

### Terminal 3: User "Charlie"

```bash
# Elixir
CHAT_USER=Charlie CHAT_ROOM=general MACULA_PORT=4435 iex -S mix

# Erlang
CHAT_USER=Charlie CHAT_ROOM=general MACULA_PORT=4435 rebar3 shell

# All terminals show:
*** Charlie joined the room
```

---

## Step 8: Chat!

### Send Messages

**In Alice's terminal (Elixir)**:
```elixir
Chat.say("Hello everyone!")
```

**In Bob's terminal (Erlang)**:
```erlang
chat_client:say(<<"Hey Alice!">>).
```

**In Charlie's terminal**:
```elixir
Chat.say("What's up?")
```

**All terminals show**:
```
[12:34:56] <Alice> Hello everyone!
[12:34:58] <Bob> Hey Alice!
[12:35:01] <Charlie> What's up?
```

### List Online Users

**In any terminal (Elixir)**:
```elixir
Chat.who()
```

**Output**:
```
Online users (3):
  - Alice
  - Bob
  - Charlie
```

### Switch Rooms

**In Charlie's terminal**:
```elixir
Chat.join("random")
```

**Alice and Bob's terminals show**:
```
*** Charlie left the room
```

**Charlie's terminal shows**:
```
Joined room: random
```

Now Charlie is in a different room and won't see messages in "general".

---

## Step 9: Test Fault Tolerance

### Stop Bob's Node

In Bob's terminal, press `Ctrl+C` twice.

**Alice and Charlie's terminals show**:
```
*** Bob left the room
```

### Restart Bob

Restart Bob's node (same command as before).

**All terminals show**:
```
*** Bob joined the room
```

**Messages continue flowing** - the mesh automatically reconnected Bob.

---

## Understanding the Architecture

### Message Flow (Pub/Sub)

```
Alice's Node                   Macula Mesh                   Bob's Node
┌──────────────┐              ┌─────────────┐              ┌──────────────┐
│ Chat.say()   │──publish───→ │   Topic:    │ ──route───→ │ handle_info  │
│              │              │ io.macula.  │              │ {:event,...} │
│              │              │ chat.room.  │              │              │
│              │              │ general     │              │ IO.puts()    │
└──────────────┘              └─────────────┘              └──────────────┘
```

**How it works**:
1. Alice calls `Chat.say("hello")`
2. ChatRoom GenServer calls `Macula.PubSub.publish(topic, %{message: "hello"})`
3. Macula encodes the message and sends it via QUIC to subscribers
4. Bob's ChatRoom GenServer receives `{:event, topic, payload}`
5. Bob's node prints the message to console

**No central server** - messages route peer-to-peer through the mesh!

### RPC Flow (Who's Online)

```
Alice's Node                   Macula Mesh                   Bob's Node
┌──────────────┐              ┌─────────────┐              ┌──────────────┐
│ Chat.who()   │──RPC call──→ │   Routing   │ ──lookup──→ │ RPC Handler  │
│              │              │   Table     │              │              │
│              │ ←──result──  │   (DHT)     │ ←─return──  │ return users │
│              │              │             │              │              │
│ Print users  │              └─────────────┘              └──────────────┘
└──────────────┘
```

**How it works**:
1. Alice calls `Chat.who()`
2. ChatRoom calls `Macula.RPC.call("chat.general.users", %{})`
3. Macula looks up which node registered "chat.general.users" (could be any node)
4. Macula routes RPC request to that node
5. RPC handler executes and returns list of users
6. Result routes back to Alice
7. Alice prints the list

**Distributed RPC** - any node can register an endpoint, any node can call it!

---

## Enhancements

Try adding these features:

### 1. Private Messages (DMs)

```elixir
# In chat_room.ex
def send_dm(to_username, message) do
  GenServer.cast(__MODULE__, {:send_dm, to_username, message})
end

def handle_cast({:send_dm, to_username, message}, state) do
  # Find target user's node via presence
  case Map.get(state.online_users, to_username) do
    nil ->
      IO.puts("User #{to_username} not found")

    node_id ->
      # Send directly to that node
      topic = "io.macula.chat.dm.#{node_id}"
      payload = %{
        from: state.username,
        to: to_username,
        message: message,
        timestamp: System.system_time(:millisecond)
      }

      Macula.PubSub.publish(topic, payload)
  end

  {:noreply, state}
end
```

### 2. Message History (Last 10 Messages)

```elixir
# In chat_room.ex
def init(opts) do
  # ... existing code ...

  state = Map.put(state, :message_history, [])

  # ... rest of init ...
end

def handle_info({:event, _topic, %{type: :message} = event}, state) do
  # ... existing code to print message ...

  # Store in history
  history = [event | state.message_history] |> Enum.take(10)
  state = Map.put(state, :message_history, history)

  {:noreply, state}
end

def get_history do
  GenServer.call(__MODULE__, :get_history)
end

def handle_call(:get_history, _from, state) do
  {:reply, {:ok, Enum.reverse(state.message_history)}, state}
end
```

### 3. Typing Indicator

```elixir
# In chat_client.ex
def typing do
  # Publish ephemeral "typing" event
  Macula.PubSub.publish("io.macula.chat.room.general.typing", %{
    username: MaculaChat.ChatRoom.get_username(),
    timestamp: System.system_time(:millisecond)
  })
end
```

### 4. File Sharing

Use RPC to request file chunks:

```elixir
def share_file(filename) do
  # Read file and encode as base64
  content = File.read!(filename) |> Base.encode64()

  # Announce file availability
  Macula.PubSub.publish("io.macula.chat.room.general.file", %{
    filename: Path.basename(filename),
    size: byte_size(content),
    owner: Macula.node_id()
  })

  # Register RPC endpoint to serve chunks
  Macula.RPC.register("chat.file.#{filename}", fn %{offset: offset, length: length} ->
    chunk = binary_part(content, offset, length)
    {:ok, %{chunk: chunk}}
  end)
end
```

---

## What You've Learned

Congratulations! You've built a fully distributed chat application using Macula. You now understand:

✅ **Pub/Sub**: How to publish events and subscribe to topics across the mesh
✅ **RPC**: How to register callable endpoints and invoke them from any node
✅ **Mesh Topology**: How nodes discover each other and form a network
✅ **Fault Tolerance**: How the mesh adapts when nodes join/leave
✅ **Presence**: How to track who's online using heartbeats
✅ **BEAM OTP**: How to structure applications with GenServers and supervisors

---

## Next Steps

- **[RPC Guide](../developer/RPC_GUIDE.md)** - Complete RPC documentation
- **[PubSub Guide](../developer/PUBSUB_GUIDE.md)** - Pub/Sub patterns
- **[Development Guide](../developer/DEVELOPMENT.md)** - Contributing to Macula
- **[Glossary](../GLOSSARY.md)** - Terminology reference
- **Build something cool!** Share it with the community

---

**Happy coding!**
