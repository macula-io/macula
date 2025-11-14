# Macula Hello World Tutorial

**Build a distributed chat application in 30 minutes!**

This repository contains a complete, step-by-step tutorial for building your first Macula application - a fully distributed multi-user chat system using pub/sub and RPC.

## What You'll Build

- **Distributed chat** where messages route peer-to-peer through the mesh
- **Presence tracking** to see who's online
- **Room switching** to join different conversations
- **Fault-tolerant** - nodes can leave and rejoin seamlessly

## Full Tutorial

👉 **[Complete Hello World Tutorial](architecture/macula_http3_mesh_hello_world.md)** 👈

The tutorial includes:

- ✅ Complete Elixir and Erlang implementations
- ✅ Step-by-step instructions (9 steps)
- ✅ Working code examples you can copy/paste
- ✅ Architecture explanations with diagrams
- ✅ Enhancement ideas (DMs, file sharing, typing indicators)
- ✅ Fault tolerance testing

## Quick Preview

**Add Macula to your project:**

```elixir
# mix.exs
def deps do
  [
    {:macula, git: "https://github.com/macula-io/macula.git", branch: "main"}
  ]
end
```

**Send a message across the mesh:**

```elixir
# Publish to all subscribers
Macula.PubSub.subscribe("chat.room.general", self())
Macula.PubSub.publish("chat.room.general", %{
  username: "Alice",
  message: "Hello distributed world!"
})
```

**Register an RPC endpoint:**

```elixir
# Any node can call this
Macula.RPC.register("chat.users", fn _args ->
  {:ok, ["Alice", "Bob", "Charlie"]}
end)

# Call from any node in the mesh
{:ok, users} = Macula.RPC.call("chat.users", %{})
```

No central server needed - everything routes peer-to-peer!

## Prerequisites

- Erlang/OTP 26+
- Elixir 1.15+ (if using Mix)
- Basic BEAM knowledge

## Time Commitment

**30 minutes** from zero to working distributed chat.

---

**Ready to get started?** 👉 **[Open the full tutorial](architecture/macula_http3_mesh_hello_world.md)**
