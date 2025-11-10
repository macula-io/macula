# Migration Guide: Macula 0.3.x → 0.4.0

## Overview

**Version 0.4.0** introduces a **breaking change** to improve API clarity and follow industry naming conventions.

**What Changed:**
- Application name: `macula_sdk` → `macula_client`
- All modules: `macula_sdk_*` → `macula_client_*`
- API calls: `:macula_sdk` → `:macula_client`

**Why This Change:**
The naming `macula_sdk` was confusing in application code. When you write `:macula_sdk.call(...)`, it reads as "SDK calling something" which doesn't make sense. The correct naming is `:macula_client.call(...)` - "client making a call" - which is clear and follows industry standards (e.g., `httpc`, `gun_client`, `pgsql_client`).

---

## For Erlang Projects

### 1. Update Dependencies

**Before (0.3.x):**
```erlang
% rebar.config
{deps, [
    {macula, "0.3.4"}
]}.
```

**After (0.4.0):**
```erlang
% rebar.config
{deps, [
    {macula, "0.4.0"}
]}.
```

### 2. Update Module References

**Before (0.3.x):**
```erlang
%% Starting a client
{ok, Client} = macula_sdk:start_link(Url, #{realm => Realm}).

%% Making RPC calls
{ok, Result} = macula_sdk:call(Client, Procedure, Args).

%% Subscribing to topics
ok = macula_sdk:subscribe(Client, Topic, Handler).

%% Publishing events
ok = macula_sdk:publish(Client, Topic, Event).

%% Unsubscribing
ok = macula_sdk:unsubscribe(Client, SubscriptionId).

%% Stopping client
ok = macula_sdk:stop(Client).
```

**After (0.4.0):**
```erlang
%% Starting a client
{ok, Client} = macula_client:start_link(Url, #{realm => Realm}).

%% Making RPC calls
{ok, Result} = macula_client:call(Client, Procedure, Args).

%% Subscribing to topics
ok = macula_client:subscribe(Client, Topic, Handler).

%% Publishing events
ok = macula_client:publish(Client, Topic, Event).

%% Unsubscribing
ok = macula_client:unsubscribe(Client, SubscriptionId).

%% Stopping client
ok = macula_client:stop(Client).
```

### 3. Search and Replace

Use your editor's search and replace feature:

**Find:** `macula_sdk:`
**Replace:** `macula_client:`

Be careful with multi-module matches - review each change.

---

## For Elixir Projects

### 1. Update Dependencies

**Before (0.3.x):**
```elixir
# mix.exs
def deps do
  [
    {:macula, "~> 0.3.4"}
  ]
end
```

**After (0.4.0):**
```elixir
# mix.exs
def deps do
  [
    {:macula, "~> 0.4.0"}
  ]
end
```

### 2. Update Module References

**Before (0.3.x):**
```elixir
# Starting a client
{:ok, client} = :macula_sdk.start_link(url, %{realm: realm})

# Making RPC calls
{:ok, result} = :macula_sdk.call(client, procedure, args)

# Subscribing to topics
:ok = :macula_sdk.subscribe(client, topic, handler)

# Publishing events
:ok = :macula_sdk.publish(client, topic, event)

# Unsubscribing
:ok = :macula_sdk.unsubscribe(client, subscription_id)

# Stopping client
:ok = :macula_sdk.stop(client)
```

**After (0.4.0):**
```elixir
# Starting a client
{:ok, client} = :macula_client.start_link(url, %{realm: realm})

# Making RPC calls
{:ok, result} = :macula_client.call(client, procedure, args)

# Subscribing to topics
:ok = :macula_client.subscribe(client, topic, handler)

# Publishing events
:ok = :macula_client.publish(client, topic, event)

# Unsubscribing
:ok = :macula_client.unsubscribe(client, subscription_id)

# Stopping client
:ok = :macula_client.stop(client)
```

### 3. Search and Replace

Use your editor's search and replace feature:

**Find:** `:macula_sdk`
**Replace:** `:macula_client`

Review each change to ensure correctness.

---

## Step-by-Step Migration Process

### Step 1: Update Dependencies
```bash
# For Erlang projects
rebar3 upgrade macula

# For Elixir projects
mix deps.update macula
```

### Step 2: Update Code References
Use your editor to search and replace all references:
- Erlang: `macula_sdk:` → `macula_client:`
- Elixir: `:macula_sdk` → `:macula_client`

### Step 3: Review Changes
Carefully review all changes, especially if you have wrapper modules or custom client logic.

### Step 4: Test Compilation
```bash
# For Erlang projects
rebar3 compile

# For Elixir projects
mix compile
```

### Step 5: Run Tests
```bash
# For Erlang projects
rebar3 eunit
rebar3 ct

# For Elixir projects
mix test
```

### Step 6: Deploy
Follow your normal deployment process. This is a **breaking change**, so coordinate with your team.

---

## Common Migration Issues

### Issue 1: Module Not Found
**Error:**
```
** (UndefinedFunctionError) function :macula_sdk.call/3 is undefined (module :macula_sdk is not available)
```

**Solution:**
You missed updating some references. Search for `:macula_sdk` or `macula_sdk:` in your codebase.

### Issue 2: Dependency Conflict
**Error:**
```
===> Dependency macula is specified with conflicting version constraints
```

**Solution:**
Ensure all dependencies use `0.4.0` (or `~> 0.4.0` for Elixir). Check umbrella apps and nested dependencies.

### Issue 3: Old Lock File
**Error:**
```
===> Lock file is out of date
```

**Solution:**
```bash
# For Erlang projects
rm rebar.lock
rebar3 get-deps

# For Elixir projects
mix deps.clean macula
mix deps.get
```

---

## No Functional Changes

**Important:** Only the **module name** changed. The **API is identical**.

- All function signatures remain the same
- All options and configurations are unchanged
- All behavior is identical

This is a **cosmetic breaking change** for better developer experience.

---

## Rollback Plan

If you need to rollback:

### Erlang Projects
```erlang
% rebar.config
{deps, [
    {macula, "0.3.4"}
]}.
```

Then revert your code changes (or use `git revert`).

### Elixir Projects
```elixir
# mix.exs
{:macula, "~> 0.3.4"}
```

Then revert your code changes.

---

## Timeline

- **0.3.x**: Deprecated, but still available on Hex.pm
- **0.4.0**: Current stable release (breaking change)
- **0.5.x**: Future releases will use `macula_client` naming

We recommend migrating to 0.4.0 as soon as possible. Version 0.3.x will not receive new features.

---

## Need Help?

- **GitHub Issues**: https://github.com/macula-io/macula/issues
- **Documentation**: https://github.com/macula-io/macula/tree/main/architecture

---

## Summary

| Before (0.3.x) | After (0.4.0) |
|---|---|
| `macula_sdk:call(...)` | `macula_client:call(...)` |
| `:macula_sdk.call(...)` | `:macula_client.call(...)` |
| `{macula, "0.3.4"}` | `{macula, "0.4.0"}` |

**Remember:** Only the module name changed. All functionality remains identical.
