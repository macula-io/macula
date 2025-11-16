# Gateway Startup Investigation

**Date:** 2025-11-10
**Context:** After upgrading to OTP 27, gateway pods crash in CrashLoopBackOff

## Problem Statement

After successfully upgrading the Macula Gateway from OTP 26 to OTP 27:
- ✅ Docker image builds successfully
- ✅ OTP 27 confirmed in logs (ERTS 15.2.7.2)
- ✅ Entrypoint script runs correctly
- ✅ Memory limits increased (2Gi → 4Gi) - no more OOMKilled errors
- ❌ **Gateway completes startup and exits (code 0)** instead of staying running
- ❌ Port 9443 never opens for connections
- ❌ Kubernetes liveness/readiness probes fail

## Investigation Findings

### Root Cause: Empty Supervision Tree

The Macula application has a supervision tree but **NO child processes configured**.

#### File: `apps/macula/src/macula_sup.erl:28-35`

```erlang
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [],  % ← NO CHILDREN!
    {ok, {SupFlags, ChildSpecs}}.
```

**Behavior:**
1. Application starts → `macula_app:start/2` called
2. Supervisor starts → `macula_sup:start_link/0` called
3. Supervisor initializes with **empty child list**
4. Application startup completes successfully
5. No long-running processes = **application exits with code 0**

### Application Architecture

#### Main Application: `macula`

**File:** `apps/macula/src/macula.app.src`

```erlang
{application, macula, [
    {description, "Macula HTTP/3 Mesh Platform - Complete distributed application framework"},
    {vsn, "0.3.4"},
    {registered, []},
    {mod, {macula_app, []}},  % ← Has application callback
    {applications, [
        kernel, stdlib, crypto, public_key, ssl,
        quicer, msgpack, gproc,
        macula_core, macula_quic, macula_protocol,
        macula_membership, macula_routing, macula_topology,
        macula_discovery, macula_pubsub, macula_rpc,
        macula_security,
        macula_gateway,  % ← Gateway is a dependency
        macula_client
    ]},
    ...
]}.
```

**Analysis:**
- Main application starts all dependencies
- `macula_gateway` is listed as a dependency application
- But `macula_gateway` itself is just a **library**, not a running application

#### Gateway Library: `macula_gateway`

**File:** `apps/macula_gateway/src/macula_gateway.app.src`

```erlang
{application, macula_gateway, [
    {description, "An OTP library"},
    {vsn, "0.3.4"},
    {registered, []},
    {applications, [kernel, stdlib]},  % ← No 'mod' tuple!
    {env, []},
    ...
]}.
```

**Analysis:**
- **NO `{mod, {Module, Args}}`** - not a standalone application
- Configured as a library, not a runnable application
- Provides `macula_gateway:start_link/1` for embedding in other apps
- Expected to be started by a parent supervisor

**File:** `apps/macula_gateway/src/macula_gateway.erl:71-82`

The gateway is a `gen_server` meant to be embedded:

```erlang
%% @doc Start the gateway with custom options.
%% Options:
%%   {port, Port} - Listen port (default: 9443)
%%   {realm, Realm} - Default realm (default: "macula.default")
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).
```

### What's Missing

To make the gateway run as a standalone application, we need:

1. **Add the gateway process to the supervision tree**

   In `apps/macula/src/macula_sup.erl`, change:

   ```erlang
   init([]) ->
       SupFlags = #{
           strategy => one_for_one,
           intensity => 10,
           period => 5
       },

       %% Read configuration from environment/config
       Port = application:get_env(macula, gateway_port, 9443),
       Realm = application:get_env(macula, gateway_realm, <<"be.cortexiq.energy">>),

       ChildSpecs = [
           #{
               id => macula_gateway,
               start => {macula_gateway, start_link, [[{port, Port}, {realm, Realm}]]},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [macula_gateway]
           }
       ],
       {ok, {SupFlags, ChildSpecs}}.
   ```

2. **Configure gateway settings**

   In `config/sys.config`:

   ```erlang
   [
       {macula, [
           {gateway_port, 9443},
           {gateway_realm, <<"be.cortexiq.energy">>}
       ]}
   ].
   ```

3. **Read environment variables in config**

   Or use `sys.config.src` with variable substitution:

   ```erlang
   [
       {macula, [
           {gateway_port, ${GATEWAY_PORT}},
           {gateway_realm, <<"${GATEWAY_REALM}">>}
       ]}
   ].
   ```

### Current Configuration Files

#### `config/sys.config`

```erlang
[
  {macula, []}
].
```

Empty configuration - explains why nothing is configured to start.

#### `entrypoint.sh`

The entrypoint correctly sets environment variables:

```bash
GATEWAY_PORT="${GATEWAY_PORT:-9443}"
GATEWAY_REALM="${GATEWAY_REALM:-be.cortexiq.energy}"
RELEASE_COOKIE="${RELEASE_COOKIE:-macula-gateway-cookie}"
NODE_NAME="${NODE_NAME:-macula@127.0.0.1}"
```

But these aren't being read by the Erlang application because:
1. `sys.config` doesn't reference them
2. The supervisor doesn't start the gateway process

## Why It Worked Locally (Historically)

The gateway code was likely developed/tested by:
1. Starting an Erlang shell: `rebar3 shell`
2. Manually starting the gateway: `macula_gateway:start_link([{port, 9443}])`

This works in development but won't work in a release/production environment where the application must start autonomously.

## Test Results

### Local Docker Test

```bash
$ timeout 10 docker run --rm macula/macula-gateway:latest

==================================================
Starting Macula Gateway
==================================================
Node Name:    macula@127.0.0.1
Gateway Port: 9443
Realm:        be.cortexiq.energy
Release Path: /opt/macula
==================================================
Erlang/OTP version:
"27"
==================================================
Exec: /opt/macula/erts-15.2.7.2/bin/erlexec ...
Root: /opt/macula
/opt/macula
=INFO REPORT==== 10-Nov-2025::09:14:09.686625 ===
SIGTERM received - shutting down
```

**Result:** Container starts, Erlang VM starts, immediately exits (SIGTERM from timeout)

### Kubernetes Pod Behavior

```
State:          Running
Last State:     Terminated
  Reason:       Completed
  Exit Code:    0  ← Success, but nothing running!

Events:
  Warning  Unhealthy  Liveness probe failed: dial tcp 10.244.0.26:9443: connect: connection refused
  Warning  Unhealthy  Readiness probe failed: dial tcp 10.244.0.26:9443: connect: connection refused
```

## Solution Summary

The gateway is **architecturally incomplete** for standalone deployment:

1. **Code exists** - `macula_gateway.erl` has all the logic
2. **Not wired up** - Supervisor doesn't start it
3. **Not configured** - No config tells supervisor what to start
4. **Works as library** - Can be embedded in other apps
5. **Doesn't work standalone** - No autonomous startup

## Recommended Fix

Choose one approach:

### Option A: Add Gateway to Main Supervisor (Recommended)

Modify `apps/macula/src/macula_sup.erl` to start the gateway as a supervised child process.

**Pros:**
- Minimal changes
- Leverages existing gateway code
- Gateway runs under supervision

**Cons:**
- Couples gateway to main app

### Option B: Create Gateway-Specific Application

Give `macula_gateway` its own application callback and supervisor:

1. Add `{mod, {macula_gateway_app, []}}` to `macula_gateway.app.src`
2. Create `macula_gateway_app.erl` with start/stop
3. Create `macula_gateway_sup.erl` to supervise gateway processes

**Pros:**
- Gateway can run standalone
- Better separation of concerns
- More modular

**Cons:**
- More files to maintain
- Slightly more complex

### Option C: Simple Permanent Process

For quick fix, start gateway in `macula_app:start/2` without complex supervision:

```erlang
start(_StartType, _StartArgs) ->
    %% Start gateway directly
    Port = application:get_env(macula, gateway_port, 9443),
    Realm = application:get_env(macula, gateway_realm, <<"be.cortexiq.energy">>),
    {ok, Pid} = macula_gateway:start_link([{port, Port}, {realm, list_to_binary(Realm)}]),

    %% Keep reference so it doesn't get garbage collected
    register(macula_gateway_proc, Pid),

    %% Start supervisor (even if empty, keeps app alive)
    macula_sup:start_link().
```

**Pros:**
- Quickest fix
- Gets gateway running immediately

**Cons:**
- No fault tolerance (no supervisor restart)
- Less robust for production

## Next Steps

1. Choose an implementation approach
2. Implement the fix
3. Test locally with Docker
4. Rebuild image and push to registry
5. Deploy to Kubernetes
6. Verify port 9443 accepts connections
7. Test actual gateway functionality

## Related Files

- `/home/rl/work/github.com/macula-io/macula/apps/macula_gateway/src/macula_gateway.erl` - Gateway gen_server
- `/home/rl/work/github.com/macula-io/macula/apps/macula_gateway/src/macula_gateway.app.src` - Gateway app spec (library)
- `/home/rl/work/github.com/macula-io/macula/apps/macula/src/macula_app.erl` - Main app callback
- `/home/rl/work/github.com/macula-io/macula/apps/macula/src/macula_sup.erl` - Main supervisor (empty children)
- `/home/rl/work/github.com/macula-io/macula/apps/macula/src/macula.app.src` - Main app spec
- `/home/rl/work/github.com/macula-io/macula/config/sys.config` - Runtime configuration (empty)
- `/home/rl/work/github.com/macula-io/macula/entrypoint.sh` - Docker entrypoint (works correctly)

## Notes

- This is NOT an OTP 27 compatibility issue
- The OTP upgrade revealed a pre-existing architectural gap
- The gateway code itself is well-written and functional
- The issue is purely in the application startup/supervision wiring
