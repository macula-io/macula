# Macula Troubleshooting Guide

**Diagnosing and resolving common issues in Macula deployments**

**Audience**: Operators, Developers
**Last Updated**: 2025-11-28

---

## Table of Contents

1. [Quick Diagnostics](#quick-diagnostics)
2. [Connection Issues](#connection-issues)
3. [RPC Problems](#rpc-problems)
4. [PubSub Problems](#pubsub-problems)
5. [Memory Issues](#memory-issues)
6. [DHT Problems](#dht-problems)
7. [Performance Issues](#performance-issues)
8. [Gateway Issues](#gateway-issues)
9. [Debug Tools](#debug-tools)

---

## Quick Diagnostics

### First Steps Checklist

```
1. [ ] Is the gateway process running?
2. [ ] Are all supervisors alive?
3. [ ] Can you reach the bootstrap node?
4. [ ] Is TLS configured correctly?
5. [ ] Are there errors in the logs?
```

### Erlang Shell Health Check

```erlang
%% Quick health check script
QuickCheck = fun() ->
    io:format("Gateway: ~p~n", [is_pid(whereis(macula_gateway))]),
    io:format("Sup: ~p~n", [is_pid(whereis(macula_sup))]),
    io:format("Clients: ~p~n", [macula_gateway_client_manager:count()]),
    io:format("Services: ~p~n", [macula_service_registry:count_services()]),
    ok
end,
QuickCheck().
```

---

## Connection Issues

### Problem: Clients Can't Connect

**Symptoms:**
- Connection timeouts
- TLS handshake failures
- "Connection refused" errors

**Diagnostic Steps:**

```erlang
%% 1. Check listener is running
is_pid(whereis(macula_quic_listener)).

%% 2. Check port is bound
%% From shell:
%% netstat -tlnp | grep 4433

%% 3. Check TLS certificates
ssl:peercert(Socket).
```

**Common Causes & Solutions:**

| Cause | Solution |
|-------|----------|
| Firewall blocking UDP | Open UDP port 4433 (or configured port) |
| TLS cert expired | Renew certificates |
| Wrong cert path in config | Verify `certfile` and `keyfile` paths |
| Listener crashed | Check supervisor, restart gateway |
| Max clients reached | Scale horizontally or increase limit |

**Fix: TLS Certificate Issues**

```erlang
%% Verify certificate is readable
file:read_file("/path/to/cert.pem").

%% Check certificate validity
ssl:pkix_verify_certificate_chain(CertDer, TrustedCerts).

%% Check expiration date
%% openssl x509 -in cert.pem -noout -dates
```

---

### Problem: Connections Drop Unexpectedly

**Symptoms:**
- Clients disconnect randomly
- "connection_closed" errors
- High reconnection rate

**Diagnostic Steps:**

```erlang
%% Check for QUIC transport errors in logs
%% grep -i "quic\|transport\|closed" /var/log/macula.log

%% Check connection state
sys:get_state(ClientPid).
```

**Common Causes & Solutions:**

| Cause | Solution |
|-------|----------|
| Network instability | Check network path, MTU settings |
| Idle timeout | Adjust `idle_timeout_ms` in QUIC config |
| NAT timeout | Enable keepalives |
| Resource limits | Check ulimit, file descriptor limits |

**Fix: Idle Timeout**

```erlang
%% In sys.config
{macula, [
    {quic_options, [
        {idle_timeout_ms, 300000}  %% 5 minutes
    ]}
]}
```

---

## RPC Problems

### Problem: RPC Calls Timeout

**Symptoms:**
- `{error, timeout}` returned from calls
- Slow response times
- High pending call count

**Diagnostic Steps:**

```erlang
%% 1. Check pending calls
sys:get_state(macula_rpc_handler).
%% Look at pending_calls map size

%% 2. Check if service is registered
macula_service_registry:lookup(<<"energy.home.get">>).

%% 3. Check DHT for providers
macula_dht:get(crypto:hash(sha256, <<"energy.home.get">>)).
```

**Common Causes & Solutions:**

| Cause | Solution |
|-------|----------|
| Service not registered | Verify provider called `register/2` |
| Provider unreachable | Check provider node connectivity |
| Handler too slow | Profile handler, add async processing |
| DHT not propagated | Wait for DHT sync (up to 30s) |
| Network partition | Check mesh connectivity |

**Fix: Increase Timeout**

```erlang
%% For specific calls
macula:call(Client, <<"slow.procedure">>, Args, #{timeout => 30000}).

%% Global default (sys.config)
{macula, [
    {rpc_timeout_ms, 10000}  %% 10 seconds
]}
```

---

### Problem: RPC Returns "No Provider"

**Symptoms:**
- `{error, no_provider}` returned
- Service works on some nodes but not others

**Diagnostic Steps:**

```erlang
%% 1. Check local registry
macula_service_registry:list_services().

%% 2. Check DHT directly
Key = crypto:hash(sha256, <<"my.procedure">>),
macula_dht:get(Key).

%% 3. Check if advertised recently
macula_advertisement_manager:get_last_advertised(<<"my.procedure">>).
```

**Common Causes & Solutions:**

| Cause | Solution |
|-------|----------|
| Service not advertised | Call `macula:advertise/3` |
| TTL expired | Re-advertise (auto every 60s) |
| DHT not synced | Wait, then query bootstrap node |
| Wrong procedure name | Check for typos in procedure name |

---

## PubSub Problems

### Problem: Subscribers Not Receiving Events

**Symptoms:**
- Publisher succeeds but subscribers get nothing
- Works locally but not across mesh

**Diagnostic Steps:**

```erlang
%% 1. Check subscription is active
macula_pubsub_handler:list_subscriptions().

%% 2. Check DHT for subscribers
Topic = <<"sensor.temperature">>,
Key = crypto:hash(sha256, Topic),
macula_dht:get(Key).

%% 3. Verify subscriber endpoint is reachable
macula_peer_connector:connect(Endpoint).
```

**Common Causes & Solutions:**

| Cause | Solution |
|-------|----------|
| Subscription not in DHT | Re-subscribe, wait for propagation |
| Wildcard mismatch | Verify wildcard pattern syntax |
| Subscriber crashed | Check subscriber process, restart |
| Endpoint unreachable | Fix network/firewall |
| Cache stale | Wait for cache refresh (60s TTL) |

**Fix: Force DHT Refresh**

```erlang
%% Clear subscriber cache for topic
macula_subscriber_cache:invalidate(<<"sensor.temperature">>).

%% Re-subscribe
macula:subscribe(Client, <<"sensor.temperature">>, Callback).
```

---

### Problem: Duplicate Events

**Symptoms:**
- Same event delivered multiple times
- Subscribers overwhelmed

**Diagnostic Steps:**

```erlang
%% Check for multiple subscriptions
macula_pubsub_handler:list_subscriptions().
%% Should see only one entry per topic
```

**Common Causes & Solutions:**

| Cause | Solution |
|-------|----------|
| Multiple subscribe calls | Track subscription refs, unsubscribe first |
| Stale DHT entries | Wait for TTL expiration |
| Gateway restart during publish | Implement idempotency in subscriber |

---

## Memory Issues

### Problem: Memory Usage Keeps Growing

**Symptoms:**
- Memory climbs over hours/days
- Eventually OOM crash

**Diagnostic Steps:**

```erlang
%% 1. Check process memory
erlang:memory().

%% 2. Find top memory consumers
lists:sort(
    fun({_, A}, {_, B}) -> A > B end,
    [{Pid, element(2, process_info(Pid, memory))}
     || Pid <- processes()]
).

%% 3. Check ETS tables
[{Tab, ets:info(Tab, size), ets:info(Tab, memory)}
 || Tab <- ets:all()].
```

**Common Causes & Solutions:**

| Cause | Solution |
|-------|----------|
| Unbounded message queue | Add backpressure, check slow handlers |
| ETS table growth | Verify TTL cleanup is running |
| Process leak | Check spawn/exit patterns |
| Binary leak | Force GC: `erlang:garbage_collect()` |

**Fix: Force Cleanup**

```erlang
%% Trigger service cleanup manually
macula_advertisement_manager:cleanup_expired().

%% Force garbage collection on specific process
erlang:garbage_collect(whereis(macula_gateway)).
```

---

### Problem: "max_clients_reached" Errors

**Symptoms:**
- New clients rejected
- Warning logs: `Client connection rejected: max_clients_reached`

**Diagnostic Steps:**

```erlang
%% Check current client count
macula_gateway_client_manager:count().

%% Check max limit
application:get_env(macula, max_clients).
```

**Solutions:**

1. **Scale horizontally** - Add more gateway nodes
2. **Increase limit** (if resources allow):
   ```erlang
   %% In sys.config
   {macula, [
       {max_clients, 20000}  %% Double the default
   ]}
   ```
3. **Investigate client churn** - Why are clients not disconnecting?

---

## DHT Problems

### Problem: DHT Queries Timeout

**Symptoms:**
- Service discovery fails
- `{error, timeout}` from DHT operations

**Diagnostic Steps:**

```erlang
%% 1. Check DHT process
is_pid(whereis(macula_dht)).

%% 2. Check bootstrap connectivity
macula_dht:ping().

%% 3. Check DHT routing table
macula_dht:get_routing_table().
```

**Common Causes & Solutions:**

| Cause | Solution |
|-------|----------|
| Bootstrap unreachable | Check network to bootstrap node |
| DHT not initialized | Wait for startup, check logs |
| Network partition | Restore connectivity |
| High DHT load | Scale bootstrap nodes |

---

### Problem: Services Not Propagating

**Symptoms:**
- Service works on registering node
- Other nodes can't discover it

**Diagnostic Steps:**

```erlang
%% On provider node
macula_service_registry:list_services().

%% On consumer node
macula_dht:get(crypto:hash(sha256, <<"service.name">>)).
```

**Common Causes & Solutions:**

| Cause | Solution |
|-------|----------|
| DHT replication delay | Wait up to 30 seconds |
| Partition during advertisement | Re-advertise service |
| TTL too short | Increase `service_ttl_ms` |

---

## Performance Issues

### Problem: High Latency

**Symptoms:**
- RPC calls take > 100ms
- User-perceived slowness

**Diagnostic Steps:**

```erlang
%% 1. Check cache hit rate
macula_subscriber_cache:stats().
%% Should see high hit_rate

%% 2. Profile a call
{Time, Result} = timer:tc(fun() ->
    macula:call(Client, Proc, Args)
end),
io:format("Call took ~p ms~n", [Time / 1000]).
```

**Common Causes & Solutions:**

| Cause | Solution |
|-------|----------|
| Cache miss | Warm up cache, check TTL settings |
| Slow handler | Profile handler code |
| Network latency | Check network path |
| DHT overloaded | Add more DHT nodes |
| QUIC handshake overhead | Enable connection reuse |

**Fix: Enable Caching**

```erlang
%% Verify caching is enabled (should be by default)
application:get_env(macula, enable_subscriber_cache).
%% Should return {ok, true}
```

---

### Problem: Low Throughput

**Symptoms:**
- PubSub < 1000 msg/sec
- System seems slow under load

**Diagnostic Steps:**

```erlang
%% Check for backpressure
sys:get_state(macula_gateway_pubsub).
%% Look at queue sizes

%% Check scheduler utilization
scheduler:utilization(1000).
```

**Solutions:**

1. **Enable caching** (see Performance Guide)
2. **Batch messages** - Send in groups
3. **Reduce DHT queries** - Increase cache TTL
4. **Profile handlers** - Find bottlenecks

---

## Gateway Issues

### Problem: Gateway Crashes on Startup

**Symptoms:**
- Gateway fails to start
- Supervisor keeps restarting

**Diagnostic Steps:**

```erlang
%% Check crash logs
%% grep "CRASH\|EXIT\|error" /var/log/macula.log

%% Try manual start to see error
macula_gateway:start_link(Config).
```

**Common Causes & Solutions:**

| Cause | Solution |
|-------|----------|
| Missing TLS certs | Provide valid cert/key paths |
| Port already in use | Change port or stop conflicting service |
| Invalid config | Verify sys.config syntax |
| Missing dependency | Check all deps started |

---

### Problem: Gateway Becomes Unresponsive

**Symptoms:**
- Gateway process alive but not handling requests
- Message queue growing

**Diagnostic Steps:**

```erlang
%% 1. Check message queue
process_info(whereis(macula_gateway), message_queue_len).

%% 2. Check if processing
sys:get_status(macula_gateway).

%% 3. Check for locks
erlang:process_info(whereis(macula_gateway), [status, current_stacktrace]).
```

**Solutions:**

1. **Restart gateway** - Last resort: `supervisor:restart_child(macula_sup, macula_gateway).`
2. **Find slow handler** - Profile message handling
3. **Add flow control** - Implement backpressure

---

## Debug Tools

### Enabling Debug Logging

```erlang
%% Temporarily enable debug logs
logger:set_primary_config(level, debug).

%% For specific module
logger:set_module_level(macula_gateway, debug).

%% Reset to normal
logger:set_primary_config(level, info).
```

### Tracing

```erlang
%% Trace function calls
dbg:tracer().
dbg:p(all, c).
dbg:tpl(macula_gateway, handle_call, '_', []).

%% Stop tracing
dbg:stop().
```

### State Inspection

```erlang
%% Get process state (gen_server)
sys:get_state(macula_gateway).

%% Get ETS table contents
ets:tab2list(macula_peers).

%% Process info
process_info(whereis(macula_gateway)).
```

### Remote Shell

```bash
# Connect to running node
/opt/macula/bin/macula remote_console

# Or via remsh
erl -name debug@localhost -setcookie macula -remsh macula@hostname
```

### Log Analysis Commands

```bash
# Find errors in last hour
journalctl -u macula --since "1 hour ago" | grep -i error

# Count warnings by type
grep -oP '\[warning\] \K[^:]+' /var/log/macula.log | sort | uniq -c | sort -rn

# Watch logs in real-time
tail -f /var/log/macula.log | grep -E "(error|warning|CRASH)"
```

---

## See Also

- [Monitoring Guide](MONITORING_GUIDE.md) - Metrics and alerting
- [Performance Guide](PERFORMANCE_GUIDE.md) - Optimization techniques
- [Memory Management](../../architecture/memory_management/README.md) - Bounded resource design
- [RPC Guide](../developer/RPC_GUIDE.md) - RPC architecture details
- [PubSub Guide](../developer/PUBSUB_GUIDE.md) - PubSub architecture details
