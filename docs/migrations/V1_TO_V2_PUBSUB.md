# Migration — V1 to V2 Pub/Sub (SDK 3.11.0)

**What broke, what changed, and how to migrate.**

> **Audience:** Applications that depended on the pre-3.11.0 SDK
> facade (`macula:connect/2`, `macula:publish/4`, `macula:subscribe/3`,
> etc.). If you are starting fresh on 3.11.0, read
> [CONNECTING_GUIDE.md](../guides/CONNECTING_GUIDE.md) and
> [PUBSUB_GUIDE.md](../guides/PUBSUB_GUIDE.md) instead.

---

## Why V2

The V1 SDK exposed a **single-connection client** through the
`macula` facade. One process held one QUIC link to one relay. If
that link died you reconnected; if you wanted resilience you wrote
your own multi-link orchestration; if you wanted realm-per-call
semantics you re-encoded them on top of a connect-time realm option.

V2 makes the resilience model explicit:

- The default handle is a **pool** of N links, not a single link.
- The pool owns failover, replication, and inbound-event dedup.
- Realm is **per call**, not connect-time. One pool serves any
  number of realms.

The legacy single-connection client (`macula_mesh_client`) is still
in the tree and still functional. The V1 facade surfaces still in
use (`subscribe/3`, `publish/3`, `call/3,4`, advertise/streams/RPC,
etc.) are unchanged. Only **four V1 facade functions changed
shape**.

---

## What changed in the facade

| Function | V1 (≤3.10.x) | V2 (≥3.11.0) | Change |
|---|---|---|---|
| `macula:connect/2` | Returns `client()` (single link) | Returns `pool()` | Return type |
| `macula:publish/4` | `(Client, Topic, Data, Opts)` | `(Pool, Realm, Topic, Payload)` | **Argument shape** |
| `macula:unsubscribe/2` | Drops V1 client sub | Drops V2 pool sub | Routing target |
| `macula:close/1` | (Stream close) | V2 pool close | **Renamed**: V1 stream close is now `macula:close_stream/1` |

Everything else is unchanged. The fully untouched V1 surfaces:

- `macula:subscribe/3`
- `macula:publish/3`
- `macula:disconnect/1`
- `macula:call/3,4`, `macula:advertise/3,4`, `macula:unadvertise/2`
- `macula:put_record/2`, `macula:find_record/2`,
  `macula:find_records_by_type/2`, `macula:subscribe_records/3`,
  `macula:unsubscribe_records/2`
- All streaming APIs except the renamed `close_stream/1`
- Directed RPC (`call_node`, `resolve`, `list_nodes`)
- Cluster + dist (`ensure_distributed/0`, `join_mesh/1`,
  `join_dist_relay/1`, etc.)

---

## Migration path A — adopt V2 (recommended)

### Connect

```erlang
%% V1
{ok, Client} = macula:connect([Url], #{realm => <<"my-realm">>}).

%% V2
RealmTag = my_realm:tag_for(<<"my-realm">>),  %% 32-byte binary
{ok, Pool} = macula:connect([Url], #{}).
```

The realm is no longer a connect-time option. Derive a 32-byte
realm tag and pass it on every operation.

### Subscribe

```erlang
%% V1
{ok, Sub} = macula:subscribe(Client, Topic, fun(Msg) ->
    handle(Msg)
end).

%% V2 — subscriber pid receives messages
{ok, Sub} = macula:subscribe(Pool, RealmTag, Topic, self()).

%% Then in handle_info/2 (or a receive block):
receive
    {macula_event, Sub, Topic, Payload, _Meta} -> handle(Topic, Payload)
end.
```

V2 delivers events as Erlang messages — `{macula_event, SubRef,
Topic, Payload, Meta}` — to a subscriber **pid** rather than
invoking a callback. If your V1 code used the callback-fun form,
wrap your handler in a small `gen_server` and pass `self()`.

### Publish

```erlang
%% V1
ok = macula:publish(Client, Topic, #{order_id => Id, total => 4999}).
%% Or with opts:
ok = macula:publish(Client, Topic, Payload, #{}).

%% V2
ok = macula:publish(Pool, RealmTag, Topic,
                    #{order_id => Id, total => 4999}).
%% Or with opts:
ok = macula:publish(Pool, RealmTag, Topic, Payload, #{}).
```

The realm sits between `Client` and `Topic`. Mind the position when
porting — the compiler accepts both shapes silently for `publish/4`
because the arities match, but the runtime will reject the V1 shape
because the second argument must be a 32-byte binary.

### Unsubscribe

```erlang
%% V1
ok = macula:unsubscribe(Client, Sub).

%% V2 — same shape, routes to the pool
ok = macula:unsubscribe(Pool, Sub).
```

The function shape is identical; the first argument is now a pool
rather than a V1 client.

### Close

```erlang
%% V1 (the function `close/1' meant "close a stream")
ok = macula:close(Stream).

%% V2 — `close/1` now means "close a pool". Stream-close is renamed.
ok = macula:close(Pool).               %% Close the pool
ok = macula:close_stream(Stream).      %% Close a V1 stream
ok = macula:close_send(Stream).        %% Half-close write side (unchanged)
```

Every callsite of `macula:close/1` against a stream pid must be
renamed to `macula:close_stream/1`. The compiler does **not** catch
this — the arity is the same; the runtime will mis-route.

If you have a project-wide V1 stream client, a single sed pass
covers most of it:

```bash
grep -rn 'macula:close(' apps/ src/ test/
# Audit each hit. If the argument is a stream() pid, rename to close_stream/1.
# If the argument is a pool() pid, leave it alone — it's V2.
```

---

## Migration path B — keep V1 semantics

If you can't move to the pool right now, the V1 single-connection
client is still in the tree at `macula_mesh_client`. Replace the
facade callsites with direct module calls:

```erlang
%% V1 facade — was
{ok, Client} = macula:connect([Url], #{realm => Realm}).
ok = macula:publish(Client, Topic, Payload).
ok = macula:unsubscribe(Client, Sub).

%% V1 direct — now
{ok, Client} = macula_mesh_client:start_link(#{relays => [Url], realm => Realm}).
ok = macula_mesh_client:publish(Client, Topic, Payload).
ok = macula_mesh_client:unsubscribe(Client, Sub).
```

The `subscribe/3`, `publish/3`, `call/3,4` facade surfaces are
unchanged and continue to drive `macula_mesh_client` exactly as
before. You only need to switch to direct-module calls for the
four functions whose facade shape changed.

V1 single-connection clients are slated for retirement at 4.0.0.
Plan a migration to V2 by then.

---

## Behaviour changes that survive the rename

A few semantic differences apply regardless of which migration path
you take.

### Realm shape

V1 used a human-readable realm string passed as `#{realm => Bin}` at
connect. V2 uses a 32-byte tag passed per call.

```erlang
%% V1 — realm was opaque to the client; the relay tagged it
ConnectOpts = #{realm => <<"my-realm">>}.

%% V2 — realm is a fixed 32 bytes
RealmTag = macula_realm:id(<<"my-realm">>).
%% (SHA-256 of the realm name; or use your application's own derivation)
```

If you operate your own realm registry, derive 32-byte tags there.
For the standard registry, use `macula_realm:id/1` (SHA-256 of the
realm name) or the equivalent in your realm domain.

### Subscriber message format

V1 callbacks received a single argument — the payload. V2
subscribers receive a 5-tuple:

```erlang
{macula_event, SubRef, Topic, Payload, Meta}
```

The extra fields are useful: `Meta` carries `realm`, `publisher`,
`seq`, and `delivered_via`. Many V1 callsites that ignored topic /
metadata can keep doing so by pattern-matching only on `Payload`.

### Termination message

V2 subscribers receive **exactly one** terminal message when the
subscription ends:

```erlang
{macula_event_gone, SubRef, Reason}
```

V1 had no equivalent. Handle it in your subscriber's `handle_info/2`
to clean up, restart, or stop. Common reasons: `pool_closed` (the
pool was explicitly closed), `{disconnected, _}` (a link backing
this sub was torn down — the pool will respawn and replay, but
existing subscribers see a `gone` event for the old link's ref).

### Replication

V1 had no concept of replication — one client, one link. V2's
`replication_factor` defaults to **1** (one link accepts each
PUBLISH). Migrating callers don't need to change anything; this
default reproduces V1 behaviour. Up the value when you want
multi-link redundancy:

```erlang
{ok, Pool} = macula:connect(Seeds, #{replication_factor => 2}).
```

### Dedup

V2 dedupes inbound EVENTs by `(Realm, Publisher, Seq)` over a
configurable window. V1 had no dedup — the application was
expected to handle replays itself. If your V1 code had idempotency
guards on the receive path, leave them: they remain useful as
a defence in depth.

### Failover

V1 reconnected after disconnect. V2's pool monitors each link;
when one dies it respawns after 1s and replays subscriptions. The
application sees a continuous event stream as long as **any** link
in the pool is up.

---

## What to test after migrating

1. **Connect.** `macula:connect/2` returns a pid; the pool boots.
2. **Subscribe.** `subscribe/4` returns `{ok, SubRef}`; subscriber
   receives `{macula_event, SubRef, _, _, _}` for matching publishes.
3. **Publish.** `publish/4` returns `ok`; another subscriber on the
   same `(Realm, Topic)` sees the payload.
4. **Realm isolation.** A subscriber on `RealmA` does not see events
   published to `RealmB`, even if `Topic` is identical.
5. **Pool close.** `close/1` triggers exactly one `event_gone` per
   live subscription with reason `pool_closed`.
6. **Subscriber death.** Killing the subscriber pid cleans up the
   sub spec automatically (no `event_gone` is delivered to a dead
   pid; the pool drops the spec on `'DOWN'`).
7. **Stream close rename** (if your app uses streams). Every
   `macula:close/1` callsite against a stream pid is now
   `macula:close_stream/1`.

A working `gen_server` test fixture lives in
`test/macula_facade_tests.erl` for reference.

---

## Removed test files (V1 facade fixtures)

If your project pinned to specific test files inside the SDK, three
were removed in 3.11.0 because the V2 surface superseded them:

- `test/macula_client_SUITE.erl`
- `test/macula_client_integration_SUITE.erl`
- `test/macula_client_pubsub_tests.erl`

V1 still has direct-module test coverage in
`macula_mesh_client_validate_tests.erl` and
`macula_multi_relay_tests.erl`. If you depended on the deleted
files, switch to those.

---

## Cheatsheet

| Action | V1 | V2 |
|---|---|---|
| Connect | `{ok, C} = macula:connect([U], #{realm => R})` | `{ok, P} = macula:connect([U], #{})` |
| Subscribe (callback fun) | `macula:subscribe(C, T, fun(M) -> ... end)` | wrap fun in a gen_server, then `subscribe(P, R, T, self())` |
| Subscribe (pid) | `macula:subscribe(C, T, self())` | `macula:subscribe(P, R, T, self())` |
| Publish | `macula:publish(C, T, Payload)` | `macula:publish(P, R, T, Payload)` |
| Publish w/ opts | `macula:publish(C, T, Payload, Opts)` | `macula:publish(P, R, T, Payload, Opts)` |
| Unsubscribe | `macula:unsubscribe(C, Sub)` | `macula:unsubscribe(P, Sub)` |
| Disconnect | `macula:disconnect(C)` | `macula:close(P)` |
| Close stream | `macula:close(Stream)` | `macula:close_stream(Stream)` |

Where `C` = V1 client, `P` = V2 pool, `R` = 32-byte realm tag,
`T` = topic.

---

## Phase 2 outlook

The 3.11.0 release is **Phase 1** of `PLAN_V2_PARITY`. Phase 2
adds:

- `macula_auth` — UCAN/DID minting, delegation, verification, proof
  carrying. Hard gate item; not optional.
- Plumtree gossip across stations so daemons connected to different
  stations see each other's events without tight DHT coupling.
- Tighter wire-level UNSUBSCRIBE on last-consumer leave.
- Subscription replay with server-side seq checkpointing
  (`at_least_once` opt-in).

These are additive — V2 callers won't break crossing 3.12+ unless we
ship a major.

---

## See also

- [Connecting Guide](../guides/CONNECTING_GUIDE.md) — pool model
- [Pub/Sub Guide](../guides/PUBSUB_GUIDE.md) — V2 surface in detail
- [`macula` facade](https://hexdocs.pm/macula/macula.html) — every
  function (V1 and V2)
- [`macula_mesh_client`](https://hexdocs.pm/macula/macula_mesh_client.html)
  — V1 single-connection client (still in the tree)
- [`macula_client`](https://hexdocs.pm/macula/macula_client.html) —
  V2 pool
