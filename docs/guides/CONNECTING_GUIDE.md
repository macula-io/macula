# Macula SDK — Connecting Guide

**How an application connects to the Macula relay mesh.**

> **Audience:** Erlang/OTP applications using the SDK to publish, subscribe,
> or call procedures across a Macula relay mesh. Since v3.11.0.

---

## TL;DR

```erlang
%% 1. Pick relay seeds.
Seeds = [<<"https://relay-1.example.com:4433">>,
         <<"https://relay-2.example.com:4433">>].

%% 2. Connect — returns a *pool*.
{ok, Pool} = macula:connect(Seeds, #{}).

%% 3. Use the pool.
ok          = macula:publish(Pool, Realm, Topic, Payload).
{ok, _Sub}  = macula:subscribe(Pool, Realm, Topic, self()).

%% 4. Stop when done.
ok = macula:close(Pool).
```

`Pool` is a process identifier — pass it to every subsequent SDK call.

---

## The pool model

`macula:connect/2` returns a `macula_client` **pool**, not a single
connection. The pool owns:

| Concern | What the pool does |
|---|---|
| **Links** | One `macula_station_link` worker per seed, all sharing one identity |
| **Replication** | `publish/5` fans the frame to N healthy links |
| **Replay** | When a link dies, the pool respawns it and replays subscriptions |
| **Dedup** | Inbound EVENT frames are deduped by `(Realm, Publisher, Seq)` before fan-out |
| **Failover** | Subscribe/publish operations only count healthy links — a dead link is excluded |

From the application's point of view there is one handle (`Pool`) and
one subscriber message format (`{macula_event, SubRef, Topic, Payload,
Meta}`) — regardless of which underlying link delivered any given event.

---

## Seeds

A **seed** is a relay endpoint. Three accepted shapes:

```erlang
%% URL binary (preferred)
<<"https://relay-1.example.com:4433">>

%% URL string
"https://relay-1.example.com:4433"

%% Map form (when you already have host + port)
#{host => <<"relay-1.example.com">>, port => 4433}
```

Pass a list. Order is not significant — every seed gets a link.

```erlang
Seeds = [<<"https://relay-eu.example.com:4433">>,
         <<"https://relay-us.example.com:4433">>,
         <<"https://relay-asia.example.com:4433">>].
{ok, Pool} = macula:connect(Seeds, #{}).
```

A pool with three seeds spawns three links. Each link runs an
independent CONNECT/HELLO handshake against its assigned station.

---

## The `Opts` map

Every option has a default. Most apps pass `#{}`.

| Option | Type | Default | What it does |
|---|---|---|---|
| `identity` | `macula_identity:key_pair()` | auto-generated | Ed25519 keypair shared by every link |
| `replication_factor` | `pos_integer()` | `1` | How many links accept each PUBLISH |
| `capabilities` | `non_neg_integer()` | `0` | Capability bitmap forwarded in CONNECT |
| `alpn` | `[binary()]` | `[<<"macula">>]` | QUIC ALPN list |
| `connect_timeout_ms` | `pos_integer()` | `30_000` | Per-link CONNECT timeout |
| `dedup_window_ms` | `non_neg_integer()` | `60_000` | Inbound-EVENT dedup window |
| `dedup_sweep_ms` | `pos_integer()` | `30_000` | Dedup-table sweep interval |

### Identity

If you don't pass `identity`, the pool generates a fresh keypair on
boot. That is fine for ephemeral clients but means every restart looks
like a brand-new node to the mesh.

For long-lived processes, persist a keypair and pass it explicitly:

```erlang
Identity = my_app_keystore:load_or_create_identity(),
{ok, Pool} = macula:connect(Seeds, #{identity => Identity}).
```

The pool uses **one shared identity for every link**. Stations see the
pool as a single peer (one pubkey, even though it is reachable at N
relay endpoints). This matters for:

- **Subscription delivery.** Stations relay each EVENT to a single
  subscriber pubkey, not per-link. The pool dedupes the resulting
  multi-relay copies before fan-out.
- **DHT presence.** The pool's pubkey appears once in the DHT.
- **Authorization.** UCAN delegations target one pubkey, not N.

### Replication factor

```erlang
{ok, Pool} = macula:connect(Seeds, #{replication_factor => 2}).

%% This PUBLISH goes to TWO healthy links (if available).
ok = macula:publish(Pool, Realm, Topic, Payload).
```

`publish/4,5` returns `ok` as soon as **at least one** of the selected
links accepts the frame. Partial success counts as success — the
remaining links are best-effort. If the pool has zero spawned links
the call returns `{error, {transient, no_healthy_station}}`; the
caller may retry.

When `replication_factor` exceeds the number of healthy links the
pool publishes to whatever it has.

---

## Lifecycle

```
                   start_link_for_seed
       ┌──────────┐    every seed          ┌─────────────────┐
       │  Pool    │ ────────────────────▶  │ macula_station_ │
       │ (gen_    │                        │ link (worker)   │
       │  server) │ ◀───────────────────── └─────────────────┘
       └──────────┘   monitor + EVENT/       (one per seed)
            ▲          GONE messages
            │
            │ subscribe / publish / close
            ▼
       Application
```

**Boot.** `connect/2` returns immediately. Each link's CONNECT/HELLO
handshake completes asynchronously. `publish/4` and `subscribe/4` issued
before any link finishes handshake will succeed (subscribe is queued)
or fail with `{error, {transient, no_healthy_station}}` (publish, if
literally zero links are usable). Apps that need to await readiness
should poll `macula_mesh_client:is_connected/1` on a representative
link or pause briefly between connect and first publish.

**Link death.** When a link's worker process dies, the pool monitor
fires. The pool:

1. Logs `_macula.client.link_down`.
2. Schedules a respawn after `1s`.
3. On respawn, re-issues every currently-tracked `(Realm, Topic)`
   subscription against the new link. Local subscribers see no gap in the message stream
   (they hold a pool-owned `SubRef` whose lifetime is the pool's,
   not any individual link's).

**Subscriber death.** When a subscriber pid dies, the pool drops its
sub spec from state. The wire-level subscription against the link
persists for the pool's lifetime — one wire sub per `(Realm, Topic)`
multiplexed across local consumers (Phase 4 will tighten this).

**Pool close.** `macula:close(Pool)` stops every link and emits
`{macula_event_gone, SubRef, pool_closed}` to every subscriber once.

---

## Embedding in a supervision tree

Use `child_spec/3` to drop the pool under your application's supervisor:

```erlang
-module(my_app_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Seeds = my_config:relay_seeds(),
    Identity = my_keystore:load_identity(),

    Pool = macula:child_spec(
             mesh_pool,
             Seeds,
             #{identity => Identity, replication_factor => 2}),

    {ok, {#{strategy => one_for_one,
            intensity => 5,
            period => 60},
          [Pool]}}.
```

The pool is a `permanent` worker. If it crashes, your supervisor
restarts it with the same seeds and opts. Subscribers must
re-subscribe on restart.

To look up the pool from elsewhere in your app, register it. The
simplest pattern is a wrapping function:

```erlang
-module(my_app_mesh).
-export([pool/0, publish/2]).

pool() ->
    [{_, Pool, _, _}] = supervisor:which_children(my_app_sup),
    Pool.

publish(Topic, Payload) ->
    macula:publish(pool(), my_app_realm(), Topic, Payload).
```

For higher-volume code paths, use `gproc` or register the pool by name.

---

## Multiple realms on one pool

The pool is realm-agnostic. A single pool multiplexes any number of
realms; the realm is passed per call:

```erlang
{ok, Pool} = macula:connect(Seeds, #{}).

ok = macula:publish(Pool, RealmA, TopicA, PayloadA),
ok = macula:publish(Pool, RealmB, TopicB, PayloadB).

{ok, SubA} = macula:subscribe(Pool, RealmA, TopicA, self()),
{ok, SubB} = macula:subscribe(Pool, RealmB, TopicA, self()).
%% ↑ same topic, different realms — two distinct subscriptions
```

There is no cost to mixing realms on one pool versus running one pool
per realm. The pool's topic index is keyed by `{Realm, Topic}` end to
end.

See [PUBSUB_GUIDE.md](PUBSUB_GUIDE.md) for the realm format and how
realms are derived.

---

## Diagnostics

The pool emits a small set of structured diagnostic events via
`macula_diagnostics:event/2`. Subscribe to them in your observability
layer:

| Event topic | When | Meta |
|---|---|---|
| `_macula.client.link_start_failed` | A `start_link/1` against a seed returned `{error, _}` | `seed`, `reason` |
| `_macula.client.link_down` | A live link's worker process died | `seed`, `pid`, `reason` |

Subscriber-pid deaths and pool close are **not** logged by the pool —
they are normal lifecycle events.

---

## Connection model summary

> One pool. N seeds. One identity. Realm-per-call.
> Subscribe and you receive `{macula_event, SubRef, Topic, Payload, Meta}`.
> Close and every subscriber gets one `{macula_event_gone, SubRef, pool_closed}`.

For the publish/subscribe surface in detail, read
[PUBSUB_GUIDE.md](PUBSUB_GUIDE.md). For the breaking changes between
the pre-3.11.0 single-connection client and the 3.11.0 pool, read
[../migrations/V1_TO_V2_PUBSUB.md](../migrations/V1_TO_V2_PUBSUB.md).

---

## See also

- [`macula:connect/2`](https://hexdocs.pm/macula/macula.html#connect-2) — facade
- [`macula_client`](https://hexdocs.pm/macula/macula_client.html) — pool implementation
- [PubSub Guide](PUBSUB_GUIDE.md) — publish/subscribe semantics
- [Authorization Guide](AUTHORIZATION_GUIDE.md) — UCAN/DID identity
- [Topic Naming Guide](TOPIC_NAMING_GUIDE.md) — `realm/org/app/domain/name_v{N}` shape
