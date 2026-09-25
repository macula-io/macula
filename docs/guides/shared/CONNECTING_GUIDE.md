# Macula SDK — Connecting Guide

**How an application connects to the Macula relay mesh.**

> **Audience:** Erlang/OTP applications using the SDK to publish, subscribe,
> or call procedures across a Macula relay mesh. Since v3.11.0.

---

## TL;DR

```erlang
%% 1. Pick relay seeds.
Seeds = [<<"quic://relay-1.example.com:4433">>,
         <<"quic://relay-2.example.com:4433">>].

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
| **Dedup** | Each publication is delivered once, keyed on its hash, the SHA-384 of its signed `tbs`, until it expires |
| **Failover** | Subscribe/publish operations only count healthy links — a dead link is excluded |

From the application's point of view there is one handle (`Pool`) and
one subscriber message format (`{macula_event, SubRef, Topic, Payload,
Meta}`) — regardless of which underlying link delivered any given event.

---

## Seeds

A **seed** is a relay endpoint. Three accepted shapes:

```erlang
%% URL binary (preferred)
<<"quic://relay-1.example.com:4433">>

%% URL string
"quic://relay-1.example.com:4433"

%% Map form (when you already have host + port)
#{host => <<"relay-1.example.com">>, port => 4433}
```

The URL scheme is a label, not a switch — the seed parser in macula_station_link
(`parse_seed/1`) extracts `host`/`port` from any scheme with an explicit port and dials over
QUIC regardless of what the scheme text says. `https://relay-1.example.com:4433`
parses and dials identically to the `quic://` form above. Both are seen in
the wild — the SDK's own examples use `quic://` since that's the actual wire
protocol, but don't be surprised to find `https://` in an existing
`MACULA_STATION_SEEDS`-style deployment config; it isn't wrong, it's just a
different label on the same dial.

Pass a list. Order is not significant — every seed gets a link.

```erlang
Seeds = [<<"quic://relay-eu.example.com:4433">>,
         <<"quic://relay-us.example.com:4433">>,
         <<"quic://relay-asia.example.com:4433">>].
{ok, Pool} = macula:connect(Seeds, #{}).
```

A pool with three seeds spawns three links. Each link runs an
independent CONNECT/HELLO handshake against its assigned station.

---

## The `Opts` map

Every option has a default. Most apps pass `#{}`.

| Option | Type | Default | What it does |
|---|---|---|---|
| `node_identity` | `macula_node_keys:node_key()` | generated | Node identity key shared by every link |
| `replication_factor` | `pos_integer()` | `2` | How many links accept each PUBLISH |
| `capabilities` | `non_neg_integer()` | `0` | Capability bitmap forwarded in CONNECT |
| `alpn` | `[binary()]` | `[<<"macula">>]` | QUIC ALPN list |
| `connect_timeout_ms` | `pos_integer()` | `30_000` | Per-link CONNECT timeout |
| `dedup_sweep_ms` | `pos_integer()` | `30_000` | Dedup-table sweep interval |
| `admission_sweep_ms` | `pos_integer()` | `30_000` | Request admission sweep interval: entries past their deadline plus 5 minutes leave |
| `renew_backoff_ms` | `pos_integer()` | `5_000` | First retry delay of a failed chain renewal, doubling, never past the chain's `not_after` (D32) |
| `renew_recheck_ms` | `pos_integer()` | `300_000` | How often a chain past its `not_after` is asked for again, so a re-grant revives the provider |

### Identity

If you don't pass `node_identity`, the pool generates an identity key
on boot, in the node's crypto profile (`crypto_profile` in the `macula`
application environment), with a node_id that meets the puzzle stations
check. That is fine for ephemeral clients but means every restart looks
like a brand-new node to the mesh.

For long-lived processes, generate and save the key once, and load it
on boot:

```erlang
{ok, Profile} = macula_crypto_profile:configured(),

%% Once
{ok, Key} = macula_node_keys:generate(identity, Profile,
                #{puzzle_difficulty => macula_node_keys:puzzle_difficulty()}),
ok = macula_node_keys:save(KeyPath, Key),

%% On every boot
{ok, NodeIdentity} = macula_node_keys:load(KeyPath, identity, Profile),
{ok, Pool} = macula:connect(Seeds, #{node_identity => NodeIdentity}).
```

A key of another purpose, or an identity key in another profile, is
refused: `macula:connect/2` returns `{error, {node_identity, Reason}}`.

The pool also generates its own CONNECT key, a separate key that signs
each connection's proof. Every link uses the same two keys.

The pool uses **one shared identity for every link**. Stations see the
pool as a single peer (one node_id, even though it is reachable at N
relay endpoints). This matters for:

- **Subscription delivery.** Stations relay each EVENT to a single
  subscriber node_id, not per-link. The pool dedupes the resulting
  multi-relay copies before fan-out.
- **DHT presence.** The pool's node_id appears once in the DHT.
- **Authorization.** UCAN delegations target one identity, not N.

### Replication factor

Default is `2`, not `1` (since 10.19.0, and only with 2+ connected
links — a single-seed pool gets no benefit from this): a station can
pass the pool's app-liveness ping and look perfectly healthy while
silently relaying a PUBLISH nowhere for a reason that check can't see
— e.g. it just doesn't serve/route the caller's realm, even though
nothing about the connection itself looks wrong. At
`replication_factor => 1` that single link is the entire story for
every publish — total, silent data loss with `ok` returned throughout.
`2` is the minimum that survives exactly that.

**This does not protect against a wrong `Realm` passed by the caller.**
Every replicated copy of a publish carries the identical `Realm`
argument — a publisher-side realm misconfiguration blackholes every
selected link the same way, replication factor notwithstanding. Raise
`replication_factor` for redundancy against a link-local relay problem
on the station's side, not as a substitute for getting the realm right.

```erlang
{ok, Pool} = macula:connect(Seeds, #{}).

%% This PUBLISH goes to the pool's default of TWO healthy links.
ok = macula:publish(Pool, Realm, Topic, Payload).

%% Opt down to the old single-link behavior if you have a reason to
%% (e.g. a very high-frequency publisher that has already reasoned
%% about the cost/redundancy tradeoff for its own traffic):
{ok, Pool2} = macula:connect(Seeds, #{replication_factor => 1}).

%% Or raise it further for extra redundancy on a low-frequency,
%% high-value fact:
{ok, Pool3} = macula:connect(Seeds, #{replication_factor => 3}).
```

`publish/4,5` returns `ok` as soon as **at least one** of the selected
links accepts the frame. Partial success counts as success — the
remaining links are best-effort. If the pool has zero connected links
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
should poll `macula:links(Pool)` for an entry with `connected => true`,
or pause briefly between connect and first publish.

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

### Private keys in logs

A node's private keys live in the processes that sign with them. Macula
keeps them out of what those processes show and what the node logs:

- A process that holds a key formats its status with the private half
  of each key replaced by `redacted`. `sys:get_status/1`, and the crash
  and logger reports of that process, show a key's public half only.
- Starting the `macula` application, and starting a pool, installs the
  primary logger filter `macula_key_redaction` once, so a pool you
  start without the application is covered too. Nothing removes it,
  stopping the application included, because a process that holds a
  key can outlive the application. In report events of the `otp` and
  `macula` domains the filter redacts every key the same way, wherever
  the report holds it, and a stack frame of a Macula module shows its
  arity in place of its arguments, since those can hold a key. Stack
  frames of your own modules keep their arguments.
- A crash report's stack trace sits outside a process's formatted
  status, so only the filter keeps key material out of it. A logger
  handler of yours that ships raw report terms relies on that filter.

`sys:get_state/1` returns a process's state as it is, keys included, so
keep its output out of logs.

A crash dump copies every process heap, keys included. A release that
runs a pool should set `ERL_CRASH_DUMP_SECONDS=0`, or treat its crash
dumps as secret.

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

See [PUBSUB_GUIDE.md](../pubsub/PUBSUB_GUIDE.md) for the realm format and how
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

These events log at `info`, which OTP's default primary level (`notice`)
does not let through, so set the node's level to `info` to see them. A
station link's `_macula.station_link.disconnected` and
`_macula.station_link.peering_exit` events log at `notice`, so a lost
connection shows at the default level.

Subscriber-pid deaths and pool close are **not** logged by the pool —
they are normal lifecycle events.

---

## Connection model summary

> One pool. N seeds. One identity. Realm-per-call.
> Subscribe and you receive `{macula_event, SubRef, Topic, Payload, Meta}`.
> Close and every subscriber gets one `{macula_event_gone, SubRef, pool_closed}`.

For the publish/subscribe surface in detail, read
[PUBSUB_GUIDE.md](../pubsub/PUBSUB_GUIDE.md).

---

## See also

- [`macula:connect/2`](https://hexdocs.pm/macula/macula.html#connect-2) — facade
- [`macula_client`](https://hexdocs.pm/macula/macula_client.html) — pool implementation
- [PubSub Guide](../pubsub/PUBSUB_GUIDE.md) — publish/subscribe semantics
- [Authorization Guide](AUTHORIZATION_GUIDE.md) — UCAN/DID identity
- [Topic Naming Guide](TOPIC_NAMING_GUIDE.md) — `realm/org/app/domain/name_v{N}` shape
