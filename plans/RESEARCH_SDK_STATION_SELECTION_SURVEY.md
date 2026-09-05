# SDK Station-Selection Survey

**Status:** Research complete, no implementation started anywhere.
**Created:** 2026-09-05
**Requested by:** github-com-d7 (coordinator), for github-com-34's station-discovery/rotation design in `macula` (this repo), to be ported to the 6 porcelain SDKs once that design is proven.

## Why this exists

34 is designing a mesh-wide station-discovery + rotation pattern in `macula` (this
Erlang SDK): today every default station list across the ecosystem is hardcoded and
tried in a fixed order, there's no load spreading, and nothing queries
`hecate-services/hecate-stations` (the canonical, live station directory) to build a
list automatically. Once that design is proven here, the plan is to port it to all 6
porcelain SDKs: go, rust, php, ts, py, dotnet.

This doc is ground truth for that later porting phase: for each SDK, exactly how it
picks a station today, whether it already holds multiple simultaneous connections,
whether it touches `hecate_stations.list_stations` at all, and where a rotation/
discovery feature would actually plug into the code as it exists right now. Nothing
here should be taken as a design proposal — it's a survey, not a plan.

## hecate_stations.list_stations, for reference

The canonical directory service (`hecate-services/hecate-stations`) serves one RPC,
`hecate_stations.list_stations`, filterable by `continent`/`country`/`city` (exact
match) or `near => #{lat, lng, limit}` (great-circle nearest-first). Response rows
are shaped `{id, node_id, hostname, city, country, continent, lat, lng, quic_port,
host_advertised}`, built from `node_record`/`station_endpoint` DHT records. It needs
no realm-granted capability beyond its own signing keypair. **None of the 6 SDKs
below reference it in any functional way** — see each section, and the one false
lead in macula-rust.

## Cross-cutting findings (read this before the per-SDK detail)

**1. The "call/publish already pooled, watch/subscribe not" framing turns out to be
backwards more often than not — verify per-SDK, don't assume it generalizes.**
It was given as already-known context going into this survey. What the survey
actually found:

| SDK | Subscribe fans out to every link? | Call/Publish rotate/spread? |
|---|---|---|
| macula-go | **Yes**, always (pool/subscribe.go) | No deliberate rotation (see #2) |
| macula-ts | **Yes**, always, per-subscription (pool.ts:640) | No — first-live-wins, publish capped to 1 |
| macula-dotnet | **Yes**, always (StationPool.cs subscribe path) | No — first-live-wins, publish capped to 1 |
| macula-php / rust / py | N/A — no pool at all | N/A |

So across every SDK mature enough to have a pool concept, **subscribe is the one
that's already fully multi-station**, and it's **call/publish that lack rotation**,
the opposite emphasis from the framing this task started with. Whoever designs the
port should verify this against `macula` (the Erlang reference) directly rather than
carry the original framing forward.

**2. macula-go's only "load spreading" today is an accident of a map, not a feature
— this is a real, flagged latent-regression risk, independent of anything else.**
`Pool.Call()` (`pool/rpc.go:87`) iterates `p.connectedActors()` (`pool/pool.go:296`),
which ranges over `p.links`, declared `map[string]*link` at `pool/pool.go:169`. Go
randomizes map iteration order by language guarantee. That means every `Call()`
today gets an incidental, **undocumented** rotation across healthy links as a side
effect of the data structure choice — not from any deliberate selection logic. There
is no test asserting this, no doc comment claiming it, nothing. **The moment anyone
"cleans up" `links` into a slice (a completely reasonable-looking refactor with no
apparent behavior change), this incidental spreading silently vanishes** and every
`Call()` goes back to hammering whichever link the iteration happens to hit
first — with nothing to notice until load concentrates on one station again. Flag
this to whoever owns `pool.go` independent of the discovery/rotation design work:
either make the rotation deliberate (and tested) now, or leave a comment on `links`
explaining why it must stay a map.

**By contrast, every other SDK's link collection is an ordered array/list/dict that
preserves insertion order deterministically** (macula-ts's plain `RoleLink[]`,
macula-dotnet's `Dictionary<Seed,PooledLink>` which preserves insertion order in
.NET in practice) — so today, **macula-go is the only SDK with any station variation
at all**, and it's fragile. The "no load spreading anywhere" framing is accurate by
design for 5 of 6 SDKs and accidentally/partially true for the 6th.

**3. Publish-side fan-out is capped to 1 station by default in both SDKs mature
enough to have the concept (ts, dotnet) — orthogonal to rotation, worth 34 knowing
about before designing "rotate per one-shot call."** Both `macula-ts`
(`pool.ts:220-230`) and `macula-dotnet` (`StationPool.cs:43`) default
`replicationFactor`/`ReplicationFactor` to 1 and clamp publish's fan-out to it. The
stated reason (ts) is that receivers have no caller-supplied-seq mechanism to
dedupe N replica copies of the same publish into one event. If "rotate the station a
one-shot publish goes to" is in scope for the ported design, this pre-existing cap
either needs raising deliberately (with the dedup gap closed first) or the design
needs to explicitly say publish stays single-target while call rotates.

**4. Every SDK has the same bootstrap-dependency: you need an existing connection to
some seed before you can ask that seed to resolve the real station list.**
`hecate_stations.list_stations` is itself served over the mesh — a station-discovery
layer can't be the thing `Pool.connect`/`StationPool.Connect`/`Session.connect`
itself does, because there's no station to ask yet. The natural shape in every SDK
surveyed is a thin wrapper/factory **above** the existing connect primitive: dial a
seed (today's hardcoded/env-configured list), call `list_stations`, then build the
real seed list and either reconnect or feed it to whatever the SDK's actual pool
constructor is. `macula-dotnet`'s `DhtClient.cs` (thin static-class RPC wrapper over
a mesh procedure) is a reasonable pattern to mirror for the wrapper shape itself.

**5. Porting-effort ranking, roughly least to most new code:**
`macula-dotnet` and `macula-go` already have a mature multi-link pool (rotation
logic can be inserted at one or two clearly-identified choke points) → `macula-ts`
has a pool but with different fan-out philosophy per operation (needs the
publish-cap conversation above) → `macula-php`, `macula-rust`, `macula-py` have
**no pool concept at all** and would need one built from scratch to hold multiple
simultaneous connections for standing subscriptions (though per d7, php's blank
slate is also the *simplest* case in the sense that there's nothing existing to
preserve or work around).

---

## macula-go

- **Station selection today:** `connection.Connect`/`connection.ConnectSeeds`
  (`connection/connection.go:61`, `:83`) are single-target/ordered-fallback
  primitives — no default list, no env var, no config file anywhere in the core
  package or `pool/`.
- **Pool:** `pool.Connect(seeds []Seed, opts Opts)` (`pool/pool.go:190`) dials every
  seed concurrently (not one-then-fallback) and returns immediately; `Call`/
  `Publish` return `ErrNoHealthyStation` until at least one link is up, `Subscribe`
  succeeds immediately and replays onto every link as it (re)connects
  (`pool/pool.go:186-189` doc comment). `Call` (`pool/rpc.go:87`) and `Publish`
  (`pool/rpc.go:33`) both route through `connectedActors()` (`pool/pool.go:296`).
  `Subscribe`'s full-fanout mechanism is `pool/subscribe.go` (`watchLinks`,
  `replayOnto`, `fanoutEvents`, `deliver`).
- **Selection mechanism:** see cross-cutting finding #2 — accidental, via Go map
  iteration randomization on `links map[string]*link` (`pool/pool.go:169`).
- **hecate_stations:** no reference anywhere.
- **Plug-in point:** `connectedActors()` (`pool/pool.go:296`) is the shared choke
  point `Call`/`Publish` both already route through — deliberate rotation would
  replace its current map-range with an explicit, tested selection policy. No
  changes needed for Subscribe (already full-fanout). A discovery wrapper would sit
  above `pool.Connect`, building the `[]Seed` it's called with.

## macula-php

- **Station selection today:** none, anywhere in the library.
  `Session::connect(host, port, identity)` (`src/Session.php:71`) and
  `Session::connectSeeds(seeds[], identity)` (`src/Session.php:98`, tries each in
  order via `connection.ConnectSeeds`, first that answers wins) both require an
  explicit host/port or seed array from the caller. No env var, no config file. No
  `Pool`/multi-connection concept exists in this repo at all — grepped, zero hits
  for `Pool`, `pool`, or any multi-link type. Every example script (`examples/*.php`)
  hardcodes `station-de-frankfurt.macula.io:4433` in the calling code, not the
  library.
- **hecate_stations:** no reference anywhere.
- **Plug-in point:** blank slate for a pool/multi-connection abstraction — nothing
  exists to preserve or work around. A port would add a new class (analogous to
  macula-go's `pool` package) wrapping N `Session`s, since `Session` itself
  (`src/Session.php:17`) is already the right unit to hold multiple of. Simplest
  case in the sense that there's no existing rotation logic, fan-out asymmetry, or
  incidental behavior to reconcile — a clean slot to add the pattern into, per d7.

## macula-rust

- **Station selection today:** none. `connection::connect(host, port, trust,
  identity)` (`src/connection.rs:353`) is the ONLY entry point — no
  `connectSeeds`-equivalent even exists here (more minimal than macula-php on this
  specific point). No env var, no config file, no default list.
- **Pool:** does not exist. No `pool.rs`, no pool-shaped dependency in `Cargo.toml`,
  zero grep hits for `Pool`/`Seed`/`fallback` as types or functions in `src/*.rs`.
- **hecate_stations — one false lead, worth naming explicitly so it isn't mistaken
  for integration later:** `macula-rust-ffi/src/lib.rs:1800` has a unit test
  (`map_containing_list_of_maps_round_trips`) that borrows `hecate_stations
  .list_stations`'s response SHAPE purely as a realistic fixture for testing generic
  FFI value marshaling (nested map-in-list encoding). No functional call, no runtime
  dependency, no station-directory awareness.
- **UniFFI layer:** `FfiSession::connect` (`macula-rust-ffi/src/lib.rs:836-846`) is a
  direct 1:1 pass-through to `connection::connect` (same signature), wrapped only in
  a `tokio::sync::Mutex` for UniFFI's `&self`-only method requirement. **One change
  surface, not two** — a port at the core-crate level needs only a thin new wrapper
  added to the FFI layer afterward, no separate station-selection logic to update
  there.
- **Plug-in point:** built from scratch, more so than any other SDK surveyed — no
  `Seed`-shaped type exists at all (unlike macula-go's `connection.Seed`), so even
  the seed-list type itself would need inventing before a pool wrapping it could be
  built. `Session` (`src/connection.rs:282`) is the existing unit a pool would hold
  N of, mirroring macula-go's `pool` package shape.

## macula-ts

- **Station selection today:** none built into the SDK itself.
  `Pool.connect(seeds: Seed[], controlIdentity, opts)` (`src/pool.ts:271`, `Seed`
  type at `pool.ts:77`) takes an explicit seed array from the caller — no default
  list, no env var, no config file, no `hecate_stations` reference anywhere in
  `src/*.ts`. (macula-mcp's own `DEFAULT_STATIONS`/`MACULA_MESH_STATIONS` env var
  live entirely in macula-mcp's `mesh_config.ts`, layered on top of macula-ts, not
  inside it.) Architecturally this is a thin FFI binding over macula-go's compiled C
  ABI — its own `cabi/` mirrors macula-php's, not a pure-TS reimplementation.
- **Pool, precisely (see cross-cutting #1 for why this corrects the original
  framing):** `#controlLinks` (`pool.ts:224`, one per seed) backs `call()`/
  `publish()`. `subscribe()` (`pool.ts:585`) builds its OWN `RoleLink[]` **per
  subscription**, one per seed (`pool.ts:640`), and awaits ALL of them connecting
  (`pool.ts:641`) — every subscription is fanned out to every configured seed
  simultaneously, each independently reconnect/resubscribe-monitored. `call()`
  (`pool.ts:555`) only tries links that are live RIGHT NOW
  (`#liveControlLinks()`, `pool.ts:466`), first-to-succeed wins.  `publish()`
  (`pool.ts:502`) targets `#liveControlLinks().slice(0, replicationFactor)`, and
  `replicationFactor` is forced to 1 regardless of what's requested
  (`pool.ts:220-230` — see cross-cutting #3).
- **Selection mechanism:** deterministic, not accidental. `#controlLinks` is a plain
  `RoleLink[]` built via `seeds.map(...)` in the constructor (`pool.ts:264`) — order
  is exactly the caller's seed-array order, forever. `#liveControlLinks()`
  (`pool.ts:466-468`) filters in place, preserving order, so `call()`'s loop
  (`pool.ts:562`) always tries `seed[0]` first if live. No incidental randomization
  the way macula-go gets from its map.
- **Plug-in point:** `#liveControlLinks()` (`pool.ts:466`) is the single choke point
  both `call()` and (after the replicationFactor conversation) `publish()` already
  route through — inserting round-robin/random selection there needs no other
  changes to either method. Nothing needs to change for `subscribe()` — already
  fully fanned out. The seed list itself is just the `Pool` constructor's `seeds`
  parameter (`pool.ts:230`); a discovery layer sits entirely outside `pool.ts`, in
  whatever builds the `Seed[]` today.

## macula-py

- **Station selection today:** none. `Session.connect(host, port, identity, *,
  handshake_timeout=...)` (`src/macula/connection.py:91-99`) is the only entry
  point — no seed-list/fallback method exists at all (matching macula-rust's
  minimalism, more so than macula-php). No env var, no config file. The only
  hardcoded station string in the repo is in `examples/quickstart.py:17` and
  `README.md`, not library code.
- **Pool:** does not exist. `src/macula/__init__.py:3`'s `__all__` has no `pool`
  entry; zero grep hits for `Pool`/`connect_seeds`/`ConnectSeeds` in `src/`.
  `Session` (`connection.py:77`) is strictly 1:1 with one station connection.
- **hecate_stations:** no reference anywhere.
- **Native vs FFI — matters for where a fix would live:** fully native Python.
  `pyproject.toml` depends on `aioquic>=1.2.0` for QUIC transport; the wire protocol
  itself (`frame.py`, `cbor.py`, `bolt4.py`, `identity.py`, `blake3_hash.py`,
  `manifest.py`, `content.py`) is reimplemented in pure Python — no `ctypes`/`cffi`/
  compiled-`.so` binding to macula-go anywhere. A ported feature here lives entirely
  in Python code; no macula-go version bump involved, unlike macula-php/macula-ts
  where a wire-protocol-adjacent fix often comes from bumping the underlying Go
  module instead.
- **Plug-in point:** blank slate, same as macula-rust and macula-php — no
  multi-connection scaffolding exists yet. `Session.connect()` (`connection.py:91`)
  is the natural insertion point for a rotate-on-one-shot-call wrapper; a genuine
  pool holding multiple `Session`s concurrently (for standing subscriptions) would
  be new code built around asyncio idioms, not an extension of anything existing.

## macula-dotnet

- **Station selection today:** none — `StationPool.Connect(IReadOnlyList<Seed>
  seeds, Trust trust, KeyPair? identity, StationPoolOptions? options)`
  (`src/Macula/Connection/StationPool.cs:195`) requires an explicit seed list from
  the caller. No env var (`Environment.GetEnvironmentVariable` — zero hits), no
  config file. The only hardcoded station is `examples/Station.cs`, example-only.
- **StationPool, precisely:** holds N concurrently-dialed links, one per seed,
  under one shared identity. `_links` (`StationPool.cs:182`) is a
  `Dictionary<Seed, PooledLink>` built via
  `seeds.Distinct().ToDictionary(s => s, s => new PooledLink(s))` — unlike Go's
  deliberately-randomized map, .NET dictionaries preserve insertion order in
  practice, so iteration order == the caller's seed-list order, deterministically.
  - `CallAsync` (`StationPool.cs:480-525`): `ConnectedLinksSnapshot()`
    (`StationPool.cs:910`) then tries `connected[0]` first, falling through only on
    per-link failure — no rotation.
  - `PublishAsync` (`StationPool.cs:221-264`): same snapshot, `connected.Take(n)`
    where `n = Clamp(ReplicationFactor, 0, connected.Count)`, and
    `ReplicationFactor` defaults to 1 (`StationPool.cs:43`) — see cross-cutting #3,
    same shape as macula-ts's cap.
  - `SubscribeAsync` (`StationPool.cs:286-292` onward): wire-subscribes every
    currently-connected link on first (realm, topic) tracking, replays onto every
    respawned link — already full fan-out, matching macula-go, not the originally-
    assumed gap.
  - **`PickConnectedSession()` (`StationPool.cs:617-620`) is a SEPARATE choke
    point** — `_links.Values.FirstOrDefault(l => l.Connected)?.Session` — used for
    direct-dial STREAM_OPEN driving, does NOT go through `ConnectedLinksSnapshot()`.
    Easy to miss: a rotation fix applied only to `ConnectedLinksSnapshot()` would
    silently leave direct-dial stream-open always hitting the first connected link.
- **Today's v0.3.1 bug sweep (commits `2f79b66`→`209785a`→`0694068`) is orthogonal
  — confirmed by reading the diffs**, not inferred from the memory note that
  triggered the check: all three fixes are about respawn locking, event-channel
  re-entrancy at respawn boundaries, and dispose-guard races. None touched
  `CallAsync`, `PublishAsync`, `ConnectedLinksSnapshot`, or `PickConnectedSession` —
  selection/rotation logic is untouched since it originally shipped.
- **hecate_stations:** no reference anywhere.
- **Plug-in point:** `ConnectedLinksSnapshot()` (`StationPool.cs:910`) is the shared
  choke point for `CallAsync`/`PublishAsync` — one change gives both rotation.
  `PickConnectedSession()` (`StationPool.cs:617`) needs the identical treatment
  SEPARATELY, since it bypasses the snapshot method entirely. Nothing needs to
  change for Subscribe. `src/Macula/Dht/DhtClient.cs` (a thin static-class RPC
  wrapper over a mesh procedure) is the existing pattern to mirror for a
  `hecate_stations.list_stations` wrapper — see cross-cutting #4 for why it can't
  live inside `StationPool.Connect` itself.
