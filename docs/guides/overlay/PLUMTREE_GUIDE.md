# Macula SDK — Plumtree Guide

**Epidemic broadcast trees, realm-scoped PubSub, and an OR-Set CRDT — all
riding on HyParView's active view.**

![Plumtree Eager-Push Tree with Lazy Repair](assets/plumtree_broadcast_tree.svg)

> **Audience:** the same as [HYPARVIEW_GUIDE.md](HYPARVIEW_GUIDE.md) — you're
> building a process that needs to flood a message to every member of a
> realm's overlay, or maintain realm-shared state that converges without
> coordination. If you just want ordinary application PubSub, use
> [PUBSUB_GUIDE.md](../pubsub/PUBSUB_GUIDE.md) instead — it already rides on
> this exact machinery internally (see [Realm PubSub](#realm-pubsub-hecate_pubsub),
> below), wrapped as a supervised OTP behaviour you don't have to drive by
> hand.

---

## Overview

[HyParView](HYPARVIEW_GUIDE.md) gives every realm member a small, bounded
**active view** — a handful of live connections, not a connection to every
other member. Flooding a message reliably over that sparse graph, without
every node needing to know the graph's global shape, is a separate problem:
naive flooding (forward to everyone, always) wastes bandwidth on
duplicates; a fixed spanning tree is bandwidth-optimal but a single failed
edge partitions the broadcast until something notices and repairs it.

**Plumtree** (Leitão, Pereira, Rodrigues, *"Epidemic Broadcast Trees"*,
SRDS 2007) gets both: an **eager-push spanning tree** emerges from the
active view for the common case (one full copy per message, no
duplicates), backed by a **lazy-push gossip layer** that announces
`IHAVE` and grafts a missing branch back in within one round trip whenever
the tree is damaged. The tree isn't configured anywhere — it emerges purely
from which peers duplicate-detect a `GOSSIP` and prune themselves to lazy.

This SDK ships the pure protocol (`hecate_plumtree`) plus two small
capabilities built on top of it: realm-scoped PubSub (`hecate_pubsub`,
`hecate_pubsub_server`, `hecate_pubsub_registry`) and an OR-Set CRDT
(`hecate_or_set`) for realm-shared mutable state. All four are pure or
GenServer-wrapped state machines — none of them touch the network directly;
you drive `hecate_plumtree` the same way you drive `macula_hyparview_proto`,
over [the overlay transport](HYPARVIEW_GUIDE.md#wire-transport-sending-and-receiving-frames).

---

## Eager push, lazy push, and how the tree heals

Every peer in `hecate_plumtree`'s state is in exactly one of two sets:

| Set | Receives | Role |
|---|---|---|
| `eager_push` | Full `GOSSIP` payloads | *Is* the spanning tree — these are the tree edges |
| `lazy_push` | `IHAVE` announcements only (message id, no payload) | Backup path — grafts back to eager on demand |

```erlang
State0 = hecate_plumtree:new(MyIdentity, Realm),
State1 = hecate_plumtree:add_peer(State0, PeerA),   %% starts eager — full GOSSIP
%% ... time passes, PeerA turns out to duplicate a GOSSIP it already had ...
```

The healing loop, end to end:

1. **Publish** — `publish/3` records the message locally, sends `GOSSIP` to
   every eager peer and `IHAVE` to every lazy peer.
2. **Receive GOSSIP, first time** — deliver it, mark the sender eager (a
   `GOSSIP` from a lazy peer means the tree just grew an edge), forward
   `GOSSIP` to every *other* eager peer and `IHAVE` to every lazy peer.
3. **Receive GOSSIP, duplicate** — the sender is redundant on this
   message's path; `PRUNE` them to lazy. This is what keeps the eager set
   converging to an actual tree instead of flooding forever.
4. **Receive IHAVE for a message not yet seen** — record the sender as a
   candidate and `GRAFT` back immediately, promoting them to eager. (A
   production deployment typically delays this briefly to let the eager
   push win the race first — this SDK's MVP grafts right away, which is
   correct, just slightly more eager than optimal.)
5. **Receive GRAFT** — promote the sender to eager and, if the message is
   on hand, reply with the full `GOSSIP`.
6. **Receive PRUNE** — demote the sender to lazy.

```erlang
{State1, Actions, Deliveries} = hecate_plumtree:publish(State0, MsgId, Payload),
lists:foreach(fun({send, Peer, Frame}) -> send_to(Peer, Frame) end, Actions),
%% Deliveries == [{MsgId, Payload}] -- your own local copy, handle it here.

handle_info({macula_overlay_frame, _Ref, Frame, #{sender := From}}, State) ->
    {PT1, Actions, Deliveries} = hecate_plumtree:process(State#state.plumtree, From, Frame),
    lists:foreach(fun({send, Peer, F}) -> send_to(Peer, F) end, Actions),
    lists:foreach(fun({MsgId, Payload}) -> deliver_locally(MsgId, Payload) end, Deliveries),
    {noreply, State#state{plumtree = PT1}};
```

`MsgId` is a caller-chosen 16-byte identifier — dedup and delivery tracking
key off it, so use something collision-resistant (a random token, or a
content hash if you want idempotent re-publish of the same payload to
converge to one delivery).

**Wiring to HyParView**: when the active view changes, tell Plumtree.

```erlang
%% HyParView admitted a new active peer -> it becomes eager immediately.
Plumtree1 = hecate_plumtree:add_peer(Plumtree0, NewActivePeer),
%% HyParView evicted/disconnected a peer -> drop it from both push sets.
Plumtree2 = hecate_plumtree:remove_peer(Plumtree1, GonePeer).
```

Plumtree never asks HyParView anything directly — the wiring is your
dispatcher process reacting to `macula_hyparview_proto:process/4`'s
`Actions` (a `DISCONNECT` sent means call `remove_peer/2`; a successful
`NEIGHBOR`/`JOIN` admission means call `add_peer/2`) and feeding the result
into `hecate_plumtree`.

---

## Realm PubSub (`hecate_pubsub`)

A thin, realm-scoped topic index sits on top of Plumtree fan-out:
`hecate_pubsub` tracks `topic => subscribers` for one realm, converts a
publish into a signed `EVENT` frame, and matches an inbound `EVENT` against
local subscribers. `hecate_pubsub_server` activates it as a `gen_server`
(one process per realm), and `hecate_pubsub_registry` holds the
`RealmTag => pid()` map, spawn-linking a fresh server for a realm the first
time it's needed.

```erlang
{ok, Registry} = hecate_pubsub_registry:start_link(#{identity => MyIdentity}),
{ok, ServerPid} = hecate_pubsub_registry:register(Registry, Realm, MyIdentity),

%% Local subscribe (no network round trip — subscription state is local;
%% Plumtree/OR-Set propagate WHO is subscribed where separately).
ok = hecate_pubsub_server:subscribe(ServerPid, <<"sensors.temperature">>, SubscriberPubkey),

%% Publish: builds the signed EVENT frame + returns matched LOCAL subscribers.
%% Fan-out to remote realm members is your dispatcher handing Frame to
%% hecate_plumtree:publish/3, keyed by a fresh MsgId.
{Frame, LocalSubs} = hecate_pubsub_server:publish(ServerPid, <<"sensors.temperature">>, Payload).
```

This is exactly the machinery `macula_pubsub`/`macula_subscriber`/
`macula_publisher` already ride on **inside a station** for ordinary
application PubSub (see [PUBSUB_GUIDE.md](../pubsub/PUBSUB_GUIDE.md)) — the
station runs one `hecate_pubsub_registry` per identity and drives it from
its own listener. You'd reach for these modules directly only when building
a *different* realm-scoped dispatcher that needs the same primitive (for
example, a realm-membership service maintaining its own overlay separately
from the station's).

`hecate_pubsub_registry`'s realm tags are opaque 32-byte namespaces — it
does not validate who's allowed to use a given tag. Authority (who may
subscribe, who may publish) is a layer above this, e.g. via
[`macula_hyparview_endorsement`](HYPARVIEW_GUIDE.md#realm-gated-admission)
gating who gets into the realm's active view in the first place.

---

## OR-Set CRDT (`hecate_or_set`)

Realm-shared *mutable* state — a member list, a chat thread's participant
set, directory metadata — needs to converge across nodes without
coordination, including under concurrent add/remove. `hecate_or_set` is an
Observed-Remove Set: `add/2` tags each element with a fresh random tag,
`remove/2` tombstones every tag *currently observed* for that element. The
payoff: if Alice adds herself back to a group in the same instant Bob
removes her, the add wins — Bob's remove could only tombstone the tags he'd
actually seen.

```erlang
Set0 = hecate_or_set:new(),
{Set1, AddDelta}    = hecate_or_set:add(Set0, <<"alice">>),
{Set2, RemoveDelta} = hecate_or_set:remove(Set1, <<"alice">>),
hecate_or_set:members(Set2).   %% => []

%% Gossip the delta over Plumtree instead of the whole set on every change:
{_, {send, Peer, Frame}} = {ok, plumtree_frame_for(AddDelta)},
%% ... on the receiving end ...
Set3 = hecate_or_set:apply_delta(RemoteSet, AddDelta).
```

Two convergence paths, both associative/commutative/idempotent so any
delivery order reaches the same final state: `merge/2` for full-state
catch-up sync (e.g. a node rejoining after a partition), `apply_delta/2`
for the common case — one delta per Plumtree `GOSSIP`.

---

## See also

- [HyParView Guide](HYPARVIEW_GUIDE.md) — the active view Plumtree's eager
  push set is built from, and the overlay transport both ride on.
- [PubSub Guide](../pubsub/PUBSUB_GUIDE.md) — the supervised, client-facing
  PubSub wrapper that already uses this machinery internally.
- `plans/PLAN_MACULA_V2_PART3_DISCOVERY.md` §7.2 (Plumtree), §7.4 (OR-Set);
  `plans/PLAN_MACULA_V2_PART6_PROTOCOL.md` §6 (realm PubSub wire format).
- Leitão, Pereira, Rodrigues, *"Epidemic Broadcast Trees"*, SRDS 2007.
- Shapiro, Preguiça, Baquero, Zawirski, *"A Comprehensive Study of
  Convergent and Commutative Replicated Data Types"*, INRIA 2011 (the
  OR-Set construction `hecate_or_set` implements).
