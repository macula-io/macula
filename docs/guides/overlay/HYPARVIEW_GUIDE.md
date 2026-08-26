# Macula SDK — HyParView Guide

**Bounded partial-view membership for realm-scoped overlays.**

![HyParView Active and Passive Views](assets/hyparview_views.svg)

> **Audience:** anyone building a process that maintains a realm's own
> membership on top of the mesh — for example a per-realm dispatcher that
> decides who else is "in" a realm and gossips over that group. If you just
> want to publish/subscribe to topics or call an RPC, you don't need this —
> use [PUBSUB_GUIDE.md](../pubsub/PUBSUB_GUIDE.md) or
> [RPC_GUIDE.md](../rpc/RPC_GUIDE.md) instead. This guide is for the layer
> those are built on when the group of participants isn't "every station",
> but "every member of realm X".

---

## Overview

A station relays opaque frames between whatever peers happen to be
connected to it — it has no notion of realms and doesn't own membership
(see [Clustering](../CLUSTERING_GUIDE.md) for the station-level,
realm-agnostic backbone). Realm membership — "who else belongs to
`io.example.myrealm`, and how do I find them" — is a separate problem, and
naively solving it by having every member connect to every other member
doesn't scale: state and connection cost grow O(N) per node, and a partition
or a burst of churn takes down the whole graph at once.

**HyParView** (Leitão, Pereira, Rodrigues, *"HyParView: A Membership Protocol
for Reliable Gossip-Based Broadcast"*, DSN 2007) solves this with **bounded
partial views**: every member keeps a small, fixed-size **active view** (the
peers it actually holds connections to) and a larger **passive view**
(candidates to promote when an active peer dies). Views self-heal under
churn through gossip alone — no coordinator, no global membership list
anywhere. [Plumtree](PLUMTREE_GUIDE.md) then rides on top of the active
view to broadcast messages efficiently across the whole realm.

This SDK ships HyParView as three pure modules — `macula_hyparview_view`
(the view data structure), `macula_hyparview_proto` (the protocol
orchestrator), and `macula_hyparview_endorsement` (realm-admission gating).
None of them touch the network. You drive them from your own process, using
[`macula_station_link`'s overlay transport](#wire-transport-sending-and-receiving-frames)
to actually move frames.

---

## Active view vs. passive view

| | Active view | Passive view |
|---|---|---|
| What it is | Peers you hold a live connection to | Candidate peers, no connection held |
| Default cap | 5 | `4 ×` active cap (20) |
| Who gossips over it | [Plumtree](PLUMTREE_GUIDE.md) eager-pushes here | Nobody — refreshed by periodic shuffles |
| On overflow | A random active peer is demoted to passive | A random passive peer is dropped |
| On active peer failure | — | A random passive peer is promoted to fill the gap |

Both caps are configurable (`macula_hyparview_view:new/2`), but the paper's
formula — `active_cap = max(5, ceil(log2(N)))`, capped at 15 — holds for
realms up to tens of thousands of members without needing per-realm tuning.

```erlang
Self = macula_identity:public(MyIdentity),
View0 = macula_hyparview_view:new(Self),                    %% defaults
View1 = macula_hyparview_view:new(Self, #{active_cap => 8}), %% custom

macula_hyparview_view:active(View0).           %% => []
macula_hyparview_view:counts(View0).           %% => #{active => 0, passive => 0}
```

`macula_hyparview_view` is pure — every mutator (`add_active/2`,
`add_passive/2`, `promote/2`, `demote/2`, `remove_active/2`,
`remove_passive/2`, `merge_shuffle/2`) takes a view and returns a new one.
Nothing here sends a frame; `macula_hyparview_proto` is what turns a
mutation into the disconnect/forward/ack messages the protocol requires.

---

## The protocol messages

`macula_hyparview_proto:process/4` takes the current view, the sender's
NodeId, an inbound frame, and a `ctx()` map, and returns `{NewView,
Actions}` — a list of `{send, TargetPeer, Frame}` tuples for your process to
transmit. It never blocks and never touches the network itself.

| Frame | Sent when | Effect |
|---|---|---|
| `hyparview_join` | A new member contacts one known realm peer | Receiver adds the joiner to active, forwards `FORWARD_JOIN` to its other active peers, evicts if over cap |
| `hyparview_forward_join` | Relaying a JOIN through the mesh | Forwarded `ARWL` hops (default 6); added to passive at `PRWL` hops (default 3) remaining; accepted into active at ttl 0 or if the active view is too small to forward |
| `hyparview_neighbor` | Ack to a JOIN, or unsolicited after a shuffle-driven promotion | `priority: high` always admits (evicting if needed); `priority: low` only admits if there's room, else adds to passive |
| `hyparview_disconnect` | Graceful active-peer teardown | Sender is demoted to the receiver's passive view |
| `hyparview_shuffle` | Every ~30s, to a random active peer | Forwarded while ttl > 0; at ttl 0, replies with a random sample of the receiver's own view and merges the incoming sample into its passive view |
| `hyparview_shuffle_reply` | Answering a `SHUFFLE` | Sender's sample is merged into the passive view |

A minimal dispatcher loop looks like:

```erlang
handle_info({macula_overlay_frame, _Ref, Frame, #{sender := From}}, State) ->
    {View1, Actions} = macula_hyparview_proto:process(State#state.view, From, Frame, State#state.ctx),
    lists:foreach(fun({send, Target, F}) -> send_to(Target, F, State) end, Actions),
    {noreply, State#state{view = View1}};
```

`send_to/3` is your own responsibility — resolve `Target` (a NodeId) to a
connection and hand `F` to
[`macula_station_link:send_overlay_frame/2`](#wire-transport-sending-and-receiving-frames),
dialing first if you're not already connected to it.

---

## Realm-gated admission

Without gating, any node that can reach a realm member can JOIN its
overlay. `ctx()`'s optional `realm_admin_pubkey` field turns that on: every
JOIN, FORWARD_JOIN, and NEIGHBOR then requires a `realm_member_endorsement`
record (`macula_record:realm_member_endorsement/2,3`) — an admin-signed
statement of the form `{realm, member_node, roles, valid_from,
valid_until}` — signed by that exact key, naming that exact `(realm,
member)` pair, currently inside its validity window. A missing or invalid
endorsement is dropped silently: no ack, no forward, the view is unchanged.
Trust is **never** assumed transitively — a FORWARD_JOIN carries the
original JOIN's endorsement all the way through the relay chain, and every
hop re-verifies it independently rather than trusting the peer that
forwarded it.

Minting an endorsement (done by whoever administers the realm — see
`GuideRealmLifecycle.AdmitRealmMember` in `macula-realm-identity` for a real
example):

```erlang
Realm  = macula_identity:public(AdminIdentity),   %% the admin's own pubkey doubles as the realm id
Member = macula_identity:public(CandidateIdentity),
Unsigned = macula_record:realm_member_endorsement(
             Realm, #{realm => Realm, member_node => Member, roles => [<<"station">>]}),
Endorsement = macula_record:sign(Unsigned, AdminIdentity).
```

Joining with it:

```erlang
Ctx = #{self_id => Member, realm => Realm, identity => CandidateIdentity},
JoinFrame0 = macula_hyparview_proto:build_join(Ctx),
JoinFrame  = JoinFrame0#{record => Endorsement}.
```

And gating admission on the receiving side:

```erlang
GatedCtx = Ctx#{realm_admin_pubkey => Realm,
                %% Required so THIS peer's own NEIGHBOR acks carry proof of
                %% its own membership — a gated receiver drops a NEIGHBOR
                %% with no endorsement attached, same as it would a JOIN.
                self_endorsement => MyOwnEndorsement}.
```

Without `realm_admin_pubkey` in `ctx()`, admission is unconditional — the
opt-in default, useful for a dev-only or single-operator realm where minting
endorsements isn't worth the overhead yet.

---

## Wire transport: sending and receiving frames

`macula_hyparview_proto` never touches a socket. The SDK's client-facing
overlay transport (`macula_station_link:overlay_subscribe/3`,
`overlay_unsubscribe/2`, `send_overlay_frame/2`) is what actually moves
`hyparview_*` frames over an existing connection:

```erlang
{ok, SubRef} = macula_station_link:overlay_subscribe(Link, Realm, self()),
%% ... your process now receives:
%%   {macula_overlay_frame, SubRef, Frame, #{sender := FromNodeId}}
%%   {macula_overlay_gone, SubRef, Reason}   -- on disconnect

ok = macula_station_link:send_overlay_frame(Link, macula_hyparview_proto:build_join(Ctx)).
```

`send_overlay_frame/2` is a raw primitive — you build and sign the frame
yourself (via `macula_hyparview_proto`'s builders), it just puts it on the
wire. There's no wire-level SUBSCRIBE/UNSUBSCRIBE round trip: overlay frames
already arrive addressed at a specific connection, they aren't fanned out by
topic the way PUBLISH/EVENT is.

`send_overlay_frame/2` only reaches whoever is on the *other end* of
`Link` — correct once you're already connected to your intended contact
(e.g. the seed peer a JOIN goes to), but most `{send, TargetPeer, Frame}`
actions `macula_hyparview_proto:process/4` returns name a peer you aren't
directly connected to at all. For that, resolve the target's own current
station first (its published `node_record`'s `station_id` field, keyed by
the target's own pubkey — `macula:find_record(Pool, TargetPeer)`), dial
that station directly the same way [direct-dial](../rpc/RPC_PROTOCOL.md#what-resolution-does-if-you-need-it-raw)
does, then use `send_overlay_frame/3`:

```erlang
ok = macula_station_link:send_overlay_frame(Link, TargetPeer,
                                            macula_hyparview_proto:build_join(Ctx)).
```

The station relays it to whichever of its *other* connections
authenticates as `TargetPeer`, and stamps the delivered copy's
`Meta.sender` with your own authenticated identity — not something you
can spoof by naming a different `TargetPeer` in the frame content, since
there isn't one; the routing lives entirely in the envelope, verified
against the connection that sent it. `macula-realm-identity`'s
`Overlay.SelfPublisher` (publish your own presence) and
`Overlay.PeerResolver.resolve_and_dial/2` (the resolve-then-dial sequence
above, as reusable code) are a concrete example of both halves.

---

## See also

- [Plumtree Guide](PLUMTREE_GUIDE.md) — broadcast trees riding on the active
  view this module maintains.
- [Records Guide](../shared/RECORDS_GUIDE.md) — signed, TTL'd records in
  general; `realm_member_endorsement` is one instance of the pattern.
- [Authorization](../shared/AUTHORIZATION_GUIDE.md) — the broader DID/UCAN
  trust model this endorsement mechanism complements.
- `plans/PLAN_MACULA_V2_PART3_DISCOVERY.md` §7.1 — the original design doc.
- Leitão, Pereira, Rodrigues, *"HyParView: A Membership Protocol for
  Reliable Gossip-Based Broadcast"*, DSN 2007.
