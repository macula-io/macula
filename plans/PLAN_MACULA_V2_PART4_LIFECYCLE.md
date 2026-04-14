# PLAN — Macula V2, Part 4: Lifecycle & Resilience

**Parent:** `PLAN_MACULA_V2_ROOT.md`
**Depends on:** Part 1 (7 pillars intro, identity, scale), Part 2 (tiers, addresses).
**Feeds:** Part 3 (Discovery & Routing — lifecycle invariants drive discovery design).
**Status:** Draft — authored 2026-04-14.
**Scope:** How state is born, refreshed, proven alive, failed, and reaped in V2. The seven pillars fully elaborated. SWIM-Lifeguard specification. BOLT#4-derived failure taxonomy. Connection, record, station, and realm lifecycle state machines. No routing algorithm (Part 3), no wire records (Part 6).

---

## 1. Purpose of this Part

Part 1 declared seven pillars in one sentence each. Part 4 turns each into an implementable specification.

V1 collapsed over 72 hours because lifecycle was implicit. Nothing said *how long* a registration lives, *who* removes it, *what* it means to be reachable, *when* a retry is safe, *what* happens when two providers claim the same slot. The absence of these answers did not mean the answers were free — it meant each module invented its own, inconsistently, and silent bugs bred in the gaps.

Part 4 closes the gaps. Every routable piece of state in V2 has:

- **A single owner process.** Dies when owner dies (Pillar 1).
- **A singleton guarantee where required.** One provider per session-bound key (Pillar 2).
- **A heartbeat, not a process-exists check.** Proven alive end-to-end (Pillar 3).
- **A bounded deadline on every state transition.** Missed deadline ⇒ structured error (Pillar 4).
- **A cascade-refresh obligation on reconnect.** Dependent state resynchronises automatically (Pillar 5).
- **Caller-assigned idempotency keys.** Retries dedupe; register/unregister commute (Pillar 6).
- **An explicit SLA tier under partial failure.** Graceful degradation is specified, not hoped for (Pillar 7).

Part 4 is prescriptive. Part 3 uses these invariants as load-bearing assumptions. If an invariant weakens, Part 3's routing guarantees weaken proportionally.

---

## 2. The lifecycle-first principle

Routing, replication, discovery are all downstream of **who owns what, for how long, and how that ownership is transferred or reaped**. In V1, routing was designed first and ownership retrofitted; the retrofits produced the seven bugs.

V2 inverts the order. Lifecycle is specified first. Every protocol operation fits into one of four categories:

| Category | Examples | Lifecycle question |
|----------|----------|---------------------|
| **Birth** | Station boot, realm join, procedure advertise, subscription, CALL initiation | When does ownership begin? What invariants must hold? |
| **Maintenance** | Heartbeat, record republish, routing-table refresh, SWIM ping | What obligation does the owner carry while alive? What deadline bounds it? |
| **Transition** | Reconnect, failover, tier upgrade, realm re-endorsement | What refreshes automatically? What is the bounded deadline? |
| **Death** | Station stop, key revoke, record tombstone, realm dissolve | Who reaps downstream state? What signals completion? |

Every module in V2 answers all four questions for every type of state it owns. A module that cannot answer is not ready for review.

---

## 3. Pillar 1 — Process-resource binding

> **One owner, one lifetime. External resource dies ⇒ owner dies.**

### 3.1 The rule

Every routable piece of state — DHT record, gproc registration, pg membership, ETS row, subscription, outgoing QUIC stream, inbound CALL context — is held by exactly one BEAM process. That process is the **owner**. When the owner process exits, for any reason, the state is reaped automatically within one scheduler tick.

There is no code path where state outlives its owner. There is no manual cleanup call that a caller has to remember. OTP monitors, supervisor `terminate`, `process_flag(trap_exit, true)` do the work.

### 3.2 The implementation contract

Every `register_*` function returns `{ok, MonitorRef}` or `{error, Reason}`. Never plain `ok`. The caller stores the MonitorRef; the registry cleans up when the referenced process dies.

```erlang
%% Correct V2 shape
-spec register_procedure(procedure_uri(), pid()) ->
    {ok, reference()} | {error, already_registered | invalid_proc | invalid_pid}.
register_procedure(Uri, Pid) ->
    MonRef = erlang:monitor(process, Pid),
    case gproc:reg_other({n, l, {proc, Uri}}, Pid) of
        true  -> {ok, MonRef};
        false -> erlang:demonitor(MonRef, [flush]),
                 {error, already_registered}
    end.
```

Every registry keeps a map `Pid → [MonRef]` and a reverse map `MonRef → {Key, Pid}`. On `{'DOWN', MonRef, process, Pid, _Reason}` the registry drops every key associated with that monitor.

### 3.3 Owner classes

V2 defines six owner classes. Every stateful entity belongs to exactly one.

| Class | Owner lifetime | Examples |
|-------|---------------|---------|
| **Connection-scoped** | Lives as long as one QUIC connection | Per-peer outbound streams, session keys, flow windows |
| **Session-scoped** | Lives as long as one logical session across reconnects | Node ↔ station session, subscription continuity |
| **Call-scoped** | Lives for one CALL in flight | Handler gen_server, retry timer, BOLT#4 failure encoder |
| **Record-scoped** | Lives as long as the DHT record's TTL + republish window | Per-record republish timer, replica set tracker |
| **Membership-scoped** | Lives as long as the station is in a realm | Realm-directory replica custody, SWIM neighbour set |
| **Station-scoped** | Lives as long as the station is running | Identity store, local config, admin endpoint |

Cross-class ownership is forbidden. A call-scoped context cannot hold a station-scoped registration; it holds a *reference* to one that the station-scoped owner maintains.

### 3.4 What "owner dies" means in practice

Owner death triggers a cascade:

1. `{'DOWN', MonRef, process, Pid, Reason}` delivered to every monitor.
2. Each registry erases its entries for that monitor.
3. Downstream registrations (records held *because* this owner was alive) publish tombstones.
4. Peers receive tombstone via gossip within the configured SWIM dissemination window (§5.4) and drop their local replicas.

The tombstone is the only cross-peer signal. No "please unregister" RPC. No cleanup protocol. The tombstone is self-authenticating (signed by the dying entity) and idempotent (Pillar 6).

### 3.5 Anti-patterns V2 refuses

- **Orphaned entries.** Registering without monitoring. V2 code review rejects this on sight.
- **Manual cleanup.** A caller that must remember to call `unregister/1` before exiting. The owner process *is* the registration; its exit *is* the unregister.
- **Detached state.** State stored in ETS without a BEAM-process owner. ETS tables are owned by a named process; table death = state death.
- **Supervisor-owned ephemeral state.** A supervisor's `init/1` registering something on behalf of its children. Children register themselves in `init/1` (or `handle_continue`) so supervisor restart triggers natural reaping.

### 3.6 V1 violation this pillar addresses

V1 bug #6: stale handler_node ghost pids accumulated in gproc because registration survived the owning process. `alive_providers` picked dead handlers at random. In V2, the registration *is* the monitored relationship; dead pid ⇒ auto-unregister; `alive_providers` cannot return a ghost.

---

## 4. Pillar 2 — Single-provider invariant

> **Session-bound keys permit exactly one provider. Registration evicts prior.**

### 4.1 The rule

Some resources are structurally singleton:

- `_dist.tunnel.{NodeId}` — the QUIC stream carrying Erlang distribution to node `NodeId`. Exactly one provider at a time.
- `_handler.call.{CallId}` — the process handling an in-flight CALL. One provider.
- `_swim.probe_target.{NodeId}` — the probe ping for a SWIM round. One probe owner per target.

For each, the registration schema is marked **singleton**. Registering for a singleton key atomically evicts the prior owner.

Non-singleton keys (e.g. `_proc.{Uri}` when a procedure may be served by many nodes) follow additive semantics. Singleton and additive are declared in the key schema, not decided per-call-site.

### 4.2 Eviction mechanics

```erlang
-spec register_singleton(key(), pid()) -> {ok, reference(), evicted_prior()}.
register_singleton(Key, NewPid) ->
    NewMon = erlang:monitor(process, NewPid),
    case gproc:lookup_pids({n, l, Key}) of
        [] ->
            gproc:reg_other({n, l, Key}, NewPid),
            {ok, NewMon, none};
        [OldPid] when OldPid =/= NewPid ->
            OldPid ! {evicted_by, NewPid, Key},
            gproc:unreg_other({n, l, Key}, OldPid),
            gproc:reg_other({n, l, Key}, NewPid),
            {ok, NewMon, {evicted, OldPid}};
        [NewPid] ->
            %% Idempotent re-register
            {ok, NewMon, already_us}
    end.
```

The evicted prior receives `{evicted_by, NewPid, Key}` and is expected to shut down cleanly. Not receiving the message (prior was already dead) is fine — the registry has already reclaimed the slot.

### 4.3 What counts as a "session"

The session boundary for a singleton key depends on the key:

| Key family | Session boundary |
|------------|------------------|
| `_dist.tunnel.{NodeId}` | Live QUIC connection. New connection ⇒ new session ⇒ new provider. |
| `_handler.call.{CallId}` | One call; call-id rotates per call. |
| `_peer.outbound.{PeerId}` | Live QUIC connection. Reconnect ⇒ new session ⇒ eviction. |
| `_swim.probe.{Round}` | One SWIM probe round. |
| `_realm.directory_replica.{RealmId}` | **NOT singleton.** Many stations serve each realm. |

Session boundaries are declared per key family in the registry schema. Violating the schema (e.g. registering a singleton key twice without eviction) is a code error, caught at unit-test time.

### 4.4 V1 violation this pillar addresses

V1 bug #3 + #6: stub URL in DHT ≠ peer_clients key because registration was additive when it should have been singleton; CALL dispatch picked whichever stale entry matched first. V2 marks the key as singleton; eviction is automatic; only one truth.

---

## 5. Pillar 3 — Liveness probes, not existence checks

> **Gproc membership alone does not prove the owner is functional. Every routing decision must be backed by end-to-end heartbeat.**

A process can exist but be unresponsive. A gproc entry can be fresh but the process is blocked on a stuck NIF. A QUIC stream can be "open" but the peer has silently gone away. Existence is necessary; it is not sufficient.

### 5.1 Heartbeat-as-registration

Every registration carries a heartbeat deadline. If the deadline passes without a refresh, the registration is treated as dead *regardless of process liveness*.

```erlang
-spec register_procedure(procedure_uri(), pid(), Opts) -> {ok, reference()}.
%% Opts = #{heartbeat_interval_ms := 5000,
%%          miss_tolerance        := 3}
%%
%% Missing >= miss_tolerance consecutive heartbeats ⇒ registry evicts.
%% Process still alive? It will re-register on next heartbeat attempt.
```

The registry runs a supervisor-owned scanner; every tick it walks registrations and evicts expired ones, sending `{heartbeat_missed, Key}` to the owner. Owner decides whether to re-register or exit.

### 5.2 SWIM-Lifeguard for station-to-station liveness

Station-to-station liveness uses **SWIM** (Das/Gupta/Motivala 2002) with **Lifeguard extensions** (Dadgar et al., HashiCorp). SWIM gives scalable weakly-consistent membership at O(1) message cost per round. Lifeguard reduces false positives under load.

V2 adopts SWIM-Lifeguard verbatim. Parameters tuned to street-level fleet:

| Parameter | V2 value | Rationale |
|-----------|----------|-----------|
| Protocol period T | 2000 ms | Balances detection latency against bandwidth; 500 ms would amplify residential-jitter false positives. |
| Indirect-ping fanout k | 3 | Enough to disambiguate network partition from peer death. |
| Suspicion timeout | 6 × T = 12 s | Lifeguard self-awareness adapts downward under load. |
| Dissemination fanout | log(N) + 1 | N = station count in SWIM group; bounded. |
| Group sizing | ≤ 256 per SWIM instance | Beyond 256, partition into multiple instances scoped by tier + country (§5.6). |
| Refutation backoff | Exponential, 1s → 30s | Flapping suspicion ⇒ longer refute; Lifeguard buddy system. |

**Lifeguard self-awareness.** A station that suspects many peers simultaneously is probably itself congested. Lifeguard detects this ("self-awareness") and shrinks its own suspicion aggressiveness — rather than declaring half the fleet dead during a local CPU spike.

**Lifeguard dogpile mitigation.** Multiple peers independently suspecting the same target coordinate via gossip so only one probe is escalated, reducing convergent load on a genuinely flapping peer.

**Lifeguard refutation-buddy.** A suspected peer can enlist a buddy to corroborate "no, I'm fine" — a second-hand refutation path when the direct path is lossy.

### 5.3 End-to-end heartbeat for handler-scoped state

SWIM covers station-level liveness. Sub-station state needs its own heartbeats:

| State type | Heartbeat mechanism | Period |
|-----------|---------------------|--------|
| Node ↔ station session | Node pings station every 10 s; station responds | 10 s, miss 3 |
| Procedure handler | Gproc heartbeat refresh every 5 s | 5 s, miss 3 |
| DHT record lease | Republish every ~20 min (tRepublish), TTL 60 min (tExpire) | Part 3 §5 |
| Subscription | Station re-announces subs to home peer on every SWIM piggyback | implicit |
| Realm directory replica | Realm admin re-signs every 24h; stations refresh within 48h | 24h, miss 2 |

A missed heartbeat is structured evidence — not timeout evidence. It signals "this target was alive N seconds ago and hasn't refreshed", which is more actionable than "this target hasn't responded for 30 s".

### 5.4 SWIM group composition

Each station is in exactly one primary SWIM group per tier it participates in, plus secondary groups for bridging:

- **Tier-same country group**: all T0s in country X form one (partitioned) SWIM set.
- **Tier-adjacent bridging group**: T1s additionally participate in a broader group spanning T0 membership they aggregate.
- **Continental tier-gateway group**: T2/T3 in a continental SWIM set for cross-country failure detection.

SWIM groups are a layer of Part 4's lifecycle story; the routing table (Part 3) draws from SWIM's current-view as its ground truth.

### 5.5 Dissemination

SWIM messages piggyback:

- Membership delta (suspect/alive/confirm).
- DHT routing-table hints (new peer observed).
- Revocation notices (tombstones to spread).
- Realm-directory refresh nudges.

Piggyback budget: ≤512 bytes per SWIM message. Larger deltas split across rounds. Critical messages (confirmed failure, revocation) get priority.

### 5.6 V1 violation this pillar addresses

V1 bug #1 + #6: DHT records aged out silently; handler pids were "alive" but their QUIC stream had half-closed. In V2, every owner has a heartbeat obligation; half-closed streams fail the heartbeat within one round (2s); `alive_providers` consults heartbeat freshness, not process existence.

---

## 6. Pillar 4 — Fast-fail over silent timeout

> **Known-unreachable targets return structured errors immediately.**

### 6.1 The failure taxonomy

Every CALL failure returns one of a finite set of structured codes. Adapted from **Lightning BOLT#4 onion-failure codes**, which have proven in adversarial conditions that a small, specific taxonomy prevents retry loops and enables post-mortem.

| Code | Name | Meaning | Retry policy |
|------|------|---------|--------------|
| 0x00 | `ok` | Success. | n/a |
| 0x01 | `unknown_next_peer` | Next-hop station not in routing table at this hop. | Retry via different disjoint path. |
| 0x02 | `temporary_relay_failure` | Next-hop station reached but transiently unavailable. | Retry same path after backoff, or different path. |
| 0x03 | `relay_disabled` | Next-hop station administratively disabled for this realm. | Different path; do not retry same. |
| 0x04 | `node_not_found_at_target_relay` | Final-hop station reached, but target node unknown. | Caller-side: refresh node_record; retry after new lookup. |
| 0x05 | `target_realm_refused` | Target node present but refused CALL (realm policy, quota, auth). | Do not retry. Application-level remedy. |
| 0x06 | `loop_detected` | CALL header showed a path that revisits a hop. | Compute-side bug; caller regenerates path. |
| 0x07 | `expiry_too_soon` | CALL deadline would expire before the path can plausibly complete. | Caller extends deadline or picks shorter path. |
| 0x08 | `upstream_congestion` | Next-hop station is rate-limited on this realm / this caller. | Exponential backoff; possibly reduce batch. |
| 0x09 | `invalid_path_header` | Source-routing header corrupt or expired. | Caller recomputes. |
| 0x0A | `crypto_puzzle_invalid` | Target NodeId fails crypto-puzzle validation. | Drop; do not retry. |
| 0x0B | `realm_not_authoritative_here` | Station does not serve this realm. | Retry via station that serves the realm (Part 3 lookup). |
| 0x0C | `tombstoned` | Target was reaped; record has a valid tombstone. | Do not retry. |
| 0x0D | `payload_too_large` | CALL payload exceeds station policy. | Caller fragments or refuses. |
| 0x0E | `signature_invalid` | Signature on record/CALL envelope fails verification. | Drop; security alert. |
| 0x0F | `unknown_error` | Unclassified. | Caller logs; retry with caution. |

Error codes are **signed by the station returning them** so downstream hops cannot forge "not my fault".

### 6.2 State-machine-based timeouts

Every CALL is a gen_statem whose states include:

```
idle → resolving → selected_target → connecting → awaiting_ack →
      ┌── succeeded
      └── failed(code, offending_hop_signature)
```

Every transition has a bounded deadline:

| Transition | Deadline |
|-----------|----------|
| idle → resolving | 100 ms (routing-table hit should be microseconds) |
| resolving → selected_target | 500 ms (may include one Kademlia lookup hop) |
| selected_target → connecting | 200 ms (QUIC 0-RTT or 1-RTT) |
| connecting → awaiting_ack | 200 ms (first-byte response from target) |
| awaiting_ack → succeeded / failed | CALL deadline (caller-set, default 5 s) |

Missing any deadline ⇒ immediate `failed(temporary_relay_failure | unknown_next_peer | ...)` with the offending hop's signature if available.

### 6.3 Fast-fail at routing-table granularity

Before sending a CALL, the router checks whether the chosen next-hop is in *alive* status in the SWIM view. If the hop is `suspect` or `confirmed_failed`, the router fails immediately with `unknown_next_peer` — no QUIC attempt, no timeout burn.

This exploits Pillar 3 (SWIM) for Pillar 4 (fast-fail). The two pillars are structurally linked.

### 6.4 Retry budgets

The caller holds a retry budget per CALL. Each retry decrements the budget; exhaustion ⇒ final failure. Defaults:

- Interactive CALL: budget = 2 retries, max latency 5 s.
- Background CALL: budget = 5 retries, max latency 30 s.
- Idempotent bulk CALL: budget = unlimited, exponential backoff 1s → 1h, caller-defined cap.

Retry uses a **different disjoint path** by default (Part 3 source routing computes k=3 paths; path rotation on retry).

### 6.5 V1 violation this pillar addresses

V1 bug #2: broadcast fan-out race where `procedure_not_found` from wrong peer beat correct reply. V1's timeout-based failure gave no attribution. V2's signed structured-code responses let the caller identify which hop reported what, dedupe competing responses by signature, and route retries accordingly.

---

## 7. Pillar 5 — Cascade refresh on reconnect

> **Any reconnect triggers refresh of dependent state automatically.**

### 7.1 What counts as a reconnect

A peer-connection gen_statem transitions to `CONNECTED` from `CONNECTING` (first connection) or from `RECONNECTING` (after disconnect). Every entry into `CONNECTED` triggers the **REFRESH** phase before the connection is marked ready-for-application-traffic.

### 7.2 What gets refreshed

```
REFRESH phase tasks (in parallel):
  1. Re-announce own advertised procedures to peer.
  2. Re-announce own subscriptions to peer.
  3. Push any DHT records this peer is a responsible replica for.
  4. Request peer's current view of our records (delta-sync).
  5. Re-push SWIM alive for us, request SWIM alive for peer's group members.
  6. Re-establish any dist-over-mesh tunnels that were routed through peer.
  7. Realm-directory version check; pull delta if needed.
```

REFRESH has a 30-second deadline. Failure to complete REFRESH = failed reconnect = connection re-entered RECONNECTING. No "partially refreshed" state.

### 7.3 Versioning for delta-sync

All peer-tracked state carries a monotonic version per owner. A station tracks, per peer, "last-known-version-they-acknowledged". On REFRESH, the station pushes deltas since that version; the peer acks.

Versions are per-category (procedures, subscriptions, records) to avoid a global monotonic counter choke. Versions are 64-bit; rollover in foreseeable lifetime is impossible.

### 7.4 Piggyback vs explicit refresh

Some refresh is piggybacked on steady-state traffic (SWIM disseminates membership diffs; every CALL carries a timestamp that implicitly refreshes peer liveness). Explicit REFRESH covers state that has no other refresh channel.

The rule of thumb: if state has a natural ambient refresh (heartbeat, SWIM, periodic republish), don't add explicit refresh. If it doesn't, add explicit refresh to REFRESH phase.

### 7.5 V1 violation this pillar addresses

V1 bug #5: station reconnected but its subscriptions were gone on peer side; publications silently dropped. In V2, REFRESH is non-skippable; subscriptions re-announce on every reconnect; version compare-and-push detects desync.

---

## 8. Pillar 6 — Idempotent operations, stable call-IDs

> **Repeat registers are no-ops. Retries with same call-id dedupe. Register/unregister commute.**

### 8.1 Caller-assigned call-IDs

Every CALL carries `call_id :: uuid_v7`. UUIDv7 is time-ordered (millisecond timestamp prefix + 10 bits sub-ms + 62 random bits). Properties exploited:

- Time-ordered ⇒ a bloom filter keyed by call_id expires the oldest first.
- Caller-assigned ⇒ every hop sees the same call_id across retries.
- Cryptographically unlinkable across callers ⇒ no collision attack surface.
- 128 bits ⇒ collision-free for any realistic volume.

### 8.2 Dedup at every hop

Each station maintains a **10-minute bloom filter** of observed call_ids. A CALL whose call_id is in the filter is treated as a retry:

- If the call is still in flight ⇒ station waits on the existing handler and returns the same response.
- If the call completed within the bloom window ⇒ station replays the cached response.
- If the call completed before the bloom window ⇒ treated as a fresh call.

Bloom parameters: 10-minute window, false-positive rate ≤0.1%. Memory: ~1 MB per station per active realm at typical volumes.

### 8.3 Registration idempotency

Registration keys are `(type, identity, payload_hash)`. Same triple ⇒ same logical registration. Re-registering with the same triple is a no-op. Different payload_hash with same type+identity ⇒ **update** (treated as atomic replace, with eviction notification if the key is singleton per Pillar 2).

### 8.4 Unregister ordering

Register and unregister may arrive in either order and the final state is deterministic:

- `unregister(K)` before `register(K)` ⇒ the register succeeds; the unregister (having nothing to remove) is a no-op.
- `register(K, V1)` before `register(K, V1)` ⇒ idempotent; one registration.
- `register(K, V1)` before `register(K, V2)` for additive K ⇒ both live.
- `register(K, V1)` before `register(K, V2)` for singleton K ⇒ V2 evicts V1 (Pillar 2).
- `register(K, V)` before `unregister(K)` ⇒ one registration, then removed.

### 8.5 Tombstones

A tombstone is the authoritative removal signal. Properties:

- Signed by the tombstone-issuer (owning NodeId or realm admin).
- Carries a monotonic `replaced_at` timestamp.
- Supersedes any record whose version is `≤ replaced_at`.
- Lives in the DHT for at least `2 × max_record_ttl` before itself being reaped.

A station seeing a tombstone for a record it holds replaces the record with the tombstone; further stores against the old record are rejected.

### 8.6 V1 violation this pillar addresses

V1 bug #4: peer CONNECT `endpoint` was relay-assigned, not caller-assigned; retries looked like fresh calls. V2 uses caller-assigned UUIDv7 call_ids; dedup is trivial at every hop.

---

## 9. Pillar 7 — Graceful degradation tiers

> **Explicit SLA S / 1 / 2 / 3 / 4 with defined behaviour at each tier.**

### 9.1 The tiers (full expansion)

| Tier | Trigger conditions | Expected behaviour | Latency p95 | Success rate | In V2.0? |
|------|--------------------|--------------------|-------------|--------------|----------|
| **S** (Peacetime) | Normal. No country-level outage. Foundation reachable. Bootstrap cascade Tier A fastest. | Full feature set. Cross-realm federation responsive. All pillars fully active. | <50 ms cross-EU | >99.99% | ✅ |
| **1** (Partial) | One country degraded (DoS, fiber cut, BGP hijack). Foundation reachable. T3s in other countries fine. | Realms in affected country fall back to in-country T1/T2. Cross-country routing uses adjacent T3. SWIM declares affected peers suspect quickly. | <150 ms | >99% | ✅ |
| **2** (Multi-region) | 2+ countries simultaneously under attack. Foundation possibly partially unreachable. | Best-effort ordering; clients see longer retries. HyParView reconfigures around failed peers. Realm membership queries favour cached replicas. | <500 ms | >95% | ✅ |
| **3** (Severe) | Only Tier-D (social / blockchain) bootstrap works; most T3/T4 down. Foundation bootstrap list signed months ago. | Stations operate on last-known-good replicas. Record writes queue for eventual publication. Realms operate read-mostly. Signed receipts for CALL requests. | Minutes — hours | Eventually-delivered | Phase 7 hardening |
| **4** (Blackout) | Internet unavailable. Satellite / radio / DTN only. | Store-and-forward via mesh-local delivery (Part 9 future work). CALL becomes message with envelope-level hop. | Days | Eventually-delivered | V3 / out of V2 scope |

### 9.2 How tier is determined

No central declaration. Every station *infers* its own tier from observables:

- Foundation reachable (DoH-PKARR resolvable)? → candidate S.
- SWIM view shows >30% of tier-same peers `confirmed_failed` in the last hour? → candidate 1 or 2.
- Bootstrap Tier A (foundation anchor) timing out repeatedly? → candidate 2 or 3.
- Zero successful CALL in last 30 s? → candidate 3.

The station publishes its current tier in `_macula.health.self_tier`. Realms aggregate this across their member stations to infer realm-level tier.

### 9.3 Behavioural differences

| Behaviour | S | 1 | 2 | 3 | 4 |
|-----------|---|---|---|---|---|
| Heartbeat frequency | 5 s | 5 s | 3 s (aggressive) | 15 s (conserve bandwidth) | 60 s |
| SWIM period | 2 s | 2 s | 1 s | 5 s | 30 s |
| Call retry budget | 2 | 3 | 5 | ∞ (with backoff) | ∞ |
| Record republish | On tRepublish schedule | Same | Same | Best effort; may skip | Queued |
| Source-route path count | k=3 | k=3 | k=5 (more redundancy) | k=3 cached | 1 (any path) |
| Realm-directory freshness target | ≤1 h | ≤1 h | ≤6 h | ≤24 h | best-effort |
| Bootstrap cascade | A (foundation) | A or B | B / C / D | C / D | D only |

### 9.4 Recovery

When conditions improve, station transitions **downward** (lower tier number = better) as observables recover. Hysteresis: must observe improved conditions for 60 s before declaring tier improvement, to prevent flapping.

Transition **upward** (worse tier) is immediate on detected condition.

### 9.5 V1 context

V1 hit an unintended Tier 2 during 2026-04-13 debug: multiple failures cascaded to total unreachability because no tier-specific backoff existed. V2 targets **Tier 1 SLA under the same conditions** — not perfect, but structured, observable, and self-recovering.

---

## 10. Connection lifecycle state machine

All station-to-station connections follow this state machine.

```
        (first-run)
            │
            ▼
       ┌─────────┐                     ┌─────────────┐
       │  IDLE   │  ─── trigger ────→  │ CONNECTING  │
       └─────────┘                     └──────┬──────┘
            ▲                                 │ QUIC handshake
            │                                 │ TLS 1.3
            │                                 │ peer ident bind
            │                                 │
            │                                 ▼
            │                          ┌─────────────┐
            │                          │ HANDSHAKING │ ─── fail ──┐
            │                          └──────┬──────┘            │
            │                                 │ protocol hello    │
            │                                 │ node_record excg  │
            │                                 ▼                   │
            │                          ┌─────────────┐            │
            │                          │   REFRESH   │            │
            │                          └──────┬──────┘            │
            │                                 │ Pillar 5 tasks    │
            │                                 │ complete          │
            │                                 ▼                   │
            │                          ┌─────────────┐            │
            │                          │  CONNECTED  │            │
            │                          └──────┬──────┘            │
            │                                 │                   │
            │                          ┌──────┴──────┐            │
            │                          │             │            │
            │                          ▼             ▼            │
            │                    ┌──────────┐ ┌────────────┐      │
            │                    │ DRAINING │ │ DISCONNECT │ ◀────┘
            │                    └────┬─────┘ └─────┬──────┘
            │                         │             │
            │                         ▼             ▼
            │                                 ┌─────────────┐
            │                                 │ RECONNECTING│ ── timeout ──┐
            │                                 └──────┬──────┘              │
            │                                        │                     │
            └────── exponential backoff cap ◀────────┘                     │
                                                                           ▼
                                                                        GIVE_UP
```

### 10.1 State semantics

| State | Meaning | Timeout |
|-------|---------|---------|
| IDLE | No attempt yet. | n/a |
| CONNECTING | QUIC handshake in progress. | 5 s |
| HANDSHAKING | QUIC up; Macula protocol hello + node_record exchange. | 3 s |
| REFRESH | Pillar 5 tasks executing. | 30 s |
| CONNECTED | Ready for application traffic. | indefinite while healthy |
| DRAINING | Own-initiated graceful close; don't accept new streams, finish existing. | 5 s drain |
| DISCONNECT | Abrupt close; peer observed dead. | n/a |
| RECONNECTING | Backoff before retry. | Exponential 1 s → 60 s cap |
| GIVE_UP | After retry budget exhausted; caller must decide to re-start. | n/a |

### 10.2 Backoff

RECONNECTING uses exponential backoff with jitter: `min(60s, base * 2^n) * (1 ± 0.3)`. Maximum 10 attempts by default, configurable. Exceeding max ⇒ GIVE_UP.

GIVE_UP is a terminal state until an external trigger (operator action, SWIM revivification) reopens the path.

---

## 11. Record lifecycle (DHT records)

### 11.1 The tReplicate / tRepublish / tExpire model (Kademlia)

| Phase | Interval | Actor | Rationale |
|-------|----------|-------|-----------|
| Publish | at birth | owner | Record stored at k closest NodeIds |
| tReplicate | 1 hour | custodian | Each of the k stations proactively re-stores to its k-closest neighbours to account for churn |
| tRepublish | 24 hours | owner | Owner re-publishes its own record to refresh ownership; prevents tombstone-by-neglect |
| tExpire | 48 hours | custodian | After this, custodian drops the record unless refreshed |

V1's bug #1 was missing tRepublish. V2 makes it a first-class owner obligation; missing the deadline = Pillar 4 fast-fail for the owning process, which may elect to shutdown.

### 11.2 Record version and supersession

Every record carries a `version :: uuid_v7` assigned at birth. Re-publishes increment (new UUIDv7, strictly later). Peers always accept the highest-version record for a given `(type, identity)` pair.

Tombstones carry a version ≥ the record they supersede. Tombstones themselves expire after `2 × tExpire` of the latest version they superseded.

### 11.3 Replica custody

When a station becomes one of the k closest to a key, it takes custody. Custody obligations:

- Honour lookup requests for this record.
- Participate in tReplicate proactively.
- Publish a tombstone if the owner issues revocation.
- Surrender custody cleanly when a closer station joins and replaces it.

Custody is **not** singleton; many peers legitimately hold the same record. Pillar 2 does not apply here.

### 11.4 Orphaned records

A record whose owner hasn't republished within tExpire is orphaned. Custodians proactively tombstone orphans if they can verify the owner is unreachable (SWIM says `confirmed_failed` for >tExpire). Failing that, the record simply drops at tExpire without tombstone — next lookup returns `not_found`.

---

## 12. Station lifecycle

### 12.1 Boot

```
1. Load encrypted StationId from disk (TPM-sealed or passphrase-derived).
2. Probe IPv6 reachability; derive ASN; assemble candidate node_record.
3. Resolve bootstrap cascade (Part 5) — Tier A → B → C → D.
4. Establish first k peer connections via S/Kademlia FIND_NODE of self.
5. Join SWIM group(s) — tier-same country, tier-adjacent bridging.
6. Publish node_record to DHT; publish gateway_capability if applicable.
7. Declare tier; expose /status endpoint; ready for Hecate app mounts.
```

Cold boot SLA: <60 s to "publishing records" under Tier S conditions.

### 12.2 Steady state

Obligations continuously running:
- Heartbeat to every owned registration.
- SWIM probe schedule.
- tReplicate for held replicas.
- tRepublish for owned records.
- REFRESH on every reconnect.
- Tier self-assessment every 30 s.
- Observability publish (bandwidth, uptime, diversity) every 5 min.

### 12.3 Graceful shutdown

Operator requests stop. Station:

1. Publishes `station_draining` to same-realm SWIM group.
2. Refuses new inbound CALLs (returns `temporary_relay_failure` with `draining=true` flag).
3. Drains existing CALLs (5 s budget).
4. Transfers replica custody for records it holds — pushes each replica to its k-closest neighbours proactively.
5. Publishes signed tombstones for ephemeral registrations.
6. Disconnects QUIC peers gracefully (DRAINING → DISCONNECT).
7. Exits.

Stop SLA: <10 s from request to process exit.

### 12.4 Abrupt shutdown

Power loss, kernel panic, SIGKILL. Peers detect via SWIM within 2–6 rounds (4–12 s). Replicas the station held are re-replicated by surviving custodians on next tReplicate cycle. No data loss provided diversity constraints (Part 2 §6.3) held.

### 12.5 Permanent retirement

Operator decommissions station.

1. Publishes signed `station_retired` record (tombstone over own node_record).
2. Realms drop the station from directory.
3. Peers drop routing-table entries on receiving tombstone.
4. After `2 × tExpire`, tombstone itself reaped.

StationId is not reused. Retirement = identity death.

---

## 13. Observability obligations

Lifecycle without observability is fiction. Every V2 station continuously emits:

| Metric | Granularity | Consumer |
|--------|-------------|---------|
| SWIM view digest | per 30 s | foundation monitoring, local `/status` |
| Routing-table size + diversity score | per 30 s | local `/status`, realm admin |
| DHT record count (own / custody) + tRepublish lag | per 1 min | local `/status` |
| Call success rate, error-code histogram | per 1 min | local `/status`, realm admin |
| Connection state-machine transitions | per event | local `/status` (last 1000 events) |
| Self-declared tier transitions | per event | foundation monitoring, realm admin |
| Bandwidth used (rx/tx, per peer) | rolling 5-min | local `/status`, gateway-capability verification |
| Heartbeat miss events | per event | local `/status` |

`/status` is an unauthenticated HTTP endpoint on loopback + signed-auth externally. Data shape specified in Part 6 (wire protocol).

Foundation monitoring is **opt-in**. Stations that decline publish still hold the data locally for the operator; foundation just can't aggregate across the fleet.

---

## 14. Integration map: how the pillars interact

```
                Pillar 1                          Pillar 2
         Process-resource binding ───────── Single-provider invariant
                  │                                    │
                  │                                    │
                  ▼                                    ▼
                Pillar 3  ◀── heartbeat ────▶  Pillar 6
           Liveness probes                Idempotent ops
                (SWIM)                    (UUIDv7 call_ids)
                  │                               │
                  │                               │
                  ▼                               ▼
                Pillar 4  ◀── fast structured ───┘
             Fast-fail                  errors
                  │
                  │
                  ▼
                Pillar 5
         Cascade refresh on reconnect
                  │
                  │
                  ▼
                Pillar 7
         Graceful degradation tiers
```

- Pillar 1 + 2 govern **who owns what**.
- Pillar 3 + 6 govern **how we know it's alive and how we detect retries**.
- Pillar 4 + 5 govern **how failures surface and recovery happens**.
- Pillar 7 is the umbrella behaviour under stress — all other pillars still operate at tier-appropriate intensity.

Violating one pillar weakens the pillars downstream of it. Pillar 1 violation (orphaned state) ⇒ Pillar 3 heartbeat scans become unreliable ⇒ Pillar 4 fast-fail returns misleading codes ⇒ Pillar 7 tier-assessment sees phantom failures.

---

## 15. Open questions specific to Part 4

- **O13 (new)** — SWIM group partition threshold. Current: 256. Revisit after Phase 2 chaos testing on a 1 000-station simulated fleet.
- **O14 (new)** — Heartbeat period under Tier 3. Current proposal: 15 s. May need 60 s+ if T3 bandwidth is actually starved.
- **O15 (new)** — Tombstone retention. Current: 2 × tExpire = 96 h. Long-running adversary could mine stale tombstones; consider 7-day floor.
- **O16 (new)** — Lifeguard "self-awareness" sensitivity tuning on residential hardware. Default Lifeguard parameters were tuned for HashiCorp consumer clusters; may need adjustment for 2 GB RPis under concurrent app load.

References to Part 4 from ROOT §10:
- O1 (crypto puzzle difficulty) is Part 3/5-relevant, not Part 4.
- No Part 4 blocker sits in the ROOT §10 list.

---

## 16. Success criteria for Part 4

Part 4 is complete when a reader can:

1. State each of the 7 pillars in their own words and give the concrete V2 mechanism enforcing each (§3–§9).
2. Trace **one routable state** (e.g. a procedure advertisement) through its full lifecycle: birth → heartbeat → reconnect-refresh → death → tombstone (§3, §5, §7, §11).
3. Pick an arbitrary CALL failure and identify the appropriate BOLT#4-style error code + retry policy (§6.1).
4. Describe the **connection state machine** including which state runs REFRESH (§10).
5. Describe SWIM-Lifeguard parameter choices and the rationale for each (§5.2).
6. Predict a station's observable behaviour when transitioning from Tier S → Tier 2 (§9.3).
7. Name three V1 bugs and map each to the pillar whose absence let the bug happen (§3.6, §4.4, §5.6, §6.5, §7.5, §8.6).
8. Explain why tRepublish is the owner's obligation but tReplicate is the custodian's (§11.1).

If any of the above is ambiguous, Part 4 revises before Part 3 builds on top.

---

*Part 4 closes the lifecycle chapter. Part 3 — Discovery & Routing — now has the invariants it needs to describe how records are found, how paths are computed, and how replicas are placed.*
