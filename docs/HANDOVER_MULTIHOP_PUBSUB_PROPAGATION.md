# HANDOVER — Multi-hop PubSub propagation (self-healing) is broken across the relay mesh

**Status:** OPEN — foundational bug. A subscriber on one relay does **not**
receive facts published by a producer on a **different** relay, and the mesh
does not self-heal subscription routing after producer churn (redeploy/restart).

**Date:** 2026-07-09
**Severity:** HIGH — multi-hop pubsub propagation is a core, foundational macula
capability. The federated mesh's whole value is that a consumer anywhere
reliably receives integration facts published anywhere, and that this
self-heals across node churn. Right now it does not.

**Symptom that surfaced it:** `macula.io/clankercab` (the ClankerCab realm
dashboard) shows no cabs. The parksim edge publishes `fleet/<city>/summary|
telemetry|rides` and `energy/<city>/summary` facts; the realm subscribes to
them; the facts never arrive.

---

## 1. TL;DR for whoever picks this up

1. **A real but SECONDARY bug was found and FIXED this session:** the realm's
   `macula_client` subscriber **pool** goes `healthy=0` (every per-seed link
   connects then closes `:normal`), while raw `macula_station_link`s from the
   same host stay up. ClankerCab was rewritten to subscribe via raw
   station-links (the `RpcAdvertiser` pattern) and deployed. **This did NOT fix
   the dashboard.** (Pool bug tracked separately — see §7.)

2. **The ACTUAL open root cause:** even with healthy station-links on both ends,
   on the **same realm**, a fact published by the edge does not reach the
   realm's subscription. A direct probe (edge `macula:publish` → realm
   `federation/0`) fails every time. This is **multi-hop pubsub propagation** —
   the subscription registered by the consumer at its relay is not reaching /
   not matching the producer's relay, so the producer's relay never forwards the
   fact toward the consumer.

3. **It is NOT global pubsub breakage:** `mpong` pubsub (`mpong/
   state_broadcast_v1.*`) was observed flowing and delivering between these same
   relay boxes during the outage. So the transport + basic pubsub work; the
   failure is specific to the realm↔edge fleet/energy topic subscriptions,
   almost certainly **cross-relay subscription propagation** (and its
   self-healing after the producer churned).

4. **Trigger:** the parksim **edge was wiped + redeployed** earlier today. Its
   stations/sessions restarted. The consumer's subscription state did not
   re-propagate to the new producer topology (no self-heal), and stayed dark.

---

## 2. Topology (you need this to reason about it)

```
 PRODUCER (edge)                 RELAY MESH (stations)              CONSUMER (realm)
 ───────────────                 ─────────────────────              ────────────────
 parksim tenants                 station-be-leuven-<hood>.macula.io  macula.io realm
 beam01/02/03.lab                :4433, many identities per box      (dashboard)
 macula ~4.8.x                   on the relay fleet:                 macula 4.8.0
 realm ABB81B5A                    Hetzner Nuremberg 91.98.238.177   realm ABB81B5A
                                   Hetzner Helsinki  95.216.141.48   (io.macula)
 publishes:                        Linode  Paris     172.239.18.117
   fleet/<city>/summary                                             subscribes to 12
   fleet/<city>/telemetry          heverlee, korbeek-lo -> Paris    leuven-<hood>
   fleet/<city>/rides              centrum, gasthuisberg -> c8b3    stations
   energy/<city>/summary           wilsele/vaartkom/wijgmaal ->     (MACULA_RELAYS)
                                      Nuremberg 1c1f:8ab8
 leuven edge links to:            arenberg -> macula.io host itself
   centrum, gasthuisberg,           (known IPv6-bind-crash/flap)
   heverlee, korbeek-lo
```

**Key fact:** the realm subscribes to AND the edge publishes at the SAME
stations (`centrum, gasthuisberg, heverlee, korbeek-lo` are in both sets). So
there is co-location — a station where both the subscription and the publish are
present — and delivery STILL fails. That means it is not only cross-relay
propagation; the co-located station is also not matching+delivering. Both must
be checked (see §6).

**Realm id:** both edge and realm resolve realm `ABB81B5A614B63551B400B810648C0C8A78EFAD845442630C94B46CC95D2FCD1`
(= `:macula_realm.id("io.macula")`). Confirmed identical. Not a realm mismatch.

---

## 3. What was RULED OUT (do not re-investigate)

- **ClankerCab UI / subscriber code** — rewritten to raw station-links; links
  connect, `subscribe/4` returns without error, `handle_info({:macula_event,…})`
  is correct. Logs show `[ClankerCab.Subscriber] linked <station> pid=…`.
- **The `macula_client` pool** — bypassed entirely now; it was a *separate* real
  bug (§7), not the delivery failure.
- **Realm mismatch** — both sides on `ABB81B5A…`.
- **macula version** — realm on 4.8.0; `mix.lock` unchanged since June; no
  pubsub/link fix exists between 4.8.0 and current `src` (5.0.0). An image
  rollback would not help.
- **Resources** — realm beam 177 MB / 3.8 GB, no `emfile`/`system_limit`.
- **QUIC idle/keepalive** — `idle_timeout=300s, keep_alive=15s` on both the
  station listeners and the SDK dial defaults (`src/peering/macula_quic.erl`
  ~L106-112, L155-156; `do_connect` passes no override so defaults apply).
- **Endpoints / network** — raw `macula_station_link`s from the realm host reach
  the same stations fine (that is what `RpcAdvertiser` uses and RPC kept
  working throughout).
- **Global pubsub** — `mpong` pubsub delivers between these relays (seen live in
  station logs).

---

## 4. Evidence log (the diagnostic trail)

- Realm pool: `:macula.status(pool)` → `healthy=0 failed=12`; links log
  `_macula.client.link_down %{reason: :normal}` cycling all seeds, respawn every
  `?LINK_RESPAWN_DELAY_MS` (1s), never stabilises. **Pool-layer bug (§7).**
- A fresh throwaway pool from the realm to the 3 **backbone relays** also went
  `healthy=0` → not the endpoints, it is the pool layer.
- `RpcAdvertiser` (raw station-links, ephemeral identity) holds links to the
  same stations from the same host → transport + raw links are fine.
- Edge→realm mesh RPC (`macula:call(P,R,<<"io.macula.realm.check_health">>,…)`)
  returned `{error, {call_error, 1, unknown_next_peer}}` while the realm pool was
  dead → no route existed via the (dead) pool.
- After the station-link rewrite deployed: subscriber links up, only ~14
  reconnects / 5 min (normal churn), `subscribe` registered — **and a probe
  publish from the edge still did not arrive** (`leuven_online=false total=nil`).
  This is the crux: healthy links, registered subscription, same realm, no
  delivery.

### The reproducer (use this to test any fix)

```erlang
%% ON THE EDGE (beam01, leuven tenant): publish a probe fact
%% docker exec parksim-leuven /app/bin/hecate_parksim eval "..."
{ok,P}=hecate_om:macula_client(),
{ok,R}=hecate_om_identity:realm(),
macula:publish(P, R, <<"fleet/leuven/summary">>,
               #{type=>fleet_summary, company=><<"leuven">>, total=>555}).
```
```elixir
# ON THE REALM (macula.io): did it arrive?
# docker exec <realm> /app/bin/macula_realm rpc "..."
f = MaculaRealm.Demos.ClankerCab.federation()
l = Enum.find(f.operators, &(&1.id == "leuven"))
IO.puts("online=#{l.online} total=#{inspect(l.overview[:total])}")
# BROKEN: online=false total=nil.  FIXED will show total=555.
```

---

## 5. Why this matters (do not downgrade this)

Multi-hop, self-healing pubsub propagation is **foundational** to macula. The
federated mesh's entire premise is:

- A consumer subscribed on relay A reliably receives integration facts published
  by a producer on relay B, via subscription state that propagates across the
  relay mesh (bloom/plumtree gossip), and
- That routing **self-heals** when producers or relays churn — a producer that
  restarts (redeploy, crash, scale) must be re-reached by existing subscriptions
  within a bounded time, with no manual resubscribe.

This outage is exactly the failure of both properties: a producer churned (the
parksim edge redeploy), and the consumer's subscription never re-propagated to
the new producer topology. It stayed dark until manually poked, and even manual
pokes did not deliver. This is not a dashboard bug; it is a mesh-correctness bug.

---

## 6. Next concrete steps (in order)

1. **Station-side co-location check (fastest, decisive).** On the Paris relay
   (`172.239.18.117`, hosts `heverlee` + `korbeek-lo`, which are in BOTH the edge
   link set and the realm subscribe set), inspect the station serving heverlee:
   - Is the **realm's subscription** for `fleet/leuven/summary` registered on
     that station? (grep the station log / state for the realm's node_id +
     topic.)
   - Does the **edge's publish** arrive at that station? (grep for inbound EVENT
     `fleet/leuven/summary`.)
   - Is it **matched and delivered** to the realm's subscription, or dropped
     (`local_matched=0`, dedup, realm-tag mismatch)?
   This splits the failure into: (a) subscription not registered, (b) publish not
   arriving, (c) matched-but-not-delivered. Each has a different fix.
   Get the realm pool/link node_id first: `:macula.status(pool).self_node_id`
   (Base16); grep the station logs for its first 16 hex.

2. **Cross-relay propagation check.** If co-location works but cross-relay
   fails, the subscription bloom/plumtree gossip between the leuven stations is
   the culprit — the subscription at the consumer's station is not reaching the
   producer's station. Inspect the station↔station subscription propagation
   (bloom filters, `push_on_change`, plumtree eager/lazy peers). Compare to how
   `mpong` (which works) is laid out — likely co-located publisher/subscriber on
   one station, hiding the cross-relay path.

3. **Self-heal-after-churn test.** Once delivery works at all, deliberately
   restart the producer (edge redeploy) and measure how long until the
   consumer's subscription re-reaches it. That must be bounded and automatic. If
   it requires a manual resubscribe, the self-heal path (subscription
   re-propagation on producer HELLO / peer-join) is the gap.

4. **Single-station control.** Point one edge tenant and the realm at the SAME
   single station, verify local (single-hop) delivery works, then add a second
   station and watch the two-hop path. This isolates the propagation layer
   cleanly from everything else.

### Diagnostic toolkit (all confirmed working this session)

| Need | How |
|---|---|
| Realm federation state | `rpc MaculaRealm.Demos.ClankerCab.federation()` |
| Pool/link health | `:macula.status(Pool)` (`healthy_links`, `failed_links`, `self_node_id`, `seeds`), `:macula.links(Pool)` |
| Realm id | `:macula_realm.id("io.macula")` / `MaculaRealm.Mesh.realm()` |
| Edge mesh client | `hecate_om:macula_client()`, `hecate_om_identity:realm()` |
| Edge publish probe | `macula:publish(P,R,Topic,Term)` (term, not JSON) |
| Edge→realm route | `macula:call(P,R,<<"io.macula.realm.check_health">>,#{},8000)` — `unknown_next_peer` = no route |
| Relay access | `ssh root@172.239.18.117` (Paris), `91.98.238.177` (Nuremberg), `95.216.141.48` (Helsinki); `docker logs <station-container>` |
| Realm host | `sshpass -f ~/.config/macula/sshpass.txt ssh root@macula.io`; compose at `/root/macula-realm-compose` |

---

## 7. Secondary open bug: `macula_client` pool goes healthy=0 on the realm

Separate from the propagation bug, and also foundational. The realm's
`macula_client` **subscriber pool** cannot hold links (`healthy=0`, links close
`:normal` ~30s after connect and every 1s respawn dies too), while:
- the **edge's** `macula_client` pool holds its links (same macula 4.8.0), and
- raw `macula_station_link`s from the **same realm host** hold fine.

So it is the pool layer, specific to a **passive multi-relay subscriber** on the
realm. Not endpoints/network/version/resources (all ruled out, §3). Suspects:
the pool passes `capabilities => 0` + a shared pool identity (macula_client.erl
~L378-383) vs `RpcAdvertiser`'s raw `%{seed}` + ephemeral identity; or a
connect-purge / liveness interaction. Needs `macula_client` instrumentation to
log why its `station_link`s exit `:normal`. Full detail in the memory note
`project_realm_macula_pool_healthy_zero`.

**Workaround shipped** (so the dashboard isn't blocked on this): ClankerCab
subscribes via raw station-links, bypassing the pool
(`macula-realm@de89276`).

---

## 8. What was shipped / touched this session

- `macula-realm` `ClankerCab.Subscriber` → raw `macula_station_link`s
  (RpcAdvertiser pattern). Committed `de89276` (GitHub-canonical). Image built
  **locally and pushed** to `codeberg.org/macula-internal/macula-realm:main`
  (digest `sha256:0b328d16…`) because the macula-io GitHub org's **hosted Actions
  runners were unavailable** (jobs queued then failed "not acquired by Runner" —
  looks like an Actions minutes/billing quota; worked at 07:15, dead by 12:23).
  Deployed on macula.io via `docker compose pull realm && up -d realm`.
- `macula-realm` durable periodic re-subscribe in the (now-bypassed) pool
  subscriber, `0ba2cc4` — harmless, kept.
- `macula-demo` `infrastructure/macula.io/.env.example` `MACULA_RELAYS` → backbone
  relays, `6fe3763`. **NOTE:** this was based on the (wrong) hypothesis that the
  edge stations were the problem; the real prod `.env` still points at the 12
  leuven stations and was NOT changed. Re-evaluate this commit once the
  propagation root cause is known — with raw station-links, subscribing directly
  at the edge stations is arguably correct (co-location), so the backbone-relay
  change may be unnecessary or even counterproductive.

## 9. Related prior art (memory + repo)

- Memory: `project_realm_macula_pool_healthy_zero` (the pool bug),
  `project_realm_build_and_flapping`, `project_arenberg_station_bind_crash`,
  and the multi-hop pubsub work: `project_pubsub_bloom_fan_shipped_2026-05-15`
  (Plumtree), `project_pubsub_push_on_change_shipped_2026-05-15`,
  `project_pubsub_phase3_shipped_2026-05-15` (transitive peer-bloom),
  `project_pubsub_resign_loop_lesson`, `project_dht_replicate_wedged_2026-05-15`.
  Those shipped multi-hop propagation; this handover is where it regressed /
  fails to self-heal after producer churn.
- Repo: `docs/guides/PUBSUB_GUIDE.md`, `docs/migrations/V1_TO_V2_PUBSUB.md`,
  `test/macula_pubsub_system/README.md`, and the pubsub source under
  `src/pubsub/` + `src/client/macula_station_link.erl` (subscribe surface,
  `subscribe/4` ~L390, post-HELLO drain ~L417) + `src/client/macula_client.erl`.

## 10. Definition of done

- A fact published by any producer on relay B is delivered to a subscriber on
  relay A, same realm, within a bounded convergence time (the reproducer in §4
  shows `total=555`).
- After the producer restarts (edge redeploy), delivery **self-heals** with no
  manual resubscribe, within a bounded time.
- A regression test in `test/macula_pubsub_system/` covers the cross-relay +
  producer-churn path so this cannot silently regress again.
