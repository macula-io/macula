# Macula SDK — Backlog

Tracked work items not yet scheduled. Newest first.

---

## Pub/Sub delivery leaks `{:text, _}` for CBOR text strings

**Filed:** 2026-06-01
**Severity:** medium (correctness / consumer ergonomics; no data loss)

Delivered pub/sub fact payloads surface CBOR **text strings (major 3)** as
`{text, binary()}` tuples (Elixir `{:text, bin}`) — in map KEYS and VALUES, at
any depth — instead of plain binaries. CBOR **byte strings (major 2)** correctly
decode to plain binaries, so the leak only bites publishers that emit text
strings (e.g. `hecate-parksim` → the `macula-realm` ClankerCab demo).

**Impact:** every consumer of a text-string fact must defensively unwrap
`{:text, _}` or it crashes the moment a key/value reaches `String.Chars`
interpolation. This already forced two band-aids in `macula-realm`:
- `clanker_cab_subscriber.ex` — recursive `{:text, bin}` → `bin` normalize at
  the subscriber boundary (commit `52bc2d2`).
- `topology/mesh_subscriber.ex` — the same `normalize/1` at `decode_record` +
  `decode_app_payload`, a `{:text, _}` clause in `get_str/2`, and a hardcoded
  `Map.get(payload, {:text, "superseded_key"})` tombstone lookup.

**Proper fix:** pub/sub delivery decode should return CBOR text strings as plain
`binary()` (matching byte-string behaviour), not `{text, _}` tuples. The
`{text, binary()}` representation is `macula_record_cbor`'s internal canonical
form (correct for deterministic sig encoding) — it should not surface in the
app-facing decoded payload. Likely fix point: the SDK delivery/decode path that
hands the fact term to subscribers (where it currently preserves the
`macula_record_cbor` value rep instead of converting major-3 to binary).

**When done:** drop the two normalize band-aids in `macula-realm` (search for
`TODO(macula)` and the ClankerCab `normalize/1`).

**Likely introduced:** a macula version between the realm's prior pinned build
and 4.8.0 (the realm's ClankerCab worked with plain binaries before the bump).

---

## Per-station addressing for pub/sub (subscribe against a chosen station)

**Filed:** 2026-06-02
**Severity:** low (enhancement; unblocks a full god-module retirement downstream)

The pool (`macula_client`) deliberately hides individual `macula_station_link`
workers — `macula:subscribe/4` targets a (realm, topic) across all links, not a
chosen station. But some diagnostics need to act on ONE specific station: e.g.
`macula-realm`'s bloom-convergence demo subscribes a throwaway
`diag.bloom.demo.*` topic against a single station's link to watch the topic
propagate into peers' blooms. Today it reaches into the private
`macula_station_link` via a bespoke per-relay pool
(`MaculaRealm.Topology.MeshSubscriber.client_for_pubkey/1`).

**Ask:** a public per-station handle on the pool, e.g.
`macula:subscribe_on_station(Pool, StationPubkey, Realm, Topic, Subscriber)`
(and/or `macula:station_link(Pool, StationPubkey) -> {ok, pid} | {error, ...}`),
so consumers never reach into `macula_station_link`.

**Why it matters downstream:** with this, `macula-realm` can delete its bespoke
relay-link pool entirely and retire the `MeshSubscriber` god-module — the last
thing keeping that pool alive is exactly this per-station subscribe. Until then,
macula-realm keeps a thin `Topology.StationLinks` module (path B of
`macula-internal/macula-realm/plans/PLAN_DEMOS_VERTICAL_SLICING.md`).
