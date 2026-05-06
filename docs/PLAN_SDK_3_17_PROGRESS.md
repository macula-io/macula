# PLAN_SDK_3_17 — running progress

Tracks A4 (closed) and the Group B style sweep module-by-module.
See `PLAN_SDK_3_17.md` for the full plan.

## Group A — A4 streaming RPC ✅ CLOSED

| Step | Commit | Subject |
|---|---|---|
| 1 | `a9b16a0` | V2 stream wire frames |
| 2 | `df4c671` | stream_v1 link-carrier peer shape |
| 3 | `f72a547` | per-station streaming RPC |
| 4 | `e66dcb8` | pool-aware streaming RPC |
| 5 | `57b6b85` | streaming RPC integration tests |

## V1 retirement ✅ CLOSED

| Step | Commit (repo) | Subject |
|---|---|---|
| 1 | `dabafa9` (macula) | dist_relay + dist_bridge ported to V2 pool |
| 1.5 | `091a4df` (macula-ecosystem) | mesh_chat presence ported to V2 |
| 2 | `a3b7a5d` (hecate-daemon) | retire macula_multi_relay + macula_mesh_client |
| 3 | `dfd495e` (hecate-app-weather) | retire macula_mesh_client |
| 4 | this commit (macula) | strip V1 facade entries, V1 stream carrier, delete `src/v1/` |

## Group B — SDK style sweep

Style rules (per CLAUDE.md):

- max 1-2 nesting levels
- multi-clause functions > `case` > `if`
- guards > `case` where viable
- no `try..catch` (documented exception: `macula_pubsub:invoke/4`)
- declarative > imperative
- public exports preserved exactly

Cadence: 1-3 modules per commit. Internal helpers may rename if the new
name screams intent better.

### Order (high-traffic first)

| # | Module | State | Commit |
|---|---|---|---|
| 1 | `src/client/macula_client.erl` | pending | |
| 2 | `src/pubsub/macula_pubsub.erl` | pending | |
| 3 | `src/client/macula_station_link.erl` | pending | |
| 4 | `src/client/macula_client_dedup.erl` | pending | |
| 5 | `src/client/macula_client_replay.erl` | pending | |
| 6 | `src/peering/macula_peering_conn.erl` | pending | |
| 7 | `src/peering/macula_peering.erl` | pending | |
| 8 | `src/peering/macula_frame.erl` | pending | |
| 9 | `src/peering/macula_bolt4.erl` | pending | |
| 10 | `src/peering/macula_quic.erl` | pending | |
| 11 | `src/peering/macula_tls.erl` | pending | |
| 12 | `src/peering/macula_source_route.erl` | pending | |
| 13 | `src/peering/macula_protocol_*.erl` (V1 codec — touch lightly) | pending | |
| 14 | `src/record/macula_record.erl` | pending | |
| 15 | `src/record/macula_record_cbor.erl` | pending | |
| 16 | `src/record/macula_record_uuid.erl` | pending | |
| 17 | `src/identity/macula_identity.erl` | pending | |
| 18 | `src/macula_stream.erl` (renamed from `macula_stream_v1`) | pending | |
| 19 | `src/v1/macula_stream_local.erl` | pending | |
| 20 | `src/v1/macula_mesh_client.erl` (V1 — touch lightly) | pending | |
| 21 | `src/v1/macula_multi_relay.erl` (V1 — touch lightly) | pending | |
| 22 | `src/macula.erl` (facade) | pending | |
| 23 | `src/macula_*` substrate (`macula_net*`, `macula_dist_*`, etc.) | pending | |
| 24 | Everything else, alphabetical | pending | |

## Group C — docs / guides / diagrams ⏳

## Group D — test extension ⏳

## Group E — release 3.17.0 ⏳
