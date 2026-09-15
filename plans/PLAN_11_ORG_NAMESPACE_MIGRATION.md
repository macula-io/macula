# 11.0.0 org and node namespace migration

This exists so every procedure a caller reaches in 11.0.0 is served only by a provider its org or its own node id
authorizes.

Status: the flip is held. It refuses a procedure without a procedure namespace as `procedure_namespace_required`, in
callers' `verify_authorization/3` and in the SDKs and CLI when the procedure is advertised. It lands together with the
migration below, gated by the full suite green with the migrated services (Jupiter's ruling, 2026-09-15). Owners mark
core cutover, yes or no, per entry.

## The rule

A procedure has exactly one of two authorized forms, both checked from the advertisement alone.
`macula_record:procedure_namespace/1`, replacing `procedure_org/1`, reads which one a name has from the text before its
first `/`. It is one predicate with no application names in it, and one table of vectors serves the Erlang and Go
stacks. Syntax and refusal names agreed by Neptune and Venus, 2026-09-15.

- **Node namespace**, `@<node_id>/Name`: `@` and exactly 64 lowercase hex characters, the 32-byte node id. Authorized
  when those bytes equal the node id verify bound to the signature: the record's `key_id`, which equals its
  `advertiser_node`. No org and no delegation. Anything else starting with `@` is malformed, so a marked prefix
  is never read as an org and a node has one spelling. `verify_authorization/3` takes only a record verify returned,
  and refuses one without a `key_id`.
- **Org namespace**, `Org/Name`: any other text before the first `/`. Authorized by the realm-signed org directory
  naming the org and the org-signed procedure delegation naming the advertiser node (D25).
- A name with no `/`, or with `_` before it, has no namespace. An empty text before it is malformed.
- An org directory refuses an org name no procedure can name as an org: empty, `_`, containing `/`, or starting with
  `@`. The two kinds are disjoint by construction.
- A caller refuses a malformed name as `malformed`, then a name with no namespace as `procedure_namespace_required`,
  whatever it carries. A node-namespaced advertisement carrying any authorization is refused as
  `authorization_not_allowed`, and one naming a node other than the advertiser as `node_namespace_mismatch`. An
  org-namespaced one is refused as `no_authorization`, `authorization_form_unsupported`, a malformed pair, then the
  delegation refusals.

## A. Outside the rule

- Served by a linked station, never advertised: `_dht.*`, `_content.*`, `_relay.ping`, `_macula.ping`.
  - A macula-station test shows no station puts an advertisement for them, or sends an ADVERTISE for them.
  - A macula source check shows nothing in `src/` publishes one.

## B. Node-namespaced procedures

| Procedure | Change | Owner | Core cutover |
|---|---|---|---|
| `agent.<node_id>.ring` (macula-mcp, advertised direct) | becomes `@<node_id>/agent.ring` | Venus | to confirm |
| `_dist.tunnel.<node>` (macula dist pool) | takes the node namespace once its node name resolves to a node id through a trusted binding | Pluto | to confirm |

## C. Services

Each service configures its org, advertises under `Org/Name` with a delegation, and drops the bare advertise. Owner:
Saturnus, end to end: the hecate_om slice first, then each service.

Through hecate_om, whose org defaults to `_` until a service sets one:

| Service | Procedures | Org | Privilege split | Core cutover |
|---|---|---|---|---|
| hecate-stations | `hecate_stations.list_stations` | to confirm | to confirm | to confirm |
| hecate-citizens | `hecate_citizens.register_presence`, `get_citizen`, `list_citizens` | to confirm | to confirm | to confirm |
| hecate-agora | `hecate_agora.get_posts_page`, `get_thread_by_post_id`, `search_posts`, `search_archive` | to confirm | to confirm | to confirm |
| hecate-tube | `tube.lookup_channel`, `lookup_content`, `lookup_video_clip`, `watch_video_clip` | to confirm | to confirm | to confirm |
| hecate-rag | `hecate-rag.*` (16 to 18 names) | to confirm | to confirm | to confirm |
| hecate-graph | `hecate_graph.learn_link`, `resolve_entity`, `resolve_link`, `narrate_entity`, `narrate_link` | to confirm | to confirm | to confirm |
| hecate-mail | `hecate_mail.initiate_mailbox`, `open_mailbox`, `get_mailbox`, `deposit_letter`, `get_letter`, `reply_to_letter`, `archive_letter` | to confirm | to confirm | to confirm |
| hecate-mods | `hecate_mods.invite_agent_to_room`, `moderate_room` | to confirm | to confirm | to confirm |
| hecate-echo | `io.macula.echo` | to confirm | to confirm | to confirm |
| hecate-search | `hecate_search.web_search` | to confirm | to confirm | to confirm |
| hecate-turn-credentials | `hecate_turn_credentials.mint_credential` | to confirm | to confirm | to confirm |
| hecate-mpong-bot | `hecate-mpong-bot.fill_seat`, `host_game`, `list_active_games`, `withdraw` | to confirm | to confirm | to confirm |
| archive, grid, news, rumble, sentinel, warden | `archive.collect_observations`, `report_gaps`; `grid.observe_datasets`; `news.report_item`; `rumble.settle_visit`; `sentinel.alert_society`, `correlate_threats`; `warden.ensnare`, `report_threat` | to confirm | to confirm | to confirm |

With their own advertise, each moving to `advertise_direct` under its org with delegations:

| Service | Procedures | Org | Privilege split | Core cutover |
|---|---|---|---|---|
| hecate-dns | `hecate-dns.*` (10 names) | to confirm | to confirm | to confirm |
| hecate-git | `hecate-git.*` (10 names) | to confirm | to confirm | to confirm |
| hecate-llm | `hecate-llm.chat`, `stream_chat`, `list_available`, `check_health`, `report_status`, `track_usage` | to confirm | to confirm | to confirm |
| hecate-nvidia-pair | `hecate-nvidia-pair.chat` | to confirm | to confirm | to confirm |
| hecate-embedder | `io.hecate.embed` (configurable) | to confirm | to confirm | to confirm |
| hecate-dronex | `dronex.raid.<island>` | to confirm | to confirm | to confirm |
| macula-rag | `macula_rag.query` | to confirm | to confirm | to confirm |

Diagnostics in hecate-stub (`_diag.probe.*`, `diag.*`) are renamed or kept out of production builds.

## D. Realm-name orgs

Owner: Saturnus. Confirmed by Jupiter, 2026-09-15.

- macula-realm (10 names), macula-portal (8), tom-ocean (3) and tom-world (8) advertise names that start with the realm
  name, so `procedure_namespace/1` reads the realm name as the org (`io.macula`, and `io.macula/<org>/_org/...`
  too).
- `io.macula`'s delegation lists only the realm node, since a delegation names no procedure scope. The realm registers
  its own name as an org and issues it through `IssueProviderAuthorizations`. The reissuer refuses a round in which the
  realm's own org lists any node other than the realm's.
- macula-portal, tom-ocean and tom-world advertise under their own orgs with their own org keys. tom-ocean and
  tom-world build their names through `macula_mri:derive_procedure/2` as `<realm>/<path>.<name>`, so their naming
  changes with their migration.

| Binary | Names | Org | Core cutover |
|---|---|---|---|
| macula-realm | `io.macula/_realm/...` | `io.macula`, realm node only | to confirm |
| macula-portal | realm-prefixed | its own org | to confirm |
| tom-ocean | `<realm>/<path>.<name>` | its own org | to confirm |
| tom-world | `<realm>/<path>.<name>` | its own org | to confirm |

## Callers that switch

- macula-mcp: `mesh_list_stations`, citizenship's `hecate_citizens.register_presence`, mesh_memory's
  `hecate-rag.answer_query`, `add_knowledge` and `upload_knowledge`, and the ring to `@<node_id>/agent.ring`.
- macula: the pool's station discovery (`hecate_stations.list_stations`).
- hecate-rag: `io.hecate.embed`. hecate-spartan: citizens, rag, graph and embed calls.
- macula-portal: `tube.*` and `hecate_agora.*`.
- macula-lazymesh: the 16 names in `internal/meshservices/catalog.go`.
- SDK quickstarts and examples: `io.macula.echo` and the un-namespaced example names in macula-go, macula-ts,
  macula-rust, macula-dotnet and macula-py.

## E. Refused at advertise time

- macula: `procedure_advertisement/5` and the `advertise_direct` paths refuse, by the caller's refusal names, a
  procedure a caller would refuse, before anything is built, signed or published. The paths are `macula_response`,
  `macula_streamer` (which `macula_upload` also takes) and `macula_direct_dial:publish_advertisement`. The builder
  raises `{Refusal, Procedure}` (Neptune, the flip pair).
- macula-go and macula-cli `serve` and `stream` refuse it the same way, before a handler is registered or anything is
  sent (Venus). The other SDKs follow in their ports.

## Owners

- The flip pair, both forms in `procedure_org/1`, and macula's advertise-time refusal: Neptune.
- The namespace vectors: the table lives in the flip pair, and Go's test cites its sha (Neptune and Venus).
- Services (C) and realm-name orgs (D): Saturnus.
- macula-go, macula-cli and the ring's move in macula-mcp: Venus.
- The station test for A: Mars.
- The trusted node-name binding for the dist tunnel: Pluto.
