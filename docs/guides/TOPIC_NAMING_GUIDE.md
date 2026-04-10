# Macula Mesh — Topic Naming Convention

**Status:** Normative (enforced by `macula_topic` module)

---

## Structure

Every mesh topic (pub/sub) and procedure (RPC) follows a single format:

```
{realm}/{org}/{app}/{domain}/{name}_v{N}
```

| Segment | Source | Example |
|---------|--------|---------|
| `realm` | Runtime config (which mesh) | `io.macula` |
| `org` | manifest.json `app_id` (first part) | `beam-campus` |
| `app` | manifest.json `app_id` (second part) | `hecate` |
| `domain` | Bounded context within the app | `mpong` |
| `name_vN` | The topic name with version suffix | `lobby_opened_v1` |

The `app_id` in manifest.json uses `{org}/{app}` format (like GitHub owner/repo).

---

## Hopes and Facts

The name segment reveals intent through verb tense:

| Type | Verb Tense | Wire Format | Meaning |
|------|-----------|-------------|---------|
| **Fact** | Past tense | `{subject}_{past_verb}_v{N}` | Something happened |
| **Hope** | Present tense | `{verb}_{subject}_v{N}` | We want something to happen |

### Facts (pub/sub — things that happened)

```
io.macula/beam-campus/hecate/mpong/lobby_opened_v1
io.macula/beam-campus/hecate/llm/model_detected_v1
io.macula/beam-campus/hecate/weather/wind_measured_v1
io.macula/acme-org/trader/portfolio/position_closed_v1
```

Facts use the same `{subject}_{past_verb}` convention as domain events.
Published via `macula:publish/3`. Subscribed via `macula:subscribe/3`.

### Hopes (RPC — things we want to happen)

```
io.macula/beam-campus/hecate/llm/chat_to_model_v1
io.macula/beam-campus/hecate/llm/list_models_v1
io.macula/beam-campus/hecate/mpong/join_game_v1
io.macula/acme-org/trader/portfolio/open_position_v1
```

Hopes use the same `{verb}_{subject}` convention as commands.
Advertised via `macula:advertise/3`. Called via `macula:call/3`.

---

## System Topics

System topics use underscore prefix and do NOT follow the 5-segment structure:

```
_mesh.node.up
_mesh.node.down
_mesh.relay.ping
```

These are owned by the macula infrastructure, not by applications.

---

## Rules

1. **Exactly 5 `/`-separated segments** (except system topics)
2. **Segments are snake_case** — lowercase, underscores, hyphens
3. **Name MUST end with `_v{N}`** — versioned from day one
4. **Past tense = fact, present tense = hope** — the tense IS the type
5. **IDs in payload, NEVER in topic** — prevents topic explosion
6. **No CRUD verbs** — `created`, `updated`, `deleted` are forbidden
7. **Realm is runtime** — comes from mesh connection config
8. **App ID is build-time** — comes from `manifest.json`

---

## Wildcard Subscriptions

The hierarchical structure enables powerful wildcards:

```
io.macula/#                                    — everything on realm
io.macula/acme-org/#                           — everything from org
io.macula/beam-campus/hecate/#               — all hecate topics
io.macula/beam-campus/hecate/mpong/#         — all mpong topics
io.macula/beam-campus/*/llm/#                — all llm topics from any beam-campus app
```

---

## API

The `macula_topic` module enforces this convention:

```erlang
%% Build a fact topic (pub/sub)
macula_topic:fact(<<"io.macula">>, <<"beam-campus/hecate">>,
                  <<"mpong">>, <<"lobby_opened">>, 1).
%% → <<"io.macula/beam-campus/hecate/mpong/lobby_opened_v1">>

%% Build a hope topic (RPC procedure)
macula_topic:hope(<<"io.macula">>, <<"beam-campus/hecate">>,
                  <<"llm">>, <<"chat_to_model">>, 1).
%% → <<"io.macula/beam-campus/hecate/llm/chat_to_model_v1">>

%% Parse a topic back into parts
{ok, #{realm := <<"io.macula">>,
       org := <<"beam-campus">>,
       app := <<"hecate">>,
       domain := <<"mpong">>,
       name := <<"lobby_opened">>,
       version := 1}} = macula_topic:parse(<<"io.macula/beam-campus/hecate/mpong/lobby_opened_v1">>).

%% Validate a topic
ok = macula_topic:validate(<<"io.macula/beam-campus/hecate/mpong/lobby_opened_v1">>).
{error, _} = macula_topic:validate(<<"bad.topic.no.structure">>).
```

---

## Anti-Patterns

| Wrong | Why | Correct |
|-------|-----|---------|
| `io.macula.hecate.mpong.lobby_opened` | Dots, no version, no structure | `io.macula/beam-campus/hecate/mpong/lobby_opened_v1` |
| `io.macula/hecate/mpong/game.available` | "available" is not past or present tense | `io.macula/beam-campus/hecate/mpong/lobby_opened_v1` |
| `io.macula/hecate/mpong/join.{game_id}` | ID in topic | `io.macula/beam-campus/hecate/mpong/join_game_v1` (game_id in payload) |
| `realm.app.llm.chat.agent.identity` | Agent identity in topic, realm baked in | `io.macula/org/app/llm/chat_to_model_v1` |
| `mpong.games.created` | CRUD verb | `mpong/lobby_opened_v1` |
