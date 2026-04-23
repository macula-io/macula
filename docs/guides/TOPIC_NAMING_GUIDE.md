# Macula Mesh — Topic Naming Convention

**Status:** Normative (enforced by `macula_topic` module + `macula_mesh_client` validation)

---

## Structure

Every mesh topic (pub/sub) and RPC procedure name follows a single 5-segment format:

```
{realm}/{publisher}/{publisher}/{domain}/{name}_v{N}
```

The two `{publisher}` slots carry different values depending on the **ownership tier** of the topic. Three tiers exist; one builder per tier.

| Segment | Source | Example |
|---------|--------|---------|
| `realm` | Runtime config (which mesh) | `io.macula` |
| `publisher` × 2 | Tier-dependent (see below) | `beam-campus` + `hecate`, or `_realm`+`_realm`, etc. |
| `domain` | Bounded context | `mpong`, `licenses`, `membership` |
| `name_vN` | Topic name + version suffix | `lobby_opened_v1`, `chat_to_model_v1` |

System topics (any leading-underscore prefix — `_mesh.*`, `_dist.*`, `_dht.*`) are exempt from this structure. They are dot-separated, infrastructure-owned, and out of scope for application code.

---

## Three ownership tiers

A topic's tier answers one question: **who owns its schema and authority?**

| Tier | Builder | Topic shape | Owner |
|---|---|---|---|
| **realm** | `realm_fact/4`, `realm_hope/4` | `{realm}/_realm/_realm/{domain}/{name}_v{N}` | The realm authority — concepts shared by every member regardless of org/app (membership, identity, ban) |
| **org** | `org_fact/5`, `org_hope/5` | `{realm}/{org}/_org/{domain}/{name}_v{N}` | An org — concepts shared by multiple apps within one org (licensing, billing, org-wide settings) |
| **app** | `app_fact/6`, `app_hope/6` | `{realm}/{org}/{app}/{domain}/{name}_v{N}` | A specific app — anything app-internal (game state, RPCs, app-specific events) |

Each builder takes the realm name as its first argument; org-tier additionally takes the org; app-tier takes both org and app. The underlying `build/6` always emits 5 tokens. Sentinels (`_realm`, `_org`) fill elided publisher slots so the parser never branches on length.

**Per-app convenience wrappers** (e.g. `hecate_topics`) typically pre-fill realm + org + app constants and expose 3-arg variants — see the Hecate practical guide for the wrapper pattern.

### Picking a tier

Walk the decision tree. First "yes" wins.

1. **Could the realm authority publish or revoke this fact at will?** (membership, ban, identity-key revocation) → **realm**
2. **Is the schema something every realm member must trust regardless of which app they run?** (cryptographic identity, capability grants signed by the realm) → **realm**
3. **Is this a concept owned by one org that crosses multiple apps within that org?** (commercial licensing, org-wide billing, shared catalog) → **org**
4. **Is this internal to one app — game state, RPC procedures it advertises, app-specific events?** → **app**

When unsure → start at **app** and promote later. Demoting is harder because subscribers exist.

### The reserved sentinels: `_realm` and `_org`

Two literal tokens are reserved and may not be used as real org or app names:

- `_realm` — fills the `org` and `app` slots when the publisher tier is realm
- `_org` — fills the `app` slot when the publisher tier is org

The leading underscore is the marker. Real org and app names must satisfy DNS-label rules (start with `[a-z0-9]`), so they cannot collide with sentinels.

There is no `_app` sentinel. App-tier topics fill all 5 slots with real values; nothing is elided.

---

## Hopes and Facts — verb tense determines intent

The name segment reveals intent through verb tense:

| Type | Verb Tense | Wire Format | Meaning |
|------|-----------|-------------|---------|
| **Fact** | Past tense | `{subject}_{past_verb}_v{N}` | Something happened |
| **Hope** | Present tense | `{verb}_{subject}_v{N}` | We want something to happen |

### Facts (pub/sub — things that happened)

```
%% realm tier
io.macula/_realm/_realm/membership/revoked_v1
io.macula/_realm/_realm/identity/public_key_announced_v1

%% org tier
io.macula/beam-campus/_org/licenses/issued_batch_v1
io.macula/beam-campus/_org/licenses/revoked_v1

%% app tier
io.macula/beam-campus/hecate/mpong/lobby_opened_v1
io.macula/beam-campus/hecate/llm/model_detected_v1
io.macula/acme-org/trader/portfolio/position_closed_v1
```

Published via `macula:publish/3`. Subscribed via `macula:subscribe/3`.

### Hopes (RPC — things we want to happen)

```
%% realm tier
io.macula/_realm/_realm/auth/check_health_v1
io.macula/_realm/_realm/auth/verify_api_key_v1

%% org tier (rare)
io.macula/beam-campus/_org/billing/get_quota_v1

%% app tier
io.macula/beam-campus/hecate/llm/chat_to_model_v1
io.macula/beam-campus/hecate/mpong/join_game_v1
io.macula/acme-org/trader/portfolio/open_position_v1
```

Advertised via `macula:advertise/3`. Called via `macula:call/4`.

---

## Rules

1. **Exactly 5 `/`-separated segments** — never fewer, never more, never dots.
2. **Segments are snake_case** — lowercase, underscores. Hyphens allowed in org/app names per DNS-label rules.
3. **Name MUST end with `_v{N}`** — versioned from day one. Bump on breaking changes.
4. **Past tense = fact, present tense = hope** — the tense IS the type.
5. **IDs in payload, NEVER in topic** — prevents topic explosion (1M sensors = 1 topic, not 1M).
6. **No CRUD verbs** — `created`, `updated`, `deleted` are forbidden. Use business verbs.
7. **Realm is runtime** — comes from mesh connection config, never hardcoded in source.
8. **`{org}/{app}` is build-time** — comes from the application's manifest.
9. **No inline topic strings** — always call a builder. CI greps for and rejects inline construction.
10. **Tier matches authority, not publisher** — a daemon publishing a realm-owned fact still uses `realm_fact`.

---

## Wildcard Subscriptions

The hierarchical structure enables wildcards:

```
io.macula/#                                     — everything on realm
io.macula/_realm/#                              — every realm-tier topic
io.macula/beam-campus/#                         — everything from one org
io.macula/beam-campus/hecate/#                  — all hecate topics
io.macula/beam-campus/hecate/mpong/#            — all mpong topics
io.macula/beam-campus/*/llm/#                   — all llm topics from any beam-campus app
```

---

## API

The `macula_topic` module exposes one builder per tier, plus the parser:

```erlang
%% Realm tier — _realm fills both publisher slots
macula_topic:realm_fact(<<"io.macula">>, <<"membership">>, <<"revoked">>, 1).
%% → <<"io.macula/_realm/_realm/membership/revoked_v1">>

macula_topic:realm_hope(<<"io.macula">>, <<"auth">>, <<"check_health">>, 1).
%% → <<"io.macula/_realm/_realm/auth/check_health_v1">>

%% Org tier — _org fills the app slot
macula_topic:org_fact(<<"io.macula">>, <<"beam-campus">>,
                      <<"licenses">>, <<"issued_batch">>, 1).
%% → <<"io.macula/beam-campus/_org/licenses/issued_batch_v1">>

%% App tier — fully qualified
macula_topic:app_fact(<<"io.macula">>, <<"beam-campus">>, <<"hecate">>,
                      <<"mpong">>, <<"lobby_opened">>, 1).
%% → <<"io.macula/beam-campus/hecate/mpong/lobby_opened_v1">>

macula_topic:app_hope(<<"io.macula">>, <<"beam-campus">>, <<"hecate">>,
                      <<"llm">>, <<"chat_to_model">>, 1).
%% → <<"io.macula/beam-campus/hecate/llm/chat_to_model_v1">>

%% Parse a topic — returns the inferred tier
{ok, #{tier := realm,
       realm := <<"io.macula">>,
       domain := <<"membership">>,
       name := <<"revoked">>,
       version := 1}} =
    macula_topic:parse(<<"io.macula/_realm/_realm/membership/revoked_v1">>).

{ok, #{tier := app,
       realm := <<"io.macula">>,
       org := <<"beam-campus">>,
       app := <<"hecate">>,
       domain := <<"mpong">>,
       name := <<"lobby_opened">>,
       version := 1}} =
    macula_topic:parse(<<"io.macula/beam-campus/hecate/mpong/lobby_opened_v1">>).

%% Validate a topic
ok = macula_topic:validate(<<"io.macula/beam-campus/hecate/mpong/lobby_opened_v1">>).
{error, _} = macula_topic:validate(<<"io.macula.mpong.lobby_opened">>).
```

### Calling from Elixir (no wrapper)

Per the macula-io coding rule "no Elixir wrappers for Erlang", Elixir consumers (e.g. `macula-realm`) call `macula_topic` directly via the Erlang interop:

```elixir
topic = :macula_topic.realm_fact("io.macula", "membership", "revoked", 1)
# => "io.macula/_realm/_realm/membership/revoked_v1"

procedure = :macula_topic.realm_hope("io.macula", "auth", "check_health", 1)
# => "io.macula/_realm/_realm/auth/check_health_v1"
```

Do NOT write `MaculaRealm.Topics` or similar wrapper modules. Call the Erlang builder directly. The realm name should come from runtime config, not hardcoded in source.

---

## Validation enforced at publish/subscribe

`macula_mesh_client:publish/3`, `subscribe/3`, `advertise/3`, and `call/4` validate the topic against `macula_topic:validate/1` and reject any non-canonical form. The only exception is the `_mesh.*` system topic prefix used by infrastructure.

This means inline string construction will **fail at runtime**, not silently publish to a dead topic. The previous failure mode — where one side built `io.macula.membership.revoked` (4 dot-separated tokens) and the other built `io.macula/beam-campus/hecate/membership/revoked_v1` (5 slash-separated tokens) and they never matched — is no longer possible.

System topics (any leading-underscore prefix) bypass canonical validation: `_mesh.node.up`, `_dist.tunnel.X`, `_dht.list_gateways` etc. all pass through.

---

## Anti-Patterns

| Wrong | Why | Correct |
|-------|-----|---------|
| `io.macula.hecate.mpong.lobby_opened` | Dots, no version, no structure | `io.macula/beam-campus/hecate/mpong/lobby_opened_v1` |
| `"#{realm}.membership.revoked"` (Elixir interpolation) | Inline, dot-form, no tier, no version | `:macula_topic.realm_fact(realm, "membership", "revoked", 1)` |
| `<<Realm/binary, "/foo/bar/baz_v1">>` (Erlang interpolation) | Inline, no validator, no tier | `macula_topic:app_fact(Realm, Org, App, "foo", "bar", 1)` |
| `app_fact("membership", "revoked", 1)` for a realm-owned event | Wrong tier — realm authority owns this | `realm_fact("membership", "revoked", 1)` |
| `io.macula/beam-campus/hecate/mpong/game.available_v1` | "available" is neither past nor present tense | Pick a tense. Past for fact, present for hope. |
| `io.macula/beam-campus/hecate/mpong/join.{game_id}_v1` | ID in topic | Put `game_id` in payload |
| `io.macula/_realm/hecate/membership/revoked_v1` | Mismatched tier sentinels — `_realm` requires `_realm` in both publisher slots | Either fully realm (`_realm/_realm`) or fully app |
| `mpong.games.created_v1` | CRUD verb | `mpong/lobby_opened_v1` |

---

## Related guides

- `docs/guides/PUBSUB_GUIDE.md` — pub/sub usage, payload conventions
- `docs/guides/RPC_GUIDE.md` — RPC usage, error handling
- `hecate-social/hecate-agents/skills/MESH_TOPIC_TIERING.md` — Hecate-specific guidance, audit history, anti-patterns from real bugs
