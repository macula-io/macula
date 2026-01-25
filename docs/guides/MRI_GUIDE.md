# Macula Resource Identifiers (MRI)

Typed, hierarchical, queryable identifiers for all resources in the Macula ecosystem.

![MRI Architecture](assets/mri-architecture.svg)

## Overview

MRI provides a unified identification scheme that is:

- **Typed** - Resource type embedded in identifier, no external lookup needed
- **Hierarchical** - Natural parent-child relationships via path structure
- **Queryable** - Efficient tree and graph queries
- **Extensible** - New types and relationships without code changes
- **Decentralized** - No central authority required for creation
- **BEAM-native** - Efficient binary representation, pattern matching friendly

## Format

```
mri:{type}:{realm}/{path}
```

| Component | Description | Rules |
|-----------|-------------|-------|
| `mri` | Literal prefix | Always `mri` |
| `type` | Resource type | Lowercase, from registry |
| `realm` | Mesh domain | Reverse-domain notation |
| `path` | Hierarchical path | Slash-separated segments |

### Path Segment Rules

```
segment = 1-63 characters
        = lowercase alphanumeric, hyphen, underscore
        = no leading/trailing hyphen
        = DNS-compatible

path    = segment *("/" segment)
        = max 10 levels deep
```

### Examples

```erlang
%% Realms and organizations
<<"mri:realm:io.macula">>
<<"mri:org:io.macula/acme">>

%% Users and apps
<<"mri:user:io.macula/acme/alice">>
<<"mri:app:io.macula/acme/counter">>
<<"mri:service:io.macula/acme/counter/api">>

%% Physical infrastructure
<<"mri:device:io.macula/citypower/cabinet-ams-42">>
<<"mri:location:io.macula/citypower/nl/amsterdam/centrum">>
<<"mri:cluster:io.macula/citypower/amsterdam-grid">>

%% Artifacts and security
<<"mri:artifact:io.macula/acme/counter-v1.2.0">>
<<"mri:license:io.macula/acme/counter/lic-001">>
<<"mri:cert:io.macula/acme/cert-2026-001">>

%% Messaging
<<"mri:topic:io.macula/acme/events.orders.created">>
<<"mri:proc:io.macula/acme/api.users.get">>

%% Taxonomy
<<"mri:class:io.macula/device/edge-device/street-cabinet">>
```

## Type Registry

### Logical Resources

| Type | Path Schema | Example |
|------|-------------|---------|
| `realm` | (none) | `mri:realm:io.macula` |
| `org` | `{org}` | `mri:org:io.macula/acme` |
| `user` | `{org}/{user}` | `mri:user:io.macula/acme/alice` |
| `app` | `{org}/{app}` | `mri:app:io.macula/acme/counter` |
| `service` | `{org}/{app}/{svc}` | `mri:service:io.macula/acme/counter/api` |

### Artifacts & Security

| Type | Path Schema | Example |
|------|-------------|---------|
| `artifact` | `{org}/{id}` | `mri:artifact:io.macula/acme/counter-v1.2.0` |
| `license` | `{org}/{app}/{id}` | `mri:license:io.macula/acme/counter/lic-001` |
| `cert` | `{org}/{id}` | `mri:cert:io.macula/acme/cert-2026-001` |
| `key` | `{org}/{id}` | `mri:key:io.macula/acme/signing-key-001` |

### Messaging

| Type | Path Schema | Example |
|------|-------------|---------|
| `topic` | `{org}/{path}` | `mri:topic:io.macula/acme/events.orders` |
| `proc` | `{org}/{path}` | `mri:proc:io.macula/acme/api.users.get` |
| `content` | `{mcid}` | `mri:content:io.macula/Qm...abc` |

### Physical Infrastructure

| Type | Path Schema | Example |
|------|-------------|---------|
| `device` | `{org}/{id}` | `mri:device:io.macula/citypower/cabinet-001` |
| `cluster` | `{org}/{name}` | `mri:cluster:io.macula/citypower/amsterdam-grid` |
| `location` | `{org}/{path}` | `mri:location:io.macula/citypower/nl/amsterdam` |
| `zone` | `{org}/{name}` | `mri:zone:io.macula/citypower/production-dmz` |
| `network` | `{org}/{name}` | `mri:network:io.macula/citypower/subnet-10.0.1.0` |

### AI/ML

| Type | Path Schema | Example |
|------|-------------|---------|
| `model` | `{org}/{name}` | `mri:model:io.macula/acme/classifier-v2` |
| `dataset` | `{org}/{name}` | `mri:dataset:io.macula/acme/training-2026-01` |
| `config` | `{org}/{path}` | `mri:config:io.macula/acme/app.settings` |

### Taxonomy

| Type | Path Schema | Example |
|------|-------------|---------|
| `class` | `{path}` | `mri:class:io.macula/edge-device/street-cabinet` |
| `taxonomy` | `{org}/{name}` | `mri:taxonomy:io.macula/acme/device-classification` |

## Core API

### Parsing

```erlang
%% Parse MRI into components
{ok, #{type := app,
       realm := <<"io.macula">>,
       path := [<<"acme">>, <<"counter">>]}} =
    macula_mri:parse(<<"mri:app:io.macula/acme/counter">>).

%% Validation
ok = macula_mri:validate(<<"mri:app:io.macula/acme/counter">>).
{error, invalid_type} = macula_mri:validate(<<"mri:unknown:io.macula/x">>).
```

### Component Access

```erlang
MRI = <<"mri:service:io.macula/acme/counter/api">>,

app = macula_mri:type(MRI),
<<"io.macula">> = macula_mri:realm(MRI),
[<<"acme">>, <<"counter">>, <<"api">>] = macula_mri:path(MRI).
```

### Hierarchy

```erlang
%% Parent resolution
<<"mri:app:io.macula/acme/counter">> =
    macula_mri:parent(<<"mri:service:io.macula/acme/counter/api">>).

<<"mri:org:io.macula/acme">> =
    macula_mri:parent(<<"mri:app:io.macula/acme/counter">>).

undefined = macula_mri:parent(<<"mri:realm:io.macula">>).

%% Ancestry check
true = macula_mri:is_ancestor(
    <<"mri:org:io.macula/acme">>,
    <<"mri:service:io.macula/acme/counter/api">>
).
```

### Construction

```erlang
%% Type-specific constructors
<<"mri:realm:io.macula">> = macula_mri:new_realm(<<"io.macula">>).

<<"mri:org:io.macula/acme">> =
    macula_mri:new_org(<<"io.macula">>, <<"acme">>).

<<"mri:app:io.macula/acme/counter">> =
    macula_mri:new_app(<<"io.macula">>, <<"acme">>, <<"counter">>).

%% Generic constructor
<<"mri:device:io.macula/citypower/cabinet-001">> =
    macula_mri:new(device, #{
        realm => <<"io.macula">>,
        path => [<<"citypower">>, <<"cabinet-001">>]
    }).
```

## Hierarchical Relationships

MRIs encode ownership through path structure:

```
mri:realm:io.macula
├── mri:org:io.macula/acme
│   ├── mri:user:io.macula/acme/alice
│   ├── mri:user:io.macula/acme/bob
│   └── mri:app:io.macula/acme/counter
│       ├── mri:service:io.macula/acme/counter/api
│       ├── mri:license:io.macula/acme/counter/lic-001
│       └── mri:artifact:io.macula/acme/counter-v1.0.0
└── mri:org:io.macula/citypower
    ├── mri:device:io.macula/citypower/cabinet-001
    └── mri:location:io.macula/citypower/nl/amsterdam
```

## Graph Relationships

Beyond hierarchy, MRIs support cross-cutting relationships:

### Relationship Types

**Spatial:**
- `located_at` / `contains`
- `adjacent_to`

**Organizational:**
- `member_of` / `has_member`
- `manages` / `managed_by`
- `owns` / `owned_by`

**Technical:**
- `depends_on` / `depended_on_by`
- `provides` / `provided_by`
- `consumes` / `consumed_by`
- `connects_to`

**Data:**
- `trained_on` / `used_to_train`
- `derived_from` / `source_of`
- `version_of`

**Taxonomic:**
- `instance_of` / `has_instance`
- `subclass_of` / `superclass_of`
- `categorized_as` / `categorizes`

### Custom Relationships

```erlang
%% Built-in predicates are atoms
{depends_on, <<"mri:app:...">>}

%% Custom predicates use tagged tuple
{{custom, <<"powered_by">>}, <<"mri:device:...">>}
```

## Storage Behaviours

The core library defines behaviours; storage is pluggable.

### `macula_mri_store` Behaviour

```erlang
-callback register(mri(), metadata()) -> ok | {error, term()}.
-callback lookup(mri()) -> {ok, metadata()} | {error, not_found}.
-callback update(mri(), metadata()) -> ok | {error, term()}.
-callback delete(mri()) -> ok | {error, term()}.
-callback exists(mri()) -> boolean().
-callback list_children(mri()) -> [mri()].
-callback list_descendants(mri()) -> [mri()].
-callback list_by_type(mri_type(), realm()) -> [mri()].
```

### `macula_mri_graph` Behaviour

```erlang
-callback create_relationship(mri(), predicate(), mri()) -> ok | {error, term()}.
-callback delete_relationship(mri(), predicate(), mri()) -> ok | {error, term()}.
-callback related_to(mri(), predicate()) -> [mri()].
-callback related_from(mri(), predicate()) -> [mri()].
-callback traverse_transitive(mri(), predicate(), direction()) -> [mri()].
```

### Available Adapters

| Adapter | Package | Use Case |
|---------|---------|----------|
| `macula_mri_ets` | `macula/` (bundled) | Development, testing |
| `macula_mri_khepri` | `macula_mri_khepri` | Production (distributed Raft) |
| Custom | Your package | Implement behaviours |

### Configuration

```erlang
%% In application config
{macula, [
    {mri_store_adapter, macula_mri_khepri_store},
    {mri_graph_adapter, macula_mri_khepri_graph}
]}.
```

## Extensibility

### Runtime Type Registration

```erlang
%% Register custom type without code changes
macula_mri_registry:register_type(<<"streetlight">>, #{
    description => <<"Street lighting fixture">>,
    path_schema => [org, device_id],
    parent_type => device
}).

%% Now valid:
<<"mri:streetlight:io.macula/citypower/light-001">>
```

### Namespace Isolation

```erlang
%% Realm-specific types
macula_mri_registry:register_type(<<"turbine">>, #{
    realm => <<"io.windpower">>,
    path_schema => [org, farm, turbine_id]
}).

%% Valid in io.windpower realm only
<<"mri:turbine:io.windpower/acme/north-farm/turbine-001">>
```

## Best Practices

### MRI Design

1. **Meaningful segments** - `/acme/counter` not `/a/c`
2. **Shallow paths** - Aim for 2-4 segments, max 10
3. **Lowercase only** - All segments lowercase
4. **DNS-compatible** - Alphanumeric, hyphen, underscore
5. **No PII** - Use IDs, not email addresses

### Type Selection

1. **Use existing types** when they fit
2. **Check parent type** for inheritance
3. **Consider queryability** - type affects indexes
4. **Document custom types** in your realm

### Relationships

1. **Prefer standard predicates** over custom
2. **Add metadata** for context (version, timestamp)
3. **Consider both directions** - forward and reverse queries
4. **Use taxonomy** for classification

## Related

- [Storage: macula_mri_khepri](https://github.com/macula-io/macula-mri-khepri)
- [Design Document](https://github.com/macula-io/macula-console/blob/main/plans/DESIGN_MACULA_RESOURCE_IDENTIFIERS.md)
