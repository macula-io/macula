# MRI Resource Lifecycle: Publications, Installations, and Licensing

This guide describes the lifecycle of resources in the Macula mesh, focusing on the critical distinction between **publications** (artifacts) and **installations** (instances), and how they relate to licensing and DHT presence.

## Overview

```
PUBLICATION (Artifact)          LICENSE                    INSTALLATION (Instance)
────────────────────           ─────────                   ────────────────────────
What you distribute     →      Permission to use    →      What actually runs
Docker image                   Software license             Docker container
npm package                    Seat license                 Running process
Helm chart                     API key                      Deployed pod
```

**Key Principle**: DHT topics and RPC procedures are bound to **installations**, not publications. A publication is a template; an installation is a running instance with network presence.

## MRI Types

| Type | Purpose | DHT Presence | Example |
|------|---------|--------------|---------|
| `artifact` | Published package/template | No (advertised via facts) | `mri:artifact:io.macula/rgfaber/counter-v1.0.0` |
| `license` | Permission to use artifact | No (verified on install) | `mri:license:io.macula/rgfaber/counter/lic-001` |
| `instance` | Running installation | **Yes** (topics + RPC) | `mri:instance:io.macula/acme/edge-01/counter.prod` |
| `device` | Node where instances run | Yes (heartbeat) | `mri:device:io.macula/acme/edge-01` |

---

## 1. Publications (Artifacts)

### Definition

A **publication** is a versioned, immutable package that can be installed on Macula nodes. It declares what capabilities it needs and what topics/procedures it will use, but has no runtime presence until installed.

### MRI Format

```
mri:artifact:{realm}/{publisher_org}/{artifact_name}-v{version}

Examples:
  mri:artifact:io.macula/rgfaber/counter-v1.0.0
  mri:artifact:io.macula/acme/inventory-service-v2.3.1
  mri:artifact:io.macula/windpower/turbine-monitor-v0.5.0
```

### Artifact Metadata

```erlang
#{
    %% Identity
    mri => <<"mri:artifact:io.macula/rgfaber/counter-v1.0.0">>,
    name => <<"counter">>,
    version => <<"1.0.0">>,

    %% Content
    mcid => <<"Qm...">>,              % Content-addressed location
    package_type => docker,            % docker | oci | wasm | beam
    package_location => <<"ghcr.io/rgfaber/counter:1.0.0">>,

    %% Publisher
    publisher_mri => <<"mri:org:io.macula/rgfaber">>,
    published_at => 1705123456789,
    signature => <<"base64...">>,      % Ed25519 signature

    %% Artifact type
    artifact_type => app,              % app | service | library | dataset

    %% Requirements
    required_capabilities => [network, storage],
    min_memory_mb => 256,
    min_cpu_cores => 1,

    %% Declared interfaces (what instances WILL expose)
    declared_topics => [
        <<"orders.created">>,
        <<"orders.shipped">>,
        <<"orders.cancelled">>
    ],
    declared_procedures => [
        <<"place_order">>,
        <<"check_stock">>,
        <<"get_order">>
    ],

    %% Licensing
    license_type => commercial,        % open | commercial | trial
    license_terms_url => <<"https://...">>
}
```

### Marketplace Advertisement

Publications are advertised to the mesh via **integration facts**, not direct DHT registration:

```erlang
%% Published to topic: io.macula.marketplace.artifact_available
#{
    fact_type => artifact_available,
    fact_version => 1,

    artifact_mri => <<"mri:artifact:io.macula/rgfaber/counter-v1.0.0">>,
    artifact_mcid => <<"Qm...">>,

    publisher_mri => <<"mri:org:io.macula/rgfaber">>,
    publisher_node_id => <<"node-abc123">>,

    license_type => commercial,
    required_capabilities => [network],

    declared_topics => [...],
    declared_procedures => [...]
}
```

---

## 2. Licenses

### Definition

A **license** grants permission to create instances of an artifact. It defines who can use the artifact, under what conditions, and with what limitations.

### MRI Format

```
mri:license:{realm}/{licensor_org}/{artifact_name}/{license_id}

Examples:
  mri:license:io.macula/rgfaber/counter/lic-001
  mri:license:io.macula/rgfaber/counter/_open      # Implicit open-source license
  mri:license:io.macula/acme/api-gateway/enterprise-2024
```

### License Metadata

```erlang
#{
    %% Identity
    mri => <<"mri:license:io.macula/rgfaber/counter/lic-001">>,

    %% What artifact(s) does this license cover?
    artifact_mri => <<"mri:artifact:io.macula/rgfaber/counter-v1.0.0">>,
    %% OR pattern for all versions:
    artifact_pattern => <<"mri:artifact:io.macula/rgfaber/counter-v*">>,

    %% Parties
    licensor_mri => <<"mri:org:io.macula/rgfaber">>,   % Who grants
    licensee_mri => <<"mri:org:io.macula/acme">>,      % Who receives

    %% Type
    license_type => commercial,  % open | commercial | trial | enterprise

    %% Capabilities and limits
    capabilities => #{
        max_instances => 5,              % Concurrent instance limit
        max_requests_per_day => 10000,   % Rate limiting
        features => [basic, advanced],    % Feature flags
        environments => [dev, staging, prod],
        regions => [eu, us]              % Geographic restrictions
    },

    %% Validity
    granted_at => 1705123456789,
    expires_at => 1736659456789,         % Or 'never' for perpetual

    %% Cryptographic proof
    ucan_token => <<"eyJ...">>,          % UCAN delegation chain

    %% Status
    revoked_at => undefined,
    revocation_reason => undefined,

    %% Usage tracking
    active_instances => 0,               % Current count
    total_activations => 0               % Lifetime count
}
```

### License Types

| Type | Creation | Limits | Expiration | Use Case |
|------|----------|--------|------------|----------|
| `open` | Implicit/auto | None | Never | Open-source artifacts |
| `trial` | On request | Typically 1-3 instances | 14-30 days | Evaluation |
| `commercial` | Purchased | Per license | Annual/perpetual | Production use |
| `enterprise` | Negotiated | Custom | Custom | Large deployments |

### Open Source Licensing

For open-source artifacts, an implicit license is auto-generated:

```erlang
%% Implicit open-source license (never stored, generated on demand)
implicit_open_license(ArtifactMRI, LicenseeMRI) ->
    #{
        mri => derive_open_license_mri(ArtifactMRI),
        artifact_mri => ArtifactMRI,
        licensor_mri => extract_publisher(ArtifactMRI),
        licensee_mri => LicenseeMRI,
        license_type => open,
        capabilities => #{
            max_instances => unlimited,
            features => all
        },
        expires_at => never
    }.
```

---

## 3. Installations (Instances)

### Definition

An **installation** is a running instance of an artifact on a specific device. It has actual network presence: it subscribes to topics, registers RPC procedures, and processes requests.

### MRI Format

```
mri:instance:{realm}/{org}/{device}/{instance_name}[.{qualifier}]

Examples:
  mri:instance:io.macula/acme/edge-01/counter           # Default instance
  mri:instance:io.macula/acme/edge-01/counter.prod      # Production
  mri:instance:io.macula/acme/edge-01/counter.staging   # Staging
  mri:instance:io.macula/acme/laptop/counter.dev        # Development
  mri:instance:io.macula/acme/edge-01/counter.2         # Second replica
```

### Instance Metadata

```erlang
#{
    %% Identity
    mri => <<"mri:instance:io.macula/acme/edge-01/counter.prod">>,

    %% What it runs
    artifact_mri => <<"mri:artifact:io.macula/rgfaber/counter-v1.0.0">>,

    %% Permission
    license_mri => <<"mri:license:io.macula/rgfaber/counter/lic-001">>,

    %% Where it runs
    device_mri => <<"mri:device:io.macula/acme/edge-01">>,

    %% Status
    status => running,  % installing | installed | starting | running | stopping | stopped | failed

    %% Lifecycle timestamps
    installed_at => 1705123456789,
    started_at => 1705123460000,
    stopped_at => undefined,

    %% Runtime info
    process_id => <<"container-xyz123">>,
    node_id => <<"macula-node-abc">>,

    %% Active interfaces (actual DHT registrations)
    active_topics => [
        <<"io.macula/acme/edge-01/counter.prod.orders.created">>,
        <<"io.macula/acme/edge-01/counter.prod.orders.shipped">>
    ],
    active_procedures => [
        <<"io.macula/acme/edge-01/counter.prod.place_order">>,
        <<"io.macula/acme/edge-01/counter.prod.check_stock">>
    ],

    %% Health
    health_status => healthy,  % healthy | degraded | unhealthy
    last_health_check => 1705123500000
}
```

### DHT Topic/Procedure Derivation

Topics and procedures are derived from the instance MRI, not the artifact:

```erlang
%% Derive topic from instance MRI + declared topic
-spec derive_topic(InstanceMRI :: binary(), DeclaredTopic :: binary()) -> binary().
derive_topic(InstanceMRI, DeclaredTopic) ->
    %% Parse instance MRI
    {ok, #{realm := Realm, path := Path}} = macula_mri:parse(InstanceMRI),

    %% Path = [Org, Device, InstanceName]
    %% Build topic: {realm}/{org}/{device}/{instance}.{declared_topic}
    InstancePath = iolist_to_binary(lists:join(<<"/">>, [Realm | Path])),
    <<InstancePath/binary, ".", DeclaredTopic/binary>>.

%% Example:
%% InstanceMRI = <<"mri:instance:io.macula/acme/edge-01/counter.prod">>
%% DeclaredTopic = <<"orders.created">>
%% Result = <<"io.macula/acme/edge-01/counter.prod.orders.created">>
```

```erlang
%% Derive procedure from instance MRI + declared procedure
-spec derive_procedure(InstanceMRI :: binary(), DeclaredProc :: binary()) -> binary().
derive_procedure(InstanceMRI, DeclaredProc) ->
    %% Same logic as topics
    {ok, #{realm := Realm, path := Path}} = macula_mri:parse(InstanceMRI),
    InstancePath = iolist_to_binary(lists:join(<<"/">>, [Realm | Path])),
    <<InstancePath/binary, ".", DeclaredProc/binary>>.

%% Example:
%% InstanceMRI = <<"mri:instance:io.macula/acme/edge-01/counter.prod">>
%% DeclaredProc = <<"place_order">>
%% Result = <<"io.macula/acme/edge-01/counter.prod.place_order">>
```

### Multiple Instances of Same Artifact

```
Artifact: mri:artifact:io.macula/rgfaber/counter-v1.0.0
                              │
          ┌───────────────────┼───────────────────┐
          │                   │                   │
          ▼                   ▼                   ▼
      Instance A          Instance B          Instance C

MRI:  mri:instance:       mri:instance:       mri:instance:
      io.macula/acme/     io.macula/acme/     io.macula/acme/
      edge-01/counter     edge-02/counter     laptop/counter.dev

Topics:                   Topics:              Topics:
.../edge-01/counter.*     .../edge-02/         .../laptop/counter.dev.*
                          counter.*

Each instance has INDEPENDENT DHT presence.
Same artifact, different network identities.
```

---

## 4. Graph Relationships

### Core Relationships

```erlang
%% Instance → Artifact (what it runs)
create_relationship(InstanceMRI, instance_of, ArtifactMRI).

%% Instance → License (permission to run)
create_relationship(InstanceMRI, licensed_by, LicenseMRI).

%% Instance → Device (where it runs)
create_relationship(InstanceMRI, runs_on, DeviceMRI).

%% Instance → Instance (runtime dependencies)
create_relationship(FrontendMRI, depends_on, BackendMRI).

%% Artifact → Org (publisher)
create_relationship(ArtifactMRI, published_by, OrgMRI).

%% Artifact → Artifact (build-time dependencies)
create_relationship(AppMRI, depends_on, LibraryMRI).

%% License → Artifact (what it covers)
create_relationship(LicenseMRI, licenses, ArtifactMRI).

%% License → Org (who granted)
create_relationship(LicenseMRI, granted_by, LicensorMRI).

%% License → Org (who received)
create_relationship(LicenseMRI, granted_to, LicenseeMRI).
```

### Query Examples

```erlang
%% "What artifact is this instance running?"
[ArtifactMRI] = macula_mri_graph:related_to(InstanceMRI, instance_of).

%% "What instances are running this artifact?" (mesh-wide)
Instances = macula_mri_graph:related_from(ArtifactMRI, instance_of).

%% "What license enables this instance?"
[LicenseMRI] = macula_mri_graph:related_to(InstanceMRI, licensed_by).

%% "How many active instances use this license?"
Instances = macula_mri_graph:related_from(LicenseMRI, licensed_by),
ActiveCount = length([I || I <- Instances, is_running(I)]).

%% "What does this instance depend on?" (runtime)
Dependencies = macula_mri_graph:related_to(InstanceMRI, depends_on).

%% "What instances depend on this one?" (reverse)
Dependents = macula_mri_graph:related_from(InstanceMRI, depends_on).
```

### Complete Relationship Diagram

```
                    ┌─────────────────────────────────────────┐
                    │           mri:org:io.macula/rgfaber     │
                    │                 (Publisher)             │
                    └──────────────────┬──────────────────────┘
                                       │
                                       │ published_by
                                       ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                 mri:artifact:io.macula/rgfaber/counter-v1.0.0          │
│                              (Publication)                              │
│                                                                         │
│  - mcid: "Qm..."                                                       │
│  - declared_topics: [orders.created, orders.shipped]                   │
│  - declared_procedures: [place_order, check_stock]                     │
└───────────────────────────────┬─────────────────────────────────────────┘
                                │
                                │ licenses
                                ▼
┌─────────────────────────────────────────────────────────────────────────┐
│              mri:license:io.macula/rgfaber/counter/lic-001             │
│                              (License)                                  │
│                                                                         │
│  - licensor: mri:org:io.macula/rgfaber                                 │
│  - licensee: mri:org:io.macula/acme                                    │
│  - max_instances: 5                                                    │
│  - expires_at: 2025-12-31                                              │
└───────────────────────────────┬─────────────────────────────────────────┘
                                │
                                │ licensed_by
                                │
        ┌───────────────────────┼───────────────────────┐
        │                       │                       │
        ▼                       ▼                       ▼
┌───────────────────┐  ┌───────────────────┐  ┌───────────────────┐
│ INSTANCE A        │  │ INSTANCE B        │  │ INSTANCE C        │
│                   │  │                   │  │                   │
│ mri:instance:     │  │ mri:instance:     │  │ mri:instance:     │
│ io.macula/acme/   │  │ io.macula/acme/   │  │ io.macula/acme/   │
│ edge-01/counter   │  │ edge-02/counter   │  │ laptop/counter    │
│                   │  │                   │  │ .dev              │
│ status: running   │  │ status: running   │  │ status: stopped   │
└────────┬──────────┘  └────────┬──────────┘  └────────┬──────────┘
         │                      │                      │
         │ runs_on              │ runs_on              │ runs_on
         ▼                      ▼                      ▼
┌───────────────────┐  ┌───────────────────┐  ┌───────────────────┐
│ mri:device:       │  │ mri:device:       │  │ mri:device:       │
│ io.macula/acme/   │  │ io.macula/acme/   │  │ io.macula/acme/   │
│ edge-01           │  │ edge-02           │  │ laptop            │
└───────────────────┘  └───────────────────┘  └───────────────────┘
```

---

## 5. Instance Naming Conventions

### Format

```
{artifact_name}[.{qualifier}]

Where qualifier is one of:
  - Environment: dev, staging, prod, test
  - Index: 1, 2, 3 (for replicas)
  - Custom: user-provided identifier
  - Hash: short hash for ephemeral instances (e.g., a1b2c3d4)
```

### Examples

| MRI | Description |
|-----|-------------|
| `mri:instance:.../counter` | Default/only instance |
| `mri:instance:.../counter.prod` | Production environment |
| `mri:instance:.../counter.staging` | Staging environment |
| `mri:instance:.../counter.dev` | Development environment |
| `mri:instance:.../counter.2` | Second replica |
| `mri:instance:.../counter.eu-west` | Region-specific |
| `mri:instance:.../counter.a1b2c3d4` | Ephemeral/auto-scaled |

### Validation Rules

```erlang
-spec validate_instance_name(binary()) -> ok | {error, term()}.
validate_instance_name(Name) ->
    %% Pattern: artifact_name[.qualifier]
    %% - artifact_name: 1-63 chars, lowercase alphanumeric + hyphen
    %% - qualifier: 1-32 chars, lowercase alphanumeric + hyphen (optional)
    Pattern = "^[a-z][a-z0-9-]{0,62}(?:\\.[a-z0-9][a-z0-9-]{0,31})?$",
    case re:run(Name, Pattern) of
        {match, _} -> ok;
        nomatch -> {error, {invalid_instance_name, Name}}
    end.
```

### Auto-Generation

```erlang
-spec generate_instance_name(ArtifactMRI, DeviceMRI, Opts) -> binary().
generate_instance_name(ArtifactMRI, DeviceMRI, Opts) ->
    %% Extract artifact name
    {ok, #{path := ArtifactPath}} = macula_mri:parse(ArtifactMRI),
    ArtifactName = extract_base_name(ArtifactPath),

    %% Determine qualifier
    Qualifier = case maps:get(qualifier, Opts, auto) of
        auto ->
            %% Count existing instances of this artifact on device
            case count_existing_instances(DeviceMRI, ArtifactName) of
                0 -> <<>>;
                N -> <<"."/utf8, (integer_to_binary(N + 1))/binary>>
            end;

        Env when Env == <<"dev">>; Env == <<"staging">>; Env == <<"prod">> ->
            <<"."/utf8, Env/binary>>;

        Custom when is_binary(Custom) ->
            <<"."/utf8, Custom/binary>>
    end,

    <<ArtifactName/binary, Qualifier/binary>>.
```

---

## 6. Installation Flow

### Complete Lifecycle

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         INSTALLATION FLOW                               │
└─────────────────────────────────────────────────────────────────────────┘

1. USER REQUESTS INSTALLATION
   ├── Input: Artifact MRI, Device MRI, Options (qualifier, config)
   └── Example: "Install counter-v1.0.0 on edge-01 as production"

2. RESOLVE ARTIFACT
   ├── Lookup artifact metadata in local cache or mesh
   ├── Verify signature
   └── Check required capabilities against device

3. CHECK LICENSE
   ├── Query: Does licensee have valid license for this artifact?
   │   └── SELECT FROM licenses
   │       WHERE licensee = user.org
   │       AND artifact matches pattern
   │       AND expires_at > now()
   │       AND revoked_at IS NULL
   │
   ├── If open-source: Auto-grant implicit license
   │
   ├── If commercial: Verify license exists
   │   ├── Check max_instances not exceeded
   │   ├── Check environment allowed
   │   └── Check region allowed
   │
   └── If no license: Reject installation

4. GENERATE INSTANCE MRI
   ├── Derive from: device MRI + artifact name + qualifier
   └── Example: mri:instance:io.macula/acme/edge-01/counter.prod

5. CREATE INSTANCE RECORD
   ├── Register MRI in Khepri store
   ├── Create relationships:
   │   ├── instance --instance_of--> artifact
   │   ├── instance --licensed_by--> license
   │   └── instance --runs_on--> device
   └── Status: installed

6. DOWNLOAD/PREPARE PACKAGE
   ├── Fetch from mcid/package_location
   ├── Verify content hash
   └── Prepare runtime (container, WASM, etc.)

7. START INSTANCE
   ├── Launch runtime process
   ├── Status: starting → running
   │
   ├── REGISTER DHT PRESENCE
   │   ├── Subscribe to topics:
   │   │   └── {instance_path}.{declared_topic} for each topic
   │   └── Register procedures:
   │       └── {instance_path}.{declared_proc} for each procedure
   │
   └── Update instance metadata with active_topics, active_procedures

8. HEALTH MONITORING
   ├── Periodic health checks
   ├── Update health_status
   └── Emit heartbeat to mesh

9. STOP/UNINSTALL (when requested)
   ├── Unregister DHT presence (topics, procedures)
   ├── Stop runtime process
   ├── Update status: stopped
   └── (Optional) Remove instance record on uninstall
```

### Code Example: Installation

```erlang
-spec install(ArtifactMRI, DeviceMRI, Opts) -> {ok, InstanceMRI} | {error, term()}.
install(ArtifactMRI, DeviceMRI, Opts) ->
    %% 1. Resolve artifact
    {ok, Artifact} = resolve_artifact(ArtifactMRI),

    %% 2. Check license
    LicenseeMRI = maps:get(licensee_mri, Opts),
    case check_license(ArtifactMRI, LicenseeMRI, Opts) of
        {ok, LicenseMRI} ->
            %% 3. Generate instance MRI
            InstanceName = generate_instance_name(ArtifactMRI, DeviceMRI, Opts),
            {ok, #{realm := Realm, path := DevicePath}} = macula_mri:parse(DeviceMRI),
            InstanceMRI = macula_mri:new(instance, Realm, DevicePath ++ [InstanceName]),

            %% 4. Create instance record
            ok = macula_mri_store:register(InstanceMRI, #{
                artifact_mri => ArtifactMRI,
                license_mri => LicenseMRI,
                device_mri => DeviceMRI,
                status => installed,
                installed_at => erlang:system_time(millisecond)
            }),

            %% 5. Create relationships
            ok = macula_mri_graph:create_relationship(InstanceMRI, instance_of, ArtifactMRI),
            ok = macula_mri_graph:create_relationship(InstanceMRI, licensed_by, LicenseMRI),
            ok = macula_mri_graph:create_relationship(InstanceMRI, runs_on, DeviceMRI),

            %% 6. Increment license usage
            ok = increment_license_usage(LicenseMRI),

            {ok, InstanceMRI};

        {error, Reason} ->
            {error, {license_check_failed, Reason}}
    end.
```

### Code Example: Starting Instance

```erlang
-spec start(InstanceMRI) -> ok | {error, term()}.
start(InstanceMRI) ->
    %% Get instance metadata
    {ok, Instance} = macula_mri_store:lookup(InstanceMRI),
    ArtifactMRI = maps:get(artifact_mri, Instance),

    %% Get artifact to know declared topics/procedures
    {ok, Artifact} = macula_mri_store:lookup(ArtifactMRI),
    DeclaredTopics = maps:get(declared_topics, Artifact, []),
    DeclaredProcedures = maps:get(declared_procedures, Artifact, []),

    %% Launch runtime (implementation-specific)
    {ok, ProcessId} = launch_runtime(Instance, Artifact),

    %% Register DHT presence
    ActiveTopics = [derive_topic(InstanceMRI, T) || T <- DeclaredTopics],
    ActiveProcedures = [derive_procedure(InstanceMRI, P) || P <- DeclaredProcedures],

    ok = register_topics(ActiveTopics),
    ok = register_procedures(ActiveProcedures),

    %% Update instance record
    ok = macula_mri_store:update(InstanceMRI, #{
        status => running,
        started_at => erlang:system_time(millisecond),
        process_id => ProcessId,
        active_topics => ActiveTopics,
        active_procedures => ActiveProcedures
    }),

    ok.
```

---

## 7. DHT Integration Summary

### What Gets Registered

| Resource | DHT Registration | Format |
|----------|-----------------|--------|
| Artifact | **No** - Advertised via marketplace facts | `io.macula.marketplace.artifact_available` |
| License | **No** - Stored in Khepri only | N/A |
| Instance (running) | **Yes** - Topics + Procedures | `{instance_path}.{topic/proc}` |
| Device | **Yes** - Heartbeat topic | `io.macula.mesh.device_heartbeat` |

### Topic Format

```
{realm}/{org}/{device}/{instance_name}.{event_type}

Example:
io.macula/acme/edge-01/counter.prod.orders.created
│         │    │       │            │
│         │    │       │            └── Event from artifact declaration
│         │    │       └── Instance name (artifact + qualifier)
│         │    └── Device name
│         └── Organization
└── Realm
```

### Procedure Format

```
{realm}/{org}/{device}/{instance_name}.{procedure_name}

Example:
io.macula/acme/edge-01/counter.prod.place_order
```

### Subscription Patterns

```erlang
%% Subscribe to all events from a specific instance
macula:subscribe(<<"io.macula/acme/edge-01/counter.prod.*">>).

%% Subscribe to specific event type from all instances of an artifact on a device
macula:subscribe(<<"io.macula/acme/edge-01/counter.*.orders.created">>).

%% Subscribe to all instances across all devices in an org
macula:subscribe(<<"io.macula/acme/*/counter.*.orders.created">>).
```

---

## 8. Best Practices

### Artifact Design

1. **Version semantically** - Use semver for artifact versions
2. **Declare all interfaces** - List all topics and procedures the artifact will use
3. **Minimize capabilities** - Request only what you need
4. **Sign everything** - All artifacts must be cryptographically signed

### Instance Naming

1. **Use meaningful qualifiers** - `prod`, `staging`, `dev` over `1`, `2`, `3`
2. **Be consistent** - Same naming scheme across all devices
3. **Keep it short** - Instance names should be concise but descriptive
4. **Avoid PII** - No user emails or personal data in names

### Licensing

1. **Check early** - Validate license before downloading package
2. **Handle expiration** - Implement grace periods and notifications
3. **Track usage** - Monitor active instances against limits
4. **Respect revocation** - Stop instances when license is revoked

### DHT Integration

1. **Instance-bound topics** - Never use artifact MRI in topic names
2. **Graceful shutdown** - Unregister from DHT before stopping
3. **Idempotent registration** - Handle restart scenarios
4. **Health reporting** - Emit regular heartbeats

---

## See Also

- [MRI Guide](mri.md) - Core MRI concepts and API
- [MRI Graph Relationships](mri_graph.md) - Relationship types and queries
- [Mesh PubSub Guide](mesh_pubsub.md) - DHT topic patterns
- [UCAN Capabilities](ucan.md) - Cryptographic authorization
