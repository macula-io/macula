# Mesh-Wide Hot Code Upgrade

**Target Release:** v1.0.0
**Status:** Design Phase
**Priority:** High (Core Platform Feature)

---

## Overview

Macula's clear separation between platform and workloads enables a powerful capability: **coordinated mesh-wide hot code upgrades** without service interruption. This document describes the design for upgrading workload applications across all nodes in the mesh using standard OTP release mechanisms coordinated through Macula's RPC layer.

---

## Problem Statement

### Current State

Individual nodes can perform hot code upgrades using standard OTP mechanisms:
- `code:load_file/1` for simple module reloads
- `release_handler` for full release upgrades
- `appup` and `relup` files for state migration

**Limitation**: Each node must be upgraded manually or via external orchestration (Ansible, Kubernetes rolling updates, etc.).

### Desired State

Mesh-wide coordinated upgrades from any node:

```erlang
%% Upgrade snake_game to v1.1.0 across entire mesh
macula:upgrade(snake_game, "1.1.0", #{
    strategy => rolling,
    batch_size => 3,
    health_check => true
}).
```

**Benefits**:
- Zero external orchestration required
- BEAM-native upgrade path (OTP release_handler)
- Rolling upgrades with automatic rollback
- Health verification between batches
- Audit trail of upgrades across mesh

---

## Architecture

### Component Overview

```
┌─────────────────────────────────────────────────────────┐
│  macula_upgrade (Coordinator)                           │
│  - Discovers nodes with target app                      │
│  - Plans upgrade batches                                │
│  - Coordinates execution                                │
│  - Monitors health                                      │
│  - Handles rollback                                     │
└────────────────────────┬────────────────────────────────┘
                         │ Macula RPC
                         ↓
┌─────────────────────────────────────────────────────────┐
│  macula_upgrade_agent (Per-Node)                        │
│  - Receives upgrade commands                            │
│  - Executes release_handler operations                  │
│  - Reports status back to coordinator                   │
│  - Performs local health checks                         │
└─────────────────────────────────────────────────────────┘
```

### New Modules

| Module | Purpose | LOC (est) |
|--------|---------|-----------|
| `macula_upgrade` | Mesh-wide coordination | ~400 |
| `macula_upgrade_agent` | Per-node execution | ~250 |
| `macula_upgrade_planner` | Batch planning strategies | ~200 |
| `macula_upgrade_health` | Health check framework | ~150 |

**Total estimated**: ~1,000 LOC

---

## API Design

### Primary API

```erlang
-module(macula).

%% Mesh-wide upgrade
-spec upgrade(App, Version, Opts) -> {ok, UpgradeResult} | {error, Reason}
    when App :: atom(),
         Version :: string(),
         Opts :: upgrade_opts(),
         UpgradeResult :: #{
             total := non_neg_integer(),
             upgraded := non_neg_integer(),
             failed := non_neg_integer(),
             details := [node_result()]
         }.

%% Check upgrade status
-spec upgrade_status(UpgradeId) -> {ok, Status} | {error, not_found}.

%% Cancel in-progress upgrade
-spec upgrade_cancel(UpgradeId) -> ok | {error, Reason}.

%% List available versions for an app
-spec upgrade_versions(App) -> {ok, [Version]} | {error, Reason}.
```

### Options

```erlang
-type upgrade_opts() :: #{
    %% Upgrade strategy
    strategy => rolling | all_at_once | canary,

    %% Nodes per batch (for rolling)
    batch_size => pos_integer(),

    %% Delay between batches (ms)
    batch_delay => non_neg_integer(),

    %% Health check configuration
    health_check => boolean() | health_opts(),

    %% Automatic rollback on failure
    rollback_on_fail => boolean(),

    %% Target specific nodes (default: all nodes with app)
    nodes => [node_id()] | all,

    %% Timeout per node (ms)
    node_timeout => pos_integer(),

    %% Release tarball location
    release_url => binary()  % URL to fetch release from
}.

-type health_opts() :: #{
    %% Health check endpoint
    endpoint => binary(),  % e.g., <<"system.health">>

    %% Required health status
    required_status => ok | degraded,

    %% Retries before declaring failure
    retries => pos_integer(),

    %% Delay between retries (ms)
    retry_delay => non_neg_integer()
}.
```

### Example Usage

```erlang
%% Simple rolling upgrade
macula:upgrade(snake_game, "1.1.0", #{
    strategy => rolling,
    batch_size => 2
}).

%% Careful production upgrade
macula:upgrade(payment_service, "2.0.0", #{
    strategy => canary,
    batch_size => 1,
    batch_delay => 30000,  % 30 second delay
    health_check => #{
        endpoint => <<"payment.health">>,
        required_status => ok,
        retries => 3,
        retry_delay => 5000
    },
    rollback_on_fail => true
}).

%% Upgrade specific nodes only
macula:upgrade(analytics, "1.5.0", #{
    nodes => [<<"node-eu-1">>, <<"node-eu-2">>],
    strategy => all_at_once
}).
```

---

## Upgrade Strategies

### 1. Rolling Upgrade (Default)

Upgrades nodes in batches, waiting for health confirmation between batches.

```
Batch 1: [node1, node2] → upgrade → health check → OK
Batch 2: [node3, node4] → upgrade → health check → OK
Batch 3: [node5, node6] → upgrade → health check → OK
Complete: 6/6 nodes upgraded
```

**Use case**: Production systems requiring zero downtime

### 2. Canary Upgrade

Upgrades a small subset first, waits for extended monitoring, then proceeds.

```
Canary: [node1] → upgrade → monitor 5 minutes → OK
Batch 1: [node2, node3, node4] → upgrade → health check → OK
Batch 2: [node5, node6] → upgrade → health check → OK
Complete: 6/6 nodes upgraded
```

**Use case**: High-risk upgrades requiring validation

### 3. All-at-Once

Upgrades all nodes simultaneously.

```
All: [node1, node2, node3, node4, node5, node6] → upgrade
Complete: 6/6 nodes upgraded
```

**Use case**: Development/staging or when rolling isn't feasible

---

## Implementation Details

### Coordinator Module

```erlang
-module(macula_upgrade).
-behaviour(gen_statem).

-export([
    upgrade/3,
    status/1,
    cancel/1
]).

%% State machine states
-type state() :: idle | discovering | planning | upgrading | verifying | complete | failed.

%% Upgrade record
-record(upgrade, {
    id :: binary(),
    app :: atom(),
    version :: string(),
    opts :: map(),
    nodes :: [node_id()],
    batches :: [[node_id()]],
    current_batch :: non_neg_integer(),
    results :: #{node_id() => result()},
    started_at :: integer(),
    completed_at :: integer() | undefined
}).

%%====================================================================
%% API
%%====================================================================

upgrade(App, Version, Opts) ->
    UpgradeId = generate_id(),
    case gen_statem:call(?MODULE, {start_upgrade, UpgradeId, App, Version, Opts}) of
        {ok, started} ->
            %% Wait for completion or return immediately based on opts
            case maps:get(async, Opts, false) of
                true -> {ok, #{upgrade_id => UpgradeId}};
                false -> wait_for_completion(UpgradeId)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% State Machine
%%====================================================================

init([]) ->
    %% Register upgrade agent on this node
    macula:advertise(<<"system.upgrade">>, fun handle_upgrade_request/1),
    {ok, idle, #{}}.

%% State: idle -> discovering
handle_event({call, From}, {start_upgrade, Id, App, Version, Opts}, idle, Data) ->
    Upgrade = #upgrade{
        id = Id,
        app = App,
        version = Version,
        opts = Opts,
        started_at = erlang:system_time(millisecond)
    },
    {next_state, discovering, Data#{Id => Upgrade},
     [{reply, From, {ok, started}}, {next_event, internal, discover}]};

%% State: discovering
handle_event(internal, discover, discovering, #{current := Upgrade} = Data) ->
    App = Upgrade#upgrade.app,
    Opts = Upgrade#upgrade.opts,

    %% Find all nodes running this app
    Nodes = case maps:get(nodes, Opts, all) of
        all -> discover_nodes_with_app(App);
        Specific -> Specific
    end,

    case Nodes of
        [] ->
            {next_state, failed, Data,
             [{next_event, internal, {error, no_nodes_found}}]};
        _ ->
            Upgrade1 = Upgrade#upgrade{nodes = Nodes},
            {next_state, planning, Data#{current := Upgrade1},
             [{next_event, internal, plan}]}
    end;

%% State: planning
handle_event(internal, plan, planning, #{current := Upgrade} = Data) ->
    Opts = Upgrade#upgrade.opts,
    Nodes = Upgrade#upgrade.nodes,

    %% Create batches based on strategy
    Batches = macula_upgrade_planner:create_batches(
        Nodes,
        maps:get(strategy, Opts, rolling),
        maps:get(batch_size, Opts, 3)
    ),

    Upgrade1 = Upgrade#upgrade{batches = Batches, current_batch = 1},
    {next_state, upgrading, Data#{current := Upgrade1},
     [{next_event, internal, upgrade_batch}]};

%% State: upgrading
handle_event(internal, upgrade_batch, upgrading, #{current := Upgrade} = Data) ->
    BatchNum = Upgrade#upgrade.current_batch,
    Batches = Upgrade#upgrade.batches,

    case BatchNum > length(Batches) of
        true ->
            %% All batches complete
            {next_state, complete, Data,
             [{next_event, internal, finalize}]};
        false ->
            Batch = lists:nth(BatchNum, Batches),
            %% Upgrade nodes in this batch
            Results = upgrade_batch_nodes(Batch, Upgrade),

            case check_batch_results(Results, Upgrade#upgrade.opts) of
                ok ->
                    Upgrade1 = update_results(Upgrade, Results),
                    Upgrade2 = Upgrade1#upgrade{current_batch = BatchNum + 1},

                    %% Optional delay between batches
                    Delay = maps:get(batch_delay, Upgrade#upgrade.opts, 0),
                    {next_state, verifying, Data#{current := Upgrade2},
                     [{state_timeout, Delay, next_batch}]};
                {error, FailedNodes} ->
                    handle_batch_failure(FailedNodes, Upgrade, Data)
            end
    end;

%% State: verifying (health checks between batches)
handle_event(state_timeout, next_batch, verifying, #{current := Upgrade} = Data) ->
    case maps:get(health_check, Upgrade#upgrade.opts, false) of
        false ->
            {next_state, upgrading, Data,
             [{next_event, internal, upgrade_batch}]};
        HealthOpts ->
            case perform_health_checks(Upgrade, HealthOpts) of
                ok ->
                    {next_state, upgrading, Data,
                     [{next_event, internal, upgrade_batch}]};
                {error, UnhealthyNodes} ->
                    handle_health_failure(UnhealthyNodes, Upgrade, Data)
            end
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

discover_nodes_with_app(App) ->
    %% Query DHT for nodes advertising this app
    case macula:discover(<<(atom_to_binary(App))/binary, ".*">>) of
        {ok, Services} ->
            %% Extract unique node IDs from service advertisements
            lists:usort([NodeId || #{node_id := NodeId} <- Services]);
        {error, _} ->
            []
    end.

upgrade_batch_nodes(Nodes, Upgrade) ->
    App = Upgrade#upgrade.app,
    Version = Upgrade#upgrade.version,
    Opts = Upgrade#upgrade.opts,
    Timeout = maps:get(node_timeout, Opts, 60000),

    %% Parallel upgrade of all nodes in batch
    Results = pmap(fun(NodeId) ->
        Request = #{
            app => App,
            version => Version,
            release_url => maps:get(release_url, Opts, undefined)
        },
        case macula:call(NodeId, <<"system.upgrade">>, Request, Timeout) of
            {ok, Result} -> {NodeId, Result};
            {error, Reason} -> {NodeId, {error, Reason}}
        end
    end, Nodes),

    maps:from_list(Results).

perform_health_checks(Upgrade, HealthOpts) when is_map(HealthOpts) ->
    %% Check health of all upgraded nodes so far
    UpgradedNodes = get_upgraded_nodes(Upgrade),
    Endpoint = maps:get(endpoint, HealthOpts, <<"system.health">>),
    Retries = maps:get(retries, HealthOpts, 3),
    RetryDelay = maps:get(retry_delay, HealthOpts, 5000),
    RequiredStatus = maps:get(required_status, HealthOpts, ok),

    check_nodes_health(UpgradedNodes, Endpoint, RequiredStatus, Retries, RetryDelay);

perform_health_checks(_Upgrade, true) ->
    %% Default health check
    perform_health_checks(_Upgrade, #{endpoint => <<"system.health">>}).

handle_batch_failure(FailedNodes, Upgrade, Data) ->
    case maps:get(rollback_on_fail, Upgrade#upgrade.opts, true) of
        true ->
            %% Rollback all upgraded nodes
            rollback_upgrade(Upgrade),
            {next_state, failed, Data,
             [{next_event, internal, {error, {batch_failed, FailedNodes}}}]};
        false ->
            %% Continue despite failures
            Upgrade1 = Upgrade#upgrade{current_batch = Upgrade#upgrade.current_batch + 1},
            {next_state, upgrading, Data#{current := Upgrade1},
             [{next_event, internal, upgrade_batch}]}
    end.

rollback_upgrade(Upgrade) ->
    %% Get all successfully upgraded nodes
    UpgradedNodes = get_upgraded_nodes(Upgrade),
    PreviousVersion = get_previous_version(Upgrade#upgrade.app),

    %% Rollback each node
    lists:foreach(fun(NodeId) ->
        macula:call(NodeId, <<"system.rollback">>, #{
            app => Upgrade#upgrade.app,
            version => PreviousVersion
        })
    end, UpgradedNodes).
```

### Agent Module (Per-Node)

```erlang
-module(macula_upgrade_agent).

-export([
    init/0,
    handle_upgrade/1,
    handle_rollback/1
]).

%%====================================================================
%% Initialization
%%====================================================================

init() ->
    %% Register upgrade handlers
    macula:advertise(<<"system.upgrade">>, fun handle_upgrade/1),
    macula:advertise(<<"system.rollback">>, fun handle_rollback/1),
    macula:advertise(<<"system.versions">>, fun handle_versions/1),
    ok.

%%====================================================================
%% Handlers
%%====================================================================

handle_upgrade(#{app := App, version := Version} = Request) ->
    logger:info("Upgrading ~p to ~s", [App, Version]),

    %% Fetch release if URL provided
    case maps:get(release_url, Request, undefined) of
        undefined -> ok;
        Url -> fetch_release(Url, Version)
    end,

    %% Perform OTP release upgrade
    try
        %% Unpack the release
        case release_handler:unpack_release(Version) of
            {ok, Vsn} ->
                logger:info("Unpacked release ~s", [Vsn]);
            {error, {already_installed, Vsn}} ->
                logger:info("Release ~s already installed", [Vsn])
        end,

        %% Install the release
        case release_handler:install_release(Version) of
            {ok, OldVsn, _} ->
                logger:info("Installed ~s (was ~s)", [Version, OldVsn]),

                %% Make permanent
                ok = release_handler:make_permanent(Version),
                logger:info("Made ~s permanent", [Version]),

                {ok, #{
                    status => upgraded,
                    from_version => OldVsn,
                    to_version => Version,
                    node_id => macula:node_id()
                }};

            {error, Reason} ->
                logger:error("Install failed: ~p", [Reason]),
                {error, {install_failed, Reason}}
        end
    catch
        Class:Error:Stack ->
            logger:error("Upgrade exception: ~p:~p~n~p", [Class, Error, Stack]),
            {error, {exception, Error}}
    end.

handle_rollback(#{app := App, version := Version}) ->
    logger:warning("Rolling back ~p to ~s", [App, Version]),

    case release_handler:install_release(Version) of
        {ok, _, _} ->
            ok = release_handler:make_permanent(Version),
            {ok, #{status => rolled_back, version => Version}};
        {error, Reason} ->
            {error, {rollback_failed, Reason}}
    end.

handle_versions(#{app := App}) ->
    %% List installed versions
    Releases = release_handler:which_releases(),
    Versions = [Vsn || {Name, Vsn, _, _} <- Releases,
                       lists:member(App, get_release_apps(Name, Vsn))],
    {ok, #{app => App, versions => Versions}}.

%%====================================================================
%% Internal Functions
%%====================================================================

fetch_release(Url, Version) ->
    %% Download release tarball
    ReleasesDir = code:root_dir() ++ "/releases",
    TarFile = ReleasesDir ++ "/" ++ Version ++ ".tar.gz",

    case httpc:request(get, {binary_to_list(Url), []}, [], [{stream, TarFile}]) of
        {ok, saved_to_file} ->
            logger:info("Downloaded release to ~s", [TarFile]),
            ok;
        {error, Reason} ->
            logger:error("Failed to download release: ~p", [Reason]),
            {error, {download_failed, Reason}}
    end.

get_release_apps(RelName, Vsn) ->
    %% Get list of apps in a release
    case release_handler:get_release(RelName, Vsn) of
        {ok, Release} ->
            proplists:get_value(applications, Release, []);
        _ ->
            []
    end.
```

---

## Health Check Integration

### Default Health Service

Every Macula node provides a default health endpoint:

```erlang
%% Registered automatically by macula_upgrade_agent
macula:advertise(<<"system.health">>, fun() ->
    #{
        status => ok,
        node_id => macula:node_id(),
        uptime => get_uptime(),
        memory => erlang:memory(),
        processes => erlang:system_info(process_count),
        apps => application:which_applications()
    }
end).
```

### Custom Health Checks

Applications can register custom health checks:

```erlang
%% In your workload app
init([]) ->
    macula:advertise(<<"snake_game.health">>, fun check_game_health/0),
    {ok, #state{}}.

check_game_health() ->
    case ets:info(game_state) of
        undefined ->
            #{status => error, reason => no_game_state};
        _ ->
            ActiveGames = ets:info(game_state, size),
            #{
                status => ok,
                active_games => ActiveGames,
                memory_mb => erlang:memory(total) / 1024 / 1024
            }
    end.
```

### Health Check During Upgrade

```erlang
%% Upgrade with custom health check
macula:upgrade(snake_game, "1.1.0", #{
    health_check => #{
        endpoint => <<"snake_game.health">>,
        required_status => ok,
        retries => 5,
        retry_delay => 2000
    }
}).
```

---

## Release Distribution

### Option 1: Pre-deployed Releases

Releases already present on each node:

```erlang
%% Nodes have releases in /releases directory
macula:upgrade(snake_game, "1.1.0", #{}).
```

### Option 2: URL-based Distribution

Fetch releases from central repository:

```erlang
macula:upgrade(snake_game, "1.1.0", #{
    release_url => <<"https://releases.example.com/snake_game-1.1.0.tar.gz">>
}).
```

### Option 3: Mesh-based Distribution (Future)

Distribute releases through the mesh itself:

```erlang
%% Upload release to mesh storage (future feature)
macula:store_release(snake_game, "1.1.0", TarballBinary),

%% Upgrade fetches from mesh
macula:upgrade(snake_game, "1.1.0", #{
    release_source => mesh
}).
```

---

## Monitoring and Observability

### Upgrade Events

Published during upgrade for monitoring:

```erlang
%% Subscribe to upgrade events
macula:subscribe(<<"system.upgrade.*">>, fun handle_upgrade_event/1).

%% Events published:
%% system.upgrade.started     - Upgrade initiated
%% system.upgrade.batch       - Batch started/completed
%% system.upgrade.node        - Node upgraded/failed
%% system.upgrade.health      - Health check result
%% system.upgrade.rollback    - Rollback initiated
%% system.upgrade.completed   - Upgrade finished
%% system.upgrade.failed      - Upgrade failed
```

### Upgrade History

Query upgrade history:

```erlang
%% Get recent upgrades
{ok, History} = macula:upgrade_history(#{limit => 10}).

%% Get specific upgrade details
{ok, Details} = macula:upgrade_details(UpgradeId).
```

### Metrics

Exported for Prometheus/OpenTelemetry:

```
# Upgrade duration histogram
macula_upgrade_duration_seconds{app="snake_game",version="1.1.0"}

# Nodes upgraded counter
macula_upgrade_nodes_total{app="snake_game",status="success"}
macula_upgrade_nodes_total{app="snake_game",status="failed"}

# Active upgrades gauge
macula_upgrade_active
```

---

## Error Handling

### Failure Scenarios

| Scenario | Detection | Response |
|----------|-----------|----------|
| Node unreachable | RPC timeout | Skip or rollback |
| Release not found | release_handler error | Abort upgrade |
| Health check fails | Custom check | Retry or rollback |
| Partial batch failure | Mixed results | Continue or rollback |
| Coordinator crash | gen_statem terminate | Agents continue running |

### Rollback Mechanism

```erlang
%% Automatic rollback on failure
macula:upgrade(my_app, "1.1.0", #{
    rollback_on_fail => true  % Default
}).

%% Manual rollback
macula:rollback(my_app, "1.0.0", #{
    nodes => all  % or specific nodes
}).
```

### Partial Upgrade Recovery

If coordinator crashes mid-upgrade:

```erlang
%% Query current state across mesh
{ok, Status} = macula:upgrade_audit(my_app).
%% Returns: #{
%%   "1.0.0" => [node1, node2, node3],
%%   "1.1.0" => [node4, node5]
%% }

%% Resume upgrade
macula:upgrade(my_app, "1.1.0", #{
    nodes => [node1, node2, node3]  % Only remaining nodes
}).
```

---

## Security Considerations

### Authorization

```erlang
%% Only authorized nodes can initiate upgrades
macula:upgrade(my_app, "1.1.0", #{
    auth_token => <<"secret_token">>
}).

%% Agent verifies authorization
handle_upgrade(#{auth_token := Token} = Request) ->
    case verify_upgrade_token(Token) of
        ok -> do_upgrade(Request);
        {error, unauthorized} -> {error, unauthorized}
    end.
```

### Release Verification

```erlang
%% Verify release signature
macula:upgrade(my_app, "1.1.0", #{
    release_url => Url,
    signature_url => SigUrl,
    public_key => PublicKey
}).
```

### Audit Logging

All upgrade operations logged with:
- Initiator node ID
- Timestamp
- Target app/version
- Affected nodes
- Result (success/failure)
- Rollback actions

---

## Testing Strategy

### Unit Tests

```erlang
%% Test batch planning
macula_upgrade_planner_tests:
  - rolling_creates_correct_batches/0
  - canary_separates_first_node/0
  - all_at_once_single_batch/0

%% Test agent operations
macula_upgrade_agent_tests:
  - successful_upgrade/0
  - failed_upgrade_returns_error/0
  - rollback_restores_version/0
```

### Integration Tests

```erlang
%% Multi-node upgrade test
upgrade_integration_test() ->
    %% Start 5-node mesh
    Nodes = start_test_mesh(5),

    %% Deploy v1.0.0 to all
    deploy_app(test_app, "1.0.0", Nodes),

    %% Upgrade to v1.1.0
    {ok, Result} = macula:upgrade(test_app, "1.1.0", #{
        strategy => rolling,
        batch_size => 2
    }),

    %% Verify all nodes upgraded
    ?assertEqual(5, maps:get(upgraded, Result)),
    ?assertEqual(0, maps:get(failed, Result)),

    %% Verify correct version running
    lists:foreach(fun(Node) ->
        {ok, #{version := "1.1.0"}} = get_app_version(Node, test_app)
    end, Nodes).
```

### Chaos Testing

```erlang
%% Test failure scenarios
chaos_upgrade_test() ->
    Nodes = start_test_mesh(6),

    %% Kill node during upgrade
    spawn(fun() ->
        timer:sleep(500),
        kill_node(lists:nth(3, Nodes))
    end),

    %% Upgrade should handle failure gracefully
    Result = macula:upgrade(test_app, "1.1.0", #{
        rollback_on_fail => true
    }),

    %% Should have rolled back
    ?assertMatch({error, {batch_failed, _}}, Result).
```

---

## Implementation Timeline

### Phase 1: Core Framework (Week 1-2)

- [ ] `macula_upgrade` coordinator module
- [ ] `macula_upgrade_agent` per-node module
- [ ] Basic rolling upgrade strategy
- [ ] Unit tests

### Phase 2: Strategies & Health (Week 3)

- [ ] `macula_upgrade_planner` with all strategies
- [ ] `macula_upgrade_health` framework
- [ ] Custom health check integration
- [ ] Integration tests

### Phase 3: Resilience (Week 4)

- [ ] Rollback mechanism
- [ ] Partial failure handling
- [ ] Coordinator crash recovery
- [ ] Chaos tests

### Phase 4: Polish & Docs (Week 5)

- [ ] Metrics and observability
- [ ] Security (auth, signatures)
- [ ] Documentation
- [ ] Performance optimization

**Total estimated effort**: 5 weeks

---

## Dependencies

- **Macula v0.9.0**: NAT traversal for reliable node connectivity
- **OTP 27+**: Modern release_handler features
- **Application appup files**: Required for stateful upgrades

---

## Future Enhancements

### v1.1.0+

- **Mesh-based release storage**: Store releases in DHT
- **Differential updates**: Only transfer changed modules
- **Scheduled upgrades**: Cron-like upgrade scheduling
- **Upgrade policies**: Automatic upgrades based on rules
- **Multi-app transactions**: Upgrade multiple apps atomically

---

## References

- [OTP Release Handling](https://www.erlang.org/doc/design_principles/release_handling.html)
- [Appup Cookbook](https://www.erlang.org/doc/design_principles/appup_cookbook.html)
- [Hot Code Loading](https://www.erlang.org/doc/reference_manual/code_loading.html)
- [Relx Documentation](https://erlware.github.io/relx/)

---

## Summary

Mesh-wide hot code upgrade is a **flagship feature** for Macula v1.0.0 that leverages:

1. **Platform/workload separation** - Upgrade workloads while platform maintains connections
2. **Macula RPC** - Coordinate upgrades across mesh without external tools
3. **OTP release_handler** - Standard BEAM upgrade mechanism
4. **Health checks** - Verify upgrades before proceeding

**Result**: True BEAM-native distributed deployment with zero-downtime rolling upgrades orchestrated through the mesh itself.
