# NAT Traversal Configuration Reference

Complete configuration reference for the Macula NAT traversal system.

---

## Application Configuration (sys.config)

```erlang
{macula, [
    %% NAT System Configuration
    {nat, [
        %% Enable/disable NAT traversal subsystem
        {enabled, true},

        %% NAT Detection Configuration
        {detection, [
            %% Detection timeout in milliseconds
            {timeout, 5000},

            %% Minimum observers required for detection
            {min_observers, 2},

            %% Auto-detect on startup
            {auto_detect, true},

            %% Refresh interval (milliseconds, 0 = manual only)
            {refresh_interval, 300000}  % 5 minutes
        ]},

        %% NAT Cache Configuration
        {cache, [
            %% Maximum cached profiles
            {max_entries, 10000},

            %% Cache TTL in seconds
            {ttl, 300},  % 5 minutes

            %% Stale grace period (seconds)
            {stale_grace, 60},

            %% Cleanup interval (milliseconds)
            {cleanup_interval, 60000}
        ]},

        %% Hole Punching Configuration
        {hole_punch, [
            %% Enable hole punching
            {enabled, true},

            %% Maximum punch attempts before relay fallback
            {max_attempts, 3},

            %% Timeout per attempt (milliseconds)
            {attempt_timeout, 5000},

            %% Coordination timeout (milliseconds)
            {coordination_timeout, 10000}
        ]},

        %% Relay Configuration
        {relay, [
            %% Enable relay functionality (as server)
            {enabled, false},

            %% Maximum concurrent relay sessions
            {max_sessions, 100},

            %% Per-session bandwidth limit (bytes/second)
            {bandwidth_limit, 1048576},  % 1 MB/s

            %% Session timeout (milliseconds)
            {session_timeout, 1800000},  % 30 minutes

            %% Auto-register as relay in DHT
            {auto_register, true}
        ]},

        %% Connection Strategy
        {connection, [
            %% Prefer direct connections over relay
            {prefer_direct, true},

            %% Allow relay fallback
            {allow_relay, true},

            %% Connection timeout (milliseconds)
            {timeout, 15000},

            %% Retry attempts
            {max_retries, 3}
        ]}
    ]}
]}.
```

---

## Environment Variables

NAT configuration can also be set via environment variables:

| Variable | Description | Default |
|----------|-------------|---------|
| `MACULA_NAT_ENABLED` | Enable NAT subsystem | `true` |
| `MACULA_NAT_DETECTION_TIMEOUT` | Detection timeout (ms) | `5000` |
| `MACULA_NAT_CACHE_TTL` | Cache TTL (seconds) | `300` |
| `MACULA_NAT_CACHE_MAX_ENTRIES` | Max cache entries | `10000` |
| `MACULA_NAT_PUNCH_ENABLED` | Enable hole punching | `true` |
| `MACULA_NAT_PUNCH_MAX_ATTEMPTS` | Max punch attempts | `3` |
| `MACULA_NAT_RELAY_ENABLED` | Enable relay server | `false` |
| `MACULA_NAT_RELAY_MAX_SESSIONS` | Max relay sessions | `100` |
| `MACULA_NAT_RELAY_BANDWIDTH` | Bandwidth limit (bytes/s) | `1048576` |

---

## Configuration Options Reference

### NAT Detection

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `timeout` | integer | 5000 | Detection timeout in milliseconds |
| `min_observers` | integer | 2 | Minimum external observers needed |
| `auto_detect` | boolean | true | Auto-detect NAT type on startup |
| `refresh_interval` | integer | 300000 | Background refresh interval (0 = disabled) |
| `observers` | list | [] | Custom observer endpoints |

### NAT Cache

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `max_entries` | integer | 10000 | Maximum cached NAT profiles |
| `ttl` | integer | 300 | Cache entry TTL in seconds |
| `stale_grace` | integer | 60 | Stale-while-revalidate grace period |
| `cleanup_interval` | integer | 60000 | Expired entry cleanup interval (ms) |
| `publish_to_dht` | boolean | true | Publish local profile to DHT |

### Hole Punching

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `enabled` | boolean | true | Enable hole punching |
| `max_attempts` | integer | 3 | Maximum punch attempts |
| `attempt_timeout` | integer | 5000 | Single attempt timeout (ms) |
| `coordination_timeout` | integer | 10000 | Coordination phase timeout (ms) |
| `port_prediction` | boolean | true | Enable port prediction for PC NATs |
| `simultaneous_open` | boolean | true | Use simultaneous open technique |

### Relay Server

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `enabled` | boolean | false | Enable relay server functionality |
| `max_sessions` | integer | 100 | Maximum concurrent relay sessions |
| `bandwidth_limit` | integer | 1048576 | Per-session bandwidth limit (bytes/s) |
| `total_bandwidth_limit` | integer | 10485760 | Total relay bandwidth limit (bytes/s) |
| `session_timeout` | integer | 1800000 | Session timeout in milliseconds |
| `auto_register` | boolean | true | Auto-register as relay in DHT |
| `priority` | integer | 50 | Relay priority (lower = preferred) |

### Connection Strategy

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `prefer_direct` | boolean | true | Try direct connection first |
| `allow_relay` | boolean | true | Allow relay fallback |
| `timeout` | integer | 15000 | Total connection timeout (ms) |
| `max_retries` | integer | 3 | Connection retry attempts |
| `strategy_order` | list | [direct, punch, relay] | Connection strategy order |

---

## Runtime Configuration

### Programmatic Configuration

```erlang
%% Update NAT configuration at runtime
application:set_env(macula, nat, [
    {cache, [{ttl, 600}]},
    {relay, [{enabled, true}]}
]).

%% Or update specific values
macula_config:set(nat_cache_ttl, 600).
macula_config:set(nat_relay_enabled, true).
```

### Per-Connection Options

```erlang
%% Override defaults for specific connection
{ok, Conn} = macula_nat_connector:connect(TargetNodeId, #{
    timeout => 30000,           % Override default timeout
    prefer_direct => false,     % Go straight to relay
    allow_relay => true,
    max_attempts => 5           % More retries
}).
```

---

## Configuration Profiles

### Edge Node (Behind NAT)

Typical configuration for nodes behind residential/mobile NAT:

```erlang
{macula, [
    {nat, [
        {detection, [
            {auto_detect, true},
            {refresh_interval, 300000}
        ]},
        {hole_punch, [
            {enabled, true},
            {max_attempts, 3}
        ]},
        {relay, [
            {enabled, false}  % Don't serve as relay
        ]},
        {connection, [
            {prefer_direct, true},
            {allow_relay, true}
        ]}
    ]}
]}.
```

### Public Server (Gateway/Relay)

Configuration for servers with public IP acting as relay:

```erlang
{macula, [
    {nat, [
        {detection, [
            {auto_detect, false}  % Public IP, no NAT detection needed
        ]},
        {hole_punch, [
            {enabled, true}  % Can coordinate punches
        ]},
        {relay, [
            {enabled, true},
            {max_sessions, 500},
            {bandwidth_limit, 5242880},  % 5 MB/s per session
            {total_bandwidth_limit, 104857600},  % 100 MB/s total
            {auto_register, true}
        ]}
    ]}
]}.
```

### Enterprise Node (Behind Corporate Firewall)

Configuration for nodes in restrictive corporate environments:

```erlang
{macula, [
    {nat, [
        {detection, [
            {auto_detect, true},
            {timeout, 10000}  % Longer timeout for slow proxies
        ]},
        {hole_punch, [
            {enabled, true},
            {max_attempts, 5}  % More attempts needed
        ]},
        {relay, [
            {enabled, false}
        ]},
        {connection, [
            {prefer_direct, false},  % Usually need relay
            {allow_relay, true},
            {timeout, 30000}
        ]}
    ]}
]}.
```

### Mobile Device

Configuration for mobile devices with changing networks:

```erlang
{macula, [
    {nat, [
        {detection, [
            {auto_detect, true},
            {refresh_interval, 60000}  % Frequent refresh for IP changes
        ]},
        {cache, [
            {ttl, 120}  % Shorter TTL for mobile
        ]},
        {connection, [
            {timeout, 20000},
            {max_retries, 5}
        ]}
    ]}
]}.
```

---

## Monitoring & Metrics

### Available Metrics

```erlang
%% Get NAT system metrics
Metrics = macula_nat_system:metrics().

%% Returns:
#{
    detection => #{
        total => 150,
        successful => 145,
        failed => 5,
        avg_latency_ms => 250
    },
    cache => #{
        size => 500,
        hits => 10000,
        misses => 200,
        hit_ratio => 0.98
    },
    punch => #{
        attempts => 1000,
        successful => 850,
        failed => 150,
        success_rate => 0.85
    },
    relay => #{
        active_sessions => 25,
        bytes_relayed => 1073741824,
        sessions_total => 500
    }
}
```

### Prometheus Metrics

If using Prometheus exporter:

```
# NAT detection
macula_nat_detection_total{status="success"} 145
macula_nat_detection_total{status="failed"} 5
macula_nat_detection_latency_seconds_bucket{le="0.5"} 140

# Cache
macula_nat_cache_size 500
macula_nat_cache_hits_total 10000
macula_nat_cache_misses_total 200

# Hole punching
macula_nat_punch_attempts_total{result="success"} 850
macula_nat_punch_attempts_total{result="failed"} 150

# Relay
macula_nat_relay_sessions_active 25
macula_nat_relay_bytes_total 1073741824
```

---

## Troubleshooting

### Common Configuration Issues

| Issue | Cause | Solution |
|-------|-------|----------|
| Detection always fails | No reachable observers | Ensure gateway/bootstrap is reachable |
| Punch always fails | Symmetric NAT on both sides | Enable relay fallback |
| High relay usage | Aggressive firewalls | Adjust `strategy_order` |
| Cache misses | TTL too short | Increase `cache.ttl` |
| Memory growth | Cache too large | Reduce `cache.max_entries` |

### Debug Configuration

Enable verbose logging for troubleshooting:

```erlang
%% In sys.config
{kernel, [
    {logger_level, debug},
    {logger, [
        {handler, default, logger_std_h, #{
            formatter => {logger_formatter, #{
                template => [time, " ", level, " ", mfa, ":", line, " ", msg, "\n"]
            }}
        }}
    ]}
]}.
```

Or at runtime:

```erlang
logger:set_module_level(macula_nat_detector, debug).
logger:set_module_level(macula_nat_cache, debug).
logger:set_module_level(macula_nat_coordinator, debug).
logger:set_module_level(macula_relay_node, debug).
```

---

## See Also

- [NAT Types Explained](NAT_TYPES_EXPLAINED.md) - Background on NAT types
- [NAT Traversal Developer Guide](NAT_TRAVERSAL_DEVELOPER_GUIDE.md) - API usage
- [NAT Architecture Diagrams](NAT_ARCHITECTURE.md) - Visual architecture documentation
- [NAT_TRAVERSAL_ROADMAP.md](../../architecture/NAT_TRAVERSAL_ROADMAP.md) - Implementation roadmap
