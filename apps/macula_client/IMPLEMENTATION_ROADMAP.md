# Macula SDK Implementation Roadmap

**Status:** Foundation complete - Ready for implementation

**Created:** 2025-11-09

---

## ✅ Completed: Foundation (Phase 0)

- [x] Created Erlang SDK skeleton in `macula/apps/macula_sdk/`
- [x] Defined public API with comprehensive documentation
- [x] Created OTP application structure (app, supervisor)
- [x] Implemented client connection manager skeleton
- [x] Added to macula release in `rebar.config`
- [x] Verified compilation succeeds
- [x] Archived legacy WAMP SDK (`macula-energy-mesh-poc/system/macula_sdk_wamp_legacy/`)
- [x] Updated organization CLAUDE.md

---

## Phase 1: HTTP/3 Connection (Weeks 1-2)

**Goal:** Establish HTTP/3 (QUIC) connections to Macula mesh nodes

### Tasks

- [ ] Integrate with `macula_quic` for QUIC transport
- [ ] Implement connection establishment in `macula_sdk_client`
- [ ] Add connection state management
- [ ] Implement basic error handling
- [ ] Add connection timeout logic
- [ ] Write unit tests for connection lifecycle

### Deliverables

```erlang
%% Basic connection works
{ok, Client} = macula_sdk:connect(<<"https://mesh.local:443">>, #{
    realm => <<"my.realm">>
}).
```

---

## Phase 2: Pub/Sub Operations (Weeks 3-4)

**Goal:** Enable publish and subscribe operations

### Tasks

- [ ] Implement publish in `macula_sdk_pubsub` module (new)
- [ ] Implement subscribe with callback support
- [ ] Add subscription registry/manager
- [ ] Handle unsubscribe operations
- [ ] Implement topic validation (no IDs in topics!)
- [ ] Add pub/sub message serialization (likely JSON via `jason`)
- [ ] Write unit tests for pub/sub

### Deliverables

```erlang
%% Publish works
ok = macula_sdk:publish(Client, <<"my.app.events">>, #{data => <<"hello">>}).

%% Subscribe works
{ok, SubRef} = macula_sdk:subscribe(Client, <<"my.app.events">>,
    fun(Event) -> io:format("Got: ~p~n", [Event]) end).
```

---

## Phase 3: RPC Operations (Weeks 5-6)

**Goal:** Enable synchronous RPC calls

### Tasks

- [ ] Create `macula_sdk_rpc` module
- [ ] Implement request/response correlation (call IDs)
- [ ] Add timeout handling for RPC calls
- [ ] Implement result/error handling
- [ ] Add pending calls tracking
- [ ] Write unit tests for RPC

### Deliverables

```erlang
%% RPC call works
{ok, Result} = macula_sdk:call(Client, <<"my.app.get_user">>, #{
    user_id => <<"123">>
}).
```

---

## Phase 4: Connection Pooling (Weeks 7-8)

**Goal:** Support high-throughput applications with connection pooling

### Tasks

- [ ] Create `macula_sdk_pool` module
- [ ] Implement pool supervisor
- [ ] Add pool checkout/checkin logic
- [ ] Implement load balancing across pool connections
- [ ] Add pool size configuration
- [ ] Handle pool member failures
- [ ] Write unit tests for pooling

### Configuration

```erlang
{ok, Client} = macula_sdk:connect(Url, #{
    realm => Realm,
    pool_size => 10  %% Multiple connections
}).
```

---

## Phase 5: Authentication (Weeks 9-10)

**Goal:** Secure connections with API key and other auth methods

### Tasks

- [ ] Create `macula_sdk_auth` module
- [ ] Implement API key authentication
- [ ] Add authentication during connection handshake
- [ ] Support auth token refresh (if applicable)
- [ ] Add namespace/realm enforcement
- [ ] Consider future: OAuth, mTLS, etc.
- [ ] Write unit tests for auth

### Deliverables

```erlang
{ok, Client} = macula_sdk:connect(Url, #{
    realm => Realm,
    auth => #{api_key => <<"secret-key">>}
}).
```

---

## Phase 6: Reconnection Logic (Weeks 11-12)

**Goal:** Handle network failures gracefully with auto-reconnect

### Tasks

- [ ] Implement connection health monitoring
- [ ] Add exponential backoff for reconnection
- [ ] Handle message queuing during disconnect
- [ ] Replay queued messages on reconnect
- [ ] Re-establish subscriptions after reconnect
- [ ] Add max retry configuration
- [ ] Write unit tests for reconnection scenarios

### Configuration

```erlang
{ok, Client} = macula_sdk:connect(Url, #{
    realm => Realm,
    reconnect => true,
    reconnect_delay => 1000,    %% Initial delay (ms)
    max_retries => 10           %% Or `infinity`
}).
```

---

## Phase 7: Metrics & Telemetry (Weeks 13-14)

**Goal:** Provide observability for SDK operations

### Tasks

- [ ] Add telemetry events for:
  - Connection lifecycle
  - Publish operations
  - Subscribe operations
  - RPC calls
  - Errors and retries
- [ ] Create `macula_sdk_metrics` module
- [ ] Integrate with `:telemetry` library
- [ ] Add example metrics collector
- [ ] Document telemetry events
- [ ] Write tests for metrics

### Events

```erlang
%% Example telemetry events
[:macula_sdk, :connect, :start]
[:macula_sdk, :connect, :stop]
[:macula_sdk, :publish, :success]
[:macula_sdk, :rpc, :error]
```

---

## Phase 8: Documentation & Examples (Weeks 15-16)

**Goal:** Comprehensive docs and working examples

### Tasks

- [ ] Generate ExDoc documentation
- [ ] Create tutorial: "Hello World with Macula SDK"
- [ ] Create example: Chat application
- [ ] Create example: Event-sourced system
- [ ] Add troubleshooting guide
- [ ] Write integration tests
- [ ] Prepare for Hex.pm publishing

---

## Future Enhancements

### Multi-Language Support

- [ ] Python wrapper/client
- [ ] JavaScript/TypeScript client
- [ ] Go client
- [ ] Rust client

### Advanced Features

- [ ] Stream processing APIs
- [ ] Binary payload support (not just JSON)
- [ ] Compression (gzip, zstd)
- [ ] Message batching for high throughput
- [ ] Circuit breaker pattern
- [ ] Rate limiting support

---

## Development Principles

Following the patterns from `macula-energy-mesh-poc/guides/implementation-guidelines.md`:

1. ✅ **Event Design:** Business-meaningful, not CRUD
2. ✅ **Topic Design:** Event types in topics, IDs in payloads
3. ✅ **Idiomatic Erlang:** Pattern matching, OTP behaviors
4. ✅ **Testing:** Unit tests for every module
5. ✅ **Documentation:** Comprehensive @doc for all public functions

---

## Current Structure

```
macula/apps/macula_sdk/
├── src/
│   ├── macula_sdk.erl           # ✅ Main API (skeleton complete)
│   ├── macula_sdk_app.erl       # ✅ Application behavior
│   ├── macula_sdk_sup.erl       # ✅ Top supervisor
│   ├── macula_sdk_client.erl    # ✅ Client manager (skeleton)
│   ├── macula_sdk_pool.erl      # ⏳ Connection pool (todo)
│   ├── macula_sdk_pubsub.erl    # ⏳ Pub/sub ops (todo)
│   ├── macula_sdk_rpc.erl       # ⏳ RPC ops (todo)
│   └── macula_sdk_auth.erl      # ⏳ Authentication (todo)
├── test/
│   └── (tests to be added)
├── README.md                    # ✅ Complete
└── IMPLEMENTATION_ROADMAP.md    # ✅ This file
```

---

## Notes

- **Language Choice:** Erlang for cross-compatibility (works in Erlang & Elixir)
- **Transport:** HTTP/3 (QUIC) via `macula_quic`
- **No WAMP:** Clean break from legacy architecture
- **Integration:** Part of `macula` release for tight coupling with platform

---

**Next Step:** Begin Phase 1 - HTTP/3 Connection implementation
