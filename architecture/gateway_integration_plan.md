# Gateway Module Integration Plan (Phase 7)

**Status:** 🔄 Ready to Execute
**Date:** 2025-01-13
**Purpose:** Integrate the three extracted modules into `macula_gateway.erl`
**Related:** `macula_gateway_behaviors.md`, `CLAUDE.md`

## Overview

Phases 2-4, 6 successfully extracted three focused modules with comprehensive tests:
- ✅ `macula_gateway_client_manager.erl` (235 LOC, 24 tests)
- ✅ `macula_gateway_pubsub.erl` (280 LOC, 31 tests)
- ✅ `macula_gateway_rpc.erl` (215 LOC, 20 tests)
- ✅ `macula_gateway_sup.erl` (113 LOC, 24 tests)

**Problem:** The old implementation code is STILL in `macula_gateway.erl` and the extracted modules are not being used!

**This Phase:** Actually integrate the modules and remove duplicate code.

---

## Phase 7 Breakdown

### Phase 7a: Analyze Current State ✅ COMPLETE

**Status:** ✅ DONE

**What We Found:**
- Lines 364-394: Client lifecycle (duplicates client_manager)
- Lines 397-428: Pub/Sub operations (duplicates pubsub)
- Lines 431-442: RPC routing (duplicates rpc)
- State contains duplicate fields: `clients`, `subscriptions`, `stream_subscriptions`, `registrations`

**Documents Created:**
- `architecture/macula_gateway_behaviors.md` - Complete responsibility catalog
- `architecture/gateway_integration_plan.md` - This document

---

### Phase 7b: Choose Integration Strategy

**Status:** ⏳ NEXT STEP

**Two Approaches:**

#### Option A: Big Bang Integration (Recommended)
**What:** Integrate all three modules in one go
**Pros:**
- Faster overall
- Single set of integration tests
- All duplicate code removed at once
- Cleaner git history

**Cons:**
- Larger changeset
- More complex debugging if issues arise
- Higher risk

**Estimated Time:** 3-4 days

#### Option B: Incremental Integration
**What:** Integrate one module at a time (client_manager → pubsub → rpc)
**Pros:**
- Lower risk per step
- Easier to debug issues
- Can verify each module independently

**Cons:**
- Takes longer overall
- State partially split across old/new
- Multiple rounds of testing
- More complex intermediate states

**Estimated Time:** 5-7 days (1-2 days per module + integration)

---

## Recommended Strategy: Option A (Big Bang)

**Rationale:**
1. Modules are already tested in isolation (99 tests passing)
2. Dependencies between modules are well understood
3. All three responsibilities are cleanly separated
4. Faster time to completion

---

## Phase 7c: Integration Implementation Plan

### Step 1: Update State Record

**File:** `src/macula_gateway.erl`
**Lines:** 52-63

**Current State:**
```erlang
-record(state, {
    port :: inet:port_number(),
    realm :: binary(),
    node_id :: binary(),
    listener :: pid() | undefined,
    clients :: #{pid() => client_info()},              % REMOVE
    subscriptions :: #{binary() => [pid()]},           % REMOVE
    stream_subscriptions :: #{pid() => [binary()]},    % REMOVE
    registrations :: #{binary() => pid()},             % REMOVE
    mesh_connections :: #{binary() => mesh_connection_info()},
    client_streams :: #{binary() => pid()}
}).
```

**New State:**
```erlang
-record(state, {
    port :: inet:port_number(),
    realm :: binary(),
    node_id :: binary(),
    listener :: pid() | undefined,
    supervisor :: pid(),                                % NEW: supervisor PID
    client_manager :: pid(),                            % NEW: client_manager PID
    pubsub :: pid(),                                    % NEW: pubsub PID
    rpc :: pid(),                                       % NEW: rpc PID
    mesh_connections :: #{binary() => mesh_connection_info()},
    client_streams :: #{binary() => pid()}
}).
```

---

### Step 2: Update init/1 to Start Supervisor

**File:** `src/macula_gateway.erl`
**Function:** `start_quic_listener/4`
**Lines:** 272-284 (State initialization)

**Current Code:**
```erlang
State = #state{
    port = Port,
    realm = Realm,
    node_id = LocalNodeId,
    listener = Listener,
    clients = #{},
    subscriptions = #{},
    stream_subscriptions = #{},
    registrations = #{},
    mesh_connections = #{},
    client_streams = #{}
},
```

**New Code:**
```erlang
%% Start supervisor with configuration
Config = #{
    port => Port,
    realm => Realm,
    node_id => LocalNodeId
},

{ok, SupPid} = macula_gateway_sup:start_link(Config),

%% Get child PIDs from supervisor
{ok, ClientMgrPid} = macula_gateway_sup:get_client_manager(SupPid),
{ok, PubSubPid} = macula_gateway_sup:get_pubsub(SupPid),
{ok, RpcPid} = macula_gateway_sup:get_rpc(SupPid),

State = #state{
    port = Port,
    realm = Realm,
    node_id = LocalNodeId,
    listener = Listener,
    supervisor = SupPid,
    client_manager = ClientMgrPid,
    pubsub = PubSubPid,
    rpc = RpcPid,
    mesh_connections = #{},
    client_streams = #{}
},
```

---

### Step 3: Replace Client Lifecycle Code

**File:** `src/macula_gateway.erl`
**Lines:** 364-394

**Current Code (TO REMOVE):**
```erlang
%% Client registered
handle_info({client_connected, ClientPid, ClientInfo}, State) ->
    io:format("Client connected: ~p~n", [ClientInfo]),
    erlang:monitor(process, ClientPid),
    Clients = maps:put(ClientPid, ClientInfo, State#state.clients),
    {noreply, State#state{clients = Clients}};

%% Client disconnected
handle_info({'DOWN', _Ref, process, ClientPid, _Reason}, State) ->
    io:format("Client disconnected: ~p~n", [ClientPid]),
    %% Remove client from all subscriptions
    Subscriptions = maps:map(fun(_Topic, Subscribers) ->
        lists:delete(ClientPid, Subscribers)
    end, State#state.subscriptions),
    %% Remove client registrations
    Registrations = maps:filter(fun(_Proc, Pid) ->
        Pid =/= ClientPid
    end, State#state.registrations),
    %% Remove client
    Clients = maps:remove(ClientPid, State#state.clients),
    {noreply, State#state{
        clients = Clients,
        subscriptions = Subscriptions,
        registrations = Registrations
    }};
```

**New Code (DELEGATED):**
```erlang
%% Client connected
handle_info({client_connected, ClientPid, ClientInfo}, State) ->
    ClientMgr = State#state.client_manager,
    ok = macula_gateway_client_manager:client_connected(ClientMgr, ClientPid, ClientInfo),
    {noreply, State};

%% Client disconnected (monitor DOWN)
handle_info({'DOWN', _Ref, process, ClientPid, _Reason}, State) ->
    %% Delegate to client manager
    ClientMgr = State#state.client_manager,
    PubSub = State#state.pubsub,
    Rpc = State#state.rpc,

    %% Client manager handles cleanup
    macula_gateway_client_manager:client_disconnected(ClientMgr, ClientPid),

    %% Clean up subscriptions in pubsub
    {ok, Topics} = macula_gateway_pubsub:get_stream_topics(PubSub, ClientPid),
    lists:foreach(fun(Topic) ->
        macula_gateway_pubsub:unsubscribe(PubSub, ClientPid, Topic)
    end, Topics),

    %% Clean up RPC registrations
    {ok, Handlers} = macula_gateway_rpc:list_handlers(Rpc),
    lists:foreach(fun({Proc, HandlerPid}) ->
        case HandlerPid =:= ClientPid of
            true -> macula_gateway_rpc:unregister_handler(Rpc, Proc);
            false -> ok
        end
    end, Handlers),

    {noreply, State};
```

---

### Step 4: Replace Pub/Sub Code

**File:** `src/macula_gateway.erl`
**Lines:** 397-428

**Current Code (TO REMOVE):**
```erlang
%% Publish message (from client)
handle_info({publish, FromPid, Topic, Payload}, State) ->
    Subscribers = maps:get(Topic, State#state.subscriptions, []),
    [SubPid ! {event, Topic, Payload} || SubPid <- Subscribers, SubPid =/= FromPid],
    {noreply, State};

%% Subscribe request
handle_info({subscribe, ClientPid, Topic}, State) ->
    Subscribers = maps:get(Topic, State#state.subscriptions, []),
    NewSubscribers = [ClientPid | Subscribers],
    Subscriptions = maps:put(Topic, NewSubscribers, State#state.subscriptions),
    ClientPid ! {subscribed, Topic},
    {noreply, State#state{subscriptions = Subscriptions}};

%% Unsubscribe request
handle_info({unsubscribe, ClientPid, Topic}, State) ->
    Subscribers = maps:get(Topic, State#state.subscriptions, []),
    NewSubscribers = lists:delete(ClientPid, Subscribers),
    Subscriptions = maps:put(Topic, NewSubscribers, State#state.subscriptions),
    ClientPid ! {unsubscribed, Topic},
    {noreply, State#state{subscriptions = Subscriptions}};
```

**New Code (DELEGATED):**
```erlang
%% NOTE: These handle_info clauses should be REMOVED
%% Pub/Sub operations now handled via handle_subscribe/handle_unsubscribe/handle_publish
%% which already delegate to the pubsub module (see lines 1170-1295)
```

**Actually:** Lines 397-428 can be completely removed because:
- Subscribe/unsubscribe are handled via SUBSCRIBE/UNSUBSCRIBE messages (lines 1170-1245)
- Those already need to be updated to use pubsub module
- The `{publish, ...}` message is legacy and should be removed

---

### Step 5: Update Pub/Sub Message Handlers

**File:** `src/macula_gateway.erl`
**Lines:** 1170-1295

**Current Functions:**
- `handle_subscribe/3` (lines 1170-1204)
- `handle_unsubscribe/3` (lines 1206-1245)
- `handle_publish/3` (lines 1247-1295)

**Update Each Function to Delegate:**

**handle_subscribe/3:**
```erlang
handle_subscribe(Stream, SubMsg, State) ->
    Topics = maps:get(<<"topics">>, SubMsg, []),
    PubSub = State#state.pubsub,

    %% Subscribe to each topic
    lists:foreach(fun(Topic) ->
        macula_gateway_pubsub:subscribe(PubSub, Stream, Topic)
    end, Topics),

    {noreply, State}.
```

**handle_unsubscribe/3:**
```erlang
handle_unsubscribe(Stream, UnsubMsg, State) ->
    Topics = maps:get(<<"topics">>, UnsubMsg, []),
    PubSub = State#state.pubsub,

    %% Unsubscribe from each topic
    lists:foreach(fun(Topic) ->
        macula_gateway_pubsub:unsubscribe(PubSub, Stream, Topic)
    end, Topics),

    {noreply, State}.
```

**handle_publish/3:**
```erlang
handle_publish(_PublisherStream, PubMsg, State) ->
    Topic = maps:get(<<"topic">>, PubMsg),
    PubSub = State#state.pubsub,

    %% Get matching subscribers
    {ok, Subscribers} = macula_gateway_pubsub:get_subscribers(PubSub, Topic),

    %% Encode and send to each subscriber
    PubBinary = macula_protocol_encoder:encode(publish, PubMsg),
    lists:foreach(fun(SubscriberStream) ->
        case macula_quic:send(SubscriberStream, PubBinary) of
            ok -> ok;
            {error, Reason} ->
                io:format("[Gateway] Failed to send to stream ~p: ~p~n",
                          [SubscriberStream, Reason])
        end
    end, Subscribers),

    {noreply, State}.
```

---

### Step 6: Replace RPC Handler Code

**File:** `src/macula_gateway.erl`
**Lines:** 310-322 (handle_call), 431-442 (handle_info)

**Current Code (TO REMOVE):**
```erlang
handle_call({register_handler, Procedure, Handler}, _From, State) ->
    Registrations = State#state.registrations,
    NewRegistrations = Registrations#{Procedure => Handler},
    NewState = State#state{registrations = NewRegistrations},
    {reply, ok, NewState};

handle_call({unregister_handler, Procedure}, _From, State) ->
    Registrations = State#state.registrations,
    NewRegistrations = maps:remove(Procedure, Registrations),
    NewState = State#state{registrations = NewRegistrations},
    {reply, ok, NewState};

handle_info({call, FromPid, CallId, Procedure, Args}, State) ->
    case maps:get(Procedure, State#state.registrations, undefined) of
        undefined ->
            FromPid ! {call_error, CallId, <<"wamp.error.no_such_procedure">>};
        HandlerPid ->
            HandlerPid ! {invoke, FromPid, CallId, Procedure, Args}
    end,
    {noreply, State};
```

**New Code (DELEGATED):**
```erlang
handle_call({register_handler, Procedure, Handler}, _From, State) ->
    Rpc = State#state.rpc,
    ok = macula_gateway_rpc:register_handler(Rpc, Procedure, Handler),
    {reply, ok, State};

handle_call({unregister_handler, Procedure}, _From, State) ->
    Rpc = State#state.rpc,
    ok = macula_gateway_rpc:unregister_handler(Rpc, Procedure),
    {reply, ok, State};

%% NOTE: The {call, FromPid, CallId, Procedure, Args} handle_info should be removed
%% RPC calls are now handled via handle_rpc_call/3 (lines 866-935) which needs updating
```

---

### Step 7: Update RPC Call Handlers

**File:** `src/macula_gateway.erl`
**Lines:** 866-935 (handle_rpc_call), 685-747 (handle_rpc_call_routed)

**These functions need to delegate to RPC module:**

**handle_rpc_call/3:**
```erlang
handle_rpc_call(Stream, CallMsg, State) ->
    Procedure = maps:get(<<"procedure">>, CallMsg),
    CallId = maps:get(<<"call_id">>, CallMsg),
    ArgsJson = maps:get(<<"args">>, CallMsg),
    Rpc = State#state.rpc,

    %% Delegate to RPC module
    Args = json:decode(ArgsJson),
    Result = macula_gateway_rpc:call(Rpc, Procedure, Args, #{timeout => 5000}),

    %% Send reply
    Reply = case Result of
        {ok, ResultMap} ->
            #{call_id => CallId, result => encode_json(ResultMap)};
        {error, no_handler} ->
            #{call_id => CallId, error => #{
                code => <<"no_such_procedure">>,
                message => <<"No handler registered for ", Procedure/binary>>
            }};
        {error, Reason} ->
            #{call_id => CallId, error => #{
                code => <<"handler_error">>,
                message => iolist_to_binary(io_lib:format("~p", [Reason]))
            }}
    end,

    ReplyBinary = macula_protocol_encoder:encode(reply, Reply),
    macula_quic:send(Stream, ReplyBinary),
    {noreply, State}.
```

**handle_rpc_call_routed/4:**
```erlang
handle_rpc_call_routed(_Stream, CallMsg, RpcRouteMsg, State) ->
    Procedure = maps:get(<<"procedure">>, CallMsg),
    CallId = maps:get(<<"call_id">>, CallMsg),
    ArgsJson = maps:get(<<"args">>, CallMsg),
    SourceNodeId = maps:get(<<"source_node_id">>, RpcRouteMsg),
    Rpc = State#state.rpc,

    %% Delegate to RPC module
    Args = json:decode(ArgsJson),
    Result = macula_gateway_rpc:call(Rpc, Procedure, Args, #{timeout => 5000}),

    %% Send reply via routing
    Reply = case Result of
        {ok, ResultMap} ->
            #{call_id => CallId, result => encode_json(ResultMap)};
        {error, no_handler} ->
            #{call_id => CallId, error => #{
                code => <<"no_such_procedure">>,
                message => <<"No handler registered for ", Procedure/binary>>
            }};
        {error, Reason} ->
            #{call_id => CallId, error => #{
                code => <<"handler_error">>,
                message => iolist_to_binary(io_lib:format("~p", [Reason]))
            }}
    end,

    send_reply_via_routing(SourceNodeId, Reply, State),
    {noreply, State}.
```

---

### Step 8: Update get_stats

**File:** `src/macula_gateway.erl`
**Lines:** 300-308

**Current Code:**
```erlang
handle_call(get_stats, _From, State) ->
    Stats = #{
        port => State#state.port,
        realm => State#state.realm,
        clients => maps:size(State#state.clients),
        subscriptions => maps:size(State#state.subscriptions),
        registrations => maps:size(State#state.registrations)
    },
    {reply, Stats, State};
```

**New Code:**
```erlang
handle_call(get_stats, _From, State) ->
    ClientMgr = State#state.client_manager,
    PubSub = State#state.pubsub,
    Rpc = State#state.rpc,

    %% Query child modules for their stats
    {ok, AllClients} = macula_gateway_client_manager:get_all_clients(ClientMgr),
    {ok, AllHandlers} = macula_gateway_rpc:list_handlers(Rpc),

    %% Note: pubsub doesn't have a count function yet, might need to add one
    %% For now, we can estimate from client subscriptions

    Stats = #{
        port => State#state.port,
        realm => State#state.realm,
        clients => length(AllClients),
        registrations => length(AllHandlers)
    },
    {reply, Stats, State};
```

---

### Step 9: Update terminate/2

**File:** `src/macula_gateway.erl`
**Lines:** 543-549

**Current Code:**
```erlang
terminate(_Reason, #state{listener = Listener}) ->
    case Listener of
        undefined -> ok;
        _ -> macula_quic:close(Listener)
    end,
    ok.
```

**New Code:**
```erlang
terminate(_Reason, #state{listener = Listener, supervisor = SupPid}) ->
    %% Close listener
    case Listener of
        undefined -> ok;
        _ -> macula_quic:close(Listener)
    end,

    %% Stop supervisor (will stop all children)
    case SupPid of
        undefined -> ok;
        _ -> supervisor:terminate_child(SupPid, macula_gateway_sup)
    end,

    ok.
```

---

## Phase 7d: Integration Testing

**Test Plan:**

### Unit Tests (Already Passing)
- ✅ Client manager: 24 tests
- ✅ Pub/Sub: 31 tests
- ✅ RPC: 20 tests
- ✅ Supervisor: 24 tests

**Total: 99 tests should still pass**

### New Integration Tests

Create `test/macula_gateway_integration_tests.erl`:

**Test Scenarios:**
1. **Gateway starts with supervisor**
   - Verify supervisor started
   - Verify all children running
   - Verify gateway state contains child PIDs

2. **Client lifecycle through gateway**
   - Send client_connected message
   - Verify client_manager tracks client
   - Kill client process
   - Verify cleanup in all modules

3. **Pub/Sub through gateway**
   - Subscribe via SUBSCRIBE message
   - Publish via PUBLISH message
   - Verify delivery to subscribers
   - Unsubscribe and verify no delivery

4. **RPC through gateway**
   - Register handler via register_handler/2
   - Send CALL message
   - Verify handler invoked
   - Verify REPLY sent back

5. **Statistics aggregation**
   - Register clients, subscriptions, handlers
   - Call get_stats
   - Verify counts match module states

6. **Gateway shutdown**
   - Start gateway
   - Verify all modules running
   - Stop gateway
   - Verify all modules stopped

**Estimated Tests:** ~20-25 integration tests

---

## Phase 7e: Documentation Updates

### Update CLAUDE.md

**Section:** Lines 87-120 (Gateway Refactoring)

**Change:**
```markdown
## 🔧 Gateway Refactoring (Phases 2-4, 6-7 COMPLETED - Nov 2025)

**COMPLETED**: Successfully extracted 3 focused modules from `macula_gateway.erl` using TDD, created supervision tree, and INTEGRATED all modules.

**Why?** The gateway module had 6 mixed responsibilities requiring separation:
- ✅ Client lifecycle management - EXTRACTED & INTEGRATED
- ✅ Pub/Sub message routing - EXTRACTED & INTEGRATED
- ✅ RPC handler registration - EXTRACTED & INTEGRATED
- ✅ Supervision tree - CREATED
- ⏳ QUIC listener management (Phase 8 - pending)
- ⏳ Mesh connection pooling (Phase 9 - pending)
- ⏳ DHT query handling (Phase 10 - pending)
- ⏳ RPC routing (Phase 11 - pending)

**Extracted & Integrated Modules (Phases 2-4, 6-7):**
- ✅ `macula_gateway_client_manager.erl` - Client lifecycle (~235 LOC, 24 tests) - INTEGRATED
- ✅ `macula_gateway_pubsub.erl` - Pub/Sub routing with wildcards (~280 LOC, 31 tests) - INTEGRATED
- ✅ `macula_gateway_rpc.erl` - RPC handler management (~215 LOC, 20 tests) - INTEGRATED

**Supervision Tree (Phase 6 - COMPLETED):**
- ✅ `macula_gateway_sup.erl` - Supervises all gateway workers (~113 LOC, 24 tests)

**Integration (Phase 7 - COMPLETED):**
- ✅ Old duplicate code removed from gateway
- ✅ Gateway delegates to child modules
- ✅ State cleaned up (clients, subscriptions, registrations removed)
- ✅ All 99+ tests passing (including integration tests)

**Achievements:**
- Single Responsibility Principle: Each module has one clear purpose
- Idiomatic Erlang: Pattern matching, guards, no deep nesting
- Comprehensive tests: 120+ passing tests, 0 failures
- Fault tolerance: Proper OTP supervision with one_for_all strategy
- Integration verified: All modules work together correctly

**Gateway Status After Phase 7:**
- Lines reduced: 1,340 → ~900 LOC (after removing duplicate code)
- Focused on orchestration, not implementation
- Clean delegation to child modules

**Next Steps:**
- Phase 8: Extract QUIC listener module
- Phase 9: Extract mesh connection pooling
- Phase 10: Extract DHT query handler
- Phase 11: Extract RPC routing module
- Phase 12: Final gateway cleanup (facade pattern)
```

---

## Rollout Plan

### Day 1: State & Init Changes
- Update state record
- Update init/1 to start supervisor
- Run existing tests (should still pass, but with warnings about unused state fields)

### Day 2: Client Manager Integration
- Replace client lifecycle code
- Update handle_info {'DOWN', ...}
- Test client connection/disconnection

### Day 3: Pub/Sub Integration
- Update handle_subscribe/unsubscribe/publish
- Remove old handle_info pub/sub code
- Test pub/sub message flow

### Day 4: RPC Integration
- Update handle_call register/unregister
- Update handle_rpc_call handlers
- Test RPC call flow
- Update get_stats
- Update terminate/2

### Day 5: Integration Testing & Documentation
- Write integration tests
- Run full test suite
- Update CLAUDE.md
- Create PR/commit

---

## Success Criteria

- ✅ All 99 existing tests still pass
- ✅ 20-25 new integration tests pass
- ✅ Gateway state no longer contains clients, subscriptions, registrations
- ✅ Old implementation code removed (lines 364-394, 397-428, 431-442)
- ✅ Gateway delegates all operations to child modules
- ✅ Manual testing: gateway starts, accepts clients, routes messages
- ✅ Documentation updated (CLAUDE.md)

---

## Risk Mitigation

**Risk 1: Breaking existing functionality**
- Mitigation: Keep all existing tests running throughout
- Mitigation: Add integration tests before removing old code
- Mitigation: Use git branches for incremental commits

**Risk 2: State synchronization issues**
- Mitigation: Clear state ownership (child modules own their state)
- Mitigation: Gateway never caches child state
- Mitigation: Always query child modules for current state

**Risk 3: Process monitoring edge cases**
- Mitigation: Test client crash scenarios explicitly
- Mitigation: Verify cleanup across all modules
- Mitigation: Monitor for memory leaks

---

## Next Steps After Phase 7

Once integration is complete:
1. **Phase 8:** Extract QUIC listener module (~2 weeks)
2. **Phase 9:** Extract mesh connection pooling (~3 weeks)
3. **Phase 10:** Extract DHT query handler (~1 week)
4. **Phase 11:** Extract RPC routing module (~2 weeks)
5. **Phase 12:** Final gateway cleanup (~1 week)

**Total Remaining:** ~9 weeks

**Gateway will then be:** A clean facade/orchestrator (~200 LOC) coordinating 8 focused modules
