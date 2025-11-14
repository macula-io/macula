# GenServer API Design - Phase 1

## Date: 2025-01-14

## Overview

Detailed API design for the OTP supervision tree refactoring of `macula_connection.erl`.

## Supervision Tree Structure

```erlang
macula_connection_sup
  Strategy: one_for_all
  Intensity: 3 restarts in 10 seconds

  Children (start order):
    1. macula_connection_manager    (connection lifecycle)
    2. macula_pubsub_handler        (pub/sub operations)
    3. macula_rpc_handler           (RPC operations)
    4. macula_advertisement_manager (DHT advertisements)
```

**Rationale for one_for_all**: If the connection manager dies, all other processes become invalid because they depend on the QUIC connection and stream. All must restart together with clean state.

---

## 1. macula_connection_sup (Supervisor)

### Module: `macula_connection_sup.erl`

### Responsibility
Supervise the connection subsystem and ensure proper restart behavior.

### API

```erlang
-module(macula_connection_sup).
-behaviour(supervisor).

%% API
-export([start_link/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

%% @doc Start the connection supervisor
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(Url, Opts) ->
    supervisor:start_link(?MODULE, {Url, Opts}).

%% @doc Stop the connection supervisor
-spec stop(pid()) -> ok.
stop(Sup) ->
    exit(Sup, shutdown),
    ok.

init({Url, Opts}) ->
    %% Configuration
    SupFlags = #{
        strategy => one_for_all,
        intensity => 3,
        period => 10
    },

    %% Child specifications
    ChildSpecs = [
        #{
            id => connection_manager,
            start => {macula_connection_manager, start_link, [Url, Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        #{
            id => pubsub_handler,
            start => {macula_pubsub_handler, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        #{
            id => rpc_handler,
            start => {macula_rpc_handler, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        #{
            id => advertisement_manager,
            start => {macula_advertisement_manager, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

### Registration Strategy

Option 1: Named registration
```erlang
%% In each child's start_link:
gen_server:start_link({local, ?MODULE}, ?MODULE, Args, [])
```

Option 2: Via tuple (allows multiple instances)
```erlang
%% Store supervisor PID in opts, children register via tuple
gen_server:start_link({via, ?MODULE, {SupPid, Name}}, ...)
```

**Recommendation**: Use named registration (Option 1) initially for simplicity. Most clients will only need one connection per node.

---

## 2. macula_connection_manager (GenServer)

### Module: `macula_connection_manager.erl`

### Responsibility
Manage QUIC connection lifecycle: connect, send messages, receive data, handle disconnection.

### State Structure

```erlang
-record(manager_state, {
    url :: binary(),
    opts :: map(),
    realm :: binary(),
    node_id :: binary(),

    %% QUIC connection
    connection :: undefined | pid(),
    stream :: undefined | pid(),
    status :: connecting | connected | disconnected | error,

    %% Message receive buffer
    recv_buffer :: binary(),

    %% Supervision
    sup_pid :: pid()
}).
```

### Public API

```erlang
-module(macula_connection_manager).
-behaviour(gen_server).

%% API
-export([
    start_link/2,
    send_message/2,
    send_message_raw/3,
    get_status/0,
    get_connection_info/0
]).

%% @doc Send a protocol message (encodes automatically)
-spec send_message(atom(), map()) -> ok | {error, term()}.
send_message(Type, Msg) ->
    gen_server:call(?MODULE, {send_message, Type, Msg}, 5000).

%% @doc Send raw encoded message on a specific stream
-spec send_message_raw(atom(), map(), pid()) -> ok | {error, term()}.
send_message_raw(Type, Msg, Stream) ->
    gen_server:call(?MODULE, {send_message_raw, Type, Msg, Stream}, 5000).

%% @doc Get current connection status
-spec get_status() -> connecting | connected | disconnected | error.
get_status() ->
    gen_server:call(?MODULE, get_status, 5000).

%% @doc Get connection info (for debugging)
-spec get_connection_info() -> #{
    status := atom(),
    url := binary(),
    node_id := binary(),
    connection := pid() | undefined,
    stream := pid() | undefined
}.
get_connection_info() ->
    gen_server:call(?MODULE, get_connection_info, 5000).
```

### Internal Callbacks

```erlang
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% @doc Initialize connection manager
init({Url, Opts}) ->
    %% Extract configuration
    {Host, Port} = macula_connection_utils:parse_url(Url),
    Realm = maps:get(realm, Opts),
    NodeId = maps:get(node_id, Opts, macula_connection_utils:generate_node_id()),

    State = #manager_state{
        url = Url,
        opts = Opts#{host => Host, port => Port},
        realm = Realm,
        node_id = NodeId,
        connection = undefined,
        stream = undefined,
        status = connecting,
        recv_buffer = <<>>,
        sup_pid = maps:get(sup_pid, Opts)
    },

    %% Initiate connection asynchronously
    self() ! connect,

    {ok, State}.

%% @doc Handle send_message call
handle_call({send_message, Type, Msg}, _From, State) ->
    case State#manager_state.status of
        connected ->
            Binary = macula_protocol_encoder:encode(Type, Msg),
            case macula_quic:send(State#manager_state.stream, Binary) of
                ok -> {reply, ok, State};
                {error, Reason} -> {reply, {error, Reason}, State}
            end;
        Status ->
            {reply, {error, {not_connected, Status}}, State}
    end;

handle_call({send_message_raw, Type, Msg, Stream}, _From, State) ->
    Binary = macula_protocol_encoder:encode(Type, Msg),
    Result = macula_quic:send(Stream, Binary),
    {reply, Result, State};

handle_call(get_status, _From, State) ->
    {reply, State#manager_state.status, State};

handle_call(get_connection_info, _From, State) ->
    Info = #{
        status => State#manager_state.status,
        url => State#manager_state.url,
        node_id => State#manager_state.node_id,
        connection => State#manager_state.connection,
        stream => State#manager_state.stream
    },
    {reply, Info, State}.

%% @doc Handle incoming QUIC data
handle_info({quic, Data, Stream, _Props}, State) when is_binary(Data) ->
    %% Validate this is our stream
    case Stream =:= State#manager_state.stream of
        true ->
            %% Append to buffer
            Buffer = <<(State#manager_state.recv_buffer)/binary, Data/binary>>,

            %% Decode messages
            {Messages, RemainingBuffer} = macula_protocol_decoder:decode_messages(Buffer, []),

            %% Route each message to appropriate handler
            lists:foreach(fun(Msg) ->
                route_message(Msg, State)
            end, Messages),

            {noreply, State#manager_state{recv_buffer = RemainingBuffer}};
        false ->
            %% Unknown stream - log and ignore
            ?LOG_WARNING("[~s] Received data from unknown stream: ~p",
                        [State#manager_state.node_id, Stream]),
            {noreply, State}
    end;

handle_info(connect, State) ->
    case do_connect(State) of
        {ok, State2} ->
            {noreply, State2};
        {error, Reason} ->
            ?LOG_ERROR("Connection failed: ~p, retrying in 5s", [Reason]),
            erlang:send_after(5000, self(), connect),
            {noreply, State#manager_state{status = error}}
    end.
```

### Message Routing

```erlang
%% @doc Route decoded message to appropriate handler
-spec route_message({atom(), map()}, #manager_state{}) -> ok.
route_message({publish, Msg}, _State) ->
    macula_pubsub_handler:handle_publish(Msg);

route_message({subscribe, Msg}, _State) ->
    macula_pubsub_handler:handle_subscribe(Msg);

route_message({puback, Msg}, _State) ->
    macula_pubsub_handler:handle_puback(Msg);

route_message({call, Msg}, _State) ->
    macula_rpc_handler:handle_call(Msg);

route_message({reply, Msg}, _State) ->
    macula_rpc_handler:handle_reply(Msg);

route_message({rpc_route, Msg}, _State) ->
    macula_rpc_handler:handle_rpc_route(Msg);

route_message({find_value_reply, Msg}, _State) ->
    %% Could be for RPC or PubSub - let both check
    macula_rpc_handler:handle_find_value_reply(Msg),
    macula_pubsub_handler:handle_find_value_reply(Msg);

route_message({store_reply, Msg}, _State) ->
    macula_advertisement_manager:handle_store_reply(Msg);

route_message({pong, _Msg}, _State) ->
    %% Keepalive - ignore
    ok;

route_message(Other, State) ->
    ?LOG_WARNING("[~s] Unknown message type: ~p",
                [State#manager_state.node_id, Other]).
```

---

## 3. macula_pubsub_handler (GenServer)

### Module: `macula_pubsub_handler.erl`

### Responsibility
Manage subscriptions, route incoming publish messages to callbacks, handle mesh-wide pub/sub with DHT discovery.

### State Structure

```erlang
-record(pubsub_state, {
    node_id :: binary(),
    realm :: binary(),

    %% Local subscriptions: #{SubRef => {Topic, Callback}}
    subscriptions :: #{reference() => {binary(), fun((map()) -> ok)}},

    %% Pending publish acknowledgments (QoS 1)
    %% #{MsgId => {Topic, Payload, QoS, RetryCount, TimerRef}}
    pending_pubacks :: #{binary() => {binary(), binary(), integer(), integer(), reference()}},

    %% Advertised subscriptions in DHT
    %% #{Topic => #{sub_ref, ttl, timer_ref}}
    advertised_subscriptions :: #{binary() => #{
        sub_ref := reference(),
        ttl := pos_integer(),
        timer_ref := reference()
    }},

    %% Pending DHT subscriber queries
    %% #{MsgId => {Topic, Payload, Qos, Opts}}
    pending_subscriber_queries :: #{binary() => {binary(), binary(), integer(), map()}},

    %% Message ID counter for this handler
    msg_id_counter :: non_neg_integer(),

    %% Topic matching configuration
    topic_separator :: binary(),
    topic_wildcard_single :: binary(),
    topic_wildcard_multi :: binary(),

    %% Service registry for subscriber discovery
    service_registry :: macula_service_registry:registry()
}).
```

### Public API

```erlang
-module(macula_pubsub_handler).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    subscribe/2,
    unsubscribe/1,
    publish/3,

    %% Internal - called by connection_manager
    handle_publish/1,
    handle_puback/1,
    handle_find_value_reply/1
]).

%% @doc Subscribe to a topic
-spec subscribe(binary(), fun((map()) -> ok)) -> {ok, reference()} | {error, term()}.
subscribe(Topic, Callback) ->
    gen_server:call(?MODULE, {subscribe, Topic, Callback}, 5000).

%% @doc Unsubscribe from a topic
-spec unsubscribe(reference()) -> ok | {error, term()}.
unsubscribe(SubRef) ->
    gen_server:call(?MODULE, {unsubscribe, SubRef}, 5000).

%% @doc Publish a message
-spec publish(binary(), map() | binary(), map()) -> ok | {error, term()}.
publish(Topic, Data, Opts) ->
    gen_server:call(?MODULE, {publish, Topic, Data, Opts}, 5000).

%% @doc Handle incoming publish message (called by connection_manager)
-spec handle_publish(map()) -> ok.
handle_publish(Msg) ->
    gen_server:cast(?MODULE, {handle_publish, Msg}).

%% @doc Handle PUBACK acknowledgment
-spec handle_puback(map()) -> ok.
handle_puback(Msg) ->
    gen_server:cast(?MODULE, {handle_puback, Msg}).

%% @doc Handle FIND_VALUE_REPLY for subscriber discovery
-spec handle_find_value_reply(map()) -> ok.
handle_find_value_reply(Msg) ->
    gen_server:cast(?MODULE, {handle_find_value_reply, Msg}).
```

### Implementation Notes

**Subscribe Flow**:
1. Client calls `subscribe(Topic, Callback)`
2. Store subscription in state
3. Send SUBSCRIBE message via connection_manager
4. Advertise subscription in DHT
5. Schedule re-advertisement timer

**Publish Flow**:
1. Client calls `publish(Topic, Data, Opts)`
2. Send PUBLISH message via connection_manager (local gateway)
3. If QoS 1, track in pending_pubacks with timeout
4. Async: Discover remote subscribers via DHT
5. Send PUBLISH to each remote subscriber via connection pool

**Incoming Publish Flow**:
1. Connection manager receives {publish, Msg}
2. Routes to `handle_publish/1`
3. Find matching local subscriptions (pattern matching)
4. Spawn callback for each match
5. If QoS 1, send PUBACK

---

## 4. macula_rpc_handler (GenServer)

### Module: `macula_rpc_handler.erl`

### Responsibility
Handle RPC calls with provider discovery, failover, and timeout management.

### State Structure

```erlang
-record(rpc_state, {
    node_id :: binary(),
    realm :: binary(),

    %% Pending RPC calls: #{CallId => CallInfo}
    %% CallInfo: {From, TimerRef} | {From, TimerRef, FailoverContext}
    pending_calls :: #{binary() => term()},

    %% Pending DHT queries for service discovery
    %% #{ServiceKey => {From, Procedure, Args, Opts, Registry, TimerRef}}
    pending_queries :: #{binary() => term()},

    %% Service registry for provider discovery
    service_registry :: macula_service_registry:registry(),

    %% Provider selection state
    provider_selector_state :: #{
        strategy := atom(),
        counters := map()
    },

    %% Message ID counter
    msg_id_counter :: non_neg_integer()
}).
```

### Public API

```erlang
-module(macula_rpc_handler).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    call/3,

    %% Internal - called by connection_manager
    handle_call/1,
    handle_reply/1,
    handle_rpc_route/1,
    handle_find_value_reply/1
]).

%% @doc Make an RPC call
-spec call(binary(), map() | list(), map()) -> {ok, term()} | {error, term()}.
call(Procedure, Args, Opts) ->
    Timeout = maps:get(timeout, Opts, 30000),
    gen_server:call(?MODULE, {call, Procedure, Args, Opts}, Timeout + 1000).

%% @doc Handle incoming RPC call (for local services)
-spec handle_call(map()) -> ok.
handle_call(Msg) ->
    gen_server:cast(?MODULE, {handle_call, Msg}).

%% @doc Handle RPC reply
-spec handle_reply(map()) -> ok.
handle_reply(Msg) ->
    gen_server:cast(?MODULE, {handle_reply, Msg}).

%% @doc Handle routed RPC message
-spec handle_rpc_route(map()) -> ok.
handle_rpc_route(Msg) ->
    gen_server:cast(?MODULE, {handle_rpc_route, Msg}).

%% @doc Handle FIND_VALUE_REPLY for provider discovery
-spec handle_find_value_reply(map()) -> ok.
handle_find_value_reply(Msg) ->
    gen_server:cast(?MODULE, {handle_find_value_reply, Msg}).
```

### Implementation Notes

**Call Flow**:
1. Client calls `call(Procedure, Args, Opts)`
2. Check service_registry for providers
3. If cache hit: Select provider and send CALL
4. If cache miss: Send FIND_VALUE to DHT
5. Track pending call with timeout timer
6. On reply: Cancel timer, return result to client
7. On timeout: Try next provider (failover) or return error

**Failover Context**:
```erlang
#{
    procedure := Procedure,
    args := Args,
    opts := Opts,
    all_providers := [Provider1, Provider2, ...],
    excluded_providers := [FailedProvider1, ...],
    attempt := AttemptNumber
}
```

---

## 5. macula_advertisement_manager (GenServer)

### Module: `macula_advertisement_manager.erl`

### Responsibility
Manage service and subscription advertisements in DHT with periodic re-advertisement.

### State Structure

```erlang
-record(advertisement_state, {
    node_id :: binary(),
    realm :: binary(),
    url :: binary(),  %% This node's endpoint

    %% Advertised services
    %% #{Procedure => #{handler, metadata, ttl, timer_ref}}
    advertised_services :: #{binary() => #{
        handler := fun((term()) -> {ok, term()} | {error, term()}),
        metadata := map(),
        ttl := pos_integer(),
        timer_ref := reference()
    }},

    %% Service registry reference
    service_registry :: macula_service_registry:registry()
}).
```

### Public API

```erlang
-module(macula_advertisement_manager).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    advertise/3,
    unadvertise/1,

    %% Internal - called by connection_manager
    handle_store_reply/1
]).

%% @doc Advertise a service
-spec advertise(binary(), fun((term()) -> term()), map()) ->
    {ok, reference()} | {error, term()}.
advertise(Procedure, Handler, Opts) ->
    gen_server:call(?MODULE, {advertise, Procedure, Handler, Opts}, 5000).

%% @doc Stop advertising a service
-spec unadvertise(binary()) -> ok | {error, term()}.
unadvertise(Procedure) ->
    gen_server:call(?MODULE, {unadvertise, Procedure}, 5000).

%% @doc Handle STORE_REPLY from DHT
-spec handle_store_reply(map()) -> ok.
handle_store_reply(Msg) ->
    gen_server:cast(?MODULE, {handle_store_reply, Msg}).
```

### Implementation Notes

**Advertise Flow**:
1. Client calls `advertise(Procedure, Handler, Opts)`
2. Register handler in local service_registry
3. Register handler with local gateway (if available)
4. Send STORE to DHT with service info
5. Schedule re-advertisement timer (TTL - 60 seconds)

**Re-advertisement**:
1. Timer fires: `{readvertise, Procedure}`
2. Send STORE to DHT again
3. Schedule next re-advertisement

---

## Inter-GenServer Communication Patterns

### 1. Pub/Sub Handler → Connection Manager

```erlang
%% Send PUBLISH message
macula_connection_manager:send_message(publish, PublishMsg)

%% Send to remote subscriber
macula_connection_manager:send_message_raw(publish, PublishMsg, Stream)
```

### 2. RPC Handler → Connection Manager

```erlang
%% Send CALL message
macula_connection_manager:send_message(call, CallMsg)

%% Send FIND_VALUE query
macula_connection_manager:send_message(find_value, FindValueMsg)
```

### 3. Advertisement Manager → Connection Manager

```erlang
%% Send STORE message for service advertisement
macula_connection_manager:send_message(store, StoreMsg)
```

### 4. Connection Manager → All Handlers (Message Routing)

```erlang
%% Incoming messages routed via cast
macula_pubsub_handler:handle_publish(Msg)
macula_rpc_handler:handle_reply(Msg)
macula_advertisement_manager:handle_store_reply(Msg)
```

---

## Shared Resources

### 1. Connection Pool

Still managed as a utility module, but now shared across handlers:

```erlang
%% In pubsub_handler state:
endpoint_connections :: #{binary() => connection_info()}

%% Same in rpc_handler state (or make it a separate GenServer?)
```

**Decision Point**: Should connection pool be a separate GenServer?

**Recommendation**: Keep as utility module initially. Connections are short-lived context, not long-lived state. If pool cleanup becomes an issue, promote to GenServer later.

### 2. Service Registry

Currently in rpc_handler state, but pub/sub also needs it for subscriber discovery.

**Options**:
1. Duplicate in both states (inconsistent)
2. Pass between handlers (coupling)
3. Make it a separate GenServer (complexity)
4. Use ETS table (shared memory)

**Recommendation**: Use ETS table managed by connection_manager, shared by RPC and PubSub handlers.

```erlang
%% In connection_manager:init/1
ets:new(macula_service_cache, [named_table, public, set])

%% In rpc_handler and pubsub_handler
%% Read/write to ets:macula_service_cache
```

---

## Migration Strategy

### Phase 1: Create Modules (Week 1)

1. Create supervisor with empty children
2. Create connection_manager with basic connect/send
3. Create empty handler modules with stub APIs
4. Compile and basic integration test

### Phase 2: Extract Connection Manager (Week 1)

1. Move connection lifecycle from macula_connection
2. Move message send/receive logic
3. Implement message routing to handlers
4. Add tests

### Phase 3: Extract PubSub Handler (Week 2)

1. Move subscription management
2. Move pub/sub message handling
3. Move DHT advertisement logic
4. Add tests

### Phase 4: Extract RPC Handler (Week 2)

1. Move RPC call logic
2. Move provider selection
3. Move failover logic
4. Add tests

### Phase 5: Extract Advertisement Manager (Week 1)

1. Move service advertisement
2. Move re-advertisement timers
3. Add tests

### Phase 6: Integration (Week 1)

1. Wire up all handlers
2. Update public API to route through supervisor
3. End-to-end testing
4. Performance testing

### Phase 7: Cleanup (Week 1)

1. Remove old macula_connection module
2. Update documentation
3. Update CLAUDE.md

---

## API Compatibility Layer

During migration, maintain backward compatibility:

```erlang
-module(macula_connection).

%% Legacy API - delegates to new architecture
-export([start_link/2, stop/1, publish/3, subscribe/3, call/3]).

start_link(Url, Opts) ->
    %% Start supervisor instead
    macula_connection_sup:start_link(Url, Opts).

publish(Client, Topic, Data) ->
    macula_pubsub_handler:publish(Topic, Data, #{}).

subscribe(Client, Topic, Callback) ->
    macula_pubsub_handler:subscribe(Topic, Callback).

call(Client, Procedure, Args) ->
    macula_rpc_handler:call(Procedure, Args, #{}).
```

---

## Testing Strategy

### Unit Tests

Each GenServer tested in isolation:
- Mock connection_manager for handler tests
- Mock handlers for connection_manager tests
- Test timer behavior with controlled time

### Integration Tests

Test inter-GenServer communication:
- Full supervision tree startup/shutdown
- Message routing end-to-end
- Restart behavior (one_for_all)

### Property-Based Tests

- Timer re-advertisement properties
- Failover behavior properties
- Message ordering properties

---

## Next Steps

1. Review this design
2. Create module skeletons
3. Start with connection_manager extraction
4. Iterate with tests

**Estimated Timeline**: 9 weeks total (per original plan)
