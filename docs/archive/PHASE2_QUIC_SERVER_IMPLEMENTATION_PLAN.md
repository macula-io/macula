# Phase 2: QUIC Server Extraction - Complete Implementation Plan

## Overview

Extract QUIC transport handling from `macula_gateway.erl` into a proper OTP gen_server (`macula_gateway_quic_server.erl`) under supervision.

**Target**: Reduce gateway from 1,011 LOC → ~600 LOC
**New Module**: `macula_gateway_quic_server.erl` (~400 LOC)

## Why Gen_Server (Not Callback Module)

✅ **Correct OTP Design**:
- One process, one responsibility
- Clean fault isolation
- Proper supervision
- True separation of concerns

❌ **Callback module would**:
- Still couple transport to gateway
- Not provide fault isolation
- Not follow OTP principles

## Implementation Steps (TDD + Idiomatic Erlang)

### Step 1: Create Basic Gen_Server Skeleton (TDD)

#### 1.1 Write Test First
```erlang
%% test/macula_gateway_quic_server_tests.erl
-module(macula_gateway_quic_server_tests).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    Opts = [{port, 9999}, {realm, <<"test">>}],
    {ok, Pid} = macula_gateway_quic_server:start_link(Opts),
    ?assert(is_process_alive(Pid)),
    ok = gen_server:stop(Pid).
```

#### 1.2 Create Minimal Gen_Server
```erlang
-module(macula_gateway_quic_server).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    listener :: pid() | undefined,
    gateway :: pid() | undefined,
    node_id :: binary(),
    port :: inet:port_number(),
    realm :: binary(),
    buffer = <<>> :: binary()
}).

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

init(Opts) ->
    Port = proplists:get_value(port, Opts, 9443),
    Realm = proplists:get_value(realm, Opts, <<"macula.default">>),
    NodeId = get_node_id(Realm, Port),

    State = #state{
        port = Port,
        realm = Realm,
        node_id = NodeId
    },

    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
```

**Run Test**: `rebar3 eunit --module=macula_gateway_quic_server_tests`
**Expected**: PASS ✅

---

### Step 2: Extract Helper Functions (LOW RISK)

#### 2.1 Move Helper Functions from Gateway

Functions to move (lines 934-1004 in gateway):
1. `complete_handshake/1`
2. `accept_streams/1`
3. `register_next_connection/1`
4. `parse_endpoint/1`
5. `resolve_host/2`
6. `get_node_id/1` (from init section)

#### 2.2 Write Tests for Helper Functions
```erlang
parse_endpoint_test() ->
    ?assertEqual({{127,0,0,1}, 9443},
                 macula_gateway_quic_server:parse_endpoint(<<"https://localhost:9443">>)),
    ?assertEqual({{0,0,0,0}, 0},
                 macula_gateway_quic_server:parse_endpoint(undefined)).

resolve_host_test() ->
    {IP, Port} = macula_gateway_quic_server:resolve_host(<<"localhost">>, 8080),
    ?assertEqual(8080, Port),
    ?assert(is_tuple(IP) andalso tuple_size(IP) == 4).
```

#### 2.3 Move Functions to QUIC Server

Pattern matching on function heads (idiomatic):
```erlang
%% @doc Parse endpoint URL to address tuple.
-spec parse_endpoint(undefined | binary()) -> {{byte(), byte(), byte(), byte()}, inet:port_number()}.
parse_endpoint(undefined) ->
    {{0,0,0,0}, 0};
parse_endpoint(Endpoint) when is_binary(Endpoint) ->
    case uri_string:parse(Endpoint) of
        #{host := Host, port := Port} when is_integer(Port) ->
            resolve_host(Host, Port);
        #{host := Host} ->
            resolve_host(Host, 9443);
        _ ->
            io:format("[QuicServer] Invalid endpoint: ~s~n", [Endpoint]),
            {{0,0,0,0}, 0}
    end.

%% @doc Resolve hostname to IP address.
-spec resolve_host(binary(), inet:port_number()) -> {{byte(), byte(), byte(), byte()}, inet:port_number()}.
resolve_host(Host, Port) when is_binary(Host), is_integer(Port) ->
    HostStr = binary_to_list(Host),
    case inet:getaddr(HostStr, inet) of
        {ok, IPTuple} -> {IPTuple, Port};
        {error, Reason} ->
            io:format("[QuicServer] DNS failed for ~s: ~p~n", [Host, Reason]),
            {{127,0,0,1}, Port}
    end.
```

**Export for Testing**:
```erlang
-ifdef(TEST).
-export([parse_endpoint/1, resolve_host/2]).
-endif.
```

**Run Tests**: `rebar3 eunit`
**Expected**: All helper function tests PASS ✅

---

### Step 3: Add QUIC Listener Initialization

#### 3.1 Write Test
```erlang
quic_listener_test() ->
    Opts = [{port, 19443}, {realm, <<"test">>}],
    {ok, Pid} = macula_gateway_quic_server:start_link(Opts),

    %% Verify listener started
    State = sys:get_state(Pid),
    ?assertNotEqual(undefined, State#state.listener),

    gen_server:stop(Pid).
```

#### 3.2 Update `init/1` to Start QUIC Listener

```erlang
init(Opts) ->
    Port = proplists:get_value(port, Opts, 9443),
    Realm = proplists:get_value(realm, Opts, <<"macula.default">>),
    GatewayPid = proplists:get_value(gateway, Opts),  % Will be set by supervisor
    NodeId = get_node_id(Realm, Port),

    %% Start QUIC listener
    case start_quic_listener(Port, NodeId) of
        {ok, Listener} ->
            io:format("[QuicServer] QUIC listener started on port ~p~n", [Port]),
            State = #state{
                listener = Listener,
                gateway = GatewayPid,
                port = Port,
                realm = Realm,
                node_id = NodeId
            },
            {ok, State};
        {error, Reason} ->
            {stop, {quic_listener_failed, Reason}}
    end.

start_quic_listener(Port, NodeId) ->
    {CertFile, KeyFile} = get_tls_certs(),
    ListenOpts = #{
        cert => CertFile,
        key => KeyFile,
        alpn => [<<"h3">>],
        peer_unidi_stream_count => 10
    },
    quicer:listen(integer_to_binary(Port), ListenOpts).
```

**Run Test**: `rebar3 eunit --module=macula_gateway_quic_server_tests`
**Expected**: Listener test PASS ✅

---

### Step 4: Move QUIC Event Handlers (TDD for Each)

#### 4.1 Move `handle_info({quic, new_conn, ...})`

**Test**:
```erlang
new_conn_event_test() ->
    %% Mock QUIC connection event
    {ok, Pid} = start_test_server(),

    %% Simulate new_conn event
    Conn = make_ref(),  % Mock connection
    ConnInfo = #{},
    Pid ! {quic, new_conn, Conn, ConnInfo},

    %% Verify handshake was initiated
    timer:sleep(100),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).
```

**Implementation** (pattern matching on event):
```erlang
%% @doc Handle new QUIC connection.
handle_info({quic, new_conn, Conn, _ConnInfo}, State) ->
    io:format("[QuicServer] New connection established~n"),
    complete_handshake(Conn),
    register_next_connection(State#state.listener),
    {noreply, State};
```

#### 4.2 Move `handle_info({quic, Data, Stream, ...})`

This is the most complex - it decodes messages and needs to route them.

**Pattern**: Decode → Call Gateway for Routing

```erlang
%% @doc Handle incoming QUIC data - decode and route to gateway.
handle_info({quic, Data, Stream, _Flags}, State) when is_binary(Data) ->
    Buffer = State#state.buffer,
    NewBuffer = <<Buffer/binary, Data/binary>>,

    case decode_messages(NewBuffer) of
        {ok, Messages, RemainingBuffer} ->
            route_messages_to_gateway(Messages, Stream, State),
            {noreply, State#state{buffer = RemainingBuffer}};
        {incomplete, _} ->
            {noreply, State#state{buffer = NewBuffer}}
    end;

decode_messages(Buffer) ->
    %% Use macula_protocol_decoder
    macula_protocol_decoder:decode_buffer(Buffer).

route_messages_to_gateway([], _Stream, _State) ->
    ok;
route_messages_to_gateway([{Type, Msg} | Rest], Stream, State) ->
    Gateway = State#state.gateway,
    gen_server:call(Gateway, {route_message, Type, Msg, Stream}),
    route_messages_to_gateway(Rest, Stream, State).
```

#### 4.3 Move Other QUIC Events

Simple delegations:
```erlang
handle_info({quic, new_stream, Stream, StreamProps}, State) ->
    io:format("[QuicServer] New stream opened: ~p~n", [Stream]),
    {noreply, State};

handle_info({quic, peer_needs_streams, _Conn, _StreamType}, State) ->
    io:format("[QuicServer] Peer needs streams~n"),
    {noreply, State};

handle_info({quic, shutdown, Conn, Reason}, State) ->
    io:format("[QuicServer] Connection shutdown: ~p~n", [Reason]),
    {noreply, State};

handle_info({quic, transport_shutdown, Conn, Reason}, State) ->
    io:format("[QuicServer] Transport shutdown: ~p~n", [Reason]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.
```

**Run Tests**: `rebar3 eunit`
**Expected**: All event handler tests PASS ✅

---

### Step 5: Update Gateway to Receive Routed Messages

#### 5.1 Add `handle_call({route_message, ...})` to Gateway

```erlang
%% In macula_gateway.erl
handle_call({route_message, Type, Msg, Stream}, _From, State) ->
    %% Use existing handle_decoded_message logic
    Result = handle_decoded_message({ok, {Type, Msg}}, Stream, State),
    {reply, ok, element(2, Result)};  % Extract state from {noreply, State}
```

#### 5.2 Remove QUIC `handle_info` Clauses from Gateway

Delete all `handle_info({quic, ...})` clauses from gateway!

**Run Gateway Tests**: `rebar3 eunit --module=macula_gateway_tests`
**Expected**: All tests still PASS ✅

---

### Step 6: Update Supervision Tree

#### 6.1 Modify `macula_gateway_sup.erl`

Add QUIC server as FIRST child:

```erlang
init(Opts) ->
    SupFlags = #{
        strategy => one_for_all,  % If QUIC crashes, restart all
        intensity => 10,
        period => 60
    },

    %% Start gateway first to get its PID
    GatewaySpec = #{
        id => gateway,
        start => {macula_gateway, start_link, [Opts]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [macula_gateway]
    },

    %% Start children in order
    {ok, Gateway} = supervisor:start_child(self(), GatewaySpec),

    %% Now start QUIC server with gateway PID
    QuicOpts = [{gateway, Gateway} | Opts],
    QuicSpec = #{
        id => quic_server,
        start => {macula_gateway_quic_server, start_link, [QuicOpts]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [macula_gateway_quic_server]
    },

    Children = [QuicSpec | other_children(Opts, Gateway)],

    {ok, {SupFlags, Children}}.
```

**Run Supervision Tests**: `rebar3 eunit --module=macula_gateway_sup_tests`
**Expected**: Supervision tree tests PASS ✅

---

### Step 7: Full Integration Test

#### 7.1 End-to-End Test
```erlang
integration_test() ->
    %% Start full supervision tree
    Opts = [{port, 29443}, {realm, <<"test.integration">>}],
    {ok, SupPid} = macula_gateway_sup:start_link(Opts),

    %% Verify all children started
    Children = supervisor:which_children(SupPid),
    ?assertEqual(8, length(Children)),  % quic_server + gateway + 6 workers

    %% Test QUIC server can receive events
    %% Test gateway can route messages

    %% Cleanup
    supervisor:terminate_child(SupPid, quic_server),
    supervisor:terminate_child(SupPid, gateway).
```

**Run Full Suite**: `rebar3 eunit`
**Expected**: All 100+ tests PASS ✅

---

## Final Architecture

```
macula_gateway_sup (supervisor)
├── macula_gateway_quic_server (gen_server) [NEW]
│   - Owns QUIC listener
│   - Receives {quic, ...} events
│   - Decodes protocol messages
│   - Routes to gateway via gen_server:call
│   - ~400 LOC
│
├── macula_gateway (gen_server) [SIMPLIFIED]
│   - API facade
│   - Message routing coordinator
│   - Delegates to children
│   - NO QUIC handling!
│   - ~600 LOC (down from 1,011)
│
├── macula_gateway_client_manager
├── macula_gateway_pubsub
├── macula_gateway_rpc
├── macula_gateway_mesh
├── macula_gateway_health
└── macula_gateway_diagnostics
```

## Benefits Achieved

✅ **Proper OTP Design** - One process, one responsibility
✅ **Fault Isolation** - QUIC crashes don't crash gateway
✅ **Clean Separation** - Transport vs. routing
✅ **Testability** - Can test QUIC server in isolation
✅ **Scalability** - Could run multiple QUIC servers
✅ **Maintainability** - Clear, focused modules

## Idiomatic Erlang Throughout

✅ Pattern matching on function heads
✅ Guards instead of `case` where possible
✅ No deep nesting (max 1-2 levels)
✅ Declarative style
✅ Let it crash (supervisor handles restarts)
✅ OTP behaviors (gen_server, supervisor)

## Next Steps After Implementation

1. Run full test suite: `rebar3 eunit`
2. Check line counts: `wc -l src/macula_gateway*.erl`
3. Verify supervision: Start/stop tests
4. E2E test: Macula Arcade matchmaking
5. Update CHANGELOG for v0.8.0 or v0.7.10
6. Commit and tag release

## Estimated Implementation Time

- Step 1-2: 30 minutes (skeleton + helpers)
- Step 3: 30 minutes (listener init)
- Step 4: 2 hours (event handlers + tests)
- Step 5: 1 hour (gateway delegation)
- Step 6: 1 hour (supervision tree)
- Step 7: 30 minutes (integration tests)

**Total**: ~5-6 hours for complete, tested implementation

## Success Criteria

✅ All tests passing (100+ tests)
✅ Gateway reduced to ~600 LOC
✅ QUIC server ~400 LOC
✅ Proper OTP supervision
✅ Clean fault isolation
✅ No regressions in functionality
