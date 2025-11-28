%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_relay_registry and macula_relay_node modules.
%%% Tests relay registration, discovery, and session management.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_relay_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Relay Registry Tests
%%%===================================================================

%% @doc Fixture for relay registry tests
relay_registry_test_() ->
    {foreach,
     fun setup_registry/0,
     fun cleanup_registry/1,
     [
         fun register_relay_test/1,
         fun unregister_relay_test/1,
         fun find_relay_test/1,
         fun find_relay_excludes_target_test/1,
         fun relay_count_test/1,
         fun is_relay_test/1,
         fun get_relays_test/1,
         fun relay_capacity_check_test/1
     ]}.

setup_registry() ->
    {ok, Pid} = macula_relay_registry:start_link(#{
        max_relays => 10,
        relay_ttl_seconds => 600
    }),
    Pid.

cleanup_registry(Pid) ->
    gen_server:stop(Pid).

register_relay_test(_Pid) ->
    fun() ->
        NodeId = <<"relay-001">>,
        Endpoint = {<<"192.168.1.100">>, 4433},

        %% Register as relay
        ?assertEqual(ok, macula_relay_registry:register(NodeId, Endpoint)),

        %% Verify registration
        ?assert(macula_relay_registry:is_relay(NodeId)),
        ?assertEqual(1, macula_relay_registry:get_relay_count())
    end.

unregister_relay_test(_Pid) ->
    fun() ->
        NodeId = <<"relay-002">>,
        Endpoint = {<<"192.168.1.101">>, 4433},

        %% Register then unregister
        ?assertEqual(ok, macula_relay_registry:register(NodeId, Endpoint)),
        ?assert(macula_relay_registry:is_relay(NodeId)),

        macula_relay_registry:unregister(NodeId),
        timer:sleep(10),

        ?assertNot(macula_relay_registry:is_relay(NodeId))
    end.

find_relay_test(_Pid) ->
    fun() ->
        %% Register multiple relays
        macula_relay_registry:register(<<"relay-a">>, {<<"10.0.0.1">>, 4433}),
        timer:sleep(5),
        macula_relay_registry:register(<<"relay-b">>, {<<"10.0.0.2">>, 4433}),
        timer:sleep(5),

        %% Find relay for a target
        {ok, RelayInfo} = macula_relay_registry:find_relay(<<"target-001">>),

        %% Should return one of the registered relays
        RelayNodeId = maps:get(node_id, RelayInfo),
        ?assert(RelayNodeId =:= <<"relay-a">> orelse RelayNodeId =:= <<"relay-b">>)
    end.

find_relay_excludes_target_test(_Pid) ->
    fun() ->
        %% Register a relay
        TargetId = <<"relay-target">>,
        macula_relay_registry:register(TargetId, {<<"10.0.0.5">>, 4433}),
        timer:sleep(5),

        %% Try to find relay for itself (should fail with no other relays)
        Result = macula_relay_registry:find_relay(TargetId),
        ?assertEqual({error, no_relays_available}, Result)
    end.

relay_count_test(_Pid) ->
    fun() ->
        %% Initially no relays
        ?assertEqual(0, macula_relay_registry:get_relay_count()),

        %% Add relays
        macula_relay_registry:register(<<"count-1">>, {<<"1.1.1.1">>, 4433}),
        timer:sleep(5),
        ?assertEqual(1, macula_relay_registry:get_relay_count()),

        macula_relay_registry:register(<<"count-2">>, {<<"2.2.2.2">>, 4433}),
        timer:sleep(5),
        ?assertEqual(2, macula_relay_registry:get_relay_count()),

        %% Remove one
        macula_relay_registry:unregister(<<"count-1">>),
        timer:sleep(5),
        ?assertEqual(1, macula_relay_registry:get_relay_count())
    end.

is_relay_test(_Pid) ->
    fun() ->
        NodeId = <<"check-relay">>,

        %% Not a relay initially
        ?assertNot(macula_relay_registry:is_relay(NodeId)),

        %% Register and check
        macula_relay_registry:register(NodeId, {<<"5.5.5.5">>, 4433}),
        timer:sleep(5),
        ?assert(macula_relay_registry:is_relay(NodeId))
    end.

get_relays_test(_Pid) ->
    fun() ->
        %% Initially empty
        ?assertEqual([], macula_relay_registry:get_relays()),

        %% Add relays
        macula_relay_registry:register(<<"list-1">>, {<<"6.6.6.1">>, 4433}),
        timer:sleep(5),
        macula_relay_registry:register(<<"list-2">>, {<<"6.6.6.2">>, 4433}),
        timer:sleep(5),

        Relays = macula_relay_registry:get_relays(),
        ?assertEqual(2, length(Relays)),

        %% All returned items should have required fields
        lists:foreach(
            fun(Relay) ->
                ?assert(maps:is_key(node_id, Relay)),
                ?assert(maps:is_key(endpoint, Relay)),
                ?assert(maps:is_key(capacity, Relay))
            end,
            Relays
        )
    end.

relay_capacity_check_test(_Pid) ->
    fun() ->
        %% Register relay with custom capacity
        NodeId = <<"capacity-relay">>,
        Endpoint = {<<"7.7.7.7">>, 4433},

        macula_relay_registry:register(NodeId, Endpoint, #{capacity => 50}),
        timer:sleep(5),

        Relays = macula_relay_registry:get_relays(),
        [Relay] = [R || R <- Relays, maps:get(node_id, R) =:= NodeId],

        ?assertEqual(50, maps:get(capacity, Relay))
    end.

%%%===================================================================
%%% Relay Node Tests
%%%===================================================================

%% @doc Fixture for relay node tests
relay_node_test_() ->
    {foreach,
     fun setup_relay_node/0,
     fun cleanup_relay_node/1,
     [
         fun relay_initially_disabled_test/1,
         fun enable_relay_test/1,
         fun disable_relay_test/1,
         fun request_relay_when_disabled_test/1,
         fun request_relay_creates_session_test/1,
         fun close_relay_session_test/1,
         fun get_stats_test/1,
         fun get_sessions_test/1
     ]}.

setup_relay_node() ->
    %% Start registry first (relay node depends on it)
    {ok, RegPid} = macula_relay_registry:start_link(#{}),
    {ok, NodePid} = macula_relay_node:start_link(#{
        max_relay_sessions => 10
    }),
    {RegPid, NodePid}.

cleanup_relay_node({RegPid, NodePid}) ->
    gen_server:stop(NodePid),
    gen_server:stop(RegPid).

relay_initially_disabled_test(_Pids) ->
    fun() ->
        ?assertNot(macula_relay_node:is_enabled())
    end.

enable_relay_test(_Pids) ->
    fun() ->
        NodeId = <<"my-relay-node">>,
        Endpoint = {<<"8.8.8.8">>, 4433},

        %% Enable relay
        ok = macula_relay_node:enable(#{
            node_id => NodeId,
            endpoint => Endpoint
        }),

        ?assert(macula_relay_node:is_enabled())
    end.

disable_relay_test(_Pids) ->
    fun() ->
        %% Enable first
        ok = macula_relay_node:enable(#{
            node_id => <<"disable-test">>,
            endpoint => {<<"9.9.9.9">>, 4433}
        }),
        ?assert(macula_relay_node:is_enabled()),

        %% Disable
        ok = macula_relay_node:disable(),
        ?assertNot(macula_relay_node:is_enabled())
    end.

request_relay_when_disabled_test(_Pids) ->
    fun() ->
        %% Request without enabling
        Result = macula_relay_node:request_relay(<<"initiator">>, <<"target">>),
        ?assertEqual({error, relay_not_enabled}, Result)
    end.

request_relay_creates_session_test(_Pids) ->
    fun() ->
        %% Enable relay first
        ok = macula_relay_node:enable(#{
            node_id => <<"session-relay">>,
            endpoint => {<<"10.10.10.10">>, 4433}
        }),

        %% Request relay session
        {ok, SessionId} = macula_relay_node:request_relay(<<"peer-a">>, <<"peer-b">>),

        ?assert(is_binary(SessionId)),
        ?assert(byte_size(SessionId) > 0),

        %% Check stats
        Stats = macula_relay_node:get_stats(),
        ?assertEqual(1, maps:get(active_sessions, Stats))
    end.

close_relay_session_test(_Pids) ->
    fun() ->
        %% Enable and create session
        ok = macula_relay_node:enable(#{
            node_id => <<"close-test-relay">>,
            endpoint => {<<"11.11.11.11">>, 4433}
        }),

        {ok, SessionId} = macula_relay_node:request_relay(<<"close-a">>, <<"close-b">>),

        %% Verify session exists
        Stats1 = macula_relay_node:get_stats(),
        ?assertEqual(1, maps:get(active_sessions, Stats1)),

        %% Close session
        macula_relay_node:close_relay(SessionId),
        timer:sleep(10),

        %% Verify session removed
        Stats2 = macula_relay_node:get_stats(),
        ?assertEqual(0, maps:get(active_sessions, Stats2))
    end.

get_stats_test(_Pids) ->
    fun() ->
        Stats = macula_relay_node:get_stats(),

        %% Stats should have required fields
        ?assert(maps:is_key(enabled, Stats)),
        ?assert(maps:is_key(max_sessions, Stats)),
        ?assert(maps:is_key(active_sessions, Stats)),
        ?assert(maps:is_key(total_bytes_relayed, Stats)),
        ?assert(maps:is_key(total_sessions_created, Stats))
    end.

get_sessions_test(_Pids) ->
    fun() ->
        %% Enable and create sessions
        ok = macula_relay_node:enable(#{
            node_id => <<"sessions-relay">>,
            endpoint => {<<"12.12.12.12">>, 4433}
        }),

        {ok, _} = macula_relay_node:request_relay(<<"sess-a">>, <<"sess-b">>),
        {ok, _} = macula_relay_node:request_relay(<<"sess-c">>, <<"sess-d">>),

        Sessions = macula_relay_node:get_sessions(),
        ?assertEqual(2, length(Sessions)),

        %% Each session should have required fields
        lists:foreach(
            fun(Session) ->
                ?assert(maps:is_key(session_id, Session)),
                ?assert(maps:is_key(initiator_id, Session)),
                ?assert(maps:is_key(target_id, Session)),
                ?assert(maps:is_key(started_at, Session)),
                ?assert(maps:is_key(bytes_relayed, Session))
            end,
            Sessions
        )
    end.
