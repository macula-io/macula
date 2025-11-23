%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_leader_election module.
%%% Tests single-node initialization, API functions, and callback system.
%%% Multi-node tests require Common Test (see macula_leader_election_SUITE).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_leader_election_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

leader_election_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"start_link creates gen_server", fun test_start_link/0},
         {"initial state has no leader", fun test_initial_no_leader/0},
         {"single node elects itself as leader", fun test_single_node_self_election/0},
         {"is_leader returns true for elected leader", fun test_is_leader_true/0},
         {"get_leader returns elected leader", fun test_get_leader/0},
         {"get_members returns single member", fun test_get_members_single/0},
         {"register_callback adds callback", fun test_register_callback/0},
         {"unregister_callback removes callback", fun test_unregister_callback/0},
         {"callback fires on leadership change", fun test_callback_fires/0},
         {"multiple callbacks all fire", fun test_multiple_callbacks/0},
         {"callback errors are caught and logged", fun test_callback_error_handling/0},
         {"callbacks receive correct is_leader value", fun test_callback_receives_true/0}
     ]}.

setup() ->
    %% Start ra application
    application:ensure_all_started(ra),

    %% Clean up any existing ra data
    os:cmd("rm -rf data/*"),

    %% Generate unique node ID and realm for each test
    NodeId = list_to_binary("test_node_" ++ integer_to_list(erlang:unique_integer([positive]))),
    Realm = <<"test_realm">>,

    %% Start leader election process
    {ok, Pid} = macula_leader_election:start_link(#{
        node_id => NodeId,
        realm => Realm
    }),

    %% Wait for initialization (mesh stabilization + cluster init = 7 seconds)
    timer:sleep(7500),

    #{pid => Pid, node_id => NodeId, realm => Realm}.

cleanup(#{pid := Pid}) ->
    catch gen_server:stop(Pid, normal, 5000),

    %% Clean up ra data
    timer:sleep(1000),
    os:cmd("rm -rf data/*"),

    ok.

%%%===================================================================
%%% Startup and Initialization Tests
%%%===================================================================

test_start_link() ->
    %% Test is implicitly run in setup - if we get here, start_link succeeded
    ok.

test_initial_no_leader() ->
    %% GIVEN: A freshly started leader election process
    %% WHEN: We query for leader before cluster initialization
    %% THEN: Leader should be undefined initially

    %% Start a new process without waiting for init
    NodeId = <<"test_node_immediate">>,
    {ok, Pid} = macula_leader_election:start_link(#{
        node_id => NodeId,
        realm => <<"test">>
    }),

    %% Query immediately (before 5 second delay)
    Result = macula_leader_election:get_leader(),

    %% Clean up
    gen_server:stop(Pid),

    %% Should have no leader yet
    ?assertMatch({ok, undefined}, Result).

%%%===================================================================
%%% Single-Node Election Tests
%%%===================================================================

test_single_node_self_election() ->
    %% GIVEN: A single-node cluster (from setup)
    %% WHEN: Cluster initializes and election runs
    %% THEN: The node should elect itself as leader

    {ok, Leader} = macula_leader_election:get_leader(),
    ?assert(Leader =/= undefined),
    ?assert(is_atom(Leader)).

test_is_leader_true() ->
    %% GIVEN: A single-node cluster where we are leader
    %% WHEN: Checking if we are leader
    %% THEN: Should return true

    IsLeader = macula_leader_election:is_leader(),
    ?assertEqual(true, IsLeader).

%%%===================================================================
%%% API Function Tests
%%%===================================================================

test_get_leader() ->
    %% GIVEN: An elected leader
    %% WHEN: Calling get_leader()
    %% THEN: Should return {ok, LeaderId} where LeaderId is an atom

    Result = macula_leader_election:get_leader(),
    ?assertMatch({ok, _Leader}, Result),

    {ok, Leader} = Result,
    ?assert(is_atom(Leader)),
    ?assert(Leader =/= undefined).

test_get_members_single() ->
    %% GIVEN: A single-node cluster
    %% WHEN: Calling get_members()
    %% THEN: Should return {ok, [SingleMember]}

    Result = macula_leader_election:get_members(),
    ?assertMatch({ok, [_Member]}, Result),

    {ok, Members} = Result,
    ?assertEqual(1, length(Members)).

%%%===================================================================
%%% Callback Registration Tests
%%%===================================================================

test_register_callback() ->
    %% GIVEN: A leader election process
    %% WHEN: Registering a callback
    %% THEN: Should return ok

    Callback = fun(_IsLeader) -> ok end,
    Result = macula_leader_election:register_callback(test_callback, Callback),
    ?assertEqual(ok, Result),

    %% Clean up
    macula_leader_election:unregister_callback(test_callback).

test_unregister_callback() ->
    %% GIVEN: A registered callback
    Callback = fun(_IsLeader) -> ok end,
    ok = macula_leader_election:register_callback(test_callback_2, Callback),

    %% WHEN: Unregistering the callback
    Result = macula_leader_election:unregister_callback(test_callback_2),

    %% THEN: Should return ok
    ?assertEqual(ok, Result).

test_callback_fires() ->
    %% GIVEN: A process to receive callback notifications
    TestPid = self(),
    Callback = fun(IsLeader) ->
        TestPid ! {callback_fired, IsLeader}
    end,

    %% WHEN: Registering callback and triggering leadership check
    ok = macula_leader_election:register_callback(test_fire, Callback),

    %% Wait for next check_leader cycle (5 seconds + buffer)
    timer:sleep(6000),

    %% THEN: Callback should have been called
    %% Note: May fire multiple times due to periodic checks
    receive
        {callback_fired, IsLeader} ->
            ?assert(is_boolean(IsLeader))
    after 1000 ->
        ?assert(false)  % Should have received callback
    end,

    %% Clean up
    macula_leader_election:unregister_callback(test_fire).

test_multiple_callbacks() ->
    %% GIVEN: Multiple registered callbacks
    TestPid = self(),

    Callback1 = fun(IsLeader) ->
        TestPid ! {callback1, IsLeader}
    end,

    Callback2 = fun(IsLeader) ->
        TestPid ! {callback2, IsLeader}
    end,

    ok = macula_leader_election:register_callback(cb1, Callback1),
    ok = macula_leader_election:register_callback(cb2, Callback2),

    %% WHEN: Leadership check occurs
    timer:sleep(6000),

    %% THEN: Both callbacks should fire
    Received1 = receive {callback1, _} -> true after 1000 -> false end,
    Received2 = receive {callback2, _} -> true after 1000 -> false end,

    ?assert(Received1),
    ?assert(Received2),

    %% Clean up
    macula_leader_election:unregister_callback(cb1),
    macula_leader_election:unregister_callback(cb2).

test_callback_error_handling() ->
    %% GIVEN: A callback that throws an error
    Callback = fun(_IsLeader) ->
        error(intentional_test_error)
    end,

    %% WHEN: Registering and firing the callback
    ok = macula_leader_election:register_callback(error_cb, Callback),

    %% Wait for check_leader cycle
    timer:sleep(6000),

    %% THEN: Process should still be alive despite callback error
    IsAlive = is_process_alive(whereis(macula_leader_election)),
    ?assert(IsAlive),

    %% Clean up
    macula_leader_election:unregister_callback(error_cb).

test_callback_receives_true() ->
    %% GIVEN: A single-node cluster where we are leader
    TestPid = self(),

    Callback = fun(IsLeader) ->
        TestPid ! {leader_status, IsLeader}
    end,

    %% WHEN: Callback fires
    ok = macula_leader_election:register_callback(status_cb, Callback),
    timer:sleep(6000),

    %% THEN: Should receive true (we are leader)
    receive
        {leader_status, IsLeader} ->
            ?assertEqual(true, IsLeader)
    after 1000 ->
        ?assert(false)  % Should have received status
    end,

    %% Clean up
    macula_leader_election:unregister_callback(status_cb).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% None needed for these tests
