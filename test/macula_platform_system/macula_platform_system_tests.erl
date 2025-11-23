%% @doc Unit tests for macula_platform_system supervisor
-module(macula_platform_system_tests).
-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

platform_system_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(test_start_link_creates_supervisor()),
      ?_test(test_supervisor_starts_leader_election()),
      ?_test(test_supervisor_strategy_one_for_one()),
      ?_test(test_child_spec_correct()),
      ?_test(test_supervisor_restart_intensity()),
      ?_test(test_leader_election_child_running()),
      ?_test(test_supervisor_stops_cleanly()),
      ?_test(test_supervisor_restarts_crashed_child())
     ]}.

setup() ->
    %% Ensure ra application is started for leader election
    application:ensure_all_started(ra),
    Config = #{},
    Config.

cleanup(_Config) ->
    %% Stop platform system if running
    catch supervisor:terminate_child(macula_platform_system, macula_leader_election),
    catch gen_server:stop(macula_platform_system),
    ok.

%%------------------------------------------------------------------------------
%% Basic Supervisor Tests
%%------------------------------------------------------------------------------

test_start_link_creates_supervisor() ->
    Config = #{},
    {ok, Pid} = macula_platform_system:start_link(Config),

    %% Verify it's a supervisor process
    ?assert(is_process_alive(Pid)),
    ?assertEqual(Pid, whereis(macula_platform_system)),

    %% Cleanup
    exit(Pid, shutdown),
    ok.

test_supervisor_starts_leader_election() ->
    Config = #{},
    {ok, Pid} = macula_platform_system:start_link(Config),

    %% Get children
    Children = supervisor:which_children(Pid),

    %% Should have exactly 1 child (leader_election)
    ?assertEqual(1, length(Children)),

    %% Verify leader_election child exists
    {ChildId, ChildPid, ChildType, _Modules} = hd(Children),
    ?assertEqual(macula_leader_election, ChildId),
    ?assert(is_pid(ChildPid)),
    ?assertEqual(worker, ChildType),

    %% Cleanup
    exit(Pid, shutdown),
    ok.

test_supervisor_strategy_one_for_one() ->
    Config = #{},
    {ok, Pid} = macula_platform_system:start_link(Config),

    %% Check supervisor flags via internal state
    %% one_for_one means each child restarts independently
    {status, _, _, [[_, StateData] | _]} = sys:get_status(Pid),
    [_, _, {data, [{"State", State}]}] = StateData,

    %% Verify strategy is one_for_one
    {state, _Name, Strategy, _Children, _Dynamics, _Intensity, _Period, _Restarts, _DynamicRestarts, _Module, _Args} = State,
    ?assertEqual(one_for_one, Strategy),

    %% Cleanup
    exit(Pid, shutdown),
    ok.

test_child_spec_correct() ->
    Config = #{},
    {ok, Pid} = macula_platform_system:start_link(Config),

    %% Get child spec
    [{Id, ChildPid, Type, Modules}] = supervisor:which_children(Pid),

    %% Verify child spec fields
    ?assertEqual(macula_leader_election, Id),
    ?assert(is_pid(ChildPid)),
    ?assertEqual(worker, Type),
    ?assertEqual([macula_leader_election], Modules),

    %% Cleanup
    exit(Pid, shutdown),
    ok.

%%------------------------------------------------------------------------------
%% Restart Policy Tests
%%------------------------------------------------------------------------------

test_supervisor_restart_intensity() ->
    Config = #{},
    {ok, Pid} = macula_platform_system:start_link(Config),

    %% Get supervisor state
    {status, _, _, [[_, StateData] | _]} = sys:get_status(Pid),
    [_, _, {data, [{"State", State}]}] = StateData,

    %% Verify restart intensity (max 5 restarts in 10 seconds)
    {state, _Name, _Strategy, _Children, _Dynamics, Intensity, Period, _Restarts, _DynamicRestarts, _Module, _Args} = State,
    ?assertEqual(5, Intensity),
    ?assertEqual(10, Period),

    %% Cleanup
    exit(Pid, shutdown),
    ok.

test_leader_election_child_running() ->
    Config = #{},
    {ok, Pid} = macula_platform_system:start_link(Config),

    %% Get leader_election child PID
    [{_Id, ChildPid, _Type, _Modules}] = supervisor:which_children(Pid),

    %% Verify child is running
    ?assert(is_process_alive(ChildPid)),

    %% Verify we can send a message (gen_server test)
    %% This will fail gracefully if leader_election isn't ready yet
    catch gen_server:call(ChildPid, get_leader, 100),

    %% Cleanup
    exit(Pid, shutdown),
    ok.

test_supervisor_stops_cleanly() ->
    Config = #{},
    {ok, Pid} = macula_platform_system:start_link(Config),

    %% Get child PID before stopping
    [{_Id, ChildPid, _Type, _Modules}] = supervisor:which_children(Pid),

    %% Stop supervisor
    exit(Pid, shutdown),
    timer:sleep(100),

    %% Verify supervisor stopped
    ?assertNot(is_process_alive(Pid)),

    %% Verify child also stopped
    ?assertNot(is_process_alive(ChildPid)),
    ok.

test_supervisor_restarts_crashed_child() ->
    Config = #{},
    {ok, Pid} = macula_platform_system:start_link(Config),

    %% Get initial child PID
    [{_Id, ChildPid1, _Type, _Modules}] = supervisor:which_children(Pid),

    %% Kill the child
    exit(ChildPid1, kill),
    timer:sleep(200),

    %% Get new child PID
    [{_, ChildPid2, _, _}] = supervisor:which_children(Pid),

    %% Verify child was restarted with new PID
    ?assertNot(ChildPid1 =:= ChildPid2),
    ?assert(is_process_alive(ChildPid2)),

    %% Cleanup
    exit(Pid, shutdown),
    ok.
