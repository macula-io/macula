%%%-------------------------------------------------------------------
%%% @doc Unit tests for macula_nat_system supervisor.
%%%
%%% Tests cover:
%%% - Supervisor start/stop
%%% - Child process management
%%% - Configuration options
%%% - Restart strategy behavior
%%% @end
%%%-------------------------------------------------------------------
-module(macula_nat_system_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

nat_system_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun start_with_defaults_test_/1,
      fun start_with_options_test_/1,
      fun child_restart_test_/1,
      fun shutdown_test_/1,
      fun expected_children_test_/1,
      fun child_specs_test_/1,
      fun supervisor_flags_test_/1
     ]}.

setup() ->
    %% Ensure clean state
    catch application:stop(gproc),
    application:start(gproc),
    ok.

cleanup(_) ->
    %% Stop supervisor if running
    case whereis(macula_nat_system) of
        undefined -> ok;
        Pid ->
            catch unlink(Pid),
            Ref = monitor(process, Pid),
            exit(Pid, shutdown),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 1000 -> ok
            end
    end,
    ok.

%%------------------------------------------------------------------------------
%% Supervisor Start Tests
%%------------------------------------------------------------------------------

start_with_defaults_test_(_) ->
    {"supervisor starts with default options", fun() ->
        {ok, Pid} = macula_nat_system:start_link(),
        ?assert(is_pid(Pid)),
        ?assertEqual(Pid, whereis(macula_nat_system)),
        ?assert(is_process_alive(Pid))
    end}.

start_with_options_test_(_) ->
    {"supervisor starts with custom options", fun() ->
        Opts = #{
            cache_max_entries => 5000,
            cache_ttl_seconds => 600,
            detection_timeout_ms => 3000
        },
        {ok, Pid} = macula_nat_system:start_link(Opts),
        ?assert(is_pid(Pid)),
        ?assertEqual(Pid, whereis(macula_nat_system))
    end}.

%%------------------------------------------------------------------------------
%% Child Management Tests
%%------------------------------------------------------------------------------

child_restart_test_(_) ->
    {"supervisor restarts crashed children", fun() ->
        {ok, _SupPid} = macula_nat_system:start_link(),
        timer:sleep(100),

        %% Get the nat_cache child PID
        Children = supervisor:which_children(macula_nat_system),
        CacheChild = lists:keyfind(nat_cache, 1, Children),
        ?assertNotEqual(false, CacheChild),

        {nat_cache, CachePid, worker, _} = CacheChild,
        ?assert(is_pid(CachePid)),

        %% Kill the cache process
        OldPid = CachePid,
        exit(CachePid, kill),
        timer:sleep(100),

        %% Supervisor should have restarted it
        NewChildren = supervisor:which_children(macula_nat_system),
        NewCacheChild = lists:keyfind(nat_cache, 1, NewChildren),
        {nat_cache, NewCachePid, worker, _} = NewCacheChild,

        %% New PID should be different
        ?assert(is_pid(NewCachePid)),
        ?assertNotEqual(OldPid, NewCachePid)
    end}.

shutdown_test_(_) ->
    {"supervisor stops all children on shutdown", fun() ->
        {ok, SupPid} = macula_nat_system:start_link(),
        timer:sleep(100),

        %% Get all child PIDs
        Children = supervisor:which_children(macula_nat_system),
        ChildPids = [P || {_, P, _, _} <- Children, is_pid(P)],

        %% Verify we have children
        ?assert(length(ChildPids) > 0),

        %% All children should be alive
        lists:foreach(fun(P) -> ?assert(is_process_alive(P)) end, ChildPids),

        %% Stop supervisor using proc_lib to avoid linking issues
        catch unlink(SupPid),
        Ref = monitor(process, SupPid),
        exit(SupPid, shutdown),
        receive
            {'DOWN', Ref, process, SupPid, _} -> ok
        after 1000 -> ok
        end,

        %% Supervisor should be gone
        ?assertEqual(undefined, whereis(macula_nat_system)),

        %% All children should be dead
        timer:sleep(50),
        lists:foreach(fun(P) -> ?assertNot(is_process_alive(P)) end, ChildPids)
    end}.

expected_children_test_(_) ->
    {"supervisor starts all expected children", fun() ->
        {ok, _SupPid} = macula_nat_system:start_link(),
        timer:sleep(100),

        %% Verify supervisor started all expected children
        Children = supervisor:which_children(macula_nat_system),
        ChildIds = [Id || {Id, _, _, _} <- Children],

        %% Expected children
        ExpectedChildren = [
            nat_cache,
            nat_detector,
            nat_coordinator,
            hole_punch,
            connection_upgrade,
            port_predictor,
            relay_registry,
            relay_node
        ],

        %% All expected children should be present
        lists:foreach(
            fun(ExpectedId) ->
                ?assert(lists:member(ExpectedId, ChildIds),
                        {missing_child, ExpectedId})
            end,
            ExpectedChildren
        )
    end}.

%%------------------------------------------------------------------------------
%% Configuration Tests
%%------------------------------------------------------------------------------

child_specs_test_(_) ->
    {"child specs are correctly formatted", fun() ->
        {ok, _SupPid} = macula_nat_system:start_link(),
        timer:sleep(100),

        Children = supervisor:which_children(macula_nat_system),

        %% Each child should have correct format
        lists:foreach(
            fun({Id, Pid, Type, Modules}) ->
                ?assert(is_atom(Id)),
                ?assert(is_pid(Pid) orelse Pid =:= undefined orelse Pid =:= restarting),
                ?assert(Type =:= worker orelse Type =:= supervisor),
                ?assert(is_list(Modules))
            end,
            Children
        )
    end}.

supervisor_flags_test_(_) ->
    {"supervisor uses one_for_one strategy", fun() ->
        {ok, _SupPid} = macula_nat_system:start_link(),
        timer:sleep(100),

        Children = supervisor:which_children(macula_nat_system),

        %% Get two different children
        {nat_cache, CachePid, _, _} = lists:keyfind(nat_cache, 1, Children),
        {nat_detector, DetectorPid, _, _} = lists:keyfind(nat_detector, 1, Children),

        %% Kill cache
        exit(CachePid, kill),
        timer:sleep(100),

        %% Detector should still be the same PID (one_for_one strategy)
        ?assert(is_process_alive(DetectorPid))
    end}.
