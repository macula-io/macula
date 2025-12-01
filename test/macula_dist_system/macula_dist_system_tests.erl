%%%-------------------------------------------------------------------
%%% @doc Unit tests for macula_dist_system supervisor.
%%%
%%% Tests cover:
%%% - Supervisor start/stop
%%% - Child process management
%%% - Configuration options (discovery_type, auto_cluster)
%%% - Restart strategy behavior
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_system_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Fixtures
%%------------------------------------------------------------------------------

dist_system_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun start_with_defaults_test_/1,
      fun start_with_mdns_discovery_test_/1,
      fun start_with_dht_discovery_test_/1,
      fun start_with_auto_cluster_test_/1,
      fun child_restart_test_/1,
      fun supervisor_flags_test_/1
     ]}.

setup() ->
    %% Ensure clean state - stop any existing dist system
    case whereis(macula_dist_system) of
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
    %% Stop discovery if running
    case whereis(macula_dist_discovery) of
        undefined -> ok;
        DiscPid ->
            catch unlink(DiscPid),
            DiscRef = monitor(process, DiscPid),
            exit(DiscPid, shutdown),
            receive
                {'DOWN', DiscRef, process, DiscPid, _} -> ok
            after 1000 -> ok
            end
    end,
    ok.

cleanup(_) ->
    %% Stop supervisor if running
    case whereis(macula_dist_system) of
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
        {ok, Pid} = macula_dist_system:start_link(),
        ?assert(is_pid(Pid)),
        ?assertEqual(Pid, whereis(macula_dist_system)),
        ?assert(is_process_alive(Pid)),

        %% Should have discovery child started
        Children = supervisor:which_children(macula_dist_system),
        ?assert(length(Children) >= 1),

        %% macula_dist_discovery should be present
        DiscChild = lists:keyfind(macula_dist_discovery, 1, Children),
        ?assertNotEqual(false, DiscChild),
        {macula_dist_discovery, DiscPid, worker, _} = DiscChild,
        ?assert(is_pid(DiscPid))
    end}.

start_with_mdns_discovery_test_(_) ->
    {"supervisor starts with mDNS discovery type", fun() ->
        Opts = #{discovery_type => mdns},
        {ok, Pid} = macula_dist_system:start_link(Opts),
        ?assert(is_pid(Pid)),

        %% Discovery should be started
        Children = supervisor:which_children(macula_dist_system),
        DiscChild = lists:keyfind(macula_dist_discovery, 1, Children),
        ?assertNotEqual(false, DiscChild)
    end}.

start_with_dht_discovery_test_(_) ->
    {"supervisor starts with DHT discovery type", fun() ->
        Opts = #{discovery_type => dht},
        {ok, Pid} = macula_dist_system:start_link(Opts),
        ?assert(is_pid(Pid)),

        %% Discovery should be started
        Children = supervisor:which_children(macula_dist_system),
        DiscChild = lists:keyfind(macula_dist_discovery, 1, Children),
        ?assertNotEqual(false, DiscChild)
    end}.

start_with_auto_cluster_test_(_) ->
    {"supervisor starts with auto_cluster enabled", fun() ->
        Opts = #{auto_cluster => true},
        {ok, Pid} = macula_dist_system:start_link(Opts),
        ?assert(is_pid(Pid)),

        %% Should have both discovery and cluster_strategy children
        Children = supervisor:which_children(macula_dist_system),

        DiscChild = lists:keyfind(macula_dist_discovery, 1, Children),
        ?assertNotEqual(false, DiscChild),

        ClusterChild = lists:keyfind(macula_cluster_strategy, 1, Children),
        ?assertNotEqual(false, ClusterChild)
    end}.

%%------------------------------------------------------------------------------
%% Child Management Tests
%%------------------------------------------------------------------------------

child_restart_test_(_) ->
    {"supervisor restarts crashed children", fun() ->
        {ok, _SupPid} = macula_dist_system:start_link(),
        timer:sleep(100),

        %% Get the discovery child PID
        Children = supervisor:which_children(macula_dist_system),
        DiscChild = lists:keyfind(macula_dist_discovery, 1, Children),
        ?assertNotEqual(false, DiscChild),

        {macula_dist_discovery, DiscPid, worker, _} = DiscChild,
        ?assert(is_pid(DiscPid)),

        %% Kill the discovery process
        OldPid = DiscPid,
        exit(DiscPid, kill),
        timer:sleep(100),

        %% Supervisor should have restarted it
        NewChildren = supervisor:which_children(macula_dist_system),
        NewDiscChild = lists:keyfind(macula_dist_discovery, 1, NewChildren),
        {macula_dist_discovery, NewDiscPid, worker, _} = NewDiscChild,

        %% New PID should be different
        ?assert(is_pid(NewDiscPid)),
        ?assertNotEqual(OldPid, NewDiscPid)
    end}.

%%------------------------------------------------------------------------------
%% Configuration Tests
%%------------------------------------------------------------------------------

supervisor_flags_test_(_) ->
    {"supervisor uses one_for_one strategy", fun() ->
        Opts = #{auto_cluster => true},
        {ok, _SupPid} = macula_dist_system:start_link(Opts),
        timer:sleep(100),

        Children = supervisor:which_children(macula_dist_system),

        %% Get two different children
        {macula_dist_discovery, DiscPid, _, _} = lists:keyfind(macula_dist_discovery, 1, Children),
        {macula_cluster_strategy, ClusterPid, _, _} = lists:keyfind(macula_cluster_strategy, 1, Children),

        %% Kill discovery
        exit(DiscPid, kill),
        timer:sleep(100),

        %% Cluster strategy should still be the same PID (one_for_one strategy)
        ?assert(is_process_alive(ClusterPid))
    end}.
