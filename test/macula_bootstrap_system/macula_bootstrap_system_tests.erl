%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for macula_bootstrap_system (supervisor).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_bootstrap_system_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

bootstrap_system_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Fixture) ->
         [
          {"supervisor starts all children", fun() -> test_all_children_started(Fixture) end},
          {"get_server_pid returns server", fun() -> test_get_server_pid(Fixture) end},
          {"get_stats delegates to server", fun() -> test_get_stats(Fixture) end},
          {"supervisor restarts crashed children", fun() -> test_restart_policy(Fixture) end}
         ]
     end}.

%%%===================================================================
%%% Setup/Cleanup
%%%===================================================================

setup() ->
    %% Start routing server (dependency for bootstrap system)
    LocalNodeId = crypto:hash(sha256, <<"test_system_node">>),
    RoutingConfig = #{k => 20, alpha => 3},
    {ok, RoutingPid} = macula_routing_server:start_link(LocalNodeId, RoutingConfig),

    %% Start bootstrap system supervisor
    Config = #{
        realm => <<"test.realm">>,
        health_check_interval => 60000  % Don't auto-check during tests
    },
    {ok, SupPid} = macula_bootstrap_system:start_link(Config),

    %% Give children time to start
    timer:sleep(100),

    #{
        routing_pid => RoutingPid,
        sup_pid => SupPid
    }.

cleanup(#{routing_pid := RoutingPid, sup_pid := SupPid}) ->
    %% Use proper cleanup pattern to avoid "unexpected termination of test process"
    %% Unlink to prevent EXIT signal from affecting test process
    case is_process_alive(SupPid) of
        true ->
            catch unlink(SupPid),
            Ref = monitor(process, SupPid),
            exit(SupPid, shutdown),
            receive
                {'DOWN', Ref, process, SupPid, _} -> ok
            after 1000 -> ok
            end;
        false ->
            ok
    end,
    catch gen_server:stop(RoutingPid),
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================

test_all_children_started(#{sup_pid := SupPid}) ->
    %% Get supervisor children
    Children = supervisor:which_children(SupPid),

    %% Should have 3 children
    ?assertEqual(3, length(Children)),

    %% Verify all children are running
    lists:foreach(fun({_Id, Pid, _Type, _Modules}) ->
        ?assert(is_pid(Pid)),
        ?assert(is_process_alive(Pid))
    end, Children),

    %% Verify specific children exist
    ChildIds = [Id || {Id, _Pid, _Type, _Modules} <- Children],
    ?assert(lists:member(bootstrap_server, ChildIds)),
    ?assert(lists:member(service_registry, ChildIds)),
    ?assert(lists:member(health_monitor, ChildIds)).

test_get_server_pid(_Fixture) ->
    %% Get server PID via API
    Result = macula_bootstrap_system:get_server_pid(),

    ?assertMatch({ok, _Pid}, Result),

    {ok, Pid} = Result,
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)).

test_get_stats(_Fixture) ->
    %% Get stats via supervisor API
    Result = macula_bootstrap_system:get_stats(),

    ?assertMatch({ok, _Stats}, Result),

    {ok, Stats} = Result,
    ?assertMatch(#{queries_handled := _, services_registered := _}, Stats).

test_restart_policy(#{sup_pid := SupPid}) ->
    %% Get initial server PID
    {ok, InitialServerPid} = macula_bootstrap_system:get_server_pid(),

    %% Kill the server
    exit(InitialServerPid, kill),

    %% Wait for supervisor to restart it
    timer:sleep(200),

    %% Get new server PID
    {ok, NewServerPid} = macula_bootstrap_system:get_server_pid(),

    %% Should be a different PID (restarted)
    ?assertNotEqual(InitialServerPid, NewServerPid),

    %% New server should be alive
    ?assert(is_process_alive(NewServerPid)),

    %% Supervisor should still be alive
    ?assert(is_process_alive(SupPid)).
