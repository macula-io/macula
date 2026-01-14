%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for macula_content_system supervisor.
%%%
%%% Tests supervisor functionality:
%%% - Child process management
%%% - Supervision strategy
%%% - Child process restarts
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_system_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Descriptions
%%%===================================================================

macula_content_system_test_() ->
    [
      %% Non-process tests (don't need supervisor running)
      {"Supervisor has correct children", fun supervisor_children/0},
      {"Child specs are valid", fun child_specs_valid/0},
      {"Uses one_for_one strategy", fun strategy_one_for_one/0}
    ].

%% Separate test for supervisor lifecycle (uses foreach to ensure cleanup)
supervisor_lifecycle_test_() ->
    {foreach,
     fun setup_supervisor/0,
     fun cleanup_supervisor/1,
     [
      fun supervisor_starts_correctly/1,
      fun supervisor_has_children/1
     ]}.

%%%===================================================================
%%% Setup and Cleanup
%%%===================================================================

setup_supervisor() ->
    %% Kill any existing processes first
    catch gen_server:stop(macula_content_store),
    catch gen_server:stop(macula_content_transfer),
    catch supervisor:terminate_child(macula_content_system, macula_content_store),
    catch supervisor:terminate_child(macula_content_system, macula_content_transfer),
    case whereis(macula_content_system) of
        undefined -> ok;
        Pid -> exit(Pid, kill), timer:sleep(100)
    end,

    %% Create temp directory
    Rand = integer_to_list(erlang:unique_integer([positive])),
    Dir = filename:join(["/tmp", "macula_content_sys_test_" ++ Rand]),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    Dir.

cleanup_supervisor(TempDir) ->
    %% Stop supervisor gracefully
    case whereis(macula_content_system) of
        undefined -> ok;
        Pid ->
            try
                supervisor:terminate_child(Pid, macula_content_store),
                supervisor:terminate_child(Pid, macula_content_transfer),
                exit(Pid, shutdown),
                timer:sleep(100)
            catch _:_ -> ok
            end
    end,
    %% Cleanup temp directory
    os:cmd("rm -rf " ++ TempDir),
    ok.

%%%===================================================================
%%% Non-Process Tests
%%%===================================================================

supervisor_children() ->
    ChildSpecs = macula_content_system:child_specs(#{}),
    ChildIds = [maps:get(id, Spec) || Spec <- ChildSpecs],
    ?assert(lists:member(macula_content_store, ChildIds)),
    ?assert(lists:member(macula_content_transfer, ChildIds)).

child_specs_valid() ->
    Specs = macula_content_system:child_specs(#{}),

    %% Each spec should have required fields
    lists:foreach(fun(Spec) ->
        ?assert(maps:is_key(id, Spec)),
        ?assert(maps:is_key(start, Spec)),
        {M, F, _A} = maps:get(start, Spec),
        ?assert(is_atom(M)),
        ?assert(is_atom(F))
    end, Specs).

strategy_one_for_one() ->
    {ok, Strategy} = macula_content_system:strategy(),
    ?assertEqual(one_for_one, Strategy).

%%%===================================================================
%%% Process Tests (with setup/teardown)
%%%===================================================================

supervisor_starts_correctly(TempDir) ->
    fun() ->
        {ok, Pid} = macula_content_system:start_link(#{store_path => TempDir}),
        ?assert(is_pid(Pid)),
        ?assert(is_process_alive(Pid))
    end.

supervisor_has_children(TempDir) ->
    fun() ->
        {ok, SupPid} = macula_content_system:start_link(#{store_path => TempDir}),
        Children = supervisor:which_children(SupPid),
        ?assert(length(Children) >= 2)
    end.
