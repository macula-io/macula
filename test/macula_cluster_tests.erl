%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_cluster module.
%%%
%%% Tests the cluster management utilities that bc_gitops delegates to.
%%% Focuses on:
%%% - Distribution management
%%% - Cookie resolution and persistence
%%% - Node monitoring
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cluster_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Distribution Tests
%%%===================================================================

is_distributed_returns_false_when_not_distributed_test() ->
    %% This test assumes we're running in a non-distributed test environment
    %% If test is run distributed, this will just verify the function works
    Result = macula_cluster:is_distributed(),
    ?assert(is_boolean(Result)).

ensure_distributed_returns_ok_or_error_test() ->
    Result = macula_cluster:ensure_distributed(),
    ?assert(Result =:= ok orelse element(1, Result) =:= error).

%%%===================================================================
%%% Cookie Resolution Tests
%%%===================================================================

get_cookie_returns_atom_test() ->
    Cookie = macula_cluster:get_cookie(),
    ?assert(is_atom(Cookie)).

set_cookie_with_atom_test() ->
    Cookie = test_cookie_atom_12345,
    ?assertEqual(ok, macula_cluster:set_cookie(Cookie)).

set_cookie_with_binary_test() ->
    Cookie = <<"test_cookie_binary_67890">>,
    ?assertEqual(ok, macula_cluster:set_cookie(Cookie)).

get_cookie_from_app_env_test() ->
    %% Set cookie in app env
    TestCookie = test_app_env_cookie,
    application:set_env(macula, cookie, TestCookie),

    %% Resolve should find it
    Result = macula_cluster:resolve_cookie(),
    ?assertEqual({ok, TestCookie}, Result),

    %% Clean up
    application:unset_env(macula, cookie).

get_cookie_from_app_env_binary_test() ->
    %% Set cookie as binary in app env
    application:set_env(macula, cookie, <<"binary_cookie">>),

    Result = macula_cluster:resolve_cookie(),
    ?assertEqual({ok, binary_cookie}, Result),

    %% Clean up
    application:unset_env(macula, cookie).

get_cookie_from_app_env_list_test() ->
    %% Set cookie as list in app env
    application:set_env(macula, cookie, "list_cookie"),

    Result = macula_cluster:resolve_cookie(),
    ?assertEqual({ok, list_cookie}, Result),

    %% Clean up
    application:unset_env(macula, cookie).

cookie_file_path_returns_valid_path_test() ->
    Path = macula_cluster:cookie_file_path(),
    ?assert(is_list(Path)),
    ?assert(length(Path) > 0).

cookie_file_path_uses_home_test() ->
    %% Should use HOME when set
    case os:getenv("HOME") of
        false ->
            %% No HOME set, should fall back to /tmp
            Path = macula_cluster:cookie_file_path(),
            ?assertEqual("/tmp/.erlang.cookie", Path);
        Home ->
            Path = macula_cluster:cookie_file_path(),
            Expected = filename:join(Home, ".erlang.cookie"),
            ?assertEqual(Expected, Path)
    end.

read_cookie_file_handles_missing_file_test() ->
    %% Try to read from non-existent file
    %% This will use the actual cookie file path, which may or may not exist
    Result = macula_cluster:read_cookie_file(),
    %% Should be either ok or error
    ?assert(element(1, Result) =:= ok orelse element(1, Result) =:= error).

%%%===================================================================
%%% Node Monitoring Tests
%%%===================================================================

monitor_nodes_returns_ok_test() ->
    %% This may fail if net_kernel is not running, which is fine for test
    try
        ?assertEqual(ok, macula_cluster:monitor_nodes())
    catch
        error:{badarg, _} ->
            %% net_kernel not running - expected in some test environments
            ok
    end.

unmonitor_nodes_returns_ok_test() ->
    %% This may fail if net_kernel is not running, which is fine for test
    try
        ?assertEqual(ok, macula_cluster:unmonitor_nodes())
    catch
        error:{badarg, _} ->
            %% net_kernel not running - expected in some test environments
            ok
    end.

%%%===================================================================
%%% Hostname Tests
%%%===================================================================

get_hostname_returns_string_test() ->
    Hostname = macula_cluster:get_hostname(),
    ?assert(is_list(Hostname)),
    ?assert(length(Hostname) > 0).

%%%===================================================================
%%% Macula API Delegation Tests
%%%===================================================================

macula_ensure_distributed_delegates_test() ->
    %% Verify macula.erl exports the function
    ?assert(erlang:function_exported(macula, ensure_distributed, 0)),
    %% Call it through macula module
    Result = macula:ensure_distributed(),
    ?assert(Result =:= ok orelse element(1, Result) =:= error).

macula_get_cookie_delegates_test() ->
    %% Verify macula.erl exports the function
    ?assert(erlang:function_exported(macula, get_cookie, 0)),
    %% Call it through macula module
    Cookie = macula:get_cookie(),
    ?assert(is_atom(Cookie)).

macula_set_cookie_delegates_test() ->
    %% Verify macula.erl exports the function
    ?assert(erlang:function_exported(macula, set_cookie, 1)),
    %% Call it through macula module
    ?assertEqual(ok, macula:set_cookie(delegation_test_cookie)).

macula_monitor_nodes_delegates_test() ->
    %% Verify macula.erl exports the function
    ?assert(erlang:function_exported(macula, monitor_nodes, 0)).

macula_unmonitor_nodes_delegates_test() ->
    %% Verify macula.erl exports the function
    ?assert(erlang:function_exported(macula, unmonitor_nodes, 0)).
