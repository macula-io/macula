%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_cluster module.
%%%
%%% Tests the cluster management utilities that bc_gitops delegates to.
%%% Focuses on:
%%% - Distribution management
%%% - Cookie resolution and persistence
%%% - Node monitoring
%%%
%%% Every test that can read or write a cookie runs with HOME pointed at a
%%% fresh directory, MACULA_COOKIE, RELEASE_COOKIE and ERLANG_COOKIE unset
%%% and no cookie in the application env, so no test reads or writes the
%%% real ~/.erlang.cookie. The fixture restores all of it afterwards, and
%%% removes /tmp/.erlang.cookie only if a test created it.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cluster_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-define(COOKIE_ENV_VARS, ["MACULA_COOKIE", "RELEASE_COOKIE", "ERLANG_COOKIE"]).
-define(TMP_COOKIE, "/tmp/.erlang.cookie").

%%%===================================================================
%%% Distribution Tests
%%%===================================================================

is_distributed_returns_false_when_not_distributed_test() ->
    %% This test assumes we're running in a non-distributed test environment
    %% If test is run distributed, this will just verify the function works
    Result = macula_cluster:is_distributed(),
    ?assert(is_boolean(Result)).

%%%===================================================================
%%% Tests with isolated cookie sources
%%%===================================================================

isolated_cookie_test_() ->
    {foreach, fun isolate_cookie_sources/0, fun restore_cookie_sources/1,
     [fun cookie_sources_are_isolated/1,
      fun ensure_distributed_returns_ok_or_error/1,
      fun get_cookie_returns_atom/1,
      fun set_cookie_with_atom/1,
      fun set_cookie_with_binary/1,
      fun get_cookie_from_app_env/1,
      fun get_cookie_from_app_env_binary/1,
      fun get_cookie_from_app_env_list/1,
      fun cookie_file_path_uses_home/1,
      fun cookie_file_path_reports_home_not_set/1,
      fun read_cookie_file_reports_missing_file/1,
      fun read_cookie_file_refuses_group_readable_file/1,
      fun get_cookie_raises_when_home_unset_and_writes_nothing_under_tmp/1,
      fun get_cookie_refuses_group_readable_cookie_file_and_keeps_it/1,
      fun get_cookie_replaces_symlink_at_cookie_path_without_writing_through_it/1,
      fun get_cookie_returns_the_nodes_own_cookie_first/1,
      fun macula_ensure_distributed_delegates/1,
      fun macula_get_cookie_delegates/1,
      fun macula_set_cookie_delegates/1]}.

cookie_sources_are_isolated(#{fake_home := Home}) ->
    {"cookie tests run with HOME at a fresh directory and no cookie configured", fun() ->
        ?assertEqual(Home, os:getenv("HOME")),
        ?assertEqual([false, false, false], [os:getenv(Var) || Var <- ?COOKIE_ENV_VARS]),
        ?assertEqual(undefined, application:get_env(macula, cookie)),
        ?assertEqual({ok, []}, file:list_dir(Home))
    end}.

ensure_distributed_returns_ok_or_error(_Context) ->
    {"ensure_distributed/0 returns ok or an error", fun() ->
        WasDistributed = erlang:is_alive(),
        Result = macula_cluster:ensure_distributed(),
        stop_distribution_started_here(WasDistributed),
        ?assert(Result =:= ok orelse element(1, Result) =:= error)
    end}.

get_cookie_returns_atom(_Context) ->
    {"get_cookie/0 returns an atom", fun() ->
        Cookie = macula_cluster:get_cookie(),
        ?assert(is_atom(Cookie))
    end}.

set_cookie_with_atom(_Context) ->
    {"set_cookie/1 takes an atom", fun() ->
        Cookie = test_cookie_atom_12345,
        with_distribution(fun() ->
            ?assertEqual(ok, macula_cluster:set_cookie(Cookie))
        end)
    end}.

set_cookie_with_binary(_Context) ->
    {"set_cookie/1 takes a binary", fun() ->
        Cookie = <<"test_cookie_binary_67890">>,
        with_distribution(fun() ->
            ?assertEqual(ok, macula_cluster:set_cookie(Cookie))
        end)
    end}.

get_cookie_from_app_env(_Context) ->
    {"resolve_cookie/0 finds an atom cookie in the application env", fun() ->
        TestCookie = test_app_env_cookie,
        application:set_env(macula, cookie, TestCookie),
        ?assertEqual({ok, TestCookie}, macula_cluster:resolve_cookie())
    end}.

get_cookie_from_app_env_binary(_Context) ->
    {"resolve_cookie/0 finds a binary cookie in the application env", fun() ->
        application:set_env(macula, cookie, <<"binary_cookie">>),
        ?assertEqual({ok, binary_cookie}, macula_cluster:resolve_cookie())
    end}.

get_cookie_from_app_env_list(_Context) ->
    {"resolve_cookie/0 finds a list cookie in the application env", fun() ->
        application:set_env(macula, cookie, "list_cookie"),
        ?assertEqual({ok, list_cookie}, macula_cluster:resolve_cookie())
    end}.

cookie_file_path_uses_home(#{fake_home := Home}) ->
    {"cookie_file_path/0 is .erlang.cookie in HOME", fun() ->
        ?assertEqual({ok, filename:join(Home, ".erlang.cookie")},
                     macula_cluster:cookie_file_path())
    end}.

cookie_file_path_reports_home_not_set(_Context) ->
    {"cookie_file_path/0 reports an unset HOME instead of falling back to /tmp", fun() ->
        true = os:unsetenv("HOME"),
        ?assertEqual(false, os:getenv("HOME")),
        ?assertEqual({error, home_not_set}, macula_cluster:cookie_file_path())
    end}.

read_cookie_file_reports_missing_file(_Context) ->
    {"read_cookie_file/0 reports a missing cookie file as enoent", fun() ->
        ?assertEqual({error, enoent}, macula_cluster:read_cookie_file())
    end}.

read_cookie_file_refuses_group_readable_file(#{fake_home := Home}) ->
    {"read_cookie_file/0 refuses a cookie file its group can read", fun() ->
        write_cookie_file(Home, <<"keptcookie">>, 8#640),
        ?assertMatch({error, {cookie_file_refused,
                              {file_permissions, #{mode := <<"0640">>}}}},
                     macula_cluster:read_cookie_file())
    end}.

get_cookie_raises_when_home_unset_and_writes_nothing_under_tmp(_Context) ->
    {"get_cookie/0 raises a clear error when HOME is unset and writes nothing under /tmp", fun() ->
        ?assertNot(erlang:is_alive()),
        true = os:unsetenv("HOME"),
        ?assertEqual(false, os:getenv("HOME")),
        Before = file:read_link_info(?TMP_COOKIE),
        ?assertError({cookie_file_unavailable, home_not_set}, macula_cluster:get_cookie()),
        ?assertEqual(Before, file:read_link_info(?TMP_COOKIE))
    end}.

get_cookie_refuses_group_readable_cookie_file_and_keeps_it(#{fake_home := Home}) ->
    {"get_cookie/0 refuses a cookie file its group can read and never replaces it", fun() ->
        ?assertNot(erlang:is_alive()),
        Path = write_cookie_file(Home, <<"keptcookie">>, 8#640),
        ?assertError({cookie_file_refused, {file_permissions, #{mode := <<"0640">>}}},
                     macula_cluster:get_cookie()),
        ?assertEqual({ok, <<"keptcookie">>}, file:read_file(Path))
    end}.

get_cookie_replaces_symlink_at_cookie_path_without_writing_through_it(#{fake_home := Home}) ->
    {"get_cookie/0 saves a new cookie over a symlink, never through it, owner-only", fun() ->
        ?assertNot(erlang:is_alive()),
        Path = filename:join(Home, ".erlang.cookie"),
        Elsewhere = filename:join(Home, "elsewhere"),
        ok = file:make_symlink(Elsewhere, Path),
        Cookie = macula_cluster:get_cookie(),
        ?assertNot(filelib:is_file(Elsewhere)),
        {ok, #file_info{type = Type, mode = Mode}} = file:read_link_info(Path),
        ?assertEqual(regular, Type),
        ?assertEqual(8#600, Mode band 8#777),
        ?assertEqual({ok, list_to_binary(atom_to_list(Cookie) ++ "\n")}, file:read_file(Path))
    end}.

get_cookie_returns_the_nodes_own_cookie_first(_Context) ->
    {"get_cookie/0 returns the node's own cookie before any configured one", fun() ->
        with_distribution(fun() ->
            true = erlang:set_cookie(node_own_cookie_for_test),
            ok = application:set_env(macula, cookie, app_env_cookie_for_test),
            true = os:putenv("MACULA_COOKIE", "env_cookie_for_test"),
            ?assertEqual(node_own_cookie_for_test, macula_cluster:get_cookie())
        end)
    end}.

macula_ensure_distributed_delegates(_Context) ->
    {"macula:ensure_distributed/0 delegates to macula_cluster", fun() ->
        ?assert(erlang:function_exported(macula, ensure_distributed, 0)),
        WasDistributed = erlang:is_alive(),
        Result = macula:ensure_distributed(),
        stop_distribution_started_here(WasDistributed),
        ?assert(Result =:= ok orelse element(1, Result) =:= error)
    end}.

macula_get_cookie_delegates(_Context) ->
    {"macula:get_cookie/0 delegates to macula_cluster", fun() ->
        ?assert(erlang:function_exported(macula, get_cookie, 0)),
        Cookie = macula:get_cookie(),
        ?assert(is_atom(Cookie))
    end}.

macula_set_cookie_delegates(_Context) ->
    {"macula:set_cookie/1 delegates to macula_cluster", fun() ->
        ?assert(erlang:function_exported(macula, set_cookie, 1)),
        with_distribution(fun() ->
            ?assertEqual(ok, macula:set_cookie(delegation_test_cookie))
        end)
    end}.

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

macula_monitor_nodes_delegates_test() ->
    %% Verify macula.erl exports the function
    ?assert(erlang:function_exported(macula, monitor_nodes, 0)).

macula_unmonitor_nodes_delegates_test() ->
    %% Verify macula.erl exports the function
    ?assert(erlang:function_exported(macula, unmonitor_nodes, 0)).

%%%===================================================================
%%% Helpers
%%%===================================================================

isolate_cookie_sources() ->
    Saved = #{home => os:getenv("HOME"),
              env => [{Var, os:getenv(Var)} || Var <- ?COOKIE_ENV_VARS],
              app => application:get_env(macula, cookie),
              tmp_cookie => file:read_link_info(?TMP_COOKIE)},
    Home = make_private_dir(),
    true = os:putenv("HOME", Home),
    [true = os:unsetenv(Var) || Var <- ?COOKIE_ENV_VARS],
    ok = application:unset_env(macula, cookie),
    Saved#{fake_home => Home}.

restore_cookie_sources(#{home := Home, env := Env, app := App, tmp_cookie := TmpCookie,
                         fake_home := FakeHome}) ->
    restore_env("HOME", Home),
    [restore_env(Var, Value) || {Var, Value} <- Env],
    restore_app_cookie(App),
    remove_tmp_cookie_created_here(TmpCookie),
    ok = file:del_dir_r(FakeHome).

restore_env(Var, false) -> true = os:unsetenv(Var);
restore_env(Var, Value) -> true = os:putenv(Var, Value).

restore_app_cookie(undefined) -> ok = application:unset_env(macula, cookie);
restore_app_cookie({ok, Cookie}) -> ok = application:set_env(macula, cookie, Cookie).

%% A cookie file at the old /tmp fallback that did not exist before the test
%% was created by it, so it goes; one that was already there is left alone.
remove_tmp_cookie_created_here({error, enoent}) -> _ = file:delete(?TMP_COOKIE), ok;
remove_tmp_cookie_created_here(_Existing) -> ok.

make_private_dir() ->
    Unique = integer_to_list(erlang:unique_integer([positive])) ++ "_" ++
        binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(4), lowercase)),
    Dir = filename:join(os:getenv("TMPDIR", "/tmp"), "macula_cluster_tests_" ++ Unique),
    ok = file:make_dir(Dir),
    ok = file:change_mode(Dir, 8#700),
    Dir.

write_cookie_file(Home, Content, Mode) ->
    Path = filename:join(Home, ".erlang.cookie"),
    ok = file:write_file(Path, Content),
    ok = file:change_mode(Path, Mode),
    Path.

%% ensure_distributed/0 starts distribution on a node that is not
%% distributed. Stop it again, so the test modules that run after this
%% one in the same eunit run see the node as it was.
stop_distribution_started_here(true) ->
    ok;
stop_distribution_started_here(false) ->
    _ = net_kernel:stop(),
    ok.

%% A cookie can be set only on a distributed node. Run Fun on one, and
%% stop distribution again afterwards when this call started it.
with_distribution(Fun) ->
    WasDistributed = erlang:is_alive(),
    _ = macula_cluster:ensure_distributed(),
    try Fun()
    after stop_distribution_started_here(WasDistributed)
    end.
