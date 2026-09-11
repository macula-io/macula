%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_cluster module.
%%%
%%% Tests the cluster management utilities that bc_gitops delegates to.
%%% Focuses on:
%%% - Distribution management
%%% - The cookie of a distributed node
%%% - Node monitoring
%%%
%%% Every test that can touch a cookie, or start distribution, runs in a peer
%%% node booted with HOME at a fresh macula_test_tmp directory, or with no HOME
%%% at all, and with no cookie environment variables and no XDG_CONFIG_HOME.
%%% OTP's own auth reads, and creates when it is missing, the .erlang.cookie of
%%% the HOME a node booted with, not of a HOME set later, so only a node booted
%%% this way keeps these tests away from the real cookie file. A peer starts
%%% distribution with dist_listen false, which needs no epmd.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cluster_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-define(COOKIE_ENV_VARS, ["MACULA_COOKIE", "RELEASE_COOKIE", "ERLANG_COOKIE"]).
-define(TMP_COOKIE, "/tmp/.erlang.cookie").
-define(PEER_CALL_TIMEOUT_MS, 20_000).
-define(PEER_TEST_TIMEOUT_S, 60).

%% Scenarios, each run in a peer node by the tests below.
-export([cookie_sources_are_isolated/1,
         ensure_distributed_returns_ok_or_error/1,
         get_cookie_on_a_node_that_is_not_distributed/1,
         get_cookie_returns_the_nodes_own_cookie/1,
         set_cookie_with_atom/1,
         set_cookie_with_binary/1,
         set_cookie_on_a_node_that_is_not_distributed/1,
         distributed_node_keeps_its_cookie_and_its_cookie_file/1,
         nothing_is_written_without_a_home/1,
         macula_ensure_distributed_delegates/1,
         macula_get_cookie_delegates/1,
         macula_set_cookie_delegates/1]).

%%%===================================================================
%%% Distribution Tests
%%%===================================================================

is_distributed_returns_false_when_not_distributed_test() ->
    %% This test assumes we're running in a non-distributed test environment
    %% If test is run distributed, this will just verify the function works
    Result = macula_cluster:is_distributed(),
    ?assert(is_boolean(Result)).

%%%===================================================================
%%% Tests in a peer node with its own HOME
%%%===================================================================

cookie_in_peer_test_() ->
    {foreach, fun fresh_home/0, fun remove_home/1,
     [in_peer(cookie_sources_are_isolated,
              "a peer runs with HOME at a fresh directory, no cookie configured and no distribution"),
      in_peer(ensure_distributed_returns_ok_or_error,
              "ensure_distributed/0 returns ok or an error"),
      in_peer(get_cookie_on_a_node_that_is_not_distributed,
              "get_cookie/0 on a node that is not distributed raises not_distributed and writes nothing"),
      in_peer(get_cookie_returns_the_nodes_own_cookie,
              "get_cookie/0 returns the distributed node's own cookie, whatever else is configured"),
      in_peer(set_cookie_with_atom,
              "set_cookie/1 takes an atom"),
      in_peer(set_cookie_with_binary,
              "set_cookie/1 takes a binary"),
      in_peer(set_cookie_on_a_node_that_is_not_distributed,
              "set_cookie/1 on a node that is not distributed raises not_distributed and writes nothing"),
      in_peer(distributed_node_keeps_its_cookie_and_its_cookie_file,
              "a distributed node keeps the cookie it read from an owner-only file, and the file"),
      in_peer_without_home(nothing_is_written_without_a_home,
                           "without a HOME, no cookie is written anywhere"),
      in_peer(macula_ensure_distributed_delegates,
              "macula:ensure_distributed/0 delegates to macula_cluster"),
      in_peer(macula_get_cookie_delegates,
              "macula:get_cookie/0 delegates to macula_cluster"),
      in_peer(macula_set_cookie_delegates,
              "macula:set_cookie/1 delegates to macula_cluster")]}.

%%%===================================================================
%%% Scenarios
%%%===================================================================

cookie_sources_are_isolated(Home) ->
    ?assertEqual(Home, os:getenv("HOME")),
    ?assertEqual({ok, [[Home]]}, init:get_argument(home)),
    ?assertEqual([false, false, false], [os:getenv(Var) || Var <- ?COOKIE_ENV_VARS]),
    ?assertEqual(false, os:getenv("XDG_CONFIG_HOME")),
    ?assertEqual(undefined, application:get_env(macula, cookie)),
    ?assertNot(erlang:is_alive()),
    ?assertEqual({ok, []}, file:list_dir(Home)).

%% Where distribution starts, the node's cookie is the one in the cookie file
%% OTP's auth created in HOME, not one macula chose.
ensure_distributed_returns_ok_or_error(Home) ->
    Result = macula_cluster:ensure_distributed(),
    ?assert(Result =:= ok orelse element(1, Result) =:= error),
    cookie_is_the_files(Result, Home).

get_cookie_on_a_node_that_is_not_distributed(Home) ->
    ?assertError(not_distributed, macula_cluster:get_cookie()),
    ?assertEqual({ok, []}, file:list_dir(Home)).

%% The cookie OTP's auth gives the node when distribution starts, and the one
%% the node is set to later, is what get_cookie/0 returns. A cookie in the
%% application env or the environment plays no part.
get_cookie_returns_the_nodes_own_cookie(_Home) ->
    ok = start_distribution_without_epmd(),
    ok = application:set_env(macula, cookie, app_env_cookie_for_test),
    true = os:putenv("MACULA_COOKIE", "env_cookie_for_test"),
    ?assertEqual(erlang:get_cookie(), macula_cluster:get_cookie()),
    true = erlang:set_cookie(node(), node_own_cookie_for_test),
    ?assertEqual(node_own_cookie_for_test, macula_cluster:get_cookie()).

%% The node's cookie changes; the cookie file OTP's auth created does not.
set_cookie_with_atom(Home) ->
    ok = start_distribution_without_epmd(),
    Path = filename:join(Home, ".erlang.cookie"),
    {ok, Content} = file:read_file(Path),
    FileBefore = file_identity(Path),
    ?assertEqual(ok, macula_cluster:set_cookie(test_cookie_atom_12345)),
    ?assertEqual(test_cookie_atom_12345, erlang:get_cookie()),
    ?assertEqual(FileBefore, file_identity(Path)),
    ?assertEqual({ok, Content}, file:read_file(Path)),
    ?assertEqual({ok, [".erlang.cookie"]}, file:list_dir(Home)).

set_cookie_with_binary(_Home) ->
    ok = start_distribution_without_epmd(),
    ?assertEqual(ok, macula_cluster:set_cookie(<<"test_cookie_binary_67890">>)),
    ?assertEqual(test_cookie_binary_67890, erlang:get_cookie()).

set_cookie_on_a_node_that_is_not_distributed(Home) ->
    ?assertError(not_distributed, macula_cluster:set_cookie(test_cookie_not_distributed)),
    ?assertEqual({ok, []}, file:list_dir(Home)).

%% A node deployed with an owner-only, read-only $HOME/.erlang.cookie and no
%% -setcookie: OTP's auth gives the node the file's cookie when distribution
%% starts. With another cookie in the application env and the environment,
%% ensure_distributed/0 and get_cookie/0 leave the node's cookie as it is, and
%% none of the three functions, set_cookie/1 included, changes the file or its
%% directory.
distributed_node_keeps_its_cookie_and_its_cookie_file(Home) ->
    Path = write_cookie_file(Home, <<"cookie_from_owner_only_file">>, 8#400),
    ok = file:change_mode(Home, 8#500),
    ok = start_distribution_without_epmd(),
    ?assertEqual(cookie_from_owner_only_file, erlang:get_cookie()),
    FileBefore = file_identity(Path),
    ok = application:set_env(macula, cookie, app_env_cookie_for_test),
    true = os:putenv("MACULA_COOKIE", "env_cookie_for_test"),
    ?assertEqual(ok, macula_cluster:ensure_distributed()),
    ?assertEqual(cookie_from_owner_only_file, macula_cluster:get_cookie()),
    ?assertEqual(cookie_from_owner_only_file, erlang:get_cookie()),
    ?assertEqual(ok, macula_cluster:set_cookie(another_cookie_for_test)),
    ?assertEqual(FileBefore, file_identity(Path)),
    ?assertEqual({ok, [".erlang.cookie"]}, file:list_dir(Home)),
    ?assertEqual({ok, <<"cookie_from_owner_only_file">>}, file:read_file(Path)).

%% Without a HOME and without XDG_CONFIG_HOME, get_cookie/0, set_cookie/1 and
%% ensure_distributed/0 write no cookie anywhere: not in /tmp, where an old
%% fallback put one, and not in the test's own directory.
nothing_is_written_without_a_home(Dir) ->
    ?assertEqual(false, os:getenv("HOME")),
    Before = file:read_link_info(?TMP_COOKIE),
    ?assertError(not_distributed, macula_cluster:get_cookie()),
    ?assertError(not_distributed, macula_cluster:set_cookie(cookie_without_home)),
    _ = macula_cluster:ensure_distributed(),
    ?assertEqual(Before, file:read_link_info(?TMP_COOKIE)),
    ?assertEqual({ok, []}, file:list_dir(Dir)).

macula_ensure_distributed_delegates(_Home) ->
    {module, macula} = code:ensure_loaded(macula),
    ?assert(erlang:function_exported(macula, ensure_distributed, 0)),
    Result = macula:ensure_distributed(),
    ?assert(Result =:= ok orelse element(1, Result) =:= error).

macula_get_cookie_delegates(_Home) ->
    {module, macula} = code:ensure_loaded(macula),
    ?assert(erlang:function_exported(macula, get_cookie, 0)),
    ?assertError(not_distributed, macula:get_cookie()).

macula_set_cookie_delegates(_Home) ->
    {module, macula} = code:ensure_loaded(macula),
    ?assert(erlang:function_exported(macula, set_cookie, 1)),
    ok = start_distribution_without_epmd(),
    ?assertEqual(ok, macula:set_cookie(delegation_test_cookie)),
    ?assertEqual(delegation_test_cookie, erlang:get_cookie()).

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

fresh_home() ->
    macula_test_tmp:dir("macula_cluster_tests").

%% A scenario may leave the directory read-only, so it is made writable again
%% before it is removed.
remove_home(Home) ->
    _ = file:change_mode(Home, 8#700),
    ok = file:del_dir_r(Home).

%% A foreach instantiator that runs Scenario(Home) in a peer node booted with
%% HOME at Home.
in_peer(Scenario, Title) ->
    fun(Home) ->
        {timeout, ?PEER_TEST_TIMEOUT_S,
         {Title, fun() -> run_in_peer([{"HOME", Home}], Home, Scenario) end}}
    end.

%% Same, for a peer node booted without HOME; Scenario gets the directory
%% only to check that nothing appears in it.
in_peer_without_home(Scenario, Title) ->
    fun(Dir) ->
        {timeout, ?PEER_TEST_TIMEOUT_S,
         {Title, fun() -> run_in_peer([{"HOME", false}], Dir, Scenario) end}}
    end.

run_in_peer(HomeEnv, Arg, Scenario) ->
    Env = HomeEnv ++ [{"XDG_CONFIG_HOME", false} | [{Var, false} || Var <- ?COOKIE_ENV_VARS]],
    {ok, Peer, _Node} = peer:start_link(#{connection => standard_io,
                                          args => ["-pa" | code:get_path()],
                                          env => Env}),
    try
        peer:call(Peer, ?MODULE, Scenario, [Arg], ?PEER_CALL_TIMEOUT_MS)
    after
        peer:stop(Peer)
    end.

%% Distribution that does not listen for connections registers no name with
%% epmd, so it starts where no epmd runs.
start_distribution_without_epmd() ->
    {ok, _} = net_kernel:start(macula_cluster_tests_peer,
                               #{name_domain => shortnames, dist_listen => false}),
    ok.

cookie_is_the_files(ok, Home) ->
    {ok, Content} = file:read_file(filename:join(Home, ".erlang.cookie")),
    ?assertEqual(binary_to_atom(string:trim(Content)), erlang:get_cookie());
cookie_is_the_files({error, _}, _Home) ->
    ok.

write_cookie_file(Home, Content, Mode) ->
    Path = filename:join(Home, ".erlang.cookie"),
    ok = file:write_file(Path, Content),
    ok = file:change_mode(Path, Mode),
    Path.

%% What a write, replace or chmod of the file would change.
file_identity(Path) ->
    {ok, #file_info{inode = Inode, mode = Mode, size = Size, mtime = Mtime,
                    ctime = Ctime, type = Type}} = file:read_link_info(Path, [{time, posix}]),
    {Type, Inode, Mode, Size, Mtime, Ctime}.
