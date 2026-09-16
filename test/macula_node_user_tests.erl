%%%-------------------------------------------------------------------
%%% @doc macula_node_user:effective_uid/0 names the user the node runs as.
%%%
%%% On a host with user ids it is the owner of a file this node creates; on a
%%% host without them it is none, and checks that compare owners are skipped.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_node_user_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

effective_uid_owns_the_files_this_node_creates_test() ->
    macula_test_tmp:with_dir("macula_node_user_tests", fun(Dir) ->
        Path = filename:join(Dir, "created"),
        ok = file:write_file(Path, <<"x">>),
        {ok, #file_info{uid = Uid}} = file:read_file_info(Path),
        ?assertEqual(expected_uid(os:type(), Uid), macula_node_user:effective_uid())
    end).

expected_uid({unix, _}, Uid) -> Uid;
expected_uid({win32, _}, _Uid) -> none.
