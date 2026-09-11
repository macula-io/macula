%% EUnit tests for macula_test_tmp: temporary directories unique across test runs, made fresh, and removed with
%% what they hold. Not named macula_test_tmp_tests: eunit would also run a module of that name as the
%% companion of macula_test_tmp, which is in the same test directory, and so run these tests twice.
-module(macula_test_tmp_dirs_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

two_directories_are_different_empty_and_readable_by_their_owner_only_test() ->
    First = macula_test_tmp:dir("macula_test_tmp_tests"),
    Second = macula_test_tmp:dir("macula_test_tmp_tests"),
    try
        ?assertNotEqual(First, Second),
        ?assertEqual({{ok, []}, {ok, []}}, {file:list_dir(First), file:list_dir(Second)}),
        ?assertEqual({8#700, 8#700}, {mode(First), mode(Second)}),
        ?assertEqual({"macula_test_tmp_tests-", "macula_test_tmp_tests-"},
                     {lists:sublist(filename:basename(First), 22), lists:sublist(filename:basename(Second), 22)})
    after
        ok = file:del_dir_r(First),
        ok = file:del_dir_r(Second)
    end.

%% The second directory draws the same name as the first once, finds it taken, and takes a new name.
an_existing_directory_is_never_reused_test() ->
    ok = meck:new(macula_test_tmp, [passthrough]),
    try
        ok = meck:expect(macula_test_tmp, unique_part, 0, meck:seq(["repeated", "repeated", "fresh"])),
        First = macula_test_tmp:dir("macula_test_tmp_tests"),
        ok = file:write_file(filename:join(First, "left behind"), <<>>),
        Second = macula_test_tmp:dir("macula_test_tmp_tests"),
        try
            ?assertNotEqual(First, Second),
            ?assertEqual({ok, []}, file:list_dir(Second)),
            ?assertEqual("macula_test_tmp_tests-fresh", filename:basename(Second))
        after
            ok = file:del_dir_r(First),
            ok = file:del_dir_r(Second)
        end
    after
        meck:unload(macula_test_tmp)
    end.

a_directory_is_removed_with_what_it_holds_once_the_function_returns_test() ->
    {Dir, Result} = macula_test_tmp:with_dir("macula_test_tmp_tests", fun filled/1),
    ?assertEqual({filled, false}, {Result, filelib:is_dir(Dir)}).

a_directory_is_removed_with_what_it_holds_when_the_function_raises_test() ->
    Test = self(),
    ?assertError(raised, macula_test_tmp:with_dir("macula_test_tmp_tests", fun(Made) -> raised(Test, Made) end)),
    Dir = receive {filled, Filled} -> Filled after 0 -> error(no_directory) end,
    ?assertNot(filelib:is_dir(Dir)).

%% A file, and a subdirectory holding another, in Dir.
filled(Dir) ->
    ok = file:write_file(filename:join(Dir, "a file"), <<"bytes">>),
    ok = file:make_dir(filename:join(Dir, "a subdirectory")),
    ok = file:write_file(filename:join([Dir, "a subdirectory", "another file"]), <<"bytes">>),
    {Dir, filled}.

raised(Test, Dir) ->
    {Dir, filled} = filled(Dir),
    Test ! {filled, Dir},
    error(raised).

mode(Path) ->
    {ok, #file_info{mode = Mode}} = file:read_file_info(Path),
    Mode band 8#777.
