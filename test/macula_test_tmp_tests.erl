%% EUnit tests for macula_test_tmp: temporary paths unique across test runs, and directories made fresh.
-module(macula_test_tmp_tests).

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

a_file_path_is_unique_and_ends_in_its_extension_test() ->
    First = macula_test_tmp:file("macula_test_tmp_tests", ".crt"),
    Second = macula_test_tmp:file("macula_test_tmp_tests", ".crt"),
    ?assertNotEqual(First, Second),
    ?assertEqual({".crt", false}, {filename:extension(First), filelib:is_file(First)}).

mode(Path) ->
    {ok, #file_info{mode = Mode}} = file:read_file_info(Path),
    Mode band 8#777.
