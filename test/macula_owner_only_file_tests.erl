%%%-------------------------------------------------------------------
%%% @doc EUnit tests for macula_owner_only_file: files only their owner
%%% can read, written atomically.
%%%
%%% Each test runs in its own directory, created with mode 0700 under the
%%% system temporary directory and removed afterwards.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_owner_only_file_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-define(BASE_PREFIX, "macula_owner_only_file_tests_").
-define(REQUIRED_MODE, <<"no access for group or others (0600 or 0400)">>).
-define(WATCHED_WRITES, 20).
-define(WATCHED_CONTENT_BYTES, 8 * 1024 * 1024).

owner_only_file_test_() ->
    {foreach, fun make_base/0, fun remove_base/1,
     [fun write_creates_owner_only_file/1,
      fun write_creates_missing_parents_innermost_owner_only/1,
      fun write_leaves_existing_parent_mode_unchanged/1,
      fun write_replaces_file_others_could_read/1,
      fun write_replaces_symlink_without_writing_through_it/1,
      fun write_ignores_symlink_at_fixed_temporary_name/1,
      fun write_fills_file_inside_owner_only_directory/1,
      fun write_leaves_no_temporary_file_after_success/1,
      fun write_removes_temporary_file_after_failure/1,
      fun read_returns_content_of_owner_only_file/1,
      fun read_accepts_owner_read_only_file/1,
      fun read_refuses_file_group_or_others_can_access/1,
      fun read_follows_symlink_to_owner_only_file/1,
      fun read_refuses_symlink_to_file_others_can_read/1,
      fun read_refuses_directory/1,
      fun read_reports_missing_file/1,
      fun read_reports_dangling_symlink/1]}.

%%%===================================================================
%%% write/2
%%%===================================================================

write_creates_owner_only_file(Base) ->
    {"write creates a regular file only its owner can read or write", fun() ->
        Path = filename:join(Base, "secret"),
        ?assertEqual(ok, macula_owner_only_file:write(Path, [<<"s3">>, "cret"])),
        ?assertEqual({ok, <<"s3cret">>}, file:read_file(Path)),
        ?assertEqual(regular, type(Path)),
        ?assertEqual(8#600, mode(Path))
    end}.

write_creates_missing_parents_innermost_owner_only(Base) ->
    {"write creates missing parent directories, the innermost usable by its owner only", fun() ->
        Dir = filename:join([Base, "outer", "inner"]),
        Path = filename:join(Dir, "secret"),
        ?assertEqual(ok, macula_owner_only_file:write(Path, <<"s3cret">>)),
        ?assertEqual(8#700, mode(Dir)),
        ?assertEqual(8#600, mode(Path))
    end}.

write_leaves_existing_parent_mode_unchanged(Base) ->
    {"write uses an existing parent directory as it is, whatever its mode", fun() ->
        Dir = make_dir_with_mode(Base, "shared_dir", 8#775),
        Path = filename:join(Dir, "secret"),
        ?assertEqual(ok, macula_owner_only_file:write(Path, <<"s3cret">>)),
        ?assertEqual(8#775, mode(Dir)),
        ?assertEqual(8#600, mode(Path))
    end}.

write_replaces_file_others_could_read(Base) ->
    {"write replaces a file others could read with one only its owner can read", fun() ->
        Path = write_with_mode(Base, "secret", 8#644),
        ?assertEqual(ok, macula_owner_only_file:write(Path, <<"new">>)),
        ?assertEqual({ok, <<"new">>}, file:read_file(Path)),
        ?assertEqual(8#600, mode(Path))
    end}.

write_replaces_symlink_without_writing_through_it(Base) ->
    {"write replaces a symlink at the path and leaves its target unchanged", fun() ->
        Target = write_with_mode(Base, "target", 8#644),
        Link = filename:join(Base, "secret"),
        ok = file:make_symlink(Target, Link),
        ?assertEqual(ok, macula_owner_only_file:write(Link, <<"new">>)),
        ?assertEqual(regular, type(Link)),
        ?assertEqual({ok, <<"new">>}, file:read_file(Link)),
        ?assertEqual(8#600, mode(Link)),
        ?assertEqual({ok, <<"s3cret">>}, file:read_file(Target)),
        ?assertEqual(8#644, mode(Target))
    end}.

write_ignores_symlink_at_fixed_temporary_name(Base) ->
    {"write never writes through a symlink planted at the path plus .tmp", fun() ->
        Target = write_with_mode(Base, "target", 8#644),
        Path = filename:join(Base, "secret"),
        Planted = Path ++ ".tmp",
        ok = file:make_symlink(Target, Planted),
        ?assertEqual(ok, macula_owner_only_file:write(Path, <<"new">>)),
        ?assertEqual(regular, type(Path)),
        ?assertEqual({ok, <<"new">>}, file:read_file(Path)),
        ?assertEqual({ok, <<"s3cret">>}, file:read_file(Target)),
        ?assertEqual(8#644, mode(Target)),
        ?assertEqual(symlink, type(Planted))
    end}.

%% A file starts with the default mode, and a descriptor another user opens
%% before a chmod keeps reading after it, so the file must never sit in a
%% directory others can use. A watcher lists the base directory while write/2
%% runs and records the mode of every temporary directory it finds holding a
%% file. Several large writes give it the time to see them.
write_fills_file_inside_owner_only_directory(Base) ->
    {"write fills its file inside a directory only its owner can use", fun() ->
        Path = filename:join(Base, "secret"),
        Content = binary:copy(<<0>>, ?WATCHED_CONTENT_BYTES),
        Watcher = spawn_link(fun() -> watch_temporary_directories(Base, #{}) end),
        Writes = [macula_owner_only_file:write(Path, Content) || _ <- lists:seq(1, ?WATCHED_WRITES)],
        Watcher ! {stop, self()},
        Modes = receive {modes_seen, Seen} -> Seen after 5000 -> error(watcher_silent) end,
        ?assertEqual([ok], lists:usort(Writes)),
        ?assertNotEqual([], maps:keys(Modes)),
        ?assertEqual([8#700], maps:keys(Modes))
    end}.

write_leaves_no_temporary_file_after_success(Base) ->
    {"write leaves no temporary file or directory behind after it succeeds", fun() ->
        Path = filename:join(Base, "secret"),
        ok = macula_owner_only_file:write(Path, <<"one">>),
        ok = macula_owner_only_file:write(Path, <<"two">>),
        ?assertEqual(["secret"], listing(Base))
    end}.

write_removes_temporary_file_after_failure(Base) ->
    {"write removes its temporary file and directory when it cannot replace the path", fun() ->
        Path = filename:join(Base, "occupied"),
        ok = file:make_dir(Path),
        ok = file:write_file(filename:join(Path, "inside"), <<"x">>),
        ?assertMatch({error, _}, macula_owner_only_file:write(Path, <<"s3cret">>)),
        ?assertEqual(["occupied"], listing(Base)),
        ?assertEqual(["inside"], listing(Path))
    end}.

%%%===================================================================
%%% read/1
%%%===================================================================

read_returns_content_of_owner_only_file(Base) ->
    {"read returns the content of a file only its owner can read", fun() ->
        Path = write_with_mode(Base, "secret", 8#600),
        ?assertEqual({ok, <<"s3cret">>}, macula_owner_only_file:read(Path))
    end}.

read_accepts_owner_read_only_file(Base) ->
    {"read accepts a file only its owner can read and nobody can write", fun() ->
        Path = write_with_mode(Base, "secret", 8#400),
        ?assertEqual({ok, <<"s3cret">>}, macula_owner_only_file:read(Path))
    end}.

read_refuses_file_group_or_others_can_access(Base) ->
    [{"read refuses a file with mode " ++ Octal ++ ", naming the file, its mode and the requirement",
      fun() ->
          Path = write_with_mode(Base, "secret_" ++ Octal, Mode),
          ?assertEqual({error, {file_permissions, #{file => Path,
                                                    mode => list_to_binary(Octal),
                                                    required => ?REQUIRED_MODE}}},
                       macula_owner_only_file:read(Path))
      end}
     || {Mode, Octal} <- [{8#640, "0640"}, {8#604, "0604"}, {8#660, "0660"},
                          {8#606, "0606"}, {8#644, "0644"}, {8#610, "0610"}]].

read_follows_symlink_to_owner_only_file(Base) ->
    {"read follows a symlink to a file only its owner can read", fun() ->
        Target = write_with_mode(Base, "target", 8#600),
        Link = filename:join(Base, "secret"),
        ok = file:make_symlink(Target, Link),
        ?assertEqual({ok, <<"s3cret">>}, macula_owner_only_file:read(Link))
    end}.

read_refuses_symlink_to_file_others_can_read(Base) ->
    {"read refuses a symlink to a file others can read, naming the path it was given", fun() ->
        Target = write_with_mode(Base, "target", 8#644),
        Link = filename:join(Base, "secret"),
        ok = file:make_symlink(Target, Link),
        ?assertEqual({error, {file_permissions, #{file => Link,
                                                  mode => <<"0644">>,
                                                  required => ?REQUIRED_MODE}}},
                     macula_owner_only_file:read(Link))
    end}.

read_refuses_directory(Base) ->
    {"read refuses a directory, naming the path, its type and the requirement", fun() ->
        Path = make_dir_with_mode(Base, "a_directory", 8#700),
        ?assertEqual({error, {file_type, #{file => Path, type => directory, required => regular}}},
                     macula_owner_only_file:read(Path))
    end}.

read_reports_missing_file(Base) ->
    {"read reports a missing file as enoent", fun() ->
        Path = filename:join(Base, "absent"),
        ?assertEqual({error, enoent}, macula_owner_only_file:read(Path))
    end}.

read_reports_dangling_symlink(Base) ->
    {"read reports a symlink to a missing file as enoent", fun() ->
        Link = filename:join(Base, "secret"),
        ok = file:make_symlink(filename:join(Base, "absent"), Link),
        ?assertEqual({error, enoent}, macula_owner_only_file:read(Link))
    end}.

%%%===================================================================
%%% Helpers
%%%===================================================================

make_base() ->
    Unique = integer_to_list(erlang:unique_integer([positive])) ++ "_" ++
        binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(4), lowercase)),
    Base = filename:join(os:getenv("TMPDIR", "/tmp"), ?BASE_PREFIX ++ Unique),
    ok = file:make_dir(Base),
    ok = file:change_mode(Base, 8#700),
    Base.

remove_base(Base) ->
    ok = file:del_dir_r(Base).

make_dir_with_mode(Parent, Name, Mode) ->
    Dir = filename:join(Parent, Name),
    ok = file:make_dir(Dir),
    ok = file:change_mode(Dir, Mode),
    Dir.

write_with_mode(Dir, Name, Mode) ->
    Path = filename:join(Dir, Name),
    ok = file:write_file(Path, <<"s3cret">>),
    ok = file:change_mode(Path, Mode),
    Path.

mode(Path) ->
    {ok, #file_info{mode = Mode}} = file:read_link_info(Path),
    Mode band 8#777.

type(Path) ->
    {ok, #file_info{type = Type}} = file:read_link_info(Path),
    Type.

listing(Dir) ->
    {ok, Names} = file:list_dir(Dir),
    lists:sort(Names).

%% Collects, until told to stop, the modes of the temporary directories under
%% Base seen holding a file.
watch_temporary_directories(Base, Modes) ->
    receive
        {stop, From} -> From ! {modes_seen, Modes}
    after 0 ->
        watch_temporary_directories(Base, temporary_directory_modes(Base, Modes))
    end.

temporary_directory_modes(Base, Modes) ->
    Temporary = [filename:join(Base, Name) || Name <- names(file:list_dir(Base)),
                                              string:find(Name, ".tmp-") =/= nomatch],
    lists:foldl(fun mode_while_holding_file/2, Modes, Temporary).

mode_while_holding_file(Dir, Modes) ->
    holding_file(names(file:list_dir(Dir)), Dir, Modes).

holding_file([], _Dir, Modes) ->
    Modes;
holding_file([_ | _], Dir, Modes) ->
    recorded_mode(file:read_file_info(Dir), Modes).

recorded_mode({ok, #file_info{mode = Mode}}, Modes) ->
    Modes#{Mode band 8#777 => seen};
recorded_mode({error, _}, Modes) ->
    Modes.

names({ok, Names}) -> Names;
names({error, _}) -> [].
