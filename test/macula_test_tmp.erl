%% Temporary directories for tests, unique across test runs on one host and removed by the tests that make them. A
%% name carries the OS process id of the run and 8 random bytes, so it never meets a path of another run, concurrent
%% or earlier, and a directory is made fresh: one that already exists under the chosen name is never reused.
-module(macula_test_tmp).

-export([dir/1, with_dir/2]).

%% The random part of a name, called through the module so a test can repeat it.
-export([unique_part/0]).

%% @doc A new, empty directory under TMPDIR, readable by its owner only, named from Prefix. The caller removes it,
%% as a fixture's cleanup does with file:del_dir_r/1.
-spec dir(string()) -> file:filename().
dir(Prefix) when is_list(Prefix) ->
    made(named(Prefix), Prefix).

%% @doc Fun called with a new directory from dir/1, which is removed with everything in it once Fun returns or
%% raises.
-spec with_dir(string(), fun((file:filename()) -> Result)) -> Result.
with_dir(Prefix, Fun) when is_list(Prefix), is_function(Fun, 1) ->
    Dir = dir(Prefix),
    try
        Fun(Dir)
    after
        ok = file:del_dir_r(Dir)
    end.

%% @doc The OS process id of this run and 8 random bytes, in hex.
-spec unique_part() -> string().
unique_part() ->
    os:getpid() ++ "-" ++ binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(8), lowercase)).

made(Path, Prefix) ->
    created(file:make_dir(Path), Path, Prefix).

created(ok, Path, _Prefix) ->
    ok = file:change_mode(Path, 8#700),
    Path;
created({error, eexist}, _Path, Prefix) ->
    made(named(Prefix), Prefix).

named(Prefix) ->
    filename:join(os:getenv("TMPDIR", "/tmp"), Prefix ++ "-" ++ ?MODULE:unique_part()).
