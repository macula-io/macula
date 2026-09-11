%%%-------------------------------------------------------------------
%%% @doc Files only their owner can read, written atomically.
%%%
%%% For the secrets macula keeps on disk: identity keys, the distribution
%%% cookie and the certificates it generates.
%%%
%%% write/2 never lets another user of the host see the content and never
%%% writes through a symlink. A missing parent directory is created, the
%%% innermost one with mode 0700; an existing parent is used as it is. The
%%% content goes into a new file inside a new private directory next to the
%%% target. That directory is restricted to mode 0700 before the file is
%%% created, because a file starts with the default mode and a descriptor
%%% another user opens before a chmod keeps reading after it. The file is
%%% restricted to 0600 before any content is written, synced, and renamed over
%%% the target, which replaces a symlink at the target instead of writing
%%% through it. The private directory is removed afterwards, and on any error
%%% the file with it.
%%%
%%% read/1 follows symlinks and accepts a regular file that gives its group
%%% and others no access, 0600 or 0400, as Erlang's own cookie check does. A
%%% path that is not a regular file is refused without being opened, and the
%%% mode is checked on the opened handle, so the file checked is the file
%%% read.
%%%
%%% A refusal names the file, what was found and what is required: the mode
%%% as an octal binary in file_permissions, the file type in file_type.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_owner_only_file).

-include_lib("kernel/include/file.hrl").

-export([write/2, read/1]).

-export_type([refusal/0]).

-type refusal() ::
        {file_permissions, #{file := file:name_all(), mode := binary(), required := binary()}}
      | {file_type, #{file := file:name_all(), type := atom(), required := regular}}.

-type file_error() :: file:posix() | badarg | terminated | system_limit.

-define(FILE_MODE, 8#600).
-define(DIRECTORY_MODE, 8#700).
-define(GROUP_OR_OTHERS, 8#077).
-define(REQUIRED_MODE, <<"no access for group or others (0600 or 0400)">>).
-define(READ_CHUNK, 65536).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Write Content to Path so that only its owner can read it, replacing
%% whatever is at Path. On an error nothing is left behind and Path is
%% unchanged.
-spec write(file:name_all(), iodata()) -> ok | {error, file_error()}.
write(Path, Content) ->
    in_parent(ensure_parent(filename:dirname(Path)), Path, Content).

%% @doc Read Path when it is a regular file its group and others have no
%% access to, following symlinks.
-spec read(file:name_all()) -> {ok, binary()} | {error, refusal() | file_error()}.
read(Path) ->
    opened_if_regular(file:read_file_info(Path), Path).

%%%===================================================================
%%% Writing
%%%===================================================================

ensure_parent(Dir) ->
    parent_for(filelib:is_dir(Dir), Dir).

parent_for(true, _Dir) ->
    ok;
parent_for(false, Dir) ->
    restricted_after(filelib:ensure_path(Dir), Dir).

restricted_after(ok, Dir) ->
    file:change_mode(Dir, ?DIRECTORY_MODE);
restricted_after({error, _} = Error, _Dir) ->
    Error.

in_parent(ok, Path, Content) ->
    Private = private_directory_name(Path),
    in_private_directory(file:make_dir(Private), Private, Path, Content);
in_parent({error, _} = Error, _Path, _Content) ->
    Error.

%% A failed make_dir means the directory is not ours, so it is never removed.
in_private_directory(ok, Private, Path, Content) ->
    Moved = moved_into_place(file:change_mode(Private, ?DIRECTORY_MODE), Private, Path, Content),
    removed(Moved, Private);
in_private_directory({error, _} = Error, _Private, _Path, _Content) ->
    Error.

moved_into_place(ok, Private, Path, Content) ->
    Temporary = filename:join(Private, filename:basename(Path)),
    renamed_after(written(Temporary, Content), Temporary, Path);
moved_into_place({error, _} = Error, _Private, _Path, _Content) ->
    Error.

renamed_after(ok, Temporary, Path) ->
    file:rename(Temporary, Path);
renamed_after({error, _} = Error, _Temporary, _Path) ->
    Error.

%% After the rename the private directory is empty and the file lives at
%% Path, which removing the directory cannot touch, so a failure to remove
%% it does not fail the write.
removed(ok, Private) ->
    _ = file:del_dir(Private),
    ok;
removed({error, _} = Error, Private) ->
    _ = file:del_dir_r(Private),
    Error.

written(Temporary, Content) ->
    filled(file:open(Temporary, [write, exclusive, raw, binary]), Temporary, Content).

filled({ok, Fd}, Temporary, Content) ->
    Filled = content_after(file:change_mode(Temporary, ?FILE_MODE), Fd, Content),
    first_error([Filled, file:close(Fd)]);
filled({error, _} = Error, _Temporary, _Content) ->
    Error.

content_after(ok, Fd, Content) ->
    synced_after(file:write(Fd, Content), Fd);
content_after({error, _} = Error, _Fd, _Content) ->
    Error.

synced_after(ok, Fd) ->
    file:sync(Fd);
synced_after({error, _} = Error, _Fd) ->
    Error.

private_directory_name(Path) ->
    Suffix = binary:encode_hex(crypto:strong_rand_bytes(8), lowercase),
    filename:join(filename:dirname(Path), suffixed(filename:basename(Path), Suffix)).

suffixed(Base, Suffix) when is_binary(Base) ->
    <<Base/binary, ".tmp-", Suffix/binary>>;
suffixed(Base, Suffix) ->
    Base ++ ".tmp-" ++ binary_to_list(Suffix).

first_error([]) -> ok;
first_error([ok | Rest]) -> first_error(Rest);
first_error([{error, _} = Error | _]) -> Error.

%%%===================================================================
%%% Reading
%%%===================================================================

%% The type is checked before opening so that a FIFO or a device is never
%% opened, and again with the mode on the opened handle.
opened_if_regular({ok, #file_info{type = regular}}, Path) ->
    read_opened(file:open(Path, [read, raw, binary]), Path);
opened_if_regular({ok, #file_info{type = Type}}, Path) ->
    {error, not_regular(Path, Type)};
opened_if_regular({error, _} = Error, _Path) ->
    Error.

read_opened({ok, Fd}, Path) ->
    Read = read_checked(file:read_file_info(Fd), Fd, Path),
    closed_after(Read, file:close(Fd));
read_opened({error, _} = Error, _Path) ->
    Error.

closed_after(Read, ok) -> Read;
closed_after(_Read, {error, _} = Error) -> Error.

read_checked({ok, #file_info{type = regular, mode = Mode}}, Fd, _Path)
  when Mode band ?GROUP_OR_OTHERS =:= 0 ->
    read_all(Fd, []);
read_checked({ok, #file_info{type = regular, mode = Mode}}, _Fd, Path) ->
    {error, {file_permissions, #{file => Path, mode => octal(Mode), required => ?REQUIRED_MODE}}};
read_checked({ok, #file_info{type = Type}}, _Fd, Path) ->
    {error, not_regular(Path, Type)};
read_checked({error, _} = Error, _Fd, _Path) ->
    Error.

read_all(Fd, Chunks) ->
    read_chunk(file:read(Fd, ?READ_CHUNK), Fd, Chunks).

read_chunk({ok, Data}, Fd, Chunks) -> read_all(Fd, [Data | Chunks]);
read_chunk(eof, _Fd, Chunks) -> {ok, iolist_to_binary(lists:reverse(Chunks))};
read_chunk({error, _} = Error, _Fd, _Chunks) -> Error.

not_regular(Path, Type) ->
    {file_type, #{file => Path, type => Type, required => regular}}.

octal(Mode) ->
    iolist_to_binary(io_lib:format("~4.8.0B", [Mode band 8#7777])).
