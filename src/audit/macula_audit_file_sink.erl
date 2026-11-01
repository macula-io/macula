%% @doc A file sink for an audit chain (macula#175 WP1): one length-prefixed entry frame per append, and a sync that
%% is a real fsync.
%%
%% This is the reference sink, not the station's. It writes one file, in the order entries were appended, and it can
%% read the file back after a restart, which is what `macula_audit_chain:start/1' needs. A deployment with a bounded
%% store or a flood policy answers the same behaviour with its own module (WP2, WP3).
%%
%% The frames are `<<Size:32, Bytes/binary>>', so an entry may hold any bytes and a truncated tail is visible as a
%% short frame rather than read as an entry.
-module(macula_audit_file_sink).

-behaviour(macula_audit_sink).

-export([open/1, append/2, sync/1, entries/1]).

-opaque state() :: #{path := file:filename()}.
-export_type([state/0]).

%% @doc The sink for one file, created empty if it does not exist yet.
-spec open(file:filename()) -> {ok, state()} | {error, term()}.
open(Path) when is_binary(Path); is_list(Path) ->
    case file:open(Path, [append, raw, binary]) of
        {ok, Fd} ->
            _ = file:close(Fd),
            {ok, #{path => Path}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Append one entry frame.
-spec append(binary(), state()) -> {ok, state()} | {error, term()}.
append(Bytes, #{path := Path} = State) when is_binary(Bytes) ->
    case file:open(Path, [append, raw, binary]) of
        {ok, Fd} ->
            Written = file:write(Fd, [<<(byte_size(Bytes)):32>>, Bytes]),
            _ = file:close(Fd),
            written(Written, State);
        {error, Reason} ->
            {error, Reason}
    end.

written(ok, State) -> {ok, State};
written({error, Reason}, _State) -> {error, Reason}.

%% @doc Flush what has been appended to the medium, which is what a checkpoint rests on.
-spec sync(state()) -> ok | {error, term()}.
sync(#{path := Path}) ->
    case file:open(Path, [read, write, raw, binary]) of
        {ok, Fd} ->
            Synced = file:sync(Fd),
            _ = file:close(Fd),
            Synced;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Every entry the file holds, in the order it was written. A frame that runs past the end of the file is
%% refused rather than read as a shorter entry.
-spec entries(state()) -> {ok, [binary()]} | {error, term()}.
entries(#{path := Path}) ->
    case file:read_file(Path) of
        {ok, Bytes}     -> frames(Bytes, []);
        {error, enoent} -> {ok, []};
        {error, Reason} -> {error, Reason}
    end.

frames(<<>>, Acc) ->
    {ok, lists:reverse(Acc)};
frames(<<Size:32, Entry:Size/binary, Rest/binary>>, Acc) ->
    frames(Rest, [Entry | Acc]);
frames(_Truncated, _Acc) ->
    {error, truncated_frame}.
