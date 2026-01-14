%%%-------------------------------------------------------------------
%%% @doc
%%% Content store module for Macula content-addressed storage.
%%%
%%% Provides local storage and retrieval of content blocks and manifests.
%%% Uses file-based storage with directory sharding (first 2 hex chars
%%% of hash) for efficient organization.
%%%
%%% == Storage Layout ==
%%% ```
%%% {base_dir}/
%%% ├── blocks/
%%% │   ├── 5d/
%%% │   │   └── 5d41402abc4b2a76b9719d911017c592.blk
%%% │   └── ...
%%% ├── manifests/
%%% │   ├── 7f/
%%% │   │   └── 7f83b1657ff1fc53b92dc18148a1d65d.man
%%% │   └── ...
%%% └── index.dets
%%% '''
%%%
%%% == Example Usage ==
%%% ```
%%% %% Store a block
%%% Data = <<"content">>,
%%% MCID = compute_mcid(Data),
%%% ok = macula_content_store:put_block(MCID, Data),
%%%
%%% %% Retrieve with verification
%%% {ok, Data} = macula_content_store:get_block(MCID).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_store).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    stop/0,
    %% Block operations
    put_block/2,
    get_block/1,
    has_block/1,
    delete_block/1,
    %% Manifest operations
    put_manifest/1,
    get_manifest/1,
    list_manifests/0,
    delete_manifest/1,
    %% Maintenance
    gc/0,
    stats/0,
    verify_integrity/0,
    %% Path helpers (for testing)
    block_path/1,
    manifest_path/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(BLOCK_EXT, ".blk").
-define(MANIFEST_EXT, ".man").

-record(state, {
    base_dir :: string(),
    blocks_dir :: string(),
    manifests_dir :: string(),
    block_index :: ets:tid(),      %% MCID -> {Size, Timestamp}
    manifest_index :: ets:tid()    %% MCID -> {Name, Size, ChunkCount, Timestamp}
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the content store server.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Stop the content store server.
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Store a block by its MCID.
%% Verifies the data matches the MCID hash before storing.
-spec put_block(binary(), binary()) -> ok | {error, term()}.
put_block(MCID, Data) ->
    gen_server:call(?SERVER, {put_block, MCID, Data}).

%% @doc Retrieve a block by its MCID.
%% Verifies the retrieved data matches the MCID hash.
-spec get_block(binary()) -> {ok, binary()} | {error, not_found | hash_mismatch}.
get_block(MCID) ->
    gen_server:call(?SERVER, {get_block, MCID}).

%% @doc Check if a block exists in the store.
-spec has_block(binary()) -> boolean().
has_block(MCID) ->
    gen_server:call(?SERVER, {has_block, MCID}).

%% @doc Delete a block from the store.
-spec delete_block(binary()) -> ok.
delete_block(MCID) ->
    gen_server:call(?SERVER, {delete_block, MCID}).

%% @doc Store a manifest.
-spec put_manifest(map()) -> ok | {error, term()}.
put_manifest(Manifest) ->
    gen_server:call(?SERVER, {put_manifest, Manifest}).

%% @doc Retrieve a manifest by its MCID.
-spec get_manifest(binary()) -> {ok, map()} | {error, not_found}.
get_manifest(MCID) ->
    gen_server:call(?SERVER, {get_manifest, MCID}).

%% @doc List all manifest MCIDs.
-spec list_manifests() -> [binary()].
list_manifests() ->
    gen_server:call(?SERVER, list_manifests).

%% @doc Delete a manifest from the store.
-spec delete_manifest(binary()) -> ok.
delete_manifest(MCID) ->
    gen_server:call(?SERVER, {delete_manifest, MCID}).

%% @doc Garbage collect orphaned blocks not referenced by any manifest.
-spec gc() -> {ok, #{removed := non_neg_integer()}}.
gc() ->
    gen_server:call(?SERVER, gc, 60000).

%% @doc Get storage statistics.
-spec stats() -> map().
stats() ->
    gen_server:call(?SERVER, stats).

%% @doc Verify integrity of all stored blocks.
-spec verify_integrity() -> {ok, non_neg_integer()} | {error, {corrupted, [binary()]}}.
verify_integrity() ->
    gen_server:call(?SERVER, verify_integrity, 60000).

%% @doc Get the file path for a block MCID.
%% Uses first 2 hex chars of hash for directory sharding.
-spec block_path(binary()) -> string().
block_path(<<_Version:8, _Codec:8, Hash:32/binary>>) ->
    BaseDir = get_default_base_dir(),
    BlocksDir = filename:join(BaseDir, "blocks"),
    HexHash = macula_content_hasher:hex_encode(Hash),
    <<Shard:2/binary, _Rest/binary>> = HexHash,
    filename:join([BlocksDir, binary_to_list(Shard),
                   binary_to_list(HexHash) ++ ?BLOCK_EXT]).

%% @doc Get the file path for a manifest MCID.
-spec manifest_path(binary()) -> string().
manifest_path(<<_Version:8, _Codec:8, Hash:32/binary>>) ->
    BaseDir = get_default_base_dir(),
    ManifestsDir = filename:join(BaseDir, "manifests"),
    HexHash = macula_content_hasher:hex_encode(Hash),
    <<Shard:2/binary, _Rest/binary>> = HexHash,
    filename:join([ManifestsDir, binary_to_list(Shard),
                   binary_to_list(HexHash) ++ ?MANIFEST_EXT]).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init(Opts) ->
    %% Support both store_path and base_dir keys for flexibility
    BaseDir = case maps:get(store_path, Opts, undefined) of
        undefined -> maps:get(base_dir, Opts, get_default_base_dir());
        Path -> Path
    end,
    BlocksDir = filename:join(BaseDir, "blocks"),
    ManifestsDir = filename:join(BaseDir, "manifests"),

    %% Ensure directories exist (create if missing)
    ensure_dir_exists(BlocksDir),
    ensure_dir_exists(ManifestsDir),

    %% Create ETS tables for indexing
    BlockIndex = ets:new(content_block_index, [set, private]),
    ManifestIndex = ets:new(content_manifest_index, [set, private]),

    %% Load existing files into index
    State = #state{
        base_dir = BaseDir,
        blocks_dir = BlocksDir,
        manifests_dir = ManifestsDir,
        block_index = BlockIndex,
        manifest_index = ManifestIndex
    },
    rebuild_index(State),

    {ok, State}.

handle_call({put_block, MCID, Data}, _From, State) ->
    Result = do_put_block(MCID, Data, State),
    {reply, Result, State};

handle_call({get_block, MCID}, _From, State) ->
    Result = do_get_block(MCID, State),
    {reply, Result, State};

handle_call({has_block, MCID}, _From, State) ->
    <<_:8, _:8, Hash:32/binary>> = MCID,
    Result = ets:member(State#state.block_index, Hash),
    {reply, Result, State};

handle_call({delete_block, MCID}, _From, State) ->
    Result = do_delete_block(MCID, State),
    {reply, Result, State};

handle_call({put_manifest, Manifest}, _From, State) ->
    Result = do_put_manifest(Manifest, State),
    {reply, Result, State};

handle_call({get_manifest, MCID}, _From, State) ->
    Result = do_get_manifest(MCID, State),
    {reply, Result, State};

handle_call(list_manifests, _From, State) ->
    MCIDs = [<<1, 16#56, Hash/binary>> || {Hash, _} <- ets:tab2list(State#state.manifest_index)],
    {reply, MCIDs, State};

handle_call({delete_manifest, MCID}, _From, State) ->
    Result = do_delete_manifest(MCID, State),
    {reply, Result, State};

handle_call(gc, _From, State) ->
    Result = do_gc(State),
    {reply, Result, State};

handle_call(stats, _From, State) ->
    Stats = do_stats(State),
    {reply, Stats, State};

handle_call(verify_integrity, _From, State) ->
    Result = do_verify_integrity(State),
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal Functions - Directory Management
%%%===================================================================

%% @private Ensure directory exists, creating it if necessary.
ensure_dir_exists(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            ok;
        false ->
            %% ensure_dir needs a trailing slash or file to create parent dirs
            %% This creates all parent directories
            case filelib:ensure_dir(filename:join(Dir, "dummy")) of
                ok ->
                    %% Now create the final directory
                    case file:make_dir(Dir) of
                        ok -> ok;
                        {error, eexist} -> ok;  %% Race condition, dir created by another process
                        {error, Reason} -> {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%%%===================================================================
%%% Internal Functions - Block Operations
%%%===================================================================

do_put_block(<<_Version:8, _Codec:8, Hash:32/binary>> = _MCID, Data, State) ->
    %% Verify data matches hash
    ActualHash = macula_content_hasher:hash(blake3, Data),
    case ActualHash =:= Hash of
        true ->
            Path = compute_block_path(Hash, State),
            ok = filelib:ensure_dir(Path),
            ok = file:write_file(Path, Data),
            ets:insert(State#state.block_index, {Hash, {byte_size(Data), erlang:system_time(second)}}),
            ok;
        false ->
            %% Try SHA256 as fallback
            Sha256Hash = macula_content_hasher:hash(sha256, Data),
            case Sha256Hash =:= Hash of
                true ->
                    Path = compute_block_path(Hash, State),
                    ok = filelib:ensure_dir(Path),
                    ok = file:write_file(Path, Data),
                    ets:insert(State#state.block_index, {Hash, {byte_size(Data), erlang:system_time(second)}}),
                    ok;
                false ->
                    {error, hash_mismatch}
            end
    end.

do_get_block(<<_Version:8, _Codec:8, Hash:32/binary>> = _MCID, State) ->
    case ets:lookup(State#state.block_index, Hash) of
        [] ->
            {error, not_found};
        [{Hash, _}] ->
            Path = compute_block_path(Hash, State),
            case file:read_file(Path) of
                {ok, Data} ->
                    %% Verify hash on retrieval
                    ActualBlake3 = macula_content_hasher:hash(blake3, Data),
                    ActualSha256 = macula_content_hasher:hash(sha256, Data),
                    case ActualBlake3 =:= Hash orelse ActualSha256 =:= Hash of
                        true -> {ok, Data};
                        false -> {error, hash_mismatch}
                    end;
                {error, _} ->
                    {error, not_found}
            end
    end.

do_delete_block(<<_Version:8, _Codec:8, Hash:32/binary>>, State) ->
    Path = compute_block_path(Hash, State),
    file:delete(Path),
    ets:delete(State#state.block_index, Hash),
    ok.

%%%===================================================================
%%% Internal Functions - Manifest Operations
%%%===================================================================

do_put_manifest(Manifest, State) ->
    MCID = maps:get(mcid, Manifest),
    <<_Version:8, _Codec:8, Hash:32/binary>> = MCID,
    Path = compute_manifest_path(Hash, State),
    ok = filelib:ensure_dir(Path),
    {ok, Encoded} = macula_content_manifest:encode(Manifest),
    ok = file:write_file(Path, Encoded),
    Info = {
        maps:get(name, Manifest),
        maps:get(size, Manifest),
        maps:get(chunk_count, Manifest),
        erlang:system_time(second)
    },
    ets:insert(State#state.manifest_index, {Hash, Info}),
    ok.

do_get_manifest(<<_Version:8, _Codec:8, Hash:32/binary>>, State) ->
    case ets:lookup(State#state.manifest_index, Hash) of
        [] ->
            {error, not_found};
        [{Hash, _}] ->
            Path = compute_manifest_path(Hash, State),
            case file:read_file(Path) of
                {ok, Encoded} ->
                    macula_content_manifest:decode(Encoded);
                {error, _} ->
                    {error, not_found}
            end
    end.

do_delete_manifest(<<_Version:8, _Codec:8, Hash:32/binary>>, State) ->
    Path = compute_manifest_path(Hash, State),
    file:delete(Path),
    ets:delete(State#state.manifest_index, Hash),
    ok.

%%%===================================================================
%%% Internal Functions - Maintenance
%%%===================================================================

do_gc(State) ->
    %% Collect all chunk hashes referenced by manifests
    ReferencedHashes = collect_referenced_hashes(State),

    %% Find orphaned blocks
    AllBlocks = [Hash || {Hash, _} <- ets:tab2list(State#state.block_index)],
    Orphans = lists:filter(fun(Hash) ->
        not sets:is_element(Hash, ReferencedHashes)
    end, AllBlocks),

    %% Delete orphans
    lists:foreach(fun(Hash) ->
        MCID = <<1, 16#55, Hash/binary>>,
        do_delete_block(MCID, State)
    end, Orphans),

    {ok, #{removed => length(Orphans)}}.

collect_referenced_hashes(State) ->
    Manifests = [Hash || {Hash, _} <- ets:tab2list(State#state.manifest_index)],
    lists:foldl(fun(ManifestHash, Acc) ->
        MCID = <<1, 16#56, ManifestHash/binary>>,
        case do_get_manifest(MCID, State) of
            {ok, Manifest} ->
                Chunks = maps:get(chunks, Manifest, []),
                lists:foldl(fun(ChunkInfo, InnerAcc) ->
                    ChunkHash = maps:get(hash, ChunkInfo),
                    sets:add_element(ChunkHash, InnerAcc)
                end, Acc, Chunks);
            {error, _} ->
                Acc
        end
    end, sets:new(), Manifests).

do_stats(State) ->
    BlockCount = ets:info(State#state.block_index, size),
    ManifestCount = ets:info(State#state.manifest_index, size),
    TotalSize = ets:foldl(fun({_Hash, {Size, _}}, Acc) ->
        Acc + Size
    end, 0, State#state.block_index),
    #{
        block_count => BlockCount,
        manifest_count => ManifestCount,
        total_size => TotalSize
    }.

do_verify_integrity(State) ->
    Corrupted = ets:foldl(fun({Hash, _}, Acc) ->
        MCID = <<1, 16#55, Hash/binary>>,
        case do_get_block(MCID, State) of
            {ok, _} -> Acc;
            {error, hash_mismatch} -> [MCID | Acc];
            {error, not_found} -> [MCID | Acc]
        end
    end, [], State#state.block_index),

    case Corrupted of
        [] ->
            {ok, ets:info(State#state.block_index, size)};
        _ ->
            {error, {corrupted, Corrupted}}
    end.

%%%===================================================================
%%% Internal Functions - Paths
%%%===================================================================

compute_block_path(Hash, State) ->
    HexHash = macula_content_hasher:hex_encode(Hash),
    <<Shard:2/binary, _Rest/binary>> = HexHash,
    filename:join([State#state.blocks_dir, binary_to_list(Shard),
                   binary_to_list(HexHash) ++ ?BLOCK_EXT]).

compute_manifest_path(Hash, State) ->
    HexHash = macula_content_hasher:hex_encode(Hash),
    <<Shard:2/binary, _Rest/binary>> = HexHash,
    filename:join([State#state.manifests_dir, binary_to_list(Shard),
                   binary_to_list(HexHash) ++ ?MANIFEST_EXT]).

%%%===================================================================
%%% Internal Functions - Index Rebuilding
%%%===================================================================

rebuild_index(State) ->
    %% Scan blocks directory
    BlockPattern = filename:join([State#state.blocks_dir, "*", "*" ++ ?BLOCK_EXT]),
    BlockFiles = filelib:wildcard(BlockPattern),
    lists:foreach(fun(Path) ->
        case file:read_file_info(Path) of
            {ok, Info} ->
                Basename = filename:basename(Path, ?BLOCK_EXT),
                case macula_content_hasher:hex_decode(list_to_binary(Basename)) of
                    {ok, Hash} ->
                        Size = element(2, Info),
                        ets:insert(State#state.block_index, {Hash, {Size, 0}});
                    {error, _} ->
                        ok
                end;
            {error, _} ->
                ok
        end
    end, BlockFiles),

    %% Scan manifests directory
    ManifestPattern = filename:join([State#state.manifests_dir, "*", "*" ++ ?MANIFEST_EXT]),
    ManifestFiles = filelib:wildcard(ManifestPattern),
    lists:foreach(fun(Path) ->
        Basename = filename:basename(Path, ?MANIFEST_EXT),
        case macula_content_hasher:hex_decode(list_to_binary(Basename)) of
            {ok, Hash} ->
                case file:read_file(Path) of
                    {ok, Encoded} ->
                        case macula_content_manifest:decode(Encoded) of
                            {ok, Manifest} ->
                                Info = {
                                    maps:get(name, Manifest),
                                    maps:get(size, Manifest),
                                    maps:get(chunk_count, Manifest),
                                    0
                                },
                                ets:insert(State#state.manifest_index, {Hash, Info});
                            {error, _} ->
                                ok
                        end;
                    {error, _} ->
                        ok
                end;
            {error, _} ->
                ok
        end
    end, ManifestFiles),
    ok.

get_default_base_dir() ->
    case application:get_env(macula, content_store_dir) of
        {ok, Dir} -> Dir;
        undefined -> "/var/lib/macula/content"
    end.
