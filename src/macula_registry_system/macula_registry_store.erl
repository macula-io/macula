%%%-------------------------------------------------------------------
%%% @doc Macula Registry Store
%%%
%%% Handles local package storage with ETS index and disk persistence:
%%% - Package metadata stored in ETS for fast lookups
%%% - BEAM archives stored on disk
%%% - TTL-based cleanup for stale entries
%%% - DHT integration for distributed discovery
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_registry_store).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([store_package/2, get_package/2, get_package/3]).
-export([list_packages/1, search_packages/2]).
-export([delete_package/3, package_exists/3]).
-export([get_versions/2, get_latest_version/2]).
-export([prune_expired/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(PACKAGE_TABLE, macula_registry_packages).
-define(INDEX_TABLE, macula_registry_index).
-define(DEFAULT_TTL, 86400000).  % 24 hours in ms
-define(CLEANUP_INTERVAL, 300000).  % 5 minutes

-record(state, {
    storage_path :: file:filename(),
    package_table :: ets:tid(),
    index_table :: ets:tid(),
    default_ttl :: pos_integer(),
    cleanup_timer :: reference() | undefined
}).

-record(package_entry, {
    key :: {binary(), binary()},  % {PackageName, Version}
    manifest :: map(),
    checksum :: binary(),
    signature :: binary(),
    public_key :: binary(),
    published_at :: integer(),
    expires_at :: integer(),
    archive_path :: file:filename()
}).

-record(index_entry, {
    package_name :: binary(),
    versions :: [binary()],
    latest :: binary()
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Start the registry store
-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% @doc Store a package in the registry
-spec store_package(pid(), map()) -> {ok, binary()} | {error, term()}.
store_package(Pid, PackageData) ->
    gen_server:call(Pid, {store_package, PackageData}, 30000).

%% @doc Get a package by name (latest version)
-spec get_package(pid(), binary()) -> {ok, map()} | {error, not_found}.
get_package(Pid, PackageName) ->
    gen_server:call(Pid, {get_package, PackageName, latest}).

%% @doc Get a package by name and version
-spec get_package(pid(), binary(), binary()) -> {ok, map()} | {error, not_found}.
get_package(Pid, PackageName, Version) ->
    gen_server:call(Pid, {get_package, PackageName, Version}).

%% @doc List all packages
-spec list_packages(pid()) -> [map()].
list_packages(Pid) ->
    gen_server:call(Pid, list_packages).

%% @doc Search packages by pattern
-spec search_packages(pid(), binary()) -> [map()].
search_packages(Pid, Pattern) ->
    gen_server:call(Pid, {search_packages, Pattern}).

%% @doc Delete a package version
-spec delete_package(pid(), binary(), binary()) -> ok | {error, not_found}.
delete_package(Pid, PackageName, Version) ->
    gen_server:call(Pid, {delete_package, PackageName, Version}).

%% @doc Check if a package version exists
-spec package_exists(pid(), binary(), binary()) -> boolean().
package_exists(Pid, PackageName, Version) ->
    gen_server:call(Pid, {package_exists, PackageName, Version}).

%% @doc Get all versions of a package
-spec get_versions(pid(), binary()) -> [binary()].
get_versions(Pid, PackageName) ->
    gen_server:call(Pid, {get_versions, PackageName}).

%% @doc Get the latest version of a package
-spec get_latest_version(pid(), binary()) -> {ok, binary()} | {error, not_found}.
get_latest_version(Pid, PackageName) ->
    gen_server:call(Pid, {get_latest_version, PackageName}).

%% @doc Remove expired packages
-spec prune_expired(pid()) -> {ok, non_neg_integer()}.
prune_expired(Pid) ->
    gen_server:call(Pid, prune_expired).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(Config) ->
    process_flag(trap_exit, true),

    StoragePath = get_storage_path(Config),
    ok = ensure_storage_dir(StoragePath),

    PackageTable = ets:new(?PACKAGE_TABLE, [
        set, protected, named_table,
        {keypos, #package_entry.key}
    ]),

    IndexTable = ets:new(?INDEX_TABLE, [
        set, protected, named_table,
        {keypos, #index_entry.package_name}
    ]),

    TTL = maps:get(default_ttl, Config, ?DEFAULT_TTL),

    %% Load existing packages from disk
    ok = load_packages_from_disk(StoragePath, PackageTable, IndexTable, TTL),

    %% Start cleanup timer
    Timer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),

    {ok, #state{
        storage_path = StoragePath,
        package_table = PackageTable,
        index_table = IndexTable,
        default_ttl = TTL,
        cleanup_timer = Timer
    }}.

%% @private
handle_call({store_package, PackageData}, _From, State) ->
    Result = do_store_package(PackageData, State),
    {reply, Result, State};

handle_call({get_package, PackageName, latest}, _From, State) ->
    Result = do_get_latest_package(PackageName, State),
    {reply, Result, State};

handle_call({get_package, PackageName, Version}, _From, State) ->
    Result = do_get_package(PackageName, Version, State),
    {reply, Result, State};

handle_call(list_packages, _From, State) ->
    Result = do_list_packages(State),
    {reply, Result, State};

handle_call({search_packages, Pattern}, _From, State) ->
    Result = do_search_packages(Pattern, State),
    {reply, Result, State};

handle_call({delete_package, PackageName, Version}, _From, State) ->
    Result = do_delete_package(PackageName, Version, State),
    {reply, Result, State};

handle_call({package_exists, PackageName, Version}, _From, State) ->
    Result = ets:member(State#state.package_table, {PackageName, Version}),
    {reply, Result, State};

handle_call({get_versions, PackageName}, _From, State) ->
    Result = do_get_versions(PackageName, State),
    {reply, Result, State};

handle_call({get_latest_version, PackageName}, _From, State) ->
    case ets:lookup(State#state.index_table, PackageName) of
        [#index_entry{latest = Latest}] -> {reply, {ok, Latest}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call(prune_expired, _From, State) ->
    Count = do_prune_expired(State),
    {reply, {ok, Count}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(cleanup, State) ->
    _Count = do_prune_expired(State),
    Timer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    {noreply, State#state{cleanup_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    case State#state.cleanup_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Get storage path from config
get_storage_path(Config) ->
    case maps:get(storage_path, Config, undefined) of
        undefined ->
            case os:getenv("MACULA_REGISTRY_STORAGE_PATH") of
                false -> "/var/lib/macula/registry";
                Path -> Path
            end;
        Path -> Path
    end.

%% @private Ensure storage directory exists
ensure_storage_dir(Path) ->
    case filelib:ensure_dir(filename:join(Path, "dummy")) of
        ok -> ok;
        {error, Reason} ->
            ?LOG_WARNING("[RegistryStore] Cannot create storage dir ~s: ~p", [Path, Reason]),
            ok  % Continue anyway - may be read-only
    end.

%% @private Store a package
do_store_package(PackageData, State) ->
    #{
        package_name := PackageName,
        version := Version,
        manifest := Manifest,
        beam_archive := BeamArchive,
        signature := Signature,
        public_key := PublicKey
    } = PackageData,

    Checksum = crypto:hash(sha256, BeamArchive),
    Now = erlang:system_time(millisecond),
    ExpiresAt = Now + State#state.default_ttl,

    %% Save archive to disk
    ArchivePath = archive_path(State#state.storage_path, PackageName, Version),
    case save_archive(ArchivePath, BeamArchive) of
        ok ->
            %% Store metadata in ETS
            Entry = #package_entry{
                key = {PackageName, Version},
                manifest = Manifest,
                checksum = Checksum,
                signature = Signature,
                public_key = PublicKey,
                published_at = Now,
                expires_at = ExpiresAt,
                archive_path = ArchivePath
            },
            ets:insert(State#state.package_table, Entry),

            %% Update index
            update_index(State#state.index_table, PackageName, Version),

            ?LOG_INFO("[RegistryStore] Stored package ~s v~s", [PackageName, Version]),
            {ok, Checksum};
        {error, Reason} ->
            {error, {storage_failed, Reason}}
    end.

%% @private Get latest package
do_get_latest_package(PackageName, State) ->
    case ets:lookup(State#state.index_table, PackageName) of
        [#index_entry{latest = Latest}] ->
            do_get_package(PackageName, Latest, State);
        [] ->
            {error, not_found}
    end.

%% @private Get package by version
do_get_package(PackageName, Version, State) ->
    case ets:lookup(State#state.package_table, {PackageName, Version}) of
        [Entry] ->
            case load_archive(Entry#package_entry.archive_path) of
                {ok, BeamArchive} ->
                    {ok, #{
                        package_name => PackageName,
                        version => Version,
                        manifest => Entry#package_entry.manifest,
                        beam_archive => BeamArchive,
                        checksum => Entry#package_entry.checksum,
                        signature => Entry#package_entry.signature,
                        public_key => Entry#package_entry.public_key,
                        published_at => Entry#package_entry.published_at
                    }};
                {error, _Reason} ->
                    %% Archive missing from disk
                    {error, not_found}
            end;
        [] ->
            {error, not_found}
    end.

%% @private List all packages
do_list_packages(State) ->
    ets:foldl(fun(#index_entry{package_name = Name, versions = Versions, latest = Latest}, Acc) ->
        [#{name => Name, versions => Versions, latest => Latest} | Acc]
    end, [], State#state.index_table).

%% @private Search packages by pattern
do_search_packages(Pattern, State) ->
    %% Simple prefix/substring matching
    PatternStr = binary_to_list(Pattern),
    ets:foldl(fun(#index_entry{package_name = Name} = Entry, Acc) ->
        NameStr = binary_to_list(Name),
        case string:find(NameStr, PatternStr) of
            nomatch -> Acc;
            _ -> [#{name => Name,
                    versions => Entry#index_entry.versions,
                    latest => Entry#index_entry.latest} | Acc]
        end
    end, [], State#state.index_table).

%% @private Delete package version
do_delete_package(PackageName, Version, State) ->
    case ets:lookup(State#state.package_table, {PackageName, Version}) of
        [Entry] ->
            %% Delete archive from disk
            file:delete(Entry#package_entry.archive_path),

            %% Remove from ETS
            ets:delete(State#state.package_table, {PackageName, Version}),

            %% Update index
            remove_from_index(State#state.index_table, PackageName, Version),

            ?LOG_INFO("[RegistryStore] Deleted package ~s v~s", [PackageName, Version]),
            ok;
        [] ->
            {error, not_found}
    end.

%% @private Get all versions
do_get_versions(PackageName, State) ->
    case ets:lookup(State#state.index_table, PackageName) of
        [#index_entry{versions = Versions}] -> Versions;
        [] -> []
    end.

%% @private Prune expired packages
do_prune_expired(State) ->
    Now = erlang:system_time(millisecond),
    Expired = ets:foldl(fun(#package_entry{key = Key, expires_at = ExpiresAt}, Acc) ->
        case ExpiresAt < Now of
            true -> [Key | Acc];
            false -> Acc
        end
    end, [], State#state.package_table),

    lists:foreach(fun({PackageName, Version}) ->
        do_delete_package(PackageName, Version, State)
    end, Expired),

    length(Expired).

%% @private Update package index
update_index(IndexTable, PackageName, Version) ->
    case ets:lookup(IndexTable, PackageName) of
        [#index_entry{versions = Versions} = Entry] ->
            NewVersions = lists:usort([Version | Versions]),
            NewLatest = find_latest_version(NewVersions),
            ets:insert(IndexTable, Entry#index_entry{
                versions = NewVersions,
                latest = NewLatest
            });
        [] ->
            ets:insert(IndexTable, #index_entry{
                package_name = PackageName,
                versions = [Version],
                latest = Version
            })
    end.

%% @private Remove version from index
remove_from_index(IndexTable, PackageName, Version) ->
    case ets:lookup(IndexTable, PackageName) of
        [#index_entry{versions = Versions}] ->
            NewVersions = lists:delete(Version, Versions),
            case NewVersions of
                [] ->
                    ets:delete(IndexTable, PackageName);
                _ ->
                    NewLatest = find_latest_version(NewVersions),
                    ets:insert(IndexTable, #index_entry{
                        package_name = PackageName,
                        versions = NewVersions,
                        latest = NewLatest
                    })
            end;
        [] ->
            ok
    end.

%% @private Find latest version using SemVer comparison
find_latest_version([]) -> undefined;
find_latest_version([V]) -> V;
find_latest_version(Versions) ->
    lists:foldl(fun(V, Max) ->
        case macula_registry_manifest:compare_versions(V, Max) of
            gt -> V;
            _ -> Max
        end
    end, hd(Versions), tl(Versions)).

%% @private Build archive path
archive_path(StoragePath, PackageName, Version) ->
    FileName = iolist_to_binary([PackageName, "-", Version, ".tar.gz"]),
    filename:join([StoragePath, "packages", FileName]).

%% @private Save archive to disk
save_archive(Path, Data) ->
    ok = filelib:ensure_dir(Path),
    file:write_file(Path, Data).

%% @private Load archive from disk
load_archive(Path) ->
    file:read_file(Path).

%% @private Load existing packages from disk
%% Note: PackageTable, IndexTable, and TTL are reserved for future implementation
%% when we add full disk-to-ETS loading capability
load_packages_from_disk(StoragePath, _PackageTable, _IndexTable, _TTL) ->
    PackagesDir = filename:join(StoragePath, "packages"),
    case filelib:is_dir(PackagesDir) of
        true ->
            ?LOG_INFO("[RegistryStore] Loading packages from ~s", [PackagesDir]),
            %% In a real implementation, we'd parse .tar.gz files to extract metadata
            %% For now, we start with an empty registry
            ok;
        false ->
            ok
    end.
