%%%-------------------------------------------------------------------
%%% @doc Macula Cluster Controller
%%%
%%% Application lifecycle management:
%%% - Deploy applications from registry
%%% - Upgrade to newer versions
%%% - Stop running applications
%%% - Auto-update policy enforcement
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cluster_controller).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([deploy_app/1, deploy_app/2]).
-export([upgrade_app/2]).
-export([stop_app/1, remove_app/1]).
-export([list_deployed_apps/0, get_app_status/1]).
-export([set_auto_update_policy/2, get_auto_update_policy/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

-type auto_update_policy() :: always | major | minor | never.

%% deployed_app must be defined before state since state references it
-record(deployed_app, {
    package_name :: binary(),
    version :: binary(),
    manifest :: map(),
    status :: pending | running | stopped | failed,
    deployed_at :: integer(),
    updated_at :: integer(),
    sup_pid :: pid() | undefined
}).

-record(state, {
    deployed_apps :: #{binary() => #deployed_app{}},
    auto_update_policies :: #{binary() => auto_update_policy()},
    app_supervisors :: #{binary() => pid()}
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Start the cluster controller
-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% @doc Deploy an application (latest version)
-spec deploy_app(PackageName :: binary()) -> {ok, pid()} | {error, term()}.
deploy_app(PackageName) ->
    gen_server:call(?SERVER, {deploy, PackageName, latest}, 60000).

%% @doc Deploy a specific version of an application
-spec deploy_app(PackageName :: binary(), Version :: binary()) -> {ok, pid()} | {error, term()}.
deploy_app(PackageName, Version) ->
    gen_server:call(?SERVER, {deploy, PackageName, Version}, 60000).

%% @doc Upgrade an application to a new version
-spec upgrade_app(PackageName :: binary(), NewVersion :: binary()) -> ok | {error, term()}.
upgrade_app(PackageName, NewVersion) ->
    gen_server:call(?SERVER, {upgrade, PackageName, NewVersion}, 60000).

%% @doc Stop a running application
-spec stop_app(PackageName :: binary()) -> ok | {error, not_running}.
stop_app(PackageName) ->
    gen_server:call(?SERVER, {stop, PackageName}).

%% @doc Remove an application completely
-spec remove_app(PackageName :: binary()) -> ok | {error, not_found}.
remove_app(PackageName) ->
    gen_server:call(?SERVER, {remove, PackageName}).

%% @doc List all deployed applications
-spec list_deployed_apps() -> [map()].
list_deployed_apps() ->
    gen_server:call(?SERVER, list_deployed).

%% @doc Get status of a specific application
-spec get_app_status(PackageName :: binary()) -> {ok, map()} | {error, not_found}.
get_app_status(PackageName) ->
    gen_server:call(?SERVER, {get_status, PackageName}).

%% @doc Set auto-update policy for an application
-spec set_auto_update_policy(PackageName :: binary(), Policy :: auto_update_policy()) -> ok.
set_auto_update_policy(PackageName, Policy) ->
    gen_server:call(?SERVER, {set_policy, PackageName, Policy}).

%% @doc Get auto-update policy for an application
-spec get_auto_update_policy(PackageName :: binary()) -> auto_update_policy().
get_auto_update_policy(PackageName) ->
    gen_server:call(?SERVER, {get_policy, PackageName}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(_Config) ->
    {ok, #state{
        deployed_apps = #{},
        auto_update_policies = #{},
        app_supervisors = #{}
    }}.

%% @private
handle_call({deploy, PackageName, Version}, _From, State) ->
    case do_deploy(PackageName, Version, State) of
        {ok, Pid, NewState} ->
            {reply, {ok, Pid}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({upgrade, PackageName, NewVersion}, _From, State) ->
    case do_upgrade(PackageName, NewVersion, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({stop, PackageName}, _From, State) ->
    case do_stop(PackageName, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({remove, PackageName}, _From, State) ->
    case do_remove(PackageName, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(list_deployed, _From, State) ->
    Apps = maps:fold(fun(_K, App, Acc) ->
        [app_to_map(App) | Acc]
    end, [], State#state.deployed_apps),
    {reply, Apps, State};

handle_call({get_status, PackageName}, _From, State) ->
    case maps:get(PackageName, State#state.deployed_apps, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        App ->
            {reply, {ok, app_to_map(App)}, State}
    end;

handle_call({set_policy, PackageName, Policy}, _From, State) ->
    Policies = maps:put(PackageName, Policy, State#state.auto_update_policies),
    {reply, ok, State#state{auto_update_policies = Policies}};

handle_call({get_policy, PackageName}, _From, State) ->
    Policy = maps:get(PackageName, State#state.auto_update_policies, never),
    {reply, Policy, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', _MonitorRef, process, Pid, Reason}, State) ->
    %% Find app by supervisor PID
    case find_app_by_pid(Pid, State#state.app_supervisors) of
        {ok, PackageName} ->
            ?LOG_WARNING("[ClusterController] App ~s supervisor died: ~p",
                        [PackageName, Reason]),
            %% Update status to failed
            case maps:get(PackageName, State#state.deployed_apps, undefined) of
                undefined ->
                    {noreply, State};
                App ->
                    NewApp = App#deployed_app{status = failed, sup_pid = undefined},
                    Apps = maps:put(PackageName, NewApp, State#state.deployed_apps),
                    Sups = maps:remove(PackageName, State#state.app_supervisors),
                    {noreply, State#state{
                        deployed_apps = Apps,
                        app_supervisors = Sups
                    }}
            end;
        not_found ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    %% Stop all deployed applications
    maps:foreach(fun(PackageName, _App) ->
        do_stop(PackageName, State)
    end, State#state.deployed_apps),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Deploy an application
do_deploy(PackageName, Version, State) ->
    %% Check if already deployed
    case maps:get(PackageName, State#state.deployed_apps, undefined) of
        #deployed_app{status = running} ->
            {error, already_running};
        _ ->
            %% Fetch package from registry
            case fetch_package(PackageName, Version) of
                {ok, Package} ->
                    deploy_package(PackageName, Package, State);
                {error, Reason} ->
                    {error, {fetch_failed, Reason}}
            end
    end.

%% @private Fetch package from registry
fetch_package(PackageName, latest) ->
    macula_registry_server:fetch_package(PackageName);
fetch_package(PackageName, Version) ->
    macula_registry_server:fetch_package(PackageName, Version).

%% @private Deploy a fetched package
deploy_package(PackageName, Package, State) ->
    #{
        version := Version,
        manifest := Manifest,
        beam_archive := BeamArchive,
        signature := Signature,
        public_key := PublicKey
    } = Package,

    %% Verify signature
    ManifestBin = macula_registry_manifest:to_binary(Manifest),
    case macula_registry_verify:verify_package(ManifestBin, BeamArchive, Signature, PublicKey) of
        ok ->
            %% Security scan
            case macula_security_scanner:scan_beam_archive(BeamArchive) of
                {ok, ScanResult} ->
                    Score = maps:get(score, ScanResult, 0),
                    case Score >= 50 of  % Minimum score threshold
                        true ->
                            %% Load and start the application
                            case load_and_start_app(PackageName, BeamArchive, Manifest) of
                                {ok, SupPid} ->
                                    Now = erlang:system_time(millisecond),
                                    App = #deployed_app{
                                        package_name = PackageName,
                                        version = Version,
                                        manifest = Manifest,
                                        status = running,
                                        deployed_at = Now,
                                        updated_at = Now,
                                        sup_pid = SupPid
                                    },
                                    Apps = maps:put(PackageName, App, State#state.deployed_apps),
                                    Sups = maps:put(PackageName, SupPid, State#state.app_supervisors),

                                    %% Monitor the supervisor
                                    erlang:monitor(process, SupPid),

                                    %% Start runtime monitoring
                                    macula_app_monitor:start_monitoring(PackageName, SupPid),

                                    ?LOG_INFO("[ClusterController] Deployed ~s v~s (score=~p)",
                                             [PackageName, Version, Score]),

                                    {ok, SupPid, State#state{
                                        deployed_apps = Apps,
                                        app_supervisors = Sups
                                    }};
                                {error, Reason} ->
                                    {error, {start_failed, Reason}}
                            end;
                        false ->
                            Warnings = maps:get(warnings, ScanResult, []),
                            ?LOG_WARNING("[ClusterController] ~s rejected: score=~p, warnings=~p",
                                        [PackageName, Score, Warnings]),
                            {error, {security_score_too_low, Score}}
                    end;
                {error, Reason} ->
                    {error, {scan_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {invalid_signature, Reason}}
    end.

%% @private Load BEAM files and start the application
load_and_start_app(_PackageName, BeamArchive, Manifest) ->
    %% Extract BEAM files
    case extract_beams(BeamArchive) of
        {ok, BeamFiles} ->
            %% Load modules
            lists:foreach(fun({ModName, BeamBin}) ->
                case code:load_binary(ModName, atom_to_list(ModName) ++ ".beam", BeamBin) of
                    {module, ModName} -> ok;
                    {error, Reason} ->
                        ?LOG_WARNING("[ClusterController] Failed to load module ~p: ~p",
                                    [ModName, Reason])
                end
            end, BeamFiles),

            %% Get supervisor from manifest (entry_module reserved for future use)
            _EntryModule = maps:get(entry_module, Manifest, undefined),
            Supervisor = maps:get(supervisor, Manifest, undefined),

            %% Start supervisor if specified
            case Supervisor of
                undefined ->
                    %% No supervisor - just return a dummy pid
                    {ok, self()};
                SupMod when is_atom(SupMod) ->
                    case SupMod:start_link() of
                        {ok, Pid} -> {ok, Pid};
                        {error, Reason} -> {error, Reason}
                    end
            end;
        {error, Reason} ->
            {error, {extraction_failed, Reason}}
    end.

%% @private Extract BEAM files from archive
extract_beams(Archive) ->
    %% Decompress if needed
    Data = try
        zlib:gunzip(Archive)
    catch
        _:_ -> Archive
    end,

    %% Try tar extraction
    case erl_tar:extract({binary, Data}, [memory, compressed]) of
        {ok, FileList} ->
            BeamFiles = lists:filtermap(fun({FileName, Content}) ->
                case filename:extension(FileName) of
                    ".beam" ->
                        ModName = list_to_atom(filename:basename(FileName, ".beam")),
                        {true, {ModName, Content}};
                    _ ->
                        false
                end
            end, FileList),
            {ok, BeamFiles};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Upgrade an application
do_upgrade(PackageName, NewVersion, State) ->
    case maps:get(PackageName, State#state.deployed_apps, undefined) of
        undefined ->
            {error, not_found};
        #deployed_app{version = CurrentVersion} ->
            case macula_registry_manifest:compare_versions(NewVersion, CurrentVersion) of
                gt ->
                    %% Stop current version
                    case do_stop(PackageName, State) of
                        {ok, State2} ->
                            %% Deploy new version
                            case do_deploy(PackageName, NewVersion, State2) of
                                {ok, _Pid, State3} ->
                                    {ok, State3};
                                {error, Reason} ->
                                    %% Rollback - restart old version
                                    ?LOG_ERROR("[ClusterController] Upgrade failed, rolling back: ~p",
                                              [Reason]),
                                    _ = do_deploy(PackageName, CurrentVersion, State2),
                                    {error, {upgrade_failed, Reason}}
                            end;
                        {error, Reason} ->
                            {error, {stop_failed, Reason}}
                    end;
                eq ->
                    {error, already_at_version};
                lt ->
                    {error, downgrade_not_allowed}
            end
    end.

%% @private Stop an application
do_stop(PackageName, State) ->
    case maps:get(PackageName, State#state.deployed_apps, undefined) of
        undefined ->
            {error, not_found};
        #deployed_app{status = stopped} ->
            {error, already_stopped};
        #deployed_app{sup_pid = undefined} = App ->
            %% No supervisor to stop
            NewApp = App#deployed_app{status = stopped, updated_at = erlang:system_time(millisecond)},
            Apps = maps:put(PackageName, NewApp, State#state.deployed_apps),
            {ok, State#state{deployed_apps = Apps}};
        #deployed_app{sup_pid = SupPid} = App ->
            %% Stop monitoring
            macula_app_monitor:stop_monitoring(PackageName),

            %% Stop supervisor
            case is_process_alive(SupPid) of
                true ->
                    exit(SupPid, shutdown);
                false ->
                    ok
            end,

            NewApp = App#deployed_app{
                status = stopped,
                sup_pid = undefined,
                updated_at = erlang:system_time(millisecond)
            },
            Apps = maps:put(PackageName, NewApp, State#state.deployed_apps),
            Sups = maps:remove(PackageName, State#state.app_supervisors),

            ?LOG_INFO("[ClusterController] Stopped ~s", [PackageName]),
            {ok, State#state{deployed_apps = Apps, app_supervisors = Sups}}
    end.

%% @private Remove an application completely
do_remove(PackageName, State) ->
    case maps:get(PackageName, State#state.deployed_apps, undefined) of
        undefined ->
            {error, not_found};
        #deployed_app{status = running} ->
            %% Stop first, then remove
            case do_stop(PackageName, State) of
                {ok, State2} ->
                    do_remove(PackageName, State2);
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            Apps = maps:remove(PackageName, State#state.deployed_apps),
            Policies = maps:remove(PackageName, State#state.auto_update_policies),
            ?LOG_INFO("[ClusterController] Removed ~s", [PackageName]),
            {ok, State#state{
                deployed_apps = Apps,
                auto_update_policies = Policies
            }}
    end.

%% @private Convert deployed_app record to map
app_to_map(#deployed_app{} = App) ->
    #{
        package_name => App#deployed_app.package_name,
        version => App#deployed_app.version,
        status => App#deployed_app.status,
        deployed_at => App#deployed_app.deployed_at,
        updated_at => App#deployed_app.updated_at
    }.

%% @private Find app by supervisor PID
find_app_by_pid(Pid, Supervisors) ->
    case maps:fold(fun(PackageName, SupPid, Acc) ->
        case SupPid =:= Pid of
            true -> {ok, PackageName};
            false -> Acc
        end
    end, not_found, Supervisors) of
        {ok, _} = Result -> Result;
        not_found -> not_found
    end.
