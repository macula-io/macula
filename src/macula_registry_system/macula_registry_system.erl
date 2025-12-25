%%%-------------------------------------------------------------------
%%% @doc Macula Registry System Supervisor
%%%
%%% Top-level supervisor for the registry subsystem. Manages:
%%% - Registry store (local package storage)
%%% - Registry server (publish/fetch API)
%%% - Cluster controller (app lifecycle management)
%%% - App monitor (runtime defense)
%%%
%%% This module is a supervisor ONLY - no business logic.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_registry_system).

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([get_server_pid/0, get_store_pid/0, get_controller_pid/0, get_monitor_pid/0]).
-export([is_enabled/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Start the registry system supervisor
-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

%% @doc Get the registry server PID
-spec get_server_pid() -> {ok, pid()} | {error, not_found}.
get_server_pid() ->
    get_child_pid(registry_server).

%% @doc Get the registry store PID
-spec get_store_pid() -> {ok, pid()} | {error, not_found}.
get_store_pid() ->
    get_child_pid(registry_store).

%% @doc Get the cluster controller PID
-spec get_controller_pid() -> {ok, pid()} | {error, not_found}.
get_controller_pid() ->
    get_child_pid(cluster_controller).

%% @doc Get the app monitor PID
-spec get_monitor_pid() -> {ok, pid()} | {error, not_found}.
get_monitor_pid() ->
    get_child_pid(app_monitor).

%% @doc Check if registry system is enabled
-spec is_enabled() -> boolean().
is_enabled() ->
    case os:getenv("MACULA_REGISTRY_ENABLED") of
        "false" -> false;
        "0" -> false;
        _ -> application:get_env(macula, registry_enabled, true)
    end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init(Config) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    %% Build child specs based on config
    ChildSpecs = build_child_specs(Config),

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Build child specifications
-spec build_child_specs(map()) -> [supervisor:child_spec()].
build_child_specs(Config) ->
    [
        %% 1. Registry Store - must start first (others depend on it)
        #{
            id => registry_store,
            start => {macula_registry_store, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_registry_store]
        },
        %% 2. Registry Server - package API
        #{
            id => registry_server,
            start => {macula_registry_server, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_registry_server]
        },
        %% 3. Cluster Controller - app lifecycle management
        #{
            id => cluster_controller,
            start => {macula_cluster_controller, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_cluster_controller]
        },
        %% 4. App Monitor - runtime defense
        #{
            id => app_monitor,
            start => {macula_app_monitor, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_app_monitor]
        }
    ].

%% @private Get child PID by ID
-spec get_child_pid(atom()) -> {ok, pid()} | {error, not_found}.
get_child_pid(ChildId) ->
    case whereis(?SERVER) of
        undefined ->
            {error, not_found};
        SupPid ->
            find_child(supervisor:which_children(SupPid), ChildId)
    end.

%% @private Find child in children list
-spec find_child([{atom(), pid() | restarting | undefined, worker | supervisor, [module()]}], atom()) ->
    {ok, pid()} | {error, not_found}.
find_child([], _ChildId) ->
    {error, not_found};
find_child([{ChildId, Pid, _, _} | _Rest], ChildId) when is_pid(Pid) ->
    {ok, Pid};
find_child([_ | Rest], ChildId) ->
    find_child(Rest, ChildId).
