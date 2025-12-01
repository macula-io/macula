%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Bridge System - Supervision tree for Bridge Nodes.
%%%
%%% Bridge Nodes provide:
%%% - Connection to parent mesh level (street to neighborhood to city to etc.)
%%% - DHT query escalation when local DHT misses
%%% - Mesh formation with other Bridge Nodes at the same level
%%% - Result caching from parent queries
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_bridge_system).
-behaviour(supervisor).

-export([start_link/1, init/1]).
-export([
    get_bridge_pid/0,
    get_mesh_pid/0,
    is_bridge_enabled/0,
    get_stats/0
]).

-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start bridge system supervisor.
%% Config should include:
%%   - parent_bridges: list of parent bridge endpoints (required for escalation)
%%   - mesh_level: atom indicating level (cluster, street, neighborhood, city, etc.)
%%   - bridge_enabled: boolean to enable/disable bridge functionality
-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

%% @doc Get Bridge Node PID.
-spec get_bridge_pid() -> {ok, pid()} | {error, not_started}.
get_bridge_pid() ->
    case whereis(macula_bridge_node) of
        undefined -> {error, not_started};
        Pid -> {ok, Pid}
    end.

%% @doc Get Bridge Mesh PID.
-spec get_mesh_pid() -> {ok, pid()} | {error, not_started}.
get_mesh_pid() ->
    case whereis(macula_bridge_mesh) of
        undefined -> {error, not_started};
        Pid -> {ok, Pid}
    end.

%% @doc Check if bridge functionality is enabled.
-spec is_bridge_enabled() -> boolean().
is_bridge_enabled() ->
    case get_bridge_pid() of
        {ok, Pid} -> macula_bridge_node:is_connected(Pid);
        {error, _} -> false
    end.

%% @doc Get bridge system statistics.
-spec get_stats() -> {ok, map()} | {error, term()}.
get_stats() ->
    case get_bridge_pid() of
        {ok, Pid} -> macula_bridge_node:get_stats(Pid);
        Error -> Error
    end.

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

init(Config) ->
    BridgeEnabled = maps:get(bridge_enabled, Config, false),
    ?LOG_INFO("[BridgeSystem] Starting with config: ~p, enabled: ~p",
              [Config, BridgeEnabled]),

    Children = build_children(BridgeEnabled, Config),

    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Build child specs based on whether bridge is enabled.
-spec build_children(boolean(), map()) -> [supervisor:child_spec()].
build_children(false, _Config) ->
    ?LOG_INFO("[BridgeSystem] Bridge disabled - no children started"),
    [];
build_children(true, Config) ->
    ?LOG_INFO("[BridgeSystem] Bridge enabled - starting bridge components"),
    [
        %% Bridge Node - manages connection to parent mesh
        #{
            id => bridge_node,
            start => {macula_bridge_node, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_bridge_node]
        },

        %% Bridge Mesh - manages mesh with peer bridges at same level
        #{
            id => bridge_mesh,
            start => {macula_bridge_mesh, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_bridge_mesh]
        },

        %% Bridge Cache - caches results from parent queries
        #{
            id => bridge_cache,
            start => {macula_bridge_cache, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_bridge_cache]
        }
    ].
