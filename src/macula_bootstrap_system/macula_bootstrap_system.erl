%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Bootstrap System - Supervision tree for bootstrap nodes.
%%%
%%% Bootstrap nodes provide:
%%% - DHT bootstrap and peer discovery
%%% - Service registry (advertised RPC endpoints)
%%% - Health monitoring
%%%
%%% Bootstrap nodes do NOT:
%%% - Relay messages (that's macula_relay_system)
%%% - Bridge realms (that's macula_bridge_system)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_bootstrap_system).
-behaviour(supervisor).

-export([start_link/1, init/1]).
-export([get_server_pid/0, get_stats/0]).

-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start bootstrap system supervisor
-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

%% @doc Get bootstrap server PID
-spec get_server_pid() -> {ok, pid()} | {error, not_started}.
get_server_pid() ->
    case whereis(macula_bootstrap_server) of
        undefined -> {error, not_started};
        Pid -> {ok, Pid}
    end.

%% @doc Get bootstrap system statistics
-spec get_stats() -> {ok, map()} | {error, term()}.
get_stats() ->
    case get_server_pid() of
        {ok, _Pid} -> macula_bootstrap_server:get_stats();
        Error -> Error
    end.

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

init(Config) ->
    ?LOG_INFO("[BootstrapSystem] Starting with config: ~p", [Config]),

    Children = [
        %% Bootstrap server (handles DHT queries, stores routing table)
        #{
            id => bootstrap_server,
            start => {macula_bootstrap_server, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_bootstrap_server]
        },

        %% Service registry (stores advertised RPC endpoints)
        #{
            id => service_registry,
            start => {macula_bootstrap_registry, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_bootstrap_registry]
        },

        %% Health monitoring (tracks system health)
        #{
            id => health_monitor,
            start => {macula_bootstrap_health, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_bootstrap_health]
        }
    ],

    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    {ok, {SupFlags, Children}}.
