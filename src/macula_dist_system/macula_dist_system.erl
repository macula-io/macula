%%%-------------------------------------------------------------------
%%% @doc Macula Distribution System Supervisor.
%%%
%%% This supervisor manages the distribution subsystem components:
%%%
%%% - macula_dist_discovery - Decentralized node discovery (replaces EPMD)
%%% - macula_cluster_strategy - Automatic cluster formation
%%%
%%% Startup: The distribution system starts automatically with macula application.
%%% Can also be started manually with macula_dist_system:start_link(Opts).
%%%
%%% Configuration options (in sys.config):
%%%   dist_port - QUIC port (default 4433)
%%%   discovery_type - mdns, dht, or both
%%%   auto_cluster - automatically form cluster
%%%
%%% @copyright 2025 Macula.io Apache-2.0
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_system).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_link/1
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the distribution system supervisor with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the distribution system supervisor with options.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init(Opts) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    DiscoveryType = maps:get(discovery_type, Opts,
        application:get_env(macula, discovery_type, both)),

    AutoCluster = maps:get(auto_cluster, Opts,
        application:get_env(macula, auto_cluster, false)),

    %% Child specifications
    Children = [
        %% Discovery service (always started)
        #{
            id => macula_dist_discovery,
            start => {macula_dist_discovery, start_link, [#{
                discovery_type => DiscoveryType
            }]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_dist_discovery]
        }
    ] ++ maybe_cluster_strategy(AutoCluster, Opts),

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Add cluster strategy child if auto_cluster is enabled
maybe_cluster_strategy(false, _Opts) ->
    [];
maybe_cluster_strategy(true, Opts) ->
    [
        #{
            id => macula_cluster_strategy,
            start => {macula_cluster_strategy, start_link, [#{
                topology => macula_cluster,
                config => maps:get(cluster_config, Opts, #{})
            }]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_cluster_strategy]
        }
    ].
