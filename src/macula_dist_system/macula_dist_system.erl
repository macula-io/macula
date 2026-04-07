%%%-------------------------------------------------------------------
%%% @doc Macula Distribution System Supervisor.
%%%
%%% This supervisor manages the distribution subsystem components:
%%%
%%% - macula_dist_bridge_sup - Supervisor for relay tunnel bridges
%%% - macula_dist_discovery - Decentralized node discovery (replaces EPMD)
%%% - macula_cluster_strategy - Automatic cluster formation (optional)
%%%
%%% The bridge supervisor hosts `macula_dist_bridge' gen_server children
%%% (one per relay tunnel). When `macula_dist_system' is not started
%%% (standalone relay mode), `macula_dist_relay:advertise_dist_accept/0'
%%% starts the bridge supervisor on demand.
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
        %% Bridge supervisor for relay distribution tunnels
        #{
            id => macula_dist_bridge_sup,
            start => {macula_dist_bridge_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [macula_dist_bridge_sup]
        },
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
