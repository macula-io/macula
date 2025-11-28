%%%-------------------------------------------------------------------
%%% @doc
%%% NAT Traversal System Supervisor.
%%%
%%% Supervises the NAT traversal subsystem which provides:
%%% - NAT type detection using NATCracker methodology
%%% - NAT information caching with stale-while-revalidate
%%% - (Future phases) Hole punching coordination, port prediction,
%%%   distributed relay, and connection quality monitoring
%%%
%%% Architecture:
%%% <pre>
%%% macula_nat_system (this supervisor)
%%% +-- macula_nat_cache       - NAT profile caching with TTL
%%% +-- macula_nat_detector    - Fast NAT type detection
%%% +-- macula_nat_coordinator - Hole punch coordination (Phase 2)
%%% </pre>
%%%
%%% Supervision Strategy:
%%% - one_for_one: Each child is independent, failures don't cascade
%%% - NAT cache failure doesn't require detector restart (and vice versa)
%%%
%%% Phase 1 Implementation (v0.12.0):
%%% - NAT type detection (mapping, filtering, allocation policies)
%%% - NAT profile caching with 5-minute TTL
%%% - DHT integration for NAT profile advertisement
%%%
%%% Future Phases:
%%% - Phase 2: Hole punching (macula_hole_punch, macula_nat_coordinator) - COMPLETED
%%% - Phase 3: Port prediction (macula_port_predictor) - COMPLETED
%%% - Phase 4: Distributed relay (macula_relay_registry, macula_relay_node) - COMPLETED
%%% @end
%%%-------------------------------------------------------------------
-module(macula_nat_system).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the NAT system supervisor with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the NAT system supervisor with given options.
%% Options:
%%   cache_max_entries - Max NAT profiles in cache (default: 10000)
%%   cache_ttl_seconds - NAT profile TTL in seconds (default: 300)
%%   detection_timeout_ms - NAT detection timeout in ms (default: 2000)
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @doc Initialize the supervisor with child specifications.
-spec init(map()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(Opts) ->
    ?LOG_INFO("Starting NAT traversal system"),

    %% Supervision strategy: one_for_one
    %% Each child is independent - failures don't cascade
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,   % Max 5 restarts
        period => 60      % Within 60 seconds
    },

    %% Children in start order
    %% Cache must start before detector (detector may cache results)
    ChildSpecs = [
        %% 1. NAT Cache - stores NAT profiles with TTL
        #{
            id => nat_cache,
            start => {macula_nat_cache, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_nat_cache]
        },

        %% 2. NAT Detector - detects local NAT type
        #{
            id => nat_detector,
            start => {macula_nat_detector, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_nat_detector]
        },

        %% 3. NAT Coordinator - coordinates hole punching (Phase 2)
        #{
            id => nat_coordinator,
            start => {macula_nat_coordinator, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_nat_coordinator]
        },

        %% 4. Port Predictor - intelligent port prediction (Phase 3)
        #{
            id => port_predictor,
            start => {macula_port_predictor, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_port_predictor]
        },

        %% 5. Relay Registry - tracks relay-capable peers (Phase 4)
        #{
            id => relay_registry,
            start => {macula_relay_registry, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_relay_registry]
        },

        %% 6. Relay Node - provides relay functionality (Phase 4)
        #{
            id => relay_node,
            start => {macula_relay_node, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_relay_node]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
