%%%-------------------------------------------------------------------
%%% @doc Macula Platform System Supervisor.
%%%
%%% This supervisor manages the platform layer services that provide
%%% distributed coordination primitives for workloads.
%%%
%%% Platform Services (v0.15.0+):
%%% - CRDT-based shared state (LWW-Register, OR-Set, G-Counter, PN-Counter)
%%% - Gossip-based state synchronization (macula_gossip)
%%% - Coordination Primitives (locks, barriers, etc. - future)
%%%
%%% Architecture:
%%% macula_root
%%% ├── [infrastructure: routing, bootstrap, gateway, peers]
%%% └── macula_platform_system (this module)
%%%     └── macula_gossip (CRDT state synchronization)
%%%
%%% Configuration:
%%% - gossip_enabled: Enable gossip protocol (default: true)
%%% - gossip_push_interval: Push interval in ms (default: 1000)
%%% - gossip_anti_entropy_interval: Anti-entropy interval in ms (default: 30000)
%%% - gossip_fanout: Number of peers per gossip round (default: 3)
%%%
%%% Note: Ra/Raft was removed in v0.14.0. Macula uses CRDTs for
%%% eventually-consistent state management without leader election.
%%% See architecture/ROADMAP.md for details.
%%% @end
%%%-------------------------------------------------------------------

-module(macula_platform_system).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1]).
-export([init/1]).

%% API for accessing gossip server
-export([
    get_gossip_pid/0,
    is_gossip_enabled/0
]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Start the platform system supervisor.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

%% @doc Get the PID of the gossip server.
-spec get_gossip_pid() -> {ok, pid()} | {error, not_started}.
get_gossip_pid() ->
    case whereis(macula_gossip) of
        undefined -> {error, not_started};
        Pid -> {ok, Pid}
    end.

%% @doc Check if gossip is enabled.
-spec is_gossip_enabled() -> boolean().
is_gossip_enabled() ->
    case get_gossip_pid() of
        {ok, _} -> true;
        _ -> false
    end.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% @doc Initialize the platform system supervisor.
%% Starts the gossip server if enabled (default: true).
init(Config) ->
    ?LOG_INFO("Starting Macula Platform System (CRDT-based, masterless)"),
    NodeId = maps:get(node_id, Config, <<"unknown">>),
    Realm = maps:get(realm, Config, <<"unknown">>),
    ?LOG_INFO("Node ID: ~s", [NodeId]),
    ?LOG_INFO("Realm: ~s", [Realm]),

    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    %% Check if gossip is enabled (default: true)
    GossipEnabled = gossip_enabled(Config),

    ChildSpecs = case GossipEnabled of
        true ->
            GossipConfig = build_gossip_config(Config),
            ?LOG_INFO("Gossip protocol enabled with config: ~p", [GossipConfig]),
            [gossip_child_spec(GossipConfig)];
        false ->
            ?LOG_INFO("Gossip protocol disabled"),
            []
    end,

    ?LOG_INFO("Platform system initialized (no Ra/Raft, using CRDTs + Gossip)"),
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Check if gossip is enabled via config or environment.
-spec gossip_enabled(map()) -> boolean().
gossip_enabled(Config) ->
    case maps:get(gossip_enabled, Config, undefined) of
        undefined ->
            %% Check environment variable
            case os:getenv("MACULA_GOSSIP_ENABLED") of
                "false" -> false;
                "0" -> false;
                _ -> true  % Default: enabled
            end;
        Enabled ->
            Enabled
    end.

%% @private Build gossip configuration from platform config.
-spec build_gossip_config(map()) -> map().
build_gossip_config(Config) ->
    #{
        node_id => maps:get(node_id, Config, undefined),
        realm => maps:get(realm, Config, <<"default">>),
        push_interval => maps:get(gossip_push_interval, Config, 1000),
        anti_entropy_interval => maps:get(gossip_anti_entropy_interval, Config, 30000),
        fanout => maps:get(gossip_fanout, Config, 3),
        peers => maps:get(gossip_peers, Config, []),
        send_fn => maps:get(gossip_send_fn, Config, default_send_fn())
    }.

%% @private Create child spec for gossip server.
-spec gossip_child_spec(map()) -> supervisor:child_spec().
gossip_child_spec(GossipConfig) ->
    #{
        id => macula_gossip,
        start => {macula_gossip, start_link, [GossipConfig]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [macula_gossip]
    }.

%% @private Default send function (no-op, to be replaced by actual implementation).
-spec default_send_fn() -> fun((binary(), term()) -> ok).
default_send_fn() ->
    fun(_PeerNodeId, _Msg) ->
        %% TODO: Integrate with macula_gateway for actual message sending
        %% For now, this is a no-op. The actual send function will be
        %% provided by the gateway system when gossip messages need to
        %% be routed to other nodes.
        ok
    end.
