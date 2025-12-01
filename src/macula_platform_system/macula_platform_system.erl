%%%-------------------------------------------------------------------
%%% @doc Macula Platform System Supervisor.
%%%
%%% This supervisor manages the platform layer services that provide
%%% distributed coordination primitives for workloads.
%%%
%%% Platform Services (v0.14.0+):
%%% - CRDT-based shared state (LWW-Register, OR-Set, G-Counter, PN-Counter)
%%% - Gossip-based state synchronization (future: macula_crdt_gossip)
%%% - Coordination Primitives (locks, barriers, etc. - future)
%%%
%%% Architecture:
%%% <pre>
%%% macula_root
%%% ├── [infrastructure: routing, bootstrap, gateway, peers]
%%% └── macula_platform_system (this module)
%%%     └── (future: macula_crdt_gossip for state sync)
%%% </pre>
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

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Start the platform system supervisor.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% @doc Initialize the platform system supervisor.
%% Currently no child processes - CRDT operations are stateless.
%% Future: Add macula_crdt_gossip for state synchronization.
init(Config) ->
    ?LOG_INFO("Starting Macula Platform System (CRDT-based, masterless)"),
    ?LOG_INFO("Node ID: ~s", [maps:get(node_id, Config, <<"unknown">>)]),
    ?LOG_INFO("Realm: ~s", [maps:get(realm, Config, <<"unknown">>)]),

    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    %% No child processes currently
    %% CRDT operations (macula_crdt module) are stateless - no gen_server needed
    %% Future: Add macula_crdt_gossip worker for periodic state synchronization
    ChildSpecs = [],

    ?LOG_INFO("Platform system initialized (no Ra/Raft, using CRDTs)"),
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
