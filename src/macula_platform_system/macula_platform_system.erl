%%%-------------------------------------------------------------------
%% @doc Macula Platform System Supervisor.
%%
%% This supervisor manages the platform layer services that provide
%% distributed coordination primitives for workloads.
%%
%% Platform Services:
%% - Leader Election (Raft-based via ra library)
%% - Shared State Management (CRDT-based replication)
%% - Coordination Primitives (locks, barriers, etc. - future)
%%
%% Architecture:
%% <pre>
%% macula_root
%% ├── [infrastructure: routing, bootstrap, gateway, peers]
%% └── macula_platform_system (this module)
%%     ├── macula_leader_election (Raft consensus)
%%     └── macula_shared_state (CRDT state)
%% </pre>
%%
%% @end
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

start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init(Config) ->
    ?LOG_INFO("Starting Macula Platform System"),
    ?LOG_INFO("Node ID: ~s", [maps:get(node_id, Config, <<"unknown">>)]),
    ?LOG_INFO("Realm: ~s", [maps:get(realm, Config, <<"unknown">>)]),

    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    %% Platform services
    ChildSpecs = [
        %% Leader Election Service (Raft-based)
        #{
            id => macula_leader_election,
            start => {macula_leader_election, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_leader_election]
        }

        %% Shared State Service (CRDT-based) - TODO: Phase 2
        %% #{
        %%     id => macula_shared_state,
        %%     start => {macula_shared_state, start_link, [Config]},
        %%     restart => permanent,
        %%     shutdown => 5000,
        %%     type => worker,
        %%     modules => [macula_shared_state]
        %% }
    ],

    ?LOG_INFO("Platform services initialized"),
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
